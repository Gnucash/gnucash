/********************************************************************\
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/
/** @addtogroup Import_Export
    @{ */
/** @internal
     @file gnc-dtaus-import.c
     @brief DTAUS import module code
     @author Copyright (c) 2002 Benoit Grégoire <bock@step.polymtl.ca>, Copyright (c) 2003 Jan-Pascal van Best <janpascal@vanbest.org>, Copyright (c) 2006 Florian Steinel, 2006 Christian Stimming.
 */
#include "config.h"

#include "gnc-file-aqb-import.h"

#include <glib/gi18n.h>
#include <glib/gstdio.h>
#include <string.h>
#include <sys/time.h>
#include <fcntl.h>

#include <aqbanking/version.h>
#include <aqbanking/banking.h>
#include <aqbanking/imexporter.h>

#include "gnc-ui.h"
#include "qof.h"
#include "Transaction.h"
#include "Account.h"

#include "gnc-engine.h"
#include "gnc-file.h"
#include "gnc-ui-util.h"

#include "gnc-hbci-utils.h"
#include "gnc-hbci-gettrans.h"
#include "hbci-interaction.h"


static QofLogModule log_module = GNC_MOD_IMPORT;


/* See aqbanking-1.6.0beta/src/tools/aqbanking-tool/import.c for hints
   on how to program aqbanking. */

/********************************************************************\
 * gnc_file_dtaus_import
 * Entry point
\********************************************************************/

void gnc_file_aqbanking_import (const gchar *aqbanking_importername,
				const gchar *aqbanking_profilename,
				gboolean execute_transactions)
{
  char *selected_filename;
  char *default_dir;
  int dtaus_fd;

  DEBUG("gnc_file_dtaus_import(): Begin...\n");

  default_dir = gnc_get_default_directory(GCONF_SECTION);
  selected_filename = gnc_file_dialog(_("Select a file to import"),
				      NULL,
				      default_dir,
				      GNC_FILE_DIALOG_IMPORT);
  g_free(default_dir);

  if(selected_filename!=NULL) {
    /* Remember the directory as the default. */
    default_dir = g_path_get_dirname(selected_filename);
    gnc_set_default_directory(GCONF_SECTION, default_dir);
    g_free(default_dir);

    /*strncpy(file,selected_filename, 255);*/
    DEBUG("Filename found: %s",selected_filename);

    DEBUG("Opening selected file");
    dtaus_fd = g_open(selected_filename, O_RDONLY, 0);
    if (dtaus_fd == -1) {
      DEBUG("Could not open file %s", selected_filename);
      return;
    }
    g_free(selected_filename);

    {
      int result;
      AB_BANKING *ab;
      AB_IMEXPORTER *importer;
      AB_IMEXPORTER_CONTEXT *ctx=0;
      GWEN_BUFFEREDIO *buffio;
      GWEN_DB_NODE *dbProfiles;
      GWEN_DB_NODE *dbProfile;
      GNCInteractor *interactor = NULL;
      const char *importerName = aqbanking_importername;
      const char *profileName = aqbanking_profilename;

      /* Get API */
      ab = gnc_AB_BANKING_new_currentbook (NULL, &interactor);
      if (ab == NULL) {
	g_message("gnc_file_dtaus_import: Couldn't get HBCI API. Nothing will happen.\n");
	return;
      }
      g_assert (interactor);

      /* get import module */
      importer=AB_Banking_GetImExporter(ab, importerName);
      if (!importer) {
	DEBUG("Import module %s not found", importerName);
	gnc_error_dialog(NULL, "%s",("Import module for DTAUS import not found."));
	return;
      }
      g_assert(importer);

      /* load the import profile */
      dbProfiles=AB_Banking_GetImExporterProfiles(ab, importerName);

      /* select profile */
      dbProfile=GWEN_DB_GetFirstGroup(dbProfiles);
      while(dbProfile) {
	const char *name;

	name=GWEN_DB_GetCharValue(dbProfile, "name", 0, 0);
	g_assert(name);
	if (strcasecmp(name, profileName)==0)
	  break;
	dbProfile=GWEN_DB_GetNextGroup(dbProfile);
      }
      if (!dbProfile) {
	g_warning("Profile \"%s\" for importer \"%s\" not found\n",
	      profileName, importerName);
	/* For debugging: Print those available names that have been found. */
	dbProfile=GWEN_DB_GetFirstGroup(dbProfiles);
	while(dbProfile) {
	  const char *name;
	  name=GWEN_DB_GetCharValue(dbProfile, "name", 0, 0);
	  g_assert(name);
	  g_warning("Only found profile \"%s\"\n", name);
	  dbProfile=GWEN_DB_GetNextGroup(dbProfile);
	}
	return;
      }
      g_assert(dbProfile);

      /* import new context */
      ctx=AB_ImExporterContext_new();
      g_assert(ctx);

      /* Wrap file in gwen_bufferedio */
      buffio = GWEN_BufferedIO_File_new(dtaus_fd);
      g_assert(buffio);
      GWEN_BufferedIO_SetReadBuffer(buffio, 0, 1024);

      result = AB_ImExporter_Import(importer,
				  ctx,
				  buffio,
				  dbProfile);

      DEBUG("Parsing result: %d\n", result);

      GWEN_BufferedIO_Close(buffio);
      GWEN_BufferedIO_free(buffio);
      GWEN_DB_Group_free(dbProfiles);

      {
	/* Now get all accountinfos */
	GNCImportMainMatcher *importer_generic_gui;
	GtkWidget *parent = NULL;
	gboolean successful = FALSE;
	GList *ab_job_list;

	/* Create importer GUI */
	importer_generic_gui = gnc_gen_trans_list_new(parent, NULL, TRUE, 14);

	/* Import the transactions from the ctx into gnucash. */
	ab_job_list = gnc_hbci_import_ctx(ab, ctx, importer_generic_gui,
					  execute_transactions);
	/* Finished importing. */

	/* We clean up here. */
	AB_ImExporterContext_free(ctx);

	if (execute_transactions) {
	  /* Wait for the gnucash importer to be finished (it is being
	     run anyway). */
	  result = gnc_gen_trans_list_run (importer_generic_gui);

	  if (result)
	    /* Execute these jobs now. This function already delete()s the
	       job. */
	    /* no parent so far; otherwise add this: GNCInteractor_reparent (interactor, parent); */
	    successful = gnc_hbci_multijob_execute (parent, ab, ab_job_list, interactor);
	  /* else */
	  
	  /* Delete all jobs from queue in any case. */
	  gnc_hbci_clearqueue (ab, ab_job_list);
	}
	else {
	  successful = TRUE;
	}

	if (successful) {
	  /* If execution was not successful, leave the log window
	     still intact and open. */
	  gnc_AB_BANKING_fini (ab);
	  gnc_AB_BANKING_delete (ab);
	}
      }
    }
  }
}


/** @} */
