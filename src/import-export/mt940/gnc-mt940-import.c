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
     @file gnc-mt940-import.c
     @brief MT940 import module code
     @author Copyright (c) 2002 Benoit Grégoire <bock@step.polymtl.ca>, Copyright (c) 2003 Jan-Pascal van Best <janpascal@vanbest.org>, Copyright (c) 2006 Florian Steinel
 */
 /* See aqbanking-1.6.0beta/src/tools/aqbanking-tool/import.c for hints */
#define _GNU_SOURCE

#include "config.h"

#include <glib.h>
#include <stdio.h>
#include <string.h>
#include <sys/time.h>

#include <aqbanking/imexporter.h>

#include "gnc-ui.h"
#include "qof.h"
#include "Transaction.h"
#include "Account.h"

#include "gnc-engine.h"
#include "gnc-file.h"
#include "gnc-ui-util.h"

#include "gnc-hbci-gettrans.h"

#include "import-main-matcher.h"
#include "import-account-matcher.h"
#include "gnc-hbci-gettrans.h"

#include "gnc-mt940-import.h"

#define GCONF_SECTION "dialogs/import/mt940"

static QofLogModule log_module = GNC_MOD_IMPORT;


/* Note: This file is broken. See
   http://bugzilla.gnome.org/show_bug.cgi?id=325170 .  The patch from
   there is already included, but the file still won't compile. */



/********************************************************************\
 * gnc_file_mt940_import
 * Entry point
\********************************************************************/

void gnc_file_mt940_import (void)
{
  char *selected_filename;
  char *default_dir;
  FILE *mt940_file;
  GNCImportMainMatcher *gnc_mt940_importer_gui = NULL;

  /* gnc_should_log(MOD_IMPORT, GNC_LOG_TRACE); */
  DEBUG("gnc_file_mt940_import(): Begin...\n");

  default_dir = gnc_gconf_get_string(GCONF_SECTION, KEY_LAST_PATH, NULL);
  if (default_dir == NULL)
    gnc_init_default_directory(&default_dir);
  selected_filename = gnc_file_dialog(_("Select an MT940 file to process"),
				      NULL,
				      default_dir,
				      GNC_FILE_DIALOG_IMPORT);
  g_free(default_dir);

  if(selected_filename!=NULL) {
    /* Remember the directory as the default. */
    gnc_extract_directory(&default_dir, selected_filename);
    gnc_gconf_set_string(GCONF_SECTION, KEY_LAST_PATH, default_dir, NULL);
    g_free(default_dir);

    /*strncpy(file,selected_filename, 255);*/
    DEBUG("Filename found: %s",selected_filename);

    /* Create the Generic transaction importer GUI. */
    gnc_mt940_importer_gui = gnc_gen_trans_list_new(NULL, NULL, FALSE);

    DEBUG("Opening selected file");
    mt940_file = fopen(selected_filename, "r");

    {
      int result;
      AB_BANKING *tr;
      AB_IMEXPORTER *importer;
      AB_IMEXPORTER_CONTEXT *ctx=0;
      GWEN_BUFFEREDIO *buffio;
      const list_HBCI_Transaction *transactions;
      /*list_HBCI_Transaction_iter *iter;*/

      tr=AB_Banking_new();
      /* get import module */
      importer=AB_Banking_GetImExporter(tr, "swift"); /* possible values: csv, swift, dtaus, */
      if (!importer) {
	DEBUG("Import module swift not found");
      }

      /* load the import profile */
      //dbProfiles=AB_Banking_GetImExporterProfiles(tr, "swift");
      dbProfile="default";

      /* import new context */
      ctx=AB_ImExporterContext_new();

      /* Wrap file in gwen_bufferedio */
      buffio = GWEN_BufferedIO_File_new(mt940_file);

      result=AB_ImExporter_Import(importer,
				  ctx,
				  buffio,
				  dbProfile);

      DEBUG("Parsing result: %d\n", result);

      GWEN_BufferedIO_Close(buffio);
      GWEN_BufferedIO_free(buffio);

      {
	/* Now get all accountinfos */
	AB_TRANSACTION *ab_trans;
	struct trans_list_data data;
	Account *gnc_acc;
	AB_IMEXPORTER_ACCOUNTINFO * accinfo;

	/* Create importer GUI */
	importer_generic_gui = gnc_gen_trans_list_new(parent, NULL, TRUE, 14);
	data.importer_generic = importer_generic_gui;

	/* Iterate through all accounts */
	accinfo = AB_ImExporterContext_GetFirstAccountInfo(ctx);
	while (accinfo) {
	  /* FIXME: need to get account name from accinfo
	     structure here */
	  gnc_acc = gnc_import_select_account(account_name, 1, NULL, NULL, 
					      NO_TYPE, NULL, NULL);
	  /* Store chosen gnucash account in callback data */
	  data.gnc_acc = gnc_acc;

	  /* Iterate through all transactions */
	  ab_trans = 
	    AB_ImExporterAccountInfo_GetFirstTransaction (accinfo);
	  while (ab_trans) {
	    /* This callback in the hbci module will add the
	       imported transaction to gnucash's importer */
	    gnc_hbci_trans_list_cb(ab_trans, &data);
	    ab_trans = 
	      AB_ImExporterAccountInfo_GetNextTransaction (accinfo);
	  }
	  /* all transaction finished. */

	  accinfo = AB_ImExporterContext_GetNextAccountInfo(ctx);
	}
	/* all accounts finished. */
	      
	AB_ImExporterContext_free(ctx);

	/* that's it */
	result=AB_Banking_Fini(ab);
	if (result) 
	  DEBUG("ERROR: Error on deinit (%d)\n",result);

	g_free(selected_filename);


	/* and run the gnucash importer. */
	gnc_gen_trans_list_run (importer_generic_gui);
      }
    }
  }
}

/** @} */
