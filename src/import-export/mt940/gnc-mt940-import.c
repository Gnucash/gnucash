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
     @author Copyright (c) 2002 Benoit Grégoire <bock@step.polymtl.ca>, Copyright (c) 2003 Jan-Pascal van Best <janpascal@vanbest.org>, Copyright (c) 2006 Florian Steinel, 2006 Christian Stimming.
 */
#define _GNU_SOURCE

#include "config.h"

#include <glib.h>
#include <glib/gi18n.h>
#include <stdio.h>
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
#include "gnc-gconf-utils.h"

#include "gnc-hbci-gettrans.h"

#include "import-main-matcher.h"
#include "import-account-matcher.h"
#include "gnc-hbci-gettrans.h"

#include "gnc-mt940-import.h"

#define GCONF_SECTION "dialogs/import/mt940"

static QofLogModule log_module = GNC_MOD_IMPORT;

/* Callback declarations */
static const AB_TRANSACTION *
translist_cb (const AB_TRANSACTION *element, void *user_data);
static AB_IMEXPORTER_ACCOUNTINFO *
accountinfolist_cb(AB_IMEXPORTER_ACCOUNTINFO *element, void *user_data);


/* If aqbanking is older than 1.9.7, use our own copies of these
   foreach functions */
#if ((AQBANKING_VERSION_MAJOR == 1) && \
     ((AQBANKING_VERSION_MINOR < 9) || \
      ((AQBANKING_VERSION_MINOR == 9) && \
       ((AQBANKING_VERSION_PATCHLEVEL < 7)))))
static AB_IMEXPORTER_ACCOUNTINFO *
AB_ImExporterContext_AccountInfoForEach(AB_IMEXPORTER_CONTEXT *iec,
					AB_IMEXPORTER_ACCOUNTINFO_LIST2_FOREACH func,
					void* user_data)
{
  AB_IMEXPORTER_ACCOUNTINFO *it;
  AB_IMEXPORTER_ACCOUNTINFO *retval;
  g_assert(iec);

  it = AB_ImExporterContext_GetFirstAccountInfo (iec);
  while (it) {
    retval = func(it, user_data);
    if (retval) {
      return retval;
    }
    it = AB_ImExporterContext_GetNextAccountInfo (iec);
  }
  return 0;

}
static const AB_TRANSACTION *
AB_ImExporterAccountInfo_TransactionsForEach(AB_IMEXPORTER_ACCOUNTINFO *iea,
					     AB_TRANSACTION_CONSTLIST2_FOREACH func,
					     void* user_data)
{
  const AB_TRANSACTION *it;
  const AB_TRANSACTION *retval;
  g_assert(iea);

  it = AB_ImExporterAccountInfo_GetFirstTransaction (iea);
  while (it) {
    retval = func(it, user_data);
    if (retval) {
      return retval;
    }
    it = AB_ImExporterAccountInfo_GetNextTransaction (iea);
  }
  return 0;
}
#endif /* aqbanking < 1.9.7 */


/* Note: The importing here is not yet tested with actual files, see
   http://bugzilla.gnome.org/show_bug.cgi?id=325170 . */

/* See aqbanking-1.6.0beta/src/tools/aqbanking-tool/import.c for hints
   on how to program aqbanking. */

/********************************************************************\
 * gnc_file_mt940_import
 * Entry point
\********************************************************************/

void gnc_file_mt940_import (void)
{
  char *selected_filename;
  char *default_dir;
  int mt940_fd;
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
    gnc_mt940_importer_gui = gnc_gen_trans_list_new(NULL, NULL, FALSE, 42);

    DEBUG("Opening selected file");
    mt940_fd = open(selected_filename, O_RDONLY);

    {
      int result;
      AB_BANKING *ab;
      AB_IMEXPORTER *importer;
      AB_IMEXPORTER_CONTEXT *ctx=0;
      GWEN_BUFFEREDIO *buffio;
      GWEN_DB_NODE *dbProfiles;
      GWEN_DB_NODE *dbProfile;
      const char *importerName = "swift"; /* possible values: csv, swift, dtaus, */
      const char *profileName = "default";

      ab = AB_Banking_new("gnucash", 0);
      /* get import module */
      importer=AB_Banking_GetImExporter(ab, importerName);
      if (!importer) {
	DEBUG("Import module swift not found");
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
      g_assert(dbProfile);
      /*if (!dbProfile) {
	DBG_ERROR(AQT_LOGDOMAIN,
		  "Profile \"%s\" for importer \"%s\" not found",
		  profileName, importerName);
	return 3;
	}*/

      /* import new context */
      ctx=AB_ImExporterContext_new();

      /* Wrap file in gwen_bufferedio */
      buffio = GWEN_BufferedIO_File_new(mt940_fd);

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
	struct trans_list_data data;
	GNCImportMainMatcher *importer_generic_gui;
	GtkWidget *parent = NULL;

	/* Create importer GUI */
	importer_generic_gui = gnc_gen_trans_list_new(parent, NULL, TRUE, 14);
	data.importer_generic = importer_generic_gui;

	/* Iterate through all accounts */
	AB_ImExporterContext_AccountInfoForEach(ctx, accountinfolist_cb, &data);
	/* all accounts finished. */
	      
	AB_ImExporterContext_free(ctx);

	/* that's it */
	result=AB_Banking_Fini(ab);
	if (result) 
	  DEBUG("ERROR: Error on deinit (%d)\n",result);

	g_free(selected_filename);
	AB_Banking_free(ab);


	/* and run the gnucash importer. */
	gnc_gen_trans_list_run (importer_generic_gui);
      }
    }
  }
}

static AB_IMEXPORTER_ACCOUNTINFO *
accountinfolist_cb(AB_IMEXPORTER_ACCOUNTINFO *accinfo, void *user_data) {
  Account *gnc_acc;
  struct trans_list_data *data = user_data;
  const char *bank_code =
    AB_ImExporterAccountInfo_GetBankCode(accinfo);
  const char *account_number = 
    AB_ImExporterAccountInfo_GetAccountNumber(accinfo);
  const char *account_name = 
    AB_ImExporterAccountInfo_GetAccountName(accinfo);
  gchar *online_id = g_strconcat (bank_code, account_number, NULL);
  
  gnc_acc = gnc_import_select_account(NULL, 
				      online_id, 1, account_name, NULL, 
				      NO_TYPE, NULL, NULL);
  g_free(online_id);
  if (gnc_acc) {
    /* Store chosen gnucash account in callback data */
    data->gnc_acc = gnc_acc;
  
    /* Iterate through all transactions.  */
    AB_ImExporterAccountInfo_TransactionsForEach (accinfo, translist_cb, data);
    /* all transactions finished. */
  }
  return NULL;
}

static const AB_TRANSACTION *
translist_cb (const AB_TRANSACTION *element, void *user_data) {
  /* This callback in the hbci module will add the imported
     transaction to gnucash's importer. */
  /* The call will use "element" only as const* */
  gnc_hbci_trans_list_cb( (AB_TRANSACTION*) element, user_data);
  return NULL;
}


/** @} */
