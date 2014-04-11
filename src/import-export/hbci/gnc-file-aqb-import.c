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

#include <glib.h>
#include <glib/gi18n.h>
#include <stdio.h>
#include <string.h>
#include <sys/time.h>
#include <fcntl.h>

#include <aqbanking/version.h>
#include <aqbanking/banking.h>
#include <aqbanking/imexporter.h>
#include <aqbanking/jobsingledebitnote.h>

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
#include "dialog-hbcitrans.h"

#include "import-main-matcher.h"
#include "import-account-matcher.h"
#include "gnc-hbci-gettrans.h"

#include "gnc-file-aqb-import.h"

static QofLogModule log_module = GNC_MOD_IMPORT;

/* Callback declarations */
static const AB_TRANSACTION *
translist_cb (const AB_TRANSACTION *element, void *user_data);
static AB_IMEXPORTER_ACCOUNTINFO *
accountinfolist_cb(AB_IMEXPORTER_ACCOUNTINFO *element, void *user_data);
static gboolean 
gnc_hbci_multijob_execute(GtkWidget *parent, AB_BANKING *api,
			  GList *job_list,  GNCInteractor *interactor);
static void multijob_cb (gpointer element, gpointer user_data);
static void delpending_cb (gpointer element, gpointer user_data);

struct import_data 
{
  Account *gnc_acc;
  GNCImportMainMatcher *importer_generic;
  AB_BANKING *ab;
  AB_ACCOUNT *hbci_account;
  GList *job_list;
  gboolean execute_transactions;
};


/* If aqbanking is older than 1.9.7, use our own copies of these
   foreach functions */
#if ((AQBANKING_VERSION_MAJOR == 1) && \
     ((AQBANKING_VERSION_MINOR < 9) || \
      ((AQBANKING_VERSION_MINOR == 9) && \
       ((AQBANKING_VERSION_PATCHLEVEL < 7)))))
static AB_IMEXPORTER_ACCOUNTINFO *
AB_ImExporterContext_AccountInfoForEach(AB_IMEXPORTER_CONTEXT *iec,
					AB_IMEXPORTER_ACCOUNTINFO *
					(* func)(AB_IMEXPORTER_ACCOUNTINFO *element,
						 void *user_data),
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
					     const AB_TRANSACTION *
					     (* func)(const AB_TRANSACTION *element,
						      void *user_data),
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

  /* qof_log_check(MOD_IMPORT, QOF_LOG_TRACE); */
  DEBUG("gnc_file_dtaus_import(): Begin...\n");

  default_dir = gnc_get_default_directory(GCONF_SECTION);
  selected_filename = gnc_file_dialog(_("Select an DTAUS file to process"),
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
    dtaus_fd = open(selected_filename, O_RDONLY);
    if (dtaus_fd == -1) {
      DEBUG("Could not open file %s", selected_filename);
      return;
    }

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
	printf("gnc_file_dtaus_import: Couldn't get HBCI API. Nothing will happen.\n");
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
	DEBUG("Profile \"%s\" for importer \"%s\" not found",
	      profileName, importerName);
	printf("Profile \"%s\" for importer \"%s\" not found\n",
	      profileName, importerName);
	/* For debugging: Print those available names that have been found. */
	dbProfile=GWEN_DB_GetFirstGroup(dbProfiles);
	while(dbProfile) {
	  const char *name;
	  name=GWEN_DB_GetCharValue(dbProfile, "name", 0, 0);
	  g_assert(name);
	  printf("Only found profile \"%s\"\n", name);
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
	struct import_data data;
	GNCImportMainMatcher *importer_generic_gui;
	GtkWidget *parent = NULL;
	gboolean successful = FALSE;

	/* Create importer GUI */
	importer_generic_gui = gnc_gen_trans_list_new(parent, NULL, TRUE, 14);
	data.importer_generic = importer_generic_gui;
	data.ab = ab;
	data.job_list = NULL;
	data.execute_transactions = execute_transactions;

	/* Iterate through all accounts */
	AB_ImExporterContext_AccountInfoForEach(ctx, accountinfolist_cb, &data);
	/* all accounts finished. */

	/* that's it */
	g_free(selected_filename);

	if (execute_transactions) {
	  /* and run the gnucash importer. */
	  result = gnc_gen_trans_list_run (importer_generic_gui);

	  if (result)
	    /* Execute these jobs now. This function already delete()s the
	       job. */
	    /* no parent so far; otherwise add this: GNCInteractor_reparent (interactor, parent); */
	    successful = gnc_hbci_multijob_execute (parent, ab, data.job_list, interactor);
	  /* else */
	  
	  /* Delete all jobs from queue in any case. */
	  g_list_foreach (data.job_list, delpending_cb, ab);
	}
	else {
	  successful = TRUE;
	}

	/* We clean up here. */
	AB_ImExporterContext_free(ctx);
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

static AB_IMEXPORTER_ACCOUNTINFO *
accountinfolist_cb(AB_IMEXPORTER_ACCOUNTINFO *accinfo, void *user_data) {
  Account *gnc_acc;
  struct import_data *data = user_data;
  const char *bank_code =
    AB_ImExporterAccountInfo_GetBankCode(accinfo);
  const char *account_number = 
    AB_ImExporterAccountInfo_GetAccountNumber(accinfo);
  const char *account_name = 
    AB_ImExporterAccountInfo_GetAccountName(accinfo);
  gchar *online_id = g_strconcat (bank_code, account_number, NULL);
  
  gnc_acc = gnc_import_select_account(NULL, 
				      online_id, 1, account_name, NULL, 
				      ACCT_TYPE_NONE, NULL, NULL);
  g_free(online_id);
  if (gnc_acc) {
    /* Store chosen gnucash account in callback data */
    data->gnc_acc = gnc_acc;

    if (data->execute_transactions) {
      /* Retrieve the aqbanking account that belongs to this gnucash
	 account */
      data->hbci_account = gnc_hbci_get_hbci_acc (data->ab, gnc_acc);
      if (data->hbci_account == NULL) {
	gnc_error_dialog (NULL, _("No HBCI account found for this gnucash account. These transactions will not be executed by HBCI."));
      }
    }
    else {
      data->hbci_account = NULL;
    }
  
    /* Iterate through all transactions.  */
    AB_ImExporterAccountInfo_TransactionsForEach (accinfo, translist_cb, data);
    /* all transactions finished. */
  }
  return NULL;
}

static const AB_TRANSACTION *
translist_cb (const AB_TRANSACTION *element, void *user_data) {
  AB_JOB *job;
  AB_TRANSACTION *trans = (AB_TRANSACTION*)element;
  GtkWidget *parent = NULL;
  struct import_data *data = user_data;
  struct trans_list_data hbci_userdata;

  /* This callback in the hbci module will add the imported
     transaction to gnucash's importer. */
  hbci_userdata.gnc_acc = data->gnc_acc;
  hbci_userdata.importer_generic = data->importer_generic;
  /* The call will use "trans" only as const* */
  gnc_hbci_trans_list_cb((AB_TRANSACTION*) trans, &hbci_userdata);

  if (data->hbci_account) {
    /* NEW: The imported transaction has been imported into
       gnucash. Now also add it as a job to aqbanking. */
    AB_Transaction_SetLocalBankCode (trans, 
				     AB_Account_GetBankCode (data->hbci_account));
    AB_Transaction_SetLocalAccountNumber (trans, AB_Account_GetAccountNumber (data->hbci_account));
    AB_Transaction_SetLocalCountry (trans, "DE");

    job = 
      gnc_hbci_trans_dialog_enqueue(trans, data->ab,
				    data->hbci_account, SINGLE_DEBITNOTE);

    /* Check whether we really got a job */
    if (!job) {
      /* Oops, no job, probably not supported by bank. */
      if (gnc_verify_dialog
	  (parent, 
	   FALSE,
	   "%s",
	   _("The backend found an error during the preparation "
	     "of the job. It is not possible to execute this job. \n"
	     "\n"
	     "Most probable the bank does not support your chosen "
	     "job or your HBCI account does not have the permission "
	     "to execute this job. More error messages might be "
	     "visible on your console log.\n"
	     "\n"
	     "Do you want to enter the job again?"))) {
	gnc_error_dialog (parent, "Sorry, not implemented yet.");
      }
      /* else
	 break; */
    }
    data->job_list = g_list_append(data->job_list, job);
  }

  return NULL;
}

gboolean 
gnc_hbci_multijob_execute(GtkWidget *parent, AB_BANKING *api, 
			  GList *job_list, GNCInteractor *interactor)
{
  gboolean successful;
  g_assert(api);

  successful = gnc_AB_BANKING_execute (parent, api, NULL, interactor);

  /*printf("dialog-hbcitrans: Ok, result of api_execute was %d.\n", 
    successful);*/
	  
  if (!successful) {
    /* AB_BANKING_executeOutbox failed. */
    gnc_error_dialog (GNCInteractor_dialog (interactor),
		      "%s",
		      _("Executing the HBCI outbox failed. Please check the log window."));
    GNCInteractor_show_nodelete(interactor);

    g_list_foreach (job_list, multijob_cb, GNCInteractor_dialog (interactor));
  }
  /* Watch out! The job *has* to be removed from the queue
     here because otherwise it might be executed again. */
  /* AB_Banking_DequeueJob(api, job); is done in the calling function. */
  return successful;
}

void multijob_cb (gpointer element, gpointer user_data)
{
  AB_JOB *job = element;
  GtkWidget *parent = user_data;

  if ((AB_Job_GetStatus (job) == AB_Job_StatusPending) ||
      (AB_Job_GetStatus (job) == AB_Job_StatusError)) {
    /* There was some error in this job. */
    if (AB_Job_GetType (job) == AB_Job_TypeDebitNote) {
      const AB_TRANSACTION *h_trans =
	AB_JobSingleDebitNote_GetTransaction (job);
      gchar *descr_name = gnc_hbci_descr_tognc (h_trans);
      gchar *value = 
	gnc_AB_VALUE_toReadableString (AB_Transaction_GetValue (h_trans));
      gchar *errortext;
      errortext =
	g_strdup_printf(_("A debit note has been refused by the bank. The refused debit note has the following data:\n"
			  "Remote bank code: \"%s\"\n"
			  "Remote account number: \"%s\"\n"
			  "Description and remote name: \"%s\"\n"
			  "Value: \"%s\"\n"),
			AB_Transaction_GetRemoteBankCode (h_trans),
			AB_Transaction_GetRemoteAccountNumber (h_trans),
			descr_name,
			value);
      printf ("%s", errortext);
      gnc_error_dialog (parent, "%s", errortext);
      g_free (errortext);
      g_free (descr_name);
    } else {
    gnc_error_dialog 
      (parent, "%s",
       _("One of the jobs was sent to the bank successfully, but the "
	 "bank is refusing to execute the job. Please check "
	 "the log window for the exact error message of the "
	 "bank. The line with the error message contains a "
	 "code number that is greater than 9000.\n"
	 "\n"
	 "The job has been removed from the queue."));
    /* FIXME: Might make more useful user feedback here. */
    }
  }
}

void delpending_cb (gpointer element, gpointer user_data)
{
  AB_JOB *job = element;
  AB_BANKING *ab = user_data;

  if (AB_Job_GetStatus (job) == AB_Job_StatusPending)
    AB_Banking_DelPendingJob(ab, job);
}

/** @} */
