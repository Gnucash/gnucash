/********************************************************************\
 * gnc-hbci-utils.c -- hbci utility functions                       *
 * Copyright (C) 2002 Christian Stimming                            *
 *                                                                  *
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

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <errno.h>
#include <iconv.h>
#include <gwenhywfar/directory.h>
#include <gwenhywfar/logger.h>

#include "gnc-ui.h"
#include "gnc-hbci-kvp.h"
#include "gnc-gconf-utils.h"
#include "gnc-ui-util.h"
#include "qof.h" 
#include "gnc-glib-utils.h"

#include "import-main-matcher.h"
#include "import-account-matcher.h"

#define AQBANKING_NOWARN_DEPRECATED
#include "gnc-hbci-utils.h"

#include "hbci-interaction.h"
#include "gnc-hbci-gettrans.h"
#include "dialog-hbcitrans.h"
#include <aqbanking/version.h>
#include <aqbanking/jobsingledebitnote.h>


/* static short module = MOD_IMPORT; */

/* Globale variables for AB_BANKING caching. */
static AB_BANKING *gnc_AB_BANKING = NULL;
static int gnc_AB_BANKING_refcnt = 0;
static GNCInteractor *gnc_hbci_inter = NULL;


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



AB_BANKING * gnc_AB_BANKING_new_currentbook (GtkWidget *parent, 
					     GNCInteractor **inter)
{
  if (gnc_AB_BANKING == NULL) {
    /* No API cached -- create new one. */
    AB_BANKING *api = NULL;
  
    api = AB_Banking_new ("gnucash", 0);
    g_assert(api);
    {
      int r = AB_Banking_Init(api);
      if (r != 0)
	g_critical("gnc_AB_BANKING_new: Warning: Error %d on AB_Banking_init\n", r);
    }
    
    gnc_hbci_inter = gnc_AB_BANKING_interactors (api, parent);
    gnc_AB_BANKING = api;
    
    if (inter)
      *inter = gnc_hbci_inter;

    gnc_AB_BANKING_refcnt = 1;
    return gnc_AB_BANKING;
  } else {
    /* API cached. */

    /* Init the API again. */
    if (gnc_AB_BANKING_refcnt == 0)
      AB_Banking_Init(gnc_AB_BANKING);

    if (inter) {
      *inter = gnc_hbci_inter;
      GNCInteractor_reparent (*inter, parent);
    }
    
    gnc_AB_BANKING_refcnt++;
    return gnc_AB_BANKING;
  }
}

void gnc_AB_BANKING_delete (AB_BANKING *api)
{
  if (api == 0)
    api = gnc_AB_BANKING;

  if (api) {
    if (api == gnc_AB_BANKING) {
      gnc_AB_BANKING = NULL;
      gnc_hbci_inter = NULL;
      if (gnc_AB_BANKING_refcnt > 0)
	AB_Banking_Fini(api);
    }

    AB_Banking_free(api);
  }
}


int gnc_AB_BANKING_fini (AB_BANKING *api) 
{
  if (api == gnc_AB_BANKING) {
    gnc_AB_BANKING_refcnt--;
    if (gnc_AB_BANKING_refcnt == 0)
      return AB_Banking_Fini(api);
  }
  else
    return AB_Banking_Fini(api);
  return 0;
}




AB_ACCOUNT *
gnc_hbci_get_hbci_acc (const AB_BANKING *api, Account *gnc_acc) 
{
  int account_uid = 0;
  AB_ACCOUNT *hbci_acc = NULL;
  const char *bankcode = NULL, *accountid = NULL;

  bankcode = gnc_hbci_get_account_bankcode (gnc_acc);
  accountid = gnc_hbci_get_account_accountid (gnc_acc);
  account_uid = gnc_hbci_get_account_uid (gnc_acc);
  if (account_uid > 0) {
    /*printf("gnc_hbci_get_hbci_acc: gnc_acc %s has blz %s and ccode %d\n",
      xaccAccountGetName (gnc_acc), bankcode, countrycode);*/
    hbci_acc = AB_Banking_GetAccount(api, account_uid);

    if (!hbci_acc && bankcode && (strlen(bankcode)>0) &&
	accountid && (strlen(accountid) > 0)) {
      g_message("gnc_hbci_get_hbci_acc: No AB_ACCOUNT found for UID %d, trying bank code\n", account_uid);
      hbci_acc = AB_Banking_GetAccountByCodeAndNumber(api, bankcode, accountid);
    }
    /* g_message("gnc_hbci_get_hbci_acc: return HBCI_Account %p\n", hbci_acc); */
    return hbci_acc;
  } else if (bankcode && (strlen(bankcode)>0) && accountid && (strlen(accountid) > 0)) {
    hbci_acc = AB_Banking_GetAccountByCodeAndNumber(api, bankcode, accountid);
    return hbci_acc;
  }

  return NULL;
}

#if 0
static void *
print_list_int_cb (int value, void *user_data)
{
  g_warning("%d, ", value);
  return NULL;
}
static void 
print_list_int (const list_int *list)
{
  g_assert(list);
  list_int_foreach (list, &print_list_int_cb, NULL);
  g_warning ("\n");
}
static void *
get_resultcode_error_cb (int value, void *user_data)
{
  int *tmp_result = user_data;
  if (value > *tmp_result)
    *tmp_result = value;
  if (value >= 9000)
    return (void*) value;
  else
    return NULL;
}
static int
get_resultcode_error (const list_int *list)
{
  int tmp_result = 0, cause = 0;
  g_assert (list);
  cause = (int) list_int_foreach (list, &get_resultcode_error_cb, &tmp_result);
  return MAX(tmp_result, cause);
}
#endif

/** Return the HBCI return code of the given 'job', or zero if none was
 * found. If 'verbose' is TRUE, make a lot of debugging messages about
 * this outboxjob. */
static int
gnc_hbci_debug_outboxjob (GNCInteractor *inter, AB_JOB *job, gboolean verbose)
{
  int cause = 0;
  AB_JOB_STATUS jobstatus;
  
  g_assert (job);

  if (verbose) {
    g_warning("gnc_hbci_debug_outboxjob: Job status: %s", AB_Job_Status2Char(AB_Job_GetStatus(job)));

    g_warning(", result: %s", AB_Job_GetResultText(job) ? AB_Job_GetResultText(job) : "(none)");
    g_warning("\n");
  }

  jobstatus = AB_Job_GetStatus (job);
  if (jobstatus == AB_Job_StatusError) {
    if (AB_Job_GetResultText (job)) {
      /* Add the "result text" to the log window */
      char *logstring = g_strdup_printf("Job %s had an error: %s\n",
					AB_Job_Type2Char(AB_Job_GetType(job)),
					AB_Job_GetResultText(job));
      GNCInteractor_add_log_text (inter, logstring);
      g_free (logstring);
    }

    if (!verbose)
      g_warning("gnc_hbci_debug_outboxjob: Job %s had an error: %s\n",
	     AB_Job_Type2Char(AB_Job_GetType(job)),
	     AB_Job_GetResultText(job) ? AB_Job_GetResultText(job) : "(none)");
    cause = 9000;
  } else {
    cause = 0;
  }

#if 0  
  /* hbci debugging code; might be adapted to aqbanking at a later
     point in time */
  list = AB_JOB_resultCodes (job);
  if (list_int_size (list) > 0) {

    cause = get_resultcode_error (list);

    if (verbose) {
      g_warning("OutboxJob resultcodes: ");
      print_list_int (list);

      switch (cause) {
      case 9310:
	msg = "Schluessel noch nicht hinterlegt";
	break;
      case 9320:
	msg = "Schluessel noch nicht freigeschaltet";
	break;
      case 9330:
	msg = "Schluessel gesperrt";
	break;
      case 9340:
	msg = "Schluessel falsch";
	break;
      default:
	msg = "Unknown";
      }
      g_warning("Probable cause of error was: code %d, msg: %s\n", cause, msg);
    }
  } else {
    if (verbose)
      g_warning("OutboxJob's resultCodes list has zero length.\n");
  }
  list_int_delete (list);
#endif

  return cause;
}

void
gnc_hbci_cleanup_job(AB_BANKING *api, AB_JOB *job)
{
  if (AB_Job_GetStatus(job) == AB_Job_StatusFinished) {
    AB_Banking_DelFinishedJob(api, job);
  } else if (AB_Job_GetStatus(job) == AB_Job_StatusPending) {
    AB_Banking_DelPendingJob(api, job);
  }
  /* Martin assured me that there will be no job in the queue after
     ExecuteQueue, so we don't need to remove it from the queue. */
}


gboolean
gnc_hbci_Error_retry (GtkWidget *parent, int error, 
		      GNCInteractor *inter)
{
  int code = error;

  switch (code) {
#if 0
    /* these error codes existed in openhbci, but no longer in
       aqbanking. Maybe they might get reintroduced later, but maybe
       not. */
  case AB_ERROR_PIN_WRONG:
    GNCInteractor_erasePIN (inter);
    return gnc_verify_dialog (parent,
				       TRUE,
				       _("The PIN you entered was wrong.\n"
					 "Do you want to try again?"));
  case AB_ERROR_PIN_WRONG_0:
    GNCInteractor_erasePIN (inter);
    return gnc_verify_dialog (parent,
				       TRUE,
				       _("The PIN you entered was wrong.\n"
					 "ATTENTION: You have zero further wrong retries left!\n"
					 "Do you want to try again?"));
  case AB_ERROR_CARD_DESTROYED:
    GNCInteractor_hide (inter);
    gnc_error_dialog
      (parent,
       _("Unfortunately you entered a wrong PIN for too many times. "
	 "Your chip card is therefore destroyed. Aborting."));
    return FALSE;
  case AB_ERROR_FILE_NOT_FOUND:
    /*     g_warning("gnc_hbci_Error_feedback: File not found error.\n"); */
    return FALSE;
  case AB_ERROR_NO_CARD:
    return gnc_verify_dialog (parent,
			      TRUE,
			      _("No chip card has been found in the chip card reader. "
				"Do you want to try again?"));
  case AB_ERROR_JOB_NOT_SUPPORTED:
    GNCInteractor_hide (inter);
    gnc_error_dialog 
      (parent,
       _("Unfortunately this Online Banking job is not supported "
	 "by your bank or for your account. Aborting."));
    return FALSE;
#endif
  case AB_ERROR_NETWORK:
    if (inter) GNCInteractor_hide (inter);
    gnc_error_dialog 
      (parent,
       _("The server of your bank refused the Online Banking connection. "
	 "Please try again later. Aborting."));
    return FALSE;
      
  default:
    return FALSE;
  }
  
}

#if 0
/* hbci debugging code; might be adapted to aqbanking at a later
   point in time */

/* Prints all results that can be found in the outbox into the interactor */
static void gnc_hbci_printresult(HBCI_Outbox *outbox, GNCInteractor *inter)
{
  /* Got no sysid. */
  GWEN_DB_NODE *rsp, *n;
  g_assert(outbox);
  if (!inter) 
    return;
  
  rsp = HBCI_Outbox_response(outbox);
  n = GWEN_DB_GetFirstGroup(rsp);
  while(n) {
    if (strcasecmp(GWEN_DB_GroupName(n), "msgresult")==0) {
      GWEN_DB_NODE *r = GWEN_DB_GetFirstGroup(n);
      while (r) {
	if (strcasecmp(GWEN_DB_GroupName(r), "result") == 0) {
	  gchar *logtext;
	  int resultcode;
	  const char *text, *elementref, *param;
	  
	  resultcode = GWEN_DB_GetIntValue(r, "resultcode", 0, 0);
	  text = GWEN_DB_GetCharValue(r, "text", 0, "Response without text");
	  elementref = GWEN_DB_GetCharValue(r, "elementref", 0, "");
	  param = GWEN_DB_GetCharValue(r, "param", 0, "");

	  if (strlen(elementref)>0 || strlen(param) > 0)
	    logtext = g_strdup_printf("%s (%d; Elementref %s; Param %s)", text, 
				      resultcode, elementref, param);
	  else
	    logtext = g_strdup_printf("%s (%d)", text, resultcode);
	  GNCInteractor_add_log_text(inter, logtext);
	  g_free(logtext);
	}
	r = GWEN_DB_GetNextGroup(r);
      }
    } 
    else if (strcasecmp(GWEN_DB_GroupName(n), "segresult")==0) {
      GWEN_DB_NODE *r = GWEN_DB_GetFirstGroup(n);
      while (r) {
	if (strcasecmp(GWEN_DB_GroupName(r), "result") == 0) {
	}
	r = GWEN_DB_GetNextGroup(r);
      }
    } 
    n=GWEN_DB_GetNextGroup(n);
  } /* while */

  GWEN_DB_Group_free(rsp);
}
#endif

/* ------------------------------------------------------- */

static gboolean hbci_Error_isOk(int err) {
  switch (err) {
  case 0:
    return TRUE;
  default:
    return FALSE;
  };
}

gboolean
gnc_AB_BANKING_execute (GtkWidget *parent, AB_BANKING *api,
			AB_JOB *job, GNCInteractor *inter)
{
  int err;
  int resultcode = 3000; /* This code means: Warnings, but not errors */
  int be_verbose = FALSE;
  g_assert (api);
	  
  if (inter)
    GNCInteractor_show (inter);

  if (gnc_gconf_get_bool(GCONF_SECTION, KEY_VERBOSE_DEBUG, NULL)) {
    GWEN_Logger_SetLevel(GWEN_LOGDOMAIN, GWEN_LoggerLevelNotice);
    GWEN_Logger_SetLevel(AQBANKING_LOGDOMAIN, GWEN_LoggerLevelInfo);
    GWEN_Logger_SetLevel("aqhbci", GWEN_LoggerLevelInfo);
    be_verbose = TRUE;
  }
  else {
    GWEN_Logger_SetLevel(GWEN_LOGDOMAIN, GWEN_LoggerLevelError);
    GWEN_Logger_SetLevel(AQBANKING_LOGDOMAIN, GWEN_LoggerLevelError);
    GWEN_Logger_SetLevel("aqhbci", GWEN_LoggerLevelError);
  }

  do {
    if (inter) {
      GNCInteractor_show_nodelete (inter);
      AB_Banking_SetPinCacheEnabled (api, GNCInteractor_get_cache_valid(inter));
    }

    err = AB_Banking_ExecuteQueue (api);

    /* Print result codes to interactor */
/*     gnc_hbci_printresult(queue, inter); */
    
  } while (gnc_hbci_Error_retry (parent, err, inter));
  
  if (job)
    resultcode = gnc_hbci_debug_outboxjob (inter, job, be_verbose);
  if (!hbci_Error_isOk(err)) {
    if (job) gnc_hbci_debug_outboxjob (inter, job, TRUE);
    if (inter) GNCInteractor_show_nodelete (inter);
    return FALSE;
  }

  GNCInteractor_set_cache_valid (inter, TRUE);
  if (resultcode <= 20 && (! GNCInteractor_errorsLogged (inter)) ) {
    return TRUE;
  }
  else {
    g_message("gnc_AB_BANKING_execute: Some error at executeQueue (see gwen/aqbanking messages above); this does not necessarily mean that the results are unusable.");
    GNCInteractor_show_nodelete (inter);
    return TRUE; /* <- This used to be a FALSE but this was probably
		  * as wrong as it could get. @%$! */
  }
}

static void multijob_cb (gpointer element, gpointer user_data);

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
		      _("Executing the Online Banking outbox failed. Please check the log window."));
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
      g_warning ("%s", errortext);
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


/* ------------------------------------------------------- */

/* Callback declarations */
static const AB_TRANSACTION *
translist_cb (const AB_TRANSACTION *element, void *user_data);
static AB_IMEXPORTER_ACCOUNTINFO *
accountinfolist_cb(AB_IMEXPORTER_ACCOUNTINFO *element, void *user_data);

struct import_data 
{
  Account *gnc_acc;
  GNCImportMainMatcher *importer_generic;
  AB_BANKING *ab;
  AB_ACCOUNT *hbci_account;
  GList *job_list;
  gboolean execute_transactions;
};


GList *
gnc_hbci_import_ctx(AB_BANKING *ab, AB_IMEXPORTER_CONTEXT *ctx,
		    GNCImportMainMatcher *importer_generic_gui,
		    gboolean exec_as_aqbanking_jobs)
{
  struct import_data data;
  data.importer_generic = importer_generic_gui;
  data.ab = ab;
  data.job_list = NULL;
  data.execute_transactions = exec_as_aqbanking_jobs;
  
  /* Iterate through all accounts */
  AB_ImExporterContext_AccountInfoForEach(ctx, accountinfolist_cb, &data);
  /* All accounts finished. Finished importing. */
  return data.job_list;
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
	gnc_error_dialog (NULL, _("No Online Banking account found for this gnucash account. These transactions will not be executed by Online Banking."));
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
	     "job or your Online Banking account does not have the permission "
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


/* ------------------------------------------------------- */

static void delpending_cb (gpointer element, gpointer user_data);

void
gnc_hbci_clearqueue(AB_BANKING *ab, GList *ab_job_list)
{
  g_list_foreach (ab_job_list, delpending_cb, ab);
}

void delpending_cb (gpointer element, gpointer user_data)
{
  AB_JOB *job = element;
  AB_BANKING *ab = user_data;

  if (AB_Job_GetStatus (job) == AB_Job_StatusPending)
    AB_Banking_DelPendingJob(ab, job);
}

/* ------------------------------------------------------- */

struct cb_struct {
  gchar **result;
  GIConv gnc_iconv_handler;
};

/* Needed for the gnc_hbci_descr_tognc and gnc_hbci_memo_tognc. */
static void *gnc_list_string_cb (const char *string, void *user_data)
{
  struct cb_struct *u = user_data;
  gchar **res = u->result;
  gchar *tmp1, *tmp2;

  if (!string) return NULL;
  tmp1 = gnc_call_iconv(u->gnc_iconv_handler, string);

  g_strstrip (tmp1);
  if (strlen (tmp1) > 0) {
    /* Ensure string is in utf8 */
    gnc_utf8_strip_invalid (tmp1);

    if (*res != NULL) {
      /* The " " is the separating string in between each two strings. */
      tmp2 = g_strjoin (" ", *res, tmp1, NULL);
      g_free (tmp1);
      
      g_free (*res);
      *res = tmp2;
    }
    else {
      *res = tmp1;
    }
  }
  
  return NULL;
}


char *gnc_hbci_descr_tognc (const AB_TRANSACTION *h_trans)
{
  /* Description */
  char *h_descr = gnc_hbci_getpurpose (h_trans);
  char *othername = gnc_hbci_getremotename (h_trans);
  char *g_descr;

  /* Get othername */
  /*DEBUG("HBCI Description '%s'", h_descr);*/

  if (othername && strlen (othername) > 0)
    g_descr = 
      ((strlen (h_descr) > 0) ?
       g_strdup_printf ("%s; %s", 
			h_descr,
			othername) :
       g_strdup (othername));
  else
    g_descr = 
      ((strlen (h_descr) > 0) ?
       g_strdup (h_descr) : 
       g_strdup (_("Unspecified")));

  g_free (h_descr);
  if (othername) g_free (othername);
  return g_descr;
}

char *gnc_hbci_getremotename (const AB_TRANSACTION *h_trans)
{
  /* Description */
  char *othername = NULL;
  char *result;
  const GWEN_STRINGLIST *h_remotename = AB_Transaction_GetRemoteName (h_trans);
  struct cb_struct cb_object;

  cb_object.gnc_iconv_handler = 
    g_iconv_open(gnc_hbci_book_encoding(), gnc_hbci_AQBANKING_encoding());
  g_assert(cb_object.gnc_iconv_handler != (GIConv)(-1));

  /* Get othername */
  cb_object.result = &othername;
  if (h_remotename)
    GWEN_StringList_ForEach (h_remotename,
			     &gnc_list_string_cb,
			     &cb_object);
  /*DEBUG("HBCI Description '%s'", h_descr);*/

  if (othername && (strlen (othername) > 0))
    result = g_strdup (othername);
  else
    result = NULL;

  g_iconv_close(cb_object.gnc_iconv_handler);
  g_free (othername);
  return result;
}

char *gnc_hbci_getpurpose (const AB_TRANSACTION *h_trans)
{
  /* Description */
  char *h_descr = NULL;
  char *g_descr;
  const GWEN_STRINGLIST *h_purpose = AB_Transaction_GetPurpose (h_trans);
  struct cb_struct cb_object;

  cb_object.gnc_iconv_handler = 
    g_iconv_open(gnc_hbci_book_encoding(), gnc_hbci_AQBANKING_encoding());
  g_assert(cb_object.gnc_iconv_handler != (GIConv)(-1));

  cb_object.result = &h_descr;
  if (h_purpose)
    GWEN_StringList_ForEach (h_purpose,
			     &gnc_list_string_cb,
			     &cb_object);

  g_descr = g_strdup (h_descr ? h_descr : "");

  g_iconv_close(cb_object.gnc_iconv_handler);
  g_free (h_descr);
  return g_descr;
}

char *gnc_hbci_memo_tognc (const AB_TRANSACTION *h_trans)
{
  /* Memo in the Split. HBCI's transactionText contains strings like
   * "STANDING ORDER", "UEBERWEISUNGSGUTSCHRIFT", etc.  */
  /*   char *h_transactionText =  */
  /*     g_strdup (AB_TRANSACTION_transactionText (h_trans)); */
  const char *h_remoteAccountNumber = 
    AB_Transaction_GetRemoteAccountNumber (h_trans);
  const char *h_remoteBankCode = 
    AB_Transaction_GetRemoteBankCode (h_trans);
  char *h_otherAccountId =
    g_strdup (h_remoteAccountNumber ? h_remoteAccountNumber : _("unknown"));
  char *h_otherBankCode =
    g_strdup (h_remoteBankCode ? h_remoteBankCode : _("unknown"));
  char *g_memo;

  /*   g_strstrip (h_transactionText); */
  g_strstrip (h_otherAccountId);
  g_strstrip (h_otherBankCode);
  /* Ensure string is in utf8 */
  gnc_utf8_strip_invalid (h_otherAccountId);
  gnc_utf8_strip_invalid (h_otherBankCode);

  g_memo = 
    (h_otherAccountId && (strlen (h_otherAccountId) > 0) ?
     g_strdup_printf ("%s %s %s %s",
		      _("Account"), h_otherAccountId,
		      _("Bank"), h_otherBankCode) :
     g_strdup (""));
  gnc_utf8_strip_invalid (g_memo);
    
  g_free (h_otherAccountId);
  g_free (h_otherBankCode);
  return g_memo;
}

char *gnc_AB_VALUE_toReadableString(const AB_VALUE *v)
{
  char tmp[100];
  if (v)
    sprintf(tmp, "%.2f %s", AB_Value_GetValue(v), AB_Value_GetCurrency(v));
  else
    sprintf(tmp, "%.2f", 0.0);
  return g_strdup(tmp);
}

/* Note: In the gnome2-branch we don't need the iconv(3) conversion
   and gnc_call_iconv() anymore since the gnc_book has an UTF-8
   encoding and the AqBanking library also expects all strings in
   UTF-8. Nevertheless we keep all these functions for now, just in
   case they might be needed later.
*/

/* Returns a newly allocated gchar, converted according to the given
   handler */
gchar *gnc_call_iconv(GIConv handler, const gchar* input)
{
  gchar *inbuffer = (gchar*)input;
  gchar *outbuffer, *outbufferstart;
  gsize inbytes, outbytes;

  inbytes = strlen(inbuffer);
  outbytes = inbytes + 2;
  outbufferstart = g_strndup(inbuffer, outbytes);
  outbuffer = outbufferstart;
  g_iconv(handler, &inbuffer, &inbytes, &outbuffer, &outbytes);
  if (outbytes > 0) 
    *outbuffer = '\0';
  return outbufferstart;
}

const char *gnc_hbci_book_encoding()
{
  return "UTF-8";
}

const char *gnc_hbci_AQBANKING_encoding()
{
  return "UTF-8";
}
