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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
\********************************************************************/

#include "config.h"
#include "gnc-hbci-utils.h"

#include <gnome.h>
#include <errno.h>
#include <openhbci.h>
#include <openhbci/error.h>

#include "gnc-ui.h"
#include "gnc-hbci-kvp.h"
#include "gnc-ui-util.h"
#include "gnc-engine-util.h" 
#include "global-options.h"

#include "hbci-interaction.h"

/* static short module = MOD_IMPORT; */

HBCI_API *
gnc_hbci_api_new (const char *filename, gboolean allowNewFile,
		  GtkWidget *parent, GNCInteractor **inter)
{
  HBCI_API *api = NULL;
  HBCI_Error *err = NULL;
  char *errstring;
  
  if (!filename)
      return NULL;
  if (!allowNewFile && 
      (!g_file_test (filename, G_FILE_TEST_ISFILE | G_FILE_TEST_ISLINK))) 
    {
      /* ENOENT is "No such file or directory" */
      gchar *errstring = g_strdup_printf ("%s: %s", filename, strerror (ENOENT));
      gnc_warning_dialog 
	(parent,
	 /* Translators: Strings from this file are really only needed
	  * inside Germany (HBCI is not supported anywhere else). You
	  * may safely ignore strings from the import-export/hbci
	  * subdirectory in other countries. */
	 _("Error while loading OpenHBCI config file:\n  %s\n"), errstring);
      g_free (errstring);
      return NULL;
    }
  

  api = HBCI_API_new (FALSE, TRUE);
  
  err = HBCI_API_loadEnvironment (api, filename);
  if (!HBCI_Error_isOk (err) && !allowNewFile) {
    errstring = HBCI_Error_errorString (err);
    HBCI_Error_delete (err);
    gnc_warning_dialog 
	(parent,
	 /* Translators: Strings from this file are really only needed
	  * inside Germany (HBCI is not supported anywhere else). You
	  * may safely ignore strings from the import-export/hbci
	  * subdirectory in other countries. */
	 _("Error while loading OpenHBCI config file:\n  %s\n"), errstring);
    free (errstring);
    HBCI_API_delete (api);
    return NULL;
  }
  HBCI_Error_delete (err);

  if (inter)
    *inter = gnc_hbci_api_interactors (api, parent);
  else
    gnc_hbci_api_interactors (api, parent);

  return api;
}

static HBCI_API *gnc_hbci_api = NULL;
static char *gnc_hbci_configfile = NULL;
static GNCInteractor *gnc_hbci_inter = NULL;

HBCI_API * gnc_hbci_api_new_currentbook (GtkWidget *parent, 
					 GNCInteractor **inter)
{
  if (gnc_hbci_api == NULL) {
    /* No API cached -- create new one. */
    gnc_hbci_configfile = 
      g_strdup (gnc_hbci_get_book_configfile (gnc_get_current_book ()));
    gnc_hbci_api = gnc_hbci_api_new (gnc_hbci_configfile, 
				     FALSE, parent, inter);
    gnc_hbci_inter = *inter;
    return gnc_hbci_api;
  } else if ((gnc_hbci_configfile != NULL) && 
	     (strcmp(gnc_hbci_configfile, 
		     gnc_hbci_get_book_configfile (gnc_get_current_book ()))
	     != 0)) {
    /* Wrong API cached -- delete old and create new. */
    gnc_hbci_api_delete (gnc_hbci_api);
    fprintf(stderr,
	    "gnc_hbci_api_new_currentbook: Wrong HBCI_API cached; creating new one.\n");
    return gnc_hbci_api_new_currentbook (parent, inter);
  } else {
    /* Correct API cached. */
    *inter = gnc_hbci_inter;
    GNCInteractor_reparent (*inter, parent);
    return gnc_hbci_api;
  }
}

void gnc_hbci_api_delete (HBCI_API *api)
{
  if (api == gnc_hbci_api) {
    gnc_hbci_api = NULL;
    gnc_hbci_inter = NULL;
    g_free (gnc_hbci_configfile);
    gnc_hbci_configfile = NULL;
  }
  HBCI_API_delete (api);
}


HBCI_Error * gnc_hbci_api_save (const HBCI_API *api)
{
  const char *file = gnc_hbci_get_book_configfile (gnc_get_current_book ());
  if ((file == NULL) || (strlen (file) == 0)) 
    return HBCI_Error_new ("gnc_hbci_api_save", ERROR_LEVEL_NORMAL, 0, 
			   ERROR_ADVISE_ABORT, 
			   "No filename for config file.", "");
  
  return HBCI_API_saveEnvironment (api, file);
}



const HBCI_Account *
gnc_hbci_get_hbci_acc (const HBCI_API *api, Account *gnc_acc) 
{
  const char *bankcode = NULL, *accountid = NULL;
  int countrycode = 0;
  const HBCI_Bank *bank = NULL;
  const HBCI_Account *hbci_acc = NULL;

  bankcode = gnc_hbci_get_account_bankcode (gnc_acc);
  countrycode = gnc_hbci_get_account_countrycode (gnc_acc);
  if (bankcode && (strlen(bankcode)>0) && (countrycode > 0)) {
    /*printf("gnc_acc %s has blz %s and ccode %d\n",
      xaccAccountGetName (gnc_acc), bankcode, countrycode);*/
    bank = HBCI_API_findBank (api, countrycode, bankcode);
    if (bank) {
      accountid = gnc_hbci_get_account_accountid (gnc_acc);
      if (accountid && (strlen(accountid)>0)) {
	hbci_acc = HBCI_Bank_findAccount (bank, accountid);
	if (hbci_acc) {
	  /*printf("can connect gnc_acc %s to hbci_acc %s\n",
	    xaccAccountGetName (gnc_acc), 
	    HBCI_Account_accountId (hbci_acc));*/
	  return hbci_acc;
	} /* hbci_acc */
      } /* accountid */
    } /* bank */
  } /* bankcode */
  return NULL;
}


static void *
print_list_int_cb (int value, void *user_data)
{
  printf("%d, ", value);
  return NULL;
}
static void 
print_list_int (const list_int *list)
{
  g_assert(list);
  list_int_foreach (list, &print_list_int_cb, NULL);
  printf ("\n");
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

int
gnc_hbci_debug_outboxjob (HBCI_OutboxJob *job, gboolean verbose)
{
  list_int *list;
  const char *msg;
  int cause = 0;
  
  g_assert (job);
/*   if (HBCI_OutboxJob_status (job) != HBCI_JOB_STATUS_DONE) */
/*     return; */
/*   if (HBCI_OutboxJob_result (job) != HBCI_JOB_RESULT_FAILED) */
/*     return; */

  if (verbose) {
    printf("OutboxJob status: ");
    switch(HBCI_OutboxJob_status (job)) {
    case HBCI_JOB_STATUS_TODO:
      printf("todo");
      break;
    case HBCI_JOB_STATUS_DONE:
      printf("done");
      break;
    default:
    case HBCI_JOB_STATUS_NONE:
      printf("none");
      break;
    }

    printf(", result: ");
    switch(HBCI_OutboxJob_result (job)) {
    case HBCI_JOB_RESULT_SUCCESS:
      printf("success");
      break;
    case HBCI_JOB_RESULT_FAILED:
      printf("failed");
      break;
    default:
    case HBCI_JOB_STATUS_NONE:
      printf("none");
      break;
    }
    printf("\n");
  }
  
  list = HBCI_OutboxJob_resultCodes (job);
  if (list_int_size (list) > 0) {

    cause = get_resultcode_error (list);

    if (verbose) {
      printf("OutboxJob resultcodes: ");
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
      printf("Probable cause of error was: code %d, msg: %s\n", cause, msg);
    }
  } else {
    if (verbose)
      printf("OutboxJob's resultCodes list has zero length.\n");
  }
  list_int_delete (list);

  return cause;
}


gboolean
gnc_hbci_error_retry (GtkWidget *parent, HBCI_Error *error,
		      GNCInteractor *inter)
{
  int code = HBCI_Error_code (error);

  switch (code) {
  case HBCI_ERROR_CODE_PIN_WRONG:
    GNCInteractor_erasePIN (inter);
    return gnc_verify_dialog (parent,
			      TRUE,
			      _("The PIN you entered was wrong.\n"
				"Do you want to try again?"));
  case HBCI_ERROR_CODE_PIN_WRONG_0:
    GNCInteractor_erasePIN (inter);
    return gnc_verify_dialog (parent,
			      TRUE,
			      _("The PIN you entered was wrong.\n"
				"ATTENTION: You have zero further wrong retries left!\n"
				"Do you want to try again?"));
  case HBCI_ERROR_CODE_PIN_WRONG_1:
    GNCInteractor_erasePIN (inter);
    return gnc_verify_dialog (parent,
			      TRUE,
			      _("The PIN you entered was wrong.\n"
				"You have one further wrong retry left.\n"
				"Do you want to try again?"));
  case HBCI_ERROR_CODE_PIN_WRONG_2:
    GNCInteractor_erasePIN (inter);
    return gnc_verify_dialog (parent,
			      TRUE,
			      _("The PIN you entered was wrong.\n"
				"You have two further wrong retries left.\n"
				"Do you want to try again?"));
  case HBCI_ERROR_CODE_PIN_ABORTED:
    /*     printf("gnc_hbci_error_feedback: PIN dialog was aborted.\n"); */
    return FALSE;
  case HBCI_ERROR_CODE_PIN_TOO_SHORT:
    GNCInteractor_erasePIN (inter);
    return gnc_verify_dialog (parent,
			      TRUE,
			      _("The PIN you entered was too short.\n"
				"Do you want to try again?"));
  case HBCI_ERROR_CODE_CARD_DESTROYED:
    GNCInteractor_hide (inter);
    gnc_error_dialog
      (parent,
       _("Unfortunately you entered a wrong PIN for too many times.\n"
	 "Your chip card is therefore destroyed. Aborting."));
    return FALSE;
  case HBCI_ERROR_CODE_FILE_NOT_FOUND:
    /*     printf("gnc_hbci_error_feedback: File not found error.\n"); */
    return FALSE;
  case HBCI_ERROR_CODE_NO_CARD:
    return gnc_verify_dialog (parent,
			      TRUE,
			      _("No chip card has been found in the chip card reader.\n"
				"Do you want to try again?"));
  case HBCI_ERROR_CODE_JOB_NOT_SUPPORTED:
    GNCInteractor_hide (inter);
    gnc_error_dialog
      (parent,
       _("Unfortunately this HBCI job is not supported \n"
	 "by your bank or for your account. Aborting."));
    return FALSE;
  case HBCI_ERROR_CODE_SOCKET_NO_CONNECT:
    GNCInteractor_hide (inter);
    gnc_error_dialog 
      (parent,
       _("The server of your bank refused the HBCI connection.\n"
	 "Please try again later. Aborting."));
    return FALSE;
  case HBCI_ERROR_CODE_MEDIUM:
    gnc_error_dialog 
      (parent,
       _("There was an error when loading the plugin for your security medium \n"
	 "(see log window). Probably the versions of your currently installed \n"
	 "OpenHBCI library and of the plugin do not match. In that case you need \n"
	 "to recompile and reinstall the plugin again. Aborting now."));
    GNCInteractor_hide (inter);
    return FALSE;
  case HBCI_ERROR_CODE_BAD_MEDIUM:
    gnc_error_dialog 
      (parent,
       _("Your security medium is not supported. No appropriate plugin \n"
	 "has been found for that medium. Aborting."));
    GNCInteractor_hide (inter);
    return FALSE;
      
  default:
    return FALSE;
  }
  
}

gboolean
gnc_hbci_api_execute (GtkWidget *parent, HBCI_API *api,
		      HBCI_OutboxJob *job, GNCInteractor *inter)
{
  HBCI_Error *err;
  int resultcode;
	  
  if (inter)
    GNCInteractor_show (inter);

  if (gnc_lookup_boolean_option("_+Advanced", 
				"HBCI Verbose Debug Messages", FALSE))
    HBCI_Hbci_setDebugLevel (4);
  else
    HBCI_Hbci_setDebugLevel (0);

  do {
    err = HBCI_API_executeQueue (api, TRUE);
    g_assert (err);
  } while (gnc_hbci_error_retry (parent, err, inter));
  
  resultcode = gnc_hbci_debug_outboxjob (job, FALSE);
  if (!HBCI_Error_isOk(err)) {
    char *errstr = 
      g_strdup_printf("gnc_hbci_api_execute: Error at executeQueue: %s",
		      HBCI_Error_message (err));
    printf("%s\n", errstr);
    HBCI_Interactor_msgStateResponse (HBCI_Hbci_interactor 
				      (HBCI_API_Hbci (api)), errstr);
    g_free (errstr);
    HBCI_Error_delete (err);
    gnc_hbci_debug_outboxjob (job, TRUE);
    GNCInteractor_show_nodelete (inter);
    return FALSE;
  }

  if (resultcode <= 20) {
    HBCI_Error_delete (err);
    return TRUE;
  }
  else {
    printf("gnc_hbci_api_execute: Some message at executeQueue: %s",
	   HBCI_Error_message (err));
    HBCI_Error_delete (err);
    GNCInteractor_show_nodelete (inter);
    return TRUE; /* <- This used to be a FALSE but this was probably
		  * as wrong as it could get. @§%$! */
  }
}

/* Needed for the gnc_hbci_descr_tognc and gnc_hbci_memo_tognc. */
static void *gnc_list_string_cb (const char *string, void *user_data)
{
  gchar **res = user_data;
  gchar *tmp1, *tmp2;

  tmp1 = g_strdup (string);
  g_strstrip (tmp1);

  if (strlen (tmp1) > 0) {
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


char *gnc_hbci_descr_tognc (const HBCI_Transaction *h_trans)
{
  /* Description */
  char *h_descr = NULL;
  char *othername = NULL;
  char *g_descr;

  /* Don't use list_string_concat_delim here since we need to
     g_strstrip every single element of the string list, which is
     only done in our callback gnc_list_string_cb. The separator is
     also set there. */
  list_string_foreach (HBCI_Transaction_description (h_trans), 
		       &gnc_list_string_cb,
		       &h_descr);
  list_string_foreach (HBCI_Transaction_otherName (h_trans), 
		       &gnc_list_string_cb,
		       &othername);
  /*DEBUG("HBCI Description '%s'", h_descr);*/

  if (othername && (strlen (othername) > 0))
    g_descr = 
      ((h_descr && (strlen (h_descr) > 0)) ?
       g_strdup_printf ("%s; %s", 
			h_descr,
			othername) :
       g_strdup (othername));
  else
    g_descr = 
      ((h_descr && (strlen (h_descr) > 0)) ?
       g_strdup (h_descr) : 
       g_strdup (_("Unspecified")));

  free (h_descr);
  free (othername);
  return g_descr;
}

char *gnc_hbci_memo_tognc (const HBCI_Transaction *h_trans)
{
  /* Memo in the Split. HBCI's transactionText contains strings like
   * "STANDING ORDER", "UEBERWEISUNGSGUTSCHRIFT", etc.  */
  char *h_transactionText = 
    g_strdup (HBCI_Transaction_transactionText (h_trans));
  char *h_otherAccountId =
    g_strdup (HBCI_Transaction_otherAccountId (h_trans));
  char *h_otherBankCode =
    g_strdup (HBCI_Transaction_otherBankCode (h_trans));
  char *g_memo;

  g_strstrip (h_transactionText);
  g_strstrip (h_otherAccountId);
  g_strstrip (h_otherBankCode);

  g_memo = 
    (h_transactionText && (strlen(h_transactionText) > 0) ?
     g_strdup_printf ("%s %s %s %s %s",
		      h_transactionText,
		      _("Account"), h_otherAccountId,
		      _("Bank"), h_otherBankCode) :
     (h_otherAccountId && (strlen (h_otherAccountId) > 0) ?
      g_strdup_printf ("%s %s %s %s",
		       _("Account"), h_otherAccountId,
		       _("Bank"), h_otherBankCode) :
      g_strdup ("")));
    
  g_free (h_transactionText);
  g_free (h_otherAccountId);
  g_free (h_otherBankCode);
  return g_memo;
}


/** Return the only customer that can act on the specified account, or
    NULL if none was found. */
const HBCI_Customer *
gnc_hbci_get_first_customer(const HBCI_Account *h_acc)
{
  /* Get one customer. */
  const list_HBCI_Customer *custlist;
  list_HBCI_Customer_iter *iter;
  const HBCI_Customer *customer;
    
  custlist = HBCI_Account_authorizedCustomers (h_acc);
  g_assert (custlist);
  switch (list_HBCI_Customer_size (custlist)) {
  case 0:
    printf("gnc_hbci_get_first_customer: No HBCI customer found.\n");
    return NULL;
  case 1:
    break;
  default:
    gnc_warning_dialog(gnc_ui_get_toplevel (), 
		       "Sorry, Choosing one out of several HBCI Customers not yet implemented.");
    return NULL;
  }
  iter = list_HBCI_Customer_begin (custlist);
  customer = list_HBCI_Customer_iter_get (iter);
  list_HBCI_Customer_iter_delete (iter);
  return customer;
}
