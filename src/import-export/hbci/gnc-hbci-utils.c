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
#include <openhbci/error.h>

#include "gnc-ui.h"
#include "gnc-hbci-kvp.h"
#include "gnc-ui-util.h"

#include "hbci-interaction.h"

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
    return NULL;

  api = HBCI_API_new (FALSE, TRUE);
  
  err = HBCI_API_loadEnvironment (api, filename);
  if (!HBCI_Error_isOk (err) && !allowNewFile) {
    errstring = HBCI_Error_errorString (err);
    HBCI_Error_delete (err);
    gnc_warning_dialog_parented 
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
};

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
  } else if ((gnc_hbci_configfile != NULL) && 
	     (strcmp(gnc_hbci_configfile, 
		     gnc_hbci_get_book_configfile (gnc_get_current_book ()))
	     != 0)) {
    /* Wrong API cached -- delete old and create new. */
    gnc_hbci_api_delete (gnc_hbci_api);
    printf("gnc_hbci_api_new_currentbook: Wrong HBCI_API cached; creating new one.\n");
    return gnc_hbci_api_new_currentbook (parent, inter);
  }
  /* Correct API cached. */
  *inter = gnc_hbci_inter;
  return gnc_hbci_api;
};

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
    //printf("gnc_acc %s has blz %s and ccode %d\n",
    //   xaccAccountGetName (gnc_acc), bankcode, countrycode);
    bank = HBCI_API_findBank (api, countrycode, bankcode);
    if (bank) {
      accountid = gnc_hbci_get_account_accountid (gnc_acc);
      if (accountid && (strlen(accountid)>0)) {
	hbci_acc = HBCI_Bank_findAccount (bank, accountid);
	if (hbci_acc) {
	  //printf("can connect gnc_acc %s to hbci_acc %s\n",
	  // xaccAccountGetName (gnc_acc), 
	  // HBCI_Account_accountId (hbci_acc));
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
  if (value >= 9000)
    return (void*) value;
  else
    return NULL;
}
static int
get_resultcode_error (const list_int *list)
{
  g_assert (list);
  return (int) list_int_foreach (list, &get_resultcode_error_cb, NULL);
}

void 
gnc_hbci_debug_outboxjob (HBCI_OutboxJob *job)
{
  list_int *list;
  const char *msg;
  int cause;
  
  g_assert (job);
/*   if (HBCI_OutboxJob_status (job) != HBCI_JOB_STATUS_DONE) */
/*     return; */
/*   if (HBCI_OutboxJob_result (job) != HBCI_JOB_RESULT_FAILED) */
/*     return; */
  list = HBCI_OutboxJob_resultCodes (job);
  if (list_int_size (list) > 0) {
    printf("OutboxJob failed. Resultcodes were: ");
    print_list_int (list);
    cause = get_resultcode_error (list);
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
  list_int_delete (list);
}


gboolean
gnc_hbci_error_retry (GtkWidget *parent, HBCI_Error *error,
		      GNCInteractor *inter)
{
  int code = HBCI_Error_code (error);

  switch (code) {
  case HBCI_ERROR_CODE_PIN_WRONG:
    GNCInteractor_erasePIN (inter);
    return gnc_verify_dialog_parented (parent,
				       TRUE,
				       _("The PIN you entered was wrong.\n"
					 "Do you want to try again?"));
  case HBCI_ERROR_CODE_PIN_ABORTED:
    /*     printf("gnc_hbci_error_feedback: PIN dialog was aborted.\n"); */
    return FALSE;
  case HBCI_ERROR_CODE_PIN_TOO_SHORT:
    GNCInteractor_erasePIN (inter);
    return gnc_verify_dialog_parented (parent,
				       TRUE,
				       _("The PIN you entered was too short.\n"
					 "Do you want to try again?"));
  case HBCI_ERROR_CODE_FILE_NOT_FOUND:
    /*     printf("gnc_hbci_error_feedback: File not found error.\n"); */
    return FALSE;
  case HBCI_ERROR_CODE_JOB_NOT_SUPPORTED:
    GNCInteractor_hide (inter);
    gnc_error_dialog_parented 
      (GTK_WINDOW (parent),
       _("Unfortunately this HBCI job is not supported \n"
	 "by your bank or for your account. Aborting."));
    return FALSE;
      
  default:
  }
  
  return FALSE;
}

gboolean
gnc_hbci_api_execute (GtkWidget *parent, HBCI_API *api,
		      HBCI_OutboxJob *job, GNCInteractor *inter)
{
  HBCI_Error *err;
	  
  if (inter)
    GNCInteractor_show (inter);

  HBCI_Hbci_setDebugLevel(0);
  do {
    err = HBCI_API_executeQueue (api, TRUE);
    g_assert (err);
  } while (gnc_hbci_error_retry (parent, err, inter));
  
  if (!HBCI_Error_isOk(err)) {
    char *errstr = 
      g_strdup_printf("gnc_hbci_api_execute: Error at executeQueue: %s",
		      HBCI_Error_message (err));
    printf("%s\n", errstr);
    HBCI_Interactor_msgStateResponse (HBCI_Hbci_interactor 
				      (HBCI_API_Hbci (api)), errstr);
    g_free (errstr);
    HBCI_Error_delete (err);
    gnc_hbci_debug_outboxjob (job);
    return FALSE;
  }

  HBCI_Error_delete (err);
  return TRUE;
}

