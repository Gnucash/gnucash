/********************************************************************\
 * gnc-hbci-getbalance.c -- hbci getbalance functions               *
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
#include "gnc-hbci-getbalance.h"

#include <openhbci2/api.h>

#include "gnc-ui.h"
#include "gnc-numeric.h"
#include "gnc-date.h"
#include "RecnWindow.h"

#include "hbci-interaction.h"
#include "gnc-hbci-utils.h"
#include "dialog-hbcitrans.h"


void gnc_hbci_getbalance_debugprint(HBCI_OutboxJob *balance_job,
				    const gnc_HBCI_Account *h_acc);

static void 
bal_print_debug(const char *name,
		const HBCI_Value *val,
		gboolean negative,
		const char *date_str,
		const char *time_str)
{
  char *str = HBCI_Value_toReadableString (val);
  printf("GetBalance: %s%s %s at date %s %s",
	 (negative ? "-" : ""), str, 
	 name, date_str, time_str);
  free (str);
}


void
gnc_hbci_getbalance (GtkWidget *parent, Account *gnc_acc)
{
  HBCI_API *api = NULL;
  HBCI_Outbox *outbox = NULL;
  const gnc_HBCI_Account *h_acc = NULL;
  GNCInteractor *interactor = NULL;
  const HBCI_Customer *customer = NULL;
  GList *hbci_accountlist = NULL;
  
  g_assert(parent);
  if (gnc_acc == NULL)
    return;

  /* Get API */
  api = gnc_hbci_api_new_currentbook (parent, &interactor, &hbci_accountlist);
  if (api == NULL) {
    printf("gnc_hbci_getbalance: Couldn't get HBCI API.\n");
    return;
  }
  g_assert (interactor);

  /* Get HBCI account */
  h_acc = gnc_hbci_get_hbci_acc (api, gnc_acc);
  if (h_acc == NULL) {
    printf("gnc_hbci_getbalance: No HBCI account found.\n");
    return;
  }
  /* printf("gnc_hbci_getbalance: HBCI account no. %s found.\n",
     gnc_HBCI_Account_accountId (h_acc)); */
  
  /* Get the customer that should be doing this job. */
  customer = gnc_hbci_get_first_customer(h_acc);
  if (!customer) 
    return;
  /* printf("gnc_hbci_getbalance: Customer id %s found.\n",
     HBCI_Customer_custId ((HBCI_Customer *)customer)); */

  {
    /* Execute a GetBalance job. */
    HBCI_OutboxJob *job;

    job = HBCI_OutboxJob_new("JobGetBalance", (HBCI_Customer *)customer, 
			     gnc_HBCI_Account_accountId(h_acc));
    
    /* Add job to API queue */
    outbox = HBCI_Outbox_new();
    HBCI_Outbox_addJob (outbox, job);

    /* Execute Outbox. */
    if (!gnc_hbci_api_execute (parent, api, outbox, job, interactor)) {

      /* HBCI_API_executeOutbox failed. */
      HBCI_Outbox_removeByStatus (outbox, HBCI_JOB_STATUS_NONE);
      return;
    }

    /* gnc_hbci_getbalance_debugprint(balance_job, h_acc); */
    
    /* Finish this job. */
    gnc_hbci_getbalance_finish (parent, 
				gnc_acc,
				job);

    /* Clean up after ourselves. */
    HBCI_Outbox_removeByStatus (outbox, HBCI_JOB_STATUS_NONE);
    HBCI_Outbox_delete(outbox);
    gnc_hbci_api_save (api);
    GNCInteractor_hide (interactor);
  }
}



void gnc_hbci_getbalance_debugprint(HBCI_OutboxJob *job,
				    const gnc_HBCI_Account *h_acc)
{
  GWEN_DB_NODE *response, *acc_bal;
  GWEN_DB_NODE *noted_grp, *booked_grp;
  HBCI_Value *booked_val, *noted_val;
  /* time_t balance_tt, noted_tt, booked_tt; */
	
  response = HBCI_Job_responseData(HBCI_OutboxJob_Job(job));
  if (!response)
    return;
  acc_bal =GWEN_DB_GetGroup(response, 
			    GWEN_PATH_FLAGS_NAMEMUSTEXIST, "balance");
  if (!acc_bal) 
    return;

  noted_grp = GWEN_DB_GetGroup(response, GWEN_PATH_FLAGS_NAMEMUSTEXIST, "noted");
  booked_grp = GWEN_DB_GetGroup(response, GWEN_PATH_FLAGS_NAMEMUSTEXIST, "booked");

  booked_val = HBCI_Value_new(GWEN_DB_GetCharValue(booked_grp, "value", 0, "0"),
			      GWEN_DB_GetCharValue(booked_grp, "currency", 0, "EUR"));
  noted_val = HBCI_Value_new(GWEN_DB_GetCharValue(noted_grp, "value", 0, "0"),
			     GWEN_DB_GetCharValue(noted_grp, "currency", 0, "EUR"));
    
  printf("GetBalance: Balances for account %s :\n",
	 gnc_HBCI_Account_accountId (h_acc));
  bal_print_debug("Booked balance",
		  booked_val,
		  (strcasecmp(GWEN_DB_GetCharValue(booked_grp, "debitmark", 0, "C"),"D")==0),
		  GWEN_DB_GetCharValue(booked_grp, "date", 0, ""),
		  GWEN_DB_GetCharValue(booked_grp, "time", 0, ""));
  bal_print_debug("Noted balance",
		  noted_val,
		  (strcasecmp(GWEN_DB_GetCharValue(noted_grp, "debitmark", 0, "C"),"D")==0),
		  GWEN_DB_GetCharValue(noted_grp, "date", 0, ""),
		  GWEN_DB_GetCharValue(noted_grp, "time", 0, ""));
/*   bal_print_debug("Bank Line",  */
/* 		  gnc_HBCI_AccountBalance_bankLine (acc_bal), FALSE, */
/* 		  balance_tt); */
/*   bal_print_debug("Disposable amount", */
/* 		  gnc_HBCI_AccountBalance_disposable (acc_bal), FALSE, */
/* 		  balance_tt); */
/*   bal_print_debug("Already disposed", */
/* 		  gnc_HBCI_AccountBalance_disposed (acc_bal), FALSE, */
/* 		  balance_tt); */
  HBCI_Value_delete(booked_val);
  HBCI_Value_delete(noted_val);
}

static gchar*
bal_print_balance(const char *format,
		  const HBCI_Value *val,
		  gboolean negative)
{
  char *str = HBCI_Value_toReadableString (val);
  char *res = g_strdup_printf(format, 
			      (negative ? "-" : ""), 
			      str);
  free (str);
  return res;
}



gboolean
gnc_hbci_getbalance_finish (GtkWidget *parent, 
			    Account *gnc_acc,
			    const HBCI_OutboxJob *job)
{
  GWEN_DB_NODE *response, *acc_bal;
  GWEN_DB_NODE *noted_grp, *booked_grp;
  HBCI_Value *booked_val, *noted_val;

  time_t booked_tt;

  gboolean booked_debit, noted_debit;
  gboolean dialogres;
	    
  response = HBCI_Job_responseData((HBCI_Job*)HBCI_OutboxJob_Job_const(job));
  if (!response)
    return TRUE;
  acc_bal =GWEN_DB_GetGroup(response, 
			    GWEN_PATH_FLAGS_NAMEMUSTEXIST, "balance");
  if (!acc_bal) 
    return TRUE;

  noted_grp = GWEN_DB_GetGroup(response, GWEN_PATH_FLAGS_NAMEMUSTEXIST, "noted");
  booked_grp = GWEN_DB_GetGroup(response, GWEN_PATH_FLAGS_NAMEMUSTEXIST, "booked");

  booked_val = HBCI_Value_new(GWEN_DB_GetCharValue(booked_grp, "value", 0, "0"),
			      GWEN_DB_GetCharValue(booked_grp, "currency", 0, "EUR"));
  booked_debit = (strcasecmp(GWEN_DB_GetCharValue(booked_grp, "debitmark", 0, "C"),"D")==0);
  
  noted_val = HBCI_Value_new(GWEN_DB_GetCharValue(noted_grp, "value", 0, "0"),
			     GWEN_DB_GetCharValue(noted_grp, "currency", 0, "EUR"));
  noted_debit = (strcasecmp(GWEN_DB_GetCharValue(noted_grp, "debitmark", 0, "C"),"D")==0);
  
  {
    HBCI_Date *a = HBCI_Date_new_string(GWEN_DB_GetCharValue(booked_grp, "date", 0, ""), 4);
    
    booked_tt = HBCI_Date_to_time_t (a);
    HBCI_Date_delete(a);
  }
    
  if ((HBCI_Value_getValue (noted_val) == 0) &&
      (HBCI_Value_getValue (booked_val) == 0))
    {
      gnome_ok_dialog_parented 
	/* Translators: Strings from this file are really only
	 * needed inside Germany (HBCI is not supported anywhere
	 * else). You may safely ignore strings from the
	 * import-export/hbci subdirectory in other countries.
	 */
	(_("The downloaded HBCI Balance was zero.\n"
	   "Either this is the correct balance, or your bank does not \n"
	   "support Balance download in this HBCI version. In the latter \n"
	   "case you should choose a higher HBCI version number in the HBCI \n"
	   "Setup. After that, try again to download the HBCI Balance.\n"),
	 GTK_WINDOW (parent));
      dialogres = FALSE;
    }
  else
    {
      char *booked_str = HBCI_Value_toReadableString (booked_val);
      char *message1 = g_strdup_printf
	(
	 /* Translators: The first %s is "-" if this amount is
	  * negative or "" if it is positive. The second %s is the
	  * amount. */
	 _("Result of HBCI job: \n"
	   "Account booked balance is %s%s\n"),
	 (booked_debit ? "-" : ""),
	 booked_str);
      char *message2 = 
	((HBCI_Value_getValue (noted_val) == 0) ?
	 g_strdup_printf("%s", "") :
	 bal_print_balance
	 /* Translators: The first %s is "-" if this amount is
	  * negative or "" if it is positive. The second %s is the
	  * amount. */
	 (_("For your information: This account also \n"
	    "has a noted balance of %s%s\n"),
	  noted_val,
	  noted_debit));
      const char *message3 = _("Reconcile account now?");

      dialogres = gnc_verify_dialog
	(parent, 
	 TRUE,
	 "%s%s\n%s",
	 message1, message2, message3);

      g_free (message1);
      g_free (message2);
      free (booked_str);
    }

      
  if (dialogres) 
    {
      gnc_numeric abs_value =
	double_to_gnc_numeric (HBCI_Value_getValue (booked_val),
			       xaccAccountGetCommoditySCU(gnc_acc),
			       GNC_RND_ROUND);
      recnWindowWithBalance (parent, 
			     gnc_acc, 
			     (booked_debit 
			      ? gnc_numeric_neg (abs_value)
			      : abs_value),
			     booked_tt);
    }
      
  return TRUE;
}
