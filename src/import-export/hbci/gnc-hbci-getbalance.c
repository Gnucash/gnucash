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

#include <aqbanking/banking.h>

#include "gnc-ui.h"
#include "gnc-numeric.h"
#include "gnc-date.h"
#include "RecnWindow.h"

#include "hbci-interaction.h"
#include "gnc-hbci-utils.h"
/* #include "dialog-hbcitrans.h" */


void gnc_hbci_getbalance_debugprint(AB_JOB *balance_job,
				    const AB_ACCOUNT *h_acc);

#if 0
static void 
bal_print_debug(const char *name,
		const AB_VALUE *val,
		gboolean negative,
		const char *date_str,
		const char *time_str)
{
  char *str = gnc_AB_VALUE_toReadableString (val);
  printf("GetBalance: %s%s %s at date %s %s",
	 (negative ? "-" : ""), str, 
	 name, date_str, time_str);
  free (str);
}
#endif 


void
gnc_hbci_getbalance (GtkWidget *parent, Account *gnc_acc)
{
  AB_BANKING *api = NULL;
  const AB_ACCOUNT *h_acc = NULL;
  GNCInteractor *interactor = NULL;
  
  g_assert(parent);
  if (gnc_acc == NULL)
    return;

  /* Get API */
  api = gnc_AB_BANKING_new_currentbook (parent, &interactor);
  if (api == NULL) {
    printf("gnc_hbci_getbalance: Couldn't get AB_BANKING API.\n");
    return;
  }
  g_assert (interactor);

  /* Get HBCI account */
  h_acc = gnc_hbci_get_hbci_acc (api, gnc_acc);
  if (h_acc == NULL) {
    printf("gnc_hbci_getbalance: No HBCI account found.\n");
    /* FIXME: free unneeded data */
    return;
  }
  /* printf("gnc_hbci_getbalance: HBCI account no. %s found.\n",
     AB_ACCOUNT_accountId (h_acc)); */
  
  {
    /* Execute a GetBalance job. */
    AB_JOB *job;

    job = AB_JobGetBalance_new((AB_ACCOUNT*)h_acc);
    if (AB_Job_CheckAvailability(job)) {
      printf("gnc_hbci_getbalance: JobGetBalance not avaiable for this account.\n");
      /* FIXME: free unneeded data */
      return;
    }
    
    /* Add job to API queue */
    AB_Banking_EnqueueJob(api, job);

    /* Execute Outbox. */
    if (!gnc_AB_BANKING_execute (parent, api, job, interactor)) {

      /* AB_BANKING_executeOutbox failed. */
      AB_Banking_DequeueJob(api, job);
      AB_Banking_DelFinishedJob(api, job);
      AB_Banking_DelPendingJob(api, job);
      /* FIXME: free unneeded data */
      return;
    }

    /* gnc_hbci_getbalance_debugprint(balance_job, h_acc); */
    
    /* Finish this job. */
    gnc_hbci_getbalance_finish (parent, 
				gnc_acc,
				job);

    /* Clean up after ourselves. */
    AB_Banking_DequeueJob(api, job);
    AB_Banking_DelFinishedJob(api, job);
    AB_Banking_DelPendingJob(api, job);
    gnc_AB_BANKING_fini (api);
    GNCInteractor_hide (interactor);
  }
}


#if 0
void gnc_hbci_getbalance_debugprint(AB_JOB *job,
				    const AB_ACCOUNT *h_acc)
{
  GWEN_DB_NODE *response, *acc_bal;
  GWEN_DB_NODE *noted_grp, *booked_grp;
  AB_VALUE *booked_val, *noted_val;
  /* time_t balance_tt, noted_tt, booked_tt; */
	
  response = HBCI_Job_responseData(AB_JOB_Job(job));
  if (!response)
    return;
  acc_bal =GWEN_DB_GetGroup(response, 
			    GWEN_PATH_FLAGS_NAMEMUSTEXIST, "balance");
  if (!acc_bal) 
    return;

  noted_grp = GWEN_DB_GetGroup(response, GWEN_PATH_FLAGS_NAMEMUSTEXIST, "noted");
  booked_grp = GWEN_DB_GetGroup(response, GWEN_PATH_FLAGS_NAMEMUSTEXIST, "booked");

  booked_val = AB_VALUE_new(GWEN_DB_GetCharValue(booked_grp, "value", 0, "0"),
			      GWEN_DB_GetCharValue(booked_grp, "currency", 0, "EUR"));
  noted_val = AB_VALUE_new(GWEN_DB_GetCharValue(noted_grp, "value", 0, "0"),
			     GWEN_DB_GetCharValue(noted_grp, "currency", 0, "EUR"));
    
  printf("GetBalance: Balances for account %s :\n",
	 AB_ACCOUNT_accountId (h_acc));
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
/* 		  AB_ACCOUNTBalance_bankLine (acc_bal), FALSE, */
/* 		  balance_tt); */
/*   bal_print_debug("Disposable amount", */
/* 		  AB_ACCOUNTBalance_disposable (acc_bal), FALSE, */
/* 		  balance_tt); */
/*   bal_print_debug("Already disposed", */
/* 		  AB_ACCOUNTBalance_disposed (acc_bal), FALSE, */
/* 		  balance_tt); */
  AB_VALUE_delete(booked_val);
  AB_VALUE_delete(noted_val);
}
#endif

static gchar*
bal_print_balance(const char *format,
		  const AB_VALUE *val)
{
  char *str = gnc_AB_VALUE_toReadableString (val);
  char *res = g_strdup_printf(format, 
			      str);
  free (str);
  return res;
}



gboolean
gnc_hbci_getbalance_finish (GtkWidget *parent, 
			    Account *gnc_acc,
			    const AB_JOB *job)
{
  const AB_ACCOUNT_STATUS *response;
  const AB_BALANCE *noted_grp, *booked_grp;
  const AB_VALUE *booked_val, *noted_val;

  time_t booked_tt;

  gboolean dialogres;

  response = AB_JobGetBalance_GetAccountStatus((AB_JOB*)job);
  if (!response) {
    printf("gnc_hbci_getbalance_finish: Oops, response == NULL.\n");
    return TRUE;
  }

  noted_grp = AB_AccountStatus_GetNotedBalance(response);
  booked_grp = AB_AccountStatus_GetBookedBalance(response);

  if (!booked_grp || !noted_grp) {
    printf("gnc_hbci_getbalance_finish: Oops, booked_grp or noted_grp == NULL.\n");
    return TRUE;
  }
  booked_val = AB_Balance_GetValue(booked_grp);
  
  noted_val = AB_Balance_GetValue(noted_grp);
  
  booked_tt = GWEN_Time_toTime_t (AB_Balance_GetTime(booked_grp));
    
  if ((AB_Value_GetValue (noted_val) == 0) &&
      (AB_Value_GetValue (booked_val) == 0))
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
      char *booked_str = gnc_AB_VALUE_toReadableString (booked_val);
      char *message1 = g_strdup_printf
	(
	 _("Result of HBCI job: \n"
	   "Account booked balance is %s\n"),
	 booked_str);
      char *message2 = 
	((AB_Value_GetValue (noted_val) == 0) ?
	 g_strdup_printf("%s", "") :
	 bal_print_balance
	 (_("For your information: This account also \n"
	    "has a noted balance of %s\n"),
	  noted_val));
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
      gnc_numeric value =
	double_to_gnc_numeric (AB_Value_GetValue (booked_val),
			       xaccAccountGetCommoditySCU(gnc_acc),
			       GNC_RND_ROUND);
      recnWindowWithBalance (parent, 
			     gnc_acc, 
			     value,
			     booked_tt);
    }
      
  return TRUE;
}
