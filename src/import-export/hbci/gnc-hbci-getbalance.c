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

#include <openhbci/api.h>
#include <openhbci/outboxaccjobs.h>

#include "gnc-ui.h"
#include "gnc-numeric.h"
#include "date.h"
#include "RecnWindow.h"

#include "hbci-interaction.h"
#include "gnc-hbci-utils.h"
#include "dialog-hbcitrans.h"


static void 
bal_print_debug(const char *name,
		const HBCI_Value *val,
		gboolean negative,
		time_t tt)
{
  char *str = HBCI_Value_toReadableString (val);
  printf("GetBalance: %s%s %s at date %s",
	 (negative ? "-" : ""), str, 
	 name, ctime(&tt));
  free (str);
}

void
gnc_hbci_getbalance (GtkWidget *parent, Account *gnc_acc)
{
  HBCI_API *api = NULL;
  const HBCI_Account *h_acc = NULL;
  GNCInteractor *interactor = NULL;
  const HBCI_Customer *customer = NULL;
  
  g_assert(parent);
  if (gnc_acc == NULL)
    return;

  api = gnc_hbci_api_new_currentbook (parent, &interactor);
  if (api == NULL) {
    printf("gnc_hbci_getbalance: Couldn't get HBCI API.\n");
    return;
  }
  g_assert (interactor);
  
  h_acc = gnc_hbci_get_hbci_acc (api, gnc_acc);
  if (h_acc == NULL) {
    printf("gnc_hbci_getbalance: No HBCI account found.\n");
    return;
  }
  /* printf("gnc_hbci_getbalance: HBCI account no. %s found.\n",
     HBCI_Account_accountId (h_acc)); */
  
  {
    /* Get one customer. */
    const list_HBCI_Customer *custlist;
    list_HBCI_Customer_iter *iter;
    
    custlist = HBCI_Account_authorizedCustomers (h_acc);
    g_assert (custlist);
    switch (list_HBCI_Customer_size (custlist)) {
    case 0:
      printf("gnc_hbci_getbalance: No HBCI customer found.\n");
      return;
    case 1:
      break;
    default:
      gnc_warning_dialog_parented(gnc_ui_get_toplevel (), 
				  "Sorry, Choosing one out of several HBCI Customers not yet implemented.");
      return;
    }
    iter = list_HBCI_Customer_begin (custlist);
    customer = list_HBCI_Customer_iter_get (iter);
    list_HBCI_Customer_iter_delete (iter);
  }
  g_assert (customer);
  /* printf("gnc_hbci_getbalance: Customer id %s found.\n",
     HBCI_Customer_custId ((HBCI_Customer *)customer)); */

  {
    /* Execute a GetBalance job. */
    HBCI_OutboxJobGetBalance *balance_job;
    HBCI_OutboxJob *job;
    
    balance_job = 
      HBCI_OutboxJobGetBalance_new (customer, (HBCI_Account *)h_acc);
    job = HBCI_OutboxJobGetBalance_OutboxJob (balance_job);
    g_assert (job);
    HBCI_API_addJob (api, job);

    /* Execute Outbox. */
    if (!gnc_hbci_api_execute (parent, api, job, interactor)) {

      /* HBCI_API_executeOutbox failed. */
      return;
    }

    {
      const HBCI_AccountBalance *acc_bal;
      const HBCI_Balance *noted_bal, *booked_bal;
      const HBCI_Value *booked_val; 
      gboolean booked_debit;
      time_t balance_tt, noted_tt, booked_tt;
      gboolean dialogres;
	    
      acc_bal = HBCI_OutboxJobGetBalance_getBalance (balance_job);
      balance_tt = 
	HBCI_DateTime_to_time_t (HBCI_AccountBalance_date (acc_bal), 
				 HBCI_AccountBalance_time (acc_bal));

      booked_bal = HBCI_AccountBalance_bookedBalance (acc_bal);
      booked_tt = HBCI_DateTime_to_time_t (HBCI_Balance_date (booked_bal), 
					   HBCI_Balance_time (booked_bal));
      booked_val = HBCI_Balance_value (booked_bal);
      booked_debit = HBCI_Balance_isDebit (booked_bal),

      noted_bal = HBCI_AccountBalance_notedBalance (acc_bal);
      noted_tt = HBCI_DateTime_to_time_t (HBCI_Balance_date (noted_bal), 
					  HBCI_Balance_time (noted_bal));

      printf("GetBalance: Balances for account %s :\n",
	     HBCI_Account_accountId (h_acc));
      bal_print_debug("Booked balance",
		      booked_val,
		      booked_debit,
		      booked_tt);
      bal_print_debug("Noted balance",
		      HBCI_Balance_value (noted_bal),
		      HBCI_Balance_isDebit (noted_bal),
		      noted_tt);
      bal_print_debug("Bank Line", 
		      HBCI_AccountBalance_bankLine (acc_bal), FALSE,
		      balance_tt);
      bal_print_debug("Disposable amount",
		      HBCI_AccountBalance_disposable (acc_bal), FALSE,
		      balance_tt);
      bal_print_debug("Already disposed",
		      HBCI_AccountBalance_disposed (acc_bal), FALSE,
		      balance_tt);

      if ((HBCI_Value_getValue (HBCI_Balance_value (noted_bal)) == 0) &&
	  (HBCI_Value_getValue (HBCI_Balance_value (booked_bal)) == 0))
	{
	  gnome_ok_dialog_parented 
	    /* Translators: Strings from this file are really only
	     * needed inside Germany (HBCI is not supported anywhere
	     * else). You may safely ignore strings from the
	     * import-export/hbci subdirectory in other countries.
	     */
	    (_("The downloaded HBCI Balance was zero.\n"
	       "It seems as if your bank does not support Balance download \n"
	       "in this HBCI version. You should choose a higher HBCI version \n"
	       "number in the HBCI Setup. After that, try again to download \n"
	       "the HBCI Balance.\n"),
	     GTK_WINDOW (parent));
	  dialogres = FALSE;
	}
      else
      {
	gboolean booked_debit = HBCI_Balance_isDebit (booked_bal);
	char *booked_str = HBCI_Value_toReadableString (booked_val);

	dialogres = gnc_verify_dialog_parented
	  (parent, 
	   TRUE,
	   /* Translators: %s is the amount. */
	   _("Result of HBCI job: \n"
	     "Account booked balance is %s%s\n"
	     "Reconcile account now?"),
	   (booked_debit ? "-" : ""),
	   booked_str);

	free (booked_str);
      }

      
      GNCInteractor_hide (interactor);
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
      
      HBCI_API_clearQueueByStatus (api, HBCI_JOB_STATUS_DONE);
    }
  }
}

