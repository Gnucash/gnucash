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
  g_assert (interactor);
  if (api == NULL) {
    printf("gnc_hbci_getbalance: Couldn't get HBCI API.\n");
    return;
  }
  
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
      const HBCI_Balance *bal1, *bal2;
      const HBCI_Value *val;
      time_t tt1, tt2;
      int choose1;
      Timespec ts1, ts2;
      gboolean dialogres;
	    
      acc_bal = HBCI_OutboxJobGetBalance_getBalance (balance_job);
      bal1 = HBCI_AccountBalance_notedBalance (acc_bal);
      bal2 = HBCI_AccountBalance_bookedBalance (acc_bal);

      tt1 = HBCI_DateTime_to_time_t (HBCI_Balance_date (bal1), 
				     HBCI_Balance_time (bal1));
      tt2 = HBCI_DateTime_to_time_t (HBCI_Balance_date (bal2), 
				     HBCI_Balance_time (bal2));

      val = HBCI_Balance_value (bal1);
      printf("Noted balance: %s for account no. %s at date %s",
	     HBCI_Value_toReadableString (val), // this ought to be free'd
	     HBCI_Account_accountId (h_acc),
	     ctime(&tt1));

      val = HBCI_Balance_value (bal2);
      printf("Booked balance: %s for account no. %s at date %s",
	     HBCI_Value_toReadableString (val),
	     HBCI_Account_accountId (h_acc),
	     ctime(&tt2));

      timespecFromTime_t (&ts1, tt1);
      timespecFromTime_t (&ts2, tt2);

      choose1 = (timespec_cmp (&ts1, &ts2) == 1);
      
      val = HBCI_Balance_value (choose1 ? bal1 : bal2);
		  
      dialogres = gnc_verify_dialog_parented
	  (parent, 
	   TRUE,
	   "Result of HBCI job: \nAccount %s balance is %g\nReconcile account now?",
	   (choose1 ? "noted" : "booked"),
	   HBCI_Value_getValue (val));

      if (dialogres) 
	recnWindowWithBalance (parent, 
			       gnc_acc, 
			       double_to_gnc_numeric 
			       (HBCI_Value_getValue (val),
				xaccAccountGetCommoditySCU(gnc_acc),
				GNC_RND_ROUND),
			       (choose1 ? tt1 : tt2));

      
    }
  }
}

