/********************************************************************\
 * gnc-hbci-actions.c -- hbci action functions                      *
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

#include "gnc-hbci-actions.h"

#include <openhbci/api.h>
#include <openhbci/outboxaccjobs.h>

#include "gnc-ui.h"
#include "gnc-numeric.h"
#include "dialog-transfer.h"
#include "date.h"
#include "Transaction.h"
#include "gnc-generic-import.h"

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
  g_assert(gnc_acc);

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
    HBCI_Error *err;
    
    balance_job = 
      HBCI_OutboxJobGetBalance_new (customer, (HBCI_Account *)h_acc);
    job = HBCI_OutboxJobGetBalance_OutboxJob (balance_job);
    g_assert (job);
    HBCI_API_addJob (api, job);

    if (interactor)
      GNCInteractor_show (interactor);

    HBCI_Hbci_setDebugLevel(0);
    err = HBCI_API_executeQueue (api, TRUE);
    g_assert (err);
    if (!HBCI_Error_isOk(err)) {
      char *errstr = g_strdup_printf("gnc_hbci_getbalance: Error at executeQueue: %s",
				     HBCI_Error_message (err));
      printf("%s; status %d, result %d\n", errstr, HBCI_OutboxJob_status(job),
	     HBCI_OutboxJob_result(job));
      HBCI_Interactor_msgStateResponse (HBCI_Hbci_interactor 
					(HBCI_API_Hbci (api)), errstr);
      g_free (errstr);
      HBCI_Error_delete (err);
      gnc_hbci_debug_outboxjob (job);
      return;
    }
    /*HBCI_API_clearQueueByStatus (api, HBCI_JOB_STATUS_DONE);*/
    HBCI_Error_delete (err);
    
    {
      const HBCI_AccountBalance *acc_bal;
      const HBCI_Balance *bal1, *bal2;
      const HBCI_Value *val;
      struct tm tm1, tm2;
      time_t tt1, tt2;
      int choose1;
      Timespec ts1, ts2;
	    
      acc_bal = HBCI_OutboxJobGetBalance_getBalance (balance_job);
      bal1 = HBCI_AccountBalance_notedBalance (acc_bal);
      bal2 = HBCI_AccountBalance_bookedBalance (acc_bal);
      tm1 = HBCI_DateTime_to_tm (HBCI_Balance_date (bal1), 
				 HBCI_Balance_time (bal1));
      tt1 = mktime (&tm1);
      timespecFromTime_t (&ts1, tt1);
      tm2 = HBCI_DateTime_to_tm (HBCI_Balance_date (bal2), 
				 HBCI_Balance_time (bal2));
      tt2 = mktime (&tm2);
      timespecFromTime_t (&ts2, tt2);
      choose1 = (timespec_cmp (&ts1, &ts2) == 1);
      
      val = HBCI_Balance_value (choose1 ? bal1 : bal2);
		  
      gnc_verify_dialog(TRUE,
			"Result of HBCI job: \nAccount %s balance is %f.",
			(choose1 ? "noted" : "booked"),
			HBCI_Value_getValue (val));
    }
  }
}

static void *trans_list_cb (const HBCI_Transaction *trans, void *user_data);
struct trans_list_data 
{
  Account *gnc_acc;
};

void
gnc_hbci_gettrans (GtkWidget *parent, Account *gnc_acc)
{
  HBCI_API *api = NULL;
  const HBCI_Account *h_acc = NULL;
  GNCInteractor *interactor = NULL;
  const HBCI_Customer *customer = NULL;
  
  g_assert(parent);
  g_assert(gnc_acc);

  api = gnc_hbci_api_new_currentbook (parent, &interactor);
  g_assert (interactor);
  if (api == NULL) {
    printf("gnc_hbci_gettrans: Couldn't get HBCI API.\n");
    return;
  }
  
  h_acc = gnc_hbci_get_hbci_acc (api, gnc_acc);
  if (h_acc == NULL) {
    printf("gnc_hbci_gettrans: No HBCI account found.\n");
    return;
  }
  /* printf("gnc_hbci_gettrans: HBCI account no. %s found.\n",
     HBCI_Account_accountId (h_acc)); */
  
  {
    /* Get one customer. */
    const list_HBCI_Customer *custlist;
    list_HBCI_Customer_iter *iter;
    
    custlist = HBCI_Account_authorizedCustomers (h_acc);
    g_assert (custlist);
    switch (list_HBCI_Customer_size (custlist)) {
    case 0:
      printf("gnc_hbci_gettrans: No HBCI customer found.\n");
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
  /* printf("gnc_hbci_gettrans: Customer id %s found.\n",
     HBCI_Customer_custId ((HBCI_Customer *)customer)); */

  {
    /* Execute a GetTransactions job. */
    HBCI_OutboxJobGetTransactions *trans_job;
    HBCI_OutboxJob *job;
    HBCI_Error *err;
    HBCI_Date *blank_date = HBCI_Date_new_blank();
        
    trans_job = 
      HBCI_OutboxJobGetTransactions_new (customer, 
					 (HBCI_Account *)h_acc,
					 blank_date,
					 blank_date);
    job = HBCI_OutboxJobGetTransactions_OutboxJob (trans_job);
    g_assert (job);
    HBCI_API_addJob (api, job);

    if (interactor)
      GNCInteractor_show (interactor);

    HBCI_Hbci_setDebugLevel(0);
    err = HBCI_API_executeQueue (api, TRUE);
    g_assert (err);
    if (!HBCI_Error_isOk(err)) {
      char *errstr = g_strdup_printf("gnc_hbci_gettrans: Error at executeQueue: %s",
				     HBCI_Error_message (err));
      printf("%s; status %d, result %d\n", errstr, HBCI_OutboxJob_status(job),
	     HBCI_OutboxJob_result(job));
      HBCI_Interactor_msgStateResponse (HBCI_Hbci_interactor 
					(HBCI_API_Hbci (api)), errstr);
      g_free (errstr);
      HBCI_Error_delete (err);
      gnc_hbci_debug_outboxjob (job);
      return;
    }
    /*HBCI_API_clearQueueByStatus (api, HBCI_JOB_STATUS_DONE);*/
    HBCI_Error_delete (err);
    HBCI_Date_delete (blank_date);
        
    {
      const list_HBCI_Transaction *trans_list;
      struct trans_list_data data;
      
      data.gnc_acc = gnc_acc;
      
      trans_list = HBCI_OutboxJobGetTransactions_transactions (trans_job);
      list_HBCI_Transaction_foreach (trans_list, trans_list_cb, &data);
    }
  }
}



static void *trans_list_cb (const HBCI_Transaction *trans, void *user_data)
{
  time_t current_time; 
  Account *gnc_acc;
  GNCBook *book;
  Transaction *transaction;
  Split *split;
  gnc_numeric gnc_amount;
  struct trans_list_data *data = user_data;
  g_assert(data);

  gnc_acc = data->gnc_acc;
  g_assert(gnc_acc);
  
  book = xaccAccountGetBook(gnc_acc);
  transaction = xaccMallocTransaction(book);
  xaccTransBeginEdit(transaction);

  /*if(data.fi_id_valid==true){
    gnc_import_set_trans_online_id(transaction, data.fi_id);
    }*/

  /* Date / Time */
  xaccTransSetDateSecs(transaction, 
		       HBCI_Date_to_time_t (HBCI_Transaction_date (trans)));
  xaccTransSetDatePostedSecs(transaction, 
			     HBCI_Date_to_time_t 
			     (HBCI_Transaction_valutaDate (trans)));
  current_time = time(NULL);
  xaccTransSetDateEnteredSecs(transaction, mktime(localtime(&current_time)));

  /* Description */
  /*xaccTransSetNum(transaction, data.check_number);*/
  xaccTransSetDescription(transaction, 
			  HBCI_Transaction_transactionText (trans));

  /* Amount */
  xaccTransSetCurrency(transaction, xaccAccountGetCommodity(gnc_acc));

  split=xaccMallocSplit(book);
  xaccTransAppendSplit(transaction, split);
  xaccAccountInsertSplit(gnc_acc, split);

  gnc_amount = double_to_gnc_numeric(HBCI_Value_getValue 
				     (HBCI_Transaction_value (trans)),
				     xaccAccountGetCommoditySCU(gnc_acc),
				     GNC_RND_ROUND);
  xaccSplitSetBaseValue(split, gnc_amount, xaccAccountGetCommodity(gnc_acc));
    
  /* Also put the ofx transaction name in the splits memo field, or
   * ofx memo if name is unavailable */ 
  xaccSplitSetMemo(split, HBCI_Transaction_transactionText (trans));
  /* xaccTransCommitEdit(transaction); */
	  
  gnc_import_add_trans(transaction);

  return NULL;
}




void 
gnc_hbci_maketrans (GtkWidget *parent, Account *gnc_acc)
{
  HBCI_API *api = NULL;
  const HBCI_Account *h_acc = NULL;
  GNCInteractor *interactor = NULL;
  const HBCI_Customer *customer = NULL;
  
  g_assert(parent);
  g_assert(gnc_acc);

  api = gnc_hbci_api_new_currentbook (parent, &interactor);
  g_assert (interactor);
  if (api == NULL) {
    printf("gnc_hbci_maketrans: Couldn't get HBCI API.\n");
    return;
  }
  
  h_acc = gnc_hbci_get_hbci_acc (api, gnc_acc);
  if (h_acc == NULL) {
    printf("gnc_hbci_maketrans: No HBCI account found.\n");
    return;
  }
  /*printf("gnc_hbci_maketrans: HBCI account no. %s found.\n",
    HBCI_Account_accountId (h_acc));*/
  
  {
    /* Get one customer. */
    const list_HBCI_Customer *custlist;
    list_HBCI_Customer_iter *iter;
    
    custlist = HBCI_Account_authorizedCustomers (h_acc);
    g_assert (custlist);
    switch (list_HBCI_Customer_size (custlist)) {
    case 0:
      printf("gnc_hbci_maketrans: No HBCI customer found.\n");
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
  /*printf("gnc_hbci_maketrans: Customer id %s found.\n",
    HBCI_Customer_custId ((HBCI_Customer *)customer));*/

  {
    /* Now open the HBCI_trans_dialog. */
    HBCI_Transaction *trans = gnc_hbci_trans (parent, api, interactor,
					      h_acc, customer);
    if (!trans)
      return;

    {
      gnc_numeric amount;
      char *description;
      XferDialog *transdialog;
      
      amount = double_to_gnc_numeric 
	(HBCI_Value_getValue (HBCI_Transaction_value (trans)),
	 100, GNC_RND_ROUND); 
      /* FIXME: This '100' must go away and instead some function of
       * the account's currency has to be used here. */
      description = g_strdup_printf("HBCI to %s", 
				    HBCI_Transaction_otherAccountId (trans));
      
      transdialog = gnc_xfer_dialog (parent, gnc_acc);
      gnc_xfer_dialog_set_title (transdialog, "HBCI initiated transaction");
      /* gnc_xfer_dialog_toggle_currency_frame (transdialog, FALSE); */
      gnc_xfer_dialog_set_amount (transdialog, amount);
      gnc_xfer_dialog_set_description (transdialog, description);
      g_free (description);
    }
    
    HBCI_Transaction_delete (trans);
  }
}

