/********************************************************************\
 * gnc-hbci-gettrans.c -- hbci get transactions functions           *
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

#include "gnc-hbci-gettrans.h"

#include <openhbci/api.h>
#include <openhbci/outboxaccjobs.h>

#include "gnc-ui.h"
#include "gnc-numeric.h"
#include "date.h"
#include "Transaction.h"
#include "gnc-generic-import.h"

#include "hbci-interaction.h"
#include "gnc-hbci-utils.h"
#include "dialog-hbcitrans.h"


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


/* list_HBCI_Transaction_foreach callback */
static void *trans_list_cb (const HBCI_Transaction *h_trans, 
			    void *user_data)
{
  time_t current_time, tt1, tt2; 
  /*struct tm tm1, tm2;*/
  Account *gnc_acc;
  GNCBook *book;
  Transaction *gnc_trans;
  Split *split;
  struct trans_list_data *data = user_data;
  g_assert(data);
  g_assert(h_trans);

  gnc_acc = data->gnc_acc;
  g_assert(gnc_acc);
  
  book = xaccAccountGetBook(gnc_acc);
  gnc_trans = xaccMallocTransaction(book);
  xaccTransBeginEdit(gnc_trans);

  /*if(data.fi_id_valid==true){
    gnc_import_set_trans_online_id(gnc_trans, data.fi_id);
    }*/

  tt1 = HBCI_Date_to_time_t (HBCI_Transaction_date(h_trans));
  tt2 = HBCI_Date_to_time_t (HBCI_Transaction_valutaDate(h_trans));
  printf("Date? %s ValutaDate? %s", ctime(&tt1), ctime(&tt2));
  /*tm1 = HBCI_Date_to_tm (HBCI_Transaction_date(h_trans));
    tm2 = HBCI_Date_to_tm (HBCI_Transaction_valutaDate(h_trans));
    printf("Date asc %s ValutaDate asc %s", asctime(&tm1), asctime(&tm2));*/
  
  
  /* Date / Time */
  xaccTransSetDateSecs
    (gnc_trans, HBCI_Date_to_time_t (HBCI_Transaction_valutaDate (h_trans)));

  current_time = time(NULL);
  xaccTransSetDateEnteredSecs(gnc_trans, mktime(localtime(&current_time)));
  
  {
    /* Number. We use the "customer reference", if there is one. */
    const char *custref = HBCI_Transaction_customerReference (h_trans);
    if (custref && (strlen (custref) > 0) && 
	(g_strncasecmp (custref, "NONREF", 6) != 0))
      xaccTransSetNum (gnc_trans, custref);
  }
  
  {
    /* Description */
    char *descr = list_string_concat (HBCI_Transaction_description (h_trans));
    xaccTransSetDescription (gnc_trans, descr);
    free (descr);
  }
  
  /* Notes. TransactionText contains strings like "STANDING ORDER",
     "UEBERWEISUNGSGUTSCHRIFT", etc.  */
  xaccTransSetNotes (gnc_trans, HBCI_Transaction_transactionText (h_trans));

  /* Currency; we take simply the default currency of the gnucash account */
  xaccTransSetCurrency(gnc_trans, xaccAccountGetCommodity(gnc_acc));

  /* Add one split */
  split=xaccMallocSplit(book);
  xaccTransAppendSplit(gnc_trans, split);
  xaccAccountInsertSplit(gnc_acc, split);

  {
    /* Amount into the split */
    gnc_numeric gnc_amount = double_to_gnc_numeric
      (HBCI_Value_getValue (HBCI_Transaction_value (h_trans)),
       xaccAccountGetCommoditySCU(gnc_acc),
       GNC_RND_ROUND);
    xaccSplitSetBaseValue(split, gnc_amount, xaccAccountGetCommodity(gnc_acc));
  }
  
  {
    /* Memo in the split. We write the recipient (i.e. "other party") in
       here since the other party might be another gnucash account, and
       in that account split's memo the respective "this party" will be
       noted. */
    char *othername = 
      list_string_concat_delim (HBCI_Transaction_otherName (h_trans), ", ");
    char *memo = 
      g_strdup_printf ("HBCI Transaction to: %s, Account %s, Bank %s", 
		       othername, 
		       HBCI_Transaction_otherAccountId (h_trans),
		       HBCI_Transaction_otherBankCode (h_trans));
    xaccSplitSetMemo(split, memo);
    free (othername);
    g_free (memo);
  }
  
  /* Instead of xaccTransCommitEdit(gnc_trans)  */
  gnc_import_add_trans(gnc_trans);

  return NULL;
}




