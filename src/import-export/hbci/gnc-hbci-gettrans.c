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

#include "config.h"
#include "gnc-hbci-gettrans.h"

#include <openhbci/api.h>
#include <openhbci/outboxaccjobs.h>

#include "gnc-ui.h"
#include "gnc-numeric.h"
#include "date.h"
#include "Transaction.h"
#include "gnc-engine-util.h" 
#include "gnc-gen-transaction.h"

#include "hbci-interaction.h"
#include "gnc-hbci-utils.h"
#include "dialog-hbcitrans.h"
#include "gnc-hbci-kvp.h"
#include "dialog-daterange.h"

static short module = MOD_IMPORT;

static void *trans_list_cb (const HBCI_Transaction *trans, void *user_data);

struct trans_list_data 
{
  Account *gnc_acc;
  GNCGenTransaction *importer;
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
    Timespec last_timespec, until_timespec;
    time_t now = time(NULL), time_convert;
    struct tm tm;
    HBCI_Date *from_date, *to_date;
    gboolean use_last_date = TRUE, 
      use_earliest_date = TRUE, use_until_now = TRUE;
    GNCGenTransaction *importer_gui;

    /* Get time of last retrieval */
    last_timespec = gnc_hbci_get_account_trans_retrieval (gnc_acc);
    if (last_timespec.tv_sec == 0) {
      use_last_date = FALSE;
      timespecFromTime_t (&last_timespec, now);
    }
    timespecFromTime_t (&until_timespec, now);

    /* Let the user choose the date range of retrieval */
    if (!gnc_hbci_enter_daterange (parent, NULL, 
				   &last_timespec, 
				   &use_last_date, &use_earliest_date,
				   &until_timespec, &use_until_now))
      return;

    /*printf("Retrieving transactions from date %s to date %s. \n",
	   ctime(&()))*/
    
    /* Now calculate from date */
    if (use_earliest_date)
      from_date = HBCI_Date_new_blank();
    else {
      if (use_last_date)
	last_timespec = gnc_hbci_get_account_trans_retrieval (gnc_acc);
      time_convert = timespecToTime_t(last_timespec);
      from_date = HBCI_Date_new (localtime_r (&time_convert, &tm));
    }

    /* Now calculate to date */
    if (use_until_now)
      timespecFromTime_t (&until_timespec, now);
    time_convert = timespecToTime_t (until_timespec);
    to_date = HBCI_Date_new (localtime_r (&time_convert, &tm));

    /* Create OutboxJob */
    trans_job = 
      HBCI_OutboxJobGetTransactions_new (customer, 
					 (HBCI_Account *)h_acc,
					 from_date,
					 to_date);
    job = HBCI_OutboxJobGetTransactions_OutboxJob (trans_job);
    g_assert (job);
    HBCI_API_addJob (api, job);

    /* Execute Outbox. */
    if (!gnc_hbci_api_execute (parent, api, job, interactor)) {

      /* HBCI_API_executeOutbox failed. */
      HBCI_Date_delete (from_date);
      HBCI_Date_delete (to_date);
      return;
    }

    HBCI_Date_delete (from_date);
    HBCI_Date_delete (to_date);

    /* Store the date of this retrieval */
    gnc_hbci_set_account_trans_retrieval (gnc_acc, until_timespec);

    importer_gui = gnc_gen_trans_new (NULL, NULL);
    gnc_gen_trans_freeze (importer_gui);
    
    {
      /* Now add the retrieved transactions to the gnucash account. */
      const list_HBCI_Transaction *trans_list;
      struct trans_list_data data;
      
      data.gnc_acc = gnc_acc;
      data.importer = importer_gui;
      
      trans_list = HBCI_OutboxJobGetTransactions_transactions (trans_job);
      printf("gnc_hbci_gettrans: Got %d transactions.\n", 
	     list_HBCI_Transaction_size(trans_list));
      list_HBCI_Transaction_foreach (trans_list, trans_list_cb, &data);
    }

    gnc_gen_trans_thaw (importer_gui);
    GNCInteractor_hide (interactor);
    /* Clean up behind ourself. */
    HBCI_API_clearQueueByStatus (api, HBCI_JOB_STATUS_DONE);
    gnc_gen_trans_run (importer_gui);
  }
}

static void *gnc_list_string_cb (const char *string, void *user_data)
{
  gchar **res = user_data;
  gchar *tmp1, *tmp2;

  tmp1 = g_strdup (string);
  g_strstrip (tmp1);

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
  
  return NULL;
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
  /*printf("Date? %s ValutaDate? %s", ctime(&tt1), ctime(&tt2));*/
  /*tm1 = HBCI_Date_to_tm (HBCI_Transaction_date(h_trans));
    tm2 = HBCI_Date_to_tm (HBCI_Transaction_valutaDate(h_trans));
    printf("Date asc %s ValutaDate asc %s", asctime(&tm1), asctime(&tm2));*/
  
  
  /* Date / Time */
  xaccTransSetDateSecs
    (gnc_trans, HBCI_Date_to_time_t (HBCI_Transaction_valutaDate (h_trans)));

  current_time = time(NULL);
  xaccTransSetDateEnteredSecs(gnc_trans, mktime(localtime(&current_time)));
  
  /* Currency; we take simply the default currency of the gnucash account */
  xaccTransSetCurrency(gnc_trans, xaccAccountGetCommodity(gnc_acc));

  {
    /* Number. We use the "customer reference", if there is one. */
    const char *custref = HBCI_Transaction_customerReference (h_trans);
    if (custref && (strlen (custref) > 0) && 
	(g_strncasecmp (custref, "NONREF", 6) != 0))
      xaccTransSetNum (gnc_trans, custref);
  }
  
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
    DEBUG("HBCI Description '%s'", h_descr);
    
    g_descr = 
      ((strlen (h_descr) > 0) ?
       g_strdup_printf ("%s; %s", 
			h_descr,
			othername) :
       g_strdup (othername));
    
    xaccTransSetDescription (gnc_trans, g_descr);

    free (h_descr);
    free (othername);
    g_free (g_descr);
  }
  
  /* Notes. */
  /*xaccTransSetNotes (gnc_trans, g_notes);*/
  /* But Nobody ever uses the Notes field? */

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
      ((strlen(h_transactionText) > 0) ?
       g_strdup_printf ("%s %s %s %s %s",
			h_transactionText,
			_("Account"), h_otherAccountId,
			_("Bank"), h_otherBankCode) :
       g_strdup_printf ("%s %s %s %s",
			_("Account"), h_otherAccountId,
			_("Bank"), h_otherBankCode));
    xaccSplitSetMemo(split, g_memo);

    g_free (h_transactionText);
    g_free (h_otherAccountId);
    g_free (h_otherBankCode);
    g_free (g_memo);
  }
  
  /* Instead of xaccTransCommitEdit(gnc_trans)  */
  /*gnc_import_add_trans(gnc_trans);*/
  gnc_gen_trans_add_trans (data->importer, gnc_trans);

  return NULL;
}
