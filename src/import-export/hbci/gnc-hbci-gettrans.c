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

/*#include "gnc-gen-transaction.h"*/
#include "import-main-matcher.h"
#include "global-options.h"

#include "hbci-interaction.h"
#include "gnc-hbci-utils.h"
#include "dialog-hbcitrans.h"
#include "gnc-hbci-kvp.h"
#include "dialog-daterange.h"

/* static short module = MOD_IMPORT; */

/*static const gboolean DEFAULT_USE_GENERIC_MATCHER = FALSE;  */

static void *trans_list_cb (const HBCI_Transaction *trans, void *user_data);

struct trans_list_data 
{
  Account *gnc_acc;
  /*GNCGenTransaction *importer;*/
  GNCImportMainMatcher *importer_generic;
  /*gboolean use_generic_matcher;*/
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
  if (api == NULL) {
    printf("gnc_hbci_gettrans: Couldn't get HBCI API.\n");
    return;
  }
  g_assert (interactor);
  
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

    {
      /* Now add the retrieved transactions to the gnucash account. */
      const list_HBCI_Transaction *trans_list;
      
      trans_list = HBCI_OutboxJobGetTransactions_transactions (trans_job);
      /*printf("gnc_hbci_gettrans: Got %d transactions.\n", 
	list_HBCI_Transaction_size(trans_list));*/

      if (list_HBCI_Transaction_size(trans_list) > 0) {
	struct trans_list_data data;
	/*GNCGenTransaction *importer_gui = NULL;*/
	GNCImportMainMatcher *importer_generic_gui = NULL;

	/*data.use_generic_matcher = 
	  gnc_lookup_boolean_option("Online Banking & Importing",
	  "HBCI Use generic import matcher",
	  DEFAULT_USE_GENERIC_MATCHER);
	  
	  if(data.use_generic_matcher == TRUE)
	  {*/
	importer_generic_gui = gnc_gen_trans_list_new(NULL, NULL, TRUE);
	data.importer_generic = importer_generic_gui;
	/*  }
	    else
	    {
	    importer_gui = gnc_gen_trans_new (NULL, NULL);
	    gnc_gen_trans_freeze (importer_gui);
	    gnc_gen_trans_set_fuzzy_amount (importer_gui, 0.0);
	    data.importer = importer_gui;
	    }*/
	data.gnc_acc = gnc_acc;
	
	list_HBCI_Transaction_foreach (trans_list, trans_list_cb, &data);

	/*if(data.use_generic_matcher == FALSE)
	  gnc_gen_trans_thaw (importer_gui);*/

	GNCInteractor_hide (interactor);
	/*if(data.use_generic_matcher == TRUE)*/
	gnc_gen_trans_list_run (importer_generic_gui);
	/*else
	  gnc_gen_trans_run (importer_gui);*/

      }
      else {
	GNCInteractor_hide (interactor);
	gnome_ok_dialog_parented 
	  (_("The HBCI import returned no transactions for the selected time period."),
	   GTK_WINDOW (parent));
      }
      
    }

    /* Clean up behind ourself. */
    HBCI_API_clearQueueByStatus (api, HBCI_JOB_STATUS_DONE);
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
  
  /* Description */
  {
    char *g_descr = gnc_hbci_descr_tognc (h_trans);
    xaccTransSetDescription (gnc_trans, g_descr);
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

  /* Memo in the Split. */
  {
    char *g_memo = gnc_hbci_memo_tognc (h_trans);
    xaccSplitSetMemo(split, g_memo);
    g_free (g_memo);
  }
    
  /* Instead of xaccTransCommitEdit(gnc_trans)  */
  /*gnc_import_add_trans(gnc_trans);*/
  /*if(data->use_generic_matcher == TRUE)
    {*/
  g_assert (data->importer_generic);
  gnc_gen_trans_list_add_trans (data->importer_generic, gnc_trans);
  /*}
    else
    {
    g_assert (data->importer);
    gnc_gen_trans_add_trans (data->importer, gnc_trans);
    }*/
  return NULL;
}
