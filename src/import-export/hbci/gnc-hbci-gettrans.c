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

#include <openhbci2/api.h>

#include "gnc-ui.h"
#include "gnc-numeric.h"
#include "gnc-date.h"
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


gboolean
gettrans_dates(GtkWidget *parent, Account *gnc_acc, 
	       HBCI_Date **from_date, HBCI_Date **to_date);

static void *trans_list_cb (GWEN_DB_NODE *trans_node, void *user_data);
static void *reports_cb(GWEN_DB_NODE *reportn, void *user_data);

struct trans_list_data 
{
  Account *gnc_acc;
  GNCImportMainMatcher *importer_generic;
};


void
gnc_hbci_gettrans (GtkWidget *parent, Account *gnc_acc)
{
  HBCI_API *api = NULL;
  HBCI_Outbox *outbox = NULL;
  const gnc_HBCI_Account *h_acc = NULL;
  GNCInteractor *interactor = NULL;
  const HBCI_Customer *customer = NULL;
  GList *hbci_accountlist = NULL;
  
  g_assert(parent);
  g_assert(gnc_acc);

  /* Get the api */
  api = gnc_hbci_api_new_currentbook (parent, &interactor, &hbci_accountlist);
  if (api == NULL) {
    printf("gnc_hbci_gettrans: Couldn't get HBCI API.\n");
    return;
  }
  g_assert (interactor);
  outbox = HBCI_Outbox_new();

  /* Get the HBCI account */
  h_acc = gnc_hbci_get_hbci_acc (api, gnc_acc);
  if (h_acc == NULL) {
    printf("gnc_hbci_gettrans: No HBCI account found.\n");
    return;
  }
  /* printf("gnc_hbci_gettrans: HBCI account no. %s found.\n",
     gnc_HBCI_Account_accountId (h_acc)); */

  /* Get the customer that should be doing this job. */
  customer = gnc_hbci_get_first_customer(h_acc);
  if (!customer) 
    return;

  /* g_assert (customer); */
  /* printf("gnc_hbci_gettrans: Customer id %s found.\n",
     HBCI_Customer_custId ((HBCI_Customer *)customer)); */

  {
    /* Execute a GetTransactions job. */
    HBCI_OutboxJob *job;
    Timespec until_timespec;
    HBCI_Date *from_date, *to_date;

    /* Get the start and end dates for the Gettrans job.  */
    if (!gettrans_dates(parent, gnc_acc, &from_date, &to_date))
      return;
    /* Use this as a local storage for the until_time below. */
    timespecFromTime_t(&until_timespec, HBCI_Date_to_time_t(to_date));
    
    /* Create OutboxJob */
    job = HBCI_OutboxJob_new("JobGetTransactions", (HBCI_Customer *)customer, 
			     gnc_HBCI_Account_accountId(h_acc));
    
    {
      HBCI_Job *jjob = HBCI_OutboxJob_Job(job);
      char *tmp;
      
      tmp = HBCI_Date_toString(from_date);
      HBCI_Job_setProperty(jjob, "fromDate", tmp);
      free(tmp);
      tmp = HBCI_Date_toString(to_date);
      HBCI_Job_setProperty(jjob, "toDate", tmp);
      free(tmp);
    }
    HBCI_Date_delete (from_date);
    HBCI_Date_delete (to_date);

    /* Add job to HBCI_API queue. */
    HBCI_Outbox_addJob (outbox, job);

    /* Execute Outbox. */
    if (!gnc_hbci_api_execute (parent, api, outbox, job, interactor)) {
      /* HBCI_API_executeOutbox failed. */
      HBCI_Outbox_removeByStatus (outbox, HBCI_JOB_STATUS_NONE);
      return;
    }

    /* Store the date of this retrieval */
    gnc_hbci_set_account_trans_retrieval (gnc_acc, until_timespec);

    /* Now finish the job duties. */
    gnc_hbci_gettrans_final(parent, gnc_acc, job, FALSE);

    /* Clean up behind ourself. */
    HBCI_Outbox_removeByStatus (outbox, HBCI_JOB_STATUS_NONE);
    HBCI_Outbox_delete(outbox);
    gnc_hbci_api_save (api);
    GNCInteractor_hide (interactor);
  }
}


/** Create and set the from_date and to_date objects, and return TRUE
    if they have been set. If FALSE is returned, then from_date and
    to_date are not pointing to valid objects and may not be
    deleted. */
gboolean
gettrans_dates(GtkWidget *parent, Account *gnc_acc, 
	       HBCI_Date **from_date, HBCI_Date **to_date)
{
  Timespec last_timespec, until_timespec;
  time_t now = time(NULL), time_convert;
  struct tm tm;
  gboolean use_last_date = TRUE, 
    use_earliest_date = TRUE, use_until_now = TRUE;

  g_assert(from_date);
  g_assert(to_date);
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
      return FALSE;

    /*printf("Retrieving transactions from date %s to date %s. \n",
	   ctime(&()))*/
    
    /* Now calculate from date */
    if (use_earliest_date)
      *from_date = HBCI_Date_new_blank();
    else {
      if (use_last_date)
	last_timespec = gnc_hbci_get_account_trans_retrieval (gnc_acc);
      time_convert = timespecToTime_t(last_timespec);
      *from_date = HBCI_Date_new (localtime_r (&time_convert, &tm));
    }

    /* Now calculate to date */
    if (use_until_now)
      timespecFromTime_t (&until_timespec, now);
    time_convert = timespecToTime_t (until_timespec);
    *to_date = HBCI_Date_new (localtime_r (&time_convert, &tm));

    return TRUE;
}


/** Finalizes all the things that have to be done with a GetTrans
 * job.  Returns true if everything has been finished succesfully. */
gboolean
gnc_hbci_gettrans_final(GtkWidget *parent, 
			Account *gnc_acc, 
			const HBCI_OutboxJob *trans_job,
			gboolean run_until_done)
{
  /* Now add the retrieved transactions to the gnucash account. */
  GWEN_DB_NODE *trans_list, *response;


  response = HBCI_Job_responseData((HBCI_Job*)HBCI_OutboxJob_Job_const(trans_job));
  if (!response) {
    printf("gnc_hbci_gettrans_final: Got no responseData.\n");
    return FALSE;
  }

  trans_list = GWEN_DB_GetGroup(response, GWEN_PATH_FLAGS_NAMEMUSTEXIST, 
				"transactions");
  if (!trans_list) {
    printf("gnc_hbci_gettrans_final: No transactions section. Response was:\n");
    GWEN_DB_Dump(response, stdout, 1);

    gnome_ok_dialog_parented 
      (_("The HBCI import returned no transactions for the selected time period."),
       GTK_WINDOW (parent));
    return TRUE;
  }
  
  trans_list = GWEN_DB_GetGroup(trans_list,
				GWEN_PATH_FLAGS_NAMEMUSTEXIST,
				"booked");

  if (!trans_list) {
    printf("gnc_hbci_gettrans_final: No booked section. Response was:\n");
    GWEN_DB_Dump(response, stdout, 1);
  }
  if (trans_list && (GWEN_DB_Groups_Count(trans_list) > 0)) {
    struct trans_list_data data;
    GNCImportMainMatcher *importer_generic_gui = 
      gnc_gen_trans_list_new(NULL, NULL, TRUE);

    data.importer_generic = importer_generic_gui;
    data.gnc_acc = gnc_acc;
	
    GWEN_DB_Groups_Foreach (trans_list, reports_cb, &data);

    if (run_until_done)
      return gnc_gen_trans_list_run (importer_generic_gui);

  }
  else {
    gnome_ok_dialog_parented 
      (_("The HBCI import returned no transactions for the selected time period."),
       GTK_WINDOW (parent));
  }

  return TRUE;
}


static void *reports_cb(GWEN_DB_NODE *reportn, void *user_data)
{
  if (!reportn) {
    /*DBG_INFO(0, "Oops: reportn is NULL");*/
    return NULL;
  }
  if (strcasecmp(GWEN_DB_GroupName(reportn), "report")==0) {
    return GWEN_DB_Groups_Foreach(reportn, trans_list_cb, user_data);
  } // if report
  else {
    /*DBG_WARN(0, "Unknown section \"%s\"", GWEN_DB_GroupName(reportn));*/
  }
  return NULL;
}


/* list_HBCI_Transaction_foreach callback. The Conversion from HBCI to
   GNC transaction is done here, once for each HBCI_Transaction.  */
static void *trans_list_cb (GWEN_DB_NODE *trans_node, 
			    void *user_data)
{
  HBCI_Transaction *h_trans;
  time_t current_time, tt1, tt2; 
  /*struct tm tm1, tm2;*/
  Account *gnc_acc;
  GNCBook *book;
  Transaction *gnc_trans;
  Split *split;
  struct trans_list_data *data = user_data;
  g_assert(data);

  if (!trans_node) return NULL;

  gnc_acc = data->gnc_acc;
  g_assert(gnc_acc);
  book = xaccAccountGetBook(gnc_acc);

  if (strcasecmp(GWEN_DB_GroupName(trans_node), "transaction")==0) {
    /* Create wrapper HBCI_Transaction */
    h_trans = HBCI_Transaction_new(trans_node);

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
    g_assert (data->importer_generic);
    gnc_gen_trans_list_add_trans (data->importer_generic, gnc_trans);

    HBCI_Transaction_delete(h_trans);
  }
  else if (strcasecmp(GWEN_DB_GroupName(trans_node), "startsaldo")==0) {
    /*DBG_INFO(0, "Found opening balance");*/
  }
  else if (strcasecmp(GWEN_DB_GroupName(trans_node), "endsaldo")==0) {
    /* Found closing balance */
    trans_node = GWEN_DB_GetGroup(trans_node,
				  GWEN_PATH_FLAGS_NAMEMUSTEXIST,
				  "value");
    /*if (trans_node) {
      HBCI_Value *bal;
      char *val;
      DBG_INFO(0, "Found closing balance");
      
      bal = HBCI_Value_new(GWEN_DB_GetCharValue(trans_node, "value", 0, "0"),
			   GWEN_DB_GetCharValue(trans_node, "currency", 0, "EUR"));
      
      val = HBCI_Value_toReadableString(bal);
      printf("Got closing balance %s\n", val);
      
      free(val);
      HBCI_Value_delete(bal);
      }*/
  }
  else {
    /*DBG_WARN(0, "Unknown section \"%s\"", GWEN_DB_GroupName(trans_node));*/
  }
  return NULL;
  
}
