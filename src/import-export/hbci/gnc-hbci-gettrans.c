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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#define AQBANKING_NOWARN_DEPRECATED
#include "gnc-hbci-gettrans.h"

#include "gnc-ui.h"
#include "qof.h"
#include "Transaction.h"

#include "import-main-matcher.h"

#include "hbci-interaction.h"
#include "gnc-hbci-utils.h"
#include "dialog-hbcitrans.h"
#include "gnc-hbci-kvp.h"
#include "dialog-daterange.h"
#include "import-utilities.h"

/* static short module = MOD_IMPORT; */


gboolean
gettrans_dates(GtkWidget *parent, Account *gnc_acc, 
	       GWEN_TIME **from_date, GWEN_TIME **to_date);




void
gnc_hbci_gettrans (GtkWidget *parent, Account *gnc_acc)
{
  AB_BANKING *api = NULL;
  const AB_ACCOUNT *h_acc = NULL;
  GNCInteractor *interactor = NULL;
  
  g_assert(parent);
  g_assert(gnc_acc);

  /* Get the api */
  api = gnc_AB_BANKING_new_currentbook (parent, &interactor);
  if (api == NULL) {
    g_message("gnc_hbci_gettrans: Couldn't get HBCI API.\n");
    return;
  }
  g_assert (interactor);

  /* Get HBCI account */
  h_acc = gnc_hbci_get_hbci_acc (api, gnc_acc);
  if (h_acc == NULL) {
    g_warning("gnc_hbci_getbalance: No HBCI account found.\n");
    /* FIXME: free unneeded data */
    return;
  }

  {
    /* Execute a GetTransactions job. */
    AB_JOB *job;
    Timespec until_timespec;
    GWEN_TIME *from_date, *to_date;

    /* Get the start and end dates for the Gettrans job.  */
    if (!gettrans_dates(parent, gnc_acc, &from_date, &to_date))
      return;
    /* Use this as a local storage for the until_time below. */
    timespecFromTime_t(&until_timespec, GWEN_Time_toTime_t(to_date));
    
    /* Create OutboxJob */
    job = AB_JobGetTransactions_new((AB_ACCOUNT*)h_acc);
    if (AB_Job_CheckAvailability(job)) {
      g_warning("gnc_hbci_gettrans: Oops, job not available. Aborting.\n");
      /* FIXME: free unneeded data */
      return;
    }
    AB_JobGetTransactions_SetFromTime(job, from_date);
    AB_JobGetTransactions_SetToTime(job, to_date);

    /* Add job to AB_BANKING queue. */
    AB_Banking_EnqueueJob(api, job);

    /* Execute Outbox. */
    if (!gnc_AB_BANKING_execute (parent, api, job, interactor) ||
	(AB_Job_GetStatus(job) == AB_Job_StatusError) ||
	GNCInteractor_hadErrors (interactor)) {
      /* AB_BANKING_executeOutbox failed. */
      gnc_hbci_cleanup_job(api, job);
      return;
    }

    /* Store the date of this retrieval */
    gnc_hbci_set_account_trans_retrieval (gnc_acc, until_timespec);

    /* Now finish the job duties. */
    gnc_hbci_gettrans_final(parent, gnc_acc, job, FALSE);

    /* Clean up behind ourself. */
    gnc_hbci_cleanup_job(api, job);
    gnc_AB_BANKING_fini (api);
    GNCInteractor_hide (interactor);
    if (from_date) 
      GWEN_Time_free (from_date);
    GWEN_Time_free (to_date);
  }
}


/** Create and set the from_date and to_date objects, and return TRUE
    if they have been set. If FALSE is returned, then from_date and
    to_date are not pointing to valid objects and may not be
    deleted. */
gboolean
gettrans_dates(GtkWidget *parent, Account *gnc_acc, 
	       GWEN_TIME **from_date, GWEN_TIME **to_date)
{
  Timespec last_timespec, until_timespec;
  time_t now = time(NULL);
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
    *from_date = NULL;
  /* for an old version this was: from_date = GWEN_Time_fromSeconds(0); */
  else {
    if (use_last_date)
      last_timespec = gnc_hbci_get_account_trans_retrieval (gnc_acc);
    *from_date = GWEN_Time_fromSeconds(timespecToTime_t(last_timespec));
  }

  /* Now calculate to date */
  if (use_until_now)
    timespecFromTime_t (&until_timespec, now);
  *to_date = GWEN_Time_fromSeconds(timespecToTime_t (until_timespec));

  return TRUE;
}


/** Finalizes all the things that have to be done with a GetTrans
 * job.  Returns true if everything has been finished succesfully. */
gboolean
gnc_hbci_gettrans_final(GtkWidget *parent, 
			Account *gnc_acc, 
			const AB_JOB *trans_job,
			gboolean run_until_done)
{
  GtkWidget *dialog;

  /* Now add the retrieved transactions to the gnucash account. */
  AB_TRANSACTION_LIST2 *trans_list;

  trans_list = AB_JobGetTransactions_GetTransactions(trans_job);
  if (trans_list && (AB_Transaction_List2_GetSize(trans_list) > 0)) {
    /* Final importing part. */
    return gnc_hbci_import_final(parent, gnc_acc, trans_list, run_until_done);
  }

  dialog = gtk_message_dialog_new(GTK_WINDOW(parent),
				  GTK_DIALOG_MODAL
				  | GTK_DIALOG_DESTROY_WITH_PARENT,
				  GTK_MESSAGE_INFO,
				  GTK_BUTTONS_OK,
				  "%s",
				  _("The Online Banking import returned no transactions "
				    "for the selected time period."));
  gtk_dialog_run(GTK_DIALOG(dialog));
  gtk_widget_destroy(GTK_WIDGET(dialog));
  return TRUE;
}


gboolean
gnc_hbci_import_final(GtkWidget *parent, 
		      Account *gnc_acc,
		      AB_TRANSACTION_LIST2 *trans_list, 
		      gboolean run_until_done)
{
  struct trans_list_data data;
  GNCImportMainMatcher *importer_generic_gui;

  if (!trans_list || (AB_Transaction_List2_GetSize(trans_list) == 0)) 
    return TRUE;
    
  importer_generic_gui = gnc_gen_trans_list_new(parent, NULL, TRUE, 14);

  data.importer_generic = importer_generic_gui;
  data.gnc_acc = gnc_acc;
	
  AB_Transaction_List2_ForEach (trans_list, gnc_hbci_trans_list_cb, &data);

  if (run_until_done)
    return gnc_gen_trans_list_run (importer_generic_gui);
  return TRUE;
}


/* list_AB_TRANSACTION_foreach callback. The Conversion from HBCI to
   GNC transaction is done here, once for each AB_TRANSACTION.  */
AB_TRANSACTION *gnc_hbci_trans_list_cb(AB_TRANSACTION *h_trans, void *user_data)
{
  time_t current_time;
  /* time_t tt1, tt2; */
  /*struct tm tm1, tm2;*/
  Account *gnc_acc;
  GNCBook *book;
  Transaction *gnc_trans;
  const GWEN_TIME *valutaDate, *normalDate;
  Split *split;
  struct trans_list_data *data = user_data;
  g_assert(data);

  if (!h_trans) return NULL;

  gnc_acc = data->gnc_acc;
  g_assert(gnc_acc);
  book = gnc_account_get_book(gnc_acc);

  /* Create new gnucash transaction for the given hbci one */
  gnc_trans = xaccMallocTransaction(book);
  xaccTransBeginEdit(gnc_trans);

  normalDate = AB_Transaction_GetDate(h_trans);
  valutaDate = AB_Transaction_GetValutaDate(h_trans);
  if (normalDate && !valutaDate)
    valutaDate = normalDate;
  /* Watch out -- any of the GWEN_TIME may be NULL */
  /*   tt1 = GWEN_Time_toTime_t (normalDate); */
  /*   tt2 = GWEN_Time_toTime_t (valutaDate); */
  /*printf("Date? %s ValutaDate? %s", ctime(&tt1), ctime(&tt2));*/
  
  
  /* Date / Time */
  if (valutaDate)
    xaccTransSetDateSecs
      (gnc_trans, GWEN_Time_toTime_t (valutaDate));
  else
    g_warning("trans_list_cb: Oops, date 'valutaDate' was NULL.\n");
    
  current_time = time(NULL);
  xaccTransSetDateEnteredSecs(gnc_trans, mktime(localtime(&current_time)));
    
  /* Currency; we take simply the default currency of the gnucash account */
  xaccTransSetCurrency(gnc_trans, xaccAccountGetCommodity(gnc_acc));
    
  {
    /* Number. We use the "customer reference", if there is one. */
    const char *custref = AB_Transaction_GetCustomerReference (h_trans);
    if (custref && (strlen (custref) > 0) && 
	(g_ascii_strncasecmp (custref, "NONREF", 6) != 0))
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
    /* OFX unique transaction ID */
    const char *fitid = AB_Transaction_GetFiId(h_trans);
    if (fitid && (strlen (fitid) > 0))
      gnc_import_set_split_online_id(split, fitid);
  }

  {
    /* Amount into the split */
    const AB_VALUE *h_value = AB_Transaction_GetValue (h_trans);
    gnc_numeric gnc_amount = double_to_gnc_numeric
      (h_value ? AB_Value_GetValue (h_value) : 0.0,
       xaccAccountGetCommoditySCU(gnc_acc),
       GNC_RND_ROUND);
    if (!h_value)
      g_warning("trans_list_cb: Oops, value was NULL. Using 0.\n");
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

  return NULL;
  
}
