/********************************************************************\
 * gnc-hbci-transfer.c -- hbci transfer functions                   *
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
#include "gnc-hbci-transfer.h"

#include <openhbci/api.h>
#include <openhbci/outboxaccjobs.h>

#include "gnc-ui.h"
#include "gnc-numeric.h"
#include "dialog-transfer.h"
#include "gnc-date.h"
#include "Transaction.h"

#include "hbci-interaction.h"
#include "gnc-hbci-utils.h"
#include "gnc-hbci-trans-templ.h"
#include "gnc-hbci-kvp.h"


void maketrans_save_templates(GtkWidget *parent, Account *gnc_acc, 
			      GList *template_list, gboolean dont_ask);


void 
gnc_hbci_maketrans (GtkWidget *parent, Account *gnc_acc,
		    GNC_HBCI_Transtype trans_type)
{
  HBCI_API *api = NULL;
  const HBCI_Account *h_acc = NULL;
  GNCInteractor *interactor = NULL;
  const HBCI_Customer *customer = NULL;
  
  g_assert(parent);
  g_assert(gnc_acc);

  /* Get API */
  api = gnc_hbci_api_new_currentbook (parent, &interactor);
  if (api == NULL) {
    printf("gnc_hbci_maketrans: Couldn't get HBCI API. Nothing will happen.\n");
    return;
  }
  g_assert (interactor);

  /* Get HBCI account */
  h_acc = gnc_hbci_get_hbci_acc (api, gnc_acc);
  if (h_acc == NULL) {
    printf("gnc_hbci_maketrans: No HBCI account found. Nothing will happen.\n");
    return;
  }
  /*printf("gnc_hbci_maketrans: HBCI account no. %s found.\n",
    HBCI_Account_accountId (h_acc));*/
  
  /* Get the customer that should be doing this job. */
  customer = gnc_hbci_get_first_customer(h_acc);
  if (!customer) 
    return;

  {
    GList *template_list = 
      gnc_trans_templ_glist_from_kvp_glist
      ( gnc_hbci_get_book_template_list
	( xaccAccountGetBook(gnc_acc)));
    unsigned nr_templates;
    int result;
    gboolean successful;
    HBCITransDialog *td;

    /* Now open the HBCI_trans_dialog, which also calls
       HBCI_API_executeQueue. */
      
    /* Create new HBCIDialogTrans */
    td = gnc_hbci_dialog_new(parent, h_acc, customer, gnc_acc, 
			     trans_type, template_list);
	
    /* Repeat until HBCI action was successful or user pressed cancel */
    do {

      nr_templates = g_list_length(template_list);

      /* Let the user enter the values. If cancel is pressed, -1 is returned.  */
      result = gnc_hbci_dialog_run_until_ok(td, h_acc);

      /* Set the template list in case it got modified. */
      template_list = gnc_hbci_dialog_get_templ(td);
      /* New templates? If yes, store them */
      if (nr_templates < g_list_length(template_list)) 
	maketrans_save_templates(parent, gnc_acc, template_list, (result >= 0));

      if (result < 0) {
	break;
      } 
	
      /* Make really sure the dialog is hidden now. */
      gnc_hbci_dialog_hide(td);

      {
	HBCI_OutboxJob *job = 
	  gnc_hbci_trans_dialog_enqueue(td, api, customer, 
					(HBCI_Account *)h_acc, trans_type);
      
	/* HBCI Transaction has been created and enqueued, so now open
	 * the gnucash transaction dialog and fill in all values. */
	successful = gnc_hbci_maketrans_final (td, gnc_acc, trans_type);

	/* User pressed cancel? Then go back to HBCI transaction */
	if (!successful)
	  continue;

	if (result == 0) {

	  /* If the user pressed "execute now", then execute this job
	     now. This function already delete()s the job. */
	  successful = gnc_hbci_trans_dialog_execute(td, api, job, interactor);

	  if (!successful) {
	    /* HBCI job failed -- then remove gnc txn from the books. */
	    Transaction *gtrans = gnc_hbci_dialog_get_gtrans(td);
	    xaccTransBeginEdit(gtrans);
	    xaccTransDestroy(gtrans);
	    xaccTransCommitEdit(gtrans);
	  }
	  
	} /* result == 0 */
	else {
	  /* huh? Only result == 0 should be possible. Simply ignore
	     this case. */
	  break;
	} /* result == 0 */
	  
      } /* Create a do-transaction (transfer) job */
	
    } while (!successful);
    
    if (result >= 0) {
      /* If we wanted to do something here with the gnc txn, we could. */
      Transaction *gtrans = gnc_hbci_dialog_get_gtrans(td);
      printf("gnc-hbci-transfer: Got gnc txn w/ description: %s\n",
	     xaccTransGetDescription(gtrans));
    }

    /* Just to be on the safe side, clear queue once again. */
    HBCI_API_clearQueueByStatus (api, HBCI_JOB_STATUS_NONE);
    gnc_hbci_api_save (api);
    gnc_hbci_dialog_delete(td);
    gnc_trans_templ_delete_glist (template_list);
    
    /* GNCInteractor_hide (interactor); */
  }
}


/** Store the template_list in the given Account gnc_acc. If dont_ask
    is FALSE, first ask whether they really should be saved. */
void maketrans_save_templates(GtkWidget *parent, Account *gnc_acc, 
			      GList *template_list, gboolean dont_ask)
{
  if (dont_ask || gnc_verify_dialog
      (parent, 
       FALSE,
       "%s",
       _("You have created a new online transfer template, but \n"
	 "you cancelled the transfer dialog. Do you nevertheless \n"
	 "want to store the new online transfer template?"))) {
    GList *kvp_list = gnc_trans_templ_kvp_glist_from_glist (template_list);
    /*printf ("Now having %d templates. List: '%s'\n", 
      g_list_length(template_list),
      kvp_value_glist_to_string(kvp_list));*/
    gnc_hbci_set_book_template_list
      (xaccAccountGetBook(gnc_acc), kvp_list);
  }
}

gboolean
gnc_hbci_maketrans_final(HBCITransDialog *td, Account *gnc_acc,
			  GNC_HBCI_Transtype trans_type)
{
  gnc_numeric amount;
  XferDialog *transdialog;
  const HBCI_Transaction *h_trans;
  gboolean run_until_done = TRUE;
  g_assert(td);

  h_trans = gnc_hbci_dialog_get_htrans(td);
  
  /* HBCI Transaction has finished, so now open the gnucash
     transaction dialog and fill in all values. */
  
  transdialog = gnc_xfer_dialog (gnc_hbci_dialog_get_parent(td), gnc_acc);
  
  switch (trans_type) {
  case SINGLE_DEBITNOTE:
    gnc_xfer_dialog_set_title (transdialog, _("Online HBCI Direct Debit Note"));
  case SINGLE_TRANSFER:
  default:
    gnc_xfer_dialog_set_title (transdialog, _("Online HBCI Transaction"));
  }
      
  /* Amount */
  amount = double_to_gnc_numeric 
    (HBCI_Value_getValue (HBCI_Transaction_value (h_trans)),
     xaccAccountGetCommoditySCU(gnc_acc),
     GNC_RND_ROUND); 
  /*switch (trans_type) {
    case SINGLE_DEBITNOTE:
    gnc_xfer_dialog_set_amount (transdialog, gnc_numeric_neg (amount));
    case SINGLE_TRANSFER:
    default:*/
  gnc_xfer_dialog_set_amount (transdialog, amount);
  /*}*/
  /* gnc_xfer_dialog_toggle_currency_frame (transdialog, FALSE); */

  {
    /* Description */
    char *g_descr = gnc_hbci_descr_tognc (h_trans);
    gnc_xfer_dialog_set_description (transdialog, g_descr);
    g_free (g_descr);
  }

  {
    /* Memo. */
    char *g_memo = gnc_hbci_memo_tognc (h_trans);
    gnc_xfer_dialog_set_memo (transdialog, g_memo);
    g_free (g_memo);
  }
  /*gnc_xfer_dialog_set_date(XferDialog *xferData, time_t set_time)*/

  /* Set the callback for the Gnucash Transaction */
  gnc_xfer_dialog_set_txn_cb(transdialog, gnc_hbci_dialog_xfer_cb, td);
  
  /* Run the dialog until the user has either successfully completed the
   * transaction (just clicking OK doesn't always count) or clicked Cancel.
   * Return TRUE if the transaction was a success, FALSE otherwise.
   */
  return run_until_done 
    ? gnc_xfer_dialog_run_until_done( transdialog )
    : TRUE;
}
