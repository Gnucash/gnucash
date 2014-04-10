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
    unsigned nr_templates = g_list_length(template_list);

    /* Now open the HBCI_trans_dialog, which also calls
       HBCI_API_executeQueue. */
    HBCI_Transaction *h_trans = gnc_hbci_trans (parent, api, interactor,
						h_acc, customer, 
						gnc_acc,
						trans_type, &template_list);

    /* New templates? If yes, store them */
    if (nr_templates < g_list_length(template_list)) 
      maketrans_save_templates(parent, gnc_acc, template_list, (h_trans != NULL));
    gnc_trans_templ_delete_glist (template_list);
    
    if (!h_trans)
      return;

    /* GNCInteractor_hide (interactor); */

    /* HBCI Transaction has finished, so now open the gnucash
       transaction dialog and fill in all values. */
    gnc_hbci_maketrans_final (parent, gnc_acc, trans_type, h_trans, FALSE);

    /* Everything finished. */
    HBCI_Transaction_delete (h_trans);
  }
}


/** Store the template_list in the given Account gnc_acc. If dont_ask
    is FALSE, first ask whether they really should be saved. */
void maketrans_save_templates(GtkWidget *parent, Account *gnc_acc, 
			      GList *template_list, gboolean dont_ask)
{
  if (dont_ask || gnc_verify_dialog_parented
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
gnc_hbci_maketrans_final (GtkWidget *parent, Account *gnc_acc,
			  GNC_HBCI_Transtype trans_type,   
			  const HBCI_Transaction *h_trans,
			  gboolean run_until_done)
{
  /* HBCI Transaction has finished, so now open the gnucash
     transaction dialog and fill in all values. */
  gnc_numeric amount;
  XferDialog *transdialog;
  
  transdialog = gnc_xfer_dialog (parent, gnc_acc);
  
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

  /* Run the dialog until the user has either successfully completed the
   * transaction (just clicking OK doesn't always count) or clicked Cancel.
   * Return TRUE if the transaction was a success, FALSE otherwise.
   */
  return run_until_done 
    ? gnc_xfer_dialog_run_until_done( transdialog )
    : TRUE;
}
