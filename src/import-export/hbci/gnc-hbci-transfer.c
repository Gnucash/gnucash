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
#include "date.h"
#include "Transaction.h"

#include "hbci-interaction.h"
#include "gnc-hbci-utils.h"



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

  api = gnc_hbci_api_new_currentbook (parent, &interactor);
  if (api == NULL) {
    printf("gnc_hbci_maketrans: Couldn't get HBCI API. Nothing will happen.\n");
    return;
  }
  g_assert (interactor);
  
  h_acc = gnc_hbci_get_hbci_acc (api, gnc_acc);
  if (h_acc == NULL) {
    printf("gnc_hbci_maketrans: No HBCI account found. Nothing will happen.\n");
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
      printf("gnc_hbci_maketrans: No HBCI customer found. Nothing will happen.\n");
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
    HBCI_Transaction *h_trans = gnc_hbci_trans (parent, api, interactor,
						h_acc, customer, 
						trans_type);
    if (!h_trans)
      return;

    GNCInteractor_hide (interactor);
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
	char *h_descr = 
	  list_string_concat (HBCI_Transaction_description (h_trans));
	char *othername = 
	  list_string_concat (HBCI_Transaction_otherName (h_trans));
	char *g_descr;

	g_strstrip (h_descr);
	g_strstrip (othername);
	
	g_descr = 
	  g_strdup_printf ("%s %s", 
			   othername, 
			   h_descr);
    
	gnc_xfer_dialog_set_description (transdialog, g_descr);
	
	free (h_descr);
	free (othername);
	g_free (g_descr);
      }

      {
	/* Memo. HBCI's transactionText contains strings like "STANDING
	 * ORDER", "UEBERWEISUNGSGUTSCHRIFT", etc.  */
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
	   
	gnc_xfer_dialog_set_memo (transdialog, g_memo);

	g_free (h_transactionText);
	g_free (h_otherAccountId);
	g_free (h_otherBankCode);
	g_free (g_memo);
      }
      /*gnc_xfer_dialog_set_date(XferDialog *xferData, time_t set_time)*/
    }
    
    HBCI_Transaction_delete (h_trans);
  }
}

