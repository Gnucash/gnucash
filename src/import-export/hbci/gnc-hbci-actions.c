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

#include "hbci-interaction.h"
#include "gnc-hbci-utils.h"
#include "dialog-hbcitrans.h"



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
	 xaccAccountGetCommoditySCU(gnc_acc),
	 GNC_RND_ROUND); 
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

