/********************************************************************\
 * dialog-hbcitrans.h -- dialog for HBCI transaction data           *
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

#ifndef DIALOG_HBCITRANS_H
#define DIALOG_HBCITRANS_H

#include <gnome.h>

#include <openhbci2/api.h>
#include <openhbci2/customer.h>
#include <openhbci2/transaction.h>
#include <openhbci2/outboxjob.h>

#include "Account.h"
#include "gnc-hbci-utils.h"

/** The dialog data structure. */
typedef struct _trans_data HBCITransDialog;

typedef enum GNC_HBCI_Transtype {
  SINGLE_TRANSFER = 0,
  SINGLE_DEBITNOTE
} GNC_HBCI_Transtype;


/*HBCI_Transaction *
gnc_hbci_trans (GtkWidget *parent,
		HBCI_API *api,
		GNCInteractor *interactor,
		const gnc_HBCI_Account *h_acc,
		const HBCI_Customer *customer,
		Account *gnc_acc,
		GNC_HBCI_Transtype type,
		GList **templ);*/

/** Constructor: Create a new HBCITransDialog, fill in the values as
 * specified by the arguments, and return a pointer to it. */
HBCITransDialog *
gnc_hbci_dialog_new (GtkWidget *parent,
		     const gnc_HBCI_Account *h_acc,
		     const HBCI_Customer *customer,
		     Account *gnc_acc,
		     GNC_HBCI_Transtype trans_type,
		     GList *templ);
/** Destructor */
void gnc_hbci_dialog_delete(HBCITransDialog *td);

/** Return the parent widget */
GtkWidget *gnc_hbci_dialog_get_parent(const HBCITransDialog *td);
/** Return the GList of transaction templates. */
GList *gnc_hbci_dialog_get_templ(const HBCITransDialog *td);
/** Return the HBCI_Transaction. */
const HBCI_Transaction *gnc_hbci_dialog_get_htrans(const HBCITransDialog *td);
/** Return the gnucash Transaction. */
Transaction *gnc_hbci_dialog_get_gtrans(const HBCITransDialog *td);
/** Hide the dialog */
void gnc_hbci_dialog_hide(HBCITransDialog *td);
/** Show the dialog */
void gnc_hbci_dialog_show(HBCITransDialog *td);



int gnc_hbci_dialog_run_until_ok(HBCITransDialog *td, 
				 const gnc_HBCI_Account *h_acc);
HBCI_OutboxJob *
gnc_hbci_trans_dialog_enqueue(HBCITransDialog *td, HBCI_API *api,
			      HBCI_Outbox *outbox,
			      const HBCI_Customer *customer, 
			      gnc_HBCI_Account *h_acc, 
			      GNC_HBCI_Transtype trans_type);
/** Callback function for gnc_xfer_dialog_set_txn_cb(). The user_data
 * has to be a pointer to a HBCITransDialog structure.  */
void gnc_hbci_dialog_xfer_cb(Transaction *trans, gpointer user_data);

/** Execute the OutboxJob, delete it when finished. Returns TRUE if
 * the application should continue, and FALSE if the user wants to
 * enter this job again.  */
gboolean 
gnc_hbci_trans_dialog_execute(HBCITransDialog *td, HBCI_API *api, 
			      HBCI_Outbox *outbox,
			      HBCI_OutboxJob *job, GNCInteractor *interactor);


#endif
