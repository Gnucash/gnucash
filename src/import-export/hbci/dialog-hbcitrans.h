/********************************************************************\
 * dialog-hbcitrans.h -- dialog for HBCI transaction data           *
 * Copyright (C) 2002 Christian Stimming                            *
 * Copyright (C) 2004 Bernd Wagner (changes for                     *
 *                     online transaction templates)                *
 * Copyright (c) 2006 David Hampton <hampton@employees.org>         *
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

#ifndef DIALOG_HBCITRANS_H
#define DIALOG_HBCITRANS_H

#include <aqbanking/banking.h>
#include <aqbanking/transaction.h>
#include <aqbanking/job.h>

#include "Account.h"
#include "gnc-hbci-utils.h"

/** The dialog data structure. */
typedef struct _trans_data HBCITransDialog;

typedef enum GNC_HBCI_Transtype
{
    SINGLE_TRANSFER = 0,
    SINGLE_DEBITNOTE,
    SINGLE_INTERNAL_TRANSFER
} GNC_HBCI_Transtype;

#define GNC_RESPONSE_NOW GTK_RESPONSE_YES
#define GNC_RESPONSE_LATER 3

/*AB_TRANSACTION *
gnc_hbci_trans (GtkWidget *parent,
		AB_BANKING *api,
		GNCInteractor *interactor,
		const AB_ACCOUNT *h_acc,
		const HBCI_Customer *customer,
		Account *gnc_acc,
		GNC_HBCI_Transtype type,
		GList **templ);*/

/** Constructor: Create a new HBCITransDialog, fill in the values as
 * specified by the arguments, and return a pointer to it. */
HBCITransDialog *
gnc_hbci_dialog_new (GtkWidget *parent,
                     const AB_ACCOUNT *h_acc,
                     Account *gnc_acc,
                     GNC_HBCI_Transtype trans_type,
                     GList *templ);
/** Destructor */
void gnc_hbci_dialog_delete(HBCITransDialog *td);

/** Return the parent widget */
GtkWidget *gnc_hbci_dialog_get_parent(const HBCITransDialog *td);
/** Return the GList of transaction templates. */
GList *gnc_hbci_dialog_get_templ(const HBCITransDialog *td);
/** Return the change status of the template list */
gboolean gnc_hbci_dialog_get_templ_changed(const HBCITransDialog *td) ;
/** Return the AB_TRANSACTION. */
const AB_TRANSACTION *gnc_hbci_dialog_get_htrans(const HBCITransDialog *td);
/** Return the gnucash Transaction. */
Transaction *gnc_hbci_dialog_get_gtrans(const HBCITransDialog *td);
/** Hide the dialog */
void gnc_hbci_dialog_hide(HBCITransDialog *td);
/** Show the dialog */
void gnc_hbci_dialog_show(HBCITransDialog *td);



int gnc_hbci_dialog_run_until_ok(HBCITransDialog *td,
                                 const AB_ACCOUNT *h_acc);
AB_JOB *
gnc_hbci_trans_dialog_enqueue(const AB_TRANSACTION *hbci_trans, AB_BANKING *api,
                              AB_ACCOUNT *h_acc,
                              GNC_HBCI_Transtype trans_type);
/** Callback function for gnc_xfer_dialog_set_txn_cb(). The user_data
 * has to be a pointer to a HBCITransDialog structure.  */
void gnc_hbci_dialog_xfer_cb(Transaction *trans, gpointer user_data);

/** Execute the OutboxJob, delete it when finished. Returns TRUE if
 * the application should continue, and FALSE if the user wants to
 * enter this job again.  */
gboolean
gnc_hbci_trans_dialog_execute(HBCITransDialog *td, AB_BANKING *api,
                              AB_JOB *job, GNCInteractor *interactor);


#endif
