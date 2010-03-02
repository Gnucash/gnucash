/********************************************************************\
 * dialog-transfer.h -- transfer dialog for GnuCash                 *
 * Copyright (C) 1999 Linas Vepstas                                 *
 * Copyright (C) 2000 Dave Peticolas                                *
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

#ifndef DIALOG_TRANSFER_H
#define DIALOG_TRANSFER_H

#include "Account.h"
#include "QuickFill.h"

typedef struct _xferDialog XferDialog;

/** Opens up a window to do an automatic transfer between accounts
 *
 * Args:   parent  - the parent of the window to be created
 *         initial - the initial account in the from/to fields
 * Return: XferDialog structure
 */
XferDialog * gnc_xfer_dialog(GtkWidget * parent, Account *initial);

/** Run the dialog until the user has either successfully completed the
 * transaction (just clicking OK doesn't always count) or clicked Cancel.
 * Return TRUE if the transaction was a success, FALSE otherwise.
 */
gboolean gnc_xfer_dialog_run_until_done( XferDialog * );

void gnc_xfer_dialog_close( XferDialog * );

/*********** Access routines ***********/
void gnc_xfer_dialog_set_title( XferDialog *, const gchar * );

/** Set the label of the topmost table */
void gnc_xfer_dialog_set_information_label( XferDialog *,
        const gchar * );

/** Add a button with a user-specified label and "clicked" callback.
 * For now this doesn't offer a lot of flexibility, but it doesn't have to.
 */
void gnc_xfer_dialog_add_user_specified_button( XferDialog *xferData,
        const gchar *label,
        GCallback callback,
        gpointer user_data );

void gnc_xfer_dialog_toggle_currency_table ( XferDialog *xferData,
        gboolean show_table );

void gnc_xfer_dialog_set_from_account_label( XferDialog *,
        const gchar * );
void gnc_xfer_dialog_set_to_account_label( XferDialog *, const gchar * );

/** Set the buttons for "Show Income/Expense" */
void gnc_xfer_dialog_set_from_show_button_active( XferDialog *, gboolean );
void gnc_xfer_dialog_set_to_show_button_active( XferDialog *, gboolean );

/**   select the from account in a xfer dialog                       */
void gnc_xfer_dialog_select_from_account(XferDialog *xferData,
        Account *account);
/**   select the to account in a xfer dialog                         */
void gnc_xfer_dialog_select_to_account(XferDialog *xferData,
                                       Account *account);

void gnc_xfer_dialog_select_from_currency(XferDialog *xferData, gnc_commodity *cur);
void gnc_xfer_dialog_select_to_currency(XferDialog *xferData, gnc_commodity *cur);

/** Prevent changes to the from account tree in an xfer dialog     */
void gnc_xfer_dialog_lock_from_account_tree(XferDialog *xferData );
/** Prevent changes to the to account tree in an xfer dialog */
void gnc_xfer_dialog_lock_to_account_tree(XferDialog *xferData );
/** Prevent changes to the from account tree in an xfer dialog */
void gnc_xfer_dialog_hide_from_account_tree(XferDialog *xferData );
/** Prevent changes to the to account tree in an xfer dialog */
void gnc_xfer_dialog_hide_to_account_tree(XferDialog *xferData );


/**
 *   set the amount in the given xfer dialog
 *
 * Args:   xferData - xfer dialog structure
 *         amount   - the amount to set
 * Return: none
 */
void gnc_xfer_dialog_set_amount(XferDialog *xferData, gnc_numeric amount);

/**
 *   set the description in the given xfer dialog
 *
 * Args:   xferData    - xfer dialog structure
 *         description - the description to set
 * Return: none
 */
void gnc_xfer_dialog_set_description(XferDialog *xferData,
                                     const char *description);

/**   set the memo in the given xfer dialog
 *
 * Args:   xferData    - xfer dialog structure
 *         memo        - the memo to set
 * Return: none
 */
void gnc_xfer_dialog_set_memo(XferDialog *xferData, const char *memo);

/**
 *   set the num in the given xfer dialog
 *
 * Args:   xferData    - xfer dialog structure
 *         num        - the num to set
 * Return: none
 */
void gnc_xfer_dialog_set_num(XferDialog *xferData, const char *num);

/**
 *   Set the date in the given xfer dialog
 *
 * Args:   xferData    - xfer dialog structure
 *         set_date    - the date to set
 * Return: none
 */
void gnc_xfer_dialog_set_date(XferDialog *xferData, time_t set_time);

/** Set the exchange rate.  If exchange-rate is 0, then do nothing */
void gnc_xfer_dialog_set_exchange_rate(XferDialog *xferData,
                                       gnc_numeric exchange_rate);

/** Indicate whether the dialog should quickfill based on the "To" account,
 * rather than the default which is the "From" account.
 */
void gnc_xfer_dialog_quickfill_to_account(XferDialog *xferData,
        gboolean qf_to_account );

/**
 *   Set the dialog as an "exchange-dialog", which means that the
 *   Transfer Information table read-only (and the dialog
 *   will NOT create a transaction when it is closed).
 *
 * In other words: Indicate that this is just trying to obtain the
 * to_amount, so make the Transfer Information read-only and the
 * dialog will NOT create a new transaction.  Pass in the location to
 * store the resulting exchange_rate when the dialog is complete.  The
 * caller should call the dialog 'run' function to make sure exch_rate
 * pointer remains valid.
 *
 * Args:   xferData - xfer dialog structure
 *         exch_rate - place to store the exchange rate at exit
 * Return: none
 */
void gnc_xfer_dialog_is_exchange_dialog(XferDialog *xferData,
                                        gnc_numeric * exch_rate);


/** Callback function type for gnc_xfer_dialog_set_txn_cb().
 *
 * @param new_trans The newly created transaction, or NULL to notify
 * of destruction of the xferDialog.
 *
 * @param user_data User-supplied pointer to some data */
typedef void (*gnc_xfer_dialog_cb)(Transaction *new_trans,
                                   gpointer user_data);

/** Register a callback function to be called with the created
 * Transaction as soon as it is created.
 *
 * Note: The caller is responsible to unregister this function in case
 * it becomes invalid. In other words, you have to reset the handler
 * to NULL in case this dialog exists longer than your callback
 * function.
 *
 * Also note: The callback will additionally be called with
 * transaction==NULL to notify the callback of destruction of the
 * XferData structure.
 *
 * @param xferData Dialog data structure
 *
 * @param handler Callback function that should be notified of the
 * newly created Transaction
 *
 * @param user_data User-supplied pointer that will be passed to
 * handler. */
void gnc_xfer_dialog_set_txn_cb(XferDialog *xferData,
                                gnc_xfer_dialog_cb handler,
                                gpointer user_data);

/* Uses the XferDialog to obtain from the user an explicit exchange
   rate.  This exchange rate will then be uses to converting 'amount',
   which is given in the commodity of the register Account, reg_acc,
   into a split value for a split whose Account is the commodity
   specified by xfer_com.

   The 'exch_rate' argument is used to set the initial value of the
   rate.  If the dialog completes sucessfully 'FALSE' is returned and
   'exch_rate' is also used to store the converted value.  Otherwise,
   TRUE is returned and the 'exch_rate' argument is undefined.
*/
gboolean gnc_xfer_dialog_run_exchange_dialog(
    XferDialog *xfer, gnc_numeric *exch_rate, gnc_numeric amount,
    Account *reg_acc, Transaction *txn, gnc_commodity *xfer_com);

#endif
