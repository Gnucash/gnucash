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

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _xferDialog XferDialog;

/** Opens up a window to do an automatic transfer between accounts
 *
 * Args:   parent  - the parent of the window to be created
 *         initial - the initial account in the from/to fields
 * Return: XferDialog structure
 */
XferDialog * gnc_xfer_dialog(GtkWidget * parent, Account *initial);

/** Callback invoked exactly once when an asynchronous transfer dialog closes.
 * @param completed TRUE only if the transaction or exchange-rate update was
 *                  committed; FALSE for cancel, close, or session shutdown.
 * @param user_data Caller-owned pointer supplied to gnc_xfer_dialog_run_async().
 */
typedef void (*gnc_xfer_dialog_finished_cb)(gboolean completed,
                                            gpointer user_data);

/** Present a transfer dialog without entering a nested main loop. The callback
 * is invoked exactly once during dialog teardown and the XferDialog is invalid
 * after it returns. */
void gnc_xfer_dialog_run_async (XferDialog *xferData,
                                gnc_xfer_dialog_finished_cb finished_cb,
                                gpointer user_data);

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

/** Set the "sensitive" state of the amount field to the given value */
void gnc_xfer_dialog_set_amount_sensitive(XferDialog *xferData, gboolean is_sensitive);

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
void gnc_xfer_dialog_set_date(XferDialog *xferData, time64 set_time);

/** Set the "sensitive" state of the date field to the given value */
void gnc_xfer_dialog_set_date_sensitive(XferDialog *xferData, gboolean is_sensitive);

/** Set the dialog's exchange rate edit.  If price_value is 0, then do
 *  nothing.
 */
void gnc_xfer_dialog_set_price_edit(XferDialog *xferData,
				    gnc_numeric price_value);

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

/** Callback invoked by gnc_xfer_dialog_run_exchange_async(). The rate is valid
 * only when completed is TRUE; cancelled and session-close paths report FALSE. */
typedef void (*gnc_xfer_dialog_exchange_finished_cb)(gboolean completed,
                                                     gnc_numeric exch_rate,
                                                     gpointer user_data);

/** Configure and present an exchange-only transfer dialog without a nested
 * main loop. The callback owns the business continuation and runs exactly once. */
void gnc_xfer_dialog_run_exchange_async(
    XferDialog *xfer, gnc_numeric exch_rate, gnc_numeric amount,
    Account *reg_acc, Transaction *txn, gnc_commodity *xfer_com,
    gboolean expanded, gnc_xfer_dialog_exchange_finished_cb finished_cb,
    gpointer user_data);
#ifdef __cplusplus
}
#endif

#endif
