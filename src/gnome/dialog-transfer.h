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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
\********************************************************************/

#ifndef DIALOG_TRANSFER_H
#define DIALOG_TRANSFER_H

#include "Account.h"
#include "account-tree.h"
#include "QuickFill.h"

typedef struct _xferDialog XferDialog;

XferDialog * gnc_xfer_dialog(GtkWidget * parent, Account *initial);

gboolean gnc_xfer_dialog_run_until_done( XferDialog * );

void gnc_xfer_dialog_close( XferDialog * );

/*********** Access routines ***********/
void gnc_xfer_dialog_set_title( XferDialog *, const gchar * );

/* set the label of the topmost frame */
void gnc_xfer_dialog_set_information_frame_label( XferDialog *,
                                                  const gchar * );

/* Add a button with a user-specified label and "clicked" callback.
 * For now this doesn't offer a lot of flexibility, but it doesn't have to.
 */
void gnc_xfer_dialog_add_user_specified_button( XferDialog *xferData,
                                                const gchar *label,
                                                GtkSignalFunc callback,
                                                gpointer user_data );

void gnc_xfer_dialog_toggle_currency_frame( XferDialog *xferData,
                                            gboolean show_frame );

void gnc_xfer_dialog_set_from_account_frame_label( XferDialog *,
                                                   const gchar * );
void gnc_xfer_dialog_set_to_account_frame_label( XferDialog *, const gchar * );

/* set the buttons for "Show Income/Expense" */
void gnc_xfer_dialog_set_from_show_button_active( XferDialog *, gboolean );
void gnc_xfer_dialog_set_to_show_button_active( XferDialog *, gboolean );

void gnc_xfer_dialog_select_from_account(XferDialog *xferData,
                                         Account *account);
void gnc_xfer_dialog_select_to_account(XferDialog *xferData,
                                       Account *account);

/* prevent the user from changing an account tree */
void gnc_xfer_dialog_lock_from_account_tree(XferDialog *xferData );
void gnc_xfer_dialog_lock_to_account_tree(XferDialog *xferData );

void gnc_xfer_dialog_set_amount(XferDialog *xferData, gnc_numeric amount);
void gnc_xfer_dialog_set_description(XferDialog *xferData,
                                     const char *description);
void gnc_xfer_dialog_set_date(XferDialog *xferData, time_t set_time);

/* Indicate whether the dialog should quickfill based on the "To" account,
 * rather than the default which is the "From" account.
 */
void gnc_xfer_dialog_quickfill_to_account(XferDialog *xferData,
                                          gboolean qf_to_account );

#endif
