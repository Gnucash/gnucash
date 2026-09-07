/********************************************************************\
 * dialog-reconciled-balance.h -- reconciled balance dialog            *
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

#ifndef DIALOG_RECONCILED_BALANCE_H
#define DIALOG_RECONCILED_BALANCE_H

#include <gtk/gtk.h>
#include "Account.h"

/** Show the reconciled balances for @a account, and let the user add or
 *  remove them.  If a dialog is already open for the account it is
 *  raised instead of a second one being created.
 *
 *  @param parent The parent window.
 *  @param account The account whose reconciled balances to show.
 */
void gnc_reconciled_balance_dialog (GtkWindow *parent, Account *account);

/** As gnc_reconciled_balance_dialog(), but pre-fill the entry row with
 *  @a date and the account's balance on that date.  This is what the
 *  register uses: the user picks the row matching their statement and
 *  the figures are already there to confirm.
 */
void gnc_reconciled_balance_dialog_for_date (GtkWindow *parent,
                                            Account *account,
                                            time64 date);

#endif /* DIALOG_RECONCILED_BALANCE_H */
