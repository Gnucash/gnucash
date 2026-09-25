/********************************************************************\
 * account-quickfill.h -- Create an account-name quick-fill         *
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
 *                                                                  *
\********************************************************************/
/** @addtogroup QuickFill Auto-complete typed user input.
   @{
*/
/** @addtogroup Account_QuickFill Account Names

    For systems with a large number of accounts (>500), the creation
    of the account name quickfill can take a significant amount of
    time (tens of seconds in bad cases).  This routine will build
    a cache of account names that can be shared by all registers,
    thus dramatically improving the performance of opening a new
    register.
    @{

    @file account-quickfill.h
    @brief Create an account-name quick-fill
    @author Copyright (C) 2004 Linas Vepstas <linas@linas.org>
*/

#ifndef QUICKFILL_ACCOUNT_H
#define QUICKFILL_ACCOUNT_H

#include <gio/gio.h>

#include "Account.h"
#include "QuickFill.h"

G_BEGIN_DECLS

typedef gboolean (*AccountBoolCB) (Account*, gpointer);

#define GNC_TYPE_ACCOUNT_LIST_ITEM (gnc_account_list_item_get_type ())
G_DECLARE_FINAL_TYPE (GncAccountListItem, gnc_account_list_item, GNC,
                      ACCOUNT_LIST_ITEM, GObject)

/**
 * A GTK4 list-model item representing one account in a shared quickfill.
 * The item keeps account identity separate from its display name, so users
 * of the model can retain a selection while account names change.
 */
Account* gnc_account_list_item_get_account (GncAccountListItem *item);
const gchar* gnc_account_list_item_get_name (GncAccountListItem *item);

/** Create/fetch a quickfill of account names.
 *
 *  The quickfill is created out of all of the subaccounts
 *  in the account group, filtered by the 'skip_cb' callback.
 *  If 'skip_cb' is not NULL, and if it returns TRUE when passed
 *  a particular account, then that account won't be included in
 *  the quickfill.  The 'cb_data' is passed to the callback.
 *
 *  The quickfill is created only once; it is then stored with
 *  the QofBook that is the parent of the root account.  It is
 *  automatically destroyed when the QofBook is destroyed.
 *
 *  Multiple, distinct quickfills, for different uses, are allowed.
 *  Each is identified with the 'key'.  Be sure to use distinct,
 *  unique keys that don't conflict with other users of QofBook.
 *
 *  This code keeps the quickfill and its shared list model synchronized with
 *  account additions, removals, name changes and visibility changes.
 */
QuickFill*
gnc_get_shared_account_name_quickfill (Account* root, const char* key,
                                       AccountBoolCB skip_cb, gpointer cb_data);
/**
 * Return the GTK4 model companion to the shared account quickfill. The
 * model is owned by the book and changes whenever its account cache changes.
 */
GListModel* gnc_get_shared_account_name_list_model (Account* root,
                                                    const char* key,
                                                    AccountBoolCB cb,
                                                    gpointer cb_data);

G_END_DECLS

#endif

/** @} */
/** @} */
