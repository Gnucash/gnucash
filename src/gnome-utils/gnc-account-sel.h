/**
 * gnc-account-sel.h -- combobox style account selection widget.
 *
 * Note that this widget will track changes in the account tree, and update
 * itself accordingly.  If an account with the same name exists in the
 * freshly-retreived account list, the widget will re-select that account.
 *
 * Copyright (C) 2002 Joshua Sled <jsled@asynchronous.org>
 * Copyright (c) 2006 David Hampton <hampton@employees.org>
 * All rights reserved.
 **/

/* GnuCash is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Library General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * Gnucash is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

#ifndef GNC_ACCOUNT_SEL_H
#define GNC_ACCOUNT_SEL_H

#include "Account.h"

#define GNC_TYPE_ACCOUNT_SEL          (gnc_account_sel_get_type())
#define GNC_ACCOUNT_SEL(obj)          G_TYPE_CHECK_INSTANCE_CAST (obj, GNC_TYPE_ACCOUNT_SEL, GNCAccountSel)
#define GNC_ACCOUNT_SEL_CLASS(klass)  G_TYPE_CHECK_CLASS_CAST (klass, GNC_TYPE_ACCOUNT_SEL, GNCAccountSelClass)
#define GNC_IS_ACCOUNT_SEL(obj)       G_TYPE_CHECK_INSTANCE_TYPE (obj, GNC_TYPE_ACCOUNT_SEL)

typedef struct
{
    GtkHBox hbox;
    gboolean initDone;
    gboolean isModal;
    GtkListStore *store;
    GtkComboBoxEntry *combo;
    GList *acctTypeFilters;
    GList *acctCommodityFilters;
    gint eventHandlerId;
    /* The state of this pointer also serves as a flag about what state
     * the widget is in WRT the new-account-button ability. */
    GtkWidget *newAccountButton;

#if 0 /* completion not implemented. */
    GCompletion *completion;
#endif /* 0 - completion not implemented */
} GNCAccountSel;

typedef struct
{
    GtkHBoxClass parent_class;

    /* Signals for notification/filtering of changes */
    void (*account_sel_changed) (GNCAccountSel *gas);
} GNCAccountSelClass;

GType      gnc_account_sel_get_type (void);
GtkWidget* gnc_account_sel_new (void);

/**
 * Sets the GAS to the given account.  If the account doesn't exist in the
 * list, then it doesn't change the state of the GAS.  If the account is
 * NULL, then the first list selection is made if set_default_acct is TRUE.
 **/
void       gnc_account_sel_set_account( GNCAccountSel *gas, Account *acct, gboolean set_default_acct );
/**
 * Returns the currently-selected Account.  If, for some reason the selection
 * is in a bad state, NULL will be returned.
 **/
Account*   gnc_account_sel_get_account( GNCAccountSel *gas );

/**
 * The GNCAccountSel can be setup to filter the accounts displayed.
 * @param typeFilters A GList of GNCAccountType identifiers which are allowed.
 * @param commodityFilters A GList of gnc_commodity types which are allowed.
 * The list is copied, of course.
 **/
void gnc_account_sel_set_acct_filters( GNCAccountSel *gas, GList *typeFilters, GList *commodityFilters );

/**
 * Conditional inclusion of a new-account button to the right of the
 * combobox.
 * @param state TRUE if the new-account button is desired, FALSE otherwise.
 **/
void gnc_account_sel_set_new_account_ability( GNCAccountSel *gas,
        gboolean state );

/**
 * Conditional call of the new-account window in modal mode.
 * @param state TRUE if the new-account window should be modal, FALSE otherwise.
 **/
void gnc_account_sel_set_new_account_modal( GNCAccountSel *gas,
        gboolean state );

gint gnc_account_sel_get_num_account( GNCAccountSel *gas );
void gnc_account_sel_purge_account( GNCAccountSel *gas, Account *acc, gboolean recursive);

#endif /* !ndef GNC_ACCOUNT_SEL_H */
