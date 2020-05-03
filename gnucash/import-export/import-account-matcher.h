/********************************************************************\
 * import-account-matcher.h - flexible account picker/matcher       *
 *                                                                  *
 * Copyright (C) 2002 Benoit Grégoire <bock@step.polymtl.ca>        *
 * Copyright (C) 2012 Robert Fewell                                 *
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
/** @addtogroup Import_Export
    @{ */
/**@file import-account-matcher.h
  @brief  Generic and very flexible account matcher/picker
 @author Copyright (C) 2002 Benoit Grégoire <bock@step.polymtl.ca>
 */
#ifndef IMPORT_ACCOUNT_MATCHER_H
#define IMPORT_ACCOUNT_MATCHER_H

#include "Account.h"
#include <gtk/gtk.h>

#include "gnc-tree-view-account.h"

typedef struct
{
    GtkWidget           *dialog;                         /* Dialog Widget */
    GtkWidget           *new_button;                     /* new account button Widget */
    GtkWidget           *ok_button;                      /* ok button Widget */
    GncTreeViewAccount  *account_tree;                   /* Account tree */
    GtkWidget           *account_tree_sw;                /* Scroll Window for Account tree */
    gboolean             auto_create;                    /* Auto create retAccount, can be used to step over this stage */
    const gchar         *account_human_description;      /* description for on line id, incoming */
    const gchar         *account_online_id_value;        /* On line id value, incoming */
    GtkWidget           *account_online_id_label;        /* the label Widget for the on line id, incoming */
    const gnc_commodity *new_account_default_commodity;  /* new account default commodity, incoming */
    GNCAccountType       new_account_default_type;       /* new account default type, incoming */
    Account             *default_account;                /* default account for selection, incoming */
    Account             *retAccount;                     /* Account value returned to caller */
    GtkWidget           *pwhbox;                         /* Placeholder Warning HBox */
    GtkWidget           *pwarning;                       /* Placeholder Warning Label */
} AccountPickerDialog;

/**  Must be called with a string containing a unique identifier for the
  account.  If an account with a matching online_id is
  found, the function immediately returns with a pointer to that
  account.  Otherwise, the user is prompted to select a GnuCash
  account or create a new one (in both cases, the unique identifier is
  written to the account, so the user won't be prompted
  again).  If the user refuses to select or create an account, NULL is
  returned.

  @param parent The parent widget. Can be NULL.

    @param account_online_id_value The string containing your unique
    account_id coming from some string of your module.  This is the
    normal mode of operation. Can be NULL.

    If account_online_id_value==NULL, you basically end up with an account
    selector that allows you to select an account whose GncGUID will be
    remembered elsewhere.  You would fill account_human_description to tell
    the user what he is looking for.  In this mode, the  online_id
    field of the found account will not be touched.  To use this mode,
    auto_create must NOT be set to 0.

    @param account_human_description
	 A human-readable description of
    the account.  Can be NULL. If it is not NULL, it will be shown before
    the id in the account matching dialog.  It will also be used as
    the default account name if a new account is created.

    @param new_account_default_commodity
	 Default commodity of
    the new account. Can be NULL. If not NULL, it will be the
    account's commodity if a new account is created.  Also, if not
    NULL, the function will also warn the user if the found or created
    account's commodity doesn't match.

    @param new_account_default_type
	 Default account type of a
    new account. Can be NULL.  If not ACCT_TYPE_NONE, it will be the
    account's type if a new account is created.  If not
    ACCT_TYPE_NONE, the function will also warn the user if the found
    or created account's commodity doesn't match.

    @param auto_create
         Only active if no account with the
    account_online_id_value could be found in gnucash, or if online-id
    was NULL. In that case, if auto_create is TRUE (nonzero), the user
    will be asked to create a new account. If auto_create is FALSE
    (zero), this function will simply return NULL but will neither
    select nor create any account.

    @param default_selection If not NULL, that account will be
    pre-selected by default.

    @param ok_pressed A pointer to gboolean.  If non-NULL, whether or
    not the picker dialog was closed by the user pressing ok will be
    stored in the parameter.  If no dialog was created by the
    gnc_import_select_account() call, TRUE is always returned.

  @return A pointer to the found or created Account, or NULL if no
  account was found or created.
*/
Account * gnc_import_select_account(GtkWidget *parent,
                                    const gchar * account_online_id_value,
                                    gboolean auto_create,
                                    const gchar * account_human_description,
                                    const gnc_commodity * new_account_default_commodity,
                                    GNCAccountType new_account_default_type,
                                    Account * default_selection,
                                    gboolean * ok_pressed
                                   );

#endif
/**@}*/
