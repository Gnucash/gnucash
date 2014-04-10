/********************************************************************\
 * dialog-account.h -- window for creating and editing accounts for *
 *                     GnuCash                                      *
 * Copyright (C) 2000 Dave Peticolas <petcola@cs.ucdavis.edu>       *
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

#ifndef DIALOG_ACCOUNT_H
#define DIALOG_ACCOUNT_H

#include "config.h"

#include "Account.h"
#include "Group.h"

/** @addtogroup GUI
    @{ */

/** @file dialog-account.h
 *
 *  This file contains the functions to present a gui to the user for
 *  creating a new account or editing an existing account.
 *
 *  @brief Dialog fo create/edit an account.
 *  @author Copyright (C) 1997 Robin D. Clark
 *  @author Copyright (C) 2000 Dave Peticolas
 */

/** PROTOTYPES ******************************************************/
typedef struct _AccountWindow  AccountWindow;

/** @name Non-Modal */
/** @{ */

/** Disply a window for editing the attributes of an existing account.
 *
 *  @param group This parameter is not used.
 */
AccountWindow *gnc_ui_edit_account_window (Account *account);


/** Disply a window for creating a new account
 *
 *  @param group This parameter is not used.
 */
AccountWindow *gnc_ui_new_account_window (AccountGroup *group);


/** Disply a window for creating a new account.  This function will
 *  also initially set the parent account of the new account to what
 *  the caller specified.  The user is free, however, to choose any
 *  parent account they wish.
 *
 *  @param group This parameter is not used.
 *
 *  @param parent The initially selected parent account.
 */
AccountWindow *gnc_ui_new_account_window_with_default (AccountGroup *group,
						       Account * parent);


/** Disply a window for creating a new account.  This function will
 *  restrict the available account type values to the list specified by the caller.
 *
 *  @param group This parameter is not used.
 *
 *  @param valid_types A GList of GNCAccountType gints [as pointers]
 *  which are allowed to be created.  The calling function is
 *  responsible for freeing this list.
 */
AccountWindow *gnc_ui_new_account_with_types (AccountGroup *unused,
					      GList *valid_types);
/** @} */



/** @name Modal */
/** @{ */

/** Disply a modal window for creating a new account
 *
 *  @param group This parameter is not used.
 */
Account * gnc_ui_new_accounts_from_name_window (const char *name);

/** Disply a modal window for creating a new account.  This function
 *  will restrict the available account type values to the list
 *  specified by the caller.
 *
 *  @param name The account name/path to be created.  This parameter
 *  is not used for determining the initially selected parent account.
 *
 *  @param valid_types A GList of GNCAccountType gints [as pointers]
 *  which are allowed to be created.  The calling function is
 *  responsible for freeing this list.
 *
 *  @return A pointer to the newly created account.
 */
/* Note that the caller owns the valid_types list */
Account * gnc_ui_new_accounts_from_name_window_with_types (const char *name,
							   GList *valid_types);


/** Disply a modal window for creating a new account.  This function
 *  will restrict the available account type values to the list
 *  specified by the caller.
 *
 *  @param name The account name/path to be created.  This parameter
 *  is not used for determining the initially selected parent account.
 *
 *  @param valid_types A GList of GNCAccountType gints [as pointers]
 *  which are allowed to be created.  The calling function is
 *  responsible for freeing this list.
 *
 *  @param default_commodity The commodity to initially select when
 *  the dialog is presented.
 *
 *  @param parent The initially selected parent account.
 *
 *  @return A pointer to the newly created account.
 */
Account * gnc_ui_new_accounts_from_name_with_defaults (const char *name,
						       GList *valid_types,
						       gnc_commodity * default_commodity,
						       Account * parent);

/** @} */
/** @} */

void gnc_ui_edit_account_window_raise (AccountWindow * winData);

/*
 * register a callback that get's called when the account has changed
 * so significantly that you need to destroy yourself.  In particular
 * this is used by the ledger display to destroy ledgers when the
 * account type has changed.
 */
void gnc_ui_register_account_destroy_callback (void (*cb)(Account *));

#endif
