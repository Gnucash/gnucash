/********************************************************************\
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
/**@file
 * \brief Account-matcher.h: A very generic and flexible account matcher/picker
 \author Copyright (C) 2002 Benoit Grégoire <bock@step.polymtl.ca>
 */
#ifndef ACCOUNT_MATCHER_H
#define ACCOUNT_MATCHER_H
 
#include "Account.h"

/* The gnc_import_select_account():

  Must be called with a string containing a unique identifier for the
  account.  If an account with a matching online_id kvp_frame is
  found, the function immediately returns with a pointer to that
  account.  Otherwise, the user is prompted to select a GnuCash
  account or create a new one (in both cases, the unique identifier is
  written to the account's kvp_frame, so the user won't be prompted
  again).  If the user refuses to select or create an account, NULL is
  returned.

  Params:

    account_online_id_value: The string containing your unique account_id
    coming from some string of your module.  This is the normal mode of
    operation.

    If account_online_id_value==NULL, you basically end up with an account
    selector that allows you to select an account whose GUID will be 
    remembered elsewhere.  You would fill account_human_description to tell
    the user what he is looking for.  In this mode, the  online_id
    kvp_frame of the found account will not be touched.  To use this mode,
    auto_create must NOT be set to 0.  

    gchar * account_human_description: A human-readable description of
    the account.  Can be NULL. If it is not NULL, it will be shown before
    the id in the account matching dialog.  It will also be used as
    the default account name if a new account is created.

    gnc_commodity * new_account_default_currenc: Default commodity of
    the new account. Can be NULL. If not NULL, it will be the
    account's commodity if a new account is created.  Also, if not
    NULL, the function will also warn the user if the found or created
    account's commodity doesn't match.

    GNCAccountType new_account_default_type: Default account type of a
    new account. Can be NULL.  If not NO_TYPE, it will be the
    account's type if a new account is created.  If not NO_TYPE, the
    function will also warn the user if the found or created account's
    commodity doesn't match.

    char auto_create: If 0, if the account_online_id_value in unknown,
    the function returns NULL, otherwise, the user will be asked to 
    create a new account.

    Account * default_selection: If not NULL, that account will be 
    pre-selected by default.

  Return: A pointer to the found or created Account, or NULL if no
  account was found or created.

*/
Account * gnc_import_select_account(char * account_online_id_value,
				    char auto_create,
				    char * account_human_description,
				    gnc_commodity * new_account_default_commodity,
				    GNCAccountType new_account_default_type,
				    Account * default_selection);

#endif
