/********************************************************************\
 * generic-import.h -- Functions and utilities to help writing      * 
 * import modules.  See file generic-import-design.txt for          *
 * description                                                      *
 *                        (GnuCash)                                 *
 * Copyright (C) 2002 Benoit Grégoire <bock@step.polymtl.ca>        *
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

#ifndef GENERIC_IMPORT_H
#define GENERIC_IMPORT_H

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

    account_online_id_value: The string containing your account_id

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

  Return: A pointer to the found or created Account, or NULL if no
  account was found or created.

*/
Account * gnc_import_select_account(char * account_online_id_value,
				    char auto_create,
				    char * account_human_description,
				    gnc_commodity * new_account_default_commodity,
				    GNCAccountType new_account_default_type);

/* The gnc_import_select_commodity():

  Must be called with a string containing a unique identifier for the
  commodity.  If an commodity with a matching exchange_code is
  found, the function immediately returns with a pointer to that
  commodity.  Otherwise, the user may be prompted to select a GnuCash
  account or create a new one (in both cases, the exchange_code is written
  written to the commodity's exchange_code field, overwriting anything that
  was there before.

  Params:

    char * exchange_code: The string containing the code for which you
    want a matching commodity.  A CUISP code or similar UNIQUE code.
    The stock ticker is NOT appropriate, unless you have no other option.

    char auto_create: If 0, if the exchange_code value in unknown,
    the function returns NULL, otherwise, the user will be asked to 
    create a new account.

    char * default_fullname: A human-readable description of the commodity, such
    as the stock name.  Can be NULL. If it is not NULL, it will be shown
    to the user when selecting a commodity.  It will also be used as
    the default if a new commodity is created.

     char * default_mnemonic:  Usually the stock ticker or similar. Can be NULL.
     If it is not NULL, it will be shown
    to the user when selecting a commodity.  It will also be used as
    the default if a new commodity is created.


  Return: A pointer to the found or created commodity, or NULL if no
  account was found or created.

*/
gnc_commodity * gnc_import_select_commodity(char * exchange_code,
				    char auto_create,
				    char * default_fullname,
				    char * default_mnemonic);

/* Your import module should create a new transaction in the current book,
   add as many splits as it knows about, and associate each split with an 
   account.  It should then call gnc_import_add_trans() with that transaction
   if a transaction with the same online_id kvp_frame exists in any of the
   transaction's split's accounts, the transaction will be destroyed.  
   Otherwise it will be added.
   Not yet implemented:  GUI to balance the transaction using heuristics
*/
void gnc_import_add_trans(Transaction *trans);

/* Setter and getter functions for the online_id kvp_frame for 
   Accounts and Transactions.
*/ 
gchar * gnc_import_get_acc_online_id(Account * account);
void gnc_import_set_acc_online_id(Account * account, gchar * string_value);
gchar * gnc_import_get_trans_online_id(Transaction * transaction);
void gnc_import_set_trans_online_id(Transaction * transaction, gchar * string_value);

typedef enum _gnc_match_probability{
  CERTAIN,
  PROBABLE,
  LIKELY,
  POSSIBLE,
  UNLIKELY,
  IMPOSSIBLE
} GNC_match_probability;



#endif
