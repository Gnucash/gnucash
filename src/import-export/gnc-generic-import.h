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

/* The gnc_import_select_account() method should be called with a string 
   containing a unique identifier for the account.  If an account with a
   matching online_id kvp_frame is found, the function immediately returns
   with a pointer to that account.  Otherwise, the user is prompted to 
   select a GnuCash account or create a new one.
   In both cases, the unique identifier is written to the account's
   kvp_frame, so the user won't be prompted again.                 
*/
Account * gnc_import_select_account(char * account_online_id_value);

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
