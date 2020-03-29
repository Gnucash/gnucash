/********************************************************************\
 * gnc-ui-balances.c -- utility functions for calculating           *
 *                      account and owner balances used in the      *
 *                      the GnuCash UI                              *
 * Copyright (C) 2000 Dave Peticolas <dave@krondo.com>              *
 * Copyright (C) 2011 Geert Janssens <geert@kobaltwit.be>           *
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



#ifndef GNC_UI_BALANCES_H_
#define GNC_UI_BALANCES_H_

#include <glib.h>

#include "Account.h"
#include "gncOwner.h"
#include "qof.h"

/********************************************************************
 * Balance calculations related to accounts
 ********************************************************************/

gnc_numeric
gnc_ui_account_get_balance_full (xaccGetBalanceInCurrencyFn fn,
                                 const Account *account,
                                 gboolean recurse,
                                 gboolean *negative,
                                 const gnc_commodity *commodity);

/**
 * This routine retrieves the total balance in an account, possibly
 * including all sub-accounts under the specified account.
 *
 * @param account           The account to retrieve data about.
 * @param include_children  Include all sub-accounts of this account.
 */
gnc_numeric gnc_ui_account_get_balance (const Account *account,
                                        gboolean include_children);

// gnc_numeric gnc_ui_account_get_balance_in_currency (const Account *account,
//         const gnc_commodity *currency,
//         gboolean recurse);
/**
 * This routine retrieves the reconciled balance in an account,
 * possibly including all sub-accounts under the specified account.
 *
 * @param account           The account to retrieve data about.
 * @param include_children  Include all sub-accounts of this account.
 */
gnc_numeric gnc_ui_account_get_reconciled_balance(const Account *account,
        gboolean include_children);

/**
 * Wrapper around gnc_ui_account_get_balance_internal that converts
 * the resulting number to a character string.  The number is
 * formatted according to the specification of the account currency.
 *
 * @param fn        The underlying function in Account.c to call to retrieve
 *                  a specific balance from the account.
 * @param account   The account to retrieve data about.
 * @param recurse   Include all sub-accounts of this account.
 * @param negative  An indication of whether or not the returned value
 *                  is negative.  This can be used by the caller to
 *                  easily decode whether or not to color the output.
 */
gchar *
gnc_ui_account_get_print_balance (xaccGetBalanceInCurrencyFn fn,
                                  const Account *account,
                                  gboolean recurse,
                                  gboolean *negative);

/**
 * Wrapper around gnc_ui_account_get_balance_internal that converts
 * the resulting number to a character string.  The number is
 * formatted according to the specification of the default reporting
 * currency.
 *
 * @param fn        The underlying function in Account.c to call to retrieve
 *                  a specific balance from the account.
 * @param account   The account to retrieve data about.
 * @param recurse   Include all sub-accounts of this account.
 * @param negative  An indication of whether or not the returned value
 *                  is negative.  This can be used by the caller to
 *                  easily decode whether or not to color the output.
 */
gchar *
gnc_ui_account_get_print_report_balance (xaccGetBalanceInCurrencyFn fn,
        const Account *account,
        gboolean recurse,
        gboolean *negative);

gnc_numeric gnc_ui_account_get_balance_as_of_date (Account *account,
						   time64 date,
						   gboolean include_children);
gnc_numeric
gnc_ui_account_get_reconciled_balance_as_of_date (Account *account,
                                                  time64 date,
                                                  gboolean include_children);

/********************************************************************
 * Balance calculations related to owners
 ********************************************************************/

/** Get the balance for the underlying owner object.
 *  The returned value is always positive,
 *  intended to be displayed to a user. However the real sign
 *  of the balance is indicated via the "negative" parameter.
 */
gnc_numeric gnc_ui_owner_get_balance_full (GncOwner *owner,
        gboolean *negative,
        const gnc_commodity *commodity);

/** Get the balance for the underlying owner object in string format
 *  and the owner's native currency.
 *  The returned value is always positive,
 *  intended to be displayed to a user. However the real sign
 *  of the balance is indicated via the "negative" parameter.
 */
gchar * gnc_ui_owner_get_print_balance (GncOwner *owner,
                                        gboolean *negative);

/** Get the balance for the underlying owner object in string format
 *  and in the default report currency.
 *  The returned value is always positive,
 *  intended to be displayed to a user. However the real sign
 *  of the balance is indicated via the "negative" parameter.
 */
gchar * gnc_ui_owner_get_print_report_balance (GncOwner *owner,
        gboolean *negative);


#endif /* GNC_UI_BALANCES_H_ */
