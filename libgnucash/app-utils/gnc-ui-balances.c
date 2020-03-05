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

#include <config.h>

#include "gnc-ui-balances.h"
#include "gnc-ui-util.h"

#include <glib.h>

#include "Account.h"
#include "gncOwner.h"
#include "qof.h"

G_GNUC_UNUSED static QofLogModule log_module = GNC_MOD_GUI;

/********************************************************************
 * Balance calculations related to accounts
 ********************************************************************/

/*
 * This is a wrapper routine around an xaccGetBalanceInCurrency
 * function that handles additional needs of the gui.
 *
 * @param fn        The underlying function in Account.c to call to retrieve
 *                  a specific balance from the account.
 * @param account   The account to retrieve data about.
 * @param recurse   Include all sub-accounts of this account.
 * @param negative  An indication of whether or not the returned value
 *                  is negative.  This can be used by the caller to
 *                  easily decode whether or not to color the output.
 * @param commodity The commodity in which the account balance should
 *                  be returned. If NULL, the value will be returned in
 *                  the commodity of the account. This is normally used
 *                  to specify a currency, which forces the conversion
 *                  of things like stock account values from share
 *                  values to an amount the requested currency.
 */
gnc_numeric
gnc_ui_account_get_balance_full (xaccGetBalanceInCurrencyFn fn,
                                 const Account *account,
                                 gboolean recurse,
                                 gboolean *negative,
                                 const gnc_commodity *commodity)
{
    gnc_numeric balance;

    balance = fn(account, commodity, recurse);

    /* reverse sign if needed */
    if (gnc_reverse_balance (account))
        balance = gnc_numeric_neg (balance);

    /* Record whether the balance is negative. */
    if (negative)
        *negative = gnc_numeric_negative_p(balance);

    return balance;
}

/*
 * This routine retrieves the total balance in an account, possibly
 * including all sub-accounts under the specified account.
 */
gnc_numeric
gnc_ui_account_get_balance (const Account *account, gboolean recurse)
{
    return gnc_ui_account_get_balance_full (xaccAccountGetBalanceInCurrency,
                                            account, recurse, NULL, NULL);
}

/*
 * This routine retrieves the total balance in an account converted to
 * a given currency, possibly including all sub-accounts under the
 * specified account.
 *
gnc_numeric
gnc_ui_account_get_balance_in_currency (const Account *account,
                                        const gnc_commodity *currency,
                                        gboolean recurse)
{
    return gnc_ui_account_get_balance_full (xaccAccountGetBalanceInCurrency,
                                            account, recurse, NULL, currency);
}
*/
/*
 * This routine retrieves the reconciled balance in an account,
 * possibly including all sub-accounts under the specified account.
 */
gnc_numeric
gnc_ui_account_get_reconciled_balance (const Account *account,
                                       gboolean recurse)
{
    return gnc_ui_account_get_balance_full (xaccAccountGetReconciledBalanceInCurrency,
                                            account, recurse, NULL, NULL);
}


/**
 * Wrapper around gnc_ui_account_get_balance_full that converts
 * the resulting number to a character string.  The number is
 * formatted according to the specification of the account currency.
 * The caller is responsible for g_free'ing the returned memory.
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
                                  gboolean *negative)
{
    GNCPrintAmountInfo print_info;
    gnc_numeric balance;

    balance = gnc_ui_account_get_balance_full(fn, account, recurse,
              negative, NULL);
    print_info = gnc_account_print_info(account, TRUE);
    return g_strdup(xaccPrintAmount(balance, print_info));
}


/**
 * Wrapper around gnc_ui_account_get_balance_full that converts
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
        gboolean *negative)
{
    GNCPrintAmountInfo print_info;
    gnc_numeric balance;
    gnc_commodity *report_commodity;

    report_commodity = gnc_default_report_currency();
    balance = gnc_ui_account_get_balance_full(fn, account, recurse,
              negative, report_commodity);
    print_info = gnc_commodity_print_info(report_commodity, TRUE);
    return g_strdup(xaccPrintAmount(balance, print_info));
}

static gnc_numeric
account_get_balance_as_of_date (Account *account,
                                time64 date,
                                gboolean include_children,
                                xaccGetBalanceAsOfDateFn fn)
{
    QofBook *book = gnc_account_get_book (account);
    GNCPriceDB *pdb = gnc_pricedb_get_db (book);
    gnc_numeric balance;
    gnc_commodity *currency;

    if (account == NULL)
        return gnc_numeric_zero ();

    currency = xaccAccountGetCommodity (account);
    balance = fn (account, date);

    if (include_children)
    {
        GList *children, *node;

        children = gnc_account_get_descendants(account);

        for (node = children; node; node = node->next)
        {
            Account *child;
            gnc_commodity *child_currency;
            gnc_numeric child_balance;

            child = node->data;
            child_currency = xaccAccountGetCommodity (child);
            child_balance = fn (child, date);
            child_balance =
                gnc_pricedb_convert_balance_latest_price (pdb, child_balance,
                                                          child_currency,
                                                          currency);
            balance = gnc_numeric_add_fixed (balance, child_balance);
        }

        g_list_free(children);
    }

    /* reverse sign if needed */
    if (gnc_reverse_balance (account))
        balance = gnc_numeric_neg (balance);

    return balance;
}

gnc_numeric
gnc_ui_account_get_balance_as_of_date (Account *account,
                                       time64 date,
                                       gboolean include_children)
{
    return account_get_balance_as_of_date (account, date, include_children,
                                           xaccAccountGetBalanceAsOfDate);
}

gnc_numeric
gnc_ui_account_get_reconciled_balance_as_of_date (Account *account,
                                                  time64 date,
                                                  gboolean include_children)
{
    return account_get_balance_as_of_date (account, date, include_children,
                                           xaccAccountGetReconciledBalanceAsOfDate);
}


/********************************************************************
 * Balance calculations related to owners
 ********************************************************************/

/*
 * This is a wrapper routine around an gncOwnerGetBalanceInCurrency
 * function that handles additional needs of the gui.
 *
 * @param owner     The owner to retrieve data about.
 * @param negative  An indication of whether or not the returned value
 *                  is negative.  This can be used by the caller to
 *                  easily decode whether or not to color the output.
 * @param commodity The commodity in which the account balance should
 *                  be returned. If NULL, the value will be returned in
 *                  the commodity of the owner. This is normally used
 *                  to specify a currency, which forces the conversion
 *                  of things like stock account values from share
 *                  values to an amount the requested currency.
 */
gnc_numeric
gnc_ui_owner_get_balance_full (GncOwner *owner,
                               gboolean *negative,
                               const gnc_commodity *commodity)
{
    gnc_numeric balance;

    if (!owner)
        return gnc_numeric_zero ();

    balance = gncOwnerGetBalanceInCurrency (owner, commodity);

    /* reverse sign if needed */
    if ((gncOwnerGetType (owner) != GNC_OWNER_CUSTOMER))
        balance = gnc_numeric_neg (balance);

    /* Record whether the balance is negative. */
    if (negative)
        *negative = gnc_numeric_negative_p (balance);

    return balance;
}


/**
 * Wrapper around gnc_ui_owner_get_balance_full that converts
 * the resulting number to a character string.  The number is
 * formatted according to the specification of the owner currency.
 * The caller is responsible for g_free'ing the returned memory.
 *
 * @param owner   The owner to retrieve data about.
 * @param negative  An indication of whether or not the returned value
 *                  is negative.  This can be used by the caller to
 *                  easily decode whether or not to color the output.
 */
gchar *
gnc_ui_owner_get_print_balance (GncOwner *owner,
                                gboolean *negative)
{
    gnc_numeric balance;
    GNCPrintAmountInfo print_info;

    balance = gnc_ui_owner_get_balance_full (owner, negative, NULL);
    print_info = gnc_commodity_print_info (gncOwnerGetCurrency (owner), TRUE);
    return g_strdup (xaccPrintAmount (balance, print_info));
}

/**
 * Wrapper around gnc_ui_owner_get_balance_full that converts
 * the resulting number to a character string.  The number is
 * formatted according to the specification of the default reporting
 * currency.
 *
 * @param account   The owner to retrieve data about.
 * @param negative  An indication of whether or not the returned value
 *                  is negative.  This can be used by the caller to
 *                  easily decode whether or not to color the output.
 */
gchar *
gnc_ui_owner_get_print_report_balance (GncOwner *owner,
                                       gboolean *negative)
{
    GNCPrintAmountInfo print_info;
    gnc_numeric balance;
    gnc_commodity *report_commodity;

    report_commodity = gnc_default_report_currency ();
    balance = gnc_ui_owner_get_balance_full (owner, negative,
              report_commodity);
    print_info = gnc_commodity_print_info (report_commodity, TRUE);
    return g_strdup (xaccPrintAmount (balance, print_info));
}
