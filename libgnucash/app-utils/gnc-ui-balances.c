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
#include "Split.h"
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


/* the following functions are used in window-autoclear: */

#define MAXIMUM_SACK_SIZE 1000000

static gboolean
ght_gnc_numeric_equal(gconstpointer v1, gconstpointer v2)
{
    gnc_numeric n1 = *(gnc_numeric *)v1, n2 = *(gnc_numeric *)v2;
    return gnc_numeric_equal(n1, n2);
}

static guint
ght_gnc_numeric_hash(gconstpointer v1)
{
    gnc_numeric n1 = *(gnc_numeric *)v1;
    gdouble d1 = gnc_numeric_to_double(n1);
    return g_double_hash (&d1);
}

typedef struct _sack_foreach_data_t
{
    gnc_numeric split_value;
    GList *reachable_list;
} *sack_foreach_data_t;

static void sack_foreach_func(gpointer key, gpointer value, gpointer user_data)
{
    sack_foreach_data_t data = (sack_foreach_data_t) user_data;
    gnc_numeric thisvalue = *(gnc_numeric *) key;
    gnc_numeric reachable_value = gnc_numeric_add_fixed (thisvalue, data->split_value);

    data->reachable_list = g_list_prepend
        (data->reachable_list, g_memdup (&reachable_value, sizeof (gnc_numeric)));
}

GList *
gnc_account_get_autoclear_splits (Account *account, gnc_numeric toclear_value,
                                  gchar **errmsg)
{
    GList *nc_list = NULL, *toclear_list = NULL;
    GHashTable *sack;
    gchar *msg = NULL;
    guint sack_size = 0;

    g_return_val_if_fail (GNC_IS_ACCOUNT (account), NULL);

    sack = g_hash_table_new_full (ght_gnc_numeric_hash, ght_gnc_numeric_equal,
                                  g_free, NULL);

    /* Extract which splits are not cleared and compute the amount we have to clear */
    for (GList *node = xaccAccountGetSplitList (account); node; node = node->next)
    {
        Split *split = (Split *)node->data;

        if (xaccSplitGetReconcile (split) == NREC)
            nc_list = g_list_prepend (nc_list, split);
        else
            toclear_value = gnc_numeric_sub_fixed
                (toclear_value, xaccSplitGetAmount (split));
    }

    if (gnc_numeric_zero_p (toclear_value))
    {
        msg = _("Account is already at Auto-Clear Balance.");
        goto skip_knapsack;
    }

    /* Run knapsack */
    /* Entries in the hash table are:
     *  - key   = amount to which we know how to clear (freed by GHashTable)
     *  - value = last split we used to clear this amount (not managed by GHashTable)
     */
    for (GList *node = nc_list; node; node = node->next)
    {
        Split *split = (Split *)node->data;
        gnc_numeric split_value = xaccSplitGetAmount (split);

        struct _sack_foreach_data_t s_data[1];
        s_data->split_value = split_value;
        s_data->reachable_list = NULL;

        /* For each value in the sack, compute a new reachable value */
        g_hash_table_foreach (sack, sack_foreach_func, s_data);

        /* Add the value of the split itself to the reachable_list */
        s_data->reachable_list = g_list_prepend
            (s_data->reachable_list, g_memdup (&split_value, sizeof (gnc_numeric)));

        /* Add everything to the sack, looking out for duplicates */
        for (GList *s_node = s_data->reachable_list; s_node; s_node = s_node->next)
        {
            gnc_numeric *reachable_value = s_node->data;

            /* Check if it already exists */
            if (g_hash_table_lookup_extended (sack, reachable_value, NULL, NULL))
            {
                /* If yes, we are in trouble, we reached an amount
                   using two solutions */
                g_hash_table_insert (sack, reachable_value, NULL);
            }
            else
            {
                g_hash_table_insert (sack, reachable_value, split);
                sack_size++;

                if (sack_size > MAXIMUM_SACK_SIZE)
                {
                    msg = _("Too many uncleared splits");
                    goto skip_knapsack;
                }
            }
        }
        g_list_free (s_data->reachable_list);
    }

    /* Check solution */
    while (!gnc_numeric_zero_p (toclear_value))
    {
        Split *split = NULL;

        if (!g_hash_table_lookup_extended (sack, &toclear_value,
                                           NULL, (gpointer) &split))
        {
            msg = _("The selected amount cannot be cleared.");
            goto skip_knapsack;
        }

        if (!split)
        {
            msg = _("Cannot uniquely clear splits. Found multiple possibilities.");
            goto skip_knapsack;
        }

        toclear_list = g_list_prepend (toclear_list, split);
        toclear_value = gnc_numeric_sub_fixed (toclear_value,
                                               xaccSplitGetAmount (split));
    }

 skip_knapsack:
    g_hash_table_destroy (sack);
    g_list_free (nc_list);

    if (msg)
    {
        *errmsg = g_strdup (msg);
        g_list_free (toclear_list);
        return NULL;
    }

    *errmsg = NULL;
    return toclear_list;
}
