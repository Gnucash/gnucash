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

#include <string>

#include "gnc-ui-balances.h"
#include "gnc-ui-util.h"

#include <glib.h>
#include <glib/gi18n.h>

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

    return g_strdup (gnc_print_amount_with_bidi_ltr_isolate (balance, print_info));
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

    return g_strdup (gnc_print_amount_with_bidi_ltr_isolate (balance, print_info));
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

            child = static_cast<Account*>(node->data);
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

// retrieve account's balance to compare against the limit.
// we use today's date because account may have future dated splits
static gnc_numeric
account_balance_for_limit (const Account *account)
{
    return gnc_ui_account_get_balance_as_of_date
        ((Account*)account, gnc_time64_get_day_end (gnc_time (NULL)),
         xaccAccountGetIncludeSubAccountBalances (account));
}

static gint
account_balance_limit_reached (const Account *account, gnc_numeric balance_limit)
{
    gnc_numeric balance = account_balance_for_limit (account);

    if (gnc_numeric_zero_p (balance))
        return 0;

    if (gnc_reverse_balance (account))
        balance_limit = gnc_numeric_neg (balance_limit);

    // Returns 1 if a>b, -1 if b>a, 0 if a == b
    return gnc_numeric_compare (balance, balance_limit);
}

static gboolean
get_limit_info (const Account *account, gnc_numeric *limit, gboolean higher)
{
    gboolean reverse = gnc_reverse_balance (account);
    if ((higher && reverse) || (!higher && !reverse))
        return xaccAccountGetLowerBalanceLimit (account, limit);
    else
        return xaccAccountGetHigherBalanceLimit (account, limit);
}

gboolean
gnc_ui_account_is_higher_balance_limit_reached (const Account *account,
                                                gboolean *is_zero)
{
    gnc_numeric balance_limit;
    gboolean limit_valid = FALSE;
    gboolean retval = FALSE;

    g_return_val_if_fail (GNC_IS_ACCOUNT(account), FALSE);

    limit_valid = get_limit_info (account, &balance_limit, TRUE);

    if (!limit_valid)
        return retval;

    if (gnc_numeric_zero_p (balance_limit))
        *is_zero = TRUE;

    if (account_balance_limit_reached (account, balance_limit) == 1)
        retval = TRUE;

    return retval;
}

gboolean
gnc_ui_account_is_lower_balance_limit_reached (const Account *account,
                                               gboolean *is_zero)
{
    gnc_numeric balance_limit;
    gboolean limit_valid = FALSE;
    gboolean retval = FALSE;

    g_return_val_if_fail (GNC_IS_ACCOUNT(account), FALSE);

    limit_valid = get_limit_info (account, &balance_limit, FALSE);

    if (!limit_valid)
        return retval;

    if (gnc_numeric_zero_p (balance_limit))
        *is_zero = TRUE;

    if (account_balance_limit_reached (account, balance_limit) == -1)
        retval = TRUE;

    return retval;
}

static gchar *
make_limit_explanation (const Account *account, const char* template_str,
                        gboolean zero, gboolean higher)
{
    gnc_commodity *currency = xaccAccountGetCommodity (account);
    GNCPrintAmountInfo pinfo = gnc_commodity_print_info (currency, TRUE);
    gnc_numeric acct_bal = account_balance_for_limit (account);
    char *fullname = gnc_account_get_full_name (account);
    char *bal_str = g_strdup (xaccPrintAmount (acct_bal, pinfo));
    char *rv;
    if (zero)
        rv = g_strdup_printf (_(template_str), fullname, bal_str);
    else
    {
        gnc_numeric limit;
        get_limit_info (account, &limit, higher);
        if (gnc_reverse_balance (account))
            limit = gnc_numeric_neg (limit);
        char *lim_str = g_strdup (xaccPrintAmount (limit, pinfo));
        rv = g_strdup_printf (_(template_str), fullname, bal_str, lim_str);
        g_free (lim_str);
    }
    g_free (bal_str);
    g_free (fullname);
    return rv;
}

static gchar *
get_balance_limit_info (const Account *account, gboolean icon)
{
    gboolean lower_limit_reached, higher_limit_reached;
    gboolean lower_is_zero = FALSE;
    gboolean higher_is_zero = FALSE;
    const char *higher_template = N_("%s balance of %s is above the upper limit %s.");
    const char *lower_template = N_("%s balance of %s is below the lower limit %s.");
    const char *zero_template = N_("%s balance of %s should be zero.");

    g_return_val_if_fail (GNC_IS_ACCOUNT(account), NULL);

    higher_limit_reached = gnc_ui_account_is_higher_balance_limit_reached (account, &higher_is_zero);

    // assume the higher value will be set mostly so test that first
    if (higher_limit_reached && !higher_is_zero)
        return icon ? g_strdup ("go-top") : make_limit_explanation (account, higher_template, FALSE, TRUE);

    lower_limit_reached = gnc_ui_account_is_lower_balance_limit_reached (account, &lower_is_zero);

    if (lower_limit_reached && (!lower_is_zero || !higher_is_zero))
        return icon ? g_strdup ("go-bottom") : make_limit_explanation (account, lower_template, FALSE, FALSE);

    if (higher_limit_reached && !lower_is_zero)
        return icon ? g_strdup ("go-top") : make_limit_explanation (account, higher_template, FALSE, TRUE);

    if ((lower_limit_reached || higher_limit_reached ) && lower_is_zero && higher_is_zero)
        return icon ? g_strdup ("dialog-warning") : make_limit_explanation (account, zero_template, TRUE, FALSE);

    return NULL;
}

gchar *
gnc_ui_account_get_balance_limit_icon_name (const Account *account)
{
    char *icon = get_balance_limit_info (account, TRUE);
    return icon ? icon : g_strdup ("");
}

gchar *
gnc_ui_account_get_balance_limit_explanation (const Account *account)
{
    return get_balance_limit_info (account, FALSE);
}

/********************************************************************
 * Reconciled balances
 ********************************************************************/

/* Assertions are stored with the engine's internal sign; the user
 * entered, and expects to read back, whatever the register shows. */
gnc_numeric
gnc_ui_reconciled_balance_get_display_amount (const GncReconciledBalance *ba)
{
    g_return_val_if_fail (GNC_IS_RECONCILED_BALANCE (ba), gnc_numeric_zero ());

    gnc_numeric amount = gnc_reconciled_balance_get_amount (ba);
    Account *acc = gnc_reconciled_balance_get_account (ba);

    return (acc && gnc_reverse_balance (acc)) ? gnc_numeric_neg (amount) : amount;
}

void
gnc_ui_reconciled_balance_set_display_amount (GncReconciledBalance *ba,
                                             gnc_numeric amount)
{
    g_return_if_fail (GNC_IS_RECONCILED_BALANCE (ba));

    Account *acc = gnc_reconciled_balance_get_account (ba);
    if (acc && gnc_reverse_balance (acc))
        amount = gnc_numeric_neg (amount);

    gnc_reconciled_balance_set_amount (ba, amount);
}

static gnc_numeric
record_display_actual (const GncReconciledBalance *ba)
{
    gnc_numeric actual = gnc_reconciled_balance_get_actual (ba);
    Account *acc = gnc_reconciled_balance_get_account (ba);

    return (acc && gnc_reverse_balance (acc)) ? gnc_numeric_neg (actual) : actual;
}

gchar *
gnc_ui_reconciled_balance_get_description (const GncReconciledBalance *ba)
{
    g_return_val_if_fail (GNC_IS_RECONCILED_BALANCE (ba), nullptr);

    Account *acc = gnc_reconciled_balance_get_account (ba);
    if (!acc)
        return g_strdup (_("This reconciled balance refers to an account that "
                           "no longer exists."));

    GNCPrintAmountInfo pinfo =
        gnc_commodity_print_info (xaccAccountGetCommodity (acc), TRUE);
    char datebuf[MAX_DATE_LENGTH + 1];
    qof_print_date_buff (datebuf, MAX_DATE_LENGTH,
                         gnc_reconciled_balance_get_date (ba));

    char *recorded = g_strdup (xaccPrintAmount
                               (gnc_ui_reconciled_balance_get_display_amount (ba),
                                pinfo));
    char *rv;

        if (gnc_reconciled_balance_get_status (ba) == GNC_RECONCILED_BALANCE_HOLDS)
        rv = g_strdup_printf (_("Balance on %s is still %s, as recorded."),
                              datebuf, recorded);
    else
    {
        char *actual = g_strdup (xaccPrintAmount (record_display_actual (ba),
                                                  pinfo));
        rv = g_strdup_printf (_("Balance on %s was recorded as %s but is now "
                                "%s: something dated on or before then has "
                                "changed since."), datebuf, recorded, actual);
        g_free (actual);
    }

    g_free (recorded);
    return rv;
}

guint
gnc_ui_account_reseal_reconciled_balances (Account *account)
{
    g_return_val_if_fail (GNC_IS_ACCOUNT (account), 0);

    auto records = gnc_reconciled_balance_get_for_account (account);
    GNCPrintAmountInfo pinfo =
        gnc_commodity_print_info (xaccAccountGetCommodity (account), TRUE);
    guint resealed = 0;

    for (auto n = records; n; n = n->next)
    {
        auto rb = GNC_RECONCILED_BALANCE (n->data);

        if (!gnc_reconciled_balance_is_broken (rb))
            continue;

        auto was = gnc_reconciled_balance_reseal (rb);
        ++resealed;

        /* Quietly: the number moves, and the figure it used to hold goes
         * into the notes rather than leaving a superseded row behind. */
        if (gnc_reverse_balance (account))
            was = gnc_numeric_neg (was);

        auto old_str = g_strdup (xaccPrintAmount (was, pinfo));
        auto notes = gnc_reconciled_balance_get_notes (rb);
        auto updated = g_strdup_printf (_("%s%swas %s"),
                                        notes ? notes : "",
                                        (notes && *notes) ? "; " : "",
                                        old_str);
        gnc_reconciled_balance_set_notes (rb, updated);
        g_free (updated);
        g_free (old_str);
    }

    g_list_free (records);
    return resealed;
}

gboolean
gnc_ui_account_broken_balances_share_delta (const Account *account,
                                            gnc_numeric *delta)
{
    g_return_val_if_fail (GNC_IS_ACCOUNT (account), FALSE);

    auto records = gnc_reconciled_balance_get_for_account (account);
    gnc_numeric shared = gnc_numeric_zero ();
    guint broken = 0;
    gboolean same = TRUE;

    for (auto n = records; n; n = n->next)
    {
        auto rb = GNC_RECONCILED_BALANCE (n->data);

        if (!gnc_reconciled_balance_is_broken (rb))
            continue;

        auto d = gnc_reconciled_balance_get_delta (rb);

        if (broken++ == 0)
            shared = d;
        else if (!gnc_numeric_equal (shared, d))
            same = FALSE;
    }

    g_list_free (records);

    if (broken < 2 || !same)
        return FALSE;

    if (delta)
        *delta = gnc_reverse_balance (account) ? gnc_numeric_neg (shared) : shared;

    return TRUE;
}

gchar *
gnc_ui_account_get_reconciled_balance_status_icon (const Account *account)
{
    g_return_val_if_fail (GNC_IS_ACCOUNT (account), g_strdup (""));

    GList *all = gnc_reconciled_balance_get_for_account (account);
    if (!all)
        return g_strdup ("");

    gboolean broken = FALSE;
    for (GList *n = all; n && !broken; n = n->next)
        broken = gnc_reconciled_balance_is_broken (GNC_RECONCILED_BALANCE (n->data));

    g_list_free (all);

    /* A broken record is worth a warning triangle; an intact one is
     * worth the reassurance of a tick, and nothing more. Neither
     * prevents the user from doing anything. */
    return g_strdup (broken ? "dialog-warning" : "emblem-default");
}

gchar *
gnc_ui_account_get_reconciled_balance_status_explanation (const Account *account)
{
    g_return_val_if_fail (GNC_IS_ACCOUNT (account), nullptr);

    auto all = gnc_reconciled_balance_get_for_account (account);
    if (!all)
        return nullptr;

    /* Newest first: the most recent statement is the one the user is
     * most likely to be looking for. */
    all = g_list_reverse (all);

    auto describe = [](GncReconciledBalance *ba)
    {
        auto line = gnc_ui_reconciled_balance_get_description (ba);
        std::string rv {line ? line : ""};
        g_free (line);
        return rv;
    };

    static constexpr unsigned max_listed = 3;
    std::string text;
    unsigned broken = 0;

    for (auto n = all; n; n = n->next)
    {
        auto ba = GNC_RECONCILED_BALANCE (n->data);

        if (!gnc_reconciled_balance_is_broken (ba))
            continue;

        if (broken++ == max_listed)
        {
            text += "\n…";
            break;
        }

        if (!text.empty())
            text += '\n';
        text += describe (ba);
    }

    /* Nothing is broken: say so for the most recent record, so the tick
     * in the account tree has something to explain it. */
    if (!broken)
        text = describe (GNC_RECONCILED_BALANCE (all->data));

    g_list_free (all);
    return g_strdup (text.c_str());
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

    return g_strdup (gnc_print_amount_with_bidi_ltr_isolate (balance, print_info));
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

    return g_strdup (gnc_print_amount_with_bidi_ltr_isolate (balance, print_info));
}

