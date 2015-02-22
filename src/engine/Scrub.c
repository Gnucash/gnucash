/********************************************************************\
 * Scrub.c -- convert single-entry accounts into clean double-entry *
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
 *                                                                  *
\********************************************************************/

/*
 * FILE:
 * Scrub.c
 *
 * FUNCTION:
 * Provides a set of functions and utilities for scrubbing clean
 * single-entry accounts so that they can be promoted into
 * self-consistent, clean double-entry accounts.
 *
 * HISTORY:
 * Created by Linas Vepstas December 1998
 * Copyright (c) 1998-2000, 2003 Linas Vepstas <linas@linas.org>
 * Copyright (c) 2002 Christian Stimming
 * Copyright (c) 2006 David Hampton
 */

#include "config.h"

#include <glib.h>
#include <glib/gi18n.h>
#include <stdio.h>
#include <string.h>

#include "Account.h"
#include "AccountP.h"
#include "Scrub.h"
#include "ScrubP.h"
#include "Transaction.h"
#include "TransactionP.h"
#include "gnc-commodity.h"

#undef G_LOG_DOMAIN
#define G_LOG_DOMAIN "gnc.engine.scrub"

static QofLogModule log_module = G_LOG_DOMAIN;

/* ================================================================ */

void
xaccAccountTreeScrubOrphans (Account *acc)
{
    if (!acc) return;

    xaccAccountScrubOrphans (acc);
    gnc_account_foreach_descendant(acc,
                                   (AccountCb)xaccAccountScrubOrphans, NULL);
}

static void
TransScrubOrphansFast (Transaction *trans, Account *root)
{
    GList *node;
    gchar *accname;

    if (!trans) return;
    g_return_if_fail (root);

    for (node = trans->splits; node; node = node->next)
    {
        Split *split = node->data;
        Account *orph;

        if (split->acc) continue;

        DEBUG ("Found an orphan \n");

        accname = g_strconcat (_("Orphan"), "-",
                               gnc_commodity_get_mnemonic (trans->common_currency),
                               NULL);
        orph = xaccScrubUtilityGetOrMakeAccount (root, trans->common_currency,
                accname, ACCT_TYPE_BANK, FALSE);
        g_free (accname);
        if (!orph) continue;

        xaccSplitSetAccount(split, orph);
    }
}

void
xaccAccountScrubOrphans (Account *acc)
{
    GList *node;
    const char *str;

    if (!acc) return;

    str = xaccAccountGetName (acc);
    str = str ? str : "(null)";
    PINFO ("Looking for orphans in account %s \n", str);

    for (node = xaccAccountGetSplitList(acc); node; node = node->next)
    {
        Split *split = node->data;

        TransScrubOrphansFast (xaccSplitGetParent (split),
                               gnc_account_get_root (acc));
    }
}


void
xaccTransScrubOrphans (Transaction *trans)
{
    SplitList *node;
    QofBook *book = NULL;
    Account *root = NULL;

    if (!trans) return;

    for (node = trans->splits; node; node = node->next)
    {
        Split *split = node->data;

        if (split->acc)
        {
            TransScrubOrphansFast (trans, gnc_account_get_root(split->acc));
            return;
        }
    }

    /* If we got to here, then *none* of the splits belonged to an
     * account.  Not a happy situation.  We should dig an account
     * out of the book the transaction belongs to.
     * XXX we should probably *always* to this, instead of the above loop!
     */
    PINFO ("Free Floating Transaction!");
    book = xaccTransGetBook (trans);
    root = gnc_book_get_root_account (book);
    TransScrubOrphansFast (trans, root);
}

/* ================================================================ */

void
xaccAccountTreeScrubSplits (Account *account)
{
    if (!account) return;

    xaccAccountScrubSplits (account);
    gnc_account_foreach_descendant(account,
                                   (AccountCb)xaccAccountScrubSplits, NULL);
}

void
xaccAccountScrubSplits (Account *account)
{
    GList *node;

    for (node = xaccAccountGetSplitList (account); node; node = node->next)
        xaccSplitScrub (node->data);
}

void
xaccSplitScrub (Split *split)
{
    Account *account;
    Transaction *trans;
    gnc_numeric value, amount;
    gnc_commodity *currency, *acc_commodity;
    int scu;

    if (!split) return;
    ENTER ("(split=%p)", split);

    trans = xaccSplitGetParent (split);
    if (!trans)
    {
        LEAVE("no trans");
        return;
    }

    account = xaccSplitGetAccount (split);

    /* If there's no account, this split is an orphan.
     * We need to fix that first, before proceeding.
     */
    if (!account)
    {
        xaccTransScrubOrphans (trans);
        account = xaccSplitGetAccount (split);
    }

    /* Grrr... the register gnc_split_register_load() line 203 of
     *  src/register/ledger-core/split-register-load.c will create
     * free-floating bogus transactions. Ignore these for now ...
     */
    if (!account)
    {
        PINFO ("Free Floating Transaction!");
        LEAVE ("no account");
        return;
    }

    /* Split amounts and values should be valid numbers */
    value = xaccSplitGetValue (split);
    if (gnc_numeric_check (value))
    {
        value = gnc_numeric_zero();
        xaccSplitSetValue (split, value);
    }

    amount = xaccSplitGetAmount (split);
    if (gnc_numeric_check (amount))
    {
        amount = gnc_numeric_zero();
        xaccSplitSetAmount (split, amount);
    }

    currency = xaccTransGetCurrency (trans);

    /* If the account doesn't have a commodity,
     * we should attempt to fix that first.
    */
    acc_commodity = xaccAccountGetCommodity(account);
    if (!acc_commodity)
    {
        xaccAccountScrubCommodity (account);
    }
    if (!acc_commodity || !gnc_commodity_equiv(acc_commodity, currency))
    {
        LEAVE ("(split=%p) inequiv currency", split);
        return;
    }

    scu = MIN (xaccAccountGetCommoditySCU (account),
               gnc_commodity_get_fraction (currency));

    if (gnc_numeric_same (amount, value, scu, GNC_HOW_RND_ROUND_HALF_UP))
    {
        LEAVE("(split=%p) different values", split);
        return;
    }

    /*
     * This will be hit every time you answer yes to the dialog "The
     * current transaction has changed. Would you like to record it.
     */
    PINFO ("Adjusted split with mismatched values, desc=\"%s\" memo=\"%s\""
           " old amount %s %s, new amount %s",
           trans->description, split->memo,
           gnc_num_dbg_to_string (xaccSplitGetAmount(split)),
           gnc_commodity_get_mnemonic (currency),
           gnc_num_dbg_to_string (xaccSplitGetValue(split)));

    xaccTransBeginEdit (trans);
    xaccSplitSetAmount (split, value);
    xaccTransCommitEdit (trans);
    LEAVE ("(split=%p)", split);
}

/* ================================================================ */

void
xaccAccountTreeScrubImbalance (Account *acc)
{
    xaccAccountScrubImbalance (acc);
    gnc_account_foreach_descendant(acc,
                                   (AccountCb)xaccAccountScrubImbalance, NULL);
}

void
xaccAccountScrubImbalance (Account *acc)
{
    GList *node, *splits;
    const char *str;
    gint split_count = 0, curr_split_no = 1;

    if (!acc) return;

    str = xaccAccountGetName(acc);
    str = str ? str : "(null)";
    PINFO ("Looking for imbalance in account %s \n", str);

    splits = xaccAccountGetSplitList(acc);
    split_count = g_list_length (splits);
    for (node = splits; node; node = node->next)
    {
        Split *split = node->data;
        Transaction *trans = xaccSplitGetParent(split);

        PINFO("Start processing split %d of %d",
              curr_split_no, split_count);

        xaccTransScrubCurrency(trans);

        xaccTransScrubImbalance (trans, gnc_account_get_root (acc), NULL);

        PINFO("Finished processing split %d of %d",
              curr_split_no, split_count);
        curr_split_no++;
    }
}

static Split *
get_balance_split (Transaction *trans, Account *root, Account *account,
                   gnc_commodity *commodity)
{
    Split *balance_split;
    gchar *accname;

    if (!account ||
            !gnc_commodity_equiv (commodity, xaccAccountGetCommodity(account)))
    {
        if (!root)
        {
            root = gnc_book_get_root_account (xaccTransGetBook (trans));
            if (NULL == root)
            {
                /* This can't occur, things should be in books */
                PERR ("Bad data corruption, no root account in book");
                return NULL;
            }
        }
        accname = g_strconcat (_("Imbalance"), "-",
                               gnc_commodity_get_mnemonic (commodity), NULL);
        account = xaccScrubUtilityGetOrMakeAccount (root, commodity,
                  accname, ACCT_TYPE_BANK, FALSE);
        g_free (accname);
        if (!account)
        {
            PERR ("Can't get balancing account");
            return NULL;
        }
    }

    balance_split = xaccTransFindSplitByAccount(trans, account);

    /* Put split into account before setting split value */
    if (!balance_split)
    {
        balance_split = xaccMallocSplit (qof_instance_get_book(trans));

        xaccTransBeginEdit (trans);
        xaccSplitSetParent(balance_split, trans);
        xaccSplitSetAccount(balance_split, account);
        xaccTransCommitEdit (trans);
    }

    return balance_split;
}

/* Get the trading split for a given commodity, creating it (and the
   necessary accounts) if it doesn't exist. */
static Split *
get_trading_split (Transaction *trans, Account *root,
                   gnc_commodity *commodity)
{
    Split *balance_split;
    Account *trading_account;
    Account *ns_account;
    Account *account;
    gnc_commodity *default_currency = NULL;

    if (!root)
    {
        root = gnc_book_get_root_account (xaccTransGetBook (trans));
        if (NULL == root)
        {
            /* This can't occur, things should be in books */
            PERR ("Bad data corruption, no root account in book");
            return NULL;
        }
    }

    /* Get the default currency.  This is harder than it seems.  It's not
       possible to call gnc_default_currency() since it's a UI function.  One
       might think that the currency of the root account would do, but the root
       account has no currency.  Instead look for the Income placeholder account
       and use its currency.  */
    default_currency = xaccAccountGetCommodity(gnc_account_lookup_by_name(root,
                       _("Income")));
    if (! default_currency)
    {
        default_currency = commodity;
    }

    trading_account = xaccScrubUtilityGetOrMakeAccount (root,
                      default_currency,
                      _("Trading"),
                      ACCT_TYPE_TRADING, TRUE);
    if (!trading_account)
    {
        PERR ("Can't get trading account");
        return NULL;
    }

    ns_account = xaccScrubUtilityGetOrMakeAccount (trading_account,
                 default_currency,
                 gnc_commodity_get_namespace(commodity),
                 ACCT_TYPE_TRADING, TRUE);
    if (!ns_account)
    {
        PERR ("Can't get namespace account");
        return NULL;
    }

    account = xaccScrubUtilityGetOrMakeAccount (ns_account, commodity,
              gnc_commodity_get_mnemonic(commodity),
              ACCT_TYPE_TRADING, FALSE);
    if (!account)
    {
        PERR ("Can't get commodity account");
        return NULL;
    }


    balance_split = xaccTransFindSplitByAccount(trans, account);

    /* Put split into account before setting split value */
    if (!balance_split)
    {
        balance_split = xaccMallocSplit (qof_instance_get_book(trans));

        xaccTransBeginEdit (trans);
        xaccSplitSetParent(balance_split, trans);
        xaccSplitSetAccount(balance_split, account);
        xaccTransCommitEdit (trans);
    }

    return balance_split;
}

/* Find the trading split for a commodity, but don't create any splits
   or accounts if they don't already exist. */
static Split *
find_trading_split (Transaction *trans, Account *root,
                    gnc_commodity *commodity)
{
    Account *trading_account;
    Account *ns_account;
    Account *account;

    if (!root)
    {
        root = gnc_book_get_root_account (xaccTransGetBook (trans));
        if (NULL == root)
        {
            /* This can't occur, things should be in books */
            PERR ("Bad data corruption, no root account in book");
            return NULL;
        }
    }

    trading_account = gnc_account_lookup_by_name (root, _("Trading"));
    if (!trading_account)
    {
        return NULL;
    }

    ns_account = gnc_account_lookup_by_name (trading_account,
                 gnc_commodity_get_namespace(commodity));
    if (!ns_account)
    {
        return NULL;
    }

    account = gnc_account_lookup_by_name (ns_account,
                                          gnc_commodity_get_mnemonic(commodity));
    if (!account)
    {
        return NULL;
    }

    return xaccTransFindSplitByAccount(trans, account);
}

static void
add_balance_split (Transaction *trans, gnc_numeric imbalance,
                   Account *root, Account *account)
{
    const gnc_commodity *commodity;
    gnc_numeric old_value, new_value;
    Split *balance_split;
    gnc_commodity *currency = xaccTransGetCurrency (trans);

    balance_split = get_balance_split(trans, root, account, currency);
    if (!balance_split)
    {
        /* Error already logged */
        LEAVE("");
        return;
    }
    account = xaccSplitGetAccount(balance_split);

    xaccTransBeginEdit (trans);

    old_value = xaccSplitGetValue (balance_split);

    /* Note: We have to round for the commodity's fraction, NOT any
      * already existing denominator (bug #104343), because either one
      * of the denominators might already be reduced.  */
    new_value = gnc_numeric_sub (old_value, imbalance,
                                 gnc_commodity_get_fraction(currency),
                                 GNC_HOW_RND_ROUND_HALF_UP);

    xaccSplitSetValue (balance_split, new_value);

    commodity = xaccAccountGetCommodity (account);
    if (gnc_commodity_equiv (currency, commodity))
    {
        xaccSplitSetAmount (balance_split, new_value);
    }

    xaccSplitScrub (balance_split);
    xaccTransCommitEdit (trans);
}

void
xaccTransScrubImbalance (Transaction *trans, Account *root,
                         Account *account)
{
    const gnc_commodity *currency;

    if (!trans) return;

    ENTER ("()");

    /* Must look for orphan splits even if there is no imbalance. */
    xaccTransScrubSplits (trans);

    /* Return immediately if things are balanced. */
    if (xaccTransIsBalanced (trans))
    {
        LEAVE ("transaction is balanced");
        return;
    }

    currency = xaccTransGetCurrency (trans);

    if (! xaccTransUseTradingAccounts (trans))
    {
        gnc_numeric imbalance;

        /* Make the value sum to zero */
        imbalance = xaccTransGetImbalanceValue (trans);
        if (! gnc_numeric_zero_p (imbalance))
        {
            PINFO ("Value unbalanced transaction");

            add_balance_split (trans, imbalance, root, account);
        }
    }
    else
    {
        MonetaryList *imbal_list;
        MonetaryList *imbalance_commod;
        GList *splits;
        gnc_numeric imbalance;
        Split *balance_split = NULL;

        /* If there are existing trading splits, adjust the price or exchange
           rate in each of them to agree with the non-trading splits for the
           same commodity.  If there are multiple non-trading splits for the
           same commodity in the transaction this will use the exchange rate in
           the last such split.  This shouldn't happen, and if it does then there's
           not much we can do about it anyway.

           While we're at it, compute the value imbalance ignoring existing
           trading splits. */

        imbalance = gnc_numeric_zero();

        for (splits = trans->splits; splits; splits = splits->next)
        {
            Split *split = splits->data;
            gnc_numeric value, amount;
            gnc_commodity *commodity;

            if (! xaccTransStillHasSplit (trans, split)) continue;

            commodity = xaccAccountGetCommodity (xaccSplitGetAccount(split));
            if (!commodity)
            {
                PERR("Split has no commodity");
                continue;
            }

            balance_split = find_trading_split (trans, root, commodity);

            if (balance_split != split)
                /* this is not a trading split */
                imbalance = gnc_numeric_add(imbalance, xaccSplitGetValue (split),
                                            GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT);

            /* Ignore splits where value or amount is zero */
            value = xaccSplitGetValue (split);
            amount = xaccSplitGetAmount (split);
            if (gnc_numeric_zero_p(amount) || gnc_numeric_zero_p(value))
                continue;

            if (balance_split && balance_split != split)
            {
                gnc_numeric convrate = gnc_numeric_div (amount, value,
                                                        GNC_DENOM_AUTO, GNC_HOW_DENOM_REDUCE);
                gnc_numeric old_value, new_value;
                old_value = xaccSplitGetValue(balance_split);
                new_value = gnc_numeric_div (xaccSplitGetAmount(balance_split),
                                             convrate,
                                             gnc_commodity_get_fraction(currency),
                                             GNC_HOW_RND_ROUND_HALF_UP);
                if (! gnc_numeric_equal (old_value, new_value))
                {
                    xaccTransBeginEdit (trans);
                    xaccSplitSetValue (balance_split, new_value);
                    xaccSplitScrub (balance_split);
                    xaccTransCommitEdit (trans);
                }
            }
        }

        /* Balance the value, ignoring existing trading splits */
        if (! gnc_numeric_zero_p (imbalance))
        {
            PINFO ("Value unbalanced transaction");

            add_balance_split (trans, imbalance, root, account);
        }

        /* If the transaction is balanced, nothing more to do */
        imbal_list = xaccTransGetImbalance (trans);
        if (!imbal_list)
        {
            LEAVE("transaction is balanced");
            return;
        }

        PINFO ("Currency unbalanced transaction");

        for (imbalance_commod = imbal_list; imbalance_commod;
                imbalance_commod = imbalance_commod->next)
        {
            gnc_monetary *imbal_mon = imbalance_commod->data;
            gnc_commodity *commodity;
            gnc_numeric old_amount, new_amount;
            gnc_numeric old_value, new_value, val_imbalance;
            GList *splits;

            commodity = gnc_monetary_commodity (*imbal_mon);

            balance_split = get_trading_split(trans, root, commodity);
            if (!balance_split)
            {
                /* Error already logged */
                gnc_monetary_list_free(imbal_list);
                LEAVE("");
                return;
            }

            account = xaccSplitGetAccount(balance_split);

            if (! gnc_commodity_equal (currency, commodity))
            {
                /* Find the value imbalance in this commodity */
                val_imbalance = gnc_numeric_zero();
                for (splits = trans->splits; splits; splits = splits->next)
                {
                    Split *split = splits->data;
                    if (xaccTransStillHasSplit (trans, split) &&
                            gnc_commodity_equal (commodity,
                                                 xaccAccountGetCommodity(xaccSplitGetAccount(split))))
                        val_imbalance = gnc_numeric_add (val_imbalance, xaccSplitGetValue (split),
                                                         GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT);
                }
            }

            xaccTransBeginEdit (trans);

            old_amount = xaccSplitGetAmount (balance_split);
            new_amount = gnc_numeric_sub (old_amount, gnc_monetary_value(*imbal_mon),
                                          gnc_commodity_get_fraction(commodity),
                                          GNC_HOW_RND_ROUND_HALF_UP);

            xaccSplitSetAmount (balance_split, new_amount);

            if (gnc_commodity_equal (currency, commodity))
            {
                /* Imbalance commodity is the transaction currency, value in the
                   split must be the same as the amount */
                xaccSplitSetValue (balance_split, new_amount);
            }
            else
            {
                old_value = xaccSplitGetValue (balance_split);
                new_value = gnc_numeric_sub (old_value, val_imbalance,
                                             gnc_commodity_get_fraction(currency),
                                             GNC_HOW_RND_ROUND_HALF_UP);

                xaccSplitSetValue (balance_split, new_value);
            }

            xaccSplitScrub (balance_split);
            xaccTransCommitEdit (trans);
        }

        gnc_monetary_list_free(imbal_list);

        if (!gnc_numeric_zero_p(xaccTransGetImbalanceValue(trans)))
        {
            /* This is probably because there are splits with zero amount
               and non-zero value.  These are usually realized gain/loss
               splits.  Add a reversing split for each of them to balance
               the value. */

            /* Copy the split list so we don't see the splits we're adding */
            GList *splits_dup = g_list_copy(trans->splits);
            for (splits = splits_dup; splits; splits = splits->next)
            {
                Split *split = splits->data;
                if (! xaccTransStillHasSplit(trans, split)) continue;
                if (!gnc_numeric_zero_p(xaccSplitGetValue(split)) &&
                        gnc_numeric_zero_p(xaccSplitGetAmount(split)))
                {
                    gnc_commodity *commodity;
                    gnc_numeric old_value, new_value;

                    commodity = xaccAccountGetCommodity(xaccSplitGetAccount(split));
                    if (!commodity)
                    {
                        PERR("Split has no commodity");
                        continue;
                    }
                    balance_split = get_trading_split(trans, root, commodity);
                    if (!balance_split)
                    {
                        /* Error already logged */
                        gnc_monetary_list_free(imbal_list);
                        LEAVE("");
                        return;
                    }
                    account = xaccSplitGetAccount(balance_split);

                    xaccTransBeginEdit (trans);

                    old_value = xaccSplitGetValue (balance_split);
                    new_value = gnc_numeric_sub (old_value, xaccSplitGetValue(split),
                                                 gnc_commodity_get_fraction(currency),
                                                 GNC_HOW_RND_ROUND_HALF_UP);
                    xaccSplitSetValue (balance_split, new_value);

                    /* Don't change the balance split's amount since the amount
                       is zero in the split we're working on */

                    xaccSplitScrub (balance_split);
                    xaccTransCommitEdit (trans);
                }
            }

            g_list_free(splits_dup);

            if (!gnc_numeric_zero_p(xaccTransGetImbalanceValue(trans)))
                PERR("Balancing currencies unbalanced value");
        }
    }
    LEAVE ("()");
}

/* ================================================================ */
/* The xaccTransFindCommonCurrency () method returns
 *    a gnc_commodity indicating a currency denomination that all
 *    of the splits in this transaction have in common, using the
 *    old/obsolete currency/security fields of the split accounts.
 */

static gnc_commodity *
FindCommonExclSCurrency (SplitList *splits,
                         gnc_commodity * ra, gnc_commodity * rb,
                         Split *excl_split)
{
    GList *node;

    if (!splits) return NULL;

    for (node = splits; node; node = node->next)
    {
        Split *s = node->data;
        gnc_commodity * sa, * sb;

        if (s == excl_split) continue;

        g_return_val_if_fail (s->acc, NULL);

        sa = DxaccAccountGetCurrency (s->acc);
        sb = xaccAccountGetCommodity (s->acc);

        if (ra && rb)
        {
            int aa = !gnc_commodity_equiv(ra, sa);
            int ab = !gnc_commodity_equiv(ra, sb);
            int ba = !gnc_commodity_equiv(rb, sa);
            int bb = !gnc_commodity_equiv(rb, sb);

            if ( (!aa) && bb) rb = NULL;
            else if ( (!ab) && ba) rb = NULL;
            else if ( (!ba) && ab) ra = NULL;
            else if ( (!bb) && aa) ra = NULL;
            else if ( aa && bb && ab && ba )
            {
                ra = NULL;
                rb = NULL;
            }

            if (!ra)
            {
                ra = rb;
                rb = NULL;
            }
        }
        else if (ra && !rb)
        {
            int aa = !gnc_commodity_equiv(ra, sa);
            int ab = !gnc_commodity_equiv(ra, sb);
            if ( aa && ab ) ra = NULL;
        }
        else if (!ra && rb)
        {
            int aa = !gnc_commodity_equiv(rb, sa);
            int ab = !gnc_commodity_equiv(rb, sb);
            ra = ( aa && ab ) ? NULL : rb;
        }

        if ((!ra) && (!rb)) return NULL;
    }

    return (ra);
}

/* This is the wrapper for those calls (i.e. the older ones) which
 * don't exclude one split from the splitlist when looking for a
 * common currency.
 */
static gnc_commodity *
FindCommonCurrency (GList *splits, gnc_commodity * ra, gnc_commodity * rb)
{
    return FindCommonExclSCurrency(splits, ra, rb, NULL);
}

static gnc_commodity *
xaccTransFindOldCommonCurrency (Transaction *trans, QofBook *book)
{
    gnc_commodity *ra, *rb, *retval;
    Split *split;

    if (!trans) return NULL;

    if (trans->splits == NULL) return NULL;

    g_return_val_if_fail (book, NULL);

    split = trans->splits->data;

    if (!split || NULL == split->acc) return NULL;

    ra = DxaccAccountGetCurrency (split->acc);
    rb = xaccAccountGetCommodity (split->acc);

    retval = FindCommonCurrency (trans->splits, ra, rb);

    if (retval && !gnc_commodity_is_currency(retval))
        retval = NULL;
        
    return retval;
}

/* Test the currency of the splits and find the most common and return
 * it, or NULL if there is no currency more common than the
 * others -- or none at all.
 */
typedef struct
{
    gnc_commodity *commodity;
    unsigned int count;
} CommodityCount;

static gint
commodity_equal (gconstpointer a, gconstpointer b)
{
    CommodityCount *cc = (CommodityCount*)a;
    gnc_commodity *com = (gnc_commodity*)b;
    if ( cc == NULL || cc->commodity == NULL ||
            !GNC_IS_COMMODITY( cc->commodity ) ) return -1;
    if ( com == NULL || !GNC_IS_COMMODITY( com ) ) return 1;
    if ( gnc_commodity_equal(cc->commodity, com) )
        return 0;
    return 1;
}

static gint
commodity_compare( gconstpointer a, gconstpointer b)
{
    CommodityCount *ca = (CommodityCount*)a, *cb = (CommodityCount*)b;
    if (ca == NULL || ca->commodity == NULL ||
            !GNC_IS_COMMODITY( ca->commodity ) )
    {
        if (cb == NULL || cb->commodity == NULL ||
                !GNC_IS_COMMODITY( cb->commodity ) )
            return 0;
        return -1;
    }
    if (cb == NULL || cb->commodity == NULL ||
            !GNC_IS_COMMODITY( cb->commodity ) )
        return 1;
    if (ca->count == cb->count)
        return 0;
    return ca->count > cb->count ? 1 : -1;
}

/* Find the commodities in the account of each of the splits of a
 * transaction, and rank them by how many splits in which they
 * occur. Commodities which are currencies count more than those which
 * aren't, because for simple buy and sell transactions it makes
 * slightly more sense for the transaction commodity to be the
 * currency -- to the extent that it makes sense for a transaction to
 * have a currency at all. jralls, 2010-11-02 */

static gnc_commodity *
xaccTransFindCommonCurrency (Transaction *trans, QofBook *book)
{
    gnc_commodity *com_scratch;
    GList *node = NULL;
    GSList *comlist = NULL, *found = NULL;

    if (!trans) return NULL;

    if (trans->splits == NULL) return NULL;

    g_return_val_if_fail (book, NULL);

    /* Find the most commonly used currency among the splits.  If a given split
       is in a non-currency commodity, then look for an ancestor account in a 
       currency, but prefer currencies used directly in splits.  Ignore trading
       account splits in this whole process, they don't add any value to this algorithm. */
    for (node = trans->splits; node; node = node->next)
    {
        Split *s = node->data;
        unsigned int curr_weight;
        
        if (s == NULL || s->acc == NULL) continue;
        if (xaccAccountGetType(s->acc) == ACCT_TYPE_TRADING) continue;
        com_scratch = xaccAccountGetCommodity(s->acc);
        if (com_scratch && gnc_commodity_is_currency(com_scratch))
        {
            curr_weight = 3;
        }
        else
        {
            com_scratch = gnc_account_get_currency_or_parent(s->acc);
            if (com_scratch == NULL) continue;
            curr_weight = 1;
        }
        if ( comlist )
        {
            found = g_slist_find_custom(comlist, com_scratch, commodity_equal);
        }
        if (comlist == NULL || found == NULL)
        {
            CommodityCount *count = g_slice_new0(CommodityCount);
            count->commodity = com_scratch;
            count->count = curr_weight;
            comlist = g_slist_append(comlist, count);
        }
        else
        {
            CommodityCount *count = (CommodityCount*)(found->data);
            count->count += curr_weight;
        }
    }
    found = g_slist_sort( comlist, commodity_compare);

    if ( found && found->data && (((CommodityCount*)(found->data))->commodity != NULL))
    {
        return ((CommodityCount*)(found->data))->commodity;
    }
    /* We didn't find a currency in the current account structure, so try
     * an old one. */
    return xaccTransFindOldCommonCurrency( trans, book );
}

/* ================================================================ */

void
xaccTransScrubCurrency (Transaction *trans)
{
    SplitList *node;
    gnc_commodity *currency;

    if (!trans) return;

    /* If there are any orphaned splits in a transaction, then the
     * this routine will fail.  Therefore, we want to make sure that
     * there are no orphans (splits without parent account).
     */
    xaccTransScrubOrphans (trans);

    currency = xaccTransGetCurrency (trans);
    if (currency && gnc_commodity_is_currency(currency)) return;

    currency = xaccTransFindCommonCurrency (trans, qof_instance_get_book(trans));
    if (currency)
    {
        xaccTransBeginEdit (trans);
        xaccTransSetCurrency (trans, currency);
        xaccTransCommitEdit (trans);
    }
    else
    {
        if (NULL == trans->splits)
        {
            PWARN ("Transaction \"%s\" has no splits in it!", trans->description);
        }
        else
        {
            SplitList *node;
            char guid_str[GUID_ENCODING_LENGTH + 1];
            guid_to_string_buff(xaccTransGetGUID(trans), guid_str);
            PWARN ("no common transaction currency found for trans=\"%s\" (%s);",
                   trans->description, guid_str);

            for (node = trans->splits; node; node = node->next)
            {
                Split *split = node->data;
                if (NULL == split->acc)
                {
                    PWARN (" split=\"%s\" is not in any account!", split->memo);
                }
                else
                {
		    gnc_commodity *currency = xaccAccountGetCommodity(split->acc);
                    PWARN ("setting to split=\"%s\" account=\"%s\" commodity=\"%s\"",
                           split->memo, xaccAccountGetName(split->acc),
                           gnc_commodity_get_mnemonic(currency));

		    xaccTransBeginEdit (trans);
		    xaccTransSetCurrency (trans, currency);
		    xaccTransCommitEdit (trans);
		    return;
                }
            }
        }
        return;
    }

    for (node = trans->splits; node; node = node->next)
    {
        Split *sp = node->data;

        if (!gnc_numeric_equal(xaccSplitGetAmount (sp),
                               xaccSplitGetValue (sp)))
        {
            gnc_commodity *acc_currency;

            acc_currency = sp->acc ? xaccAccountGetCommodity(sp->acc) : NULL;
            if (acc_currency == currency)
            {
                /* This Split needs fixing: The transaction-currency equals
                 * the account-currency/commodity, but the amount/values are
                 * inequal i.e. they still correspond to the security
                 * (amount) and the currency (value). In the new model, the
                 * value is the amount in the account-commodity -- so it
                 * needs to be set to equal the amount (since the
                 * account-currency doesn't exist anymore).
                 *
                 * Note: Nevertheless we lose some information here. Namely,
                 * the information that the 'amount' in 'account-old-security'
                 * was worth 'value' in 'account-old-currency'. Maybe it would
                 * be better to store that information in the price database?
                 * But then, for old currency transactions there is still the
                 * 'other' transaction, which is going to keep that
                 * information. So I don't bother with that here. -- cstim,
                 * 2002/11/20. */

                PWARN ("Adjusted split with mismatched values, desc=\"%s\" memo=\"%s\""
                       " old amount %s %s, new amount %s",
                       trans->description, sp->memo,
                       gnc_num_dbg_to_string (xaccSplitGetAmount(sp)),
                       gnc_commodity_get_mnemonic (currency),
                       gnc_num_dbg_to_string (xaccSplitGetValue(sp)));
                xaccTransBeginEdit (trans);
                xaccSplitSetAmount (sp, xaccSplitGetValue(sp));
                xaccTransCommitEdit (trans);
            }
            /*else
            {
              PINFO ("Ok: Split '%s' Amount %s %s, value %s %s",
              xaccSplitGetMemo (sp),
              gnc_num_dbg_to_string (amount),
              gnc_commodity_get_mnemonic (currency),
              gnc_num_dbg_to_string (value),
              gnc_commodity_get_mnemonic (acc_currency));
            }*/
        }
    }

}

/* ================================================================ */

void
xaccAccountScrubCommodity (Account *account)
{
    gnc_commodity *commodity;

    if (!account) return;
    if (xaccAccountGetType(account) == ACCT_TYPE_ROOT) return;

    commodity = xaccAccountGetCommodity (account);
    if (commodity) return;

    /* Use the 'obsolete' routines to try to figure out what the
     * account commodity should have been. */
    commodity = xaccAccountGetCommodity (account);
    if (commodity)
    {
        xaccAccountSetCommodity (account, commodity);
        return;
    }

    commodity = DxaccAccountGetCurrency (account);
    if (commodity)
    {
        xaccAccountSetCommodity (account, commodity);
        return;
    }

    PERR ("Account \"%s\" does not have a commodity!",
          xaccAccountGetName(account));
}

/* ================================================================ */

/* EFFECTIVE FRIEND FUNCTION declared in qofinstance-p.h */
extern void qof_instance_set_dirty (QofInstance*);

static void
xaccAccountDeleteOldData (Account *account)
{
    if (!account) return;
    xaccAccountBeginEdit (account);

    kvp_frame_set_slot_nc (account->inst.kvp_data, "old-currency", NULL);
    kvp_frame_set_slot_nc (account->inst.kvp_data, "old-security", NULL);
    kvp_frame_set_slot_nc (account->inst.kvp_data, "old-currency-scu", NULL);
    kvp_frame_set_slot_nc (account->inst.kvp_data, "old-security-scu", NULL);
    qof_instance_set_dirty (QOF_INSTANCE (account));
    xaccAccountCommitEdit (account);
}

static int
scrub_trans_currency_helper (Transaction *t, gpointer data)
{
    xaccTransScrubCurrency (t);
    return 0;
}

static void
scrub_account_commodity_helper (Account *account, gpointer data)
{
    xaccAccountScrubCommodity (account);
    xaccAccountDeleteOldData (account);
}

void
xaccAccountTreeScrubCommodities (Account *acc)
{
    if (!acc) return;

    xaccAccountTreeForEachTransaction (acc, scrub_trans_currency_helper, NULL);

    scrub_account_commodity_helper (acc, NULL);
    gnc_account_foreach_descendant (acc, scrub_account_commodity_helper, NULL);
}

/* ================================================================ */

static gboolean
check_quote_source (gnc_commodity *com, gpointer data)
{
    gboolean *commodity_has_quote_src = (gboolean *)data;
    if (com && !gnc_commodity_is_iso(com))
        *commodity_has_quote_src |= gnc_commodity_get_quote_flag(com);
    return TRUE;
}

static void
move_quote_source (Account *account, gpointer data)
{
    gnc_commodity *com;
    gnc_quote_source *quote_source;
    gboolean new_style = GPOINTER_TO_INT(data);
    const char *source, *tz;

    com = xaccAccountGetCommodity(account);
    if (!com)
        return;

    if (!new_style)
    {
        source = dxaccAccountGetPriceSrc(account);
        if (!source || !*source)
            return;
        tz = dxaccAccountGetQuoteTZ(account);

        PINFO("to %8s from %s", gnc_commodity_get_mnemonic(com),
              xaccAccountGetName(account));
        gnc_commodity_set_quote_flag(com, TRUE);
        quote_source = gnc_quote_source_lookup_by_internal(source);
        if (!quote_source)
            quote_source = gnc_quote_source_add_new(source, FALSE);
        gnc_commodity_set_quote_source(com, quote_source);
        gnc_commodity_set_quote_tz(com, tz);
    }

    dxaccAccountSetPriceSrc(account, NULL);
    dxaccAccountSetQuoteTZ(account, NULL);
    return;
}


void
xaccAccountTreeScrubQuoteSources (Account *root, gnc_commodity_table *table)
{
    gboolean new_style = FALSE;
    ENTER(" ");

    if (!root || !table)
    {
        LEAVE("Oops");
        return;
    }

    gnc_commodity_table_foreach_commodity (table, check_quote_source, &new_style);

    move_quote_source(root, GINT_TO_POINTER(new_style));
    gnc_account_foreach_descendant (root, move_quote_source,
                                    GINT_TO_POINTER(new_style));
    LEAVE("Migration done");
}

/* ================================================================ */

void
xaccAccountScrubKvp (Account *account)
{
    const gchar *str;
    gchar *str2;
    KvpFrame *frame;

    if (!account) return;

    str = kvp_frame_get_string(account->inst.kvp_data, "notes");
    if (str)
    {
        str2 = g_strstrip(g_strdup(str));
        if (strlen(str2) == 0)
            kvp_frame_set_slot_nc (account->inst.kvp_data, "notes", NULL);
        g_free(str2);
    }

    str = kvp_frame_get_string(account->inst.kvp_data, "placeholder");
    if (str && strcmp(str, "false") == 0)
        kvp_frame_set_slot_nc (account->inst.kvp_data, "placeholder", NULL);

    frame = kvp_frame_get_frame(account->inst.kvp_data, "hbci");
    if (frame && kvp_frame_is_empty(frame))
    {
        kvp_frame_set_frame_nc(account->inst.kvp_data, "hbci", NULL);
    }
}

/* ================================================================ */

Account *
xaccScrubUtilityGetOrMakeAccount (Account *root, gnc_commodity * currency,
                                  const char *accname, GNCAccountType acctype,
                                  gboolean placeholder)
{
    Account * acc;

    g_return_val_if_fail (root, NULL);

    /* build the account name */
    if (!currency)
    {
        PERR ("No currency specified!");
        return NULL;
    }

    /* See if we've got one of these going already ... */
    acc = gnc_account_lookup_by_name(root, accname);

    if (acc == NULL)
    {
        /* Guess not. We'll have to build one. */
        acc = xaccMallocAccount(gnc_account_get_book (root));
        xaccAccountBeginEdit (acc);
        xaccAccountSetName (acc, accname);
        xaccAccountSetCommodity (acc, currency);
        xaccAccountSetType (acc, acctype);
        xaccAccountSetPlaceholder (acc, placeholder);

        /* Hang the account off the root. */
        gnc_account_append_child (root, acc);
        xaccAccountCommitEdit (acc);
    }

    return acc;
}

/* ==================== END OF FILE ==================== */
