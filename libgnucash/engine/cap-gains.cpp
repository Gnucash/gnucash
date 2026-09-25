/********************************************************************\
 * cap-gains.c -- Automatically Compute Capital Gains/Losses        *
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

/** @file cap-gains.c
 *  @brief Utilities to Automatically Compute Capital Gains/Losses.
 *  @author Created by Linas Vepstas August 2003
 *  @author Copyright (c) 2003,2004 Linas Vepstas <linas@linas.org>
 *
 *  This file implements the various routines to automatically
 *  compute and handle Cap Gains/Losses resulting from trading
 *  activities.  Some of these routines might have broader
 *  applicability, for handling depreciation & etc.
 *
 *  This code is under development, and is 'beta': we think we're
 *  mostly done, and we've tested and "things work for us", but there
 *  may still be something missing, and there might still be some
 *  bugs.
 *
 * This code uses a 'gains dirty' flag: A 'dirty' flag on the source
 * split indicates that the gains transaction needs to be recomputed.
 * Another flag, the gains transaction flag, marks the split as
 * being a gains split, and that the source transaction should be
 * checked for dirtiness before returning the date, the amount, the
 * value, etc.  Finally, these flags make amount and value read-only
 * for the gains splits. (the memo is user-modifieable).
 *
 * If the amount in a split is changed, then the lot has to be recomputed.
 * This has a potential trickle-through effect on all later lots.
 * Ideally, later lots are dissolved, and recomputed.  However, some
 * lots may have been user-hand-built. These should be left alone.
 *
ToDo:
 o XXX Need to create a data-integrity scrubber, that makes sure that
   the various flags, and pointers & etc. match. See sections marked
   with XXX below for things that might go wrong.
 */

#include <config.h>

#include <glib.h>
#include <glib/gi18n.h>

#include <deque>

#include "Account.hpp"
#include "AccountP.hpp"
#include "Scrub.h"
#include "Scrub2.h"
#include "Scrub3.h"
#include "Transaction.h"
#include "TransactionP.hpp"
#include "SplitP.hpp"
#include "cap-gains.h"
#include "gnc-engine.h"
#include "engine-helpers.h"
#include "gnc-lot.h"
#include "policy.h"
#include "policy-p.h"

static QofLogModule log_module = GNC_MOD_LOT;


enum class CapPlanPhase
{
    SCAN_LOT,
    EVALUATE,
    RESOLVE_GAIN_ACCOUNT,
    APPLY,
};

struct GncCapGainsPlan
{
    GncScrubContext *context;
    QofBook *book;
    GncGUID split_guid;
    GncGUID lot_guid;
    GncGUID gain_account_guid;
    guint64 lot_generation;
    GList *next;
    GncGUID earliest_guid;
    gboolean has_earliest;
    gboolean earliest_dirty;
    gnc_numeric balance_before_amount;
    gnc_numeric balance_before_value;
    gnc_numeric gain_value;
    CapPlanPhase phase;
    GncCapGainsPlanState state;
    gchar *orphan_name;
    std::deque<GncGUID> bfs_current;
    std::deque<GncGUID> bfs_next;
    GncGUID bfs_parent;
    guint64 bfs_generation;
    size_t bfs_index;
    gboolean bfs_enumerating;
};

static gboolean
cap_plan_valid (GncCapGainsPlan *plan)
{
    if (!plan || plan->state != GNC_CAP_GAINS_PLAN_RUNNING ||
        gnc_scrub_context_is_cancelled (plan->context) ||
        !gnc_scrub_context_owns_book (plan->context, plan->book))
        return FALSE;
    auto lot = gnc_lot_lookup (&plan->lot_guid, plan->book);
    return lot && gnc_lot_get_scrub_generation (lot) == plan->lot_generation;
}

static void
cap_plan_fail_validation (GncCapGainsPlan *plan)
{
    if (!plan || plan->state != GNC_CAP_GAINS_PLAN_RUNNING)
        return;
    plan->state = gnc_scrub_context_is_cancelled (plan->context)
        ? GNC_CAP_GAINS_PLAN_CANCELLED : GNC_CAP_GAINS_PLAN_STALE;
}

/* ============================================================== */

gboolean
xaccAccountHasTrades (const Account *acc)
{
    gnc_commodity *acc_comm;

    if (!acc) return FALSE;

    if (xaccAccountIsPriced (acc))
        return TRUE;

    acc_comm = xaccAccountGetCommodity(acc);

    for (auto s : xaccAccountGetSplits (acc))
    {
        Transaction *t = s->parent;
	if (s->gains == GAINS_STATUS_GAINS) continue;
        if (acc_comm != t->common_currency) return TRUE;
    }

    return FALSE;
}

struct GncAccountTradesPlan
{
    GncScrubContext *context;
    QofBook *book;
    GncGUID account_guid;
    guint64 splits_generation;
    guint64 scrub_generation;
    size_t index;
    gboolean has_trades;
    GncAccountTradesPlanState state;
};

static gboolean
account_trades_plan_valid (GncAccountTradesPlan *plan)
{
    if (!plan || plan->state != GNC_ACCOUNT_TRADES_PLAN_RUNNING)
        return FALSE;
    if (gnc_scrub_context_is_cancelled (plan->context))
    {
        plan->state = GNC_ACCOUNT_TRADES_PLAN_CANCELLED;
        return FALSE;
    }
    auto account = xaccAccountLookup (&plan->account_guid, plan->book);
    if (!gnc_scrub_context_owns_book (plan->context, plan->book) || !account ||
        gnc_account_get_splits_generation (account) != plan->splits_generation ||
        gnc_account_get_scrub_generation (account) != plan->scrub_generation)
    {
        plan->state = GNC_ACCOUNT_TRADES_PLAN_STALE;
        return FALSE;
    }
    return TRUE;
}

GncAccountTradesPlan *
gnc_account_trades_plan_begin (const Account *account,
                               GncScrubContext *context)
{
    if (!account || !context)
        return nullptr;
    auto book = qof_instance_get_book (QOF_INSTANCE (account));
    if (!gnc_scrub_context_owns_book (context, book))
        return nullptr;
    return new GncAccountTradesPlan {
        gnc_scrub_context_ref (context), book,
        *qof_instance_get_guid (QOF_INSTANCE (account)),
        gnc_account_get_splits_generation (account),
        gnc_account_get_scrub_generation (account), 0, FALSE,
        GNC_ACCOUNT_TRADES_PLAN_RUNNING};
}

GncAccountTradesPlanState
gnc_account_trades_plan_step (GncAccountTradesPlan *plan, guint max_work)
{
    if (!plan || plan->state != GNC_ACCOUNT_TRADES_PLAN_RUNNING || max_work == 0)
        return plan ? plan->state : GNC_ACCOUNT_TRADES_PLAN_FAILED;
    guint work = 0;
    while (work++ < max_work && plan->state == GNC_ACCOUNT_TRADES_PLAN_RUNNING)
    {
        if (!account_trades_plan_valid (plan)) break;
        auto account = xaccAccountLookup (&plan->account_guid, plan->book);
        if (xaccAccountIsPriced (account))
        {
            plan->has_trades = TRUE;
            plan->state = GNC_ACCOUNT_TRADES_PLAN_DONE;
            break;
        }
        if (plan->index >= xaccAccountGetSplitsSize (account))
        {
            plan->state = GNC_ACCOUNT_TRADES_PLAN_DONE;
            break;
        }
        GncGUID guid;
        if (!gnc_account_get_split_guid_at (account, plan->splits_generation,
                                             plan->index++, &guid))
        {
            plan->state = GNC_ACCOUNT_TRADES_PLAN_STALE;
            break;
        }
        auto split = xaccSplitLookup (&guid, plan->book);
        auto transaction = split ? xaccSplitGetParent (split) : nullptr;
        if (!split || !transaction || xaccSplitGetAccount (split) != account)
        {
            plan->state = GNC_ACCOUNT_TRADES_PLAN_STALE;
            break;
        }
        if (split->gains != GAINS_STATUS_GAINS &&
            xaccAccountGetCommodity (account) !=
                xaccTransGetCurrency (transaction))
        {
            plan->has_trades = TRUE;
            plan->state = GNC_ACCOUNT_TRADES_PLAN_DONE;
        }
    }
    return plan->state;
}

GncAccountTradesPlanState
gnc_account_trades_plan_get_state (const GncAccountTradesPlan *plan)
{
    return plan ? plan->state : GNC_ACCOUNT_TRADES_PLAN_FAILED;
}

gboolean
gnc_account_trades_plan_get_result (const GncAccountTradesPlan *plan,
                                    gboolean *has_trades)
{
    if (!plan || plan->state != GNC_ACCOUNT_TRADES_PLAN_DONE || !has_trades)
        return FALSE;
    *has_trades = plan->has_trades;
    return TRUE;
}

void gnc_account_trades_plan_cancel (GncAccountTradesPlan *plan)
{
    if (plan && plan->state == GNC_ACCOUNT_TRADES_PLAN_RUNNING)
        plan->state = GNC_ACCOUNT_TRADES_PLAN_CANCELLED;
}

void gnc_account_trades_plan_free (GncAccountTradesPlan *plan)
{
    if (!plan) return;
    gnc_scrub_context_unref (plan->context);
    delete plan;
}

/* ============================================================== */

struct FindLot
{
    GNCLot *lot;
    gnc_commodity *currency;
    time64 time;
    int (*numeric_pred)(gnc_numeric);
    gboolean (*date_pred)(time64 e, time64 tr);
};

static gboolean
earliest_pred (time64 earl, time64 tran)
{
    return earl > tran;
}

static gboolean
latest_pred (time64 earl, time64 tran)
{
    return earl < tran;
}

static gpointer
finder_helper (GNCLot *lot,  gpointer user_data)
{
    auto els = static_cast<FindLot*>(user_data);
    Split *s;
    Transaction *trans;
    gnc_numeric bal;
    gboolean opening_is_positive, bal_is_positive;
    time64 posted = 0;

    if (gnc_lot_is_closed (lot)) return nullptr;

    s = gnc_lot_get_earliest_split (lot);
    if (s == nullptr) return nullptr;

    /* We want a lot whose balance is of the correct sign.  All splits
       in a lot must be the opposite sign of the opening split.  We also
       want to ignore lots that are overfull, i.e., where the balance in
       the lot is of opposite sign to the opening split in the lot. */
    if (0 == (els->numeric_pred) (s->amount)) return nullptr;
    bal = gnc_lot_get_balance (lot);
    opening_is_positive = gnc_numeric_positive_p (s->amount);
    bal_is_positive = gnc_numeric_positive_p (bal);
    if (opening_is_positive != bal_is_positive) return nullptr;

    trans = s->parent;
    if (els->currency &&
            (FALSE == gnc_commodity_equiv (els->currency,
                                           trans->common_currency)))
    {
        return nullptr;
    }

    posted = trans->date_posted;
    if (els->date_pred (els->time, posted))
    {
        els->time = trans->date_posted;
        els->lot = lot;
    }

    return nullptr;
}

static inline GNCLot *
xaccAccountFindOpenLot (Account *acc, gnc_numeric sign,
                        gnc_commodity *currency,
                        gint64 guess,
                        gboolean (*date_pred)(time64, time64))
{
    FindLot es;

    es.lot = nullptr;
    es.currency = currency;
    es.time = guess;
    es.date_pred = date_pred;

    if (gnc_numeric_positive_p(sign)) es.numeric_pred = gnc_numeric_negative_p;
    else es.numeric_pred = gnc_numeric_positive_p;

    xaccAccountForEachLot (acc, finder_helper, &es);
    return es.lot;
}

GNCLot *
xaccAccountFindEarliestOpenLot (Account *acc, gnc_numeric sign,
                                gnc_commodity *currency)
{
    GNCLot *lot;
    ENTER (" sign=%" G_GINT64_FORMAT "/%" G_GINT64_FORMAT, sign.num,
           sign.denom);

    lot = xaccAccountFindOpenLot (acc, sign, currency,
                                  G_MAXINT64, earliest_pred);
    LEAVE ("found lot=%p %s baln=%s", lot, gnc_lot_get_title (lot),
           gnc_num_dbg_to_string(gnc_lot_get_balance(lot)));
    return lot;
}

GNCLot *
xaccAccountFindLatestOpenLot (Account *acc, gnc_numeric sign,
                              gnc_commodity *currency)
{
    GNCLot *lot;
    ENTER (" sign=%" G_GINT64_FORMAT "/%" G_GINT64_FORMAT,
           sign.num, sign.denom);

    lot = xaccAccountFindOpenLot (acc, sign, currency,
                                  G_MININT64, latest_pred);
    LEAVE ("found lot=%p %s", lot, gnc_lot_get_title (lot));
    return lot;
}

/* ============================================================== */

Split *
gnc_split_assign_to_lot_prepared (Split *split, GNCLot *lot,
                                  gnc_numeric baln, gboolean had_splits)
{
    Account *acc;
    int cmp;
    gboolean baln_is_positive, amt_is_positive;

    if (!lot) return split;
    if (!split) return nullptr;

    /* If this split already belongs to a lot, we are done. */
    if (split->lot) return nullptr;

    /* Anomalous situation; except for voided transactions,
     * we don't expect to see splits with no amount ..
     * unless they're gains splits, and we shouldn't see those.
     */
    if (gnc_numeric_zero_p (split->amount))
    {
        if (xaccTransGetVoidStatus(split->parent)) return nullptr;

        PWARN ("split with zero amount; value=%s gflag=%x gsplit=%p",
               gnc_num_dbg_to_string (split->amount),
               split->gains,
               split->gains_split);
        if (split->gains_split)
        {
            PWARN ("gains amt=%s value=%s",
                   gnc_num_dbg_to_string (split->gains_split->amount),
                   gnc_num_dbg_to_string (split->gains_split->value));
        }
        return nullptr;
    }

    /* If the lot is closed, we can't add anything to it */
    if (had_splits && gnc_numeric_zero_p (baln)) return split;

    /* If the lot balance is zero, but the lot is open, then
     * the lot is empty. Unconditionally add the split. */
    if (gnc_numeric_zero_p (baln))
    {
        acc = split->acc;
        xaccAccountBeginEdit (acc);
        gnc_lot_add_split (lot, split);
        xaccAccountCommitEdit (acc);
        return nullptr;
    }

    /* If the sign of the split is the same as the sign of the lot,
     * add the split, but complain about it ... none of the currently
     * implemented accounting policies should be giving us splits
     * that make lots larger.  One a lot is open, the FIFO/LIFO
     * policies should be working only to make the lot smaller.
     * We can remove the warning emssage come the day we have
     * fancier policies.
     */
    baln_is_positive = gnc_numeric_positive_p (baln);
    amt_is_positive = gnc_numeric_positive_p (split->amount);
    if ((baln_is_positive && amt_is_positive) ||
            ((!baln_is_positive) && (!amt_is_positive)))
    {
        PWARN ("accounting policy gave us split that enlarges the lot!\n"
               "old lot baln=%s split amt=%s lot=%s",
               gnc_num_dbg_to_string (baln),
               gnc_num_dbg_to_string (split->amount),
               gnc_lot_get_title (lot));

        acc = split->acc;
        xaccAccountBeginEdit (acc);
        gnc_lot_add_split (lot, split);
        xaccAccountCommitEdit (acc);
        return nullptr;
    }

    /* If adding the split would make the lot balance change sign,
     * then we split the split into two pieces: one piece that will
     * bring the lot balance to zero, and another to be dealt with
     * later.  */
    cmp = gnc_numeric_compare (gnc_numeric_abs(xaccSplitGetAdjustedAmount (split)),
                               gnc_numeric_abs(baln));

    PINFO ("found open lot with baln=%s (%s)", gnc_num_dbg_to_string (baln),
           gnc_lot_get_title (lot));

    /* cmp == -1 if amt < baln, ==0 if amt==baln */
    if (0 >= cmp)
    {
        acc = split->acc;
        xaccAccountBeginEdit (acc);
        gnc_lot_add_split (lot, split);
        xaccAccountCommitEdit (acc);
        return nullptr;
    }

    /* If we are here, then (cmp == +1 iff (amt > baln)) and we need
     * to split up the split into pieces. Do it. */
    {
        time64 now = gnc_time (nullptr), time = 0;
        Split * new_split;
        gnc_numeric amt_a, amt_b, amt_tot;
        gnc_numeric val_a, val_b, val_tot;
        gnc_numeric frac;
        Transaction *trans;

        acc = split->acc;
        xaccAccountBeginEdit (acc);
        trans = split->parent;
        xaccTransBeginEdit (trans);

        /* Adjust split's view of the lot balance for stock splits */
        if (!gnc_numeric_equal (split->amount, xaccSplitGetAdjustedAmount (split)))
        {
            frac = gnc_numeric_div (split->amount, xaccSplitGetAdjustedAmount (split),
                                    GNC_DENOM_AUTO, GNC_HOW_DENOM_REDUCE);
            baln = gnc_numeric_mul (baln, frac,
                                    xaccAccountGetCommoditySCU(acc),
                                    GNC_HOW_RND_ROUND_HALF_UP);
        }

        amt_tot = split->amount;
        amt_a = gnc_numeric_neg (baln);
        amt_b = gnc_numeric_sub_fixed (amt_tot, amt_a);
        g_return_val_if_fail(gnc_numeric_check(amt_b) == GNC_ERROR_OK, nullptr);

        PINFO ("++++++++++++++ splitting split=%p into amt = %s + %s",
               split,
               gnc_num_dbg_to_string(amt_a),
               gnc_num_dbg_to_string(amt_b) );

        /* Compute the value so that it holds in the same proportion:
         * i.e. so that (amt_a / amt_tot) = (val_a / val_tot)
         */
        val_tot = split->value;
        frac = gnc_numeric_div (amt_a, amt_tot,
                                GNC_DENOM_AUTO, GNC_HOW_DENOM_REDUCE);
        val_a = gnc_numeric_mul (frac, val_tot,
                                 gnc_numeric_denom(val_tot),
                                 GNC_HOW_RND_ROUND_HALF_UP | GNC_HOW_DENOM_EXACT);

        val_b = gnc_numeric_sub_fixed (val_tot, val_a);
        if (gnc_numeric_check(val_a))
        {
            PERR("Numeric overflow\n"
                 "Acct=%s Txn=%s\n"
                 "\tval_tot=%s amt_a=%s amt_tot=%s\n",
                 xaccAccountGetName(acc),
                 xaccTransGetDescription(trans),
                 gnc_num_dbg_to_string(val_tot),
                 gnc_num_dbg_to_string(amt_a),
                 gnc_num_dbg_to_string(amt_tot));
        }

        if (gnc_numeric_zero_p(amt_a) || gnc_numeric_zero_p(amt_b))
        {
            PERR ("Failed to split into two!");
        }

        PINFO ("split value is = %s = %s + %s",
               gnc_num_dbg_to_string(val_tot),
               gnc_num_dbg_to_string(val_a),
               gnc_num_dbg_to_string(val_b) );

        g_return_val_if_fail (!gnc_numeric_zero_p (amt_a), nullptr);
        g_return_val_if_fail (!gnc_numeric_check (val_a), nullptr);
        xaccSplitSetAmount (split, amt_a);
        xaccSplitSetValue (split, val_a);

        /* Adding this split will have the effect of closing this lot,
         * because the new balance should be precisely zero. */
        gnc_lot_add_split (lot, split);

        /* Put the remainder of the balance into a new split,
         * which is in other respects just a clone of this one. */
        new_split = xaccMallocSplit (qof_instance_get_book(acc));

        /* Copy most of the split attributes */
        xaccSplitSetMemo (new_split, xaccSplitGetMemo (split));
        /* Set split-action with gnc_set_num_action which is the same as
         * xaccSplitSetAction with these arguments; use gnc_get_num_action to get
         * split-action which is the same as xaccSplitGetAction */
        gnc_set_num_action(nullptr, new_split, nullptr, gnc_get_num_action(nullptr, split));
        xaccSplitSetReconcile (new_split, xaccSplitGetReconcile (split));
        time = xaccSplitGetDateReconciled (split);
        xaccSplitSetDateReconciledSecs (new_split, time);

        /* Set the lot-split and peer_guid properties on the two
         * splits to indicate that they're linked. 
         */
        xaccSplitAddPeerSplit(split, new_split, now);
        xaccSplitAddPeerSplit(new_split, split, now);
        xaccAccountInsertSplit (acc, new_split);
        xaccTransAppendSplit (trans, new_split);
        /* Set the amount and value after the split is in the transaction
           so it can find the correct denominator to use.  Otherwise it
           uses 100000 which may cause an overflow in some of the tests
           in test-period */
        xaccSplitSetAmount (new_split, amt_b);
        xaccSplitSetValue (new_split, val_b);
        xaccTransCommitEdit (trans);
        xaccAccountCommitEdit (acc);
        return new_split;
    }
}

Split *
xaccSplitAssignToLot (Split *split, GNCLot *lot)
{
    if (!lot)
        return split;
    if (!split)
        return nullptr;
    auto had_splits = gnc_lot_get_split_list (lot) != nullptr;
    auto balance = gnc_lot_get_balance (lot);
    return gnc_split_assign_to_lot_prepared (split, lot, balance, had_splits);
}

/* ============================================================== */

/* Accounting-policy callback.  Given an account and an amount,
 * this routine should return a lot.  By implementing this as
 * a callback, we can 'easily' add other accounting policies.
 */
gboolean
xaccSplitAssign (Split *split)
{
    Account *acc;
    gboolean splits_split_up = FALSE;
    GNCLot *lot;
    GNCPolicy *pcy;

    if (!split) return FALSE;

    /* If this split already belongs to a lot or the account doesn't
     * have lots, we are done.
     */
    if (split->lot) return FALSE;
    g_return_val_if_fail (split->gains == GAINS_STATUS_UNKNOWN ||
                          (split->gains & GAINS_STATUS_GAINS) == FALSE, FALSE);
    acc = split->acc;
    if (!xaccAccountHasTrades (acc))
        return FALSE;
    if (gnc_numeric_zero_p (split->amount))
        return FALSE;

    ENTER ("(split=%p)", split);

    pcy = gnc_account_get_policy(acc);
    xaccAccountBeginEdit (acc);

    /* If we are here, this split does not belong to any lot.
     * We ask the policy for a lot to assign it to.  This
     * block is written in the form of a while loop, since we
     * may have to bust a split across several lots.
     */
    while (split)
    {
        PINFO ("have split %p amount=%s", split,
               gnc_num_dbg_to_string (split->amount));
        split->gains |= GAINS_STATUS_VDIRTY;
        lot = pcy->PolicyGetLot (pcy, split);
        if (!lot)
        {
            lot = gnc_lot_make_default (acc);
            PINFO ("start new lot (%s)", gnc_lot_get_title(lot));
        }
        split = xaccSplitAssignToLot (split, lot);
        if (split) splits_split_up = TRUE;
    }
    xaccAccountCommitEdit (acc);

    LEAVE (" split_up=%d", splits_split_up);
    return splits_split_up;
}

/* ============================================================== */

static Split *
find_split_by_type (const gchar *type, const Split *split)
{
    GncGUID *type_guid = nullptr;

    if (!split) return nullptr;

    qof_instance_get (QOF_INSTANCE (split), type, &type_guid, nullptr);
    if (!type_guid) return nullptr;

    /* Both splits will be in the same collection, so search there. */
    auto *guid_split = (Split *) qof_collection_lookup_entity (qof_instance_get_collection (split), type_guid);
    PINFO ("split=%p has %s=%p", split, type, guid_split);
    guid_free (type_guid);
    return guid_split;
}


Split *
xaccSplitGetCapGainsSplit (const Split *split)
{
    return find_split_by_type ("gains-split", split);
}

Split *
xaccSplitGetGainsSourceSplit (const Split *split)
{
    return find_split_by_type ("gains-source", split);
}

static gboolean
apply_cap_gains_value (Split *split, Account *gain_acc, gnc_numeric value)
{
    if (!split || !split->lot || gnc_numeric_zero_p (value))
        return TRUE;
    auto lot = split->lot;
    auto currency = split->parent->common_currency;
    auto zero = gnc_numeric_zero ();
    auto negvalue = gnc_numeric_neg (value);
    Transaction *trans;
    Split *lot_split = split->gains_split;
    Split *gain_split;
    gboolean update;

    if (!lot_split)
    {
        auto lot_acc = gnc_lot_get_account (lot);
        auto book = qof_instance_get_book (QOF_INSTANCE (lot_acc));
        auto base_txn = xaccSplitGetParent (split);
        if (!gain_acc || !gnc_commodity_equiv (
                currency, xaccAccountGetCommodity (gain_acc)))
            return FALSE;

        update = TRUE;
        lot_split = xaccMallocSplit (book);
        gain_split = xaccMallocSplit (book);
        xaccAccountBeginEdit (gain_acc);
        xaccAccountInsertSplit (gain_acc, gain_split);
        xaccAccountCommitEdit (gain_acc);
        xaccAccountBeginEdit (lot_acc);
        xaccAccountInsertSplit (lot_acc, lot_split);
        xaccAccountCommitEdit (lot_acc);
        trans = xaccMallocTransaction (book);
        xaccTransBeginEdit (trans);
        xaccTransSetCurrency (trans, currency);
        xaccTransSetDescription (trans, _("Realized Gain/Loss"));
        xaccTransAppendSplit (trans, lot_split);
        xaccTransAppendSplit (trans, gain_split);
        xaccSplitSetMemo (lot_split, _("Realized Gain/Loss"));
        xaccSplitSetMemo (gain_split, _("Realized Gain/Loss"));
        xaccTransBeginEdit (base_txn);
        qof_instance_set (QOF_INSTANCE (split), "gains-split",
                          xaccSplitGetGUID (lot_split), nullptr);
        xaccTransCommitEdit (base_txn);
        qof_instance_set (QOF_INSTANCE (lot_split), "gains-source",
                          xaccSplitGetGUID (split), nullptr);
    }
    else
    {
        trans = lot_split->parent;
        gain_split = xaccSplitGetOtherSplit (lot_split);
        if (!gain_split)
            update = FALSE;
        else if (split->gains_split == lot_split &&
                 lot_split->gains_split == split &&
                 gain_split->gains_split == split &&
                 gnc_numeric_equal (xaccSplitGetValue (lot_split), value) &&
                 gnc_numeric_zero_p (xaccSplitGetAmount (lot_split)) &&
                 gnc_numeric_equal (xaccSplitGetValue (gain_split), negvalue) &&
                 gnc_numeric_equal (xaccSplitGetAmount (gain_split), negvalue))
            update = FALSE;
        else
        {
            update = TRUE;
            xaccTransBeginEdit (trans);
            if (!gnc_commodity_equiv (currency, trans->common_currency))
                xaccTransSetCurrency (trans, currency);
        }
    }

    if (!update)
        return TRUE;
    auto posted = xaccTransRetDatePosted (split->parent);
    xaccTransSetDatePostedSecs (trans, posted);
    xaccTransSetDateEnteredSecs (trans, gnc_time (nullptr));
    xaccSplitSetAmount (lot_split, zero);
    xaccSplitSetValue (lot_split, value);
    xaccSplitSetAmount (gain_split, negvalue);
    xaccSplitSetValue (gain_split, negvalue);
    gnc_split_bump_scrub_generations (split);
    gnc_split_bump_scrub_generations (lot_split);
    gnc_split_bump_scrub_generations (gain_split);
    split->gains = GAINS_STATUS_CLEAN;
    split->gains_split = lot_split;
    lot_split->gains = GAINS_STATUS_GAINS;
    lot_split->gains_split = split;
    gain_split->gains = GAINS_STATUS_GAINS;
    gain_split->gains_split = split;
    gnc_lot_add_split (lot, lot_split);
    xaccTransCommitEdit (trans);
    return TRUE;
}

GncCapGainsPlan *
gnc_cap_gains_plan_begin (Split *split, Account *gain_account,
                          GncScrubContext *context)
{
    if (!split || !context)
        return nullptr;
    xaccSplitDetermineGainStatus (split);
    if ((split->gains & GAINS_STATUS_GAINS) && split->gains_split)
        split = split->gains_split;
    auto lot = xaccSplitGetLot (split);
    auto book = qof_instance_get_book (QOF_INSTANCE (split));
    if (!lot || !gnc_scrub_context_owns_book (context, book))
        return nullptr;
    GncGUID gain_guid = *guid_null ();
    if (gain_account)
        gain_guid = *qof_instance_get_guid (QOF_INSTANCE (gain_account));
    return new GncCapGainsPlan {
        gnc_scrub_context_ref (context), book,
        *qof_instance_get_guid (QOF_INSTANCE (split)),
        *qof_instance_get_guid (QOF_INSTANCE (lot)), gain_guid,
        gnc_lot_get_scrub_generation (lot), gnc_lot_get_split_list (lot),
        *guid_null (), FALSE, FALSE, gnc_numeric_zero (), gnc_numeric_zero (),
        gnc_numeric_zero (), CapPlanPhase::SCAN_LOT,
        GNC_CAP_GAINS_PLAN_RUNNING, nullptr, {}, {}, *guid_null (), 0, 0, FALSE};
}

static gboolean
cap_plan_scan_one (GncCapGainsPlan *plan)
{
    if (!plan->next)
    {
        plan->phase = CapPlanPhase::EVALUATE;
        return TRUE;
    }
    auto candidate = GNC_SPLIT (plan->next->data);
    plan->next = plan->next->next;
    auto split = xaccSplitLookup (&plan->split_guid, plan->book);
    auto lot = gnc_lot_lookup (&plan->lot_guid, plan->book);
    if (!candidate || !split || xaccSplitGetLot (candidate) != lot)
        return FALSE;
    xaccSplitDetermineGainStatus (candidate);
    if (!plan->has_earliest)
    {
        plan->earliest_guid = *qof_instance_get_guid (QOF_INSTANCE (candidate));
        plan->earliest_dirty = candidate->gains & GAINS_STATUS_VDIRTY;
        plan->has_earliest = TRUE;
    }
    else
    {
        auto earliest = xaccSplitLookup (&plan->earliest_guid, plan->book);
        if (!earliest)
            return FALSE;
        if (xaccSplitOrderDateOnly (candidate, earliest) < 0)
        {
            plan->earliest_guid = *qof_instance_get_guid (QOF_INSTANCE (candidate));
            plan->earliest_dirty = candidate->gains & GAINS_STATUS_VDIRTY;
        }
    }

    auto target = xaccSplitGetGainsSourceSplit (split);
    if (!target) target = split;
    auto source = xaccSplitGetGainsSourceSplit (candidate);
    if (!source) source = candidate;
    auto ta = xaccSplitGetParent (source);
    auto tb = xaccSplitGetParent (target);
    if ((ta == tb && source != target) || xaccTransOrder (ta, tb) < 0)
    {
        plan->balance_before_amount = gnc_numeric_add_fixed (
            plan->balance_before_amount, xaccSplitGetAmount (candidate));
        plan->balance_before_value = gnc_numeric_add_fixed (
            plan->balance_before_value, xaccSplitGetValue (candidate));
    }
    return !gnc_numeric_check (plan->balance_before_amount) &&
           !gnc_numeric_check (plan->balance_before_value);
}

static gboolean
cap_plan_complete (GncCapGainsPlan *plan)
{
    auto split = plan ? xaccSplitLookup (&plan->split_guid, plan->book) : nullptr;
    if (!split)
        return FALSE;
    split->gains &= ~GAINS_STATUS_VDIRTY;
    if (split->gains_split)
        split->gains_split->gains &= ~GAINS_STATUS_VDIRTY;
    plan->state = GNC_CAP_GAINS_PLAN_DONE;
    return TRUE;
}

static gboolean
cap_plan_evaluate (GncCapGainsPlan *plan)
{
    auto split = xaccSplitLookup (&plan->split_guid, plan->book);
    auto earliest = xaccSplitLookup (&plan->earliest_guid, plan->book);
    if (!split || !earliest)
        return FALSE;
    auto currency = xaccTransGetCurrency (xaccSplitGetParent (split));
    auto account = xaccSplitGetAccount (split);
    if (!currency || !account)
        return FALSE;
    if (gnc_commodity_equal (currency, xaccAccountGetCommodity (account)) ||
        split == earliest || g_strcmp0 ("stock-split", xaccSplitGetType (split)) == 0 ||
        (split->gains & GAINS_STATUS_GAINS) || gnc_numeric_zero_p (split->amount))
    {
        return cap_plan_complete (plan);
    }
    if (plan->earliest_dirty)
        split->gains |= GAINS_STATUS_VDIRTY;
    if (!(split->gains & GAINS_STATUS_A_VDIRTY) && split->gains_split &&
        !(split->gains_split->gains & GAINS_STATUS_A_VDIRTY))
    {
        return cap_plan_complete (plan);
    }
    if (!gnc_commodity_equiv (currency,
                              xaccTransGetCurrency (xaccSplitGetParent (earliest))))
    {
        return cap_plan_complete (plan);
    }
    if (gnc_numeric_compare (gnc_numeric_abs (plan->balance_before_amount),
                             gnc_numeric_abs (split->amount)) < 0)
        return FALSE;
    auto frac = gnc_numeric_div (split->amount, plan->balance_before_amount,
                                 GNC_DENOM_AUTO, GNC_HOW_DENOM_REDUCE);
    auto basis = gnc_numeric_mul (frac, plan->balance_before_value,
                                  gnc_numeric_denom (xaccSplitGetValue (earliest)),
                                  GNC_HOW_DENOM_EXACT | GNC_HOW_RND_ROUND_HALF_UP);
    plan->gain_value = gnc_numeric_sub (basis, split->value,
                                        GNC_DENOM_AUTO, GNC_HOW_DENOM_FIXED);
    if (gnc_numeric_check (plan->gain_value))
        return FALSE;
    if (gnc_numeric_zero_p (plan->gain_value)) return cap_plan_complete (plan);

    if (guid_equal (&plan->gain_account_guid, guid_null ()))
    {
        auto configured = gnc_account_get_configured_gains_account (account, currency);
        if (configured)
            plan->gain_account_guid = *qof_instance_get_guid (QOF_INSTANCE (configured));
    }
    if (!guid_equal (&plan->gain_account_guid, guid_null ()))
        plan->phase = CapPlanPhase::APPLY;
    else
    {
        plan->orphan_name = gnc_account_dup_orphan_gains_name (currency);
        auto root = gnc_account_get_root (account);
        if (!root || !plan->orphan_name)
            return FALSE;
        plan->bfs_current.push_back (*qof_instance_get_guid (QOF_INSTANCE (root)));
        plan->phase = CapPlanPhase::RESOLVE_GAIN_ACCOUNT;
    }
    return TRUE;
}

static gboolean
cap_plan_resolve_account_one (GncCapGainsPlan *plan)
{
    if (!plan->bfs_enumerating)
    {
        if (plan->bfs_current.empty ())
        {
            if (!plan->bfs_next.empty ())
                plan->bfs_current.swap (plan->bfs_next);
            else
            {
                auto split = xaccSplitLookup (&plan->split_guid, plan->book);
                auto account = split ? xaccSplitGetAccount (split) : nullptr;
                auto currency = split ? xaccTransGetCurrency (xaccSplitGetParent (split)) : nullptr;
                auto root = account ? gnc_account_get_root (account) : nullptr;
                auto gains = gnc_account_create_orphan_gains_account_prepared (root, currency);
                if (!gains) return FALSE;
                gnc_account_set_configured_gains_account (account, currency, gains);
                plan->gain_account_guid = *qof_instance_get_guid (QOF_INSTANCE (gains));
                plan->phase = CapPlanPhase::APPLY;
                return TRUE;
            }
        }
        plan->bfs_parent = plan->bfs_current.front ();
        plan->bfs_current.pop_front ();
        auto parent = xaccAccountLookup (&plan->bfs_parent, plan->book);
        if (!parent) return FALSE;
        plan->bfs_generation = gnc_account_get_children_generation (parent);
        plan->bfs_index = 0;
        plan->bfs_enumerating = TRUE;
    }
    auto parent = xaccAccountLookup (&plan->bfs_parent, plan->book);
    if (!parent || gnc_account_get_children_generation (parent) != plan->bfs_generation)
        return FALSE;
    if (plan->bfs_index >= static_cast<size_t> (gnc_account_n_children (parent)))
    {
        plan->bfs_enumerating = FALSE;
        return TRUE;
    }
    GncGUID child_guid;
    if (!gnc_account_get_child_guid_at (parent, plan->bfs_generation,
                                        plan->bfs_index++, &child_guid))
        return FALSE;
    auto child = xaccAccountLookup (&child_guid, plan->book);
    if (!child) return FALSE;
    plan->bfs_next.push_back (child_guid);
    if (g_strcmp0 (xaccAccountGetName (child), plan->orphan_name) == 0)
    {
        plan->gain_account_guid = child_guid;
        auto split = xaccSplitLookup (&plan->split_guid, plan->book);
        auto currency = split ? xaccTransGetCurrency (xaccSplitGetParent (split)) : nullptr;
        gnc_account_set_configured_gains_account (xaccSplitGetAccount (split),
                                                   currency, child);
        plan->phase = CapPlanPhase::APPLY;
    }
    return TRUE;
}

GncCapGainsPlanState
gnc_cap_gains_plan_step (GncCapGainsPlan *plan, guint max_work)
{
    if (!plan || plan->state != GNC_CAP_GAINS_PLAN_RUNNING || max_work == 0)
        return plan ? plan->state : GNC_CAP_GAINS_PLAN_FAILED;
    guint work = 0;
    while (work++ < max_work && plan->state == GNC_CAP_GAINS_PLAN_RUNNING)
    {
        if (!cap_plan_valid (plan))
        {
            cap_plan_fail_validation (plan);
            break;
        }
        gboolean ok = FALSE;
        switch (plan->phase)
        {
        case CapPlanPhase::SCAN_LOT: ok = cap_plan_scan_one (plan); break;
        case CapPlanPhase::EVALUATE: ok = cap_plan_evaluate (plan); break;
        case CapPlanPhase::RESOLVE_GAIN_ACCOUNT:
            ok = cap_plan_resolve_account_one (plan); break;
        case CapPlanPhase::APPLY:
        {
            auto split = xaccSplitLookup (&plan->split_guid, plan->book);
            auto gains = xaccAccountLookup (&plan->gain_account_guid, plan->book);
            ok = apply_cap_gains_value (split, gains, plan->gain_value);
            if (ok) ok = cap_plan_complete (plan);
            break;
        }
        }
        if (!ok && plan->state == GNC_CAP_GAINS_PLAN_RUNNING)
            plan->state = GNC_CAP_GAINS_PLAN_FAILED;
    }
    return plan->state;
}

GncCapGainsPlanState
gnc_cap_gains_plan_get_state (const GncCapGainsPlan *plan)
{
    return plan ? plan->state : GNC_CAP_GAINS_PLAN_FAILED;
}

void gnc_cap_gains_plan_cancel (GncCapGainsPlan *plan)
{
    if (plan && plan->state == GNC_CAP_GAINS_PLAN_RUNNING)
        plan->state = GNC_CAP_GAINS_PLAN_CANCELLED;
}

void gnc_cap_gains_plan_free (GncCapGainsPlan *plan)
{
    if (!plan) return;
    gnc_scrub_context_unref (plan->context);
    g_free (plan->orphan_name);
    delete plan;
}

/* ============================================================== */

void
xaccSplitComputeCapGains(Split *split, Account *gain_acc)
{
    SplitList *node;
    GNCLot *lot;
    GNCPolicy *pcy;
    gnc_commodity *currency = nullptr;
    gnc_numeric zero = gnc_numeric_zero();
    gnc_numeric value;
    gnc_numeric frac;
    gnc_numeric opening_amount, opening_value;
    gnc_numeric lot_amount, lot_value;
    gnc_commodity *opening_currency;

    if (!split) return;
    lot = split->lot;
    if (!lot) return;
    pcy = gnc_account_get_policy(gnc_lot_get_account(lot));
    currency = split->parent->common_currency;

    ENTER ("(split=%p gains=%p status=0x%x lot=%s)", split,
           split->gains_split, split->gains, gnc_lot_get_title(lot));

    /* Make sure the status flags and pointers are initialized */
    xaccSplitDetermineGainStatus(split);

    /* Not possible to have gains if the transaction currency and
     * account commodity are identical. */
    if (gnc_commodity_equal (currency,
                             xaccAccountGetCommodity(split->acc)))
    {
        LEAVE ("Currency transfer, gains not possible, returning.");
        return;
    }

    if (pcy->PolicyIsOpeningSplit (pcy, lot, split))
    {
#if MOVE_THIS_TO_A_DATA_INTEGRITY_SCRUBBER
        /* Check to make sure that this opening split doesn't
         * have a cap-gain transaction associated with it.
         * If it does, that's wrong, and we ruthlessly destroy it.
         * XXX Don't do this, it leads to infinite loops.
         * We need to scrub out errors like this elsewhere!
         */
        if (xaccSplitGetCapGainsSplit (split))
        {
            Split *gains_split = xaccSplitGetCapGainsSplit(split);
            Transaction *trans = gains_split->parent;
            PERR ("Opening Split must not have cap gains!!\n");

            xaccTransBeginEdit (trans);
            xaccTransDestroy (trans);
            xaccTransCommitEdit (trans);
        }
#endif
        LEAVE ("Lot opening split, returning.");
        return;
    }

    if (xaccSplitIsStockSplit (split))
    {
        LEAVE ("Stock split split, returning.");
        return;
    }

    if (GAINS_STATUS_GAINS & split->gains)
    {
        Split *s;
        PINFO ("split is a gains recording split, switch over");
        /* If this is the split that records the gains, then work with
         * the split that generates the gains.
         */
        /* split = xaccSplitGetCapGainsSplit (split); */
        s = split->gains_split;

        /* This should never be nullptr, and if it is, and its matching
         * parent can't be found, then its a bug, and we should be
         * discarding this split.   But ... for now .. return.
         * XXX move appropriate actions to a 'scrub' routine'
         */
        if (!s)
        {
            PERR ("Bad gains-split pointer! .. trying to recover.");
            split->gains_split = xaccSplitGetCapGainsSplit (split);
            s = split->gains_split;
            if (!s) return;
#if MOVE_THIS_TO_A_DATA_INTEGRITY_SCRUBBER
            xaccTransDestroy (trans);
#endif
        }
        split = s;
    }

    /* Note: if the value of the 'opening' split(s) has changed,
     * then the cap gains are changed. So we need to check not
     * only if this split is dirty, but also the lot-opening splits. */
    for (node = gnc_lot_get_split_list(lot); node; node = node->next)
    {
        Split *s = GNC_SPLIT(node->data);
        if (pcy->PolicyIsOpeningSplit(pcy, lot, s))
        {
            if (GAINS_STATUS_UNKNOWN == s->gains) xaccSplitDetermineGainStatus (s);
            if (s->gains & GAINS_STATUS_VDIRTY)
            {
                /* Force a recompute to occur */
                split->gains |= GAINS_STATUS_VDIRTY;
                break;
            }
        }
    }

    /* If it doesn't look like this split is 'dirty', then there's
     * nothing to do. Just return. */
    if ((FALSE == (split->gains & GAINS_STATUS_A_VDIRTY))  &&
            (split->gains_split) &&
            (FALSE == (split->gains_split->gains & GAINS_STATUS_A_VDIRTY)))
    {
        LEAVE ("split not dirty, returning");
        return;
    }

    /* Yow! If amount is zero, there's nothing to do! Amount-zero splits
     * may exist if users attempted to manually record gains. */
    if (gnc_numeric_zero_p (split->amount)) return;

    /* If we got to here, then the split or something related is
     * 'dirty' and the gains really do need to be recomputed.
     * So start working things. */

    /* Get the amount and value in this lot at the time of this transaction. */
    gnc_lot_get_balance_before (lot, split, &lot_amount, &lot_value);

    pcy->PolicyGetLotOpening (pcy, lot, &opening_amount, &opening_value,
                              &opening_currency);

    /* Check to make sure the lot-opening currency and this split
     * use the same currency */
    if (FALSE == gnc_commodity_equiv (currency, opening_currency))
    {
        /* OK, the purchase and the sale were made in different currencies.
         * I don't know how to compute cap gains for that.  This is not
         * an error. Just punt, silently.
         */
        LEAVE ("Can't compute gains, mismatched commodities!");
        return;
    }

    /* Opening amount should be larger (or equal) to current split,
     * and it should be of the opposite sign.
     * XXX This should really be a part of a scrub routine that
     * cleans up the lot, before we get at it!
     */
    if (0 > gnc_numeric_compare (gnc_numeric_abs(lot_amount),
                                 gnc_numeric_abs(xaccSplitGetAdjustedAmount (split))))
    {
        GList *n;
        for (n = gnc_lot_get_split_list(lot); n; n = n->next)
        {
            Split *s = GNC_SPLIT(n->data);
            PINFO ("split adj amt=%s", gnc_num_dbg_to_string(xaccSplitGetAdjustedAmount (s)));
        }
        PERR ("Malformed Lot \"%s\"! (too thin!) "
              "opening amt=%s split adj amt=%s baln=%s",
              gnc_lot_get_title (lot),
              gnc_num_dbg_to_string (lot_amount),
              gnc_num_dbg_to_string (xaccSplitGetAdjustedAmount (split)),
              gnc_num_dbg_to_string (gnc_lot_get_balance(lot)));
        return;
    }
    if ( (gnc_numeric_negative_p(lot_amount) ||
            gnc_numeric_positive_p(xaccSplitGetAdjustedAmount (split))) &&
            (gnc_numeric_positive_p(lot_amount) ||
             gnc_numeric_negative_p(xaccSplitGetAdjustedAmount (split))))
    {
        GList *n;
        for (n = gnc_lot_get_split_list(lot); n; n = n->next)
        {
            Split *s = GNC_SPLIT(n->data);
            PINFO ("split adj amt=%s", gnc_num_dbg_to_string(xaccSplitGetAdjustedAmount (s)));
        }
        PERR ("Malformed Lot \"%s\"! (too fat!) "
              "opening adj amt=%s split adj amt=%s baln=%s",
              gnc_lot_get_title (lot),
              gnc_num_dbg_to_string (lot_amount),
              gnc_num_dbg_to_string (xaccSplitGetAdjustedAmount (split)),
              gnc_num_dbg_to_string (gnc_lot_get_balance(lot)));
        return;
    }

    /* The cap gains is the difference between the basis prior to the
     * current split, and the current split, pro-rated for an equal
     * amount of shares.
     * i.e. purchase_price = lot_value / lot_amount
     * cost_basis = purchase_price * current_split_amount
     * cap_gain = current_split_value - cost_basis
     */
    /* Fraction of the lot that this split represents: */
    frac = gnc_numeric_div (xaccSplitGetAdjustedAmount (split), lot_amount,
                            GNC_DENOM_AUTO,
                            GNC_HOW_DENOM_REDUCE);
    /* Basis for this split: */
    value = gnc_numeric_mul (frac, lot_value,
                             gnc_numeric_denom(opening_value),
                             GNC_HOW_DENOM_EXACT | GNC_HOW_RND_ROUND_HALF_UP);
    /* Capital gain for this split: */
    value = gnc_numeric_sub (value, split->value,
                             GNC_DENOM_AUTO, GNC_HOW_DENOM_FIXED);
    PINFO ("Open amt=%s val=%s;  split adj amt=%s val=%s; gains=%s\n",
           gnc_num_dbg_to_string (lot_amount),
           gnc_num_dbg_to_string (lot_value),
           gnc_num_dbg_to_string (xaccSplitGetAdjustedAmount (split)),
           gnc_num_dbg_to_string (split->value),
           gnc_num_dbg_to_string (value));
    if (gnc_numeric_check (value))
    {
        PERR ("Numeric overflow during gains calculation\n"
              "Acct=%s Txn=%s\n"
              "\tOpen amt=%s val=%s\n\tsplit amt=%s val=%s\n\tgains=%s\n",
              xaccAccountGetName(split->acc),
              xaccTransGetDescription(split->parent),
              gnc_num_dbg_to_string (lot_amount),
              gnc_num_dbg_to_string (lot_value),
              gnc_num_dbg_to_string (xaccSplitGetAdjustedAmount (split)),
              gnc_num_dbg_to_string (split->value),
              gnc_num_dbg_to_string (value));
        return;
    }

    /* Are the cap gains zero?  If not, add a balancing transaction.
     * As per design doc lots.txt: the transaction has two splits,
     * with equal & opposite values.  The amt of one is zero (so as
     * not to upset the lot balance), the amt of the other is the same
     * as its value (its the realized gain/loss).
     */
    if (FALSE == gnc_numeric_zero_p (value))
    {
        Transaction *trans;
        Split *lot_split, *gain_split;
        gboolean new_gain_split;
        gnc_numeric negvalue = gnc_numeric_neg (value);

        /* See if there already is an associated gains transaction.
         * If there is, adjust its value as appropriate. Else, create
         * a new gains transaction.
         */
        /* lot_split = xaccSplitGetCapGainsSplit (split);  */
        lot_split = split->gains_split;

        if (nullptr == lot_split)
        {
            Account *lot_acc = gnc_lot_get_account(lot);
            QofBook *book = qof_instance_get_book(lot_acc);
            Transaction *base_txn = xaccSplitGetParent (split);

            new_gain_split = TRUE;

            lot_split = xaccMallocSplit (book);
            gain_split = xaccMallocSplit (book);

            /* Check to make sure the gains account currency matches. */
            if ((nullptr == gain_acc) ||
                    (FALSE == gnc_commodity_equiv (currency,
                                                   xaccAccountGetCommodity(gain_acc))))
            {
                gain_acc = xaccAccountGainsAccount (lot_acc, currency);
            }

            xaccAccountBeginEdit (gain_acc);
            xaccAccountInsertSplit (gain_acc, gain_split);
            xaccAccountCommitEdit (gain_acc);

            xaccAccountBeginEdit (lot_acc);
            xaccAccountInsertSplit (lot_acc, lot_split);
            xaccAccountCommitEdit (lot_acc);

            trans = xaccMallocTransaction (book);

            xaccTransBeginEdit (trans);
            xaccTransSetCurrency (trans, currency);
            xaccTransSetDescription (trans, _("Realized Gain/Loss"));

            xaccTransAppendSplit (trans, lot_split);
            xaccTransAppendSplit (trans, gain_split);

            xaccSplitSetMemo (lot_split, _("Realized Gain/Loss"));
            xaccSplitSetMemo (gain_split, _("Realized Gain/Loss"));

            /* For the new transaction, set the split properties indicating
             * that this is the gains transaction that corresponds
             * to the gains source.
             */
            xaccTransBeginEdit (base_txn);
            qof_instance_set (QOF_INSTANCE (split),
                              "gains-split", xaccSplitGetGUID (lot_split),
                              nullptr);
            xaccTransCommitEdit (base_txn);
            qof_instance_set (QOF_INSTANCE (lot_split),
                              "gains-source", xaccSplitGetGUID (split),
                              nullptr);

        }
        else
        {
            trans = lot_split->parent;
            gain_split = xaccSplitGetOtherSplit (lot_split);

            /* If the gains transaction has been edited so that it no longer has
               just two splits, ignore it and assume it's still correct. */
            if (!gain_split)
            {
                new_gain_split = FALSE;
            }
            /* If the gain is already recorded correctly do nothing.  This is
             * more than just an optimization since this may be called during
             * gnc_book_partition_txn and depending on the order in which things
             * happen some splits may be in the wrong book at that time. */
            else if (split->gains_split == lot_split &&
                     lot_split->gains_split == split &&
                     gain_split->gains_split == split &&
                     gnc_numeric_equal (xaccSplitGetValue (lot_split), value) &&
                     gnc_numeric_zero_p (xaccSplitGetAmount (lot_split)) &&
                     gnc_numeric_equal (xaccSplitGetValue (gain_split), negvalue) &&
                     gnc_numeric_equal (xaccSplitGetAmount (gain_split), negvalue))
            {
                new_gain_split = FALSE;
            }
            else
            {
                new_gain_split = TRUE;
                xaccTransBeginEdit (trans);

                /* Make sure the existing gains trans has the correct currency,
                 * just in case someone screwed with it! */
                if (FALSE == gnc_commodity_equiv(currency, trans->common_currency))
                {
                    PWARN ("Resetting the transaction currency!");
                    xaccTransSetCurrency (trans, currency);
                }
            }
        }

        if (new_gain_split)
        {
            /* Common to both */
            time64 time = xaccTransRetDatePosted (split->parent);
            xaccTransSetDatePostedSecs (trans, time);
            xaccTransSetDateEnteredSecs (trans, gnc_time (nullptr));

            xaccSplitSetAmount (lot_split, zero);
            xaccSplitSetValue (lot_split, value);

            xaccSplitSetAmount (gain_split, negvalue);
            xaccSplitSetValue (gain_split, negvalue);

            gnc_split_bump_scrub_generations (split);
            gnc_split_bump_scrub_generations (lot_split);
            gnc_split_bump_scrub_generations (gain_split);

            /* Some short-cuts to help avoid the above property lookup. */
            split->gains = GAINS_STATUS_CLEAN;
            split->gains_split = lot_split;
            lot_split->gains = GAINS_STATUS_GAINS;
            lot_split->gains_split = split;
            gain_split->gains = GAINS_STATUS_GAINS;
            gain_split->gains_split = split;

            /* Do this last since it may generate an event that will call us
               recursively. */
            gnc_lot_add_split (lot, lot_split);

            xaccTransCommitEdit (trans);
        }
    }
    LEAVE ("(lot=%s)", gnc_lot_get_title(lot));
}

/* ============================================================== */

gnc_numeric
xaccLotFreeSplitCapGain(Split *split, GNCLot *lot)
{
    g_return_val_if_fail (split && lot && !split->lot, gnc_numeric_zero());

    ENTER ("(split=%p lot=%s)", split, gnc_lot_get_title(lot));

    auto pcy = gnc_account_get_policy (gnc_lot_get_account (lot));
    auto currency = split->parent->common_currency;

    /* Not possible to have gains if the transaction currency and
     * account commodity are identical. */
    if (gnc_commodity_equal (currency,
                             xaccAccountGetCommodity(split->acc)))
    {
        LEAVE ("Currency transfer, gains not possible, returning.");
        return gnc_numeric_zero();
    }

    auto *esplit = gnc_lot_get_earliest_split (lot);
    if (!esplit)
    {
        LEAVE ("Lot is empty, returning.");
        return gnc_numeric_zero();
    }

    if (xaccTransGetDate (xaccSplitGetParent (esplit)) >= xaccTransGetDate (xaccSplitGetParent (split)))
    {
        LEAVE ("Split is too early, returning.");
        return gnc_numeric_zero();
    }

    if (xaccSplitIsStockSplit (split))
    {
        LEAVE ("Stock split split, returning.");
        return gnc_numeric_zero();
    }

    /* If amount is zero, there's nothing to do! Amount-zero splits
     * may exist if users attempted to manually record gains. */
    if (gnc_numeric_zero_p (split->amount)) return gnc_numeric_zero();

    /* If we got to here, then the gains really do need to be recomputed.
     * So start working things. */

    /* Get the amount and value in this lot at the time of this transaction. */
    gnc_numeric lot_amount, lot_value;
    gnc_lot_get_balance_before (lot, split, &lot_amount, &lot_value);

    gnc_numeric opening_amount, opening_value;
    gnc_commodity *opening_currency;
    pcy->PolicyGetLotOpening (pcy, lot, &opening_amount, &opening_value,
                              &opening_currency);

    /* Check to make sure the lot-opening currency and this split
     * use the same currency */
    if (FALSE == gnc_commodity_equiv (currency, opening_currency))
    {
        /* OK, the purchase and the sale were made in different currencies.
         * I don't know how to compute cap gains for that.  This is not
         * an error. Just punt, silently.
         */
        LEAVE ("Can't compute gains, mismatched commodities!");
        return gnc_numeric_zero();
    }

    /* Opening amount should be larger (or equal) to current split,
     * and it should be of the opposite sign.
     * XXX This should really be a part of a scrub routine that
     * cleans up the lot, before we get at it!
     */
    if (0 > gnc_numeric_compare (gnc_numeric_abs(lot_amount),
                                 gnc_numeric_abs(xaccSplitGetAdjustedAmount (split))))
    {
        for (auto *n = gnc_lot_get_split_list(lot); n; n = n->next)
        {
            auto *s = GNC_SPLIT(n->data);
            PINFO ("split adj amt=%s", gnc_num_dbg_to_string(xaccSplitGetAdjustedAmount (s)));
        }
        return gnc_numeric_zero();
    }
    if ( (gnc_numeric_negative_p(lot_amount) ||
            gnc_numeric_positive_p(xaccSplitGetAdjustedAmount (split))) &&
            (gnc_numeric_positive_p(lot_amount) ||
             gnc_numeric_negative_p(xaccSplitGetAdjustedAmount (split))))
    {
        for (auto *n = gnc_lot_get_split_list(lot); n; n = n->next)
        {
            auto *s = GNC_SPLIT(n->data);
            PINFO ("split adj amt=%s", gnc_num_dbg_to_string(xaccSplitGetAdjustedAmount (s)));
        }
        return gnc_numeric_zero();
    }

    /* The cap gains is the difference between the basis prior to the
     * current split, and the current split, pro-rated for an equal
     * amount of shares.
     * i.e. purchase_price = lot_value / lot_amount
     * cost_basis = purchase_price * current_split_amount
     * cap_gain = current_split_value - cost_basis
     */
    /* Fraction of the lot that this split represents: */
    auto frac = gnc_numeric_div (xaccSplitGetAdjustedAmount (split), lot_amount,
                            GNC_DENOM_AUTO,
                            GNC_HOW_DENOM_REDUCE);
    /* Basis for this split: */
    auto value = gnc_numeric_mul (frac, lot_value,
                             gnc_numeric_denom(opening_value),
                             GNC_HOW_DENOM_EXACT | GNC_HOW_RND_ROUND_HALF_UP);
    /* Capital gain for this split: */
    value = gnc_numeric_sub (value, split->value,
                             GNC_DENOM_AUTO, GNC_HOW_DENOM_FIXED);

    PINFO ("Open amt=%s val=%s;  split adj amt=%s val=%s; gains=%s\n",
           gnc_num_dbg_to_string (lot_amount),
           gnc_num_dbg_to_string (lot_value),
           gnc_num_dbg_to_string (xaccSplitGetAdjustedAmount (split)),
           gnc_num_dbg_to_string (split->value),
           gnc_num_dbg_to_string (value));

    if (gnc_numeric_check (value))
    {
        PERR ("Numeric overflow during gains calculation\n"
              "Acct=%s Txn=%s\n"
              "\tOpen amt=%s val=%s\n\tsplit amt=%s val=%s\n\tgains=%s\n",
              xaccAccountGetName(split->acc),
              xaccTransGetDescription(split->parent),
              gnc_num_dbg_to_string (lot_amount),
              gnc_num_dbg_to_string (lot_value),
              gnc_num_dbg_to_string (xaccSplitGetAdjustedAmount (split)),
              gnc_num_dbg_to_string (split->value),
              gnc_num_dbg_to_string (value));
        return gnc_numeric_zero();
    }

    LEAVE ("(lot=%s)", gnc_lot_get_title(lot));

    return value;
}

/* ============================================================== */

gnc_numeric
xaccSplitGetCapGains(Split * split)
{
    if (!split) return gnc_numeric_zero();
    ENTER("(split=%p)", split);

    if (GAINS_STATUS_UNKNOWN == split->gains)
        xaccSplitDetermineGainStatus(split);
    if ((split->gains & GAINS_STATUS_A_VDIRTY) ||
            (split->gains_split &&
             (split->gains_split->gains & GAINS_STATUS_A_VDIRTY)))
    {
        xaccSplitComputeCapGains (split, nullptr);
    }

    /* If this is the source split, get the gains from the one
     * that records the gains.  If this already is the gains split,
     * its a no-op. */
    if (!(GAINS_STATUS_GAINS & split->gains))
    {
        /* split = xaccSplitGetCapGainsSplit (split); */
        split = split->gains_split;
    }

    LEAVE("(split=%p)", split);
    if (!split) return gnc_numeric_zero();

    return split->value;
}

/* ============================================================== */

void
xaccLotComputeCapGains (GNCLot *lot, Account *gain_acc)
{
    SplitList *node;
    GNCPolicy *pcy;
    gboolean is_dirty = FALSE;

    /* Note: if the value of the 'opening' split(s) has changed,
     * then the cap gains are changed. To capture this, we need
     * to mark all splits dirty if the opening splits are dirty. */

    ENTER("(lot=%p)", lot);
    pcy = gnc_account_get_policy(gnc_lot_get_account(lot));
    for (node = gnc_lot_get_split_list(lot); node; node = node->next)
    {
        Split *s = GNC_SPLIT(node->data);
        if (pcy->PolicyIsOpeningSplit(pcy, lot, s))
        {
            if (GAINS_STATUS_UNKNOWN == s->gains)
                xaccSplitDetermineGainStatus(s);
            if (s->gains & GAINS_STATUS_VDIRTY)
            {
                is_dirty = TRUE;
                s->gains &= ~GAINS_STATUS_VDIRTY;
            }
        }
    }

    if (is_dirty)
    {
        for (node = gnc_lot_get_split_list(lot); node; node = node->next)
        {
            Split *s = GNC_SPLIT(node->data);
            s->gains |= GAINS_STATUS_VDIRTY;
        }
    }

    for (node = gnc_lot_get_split_list(lot); node; node = node->next)
    {
        Split *s = GNC_SPLIT(node->data);
        xaccSplitComputeCapGains (s, gain_acc);
    }
    LEAVE("(lot=%p)", lot);
}

/* =========================== END OF FILE ======================= */
