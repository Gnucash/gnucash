/********************************************************************\
 * Scrub2.c -- Convert Stock Accounts to use Lots                   *
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

/** @file Scrub2.c
 *  @brief Utilities to Convert Stock Accounts to use Lots
 *  @author Created by Linas Vepstas March 2003
 *  @author Copyright (c) 2003 Linas Vepstas <linas@linas.org>
 *
 * Provides a set of functions and utilities for checking and
 * repairing ('scrubbing clean') the usage of Lots and lot balances
 * in stock and commodity accounts.  Broken lots are repaired using
 * the accounts specific accounting policy (probably FIFO).
 */

#include <config.h>

#include <glib.h>

#include <deque>

#include "qof.h"
#include "Account.h"
#include "AccountP.hpp"
#include "Account.hpp"
#include "Transaction.h"
#include "TransactionP.hpp"
#include "Scrub.h"
#include "Scrub2.h"
#include "cap-gains.h"
#include "gnc-engine.h"
#include "gncInvoice.h"
#include "gnc-lot.h"
#include "policy-p.h"

static QofLogModule log_module = GNC_MOD_LOT;

struct GncLotStatsPlan
{
    GncScrubContext *context;
    QofBook *book;
    GncGUID lot_guid;
    GList *next;
    guint64 generation;
    gnc_numeric amount;
    gnc_numeric value;
    guint split_count;
    GncGUID earliest_split;
    GncGUID latest_split;
    GncGUID currency;
    gboolean has_earliest;
    gboolean has_latest;
    gboolean has_currency;
    GncLotStatsPlanState state;
};

struct GncLotAssignmentPlan
{
    GncScrubContext *context;
    QofBook *book;
    GncGUID account_guid;
    std::deque<GncGUID> split_guids;
    guint completed;
    guint examined;
    GncLotAssignmentPlanState state;
};

static GncLotStatsPlanState
lot_stats_validate (GncLotStatsPlan *plan)
{
    if (!plan || plan->state != GNC_LOT_STATS_RUNNING)
        return plan ? plan->state : GNC_LOT_STATS_FAILED;
    if (gnc_scrub_context_is_cancelled (plan->context))
        return plan->state = GNC_LOT_STATS_CANCELLED;
    if (!gnc_scrub_context_owns_book (plan->context, plan->book))
        return plan->state = GNC_LOT_STATS_STALE;
    auto lot = gnc_lot_lookup (&plan->lot_guid, plan->book);
    if (!lot || gnc_lot_get_scrub_generation (lot) != plan->generation)
        return plan->state = GNC_LOT_STATS_STALE;
    return plan->state;
}

GncLotStatsPlan *
gnc_lot_stats_plan_begin (GNCLot *lot, GncScrubContext *context)
{
    if (!lot || !context)
        return nullptr;
    auto book = qof_instance_get_book (QOF_INSTANCE (lot));
    if (!gnc_scrub_context_owns_book (context, book))
        return nullptr;
    return new GncLotStatsPlan {
        gnc_scrub_context_ref (context), book,
        *qof_instance_get_guid (QOF_INSTANCE (lot)),
        gnc_lot_get_split_list (lot), gnc_lot_get_scrub_generation (lot),
        gnc_numeric_zero (), gnc_numeric_zero (), 0, *guid_null (), *guid_null (),
        *guid_null (), FALSE, FALSE, FALSE, GNC_LOT_STATS_RUNNING};
}

GncLotStatsPlanState
gnc_lot_stats_plan_step (GncLotStatsPlan *plan, guint max_work)
{
    if (lot_stats_validate (plan) != GNC_LOT_STATS_RUNNING || max_work == 0)
        return plan ? plan->state : GNC_LOT_STATS_FAILED;

    guint work = 0;
    while (work++ < max_work && plan->next)
    {
        if (lot_stats_validate (plan) != GNC_LOT_STATS_RUNNING)
            return plan->state;
        auto split = GNC_SPLIT (plan->next->data);
        plan->next = plan->next->next;
        auto lot = gnc_lot_lookup (&plan->lot_guid, plan->book);
        if (!split || xaccSplitGetLot (split) != lot)
            return plan->state = GNC_LOT_STATS_STALE;

        plan->amount = gnc_numeric_add (plan->amount, xaccSplitGetAmount (split),
                                         GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT);
        plan->value = gnc_numeric_add (plan->value, xaccSplitGetValue (split),
                                       GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT);
        if (gnc_numeric_check (plan->amount) || gnc_numeric_check (plan->value))
            return plan->state = GNC_LOT_STATS_FAILED;
        ++plan->split_count;

        auto transaction = xaccSplitGetParent (split);
        if (!transaction)
            return plan->state = GNC_LOT_STATS_STALE;
        auto currency = xaccTransGetCurrency (transaction);
        if (!plan->has_currency && currency)
        {
            plan->currency = *qof_instance_get_guid (QOF_INSTANCE (currency));
            plan->has_currency = TRUE;
        }
        if (!plan->has_earliest)
        {
            plan->earliest_split = *qof_instance_get_guid (QOF_INSTANCE (split));
            plan->has_earliest = TRUE;
        }
        else
        {
            auto earliest = xaccSplitLookup (&plan->earliest_split, plan->book);
            if (!earliest || xaccSplitOrderDateOnly (split, earliest) < 0)
                plan->earliest_split = *qof_instance_get_guid (QOF_INSTANCE (split));
        }
        if (!plan->has_latest)
        {
            plan->latest_split = *qof_instance_get_guid (QOF_INSTANCE (split));
            plan->has_latest = TRUE;
        }
        else
        {
            auto latest = xaccSplitLookup (&plan->latest_split, plan->book);
            if (!latest || xaccSplitOrderDateOnly (split, latest) > 0)
                plan->latest_split = *qof_instance_get_guid (QOF_INSTANCE (split));
        }
    }
    if (!plan->next)
        plan->state = GNC_LOT_STATS_DONE;
    return plan->state;
}

GncLotStatsPlanState
gnc_lot_stats_plan_get_state (const GncLotStatsPlan *plan)
{
    return plan ? plan->state : GNC_LOT_STATS_FAILED;
}

gboolean
gnc_lot_stats_plan_get_result (const GncLotStatsPlan *plan,
                               gnc_numeric *amount, gnc_numeric *value,
                               guint *split_count, GncGUID *earliest_split,
                               GncGUID *latest_split, GncGUID *currency)
{
    if (!plan || plan->state != GNC_LOT_STATS_DONE)
        return FALSE;
    if (amount) *amount = plan->amount;
    if (value) *value = plan->value;
    if (split_count) *split_count = plan->split_count;
    if (earliest_split) *earliest_split = plan->earliest_split;
    if (latest_split) *latest_split = plan->latest_split;
    if (currency) *currency = plan->currency;
    return TRUE;
}

void
gnc_lot_stats_plan_free (GncLotStatsPlan *plan)
{
    if (!plan) return;
    gnc_scrub_context_unref (plan->context);
    delete plan;
}

enum class LotAssignmentOutcome
{
    SKIPPED,
    ASSIGNED,
    FAILED,
};

struct LotAssignmentResult
{
    LotAssignmentOutcome outcome;
    Split *remainder;
};

/*
 * Execute exactly one policy-selected split-to-lot assignment. Both the
 * synchronous scrub and the bounded plan use this primitive so that policy
 * selection, FIFO ordering, and split remainder handling have one source of
 * truth. Ineligible splits are explicitly skipped, matching xaccSplitAssign.
 * The returned remainder is valid only in this call and must be converted to
 * its GUID before returning to the main loop.
 */
static LotAssignmentResult
assign_split_to_next_lot (Split *split)
{
    if (!split || split->lot)
        return {LotAssignmentOutcome::SKIPPED, nullptr};
    g_return_val_if_fail (split->gains == GAINS_STATUS_UNKNOWN ||
                          (split->gains & GAINS_STATUS_GAINS) == FALSE,
                          (LotAssignmentResult {LotAssignmentOutcome::FAILED,
                                                nullptr}));

    auto account = split->acc;
    if (!xaccAccountHasTrades (account) || gnc_numeric_zero_p (split->amount))
        return {LotAssignmentOutcome::SKIPPED, nullptr};

    auto policy = gnc_account_get_policy (account);
    if (!policy)
        return {LotAssignmentOutcome::SKIPPED, nullptr};

    split->gains |= GAINS_STATUS_VDIRTY;
    auto lot = policy->PolicyGetLot (policy, split);
    if (!lot)
        lot = gnc_lot_make_default (account);
    if (!lot)
        return {LotAssignmentOutcome::FAILED, nullptr};

    auto remainder = xaccSplitAssignToLot (split, lot);
    if (!split->lot || remainder == split)
        return {LotAssignmentOutcome::FAILED, nullptr};
    return {LotAssignmentOutcome::ASSIGNED, remainder};
}

static void
lot_assignment_plan_finish (GncLotAssignmentPlan *plan,
                            GncLotAssignmentPlanState state)
{
    if (!plan || plan->state != GNC_LOT_ASSIGNMENT_PLAN_RUNNING)
        return;

    plan->split_guids.clear ();
    plan->state = state;
}

static GncLotAssignmentPlanState
lot_assignment_plan_validate (GncLotAssignmentPlan *plan)
{
    if (!plan || plan->state != GNC_LOT_ASSIGNMENT_PLAN_RUNNING)
        return plan ? plan->state : GNC_LOT_ASSIGNMENT_PLAN_FAILED;

    if (gnc_scrub_context_is_cancelled (plan->context))
    {
        lot_assignment_plan_finish (plan, GNC_LOT_ASSIGNMENT_PLAN_CANCELLED);
        return plan->state;
    }
    if (!gnc_scrub_context_owns_book (plan->context, plan->book))
    {
        lot_assignment_plan_finish (plan, GNC_LOT_ASSIGNMENT_PLAN_STALE);
        return plan->state;
    }
    if (g_getenv ("GNC_AUTO_SCRUB_LOTS") != nullptr &&
        !gnc_scrub_context_commit_deferral_enabled (
            plan->context, GNC_SCRUB_DEFERRED_COMMIT_GAINS))
    {
        lot_assignment_plan_finish (plan, GNC_LOT_ASSIGNMENT_PLAN_STALE);
        return plan->state;
    }
    return GNC_LOT_ASSIGNMENT_PLAN_RUNNING;
}

GncLotAssignmentPlan *
gnc_lot_assignment_plan_begin (Account *account, GncScrubContext *context)
{
    if (!account || !context)
        return nullptr;

    auto book = qof_instance_get_book (QOF_INSTANCE (account));
    if (!gnc_scrub_context_owns_book (context, book))
        return nullptr;
    if (g_getenv ("GNC_AUTO_SCRUB_LOTS") != nullptr &&
        !gnc_scrub_context_commit_deferral_enabled (
            context, GNC_SCRUB_DEFERRED_COMMIT_GAINS))
        return nullptr;

    auto plan = new GncLotAssignmentPlan {gnc_scrub_context_ref (context),
                                          book,
                                          *xaccAccountGetGUID (account),
                                          {}, 0,
                                          0,
                                          GNC_LOT_ASSIGNMENT_PLAN_RUNNING};
    for (auto split : xaccAccountGetSplits (account))
        plan->split_guids.push_back (*qof_instance_get_guid (QOF_INSTANCE (split)));
    return plan;
}

GncLotAssignmentPlanState
gnc_lot_assignment_plan_step (GncLotAssignmentPlan *plan, guint max_work)
{
    if (lot_assignment_plan_validate (plan) != GNC_LOT_ASSIGNMENT_PLAN_RUNNING ||
        max_work == 0)
        return plan ? plan->state : GNC_LOT_ASSIGNMENT_PLAN_FAILED;

    auto account = xaccAccountLookup (&plan->account_guid, plan->book);
    if (!account)
    {
        lot_assignment_plan_finish (plan, GNC_LOT_ASSIGNMENT_PLAN_STALE);
        return plan->state;
    }

    guint work = 0;
    while (work < max_work && !plan->split_guids.empty ())
    {
        if (lot_assignment_plan_validate (plan) != GNC_LOT_ASSIGNMENT_PLAN_RUNNING)
            return plan->state;

        auto guid = plan->split_guids.front ();
        plan->split_guids.pop_front ();
        ++work;
        ++plan->examined;
        auto split = xaccSplitLookup (&guid, plan->book);
        if (!split || xaccSplitGetAccount (split) != account)
            continue;

        auto result = assign_split_to_next_lot (split);
        if (result.outcome == LotAssignmentOutcome::SKIPPED)
            continue;
        if (result.outcome == LotAssignmentOutcome::FAILED)
        {
            lot_assignment_plan_finish (plan, GNC_LOT_ASSIGNMENT_PLAN_FAILED);
            return plan->state;
        }

        g_assert (result.outcome == LotAssignmentOutcome::ASSIGNED);
        ++plan->completed;
        if (result.remainder)
        {
            plan->split_guids.push_front (
                *qof_instance_get_guid (QOF_INSTANCE (result.remainder)));
        }
    }

    if (plan->split_guids.empty ())
        lot_assignment_plan_finish (plan, GNC_LOT_ASSIGNMENT_PLAN_DONE);
    return plan->state;
}

void
gnc_lot_assignment_plan_cancel (GncLotAssignmentPlan *plan)
{
    lot_assignment_plan_finish (plan, GNC_LOT_ASSIGNMENT_PLAN_CANCELLED);
}

GncLotAssignmentPlanState
gnc_lot_assignment_plan_get_state (const GncLotAssignmentPlan *plan)
{
    return plan ? plan->state : GNC_LOT_ASSIGNMENT_PLAN_FAILED;
}

guint
gnc_lot_assignment_plan_get_examined (const GncLotAssignmentPlan *plan)
{
    return plan ? plan->examined : 0;
}

guint
gnc_lot_assignment_plan_get_completed (const GncLotAssignmentPlan *plan)
{
    return plan ? plan->completed : 0;
}

void
gnc_lot_assignment_plan_free (GncLotAssignmentPlan *plan)
{
    if (!plan)
        return;

    plan->split_guids.clear ();
    gnc_scrub_context_unref (plan->context);
    delete plan;
}

/* ============================================================== */
/** Loop over all splits, and make sure that every split
 * belongs to some lot.  If a split does not belong to
 * any lots, poke it into one.
 */

void
xaccAccountAssignLots (Account *acc)
{
    if (!acc) return;

    ENTER ("acc=%s", xaccAccountGetName(acc));
    xaccAccountBeginEdit (acc);

restart_loop:
    for (auto split : xaccAccountGetSplits (acc))
    {
        /* If already in lot, then no-op */
        if (split->lot) continue;

        /* Skip stock splits */
        if (xaccSplitIsStockSplit(split)) continue;

        /* Skip voided transactions */
        if (gnc_numeric_zero_p (split->amount) &&
                xaccTransGetVoidStatus(split->parent)) continue;

        auto remainder = split;
        auto split_up = false;
        while (remainder)
        {
            auto result = assign_split_to_next_lot (remainder);
            if (result.outcome != LotAssignmentOutcome::ASSIGNED)
                break;
            split_up = split_up || result.remainder;
            remainder = result.remainder;
        }

        /* A split remainder is inserted into the account, invalidating the
         * vector cursor. Restart as the original scrubber did before the
         * account split container was converted to std::vector. */
        if (split_up)
            goto restart_loop;
    }
    xaccAccountCommitEdit (acc);
    LEAVE ("acc=%s", xaccAccountGetName(acc));
}

/* ============================================================== */

/** The xaccLotFill() routine attempts to assign splits to the
 *  indicated lot until the lot balance goes to zero, or until
 *  there are no suitable (i.e. unassigned) splits left in the
 *  account.  It uses the default accounting policy to choose
 *  the splits to fill out the lot.
 */

void
xaccLotFill (GNCLot *lot)
{
    Account *acc;
    Split *split;
    GNCPolicy *pcy;

    if (!lot) return;
    acc = gnc_lot_get_account(lot);
    pcy = gnc_account_get_policy(acc);

    ENTER ("(lot=%s, acc=%s)", gnc_lot_get_title(lot), xaccAccountGetName(acc));

    /* If balance already zero, we have nothing to do. */
    if (gnc_lot_is_closed (lot))
    {
        LEAVE ("Lot Closed (lot=%s, acc=%s)", gnc_lot_get_title(lot),
               xaccAccountGetName(acc));
        return;
    }
    split = pcy->PolicyGetSplit (pcy, lot);
    if (!split)
    {
        LEAVE ("No Split (lot=%s, acc=%s)", gnc_lot_get_title(lot),
               xaccAccountGetName(acc));
        return;   /* Handle the common case */
    }

    /* Reject stock split transactions */
    if (xaccSplitIsStockSplit(split))
    {
        LEAVE ("Stock split transaction (lot=%s, acc=%s)",
           gnc_lot_get_title(lot), xaccAccountGetName(acc));
        return;
    }

    /* Reject voided transactions */
    if (gnc_numeric_zero_p(split->amount) &&
            xaccTransGetVoidStatus(split->parent))
    {
        LEAVE ("Voided transaction (lot=%s, acc=%s)",
               gnc_lot_get_title(lot), xaccAccountGetName(acc));
        return;
    }

    xaccAccountBeginEdit (acc);

    /* Loop until we've filled up the lot, (i.e. till the
     * balance goes to zero) or there are no splits left.  */
    while (1)
    {
        Split *subsplit;

        subsplit = xaccSplitAssignToLot (split, lot);
        if (subsplit == split)
        {
            PERR ("Accounting Policy gave us a split that "
                  "doesn't fit into this lot\n"
                  "lot baln=%s, isclosed=%d, aplit amt=%s",
                  gnc_num_dbg_to_string (gnc_lot_get_balance(lot)),
                  gnc_lot_is_closed (lot),
                  gnc_num_dbg_to_string (split->amount));
            break;
        }

        if (gnc_lot_is_closed (lot)) break;

        split = pcy->PolicyGetSplit (pcy, lot);
        if (!split) break;
    }
    xaccAccountCommitEdit (acc);
    LEAVE ("(lot=%s, acc=%s)", gnc_lot_get_title(lot), xaccAccountGetName(acc));
}

/* ============================================================== */

void
xaccLotScrubDoubleBalance (GNCLot *lot)
{
    gnc_commodity *currency = nullptr;
    SplitList *snode;
    GList *node;
    gnc_numeric zero = gnc_numeric_zero();
    gnc_numeric value = zero;

    if (!lot) return;

    ENTER ("lot=%s", gnc_lot_get_title(lot));

    for (snode = gnc_lot_get_split_list(lot); snode; snode = snode->next)
    {
        Split *s = GNC_SPLIT(snode->data);
        xaccSplitComputeCapGains (s, nullptr);
    }

    /* We double-check only closed lots */
    if (FALSE == gnc_lot_is_closed (lot))
    {
        LEAVE ("lot=%s is closed", gnc_lot_get_title(lot));
        return;
    }

    for (snode = gnc_lot_get_split_list(lot); snode; snode = snode->next)
    {
        Split *s = GNC_SPLIT(snode->data);
        Transaction *trans = s->parent;

        /* Check to make sure all splits in the lot have a common currency */
        if (nullptr == currency)
        {
            currency = trans->common_currency;
        }
        if (FALSE == gnc_commodity_equiv (currency, trans->common_currency))
        {
            /* This lot has mixed currencies. Can't double-balance.
             * Silently punt */
            PWARN ("Lot with multiple currencies:\n"
                   "\ttrans=%s curr=%s", xaccTransGetDescription(trans),
                   gnc_commodity_get_fullname(trans->common_currency));
            break;
        }

        /* Now, total up the values */
        value = gnc_numeric_add (value, xaccSplitGetValue (s),
                                 GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT);
        PINFO ("Split=%p value=%s Accum Lot value=%s", s,
               gnc_num_dbg_to_string (s->value),
               gnc_num_dbg_to_string (value));

    }

    if (FALSE == gnc_numeric_equal (value, zero))
    {
        /* Unhandled error condition. Not sure what to do here,
         * Since the ComputeCapGains should have gotten it right.
         * I suppose there might be small rounding errors, a penny or two,
         * the ideal thing would to figure out why there's a rounding
         * error, and fix that.
         */
        PERR ("Closed lot fails to double-balance !! lot value=%s",
              gnc_num_dbg_to_string (value));
        for (node = gnc_lot_get_split_list(lot); node; node = node->next)
        {
            Split *s = GNC_SPLIT(node->data);
            PERR ("s=%p amt=%s val=%s", s,
                  gnc_num_dbg_to_string(s->amount),
                  gnc_num_dbg_to_string(s->value));
        }
    }

    LEAVE ("lot=%s", gnc_lot_get_title(lot));
}

/* ================================================================= */

static inline gboolean
is_subsplit (Split *split)
{

    /* generic stop-progress conditions */
    if (!split) return FALSE;
    g_return_val_if_fail (split->parent, FALSE);

    /* If there are no sub-splits, then there's nothing to do. */
    return xaccSplitHasPeers (split);
}

/* ================================================================= */


/* ================================================================= */

/* Remove the guid of b from a.  Note that a may not contain the guid
 * of b, (and v.v.) in which case, it will contain other guids which
 * establish the links. So merge them back in. */

static void
remove_guids (Split *sa, Split *sb)
{
     xaccSplitRemovePeerSplit (sa, sb);
     xaccSplitRemovePeerSplit (sb, sa);
     xaccSplitMergePeerSplits (sa, sb);
}

/* The merge_splits() routine causes the amount & value of sb
 * to be merged into sa; it then destroys sb.  It also performs
 * some other misc cleanup */

static void
merge_splits (Split *sa, Split *sb)
{
    Account *act;
    Transaction *txn;
    gnc_numeric amt, val;

    act = xaccSplitGetAccount (sb);
    xaccAccountBeginEdit (act);

    txn = sa->parent;
    xaccTransBeginEdit (txn);

    /* Remove the guid of sb from the 'gemini' of sa */
    remove_guids (sa, sb);

    /* Add amount of sb into sa, ditto for value. */
    amt = xaccSplitGetAmount (sa);
    amt = gnc_numeric_add_fixed (amt, xaccSplitGetAmount (sb));
    xaccSplitSetAmount (sa, amt);

    val = xaccSplitGetValue (sa);
    val = gnc_numeric_add_fixed (val, xaccSplitGetValue (sb));
    xaccSplitSetValue (sa, val);

    /* Set reconcile to no; after this much violence,
     * no way its reconciled. */
    xaccSplitSetReconcile (sa, NREC);

    /* If sb has associated gains splits, trash them. */
    if ((sb->gains_split) &&
            (sb->gains_split->gains & GAINS_STATUS_GAINS))
    {
        Transaction *t = sb->gains_split->parent;
        xaccTransBeginEdit (t);
        xaccTransDestroy (t);
        xaccTransCommitEdit (t);
    }

    /* Finally, delete sb */
    xaccSplitDestroy(sb);

    xaccTransCommitEdit (txn);
    xaccAccountCommitEdit (act);
}

gboolean
gnc_scrub_merge_split_pair_prepared (Split *keep, Split *remove,
                                      gboolean strict)
{
    if (!keep || !remove || keep == remove ||
        xaccSplitGetParent (keep) != xaccSplitGetParent (remove) ||
        xaccSplitGetLot (keep) != xaccSplitGetLot (remove) ||
        qof_instance_get_destroying (remove))
        return FALSE;
    auto transaction = xaccSplitGetParent (keep);
    if (!transaction || gncInvoiceGetInvoiceFromTxn (transaction))
        return FALSE;
    if (strict && (!is_subsplit (keep) || !xaccSplitIsPeerSplit (keep, remove)))
        return FALSE;
    merge_splits (keep, remove);
    return TRUE;
}

gboolean
xaccScrubMergeSubSplits (Split *split, gboolean strict)
{
    gboolean rc = FALSE;
    Transaction *txn;
    SplitList *node;
    GNCLot *lot;

    if (strict && (FALSE == is_subsplit (split))) return FALSE;

    txn = split->parent;

    // Don't mess with splits from an invoice transaction
    // Those are the responsibility of the business code
    if (gncInvoiceGetInvoiceFromTxn (txn)) return FALSE;

    lot = xaccSplitGetLot (split);

    ENTER ("(Lot=%s)", gnc_lot_get_title(lot));
restart:
    for (node = txn->splits; node; node = node->next)
    {
        Split *s = GNC_SPLIT(node->data);
        if (xaccSplitGetLot (s) != lot) continue;
        if (s == split) continue;
        if (qof_instance_get_destroying(s)) continue;

        // Don't mess with splits from an invoice transaction
        // Those are the responsibility of the business code
        if (gncInvoiceGetInvoiceFromTxn (s->parent)) return FALSE;

        if (strict)
        {
            /* OK, this split is in the same lot (and thus same account)
             * as the indicated split.  Make sure it is really a subsplit
             * of the split we started with.  It's possible to have two
             * splits in the same lot and transaction that are not subsplits
             * of each other, the test-period test suite does this, for
             * example.  Only worry about adjacent sub-splits.  By
             * repeatedly merging adjacent subsplits, we'll get the non-
             * adjacent ones too. */
            if (!xaccSplitIsPeerSplit (split, s))
                continue;
        }

        if (!gnc_scrub_merge_split_pair_prepared (split, s, strict))
            continue;
        rc = TRUE;
        goto restart;
    }
    if (rc && gnc_numeric_zero_p (split->amount))
    {
        time64 pdate = xaccTransGetDate (txn);
        gchar *pdatestr = gnc_ctime (&pdate);
        PWARN ("Result of merge has zero amt!");
        PWARN ("Transaction details - posted date %s - description %s", pdatestr, xaccTransGetDescription(txn));
        g_free (pdatestr);
    }
    LEAVE (" splits merged=%d", rc);
    return rc;
}

gboolean
xaccScrubMergeLotSubSplits (GNCLot *lot, gboolean strict)
{
    gboolean rc = FALSE;
    SplitList *node;

    if (!lot) return FALSE;

    ENTER (" ");
restart:
    for (node = gnc_lot_get_split_list(lot); node; node = node->next)
    {
        Split *s = GNC_SPLIT(node->data);
        if (!xaccScrubMergeSubSplits(s, strict)) continue;

        rc = TRUE;
        goto restart;
    }
    LEAVE (" splits merged=%d", rc);
    return rc;
}

/* =========================== END OF FILE ======================= */
