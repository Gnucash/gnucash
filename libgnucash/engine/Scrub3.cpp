/********************************************************************\
 * Scrub3.c -- Constrain Cap Gains to Track Sources of Gains        *
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

/** @file Scrub3.c
 *  @brief Constrain Cap Gains to Track Sources of Gains
 *  @author Created by Linas Vepstas Sept 2003
 *  @author Copyright (c) 2003,2004 Linas Vepstas <linas@linas.org>
 *
 * Provides a set of functions and utilities for checking and
 * repairing ('scrubbing clean') the usage of Cap Gains
 * transactions in stock and commodity accounts.
 */

#include <config.h>

#include <glib.h>

#include <deque>
#include <unordered_set>

#include "cap-gains.h"
#include "gnc-commodity.h"
#include "gnc-engine.h"
#include "gnc-lot.h"
#include "gnc-session.h"
#include "guid.hpp"
#include "policy-p.h"
#include "Account.h"
#include "AccountP.hpp"
#include "Scrub2.h"
#include "Scrub3.h"
#include "ScrubP.h"
#include "SplitP.hpp"
#include "Transaction.h"
#include "TransactionP.hpp"

static QofLogModule log_module = GNC_MOD_LOT;

enum class LotScrubPhase
{
    MERGE_START,
    MERGE_LOT_SCAN,
    MERGE_TRANSACTION_SCAN,
    STATS_START,
    STATS,
    RETHIN_SCAN,
    FILL_SCAN_START,
    FILL_SCAN,
    FILL_APPLY,
    CAP_START,
    CAP_SCAN,
    CAP_CHILD,
    FINAL_STATS_START,
    FINAL_STATS,
    FINAL_VERIFY_START,
    FINAL_VERIFY,
    FINAL_CLEAR_START,
    FINAL_CLEAR,
    FINALIZE,
};

struct GncLotScrubPlan
{
    GncScrubContext *context;
    QofBook *book;
    GncGUID lot_guid;
    GncGUID account_guid;
    GncLotScrubPlanState state;
    LotScrubPhase phase;
    guint64 lot_generation;
    GList *lot_next;
    GncGUID merge_keep_guid;
    GncTransactionSplitCursor *transaction_cursor;
    GncLotStatsPlan *stats;
    gnc_numeric balance;
    gnc_numeric value;
    guint split_count;
    GncGUID earliest_guid;
    GncGUID latest_guid;
    GncGUID currency_guid;
    guint64 account_splits_generation;
    guint64 account_scrub_generation;
    size_t account_index;
    GncGUID fill_best_guid;
    gboolean has_fill_best;
    gboolean mark_all_value_dirty;
    GncGUID value_dirty_marker_guid;
    GncGUID structural_dirty_marker_guid;
    GncCapGainsPlan *cap_child;
    gboolean cap_restart_scan;
    gboolean filling;
    gboolean fill_complete;
    gboolean splits_deleted;
};

enum class AccountLotsPhase
{
    NEXT_ACCOUNT,
    TRADE_SCAN_START,
    TRADE_SCAN,
    SPLIT_SCAN_START,
    SPLIT_SCAN,
    SPLIT_CHILD,
    LOT_SCAN_START,
    LOT_SCAN,
    LOT_CHILD,
    CHILD_SCAN_START,
    CHILD_SCAN,
};

struct GncAccountLotsPlan
{
    GncScrubContext *context;
    QofBook *book;
    GncGUID root_guid;
    gboolean descendants;
    GncAccountLotsPlanState state;
    AccountLotsPhase phase;
    std::deque<GncGUID> account_queue;
    GncGUID account_guid;
    guint64 split_generation;
    guint64 scrub_generation;
    size_t split_index;
    GncAccountTradesPlan *trades_child;
    gboolean has_trades;
    std::unordered_set<GncGUID> completed_splits;
    GncGUID active_split_guid;
    GncSplitAssignPlan *assign_child;
    GncAccountLotCursor *lot_cursor;
    std::unordered_set<GncGUID> completed_lots;
    GncGUID active_lot_guid;
    GncLotScrubPlan *lot_child;
    guint64 children_generation;
    size_t child_index;
    guint completed_accounts;
};

static gboolean
structural_plan_deferral_valid (GncScrubContext *context)
{
    return g_getenv ("GNC_AUTO_SCRUB_LOTS") == nullptr ||
           gnc_scrub_context_commit_deferral_enabled (
               context, GNC_SCRUB_DEFERRED_COMMIT_GAINS);
}

static gboolean
account_lots_valid (GncAccountLotsPlan *plan)
{
    if (!plan || plan->state != GNC_ACCOUNT_LOTS_PLAN_RUNNING) return FALSE;
    if (gnc_scrub_context_is_cancelled (plan->context))
        plan->state = GNC_ACCOUNT_LOTS_PLAN_CANCELLED;
    else if (!gnc_scrub_context_owns_book (plan->context, plan->book))
        plan->state = GNC_ACCOUNT_LOTS_PLAN_STALE;
    else if (!structural_plan_deferral_valid (plan->context))
        plan->state = GNC_ACCOUNT_LOTS_PLAN_STALE;
    return plan->state == GNC_ACCOUNT_LOTS_PLAN_RUNNING;
}

static gboolean
lot_scrub_valid (GncLotScrubPlan *plan)
{
    if (!plan || plan->state != GNC_LOT_SCRUB_PLAN_RUNNING)
        return FALSE;
    if (gnc_scrub_context_is_cancelled (plan->context))
        plan->state = GNC_LOT_SCRUB_PLAN_CANCELLED;
    else if (!gnc_scrub_context_owns_book (plan->context, plan->book))
        plan->state = GNC_LOT_SCRUB_PLAN_STALE;
    else if (!structural_plan_deferral_valid (plan->context))
        plan->state = GNC_LOT_SCRUB_PLAN_STALE;
    else
    {
        auto lot = gnc_lot_lookup (&plan->lot_guid, plan->book);
        auto account = xaccAccountLookup (&plan->account_guid, plan->book);
        if (!lot || !account || gnc_lot_get_account (lot) != account)
            plan->state = GNC_LOT_SCRUB_PLAN_STALE;
        else if (xaccAccountIsAPARType (xaccAccountGetType (account)))
            plan->state = GNC_LOT_SCRUB_PLAN_STALE;
        else if (gnc_lot_get_scrub_generation (lot) !=
                 plan->lot_generation)
            plan->state = GNC_LOT_SCRUB_PLAN_STALE;
    }
    return plan->state == GNC_LOT_SCRUB_PLAN_RUNNING;
}

enum class SplitAssignPhase
{
    TRADE_SCAN_START,
    TRADE_SCAN,
    START_SCAN,
    NEXT_LOT,
    LOT_STATS,
    CREATE_LOT,
    APPLY,
};

struct GncSplitAssignPlan
{
    GncScrubContext *context;
    QofBook *book;
    GncGUID split_guid;
    GncGUID account_guid;
    GncAccountTradesPlan *trades;
    guint64 account_scrub_generation;
    GncAccountLotCursor *lot_cursor;
    GncLotStatsPlan *lot_stats;
    GncGUID stats_lot_guid;
    GncGUID best_lot_guid;
    GncGUID best_opening_guid;
    gnc_numeric best_balance;
    guint best_count;
    guint64 best_generation;
    gboolean has_best;
    SplitAssignPhase phase;
    GncSplitAssignPlanState state;
};

static gboolean
split_assign_valid (GncSplitAssignPlan *plan)
{
    if (!plan || plan->state != GNC_SPLIT_ASSIGN_PLAN_RUNNING)
        return FALSE;
    if (gnc_scrub_context_is_cancelled (plan->context))
    {
        plan->state = GNC_SPLIT_ASSIGN_PLAN_CANCELLED;
        return FALSE;
    }
    if (!gnc_scrub_context_owns_book (plan->context, plan->book))
    {
        plan->state = GNC_SPLIT_ASSIGN_PLAN_STALE;
        return FALSE;
    }
    if (!structural_plan_deferral_valid (plan->context))
    {
        plan->state = GNC_SPLIT_ASSIGN_PLAN_STALE;
        return FALSE;
    }
    auto split = xaccSplitLookup (&plan->split_guid, plan->book);
    auto account = xaccAccountLookup (&plan->account_guid, plan->book);
    if (!split || !account || xaccSplitGetAccount (split) != account)
    {
        plan->state = GNC_SPLIT_ASSIGN_PLAN_STALE;
        return FALSE;
    }
    if (plan->phase != SplitAssignPhase::TRADE_SCAN_START &&
        plan->phase != SplitAssignPhase::TRADE_SCAN &&
        gnc_account_get_scrub_generation (account) !=
            plan->account_scrub_generation)
    {
        plan->state = GNC_SPLIT_ASSIGN_PLAN_STALE;
        return FALSE;
    }
    return TRUE;
}

static void
split_assign_reset_scan (GncSplitAssignPlan *plan)
{
    gnc_account_trades_plan_free (plan->trades);
    gnc_account_lot_cursor_free (plan->lot_cursor);
    gnc_lot_stats_plan_free (plan->lot_stats);
    plan->trades = nullptr;
    plan->lot_cursor = nullptr;
    plan->lot_stats = nullptr;
    plan->account_scrub_generation = 0;
    plan->has_best = FALSE;
    plan->best_lot_guid = *guid_null ();
    plan->best_opening_guid = *guid_null ();
    plan->phase = SplitAssignPhase::TRADE_SCAN_START;
}

static GncSplitAssignPlan *
split_assign_plan_begin_internal (Split *split, GncScrubContext *context,
                                  gboolean trades_prepared,
                                  guint64 account_scrub_generation)
{
    if (!split || !context || xaccSplitGetLot (split))
        return nullptr;
    auto account = xaccSplitGetAccount (split);
    auto book = qof_instance_get_book (QOF_INSTANCE (split));
    if (!account || !gnc_scrub_context_owns_book (context, book))
        return nullptr;
    if (!structural_plan_deferral_valid (context))
        return nullptr;
    if (trades_prepared && gnc_account_get_scrub_generation (account) !=
                               account_scrub_generation)
        return nullptr;
    return new GncSplitAssignPlan {
        gnc_scrub_context_ref (context), book,
        *qof_instance_get_guid (QOF_INSTANCE (split)),
        *qof_instance_get_guid (QOF_INSTANCE (account)), nullptr,
        trades_prepared ? account_scrub_generation : 0, nullptr, nullptr,
        *guid_null (), *guid_null (), *guid_null (), gnc_numeric_zero (), 0, 0,
        FALSE, trades_prepared ? SplitAssignPhase::START_SCAN
                               : SplitAssignPhase::TRADE_SCAN_START,
        GNC_SPLIT_ASSIGN_PLAN_RUNNING};
}

GncSplitAssignPlan *
gnc_split_assign_plan_begin (Split *split, GncScrubContext *context)
{
    return split_assign_plan_begin_internal (split, context, FALSE, 0);
}

static gboolean
split_assign_consider_stats (GncSplitAssignPlan *plan)
{
    gnc_numeric balance, value;
    guint count;
    GncGUID earliest, latest, currency_guid;
    if (!gnc_lot_stats_plan_get_result (plan->lot_stats, &balance, &value,
                                        &count, &earliest, &latest,
                                        &currency_guid))
        return FALSE;
    auto split = xaccSplitLookup (&plan->split_guid, plan->book);
    auto lot = gnc_lot_lookup (&plan->stats_lot_guid, plan->book);
    auto currency = split ? xaccTransGetCurrency (xaccSplitGetParent (split)) : nullptr;
    auto lot_currency = gnc_commodity_find_commodity_by_guid (&currency_guid,
                                                               plan->book);
    if (split && lot && count && !gnc_numeric_zero_p (balance) && currency &&
        lot_currency && gnc_commodity_equiv (currency, lot_currency) &&
        gnc_numeric_positive_p (balance) !=
            gnc_numeric_positive_p (xaccSplitGetAmount (split)))
    {
        auto opening = xaccSplitLookup (&earliest, plan->book);
        auto best = xaccSplitLookup (&plan->best_opening_guid, plan->book);
        if (opening && (!plan->has_best ||
                        gnc_policy_candidate_is_better (
                            gnc_account_get_policy (xaccSplitGetAccount (split)),
                            opening, best)))
        {
            plan->has_best = TRUE;
            plan->best_lot_guid = plan->stats_lot_guid;
            plan->best_opening_guid = earliest;
            plan->best_balance = balance;
            plan->best_count = count;
            plan->best_generation = gnc_lot_get_scrub_generation (lot);
        }
    }
    gnc_lot_stats_plan_free (plan->lot_stats);
    plan->lot_stats = nullptr;
    plan->phase = SplitAssignPhase::NEXT_LOT;
    return TRUE;
}

static gboolean
split_assign_one (GncSplitAssignPlan *plan)
{
    auto split = xaccSplitLookup (&plan->split_guid, plan->book);
    auto account = xaccAccountLookup (&plan->account_guid, plan->book);
    if (!split || !account)
        return FALSE;
    switch (plan->phase)
    {
    case SplitAssignPhase::TRADE_SCAN_START:
        if (xaccSplitGetLot (split) ||
            gnc_numeric_zero_p (xaccSplitGetAmount (split)))
        {
            plan->state = GNC_SPLIT_ASSIGN_PLAN_DONE;
            return TRUE;
        }
        plan->trades = gnc_account_trades_plan_begin (account, plan->context);
        if (!plan->trades) return FALSE;
        plan->phase = SplitAssignPhase::TRADE_SCAN;
        return TRUE;

    case SplitAssignPhase::TRADE_SCAN:
    {
        auto state = gnc_account_trades_plan_step (plan->trades, 1);
        if (state == GNC_ACCOUNT_TRADES_PLAN_RUNNING) return TRUE;
        gboolean has_trades = FALSE;
        auto done = state == GNC_ACCOUNT_TRADES_PLAN_DONE &&
                    gnc_account_trades_plan_get_result (plan->trades,
                                                        &has_trades);
        gnc_account_trades_plan_free (plan->trades);
        plan->trades = nullptr;
        if (!done)
        {
            plan->state = state == GNC_ACCOUNT_TRADES_PLAN_CANCELLED
                ? GNC_SPLIT_ASSIGN_PLAN_CANCELLED : GNC_SPLIT_ASSIGN_PLAN_STALE;
            return TRUE;
        }
        if (!has_trades)
        {
            plan->state = GNC_SPLIT_ASSIGN_PLAN_DONE;
            return TRUE;
        }
        plan->account_scrub_generation =
            gnc_account_get_scrub_generation (account);
        plan->phase = SplitAssignPhase::START_SCAN;
        return TRUE;
    }

    case SplitAssignPhase::START_SCAN:
        if (xaccSplitGetLot (split) ||
            gnc_numeric_zero_p (xaccSplitGetAmount (split)))
        {
            plan->state = GNC_SPLIT_ASSIGN_PLAN_DONE;
            return TRUE;
        }
        split->gains |= GAINS_STATUS_VDIRTY;
        plan->lot_cursor = gnc_account_lot_cursor_begin (account, plan->context);
        if (!plan->lot_cursor) return FALSE;
        plan->phase = SplitAssignPhase::NEXT_LOT;
        return TRUE;

    case SplitAssignPhase::NEXT_LOT:
    {
        GncGUID guid;
        auto state = gnc_account_lot_cursor_next (plan->lot_cursor, &guid);
        if (state == GNC_ACCOUNT_LOT_CURSOR_CANCELLED)
        {
            plan->state = GNC_SPLIT_ASSIGN_PLAN_CANCELLED;
            return TRUE;
        }
        if (state == GNC_ACCOUNT_LOT_CURSOR_STALE)
        {
            plan->state = GNC_SPLIT_ASSIGN_PLAN_STALE;
            return TRUE;
        }
        if (state == GNC_ACCOUNT_LOT_CURSOR_DONE)
        {
            gnc_account_lot_cursor_free (plan->lot_cursor);
            plan->lot_cursor = nullptr;
            plan->phase = plan->has_best ? SplitAssignPhase::APPLY
                                         : SplitAssignPhase::CREATE_LOT;
            return TRUE;
        }
        auto lot = gnc_lot_lookup (&guid, plan->book);
        if (!lot) return FALSE;
        plan->stats_lot_guid = guid;
        plan->lot_stats = gnc_lot_stats_plan_begin (lot, plan->context);
        if (!plan->lot_stats) return FALSE;
        plan->phase = SplitAssignPhase::LOT_STATS;
        return TRUE;
    }

    case SplitAssignPhase::LOT_STATS:
    {
        auto state = gnc_lot_stats_plan_step (plan->lot_stats, 1);
        if (state == GNC_LOT_STATS_RUNNING) return TRUE;
        if (state != GNC_LOT_STATS_DONE)
        {
            plan->state = state == GNC_LOT_STATS_CANCELLED
                ? GNC_SPLIT_ASSIGN_PLAN_CANCELLED : GNC_SPLIT_ASSIGN_PLAN_STALE;
            return TRUE;
        }
        return split_assign_consider_stats (plan);
    }

    case SplitAssignPhase::CREATE_LOT:
    {
        auto lot = gnc_lot_make_default (account);
        if (!lot) return FALSE;
        plan->best_lot_guid = *qof_instance_get_guid (QOF_INSTANCE (lot));
        plan->best_balance = gnc_numeric_zero ();
        plan->best_count = 0;
        plan->best_generation = gnc_lot_get_scrub_generation (lot);
        plan->has_best = TRUE;
        plan->account_scrub_generation =
            gnc_account_get_scrub_generation (account);
        plan->phase = SplitAssignPhase::APPLY;
        return TRUE;
    }

    case SplitAssignPhase::APPLY:
    {
        auto lot = gnc_lot_lookup (&plan->best_lot_guid, plan->book);
        if (!lot || gnc_lot_get_scrub_generation (lot) != plan->best_generation)
        {
            plan->state = GNC_SPLIT_ASSIGN_PLAN_STALE;
            return TRUE;
        }
        auto remainder = gnc_split_assign_to_lot_prepared (
            split, lot, plan->best_balance, plan->best_count != 0);
        if (remainder == split || !xaccSplitGetLot (split))
            return FALSE;
        if (!remainder)
        {
            plan->state = GNC_SPLIT_ASSIGN_PLAN_DONE;
            return TRUE;
        }
        plan->split_guid = *qof_instance_get_guid (QOF_INSTANCE (remainder));
        split_assign_reset_scan (plan);
        return TRUE;
    }
    }
    return FALSE;
}

GncSplitAssignPlanState
gnc_split_assign_plan_step (GncSplitAssignPlan *plan, guint max_work)
{
    if (!plan || plan->state != GNC_SPLIT_ASSIGN_PLAN_RUNNING || max_work == 0)
        return plan ? plan->state : GNC_SPLIT_ASSIGN_PLAN_FAILED;
    guint work = 0;
    while (work++ < max_work && plan->state == GNC_SPLIT_ASSIGN_PLAN_RUNNING)
    {
        if (!split_assign_valid (plan)) break;
        if (!split_assign_one (plan))
            plan->state = GNC_SPLIT_ASSIGN_PLAN_FAILED;
    }
    return plan->state;
}

GncSplitAssignPlanState
gnc_split_assign_plan_get_state (const GncSplitAssignPlan *plan)
{
    return plan ? plan->state : GNC_SPLIT_ASSIGN_PLAN_FAILED;
}

void gnc_split_assign_plan_cancel (GncSplitAssignPlan *plan)
{
    if (plan && plan->state == GNC_SPLIT_ASSIGN_PLAN_RUNNING)
        plan->state = GNC_SPLIT_ASSIGN_PLAN_CANCELLED;
}

void gnc_split_assign_plan_free (GncSplitAssignPlan *plan)
{
    if (!plan) return;
    gnc_account_trades_plan_free (plan->trades);
    gnc_account_lot_cursor_free (plan->lot_cursor);
    gnc_lot_stats_plan_free (plan->lot_stats);
    gnc_scrub_context_unref (plan->context);
    delete plan;
}

GncLotScrubPlan *
gnc_lot_scrub_plan_begin (GNCLot *lot, GncScrubContext *context)
{
    if (!lot || !context)
        return nullptr;
    auto account = gnc_lot_get_account (lot);
    auto book = qof_instance_get_book (QOF_INSTANCE (lot));
    if (!account || !gnc_scrub_context_owns_book (context, book) ||
        xaccAccountIsAPARType (xaccAccountGetType (account)))
        return nullptr;
    if (!structural_plan_deferral_valid (context))
        return nullptr;
    return new GncLotScrubPlan {
        gnc_scrub_context_ref (context), book,
        *qof_instance_get_guid (QOF_INSTANCE (lot)),
        *qof_instance_get_guid (QOF_INSTANCE (account)),
        GNC_LOT_SCRUB_PLAN_RUNNING, LotScrubPhase::MERGE_START,
        gnc_lot_get_scrub_generation (lot), nullptr,
        *guid_null (), nullptr, nullptr, gnc_numeric_zero (), gnc_numeric_zero (),
        0, *guid_null (), *guid_null (), *guid_null (), 0, 0, 0, *guid_null (),
        FALSE, FALSE, *guid_null (), *guid_null (), nullptr, FALSE, FALSE,
        FALSE, FALSE};
}

static gboolean
lot_scrub_start_raw_scan (GncLotScrubPlan *plan, LotScrubPhase phase)
{
    auto lot = gnc_lot_lookup (&plan->lot_guid, plan->book);
    if (!lot || gnc_lot_get_scrub_generation (lot) != plan->lot_generation)
        return FALSE;
    plan->lot_next = gnc_lot_get_split_list (lot);
    plan->phase = phase;
    return TRUE;
}

static gboolean
lot_scrub_accept_own_mutation (GncLotScrubPlan *plan)
{
    auto lot = gnc_lot_lookup (&plan->lot_guid, plan->book);
    if (!lot) return FALSE;
    plan->lot_generation = gnc_lot_get_scrub_generation (lot);
    return TRUE;
}

static gboolean
lot_scrub_raw_scan_valid (GncLotScrubPlan *plan)
{
    auto lot = gnc_lot_lookup (&plan->lot_guid, plan->book);
    return lot && gnc_lot_get_scrub_generation (lot) == plan->lot_generation;
}

static gboolean
lot_scrub_merge_one (GncLotScrubPlan *plan)
{
    auto lot = gnc_lot_lookup (&plan->lot_guid, plan->book);
    switch (plan->phase)
    {
    case LotScrubPhase::MERGE_START:
        return lot_scrub_start_raw_scan (plan, LotScrubPhase::MERGE_LOT_SCAN);
    case LotScrubPhase::MERGE_LOT_SCAN:
    {
        if (!lot_scrub_raw_scan_valid (plan)) return FALSE;
        if (!plan->lot_next)
        {
            plan->phase = LotScrubPhase::STATS_START;
            return TRUE;
        }
        auto keep = GNC_SPLIT (plan->lot_next->data);
        plan->lot_next = plan->lot_next->next;
        if (!keep || xaccSplitGetLot (keep) != lot) return FALSE;
        if (!xaccSplitHasPeers (keep)) return TRUE;
        plan->merge_keep_guid = *qof_instance_get_guid (QOF_INSTANCE (keep));
        plan->transaction_cursor = gnc_transaction_split_cursor_begin (
            xaccSplitGetParent (keep), plan->context);
        if (!plan->transaction_cursor) return FALSE;
        plan->phase = LotScrubPhase::MERGE_TRANSACTION_SCAN;
        return TRUE;
    }
    case LotScrubPhase::MERGE_TRANSACTION_SCAN:
    {
        GncGUID guid;
        auto state = gnc_transaction_split_cursor_next (plan->transaction_cursor,
                                                         &guid);
        if (state == GNC_TRANSACTION_SPLIT_CURSOR_CANCELLED)
        {
            plan->state = GNC_LOT_SCRUB_PLAN_CANCELLED;
            return TRUE;
        }
        if (state == GNC_TRANSACTION_SPLIT_CURSOR_STALE) return FALSE;
        if (state == GNC_TRANSACTION_SPLIT_CURSOR_DONE)
        {
            gnc_transaction_split_cursor_free (plan->transaction_cursor);
            plan->transaction_cursor = nullptr;
            plan->phase = LotScrubPhase::MERGE_LOT_SCAN;
            return TRUE;
        }
        auto keep = xaccSplitLookup (&plan->merge_keep_guid, plan->book);
        auto remove = xaccSplitLookup (&guid, plan->book);
        if (!keep || !remove) return FALSE;
        if (keep == remove || xaccSplitGetLot (remove) != lot ||
            qof_instance_get_destroying (remove) ||
            !xaccSplitIsPeerSplit (keep, remove))
            return TRUE;
        gnc_transaction_split_cursor_free (plan->transaction_cursor);
        plan->transaction_cursor = nullptr;
        if (!gnc_scrub_merge_split_pair_prepared (keep, remove, TRUE))
            return FALSE;
        if (!lot_scrub_accept_own_mutation (plan)) return FALSE;
        plan->splits_deleted = TRUE;
        plan->phase = LotScrubPhase::MERGE_START;
        return TRUE;
    }
    default: return FALSE;
    }
}

static gboolean
lot_scrub_stats_one (GncLotScrubPlan *plan, gboolean final)
{
    auto lot = gnc_lot_lookup (&plan->lot_guid, plan->book);
    if ((final && plan->phase == LotScrubPhase::FINAL_STATS_START) ||
        (!final && plan->phase == LotScrubPhase::STATS_START))
    {
        plan->stats = gnc_lot_stats_plan_begin (lot, plan->context);
        if (!plan->stats) return FALSE;
        plan->phase = final ? LotScrubPhase::FINAL_STATS : LotScrubPhase::STATS;
        return TRUE;
    }
    auto state = gnc_lot_stats_plan_step (plan->stats, 1);
    if (state == GNC_LOT_STATS_RUNNING) return TRUE;
    if (state != GNC_LOT_STATS_DONE) return FALSE;
    if (gnc_lot_get_scrub_generation (lot) != plan->lot_generation)
        return FALSE;
    if (!gnc_lot_stats_plan_get_result (
            plan->stats, &plan->balance, &plan->value, &plan->split_count,
            &plan->earliest_guid, &plan->latest_guid, &plan->currency_guid))
        return FALSE;
    gnc_lot_stats_plan_free (plan->stats);
    plan->stats = nullptr;
    if (final)
    {
        if (gnc_numeric_zero_p (plan->balance) &&
            !gnc_numeric_zero_p (plan->value))
        {
            PERR ("Closed lot fails to double-balance: value=%s",
                  gnc_num_dbg_to_string (plan->value));
            return FALSE;
        }
        plan->phase = LotScrubPhase::FINAL_VERIFY_START;
        return TRUE;
    }
    if (plan->split_count == 0)
    {
        plan->state = GNC_LOT_SCRUB_PLAN_DONE;
        return TRUE;
    }
    if (plan->fill_complete)
    {
        plan->filling = FALSE;
        plan->fill_complete = FALSE;
        plan->phase = LotScrubPhase::CAP_START;
        return TRUE;
    }
    if (gnc_numeric_zero_p (plan->balance))
    {
        if (plan->filling)
        {
            plan->fill_complete = TRUE;
            plan->phase = LotScrubPhase::MERGE_START;
        }
        else
            plan->phase = LotScrubPhase::CAP_START;
        return TRUE;
    }
    auto opening = xaccSplitLookup (&plan->earliest_guid, plan->book);
    if (!opening) return FALSE;
    if (gnc_numeric_positive_p (xaccSplitGetAmount (opening)) !=
        gnc_numeric_positive_p (plan->balance))
        return lot_scrub_start_raw_scan (plan,
                                         LotScrubPhase::RETHIN_SCAN);
    else
    {
        plan->filling = TRUE;
        plan->phase = LotScrubPhase::FILL_SCAN_START;
    }
    return TRUE;
}

static gboolean
lot_scrub_rethin_one (GncLotScrubPlan *plan)
{
    if (!lot_scrub_raw_scan_valid (plan)) return FALSE;
    auto lot = gnc_lot_lookup (&plan->lot_guid, plan->book);
    if (!plan->lot_next)
    {
        plan->phase = LotScrubPhase::STATS_START;
        return TRUE;
    }
    auto split = GNC_SPLIT (plan->lot_next->data);
    plan->lot_next = plan->lot_next->next;
    if (!split || xaccSplitGetLot (split) != lot) return FALSE;
    if (guid_equal (qof_instance_get_guid (QOF_INSTANCE (split)),
                    &plan->earliest_guid))
        return TRUE;
    gnc_lot_remove_split (lot, split);
    if (!lot_scrub_accept_own_mutation (plan)) return FALSE;
    return lot_scrub_start_raw_scan (plan,
                                     LotScrubPhase::RETHIN_SCAN);
}

static gboolean
lot_scrub_fill_one (GncLotScrubPlan *plan)
{
    auto account = xaccAccountLookup (&plan->account_guid, plan->book);
    auto lot = gnc_lot_lookup (&plan->lot_guid, plan->book);
    switch (plan->phase)
    {
    case LotScrubPhase::FILL_SCAN_START:
        plan->account_splits_generation = gnc_account_get_splits_generation (account);
        plan->account_scrub_generation = gnc_account_get_scrub_generation (account);
        plan->account_index = 0;
        plan->has_fill_best = FALSE;
        plan->fill_best_guid = *guid_null ();
        plan->phase = LotScrubPhase::FILL_SCAN;
        return TRUE;
    case LotScrubPhase::FILL_SCAN:
    {
        if (gnc_account_get_splits_generation (account) != plan->account_splits_generation ||
            gnc_account_get_scrub_generation (account) != plan->account_scrub_generation)
            return FALSE;
        if (plan->account_index >= xaccAccountGetSplitsSize (account))
        {
            if (plan->has_fill_best)
                plan->phase = LotScrubPhase::FILL_APPLY;
            else
            {
                plan->fill_complete = TRUE;
                plan->phase = LotScrubPhase::MERGE_START;
            }
            return TRUE;
        }
        GncGUID guid;
        if (!gnc_account_get_split_guid_at (account,
                plan->account_splits_generation, plan->account_index++, &guid))
            return FALSE;
        auto split = xaccSplitLookup (&guid, plan->book);
        auto latest = xaccSplitLookup (&plan->latest_guid, plan->book);
        auto lot_currency = gnc_commodity_find_commodity_by_guid (
            &plan->currency_guid, plan->book);
        if (!split || !latest || !lot_currency) return FALSE;
        auto transaction = xaccSplitGetParent (split);
        if (xaccSplitGetLot (split) ||
            (gnc_numeric_zero_p (xaccSplitGetAmount (split)) &&
             xaccTransGetVoidStatus (transaction)) ||
            gnc_numeric_zero_p (xaccSplitGetAmount (split)) ||
            xaccTransRetDatePosted (transaction) <
                xaccTransRetDatePosted (xaccSplitGetParent (latest)) ||
            !gnc_commodity_equiv (lot_currency, xaccTransGetCurrency (transaction)) ||
            gnc_numeric_positive_p (xaccSplitGetAmount (split)) ==
                gnc_numeric_positive_p (plan->balance))
            return TRUE;
        auto best = xaccSplitLookup (&plan->fill_best_guid, plan->book);
        if (!plan->has_fill_best || gnc_policy_candidate_is_better (
                gnc_account_get_policy (account), split, best))
        {
            plan->has_fill_best = TRUE;
            plan->fill_best_guid = guid;
        }
        return TRUE;
    }
    case LotScrubPhase::FILL_APPLY:
    {
        auto split = xaccSplitLookup (&plan->fill_best_guid, plan->book);
        if (!split ||
            gnc_lot_get_scrub_generation (lot) != plan->lot_generation)
            return FALSE;
        auto remainder = gnc_split_assign_to_lot_prepared (
            split, lot, plan->balance, plan->split_count != 0);
        if (remainder == split || !xaccSplitGetLot (split)) return FALSE;
        if (!lot_scrub_accept_own_mutation (plan)) return FALSE;
        plan->phase = LotScrubPhase::STATS_START;
        return TRUE;
    }
    default: return FALSE;
    }
}

static gboolean
lot_scrub_cap_one (GncLotScrubPlan *plan)
{
    auto lot = gnc_lot_lookup (&plan->lot_guid, plan->book);
    auto account = xaccAccountLookup (&plan->account_guid, plan->book);
    switch (plan->phase)
    {
    case LotScrubPhase::CAP_START:
    {
        auto first = xaccSplitLookup (&plan->earliest_guid, plan->book);
        if (!first || !account) return FALSE;
        xaccSplitDetermineGainStatus (first);
        plan->mark_all_value_dirty = first->gains & GAINS_STATUS_VDIRTY;
        plan->value_dirty_marker_guid = plan->mark_all_value_dirty
            ? *qof_instance_get_guid (QOF_INSTANCE (first)) : *guid_null ();
        /* The opening VDIRTY bit is the durable FIFO work marker.  It stays
         * set until FINALIZE has proved that the complete lot and all gains
         * relationships are clean. */
        return lot_scrub_start_raw_scan (plan,
                                         LotScrubPhase::CAP_SCAN);
    }
    case LotScrubPhase::CAP_SCAN:
    {
        if (!lot_scrub_raw_scan_valid (plan)) return FALSE;
        if (!plan->lot_next)
        {
            plan->phase = LotScrubPhase::FINAL_STATS_START;
            return TRUE;
        }
        auto split = GNC_SPLIT (plan->lot_next->data);
        plan->lot_next = plan->lot_next->next;
        if (!split || xaccSplitGetLot (split) != lot) return FALSE;
        xaccSplitDetermineGainStatus (split);
        auto source = xaccSplitGetGainsSourceSplit (split);
        if (!source) source = split;
        if (plan->mark_all_value_dirty &&
            guid_equal (qof_instance_get_guid (QOF_INSTANCE (source)),
                        &plan->value_dirty_marker_guid))
            return TRUE;
        if (plan->mark_all_value_dirty)
            source->gains |= GAINS_STATUS_VDIRTY;
        plan->cap_child = gnc_cap_gains_plan_begin (source, nullptr,
                                                    plan->context);
        if (!plan->cap_child) return FALSE;
        plan->phase = LotScrubPhase::CAP_CHILD;
        return TRUE;
    }
    case LotScrubPhase::CAP_CHILD:
    {
        auto old_generation = plan->lot_generation;
        auto state = gnc_cap_gains_plan_step (plan->cap_child, 1);
        auto current_generation = gnc_lot_get_scrub_generation (lot);
        if ((state == GNC_CAP_GAINS_PLAN_RUNNING ||
             state == GNC_CAP_GAINS_PLAN_DONE) &&
            current_generation != old_generation)
        {
            plan->lot_generation = current_generation;
            plan->cap_restart_scan = TRUE;
        }
        if (state == GNC_CAP_GAINS_PLAN_RUNNING) return TRUE;
        gnc_cap_gains_plan_free (plan->cap_child);
        plan->cap_child = nullptr;
        if (state != GNC_CAP_GAINS_PLAN_DONE) return FALSE;
        if (plan->cap_restart_scan)
        {
            plan->cap_restart_scan = FALSE;
            return lot_scrub_start_raw_scan (plan,
                                             LotScrubPhase::CAP_SCAN);
        }
        plan->phase = LotScrubPhase::CAP_SCAN;
        return TRUE;
    }
    default: return FALSE;
    }
}

static gboolean
lot_scrub_unresolved_value_dirty (GncLotScrubPlan *plan, Split *split)
{
    auto dirty = split ? split->gains & GAINS_STATUS_VDIRTY : 0;
    if (split && plan->mark_all_value_dirty &&
        guid_equal (qof_instance_get_guid (QOF_INSTANCE (split)),
                    &plan->value_dirty_marker_guid))
        dirty &= ~GAINS_STATUS_VDIRTY;
    return dirty != 0;
}

static gboolean
lot_scrub_is_structural_marker (GncLotScrubPlan *plan, Split *split)
{
    return split && !guid_equal (&plan->structural_dirty_marker_guid,
                                 guid_null ()) &&
           guid_equal (qof_instance_get_guid (QOF_INSTANCE (split)),
                       &plan->structural_dirty_marker_guid);
}

static gboolean
lot_scrub_finalize_one (GncLotScrubPlan *plan)
{
    auto lot = gnc_lot_lookup (&plan->lot_guid, plan->book);
    switch (plan->phase)
    {
    case LotScrubPhase::FINAL_VERIFY_START:
        return lot_scrub_start_raw_scan (plan,
                                         LotScrubPhase::FINAL_VERIFY);
    case LotScrubPhase::FINAL_VERIFY:
    {
        if (!lot_scrub_raw_scan_valid (plan)) return FALSE;
        if (!plan->lot_next)
        {
            plan->phase = LotScrubPhase::FINAL_CLEAR_START;
            return TRUE;
        }
        auto split = GNC_SPLIT (plan->lot_next->data);
        plan->lot_next = plan->lot_next->next;
        if (!split || xaccSplitGetLot (split) != lot) return FALSE;
        xaccSplitDetermineGainStatus (split);
        if (lot_scrub_unresolved_value_dirty (plan, split))
            return FALSE;
        if ((split->gains & GAINS_STATUS_ADIRTY) &&
            guid_equal (&plan->structural_dirty_marker_guid, guid_null ()))
            plan->structural_dirty_marker_guid =
                *qof_instance_get_guid (QOF_INSTANCE (split));
        auto gains = split->gains_split;
        if (gains && gains != split)
        {
            if (lot_scrub_unresolved_value_dirty (plan, gains)) return FALSE;
            if ((gains->gains & GAINS_STATUS_ADIRTY) &&
                xaccSplitGetLot (gains) != lot)
                return FALSE;
        }
        return TRUE;
    }
    case LotScrubPhase::FINAL_CLEAR_START:
        return lot_scrub_start_raw_scan (plan,
                                         LotScrubPhase::FINAL_CLEAR);
    case LotScrubPhase::FINAL_CLEAR:
    {
        if (!lot_scrub_raw_scan_valid (plan)) return FALSE;
        if (!plan->lot_next)
        {
            plan->phase = LotScrubPhase::FINALIZE;
            return TRUE;
        }
        auto split = GNC_SPLIT (plan->lot_next->data);
        plan->lot_next = plan->lot_next->next;
        if (!split || xaccSplitGetLot (split) != lot) return FALSE;
        if (lot_scrub_unresolved_value_dirty (plan, split)) return FALSE;
        if (lot_scrub_is_structural_marker (plan, split)) return TRUE;
        if (split->gains & GAINS_STATUS_ADIRTY)
        {
            gnc_split_bump_scrub_generations (split);
            split->gains &= ~GAINS_STATUS_ADIRTY;
            if (!lot_scrub_accept_own_mutation (plan)) return FALSE;
        }
        return TRUE;
    }
    case LotScrubPhase::FINALIZE:
    {
        if (!lot_scrub_raw_scan_valid (plan)) return FALSE;
        Split *value_marker = nullptr;
        Split *structural_marker = nullptr;
        if (plan->mark_all_value_dirty)
        {
            value_marker = xaccSplitLookup (&plan->value_dirty_marker_guid,
                                            plan->book);
            if (!value_marker || xaccSplitGetLot (value_marker) != lot ||
                !(value_marker->gains & GAINS_STATUS_VDIRTY))
                return FALSE;
        }
        if (!guid_equal (&plan->structural_dirty_marker_guid, guid_null ()))
        {
            structural_marker =
                xaccSplitLookup (&plan->structural_dirty_marker_guid,
                                 plan->book);
            if (!structural_marker ||
                xaccSplitGetLot (structural_marker) != lot ||
                !(structural_marker->gains & GAINS_STATUS_ADIRTY))
                return FALSE;
        }
        if (value_marker)
            gnc_split_bump_scrub_generations (value_marker);
        if (structural_marker && structural_marker != value_marker)
            gnc_split_bump_scrub_generations (structural_marker);
        if (value_marker)
            value_marker->gains &= ~GAINS_STATUS_VDIRTY;
        if (structural_marker)
            structural_marker->gains &= ~GAINS_STATUS_ADIRTY;
        plan->state = GNC_LOT_SCRUB_PLAN_DONE;
        return TRUE;
    }
    default:
        return FALSE;
    }
}

GncLotScrubPlanState
gnc_lot_scrub_plan_step (GncLotScrubPlan *plan, guint max_work)
{
    if (!plan || plan->state != GNC_LOT_SCRUB_PLAN_RUNNING || max_work == 0)
        return plan ? plan->state : GNC_LOT_SCRUB_PLAN_FAILED;
    guint work = 0;
    while (work++ < max_work && plan->state == GNC_LOT_SCRUB_PLAN_RUNNING)
    {
        if (!lot_scrub_valid (plan)) break;
        gboolean ok = FALSE;
        switch (plan->phase)
        {
        case LotScrubPhase::MERGE_START:
        case LotScrubPhase::MERGE_LOT_SCAN:
        case LotScrubPhase::MERGE_TRANSACTION_SCAN:
            ok = lot_scrub_merge_one (plan); break;
        case LotScrubPhase::STATS_START:
        case LotScrubPhase::STATS:
            ok = lot_scrub_stats_one (plan, FALSE); break;
        case LotScrubPhase::RETHIN_SCAN:
            ok = lot_scrub_rethin_one (plan); break;
        case LotScrubPhase::FILL_SCAN_START:
        case LotScrubPhase::FILL_SCAN:
        case LotScrubPhase::FILL_APPLY:
            ok = lot_scrub_fill_one (plan); break;
        case LotScrubPhase::CAP_START:
        case LotScrubPhase::CAP_SCAN:
        case LotScrubPhase::CAP_CHILD:
            ok = lot_scrub_cap_one (plan); break;
        case LotScrubPhase::FINAL_STATS_START:
        case LotScrubPhase::FINAL_STATS:
            ok = lot_scrub_stats_one (plan, TRUE); break;
        case LotScrubPhase::FINAL_VERIFY_START:
        case LotScrubPhase::FINAL_VERIFY:
        case LotScrubPhase::FINAL_CLEAR_START:
        case LotScrubPhase::FINAL_CLEAR:
        case LotScrubPhase::FINALIZE:
            ok = lot_scrub_finalize_one (plan); break;
        }
        if (!ok && plan->state == GNC_LOT_SCRUB_PLAN_RUNNING)
            plan->state = GNC_LOT_SCRUB_PLAN_STALE;
    }
    return plan->state;
}

GncLotScrubPlanState gnc_lot_scrub_plan_get_state (const GncLotScrubPlan *plan)
{
    return plan ? plan->state : GNC_LOT_SCRUB_PLAN_FAILED;
}
gboolean
gnc_lot_scrub_plan_get_splits_deleted (const GncLotScrubPlan *plan)
{
    return plan && plan->splits_deleted;
}


void gnc_lot_scrub_plan_cancel (GncLotScrubPlan *plan)
{
    if (plan && plan->state == GNC_LOT_SCRUB_PLAN_RUNNING)
        plan->state = GNC_LOT_SCRUB_PLAN_CANCELLED;
}

void gnc_lot_scrub_plan_free (GncLotScrubPlan *plan)
{
    if (!plan) return;
    gnc_transaction_split_cursor_free (plan->transaction_cursor);
    gnc_lot_stats_plan_free (plan->stats);
    gnc_cap_gains_plan_free (plan->cap_child);
    gnc_scrub_context_unref (plan->context);
    delete plan;
}

GncAccountLotsPlan *
gnc_account_lots_plan_begin (Account *account, gboolean descendants,
                             GncScrubContext *context)
{
    if (!account || !context)
        return nullptr;
    auto book = qof_instance_get_book (QOF_INSTANCE (account));
    if (!gnc_scrub_context_owns_book (context, book))
        return nullptr;
    if (!structural_plan_deferral_valid (context))
        return nullptr;
    auto plan = new GncAccountLotsPlan {
        gnc_scrub_context_ref (context), book,
        *qof_instance_get_guid (QOF_INSTANCE (account)), descendants,
        GNC_ACCOUNT_LOTS_PLAN_RUNNING, AccountLotsPhase::NEXT_ACCOUNT, {},
        *guid_null (), 0, 0, 0, nullptr, FALSE, {}, *guid_null (), nullptr,
        nullptr, {}, *guid_null (), nullptr,
        0, 0, 0};
    plan->account_queue.push_back (plan->root_guid);
    return plan;
}

static gboolean
account_lots_next_account (GncAccountLotsPlan *plan)
{
    if (plan->account_queue.empty ())
    {
        plan->state = GNC_ACCOUNT_LOTS_PLAN_DONE;
        return TRUE;
    }
    plan->account_guid = plan->account_queue.front ();
    plan->account_queue.pop_front ();
    auto account = xaccAccountLookup (&plan->account_guid, plan->book);
    if (!account) return FALSE;
    plan->completed_splits.clear ();
    plan->completed_lots.clear ();
    plan->scrub_generation = gnc_account_get_scrub_generation (account);
    plan->phase = xaccAccountIsAPARType (xaccAccountGetType (account))
        ? AccountLotsPhase::CHILD_SCAN_START
        : AccountLotsPhase::TRADE_SCAN_START;
    return TRUE;
}

static gboolean
account_lots_trades_one (GncAccountLotsPlan *plan)
{
    auto account = xaccAccountLookup (&plan->account_guid, plan->book);
    if (!account || gnc_account_get_scrub_generation (account) !=
                        plan->scrub_generation)
        return FALSE;
    if (plan->phase == AccountLotsPhase::TRADE_SCAN_START)
    {
        if (xaccAccountIsAPARType (xaccAccountGetType (account)))
        {
            plan->phase = AccountLotsPhase::CHILD_SCAN_START;
            return TRUE;
        }
        plan->trades_child = gnc_account_trades_plan_begin (account,
                                                            plan->context);
        if (!plan->trades_child) return FALSE;
        plan->phase = AccountLotsPhase::TRADE_SCAN;
        return TRUE;
    }
    auto state = gnc_account_trades_plan_step (plan->trades_child, 1);
    if (state == GNC_ACCOUNT_TRADES_PLAN_RUNNING) return TRUE;
    auto done = state == GNC_ACCOUNT_TRADES_PLAN_DONE &&
                gnc_account_trades_plan_get_result (plan->trades_child,
                                                    &plan->has_trades);
    gnc_account_trades_plan_free (plan->trades_child);
    plan->trades_child = nullptr;
    if (!done) return FALSE;
    plan->scrub_generation = gnc_account_get_scrub_generation (account);
    if (!plan->has_trades)
    {
        plan->phase = AccountLotsPhase::CHILD_SCAN_START;
        return TRUE;
    }
    plan->split_generation = gnc_account_get_splits_generation (account);
    plan->split_index = 0;
    plan->phase = AccountLotsPhase::SPLIT_SCAN;
    return TRUE;
}

static gboolean
account_lots_split_one (GncAccountLotsPlan *plan)
{
    auto account = xaccAccountLookup (&plan->account_guid, plan->book);
    switch (plan->phase)
    {
    case AccountLotsPhase::SPLIT_SCAN_START:
        plan->split_generation = gnc_account_get_splits_generation (account);
        plan->scrub_generation = gnc_account_get_scrub_generation (account);
        plan->split_index = 0;
        plan->phase = AccountLotsPhase::SPLIT_SCAN;
        return TRUE;
    case AccountLotsPhase::SPLIT_SCAN:
    {
        if (gnc_account_get_splits_generation (account) != plan->split_generation ||
            gnc_account_get_scrub_generation (account) != plan->scrub_generation)
            return FALSE;
        if (plan->split_index >= xaccAccountGetSplitsSize (account))
        {
            plan->phase = AccountLotsPhase::LOT_SCAN_START;
            return TRUE;
        }
        GncGUID guid;
        if (!gnc_account_get_split_guid_at (account, plan->split_generation,
                                            plan->split_index++, &guid))
            return FALSE;
        auto split = xaccSplitLookup (&guid, plan->book);
        if (!split || xaccSplitGetAccount (split) != account) return FALSE;
        if (plan->completed_splits.contains (guid)) return TRUE;
        if (xaccSplitGetLot (split) ||
            (gnc_numeric_zero_p (xaccSplitGetAmount (split)) &&
             xaccTransGetVoidStatus (xaccSplitGetParent (split))))
            return TRUE;
        plan->assign_child = split_assign_plan_begin_internal (
            split, plan->context, TRUE, plan->scrub_generation);
        if (!plan->assign_child)
            return FALSE;
        plan->active_split_guid = guid;
        plan->phase = AccountLotsPhase::SPLIT_CHILD;
        return TRUE;
    }
    case AccountLotsPhase::SPLIT_CHILD:
    {
        auto state = gnc_split_assign_plan_step (plan->assign_child, 1);
        if (state == GNC_SPLIT_ASSIGN_PLAN_RUNNING) return TRUE;
        gnc_split_assign_plan_free (plan->assign_child);
        plan->assign_child = nullptr;
        if (state != GNC_SPLIT_ASSIGN_PLAN_DONE) return FALSE;
        plan->completed_splits.insert (plan->active_split_guid);
        auto current = xaccAccountLookup (&plan->account_guid, plan->book);
        if (!current) return FALSE;
        plan->split_generation = gnc_account_get_splits_generation (current);
        plan->scrub_generation = gnc_account_get_scrub_generation (current);
        plan->split_index = 0;
        plan->phase = AccountLotsPhase::SPLIT_SCAN;
        return TRUE;
    }
    default: return FALSE;
    }
}

static gboolean
account_lots_lot_one (GncAccountLotsPlan *plan)
{
    auto account = xaccAccountLookup (&plan->account_guid, plan->book);
    switch (plan->phase)
    {
    case AccountLotsPhase::LOT_SCAN_START:
        if (gnc_account_get_scrub_generation (account) !=
            plan->scrub_generation)
            return FALSE;
        gnc_account_lot_cursor_free (plan->lot_cursor);
        plan->lot_cursor = gnc_account_lot_cursor_begin (account,
                                                         plan->context);
        if (!plan->lot_cursor) return FALSE;
        plan->phase = AccountLotsPhase::LOT_SCAN;
        return TRUE;
    case AccountLotsPhase::LOT_SCAN:
    {
        if (gnc_account_get_scrub_generation (account) !=
            plan->scrub_generation)
            return FALSE;
        GncGUID guid;
        auto state = gnc_account_lot_cursor_next (plan->lot_cursor, &guid);
        if (state == GNC_ACCOUNT_LOT_CURSOR_CANCELLED)
        {
            plan->state = GNC_ACCOUNT_LOTS_PLAN_CANCELLED;
            return TRUE;
        }
        if (state == GNC_ACCOUNT_LOT_CURSOR_STALE) return FALSE;
        if (state == GNC_ACCOUNT_LOT_CURSOR_DONE)
        {
            gnc_account_lot_cursor_free (plan->lot_cursor);
            plan->lot_cursor = nullptr;
            plan->phase = AccountLotsPhase::CHILD_SCAN_START;
            return TRUE;
        }
        if (plan->completed_lots.contains (guid)) return TRUE;
        auto lot = gnc_lot_lookup (&guid, plan->book);
        if (!lot) return FALSE;
        plan->active_lot_guid = guid;
        plan->lot_child = gnc_lot_scrub_plan_begin (lot, plan->context);
        if (!plan->lot_child) return FALSE;
        plan->phase = AccountLotsPhase::LOT_CHILD;
        return TRUE;
    }
    case AccountLotsPhase::LOT_CHILD:
    {
        auto state = gnc_lot_scrub_plan_step (plan->lot_child, 1);
        if (state == GNC_LOT_SCRUB_PLAN_RUNNING) return TRUE;
        gnc_lot_scrub_plan_free (plan->lot_child);
        plan->lot_child = nullptr;
        if (state != GNC_LOT_SCRUB_PLAN_DONE) return FALSE;
        plan->completed_lots.insert (plan->active_lot_guid);
        plan->scrub_generation = gnc_account_get_scrub_generation (account);
        plan->phase = AccountLotsPhase::LOT_SCAN_START;
        return TRUE;
    }
    default: return FALSE;
    }
}

static gboolean
account_lots_children_one (GncAccountLotsPlan *plan)
{
    auto account = xaccAccountLookup (&plan->account_guid, plan->book);
    if (plan->phase == AccountLotsPhase::CHILD_SCAN_START)
    {
        if (gnc_account_get_scrub_generation (account) !=
            plan->scrub_generation)
            return FALSE;
        if (!plan->descendants)
        {
            ++plan->completed_accounts;
            plan->phase = AccountLotsPhase::NEXT_ACCOUNT;
            return TRUE;
        }
        plan->children_generation = gnc_account_get_children_generation (account);
        plan->child_index = 0;
        plan->phase = AccountLotsPhase::CHILD_SCAN;
        return TRUE;
    }
    if (gnc_account_get_children_generation (account) !=
            plan->children_generation ||
        gnc_account_get_scrub_generation (account) != plan->scrub_generation)
        return FALSE;
    if (plan->child_index >= static_cast<size_t> (gnc_account_n_children (account)))
    {
        ++plan->completed_accounts;
        plan->phase = AccountLotsPhase::NEXT_ACCOUNT;
        return TRUE;
    }
    GncGUID guid;
    if (!gnc_account_get_child_guid_at (account, plan->children_generation,
                                        plan->child_index++, &guid))
        return FALSE;
    plan->account_queue.push_back (guid);
    return TRUE;
}

GncAccountLotsPlanState
gnc_account_lots_plan_step (GncAccountLotsPlan *plan, guint max_work)
{
    if (!plan || plan->state != GNC_ACCOUNT_LOTS_PLAN_RUNNING || max_work == 0)
        return plan ? plan->state : GNC_ACCOUNT_LOTS_PLAN_FAILED;
    guint work = 0;
    while (work++ < max_work && plan->state == GNC_ACCOUNT_LOTS_PLAN_RUNNING)
    {
        if (!account_lots_valid (plan)) break;
        gboolean ok = FALSE;
        switch (plan->phase)
        {
        case AccountLotsPhase::NEXT_ACCOUNT:
            ok = account_lots_next_account (plan); break;
        case AccountLotsPhase::TRADE_SCAN_START:
        case AccountLotsPhase::TRADE_SCAN:
            ok = account_lots_trades_one (plan); break;
        case AccountLotsPhase::SPLIT_SCAN_START:
        case AccountLotsPhase::SPLIT_SCAN:
        case AccountLotsPhase::SPLIT_CHILD:
            ok = account_lots_split_one (plan); break;
        case AccountLotsPhase::LOT_SCAN_START:
        case AccountLotsPhase::LOT_SCAN:
        case AccountLotsPhase::LOT_CHILD:
            ok = account_lots_lot_one (plan); break;
        case AccountLotsPhase::CHILD_SCAN_START:
        case AccountLotsPhase::CHILD_SCAN:
            ok = account_lots_children_one (plan); break;
        }
        if (!ok && plan->state == GNC_ACCOUNT_LOTS_PLAN_RUNNING)
            plan->state = GNC_ACCOUNT_LOTS_PLAN_STALE;
    }
    return plan->state;
}

guint gnc_account_lots_plan_get_completed (const GncAccountLotsPlan *plan)
{
    return plan ? plan->completed_accounts : 0;
}

void gnc_account_lots_plan_cancel (GncAccountLotsPlan *plan)
{
    if (plan && plan->state == GNC_ACCOUNT_LOTS_PLAN_RUNNING)
        plan->state = GNC_ACCOUNT_LOTS_PLAN_CANCELLED;
}

void gnc_account_lots_plan_free (GncAccountLotsPlan *plan)
{
    if (!plan) return;
    gnc_account_trades_plan_free (plan->trades_child);
    gnc_split_assign_plan_free (plan->assign_child);
    gnc_account_lot_cursor_free (plan->lot_cursor);
    gnc_lot_scrub_plan_free (plan->lot_child);
    gnc_scrub_context_unref (plan->context);
    delete plan;
}

/* ================================================================= */
/** Cap gains are possible only if the lot commodity is not the same
 * as the transaction currency.  We assume here that all splits in
 * the lot share the same transaction currency, and so we look at
 * the first split, and see what it's currency is.
 * This routine returns TRUE if cap gains are possible.
 */

static inline gboolean
gains_possible (GNCLot *lot)
{
    SplitList *node;
    Account *acc;
    Split *split;
    gboolean comeq;
    gnc_commodity *acc_commodity;

    acc = gnc_lot_get_account (lot);

    node = gnc_lot_get_split_list (lot);
    if (!node) return FALSE;
    split = GNC_SPLIT(node->data);

    acc_commodity = xaccAccountGetCommodity(acc);
    comeq = gnc_commodity_equiv (acc_commodity, split->parent->common_currency);
    return (FALSE == comeq);
}

/* ================================================================= */
/* XXX What happens if, as a result of scrubbing, the lot is empty?
 * I don't think this is handled properly.  I think that what will
 * happen is we'll end up with an empty, closed lot ... ?
 */

gboolean
xaccScrubLotInternal (GNCLot *lot, GncScrubContext *context)
{
    gboolean splits_deleted = FALSE;
    gnc_numeric lot_baln;
    gboolean opening_baln_is_pos, lot_baln_is_pos;
    Account *acc;
    GNCPolicy *pcy;

    if (!lot) return FALSE;
    if (gnc_scrub_context_is_cancelled (context)) return FALSE;

    ENTER ("(lot=%p) %s", lot, gnc_lot_get_title(lot));

    acc = gnc_lot_get_account (lot);
    pcy = gnc_account_get_policy(acc);
    xaccAccountBeginEdit(acc);
    xaccScrubMergeLotSubSplits (lot, TRUE);

    /* If the lot balance is zero, we don't need to rebalance */
    lot_baln = gnc_lot_get_balance (lot);
    PINFO ("lot baln=%s for %s", gnc_num_dbg_to_string (lot_baln),
           gnc_lot_get_title(lot));
    if (! gnc_numeric_zero_p (lot_baln))
    {
        SplitList *node;
        gnc_numeric opening_baln;

        /* Get the opening balance for this lot */
        pcy->PolicyGetLotOpening (pcy, lot, &opening_baln, nullptr, nullptr);
        PINFO ("lot opener baln=%s", gnc_num_dbg_to_string (opening_baln));

        /* If the lot is fat, give the boot to all the non-opening
         * splits, and refill it */
        opening_baln_is_pos = gnc_numeric_positive_p(opening_baln);
        lot_baln_is_pos = gnc_numeric_positive_p(lot_baln);
        if ((opening_baln_is_pos || lot_baln_is_pos) &&
                ((!opening_baln_is_pos) || (!lot_baln_is_pos)))
        {
rethin:
            for (node = gnc_lot_get_split_list(lot); node; node = node->next)
            {
                Split *s = GNC_SPLIT(node->data);
                if (pcy->PolicyIsOpeningSplit (pcy, lot, s)) continue;
                gnc_lot_remove_split (lot, s);
                goto rethin;
            }
        }

        /* At this point the lot is thin, so try to fill it */
        xaccLotFill (lot);

        /* Make sure there are no subsplits. */
        splits_deleted = xaccScrubMergeLotSubSplits (lot, TRUE);
    }

    /* Now re-compute cap gains, and then double-check that.
     * But we only compute cap-gains if gains are possible;
     * that is if the lot commodity is not the same as the
     * currency. That is, one can't possibly have gains
     * selling dollars for dollars.  The business modules
     * use lots with lot commodity == lot currency.
     */
    if (gains_possible (lot))
    {
        xaccLotComputeCapGains (lot, nullptr);
        xaccLotScrubDoubleBalance (lot);
    }
    xaccAccountCommitEdit(acc);

    LEAVE ("(lot=%s, deleted=%d)", gnc_lot_get_title(lot), splits_deleted);
    return splits_deleted;
}

gboolean
xaccScrubLot (GNCLot *lot)
{
    auto book = lot ? qof_instance_get_book (QOF_INSTANCE (lot)) : nullptr;
    if (!lot || !gnc_scrub_legacy_operation_allowed (book, "lot scrub"))
        return FALSE;
    if (!gnc_current_session_exist () ||
        qof_session_get_book (gnc_get_current_session ()) != book)
        return xaccScrubLotInternal (lot, nullptr);
    auto job = gnc_scrub_lot_job_begin (lot);
    if (!job)
        return FALSE;
    auto state = GNC_SCRUB_JOB_RUNNING;
    while (state == GNC_SCRUB_JOB_RUNNING)
        state = gnc_scrub_job_step (job, 1);
    auto changed = state == GNC_SCRUB_JOB_DONE &&
                   gnc_scrub_job_get_changed (job);
    gnc_scrub_job_free (job);
    return changed;
}

gboolean
xaccScrubLotWithContext (GNCLot *lot, GncScrubContext *context)
{
    auto book = lot ? qof_instance_get_book (QOF_INSTANCE (lot)) : nullptr;
    if (!lot || !gnc_scrub_context_validate_for_book (
                    context, book, "lot scrub"))
        return FALSE;
    auto plan = gnc_lot_scrub_plan_begin (lot, context);
    if (!plan)
        return FALSE;
    auto state = GNC_LOT_SCRUB_PLAN_RUNNING;
    while (state == GNC_LOT_SCRUB_PLAN_RUNNING)
        state = gnc_lot_scrub_plan_step (plan, 1);
    auto changed = state == GNC_LOT_SCRUB_PLAN_DONE &&
                   gnc_lot_scrub_plan_get_splits_deleted (plan);
    gnc_lot_scrub_plan_free (plan);
    return changed;
}

/* ============================================================== */

static void
AccountScrubLots (Account *acc, GncScrubContext *context)
{
    LotList *lots, *node;
    if (!acc) return;
    if (gnc_scrub_context_is_cancelled (context)) return;
    if (FALSE == xaccAccountHasTrades (acc)) return;

    ENTER ("(acc=%s)", xaccAccountGetName(acc));
    xaccAccountBeginEdit(acc);
    xaccAccountAssignLots (acc);

    lots = xaccAccountGetLotList(acc);
    for (node = lots; node; node = node->next)
    {
        GNCLot *lot = GNC_LOT(node->data);
        if (gnc_scrub_context_is_cancelled (context))
            break;
        xaccScrubLotInternal (lot, context);
    }
    g_list_free(lots);
    xaccAccountCommitEdit(acc);
    LEAVE ("(acc=%s)", xaccAccountGetName(acc));
}

/* ============================================================== */

struct LotTreeScrubData
{
    GncScrubContext *context;
};

static void
lot_scrub_cb (Account *acc, gpointer data)
{
    auto scrub_data = static_cast<LotTreeScrubData *> (data);
    if (FALSE == xaccAccountHasTrades (acc)) return;
    if (gnc_scrub_context_is_cancelled (scrub_data->context)) return;
    AccountScrubLots (acc, scrub_data->context);
}

static void
AccountTreeScrubLots (Account *acc, GncScrubContext *context)
{
    if (!acc) return;
    LotTreeScrubData data {context};
    gnc_account_foreach_descendant (acc, lot_scrub_cb, &data);
    if (!gnc_scrub_context_is_cancelled (context))
        AccountScrubLots (acc, context);
}

void
xaccAccountScrubLots (Account *acc)
{
    auto book = acc ? qof_instance_get_book (QOF_INSTANCE (acc)) : nullptr;
    if (!acc || !gnc_scrub_legacy_operation_allowed (book, "account lot scrub"))
        return;
    if (!gnc_current_session_exist () ||
        qof_session_get_book (gnc_get_current_session ()) != book)
    {
        AccountScrubLots (acc, nullptr);
        return;
    }
    auto job = gnc_scrub_lots_job_begin (acc, FALSE);
    if (!job)
        return;
    while (gnc_scrub_job_step (job, 1) == GNC_SCRUB_JOB_RUNNING)
        ;
    gnc_scrub_job_free (job);
}

void
xaccAccountScrubLotsWithContext (Account *acc, GncScrubContext *context)
{
    auto book = acc ? qof_instance_get_book (QOF_INSTANCE (acc)) : nullptr;
    if (!acc || !gnc_scrub_context_validate_for_book (
                    context, book, "account lot scrub"))
        return;
    auto plan = gnc_account_lots_plan_begin (acc, FALSE, context);
    if (!plan)
        return;
    while (gnc_account_lots_plan_step (plan, 1) ==
           GNC_ACCOUNT_LOTS_PLAN_RUNNING)
        ;
    gnc_account_lots_plan_free (plan);
}

void
xaccAccountTreeScrubLots (Account *acc)
{
    auto book = acc ? qof_instance_get_book (QOF_INSTANCE (acc)) : nullptr;
    if (!acc || !gnc_scrub_legacy_operation_allowed (
                    book, "account-tree lot scrub"))
        return;
    if (!gnc_current_session_exist () ||
        qof_session_get_book (gnc_get_current_session ()) != book)
    {
        AccountTreeScrubLots (acc, nullptr);
        return;
    }
    auto job = gnc_scrub_lots_job_begin (acc, TRUE);
    if (!job)
        return;
    while (gnc_scrub_job_step (job, 1) == GNC_SCRUB_JOB_RUNNING)
        ;
    gnc_scrub_job_free (job);
}

void
xaccAccountTreeScrubLotsWithContext (Account *acc,
                                     GncScrubContext *context)
{
    auto book = acc ? qof_instance_get_book (QOF_INSTANCE (acc)) : nullptr;
    if (!acc || !gnc_scrub_context_validate_for_book (
                    context, book, "account-tree lot scrub"))
        return;
    auto plan = gnc_account_lots_plan_begin (acc, TRUE, context);
    if (!plan)
        return;
    while (gnc_account_lots_plan_step (plan, 1) ==
           GNC_ACCOUNT_LOTS_PLAN_RUNNING)
        ;
    gnc_account_lots_plan_free (plan);
}

/* ========================== END OF FILE  ========================= */
