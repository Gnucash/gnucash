/********************************************************************\
 * Scrub2.h -- Low-level Lot Management Routines.                   *
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

/** @addtogroup Engine
    @{ */
/** @addtogroup Scrub  Data Validation
    @{ */
/** @file Scrub2.h
 *  @brief Utilities to Convert Stock Accounts to use Lots
 *  @author Created by Linas Vepstas March 2003
 *  @author Copyright (c) 2003 Linas Vepstas <linas@linas.org>
 */

#ifndef XACC_SCRUB2_H
#define XACC_SCRUB2_H

#include "gnc-engine.h"

typedef struct GncScrubContext GncScrubContext;
typedef struct GncLotAssignmentPlan GncLotAssignmentPlan;
typedef struct GncLotStatsPlan GncLotStatsPlan;

#ifdef __cplusplus
extern "C" {
#endif

/** @name Lot Management Routines
 * Provides the low-level API for checking and repairing ('scrubbing
 * clean') the usage of Lots and lot balances in stock and commodity
 * accounts.  Broken lots are repaired using a first-in, first-out
 * (FIFO) accounting schedule.
 *
 * This is a 'low-level' API in the sense that each routine accomplishes
 * only one particular task needed to clean up a Lot.  To clean up a
 * Lot as a whole, you almost certainly want to use one of the
 * high-level API routines from the Scrub3.h file.
 @{ */

/** The xaccAccountAssignLots() routine will walk over all of
 *   the splits in an account, and make sure that each belongs
 *   to a lot.  Currently, the default (and only implemented)
 *   assignment policy is a FIFO policy: Any splits that are
 *   not in a lot will be used to close the oldest open lot(s).
 *   If there are no open lots, a new lot will be started.
 *   By trying to close the oldest lots, this effectively
 *   implements a FIFO accounting policy.
 */
void xaccAccountAssignLots (Account *acc);

typedef enum
{
    GNC_LOT_STATS_RUNNING,
    GNC_LOT_STATS_DONE,
    GNC_LOT_STATS_CANCELLED,
    GNC_LOT_STATS_STALE,
    GNC_LOT_STATS_FAILED,
} GncLotStatsPlanState;

/** Incrementally collect amount/value balance, split count, earliest/latest
 * positions, and first currency. Each step consumes at most max_work nodes. */
GncLotStatsPlan *gnc_lot_stats_plan_begin (GNCLot *lot,
                                           GncScrubContext *context);
GncLotStatsPlanState gnc_lot_stats_plan_step (GncLotStatsPlan *plan,
                                               guint max_work);
GncLotStatsPlanState gnc_lot_stats_plan_get_state (
    const GncLotStatsPlan *plan);
gboolean gnc_lot_stats_plan_get_result (const GncLotStatsPlan *plan,
                                         gnc_numeric *amount,
                                         gnc_numeric *value,
                                         guint *split_count,
                                         GncGUID *earliest_split,
                                         GncGUID *latest_split,
                                         GncGUID *currency);
void gnc_lot_stats_plan_free (GncLotStatsPlan *plan);

/** Validate and apply exactly one already-selected subsplit merge. */
gboolean gnc_scrub_merge_split_pair_prepared (Split *keep,
                                               Split *remove,
                                               gboolean strict);

/** State of a bounded, context-bound account lot-assignment plan. */
typedef enum
{
    GNC_LOT_ASSIGNMENT_PLAN_RUNNING,
    GNC_LOT_ASSIGNMENT_PLAN_DONE,
    GNC_LOT_ASSIGNMENT_PLAN_CANCELLED,
    GNC_LOT_ASSIGNMENT_PLAN_STALE,
    GNC_LOT_ASSIGNMENT_PLAN_FAILED,
} GncLotAssignmentPlanState;

/**
 * Create a bounded FIFO lot-assignment plan for @a account.
 *
 * The caller supplies an already active scrub context. The plan retains that
 * context but never acquires or releases its SCRUB lease; it is consequently
 * safe to use as one phase of a larger composite scrub job. Account and split
 * identity are retained only as GUIDs, never as object pointers across turns.
 * When GNC_AUTO_SCRUB_LOTS is set, the context must also have active GAINS
 * commit deferral. This prevents a split-assignment commit from re-entering
 * the unbounded synchronous gains scrub; the resulting transaction GUID is
 * retained by the book-bound deferred-commit queue for a later gains job.
 */
GncLotAssignmentPlan *gnc_lot_assignment_plan_begin (Account *account,
                                                      GncScrubContext *context);

/**
 * Perform at most @a max_work queued split-to-lot work units.
 *
 * At most @a max_work queued GUIDs are dequeued and resolved in one turn,
 * including entries that are already assigned, missing, void, or otherwise
 * ineligible. Successful assignments are counted separately by
 * gnc_lot_assignment_plan_get_completed(). A zero limit is a no-op. A split
 * remainder created while closing a lot is queued ahead of the next original
 * split, but is not examined until a later work unit.
 */
GncLotAssignmentPlanState gnc_lot_assignment_plan_step (
    GncLotAssignmentPlan *plan, guint max_work);

/** Stop this plan only. It deliberately does not cancel the shared context. */
void gnc_lot_assignment_plan_cancel (GncLotAssignmentPlan *plan);
GncLotAssignmentPlanState gnc_lot_assignment_plan_get_state (
    const GncLotAssignmentPlan *plan);
/** Number of queued GUIDs examined across all bounded work turns. */
guint gnc_lot_assignment_plan_get_examined (
    const GncLotAssignmentPlan *plan);
/** Number of successful individual split-to-lot assignments. */
guint gnc_lot_assignment_plan_get_completed (
    const GncLotAssignmentPlan *plan);

/** Release the retained context reference. Passing NULL is a no-op. */
void gnc_lot_assignment_plan_free (GncLotAssignmentPlan *plan);

/** The xaccLotFill() routine attempts to assign splits to the
 *  indicated lot until the lot balance goes to zero, or until
 *  there are no suitable (i.e. unassigned) splits left in the
 *  account.  It uses the default accounting policy to choose
 *  the splits to fill out the lot.
 */
void xaccLotFill (GNCLot *lot);

/** The xaccLotScrubDoubleBalance() routine examines the indicated
 *   lot.  If it is open, it does nothing. If it is closed,
 *   it then verifies that the lot is 'double balanced'.
 *   By 'double balance', we mean that both the sum of the
 *   split amounts is zero, and that the sum of the split
 *   values is zero.  If the lot is closed and the sum of the
 *   values is not zero, the lot is considered to have a
 *   'realized gain or loss' that hadn't been correctly handled.
 *   This routine then creates a balancing transaction to so
 *   as to record the realized gain/loss, adds it to the lot,
 *   and adds it to a gain/loss account.  If there is no default
 *   gain/loss account, it creates one.
 */
void xaccLotScrubDoubleBalance (GNCLot *lot);

/** The xaccScrubMergeSubSplits() routine will merge together
 *    all of the splits that were at one time split off from this
 *    split, but are no longer needed to be kept separate.  Splits
 *    might be split up if they need to be divided over multiple
 *    lots; they can be merged back together if the lots change.
 *    In particular, two sub-splits may be merged if they are in
 *    the same lot, or in no lot.  Note that, by definition, all
 *    subsplits belong to the same transaction.
 *
 *    There are two ways to find matching subsplits. The first
 *    way will consider splits to be subsplits only if they
 *    are explicitly marked as such while splitting the original
 *    split. Set strict to TRUE for this matching algorithm.
 *
 *    The second way is more relaxed. It will consider any two
 *    splits that happen to be part of the same lot and the
 *    same transaction to be subsplits. Set strict to FALSE for
 *    this matching algorithm.
 *
 *    The routine returns TRUE if a merger was performed, else
 *    it returns FALSE.
 */
gboolean xaccScrubMergeSubSplits (Split *split, gboolean strict);

/** The xaccScrubMergeLotSubSplits() routine does the same as
 *    the xaccScrubMergSubSplits, except that it does it
 *    for all of the splits in the lot.
 */
gboolean xaccScrubMergeLotSubSplits (GNCLot *lot, gboolean strict);

#ifdef __cplusplus
}
#endif

#endif /* XACC_SCRUB2_H */
/** @} */
/** @} */
/** @} */
