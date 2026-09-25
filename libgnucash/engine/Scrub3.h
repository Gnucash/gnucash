/********************************************************************\
 * Scrub3.h -- High-Level Lot Constraint routines.                  *
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
/** @addtogroup Scrub
    @{ */

/** @file Scrub3.h
 *  @brief High-Level API for imposing Lot constraints
 *  @author Created by Linas Vepstas Sept 2003
 *  @author Copyright (c) 2003 Linas Vepstas <linas@linas.org>
 */
#ifndef XACC_SCRUB3_H
#define XACC_SCRUB3_H

#include "gnc-engine.h"
#include "Scrub.h"

typedef struct GncSplitAssignPlan GncSplitAssignPlan;
typedef struct GncLotScrubPlan GncLotScrubPlan;
typedef struct GncAccountLotsPlan GncAccountLotsPlan;

#ifdef __cplusplus
extern "C" {
#endif

/** @name High-Level Lot Constraint
 * Provides the high-level API for checking and repairing ('scrubbing
 * clean') the usage of Lots and Cap Gains transactions in stock and
 * commodity accounts.
 @{ */

/** The xaccScrubLot() routine makes sure that the indicated lot is
 *    self-consistent and properly balanced, and fixes it if its not.
 *    This is an important routine to call if the amount of any split
 *    in the lot is changed.  That's because (obviously) changing
 *    split values is guaranteed to throw off lot balances.
 *    This routine may end up closing the lot, or at least trying
 *    to. It will also cause cap gains to be recomputed.
 *
 *    Scrubbing the lot may cause subsplits to be merged together,
 *    i.e. for splits to be deleted.  This routine returns true if
 *    any splits were deleted.
 */
gboolean xaccScrubLot (GNCLot *lot);
gboolean xaccScrubLotWithContext (GNCLot *lot,
                                  GncScrubContext *context);

/** The xaccAccountScrubLots() routine makes sure that every split
 *    in the account is assigned to a lot, and that then, every
 *    lot is self-consistent (by calling xaccScrubLot() on each lot).
 *
 *    This routine is the primary routine for ensuring that the
 *    lot structure, and the cap-gains for an account are in good
 *    order.
 *
 * Most GUI routines will want to use one of these xacc[*]ScrubLots()
 * routines, instead of the various component routines, since it will
 * usually makes sense to work only with these high-level routines.
 */
void xaccAccountScrubLots (Account *acc);
void xaccAccountTreeScrubLots (Account *acc);
void xaccAccountScrubLotsWithContext (Account *acc,
                                      GncScrubContext *context);
void xaccAccountTreeScrubLotsWithContext (Account *acc,
                                          GncScrubContext *context);

typedef enum
{
    GNC_SPLIT_ASSIGN_PLAN_RUNNING,
    GNC_SPLIT_ASSIGN_PLAN_DONE,
    GNC_SPLIT_ASSIGN_PLAN_CANCELLED,
    GNC_SPLIT_ASSIGN_PLAN_STALE,
    GNC_SPLIT_ASSIGN_PLAN_FAILED,
} GncSplitAssignPlanState;

/** Assign one split (and any generated remainder) without a policy full-scan.
 * Open-lot selection is a bounded best-candidate collection over raw lists. */
GncSplitAssignPlan *gnc_split_assign_plan_begin (Split *split,
                                                 GncScrubContext *context);
GncSplitAssignPlanState gnc_split_assign_plan_step (
    GncSplitAssignPlan *plan, guint max_work);
GncSplitAssignPlanState gnc_split_assign_plan_get_state (
    const GncSplitAssignPlan *plan);
void gnc_split_assign_plan_cancel (GncSplitAssignPlan *plan);
void gnc_split_assign_plan_free (GncSplitAssignPlan *plan);

gboolean gnc_lot_scrub_plan_get_splits_deleted (
    const GncLotScrubPlan *plan);
typedef enum
{
    GNC_LOT_SCRUB_PLAN_RUNNING,
    GNC_LOT_SCRUB_PLAN_DONE,
    GNC_LOT_SCRUB_PLAN_CANCELLED,
    GNC_LOT_SCRUB_PLAN_STALE,
    GNC_LOT_SCRUB_PLAN_FAILED,
} GncLotScrubPlanState;

/** Run strict merge, rethin, policy fill, capital gains, and final double
 * balance as individually bounded scans/mutations on one non-business lot. */
GncLotScrubPlan *gnc_lot_scrub_plan_begin (GNCLot *lot,
                                           GncScrubContext *context);
GncLotScrubPlanState gnc_lot_scrub_plan_step (GncLotScrubPlan *plan,
                                               guint max_work);
GncLotScrubPlanState gnc_lot_scrub_plan_get_state (
    const GncLotScrubPlan *plan);
void gnc_lot_scrub_plan_cancel (GncLotScrubPlan *plan);
void gnc_lot_scrub_plan_free (GncLotScrubPlan *plan);

typedef enum
{
    GNC_ACCOUNT_LOTS_PLAN_RUNNING,
    GNC_ACCOUNT_LOTS_PLAN_DONE,
    GNC_ACCOUNT_LOTS_PLAN_CANCELLED,
    GNC_ACCOUNT_LOTS_PLAN_STALE,
    GNC_ACCOUNT_LOTS_PLAN_FAILED,
} GncAccountLotsPlanState;

/** Sequence bounded split assignment and lot scrub jobs over one account or
 * a breadth-first account tree. AP/AR accounts remain in the business path. */
GncAccountLotsPlan *gnc_account_lots_plan_begin (Account *account,
                                                 gboolean descendants,
                                                 GncScrubContext *context);
GncAccountLotsPlanState gnc_account_lots_plan_step (
    GncAccountLotsPlan *plan, guint max_work);
guint gnc_account_lots_plan_get_completed (const GncAccountLotsPlan *plan);
void gnc_account_lots_plan_cancel (GncAccountLotsPlan *plan);
void gnc_account_lots_plan_free (GncAccountLotsPlan *plan);

/** @} */

#ifdef __cplusplus
}
#endif

#endif /* XACC_SCRUB3_H */
/** @} */
/** @} */
