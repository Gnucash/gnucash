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
 *   implements a FIFO acounting policy.
 */
void xaccAccountAssignLots (Account *acc);

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

/** If a split has been pulled apart to make it fit into two (or more)
 * lots, then it becomes theoretically possible for each subsplit to
 * have a distinct price.  But this would be wrong: each subsplit should
 * have the same price, within rounding errors.  This routine will
 * examine the indicated split for sub-splits, and adjust the value
 * of each so that they all have the same price.
 *
 * There is a bit of a problem with the interpretation of 'rounding
 * errors' because there are pathological corner cases of small
 * amounts.  So this routine is loose, hopefully loose enough so
 * that the user can manually fine tune without having this routine
 * clobber thier work.
 *
 * This routine ignores price differences smaller than 1/maxmult.
 * This routine ignores price differences when the split with a crazy
 * price involes only a small amount: specifically, an amount that
 * is less than maxamtscu/amount.denom.
 *
 * Reasonable/recommended values might be maxmult=3, maxamtscu = 2.
 */
void xaccScrubSubSplitPrice (Split *split, int maxmult, int maxamtscu);

/** The xaccScrubMergeSubSplits() routine will merge together
 *    all of the splits that were at one time split off from this
 *    split, but are no longer needed to be kept separate.  Splits
 *    might be split up if they need to be divided over multiple
 *    lots; they can be merged back together if the lots change.
 *    In particular, two sub-splits may be merged if they are in
 *    the same lot, or in no lot.  Note that, by definition, all
 *    subsplits belong to the same transaction.
 *
 *    The routine returns TRUE if a merger was performed, else
 *    it returns FALSE.
 *
 *  The xaccScrubMergeTransSubSplits() routine does the same, except
 *    that it does it for all of the splits in the transaction.
 *  The xaccScrubMergeLotSubSplits() routine does the same, except
 *    that it does it for all of the splits in the lot.
 */
gboolean xaccScrubMergeSubSplits (Split *split);
gboolean xaccScrubMergeTransSubSplits (Transaction *txn);
gboolean xaccScrubMergeLotSubSplits (GNCLot *lot);

#endif /* XACC_SCRUB2_H */
/** @} */
/** @} */
/** @} */
