/********************************************************************\
 * Scrub3.h -- Constrain Cap Gains to Track Sources of Gains        *
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
\********************************************************************/

/** @addtogroup Engine
    @{ */
/** @file Scrub3.h
 *  @breif Constrain Cap Gains to Track Sources of Gains
 *  @author Created by Linas Vepstas Sept 2003
 *  @author Copyright (c) 2003 Linas Vepstas <linas@linas.org>
 *
 * Provides a set of functions and utilities for checking and
 * repairing ('scrubbing clean') the usage of Cap Gains
 * transactions in stock and commodity accounts.  
 *
 * NOTE: Unless you have special needs, the functions you are looking
 * for and almost certainly want to use are either xaccScrubLot() or
 * xaccAccountScrubLots().
 */
#ifndef XACC_SCRUB3_H
#define XACC_SCRUB3_H

#include "gnc-engine.h"

/** The xaccScrubLot() routine makes sure that the indicated lot is
 *    self-consistent and properly balanced, and fixes it if its not.
 *    This is an important routine to call if the amount of any split
 *    in the lot is changed.  That's because (obviously) changing 
 *    split values is gaurenteed to throw off lot balances.
 *    This routine may end up closing the lot, or at least trying
 *    to. It will also cause cap gains to be recomputed.
 *
 *    Scrubbing the lot may cause subsplits to be merged together,
 *    i.e. for splits to be deleted.  This routine returns true if
 *    any splits were deleted.
 */
gboolean xaccScrubLot (GNCLot *lot);

/** The xaccAccountScrubLots() routine makes sure that every split
 *    in the account is assigned to a lot, and that then, every
 *    lot is self-consistent (by calling xaccScrubLot() on each lot).
 *
 *    This routine is the primary routine for ensuring that the 
 *    lot structure, and the cap-gains for an account are in good 
 *    order.
 *
 * The xaccGroupScrubLots() routine walks the account tree, and invokes 
 *    xaccAccountScrubLots() on all accounts that are trading accounts.
 * The xaccAccountTreeScrubLots() does the same.
 *
 * Most GUI routines will want to use one of these xacc[*]ScrubLots()
 * routines, instead of the various component routines, since it will 
 * usually makes sense to work only with these high-level routines.
 */
void xaccAccountScrubLots (Account *acc);
void xaccGroupScrubLots (AccountGroup *grp);
void xaccAccountTreeScrubLots (Account *acc);


/** If a split has been pulled apart to make it fit into two (or more)
 * lots, then it becomes theoretically possible for each subsplit to
 * have a distinct price.  But this would be wrong: each subsplit should
 * have the same price, within rounding errors.  This routine will
 * examine the indicated split for sub-splits, and adjust the value
 * of each so that they all have the same price.
 *
 * There is a bit of a problem with the interpretation of 'rounding
 * errors' because there are pathological corner cases of small 
 * amounts.  So this routine is fairly loose, hopefully loose enough
 * so that the user can manually fine tune without having this routine
 * clobber thier work.
 */
void xaccScrubSubSplitPrice (Split *split);

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

#endif /* XACC_SCRUB3_H */
/** @} */
