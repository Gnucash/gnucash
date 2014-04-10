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
 */
#ifndef XACC_SCRUB3_H
#define XACC_SCRUB3_H

#include "gnc-engine.h"

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
 *    migt be split up if they need to be divided over multiple
 *    lots; they can be merged back together if the lots change.
 *    In particular, two sub-splits may be merged if they are in 
 *    the same lot, or in no lot.  Note that, by definition, all
 *    subsplits belong to the same transaction.
 * 
 *    The routine returns TRUE if a merger was performed, else 
 *    it returns FALSE. 
 *
 *  The xaccScrubMergeTxnSubSplits() routine does the same, except 
 *    that it does it for all of the splits in the transaction.
 */
gboolean xaccScrubMergeSubSplits (Split *split);
gboolean xaccScrubMergeTxnSubSplits (Transaction *txn);

#endif /* XACC_SCRUB3_H */
/** @} */
