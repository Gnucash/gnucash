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
 *  @brief Hiogh-Level API for imposing Lot constraints
 *  @author Created by Linas Vepstas Sept 2003
 *  @author Copyright (c) 2003 Linas Vepstas <linas@linas.org>
 */
#ifndef XACC_SCRUB3_H
#define XACC_SCRUB3_H

#include "gnc-engine.h"

/** @name High-Level Lot Constraint
 * Provides the high-level API for checking and repairing ('scrubbing
 * clean') the usage of Lots and Cap Gains transactions in stock and
 * commodity accounts.
 @{ */

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
 * Most GUI routines will want to use one of these xacc[*]ScrubLots()
 * routines, instead of the various component routines, since it will
 * usually makes sense to work only with these high-level routines.
 */
void xaccAccountScrubLots (Account *acc);
void xaccAccountTreeScrubLots (Account *acc);

/** @} */
#endif /* XACC_SCRUB3_H */
/** @} */
/** @} */
