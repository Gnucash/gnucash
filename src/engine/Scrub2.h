/********************************************************************\
 * Scrub2.h -- Convert Stock Accounts to use Lots                   *
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
/** @file Scrub2.h
 *  @breif Utilities to Convert Stock Accounts to use Lots
 *  @author Created by Linas Vepstas March 2003
 *  @author Copyright (c) 2003 Linas Vepstas <linas@linas.org>
 *
 *
 * Provides a set of functions and utilities for checking and
 * repairing ('scrubbing clean') the usage of Lots and lot balances
 * in stock and commodity accounts.  Broken lots are repaired using
 * a first-in, first-out (FIFO) accounting schedule.
 */

#ifndef XACC_SCRUB2_H
#define XACC_SCRUB2_H

#include "gnc-engine.h"


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

/** The xaccAccountScrubDoubleBalance() routine examines all
 *   of the closed lots in an account, and verifies that the
 *   lots are 'double balanced'.  By 'double balance', we mean
 *   that both the sum of the split amounts is zero, and that
 *   the sum of the split values is zero.  If a closed lot is 
 *   found where the sum of the values is not zero, the lot
 *   is considered to have a 'realized gain or loss' that
 *   hadn't been correctly handled.  This routine then creates
 *   a balancing transaction so as to record the realized 
 *   gain/loss, adds it to the lot, and adds it to a gain/loss
 *   account.  If there is no default gain/loss account, it 
 *   creates one.
 */
void xaccAccountScrubDoubleBalance (Account *acc);

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

#endif /* XACC_SCRUB2_H */
/** @} */
