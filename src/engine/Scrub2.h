/********************************************************************\
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
 * repairing ('scrubbing clean') stock and commodity accounts
 * to use Lots & accounting schedules so that books can be closed.
 *
 */

#ifndef XACC_SCRUB2_H
#define XACC_SCRUB2_H

#include "gnc-engine.h"

/** The xaccAccountHasTrades() method checks to see if the 
 *    indicated account is used in the trading of commodities.
 *    A 'trading' account will contain transactions whose 
 *    transaction currency is not the same as the account
 *    commodity.  The existance of such transactions is
 *    the very definition of a 'trade'.   This routine returns
 *    TRUE if this is a trading account, else it returns
 *    FALSE.
 */
gboolean xaccAccountHasTrades (Account *);

/** The xaccAccountFindEarliestOpenLot() method is a handy
 *   utility routine for finding the earliest open lot in
 *   an account whose lot balance is *opposite* to the 
 *   passed argument 'sign'.   By 'earliest lot', we mean
 *   the lot that has a split with the earliest 'date_posted'.
 *   The sign comparison helps identify a lot that can be 
 *   added to: usually, one wants to add splits to a lot so
 *   that the balance only decreases.
 */
GNCLot * xaccAccountFindEarliestOpenLot (Account *acc, gnc_numeric sign);


/** XXX need docos
 */


#endif /* XACC_SCRUB2_H */
/** @} */
