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
 * repairing ('scrubbing clean') the usage of Lots and lot balances
 * in stock and commodity accounts.  Broken lots are repaired using
 * a first-in, first-out (FIFO) accounting schedule.
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

/** The xaccAccountGetDefaultGainAccount() routine will return
 *   the account to which realized gains/losses may be posted.  
 *   Because gains may be in different currencies, one must
 *   specify the currency type in which the gains will be posted.
 *   This routine does nothing more than return the value of
 *   the "/lot-mgmt/gains-act/XXX" key, where XXX is the unique
 *   currency name.  IOf there is no default account for this
 *   currency, NULL will be returned.
 */
Account * xaccAccountGetDefaultGainAccount (Account *acc, gnc_commodity * currency);

/** The xaccAccountSetDefaultGainAccount() routine can be used 
 *   to set the account to which realized gains/losses will be 
 *   posted by default. This routine does nothing more than set 
 *   value of the "/lot-mgmt/gains-act/XXX" key, where XXX is the 
 *   unique currency name of the currency of gains account.
 */
void xaccAccountSetDefaultGainAccount (Account *acc, Account *gains_acct);

/** The xaccAccountScrubLots() routine will walk over all of
 *   the splits in an account, and make sure that each belongs
 *   to a lot.  Any splits that are not in a lot will be used
 *   to close the oldest open lot(s).  If there are no open 
 *   lots, a new lot will be started.  By trying to close the 
 *   oldest lots, this routine implements a FIFO acounting
 *   policy.
 */
void xaccAccountScrubLots (Account *acc);


/** The xaccAccountScrubDoubleBalance() routine examines all
 *   of the closed lots in an account, and verifies that the
 *   lots are 'double balanced'.  By 'double balance', we mean
 *   that both the sum of the split amounts is zero, and that
 *   the sum of the split values is zero.  If a closed lot is 
 *   found where the sum of the values is not zero, the lot
 *   is considered to have a 'realized gain or loss' that
 *   hadn't been correctly handled.  This routine then creates
 *   a balancing transaction to make not of the realized 
 *   gain/loss, adds it to the lot, and add it to a special
 *   orphaned gain/loss account.
 */
void xaccAccountScrubDoubleBalance (Account *acc);

/** The xaccGroupScrubLotsBalance() routine walks the
 *   account tree, and invokes xaccAccountScrubLots()
 *   and xaccAccountScrubDoubleBalance() on all accounts 
 *   that are trading accounts.
 */
void xaccGroupScrubLotsBalance (AccountGroup *grp);
void xaccAccountScrubLotsBalance (Account *acc);
void xaccAccountTreeScrubLotsBalance (Account *acc);

#endif /* XACC_SCRUB2_H */
/** @} */
