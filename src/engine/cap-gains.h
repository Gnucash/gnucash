/********************************************************************\
 * cap-gains.h -- Automatically Compute Capital Gains/Losses        *
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
 *     @{ */

/** @addtogroup CapGains Cap Gains
 *  This file implements the various routines to automatically
 *  compute and handle Cap Gains/Losses resulting from trading
 *  activities.  Some of these routines might have broader
 *  applicability, for handling depreciation & etc.
 *
 *  This code is under development, and is 'beta': we think we're
 *  mostly done, and we've tested and "things work for us", but there
 *  may still be something missing, and there might still be some
 *  bugs.
 *
 *  This code does not currently handle tax distinctions, e.g
 *  the different tax treatment that short-term and long-term
 *  cap gains have.
 *
 * The computation of (Realized) Gains/Losses is performed automatically by
 * the lot "scrub" routines, using a "double-balance" algorithm. Every
 * split has two numbers associated with it: an "amount", which is the
 * number of items that a split describes, and the "value", which is the
 * cost of those items. In a closed lot, the grand-total amount of items in
 * the lot is zero: the number of items bought equals the number of items
 * sold; thus the amount-balance is zero. But since the purchase/sale of
 * the items in the lot typically happen at different prices, there will
 * typically be a gain/loss. This gain/loss is the grand-total value of all
 * the items in the lot (total costs minus total income).
 *
 * In order to properly account for the gains/losses, an "adjusting split"
 * is added that brings the total gains/losses back to exactly zero (this
 * is the second "balance" of "double balance"). This adjusting split will
 * have an amount of zero (no items are involved) but have a non-zero value
 * (equal to the total gain/loss). This split can then participate in a
 * "gains transaction" which records the gains in another account. Thus,
 * for example, if you record $300 in your bank account due to the purchase
 * and then the sale of some item, the "gains transaction" will record $300
 * in income in an income account. Thus, the change in the bank balance is
 * always reflected by an equal change in income, assuring that the books
 * are balanced.
 *
 * Notes about auto-recompute:  If the amount in a split is changed,
 * then the lot has to be recomputed.
 * This has a potential trickle-through effect on all later lots.
 * Ideally, later lots are dissolved, and recomputed.  However, some
 * lots may have been user-hand-built. These should be left alone.
 *
ToDo:
 o XXX Need to create a data-integrity scrubber, tht makes sure that
   the various flags, and pointers & etc. match.
 *     @{ */

/** @file cap-gains.h
 *  @brief Utilities to Automatically Compute Capital Gains/Losses.
 *  @author Created by Linas Vepstas August 2003
 *  @author Copyright (c) 2003,2004 Linas Vepstas <linas@linas.org>
 */

#ifndef XACC_CAP_GAINS_H
#define XACC_CAP_GAINS_H

#include "gnc-engine.h"

/** The xaccSplitGetCapGains() method returns the value of
 *    capital gains (if any) associated with the indicated
 *    split. In order for there to be any capital gains,
 *    several things must hold true about this split:
 *    (1) It must have been involved in trading (for aexample,
 *        by belonging to a stock or trading account)
 *    (2) It must have been assigned to a lot.
 *    (3) It cannot be the opening split of a lot; that
 *        is, it must be a matching sale of an earlier purchase
 *        (or vice versa).
 */
gnc_numeric xaccSplitGetCapGains(Split *);

/** The xaccAccountHasTrades() method checks to see if the
 *    indicated account is used in the trading of commodities.
 *    A 'trading' account will contain transactions whose
 *    transaction currency is not the same as the account
 *    commodity.  The existance of such transactions is
 *    the very definition of a 'trade'.   This routine returns
 *    TRUE if this is a trading account, else it returns
 *    FALSE.
 */
gboolean xaccAccountHasTrades (const Account *);

/** The xaccAccountFindEarliestOpenLot() method is a handy
 *   utility routine for finding the earliest open lot in
 *   an account whose lot balance is *opposite* to the
 *   passed argument 'sign'.   By 'earliest lot', we mean
 *   the lot that has a split with the earliest 'date_posted'.
 *   The sign comparison helps identify a lot that can be
 *   added to: usually, one wants to add splits to a lot so
 *   that the balance only decreases.
 *   If 'currency' is non-null, then this attempts to find
 *   a lot whose opening transaction has the same currency.
 */
GNCLot * xaccAccountFindEarliestOpenLot (Account *acc,
        gnc_numeric sign,
        gnc_commodity *currency);
GNCLot * xaccAccountFindLatestOpenLot (Account *acc,
                                       gnc_numeric sign,
                                       gnc_commodity *currency);

/** The xaccAccountGetDefaultGainAccount() routine will return
 *   the account to which realized gains/losses may be posted.
 *   Because gains may be in different currencies, one must
 *   specify the currency type in which the gains will be posted.
 *   This routine does nothing more than return the value of
 *   the "/lot-mgmt/gains-act/XXX" key, where XXX is the unique
 *   currency name.  IOf there is no default account for this
 *   currency, NULL will be returned.
 */
Account * xaccAccountGetDefaultGainAccount (const Account *acc, const gnc_commodity * currency);

/** The xaccAccountSetDefaultGainAccount() routine can be used
 *   to set the account to which realized gains/losses will be
 *   posted by default. This routine does nothing more than set
 *   value of the "/lot-mgmt/gains-act/XXX" key, where XXX is the
 *   unique currency name of the currency of gains account.
 */
void xaccAccountSetDefaultGainAccount (Account *acc, const Account *gains_acct);

/** The xaccSplitGetCapGainsSplit() routine returns the split
 *  that records the cap gains for this split.  It returns NULL
 *  if not found.  This routine does nothing more than search for
 *  the split recorded in the KVP key "/gains-split"
 */
Split * xaccSplitGetCapGainsSplit (const Split *);

/** The xaccSplitGetGainsSourceSplit() routine returns the split
 *  that is the source of the cap gains in this split.  It returns
 *  NULL if not found.  This routine does nothing more than search
 *  for the split recorded in the KVP key "/gains-source"
 */
Split * xaccSplitGetGainsSourceSplit (const Split *);

/** The`xaccSplitAssign() routine will take the indicated
 *  split and, if it doesn't already belong to a lot, it will attempt
 *  to assign it to an appropriate lot.
 *  If the split already belongs to a Lot, this routine does nothing.
 *  If there are no open Lots, this routine will create a new lot
 *  and place the split into it.  If there's an open lot, and its
 *  big enough to accept the split in it's entirety, then the split
 *  will be placed into that lot.  If the split is too big to fit
 *  into the currently open lot, it will be busted up into two
 *  (or more) pieces, and each placed into a lot accordingly.
 *  If the split needed to be broken up into several pieces, this
 *  routine will return TRUE, else it returns FALSE.
 *
 *  If the split had to be broken up, kvp markup in the "/lot-split"
 *  directory is used to identify the peers. 'gemini'-style kvp's
 *  are used.
 *
 *  This routine uses the "FIFOPolicy" callback, and thus
 *  implements a "FIFO" First-In First-Out accounting policy.
 *  This is currently the only implemented policy; adding new
 *  policies should be 'easy'; read the source luke.
 */

gboolean xaccSplitAssign (Split *split);

/** The xaccSplitAssignToLot() routine will fit the indicated split
 *    into the indicated lot, with the goal of closing the lot, or
 *    at least bringing the lot balance closer to closure.  (A closed
 *    lot has a balance of zero).  To make this "fit", a variety of
 *    checks and actions are performed.  First, the lot must be open,
 *    and the sign of the split amount must be opposite to the sign
 *    of the lot balance.  The 'opposite-sign' requirement is so that
 *    inserting the split will cause the size of the lot to decrease.
 *    If the amount of the split is too small, or is just right to
 *    close the lot, the split is added, and NULL is returned.  If
 *    the split is larger than the lot balance, the split will be
 *    divided into sub-splits, one of which is just right to close
 *    the lot.   A pointer to the other sub-split will be returned.
 *
 *    If the split had to be broken up, kvp markup in the "/lot-split"
 *    directory is used to identify the peers. 'gemini'-style kvp's
 *    are used.
 */
Split * xaccSplitAssignToLot (Split *split, GNCLot *lot);

/** The xaccSplitComputeCapGains() routine computes the cap gains
 *  or losses for the indicated split.  The gains are placed into
 *  the 'gains_acct'.  If the gains_acct is NULL, then the appropriate
 *  default account is used (and created, if needed).
 *
 *  To compute the gains, the split must belong to a lot. If the
 *  split is the 'opening split', i.e. the earliest split in the
 *  lot, then nothing is done, as there are no gains/losses (something
 *  must be bought *and* sold for there to be a gain/loss).
 *
 *  Note also: the 'amount' of the split must be of opposite sign,
 *  and must be equal to or smaller, than the 'amount' of the opening
 *  split; its an error otherwise.  If the 'amount' of the split is
 *  less than the opening amount, the gains are pro-rated.
 *
 *  The xaccLotComputeCapGains() routine merely invokes the above on
 *    each split in the lot.
 */

void xaccSplitComputeCapGains(Split *split, Account *gain_acc);
void xaccLotComputeCapGains (GNCLot *lot, Account *gain_acc);

#endif /* XACC_CAP_GAINS_H */
/** @} */
/** @} */

/* =========================== END OF FILE ======================= */
