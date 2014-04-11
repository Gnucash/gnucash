/********************************************************************\
 * Scrub.h -- convert single-entry accounts to clean double-entry   *
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
 *                                                                  *
\********************************************************************/

/** @addtogroup Engine
    @{ */
/** @addtogroup Scrub
    Data scrubbing, repairing and forward migration routines.
    These routines check and repair data, making sure that it
    is in a format that the current version of the GnuCash
    Engine likes.  These routines serve both to provide backwards
    compatibility with older versions of GnuCash, and to fix
    or at least paper over possible current problems.

    It is typically expected that the scrub routines are run 
    over newly imported data, as well as during data file input.
    
    In some cases, it is entirely appropriate to invoke these
    routines from the GUI, to validate that the user input 
    through the GUI is in a format that the system likes.  
    This includes things like balancing individual transactions,
    or assigning splits to lots, so that capital gains can be 
    computed.
    @{ */

/** @file Scrub.h
 *  @brief convert single-entry accounts to clean double-entry 
 *  @author Created by Linas Vepstas December 1998
 *  @author Copyright (c) 1998-2000, 2003 Linas Vepstas <linas@linas.org>
 */

#ifndef XACC_SCRUB_H
#define XACC_SCRUB_H

#include "gnc-engine.h"

/** @name Double-Entry Scrubbing
    Convert single-entry accounts to clean double-entry 

    Provides a set of functions and utilities for checking and
    repairing (formerly called 'scrubbing clean') single-entry accounts
    so that they can be promoted into self-consistent, clean
    double-entry accounts. Basically and additionally, this file
    collects all functions that turn old (deprecated) data structures
    into the current new data model.

    The ScrubOrphans() methods search for transacations that contain
    splits that do not have a parent account. These "orphaned splits"
    are placed into an "orphan account" which the user will have to 
    go into and clean up.  Kind of like the unix "Lost+Found" directory
    for orphaned inodes.  
    @{  */

/** The xaccTransScrubOrphans() method scrubs only the splits in the
 *    given transaction. 
 */
void xaccTransScrubOrphans (Transaction *trans);

/** The xaccAccountScrubOrphans() method performs this scrub only for the 
 *    indicated account, and not for any of its children.
 */
void xaccAccountScrubOrphans (Account *acc);

/** The xaccAccountTreeScrubOrphans() method performs this scrub for the 
 *    indicated account and its children.
 */
void xaccAccountTreeScrubOrphans (Account *acc);

/** The xaccSplitScrub method ensures that if this split has the same
 *   commodity and currency, then it will have the same amount and value.  
 *   If the commodity is the currency, the split->amount is set to the 
 *   split value.  In addition, if this split is an orphan, that is
 *   fixed first.  If the split account doesn't have a commodity declared,
 *   an attempt is made to fix that first.
 */
void xaccSplitScrub (Split *split);

/** The xacc*ScrubSplits() calls xaccSplitScrub() on each split
 *    in the respective structure: transaction, account, 
 *    account & it's children, account-group.
 */
void xaccTransScrubSplits (Transaction *trans);
void xaccAccountScrubSplits (Account *account);
void xaccAccountTreeScrubSplits (Account *account);

/** The xaccScrubImbalance() method searches for transactions that do
 *    not balance to zero. If any such transactions are found, a split
 *    is created to offset this amount and is added to an "imbalance"
 *    account.
 */
void xaccTransScrubImbalance (Transaction *trans, Account *root,
                              Account *parent);
void xaccAccountScrubImbalance (Account *acc);
void xaccAccountTreeScrubImbalance (Account *acc);

/** The xaccTransScrubCurrency method fixes transactions without a
 * common_currency by using the old account currency and security
 * fields of the parent accounts of the transaction's splits. */
void xaccTransScrubCurrency (Transaction *trans);

/** The xaccTransScrubCurrencyFromSplits method fixes transactions
 * where the currency doesn't match the currency used in the splits
 * in the transaction.  If all splits where the amount equals the 
 * value and where the commodity is a currency have the same 
 * currency, it sets the transaction's currency to that if it is
 * anything else.  If the splits don't match that description the
 * transaction currency is not changed. */
void xaccTransScrubCurrencyFromSplits(Transaction *trans);

/** The xaccAccountScrubCommodity method fixed accounts without
 * a commodity by using the old account currency and security. */
void xaccAccountScrubCommodity (Account *account);

/** The xaccAccountTreeScrubCommodities will scrub the
 * currency/commodity of all accounts & transactions in the specified
 * account or any child account. */
void xaccAccountTreeScrubCommodities (Account *acc);

/** This routine will migrate the information about price quote
 *  sources from the account data structures to the commodity data
 *  structures.  It first checks to see if this is necessary since,
 *  for the time being, the quote information will still be written
 *  out as part of the account.  Just in case anyone needs to fall
 *  back from CVS to a production version of code.
 *
 *  @param root A pointer to the root account containing all
 *  accounts in the current book.
 *
 *  @param table A pointer to the commodity table for the current
 *  book.
 */
void xaccAccountTreeScrubQuoteSources (Account *root, gnc_commodity_table *table);

void xaccAccountScrubKvp (Account *account);

#endif /* XACC_SCRUB_H */
/** @} */
/** @} */
/** @} */
