/********************************************************************\
 * AccountP.h -- Account engine-private data structure              *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1997-2002, Linas Vepstas <linas@linas.org>         *
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

/** @file AccountP.h
 *
 * This is the *private* header for the account structure.
 * No one outside of the engine should ever include this file.
 *
 * This header includes prototypes for "dangerous" functions.
 * Invoking any of these functions potentially leave the account
 * in an inconsistent state.  If they are not used in the proper
 * setting, they can leave the account structures in an inconsistent
 * state.  Thus, these methods should never be used outside of
 * the engine, which is why they are "hidden" here. 
 *
 */

#ifndef XACC_ACCOUNT_P_H
#define XACC_ACCOUNT_P_H

#include "Account.h"
#include "gnc-commodity.h"
#include "gnc-engine.h"
#include "policy.h"

/** STRUCTS *********************************************************/

/** This is the data that describes an account. 
 *
 * This is the *private* header for the account structure.
 * No one outside of the engine should ever include this file.
*/


/* The xaccAccountSortSplits() routine will resort the account's 
 * splits if the sort is dirty. If 'force' is true, the account 
 * is sorted even if the editlevel is not zero. 
 */
void xaccAccountSortSplits (Account *acc, gboolean force);

/* The following recompute the partial balances (stored with the
 * transaction) and the total balance, for this account 
 */
void xaccAccountRecomputeBalance (Account *);

/* Set the account's GUID. This should only be done when reading
 * an account from a datafile, or some other external source. Never
 * call this on an existing account! */
void xaccAccountSetGUID (Account *account, const GUID *guid);

/* The xaccAccountSetStartingBalance() routine will set the starting
 *    commodity balance for this account.  This routine is intended for
 *    use with backends that do not return the complete list of splits
 *    for an account, but rather return a partial list.  In such a case,
 *    the backend will typically return all of the splits after some 
 *    certain date, and the 'starting balance' will represent the summation 
 *    of the splits up to that date.
 *
 *    This routine is in the private .h file because only backends are 
 *    allowed to set the starting balance.  This is *not* a user interface
 *    function.
 */
void xaccAccountSetStartingBalance(Account *account, 
                                   const gnc_numeric start_baln, 
                                   const gnc_numeric start_cleared_baln, 
                                   const gnc_numeric start_reconciled_baln); 

/* The xaccFreeAccount() routine releases memory associated with the
 *    account.  It should never be called directly from user code;
 *    instead, the xaccAccountDestroy() routine should be used
 *    (because xaccAccountDestroy() has the correct commit semantics).
 */

void xaccFreeAccount (Account *account);

/* The xaccAccountSet/GetVersion() routines set & get the version 
 *    numbers on this account.  The version number is used to manage
 *    multi-user updates.  These routines are private because we don't
 *    want anyone except the backend to mess with them.
 */
void xaccAccountSetVersion (Account*, gint32);
gint32 xaccAccountGetVersion (const Account* acc);

/* Register Accounts with the engine */
gboolean xaccAccountRegister (void);

/** killed for now, need to resurect this or something similar
 *  * for transactional/dirty kvp.  Later.  Right now a place holder
 *   */
#define xaccAccountSetSlots_nc(A,S) qof_instance_set_slots(QOF_INSTANCE(A),S)

#endif /* XACC_ACCOUNT_P_H */
