/********************************************************************\
 * Transaction.h -- api for transactions & splits (journal entries) *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1997, 1998, 1999, 2000 Linas Vepstas               *
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
 *                                                                  *
\********************************************************************/

#ifndef XACC_TRANSACTION_H
#define XACC_TRANSACTION_H

#include <time.h>

#include "gnc-commodity.h"
#include "gnc-engine.h"
#include "kvp_frame.h"
#include "GNCId.h"
#include "date.h"


/* Values for the reconciled field in Splits */
#define CREC 'c'              /* The Split has been cleared        */
#define YREC 'y'              /* The Split has been reconciled     */
#define FREC 'f'              /* frozen into accounting period     */
#define NREC 'n'              /* not reconciled or cleared         */
#define VREC 'v'              /* split is void                     */

/** STRUCTS *********************************************************/

typedef struct account_s       Account;
typedef struct account_group_s AccountGroup;
typedef struct split_s         Split;
typedef struct transaction_s   Transaction;
typedef GList                  AccountList;
typedef GList                  SplitList;


/** PROTOTYPES ******************************************************/

/*
 * The xaccConfigSetForceDoubleEntry() and xaccConfigGetForceDoubleEntry()
 *    set and get the "force_double_entry" flag.  This flag determines how
 *    the splits in a transaction will be balanced.
 *
 *    The following values have significance:
 *    0 -- anything goes
 *    1 -- The sum of all splits in a transaction will be
 *         forced to be zero, even if this requires the
 *         creation of additional splits.  Note that a split
 *         whose value is zero (e.g. a stock price) can exist
 *         by itself. Otherwise, all splits must come in at
 *         least pairs.
 *    2 -- splits without parents will be forced into a
 *         lost & found account.  (Not implemented)
 */

void   xaccConfigSetForceDoubleEntry (int force);
int    xaccConfigGetForceDoubleEntry (void);

/*
 * The xaccMallocTransaction() will malloc memory and initialize it.
 *    Once created, it is usually unsafe to merely "free" this memory;
 *    the xaccTransDestroy() method should be called.
 */ 
Transaction * xaccMallocTransaction (GNCSession *session); 

gboolean xaccTransEqual(const Transaction *ta,
                        const Transaction *tb,
                        gboolean check_guids,
                        gboolean check_splits);


/* The xaccTransDestroy() method will remove all 
 *    of the splits from each of their accounts, free the memory
 *    associated with them.  This routine must be followed by either
 *    an xaccTransCommitEdit(), in which case the transaction 
 *    memory will be freed, or by xaccTransRollbackEdit(), in which 
 *    case nothing at all is freed, and everything is put back into 
 *    original order.
 */
void          xaccTransDestroy (Transaction *trans);

/* The xaccTransBeginEdit() method must be called before any changes
 *    are made to a transaction or any of its component splits.  If 
 *    this is not done, errors will result.
 *
 * The xaccTransCommitEdit() method indicates that the changes to the
 *    transaction and its splits are complete and should be made
 *    permanent. Note this routine may result in the deletion of the
 *    transaction, if the transaction is "empty" (has no splits), or
 *    of xaccTransDestroy() was called on the transaction.
 *
 * The xaccTransRollbackEdit() routine rejects all edits made, and 
 *    sets the transaction back to where it was before the editing 
 *    started.  This includes restoring any deleted splits, removing
 *    any added splits, and undoing the effects of xaccTransDestroy,
 *    as well as restoring share quantities, memos, descriptions, etc.
 *
 * The xaccTransIsOpen() method returns TRUE if the transaction
 *    is open for editing. Otherwise, it returns false.  */
void          xaccTransBeginEdit (Transaction *trans);
void          xaccTransCommitEdit (Transaction *trans);
void          xaccTransRollbackEdit (Transaction *trans);

gboolean      xaccTransIsOpen (Transaction *trans);

/*
 * The xaccTransGetGUID() subroutine will return the
 *    globally unique id associated with that transaction.
 *    xaccTransReturnGUID() does the same thing but
 *    returns a GUID struct.
 *
 * The xaccTransLookup() subroutine will return the
 *    transaction associated with the given id, or NULL
 *    if there is no such transaction.
 */
const GUID  * xaccTransGetGUID (Transaction *trans);
GUID          xaccTransReturnGUID (Transaction *trans);
Transaction * xaccTransLookup (const GUID *guid, GNCSession *session);


/* Transaction slots are used to store arbitrary strings, numbers, and
 * structures which aren't members of the transaction struct.  */

kvp_frame *xaccTransGetSlots(Transaction *trans);
void xaccTransSetSlots_nc(Transaction *t, kvp_frame *frm);

/* The xaccTransSetDateSecs() method will modify the posted date 
 *    of the transaction.  (Footnote: this shouldn't matter to a user,
 *    but anyone modifying the engine should understand that when
 *    xaccTransCommitEdit() is called, the date order of each of the 
 *    component splits will be checked, and will be restored in 
 *    ascending date order.)
 *
 * The xaccTransSetDate() method does the same thing as 
 *    xaccTransSetDateSecs(), but takes a convenient day-month-year format.
 *
 * The xaccTransSetDatePostedTS() method does the same thing as 
 *    xaccTransSetDateSecs(), but takes a struct timespec64.
 *
 */
void          xaccTransSetDate (Transaction *trans,
                                int day, int mon, int year);
void          xaccTransSetDateSecs (Transaction *trans, time_t time);
void          xaccTransSetDatePostedTS (Transaction *trans,
                                        const Timespec *ts);

void          xaccTransSetDateEnteredSecs (Transaction *trans, time_t time);
void          xaccTransSetDateEnteredTS (Transaction *trans,
                                         const Timespec *ts);

/* set the Num, Description, and Notes fields */
void          xaccTransSetNum (Transaction *trans, const char *num);
void          xaccTransSetDescription (Transaction *trans, const char *desc);
void          xaccTransSetNotes (Transaction *trans, const char *notes);

/* The xaccTransAppendSplit() method will append the indicated 
 *    split to the collection of splits in this transaction.
 *    If the split is already a part of another transaction,
 *    it will be removed from that transaction first.
 */
void          xaccTransAppendSplit (Transaction *trans, Split *split);

/* The xaccSplitDestroy() method will update its parent account and 
 *    transaction in a consistent manner, resulting in the complete 
 *    unlinking of the split, and the freeing of its associated memory.
 *    The goal of this routine is to perform the removal and destruction
 *    of the split in an atomic fashion, with no chance of accidentally
 *    leaving the accounting structure out-of-balance or otherwise
 *    inconsistent.
 *
 *    If the deletion of the split leaves the transaction with no
 *    splits, then the transaction will be marked for deletion. (It
 *    will not be deleted until the xaccTransCommitEdit() routine is
 *    called.)
 */
void          xaccSplitDestroy (Split *split);

/* ------------- gets --------------- */
/* The xaccTransGetSplit() method returns a pointer to each of the 
 *    splits in this transaction.  Valid values for i are zero to 
 *    (number_of__splits-1).  An invalid value of i will cause NULL to
 *    be returned.  A convenient way of cycling through all splits is
 *    to start at zero, and keep incrementing until a null value is returned.
 */
Split *       xaccTransGetSplit (Transaction *trans, int i);

/* The xaccTransGetSplitList() method returns a GList of the splits
 * in a transaction.  This list must not be modified.  Do *NOT* free
 * this list when you are done with it. */
SplitList *   xaccTransGetSplitList (Transaction *trans);

/* These routines return the Num (or ID field), the description, 
 * the notes, and the date field.
 */
const char *  xaccTransGetNum (Transaction *trans);
const char *  xaccTransGetDescription (Transaction *trans);
const char *  xaccTransGetNotes (Transaction *trans);
time_t        xaccTransGetDate (Transaction *trans);

void          xaccTransGetDatePostedTS (Transaction *trans, Timespec *ts);

void          xaccTransGetDateEnteredTS (Transaction *trans, Timespec *ts);

Timespec      xaccTransRetDateEnteredTS (Transaction *trans);
Timespec      xaccTransRetDatePostedTS (Transaction *trans);

/* The xaccTransCountSplits() method returns the number of splits
 * in a transaction.
 */
int           xaccTransCountSplits (Transaction *trans);

/* --------------------------------------------------------------- */
/* Commmodity routines. Each transaction's 'currency' is by definition
 * the balancing common currency for the splits in that transaction.
 * */
gnc_commodity * xaccTransGetCurrency (Transaction *trans);
void xaccTransSetCurrency (Transaction *trans, gnc_commodity *curr);

/* The xaccTransGetImbalance() method returns the total value of the
 *    transaction.  In a pure double-entry system, this imbalance
 *    should be exactly zero, and if it is not, something is broken.
 *    However, when double-entry semantics are not enforced, unbalanced
 *    transactions can sneak in, and this routine can be used to find
 *    out how much things are off by.  The value returned is denominated
 *    in the currency that is returned by the xaccTransFindCommonCurrency()
 *    method.
 */
gnc_numeric xaccTransGetImbalance (Transaction * trans);

/* ------------- splits --------------- */
Split       * xaccMallocSplit (GNCSession *session);

gboolean xaccSplitEqual(const Split *sa, const Split *sb,
                        gboolean check_guids,
                        gboolean check_txn_splits);

/* Split slots are used to store arbitrary strings, numbers, and
 * structures which aren't members of the transaction struct.
 *
 * See kvp_doc.txt for reserved slot names.
 */
kvp_frame *xaccSplitGetSlots(Split *split);
void xaccSplitSetSlots_nc(Split *s, kvp_frame *frm);

/* The xaccSplitGetGUID() subroutine will return the
 *    globally unique id associated with that split.
 *    xaccSplitReturnGUID also returns the guid, but
 *    in a GUID struct.
 *
 * The xaccSplitLookup() subroutine will return the
 *    split associated with the given id, or NULL
 *    if there is no such split.
 */
const GUID * xaccSplitGetGUID (Split *split);
GUID         xaccSplitReturnGUID (Split *split);
Split      * xaccSplitLookup (const GUID *guid, GNCSession *session);

/* The memo is an arbitrary string associated with a split.
 *    Users typically type in free form text from the GUI.
 */
void          xaccSplitSetMemo (Split *split, const char *memo);

/* The Action is essentially an arbitrary string, but is 
 * meant to be conveniently limited to a menu of selections 
 * such as  "Buy", "Sell", "Interest", etc.  However,
 * as far as the engine is concerned, its an arbitrary string.
 */
void          xaccSplitSetAction (Split *split, const char *action);

/* The Reconcile is a single byte, whose values are typically
 * are "N", "C" and "R"
 */
void          xaccSplitSetReconcile (Split *split, char reconciled_flag);
void          xaccSplitSetDateReconciledSecs (Split *split, time_t time);
void          xaccSplitSetDateReconciledTS (Split *split, Timespec *ts);
void          xaccSplitGetDateReconciledTS (Split *split, Timespec *ts);
Timespec      xaccSplitRetDateReconciledTS (Split *split);

/* 
 * The following four functions set the prices and amounts.
 * All of the routines always maintain balance: that is, 
 * invoking any of them will cause other splits in the transaction
 * to be modified so that the net value of the transaction is zero. 
 *
 * IMPORTANT: The split should be parented by an account before
 * any of these routines are invoked!  This is because the actual
 * setting of amounts/values requires SCU settings from the account.
 * If these are not available, then amounts/values will be set to 
 * -1/0, which is an invalid value.  I beleive this order dependency
 * is a bug, but I'm too lazy to find, fix & test at the moment ... 
 *
 * The xaccSplitSetAmount() (formerly xaccSplitSetShareAmount) method
 *     sets the amount in the account's commodity that the split
 *     should have.
 *
 * The xaccSplitSetSharePrice() method sets the price of the
 *     split. DEPRECATED - set the value and amount instead.
 *
 * The xaccSplitSetValue() method adjusts the number of shares in 
 *     the split so that the number of shares times the share price
 *     equals the value passed in.
 *
 * The xaccSplitSetSharePriceAndAmount() method will simultaneously
 *     update the share price and the number of shares. This 
 *     is a utility routine that is equivalent to a xaccSplitSetSharePrice()
 *     followed by and xaccSplitSetAmount(), except that it incurs the
 *     processing overhead of balancing only once, instead of twice.  */

void         DxaccSplitSetSharePriceAndAmount (Split *split, double price,
                                               double amount);
void         DxaccSplitSetShareAmount (Split *split, double amount);
void         DxaccSplitSetSharePrice (Split *split, double price);
void         DxaccSplitSetValue (Split *split, double value);
void         DxaccSplitSetBaseValue (Split *split, double value,
                                     const gnc_commodity * base_currency);

void         xaccSplitSetSharePriceAndAmount (Split *split, gnc_numeric price,
                                              gnc_numeric amount);
void         xaccSplitSetAmount (Split *split, gnc_numeric amount);
void         xaccSplitSetSharePrice (Split *split, gnc_numeric price);
void         xaccSplitSetValue (Split *split, gnc_numeric value);
void         xaccSplitSetBaseValue (Split *split, gnc_numeric value,
                                    const gnc_commodity * base_currency);

/* The following four subroutines return the running balance up
 * to & including the indicated split.
 * 
 * The balance is the currency-denominated balance.  For accounts
 * with non-unit share prices, it is correctly adjusted for
 * share prices.
 * 
 * The share-balance is the number of shares. 
 * Price fluctuations do not change the share balance.
 * 
 * The cleared-balance is the currency-denominated balance 
 * of all transactions that have been marked as cleared or reconciled.
 * It is correctly adjusted for price fluctuations.
 * 
 * The reconciled-balance is the currency-denominated balance
 * of all transactions that have been marked as reconciled.
 */

gnc_numeric xaccSplitGetBalance (Split *split);
gnc_numeric xaccSplitGetClearedBalance (Split *split);
gnc_numeric xaccSplitGetReconciledBalance (Split *split);
gnc_numeric xaccSplitGetBaseValue (Split *split, 
                                   const gnc_commodity * base_currency);

/* return the parent transaction of the split */
Transaction * xaccSplitGetParent (Split *split);

/* return the memo, action strings */
const char *  xaccSplitGetMemo (Split *split);
const char *  xaccSplitGetAction (Split *split);

/* return the value of the reconcile flag */
char          xaccSplitGetReconcile (Split *split);
double        DxaccSplitGetShareAmount (Split * split);
double        DxaccSplitGetSharePrice (Split * split);
double        DxaccSplitGetValue (Split * split);

gnc_numeric   xaccSplitGetAmount (Split * split);
gnc_numeric   xaccSplitGetSharePrice (Split * split);
gnc_numeric   xaccSplitGetValue (Split * split);

Account *     xaccSplitGetAccount (Split *split);
const GUID *  xaccSplitGetAccountGUID(Split *split);
void          xaccSplitSetAccount(Split *s, Account *act);
void          xaccSplitSetAccountGUID(Split *s, GUID id);

/* split types: normal stock-split */
const char *xaccSplitGetType(const Split *s);

/* reconfgure a split to be a stock split - after this, you shouldn't
   mess with the value, just the damount. */
void xaccSplitMakeStockSplit(Split *s);

/********************************************************************\
 * sorting comparison function
 *
 * The xaccTransOrder(ta,tb) method is useful for sorting.
 *    return a negative value if transaction ta is dated earlier than tb, 
 *    return a positive value if transaction ta is dated later than tb,
 *    then compares num and description values, using the strcmp()
 *    c-library routine, returning  what strcmp would return.
 *    Finally, it returns zero if all of the above match.
 *    Note that it does *NOT* compare its member splits.
 *
 * The xaccSplitDateOrder(sa,sb) method is useful for sorting.
 *    if sa and sb have different transactions, return their xaccTransOrder
 *    return a negative value if split sa has a smaller currency-value than sb,
 *    return a positive value if split sa has a larger currency-value than sb,
 *    return a negative value if split sa has a smaller share-price than sb,  
 *    return a positive value if split sa has a larger share-price than sb,  
 *    then compares memo and action using the strcmp()
 *    c-library routine, returning  what strcmp would return.
 *    Then it compares the reconciled flags, then the reconciled dates,
 *    Finally, it returns zero if all of the above match.
 *
 */

int  xaccTransOrder     (Transaction *ta, Transaction *tb);
int  xaccSplitDateOrder (Split *sa, Split *sb);

/********************************************************************\
 * Miscellaneous utility routines.
\********************************************************************/

/*
 * These functions compare two splits by different criteria.  The *Other*
 * functions attempt to find the split on the other side of a transaction
 * and compare on it.  They return similar to strcmp.
 * 
 * These functions were added because converting strings to guile 
 * for comparisons in the transaction report is terribly inefficient.
 * More may be added here in future if it turns out that other types
 * of comparisons also induces guile slowdowns.
 */

int xaccSplitCompareAccountFullNames(Split *sa, Split *sb);
int xaccSplitCompareAccountCodes(Split *sa, Split *sb);
int xaccSplitCompareOtherAccountFullNames(Split *sa, Split *sb);
int xaccSplitCompareOtherAccountCodes(Split *sa, Split *sb);


/*
 * These functions take a split, get the corresponding split on the
 * "other side" of the transaction, and extract either the name or code
 * of that split, reverting to returning a constant "Split" if the 
 * transaction has more than one split on the "other side".  These
 * were added for the transaction report, and is in C because the code
 * was already written in C for the above functions and duplication 
 * is silly. 
 */

char * xaccSplitGetCorrAccountFullName(Split *sa, char seperator);
const char * xaccSplitGetCorrAccountName(Split *sa);
const char * xaccSplitGetCorrAccountCode(Split *sa);

/* 
 * The xaccGetAccountByName() is a convenience routine that 
 *    is essentially identical to xaccGetPeerAccountFromName(),
 *    except that it accepts the handy transaction as root.
 *
 * The xaccGetAccountByFullName routine is similar, but uses
 *    full names using the given separator.
 */
Account * xaccGetAccountByName (Transaction *trans, const char *name);
Account * xaccGetAccountByFullName (Transaction *trans,
                                    const char *name,
                                    const char separator);

/* 
 * The xaccSplitGetOtherSplit() is a convenience routine that returns
 *    the other of a pair of splits.  If there are more than two 
 *    splits, it returns NULL.
 */
Split * xaccSplitGetOtherSplit (Split *split);

/* The xaccIsPeerSplit() is a convenience routine that returns
 *    a non-zero value if the two splits share a common 
 *    parent transaction, else it returns zero.
 */
int xaccIsPeerSplit (Split *split_1, Split *split_2);


/*
 * xaccTransactionVoid voids a transaction.  A void transaction
 * has no values, is unaffected by reconciliation, and, by default
 * is not included in any queries.  A voided transaction 
 * should not be altered (and we'll try to make it so it can't be).
 * voiding is irreversible.  Once voided, a transaction cannot be
 * un-voided.
 */

void xaccTransVoid(Transaction *transaction, 
			 const char *reason);

gboolean xaccTransGetVoidStatus(Transaction *transaction);

char *xaccTransGetVoidReason(Transaction *transaction);

gnc_numeric xaccSplitVoidFormerAmount(Split *split);
gnc_numeric xaccSplitVoidFormerValue(Split *split);

Timespec xaccTransGetVoidTime(Transaction *tr);
#endif /* XACC_TRANSACTION_H */
