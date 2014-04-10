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
 *                                                                  *
\********************************************************************/
/** @addtogroup Engine
    @{ */
/** @file Transaction.h 
    @brief API for Transactions and Splits (journal entries)
    @author Copyright (C) 1997 Robin D. Clark
    @author Copyright (C) 1997-2001 Linas Vepstas <linas@linas.org>
*/

#ifndef XACC_TRANSACTION_H
#define XACC_TRANSACTION_H

#include <time.h>

#include "gnc-commodity.h"
#include "gnc-date.h"
#include "gnc-engine.h"
#include "guid.h"
#include "kvp_frame.h"
#include "qofbook.h"

/** @name Split Reconciled field values
    If you change these
    be sure to change gnc-ui-util.c:gnc_get_reconciled_str() and
    associated functions
*/
/**@{*/
#define CREC 'c'              /**< The Split has been cleared    */
#define YREC 'y'              /**< The Split has been reconciled */
#define FREC 'f'              /**< frozen into accounting period */
#define NREC 'n'              /**< not reconciled or cleared     */
#define VREC 'v'              /**< split is void                 */
/**@}*/

/** @name Transaction Type field values */
/**@{*/
#define TXN_TYPE_NONE	 '\0' /**< No transaction type       */
#define TXN_TYPE_INVOICE 'I'  /**< Transaction is an invoice */
#define TXN_TYPE_PAYMENT 'P'  /**< Transaction is a payment  */
/**@}*/


/** @name Configuration ForceDoubleEntry getters/setters
 */
/**@{*/
/**
 * The xaccConfigSetForceDoubleEntry() and xaccConfigGetForceDoubleEntry()
 *    set and get the "force_double_entry" flag.  This flag determines how
 *    the splits in a transaction will be balanced.
 *
 *    The following values have significance:
 *
 *    0 -- anything goes
 *
 *    1 -- The sum of all splits in a transaction will be
 *         forced to be zero, even if this requires the
 *         creation of additional splits.  Note that a split
 *         whose value is zero (e.g. a stock price) can exist
 *         by itself. Otherwise, all splits must come in at
 *         least pairs.
 *
 *    2 -- splits without parents will be forced into a
 *         lost & found account.  (Not implemented)
 */
void   xaccConfigSetForceDoubleEntry (int force);
/**
 * The xaccConfigSetForceDoubleEntry() and xaccConfigGetForceDoubleEntry()
 *    set and get the "force_double_entry" flag.  This flag determines how
 *    the splits in a transaction will be balanced.
 */
int    xaccConfigGetForceDoubleEntry (void);
/**@}*/


/***************************************************************
 * Transaction
 */

/** @name Transaction creation and editing */
/**@{*/
/** 
 The xaccMallocTransaction() will malloc memory and initialize it.
 Once created, it is usually unsafe to merely "free" this memory;
 the xaccTransDestroy() method should be called. */ 
Transaction * xaccMallocTransaction (QofBook *book); 

/**
 The xaccTransDestroy() method will remove all 
 of the splits from each of their accounts, free the memory
 associated with them.  This routine must be followed by either
 an xaccTransCommitEdit(), in which case the transaction 
 memory will be freed, or by xaccTransRollbackEdit(), in which 
 case nothing at all is freed, and everything is put back into 
 original order. */
void          xaccTransDestroy (Transaction *trans);

/**
 The xaccTransClone() method will create a complete copy of an
 existing transaction.
 */
Transaction * xaccTransClone (Transaction *t);

/** Equality.
 *
 * @param ta First transaction to compare
 * @param tb Second transaction to compare
 *
 * @param check_guids If TRUE, try a guid_equal() on the GUIDs of both
 * transactions if their pointers are not equal in the first place.
 * Also passed to subsidiary calls to xaccSplitEqual.
 *
 * @param check_splits If TRUE, after checking the transaction data
 * structures for equality, also check all splits attached to the
 * transation for equality.
 *
 * @param check_balances If TRUE, when checking splits also compare
 * balances between the two splits.  Balances are recalculated
 * whenever a split is added or removed from an account, so YMMV on
 * whether this should be set.
 *
 * @param assume_ordered If TRUE, assume that the splits in each
 * transaction appear in the same order.  This saves some time looking
 * up splits by GUID, and is required for checking duplicated
 * transactions because all the splits have new GUIDs.
 */
gboolean xaccTransEqual(const Transaction *ta,
                        const Transaction *tb,
                        gboolean check_guids,
                        gboolean check_splits,
                        gboolean check_balances,
                        gboolean assume_ordered);

/** The xaccTransBeginEdit() method must be called before any changes
    are made to a transaction or any of its component splits.  If 
    this is not done, errors will result. */
void          xaccTransBeginEdit (Transaction *trans);

/** The xaccTransCommitEdit() method indicates that the changes to the
    transaction and its splits are complete and should be made
    permanent. Note this routine may result in the deletion of the
    transaction, if the transaction is "empty" (has no splits), or
    of xaccTransDestroy() was called on the transaction. */
void          xaccTransCommitEdit (Transaction *trans);

/** The xaccTransRollbackEdit() routine rejects all edits made, and 
    sets the transaction back to where it was before the editing 
    started.  This includes restoring any deleted splits, removing
    any added splits, and undoing the effects of xaccTransDestroy,
    as well as restoring share quantities, memos, descriptions, etc. */
void          xaccTransRollbackEdit (Transaction *trans);

/** The xaccTransIsOpen() method returns TRUE if the transaction
    is open for editing. Otherwise, it returns false.  */
gboolean      xaccTransIsOpen (const Transaction *trans);

/** The xaccTransLookup() subroutine will return the
    transaction associated with the given id, or NULL
    if there is no such transaction. */
Transaction * xaccTransLookup (const GUID *guid, QofBook *book);
/** The xaccTransLookup() subroutine will return the
    transaction associated with the given id, or NULL
    if there is no such transaction. */
Transaction * xaccTransLookupDirect (GUID guid, QofBook *book);


/** \warning XXX FIXME 
 * gnc_book_count_transactions is a utility function, 
 * probably needs to be moved to a utility file somewhere.
 */
guint gnc_book_count_transactions(QofBook *book);

/**@}*/


/** @name Transaction general getters/setters */
/**@{*/
/** The xaccTransGetGUID() subroutine will return the
    globally unique id associated with that transaction. */
const GUID  * xaccTransGetGUID (const Transaction *trans);

/** xaccTransReturnGUID() will returns a GUID struct 
    associated with that transaction. */
GUID          xaccTransReturnGUID (const Transaction *trans);

/** Returns the book in which the transaction is stored */
QofBook *     xaccTransGetBook (const Transaction *trans);

/** Sorts the splits in a transaction, putting the debits first,
 *  followed by the credits.
 */
void          xaccTransSortSplits (Transaction *trans);

/** Print the transaction out to the console. Used for debugging.
 */
void          xaccTransDump (Transaction *trans, const char *tag);

/** Returns the transaction's KvpFrame slots.
 *
 Transaction slots are used to store arbitrary strings, numbers, and
 structures which aren't members of the transaction struct.  */
KvpFrame *xaccTransGetSlots(const Transaction *trans);

/** Set the KvpFrame slots of this transaction to the given frm by
 * directly using the frm pointer (i.e. non-copying). */
void xaccTransSetSlots_nc(Transaction *t, KvpFrame *frm);


/** Set the  Transaction Type
 *
 * See #define TXN_TYPE_NONE, TXN_TYPE_INVOICE and TXN_TYPE_PAYMENT */
void	      xaccTransSetTxnType (Transaction *trans, char type);
/** Returns the  Transaction Type
 *
 * See #define TXN_TYPE_NONE, TXN_TYPE_INVOICE and TXN_TYPE_PAYMENT */
char	      xaccTransGetTxnType (const Transaction *trans);


/** Sets the transaction Number (or ID) field*/
void          xaccTransSetNum (Transaction *trans, const char *num);
/** Sets the transaction Description */
void          xaccTransSetDescription (Transaction *trans, const char *desc);
/** Sets the transaction Notes
 *
 The Notes field is only visible in the register in double-line mode */
void          xaccTransSetNotes (Transaction *trans, const char *notes);

/** Gets the transaction Number (or ID) field*/
const char *  xaccTransGetNum (const Transaction *trans);
/** Gets the transaction Description */
const char *  xaccTransGetDescription (const Transaction *trans);
/** Gets the transaction Notes
 *
 The Notes field is only visible in the register in double-line mode */
const char *  xaccTransGetNotes (const Transaction *trans);


/** Add a split to the transaction
 * 
 The xaccTransAppendSplit() method will append the indicated 
 split to the collection of splits in this transaction.
 @note If the split is already a part of another transaction,
 it will be removed from that transaction first.
*/
void          xaccTransAppendSplit (Transaction *trans, Split *split);

/** The xaccTransGetSplit() method returns a pointer to each of the 
    splits in this transaction.
    @param trans The transaction  
    @param i The split number.  Valid values for i are zero to 
    (number_of__splits-1).  An invalid value of i will cause NULL to
    be returned.  A convenient way of cycling through all splits is
    to start at zero, and keep incrementing until a null value is returned. */
Split *       xaccTransGetSplit (const Transaction *trans, int i);

/** The xaccTransGetSplitList() method returns a GList of the splits
    in a transaction.  
    @return The list of splits. This list must NOT be modified.  Do *NOT* free
    this list when you are done with it. */
SplitList *   xaccTransGetSplitList (const Transaction *trans);


/** Set the transaction to be ReadOnly */
void          xaccTransSetReadOnly (Transaction *trans, const char *reason);
void	      xaccTransClearReadOnly (Transaction *trans);
/** FIXME: document me */
const char *  xaccTransGetReadOnly (const Transaction *trans);
/** FIXME: document me */
gboolean      xaccTransWarnReadOnly (const Transaction *trans);

/** Returns the number of splits in this transaction. */
int           xaccTransCountSplits (const Transaction *trans);

/** FIXME: document me */
gboolean      xaccTransHasReconciledSplits (const Transaction *trans);
/** FIXME: document me */
gboolean      xaccTransHasReconciledSplitsByAccount (const Transaction *trans,
						     const Account *account);

/** FIXME: document me */
gboolean      xaccTransHasSplitsInState (const Transaction *trans, const char state);
/** FIXME: document me */
gboolean      xaccTransHasSplitsInStateByAccount (const Transaction *trans,
						  const char state,
						  const Account *account);


/** Returns the valuation commodity of this transaction.
 *
 * Each transaction's valuation commodity, or 'currency' is, by definition,
 * the common currency in which all splits in the transaction can be valued.
 * The total value of the transaction must be zero when all splits 
 * are valued in this currency.
 * @note What happens if the Currency isn't set?  Ans: bad things.  */
gnc_commodity * xaccTransGetCurrency (const Transaction *trans);

/** Set the commodity of this transaction. */
void xaccTransSetCurrency (Transaction *trans, gnc_commodity *curr);

/** The xaccTransGetImbalance() method returns the total value of the
 * transaction.  In a pure double-entry system, this imbalance
 * should be exactly zero, and if it is not, something is broken.
 * However, when double-entry semantics are not enforced, unbalanced
 * transactions can sneak in, and this routine can be used to find
 * out how much things are off by.  The value returned is denominated
 * in the currency that is returned by the xaccTransFindCommonCurrency()
 * method. */
gnc_numeric xaccTransGetImbalance (const Transaction * trans);

/** The xaccTransGetAccountValue() method returns the total value applied
 *  to a particular account.  In some cases there may be multiple Splits
 *  in a single Transaction applied to one account (in particular when
 *  trying to balance Lots) -- this function is just a convienience to
 *  view everything at once.
 */
gnc_numeric xaccTransGetAccountValue (const Transaction *trans, 
				      const Account *account);

/**
 * The xaccTransOrder(ta,tb) method is useful for sorting.
 *    return a negative value if transaction ta is dated earlier than tb, 
 *    return a positive value if transaction ta is dated later than tb,
 *    then compares num and description values, using the strcmp()
 *    c-library routine, returning  what strcmp would return.
 *    Finally, it returns zero if all of the above match.
 *    Note that it does *NOT* compare its member splits.
 */
int  xaccTransOrder     (const Transaction *ta, const Transaction *tb);

/**@}*/


/** @name Transaction date setters/getters */
/**@{*/
   
/** The xaccTransSetDate() method does the same thing as
    xaccTransSetDate[Posted]Secs(), but takes a convenient
    day-month-year format.

 (Footnote: this shouldn't matter to a user, but anyone modifying
 the engine should understand that when xaccTransCommitEdit() is
 called, the date order of each of the component splits will be
 checked, and will be restored in ascending date order.)
 */
void          xaccTransSetDate (Transaction *trans,
                                int day, int mon, int year);

/** The xaccTransSetDatePostedSecs() method will modify the <i>posted</i>
    date of the transaction, specified by a time_t (see ctime(3)). The
    posted date is the date when this transaction was posted at the
    bank. */
#define xaccTransSetDateSecs xaccTransSetDatePostedSecs
void          xaccTransSetDatePostedSecs (Transaction *trans, time_t time);

/**  The xaccTransSetDatePostedTS() method does the same thing as
     xaccTransSetDatePostedSecs(), but takes a struct timespec64. */
void          xaccTransSetDatePostedTS (Transaction *trans,
                                        const Timespec *ts);

/** Modify the date of when the transaction was entered. The entered
 * date is the date when the register entry was made. */
void          xaccTransSetDateEnteredSecs (Transaction *trans, time_t time);
/** Modify the date of when the transaction was entered. The entered
 * date is the date when the register entry was made. */
void          xaccTransSetDateEnteredTS (Transaction *trans,
                                        const Timespec *ts);

/** Dates and txn-type for A/R and A/P "invoice" postings */
void	      xaccTransSetDateDueTS (Transaction *trans, const Timespec *ts);

/** Retrieve the posted date of the transaction. The posted date is
    the date when this transaction was posted at the bank. (Although
    having different function names, GetDate and GetDatePosted refer
    to the same single date.)*/
time_t        xaccTransGetDate (const Transaction *trans);
/** Retrieve the posted date of the transaction. The posted date is
    the date when this transaction was posted at the bank. (Although
    having different function names, GetDate and GetDatePosted refer
    to the same single date.)*/
void          xaccTransGetDatePostedTS (const Transaction *trans, Timespec *ts);
/** Retrieve the posted date of the transaction. The posted date is
    the date when this transaction was posted at the bank. (Although
    having different function names, GetDate and GetDatePosted refer
    to the same single date.)*/
Timespec      xaccTransRetDatePostedTS (const Transaction *trans);

/** Retrieve the date of when the transaction was entered. The entered
 * date is the date when the register entry was made.*/
void          xaccTransGetDateEnteredTS (const Transaction *trans, Timespec *ts);
/** Retrieve the date of when the transaction was entered. The entered
 * date is the date when the register entry was made.*/
Timespec      xaccTransRetDateEnteredTS (const Transaction *trans);

/** Dates and txn-type for A/R and A/P "invoice" postings */
Timespec      xaccTransRetDateDueTS (const Transaction *trans);
/** Dates and txn-type for A/R and A/P "invoice" postings */
void	      xaccTransGetDateDueTS (const Transaction *trans, Timespec *ts);

/**@}*/


/*-----------------------------------------------------------------------
 * Splits
 *-----------------------------------------------------------------------*/

/** @name Split general getters/setters */
/*@{*/

/** Constructor. */
Split       * xaccMallocSplit (QofBook *book);

/** Destructor.
 *
 * The xaccSplitDestroy() method will update its parent account and
 * transaction in a consistent manner, resulting in the complete
 * unlinking of the split, and the freeing of its associated memory.
 * The goal of this routine is to perform the removal and destruction
 * of the split in an atomic fashion, with no chance of accidentally
 * leaving the accounting structure out-of-balance or otherwise
 * inconsistent.
 *
 * If the deletion of the split leaves the transaction with no splits,
 * then the transaction will be marked for deletion. (It will not be
 * deleted until the xaccTransCommitEdit() routine is called.)
 *
 * @return TRUE upon successful deletion of the split. FALSE when
 * the parenting Transaction is a read-only one.
 */
gboolean      xaccSplitDestroy (Split *split);

/** Returns the book of this split, i.e. the entity where this split
 * is stored. */
QofBook *   xaccSplitGetBook (const Split *split);

/** Returns the account of this split, which was set through
 * xaccAccountInsertSplit(). */
Account *     xaccSplitGetAccount (const Split *split);

/** Returns the parent transaction of the split, which was set through
 * xaccTransAppendSplit(). */
Transaction * xaccSplitGetParent (const Split *split);

/** Returns the pointer to the debited/credited Lot where this split
 * belongs to, or NULL if it doesn't belong to any. */
GNCLot *      xaccSplitGetLot (const Split *split);


/** Returns the KvpFrame slots of this split for direct editing. 
 *
 * Split slots are used to store arbitrary strings, numbers, and
 * structures which aren't members of the transaction struct.  See
 * kvp_doc.txt for reserved slot names.
 */
KvpFrame *xaccSplitGetSlots(const Split *split);

/** Set the KvpFrame slots of this split to the given frm by directly
 * using the frm pointer (i.e. non-copying). */
void xaccSplitSetSlots_nc(Split *s, KvpFrame *frm);


/** The xaccSplitGetGUID() subroutine will return the
 *    globally unique id associated with that split. */
const GUID * xaccSplitGetGUID (const Split *split);

/** xaccSplitReturnGUID also returns the guid (globally unique id),
 * but in a GUID struct.*/
GUID         xaccSplitReturnGUID (const Split *split);


/** The memo is an arbitrary string associated with a split.  It is
 * intended to hold a short (zero to forty character) string that is
 * displayed by the GUI along with this split.  Users typically type
 * in free form text from the GUI.  */
void          xaccSplitSetMemo (Split *split, const char *memo);

/** Returns the memo string. */
const char *  xaccSplitGetMemo (const Split *split);

/** The Action is an arbitrary user-assigned string. 
 * The action field is an arbitrary user-assigned value.
 * It is meant to be a very short (one to ten cahracter) string that
 * signifies the "type" of this split, such as e.g. Buy, Sell, Div,
 * Withdraw, Deposit, ATM, Check, etc. The idea is that this field
 * can be used to create custom reports or graphs of data. */
void          xaccSplitSetAction (Split *split, const char *action);

/** Returns the action string. */
const char *  xaccSplitGetAction (const Split *split);
/**@}*/

/** @name Split Date getters/setters */
/**@{*/
/** Set the reconcile flag. The Reconcile flag is a single char, whose
 * values are typically are 'n', 'y', 'c'.  In Transaction.h, macros
 * are defined for typical values (e.g. CREC, YREC). */
void          xaccSplitSetReconcile (Split *split, char reconciled_flag);
/** Returns the value of the reconcile flag. */
char          xaccSplitGetReconcile (const Split *split);

/** Set the date on which this split was reconciled by specifying the
 * time as time_t. */
void          xaccSplitSetDateReconciledSecs (Split *split, time_t time);
/** Set the date on which this split was reconciled by specifying the
 * time as Timespec. */
void          xaccSplitSetDateReconciledTS (Split *split, Timespec *ts);
/** Get the date on which this split was reconciled by having it
 * written into the Timespec that 'ts' is pointing to. */
void          xaccSplitGetDateReconciledTS (const Split *split, 
					    Timespec *ts);
/** Returns the date (as Timespec) on which this split was reconciled. */
Timespec      xaccSplitRetDateReconciledTS (const Split *split);

/**@}*/


/** @name Split amount getters/setters 
 *
 * 'value' vs. 'amount' of a Split: The 'value' is the amount of the
 * _transaction_ balancing commodity (i.e. currency) involved,
 * 'amount' is the amount of the _account's_ commodity involved.
*/
/*@{*/

/** The xaccSplitSetAmount() (formerly xaccSplitSetShareAmount) method
 * sets the amount in the account's commodity that the split should
 * have.
 *
 * The following four setter functions set the prices and amounts.
 * All of the routines always maintain balance: that is, invoking any
 * of them will cause other splits in the transaction to be modified
 * so that the net value of the transaction is zero.
 *
 * IMPORTANT: The split should be parented by an account before
 * any of these routines are invoked!  This is because the actual
 * setting of amounts/values requires SCU settings from the account.
 * If these are not available, then amounts/values will be set to 
 * -1/0, which is an invalid value.  I beleive this order dependency
 * is a bug, but I'm too lazy to find, fix & test at the moment ... 
 *
 * @note If you use this on a newly created transaction, make sure
 * that the 'value' is also set so that it doesn't remain zero.
 */
void         xaccSplitSetAmount (Split *split, gnc_numeric amount);

/** Returns the amount of the split in the account's commodity. */
gnc_numeric   xaccSplitGetAmount (const Split * split);

/** The xaccSplitSetValue() method sets the value of this split in the
 * transaction's commodity. 
 *
 * @note If you use this on a newly created transaction, make sure
 * that the 'amount' is also set so that it doesn't remain zero.
 */
void         xaccSplitSetValue (Split *split, gnc_numeric value);

/** Returns the value of this split in the transaction's commodity. */
gnc_numeric   xaccSplitGetValue (const Split * split);

/** The xaccSplitSetSharePriceAndAmount() method will simultaneously
 * update the share price and the number of shares. This is a utility
 * routine that is equivalent to a xaccSplitSetSharePrice() followed
 * by and xaccSplitSetAmount(), except that it incurs the processing
 * overhead of balancing only once, instead of twice. */
void         xaccSplitSetSharePriceAndAmount (Split *split, 
					      gnc_numeric price,
					      gnc_numeric amount);

/** Returns the price of the split, that is, the value divided by the
 * amount. If the amount is zero, returns a gnc_numeric of value
 * one. */
gnc_numeric   xaccSplitGetSharePrice (const Split * split);

/** Depending on the base_currency, set either the value or the amount
 * of this split or both: If the base_currency is the transaction's
 * commodity, set the value.  If it is the account's commodity, set the
 * amount. If both, set both. 
 *
 * @note <b>WATCH OUT:</b> When using this function and the
 * transaction's and account's commodities are different, the amount
 * or the value will be left as zero. This might screw up the
 * multi-currency handling code in the register. So please think twice
 * whether you need this function -- using xaccSplitSetValue()
 * together with xaccSplitSetAmount() is definitely the better and
 * safer solution!
 */
void         xaccSplitSetBaseValue (Split *split, gnc_numeric value,
                                    const gnc_commodity * base_currency);

/** Depending on the base_currency, return either the value or the
 * amount of this split: If the base_curreny is the transaction's
 * commodity, return the value. If it is the account's commodity,
 * return the amount. If it is neither and the force_double_entry flag
 * is false, return the value. If is is neither and force_double_entry
 * is true, print a warning message and return gnc_numeric_zero(). 
 */
gnc_numeric xaccSplitGetBaseValue (const Split *split, 
                                   const gnc_commodity * base_currency);

/** Returns the running balance up to and including the indicated split. 
 * The balance is the currency-denominated balance.  For accounts
 * with non-unit share prices, it is correctly adjusted for
 * share prices.
 *
 * Returns the running balance up to & including the indicated split. 
 */
gnc_numeric xaccSplitGetBalance (const Split *split);

/**
 * The cleared-balance is the currency-denominated balance 
 * of all transactions that have been marked as cleared or reconciled.
 * It is correctly adjusted for price fluctuations.
 *
 * Returns the running balance up to & including the indicated split. 
 */
gnc_numeric xaccSplitGetClearedBalance (const Split *split);

/**
 * Returns the reconciled-balance of this split. The
 * reconciled-balance is the currency-denominated balance of all
 * transactions that have been marked as reconciled.
 *
 * Returns the running balance up to & including the indicated split. 
 */
gnc_numeric xaccSplitGetReconciledBalance (const Split *split);

/**@}*/



/** @name Split utility functions */
/**@{*/

/** Equality.
 *
 * @param sa First split to compare
 * @param sb Second split to compare
 *
 * @param check_guids If TRUE, try a guid_equal() on the GUIDs of both
 * splits if their pointers are not equal in the first place.
 *
 * @param check_balances If TRUE, compare balances between the two
 * splits.  Balances are recalculated whenever a split is added or
 * removed from an account, so YMMV on whether this should be set.
 *
 * @param check_txn_splits If the pointers are not equal, but
 * everything else so far is equal (including memo, amount, value,
 * kvp_frame), then, when comparing the parenting transactions with
 * xaccTransEqual(), set its argument check_splits to be TRUE.
 */
gboolean xaccSplitEqual(const Split *sa, const Split *sb,
                        gboolean check_guids,
                	gboolean check_balances,
                        gboolean check_txn_splits);

/** The xaccSplitLookup() subroutine will return the
 *    split associated with the given id, or NULL
 *    if there is no such split. */
Split      * xaccSplitLookup (const GUID *guid, QofBook *book);
/** Returns the split associated with the given id, or NULL if there
 * is no such split. */
Split      * xaccSplitLookupDirect (GUID guid, QofBook *book);


/** 
 * The xaccSplitGetOtherSplit() is a convenience routine that returns
 *    the other of a pair of splits.  If there are more than two 
 *    splits, it returns NULL.
 */
Split * xaccSplitGetOtherSplit (const Split *split);

/** The xaccIsPeerSplit() is a convenience routine that returns TRUE
 * (a non-zero value) if the two splits share a common parent
 * transaction, else it returns FALSE (zero).
 */
gboolean xaccIsPeerSplit (const Split *split_1, const Split *split_2);

/** Returns the split type, which is either the string "normal", or
 * "stock-split" for a split from a stock split (pun intended? :-).  */
const char *xaccSplitGetType(const Split *s);

/** Mark a split to be of type stock split - after this, you shouldn't
   modify the value anymore, just the amount. */
void xaccSplitMakeStockSplit(Split *s);

/**
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
 */
int  xaccSplitDateOrder (const Split *sa, const Split *sb);


/*
 * These functions compare two splits by different criteria.   
 *
 * These functions were added because converting strings to guile 
 * for comparisons in the transaction report is terribly inefficient.
 * More may be added here in future if it turns out that other types
 * of comparisons also induces guile slowdowns.
 */

/** Compare two splits by full name of account. Returns similar to
 * strcmp. */
int xaccSplitCompareAccountFullNames(Split *sa, Split *sb);
/** Compare two splits by code of account. Returns similar to
 * strcmp. */
int xaccSplitCompareAccountCodes(Split *sa, Split *sb);
/** Compare two splits by full name of the other account. Returns
 * similar to strcmp. This function attempts to find the split on the
 * other side of a transaction and compare on it. */
int xaccSplitCompareOtherAccountFullNames(Split *sa, Split *sb);
/** Compare two splits by code of the other account. Returns similar
 * to strcmp. This function attempts to find the split on the
 * other side of a transaction and compare on it. */
int xaccSplitCompareOtherAccountCodes(Split *sa, Split *sb);


/**
 * These functions take a split, get the corresponding split on the
 * "other side" of the transaction, and extract either the name or code
 * of that split, reverting to returning a constant "Split" if the 
 * transaction has more than one split on the "other side".  These
 * were added for the transaction report, and is in C because the code
 * was already written in C for the above functions and duplication 
 * is silly. 
 */

char * xaccSplitGetCorrAccountFullName(const Split *sa, char seperator);
/** document me */
const char * xaccSplitGetCorrAccountName(const Split *sa);
/** document me */
const char * xaccSplitGetCorrAccountCode(const Split *sa);

/*@}*/



/** @name Split deprecated functions */
/*@{*/

/** @deprecated The xaccSplitSetSharePrice() method sets the price of the
 * split. DEPRECATED - set the value and amount instead. */
void         xaccSplitSetSharePrice (Split *split, gnc_numeric price);

/** @deprecated Don't use doubles anymore, only use gnc_numerics. */
void         DxaccSplitSetAmount (Split *s, double damt); 
/** @deprecated Don't use doubles anymore, only use gnc_numerics. 
 *
 * WARNING:  The xaccSplitSetValue and DxaccSplitSetValue do NOT have the same
 * behavior.  The later divides the value given by the current value and set's 
 * the result as the new split value.  Is that a but or just strange undocumented
 * feature?  Benoit Grégoire 2002-6-12 */
void         DxaccSplitSetValue (Split *split, double value);
/** @deprecated Don't use doubles anymore, only use gnc_numerics. */
double        DxaccSplitGetValue (const Split * split);
/** @deprecated Don't use doubles anymore, only use gnc_numerics. */
void         DxaccSplitSetSharePriceAndAmount (Split *split, double price,
                                               double amount);
/** @deprecated Don't use doubles anymore, only use gnc_numerics. */
void         DxaccSplitSetShareAmount (Split *split, double amount);
/** @deprecated Don't use doubles anymore, only use gnc_numerics. */
double        DxaccSplitGetShareAmount (const Split * split);
/** @deprecated Don't use doubles anymore, only use gnc_numerics. */
void         DxaccSplitSetSharePrice (Split *split, double price);
/** @deprecated Don't use doubles anymore, only use gnc_numerics. */
double        DxaccSplitGetSharePrice (const Split * split);
/** @deprecated Don't use doubles anymore, only use gnc_numerics. */
void         DxaccSplitSetBaseValue (Split *split, double value,
                                     const gnc_commodity * base_currency);
/*@}*/




/********************************************************************\
 * Miscellaneous utility routines.
\********************************************************************/


/** The xaccGetAccountByName() is a convenience routine that 
 *  is essentially identical to xaccGetPeerAccountFromName(),
 *  except that it accepts the handy transaction as root.*/
Account * xaccGetAccountByName (Transaction *trans, const char *name);
/** The xaccGetAccountByFullName routine is similar to xaccGetAccountByName, but uses
 *  full names using the given separator.*/
Account * xaccGetAccountByFullName (Transaction *trans,
                                    const char *name,
                                    const char separator);


/** @name Transaction voiding */
/*@{*/
/** xaccTransVoid voids a transaction.  A void transaction has no
 *  values, is unaffected by reconciliation, and, by default is not
 *  included in any queries.  A voided transaction may not be altered.
 *
 *  @param transaction The transaction to void.
 *
 *  @param reason The textual reason why this transaction is being
 *  voided.
 */
void xaccTransVoid(Transaction *transaction, 
		   const char *reason);

/** xaccTransUnvoid restores a voided transaction to its original
 *  state.  At some point when gnucash is enhanced to support an audit
 *  trail (i.e. write only transactions) this command should be
 *  automatically disabled when the audit trail feature is enabled.
 *
 *  @param transaction The transaction to restore from voided state.
 */
void xaccTransUnvoid(Transaction *transaction);

/** xaccTransReverse inverts all the numerical values on the given
 *  transaction.  This function can be used after xaccTransClone
 *  to create a transaction that cancels out the effect of an
 *  earlier transaction.  This will be needed by write only accounts
 *  as a way to void a previous transaction (since you can't alter
 *  the existing transaction).
 *
 *  @param transaction The transaction to reverse.
 */
void xaccTransReverse(Transaction *transaction);

/** Retrieve information on whether or not a transaction has been voided.
 *
 *  @param transaction The transaction in question.
 *
 *  @return TRUE if the transaction is void, FALSE otherwise. Also
 *  returns FALSE upon an error.
 */
gboolean xaccTransGetVoidStatus(const Transaction *transaction);

/** Returns the user supplied textual reason why a transaction was
 *  voided.
 *
 *  @param transaction The transaction in question.
 *
 *  @return A pointer to the user supplied reason for voiding.
 */
char *xaccTransGetVoidReason(const Transaction *transaction);

/** Returns the original pre-void amount of a split.
 *
 *  @param split The split in question.
 *
 *  @return A gnc_numeric containing the original value of this split.
 *  Returns a gnc_numeric of zero upon error.
 */
gnc_numeric xaccSplitVoidFormerAmount(const Split *split);

/** Returns the original pre-void value of a split.
 *
 *  @param split The split in question.
 *
 *  @return A gnc_numeric containing the original amount of this split.
 *  Returns a gnc_numeric of zero upon error.
 */
gnc_numeric xaccSplitVoidFormerValue(const Split *split);

/** Returns the time that a transaction was voided.
 *
 *  @param split The transaction in question.
 *
 *  @return A Timespec containing the time that this transaction was
 *  voided. Returns a time of zero upon error.
 */
Timespec xaccTransGetVoidTime(const Transaction *tr);
/**@}*/

/** @name Split Parameter names
 * Note, if you want to get the equivalent of "ACCT_MATCH_ALL" you
 * need to create a search on the following parameter list:
 * SPLIT->SPLIT_TRANS->TRANS_SPLITLIST->SPLIT_ACCOUNT_GUID.  If you do
 * this, you might want to use the ACCOUNT_MATCH_ALL_TYPE as the
 * override so the gnome-search dialog displays the right type.
 */
/**@{*/
#define SPLIT_KVP		"kvp"

#define SPLIT_DATE_RECONCILED	"date-reconciled"
#define SPLIT_BALANCE		"balance"
#define SPLIT_CLEARED_BALANCE	"cleared-balance"
#define SPLIT_RECONCILED_BALANCE	"reconciled-balance"
#define SPLIT_MEMO		"memo"
#define SPLIT_ACTION		"action"
#define SPLIT_RECONCILE		"reconcile-flag"
#define SPLIT_AMOUNT		"amount"
#define SPLIT_SHARE_PRICE	"share-price"
#define SPLIT_VALUE		"value"
#define SPLIT_TYPE		"type"
#define SPLIT_VOIDED_AMOUNT	"voided-amount"
#define SPLIT_VOIDED_VALUE	"voided-value"
#define SPLIT_LOT		"lot"
#define SPLIT_TRANS		"trans"
#define SPLIT_ACCOUNT		"account"
#define SPLIT_ACCOUNT_GUID	"account-guid" /**< for guid_match_all */
/* used for SORTING ONLY */
#define SPLIT_ACCT_FULLNAME	"acct-fullname"
#define SPLIT_CORR_ACCT_NAME	"corr-acct-fullname"
#define SPLIT_CORR_ACCT_CODE	"corr-acct-code"
/**@}*/

/** @name Transaction Parameter names */
/**@{*/
#define TRANS_KVP		"kvp"
#define TRANS_NUM		"num"
#define TRANS_DESCRIPTION	"desc"
#define TRANS_DATE_ENTERED	"date-entered"
#define TRANS_DATE_POSTED	"date-posted"
#define TRANS_DATE_DUE		"date-due"
#define TRANS_IMBALANCE		"trans-imbalance"
#define TRANS_IS_BALANCED	"trans-balanced?"
#define TRANS_NOTES		"notes"
#define TRANS_TYPE		"type"
#define TRANS_VOID_STATUS	"void-p"
#define TRANS_VOID_REASON	"void-reason"
#define TRANS_VOID_TIME		"void-time"
#define TRANS_SPLITLIST		"split-list" /* for guid_match_all */
/**@}*/

#define RECONCILED_MATCH_TYPE	"reconciled-match"

#endif /* XACC_TRANSACTION_H */
/** @} */
