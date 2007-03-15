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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/
/** @addtogroup Engine
    @{ */
/** @addtogroup GncTransaction Financial Transactions
    A good overview of transactions, splits and GncAccounts can be 
    found in the texinfo documentation, together with an overview of
    how to use this API.

Splits, or "Ledger Entries" are the fundamental
GncAccounting units. Each Split consists of an amount (number of dollar
bills, number of shares, etc.), the value of that amount expressed in
a (possibly) different currency than the amount, a Memo, a pointer to
the parent GncTransaction, a pointer to the debited GncAccount, a reconciled
flag and timestamp, an "Action" field, and a key-value frame which can
store arbitrary data.
                                                                              
Transactions embody the notion of "double entry" GncAccounting. 
A GncTransaction consists of a date, a description, an ID number, 
a list of one or more Splits, and a key-value frame.  The transaction
also specifies the currency with which all of the splits will be valued.
When double-entry rules are enforced, the sum total value of the splits 
are zero.  If there are only two splits, then the value of one must be 
positive, the other negative: this denotes that one GncAccount is debited, 
and another is credited by an equal amount.  By forcing the value of the
splits to always 'add up' to zero, we can guarantee that the balances
of the GncAccounts are always correctly balanced.

The engine does not enforce double-entry GncAccounting, but provides an API
to enable user-code to find unbalanced transactions and 'repair' them so
that they are in balance. 

Note the sum of the values of Splits in a GncTransaction is always computed
with respect to a currency; thus splits can be balanced even when they
are in different currencies, as long as they share a common currency.
This feature allows currency-trading GncAccounts to be established.
                                                                              
Every Split must point to its parent GncTransaction, and that GncTransaction
must in turn include that Split in the GncTransaction's list of Splits. A
Split can belong to at most one GncTransaction. These relationships are
enforced by the engine. The engine user cannnot accidentally destroy
this relationship as long as they stick to using the API and never
access internal structures directly.

Splits are grouped into GncAccounts which are also known
as "Ledgers" in GncAccounting practice. Each GncAccount consists of a list of
Splits that debit that GncAccount. To ensure consistency, if a Split points
to an GncAccount, then the GncAccount must point to the Split, and vice-versa.
A Split can belong to at most one GncAccount. Besides merely containing a
list of Splits, the GncAccount structure also gives the GncAccount a name, a
code number, description and notes fields, a key-value frame, a pointer
to the commodity that is used for all splits in this GncAccount. The
commodity can be the name of anything traded and tradable: a stock 
(e.g. "IBM", "McDonald's"), a currency (e.g. "USD", "GBP"), or anything
added to the commodity table.  

GncAccounts can be arranged in a hierarchical tree. The nodes of the tree
are called "GncAccount Groups". By GncAccounting
convention, the value of an GncAccount is equal to the value of all of its
Splits plus the value of all of its sub-GncAccounts.

    @{ */
/** @file GncTransaction.h 
    @brief API for Transactions and Splits (journal entries)
    @author Copyright (C) 1997 Robin D. Clark
    @author Copyright (C) 1997-2001 Linas Vepstas <linas@linas.org>
*/

#ifndef XACC_TRANSACTION_H
#define XACC_TRANSACTION_H

#include <time.h>

#include "gnc-commodity.h"
#include "gnc-engine.h"
#include "Split.h"
#include "Account.h"


/** @name GncTransaction Type field values
@{
*/
#define TXN_TYPE_NONE	 '\0' /**< No transaction type       */
#define TXN_TYPE_INVOICE 'I'  /**< GncTransaction is an invoice */
#define TXN_TYPE_PAYMENT 'P'  /**< GncTransaction is a payment  */
/** @} */

/* --------------------------------------------------------------- */
/* Transactions */

/** @name GncTransaction creation and editing
 @{
*/
/** 
 The xaccMallocTransaction() will malloc memory and initialize it.
 Once created, it is usually unsafe to merely "free" this memory;
 the xaccTransDestroy() method should be called. */ 
GncTransaction * xaccMallocTransaction (QofBook *book); 

/**
 The xaccTransDestroy() method will remove all 
 of the splits from each of their GncAccounts, free the memory
 associated with them.  This routine must be followed by either
 an xaccTransCommitEdit(), in which case the transaction 
 memory will be freed, or by xaccTransRollbackEdit(), in which 
 case nothing at all is freed, and everything is put back into 
 original order. */
void          xaccTransDestroy (GncTransaction *trans);

/**
 The xaccTransClone() method will create a complete copy of an
 existing transaction.
 */
GncTransaction * xaccTransClone (const GncTransaction *t);

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
 * whenever a split is added or removed from an GncAccount, so YMMV on
 * whether this should be set.
 *
 * @param assume_ordered If TRUE, assume that the splits in each
 * transaction appear in the same order.  This saves some time looking
 * up splits by GUID, and is required for checking duplicated
 * transactions because all the splits have new GUIDs.
 */
gboolean xaccTransEqual(const GncTransaction *ta,
                        const GncTransaction *tb,
                        gboolean check_guids,
                        gboolean check_splits,
                        gboolean check_balances,
                        gboolean assume_ordered);

/** The xaccTransBeginEdit() method must be called before any changes
    are made to a transaction or any of its component splits.  If 
    this is not done, errors will result. */
void          xaccTransBeginEdit (GncTransaction *trans);

/** The xaccTransCommitEdit() method indicates that the changes to the
    transaction and its splits are complete and should be made
    permanent. Note this routine may result in the deletion of the
    transaction, if the transaction is "empty" (has no splits), or
    of xaccTransDestroy() was called on the transaction. */
void          xaccTransCommitEdit (GncTransaction *trans);

/** The xaccTransRollbackEdit() routine rejects all edits made, and 
    sets the transaction back to where it was before the editing 
    started.  This includes restoring any deleted splits, removing
    any added splits, and undoing the effects of xaccTransDestroy,
    as well as restoring share quantities, memos, descriptions, etc. */
void          xaccTransRollbackEdit (GncTransaction *trans);

/** The xaccTransIsOpen() method returns TRUE if the transaction
    is open for editing. Otherwise, it returns false.  
    XXX this routne should probably be deprecated.  its, umm,
    hard to imagine legitamate uses (but it is used by
    the import/export code for reasons I can't understand.)
 */
gboolean      xaccTransIsOpen (const GncTransaction *trans);

/** The xaccTransLookup() subroutine will return the
    transaction associated with the given id, or NULL
    if there is no such transaction. */
GncTransaction * xaccTransLookup (const GUID *guid, QofBook *book);
#define xaccTransLookupDirect(g,b) xaccTransLookup(&(g),b)

Split * xaccTransFindSplitByAccount(const GncTransaction *trans, 
                                    const GncAccount *acc);

/** The xaccTransScrubGains() routine performs a number of cleanup
 *  functions on the indicated transaction, with the end-goal of
 *  setting up a consistent set of gains/losses for all the splits
 *  in the transaction.  This includes making sure that the lot
 *  assignments of all the splits are good, and that the lots 
 *  balance appropriately.
 */
void xaccTransScrubGains (GncTransaction *trans, GncAccount *gain_acc);


/** \warning XXX FIXME 
 * gnc_book_count_transactions is a utility function, 
 * probably needs to be moved to a utility file somewhere.
 */
guint gnc_book_count_transactions(QofBook *book);

/** @} */


/** @name GncTransaction general getters/setters
 @{
*/

/** Sorts the splits in a transaction, putting the debits first,
 *  followed by the credits.
 */
void          xaccTransSortSplits (GncTransaction *trans);

/** Set the  GncTransaction Type
 *
 * See #TXN_TYPE_NONE, #TXN_TYPE_INVOICE and #TXN_TYPE_PAYMENT */
void	      xaccTransSetTxnType (GncTransaction *trans, char type);
/** Returns the  GncTransaction Type
 *
 * See #TXN_TYPE_NONE, #TXN_TYPE_INVOICE and #TXN_TYPE_PAYMENT */
char	      xaccTransGetTxnType (const GncTransaction *trans);


/** Sets the transaction Number (or ID) field*/
void          xaccTransSetNum (GncTransaction *trans, const char *num);
/** Sets the transaction Description */
void          xaccTransSetDescription (GncTransaction *trans, const char *desc);
/** Sets the transaction Notes
 *
 The Notes field is only visible in the register in double-line mode */
void          xaccTransSetNotes (GncTransaction *trans, const char *notes);

/** Gets the transaction Number (or ID) field*/
const char *  xaccTransGetNum (const GncTransaction *trans);
/** Gets the transaction Description */
const char *  xaccTransGetDescription (const GncTransaction *trans);
/** Gets the transaction Notes
 *
 The Notes field is only visible in the register in double-line mode */
const char *  xaccTransGetNotes (const GncTransaction *trans);


/** Add a split to the transaction
 * 
 The xaccTransAppendSplit() method will append the indicated 
 split to the collection of splits in this transaction.
 @note If the split is already a part of another transaction,
 it will be removed from that transaction first.
*/
#define xaccTransAppendSplit(t, s) xaccSplitSetParent((s), (t))

/** The xaccTransGetSplit() method returns a pointer to each of the 
    splits in this transaction.
    @param trans The transaction  
    @param i The split number.  Valid values for i are zero to 
    (number_of__splits-1).  An invalid value of i will cause NULL to
    be returned.  A convenient way of cycling through all splits is
    to start at zero, and keep incrementing until a null value is returned. */
Split *       xaccTransGetSplit (const GncTransaction *trans, int i);

/** Inverse of xaccTransGetSplit() */
int xaccTransGetSplitIndex(const GncTransaction *trans, const Split *split);

/** The xaccTransGetSplitList() method returns a GList of the splits
    in a transaction.  
    @return The list of splits. This list must NOT be modified.  Do *NOT* free
    this list when you are done with it. */
SplitList *   xaccTransGetSplitList (const GncTransaction *trans);
gboolean xaccTransStillHasSplit(const GncTransaction *trans, const Split *s);


/** Set the transaction to be ReadOnly */
void          xaccTransSetReadOnly (GncTransaction *trans, const char *reason);
void	      xaccTransClearReadOnly (GncTransaction *trans);
/** FIXME: document me */
const char *  xaccTransGetReadOnly (const GncTransaction *trans);

/** Returns the number of splits in this transaction. */
int           xaccTransCountSplits (const GncTransaction *trans);

/** FIXME: document me */
gboolean      xaccTransHasReconciledSplits (const GncTransaction *trans);
/** FIXME: document me */
gboolean      xaccTransHasReconciledSplitsByAccount (const GncTransaction *trans,
						     const GncAccount *GncAccount);

/** FIXME: document me */
gboolean      xaccTransHasSplitsInState (const GncTransaction *trans, const char state);
/** FIXME: document me */
gboolean      xaccTransHasSplitsInStateByAccount (const GncTransaction *trans,
						  const char state,
						  const GncAccount *GncAccount);


/** Returns the valuation commodity of this transaction.
 *
 * Each transaction's valuation commodity, or 'currency' is, by definition,
 * the common currency in which all splits in the transaction can be valued.
 * The total value of the transaction must be zero when all splits 
 * are valued in this currency.
 * @note What happens if the Currency isn't set?  Ans: bad things.  */
gnc_commodity * xaccTransGetCurrency (const GncTransaction *trans);

/** Set the commodity of this transaction. */
void xaccTransSetCurrency (GncTransaction *trans, gnc_commodity *curr);

/** The xaccTransGetImbalance() method returns the total value of the
 * transaction.  In a pure double-entry system, this imbalance
 * should be exactly zero, and if it is not, something is broken.
 * However, when double-entry semantics are not enforced, unbalanced
 * transactions can sneak in, and this routine can be used to find
 * out how much things are off by.  The value returned is denominated
 * in the currency that is returned by the xaccTransFindCommonCurrency()
 * method. */
gnc_numeric xaccTransGetImbalance (const GncTransaction * trans);

/** The xaccTransGetGncAccountValue() method returns the total value applied
 *  to a particular GncAccount.  In some cases there may be multiple Splits
 *  in a single GncTransaction applied to one GncAccount (in particular when
 *  trying to balance Lots) -- this function is just a convienience to
 *  view everything at once.
 */
gnc_numeric xaccTransGetAccountValue (const GncTransaction *trans, 
				      const GncAccount *GncAccount);

/** Same as xaccTransGetGncAccountValue, but uses the GncAccount's commodity. */
gnc_numeric xaccTransGetAccountAmount (const GncTransaction *trans,
                                       const GncAccount *GncAccount);

/* Compute the conversion rate for the transaction to this GncAccount.
 * Any "split value" (which is in the transaction currency),
 * multiplied by this conversion rate, will give you the value you
 * should display for this GncAccount.
 *
 * If 'acc' is NULL, return unity.
 */
gnc_numeric xaccTransGetAccountConvRate(GncTransaction *txn, GncAccount *acc);

/** Get the GncAccount balance for the specified GncAccount after the last
    split in the specified transaction. */
gnc_numeric xaccTransGetAccountBalance (const GncTransaction *trans,
                                        const GncAccount *GncAccount);

/**
 * The xaccTransOrder(ta,tb) method is useful for sorting.
 *    Orders ta and tb
 *      return <0 if ta sorts before tb
 *      return >0 if ta sorts after tb
 *      return 0 if they are absolutely equal
 *
 *    The comparrison uses the following fields, in order:
 *      date posted  (compare as a date)
 *      num field (compare as an integer)
 *      date entered (compare as a date)
 *      description field (comcpare as a string using strcmp())
 *      GUID (compare as a guid)
 *    Finally, it returns zero if all of the above match.
 *    Note that it does *NOT* compare its member splits.
 */
int  xaccTransOrder     (const GncTransaction *ta, const GncTransaction *tb);

/** @} */


/** @name GncTransaction date setters/getters
@{
*/
   
/** The xaccTransSetDate() method does the same thing as
    xaccTransSetDate[Posted]Secs(), but takes a convenient
    day-month-year format.

 (Footnote: this shouldn't matter to a user, but anyone modifying
 the engine should understand that when xaccTransCommitEdit() is
 called, the date order of each of the component splits will be
 checked, and will be restored in ascending date order.)
 */
void          xaccTransSetDate (GncTransaction *trans,
                                int day, int mon, int year);

/** The xaccTransSetDatePostedSecs() method will modify the <i>posted</i>
    date of the transaction, specified by a time_t (see ctime(3)). The
    posted date is the date when this transaction was posted at the
    bank. */
#define xaccTransSetDateSecs xaccTransSetDatePostedSecs
void          xaccTransSetDatePostedSecs (GncTransaction *trans, time_t time);

/**  The xaccTransSetDatePostedTS() method does the same thing as
     xaccTransSetDatePostedSecs(), but takes a struct timespec64. */
void          xaccTransSetDatePostedTS (GncTransaction *trans,
                                        const Timespec *ts);

/** Modify the date of when the transaction was entered. The entered
 * date is the date when the register entry was made. */
void          xaccTransSetDateEnteredSecs (GncTransaction *trans, time_t time);
/** Modify the date of when the transaction was entered. The entered
 * date is the date when the register entry was made. */
void          xaccTransSetDateEnteredTS (GncTransaction *trans,
                                        const Timespec *ts);

/** Dates and txn-type for A/R and A/P "invoice" postings */
void	      xaccTransSetDateDueTS (GncTransaction *trans, const Timespec *ts);

/** Retrieve the posted date of the transaction. The posted date is
    the date when this transaction was posted at the bank. (Although
    having different function names, GetDate and GetDatePosted refer
    to the same single date.)*/
time_t        xaccTransGetDate (const GncTransaction *trans);
/** Retrieve the posted date of the transaction. The posted date is
    the date when this transaction was posted at the bank. (Although
    having different function names, GetDate and GetDatePosted refer
    to the same single date.)*/
void          xaccTransGetDatePostedTS (const GncTransaction *trans, Timespec *ts);
/** Retrieve the posted date of the transaction. The posted date is
    the date when this transaction was posted at the bank. (Although
    having different function names, GetDate and GetDatePosted refer
    to the same single date.)*/
Timespec      xaccTransRetDatePostedTS (const GncTransaction *trans);

/** Retrieve the date of when the transaction was entered. The entered
 * date is the date when the register entry was made.*/
void          xaccTransGetDateEnteredTS (const GncTransaction *trans, Timespec *ts);
/** Retrieve the date of when the transaction was entered. The entered
 * date is the date when the register entry was made.*/
Timespec      xaccTransRetDateEnteredTS (const GncTransaction *trans);

/** Dates and txn-type for A/R and A/P "invoice" postings */
Timespec      xaccTransRetDateDueTS (const GncTransaction *trans);
/** Dates and txn-type for A/R and A/P "invoice" postings */
void	      xaccTransGetDateDueTS (const GncTransaction *trans, Timespec *ts);
/** @} */



/********************************************************************\
 * Miscellaneous utility routines.
\********************************************************************/


/** The xaccGetGncAccountByName() is a convenience routine that 
 *  is essentially identical to xaccGetPeerGncAccountFromName(),
 *  except that it accepts the handy transaction as root.*/
GncAccount * xaccGetAccountByName (const GncTransaction *trans, const char *name);
/** The xaccGetGncAccountByFullName routine is similar to xaccGetGncAccountByName, but uses
 *  full names using the given separator.*/
GncAccount * xaccGetAccountByFullName (const GncTransaction *trans,
                                    const char *name);


/** @name GncTransaction voiding
@{
*/
/** xaccTransVoid voids a transaction.  A void transaction has no
 *  values, is unaffected by reconciliation, and, by default is not
 *  included in any queries.  A voided transaction may not be altered.
 *
 *  @param transaction The transaction to void.
 *
 *  @param reason The textual reason why this transaction is being
 *  voided.
 */
void xaccTransVoid(GncTransaction *transaction, 
		   const char *reason);

/** xaccTransUnvoid restores a voided transaction to its original
 *  state.  At some point when gnucash is enhanced to support an audit
 *  trail (i.e. write only transactions) this command should be
 *  automatically disabled when the audit trail feature is enabled.
 *
 *  @param transaction The transaction to restore from voided state.
 */
void xaccTransUnvoid(GncTransaction *transaction);

/** xaccTransReverse creates a GncTransaction that reverses the given
 *  tranaction by inverting all the numerical values in the given
 *  transaction.  This function cancels out the effect of an earlier
 *  transaction.  This will be needed by write only GncAccounts as a way
 *  to void a previous transaction (since you can't alter the existing
 *  transaction).
 *
 *  @param transaction The transaction to create a reverse of.
 *
 *  @return a new transaction which reverses the given transaction
 */
GncTransaction * xaccTransReverse(GncTransaction *transaction);

/** Returns the transaction that reversed the given transaction.
 *
 *  @param trans a GncTransaction that has been reversed
 *
 *  @param the transaction that reversed the given transaction, or
 *  NULL if the given transaction has not been reversed.
 */
GncTransaction * xaccTransGetReversedBy(const GncTransaction *trans);

/** Retrieve information on whether or not a transaction has been voided.
 *
 *  @param transaction The transaction in question.
 *
 *  @return TRUE if the transaction is void, FALSE otherwise. Also
 *  returns FALSE upon an error.
 */
gboolean xaccTransGetVoidStatus(const GncTransaction *transaction);

/** Returns the user supplied textual reason why a transaction was
 *  voided.
 *
 *  @param transaction The transaction in question.
 *
 *  @return A pointer to the user supplied reason for voiding.
 */
const char *xaccTransGetVoidReason(const GncTransaction *transaction);

/** Returns the time that a transaction was voided.
 *
 *  @param tr The transaction in question.
 *
 *  @return A Timespec containing the time that this transaction was
 *  voided. Returns a time of zero upon error.
 */
Timespec xaccTransGetVoidTime(const GncTransaction *tr);
/** @} */

/** @name GncTransaction Parameter names
@{
*/
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

/** \deprecated */
#define xaccTransGetBook(X)      qof_instance_get_book (QOF_INSTANCE(X))
/** \deprecated */
#define xaccTransGetGUID(X)      qof_instance_get_guid(QOF_INSTANCE(X))
/** \deprecated */
#define xaccTransReturnGUID(X) (X ? *(qof_instance_get_guid(QOF_INSTANCE(X))) : *(guid_null()))
/** \deprecated */
#define xaccTransGetSlots(X)     qof_instance_get_slots (QOF_INSTANCE(X))

#endif /* XACC_TRANSACTION_H */
/** @} */
/** @} */
