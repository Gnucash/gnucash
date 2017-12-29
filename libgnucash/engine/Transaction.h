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
/** @addtogroup Transaction Transaction, Split
    A good overview of transactions, splits and accounts can be
    found in the texinfo documentation, together with an overview of
    how to use this API.

Splits, or "Ledger Entries" are the fundamental
accounting units. Each Split consists of an amount (number of dollar
bills, number of shares, etc.), the value of that amount expressed in
a (possibly) different currency than the amount, a Memo, a pointer to
the parent Transaction, a pointer to the debited Account, a reconciled
flag and timestamp, an "Action" field, and a key-value frame which can
store arbitrary data.

Transactions embody the notion of "double entry" accounting.
A Transaction consists of a date, a description, an ID number,
a list of one or more Splits, and a key-value frame.  The transaction
also specifies the currency with which all of the splits will be valued.
When double-entry rules are enforced, the sum total value of the splits
are zero.  If there are only two splits, then the value of one must be
positive, the other negative: this denotes that one account is debited,
and another is credited by an equal amount.  By forcing the value of the
splits to always 'add up' to zero, we can guarantee that the balances
of the accounts are always correctly balanced.

The engine does not enforce double-entry accounting, but provides an API
to enable user-code to find unbalanced transactions and 'repair' them so
that they are in balance.

Note the sum of the values of Splits in a Transaction is always computed
with respect to a currency; thus splits can be balanced even when they
are in different currencies, as long as they share a common currency.
This feature allows currency-trading accounts to be established.

Every Split must point to its parent Transaction, and that Transaction
must in turn include that Split in the Transaction's list of Splits. A
Split can belong to at most one Transaction. These relationships are
enforced by the engine. The engine user cannnot accidentally destroy
this relationship as long as they stick to using the API and never
access internal structures directly.

Splits are grouped into Accounts which are also known
as "Ledgers" in accounting practice. Each Account consists of a list of
Splits that debit that Account. To ensure consistency, if a Split points
to an Account, then the Account must point to the Split, and vice-versa.
A Split can belong to at most one Account. Besides merely containing a
list of Splits, the Account structure also gives the Account a name, a
code number, description and notes fields, a key-value frame, a pointer
to the commodity that is used for all splits in this account. The
commodity can be the name of anything traded and tradable: a stock
(e.g. "IBM", "McDonald's"), a currency (e.g. "USD", "GBP"), or anything
added to the commodity table.

Accounts can be arranged in a hierarchical tree. The nodes of the tree
are called "Account Groups". By accounting
convention, the value of an Account is equal to the value of all of its
Splits plus the value of all of its sub-Accounts.

    @{ */
/** @file Transaction.h
    @brief API for Transactions and Splits (journal entries)
    @author Copyright (C) 1997 Robin D. Clark
    @author Copyright (C) 1997-2001 Linas Vepstas <linas@linas.org>
*/

#ifndef XACC_TRANSACTION_H
#define XACC_TRANSACTION_H

typedef struct _TransactionClass TransactionClass;

#include <time.h>

#include "gnc-commodity.h"
#include "gnc-engine.h"
#include "Split.h"

#ifdef __cplusplus
extern "C" {
#endif

/* --- type macros --- */
#define GNC_TYPE_TRANSACTION            (gnc_transaction_get_type ())
#define GNC_TRANSACTION(o)              \
     (G_TYPE_CHECK_INSTANCE_CAST ((o), GNC_TYPE_TRANSACTION, Transaction))
#define GNC_TRANSACTION_CLASS(k)        \
     (G_TYPE_CHECK_CLASS_CAST((k), GNC_TYPE_TRANSACTION, TransactionClass))
#define GNC_IS_TRANSACTION(o)           \
     (G_TYPE_CHECK_INSTANCE_TYPE ((o), GNC_TYPE_TRANSACTION))
#define GNC_IS_TRANSACTION_CLASS(k)     \
     (G_TYPE_CHECK_CLASS_TYPE ((k), GNC_TYPE_TRANSACTION))
#define GNC_TRANSACTION_GET_CLASS(o)    \
     (G_TYPE_INSTANCE_GET_CLASS ((o), GNC_TYPE_TRANSACTION, TransactionClass))
GType gnc_transaction_get_type(void);


/* FIXME: These macros are not consistent with the type name */
#define GNC_IS_TRANS(obj)  GNC_IS_TRANSACTION(obj)
#define GNC_TRANS(obj)     GNC_TRANSACTION(obj)

/** @name Transaction Type field values
@{
*/
#define TXN_TYPE_NONE	 '\0' /**< No transaction type       */
#define TXN_TYPE_INVOICE 'I'  /**< Transaction is an invoice */
#define TXN_TYPE_PAYMENT 'P'  /**< Transaction is a payment  */
#define TXN_TYPE_LINK    'L'  /**< Transaction is a link between (invoice and payment) lots  */
/** @} */

/* --------------------------------------------------------------- */
/* Transactions */

/** @name Transaction creation and editing
 @{
*/
/**
 The xaccMallocTransaction() will malloc memory and initialize it.
 Once created, it is usually unsafe to merely "free" this memory;
 the xaccTransDestroy() method should be called. */
Transaction * xaccMallocTransaction (QofBook *book);

/** Destroys a transaction.
 *  Each split in transaction @a trans is removed from its
 *  account and destroyed as well.
 *
 *  If the transaction has not already been opened for editing with
 *  ::xaccTransBeginEdit() then the changes are committed immediately.
 *  Otherwise, the caller must follow up with either
 *  ::xaccTransCommitEdit(), in which case the transaction and
 *  split memory will be freed, or xaccTransRollbackEdit(), in which
 *  case nothing at all is freed, and everything is put back into
 *  original order.
 *
 *  @param trans the transaction to destroy
 */
void          xaccTransDestroy (Transaction *trans);

/**
 The xaccTransClone() method will create a complete copy of an
 existing transaction.
 */
Transaction * xaccTransClone (const Transaction *t);

/**
 The xaccTransCloneNoKvp() method will create a complete copy of an
 existing transaction except that the KVP slots will be empty.
 */
Transaction * xaccTransCloneNoKvp (const Transaction *t);

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
 * up splits by GncGUID, and is required for checking duplicated
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
    is open for editing. Otherwise, it returns false.
    XXX this routine should probably be deprecated.  its, umm,
    hard to imagine legitimate uses (but it is used by
    the import/export code for reasons I can't understand.)
 */
gboolean      xaccTransIsOpen (const Transaction *trans);

/** The xaccTransLookup() subroutine will return the
    transaction associated with the given id, or NULL
    if there is no such transaction. */
/*@ dependent @*//*@ null @*/
Transaction * xaccTransLookup (const GncGUID *guid, QofBook *book);
#define xaccTransLookupDirect(g,b) xaccTransLookup(&(g),b)

/*################## Added for Reg2 #################*/

/** Copy a transaction to the 'clipboard' transaction using
 *  dupe_transaction. The 'clipboard' transaction must never
 *  be dereferenced.
 */
Transaction * xaccTransCopyToClipBoard(const Transaction *from_trans);

/** Copy a transaction to another using the function below without
 *  changing any account information.
 */
void xaccTransCopyOnto(const Transaction *from_trans, Transaction *to_trans);

/** This function explicitly must robustly handle some unusual input.
 *
 *  'from_trans' may be a duped trans (see xaccDupeTransaction), so its
 *   splits may not really belong to the accounts that they say they do.
 *
 *  'from_acc' need not be a valid account. It may be an already freed
 *   Account. Therefore, it must not be dereferenced at all.
 *
 *   Neither 'from_trans', nor 'from_acc', nor any of 'from's splits may be modified
 *   in any way.
 *
 *   'no_date' if TRUE will not copy the date posted.
 *
 *   The 'to_trans' transaction will end up with valid copies of from's
 *   splits.  In addition, the copies of any of from's splits that were
 *   in from_acc (or at least claimed to be) will end up in to_acc.
 */
void xaccTransCopyFromClipBoard(const Transaction *from_trans, Transaction *to_trans,
                           const Account *from_acc, Account *to_acc, gboolean no_date);

/*################## Added for Reg2 #################*/


Split * xaccTransFindSplitByAccount(const Transaction *trans,
                                    const Account *acc);

/** The xaccTransScrubGains() routine performs a number of cleanup
 *  functions on the indicated transaction, with the end-goal of
 *  setting up a consistent set of gains/losses for all the splits
 *  in the transaction.  This includes making sure that the lot
 *  assignments of all the splits are good, and that the lots
 *  balance appropriately.
 */
void xaccTransScrubGains (Transaction *trans, Account *gain_acc);


/** \warning XXX FIXME
 * gnc_book_count_transactions is a utility function,
 * probably needs to be moved to a utility file somewhere.
 */
guint gnc_book_count_transactions(QofBook *book);

/** @} */


/** @name Transaction general getters/setters
 @{
*/

/** Determine whether this transaction should use commodity trading accounts
 */
gboolean xaccTransUseTradingAccounts(const Transaction *trans);

/** Sorts the splits in a transaction, putting the debits first,
 *  followed by the credits.
 */
void          xaccTransSortSplits (Transaction *trans);

/** Set the  Transaction Type
 *
 * See #TXN_TYPE_NONE, #TXN_TYPE_INVOICE and #TXN_TYPE_PAYMENT */
void	      xaccTransSetTxnType (Transaction *trans, char type);
/** Returns the  Transaction Type
 *
 * See #TXN_TYPE_NONE, #TXN_TYPE_INVOICE and #TXN_TYPE_PAYMENT */
char	      xaccTransGetTxnType (const Transaction *trans);

/** Sets the transaction Number (or ID) field; rather than use this function
 *  directly, see 'gnc_set_num_action' in engine/engine-helpers.c & .h which
 *  takes a user-set book option for selecting the source for the num-cell (the
 *  transaction-number or the split-action field) in registers/reports into
 *  account automatically  */
void          xaccTransSetNum (Transaction *trans, const char *num);

/** Sets the transaction Description */
void          xaccTransSetDescription (Transaction *trans, const char *desc);

/** Sets the transaction Association */
void          xaccTransSetAssociation (Transaction *trans, const char *assoc);

/** Sets the transaction Notes
 *
 The Notes field is only visible in the register in double-line mode */
void          xaccTransSetNotes (Transaction *trans, const char *notes);

/** Gets the transaction Number (or ID) field; rather than use this function
 *  directly, see 'gnc_get_num_action' and 'gnc_get_action_num' in
 *  engine/engine-helpers.c & .h which takes a user-set book option for
 *  selecting the source for the num-cell (the transaction-number or the
 *  split-action field) in registers/reports into account automatically  */
const char *  xaccTransGetNum (const Transaction *trans);
/** Gets the transaction Description */
const char *  xaccTransGetDescription (const Transaction *trans);
/** Gets the transaction association */
const char *  xaccTransGetAssociation(const Transaction *trans);
/** Gets the transaction Notes
 *
 The Notes field is only visible in the register in double-line mode */
const char *  xaccTransGetNotes (const Transaction *trans);


/** Sets whether or not this transaction is a "closing transaction" */
void          xaccTransSetIsClosingTxn (Transaction *trans, gboolean is_closing);

/** Returns whether this transaction is a "closing transaction" */
gboolean      xaccTransGetIsClosingTxn (const Transaction *trans);


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
Split *       xaccTransGetSplit (const Transaction *trans, int i);

/** Inverse of xaccTransGetSplit() */
int xaccTransGetSplitIndex(const Transaction *trans, const Split *split);

/** The xaccTransGetSplitList() method returns a GList of the splits
    in a transaction.
    @param trans The transaction
    @return The list of splits. This list must NOT be modified.  Do *NOT* free
    this list when you are done with it. */
/*@ dependent @*/
SplitList *   xaccTransGetSplitList (const Transaction *trans);

/** The xaccTransGetPaymentAcctSplitList() method returns a GList of the splits
    in a transaction that belong to an account which is considered a
    valid account for business payments.
    @param trans The transaction
    @return The list of splits. This list must be freed when you are done with it. */
SplitList *   xaccTransGetPaymentAcctSplitList (const Transaction *trans);

/** The xaccTransGetAPARSplitList() method returns a GList of the splits
    in a transaction that belong to an AR or AP account.
    @param trans The transaction
    @param strict This slightly modifies the test to only consider splits in an AR or AP account and the split is part of a business lot
    @return The list of splits. This list must be freed when you are done with it. */
SplitList *   xaccTransGetAPARAcctSplitList (const Transaction *trans, gboolean strict);


gboolean      xaccTransStillHasSplit(const Transaction *trans, const Split *s);

/** The xaccTransGetFirstPaymentAcctSplit() method returns a pointer to the first
    split in this transaction that belongs to an account which is considered a
    valid account for business payments.
    @param trans The transaction

    If there is no such split in the transaction NULL will be returned. */
Split *       xaccTransGetFirstPaymentAcctSplit (const Transaction *trans);

/** The xaccTransGetFirstPaymentAcctSplit() method returns a pointer to the first
    split in this transaction that belongs to an AR or AP account.
    @param trans The transaction
    @param strict This slightly modifies the test to only consider splits in an AR or AP account and the split is part of a business lot

    If there is no such split in the transaction NULL will be returned. */
Split *       xaccTransGetFirstAPARAcctSplit (const Transaction *trans, gboolean strict);

/** Set the transaction to be ReadOnly by setting a non-NULL value as "reason".
 *
 * FIXME: If "reason" is NULL, this function does nothing, instead of removing the
 * readonly flag; the actual removal is possible only through
 * xaccTransClearReadOnly(). */
void          xaccTransSetReadOnly (Transaction *trans, const char *reason);
void	      xaccTransClearReadOnly (Transaction *trans);

/** Returns a non-NULL value if this Transaction was marked as read-only with
 * some specific "reason" text. */
const char *  xaccTransGetReadOnly (const Transaction *trans);

/** Returns TRUE if this Transaction is read-only because its posted-date is
 * older than the "auto-readonly" threshold of this book. See
 * qof_book_uses_autofreeze() and qof_book_get_autofreeze_gdate(). */
gboolean xaccTransIsReadonlyByPostedDate(const Transaction *trans);

/*################## Added for Reg2 #################*/

/** Returns TRUE if this Transaction's posted-date is in the future */
gboolean xaccTransInFutureByPostedDate (const Transaction *trans);

/*################## Added for Reg2 #################*/

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
/*@ dependent @*/
gnc_commodity * xaccTransGetCurrency (const Transaction *trans);

/** Set the commodity of this transaction. */
void xaccTransSetCurrency (Transaction *trans, gnc_commodity *curr);

/** The xaccTransGetImbalanceValue() method returns the total value of the
 * transaction.  In a pure double-entry system, this imbalance
 * should be exactly zero, and if it is not, something is broken.
 * However, when double-entry semantics are not enforced, unbalanced
 * transactions can sneak in, and this routine can be used to find
 * out how much things are off by.  The value returned is denominated
 * in the currency that is returned by the xaccTransFindCommonCurrency()
 * method.
 *
 * If the use of currency exchange accounts is enabled then the a
 * a transaction must be balanced in each currency it uses to be considered
 * to be balanced.  The method xaccTransGetImbalance is used by most
 * code to take this into consideration.  This method is only used in a few
 * places that want the transaction value even if currency exchange accounts
 * are enabled. */
gnc_numeric xaccTransGetImbalanceValue (const Transaction * trans);

/** The xaccTransGetImbalance method returns a list giving the value of
 * the transaction in each currency for which the balance is not zero.
 * If the use of currency accounts is disabled, then this will be only
 * the common currency for the transaction and xaccTransGetImbalance
 * becomes equivalent to xaccTransGetImbalanceValue.  Otherwise it will
 * return a list containing the imbalance in each currency. */
MonetaryList *xaccTransGetImbalance (const Transaction * trans);

/** Returns true if the transaction is balanced according to the rules
 * currently in effect. */
gboolean xaccTransIsBalanced(const Transaction * trans);

/** The xaccTransGetAccountValue() method returns the total value applied
 *  to a particular account.  In some cases there may be multiple Splits
 *  in a single Transaction applied to one account (in particular when
 *  trying to balance Lots) -- this function is just a convienience to
 *  view everything at once.
 */
gnc_numeric xaccTransGetAccountValue (const Transaction *trans,
                                      const Account *account);

/** Same as xaccTransGetAccountValue, but uses the Account's commodity. */
gnc_numeric xaccTransGetAccountAmount (const Transaction *trans,
                                       const Account *account);

/*################## Added for Reg2 #################*/
/* Gets the amt/val rate, i.e. rate from the transaction currency to
   the 'split_com' */
gboolean
xaccTransGetRateForCommodity(const Transaction *trans,
                             const gnc_commodity *split_com,
                             const Split *split_to_exclude, gnc_numeric *rate);
/*################## Added for Reg2 #################*/

/* Compute the conversion rate for the transaction to this account.
 * Any "split value" (which is in the transaction currency),
 * multiplied by this conversion rate, will give you the value you
 * should display for this account.
 *
 * If 'acc' is NULL, return unity.
 */
gnc_numeric xaccTransGetAccountConvRate(const Transaction *txn, const Account *acc);

/** Get the account balance for the specified account after the last
    split in the specified transaction. */
gnc_numeric xaccTransGetAccountBalance (const Transaction *trans,
                                        const Account *account);

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
 *      GncGUID (compare as a guid)
 *    Finally, it returns zero if all of the above match.
 *    Note that it does *NOT* compare its member splits.
 *    Note also that it calls xaccTransOrder_num_action with actna and actnb
 *    set as NULL.
 */
int  xaccTransOrder     (const Transaction *ta, const Transaction *tb);


/**
 * The xaccTransOrder_num_action(ta,actna,tb,actnb) method is useful for sorting.
 *    Orders ta and tb
 *      return <0 if ta sorts before tb
 *      return >0 if ta sorts after tb
 *      return 0 if they are absolutely equal
 *
 *    The comparrison uses the following fields, in order:
 *      date posted  (compare as a date)
 *      if actna and actnb are NULL,
 *          num field (compare as an integer)
 *      else actna and actnb  (compare as an integer)
 *      date entered (compare as a date)
 *      description field (comcpare as a string using strcmp())
 *      GncGUID (compare as a guid)
 *    Finally, it returns zero if all of the above match.
 *    Note that it does *NOT* compare its member splits (except action as
 *    specified above).
 */
int  xaccTransOrder_num_action (const Transaction *ta, const char *actna,
                                const Transaction *tb, const char *actnb);

/** @} */


/** @name Transaction date setters/getters
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
void          xaccTransSetDate (Transaction *trans,
                                int day, int mon, int year);

/** This method modifies <i>posted</i> date of the transaction,
 * specified by a GDate. The posted date is the date when this
 * transaction was posted at the bank.
 *
 * This is identical to xaccTransSetDate(), but different from
 * xaccTransSetDatePostedSecs which artificially introduces the
 * time-of-day part, which needs to be ignored. */
void xaccTransSetDatePostedGDate (Transaction *trans, GDate date);

/** The xaccTransSetDatePostedSecs() method will modify the <i>posted</i>
 *  date of the transaction, specified by a time64 (see ctime(3)). The
 *  posted date is the date when this transaction was posted at the
 *  bank.
 *
 * Please do not use this function, as the extra time-of-day part messes up a
 * lot of places. Rather, please use xaccTransSetDatePostedGDate() or
 * xaccTransSetDatePostedSecsNormalized().
 */
void          xaccTransSetDatePostedSecs (Transaction *trans, time64 time);

/** This function sets the <i>posted</i> date of the transaction, specified by
 * a time64 (see ctime(3)). Contrary to xaccTransSetDatePostedSecs(), the time
 * will be normalized to only the date part, and the time-of-day will be
 * ignored. The resulting date is the same as if it had been set as a GDate
 * through xaccTransSetDatePostedGDate().
 *
 * Please prefer this function over xaccTransSetDatePostedSecs().
 *
 * The posted date is the date when this transaction was posted at the bank. */
void          xaccTransSetDatePostedSecsNormalized (Transaction *trans, time64 time);

/**  The xaccTransSetDatePostedTS() method does the same thing as
     xaccTransSetDatePostedSecs(), but takes a struct timespec64. */
void          xaccTransSetDatePostedTS (Transaction *trans,
                                        const Timespec *ts);

/** Modify the date of when the transaction was entered. The entered
 * date is the date when the register entry was made. */
void          xaccTransSetDateEnteredSecs (Transaction *trans, time64 time);
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
time64        xaccTransGetDate (const Transaction *trans);
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
/** Retrieve the posted date of the transaction. The posted date is
    the date when this transaction was posted at the bank. */
GDate      xaccTransGetDatePostedGDate (const Transaction *trans);

/*################## Added for Reg2 #################*/
/** Retrieve the date of when the transaction was entered. The entered
 * date is the date when the register entry was made.*/
time64        xaccTransGetDateEntered (const Transaction *trans);
/*################## Added for Reg2 #################*/
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
/** @} */



/********************************************************************\
 * Miscellaneous utility routines.
\********************************************************************/


/** @name Transaction voiding
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

/** xaccTransReverse creates a Transaction that reverses the given
 *  tranaction by inverting all the numerical values in the given
 *  transaction.  This function cancels out the effect of an earlier
 *  transaction.  This will be needed by write only accounts as a way
 *  to void a previous transaction (since you can't alter the existing
 *  transaction).
 *
 *  @param transaction The transaction to create a reverse of.
 *
 *  @return a new transaction which reverses the given transaction
 */
Transaction * xaccTransReverse(Transaction *transaction);

/** Returns the transaction that reversed the given transaction.
 *
 *  @param trans a Transaction that has been reversed
 *
 *  @return the transaction that reversed the given transaction, or
 *  NULL if the given transaction has not been reversed.
 */
Transaction * xaccTransGetReversedBy(const Transaction *trans);

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
const char *xaccTransGetVoidReason(const Transaction *transaction);

/** Returns the time that a transaction was voided.
 *
 *  @param tr The transaction in question.
 *
 *  @return A Timespec containing the time that this transaction was
 *  voided. Returns a time of zero upon error.
 */
Timespec xaccTransGetVoidTime(const Transaction *tr);
/** @} */

/** @name Transaction Parameter names
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
#define TRANS_IS_CLOSING        "trans-is-closing?"
#define TRANS_NOTES		"notes"
#define TRANS_ASSOCIATION	"assoc"
#define TRANS_TYPE		"type"
#define TRANS_VOID_STATUS	"void-p"
#define TRANS_VOID_REASON	"void-reason"
#define TRANS_VOID_TIME		"void-time"
#define TRANS_SPLITLIST		"split-list" /* for guid_match_all */
/**@}*/

#ifdef DUMP_FUNCTIONS
void xaccTransDump (const Transaction *trans, const char *tag);
#endif

#define RECONCILED_MATCH_TYPE	"reconciled-match"

/** \deprecated */
#define xaccTransGetBook(X)      qof_instance_get_book (QOF_INSTANCE(X))
/** \deprecated */
#define xaccTransGetGUID(X)      qof_entity_get_guid(QOF_INSTANCE(X))
/** \deprecated */
#define xaccTransReturnGUID(X) (X ? *(qof_entity_get_guid(QOF_INSTANCE(X))) : *(guid_null()))

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* XACC_TRANSACTION_H */
/** @} */
/** @} */
