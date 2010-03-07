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
/** @addtogroup Transaction Financial Transactions
    A good overview of transactions, splits and accounts can be
    found in the texinfo documentation, together with an overview of
    how to use this API.

    @{ */
/** @file Split.h
    @brief API for Transactions and Splits (journal entries)
    @author Copyright (C) 1997 Robin D. Clark
    @author Copyright (C) 1997-2001 Linas Vepstas <linas@linas.org>
*/

#ifndef XACC_SPLIT_H
#define XACC_SPLIT_H

typedef struct _SplitClass SplitClass;

#include <time.h>

#include "gnc-commodity.h"
#include "gnc-engine.h"

/* --- type macros --- */
#define GNC_TYPE_SPLIT            (gnc_split_get_type ())
#define GNC_SPLIT(o)              \
     (G_TYPE_CHECK_INSTANCE_CAST ((o), GNC_TYPE_SPLIT, Split))
#define GNC_SPLIT_CLASS(k)        \
     (G_TYPE_CHECK_CLASS_CAST((k), GNC_TYPE_SPLIT, SplitClass))
#define GNC_IS_SPLIT(o)           \
     (G_TYPE_CHECK_INSTANCE_TYPE ((o), GNC_TYPE_SPLIT))
#define GNC_IS_SPLIT_CLASS(k)     \
     (G_TYPE_CHECK_CLASS_TYPE ((k), GNC_TYPE_SPLIT))
#define GNC_SPLIT_GET_CLASS(o)    \
     (G_TYPE_INSTANCE_GET_CLASS ((o), GNC_TYPE_SPLIT, SplitClass))
GType gnc_split_get_type(void);


/** @name Split Reconciled field values

    If you change these
    be sure to change gnc-ui-util.c:gnc_get_reconciled_str() and
    associated functions

@{
*/
#define CREC 'c'              /**< The Split has been cleared    */
#define YREC 'y'              /**< The Split has been reconciled */
#define FREC 'f'              /**< frozen into accounting period */
#define NREC 'n'              /**< not reconciled or cleared     */
#define VREC 'v'              /**< split is void                 */
/** @} */

/* Convert the amount/value of the Split for viewing in the account --
 * in particular we want to convert the Split to be in to_commodity.
 * Returns the amount.
 */
gnc_numeric xaccSplitConvertAmount (const Split *split, const Account * account);

/*-----------------------------------------------------------------------
 * Splits
 *-----------------------------------------------------------------------*/

/** @name Split general getters/setters
@{
*/

/** Constructor. */
Split       * xaccMallocSplit (QofBook *book);

/* Reinit a previously malloc'd split. Split remains in the book it
   was already in, and the QofInstance portions also remain unchanged.
   It's basically the data elements that are reverted to default
   values. */
void xaccSplitReinit(Split * split);

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
void xaccSplitSetAccount (Split *s, Account *acc);

/** Returns the parent transaction of the split. */
Transaction * xaccSplitGetParent (const Split *split);
void xaccSplitSetParent (Split *split, Transaction *trans);

/** Returns the pointer to the debited/credited Lot where this split
 * belongs to, or NULL if it doesn't belong to any. */
GNCLot *      xaccSplitGetLot (const Split *split);

/** Assigns the split to a specific Lot */
void xaccSplitSetLot(Split* split, GNCLot* lot);


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


/** The memo is an arbitrary string associated with a split.  It is
 * intended to hold a short (zero to forty character) string that is
 * displayed by the GUI along with this split.  Users typically type
 * in free form text from the GUI.  */
void          xaccSplitSetMemo (Split *split, const char *memo);

/** Returns the memo string. */
const char *  xaccSplitGetMemo (const Split *split);

/** The Action is an arbitrary user-assigned string.
 * The action field is an arbitrary user-assigned value.
 * It is meant to be a very short (one to ten character) string that
 * signifies the "type" of this split, such as e.g. Buy, Sell, Div,
 * Withdraw, Deposit, ATM, Check, etc. The idea is that this field
 * can be used to create custom reports or graphs of data. */
void          xaccSplitSetAction (Split *split, const char *action);

/** Returns the action string. */
const char *  xaccSplitGetAction (const Split *split);
/** @} */

/** @name Split Date getters/setters
@{
*/
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
 * time as Timespec.  Caller still owns *ts! */
void          xaccSplitSetDateReconciledTS (Split *split, Timespec *ts);
/** Get the date on which this split was reconciled by having it
 * written into the Timespec that 'ts' is pointing to. */
void          xaccSplitGetDateReconciledTS (const Split *split,
        Timespec *ts);
/** Returns the date (as Timespec) on which this split was reconciled. */
Timespec      xaccSplitRetDateReconciledTS (const Split *split);

/** @} */


/** @name Split amount getters/setters
 *
 * 'value' vs. 'amount' of a Split: The 'value' is the amount of the
 * _transaction_ balancing commodity (i.e. currency) involved,
 * 'amount' is the amount of the _account's_ commodity involved.
@{
*/

/** The xaccSplitSetAmount() method sets the amount in the account's
 * commodity that the split should have.
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
 * -1/0, which is an invalid value.  I believe this order dependency
 * is a bug, but I'm too lazy to find, fix & test at the moment ...
 *
 * @note If you use this on a newly created transaction, make sure
 * that the 'value' is also set so that it doesn't remain zero.
 */
void         xaccSplitSetAmount (Split *split, gnc_numeric amount);

/** Returns the amount of the split in the account's commodity.
 *   Note that for cap-gains splits, this is slaved to the transaction
 *   that is causing the gains to occur.
 */
gnc_numeric   xaccSplitGetAmount (const Split * split);

/** The xaccSplitSetValue() method sets the value of this split in the
 * transaction's commodity.
 *
 * @note If you use this on a newly created transaction, make sure
 * that the 'amount' is also set so that it doesn't remain zero.
 */
void         xaccSplitSetValue (Split *split, gnc_numeric value);

/** Returns the value of this split in the transaction's commodity.
 *   Note that for cap-gains splits, this is slaved to the transaction
 *   that is causing the gains to occur.
*/
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
 * return the amount. If it is neither print a warning message and
 * return gnc_numeric_zero().
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

/** @} */

/** @name Split utility functions
@{
*/

/* Get a GList of unique transactions containing the given list of Splits. */
GList *xaccSplitListGetUniqueTransactions(const GList *splits);

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
#define      xaccSplitLookupDirect(g,b) xaccSplitLookup(&(g),b)


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
 * The xaccSplitOrder(sa,sb) method is useful for sorting.
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
gint xaccSplitOrder (const Split *sa, const Split *sb);
gint xaccSplitOrderDateOnly (const Split *sa, const Split *sb);


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
int xaccSplitCompareAccountFullNames(const Split *sa, const Split *sb);
/** Compare two splits by code of account. Returns similar to
 * strcmp. */
int xaccSplitCompareAccountCodes(const Split *sa, const Split *sb);
/** Compare two splits by full name of the other account. Returns
 * similar to strcmp. This function attempts to find the split on the
 * other side of a transaction and compare on it. */
int xaccSplitCompareOtherAccountFullNames(const Split *sa, const Split *sb);
/** Compare two splits by code of the other account. Returns similar
 * to strcmp. This function attempts to find the split on the
 * other side of a transaction and compare on it. */
int xaccSplitCompareOtherAccountCodes(const Split *sa, const Split *sb);


/**
 * These functions take a split, get the corresponding split on the
 * "other side" of the transaction, and extract either the name or code
 * of that split, reverting to returning a constant "Split" if the
 * transaction has more than one split on the "other side".  These
 * were added for the transaction report, and is in C because the code
 * was already written in C for the above functions and duplication
 * is silly.
 */

char * xaccSplitGetCorrAccountFullName(const Split *sa);
/** document me */
const char * xaccSplitGetCorrAccountName(const Split *sa);
/** document me */
const char * xaccSplitGetCorrAccountCode(const Split *sa);

#ifdef DUMP_FUNCTIONS
void xaccSplitDump (const Split *split, const char *tag);
#endif

/** @} */



/** @name Split deprecated functions
@{
*/

/** @deprecated The xaccSplitSetSharePrice() method sets the price of the
 * split. DEPRECATED - set the value and amount instead. */
void         xaccSplitSetSharePrice (Split *split, gnc_numeric price);

/** @} */


/********************************************************************\
 * Miscellaneous utility routines.
\********************************************************************/


/** @name Split voiding
@{
*/

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

/** @} */

/** @name Split Parameter names

 * Note, if you want to get the equivalent of "ACCT_MATCH_ALL" you
 * need to create a search on the following parameter list:
 * SPLIT->SPLIT_TRANS->TRANS_SPLITLIST->SPLIT_ACCOUNT_GUID.  If you do
 * this, you might want to use the ACCOUNT_MATCH_ALL_TYPE as the
 * override so the gnome-search dialog displays the right type.
 @{
*/
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
/** @} */

/** \deprecated */
#define xaccSplitGetGUID(X)      qof_entity_get_guid(QOF_INSTANCE(X))
/** \deprecated */
#define xaccSplitReturnGUID(X) (X ? *(qof_entity_get_guid(QOF_INSTANCE(X))) : *(guid_null()))

#endif /* XACC_SPLIT_H */
/** @} */
/** @} */
