/********************************************************************\
 * Account.h -- Account handling public routines                    *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1997-2001 Linas Vepstas <linas@linas.org>          *
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

#ifndef XACC_ACCOUNT_H
#define XACC_ACCOUNT_H

#include "gnc-book.h"
#include "GNCId.h"
#include "gnc-engine.h"
#include "kvp_frame.h"



/** ENUMS ******************************************************/
/*
 * The account types are used to determine how the transaction data
 * in the account is displayed.   These values can be safely changed
 * from one release to the next.  Note that if values are added,
 * the file IO translation routines need to be updated. Note 
 * also that GUI code depends on these numbers.
 *
 * ***IMPORTANT***: If you do change the enumeration names (not the
 * numbers), you need to update xaccAccountTypeEnumAsString --- used
 * for text file exports */

typedef enum 
{
  BAD_TYPE = -1,
  NO_TYPE = -1,
  /* Not a type */

  BANK = 0,
  /* The bank account type denotes a savings or checking account
   * held at a bank.  Often interest bearing.
   */

  CASH = 1,
  /* The cash account type is used to denote a shoe-box or pillowcase
   * stuffed with cash.
   */

  CREDIT = 3,  
  /* The Credit card account is used to denote credit (e.g. amex) and 
   * debit (e.g. visa, mastercard) card accounts 
   */

  ASSET = 2,
  LIABILITY = 4,
  /* asset and liability accounts indicate generic, generalized accounts
   * that are none of the above.
   */

  STOCK = 5,
  MUTUAL= 6, 
  /* Stock and Mutual Fund accounts will typically be shown in registers
   * which show three columns: price, number of shares, and value.
   */

  CURRENCY = 7, 
  /* The currency account type indicates that the account is a
   * currency trading account.  In many ways, a currency trading
   * account is like a stock trading account, where both values
   * and share quantities are set.
   */

  INCOME = 8,
  EXPENSE = 9,
  /* Income and expense accounts are used to denote income and expenses. */

  EQUITY = 10,
  /* Equity account is used to balance the balance sheet. */

  NUM_ACCOUNT_TYPES = 11,
  /* stop here; the following types just aren't ready for prime time */

  /* bank account types */
  CHECKING = 11,
  SAVINGS = 12,
  MONEYMRKT = 13,
  CREDITLINE = 14,     /* line of credit */
} GNCAccountType;

/* ------------------ */
const char * xaccAccountGetTypeStr (GNCAccountType type); /* GUI names */

/* Conversion routines for the account types to/from strings.
 * Critical for the text communication mechanisms. i.e. INCOME ->
 * "INCOME". */
char *   xaccAccountTypeEnumAsString (GNCAccountType type); 
gboolean xaccAccountStringToType (const char* str, GNCAccountType *type);
GNCAccountType xaccAccountStringToEnum (const char* str);

/* Return TRUE if accounts of type parent_type can have accounts
 * of type child_type as children. */
gboolean xaccAccountTypesCompatible (GNCAccountType parent_type,
                                     GNCAccountType child_type);

/** PROTOTYPES ******************************************************/
/* 
 * The xaccAccountBeginEdit() and xaccAccountCommitEdit() subroutines
 *    provide a two-phase-commit wrapper for account updates. 
 *
 * The xaccAccountDestroy() routine can be used to get rid of an
 *    account.  The account should have been opened for editing 
 *    (by calling xaccAccountBeginEdit()) before calling this routine.
 *
 * The xaccCloneAccountSimple() routine makes a simple copy of the
 *    indicated account, placing it in the indicated book.  It copies
 *    the account type, name, description, and the kvp values;
 *    it does not copy splits/transactions.  Note also that it 
 *    does NOT use the 'gemini' kvp value to indicate where it 
 *    was copied from.
 *
 * The xaccCloneAccount() does teh same as above, except that it
 *    also uses the 'gemini' kvp value to mark the account from
 *    which it was copied.
 */
Account    * xaccMallocAccount (GNCBook *book);
Account    * xaccCloneAccount (const Account *from, GNCBook *book);
Account    * xaccCloneAccountSimple (const Account *from, GNCBook *book);

void         xaccAccountBeginEdit (Account *account);
void         xaccAccountCommitEdit (Account *account);
void         xaccAccountDestroy (Account *account);

kvp_frame * xaccAccountGetSlots (Account *account);
void xaccAccountSetSlots_nc(Account *account, kvp_frame *frame);

GNCBook * xaccAccountGetBook (Account *account);

/* ------------------ */
/*
 * The xaccAccountGetGUID() subroutine will return the
 *    globally unique id associated with that account.
 *    xaccAccountReturnGUID() returns the same as a
 *    struct.
 *
 * The xaccAccountLookup() subroutine will return the
 *    account associated with the given id, or NULL
 *    if there is no such account.
 *    xaccAccountLookupDirect performs the same
 *    function but takes a GUID struct directly.
 */
const GUID * xaccAccountGetGUID (Account *account);
GUID         xaccAccountReturnGUID (Account *account);
Account    * xaccAccountLookup (const GUID *guid, GNCBook *book);
Account    * xaccAccountLookupDirect (GUID guid, GNCBook *book);

/* The xaccAccountLookupTwin() routine will find the
 *    "twin" of this account (if it exists) in another book.
 *    When accounts are copied or cloned,  both of the pair
 *    are marked with the guid of thier copy, thus allowing
 *    the sibling-copy of an account to be found.  Since the 
 *    sibling may end up in a different book, we need a way 
 *    of finding it, given only that we know the book, and 
 *    that we know its twin.  That's what this routine does.
 *    Given some book 'book', and an account 'acc', it will 
 *    find the sibling account of 'acc' that is in 'book',
 *    and return it.  If not found, it returns NULL.
 *    This routine uses the 'gemini' kvp values to do its work.
 */
Account * xaccAccountLookupTwin (Account *acc,  GNCBook *book);

/* ------------------ */

/* Compare two accounts for equality - this is a deep compare. */
gboolean xaccAccountEqual(Account *a, Account* b, gboolean check_guids);

/* ------------------ */
/*
 * The xaccAccountInsertSplit() method will insert the indicated
 *    split into the indicated account.  If the split already 
 *    belongs to another account, it will be removed from that
 *    account first.
 */
void         xaccAccountInsertSplit (Account *account, Split *split);

/* The xaccAccountFixSplitDateOrder() subroutine checks to see if 
 *    a split is in proper sorted date order with respect 
 *    to the other splits in this account.
 *
 * The xaccTransFixSplitDateOrder() checks to see if 
 *    all of the splits in this transaction are in
 *    proper date order.
 */
void         xaccAccountFixSplitDateOrder (Account *account, Split *split);
void         xaccTransFixSplitDateOrder (Transaction *trans);

/* The xaccAccountOrder() subroutine defines a sorting order 
 *    on accounts.  It takes pointers to two accounts, and
 *    returns -1 if the first account is "less than" the second,
 *    returns +1 if the first is "greater than" the second, and
 *    0 if they are equal.  To determine the sort order, first
 *    the account codes are compared, and if these are equal, then 
 *    account types, and, if these are equal, the account names.
 */
int          xaccAccountOrder (Account **account_1, Account **account_2);

void xaccAccountSetType (Account *account, int);
void xaccAccountSetName (Account *account, const char *name);
void xaccAccountSetCode (Account *account, const char *code);
void xaccAccountSetDescription (Account *account, const char *desc);
void xaccAccountSetNotes (Account *account, const char *notes);

GNCAccountType xaccAccountGetType (Account *account);
const char *   xaccAccountGetName (Account *account);
const char *   xaccAccountGetCode (Account *account);
const char *   xaccAccountGetDescription (Account *account);
const char *   xaccAccountGetNotes (Account *account);

/* New commodity access routines.
 *
 * The account structure no longer stores two commodities ('currency'
 * and 'security'). Instead it stores only one commodity, that is the
 * one formerly known as 'security'.  Use xaccAccountSetCommodity()
 * and xaccAccountGetCommodity() to set and fetch it.
 *
 * Basically, the engine eliminates the 'currency' field of the
 * Account structure. Instead, the common currency is stored with the
 * transaction.  The 'value' of a split is a translation of the
 * Split's 'amount' (which is the amount of the Account's commodity
 * involved) into the Transaction's balancing currency. */
void xaccAccountSetCommodity (Account *account, gnc_commodity *comm);
gnc_commodity * xaccAccountGetCommodity (Account *account);
int  xaccAccountGetCommoditySCU (Account *account);
void xaccAccountSetCommoditySCU (Account *account, int frac);

/* Deprecated currency/security access routines.
 * The current API associates only one thing with an account:
 * the 'commodity'. Use xaccAccountGetCommodity() to fetch it.
 */
/* these two funcs take control of their gnc_commodity args. Don't free */
void DxaccAccountSetCurrency (Account *account, gnc_commodity *currency,
                              GNCBook *book);
void DxaccAccountSetSecurity (Account *account, gnc_commodity *security,
                              GNCBook *book);
gnc_commodity * DxaccAccountGetCurrency (Account *account,
                                         GNCBook *book);
gnc_commodity * DxaccAccountGetSecurity (Account *account,
                                         GNCBook *book);
void DxaccAccountSetCurrencySCU (Account *account, int frac);
int  DxaccAccountGetCurrencySCU (Account *account);

/* Delete any old data in the account's kvp data.
 * This includes the old currency and security fields. */
void xaccAccountDeleteOldData (Account *account);

AccountGroup * xaccAccountGetChildren (Account *account);
AccountGroup * xaccAccountGetParent (Account *account);
Account *      xaccAccountGetParentAccount (Account *account);

gnc_numeric     xaccAccountGetBalance (Account *account);
gnc_numeric     xaccAccountGetClearedBalance (Account *account);
gnc_numeric     xaccAccountGetReconciledBalance (Account *account);

void            xaccAccountSetReconcileChildrenStatus(Account *account, gboolean status);
gboolean        xaccAccountGetReconcileChildrenStatus(Account *account);

gnc_numeric     xaccAccountGetBalanceAsOfDate (Account *account, time_t date);

SplitList*      xaccAccountGetSplitList (Account *account);

gboolean        xaccAccountGetTaxRelated (Account *account);
void            xaccAccountSetTaxRelated (Account *account,
                                          gboolean tax_related);

const char *    xaccAccountGetTaxUSCode (Account *account);
void            xaccAccountSetTaxUSCode (Account *account, const char *code);
const char *    xaccAccountGetTaxUSPayerNameSource (Account *account);
void            xaccAccountSetTaxUSPayerNameSource (Account *account,
                                                    const char *source);

/* The xaccAccountGetFullName routine returns the fully qualified name
 * of the account using the given separator char. The name must be freed
 * after use. The fully qualified name of an account is the concatenation
 * of the names of the account and all its ancestor accounts starting with
 * the topmost account and ending with the given account. Each name is
 * separated by the given character.
 *
 * WAKE UP!
 * Unlike all other gets, the string returned by xaccAccountGetFullName() 
 * must be freed by you the user !!!
 * hack alert -- since it breaks the rule of string allocation, maybe this
 * routine should not be in this library, but some utility library?
 */
char *         xaccAccountGetFullName (Account *account, const char separator);

/* Returns true if the account has 'ancestor' as an ancestor.
 * Returns false if either is NULL. */
gboolean       xaccAccountHasAncestor (Account *account, Account *ancestor);

/* Get and Set a mark on the account.  The meaning of this mark is
 * completely undefined. Its presented here as a utility for the
 * programmer, to use as desired.  Handy for performing customer traversals
 * over the account tree.  The mark is *not* stored in the database/file
 * format.  When accounts are newly created, the mark is set to zero.
 *
 * The xaccClearMark will find the topmost group, and clear the mark in
 * the entire group tree.  
 * The xaccClearMarkDown will clear the mark only in this and in
 * sub-accounts.
 */
short          xaccAccountGetMark (Account *account); 
void           xaccAccountSetMark (Account *account, short mark); 
void           xaccClearMark (Account *account, short val);
void           xaccClearMarkDown (Account *account, short val);
void           xaccClearMarkDownGr (AccountGroup *group, short val);

/* The following functions get and set reconciliation information */
gboolean       xaccAccountGetReconcileLastDate (Account *account,
                                                time_t *last_date);
void           xaccAccountSetReconcileLastDate (Account *account,
                                                time_t last_date);

gboolean       xaccAccountGetReconcilePostponeDate (Account *account,
                                                    time_t *postpone_date);
void           xaccAccountSetReconcilePostponeDate (Account *account,
                                                    time_t postpone_date);

gboolean       xaccAccountGetReconcilePostponeBalance (Account *account,
                                                       gnc_numeric *balance);
void           xaccAccountSetReconcilePostponeBalance (Account *account,
                                                       gnc_numeric balance);

void           xaccAccountClearReconcilePostpone (Account *account);

gboolean xaccAccountGetAutoInterestXfer (Account *account, gboolean default_value);
void     xaccAccountSetAutoInterestXfer (Account *account, gboolean option);

/* Get and set the last num field of an Account */
const char *   xaccAccountGetLastNum (Account *account);
void           xaccAccountSetLastNum (Account *account, const char *num);

/* The xaccAccountSetPriceSrc() and xaccAccountGetPriceSrc() routines
   are used to get and set a string that identifies the Finance::Quote
   backend that should be used to retrieve online prices.  See
   price-quotes.scm for more information.

   xaccAccountGetQuoteTZ() and xaccAccountSetQuoteTZ() set the
   timezone to be used when interpreting the results from a given
   Finance::Quote backend.  Unfortunately, the upstream sources don't
   label their output, so the user has to specify this bit.

   Since prices are not going to be stored in the accounts in the
   future, and since the whole commodities infrastructure is changing
   radically as we speak, this interface is not long for this
   world. */

void         xaccAccountSetPriceSrc (Account *account, const char *src);
const char * xaccAccountGetPriceSrc (Account *account);

void         xaccAccountSetQuoteTZ (Account *account, const char *tz);
const char * xaccAccountGetQuoteTZ (Account *account);


typedef  gpointer (*SplitCallback)(Split *s, gpointer data);
gpointer xaccAccountForEachSplit(Account *account,
                                 SplitCallback,
                                 gpointer data);

/* Traverse all of the transactions in the given account.  Continue
   processing IFF proc does not return FALSE. This function does not
   descend recursively to traverse transactions in child accounts.

   Proc will be called exactly once for each transaction that is
   pointed to by at least one split in the given account.

   Note too, that if you call this function on two separate accounts
   and those accounts share transactions, proc will be called once per
   account for the shared transactions.
   
   The result of this function will not be FALSE IFF every relevant
   transaction was traversed exactly once.  */
typedef  gboolean (*TransactionCallback)(Transaction *t, void *data);
gboolean
xaccAccountForEachTransaction(Account *account,
                              TransactionCallback,
                              void *data);

/* Visit every transaction in the account that hasn't already been
   visited exactly once.  visited_txns must be a hash table created
   via guid_hash_table_new() and is the authority about which
   transactions have already been visited.  Further, when this
   procedure returns visited_txns will have been modified to reflect
   all the newly visited transactions.

   The result of this function will not be FALSE IFF every relevant
   transaction was traversed exactly once.  */
gboolean
xaccAccountVisitUnvisitedTransactions(Account *account,
                                      TransactionCallback,
                                      void *data,
                                      GHashTable *visited_txns);

/* Returns a pointer to the transaction, not a copy. */
Transaction *
xaccAccountFindTransByDesc(Account *account, const char *description);
Split *
xaccAccountFindSplitByDesc(Account *account, const char *description);

#endif /* XACC_ACCOUNT_H */
