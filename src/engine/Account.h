/********************************************************************\
 * Account.h -- the Account data structure                          *
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

#ifndef __XACC_ACCOUNT_H__
#define __XACC_ACCOUNT_H__

#include "config.h"
#include "AccInfo.h"
#include "Transaction.h"
#include "GNCId.h"


/** PROTOTYPES ******************************************************/

Account     *xaccMallocAccount( void );
void         xaccInitAccount( Account * );
void         xaccFreeAccount( Account * );

/* 
 * The xaccAccountBeginEdit() and xaccAccountCommitEdit() subroutines
 * provide a pseudo-two-phase-commit wrapper for account updates. 
 * They are mildly useful for detecting attempted updates outside
 * of their scope. However, they do not provide any true two-phase-anything
 * in the current implementation.
 *
 * The defer flag, if set, will defer all attempts at rebalancing 
 * of accounts until the commit.
 */
void         xaccAccountBeginEdit (Account *, int defer);
void         xaccAccountCommitEdit (Account *);

/*
 * The xaccAccountGetGUID() subroutine will return the
 *    globally unique id associated with that account.
 *    User code should use this id to reference accounts
 *    and *not* the integer account id below.
 *
 * The xaccAccountLookup() subroutine will return the
 *    account associated with the given id, or NULL
 *    if there is no such account.
 */
const GUID * xaccAccountGetGUID (Account *account);
Account    * xaccAccountLookup (const GUID *guid);

int          xaccGetAccountID (Account *);

/* AccountFlags is currently not used for anything.
 * If you need to add a bitflag, this may not be a bad 
 * way to go.  This flag *is* stored in the file-file DB.
 */
char         xaccGetAccountFlags (Account *);

/*
 * The xaccAccountInsertSplit() method will insert the indicated
 *    split into the indicated account.  If the split already 
 *    belongs to another account, it will be removed from that
 *    account first.
 */
void         xaccAccountInsertSplit (Account *, Split *);

/* The xaccCheckDateOrder() subroutine checks to see if 
 *    a split is in proper sorted date order with respect 
 *    to the other splits in this account.
 *
 * The xaccCheckTransDateOrder() checks to see if 
 *    all of the splits in this transaction are in
 *    proper date order.
 */
int          xaccCheckDateOrder (Account *, Split *);
int          xaccCheckTransDateOrder (Transaction *);

/* The xaccIsAccountInList() subroutine returns the number of times
 *    that an account appears in the account list. 
 */
int          xaccIsAccountInList (Account * acc, Account **list);
void         xaccZeroRunningBalances (Account **list);

/* The xaccAccountOrder() subroutine defines a sorting order 
 *    on accounts.  It takes pointers to two accounts, and
 *    returns -1 if the first account is "less than" the second,
 *    returns +1 if the first is "greater than" the second, and
 *    0 if they are equal.  To determine the sort order, first
 *    the account codes are compared, and if these are equal, then 
 *    account types, and, if these are equal, the account names.
 */
int          xaccAccountOrder (Account**, Account **);

/* The xaccAccountAutoCode() method will assign an automatically
 *    generated account code to the account, if one does not already 
 *    exist.  Account codes will have the indicated number of digits
 *    in them.  The numbering scheme roughly follows generally
 *    accepted accounting practice, in that top-level accounts
 *    will be number 100, 200, etc., second level accounts 110, 120,
 *    .. 210, 220, ...etc. and third level accounts 111, 112, .. etc.
 */
void         xaccAccountAutoCode (Account *, int digits);

/* The xaccConsolidateTransactions() subroutine scans through
 *    all of the transactions in an account, and compares them.
 *    If any of them are exact duplicates, the duplicates are removed.
 *    duplicates may occur when accounts from multiple sources are 
 *    merged.  Note that this can be a dangerous operation to perform,
 *    as it may remove transactions that were not true duplicatees ...
 */

void         xaccConsolidateTransactions (Account *);

/* The xaccMoveFarEnd() method changes the account to which the 
 *    "far end" of the split belongs.  The "far end" is as follows:
 *    Double-entry transactions by their nature consist of a set of 
 *    two or more splits. If the transaction has precisely two splits,
 *    then the "far end" is the "other split" of the pair.  If
 *    the transaction consists of three or more splits, then the 
 *    "far end" is undefined.  All that the xaccMoveFareEnd() method
 *    does is reparent the "other split" to the indicated account.
 *    The first argument is the split whose far end will be changed,
 *    the second argument is the new far-end account.
 */

void xaccMoveFarEnd (Split *, Account *);
void xaccMoveFarEndByName (Split *, const char *);

void xaccAccountSetType (Account *, int);
void xaccAccountSetName (Account *, const char *);
void xaccAccountSetCode (Account *, const char *);
void xaccAccountSetDescription (Account *, const char *);
void xaccAccountSetNotes (Account *, const char *);
void xaccAccountSetCurrency (Account *, const char *);
void xaccAccountSetSecurity (Account *, const char *);

int            xaccAccountGetType (Account *);
char *         xaccAccountGetName (Account *);
char *         xaccAccountGetFullName (Account *, const char separator);
char *         xaccAccountGetCode (Account *);
char *         xaccAccountGetDescription (Account *);
char *         xaccAccountGetNotes (Account *);
char *         xaccAccountGetCurrency (Account *);
char *         xaccAccountGetSecurity (Account *);
AccountGroup * xaccAccountGetChildren (Account *);
AccountGroup * xaccAccountGetParent (Account *);
Account *      xaccAccountGetParentAccount (Account *);
AccInfo *      xaccAccountGetAccInfo (Account *);

double         xaccAccountGetBalance (Account *);
double         xaccAccountGetClearedBalance (Account *);
double         xaccAccountGetReconciledBalance (Account *);
Split *        xaccAccountGetSplit (Account *acc, int i);
Split **       xaccAccountGetSplitList (Account *acc);
int            xaccAccountGetNumSplits (Account *acc);

/* The IthAccount() routine merely dereferences: the returned
 *    value is just list[i].  This routine is needed for the perl 
 *    swig wrappers, which cannot dereference a list.
 */

Account *      IthAccount (Account **list, int i);

/* xaccAccountsHaveCommonCurrency returns true if the two given accounts
 * have a currency in common, i.e., if they can have common transactions.
 * Useful for UI sanity checks.
 */
gncBoolean xaccAccountsHaveCommonCurrency(Account *account_1,
					  Account *account_2);

/* Returns true if the account has 'ancestor' as an ancestor.
 * Returns false if either is NULL. */
gncBoolean     xaccAccountHasAncestor (Account *, Account * ancestor);

/* Get and Set a mark on the account.  The meaning of this mark is
 * completely undefined. Its presented here as a utility for the
 * programmer, to use as desired.  Handy for performing customer traversals
 * over the account tree.  The mark is *not* stored in the database/file
 * format.  When accounts are newly created, the mark is set to zero.
 *
 * The xaccClearMark will find the topmost group, and clear the mark in
 * the entire group tree.  
 * The xaccClearMarkDown will clear the mark inly in this and in
 * sub-accounts.
 */
short          xaccAccountGetMark (Account *acc); 
void           xaccAccountSetMark (Account *acc, short mark); 
void           xaccClearMark (Account *, short val);
void           xaccClearMarkDown (Account *, short val);
void           xaccClearMarkDownGr (AccountGroup *, short val);

#endif /* __XACC_ACCOUNT_H__ */
