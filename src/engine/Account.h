/********************************************************************\
 * Account.h -- the Account data structure                          *
 * Copyright (C) 1997 Robin D. Clark                                *
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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
 *                                                                  *
 *   Author: Rob Clark                                              *
 * Internet: rclark@cs.hmc.edu                                      *
 *  Address: 609 8th Street                                         *
 *           Huntington Beach, CA 92648-4632                        *
\********************************************************************/

#ifndef __ACCOUNT_H__
#define __ACCOUNT_H__

#include "config.h"
#include "Transaction.h"

/* The account types. 
 * Note: the actual values of these are *very* important, 
 * as it is the values, not the enums, that are stored 
 * in the file format! 
 */
enum {
  BANK = 0,
  CASH = 1,
  ASSET = 2,
  CREDIT = 3,
  LIABILITY = 4,
  STOCK = 5,
  MUTUAL= 6, 
  INCOME = 7,
  EXPENSE = 8,
  EQUITY = 9,
  NUM_ACCOUNT_TYPES = 10
};

/* the english-language names here should match 
 * the enumerated types above */
extern char *account_type_name[];

/** STRUCTS *********************************************************/
typedef struct _Account {
  /* public data, describes account */

  struct _account_group *parent;    /* back-pointer to parent */
  struct _account_group *children;  /* pointer to sub-accounts */
  int       id;                     /* unique account id, internally assigned */
  char      flags;
  short     type;
  char     *accountName;
  char     *description;
  char     *notes;

  /* protected data, cached parameters */
  double balance;
  double cleared_balance;
  double reconciled_balance;

  double running_balance;
  double running_cleared_balance;
  double running_reconciled_balance;

  int numSplits;                /* length of splits array below   */
  Split **splits;               /* ptr to array of ptrs to splits */

  /* the modified flag helps the gui keep track of 
   * changes to this account */
  short changed;
} Account;

/** PROTOTYPES ******************************************************/

Account     *xaccMallocAccount( void );
void         xaccInitAccount( Account * );
void         xaccFreeAccount( Account * );

int          xaccGetAccountID (Account *);

void         xaccInsertSplit (Account *, Split *);
void         xaccRemoveSplit (Account *, Split *);

/* the following recompute the partial balances (stored with the transaction)
 * and the total balance, for this account */
void         xaccRecomputeBalance (Account *);
void         xaccRecomputeBalances (Account **);

/* The xaccCheckDateOrder() surboutine checks to see if 
 *    a split is in proper sorted date order with respect 
 *    to the other splits in this account.
 *
 * The xaccCheckTransDateOrder() checks to see if 
 *    all of the splits in this transaction are in
 *    proper date order.
 */
int          xaccCheckDateOrder   (Account *, Split *);
int          xaccCheckTransDateOrder (Transaction *);

/* The xaccIsAccountInList() subroutine returns the number of times
 * that an account appears in the account list. */
int          xaccIsAccountInList (Account * acc, Account **list);
void         xaccZeroRunningBalances (Account **list);

/* The xaccConsolidateTransactions() subroutine scans through
 *    all of the transactions in an account, and compares them.
 *    if any of them are exact duplicates, the duplicates are removed.
 *    duplicates may occur when accounts from multiple sources are 
 *    merged.  Note that this can be a dangerous operation to perform.
 */

void        xaccConsolidateTransactions (Account *);

/* The xaccMoveFarEnd() method changes the account to which the 
 *    "far end" of the split belongs.  The "far end" is as follows:
 *    Double-entry transactions by thier nature consist of a set of 
 *    splits, each split indicating a debited or credited account.
 *    Given a debit split, the "far end" is the account indicated 
 *    by the credit split.  Given a credit split, the "far end"
 *    is the debit split, if there is only one debit split, otherwise
 *    there is no far end.  This subroutine will change the "far end".
 *    The first argument is the split whose fare end will be changed,
 *    the second argument is the new far-end account.
 */

void xaccMoveFarEnd (Split *, Account *);
void xaccMoveFarEndByName (Split *, const char *);

Account *     xaccSplitGetAccount (Split *);

/** GLOBALS *********************************************************/

extern int next_free_unique_account_id;

#endif
