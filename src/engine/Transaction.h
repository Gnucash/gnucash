/********************************************************************\
 * Transaction.h -- defines transaction for xacc (X-Accountant)     *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1997, 1998 Linas Vepstas                           *
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

#ifndef __XACC_TRANSACTION_H__
#define __XACC_TRANSACTION_H__

#include "config.h"
#include "date.h"   /* for Date */

/* Values for the reconciled field in Transaction: */
#define CREC 'c'              /* The transaction has been cleared        */
#define YREC 'y'              /* The transaction has been reconciled     */
#define FREC 'f'              /* frozen into accounting period           */
#define NREC 'n'              /* not reconciled or cleared               */

/** STRUCTS *********************************************************/
/* The debit & credit pointers are used to implement a double-entry 
 * accounting system.  Basically, the idea with double entry is that
 * there is always an account that is debited, and another that is
 * credited.  These two pointers identify the two accounts. 
 */

/* A split transaction is one which shows up as a credit (or debit) in
 * one account, and peices of it show up as debits (or credits) in other
 * accounts.  Thus, a single credit-card transaction might be split
 * between "dining", "tips" and "taxes" categories.
 */

typedef struct _account       Account;
typedef struct _account_group AccountGroup;
typedef struct _split         Split;
typedef struct _transaction   Transaction;


/** PROTOTYPES ******************************************************/
/*
 * The xaccTransSetDate() method will modify the date of the 
 *    transaction.  It will also make sure that the transaction
 *    is stored in proper date order in the accounts.
 */

Transaction * xaccMallocTransaction (void);       /* mallocs and inits */
void          xaccInitTransaction (Transaction *);/* clears a trans struct */

void          xaccTransBeginEdit (Transaction *);
void          xaccTransCommitEdit (Transaction *);

void          xaccTransSetDate (Transaction *, int day, int mon, int year);
void          xaccTransSetDateStr (Transaction *, char *);

/* set the transaction date to the current system time. */
void          xaccTransSetDateToday (Transaction *);

void          xaccTransSetNum (Transaction *, const char *);
void          xaccTransSetDescription (Transaction *, const char *);
void          xaccTransSetMemo (Transaction *, const char *);
void          xaccTransSetAction (Transaction *, const char *);

void          xaccTransAppendSplit (Transaction *, Split *);

/* 
 * The xaccSplitDestroy() method will update its parent account and 
 *    transaction in a consistent maner, resulting in the complete 
 *    unlinking of the split, and the freeing of it's associated memory.
 *    The goal of this routine is to perform the removal and destruction
 *    of the split in an atomic fashion, with no chance of accidentally
 *    leaving the accounting structure out-of-balance or otherwise
 *    inconsistent.
 *
 *    If the parent transaction of the split has three or more splits
 *    in it, then only this one split is unlinked. If the parent
 *    transaction has only two splits in it (and thus, this is one of
 *    them), then both splits and the transaction are destroyed.
 */
void          xaccSplitDestroy (Split *);

/* ------------- gets --------------- */
/* return pointer to each of the splits */
Split *       xaccTransGetSplit (Transaction *trans, int i);

char *        xaccTransGetNum (Transaction *);
char *        xaccTransGetDescription (Transaction *);
Date *        xaccTransGetDate (Transaction *);
char *        xaccTransGetDateStr (Transaction *);

/* return the number of splits */
int           xaccTransCountSplits (Transaction *trans);

Split       * xaccMallocSplit (void);
void          xaccInitSplit   (Split *);    /* clears a split struct */
int           xaccCountSplits (Split **sarray);

void          xaccSplitSetMemo (Split *, const char *);
void          xaccSplitSetAction (Split *, const char *);
void          xaccSplitSetReconcile (Split *, char);


/* 
 * The following four functions set the prices and amounts.
 * All of the routines always maintain balance: that is, 
 * invoking any of them will cause other splits in the transaction
 * to be modified so that the net value of the transaction is zero. 
 *
 * The xaccSetShareAmount() method sets the number of shares
 *     that the split should have.  
 *
 * The xaccSetSharePrice() method sets the price of the split.
 *
 * The xaccSetValue() method adjusts the number of shares in 
 *     the split so that the number of shares times the share price
 *     equals the value passed in.
 *
 * The xaccSetSharePriceAndAmount() method will simultaneously
 *     update the share price and the number of shares. This 
 *     is a utility routine that is equivalent to a xaccSplitSetSharePrice()
 *     followed by and xaccSplitSetAmount(), except that it incurs the
 *     processing overhead of balancing only once, instead of twise.
 */

void         xaccSplitSetSharePriceAndAmount (Split *, double price,
                                              double amount);
void         xaccSplitSetShareAmount (Split *, double);
void         xaccSplitSetSharePrice (Split *, double);
void         xaccSplitSetValue (Split *, double);


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

double xaccSplitGetBalance (Split *);
double xaccSplitGetClearedBalance (Split *);
double xaccSplitGetReconciledBalance (Split *);
double xaccSplitGetShareBalance (Split *);

/* return teh parent transaction of the split */
Transaction * xaccSplitGetParent (Split *);

/* return the value of the reconcile flag */
char *        xaccSplitGetMemo (Split *split);
char *        xaccSplitGetAction (Split *split);

char          xaccSplitGetReconcile (Split *split);
double        xaccSplitGetShareAmount (Split * split);
double        xaccSplitGetSharePrice (Split * split);
double        xaccSplitGetValue (Split * split);

Account *     xaccSplitGetAccount (Split *);

/********************************************************************\
 * sorting comparison function
 *
 * returns a negative value if transaction a is dated earlier than b,        
 * returns a positive value if transaction a is dated later than b,
 * returns zero if both transactions are on the same date.
 *
\********************************************************************/

int  xaccTransOrder (Transaction **ta, Transaction **tb);
int  xaccSplitOrder (Split **sa, Split **sb);

/*
 * count the number of transactions in the null-terminated array
 */
int xaccCountTransactions (Transaction **tarray);

/** GLOBALS *********************************************************/

#endif /* __XACC_TRANSACTION_H__ */
