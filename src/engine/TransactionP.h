/*
 * FILE:
 * TransactionP.h
 *
 * FUNCTION:
 * The is the *private* transaction header file.  Code outside of 
 * engine should *not* include this file.  This is because code
 * outside of the engine should *never* access any of the structure
 * members directly.
 *
 * Note that this header file also defines prototypes for various
 * routines that perform sub-atomic updates of the accounting
 * structures.  If these routines are not used properly, they
 * can result in inconsistent, unbalanced accounting structures.
 * In other words, thier use is dangerous, and thier use outside
 * of the scope of the engine is forbidden.
 *
 */

/********************************************************************\
 * TransactionP.h -- defines transaction for xacc (X-Accountant)    *
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

#ifndef __XACC_TRANSACTION_P_H__
#define __XACC_TRANSACTION_P_H__

#include "config.h"
#include "date.h"   /* for Date */
#include "Transaction.h"   /* for typedefs */


/** STRUCTS *********************************************************/
/* 
 * Double-entry is forced by having at least two splits in every
 * transaction.  By convention, (and only by convention, not by
 * any inate requirement), the first split is considered to be
 * the source split or the crediting split, and the others are
 * the destination, or debiting splits.  The grand total of all
 * of the splits must always be kept zero.
 */

/* A split transaction is one which shows up as a credit (or debit) in
 * one account, and pieces of it show up as debits (or credits) in other
 * accounts.  Thus, a single credit-card transaction might be split
 * between "dining", "tips" and "taxes" categories.
 */

struct _split 
{
  Account *acc;     /* back-pointer to debited/credited account   */
  Transaction *parent; /* parent of split                         */
  char  * memo;
  char  * action;            /* Buy, Sell, Div, etc.                      */
  char    reconciled;
  double  damount;           /* num-shares; if > 0.0, deposit, else paymt */
  double  share_price;       /* the share price, ==1.0 for bank account   */

  /* the various "balances" are the sum of all of the values of 
   * all the splits in the account, up to and including this split */
  double  balance;
  double  cleared_balance;
  double  reconciled_balance;

  double  share_balance;
  double  share_cleared_balance;
  double  share_reconciled_balance;

};


struct _transaction 
{
  char  * num;               /* transaction id                            */
  Date    date;              /* transaction date                          */
  char  * description;        

  Split   **splits;          /* list of splits, null terminated           */

  char    write_flag;        /* used only during file IO                  */

  /* the "open" flag indicates if the transaction has been 
   * opened for editing. */
  char open;
};


/* freeTransaction only does so if the transaction is not part of an
 * account. (i.e. if none of the member splits are in an account). */
void  xaccFreeTransaction (Transaction *);

void  xaccFreeSplit   (Split *);    /* frees memory */

/* The xaccTransRemoveSplit() routine will remove the indicated
 *    split from the transaction.  It will *NOT* otherwise 
 *    readjust balances, modify accounts, etc.
 */
void  xaccTransRemoveSplit (Transaction*, Split *);


/*
 * The xaccSplitRebalance() routine is an important routine for
 * maintaining and ensuring that double-entries balance properly.
 * This routine forces the sum-total of the values of all the
 * splits in a transaction to total up to exactly zero.
 *
 * It is worthwhile to understand the algorithm that this routine
 * uses to acheive balance.  It goes like this:
 * If the indicated split is a destination split (i.e. is not
 * the first split), then the total value of the destination 
 * splits is computed, and the value of the source split (ie.
 * the first split) is adjusted to be minus this amount.
 * (the share price of the source split is not changed).
 * If the indicated split is the source split, then the value
 * of the very first destination split is adjusted so that
 * the blanace is zero.   If there is not destination split,
 * one of two outcomes are possible, depending on whether
 * "forced_double_entry" is enabled or disabled.
 * (1) if forced-double-entry is disabled, the fact that
 *     the destination is missing is ignored.
 * (2) if force-double-entry is enabled, then a destination
 *     split that exactly mirrors the source split is created,
 *     and credited to the same account as the source split.
 *     Hopefully, the user will notice this, and reparent the
 *     destination split properly.
 */

void xaccSplitRebalance (Split *);


#endif /* __XACC_TRANSACTION_P_H__ */
