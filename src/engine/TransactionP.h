/********************************************************************\
 * TransactionP.h -- defines transaction for xacc (X-Accountant)    *
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
 * In other words, their use is dangerous, and their use outside
 * of the scope of the engine is forbidden.
 *
 */

#ifndef __XACC_TRANSACTION_P_H__
#define __XACC_TRANSACTION_P_H__

#include <time.h>

#include "config.h"
#include "Transaction.h"   /* for typedefs */
#include "GNCId.h"


/** STRUCTS *********************************************************/
/* 
 * Double-entry is forced by having at least two splits in every
 * transaction.  By convention, (and only by convention, not by
 * any innate requirement), the first split is considered to be
 * the source split or the crediting split, and the others are
 * the destination, or debiting splits.  The grand total of all
 * of the splits must always be kept zero.
 */

/* A split transaction is one which shows up as a credit (or debit) in
 * one account, and pieces of it show up as debits (or credits) in other
 * accounts.  Thus, a single credit-card transaction might be split
 * between "dining", "tips" and "taxes" categories.
 *
 * A "split" is more commonly refered to as a "entry" in a "transaction".
 */


struct _split 
{
  GUID guid;  /* globally unique id */

  Account *acc;              /* back-pointer to debited/credited account  */
  Transaction *parent;       /* parent of split                           */

  /* The memo field is an arbitrary user-assiged value. 
   * It is intended to hold a short (zero to forty character) string 
   * that is displayed by the GUI along with this split. 
   */
  char  * memo;

  /* The action field is an arbitrary user-assigned value.
   * It is meant to be a very short (one to ten cahracter) string that
   * signifies the "type" of this split, such as e.g. Buy, Sell, Div,
   * Withdraw, Deposit, ATM, Check, etc. The idea is that this field
   * can be used to create custom reports or graphs of data.
   */ 
  char  * action;            /* Buy, Sell, Div, etc.                      */

  /* The docref field is a hook for arbitrary additional user-assigned
   * data, such as invoice numbers, clearing/posting reference numbers, 
   * supporting document references, etc. This additional data should
   * be encoded in a machine-readable format, e.g. a mime-type encapsulated
   * form, which any key-value pairs being URL-encoded.
   */
  char * docref;

  /* The reconciled field ...
   */
  char    reconciled;
  Timespec date_reconciled;  /* date split was reconciled                 */

  double  damount;           /* num-shares; if > 0.0, deposit, else paymt */
  double  share_price;       /* the share price, ==1.0 for bank account   */

  /* -------------------------------------------------------------- */
  /* Below follow some 'temporary' fields */

  /* The various "balances" are the sum of all of the values of 
   * all the splits in the account, up to and including this split.
   * These balances apply to a sorting order by date posted
   * (not by date entered). */
  double  balance;
  double  cleared_balance;
  double  reconciled_balance;

  double  share_balance;
  double  share_cleared_balance;
  double  share_reconciled_balance;

  double cost_basis;


  int ticket; /* used for matching up splits for QIFIO.c */
};


struct _transaction 
{
  /* guid is a globally unique identifier which can be used to
   * reference the transaction.
   */
  GUID guid;

  Timespec date_entered;     /* date register entry was made              */
  Timespec date_posted;      /* date transaction was posted at bank       */

  /* The num field is a arbitrary user-assigned field.  
   * It is intended to store a short id number, typically the check number,
   * deposit number, invoice number or other tracking number.
   */
  char  * num;  

  /* The description field is an arbitrary user-assigned value. 
   * It is meant to be a short descriptive phrase.
   */
  char  * description;        

  /* The docref field is a hook for arbitrary additional user-assigned
   * data, such as invoice numbers, clearing/posting reference numbers, 
   * supporting document references, etc. This additional data should
   * be encoded in a machine-readable format, e.g. a mime-type encapsulated
   * form, which any key-value pairs being URL-encoded.
   */
  char * docref;

  Split   **splits;          /* list of splits, null terminated           */

  /* marker is used to track the progress of transaction traversals. 
   * 0 is never a legitimate marker value, so we can tell is we hit
   * a new transaction in the middle of a traversal. All each new
   * traversal cares about is whether or not the marker stored in
   * a transaction is the same as or different than the one
   * corresponding to the current traversal. */
  unsigned char  marker;      

  /* the "open" flag indicates if the transaction has been 
   * opened for editing. */
  char open;

  /* the orig pointer points at a copy of the original transaction,
   * before editing was started.  This orig copy is used to rollback 
   * any changes made if/when the edit is abandoned.
   */
  Transaction *orig;
};


/* Set the transaction's GUID. This should only be done when reading
 * a transaction from a datafile, or some other external source. Never
 * call this on an existing transaction! */
void xaccTransSetGUID (Transaction *trans, GUID *guid);

/* Set the split's GUID. This should only be done when reading
 * a split from a datafile, or some other external source. Never
 * call this on an existing split! */
void xaccSplitSetGUID (Split *split, GUID *guid);

/* The xaccFreeTransaction() method simply frees all memory associated
 * with the transaction.  It does not perform any consistency checks 
 * to verify that such freeing can be safely done. (e.g. id does
 * not check to see if any of the member splits are referenced
 * by an account.
 */
void  xaccFreeTransaction (Transaction *);

/* The xaccFreeSplit() method simply frees all memory associated
 * with the split.  It does not verify that the split isn't
 * referenced in some account.  If the split is referenced by an 
 * account, then calling this method will leave the system in an 
 * inconsistent state.
 */
void  xaccFreeSplit   (Split *);    /* frees memory */

/* The xaccTransRemoveSplit() routine will remove the indicated
 *    split from the transaction.  It will *NOT* otherwise 
 *    re-adjust balances, modify accounts, etc.
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
 * the balance is zero.  If there is not destination split,
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
