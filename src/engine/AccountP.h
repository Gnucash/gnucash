/********************************************************************\
 * AccountP.h -- the Account data structure                         *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1997-2000, Linas Vepstas <linas@linas.org>         *
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
 * AccountP.h
 *
 * FUNCTION:
 * This is the *private* header for the account structure.
 * No one outside of the engine should ever include this file.
 *
 * This header includes prototypes for "dangerous" functions.
 * Invoking any of these functions potentially leave the account
 * in an inconsistent state.  If they are not used in the proper
 * setting, they can leave the account structures in an inconsistent
 * state.  Thus, these methods should never be used outside of
 * the engine, which is why they are "hidden" here. 
 *
 */

#ifndef __XACC_ACCOUNT_P_H__
#define __XACC_ACCOUNT_P_H__

#include "config.h"
#include "AccInfo.h"
#include "GNCId.h"
#include "Transaction.h"


/** STRUCTS *********************************************************/
struct _account {
  /* public data, describes account */
  GUID      guid;          /* globally unique account id */

  /* The accountName is an arbitrary string assinged by the user. 
   * It is intended to a short, 5 to 30 character long string that
   * is displayed by the GUI as the account mnomenic. 
   */
  char     *accountName;

  /* The accountCode is an arbitary string assigned by the user.
   * It is intended to be reporting code that is a synonym for the 
   * accountName. Typically, it will be a numeric value tht follows 
   * the numbering assignments commonly used by accountants, such 
   * as 100, 200 or 600 for top-level * accounts, and 101, 102..  etc.
   * for detail accounts.
   */
  char     *accountCode;

  /* The description is an arbitraary string assigned by the user. 
   * It is intended to be a longer, 1-5 sentance description of what
   * this account is all about.
   */
  char     *description;

  /* The notes field is an arbitrary string assigned by the user.
   * It is intended to hold long, free-form arbitrary additional 
   * data about the account. Machine-readable data *must* be 
   * structured using standard mime-type techniques.  For example,
   * image data would be Base64 encoded, and lists of key-value
   * pairs would be URL-encoded.
   */
  char     *notes;

  /* The type field is the account type, picked from the enumerated 
   * list that includes BANK, STOCK, CREDIT, INCOME, etc.  Its
   * intended use is to be a hint to the GUI as to how to display   
   * and format the transaction data.
   */
  short     type;

  /* The accInfo field provides a hook for storing additional 
   * account-type specific data.  Thus, it will contain different
   * structures depending on whether the account is a bank, investment
   * or other type of account.  Implemented as a union.
   */
  AccInfo *accInfo;

  /* The currency field denotes the default currency in which all
   * splits in this account are denominated.  Its value *MUST*
   * be a three-letter ISO currency code, or it must be a comma followed
   * by an arbitrary string (security name).  Currency trading accounts
   * allow splits between accounts when the currency string matches the
   * security string.
   */
  char    *currency;
  char    *security;

  /* The parent and children pointers are used to implement an account
   * hierarchy, of accounts that have sub-accounts ("detail accounts").
   */
  AccountGroup *parent;    /* back-pointer to parent */
  AccountGroup *children;  /* pointer to sub-accounts */

  /* The id number is internally assigned by the engine, and is used for 
   * various housekeeping operations by the engine.
   */
  int       id;            /* unique account id, internally assigned */
  char      flags;

  /* protected data, cached parameters */
  double balance;
  double cleared_balance;
  double reconciled_balance;

  double running_balance;
  double running_cleared_balance;
  double running_reconciled_balance;

  int numSplits;                /* length of splits array below   */
  Split **splits;               /* ptr to array of ptrs to splits */

  /* The "changed" flag is used to invalidate cached values in this structure.
   * currently, the balances and the cost basis are cached.
   */
  short changed;

  /* the "open" flag indicates if the account has been 
   * opened for editing. */
  short open;
};

/* bitfields for the changed flag */
#define ACC_INVALID_BALN      0x1
#define ACC_INVALID_COSTB     0x2
#define ACC_INVALIDATE_ALL    0x3

/* bitflields for the open flag */
#define ACC_BEGIN_EDIT        0x1
#define ACC_DEFER_REBALANCE   0x2
#define ACC_BEING_DESTROYED   0x4


/* The xaccAccountRemoveSplit() routine will remove the indicated split
 *    from the indicated account.  Note that this will leave the split
 *    "dangling", i.e. unassigned to any account, and therefore will put
 *    the engine into an inconsistent state.  After removing a split, 
 *    it should be immediately destroyed, or it should be inserted into  
 *    an account.
 */
void         xaccAccountRemoveSplit (Account *, Split *);

/* the following recompute the partial balances (stored with the
 * transaction)
 * and the total balance, for this account */
void         xaccAccountRecomputeBalance (Account *);
void         xaccAccountRecomputeBalances (Account **);

/*
 * recomputes the cost basis 
 */
void         xaccAccountRecomputeCostBasis (Account *);


/* Set the account's GUID. This should only be done when reading
 * an account from a datafile, or some other external source. Never
 * call this on an existing account! */
void xaccAccountSetGUID (Account *account, GUID *guid);


/** GLOBALS *********************************************************/

extern int next_free_unique_account_id;
 
#endif /* __XACC_ACCOUNT_P_H__ */
