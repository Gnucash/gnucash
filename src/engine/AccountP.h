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

/********************************************************************\
 * Account.h -- the Account data structure                          *
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

#ifndef __XACC_ACCOUNT_P_H__
#define __XACC_ACCOUNT_P_H__

#include "config.h"
#include "Transaction.h"

/** STRUCTS *********************************************************/
struct _account {
  /* public data, describes account */

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
   * It is intended to hold long, free-form and/or MIME-format 
   * arbitrary additional data about the account.
   */
  char     *notes;

  /* The type field is the account type, picked from the enumerated 
   * list that includes BANK, STOCK, CREDIT, INCOME, etc.  It's
   * intended use is to be a hint to the GUI as to how to display   
   * and format the transaction data.
   */
  short     type;

  /* The currency field denotes the default currency in which all
   * splits in this account are denominated.  It's value *MUST*
   * be a three-letter ISO currency code.  Currency trading accounts
   * are created by creating two accounts, each with a different
   * default currency, and then having transactions with one split in one
   * currency account, and another split in the other currency account.
   */
  char    *currency;

  /* The parent and children pointers are used to implement an account
   * heirarchy, of accounts that have sub-accounts ("detail accounts").
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

  /* the "changed" flag helps the gui keep track of 
   * changes to this account */
  short changed;

  /* the "open" flag indicates if the account has been 
   * opened for editing. */
  short open;
};


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

#endif /* __XACC_ACCOUNT_P_H__ */
