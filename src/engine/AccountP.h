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

  AccountGroup *parent;    /* back-pointer to parent */
  AccountGroup *children;  /* pointer to sub-accounts */
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
