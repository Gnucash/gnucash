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

  Split   source_split;      /* source (creidted) account                 */
  Split   **dest_splits;     /* list of splits, null terminated           */

  char    write_flag;        /* used only during file IO                  */

  /* the "open" flag indicates if the transaction has been 
   * opened for editing. */
  char open;
};


#endif /* __XACC_TRANSACTION_P_H__ */
