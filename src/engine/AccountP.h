/********************************************************************\
 * AccountP.h -- Account engine-private data structure              *
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

#include "Account.h"
#include "GNCId.h"
#include "Transaction.h"
#include "gnc-commodity.h"
#include "gnc-numeric.h"
#include "kvp_frame.h"


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

  /* kvp_data is a key-value pair database for storing simple "extra"
   * information in splits, transactions, and accounts.  it's NULL
   * until accessed.  See ??? for a list and description of the
   * important keys. */
  kvp_frame * kvp_data;

  /* The type field is the account type, picked from the enumerated 
   * list that includes BANK, STOCK, CREDIT, INCOME, etc.  Its
   * intended use is to be a hint to the GUI as to how to display   
   * and format the transaction data.
   */
  GNCAccountType type;

  /* The currency field denotes the default currency in which all
   * splits in this account are denominated.  The gnc_commodity type
   * represents the namespace, full name, and symbol for the currency.
   * Currency trading accounts allow splits between accounts when the
   * currency string matches the security string.  */
  const gnc_commodity * currency;
  const gnc_commodity * security;
  int  currency_scu;
  int  security_scu;

  /* The parent and children pointers are used to implement an account
   * hierarchy, of accounts that have sub-accounts ("detail accounts").
   */
  AccountGroup *parent;    /* back-pointer to parent */
  AccountGroup *children;  /* pointer to sub-accounts */

  /* protected data, cached parameters */
  gnc_numeric balance;
  gnc_numeric cleared_balance;
  gnc_numeric reconciled_balance;

  gnc_numeric share_balance;
  gnc_numeric share_cleared_balance;
  gnc_numeric share_reconciled_balance;

  GList *splits;           /* list of split pointers */

  /* The "changed" flag is used to invalidate cached values in this structure.
   * Currently, the balances and the cost basis are cached.
   */
  /*short changed;*/

  /* The "open" flag indicates if the account has been 
   * opened for editing. */
  /* short open; */

  /* keep track of nesting level of begin/end edit calls */
  gint32 editlevel;

  gboolean balance_dirty;
  gboolean sort_dirty;

  /* The "mark" flag can be used by the user to mark this account
   * in any way desired.  Handy for specialty traversals of the 
   * account tree. */
  short mark;
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
 * transaction) and the total balance, for this account */
void         xaccAccountRecomputeBalance (Account *);

/* Set the account's GUID. This should only be done when reading
 * an account from a datafile, or some other external source. Never
 * call this on an existing account! */
void         xaccAccountSetGUID (Account *account, GUID *guid);

#endif /* __XACC_ACCOUNT_P_H__ */
