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

#ifndef XACC_ACCOUNT_P_H
#define XACC_ACCOUNT_P_H

#include "config.h"

#include "Account.h"
#include "GNCId.h"
#include "Transaction.h"
#include "gnc-commodity.h"
#include "gnc-numeric.h"
#include "kvp_frame.h"


/** STRUCTS *********************************************************/
struct account_s
{
  /* public data, describes account */
  GUID      guid;          /* globally unique account id */

  /* The accountName is an arbitrary string assigned by the user. 
   * It is intended to a short, 5 to 30 character long string that
   * is displayed by the GUI as the account mnemonic. 
   */
  char     *accountName;

  /* The accountCode is an arbitrary string assigned by the user.
   * It is intended to be reporting code that is a synonym for the 
   * accountName. Typically, it will be a numeric value that follows 
   * the numbering assignments commonly used by accountants, such 
   * as 100, 200 or 600 for top-level * accounts, and 101, 102..  etc.
   * for detail accounts.
   */
  char     *accountCode;

  /* The description is an arbitrary string assigned by the user. 
   * It is intended to be a longer, 1-5 sentence description of what
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

  /* Old semantics: The currency field denotes the default currency in
   * which all splits in this account are denominated.  Currency
   * trading accounts allow splits between accounts when the currency
   * string matches the security string.
   *
   * The gnc_commodity type represents the namespace, full name, and
   * symbol for the currency.
   *
   * New semantics: The account structure will no longer store a
   * 'currency' and a 'security'. Instead it will store only one
   * commodity (i.e. currency), that is the one formerly known as
   * 'security'. The 'amount' of each split represents the
   * transferred amount in the account's commodity (formerly known as
   * security).
   */
  gnc_commodity * commodity;
  int commodity_scu;

  /* The parent and children pointers are used to implement an account
   * hierarchy, of accounts that have sub-accounts ("detail accounts").
   */
  AccountGroup *parent;    /* back-pointer to parent */
  AccountGroup *children;  /* pointer to sub-accounts */

  /* protected data, cached parameters */
  gnc_numeric starting_balance;
  gnc_numeric starting_cleared_balance;
  gnc_numeric starting_reconciled_balance;

  gnc_numeric balance;
  gnc_numeric cleared_balance;
  gnc_numeric reconciled_balance;

  /* version number, used for tracking multiuser updates */
  gint32 version;
  guint32  version_check;  /* data aging timestamp */

  GList *splits;           /* list of split pointers */

  /* keep track of nesting level of begin/end edit calls */
  gint32 editlevel;

  gboolean balance_dirty;  /* balances in splits incorrect */
  gboolean sort_dirty;     /* sort order of splits is bad */
  gboolean core_dirty;     /* fields in this struct have changed */
  gboolean do_free;        /* in process of being destroyed */

  /* The "mark" flag can be used by the user to mark this account
   * in any way desired.  Handy for specialty traversals of the 
   * account tree. */
  short mark;

  /* -------------------------------------------------------------- */
  /* Backend private expansion data */
  guint32  idata;     /* used by the sql backend for kvp management */
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
 * transaction) and the total balance, for this account */
void         xaccAccountRecomputeBalance (Account *);

/* Set the account's GUID. This should only be done when reading
 * an account from a datafile, or some other external source. Never
 * call this on an existing account! */
void         xaccAccountSetGUID (Account *account, const GUID *guid);

/* The xaccAccountSetStartingBalance() routine will set the starting
 *    commodity balance for this account.  This routine is intended for
 *    use with backends that do not return the complete list of splits
 *    for an account, but rather return a partial list.  In such a case,
 *    the backend will typically return all of the splits after some 
 *    certain date, and the 'starting balance' will represent the summation 
 *    of the splits up to that date.
 *
 *    Design Note: this routine assumes that there is only one commodity
 *    associated with this account, and that the reporting currency will
 *    no longer be stored with the account.
 *
 *    This routine is in the private .h file because only backends are 
 *    allowed to set the starting balance.  This is *not* a user interface
 *    function.
 */
void xaccAccountSetStartingBalance(Account *account, 
                                   const gnc_numeric start_baln, 
                                   const gnc_numeric start_cleared_baln, 
                                   const gnc_numeric start_reconciled_baln); 

/* The xaccFreeAccount() routine releases memory associated with the
 *    account.  It should never be called directly from user code;
 *    instead, the xaccAccountDestroy() routine should be used
 *    (because xaccAccountDestroy() has the correct commit semantics).
 */

void xaccFreeAccount (Account *account);

/* The xaccAccountSet/GetVersion() routines set & get the version 
 *    numbers on this account.  The version number is used to manage
 *    multi-user updates.  These routines are private because we don't
 *    want anyone except the backend to mess with them.
 */
void xaccAccountSetVersion (Account*, gint32);
gint32 xaccAccountGetVersion (Account*);

#endif /* XACC_ACCOUNT_P_H */
