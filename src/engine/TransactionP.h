/********************************************************************\
 * TransactionP.h -- private header for transaction & splits        *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1997, 1998, 1999, 2000 Linas Vepstas               *
 * Copyright (C) 2000 Bill Gribble                                  *
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

#ifndef XACC_TRANSACTION_P_H
#define XACC_TRANSACTION_P_H

#include <time.h>
#include <glib.h>

#include "config.h"
#include "gnc-engine.h"   /* for typedefs */
#include "gnc-numeric.h"
#include "GNCIdP.h"
#include "kvp_frame.h"


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


struct split_s
{
  GUID guid;  /* globally unique id */

  GNCBook *book;             /* The enitity table where this split is stored. */

  Account *acc;              /* back-pointer to debited/credited account  */

  GNCLot *lot;               /* back-pointer to debited/credited lot */

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

  /* kvp_data is a key-value pair database for storing simple 
   * "extra" information in splits, transactions, and accounts. 
   * it's NULL until accessed. */
  kvp_frame * kvp_data;

  char    reconciled;        /* The reconciled field                      */
  Timespec date_reconciled;  /* date split was reconciled                 */

  /* 'value' is the amount of the transaction balancing commodity
   * (i.e. currency) involved, 'amount' is the amount of the account's
   * commodity (formerly known as 'security') involved. */
  gnc_numeric  value;
  gnc_numeric  amount;

  /* -------------------------------------------------------------- */
  /* Below follow some 'temporary' fields */

  /* The various "balances" are the sum of all of the values of 
   * all the splits in the account, up to and including this split.
   * These balances apply to a sorting order by date posted
   * (not by date entered). */
  gnc_numeric  balance;
  gnc_numeric  cleared_balance;
  gnc_numeric  reconciled_balance;

  /* -------------------------------------------------------------- */
  /* Backend private expansion data */
  guint32  idata;     /* used by the sql backend for kvp management */
};


struct transaction_s
{
  /* guid is a globally unique identifier which can be used to
   * reference the transaction.
   */
  GUID guid;

  GNCBook *book;         /* The entity_table where the transaction is stored */

  Timespec date_entered;     /* date register entry was made              */
  Timespec date_posted;      /* date transaction was posted at bank       */

  /* The num field is a arbitrary user-assigned field.  
   * It is intended to store a short id number, typically the check number,
   * deposit number, invoice number or other tracking number.
   */
  char * num;  

  /* The description field is an arbitrary user-assigned value. 
   * It is meant to be a short descriptive phrase.
   */
  char * description;        

  /* kvp_data is a key-value pair database for storing simple 
   * "extra" information in splits, transactions, and accounts. 
   * it's NULL until accessed. */
  kvp_frame * kvp_data;


  /* The common_currency field is the balancing common currency for
   * all the splits in the transaction. 
   *
   * This field is going to replace the currency field in the account
   * structures.  However, right now we are in a transition period: we
   * store it here an in the account, and test its value dynamically
   * for correctness.  If we can run for a few months without errors,
   * then we'll make the conversion permanent.
   *
   * Alternate, better(?) name: "valuation currency": it is the
   * currency in which all of the splits can be valued.  */
  gnc_commodity *common_currency;

  /* version number, used for tracking multiuser updates */
  gint32 version;
  guint32 version_check; /* data aging timestamp */

  GList * splits; /* list of splits */

  /* marker is used to track the progress of transaction traversals. 
   * 0 is never a legitimate marker value, so we can tell is we hit
   * a new transaction in the middle of a traversal. All each new
   * traversal cares about is whether or not the marker stored in
   * a transaction is the same as or different than the one
   * corresponding to the current traversal. */
  unsigned char  marker;      

  gint32 editlevel; /* nestcount of begin/end edit calls */
  gboolean do_free; /* transaction in process of being destroyed */

  /* the orig pointer points at a copy of the original transaction,
   * before editing was started.  This orig copy is used to rollback 
   * any changes made if/when the edit is abandoned.
   */
  Transaction *orig;

  /* -------------------------------------------------------------- */
  /* Backend private expansion data */
  guint32  idata;     /* used by the sql backend for kvp management */
};

/* Set the transaction's GUID. This should only be done when reading
 * a transaction from a datafile, or some other external source. Never
 * call this on an existing transaction! */
void xaccTransSetGUID (Transaction *trans, const GUID *guid);

/* Set the split's GUID. This should only be done when reading
 * a split from a datafile, or some other external source. Never
 * call this on an existing split! */
void xaccSplitSetGUID (Split *split, const GUID *guid);

/* The xaccFreeSplit() method simply frees all memory associated
 * with the split.  It does not verify that the split isn't
 * referenced in some account.  If the split is referenced by an 
 * account, then calling this method will leave the system in an 
 * inconsistent state.  This *will* lead to crashes and hangs.
 */
void  xaccFreeSplit (Split *split);    /* frees memory */

/* This routine makes a 'duplicate' of the indicated transaction.
 * This routine cannot be exposed publically since the duplicate
 * is wrong in many ways: it is not issued a unique guid, and thus
 * not a properly registered Entity.  The splits are copied, but
 * these are also funny: they aren't inserted into the accounts 
 * they claim to be in.  The splits also have bogus GUID's.
 * Another 'feature': the splits point at the old transaction
 * as the parent, not the new transaction.  
 */
Transaction * xaccDupeTransaction (Transaction *t);

/* compute the value of a list of splits in the given currency,
 * excluding the skip_me split. */
gnc_numeric xaccSplitsComputeValue (GList *splits, Split * skip_me,
                                    const gnc_commodity * base_currency);

/* The xaccTransSet/GetVersion() routines set & get the version
 *    numbers on this transaction.  The version number is used to manage
 *    multi-user updates.  These routines are private because we don't
 *    want anyone except the backend to mess with them.
 */
void xaccTransSetVersion (Transaction*, gint32);
gint32 xaccTransGetVersion (Transaction*);

/* The xaccTransFindCommonCurrency () method returns a gnc_commodity
 *    indicating a currency denomination that all of the splits in this
 *    transaction have in common, using the old currency/security fields
 *    of the split accounts. */
gnc_commodity * xaccTransFindOldCommonCurrency (Transaction *trans,
                                                GNCBook *book);

/* Code to register Split and Transaction types with the engine */
gboolean xaccSplitRegister (void);
gboolean xaccTransRegister (void);


#endif /* XACC_TRANSACTION_P_H */
