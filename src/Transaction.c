/********************************************************************\
 * Transaction.c -- the transaction data structure                  *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1997 Linas Vepstas                                 *
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

#include <string.h>

#include "config.h"

#include "date.h"
#include "Transaction.h"
#include "util.h"

/********************************************************************\
 * Because I can't use C++ for this project, doesn't mean that I    *
 * can't pretend too!  These functions perform actions on the       *
 * Transaction data structure, in order to encapsulate the knowledge       *
 * of the internals of the Transaction in one file.                        *
\********************************************************************/

/********************************************************************\
 * xaccInitSplit
 * Initialize a splitaction structure
\********************************************************************/

void
xaccInitSplit( Split * split )
  {
  
  /* fill in some sane defaults */
  split->acc         = NULL;
  split->parent      = NULL;
  
  split->action      = strdup("");
  split->memo        = strdup("");
  split->reconciled  = NREC;
  split->damount     = 0.0;
  split->share_price = 1.0;
  split->balance             = 0.0;
  split->cleared_balance     = 0.0;
  split->reconciled_balance  = 0.0;
  split->share_balance             = 0.0;
  split->share_cleared_balance     = 0.0;
  split->share_reconciled_balance  = 0.0;

  }

/********************************************************************\
\********************************************************************/
Split *
xaccMallocSplit( void )
  {
  Split *split = (Split *)_malloc(sizeof(Split));
  xaccInitSplit (split);
  return split;
  }

/********************************************************************\
\********************************************************************/

void
xaccFreeSplit( Split *split )
  {
  if (!split) return;

  /* free a split only if it is not claimed
   * by any accounts. */
  if (split->acc) return;

  xaccTransRemoveSplit (split->parent, split);

  free(split->memo);
  free(split->action);

  /* just in case someone looks up freed memory ... */
  split->memo        = 0x0;
  split->reconciled  = NREC;
  split->damount     = 0.0;
  split->share_price = 1.0;
  split->parent      = NULL;

  _free(split);
}

/********************************************************************\
\********************************************************************/

int
xaccCountSplits (Split **tarray)
{
   Split *split;
   int nsplit = 0;

   if (!tarray) return 0;

   split = tarray[0];
   while (split) {
      nsplit ++;
      split = tarray[nsplit];
   }
   return nsplit;
}

/********************************************************************\
\********************************************************************/

void xaccSetShareAmount (Split *s, double amt)
{
   s -> damount = amt;
}

void xaccSetAmount (Split *s, double amt)
{
   /* remember, damount is actually share price */
   s -> damount = amt / (s->share_price);
}

/********************************************************************\
\********************************************************************/

double xaccGetBalance (Split *s) 
{
   if (!s) return 0.0;
   return s->balance;
}

double xaccGetClearedBalance (Split *s) 
{
   if (!s) return 0.0;
   return s->cleared_balance;
}

double xaccGetReconciledBalance (Split *s) 
{
   if (!s) return 0.0;
   return s->reconciled_balance;
}

double xaccGetShareBalance (Split *s) 
{
   if (!s) return 0.0;
   return s->share_balance;
}

/********************************************************************\
 * xaccInitTransaction
 * Initialize a transaction structure
\********************************************************************/

void
xaccInitTransaction( Transaction * trans )
  {
  
  /* fill in some sane defaults */
  trans->num         = strdup("");
  trans->description = strdup("");

  trans->debit_splits    = (Split **) _malloc (sizeof (Split *));
  trans->debit_splits[0] = NULL;

  xaccInitSplit ( &(trans->credit_split));
  trans->credit_split.parent = trans;

  trans->date.year   = 1900;        
  trans->date.month  = 1;        
  trans->date.day    = 1;        
  }

/********************************************************************\
\********************************************************************/

Transaction *
xaccMallocTransaction( void )
  {
  Transaction *trans = (Transaction *)_malloc(sizeof(Transaction));
  xaccInitTransaction (trans);
  return trans;
  }

/********************************************************************\
\********************************************************************/

void
xaccFreeTransaction( Transaction *trans )
  {
  int i;
  Split *s;

  if (!trans) return;

  /* free a transaction only if it is not claimed
   * by any accounts. */
  if (trans->credit_split.acc) return;

  i = 0;
  s = trans->debit_splits[i];
  while (s) {
    if (s->acc) return;
    i++;
    s = trans->debit_splits[i];
  }

  _free (trans->debit_splits);
  free(trans->num);
  free(trans->description);

  /* just in case someone looks up freed memory ... */
  trans->num         = 0x0;
  trans->description = 0x0;

  trans->date.year   = 1900;        
  trans->date.month  = 1;        
  trans->date.day    = 1;        

  _free(trans);
}

/********************************************************************\
 * Walk the debit-splits array, and compute the new 
 * credit amounts based on that.
\********************************************************************/

void
xaccTransRecomputeAmount (Transaction *trans)
{
  Split *s;
  int i = 0;
  double amount = 0.0;

  s = trans->debit_splits[i];
  while (s) {
    amount += s->share_price * s->damount;
    i++;
    s = trans->debit_splits[i];
  }

  /* if there is just one split, then the credited 
   * and the debited splits should match up. */
  if (1 == i) {
    s = trans->debit_splits[0];
    trans -> credit_split.damount = - (s->damount);
    trans -> credit_split.share_price = s->share_price;
  } else {
    trans -> credit_split.damount = -amount;
    trans -> credit_split.share_price = 1.0;
  }
  
}

/********************************************************************\
\********************************************************************/
void
xaccTransAppendSplit (Transaction *trans, Split *split) 
{
   int i, num;
   Split **oldarray;

   if (!trans) return;
   if (!split) return;
   
   /* first, insert the split into the array */
   split->parent = (struct _transaction *) trans;
   num = xaccCountSplits (trans->debit_splits);

   oldarray = trans->debit_splits;
   trans->debit_splits = (Split **) _malloc ((num+2)*sizeof(Split *));
   for (i=0; i<num; i++) {
      (trans->debit_splits)[i] = oldarray[i];
   }
   trans->debit_splits[num] = split;
   trans->debit_splits[num+1] = NULL;

   if (oldarray) _free (oldarray);
}

/********************************************************************\
\********************************************************************/

void
xaccTransRemoveSplit (Transaction *trans, Split *split) 
{
   int i=0, n=0;
   Split *s;

   if (!split) return;
   if (!trans) return;
   split->parent = NULL;

   s = trans->debit_splits[0];
   while (s) {
     trans->debit_splits[i] = trans->debit_splits[n];
     if (split == s) { i--; }
     i++;
     n++;
     s = trans->debit_splits[n];
   }
   trans->debit_splits[i] = NULL;

   /* bring dollar amounts into synchrony */
   xaccTransRecomputeAmount (trans);
}

/********************************************************************\
 * sorting comparison function
 *
 * returns a negative value if transaction a is dated earlier than b, 
 * returns a positive value if transaction a is dated later than b, 
 *
 * This function tries very hard to uniquely order all transactions.
 * If two transactions occur on the same date, then thier "num" fields
 * are compared.  If the num fileds are identical, then the description
 * fileds are compared.  If these are identical, then the memo fileds 
 * are compared.  Hopefully, there will not be any transactions that
 * occur on the same day that have all three of these values identical.
 *
 * Note that being able to establish this kind of absolute order is 
 * important for some of the ledger display functions.  In particular,
 * grep for "running_balance" in the code, and see the notes there.
 *
 * Yes, this kindof code dependency is ugly, but the alternatives seem
 * ugly too.
 *
\********************************************************************/
int
xaccSplitOrder (Split **sa, Split **sb)
{
  int retval;
  char *da, *db;

  if ( (*sa) && !(*sb) ) return -1;
  if ( !(*sa) && (*sb) ) return +1;
  if ( !(*sa) && !(*sb) ) return 0;

  retval = xaccTransOrder ( ((Transaction **) &((*sa)->parent)), 
                            ((Transaction **) &((*sb)->parent)));
  if (0 != retval) return retval;

  /* otherwise, sort on memo strings */
  da = (*sa)->memo;
  db = (*sb)->memo;
  if (da && db) {
    retval = strcmp (da, db);
    /* if strings differ, return */
    if (retval) return retval;
  } else 
  if (!da && db) {
    return -1;
  } else 
  if (da && !db) {
    return +1;
  }

  /* otherwise, sort on action strings */
  da = (*sa)->action;
  db = (*sb)->action;
  if (da && db) {
    retval = strcmp (da, db);
    /* if strings differ, return */
    if (retval) return retval;
  } else 
  if (!da && db) {
    return -1;
  } else 
  if (da && !db) {
    return +1;
  }

  return 0;
}


int
xaccTransOrder (Transaction **ta, Transaction **tb)
{
  int retval;
  char *da, *db;

  if ( (*ta) && !(*tb) ) return -1;
  if ( !(*ta) && (*tb) ) return +1;
  if ( !(*ta) && !(*tb) ) return 0;

  /* if dates differ, return */
  retval = datecmp (&((*ta)->date), &((*tb)->date));
  if (retval) return retval;

  /* otherwise, sort on transaction strings */
  da = (*ta)->num;
  db = (*tb)->num;
  if (da && db) {
    retval = strcmp (da, db);
    /* if strings differ, return */
    if (retval) return retval;
  } else 
  if (!da && db) {
    return -1;
  } else 
  if (da && !db) {
    return +1;
  }

  /* otherwise, sort on transaction strings */
  da = (*ta)->description;
  db = (*tb)->description;
  if (da && db) {
    retval = strcmp (da, db);
    /* if strings differ, return */
    if (retval) return retval;
  } else 
  if (!da && db) {
    return -1;
  } else 
  if (da && !db) {
    return +1;
  }

  return 0;
}

/********************************************************************\
\********************************************************************/

int
xaccCountTransactions (Transaction **tarray)
{
   Transaction *trans;
   int ntrans = 0;

   if (!tarray) return 0;

   trans = tarray[0];
   while (trans) {
      ntrans ++;
      trans = tarray[ntrans];
   }
   return ntrans;
}

/********************************************************************\
\********************************************************************/

void
xaccTransSetDate (Transaction *trans, int day, int mon, int year)
{
   trans->date.year = year;
   trans->date.month = mon;
   trans->date.day = day;
}

void
xaccTransSetNum (Transaction *trans, const char *xnum)
{
   if (trans->num) free (trans->num);
   trans->num = strdup (xnum);
}

void
xaccTransSetDescription (Transaction *trans, const char *desc)
{
   if (trans->description) free (trans->description);
   trans->description = strdup (desc);
}

void
xaccTransSetMemo (Transaction *trans, const char *memo)
{
   if (trans->credit_split.memo) free (trans->credit_split.memo);
   trans->credit_split.memo = strdup (memo);

   /* if there is only one split, then keep memos in sync. */
   if (trans->debit_splits) {
      if (0x0 == trans->debit_splits[1]) {
         free (trans->debit_splits[0]->memo);
         trans->debit_splits[0]->memo = strdup (memo);
      }
   }
}

void
xaccTransSetAction (Transaction *trans, const char *actn)
{
   if (trans->credit_split.action) free (trans->credit_split.action);
   trans->credit_split.action = strdup (actn);

   /* if there is only one split, then keep action in sync. */
   if (trans->debit_splits) {
      if (0x0 == trans->debit_splits[1]) {
         free (trans->debit_splits[0]->action);
         trans->debit_splits[0]->action = strdup (actn);
      }
   }
}

void
xaccTransSetReconcile (Transaction *trans, char recn)
{
   trans->credit_split.reconciled = recn;
}

/********************************************************************\
\********************************************************************/

void
xaccSplitSetMemo (Split *split, const char *memo)
{
   if (split->memo) free (split->memo);
   split->memo = strdup (memo);
}

void
xaccSplitSetAction (Split *split, const char *actn)
{
   if (split->action) free (split->action);
   split->action = strdup (actn);
}

void
xaccSplitSetReconcile (Split *split, char recn)
{
   split->reconciled = recn;
}

/************************ END OF ************************************\
\************************* FILE *************************************/
