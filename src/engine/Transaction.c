/********************************************************************\
 * Transaction.c -- the transaction data structure                  *
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

#include <string.h>

#include "config.h"

#include "Account.h"
#include "AccountP.h"
#include "date.h"
#include "Transaction.h"
#include "TransactionP.h"
#include "util.h"


/* if the "force_double_entry" flag has a non-zero value,
 * then all transactions will be *forced* to balance.
 * This will be forced even if it requires a new split 
 * to be created.
 */
int force_double_entry = 0;

/********************************************************************\
 * Because I can't use C++ for this project, doesn't mean that I    *
 * can't pretend too!  These functions perform actions on the       *
 * Transaction data structure, in order to encapsulate the          *
 * knowledge of the internals of the Transaction in one file.       *
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
  Transaction *trans;

  if (!split) return;

  /* free a split only if it is not claimed
   * by any accounts. */
  if (split->acc) return;

  /* free the split only if its not a source split */
  trans = split->parent;
  if (&(trans->source_split) == split) return;

  xaccTransRemoveSplit (trans, split);

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

#define MARK_SPLIT(split) {			\
   Account *acc = (Account *) ((split)->acc);	\
   if (acc) acc->changed = 1;			\
}

static void
MarkChanged (Transaction *trans)
{
   MARK_SPLIT (&(trans->source_split));

   if (trans->dest_splits) {
      int i=0;
      while (trans->dest_splits[i]) {
         MARK_SPLIT (trans->dest_splits[i]);
         i++;
      }
   }
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

void xaccSplitSetShareAmount (Split *s, double amt)
{
   MARK_SPLIT(s);
   s -> damount = amt;
}

void xaccSplitSetValue (Split *s, double amt)
{
   MARK_SPLIT(s);
   /* remember, damount is actually share price */
   s -> damount = amt / (s->share_price);
}

/********************************************************************\
\********************************************************************/

double xaccSplitGetBalance (Split *s) 
{
   if (!s) return 0.0;
   return s->balance;
}

double xaccSplitGetClearedBalance (Split *s) 
{
   if (!s) return 0.0;
   return s->cleared_balance;
}

double xaccSplitGetReconciledBalance (Split *s) 
{
   if (!s) return 0.0;
   return s->reconciled_balance;
}

double xaccSplitGetShareBalance (Split *s) 
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

  trans->dest_splits    = (Split **) _malloc (sizeof (Split *));
  trans->dest_splits[0] = NULL;

  xaccInitSplit ( &(trans->source_split));
  trans->source_split.parent = trans;

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
  if (trans->source_split.acc) return;

  i = 0;
  s = trans->dest_splits[i];
  while (s) {
    if (s->acc) return;
    i++;
    s = trans->dest_splits[i];
  }

  /* free up the destination splits */
  i = 0;
  s = trans->dest_splits[i];
  while (s) {
    xaccFreeSplit (s);
    i++;
    s = trans->dest_splits[i];
  }

  _free (trans->dest_splits);


  /* free up the source-split related stuff. */
  free(trans->source_split.memo);
  free(trans->source_split.action);

  /* just in case someone looks up freed memory ... */
  trans->source_split.memo        = 0x0;
  trans->source_split.reconciled  = NREC;
  trans->source_split.damount     = 0.0;
  trans->source_split.share_price = 1.0;
  trans->source_split.parent      = NULL;

  /* free up transaction strings */
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
\********************************************************************/

void
xaccTransRebalance (Transaction * trans)
{
  xaccSplitRebalance (&(trans->source_split));
}

/* hack alert -- the Rebalance algorithm is wrong. Needs fixing */
void
xaccSplitRebalance (Split *split)
{
  Transaction *trans;
  Split *s;
  int i = 0;
  double value = 0.0;


  trans = split->parent;

  if (&(trans->source_split) == split) {
    /* The indicated split is the source split.
     * Pick a destination split (by default, 
     * the first destination split), and force 
     * the total on it. 
     */

    s = trans->dest_splits[0];
    if (s) {
      /* first, add the source split */
      value = split->share_price * split->damount;

      /* now add in the sum of the destination splits */
      i = 0;
      while (s) {
        value += s->share_price * s->damount;
        i++;
        s = trans->dest_splits[i];
      }

      /* subtract the first destination split */
      s = trans->dest_splits[0];
      value -= (s->share_price) * (s->damount);

      /* the new value of the destination split 
       * will be the result.
       */
      s -> damount = - (value / (s->share_price));   
      MARK_SPLIT (s);

    } else{
      /* There are no destination splits !! 
       * Either this is allowed, in which case 
       * we just blow it off, or its forbidden,
       * in which case we force a balacing split 
       * to be created.
       */

       if (force_double_entry) {
          value = split->share_price * split->damount;

          /* malloc a new split, mirror it to the source split */
          s = xaccMallocSplit ();
          s->damount = -value;
          free (s->memo);
          s->memo = strdup (split->memo);
          free (s->action);
          s->action = strdup (split->action);

          /* insert the new split into the transaction and 
           * the same account as the source split */
          xaccTransAppendSplit (trans, s); 
          xaccAccountInsertSplit (split->acc, s);
          MARK_SPLIT (s);
       }
    }
  } else {

    /* The indicated split is a destination split.
     * Compute grand total of all distination splits,
     * and force the source split to blanace.
     */
    i = 0;
    s = trans->dest_splits[i];
    value = 0.0;
    while (s) {
      value += s->share_price * s->damount;
      i++;
      s = trans->dest_splits[i];
    }

    s = &(trans->source_split);
    s -> damount = - (value / (s->share_price));   
    MARK_SPLIT (s);
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
   num = xaccCountSplits (trans->dest_splits);

   oldarray = trans->dest_splits;
   trans->dest_splits = (Split **) _malloc ((num+2)*sizeof(Split *));
   for (i=0; i<num; i++) {
      (trans->dest_splits)[i] = oldarray[i];
   }
   trans->dest_splits[num] = split;
   trans->dest_splits[num+1] = NULL;

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

   s = trans->dest_splits[0];
   while (s) {
     trans->dest_splits[i] = trans->dest_splits[n];
     if (split == s) { i--; }
     i++;
     n++;
     s = trans->dest_splits[n];
   }
   trans->dest_splits[i] = NULL;
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
   Split *split;
   Account *acc;

   trans->date.year = year;
   trans->date.month = mon;
   trans->date.day = day;

   /* since the date has changed, we need to be careful to 
    * make sure all associated splits are in proper order
    * in thier accounts.  The easiest way of ensuring this
    * is to remove and reinsert every split. The reinsertion
    * process will place the split in the correct date-sorted
    * order.
    */

   split = &(trans->source_split);
   acc = (Account *) split->acc;
   xaccAccountRemoveSplit (acc, split);
   xaccAccountInsertSplit (acc, split);
   xaccRecomputeBalance (acc);

   if (trans->dest_splits) {
      int i=0;
      split = trans->dest_splits[i];
      while (split) {
         acc = (Account *) split->acc;
         xaccAccountRemoveSplit (acc, split);
         xaccAccountInsertSplit (acc, split);
         xaccRecomputeBalance (acc);

         i++;
         split = trans->dest_splits[i];
      }
   }
}

void
xaccTransSetDateToday (Transaction *trans)
{
   Date d;

   todaysDate (&d);
   xaccTransSetDate (trans, d.day, d.month, d.year);
}

void
xaccTransSetDateStr (Transaction *trans, char *str)
{
   Date d;

   sscandate (str, &d, DATE_FULL);
   xaccTransSetDate (trans, d.day, d.month, d.year);
}

/********************************************************************\
\********************************************************************/


void
xaccTransSetNum (Transaction *trans, const char *xnum)
{
   if (trans->num) free (trans->num);
   trans->num = strdup (xnum);
   MarkChanged (trans);
}

void
xaccTransSetDescription (Transaction *trans, const char *desc)
{
   if (trans->description) free (trans->description);
   trans->description = strdup (desc);
   MarkChanged (trans);
}

void
xaccTransSetMemo (Transaction *trans, const char *memo)
{
   if (trans->source_split.memo) free (trans->source_split.memo);
   trans->source_split.memo = strdup (memo);
   MARK_SPLIT (&(trans->source_split));

   /* if there is only one split, then keep memos in sync. */
   if (trans->dest_splits) {
      if (0x0 != trans->dest_splits[0]) {
         if (0x0 == trans->dest_splits[1]) {
            free (trans->dest_splits[0]->memo);
            trans->dest_splits[0]->memo = strdup (memo);
            MARK_SPLIT (trans->dest_splits[0]);
         }
      }
   }
}

void
xaccTransSetAction (Transaction *trans, const char *actn)
{
   if (trans->source_split.action) free (trans->source_split.action);
   trans->source_split.action = strdup (actn);
   MARK_SPLIT (&(trans->source_split));

   /* if there is only one split, then keep action in sync. */
   if (trans->dest_splits) {
      if (0x0 != trans->dest_splits[0]) {
         if (0x0 == trans->dest_splits[1]) {
            free (trans->dest_splits[0]->action);
            trans->dest_splits[0]->action = strdup (actn);
            MARK_SPLIT (trans->dest_splits[0]);
         }
      }
   }
}

void
xaccTransSetReconcile (Transaction *trans, char recn)
{
   trans->source_split.reconciled = recn;
   MARK_SPLIT (&(trans->source_split));
}

/********************************************************************\
\********************************************************************/
Split *
xaccTransGetSourceSplit (Transaction *trans) 
{
   return (&(trans->source_split));
}

Split *
xaccTransGetDestSplit (Transaction *trans, int i) 
{
   if (trans->dest_splits) {
      return (trans->dest_splits[i]);
   }
   return NULL;
}

char *
xaccTransGetNum (Transaction *trans)
{
   return (trans->num);
}

char * 
xaccTransGetDescription (Transaction *trans)
{
   return (trans->description);
}

Date *
xaccTransGetDate (Transaction *trans)
{
   return (&(trans->date));
}

char *
xaccTransGetDateStr (Transaction *trans)
{

   char buf [100];
   sprintf( buf, "%2d/%2d/%02d",
             trans->date.month,
             trans->date.day,
             (trans->date.year%100) );
   return strdup (buf);
}

int 
xaccTransCountSplits (Transaction *trans)
{
   return (xaccCountSplits (trans->dest_splits));
}

int
xaccTransIsSource (Transaction *trans, Split *split)
{
   return (split == &(trans->source_split));
}

/********************************************************************\
\********************************************************************/

void
xaccSplitSetMemo (Split *split, const char *memo)
{
   if (split->memo) free (split->memo);
   split->memo = strdup (memo);
   MARK_SPLIT (split);
}

void
xaccSplitSetAction (Split *split, const char *actn)
{
   if (split->action) free (split->action);
   split->action = strdup (actn);
   MARK_SPLIT (split);
}

void
xaccSplitSetReconcile (Split *split, char recn)
{
   split->reconciled = recn;
   MARK_SPLIT (split);
}

/********************************************************************\
\********************************************************************/

/* return the parent transaction of the split */
Transaction * 
xaccSplitGetParent (Split *split)
{
   return (split->parent);
}

Account *
xaccSplitGetAccount (Split *split)
{
   return (split->acc);
}

char *
xaccSplitGetMemo (Split *split)
{
   return (split->memo);
}

char *
xaccSplitGetAction (Split *split)
{
   return (split->action);
}

char 
xaccSplitGetReconcile (Split *split)
{
   return (split->reconciled);
}

double
xaccSplitGetAmount (Split * split)
{
   return (split->damount);
}

double
xaccSplitGetValue (Split * split)
{
   return ((split->damount) * (split->share_price));
}

double
xaccSplitGetSharePrice (Split * split)
{
   return (split->share_price);
}

/************************ END OF ************************************\
\************************* FILE *************************************/
