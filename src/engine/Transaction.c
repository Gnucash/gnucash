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

#include <assert.h>
#include <string.h>

#include "config.h"

#include "Account.h"
#include "AccountP.h"
#include "date.h"
#include "Transaction.h"
#include "TransactionP.h"
#include "TransLog.h"
#include "util.h"


/* 
 * If the "force_double_entry" flag has a non-zero value,
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
  if (!split) return;

  free(split->memo);
  free(split->action);

  /* just in case someone looks up freed memory ... */
  split->memo        = 0x0;
  split->reconciled  = NREC;
  split->damount     = 0.0;
  split->share_price = 1.0;
  split->parent      = NULL;
  split->acc         = NULL;

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
   if (trans->splits) {
      int i=0;
      while (trans->splits[i]) {
         MARK_SPLIT (trans->splits[i]);
         i++;
      }
   }
}

/********************************************************************\
\********************************************************************/

void
xaccSplitDestroy (Split *split)
{
   Account *acc;
   Transaction *trans;
   int numsplits = 0;
   int ismember = 0;
   Split *s;

   trans = split->parent;
   assert (trans);
   assert (trans->splits);

   numsplits = 0;
   s = trans->splits[0];
   while (s) {
/* xxxxxxx */
printf ("trans %p, %d %p\n", trans, numsplits, s);
      MARK_SPLIT(s);
      if (s == split) ismember = 1;
      numsplits ++;
      s = trans->splits[numsplits];
   }
   assert (ismember);

   /* if the accoount has three or more splits, 
    * merely unlink & free the split. 
    */
   if (2 < numsplits) {
      MARK_SPLIT (split);
      xaccTransRemoveSplit (trans, split);
      acc = split->acc;
      xaccAccountRemoveSplit (acc, split);
      xaccAccountRecomputeBalance (acc);
      xaccFreeSplit (split);
      xaccSplitRebalance (trans->splits[0]);
   } else {
      /* if the transaction has only two splits,
       * remove both of them, and them destroy the 
       * transaction. 
       */
      s = trans->splits[0];
      acc = s->acc;
      MARK_SPLIT (s);
      xaccAccountRemoveSplit (acc, s);
      xaccAccountRecomputeBalance (acc);

      s = trans->splits[1];
      acc = s->acc;
      MARK_SPLIT (s);
      xaccAccountRemoveSplit (acc, s);
      xaccAccountRecomputeBalance (acc);
      xaccFreeTransaction (trans);
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

void xaccSplitSetSharePriceAndAmount (Split *s, double price, double amt)
{
   MARK_SPLIT(s);
   s -> share_price = price;
   s -> damount = amt;

   /* force double entry to always balance */
   xaccSplitRebalance (s);
}

void xaccSplitSetSharePrice (Split *s, double amt)
{
   MARK_SPLIT(s);
   s -> share_price = amt;

   /* force double entry to always balance */
   xaccSplitRebalance (s);
}

void xaccSplitSetShareAmount (Split *s, double amt)
{
   MARK_SPLIT(s);
   s -> damount = amt;

   /* force double entry to always balance */
   xaccSplitRebalance (s);
}

void xaccSplitSetValue (Split *s, double amt)
{
   MARK_SPLIT(s);
   /* remember, damount is actually share price */
   s -> damount = amt / (s->share_price);

   /* force double entry to always balance */
   xaccSplitRebalance (s);
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
  Split *split;
  
  /* fill in some sane defaults */
  trans->num         = strdup("");
  trans->description = strdup("");

  trans->splits    = (Split **) _malloc (3* sizeof (Split *));

  /* create a pair of splits */
  split = xaccMallocSplit ();
  split->parent = trans;
  trans->splits[0] = split;

  split = xaccMallocSplit ();
  split->parent = trans;
  trans->splits[1] = split;

  trans->splits[2] = NULL;

  trans->date.year   = 1900;        
  trans->date.month  = 1;        
  trans->date.day    = 1;        
  trans->open        = 0;
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

  /* free up the destination splits */
  i = 0;
  s = trans->splits[i];
  while (s) {
    xaccFreeSplit (s);
    i++;
    s = trans->splits[i];
  }

  _free (trans->splits);

  /* free up transaction strings */
  free(trans->num);
  free(trans->description);

  /* just in case someone looks up freed memory ... */
  trans->num         = 0x0;
  trans->description = 0x0;

  trans->date.year   = 1900;        
  trans->date.month  = 1;        
  trans->date.day    = 1;        

  trans->open = 0;

  _free(trans);
}

/********************************************************************\
\********************************************************************/

void
xaccTransDestroy (Transaction *trans)
{
   int i;
   Split *split;
   Account *acc;

   if (!trans) return;

   i=0;
   split = trans->splits[i];
   while (split) {
      MARK_SPLIT (split);
      acc = split ->acc;
      xaccAccountRemoveSplit (acc, split);
      xaccAccountRecomputeBalance (acc); 
      i++;
      split = trans->splits[i];
   }

   xaccFreeTransaction (trans);
}

/********************************************************************\
\********************************************************************/

void
xaccTransBeginEdit (Transaction *trans)
{
   trans->open = 1;
   xaccOpenLog ();
}

void
xaccTransCommitEdit (Transaction *trans)
{
   trans->open = 0;
   xaccTransWriteLog (trans);
}

/********************************************************************\
\********************************************************************/

void
xaccTransRebalance (Transaction * trans)
{
  xaccSplitRebalance (trans->splits[0]);
}

void
xaccSplitRebalance (Split *split)
{
  Transaction *trans;
  Split *s;
  int i = 0;
  double value = 0.0;

  trans = split->parent;
  assert (trans);
  assert (trans->splits);
  assert (trans->splits[0]);

  if (split == trans->splits[0]) {
    /* The indicated split is the source split.
     * Pick a destination split (by default, 
     * the first destination split), and force 
     * the total on it. 
     */

    s = trans->splits[1];
    if (s) {
      /* first, add the source split */
      value = split->share_price * split->damount;

      /* now add in the sum of the destination splits */
      i = 1;
      while (s) {
        value += s->share_price * s->damount;
        i++;
        s = trans->splits[i];
      }

      /* subtract the first destination split */
      s = trans->splits[1];
      value -= (s->share_price) * (s->damount);

      /* the new value of the destination split 
       * will be the result.
       */
      s -> damount = - (value / (s->share_price));   
      MARK_SPLIT (s);
      xaccAccountRecomputeBalance (s->acc); 

    } else{
      /* There are no destination splits !! 
       * Either this is allowed, in which case 
       * we just blow it off, or its forbidden,
       * in which case we force a balacing split 
       * to be created.
       */

/* hack alert -- I think this is broken */
#ifdef HACK_ALERT
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
          MARK_SPLIT (s);
          xaccTransAppendSplit (trans, s); 
          xaccAccountInsertSplit (split->acc, s);
       }
#endif

    }
  } else {

    /* The indicated split is a destination split.
     * Compute grand total of all destination splits,
     * and force the source split to blanace.
     */
    i = 1;
    s = trans->splits[i];
    value = 0.0;
    while (s) {
      value += s->share_price * s->damount;
      i++;
      s = trans->splits[i];
    }

    s = trans->splits[0];
    s -> damount = - (value / (s->share_price));   
    MARK_SPLIT (s);
    xaccAccountRecomputeBalance (s->acc); 
  }

  /* hack alert -- if the "force-double-entry" flag is set,
   * we should check to make sure that every split belongs
   * to some account.  If any of them don't, force them 
   * into the current account. If there's not current account,
   * force them into a lost & found account */
  /* hack alert -- implement the above */

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
   split->parent = trans;
   num = xaccCountSplits (trans->splits);

   oldarray = trans->splits;
   trans->splits = (Split **) _malloc ((num+2)*sizeof(Split *));
   for (i=0; i<num; i++) {
      (trans->splits)[i] = oldarray[i];
   }
   trans->splits[num] = split;
   trans->splits[num+1] = NULL;

   if (oldarray) _free (oldarray);

   /* force double entry to always be consistent */
   xaccSplitRebalance (split);
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

   s = trans->splits[0];
   while (s) {
     trans->splits[i] = trans->splits[n];
     if (split == s) { i--; }
     i++;
     n++;
     s = trans->splits[n];
   }
   trans->splits[i] = NULL;
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
   int i=0;

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

   assert (trans->splits);

   i=0;
   split = trans->splits[i];
   while (split) {
      acc = split->acc;
      xaccAccountRemoveSplit (acc, split);
      xaccAccountInsertSplit (acc, split);

      i++;
      split = trans->splits[i];
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

   scanDate(str, &(d.day), &(d.month), &(d.year));
   xaccTransSetDate (trans, d.day, d.month, d.year);
}

/********************************************************************\
\********************************************************************/


void
xaccTransSetNum (Transaction *trans, const char *xnum)
{
   char * tmp = strdup (xnum);
   if (trans->num) free (trans->num);
   trans->num = tmp;
   MarkChanged (trans);
}

void
xaccTransSetDescription (Transaction *trans, const char *desc)
{
   char * tmp = strdup (desc);
   if (trans->description) free (trans->description);
   trans->description = tmp;
   MarkChanged (trans);
}

#define SET_TRANS_FIELD(trans,field,value)			\
{								\
   if (!trans) return;						\
								\
   /* the engine *must* always be internally consistent */	\
   assert (trans->splits);					\
								\
   if (force_double_entry) {					\
     assert (trans->splits[0]);					\
     assert (trans->splits[1]);					\
   }								\
								\
   if (0x0 != trans->splits[0]) {				\
      char * tmp = strdup (value);				\
      free (trans->splits[0]->field);				\
      trans->splits[0]->field = tmp;				\
      MARK_SPLIT (trans->splits[0]);				\
 								\
      /* if there are just two splits, then keep them in sync. */\
      if (0x0 != trans->splits[1]) {				\
         if (0x0 == trans->splits[2]) {				\
            free (trans->splits[1]->field);			\
            trans->splits[1]->field = strdup (tmp);		\
            MARK_SPLIT (trans->splits[1]);			\
         }							\
      }								\
   }								\
}

void
xaccTransSetMemo (Transaction *trans, const char *mimeo)
{
   SET_TRANS_FIELD (trans, memo, mimeo);
}

void
xaccTransSetAction (Transaction *trans, const char *actn)
{
   SET_TRANS_FIELD (trans, action, actn);
}

/********************************************************************\
\********************************************************************/

Split *
xaccTransGetSplit (Transaction *trans, int i) 
{
   if (trans->splits) {
      return (trans->splits[i]);
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
   char buf [MAX_DATE_LENGTH];
   printDate(buf, trans->date.day, trans->date.month, trans->date.year%100);
   return strdup (buf);
}

int 
xaccTransCountSplits (Transaction *trans)
{
   return (xaccCountSplits (trans->splits));
}

/********************************************************************\
\********************************************************************/

void
xaccSplitSetMemo (Split *split, const char *memo)
{
   char * tmp = strdup (memo);
   if (split->memo) free (split->memo);
   split->memo = tmp;
   MARK_SPLIT (split);
}

void
xaccSplitSetAction (Split *split, const char *actn)
{
   char * tmp = strdup (actn);
   if (split->action) free (split->action);
   split->action = tmp;
   MARK_SPLIT (split);
}

void
xaccSplitSetReconcile (Split *split, char recn)
{
   split->reconciled = recn;
   MARK_SPLIT (split);
   xaccAccountRecomputeBalance (split->acc);
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
xaccSplitGetShareAmount (Split * split)
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
