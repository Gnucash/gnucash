/********************************************************************\
 * Transaction.c -- the transaction data structure                  *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1997, 1998, 1999, 2000 Linas Vepstas               *
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
#include <sys/time.h>
#include <time.h>
#include <unistd.h>

#include "config.h"

#include "Account.h"
#include "AccountP.h"
#include "Group.h"
#include "Transaction.h"
#include "TransactionP.h"
#include "TransLog.h"
#include "util.h"
#include "date.h"


/* 
 * The "force_double_entry" flag determines how 
 * the splits in a transaction will be balanced. 
 *
 * The following values have significance:
 * 0 -- anything goes
 * 1 -- The sum of all splits in a transaction will be
 *      forced to be zero, even if this requires the
 *      creation of additional splits.  Note that a split
 *      whose value is zero (e.g. a stock price) can exist
 *      by itself. Otherwise, all splits must come in at 
 *      least pairs.
 * 2 -- splits without parents will be forced into a
 *      lost & found account.  (Not implemented)
 */
int force_double_entry = 0;

/* bit-field flags for controlling transaction commits */
#define BEGIN_EDIT 0x1
#define DEFER_REBALANCE 0x2

/********************************************************************\
 * Because I can't use C++ for this project, doesn't mean that I    *
 * can't pretend too!  These functions perform actions on the       *
 * Transaction data structure, in order to encapsulate the          *
 * knowledge of the internals of the Transaction in one file.       *
\********************************************************************/

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_ENGINE;

/********************************************************************\
 * xaccInitSplit
 * Initialize a splitaction structure
\********************************************************************/

void
xaccInitSplit(Split * split)
{
  /* fill in some sane defaults */
  split->acc         = NULL;
  split->parent      = NULL;
  
  split->action      = strdup("");
  split->memo        = strdup("");
  split->docref      = strdup("");
  split->reconciled  = NREC;
  split->damount     = 0.0;
  split->share_price = 1.0;

  split->date_reconciled.tv_sec  = 0;
  split->date_reconciled.tv_nsec = 0;

  split->balance             = 0.0;
  split->cleared_balance     = 0.0;
  split->reconciled_balance  = 0.0;
  split->share_balance             = 0.0;
  split->share_cleared_balance     = 0.0;
  split->share_reconciled_balance  = 0.0;
  split->cost_basis                = 0.0;

  split->tickee = 0;
}

/********************************************************************\
\********************************************************************/

Split *
xaccMallocSplit(void)
{
  Split *split = (Split *)_malloc(sizeof(Split));
  xaccInitSplit (split);
  return split;
}

/********************************************************************\
\********************************************************************/
/* This routine is not exposed externally, since it does weird things, 
 * like not really setting up the parent account correctly, and ditto 
 * the parent transaction.  This routine is prone to programmer error
 * if not used correctly.  It is used only by the edit-rollback code.
 */

static Split *
xaccCloneSplit (Split *s)
{
  Split *split = (Split *)_malloc(sizeof(Split));

  split->acc         = s ->acc;
  split->parent      = s->parent;
  
  split->action      = strdup(s->action);
  split->memo        = strdup(s->memo);
  split->docref      = strdup(s->docref);
  split->reconciled  = s->reconciled;
  split->damount     = s->damount;
  split->share_price = s->share_price;

  split->date_reconciled.tv_sec  = s->date_reconciled.tv_sec;
  split->date_reconciled.tv_nsec = s->date_reconciled.tv_nsec;

  /* no need to futz with the balances;  these get wiped each time ... 
   * split->balance             = s->balance;
   * split->cleared_balance     = s->cleared_balance;
   * split->reconciled_balance  = s->reconciled_balance;
   * split->share_balance             = s->share_balance;
   * split->share_cleared_balance     = s->share_cleared_balance;
   * split->share_reconciled_balance  = s->share_reconciled_balance;
   */

  return (split);
}

/********************************************************************\
\********************************************************************/

void
xaccFreeSplit( Split *split )
{
  if (!split) return;

  if (split->memo) free (split->memo);
  if (split->action) free (split->action);
  if (split->docref) free (split->docref);

  /* just in case someone looks up freed memory ... */
  split->memo        = 0x0;
  split->action      = 0x0;
  split->docref      = 0x0;
  split->reconciled  = NREC;
  split->damount     = 0.0;
  split->share_price = 1.0;
  split->parent      = NULL;
  split->acc         = NULL;

  split->date_reconciled.tv_sec = 0;
  split->date_reconciled.tv_nsec = 0;

  _free(split);
}

/********************************************************************\
\********************************************************************/

void
xaccConfigSetForceDoubleEntry (int force) 
{
   force_double_entry = force;
}

int
xaccConfigGetForceDoubleEntry (void) 
{
   return (force_double_entry);
}

/********************************************************************\
\********************************************************************/

#define MARK_SPLIT(split) {			       \
   Account *acc = (Account *) ((split)->acc);	       \
   if (acc) acc->changed |= ACC_INVALIDATE_ALL;	       \
   if (acc) xaccAccountGroupMarkNotSaved(acc->parent); \
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

double xaccSplitGetCostBasis (Split *s) 
{
   if (!s) return 0.0;
   xaccAccountRecomputeCostBasis (s->acc);
   return s->cost_basis;
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
  trans->docref      = strdup("");

  trans->splits    = (Split **) _malloc (3* sizeof (Split *));

  /* create a single split only.  As soon as the balance becomes
   * non-zero, additional splits will get created. 
   */
  split = xaccMallocSplit ();
  split->parent = trans;
  trans->splits[0] = split;
  trans->splits[1] = NULL;

  trans->date_entered.tv_sec  = 0;
  trans->date_entered.tv_nsec = 0;

  trans->date_posted.tv_sec  = 0;
  trans->date_posted.tv_nsec = 0;

  trans->marker = 0;
  trans->open = 0;
  trans->orig = NULL;
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
/* This routine is not exposed externally, since it does weird things, 
 * like not really owning the splits correctly, and other weirdnesses. 
 * This routine is prone to programmer snafu if not used correctly. 
 * It is used only by the edit-rollback code.
 */

static Transaction *
xaccCloneTransaction (Transaction *t)
{
  Transaction *trans;
  int n;

  trans = (Transaction *)_malloc(sizeof(Transaction));

  trans->num         = strdup(t->num);
  trans->description = strdup(t->description);
  trans->docref      = strdup(t->docref);

  n=0; while (t->splits[n]) n++;
  trans->splits    = (Split **) _malloc ((n+1)* sizeof (Split *));

  n=0; 
  while (t->splits[n]) {
    trans->splits[n] = xaccCloneSplit (t->splits[n]);
    n++;
  }
  trans->splits[n] = NULL;

  trans->date_entered.tv_sec  = t->date_entered.tv_sec;
  trans->date_entered.tv_nsec = t->date_entered.tv_nsec;

  trans->date_posted.tv_sec  = t->date_posted.tv_sec;
  trans->date_posted.tv_nsec = t->date_posted.tv_nsec;

  trans->open = 0;
  trans->orig = NULL;

  return (trans);
}


/********************************************************************\
\********************************************************************/

void
xaccFreeTransaction( Transaction *trans )
{
  int i;
  Split *s;

  if (!trans) return;
  ENTER ("xaccFreeTransaction(): addr=%p\n", trans);

  /* free up the destination splits */
  if (trans->splits) {
    i = 0;
    s = trans->splits[i];
    while (s) {
      xaccFreeSplit (s);
      i++;
      s = trans->splits[i];
    }
  }

  _free (trans->splits);

  /* free up transaction strings */
  if (trans->num) free (trans->num);
  if (trans->description) free (trans->description);
  if (trans->docref) free (trans->docref);

  /* just in case someone looks up freed memory ... */
  trans->num         = 0x0;
  trans->description = 0x0;
  trans->docref      = 0x0;

  trans->date_entered.tv_sec = 0;
  trans->date_entered.tv_nsec = 0;

  trans->date_posted.tv_sec = 0;
  trans->date_posted.tv_nsec = 0;

  trans->open = 0;

  if (trans->orig) {
    xaccFreeTransaction (trans->orig);
    trans->orig = NULL;
  }

  _free(trans);
  LEAVE ("xaccFreeTransaction(): addr=%p\n", trans);
}

/********************************************************************\
\********************************************************************/

void
xaccSplitSetBaseValue (Split *s, double value, char * base_currency)
{
   if (!s) return;

   /* Novice/casual users may not want or use the double entry 
    * features of this engine. So, in particular, there
    * may be the occasional split without a parent account. 
    * Well, that's ok, we'll just go with the flow. 
    */
   if (!(s->acc)) {
      if (force_double_entry) {
         assert (s->acc);
      } else { 
         s -> damount = (value / (s->share_price));   
         return;
      }
   }

   /* be more precise -- the value depends on the currency 
    * we want it expressed in.
    */
   if (!safe_strcmp(s->acc->currency, base_currency)) {
      s -> damount = (value / (s->share_price));   
   } else 
   if (!safe_strcmp(s->acc->security, base_currency)) {
      s -> damount = value;   
   } else 
   if ((0x0==base_currency) && (0 == force_double_entry)) {
      s -> damount = (value / (s->share_price));   
   } else 
   {
      PERR ("xaccSplitSetBaseValue(): "
            " inappropriate base currency %s "
            " given split currency=%s and security=%s\n",
              base_currency, s->acc->currency, s->acc->security);
      return;
   }
}


double
xaccSplitGetBaseValue (Split *s, char * base_currency)
{
   double value;
   if (!s) return 0.0;

   /* ahh -- users may not want or use the double entry 
    * features of this engine.  So, in particular, there
    * may be the occasional split without a parent account. 
    * Well, that's ok, we'll just go with the flow. 
    */
   if (!(s->acc)) {
      if (force_double_entry) {
         assert (s->acc);
      } else { 
         value = s->damount * s->share_price;   
         return value;
      }
   }

   /* be more precise -- the value depends on the curency 
    * we want it expressed in.
    */
   if (!safe_strcmp(s->acc->currency, base_currency)) {
      value = s->damount * s->share_price;   
   } else 
   if (!safe_strcmp(s->acc->security, base_currency)) {
      value = s->damount;   
   } else 
   if ((0x0==base_currency) && (0 == force_double_entry)) {
      value = s->damount * s->share_price;   
   } else 
   {
      PERR ("xaccSplitGetBaseValue(): "
            " inappropriate base currency %s "
            " given split currency=%s and security=%s\n",
              base_currency, s->acc->currency, s->acc->security);
      return 0.0;
   }
   return value;
}

/********************************************************************\
\********************************************************************/

static double
ComputeValue (Split **sarray, Split * skip_me, char * base_currency)
{
   Split *s;
   int i=0;
   double value = 0.0;

   s = sarray[0];
   while (s) {
      if (s != skip_me) {
         /* ahh -- users may not want or use the double entry 
          * features of this engine.  So, in particular, there
          * may be the occasional split without a parent account. 
          * Well, that's ok, we'll just go with the flow. 
          */
         if (!(s->acc)) {
            if (force_double_entry) {
               assert (s->acc);
            } else { 
               value += s->damount * s->share_price;   
            }
         } else 
         if ((0x0 == base_currency) && (0 == force_double_entry)) {
            value += s->damount * s->share_price;   
         } else {

            /* OK, we've got a parent account, we've got currency, 
             * lets behave like professionals now, instead of the
             * shenanigans above.
             */
            if (!safe_strcmp(s->acc->currency, base_currency)) {
               value += s->share_price * s->damount;
            } else 
            if (!safe_strcmp(s->acc->security, base_currency)) {
               value += s->damount;
            } else {
               PERR ("Internal Error: ComputeValue(): "
                     " inconsistent currencies \n");
               assert (0);
            }
         }
      }
      i++; s = sarray [i];
   }

   return value;
}

double
xaccTransGetImbalance (Transaction * trans)
{
  char * currency = xaccTransFindCommonCurrency (trans);
  double imbal = ComputeValue (trans->splits, NULL, currency);
  return imbal;
}

/********************************************************************\
\********************************************************************/
gncBoolean xaccIsCommonCurrency(char *currency_1, char *security_1,
				char *currency_2, char *security_2)
{
  int c1c2, c1s2, s1c2, s1s2;

  if ((currency_1 == NULL) || (currency_2 == NULL))
    return GNC_F;

  if ((security_1 != NULL) && (security_1[0] == 0x0))
    security_1 = NULL;

  if ((security_2 != NULL) && (security_2[0] == 0x0))
    security_2 = NULL;

  c1c2 = safe_strcmp(currency_1, currency_2);
  c1s2 = safe_strcmp(currency_1, security_2);

  if (security_1 != NULL)
  {
    s1c2 = safe_strcmp(security_1, currency_2);
    s1s2 = safe_strcmp(security_1, security_2);
  }
  else /* no match */
  {
    s1c2 = 1;
    s1s2 = 1;
  }

  return (c1c2 == 0) || (c1s2 == 0) || (s1c2 == 0) || (s1s2 == 0);
}

static char *
FindCommonCurrency (Split **slist, char * ra, char * rb)
{
  Split *s;
  int i = 0;

  if (!slist) return NULL;

  if (rb && (0x0==rb[0])) rb = 0x0;

  i=0; s = slist[0];
  while (s) {
    char *sa, *sb;

    /* Novice/casual users may not want or use the double entry 
     * features of this engine.   Because of this, there
     * may be the occasional split without a parent account. 
     * Well, that's ok,  we'll just go with the flow. 
     */
    if (force_double_entry) {
       assert (s->acc);
    } else
    if (NULL == s->acc) {
       i++; s=slist[i]; continue;
    }

    sa = s->acc->currency;
    sb = s->acc->security;
    if (sb && (0x0==sb[0])) sb = 0x0;

    if (ra && rb) {
       int aa = safe_strcmp (ra,sa);
       int ab = safe_strcmp (ra,sb);
       int ba = safe_strcmp (rb,sa);
       int bb = safe_strcmp (rb,sb);
       if ( (!aa) && bb) rb = 0x0;
       else
       if ( (!ab) && ba) rb = 0x0;
       else
       if ( (!ba) && ab) ra = 0x0;
       else
       if ( (!bb) && aa) ra = 0x0;
       else
       if ( aa && bb && ab && ba ) { ra=0x0; rb=0x0; }

       if (!ra) { ra=rb; rb=0x0; }
    } 
    else
    if (ra && !rb) {
       int aa = safe_strcmp (ra,sa);
       int ab = safe_strcmp (ra,sb);
       if ( aa && ab )  ra= 0x0;
    }

    if ((!ra) && (!rb)) return NULL;
    i++; s = slist[i];
  }

  return (ra);
}


char *
xaccTransFindCommonCurrency (Transaction *trans)
{
  char *ra, *rb, *com;

  assert (trans->splits);
  assert (trans->splits[0]);
  assert (trans->splits[0]->acc);

  ra = trans->splits[0]->acc->currency;
  rb = trans->splits[0]->acc->security;

  com = FindCommonCurrency (trans->splits, ra, rb);
  return com;
}

char *
xaccTransIsCommonCurrency (Transaction *trans, char * ra)
{
  char *com;
  com = FindCommonCurrency (trans->splits, ra, NULL);
  return com;
}

/********************************************************************\
\********************************************************************/

/* hack alert -- the algorithm used in this rebalance routine
 * is less than intuitive, and could use some write-up.  
 * Maybe it does indeed do the right thing, but that is
 * not at all obvious.
 */

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
  char *base_currency=0x0;

  trans = split->parent;

  /* We might have gotten here if someone is manipulating
   * a split that has not yet been inserted in a transaction.
   * Rather than punishing them with an assert, lets just
   * quietly return. 
   */
  if (!trans) return;

  if (DEFER_REBALANCE & (trans->open)) return;
  if (split->acc) {
    char *ra, *rb;
    if (ACC_DEFER_REBALANCE & (split->acc->open)) return;
    assert (trans->splits);
    assert (trans->splits[0]);
  
    /* lets find out if we are dealing with multiple currencies,
     * and which one(s) all of the splits have in common.  */
    ra = split->acc->currency;
    rb = split->acc->security;
    base_currency = FindCommonCurrency (trans->splits, ra, rb);

    if (!base_currency) {
      PERR ("Internal Error: SplitRebalance(): no common split currencies \n");
      s = trans->splits[0];
      while (s) {
        if (s->acc) {
          PERR ("\taccount=%s currency=%s security=%s\n",
                s->acc->accountName, s->acc->currency, s->acc->security);
        } else {
          PERR ("\t*** No parent account *** \n");
        }
        i++; s = trans->splits[i];
      }
      assert (0);
      return;
    }
  } else {
    assert (trans->splits);
    assert (trans->splits[0]);
  }

  if (split == trans->splits[0]) {
    /* The indicated split is the source split.
     * Pick a destination split (by default, 
     * the first destination split), and force 
     * the total on it. 
     */

    s = trans->splits[1];
    if (s) {
      /* the new value of the destination split will be the result.  */
      value = ComputeValue (trans->splits, s, base_currency);
      xaccSplitSetBaseValue (s, -value, base_currency);
      MARK_SPLIT (s);
      xaccAccountRecomputeBalance (s->acc); 

    } else {
      /* There are no destination splits !! 
       * Either this is allowed, in which case 
       * we just blow it off, or its forbidden,
       * in which case we force a balancing split 
       * to be created.
       *
       * Note that its ok to have a single split whose amount is zero.
       * this is just a split that is recording a price, and nothing
       * else.  (i.e. it still obeys the rule that the sum of the 
       * value of all the splits is zero).
       */

       if (force_double_entry) {
          if (! (DEQ (0.0, split->damount))) {
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
       }
    }
  } else {

    /* The indicated split is a destination split.
     * Compute grand total of all destination splits,
     * and force the source split to balance.
     */
    s = trans->splits[0];
    value = ComputeValue (trans->splits, s, base_currency);
    xaccSplitSetBaseValue (s, -value, base_currency);
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

#define CHECK_OPEN(trans) {					\
   if (!trans->open) {						\
      PERR ("transaction %p not open for editing\n", trans);	\
      /* assert (trans->open); */				\
      PERR ("\t%s:%d \n", __FILE__, __LINE__);			\
      /* return; */						\
   }								\
}

void
xaccTransBeginEdit (Transaction *trans, int defer)
{
   char open;

   assert (trans);
   open = trans->open;
   trans->open = BEGIN_EDIT;
   if (defer) trans->open |= DEFER_REBALANCE;
   if (open & BEGIN_EDIT) return;

   xaccOpenLog ();
   xaccTransWriteLog (trans, 'B');

   /* make a clone of the transaction; we will use this 
    * in case we need to roll-back the edit. 
    */
   trans->orig = xaccCloneTransaction (trans);
}

void
xaccTransCommitEdit (Transaction *trans)
{
   int i;
   Split *split;
   Account *acc;

   if (!trans) return;
   ENTER ("xaccTransCommitEdit(): trans addr=%p\n", trans);
   CHECK_OPEN (trans);

   /* At this point, we check to see if we have a valid transaction.
    * As a result of editing, we could end up with a transaction that
    * has no splits in it, in which case we delete the transaction and
    * return.  
    */
   split = trans->splits[0];
   if (!split)
   {
      PINFO ("xaccTransCommitEdit(): delete trans at addr=%p\n", trans);
      /* Make a log in the journal before destruction.  */
      xaccTransWriteLog (trans, 'D');
      xaccFreeTransaction (trans);
      return;
   }

   /* try to get the sorting order lined up according to 
    * when the user typed things in.  */
   if (0 == trans->date_entered.tv_sec) {
      struct timeval tv;
      gettimeofday (&tv, NULL);
      trans->date_entered.tv_sec = tv.tv_sec;
      trans->date_entered.tv_nsec = 1000 * tv.tv_usec;
   }

   /* Alternately the transaction may have only one split in 
    * it, in which case ... that's OK if and only if the split has no 
    * value (i.e. is only recording a price).  Otherwise, a single
    * split with a value can't possibly balance, thus violating the 
    * rules of double-entry, and that's way bogus. So create 
    * a matching opposite and place it either here (if force==1), 
    * or in some dummy account (if force==2).
    */
   if ((1 == force_double_entry) &&
       (NULL == trans->splits[1]) && (!(DEQ(0.0, split->damount))))
   {
      Split * s = xaccMallocSplit();
      xaccSplitSetMemo  (s, split->memo);
      xaccSplitSetAction (s, split->action);
      s->damount     = -(split->damount);
      s->share_price = split->share_price;

      xaccTransAppendSplit (trans, s);
      s->acc = NULL;
      xaccAccountInsertSplit (split->acc, s);
   }

   trans->open &= ~DEFER_REBALANCE;
   xaccTransRebalance (trans);

   /* Make sure all associated splits are in proper order
    * in their accounts. */
   i=0;
   split = trans->splits[i];
   while (split) {
      acc = split ->acc;
      xaccCheckDateOrder(acc, trans->splits[i]);
      i++;
      split = trans->splits[i];
   }

   /* Recompute the account balances. */
   i=0;
   split = trans->splits[i];
   while (split) {
      acc = split->acc;
      xaccAccountRecomputeBalance (acc); 
      i++;
      split = trans->splits[i];
   }

   trans->open = 0;
   xaccTransWriteLog (trans, 'C');

   /* get rid of the copy we made. We won't be rolling back, 
    * so we don't need it any more.  */
   xaccFreeTransaction (trans->orig);
   trans->orig = NULL;

   LEAVE ("xaccTransCommitEdit(): trans addr=%p\n", trans);
}

void
xaccTransRollbackEdit (Transaction *trans)
{
   Transaction *orig;
   Split *s, *so;
   Account * acc;
   int force_it=0, mismatch=0, i;

   if (!trans) return;
   CHECK_OPEN (trans);
   ENTER ("xaccTransRollbackEdit(): trans addr=%p\n", trans);

   /* copy the original values back in. */
   orig = trans->orig;

#define PUT_BACK(val) { free(trans->val); trans->val=orig->val; orig->val=0x0; }
   PUT_BACK (num);
   PUT_BACK (description);
   PUT_BACK (docref);

   trans->date_entered.tv_sec  = orig->date_entered.tv_sec;
   trans->date_entered.tv_nsec = orig->date_entered.tv_nsec;

   trans->date_posted.tv_sec  = orig->date_posted.tv_sec;
   trans->date_posted.tv_nsec = orig->date_posted.tv_nsec;

   /* OK, we also have to restore the state of the splits.  Of course,
    * we could brute-force our way through this, and just clobber all of the
    * old splits, and insert all of the new splits, but this kind of brute
    * forcing will suck memory cycles.  So instead we'll try the gentle 
    * approach first.  Note that even in the gentle approach, the 
    * CheckDateOrder routine could be cpu-cyle brutal, so it maybe 
    * it could use some tuning ...
    */
   i=0; 
   s = trans->splits[0];
   so = orig->splits[0];
   while (s && so) {
      if (so->acc != s->acc) { force_it = 1;  mismatch=i; break; }

#define HONKY_CAT(val) { free(s->val); s->val=so->val; so->val=0x0; }
      HONKY_CAT (action);
      HONKY_CAT (memo);
      HONKY_CAT (docref);

      s->reconciled  = so->reconciled;
      s->damount     = so->damount;
      s->share_price = so->share_price;

      s->date_reconciled.tv_sec  = so->date_reconciled.tv_sec;
      s->date_reconciled.tv_nsec = so->date_reconciled.tv_nsec;

      /* do NOT check date order until all of teh other fields 
       * have beenproperly restored */
      xaccCheckDateOrder (s->acc, s); 
      MARK_SPLIT (s);
      xaccAccountRecomputeBalance (s->acc);
      i++;
      s = trans->splits[i];
      so = orig->splits[i];
   }
   if (so != s) { force_it = 1; mismatch=i; }

   /* OK, if force_it got set, we'll have to tough it out and brute-force
    * the rest of the way.  Clobber all the edited splits, add all new splits.
    * Unfortunately, this can suck up CPU cycles in the Remove/Insert routines.
    */  
   if (force_it) {
      i=0; s = trans->splits[i];
      while (s && (i<mismatch)) {
         xaccFreeSplit (orig->splits[i]);
         orig->splits[i] = s;
         i++;
         s =  trans->splits[i];
      }
      i=mismatch; s = trans->splits[i];
      while (s) {
         acc = s->acc;
         MARK_SPLIT (s);
         xaccAccountRemoveSplit (acc, s);
         xaccAccountRecomputeBalance (acc);
         xaccFreeSplit (s);
         i++;
         s =  trans->splits[i];
      }
      _free (trans->splits);
   
      trans->splits = orig->splits;
      orig->splits = NULL;
   
      i=mismatch; s = trans->splits[i];
      while (s) {
         acc = s->acc;
         MARK_SPLIT (s);
         xaccAccountInsertSplit (acc, s);
         xaccAccountRecomputeBalance (acc);
         i++;
         s =  trans->splits[i];
      }
   }

   xaccTransWriteLog (trans, 'R');

   xaccFreeTransaction (trans->orig);
   trans->orig = NULL;
   trans->open = 0;
   LEAVE ("xaccTransRollbackEdit(): trans addr=%p\n", trans);
}

gncBoolean
xaccTransIsOpen (Transaction *trans)
{
  if (trans == NULL) return GNC_F;

  return ((trans->open & BEGIN_EDIT) != 0);
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
   CHECK_OPEN (trans);
   xaccTransWriteLog (trans, 'D');

   i=0;
   split = trans->splits[i];
   while (split) {
      MARK_SPLIT (split);
      acc = split ->acc;
      xaccAccountRemoveSplit (acc, split);
      xaccAccountRecomputeBalance (acc); 
      xaccFreeSplit (split);
      trans->splits[i] = NULL;
      i++;
      split = trans->splits[i];
   }

   /* the actual free is done with the commit call, else its rolled back */
   /* xaccFreeTransaction (trans);  don't do this here ... */
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
   CHECK_OPEN (trans);

   numsplits = 0;
   s = trans->splits[0];
   while (s) {
      MARK_SPLIT(s);
      if (s == split) ismember = 1;
      numsplits ++;
      s = trans->splits[numsplits];
   }
   assert (ismember);

   /* If the account has three or more splits, 
    * merely unlink & free the split. 
    *
    * Or if the account has only two splits, 
    * then this destroy will leave only one split.
    * Don't rebalance, as this will goof up the
    * value of teh remaining split.
    */
   MARK_SPLIT (split);
   xaccTransRemoveSplit (trans, split);
   acc = split->acc;
   xaccAccountRemoveSplit (acc, split);
   xaccAccountRecomputeBalance (acc);
   xaccFreeSplit (split);

   if (2 < numsplits) {
      xaccSplitRebalance (trans->splits[0]);
   }
}

/********************************************************************\
\********************************************************************/

void
xaccTransAppendSplit (Transaction *trans, Split *split) 
{
   int i, num;
   Split **oldarray;
   Transaction *oldtrans;

   if (!trans) return;
   if (!split) return;

   CHECK_OPEN (trans);

   /* first, make sure that the split isn't already inserted 
    * elsewhere. If so, then remove it. */
   oldtrans = split->parent;
   if (oldtrans) {
      xaccTransRemoveSplit (oldtrans, split);
      xaccTransRebalance (oldtrans);
   }
   
   /* now, insert the split into the array */
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
 * TransRemoveSplit is an engine private function and does not/should
 * not cause any rebalancing to occur.
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
 * are compared.  If the num fields are identical, then the description
 * fields are compared.  If these are identical, then the memo fields 
 * are compared.  Hopefully, there will not be any transactions that
 * occur on the same day that have all three of these values identical.
 *
 * Note that being able to establish this kind of absolute order is 
 * important for some of the ledger display functions.  In particular,
 * grep for "running_balance" in the code, and see the notes there.
 *
 * Yes, this kind of code dependency is ugly, but the alternatives seem
 * ugly too.
 *
\********************************************************************/


#define DATE_CMP(aaa,bbb,field) {			\
  /* if dates differ, return */				\
  if ( ((*aaa)->field.tv_sec) <				\
       ((*bbb)->field.tv_sec)) {			\
    return -1;						\
  } else						\
  if ( ((*aaa)->field.tv_sec) >				\
       ((*bbb)->field.tv_sec)) {			\
    return +1;						\
  }							\
							\
  /* else, seconds match. check nanoseconds */		\
  if ( ((*aaa)->field.tv_nsec) <			\
       ((*bbb)->field.tv_nsec)) {			\
    return -1;						\
  } else						\
  if ( ((*aaa)->field.tv_nsec) >			\
       ((*bbb)->field.tv_nsec)) {			\
    return +1;						\
  }							\
}



int
xaccSplitDateOrder (Split **sa, Split **sb)
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
  SAFE_STRCMP (da, db);

  /* otherwise, sort on action strings */
  da = (*sa)->action;
  db = (*sb)->action;
  SAFE_STRCMP (da, db);

  /* otherwise, sort on docref string */
  da = (*sa)->docref;
  db = (*sb)->docref;
  SAFE_STRCMP (da, db);

  /* the reconciled flag ... */
  if (((*sa)->reconciled) < ((*sb)->reconciled)) return -1;
  if (((*sa)->reconciled) > ((*sb)->reconciled)) return +1;

  /* compare amounts */
  if ((((*sa)->damount)+EPS) < ((*sb)->damount)) return -1;
  if ((((*sa)->damount)-EPS) > ((*sb)->damount)) return +1;

  if ((((*sa)->share_price)+EPS) < ((*sb)->share_price)) return -1;
  if ((((*sa)->share_price)-EPS) > ((*sb)->share_price)) return +1;

  /* if dates differ, return */
  DATE_CMP(sa,sb,date_reconciled);

  return 0;
}


int
xaccSplitOrder (Split **sa, Split **sb)
{
  char *da, *db;
  char diff;

  if ( (*sa) && !(*sb) ) return -1;
  if ( !(*sa) && (*sb) ) return +1;
  if ( !(*sa) && !(*sb) ) return 0;

  /* compare amounts use parenthesis paranoia for multiplication, pointers etc. */
  if ( ((((*sa)->damount)*((*sa)->share_price))+EPS) < 
        (((*sb)->damount)*((*sb)->share_price))) return -1;

  if ( ((((*sa)->damount)*((*sa)->share_price))-EPS) > 
        (((*sb)->damount)*((*sb)->share_price))) return +1;

  if ((((*sa)->share_price)+EPS) < ((*sb)->share_price)) return -1;
  if ((((*sa)->share_price)-EPS) > ((*sb)->share_price)) return +1;

  /* otherwise, sort on memo strings */
  da = (*sa)->memo;
  db = (*sb)->memo;
  SAFE_STRCMP (da, db);

  /* otherwise, sort on action strings */
  da = (*sa)->action;
  db = (*sb)->action;
  SAFE_STRCMP (da, db);

  /* the reconciled flag ... */
  diff = ((*sa)->reconciled) - ((*sb)->reconciled);
  if (diff) return diff;

  /* if dates differ, return */
  DATE_CMP(sa,sb,date_reconciled);

  /* otherwise, sort on docref string */
  da = (*sa)->docref;
  db = (*sb)->docref;
  SAFE_STRCMP (da, db);

  return 0;
}

int
xaccSplitMatch (Split **sa, Split **sb)
{
  char *da, *db;
  char diff;

  if ( (*sa) && !(*sb) ) return -1;
  if ( !(*sa) && (*sb) ) return +1;
  if ( !(*sa) && !(*sb) ) return 0;

  /* compare amounts use parenthesis paranoia for multiplication, pointers etc. */
  if ( ((((*sa)->damount)*((*sa)->share_price))+EPS) < 
        (((*sb)->damount)*((*sb)->share_price))) return -1;

  if ( ((((*sa)->damount)*((*sa)->share_price))-EPS) > 
        (((*sb)->damount)*((*sb)->share_price))) return +1;

  if ((((*sa)->share_price)+EPS) < ((*sb)->share_price)) return -1;
  if ((((*sa)->share_price)-EPS) > ((*sb)->share_price)) return +1;

  /* otherwise, sort on memo strings */
  da = (*sa)->memo;
  db = (*sb)->memo;
  SAFE_STRCMP (da, db);

  /* otherwise, sort on action strings */
  da = (*sa)->action;
  db = (*sb)->action;
  SAFE_STRCMP (da, db);

  /* If the reconciled flags are different, don't compare the
   * dates, since we want to match splits with different reconciled
   * values. But if they do match, the dates must match as well. 
   * Note that 
   */
  diff = ((*sa)->reconciled) - ((*sb)->reconciled);
  if (!diff) {
    DATE_CMP(sa,sb,date_reconciled);
  }

  /* otherwise, sort on docref string */
  da = (*sa)->docref;
  db = (*sb)->docref;
  SAFE_STRCMP (da, db);

  return 0;
}

int
xaccTransOrder (Transaction **ta, Transaction **tb)
{
  char *da, *db;

  if ( (*ta) && !(*tb) ) return -1;
  if ( !(*ta) && (*tb) ) return +1;
  if ( !(*ta) && !(*tb) ) return 0;

  /* if dates differ, return */
  DATE_CMP(ta,tb,date_posted);

  /* otherwise, sort on number string */
  da = (*ta)->num;
  db = (*tb)->num;
  SAFE_STRCMP (da, db);

  /* if dates differ, return */
  DATE_CMP(ta,tb,date_entered);

  /* otherwise, sort on description string */
  da = (*ta)->description;
  db = (*tb)->description;
  SAFE_STRCMP (da, db);

  /* otherwise, sort on docref string */
  da = (*ta)->docref;
  db = (*tb)->docref;
  SAFE_STRCMP (da, db);

  return 0;
}

int
xaccTransMatch (Transaction **tap, Transaction **tbp)
{
  int retval;
  Transaction *ta, *tb;
  Split *sa, *sb;
  int na, nb;

  /* first, do the basic comparison */
  retval = xaccTransOrder (tap, tbp);
  if (0 != retval) return retval;
  ta = *tap;
  tb = *tbp;

  /* Now, start comparing splits */
  na=0; while (ta->splits[na]) na++;
  nb=0; while (tb->splits[nb]) nb++;
  if (na-nb) return (na-nb);

  /* Ugh, no we've got to compare individual splits.  They do not necessarily
   * have to be in identical order to match.  So we have to cycle through them,
   * without creating bogus matches.
   */
  na=0; while ((sa=ta->splits[na])) { sa->tickee = -1; na++; }
  nb=0; while ((sb=tb->splits[nb])) { sb->tickee = -1; nb++; }

  na=0; 
  while ((sa=ta->splits[na])) { 
     if (-1 < sa->tickee) {na++; continue;}
    
     nb=0; 
     while ((sb=tb->splits[nb])) { 
        if (-1 < sb->tickee) {nb++; continue;}
        retval = xaccSplitMatch (&sa, &sb);
        if ((0 == retval) && (sa->acc == sb->acc)) {
           sb->tickee = na;
           sa->tickee = nb;
           break;
        }
        nb++;
     }

     if (-1 == sa->tickee) return -1;
     na++;
  }

  nb=0; 
  while ((sb=tb->splits[nb])) { 
     if (-1 == sb->tickee) return +1;
     nb++;
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
xaccTransSetDateSecs (Transaction *trans, time_t secs)
{
   if (!trans) return;
   CHECK_OPEN (trans);
   PINFO ("xaccTransSetDateSecs(): addr=%p set date to %lu %s \n",
           trans, secs, ctime (&secs));

   trans->date_posted.tv_sec = secs;
   trans->date_posted.tv_nsec = 0;

   /* Because the date has changed, we need to make sure that each of the
    * splits is properly ordered in each of thier accounts.  We could do that
    * here, simply by reinserting each split into its account.  However, in
    * some ways this is bad behaviour, and it seems much better/nicer to defer
    * that until the commit phase, i.e. until the user has called the
    * xaccTransCommitEdit() routine.  So, for now, we are done.
    */

}

void
xaccTransSetDateSecsL (Transaction *trans, long long secs)
{
   if (!trans) return;
   CHECK_OPEN (trans);
   DEBUGCMD ({ 
      time_t sicko = secs;
      PINFO ("xaccTransSetDateSecsL(): addr=%p set date to %Lu %s \n",
              trans, secs, ctime (&sicko));
   })

   trans->date_posted.tv_sec = secs;
   trans->date_posted.tv_nsec = 0;
}

void
xaccTransSetDateEnteredSecs (Transaction *trans, time_t secs)
{
   if (!trans) return;
   CHECK_OPEN (trans);

   trans->date_entered.tv_sec = secs;
   trans->date_entered.tv_nsec = 0;
}

void
xaccTransSetDateTS (Transaction *trans, const Timespec *ts)
{
   if (!trans) return;
   CHECK_OPEN (trans);
   DEBUGCMD ({
         time_t sicko = ts->tv_sec;
         PINFO ("xaccTransSetDateTS(): addr=%p set date to %Lu %s \n",
                trans, ts->tv_sec, ctime (&sicko));
    })

   trans->date_posted.tv_sec = ts->tv_sec;
   trans->date_posted.tv_nsec = ts->tv_nsec;
}

void
xaccTransSetDateEnteredTS (Transaction *trans, const Timespec *ts)
{
   if (!trans) return;
   CHECK_OPEN (trans);

   trans->date_entered.tv_sec = ts->tv_sec;
   trans->date_entered.tv_nsec = ts->tv_nsec;
}

#define THIRTY_TWO_YEARS 0x3c30fc00LL

Timespec
gnc_dmy2timespec(int day, int month, int year) {
  Timespec result;
  struct tm date;
  long long secs = 0;
  long long era = 0;
  
  year -= 1900;
  
  /* make a crude attempt to deal with dates outside the 
   * range of Dec 1901 to Jan 2038. Note the we screw up 
   * centenial leap years here ... so hack alert --
   */
  if ((2 > year) || (136 < year)) 
  {
    era = year / 32;
    year %= 32;
    if (0 > year) { year += 32; era -= 1; } 
  }
  
  date.tm_year = year;
  date.tm_mon = month - 1;
  date.tm_mday = day;
  date.tm_hour = 11;
  date.tm_min = 0;
  date.tm_sec = 0;
  
  /* compute number of seconds */
  secs = mktime (&date);
  
  secs += era * THIRTY_TWO_YEARS;
  
  result.tv_sec = secs;
  result.tv_nsec = 0;
  
  return(result);
}


void
xaccTransSetDate (Transaction *trans, int day, int mon, int year) {
  Timespec ts = gnc_dmy2timespec(day, mon, year);
  xaccTransSetDateTS (trans, &ts);
}

void
xaccTransSetDateToday (Transaction *trans)
{
   struct timeval tv;

   if (!trans) return;
   CHECK_OPEN (trans);

   gettimeofday (&tv, NULL);
   trans->date_posted.tv_sec = tv.tv_sec;
   trans->date_posted.tv_nsec = 1000 * tv.tv_usec;

   PINFO ("xaccTransSetDateToday(): addr=%p set date to %lu %s \n",
         trans, tv.tv_sec, ctime ((time_t *)&tv.tv_sec));

}


/********************************************************************\
\********************************************************************/

void
xaccTransSetNum (Transaction *trans, const char *xnum)
{
   char * tmp;
   if (!trans || !xnum) return;
   CHECK_OPEN (trans);

   tmp = strdup (xnum);
   if (trans->num) free (trans->num);
   trans->num = tmp;
   MarkChanged (trans);
}

void
xaccTransSetDescription (Transaction *trans, const char *desc)
{
   char * tmp;
   if (!trans || !desc) return;
   CHECK_OPEN (trans);

   tmp = strdup (desc);
   if (trans->description) free (trans->description);
   trans->description = tmp;
   MarkChanged (trans);
}

void
xaccTransSetDocref (Transaction *trans, const char *docs)
{
   char * tmp;
   if (!trans || !docs) return;
   CHECK_OPEN (trans);

   tmp = strdup (docs);
   if (trans->docref) free (trans->docref);
   trans->docref = tmp;
   MarkChanged (trans);
}

#define SET_TRANS_FIELD(trans,field,value)			\
{								\
   char * tmp;							\
   if (!trans) return;						\
   CHECK_OPEN (trans);						\
								\
   /* the engine *must* always be internally consistent */	\
   assert (trans->splits);					\
   assert (trans->splits[0]);					\
								\
   /* there must be two splits if value of one non-zero */	\
   if (force_double_entry) {					\
     if (! (DEQ (0.0, trans->splits[0]->damount))) {		\
        assert (trans->splits[1]);				\
     }								\
   }								\
								\
   tmp = strdup (value);					\
   free (trans->splits[0]->field);				\
   trans->splits[0]->field = tmp;				\
   MARK_SPLIT (trans->splits[0]);				\
								\
   /* If there are just two splits, then keep them in sync. */	\
   if (0x0 != trans->splits[1]) {				\
      if (0x0 == trans->splits[2]) {				\
         free (trans->splits[1]->field);			\
         trans->splits[1]->field = strdup (tmp);		\
         MARK_SPLIT (trans->splits[1]);				\
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
   if (!trans) return NULL;
   if (0 > i) return NULL;
   /* hack alert - should check if i > sizeof array */
   if (trans->splits) {
      return (trans->splits[i]);
   }
   return NULL;
}

char *
xaccTransGetNum (Transaction *trans)
{
   if (!trans) return NULL;
   return (trans->num);
}

char * 
xaccTransGetDescription (Transaction *trans)
{
   if (!trans) return NULL;
   return (trans->description);
}

char * 
xaccTransGetDocref (Transaction *trans)
{
   if (!trans) return NULL;
   return (trans->docref);
}

time_t
xaccTransGetDate (Transaction *trans)
{
   if (!trans) return 0;
   return (trans->date_posted.tv_sec);
}

long long 
xaccTransGetDateL (Transaction *trans)
{
   if (!trans) return 0;
   return (trans->date_posted.tv_sec);
}

void
xaccTransGetDateTS (Transaction *trans, Timespec *ts)
{
   if (!trans || !ts) return;
   *ts = (trans->date_posted);
}

void
xaccTransGetDateEnteredTS (Transaction *trans, Timespec *ts)
{
   if (!trans || !ts) return;
   *ts = (trans->date_entered);
}

int 
xaccTransCountSplits (Transaction *trans)
{
   if (!trans) return 0;
   return (xaccCountSplits (trans->splits));
}

/********************************************************************\
\********************************************************************/

void
xaccSplitSetMemo (Split *split, const char *memo)
{
   char * tmp;
   if (!split || !memo) return;
   tmp = strdup (memo);
   if (split->memo) free (split->memo);
   split->memo = tmp;
   MARK_SPLIT (split);
}

void
xaccSplitSetAction (Split *split, const char *actn)
{
   char * tmp;
   if (!split || !actn) return;
   tmp = strdup (actn);
   if (split->action) free (split->action);
   split->action = tmp;
   MARK_SPLIT (split);
}

void
xaccSplitSetDocref (Split *split, const char *docs)
{
   char * tmp;
   if (!split || !docs) return;
   tmp = strdup (docs);
   if (split->docref) free (split->docref);
   split->docref = tmp;
   MARK_SPLIT (split);
}

void
xaccSplitSetReconcile (Split *split, char recn)
{
   struct timeval tv;

   if (!split) return;
   split->reconciled = recn;
   MARK_SPLIT (split);

   gettimeofday (&tv, NULL);
   split->date_reconciled.tv_sec = tv.tv_sec;
   split->date_reconciled.tv_nsec = 1000 * tv.tv_usec;

   xaccAccountRecomputeBalance (split->acc);
}

void
xaccSplitSetDateReconciledSecs (Split *split, time_t secs)
{
   if (!split) return;
   MARK_SPLIT (split);

   split->date_reconciled.tv_sec = secs;
   split->date_reconciled.tv_nsec = 0;
}

void
xaccSplitSetDateReconciledTS (Split *split, Timespec *ts)
{
   if (!split) return;
   MARK_SPLIT (split);

   split->date_reconciled =  *ts;
}

void
xaccSplitGetDateReconciledTS (Split * split, Timespec *ts)
{
   if (!split || !ts) return;
   *ts = (split->date_reconciled);
}

/********************************************************************\
\********************************************************************/

/* return the parent transaction of the split */
Transaction * 
xaccSplitGetParent (Split *split)
{
   if (!split) return NULL;
   return (split->parent);
}

Account *
xaccSplitGetAccount (Split *split)
{
   if (!split) return NULL;
   return (split->acc);
}

char *
xaccSplitGetMemo (Split *split)
{
   if (!split) return NULL;
   return (split->memo);
}

char *
xaccSplitGetAction (Split *split)
{
   if (!split) return NULL;
   return (split->action);
}

char *
xaccSplitGetDocref (Split *split)
{
   if (!split) return NULL;
   return (split->docref);
}

char 
xaccSplitGetReconcile (Split *split)
{
   if (!split) return ' ';
   return (split->reconciled);
}

double
xaccSplitGetShareAmount (Split * split)
{
   if (!split) return 0.0;
   return (split->damount);
}

double
xaccSplitGetValue (Split * split)
{
   if (!split) return 0.0;
   return ((split->damount) * (split->share_price));
}

double
xaccSplitGetSharePrice (Split * split)
{
   if (!split) return 1.0;
   return (split->share_price);
}

/********************************************************************\
\********************************************************************/

Account *
xaccGetAccountByName (Transaction *trans, const char * name)
{
   Split *s;
   Account *acc = NULL;
   int i;

   if (!trans) return NULL;
   if (!name) return NULL;

   /* walk through the splits, looking for one, any one, that has a parent account */
   i = 0;
   s = trans->splits[0];
   while (s) {
      acc = s->acc;
      if (acc) break;
      i++;
      s = trans->splits[i];
   }
   
   if (!acc) return 0x0;

   acc = xaccGetPeerAccountFromName (acc, name);
   return acc;
}

/********************************************************************\
\********************************************************************/

Account *
xaccGetAccountByFullName (Transaction *trans, const char * name,
                          const char separator)
{
   Split *s;
   Account *acc = NULL;
   int i;

   if (!trans) return NULL;
   if (!name) return NULL;

   /* walk through the splits, looking for one, any one, that has a parent account */
   i = 0;
   s = trans->splits[0];
   while (s) {
      acc = s->acc;
      if (acc) break;
      i++;
      s = trans->splits[i];
   }
   
   if (!acc) return 0x0;

   acc = xaccGetPeerAccountFromFullName (acc, name, separator);
   return acc;
}

/********************************************************************\
\********************************************************************/

Split *
xaccGetOtherSplit (Split *split)
{
   Transaction *trans;

   if (!split) return NULL;
   trans = split->parent;

   /* if more than two splits, return NULL */
   if ((trans->splits[1]) && (trans->splits[2])) return NULL;

   if (split == trans->splits[0]) return (trans->splits[1]);
   if (split == trans->splits[1]) return (trans->splits[0]);
   return NULL;  /* never reached, in theory */
}

/********************************************************************\
\********************************************************************/

int
xaccIsPeerSplit (Split *sa, Split *sb)
{
   Transaction *ta, *tb;
   if (!sa || !sb) return 0;
   ta = sa->parent;
   tb = sb->parent;
   if (ta == tb) return 1;
   return 0;
}

/********************************************************************\
\********************************************************************/

Split *
IthSplit (Split **list, int i)
{
   if (!list || 0 > i) return NULL;
   return list[i];
}

Transaction *
IthTransaction (Transaction **list, int i)
{
   if (!list || 0 > i) return NULL;
   return list[i];
}

/************************ END OF ************************************\
\************************* FILE *************************************/
