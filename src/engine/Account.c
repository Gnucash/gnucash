/********************************************************************\
 * Account.c -- the Account data structure                          *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1997, 1998, 1999 Linas Vepstas                     *
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
#include "Group.h"
#include "GroupP.h"
#include "date.h"
#include "messages.h"
#include "Queue.h"
#include "Transaction.h"
#include "TransactionP.h"
#include "util.h"

/* The unsafe_ops flag allows certain unsafe manipulations to be 
 * performed on the data structures. Normally, this is disabled,
 * as it can lead to scrambled data.
 * hack alert -- this should be a configurable parameter.
 */
int unsafe_ops = 1;

int next_free_unique_account_id = 0;

static short module = MOD_ENGINE; 

#ifndef FALSE
#define FALSE 0
#endif

#ifndef TRUE
#define TRUE 1
#endif

/********************************************************************\
 * Because I can't use C++ for this project, doesn't mean that I    *
 * can't pretend too!  These functions perform actions on the       *
 * account data structure, in order to encapsulate the knowledge    *
 * of the internals of the Account in one file.                     *
\********************************************************************/

/********************************************************************\
\********************************************************************/
void
xaccInitAccount (Account * acc)
{
  acc->id = next_free_unique_account_id;
  next_free_unique_account_id ++;

  acc->parent   = NULL;
  acc->children = NULL;

  acc->balance  = 0.0;
  acc->cleared_balance = 0.0;
  acc->reconciled_balance = 0.0;
  acc->running_balance  = 0.0;
  acc->running_cleared_balance = 0.0;
  acc->running_reconciled_balance = 0.0;

  acc->flags = 0;
  acc->type  = -1;
  acc->accInfo = NULL;
  
  acc->accountName = strdup("");
  acc->accountCode = strdup("");
  acc->description = strdup("");
  acc->notes       = strdup("");
  acc->currency    = strdup("");
  acc->security    = strdup("");
  
  acc->numSplits   = 0;
  acc->splits      = (Split **) _malloc (sizeof (Split *));
  acc->splits[0]   = NULL;
  
  acc->changed     = 0;
  acc->open        = 0;
}

/********************************************************************\
\********************************************************************/

Account *
xaccMallocAccount( void )
{
  Account *acc = (Account *)_malloc(sizeof(Account));
  xaccInitAccount (acc);
  return acc;
}

/********************************************************************\
\********************************************************************/

void
xaccFreeAccount( Account *acc )
{
  int i=0;
  Split *s;
  Transaction *t;

  if (NULL == acc) return;
    
  /* First, recursively free children */
  xaccFreeAccountGroup (acc->children);
  
  /* Next, clean up the splits */
  /* any split pointing at this account needs to be unmarked */
  for (i=0; i<acc->numSplits; i++) {
    s = acc->splits[i];
    s->acc = NULL;
  }

  /* destroy all of the splits. The xaccCommitEdit() call
   * will automatically clean up orphaned transactions.
   */
  acc->open |= ACC_BEING_DESTROYED;
  acc->open |= ACC_DEFER_REBALANCE;
  for (i=0; i<acc->numSplits; i++) {
    s = acc->splits[i];
    t = s->parent;
    xaccTransBeginEdit (t, 1);
    xaccSplitDestroy (s);
    xaccTransCommitEdit (t);
  }

  /* free up array of split pointers */
  _free (acc->splits);
  acc->splits = NULL;
  acc->numSplits = 0;
  
  /* Finally, clean up the account info */
  if (acc->accInfo) xaccFreeAccInfo (acc->accInfo); 
  acc->accInfo = NULL;

  if (acc->accountName) free (acc->accountName);
  if (acc->accountCode) free (acc->accountCode);
  if (acc->description) free (acc->description);
  if (acc->notes) free (acc->notes);
  if (acc->currency) free (acc->currency);
  if (acc->security) free (acc->security);

  /* zero out values, just in case stray 
   * pointers are pointing here. */

  acc->parent   = NULL;
  acc->children = NULL;

  acc->balance  = 0.0;
  acc->cleared_balance = 0.0;
  acc->reconciled_balance = 0.0;

  acc->flags = 0;
  acc->type  = -1;
  
  acc->accountName = NULL;
  acc->description = NULL;
  acc->notes       = NULL;
  acc->currency    = NULL;
  acc->security    = NULL;
  
  acc->changed     = 0;
  acc->open        = 0;

  _free(acc);
}

/********************************************************************\
\********************************************************************/

void 
xaccAccountBeginEdit (Account *acc, int defer)
{
   if (!acc) return;
   acc->open = ACC_BEGIN_EDIT;
   if (defer) acc->open |= ACC_DEFER_REBALANCE;
}

void 
xaccAccountCommitEdit (Account *acc)
{
   if (!acc) return;
   acc->changed |= ACC_INVALIDATE_ALL;
   acc->open = 0;
}

/********************************************************************\
\********************************************************************/

int
xaccGetAccountID (Account *acc)
{
  if (!acc) return -1;
  return acc->id;
}

/********************************************************************\
\********************************************************************/

char
xaccGetAccountFlags (Account *acc)
{
  if (!acc) return -1;
  return acc->flags;
}

/********************************************************************\
\********************************************************************/

#define CHECK(acc) {					\
   if (0 == acc->open) {				\
      /* not today, some day in the future ... */	\
      /* printf ("Error: Account not open for editing\n"); */	\
      /* assert (0); */					\
      /* return; */					\
   }							\
  if (NULL != acc->parent) acc->parent->saved = FALSE;	\
}

/********************************************************************\
\********************************************************************/

void
xaccAccountInsertSplit ( Account *acc, Split *split )
{
  int  i,j;
  Split **oldsplits;
  Account *oldacc;

  if (!acc) return;
  if (!split) return;
  CHECK (acc);

  /* if this split belongs to another account, make sure that
   * the moving it is allowed by the currency denominations of 
   * the old and new accounts. Basically, both old and new accounts
   * must be denominated in the same currency.
   */
/*
hack alert -- in fact this logic is wildly incorrect;
disable for now till we figure out what the right thing is.
  if (split->acc) {
    if (acc->currency) {
       if (!(split->acc->currency)) return;
       if (strcmp (acc->currency, split->acc->currency)) return;
    }  else { 
       if (split->acc->currency) return;
    }
  }
*/

  /* mark the account as having changed */
  acc -> changed |= ACC_INVALIDATE_ALL;

  /* if this split belongs to another acount, remove it from 
   * there first.  We don't want to ever leave the system
   * in an inconsistent state.
   */
  oldacc = split->acc;
  if (split->acc) xaccAccountRemoveSplit (split->acc, split);
  split->acc = acc;
    
  /* enlarge the size of the split array to accomadate the new split,
   * and copy all the splits over to the new array. 
   * If the old and new accounts are the same account, then we
   * are just shuffling around the split, resumably due to a 
   * date reordering.  In this case, most of the malloc/copy/free bit
   * can be avoided.
   */
  if (oldacc != acc) {
     oldsplits = acc->splits;
     acc->numSplits ++;

     acc->splits = (Split **)_malloc(((acc->numSplits) + 1) * sizeof(Split *));
     
     /* Find the insertion point */
     /* to get realy fancy, could use binary search. */
     for(i = 0; i < (acc->numSplits - 1);) {
       if(xaccSplitDateOrder(&(oldsplits[i]), &split) > 0) {
         break;
       } else {
         acc->splits[i] = oldsplits[i];
       }
       i++;  /* Don't put this in the loop guard!  It'll go too far. */
     }
     /* Insertion point is now i */
   
     //fprintf(stderr, "Insertion position is: %d\n", i);
   
     /* Move all the other splits down (this could be done faster with memmove)*/
     for( j = acc->numSplits; j > i; j--) {
       acc->splits[j] = oldsplits[j - 1];
     }
   
     /* Now insert the new split */
     acc->splits[i] = split;
   
     /* make sure the array is NULL terminated */
     acc->splits[acc->numSplits] = NULL;
   
     _free(oldsplits);
  } else {
     acc->numSplits ++;

     /* Find the insertion point */
     /* to get realy fancy, could use binary search. */
     for(i = 0; i < (acc->numSplits - 1);) {
       if(xaccSplitDateOrder(&(acc->splits[i]), &split) > 0) {
         break;
       }
       i++;  /* Don't put this in the loop guard!  It'll go too far. */
     }
     /* Insertion point is now i */
   
     /* Move all the other splits down (this could be done faster with memmove)*/
     for( j = acc->numSplits; j > i; j--) {
       acc->splits[j] = acc->splits[j - 1];
     }
   
     /* Now insert the new split */
     acc->splits[i] = split;
   
     /* make sure the array is NULL terminated */
     acc->splits[acc->numSplits] = NULL;
  }

  xaccAccountRecomputeBalance (acc);
}


/********************************************************************\
\********************************************************************/

void
xaccAccountRemoveSplit ( Account *acc, Split *split )
  {
  int  i,j;

  if (!acc) return;
  if (!split) return;

  /* the being-destroyed flag prevents recursive scribbling upon oneself */
  if (acc->open & ACC_BEING_DESTROYED) return;
  CHECK (acc);

  /* mark the account as having changed */
  acc -> changed |= ACC_INVALIDATE_ALL;
  
  for( i=0,j=0; j<acc->numSplits; i++,j++ ) {
    acc->splits[i] = acc->splits[j];
    if (split == acc->splits[i]) i--;
  }
  
  split->acc = NULL;

  acc->numSplits --;

  /* make sure the array is NULL terminated */
  acc->splits[acc->numSplits] = NULL;

}


/********************************************************************\
 * xaccAccountRecomputeBalance                                      *
 *   recomputes the partial balances and the current balance for    *
 *   this account.                                                  *
 * 
 * The way the computation is done depends on whether the partial
 * balances are for a monetary account (bank, cash, etc.) or a 
 * certificate account (stock portfolio, mutual fund).  For bank
 * accounts, the invariant amount is the dollar amount. For share
 * accounts, the invariant amount is the number of shares. For
 * share accounts, the share price fluctuates, and the current 
 * value of such an account is the number of shares times the current 
 * share price.
 * 
 * Part of the complexity of this computatation stems from the fact 
 * xacc uses a double-entry system, meaning that one transaction
 * appears in two accounts: one account is debited, and the other 
 * is credited.  When the transaction represents a sale of shares,
 * or a purchase of shares, some care must be taken to compute 
 * balances correctly.  For a sale of shares, the stock account must
 * be debited in shares, but the bank account must be credited 
 * in dollars.  Thus, two different mechanisms must be used to
 * compute balances, depending on account type.
 *                                                                  *
 * Args:   account -- the account for which to recompute balances   *
 * Return: void                                                     *
\********************************************************************/

void
xaccAccountRecomputeBalance( Account * acc )
{
  int  i = 0; 
  double  dbalance    = 0.0;
  double  dcleared_balance = 0.0;
  double  dreconciled_balance = 0.0;
  double  share_balance    = 0.0;
  double  share_cleared_balance = 0.0;
  double  share_reconciled_balance = 0.0;
  double  amt = 0.0;
  Split *split, *last_split = NULL;
  
  if( NULL == acc ) return;
  if (0x0 == (ACC_INVALID_BALN & acc->changed)) return;
  acc->changed &= ~ACC_INVALID_BALN;

  split = acc->splits[0];
  while (split) {

    /* compute both dollar and share balances */
    amt = split->damount;
    share_balance += amt;
    dbalance += amt * (split->share_price);
    
    if( NREC != split -> reconciled ) {
      share_cleared_balance += amt;
      dcleared_balance += amt * (split->share_price);
    }

    if( YREC == split -> reconciled ) {
      share_reconciled_balance += amt;
      dreconciled_balance += amt * (split->share_price);
    }

    /* For bank accounts, the invariant subtotal is the dollar
     * amount.  For stock accounts, the invariant is the share amount */
    if ( (STOCK == acc->type) || ( MUTUAL == acc->type) ) {
      split -> share_balance = share_balance;
      split -> share_cleared_balance = share_cleared_balance;
      split -> share_reconciled_balance = share_reconciled_balance;
      split -> balance = split->share_price * share_balance;
      split -> cleared_balance = split->share_price * share_cleared_balance;
      split -> reconciled_balance = split->share_price * share_reconciled_balance;
    } else {
      split -> share_balance = dbalance;
      split -> share_cleared_balance = dcleared_balance;
      split -> share_reconciled_balance = dreconciled_balance;
      split -> balance = dbalance;
      split -> cleared_balance = dcleared_balance;
      split -> reconciled_balance = dreconciled_balance;
    }
    /* invalidate the cost basis; this has to be computed with other routine */
    split -> cost_basis = 0.0;

    last_split = split;
    i++;
    split = acc->splits[i];
  }

  if ( (STOCK == acc->type) || ( MUTUAL == acc->type) ) {
    if (last_split) {
       acc -> balance = share_balance * (last_split->share_price);
       acc -> cleared_balance = share_cleared_balance * (last_split->share_price);
       acc -> reconciled_balance = share_reconciled_balance * (last_split->share_price);
    } else {
       acc -> balance = 0.0;
       acc -> cleared_balance = 0.0;
       acc -> reconciled_balance = 0.0;
    }
  } else {
    acc -> balance = dbalance;
    acc -> cleared_balance = dcleared_balance;
    acc -> reconciled_balance = dreconciled_balance;
  }
    
  return;
}

/********************************************************************\
\********************************************************************/

void
xaccAccountRecomputeCostBasis( Account * acc )
{
  int  i = 0; 
  double  amt = 0.0;
  Split *split = NULL;
  Queue *q;
  
  if( NULL == acc ) return;
  if (0x0 == (ACC_INVALID_COSTB & acc->changed)) return;
  acc->changed &= ~ACC_INVALID_COSTB;

  /* create the FIFO queue */
  q = xaccMallocQueue ();

  /* loop over all splits in this account */
  split = acc->splits[0];
  while (split) {

    /* positive amounts are a purchase, negative are a sale. 
     * Use FIFO accounting: purchase to head, sale from tail. */
    amt = split->damount;
    if (0.0 < amt) {
       xaccQueuePushHead (q, split);
    } else 
    if (0.0 > amt) {
       xaccQueuePopTailShares (q, -amt);
    }
    split->cost_basis = xaccQueueGetValue (q);

    i++;
    split = acc->splits[i];
  }

  xaccFreeQueue (q);
}

/********************************************************************\
 * xaccCheckDateOrder                                               *
 *   check this split to see if the date is in correct order        *
 *   If it is not, reorder the transactions ...                     *
 *                                                                  *
 * Args:   acc   -- the account to check                            *
 *         split -- the split to check                              *
 *
 * Return: int -- non-zero if out of order                          *
\********************************************************************/

int
xaccCheckDateOrder (Account * acc, Split *split )
{
  int outOfOrder = 0;
  Split *s;
  Split *prevSplit = NULL;
  Split *nextSplit = NULL;
  int position;

  if (NULL == acc) return 0;
  if (NULL == split) return 0;

  /* find the split's location in the array */
  position = 0;
  s = acc->splits[0];
  while (s) {
     if (s == split) break;
     position ++;
     s = acc->splits[position];
  }

  if (!s) {
     printf ("Internal Error: xaccCheckDateOrder(): ");
     printf (" split %p not present in account \n", split);
     return 0;
  }

  /* if zeroth split, then there is no previous */
  if (0 < position) prevSplit = acc->splits [position-1];

  /* if last split, OK, since array is null terminated, and last+1 is null */
  nextSplit = acc->splits [position+1];  

  /* figure out if the transactions are out of order */
  if (NULL != prevSplit) {
    if( xaccSplitDateOrder (&prevSplit, &split) > 0 ) outOfOrder = TRUE;
  }
  if (NULL != nextSplit) {
    if( xaccSplitDateOrder (&split, &nextSplit) > 0 ) outOfOrder = TRUE;
  }

  /* take care of re-ordering, if necessary */
  if( outOfOrder ) {
    xaccAccountInsertSplit( acc, split );
    return 1;
  }
  return 0;
}

/********************************************************************\
 * xaccCheckTransDateOrder                                          *
 *   check this transaction to see if the date is in correct order  *
 *   If it is not, reorder the transactions ...                     *
 *   This routine perfroms the check for both of the double-entry   *
 *   transaction entries ...                                        *
 *                                                                  *
 * Args:   trans -- the transaction to check                        *
 * Return: int -- non-zero if out of order                          *
\********************************************************************/

int
xaccCheckTransDateOrder (Transaction *trans )
{
  Account * acc;
  int outOfOrder = 0;
  Split *s;
  int i = 0;

  if (NULL == trans) return 0;

  i=0;
  s = trans->splits[0];
  while (s) {
    acc = (Account *) (s->acc);
    outOfOrder += xaccCheckDateOrder (acc, s);
    i++;
    s = trans->splits[i];
  }

  if (outOfOrder) return 1;
  return 0;
}

/********************************************************************\
\********************************************************************/

/* The sort order is used to implicitly define an 
 * order for report generation */

static int typeorder[NUM_ACCOUNT_TYPES] = {
     BANK, STOCK, MUTUAL, CURRENCY, CASH, ASSET, 
     CREDIT, LIABILITY, INCOME, EXPENSE, EQUITY };

static int revorder[NUM_ACCOUNT_TYPES] = {
     -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 };


int
xaccAccountOrder (Account **aa, Account **ab)
{
  char *da, *db; 
  int ta, tb;

  if ( (*aa) && !(*ab) ) return -1;
  if ( !(*aa) && (*ab) ) return +1;
  if ( !(*aa) && !(*ab) ) return 0;

  /* sort on accountCode strings */
  da = (*aa)->accountCode;
  db = (*ab)->accountCode;
  SAFE_STRCMP (da, db);

  /* if acccount-type-order array not initialized, initialize it */
  /* this will happen at most once during program invocation */
  if (-1 == revorder[0]) {
    int i;
    for (i=0; i<NUM_ACCOUNT_TYPES; i++) {
      revorder [typeorder[i]] = i;
    }
  }

  /* otherwise, sort on account type */
  ta = (*aa)->type;
  tb = (*ab)->type;
  ta = revorder[ta];
  tb = revorder[tb];
  if (ta < tb) return -1;
  if (ta > tb) return +1;

  /* otherwise, sort on accountName strings */
  da = (*aa)->accountName;
  db = (*ab)->accountName;
  SAFE_STRCMP (da, db);

  /* accountName strings should really, really be unique, and so in theory
   * we should never ever get here.  But just in case theory is broke ... */
  da = (*aa)->currency;
  db = (*ab)->currency;
  SAFE_STRCMP (da, db);

  da = (*aa)->security;
  db = (*ab)->security;
  SAFE_STRCMP (da, db);

  return 0;
}

/********************************************************************\
\********************************************************************/
/* account codes will be assigned base-36, with three digits */

#define BASE 36

void 
xaccAccountAutoCode (Account *acc, int digits)
{
  if (!acc) return;
  if (acc->accountCode) return;   /* no-op if code already assinged */
  if (!(acc->parent)) return; 

  acc->accountCode = xaccGroupGetNextFreeCode (acc->parent, digits);
  acc->parent->saved = FALSE;
}

/********************************************************************\
\********************************************************************/

int
xaccIsAccountInList (Account * acc, Account **list)
{
   Account * chk;
   int nacc = 0;
   int nappearances = 0;
   if (!acc) return 0;
   if (!list) return 0;

   chk = list[0];
   while (chk) {
      if (acc == chk) nappearances ++;
      nacc++;
      chk = list[nacc];
   }
   return nappearances;
}

/********************************************************************\
\********************************************************************/

void
xaccAccountRecomputeBalances( Account **list )
{
   Account * acc;
   int nacc = 0;
   if (!list) return;

   acc = list[0];
   while (acc) {
      xaccAccountRecomputeBalance (acc);
      nacc++;
      acc = list[nacc];
   }
}

/********************************************************************\
\********************************************************************/

void
xaccZeroRunningBalances( Account **list )
{
   Account * acc;
   int nacc = 0;
   if (!list) return;

   acc = list[0];
   while (acc) {
      acc -> running_balance = 0.0;
      acc -> running_cleared_balance = 0.0;
      nacc++;
      acc = list[nacc];
   }
}

/********************************************************************\
\********************************************************************/

void
xaccMoveFarEnd (Split *split, Account *new_acc)
{
   Split *partner_split = 0x0;
   Transaction *trans;
   Account * acc;
   int numsplits = 0;

   if (!split) return;
   
   /* if the transaction has two splits, then the "far end" 
    * is the other one. Otherwise, fare end is undefined. 
    * If the new destination does not match the current dest,
    * then move the far end of the split to the new location.
    */
   trans = (Transaction *) (split->parent);
   assert (trans);
   assert (trans->splits);

   numsplits = xaccCountSplits (trans->splits);
   if (2 < numsplits) return;

   if (split == trans->splits[0]) {
      partner_split = trans->splits [1];
   } else

   if (split == trans->splits[1]) {
      partner_split = trans->splits [0];
   } else 

   if (new_acc) {
      /* Gosh, the far end doesn't exist! create it! */
      partner_split = xaccMallocSplit ();
      xaccTransAppendSplit (trans, partner_split);
      xaccAccountInsertSplit (new_acc, partner_split);
      return;
   } else {
      /* no partner split, AND no far-end accouont. return */
      return;
   }

   /* move the partner split from the old account to the new */ 
   acc = (Account *) (partner_split->acc);
   if (acc != new_acc) {
      xaccAccountInsertSplit (new_acc, partner_split);
   }
}

/********************************************************************\
\********************************************************************/

void
xaccMoveFarEndByName (Split *split, const char *new_acc_name)
{
   Account *acc;

   if (!split) return;
   if (0 == strcmp (SPLIT_STR, new_acc_name)) return;

   acc = (Account *) split->acc;
   acc = xaccGetPeerAccountFromName (acc, new_acc_name);
   xaccMoveFarEnd (split, acc);
}

/********************************************************************\
\********************************************************************/

void
xaccConsolidateTransactions (Account * acc)
{
   Split *sa, *sb;
   Transaction *ta, *tb;
   Timespec ts;
   int i,j;
   int retval;

   if (!acc) return;
   CHECK (acc);

   for (i=0; i<acc->numSplits; i++) {
      sa = acc->splits[i];
      ta = sa->parent;
      for (j=i+1; j<acc->numSplits; j++) {
         sb = acc->splits[j];
         tb = sb->parent;

         /* A single transaction can have multiple splits in the same
          * account.  For instance, a split deposit in AccountA of two
          * checks from AccountB creates two splits in AccountB with
          * the same parent transaction.  Skip this case.
          */
         if (ta == tb) continue;

         /* if no match, then continue on in the loop.
          * we really must match everything to get a duplicate */
         retval = xaccTransMatch (&ta, &tb);
         if (retval) continue;

         /* OK, looks like the two splits are a matching pair. 
          * Blow one of them, and its entire associated transaction, away.
          * (We blow away the transaction because not only do the splits 
          * match, but so do all of their partner-splits.)
          *
          * But, before we blow it away, we go through each split and
          * update the reconciled flag and date of the split in the
          * remaining transaction with those in the one being deleted,
          * but only if the remaining transaction has an NREC reconciled
          * flag. In other words, the two splits match on everything 
          * but the reconciled flags and dates, so we assume that the
          * one which is not NREC is 'more correct'. This is true in
          * the case of importing two QIF files with overlapping
          * transactions. Each file will have one 'half' of the
          * transaction, but the other half will be generated by the
          * QIF importing routines, but with default values for the
          * reconciled data. When we load the other file, we need to
          * replace the generated 'half' with the real one.
          */

         xaccTransBeginEdit (ta, 1);

         for (i=xaccTransCountSplits(ta); i>0; i--) {
           sa = xaccTransGetSplit (ta, i - 1);

           /* If the remaining split is reconciled as something
              other than NREC, just leave it alone. */
           if (xaccSplitGetReconcile(sa) != NREC)
             continue;

           /* We get the matching split using the tickee value
              generated by xaccTransMatch above. */
           sb = xaccTransGetSplit(tb, sa->tickee);

           xaccSplitSetReconcile (sa, xaccSplitGetReconcile(sb));
           xaccSplitGetDateReconciledTS (sb, &ts);
           xaccSplitSetDateReconciledTS (sa, &ts);
         }

         xaccTransCommitEdit (ta);

         xaccTransBeginEdit (tb, 1);
         xaccTransDestroy (tb);
         xaccTransCommitEdit (tb);


         /* It should be safe to just "break" here, as all splits
          * with index i or less have been checked already and couldn't
          * have been dupes.  So index i is still valid, although j is 
          * not.  Note that numSplits changed ...
          */
         break;
      }
   }
}

/********************************************************************\
\********************************************************************/

void 
xaccAccountSetType (Account *acc, int tip)
{
   if (!acc) return;
   CHECK (acc);

   /* After an account type has been set, it cannot be changed */
   if (-1 < acc->type) {
      PERR ("xaccAccountSetType(): "
            "the type of the account cannot be changed "
            "after its been set! \n"
           );
      return;
   }

   /* refuse invalid account types */
   if (NUM_ACCOUNT_TYPES <= tip) return;
   acc->type = tip;

   /* initialize the auxilliary account info as well */
   if (acc->accInfo) xaccFreeAccInfo (acc->accInfo); 
   acc->accInfo = xaccMallocAccInfo (tip);
}

void 
xaccAccountSetName (Account *acc, char *str)
{
   char * tmp;
   if ((!acc) || (!str)) return;
   CHECK (acc);

   /* make strdup before freeing */
   tmp = strdup (str);
   if (acc->accountName) free (acc->accountName);
   acc->accountName = tmp;
}

void 
xaccAccountSetCode (Account *acc, char *str)
{
   char * tmp;
   if ((!acc) || (!str)) return;
   CHECK (acc);

   /* make strdup before freeing */
   tmp = strdup (str);
   if (acc->accountCode) free (acc->accountCode);
   acc->accountCode = tmp;
}

void 
xaccAccountSetDescription (Account *acc, char *str)
{
   char * tmp;
   if ((!acc) || (!str)) return;
   CHECK (acc);

   /* make strdup before freeing */
   tmp = strdup (str);
   if (acc->description) free (acc->description);
   acc->description = tmp;
}

void 
xaccAccountSetNotes (Account *acc, char *str)
{
   char * tmp;
   if ((!acc) || (!str)) return;
   CHECK (acc);

   /* make strdup before freeing */
   tmp = strdup (str);
   if (acc->notes) free (acc->notes);
   acc->notes = tmp;
}

void 
xaccAccountSetCurrency (Account *acc, char *str)
{
   if ((!acc) || (!str)) return;
   CHECK (acc);

   if (acc->currency && (0x0 != acc->currency[0])) {
      if (unsafe_ops) {
         PWARN ("xaccAccountSetCurrency(): "
                "it is dangerous to change the currency denomination of an account! \n"
                "\tAccount=%s old currency=%s new currency=%s \n",
                acc->accountName, acc->currency, str);
      } else {
         PERR ("xaccAccountSetCurrency(): "
               "the currency denomination of an account cannot be changed!\n"
                "\tAccount=%s \n", acc->accountName);
         return;
      }
   }
   /* free the zero-length string */
   if (acc->currency) free (acc->currency);
   acc->currency = strdup (str);
}

void 
xaccAccountSetSecurity (Account *acc, char *str)
{
   if ((!acc) || (!str)) return;
   CHECK (acc);

   if (acc->security && (0x0 != acc->security[0])) {
      if (unsafe_ops) {
         PWARN ("xaccAccountSetSecurity(): "
                "it is dangerous to change the security denomination of an account! \n"
                "\tAccount=%s old security=%s new security=%s \n",
                acc->accountName, acc->security, str);
      } else {
         PERR ("xaccAccountSetSecurity(): "
               "the security denomination of an account cannot be changed!\n"
                "\tAccount=%s \n", acc->accountName);
         return;
      }
   }
   /* free the zero-length string */
   if (acc->security) free (acc->security);
   acc->security = strdup (str);
}

/********************************************************************\
\********************************************************************/

AccInfo *
xaccAccountGetAccInfo (Account *acc)
{
   if (!acc) return NULL;
   return (acc->accInfo);
}

AccountGroup *
xaccAccountGetChildren (Account *acc)
{
   if (!acc) return NULL;
   return (acc->children);
}

AccountGroup *
xaccAccountGetParent (Account *acc)
{
   if (!acc) return NULL;
   return (acc->parent);
}

Account *
xaccAccountGetParentAccount (Account * acc)
{
  if (!acc) return NULL;
  return xaccGroupGetParentAccount(acc->parent);
}

int
xaccAccountGetType (Account *acc)
{
   if (!acc) return 0;
   return (acc->type);
}

char *
xaccAccountGetName (Account *acc)
{
   if (!acc) return NULL;
   return (acc->accountName);
}

char *
xaccAccountGetFullName(Account *account, const char separator)
{
  Account *a;
  char *fullname;
  char *name;
  char *p;
  int length;

  if (account == NULL)
  {
    fullname = strdup("");
    assert(fullname != NULL);
    return fullname;
  }

  /* Figure out how much space is needed */
  length = 0;
  a = account;
  while (a != NULL)
  {
    name = xaccAccountGetName(a);

    length += strlen(name) + 1; /* plus one for the separator */

    a = xaccAccountGetParentAccount(a);
  }

  /* length has one extra separator in it, that's ok, because it will
   * hold the null character at the end. */

  /* allocate the memory */
  fullname = malloc(length * sizeof(char));
  assert(fullname != 0);

  /* go to end of string */
  p = fullname + length - 1;

  /* put in the null character and move to the previous char */
  *p-- = 0;

  a = account;
  while (a != NULL)
  {
    name = xaccAccountGetName(a);
    length = strlen(name);

    /* copy the characters going backwards */
    while (length > 0)
      *p-- = name[--length];

    a = xaccAccountGetParentAccount(a);

    /* if we're not at the root, add another separator */
    if (a != NULL)
      *p-- = separator;
  }

  return fullname;
}

char *
xaccAccountGetCode (Account *acc)
{
   if (!acc) return NULL;
   return (acc->accountCode);
}

char * 
xaccAccountGetDescription (Account *acc)
{
   if (!acc) return NULL;
   return (acc->description);
}

char * 
xaccAccountGetNotes (Account *acc)
{
   if (!acc) return NULL;
   return (acc->notes);
}

char * 
xaccAccountGetCurrency (Account *acc)
{
   if (!acc) return NULL;
   return (acc->currency);
}

char * 
xaccAccountGetSecurity (Account *acc)
{
   if (!acc) return NULL;
   return (acc->security);
}

double
xaccAccountGetBalance (Account *acc)
{
   if (!acc) return 0.0;
   return (acc->balance);
}

double
xaccAccountGetClearedBalance (Account *acc)
{
   if (!acc) return 0.0;
   return (acc->cleared_balance);
}

double
xaccAccountGetReconciledBalance (Account *acc)
{
   if (!acc) return 0.0;
   return (acc->reconciled_balance);
}

Split *
xaccAccountGetSplit (Account *acc, int i)
{
   if (!acc) return NULL;
   if (!(acc->splits)) return NULL;

   return (acc->splits[i]);
}

Split **
xaccAccountGetSplitList (Account *acc)
{
   if (!acc) return NULL;
   return (acc->splits);
}

int
xaccAccountGetNumSplits (Account *acc)
{
   if (!acc) return 0;
   return (acc->numSplits);
}

/********************************************************************\
\********************************************************************/

Account * 
IthAccount (Account **list, int i)
{
   if (!list || 0 > i) return NULL;
   return list[i];
}

/********************************************************************\
\********************************************************************/

gncBoolean
xaccAccountsHaveCommonCurrency(Account *account_1, Account *account_2)
{
  if ((account_1 == NULL) || (account_2 == NULL))
    return GNC_F;

  return xaccIsCommonCurrency(account_1->currency, account_1->security,
			      account_2->currency, account_2->security);
}

/********************************************************************\
\********************************************************************/

gncBoolean
xaccAccountHasAncestor (Account *account, Account * ancestor)
{
  Account *parent;

  if ((account == NULL) || (ancestor == NULL))
    return GNC_F;

  parent = xaccAccountGetParentAccount(account);
  while (parent != NULL)
  {
    if (parent == ancestor)
      return GNC_T;

    parent = xaccAccountGetParentAccount(parent);
  }

  return GNC_F;
}

/*************************** END OF FILE **************************** */
