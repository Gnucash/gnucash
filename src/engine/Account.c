/********************************************************************\
 * Account.c -- the Account data structure                          *
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

#include "config.h"

#include "Account.h"
#include "Group.h"
#include "date.h"
#include "messages.h"
#include "Transaction.h"
#include "util.h"

int next_free_unique_account_id = 0;

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
  acc->running_balance  = 0.0;
  acc->running_cleared_balance = 0.0;

  acc->flags = 0;
  acc->type  = -1;
  
  acc->accountName = NULL;
  acc->description = NULL;
  acc->notes       = NULL;
  
  acc->numSplits   = 0;
  acc->splits      = (Split **) _malloc (sizeof (Split *));
  acc->splits[0]   = NULL;
  
  acc->changed     = 0;
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
  int i=0, j=0;
  Split *s, *debit_s;
  int dont_free_transaction = 0;

  if (NULL == acc) return;
    
  /* recursively free children */
  xaccFreeAccountGroup (acc->children);

  free(acc->accountName);
  free(acc->description);
  free(acc->notes);
  
  /* any split pointing at this account needs to be unmarked */
  i=0;
  s = acc->splits[0];
  while (s) {
    s->acc = NULL;
    i++;
    s = acc->splits[i];
  }

  /* search for orphaned transactions, and delete them */
  i=0;
  s = acc->splits[0];
  while (s) {
    Transaction *trans =  (Transaction *) s->parent;

    j=0;
    debit_s = trans->dest_splits[0];
    while (debit_s) {
      if (debit_s->acc) { dont_free_transaction = 1; break; }
      j++;
      debit_s = trans->dest_splits[j];
    }

    if ( (!dont_free_transaction) && (NULL == trans->source_split.acc) ) {
      xaccFreeTransaction( trans );
    }

    i++;
    s = acc->splits[i];
  }

  /* free up array of split pointers */
  _free (acc->splits);
  acc->splits = NULL;
  
  /* zero out values, just in case stray 
   * pointers are pointing here. */

  acc->parent   = NULL;
  acc->children = NULL;

  acc->balance  = 0.0;
  acc->cleared_balance = 0.0;

  acc->flags = 0;
  acc->type  = -1;
  
  acc->accountName = NULL;
  acc->description = NULL;
  acc->notes       = NULL;
  
  _free(acc);
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

void
xaccInsertSplit ( Account *acc, Split *split )
  {
  int  i,j;
  int  inserted = FALSE;
  Split **oldsplits;
  Transaction *trans;

  if (!acc) return;
  if (!split) return;

  /* mark the account as having changed, and
   * the account group as requiring a save */
  acc -> changed = TRUE;
  if( acc->parent != NULL ) acc->parent->saved = FALSE;

  split->acc = (struct _account *) acc;
    
  oldsplits = acc->splits;
  acc->numSplits ++;
  acc->splits = (Split **)_malloc(((acc->numSplits) + 1) * sizeof(Split *));
  
  /* dt is the date of the transaction we are inserting, and dj
   * is the date of the "cursor" transaction... we want to insert
   * the new transaction before the first transaction of the same
   * or later date.  The !inserted bit is a bit of a kludge to 
   * make sure we only insert the new transaction once! */
  trans = (Transaction *) (split->parent);
  for( i=0,j=0; i<acc->numSplits; i++,j++ ) {
    /* if we didn't do this, and we needed to insert into the
     * last spot in the array, we would walk off the end of the
     * old array, which is no good! */
    if( j>=(acc->numSplits-1) ) {
      acc->splits[i] = split;
      break;
    } else {
      if (!inserted) {
        Transaction *ot;
        ot = (Transaction *) (oldsplits[j] -> parent);
        if (xaccTransOrder (&ot,&trans) > 0) {
          acc->splits[i] = split;
          j--;
          inserted = TRUE;
        } else {
          acc->splits[i] = oldsplits[j];
        }
      } else {
        acc->splits[i] = oldsplits[j];
      }
    }
  }
  
  /* make sure the array is NULL terminated */
  acc->splits[acc->numSplits] = NULL;

  _free(oldsplits);
}


/********************************************************************\
\********************************************************************/

void
xaccRemoveSplit ( Account *acc, Split *split )
  {
  int  i,j;

  if (!acc) return;
  if (!split) return;

  /* mark the account as having changed, and
   * the account group as requiring a save */
  acc -> changed = TRUE;
  if( acc->parent != NULL ) acc->parent->saved = FALSE;
  
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
 * xaccRecomputeBalance                                             *
 *   recomputes the partial balances and the current balance for    *
 *   this account.                                                  *
 * 
 * The way the computation is done depends on whether the partial
 * balances are for a monetary account (bank, cash, etc.) or a 
 * certificate account (stock portfolio, mutual fund).  For bank
 * accounts, the invarient amount is the dollar amount. For share
 * accounts, the invarient amount is the number of shares. For
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
xaccRecomputeBalance( Account * acc )
{
  int  i = 0; 
  double  dbalance    = 0.0;
  double  dcleared_balance = 0.0;
  double  dreconciled_balance = 0.0;
  double  share_balance    = 0.0;
  double  share_cleared_balance = 0.0;
  double  share_reconciled_balance = 0.0;
  double  amt = 0.0;
  Split *split, *last_split;
  
  if( NULL == acc ) return;
  if (FALSE == acc->changed) return;

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

    /* For bank accounts, the invarient subtotal is the dollar
     * amount.  For stock accoounts, the invarient is the share amount */
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
 * xaccCheckDateOrder                                               *
 *   check this transaction to see if the date is in correct order  *
 *   If it is not, reorder the transactions ...                     *
 *                                                                  *
 * Args:   acc   -- the account to check                            *
 *         trans -- the transaction to check                        *
 *
 * Return: int -- non-zero if out of order                          *
\********************************************************************/

int
xaccCheckDateOrder (Account * acc, Split *split )
{
  int outOfOrder = 0;
  Split *s;
  Split *prevSplit;
  Split *nextSplit;
  int position;

  if (NULL == acc) return 0;

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
     printf (" split not present in account \n");
     return 0;
  }

  prevSplit = acc->splits [position-1];
  nextSplit = acc->splits [position+1];

  /* figure out if the transactions are out of order */
  if (NULL != prevSplit) {
    if( xaccTransOrder (&(prevSplit->parent), &(split->parent)) >0 ) outOfOrder = TRUE;
  }
  if (NULL != nextSplit) {
    if( xaccTransOrder (&(split->parent), &(nextSplit->parent)) >0 ) outOfOrder = TRUE;
  }

  /* take care of re-ordering, if necessary */
  if( outOfOrder ) {
    xaccRemoveSplit( acc, split );
    xaccInsertSplit( acc, split );
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

  acc = (Account *) (trans->source_split.acc);
  outOfOrder += xaccCheckDateOrder (acc, &(trans->source_split));

  i=0;
  s = trans->dest_splits[0];
  while (s) {
    acc = (Account *) (s->acc);
    outOfOrder += xaccCheckDateOrder (acc, s);
    i++;
    s = trans->dest_splits[i];
  }

  if (outOfOrder) return 1;
  return 0;
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
xaccRecomputeBalances( Account **list )
{
   Account * acc;
   int nacc = 0;
   if (!list) return;

   acc = list[0];
   while (acc) {
      xaccRecomputeBalance (acc);
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

   if (!split) return;
   
   /* if the new desitnation does not match the current dest,
    * then move the far end of the split to the new location.
    */
   trans = (Transaction *) (split->parent);
   if (split != &(trans->source_split)) {
      partner_split = &(trans->source_split);
   } else {
      /* perform that transfer *only* if there is one split */
      if (trans->dest_splits) {
         if (0x0 != trans->dest_splits[0]) {
            if (0x0 == trans->dest_splits[1]) {
               partner_split = trans->dest_splits[0];
            }
         }
      }
   }

   if (partner_split) {
      /* remove the partner split from the old account */
      acc = (Account *) (partner_split->acc);
      if (acc != new_acc) {
         xaccRemoveSplit (acc, partner_split);
         xaccInsertSplit (new_acc, partner_split);
      }
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
   int i,j;

   if (!acc) return;

   for (i=0; i<acc->numSplits; i++) {
      sa = acc->splits[i];
      for (j=i+1; j<acc->numSplits; j++) {
         sb = acc->splits[j];

         /* if no match, then continue on in the loop.
          * we really must match everything to get a duplicate */
         if (sa->parent != sb->parent) continue;
         if (sa->reconciled != sb->reconciled) continue;
         if (0 == DEQ(sa->damount, sb->damount)) continue;
         if (0 == DEQ(sa->share_price, sb->share_price)) continue;
         if (strcmp (sa->memo, sb->memo)) continue;

#ifdef STILL_BROKEN
/* hack alert -- still broken from splits */
         /* Free the transaction, and shuffle down by one.
          * Need to shuffle in order to preserve date ordering. */
         xaccFreeTransaction (tb);
         
         for (k=j+1; k<acc->numTrans; k++) {
            acc->transaction[k-1] = acc->transaction[k];
         }
         acc->transaction[acc->numTrans -1] = NULL;
         acc->numTrans --;
#endif
      }
   }
}

/*************************** END OF FILE **************************** */
