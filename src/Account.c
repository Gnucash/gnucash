/********************************************************************\
 * Account.c -- the Account data structure                          *
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

#include "config.h"

#include "Account.h"
#include "Data.h"
#include "date.h"
#include "Transaction.h"
#include "util.h"

int next_free_unique_account_id = 0;

/********************************************************************\
 * Because I can't use C++ for this project, doesn't mean that I    *
 * can't pretend too!  These functions perform actions on the       *
 * account data structure, in order to encapsulate the knowledge    *
 * of the internals of the Account in one file.                     *
\********************************************************************/

/********************************************************************\
\********************************************************************/
Account *
mallocAccount( void )
  {
  Account *acc = (Account *)_malloc(sizeof(Account));
  
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
  
  /* private data */
  acc->arrowb        = NULL;
  acc->expand        = 0;
  acc->regData       = NULL;
  acc->regLedger     = NULL;
  acc->ledgerList    = NULL;
  acc->recnData      = NULL;
  acc->adjBData      = NULL;
  acc->editAccData   = NULL;
  acc->editNotesData = NULL;
  
  return acc;
  }

/********************************************************************\
\********************************************************************/
void
freeAccount( Account *acc )
{
  int i=0, j=0;
  Split *s, *debit_s;
  int dont_free_transaction = 0;
  struct _account * _acc = (struct _account *) acc; 

  if (NULL == acc) return;
    
  /* recursively free children */
  freeAccountGroup (acc->children);

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
    debit_s = trans->debit_splits[0];
    while (debit_s) {
      if (debit_s->acc) { dont_free_transaction = 1; break; }
      j++;
      debit_s = trans->debit_splits[j];
    }

    if ( (!dont_free_transaction) && (NULL == trans->credit_split.acc) ) {
      freeTransaction( trans );
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
  
  /* hack alert -- shouldn't we destroy this widget ??? */
  acc->arrowb   = NULL;  
  acc->expand   = 0;
  acc->regData  = NULL;
  acc->regLedger = NULL;
  acc->recnData = NULL;
  acc->adjBData = NULL;

  if (acc->ledgerList) _free (acc->ledgerList);
  acc->ledgerList = NULL;

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
  int  inserted = False;
  Split **oldsplits;
  Transaction *trans;

  if (!acc) return;
  if (!split) return;

  /* mark the data file as needing to be saved: */
  if( acc->parent != NULL ) acc->parent->saved = False;

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
          inserted = True;
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

  /* mark the data file as needing to be saved: */
  if( acc->parent != NULL ) acc->parent->saved = False;
  
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
    if( xaccTransOrder (&(prevSplit->parent), &(split->parent)) >0 ) outOfOrder = True;
  }
  if (NULL != nextSplit) {
    if( xaccTransOrder (&(split->parent), &(nextSplit->parent)) >0 ) outOfOrder = True;
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

  acc = (Account *) (trans->credit_split.acc);
  outOfOrder += xaccCheckDateOrder (acc, &(trans->credit_split));

  i=0;
  s = trans->debit_splits[0];
  while (s) {
    acc = (Account *) (s->acc);
    outOfOrder += xaccCheckDateOrder (acc, s);
    i++;
    s = trans->debit_splits[i];
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
xaccConsolidateTransactions (Account * acc)
{
   Split *sa, *sb;
   int i,j,k;

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

#ifdef STIL_BROKEN
/* hack alert -- still broken from splits */
         /* Free the transaction, and shuffle down by one.
          * Need to shuffle in order to preserve date ordering. */
         freeTransaction (tb);
         
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
