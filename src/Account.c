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
#include "main.h"
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
  
  /* transction array should be null-terminated */
  acc->numTrans    = 0;
  acc->transaction = (Transaction **) _malloc (sizeof (Transaction *));
  acc->transaction[0] = NULL;
  
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
  acc->qfRoot   = mallocQuickFill();
  
  return acc;
  }

/********************************************************************\
\********************************************************************/
void
freeAccount( Account *acc )
{
  int i;

  if (NULL == acc) return;
    
  /* recursively free children */
  freeAccountGroup (acc->children);

  XtFree(acc->accountName);
  XtFree(acc->description);
  XtFree(acc->notes);
  
  freeQuickFill(acc->qfRoot);
  
  for( i=0; i<acc->numTrans; i++ ) {
    Transaction *trans = acc->transaction[i];
    struct _account * _acc = (struct _account *) acc; 

    if (!trans) continue;
    /* free the transaction only if its not 
     * a part of a double entry */
    if (_acc == trans->credit) trans->credit = NULL;
    if (_acc == trans->debit) trans->debit  = NULL;
    if ( (NULL == trans->debit) && (NULL == trans->credit) ) {
      freeTransaction( trans );
    }
  }
  
  /* free the array of pointers */
  _free( acc->transaction );
  
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
  
  acc->numTrans    = 0;
  acc->transaction = NULL; 
  
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

Transaction *
getTransaction( Account *acc, int num )
  {
  if( NULL == acc ) return NULL;

  if( (num >= 0) && (num < acc->numTrans) )
    return acc->transaction[num];
  else
    return NULL;
  }

/********************************************************************\
\********************************************************************/

int
getNumOfTransaction( Account *acc, Transaction *trans )
  {
  int i;
  for (i=0; i<acc->numTrans; i++) {
      if (trans == acc->transaction[i]) return i;
    }
  return -1;
  }

/********************************************************************\
\********************************************************************/

Transaction *
removeTransaction( Account *acc, int num )
{
  Transaction *trans = NULL;
  Transaction **oldTrans;
  int  i,j;

  if (NULL == acc) return NULL;

  oldTrans = acc->transaction;

  /* check for valid number */
  if( (0 > num) || (num >= acc->numTrans) ) return NULL;

  /* Set this flag, so we know we need to save the data file: */
  if( NULL != acc->parent ) acc->parent->saved = False;
    
  acc->numTrans--;

  acc->transaction = (Transaction **)_malloc(((acc->numTrans)+1)*
                                               sizeof(Transaction *));
     
  trans = oldTrans[acc->numTrans];/* In case we are deleting last in
                                     * old array */
  for( i=0,j=0; i<acc->numTrans; i++,j++ ) {
    if( j != num ) {
      acc->transaction[i] = oldTrans[j];
    } else {
      trans = oldTrans[j];
      i--;
    }
  }

  /* make sure the array is NULL terminated */
  acc->transaction[acc->numTrans] = NULL;

  _free (oldTrans);

  /* if this is a double-entry transaction, be sure to
   * unmark it. */
  if (((Account *)trans->credit) == acc) trans->credit = NULL;
  if (((Account *)trans->debit)  == acc) trans->debit  = NULL;

  /* hack alert -- we should alos remove splits from accounts */

  return trans;
}

/********************************************************************\
\********************************************************************/

void
xaccRemoveTransaction( Account *acc, Transaction *trans)
{
  int i;

  if (!acc) return;
  if (!trans) return;

  i = getNumOfTransaction (acc, trans);
  if (0 <= i) {
    removeTransaction (acc, i);
  }
}

/********************************************************************\
\********************************************************************/

int
insertTransaction( Account *acc, Transaction *trans )
  {
  int position=-1;
  int  i,j;
  int  inserted = False;
  Transaction **oldTrans;

  if (NULL == acc) {
    printf ("Internal Error: insertTransaction(): \n");
    printf (" no account specified ! \n");
    return -1;
  }
    
  /*  
   * If the transaction hasn't already been marked as a debit
   * or a credit to this account, then provide a default
   * behavior for double-entry insertion.
   *
   * If this appears to be a new transaction, then default
   * it to being a credit.  If this transaction is already
   * in another account, assume this is the other half. 
   */
  
  if ( (acc != (Account *) trans->credit) &&
       (acc != (Account *) trans->debit) ) {

    if (NULL == trans->credit) {
      trans->credit = (struct _account *) acc;
    } else 
    if (NULL == trans->debit) {
      trans->debit = (struct _account *) acc;
    } else
    {
      printf ("Internal Error: insertTransaction(): \n");
      printf ("can't insert a transaction more than twice! \n");
      printf ("This error should not occur, please report it \n");
    }
  }

  if (trans->debit == trans->credit) {
    printf ("Internal Error: insertTransaction(): \n");
    printf ("debited and credit accounts cannot be the same\n");
    return -1;
  }

  /* mark the data file as needing to be saved: */
  if( acc->parent != NULL ) acc->parent->saved = False;
  
  acc->numTrans++;
  oldTrans = acc->transaction;
  acc->transaction = (Transaction **)_malloc(((acc->numTrans) + 1) *
                                             sizeof(Transaction *));
  
  /* dt is the date of the transaction we are inserting, and dj
   * is the date of the "cursor" transaction... we want to insert
   * the new transaction before the first transaction of the same
   * or later date.  The !inserted bit is a bit of a kludge to 
   * make sure we only insert the new transaction once! */
  for( i=0,j=0; i<acc->numTrans; i++,j++ )
    {
    /* if we didn't do this, and we needed to insert into the
     * last spot in the array, we would walk off the end of the
     * old array, which is no good! */
    if( j>=(acc->numTrans-1) )
      {
      position = i;
      acc->transaction[i] = trans;
      break;
      }
    else
      {
      if( (xaccTransOrder (&(oldTrans[j]),&trans) > 0) && !inserted )
        {
        position = i;
        acc->transaction[i] = trans;
        j--;
        inserted = True;
        }
      else
        acc->transaction[i] = oldTrans[j];
      }
    }
  
  /* make sure the array is NULL terminated */
  acc->transaction[acc->numTrans] = NULL;

  _free(oldTrans);

  if( position != -1 )
    qfInsertTransaction( acc->qfRoot, trans );
  
  return position;
}

/********************************************************************\
\********************************************************************/

Account *
xaccGetOtherAccount( Account *acc, Transaction *trans )
{
  if (NULL == acc) return NULL;

  if (acc == ((Account *) trans->debit)) {
     return ((Account *) trans->credit);
  } else
  if (acc == ((Account *) trans->credit)) {
     return ((Account *) trans->debit);
  } else {
     printf ("Internal Error: xaccGetOtherAccount(): inconsistent entry \n");
  }

  return NULL;
}

/********************************************************************\
\********************************************************************/

double xaccGetAmount (Account *acc, Transaction *trans)
{
   double themount; /* amount */

   themount = xaccGetShareAmount (acc, trans);
   themount *= trans->share_price;
   return themount;
}
    
/********************************************************************\
\********************************************************************/

double xaccGetShareAmount (Account *acc, Transaction *trans)
{
   double themount; /* amount */
   if (NULL == trans) return 0.0;
   if (NULL == acc) return 0.0;
      
   /* for a double-entry, determine if this is a credit or a debit */
   if ( trans->credit == ((struct _account *) acc) ) {
      themount = trans->damount;
   } else 
   if ( trans->debit == ((struct _account *) acc) ) {
      themount = - (trans->damount);
   } else {
      printf ("Internal Error: xaccGetShareAmount: missing double entry \n");
      printf ("this error should not occur. Please report the problem. \n");
      printf ("acc=%p deb=%p cred=%p\n", acc, trans->debit, trans->credit);
      themount = 0.0;  /* punt */
   }
   return themount;
}
    
/********************************************************************\
\********************************************************************/

void xaccSetShareAmount (Account *acc, Transaction *trans, double themount)
{
   /* for a double-entry, determine if this is a credit or a debit */
   if ( trans->credit == ((struct _account *) acc) ) {
      trans->damount = themount;
   } else 
   if ( trans->debit == ((struct _account *) acc) ) {
      trans->damount = - themount;
   } else {
      printf ("Internal Error: xaccSetShareAmount: missing double entry \n");
      printf ("this error should not occur. Please report the problem. \n");
      trans->damount = 0.0; /* punt */
   }
}

/********************************************************************\
\********************************************************************/

void xaccSetAmount (Account *acc, Transaction *trans, double themount)
{
   if (0.0 < trans->share_price) {
     themount /= trans->share_price;
     xaccSetShareAmount (acc, trans, themount);
  }
}

/********************************************************************\
\********************************************************************/

double xaccGetBalance (Account *acc, Transaction *trans)
{
   double themount; /* amount */
      
   /* for a double-entry, determine if this is a credit or a debit */
   if ( trans->credit == ((struct _account *) acc) ) {
      themount = trans->credit_balance;
   } else 
   if ( trans->debit == ((struct _account *) acc) ) {
      themount = trans->debit_balance;
   } else {
      printf ("Internal Error: xaccGetBalance: missing double entry \n");
      printf ("this error should not occur. Please report the problem. \n");
      themount = 0.0;  /* punt */
   }
   return themount;
}
    
/********************************************************************\
\********************************************************************/

double xaccGetClearedBalance (Account *acc, Transaction *trans)
{
   double themount; /* amount */
      
   /* for a double-entry, determine if this is a credit or a debit */
   if ( trans->credit == ((struct _account *) acc) ) {
      themount = trans->credit_cleared_balance;
   } else 
   if ( trans->debit == ((struct _account *) acc) ) {
      themount = trans->debit_cleared_balance;
   } else {
      printf ("Internal Error: xaccGetClearedBalance: missing double entry \n");
      printf ("this error should not occur. Please report the problem. \n");
      themount = 0.0;  /* punt */
   }
   return themount;
}
    
/********************************************************************\
\********************************************************************/

double xaccGetReconciledBalance (Account *acc, Transaction *trans)
{
   double themount; /* amount */
      
   /* for a double-entry, determine if this is a credit or a debit */
   if ( trans->credit == ((struct _account *) acc) ) {
      themount = trans->credit_reconciled_balance;
   } else 
   if ( trans->debit == ((struct _account *) acc) ) {
      themount = trans->debit_reconciled_balance;
   } else {
      printf ("Internal Error: xaccGetReconciledBalance: missing double entry \n");
      printf ("this error should not occur. Please report the problem. \n");
      themount = 0.0;  /* punt */
   }
   return themount;
}
    
/********************************************************************\
\********************************************************************/

double xaccGetShareBalance (Account *acc, Transaction *trans)
{
   double themount; /* amount */
      
   /* for a double-entry, determine if this is a credit or a debit */
   if ( trans->credit == ((struct _account *) acc) ) {
      themount = trans->credit_share_balance;
   } else 
   if ( trans->debit == ((struct _account *) acc) ) {
      themount = trans->debit_share_balance;
   } else {
      printf ("Internal Error: xaccGetShareBalance: missing double entry \n");
      printf ("this error should not occur. Please report the problem. \n");
      themount = 0.0;  /* punt */
   }
   return themount;
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
  int  i; 
  double  dbalance    = 0.0;
  double  dcleared_balance = 0.0;
  double  dreconciled_balance = 0.0;
  double  share_balance    = 0.0;
  double  share_cleared_balance = 0.0;
  double  share_reconciled_balance = 0.0;
  double  amt = 0.0;
  Transaction *trans, *last_trans=NULL;
  Account *tracc;
  
  if( NULL == acc ) return;

  for( i=0; (trans=getTransaction(acc,i)) != NULL; i++ ) {

    /* compute both dollar and share balances */
    amt = xaccGetShareAmount (acc, trans);
    share_balance += amt;
    dbalance += amt * (trans->share_price);
    
    if( NREC != trans->reconciled ) {
      share_cleared_balance += amt;
      dcleared_balance += amt * (trans->share_price);
    }

    if( YREC == trans->reconciled ) {
      share_reconciled_balance += amt;
      dreconciled_balance += amt * (trans->share_price);
    }

    tracc = (Account *) trans->credit;
    if (tracc == acc) {
      /* For bank accounts, the invarient subtotal is the dollar
       * amount.  For stock accoounts, the invarient is the share amount */
      if ( (STOCK == tracc->type) || ( MUTUAL == tracc->type) ) {
        trans -> credit_share_balance = share_balance;
        trans -> credit_share_cleared_balance = share_cleared_balance;
        trans -> credit_share_reconciled_balance = share_reconciled_balance;
        trans -> credit_balance = trans->share_price * share_balance;
        trans -> credit_cleared_balance = trans->share_price * share_cleared_balance;
        trans -> credit_reconciled_balance = trans->share_price * share_reconciled_balance;
      } else {
        trans -> credit_share_balance = dbalance;
        trans -> credit_share_cleared_balance = dcleared_balance;
        trans -> credit_share_reconciled_balance = dreconciled_balance;
        trans -> credit_balance = dbalance;
        trans -> credit_cleared_balance = dcleared_balance;
        trans -> credit_reconciled_balance = dreconciled_balance;
      }
    }
    tracc = (Account *) trans->debit;
    if (tracc == acc) {
      if ( (STOCK == tracc->type) || ( MUTUAL == tracc->type) ) {
        trans -> debit_share_balance = share_balance;
        trans -> debit_share_cleared_balance = share_cleared_balance;
        trans -> debit_share_reconciled_balance = share_reconciled_balance;
        trans -> debit_balance = trans->share_price * share_balance;
        trans -> debit_cleared_balance = trans->share_price * share_cleared_balance;
        trans -> debit_reconciled_balance = trans->share_price * share_reconciled_balance;
      } else {
        trans -> debit_share_balance = dbalance;
        trans -> debit_share_cleared_balance = dcleared_balance;
        trans -> debit_share_reconciled_balance = dreconciled_balance;
        trans -> debit_balance = dbalance;
        trans -> debit_cleared_balance = dcleared_balance;
        trans -> debit_reconciled_balance = dreconciled_balance;
      }
    }

    last_trans = trans;
  }

  if ( (STOCK == acc->type) || ( MUTUAL == acc->type) ) {
    if (last_trans) {
       acc -> balance = share_balance * (last_trans->share_price);
       acc -> cleared_balance = share_cleared_balance * (last_trans->share_price);
       acc -> reconciled_balance = share_reconciled_balance * (last_trans->share_price);
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
xaccCheckDateOrder (Account * acc, Transaction *trans )
{
  int outOfOrder = 0;
  Transaction *prevTrans;
  Transaction *nextTrans;
  int position;

  if (NULL == acc) return 0;

  position = getNumOfTransaction (acc, trans);
  if (-1 == position) {
    printf ("Internal Error: xaccCheckDateOrder(): \n");
    printf ("transaction not present in the account !\n");
    return 0;
  }

  prevTrans = getTransaction( acc, position-1 );
  nextTrans = getTransaction( acc, position+1 );

  /* figure out if the transactions are out of order */
  if (NULL != prevTrans) {
    if( xaccTransOrder (&prevTrans, &trans) >0 ) outOfOrder = True;
  }
  if (NULL != nextTrans) {
    if( xaccTransOrder (&trans, &nextTrans) >0 ) outOfOrder = True;
  }

  /* take care of re-ordering, if necessary */
  if( outOfOrder ) {
    removeTransaction( acc, position );
    insertTransaction( acc, trans );
    return 1;
  }
  return 0;
}

/********************************************************************\
 * xaccCheckDateOrderDE                                             *
 *   check this transaction to see if the date is in correct order  *
 *   If it is not, reorder the transactions ...                     *
 *   This routine perfroms the check for both of the double-entry   *
 *   transaction entries ...                                        *
 *                                                                  *
 * Args:   trans -- the transaction to check                        *
 * Return: int -- non-zero if out of order                          *
\********************************************************************/

int
xaccCheckDateOrderDE (Transaction *trans )
{
  Account * acc;
  int outOfOrder = 0;

  if (NULL == trans) return 0;

  acc = (Account *) (trans->credit);
  outOfOrder += xaccCheckDateOrder (acc, trans);
  acc = (Account *) (trans->debit);
  outOfOrder += xaccCheckDateOrder (acc, trans);

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
   Transaction *ta, *tb;
   int i,j,k;

   if (!acc) return;

   for (i=0; i<acc->numTrans; i++) {
      ta = acc->transaction[i];
      for (j=i+1; j<acc->numTrans; j++) {
         tb = acc->transaction[j];

         /* if no match, then continue on in the loop.
          * we really must match everything to get a duplicate */
         if (ta->credit != tb->credit) continue;
         if (ta->debit != tb->debit) continue;
         if (ta->reconciled != tb->reconciled) continue;
         if (ta->date.year != tb->date.year) continue;
         if (ta->date.month != tb->date.month) continue;
         if (ta->date.day != tb->date.day) continue;
         if (strcmp (ta->num, tb->num)) continue;
         if (strcmp (ta->description, tb->description)) continue;
         if (strcmp (ta->memo, tb->memo)) continue;
         if (strcmp (ta->action, tb->action)) continue;
         if (0 == DEQ(ta->damount, tb->damount)) continue;
         if (0 == DEQ(ta->share_price, tb->share_price)) continue;

         /* if we got to here, then there must be a duplicate. */
         /* before deleting it, remove it from the other 
          * double-entry account */
         if (acc == (Account *) tb->credit) {
            xaccRemoveTransaction ((Account *) tb->debit, tb);
         }
         if (acc == (Account *) tb->debit) {
            xaccRemoveTransaction ((Account *) tb->credit, tb);
         }
         tb->credit = NULL;
         tb->debit = NULL;

         /* Free the transaction, and shuffle down by one.
          * Need to shuffle in order to preserve date ordering. */
         freeTransaction (tb);
         
         for (k=j+1; k<acc->numTrans; k++) {
            acc->transaction[k-1] = acc->transaction[k];
         }
         acc->transaction[acc->numTrans -1] = NULL;
         acc->numTrans --;
      }
   }
}

/*************************** END OF FILE **************************** */
