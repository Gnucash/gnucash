/********************************************************************\
 * Group.c -- the main data structure of the program                *
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
#include "AccountP.h"
#include "Group.h"
#include "GroupP.h"
#include "TransactionP.h"
#include "util.h"

#ifndef FALSE
#define FALSE 0
#endif

#ifndef TRUE
#define TRUE 1
#endif

/********************************************************************\
 * Because I can't use C++ for this project, doesn't mean that I    *
 * can't pretend too!  These functions perform actions on the       *
 * AccountGroup data structure, in order to encapsulate the         *
 * knowledge of the internals of the AccountGroup in one file.      *
\********************************************************************/

AccountGroup *topgroup = 0x0;

/********************************************************************\
\********************************************************************/
void
xaccInitializeAccountGroup (AccountGroup *grp)
  {
  grp->saved       = TRUE;
  grp->new         = FALSE;
  
  grp->parent      = NULL;
  grp->numAcc      = 0;
  grp->account     = NULL;
  
  grp->balance     = 0.0;
  }

/********************************************************************\
\********************************************************************/
AccountGroup *
xaccMallocAccountGroup( void )
  {
  AccountGroup *grp = (AccountGroup *)_malloc(sizeof(AccountGroup));
  
  xaccInitializeAccountGroup (grp);

  return grp;
  }

/********************************************************************\
\********************************************************************/
void
xaccFreeAccountGroup( AccountGroup *grp )
{
  int i;
  if (NULL == grp) return;

  if (grp == topgroup) topgroup = NULL;  /* safety device */

  for( i=0; i<grp->numAcc; i++ ) {
    xaccFreeAccount( grp->account[i] );
  }
    
  _free( grp->account );

  /* null everything out, just in case somebody 
   * tries to traverse freed memory */
  xaccInitializeAccountGroup (grp);

  _free(grp);
}

/********************************************************************\
\********************************************************************/
void
xaccAccountGroupMarkSaved (AccountGroup *grp)
{
   int i;

   if (!grp) return;
   grp->saved = TRUE;

   for (i=0; i<grp->numAcc; i++) {
      xaccAccountGroupMarkSaved (grp->account[i]->children); 
   }
}

/********************************************************************\
\********************************************************************/
int
xaccAccountGroupNotSaved (AccountGroup *grp)
{
   int not_saved;
   int i;

   if (!grp) return 0;
   if (FALSE == grp->saved) return 1;

   for (i=0; i<grp->numAcc; i++) {
      not_saved = xaccAccountGroupNotSaved (grp->account[i]->children); 
      if (not_saved) return 1;
   }
   return 0;
}

/********************************************************************\
 * Get the number of accounts, including subaccounts                *
\********************************************************************/

int
xaccGetNumAccounts ( AccountGroup *root )
{
  int num_acc = 0;
  int i;

  if (NULL == root) return 0;

  num_acc = root->numAcc;

  for (i=0; i<root->numAcc; i++) {
    num_acc += xaccGetNumAccounts (root->account[i]->children);
  }

  return num_acc;
}

/********************************************************************\
 * Fetch an account, given only it's ID number                      *
\********************************************************************/

Account *
xaccGetAccountFromID ( AccountGroup *root, int acc_id )
{
  Account *acc;
  int i;

  if (NULL == root) return NULL;
  if (0 > acc_id) return NULL;

  /* first, look for accounts hanging off the root */
  for (i=0; i<root->numAcc; i++) {
    acc = root->account[i];
    if (acc_id == acc->id) return acc;
  }

  /* if we are still here, then we haven't found the account yet.
   * Recursively search the subgroups next */
  for (i=0; i<root->numAcc; i++) {
    acc = xaccGetAccountFromID (root->account[i]->children, acc_id);
    if (acc) return acc;
  }

  return NULL;
}

/********************************************************************\
 * Fetch the root of the tree                                       *
\********************************************************************/

AccountGroup *
xaccGetAccountRoot (Account * acc) 
{
  Account *parent_acc;
  AccountGroup * grp;
  AccountGroup * root = NULL;

  if (!acc) return NULL;

  /* find the root of the account group structure */
  grp = (AccountGroup *) acc->parent;
  while (grp) {
    root = grp;
    parent_acc = grp -> parent;
    grp = NULL;
    if (parent_acc) {
       grp = (AccountGroup *) parent_acc->parent;
    }
  }
  return root;
}

/********************************************************************\
 * Fetch an account, given only it's ID number                      *
\********************************************************************/

Account *
xaccGetPeerAccountFromID ( Account *acc, int acc_id )
{
  AccountGroup * root;
  Account *peer_acc;

  if (NULL == acc) return NULL;
  if (-1 >= acc_id) return NULL;

  /* first, find the root of the account group structure */
  root = xaccGetAccountRoot (acc);

  /* now search all acounts hanging off the root */
  peer_acc = xaccGetAccountFromID (root, acc_id);

  return peer_acc;
}

/********************************************************************\
 * Fetch an account, given it's name                                *
\********************************************************************/

Account *
xaccGetAccountFromName ( AccountGroup *root, const char * name )
{
  Account *acc;
  int i;

  if (NULL == root) return NULL;
  if (NULL == name) return NULL;

  /* first, look for accounts hanging off the root */
  for (i=0; i<root->numAcc; i++) {
    acc = root->account[i];
    if (!strcmp(acc->accountName, name)) return acc;
  }

  /* if we are still here, then we haven't found the account yet.
   * Recursively search the subgroups next */
  for (i=0; i<root->numAcc; i++) {
    acc = xaccGetAccountFromName (root->account[i]->children, name);
    if (acc) return acc;
  }

  return NULL;
}

/********************************************************************\
 * Fetch an account, given it's name                                *
\********************************************************************/

Account *
xaccGetPeerAccountFromName ( Account *acc, const char * name )
{
  AccountGroup * root;
  Account *peer_acc;

  if (NULL == acc) return NULL;
  if (NULL == name) return NULL;

  /* first, find the root of the account group structure */
  root = xaccGetAccountRoot (acc);

  /* now search all acounts hanging off the root */
  peer_acc = xaccGetAccountFromName (root, name);

  return peer_acc;
}

/********************************************************************\
\********************************************************************/
Account *
removeAccount( AccountGroup *grp, int num )
  {
  Account *acc = NULL;
  
  if( NULL != grp )
    {
    int i,j;
    Account **oldAcc = grp->account;

    grp->saved = FALSE;
    
    grp->numAcc--;
    grp->account = (Account **)_malloc((grp->numAcc)*sizeof(Account *));
    
    acc = oldAcc[grp->numAcc];    /* In case we are deleting last in
				    * old array */
    for( i=0,j=0; i<grp->numAcc; i++,j++ )
      {
      if( j != num )
        grp->account[i] = oldAcc[j];
      else
        {
        acc = oldAcc[j];
        i--;
        }
      }
    
    _free(oldAcc);
    }
  return acc;
  }

/********************************************************************\
\********************************************************************/

void
xaccRemoveGroup (AccountGroup *grp)
{
   Account *acc;

   if (NULL == grp) return;
   acc = grp->parent;

   /* if this group has no parent, it must be the topgroup */
   if (NULL == acc) return;

   acc->children = NULL;

   /* make sure that the parent of the group is marked 
    * as having been modified. */
   grp = acc -> parent;
   if (!grp) return;

   grp->saved = FALSE;
}

/********************************************************************\
\********************************************************************/

void
xaccRemoveAccount (Account *acc)
{
   int i,j;
   AccountGroup *grp;
   Account **oldAcc;

   if (NULL == acc) return;
   grp = acc->parent;
   acc->parent = NULL;

   /* this routine might be called on accounts which 
    * are not yet parented. */
   if (NULL == grp) return;

   oldAcc = grp->account;

   grp->saved = FALSE;
    
   grp->numAcc--;

   if (0 < grp->numAcc) {
      grp->account = (Account **)_malloc((grp->numAcc)*sizeof(Account *));
       
      for( i=0,j=0; i<grp->numAcc; i++,j++ ) {
         if( acc != oldAcc[j] ) {
            grp->account[i] = oldAcc[j];
         } else {
            i--;
         }
      }
   } else {
      grp->account = NULL;

      /* if this was the last account in a group, delete
       * the group as well (unless its a root group) */
      if (grp->parent) {
         xaccRemoveGroup (grp);
         xaccFreeAccountGroup (grp);
       }
    }
    
   _free(oldAcc);
}

/********************************************************************\
\********************************************************************/
int
xaccInsertSubAccount( Account *adult, Account *child )
{
  int retval;

  if (NULL == adult) return -1;

  /* if a container for the children doesn't yet exist, add it */
  if (NULL == adult->children) {
    adult->children = xaccMallocAccountGroup();
  }

  /* set back-pointer to parent */
  adult->children->parent = adult;

  /* allow side-effect of creating a child-less account group */
  if (NULL == child) return -1;

  retval = insertAccount (adult->children, child);
  return retval;
}

/********************************************************************\
\********************************************************************/
int
insertAccount( AccountGroup *grp, Account *acc )
  {
  int i=-1;
  Account **oldAcc;
  
  if (NULL == grp) return -1;
  if (NULL == acc) return -1;

  /* set back-pointer to the accounts parent */
  acc->parent = (struct _account_group *) grp;

  oldAcc = grp->account;
    
  grp->saved = FALSE;
  
  grp->numAcc++;
  grp->account = (Account **)_malloc((grp->numAcc)*sizeof(Account *));

  if (1 < grp->numAcc) {
    for( i=0; i<(grp->numAcc-1); i++ ) {
      grp->account[i] = oldAcc[i];
    }
    _free(oldAcc);
  } else {
    i = 0;
  }
  grp->account[i] = acc;

  return i;
  }

/********************************************************************\
\********************************************************************/
void
xaccRecomputeGroupBalance (AccountGroup *grp)
{
   int i;
   Account *acc;

   if (!grp) return;

   grp->balance = 0.0;
   for (i=0; i<grp->numAcc; i++) {
      acc = grp->account[i];

      /* first, get subtotals recursively */
      if (acc->children) {
         xaccRecomputeGroupBalance (acc->children);
         grp->balance += acc->children->balance;
      }

      /* then add up accounts in this group */
      xaccAccountRecomputeBalance (acc);
      grp->balance += acc->balance;
   }
}

/********************************************************************\
\********************************************************************/

void 
xaccConcatGroups (AccountGroup *togrp, AccountGroup *fromgrp)
{
   Account * acc;
   int i;

   if (!togrp) return;
   if (!fromgrp) return;
   
   for (i=0; i<fromgrp->numAcc; i++) {
      acc = fromgrp->account[i];
      insertAccount (togrp, acc);
      fromgrp->account[i] = NULL;
   }
   _free (fromgrp->account);
   fromgrp->account = NULL;
   fromgrp->numAcc = 0;
}

/********************************************************************\
\********************************************************************/

void 
xaccMergeAccounts (AccountGroup *grp)
{
   Account *acc_a, *acc_b;
   int i,j, k;

   if (!grp) return;
   
   for (i=0; i<grp->numAcc; i++) {
      acc_a = grp->account[i];
      for (j=i+1; j<grp->numAcc; j++) {
         acc_b = grp->account[j];
         if ((0 == strcmp(acc_a->accountName, acc_b->accountName)) &&
             (0 == strcmp(acc_a->description, acc_b->description)) &&
             (0 == strcmp(acc_a->notes, acc_b->notes)) &&
             (acc_a->type == acc_b->type)) {

            AccountGroup *ga, *gb;

            /* consolidate children */
            ga = (AccountGroup *) acc_a->children;
            gb = (AccountGroup *) acc_b->children;
            if (gb) {
               if (!ga) {
                  acc_a->children = (struct _account_group *) gb;
                  gb->parent = acc_a;
                  acc_b->children = NULL;
               } else {
                  xaccConcatGroups (ga, gb);
                  xaccFreeAccountGroup (gb);
                  acc_b->children = NULL;
               }
            }

            /* recurse to do the children's children */
            xaccMergeAccounts (ga);

            /* consolidate transactions */
            for (k=0; k<acc_b->numSplits; k++) {
               Split *split;
               split = acc_b->splits[k];
               acc_b->splits[k] = NULL;
               if (acc_b == (Account *) split->acc) 
                   split->acc = (struct _account *) acc_a;
               xaccAccountInsertSplit (acc_a, split);
            }

            /* free the account structure itself */
            acc_b->numSplits = 0;
            xaccFreeAccount (acc_b);
            grp->account[j] = grp->account[grp->numAcc -1];
            grp->account[grp->numAcc -1] = NULL;
            grp->numAcc --;
         }
      }
   }
}

/********************************************************************\
\********************************************************************/

void 
xaccConsolidateGrpTransactions (AccountGroup *grp)
{
   Account * acc;
   int i;

   if (!grp) return;
   
   for (i=0; i<grp->numAcc; i++) {
      acc = grp->account[i];
      xaccConsolidateTransactions (acc);

      /* recursively do the children */
      xaccConsolidateGrpTransactions (acc->children);
   }
}

/********************************************************************\
\********************************************************************/

int     
xaccGroupGetNumAccounts (AccountGroup *grp)
{
   return (grp->numAcc);
}

Account *
xaccGroupGetAccount (AccountGroup *grp, int i)
{
   if (!grp) return NULL;
   if (!(grp->account)) return NULL;
   if((0>i) || (i >= grp->numAcc)) return NULL;
   return (grp->account[i]);
}

double
xaccGroupGetBalance (AccountGroup * grp)
{
   return (grp->balance);
}

/****************** END OF FILE *************************************/
