/********************************************************************\
 * Group.c -- the main data structure of the program                *
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

/********************************************************************\
\********************************************************************/
void
xaccInitializeAccountGroup (AccountGroup *grp)
  {
  grp->saved       = TRUE;
  grp->new         = FALSE;
  
  grp->parent      = NULL;
  grp->numAcc      = 0;
  grp->account     = _malloc (sizeof (Account *));
  grp->account[0]  = NULL;   /* null-terminated array */
  
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

  for( i=0; i<grp->numAcc; i++ ) {
    xaccFreeAccount( grp->account[i] );
  }
    
  _free( grp->account );

  /* null everything out, just in case somebody 
   * tries to traverse freed memory */
  grp->parent      = NULL;
  grp->numAcc      = 0;
  grp->account     = NULL;
  grp->balance     = 0.0;

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
    if (!safe_strcmp(acc->accountName, name)) return acc;
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
   int i,j, nacc;
   AccountGroup *grp;
   Account **arr;

   if (NULL == acc) return;
   grp = acc->parent;
   acc->parent = NULL;

   /* this routine might be called on accounts which 
    * are not yet parented. */
   if (NULL == grp) return;

   nacc = grp->numAcc;
   assert (nacc);

   arr = grp->account;

   for( i=0,j=0; j<nacc; i++,j++ ) {
      arr[i] = arr[j];
      if( acc == arr[j] ) { i--; }
   }
   nacc --;
   arr[nacc] = NULL;
   grp->numAcc = nacc;
   grp->saved = FALSE;

   /* if this was the last account in a group, delete
    * the group as well (unless its a root group) */
   if ((0 == nacc) && (grp->parent)) {
      xaccRemoveGroup (grp);
      xaccFreeAccountGroup (grp);
   }
}

/********************************************************************\
\********************************************************************/

void
xaccInsertSubAccount( Account *adult, Account *child )
{
  if (NULL == adult) return;

  /* if a container for the children doesn't yet exist, add it */
  if (NULL == adult->children) {
    adult->children = xaccMallocAccountGroup();
  }

  /* set back-pointer to parent */
  adult->children->parent = adult;

  /* allow side-effect of creating a child-less account group */
  if (NULL == child) return;

  xaccGroupInsertAccount (adult->children, child);
}

/********************************************************************\
\********************************************************************/

void
xaccGroupInsertAccount( AccountGroup *grp, Account *acc )
{
  int i,nacc;
  Account **arr;
  int ralo = 1;
  
  if (NULL == grp) return;
  if (NULL == acc) return;

  /* If the account is currently in another group, remove it there first.
   * Basically, we can't have accounts being in two places at once. 
   * If old and new parents are the same, reinsertion causes the sort order
   * to be checked.
   */
  if (acc->parent) {
    if (grp == acc->parent) ralo = 0;
    xaccRemoveAccount (acc);
  }
  grp->saved = FALSE;

  /* set back-pointer to the accounts parent */
  acc->parent = grp;

  nacc = grp->numAcc;
  arr = grp->account;
  if (ralo) {
     arr = (Account **) realloc (arr, (nacc+2)*sizeof(Account *));
  }

  /* insert account in proper sort order */
  for (i=nacc; i>=0; i--) {
    if ((0<i) && (0 < xaccAccountOrder (&(arr[i-1]), &acc))) {
       arr[i] = arr[i-1];
    } else {
       arr[i] = acc;
       break;
    }
  }

  nacc++;
  arr[nacc] = NULL;
  grp->account = arr;
  grp->numAcc = nacc;
}

/********************************************************************\
\********************************************************************/

void
xaccRecomputeGroupBalance (AccountGroup *grp)
{
   int i;
   Account *acc;
   char * default_currency;

   if (!grp) return;
   if (!(grp->account)) return;

   acc = grp->account[0];
   if (!acc) return;
   default_currency = acc->currency;

   grp->balance = 0.0;
   for (i=0; i<grp->numAcc; i++) {
      acc = grp->account[i];

      /* first, get subtotals recursively */
      if (acc->children) {
         xaccRecomputeGroupBalance (acc->children);

         if (!safe_strcmp (default_currency, acc->currency)) {
            grp->balance += acc->children->balance;
         }
      }

      /* then add up accounts in this group */
      xaccAccountRecomputeBalance (acc);
      if (!safe_strcmp (default_currency, acc->currency)) {
         grp->balance += acc->balance;
      }
   }
}

/********************************************************************\
\********************************************************************/
/* account codes will be assigned base-36, with three digits */

#define BASE 36

char *
xaccGroupGetNextFreeCode (AccountGroup *grp, int digits)
{
  Account *acc;
  int i, maxcode = 0;
  char * retval;

  if (!grp) return NULL;

  /* count levels to top */
  acc = grp->parent;
  while (acc) {
    digits --;
    assert (acc->parent);
    acc = acc->parent->parent;
  }

  /* if (0>digits)  we could insert a decimal place, but I am too lazy
   * to write this code.  It doesn't seem important at the moment ... */

  /* find the largest used code */
  acc = grp->parent;
  if (acc) {
     if (acc->accountCode) {
        maxcode = strtol (acc->accountCode, NULL, BASE);
     }
  }
  for (i=0; i<grp->numAcc; i++) {
     Account *acnt = grp->account[i];
     if (acnt->accountCode) {
        int code = strtol (acnt->accountCode, NULL, BASE);
        if (code > maxcode) maxcode = code;
     }
  }

  /* right-shift */
  for (i=1; i<digits; i++) {
     maxcode /= BASE;
  }
  maxcode ++;

  /* left-shift */
  for (i=1; i<digits; i++) {
     maxcode *= BASE;
  }

  /* print */
  retval = ultostr ((unsigned long) maxcode, BASE);
  return retval;
}

/********************************************************************\
\********************************************************************/
/* almost identical code to above, but altered to deal with 
 * specified account */

char *
xaccAccountGetNextChildCode (Account *parent_acc, int digits)
{
  Account *acc;
  int i, maxcode = 0;
  char * retval;
  AccountGroup *grp;

  if (!parent_acc) return NULL;

  /* count levels to top */
  acc = parent_acc;
  while (acc) {
    digits --;
    assert (acc->parent);   /* all acounts must be in a group */
    acc = acc->parent->parent;
  }

  /* if (0>digits)  we could insert a decimal place, but I am too lazy
   * to write this code.  It doesn't seem important at the moment ... */

  /* find the largest used code */
  acc = parent_acc;
  if (acc) {
     if (acc->accountCode) {
        maxcode = strtol (acc->accountCode, NULL, BASE);
     }
  }
  grp = parent_acc->children;
  if (grp) {
     for (i=0; i<grp->numAcc; i++) {
        Account *acnt = grp->account[i];
        if (acnt->accountCode) {
           int code = strtol (acnt->accountCode, NULL, BASE);
           if (code > maxcode) maxcode = code;
        }
     }
  }

  /* right-shift */
  for (i=1; i<digits; i++) {
     maxcode /= BASE;
  }
  maxcode ++;

  /* left-shift */
  for (i=1; i<digits; i++) {
     maxcode *= BASE;
  }

  /* print */
  retval = ultostr ((unsigned long) maxcode, BASE);
  return retval;
}

/********************************************************************\
\********************************************************************/

void
xaccGroupDepthAutoCode (AccountGroup *grp)
{
   int depth;
   if (!grp) return;

   /* get the depth */
   depth = xaccGroupGetDepth (grp);
   if (3>depth) depth = 3;

   xaccGroupAutoCode (grp, depth);
} 

void
xaccGroupAutoCode (AccountGroup *grp, int depth)
{
   int i;
   if (!grp || (0>depth)) return;

   for (i=0; i<grp->numAcc; i++) {
      Account *acc = grp->account[i];
      xaccAccountAutoCode (acc, depth);
      xaccGroupAutoCode (acc->children, depth);
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
      xaccGroupInsertAccount (togrp, acc);
      fromgrp->account[i] = NULL;
   }
   fromgrp->account[0] = NULL;
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
         if ((0 == safe_strcmp(acc_a->accountName, acc_b->accountName)) &&
             (0 == safe_strcmp(acc_a->accountCode, acc_b->accountCode)) &&
             (0 == safe_strcmp(acc_a->description, acc_b->description)) &&
             (0 == safe_strcmp(acc_a->currency, acc_b->currency)) &&
             (0 == safe_strcmp(acc_a->security, acc_b->security)) &&
             (0 == safe_strcmp(acc_a->notes, acc_b->notes)) &&
             (acc_a->type == acc_b->type)) {

            AccountGroup *ga, *gb;

            /* consolidate children */
            ga = (AccountGroup *) acc_a->children;
            gb = (AccountGroup *) acc_b->children;
            if (gb) {
               if (!ga) {
                  acc_a->children = gb;
                  gb->parent = acc_a;
                  acc_b->children = NULL;
               } else {
                  xaccConcatGroups (ga, gb);
                  acc_b->children = NULL;
                  xaccFreeAccountGroup (gb);
               }
            }

            /* recurse to do the children's children */
            xaccMergeAccounts (ga);

            /* consolidate transactions */
            for (k=0; k<acc_b->numSplits; k++) {
               Split *split;
               split = acc_b->splits[k];
               acc_b->splits[k] = NULL;
               split->acc = NULL;
               xaccAccountInsertSplit (acc_a, split);
            }

            /* free the account structure itself */
            acc_b->numSplits = 0;
            xaccFreeAccount (acc_b);
            grp->account[j] = grp->account[grp->numAcc -1];
            grp->account[grp->numAcc -1] = NULL;
            grp->numAcc --;
            break;
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
   if (!grp) return 0;
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
   if (!grp) return 0.0;
   return (grp->balance);
}

/********************************************************************\
\********************************************************************/

int     
xaccGroupGetDepth (AccountGroup *grp)
{
   int i, depth=0, maxdepth=0;
   if (!grp) return 0;

   for (i=0; i<grp->numAcc; i++) {
      depth = xaccGroupGetDepth (grp->account[i]->children);
      if (depth > maxdepth) maxdepth = depth;
   }

   maxdepth++;
   return maxdepth;
}

/****************** END OF FILE *************************************/
