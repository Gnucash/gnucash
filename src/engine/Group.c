/********************************************************************\
 * Group.c -- the main data structure of the program                *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1997-2000 Linas Vepstas <linas@linas.org>          *
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
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#include "config.h"

#include <assert.h>
#include <string.h>

#include "Account.h"
#include "AccountP.h"
#include "BackendP.h"
#include "GNCIdP.h"
#include "Group.h"
#include "GroupP.h"
#include "TransactionP.h"
#include "gnc-common.h"
#include "gnc-engine-util.h"
#include "gnc-numeric.h"

/* static short module = MOD_ENGINE; */

/********************************************************************\
 * Because I can't use C++ for this project, doesn't mean that I    *
 * can't pretend to!  These functions perform actions on the        *
 * AccountGroup data structure, in order to encapsulate the         *
 * knowledge of the internals of the AccountGroup in one file.      *
\********************************************************************/

/********************************************************************\
\********************************************************************/

static void
xaccInitializeAccountGroup (AccountGroup *grp)
{
  grp->saved       = TRUE;

  grp->parent      = NULL;
  grp->numAcc      = 0;
  grp->account     = g_malloc (sizeof(Account *));
  grp->account[0]  = NULL;   /* null-terminated array */

  grp->balance     = gnc_numeric_zero();

  grp->backend     = NULL;

}

/********************************************************************\
\********************************************************************/

AccountGroup *
xaccMallocAccountGroup( void )
{
  AccountGroup *grp = g_new(AccountGroup, 1);

  xaccInitializeAccountGroup (grp);

  return grp;
}

/********************************************************************\
\********************************************************************/

gboolean
xaccGroupEqual(AccountGroup *ga,
               AccountGroup *gb,
               gboolean check_guids) {
  Account **accs_ga;
  Account **accs_gb;

  if(!ga && !gb) return(TRUE);
  if(!ga) return(FALSE);
  if(!gb) return(FALSE);

  accs_ga = ga->account;
  accs_gb = gb->account;

  if(!accs_ga && accs_gb) return(FALSE);
  if(accs_ga && !accs_gb) return(FALSE);

  if(accs_ga && accs_gb) {
    while(*accs_ga && *accs_gb) {
      Account *aa = *accs_ga;
      Account *ab = *accs_gb;
      
      if(!xaccAccountEqual(aa, ab, check_guids)) return(FALSE);
      accs_ga++;
      accs_gb++;
    }
    if(*accs_ga) return(FALSE);
    if(*accs_gb) return(FALSE);
  }

  return(TRUE);
} 

/********************************************************************\
\********************************************************************/

static void
xaccAccountGroupBeginEdit( AccountGroup *grp, int defer )
{
  int i;

  if (NULL == grp) return;

  for(i = 0; i < grp->numAcc; i++ )
  {
    xaccAccountBeginEdit(grp->account[i]);
    xaccAccountGroupBeginEdit (grp->account[i]->children, defer);
  }
}

/********************************************************************\
\********************************************************************/

void
xaccAccountGroupCommitEdit( AccountGroup *grp )
{
  int i;

  if (NULL == grp) return;

  for(i = 0; i < grp->numAcc; i++ )
  {
    xaccAccountCommitEdit(grp->account[i]);
    xaccAccountGroupCommitEdit (grp->account[i]->children);
  }
}

/********************************************************************\
\********************************************************************/

void
xaccFreeAccountGroup( AccountGroup *grp )
{
  int i;

  if (NULL == grp) return;

  xaccAccountGroupBeginEdit (grp, 1);

  for(i = 0; i < grp->numAcc; i++ )
    xaccFreeAccount(grp->account[i]);

  g_free(grp->account);

  /* null everything out, just in case somebody 
   * tries to traverse freed memory */
  grp->parent      = NULL;
  grp->numAcc      = 0;
  grp->account     = NULL;
  grp->balance     = gnc_numeric_zero();

  g_free(grp);
}

/********************************************************************\
\********************************************************************/

void
xaccGroupMarkSaved (AccountGroup *grp)
{
   int i;

   if (!grp) return;
   grp->saved = TRUE;

   for (i=0; i<grp->numAcc; i++) {
      xaccGroupMarkSaved (grp->account[i]->children); 
   }
}

/********************************************************************\
\********************************************************************/

void
xaccGroupMarkNotSaved (AccountGroup *grp)
{
   if (!grp) return;
   grp->saved = FALSE;
}

/********************************************************************\
\********************************************************************/

int
xaccGroupNotSaved (AccountGroup *grp)
{
   int not_saved;
   int i;

   if (!grp) return 0;
   if (FALSE == grp->saved) return 1;

   for (i=0; i<grp->numAcc; i++) {
      not_saved = xaccGroupNotSaved (grp->account[i]->children); 
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
 * Get all of the accounts, including subaccounts                   *
\********************************************************************/

int
xaccFillInAccounts ( AccountGroup *root, Account **arr )
{
  int num_acc = 0;
  int i,j;

  if (!root || !arr) return 0;

  num_acc = root->numAcc;
  for (i=0, j=0; i<num_acc; i++) {
    arr[j] = root->account[i];
    j++;
    j += xaccFillInAccounts (root->account[i]->children, &arr[j]);
  }

  arr[j] = NULL;
  return j;
}

Account ** 
xaccGetAccounts ( AccountGroup *root )
{
  Account **arr;
  int num_acc = 0;
  int num_done;

  if (NULL == root) return NULL;

  num_acc = xaccGetNumAccounts (root);
  arr = (Account **) malloc ((num_acc+1)*sizeof (Account *));

  num_done = xaccFillInAccounts (root, arr);
  assert (num_done == num_acc);

  arr[num_acc] = NULL;
  return arr;
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
  grp = acc->parent;
  while (grp) {
    root = grp;
    parent_acc = grp -> parent;
    grp = NULL;
    if (parent_acc) {
       grp = parent_acc->parent;
    }
  }
  return root;
}

/********************************************************************\
 * Fetch an account, given its name                                *
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
 * Fetch an account, given its full name                            *
\********************************************************************/

Account *
xaccGetAccountFromFullName (AccountGroup *root,
                            const char *name,
                            const char separator)
{
  Account *account;
  Account *found;
  char *p;
  int i;

  if (NULL == root) return NULL;
  if (NULL == name) return NULL;

  p = (char *) name;
  found = NULL;

  while (1)
  {
    /* Look for the first separator. */
    p = strchr(p, separator);

    /* If found, switch it to the null char. */
    if (p != NULL)
      *p = 0;

    /* Now look for that name in the children. */
    for (i = 0; i < root->numAcc; i++) {
      account = root->account[i];
      if (safe_strcmp(account->accountName, name) == 0)
      {
        /* We found an account.
         * If p == NULL, there is nothing left
         * in the name, so just return the account.
         * We don't need to put back the separator,
         * because it was never erased (p == NULL). */
        if (p == NULL)
          return account;

        /* There's stuff left to search for.
         * Search recursively after the separator. */
        found = xaccGetAccountFromFullName(account->children,
                                           p + 1, separator);

        /* If we found the account, break out. */
        if (found != NULL)
          break;
      }
    }

    /* If found != NULL, an account was found. */
    /* If p == NULL, there are no more separators left. */

    /* Put back the separator. */
    if (p != NULL)
      *p = separator;

    /* If we found the account, return it. */
    if (found != NULL)
      return found;

    /* We didn't find the account. If there
     * are no more separators, return NULL. */
    if (p == NULL)
      return NULL;

    /* If we are here, we didn't find anything and there
     * must be more separators. So, continue looking with
     * a longer name, in case there is a name with the
     * separator character in it. */
    p++;
  }
}

/********************************************************************\
 * Fetch an account, given its name                                *
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

  /* now search all accounts hanging off the root */
  peer_acc = xaccGetAccountFromName (root, name);

  return peer_acc;
}

/********************************************************************\
 * Fetch an account, given its full name                            *
\********************************************************************/

Account *
xaccGetPeerAccountFromFullName ( Account *acc, const char * name,
                                 const char separator )
{
  AccountGroup * root;
  Account *peer_acc;

  if (NULL == acc) return NULL;
  if (NULL == name) return NULL;

  /* first, find the root of the account group structure */
  root = xaccGetAccountRoot (acc);

  /* now search all acounts hanging off the root */
  peer_acc = xaccGetAccountFromFullName (root, name, separator);

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

  /* set back-pointer to the account's parent */
  acc->parent = grp;

  nacc = grp->numAcc;
  arr = grp->account;
  if (ralo) {
     arr = g_realloc (arr, (nacc+2)*sizeof(Account *));
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
 * FIXME : this code needs to work differently. 
\********************************************************************/

void
xaccRecomputeGroupBalance (AccountGroup *grp) {
  int i;
  Account *acc;
  const gnc_commodity * default_currency;

  if (!grp) return;
  if (!(grp->account)) return;

  acc = grp->account[0];
  if (!acc) return;
  default_currency = acc->currency;

  grp->balance = gnc_numeric_zero();
  for (i=0; i<grp->numAcc; i++) {
    acc = grp->account[i];

    /* first, get subtotals recursively */
    if (acc->children) {
      xaccRecomputeGroupBalance (acc->children);
      
      if (gnc_commodity_equiv(default_currency, acc->currency)) {
        grp->balance = 
          gnc_numeric_add(grp->balance, acc->children->balance,
                          GNC_DENOM_AUTO, GNC_DENOM_LCD | GNC_RND_NEVER);
      }
    }

    /* then add up accounts in this group */
    xaccAccountRecomputeBalance (acc);
    if (gnc_commodity_equiv(default_currency, acc->currency)) {
      grp->balance = 
        gnc_numeric_add(grp->balance, acc->balance,
                        GNC_DENOM_AUTO, GNC_DENOM_LCD | GNC_RND_NEVER);
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
   int i, n;
   if (!grp || (0>depth)) return;

   n = grp->numAcc;
   for (i=0; i<n; i++) {
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
   int numAcc;

   if (!togrp) return;
   if (!fromgrp) return;

   /* The act of inserting the account into togrp also causes
    * it to automatically be deleted from fromgrp. But use a
    * saved copy of fromgrp's numAcc member since, after the
    * last insertion, fromgrp will be pointing to freed memory.
    */
   numAcc = fromgrp->numAcc;
   while (numAcc) {
      acc = fromgrp->account[0];
      xaccGroupInsertAccount (togrp, acc);
      numAcc--;
   }
}

/********************************************************************\
\********************************************************************/

void 
xaccMergeAccounts (AccountGroup *grp)
{
   Account *acc_a, *acc_b;
   int i, j;
   GList *lp;

   if (!grp) return;

   for (i=0; i<grp->numAcc; i++) {
      acc_a = grp->account[i];
      for (j=i+1; j<grp->numAcc; j++) {
         acc_b = grp->account[j];
         if ((0 == safe_strcmp(xaccAccountGetName(acc_a),
                               xaccAccountGetName(acc_b))) &&
             (0 == safe_strcmp(xaccAccountGetCode(acc_a),
                               xaccAccountGetCode(acc_b))) &&
             (0 == safe_strcmp(xaccAccountGetDescription(acc_a),
                               xaccAccountGetDescription(acc_b))) &&
             (gnc_commodity_equiv(xaccAccountGetCurrency(acc_a),
				  xaccAccountGetCurrency(acc_b))) &&
             (gnc_commodity_equiv(xaccAccountGetSecurity(acc_a),
				  xaccAccountGetSecurity(acc_b))) &&
             (0 == safe_strcmp(xaccAccountGetNotes(acc_a),
                               xaccAccountGetNotes(acc_b))) &&
             (xaccAccountGetType(acc_a) == xaccAccountGetType(acc_b))) {

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
               }
            }

            /* recurse to do the children's children */
            xaccMergeAccounts (ga);

            /* consolidate transactions */
            lp = acc_b->splits;

            for(lp = acc_b->splits; lp; lp = lp->next) {
              Split *split = (Split *) lp->data;
              split->acc = NULL;
              xaccAccountInsertSplit (acc_a, split);
            }
            
            g_list_free(acc_b->splits);
            acc_b->splits = NULL;
            
            /* free the account structure itself */
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

Account *
xaccGroupGetParentAccount (AccountGroup * grp)
{
  if (!grp) return NULL;
  return grp->parent;
}

double
DxaccGroupGetBalance (AccountGroup * grp)
{
  return gnc_numeric_to_double(xaccGroupGetBalance(grp));
}

gnc_numeric
xaccGroupGetBalance (AccountGroup * grp)
{
  if (!grp) return gnc_numeric_zero();
  return grp->balance;
}

/********************************************************************\
\********************************************************************/

int     
xaccGroupGetDepth (AccountGroup *grp)
{
   int i, depth=0, maxdepth=0;
   if (!grp) return 0;

   for (i=0; i < grp->numAcc; i++) {
      depth = xaccGroupGetDepth (grp->account[i]->children);
      if (depth > maxdepth) maxdepth = depth;
   }

   maxdepth++;
   return maxdepth;
}

/********************************************************************\
\********************************************************************/

void
xaccSplitsBeginStagedTransactionTraversals (GList *splits)
{
  Transaction *trans;
  GList *lp;

  if (splits == NULL) return;

  for(lp = splits; lp; lp = lp->next) {
    Split *s = (Split *) lp->data;
    trans = s->parent;
    if (trans != NULL)
      trans->marker = 0;
  }
}

void
xaccAccountBeginStagedTransactionTraversals (Account *account)
{
  if (account == NULL) return;
  xaccSplitsBeginStagedTransactionTraversals(account->splits);
}

void
xaccAccountsBeginStagedTransactionTraversals (Account **accounts)
{
  Account **aptr;

  if (accounts == NULL) return;

  for (aptr = accounts; *aptr != NULL; aptr++)
    xaccAccountBeginStagedTransactionTraversals(*aptr);
}

gboolean
xaccTransactionTraverse(Transaction *trans, int stage)
{
  if (trans == NULL) return FALSE;

  if (trans->marker < stage)
  {
    trans->marker = stage;
    return TRUE;
  }

  return FALSE;
}

gboolean
xaccSplitTransactionTraverse(Split *split, int stage)
{
  if (split == NULL) return FALSE;

  return xaccTransactionTraverse(split->parent, stage);
}

void
xaccGroupBeginStagedTransactionTraversals (AccountGroup *grp) 
{
  unsigned int numAcc;
  unsigned int i;

  if (!grp) return;

  numAcc = grp->numAcc;
  for(i = 0; i < numAcc; i++) {
    Account *acc = xaccGroupGetAccount(grp, i);
    GList *lp;

    if (!acc) return;

    /* recursively do sub-accounts */
    xaccGroupBeginStagedTransactionTraversals(acc->children);

    for(lp = acc->splits; lp; lp = lp->next) {
      Split *s = (Split *) lp->data;
      Transaction *trans = s->parent;
      trans->marker = 0;
    }
  }
}

int
xaccAccountStagedTransactionTraversal (Account *acc,
                                       unsigned int stage,
                                       int (*callback)(Transaction *t,
                                                       void *cb_data),
                                       void *cb_data) {
  if (!acc) return 0;
  if (callback) {
    GList *lp;
    for(lp = acc->splits; lp; lp = lp->next) {
      Split *s = (Split *) lp->data;
      Transaction *trans = s->parent;   
      if (trans && (trans->marker < stage)) {
        int retval;
        trans->marker = stage;
        retval = callback(trans, cb_data);
        if (retval) return retval;
      }
    }
  } else {
    GList *lp;
    for(lp = acc->splits; lp; lp = lp->next) {
      Split *s = (Split *) lp->data;
      Transaction *trans = s->parent;      
      if (trans && (trans->marker < stage)) {
        trans->marker = stage;
      }
    }
  }
  return 0;
}

int
xaccGroupStagedTransactionTraversal(AccountGroup *grp,
                                    unsigned int stage,
                                    int (*callback)(Transaction *t,
                                                    void *cb_data),
                                    void *cb_data)
{
  unsigned int numAcc;
  unsigned int i;

  if (!grp) return 0;

  numAcc = grp->numAcc;
  for(i = 0; i < numAcc; i++) {
    int retval;
    Account *acc;

    acc = xaccGroupGetAccount(grp, i);

    /* recursively do sub-accounts */
    retval = xaccGroupStagedTransactionTraversal (acc->children, stage,
                                                  callback, cb_data);
    if (retval) return retval;

    retval = xaccAccountStagedTransactionTraversal (acc, stage,
                                                    callback, cb_data);
    if (retval) return retval;
  }

  return 0;
}

/********************************************************************\
\********************************************************************/

gboolean
xaccGroupVisitUnvisitedTransactions(AccountGroup *g,
                                    gboolean (*proc)(Transaction *t,
                                                     void *data),
                                    void *data,
                                    GHashTable *visited_txns) {
  Account **list;
  Account **accounts;
  gboolean keep_going = TRUE;

  if(!g) return(FALSE);
  if(!proc) return(FALSE);
  if(!visited_txns) return(FALSE);

  list = accounts = xaccGetAccounts(g);
  if(!accounts) return(FALSE);

  while(*accounts && keep_going) {
    Account *acc = *accounts;

    keep_going =
      xaccAccountVisitUnvisitedTransactions(acc, proc, data, visited_txns);

    if(keep_going) accounts++;
  }

  if (list)
    free (list);

  return(keep_going);
}

gboolean
xaccGroupForEachTransaction(AccountGroup *g,
                            gboolean (*proc)(Transaction *t, void *data),
                            void *data) {
  GHashTable *visited_txns = NULL;
  gboolean result = FALSE;

  if(!g) return(FALSE);
  if(!proc) return(FALSE);
  
  visited_txns = guid_hash_table_new();
  if(visited_txns) {
    result = xaccGroupVisitUnvisitedTransactions(g, proc, data, visited_txns);
  }
  
  /* cleanup */
  if(visited_txns) g_hash_table_destroy(visited_txns);  
  return(result);
}

/********************************************************************\
\********************************************************************/

GSList *
xaccGroupMapAccounts(AccountGroup *grp,
                     gpointer (*thunk)(Account *a, void *data),
                     gpointer data) {
  Account **accts;
  GSList *result = NULL;

  if(!grp) return(NULL);
  if(!thunk) return(NULL);

  accts = grp->account;
  if(!accts) return(NULL);
  while(*accts) {
    gpointer thunk_result = thunk(*accts, data);
    if(thunk_result) result = g_slist_prepend(result, thunk_result);
    accts++;
  }
  return(g_slist_reverse(result));
}

gpointer
xaccGroupForEachAccountDeeply(AccountGroup *grp,
                              gpointer (*thunk)(Account *a, void *data),
                              gpointer data) {
  Account **accts;

  if(!grp) return(NULL);
  if(!thunk) return(NULL);

  accts = grp->account;
  if(!accts) return(NULL);
  while(*accts) {
    Account *acc = *accts;
    gpointer result = thunk(acc, data);
    if(result) return(result);
    result = xaccGroupForEachAccountDeeply(acc->children, thunk, data);
    if(result) return(result);
    accts++;
  }
  return(NULL);
}
