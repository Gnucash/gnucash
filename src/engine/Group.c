/********************************************************************\
 * Group.c -- chart of accounts (hierarchical tree of accounts)     *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1997-2001 Linas Vepstas <linas@linas.org>          *
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

#include <glib.h>
#include <string.h>

#include "Account.h"
#include "AccountP.h"
#include "BackendP.h"
#include "GNCIdP.h"
#include "Group.h"
#include "GroupP.h"
#include "TransactionP.h"
#include "gnc-book-p.h"
#include "gnc-engine-util.h"
#include "gnc-event-p.h"
#include "gnc-numeric.h"

static short module = MOD_ENGINE;

/********************************************************************\
 * Because I can't use C++ for this project, doesn't mean that I    *
 * can't pretend to!  These functions perform actions on the        *
 * AccountGroup data structure, in order to encapsulate the         *
 * knowledge of the internals of the AccountGroup in one file.      *
\********************************************************************/

/********************************************************************\
\********************************************************************/

static void
xaccInitializeAccountGroup (AccountGroup *grp, GNCBook *book)
{
  grp->saved       = 1;

  grp->parent      = NULL;
  grp->accounts    = NULL;

  grp->book        = book;
  grp->editlevel   = 0;
}

/********************************************************************\
\********************************************************************/

AccountGroup *
xaccMallocAccountGroup (GNCBook *book)
{
  AccountGroup *grp;
  g_return_val_if_fail (book, NULL);

  grp = g_new (AccountGroup, 1);
  xaccInitializeAccountGroup (grp, book);

  return grp;
}

/********************************************************************\
\********************************************************************/

gboolean
xaccGroupEqual(AccountGroup *ga,
               AccountGroup *gb,
               gboolean check_guids)
{
  GList *na;
  GList *nb;

  if (!ga && !gb) return(TRUE);

  if (!ga || !gb)
  {
    PWARN ("one is NULL");
    return(FALSE);
  }

  na = ga->accounts;
  nb = gb->accounts;

  if ((!na && nb) || (na && !nb))
  {
    PWARN ("only one has accounts");
    return(FALSE);
  }

  while (na && nb)
  {
    Account *aa = na->data;
    Account *ab = nb->data;

    if (!xaccAccountEqual(aa, ab, check_guids))
    {
      char sa[GUID_ENCODING_LENGTH + 1];
      char sb[GUID_ENCODING_LENGTH + 1];

      guid_to_string_buff (xaccAccountGetGUID (aa), sa);
      guid_to_string_buff (xaccAccountGetGUID (ab), sb);

      PWARN ("accounts %s and %s differ", sa, sb);

      return(FALSE);
    }

    na = na->next;
    nb = nb->next;
  }

  if (na || nb)
  {
    PWARN ("different numbers of accounts");
    return(FALSE);
  }

  return(TRUE);
}

/********************************************************************\
\********************************************************************/

void
xaccAccountGroupBeginEdit (AccountGroup *grp)
{
  GList *node;

  if (!grp) return;
  grp->editlevel++;

  for (node = grp->accounts; node; node = node->next)
  {
    Account *account = node->data;

    xaccAccountBeginEdit (account);
    xaccAccountGroupBeginEdit (account->children);
  }
}

/********************************************************************\
\********************************************************************/

void
xaccAccountGroupCommitEdit (AccountGroup *grp)
{
  GList *node;

  if (!grp) return;

  for (node = grp->accounts; node; node = node->next)
  {
    Account *account = node->data;

    xaccAccountGroupCommitEdit (account->children);
    xaccAccountCommitEdit (account);
  }
  grp->editlevel--;
}

/********************************************************************\
\********************************************************************/

static void
xaccGroupMarkDoFree (AccountGroup *grp)
{
  GList *node;

  if (!grp) return;

  for (node = grp->accounts; node; node = node->next)
  {
    Account *account = node->data;
    account->do_free = TRUE;
    xaccGroupMarkDoFree (account->children); 
  }
}

void
xaccAccountGroupDestroy (AccountGroup *grp)
{
   xaccGroupMarkDoFree (grp);
   xaccFreeAccountGroup (grp);
}

/********************************************************************\
\********************************************************************/

GNCBook *
xaccGroupGetBook (AccountGroup *group)
{
  if (!group) return NULL;
  return group->book;
}

/********************************************************************\
\********************************************************************/

void
xaccFreeAccountGroup (AccountGroup *grp)
{
  gboolean root_grp;

  if (!grp) return;

  root_grp = grp->parent == NULL;

  if (grp->accounts)
  {
    Account *account;
    /* This is a weird iterator & needs some explanation.
     * xaccAccountDestroy() will rip the account out of the list, thus
     * iterating while grp->accounts is non-null is enough to iterate
     * the loop.  But when it deletes the last account, then it will
     * also delete the group, unless it's the root group, making the
     * grp pointer invalid. So we have to be careful with the last
     * deletion: in particular, g_free(grp) would be freeing that
     * memory a second time, so don't do it. */
    while (grp->accounts->next)
    {
      account = grp->accounts->next->data;

      /* FIXME: this and the same code below is kind of hacky.
       *        actually, all this code seems to assume that
       *        the account edit levels are all 1. */
      if (account->editlevel == 0)
        xaccAccountBeginEdit (account);

      xaccAccountDestroy (account);
    }
    account = grp->accounts->data;
    if (account->editlevel == 0)
      xaccAccountBeginEdit (account);
    xaccAccountDestroy (account);

    if (!root_grp) return;
  }

  if (grp->parent) grp->parent->children = NULL;

  grp->parent   = NULL;

  g_free (grp);
}

/********************************************************************\
\********************************************************************/

void
xaccGroupMarkSaved (AccountGroup *grp)
{
  GList *node;

  if (!grp) return;

  grp->saved = 1;

  for (node = grp->accounts; node; node = node->next)
  {
    Account *account = node->data;

    xaccGroupMarkSaved (account->children); 
  }
}

/********************************************************************\
\********************************************************************/

void
xaccGroupMarkNotSaved (AccountGroup *grp)
{
  if (!grp) return;

  grp->saved = 0;
}

/********************************************************************\
\********************************************************************/

gboolean
xaccGroupNotSaved (AccountGroup *grp)
{
  GList *node;

  if (!grp) return FALSE;

  if (grp->saved == 0) return TRUE;

  for (node = grp->accounts; node; node = node->next)
  {
    Account *account = node->data;

    if (xaccGroupNotSaved (account->children))
      return TRUE;
  }

  return FALSE;
}

/********************************************************************\
 * Get the number of accounts, including subaccounts                *
\********************************************************************/

int
xaccGroupGetNumSubAccounts (AccountGroup *grp)
{
  GList *node;
  int num_acc;

  if (!grp) return 0;

  num_acc = g_list_length (grp->accounts);

  for (node = grp->accounts; node; node = node->next)
  {
    Account *account = node->data;

    num_acc += xaccGroupGetNumSubAccounts (account->children);
  }

  return num_acc;
}

/********************************************************************\
 * Get all of the accounts, including subaccounts                   *
\********************************************************************/

static void
xaccPrependAccounts (AccountGroup *grp, GList **accounts_p)
{
  GList *node;

  if (!grp || !accounts_p) return;

  for (node = grp->accounts; node; node = node->next)
  {
    Account *account = node->data;

    *accounts_p = g_list_prepend (*accounts_p, account);

    xaccPrependAccounts (account->children, accounts_p);
  }
}

AccountList *
xaccGroupGetSubAccounts (AccountGroup *grp)
{
  GList *accounts = NULL;

  if (!grp) return NULL;

  xaccPrependAccounts (grp, &accounts);

  return g_list_reverse (accounts);
}

AccountList *
xaccGroupGetAccountList (AccountGroup *grp)
{
  if (!grp) return NULL;

  return grp->accounts;
}

/********************************************************************\
 * Fetch the root of the tree                                       *
\********************************************************************/

AccountGroup *
xaccGroupGetRoot (AccountGroup * grp) 
{
  AccountGroup * root = NULL;

  /* find the root of the account group structure */
  while (grp)
  {
    Account *parent_acc;

    root = grp;
    parent_acc = grp->parent;

    if (parent_acc)
      grp = parent_acc->parent;
    else
      grp = NULL;
  }

  return root;
}

AccountGroup *
xaccAccountGetRoot (Account * acc) 
{
  if (!acc) return NULL;
  return xaccGroupGetRoot (acc->parent);
}

/********************************************************************\
 * Fetch an account, given its name                                *
\********************************************************************/

Account *
xaccGetAccountFromName (AccountGroup *grp, const char * name)
{
  GList *node;

  if (!grp) return NULL;
  if (!name) return NULL;

  /* first, look for accounts hanging off the root */
  for (node = grp->accounts; node; node = node->next)
  {
    Account *account = node->data;

    if (safe_strcmp(xaccAccountGetName (account), name) == 0)
      return account;
  }

  /* if we are still here, then we haven't found the account yet.
   * Recursively search the subgroups next */
  /* first, look for accounts hanging off the root */
  for (node = grp->accounts; node; node = node->next)
  {
    Account *account = node->data;
    Account *acc;

    acc = xaccGetAccountFromName (account->children, name);
    if (acc)
      return acc;
  }

  return NULL;
}

/********************************************************************\
 * Fetch an account, given its full name                            *
\********************************************************************/

Account *
xaccGetAccountFromFullName (AccountGroup *grp,
                            const char *name,
                            const char separator)
{
  GList *node;
  Account *found;
  char *p;

  if (!grp) return NULL;
  if (!name) return NULL;

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
    for (node = grp->accounts; node; node = node->next)
    {
      Account *account = node->data;

      if (safe_strcmp(xaccAccountGetName (account), name) == 0)
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
xaccGetPeerAccountFromName (Account *acc, const char * name)
{
  AccountGroup * root;
  Account *peer_acc;

  if (!acc) return NULL;
  if (!name) return NULL;

  /* first, find the root of the account group structure */
  root = xaccAccountGetRoot (acc);

  /* now search all accounts hanging off the root */
  peer_acc = xaccGetAccountFromName (root, name);

  return peer_acc;
}

/********************************************************************\
 * Fetch an account, given its full name                            *
\********************************************************************/

Account *
xaccGetPeerAccountFromFullName (Account *acc, const char * name,
                                const char separator)
{
  AccountGroup * root;
  Account *peer_acc;

  if (!acc) return NULL;
  if (!name) return NULL;

  /* first, find the root of the account group structure */
  root = xaccAccountGetRoot (acc);

  /* now search all acounts hanging off the root */
  peer_acc = xaccGetAccountFromFullName (root, name, separator);

  return peer_acc;
}

/********************************************************************\
\********************************************************************/

void
xaccAccountRemoveGroup (Account *acc)
{
  AccountGroup *grp;

  /* if this group has no parent, it must be the topgroup */
  if (!acc) return;

  grp = acc->children;

  if (grp) grp->parent = NULL;
  acc->children = NULL;

  /* make sure that the parent of the group is marked 
   * as having been modified. */
  grp = acc->parent;
  if (!grp) return;

  grp->saved = 0;

  gnc_engine_generate_event (&acc->guid, GNC_EVENT_MODIFY);
}

/********************************************************************\
\********************************************************************/

void
xaccGroupRemoveAccount (AccountGroup *grp, Account *acc)
{
  if (!acc) return;

  /* Note this routine might be called on accounts which 
   * are not yet parented. */
  if (!grp) return;

  if (acc->parent != grp)
  {
    PERR ("account not in group");
    return;
  }

  acc->parent = NULL;

  grp->accounts = g_list_remove (grp->accounts, acc);

  grp->saved = 0;

  /* if this was the last account in a group, delete
   * the group as well (unless its a root group) */
  if ((grp->accounts == NULL) && (grp->parent))
  {
    xaccAccountRemoveGroup (grp->parent);
    xaccFreeAccountGroup (grp);
  }

  gnc_engine_generate_event (&acc->guid, GNC_EVENT_MODIFY);
}

/********************************************************************\
\********************************************************************/

void
xaccAccountInsertSubAccount (Account *adult, Account *child)
{
  if (!adult) return;

  /* if a container for the children doesn't yet exist, add it */
  if (adult->children == NULL)
    adult->children = xaccMallocAccountGroup (adult->book);

  /* set back-pointer to parent */
  adult->children->parent = adult;

  /* allow side-effect of creating a child-less account group */
  if (!child) return;

  xaccGroupInsertAccount (adult->children, child);

  gnc_engine_generate_event (&adult->guid, GNC_EVENT_MODIFY);
}

/********************************************************************\
\********************************************************************/

static int
group_sort_helper (gconstpointer a, gconstpointer b)
{
  Account *aa = (Account *) a;
  Account *bb = (Account *) b;

  /* return > 1 if aa should come after bb */
  return xaccAccountOrder (&aa, &bb);
}

void
xaccGroupInsertAccount (AccountGroup *grp, Account *acc)
{
  if (!grp || !grp->book) return;
  if (!acc) return;

  /* If the account is currently in another group, remove it there
   * first. Basically, we can't have accounts being in two places at
   * once. If old and new parents are the same, reinsertion causes
   * the sort order to be checked. */
  if (acc->parent == grp)
  {
    grp->accounts = g_list_sort (grp->accounts, group_sort_helper);
  }
  else
  {
    xaccAccountBeginEdit (acc);

    if (acc->parent) 
    {
      xaccGroupRemoveAccount (acc->parent, acc);

      /* switch over between books, if needed */
      if (grp->book != acc->book)
      {
// xxxxxxxxxxxxxxxxxxxxxxx
         /* hack alert -- this implementation is not exactly correct.
          * If the entity tables are not identical, then the 'from' book 
          * will have a different backend than the 'to' book.  This means
          * that we should get the 'from' backend to destroy this account,
          * and the 'to' backend to save it.  Right now, this is broken.
          *  
          * A 'correct' implementation similar to this is in Period.c
          * except its for transactions ...
          *
          * Note also, we need to reparent the children to the new book as well.
          */
         PWARN ("reparenting accounts accross books is not correctly supported\n");

         gnc_engine_generate_event (&acc->guid, GNC_EVENT_DESTROY);
         xaccRemoveEntity (acc->book->entity_table, &acc->guid);

         xaccStoreEntity (grp->book->entity_table, acc, &acc->guid, GNC_ID_ACCOUNT);
         gnc_engine_generate_event (&acc->guid, GNC_EVENT_CREATE);
      }
    }

    /* set back-pointer to the account's parent */
    acc->parent = grp;

    grp->accounts = g_list_insert_sorted (grp->accounts, acc,
                                          group_sort_helper);

    acc->core_dirty = TRUE;
    xaccAccountCommitEdit (acc);
  }

  grp->saved = 0;

  gnc_engine_generate_event (&acc->guid, GNC_EVENT_MODIFY);
}

/********************************************************************\
\********************************************************************/

void 
xaccGroupConcatGroup (AccountGroup *togrp, AccountGroup *fromgrp)
{
  if (!togrp) return;
  if (!fromgrp) return;

  /* The act of inserting the account into togrp also causes it to
   * automatically be deleted from fromgrp.  This causes linked 
   * lists to be re-written, and so a cursor traversal is not safe.
   * Be careful! */

  while (TRUE)
  {
    Account *account;
    GList *accounts;
    GList *next;

    accounts = fromgrp->accounts;
    if (!accounts) return;

    next = accounts->next;

    account = accounts->data;

    xaccGroupInsertAccount (togrp, account);

    if (!next) return;
  }
}

void
xaccGroupCopyGroup (AccountGroup *to, AccountGroup *from)
{
   int i;
   GList *node;
   if (!to || !from) return;
   if (!from->accounts || !to->book) return;

   ENTER (" ");
   for (node = from->accounts; node; node=node->next)
   {
      Account *to_acc, *from_acc = node->data;

      /* This will copy the basic data and the KVP.  It will
       * not copy any splits/transactions. It will gemini. */
      to_acc = xaccCloneAccount (from_acc, to->book);

      xaccAccountBeginEdit (to_acc);
      to->accounts = g_list_append (to->accounts, to_acc);

      /* copy child accounts too. */
      if (from_acc->children)
      {
         to_acc->children = xaccMallocAccountGroup (to->book);
         xaccGroupCopyGroup (to_acc->children, from_acc->children);
      }
      xaccAccountCommitEdit (to_acc);

      /* make sure that we have a symmetric, uniform number of 
       * begin-edits, so that subsequent GroupCommitEdit's 
       * balance out. */
      for (i=0; i<to->editlevel; i++)
      {
         xaccAccountBeginEdit (to_acc);
         xaccAccountGroupBeginEdit (to_acc->children);
      }
   }
   LEAVE (" ");
}

/********************************************************************\
\********************************************************************/

void 
xaccGroupMergeAccounts (AccountGroup *grp)
{
  GList *node_a;
  GList *node_b;

  if (!grp) return;

  for (node_a = grp->accounts; node_a; node_a = node_a->next)
  {
    Account *acc_a = node_a->data;

    for (node_b = node_a->next; node_b; node_b = node_b->next)
    {
      Account *acc_b = node_b->data;

      if ((0 == safe_strcmp(xaccAccountGetName(acc_a),
                            xaccAccountGetName(acc_b))) &&
          (0 == safe_strcmp(xaccAccountGetCode(acc_a),
                            xaccAccountGetCode(acc_b))) &&
          (0 == safe_strcmp(xaccAccountGetDescription(acc_a),
                            xaccAccountGetDescription(acc_b))) &&
          (gnc_commodity_equiv(xaccAccountGetCommodity(acc_a),
                               xaccAccountGetCommodity(acc_b))) &&
          (0 == safe_strcmp(xaccAccountGetNotes(acc_a),
                            xaccAccountGetNotes(acc_b))) &&
          (xaccAccountGetType(acc_a) == xaccAccountGetType(acc_b)))
      {
        AccountGroup *ga, *gb;
        GList *lp;

        /* consolidate children */
        ga = (AccountGroup *) acc_a->children;
        gb = (AccountGroup *) acc_b->children;

        if (gb)
        {
          if (!ga)
          {
            acc_a->children = gb;
            gb->parent = acc_a;
            acc_b->children = NULL;

            gnc_engine_generate_event (&acc_a->guid, GNC_EVENT_MODIFY);
            gnc_engine_generate_event (&acc_b->guid, GNC_EVENT_MODIFY);
          }
          else
          {
            xaccGroupConcatGroup (ga, gb);
            acc_b->children = NULL;
            gnc_engine_generate_event (&acc_b->guid, GNC_EVENT_MODIFY);
          }
        }

        /* recurse to do the children's children */
        xaccGroupMergeAccounts (ga);

        /* consolidate transactions */
        lp = acc_b->splits;
        
        for (lp = acc_b->splits; lp; lp = lp->next)
        {
          Split *split = lp->data;

          gnc_engine_generate_event (&xaccSplitGetAccount(split)->guid,
                                     GNC_EVENT_MODIFY);
          xaccSplitSetAccount(split, NULL);
          xaccAccountInsertSplit (acc_a, split);
        }

        g_list_free(acc_b->splits);
        acc_b->splits = NULL;

        /* move back one before removal */
        node_b = node_b->prev;

        /* remove from list -- node_a is ok, it's before node_b */
        grp->accounts = g_list_remove (grp->accounts, acc_b);

        xaccAccountBeginEdit (acc_b);
        xaccAccountDestroy (acc_b);
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

   return g_list_length (grp->accounts);
}

Account *
xaccGroupGetAccount (AccountGroup *grp, int i)
{
   if (!grp) return NULL;

   return g_list_nth_data (grp->accounts, i);
}

Account *
xaccGroupGetParentAccount (AccountGroup * grp)
{
  if (!grp) return NULL;

  return grp->parent;
}

/********************************************************************\
\********************************************************************/

int     
xaccGroupGetDepth (AccountGroup *grp)
{
  GList *node;
  int depth = 0;
  int maxdepth = 0;

  if (!grp) return 0;

  for (node = grp->accounts; node; node = node->next)
  {
    Account *account = node->data;

    depth = xaccGroupGetDepth (account->children);

    if (depth > maxdepth)
      maxdepth = depth;
  }

  maxdepth++;

  return maxdepth;
}

/********************************************************************\
\********************************************************************/

void
xaccSplitsBeginStagedTransactionTraversals (GList *splits)
{
  GList *lp;

  for (lp = splits; lp; lp = lp->next)
  {
    Split *s = lp->data;
    Transaction *trans = s->parent;

    if (trans)
      trans->marker = 0;
  }
}

void
xaccAccountBeginStagedTransactionTraversals (Account *account)
{
  if (account == NULL) return;
  xaccSplitsBeginStagedTransactionTraversals (account->splits);
}

gboolean
xaccTransactionTraverse (Transaction *trans, int stage)
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
xaccSplitTransactionTraverse (Split *split, int stage)
{
  if (split == NULL) return FALSE;

  return xaccTransactionTraverse (split->parent, stage);
}

void
xaccGroupBeginStagedTransactionTraversals (AccountGroup *grp) 
{
  GList *node;

  if (!grp) return;

  for (node = grp->accounts; node; node = node->next)
  {
    Account *account = node->data;
    GList *lp;

    /* recursively do sub-accounts */
    xaccGroupBeginStagedTransactionTraversals (account->children);

    for (lp = account->splits; lp; lp = lp->next)
    {
      Split *s = lp->data;
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
                                       void *cb_data)
{
  if (!acc) return 0;

  if (callback)
  {
    GList *lp;
    for(lp = acc->splits; lp; lp = lp->next)
    {
      Split *s = (Split *) lp->data;
      Transaction *trans = s->parent;   
      if (trans && (trans->marker < stage))
      {
        int retval;
        trans->marker = stage;
        retval = callback(trans, cb_data);
        if (retval) return retval;
      }
    }
  }
  else
  {
    GList *lp;
    for(lp = acc->splits; lp; lp = lp->next)
    {
      Split *s = (Split *) lp->data;
      Transaction *trans = s->parent;      
      if (trans && (trans->marker < stage))
        trans->marker = stage;
    }
  }

  return 0;
}

int
xaccGroupStagedTransactionTraversal (AccountGroup *grp,
                                     unsigned int stage,
                                     int (*callback)(Transaction *t,
                                                     void *cb_data),
                                     void *cb_data)
{
  GList *node;

  if (!grp) return 0;

  for (node = grp->accounts; node; node = node->next)
  {
    Account *account = node->data;
    int retval;

    /* recursively do sub-accounts */
    retval = xaccGroupStagedTransactionTraversal (account->children, stage,
                                                  callback, cb_data);
    if (retval) return retval;

    retval = xaccAccountStagedTransactionTraversal (account, stage,
                                                    callback, cb_data);
    if (retval) return retval;
  }

  return 0;
}

/********************************************************************\
\********************************************************************/

struct group_visit_data
{
    gboolean (*proc)(Transaction *t, void *data);
    void *up_data;
    GHashTable *visit_table;
};

static gboolean
xaccGroupVisitUnvisitedTransactions_thunk(Transaction *trn,
                                          void *data)
{
    gpointer test_trn;
    struct group_visit_data *grdata = (struct group_visit_data*)data;
    gboolean result;

    test_trn = g_hash_table_lookup(grdata->visit_table, trn);

    if(!test_trn)
    {
        g_hash_table_insert(grdata->visit_table, trn, "");

        result = grdata->proc(trn, grdata->up_data);
    }
    else
      result = TRUE;

    return result;
}

gboolean
xaccGroupVisitUnvisitedTransactions (AccountGroup *g,
                                     gboolean (*proc)(Transaction *t,
                                                      void *data),
                                     void *data,
                                     GHashTable *visited_txns)
{
  gboolean keep_going = TRUE;
  GList *list;
  GList *node;
  struct group_visit_data grdata;
  
  if (!g) return(FALSE);
  if (!proc) return(FALSE);
  if (!visited_txns) return(FALSE);

  list = xaccGroupGetSubAccounts (g);

  grdata.proc = proc;
  grdata.up_data = data;
  grdata.visit_table = visited_txns;
  
  for (node = list; node && keep_going; node = node->next)
  {
    Account *account = node->data;
    
    keep_going = xaccAccountForEachTransaction(
        account, xaccGroupVisitUnvisitedTransactions_thunk, (void*)&grdata);
  }

  g_list_free (list);

  return(keep_going);
}

gboolean
xaccGroupForEachTransaction (AccountGroup *g,
                             gboolean (*proc)(Transaction *t, void *data),
                             void *data)
{
  GHashTable *visited_txns = NULL;
  gboolean result = FALSE;

  if (!g) return(FALSE);
  if (!proc) return(FALSE);

  visited_txns = guid_hash_table_new();
  if (visited_txns)
    result = xaccGroupVisitUnvisitedTransactions(g, proc, data, visited_txns);

  /* cleanup */
  if (visited_txns)
    g_hash_table_destroy(visited_txns);  

  return(result);
}

/********************************************************************\
\********************************************************************/

AccountList *
xaccGroupMapAccounts (AccountGroup *grp,
                      gpointer (*thunk)(Account *a, gpointer data),
                      gpointer data)
{
  GList *result = NULL;
  GList *node;

  if (!grp) return(NULL);
  if (!thunk) return(NULL);

  for (node = grp->accounts; node; node = node->next)
  {
    Account *account = node->data;
    gpointer thunk_result = thunk (account, data);

    if (thunk_result)
    {
      result = g_list_append (result, thunk_result);
    }
  }

  return (result);
}

gpointer
xaccGroupForEachAccount (AccountGroup *grp,
                         gpointer (*thunk)(Account *a, gpointer data),
                         gpointer data,
                         gboolean deeply)
{
  GList *node;

  if (!grp) return(NULL);
  if (!thunk) return(NULL);

  for (node = grp->accounts; node; node = node->next)
  {
    Account *account = node->data;
    gpointer result = thunk (account, data);

    if (result)
      return(result);

    if(deeply)
        result = xaccGroupForEachAccount (account->children,
                                          thunk, data, TRUE);

    if (result)
      return(result);
  }

  return(NULL);
}

/* ========================= END OF FILE ======================== */
