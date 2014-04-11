/********************************************************************\
 * Group.c -- chart of accounts (hierarchical tree of accounts)     *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1997-2001,2003 Linas Vepstas <linas@linas.org>     *
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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#include "config.h"

#include <glib.h>
#include <string.h>

#include "Account.h"
#include "AccountP.h"
#include "Group.h"
#include "GroupP.h"
#include "TransactionP.h"
#include "gnc-event.h"

static QofLogModule log_module = GNC_MOD_ENGINE;

/********************************************************************\
 * Because I can't use C++ for this project, doesn't mean that I    *
 * can't pretend to!  These functions perform actions on the        *
 * AccountGroup data structure, in order to encapsulate the         *
 * knowledge of the internals of the AccountGroup in one file.      *
\********************************************************************/

/********************************************************************\
\********************************************************************/

static void
xaccInitializeAccountGroup (AccountGroup *grp, QofBook *book)
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
xaccMallocAccountGroup (QofBook *book)
{
  AccountGroup *grp;
  g_return_val_if_fail (book, NULL);

  grp = g_new (AccountGroup, 1);
  xaccInitializeAccountGroup (grp, book);

  return grp;
}

/********************************************************************\
\********************************************************************/

AccountGroup * 
xaccCollGetAccountGroup (const QofCollection *col)
{
  if (!col) return NULL;
  return qof_collection_get_data (col);
}

void
xaccCollSetAccountGroup (QofCollection *col, AccountGroup *grp)
{
  AccountGroup *old_grp;
  if (!col) return;

  old_grp = xaccCollGetAccountGroup (col);
  if (old_grp == grp) return;

  qof_collection_set_data (col, grp);

  xaccAccountGroupBeginEdit (old_grp);
  xaccAccountGroupDestroy (old_grp);
}

AccountGroup * 
xaccGetAccountGroup (QofBook *book)
{
  QofCollection *col;
  if (!book) return NULL;
  col = qof_book_get_collection (book, GNC_ID_GROUP);
  return xaccCollGetAccountGroup (col);
}

void
xaccSetAccountGroup (QofBook *book, AccountGroup *grp)
{
  QofCollection *col;
  if (!book) return;

  if (grp && grp->book != book)
  {
     PERR ("cannot mix and match books freely!");
     return;
  }

  col = qof_book_get_collection (book, GNC_ID_GROUP);
  xaccCollSetAccountGroup (col, grp);
}

/********************************************************************\
\********************************************************************/

gboolean
xaccGroupEqual(const AccountGroup *ga,
               const AccountGroup *gb,
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
    account->inst.do_free = TRUE;
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

QofBook *
xaccGroupGetBook (const AccountGroup *group)
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
      if (account->inst.editlevel == 0)
        xaccAccountBeginEdit (account);

      xaccAccountDestroy (account);
    }
    account = grp->accounts->data;
    if (account->inst.editlevel == 0) 
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
xaccGroupNotSaved (const AccountGroup *grp)
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
xaccGroupGetNumSubAccounts (const AccountGroup *grp)
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
 * Recursively get all of the accounts, including subaccounts       *
\********************************************************************/

static void
xaccPrependAccounts (const AccountGroup *grp, GList **accounts_p)
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
xaccGroupGetSubAccounts (const AccountGroup *grp)
{
  GList *accounts = NULL;

  if (!grp) return NULL;

  xaccPrependAccounts (grp, &accounts);

  return g_list_reverse (accounts);
}

static int
group_sort_helper (gconstpointer a, gconstpointer b)
{
  const Account *aa = (const Account *) a;
  const Account *bb = (const Account *) b;

  /* xaccAccountOrder returns > 1 if aa should come after bb.
   * This funciton is building a reversed list. */
  return xaccAccountOrder (&aa, &bb);
}

static void
xaccPrependAccountsSorted (const AccountGroup *grp, GList **accounts_p)
{
  GList *node, *tmp_list;

  if (!grp || !accounts_p) return;

  tmp_list = g_list_copy(grp->accounts);
  tmp_list = g_list_sort(tmp_list, group_sort_helper);

  for (node = tmp_list; node; node = node->next)
  {
    Account *account = node->data;

    *accounts_p = g_list_prepend (*accounts_p, account);

    xaccPrependAccountsSorted (account->children, accounts_p);
  }
  g_list_free(tmp_list);
}

AccountList *
xaccGroupGetSubAccountsSorted (const AccountGroup *grp)
{
  GList *accounts = NULL;

  if (!grp) return NULL;

  xaccPrependAccountsSorted (grp, &accounts);

  return g_list_reverse (accounts);
}

AccountList *
xaccGroupGetAccountList (const AccountGroup *grp)
{
  if (!grp) return NULL;

  return grp->accounts;
}

AccountList *
xaccGroupGetAccountListSorted (const AccountGroup *grp)
{
  if (!grp) return NULL;

  return g_list_sort(g_list_copy(grp->accounts), group_sort_helper);

}

/********************************************************************\
 * Fetch the root of the tree                                       *
\********************************************************************/

AccountGroup *
xaccGroupGetRoot (const AccountGroup * grp) 
{
  const AccountGroup * root = NULL;

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

  return (AccountGroup*)root;
}

AccountGroup *
xaccAccountGetRoot (const Account * acc) 
{
  return acc ? xaccGroupGetRoot (acc->parent) : NULL;
}

/********************************************************************\
 * Fetch an account, given its name                                *
\********************************************************************/

Account *
xaccGetAccountFromName (const AccountGroup *grp, const char * name)
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

static Account *
xaccGetAccountFromFullNameHelper (const AccountGroup *grp,
				  gchar **names)
{
  Account *found = NULL;
  GList *node;
  gchar *name_str, *temp_str;
  const gchar *separator;
  gboolean str_alloced = FALSE;

  g_return_val_if_fail(grp, NULL);
  g_return_val_if_fail(names, NULL);

  separator = gnc_get_account_separator_string();
  /* start with the first name in the list */
  name_str = names[0];

  /* Make sure we actually HAVE a string! */
  if (name_str == NULL)
    return NULL;

  while (1)
  {
    /* Look for the first name in the children. */
    for (node = grp->accounts; node; node = node->next) {
      Account *account = node->data;

      if (safe_strcmp(xaccAccountGetName (account), name_str) == 0) {
	/* We found an account.  If the next entry is NULL, there is
	 * nothing left in the name, so just return the account. */
	if (names[1] == NULL) {
	  found = account;
	  goto done;
	}

	/* No children?  We're done. */
	if (!account->children) {
	  found = NULL;
	  goto done;
	}

	/* There's stuff left to search for.  Search recursively. */
	found = xaccGetAccountFromFullNameHelper(account->children, &names[1]);
	if (found != NULL) {
	  goto done;
	}
      }
    }

    /* If we got here then we didn't find a match based on name_str
     * so build a new name_str using the next token and try again.
     */

    /* If there's no more names then we're done.  We didn't find anything */
    if (names[1] == NULL) {
      found = NULL;
      break;
    }

    /* If we are here, we didn't find anything and there
     * must be more separators. So, continue looking with
     * a longer name, in case there is a name with the
     * separator character in it. */ 

    /* Build the new name string */
    temp_str = g_strconcat(name_str, separator, names[1], NULL);
    if (str_alloced)
      g_free(name_str);
    str_alloced = TRUE;
    names++;
    name_str = temp_str;    
  }

done:
  if (str_alloced)
    g_free(name_str);
  return found;
}


Account *
xaccGetAccountFromFullName (const AccountGroup *grp,
			    const char *name)
{
  Account *found;
  gchar **names;

  if (!grp) return NULL;
  if (!name) return NULL;

  names = g_strsplit(name, gnc_get_account_separator_string(), -1);
  found = xaccGetAccountFromFullNameHelper(grp, names);
  g_strfreev(names);
  return found;
}

/********************************************************************\
 * Fetch an account, given its name                                *
\********************************************************************/

Account *
xaccGetPeerAccountFromName (const Account *acc, const char * name)
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
xaccGetPeerAccountFromFullName (const Account *acc, const char * name)
{
  AccountGroup * root;
  Account *peer_acc;

  if (!acc) return NULL;
  if (!name) return NULL;

  /* first, find the root of the account group structure */
  root = xaccAccountGetRoot (acc);

  /* now search all acounts hanging off the root */
  peer_acc = xaccGetAccountFromFullName (root, name);

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

  qof_event_gen (&acc->inst.entity, QOF_EVENT_MODIFY, NULL);
}

/********************************************************************\
\********************************************************************/

void
xaccGroupRemoveAccount (AccountGroup *grp, Account *acc)
{
  GncEventData ed;

  if (!acc) return;

  /* Note this routine might be called on accounts which 
   * are not yet parented. */
  if (!grp) return;

  if (acc->parent != grp)
  {
    PERR ("account not in group");
    return;
  }

  /* Gather event data */
  ed.node = grp->parent;
  ed.idx = g_list_index(grp->accounts, acc);

  grp->accounts = g_list_remove (grp->accounts, acc);

  /* Now send the event. */
  qof_event_gen(&acc->inst.entity, QOF_EVENT_REMOVE, &ed);

  /* clear the account's group pointer after REMOVE event generation. */
  acc->parent = NULL;

  grp->saved = 0;

  /* if this was the last account in a group, delete
   * the group as well (unless its a root group) */
  if ((grp->accounts == NULL) && (grp->parent))
  {
    xaccAccountRemoveGroup (grp->parent);
    xaccFreeAccountGroup (grp);
  }

  qof_event_gen (&acc->inst.entity, QOF_EVENT_MODIFY, NULL);
}

/********************************************************************\
\********************************************************************/

void
xaccAccountInsertSubAccount (Account *adult, Account *child)
{
  if (!adult) return;

  /* if a container for the children doesn't yet exist, add it */
  if (adult->children == NULL)
    adult->children = xaccMallocAccountGroup (adult->inst.book);

  /* set back-pointer to parent */
  adult->children->parent = adult;

  /* allow side-effect of creating a child-less account group */
  if (!child) return;

  xaccGroupInsertAccount (adult->children, child);

  qof_event_gen (&adult->inst.entity, QOF_EVENT_MODIFY, NULL);
}

/********************************************************************\
\********************************************************************/

void
xaccGroupInsertAccount (AccountGroup *grp, Account *acc)
{
  if (!grp || !grp->book) return;
  if (!acc) return;

  ENTER("group %p, account %p named %s", grp, acc, xaccAccountGetName(acc));
  /* If the account is currently in another group, remove it there
   * first. Basically, we can't have accounts being in two places at
   * once. If old and new parents are the same, reinsertion causes
   * the sort order to be checked. */
  if (acc->parent != grp)
  {
    xaccAccountBeginEdit (acc);

    if (acc->parent) 
    {
      xaccGroupRemoveAccount (acc->parent, acc);

      /* switch over between books, if needed */
      if (grp->book != acc->inst.book)
      {
         QofCollection *col;
// xxxxxxxxxxxxxxxxxxxxxxx
         /* hack alert -- this implementation is not exactly correct.
          * If the entity tables are not identical, then the 'from' book 
          * may have a different backend than the 'to' book.  This means
          * that we should get the 'from' backend to destroy this account,
          * and the 'to' backend to save it.  Right now, this is broken.
          *  
          * A 'correct' implementation similar to this is in Period.c
          * except its for transactions ...
          *
          * Note also, we need to reparent the children to the new book as well.
          */
         PWARN ("reparenting accounts across books is not correctly supported\n");

         qof_event_gen (&acc->inst.entity, QOF_EVENT_DESTROY, NULL);
         col = qof_book_get_collection (grp->book, GNC_ID_ACCOUNT);
         qof_collection_insert_entity (col, &acc->inst.entity);
         qof_event_gen (&acc->inst.entity, QOF_EVENT_CREATE, NULL);
      }
    }

    /* set back-pointer to the account's parent */
    acc->parent = grp;

    grp->accounts = g_list_append (grp->accounts, acc);

    /* Gather event data */
    qof_event_gen (&acc->inst.entity, QOF_EVENT_ADD, NULL);

    qof_instance_set_dirty(&acc->inst);
    xaccAccountCommitEdit (acc);
  }

  grp->saved = 0;

  qof_event_gen (&acc->inst.entity, QOF_EVENT_MODIFY, NULL);
  LEAVE(" ");
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
   xaccAccountGroupBeginEdit(to);
   xaccAccountGroupBeginEdit(from);
   for (node = from->accounts; node; node=node->next)
   {
      Account *to_acc, *from_acc = node->data;

      /* This will copy the basic data and the KVP.  It will
       * not copy any splits/transactions. It will gemini. */
      to_acc = xaccCloneAccount (from_acc, to->book);

      xaccAccountBeginEdit (to_acc);
      to->accounts = g_list_append (to->accounts, to_acc);

      to_acc->parent = to;
      qof_instance_set_dirty(&to_acc->inst);

      /* Copy child accounts too. */
      if (from_acc->children)
      {
         to_acc->children = xaccMallocAccountGroup (to->book);
         to_acc->children->parent = to_acc;
         xaccGroupCopyGroup (to_acc->children, from_acc->children);
      }
      xaccAccountCommitEdit (to_acc);
      qof_event_gen (&to_acc->inst.entity, QOF_EVENT_CREATE, NULL);

      /* make sure that we have a symmetric, uniform number of 
       * begin-edits, so that subsequent GroupCommitEdit's 
       * balance out. */
      for (i=0; i<to->editlevel; i++)
      {
         xaccAccountBeginEdit (to_acc);
         xaccAccountGroupBeginEdit (to_acc->children);
      }
   }
   xaccAccountGroupCommitEdit(from);
   xaccAccountGroupCommitEdit(to);
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

            qof_event_gen (&acc_a->inst.entity, QOF_EVENT_MODIFY, NULL);
            qof_event_gen (&acc_b->inst.entity, QOF_EVENT_MODIFY, NULL);
          }
          else
          {
            xaccGroupConcatGroup (ga, gb);
            acc_b->children = NULL;
            qof_event_gen (&acc_b->inst.entity, QOF_EVENT_MODIFY, NULL);
          }
        }

        /* recurse to do the children's children */
        xaccGroupMergeAccounts (ga);

        /* consolidate transactions */
        while (acc_b->splits)
          xaccSplitSetAccount (acc_b->splits->data, acc_a);

        /* move back one before removal */
        node_b = node_b->prev;

        /* The destroy function will remove from list -- node_a is ok,
	 * it's before node_b */
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
xaccGroupGetNumAccounts (const AccountGroup *grp)
{
   if (!grp) return 0;

   return g_list_length (grp->accounts);
}

Account *
xaccGroupGetAccount (const AccountGroup *grp, int i)
{
   if (!grp) return NULL;

   return g_list_nth_data (grp->accounts, i);
}

Account *
xaccGroupGetParentAccount (const AccountGroup * grp)
{
  if (!grp) return NULL;

  return grp->parent;
}

/********************************************************************\
\********************************************************************/

int     
xaccGroupGetDepth (const AccountGroup *grp)
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
xaccAccountBeginStagedTransactionTraversals (const Account *account)
{
  if (account)
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
xaccAccountStagedTransactionTraversal (const Account *acc,
                                       unsigned int stage,
                                       int (*callback)(Transaction *t,
                                                       void *cb_data),
                                       void *cb_data)
{
  GList *lp;
  if (!acc) return 0;

  for(lp = acc->splits; lp; lp = lp->next)
  {
    Split *s = (Split *) lp->data;
    Transaction *trans = s->parent;   
    if (trans && (trans->marker < stage))
    {
      trans->marker = stage;
      if (callback)
      {
        int retval;
        retval = callback(trans, cb_data);
        if (retval) return retval;
      }
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

int
xaccGroupForEachTransaction (AccountGroup *g,
                             int (*proc)(Transaction *t, void *data),
                             void *data)
{
  if (!g || !proc) return 0;

  xaccGroupBeginStagedTransactionTraversals (g);
  return xaccGroupStagedTransactionTraversal (g, 42, proc, data);
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

  if (!grp || !thunk) return NULL;

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

  if (!grp || !thunk) return(NULL);

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

/* ============================================================== */

QofBackend *
xaccGroupGetBackend (const AccountGroup *grp)
{
  grp = xaccGroupGetRoot (grp);
  if (!grp || !grp->book) return NULL;
  return qof_book_get_backend(grp->book);
}

/* ============================================================== */
/* gncObject function implementation and registration */

static void 
group_book_begin (QofBook *book)
{
  xaccSetAccountGroup (book, xaccMallocAccountGroup(book));
}

static void 
group_book_end (QofBook *book)
{
  xaccSetAccountGroup (book, NULL);
}

static gboolean
group_is_dirty (const QofCollection *col)
{
  return xaccGroupNotSaved(xaccCollGetAccountGroup(col));
}

static void
group_mark_clean(QofCollection *col)
{
  xaccGroupMarkSaved(xaccCollGetAccountGroup(col));
}

static QofObject group_object_def = 
{
  interface_version: QOF_OBJECT_VERSION,
  e_type:            GNC_ID_GROUP,
  type_label:        "AccountGroup",
  create:            (gpointer)xaccMallocAccountGroup,
  book_begin:        group_book_begin,
  book_end:          group_book_end,
  is_dirty:          group_is_dirty,
  mark_clean:        group_mark_clean,
  foreach:           NULL,
  printable:         NULL,
  version_cmp:       NULL,
};

gboolean 
xaccGroupRegister (void)
{
  return qof_object_register (&group_object_def);
}

/* ========================= END OF FILE ======================== */
