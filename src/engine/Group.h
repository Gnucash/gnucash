/********************************************************************\
 * Group.h -- chart of accounts (hierarchical tree of accounts)     *
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
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#ifndef XACC_ACCOUNT_GROUP_H
#define XACC_ACCOUNT_GROUP_H

#include "config.h"

#include <glib.h>

#include "Account.h"
#include "GNCId.h"
#include "gnc-common.h"


/** PROTOTYPES ******************************************************/
AccountGroup *xaccMallocAccountGroup (void);
void          xaccFreeAccountGroup (AccountGroup *account_group);
void          xaccAccountGroupBeginEdit (AccountGroup *grp);
void 	      xaccAccountGroupCommitEdit (AccountGroup *grp);

/*
 * The xaccGroupConcatGroup() subroutine will move all accounts
 *    from the "from" group to the "to" group
 *
 * The xaccGroupMergeAccounts() subroutine will go through a group,
 *    merging all accounts that have the same name and description.
 *    This function is useful when importing Quicken(TM) files.
 */

void    xaccGroupConcatGroup (AccountGroup *to, AccountGroup *from);
void    xaccGroupMergeAccounts (AccountGroup *grp);

/*
 * The xaccGroupNotSaved() subroutine will return TRUE
 *    if any account in the group or in any subgroup
 *    hasn't been saved.
 *
 * The xaccGroupMarkSaved() subroutine will mark
 *    the entire group as having been saved, including 
 *    all of the child accounts.
 *
 * The xaccGroupMarkNotSaved() subroutine will mark
 *    the given group as not having been saved.
 *
 * The xaccGroupMarkDoFree() subroutine will mark
 *    all accounts in the group as being destroyed.
 */

gboolean xaccGroupNotSaved  (AccountGroup *grp);
void     xaccGroupMarkSaved (AccountGroup *grp);
void     xaccGroupMarkNotSaved (AccountGroup *grp);

void     xaccGroupMarkDoFree (AccountGroup *grp);

/*
 * The xaccGroupRemoveAccount() subroutine will remove the indicated
 *    account from its parent account group. It will NOT free the
 *    associated memory or otherwise alter the account: the account
 *    can now be reparented to a new location.
 *    Note, however, that it will mark the old parents as having 
 *    been modified.
 *
 * The xaccAccountRemoveGroup() subroutine will remove the indicated
 *    account group from its parent account. It will NOT free the
 *    associated memory or otherwise alter the account group: the 
 *    account group can now be reparented to a new location.
 *    Note, however, that it will mark the old parents as having 
 *    been modified.
 */

void    xaccGroupRemoveAccount (AccountGroup *grp, Account *account);
void    xaccAccountRemoveGroup (Account *acc);
void    xaccGroupInsertAccount (AccountGroup *grp, Account *acc);
void    xaccAccountInsertSubAccount (Account *parent, Account *child);

/*
 * The xaccGroupGetNumSubAccounts() subroutine returns the number
 *    of accounts, including subaccounts, in the account group
 *
 * The xaccGroupGetNumAccounts() subroutine returns the number
 *    of accounts in the indicated group only (children not counted).
 *
 * The xaccGroupGetDepth() subroutine returns the length of the 
 *    longest tree branch.  Each link between an account and its
 *    (non-null) children counts as one unit of length.
 */

int     xaccGroupGetNumSubAccounts (AccountGroup *grp);
int     xaccGroupGetNumAccounts (AccountGroup *grp);
int     xaccGroupGetDepth (AccountGroup *grp);
Account * xaccGroupGetAccount (AccountGroup *group, int index);

/*
 * The xaccGroupGetSubAccounts() subroutine returns an list of the accounts,
 *    including subaccounts, in the account group. The returned list
 *    should be freed with g_list_free when no longer needed.
 *
 * The xaccGroupGetAccountList() subroutines returns only the immediate
 *    children of the account group. The returned list should *not*
 *    be freed by the caller.
 */

GList * xaccGroupGetSubAccounts (AccountGroup *grp);
GList * xaccGroupGetAccountList (AccountGroup *grp);

/* 
 * The xaccGetAccountFromName() subroutine fetches the
 *    account by name from the collection of accounts
 *    in the indicated AccountGroup group.  It returns NULL if the
 *    account was not found.
 *
 * The xaccGetAccountFromFullName() subroutine works like
 *    xaccGetAccountFromName, but uses fully-qualified names
 *    using the given separator.
 *
 * The xaccGetPeerAccountFromName() subroutine fetches the
 *    account by name from the collection of accounts
 *    in the same AccountGroup anchor group. It returns NULL if the
 *    account was not found.
 *
 * The xaccGetPeerAccountFromFullName() subroutine works like
 *     xaccGetPeerAccountFromName, but uses fully-qualified
 *     names using the given separator.
 */

Account *xaccGetAccountFromName     (AccountGroup *group, const char *name);
Account *xaccGetAccountFromFullName (AccountGroup *group,
                                     const char *name,
                                     const char separator);
Account *xaccGetPeerAccountFromName (Account *account, const char *name);
Account *xaccGetPeerAccountFromFullName (Account *acc,
                                         const char * name,
                                         const char separator);

/*
 * The xaccGetAccountRoot () subroutine will find the topmost 
 *    (root) group to which this account belongs.
 */
AccountGroup * xaccGetAccountRoot (Account *account);

/* The xaccGroupGetParentAccount() subroutine returns the parent
 * account of the group, or NULL.
 */
Account * xaccGroupGetParentAccount (AccountGroup *group);

/* if the function returns null for a given item, it won't show up in
   the result list */
GSList *xaccGroupMapAccounts(AccountGroup *grp,
                             gpointer (*thunk)(Account *a, gpointer data),
                             gpointer data);

/* The xaccGroupForEachAccount() method will traverse the AccountGroup
 *    tree, calling 'func' on each account.   Traversal will stop when
 *    func returns a non-null value, and the routine wil return with that 
 *    value.  If 'deeply' is FALSE, then only the immediate children of 
 *    the account will be traversed.  If TRUE, then the whole tree will
 *    be traversed.
 */

gpointer xaccGroupForEachAccount (AccountGroup *grp,
                                  gpointer (*func) (Account *a,
                                                     gpointer data),
                                  gpointer data,
                                  gboolean deeply);

gboolean xaccGroupEqual(AccountGroup *a, AccountGroup *b,
                        gboolean check_guids);

/*
 * The following functions provide support for "staged traversals"
 * over all of the transactions in an account or group.  The idea
 * is to be able to perform a sequence of traversals ("stages"),
 * and perform an operation on each transaction exactly once 
 * for that stage.  
 *
 * Only transactions whose current "stage" is less than the
 * stage of the current traversal will be affected, and they will
 * be "brought up" to the current stage when they are processed.
 *
 * For example, you could perform a stage 1 traversal of all the
 * transactions in an account, and then perform a stage 1 traversal of
 * the transactions in a second account.  Presuming the traversal of
 * the first account didn't abort prematurely, any transactions shared
 * by both accounts would be ignored during the traversal of the
 * second account since they had been processed while traversing the
 * first account.
 *
 * However, if you had traversed the second account using a stage 
 * of 2, then all the transactions in the second account would have 
 * been processed.
 *
 * Traversal can be aborted by having the callback function return
 * a non-zero value.  The traversal is aborted immediately, and the 
 * non-zero value is returned.  Note that an aborted traversal can 
 * be restarted; no information is lost due to an abort.
 *
 * The initial impetus for this particular approach came from
 * generalizing a mark/sweep practice that was already being
 * used in FileIO.c.
 *
 * Note that currently, there is a hard limit of 256 stages, which
 * can be changed by enlarging "marker" in the transaction struct.
 * */

/* xaccGroupBeginStagedTransactionTraversals() resets the traversal
 *    marker inside each of all the transactions in the group so that
 *    a new sequence of staged traversals can begin.
 *
 * xaccSplitsBeginStagedTransactionTraversals() resets the traversal
 *    marker for each transaction which is a parent of one of the
 *    splits in the list.
 *
 * xaccAccountBeginStagedTransactionTraversals() resets the traversal
 *    marker for each transaction which is a parent of one of the
 *    splits in the account.
 *
 */

void xaccGroupBeginStagedTransactionTraversals(AccountGroup *grp);
void xaccSplitsBeginStagedTransactionTraversals(GList *splits);
void xaccAccountBeginStagedTransactionTraversals(Account *account);

/* xaccTransactionTraverse() checks the stage of the given transaction.
 *    If the transaction hasn't reached the given stage, the transaction
 *    is updated to that stage and the function returns TRUE. Otherwise
 *    no change is made and the function returns FALSE.
 *
 * xaccSplitTransactionTraverse() behaves as above using the parent of
 *    the given split.
 */

gboolean xaccTransactionTraverse(Transaction *trans, int stage);
gboolean xaccSplitTransactionTraverse(Split *split, int stage);

/* xaccGroupStagedTransactionTraversal() calls thunk on each
 *    transaction in the group whose current marker is less than the
 *    given `stage' and updates each transaction's marker to be `stage'.
 *    The traversal will stop if thunk() returns a non-zero value.
 *    xaccGroupStagedTransactionTraversal() function will return zero 
 *    or the non-zero value returned by thunk().  This
 *    API does not handle handle recursive traversals.
 *
 *    Currently the result of adding or removing transactions during
 *    a traversal is undefined, so don't do that.
 */

int 
xaccGroupStagedTransactionTraversal(AccountGroup *grp,
                                    unsigned int stage,
                                    int (*thunk)(Transaction *t, void *data),
                                    void *data);

/* xaccAccountStagedTransactionTraversal() calls thunk on each
 *    transaction in the account whose current marker is less than the
 *    given `stage' and updates each transaction's marker to be `stage'.
 *    The traversal will stop if thunk() returns a non-zero value.
 *    xaccAccountStagedTransactionTraversal() function will return zero
 *    or the non-zero value returned by thunk().
 *    This API does not handle handle recursive traversals.
 *
 *    Currently the result of adding or removing transactions during
 *    a traversal is undefined, so don't do that. 
 */

int xaccAccountStagedTransactionTraversal(Account *a,
                                          unsigned int stage,
                                          int (*thunk)(Transaction *t,
                                                       void *data),
                                          void *data);

/* Traverse all of the transactions in the given account group.
   Continue processing IFF proc does not return FALSE. This function
   will descend recursively to traverse transactions in the
   children of the accounts in the group.

   Proc will be called exactly once for each transaction that is
   pointed to by at least one split in any account in the hierarchy
   topped by AccountGroup g.

   Note too, that if you call this function on two separate account
   groups and those accounts groups share transactions, proc will be
   called once per account on the shared transactions.

   The result of this function will not be FALSE IFF every relevant
   transaction was traversed exactly once.  */
gboolean
xaccGroupForEachTransaction(AccountGroup *g,
                            gboolean (*proc)(Transaction *t, void *data),
                            void *data);

/* Visit every transaction in the account that hasn't already been
   visited exactly once.  visited_txns must be a hash table created
   via guid_hash_table_new() and is the authority about which
   transactions have already been visited.  Further, when this
   procedure returns, visited_txns will have been modified to reflect
   all the newly visited transactions.

   The result of this function will not be FALSE IFF every relevant
   transaction was traversed exactly once.  */
gboolean
xaccGroupVisitUnvisitedTransactions(AccountGroup *g,
                                    gboolean (*proc)(Transaction *t,
                                                     void *data),
                                    void *data,
                                    GHashTable *visited_txns);

#endif /* XACC_ACCOUNT_GROUP_H */
