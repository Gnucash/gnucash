/********************************************************************\
 * Account.c -- Account data structure implementation               *
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

#include "config.h"

#include <assert.h>
#include <glib.h>
#include <string.h>

#include "Account.h"
#include "AccountP.h"
#include "BackendP.h"
#include "GNCIdP.h"
#include "Group.h"
#include "GroupP.h"
#include "Transaction.h"
#include "TransactionP.h"
#include "date.h"
#include "gnc-commodity.h"
#include "gnc-engine-util.h"
#include "gnc-event-p.h"
#include "kvp_frame.h"
#include "messages.h"

static short module = MOD_ENGINE; 


/********************************************************************\
 * Because I can't use C++ for this project, doesn't mean that I    *
 * can't pretend to!  These functions perform actions on the        *
 * account data structure, in order to encapsulate the knowledge    *
 * of the internals of the Account in one file.                     *
\********************************************************************/

static void xaccAccountBringUpToDate(Account *acc);

/********************************************************************\
\********************************************************************/

G_INLINE_FUNC void mark_account (Account *account);
G_INLINE_FUNC void
mark_account (Account *account)
{
  if (account->parent)
    account->parent->saved = FALSE;

  gnc_engine_generate_event (&account->guid, GNC_EVENT_MODIFY);
}

/********************************************************************\
\********************************************************************/

static void
xaccInitAccount (Account * acc)
{
  acc->parent   = NULL;
  acc->children = NULL;

  acc->balance = gnc_numeric_zero();
  acc->cleared_balance = gnc_numeric_zero();
  acc->reconciled_balance = gnc_numeric_zero();

  acc->share_balance = gnc_numeric_zero();
  acc->share_cleared_balance = gnc_numeric_zero();
  acc->share_reconciled_balance = gnc_numeric_zero();

  acc->starting_balance = gnc_numeric_zero();
  acc->starting_cleared_balance = gnc_numeric_zero();
  acc->starting_reconciled_balance = gnc_numeric_zero();

  acc->starting_share_balance = gnc_numeric_zero();
  acc->starting_share_cleared_balance = gnc_numeric_zero();
  acc->starting_share_reconciled_balance = gnc_numeric_zero();

  acc->type = NO_TYPE;

  acc->accountName = g_strdup("");
  acc->accountCode = g_strdup("");
  acc->description = g_strdup("");

  acc->kvp_data    = kvp_frame_new();

  acc->currency    = NULL;
  acc->security    = NULL;
  acc->currency_scu = 100000;
  acc->security_scu = 100000;

  acc->splits      = NULL;

  acc->version = 0;
  acc->editlevel = 0;
  acc->balance_dirty = FALSE;
  acc->sort_dirty = FALSE;
  acc->core_dirty = FALSE;
  acc->do_free = FALSE;

  xaccGUIDNew(&acc->guid);
  xaccStoreEntity(acc, &acc->guid, GNC_ID_ACCOUNT);
  LEAVE ("account=%p\n", acc);
}

/********************************************************************\
\********************************************************************/

Account *
xaccMallocAccount (void)
{
  Account *acc = g_new (Account, 1);

  xaccInitAccount (acc);

  gnc_engine_generate_event (&acc->guid, GNC_EVENT_CREATE);

  return acc;
}

Account *
xaccCloneAccountSimple(const Account *from)
{
    Account *ret;

    ret = xaccMallocAccount();
    xaccInitAccount(ret);
    
    ret->type = from->type;

    ret->accountName = g_strdup(from->accountName);
    ret->accountCode = g_strdup(from->accountCode);
    ret->description = g_strdup(from->description);

    ret->kvp_data    = kvp_frame_copy(from->kvp_data);

    ret->currency    = from->currency;

    return ret;
}

/********************************************************************\
\********************************************************************/

void
xaccFreeAccount (Account *acc)
{
  Transaction *t;
  GList *lp;

  if (NULL == acc) return;

  gnc_engine_generate_event (&acc->guid, GNC_EVENT_DESTROY);

  xaccRemoveEntity(&acc->guid);

  if (acc->children) 
  {
    PERR (" xinstead of calling xaccFreeAccount(), please call \n"
          " xaccAccountBeginEdit(); xaccAccountDestroy(); \n");
  
     /* First, recursively free children */
     xaccFreeAccountGroup (acc->children);
     acc->children = NULL;
  }

  /* Next, clean up the splits */
  /* NB there shouldn't be any splits by now ... they should 
   * have been all been freed by CommitEdit().  We can remove this
   * check once we know the warning isn't occurring any more. */
  if (acc->splits) 
  {
    PERR (" instead of calling xaccFreeAccount(), please call \n"
          " xaccAccountBeginEdit(); xaccAccountDestroy(); \n");
  
    /* any split pointing at this account needs to be unmarked */
    for(lp = acc->splits; lp; lp = lp->next) 
    {
      xaccSplitSetAccount((Split *) lp->data, NULL);
    }
  
    acc->editlevel = 0;
  
    for(lp = acc->splits; lp; lp = lp->next) {
      Split *s = (Split *) lp->data;
      t = s->parent;
      xaccTransBeginEdit (t);
      xaccSplitDestroy (s);
      xaccTransCommitEdit (t);
    }
  
    /* free up array of split pointers */
    g_list_free(acc->splits);
    acc->splits = NULL;
  }

  if (acc->accountName) g_free (acc->accountName);
  acc->accountName = NULL;
  if (acc->accountCode) g_free (acc->accountCode);
  acc->accountCode = NULL;
  if (acc->description) g_free (acc->description);
  acc->description = NULL;

  kvp_frame_delete (acc->kvp_data);
  acc->kvp_data = NULL;

  /* zero out values, just in case stray 
   * pointers are pointing here. */

  acc->currency = NULL;
  acc->security = NULL;

  acc->parent   = NULL;
  acc->children = NULL;

  acc->balance  = gnc_numeric_zero();
  acc->cleared_balance = gnc_numeric_zero();
  acc->reconciled_balance = gnc_numeric_zero();

  acc->share_balance = gnc_numeric_zero();
  acc->share_cleared_balance = gnc_numeric_zero();
  acc->share_reconciled_balance = gnc_numeric_zero();

  acc->type = NO_TYPE;

  acc->accountName = NULL;
  acc->description = NULL;
  acc->currency    = NULL;
  acc->security    = NULL;

  acc->version = 0;
  acc->editlevel = 0;
  acc->balance_dirty = FALSE;
  acc->sort_dirty = FALSE;
  acc->core_dirty = FALSE;

  g_free(acc);
}

/********************************************************************\
 * transactional routines
\********************************************************************/

void 
xaccAccountBeginEdit (Account *acc) 
{
  Backend * be;
  if (!acc) return;

  acc->editlevel++;
  if (1 < acc->editlevel) return;

  if (0 >= acc->editlevel) 
  {
    PERR ("unbalanced call - resetting (was %d)", acc->editlevel);
    acc->editlevel = 1;
  }

  acc->core_dirty = FALSE;

  /* See if there's a backend.  If there is, invoke it. */
  be = xaccAccountGetBackend (acc);
  if (be && be->account_begin_edit) {
     (be->account_begin_edit) (be, acc);
  }
}

void 
xaccAccountCommitEdit (Account *acc) 
{
  Backend * be;
  int rc;

  if (!acc) return;

  acc->editlevel--;
  if (0 < acc->editlevel) return;

  if (0 > acc->editlevel) 
  {
    PERR ("unbalanced call - resetting (was %d)", acc->editlevel);
    acc->editlevel = 0;
  }

  /* If marked for deletion, get rid of subaccounts first,
   * and then the splits ... */
  if (acc->do_free)
  {
    GList *lp;

    /* First, recursively free children */
    xaccFreeAccountGroup (acc->children);
    acc->children = NULL;

    PINFO ("freeing splits for account %p (%s)\n",
           acc, acc->accountName ? acc->accountName : "(null)");

    /* any split pointing at this account needs to be unmarked */
    for(lp = acc->splits; lp; lp = lp->next) 
    {
      xaccSplitSetAccount((Split *) lp->data, NULL);
    }

    for(lp = acc->splits; lp; lp = lp->next) 
    {
      Split *s = (Split *) lp->data;
      Transaction *t = s->parent;
      xaccTransBeginEdit (t);
      xaccSplitDestroy (s);
      xaccTransCommitEdit (t);
    }

    /* free up array of split pointers */
    g_list_free(acc->splits);
    acc->splits = NULL;

    acc->core_dirty = TRUE;
  }
  else 
  {
    xaccAccountBringUpToDate(acc);
  }

  /* See if there's a backend.  If there is, invoke it. */
  be = xaccAccountGetBackend (acc);
  if (be && be->account_commit_edit) 
  {
    rc = (be->account_commit_edit) (be, acc);
    /* hack alert -- we really really should be checking 
     * for errors returned by the back end ... */
    if (rc)
    {
      /* destroys must be rolled back as well ... ??? */
      acc->do_free = FALSE;
      PERR (" backend asked engine to rollback, but this isn't"
            " handled yet. Return code=%d", rc);
    }
  }
  acc->core_dirty = FALSE;

  /* final stages of freeing the account */
  if (acc->do_free)
  {
    xaccGroupRemoveAccount(acc->parent, acc);
    xaccFreeAccount(acc);
  }
}

void 
xaccAccountDestroy (Account *acc) 
{
  if (!acc) return;
  acc->do_free = TRUE;

  xaccAccountCommitEdit (acc);
}


void 
xaccAccountSetVersion (Account *acc, gint32 vers)
{
  if (!acc) return;
  acc->version = vers;
}

gint32 
xaccAccountGetVersion (Account *acc)
{
  if (!acc) return 0;
  return (acc->version);
}

/********************************************************************\
\********************************************************************/

gboolean
xaccAccountEqual(Account *aa, Account *ab, gboolean check_guids) {
  if(!aa && !ab) return TRUE;
  if(!aa) return FALSE;
  if(!ab) return FALSE;

  if(aa->type != ab->type) return FALSE;

  if(safe_strcmp(aa->accountName, ab->accountName) != 0) return FALSE;
  if(safe_strcmp(aa->accountCode, ab->accountCode) != 0) return FALSE;
  if(safe_strcmp(aa->description, ab->description) != 0) return FALSE;
  if(!gnc_commodity_equiv(aa->currency, ab->currency)) return FALSE;
  if(!gnc_commodity_equiv(aa->security, ab->security)) return FALSE;

  if(check_guids) {
    if(!guid_equal(&aa->guid, &ab->guid)) return FALSE;
  }

  if(kvp_frame_compare(aa->kvp_data, ab->kvp_data) != 0) return FALSE;

  /* no parent; always compare downwards. */

  {
    GList *la = aa->splits;
    GList *lb = ab->splits;

    if( la && !lb) return FALSE;
    if(!la &&  lb) return FALSE;
    if(la && lb) {
      /* presume that the splits are in the same order */
      while(la && lb) {
        Split *sa = (Split *) la->data;
        Split *sb = (Split *) lb->data;
        if(!xaccSplitEqual(sa, sb, check_guids, FALSE)) return(FALSE);
        la = la->next;
        lb = lb->next;
      }
      if((la != NULL) || (lb != NULL)) return(FALSE);
    }
  }

  if(!xaccGroupEqual(aa->children, ab->children, check_guids)) return FALSE;

  return(TRUE);
}

/********************************************************************\
\********************************************************************/

static gint
split_sort_func(gconstpointer a, gconstpointer b) {
  /* don't coerce xaccSplitDateOrder so we'll catch changes */
  Split *sa = (Split *) a;
  Split *sb = (Split *) b;
  return(xaccSplitDateOrder(sa, sb));
}

static void
xaccAccountSortSplits (Account *acc) 
{
  if(!acc) return;

  if(!acc->sort_dirty) return;
  if(acc->editlevel > 0) return;
  acc->splits = g_list_sort(acc->splits, split_sort_func);
  acc->sort_dirty = FALSE;
}

static void
xaccAccountBringUpToDate(Account *acc) 
{
  if(!acc) return;

  /* if a re-sort happens here, then everything will update, so the
     cost basis and balance calls are no-ops */
  xaccAccountSortSplits(acc);
  xaccAccountRecomputeBalance(acc);
}


/********************************************************************
 * xaccAccountGetSlots
 ********************************************************************/

kvp_frame * 
xaccAccountGetSlots(Account * account) {
  return(account->kvp_data);
}

void
xaccAccountSetSlots_nc(Account *account, kvp_frame *frame)
{
  if (!account) return;
  if (frame == account->kvp_data) return;

  xaccAccountBeginEdit (account);   
  if(account->kvp_data)
  {
      kvp_frame_delete (account->kvp_data);
  }
  account->kvp_data = frame;
  xaccAccountCommitEdit (account);   
}


/********************************************************************\
\********************************************************************/

const GUID *
xaccAccountGetGUID (Account *account)
{
  if (!account)
    return xaccGUIDNULL();

  return &account->guid;
}

/********************************************************************\
\********************************************************************/

void 
xaccAccountSetGUID (Account *account, const GUID *guid)
{
  if (!account || !guid) return;

  PINFO("acct=%p", account);
  xaccAccountBeginEdit (account);
  xaccRemoveEntity(&account->guid);

  account->guid = *guid;

  xaccStoreEntity(account, &account->guid, GNC_ID_ACCOUNT);
  account->core_dirty = TRUE;
  xaccAccountCommitEdit (account);
}

/********************************************************************\
\********************************************************************/

Account *
xaccAccountLookup (const GUID *guid)
{
  if (!guid) return NULL;
  return xaccLookupEntity(guid, GNC_ID_ACCOUNT);
}

/********************************************************************\
\********************************************************************/

short
xaccAccountGetMark (Account *acc)
{
  if (!acc) return 0;
  return acc->mark;
}

void
xaccAccountSetMark (Account *acc, short m)
{
  if (!acc) return;
  acc->mark = m;
}

void
xaccClearMark (Account *acc, short val)
{
  AccountGroup *topgrp;

  if (!acc) return;
  topgrp = xaccGetAccountRoot (acc);

  if (topgrp)
  {
    GList *list;
    GList *node;

    list = xaccGroupGetAccountList (topgrp);

    for (node = list; node; node = node->next)
    {
      Account *account = node->data;

      xaccClearMarkDown (account, val);
    }
  }
  else
    xaccClearMarkDown (acc, val);
}

void
xaccClearMarkDown (Account *acc, short val)
{
  AccountGroup *children;

  if (!acc) return;
  acc->mark = val;

  children = acc->children;
  if (children)
  {
    GList *list;
    GList *node;

    list = xaccGroupGetAccountList (children);

    for (node = list; node; node = node->next)
    {
      Account *account = node->data;

      xaccClearMarkDown (account, val);
    }
  }
}

void
xaccClearMarkDownGr (AccountGroup *grp, short val)
{
  GList *list;
  GList *node;

  if (!grp) return;

  list = xaccGroupGetAccountList (grp);

  for (node = list; node; node = node->next)
  {
    Account *account = node->data;

    xaccClearMarkDown (account, val);
  }
}

/********************************************************************\
\********************************************************************/

void
xaccAccountInsertSplit (Account *acc, Split *split)
{
  if (!acc) return;
  if (!split) return;

  /* Make sure the currencies in the transaction will still
   * be acceptable. This means either the currency or the security
   * of the new account must be 'in common' with the currencies used
   * in the transaction. */
#if 0
  if (xaccTransCountSplits(split->parent) > 1) {
    if (!xaccTransIsCommonCurrency(split->parent, acc->currency) &&
        !xaccTransIsCommonCurrency(split->parent, acc->security))
      
      return;
  }
#endif

  xaccAccountBeginEdit(acc);

  acc->balance_dirty = TRUE;
  acc->sort_dirty = TRUE;

  /* convert the split to the new account's denominators */
  /* if the denominator can't be exactly converted, it's an error */
  /* FIXME : need to enforce ordering of insertion/value */
  split->damount = gnc_numeric_convert(split->damount, 
                                       xaccAccountGetSecuritySCU(acc),
                                       GNC_RND_ROUND);

  split->value   = gnc_numeric_convert(split->value, 
                                       xaccAccountGetCurrencySCU(acc),
                                       GNC_RND_ROUND);

  /* if this split belongs to another account, remove it from there
     * first.  We don't want to ever leave the system in an inconsistent
     * state.  Note that it might belong to the current account if we're
     * just using this call to re-order.  */
  if (xaccSplitGetAccount(split) &&
      xaccSplitGetAccount(split) != acc)
      xaccAccountRemoveSplit (xaccSplitGetAccount(split), split);
  xaccSplitSetAccount(split, acc);

  if(g_list_index(acc->splits, split) == -1)
  {
      if (acc->editlevel == 1)
      {
          acc->splits = g_list_insert_sorted(acc->splits, split,
                                             split_sort_func);
          acc->sort_dirty = FALSE;
      }
      else
          acc->splits = g_list_prepend(acc->splits, split);

      mark_account (acc);
      if (split->parent)
          gnc_engine_generate_event (&split->parent->guid, GNC_EVENT_MODIFY);
  }
  
  xaccAccountCommitEdit(acc);
}

/********************************************************************\
\********************************************************************/

void
xaccAccountRemoveSplit (Account *acc, Split *split)
{
  if (!acc) return;
  if (!split) return;
  
  xaccAccountBeginEdit(acc);
  {
    GList *node;

    node = g_list_find (acc->splits, split);
    if (!node)
    {
      PERR ("split not in account");
    }
    else
    {
      acc->splits = g_list_remove_link (acc->splits, node);
      g_list_free_1 (node);

      acc->balance_dirty = TRUE;
      xaccSplitSetAccount(split, NULL);

      mark_account (acc);
      if (split->parent)
        gnc_engine_generate_event (&split->parent->guid, GNC_EVENT_MODIFY);
    }
  }
  xaccAccountCommitEdit(acc);
}


/********************************************************************\
 * xaccAccountRecomputeBalance                                      *
 *   recomputes the partial balances and the current balance for    *
 *   this account.                                                  *
 *                                                                  *
 * The way the computation is done depends on whether the partial   *
 * balances are for a monetary account (bank, cash, etc.) or a      *
 * certificate account (stock portfolio, mutual fund).  For bank    *
 * accounts, the invariant amount is the dollar amount. For share   *
 * accounts, the invariant amount is the number of shares. For      *
 * share accounts, the share price fluctuates, and the current      *
 * value of such an account is the number of shares times the       *
 * current share price.                                             *
 *                                                                  *
 * Part of the complexity of this computation stems from the fact   *
 * xacc uses a double-entry system, meaning that one transaction    *
 * appears in two accounts: one account is debited, and the other   *
 * is credited.  When the transaction represents a sale of shares,  *
 * or a purchase of shares, some care must be taken to compute      *
 * balances correctly.  For a sale of shares, the stock account must*
 * be debited in shares, but the bank account must be credited      *
 * in dollars.  Thus, two different mechanisms must be used to      *
 * compute balances, depending on account type.                     *
 *                                                                  *
 * Args:   account -- the account for which to recompute balances   *
 * Return: void                                                     *
\********************************************************************/

static gnc_numeric
price_xfer(Split * s, gnc_numeric share_count) {
  gnc_numeric temp;
  if(!gnc_numeric_zero_p(s->damount)) {
    temp = gnc_numeric_div(s->value, s->damount,
                           1000000, GNC_DENOM_LCD);
    temp = gnc_numeric_mul(share_count, temp,
                           gnc_numeric_denom(s->value),
                           GNC_RND_ROUND);
    return temp;
  }
  else {
    return gnc_numeric_zero();
  }
}      
                              
void
xaccAccountRecomputeBalance (Account * acc)
{
  gnc_numeric  dbalance;
  gnc_numeric  dcleared_balance; 
  gnc_numeric  dreconciled_balance;
  gnc_numeric  share_balance; 
  gnc_numeric  share_cleared_balance; 
  gnc_numeric  share_reconciled_balance;
  Split   *last_split = NULL;
  GList   *lp;

  if(NULL == acc) return;
  if(acc->editlevel > 0) return;
  if(!acc->balance_dirty) return;
  if(acc->do_free) return;

  dbalance =                 acc->starting_balance;
  dcleared_balance =         acc->starting_cleared_balance;
  dreconciled_balance =      acc->starting_reconciled_balance;
  share_balance =            acc->starting_share_balance;
  share_cleared_balance =    acc->starting_share_cleared_balance;
  share_reconciled_balance = acc->starting_share_reconciled_balance;

  for(lp = acc->splits; lp; lp = lp->next) {
    Split *split = (Split *) lp->data;

    /* compute both dollar and share balances */
    share_balance = gnc_numeric_add_fixed(share_balance, split->damount);
    dbalance      = gnc_numeric_add_fixed(dbalance, split->value);

    if( NREC != split -> reconciled ) {
      share_cleared_balance = 
        gnc_numeric_add_fixed(share_cleared_balance, split->damount);
      dcleared_balance = 
        gnc_numeric_add_fixed(dcleared_balance, split->value);
    }

    if( YREC == split -> reconciled ) {
      share_reconciled_balance = 
        gnc_numeric_add_fixed(share_cleared_balance, split->damount);
      dreconciled_balance =  
        gnc_numeric_add_fixed(dreconciled_balance, split->value);
    }

    /* For bank accounts, the invariant subtotal is the dollar
     * amount.  For stock accounts, the invariant is the share amount */
    if ( (STOCK    == acc->type) ||
         (MUTUAL   == acc->type) ||
         (CURRENCY == acc->type) ) {
      split -> share_balance = share_balance;
      split -> share_cleared_balance = share_cleared_balance;
      split -> share_reconciled_balance = share_reconciled_balance;
      split -> balance = price_xfer(split, share_balance);
      split -> cleared_balance = price_xfer(split, share_cleared_balance);
      split -> reconciled_balance =
        price_xfer(split, share_reconciled_balance);
    }
    else {
      split -> share_balance = dbalance;
      split -> share_cleared_balance = dcleared_balance;
      split -> share_reconciled_balance = dreconciled_balance;
      split -> balance = dbalance;
      split -> cleared_balance = dcleared_balance;
      split -> reconciled_balance = dreconciled_balance;
    }

    last_split = split;
  }

  if ( (STOCK == acc->type)  ||
       (MUTUAL == acc->type) ||
       (CURRENCY == acc->type) ) {
    acc -> share_balance = share_balance;
    acc -> share_cleared_balance = share_cleared_balance;
    acc -> share_reconciled_balance = share_reconciled_balance;
    if (last_split) {
      acc -> balance = price_xfer(last_split, share_balance);
      acc -> cleared_balance = price_xfer(last_split, share_cleared_balance);
      acc -> reconciled_balance = 
        price_xfer(last_split, share_reconciled_balance);
    } 
    else {
      acc -> balance = dbalance;
      acc -> cleared_balance = dcleared_balance;
      acc -> reconciled_balance = dreconciled_balance;
    }
  } else {
    acc -> share_balance = dbalance;
    acc -> share_cleared_balance = dcleared_balance;
    acc -> share_reconciled_balance = dreconciled_balance;
    acc -> balance = dbalance;
    acc -> cleared_balance = dcleared_balance;
    acc -> reconciled_balance = dreconciled_balance;
  }

  acc->balance_dirty = FALSE;
  return;
}

/********************************************************************\
\********************************************************************/

void 
xaccAccountSetStartingBalance(Account *acc,
                              const gnc_numeric start_baln,
                              const gnc_numeric start_cleared_baln,
                              const gnc_numeric start_reconciled_baln)  
{
  if (!acc) return;

  /* hack alert -- this routine is meant to set the one and only
   * account commodity starting balance. However, until we remove
   * the 'reporting currency' from accounts, we will have to guess
   * which to set based on the account type.
   */
  switch (acc->type) 
  {
     case BANK:
     case CASH:
     case CREDIT:
     case ASSET:
     case LIABILITY:
     case INCOME:
     case EXPENSE:
     case EQUITY:
        acc->starting_balance = start_baln;
        acc->starting_cleared_balance = start_cleared_baln;
        acc->starting_reconciled_balance = start_reconciled_baln;
        break;
     case STOCK:
     case MUTUAL:
     case CURRENCY:
        acc->starting_share_balance = start_baln;
        acc->starting_share_cleared_balance = start_cleared_baln;
        acc->starting_share_reconciled_balance = start_reconciled_baln;
        break;
     default:
  }

  acc->balance_dirty = TRUE;
}

/********************************************************************\
 * xaccAccountFixSplitDateOrder                                     *
 *   check this split to see if the date is in correct order        *
 *   If it is not, reorder the transactions ...                     *
 *                                                                  *
 * Args:   acc   -- the account to check                            *
 *         split -- the split to check                              *
 *                                                                  *
 * Return: int -- non-zero if out of order                          *
\********************************************************************/

void
xaccAccountFixSplitDateOrder (Account * acc, Split *split) 
{
  if (NULL == acc) return;
  if (NULL == split) return;

  if (acc->do_free) return;

  acc->sort_dirty = TRUE;
  acc->balance_dirty = TRUE;

  if (acc->editlevel > 0) return;

  xaccAccountBringUpToDate (acc);
}

/********************************************************************\
 * xaccCheckTransDateOrder                                          *
 *   check this transaction to see if the date is in correct order  *
 *   If it is not, reorder the transactions.                        *
 *   This routine perfroms the check for both of the double-entry   *
 *   transaction entries.                                           *
 *                                                                  *
 * Args:   trans -- the transaction to check                        *
 * Return: int -- non-zero if out of order                          *
\********************************************************************/

void
xaccTransFixSplitDateOrder (Transaction *trans)
{
  GList *node;

  if (trans == NULL) return;

  for (node = trans->splits; node; node = node->next)
  {
    Split *s = node->data;
    xaccAccountFixSplitDateOrder (xaccSplitGetAccount(s), s);
  }
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
xaccAccountOrder (Account **aa, Account **ab) {
  char *da, *db;
  char *endptr = NULL;
  int ta, tb;
  long la, lb;

  if ( (*aa) && !(*ab) ) return -1;
  if ( !(*aa) && (*ab) ) return +1;
  if ( !(*aa) && !(*ab) ) return 0;

  /* sort on accountCode strings */
  da = (*aa)->accountCode;
  db = (*ab)->accountCode;

  /* If accountCodes are both base 36 integers do an integer sort */
  la = strtoul (da, &endptr, 36);
  if((*da != '\0') && (*endptr == '\0')) {
    lb = strtoul (db, &endptr, 36);
    if((*db != '\0') && (*endptr == '\0')) {
      if (la < lb) return -1;
      if (la > lb) return +1;
    }
  }

  /* Otherwise do a string sort */
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

  return 0;
}

/********************************************************************\
\********************************************************************/
/* account codes will be assigned base-36, with three digits */

#define BASE 36

void 
xaccAccountAutoCode (Account *acc, int digits) {
  if (!acc) return;
  if (acc->accountCode) return;   /* no-op if code already assinged */
  if (!(acc->parent)) return; 

  xaccAccountBeginEdit(acc);
  {
    acc->accountCode = xaccGroupGetNextFreeCode (acc->parent, digits);
    acc->parent->saved = FALSE;
    acc->core_dirty = TRUE;
  }
  xaccAccountCommitEdit(acc);
}

/********************************************************************\
\********************************************************************/

void 
xaccAccountSetType (Account *acc, int tip) {

  if (!acc) return;

  xaccAccountBeginEdit(acc);
  {
    /* refuse invalid account types, and don't bother if not new type. */
    if((NUM_ACCOUNT_TYPES > tip) && (acc->type != tip)) {
      acc->type = tip;
      acc->balance_dirty = TRUE; /* new type may affect balance computation */
    }

    mark_account (acc);
  }
  acc->core_dirty = TRUE;
  xaccAccountCommitEdit(acc);
}

void 
xaccAccountSetName (Account *acc, const char *str) {
   char * tmp;

   if ((!acc) || (!str)) return;

   xaccAccountBeginEdit(acc);
   {
     /* make strdup before freeing (just in case str==accountName !!) */
     tmp = g_strdup (str);
     g_free (acc->accountName);
     acc->accountName = tmp;

     mark_account (acc);
   }
   acc->core_dirty = TRUE;
   xaccAccountCommitEdit(acc);
}

void 
xaccAccountSetCode (Account *acc, const char *str) {
   char * tmp;
   if ((!acc) || (!str)) return;

   xaccAccountBeginEdit(acc);
   {
     /* make strdup before freeing */
     tmp = g_strdup (str);
     g_free (acc->accountCode);
     acc->accountCode = tmp;

     mark_account (acc);
   }
   acc->core_dirty = TRUE;
   xaccAccountCommitEdit(acc);
}

void
xaccAccountSetDescription (Account *acc, const char *str) {
   char * tmp;
   if ((!acc) || (!str)) return;

   xaccAccountBeginEdit(acc);
   {
     /* make strdup before freeing (just in case str==description !!) */
     tmp = g_strdup (str);
     g_free (acc->description);
     acc->description = tmp;

     mark_account (acc);
   }
   acc->core_dirty = TRUE;
   xaccAccountCommitEdit(acc);
}

void
xaccAccountSetNotes (Account *acc, const char *str) 
{
  if ((!acc) || (!str)) return;

  xaccAccountBeginEdit(acc);
  kvp_frame_set_slot_nc(acc->kvp_data, "notes", 
                        kvp_value_new_string(str));
  mark_account (acc);
  acc->core_dirty = TRUE;
  xaccAccountCommitEdit(acc);
}

/* FIXME : is this the right way to do this? */
static void
update_split_currency(Account * acc) 
{
  GList *lp;

  if(!acc) return;

  xaccAccountBeginEdit(acc);
  /* iterate over splits */
  for(lp = acc->splits; lp; lp = lp->next) {
    Split *s = (Split *) lp->data;
    s->value = gnc_numeric_convert(s->value,
                                   xaccAccountGetCurrencySCU(acc),
                                   GNC_RND_ROUND);
    s->damount = gnc_numeric_convert(s->damount,
                                     xaccAccountGetSecuritySCU(acc),
				     GNC_RND_ROUND);
  }
  xaccAccountCommitEdit(acc);
}

/********************************************************************\
\********************************************************************/
/* This is an experimental implementation of set commodity.  In the
 * long haul, it will need to set the one and only commodity field.
 * But in the interim phase, we try to guess right ...
 */

void 
xaccAccountSetCommodity (Account * acc, gnc_commodity * com) 
{
  if ((!acc) || (!com)) return;

  xaccAccountBeginEdit(acc);
  switch (acc->type) 
  {
     case BANK:
     case CASH:
     case CREDIT:
     case ASSET:
     case LIABILITY:
     case INCOME:
     case EXPENSE:
     case EQUITY:
        xaccAccountSetCurrency (acc, com);
        break;
     case STOCK:
     case MUTUAL:
     case CURRENCY:
        xaccAccountSetSecurity (acc, com);
        break;
     default:
  }
  acc->core_dirty = TRUE;
  xaccAccountCommitEdit(acc);
}

/********************************************************************\
\********************************************************************/
/* below follow the old, deprecated currency/security routines. */

void 
xaccAccountSetCurrency (Account * acc, gnc_commodity * currency) {

  if ((!acc) || (!currency)) return;
  
  xaccAccountBeginEdit(acc);
  {
    acc->currency     = currency;
    acc->currency_scu = gnc_commodity_get_fraction(currency);
    update_split_currency(acc);

    acc->sort_dirty = TRUE;
    acc->balance_dirty = TRUE;

    mark_account (acc);
  }
  acc->core_dirty = TRUE;
  xaccAccountCommitEdit(acc);
}

void 
xaccAccountSetSecurity (Account *acc, gnc_commodity * security) {
  
  if ((!acc) || (!security)) return;
  
  xaccAccountBeginEdit(acc);
  {
    acc->security     = security;
    acc->security_scu = gnc_commodity_get_fraction(security);    
    update_split_currency(acc);

    acc->sort_dirty = TRUE;
    acc->balance_dirty = TRUE;

    mark_account (acc);
  }
  acc->core_dirty = TRUE;
  xaccAccountCommitEdit(acc);
}

void 
xaccAccountSetCurrencySCU (Account * acc, int scu) {

  if (!acc) return;

  xaccAccountBeginEdit(acc);
  {
    acc->currency_scu = scu;
    mark_account (acc);
  }
  acc->core_dirty = TRUE;
  xaccAccountCommitEdit(acc);
}

void
xaccAccountSetSecuritySCU (Account *acc, int scu) {

  if (!acc) return;

  xaccAccountBeginEdit(acc);
  {
    acc->security_scu = scu;
    mark_account (acc);
  }
  acc->core_dirty = TRUE;
  xaccAccountCommitEdit(acc);
}

int
xaccAccountGetCurrencySCU (Account * acc) {
  if (!acc) return 0;
  return acc->currency_scu;
}

int
xaccAccountGetSecuritySCU (Account * acc) {
  if (!acc) return 0;

  if (acc->security == NULL)
    return acc->currency_scu;

  return acc->security_scu;
}


/********************************************************************\
\********************************************************************/

AccountGroup *
xaccAccountGetChildren (Account *acc)
{
   if (!acc) return NULL;
   if (acc->children == NULL)
       acc->children = xaccMallocAccountGroup ();
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

GNCAccountType
xaccAccountGetType (Account *acc)
{
   if (!acc) return NO_TYPE;
   return (acc->type);
}

const char *
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
  const char *name;
  char *p;
  int length;

  if (account == NULL)
    return g_strdup("");

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
  fullname = g_new(char, length);

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

const char *
xaccAccountGetCode (Account *acc)
{
   if (!acc) return NULL;
   return (acc->accountCode);
}

const char * 
xaccAccountGetDescription (Account *acc)
{
   if (!acc) return NULL;
   return (acc->description);
}

const char * 
xaccAccountGetNotes (Account *acc) 
{
  kvp_value *v;

  if (!acc) return NULL;
  v = kvp_frame_get_slot(acc->kvp_data, "notes");
  if(v) return(kvp_value_get_string(v));
  return(NULL);
}

gnc_commodity * 
xaccAccountGetCurrency (Account *acc)
{
  if (!acc) return NULL;
  return (acc->currency);
}

gnc_commodity * 
xaccAccountGetEffectiveSecurity (Account *acc)
{
  if (!acc) return NULL;

  if (acc->security == NULL)
    return acc->currency;

  return (acc->security);
}

gnc_commodity *
xaccAccountGetSecurity (Account *account)
{
  if (!account) return NULL;

  return account->security;
}

double
DxaccAccountGetBalance (Account *acc)
{
  return gnc_numeric_to_double(xaccAccountGetBalance(acc));
}

double
DxaccAccountGetClearedBalance (Account *acc)
{
  return gnc_numeric_to_double(xaccAccountGetClearedBalance(acc));
}

double
DxaccAccountGetReconciledBalance (Account *acc)
{
  return gnc_numeric_to_double(xaccAccountGetReconciledBalance(acc));
}

double
DxaccAccountGetShareBalance (Account *acc)
{
  return gnc_numeric_to_double(xaccAccountGetShareBalance(acc));
}

double
DxaccAccountGetShareClearedBalance (Account *acc)
{
  return gnc_numeric_to_double(xaccAccountGetShareClearedBalance(acc));
}

double
DxaccAccountGetShareReconciledBalance (Account *acc)
{
  return gnc_numeric_to_double(xaccAccountGetShareReconciledBalance(acc));
}

gnc_numeric
xaccAccountGetBalance (Account *acc) {
  if (!acc) return gnc_numeric_zero();
  return acc->balance;
}

gnc_numeric
xaccAccountGetClearedBalance (Account *acc)
{
   if (!acc) return gnc_numeric_zero();
   return acc->cleared_balance;
}

gnc_numeric
xaccAccountGetReconciledBalance (Account *acc)
{
   if (!acc) return gnc_numeric_zero();
   return acc->reconciled_balance;
}

gnc_numeric
xaccAccountGetShareBalance (Account *acc)
{
   if (!acc) return gnc_numeric_zero();
   return acc->share_balance;
}

gnc_numeric
xaccAccountGetShareClearedBalance (Account *acc)
{
   if (!acc) return gnc_numeric_zero();
   return acc->share_cleared_balance;
}

gnc_numeric
xaccAccountGetShareReconciledBalance (Account *acc)
{
  if (!acc) return gnc_numeric_zero();
  return acc->share_reconciled_balance;
}

/********************************************************************\
\********************************************************************/

static gnc_numeric
get_balance_as_of_date (Account *acc, time_t date, gboolean use_shares)
{
  /* This is common code to handle both xaccAccountGetBalanceAsOfDate
   * and xaccAccountGetShareBalanceAsOfDate.  use_shares is TRUE if the
   * share balance is being requested.
   */

  /* Ideally this could use xaccAccountForEachSplit, but
   * it doesn't exist yet and I'm uncertain of exactly how
   * it would work at this time, since it differs from
   * xaccAccountForEachTransaction by using gpointer return
   * values rather than gbooleans.
   */

  GList   *lp;
  Timespec ts, trans_ts;
  gboolean found = FALSE;
  gnc_numeric balance;
  
  balance = use_shares ?
    xaccAccountGetShareBalance( acc ) : xaccAccountGetBalance( acc );

  xaccAccountSortSplits( acc );   /* just in case, normally a nop */

  /* Since transaction post times are stored as a Timespec,
   * convert date into a Timespec as well rather than converting
   * each transaction's Timespec into a time_t.
   */

  ts.tv_sec = date;
  ts.tv_nsec = 0;

  /* Do checks from xaccAccountRecomputeBalance.  balance_dirty isn't
   * checked because it shouldn't be necessary.
   */

  if( NULL == acc ||
      acc->editlevel > 0 ||
      acc->do_free           )
  {
    return ( balance );
  }

  lp = xaccAccountGetSplitList( acc );
  while( lp && !found )
  {
    xaccTransGetDatePostedTS( xaccSplitGetParent( (Split *)lp->data ),
                              &trans_ts );
    if( timespec_cmp( &trans_ts, &ts ) > 0 )
      found = TRUE;
    else
      lp = lp->next;
  }

  if( lp && lp->prev )
  {
    /* Since lp is now pointing to a split which was past the reconcile
     * date, get the running balance of the previous split.
     */
    balance = use_shares ?
      xaccSplitGetShareBalance( (Split *)lp->prev->data ) :
      xaccSplitGetBalance( (Split *)lp->prev->data );
  }

  /* Otherwise there were no splits posted after the given date,
   * so the latest account balance should be good enough.
   */

  return( balance );
}

gnc_numeric
xaccAccountGetBalanceAsOfDate (Account *acc, time_t date)
{
   return( get_balance_as_of_date( acc, date, FALSE ) );
}

gnc_numeric
xaccAccountGetShareBalanceAsOfDate (Account *acc, time_t date)
{
   return( get_balance_as_of_date( acc, date, TRUE ) );
}

/********************************************************************\
\********************************************************************/

Split *
xaccAccountGetSplit(Account *acc, int i) {
  GList *result;

  if (!acc) return(NULL);
  result = g_list_nth(acc->splits, i);
  if(!result) return(NULL);
  return((Split *) result->data);
}

GList *
xaccAccountGetSplitList (Account *acc) {
  if (!acc) return NULL;
  return (acc->splits);
}

int
xaccAccountGetNumSplits (Account *acc) {
  if (!acc) return 0;
  return g_list_length(acc->splits);
}

/********************************************************************\
\********************************************************************/

gboolean
xaccAccountGetTaxRelated (Account *account)
{
  kvp_value *kvp;

  if (!account)
    return FALSE;

  kvp = kvp_frame_get_slot (account->kvp_data, "tax-related");
  if (!kvp)
    return FALSE;

  return kvp_value_get_gint64 (kvp);
}

void
xaccAccountSetTaxRelated (Account *account, gboolean tax_related)
{
  kvp_value *new_value;

  if (!account)
    return;

  if (tax_related)
    new_value = kvp_value_new_gint64 (tax_related);
  else
    new_value = NULL;

  xaccAccountBeginEdit (account);
  kvp_frame_set_slot_nc(account->kvp_data, "tax-related", new_value);

  mark_account (account);
  account->core_dirty = TRUE;
  xaccAccountCommitEdit (account);
}

const char *
xaccAccountGetTaxUSCode (Account *account)
{
  kvp_value *value;

  if (!account)
    return FALSE;

  value = kvp_frame_get_slot_path (account->kvp_data, "tax-US", "code", NULL);
  if (!value)
    return NULL;

  return kvp_value_get_string (value);
}

void
xaccAccountSetTaxUSCode (Account *account, const char *code)
{
  kvp_frame *frame;

  if (!account)
    return;

  xaccAccountBeginEdit (account);

  frame = kvp_frame_get_frame (account->kvp_data, "tax-US", NULL);

  kvp_frame_set_slot_nc (frame, "code",
                         code ? kvp_value_new_string (code) : NULL);

  mark_account (account);
  account->core_dirty = TRUE;
  xaccAccountCommitEdit (account);
}

const char *
xaccAccountGetTaxUSPayerNameSource (Account *account)
{
  kvp_value *value;

  if (!account)
    return FALSE;

  value = kvp_frame_get_slot_path (account->kvp_data,
                                   "tax-US", "payer-name-source", NULL);
  if (!value)
    return NULL;

  return kvp_value_get_string (value);
}

void
xaccAccountSetTaxUSPayerNameSource (Account *account, const char *source)
{
  kvp_frame *frame;

  if (!account)
    return;

  xaccAccountBeginEdit (account);

  frame = kvp_frame_get_frame (account->kvp_data, "tax-US", NULL);

  kvp_frame_set_slot_nc (frame, "payer-name-source",
                         source ? kvp_value_new_string (source) : NULL);

  mark_account (account);
  account->core_dirty = TRUE;
  xaccAccountCommitEdit (account);
}

/********************************************************************\
\********************************************************************/

gboolean
xaccAccountsHaveCommonCurrency(Account *account_1, Account *account_2)
{
  if ((account_1 == NULL) || (account_2 == NULL))
    return FALSE;

  return xaccIsCommonCurrency(account_1->currency, account_1->security,
			      account_2->currency, account_2->security);
}

/********************************************************************\
\********************************************************************/

gboolean
xaccAccountHasAncestor (Account *account, Account * ancestor)
{
  Account *parent;

  if ((account == NULL) || (ancestor == NULL))
    return FALSE;

  parent = xaccAccountGetParentAccount(account);
  while (parent != NULL)
  {
    if (parent == ancestor)
      return TRUE;

    parent = xaccAccountGetParentAccount(parent);
  }

  return FALSE;
}

/********************************************************************\
\********************************************************************/

/* You must edit the functions in this block in tandem.  KEEP THEM IN
   SYNC! */

#define GNC_RETURN_ENUM_AS_STRING(x) case (x): return #x;

char *
xaccAccountTypeEnumAsString(GNCAccountType type) {
  switch(type) {
    GNC_RETURN_ENUM_AS_STRING(NO_TYPE);
    GNC_RETURN_ENUM_AS_STRING(BANK);
    GNC_RETURN_ENUM_AS_STRING(CASH);
    GNC_RETURN_ENUM_AS_STRING(CREDIT);
    GNC_RETURN_ENUM_AS_STRING(ASSET);
    GNC_RETURN_ENUM_AS_STRING(LIABILITY);
    GNC_RETURN_ENUM_AS_STRING(STOCK);
    GNC_RETURN_ENUM_AS_STRING(MUTUAL);
    GNC_RETURN_ENUM_AS_STRING(CURRENCY);
    GNC_RETURN_ENUM_AS_STRING(INCOME);
    GNC_RETURN_ENUM_AS_STRING(EXPENSE);
    GNC_RETURN_ENUM_AS_STRING(EQUITY);
    GNC_RETURN_ENUM_AS_STRING(CHECKING);
    GNC_RETURN_ENUM_AS_STRING(SAVINGS);
    GNC_RETURN_ENUM_AS_STRING(MONEYMRKT);
    GNC_RETURN_ENUM_AS_STRING(CREDITLINE);
    default:
      PERR ("asked to translate unknown account type %d.\n", type);
      break;
  }
  return(NULL);
}

#undef GNC_RETURN_ENUM_AS_STRING

#define GNC_RETURN_ON_MATCH(x) \
  if(safe_strcmp(#x, (str)) == 0) { *type = x; return(TRUE); }

gboolean
xaccAccountStringToType(const char* str, int *type) {

  GNC_RETURN_ON_MATCH(NO_TYPE);
  GNC_RETURN_ON_MATCH(BANK);
  GNC_RETURN_ON_MATCH(CASH);
  GNC_RETURN_ON_MATCH(CREDIT);
  GNC_RETURN_ON_MATCH(ASSET);
  GNC_RETURN_ON_MATCH(LIABILITY);
  GNC_RETURN_ON_MATCH(STOCK);
  GNC_RETURN_ON_MATCH(MUTUAL);
  GNC_RETURN_ON_MATCH(CURRENCY);
  GNC_RETURN_ON_MATCH(INCOME);
  GNC_RETURN_ON_MATCH(EXPENSE);
  GNC_RETURN_ON_MATCH(EQUITY);
  GNC_RETURN_ON_MATCH(CHECKING);
  GNC_RETURN_ON_MATCH(SAVINGS);
  GNC_RETURN_ON_MATCH(MONEYMRKT);
  GNC_RETURN_ON_MATCH(CREDITLINE);

  PERR("asked to translate unknown account type string %s.\n",
       str ? str : "(null)");

  return(FALSE);
}

#undef GNC_RETURN_ON_MATCH

/* impedance mismatch is a source of loss */
GNCAccountType
xaccAccountStringToEnum(const char* str) 
{
  int type;
  gboolean rc;
  rc = xaccAccountStringToType(str, &type);
  if (FALSE == rc) return BAD_TYPE;
  return ((GNCAccountType) type);
}

/********************************************************************\
\********************************************************************/

static char *
account_type_name[NUM_ACCOUNT_TYPES] = { 
  N_("Bank"),
  N_("Cash"),
  N_("Asset"),
  N_("Credit Card"),
  N_("Liability"),
  N_("Stock"),
  N_("Mutual Fund"),
  N_("Currency"),
  N_("Income"),
  N_("Expense"),
  N_("Equity")
  /*
    N_("Checking"),
    N_("Savings"),
    N_("Money Market"),
    N_("Credit Line")
  */
};

char *
xaccAccountGetTypeStr(int type) {
  if (0 > type) return "";
  if (NUM_ACCOUNT_TYPES <= type) return "";
  return _(account_type_name [type]);
}

/********************************************************************\
\********************************************************************/

gboolean
xaccAccountTypesCompatible (int parent_type, int child_type)
{
  gboolean compatible = FALSE;

  switch(parent_type)
  {
    case BANK:
    case CASH: 
    case ASSET:
    case STOCK:
    case MUTUAL:
    case CURRENCY:
    case CREDIT:
    case LIABILITY:
      compatible = ((child_type == BANK)     ||
		    (child_type == CASH)     ||
		    (child_type == ASSET)    ||
		    (child_type == STOCK)    ||
		    (child_type == MUTUAL)   ||
		    (child_type == CURRENCY) ||
                    (child_type == CREDIT)   ||
                    (child_type == LIABILITY));
      break;
    case INCOME:
    case EXPENSE:
      compatible = ((child_type == INCOME) ||
                    (child_type == EXPENSE));
      break;
    case EQUITY:
      compatible = (child_type == EQUITY);
      break;
    default:
      PERR("bad account type: %d", parent_type);
      break;
  }

  return compatible;
}

/********************************************************************\
\********************************************************************/
gboolean
xaccAccountGetReconcileLastDate (Account *account, time_t *last_date)
{
  kvp_value *value;

  if (!account)
    return FALSE;

  value = kvp_frame_get_slot_path (account->kvp_data,
                                   "reconcile-info", "last-date", NULL);
  if (!value)
    return FALSE;

  if (kvp_value_get_type (value) == KVP_TYPE_GINT64)
  {
    if (last_date)
      *last_date = kvp_value_get_gint64 (value);

    return TRUE;
  }

  return FALSE;
}

/********************************************************************\
\********************************************************************/

void
xaccAccountSetReconcileLastDate (Account *account, time_t last_date)
{
  kvp_frame *frame;
  if (!account)
    return;

  xaccAccountBeginEdit (account);
  frame = kvp_frame_get_frame (account->kvp_data, "reconcile-info", NULL);
  kvp_frame_set_slot_nc (frame, "last-date", 
                               kvp_value_new_gint64 (last_date));

  mark_account (account);
  account->core_dirty = TRUE;
  xaccAccountCommitEdit (account);
}

/********************************************************************\
\********************************************************************/

gboolean
xaccAccountGetReconcilePostponeDate (Account *account,
                                     time_t *postpone_date)
{
  kvp_value *value;

  if (!account)
    return FALSE;

  value = kvp_frame_get_slot_path (account->kvp_data,
                                   "reconcile-info", "postpone", "date", NULL);
  if (!value)
    return FALSE;

  if (kvp_value_get_type (value) == KVP_TYPE_GINT64)
  {
    if (postpone_date)
      *postpone_date = kvp_value_get_gint64 (value);

    return TRUE;
  }

  return FALSE;
}

/********************************************************************\
\********************************************************************/

void
xaccAccountSetReconcilePostponeDate (Account *account,
                                     time_t postpone_date)
{
  kvp_frame *frame;
  if (!account)
    return;

  xaccAccountBeginEdit (account);
  frame = kvp_frame_get_frame (account->kvp_data, 
                         "reconcile-info", "postpone", NULL);

  kvp_frame_set_slot_nc (frame, "date", 
                         kvp_value_new_gint64 (postpone_date));

  mark_account (account);
  account->core_dirty = TRUE;
  xaccAccountCommitEdit (account);
}

/********************************************************************\
\********************************************************************/

gboolean
xaccAccountGetReconcilePostponeBalance (Account *account,
                                        gnc_numeric *balance)
{
  kvp_value *value;

  if (!account)
    return FALSE;

  value = kvp_frame_get_slot_path (account->kvp_data,
                                   "reconcile-info", "postpone", "balance",
                                   NULL);
  if (!value)
    return FALSE;

  if (kvp_value_get_type (value) == KVP_TYPE_NUMERIC)
  {
    if (balance)
      *balance = kvp_value_get_numeric (value);

    return TRUE;
  }

  return FALSE;
}

/********************************************************************\
\********************************************************************/

void
xaccAccountSetReconcilePostponeBalance (Account *account,
                                        gnc_numeric balance)
{
  kvp_frame *frame;
  if (!account)
    return;

  xaccAccountBeginEdit (account);
  frame = kvp_frame_get_frame (account->kvp_data, 
                         "reconcile-info", "postpone", NULL);

  kvp_frame_set_slot_nc (frame, "balance", 
                         kvp_value_new_gnc_numeric (balance));

  mark_account (account);
  account->core_dirty = TRUE;
  xaccAccountCommitEdit (account);
}

/********************************************************************\

\********************************************************************/

void
xaccAccountClearReconcilePostpone (Account *account)
{
  if (!account)
    return;

  xaccAccountBeginEdit (account);
  {
    kvp_frame_set_slot_path (account->kvp_data, NULL,
                             "reconcile-info", "postpone", NULL);

    mark_account (account);
  }
  account->core_dirty = TRUE;
  xaccAccountCommitEdit (account);
}

/********************************************************************\
\********************************************************************/

const char *
xaccAccountGetLastNum (Account *account)
{
  kvp_value *value;

  if (!account)
    return FALSE;

  value = kvp_frame_get_slot (account->kvp_data, "last-num");
  if (!value)
    return FALSE;

  return kvp_value_get_string (value);
}

/********************************************************************\
\********************************************************************/

void
xaccAccountSetLastNum (Account *account, const char *num)
{
  if (!account)
    return;

  xaccAccountBeginEdit (account);
  kvp_frame_set_slot_nc (account->kvp_data, "last-num", 
                                            kvp_value_new_string (num));
  mark_account (account);
  account->core_dirty = TRUE;
  xaccAccountCommitEdit (account);
}

/********************************************************************\
\********************************************************************/

void
xaccAccountSetPriceSrc(Account *acc, const char *src)
{
  if(!acc) return;

  xaccAccountBeginEdit(acc);
  {
    GNCAccountType t = xaccAccountGetType(acc);

    if((t == STOCK) || (t == MUTUAL) || (t == CURRENCY)) {
      kvp_frame_set_slot_nc(acc->kvp_data,
                            "old-price-source",
                            src ? kvp_value_new_string(src) : NULL);
      mark_account (acc);
    }
  }
  acc->core_dirty = TRUE;
  xaccAccountCommitEdit(acc);
}

/********************************************************************\
\********************************************************************/

const char*
xaccAccountGetPriceSrc(Account *acc) 
{
  GNCAccountType t;
  if(!acc) return NULL;

  t = xaccAccountGetType(acc);
  if((t == STOCK) || (t == MUTUAL) || (t == CURRENCY)) 
  {
    kvp_value *value = kvp_frame_get_slot(acc->kvp_data, "old-price-source");
    if(value) return (kvp_value_get_string(value));
  }
  return NULL;
}

/********************************************************************\
\********************************************************************/

void
xaccAccountSetQuoteTZ(Account *acc, const char *tz) 
{
  if(!acc) return;
  if(!tz) return;

  xaccAccountBeginEdit(acc);
  {
    GNCAccountType t = xaccAccountGetType(acc);

    if((t == STOCK) || (t == MUTUAL) || (t == CURRENCY)) {
      kvp_frame_set_slot_nc(acc->kvp_data,
                            "old-quote-tz",
                            kvp_value_new_string(tz));
      mark_account (acc);
    }
  }
  acc->core_dirty = TRUE;
  xaccAccountCommitEdit(acc);
}

/********************************************************************\
\********************************************************************/

const char*
xaccAccountGetQuoteTZ(Account *acc) 
{
  GNCAccountType t;
  if(!acc) return NULL;

  t = xaccAccountGetType(acc);
  if((t == STOCK) || (t == MUTUAL) || (t == CURRENCY))
  {
    kvp_value *value = kvp_frame_get_slot(acc->kvp_data, "old-quote-tz");
    if(value) return (kvp_value_get_string(value));
  }
  return NULL;
}

/********************************************************************\
\********************************************************************/

gboolean
xaccAccountVisitUnvisitedTransactions(Account *acc,
                                      gboolean (*proc)(Transaction *t,
                                                       void *data),
                                      void *data,
                                      GHashTable *visited_txns) {
  gboolean keep_going = TRUE;
  GList *lp;

  if(!acc) return(FALSE);
  if(!proc) return(FALSE);
  if(!visited_txns) return(FALSE);

  for(lp = xaccAccountGetSplitList(acc); lp && keep_going; lp = lp->next) {
    Split *s = (Split *) lp->data;
    Transaction *t = xaccSplitGetParent(s);

    if(t) {
      const GUID *guid = xaccTransGetGUID(t);
      gpointer been_here = g_hash_table_lookup(visited_txns, guid);

      if(!GPOINTER_TO_INT(been_here)) {
        g_hash_table_insert(visited_txns, (gpointer) guid,
                            GINT_TO_POINTER(TRUE));
        if(!proc(t, data)) {
          keep_going = FALSE;
        }
      }
    }
  }
  return(keep_going);
}

gboolean
xaccAccountForEachTransaction(Account *acc,
                              gboolean (*proc)(Transaction *t, void *data),
                              void *data) {
  GHashTable *visited_txns = NULL;
  gboolean result = FALSE;

  if(!acc) return(FALSE);
  if(!proc) return(FALSE);
  
  visited_txns = guid_hash_table_new();
  if(visited_txns) {
    result =
      xaccAccountVisitUnvisitedTransactions(acc, proc, data, visited_txns);
  }
  
  /* cleanup */
  if(visited_txns) g_hash_table_destroy(visited_txns);  
  return(result);
}

/********************************************************************\
\********************************************************************/
