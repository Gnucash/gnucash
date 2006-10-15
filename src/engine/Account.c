/********************************************************************\
 * Account.c -- Account data structure implementation               *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1997-2003 Linas Vepstas <linas@linas.org>          *
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
#include <glib/gi18n.h>
#include <stdlib.h>
#include <string.h>

#include "AccountP.h"
#include "Group.h"
#include "GroupP.h"
#include "TransactionP.h"
#include "gnc-event.h"
#include "gnc-glib-utils.h"
#include "gnc-lot.h"
#include "gnc-lot-p.h"
#include "gnc-pricedb.h"
#include "policy.h"

static QofLogModule log_module = GNC_MOD_ACCOUNT;

/* The Canonical Account Separator.  Pre-Initialized. */
static gchar account_separator[8] = ".";
gunichar account_uc_separator = ':';

/********************************************************************\
 * Because I can't use C++ for this project, doesn't mean that I    *
 * can't pretend to!  These functions perform actions on the        *
 * account data structure, in order to encapsulate the knowledge    *
 * of the internals of the Account in one file.                     *
\********************************************************************/

static void xaccAccountBringUpToDate (Account *acc);


/********************************************************************\
 * gnc_get_account_separator                                        *
 *   returns the current account separator character                *
 *                                                                  *
 * Args: none                                                       *
 * Returns: account separator character                             *
 \*******************************************************************/
const gchar *
gnc_get_account_separator_string (void)
{
  return account_separator;
}

gunichar
gnc_get_account_separator (void)
{
  return account_uc_separator;
}

void
gnc_set_account_separator (const gchar *separator)
{
  gunichar uc;
  gint count;

  uc = g_utf8_get_char_validated(separator, -1);
  if ((uc == (gunichar)-2) || (uc == (gunichar)-1) || g_unichar_isalnum(uc)) {
    account_uc_separator = ':';
    strcpy(account_separator, ":");
    return;
  }

  account_uc_separator = uc;
  count = g_unichar_to_utf8(uc, account_separator);
  account_separator[count] = '\0';
}

/********************************************************************\
\********************************************************************/

G_INLINE_FUNC void mark_account (Account *acc);
void
mark_account (Account *acc)
{
  if (acc->parent) acc->parent->saved = FALSE;
  qof_instance_set_dirty(&acc->inst);
}

/********************************************************************\
\********************************************************************/

static void
xaccInitAccount (Account * acc, QofBook *book)
{
  ENTER ("book=%p\n", book);
  qof_instance_init (&acc->inst, GNC_ID_ACCOUNT, book);

  acc->parent   = NULL;
  acc->children = NULL;

  acc->balance = gnc_numeric_zero();
  acc->cleared_balance = gnc_numeric_zero();
  acc->reconciled_balance = gnc_numeric_zero();

  acc->starting_balance = gnc_numeric_zero();
  acc->starting_cleared_balance = gnc_numeric_zero();
  acc->starting_reconciled_balance = gnc_numeric_zero();

  acc->type = ACCT_TYPE_NONE;

  acc->accountName = CACHE_INSERT("");
  acc->accountCode = CACHE_INSERT("");
  acc->description = CACHE_INSERT("");

  acc->idata = 0;

  acc->commodity = NULL;
  acc->commodity_scu = 0;
  acc->non_standard_scu = FALSE;

  acc->splits = NULL;
  acc->lots = NULL;
  acc->policy = xaccGetFIFOPolicy();

  acc->version = 0;
  acc->version_check = 0;
  acc->balance_dirty = FALSE;
  acc->sort_dirty = FALSE;

  LEAVE ("account=%p\n", acc);
}

/********************************************************************\
\********************************************************************/

Account *
xaccMallocAccount (QofBook *book)
{
  Account *acc;

  g_return_val_if_fail (book, NULL);

  acc = g_new (Account, 1);
  xaccInitAccount (acc, book);
  qof_event_gen (&acc->inst.entity, QOF_EVENT_CREATE, NULL);

  return acc;
}

static Account *
xaccCloneAccountCommon(const Account *from, QofBook *book)
{
    Account *ret;

    if (!from || !book) return NULL;
    ENTER (" ");

    ret = g_new (Account, 1);
    g_return_val_if_fail (ret, NULL);

    xaccInitAccount (ret, book);

    /* Do not Begin/CommitEdit() here; give the caller 
     * a chance to fix things up, and let them do it.
     * Also let caller issue the generate_event (EVENT_CREATE) */
    ret->type = from->type;

    ret->accountName = CACHE_INSERT(from->accountName);
    ret->accountCode = CACHE_INSERT(from->accountCode);
    ret->description = CACHE_INSERT(from->description);

    kvp_frame_delete(ret->inst.kvp_data);
    ret->inst.kvp_data = kvp_frame_copy(from->inst.kvp_data);

    /* The new book should contain a commodity that matches
     * the one in the old book. Find it, use it. */
    ret->commodity = gnc_commodity_obtain_twin (from->commodity, book);

    ret->commodity_scu = from->commodity_scu;
    ret->non_standard_scu = from->non_standard_scu;

    LEAVE (" ");
    return ret;
}

Account *
xaccCloneAccount (const Account *from, QofBook *book)
{
    Account *ret = xaccCloneAccountCommon(from, book);
    qof_instance_gemini (&ret->inst, (QofInstance *) &from->inst);
    g_assert (ret ==
              (Account*) qof_instance_lookup_twin (QOF_INSTANCE(from), book));
    return ret;
}

Account *
xaccCloneAccountSimple (const Account *from, QofBook *book)
{
    Account *ret = xaccCloneAccountCommon(from, book);    
    qof_instance_set_dirty(&ret->inst);
    return ret;
}

/********************************************************************\
\********************************************************************/

void
xaccFreeAccount (Account *acc)
{
  GList *lp;

  if (!acc) return;

  qof_event_gen (&acc->inst.entity, QOF_EVENT_DESTROY, NULL);

  if (acc->children) 
  {
    PERR (" instead of calling xaccFreeAccount(), please call \n"
          " xaccAccountBeginEdit(); xaccAccountDestroy(); \n");

    /* First, recursively free children */
    xaccFreeAccountGroup (acc->children);
    acc->children = NULL;
  }

  /* remove lots -- although these should be gone by now. */
  if (acc->lots)
  {
    PERR (" instead of calling xaccFreeAccount(), please call \n"
          " xaccAccountBeginEdit(); xaccAccountDestroy(); \n");
  
    for (lp=acc->lots; lp; lp=lp->next)
    {
      GNCLot *lot = lp->data;
      gnc_lot_destroy (lot);
    }
    g_list_free (acc->lots);
    acc->lots = NULL;
  }

  /* Next, clean up the splits */
  /* NB there shouldn't be any splits by now ... they should 
   * have been all been freed by CommitEdit().  We can remove this
   * check once we know the warning isn't occurring any more. */
  if (acc->splits) 
  {
    GList *slist;
    PERR (" instead of calling xaccFreeAccount(), please call \n"
          " xaccAccountBeginEdit(); xaccAccountDestroy(); \n");
  
    acc->inst.editlevel = 0;

    slist = g_list_copy(acc->splits);
    for (lp = slist; lp; lp = lp->next) {
      Split *s = (Split *) lp->data;
      g_assert(xaccSplitGetAccount(s) == acc);
      xaccSplitDestroy (s);
    }
    g_list_free(slist);
    g_assert(acc->splits == NULL);
  }

  CACHE_REPLACE(acc->accountName, NULL);
  CACHE_REPLACE(acc->accountCode, NULL);
  CACHE_REPLACE(acc->description, NULL);

  /* zero out values, just in case stray 
   * pointers are pointing here. */

  acc->commodity = NULL;
  acc->parent = NULL;
  acc->children = NULL;

  acc->balance  = gnc_numeric_zero();
  acc->cleared_balance = gnc_numeric_zero();
  acc->reconciled_balance = gnc_numeric_zero();

  acc->type = ACCT_TYPE_NONE;
  acc->commodity = NULL;

  acc->version = 0;
  acc->balance_dirty = FALSE;
  acc->sort_dirty = FALSE;

  qof_instance_release (&acc->inst);
  g_free(acc);
}

/********************************************************************\
 * transactional routines
\********************************************************************/

void 
xaccAccountBeginEdit (Account *acc) 
{
	g_return_if_fail(acc);
	qof_begin_edit(&acc->inst);
}

static inline void on_done(QofInstance *inst) 
{
    /* old event style */
    qof_event_gen (&inst->entity, QOF_EVENT_MODIFY, NULL);
}

static inline void on_err (QofInstance *inst, QofBackendError errcode)
{
  PERR("commit error: %d", errcode);
}

static inline void acc_free (QofInstance *inst)
{
  Account *acc = (Account *) inst;
  xaccGroupRemoveAccount(acc->parent, acc);
  xaccFreeAccount(acc);
}

static void
destroy_pending_splits_for_account(QofEntity *ent, gpointer acc)
{
    Transaction *trans = (Transaction *) ent;
    Split *split;

    if (xaccTransIsOpen(trans))
        while ((split = xaccTransFindSplitByAccount(trans, acc)))
            xaccSplitDestroy(split);
}

void 
xaccAccountCommitEdit (Account *acc) 
{
  g_return_if_fail(acc);
  if (!qof_commit_edit(&acc->inst)) return;

  /* If marked for deletion, get rid of subaccounts first,
   * and then the splits ... */
  if (acc->inst.do_free)
  {
    GList *lp, *slist;
    QofCollection *col;
 
    acc->inst.editlevel++;

    /* First, recursively free children */
    xaccFreeAccountGroup (acc->children);
    acc->children = NULL;

    PINFO ("freeing splits for account %p (%s)",
           acc, acc->accountName ? acc->accountName : "(null)");

    slist = g_list_copy(acc->splits);
    for (lp = slist; lp; lp = lp->next)
    {
      Split *s = lp->data;
      xaccSplitDestroy (s);
    }
    g_list_free(slist); 
    /* It turns out there's a case where this assertion does not hold:
       When the user tries to delete an Imbalance account, while also
       deleting all the splits in it.  The splits will just get
       recreated and put right back into the same account!

       g_assert(acc->splits == NULL || qof_book_shutting_down(acc->inst.book));
    */

    if (!qof_book_shutting_down(acc->inst.book)) {
      col = qof_book_get_collection(acc->inst.book, GNC_ID_TRANS);
      qof_collection_foreach(col, destroy_pending_splits_for_account, acc);
    }

    /* the lots should be empty by now */
    for (lp = acc->lots; lp; lp = lp->next)
    {
      GNCLot *lot = lp->data;
      gnc_lot_destroy (lot);
    }
    g_list_free (acc->lots);
    acc->lots = NULL;

    qof_instance_set_dirty(&acc->inst);
    acc->inst.editlevel--;
  }
  else 
  {
    xaccAccountBringUpToDate(acc);
  }

  qof_commit_edit_part2(&acc->inst, on_err, on_done, acc_free);
}

void 
xaccAccountDestroy (Account *acc) 
{
  if (!acc) return;
  acc->inst.do_free = TRUE;

  xaccAccountCommitEdit (acc);
}

void 
xaccAccountSetVersion (Account *acc, gint32 vers)
{
  if (!acc) return;
  acc->version = vers;
}

gint32 
xaccAccountGetVersion (const Account *acc)
{
  if (!acc) return 0;
  return (acc->version);
}

/********************************************************************\
\********************************************************************/

gboolean
xaccAccountEqual(const Account *aa, const Account *ab, gboolean check_guids)
{
  if(!aa && !ab) return TRUE;

  if(!aa || !ab)
  {
    PWARN ("one is NULL");
    return FALSE;
  }

  if (aa->type != ab->type)
  {
    PWARN ("types differ: %d vs %d", aa->type, ab->type);
    return FALSE;
  }

  if (safe_strcmp(aa->accountName, ab->accountName) != 0)
  {
    PWARN ("names differ: %s vs %s", aa->accountName, ab->accountName);
    return FALSE;
  }

  if (safe_strcmp(aa->accountCode, ab->accountCode) != 0)
  {
    PWARN ("codes differ: %s vs %s", aa->accountCode, ab->accountCode);
    return FALSE;
  }

  if (safe_strcmp(aa->description, ab->description) != 0)
  {
    PWARN ("descriptions differ: %s vs %s", aa->description, ab->description);
    return FALSE;
  }

  if (!gnc_commodity_equal(aa->commodity, ab->commodity))
  {
    PWARN ("commodities differ");
    return FALSE;
  }

  if(check_guids) {
    if(!guid_equal(&aa->inst.entity.guid, &ab->inst.entity.guid))
    {
      PWARN ("GUIDs differ");
      return FALSE;
    }
  }

  if (kvp_frame_compare(aa->inst.kvp_data, ab->inst.kvp_data) != 0)
  {
    char *frame_a;
    char *frame_b;

    frame_a = kvp_frame_to_string (aa->inst.kvp_data);
    frame_b = kvp_frame_to_string (ab->inst.kvp_data);

    PWARN ("kvp frames differ:\n%s\n\nvs\n\n%s", frame_a, frame_b);

    g_free (frame_a);
    g_free (frame_b);

    return FALSE;
  }

  if (!gnc_numeric_equal (aa->starting_balance, ab->starting_balance))
  {
    char *str_a;
    char *str_b;

    str_a = gnc_numeric_to_string (aa->starting_balance);
    str_b = gnc_numeric_to_string (ab->starting_balance);

    PWARN ("starting balances differ: %s vs %s", str_a, str_b);

    g_free (str_a);
    g_free (str_b);

    return FALSE;
  }

  if (!gnc_numeric_equal (aa->starting_cleared_balance,
                          ab->starting_cleared_balance))
  {
    char *str_a;
    char *str_b;

    str_a = gnc_numeric_to_string (aa->starting_cleared_balance);
    str_b = gnc_numeric_to_string (ab->starting_cleared_balance);

    PWARN ("starting cleared balances differ: %s vs %s", str_a, str_b);

    g_free (str_a);
    g_free (str_b);

    return FALSE;
  }

  if (!gnc_numeric_equal (aa->starting_reconciled_balance,
                          ab->starting_reconciled_balance))
  {
    char *str_a;
    char *str_b;

    str_a = gnc_numeric_to_string (aa->starting_reconciled_balance);
    str_b = gnc_numeric_to_string (ab->starting_reconciled_balance);

    PWARN ("starting reconciled balances differ: %s vs %s", str_a, str_b);

    g_free (str_a);
    g_free (str_b);

    return FALSE;
  }

  if (!gnc_numeric_equal (aa->balance, ab->balance))
  {
    char *str_a;
    char *str_b;

    str_a = gnc_numeric_to_string (aa->balance);
    str_b = gnc_numeric_to_string (ab->balance);

    PWARN ("balances differ: %s vs %s", str_a, str_b);

    g_free (str_a);
    g_free (str_b);

    return FALSE;
  }

  if (!gnc_numeric_equal (aa->cleared_balance, ab->cleared_balance))
  {
    char *str_a;
    char *str_b;

    str_a = gnc_numeric_to_string (aa->cleared_balance);
    str_b = gnc_numeric_to_string (ab->cleared_balance);

    PWARN ("cleared balances differ: %s vs %s", str_a, str_b);

    g_free (str_a);
    g_free (str_b);

    return FALSE;
  }

  if (!gnc_numeric_equal (aa->reconciled_balance, ab->reconciled_balance))
  {
    char *str_a;
    char *str_b;

    str_a = gnc_numeric_to_string (aa->reconciled_balance);
    str_b = gnc_numeric_to_string (ab->reconciled_balance);

    PWARN ("reconciled balances differ: %s vs %s", str_a, str_b);

    g_free (str_a);
    g_free (str_b);

    return FALSE;
  }

  /* no parent; always compare downwards. */

  {
    GList *la = aa->splits;
    GList *lb = ab->splits;

    if ((la && !lb) || (!la && lb))
    {
      PWARN ("only one has splits");
      return FALSE;
    }

    if(la && lb)
    {
      /* presume that the splits are in the same order */
      while (la && lb)
      {
        Split *sa = (Split *) la->data;
        Split *sb = (Split *) lb->data;

        if (!xaccSplitEqual(sa, sb, check_guids, TRUE, FALSE))
        {
          PWARN ("splits differ");
          return(FALSE);
        }

        la = la->next;
        lb = lb->next;
      }

      if ((la != NULL) || (lb != NULL))
      {
        PWARN ("number of splits differs");
        return(FALSE);
      }
    }
  }

  if (!xaccGroupEqual(aa->children, ab->children, check_guids))
  {
    PWARN ("children differ");
    return FALSE;
  }

  return(TRUE);
}

/********************************************************************\
\********************************************************************/
void
xaccAccountSortSplits (Account *acc, gboolean force)
{
  if (!acc || !acc->sort_dirty || (!force && acc->inst.editlevel > 0)) return;

  acc->splits = g_list_sort(acc->splits, (GCompareFunc)xaccSplitDateOrder);
  acc->sort_dirty = FALSE;
  acc->balance_dirty = TRUE;
}

static void
xaccAccountBringUpToDate(Account *acc) 
{
  if (!acc) return;

  /* if a re-sort happens here, then everything will update, so the
     cost basis and balance calls are no-ops */
  xaccAccountSortSplits(acc, FALSE);
  xaccAccountRecomputeBalance(acc);
}

/********************************************************************\
\********************************************************************/

void 
xaccAccountSetGUID (Account *acc, const GUID *guid)
{
  if (!acc || !guid) return;

  /* XXX this looks fishy and weird to me ... */
  PINFO("acct=%p", acc);
  xaccAccountBeginEdit (acc);
  qof_entity_set_guid (&acc->inst.entity, guid);
  qof_instance_set_dirty(&acc->inst);
  xaccAccountCommitEdit (acc);
}

/********************************************************************\
\********************************************************************/

Account *
xaccAccountLookup (const GUID *guid, QofBook *book)
{
  QofCollection *col;
  if (!guid || !book) return NULL;
  col = qof_book_get_collection (book, GNC_ID_ACCOUNT);
  return (Account *) qof_collection_lookup_entity (col, guid);
}

/********************************************************************\
\********************************************************************/

short
xaccAccountGetMark (const Account *acc)
{
  return acc ? acc->mark : 0;
}

void
xaccAccountSetMark (Account *acc, short m)
{
  if (acc) 
      acc->mark = m;
}

void
xaccClearMark (Account *acc, short val)
{
  AccountGroup *topgrp;

  if (!acc) return;
  topgrp = xaccAccountGetRoot (acc);

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
xaccAccountRemoveLot (Account *acc, GNCLot *lot)
{
    if (!acc || !lot || !acc->lots) return;
    ENTER ("(acc=%p, lot=%p)", acc, lot);

    acc->lots = g_list_remove (acc->lots, lot);
    LEAVE ("(acc=%p, lot=%p)", acc, lot);
}

void
xaccAccountInsertLot (Account *acc, GNCLot *lot)
{
   Account * old_acc = NULL;

   if (!acc || !lot || lot->account == acc) return;
   ENTER ("(acc=%p, lot=%p)", acc, lot);


   /* pull it out of the old account */
   if (lot->account) {
      old_acc = lot->account;
      old_acc->lots = g_list_remove (old_acc->lots, lot);
   }

   acc->lots = g_list_prepend (acc->lots, lot);
   lot->account = acc;

   /* Don't move the splits to the new account.  The caller will do this
    * if appropriate, and doing it here will not work if we are being 
    * called from gnc_book_close_period since xaccAccountInsertSplit
    * will try to balance capital gains and things aren't ready for that. */

   LEAVE ("(acc=%p, lot=%p)", acc, lot);
}

/********************************************************************\
\********************************************************************/
static void
xaccPreSplitMove (Split *split, gpointer dummy)
{
  xaccTransBeginEdit (xaccSplitGetParent (split));
}

static void
xaccPostSplitMove (Split *split, Account *accto)
{
  Transaction *trans;

  xaccSplitSetAccount(split, accto);
  xaccSplitSetAmount(split, split->amount);
  trans = xaccSplitGetParent (split);
  xaccTransCommitEdit (trans);
}

void
xaccAccountMoveAllSplits (Account *accfrom, Account *accto)
{
  /* Handle special cases. */
  if (!accfrom || !accto || !accfrom->splits || accfrom == accto) return;

  /* check for book mix-up */
  g_return_if_fail (accfrom->inst.book == accto->inst.book);
  ENTER ("(accfrom=%p, accto=%p)", accfrom, accto);

  xaccAccountBeginEdit(accfrom);
  xaccAccountBeginEdit(accto);
  /* Begin editing both accounts and all transactions in accfrom. */
  g_list_foreach(accfrom->splits, (GFunc)xaccPreSplitMove, NULL);

  /* Concatenate accfrom's lists of splits and lots to accto's lists. */
  //accto->splits = g_list_concat(accto->splits, accfrom->splits);
  //accto->lots = g_list_concat(accto->lots, accfrom->lots);

  /* Set appropriate flags. */
  //accfrom->balance_dirty = TRUE;
  //accfrom->sort_dirty = FALSE;
  //accto->balance_dirty = TRUE;
  //accto->sort_dirty = TRUE;

  /*
   * Change each split's account back pointer to accto.
   * Convert each split's amount to accto's commodity.
   * Commit to editing each transaction.
   */
  g_list_foreach(accfrom->splits, (GFunc)xaccPostSplitMove, (gpointer)accto);

  /* Finally empty accfrom. */
  g_assert(accfrom->splits == NULL);
  g_assert(accfrom->lots == NULL);
  xaccAccountCommitEdit(accfrom);
  xaccAccountCommitEdit(accto);

  LEAVE ("(accfrom=%p, accto=%p)", accfrom, accto);
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

void
xaccAccountRecomputeBalance (Account * acc)
{
  gnc_numeric  balance;
  gnc_numeric  cleared_balance; 
  gnc_numeric  reconciled_balance;
  Split *last_split = NULL;
  GList *lp;

  if (NULL == acc) return;
  if (acc->inst.editlevel > 0) return;
  if (!acc->balance_dirty) return;
  if (acc->inst.do_free) return;
  if (qof_book_shutting_down(acc->inst.book)) return;

  balance            = acc->starting_balance;
  cleared_balance    = acc->starting_cleared_balance;
  reconciled_balance = acc->starting_reconciled_balance;

  PINFO ("acct=%s starting baln=%" G_GINT64_FORMAT "/%" G_GINT64_FORMAT,
	 acc->accountName, balance.num, balance.denom);
  for(lp = acc->splits; lp; lp = lp->next) 
  {
    Split *split = (Split *) lp->data;
    gnc_numeric amt = xaccSplitGetAmount (split);

    balance = gnc_numeric_add_fixed(balance, amt);

    if (NREC != split->reconciled)
    {
      cleared_balance = gnc_numeric_add_fixed(cleared_balance, amt);
    }

    if (YREC == split->reconciled ||
        FREC == split->reconciled) 
    {
      reconciled_balance =
        gnc_numeric_add_fixed(reconciled_balance, amt);
    }

    split->balance = balance;
    split->cleared_balance = cleared_balance;
    split->reconciled_balance = reconciled_balance;

    last_split = split;
  }

  acc->balance = balance;
  acc->cleared_balance = cleared_balance;
  acc->reconciled_balance = reconciled_balance;

  acc->balance_dirty = FALSE;

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

  acc->starting_balance = start_baln;
  acc->starting_cleared_balance = start_cleared_baln;
  acc->starting_reconciled_balance = start_reconciled_baln;

  acc->balance_dirty = TRUE;
}

/********************************************************************\
\********************************************************************/

/* The sort order is used to implicitly define an 
 * order for report generation */

static int typeorder[NUM_ACCOUNT_TYPES] = {
     ACCT_TYPE_BANK, ACCT_TYPE_STOCK, ACCT_TYPE_MUTUAL, ACCT_TYPE_CURRENCY,
     ACCT_TYPE_CASH, ACCT_TYPE_ASSET, ACCT_TYPE_RECEIVABLE,
     ACCT_TYPE_CREDIT, ACCT_TYPE_LIABILITY, ACCT_TYPE_PAYABLE,
     ACCT_TYPE_INCOME, ACCT_TYPE_EXPENSE, ACCT_TYPE_EQUITY };

static int revorder[NUM_ACCOUNT_TYPES] = {
     -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 };


int
xaccAccountOrder (const Account **aa, const Account **ab) 
{
  char *da, *db;
  char *endptr = NULL;
  int ta, tb, result;
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
  result = safe_strcmp (da, db);
  if (result)
    return result;

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
  result = safe_utf8_collate(da, db);
  if (result)
    return result;

  /* guarantee a stable sort */
  return guid_compare (&((*aa)->inst.entity.guid), &((*ab)->inst.entity.guid));
}

/********************************************************************\
\********************************************************************/

void 
xaccAccountSetType (Account *acc, GNCAccountType tip) 
{
  /* refuse invalid account types, and don't bother if not new type. */
  if (!acc || NUM_ACCOUNT_TYPES <= tip || acc->type == tip) return;

  xaccAccountBeginEdit(acc);
  acc->type = tip;
  acc->balance_dirty = TRUE; /* new type may affect balance computation */
  mark_account(acc);
  xaccAccountCommitEdit(acc);
}

void 
xaccAccountSetName (Account *acc, const char *str) 
{
   if (!acc || !str || str == acc->accountName) return;

   xaccAccountBeginEdit(acc);
   CACHE_REPLACE(acc->accountName, str);
   mark_account (acc);
   
   xaccAccountCommitEdit(acc);
}

void 
xaccAccountSetCode (Account *acc, const char *str) 
{
   if (!acc || !str || str == acc->accountCode) return;

   xaccAccountBeginEdit(acc);
   CACHE_REPLACE(acc->accountCode, str);
   mark_account (acc);

   xaccAccountCommitEdit(acc);
}

void
xaccAccountSetDescription (Account *acc, const char *str) 
{
   if (!acc || !str || str == acc->description) return;

   xaccAccountBeginEdit(acc);
   CACHE_REPLACE(acc->description, str);
   mark_account (acc);
   
   xaccAccountCommitEdit(acc);
}

static void
qofAccountSetParent (Account *acc, QofEntity *parent) 
{
	Account *parent_acc;
	
	if (!acc || !parent) return;
	parent_acc = (Account*)parent;
	xaccAccountBeginEdit(acc);
	xaccAccountBeginEdit(parent_acc);
	xaccAccountInsertSubAccount(parent_acc, acc);
	mark_account (parent_acc);
	mark_account (acc);
	xaccAccountCommitEdit(acc);
	xaccAccountCommitEdit(parent_acc);
}

void
xaccAccountSetNotes (Account *acc, const char *str) 
{
  if (!acc) return;

  xaccAccountBeginEdit(acc);
  if (str) {
    gchar *tmp = g_strstrip(g_strdup(str));
    kvp_frame_set_slot_nc(acc->inst.kvp_data, "notes", 
			  strlen(tmp) ? kvp_value_new_string(tmp) : NULL);
    g_free(tmp);
  } else {
    kvp_frame_set_slot_nc(acc->inst.kvp_data, "notes", NULL);
  }
  mark_account(acc);
  xaccAccountCommitEdit(acc);
}

void 
xaccAccountSetCommodity (Account * acc, gnc_commodity * com) 
{
  GList *lp;
  if (!acc || !com || com == acc->commodity) return;

  xaccAccountBeginEdit(acc);

  acc->commodity = com;
  acc->commodity_scu = gnc_commodity_get_fraction(com);
  acc->non_standard_scu = FALSE;

  /* iterate over splits */
  for (lp = acc->splits; lp; lp = lp->next)
  {
      Split *s = (Split *) lp->data;
      Transaction *trans = xaccSplitGetParent (s);

      xaccTransBeginEdit (trans);
      xaccSplitSetAmount (s, xaccSplitGetAmount(s));
      xaccTransCommitEdit (trans);
  }

  acc->sort_dirty = TRUE;  /* Not needed. */
  acc->balance_dirty = TRUE;
  mark_account (acc);

  if (gnc_commodity_is_iso(com)) {
    /* compatability hack - Gnucash 1.8 gets currency quotes when a
       non-default currency is assigned to an account.  */
    gnc_commodity_set_quote_flag(com, TRUE);
    gnc_commodity_set_quote_source(com, 
        gnc_commodity_get_default_quote_source(com));
  }
  xaccAccountCommitEdit(acc);
}

/*
 * Set the account scu and then check to see if it is the same as the
 * commodity scu.  This function is called when parsing the data file
 * and is designed to catch cases where the two were accidentally set
 * to mismatched values in the past.
 */
void
xaccAccountSetCommoditySCU (Account *acc, int scu)
{
  if (!acc) return;

  xaccAccountBeginEdit(acc);
  acc->commodity_scu = scu;
  if (scu != gnc_commodity_get_fraction(acc->commodity))
      acc->non_standard_scu = TRUE;
  mark_account(acc);
  xaccAccountCommitEdit(acc);
}

int
xaccAccountGetCommoditySCUi (const Account * acc)
{
  return acc ? acc->commodity_scu : 0;
}

int
xaccAccountGetCommoditySCU (const Account * acc)
{
  if (!acc) return 0;

  if (acc->non_standard_scu || !acc->commodity)
    return acc->commodity_scu;
  return gnc_commodity_get_fraction(acc->commodity);
}

void
xaccAccountSetNonStdSCU (Account *acc, gboolean flag)
{
  if (!acc || acc->non_standard_scu == flag) return;

  xaccAccountBeginEdit(acc);
  acc->non_standard_scu = flag;
  mark_account (acc);
  xaccAccountCommitEdit(acc);
}

gboolean
xaccAccountGetNonStdSCU (const Account * acc)
{
  return acc ? acc->non_standard_scu : 0;
}

/********************************************************************\
\********************************************************************/
/* below follow the old, deprecated currency/security routines. */

void 
DxaccAccountSetCurrency (Account * acc, gnc_commodity * currency)
{
  const char *string;
  gnc_commodity *commodity;

  if ((!acc) || (!currency)) return;

  xaccAccountBeginEdit(acc);
  string = gnc_commodity_get_unique_name (currency);
  kvp_frame_set_slot_nc(acc->inst.kvp_data, "old-currency",
                        kvp_value_new_string(string));
  mark_account (acc);
  xaccAccountCommitEdit(acc);

  commodity = DxaccAccountGetCurrency (acc);
  if (!commodity)
  {
    gnc_commodity_table_insert (gnc_commodity_table_get_table (acc->inst.book), currency);
  }
}

/********************************************************************\
\********************************************************************/

AccountGroup *
xaccAccountGetChildren (const Account *acc)
{
   return acc ? acc->children : NULL;
}

AccountGroup *
xaccAccountGetParent (const Account *acc)
{
   return acc ? acc->parent : NULL;
}

Account *
xaccAccountGetParentAccount (const Account * acc)
{
  return acc ? xaccGroupGetParentAccount(acc->parent) : NULL;
}

GList *
xaccAccountGetDescendants (const Account *acc)
{
   return acc ? xaccGroupGetSubAccounts(acc->children) : NULL;
}

GNCAccountType
xaccAccountGetType (const Account *acc)
{
   return acc ? acc->type : ACCT_TYPE_NONE;
}

static const char*
qofAccountGetTypeString (const Account *acc)
{
   return acc ? xaccAccountTypeEnumAsString(acc->type) : NULL;
}

static void
qofAccountSetType (Account *acc, const char *type_string)
{
   xaccAccountSetType(acc, xaccAccountStringToEnum(type_string));
}

const char *
xaccAccountGetName (const Account *acc)
{
   return acc ? acc->accountName : NULL;
}

char *
xaccAccountGetFullName(const Account *account)
{
  const Account *a;
  char *fullname;
  gchar **names;
  int level;

  if (account == NULL)
    return g_strdup("");

  /* Figure out how much space is needed */
  level = 0;
  for (a = account; a; a = xaccAccountGetParentAccount(a)) {
    level++;
  }

  /* Get all the pointers in the right order. */
  names = g_malloc((level+1) * sizeof(gchar *));
  names[level] = NULL;
  for (a = account; a; a = xaccAccountGetParentAccount(a)) {
    names[--level] = a->accountName;
  }

  /* Build it */
  fullname =  g_strjoinv(account_separator, names);
  g_free(names);

  return fullname;
}

const char *
xaccAccountGetCode (const Account *acc)
{
   return acc ? acc->accountCode : NULL;
}

const char * 
xaccAccountGetDescription (const Account *acc)
{
   return acc ? acc->description : NULL;
}

const char * 
xaccAccountGetNotes (const Account *acc) 
{
   return acc ? kvp_frame_get_string(acc->inst.kvp_data, "notes") : NULL;
}

gnc_commodity * 
DxaccAccountGetCurrency (const Account *acc)
{
  KvpValue *v;
  const char *s;
  gnc_commodity_table *table;

  if (!acc) return NULL;

  v = kvp_frame_get_slot(acc->inst.kvp_data, "old-currency");
  if (!v) return NULL;

  s = kvp_value_get_string (v);
  if (!s) return NULL;

  table = gnc_commodity_table_get_table (acc->inst.book);

  return gnc_commodity_table_lookup_unique (table, s);
}

gnc_commodity * 
xaccAccountGetCommodity (const Account *acc)
{
  return acc ? acc->commodity : NULL;
}

gnc_numeric
xaccAccountGetBalance (const Account *acc) 
{
  return acc ? acc->balance : gnc_numeric_zero();
}

gnc_numeric
xaccAccountGetClearedBalance (const Account *acc)
{
  return acc ? acc->cleared_balance : gnc_numeric_zero();
}

gnc_numeric
xaccAccountGetReconciledBalance (const Account *acc)
{
  return acc ? acc->reconciled_balance : gnc_numeric_zero();
}

gnc_numeric
xaccAccountGetProjectedMinimumBalance (const Account *acc)
{
  GList *node;
  time_t today;
  gnc_numeric lowest = gnc_numeric_zero ();
  int seen_a_transaction = 0;

  if (!acc) return gnc_numeric_zero ();

  today = gnc_timet_get_today_end();
  for (node = g_list_last (acc->splits); node; node = node->prev)
  {
    Split *split = node->data;

    if (!seen_a_transaction)
    {
      lowest = xaccSplitGetBalance (split);
      seen_a_transaction = 1;
    } else if (gnc_numeric_compare(xaccSplitGetBalance (split), lowest) < 0) {
      lowest = xaccSplitGetBalance (split);
    }

    if (xaccTransGetDate (xaccSplitGetParent (split)) <= today)
      return lowest;
  }

  return lowest;
}


/********************************************************************\
\********************************************************************/

gnc_numeric
xaccAccountGetBalanceAsOfDate (Account *acc, time_t date)
{
  /* Ideally this could use xaccAccountForEachSplit, but
   * it doesn't exist yet and I'm uncertain of exactly how
   * it would work at this time, since it differs from
   * xaccAccountForEachTransaction by using gpointer return
   * values rather than gints.
   */
  GList   *lp;
  Timespec ts, trans_ts;
  gboolean found = FALSE;
  gnc_numeric balance;

  if (!acc) return gnc_numeric_zero ();

  xaccAccountSortSplits (acc, TRUE); /* just in case, normally a noop */
  xaccAccountRecomputeBalance (acc); /* just in case, normally a noop */

  balance = acc->balance;

  /* Since transaction post times are stored as a Timespec,
   * convert date into a Timespec as well rather than converting
   * each transaction's Timespec into a time_t.
   *
   * FIXME: CAS: I think this comment is a bogus justification for
   * using xaccTransGetDatePostedTS.  There's no benefit to using
   * Timespec when the input argument is time_t, and it's hard to
   * imagine that casting long long to long and comparing two longs is
   * worse than comparing two long longs every time.  IMO,
   * xaccAccountGetPresentBalance gets this right, and its algorithm
   * should be used here.
   */
  ts.tv_sec = date;
  ts.tv_nsec = 0;

  lp = acc->splits;
  while( lp && !found )
  {
    xaccTransGetDatePostedTS( xaccSplitGetParent( (Split *)lp->data ),
                              &trans_ts );
    if( timespec_cmp( &trans_ts, &ts ) >= 0 )
      found = TRUE;
    else
      lp = lp->next;
  }

  if( lp ) {
    if ( lp->prev ) {
      /* Since lp is now pointing to a split which was past the reconcile
       * date, get the running balance of the previous split.
       */
      balance = xaccSplitGetBalance( (Split *)lp->prev->data );
    }		
    else {
      /* AsOf date must be before any entries, return zero. */
      balance = gnc_numeric_zero();
    }
  }

  /* Otherwise there were no splits posted after the given date,
   * so the latest account balance should be good enough.
   */

  return( balance );
}

/*
 * Originally gsr_account_present_balance in gnc-split-reg.c
 *
 * How does this routine compare to xaccAccountGetBalanceAsOfDate just
 * above?  These two routines should eventually be collapsed into one.
 * Perhaps the startup logic from that one, and the logic from this
 * one that walks from the tail of the split list.
 */
gnc_numeric
xaccAccountGetPresentBalance (const Account *acc)
{
  GList *node;
  time_t today;

  g_return_val_if_fail(acc, gnc_numeric_zero());

  today = gnc_timet_get_today_end();
  for (node = g_list_last (acc->splits); node; node = node->prev)
  {
    Split *split = node->data;

    if (xaccTransGetDate (xaccSplitGetParent (split)) <= today)
      return xaccSplitGetBalance (split);
  }

  return gnc_numeric_zero ();
}


/********************************************************************\
\********************************************************************/
/* XXX TODO: These 'GetBal' routines should be moved to some
 * utility area outside of the core account engine area. 
 */

/*
 * Convert a balance from one currency to another.
 */
gnc_numeric
xaccAccountConvertBalanceToCurrency(const Account *acc, /* for book */
				    gnc_numeric balance,
				    const gnc_commodity *balance_currency,
				    const gnc_commodity *new_currency)
{
  QofBook *book;
  GNCPriceDB *pdb;

  if (gnc_numeric_zero_p (balance) ||
      gnc_commodity_equiv (balance_currency, new_currency))
    return balance;

  book = xaccGroupGetBook (xaccAccountGetRoot (acc));
  pdb = gnc_pricedb_get_db (book);

  balance = gnc_pricedb_convert_balance_latest_price(
      pdb, balance, balance_currency, new_currency);

  return balance;
}

/*
 * Convert a balance from one currency to another with price of
 * a given date.
 */
gnc_numeric
xaccAccountConvertBalanceToCurrencyAsOfDate(const Account *acc, /* for book */
					    gnc_numeric balance,
					    gnc_commodity *balance_currency,
					    gnc_commodity *new_currency,
					    time_t date)
{
  QofBook *book;
  GNCPriceDB *pdb;
  Timespec ts;

  if (gnc_numeric_zero_p (balance) ||
      gnc_commodity_equiv (balance_currency, new_currency))
    return balance;

  book = xaccGroupGetBook (xaccAccountGetRoot (acc));
  pdb = gnc_book_get_pricedb (book);

  ts.tv_sec = date;
  ts.tv_nsec = 0;

  balance = gnc_pricedb_convert_balance_nearest_price(
      pdb, balance, balance_currency, new_currency, ts);

  return balance;
}

/*
 * Given an account and a GetBalanceFn pointer, extract the requested
 * balance from the account and then convert it to the desired
 * currency.
 */
static gnc_numeric
xaccAccountGetXxxBalanceInCurrency (const Account *acc,
				    xaccGetBalanceFn fn,
				    const gnc_commodity *report_currency)
{
  gnc_numeric balance;

  if (!acc || !fn || !report_currency) return gnc_numeric_zero ();
  balance = fn(acc);
  balance = xaccAccountConvertBalanceToCurrency(acc, balance,
                                                acc->commodity,
                                                report_currency);
  return balance;
}

static gnc_numeric
xaccAccountGetXxxBalanceAsOfDateInCurrency(Account *acc, time_t date,
                                           xaccGetBalanceAsOfDateFn fn,
                                           const gnc_commodity *report_commodity)
{
    g_return_val_if_fail(acc && fn && report_commodity, gnc_numeric_zero());
    return xaccAccountConvertBalanceToCurrency(
        acc, fn(acc, date), acc->commodity, report_commodity);
}

/*
 * Data structure used to pass various arguments into the following fn.
 */
typedef struct
{
  const gnc_commodity *currency;
  gnc_numeric balance;
  xaccGetBalanceFn fn;
  xaccGetBalanceAsOfDateFn asOfDateFn;
  time_t date;
} CurrencyBalance;


/*
 * A helper function for iterating over all the accounts in a list or
 * tree.  This function is called once per account, and sums up the
 * values of all these accounts.
 */
static gpointer
xaccAccountBalanceHelper (Account *acc, gpointer data)
{
  CurrencyBalance *cb = data;
  gnc_numeric balance;

  if (!cb->fn || !cb->currency)
    return NULL;
  balance = xaccAccountGetXxxBalanceInCurrency (acc, cb->fn, cb->currency);
  cb->balance = gnc_numeric_add (cb->balance, balance,
                                 gnc_commodity_get_fraction (cb->currency),
                                 GNC_HOW_RND_ROUND);
  return NULL;
}
static gpointer
xaccAccountBalanceAsOfDateHelper (Account *acc, gpointer data)
{
    CurrencyBalance *cb = data;
    gnc_numeric balance;

    g_return_val_if_fail (cb->asOfDateFn && cb->currency, NULL);

    balance = xaccAccountGetXxxBalanceAsOfDateInCurrency (
        acc, cb->date, cb->asOfDateFn, cb->currency);
    cb->balance = gnc_numeric_add (cb->balance, balance,
                                   gnc_commodity_get_fraction (cb->currency),
                                   GNC_HOW_RND_ROUND);
    return NULL;
}



/*
 * Common function that iterates recursively over all accounts below
 * the specified account.  It uses xaccAccountBalanceHelper to sum up
 * the balances of all its children, and uses the specified function
 * 'fn' for extracting the balance.  This function may extract the
 * current value, the reconciled value, etc.
 *
 * If 'report_commodity' is NULL, just use the account's commodity.
 * If 'include_children' is FALSE, this function doesn't recurse at all.
 */
static gnc_numeric
xaccAccountGetXxxBalanceInCurrencyRecursive (const Account *acc,
					     xaccGetBalanceFn fn,
					     const gnc_commodity *report_commodity,
					     gboolean include_children)
{
  gnc_numeric balance;

  if (!acc) return gnc_numeric_zero ();
  if (!report_commodity)
    report_commodity = xaccAccountGetCommodity (acc);

  balance = xaccAccountGetXxxBalanceInCurrency (acc, fn, report_commodity);

  /* If needed, sum up the children converting to the *requested*
     commodity. */
  if (include_children) {
    CurrencyBalance cb = { report_commodity, balance, fn, NULL, 0 };

    xaccGroupForEachAccount (acc->children, xaccAccountBalanceHelper,
                             &cb, TRUE);
    balance = cb.balance;
  }

  return balance;
}

static gnc_numeric
xaccAccountGetXxxBalanceAsOfDateInCurrencyRecursive (
    Account *acc, time_t date, xaccGetBalanceAsOfDateFn fn,
    gnc_commodity *report_commodity, gboolean include_children)
{
  gnc_numeric balance;

  g_return_val_if_fail(acc, gnc_numeric_zero());
  if (!report_commodity)
      report_commodity = xaccAccountGetCommodity (acc);

  balance = xaccAccountGetXxxBalanceAsOfDateInCurrency(
      acc, date, fn, report_commodity);

  /* If needed, sum up the children converting to the *requested*
     commodity. */
  if (include_children) {
    CurrencyBalance cb = { report_commodity, balance, NULL, fn, date };

    xaccGroupForEachAccount (acc->children,
                             xaccAccountBalanceAsOfDateHelper, &cb, TRUE);
    balance = cb.balance;
  }

  return balance;
}

gnc_numeric
xaccAccountGetBalanceInCurrency (const Account *acc, 
                                 const gnc_commodity *report_commodity,
				 gboolean include_children)
{
  gnc_numeric rc;
  rc = xaccAccountGetXxxBalanceInCurrencyRecursive (
      acc, xaccAccountGetBalance, report_commodity, include_children);
  PINFO(" baln=%" G_GINT64_FORMAT "/%" G_GINT64_FORMAT, rc.num, rc.denom);
  return rc;
}


gnc_numeric
xaccAccountGetClearedBalanceInCurrency (const Account *acc,
                                        const gnc_commodity *report_commodity,
                                        gboolean include_children)
{
  return xaccAccountGetXxxBalanceInCurrencyRecursive (
      acc, xaccAccountGetClearedBalance, report_commodity,
      include_children);
}


gnc_numeric
xaccAccountGetReconciledBalanceInCurrency (const Account *acc,
                                           const gnc_commodity *report_commodity,
                                           gboolean include_children)
{
  return xaccAccountGetXxxBalanceInCurrencyRecursive (
      acc, xaccAccountGetReconciledBalance, report_commodity,
      include_children);
}

gnc_numeric
xaccAccountGetPresentBalanceInCurrency (const Account *acc,
					const gnc_commodity *report_commodity,
					gboolean include_children)
{
  return xaccAccountGetXxxBalanceInCurrencyRecursive (
      acc, xaccAccountGetPresentBalance, report_commodity,
      include_children);
}

gnc_numeric
xaccAccountGetProjectedMinimumBalanceInCurrency (
    const Account *acc, 
    const gnc_commodity *report_commodity,
    gboolean include_children)
{
  return xaccAccountGetXxxBalanceInCurrencyRecursive (
      acc, xaccAccountGetProjectedMinimumBalance, report_commodity,
      include_children);
}

gnc_numeric
xaccAccountGetBalanceAsOfDateInCurrency(
    Account *acc, time_t date, gnc_commodity *report_commodity,
    gboolean include_children)
{
    return xaccAccountGetXxxBalanceAsOfDateInCurrencyRecursive (
        acc, date, xaccAccountGetBalanceAsOfDate, report_commodity,
        include_children);
}

gnc_numeric
xaccAccountGetBalanceChangeForPeriod (Account *acc, time_t t1, time_t t2, gboolean recurse)
{
  gnc_numeric b1, b2;  

  b1 = xaccAccountGetBalanceAsOfDateInCurrency(acc, t1, NULL, recurse);
  b2 = xaccAccountGetBalanceAsOfDateInCurrency(acc, t2, NULL, recurse);
  return gnc_numeric_sub(b2, b1, GNC_DENOM_AUTO, GNC_HOW_DENOM_FIXED);
}


/********************************************************************\
\********************************************************************/

SplitList *
xaccAccountGetSplitList (const Account *acc) 
{
  return acc ? acc->splits : NULL;
}

LotList *
xaccAccountGetLotList (const Account *acc) 
{
  return acc ? acc->lots : NULL;
}

LotList *
xaccAccountFindOpenLots (const Account *acc,
                         gboolean (*match_func)(GNCLot *lot,
                                                gpointer user_data),
                         gpointer user_data, GCompareFunc sort_func)
{
  GList *lot_list;
  GList *retval = NULL;

  if (!acc)
    return NULL;

  lot_list = xaccAccountGetLotList (acc);
  for ( ; lot_list ; lot_list = lot_list->next ) {
    GNCLot *lot = lot_list->data;

    /* If this lot is closed, then ignore it */
    if (gnc_lot_is_closed (lot))
      continue;

    if (match_func && !(match_func)(lot, user_data))
      continue;

    /* Ok, this is a valid lot.  Add it to our list of lots */
    if (sort_func)
      retval = g_list_insert_sorted (retval, lot, sort_func);
    else
      retval = g_list_prepend (retval, lot);
  }

  return retval;
}

gpointer
xaccAccountForEachLot(const Account *acc, 
                      gpointer (*proc)(GNCLot *lot, void *data), void *data) 
{
  LotList *node;
  gpointer result = NULL;

  if (!acc || !proc) return NULL;
  
  for (node = acc->lots; node; node = node->next)
      if ((result = proc((GNCLot *)node->data, data))) 
          break;
  
  return result;
}

/********************************************************************\
\********************************************************************/

/* These functions use interchange gint64 and gboolean.  Is that right? */
gboolean
xaccAccountGetTaxRelated (const Account *acc)
{
  return acc ? kvp_frame_get_gint64(acc->inst.kvp_data, "tax-related") : FALSE;
}

void
xaccAccountSetTaxRelated (Account *acc, gboolean tax_related)
{
  KvpValue *new_value;

  if (!acc)
    return;

  if (tax_related)
    new_value = kvp_value_new_gint64 (tax_related);
  else
    new_value = NULL;

  xaccAccountBeginEdit (acc);
  kvp_frame_set_slot_nc(acc->inst.kvp_data, "tax-related", new_value);
  mark_account (acc);
  xaccAccountCommitEdit (acc);
}

const char *
xaccAccountGetTaxUSCode (const Account *acc)
{
  return acc ? kvp_frame_get_string(acc->inst.kvp_data, "tax-US/code") : NULL;
}

void
xaccAccountSetTaxUSCode (Account *acc, const char *code)
{
  if (!acc) return;

  xaccAccountBeginEdit (acc);
  kvp_frame_set_string (acc->inst.kvp_data, "/tax-US/code", code);
  mark_account (acc);
  xaccAccountCommitEdit (acc);
}

const char *
xaccAccountGetTaxUSPayerNameSource (const Account *acc)
{
  return acc ? kvp_frame_get_string(acc->inst.kvp_data, 
                                    "tax-US/payer-name-source") : NULL;
}

void
xaccAccountSetTaxUSPayerNameSource (Account *acc, const char *source)
{
  if (!acc) return;

  xaccAccountBeginEdit (acc);
  kvp_frame_set_string (acc->inst.kvp_data, 
                        "/tax-US/payer-name-source", source);
  mark_account (acc);
  xaccAccountCommitEdit (acc);
}

/********************************************************************\
\********************************************************************/

gboolean
xaccAccountGetPlaceholder (const Account *acc)
{
  const char *str;
  if (!acc) return FALSE;
  
  str = kvp_frame_get_string(acc->inst.kvp_data, "placeholder");
  return (str && !strcmp(str, "true"));
}

void
xaccAccountSetPlaceholder (Account *acc, gboolean val)
{
  if (!acc) return;
  
  xaccAccountBeginEdit (acc);
  kvp_frame_set_string (acc->inst.kvp_data, 
                        "placeholder", val ? "true" : NULL);
  mark_account (acc);
  xaccAccountCommitEdit (acc);
}

GNCPlaceholderType
xaccAccountGetDescendantPlaceholder (const Account *acc)
{
  GList *descendants, *node;
  GNCPlaceholderType ret = PLACEHOLDER_NONE;

  if (!acc) return PLACEHOLDER_NONE;
  if (xaccAccountGetPlaceholder(acc)) return PLACEHOLDER_THIS;

  descendants = xaccGroupGetSubAccounts(acc->children);
  for (node = descendants; node; node = node->next) 
      if (xaccAccountGetPlaceholder((Account *) node->data)) {
          ret = PLACEHOLDER_CHILD;
          break;
      }

  g_list_free(descendants);
  return ret;
}

/********************************************************************\
\********************************************************************/

gboolean
xaccAccountGetHidden (const Account *acc)
{
  const char *str;
  if (!acc) return FALSE;
  
  str = kvp_frame_get_string(acc->inst.kvp_data, "hidden");
  return (str && !strcmp(str, "true"));
}

void
xaccAccountSetHidden (Account *acc, gboolean val)
{
  if (!acc) return;
  
  xaccAccountBeginEdit (acc);
  kvp_frame_set_string (acc->inst.kvp_data, "hidden",
			val ? "true" : NULL);
  mark_account (acc);
  xaccAccountCommitEdit (acc);
}

gboolean
xaccAccountIsHidden (const Account *acc)
{
  if (!acc)
    return FALSE;
  if (xaccAccountGetHidden(acc))
    return TRUE;

  while ((acc = xaccAccountGetParentAccount(acc)) != NULL) {
    if (xaccAccountGetHidden(acc))
      return TRUE;
  }
  return FALSE;
}

/********************************************************************\
\********************************************************************/

gboolean
xaccAccountHasAncestor (const Account *acc, const Account * ancestor)
{
  const Account *parent = acc;

  if (!acc || !ancestor) return FALSE;

  while (parent && parent != ancestor)
      parent = xaccAccountGetParentAccount(parent);

  return (parent == ancestor);
}

/********************************************************************\
\********************************************************************/

/* You must edit the functions in this block in tandem.  KEEP THEM IN
   SYNC! */

#define GNC_RETURN_ENUM_AS_STRING(x) case (ACCT_TYPE_ ## x): return #x;

const char *
xaccAccountTypeEnumAsString(GNCAccountType type) 
{
  switch(type) 
  {
    GNC_RETURN_ENUM_AS_STRING(NONE);
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
    GNC_RETURN_ENUM_AS_STRING(RECEIVABLE);
    GNC_RETURN_ENUM_AS_STRING(PAYABLE);
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
  if(safe_strcmp(#x, (str)) == 0) { *type = ACCT_TYPE_ ## x; return(TRUE); }

gboolean
xaccAccountStringToType(const char* str, GNCAccountType *type)
{

  GNC_RETURN_ON_MATCH(NONE);
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
  GNC_RETURN_ON_MATCH(RECEIVABLE);
  GNC_RETURN_ON_MATCH(PAYABLE);
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
  GNCAccountType type;
  gboolean rc;
  rc = xaccAccountStringToType(str, &type);
  if (FALSE == rc) return ACCT_TYPE_INVALID;
  return type;
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
  N_("Equity"),
  N_("A/Receivable"),
  N_("A/Payable")
  /*
    N_("Checking"),
    N_("Savings"),
    N_("Money Market"),
    N_("Credit Line")
  */
};

const char *
xaccAccountGetTypeStr(GNCAccountType type) {
  if (type < 0 || NUM_ACCOUNT_TYPES <= type ) return "";
  return _(account_type_name [type]);
}

GNCAccountType
xaccAccountGetTypeFromStr (const gchar *str)
{
  gint type;

  for (type = 0; type < NUM_ACCOUNT_TYPES; type++)
  {
    if (!safe_strcmp (str, _(account_type_name [type])))
      return type;
  }

  PERR("asked to translate unknown account type string %s.\n",
       str ? str : "(null)");

  return ACCT_TYPE_INVALID;
}


/********************************************************************\
\********************************************************************/

guint32
xaccAccountTypesCompatibleWith (GNCAccountType type)
{
  switch (type) {
  case ACCT_TYPE_BANK:
  case ACCT_TYPE_CASH:
  case ACCT_TYPE_ASSET:
  case ACCT_TYPE_STOCK:
  case ACCT_TYPE_MUTUAL:
  case ACCT_TYPE_CURRENCY:
  case ACCT_TYPE_CREDIT:
  case ACCT_TYPE_LIABILITY:
  case ACCT_TYPE_RECEIVABLE:
  case ACCT_TYPE_PAYABLE:
    return
      (1 << ACCT_TYPE_BANK)       |
      (1 << ACCT_TYPE_CASH)       |
      (1 << ACCT_TYPE_ASSET)      |
      (1 << ACCT_TYPE_STOCK)      |
      (1 << ACCT_TYPE_MUTUAL)     |
      (1 << ACCT_TYPE_CURRENCY)   |
      (1 << ACCT_TYPE_CREDIT)     |
      (1 << ACCT_TYPE_LIABILITY)  |
      (1 << ACCT_TYPE_RECEIVABLE) |
      (1 << ACCT_TYPE_PAYABLE);
  case ACCT_TYPE_INCOME:
  case ACCT_TYPE_EXPENSE:
    return
      (1 << ACCT_TYPE_INCOME)     |
      (1 << ACCT_TYPE_EXPENSE);
  case ACCT_TYPE_EQUITY:
    return
      (1 << ACCT_TYPE_EQUITY);
  default:
    PERR("bad account type: %d", type);
    return 0;
  }
}

gboolean
xaccAccountTypesCompatible (GNCAccountType parent_type,
                            GNCAccountType child_type)
{
  return ((xaccAccountTypesCompatibleWith (parent_type) &
           (1 << child_type))
          != 0);
}

guint32
xaccAccountTypesValid(void)
{
    guint32 mask = (1 << NUM_ACCOUNT_TYPES) - 1;
    mask &= ~(1 << ACCT_TYPE_CURRENCY);  /* DEPRECATED */

    return mask;
}

gboolean
xaccAccountIsPriced(const Account *acc)
{
    if (!acc) return FALSE;

    return (acc->type == ACCT_TYPE_STOCK || acc->type == ACCT_TYPE_MUTUAL || 
            acc->type == ACCT_TYPE_CURRENCY);
}

/********************************************************************\
\********************************************************************/

gboolean
xaccAccountGetReconcileLastDate (const Account *acc, time_t *last_date)
{
  KvpValue *v;

  if (!acc) return FALSE;

  v = kvp_frame_get_value(acc->inst.kvp_data, "reconcile-info/last-date");
  
  if (!v || kvp_value_get_type(v) != KVP_TYPE_GINT64)
      return FALSE;

  if (last_date)
      *last_date = kvp_value_get_gint64(v);

  return TRUE;
}

/********************************************************************\
\********************************************************************/

void
xaccAccountSetReconcileLastDate (Account *acc, time_t last_date)
{
  if (!acc) return;

  xaccAccountBeginEdit (acc);
  kvp_frame_set_gint64 (acc->inst.kvp_data, 
                        "/reconcile-info/last-date", last_date);
  mark_account (acc);
  xaccAccountCommitEdit (acc);
}

/********************************************************************\
\********************************************************************/

gboolean
xaccAccountGetReconcileLastInterval (const Account *acc, 
                                     int *months, int *days)
{
  KvpValue *v1, *v2;

  if (!acc) return FALSE;

  v1 = kvp_frame_get_value(acc->inst.kvp_data, 
                           "reconcile-info/last-interval/months");
  v2 = kvp_frame_get_value(acc->inst.kvp_data, 
                           "reconcile-info/last-interval/days");
  if (!v1 || (kvp_value_get_type (v1) != KVP_TYPE_GINT64) ||
      !v2 || (kvp_value_get_type (v2) != KVP_TYPE_GINT64))
    return FALSE;

  if (months)
    *months = kvp_value_get_gint64 (v1);
  if (days)
    *days = kvp_value_get_gint64 (v2);
  return TRUE;
}

/********************************************************************\
\********************************************************************/

void
xaccAccountSetReconcileLastInterval (Account *acc, int months, int days)
{
  KvpFrame *frame;
  if (!acc) return;

  xaccAccountBeginEdit (acc);

  frame = kvp_frame_get_frame_slash (acc->inst.kvp_data, 
         "/reconcile-info/last-interval");
  g_assert(frame);

  kvp_frame_set_gint64 (frame, "months", months);
  kvp_frame_set_gint64 (frame, "days", days);

  mark_account (acc);
  xaccAccountCommitEdit (acc);
}

/********************************************************************\
\********************************************************************/

gboolean
xaccAccountGetReconcilePostponeDate (const Account *acc, time_t *postpone_date)
{
  KvpValue *v;

  if (!acc) return FALSE;

  v = kvp_frame_get_value(acc->inst.kvp_data, "reconcile-info/postpone/date");
  if (!v || kvp_value_get_type (v) != KVP_TYPE_GINT64)
      return FALSE;

  if (postpone_date)
      *postpone_date = kvp_value_get_gint64 (v);

  return TRUE;
}

/********************************************************************\
\********************************************************************/

void
xaccAccountSetReconcilePostponeDate (Account *acc, time_t postpone_date)
{
  if (!acc) return;

  xaccAccountBeginEdit (acc);

  /* XXX this should be using timespecs, not gints !! */
  kvp_frame_set_gint64 (acc->inst.kvp_data,
            "reconcile-info/postpone/date", postpone_date);
  mark_account (acc);
  xaccAccountCommitEdit (acc);
}

/********************************************************************\
\********************************************************************/

gboolean
xaccAccountGetReconcilePostponeBalance (const Account *acc, 
                                        gnc_numeric *balance)
{
  KvpValue *v;

  if (!acc) return FALSE;

  v = kvp_frame_get_value(acc->inst.kvp_data, 
                          "reconcile-info/postpone/balance");
  if (!v || kvp_value_get_type (v) != KVP_TYPE_NUMERIC)
      return FALSE;

  if (balance)
      *balance = kvp_value_get_numeric (v);

  return TRUE;
}

/********************************************************************\
\********************************************************************/

void
xaccAccountSetReconcilePostponeBalance (Account *acc, gnc_numeric balance)
{
  if (!acc) return;

  xaccAccountBeginEdit (acc);
  kvp_frame_set_gnc_numeric (acc->inst.kvp_data,
           "/reconcile-info/postpone/balance", balance);
  mark_account (acc);
  xaccAccountCommitEdit (acc);
}

/********************************************************************\

\********************************************************************/

void
xaccAccountClearReconcilePostpone (Account *acc)
{
  if (!acc) return;

  xaccAccountBeginEdit (acc);
  kvp_frame_set_value (acc->inst.kvp_data, "reconcile-info/postpone", NULL);
  mark_account (acc);
  xaccAccountCommitEdit (acc);
}

/********************************************************************\
\********************************************************************/

/* xaccAccountGetAutoInterestXfer: determine whether the auto interest
 * xfer option is enabled for this account, and return that value.
 * If it is not defined for the account, return the default value.
 */
gboolean
xaccAccountGetAutoInterestXfer (const Account *acc, gboolean default_value)
{
  const char *str = NULL;
  if (!acc) return default_value;

  str = kvp_frame_get_string(acc->inst.kvp_data, 
                             "reconcile-info/auto-interest-transfer");
  return str ? !strcmp(str, "true") : default_value;
}

/********************************************************************\
\********************************************************************/

void
xaccAccountSetAutoInterestXfer (Account *acc, gboolean option)
{
  if (!acc) return;

  xaccAccountBeginEdit (acc);
  /* FIXME: need KVP_TYPE_BOOLEAN for this someday */
  kvp_frame_set_string (acc->inst.kvp_data,
                        "/reconcile-info/auto-interest-transfer",
                        (option ? "true" : "false"));
  mark_account (acc);
  xaccAccountCommitEdit (acc);
}

/********************************************************************\
\********************************************************************/

const char *
xaccAccountGetLastNum (const Account *acc)
{
  return acc ? kvp_frame_get_string(acc->inst.kvp_data, "last-num") : NULL;
}

/********************************************************************\
\********************************************************************/

void
xaccAccountSetLastNum (Account *acc, const char *num)
{
  if (!acc) return;

  xaccAccountBeginEdit (acc);
  kvp_frame_set_string(acc->inst.kvp_data, "last-num", num);
  mark_account (acc);
  xaccAccountCommitEdit (acc);
}

/********************************************************************\
\********************************************************************/

void
dxaccAccountSetPriceSrc(Account *acc, const char *src)
{
  if (!acc) return;

  xaccAccountBeginEdit(acc);
  if (xaccAccountIsPriced(acc)) {
      kvp_frame_set_slot_nc(acc->inst.kvp_data,
                            "old-price-source",
                            src ? kvp_value_new_string(src) : NULL);
      mark_account (acc);
  }
  
  qof_instance_set_dirty(&acc->inst);
  xaccAccountCommitEdit(acc);
}

/********************************************************************\
\********************************************************************/

const char*
dxaccAccountGetPriceSrc(const Account *acc) 
{
  if(!acc) return NULL;

  if (xaccAccountIsPriced(acc)) {
      KvpValue *value = kvp_frame_get_slot(acc->inst.kvp_data, 
                                           "old-price-source");
      if (value) return (kvp_value_get_string(value));
  }
  return NULL;
}

/********************************************************************\
\********************************************************************/

void
dxaccAccountSetQuoteTZ(Account *acc, const char *tz) 
{
  if (!acc) return;

  xaccAccountBeginEdit(acc);
  if (xaccAccountIsPriced(acc)) {
      kvp_frame_set_slot_nc(acc->inst.kvp_data,
                            "old-quote-tz",
                            tz ? kvp_value_new_string(tz) : NULL);
      mark_account (acc);
  }
  qof_instance_set_dirty(&acc->inst);
  xaccAccountCommitEdit(acc);
}

/********************************************************************\
\********************************************************************/

const char*
dxaccAccountGetQuoteTZ(const Account *acc) 
{
  if (!acc) return NULL;

  if (xaccAccountIsPriced(acc)) {
      KvpValue *value = kvp_frame_get_slot(acc->inst.kvp_data, "old-quote-tz");
      if(value) return (kvp_value_get_string(value));
  }
  return NULL;
}

/********************************************************************\
\********************************************************************/

void
xaccAccountSetReconcileChildrenStatus(Account *acc, gboolean status)
{ 
  if (!acc) return;
  
  xaccAccountBeginEdit (acc);
  
  /* XXX FIXME: someday this should use KVP_TYPE_BOOLEAN */
  kvp_frame_set_gint64 (acc->inst.kvp_data, 
                        "/reconcile-info/include-children", status);
  mark_account(acc);
  xaccAccountCommitEdit (acc);
}

/********************************************************************\
\********************************************************************/

gboolean
xaccAccountGetReconcileChildrenStatus(const Account *acc)
{
  /* access the account's kvp-data for status and return that, if no value
   * is found then we can assume not to include the children, that being
   * the default behaviour 
   */
  return acc ? kvp_frame_get_gint64(acc->inst.kvp_data, 
                                    "reconcile-info/include-children") : FALSE;
}

/********************************************************************\
\********************************************************************/

gint
xaccAccountForEachTransaction(const Account *acc, TransactionCallback proc,
                              void *data) 
{
  if (!acc || !proc) return 0;
  xaccAccountBeginStagedTransactionTraversals (acc);
  return xaccAccountStagedTransactionTraversal(acc, 42, proc, data);
}

/********************************************************************\
\********************************************************************/

/* The caller of this function can get back one or both of the
 * matching split and transaction pointers, depending on whether
 * a valid pointer to the location to store those pointers is
 * passed.
 */
static void
finder_help_function(const Account *acc, const char *description,
                     Split **split, Transaction **trans )
{
  GList *slp;

  /* First, make sure we set the data to NULL BEFORE we start */
  if (split) *split = NULL;
  if (trans) *trans = NULL;

  /* Then see if we have any work to do */
  if (acc == NULL) return;

  /* Why is this loop iterated backwards ?? Presumably because the split
   * list is in date order, and the most recent matches should be 
   * returned!?  */
  for (slp = g_list_last (acc->splits); slp; slp = slp->prev) {
    Split *lsplit = slp->data;
    Transaction *ltrans = xaccSplitGetParent(lsplit);

    if (safe_strcmp (description, xaccTransGetDescription (ltrans)) == 0)
    {
      if (split) *split = lsplit;
      if (trans) *trans = ltrans;
      return;
    }
  }
}

Split *
xaccAccountFindSplitByDesc(const Account *acc, const char *description)
{
  Split *split;

  /* Get the split which has a transaction matching the description. */
  finder_help_function(acc, description, &split, NULL);
  return split;
}

/* This routine is for finding a matching transaction in an account by
 * matching on the description field. [CAS: The rest of this comment
 * seems to belong somewhere else.] This routine is used for
 * auto-filling in registers with a default leading account. The
 * dest_trans is a transaction used for currency checking. */
Transaction *
xaccAccountFindTransByDesc(const Account *acc, const char *description)
{
  Transaction *trans;

  /* Get the transation matching the description. */
  finder_help_function(acc, description, NULL, &trans);
  return trans;
}

/* ================================================================ */
/* QofObject function implementation and registration */

static QofObject account_object_def = {
  interface_version:     QOF_OBJECT_VERSION,
  e_type:                GNC_ID_ACCOUNT,
  type_label:            "Account",
  create:                (gpointer)xaccMallocAccount,
  book_begin:            NULL,
  book_end:              NULL,
  is_dirty:              qof_collection_is_dirty,
  mark_clean:            qof_collection_mark_clean,
  foreach:               qof_collection_foreach,
  printable:             (const char* (*)(gpointer)) xaccAccountGetName,
  version_cmp:           (int (*)(gpointer,gpointer)) qof_instance_version_cmp,
};

gboolean xaccAccountRegister (void)
{
  static QofParam params[] = {
    { ACCOUNT_NAME_, QOF_TYPE_STRING, 
      (QofAccessFunc) xaccAccountGetName,
      (QofSetterFunc) xaccAccountSetName },
    { ACCOUNT_CODE_, QOF_TYPE_STRING, 
      (QofAccessFunc) xaccAccountGetCode,
      (QofSetterFunc) xaccAccountSetCode },
    { ACCOUNT_DESCRIPTION_, QOF_TYPE_STRING, 
      (QofAccessFunc) xaccAccountGetDescription,
      (QofSetterFunc) xaccAccountSetDescription },
    { ACCOUNT_NOTES_, QOF_TYPE_STRING, 
      (QofAccessFunc) xaccAccountGetNotes,
      (QofSetterFunc) xaccAccountSetNotes },
    { ACCOUNT_PRESENT_, QOF_TYPE_NUMERIC, 
      (QofAccessFunc) xaccAccountGetPresentBalance, NULL },
    { ACCOUNT_BALANCE_, QOF_TYPE_NUMERIC, 
      (QofAccessFunc) xaccAccountGetBalance, NULL },
    { ACCOUNT_CLEARED_, QOF_TYPE_NUMERIC, 
      (QofAccessFunc) xaccAccountGetClearedBalance, NULL },
    { ACCOUNT_RECONCILED_, QOF_TYPE_NUMERIC, 
      (QofAccessFunc) xaccAccountGetReconciledBalance, NULL },
    { ACCOUNT_TYPE_, QOF_TYPE_STRING, 
      (QofAccessFunc) qofAccountGetTypeString,
      (QofSetterFunc) qofAccountSetType },
    { ACCOUNT_FUTURE_MINIMUM_, QOF_TYPE_NUMERIC, 
      (QofAccessFunc) xaccAccountGetProjectedMinimumBalance, NULL },
    { ACCOUNT_TAX_RELATED, QOF_TYPE_BOOLEAN, 
      (QofAccessFunc) xaccAccountGetTaxRelated, 
      (QofSetterFunc) xaccAccountSetTaxRelated },
    { ACCOUNT_SCU, QOF_TYPE_INT32, 
      (QofAccessFunc) xaccAccountGetCommoditySCU,
      (QofSetterFunc) xaccAccountSetCommoditySCU },
    { ACCOUNT_NSCU, QOF_TYPE_BOOLEAN, 
      (QofAccessFunc) xaccAccountGetNonStdSCU, 
      (QofSetterFunc) xaccAccountSetNonStdSCU },
    { ACCOUNT_PARENT, GNC_ID_ACCOUNT,
      (QofAccessFunc) xaccAccountGetParentAccount, 
      (QofSetterFunc) qofAccountSetParent },
    { QOF_PARAM_BOOK, QOF_ID_BOOK, 
      (QofAccessFunc) qof_instance_get_book, NULL },
    { QOF_PARAM_GUID, QOF_TYPE_GUID, 
      (QofAccessFunc) qof_instance_get_guid, NULL },
    { ACCOUNT_KVP, QOF_TYPE_KVP, 
      (QofAccessFunc) qof_instance_get_slots, NULL },
    { NULL },
  };

  qof_class_register (GNC_ID_ACCOUNT, (QofSortFunc) xaccAccountOrder, params);

  return qof_object_register (&account_object_def);
}

/* ======================= END OF FILE =========================== */
