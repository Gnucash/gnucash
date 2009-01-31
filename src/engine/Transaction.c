/********************************************************************\
 * Transaction.c -- transaction implementation                      *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1997-2003 Linas Vepstas <linas@linas.org>          *
 * Copyright (C) 2000 Bill Gribble <grib@billgribble.com>           *
 * Copyright (c) 2006 David Hampton <hampton@employees.org>         *
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
#include <sys/time.h>
#include <time.h>
#include <unistd.h>

#include "AccountP.h"
#include "Scrub.h"
#include "Scrub3.h"
#include "TransactionP.h"
#include "SplitP.h"
#include "TransLog.h"
#include "cap-gains.h"
#include "gnc-commodity.h"
#include "gnc-engine.h"
#include "gnc-lot-p.h"
#include "gnc-lot.h"
#include "gnc-event.h"

#include "qofbackend-p.h"

/* Notes about xaccTransBeginEdit(), xaccTransCommitEdit(), and
 *  xaccTransRollback():
 *
 * Why use it:
 *
 *   Data consistency: Wrapping your changes to financial data inside
 *   a BeginEdit/CommitEdit block allows the engine to verify that
 *   your changes still leave the financial objects in an internally
 *   consistent state.  This is true even though you may make a series
 *   of individual changes that are not consistent by themselves.  In
 *   this way, it's like telling the engine, "Okay, I've finished my
 *   edits.  Please check my work."
 *
 *   Data integrity: The other benefit of the BeginEdit/CommitEdit
 *   block is that it allows the engine (and the backend) to remember
 *   the last known correct state of your data.  This allows you to
 *   undo any changes that you don't want to keep.  In this way, it's
 *   like telling the engine telling the back end, "Yes, I really mean
 *   it.  Remember this data." or "Nevermind, scratch that."  The
 *   important feature here is that if things go bad, for whatever
 *   reason (e.g. the application crashed, you lost the backend), your
 *   data remains in the state it was in just after the previous
 *   xaccTransCommitEdit().  [assuming no nesting, which probably
 *   isn't useful outside the engine.]
 *
 *   Note that the backend doesn't care about data consistency -
 *   that's the engine's job.
 *
 * Example Use:
 *
 *   xaccTransBeginEdit(trans);
 *
 *
 *   split = xaccMallocSplit(book);
 *   xaccSplitSetAccount(split, acc);
 *   xaccSplitSetParent(split, trans);  // Adding a new split
 *
 *   xaccSplitSetValue(split, val);     // Changing a split
 *
 *   xaccSplitDestroy(split);           // Removing a split
 *
 *   xaccTransSetNum(trans, "501");     // Changing the trans
 *
 *   if (really_do_it)
 *      xaccTransCommitEdit(trans);
 *   else
 *      xaccTransRollbackEdit(trans);
 *
 * How it works:
 *
 *   Calling xaccTransBeginEdit() starts a BeginEdit/CommitEdit block.
 *   Inside the block any changes to the transaction or any splits in
 *   the transaction are considered "pending".  What does that mean?
 *
 *   In general that means that if you set and then get the
 *   transaction's or split's parameters inside the
 *   BeginEdit/CommitEdit block, you'll get the values you just set.
 *   However, if you change an object's many-to-one relationship with
 *   another object, you won't see the change from the "many" side
 *   until the CommitEdit.  For example, if you move a split from one
 *   account into another, you can see the change with
 *   xaccSplitGetAccount(), but both Accounts' split lists won't be
 *   updated until the CommitEdit.  Correspondingly, no signals
 *   (events) will be generated for those "foreign" objects, or the
 *   Transaction, until the CommitEdit.
 *
 *   This behavior is important because, when we're finally ready to
 *   commit to the backend, we can't be 100% sure that the backend
 *   will still be available.  We have to offer the backend all of the
 *   new state as if it were already "true", but we need to save all of
 *   the old state in case the backend won't accept our commit.  If
 *   the backend commit fails, we have to restore all the old state.
 *   If the backend commit succeeds, and *only* after it succeeds, we
 *   can advertise the new state to the rest of the engine (and gui).
 *
 *  Q: Who owns the ref of an added split if the Transaction is rolled
 *  back?
 *
 *  A: This is a design decision.  If the answer is 'the user',
 *  then the burden is on the api user to check the transaction after
 *  every commit to see if the added split is really in the
 *  transaction.  If they don't they risk leaking the split if the
 *  commit was rolled back.  Another design is to answer 'the engine'.
 *  In that case the burden is on the engine to free a newly added
 *  split if the commit is rolled back.  Unfortunately the engine
 *  objects aren't ref-counted, so this is tricky.
 *
 *  In the current implementation, the answer is 'the engine', but
 *  that means that you must not add the split to two different
 *  transactions during the begin/commit block, because if one rolls
 *  back, they will both think they own the split.  This is one
 *  specific example of the general problem that the outcome of two
 *  parallel begin/commit edit blocks for two transactions where edits
 *  for both transactions involve the same splits and one or more
 *  edit-blocks is rolled-back, is poorly-defined.
 *
 *
 *
 * Design notes on event-generation: transaction-modified-events 
 * should not be generated until transaction commit or rollback
 * time.  They should not be generated as each field is tweaked. 
 * This for two reasons:
 * 1) Most editing events make multiple changes to a transaction,
 *    which would generate a flurry of (needless) events, if they
 *    weren't saved up till the commit.
 * 2) Technically, its incorrect to use transaction data 
 *    until the transaction is commited.  The GUI element that
 *    is changing the data can look at it, but all of the rest
 *    of the GUI should ignore the data until its commited.
 */

const char *trans_notes_str = "notes";
const char *void_reason_str = "void-reason";
const char *void_time_str = "void-time";
const char *void_former_notes_str = "void-former-notes";

/* KVP entry for date-due value */
#define TRANS_DATE_DUE_KVP       "trans-date-due"
#define TRANS_TXN_TYPE_KVP       "trans-txn-type"
#define TRANS_READ_ONLY_REASON   "trans-read-only"
#define TRANS_REVERSED_BY        "reversed-by"

#define ISO_DATELENGTH 32 /* length of an iso 8601 date string. */

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_ENGINE;

void check_open (const Transaction *trans)
{
  if (trans && 0 >= qof_instance_get_editlevel(trans))
    PERR ("transaction %p not open for editing", trans);
}
/********************************************************************\
\********************************************************************/
gboolean
xaccTransStillHasSplit(const Transaction *trans, const Split *s)
{
    return (s->parent == trans && !qof_instance_get_destroying(s));
}

/* Executes 'cmd_block' for each split currently in the transaction,
 * using the in-edit state.  Use the variable 's' for each split. */
#define FOR_EACH_SPLIT(trans, cmd_block) do {                           \
        GList *splits;                                                  \
        for (splits = (trans)->splits; splits; splits = splits->next) { \
            Split *s = splits->data;                                    \
            if (xaccTransStillHasSplit(trans, s)) {                     \
                cmd_block;                                              \
            }                                                           \
        }                                                               \
    } while (0)

G_INLINE_FUNC void mark_trans (Transaction *trans);
void mark_trans (Transaction *trans)
{
  FOR_EACH_SPLIT(trans, mark_split(s));
}

G_INLINE_FUNC void gen_event_trans (Transaction *trans);
void gen_event_trans (Transaction *trans)
{
#ifndef REGISTER_STILL_DEPENDS_ON_ACCOUNT_EVENTS
  GList *node;

  for (node = trans->splits; node; node = node->next)
  {
    Split *s = node->data;
    Account *account = s->acc;
    GNCLot *lot = s->lot;
    if (account)
      qof_event_gen (&account->inst, GNC_EVENT_ITEM_CHANGED, s);

    if (lot)
    {
      /* A change of transaction date might affect opening date of lot */
      qof_event_gen (&lot->inst, QOF_EVENT_MODIFY, NULL);
    }
  }
#endif
}

/* GObject Initialization */
QOF_GOBJECT_IMPL(gnc_transaction, Transaction, QOF_TYPE_INSTANCE);

static void
gnc_transaction_init(Transaction* txn)
{
}

static void
gnc_transaction_dispose_real (GObject *txnp)
{
}

static void
gnc_transaction_finalize_real(GObject* txnp)
{
}


/********************************************************************\
 * xaccInitTransaction
 * Initialize a transaction structure
\********************************************************************/

static void
xaccInitTransaction (Transaction * trans, QofBook *book)
{
  ENTER ("trans=%p", trans);
  /* Fill in some sane defaults */
  trans->num         = CACHE_INSERT("");
  trans->description = CACHE_INSERT("");

  trans->common_currency = NULL;
  trans->splits = NULL;

  trans->date_entered.tv_sec  = 0;
  trans->date_entered.tv_nsec = 0;

  trans->date_posted.tv_sec  = 0;
  trans->date_posted.tv_nsec = 0;

  trans->marker = 0;
  trans->orig = NULL;

  qof_instance_init_data (&trans->inst, GNC_ID_TRANS, book);
  LEAVE (" ");
}

/********************************************************************\
\********************************************************************/

Transaction *
xaccMallocTransaction (QofBook *book)
{
  Transaction *trans;

  g_return_val_if_fail (book, NULL);

  trans = g_object_new(GNC_TYPE_TRANSACTION, NULL);
  xaccInitTransaction (trans, book);
  qof_event_gen (&trans->inst, QOF_EVENT_CREATE, NULL);

  return trans;
}

#ifdef DUMP_FUNCTIONS
void
xaccTransDump (const Transaction *trans, const char *tag)
{
  GList *node;

  printf("%s Trans %p", tag, trans);
  printf("    Entered:     %s\n", gnc_print_date(trans->date_entered));
  printf("    Posted:      %s\n", gnc_print_date(trans->date_posted));
  printf("    Num:         %s\n", trans->num ? trans->num : "(null)");
  printf("    Description: %s\n", 
         trans->description ? trans->description : "(null)");
  printf("    Currency:    %s\n", 
         gnc_commodity_get_printname(trans->common_currency));
  printf("    version:     %x\n", qof_instance_get_version(trans));
  printf("    version_chk: %x\n", qof_instance_get_version_check(trans));
  printf("    editlevel:   %x\n", qof_instance_get_editlevel(trans));
  printf("    orig:        %p\n", trans->orig);
  printf("    idata:       %x\n", qof_instance_get_idata(trans));
  printf("    splits:      ");
  for (node = trans->splits; node; node = node->next)
  {
    printf("%p ", node->data);
  }
  printf("\n");
  for (node = trans->splits; node; node = node->next)
  {
    xaccSplitDump(node->data, tag);
  }
  printf("\n");
}
#endif

void
xaccTransSortSplits (Transaction *trans)
{
  GList *node, *new_list = NULL;
  Split *split;

  /* first debits */
  for (node = trans->splits; node; node = node->next) {
    split = node->data;
    if (gnc_numeric_negative_p (xaccSplitGetValue(split)))
      continue;
    new_list = g_list_append(new_list, split);
  }

  /* then credits */
  for (node = trans->splits; node; node = node->next) {
    split = node->data;
    if (!gnc_numeric_negative_p (xaccSplitGetValue(split)))
      continue;
    new_list = g_list_append(new_list, split);
  }

  /* install newly sorted list */
  g_list_free(trans->splits);
  trans->splits = new_list;
}


/********************************************************************\
\********************************************************************/
/* This routine is not exposed externally, since it does weird things, 
 * like not really owning the splits correctly, and other weirdnesses. 
 * This routine is prone to programmer snafu if not used correctly. 
 * It is used only by the edit-rollback code.
 */
/* Actually, it *is* public, and used by Period.c */
Transaction *
xaccDupeTransaction (const Transaction *t)
{
  Transaction *trans;
  GList *node;

  trans = g_object_new (GNC_TYPE_TRANSACTION, NULL);

  trans->num         = CACHE_INSERT (t->num);
  trans->description = CACHE_INSERT (t->description);

  trans->splits = g_list_copy (t->splits);
  for (node = trans->splits; node; node = node->next)
  {
    node->data = xaccDupeSplit (node->data);
  }

  trans->date_entered = t->date_entered;
  trans->date_posted = t->date_posted;
  qof_instance_copy_version(trans, t);
  trans->orig = NULL;

  trans->common_currency = t->common_currency;

  /* Trash the guid and entity table. We don't want to mistake 
   * the cloned transaction as something official.  If we ever 
   * use this transaction, we'll have to fix this up.
   */
  trans->inst.e_type = NULL;
  qof_instance_set_guid(trans, guid_null());
  qof_instance_copy_book(trans, t);
  trans->inst.kvp_data = kvp_frame_copy (t->inst.kvp_data);

  return trans;
}

/*
 * Use this routine to externally duplicate a transaction.  It creates
 * a full fledged transaction with unique guid, splits, etc.
 */
Transaction *
xaccTransClone (const Transaction *t)
{
  Transaction *trans;
  Split *split;
  GList *node;

  qof_event_suspend();
  trans = g_object_new (GNC_TYPE_TRANSACTION, NULL);

  trans->date_entered    = t->date_entered;
  trans->date_posted     = t->date_posted;
  trans->num             = CACHE_INSERT (t->num);
  trans->description     = CACHE_INSERT (t->description);
  trans->common_currency = t->common_currency;
  qof_instance_copy_version(trans, t);
  qof_instance_copy_version_check(trans, t);

  trans->orig            = NULL;

  qof_instance_init_data (&trans->inst, GNC_ID_TRANS, qof_instance_get_book(t));
  kvp_frame_delete (trans->inst.kvp_data);
  trans->inst.kvp_data    = kvp_frame_copy (t->inst.kvp_data);

  xaccTransBeginEdit(trans);
  for (node = t->splits; node; node = node->next)
  {
    split = xaccSplitClone(node->data);
    split->parent = trans;
    trans->splits = g_list_append (trans->splits, split);
  }
  qof_instance_set_dirty(QOF_INSTANCE(trans));
  xaccTransCommitEdit(trans);
  qof_event_resume();

  return trans;
}


/********************************************************************\
\********************************************************************/

static void
xaccFreeTransaction (Transaction *trans)
{
  GList *node;

  if (!trans) return;

  ENTER ("(addr=%p)", trans);
  if (((char *) 1) == trans->num)
  {
    PERR ("double-free %p", trans);
    LEAVE (" ");
    return;
  }

  /* free up the destination splits */
  for (node = trans->splits; node; node = node->next)
    xaccFreeSplit (node->data);
  g_list_free (trans->splits);
  trans->splits = NULL;

  /* free up transaction strings */
  CACHE_REMOVE(trans->num);
  CACHE_REMOVE(trans->description);

  /* Just in case someone looks up freed memory ... */
  trans->num         = (char *) 1;
  trans->description = NULL;

  trans->date_entered.tv_sec = 0;
  trans->date_entered.tv_nsec = 0;
  trans->date_posted.tv_sec = 0;
  trans->date_posted.tv_nsec = 0;

  if (trans->orig)
  {
    xaccFreeTransaction (trans->orig);
    trans->orig = NULL;
  }

  /* qof_instance_release (&trans->inst); */
  g_object_unref(trans);

  LEAVE ("(addr=%p)", trans);
}

/********************************************************************
 xaccTransEqual

 Compare two transactions for equality.  We don't pay any attention to
 rollback issues here, and we only care about equality of "permanent
 fields", basically the things that would survive a file save/load
 cycle.

 ********************************************************************/

/* return 0 when splits have equal guids */
static gint
compare_split_guids (gconstpointer a, gconstpointer b)
{
  const Split *sa = a;
  const Split *sb = b;

  if (sa == sb) return 0;
  if (!sa || !sb) return 1;

  return guid_compare (xaccSplitGetGUID (sa), xaccSplitGetGUID (sb));
}

gboolean
xaccTransEqual(const Transaction *ta, const Transaction *tb,
               gboolean check_guids,
               gboolean check_splits,
               gboolean check_balances,
               gboolean assume_ordered)
{
  if (!ta && !tb) return TRUE; /* Arguable.  FALSE may be better. */

  if (!ta || !tb)
  {
    PWARN ("one is NULL");
    return FALSE;
  }

  if (ta == tb) return TRUE;

  if (check_guids) {
    if (qof_instance_guid_compare(ta, tb) != 0)
    {
      PWARN ("GUIDs differ");
      return FALSE;
    }
  }

  if (!gnc_commodity_equal(ta->common_currency, tb->common_currency))
  {
    PWARN ("commodities differ %s vs %s",
           gnc_commodity_get_unique_name (ta->common_currency),
           gnc_commodity_get_unique_name (tb->common_currency));
    return FALSE;
  }

  if (timespec_cmp(&(ta->date_entered), &(tb->date_entered)))
  {
    PWARN ("date entered differs");
    return FALSE;
  }

  if (timespec_cmp(&(ta->date_posted), &(tb->date_posted)))
  {
    PWARN ("date posted differs");
    return FALSE;
  }

  /* Since we use cached strings, we can just compare pointer
   * equality for num and description
   */
  if (ta->num != tb->num)
  {
    PWARN ("num differs: %s vs %s", ta->num, tb->num);
    return FALSE;
  }

  if (ta->description != tb->description)
  {
    PWARN ("descriptions differ: %s vs %s", ta->description, tb->description);
    return FALSE;
  }

  if (kvp_frame_compare(ta->inst.kvp_data, tb->inst.kvp_data) != 0)
  {
    char *frame_a;
    char *frame_b;

    frame_a = kvp_frame_to_string (ta->inst.kvp_data);
    frame_b = kvp_frame_to_string (tb->inst.kvp_data);

    PWARN ("kvp frames differ:\n%s\n\nvs\n\n%s", frame_a, frame_b);

    g_free (frame_a);
    g_free (frame_b);

    return FALSE;
  }

  if (check_splits)
  {
    if ((!ta->splits && tb->splits) || (!tb->splits && ta->splits))
    {
      PWARN ("only one has splits");
      return FALSE;
    }

    if (ta->splits && tb->splits)
    {
      GList *node_a, *node_b;

      for (node_a = ta->splits, node_b = tb->splits;
           node_a;
           node_a = node_a->next, node_b = node_b->next)
      {
        Split *split_a = node_a->data;
        Split *split_b;

        /* don't presume that the splits are in the same order */
        if (!assume_ordered)
          node_b = g_list_find_custom (tb->splits, split_a, 
                                       compare_split_guids);

        if (!node_b)
        {
          PWARN ("first has split %s and second does not",
                 guid_to_string (xaccSplitGetGUID (split_a)));
          return FALSE;
        }

        split_b = node_b->data;

        if (!xaccSplitEqual (split_a, split_b, check_guids, check_balances, 
                             FALSE))
        {
          char str_a[GUID_ENCODING_LENGTH+1];
          char str_b[GUID_ENCODING_LENGTH+1];

          guid_to_string_buff (xaccSplitGetGUID (split_a), str_a);
          guid_to_string_buff (xaccSplitGetGUID (split_b), str_b);

          PWARN ("splits %s and %s differ", str_a, str_b);
          return FALSE;
        }
      }

      if (g_list_length (ta->splits) != g_list_length (tb->splits))
      {
        PWARN ("different number of splits");
        return FALSE;
      }
    }
  }

  return TRUE;
}

/********************************************************************\
\********************************************************************/

Transaction *
xaccTransLookup (const GUID *guid, QofBook *book)
{
  QofCollection *col;
  if (!guid || !book) return NULL;
  col = qof_book_get_collection (book, GNC_ID_TRANS);
  return (Transaction *) qof_collection_lookup_entity (col, guid);
}

/********************************************************************\
\********************************************************************/

gnc_numeric
xaccTransGetImbalance (const Transaction * trans)
{
  gnc_numeric imbal = gnc_numeric_zero();
  if (!trans) return imbal;

  ENTER("(trans=%p)", trans);
  /* Could use xaccSplitsComputeValue, except that we want to use
     GNC_HOW_DENOM_EXACT */
  FOR_EACH_SPLIT(trans, imbal =
                 gnc_numeric_add(imbal, xaccSplitGetValue(s),
                                 GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT));
  LEAVE("(trans=%p) imbal=%s", trans, gnc_num_dbg_to_string(imbal));
  return imbal;
}

gnc_numeric
xaccTransGetAccountValue (const Transaction *trans, 
                          const Account *acc)
{
  gnc_numeric total = gnc_numeric_zero ();
  if (!trans || !acc) return total;

  FOR_EACH_SPLIT(trans, if (acc == xaccSplitGetAccount(s)) {
                     total = gnc_numeric_add (total, xaccSplitGetValue (s),
                                              GNC_DENOM_AUTO,
                                              GNC_HOW_DENOM_EXACT);
                 });
  return total;
}

gnc_numeric
xaccTransGetAccountAmount (const Transaction *trans, const Account *acc)
{
  gnc_numeric total = gnc_numeric_zero ();
  if (!trans || !acc) return total;

  total = gnc_numeric_convert (total, xaccAccountGetCommoditySCU (acc),
                               GNC_RND_ROUND);
  FOR_EACH_SPLIT(trans, if (acc == xaccSplitGetAccount(s))
                            total = gnc_numeric_add_fixed(
                                total, xaccSplitGetAmount(s)));
  return total;
}

gnc_numeric
xaccTransGetAccountConvRate(Transaction *txn, Account *acc)
{
  gnc_numeric amount, value, convrate;
  GList *splits;
  Split *s;
  gboolean found_acc_match = FALSE;

  /* We need to compute the conversion rate into _this account_.  So,
   * find the first split into this account, compute the conversion
   * rate (based on amount/value), and then return this conversion
   * rate.
   */
  if (gnc_commodity_equal(xaccAccountGetCommodity(acc), 
                          xaccTransGetCurrency(txn)))
      return gnc_numeric_create(1, 1);

  for (splits = txn->splits; splits; splits = splits->next) {
    s = splits->data;

    if (!xaccTransStillHasSplit(txn, s))
      continue;
    if (xaccSplitGetAccount (s) != acc)
      continue;

    found_acc_match = TRUE;
    amount = xaccSplitGetAmount (s);

    /* Ignore splits with "zero" amount */
    if (gnc_numeric_zero_p (amount))
      continue;

    value = xaccSplitGetValue (s);
    if (gnc_numeric_zero_p (value))
        PWARN("How can amount be nonzero and value be zero?");

    convrate = gnc_numeric_div(amount, value, GNC_DENOM_AUTO, GNC_DENOM_REDUCE);
    return convrate;
  }

  if (acc) {
    /* If we did find a matching account but its amount was zero,
     * then perhaps this is a "special" income/loss transaction
     */
    if (found_acc_match)
      return gnc_numeric_zero();
    else
      PERR("Cannot convert transaction -- no splits with proper conversion ratio");
  }
  return gnc_numeric_create (100, 100);
}

gnc_numeric
xaccTransGetAccountBalance (const Transaction *trans,
                            const Account *account)
{
  GList *node;
  Split *last_split = NULL;

  // Not really the appropriate error value.
  g_return_val_if_fail(account && trans, gnc_numeric_error(GNC_ERROR_ARG));

  for (node = trans->splits; node; node = node->next)
  {
    Split *split = node->data;

    if (!xaccTransStillHasSplit(trans, split))
      continue;
    if (xaccSplitGetAccount(split) != account)
      continue;

    if (!last_split)
    {
      last_split = split;
      continue;
    }

    /* This test needs to correspond to the comparison function used when
       sorting the splits for computing the running balance. */
    if (xaccSplitOrder (last_split, split) < 0)
      last_split = split;
  }

  return xaccSplitGetBalance (last_split);
}

/********************************************************************\
\********************************************************************/
/* The new routine for setting the common currency */

gnc_commodity *
xaccTransGetCurrency (const Transaction *trans)
{
  return trans ? trans->common_currency : NULL;
}

void
xaccTransSetCurrency (Transaction *trans, gnc_commodity *curr)
{
  gint fraction, old_fraction;

  if (!trans || !curr || trans->common_currency == curr) return;
  xaccTransBeginEdit(trans);

  old_fraction = gnc_commodity_get_fraction (trans->common_currency);
  trans->common_currency = curr;
  fraction = gnc_commodity_get_fraction (curr);

  /* avoid needless crud if fraction didn't change */
  if (fraction != old_fraction) {
      FOR_EACH_SPLIT(trans, xaccSplitSetValue(s, xaccSplitGetValue(s)));
  }

  qof_instance_set_dirty(QOF_INSTANCE(trans));
  mark_trans(trans);  /* Dirty balance of every account in trans */
  xaccTransCommitEdit(trans);
}

/********************************************************************\
\********************************************************************/

void
xaccTransBeginEdit (Transaction *trans)
{
   if (!trans) return;
   if (!qof_begin_edit(&trans->inst)) return;

   if (qof_book_shutting_down(qof_instance_get_book(trans))) return;

   xaccOpenLog ();
   xaccTransWriteLog (trans, 'B');

   /* Make a clone of the transaction; we will use this 
    * in case we need to roll-back the edit. */
   trans->orig = xaccDupeTransaction (trans);
}

/********************************************************************\
\********************************************************************/

void
xaccTransDestroy (Transaction *trans)
{
  if (!trans) return;

  if (!xaccTransGetReadOnly (trans) || 
      qof_book_shutting_down(qof_instance_get_book(trans))) {
      xaccTransBeginEdit(trans);
      qof_instance_set_destroying(trans, TRUE);
      xaccTransCommitEdit(trans);
  }
}

static void
destroy_gains (Transaction *trans)
{
  SplitList *node;
  for (node = trans->splits; node; node = node->next)
  {
    Split *s = node->data;
    if (!xaccTransStillHasSplit(trans, s))
      continue;

    if (GAINS_STATUS_UNKNOWN == s->gains) xaccSplitDetermineGainStatus(s);
    if (s->gains_split && (GAINS_STATUS_GAINS & s->gains_split->gains))
    {
      Transaction *t = s->gains_split->parent;
      xaccTransDestroy (t);
      s->gains_split = NULL;
    }
  }
}

static void
do_destroy (Transaction *trans)
{
  SplitList *node;
  gboolean shutting_down = qof_book_shutting_down(qof_instance_get_book(trans));

  /* If there are capital-gains transactions associated with this, 
   * they need to be destroyed too.  */
  destroy_gains (trans);

  /* Make a log in the journal before destruction.  */
  if (!shutting_down)
    xaccTransWriteLog (trans, 'D');

  qof_event_gen (&trans->inst, QOF_EVENT_DESTROY, NULL);

  /* We only own the splits that still think they belong to us. */
  trans->splits = g_list_copy(trans->splits);
  for (node = trans->splits; node; node = node->next) {
      Split *s = node->data;
      if (s->parent == trans) {
          xaccSplitDestroy(s);
          xaccSplitCommitEdit(s);
      }
  }
  g_list_free (trans->splits);
  trans->splits = NULL;
  xaccFreeTransaction (trans);
}

/********************************************************************\
\********************************************************************/

/* Temporary hack for data consitency */
static int scrub_data = 1;
void xaccEnableDataScrubbing(void) { scrub_data = 1; }
void xaccDisableDataScrubbing(void) { scrub_data = 0; }

/* Check for an implicitly deleted transaction */
static gboolean was_trans_emptied(Transaction *trans)
{
    FOR_EACH_SPLIT(trans, return FALSE);
    return TRUE;
}

static void trans_on_error(Transaction *trans, QofBackendError errcode)
{
    /* If the backend puked, then we must roll-back
     * at this point, and let the user know that we failed.
     * The GUI should check for error conditions ...
     */
    if (ERR_BACKEND_MODIFIED == errcode) {
        PWARN("Another user has modified this transaction\n"
              "\tjust a moment ago. Please look at their changes,\n"
              "\tand try again, if needed.\n");
    }

    xaccTransRollbackEdit(trans);
}

static void trans_cleanup_commit(Transaction *trans)
{
    GList *slist, *node;

    /* ------------------------------------------------- */
    /* Make sure all associated splits are in proper order
     * in their accounts with the correct balances. */

    /* Iterate over existing splits */
    slist = g_list_copy(trans->splits);
    for (node = slist; node; node = node->next) {
        Split *s = node->data;
        if (!qof_instance_is_dirty(QOF_INSTANCE(s)))
            continue;

        if ((s->parent != trans) || qof_instance_get_destroying(s)) {
            /* Existing split either moved to another transaction or
               was destroyed, drop from list */
            GncEventData ed;
            ed.node = trans;
            ed.idx = g_list_index(trans->splits, s);
            trans->splits = g_list_remove(trans->splits, s);
            qof_event_gen(&s->inst, QOF_EVENT_REMOVE, &ed);
        }

        if (s->parent == trans) {
            /* Split was either added, destroyed or just changed */
            if (qof_instance_get_destroying(s))
                qof_event_gen(&s->inst, QOF_EVENT_DESTROY, NULL);
            else qof_event_gen(&s->inst, QOF_EVENT_MODIFY, NULL);
            xaccSplitCommitEdit(s);
        }
    }
    g_list_free(slist);

    xaccTransWriteLog (trans, 'C');

    /* Get rid of the copy we made. We won't be rolling back,
     * so we don't need it any more.  */
    PINFO ("get rid of rollback trans=%p", trans->orig);
    xaccFreeTransaction (trans->orig);
    trans->orig = NULL;

    /* Sort the splits. Why do we need to do this ?? */
    /* Good question.  Who knows?  */
    xaccTransSortSplits(trans);

    /* Put back to zero. */
    qof_instance_decrease_editlevel(trans);
    g_assert(qof_instance_get_editlevel(trans) == 0);

    gen_event_trans (trans); //TODO: could be conditional
    qof_event_gen (&trans->inst, QOF_EVENT_MODIFY, NULL);
}

void
xaccTransCommitEdit (Transaction *trans)
{
   if (!trans) return;
   ENTER ("(trans=%p)", trans);

   if (!qof_commit_edit (QOF_INSTANCE(trans))) {
       LEAVE("editlevel non-zero");
       return;
   }

   /* We increment this for the duration of the call
    * so other functions don't result in a recursive
    * call to xaccTransCommitEdit. */
   qof_instance_increase_editlevel(trans);

   if (was_trans_emptied(trans))
     qof_instance_set_destroying(trans, TRUE);

   /* Before commiting the transaction, we're gonna enforce certain
    * constraints.  In particular, we want to enforce the cap-gains
    * and the balanced lot constraints.  These constraints might 
    * change the number of splits in this transaction, and the 
    * transaction itself might be deleted.  This is also why
    * we can't really enforce these constraints elsewhere: they
    * can cause pointers to splits and transactions to disapear out
    * from under the holder.
    */
   if (!qof_instance_get_destroying(trans) && scrub_data && 
       !qof_book_shutting_down(xaccTransGetBook(trans))) {
     /* If scrubbing gains recurses through here, don't call it again. */
     scrub_data = 0; 
     /* The total value of the transaction should sum to zero. 
      * Call the trans scrub routine to fix it.   Indirectly, this 
      * routine also performs a number of other transaction fixes too.
      */
     xaccTransScrubImbalance (trans, NULL, NULL);
     /* Get the cap gains into a consistent state as well. */
     
     /* Lot Scrubbing is temporarily disabled. */
     if (g_getenv("GNC_AUTO_SCRUB_LOTS") != NULL)
         xaccTransScrubGains (trans, NULL);

     /* Allow scrubbing in transaction commit again */
     scrub_data = 1;
   }

   /* Record the time of last modification */
   if (0 == trans->date_entered.tv_sec) {
      struct timeval tv;
#ifdef HAVE_GETTIMEOFDAY
      gettimeofday (&tv, NULL);
#else
      time (&(tv.tv_sec));
      tv.tv_usec = 0;
#endif
      trans->date_entered.tv_sec = tv.tv_sec;
      trans->date_entered.tv_nsec = 1000 * tv.tv_usec;
   }

   qof_commit_edit_part2(QOF_INSTANCE(trans),
                         (void (*) (QofInstance *, QofBackendError))
                         trans_on_error,
                         (void (*) (QofInstance *)) trans_cleanup_commit,
                         (void (*) (QofInstance *)) do_destroy);
   LEAVE ("(trans=%p)", trans);
}

#define SWAP(a, b) do { gpointer tmp = (a); (a) = (b); (b) = tmp; } while (0);

/* Ughhh. The Rollback function is terribly complex, and, what's worse,
 * it only rolls back the basics.  The TransCommit functions did a bunch
 * of Lot/Cap-gains scrubbing that don't get addressed/undone here, and
 * so the rollback can potentially leave a bit of a mess behind.  We
 * really need a more robust undo capability.  Part of the problem is
 * that the biggest user of the undo is the multi-user backend, which
 * also adds complexity.
 */
void
xaccTransRollbackEdit (Transaction *trans)
{
   GList *node, *onode;
   QofBackend *be;
   Transaction *orig;
   GList *slist;
   int num_preexist, i;
   ENTER ("trans addr=%p\n", trans);

   check_open(trans);

   /* copy the original values back in. */
   orig = trans->orig;
   SWAP(trans->num, orig->num);
   SWAP(trans->description, orig->description);
   trans->date_entered = orig->date_entered;
   trans->date_posted = orig->date_posted;
   SWAP(trans->common_currency, orig->common_currency);
   SWAP(trans->inst.kvp_data, orig->inst.kvp_data);

   /* The splits at the front of trans->splits are exactly the same
      splits as in the original, but some of them may have changed, so
      we restore only those. */
   num_preexist = g_list_length(orig->splits);
   slist = g_list_copy(trans->splits);
   for (i = 0, node = slist, onode = orig->splits; node;
        i++, node = node->next, onode = onode ? onode->next : NULL) {
       Split *s = node->data;

       if (!qof_instance_is_dirty(QOF_INSTANCE(s)))
           continue;

       if (i < num_preexist) {
           Split *so = onode->data;

           xaccSplitRollbackEdit(s);
           SWAP(s->action, so->action);
           SWAP(s->memo, so->memo);
           SWAP(s->inst.kvp_data, so->inst.kvp_data);
           s->reconciled = so->reconciled;
           s->amount = so->amount;
           s->value = so->value;
           s->lot = so->lot;
           s->gains_split = s->gains_split;
           //SET_GAINS_A_VDIRTY(s);
           s->date_reconciled = so->date_reconciled;
           qof_instance_mark_clean(QOF_INSTANCE(s));
           xaccFreeSplit(so);
       } else {
           /* Potentially added splits */
           if (trans != xaccSplitGetParent(s)) {
               trans->splits = g_list_remove(trans->splits, s);
               /* New split added, but then moved to another
                  transaction */
               continue;
           }
           xaccSplitRollbackEdit(s);
           trans->splits = g_list_remove(trans->splits, s);
           g_assert(trans != xaccSplitGetParent(s));
           /* NB: our memory management policy here is that a new split
              added to the transaction which is then rolled-back still
              belongs to the engine.  Specifically, it's freed by the
              transaction to which it was added.  Don't add the Split to
              more than one transaction during the begin/commit block! */
           if (NULL == xaccSplitGetParent(s)) {
               xaccFreeSplit(s);  // a newly malloc'd split
           }
       }
   }
   g_list_free(slist);
   g_list_free(orig->splits);
   orig->splits = NULL;

   /* Now that the engine copy is back to its original version,
    * get the backend to fix it in the database */
   be = qof_book_get_backend(qof_instance_get_book(trans));
   /** \todo Fix transrollbackedit in QOF so that rollback
   is exposed via the API. */
   if (be && be->rollback) 
   {
      QofBackendError errcode;

      /* clear errors */
      do {
        errcode = qof_backend_get_error (be);
      } while (ERR_BACKEND_NO_ERR != errcode);

      (be->rollback) (be, &(trans->inst));

      errcode = qof_backend_get_error (be);
      if (ERR_BACKEND_MOD_DESTROY == errcode)
      {
         /* The backend is asking us to delete this transaction.
          * This typically happens because another (remote) user
          * has deleted this transaction, and we haven't found
          * out about it until this user tried to edit it.
          */
         xaccTransDestroy (trans);
         do_destroy (trans);

         /* push error back onto the stack */
         qof_backend_set_error (be, errcode);
         LEAVE ("deleted trans addr=%p\n", trans);
         return;
      }
      if (ERR_BACKEND_NO_ERR != errcode) 
      {
        PERR ("Rollback Failed.  Ouch!");
        /* push error back onto the stack */
        qof_backend_set_error (be, errcode);
      }
   }

   xaccTransWriteLog (trans, 'R');

   xaccFreeTransaction (trans->orig);

   trans->orig = NULL;
   qof_instance_set_destroying(trans, FALSE);

   /* Put back to zero. */
   qof_instance_decrease_editlevel(trans);
   /* FIXME: The register code seems to depend on the engine to
      generate an event during rollback, even though the state is just
      reverting to what it was. */
   gen_event_trans (trans);

   LEAVE ("trans addr=%p\n", trans);
}

gboolean
xaccTransIsOpen (const Transaction *trans)
{
  return trans ? (0 < qof_instance_get_editlevel(trans)) : FALSE;
}

#define SECS_PER_DAY 86400

int
xaccTransOrder (const Transaction *ta, const Transaction *tb)
{
  char *da, *db;
  int na, nb, retval;

  if ( ta && !tb ) return -1;
  if ( !ta && tb ) return +1;
  if ( !ta && !tb ) return 0;

  /* if dates differ, return */
  DATE_CMP(ta,tb,date_posted);

  /* otherwise, sort on number string */
  na = atoi(ta->num);
  nb = atoi(tb->num);
  if (na < nb) return -1;
  if (na > nb) return +1;

  /* if dates differ, return */
  DATE_CMP(ta,tb,date_entered);

  /* otherwise, sort on description string */
  da = ta->description ? ta->description : "";
  db = tb->description ? tb->description : "";
  retval = g_utf8_collate (da, db);
  if (retval)
    return retval;

  /* else, sort on guid - keeps sort stable. */
  return qof_instance_guid_compare(ta, tb);
}

/********************************************************************\
\********************************************************************/

static inline void
xaccTransSetDateInternal(Transaction *trans, Timespec *dadate, Timespec val)
{
    xaccTransBeginEdit(trans);

    {
        time_t secs = (time_t) val.tv_sec;
        gchar *tstr = ctime(&secs);
        PINFO ("addr=%p set date to %" G_GUINT64_FORMAT ".%09ld %s",
               trans, val.tv_sec, val.tv_nsec, tstr ? tstr : "(null)");
    }
    
    *dadate = val;
    qof_instance_set_dirty(QOF_INSTANCE(trans));
    mark_trans(trans);
    xaccTransCommitEdit(trans);

   /* Because the date has changed, we need to make sure that each of
    * the splits is properly ordered in each of their accounts. We
    * could do that here, simply by reinserting each split into its
    * account. However, in some ways this is bad behaviour, and it
    * seems much better/nicer to defer that until the commit phase,
    * i.e. until the user has called the xaccTransCommitEdit()
    * routine. So, for now, we are done. */
}

static inline void
set_gains_date_dirty (Transaction *trans)
{
    FOR_EACH_SPLIT(trans, s->gains |= GAINS_STATUS_DATE_DIRTY);
}

void
xaccTransSetDatePostedSecs (Transaction *trans, time_t secs)
{
   Timespec ts = {secs, 0};
   if (!trans) return;
   xaccTransSetDateInternal(trans, &trans->date_posted, ts);
   set_gains_date_dirty (trans);
}

void
xaccTransSetDateEnteredSecs (Transaction *trans, time_t secs)
{
   Timespec ts = {secs, 0};
   if (!trans) return;
   xaccTransSetDateInternal(trans, &trans->date_entered, ts);
}

static void
qofTransSetDatePosted (Transaction *trans, Timespec ts)
{
   if (!trans) return;
   if ((ts.tv_nsec == 0) && (ts.tv_sec == 0)) return;
   if (!qof_begin_edit(&trans->inst)) return;
   xaccTransSetDateInternal(trans, &trans->date_posted, ts);
   set_gains_date_dirty(trans);
   qof_commit_edit(&trans->inst);
}

void
xaccTransSetDatePostedTS (Transaction *trans, const Timespec *ts)
{
   if (!trans || !ts) return;
   xaccTransSetDateInternal(trans, &trans->date_posted, *ts);
   set_gains_date_dirty (trans);
}

static void
qofTransSetDateEntered (Transaction *trans, Timespec ts)
{
   if (!trans) return;
   if ((ts.tv_nsec == 0) && (ts.tv_sec == 0)) return;
   if (!qof_begin_edit(&trans->inst)) return;
   xaccTransSetDateInternal(trans, &trans->date_entered, ts);
   qof_commit_edit(&trans->inst);
}

void
xaccTransSetDateEnteredTS (Transaction *trans, const Timespec *ts)
{
   if (!trans || !ts) return;
   xaccTransSetDateInternal(trans, &trans->date_entered, *ts);
}

void
xaccTransSetDate (Transaction *trans, int day, int mon, int year) 
{
   Timespec ts;
   if (!trans) return;
   ts = gnc_dmy2timespec(day, mon, year);
   xaccTransSetDateInternal(trans, &trans->date_posted, ts);
   set_gains_date_dirty (trans);
}

void
xaccTransSetDateDueTS (Transaction *trans, const Timespec *ts)
{
   if (!trans || !ts) return;
   xaccTransBeginEdit(trans);
   kvp_frame_set_timespec (trans->inst.kvp_data, TRANS_DATE_DUE_KVP, *ts);
   qof_instance_set_dirty(QOF_INSTANCE(trans));
   xaccTransCommitEdit(trans);
}

void
xaccTransSetTxnType (Transaction *trans, char type)
{
  char s[2] = {type, '\0'};
  g_return_if_fail(trans);
  xaccTransBeginEdit(trans);
  kvp_frame_set_str (trans->inst.kvp_data, TRANS_TXN_TYPE_KVP, s);
  qof_instance_set_dirty(QOF_INSTANCE(trans));
  xaccTransCommitEdit(trans);
}

void xaccTransClearReadOnly (Transaction *trans)
{
    if (trans) {
        xaccTransBeginEdit(trans);
        kvp_frame_set_slot_path (trans->inst.kvp_data, NULL, 
                                 TRANS_READ_ONLY_REASON, NULL);
        qof_instance_set_dirty(QOF_INSTANCE(trans));
        xaccTransCommitEdit(trans);
    }
}

void
xaccTransSetReadOnly (Transaction *trans, const char *reason)
{
    if (trans && reason) {
        xaccTransBeginEdit(trans);
        kvp_frame_set_str (trans->inst.kvp_data, 
                           TRANS_READ_ONLY_REASON, reason);
        qof_instance_set_dirty(QOF_INSTANCE(trans));
        xaccTransCommitEdit(trans);
    }
}

/********************************************************************\
\********************************************************************/

/* QOF does not open the trans before setting a parameter,
but the call uses check_open so we cannot use the call directly. */
static void
qofTransSetNum (Transaction *trans, const char *xnum)
{
	if (!qof_begin_edit(&trans->inst)) return;
	xaccTransSetNum(trans, xnum);
	qof_commit_edit(&trans->inst);
}

void
xaccTransSetNum (Transaction *trans, const char *xnum)
{
   if (!trans || !xnum) return;
   xaccTransBeginEdit(trans);

   CACHE_REPLACE(trans->num, xnum);
   qof_instance_set_dirty(QOF_INSTANCE(trans));
   mark_trans(trans);  /* Dirty balance of every account in trans */
   xaccTransCommitEdit(trans);
}

static void
qofTransSetDescription (Transaction *trans, const char *desc)
{
	if (!qof_begin_edit(&trans->inst)) return;
	xaccTransSetDescription(trans, desc);
	qof_commit_edit(&trans->inst);
}

void
xaccTransSetDescription (Transaction *trans, const char *desc)
{
   if (!trans || !desc) return;
   xaccTransBeginEdit(trans);

   CACHE_REPLACE(trans->description, desc);
   qof_instance_set_dirty(QOF_INSTANCE(trans));
   xaccTransCommitEdit(trans);
}

static void
qofTransSetNotes (Transaction *trans, const char *notes)
{
	if (!qof_begin_edit(&trans->inst)) return;
	xaccTransSetNotes(trans, notes);
	qof_commit_edit(&trans->inst);
}

void
xaccTransSetNotes (Transaction *trans, const char *notes)
{
  if (!trans || !notes) return;
  xaccTransBeginEdit(trans);

  kvp_frame_set_str (trans->inst.kvp_data, trans_notes_str, notes);
  qof_instance_set_dirty(QOF_INSTANCE(trans));
  xaccTransCommitEdit(trans);
}

/********************************************************************\
\********************************************************************/

Split *
xaccTransGetSplit (const Transaction *trans, int i) 
{
    int j = 0;
    if (!trans || i < 0) return NULL;

    FOR_EACH_SPLIT(trans, { if (i == j) return s; j++; });
    return NULL;
}

int
xaccTransGetSplitIndex(const Transaction *trans, const Split *split) 
{
    int j = 0;
    g_return_val_if_fail(trans && split, -1);

    FOR_EACH_SPLIT(trans, { if (s == split) return j; j++; });
    return -1;
}

SplitList *
xaccTransGetSplitList (const Transaction *trans)
{
  return trans ? trans->splits : NULL;
}

int
xaccTransCountSplits (const Transaction *trans)
{
    gint i = 0;
    FOR_EACH_SPLIT(trans, i++);
    return i;
}

const char *
xaccTransGetNum (const Transaction *trans)
{
   return trans ? trans->num : NULL;
}

const char * 
xaccTransGetDescription (const Transaction *trans)
{
   return trans ? trans->description : NULL;
}

const char * 
xaccTransGetNotes (const Transaction *trans)
{
  return trans ? 
      kvp_frame_get_string (trans->inst.kvp_data, trans_notes_str) : NULL;
}

/********************************************************************\
\********************************************************************/

time_t
xaccTransGetDate (const Transaction *trans)
{
   return trans ? trans->date_posted.tv_sec : 0;
}

void
xaccTransGetDatePostedTS (const Transaction *trans, Timespec *ts)
{
   if (trans && ts)
       *ts = trans->date_posted;
}

void
xaccTransGetDateEnteredTS (const Transaction *trans, Timespec *ts)
{
   if (trans && ts)
       *ts = trans->date_entered;
}

Timespec
xaccTransRetDatePostedTS (const Transaction *trans)
{
   Timespec ts = {0,0};
   return trans ? trans->date_posted : ts;
}

Timespec
xaccTransRetDateEnteredTS (const Transaction *trans)
{
   Timespec ts = {0,0};
   return trans ? trans->date_entered : ts;
}

void
xaccTransGetDateDueTS (const Transaction *trans, Timespec *ts)
{
  KvpValue *value;

  if (!trans || !ts) return;

  value = kvp_frame_get_slot (trans->inst.kvp_data, TRANS_DATE_DUE_KVP);
  if (value)
    *ts = kvp_value_get_timespec (value);
  else
    xaccTransGetDatePostedTS (trans, ts);
}

Timespec
xaccTransRetDateDueTS (const Transaction *trans)
{
  Timespec ts = {0, 0};
  if (trans) xaccTransGetDateDueTS (trans, &ts);
  return ts;
}

char
xaccTransGetTxnType (const Transaction *trans)
{
  const char *s;
  if (!trans) return TXN_TYPE_NONE;
  s = kvp_frame_get_string (trans->inst.kvp_data, TRANS_TXN_TYPE_KVP);
  if (s) return *s;

  return TXN_TYPE_NONE;
}

const char * 
xaccTransGetReadOnly (const Transaction *trans)
{
  /* XXX This flag should be cached in the transaction structure
   * for performance reasons, since its checked every trans commit.
   */
  return trans ? kvp_frame_get_string (
      trans->inst.kvp_data, TRANS_READ_ONLY_REASON) : NULL;
}

gboolean
xaccTransHasReconciledSplitsByAccount (const Transaction *trans, 
                                       const Account *account)
{
  GList *node;

  for (node = xaccTransGetSplitList (trans); node; node = node->next)
  {
    Split *split = node->data;

    if (!xaccTransStillHasSplit(trans, split))
      continue;
    if (account && (xaccSplitGetAccount(split) != account))
      continue;

    switch (xaccSplitGetReconcile (split))
    {
      case YREC:
      case FREC:
        return TRUE;

      default:
        break;
    }
  }

  return FALSE;
}

gboolean
xaccTransHasReconciledSplits (const Transaction *trans)
{
  return xaccTransHasReconciledSplitsByAccount (trans, NULL);
}


gboolean
xaccTransHasSplitsInStateByAccount (const Transaction *trans,
                                    const char state,
                                    const Account *account)
{
  GList *node;

  for (node = xaccTransGetSplitList (trans); node; node = node->next)
  {
    Split *split = node->data;

    if (!xaccTransStillHasSplit(trans, split))
      continue;
    if (account && (xaccSplitGetAccount(split) != account))
      continue;

    if (split->reconciled == state)
      return TRUE;
  }

  return FALSE;
}

gboolean
xaccTransHasSplitsInState (const Transaction *trans, const char state)
{
  return xaccTransHasSplitsInStateByAccount (trans, state, NULL);
}


/********************************************************************\
\********************************************************************/


/* ====================================================================== */

static int
counter_thunk(Transaction *t, void *data)
{
    (*((guint*)data))++;
    return 0;
}

guint
gnc_book_count_transactions(QofBook *book)
{
    guint count = 0;
    xaccAccountTreeForEachTransaction(gnc_book_get_root_account(book),
                                counter_thunk, (void*)&count);
    return count;
}

/********************************************************************\
\********************************************************************/

void
xaccTransVoid(Transaction *trans, const char *reason)
{
  KvpFrame *frame;
  KvpValue *val;
  Timespec now;
  char iso8601_str[ISO_DATELENGTH+1] = "";

  g_return_if_fail(trans && reason);

  xaccTransBeginEdit(trans);
  frame = trans->inst.kvp_data;

  val = kvp_frame_get_slot(frame, trans_notes_str);
  kvp_frame_set_slot(frame, void_former_notes_str, val);

  kvp_frame_set_string(frame, trans_notes_str, _("Voided transaction"));
  kvp_frame_set_string(frame, void_reason_str, reason);

  now.tv_sec = time(NULL);
  now.tv_nsec = 0;
  gnc_timespec_to_iso8601_buff(now, iso8601_str);
  kvp_frame_set_string(frame, void_time_str, iso8601_str);

  FOR_EACH_SPLIT(trans, xaccSplitVoid(s));

  /* Dirtying taken care of by SetReadOnly */
  xaccTransSetReadOnly(trans, _("Transaction Voided"));
  xaccTransCommitEdit(trans);
}

gboolean 
xaccTransGetVoidStatus(const Transaction *trans)
{
  g_return_val_if_fail(trans, FALSE);
  return (kvp_frame_get_slot(trans->inst.kvp_data, void_reason_str) != NULL);
}

const char *
xaccTransGetVoidReason(const Transaction *trans)
{
  g_return_val_if_fail(trans, NULL);
  return kvp_frame_get_string(trans->inst.kvp_data, void_reason_str);
}

Timespec
xaccTransGetVoidTime(const Transaction *tr)
{
  const char *val;
  Timespec void_time = {0,0};

  g_return_val_if_fail(tr, void_time);

  val = kvp_frame_get_string(tr->inst.kvp_data, void_time_str);
  return val ? gnc_iso8601_to_timespec_gmt(val) : void_time;
}

void
xaccTransUnvoid (Transaction *trans)
{
  KvpFrame *frame;
  KvpValue *val;

  g_return_if_fail(trans);

  frame = trans->inst.kvp_data;
  val = kvp_frame_get_slot(frame, void_reason_str);
  if (!val) return; /* Transaction isn't voided. Bail. */

  xaccTransBeginEdit(trans);

  val = kvp_frame_get_slot(frame, void_former_notes_str);
  kvp_frame_set_slot(frame, trans_notes_str, val);
  kvp_frame_set_slot_nc(frame, void_former_notes_str, NULL);
  kvp_frame_set_slot_nc(frame, void_reason_str, NULL);
  kvp_frame_set_slot_nc(frame, void_time_str, NULL);

  FOR_EACH_SPLIT(trans, xaccSplitUnvoid(s));

  /* Dirtying taken care of by ClearReadOnly */
  xaccTransClearReadOnly(trans);
  xaccTransCommitEdit(trans);
}

Transaction *
xaccTransReverse (Transaction *orig)
{
  Transaction *trans;
  kvp_value *kvp_val;
  g_return_val_if_fail(orig, NULL);

  trans = xaccTransClone(orig);
  xaccTransBeginEdit(trans);

  /* Reverse the values on each split. Clear per-split info. */
  FOR_EACH_SPLIT(trans, {
          xaccSplitSetAmount(s, gnc_numeric_neg(xaccSplitGetAmount(s)));
          xaccSplitSetValue(s, gnc_numeric_neg(xaccSplitGetValue(s)));
          xaccSplitSetReconcile(s, NREC);
          qof_instance_set_dirty(QOF_INSTANCE(trans));
      });

  /* Now update the original with a pointer to the new one */
  kvp_val = kvp_value_new_guid(xaccTransGetGUID(trans));
  kvp_frame_set_slot_nc(orig->inst.kvp_data, TRANS_REVERSED_BY, kvp_val);

  xaccTransCommitEdit(trans);
  return trans;
}

Transaction *
xaccTransGetReversedBy(const Transaction *trans)
{
    GUID *guid;

    g_return_val_if_fail(trans, NULL);
    guid = kvp_frame_get_guid(trans->inst.kvp_data, TRANS_REVERSED_BY);
    return xaccTransLookup(guid, qof_instance_get_book(trans));
}

void
xaccTransScrubSplits (Transaction *trans)
{
    gnc_commodity *currency;

    if (!trans) return;

    xaccTransBeginEdit(trans);
    /* The split scrub expects the transaction to have a currency! */
    currency = xaccTransGetCurrency (trans);
    if (!currency)
        PERR ("Transaction doesn't have a currency!");

    FOR_EACH_SPLIT(trans, xaccSplitScrub(s));
    xaccTransCommitEdit(trans);
}

/* ============================================================== */
/** The xaccTransScrubGainsDate() routine is used to keep the posted date
 *    of gains splits in sync with the posted date of the transaction
 *    that caused the gains.
 *  
 *    The posted date is kept in sync using a lazy-evaluation scheme.
 *    If xaccTransactionSetDatePosted() is called, the date change is
 *    accepted, and the split is marked date-dirty.  If the posted date
 *    is queried for (using GetDatePosted()), then the transaction is
 *    evaluated. If it's a gains-transaction, then its date is copied 
 *    from the source transaction that created the gains.
 */

static void
xaccTransScrubGainsDate (Transaction *trans)
{
   SplitList *node;
   Timespec ts = {0,0};
//restart_search:
   for (node = trans->splits; node; node=node->next) {
      Split *s = node->data;

      if (!xaccTransStillHasSplit(trans, s)) continue;
      xaccSplitDetermineGainStatus(s);

      if ((GAINS_STATUS_GAINS & s->gains) && 
          s->gains_split &&
          ((s->gains_split->gains & GAINS_STATUS_DATE_DIRTY) ||
           (s->gains & GAINS_STATUS_DATE_DIRTY)))
      {
         Transaction *source_trans = s->gains_split->parent;
         ts = source_trans->date_posted;
         s->gains &= ~GAINS_STATUS_DATE_DIRTY;
         s->gains_split->gains &= ~GAINS_STATUS_DATE_DIRTY;

         xaccTransSetDatePostedTS(trans, &ts);
         FOR_EACH_SPLIT(trans, s->gains &= ~GAINS_STATUS_DATE_DIRTY);
         //goto restart_search;
      }
   }
}

/* ============================================================== */

void
xaccTransScrubGains (Transaction *trans, Account *gain_acc)
{
   SplitList *node;

   ENTER("(trans=%p)", trans);
   /* Lock down posted date, its to be synced to the posted date 
    * for the source of the cap gains. */
   xaccTransScrubGainsDate(trans);

   /* Fix up the split amount */
restart:
   for (node = trans->splits; node; node = node->next) {
      Split *s = node->data;

      if (!xaccTransStillHasSplit(trans, s)) continue;

      xaccSplitDetermineGainStatus(s);
      if (s->gains & GAINS_STATUS_ADIRTY) {
          gboolean altered = FALSE;
          s->gains &= ~GAINS_STATUS_ADIRTY;
          if (s->lot) 
              altered = xaccScrubLot(s->lot);
          else
              altered = xaccSplitAssign(s);
          if (altered) goto restart;
      }
   }

   /* Fix up gains split value */
   FOR_EACH_SPLIT(trans, 
                  if ((s->gains & GAINS_STATUS_VDIRTY) ||
                      (s->gains_split &&
                       (s->gains_split->gains & GAINS_STATUS_VDIRTY)))
                      xaccSplitComputeCapGains(s, gain_acc);
                  );
                      
   LEAVE("(trans=%p)", trans);
}

Split *
xaccTransFindSplitByAccount(const Transaction *trans, const Account *acc)
{
    if (!trans || !acc) return NULL;
    FOR_EACH_SPLIT(trans, if (xaccSplitGetAccount(s) == acc) return s);
    return NULL;
}

/********************************************************************\
\********************************************************************/
/* QofObject function implementation */

/* Hook into the QofObject registry */
static QofObject trans_object_def = {
  interface_version:   QOF_OBJECT_VERSION,
  e_type:              GNC_ID_TRANS,
  type_label:          "Transaction",
  create:              (gpointer)xaccMallocTransaction,
  book_begin:          NULL,
  book_end:            NULL,
  is_dirty:            qof_collection_is_dirty,
  mark_clean:          qof_collection_mark_clean,
  foreach:             qof_collection_foreach,
  printable:           (const char* (*)(gpointer)) xaccTransGetDescription,
  version_cmp:         (int (*)(gpointer,gpointer)) qof_instance_version_cmp,
};

static gboolean
trans_is_balanced_p (const Transaction *trans)
{
  return trans ? gnc_numeric_zero_p(xaccTransGetImbalance(trans)) : FALSE;
}

gboolean xaccTransRegister (void)
{
  static QofParam params[] = {
    { TRANS_NUM, QOF_TYPE_STRING, 
      (QofAccessFunc)xaccTransGetNum, 
      (QofSetterFunc)qofTransSetNum,
       qof_string_number_compare_func },
    { TRANS_DESCRIPTION, QOF_TYPE_STRING, 
      (QofAccessFunc)xaccTransGetDescription, 
      (QofSetterFunc)qofTransSetDescription },
    { TRANS_DATE_ENTERED, QOF_TYPE_DATE, 
      (QofAccessFunc)xaccTransRetDateEnteredTS, 
      (QofSetterFunc)qofTransSetDateEntered },
    { TRANS_DATE_POSTED, QOF_TYPE_DATE, 
      (QofAccessFunc)xaccTransRetDatePostedTS, 
      (QofSetterFunc)qofTransSetDatePosted },
    { TRANS_DATE_DUE, QOF_TYPE_DATE, 
      (QofAccessFunc)xaccTransRetDateDueTS, NULL },
    { TRANS_IMBALANCE, QOF_TYPE_NUMERIC, 
      (QofAccessFunc)xaccTransGetImbalance, NULL },
    { TRANS_NOTES, QOF_TYPE_STRING, 
      (QofAccessFunc)xaccTransGetNotes, 
      (QofSetterFunc)qofTransSetNotes },
    { TRANS_IS_BALANCED, QOF_TYPE_BOOLEAN, 
      (QofAccessFunc)trans_is_balanced_p, NULL },
    { TRANS_TYPE, QOF_TYPE_CHAR, 
      (QofAccessFunc)xaccTransGetTxnType, 
      (QofSetterFunc)xaccTransSetTxnType },
    { TRANS_VOID_STATUS, QOF_TYPE_BOOLEAN, 
      (QofAccessFunc)xaccTransGetVoidStatus, NULL },
    { TRANS_VOID_REASON, QOF_TYPE_STRING, 
      (QofAccessFunc)xaccTransGetVoidReason, NULL },
    { TRANS_VOID_TIME, QOF_TYPE_DATE,    
      (QofAccessFunc)xaccTransGetVoidTime, NULL },
    { TRANS_SPLITLIST, GNC_ID_SPLIT,     
      (QofAccessFunc)xaccTransGetSplitList, NULL },
    { TRANS_KVP, QOF_TYPE_KVP,     
      (QofAccessFunc)qof_instance_get_slots, NULL },
    { QOF_PARAM_BOOK, QOF_ID_BOOK,      
      (QofAccessFunc)qof_instance_get_book, NULL },
    { QOF_PARAM_GUID, QOF_TYPE_GUID,    
      (QofAccessFunc)qof_entity_get_guid, NULL },
    { NULL },
  };

  qof_class_register (GNC_ID_TRANS, (QofSortFunc)xaccTransOrder, params);

  return qof_object_register (&trans_object_def);
}

/************************ END OF ************************************\
\************************* FILE *************************************/
