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
#include "Group.h"
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

/*
 * Design notes on event-generation: transaction-modified-events 
 * should not be generated until transacation commit or rollback 
 * time.  They should not be generated as each field is tweaked. 
 * This for two reasons:
 * 1) Most editing events make multiple changes to a trnasaction,
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

#define ISO_DATELENGTH 32 /* length of an iso 8601 date string. */

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_ENGINE;

void check_open (const Transaction *trans)
{
  if (trans && 0 >= trans->inst.editlevel)
    PERR ("transaction %p not open for editing", trans);
}

/********************************************************************\
\********************************************************************/

G_INLINE_FUNC void mark_trans (Transaction *trans);
void mark_trans (Transaction *trans)
{
  GList *node;

  for (node = trans->splits; node; node = node->next)
  {
    mark_split (node->data);
  }
}

G_INLINE_FUNC void gen_event_trans (Transaction *trans);
void gen_event_trans (Transaction *trans)
{
  GList *node;

  for (node = trans->splits; node; node = node->next)
  {
    Split *s = node->data;
    Account *account = s->acc;
    GNCLot *lot = s->lot;
    if (account)
    {
      xaccGroupMarkNotSaved (account->parent);
      gnc_engine_gen_event (&account->inst.entity, GNC_EVENT_MODIFY);
    }
    if (lot)
    {
      /* A change of transaction date might affect opening date of lot */
      gnc_engine_gen_event (&lot->inst.entity, GNC_EVENT_MODIFY);
    }
  }

  gnc_engine_gen_event (&trans->inst.entity, GNC_EVENT_MODIFY);
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
  trans->num         = gnc_string_cache_insert("");
  trans->description = gnc_string_cache_insert("");

  trans->common_currency = NULL;
  trans->splits = NULL;

  trans->date_entered.tv_sec  = 0;
  trans->date_entered.tv_nsec = 0;

  trans->date_posted.tv_sec  = 0;
  trans->date_posted.tv_nsec = 0;

  trans->version = 0;
  trans->version_check = 0;
  trans->marker = 0;
  trans->orig = NULL;

  trans->idata = 0;
  qof_instance_init (&trans->inst, GNC_ID_TRANS, book);
  LEAVE (" ");
}

/********************************************************************\
\********************************************************************/

Transaction *
xaccMallocTransaction (QofBook *book)
{
  Transaction *trans;

  g_return_val_if_fail (book, NULL);

  trans = g_new(Transaction, 1);
  xaccInitTransaction (trans, book);
  gnc_engine_gen_event (&trans->inst.entity, GNC_EVENT_CREATE);

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
  printf("    version:     %x\n", trans->version);
  printf("    version_chk: %x\n", trans->version_check);
  printf("    editlevel:   %x\n", trans->editlevel);
  printf("    orig:        %p\n", trans->orig);
  printf("    idata:       %x\n", trans->idata);
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

  trans = g_new0 (Transaction, 1);

  trans->num         = gnc_string_cache_insert (t->num);
  trans->description = gnc_string_cache_insert (t->description);

  trans->splits = g_list_copy (t->splits);
  for (node = trans->splits; node; node = node->next)
  {
    node->data = xaccDupeSplit (node->data);
  }

  trans->date_entered = t->date_entered;
  trans->date_posted = t->date_posted;
  trans->version = t->version;
  trans->orig = NULL;

  trans->common_currency = t->common_currency;

  /* Trash the guid and entity table. We don't want to mistake 
   * the cloned transaction as something official.  If we ever 
   * use this transaction, we'll have to fix this up.
   */
  trans->inst.entity.e_type = NULL;
  trans->inst.entity.guid = *guid_null();
  trans->inst.entity.collection = NULL;
  trans->inst.book = t->inst.book;
  trans->inst.editlevel = 0;
  trans->inst.do_free = FALSE;
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

  gnc_engine_suspend_events();
  trans = g_new0 (Transaction, 1);

  trans->date_entered    = t->date_entered;
  trans->date_posted     = t->date_posted;
  trans->num             = gnc_string_cache_insert (t->num);
  trans->description     = gnc_string_cache_insert (t->description);
  trans->common_currency = t->common_currency;
  trans->version         = t->version;
  trans->version_check   = t->version_check;

  trans->orig            = NULL;
  trans->idata           = 0;

  qof_instance_init (&trans->inst, GNC_ID_TRANS, t->inst.book);
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
  gnc_engine_resume_events();

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
  gnc_string_cache_remove(trans->num);
  gnc_string_cache_remove(trans->description);

  /* Just in case someone looks up freed memory ... */
  trans->num         = (char *) 1;
  trans->description = NULL;

  trans->date_entered.tv_sec = 0;
  trans->date_entered.tv_nsec = 0;
  trans->date_posted.tv_sec = 0;
  trans->date_posted.tv_nsec = 0;
  trans->version = 0;

  if (trans->orig)
  {
    xaccFreeTransaction (trans->orig);
    trans->orig = NULL;
  }

  qof_instance_release (&trans->inst);
  g_free(trans);

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
    if (!guid_equal(&(ta->inst.entity.guid), &(tb->inst.entity.guid)))
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
  GList *node;
  gnc_numeric imbal = gnc_numeric_zero();
  if (!trans) return imbal;

  ENTER("(trans=%p)", trans);
  /* Could use xaccSplitsComputeValue, except that we want to use
     GNC_HOW_DENOM_EXACT */
  for (node = trans->splits; node; node = node->next)
  {
    Split *s = node->data;
    imbal = gnc_numeric_add(imbal, xaccSplitGetValue(s),
                              GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT);
  }
  LEAVE("(trans=%p) imbal=%s", trans, gnc_num_dbg_to_string(imbal));
  return imbal;
}

gnc_numeric
xaccTransGetAccountValue (const Transaction *trans, 
                          const Account *account)
{
  gnc_numeric total = gnc_numeric_zero ();
  GList *splits;

  if (!trans || !account) return total;

  for (splits = trans->splits; splits; splits = splits->next)
  {
    Split *s = splits->data;
    Account *a = xaccSplitGetAccount (s);
    if (a == account)
      total = gnc_numeric_add (total, xaccSplitGetValue (s),
                               GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT);
  }
  return total;
}

gnc_numeric
xaccTransGetAccountAmount (const Transaction *trans, const Account *account)
{
  gnc_numeric total = gnc_numeric_zero ();
  GList *splits;

  if (!trans || !account)
    return total;

  total = gnc_numeric_convert (total, xaccAccountGetCommoditySCU (account),
                               GNC_RND_ROUND);

  for (splits = xaccTransGetSplitList (trans); splits; splits = splits->next) {
      Split *s = splits->data;
      Account *a = xaccSplitGetAccount (s);
      if (a == account)
          total = gnc_numeric_add_fixed (total, xaccSplitGetAmount (s));
  }
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

  splits = xaccTransGetSplitList(txn);
  for (; splits; splits = splits->next) {
    s = splits->data;

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
    /* If we did find a matching account but it's amount was zero,
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

  for (node = xaccTransGetSplitList(trans); node; node = node->next)
  {
    Split *split = node->data;

    if (xaccSplitGetAccount(split) != account)
      continue;

    if (!last_split)
    {
      last_split = split;
      continue;
    }

    /* This test needs to correspond to the comparison function used when
       sorting the splits for computing the running balance. */
    if (xaccSplitDateOrder (last_split, split) < 0)
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
  GList *splits;
  gint fraction, old_fraction;

  if (!trans || !curr || trans->common_currency == curr) return;
  qof_begin_edit(QOF_INSTANCE(trans));

  old_fraction = gnc_commodity_get_fraction (trans->common_currency);
  trans->common_currency = curr;
  fraction = gnc_commodity_get_fraction (curr);

  /* avoid needless crud if fraction didn't change */
  if (fraction != old_fraction)
  {
    for (splits = trans->splits; splits; splits = splits->next)
    {
      Split *s = splits->data;
      s->value = gnc_numeric_convert(xaccSplitGetValue(s), 
                                     fraction, GNC_HOW_RND_ROUND);
      SET_GAINS_VDIRTY(s);
    }
  }

  qof_instance_set_dirty(QOF_INSTANCE(trans));
  mark_trans(trans);  /* Dirty balance of every account in trans */
  qof_commit_edit(QOF_INSTANCE(trans));
}

/********************************************************************\
\********************************************************************/

void
xaccTransBeginEdit (Transaction *trans)
{
   if (!trans) return;
   if (!qof_begin_edit(&trans->inst)) return;

   if (qof_book_shutting_down(trans->inst.book)) return;

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
      qof_book_shutting_down(trans->inst.book)) {
      xaccTransBeginEdit(trans);
      trans->inst.do_free = TRUE;
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
    if (GAINS_STATUS_UNKNOWN == s->gains) xaccSplitDetermineGainStatus(s);
    if (s->gains_split && (GAINS_STATUS_GAINS & s->gains_split->gains))
    {
      Transaction *t = s->gains_split->parent;
      xaccTransBeginEdit (t);
      xaccTransDestroy (t);
      xaccTransCommitEdit (t);
      s->gains_split = NULL;
    }
  }
}

static void
do_destroy (Transaction *trans)
{
  SplitList *node;
  gboolean shutting_down;

  /* If there are capital-gains transactions associated with this, 
   * they need to be destroyed too.  */
  destroy_gains (trans);

  shutting_down = qof_book_shutting_down(trans->inst.book);

  /* Make a log in the journal before destruction.  */
  if (!shutting_down)
    xaccTransWriteLog (trans, 'D');

  gnc_engine_gen_event (&trans->inst.entity, GNC_EVENT_DESTROY);

  for (node = trans->splits; node; node = node->next)
  {
    Split *split = node->data;

    mark_split (split);
    xaccAccountRemoveSplit (split->acc, split);
    if (!shutting_down)
      xaccAccountRecomputeBalance (split->acc);
    gen_event (split);
    xaccFreeSplit (split);

    node->data = NULL;
  }

  g_list_free (trans->splits);
  trans->splits = NULL;

  /* The actual free is done with the commit call, else its rolled back */
}

/********************************************************************\
\********************************************************************/

/* Temporary hack for data consitency */
static int scrub_data = 1;
void xaccEnableDataScrubbing(void) { scrub_data = 1; }
void xaccDisableDataScrubbing(void) { scrub_data = 0; }


void
xaccTransCommitEdit (Transaction *trans)
{
   QofBackend *be;

   if (!trans) return;

   QOF_COMMIT_EDIT_PART1 (&trans->inst);

   /* We increment this for the duration of the call
    * so other functions don't result in a recursive
    * call to xaccTransCommitEdit. */
   trans->inst.editlevel++;
   /* Before commiting the transaction, we're gonna enforce certain
    * constraints.  In particular, we want to enforce the cap-gains
    * and the balanced lot constraints.  These constraints might 
    * change the number of splits in this transaction, and the 
    * transaction itself might be deleted.  This is also why
    * we can't really enforce these constraints elsewhere: they
    * can cause pointers to splits and transactions to disapear out
    * from under the holder.
    */
   if (trans->splits && !(trans->inst.do_free) && scrub_data)
   {
     /* The total value of the transaction should sum to zero. 
      * Call the trans scrub routine to fix it.   Indirectly, this 
      * routine also performs a number of other transaction fixes too.
      */
     xaccTransScrubImbalance (trans, NULL, NULL);
     /* Get the cap gains into a consistent state as well. */
     xaccTransScrubGains (trans, NULL);
   }

   /* Record the time of last modification */
   if (0 == trans->date_entered.tv_sec) 
   {
      struct timeval tv;
      gettimeofday (&tv, NULL);
      trans->date_entered.tv_sec = tv.tv_sec;
      trans->date_entered.tv_nsec = 1000 * tv.tv_usec;
   }

   /* Sort the splits. Why do we need to do this ?? */
   /* Good question.  Who knows?  */
   xaccTransSortSplits(trans);
   if (!trans->splits) trans->inst.do_free = TRUE;

   /* XXX the code below is almost identical to 
    * QOF_COMMIT_EDIT_PART1 (&trans->inst);
    * except for the rollback bits */
   /* See if there's a backend.  If there is, invoke it. */
   be = qof_book_get_backend (trans->inst.book);
   if (be && be->commit) 
   {
      QofBackendError errcode;

      /* clear errors */
      do {
        errcode = qof_backend_get_error (be);
      } while (ERR_BACKEND_NO_ERR != errcode);

      qof_backend_run_commit(be, &(trans->inst));

      errcode = qof_backend_get_error (be);
      if (ERR_BACKEND_NO_ERR != errcode)
      {
         /* If the backend puked, then we must roll-back 
          * at this point, and let the user know that we failed.
          * The GUI should check for error conditions ... 
          */
        if (ERR_BACKEND_MODIFIED == errcode)
        {
           PWARN("Another user has modified this transaction\n"
                 "\tjust a moment ago. Please look at their changes,\n"
                 "\tand try again, if needed.\n");
        }

        /* push error back onto the stack */
        qof_backend_set_error (be, errcode);

        xaccTransRollbackEdit (trans);
        return;
      }
   }

   if (trans->inst.do_free)
   {
      PINFO ("delete trans at addr=%p", trans);
      do_destroy (trans);
      xaccFreeTransaction (trans);
      return;
   }

   /* ------------------------------------------------- */
   /* Make sure all associated splits are in proper order
    * in their accounts with the correct balances. */
   /* FIXME: That's not exactly what this call does, and I think it
      may be unneeded. Maybe all that's needed is to call
      xaccAccountBringUpToDate for each account. OTOH, maybe that will
      be taken care of when the Account is commited, if needed. */
   xaccTransFixSplitDateOrder (trans);

   xaccTransWriteLog (trans, 'C');

   /* Get rid of the copy we made. We won't be rolling back, 
    * so we don't need it any more.  */
   PINFO ("get rid of rollback trans=%p", trans->orig);
   xaccFreeTransaction (trans->orig);
   trans->orig = NULL;

   /* Put back to zero. */
   trans->inst.editlevel--;

   gen_event_trans (trans);
   LEAVE ("(trans=%p)", trans);
}

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
   QofBackend *be;
   Transaction *orig;
   int force_it = 0, mismatch = 0;
   int i;
   ENTER ("trans addr=%p\n", trans);

   QOF_COMMIT_EDIT_PART1(&trans->inst);

   /* We increment this for the duration of the call
    * so other functions don't result in a recursive
    * call to xaccTransCommitEdit. */
   trans->inst.editlevel++;

   /* copy the original values back in. */
   orig = trans->orig;

   /* TODO: Perhaps this could be simplified to use do_destroy */
   trans->common_currency = orig->common_currency;

   gnc_string_cache_remove (trans->num);
   trans->num = orig->num;
   orig->num = gnc_string_cache_insert("");

   gnc_string_cache_remove (trans->description);
   trans->description = orig->description;
   orig->description = gnc_string_cache_insert("");

   kvp_frame_delete (trans->inst.kvp_data);
   trans->inst.kvp_data = orig->inst.kvp_data;
   if (!trans->inst.kvp_data)
     trans->inst.kvp_data = kvp_frame_new ();
   orig->inst.kvp_data = kvp_frame_new ();

   trans->date_entered = orig->date_entered;
   trans->date_posted = orig->date_posted;

   /* OK, we also have to restore the state of the splits.  Of course,
    * we could brute-force our way through this, and just clobber all of the
    * old splits, and insert all of the new splits, but this kind of brute
    * forcing will suck memory cycles.  So instead we'll try the gentle 
    * approach first.  Note that even in the gentle approach, the 
    * CheckDateOrder routine could be cpu-cycle brutal, so it maybe 
    * it could use some tuning.
    */
   if (trans->inst.do_free)
   {
      force_it = 1;
      mismatch = 0;
   }
   else 
   {
      GList *node;
      GList *node_orig;
      Split *s, *so;

      s = so = NULL;

      for (i = 0, node = trans->splits, node_orig = orig->splits ;
           node && node_orig ;
           i++, node = node->next, node_orig = node_orig->next)
      {
         s = node->data;
         so = node_orig->data;

         if (so->acc != s->acc)
         {
           force_it = 1;
           mismatch = i;
           break;
         }

         gnc_string_cache_remove (s->action);
         s->action = so->action;
         so->action = gnc_string_cache_insert("");

         gnc_string_cache_remove (s->memo);
         s->memo = so->memo;
         so->memo = gnc_string_cache_insert("");

         kvp_frame_delete (s->inst.kvp_data);
         s->inst.kvp_data = so->inst.kvp_data;
         if (!s->inst.kvp_data)
           s->inst.kvp_data = kvp_frame_new ();
         so->inst.kvp_data = kvp_frame_new ();

         s->reconciled  = so->reconciled;
         s->amount      = so->amount;
         s->value       = so->value;
         SET_GAINS_A_VDIRTY(s);

         s->date_reconciled = so->date_reconciled;

         /* do NOT check date order until all of the other fields 
          * have been properly restored */
         mark_split (s);
         xaccAccountFixSplitDateOrder (s->acc, s); 
         xaccAccountRecomputeBalance (s->acc);
         gen_event (s);
      }

      /* if the number of splits were not identical... then force */
      if (node || node_orig)
      {
        force_it = 1;
        mismatch = i;
      }
   }

   /* OK, if force_it got set, we'll have to tough it out and brute-force
    * the rest of the way.  Clobber all the edited splits, add all new splits.
    * Unfortunately, this can suck up CPU cycles in the Remove/Insert routines.
    */  
   if (force_it)
   {
      GList *node;

      /* In this loop, we tuck the fixed-up splits back into 
       * orig array, for temp safekeeping. */
      for (i = 0, node = trans->splits ;
           node && i < mismatch ;
           i++, node = node->next)
      {
         Split *s = node->data;
         GList *node_orig;

         node_orig = g_list_nth (orig->splits, i);
         xaccFreeSplit (node_orig->data);
         node_orig->data = s;
      }

      /* In this loop, we remove excess new splits that had been added */
      for (node = g_list_nth (trans->splits, mismatch) ;
           node ; node = node->next)
      {
         Split *s = node->data;
         Account *acc = s->acc;

         mark_split (s);
         xaccAccountRemoveSplit (acc, s);
         xaccAccountRecomputeBalance (acc);
         gen_event (s);
         xaccFreeSplit (s);
      }

      g_list_free (trans->splits);

      trans->splits = orig->splits;
      orig->splits = NULL;

      /* In this loop, we fix up the remaining orig splits to be healthy */
      for (node = g_list_nth (trans->splits, mismatch) ;
           node ; node = node->next)
      {
         Split *s = node->data;
         Account *account = s->acc;

         s->parent = trans;
         s->acc = NULL;
         xaccAccountInsertSplit (account, s);
         mark_split (s);
         xaccAccountRecomputeBalance (account);
         gen_event (s);
      }
   }

   /* Now that the engine copy is back to its original version,
    * get the backend to fix it in the database */
   be = qof_book_get_backend (trans->inst.book);
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
         xaccFreeTransaction (trans);

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
   trans->inst.do_free = FALSE;

   /* Put back to zero. */
   trans->inst.editlevel--;

   LEAVE ("trans addr=%p\n", trans);
}

gboolean
xaccTransIsOpen (const Transaction *trans)
{
  return trans ? (0 < trans->inst.editlevel) : FALSE;
}

/* Only used by postgres backend. Not sure if it should dirty the trans. */
void
xaccTransSetVersion (Transaction *trans, gint32 vers)
{
  if (trans) 
      trans->version = vers;
}

gint32
xaccTransGetVersion (const Transaction *trans)
{
  return trans ? trans->version : 0;
}

/********************************************************************\
 * TransRemoveSplit is an engine private function and does not/should
 * not cause any rebalancing to occur.
\********************************************************************/

void
xaccTransRemoveSplit (Transaction *trans, const Split *split) 
{
    if (trans) {
        trans->splits = g_list_remove (trans->splits, split);
        qof_instance_set_dirty(QOF_INSTANCE(trans));
    }
}

/********************************************************************\
\********************************************************************/

void
xaccTransAppendSplit (Transaction *trans, Split *split) 
{
   if (!trans || !split) return;
   g_return_if_fail (trans->inst.book == split->inst.book);

   qof_begin_edit(QOF_INSTANCE(trans));

   /* First, make sure that the split isn't already inserted 
    * elsewhere. If so, then remove it. */
   if (split->parent)
      xaccTransRemoveSplit (split->parent, split);

   /* Now, insert the split into the array */
   split->parent = trans;
   trans->splits = g_list_append (trans->splits, split);
   qof_instance_set_dirty(QOF_INSTANCE(trans));

   /* Convert the split to the new transaction's commodity denominator */
   /* If the denominator can't be exactly converted, it's an error */
   /* Actually, I _think_ the only error that can result with
      RND_ROUND is overflow.  If we really want it exact we should use
      something else. */
   if (trans->common_currency)
   {
     int fraction = gnc_commodity_get_fraction (trans->common_currency);
     gnc_numeric new_value;

     new_value = gnc_numeric_convert(xaccSplitGetValue(split), 
                        fraction, GNC_HOW_RND_ROUND);

     if (gnc_numeric_check (new_value) == GNC_ERROR_OK)
       split->value = new_value;
     SET_GAINS_VDIRTY(split);  /* Is this right? Dirty gains even if error? */
     mark_split(split);
   }
   qof_commit_edit(QOF_INSTANCE(trans));
}

int
xaccTransOrder (const Transaction *ta, const Transaction *tb)
{
  char *da, *db;
  int na, nb;

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
  da = ta->description;
  db = tb->description;
  SAFE_STRCMP (da, db);

  /* else, sort on guid - keeps sort stable. */
  return guid_compare(&(ta->inst.entity.guid), &(tb->inst.entity.guid));
}

/********************************************************************\
\********************************************************************/

static inline void
xaccTransSetDateInternal(Transaction *trans, Timespec *dadate, Timespec val)
{
    qof_begin_edit(QOF_INSTANCE(trans));

    PINFO ("addr=%p set date to %llu.%09ld %s",
           trans, val.tv_sec, val.tv_nsec, 
           ctime (({time_t secs = (time_t) val.tv_sec; &secs;})));
    
    *dadate = val;
    qof_instance_set_dirty(QOF_INSTANCE(trans));
    mark_trans(trans);
    qof_commit_edit(QOF_INSTANCE(trans));

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
   SplitList *node;
   for (node = trans->splits; node; node = node->next)
   {
      Split *s = node->data;
      s->gains |= GAINS_STATUS_DATE_DIRTY;
   }
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
   qof_begin_edit(QOF_INSTANCE(trans));
   kvp_frame_set_timespec (trans->inst.kvp_data, TRANS_DATE_DUE_KVP, *ts);
   qof_instance_set_dirty(QOF_INSTANCE(trans));
   qof_commit_edit(QOF_INSTANCE(trans));
}

void
xaccTransSetTxnType (Transaction *trans, char type)
{
  char s[2] = {type, '\0'};
  g_return_if_fail(trans);
  qof_begin_edit(&trans->inst);
  kvp_frame_set_str (trans->inst.kvp_data, TRANS_TXN_TYPE_KVP, s);
  qof_instance_set_dirty(QOF_INSTANCE(trans));
  qof_commit_edit(&trans->inst);
}

void xaccTransClearReadOnly (Transaction *trans)
{
    if (trans) {
        qof_begin_edit(QOF_INSTANCE(trans));
        kvp_frame_set_slot_path (trans->inst.kvp_data, NULL, 
                                 TRANS_READ_ONLY_REASON, NULL);
        qof_instance_set_dirty(QOF_INSTANCE(trans));
        qof_commit_edit(QOF_INSTANCE(trans));
    }
}

void
xaccTransSetReadOnly (Transaction *trans, const char *reason)
{
    if (trans && reason) {
        qof_begin_edit(QOF_INSTANCE(trans));
        kvp_frame_set_str (trans->inst.kvp_data, 
                           TRANS_READ_ONLY_REASON, reason);
        qof_instance_set_dirty(QOF_INSTANCE(trans));
        qof_commit_edit(QOF_INSTANCE(trans));
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
   qof_begin_edit(QOF_INSTANCE(trans));

   CACHE_REPLACE(trans->num, xnum);
   qof_instance_set_dirty(QOF_INSTANCE(trans));
   qof_commit_edit(QOF_INSTANCE(trans));
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
   qof_begin_edit(QOF_INSTANCE(trans));

   CACHE_REPLACE(trans->description, desc);
   qof_instance_set_dirty(QOF_INSTANCE(trans));
   qof_commit_edit(QOF_INSTANCE(trans));
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
  qof_begin_edit(QOF_INSTANCE(trans));

  kvp_frame_set_str (trans->inst.kvp_data, trans_notes_str, notes);
  qof_instance_set_dirty(QOF_INSTANCE(trans));
  qof_commit_edit(QOF_INSTANCE(trans));
}

/********************************************************************\
\********************************************************************/

Split *
xaccTransGetSplit (const Transaction *trans, int i) 
{
   if (!trans || i < 0) return NULL;

   return g_list_nth_data (trans->splits, i);
}

SplitList *
xaccTransGetSplitList (const Transaction *trans)
{
  return trans ? trans->splits : NULL;
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

int
xaccTransCountSplits (const Transaction *trans)
{
   return trans ? g_list_length (trans->splits) : 0;
}

gboolean
xaccTransHasReconciledSplitsByAccount (const Transaction *trans, 
                                       const Account *account)
{
  GList *node;

  for (node = xaccTransGetSplitList (trans); node; node = node->next)
  {
    Split *split = node->data;

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

    if (account && (split->acc != account))
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
    xaccGroupForEachTransaction(xaccGetAccountGroup(book),
                                counter_thunk, (void*)&count);
    return count;
}

/********************************************************************\
\********************************************************************/
/* walk through the splits, looking for any account */
static Account * 
get_any_account(const Transaction *trans)
{
    GList *node;
    if (!trans) return NULL;
    for (node = trans->splits; node; node = node->next)
        if (((Split *)node->data)->acc)
            return ((Split *)node->data)->acc;
    return NULL;
}
Account *
xaccGetAccountByName (const Transaction *trans, const char * name)
{
   Account *acc;
   if (!trans || !name) return NULL;

   acc = get_any_account(trans);
   return acc ? xaccGetPeerAccountFromName (acc, name) : NULL;
}

/********************************************************************\
\********************************************************************/

Account *
xaccGetAccountByFullName (const Transaction *trans, const char * name,
                          const char separator)
{
   Account *acc;
   if (!trans || !name) return NULL;

   acc = get_any_account(trans);
   return acc ? xaccGetPeerAccountFromFullName (acc, name, separator) : NULL;
}

/********************************************************************\
\********************************************************************/

void
xaccTransVoid(Transaction *trans, const char *reason)
{
  KvpFrame *frame;
  KvpValue *val;
  GList *split_list;
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

  for (split_list = trans->splits; split_list; split_list = split_list->next)
  {
      Split * split = split_list->data;
      xaccSplitVoid(split);
  }

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

char *
xaccTransGetVoidReason(const Transaction *trans)
{
  g_return_val_if_fail(trans, NULL);
  return kvp_frame_get_string(trans->inst.kvp_data, void_reason_str);
}

Timespec
xaccTransGetVoidTime(const Transaction *tr)
{
  char *val;
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
  GList *split_list;

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

  for (split_list = trans->splits; split_list; split_list = split_list->next)
  {
      Split *split = split_list->data;
      xaccSplitUnvoid(split);
  }

  /* Dirtying taken care of by ClearReadOnly */
  xaccTransClearReadOnly(trans);
  xaccTransCommitEdit(trans);
}

void
xaccTransReverse (Transaction *trans)
{
  GList *split_list;
  Split *split;

  g_return_if_fail(trans);

  xaccTransBeginEdit(trans);

  /* Reverse the values on each split. Clear per-split info. */
  for (split_list = trans->splits; split_list; split_list = split_list->next)
  {
    split = split_list->data;
    split->amount = gnc_numeric_neg(xaccSplitGetAmount(split));
    split->value = gnc_numeric_neg(xaccSplitGetValue(split));
    SET_GAINS_A_VDIRTY(split);
    split->reconciled = NREC;
    xaccSplitSetDateReconciledSecs (split, 0);
    mark_split(split);
  }

  if (trans->splits)
      qof_instance_set_dirty(QOF_INSTANCE(trans));
  xaccTransCommitEdit(trans);
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
      (QofSetterFunc)qofTransSetNum },
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
