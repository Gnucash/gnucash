/********************************************************************\
 * Transaction.c -- transaction & split implementation              *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1997-2000 Linas Vepstas <linas@linas.org>          *
 * Copyright (C) 2000 Bill Gribble <grib@billgribble.com>           *
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

#include <string.h>
#include <sys/time.h>
#include <time.h>
#include <unistd.h>

#include "AccountP.h"
#include "BackendP.h"
#include "GNCIdP.h"
#include "Group.h"
#include "Scrub.h"
#include "TransactionP.h"
#include "TransLog.h"
#include "date.h"
#include "gnc-commodity.h"
#include "gnc-engine-util.h"
#include "gnc-engine.h"
#include "gnc-event-p.h"
#include "gnc-session-p.h"
#include "messages.h"


/* 
 * The "force_double_entry" flag determines how 
 * the splits in a transaction will be balanced. 
 *
 * The following values have significance:
 * 0 -- anything goes
 * 1 -- The sum of all splits in a transaction will be
 *      forced to be zero, even if this requires the
 *      creation of additional splits.  Note that a split
 *      whose value is zero (e.g. a stock price) can exist
 *      by itself. Otherwise, all splits must come in at 
 *      least pairs.
 * 2 -- splits without parents will be forced into a
 *      lost & found account.  (Not implemented)
 */
int force_double_entry = 0;

const char *void_reason_str = "void-reason";
const char *void_time_str = "void-time";
const char *void_former_amt_str = "void-former-amount";
const char *void_former_val_str = "void-former-value";

#define PRICE_SIGFIGS 6

#define ISO_DATELENGTH 30 /* length of an iso 8601 date string.
			   * not sure, can't be bothered counting :) */

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_ENGINE;


G_INLINE_FUNC void check_open (Transaction *trans);
G_INLINE_FUNC void
check_open (Transaction *trans)
{
  if (trans && 0 >= trans->editlevel)
  {
    PERR ("transaction %p not open for editing\n", trans);
    PERR ("\t%s:%d \n", __FILE__, __LINE__);
  }
}

/********************************************************************\
 * xaccInitSplit
 * Initialize a Split structure
\********************************************************************/

static void
xaccInitSplit(Split * split, GNCEntityTable *entity_table)
{
  /* fill in some sane defaults */
  xaccSplitSetAccount(split, NULL);
  split->parent      = NULL;

  split->action      = g_cache_insert(gnc_engine_get_string_cache(), "");
  split->memo        = g_cache_insert(gnc_engine_get_string_cache(), "");
  split->reconciled  = NREC;
  split->amount      = gnc_numeric_zero();
  split->value       = gnc_numeric_zero();

  split->date_reconciled.tv_sec  = 0;
  split->date_reconciled.tv_nsec = 0;

  split->balance             = gnc_numeric_zero();
  split->cleared_balance     = gnc_numeric_zero();
  split->reconciled_balance  = gnc_numeric_zero();

  split->kvp_data = kvp_frame_new();
  split->idata = 0;

  split->entity_table = entity_table;

  xaccGUIDNewEntityTable (&split->guid, split->entity_table);
  xaccStoreEntity(split->entity_table, split, &split->guid, GNC_ID_SPLIT);
}

/********************************************************************\
\********************************************************************/

static Split *
xaccMallocSplitEntityTable (GNCEntityTable *entity_table)
{
  Split *split;

  g_return_val_if_fail (entity_table, NULL);

  split = g_new (Split, 1);
  xaccInitSplit (split, entity_table);

  return split;
}

Split *
xaccMallocSplit(GNCSession *session)
{
  g_return_val_if_fail (session, NULL);
  return xaccMallocSplitEntityTable (gnc_session_get_entity_table (session));
}

/********************************************************************\
\********************************************************************/
/* This routine is not exposed externally, since it does weird things, 
 * like not really setting up the parent account correctly, and ditto 
 * the parent transaction.  This routine is prone to programmer error
 * if not used correctly.  It is used only by the edit-rollback code.
 */

static Split *
xaccCloneSplit (Split *s)
{
  Split *split = g_new0 (Split, 1);

  /* copy(!) the guid and entity table. The cloned split is *not* unique,
   * is a sick twisted clone that holds 'undo' information. */
  split->guid = s->guid;
  split->entity_table = s->entity_table;

  xaccSplitSetAccountGUID(split, s->acc_guid);
  split->parent = s->parent;

  split->memo = g_cache_insert (gnc_engine_get_string_cache(), s->memo);
  split->action = g_cache_insert (gnc_engine_get_string_cache(), s->action);

  split->kvp_data = kvp_frame_copy (s->kvp_data);

  split->reconciled = s->reconciled;
  split->date_reconciled = s->date_reconciled;

  split->value = s->value;
  split->amount = s->amount;

  /* no need to futz with the balances;  these get wiped each time ... 
   * split->balance             = s->balance;
   * split->cleared_balance     = s->cleared_balance;
   * split->reconciled_balance  = s->reconciled_balance;
   */

  return split;
}

/********************************************************************\
\********************************************************************/

void
xaccFreeSplit (Split *split)
{
  if (!split) return;

  kvp_frame_delete (split->kvp_data);

  g_cache_remove(gnc_engine_get_string_cache(), split->memo);
  g_cache_remove(gnc_engine_get_string_cache(), split->action);

  /* just in case someone looks up freed memory ... */
  split->memo        = NULL;
  split->action      = NULL;
  split->kvp_data    = NULL;
  split->reconciled  = NREC;
  split->amount      = gnc_numeric_zero();
  split->value       = gnc_numeric_zero();
  split->parent      = NULL;
  xaccSplitSetAccount(split, NULL);
  
  split->date_reconciled.tv_sec = 0;
  split->date_reconciled.tv_nsec = 0;

  g_free(split);
}

/********************************************************************
 * xaccSplitEqual
 ********************************************************************/
gboolean
xaccSplitEqual(const Split *sa, const Split *sb,
               gboolean check_guids,
               gboolean check_txn_splits)
{
  if (!sa && !sb) return TRUE;

  if (!sa || !sb)
  {
    PWARN ("one is NULL");
    return FALSE;
  }

  if(check_guids) {
    if(!guid_equal(&(sa->guid), &(sb->guid)))
    {
      PWARN ("GUIDs differ");
      return FALSE;
    }
  }

  /* Since these strings are cached we can just use pointer equality */
  if (sa->memo != sb->memo)
  {
    PWARN ("memos differ: %s vs %s", sa->memo, sb->memo);
    return FALSE;
  }

  if (sa->action != sb->action)
  {
    PWARN ("actions differ: %s vs %s", sa->action, sb->action);
    return FALSE;
  }

  if (kvp_frame_compare(sa->kvp_data, sb->kvp_data) != 0)
  {
    char *frame_a;
    char *frame_b;

    frame_a = kvp_frame_to_string (sa->kvp_data);
    frame_b = kvp_frame_to_string (sb->kvp_data);

    PWARN ("kvp frames differ:\n%s\n\nvs\n\n%s", frame_a, frame_b);

    g_free (frame_a);
    g_free (frame_b);

    return FALSE;
  }

  if (sa->reconciled != sb->reconciled)
  {
    PWARN ("reconcile flags differ: %c vs %c", sa->reconciled, sb->reconciled);
    return FALSE;
  }

  if (timespec_cmp(&(sa->date_reconciled),
                   &(sb->date_reconciled)))
  {
    PWARN ("reconciled date differs");
    return FALSE;
  }

  if (!gnc_numeric_eq(sa->amount, sb->amount))
  {
    char *str_a;
    char *str_b;

    str_a = gnc_numeric_to_string (sa->amount);
    str_b = gnc_numeric_to_string (sb->amount);

    PWARN ("amounts differ: %s vs %s", str_a, str_b);

    g_free (str_a);
    g_free (str_b);

    return FALSE;
  }

  if (!gnc_numeric_eq(sa->value, sb->value))
  {
    char *str_a;
    char *str_b;

    str_a = gnc_numeric_to_string (sa->amount);
    str_b = gnc_numeric_to_string (sb->amount);

    PWARN ("values differ: %s vs %s", str_a, str_b);

    g_free (str_a);
    g_free (str_b);

    return FALSE;
  }

  if (!xaccTransEqual(sa->parent, sb->parent, check_guids, check_txn_splits))
  {
    PWARN ("transactions differ");
    return FALSE;
  }

  return(TRUE);
}

/********************************************************************
 * xaccSplitGetSlots
 ********************************************************************/

kvp_frame * 
xaccSplitGetSlots(Split * s) {
  if(!s) return NULL;
  return(s->kvp_data);
}

void
xaccSplitSetSlots_nc(Split *s, kvp_frame *frm)
{
    g_return_if_fail(s);
    g_return_if_fail(frm);

    if(s->kvp_data)
    {
        kvp_frame_delete(s->kvp_data);
    }

    s->kvp_data = frm;
}

/********************************************************************
 * Account funcs
 ********************************************************************/

static void
xaccSplitSetAccount_Internal(Split *s, Account *act)
{
    if(!act)
    {
        return;
    }
    s->acc = act;
}

Account*
xaccSplitGetAccount(Split *s)
{
    if(!s) return NULL;
    
    if (!s->acc)
    {
        Account *account;

        account = xaccAccountLookupEntityTable (&s->acc_guid, s->entity_table);
        xaccSplitSetAccount_Internal (s, account);
    }

    return s->acc;
}

const GUID *
xaccSplitGetAccountGUID(Split *split)
{
    if (!split) return NULL;
    return (const GUID*) &split->acc_guid;
}
    
void
xaccSplitSetAccount(Split *s, Account *act)
{
    if (!s) return;

    if(!act)
    {
        s->acc_guid = *xaccGUIDNULL();
    }
    else
    {
        const GUID *id = xaccAccountGetGUID(act);
        s->acc_guid = *id;
    }

    s->acc = act;
}

void
xaccSplitSetAccountGUID(Split *s, GUID id)
{
    if (!s) return;
    s->acc_guid = id;
    s->acc = NULL;
}


/********************************************************************\
\********************************************************************/

const GUID *
xaccSplitGetGUID (Split *split)
{
  if (!split) return xaccGUIDNULL();
  return &split->guid;
}

GUID
xaccSplitReturnGUID (Split *split)
{
  if (!split) return *xaccGUIDNULL();
  return split->guid;
}

/********************************************************************\
\********************************************************************/

void 
xaccSplitSetGUID (Split *split, const GUID *guid)
{
  if (!split || !guid) return;
  check_open (split->parent);
  xaccRemoveEntity(split->entity_table, &split->guid);
  split->guid = *guid;
  xaccStoreEntity(split->entity_table, split, &split->guid, GNC_ID_SPLIT);
}

/********************************************************************\
\********************************************************************/

Split *
xaccSplitLookupEntityTable (const GUID *guid, GNCEntityTable *entity_table)
{
  if (!guid) return NULL;
  g_return_val_if_fail (entity_table, NULL);
  return xaccLookupEntity(entity_table, guid, GNC_ID_SPLIT);
}

Split *
xaccSplitLookup (const GUID *guid, GNCSession *session)
{
  if (!guid) return NULL;
  g_return_val_if_fail (session, NULL);
  return xaccLookupEntity(gnc_session_get_entity_table (session),
                          guid, GNC_ID_SPLIT);
}

/********************************************************************\
\********************************************************************/

void
xaccConfigSetForceDoubleEntry (int force) 
{
   force_double_entry = force;
}

int
xaccConfigGetForceDoubleEntry (void) 
{
   return (force_double_entry);
}

/********************************************************************\
\********************************************************************/

G_INLINE_FUNC void mark_split_internal (Split *split,
                                        gboolean generate_events);
G_INLINE_FUNC void
mark_split_internal (Split *split, gboolean generate_events)
{
  Account *account = xaccSplitGetAccount(split);
  Transaction *trans;

  if (account && !account->do_free)
  {
    account->balance_dirty = TRUE;
    account->sort_dirty = TRUE;

    xaccGroupMarkNotSaved (account->parent);

    if (generate_events)
      gnc_engine_generate_event (&account->guid, GNC_EVENT_MODIFY);
  }

  trans = split->parent;
  if (trans && generate_events)
    gnc_engine_generate_event (&trans->guid, GNC_EVENT_MODIFY);
}

G_INLINE_FUNC void mark_split (Split *split);
G_INLINE_FUNC void
mark_split (Split *split)
{
  mark_split_internal (split, TRUE);
}

G_INLINE_FUNC void mark_trans (Transaction *trans);
G_INLINE_FUNC void
mark_trans (Transaction *trans)
{
  GList *node;

  for (node = trans->splits; node; node = node->next)
    mark_split_internal (node->data, FALSE);

  gnc_engine_generate_event (&trans->guid, GNC_EVENT_MODIFY);
}

/********************************************************************\
\********************************************************************/

static int
get_currency_denom(Split * s)
{
    if(!s)
    {
        return 0;
    }
    else if(!s->parent || !s->parent->common_currency)
    {
        return 100000;
    }
    else
    {
        return gnc_commodity_get_fraction (s->parent->common_currency);
    }
}

static int
get_commodity_denom(Split * s) 
{
    if(!s)
    {
        return 0;
    }
    else if(!xaccSplitGetAccount(s))
    {
        return 100000;
    }
    else
    {
        return xaccAccountGetCommoditySCU(xaccSplitGetAccount(s));
    }
}

/********************************************************************\
\********************************************************************/

void 
DxaccSplitSetSharePriceAndAmount (Split *s, double price, double amt)
{
  if (!s) return;
  check_open (s->parent);

  s->amount = double_to_gnc_numeric(amt, get_commodity_denom(s),
                                    GNC_RND_ROUND);
  s->value  = double_to_gnc_numeric(price * amt, get_currency_denom(s),
                                    GNC_RND_ROUND);

  mark_split (s);
}

void 
xaccSplitSetSharePriceAndAmount (Split *s, gnc_numeric price, 
                                 gnc_numeric amt)
{
  if (!s) return;
  check_open (s->parent);

  s->amount = gnc_numeric_convert(amt, get_commodity_denom(s), GNC_RND_ROUND);
  s->value  = gnc_numeric_mul(s->amount, price, 
                              get_currency_denom(s), GNC_RND_ROUND);

  mark_split (s);
}

void 
DxaccSplitSetSharePrice (Split *s, double amt) 
{
  xaccSplitSetSharePrice
    (s, double_to_gnc_numeric(amt, GNC_DENOM_AUTO,
                              GNC_DENOM_SIGFIGS(PRICE_SIGFIGS) |
                              GNC_RND_ROUND));
}

void 
xaccSplitSetSharePrice (Split *s, gnc_numeric price) 
{
  if (!s) return;
  check_open (s->parent);

  s->value = gnc_numeric_mul(s->amount, price, get_currency_denom(s),
                             GNC_RND_ROUND);

  mark_split (s);
}

void 
DxaccSplitSetShareAmount (Split *s, double damt) 
{
  gnc_numeric old_price;
  gnc_numeric amt = double_to_gnc_numeric(damt, get_commodity_denom(s), 
                                          GNC_RND_ROUND); 
  if (!s) return;
  check_open (s->parent);
  
  if(!gnc_numeric_zero_p(s->amount)) {
    old_price = gnc_numeric_div(s->value, s->amount, GNC_DENOM_AUTO,
                                GNC_DENOM_REDUCE);
  }
  else {
    old_price = gnc_numeric_create(1, 1);
  }

  s->amount = gnc_numeric_convert(amt, get_commodity_denom(s), 
                                  GNC_RND_NEVER);
  s->value  = gnc_numeric_mul(s->amount, old_price, 
                              get_currency_denom(s), GNC_RND_ROUND);

  mark_split (s);
}

void 
xaccSplitSetAmount (Split *s, gnc_numeric amt) 
{
  if(!s) return;
  check_open (s->parent);

  s->amount = gnc_numeric_convert(amt, get_commodity_denom(s), GNC_RND_ROUND);

  mark_split (s);
}

void 
DxaccSplitSetValue (Split *s, double damt) 
{
  gnc_numeric amt = double_to_gnc_numeric(damt, 
                                          get_currency_denom(s), 
                                          GNC_RND_ROUND);
  gnc_numeric old_price;
  if (!s) return;
  check_open (s->parent);

  if(!gnc_numeric_zero_p(s->amount)) {
    old_price = gnc_numeric_div(s->value, s->amount, GNC_DENOM_AUTO,
                                GNC_DENOM_REDUCE);
  }
  else {
    old_price = gnc_numeric_create(1, 1);
  }

  s->value = gnc_numeric_convert(amt, get_currency_denom(s), 
                                 GNC_RND_NEVER);

  if(!gnc_numeric_zero_p(old_price)) {
    s->amount = gnc_numeric_div(s->value, old_price, get_currency_denom(s),
                                GNC_RND_ROUND);
  }

  mark_split (s);
}

void 
xaccSplitSetValue (Split *s, gnc_numeric amt) 
{
  if(!s) return;
  check_open (s->parent);

  s->value = gnc_numeric_convert(amt, get_currency_denom(s), GNC_RND_ROUND);

  mark_split (s);
}

/********************************************************************\
\********************************************************************/

gnc_numeric 
xaccSplitGetBalance (Split *s) {
   if (!s) return gnc_numeric_zero();
   return s->balance;
}

gnc_numeric 
xaccSplitGetClearedBalance (Split *s) {
   if (!s) return gnc_numeric_zero();
   return s->cleared_balance;
}

gnc_numeric 
xaccSplitGetReconciledBalance (Split *s)  {
   if (!s) return gnc_numeric_zero();
   return s->reconciled_balance;
}

/********************************************************************\
 * xaccInitTransaction
 * Initialize a transaction structure
\********************************************************************/

static void
xaccInitTransaction (Transaction * trans, GNCSession *session)
{
  /* Fill in some sane defaults */
  trans->num         = g_cache_insert(gnc_engine_get_string_cache(), "");
  trans->description = g_cache_insert(gnc_engine_get_string_cache(), "");

  trans->common_currency = NULL;
  trans->splits = NULL;

  trans->date_entered.tv_sec  = 0;
  trans->date_entered.tv_nsec = 0;

  trans->date_posted.tv_sec  = 0;
  trans->date_posted.tv_nsec = 0;

  trans->version = 0;
  trans->version_check = 0;
  trans->marker = 0;
  trans->editlevel = 0;
  trans->do_free = FALSE;
  trans->orig = NULL;

  trans->kvp_data = kvp_frame_new();
  trans->idata = 0;

  trans->entity_table = gnc_session_get_entity_table (session);

  xaccGUIDNew (&trans->guid, session);
  xaccStoreEntity (trans->entity_table, trans, &trans->guid, GNC_ID_TRANS);
}

/********************************************************************\
\********************************************************************/

Transaction *
xaccMallocTransaction (GNCSession *session)
{
  Transaction *trans = g_new(Transaction, 1);

  xaccInitTransaction (trans, session);

  gnc_engine_generate_event (&trans->guid, GNC_EVENT_CREATE);

  return trans;
}

/********************************************************************\
\********************************************************************/
/* This routine is not exposed externally, since it does weird things, 
 * like not really owning the splits correctly, and other weirdnesses. 
 * This routine is prone to programmer snafu if not used correctly. 
 * It is used only by the edit-rollback code.
 */

static Transaction *
xaccCloneTransaction (Transaction *t)
{
  Transaction *trans;
  GList *node;

  trans = g_new0 (Transaction, 1);

  trans->num         = g_cache_insert (gnc_engine_get_string_cache(), t->num);
  trans->description = g_cache_insert (gnc_engine_get_string_cache(), t->description);

  trans->kvp_data = kvp_frame_copy (t->kvp_data);

  trans->splits = g_list_copy (t->splits);
  for (node = trans->splits; node; node = node->next)
    node->data = xaccCloneSplit (node->data);

  trans->date_entered.tv_sec  = t->date_entered.tv_sec;
  trans->date_entered.tv_nsec = t->date_entered.tv_nsec;

  trans->date_posted.tv_sec  = t->date_posted.tv_sec;
  trans->date_posted.tv_nsec = t->date_posted.tv_nsec;

  trans->version = t->version;
  trans->editlevel = 0;
  trans->do_free = FALSE;
  trans->orig = NULL;

  trans->common_currency = t->common_currency;

  /* copy(!) the guid and entity table.  The cloned transaction is
   * *not* unique, is a sick twisted clone that holds 'undo'
   * information. */
  trans->guid = t->guid;
  trans->entity_table = t->entity_table;

  return trans;
}


/********************************************************************\
\********************************************************************/

static void
xaccFreeTransaction (Transaction *trans)
{
  GList *node;

  if (!trans) return;

  ENTER ("addr=%p\n", trans);

  /* free up the destination splits */
  for (node = trans->splits; node; node = node->next)
    xaccFreeSplit (node->data);
  g_list_free (trans->splits);
  trans->splits = NULL;

  /* free up transaction strings */
  g_cache_remove(gnc_engine_get_string_cache(), trans->num);
  g_cache_remove(gnc_engine_get_string_cache(), trans->description);

  kvp_frame_delete (trans->kvp_data);

  /* just in case someone looks up freed memory ... */
  trans->num         = NULL;
  trans->description = NULL;
  trans->kvp_data    = NULL;

  trans->date_entered.tv_sec = 0;
  trans->date_entered.tv_nsec = 0;

  trans->date_posted.tv_sec = 0;
  trans->date_posted.tv_nsec = 0;

  trans->version = 0;
  trans->editlevel = 0;
  trans->do_free = FALSE;

  if (trans->orig)
  {
    xaccFreeTransaction (trans->orig);
    trans->orig = NULL;
  }

  g_free(trans);

  LEAVE ("addr=%p\n", trans);
}

/********************************************************************
 xaccTransEqual

 Compare two transactions for equality.  We don't pay any attention to
 rollback issues here, and we only care about equality of "permanent
 fields", basically the things that would survive a file save/load
 cycle.

 ********************************************************************/

gboolean
xaccTransEqual(const Transaction *ta, const Transaction *tb,
               gboolean check_guids,
               gboolean check_splits) {

  if(!ta && !tb) return TRUE;

  if(!ta || !tb)
  {
    PWARN ("one is NULL");
    return FALSE;
  }

  if(check_guids) {
    if(!guid_equal(&(ta->guid), &(tb->guid)))
    {
      PWARN ("GUIDs differ");
      return FALSE;
    }
  }

  if(!gnc_commodity_equal(ta->common_currency, tb->common_currency))
  {
    PWARN ("commodities differ %s vs %s",
           gnc_commodity_get_unique_name (ta->common_currency),
           gnc_commodity_get_unique_name (tb->common_currency));
    return FALSE;
  }

  if(timespec_cmp(&(ta->date_entered), &(tb->date_entered)))
  {
    PWARN ("date entered differs");
    return FALSE;
  }

  if(timespec_cmp(&(ta->date_posted), &(tb->date_posted)))
  {
    PWARN ("date posted differs");
    return FALSE;
  }

  /* Since we use cached strings, we can just compare pointer
   * equality for num and description
   */
  if(ta->num != tb->num)
  {
    PWARN ("num differs: %s vs %s", ta->num, tb->num);
    return FALSE;
  }

  if(ta->description != tb->description)
  {
    PWARN ("descriptions differ: %s vs %s", ta->description, tb->description);
    return FALSE;
  }

  if(kvp_frame_compare(ta->kvp_data, tb->kvp_data) != 0)
  {
    char *frame_a;
    char *frame_b;

    frame_a = kvp_frame_to_string (ta->kvp_data);
    frame_b = kvp_frame_to_string (tb->kvp_data);

    PWARN ("kvp frames differ:\n%s\n\nvs\n\n%s", frame_a, frame_b);

    g_free (frame_a);
    g_free (frame_b);

    return FALSE;
  }

  if (check_splits)
  {
    GList *sa = ta->splits;
    GList *sb = tb->splits;

    if ((!sa && sb) || (!sb && sa))
    {
      PWARN ("only one has splits");
      return FALSE;
    }

    if (sa && sb)
    {
      /* presume that the splits are in the same order */
      while (sa && sb)
      {
        if (!xaccSplitEqual(sa->data, sb->data, check_guids, FALSE))
        {
          PWARN ("splits differ");
          return(FALSE);
        }

        sa = sa->next;
        sb = sb->next;
      }

      if ((sa != NULL) || (sb != NULL))
      {
        PWARN ("different number of splits");
        return(FALSE);
      }
    }
  }

  return(TRUE);
}

/********************************************************************
 * xaccTransGetSlots
 ********************************************************************/

kvp_frame * 
xaccTransGetSlots(Transaction *t) {
  if(!t) return NULL;
  return(t->kvp_data);
}

void
xaccTransSetSlots_nc(Transaction *t, kvp_frame *frm)
{
    g_return_if_fail(t);
    g_return_if_fail(frm);

    if(t->kvp_data)
    {
        kvp_frame_delete(t->kvp_data);
    }

    t->kvp_data = frm;
}

/********************************************************************\
\********************************************************************/

const GUID *
xaccTransGetGUID (Transaction *trans)
{
  if (!trans) return xaccGUIDNULL();
  return &trans->guid;
}

GUID
xaccTransReturnGUID (Transaction *trans)
{
  if (!trans) return *xaccGUIDNULL();
  return trans->guid;
}

/********************************************************************\
\********************************************************************/

void 
xaccTransSetGUID (Transaction *trans, const GUID *guid)
{
  if (!trans || !guid) return;
  xaccRemoveEntity(trans->entity_table, &trans->guid);
  trans->guid = *guid;
  xaccStoreEntity(trans->entity_table, trans, &trans->guid, GNC_ID_TRANS);
}


/********************************************************************\
\********************************************************************/

Transaction *
xaccTransLookupEntityTable (const GUID *guid,
                            GNCEntityTable *entity_table)
{
  g_return_val_if_fail (entity_table, NULL);
  return xaccLookupEntity (entity_table, guid, GNC_ID_TRANS);
}

Transaction *
xaccTransLookup (const GUID *guid, GNCSession *session)
{
  if (!guid) return NULL;
  g_return_val_if_fail (session, NULL);
  return xaccLookupEntity (gnc_session_get_entity_table (session),
                           guid, GNC_ID_TRANS);
}

/********************************************************************\
\********************************************************************/

void
DxaccSplitSetBaseValue (Split *s, double value, 
                       const gnc_commodity * base_currency)
{
  xaccSplitSetBaseValue(s, 
                        double_to_gnc_numeric(value, get_currency_denom(s), 
                                              GNC_RND_ROUND),
                        base_currency);
}

void
xaccSplitSetBaseValue (Split *s, gnc_numeric value, 
                       const gnc_commodity * base_currency)
{
  const gnc_commodity *currency;
  const gnc_commodity *commodity;

  if (!s) return;
  check_open (s->parent);

  /* Novice/casual users may not want or use the double entry
   * features of this engine. So, in particular, there may be the
   * occasional split without a parent account. Well, that's ok,
   * we'll just go with the flow. */
  if (!xaccSplitGetAccount(s)) {
    if (force_double_entry) {
      PERR ("split must have a parent\n");
      g_return_if_fail (xaccSplitGetAccount(s));
    } 
    else { 
      /* this is a change in semantics.  previously, calling 
       * setbasevalue on the same split twice would set the 
       * amount the first time and the value the second.  
       * that's bogus. -- bg */
      s->value = value;
      s->amount = value;
    }
    mark_split (s);
    return;
  }

  currency = xaccTransGetCurrency (xaccSplitGetParent(s));
  commodity = xaccAccountGetCommodity (xaccSplitGetAccount(s));

  /* if the base_currency is the account currency, set the 
   * value.  If it's the account commodity, set the damount. 
   * If both, set both. */
  if (gnc_commodity_equiv(currency, base_currency)) {
    if(gnc_commodity_equiv(commodity, base_currency)) {
      s->amount = gnc_numeric_convert(value,
                                      get_commodity_denom(s), 
                                      GNC_RND_NEVER);
    }
    s->value = gnc_numeric_convert(value, 
                                   get_currency_denom(s),
                                   GNC_RND_NEVER);
  }
  else if (gnc_commodity_equiv(commodity, base_currency)) {
    s->amount = gnc_numeric_convert(value, get_commodity_denom(s),
                                    GNC_RND_NEVER);
  }
  else if ((NULL==base_currency) && (0 == force_double_entry)) { 
    s->value = gnc_numeric_convert(value, get_currency_denom(s),
                                   GNC_RND_NEVER);
  }
  else {
    PERR ("inappropriate base currency %s "
          "given split currency=%s and commodity=%s\n",
          gnc_commodity_get_printname(base_currency), 
          gnc_commodity_get_printname(currency), 
          gnc_commodity_get_printname(commodity));
    return;
  }

  mark_split (s);
}

gnc_numeric
xaccSplitGetBaseValue (Split *s, const gnc_commodity * base_currency)
{
  const gnc_commodity *currency;
  const gnc_commodity *commodity;
  gnc_numeric value;

  if (!s) return gnc_numeric_zero();

  /* ahh -- users may not want or use the double entry 
   * features of this engine.  So, in particular, there
   * may be the occasional split without a parent account. 
   * Well, that's ok, we'll just go with the flow. 
   */
  if (!xaccSplitGetAccount(s)) {
    if (force_double_entry) {
      g_return_val_if_fail (xaccSplitGetAccount(s), gnc_numeric_zero ());
    } 
    else { 
      return s->value;
    }
  }

  currency = xaccTransGetCurrency (xaccSplitGetParent(s));
  commodity = xaccAccountGetCommodity (xaccSplitGetAccount(s));

  /* be more precise -- the value depends on the currency we want it
   * expressed in.  */
  if (gnc_commodity_equiv(currency, base_currency)) {
    value = s->value;
  }
  else if (gnc_commodity_equiv(commodity, base_currency)) {
    value = s->amount;   
  }
  else if ((NULL == base_currency) && (0 == force_double_entry)) {
    value = s->value;
  }
  else {
    PERR ("inappropriate base currency %s "
          "given split currency=%s and commodity=%s\n",
          gnc_commodity_get_printname(base_currency), 
          gnc_commodity_get_printname(currency), 
          gnc_commodity_get_printname(commodity));
    return gnc_numeric_zero();
  }

  return value;
}

/********************************************************************\
\********************************************************************/

gnc_numeric
xaccSplitsComputeValue (GList *splits, Split * skip_me,
                        const gnc_commodity * base_currency)
{
  GList *node;
  gnc_numeric value;

  value = gnc_numeric_zero();

  for (node = splits; node; node = node->next)
  {
    Split *s = node->data;

    if (s == skip_me)
      continue;

    /* ahh -- users may not want or use the double entry features of
     * this engine. So, in particular, there may be the occasional
     * split without a parent account. Well, that's ok, we'll just
     * go with the flow. */
    if (!xaccSplitGetAccount(s)) {
      if (force_double_entry) {
        g_return_val_if_fail (xaccSplitGetAccount(s), gnc_numeric_zero ());
      } 
      else { 
        value = gnc_numeric_add(value, s->value,
                                GNC_DENOM_AUTO, GNC_DENOM_LCD);
      }
    }
    else if ((NULL == base_currency) && (0 == force_double_entry)) {
      value = gnc_numeric_add(value, s->value,
                              GNC_DENOM_AUTO, GNC_DENOM_LCD);
    }
    else {
      const gnc_commodity *currency;
      const gnc_commodity *commodity;

      currency = xaccTransGetCurrency (xaccSplitGetParent(s));
      commodity = xaccAccountGetCommodity (xaccSplitGetAccount(s));

      /* OK, we've got a parent account, we've got currency, lets
       * behave like professionals now, instead of the shenanigans
       * above. Note that just because the currencies are equivalent
       * doesn't mean the denominators are the same! */
      if (base_currency &&
          gnc_commodity_equiv(currency, base_currency)) {
        value = gnc_numeric_add(value, s->value,
                                GNC_DENOM_AUTO, GNC_DENOM_LCD);
      }
      else if (base_currency && 
               gnc_commodity_equiv(commodity, base_currency)) {
        value = gnc_numeric_add(value, s->amount,
                                GNC_DENOM_AUTO, GNC_DENOM_LCD);
      }
      else {
        PERR ("inconsistent currencies\n"   
              "\tbase = '%s', curr='%s', sec='%s'\n",
               gnc_commodity_get_printname(base_currency),
               gnc_commodity_get_printname(currency),
               gnc_commodity_get_printname(commodity));
        g_return_val_if_fail (FALSE, gnc_numeric_zero ());
      }
    }
  }

  if (base_currency)
    return gnc_numeric_convert (value,
                                gnc_commodity_get_fraction (base_currency),
                                GNC_RND_ROUND);
  else
    return gnc_numeric_convert (value, GNC_DENOM_AUTO, GNC_DENOM_REDUCE);
}

gnc_numeric
xaccTransGetImbalance (Transaction * trans)
{
  const gnc_commodity * currency;

  if (!trans)
    return gnc_numeric_zero ();

  currency = xaccTransGetCurrency (trans);
  return xaccSplitsComputeValue (trans->splits, NULL, currency);
}

/********************************************************************\
\********************************************************************/

static gnc_commodity *
FindCommonExclSCurrency (GList *splits,
                         gnc_commodity * ra, gnc_commodity * rb,
                         Split *excl_split,
                         GNCSession *session)
{
  GList *node;

  if (!splits) return NULL;

  g_return_val_if_fail (session, NULL);

  for (node = splits; node; node = node->next)
  {
    Split *s = node->data;
    gnc_commodity * sa, * sb;

    if (s == excl_split)
      continue;

    /* Novice/casual users may not want or use the double entry 
     * features of this engine.   Because of this, there
     * may be the occasional split without a parent account. 
     * Well, that's ok,  we'll just go with the flow. 
     */
    if (force_double_entry)
       g_return_val_if_fail (xaccSplitGetAccount(s), NULL);
    else if (xaccSplitGetAccount(s) == NULL)
      continue;

    sa = DxaccAccountGetCurrency (xaccSplitGetAccount(s), session);
    sb = DxaccAccountGetSecurity (xaccSplitGetAccount(s), session);

    if (ra && rb) {
       int aa = !gnc_commodity_equiv(ra,sa);
       int ab = !gnc_commodity_equiv(ra,sb);
       int ba = !gnc_commodity_equiv(rb,sa);
       int bb = !gnc_commodity_equiv(rb,sb);

       if ( (!aa) && bb) rb = NULL;
       else
       if ( (!ab) && ba) rb = NULL;
       else
       if ( (!ba) && ab) ra = NULL;
       else
       if ( (!bb) && aa) ra = NULL;
       else
       if ( aa && bb && ab && ba ) { ra = NULL; rb = NULL; }

       if (!ra) { ra = rb; rb = NULL; }
    }
    else
    if (ra && !rb) {
       int aa = !gnc_commodity_equiv(ra,sa);
       int ab = !gnc_commodity_equiv(ra,sb);
       if ( aa && ab ) ra = NULL;
    }

    if ((!ra) && (!rb)) return NULL;
  }

  return (ra);
}

/* This is the wrapper for those calls (i.e. the older ones) which
 * don't exclude one split from the splitlist when looking for a
 * common currency.  
 */
static gnc_commodity *
FindCommonCurrency (GList *splits, gnc_commodity * ra, gnc_commodity * rb,
                    GNCSession *session)
{
  return FindCommonExclSCurrency(splits, ra, rb, NULL, session);
}

gnc_commodity *
xaccTransFindOldCommonCurrency (Transaction *trans, GNCSession *session)
{
  gnc_commodity *ra, *rb, *retval;
  Split *split;

  if (!trans) return NULL;

  if (trans->splits == NULL) return NULL;

  g_return_val_if_fail (session, NULL);

  split = trans->splits->data;

  if (xaccSplitGetAccount(split) == NULL) return NULL;

  ra = DxaccAccountGetCurrency (xaccSplitGetAccount(split), session);
  rb = DxaccAccountGetSecurity (xaccSplitGetAccount(split), session);

  retval = FindCommonCurrency (trans->splits, ra, rb, session);

  /* compare this value to what we think should be the 'right' value */
  if (!trans->common_currency)
  {
    trans->common_currency = retval;
  }
  else if (!gnc_commodity_equiv (retval,trans->common_currency))
  {
    PWARN ("expected common currency %s but found %s\n",
           gnc_commodity_get_unique_name (trans->common_currency),
           gnc_commodity_get_unique_name (retval));
  }

  if (NULL == retval)
  {
     /* in every situation I can think of, this routine should return 
      * common currency.  So make note of this ... */
     PWARN ("unable to find a common currency, and that is strange.");
  }

  return retval;
}

/********************************************************************\
\********************************************************************/
/* The new routine for setting the common currency */

gnc_commodity *
xaccTransGetCurrency (Transaction *trans)
{
  if (!trans) return NULL;
  return trans->common_currency;
}

void
xaccTransSetCurrency (Transaction *trans, gnc_commodity *curr)
{
  GList *splits;
  gint fraction;

  if (!trans || !curr) return;
  check_open (trans);

  trans->common_currency = curr;
  fraction = gnc_commodity_get_fraction (curr);

  for (splits = trans->splits; splits; splits = splits->next)
  {
    Split *s = splits->data;
    s->value = gnc_numeric_convert(s->value, fraction, GNC_RND_ROUND);
  }

  mark_trans (trans);
}

/********************************************************************\
\********************************************************************/

void
xaccTransBeginEdit (Transaction *trans)
{
   Backend *be;
   if (!trans) return;

   trans->editlevel ++;
   if (1 < trans->editlevel) return;

   if (0 >= trans->editlevel) 
   {
      PERR ("unbalanced call - resetting (was %d)", trans->editlevel);
      trans->editlevel = 1;
   }

   /* See if there's a backend.  If there is, invoke it. */
   be = xaccTransactionGetBackend (trans);
   if (be && be->trans_begin_edit)
      (be->trans_begin_edit) (be, trans);

   xaccOpenLog ();
   xaccTransWriteLog (trans, 'B');

   /* make a clone of the transaction; we will use this 
    * in case we need to roll-back the edit. 
    */
   trans->orig = xaccCloneTransaction (trans);
}

void
xaccTransCommitEdit (Transaction *trans)
{
   Split *split;
   Backend *be;
   const char *str;

   if (!trans) return;
   ENTER ("trans addr=%p", trans);

   trans->editlevel--;
   if (0 < trans->editlevel) return;

   if (0 > trans->editlevel)
   {
      PERR ("unbalanced call - resetting (was %d)", trans->editlevel);
      trans->editlevel = 0;
   }

   /* At this point, we check to see if we have a valid transaction.
    * There are two possiblities:
    *   1) Its more or less OK, and needs a little cleanup
    *   2) It has zero splits, i.e. is meant to be destroyed.
    * We handle 1) immediately, and we call the backend before 
    * we go through with 2).
    */
   if (trans->splits && !(trans->do_free))
   {
      split = trans->splits->data;
 
      /* Try to get the sorting order lined up according to 
       * when the user typed things in.  */
      if (0 == trans->date_entered.tv_sec) {
         struct timeval tv;
         gettimeofday (&tv, NULL);
         trans->date_entered.tv_sec = tv.tv_sec;
         trans->date_entered.tv_nsec = 1000 * tv.tv_usec;
      }

      /* Alternately the transaction may have only one split in 
       * it, in which case that's OK if and only if the split has no 
       * value (i.e. is only recording a price). Otherwise, a single
       * split with a value can't possibly balance, thus violating the 
       * rules of double-entry, and that's way bogus. So create 
       * a matching opposite and place it either here (if force==1), 
       * or in some dummy account (if force==2).
       */
      if ((1 == force_double_entry) &&
          (NULL == g_list_nth(trans->splits, 1)) &&
          (!gnc_numeric_zero_p(split->amount))) {
        Split * s = xaccMallocSplitEntityTable(trans->entity_table);
        xaccTransAppendSplit (trans, s);
        xaccAccountInsertSplit (xaccSplitGetAccount(s), s);
        xaccSplitSetMemo (s, split->memo);
        xaccSplitSetAction (s, split->action);
        xaccSplitSetAmount(s, gnc_numeric_neg(split->amount));
        xaccSplitSetValue(s, gnc_numeric_neg(split->value));
      }
   }

   /* ------------------------------------------------- */
   /* OK, at this point, we are done making sure that 
    * we've got a validly constructed transaction.
    * Next, we send it off to the back-end, to see if the
    * back-end will accept it.
    */

   /* See if there's a backend.  If there is, invoke it. */
   str = xaccTransGetDescription(trans);
   str = str ? str : "(null)";
   PINFO ("descr is %s", str);

   be = xaccTransactionGetBackend (trans);
   if (be && be->trans_commit_edit) 
   {
      GNCBackendError errcode;

      /* clear errors */
      do {
        errcode = xaccBackendGetError (be);
      } while (ERR_BACKEND_NO_ERR != errcode);

      (be->trans_commit_edit) (be, trans, trans->orig);

      errcode = xaccBackendGetError (be);
      if (ERR_BACKEND_NO_ERR != errcode)
      {
         /* if the backend puked, then we must roll-back 
          * at this point, and let the user know that we failed.
          */
        /* XXX hack alert -- turn this into a gui dialog */
        if (ERR_BACKEND_MODIFIED == errcode)
        {
           PWARN("Another user has modified this transaction\n"
                 "\tjust a moment ago.  Please look at thier changes,\n"
                 "\t and try again, if needed.\n"
                 "\t(This dialog should be a gui dialog and \n"
                 "\tshould check for errors)\n");
        }

        /* push error back onto the stack */
        xaccBackendSetError (be, errcode);

        trans->editlevel++;
        xaccTransRollbackEdit (trans);
        return;
      }
   }

   /* ------------------------------------------------- */
   if (!trans->splits || trans->do_free)
   {
      PINFO ("delete trans at addr=%p", trans);
      /* Make a log in the journal before destruction.  */
      xaccTransWriteLog (trans, 'D');
      xaccRemoveEntity(trans->entity_table, &trans->guid);
      xaccFreeTransaction (trans);
      return;
   }

   /* ------------------------------------------------- */
   /* Make sure all associated splits are in proper order
    * in their accounts with the correct balances. */
   xaccTransFixSplitDateOrder (trans);

   trans->do_free = FALSE;
   xaccTransWriteLog (trans, 'C');

   /* Get rid of the copy we made. We won't be rolling back, 
    * so we don't need it any more.  */
   xaccFreeTransaction (trans->orig);
   trans->orig = NULL;

   LEAVE ("trans addr=%p\n", trans);
}

void
xaccTransRollbackEdit (Transaction *trans)
{
   Backend *be;
   Transaction *orig;
   int force_it=0, mismatch=0;
   int i;
   ENTER ("trans addr=%p\n", trans);

   if (!trans) return;
   trans->editlevel--;
   if (0 < trans->editlevel) return;

   if (0 > trans->editlevel)
   {
      PERR ("unbalanced call - resetting (was %d)", trans->editlevel);
      trans->editlevel = 0;
   }

   /* copy the original values back in. */
   orig = trans->orig;

   /* If the transaction had been deleted before the rollback,
    * the guid would have been unlisted. Restore that */
   xaccStoreEntity(trans->entity_table, trans, &trans->guid, GNC_ID_TRANS);

   trans->common_currency = orig->common_currency;

   g_cache_remove (gnc_engine_get_string_cache(), trans->num);
   trans->num = orig->num;
   orig->num = g_cache_insert(gnc_engine_get_string_cache(), "");

   g_cache_remove (gnc_engine_get_string_cache(), trans->description);
   trans->description = orig->description;
   orig->description = g_cache_insert(gnc_engine_get_string_cache(), "");

   kvp_frame_delete (trans->kvp_data);
   trans->kvp_data = orig->kvp_data;
   if (!trans->kvp_data)
     trans->kvp_data = kvp_frame_new ();
   orig->kvp_data = kvp_frame_new ();

   trans->date_entered.tv_sec  = orig->date_entered.tv_sec;
   trans->date_entered.tv_nsec = orig->date_entered.tv_nsec;

   trans->date_posted.tv_sec  = orig->date_posted.tv_sec;
   trans->date_posted.tv_nsec = orig->date_posted.tv_nsec;

   /* OK, we also have to restore the state of the splits.  Of course,
    * we could brute-force our way through this, and just clobber all of the
    * old splits, and insert all of the new splits, but this kind of brute
    * forcing will suck memory cycles.  So instead we'll try the gentle 
    * approach first.  Note that even in the gentle approach, the 
    * CheckDateOrder routine could be cpu-cyle brutal, so it maybe 
    * it could use some tuning.
    */
   if (trans->do_free)
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

         if (xaccSplitGetAccount(so) != xaccSplitGetAccount(s))
         {
           force_it = 1;
           mismatch = i;
           break;
         }

         g_cache_remove (gnc_engine_get_string_cache(), s->action);
         s->action = so->action;
         so->action = g_cache_insert(gnc_engine_get_string_cache(), "");

         g_cache_remove (gnc_engine_get_string_cache(), s->memo);
         s->memo = so->memo;
         so->memo = g_cache_insert(gnc_engine_get_string_cache(), "");

         kvp_frame_delete (s->kvp_data);
         s->kvp_data = so->kvp_data;
         if (!s->kvp_data)
           s->kvp_data = kvp_frame_new ();
         so->kvp_data = kvp_frame_new ();

         s->reconciled  = so->reconciled;
         s->amount      = so->amount;
         s->value       = so->value;

         s->date_reconciled.tv_sec  = so->date_reconciled.tv_sec;
         s->date_reconciled.tv_nsec = so->date_reconciled.tv_nsec;

         /* do NOT check date order until all of the other fields 
          * have been properly restored */
         xaccAccountFixSplitDateOrder (xaccSplitGetAccount(s), s); 
         xaccAccountRecomputeBalance (xaccSplitGetAccount(s));
         mark_split (s);
      }

      if (so != s)
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

      for (node = g_list_nth (trans->splits, mismatch) ;
           node ; node = node->next)
      {
         Split *s = node->data;

         trans->editlevel++;

         mark_split (s);
         xaccAccountRemoveSplit (xaccSplitGetAccount(s), s);
         xaccAccountRecomputeBalance (xaccSplitGetAccount(s));
         xaccRemoveEntity(s->entity_table, &s->guid);
         xaccFreeSplit (s);

         trans->editlevel--;
      }

      g_list_free (trans->splits);

      trans->splits = orig->splits;
      orig->splits = NULL;

      for (node = g_list_nth (trans->splits, mismatch) ;
           node ; node = node->next)
      {
         Split *s = node->data;
         Account *account = xaccSplitGetAccount(s);

         xaccSplitSetAccount(s, NULL);
         xaccStoreEntity(s->entity_table, s, &s->guid, GNC_ID_SPLIT);
         xaccAccountInsertSplit (account, s);
         xaccAccountRecomputeBalance (account);
         mark_split (s);
      }
   }

   be = xaccTransactionGetBackend (trans);
   if (be && be->trans_rollback_edit) 
   {
      GNCBackendError errcode;

      /* clear errors */
      do {
        errcode = xaccBackendGetError (be);
      } while (ERR_BACKEND_NO_ERR != errcode);

      (be->trans_rollback_edit) (be, trans);

      errcode = xaccBackendGetError (be);
      if (ERR_BACKEND_MOD_DESTROY == errcode)
      {
         /* The backend is asking us to delete this transaction.
          * This typically happens because another (remote) user
          * has deleted this transaction, and we haven't found
          * out about it until this user tried to edit it.
          */
         trans->editlevel++;
         xaccTransDestroy (trans);
         xaccFreeTransaction (trans);

         /* push error back onto the stack */
         xaccBackendSetError (be, errcode);
         LEAVE ("deleted trans addr=%p\n", trans);
         return;
      }
      if (ERR_BACKEND_NO_ERR != errcode) 
      {
        PERR ("Rollback Failed.  Ouch!");
        /* push error back onto the stack */
        xaccBackendSetError (be, errcode);
      }
   }

   xaccTransWriteLog (trans, 'R');

   xaccFreeTransaction (trans->orig);

   trans->orig = NULL;
   trans->do_free = FALSE;

   LEAVE ("trans addr=%p\n", trans);
}

gboolean
xaccTransIsOpen (Transaction *trans)
{
  if (!trans) return FALSE;
  return (0 < trans->editlevel);
}

void
xaccTransSetVersion (Transaction *trans, gint32 vers)
{
  if (!trans) return;
  trans->version = vers;
}

gint32
xaccTransGetVersion (Transaction *trans)
{
  if (!trans) return 0;
  return (trans->version);
}

/********************************************************************\
\********************************************************************/

void
xaccTransDestroy (Transaction *trans)
{
  GList *node;

  if (!trans) return;
  check_open (trans);
  trans->do_free = TRUE;
  xaccTransWriteLog (trans, 'D');

  gnc_engine_generate_event (&trans->guid, GNC_EVENT_DESTROY);

  for (node = trans->splits; node; node = node->next)
  {
    Split *split = node->data;

    mark_split (split);

    xaccAccountRemoveSplit (xaccSplitGetAccount(split), split);
    xaccAccountRecomputeBalance (xaccSplitGetAccount(split));
    xaccRemoveEntity(split->entity_table, &split->guid);
    xaccFreeSplit (split);

    node->data = NULL;
  }

  g_list_free (trans->splits);
  trans->splits = NULL;

  xaccRemoveEntity(trans->entity_table, &trans->guid);

  /* the actual free is done with the commit call, else its rolled back */
  /* xaccFreeTransaction (trans);  don't do this here ... */
}

/********************************************************************\
 * TransRemoveSplit is an engine private function and does not/should
 * not cause any rebalancing to occur.
\********************************************************************/

static void
xaccTransRemoveSplit (Transaction *trans, Split *split) 
{
  if (trans == NULL)
    return;

  trans->splits = g_list_remove (trans->splits, split);
}

/********************************************************************\
\********************************************************************/

void
xaccSplitDestroy (Split *split)
{
   Transaction *trans;

   if (!split) return;

   trans = split->parent;
   check_open (trans);

   mark_split (split);
   xaccRemoveEntity (split->entity_table, &split->guid);

   if (trans)
   {
     gboolean ismember = (g_list_find (trans->splits, split) != NULL);

     if (!ismember)
     {
       PERR ("split not in transaction");
     }
     else
       xaccTransRemoveSplit (trans, split);
   }

   xaccAccountRemoveSplit (xaccSplitGetAccount(split), split);
   xaccAccountRecomputeBalance (xaccSplitGetAccount(split));

   xaccFreeSplit (split);
}

/********************************************************************\
\********************************************************************/

void
xaccTransAppendSplit (Transaction *trans, Split *split) 
{
   Transaction *oldtrans;

   if (!trans || !split) return;
   g_return_if_fail (trans->entity_table == split->entity_table);
   check_open (trans);

   /* first, make sure that the split isn't already inserted 
    * elsewhere. If so, then remove it. */
   oldtrans = split->parent;
   if (oldtrans)
      xaccTransRemoveSplit (oldtrans, split);

   /* now, insert the split into the array */
   split->parent = trans;
   trans->splits = g_list_append (trans->splits, split);

   /* convert the split to the new transaction's commodity denominator */
   /* if the denominator can't be exactly converted, it's an error */
   if (trans->common_currency)
   {
     int fraction = gnc_commodity_get_fraction (trans->common_currency);
     gnc_numeric new_value;

     new_value = gnc_numeric_convert(split->value, fraction, GNC_RND_ROUND);
     if (gnc_numeric_check (new_value) == GNC_ERROR_OK)
       split->value = new_value;
   }
}

/********************************************************************\
 * sorting comparison function
 *
 * returns a negative value if transaction a is dated earlier than b, 
 * returns a positive value if transaction a is dated later than b, 
 *
 * This function tries very hard to uniquely order all transactions.
 * If two transactions occur on the same date, then their "num" fields
 * are compared.  If the num fields are identical, then the description
 * fields are compared.  If these are identical, then the memo fields 
 * are compared.  Hopefully, there will not be any transactions that
 * occur on the same day that have all three of these values identical.
 *
 * Note that being able to establish this kind of absolute order is 
 * important for some of the ledger display functions.
 *
 * Yes, this kind of code dependency is ugly, but the alternatives seem
 * ugly too.
 *
\********************************************************************/


#define DATE_CMP(aaa,bbb,field) {			\
  /* if dates differ, return */				\
  if ( (aaa->field.tv_sec) <				\
       (bbb->field.tv_sec)) {			\
    return -1;						\
  } else						\
  if ( (aaa->field.tv_sec) >				\
       (bbb->field.tv_sec)) {			\
    return +1;						\
  }							\
							\
  /* else, seconds match. check nanoseconds */		\
  if ( (aaa->field.tv_nsec) <			\
       (bbb->field.tv_nsec)) {			\
    return -1;						\
  } else						\
  if ( (aaa->field.tv_nsec) >			\
       (bbb->field.tv_nsec)) {			\
    return +1;						\
  }							\
}



int
xaccSplitDateOrder (Split *sa, Split *sb)
{
  int retval;
  int comp;
  char *da, *db;

  if(sa == sb) return 0;
  /* nothing is always less than something */
  if(!sa && sb) return -1;
  if(sa && !sb) return +1;

  retval = xaccTransOrder (sa->parent, sb->parent);
  if (0 != retval) return retval;

  /* otherwise, sort on memo strings */
  da = sa->memo;
  db = sb->memo;
  SAFE_STRCMP (da, db);

  /* otherwise, sort on action strings */
  da = sa->action;
  db = sb->action;
  SAFE_STRCMP (da, db);

  /* the reconciled flag ... */
  if ((sa->reconciled) < (sb->reconciled)) return -1;
  if ((sa->reconciled) > (sb->reconciled)) return +1;

  /* compare amounts */
  comp = gnc_numeric_compare(sa->amount, sb->amount);
  if(comp < 0) return -1;
  if(comp > 0) return +1;

  comp = gnc_numeric_compare(sa->value, sb->value);
  if(comp < 0) return -1;
  if(comp > 0) return +1;

  /* if dates differ, return */
  DATE_CMP(sa,sb,date_reconciled);

#if 0
  /* sort on txn guid. */
  if(sa->parent && !sb->parent) return -1;
  if(!sa->parent && sb->parent) return 1;
  if(sa->parent && sb->parent) {
    retval = guid_compare(&(sa->guid), &(sb->guid));
    if(retval != 0) return retval;
  }
#endif

  /* else, sort on guid - keeps sort stable. */
  retval = guid_compare(&(sa->guid), &(sb->guid));
  if(retval != 0) return retval;

  return 0;
}

int
xaccTransOrder (Transaction *ta, Transaction *tb)
{
  char *da, *db;
  int retval;

  if ( ta && !tb ) return -1;
  if ( !ta && tb ) return +1;
  if ( !ta && !tb ) return 0;

  /* if dates differ, return */
  DATE_CMP(ta,tb,date_posted);

  /* otherwise, sort on number string */
  da = ta->num;
  db = tb->num;
  SAFE_STRCMP (da, db);

  /* if dates differ, return */
  DATE_CMP(ta,tb,date_entered);

  /* otherwise, sort on description string */
  da = ta->description;
  db = tb->description;
  SAFE_STRCMP (da, db);

  /* else, sort on guid - keeps sort stable. */
  retval = guid_compare(&(ta->guid), &(tb->guid));
  if(retval != 0) return retval;

  return 0;
}
static gboolean
get_corr_account_split(Split *sa, Split **retval)
{
 
  Split *current_split;
  GList *split_list;
  Transaction * ta;
  gnc_numeric sa_value, current_value;
  gboolean sa_value_positive, current_value_positive, seen_different = FALSE;

  *retval = NULL;
  g_return_val_if_fail(sa, TRUE);
  ta = xaccSplitGetParent(sa);
  
  sa_value = xaccSplitGetValue(sa);
  sa_value_positive = gnc_numeric_positive_p(sa_value);

  for (split_list = xaccTransGetSplitList(ta);
       split_list; split_list = split_list->next)
  {
    current_split = split_list->data;
    if(current_split != sa)
    {
      current_value = xaccSplitGetValue(current_split);
      current_value_positive = gnc_numeric_positive_p(current_value);
      if((sa_value_positive && !current_value_positive) || 
	 (!sa_value_positive && current_value_positive))
      {
	if(seen_different)
	{
	  *retval = NULL;
	  return TRUE;
	}
	else
	{
	  seen_different = TRUE;
	  *retval = current_split;
	}
      }
    }
  }
  return FALSE;
}

const char *
xaccSplitGetCorrAccountName(Split *sa)
{
  static const char *split_const = NULL;
  Split *other_split;
  Account *other_split_acc;

  if(get_corr_account_split(sa, &other_split))
  {
    if (!split_const)
      split_const = _("-- Split Transaction --");

    return split_const;
  }
  else
  {
    other_split_acc = xaccSplitGetAccount(other_split);
    return xaccAccountGetName(other_split_acc);
  }
}

char *
xaccSplitGetCorrAccountFullName(Split *sa, char separator)
{
  static const char *split_const = NULL;
  Split *other_split;
  Account *other_split_acc;

  if(get_corr_account_split(sa, &other_split))
  {
    if (!split_const)
      split_const = _("-- Split Transaction --");

    return g_strdup(split_const);
  }
  else
  {
    other_split_acc = xaccSplitGetAccount(other_split);
    return xaccAccountGetFullName(other_split_acc, separator);
  }
}

const char *
xaccSplitGetCorrAccountCode(Split *sa)
{
  static const char *split_const = NULL;
  Split *other_split;
  Account *other_split_acc;

  if(get_corr_account_split(sa, &other_split))
  {
    if (!split_const)
      split_const = _("Split");

    return split_const;
  }
  else
  {
    other_split_acc = xaccSplitGetAccount(other_split);
    return xaccAccountGetName(other_split_acc);
  }
}

int 
xaccSplitCompareAccountFullNames(Split *sa, Split *sb)
{
  Account *aa, *ab;
  char *full_a, *full_b;
  int retval;
  if (!sa && !sb) return 0;
  if (!sa) return -1;
  if (!sb) return 1;

  aa = xaccSplitGetAccount(sa);
  ab = xaccSplitGetAccount(sb);
  full_a = xaccAccountGetFullName(aa, ':');
  full_b = xaccAccountGetFullName(ab, ':');
  /* for comparison purposes it doesn't matter what we use as a separator */
  retval = safe_strcmp(full_a, full_b);
  g_free(full_a);
  g_free(full_b);
  return retval;

}


int 
xaccSplitCompareAccountCodes(Split *sa, Split *sb)
{
  Account *aa, *ab;
  if (!sa && !sb) return 0;
  if (!sa) return -1;
  if (!sb) return 1;

  aa = xaccSplitGetAccount(sa);
  ab = xaccSplitGetAccount(sb);
  
  return safe_strcmp(xaccAccountGetName(aa), xaccAccountGetName(ab));
}

int 
xaccSplitCompareOtherAccountFullNames(Split *sa, Split *sb)
{
  char *ca, *cb; 
  int retval;
  if (!sa && !sb) return 0;
  if (!sa) return -1;
  if (!sb) return 1;

  /* doesn't matter what separator we use
   * as long as they are the same 
   */

  ca = xaccSplitGetCorrAccountFullName(sa, ':');
  cb = xaccSplitGetCorrAccountFullName(sb, ':');
  retval = safe_strcmp(ca, cb);
  g_free(ca);
  g_free(cb);
  return retval;
}

int
xaccSplitCompareOtherAccountCodes(Split *sa, Split *sb)
{
  const char *ca, *cb;
  if (!sa && !sb) return 0;
  if (!sa) return -1;
  if (!sb) return 1;

  ca = xaccSplitGetCorrAccountCode(sa);
  cb = xaccSplitGetCorrAccountCode(sb);
  return safe_strcmp(ca, cb);
}
/********************************************************************\
\********************************************************************/

enum { TDATE_POSTED, TDATE_ENTERED };

static void
xaccTransSetDateInternal(Transaction *trans, int which, time_t secs,
                         long int nsecs)
{
    Timespec *dadate = 0;
    if(!trans) return;
    check_open(trans);

    PINFO ("addr=%p set %d date to %lu %li %s \n",
           trans, which, secs, nsecs, ctime (&secs));
    
    dadate = ((which == TDATE_POSTED)
              ? &trans->date_posted
              : &trans->date_entered);
    dadate->tv_sec = secs;
    dadate->tv_nsec = nsecs;

    mark_trans(trans);
   /* Because the date has changed, we need to make sure that each of
    * the splits is properly ordered in each of their accounts. We
    * could do that here, simply by reinserting each split into its
    * account. However, in some ways this is bad behaviour, and it
    * seems much better/nicer to defer that until the commit phase,
    * i.e. until the user has called the xaccTransCommitEdit()
    * routine. So, for now, we are done. */
}

void
xaccTransSetDateSecs (Transaction *trans, time_t secs)
{
    xaccTransSetDateInternal(trans, TDATE_POSTED, secs, 0);
}

void
xaccTransSetDateEnteredSecs (Transaction *trans, time_t secs)
{
    xaccTransSetDateInternal(trans, TDATE_ENTERED, secs, 0);
}

void
xaccTransSetDatePostedTS (Transaction *trans, const Timespec *ts)
{
   if (!ts) return;
   xaccTransSetDateInternal(trans, TDATE_POSTED, ts->tv_sec, ts->tv_nsec);
}

void
xaccTransSetDateEnteredTS (Transaction *trans, const Timespec *ts)
{
   if (!ts) return;
   xaccTransSetDateInternal(trans, TDATE_ENTERED, ts->tv_sec, ts->tv_nsec);
}

void
xaccTransSetDate (Transaction *trans, int day, int mon, int year) 
{
  Timespec ts = gnc_dmy2timespec(day, mon, year);
  xaccTransSetDateInternal(trans, TDATE_POSTED, ts.tv_sec, ts.tv_nsec);
}

/********************************************************************\
\********************************************************************/

void
xaccTransSetNum (Transaction *trans, const char *xnum)
{
   char * tmp;
   if (!trans || !xnum) return;
   check_open (trans);

   tmp = g_cache_insert(gnc_engine_get_string_cache(), (gpointer) xnum);
   g_cache_remove(gnc_engine_get_string_cache(), trans->num);
   trans->num = tmp;
   mark_trans (trans);
}

void
xaccTransSetDescription (Transaction *trans, const char *desc)
{
   char * tmp;
   if (!trans || !desc) return;
   check_open (trans);

   tmp = g_cache_insert(gnc_engine_get_string_cache(), (gpointer) desc);
   g_cache_remove(gnc_engine_get_string_cache(), trans->description);
   trans->description = tmp;
   mark_trans (trans);
}

void
xaccTransSetNotes (Transaction *trans, const char *notes)
{
  if (!trans || !notes) return;
  check_open (trans);

  kvp_frame_set_slot_nc (trans->kvp_data, "notes", 
                                    kvp_value_new_string (notes));
  mark_trans (trans);
}

/********************************************************************\
\********************************************************************/

Split *
xaccTransGetSplit (Transaction *trans, int i) 
{
   if (!trans) return NULL;
   if (i < 0) return NULL;

   return g_list_nth_data (trans->splits, i);
}

GList *
xaccTransGetSplitList (Transaction *trans)
{
  if (!trans) return NULL;

  return trans->splits;
}

const char *
xaccTransGetNum (Transaction *trans)
{
   if (!trans) return NULL;
   return (trans->num);
}

const char * 
xaccTransGetDescription (Transaction *trans)
{
   if (!trans) return NULL;
   return (trans->description);
}

const char * 
xaccTransGetNotes (Transaction *trans)
{
  kvp_value *v;

  if (!trans) return NULL;

  v = kvp_frame_get_slot (xaccTransGetSlots (trans), "notes");
  if (!v)
    return NULL;

  return kvp_value_get_string (v);
}

time_t
xaccTransGetDate (Transaction *trans)
{
   if (!trans) return 0;
   return (trans->date_posted.tv_sec);
}

void
xaccTransGetDatePostedTS (Transaction *trans, Timespec *ts)
{
   if (!trans || !ts) return;
   *ts = (trans->date_posted);
}

void
xaccTransGetDateEnteredTS (Transaction *trans, Timespec *ts)
{
   if (!trans || !ts) return;
   *ts = (trans->date_entered);
}

Timespec
xaccTransRetDatePostedTS (Transaction *trans)
{
   Timespec ts;
   ts.tv_sec = 0; ts.tv_nsec = 0;
   if (!trans) return ts;
   return (trans->date_posted);
}

Timespec
xaccTransRetDateEnteredTS (Transaction *trans)
{
   Timespec ts;
   ts.tv_sec = 0; ts.tv_nsec = 0;
   if (!trans) return ts;
   return (trans->date_entered);
}

int
xaccTransCountSplits (Transaction *trans)
{
   if (!trans) return 0;
   return g_list_length (trans->splits);
}

/********************************************************************\
\********************************************************************/

void
xaccSplitSetMemo (Split *split, const char *memo)
{
   char * tmp;
   if (!split || !memo) return;
   check_open (split->parent);

   tmp = g_cache_insert(gnc_engine_get_string_cache(), (gpointer) memo);
   g_cache_remove(gnc_engine_get_string_cache(), split->memo);
   split->memo = tmp;
   mark_split (split);
}

void
xaccSplitSetAction (Split *split, const char *actn)
{
   char * tmp;
   if (!split || !actn) return;
   check_open (split->parent);

   tmp = g_cache_insert(gnc_engine_get_string_cache(), (gpointer) actn);
   g_cache_remove(gnc_engine_get_string_cache(), split->action);
   split->action = tmp;
   mark_split (split);
}

void
xaccSplitSetReconcile (Split *split, char recn)
{
   if (!split) return;
   check_open (split->parent);

   switch (recn)
   {
   case NREC:
   case CREC:
   case YREC:
   case FREC:
   case VREC:
     break;
   default:
     PERR("Bad reconciled flag");
     return;
   }

   split->reconciled = recn;

   xaccAccountRecomputeBalance (xaccSplitGetAccount(split));
   mark_split (split);
}

void
xaccSplitSetDateReconciledSecs (Split *split, time_t secs)
{
   if (!split) return;
   check_open (split->parent);

   split->date_reconciled.tv_sec = secs;
   split->date_reconciled.tv_nsec = 0;
   mark_split (split);
}

void
xaccSplitSetDateReconciledTS (Split *split, Timespec *ts)
{
   if (!split || !ts) return;
   check_open (split->parent);

   split->date_reconciled = *ts;
   mark_split (split);
}

void
xaccSplitGetDateReconciledTS (Split * split, Timespec *ts)
{
   if (!split || !ts) return;
   *ts = (split->date_reconciled);
}

Timespec
xaccSplitRetDateReconciledTS (Split * split)
{
   Timespec ts; ts.tv_sec=0; ts.tv_nsec=0;
   if (!split) return ts;
   return (split->date_reconciled);
}

/********************************************************************\
\********************************************************************/

/* return the parent transaction of the split */
Transaction * 
xaccSplitGetParent (Split *split)
{
   if (!split) return NULL;
   return (split->parent);
}

const char *
xaccSplitGetMemo (Split *split)
{
   if (!split) return NULL;
   return (split->memo);
}

const char *
xaccSplitGetAction (Split *split)
{
   if (!split) return NULL;
   return (split->action);
}

char 
xaccSplitGetReconcile (Split *split) {
  if (!split) return ' ';
  return (split->reconciled);
}

double
DxaccSplitGetShareAmount (Split * split) {
  return gnc_numeric_to_double(xaccSplitGetAmount(split));
}

double
DxaccSplitGetValue (Split * split) {
  return gnc_numeric_to_double(xaccSplitGetValue(split));
}

double
DxaccSplitGetSharePrice (Split * split)
{
  return gnc_numeric_to_double(xaccSplitGetSharePrice(split));
}

gnc_numeric
xaccSplitGetAmount (Split * split)
{
  if (!split) return gnc_numeric_zero();
  return split->amount;
}

gnc_numeric
xaccSplitGetValue (Split * split) {
  if (!split) return gnc_numeric_zero();
  return split->value; 
}

gnc_numeric
xaccSplitGetSharePrice (Split * split) {
  if(!split || gnc_numeric_zero_p(split->amount)) {
    return gnc_numeric_create(1, 1);
  }
  return gnc_numeric_div(split->value, 
                         split->amount,
                         GNC_DENOM_AUTO, 
                         GNC_DENOM_SIGFIGS(PRICE_SIGFIGS) |
                         GNC_RND_ROUND);
}

/********************************************************************\
\********************************************************************/

const char *
xaccSplitGetType(const Split *s)
{
  kvp_frame *frame;
  kvp_value *split_type;

  if(!s) return NULL;
  frame = xaccSplitGetSlots((Split *) s);
  if(!frame) return NULL;
  split_type = kvp_frame_get_slot(frame, "split-type");
  if(!split_type) return "normal";
  if(kvp_value_get_type(split_type) != KVP_TYPE_STRING) return NULL;
  return(kvp_value_get_string(split_type));
}

/* reconfigure a split to be a stock split - after this, you shouldn't
   mess with the value, just the amount. */
void
xaccSplitMakeStockSplit(Split *s)
{
  check_open (s->parent);

  xaccSplitSetValue(s, gnc_numeric_zero());
  kvp_frame_set_slot_nc(s->kvp_data,
                        "split-type",
                        kvp_value_new_string("stock-split"));
  mark_split(s);
}


/********************************************************************\
\********************************************************************/

Account *
xaccGetAccountByName (Transaction *trans, const char * name)
{
   Account *acc = NULL;
   GList *node;

   if (!trans) return NULL;
   if (!name) return NULL;

   /* walk through the splits, looking for one, any one, that has a
    * parent account */
   for (node = trans->splits; node; node = node->next)
   {
     Split *s = node->data;

     acc = xaccSplitGetAccount(s);
     if (acc) break;
   }
   
   if (!acc) return NULL;

   return xaccGetPeerAccountFromName (acc, name);
}

/********************************************************************\
\********************************************************************/

Account *
xaccGetAccountByFullName (Transaction *trans, const char * name,
                          const char separator)
{
   Account *acc = NULL;
   GList *node;

   if (!trans) return NULL;
   if (!name) return NULL;

   /* walk through the splits, looking for one, any one, that has a
    * parent account */
   for (node = trans->splits; node; node = node->next)
   {
     Split *s = node->data;

     acc = xaccSplitGetAccount(s);
     if (acc) break;
   }
   
   if (!acc) return NULL;

   return xaccGetPeerAccountFromFullName (acc, name, separator);
}

/********************************************************************\
\********************************************************************/

Split *
xaccSplitGetOtherSplit (Split *split)
{
  Split *s1, *s2;
  Transaction *trans;

  if (!split) return NULL;
  trans = split->parent;

  if (g_list_length (trans->splits) != 2)
    return NULL;

  s1 = g_list_nth_data (trans->splits, 0);
  s2 = g_list_nth_data (trans->splits, 1);

  if (s1 == split)
    return s2;

  return s1;
}

/********************************************************************\
\********************************************************************/

int
xaccIsPeerSplit (Split *sa, Split *sb)
{
   Transaction *ta, *tb;
   if (!sa || !sb) return 0;
   ta = sa->parent;
   tb = sb->parent;
   if (ta == tb) return 1;
   return 0;
}


/********************************************************************\
\********************************************************************/

void
xaccTransVoid(Transaction *transaction,
	      const char *reason)
{
  kvp_frame *frame;
  kvp_value *val;
  gnc_numeric amt, zero;
  GList *split_list;
  Split *split;
  Timespec now;
  char iso8601_str[ISO_DATELENGTH+1] = "";

  g_return_if_fail(transaction && reason);

  xaccTransBeginEdit(transaction);
  zero = gnc_numeric_zero();
  frame = xaccTransGetSlots(transaction);

  val = kvp_value_new_string(reason);

  kvp_frame_set_slot_nc(frame, 
			void_reason_str,
			val);


  now.tv_sec = time(NULL);
  now.tv_nsec = 0;

  gnc_timespec_to_iso8601_buff(now, iso8601_str);

  
  val = kvp_value_new_string (iso8601_str);

  kvp_frame_set_slot_nc(frame, 
			void_time_str,
			val);

  for(  split_list = xaccTransGetSplitList(transaction); 
	split_list; 
	split_list = g_list_next(split_list))
  {
    split = split_list->data;
    
    amt = xaccSplitGetAmount(split);

    val = kvp_value_new_gnc_numeric(amt);

    frame = xaccSplitGetSlots(split);
    
    kvp_frame_set_slot_nc(frame, void_former_amt_str, val);
    
    amt = xaccSplitGetValue(split);
    val = kvp_value_new_gnc_numeric(amt);
    kvp_frame_set_slot_nc(frame, void_former_val_str, val);
    
    
    xaccSplitSetAmount(split, zero);
    xaccSplitSetValue(split, zero);
    xaccSplitSetReconcile(split, VREC);
   
  }

  xaccTransCommitEdit(transaction);

  
  return;
}

gboolean 
xaccTransGetVoidStatus(Transaction *trans)
{
  kvp_frame *frame;

  
  g_return_val_if_fail(trans, FALSE);

  frame = xaccTransGetSlots(trans);

  return (gboolean) kvp_frame_get_slot(frame, void_reason_str);

}

char *
xaccTransGetVoidReason(Transaction *trans)
{
  kvp_frame *frame;
  kvp_value *val;
  char *reason;
  g_return_val_if_fail(trans, NULL);

  frame = xaccTransGetSlots(trans);

  val = kvp_frame_get_slot(frame, void_reason_str);
  
  if(val)
  {
    reason = kvp_value_get_string(val);
    return reason;
  }

  return NULL;
}

gnc_numeric
xaccSplitVoidFormerAmount(Split *split)
{
  kvp_frame *frame;
  kvp_value *val;
  gnc_numeric amt = gnc_numeric_zero();
  g_return_val_if_fail(split, amt);

  frame = xaccSplitGetSlots(split);

  val = kvp_frame_get_slot(frame, void_former_amt_str);
  
  if(val)
  {
    amt = kvp_value_get_numeric(val);
  }

  return amt;
  
}

gnc_numeric
xaccSplitVoidFormerValue(Split *split)
{
  kvp_frame *frame;
  kvp_value *val;
  gnc_numeric amt = gnc_numeric_zero();
  g_return_val_if_fail(split, amt);

  frame = xaccSplitGetSlots(split);

  val = kvp_frame_get_slot(frame, void_former_val_str);
  
  if(val)
  {
    amt = kvp_value_get_numeric(val);
  }

  return amt;
}

Timespec
xaccTransGetVoidTime(Transaction *tr)
{
  kvp_frame *frame;
  kvp_value *val;
  char *iso8601_str;
  Timespec void_time= {0,0};
  g_return_val_if_fail(tr, void_time);

  frame = xaccTransGetSlots(tr);

  val = kvp_frame_get_slot(frame, void_time_str);
  
  if(val)
  {
    void_time = gnc_iso8601_to_timespec_local(kvp_value_get_string(val));
  }

  return void_time;
}
/************************ END OF ************************************\
\************************* FILE *************************************/
