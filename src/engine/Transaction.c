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

#include <assert.h>
#include <string.h>
#include <sys/time.h>
#include <time.h>
#include <unistd.h>

#include "Account.h"
#include "AccountP.h"
#include "BackendP.h"
#include "GNCIdP.h"
#include "Group.h"
#include "Scrub.h"
#include "Transaction.h"
#include "TransactionP.h"
#include "TransLog.h"
#include "date.h"
#include "gnc-commodity.h"
#include "gnc-engine-util.h"
#include "gnc-engine.h"
#include "gnc-event-p.h"


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

/* bit-field flags for controlling transaction commits */
typedef enum
{
  BEGIN_EDIT      = 1 << 0,
  BEING_DESTROYED = 1 << 1,
} TransFlags;

/* arbitrary price per share increment FIXME */
#define PRICE_DENOM 1000000


/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_ENGINE;


/********************************************************************\
 * xaccInitSplit
 * Initialize a Split structure
\********************************************************************/

static void
xaccInitSplit(Split * split)
{
  /* fill in some sane defaults */
  split->acc         = NULL;
  split->parent      = NULL;

  split->action      = g_cache_insert(gnc_string_cache, "");
  split->memo        = g_cache_insert(gnc_string_cache, "");
  split->reconciled  = NREC;
  split->damount     = gnc_numeric_zero();
  split->value       = gnc_numeric_zero();

  split->date_reconciled.tv_sec  = 0;
  split->date_reconciled.tv_nsec = 0;

  split->balance             = gnc_numeric_zero();
  split->cleared_balance     = gnc_numeric_zero();
  split->reconciled_balance  = gnc_numeric_zero();
  split->share_balance             = gnc_numeric_zero();
  split->share_cleared_balance     = gnc_numeric_zero();
  split->share_reconciled_balance  = gnc_numeric_zero();

  split->kvp_data = kvp_frame_new();

  xaccGUIDNew(&split->guid);
  xaccStoreEntity(split, &split->guid, GNC_ID_SPLIT);
}

/********************************************************************\
\********************************************************************/

Split *
xaccMallocSplit(void)
{
  Split *split = g_new(Split, 1);
  xaccInitSplit (split);
  return split;
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

  /* copy(!) the guid.  The cloned split is *not* unique,
   * is a sick twisted clone that holds 'undo' information. */
  split->guid = s->guid;

  split->acc         = s->acc;
  split->parent      = s->parent;

  split->memo        = g_cache_insert (gnc_string_cache, s->memo);
  split->action      = g_cache_insert (gnc_string_cache, s->action);

  split->kvp_data    = kvp_frame_copy (s->kvp_data);

  split->reconciled  = s->reconciled;
  split->date_reconciled = s->date_reconciled;

  split->value       = s->value;
  split->damount     = s->damount;

  /* no need to futz with the balances;  these get wiped each time ... 
   * split->balance             = s->balance;
   * split->cleared_balance     = s->cleared_balance;
   * split->reconciled_balance  = s->reconciled_balance;
   * split->share_balance             = s->share_balance;
   * split->share_cleared_balance     = s->share_cleared_balance;
   * split->share_reconciled_balance  = s->share_reconciled_balance;
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

  g_cache_remove(gnc_string_cache, split->memo);
  g_cache_remove(gnc_string_cache, split->action);

  /* just in case someone looks up freed memory ... */
  split->memo        = NULL;
  split->action      = NULL;
  split->kvp_data    = NULL;
  split->reconciled  = NREC;
  split->damount     = gnc_numeric_zero();
  split->value       = gnc_numeric_zero();
  split->parent      = NULL;
  split->acc         = NULL;

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
               gboolean check_txn_splits) {

  if(!sa && !sb) return TRUE;
  if(!sa) return FALSE;
  if(!sb) return FALSE;

  if(check_guids) {
    if(!guid_equal(&(sa->guid), &(sb->guid))) return FALSE;
  }

  /* Since these strings are cached we can just use pointer equality */
  if(sa->memo != sb->memo) return FALSE;
  if(sa->action != sb->action) return FALSE;

  if(kvp_frame_compare(sa->kvp_data, sb->kvp_data) != 0) return FALSE;

  if(sa->reconciled != sb->reconciled) return FALSE;
  if(!timespec_equal(&(sa->date_reconciled),
                     &(sb->date_reconciled))) return FALSE;

  if(!gnc_numeric_eq(sa->damount, sb->damount)) return FALSE;
  if(!gnc_numeric_eq(sa->value, sb->value)) return FALSE;

  if(!xaccTransEqual(sa->parent, sb->parent,
                     check_guids,
                     check_txn_splits)) {
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

/********************************************************************\
\********************************************************************/

const GUID *
xaccSplitGetGUID (Split *split)
{
  if (!split) return xaccGUIDNULL();
  return &split->guid;
}

/********************************************************************\
\********************************************************************/

void 
xaccSplitSetGUID (Split *split, GUID *guid)
{
  if (!split || !guid) return;
  xaccRemoveEntity(&split->guid);
  split->guid = *guid;
  xaccStoreEntity(split, &split->guid, GNC_ID_SPLIT);
}

/********************************************************************\
\********************************************************************/

Split *
xaccSplitLookup (const GUID *guid)
{
  if (!guid) return NULL;
  return xaccLookupEntity(guid, GNC_ID_SPLIT);
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
  Account *account = split->acc;
  Transaction *trans;

  if (account)
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
get_currency_denom(Split * s) {  
  if(!s) return 0;

  else if(!(s->acc)) {
    return 100000;
  }
  else {
    return xaccAccountGetCurrencySCU(s->acc);
  }
}

static int
get_security_denom(Split * s) {
  if(!s) return 0;
  else if(!(s->acc)) {
    return 100000;
  }
  else {
    return xaccAccountGetSecuritySCU(s->acc);
  }
}

/********************************************************************\
\********************************************************************/

void 
DxaccSplitSetSharePriceAndAmount (Split *s, double price, double amt)
{
  xaccSplitSetSharePriceAndAmount
    (s,
     double_to_gnc_numeric(price, PRICE_DENOM, GNC_RND_ROUND),
     double_to_gnc_numeric(amt, get_security_denom(s), GNC_RND_ROUND));
}

void 
xaccSplitSetSharePriceAndAmount (Split *s, gnc_numeric price, 
                                 gnc_numeric amt)
{
  if (!s) return;

  s->damount = gnc_numeric_convert(amt, get_security_denom(s), GNC_RND_ROUND);;
  s->value   = gnc_numeric_mul(s->damount, price, 
                               get_currency_denom(s), GNC_RND_ROUND);

  mark_split (s);
}

void 
DxaccSplitSetSharePrice (Split *s, double amt) {
  xaccSplitSetSharePrice
    (s, double_to_gnc_numeric(amt, PRICE_DENOM, GNC_RND_ROUND));
}

void 
xaccSplitSetSharePrice (Split *s, gnc_numeric price) {
  if (!s) return;

  s->value = gnc_numeric_mul(s->damount, price, get_currency_denom(s),
                             GNC_RND_ROUND);

  mark_split (s);
}

void 
DxaccSplitSetShareAmount (Split *s, double damt) {
  gnc_numeric old_price;
  gnc_numeric amt = double_to_gnc_numeric(damt, get_security_denom(s), 
                                          GNC_RND_ROUND); 
  if (!s) return;
  
  if(!gnc_numeric_zero_p(s->damount)) {
    old_price = gnc_numeric_div(s->value, s->damount, GNC_DENOM_AUTO,
                                GNC_DENOM_REDUCE);
  }
  else {
    old_price = gnc_numeric_create(PRICE_DENOM, PRICE_DENOM);
  }
  
  s->damount = gnc_numeric_convert(amt, get_security_denom(s), 
                                   GNC_RND_NEVER);
  s->value   = gnc_numeric_mul(s->damount, old_price, 
                               get_currency_denom(s), GNC_RND_ROUND);
  
  mark_split (s);
}

void 
xaccSplitSetShareAmount (Split *s, gnc_numeric amt) {
  if(!s) return;

  s->damount = gnc_numeric_convert(amt, get_security_denom(s), GNC_RND_ROUND);

  mark_split (s);
}

void 
DxaccSplitSetValue (Split *s, double damt) {
  gnc_numeric amt = double_to_gnc_numeric(damt, 
                                          get_currency_denom(s), 
                                          GNC_RND_ROUND);
  gnc_numeric old_price;
  if (!s) return;

  if(!gnc_numeric_zero_p(s->damount)) {
    old_price = gnc_numeric_div(s->value, s->damount, GNC_DENOM_AUTO,
                                GNC_DENOM_REDUCE);
  }
  else {
    old_price = gnc_numeric_create(PRICE_DENOM, PRICE_DENOM);
  }

  s->value = gnc_numeric_convert(amt, get_currency_denom(s), 
                                 GNC_RND_NEVER);

  if(!gnc_numeric_zero_p(old_price)) {
    s->damount = gnc_numeric_div(s->value, old_price, get_currency_denom(s),
                                 GNC_RND_ROUND);
  }

  mark_split (s);
}

void 
xaccSplitSetValue (Split *s, gnc_numeric amt) {
  if(!s) return;

  s->value = gnc_numeric_convert(amt, get_currency_denom(s), GNC_RND_ROUND);

  mark_split (s);
}

/********************************************************************\
\********************************************************************/

double 
DxaccSplitGetBalance (Split *s) {
  return gnc_numeric_to_double(xaccSplitGetBalance(s));
}

double 
DxaccSplitGetClearedBalance (Split *s) {
  return gnc_numeric_to_double(xaccSplitGetClearedBalance(s));
}

double 
DxaccSplitGetReconciledBalance (Split *s)  {
  return gnc_numeric_to_double(xaccSplitGetReconciledBalance(s));
}

double 
DxaccSplitGetShareBalance (Split *s) {
  return gnc_numeric_to_double(xaccSplitGetShareBalance(s));
}

double 
DxaccSplitGetShareClearedBalance (Split *s) {
  return gnc_numeric_to_double(xaccSplitGetShareClearedBalance(s));
}

double 
DxaccSplitGetShareReconciledBalance (Split *s) {
  return gnc_numeric_to_double(xaccSplitGetShareReconciledBalance(s));
}


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

gnc_numeric 
xaccSplitGetShareBalance (Split *s) {
   if (!s) return gnc_numeric_zero();
   return s->share_balance;
}

gnc_numeric 
xaccSplitGetShareClearedBalance (Split *s) {
   if (!s) return gnc_numeric_zero();
   return s->share_cleared_balance;
}

gnc_numeric 
xaccSplitGetShareReconciledBalance (Split *s) {
   if (!s) return gnc_numeric_zero();
   return s->share_reconciled_balance;
}


/********************************************************************\
 * xaccInitTransaction
 * Initialize a transaction structure
\********************************************************************/

static void
xaccInitTransaction (Transaction * trans)
{
  /* Fill in some sane defaults */
  trans->num         = g_cache_insert(gnc_string_cache, "");
  trans->description = g_cache_insert(gnc_string_cache, "");

  trans->splits = NULL;

  trans->date_entered.tv_sec  = 0;
  trans->date_entered.tv_nsec = 0;

  trans->date_posted.tv_sec  = 0;
  trans->date_posted.tv_nsec = 0;

  trans->marker = 0;
  trans->open = 0;
  trans->orig = NULL;

  trans->kvp_data = kvp_frame_new();

  xaccGUIDNew(&trans->guid);
  xaccStoreEntity(trans, &trans->guid, GNC_ID_TRANS);
}

/********************************************************************\
\********************************************************************/

Transaction *
xaccMallocTransaction (void)
{
  Transaction *trans = g_new(Transaction, 1);

  xaccInitTransaction (trans);

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

  trans->num         = g_cache_insert (gnc_string_cache, t->num);
  trans->description = g_cache_insert (gnc_string_cache, t->description);

  trans->kvp_data = kvp_frame_copy (t->kvp_data);

  trans->splits = g_list_copy (t->splits);
  for (node = trans->splits; node; node = node->next)
    node->data = xaccCloneSplit (node->data);

  trans->date_entered.tv_sec  = t->date_entered.tv_sec;
  trans->date_entered.tv_nsec = t->date_entered.tv_nsec;

  trans->date_posted.tv_sec  = t->date_posted.tv_sec;
  trans->date_posted.tv_nsec = t->date_posted.tv_nsec;

  trans->open = 0;
  trans->orig = NULL;

  /* copy(!) the guid.  The cloned transaction is *not* unique,
   * is a sick twisted clone that holds 'undo' information. */
  trans->guid = t->guid;

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
  g_cache_remove(gnc_string_cache, trans->num);
  g_cache_remove(gnc_string_cache, trans->description);

  kvp_frame_delete (trans->kvp_data);

  /* just in case someone looks up freed memory ... */
  trans->num         = NULL;
  trans->description = NULL;

  trans->kvp_data = NULL;

  trans->date_entered.tv_sec = 0;
  trans->date_entered.tv_nsec = 0;

  trans->date_posted.tv_sec = 0;
  trans->date_posted.tv_nsec = 0;

  trans->open = 0;

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
  if(!ta) return FALSE;
  if(!tb) return FALSE;

  if(check_guids) {
    if(!guid_equal(&(ta->guid), &(tb->guid))) return FALSE;
  }

  if(!timespec_equal(&(ta->date_entered), &(tb->date_entered))) return FALSE;
  if(!timespec_equal(&(ta->date_posted), &(tb->date_posted))) return FALSE;
  	/* Since we use cached strings, we can just compare pointer
	 * equality for num and description
	 */
  if(ta->num != tb->num) return FALSE;
  if(ta->description != tb->description) return FALSE;

  if(kvp_frame_compare(ta->kvp_data, tb->kvp_data) != 0) return FALSE;

  if(check_splits) {
    GList *sa = ta->splits;
    GList *sb = tb->splits;

    if(!sa && sb) return FALSE;
    if(!sb && sa) return FALSE;

    if(sa && sb) {
      /* presume that the splits are in the same order */
      while(sa && sb) {
        if(!xaccSplitEqual(sa->data, sb->data, check_guids, FALSE))
          return(FALSE);
        sa = sa->next;
        sb = sb->next;
      }
      if(sa != NULL) return(FALSE);
      if(sb != NULL) return(FALSE);
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

/********************************************************************\
\********************************************************************/

const GUID *
xaccTransGetGUID (Transaction *trans)
{
  if (!trans) return xaccGUIDNULL();
  return &trans->guid;
}

/********************************************************************\
\********************************************************************/

void 
xaccTransSetGUID (Transaction *trans, GUID *guid)
{
  if (!trans || !guid) return;
  xaccRemoveEntity(&trans->guid);
  trans->guid = *guid;
  xaccStoreEntity(trans, &trans->guid, GNC_ID_TRANS);
}


/********************************************************************\
\********************************************************************/

Transaction *
xaccTransLookup (const GUID *guid)
{
  if (!guid) return NULL;
  return xaccLookupEntity(guid, GNC_ID_TRANS);
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
  const gnc_commodity *security;

  if (!s) return;

  /* Novice/casual users may not want or use the double entry
   * features of this engine. So, in particular, there may be the
   * occasional split without a parent account. Well, that's ok,
   * we'll just go with the flow. */
  if (!(s->acc)) {
    if (force_double_entry) {
      PERR ("split must have a parent\n");
      assert (s->acc);
    } 
    else { 
      /* this is a change in semantics.  previously, calling 
       * setbasevalue on the same split twice would set the 
       * amount the first time and the value the second.  
       * that's bogus. -- bg */
      s->value = value;
      s->damount = value;
    }
    mark_split (s);
    return;
  }

  currency = xaccAccountGetCurrency (s->acc);
  security = xaccAccountGetEffectiveSecurity (s->acc);

  /* if the base_currency is the account currency, set the 
   * value.  If it's the account security, set the damount. 
   * If both, set both. */
  if (gnc_commodity_equiv(currency, base_currency)) {
    if(gnc_commodity_equiv(security, base_currency)) {
      s->damount = gnc_numeric_convert(value,
                                       get_security_denom(s), 
                                       GNC_RND_NEVER);
    }
    s->value = gnc_numeric_convert(value, 
                                   get_currency_denom(s),
                                   GNC_RND_NEVER);
  }
  else if (gnc_commodity_equiv(security, base_currency)) {
    s->damount = gnc_numeric_convert(value, get_security_denom(s),
                                     GNC_RND_NEVER);
  }
  else if ((NULL==base_currency) && (0 == force_double_entry)) { 
    s->value = gnc_numeric_convert(value, get_currency_denom(s),
                                   GNC_RND_NEVER);
  }
  else {
    PERR ("inappropriate base currency %s "
          "given split currency=%s and security=%s\n",
          gnc_commodity_get_printname(base_currency), 
          gnc_commodity_get_printname(currency), 
          gnc_commodity_get_printname(security));
    return;
  }

  mark_split (s);
}


double
DxaccSplitGetBaseValue (Split *s, const gnc_commodity * base_currency)
{
  return gnc_numeric_to_double(xaccSplitGetBaseValue(s, base_currency));
}


gnc_numeric
xaccSplitGetBaseValue (Split *s, const gnc_commodity * base_currency)
{
  const gnc_commodity *currency;
  const gnc_commodity *security;
  gnc_numeric value;

  if (!s) return gnc_numeric_zero();
  
  /* ahh -- users may not want or use the double entry 
   * features of this engine.  So, in particular, there
   * may be the occasional split without a parent account. 
   * Well, that's ok, we'll just go with the flow. 
   */
  if (!(s->acc)) {
    if (force_double_entry) {
      assert (s->acc);
    } 
    else { 
      return s->value;
    }
  }

  currency = xaccAccountGetCurrency (s->acc);
  security = xaccAccountGetSecurity (s->acc);

  /* be more precise -- the value depends on the currency we want it
   * expressed in.  */
  if (gnc_commodity_equiv(currency, base_currency)) {
    value = s->value;
  }
  else if (gnc_commodity_equiv(security, base_currency)) {
    value = s->damount;   
  }
  else if ((NULL == base_currency) && (0 == force_double_entry)) {
    value = s->value;
  }
  else {
    PERR ("inappropriate base currency %s "
          "given split currency=%s and security=%s\n",
          gnc_commodity_get_printname(base_currency), 
          gnc_commodity_get_printname(currency), 
          gnc_commodity_get_printname(security));
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
    if (!(s->acc)) {
      if (force_double_entry) {
        assert (s->acc);
      } 
      else { 
        value = gnc_numeric_add(value, s->value, GNC_DENOM_AUTO, 
                                GNC_DENOM_REDUCE);
      }
    } 
    else if ((NULL == base_currency) && (0 == force_double_entry)) {
      value = gnc_numeric_add(value, s->value, GNC_DENOM_AUTO, 
                              GNC_DENOM_REDUCE);
    } 
    else {
      const gnc_commodity *currency;
      const gnc_commodity *security;

      currency = xaccAccountGetCurrency (s->acc);
      security = xaccAccountGetSecurity (s->acc);

      /* OK, we've got a parent account, we've got currency, lets
       * behave like professionals now, instead of the shenanigans
       * above. */
      if (gnc_commodity_equiv(currency, base_currency)) {
        value = gnc_numeric_add_fixed(value, s->value);
      } 
      else if (gnc_commodity_equiv(security, base_currency)) {
        value = gnc_numeric_add_fixed(value, s->damount);
      } 
      else {
        PERR ("inconsistent currencies\n");      
        printf("base = '%s', curr='%s', sec='%s'\n",
               gnc_commodity_get_printname(base_currency),
               gnc_commodity_get_printname(currency),
               gnc_commodity_get_printname(security));
        assert (0);
      }
    }
  }

  return value;
}

gnc_numeric
xaccTransGetImbalance (Transaction * trans)
{
  const gnc_commodity * currency;

  if (!trans)
    return gnc_numeric_zero ();

  currency = xaccTransFindCommonCurrency (trans);
  return xaccSplitsComputeValue (trans->splits, NULL, currency);
}

/********************************************************************\
\********************************************************************/
gboolean
xaccIsCommonCurrency(const gnc_commodity * currency_1, 
                     const gnc_commodity * security_1,
                     const gnc_commodity * currency_2, 
                     const gnc_commodity * security_2)
{
  int c1c2, c1s2, s1c2, s1s2;

  if ((currency_1 == NULL) || (currency_2 == NULL))
    return FALSE;

  c1c2 = !gnc_commodity_equiv(currency_1, currency_2);
  c1s2 = !gnc_commodity_equiv(currency_1, security_2);

  if (security_1 != NULL)
  {
    s1c2 = !gnc_commodity_equiv(security_1, currency_2);
    s1s2 = !gnc_commodity_equiv(security_1, security_2);
  }
  else /* no match */
  {
    s1c2 = 0;
    s1s2 = 0;
  }

  return (c1c2 == 1) || (c1s2 == 1) || (s1c2 == 1) || (s1s2 == 1);
}

static const gnc_commodity *
FindCommonExclSCurrency (GList *splits, const gnc_commodity * ra, 
                         const gnc_commodity * rb, Split *excl_split)
{
  GList *node;

  if (!splits) return NULL;

  for (node = splits; node; node = node->next)
  {
    Split *s = node->data;
    const gnc_commodity * sa, * sb;

    if (s == excl_split)
      continue;

    /* Novice/casual users may not want or use the double entry 
     * features of this engine.   Because of this, there
     * may be the occasional split without a parent account. 
     * Well, that's ok,  we'll just go with the flow. 
     */
    if (force_double_entry)
       assert (s->acc);
    else if (s->acc == NULL)
      continue;

    sa = xaccAccountGetCurrency (s->acc);
    sb = xaccAccountGetSecurity (s->acc);

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
static const gnc_commodity *
FindCommonCurrency (GList *splits,
                    const gnc_commodity * ra, const gnc_commodity * rb)
{
  return FindCommonExclSCurrency(splits, ra, rb, NULL);
}

const gnc_commodity *
xaccTransFindCommonCurrency (Transaction *trans)
{
  const gnc_commodity * ra, * rb;
  Split *split;

  if (!trans) return NULL;

  if (trans->splits == NULL) return NULL;

  split = trans->splits->data;

  if (split->acc == NULL) return NULL;

  ra = xaccAccountGetCurrency (split->acc);
  rb = xaccAccountGetSecurity (split->acc);

  return FindCommonCurrency (trans->splits, ra, rb);
}

const gnc_commodity *
xaccTransIsCommonCurrency (Transaction *trans, const gnc_commodity * ra)
{
  if (!trans) return NULL;
  return FindCommonCurrency (trans->splits, ra, NULL);
}

const gnc_commodity *
xaccTransIsCommonExclSCurrency (Transaction *trans, 
				const gnc_commodity * ra, 
                                Split *excl_split)
{
  if (!trans) return NULL;
  return FindCommonExclSCurrency (trans->splits, ra, NULL, excl_split);
}

/********************************************************************\
\********************************************************************/

G_INLINE_FUNC void check_open (Transaction *trans);
G_INLINE_FUNC void
check_open (Transaction *trans)
{
  if (!trans->open)
  {
    PERR ("transaction %p not open for editing\n", trans);
    /* assert (trans->open); */
    PERR ("\t%s:%d \n", __FILE__, __LINE__);
  }
}

void
xaccTransBeginEdit (Transaction *trans)
{
   char open;
   Backend *be;

   assert (trans);
   open = trans->open;
   trans->open = BEGIN_EDIT;

   if (open & BEGIN_EDIT) return;

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
   GList *node;
   Split *split;
   Backend *be;

   if (!trans) return;
   ENTER ("trans addr=%p\n", trans);
   check_open (trans);

   /* At this point, we check to see if we have a valid transaction.
    * As a result of editing, we could end up with a transaction that
    * has no splits in it, in which case we delete the transaction and
    * return.  
    */
   if (!trans->splits || (trans->open & BEING_DESTROYED))
   {
      PINFO ("delete trans at addr=%p\n", trans);
      /* Make a log in the journal before destruction.  */
      xaccTransWriteLog (trans, 'D');
      xaccRemoveEntity(&trans->guid);
      xaccFreeTransaction (trans);
      return;
   }

   split = trans->splits->data;

   /* try to get the sorting order lined up according to 
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
       (!gnc_numeric_zero_p(split->damount))) {
     Split * s = xaccMallocSplit();
     xaccTransAppendSplit (trans, s);
     xaccAccountInsertSplit (split->acc, s);
     xaccSplitSetMemo (s, split->memo);
     xaccSplitSetAction (s, split->action);
     xaccSplitSetShareAmount(s, gnc_numeric_neg(split->damount));
     xaccSplitSetValue(s, gnc_numeric_neg(split->value));
   }

   /* ------------------------------------------------- */
   /* OK, at this point, we are done making sure that 
    * we've got a validly constructed transaction.
    * Next, we send it off to the back-end, to see if the
    * back-end will accept it.
    */

   /* See if there's a backend.  If there is, invoke it. */
   be = xaccTransactionGetBackend (trans);
   if (be && be->trans_commit_edit) {
      int rc = 0;
      rc = (be->trans_commit_edit) (be, trans, trans->orig);

      if (rc) {
         /* if the backend puked, then we must roll-back 
          * at this point, and let the user know that we failed.
          */
        /* hack alert -- finish this */
      }
   }

   /* ------------------------------------------------- */
   /* Make sure all associated splits are in proper order
    * in their accounts. */
   for (node = trans->splits; node; node = node->next) {
     split = node->data;
     xaccAccountFixSplitDateOrder(split->acc, split);
   }

   /* Recompute the account balances. */
   for (node = trans->splits; node; node = node->next) {
     split = node->data;
     xaccAccountRecomputeBalance (split->acc); 
   }

   trans->open = 0;
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
   Transaction *orig;
   int force_it=0, mismatch=0;
   int i;

   if (!trans) return;

   check_open (trans);

   ENTER ("trans addr=%p\n", trans);

   /* copy the original values back in. */
   orig = trans->orig;

   /* If the transaction had been deleted before the rollback,
    * the guid would have been unlisted. Restore that */
   xaccStoreEntity(trans, &trans->guid, GNC_ID_TRANS);

   g_cache_remove (gnc_string_cache, trans->num);
   trans->num = orig->num;
   orig->num = g_cache_insert(gnc_string_cache, "");

   g_cache_remove (gnc_string_cache, trans->description);
   trans->description = orig->description;
   orig->description = g_cache_insert(gnc_string_cache, "");

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
   if (trans->open & BEING_DESTROYED)
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

         g_cache_remove (gnc_string_cache, s->action);
         s->action = so->action;
         so->action = g_cache_insert(gnc_string_cache, "");

         g_cache_remove (gnc_string_cache, s->memo);
         s->memo = so->memo;
         so->memo = g_cache_insert(gnc_string_cache, "");

         kvp_frame_delete (s->kvp_data);
         s->kvp_data = so->kvp_data;
         if (!s->kvp_data)
           s->kvp_data = kvp_frame_new ();
         so->kvp_data = kvp_frame_new ();

         s->reconciled  = so->reconciled;
         s->damount     = so->damount;
         s->value       = so->value;

         s->date_reconciled.tv_sec  = so->date_reconciled.tv_sec;
         s->date_reconciled.tv_nsec = so->date_reconciled.tv_nsec;

         /* do NOT check date order until all of the other fields 
          * have been properly restored */
         xaccAccountFixSplitDateOrder (s->acc, s); 
         xaccAccountRecomputeBalance (s->acc);
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

         mark_split (s);
         xaccAccountRemoveSplit (s->acc, s);
         xaccAccountRecomputeBalance (s->acc);
         xaccRemoveEntity(&s->guid);
         xaccFreeSplit (s);
      }

      g_list_free (trans->splits);

      trans->splits = orig->splits;
      orig->splits = NULL;

      for (node = g_list_nth (trans->splits, mismatch) ;
           node ; node = node->next)
      {
         Split *s = node->data;
         Account *account = s->acc;

         s->acc = NULL;
         xaccStoreEntity(s, &s->guid, GNC_ID_SPLIT);
         xaccAccountInsertSplit (account, s);
         xaccAccountRecomputeBalance (account);
         mark_split (s);
      }
   }

   xaccTransWriteLog (trans, 'R');

   xaccFreeTransaction (trans->orig);

   trans->orig = NULL;
   trans->open = 0;

   LEAVE ("trans addr=%p\n", trans);
}

gboolean
xaccTransIsOpen (Transaction *trans)
{
  if (!trans) return FALSE;
  return (0 != (trans->open & BEGIN_EDIT));
}

/********************************************************************\
\********************************************************************/

void
xaccTransDestroy (Transaction *trans)
{
  GList *node;

  if (!trans) return;
  check_open (trans);
  trans->open |= BEING_DESTROYED;
  xaccTransWriteLog (trans, 'D');

  gnc_engine_generate_event (&trans->guid, GNC_EVENT_DESTROY);

  for (node = trans->splits; node; node = node->next)
  {
    Split *split = node->data;

    mark_split (split);

    xaccAccountRemoveSplit (split->acc, split);
    xaccAccountRecomputeBalance (split->acc); 
    xaccRemoveEntity(&split->guid);
    xaccFreeSplit (split);

    node->data = NULL;
  }

  g_list_free (trans->splits);
  trans->splits = NULL;

  xaccRemoveEntity(&trans->guid);

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

   xaccRemoveEntity (&split->guid);

   if (trans)
   {
     gboolean ismember = (g_list_find (trans->splits, split) != NULL);
     assert (ismember);
   }

   mark_split (split);

   if (trans)
     xaccTransRemoveSplit (trans, split);

   xaccAccountRemoveSplit (split->acc, split);
   xaccAccountRecomputeBalance (split->acc);

   xaccFreeSplit (split);
}

/********************************************************************\
\********************************************************************/

void
xaccTransAppendSplit (Transaction *trans, Split *split) 
{
   Transaction *oldtrans;

   if (!trans) return;
   if (!split) return;

   check_open (trans);

   /* first, make sure that the split isn't already inserted 
    * elsewhere. If so, then remove it. */
   oldtrans = split->parent;
   if (oldtrans)
      xaccTransRemoveSplit (oldtrans, split);

   /* now, insert the split into the array */
   split->parent = trans;
   trans->splits = g_list_append (trans->splits, split);
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
  comp = gnc_numeric_compare(sa->damount, sb->damount);
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
xaccTransSetDateTS (Transaction *trans, const Timespec *ts)
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

void
xaccTransSetDateToday (Transaction *trans)
{
   struct timeval tv;
   gettimeofday (&tv, NULL);
   xaccTransSetDateInternal(trans, TDATE_POSTED, tv.tv_sec, tv.tv_usec);
}


/********************************************************************\
\********************************************************************/

void
xaccTransSetNum (Transaction *trans, const char *xnum)
{
   char * tmp;
   if (!trans || !xnum) return;
   check_open (trans);

   tmp = g_cache_insert(gnc_string_cache, (gpointer) xnum);
   g_cache_remove(gnc_string_cache, trans->num);
   trans->num = tmp;
   mark_trans (trans);
}

void
xaccTransSetDescription (Transaction *trans, const char *desc)
{
   char * tmp;
   if (!trans || !desc) return;
   check_open (trans);

   tmp = g_cache_insert(gnc_string_cache, (gpointer) desc);
   g_cache_remove(gnc_string_cache, trans->description);
   trans->description = tmp;
   mark_trans (trans);
}

void
xaccTransSetNotes (Transaction *trans, const char *notes)
{
  kvp_value *new_value;
  
  if (!trans || !notes) return;

  check_open (trans);

  new_value = kvp_value_new_string (notes);

  if (new_value)
  {
    kvp_frame_set_slot (xaccTransGetSlots (trans), "notes", new_value);
    kvp_value_delete (new_value);
    mark_trans (trans);
  }
  else
  {
    PERR ("failed to allocate kvp");
  }
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

long long 
xaccTransGetDateL (Transaction *trans)
{
   if (!trans) return 0;
   return (trans->date_posted.tv_sec);
}

void
xaccTransGetDateTS (Transaction *trans, Timespec *ts)
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

   tmp = g_cache_insert(gnc_string_cache, (gpointer) memo);
   g_cache_remove(gnc_string_cache, split->memo);
   split->memo = tmp;
   mark_split (split);
}

void
xaccSplitSetAction (Split *split, const char *actn)
{
   char * tmp;
   if (!split || !actn) return;

   tmp = g_cache_insert(gnc_string_cache, (gpointer) actn);
   g_cache_remove(gnc_string_cache, split->action);
   split->action = tmp;
   mark_split (split);
}

void
xaccSplitSetReconcile (Split *split, char recn)
{
   if (!split) return;

   switch (recn)
   {
     case NREC:
     case CREC:
     case YREC:
     case FREC:
       break;
     default:
       PERR("Bad reconciled flag");
       return;
   }

   split->reconciled = recn;

   xaccAccountRecomputeBalance (split->acc);
   mark_split (split);
}

void
xaccSplitSetDateReconciledSecs (Split *split, time_t secs)
{
   if (!split) return;

   split->date_reconciled.tv_sec = secs;
   split->date_reconciled.tv_nsec = 0;
   mark_split (split);
}

void
xaccSplitSetDateReconciledTS (Split *split, Timespec *ts)
{
   if (!split || !ts) return;

   split->date_reconciled = *ts;
   mark_split (split);
}

void
xaccSplitGetDateReconciledTS (Split * split, Timespec *ts)
{
   if (!split || !ts) return;
   *ts = (split->date_reconciled);
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

Account *
xaccSplitGetAccount (Split *split)
{
   if (!split) return NULL;
   return (split->acc);
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
  return gnc_numeric_to_double(xaccSplitGetShareAmount(split));
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
xaccSplitGetShareAmount (Split * split)
{
  if (!split) return gnc_numeric_zero();
  return split->damount;
}

gnc_numeric
xaccSplitGetValue (Split * split) {
  if (!split) return gnc_numeric_zero();
  return split->value; 
}

gnc_numeric
xaccSplitGetSharePrice (Split * split) {
  if(!split || gnc_numeric_zero_p(split->damount)) {
    return gnc_numeric_create(PRICE_DENOM, PRICE_DENOM);
  }
  return gnc_numeric_div(split->value, 
                         split->damount,
                         PRICE_DENOM, GNC_RND_ROUND);
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

/* reconfgure a split to be a stock split - after this, you shouldn't
   mess with the value, just the damount. */
void
xaccSplitMakeStockSplit(Split *s)
{
  kvp_frame *frame = xaccSplitGetSlots(s);

  xaccSplitSetValue(s, gnc_numeric_zero());
  kvp_frame_set_slot(frame,
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

     acc = s->acc;
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

     acc = s->acc;
     if (acc) break;
   }
   
   if (!acc) return NULL;

   return xaccGetPeerAccountFromFullName (acc, name, separator);
}

/********************************************************************\
\********************************************************************/

Split *
xaccGetOtherSplit (Split *split)
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

/************************ END OF ************************************\
\************************* FILE *************************************/
