/********************************************************************\
 * Scrub.c -- convert single-entry accounts into clean double-entry *
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

/*
 * FILE:
 * Scrub.c
 *
 * FUNCTION:
 * Provides a set of functions and utilities for scrubbing clean 
 * single-entry accounts so that they can be promoted into 
 * self-consistent, clean double-entry accounts.
 *
 * HISTORY:
 * Created by Linas Vepstas December 1998
 * Copyright (c) 1998-2000, 2003 Linas Vepstas <linas@linas.org>
 * Copyright (c) 2002 Christian Stimming
 */

#include "config.h"

#include <glib.h>
#include <stdio.h>
#include <string.h>

#include "Account.h"
#include "AccountP.h"
#include "Group.h"
#include "GroupP.h"
#include "Scrub.h"
#include "ScrubP.h"
#include "Transaction.h"
#include "TransactionP.h"
#include "gnc-engine-util.h"
#include "messages.h"
#include "gnc-commodity.h"
#include "gnc-trace.h"

static short module = MOD_SCRUB;

/* ================================================================ */

void
xaccGroupScrubOrphans (AccountGroup *grp)
{
  GList *list;
  GList *node;

  if (!grp) return;

  list = xaccGroupGetAccountList (grp);

  for (node = list; node; node = node->next)
  {
    Account *account = node->data;

    xaccAccountTreeScrubOrphans (account);
  }
}

void
xaccAccountTreeScrubOrphans (Account *acc)
{
  if (!acc) return;

  xaccGroupScrubOrphans (xaccAccountGetChildren(acc));
  xaccAccountScrubOrphans (acc);
}

static void
TransScrubOrphansFast (Transaction *trans, AccountGroup *root)
{
  GList *node;

  if (!trans) return;
  g_return_if_fail (root);

  for (node = trans->splits; node; node = node->next)
  {
    Split *split = node->data;
    Account *orph;

    if (split->acc) continue;

    DEBUG ("Found an orphan \n");

    orph = xaccScrubUtilityGetOrMakeAccount (root, trans->common_currency, _("Orphan"));
    if (!orph) continue;

    xaccAccountBeginEdit (orph);
    xaccAccountInsertSplit (orph, split);
    xaccAccountCommitEdit (orph);
  }
}

void
xaccAccountScrubOrphans (Account *acc)
{
  GList *node;
  const char *str;

  if (!acc) return;

  str = xaccAccountGetName (acc);
  str = str ? str : "(null)";
  PINFO ("Looking for orphans in account %s \n", str);

  for (node = xaccAccountGetSplitList(acc); node; node = node->next)
  {
    Split *split = node->data;

    TransScrubOrphansFast (xaccSplitGetParent (split),
                           xaccAccountGetRoot (acc));
  }
}


void
xaccTransScrubOrphans (Transaction *trans)
{
  SplitList *node;
  for (node = trans->splits; node; node = node->next)
  {
    Split *split = node->data;

    if (split->acc)
    {
      TransScrubOrphansFast (trans, xaccAccountGetRoot(split->acc));
      break;
    }
  }
}

/* ================================================================ */

void
xaccGroupScrubSplits (AccountGroup *group)
{
  GList *list;
  GList *node;

  if (!group) return;

  list = xaccGroupGetAccountList (group);

  for (node = list; node; node = node->next)
  {
    Account *account = node->data;

    xaccAccountTreeScrubSplits (account);
  }
}

void
xaccAccountTreeScrubSplits (Account *account)
{
  xaccGroupScrubSplits (xaccAccountGetChildren(account));
  xaccAccountScrubSplits (account);
}

void
xaccAccountScrubSplits (Account *account)
{
  GList *node;

  for (node = xaccAccountGetSplitList (account); node; node = node->next)
    xaccSplitScrub (node->data);
}

void
xaccTransScrubSplits (Transaction *trans)
{
  GList *node;

  if (!trans)
    return;

  for (node = trans->splits; node; node = node->next)
    xaccSplitScrub (node->data);
}

void
xaccSplitScrub (Split *split)
{
  Account *account;
  Transaction *trans;
  gnc_numeric value;
  gnc_commodity *currency;
  int scu;

  if (!split) return;

  trans = xaccSplitGetParent (split);
  if (!trans) return;

  account = xaccSplitGetAccount (split);

  /* If theres no account, this split is an orphan.
   * We need to fix that first, before proceeding.
   */
  if (!account)
  {
    xaccTransScrubOrphans (trans);
    account = xaccSplitGetAccount (split);
  }

  /* Grrr... the register gnc_split_register_load() line 203 of
   *  src/register/ledger-core/split-register-load.c will create
   * free-floating bogus transactions. Ignore these for now ... 
   */
  if (!account) 
  {
    PINFO ("Free Floating Transaction!");
    return;  
  }

  currency = xaccTransGetCurrency (trans);

  /* If the account doesn't have a commodity, 
   * we should attempt to fix that first. 
  */
  if (!account->commodity)
  {
    xaccAccountScrubCommodity (account);
  }
  if (!account->commodity || !gnc_commodity_equiv (account->commodity, currency))
    return;

  scu = MIN (xaccAccountGetCommoditySCU (account),
             gnc_commodity_get_fraction (currency));

  value = xaccSplitGetValue (split);

  if (gnc_numeric_same (xaccSplitGetAmount (split),
                        value, scu, GNC_HOW_RND_ROUND))
  {
    return;
  }

  /*
   * This will be hit every time you answer yes to the dialog "The
   * current transaction has changed. Would you like to record it.
   */
  PINFO ("Adjusted split with mismatched values, desc=\"%s\" memo=\"%s\"" 
         " old amount %s %s, new amount %s",
            trans->description, split->memo,
            gnc_numeric_to_string (xaccSplitGetAmount(split)),
            gnc_commodity_get_mnemonic (currency),
            gnc_numeric_to_string (xaccSplitGetValue(split)));

  xaccTransBeginEdit (trans);
  xaccSplitSetAmount (split, value);
  xaccTransCommitEdit (trans);
}

/* ================================================================ */

void
xaccGroupScrubImbalance (AccountGroup *grp)
{
  GList *list;
  GList *node;

  if (!grp) return;

  list = xaccGroupGetAccountList (grp);

  for (node = list; node; node = node->next)
  {
    Account *account = node->data;

    xaccAccountTreeScrubImbalance (account);
  }
}

void
xaccAccountTreeScrubImbalance (Account *acc)
{
  xaccGroupScrubImbalance (xaccAccountGetChildren(acc));
  xaccAccountScrubImbalance (acc);
}

void
xaccAccountScrubImbalance (Account *acc)
{
  GList *node;
  const char *str;

  if (!acc) return;

  str = xaccAccountGetName(acc);
  str = str ? str : "(null)";
  PINFO ("Looking for imbalance in account %s \n", str);

  for(node = xaccAccountGetSplitList(acc); node; node = node->next)
  {
    Split *split = node->data;
    Transaction *trans = xaccSplitGetParent(split);

    xaccTransScrubImbalance (trans, xaccAccountGetRoot (acc), NULL);
  }
}

void
xaccTransScrubImbalance (Transaction *trans, AccountGroup *root,
                         Account *parent)
{
  Split *balance_split = NULL;
  gnc_numeric imbalance;
  Account *account;
  SplitList *node, *slist;

  if (!trans) return;

  xaccTransScrubSplits (trans);

  /* If the transaction is balanced, nothing more to do */
  imbalance = xaccTransGetImbalance (trans);
  if (gnc_numeric_zero_p (imbalance)) return;

  slist = xaccTransGetSplitList (trans);
  if (!slist) return;

  if (!parent)
  {
    if (!root) 
    { 
       Split *s = slist->data; 
       root = xaccAccountGetRoot (s->acc);
    }
    account = xaccScrubUtilityGetOrMakeAccount (root, 
        trans->common_currency, _("Imbalance"));
  }
  else
  {
    account = parent;
  }

  if (!account) 
  {
      PERR ("Can't get balancing account");
      return;
  }

  for (node = slist; node; node = node->next)
  {
    Split *split = node->data;

    if (xaccSplitGetAccount (split) == account)
    {
      balance_split = split;
      break;
    }
  }

  /* Put split into account before setting split value */
  if (!balance_split)
  {
    balance_split = xaccMallocSplit (trans->inst.book);

    xaccAccountBeginEdit (account);
    xaccAccountInsertSplit (account, balance_split);
    xaccAccountCommitEdit (account);
  }

  PINFO ("unbalanced transaction");

  {
    const gnc_commodity *currency;
    const gnc_commodity *commodity;
    gnc_numeric old_value, new_value;

    xaccTransBeginEdit (trans);

    currency = xaccTransGetCurrency (trans);
    old_value = xaccSplitGetValue (balance_split);

    /* Note: We have to round for the commodity's fraction, NOT any
     * already existing denominator (bug #104343), because either one
     * of the denominators might already be reduced.  */
    new_value = gnc_numeric_sub (old_value, imbalance,
             gnc_commodity_get_fraction(currency), 
             GNC_HOW_RND_ROUND);

    xaccSplitSetValue (balance_split, new_value);

    commodity = xaccAccountGetCommodity (account);
    if (gnc_commodity_equiv (currency, commodity))
    {
      xaccSplitSetAmount (balance_split, new_value);
    }

    xaccTransAppendSplit (trans, balance_split);
    xaccSplitScrub (balance_split);
    xaccTransCommitEdit (trans);
  }
}

/* ================================================================ */
/* The xaccTransFindCommonCurrency () method returns
 *    a gnc_commodity indicating a currency denomination that all
 *    of the splits in this transaction have in common, using the
 *    old/obsolete currency/security fields of the split accounts.  
 */

static gnc_commodity *
FindCommonExclSCurrency (SplitList *splits,
                         gnc_commodity * ra, gnc_commodity * rb,
                         Split *excl_split)
{
  GList *node;

  if (!splits) return NULL;

  for (node = splits; node; node = node->next)
  {
    Split *s = node->data;
    gnc_commodity * sa, * sb;

    if (s == excl_split) continue;

    g_return_val_if_fail (s->acc, NULL);

    sa = DxaccAccountGetCurrency (s->acc);
    sb = xaccAccountGetCommodity (s->acc);

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
FindCommonCurrency (GList *splits, gnc_commodity * ra, gnc_commodity * rb)
{
  return FindCommonExclSCurrency(splits, ra, rb, NULL);
}

static gnc_commodity *
xaccTransFindOldCommonCurrency (Transaction *trans, QofBook *book)
{
  gnc_commodity *ra, *rb, *retval;
  Split *split;

  if (!trans) return NULL;

  if (trans->splits == NULL) return NULL;

  g_return_val_if_fail (book, NULL);

  split = trans->splits->data;

  if (!split || NULL == split->acc) return NULL;

  ra = DxaccAccountGetCurrency (split->acc);
  rb = xaccAccountGetCommodity (split->acc);

  retval = FindCommonCurrency (trans->splits, ra, rb);

  /* Compare this value to what we think should be the 'right' value */
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
     /* In every situation I can think of, this routine should return 
      * common currency.  So make note of this ... */
     PWARN ("unable to find a common currency, and that is strange.");
  }

  return retval;
}

/* ================================================================ */

void
xaccTransScrubCurrency (Transaction *trans)
{
  SplitList *node;
  gnc_commodity *currency;

  if (!trans) return;

  /* If there are any orphaned splits in a transaction, then the 
   * this routine will fail.  Therefore, we want to make sure that
   * tehre are no orphans (splits without parent account).
   */
  xaccTransScrubOrphans (trans);

  currency = xaccTransGetCurrency (trans);
  if (currency) return;
  
  currency = xaccTransFindOldCommonCurrency (trans, trans->inst.book);
  if (currency)
  {
    xaccTransBeginEdit (trans);
    xaccTransSetCurrency (trans, currency);
    xaccTransCommitEdit (trans);
  }
  else
  {
    if (NULL == trans->splits)
    {
      PWARN ("Transaction \"%s\" has no splits in it!", trans->description);
    }
    else
    {
      SplitList *node;
      PWARN ("no common transaction currency found for trans=\"%s\"", trans->description);
      for (node=trans->splits; node; node=node->next)
      {
        Split *split = node->data;
        if (NULL == split->acc)
        {
          PWARN (" split=\"%s\" is not in any account!", split->memo);
        }
        else
        {
          PWARN (" split=\"%s\" account=\"%s\" commodity=\"%s\"", 
              split->memo, split->acc->accountName, gnc_commodity_get_mnemonic (split->acc->commodity));
        }
      }
    }
  }

  for (node=trans->splits; node; node=node->next)
  {
    Split *sp = node->data;

    if (!gnc_numeric_equal(xaccSplitGetAmount (sp), 
        xaccSplitGetValue (sp))) 
    {
      gnc_commodity *acc_currency = xaccAccountGetCommodity (sp->acc);
      if (acc_currency == currency) 
      {
        /* This Split needs fixing: The transaction-currency equals
         * the account-currency/commodity, but the amount/values are
         * inequal i.e. they still correspond to the security
         * (amount) and the currency (value). In the new model, the
         * value is the amount in the account-commodity -- so it
         * needs to be set to equal the amount (since the
         * account-currency doesn't exist anymore). 
         *
         * Note: Nevertheless we lose some information here. Namely,
         * the information that the 'amount' in 'account-old-security'
         * was worth 'value' in 'account-old-currency'. Maybe it would
         * be better to store that information in the price database? 
         * But then, for old currency transactions there is still the
         * 'other' transaction, which is going to keep that
         * information. So I don't bother with that here. -- cstim,
         * 2002/11/20. */
          
        PWARN ("Adjusted split with mismatched values, desc=\"%s\" memo=\"%s\"" 
               " old amount %s %s, new amount %s",
               trans->description, sp->memo,
               gnc_numeric_to_string (xaccSplitGetAmount(sp)),
               gnc_commodity_get_mnemonic (currency),
               gnc_numeric_to_string (xaccSplitGetValue(sp)));
        xaccTransBeginEdit (trans);
        xaccSplitSetAmount (sp, xaccSplitGetValue(sp));
        xaccTransCommitEdit (trans);
      }
      /*else 
      {
        PINFO ("Ok: Split '%s' Amount %s %s, value %s %s",
        xaccSplitGetMemo (sp),
        gnc_numeric_to_string (amount),
        gnc_commodity_get_mnemonic (currency),
        gnc_numeric_to_string (value),
        gnc_commodity_get_mnemonic (acc_currency));
      }*/
    }
  }
}

/* ================================================================ */

void
xaccAccountScrubCommodity (Account *account)
{
  gnc_commodity *commodity;

  if (!account) return;

  commodity = xaccAccountGetCommodity (account);
  if (commodity) return;

  /* Use the 'obsolete' routines to try to figure out what the
   * account commodity should have been. */
  commodity = DxaccAccountGetSecurity (account);
  if (commodity)
  {
    xaccAccountSetCommodity (account, commodity);
    return;
  }

  commodity = DxaccAccountGetCurrency (account);
  if (commodity)
  {
    xaccAccountSetCommodity (account, commodity);
    return;
  }

  PERR ("Account \"%s\" does not have a commodity!", account->accountName);
}

/* ================================================================ */

static void
xaccAccountDeleteOldData (Account *account)
{
  if (!account) return;

  kvp_frame_set_slot_nc (account->inst.kvp_data, "old-currency", NULL);
  kvp_frame_set_slot_nc (account->inst.kvp_data, "old-security", NULL);
  kvp_frame_set_slot_nc (account->inst.kvp_data, "old-currency-scu", NULL);
  kvp_frame_set_slot_nc (account->inst.kvp_data, "old-security-scu", NULL);
}

static int
scrub_trans_currency_helper (Transaction *t, gpointer data)
{
  xaccTransScrubCurrency (t);
  return 0;
}

static gpointer
scrub_account_commodity_helper (Account *account, gpointer data)
{
  xaccAccountScrubCommodity (account);
  xaccAccountDeleteOldData (account);
  return NULL;
}

void
xaccGroupScrubCommodities (AccountGroup *group)
{
  if (!group) return;

  xaccAccountGroupBeginEdit (group);

  xaccGroupForEachTransaction (group, scrub_trans_currency_helper, NULL);

  xaccGroupForEachAccount (group, scrub_account_commodity_helper,
                           NULL, TRUE);

  xaccAccountGroupCommitEdit (group);
}

/* ================================================================ */

static gboolean
check_quote_source (gnc_commodity *com, gpointer data)
{
  gboolean *commodity_has_quote_src = (gboolean *)data;
  if (com && !gnc_commodity_is_iso(com))
    *commodity_has_quote_src |= gnc_commodity_get_quote_flag(com);
  return TRUE;
}

static gpointer
move_quote_source (Account *account, gpointer data)
{
  gnc_commodity *com;
  gnc_quote_source *quote_source;
  gboolean new_style = GPOINTER_TO_INT(data);
  const char *source, *tz;

  com = xaccAccountGetCommodity(account);
  if (!com)
    return NULL;

  if (!new_style) {
    source = dxaccAccountGetPriceSrc(account);
    if (!source || !*source)
      return NULL;
    tz = dxaccAccountGetQuoteTZ(account);

    PINFO("to %8s from %s", gnc_commodity_get_mnemonic(com),
          xaccAccountGetName(account));
    gnc_commodity_set_quote_flag(com, TRUE);
    quote_source = gnc_quote_source_lookup_by_internal(source);
    if (!quote_source)
      quote_source = gnc_quote_source_add_new(source, FALSE);
    gnc_commodity_set_quote_source(com, quote_source);
    gnc_commodity_set_quote_tz(com, tz);
  }

  dxaccAccountSetPriceSrc(account, NULL);
  dxaccAccountSetQuoteTZ(account, NULL);
  return NULL;
}


void
xaccGroupScrubQuoteSources (AccountGroup *group, gnc_commodity_table *table)
{
  gboolean new_style = FALSE;
  ENTER(" ");

  if (!group || !table) {
    LEAVE("Oops")
    return;
  }

  gnc_commodity_table_foreach_commodity (table, check_quote_source, &new_style);

  xaccAccountGroupBeginEdit (group);
  xaccGroupForEachAccount (group, move_quote_source,
                           GINT_TO_POINTER(new_style), TRUE);
  xaccAccountGroupCommitEdit (group);
  LEAVE("Migration done");
}

/* ================================================================ */

Account *
xaccScrubUtilityGetOrMakeAccount (AccountGroup *root, gnc_commodity * currency,
                  const char *name_root)
{
  char * accname;
  Account * acc;

  g_return_val_if_fail (root, NULL);

  /* build the account name */
  if (!currency)
  {
    PERR ("No currency specified!");
    return NULL;
  }

  accname = g_strconcat (name_root, "-",
                         gnc_commodity_get_mnemonic (currency), NULL);

  /* See if we've got one of these going already ... */
  acc = xaccGetAccountFromName (root, accname);

  if (acc == NULL)
  {
    /* Guess not. We'll have to build one. */
    acc = xaccMallocAccount (root->book);
    xaccAccountBeginEdit (acc);
    xaccAccountSetName (acc, accname);
    xaccAccountSetCommodity (acc, currency);
    xaccAccountSetType (acc, BANK);

    /* Hang the account off the root. */
    xaccGroupInsertAccount (root, acc);
    xaccAccountCommitEdit (acc);
  }

  g_free (accname);

  return acc;
}

/* ==================== END OF FILE ==================== */
