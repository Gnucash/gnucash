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
 * Copyright (c) 1998, 1999, 2000 Linas Vepstas
 */

#include <glib.h>
#include <stdio.h>
#include <string.h>

#include "Account.h"
#include "Group.h"
#include "GroupP.h"
#include "Scrub.h"
#include "Transaction.h"
#include "TransactionP.h"
#include "gnc-engine-util.h"
#include "messages.h"

static short module = MOD_SCRUB;
static Account * GetOrMakeAccount (Account *, Transaction *, const char *);

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
  xaccGroupScrubOrphans (xaccAccountGetChildren(acc));
  xaccAccountScrubOrphans (acc);
}

void
xaccAccountScrubOrphans (Account *acc)
{
  GList *slp;
  Transaction *trans;
  Account * parent;

  PINFO ("Looking for orphans in account %s \n", xaccAccountGetName(acc));

  for(slp = xaccAccountGetSplitList(acc); slp; slp = slp->next) {
    Split *split = (Split *) slp->data;
    Split * tsplit;
    int j = 0;

    trans = xaccSplitGetParent (split);
    tsplit = xaccTransGetSplit (trans, 0);
    while (tsplit) {
      parent = xaccSplitGetAccount (tsplit);
      if (!parent) {
        Account *orph;
        DEBUG ("Found an orphan \n");
        /* OK, we found an orphan.  Put it in an orphan account. */
        orph = GetOrMakeAccount (acc, trans, _("Orphan"));
        xaccAccountBeginEdit (orph);
        xaccAccountInsertSplit (orph, tsplit);
        xaccAccountCommitEdit (orph);
      }
      j++; 
      tsplit = xaccTransGetSplit (trans, j);
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
  gboolean trans_was_open;
  int scu;

  if (!split)
    return;

  trans = xaccSplitGetParent (split);
  if (!trans)
    return;

  account = xaccSplitGetAccount (split);
  if (!account)
    return;

  if (!gnc_commodity_equiv (xaccAccountGetCurrency (account),
                            xaccAccountGetEffectiveSecurity (account)))
    return;

  scu = MIN (xaccAccountGetCurrencySCU (account),
             xaccAccountGetSecuritySCU (account));

  if (gnc_numeric_same (xaccSplitGetShareAmount (split),
                        xaccSplitGetValue (split),
                        scu, GNC_RND_ROUND))
    return;

  PINFO ("split with mismatched values: %s",
         guid_to_string (xaccSplitGetGUID (split)));

  trans_was_open = xaccTransIsOpen (trans);

  if (!trans_was_open)
    xaccTransBeginEdit (trans, TRUE);

  xaccSplitSetShareAmount (split, xaccSplitGetValue (split));

  if (!trans_was_open)
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
  GList *slp;

  PINFO ("Looking for imbalance in account %s \n", xaccAccountGetName(acc));

  for(slp = xaccAccountGetSplitList(acc); slp; slp = slp->next)
  {
    Split *split = (Split *) slp->data;
    Transaction *trans = xaccSplitGetParent(split);

    xaccTransScrubImbalance (trans);
  }
}

void
xaccTransScrubImbalance (Transaction *trans)
{
  Split *balance_split = NULL;
  gnc_numeric imbalance;

  if (!trans)
    return;

  xaccTransScrubSplits (trans);

  {
    GList *node;
    Account *account;
    Account *peer = NULL;

    imbalance = xaccTransGetImbalance (trans);
    if (gnc_numeric_zero_p (imbalance))
      return;

    for (node = trans->splits; node; node = node->next)
    {
      Split *split = node->data;

      peer = xaccSplitGetAccount (split);
      if (peer)
        break;
    }

    if (!peer)
    {
      PERR ("Transaction with no accounts");
      return;
    }

    account = GetOrMakeAccount (peer, trans, _("Imbalance"));

    /* put split into account before setting split value */
    balance_split = xaccMallocSplit();

    xaccAccountBeginEdit (account);
    xaccAccountInsertSplit (account, balance_split);
    xaccAccountCommitEdit (account);
  }

  PINFO ("unbalanced transaction: %s",
         guid_to_string (xaccTransGetGUID (trans)));

  {
    const gnc_commodity *common_currency;
    const gnc_commodity *commodity;
    gboolean trans_was_open;
    Account *account;

    trans_was_open = xaccTransIsOpen (trans);

    if (!trans_was_open)
      xaccTransBeginEdit (trans, TRUE);

    common_currency = xaccTransFindCommonCurrency (trans);
    account = xaccSplitGetAccount (balance_split);

    commodity = xaccAccountGetCurrency (account);
    if (gnc_commodity_equiv (common_currency, commodity))
    {
      gnc_numeric new_value = xaccSplitGetValue (balance_split);

      new_value = gnc_numeric_sub_fixed (new_value, imbalance);

      xaccSplitSetValue (balance_split, new_value);
    }

    commodity = xaccAccountGetSecurity (account);
    if (gnc_commodity_equiv (common_currency, commodity))
    {
      gnc_numeric new_share_amount = xaccSplitGetShareAmount (balance_split);

      new_share_amount = gnc_numeric_sub_fixed (new_share_amount, imbalance);

      xaccSplitSetShareAmount (balance_split, new_share_amount);
    }

    xaccTransAppendSplit (trans, balance_split);

    if (!trans_was_open)
      xaccTransCommitEdit (trans);
  }
}

/* ================================================================ */

static Account *
GetOrMakeAccount (Account *peer, Transaction *trans, const char *name_root)
{
  const gnc_commodity * currency;
  AccountGroup *root;
  char * accname;
  Account * acc;

  /* build the account name */
  currency = xaccTransFindCommonCurrency (trans);

  accname = g_strconcat (name_root, "-",
                         gnc_commodity_get_mnemonic(currency), NULL);

  /* see if we've got one of these going already ... */
  acc = xaccGetPeerAccountFromName (peer, accname);

  if (acc == NULL)
  {
    /* guess not. We'll have to build one */
    acc = xaccMallocAccount ();
    xaccAccountBeginEdit (acc);
    xaccAccountSetName (acc, accname);
    xaccAccountSetCurrency (acc, currency);
    xaccAccountSetType (acc, BANK);

    /* hang the account off the root */
    root = xaccGetAccountRoot (peer);
    xaccGroupInsertAccount (root, acc);
    xaccAccountCommitEdit (acc);
  }

  g_free (accname);

  return acc;
}

/* ==================== END OF FILE ==================== */
