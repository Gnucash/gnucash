/********************************************************************\
 * Scrub3.c -- Constrain Cap Gains to Track Sources of Gains        *
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
\********************************************************************/

/*  @file Scrub3.c
 *  @breif Constrain Cap Gains to Track Sources of Gains
 *  @author Created by Linas Vepstas Sept 2003
 *  @author Copyright (c) 2003 Linas Vepstas <linas@linas.org>
 *
 * Provides a set of functions and utilities for checking and
 * repairing ('scrubbing clean') the usage of Cap Gains
 * transactions in stock and commodity accounts.  
 */

#include "config.h"

#include <glib.h>

#include "cap-gains.h"
#include "gnc-commodity.h"
#include "gnc-engine.h"
#include "gnc-lot.h"
#include "gnc-numeric.h"
#include "gnc-trace.h"
#include "kvp_frame.h"
#include "kvp-util-p.h"
#include "policy-p.h"
#include "Account.h"
#include "AccountP.h"
#include "Group.h"
#include "Scrub2.h"
#include "Scrub3.h"
#include "Transaction.h"
#include "TransactionP.h"

static short module = MOD_LOT;

/* ================================================================= */

gboolean
xaccScrubLot (GNCLot *lot)
{
  gboolean splits_deleted = FALSE;
  gnc_numeric lot_baln;
  gboolean opening_baln_is_pos, lot_baln_is_pos;
  Account *acc;
  GNCPolicy *pcy;

  if (!lot) return FALSE;
  ENTER (" ");

  acc = gnc_lot_get_account (lot);
  pcy = acc->policy;
  xaccAccountBeginEdit(acc);
  xaccScrubMergeLotSubSplits (lot);

  /* If the lot balance is zero, we don't need to rebalance */
  lot_baln = gnc_lot_get_balance (lot);
  if (! gnc_numeric_zero_p (lot_baln))
  {
    SplitList *node;
    gnc_numeric opening_baln;

    /* Get the opening balance for this lot */
    pcy->PolicyGetLotOpening (pcy, lot, &opening_baln, NULL, NULL);

    /* If the lot is fat, give the boot to all the non-opening 
     * splits, and refill it */
    opening_baln_is_pos = gnc_numeric_positive_p(opening_baln);
    lot_baln_is_pos = gnc_numeric_positive_p(lot_baln);
    if ((opening_baln_is_pos || lot_baln_is_pos) &&
        ((!opening_baln_is_pos) || (!lot_baln_is_pos)))
    {
rethin:
      for (node=gnc_lot_get_split_list(lot); node; node=node->next)
      {
        Split *s = node->data;
        if (pcy->PolicyIsOpeningSplit (pcy, lot, s)) continue;
        gnc_lot_remove_split (lot, s);
        goto rethin;
      }
    }

    /* At this point the lot is thin, so try to fill it */
    xaccLotFill (lot);

    /* Make sure there are no subsplits. */
    splits_deleted = xaccScrubMergeLotSubSplits (lot);
  }

  /* Now re-compute cap gains, and then double-check that. */
  xaccLotComputeCapGains (lot, NULL);
  xaccLotScrubDoubleBalance (lot);
  xaccAccountCommitEdit(acc);

  LEAVE (" deleted=%d", splits_deleted);
  return splits_deleted;
}

/* ============================================================== */

void
xaccAccountScrubLots (Account *acc)
{
  LotList *node;
  if (!acc) return;
  if (FALSE == xaccAccountHasTrades (acc)) return;
                                                                                
  ENTER ("acc=%s", acc->accountName);
  xaccAccountBeginEdit(acc);
  xaccAccountAssignLots (acc);

  for (node = acc->lots; node; node=node->next)
  {
    GNCLot *lot = node->data;
    xaccScrubLot (lot);
  }
  xaccAccountCommitEdit(acc);
  LEAVE ("acc=%s", acc->accountName);
}

/* ============================================================== */

static gpointer 
lot_scrub_cb (Account *acc, gpointer data)
{
   if (FALSE == xaccAccountHasTrades (acc)) return NULL;
   xaccAccountScrubLots (acc);
   return NULL;
}

void 
xaccGroupScrubLots (AccountGroup *grp)
{
   if (!grp) return;
   xaccGroupForEachAccount (grp, lot_scrub_cb, NULL, TRUE);
}

void 
xaccAccountTreeScrubLots (Account *acc)
{
   if (!acc) return;

   xaccGroupScrubLots (acc->children);
   xaccAccountScrubLots (acc);
}

/* ========================== END OF FILE  ========================= */
