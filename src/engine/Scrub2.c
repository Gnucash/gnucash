/********************************************************************\
 * Scrub2.c -- Convert Stock Accounts to use Lots                   *
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

/** @file Scrub2.c
 *  @breif Utilities to Convert Stock Accounts to use Lots
 *  @author Created by Linas Vepstas March 2003
 *  @author Copyright (c) 2003 Linas Vepstas <linas@linas.org>
 *
 * Provides a set of functions and utilities for checking and
 * repairing ('scrubbing clean') the usage of Lots and lot balances
 * in stock and commodity accounts.  Broken lots are repaired using
 * a first-in, first-out (FIFO) accounting schedule.
 */

#include "config.h"

#include <glib.h>

#include "Account.h"
#include "AccountP.h"
#include "Group.h"
#include "GroupP.h"
#include "Transaction.h"
#include "TransactionP.h"
#include "Scrub2.h"
#include "ScrubP.h"
#include "cap-gains.h"
#include "gnc-engine.h"
#include "gnc-engine-util.h"
#include "gnc-lot.h"
#include "gnc-lot-p.h"
#include "kvp-util-p.h"
#include "messages.h"

static short module = MOD_LOT;

/* ============================================================== */

void
xaccAccountScrubLots (Account *acc)
{
   SplitList *node;

   if (!acc) return;

   ENTER ("acc=%s", acc->accountName);
   xaccAccountBeginEdit (acc);

   /* Loop over all splits, and make sure that every split
    * belongs to some lot.  If a split does not belong to 
    * any lots, its is placed into the earliest possible
    * lot (thus enforcing FIFO accounting rules).
    */
restart_loop:
   for (node=acc->splits; node; node=node->next)
   {
      Split * split = node->data;

		/* If already in lot, then no-op */
		if (split->lot) continue;
		if (xaccSplitFIFOAssignToLot (split)) goto restart_loop;
   }
   xaccAccountCommitEdit (acc);
   LEAVE ("acc=%s", acc->accountName);
}


/* ============================================================== */

void
xaccAccountScrubDoubleBalance (Account *acc)
{
   LotList *node;

   if (!acc) return;

   ENTER ("acc=%s", acc->accountName);
   for (node = acc->lots; node; node=node->next)
   {
      GNCLot *lot = node->data;

      /* We examine only closed lots */
      if (FALSE == gnc_lot_is_closed (lot)) continue;
      xaccLotScrubDoubleBalance (lot);
   }
   LEAVE ("acc=%s", acc->accountName);
}

/* ============================================================== */

void
xaccLotScrubDoubleBalance (GNCLot *lot)
{
   gnc_commodity *currency = NULL;
   SplitList *snode;
   gnc_numeric zero = gnc_numeric_zero();
   gnc_numeric value = zero;

   if (!lot) return;

   /* We examine only closed lots */
   if (FALSE == gnc_lot_is_closed (lot)) return;

   ENTER ("lot=%s", kvp_frame_get_string (gnc_lot_get_slots (lot), "/title"));

   for (snode = lot->splits; snode; snode=snode->next)
   {
      Split *s = snode->data;
		xaccSplitComputeCapGains (s, NULL);
   }

   for (snode = lot->splits; snode; snode=snode->next)
   {
      Split *s = snode->data;
      Transaction *trans = s->parent;

      /* Check to make sure all splits in the lot have a common currency */
      if (NULL == currency)
      {
         currency = trans->common_currency;
      }
      if (FALSE == gnc_commodity_equiv (currency, trans->common_currency))
      {
			/* This lot has mixed currencies. Can't double-balance.
			 * Silently punt */
         PWARN ("Lot with multiple currencies:\n"
               "\ttrans=%s curr=%s\n", xaccTransGetDescription(trans), 
               gnc_commodity_get_fullname(trans->common_currency)); 
			break;
      }

      /* Now, total up the values */
      value = gnc_numeric_add (value, xaccSplitGetValue (s),
                           GNC_DENOM_AUTO, GNC_DENOM_LCD);
      PINFO ("Split value=%s Accum Lot value=%s", 
          gnc_numeric_to_string (xaccSplitGetValue(s)),
          gnc_numeric_to_string (value));
          
		if (FALSE == gnc_numeric_equal (value, zero))
		{
			/* Unhandled error condition. Not sure what to do here,
			 * Since the ComputeCapGains should have gotten it right. */
			PERR ("Closed lot fails to double-balance !!\n");
		}
   }

   LEAVE ("lot=%s", kvp_frame_get_string (gnc_lot_get_slots (lot), "/title"));
}

/* ============================================================== */

static gpointer 
lot_scrub_cb (Account *acc, gpointer data)
{
   if (FALSE == xaccAccountHasTrades (acc)) return NULL;
   xaccAccountScrubLots (acc);
   xaccAccountScrubDoubleBalance (acc);
   return NULL;
}

void 
xaccGroupScrubLotsBalance (AccountGroup *grp)
{
   if (!grp) return;
   xaccGroupForEachAccount (grp, lot_scrub_cb, NULL, TRUE);
}

void 
xaccAccountScrubLotsBalance (Account *acc)
{
   if (!acc) return;
   if (FALSE == xaccAccountHasTrades (acc)) return;
   xaccAccountScrubLots (acc);
   xaccAccountScrubDoubleBalance (acc);
}

void 
xaccAccountTreeScrubLotsBalance (Account *acc)
{
   if (!acc) return;

   xaccGroupScrubLotsBalance (acc->children);
   
   if (FALSE == xaccAccountHasTrades (acc)) return;
   xaccAccountScrubLots (acc);
   xaccAccountScrubDoubleBalance (acc);
}

/* =========================== END OF FILE ======================= */
