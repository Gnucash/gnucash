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

#include "gnc-commodity.h"
#include "gnc-engine.h"
#include "gnc-numeric.h"
#include "gnc-trace.h"
#include "kvp_frame.h"
#include "kvp-util-p.h"
#include "Account.h"
#include "Scrub3.h"
#include "Transaction.h"
#include "TransactionP.h"

static short module = MOD_LOT;

/* ================================================================= */

static KvpFrame *
is_subsplit (Split *split)
{
   KvpValue *kval;
   KvpFrame *ksub;

   if (!split) return NULL;
   g_return_val_if_fail (split->parent, NULL);

   /* If there are no sub-splits, then there's nothing to do. */
   kval = kvp_frame_get_slot (split->kvp_data, "lot-split");
   if (!kval) return NULL;  

   ksub = kvp_value_get_frame (kval);
   g_return_val_if_fail (ksub, NULL);

   return ksub;
}

/* ================================================================= */

void
xaccScrubSubSplitPrice (Split *split)
{
   gnc_numeric src_amt, src_val;
   SplitList *node;

   if (NULL == is_subsplit (split)) return;

   ENTER (" ");
   /* Get 'price' of the indicated split */
   src_amt = xaccSplitGetAmount (split);
   src_val = xaccSplitGetValue (split);

   /* Loop over splits, adjust each so that it has the same
    * ratio (i.e. price).  Change the value to get things 
    * right; do not change the amount */
   for (node=split->parent->splits; node; node=node->next)
   {
      Split *s = node->data;
      Transaction *txn = s->parent;
      gnc_numeric dst_amt, dst_val, target_val;
      gnc_numeric delta;
      int scu;

      /* Skip the reference split */
      if (s == split) continue;

      scu = gnc_commodity_get_fraction (txn->common_currency);

      dst_amt = xaccSplitGetAmount (s);
      dst_val = xaccSplitGetValue (s);
      target_val = gnc_numeric_mul (dst_amt, src_val,
                        GNC_DENOM_AUTO, GNC_DENOM_REDUCE);
      target_val = gnc_numeric_div (target_val, src_amt,
                        scu, GNC_DENOM_EXACT);

      /* If the required price changes are 'small', do nothing.
       * That is a case that the user will have to deal with
       * manually.  This routine is really intended only for
       * a gross level of synchronization.
       */
      delta = gnc_numeric_sub_fixed (target_val, dst_val);
      delta = gnc_numeric_abs (delta);
      if (3 * delta.num  < delta.denom) continue;

      /* If the amount is small, pass on that too */
      if ((-2 < dst_amt.num) && (dst_amt.num < 2)) continue;

      /* Make the actual adjustment */
      xaccTransBeginEdit (txn);
      xaccSplitSetValue (s, target_val);
      xaccTransCommitEdit (txn);
   }
   LEAVE (" ");
}

/* ================================================================= */

/* Remove the guid of b from a */
static void
remove_guids (Split *sa, Split *sb)
{
   KvpFrame *ksub;

   /* Find and remove the matching guid's */
   ksub = gnc_kvp_bag_find_by_guid (sa->kvp_data, "lot-split",
                    "peer_guid", &sb->guid);
   if (!ksub) 
   {
      PERR ("merging splits that didn't have correct gemini values!");
      return;
   }
   gnc_kvp_bag_remove_frame (sa->kvp_data, "lot-split", ksub);
   kvp_frame_delete (ksub);
}

/* The 'merge_splits() routine causes the amount & value of sb 
 * to be merged into sa; it then destroys sb.  It also performs
 * some other misc cleanup */

static void
merge_splits (Split *sa, Split *sb)
{
   Account *act;
   Transaction *txn;
   gnc_numeric amt, val;

   act = xaccSplitGetAccount (sb);
   xaccAccountBeginEdit (act);

   txn = sa->parent;
   xaccTransBeginEdit (txn);

   /* Remove the guid of sb from the 'gemini' of sa */
   remove_guids (sa, sb);

   /* Add amount of sb into sa, ditto for value. */
   amt = xaccSplitGetAmount (sa);
   amt = gnc_numeric_add_fixed (amt, xaccSplitGetAmount (sb));
   xaccSplitSetAmount (sa, amt);

   val = xaccSplitGetValue (sa);
   val = gnc_numeric_add_fixed (val, xaccSplitGetValue (sb));
   xaccSplitSetValue (sa, val);

   /* Set reconcile to no; after this much violence, 
    * no way its reconciled. */
   xaccSplitSetReconcile (sa, NREC);

   /* Finally, delete sb */
   xaccSplitDestroy(sb);

   xaccTransCommitEdit (txn);
   xaccAccountCommitEdit (act);
}

gboolean 
xaccScrubMergeSubSplits (Split *split)
{
   gboolean rc = FALSE;
   Transaction *txn;
   KvpFrame *sf;
   SplitList *node;
   GNCLot *lot;

   sf = is_subsplit (split);
   if (!sf) return FALSE;

   txn = split->parent;
   lot = xaccSplitGetLot (split);

   ENTER (" ");
restart:
   for (node=txn->splits; node; node=node->next)
   {
      Split *s = node->data;
      if (xaccSplitGetLot (s) != lot) continue;

      /* OK, this split is in the same lot (and thus same account)
       * as the indicated split.  It must be a subsplit (although
       * we should double-check the kvp's to be sure).  Merge the
       * two back together again. */
      merge_splits (split, s);
      rc = TRUE;
      goto restart;
   }
   LEAVE (" splits merged=%d", rc);
   return rc;
}

gboolean 
xaccScrubMergeTxnSubSplits (Transaction *txn)
{
   gboolean rc = FALSE;
   SplitList *node;

   if (!txn) return FALSE;

   ENTER (" ");
restart:
   for (node=txn->splits; node; node=node->next)
   {
      Split *s = node->data;
      if (!xaccScrubMergeSubSplits(s)) continue;

      rc = TRUE;
      goto restart;
   }
   LEAVE (" splits merged=%d", rc);
   return rc;
}

/* ========================== END OF FILE  ========================= */
