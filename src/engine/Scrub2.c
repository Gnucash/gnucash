/********************************************************************\
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

 * XXX under construction, just started, not done
 *
 * Provides a set of functions and utilities for checking and
 * repairing ('scrubbing clean') stock and commodity accounts
 * to use Lots & accounting schedules so that books can be closed.
 *
 */

#include "AccountP.h"
#include "Transaction.h"
#include "TransactionP.h"
#include "Scrub2.h"
#include "gnc-engine.h"
#include "gnc-lot.h"

/* ============================================================== */

gboolean 
xaccAccountHasTrades (Account *acc)
{
   gnc_commodity *acc_comm;
   SplitList *node;

   if (!acc) return FALSE;

   acc_comm = acc->commodity;

   for (node=acc->splits; node; node=node->next)
   {
      Split *s = node->data;
      Transaction *t = s->parent;
      if (acc_comm != t->common_currency) return TRUE;
   }

   return FALSE;
}

/* ============================================================== */

struct early_lot_s
{
   GNCLot *lot;
   Timespec ts;
   int (*numeric_pred)(gnc_numeric);
};

static gpointer earliest_helper (GNCLot *lot,  gpointer user_data)
{
   struct early_lot_s *els = user_data;
   Split *s;
   Transaction *trans;
   gnc_numeric bal;

   if (gnc_lot_is_closed (lot)) return NULL;

   /* We want a lot whose balance is of the correct sign */
   bal = gnc_lot_get_balance (lot);
   if (0 == (els->numeric_pred) (bal)) return NULL;
   
   s = gnc_lot_get_earliest_split (lot);
   trans = s->parent;
   if ((els->ts.tv_sec > trans->date_posted.tv_sec)  ||
       ((els->ts.tv_sec == trans->date_posted.tv_sec) &&
        (els->ts.tv_nsec > trans->date_posted.tv_nsec)))
   {
      els->ts = trans->date_posted;
      els->lot = lot;
   }
   
   return NULL;
}

GNCLot *
xaccAccountFindEarliestOpenLot (Account *acc, gnc_numeric sign)
{
   struct early_lot_s es;

   es.lot = NULL;
   es.ts.tv_sec = 10000000LL * ((long long) LONG_MAX);
   es.ts.tv_nsec = 0;

   if (gnc_numeric_positive_p(sign)) es.numeric_pred = gnc_numeric_positive_p;
   else es.numeric_pred = gnc_numeric_negative_p;
      
   xaccAccountForEachLot (acc, earliest_helper, &es);
   return es.lot;
}

/* ============================================================== */

void
xaccAccountScrubLots (Account *acc)
{
   SplitList *node;

   if (!acc) return;

   /* Loop over all splits, and make sure that every split
    * belongs to some lot.  If a split does not belong to 
    * any lots, its is placed into the earliest possible
    * lot (thus enforcing FIFO accounting rules).
    */
   for (node=acc->splits; node; node=node->next)
   {
      Split * split = node->data;
      GNCLot *lot = split->lot;

      /* If this split belongs to a lot, its good. */
      if (lot) continue;

      /* If we are here, this split does not belong to any lot.
       * Lets put it in the earliest one we can find.  This 
       * block is written in the form of a while loop, since we
       * may have to bust a split across several lots.
       */
     while (split)
     {
        lot = xaccAccountFindEarliestOpenLot (acc, split->amount);
        if (lot)
        {
           /* If the amount is smaller than open balance ... */
           gnc_numeric baln = gnc_lot_get_balance (lot);
           int cmp = gnc_numeric_compare (split->amount, baln);

           /* cmp == +1 if amt > baln */
           if (0 < cmp) 
           {
              Split * new_split;
              gnc_numeric amt_a, amt_b, amt_tot;
              gnc_numeric val_a, val_b, val_tot;
              Transaction *trans;
              Timespec ts;

              trans = split->parent;
              xaccTransBeginEdit (trans);

              amt_tot = split->amount;
              amt_a = gnc_numeric_neg (baln);
              amt_b = gnc_numeric_sub_fixed (amt_tot, amt_a);

              /* Compute the value so that it holds in the same proportion:
               * i.e. so that (amt_a / amt_tot) = (val_a / val_tot)
               */
              val_tot = split->value;
              val_a = gnc_numeric_mul (amt_a, val_tot, GNC_DENOM_AUTO, GNC_RND_NEVER);
              val_a = gnc_numeric_div (val_a, amt_tot, gnc_numeric_denom(val_tot), GNC_DENOM_EXACT);

              val_b = gnc_numeric_sub_fixed (val_tot, val_a);
        
              xaccSplitSetAmount (split, amt_a);
              xaccSplitSetValue (split, val_a);

              /* Adding this split will have the effect of closing this lot,
               * because the new balance should be precisely zero. */
              gnc_lot_add_split (lot, split);

              /* put the remainder of teh balance into a new split, which is
               * in other respects just a clone of this one */
              /* XXX FIXME: we should add some kvp markup to indicate that these
               * two splits used to be one before being 'split' */
              new_split = xaccMallocSplit (acc->book);

              /* Copy most of teh split attributes */
              xaccSplitSetMemo (new_split, xaccSplitGetMemo (split));
              xaccSplitSetAction (new_split, xaccSplitGetAction (split));
              xaccSplitSetReconcile (new_split, xaccSplitGetReconcile (split));
              ts = xaccSplitRetDateReconciledTS (split);
              xaccSplitSetDateReconciledTS (new_split, &ts);

              /* Copying the KVP tree seems like the right thing to do, 
               * this is potentially dangerous, depending on how other 
               * users use it.*/
              xaccSplitSetSlots_nc (new_split, kvp_frame_copy(xaccSplitGetSlots (split)));  

              xaccSplitSetAmount (new_split, amt_b);
              xaccSplitSetValue (new_split, val_b);
              
              xaccAccountInsertSplit (acc, new_split);
              xaccTransAppendSplit (trans, new_split);
              xaccTransCommitEdit (trans);
              split = new_split;
           }
           else
           {
              gnc_lot_add_split (lot, split);
              split = NULL;
           }
        }
        else
        {
           /* No lot was found.  Start a new lot */
           lot = gnc_lot_new (acc->book);
           gnc_lot_add_split (lot, split);
           split = NULL;
        }
      }
   }
}

/* =========================== END OF FILE ======================= */
