/********************************************************************\
 * policy.c -- Implement FIFO Accounting Policy                     *
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

/** @file policy.c
 *  @breif Implement FIFO Accounting Policy.
 *  @author Created by Linas Vepstas August 2003
 *  @author Copyright (c) 2003 Linas Vepstas <linas@linas.org>
 *
 *  This file implements the FIFO Accounting Policy (and, in the 
 *  future, others as well).  The Accounting Polciy determines 
 *  how splits are assigned to lots.  
 */

#include "config.h"

#include <glib.h>

#include "Account.h"
#include "Transaction.h"
#include "TransactionP.h"
#include "cap-gains.h"
#include "gnc-engine.h"
#include "gnc-lot.h"
#include "gnc-lot-p.h"
#include "gnc-trace.h"
#include "policy.h"
#include "policy-p.h"

/* static short module = MOD_LOT; */

/* ============================================================== */

GNCLot * 
FIFOPolicyGetLot (Split *split, gpointer user_data)
{
   return xaccAccountFindEarliestOpenLot (split->acc, split->amount);
}

Split * 
FIFOPolicyGetSplit (GNCLot *lot, gpointer user_data)
{
   SplitList *node;
   gboolean want_positive;

   want_positive = gnc_numeric_negative_p (gnc_lot_get_balance (lot));

   /* Make use of the fact that the splits in a lot are already
    * in date order; so we don't have to search for the earliest. */
   for (node = xaccAccountGetSplitList (lot->account); node; node=node->next)
   {
      gboolean is_positive;
      Split *split = node->data;
      if (split->lot) continue;

      is_positive = gnc_numeric_positive_p (split->amount);
      if ((want_positive && is_positive) ||
          ((!want_positive) && (!is_positive))) return split;
   }
   return NULL;
}

void
FIFOPolicyGetLotOpening (GNCLot *lot,
        gnc_numeric *ret_amount, gnc_numeric *ret_value,
        gnc_commodity **ret_currency,
        gpointer user_data)
{
   Split *opening_split;
   opening_split = gnc_lot_get_earliest_split(lot);

   if (ret_amount) *ret_amount = opening_split->amount;
   if (ret_value) *ret_value = opening_split->value;
   if (ret_currency) *ret_currency = opening_split->parent->common_currency;
}

gboolean
FIFOPolicyIsOpeningSplit (GNCLot *lot, Split *split, gpointer user_data)
{
   Split *opening_split;
   opening_split = gnc_lot_get_earliest_split(lot);
   return (split == opening_split);
}


/* =========================== END OF FILE ======================= */
