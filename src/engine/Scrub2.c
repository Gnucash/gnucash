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

#if 0
void
xaccAccountScrubLots (Account *acc)
{
   if (!acc) return;

   for (node=acc->splits; node; node=node->next)
   {
   	Split * s = node->data;
      GNCLot *lot = s->lot;

      
      gnc_lot_is_closed (lot);
	}
}
#endif

/* =========================== END OF FILE ======================= */
