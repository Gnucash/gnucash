/********************************************************************\
 * gnc-lot.c -- AR/AP invoices; inventory lots; stock lots          *
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

/*
 * FILE:
 * gnc-lot.c
 * 
 * FUNCTION:
 * Lots implement the fundamental conceptual idea behind invoices,
 * inventory lots, and stock market investment lots.  See the file
 * src/doc/lots.txt for implmentation overview.
 *
 * HISTORY:
 * Created by Linas Vepstas May 2002
 * Copyright (c) 2002 Linas Vepstas <linas@linas.org>
 */

#include "Account.h"
#include "gnc-book-p.h"
#include "gnc-engine-util.h"
#include "gnc-event.h"
#include "gnc-event-p.h"
#include "gnc-lot.h"
#include "gnc-lot-p.h"
#include "Transaction.h"
#include "TransactionP.h"

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_LOT;

/* ============================================================= */

static void
gnc_lot_init (GNCLot *lot, GNCBook *book)
{
   ENTER ("(lot=%p, book=%p)", lot, book);
   lot->kvp_data = NULL;
   lot->account = NULL;
   lot->splits = NULL;
   lot->is_closed = -1;
  
   lot->book = book;
   xaccGUIDNew (&lot->guid, book);
   xaccStoreEntity (book->entity_table, lot, &lot->guid, GNC_ID_LOT);
   LEAVE ("(lot=%p, book=%p)", lot, book);
}

GNCLot *
gnc_lot_new (GNCBook *book)
{
   GNCLot *lot;
   g_return_val_if_fail (book, NULL);

   lot = g_new (GNCLot, 1);
   gnc_lot_init (lot, book);
   return lot;
}

void 
gnc_lot_destroy (GNCLot *lot)
{
   GList *node;
   if (!lot) return;
   
	ENTER ("(lot=%p)", lot);
   gnc_engine_generate_event (&lot->guid, GNC_EVENT_DESTROY);

   xaccRemoveEntity (lot->book->entity_table, &lot->guid);
   
   for (node=lot->splits; node; node=node->next)
   {
      Split *s = node->data;
      s->lot = NULL;
   }
   g_list_free (lot->splits);
   
   kvp_frame_delete (lot->kvp_data);
   lot->kvp_data = NULL;
   
   lot->account = NULL;
   lot->is_closed = TRUE;

   g_free (lot);
}

/* ============================================================= */

const GUID * 
gnc_lot_get_guid (GNCLot *lot)
{
   if (!lot) return NULL;
   return &lot->guid;
}

void
gnc_lot_set_guid (GNCLot *lot, GUID uid)
{
   if (!lot) return;

   if (guid_equal (&lot->guid, &uid)) return;

   xaccRemoveEntity(lot->book->entity_table, &lot->guid);
   lot->guid = uid;
   xaccStoreEntity(lot->book->entity_table, lot, &lot->guid, GNC_ID_LOT);
}

GNCLot *
gnc_lot_lookup (const GUID *guid, GNCBook *book)
{
  if (!guid || !book) return NULL;
  return xaccLookupEntity (gnc_book_get_entity_table (book),
                                          guid, GNC_ID_LOT);
}

GNCBook *
gnc_lot_get_book (GNCLot *lot)
{
  if (!lot) return NULL;
  return lot->book;
}


/* ============================================================= */

gboolean 
gnc_lot_is_closed (GNCLot *lot)
{
   if (!lot) return TRUE;
   if (0 > lot->is_closed) gnc_lot_get_balance (lot);
   return lot->is_closed;
}

Account *
gnc_lot_get_account (GNCLot *lot)
{
   if (!lot) return NULL;
   return lot->account;
}

kvp_frame *
gnc_lot_get_slots (GNCLot *lot)
{
   if (!lot) return NULL;
   return lot->kvp_data;
}

SplitList *
gnc_lot_get_split_list (GNCLot *lot)
{
   if (!lot) return NULL;
   return lot->splits;
}

/* ============================================================= */

gnc_numeric
gnc_lot_get_balance (GNCLot *lot)
{
   GList *node;
   gnc_numeric zero = gnc_numeric_zero();
   gnc_numeric baln = zero;
   if (!lot) return zero;

   if (!lot->splits) return zero;

   /* Sum over splits; because they all belong to same account
    * they will have same denominator. 
    */
   for (node=lot->splits; node; node=node->next)
   {
      Split *s = node->data;
      gnc_numeric amt = xaccSplitGetAmount (s);
      baln = gnc_numeric_add (baln, amt, GNC_DENOM_AUTO, GNC_DENOM_FIXED);
   }

   /* cache a zero balance as a closed lot */
   if (gnc_numeric_equal (baln, zero))
   {
      lot->is_closed = TRUE;
   }
   else
   {
      lot->is_closed = FALSE;
   }
   
   return baln;
}

/* ============================================================= */

void
gnc_lot_add_split (GNCLot *lot, Split *split)
{
   Account * acc;
   if (!lot || !split) return;

	ENTER ("(lot=%p, split=%p)", lot, split);
   acc = xaccSplitGetAccount (split);
   if (NULL == lot->account)
   {
      xaccAccountInsertLot (acc, lot);
   }
   else if (lot->account != acc)
   {
      PERR ("splits from different accounts cannot "
            "be added to this lot!\n"
            "\tlot account=\'%s\', split account=\'%s\'\n",
            xaccAccountGetName(lot->account), xaccAccountGetName (acc));
      return;
   }

   if (split->lot)
   {
      gnc_lot_remove_split (split->lot, split);
   }
   split->lot = lot;

   lot->splits = g_list_append (lot->splits, split);
}

void
gnc_lot_remove_split (GNCLot *lot, Split *split)
{
   if (!lot || !split) return;

	ENTER ("(lot=%p, split=%p)", lot, split);
   lot->splits = g_list_remove (lot->splits, split);
   split->lot = NULL;

   if (NULL == lot->splits)
   {
      lot->account = NULL;
      lot->is_closed = FALSE;
   }
}

/* ========================== END OF FILE ========================= */
