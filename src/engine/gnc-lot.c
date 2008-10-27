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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
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
 * XXX Lots are not currently treated in a correct transactional
 * manner.  There's now a per-Lot dirty flag in the QofInstance, but
 * this code still needs to emit the correct signals when a lot has
 * changed.  This is true both in the Scrub2.c and in
 * src/gnome/lot-viewer.c
 *
 * HISTORY:
 * Created by Linas Vepstas May 2002
 * Copyright (c) 2002,2003 Linas Vepstas <linas@linas.org>
 */

#include <glib.h>
#include <glib/gi18n.h>

#include "config.h"
#include "Account.h"
#include "AccountP.h"
#include "gnc-lot.h"
#include "gnc-lot-p.h"
#include "cap-gains.h"
#include "Transaction.h"
#include "TransactionP.h"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_LOT;

/* ============================================================= */

/* GObject Initialization */
QOF_GOBJECT_IMPL(gnc_lot, GNCLot, QOF_TYPE_INSTANCE);

static void
gnc_lot_init(GNCLot* lot)
{
}

static void
gnc_lot_dispose_real (GObject *lotp)
{
}

static void
gnc_lot_finalize_real(GObject* lotp)
{
}

static void
gnc_lot_init_data (GNCLot *lot, QofBook *book)
{
   ENTER ("(lot=%p, book=%p)", lot, book);
   lot->account = NULL;
   lot->splits = NULL;
   lot->is_closed = -1;
   lot->marker = 0;
  
   qof_instance_init_data(&lot->inst, GNC_ID_LOT, book);
   LEAVE ("(lot=%p, book=%p)", lot, book);
}

GNCLot *
gnc_lot_new (QofBook *book)
{
   GNCLot *lot;
   g_return_val_if_fail (book, NULL);

   lot = g_object_new (GNC_TYPE_LOT, NULL);
   gnc_lot_init_data (lot, book);
   qof_event_gen (&lot->inst, QOF_EVENT_CREATE, NULL);
   return lot;
}

static void
gnc_lot_free(GNCLot* lot)
{
   GList *node;
   if (!lot) return;
   
   ENTER ("(lot=%p)", lot);
   qof_event_gen (&lot->inst, QOF_EVENT_DESTROY, NULL);

   
   for (node=lot->splits; node; node=node->next)
   {
      Split *s = node->data;
      s->lot = NULL;
   }
   g_list_free (lot->splits);
   
   lot->account = NULL;
   lot->is_closed = TRUE;
   /* qof_instance_release (&lot->inst); */
   g_object_unref (lot);
}

void 
gnc_lot_destroy (GNCLot *lot)
{
   if (!lot) return;
   
   gnc_lot_begin_edit(lot);
   qof_instance_set_destroying(lot, TRUE);
   gnc_lot_commit_edit(lot);
}

/* ============================================================= */

void
gnc_lot_begin_edit (GNCLot *lot)
{
  qof_begin_edit(&lot->inst);
}

static void commit_err (QofInstance *inst, QofBackendError errcode)
{
  PERR ("Failed to commit: %d", errcode);
  gnc_engine_signal_commit_error( errcode );
}

static void lot_free(QofInstance* inst)
{
	GNCLot* lot = GNC_LOT(inst);

	gnc_lot_free(lot);
}

static void noop (QofInstance *inst) {}

void
gnc_lot_commit_edit (GNCLot *lot)
{
  if (!qof_commit_edit (QOF_INSTANCE(lot))) return;
  qof_commit_edit_part2 (&lot->inst, commit_err, noop, lot_free);
}

/* ============================================================= */

GNCLot *
gnc_lot_lookup (const GUID *guid, QofBook *book)
{
  QofCollection *col;
  if (!guid || !book) return NULL;
  col = qof_book_get_collection (book, GNC_ID_LOT);
  return (GNCLot *) qof_collection_lookup_entity (col, guid);
}

QofBook *
gnc_lot_get_book (GNCLot *lot)
{
    return qof_instance_get_book(QOF_INSTANCE(lot));
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
gnc_lot_get_account (const GNCLot *lot)
{
   if (!lot) return NULL;
   return lot->account;
}

KvpFrame *
gnc_lot_get_slots (const GNCLot *lot)
{
    return qof_instance_get_slots(QOF_INSTANCE(lot));
}

SplitList *
gnc_lot_get_split_list (const GNCLot *lot)
{
   if (!lot) return NULL;
   return lot->splits;
}

gint gnc_lot_count_splits (const GNCLot *lot)
{
   if (!lot) return 0;
   return g_list_length (lot->splits);
}

/* ============================================================== */
/* Hmm, we should probably inline these. */

const char * 
gnc_lot_get_title (const GNCLot *lot)
{
   if (!lot) return NULL;
   return kvp_frame_get_string (lot->inst.kvp_data, "/title");
}

const char * 
gnc_lot_get_notes (const GNCLot *lot)
{
   if (!lot) return NULL;
   return kvp_frame_get_string (lot->inst.kvp_data, "/notes");
}

void
gnc_lot_set_title (GNCLot *lot, const char *str)
{
   if (!lot) return;
   qof_begin_edit(QOF_INSTANCE(lot));
   qof_instance_set_dirty(QOF_INSTANCE(lot));
   kvp_frame_set_str (lot->inst.kvp_data, "/title", str);
   gnc_lot_commit_edit(lot);
}

void
gnc_lot_set_notes (GNCLot *lot, const char *str)
{
   if (!lot) return;
   gnc_lot_begin_edit(lot);
   qof_instance_set_dirty(QOF_INSTANCE(lot));
   kvp_frame_set_str (lot->inst.kvp_data, "/notes", str);
   gnc_lot_commit_edit(lot);
}

/* ============================================================= */

gnc_numeric
gnc_lot_get_balance (GNCLot *lot)
{
   GList *node;
   gnc_numeric zero = gnc_numeric_zero();
   gnc_numeric baln = zero;
   if (!lot) return zero;

   if (!lot->splits) 
   {
      lot->is_closed = FALSE;
      return zero;
   }

   /* Sum over splits; because they all belong to same account
    * they will have same denominator. 
    */
   for (node=lot->splits; node; node=node->next)
   {
      Split *s = node->data;
      gnc_numeric amt = xaccSplitGetAmount (s);
      baln = gnc_numeric_add_fixed (baln, amt);
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
gnc_lot_get_balance_before (const GNCLot *lot, const Split *split,
                            gnc_numeric *amount, gnc_numeric *value)
{
   GList *node;
   gnc_numeric zero = gnc_numeric_zero();
   gnc_numeric amt = zero;
   gnc_numeric val = zero;
   
   if (lot && lot->splits)
   {
      Transaction *ta, *tb;
      const Split *target;
      /* If this is a gains split, find the source of the gains and use
         its transaction for the comparison.  Gains splits are in separate
         transactions that may sort after non-gains transactions.  */
      target = xaccSplitGetGainsSourceSplit (split);
      if (target == NULL)
         target = split;
      tb = xaccSplitGetParent (target);
      for (node = lot->splits; node; node = node->next)
      {
         Split *s = node->data;
         Split *source = xaccSplitGetGainsSourceSplit (s);
         if (source == NULL)
            source = s;
         ta = xaccSplitGetParent (source);
         if ((ta == tb && source != target) ||
             xaccTransOrder (ta, tb) < 0)
         {
            gnc_numeric tmpval = xaccSplitGetAmount (s);
            amt = gnc_numeric_add_fixed (amt, tmpval);
            tmpval = xaccSplitGetValue (s);
            val = gnc_numeric_add_fixed (val, tmpval);
         }
      }
   }

   *amount = amt;
   *value = val;
}
               
/* ============================================================= */

void
gnc_lot_add_split (GNCLot *lot, Split *split)
{
   Account * acc;
   if (!lot || !split) return;

   ENTER ("(lot=%p, split=%p) %s amt=%s val=%s", lot, split,
        gnc_lot_get_title (lot), 
        gnc_num_dbg_to_string (split->amount),
        gnc_num_dbg_to_string (split->value));
   gnc_lot_begin_edit(lot);
   acc = xaccSplitGetAccount (split);
   qof_instance_set_dirty(QOF_INSTANCE(lot));
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
      gnc_lot_commit_edit(lot);
      LEAVE("different accounts");
      return;
   }

   if (lot == split->lot) {
        gnc_lot_commit_edit(lot);
        LEAVE("already in lot");
	return; /* handle not-uncommon no-op */
   }
   if (split->lot)
   {
      gnc_lot_remove_split (split->lot, split);
   }
   xaccSplitSetLot(split, lot);

   lot->splits = g_list_append (lot->splits, split);

    /* for recomputation of is-closed */
   lot->is_closed = -1;
   gnc_lot_commit_edit(lot);

   qof_event_gen (&lot->inst, QOF_EVENT_MODIFY, NULL);
   LEAVE("added to lot");
}

void
gnc_lot_remove_split (GNCLot *lot, Split *split)
{
   if (!lot || !split) return;

   ENTER ("(lot=%p, split=%p)", lot, split);
   gnc_lot_begin_edit(lot);
   qof_instance_set_dirty(QOF_INSTANCE(lot));
   lot->splits = g_list_remove (lot->splits, split);
   xaccSplitSetLot(split, NULL);
   lot->is_closed = -1;   /* force an is-closed computation */

   if (NULL == lot->splits)
   {
      xaccAccountRemoveLot (lot->account, lot);
      lot->account = NULL;
   }
   gnc_lot_commit_edit(lot);
   qof_event_gen (&lot->inst, QOF_EVENT_MODIFY, NULL);
}

/* ============================================================== */
/* Utility function, get earliest split in lot */

Split *
gnc_lot_get_earliest_split (GNCLot *lot)
{
  if (! lot->splits)
    return NULL;
  lot->splits = g_list_sort (lot->splits, (GCompareFunc) xaccSplitOrderDateOnly);
  return lot->splits->data;
}

Split *
gnc_lot_get_latest_split (GNCLot *lot)
{
  SplitList *node;

  if (! lot->splits)
    return NULL;
  lot->splits = g_list_sort (lot->splits, (GCompareFunc) xaccSplitOrderDateOnly);

  for (node = lot->splits; node->next; node = node->next)
    ;

  return node->data;
}

/* ============================================================= */

static QofObject gncLotDesc =
{
    interface_version:  QOF_OBJECT_VERSION,
    e_type:             GNC_ID_LOT,
    type_label:         "Lot",
    create:             (gpointer)gnc_lot_new,
    book_begin:         NULL,
    book_end:           NULL,
    is_dirty:           qof_collection_is_dirty,
    mark_clean:         qof_collection_mark_clean,
    foreach:            qof_collection_foreach,
    printable:          NULL,
    version_cmp:        (int (*)(gpointer,gpointer))qof_instance_version_cmp,
};


gboolean gnc_lot_register (void)
{
    static const QofParam params[] = {
        { LOT_TITLE, QOF_TYPE_STRING, 
          (QofAccessFunc) gnc_lot_get_title, 
          (QofSetterFunc) gnc_lot_set_title },
        { LOT_NOTES, QOF_TYPE_STRING, 
          (QofAccessFunc) gnc_lot_get_notes, 
          (QofSetterFunc) gnc_lot_set_notes },
        { QOF_PARAM_GUID, QOF_TYPE_GUID, 
          (QofAccessFunc) qof_entity_get_guid, NULL },
        { QOF_PARAM_BOOK, QOF_ID_BOOK, 
          (QofAccessFunc) gnc_lot_get_book, NULL },
        { LOT_IS_CLOSED, QOF_TYPE_BOOLEAN, 
          (QofAccessFunc) gnc_lot_is_closed, NULL },
        { LOT_BALANCE, QOF_TYPE_NUMERIC, 
          (QofAccessFunc) gnc_lot_get_balance, NULL },
        { NULL },
    };

    qof_class_register (GNC_ID_LOT, NULL, params);
    return qof_object_register(&gncLotDesc);
}

GNCLot * gnc_lot_make_default (Account *acc)
{
   GNCLot * lot;
   gint64 id;
   char buff[200];

   lot = gnc_lot_new (qof_instance_get_book(acc));

   /* Provide a reasonable title for the new lot */
   id = kvp_frame_get_gint64 (xaccAccountGetSlots (acc), "/lot-mgmt/next-id");
   snprintf (buff, 200, ("%s %" G_GINT64_FORMAT), _("Lot"), id);
   kvp_frame_set_str (gnc_lot_get_slots (lot), "/title", buff);
   id ++;
   kvp_frame_set_gint64 (xaccAccountGetSlots (acc), "/lot-mgmt/next-id", id);

   return lot;
}

/* ========================== END OF FILE ========================= */
