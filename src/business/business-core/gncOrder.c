/********************************************************************\
 * gncOrder.c -- the Core Business Order                            *
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
 *                                                                  *
\********************************************************************/

/*
 * Copyright (C) 2001,2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#include "config.h"

#include <glib.h>
#include <glib/gi18n.h>

#include "gncEntry.h"
#include "gncEntryP.h"
#include "gncOrder.h"
#include "gncOrderP.h"
#include "gncOwner.h"
#include "gncOwnerP.h"

struct _gncOrder 
{
  QofInstance inst;

  char *	id;
  char *	notes;
  gboolean 	active;

  char *	reference;
  char *	printname;
  GncOwner	owner;
  GList *	entries;
  Timespec 	opened;
  Timespec 	closed;
};

struct _gncOrderClass
{
  QofInstanceClass parent_class;
};

static QofLogModule log_module = GNC_MOD_BUSINESS;

#define _GNC_MOD_NAME	GNC_ID_ORDER

#define SET_STR(obj, member, str) { \
	char * tmp; \
	\
	if (!safe_strcmp (member, str)) return; \
	gncOrderBeginEdit (obj); \
	tmp = CACHE_INSERT (str); \
	CACHE_REMOVE (member); \
	member = tmp; \
	}

G_INLINE_FUNC void mark_order (GncOrder *order);
void mark_order (GncOrder *order)
{
  qof_instance_set_dirty(&order->inst);
  qof_event_gen (&order->inst, QOF_EVENT_MODIFY, NULL);
}

/* =============================================================== */
/* GObject Initialization */
QOF_GOBJECT_IMPL(gnc_order, GncOrder, QOF_TYPE_INSTANCE);

static void
gnc_order_init(GncOrder* order)
{
}

static void
gnc_order_dispose_real (GObject *orderp)
{
}

static void
gnc_order_finalize_real(GObject* orderp)
{
}

/* Create/Destroy Functions */
GncOrder *gncOrderCreate (QofBook *book)
{
  GncOrder *order;

  if (!book) return NULL;

  order = g_object_new (GNC_TYPE_ORDER, NULL);
  qof_instance_init_data (&order->inst, _GNC_MOD_NAME, book);

  order->id = CACHE_INSERT ("");
  order->notes = CACHE_INSERT ("");
  order->reference = CACHE_INSERT ("");

  order->active = TRUE;

  qof_event_gen (&order->inst, QOF_EVENT_CREATE, NULL);

  return order;
}

void gncOrderDestroy (GncOrder *order)
{
  if (!order) return;
  qof_instance_set_destroying(order, TRUE);
  gncOrderCommitEdit (order);
}

static void gncOrderFree (GncOrder *order)
{
  if (!order) return;

  qof_event_gen (&order->inst, QOF_EVENT_DESTROY, NULL);

  g_list_free (order->entries);
  CACHE_REMOVE (order->id);
  CACHE_REMOVE (order->notes);
  CACHE_REMOVE (order->reference);

  if (order->printname) g_free (order->printname);

  /* qof_instance_release (&order->inst); */
  g_object_unref (order);
}

GncOrder *
gncCloneOrder (GncOrder *from, QofBook *book)
{
  GList *node;
  GncOrder *order;

  if (!book) return NULL;

  order = g_object_new (GNC_TYPE_ORDER, NULL);
  qof_instance_init_data (&order->inst, _GNC_MOD_NAME, book);
  qof_instance_gemini (&order->inst, &from->inst);

  order->id = CACHE_INSERT (from->id);
  order->notes = CACHE_INSERT (from->notes);
  order->reference = CACHE_INSERT (from->reference);

  order->active = from->active;
  order->printname = NULL; /* yes, null, that's right */
  order->opened = from->opened;
  order->closed = from->closed;

  order->owner = gncCloneOwner (&from->owner, book);

  order->entries = NULL;
  for (node = g_list_last(from->entries); node; node=node->prev)
  {
    GncEntry *entry = node->data;
    entry = gncEntryObtainTwin (entry, book);
    order->entries = g_list_prepend (order->entries, entry);
  }

  qof_event_gen (&order->inst, QOF_EVENT_CREATE, NULL);

  return order;
}

GncOrder *
gncOrderObtainTwin (GncOrder *from, QofBook *book)
{
  GncOrder *order;
  if (!book) return NULL;

  order = (GncOrder *) qof_instance_lookup_twin (QOF_INSTANCE(from), book);
  if (!order)
  {
    order = gncCloneOrder (from, book);
  }

  return order;
}

/* =============================================================== */
/* Set Functions */

void gncOrderSetID (GncOrder *order, const char *id)
{
  if (!order || !id) return;
  SET_STR (order, order->id, id);
  mark_order (order);
  gncOrderCommitEdit (order);
}

void gncOrderSetOwner (GncOrder *order, GncOwner *owner)
{
  if (!order || !owner) return;
  if (gncOwnerEqual (&order->owner, owner)) return;

  gncOrderBeginEdit (order);
  gncOwnerCopy (owner, &order->owner);
  mark_order (order);
  gncOrderCommitEdit (order);
}

void gncOrderSetDateOpened (GncOrder *order, Timespec date)
{
  if (!order) return;
  if (timespec_equal (&order->opened, &date)) return;
  gncOrderBeginEdit (order);
  order->opened = date;
  mark_order (order);
  gncOrderCommitEdit (order);
}

void gncOrderSetDateClosed (GncOrder *order, Timespec date)
{
  if (!order) return;
  if (timespec_equal (&order->closed, &date)) return;
  gncOrderBeginEdit (order);
  order->closed = date;
  mark_order (order);
  gncOrderCommitEdit (order);
}

void gncOrderSetNotes (GncOrder *order, const char *notes)
{
  if (!order || !notes) return;
  SET_STR (order, order->notes, notes);
  mark_order (order);
  gncOrderCommitEdit (order);
}

void gncOrderSetReference (GncOrder *order, const char *reference)
{
  if (!order || !reference) return;
  SET_STR (order, order->reference, reference);
  mark_order (order);
  gncOrderCommitEdit (order);
}

void gncOrderSetActive (GncOrder *order, gboolean active)
{
  if (!order) return;
  if (order->active == active) return;
  gncOrderBeginEdit (order);
  order->active = active;
  mark_order (order);
  gncOrderCommitEdit (order);
}

/* =============================================================== */
/* Add an Entry to the Order */
void gncOrderAddEntry (GncOrder *order, GncEntry *entry)
{
  GncOrder *old;

  if (!order || !entry) return;

  old = gncEntryGetOrder (entry);
  if (old == order) return;			/* I already own it */
  if (old) gncOrderRemoveEntry (old, entry);

  order->entries = g_list_insert_sorted (order->entries, entry,
					 (GCompareFunc)gncEntryCompare);

  /* This will send out an event -- make sure we're attached */
  gncEntrySetOrder (entry, order);
  mark_order (order);
}

void gncOrderRemoveEntry (GncOrder *order, GncEntry *entry)
{
  if (!order || !entry) return;

  gncEntrySetOrder (entry, NULL);
  order->entries = g_list_remove (order->entries, entry);
  mark_order (order);
}

/* Get Functions */

const char * gncOrderGetID (const GncOrder *order)
{
  if (!order) return NULL;
  return order->id;
}

GncOwner * gncOrderGetOwner (GncOrder *order)
{
  if (!order) return NULL;
  return &order->owner;
}

Timespec gncOrderGetDateOpened (const GncOrder *order)
{
  Timespec ts; ts.tv_sec = 0; ts.tv_nsec = 0;
  if (!order) return ts;
  return order->opened;
}

Timespec gncOrderGetDateClosed (const GncOrder *order)
{
  Timespec ts; ts.tv_sec = 0; ts.tv_nsec = 0;
  if (!order) return ts;
  return order->closed;
}

const char * gncOrderGetNotes (const GncOrder *order)
{
  if (!order) return NULL;
  return order->notes;
}

const char * gncOrderGetReference (const GncOrder *order)
{
  if (!order) return NULL;
  return order->reference;
}

gboolean gncOrderGetActive (const GncOrder *order)
{
  if (!order) return FALSE;
  return order->active;
}

/* Get the list Entries */
GList * gncOrderGetEntries (GncOrder *order)
{
  if (!order) return NULL;
  return order->entries;
}

gboolean gncOrderIsClosed (const GncOrder *order)
{
  if (!order) return FALSE;
  if (order->closed.tv_sec || order->closed.tv_nsec) return TRUE;
  return FALSE;
}

/* =============================================================== */

void gncOrderBeginEdit (GncOrder *order)
{
  qof_begin_edit(&order->inst);
}

static void gncOrderOnError (QofInstance *order, QofBackendError errcode)
{
  PERR("Order QofBackend Failure: %d", errcode);
  gnc_engine_signal_commit_error( errcode );
}

static void gncOrderOnDone (QofInstance *order) {}

static void order_free (QofInstance *inst)
{
  GncOrder *order = (GncOrder *) inst;
  gncOrderFree (order);
}

void gncOrderCommitEdit (GncOrder *order)
{
  if (!qof_commit_edit (QOF_INSTANCE(order))) return;
  qof_commit_edit_part2 (&order->inst, gncOrderOnError,
			 gncOrderOnDone, order_free);
}

int gncOrderCompare (const GncOrder *a, const GncOrder *b)
{
  int compare;

  if (a == b) return 0;
  if (!a && b) return -1;
  if (a && !b) return 1;

  compare = safe_strcmp (a->id, b->id);
  if (compare) return compare;

  compare = timespec_cmp (&(a->opened), &(b->opened));
  if (compare) return compare;

  compare = timespec_cmp (&(a->closed), &(b->closed));
  if (compare) return compare;

  return qof_instance_guid_compare(a, b);
}

/* =========================================================== */
/* Package-Private functions */

static const char * 
_gncOrderPrintable (gpointer obj)
{
  GncOrder *order = obj;

  g_return_val_if_fail (order, NULL);

  if (qof_instance_get_dirty_flag(order) || order->printname == NULL) {
    if (order->printname) g_free (order->printname);

    order->printname =
      g_strdup_printf ("%s%s", order->id,
		       gncOrderIsClosed (order) ? _(" (closed)") : "");
  }

  return order->printname;
}

static QofObject gncOrderDesc =
{
  .interface_version = QOF_OBJECT_VERSION,
  .e_type            = _GNC_MOD_NAME,
  .type_label        = "Order",
  .create            = (gpointer)gncOrderCreate,
  .book_begin        = NULL,
  .book_end          = NULL,
  .is_dirty          = qof_collection_is_dirty,
  .mark_clean        = qof_collection_mark_clean,
  .foreach           = qof_collection_foreach,
  .printable         = _gncOrderPrintable,
  .version_cmp       = (int (*)(gpointer, gpointer)) qof_instance_version_cmp,
};

gboolean gncOrderRegister (void)
{
  static QofParam params[] = {
    { ORDER_ID, QOF_TYPE_STRING, (QofAccessFunc)gncOrderGetID, (QofSetterFunc)gncOrderSetID },
    { ORDER_REFERENCE, QOF_TYPE_STRING, (QofAccessFunc)gncOrderGetReference, (QofSetterFunc)gncOrderSetReference },
    { ORDER_OWNER, GNC_ID_OWNER, (QofAccessFunc)gncOrderGetOwner, (QofSetterFunc)gncOrderSetOwner },
    { ORDER_OPENED, QOF_TYPE_DATE, (QofAccessFunc)gncOrderGetDateOpened, (QofSetterFunc)gncOrderSetDateOpened },
    { ORDER_IS_CLOSED, QOF_TYPE_BOOLEAN, (QofAccessFunc)gncOrderIsClosed, NULL },
    { ORDER_CLOSED, QOF_TYPE_DATE, (QofAccessFunc)gncOrderGetDateClosed, (QofSetterFunc)gncOrderSetDateClosed },
    { ORDER_NOTES, QOF_TYPE_STRING, (QofAccessFunc)gncOrderGetNotes, (QofSetterFunc)gncOrderSetNotes },
    { QOF_PARAM_ACTIVE, QOF_TYPE_BOOLEAN, (QofAccessFunc)gncOrderGetActive, (QofSetterFunc)gncOrderSetActive },
    { QOF_PARAM_BOOK, QOF_ID_BOOK, (QofAccessFunc)qof_instance_get_book, NULL },
    { QOF_PARAM_GUID, QOF_TYPE_GUID, (QofAccessFunc)qof_instance_get_guid, NULL },
    { NULL },
  };

  qof_class_register (_GNC_MOD_NAME, (QofSortFunc)gncOrderCompare, params);

  return qof_object_register (&gncOrderDesc);
}

gint64 gncOrderNextID (QofBook *book)
{
  return qof_book_get_counter (book, _GNC_MOD_NAME);
}
