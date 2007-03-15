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

/* GObject declarations */

static void gnc_order_class_init(GncOrderClass *klass);
static void gnc_order_init(GncOrder *sp);
static void gnc_order_finalize(GObject *object);

struct _GncOrderPrivate
{
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

typedef struct _GncOrderSignal GncOrderSignal;
typedef enum _GncOrderSignalType GncOrderSignalType;

enum _GncOrderSignalType {
	/* Signals */
	LAST_SIGNAL
};

/* properties */
enum
{
        PROP_0
};

struct _GncOrderSignal {
	GncOrder *object;
};

static guint gnc_order_signals[LAST_SIGNAL] = { 0 };
static GObjectClass *parent_class = NULL;

GType
gnc_order_get_type()
{
	static GType type = 0;

	if(type == 0) {
		static const GTypeInfo our_info = {
			sizeof (GncOrderClass),
			NULL,
			NULL,
			(GClassInitFunc)gnc_order_class_init,
			NULL,
			NULL,
			sizeof (GncOrder),
			0,
			(GInstanceInitFunc)gnc_order_init,
		};

		type = g_type_register_static(QOF_TYPE_INSTANCE, 
			"GncOrder", &our_info, 0);
	}

	return type;
}

static void
gnc_order_class_init(GncOrderClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS(klass);

	parent_class = g_type_class_peek_parent(klass);
	object_class->finalize = gnc_order_finalize;
	object_class->set_property = gnc_order_set_property;
    object_class->get_property = gnc_order_get_property;

	/* Install properties */
	
	/* Create signals here:*/
 	
}

static void
gnc_order_init(GncOrder *obj)
{
	/* Initialize private members, etc. */
}

static void
gnc_order_finalize(GObject *object)
{
	
	/* Free private members, etc. */
	
	G_OBJECT_CLASS(parent_class)->finalize(object);
}

static void
gnc_order_set_property (GObject *object,
				  guint param_id,
				  const GValue *value,
				  GParamSpec *pspec)
{
	GncOrder *obj;
	
	obj = GNC_ORDER (object);
	switch (param_id) {		
		default:
   			/* We don't have any other property... */
    		G_OBJECT_WARN_INVALID_PROPERTY_ID(object,property_id,pspec);
    	break;
	}
}

static void
gnc_order_get_property (GObject      *object,
                        guint         property_id,
                        GValue       *value,
                        GParamSpec   *pspec)
{
  GncOrder *obj;
  
  obj = GNC_ORDER (object);

  switch (property_id) {
  default:
    /* We don't have any other property... */
    G_OBJECT_WARN_INVALID_PROPERTY_ID(object,property_id,pspec);
    break;
  }
}

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
  qof_instance_set_dirty(QOF_INSTANCE (order));
  qof_event_gen (QOF_INSTANCE (order), QOF_EVENT_MODIFY, NULL);
}

/* =============================================================== */
/* Create/Destroy Functions */

GncOrder *gncOrderCreate (QofBook *book)
{
  GncOrder *order;

  if (!book) return NULL;

  order = GNC_ORDER (g_object_new (GNC_TYPE_ORDER, NULL));
  order->priv = g_new0 (GncOrderPrivate, 1);
  
  qof_instance_init (QOF_INSTANCE (order), _GNC_MOD_NAME, book);

  order->priv->id = CACHE_INSERT ("");
  order->priv->notes = CACHE_INSERT ("");
  order->priv->reference = CACHE_INSERT ("");

  order->priv->active = TRUE;

  qof_event_gen (QOF_INSTANCE (order), QOF_EVENT_CREATE, NULL);

  return order;
}

void gncOrderDestroy (GncOrder *order)
{
  if (!order) return;
  order->priv->inst.do_free = TRUE;
  gncOrderCommitEdit (order);
}

static void gncOrderFree (GncOrder *order)
{
  if (!order) return;

  qof_event_gen (QOF_INSTANCE (order), QOF_EVENT_DESTROY, NULL);

  g_list_free (order->priv->entries);
  CACHE_REMOVE (order->priv->id);
  CACHE_REMOVE (order->priv->notes);
  CACHE_REMOVE (order->priv->reference);

  if (order->priv->printname) g_free (order->priv->printname);

  qof_instance_release (QOF_INSTANCE (order));
}

GncOrder *
gncCloneOrder (GncOrder *from, QofBook *book)
{
  GList *node;
  GncOrder *order;

  if (!book) return NULL;
  order = GNC_ORDER (g_object_new (GNC_TYPE_ORDER, NULL))
  order->priv = g_new0 (GncOrder, 1);
  
  qof_instance_init (QOF_INSTANCE (order), _GNC_MOD_NAME, book);
  qof_instance_gemini (QOF_INSTANCE (order), &from->inst);

  order->priv->id = CACHE_INSERT (from->id);
  order->priv->notes = CACHE_INSERT (from->notes);
  order->priv->reference = CACHE_INSERT (from->reference);

  order->priv->active = from->active;
  order->priv->printname = NULL; /* yes, null, that's right */
  order->priv->opened = from->opened;
  order->priv->closed = from->closed;

  order->priv->owner = gncCloneOwner (&from->owner, book);

  order->priv->entries = NULL;
  for (node = g_list_last(from->entries); node; node=node->prev)
  {
    GncEntry *entry = node->data;
    entry = gncEntryObtainTwin (entry, book);
    order->priv->entries = g_list_prepend (order->priv->entries, entry);
  }

  qof_event_gen (QOF_INSTANCE (order), QOF_EVENT_CREATE, NULL);

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
  SET_STR (order, order->priv->id, id);
  mark_order (order);
  gncOrderCommitEdit (order);
}

void gncOrderSetOwner (GncOrder *order, GncOwner *owner)
{
  if (!order || !owner) return;
  if (gncOwnerEqual (&order->priv->owner, owner)) return;

  gncOrderBeginEdit (order);
  gncOwnerCopy (owner, &order->priv->owner);
  mark_order (order);
  gncOrderCommitEdit (order);
}

void gncOrderSetDateOpened (GncOrder *order, Timespec date)
{
  if (!order) return;
  if (timespec_equal (&order->priv->opened, &date)) return;
  gncOrderBeginEdit (order);
  order->priv->opened = date;
  mark_order (order);
  gncOrderCommitEdit (order);
}

void gncOrderSetDateClosed (GncOrder *order, Timespec date)
{
  if (!order) return;
  if (timespec_equal (&order->priv->closed, &date)) return;
  gncOrderBeginEdit (order);
  order->priv->closed = date;
  mark_order (order);
  gncOrderCommitEdit (order);
}

void gncOrderSetNotes (GncOrder *order, const char *notes)
{
  if (!order || !notes) return;
  SET_STR (order, order->priv->notes, notes);
  mark_order (order);
  gncOrderCommitEdit (order);
}

void gncOrderSetReference (GncOrder *order, const char *reference)
{
  if (!order || !reference) return;
  SET_STR (order, order->priv->reference, reference);
  mark_order (order);
  gncOrderCommitEdit (order);
}

void gncOrderSetActive (GncOrder *order, gboolean active)
{
  if (!order) return;
  if (order->priv->active == active) return;
  gncOrderBeginEdit (order);
  order->priv->active = active;
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

  order->priv->entries = g_list_insert_sorted (order->priv->entries, entry,
					 (GCompareFunc)gncEntryCompare);

  /* This will send out an event -- make sure we're attached */
  gncEntrySetOrder (entry, order);
  mark_order (order);
}

void gncOrderRemoveEntry (GncOrder *order, GncEntry *entry)
{
  if (!order || !entry) return;

  gncEntrySetOrder (entry, NULL);
  order->priv->entries = g_list_remove (order->priv->entries, entry);
  mark_order (order);
}

/* Get Functions */

const char * gncOrderGetID (GncOrder *order)
{
  if (!order) return NULL;
  return order->priv->id;
}

GncOwner * gncOrderGetOwner (GncOrder *order)
{
  if (!order) return NULL;
  return &order->priv->owner;
}

Timespec gncOrderGetDateOpened (GncOrder *order)
{
  Timespec ts; ts.tv_sec = 0; ts.tv_nsec = 0;
  if (!order) return ts;
  return order->priv->opened;
}

Timespec gncOrderGetDateClosed (GncOrder *order)
{
  Timespec ts; ts.tv_sec = 0; ts.tv_nsec = 0;
  if (!order) return ts;
  return order->priv->closed;
}

const char * gncOrderGetNotes (GncOrder *order)
{
  if (!order) return NULL;
  return order->priv->notes;
}

const char * gncOrderGetReference (GncOrder *order)
{
  if (!order) return NULL;
  return order->priv->reference;
}

gboolean gncOrderGetActive (GncOrder *order)
{
  if (!order) return FALSE;
  return order->priv->active;
}

/* Get the list Entries */
GList * gncOrderGetEntries (GncOrder *order)
{
  if (!order) return NULL;
  return order->priv->entries;
}

gboolean gncOrderIsClosed (GncOrder *order)
{
  if (!order) return FALSE;
  if (order->priv->closed.tv_sec || order->priv->closed.tv_nsec) return TRUE;
  return FALSE;
}

/* =============================================================== */

void gncOrderBeginEdit (GncOrder *order)
{
  qof_begin_edit(QOF_INSTANCE (order));
}

static void gncOrderOnError (QofInstance *order, QofBackendError errcode)
{
  PERR("Order QofBackend Failure: %d", errcode);
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
  qof_commit_edit_part2 (QOF_INSTANCE (order), gncOrderOnError,
			 gncOrderOnDone, order_free);
}

int gncOrderCompare (GncOrder *a, GncOrder *b)
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

  return guid_compare (&(a->inst.entity.guid), &(b->inst.entity.guid));
}

/* =========================================================== */
/* Package-Private functions */

static const char * 
_gncOrderPrintable (gpointer obj)
{
  GncOrder *order = obj;

  g_return_val_if_fail (order, NULL);

  if (qof_instance_is_dirty (QOF_INSTANCE (order)) || order->priv->printname == NULL) {
    if (order->priv->printname) g_free (order->priv->printname);

    order->priv->printname =
      g_strdup_printf ("%s%s", order->priv->id,
		       gncOrderIsClosed (order) ? _(" (closed)") : "");
  }

  return order->priv->printname;
}

static QofObject gncOrderDesc =
{
  interface_version:  QOF_OBJECT_VERSION,
  e_type:             _GNC_MOD_NAME,
  type_label:         "Order",
  create:             (gpointer)gncOrderCreate,
  book_begin:         NULL,
  book_end:           NULL,
  is_dirty:           qof_collection_is_dirty,
  mark_clean:         qof_collection_mark_clean,
  foreach:            qof_collection_foreach,
  printable:          _gncOrderPrintable,
  version_cmp:        (int (*)(gpointer, gpointer)) qof_instance_version_cmp,
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
