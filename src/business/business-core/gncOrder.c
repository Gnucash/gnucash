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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

/*
 * Copyright (C) 2001,2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#include "config.h"

#include <glib.h>

#include "messages.h"
#include "gnc-numeric.h"
#include "kvp_frame.h"
#include "gnc-engine-util.h"

#include "qofbook.h"
#include "qofclass.h"
#include "qofid.h"
#include "qofid-p.h"
#include "qofinstance.h"
#include "qofinstance-p.h"
#include "qofobject.h"
#include "qofquery.h"
#include "qofquerycore.h"

#include "gnc-event-p.h"
#include "gnc-be-utils.h"

#include "gncBusiness.h"
#include "gncEntry.h"
#include "gncEntryP.h"
#include "gncOrder.h"
#include "gncOrderP.h"
#include "gncOwner.h"

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

static short	module = MOD_BUSINESS;

#define _GNC_MOD_NAME	GNC_ORDER_MODULE_NAME

#define CACHE_INSERT(str) g_cache_insert(gnc_engine_get_string_cache(), (gpointer)(str));
#define CACHE_REMOVE(str) g_cache_remove(gnc_engine_get_string_cache(), (str));

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
G_INLINE_FUNC void
mark_order (GncOrder *order)
{
  order->inst.dirty = TRUE;
  gncBusinessSetDirtyFlag (order->inst.book, _GNC_MOD_NAME, TRUE);

  gnc_engine_gen_event (&order->inst.entity, GNC_EVENT_MODIFY);
}

/* Create/Destroy Functions */

GncOrder *gncOrderCreate (QofBook *book)
{
  GncOrder *order;

  if (!book) return NULL;

  order = g_new0 (GncOrder, 1);
  qof_instance_init (&order->inst, _GNC_MOD_NAME, book);

  order->id = CACHE_INSERT ("");
  order->notes = CACHE_INSERT ("");
  order->reference = CACHE_INSERT ("");

  order->active = TRUE;

  gnc_engine_gen_event (&order->inst.entity, GNC_EVENT_CREATE);

  return order;
}

void gncOrderDestroy (GncOrder *order)
{
  if (!order) return;
  order->inst.do_free = TRUE;
  gncOrderCommitEdit (order);
}

static void gncOrderFree (GncOrder *order)
{
  if (!order) return;

  gnc_engine_gen_event (&order->inst.entity, GNC_EVENT_DESTROY);

  g_list_free (order->entries);
  CACHE_REMOVE (order->id);
  CACHE_REMOVE (order->notes);
  CACHE_REMOVE (order->reference);

  if (order->printname) g_free (order->printname);

  qof_instance_release (&order->inst);
  g_free (order);
}

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

/* XXX the existance of this routie is wrong */
void gncOrderSetDirty (GncOrder *order, gboolean dirty)
{
  if (!order) return;
  order->inst.dirty = dirty;
}

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

const char * gncOrderGetID (GncOrder *order)
{
  if (!order) return NULL;
  return order->id;
}

GncOwner * gncOrderGetOwner (GncOrder *order)
{
  if (!order) return NULL;
  return &order->owner;
}

Timespec gncOrderGetDateOpened (GncOrder *order)
{
  Timespec ts; ts.tv_sec = 0; ts.tv_nsec = 0;
  if (!order) return ts;
  return order->opened;
}

Timespec gncOrderGetDateClosed (GncOrder *order)
{
  Timespec ts; ts.tv_sec = 0; ts.tv_nsec = 0;
  if (!order) return ts;
  return order->closed;
}

const char * gncOrderGetNotes (GncOrder *order)
{
  if (!order) return NULL;
  return order->notes;
}

const char * gncOrderGetReference (GncOrder *order)
{
  if (!order) return NULL;
  return order->reference;
}

gboolean gncOrderGetActive (GncOrder *order)
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

GncOrder * gncOrderLookup (QofBook *book, const GUID *guid)
{
  ELOOKUP(GncOrder);
}

gboolean gncOrderIsClosed (GncOrder *order)
{
  if (!order) return FALSE;
  if (order->closed.tv_sec || order->closed.tv_nsec) return TRUE;
  return FALSE;
}

void gncOrderBeginEdit (GncOrder *order)
{
  GNC_BEGIN_EDIT (&order->inst);
}

static inline void gncOrderOnError (QofInstance *order, QofBackendError errcode)
{
  PERR("Order QofBackend Failure: %d", errcode);
}

static inline void gncOrderOnDone (QofInstance *order) {}

static inline void order_free (QofInstance *inst)
{
  GncOrder *order = (GncOrder *) inst;
  gncOrderFree (order);
}

void gncOrderCommitEdit (GncOrder *order)
{
  GNC_COMMIT_EDIT_PART1 (&order->inst);
  GNC_COMMIT_EDIT_PART2 (&order->inst, gncOrderOnError,
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

/* Package-Private functions */

static void _gncOrderCreate (QofBook *book)
{
  gncBusinessCreate (book, _GNC_MOD_NAME);
}

static void _gncOrderDestroy (QofBook *book)
{
  gncBusinessDestroy (book, _GNC_MOD_NAME);
}

static gboolean _gncOrderIsDirty (QofBook *book)
{
  return gncBusinessIsDirty (book, _GNC_MOD_NAME);
}

static void _gncOrderMarkClean (QofBook *book)
{
  gncBusinessSetDirtyFlag (book, _GNC_MOD_NAME, FALSE);
}

static void _gncOrderForeach (QofBook *book, QofForeachCB cb,
			      gpointer user_data)
{
  gncBusinessForeach (book, _GNC_MOD_NAME, cb, user_data);
}

static const char * _gncOrderPrintable (gpointer obj)
{
  GncOrder *order = obj;

  g_return_val_if_fail (order, NULL);

  if (order->inst.dirty || order->printname == NULL) {
    if (order->printname) g_free (order->printname);

    order->printname =
      g_strdup_printf ("%s%s", order->id,
		       gncOrderIsClosed (order) ? _(" (closed)") : "");
  }

  return order->printname;
}

static QofObject gncOrderDesc = {
  QOF_OBJECT_VERSION,
  _GNC_MOD_NAME,
  "Order",
  _gncOrderCreate,
  _gncOrderDestroy,
  _gncOrderIsDirty,
  _gncOrderMarkClean,
  _gncOrderForeach,
  _gncOrderPrintable,
};

gboolean gncOrderRegister (void)
{
  static QofParam params[] = {
    { ORDER_ID, QOF_TYPE_STRING, (QofAccessFunc)gncOrderGetID, NULL },
    { ORDER_REFERENCE, QOF_TYPE_STRING, (QofAccessFunc)gncOrderGetReference, NULL },
    { ORDER_OWNER, GNC_OWNER_MODULE_NAME, (QofAccessFunc)gncOrderGetOwner, NULL },
    { ORDER_OPENED, QOF_TYPE_DATE, (QofAccessFunc)gncOrderGetDateOpened, NULL },
    { ORDER_IS_CLOSED, QOF_TYPE_BOOLEAN, (QofAccessFunc)gncOrderIsClosed, NULL },
    { ORDER_CLOSED, QOF_TYPE_DATE, (QofAccessFunc)gncOrderGetDateClosed, NULL },
    { ORDER_NOTES, QOF_TYPE_STRING, (QofAccessFunc)gncOrderGetNotes, NULL },
    { QOF_QUERY_PARAM_ACTIVE, QOF_TYPE_BOOLEAN, (QofAccessFunc)gncOrderGetActive, NULL },
    { QOF_QUERY_PARAM_BOOK, QOF_ID_BOOK, (QofAccessFunc)qof_instance_get_book, NULL },
    { QOF_QUERY_PARAM_GUID, QOF_TYPE_GUID, (QofAccessFunc)qof_instance_get_guid, NULL },
    { NULL },
  };

  qof_class_register (_GNC_MOD_NAME, (QofSortFunc)gncOrderCompare, params);

  return qof_object_register (&gncOrderDesc);
}

gint64 gncOrderNextID (QofBook *book)
{
  return gnc_book_get_counter (book, _GNC_MOD_NAME);
}
