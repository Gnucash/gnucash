/*
 * gncOrder.c -- the Core Business Order
 * Copyright (C) 2001,2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#include "config.h"

#include <glib.h>

#include "messages.h"
#include "gnc-numeric.h"
#include "kvp_frame.h"
#include "gnc-engine-util.h"
#include "gnc-book-p.h"
#include "GNCIdP.h"
#include "QueryObject.h"
#include "gnc-event-p.h"

#include "gncBusiness.h"
#include "gncEntry.h"
#include "gncEntryP.h"
#include "gncOrder.h"
#include "gncOrderP.h"
#include "gncOwner.h"

struct _gncOrder {
  GNCBook *book;

  GUID		guid;
  char *	id;
  char *	notes;
  char *	reference;
  char *	printname;
  GncOwner	owner;
  GList *	entries;
  Timespec 	opened;
  Timespec 	closed;
  gboolean 	active;

  gboolean	dirty;
};

#define _GNC_MOD_NAME	GNC_ORDER_MODULE_NAME

#define CACHE_INSERT(str) g_cache_insert(gnc_engine_get_string_cache(), (gpointer)(str));
#define CACHE_REMOVE(str) g_cache_remove(gnc_engine_get_string_cache(), (str));

#define SET_STR(member, str) { \
	char * tmp; \
	\
	if (!safe_strcmp (member, str)) return; \
	tmp = CACHE_INSERT (str); \
	CACHE_REMOVE (member); \
	member = tmp; \
	}

static void addObj (GncOrder *order);
static void remObj (GncOrder *order);

G_INLINE_FUNC void mark_order (GncOrder *order);
G_INLINE_FUNC void
mark_order (GncOrder *order)
{
  order->dirty = TRUE;

  gnc_engine_generate_event (&order->guid, GNC_EVENT_MODIFY);
}

/* Create/Destroy Functions */

GncOrder *gncOrderCreate (GNCBook *book)
{
  GncOrder *order;

  if (!book) return NULL;

  order = g_new0 (GncOrder, 1);
  order->book = book;

  order->id = CACHE_INSERT ("");
  order->notes = CACHE_INSERT ("");
  order->reference = CACHE_INSERT ("");

  order->active = TRUE;

  xaccGUIDNew (&order->guid, book);
  addObj (order);

  gnc_engine_generate_event (&order->guid, GNC_EVENT_CREATE);

  return order;
}

void gncOrderDestroy (GncOrder *order)
{
  if (!order) return;

  gnc_engine_generate_event (&order->guid, GNC_EVENT_DESTROY);

  g_list_free (order->entries);
  CACHE_REMOVE (order->id);
  CACHE_REMOVE (order->notes);
  CACHE_REMOVE (order->reference);
  remObj (order);

  if (order->printname) g_free (order->printname);

  g_free (order);
}

/* Set Functions */

void gncOrderSetGUID (GncOrder *order, const GUID *guid)
{
  if (!order || !guid) return;
  if (guid_equal (guid, &order->guid)) return;

  remObj (order);
  order->guid = *guid;
  addObj (order);
}

void gncOrderSetID (GncOrder *order, const char *id)
{
  if (!order || !id) return;
  SET_STR (order->id, id);
  mark_order (order);
}

void gncOrderSetOwner (GncOrder *order, GncOwner *owner)
{
  if (!order || !owner) return;

  gncOwnerCopy (owner, &order->owner);
  mark_order (order);
}

void gncOrderSetDateOpened (GncOrder *order, Timespec date)
{
  if (!order) return;
  order->opened = date;
  mark_order (order);
}

void gncOrderSetDateClosed (GncOrder *order, Timespec date)
{
  if (!order) return;
  order->closed = date;
  mark_order (order);
}

void gncOrderSetNotes (GncOrder *order, const char *notes)
{
  if (!order || !notes) return;
  SET_STR (order->notes, notes);
  mark_order (order);
}

void gncOrderSetReference (GncOrder *order, const char *reference)
{
  if (!order || !reference) return;
  SET_STR (order->reference, reference);
  mark_order (order);
}

void gncOrderSetActive (GncOrder *order, gboolean active)
{
  if (!order) return;
  order->active = active;
  mark_order (order);
}

void gncOrderSetDirty (GncOrder *order, gboolean dirty)
{
  if (!order) return;
  order->dirty = dirty;
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

GNCBook * gncOrderGetBook (GncOrder *order)
{
  if (!order) return NULL;
  return order->book;
}

const GUID * gncOrderGetGUID (GncOrder *order)
{
  if (!order) return NULL;
  return &(order->guid);
}

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

GncOrder * gncOrderLookup (GNCBook *book, const GUID *guid)
{
  if (!book || !guid) return NULL;
  return xaccLookupEntity (gnc_book_get_entity_table (book),
			   guid, _GNC_MOD_NAME);
}

gboolean gncOrderIsDirty (GncOrder *order)
{
  if (!order) return FALSE;
  return order->dirty;
}

gboolean gncOrderIsClosed (GncOrder *order)
{
  if (!order) return FALSE;
  if (order->closed.tv_sec || order->closed.tv_nsec) return TRUE;
  return FALSE;
}

void gncOrderBeginEdit (GncOrder *order)
{
  if (!order) return;
}

void gncOrderCommitEdit (GncOrder *order)
{
  if (!order) return;
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

  return guid_compare (&(a->guid), &(b->guid));
}

/* Package-Private functions */

static void addObj (GncOrder *order)
{
  gncBusinessAddObject (order->book, _GNC_MOD_NAME, order, &order->guid);
}

static void remObj (GncOrder *order)
{
  gncBusinessRemoveObject (order->book, _GNC_MOD_NAME, &order->guid);
}

static void _gncOrderCreate (GNCBook *book)
{
  gncBusinessCreate (book, _GNC_MOD_NAME);
}

static void _gncOrderDestroy (GNCBook *book)
{
  gncBusinessDestroy (book, _GNC_MOD_NAME);
}

static gboolean _gncOrderIsDirty (GNCBook *book)
{
  return gncBusinessIsDirty (book, _GNC_MOD_NAME);
}

static void _gncOrderForeach (GNCBook *book, foreachObjectCB cb,
			      gpointer user_data)
{
  gncBusinessForeach (book, _GNC_MOD_NAME, cb, user_data);
}

static const char * _gncOrderPrintable (gpointer obj)
{
  GncOrder *order = obj;

  g_return_val_if_fail (order, NULL);

  if (order->dirty || order->printname == NULL) {
    if (order->printname) g_free (order->printname);

    order->printname =
      g_strdup_printf ("%s%s", order->id,
		       gncOrderIsClosed (order) ? _(" (closed)") : "");
  }

  return order->printname;
}

static GncObject_t gncOrderDesc = {
  GNC_OBJECT_VERSION,
  _GNC_MOD_NAME,
  "Order",
  _gncOrderCreate,
  _gncOrderDestroy,
  _gncOrderIsDirty,
  _gncOrderForeach,
  _gncOrderPrintable,
};

gboolean gncOrderRegister (void)
{
  static QueryObjectDef params[] = {
    { ORDER_ID, QUERYCORE_STRING, (QueryAccess)gncOrderGetID },
    { ORDER_OWNER, GNC_OWNER_MODULE_NAME, (QueryAccess)gncOrderGetOwner },
    { ORDER_OPENED, QUERYCORE_DATE, (QueryAccess)gncOrderGetDateOpened },
    { ORDER_IS_CLOSED, QUERYCORE_BOOLEAN, (QueryAccess)gncOrderIsClosed },
    { ORDER_CLOSED, QUERYCORE_DATE, (QueryAccess)gncOrderGetDateClosed },
    { ORDER_NOTES, QUERYCORE_STRING, (QueryAccess)gncOrderGetNotes },
    { QUERY_PARAM_BOOK, GNC_ID_BOOK, (QueryAccess)gncOrderGetBook },
    { QUERY_PARAM_GUID, QUERYCORE_GUID, (QueryAccess)gncOrderGetGUID },
    { NULL },
  };

  gncQueryObjectRegister (_GNC_MOD_NAME, (QuerySort)gncOrderCompare, params);

  return gncObjectRegister (&gncOrderDesc);
}

static gint lastId = 471;	/* XXX */

gint gncOrderNextID (GNCBook *book)
{
  return lastId++;
}
