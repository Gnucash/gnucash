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

/* Create/Destroy Functions */

GncOrder *gncOrderCreate (GNCBook *book)
{
  GncOrder *order;

  if (!book) return NULL;

  order = g_new0 (GncOrder, 1);
  order->book = book;

  order->id = CACHE_INSERT ("");
  order->notes = CACHE_INSERT ("");

  order->active = TRUE;

  xaccGUIDNew (&order->guid, book);
  addObj (order);

  return order;
}

void gncOrderDestroy (GncOrder *order)
{
  if (!order) return;

  g_list_free (order->entries);
  CACHE_REMOVE (order->id);
  CACHE_REMOVE (order->notes);
  remObj (order);

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
  order->dirty = TRUE;
}

void gncOrderSetOwner (GncOrder *order, GncOwner *owner)
{
  if (!order || !owner) return;

  gncOwnerCopy (owner, &order->owner);
  order->dirty = TRUE;
}

void gncOrderSetDateOpened (GncOrder *order, Timespec *date)
{
  if (!order || !date) return;
  order->opened = *date;
  order->dirty = TRUE;
}

void gncOrderSetDateClosed (GncOrder *order, Timespec *date)
{
  if (!order || !date) return;
  order->closed = *date;
  order->dirty = TRUE;
}

void gncOrderSetNotes (GncOrder *order, const char *notes)
{
  if (!order || !notes) return;
  SET_STR (order->notes, notes);
  order->dirty = TRUE;
}

void gncOrderSetActive (GncOrder *order, gboolean active)
{
  if (!order) return;
  order->active = active;
  order->dirty = TRUE;
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

  gncEntrySetOrder (entry, order);
  order->entries = g_list_append (order->entries, entry);
  order->dirty = TRUE;
}

void gncOrderRemoveEntry (GncOrder *order, GncEntry *entry)
{
  if (!order || !entry) return;

  gncEntrySetOrder (entry, NULL);
  order->entries = g_list_remove (order->entries, entry);
  order->dirty = TRUE;
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
  if (!compare) return compare;

  compare = timespec_cmp (&(a->opened), &(b->opened));
  if (!compare) return compare;

  compare = timespec_cmp (&(a->closed), &(b->closed));
  if (!compare) return compare;

  return guid_compare (&(a->guid), &(b->guid));
}

/* Package-Private functions */

static void addObj (GncOrder *order)
{
  GHashTable *ht;

  xaccStoreEntity (gnc_book_get_entity_table (order->book),
		   order, &order->guid, _GNC_MOD_NAME);

  ht = gnc_book_get_data (order->book, _GNC_MOD_NAME);
  g_hash_table_insert (ht, &order->guid, order);
}

static void remObj (GncOrder *order)
{
  GHashTable *ht;

  xaccRemoveEntity (gnc_book_get_entity_table (order->book), &order->guid);
  ht = gnc_book_get_data (order->book, _GNC_MOD_NAME);
  g_hash_table_remove (ht, &order->guid);
}

static void _gncOrderCreate (GNCBook *book)
{
  GHashTable *ht;

  if (!book) return;

  ht = guid_hash_table_new ();
  gnc_book_set_data (book, _GNC_MOD_NAME, ht);
}

static void _gncOrderDestroy (GNCBook *book)
{
  GHashTable *ht;

  if (!book) return;

  ht = gnc_book_get_data (book, _GNC_MOD_NAME);

  /* XXX : Destroy the objects? */
  g_hash_table_destroy (ht);
}

static void _gncOrderForeach (GNCBook *book, foreachObjectCB cb,
			      gpointer user_data)
{
  if (!book || !cb) return;
  gncBusinessForeach (book, _GNC_MOD_NAME, cb, user_data);
}

static GncObject_t gncOrderDesc = {
  GNC_OBJECT_VERSION,
  _GNC_MOD_NAME,
  "Order",
  _gncOrderCreate,
  _gncOrderDestroy,
  _gncOrderForeach,
  NULL				/* printable */
};

gboolean gncOrderRegister (void)
{
  static QueryObjectDef params[] = {
    { ORDER_GUID, QUERYCORE_GUID, (QueryAccess)gncOrderGetGUID },
    { ORDER_ID, QUERYCORE_STRING, (QueryAccess)gncOrderGetID },
    { ORDER_OWNER, GNC_OWNER_MODULE_NAME, (QueryAccess)gncOrderGetOwner },
    { ORDER_OPENED, QUERYCORE_DATE, (QueryAccess)gncOrderGetDateOpened },
    { ORDER_IS_CLOSED, QUERYCORE_BOOLEAN, (QueryAccess)gncOrderIsClosed },
    { ORDER_CLOSED, QUERYCORE_DATE, (QueryAccess)gncOrderGetDateClosed },
    { ORDER_NOTES, QUERYCORE_STRING, (QueryAccess)gncOrderGetNotes },
    { QUERY_PARAM_BOOK, GNC_ID_BOOK, (QueryAccess)gncOrderGetBook },
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
