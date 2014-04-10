/*
 * gncOrder.c -- the Core Business Order
 * Copyright (C) 2001 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#include "config.h"

#include <glib.h>

#include "messages.h"
#include "gnc-numeric.h"
#include "kvp_frame.h"
#include "gnc-engine-util.h"

#include "gncBusiness.h"
#include "gncJob.h"
#include "gncVendor.h"
#include "gncEntry.h"
#include "gncEntryP.h"
#include "gncOrder.h"
#include "gncOrderP.h"

struct _gncOrder {
  GncBusiness *business;

  GUID		guid;
  char *	id;
  char *	notes;
  GncOrderType 	type;
  union {
    GncJob *	job;
    GncVendor *	vendor;
  } owner;
  GList *	entries;
  Timespec 	opened;
  Timespec 	closed;
  gboolean 	active;

  gboolean	dirty;
};

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

/* Create/Destroy Functions */

GncOrder *gncOrderCreate (GncBusiness *business, GncOrderType type)
{
  GncOrder *order;

  if (!business) return NULL;
  if (type != GNC_ORDER_SALES && type != GNC_ORDER_PURCHASE) return NULL;

  order = g_new0 (GncOrder, 1);
  order->business = business;

  order->id = CACHE_INSERT ("");
  order->notes = CACHE_INSERT ("");

  order->active = TRUE;
  order->type = type;

  guid_new (&order->guid);
  gncBusinessAddEntity (business, GNC_ORDER_MODULE_NAME, &order->guid, order);

  return order;
}

void gncOrderDestroy (GncOrder *order)
{
  GList *item;

  if (!order) return;

  g_list_free (order->entries);
  CACHE_REMOVE (order->id);
  CACHE_REMOVE (order->notes);
  gncBusinessRemoveEntity (order->business, GNC_ORDER_MODULE_NAME,
			   &order->guid);

  g_free (order);
}

/* Set Functions */

void gncOrderSetGUID (GncOrder *order, const GUID *guid)
{
  if (!order || !guid) return;
  gncBusinessRemoveEntity (order->business, GNC_ORDER_MODULE_NAME,
			   &order->guid);
  order->guid = *guid;
  gncBusinessAddEntity (order->business, GNC_ORDER_MODULE_NAME,
			&order->guid, order);
}

void gncOrderSetID (GncOrder *order, const char *id)
{
  if (!order || !id) return;
  SET_STR (order->id, id);
  order->dirty = TRUE;
}

void gncOrderSetJob (GncOrder *order, GncJob *job)
{
  if (!order) return;
  if (order->type != GNC_ORDER_SALES) return;
  order->owner.job = job;
  order->dirty = TRUE;
}

void gncOrderSetVendor (GncOrder *order, GncVendor *vendor)
{
  if (!order) return;
  if (order->type != GNC_ORDER_PURCHASE) return;

  order->owner.vendor = vendor;
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

GncBusiness * gncOrderGetBusiness (GncOrder *order)
{
  if (!order) return NULL;
  return order->business;
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

GncOrderType gncOrderGetType (GncOrder *order)
{
  if (!order) return GNC_ORDER_NONE;
  return order->type;
}

GncJob * gncOrderGetJob (GncOrder *order)
{
  if (!order) return NULL;
  if (order->type != GNC_ORDER_SALES) return NULL;
  return order->owner.job;
}

GncVendor * gncOrderGetVendor (GncOrder *order)
{
  if (!order) return NULL;
  if (order->type != GNC_ORDER_PURCHASE) return NULL;
  return order->owner.vendor;
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

gboolean gncOrderIsDirty (GncOrder *order)
{
  if (!order) return FALSE;
  return order->dirty;
}

void gncOrderBeginEdit (GncOrder *order)
{
  if (!order) return;
}

void gncOrderCommitEdit (GncOrder *order)
{
  if (!order) return;
}

static GncBusinessObject gncOrderDesc = {
  GNC_BUSINESS_VERSION,
  GNC_ORDER_MODULE_NAME,
  "Purchase/Sales Order",
  NULL,				/* destroy */
  NULL,				/* get list */
  NULL				/* printable */
};

gboolean gncOrderRegister (void)
{
  return gncBusinessRegister (&gncOrderDesc);
}

static gint lastId = 471;	/* XXX */

gint gncOrderNextID (GncBusiness *business)
{
  return lastId++;
}
