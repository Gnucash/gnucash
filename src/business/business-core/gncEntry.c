/*
 * gncEntry.c -- the Core Business Entry Interface
 * Copyright (C) 2001 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#include "config.h"

#include <glib.h>

#include "messages.h"
#include "gnc-numeric.h"
#include "gnc-engine-util.h"
#include "gnc-book-p.h"
#include "GNCIdP.h"

#include "gncBusiness.h"
#include "gncEntry.h"
#include "gncEntryP.h"
#include "gncInvoice.h"
#include "gncOrder.h"

struct _gncEntry {
  GNCBook *	book;

  GUID		guid;
  Timespec	date;
  char *	desc;
  char *	action;
  gnc_numeric 	quantity;
  gnc_numeric 	price;
  gnc_numeric 	tax;
  gint		tax_type;
  gnc_numeric 	discount;
  gint		disc_type;
  Account *	account;
  Account *	taxaccount;

  GncOrder *	order;
  GncInvoice *	invoice;

  gboolean	dirty;
};

#define _GNC_MOD_NAME	GNC_ENTRY_MODULE_NAME

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

static void addObj (GncEntry *entry);
static void remObj (GncEntry *entry);

/* Create/Destroy Functions */

GncEntry *gncEntryCreate (GNCBook *book)
{
  GncEntry *entry;

  if (!book) return NULL;

  entry = g_new0 (GncEntry, 1);
  entry->book = book;

  entry->desc = CACHE_INSERT ("");
  entry->action = CACHE_INSERT ("");

  {
    gnc_numeric zero = gnc_numeric_zero ();
    gncEntrySetQuantity (entry, zero);
    gncEntrySetPrice (entry, zero);
    gncEntrySetTax (entry, zero);
    gncEntrySetDiscount (entry, zero);
  }
  entry->dirty = FALSE;

  xaccGUIDNew (&entry->guid, book);
  addObj (entry);

  return entry;
}

void gncEntryDestroy (GncEntry *entry)
{
  if (!entry) return;

  CACHE_REMOVE (entry->desc);
  CACHE_REMOVE (entry->action);
  remObj (entry);

  g_free (entry);
}

/* Set Functions */

void gncEntrySetGUID (GncEntry *entry, const GUID *guid)
{
  if (!entry || !guid) return;
  if (guid_equal (guid, &entry->guid)) return;

  remObj (entry);
  entry->guid = *guid;
  addObj (entry);
}

void gncEntrySetDate (GncEntry *entry, Timespec *date)
{
  if (!entry || !date) return;
  entry->date = *date;
  entry->dirty = TRUE;
}

void gncEntrySetDescription (GncEntry *entry, const char *desc)
{
  if (!entry || !desc) return;
  SET_STR (entry->desc, desc);
  entry->dirty = TRUE;
}

void gncEntrySetAction (GncEntry *entry, const char *action)
{
  if (!entry || !action) return;
  SET_STR (entry->action, action);
  entry->dirty = TRUE;
}

void gncEntrySetQuantity (GncEntry *entry, gnc_numeric quantity)
{
  if (!entry) return;
  entry->quantity = quantity;
  entry->dirty = TRUE;
}

void gncEntrySetPrice (GncEntry *entry, gnc_numeric price)
{
  if (!entry) return;
  entry->price = price;
  entry->dirty = TRUE;
}

void gncEntrySetTax (GncEntry *entry, gnc_numeric tax)
{
  if (!entry) return;
  entry->tax = tax;
  entry->dirty = TRUE;
}

void gncEntrySetDiscount (GncEntry *entry, gnc_numeric discount)
{
  if (!entry) return;
  entry->discount = discount;
  entry->dirty = TRUE;
}

void gncEntrySetAccount (GncEntry *entry, Account *acc)
{
  if (!entry) return;
  entry->account = acc;
  entry->dirty = TRUE;
}

void gncEntrySetTaxAccount (GncEntry *entry, Account *acc)
{
  if (!entry) return;
  entry->taxaccount = acc;
  entry->dirty = TRUE;
}

/* Called from gncOrder when we're added to the Order */
void gncEntrySetOrder (GncEntry *entry, GncOrder *order)
{
  if (!entry) return;
  entry->order = order;
  entry->dirty = TRUE;
}

/* called from gncInvoice when we're added to the Invoice */
void gncEntrySetInvoice (GncEntry *entry, GncInvoice *invoice)
{
  if (!entry) return;
  entry->invoice = invoice;
  entry->dirty = TRUE;
}

void gncEntrySetTaxType (GncEntry *entry, gint type)
{
  if (!entry) return;
  if (type < 0 || type > 3) return;

  entry->tax_type = type;
  entry->dirty = TRUE;
}

void gncEntrySetDiscountType (GncEntry *entry, gint type)
{
  if (!entry) return;
  if (type < 0 || type > 3) return;

  entry->disc_type = type;
  entry->dirty = TRUE;
}

void gncEntrySetDirty (GncEntry *entry, gboolean dirty)
{
  if (!entry) return;
  entry->dirty = dirty;
}

/* Get Functions */

GNCBook * gncEntryGetBook (GncEntry *entry)
{
  if (!entry) return NULL;
  return entry->book;
}

const GUID * gncEntryGetGUID (GncEntry *entry)
{
  if (!entry) return NULL;
  return &(entry->guid);
}

Timespec gncEntryGetDate (GncEntry *entry)
{
  Timespec ts; ts.tv_sec = 0; ts.tv_nsec = 0;
  if (!entry) return ts;
  return entry->date;
}

const char * gncEntryGetDescription (GncEntry *entry)
{
  if (!entry) return NULL;
  return entry->desc;
}

const char * gncEntryGetAction (GncEntry *entry)
{
  if (!entry) return NULL;
  return entry->action;
}

gnc_numeric gncEntryGetQuantity (GncEntry *entry)
{
  if (!entry) return gnc_numeric_zero();
  return entry->quantity;
}

gnc_numeric gncEntryGetPrice (GncEntry *entry)
{
  if (!entry) return gnc_numeric_zero();
  return entry->price;
}

gnc_numeric gncEntryGetTax (GncEntry *entry)
{
  if (!entry) return gnc_numeric_zero();
  return entry->tax;
}

gnc_numeric gncEntryGetDiscount (GncEntry *entry)
{
  if (!entry) return gnc_numeric_zero();
  return entry->discount;
}

Account * gncEntryGetAccount (GncEntry *entry)
{
  if (!entry) return NULL;
  return entry->account;
}

Account * gncEntryGetTaxAccount (GncEntry *entry)
{
  if (!entry) return NULL;
  return entry->taxaccount;
}

GncInvoice * gncEntryGetInvoice (GncEntry *entry)
{
  if (!entry) return NULL;
  return entry->invoice;
}

GncOrder * gncEntryGetOrder (GncEntry *entry)
{
  if (!entry) return NULL;
  return entry->order;
}

gint gncEntryGetTaxType (GncEntry *entry)
{
  if (!entry) return 0;
  return entry->tax_type;
}

gint gncEntryGetDiscountType (GncEntry *entry)
{
  if (!entry) return 0;
  return entry->disc_type;
}

GncEntry * gncEntryLookup (GNCBook *book, const GUID *guid)
{
  if (!book || !guid) return NULL;
  return xaccLookupEntity (gnc_book_get_entity_table (book),
			   guid, _GNC_MOD_NAME);
}

void gncEntryCommitEdit (GncEntry *entry)
{
  if (!entry) return;
  /* XXX */
}

/* Package-Private functions */

static void addObj (GncEntry *entry)
{
  GHashTable *ht;

  xaccStoreEntity (gnc_book_get_entity_table (entry->book),
		   entry, &entry->guid, _GNC_MOD_NAME);

  ht = gnc_book_get_data (entry->book, _GNC_MOD_NAME);
  g_hash_table_insert (ht, &entry->guid, entry);
}

static void remObj (GncEntry *entry)
{
  GHashTable *ht;

  xaccRemoveEntity (gnc_book_get_entity_table (entry->book), &entry->guid);
  ht = gnc_book_get_data (entry->book, _GNC_MOD_NAME);
  g_hash_table_remove (ht, &entry->guid);
}

static void _gncEntryCreate (GNCBook *book)
{
  GHashTable *ht;

  if (!book) return;

  ht = guid_hash_table_new ();
  gnc_book_set_data (book, _GNC_MOD_NAME, ht);
}

static void _gncEntryDestroy (GNCBook *book)
{
  GHashTable *ht;

  if (!book) return;

  ht = gnc_book_get_data (book, _GNC_MOD_NAME);

  /* XXX : Destroy the objects? */
  g_hash_table_destroy (ht);
}

static GncBusinessObject gncEntryDesc = {
  GNC_BUSINESS_VERSION,
  _GNC_MOD_NAME,
  "Order/Invoice Entry",
  _gncEntryCreate,
  _gncEntryDestroy,
  NULL,				/* get list */
  NULL				/* printable */
};

gboolean gncEntryRegister (void)
{
  return gncBusinessRegister (&gncEntryDesc);
}
