/*
 * gncEntry.c -- the Core Business Entry Interface
 * Copyright (C) 2001,2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#include "config.h"

#include <glib.h>

#include "messages.h"
#include "gnc-numeric.h"
#include "gnc-engine-util.h"
#include "gnc-book-p.h"
#include "GNCIdP.h"
#include "QueryObject.h"
#include "gnc-event-p.h"

#include "gncBusiness.h"
#include "gncEntry.h"
#include "gncEntryP.h"
#include "gncInvoice.h"
#include "gncOrder.h"

struct _gncEntry {
  GNCBook *	book;

  GUID		guid;
  Timespec	date;
  Timespec	date_entered;
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

  gnc_numeric	value;
  gnc_numeric	value_rounded;
  gnc_numeric	tax_value;
  gnc_numeric	tax_value_rounded;
  gnc_numeric	disc_value;
  gnc_numeric	disc_value_rounded;
  gboolean	values_dirty;

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

static char * typeStrs[] = {
  N_("Value"),
  N_("Percent"),
  N_("Value/Pretax"),
  N_("Percent/Pretax")
};

const char *gncEntryGetTaxTypeStr (gint type)
{
  return typeStrs[type & 0x01];	/* Only 0, 1 */
}

const char *gncEntryGetDiscountTypeStr (gint type)
{
  return typeStrs[type & 0x03];	/* Only 0, 1, 2, 3 */
}

gint gncEntryGetTypeFromStr (const char *type)
{
  gint i;
  for (i = 0; i < 3; i++)
    if (!safe_strcmp (typeStrs[i], type))
      return i;

  return -1;
}

G_INLINE_FUNC void mark_entry (GncEntry *entry);
G_INLINE_FUNC void
mark_entry (GncEntry *entry)
{
  entry->dirty = TRUE;

  gnc_engine_generate_event (&entry->guid, GNC_EVENT_MODIFY);
}

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
    entry->quantity = zero;
    entry->price = zero;
    entry->tax = zero;
    entry->discount = zero;
  }
  entry->tax_type = GNC_ENTRY_INTERP_PERCENT;
  entry->disc_type = GNC_ENTRY_INTERP_PERCENT;
  entry->dirty = FALSE;
  entry->values_dirty = TRUE;

  xaccGUIDNew (&entry->guid, book);
  addObj (entry);

  gnc_engine_generate_event (&entry->guid, GNC_EVENT_CREATE);

  return entry;
}

void gncEntryDestroy (GncEntry *entry)
{
  if (!entry) return;

  gnc_engine_generate_event (&entry->guid, GNC_EVENT_DESTROY);

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

void gncEntrySetDate (GncEntry *entry, Timespec date)
{
  if (!entry) return;
  entry->date = date;
  mark_entry (entry);
}

void gncEntrySetDateEntered (GncEntry *entry, Timespec date)
{
  if (!entry) return;
  entry->date_entered = date;
  mark_entry (entry);
}

void gncEntrySetDescription (GncEntry *entry, const char *desc)
{
  if (!entry || !desc) return;
  SET_STR (entry->desc, desc);
  mark_entry (entry);
}

void gncEntrySetAction (GncEntry *entry, const char *action)
{
  if (!entry || !action) return;
  SET_STR (entry->action, action);
  mark_entry (entry);
}

void gncEntrySetQuantity (GncEntry *entry, gnc_numeric quantity)
{
  if (!entry) return;
  entry->quantity = quantity;
  entry->values_dirty = TRUE;
  mark_entry (entry);
}

void gncEntrySetPrice (GncEntry *entry, gnc_numeric price)
{
  if (!entry) return;
  entry->price = price;
  entry->values_dirty = TRUE;
  mark_entry (entry);
}

void gncEntrySetTax (GncEntry *entry, gnc_numeric tax)
{
  if (!entry) return;
  entry->tax = tax;
  entry->values_dirty = TRUE;
  mark_entry (entry);
}

void gncEntrySetDiscount (GncEntry *entry, gnc_numeric discount)
{
  if (!entry) return;
  entry->discount = discount;
  entry->values_dirty = TRUE;
  mark_entry (entry);
}

void gncEntrySetAccount (GncEntry *entry, Account *acc)
{
  if (!entry) return;
  entry->account = acc;
  mark_entry (entry);
}

void gncEntrySetTaxAccount (GncEntry *entry, Account *acc)
{
  if (!entry) return;
  entry->taxaccount = acc;
  mark_entry (entry);
}

/* Called from gncOrder when we're added to the Order */
void gncEntrySetOrder (GncEntry *entry, GncOrder *order)
{
  if (!entry) return;
  entry->order = order;
  mark_entry (entry);

  /* Generate an event modifying the Order's end-owner */
  gnc_engine_generate_event (gncOwnerGetEndGUID (gncOrderGetOwner (order)),
			     GNC_EVENT_MODIFY);
}

/* called from gncInvoice when we're added to the Invoice */
void gncEntrySetInvoice (GncEntry *entry, GncInvoice *invoice)
{
  if (!entry) return;
  entry->invoice = invoice;
  mark_entry (entry);
}

void gncEntrySetTaxType (GncEntry *entry, gint type)
{
  if (!entry) return;
  if (type < 0 || type > 1) return;

  entry->tax_type = type;
  entry->values_dirty = TRUE;
  mark_entry (entry);
}

void gncEntrySetDiscountType (GncEntry *entry, gint type)
{
  if (!entry) return;
  if (type < 0 || type > 3) return;

  entry->disc_type = type;
  entry->values_dirty = TRUE;
  mark_entry (entry);
}

void gncEntrySetDirty (GncEntry *entry, gboolean dirty)
{
  if (!entry) return;
  entry->dirty = dirty;
}

void gncEntryCopy (const GncEntry *src, GncEntry *dest)
{
  if (!src || !dest) return;

  dest->date 			= src->date;
  dest->date_entered		= src->date_entered; /* ??? */
  gncEntrySetDescription (dest, src->desc);
  gncEntrySetAction (dest, src->action);
  dest->quantity		= src->quantity;
  dest->price			= src->price;
  dest->tax			= src->tax;
  dest->tax_type		= src->tax_type;
  dest->discount		= src->discount;
  dest->disc_type		= src->disc_type;
  dest->account			= src->account;
  dest->taxaccount		= src->taxaccount;

  if (src->order)
    gncOrderAddEntry (src->order, dest);

  if (src->invoice)
    gncInvoiceAddEntry (src->invoice, dest);

  dest->values_dirty = TRUE;
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

Timespec gncEntryGetDateEntered (GncEntry *entry)
{
  Timespec ts; ts.tv_sec = 0; ts.tv_nsec = 0;
  if (!entry) return ts;
  return entry->date_entered;
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

void gncEntryComputeValue (gnc_numeric qty, gnc_numeric price,
			   gnc_numeric tax, gint tax_type,
			   gnc_numeric discount, gint discount_type,
			   gnc_numeric *value, gnc_numeric *tax_value,
			   gnc_numeric *discount_value)
{
  gnc_numeric	subtotal;
  gnc_numeric	this_value;
  gnc_numeric	percent = gnc_numeric_create (100, 1);

  /* Compute the value */

  subtotal = gnc_numeric_mul (qty, price, GNC_DENOM_AUTO, GNC_DENOM_LCD);

  if (GNC_ENTRY_INTERP_IS_PERCENT (discount_type)) {
    discount = gnc_numeric_div (discount, percent, GNC_DENOM_AUTO, 
				GNC_DENOM_LCD);
    discount = gnc_numeric_mul (subtotal, discount, GNC_DENOM_AUTO,
				GNC_DENOM_LCD);
  }

  this_value = gnc_numeric_sub (subtotal, discount, GNC_DENOM_AUTO,
				GNC_DENOM_LCD);
  if (discount_type & GNC_ENTRY_PRETAX_FLAG)
    subtotal = this_value;

  /* Save the discount and value return values */

  if (discount_value != NULL)
    *discount_value = discount;

  if (value != NULL)
    *value = this_value;

  /* Now... Compute the tax value (if the caller wants it) */

  if (tax_value != NULL) {
    if (GNC_ENTRY_INTERP_IS_PERCENT (tax_type)) {
      tax = gnc_numeric_div (tax, percent, GNC_DENOM_AUTO, GNC_DENOM_LCD);
      tax = gnc_numeric_mul (subtotal, tax, GNC_DENOM_AUTO, GNC_DENOM_LCD);
    }

    *tax_value = tax;
  }

  return;
}

static int
get_commodity_denom (GncEntry *entry)
{
  if (!entry)
    return 0;
  if (entry->invoice) {
    gnc_commodity *c = gncInvoiceGetCommonCommodity (entry->invoice);
    if (c)
      return (gnc_commodity_get_fraction (c));
  }
  return 100000;
}

static void
gncEntryRecomputeValues (GncEntry *entry)
{
  int denom;

  if (!entry->values_dirty)
    return;

  gncEntryComputeValue (entry->quantity, entry->price,
			entry->tax, entry->tax_type,
			entry->discount, entry->disc_type,
			&(entry->value), &(entry->tax_value),
			&(entry->disc_value));

  denom = get_commodity_denom (entry);
  entry->value_rounded = gnc_numeric_convert (entry->value, denom,
					      GNC_RND_ROUND);
  entry->tax_value_rounded = gnc_numeric_convert (entry->tax_value, denom,
					      GNC_RND_ROUND);
  entry->disc_value_rounded = gnc_numeric_convert (entry->disc_value, denom,
					      GNC_RND_ROUND);
  entry->values_dirty = FALSE;
}

void gncEntryGetValue (GncEntry *entry, gnc_numeric *value,
		       gnc_numeric *tax_value, gnc_numeric *discount_value)
{
  if (!entry) return;
  gncEntryRecomputeValues (entry);
  if (value)
    *value = entry->value;
  if (tax_value)
    *tax_value = entry->tax_value;
  if (discount_value)
    *discount_value = entry->disc_value;
}

gnc_numeric gncEntryReturnValue (GncEntry *entry)
{
  if (!entry) return gnc_numeric_zero();
  gncEntryRecomputeValues (entry);
  return entry->value_rounded;
}

gnc_numeric gncEntryReturnTaxValue (GncEntry *entry)
{
  if (!entry) return gnc_numeric_zero();
  gncEntryRecomputeValues (entry);
  return entry->tax_value_rounded;
}

gnc_numeric gncEntryReturnDiscountValue (GncEntry *entry)
{
  if (!entry) return gnc_numeric_zero();
  gncEntryRecomputeValues (entry);
  return entry->disc_value_rounded;
}

void gncEntryCommitEdit (GncEntry *entry)
{
  if (!entry) return;
  /* XXX */
  if (entry->dirty)
    gncBusinessSetDirtyFlag (entry->book, _GNC_MOD_NAME, TRUE);
  entry->dirty = FALSE;
}

int gncEntryCompare (GncEntry *a, GncEntry *b)
{
  int compare;

  if (a == b) return 0;
  if (!a && b) return -1;
  if (a && !b) return 1;

  compare = timespec_cmp (&(a->date), &(b->date));
  if (compare) return compare;

  compare = timespec_cmp (&(a->date_entered), &(b->date_entered));
  if (compare) return compare;

  compare = safe_strcmp (a->desc, b->desc);
  if (compare) return compare;

  compare = safe_strcmp (a->action, b->action);
  if (compare) return compare;

  return guid_compare (&(a->guid), &(b->guid));
}

/* Package-Private functions */

static void addObj (GncEntry *entry)
{
  gncBusinessAddObject (entry->book, _GNC_MOD_NAME, entry, &entry->guid);
}

static void remObj (GncEntry *entry)
{
  gncBusinessRemoveObject (entry->book, _GNC_MOD_NAME, &entry->guid);
}

static void _gncEntryCreate (GNCBook *book)
{
  gncBusinessCreate (book, _GNC_MOD_NAME);
}

static void _gncEntryDestroy (GNCBook *book)
{
  gncBusinessDestroy (book, _GNC_MOD_NAME);
}

static gboolean _gncEntryIsDirty (GNCBook *book)
{
  return gncBusinessIsDirty (book, _GNC_MOD_NAME);
}

static void _gncEntryMarkClean (GNCBook *book)
{
  gncBusinessSetDirtyFlag (book, _GNC_MOD_NAME, FALSE);
}

static void _gncEntryForeach (GNCBook *book, foreachObjectCB cb,
			      gpointer user_data)
{
  gncBusinessForeach (book, _GNC_MOD_NAME, cb, user_data);
}

static GncObject_t gncEntryDesc = {
  GNC_OBJECT_VERSION,
  _GNC_MOD_NAME,
  "Order/Invoice Entry",
  _gncEntryCreate,
  _gncEntryDestroy,
  _gncEntryIsDirty,
  _gncEntryMarkClean,
  _gncEntryForeach,
  NULL				/* printable */
};

gboolean gncEntryRegister (void)
{
  static QueryObjectDef params[] = {
    { ENTRY_DATE, QUERYCORE_DATE, (QueryAccess)gncEntryGetDate },
    { ENTRY_DESC, QUERYCORE_STRING, (QueryAccess)gncEntryGetDescription },
    { ENTRY_ACTION, QUERYCORE_STRING, (QueryAccess)gncEntryGetAction },
    { ENTRY_QTY, QUERYCORE_NUMERIC, (QueryAccess)gncEntryGetQuantity },
    { ENTRY_PRICE, QUERYCORE_NUMERIC, (QueryAccess)gncEntryGetPrice },
    { ENTRY_INVOICE, GNC_INVOICE_MODULE_NAME, (QueryAccess)gncEntryGetInvoice },
    { ENTRY_ORDER, GNC_ORDER_MODULE_NAME, (QueryAccess)gncEntryGetOrder },
    { QUERY_PARAM_BOOK, GNC_ID_BOOK, (QueryAccess)gncEntryGetBook },
    { QUERY_PARAM_GUID, QUERYCORE_GUID, (QueryAccess)gncEntryGetGUID },
    { NULL },
  };

  gncQueryObjectRegister (_GNC_MOD_NAME, (QuerySort)gncEntryCompare, params);

  return gncObjectRegister (&gncEntryDesc);
}
