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
  char *	notes;
  gnc_numeric 	quantity;
  gnc_numeric 	price;
  gnc_numeric 	discount;
  GncAmountType	disc_type;
  GncDiscountHow disc_how;
  Account *	account;

  gboolean	taxable;
  gboolean	taxincluded;
  GncTaxTable *	tax_table;

  GncOrder *	order;
  GncInvoice *	invoice;

  gnc_numeric	value;
  gnc_numeric	value_rounded;
  GList *	tax_values;
  gnc_numeric	tax_value;
  gnc_numeric	tax_value_rounded;
  gnc_numeric	disc_value;
  gnc_numeric	disc_value_rounded;
  gboolean	values_dirty;
  Timespec	taxtable_modtime;

  gboolean	dirty;
};

/* You must edit the functions in this block in tandem.  KEEP THEM IN
   SYNC! */

#define GNC_RETURN_ENUM_AS_STRING(x,s) case (x): return (s);
const char *
gncEntryDiscountHowToString (GncDiscountHow how)
{
  switch(how)
  {
    GNC_RETURN_ENUM_AS_STRING(GNC_DISC_PRETAX, "PRETAX");
    GNC_RETURN_ENUM_AS_STRING(GNC_DISC_SAMETIME, "SAMETIME");
    GNC_RETURN_ENUM_AS_STRING(GNC_DISC_POSTTAX, "POSTTAX");
    default:
      g_warning ("asked to translate unknown discount-how %d.\n", how);
      break;
  }
  return(NULL);
}
#undef GNC_RETURN_ENUM_AS_STRING
#define GNC_RETURN_ON_MATCH(s,x) \
  if(safe_strcmp((s), (str)) == 0) { *how = x; return(TRUE); }
gboolean gncEntryDiscountStringToHow (const char *str, GncDiscountHow *how)
{
  GNC_RETURN_ON_MATCH ("PRETAX", GNC_DISC_PRETAX);
  GNC_RETURN_ON_MATCH ("SAMETIME", GNC_DISC_SAMETIME);
  GNC_RETURN_ON_MATCH ("POSTTAX", GNC_DISC_POSTTAX);
  g_warning ("asked to translate unknown discount-how string %s.\n",
       str ? str : "(null)");

  return(FALSE);
}
#undef GNC_RETURN_ON_MATCH

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
  entry->notes = CACHE_INSERT ("");

  {
    gnc_numeric zero = gnc_numeric_zero ();
    entry->quantity = zero;
    entry->price = zero;
    entry->discount = zero;
  }
  entry->disc_type = GNC_AMT_TYPE_PERCENT;
  entry->disc_how = GNC_DISC_PRETAX;
  entry->taxable = TRUE;
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
  CACHE_REMOVE (entry->notes);
  if (entry->tax_values)
    gncAccountValueDestroy (entry->tax_values);
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
  if (timespec_equal (&entry->date, &date)) return;
  entry->date = date;
  mark_entry (entry);
}

void gncEntrySetDateEntered (GncEntry *entry, Timespec date)
{
  if (!entry) return;
  if (timespec_equal (&entry->date_entered, &date)) return;
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

void gncEntrySetNotes (GncEntry *entry, const char *notes)
{
  if (!entry || !notes) return;
  SET_STR (entry->notes, notes);
  mark_entry (entry);
}

void gncEntrySetQuantity (GncEntry *entry, gnc_numeric quantity)
{
  if (!entry) return;
  if (gnc_numeric_eq (entry->quantity, quantity)) return;
  entry->quantity = quantity;
  entry->values_dirty = TRUE;
  mark_entry (entry);
}

void gncEntrySetPrice (GncEntry *entry, gnc_numeric price)
{
  if (!entry) return;
  if (gnc_numeric_eq (entry->price, price)) return;
  entry->price = price;
  entry->values_dirty = TRUE;
  mark_entry (entry);
}

void gncEntrySetDiscount (GncEntry *entry, gnc_numeric discount)
{
  if (!entry) return;
  if (gnc_numeric_eq (entry->discount, discount)) return;
  entry->discount = discount;
  entry->values_dirty = TRUE;
  mark_entry (entry);
}

void gncEntrySetAccount (GncEntry *entry, Account *acc)
{
  if (!entry) return;
  if (entry->account == acc) return;
  entry->account = acc;
  mark_entry (entry);
}

/* Called from gncOrder when we're added to the Order */
void gncEntrySetOrder (GncEntry *entry, GncOrder *order)
{
  if (!entry) return;
  if (entry->order == order) return;
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
  if (entry->invoice == invoice) return;
  entry->invoice = invoice;
  mark_entry (entry);
}

void gncEntrySetTaxable (GncEntry *entry, gboolean taxable)
{
  if (!entry) return;
  if (entry->taxable == taxable) return;
  entry->taxable = taxable;
  entry->values_dirty = TRUE;
  mark_entry (entry);
}

void gncEntrySetTaxIncluded (GncEntry *entry, gboolean taxincluded)
{
  if (!entry) return;
  if (entry->taxincluded == taxincluded) return;
  entry->taxincluded = taxincluded;
  entry->values_dirty = TRUE;
  mark_entry (entry);
}

void gncEntrySetTaxTable (GncEntry *entry, GncTaxTable *table)
{
  if (!entry) return;
  if (entry->tax_table == table) return;
  if (entry->tax_table)
    gncTaxTableDecRef (entry->tax_table);
  if (table)
    gncTaxTableIncRef (table);
  entry->tax_table = table;
  entry->values_dirty = TRUE;
  mark_entry (entry);
}

void gncEntrySetDiscountType (GncEntry *entry, GncAmountType type)
{
  if (!entry) return;
  if (entry->disc_type == type) return;

  entry->disc_type = type;
  entry->values_dirty = TRUE;
  mark_entry (entry);
}

void gncEntrySetDiscountHow (GncEntry *entry, GncDiscountHow how)
{
  if (!entry) return;
  if (entry->disc_how == how) return;

  entry->disc_how = how;
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
  gncEntrySetNotes (dest, src->notes);
  dest->quantity		= src->quantity;
  dest->price			= src->price;
  dest->discount		= src->discount;
  dest->disc_type		= src->disc_type;
  dest->account			= src->account;
  dest->taxable			= src->taxable;
  dest->taxincluded		= src->taxincluded;

  if (src->tax_table)
    gncEntrySetTaxTable (dest, src->tax_table);

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

const char * gncEntryGetNotes (GncEntry *entry)
{
  if (!entry) return NULL;
  return entry->notes;
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

GncAmountType gncEntryGetDiscountType (GncEntry *entry)
{
  if (!entry) return 0;
  return entry->disc_type;
}

GncDiscountHow gncEntryGetDiscountHow (GncEntry *entry)
{
  if (!entry) return 0;
  return entry->disc_how;
}

gboolean gncEntryGetTaxable (GncEntry *entry)
{
  if (!entry) return FALSE;
  return entry->taxable;
}

gboolean gncEntryGetTaxIncluded (GncEntry *entry)
{
  if (!entry) return FALSE;
  return entry->taxincluded;
}

GncTaxTable * gncEntryGetTaxTable (GncEntry *entry)
{
  if (!entry) return NULL;
  return entry->tax_table;
}

GncEntry * gncEntryLookup (GNCBook *book, const GUID *guid)
{
  if (!book || !guid) return NULL;
  return xaccLookupEntity (gnc_book_get_entity_table (book),
			   guid, _GNC_MOD_NAME);
}

/*
 * This is the logic of computing the total for an Entry, so you know
 * what values to put into various Splits or to display in the ledger.
 * In other words, we combine the quantity, unit-price, discount and
 * taxes together, depending on various flags.
 *
 * There are four potental ways to combine these numbers:
 * Discount:     Pre-Tax   Post-Tax
 *   Tax   :     Included  Not-Included
 *
 * The process is relatively simple:
 *
 *  1) compute the agregate price (price*qty)
 *  2) if taxincluded, then back-compute the agregate pre-tax price
 *  3) apply discount and taxes in the appropriate order
 *  4) return the requested results.
 *
 * step 2 can be done with agregate taxes; no need to compute them all
 * unless the caller asked for the tax_value.
 *
 * Note that the returned "value" is such that value + tax == "total
 * to pay," which means in the case of tax-included that the returned
 * "value" may be less than the agregate price, even without a
 * discount.  If you want to display the tax-included value, you need
 * to add the value and taxes together.  In other words, the value is
 * the amount the merchant gets; the taxes are the amount the gov't
 * gets, and the customer pays the sum or value + taxes.
 *
 * The discount return value is just for entertainment -- you may way
 * to let a consumer know how much they saved.
 */
void gncEntryComputeValue (gnc_numeric qty, gnc_numeric price,
			   GncTaxTable *tax_table, gboolean tax_included,
			   gnc_numeric discount, GncAmountType discount_type,
			   GncDiscountHow discount_how,
			   gnc_numeric *value, gnc_numeric *discount_value,
			   GList **tax_value)
{
  gnc_numeric	aggregate;
  gnc_numeric	pretax;
  gnc_numeric	result;
  gnc_numeric	tax;
  gnc_numeric	percent = gnc_numeric_create (100, 1);
  gnc_numeric	tpercent = gnc_numeric_zero ();
  gnc_numeric	tvalue = gnc_numeric_zero ();

  GList * 	entries = gncTaxTableGetEntries (tax_table);
  GList * 	node;

  /* Step 1: compute the aggregate price */

  aggregate = gnc_numeric_mul (qty, price, GNC_DENOM_AUTO, GNC_DENOM_LCD);

  /* Step 2: compute the pre-tax aggregate */

  /* First, compute the aggregate tpercent and tvalue numbers */
  for (node = entries; node; node = node->next) {
    GncTaxTableEntry *entry = node->data;
    gnc_numeric amount = gncTaxTableEntryGetAmount (entry);

    switch (gncTaxTableEntryGetType (entry)) {
    case GNC_AMT_TYPE_VALUE:
      tvalue = gnc_numeric_add (tvalue, amount, GNC_DENOM_AUTO,
				GNC_DENOM_LCD);
      break;
    case GNC_AMT_TYPE_PERCENT:
      tpercent = gnc_numeric_add (tpercent, amount, GNC_DENOM_AUTO,
				  GNC_DENOM_LCD);
      break;
    default:
      g_warning ("Unknown tax type: %d", gncTaxTableEntryGetType (entry));
    }
  }
  /* now we need to convert from 5% -> .05 */
  tpercent = gnc_numeric_div (tpercent, percent, GNC_DENOM_AUTO,
			      GNC_DENOM_LCD);

  /* Next, actually compute the pre-tax aggregate value based on the
   * taxincluded flag.
   */
  if (tax_table && tax_included) {
    /* Back-compute the pre-tax aggregate value.
     * We know that aggregate = pretax + pretax*tpercent + tvalue, so
     * pretax = (aggregate-tvalue)/(1+tpercent)
     */
    pretax = gnc_numeric_sub (aggregate, tvalue, GNC_DENOM_AUTO,
			      GNC_DENOM_LCD);
    pretax = gnc_numeric_div (pretax,
			      gnc_numeric_add (tpercent,
					       gnc_numeric_create (1, 1),
					       GNC_DENOM_AUTO, GNC_DENOM_LCD),
			      GNC_DENOM_AUTO, GNC_DENOM_LCD);
  } else {
    pretax = aggregate;
  }

  /* Step 3:  apply discount and taxes in the appropriate order */

  /*
   * There are two ways to apply discounts and taxes.  In one way, you
   * always compute the discount off the pretax number, and compute
   * the taxes off of either the pretax value or "pretax-discount"
   * value.  In the other way, you always compute the tax on "pretax",
   * and compute the discount on either "pretax" or "pretax+taxes".
   *
   * I don't know which is the "correct" way.
   */

  /*
   * Type:	discount	tax
   * PRETAX	pretax		pretax-discount
   * SAMETIME	pretax		pretax
   * POSTTAX	pretax+tax	pretax
   */

  switch (discount_how) {
  case GNC_DISC_PRETAX:
  case GNC_DISC_SAMETIME:
    /* compute the discount from pretax */

    if (discount_type == GNC_AMT_TYPE_PERCENT) {
      discount = gnc_numeric_div (discount, percent, GNC_DENOM_AUTO, 
				  GNC_DENOM_LCD);
      discount = gnc_numeric_mul (pretax, discount, GNC_DENOM_AUTO,
				  GNC_DENOM_LCD);
    }

    result = gnc_numeric_sub (pretax, discount, GNC_DENOM_AUTO, GNC_DENOM_LCD);

    /* Figure out when to apply the tax, pretax or pretax-discount */
    if (discount_how == GNC_DISC_PRETAX)
      pretax = result;
    break;

  case GNC_DISC_POSTTAX:
    /* compute discount on pretax+taxes */

    if (discount_type == GNC_AMT_TYPE_PERCENT) {
      gnc_numeric after_tax;

      tax = gnc_numeric_mul (pretax, tpercent, GNC_DENOM_AUTO, GNC_DENOM_LCD);
      after_tax = gnc_numeric_add (pretax, tax, GNC_DENOM_AUTO, GNC_DENOM_LCD);
      after_tax = gnc_numeric_add (after_tax, tvalue, GNC_DENOM_AUTO,
				   GNC_DENOM_LCD);
      discount = gnc_numeric_div (discount, percent, GNC_DENOM_AUTO,
				  GNC_DENOM_LCD);
      discount = gnc_numeric_mul (after_tax, discount, GNC_DENOM_AUTO,
				  GNC_DENOM_LCD);
    }

    result = gnc_numeric_sub (pretax, discount, GNC_DENOM_AUTO, GNC_DENOM_LCD);
    break;

  default:
    g_warning ("unknown DiscountHow value: %d", discount_how);
  }

  /* Step 4:  return the requested results. */

  /* result == amount merchant gets
   * discount == amount of discount
   * need to compute taxes (based on 'pretax') if the caller wants it.
   */

  if (discount_value != NULL)
    *discount_value = discount;

  if (value != NULL)
    *value = result;

  /* Now... Compute the list of tax values (if the caller wants it) */

  if (tax_value != NULL) {
    GList *	taxes = NULL;

    for (node = entries; node; node = node->next) {
      GncTaxTableEntry *entry = node->data;
      Account *acc = gncTaxTableEntryGetAccount (entry);
      gnc_numeric amount = gncTaxTableEntryGetAmount (entry);

      g_return_if_fail (acc);

      switch (gncTaxTableEntryGetType (entry)) {
      case GNC_AMT_TYPE_VALUE:
	taxes = gncAccountValueAdd (taxes, acc, amount);
	break;
      case GNC_AMT_TYPE_PERCENT:
	amount = gnc_numeric_div (amount, percent, GNC_DENOM_AUTO,
				  GNC_DENOM_LCD);
	tax = gnc_numeric_mul (pretax, amount, GNC_DENOM_AUTO, GNC_DENOM_LCD);
	taxes = gncAccountValueAdd (taxes, acc, tax);
	break;
      default:
      }
    }
    *tax_value = taxes;
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

  /* See if the tax table changed since we last computed values */
  if (entry->tax_table) {
    Timespec modtime = gncTaxTableLastModified (entry->tax_table);
    if (timespec_cmp (&entry->taxtable_modtime, &modtime)) {
      entry->values_dirty = TRUE;
      entry->taxtable_modtime = modtime;
    }
  }

  if (!entry->values_dirty)
    return;

  if (entry->tax_values) {
    gncAccountValueDestroy (entry->tax_values);
    entry->tax_values = NULL;
  }

  gncEntryComputeValue (entry->quantity, entry->price,
			(entry->taxable ? entry->tax_table : NULL),
			entry->taxincluded,
			entry->discount, entry->disc_type,
			entry->disc_how,
			&(entry->value), &(entry->disc_value),
			&(entry->tax_values));

  denom = get_commodity_denom (entry);
  entry->value_rounded = gnc_numeric_convert (entry->value, denom,
					      GNC_RND_ROUND);
  entry->disc_value_rounded = gnc_numeric_convert (entry->disc_value, denom,
					      GNC_RND_ROUND);
  entry->tax_value = gncAccountValueTotal (entry->tax_values);
  entry->tax_value_rounded = gnc_numeric_convert (entry->tax_value, denom,
					      GNC_RND_ROUND);
  entry->values_dirty = FALSE;
}

void gncEntryGetValue (GncEntry *entry, gnc_numeric *value,
		       gnc_numeric *discount_value, gnc_numeric *tax_value,
		       GList **tax_values)
{
  if (!entry) return;
  gncEntryRecomputeValues (entry);
  if (value)
    *value = entry->value;
  if (discount_value)
    *discount_value = entry->disc_value;
  if (tax_value)
    *tax_value = entry->tax_value;
  if (tax_values)
    *tax_values = entry->tax_values;
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

GList * gncEntryReturnTaxValues (GncEntry *entry)
{
  if (!entry) return NULL;
  gncEntryRecomputeValues (entry);
  return entry->tax_values;
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
    { ENTRY_NOTES, QUERYCORE_STRING, (QueryAccess)gncEntryGetNotes },
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
