/*
 * gncVendor.c -- the Core Vendor Interface
 * Copyright (C) 2001, 2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#include "config.h"

#include <glib.h>
#include <string.h>

#include "guid.h"
#include "messages.h"
#include "gnc-book.h"
#include "gnc-commodity.h"
#include "gnc-engine-util.h"
#include "gnc-event-p.h"
#include "qofid.h"
#include "qofquerycore.h"
#include "qofquery.h"
#include "qofqueryobject.h"
#include "gnc-be-utils.h"
#include "qofid-p.h"

#include "gncBusiness.h"
#include "gncVendor.h"
#include "gncVendorP.h"
#include "gncAddress.h"

struct _gncVendor {
  QofBook *	book;
  GUID		guid;
  char *	id;
  char *	name;
  char *	notes;
  GncBillTerm *	terms;
  GncAddress *	addr;
  gnc_commodity * currency;
  GncTaxIncluded taxincluded;
  gboolean	active;
  GList *	jobs;
  GncTaxTable*	taxtable;
  gboolean	taxtable_override;

  int		editlevel;
  gboolean	do_free;

  gboolean	dirty;
};

static short	module = MOD_BUSINESS;

#define _GNC_MOD_NAME	GNC_VENDOR_MODULE_NAME

#define CACHE_INSERT(str) g_cache_insert(gnc_engine_get_string_cache(), (gpointer)(str));
#define CACHE_REMOVE(str) g_cache_remove(gnc_engine_get_string_cache(), (str));

static void addObj (GncVendor *vendor);
static void remObj (GncVendor *vendor);

G_INLINE_FUNC void mark_vendor (GncVendor *vendor);
G_INLINE_FUNC void
mark_vendor (GncVendor *vendor)
{
  vendor->dirty = TRUE;
  gncBusinessSetDirtyFlag (vendor->book, _GNC_MOD_NAME, TRUE);

  gnc_engine_generate_event (&vendor->guid, _GNC_MOD_NAME, GNC_EVENT_MODIFY);
}

/* Create/Destroy Functions */

GncVendor *gncVendorCreate (QofBook *book)
{
  GncVendor *vendor;

  if (!book) return NULL;

  vendor = g_new0 (GncVendor, 1);
  vendor->book = book;
  vendor->dirty = FALSE;
  vendor->id = CACHE_INSERT ("");
  vendor->name = CACHE_INSERT ("");
  vendor->notes = CACHE_INSERT ("");
  vendor->addr = gncAddressCreate (book, &vendor->guid, _GNC_MOD_NAME);
  vendor->taxincluded = GNC_TAXINCLUDED_USEGLOBAL;
  vendor->active = TRUE;

  qof_entity_guid_new (qof_book_get_entity_table (book), &vendor->guid);
  addObj (vendor);

  gnc_engine_generate_event (&vendor->guid, _GNC_MOD_NAME, GNC_EVENT_CREATE);

  return vendor;
}

void gncVendorDestroy (GncVendor *vendor)
{
  if (!vendor) return;
  vendor->do_free = TRUE;
  gncVendorCommitEdit (vendor);
}

static void gncVendorFree (GncVendor *vendor)
{
  if (!vendor) return;

  gnc_engine_generate_event (&vendor->guid, _GNC_MOD_NAME, GNC_EVENT_DESTROY);

  CACHE_REMOVE (vendor->id);
  CACHE_REMOVE (vendor->name);
  CACHE_REMOVE (vendor->notes);
  gncAddressDestroy (vendor->addr);
  g_list_free (vendor->jobs);

  remObj (vendor);

  if (vendor->terms)
    gncBillTermDecRef (vendor->terms);
  if (vendor->taxtable)
    gncTaxTableDecRef (vendor->taxtable);

  g_free (vendor);
}

/* Set Functions */

#define SET_STR(obj, member, str) { \
	char * tmp; \
	\
	if (!safe_strcmp (member, str)) return; \
	gncVendorBeginEdit (obj); \
	tmp = CACHE_INSERT (str); \
	CACHE_REMOVE (member); \
	member = tmp; \
	}

void gncVendorSetID (GncVendor *vendor, const char *id)
{
  if (!vendor) return;
  if (!id) return;
  SET_STR(vendor, vendor->id, id);
  mark_vendor (vendor);
  gncVendorCommitEdit (vendor);
}

void gncVendorSetName (GncVendor *vendor, const char *name)
{
  if (!vendor) return;
  if (!name) return;
  SET_STR(vendor, vendor->name, name);
  mark_vendor (vendor);
  gncVendorCommitEdit (vendor);
}

void gncVendorSetNotes (GncVendor *vendor, const char *notes)
{
  if (!vendor) return;
  if (!notes) return;
  SET_STR(vendor,vendor->notes, notes);
  mark_vendor (vendor);
  gncVendorCommitEdit (vendor);
}

void gncVendorSetGUID (GncVendor *vendor, const GUID *guid)
{
  if (!vendor || !guid) return;
  if (guid_equal (guid, &vendor->guid)) return;

  gncVendorBeginEdit (vendor);
  remObj (vendor);
  vendor->guid = *guid;
  addObj (vendor);
  gncVendorCommitEdit (vendor);
}

void gncVendorSetTerms (GncVendor *vendor, GncBillTerm *terms)
{
  if (!vendor) return;
  if (vendor->terms == terms) return;

  gncVendorBeginEdit (vendor);
  if (vendor->terms)
    gncBillTermDecRef (vendor->terms);
  vendor->terms = terms;
  if (vendor->terms)
    gncBillTermDecRef (vendor->terms);
  mark_vendor (vendor);
  gncVendorCommitEdit (vendor);
}

void gncVendorSetTaxIncluded (GncVendor *vendor, GncTaxIncluded taxincl)
{
  if (!vendor) return;
  if (taxincl == vendor->taxincluded) return;
  gncVendorBeginEdit (vendor);
  vendor->taxincluded = taxincl;
  mark_vendor (vendor);
  gncVendorCommitEdit (vendor);
}

void gncVendorSetCurrency (GncVendor *vendor, gnc_commodity *currency)
{
  if (!vendor || !currency) return;
  if (vendor->currency &&
      gnc_commodity_equal (vendor->currency, currency))
    return;
  gncVendorBeginEdit (vendor);
  vendor->currency = currency;
  mark_vendor (vendor);
  gncVendorCommitEdit (vendor);
}

void gncVendorSetActive (GncVendor *vendor, gboolean active)
{
  if (!vendor) return;
  if (active == vendor->active) return;
  gncVendorBeginEdit (vendor);
  vendor->active = active;
  mark_vendor (vendor);
  gncVendorCommitEdit (vendor);
}

void gncVendorSetTaxTableOverride (GncVendor *vendor, gboolean override)
{
  if (!vendor) return;
  if (vendor->taxtable_override == override) return;
  gncVendorBeginEdit (vendor);
  vendor->taxtable_override = override;
  mark_vendor (vendor);
  gncVendorCommitEdit (vendor);
}

void gncVendorSetTaxTable (GncVendor *vendor, GncTaxTable *table)
{
  if (!vendor) return;
  if (vendor->taxtable == table) return;
  gncVendorBeginEdit (vendor);
  if (vendor->taxtable)
    gncTaxTableDecRef (vendor->taxtable);
  if (table)
    gncTaxTableIncRef (table);
  vendor->taxtable = table;
  mark_vendor (vendor);
  gncVendorCommitEdit (vendor);
}

/* Get Functions */

QofBook * gncVendorGetBook (GncVendor *vendor)
{
  if (!vendor) return NULL;
  return vendor->book;
}

const GUID * gncVendorGetGUID (GncVendor *vendor)
{
  if (!vendor) return NULL;
  return &vendor->guid;
}

const char * gncVendorGetID (GncVendor *vendor)
{
  if (!vendor) return NULL;
  return vendor->id;
}

const char * gncVendorGetName (GncVendor *vendor)
{
  if (!vendor) return NULL;
  return vendor->name;
}

GncAddress * gncVendorGetAddr (GncVendor *vendor)
{
  if (!vendor) return NULL;
  return vendor->addr;
}

const char * gncVendorGetNotes (GncVendor *vendor)
{
  if (!vendor) return NULL;
  return vendor->notes;
}

GncBillTerm * gncVendorGetTerms (GncVendor *vendor)
{
  if (!vendor) return 0;
  return vendor->terms;
}

GncTaxIncluded gncVendorGetTaxIncluded (GncVendor *vendor)
{
  if (!vendor) return GNC_TAXINCLUDED_USEGLOBAL;
  return vendor->taxincluded;
}

gnc_commodity * gncVendorGetCurrency (GncVendor *vendor)
{
  if (!vendor) return NULL;
  return vendor->currency;
}

gboolean gncVendorGetActive (GncVendor *vendor)
{
  if (!vendor) return FALSE;
  return vendor->active;
}

gboolean gncVendorGetTaxTableOverride (GncVendor *vendor)
{
  if (!vendor) return FALSE;
  return vendor->taxtable_override;
}

GncTaxTable* gncVendorGetTaxTable (GncVendor *vendor)
{
  if (!vendor) return NULL;
  return vendor->taxtable;
}

/* Note that JobList changes do not affect the "dirtiness" of the vendor */
void gncVendorAddJob (GncVendor *vendor, GncJob *job)
{
  if (!vendor) return;
  if (!job) return;

  if (g_list_index(vendor->jobs, job) == -1)
    vendor->jobs = g_list_insert_sorted (vendor->jobs, job,
					 (GCompareFunc)gncJobCompare);

  gnc_engine_generate_event (&vendor->guid, _GNC_MOD_NAME, GNC_EVENT_MODIFY);
}

void gncVendorRemoveJob (GncVendor *vendor, GncJob *job)
{
  GList *node;

  if (!vendor) return;
  if (!job) return;

  node = g_list_find (vendor->jobs, job);
  if (!node) {
    /*    PERR ("split not in account"); */
  } else {
    vendor->jobs = g_list_remove_link (vendor->jobs, node);
    g_list_free_1 (node);
  }

  gnc_engine_generate_event (&vendor->guid, _GNC_MOD_NAME, GNC_EVENT_MODIFY);
}

void gncVendorBeginEdit (GncVendor *vendor)
{
  GNC_BEGIN_EDIT (vendor, _GNC_MOD_NAME);
}

static void gncVendorOnError (GncVendor *vendor, QofBackendError errcode)
{
  PERR("Vendor QofBackend Failure: %d", errcode);
}

static void gncVendorOnDone (GncVendor *vendor)
{
  vendor->dirty = FALSE;
  gncAddressClearDirty (vendor->addr);
}

void gncVendorCommitEdit (GncVendor *vendor)
{
  GNC_COMMIT_EDIT_PART1 (vendor);
  GNC_COMMIT_EDIT_PART2 (vendor, _GNC_MOD_NAME, gncVendorOnError,
			 gncVendorOnDone, gncVendorFree);
}

/* Other functions */

int gncVendorCompare (GncVendor *a, GncVendor *b)
{
  if (!a && !b) return 0;
  if (!a && b) return 1;
  if (a && !b) return -1;

  return(strcmp(a->name, b->name));
}

GList * gncVendorGetJoblist (GncVendor *vendor, gboolean show_all)
{
  if (!vendor) return NULL;

  if (show_all) {
    return (g_list_copy (vendor->jobs));
  } else {
    GList *list = NULL, *iterator;
    for (iterator = vendor->jobs; iterator; iterator=iterator->next) {
      GncJob *j = iterator->data;
      if (gncJobGetActive (j))
	list = g_list_append (list, j);
    }
    return list;
  }
}

GUID gncVendorRetGUID (GncVendor *vendor)
{
  if (!vendor)
    return *guid_null();

  return vendor->guid;
}

GncVendor * gncVendorLookupDirect (GUID guid, QofBook *book)
{
  if (!book) return NULL;
  return gncVendorLookup (book, &guid);
}

GncVendor * gncVendorLookup (QofBook *book, const GUID *guid)
{
  if (!book || !guid) return NULL;
  return qof_entity_lookup (gnc_book_get_entity_table (book),
			   guid, _GNC_MOD_NAME);
}

gboolean gncVendorIsDirty (GncVendor *vendor)
{
  if (!vendor) return FALSE;
  return (vendor->dirty || gncAddressIsDirty (vendor->addr));
}

/* Package-Private functions */

static void addObj (GncVendor *vendor)
{
  gncBusinessAddObject (vendor->book, _GNC_MOD_NAME, vendor, &vendor->guid);
}

static void remObj (GncVendor *vendor)
{
  gncBusinessRemoveObject (vendor->book, _GNC_MOD_NAME, &vendor->guid);
}

static void _gncVendorCreate (QofBook *book)
{
  gncBusinessCreate (book, _GNC_MOD_NAME);
}

static void _gncVendorDestroy (QofBook *book)
{
  gncBusinessDestroy (book, _GNC_MOD_NAME);
}

static gboolean _gncVendorIsDirty (QofBook *book)
{
  return gncBusinessIsDirty (book, _GNC_MOD_NAME);
}

static void _gncVendorMarkClean (QofBook *book)
{
  gncBusinessSetDirtyFlag (book, _GNC_MOD_NAME, FALSE);
}

static void _gncVendorForeach (QofBook *book, QofEntityForeachCB cb,
			       gpointer user_data)
{
  gncBusinessForeach (book, _GNC_MOD_NAME, cb, user_data);
}

static const char * _gncVendorPrintable (gpointer item)
{
  GncVendor *v;

  if (!item) return NULL;

  v = item;
  return v->name;
}

static QofObject gncVendorDesc = {
  QOF_OBJECT_VERSION,
  _GNC_MOD_NAME,
  "Vendor",
  _gncVendorCreate,
  _gncVendorDestroy,
  _gncVendorIsDirty,
  _gncVendorMarkClean,
  _gncVendorForeach,
  _gncVendorPrintable
};

gboolean gncVendorRegister (void)
{
  static QofParam params[] = {
    { VENDOR_ID, QOF_TYPE_STRING, (QofAccessFunc)gncVendorGetID, NULL },
    { VENDOR_NAME, QOF_TYPE_STRING, (QofAccessFunc)gncVendorGetName, NULL },
    { VENDOR_ADDR, GNC_ADDRESS_MODULE_NAME, (QofAccessFunc)gncVendorGetAddr, NULL },
    { QOF_QUERY_PARAM_BOOK, QOF_ID_BOOK, (QofAccessFunc)gncVendorGetBook, NULL },
    { QOF_QUERY_PARAM_GUID, QOF_TYPE_GUID, (QofAccessFunc)gncVendorGetGUID, NULL },
    { QOF_QUERY_PARAM_ACTIVE, QOF_TYPE_BOOLEAN, (QofAccessFunc)gncVendorGetActive, NULL },
    { NULL },
  };

  qof_class_register (_GNC_MOD_NAME, (QofSortFunc)gncVendorCompare, params);

  return qof_object_register (&gncVendorDesc);
}

gint64 gncVendorNextID (QofBook *book)
{
  return gnc_book_get_counter (book, _GNC_MOD_NAME);
}
