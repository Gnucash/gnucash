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
#include "gnc-engine-util.h"
#include "gnc-book-p.h"
#include "GNCIdP.h"
#include "QueryObject.h"
#include "gnc-event-p.h"

#include "gncBusiness.h"
#include "gncVendor.h"
#include "gncVendorP.h"
#include "gncAddress.h"

struct _gncVendor {
  GNCBook *	book;
  GUID		guid;
  char *	id;
  char *	name;
  char *	notes;
  char *	terms;
  GncAddress *	addr;
  gboolean	taxincluded;
  gboolean	active;
  GList *	jobs;
  gboolean	dirty;
};

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

  gnc_engine_generate_event (&vendor->guid, GNC_EVENT_MODIFY);
}

/* Create/Destroy Functions */

GncVendor *gncVendorCreate (GNCBook *book)
{
  GncVendor *vendor;

  if (!book) return NULL;

  vendor = g_new0 (GncVendor, 1);
  vendor->book = book;
  vendor->dirty = FALSE;
  vendor->id = CACHE_INSERT ("");
  vendor->name = CACHE_INSERT ("");
  vendor->notes = CACHE_INSERT ("");
  vendor->terms = CACHE_INSERT ("");
  vendor->addr = gncAddressCreate (book, &vendor->guid);
  vendor->taxincluded = FALSE;
  vendor->active = TRUE;

  xaccGUIDNew (&vendor->guid, book);
  addObj (vendor);

  gnc_engine_generate_event (&vendor->guid, GNC_EVENT_CREATE);

  return vendor;
}

void gncVendorDestroy (GncVendor *vendor)
{
  if (!vendor) return;

  gnc_engine_generate_event (&vendor->guid, GNC_EVENT_DESTROY);

  CACHE_REMOVE (vendor->id);
  CACHE_REMOVE (vendor->name);
  CACHE_REMOVE (vendor->notes);
  CACHE_REMOVE (vendor->terms);
  gncAddressDestroy (vendor->addr);
  g_list_free (vendor->jobs);

  remObj (vendor);

  g_free (vendor);
}

/* Set Functions */

#define SET_STR(member, str) { \
	char * tmp; \
	\
	if (!safe_strcmp (member, str)) return; \
	tmp = CACHE_INSERT (str); \
	CACHE_REMOVE (member); \
	member = tmp; \
	}

void gncVendorSetID (GncVendor *vendor, const char *id)
{
  if (!vendor) return;
  if (!id) return;
  SET_STR(vendor->id, id);
  mark_vendor (vendor);
}

void gncVendorSetName (GncVendor *vendor, const char *name)
{
  if (!vendor) return;
  if (!name) return;
  SET_STR(vendor->name, name);
  mark_vendor (vendor);
}

void gncVendorSetNotes (GncVendor *vendor, const char *notes)
{
  if (!vendor) return;
  if (!notes) return;
  SET_STR(vendor->notes, notes);
  mark_vendor (vendor);
}

void gncVendorSetGUID (GncVendor *vendor, const GUID *guid)
{
  if (!vendor || !guid) return;
  if (guid_equal (guid, &vendor->guid)) return;

  remObj (vendor);
  vendor->guid = *guid;
  addObj (vendor);
}

void gncVendorSetTerms (GncVendor *vendor, const char *terms)
{
  if (!vendor || !terms) return;
  SET_STR(vendor->terms, terms);
  mark_vendor (vendor);
}

void gncVendorSetTaxIncluded (GncVendor *vendor, gboolean taxincl)
{
  if (!vendor) return;
  if (taxincl == vendor->taxincluded) return;
  vendor->taxincluded = taxincl;
  mark_vendor (vendor);
}

void gncVendorSetActive (GncVendor *vendor, gboolean active)
{
  if (!vendor) return;
  if (active == vendor->active) return;
  vendor->active = active;
  mark_vendor (vendor);
}

/* Get Functions */

GNCBook * gncVendorGetBook (GncVendor *vendor)
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

const char * gncVendorGetTerms (GncVendor *vendor)
{
  if (!vendor) return 0;
  return vendor->terms;
}

gboolean gncVendorGetTaxIncluded (GncVendor *vendor)
{
  if (!vendor) return FALSE;
  return vendor->taxincluded;
}

gboolean gncVendorGetActive (GncVendor *vendor)
{
  if (!vendor) return FALSE;
  return vendor->active;
}

/* Note that JobList changes do not affect the "dirtiness" of the vendor */
void gncVendorAddJob (GncVendor *vendor, GncJob *job)
{
  if (!vendor) return;
  if (!job) return;

  if (g_list_index(vendor->jobs, job) == -1)
    vendor->jobs = g_list_insert_sorted (vendor->jobs, job,
					 (GCompareFunc)gncJobCompare);

  gnc_engine_generate_event (&vendor->guid, GNC_EVENT_MODIFY);
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

  gnc_engine_generate_event (&vendor->guid, GNC_EVENT_MODIFY);
}

void gncVendorCommitEdit (GncVendor *vendor)
{

  /* XXX COMMIT TO DATABASE */
  vendor->dirty = FALSE;
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

GncVendor * gncVendorLookup (GNCBook *book, const GUID *guid)
{
  if (!book || !guid) return NULL;
  return xaccLookupEntity (gnc_book_get_entity_table (book),
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

static void _gncVendorCreate (GNCBook *book)
{
  gncBusinessCreate (book, _GNC_MOD_NAME);
}

static void _gncVendorDestroy (GNCBook *book)
{
  gncBusinessDestroy (book, _GNC_MOD_NAME);
}

static gboolean _gncVendorIsDirty (GNCBook *book)
{
  return gncBusinessIsDirty (book, _GNC_MOD_NAME);
}

static void _gncVendorForeach (GNCBook *book, foreachObjectCB cb,
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

static GncObject_t gncVendorDesc = {
  GNC_OBJECT_VERSION,
  _GNC_MOD_NAME,
  "Vendor",
  _gncVendorCreate,
  _gncVendorDestroy,
  _gncVendorIsDirty,
  _gncVendorForeach,
  _gncVendorPrintable
};

gboolean gncVendorRegister (void)
{
  static QueryObjectDef params[] = {
    { VENDOR_ID, QUERYCORE_STRING, (QueryAccess)gncVendorGetID },
    { VENDOR_NAME, QUERYCORE_STRING, (QueryAccess)gncVendorGetName },
    { VENDOR_ADDR, GNC_ADDRESS_MODULE_NAME, (QueryAccess)gncVendorGetAddr },
    { QUERY_PARAM_BOOK, GNC_ID_BOOK, (QueryAccess)gncVendorGetBook },
    { QUERY_PARAM_GUID, QUERYCORE_GUID, (QueryAccess)gncVendorGetGUID },
    { NULL },
  };

  gncQueryObjectRegister (_GNC_MOD_NAME, (QuerySort)gncVendorCompare, params);

  return gncObjectRegister (&gncVendorDesc);
}

static gint lastVendor = 17;

gint gncVendorNextID (GNCBook *book)
{
  return ++lastVendor;		/* XXX: Look into Database! */
}
