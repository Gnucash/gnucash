/*
 * gncVendor.c -- the Core Vendor Interface
 * Copyright (C) 2001 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#include "config.h"

#include <glib.h>
#include <string.h>

#include "guid.h"
#include "messages.h"
#include "gnc-engine-util.h"

#include "gncVendor.h"
#include "gncVendorP.h"
#include "gncAddress.h"
#include "gncBusiness.h"

struct _gncVendor {
  GncBusiness *	business;
  GUID		guid;
  char *	id;
  char *	name;
  char *	notes;
  GncAddress *	addr;
  gint		terms;
  gboolean	taxincluded;
  gboolean	active;
  gboolean	dirty;
};

#define CACHE_INSERT(str) g_cache_insert(gnc_engine_get_string_cache(), (gpointer)(str));
#define CACHE_REMOVE(str) g_cache_remove(gnc_engine_get_string_cache(), (str));

/* Create/Destroy Functions */

GncVendor *gncVendorCreate (GncBusiness *business)
{
  GncVendor *vendor;

  if (!business) return NULL;

  vendor = g_new0 (GncVendor, 1);
  vendor->business = business;
  vendor->dirty = FALSE;
  vendor->id = CACHE_INSERT ("");
  vendor->name = CACHE_INSERT ("");
  vendor->notes = CACHE_INSERT ("");
  vendor->addr = gncAddressCreate (business);
  vendor->terms = 0;
  vendor->taxincluded = FALSE;
  vendor->active = TRUE;

  guid_new (&vendor->guid);

  gncBusinessAddEntity (business, GNC_VENDOR_MODULE_NAME, &vendor->guid,
			vendor);

  return vendor;
}

void gncVendorDestroy (GncVendor *vendor)
{
  if (!vendor) return;

  CACHE_REMOVE (vendor->id);
  CACHE_REMOVE (vendor->name);
  CACHE_REMOVE (vendor->notes);
  gncAddressDestroy (vendor->addr);

  gncBusinessRemoveEntity (vendor->business, GNC_VENDOR_MODULE_NAME,
			   &vendor->guid);

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
  vendor->dirty = TRUE;
}

void gncVendorSetName (GncVendor *vendor, const char *name)
{
  if (!vendor) return;
  if (!name) return;
  SET_STR(vendor->name, name);
  vendor->dirty = TRUE;
}

void gncVendorSetNotes (GncVendor *vendor, const char *notes)
{
  if (!vendor) return;
  if (!notes) return;
  SET_STR(vendor->notes, notes);
  vendor->dirty = TRUE;
}

void gncVendorSetGUID (GncVendor *vendor, const GUID *guid)
{
  if (!vendor || !guid) return;
  if (guid_equal (guid, &vendor->guid)) return;
  gncBusinessRemoveEntity (vendor->business, GNC_VENDOR_MODULE_NAME,
			   &vendor->guid);
  vendor->guid = *guid;
  gncBusinessAddEntity (vendor->business, GNC_VENDOR_MODULE_NAME,
			&vendor->guid, vendor);
}

void gncVendorSetTerms (GncVendor *vendor, gint terms)
{
  if (!vendor) return;
  if (terms == vendor->terms) return;
  vendor->terms = terms;
  vendor->dirty = TRUE;
}

void gncVendorSetTaxIncluded (GncVendor *vendor, gboolean taxincl)
{
  if (!vendor) return;
  if (taxincl == vendor->taxincluded) return;
  vendor->taxincluded = taxincl;
  vendor->dirty = TRUE;
}

void gncVendorSetActive (GncVendor *vendor, gboolean active)
{
  if (!vendor) return;
  if (active == vendor->active) return;
  vendor->active = active;
  vendor->dirty = TRUE;
}

/* Get Functions */

GncBusiness * gncVendorGetBusiness (GncVendor *vendor)
{
  if (!vendor) return NULL;
  return vendor->business;
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

gint gncVendorGetTerms (GncVendor *vendor)
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

gboolean gncVendorIsDirty (GncVendor *vendor)
{
  if (!vendor) return FALSE;
  return (vendor->dirty || gncAddressIsDirty (vendor->addr));
}

void gncVendorCommitEdit (GncVendor *vendor)
{

  /* XXX COMMIT TO DATABASE */
  vendor->dirty = FALSE;
}

/* Other functions */

static gint gncVendorSortFunc (gconstpointer a, gconstpointer b) {
  GncVendor *va = (GncVendor *) a;
  GncVendor *vb = (GncVendor *) b;
  return(strcmp(va->name, vb->name));
}

/* Package-Private functions */

struct _iterate {
  GList *list;
  gboolean show_all;
};

static void get_list (gpointer key, gpointer item, gpointer arg)
{
  struct _iterate *iter = arg;
  GncVendor *vendor = item;

  if (iter->show_all || gncVendorGetActive (vendor)) {
    iter->list = g_list_insert_sorted (iter->list, vendor, gncVendorSortFunc);
  }
}

static GList * _gncVendorGetList (GncBusiness *bus, gboolean show_all)
{
  GHashTable *ht;
  struct _iterate iter;

  if (!bus) return NULL;

  iter.list = NULL;
  iter.show_all = show_all;

  ht = gncBusinessEntityTable (bus, GNC_VENDOR_MODULE_NAME);
  if (ht)
    g_hash_table_foreach (ht, get_list, &iter);

  return iter.list;
}

static const char * _gncVendorPrintable (gpointer item)
{
  GncVendor *v;

  if (!item) return NULL;

  v = item;
  return v->name;
}

static void _gncVendorDestroy (GncBusiness *obj)
{
  if (!obj) return;

  /* XXX: should we be sure to destroy all the vendor objects? */
}

static GncBusinessObject gncVendorDesc = {
  GNC_BUSINESS_VERSION,
  GNC_VENDOR_MODULE_NAME,
  "Vendor",
  _gncVendorDestroy,
  _gncVendorGetList,
  _gncVendorPrintable
};

gboolean gncVendorRegister (void)
{
  return gncBusinessRegister (&gncVendorDesc);
}

static gint lastVendor = 17;

gint gncVendorNextID (GncBusiness *business)
{
  return ++lastVendor;		/* XXX: Look into Database! */
}
