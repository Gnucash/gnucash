/*
 * gncCustomer.c -- the Core Customer Interface
 * Copyright (C) 2001 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#include "config.h"

#include <glib.h>
#include <string.h>

#include "messages.h"
#include "gnc-engine-util.h"

#include "gnc-numeric.h"
#include "gncCustomer.h"
#include "gncCustomerP.h"
#include "gncAddress.h"
#include "gncBusiness.h"

struct _gncCustomer {
  GncBusiness *	business;
  GUID		guid;
  char *	id;
  char *	name;
  char *	notes;
  GncAddress *	addr;
  GncAddress *	shipaddr;
  gnc_numeric	discount;
  gnc_numeric	credit;
  gint		terms;
  gboolean	taxincluded;
  gboolean	active;
  GList *	jobs;
  gboolean	dirty;
};

#define CACHE_INSERT(str) g_cache_insert(gnc_engine_get_string_cache(), (gpointer)(str));
#define CACHE_REMOVE(str) g_cache_remove(gnc_engine_get_string_cache(), (str));

/* Create/Destroy Functions */

GncCustomer *gncCustomerCreate (GncBusiness *business)
{
  GncCustomer *cust;

  if (!business) return NULL;

  cust = g_new0 (GncCustomer, 1);
  cust->business = business;
  cust->dirty = FALSE;
  cust->id = CACHE_INSERT ("");
  cust->name = CACHE_INSERT ("");
  cust->notes = CACHE_INSERT ("");
  cust->addr = gncAddressCreate (business);
  cust->shipaddr = gncAddressCreate (business);
  cust->discount = gnc_numeric_zero();
  cust->credit = gnc_numeric_zero();
  cust->terms = 30;
  cust->taxincluded = FALSE;
  cust->active = TRUE;
  cust->jobs = NULL;

  guid_new (&cust->guid);

  gncBusinessAddEntity (business, GNC_CUSTOMER_MODULE_NAME,
			&cust->guid, cust);

  return cust;
}

void gncCustomerDestroy (GncCustomer *cust)
{
  if (!cust) return;

  CACHE_REMOVE (cust->id);
  CACHE_REMOVE (cust->name);
  CACHE_REMOVE (cust->notes);
  gncAddressDestroy (cust->addr);
  gncAddressDestroy (cust->shipaddr);
  g_list_free (cust->jobs);

  gncBusinessRemoveEntity (cust->business, GNC_CUSTOMER_MODULE_NAME,
			   &cust->guid);

  g_free (cust);
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

void gncCustomerSetID (GncCustomer *cust, const char *id)
{
  if (!cust) return;
  if (!id) return;
  SET_STR(cust->id, id);
  cust->dirty = TRUE;
}

void gncCustomerSetName (GncCustomer *cust, const char *name)
{
  if (!cust) return;
  if (!name) return;
  SET_STR(cust->name, name);
  cust->dirty = TRUE;
}

void gncCustomerSetNotes (GncCustomer *cust, const char *notes)
{
  if (!cust) return;
  if (!notes) return;
  SET_STR(cust->notes, notes);
  cust->dirty = TRUE;
}

void gncCustomerSetGUID (GncCustomer *cust, const GUID *guid)
{
  if (!cust || !guid) return;
  if (guid_equal (guid, &cust->guid)) return;
  gncBusinessRemoveEntity (cust->business, GNC_CUSTOMER_MODULE_NAME,
			   &cust->guid);
  cust->guid = *guid;
  gncBusinessAddEntity (cust->business, GNC_CUSTOMER_MODULE_NAME, &cust->guid,
			cust);
}

void gncCustomerSetTerms (GncCustomer *cust, gint terms)
{
  if (!cust) return;
  if (terms == cust->terms) return;
  cust->terms = terms;
  cust->dirty = TRUE;
}

void gncCustomerSetTaxIncluded (GncCustomer *cust, gboolean taxincl)
{
  if (!cust) return;
  if (taxincl == cust->taxincluded) return;
  cust->taxincluded = taxincl;
  cust->dirty = TRUE;
}

void gncCustomerSetActive (GncCustomer *cust, gboolean active)
{
  if (!cust) return;
  if (active == cust->active) return;
  cust->active = active;
  cust->dirty = TRUE;
}

void gncCustomerSetDiscount (GncCustomer *cust, gnc_numeric discount)
{
  if (!cust) return;
  if (gnc_numeric_equal (discount, cust->discount)) return;
  cust->discount = discount;
  cust->dirty = TRUE;
}

void gncCustomerSetCredit (GncCustomer *cust, gnc_numeric credit)
{
  if (!cust) return;
  if (gnc_numeric_equal (credit, cust->credit)) return;
  cust->credit = credit;
  cust->dirty = TRUE;
}

/* Note that JobList changes do not affect the "dirtiness" of the customer */
void gncCustomerAddJob (GncCustomer *cust, GncJob *job)
{
  if (!cust) return;
  if (!job) return;

  if (g_list_index(cust->jobs, job) == -1)
    cust->jobs = g_list_insert_sorted (cust->jobs, job, gncJobSortFunc);
}

void gncCustomerRemoveJob (GncCustomer *cust, GncJob *job)
{
  GList *node;

  if (!cust) return;
  if (!job) return;

  node = g_list_find (cust->jobs, job);
  if (!node) {
    /*    PERR ("split not in account"); */
  } else {
    cust->jobs = g_list_remove_link (cust->jobs, node);
    g_list_free_1 (node);
  }
}

void gncCustomerCommitEdit (GncCustomer *cust)
{
  /* XXX COMMIT TO DATABASE */
  cust->dirty = FALSE;
}

/* Get Functions */

GncBusiness * gncCustomerGetBusiness (GncCustomer *cust)
{
  if (!cust) return NULL;
  return cust->business;
}

const GUID * gncCustomerGetGUID (GncCustomer *cust)
{
  if (!cust) return NULL;
  return &cust->guid;
}

const char * gncCustomerGetID (GncCustomer *cust)
{
  if (!cust) return NULL;
  return cust->id;
}

const char * gncCustomerGetName (GncCustomer *cust)
{
  if (!cust) return NULL;
  return cust->name;
}

GncAddress * gncCustomerGetAddr (GncCustomer *cust)
{
  if (!cust) return NULL;
  return cust->addr;
}

GncAddress * gncCustomerGetShipAddr (GncCustomer *cust)
{
  if (!cust) return NULL;
  return cust->shipaddr;
}

const char * gncCustomerGetNotes (GncCustomer *cust)
{
  if (!cust) return NULL;
  return cust->notes;
}

gint gncCustomerGetTerms (GncCustomer *cust)
{
  if (!cust) return 0;
  return cust->terms;
}

gboolean gncCustomerGetTaxIncluded (GncCustomer *cust)
{
  if (!cust) return FALSE;
  return cust->taxincluded;
}

gboolean gncCustomerGetActive (GncCustomer *cust)
{
  if (!cust) return FALSE;
  return cust->active;
}

gnc_numeric gncCustomerGetDiscount (GncCustomer *cust)
{
  if (!cust) return gnc_numeric_zero();
  return cust->discount;
}

gnc_numeric gncCustomerGetCredit (GncCustomer *cust)
{
  if (!cust) return gnc_numeric_zero();
  return cust->credit;
}

GList * gncCustomerGetJoblist (GncCustomer *cust, gboolean show_all)
{
  if (!cust) return NULL;

  if (show_all) {
    return (g_list_copy (cust->jobs));
  } else {
    GList *list = NULL, *iterator;
    for (iterator = cust->jobs; iterator; iterator=iterator->next) {
      GncJob *j = iterator->data;
      if (gncJobGetActive (j))
	list = g_list_append (list, j);
    }
    return list;
  }
}

gboolean gncCustomerIsDirty (GncCustomer *cust)
{
  if (!cust) return FALSE;
  return (cust->dirty ||
	  gncAddressIsDirty (cust->addr) ||
	  gncAddressIsDirty (cust->shipaddr));
}

/* Other functions */

static gint gncCustomerSortFunc (gconstpointer a, gconstpointer b) {
  GncCustomer *ca = (GncCustomer *) a;
  GncCustomer *cb = (GncCustomer *) b;
  return(strcmp(ca->name, cb->name));
}

/* Package-Private functions */

struct _iterate {
  GList *list;
  gboolean show_all;
};

static void get_list (gpointer key, gpointer item, gpointer arg)
{
  struct _iterate *iter = arg;
  GncCustomer *cust = item;

  if (iter->show_all || gncCustomerGetActive (cust)) {
    iter->list = g_list_insert_sorted (iter->list, cust, gncCustomerSortFunc);
  }
}

static GList * _gncCustomerGetList (GncBusiness *bus, gboolean show_all)
{
  GHashTable *ht;
  struct _iterate iter;

  if (!bus) return NULL;

  iter.list = NULL;
  iter.show_all = show_all;

  ht = gncBusinessEntityTable (bus, GNC_CUSTOMER_MODULE_NAME);
  if (ht)
    g_hash_table_foreach (ht, get_list, &iter);

  return iter.list;
}

static const char * _gncCustomerPrintable (gpointer item)
{
  GncCustomer *c;

  if (!item) return NULL;

  c = item;
  return c->name;
}

static void _gncCustomerDestroy (GncBusiness *bus)
{
  if (!bus) return;

  /* XXX: should we be sure to destroy all the customer objects? */
}

static GncBusinessObject gncCustomerDesc = {
  GNC_BUSINESS_VERSION,
  GNC_CUSTOMER_MODULE_NAME,
  "Customer",
  _gncCustomerDestroy,
  _gncCustomerGetList,
  _gncCustomerPrintable
};

gboolean gncCustomerRegister (void)
{
  return gncBusinessRegister (&gncCustomerDesc);
}

static gint lastCustomer = 27;

gint gncCustomerNextID (GncBusiness *business)
{
  return ++lastCustomer;	/* XXX: Look into Database! */
}
