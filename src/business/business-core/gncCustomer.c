/*
 * gncCustomer.c -- the Core Customer Interface
 * Copyright (C) 2001,2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#include "config.h"

#include <glib.h>
#include <string.h>

#include "messages.h"
#include "gnc-engine-util.h"
#include "gnc-book-p.h"
#include "GNCIdP.h"
#include "gnc-numeric.h"
#include "gncObject.h"
#include "QueryObject.h"

#include "gncBusiness.h"
#include "gncCustomer.h"
#include "gncCustomerP.h"
#include "gncAddress.h"

struct _gncCustomer {
  GNCBook *	book;
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

#define _GNC_MOD_NAME	GNC_CUSTOMER_MODULE_NAME

#define CACHE_INSERT(str) g_cache_insert(gnc_engine_get_string_cache(), (gpointer)(str));
#define CACHE_REMOVE(str) g_cache_remove(gnc_engine_get_string_cache(), (str));

static void addObj (GncCustomer *cust);
static void remObj (GncCustomer *cust);

/* Create/Destroy Functions */

GncCustomer *gncCustomerCreate (GNCBook *book)
{
  GncCustomer *cust;

  if (!book) return NULL;

  cust = g_new0 (GncCustomer, 1);
  cust->book = book;
  cust->dirty = FALSE;
  cust->id = CACHE_INSERT ("");
  cust->name = CACHE_INSERT ("");
  cust->notes = CACHE_INSERT ("");
  cust->addr = gncAddressCreate (book);
  cust->shipaddr = gncAddressCreate (book);
  cust->discount = gnc_numeric_zero();
  cust->credit = gnc_numeric_zero();
  cust->terms = 30;
  cust->taxincluded = FALSE;
  cust->active = TRUE;
  cust->jobs = NULL;

  xaccGUIDNew (&cust->guid, book);
  addObj (cust);

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

  remObj (cust);

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

  remObj (cust);
  cust->guid = *guid;
  addObj (cust);
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
    cust->jobs = g_list_insert_sorted (cust->jobs, job,
				       (GCompareFunc)gncJobCompare);
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

GNCBook * gncCustomerGetBook (GncCustomer *cust)
{
  if (!cust) return NULL;
  return cust->book;
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

GncCustomer * gncCustomerLookup (GNCBook *book, const GUID *guid)
{
  if (!book || !guid) return NULL;
  return xaccLookupEntity (gnc_book_get_entity_table (book),
			   guid, _GNC_MOD_NAME);
}

gboolean gncCustomerIsDirty (GncCustomer *cust)
{
  if (!cust) return FALSE;
  return (cust->dirty ||
	  gncAddressIsDirty (cust->addr) ||
	  gncAddressIsDirty (cust->shipaddr));
}

/* Other functions */

int gncCustomerCompare (GncCustomer *a, GncCustomer *b)
{
  if (!a && !b) return 0;
  if (!a && b) return 1;
  if (a && !b) return -1;

  return(strcmp(a->name, b->name));
}

/* Package-Private functions */

static void addObj (GncCustomer *cust)
{
  GHashTable *ht;

  xaccStoreEntity (gnc_book_get_entity_table (cust->book),
		   cust, &cust->guid, _GNC_MOD_NAME);

  ht = gnc_book_get_data (cust->book, _GNC_MOD_NAME);
  g_hash_table_insert (ht, &cust->guid, cust);
}

static void remObj (GncCustomer *cust)
{
  GHashTable *ht;

  xaccRemoveEntity (gnc_book_get_entity_table (cust->book), &cust->guid);
  ht = gnc_book_get_data (cust->book, _GNC_MOD_NAME);
  g_hash_table_remove (ht, &cust->guid);
}

static void _gncCustomerForeach (GNCBook *book, foreachObjectCB cb,
				 gpointer user_data)
{
  if (!book || !cb) return;
  gncBusinessForeach (book, _GNC_MOD_NAME, cb, user_data);
}

static const char * _gncCustomerPrintable (gpointer item)
{
  GncCustomer *c;

  if (!item) return NULL;

  c = item;
  return c->name;
}

static void _gncCustomerCreate (GNCBook *book)
{
  GHashTable *ht;

  if (!book) return;

  ht = guid_hash_table_new ();
  gnc_book_set_data (book, _GNC_MOD_NAME, ht);
}

static void _gncCustomerDestroy (GNCBook *book)
{
  GHashTable *ht;

  if (!book) return;

  ht = gnc_book_get_data (book, _GNC_MOD_NAME);

  /* XXX : Destroy the objects? */
  g_hash_table_destroy (ht);
}

static GncObject_t gncCustomerDesc = {
  GNC_OBJECT_VERSION,
  _GNC_MOD_NAME,
  "Customer",
  _gncCustomerCreate,
  _gncCustomerDestroy,
  _gncCustomerForeach,
  _gncCustomerPrintable
};

gboolean gncCustomerRegister (void)
{
  static QueryObjectDef params[] = {
    { CUSTOMER_GUID, QUERYCORE_GUID, (QueryAccess)gncCustomerGetGUID },
    { CUSTOMER_ID, QUERYCORE_STRING, (QueryAccess)gncCustomerGetID },
    { CUSTOMER_NAME, QUERYCORE_STRING, (QueryAccess)gncCustomerGetName },
    { QUERY_PARAM_BOOK, GNC_ID_BOOK, (QueryAccess)gncCustomerGetBook },
    { CUSTOMER_ADDR, GNC_ADDRESS_MODULE_NAME, (QueryAccess)gncCustomerGetAddr },
    { CUSTOMER_SHIPADDR, GNC_ADDRESS_MODULE_NAME, (QueryAccess)gncCustomerGetShipAddr },
    { NULL },
  };

  gncQueryObjectRegister (_GNC_MOD_NAME, (QuerySort)gncCustomerCompare,params);

  return gncObjectRegister (&gncCustomerDesc);
}

static gint lastCustomer = 27;

gint gncCustomerNextID (GNCBook *book)
{
  return ++lastCustomer;	/* XXX: Look into Database! */
}
