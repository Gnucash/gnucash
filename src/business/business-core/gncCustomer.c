/********************************************************************\
 * gncCustomer.c -- the Core Customer Interface                     *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

/*
 * Copyright (C) 2001,2002 Derek Atkins
 * Copyright (C) 2003 Linas Vepstas <linas@linas.org>
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#include "config.h"

#include <glib.h>
#include <string.h>

#include "messages.h"
#include "gnc-engine-util.h"
#include "gnc-commodity.h"
#include "gnc-numeric.h"
#include "qofobject.h"
#include "gnc-event-p.h"
#include "gnc-be-utils.h"

#include "qofbook.h"
#include "qofclass.h"
#include "qofid-p.h"
#include "qofid.h"
#include "qofinstance.h"
#include "qofquerycore.h"
#include "qofquery.h"

#include "gncBusiness.h"
#include "gncCustomer.h"
#include "gncCustomerP.h"
#include "gncAddress.h"

struct _gncCustomer 
{
  QofInstance inst;
  char *	id;
  char *	name;
  char *	notes;
  GncBillTerm *	terms;
  GncAddress *	addr;
  GncAddress *	shipaddr;
  gnc_commodity	* currency;
  gnc_numeric	discount;
  gnc_numeric	credit;
  GncTaxIncluded taxincluded;

  gboolean	active;
  GList *	jobs;

  GncTaxTable*	taxtable;
  gboolean	taxtable_override;
};

static short	module = MOD_BUSINESS;

#define _GNC_MOD_NAME	GNC_CUSTOMER_MODULE_NAME

/* ============================================================== */
/* misc inline funcs */

#define CACHE_INSERT(str) g_cache_insert(gnc_engine_get_string_cache(), (gpointer)(str));
#define CACHE_REMOVE(str) g_cache_remove(gnc_engine_get_string_cache(), (str));

static inline void addObj (GncCustomer *cust)
{
  gncBusinessAddObject (cust->inst.book, _GNC_MOD_NAME, cust, &cust->inst.guid);
}

static inline void remObj (GncCustomer *cust)
{
  gncBusinessRemoveObject (cust->inst.book, _GNC_MOD_NAME, &cust->inst.guid);
}

G_INLINE_FUNC void mark_customer (GncCustomer *customer);
G_INLINE_FUNC void
mark_customer (GncCustomer *customer)
{
  customer->inst.dirty = TRUE;
  gncBusinessSetDirtyFlag (customer->inst.book, _GNC_MOD_NAME, TRUE);

  gnc_engine_generate_event (&customer->inst.guid, _GNC_MOD_NAME, GNC_EVENT_MODIFY);
}

/* ============================================================== */
/* Create/Destroy Functions */

GncCustomer *gncCustomerCreate (QofBook *book)
{
  GncCustomer *cust;

  if (!book) return NULL;

  cust = g_new0 (GncCustomer, 1);
  qof_instance_init (&cust->inst, book);
  cust->id = CACHE_INSERT ("");
  cust->name = CACHE_INSERT ("");
  cust->notes = CACHE_INSERT ("");
  cust->addr = gncAddressCreate (book, &cust->inst.guid, _GNC_MOD_NAME);
  cust->shipaddr = gncAddressCreate (book, &cust->inst.guid, _GNC_MOD_NAME);
  cust->discount = gnc_numeric_zero();
  cust->credit = gnc_numeric_zero();
  cust->taxincluded = GNC_TAXINCLUDED_USEGLOBAL;
  cust->active = TRUE;
  cust->jobs = NULL;

  addObj (cust);
  gnc_engine_generate_event (&cust->inst.guid, _GNC_MOD_NAME, GNC_EVENT_CREATE);

  return cust;
}

/** Create a copy of a customer, placing the copy into a new book. */
GncCustomer *
gncCloneCustomer (GncCustomer *from, QofBook *book)
{
  GncCustomer *cust;

  cust = g_new0 (GncCustomer, 1);

  qof_instance_init (&cust->inst, book);
  qof_instance_gemini (&cust->inst, &from->inst);

  cust->id = CACHE_INSERT (from->id);
  cust->name = CACHE_INSERT (from->name);
  cust->notes = CACHE_INSERT (from->notes);
  cust->discount = from->discount;
  cust->credit = from->credit;
  cust->taxincluded = from->taxincluded;
  cust->active = from->active;

  /* cust->jobs = ??? XXX fixme not sure what to do here */
  /* cust->terms = ??? XXX fixme not sure what to do here */
  /* cust->taxtable = ??? XXX fixme not sure what to do here */
  cust->addr = gncCloneAddress (from->addr, book);
  cust->shipaddr = gncCloneAddress (from->shipaddr, book);
  addObj (cust);

  gnc_engine_generate_event (&cust->inst.guid, _GNC_MOD_NAME, GNC_EVENT_CREATE);

  return cust;
}

void gncCustomerDestroy (GncCustomer *cust)
{
  if (!cust) return;
  cust->inst.do_free = TRUE;
  // ??? why not?? gncBusinessSetDirtyFlag (table->book, _GNC_MOD_NAME, TRUE);
  gncCustomerCommitEdit (cust);
}

static void gncCustomerFree (GncCustomer *cust)
{
  if (!cust) return;

  gnc_engine_generate_event (&cust->inst.guid, _GNC_MOD_NAME, GNC_EVENT_DESTROY);

  CACHE_REMOVE (cust->id);
  CACHE_REMOVE (cust->name);
  CACHE_REMOVE (cust->notes);
  gncAddressDestroy (cust->addr);
  gncAddressDestroy (cust->shipaddr);
  g_list_free (cust->jobs);

  remObj (cust);

  if (cust->terms)
    gncBillTermDecRef (cust->terms);
  if (cust->taxtable)
    gncTaxTableDecRef (cust->taxtable);

  qof_instance_release (&cust->inst);
  g_free (cust);
}

/* ============================================================== */
/* Set Functions */

#define SET_STR(obj, member, str) { \
	char * tmp; \
	\
	if (!safe_strcmp (member, str)) return; \
	gncCustomerBeginEdit (obj); \
	tmp = CACHE_INSERT (str); \
	CACHE_REMOVE (member); \
	member = tmp; \
	}

void gncCustomerSetID (GncCustomer *cust, const char *id)
{
  if (!cust) return;
  if (!id) return;
  SET_STR(cust, cust->id, id);
  mark_customer (cust);
  gncCustomerCommitEdit (cust);
}

void gncCustomerSetName (GncCustomer *cust, const char *name)
{
  if (!cust) return;
  if (!name) return;
  SET_STR(cust, cust->name, name);
  mark_customer (cust);
  gncCustomerCommitEdit (cust);
}

void gncCustomerSetNotes (GncCustomer *cust, const char *notes)
{
  if (!cust) return;
  if (!notes) return;
  SET_STR(cust, cust->notes, notes);
  mark_customer (cust);
  gncCustomerCommitEdit (cust);
}

void gncCustomerSetGUID (GncCustomer *cust, const GUID *guid)
{
  if (!cust || !guid) return;
  if (guid_equal (guid, &cust->inst.guid)) return;

  /* XXX this looks fishy, chinging guid's is deep, not transactional */
  gncCustomerBeginEdit (cust);
  remObj (cust);
  cust->inst.guid = *guid;
  addObj (cust);
  gncCustomerCommitEdit (cust);
}

void gncCustomerSetTerms (GncCustomer *cust, GncBillTerm *terms)
{
  if (!cust) return;
  if (cust->terms == terms) return;

  gncCustomerBeginEdit (cust);
  if (cust->terms)
    gncBillTermDecRef (cust->terms);
  cust->terms = terms;
  if (cust->terms)
    gncBillTermIncRef (cust->terms);
  mark_customer (cust);
  gncCustomerCommitEdit (cust);
}

void gncCustomerSetTaxIncluded (GncCustomer *cust, GncTaxIncluded taxincl)
{
  if (!cust) return;
  if (taxincl == cust->taxincluded) return;
  gncCustomerBeginEdit (cust);
  cust->taxincluded = taxincl;
  mark_customer (cust);
  gncCustomerCommitEdit (cust);
}

void gncCustomerSetActive (GncCustomer *cust, gboolean active)
{
  if (!cust) return;
  if (active == cust->active) return;
  gncCustomerBeginEdit (cust);
  cust->active = active;
  mark_customer (cust);
  gncCustomerCommitEdit (cust);
}

void gncCustomerSetDiscount (GncCustomer *cust, gnc_numeric discount)
{
  if (!cust) return;
  if (gnc_numeric_equal (discount, cust->discount)) return;
  gncCustomerBeginEdit (cust);
  cust->discount = discount;
  mark_customer (cust);
  gncCustomerCommitEdit (cust);
}

void gncCustomerSetCredit (GncCustomer *cust, gnc_numeric credit)
{
  if (!cust) return;
  if (gnc_numeric_equal (credit, cust->credit)) return;
  gncCustomerBeginEdit (cust);
  cust->credit = credit;
  mark_customer (cust);
  gncCustomerCommitEdit (cust);
}

void gncCustomerSetCurrency (GncCustomer *cust, gnc_commodity *currency)
{
  if (!cust || !currency) return;
  if (cust->currency && gnc_commodity_equal (cust->currency, currency)) return;
  gncCustomerBeginEdit (cust);
  cust->currency = currency;
  mark_customer (cust);
  gncCustomerCommitEdit (cust);
}

void gncCustomerSetTaxTableOverride (GncCustomer *customer, gboolean override)
{
  if (!customer) return;
  if (customer->taxtable_override == override) return;
  gncCustomerBeginEdit (customer);
  customer->taxtable_override = override;
  mark_customer (customer);
  gncCustomerCommitEdit (customer);
}

void gncCustomerSetTaxTable (GncCustomer *customer, GncTaxTable *table)
{
  if (!customer) return;
  if (customer->taxtable == table) return;

  gncCustomerBeginEdit (customer);
  if (customer->taxtable)
    gncTaxTableDecRef (customer->taxtable);
  if (table)
    gncTaxTableIncRef (table);
  customer->taxtable = table;
  mark_customer (customer);
  gncCustomerCommitEdit (customer);
}

/* Note that JobList changes do not affect the "dirtiness" of the customer */
void gncCustomerAddJob (GncCustomer *cust, GncJob *job)
{
  if (!cust) return;
  if (!job) return;

  if (g_list_index(cust->jobs, job) == -1)
    cust->jobs = g_list_insert_sorted (cust->jobs, job,
				       (GCompareFunc)gncJobCompare);

  gnc_engine_generate_event (&cust->inst.guid, _GNC_MOD_NAME, GNC_EVENT_MODIFY);
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
  gnc_engine_generate_event (&cust->inst.guid, _GNC_MOD_NAME, GNC_EVENT_MODIFY);
}

void gncCustomerBeginEdit (GncCustomer *cust)
{
  GNC_BEGIN_EDIT (&cust->inst, _GNC_MOD_NAME);
}

static void gncCustomerOnError (QofInstance *inst, QofBackendError errcode)
{
  PERR("Customer QofBackend Failure: %d", errcode);
}

static void gncCustomerOnDone (QofInstance *inst)
{
  GncCustomer *cust = (GncCustomer *) inst;
  cust->inst.dirty = FALSE;
  gncAddressClearDirty (cust->addr);
  gncAddressClearDirty (cust->shipaddr);
}

static void cust_free (QofInstance *inst)
{
  GncCustomer *cust = (GncCustomer *) inst;
  gncCustomerFree (cust);
}

void gncCustomerCommitEdit (GncCustomer *cust)
{
  GNC_COMMIT_EDIT_PART1 (&cust->inst);
  GNC_COMMIT_EDIT_PART2 (&cust->inst, _GNC_MOD_NAME, gncCustomerOnError,
			 gncCustomerOnDone, cust_free);
}

/* Get Functions */

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

GncBillTerm * gncCustomerGetTerms (GncCustomer *cust)
{
  if (!cust) return NULL;
  return cust->terms;
}

GncTaxIncluded gncCustomerGetTaxIncluded (GncCustomer *cust)
{
  if (!cust) return GNC_TAXINCLUDED_USEGLOBAL;
  return cust->taxincluded;
}

gnc_commodity * gncCustomerGetCurrency (GncCustomer *cust)
{
  if (!cust) return NULL;
  return cust->currency;
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

gboolean gncCustomerGetTaxTableOverride (GncCustomer *customer)
{
  if (!customer) return FALSE;
  return customer->taxtable_override;
}

GncTaxTable* gncCustomerGetTaxTable (GncCustomer *customer)
{
  if (!customer) return NULL;
  return customer->taxtable;
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

GUID gncCustomerRetGUID (GncCustomer *customer)
{
  if (!customer)
    return *guid_null();

  return customer->inst.guid;
}

GncCustomer * gncCustomerLookupDirect (GUID guid, QofBook *book)
{
  if (!book) return NULL;
  return gncCustomerLookup (book, &guid);
}

GncCustomer * gncCustomerLookup (QofBook *book, const GUID *guid)
{
  if (!book || !guid) return NULL;
  return qof_entity_lookup (gnc_book_get_entity_table (book),
			   guid, _GNC_MOD_NAME);
}

gboolean gncCustomerIsDirty (GncCustomer *cust)
{
  if (!cust) return FALSE;
  return (cust->inst.dirty ||
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

static void _gncCustomerCreate (QofBook *book)
{
  gncBusinessCreate (book, _GNC_MOD_NAME);
}

static void _gncCustomerDestroy (QofBook *book)
{
  return gncBusinessDestroy (book, _GNC_MOD_NAME);
}

static gboolean _gncCustomerIsDirty (QofBook *book)
{
  return gncBusinessIsDirty (book, _GNC_MOD_NAME);
}

static void _gncCustomerMarkClean (QofBook *book)
{
  gncBusinessSetDirtyFlag (book, _GNC_MOD_NAME, FALSE);
}

static void _gncCustomerForeach (QofBook *book, QofEntityForeachCB cb,
				 gpointer user_data)
{
  gncBusinessForeach (book, _GNC_MOD_NAME, cb, user_data);
}

static const char * _gncCustomerPrintable (gpointer item)
{
  GncCustomer *c;

  if (!item) return NULL;

  c = item;
  return c->name;
}

static QofObject gncCustomerDesc = {
  QOF_OBJECT_VERSION,
  _GNC_MOD_NAME,
  "Customer",
  _gncCustomerCreate,
  _gncCustomerDestroy,
  _gncCustomerIsDirty,
  _gncCustomerMarkClean,
  _gncCustomerForeach,
  _gncCustomerPrintable,
};

gboolean gncCustomerRegister (void)
{
  static QofParam params[] = {
    { CUSTOMER_ID, QOF_TYPE_STRING, (QofAccessFunc)gncCustomerGetID, NULL },
    { CUSTOMER_NAME, QOF_TYPE_STRING, (QofAccessFunc)gncCustomerGetName, NULL },
    { CUSTOMER_ADDR, GNC_ADDRESS_MODULE_NAME, (QofAccessFunc)gncCustomerGetAddr, NULL },
    { CUSTOMER_SHIPADDR, GNC_ADDRESS_MODULE_NAME, (QofAccessFunc)gncCustomerGetShipAddr, NULL },
    { QOF_QUERY_PARAM_ACTIVE, QOF_TYPE_BOOLEAN, (QofAccessFunc)gncCustomerGetActive, NULL },
    { QOF_QUERY_PARAM_BOOK, QOF_ID_BOOK, (QofAccessFunc)qof_instance_get_book, NULL },
    { QOF_QUERY_PARAM_GUID, QOF_TYPE_GUID, (QofAccessFunc)qof_instance_get_guid, NULL },
    { NULL },
  };

  qof_class_register (_GNC_MOD_NAME, (QofSortFunc)gncCustomerCompare,params);

  return qof_object_register (&gncCustomerDesc);
}

gint64 gncCustomerNextID (QofBook *book)
{
  return gnc_book_get_counter (book, _GNC_MOD_NAME);
}
