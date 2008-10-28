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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
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

#include "gnc-commodity.h"

#include "gncAddressP.h"
#include "gncBillTermP.h"
#include "gncInvoice.h"
#ifdef GNUCASH_MAJOR_VERSION
#include "gncBusiness.h"
#endif

#include "gncCustomer.h"
#include "gncCustomerP.h"
#include "gncJobP.h"
#include "gncTaxTableP.h"

struct _gncCustomer
{
  QofInstance     inst;

  /* The following fields are identical to 'vendor' */
  char *          id;
  char *          name;
  char *          notes;
  GncBillTerm *   terms;
  GncAddress *    addr;
  gnc_commodity * currency;
  GncTaxTable*    taxtable;
  gboolean        taxtable_override;
  GncTaxIncluded  taxincluded;
  gboolean        active;
  GList *         jobs;

  /* The following fields are unique to 'customer' */
  gnc_numeric     credit;
  gnc_numeric     discount;
  GncAddress *    shipaddr;
};

struct _gncCustomerClass
{
  QofInstanceClass parent_class;
};

static QofLogModule log_module = GNC_MOD_BUSINESS;

#define _GNC_MOD_NAME        GNC_ID_CUSTOMER

/* ============================================================== */
/* misc inline funcs */

G_INLINE_FUNC void mark_customer (GncCustomer *customer);
void mark_customer (GncCustomer *customer)
{
  qof_instance_set_dirty(&customer->inst);
  qof_event_gen (&customer->inst, QOF_EVENT_MODIFY, NULL);
}

/* ============================================================== */

/* GObject Initialization */
QOF_GOBJECT_IMPL(gnc_customer, GncCustomer, QOF_TYPE_INSTANCE);

static void
gnc_customer_init(GncCustomer* cust)
{
}

static void
gnc_customer_dispose_real (GObject *custp)
{
}

static void
gnc_customer_finalize_real(GObject* custp)
{
}

/* Create/Destroy Functions */
GncCustomer *gncCustomerCreate (QofBook *book)
{
  GncCustomer *cust;

  if (!book) return NULL;

  cust = g_object_new (GNC_TYPE_CUSTOMER, NULL);
  qof_instance_init_data (&cust->inst, _GNC_MOD_NAME, book);

  cust->id = CACHE_INSERT ("");
  cust->name = CACHE_INSERT ("");
  cust->notes = CACHE_INSERT ("");
  cust->addr = gncAddressCreate (book, &cust->inst);
  cust->taxincluded = GNC_TAXINCLUDED_USEGLOBAL;
  cust->active = TRUE;
  cust->jobs = NULL;

  cust->discount = gnc_numeric_zero();
  cust->credit = gnc_numeric_zero();
  cust->shipaddr = gncAddressCreate (book, &cust->inst);

  qof_event_gen (&cust->inst, QOF_EVENT_CREATE, NULL);

  return cust;
}

/** Create a copy of a customer, placing the copy into a new book. */
GncCustomer *
gncCloneCustomer (GncCustomer *from, QofBook *book)
{
  GList *node;
  GncCustomer *cust;

  cust = g_object_new (GNC_TYPE_CUSTOMER, NULL);

  qof_instance_init_data (&cust->inst, _GNC_MOD_NAME, book);
  qof_instance_gemini (&cust->inst, &from->inst);

  cust->id = CACHE_INSERT (from->id);
  cust->name = CACHE_INSERT (from->name);
  cust->notes = CACHE_INSERT (from->notes);
  cust->discount = from->discount;
  cust->credit = from->credit;
  cust->taxincluded = from->taxincluded;
  cust->active = from->active;
  cust->taxtable_override = from->taxtable_override;

  cust->addr = gncCloneAddress (from->addr, &cust->inst, book);
  cust->shipaddr = gncCloneAddress (from->shipaddr, &cust->inst, book);

  /* Find the matching currency in the new book, assumes
   * currency has already been copied into new book. */
  cust->currency = gnc_commodity_obtain_twin (from->currency, book);

  /* Find the matching bill term, tax table in the new book */
  cust->terms = gncBillTermObtainTwin(from->terms, book);
  cust->taxtable = gncTaxTableObtainTwin (from->taxtable, book);

  for (node=g_list_last(cust->jobs); node; node=node->next)
  {
    GncJob *job = node->data;
    job = gncJobObtainTwin (job, book);
    cust->jobs = g_list_prepend(cust->jobs, job);
  }

  qof_event_gen (&cust->inst, QOF_EVENT_CREATE, NULL);

  return cust;
}

void gncCustomerDestroy (GncCustomer *cust)
{
  if (!cust) return;
  qof_instance_set_destroying(cust, TRUE);
  qof_instance_set_dirty (&cust->inst);
  gncCustomerCommitEdit (cust);
}

static void gncCustomerFree (GncCustomer *cust)
{
  if (!cust) return;

  qof_event_gen (&cust->inst, QOF_EVENT_DESTROY, NULL);

  CACHE_REMOVE (cust->id);
  CACHE_REMOVE (cust->name);
  CACHE_REMOVE (cust->notes);
  gncAddressBeginEdit (cust->addr);
  gncAddressDestroy (cust->addr);
  gncAddressBeginEdit (cust->shipaddr);
  gncAddressDestroy (cust->shipaddr);
  g_list_free (cust->jobs);

  if (cust->terms)
    gncBillTermDecRef (cust->terms);
  if (cust->taxtable) {
    gncTaxTableDecRef (cust->taxtable);
  }

  /* qof_instance_release (&cust->inst); */
  g_object_unref (cust);
}

GncCustomer *
gncCustomerObtainTwin (GncCustomer *from, QofBook *book)
{
  GncCustomer *cust;
  if (!from) return NULL;

  cust = (GncCustomer *) qof_instance_lookup_twin (QOF_INSTANCE(from), book);
  if (!cust)
  {
    cust = gncCloneCustomer (from, book);
  }
  return cust;
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

  qof_event_gen (&cust->inst, QOF_EVENT_MODIFY, NULL);
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
  qof_event_gen (&cust->inst, QOF_EVENT_MODIFY, NULL);
}

void gncCustomerBeginEdit (GncCustomer *cust)
{
  qof_begin_edit (&cust->inst);
}

static void gncCustomerOnError (QofInstance *inst, QofBackendError errcode)
{
  PERR("Customer QofBackend Failure: %d", errcode);
  gnc_engine_signal_commit_error( errcode );
}

static void gncCustomerOnDone (QofInstance *inst)
{
  GncCustomer *cust = (GncCustomer *) inst;
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
  if (!qof_commit_edit (QOF_INSTANCE(cust))) return;
  qof_commit_edit_part2 (&cust->inst, gncCustomerOnError,
                         gncCustomerOnDone, cust_free);
}

/* ============================================================== */
/* Get Functions */

const char * gncCustomerGetID (const GncCustomer *cust)
{
  if (!cust) return NULL;
  return cust->id;
}

const char * gncCustomerGetName (const GncCustomer *cust)
{
  if (!cust) return NULL;
  return cust->name;
}

GncAddress * gncCustomerGetAddr (const GncCustomer *cust)
{
  if (!cust) return NULL;
  return cust->addr;
}

static void
qofCustomerSetAddr (GncCustomer *cust, QofInstance *addr_ent)
{
	GncAddress *addr;

	if(!cust || !addr_ent) { return; }
	addr = (GncAddress*)addr_ent;
	if(addr == cust->addr) { return; }
	if(cust->addr != NULL) {
		gncAddressBeginEdit(cust->addr);
		gncAddressDestroy(cust->addr);
	}
	gncCustomerBeginEdit(cust);
	cust->addr = addr;
	gncCustomerCommitEdit(cust);
}

static void
qofCustomerSetShipAddr (GncCustomer *cust, QofInstance *ship_addr_ent)
{
	GncAddress *ship_addr;

	if(!cust || !ship_addr_ent) { return; }
	ship_addr = (GncAddress*)ship_addr_ent;
	if(ship_addr == cust->shipaddr) { return; }
	if(cust->shipaddr != NULL) {
		gncAddressBeginEdit(cust->shipaddr);
		gncAddressDestroy(cust->shipaddr);
	}
	gncCustomerBeginEdit(cust);
	cust->shipaddr = ship_addr;
	gncCustomerCommitEdit(cust);
}

GncAddress * gncCustomerGetShipAddr (const GncCustomer *cust)
{
  if (!cust) return NULL;
  return cust->shipaddr;
}

const char * gncCustomerGetNotes (const GncCustomer *cust)
{
  if (!cust) return NULL;
  return cust->notes;
}

GncBillTerm * gncCustomerGetTerms (const GncCustomer *cust)
{
  if (!cust) return NULL;
  return cust->terms;
}

GncTaxIncluded gncCustomerGetTaxIncluded (const GncCustomer *cust)
{
  if (!cust) return GNC_TAXINCLUDED_USEGLOBAL;
  return cust->taxincluded;
}

gnc_commodity * gncCustomerGetCurrency (const GncCustomer *cust)
{
  if (!cust) return NULL;
  return cust->currency;
}

gboolean gncCustomerGetActive (const GncCustomer *cust)
{
  if (!cust) return FALSE;
  return cust->active;
}

gnc_numeric gncCustomerGetDiscount (const GncCustomer *cust)
{
  if (!cust) return gnc_numeric_zero();
  return cust->discount;
}

gnc_numeric gncCustomerGetCredit (const GncCustomer *cust)
{
  if (!cust) return gnc_numeric_zero();
  return cust->credit;
}

gboolean gncCustomerGetTaxTableOverride (const GncCustomer *customer)
{
  if (!customer) return FALSE;
  return customer->taxtable_override;
}

GncTaxTable* gncCustomerGetTaxTable (const GncCustomer *customer)
{
  if (!customer) return NULL;
  return customer->taxtable;
}

GList * gncCustomerGetJoblist (const GncCustomer *cust, gboolean show_all)
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
  return (qof_instance_is_dirty(&cust->inst) ||
          gncAddressIsDirty (cust->addr) ||
          gncAddressIsDirty (cust->shipaddr));
}

/* Other functions */

int gncCustomerCompare (const GncCustomer *a, const GncCustomer *b)
{
  if (!a && !b) return 0;
  if (!a && b) return 1;
  if (a && !b) return -1;

  return(strcmp(a->name, b->name));
}

/* ============================================================== */
/* Package-Private functions */
static const char * _gncCustomerPrintable (gpointer item)
{
//  GncCustomer *c = item;
  if (!item) return "failed";
  return gncCustomerGetName((GncCustomer*)item);
}

static QofObject gncCustomerDesc =
{
  interface_version:  QOF_OBJECT_VERSION,
  e_type:             _GNC_MOD_NAME,
  type_label:         "Customer",
  create:             (gpointer)gncCustomerCreate,
  book_begin:         NULL,
  book_end:           NULL,
  is_dirty:           qof_collection_is_dirty,
  mark_clean:         qof_collection_mark_clean,
  foreach:            qof_collection_foreach,
  printable:          (const char* (*)(gpointer))gncCustomerGetName,
  version_cmp:        (int (*)(gpointer, gpointer)) qof_instance_version_cmp,
};

gboolean gncCustomerRegister (void)
{
  static QofParam params[] = {
    { CUSTOMER_ID, QOF_TYPE_STRING, (QofAccessFunc)gncCustomerGetID, (QofSetterFunc)gncCustomerSetID },
    { CUSTOMER_NAME, QOF_TYPE_STRING, (QofAccessFunc)gncCustomerGetName, (QofSetterFunc)gncCustomerSetName },
	{ CUSTOMER_NOTES, QOF_TYPE_STRING, (QofAccessFunc)gncCustomerGetNotes, (QofSetterFunc)gncCustomerSetNotes },
	{ CUSTOMER_DISCOUNT, QOF_TYPE_NUMERIC, (QofAccessFunc)gncCustomerGetDiscount,
		(QofSetterFunc)gncCustomerSetDiscount },
	{ CUSTOMER_CREDIT, QOF_TYPE_NUMERIC, (QofAccessFunc)gncCustomerGetCredit,
		(QofSetterFunc)gncCustomerSetCredit },
    { CUSTOMER_ADDR, GNC_ID_ADDRESS, (QofAccessFunc)gncCustomerGetAddr, (QofSetterFunc)qofCustomerSetAddr },
    { CUSTOMER_SHIPADDR, GNC_ID_ADDRESS, (QofAccessFunc)gncCustomerGetShipAddr, (QofSetterFunc)qofCustomerSetShipAddr },
	{ CUSTOMER_TT_OVER, QOF_TYPE_BOOLEAN, (QofAccessFunc)gncCustomerGetTaxTableOverride, 
		(QofSetterFunc)gncCustomerSetTaxTableOverride },
	{ CUSTOMER_TERMS, GNC_ID_BILLTERM, (QofAccessFunc)gncCustomerGetTerms, (QofSetterFunc)gncCustomerSetTerms },
	{ CUSTOMER_SLOTS, QOF_TYPE_KVP, (QofAccessFunc)qof_instance_get_slots, NULL },
    { QOF_PARAM_ACTIVE, QOF_TYPE_BOOLEAN, (QofAccessFunc)gncCustomerGetActive, (QofSetterFunc)gncCustomerSetActive },
    { QOF_PARAM_BOOK, QOF_ID_BOOK, (QofAccessFunc)qof_instance_get_book, NULL },
    { QOF_PARAM_GUID, QOF_TYPE_GUID, (QofAccessFunc)qof_instance_get_guid, NULL },
    { NULL },
  };

  if(!qof_choice_add_class(GNC_ID_INVOICE, GNC_ID_CUSTOMER, INVOICE_OWNER)) { return FALSE; }
  if(!qof_choice_add_class(GNC_ID_JOB, GNC_ID_CUSTOMER, JOB_OWNER)) { return FALSE; }
  qof_class_register (_GNC_MOD_NAME, (QofSortFunc)gncCustomerCompare,params);
  if(!qof_choice_create(GNC_ID_CUSTOMER)) { return FALSE;}
  /* temp */
  _gncCustomerPrintable(NULL);
  return qof_object_register (&gncCustomerDesc);
}

gint64 gncCustomerNextID (QofBook *book)
{
  return qof_book_get_counter (book, _GNC_MOD_NAME);
}
