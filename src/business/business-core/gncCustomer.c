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

/* GObject declarations */

static void gnc_customer_class_init(GncCustomerClass *klass);
static void gnc_customer_init(GncCustomer *sp);
static void gnc_customer_finalize(GObject *object);


typedef struct _GncCustomerSignal GncCustomerSignal;
typedef enum _GncCustomerSignalType GncCustomerSignalType;

struct _GncCustomerPrivate
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

enum _GncCustomerSignalType {
	/* Signals */
	LAST_SIGNAL
};

/* properties */
enum
{
        PROP_0
};

struct _GncCustomerSignal {
	GncCustomer *object;
};

static guint gnc_customer_signals[LAST_SIGNAL] = { 0 };
static GObjectClass *parent_class = NULL;

GType
gnc_customer_get_type()
{
	static GType type = 0;

	if(type == 0) {
		static const GTypeInfo our_info = {
			sizeof (GncCustomerClass),
			NULL,
			NULL,
			(GClassInitFunc)gnc_customer_class_init,
			NULL,
			NULL,
			sizeof (GncCustomer),
			0,
			(GInstanceInitFunc)gnc_customer_init,
		};

		type = g_type_register_static(QOF_TYPE_INSTANCE, 
			"GncCustomer", &our_info, 0);
	}

	return type;
}

static void
gnc_customer_class_init(GncCustomerClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS(klass);

	parent_class = g_type_class_peek_parent(klass);
	object_class->finalize = gnc_customer_finalize;
	object_class->set_property = gnc_customer_set_property;
    object_class->get_property = gnc_customer_get_property;

	/* Install properties */
	
	/* Create signals here:*/
 	
}

static void
gnc_customer_init(GncCustomer *obj)
{
	/* Initialize private members, etc. */
}

static void
gnc_customer_finalize(GObject *object)
{
	
	/* Free private members, etc. */
	
	G_OBJECT_CLASS(parent_class)->finalize(object);
}

static void
gnc_customer_set_property (GObject *object,
				  guint param_id,
				  const GValue *value,
				  GParamSpec *pspec)
{
	GncCustomer *obj;
	
	obj = GNC_CUSTOMER (object);
	switch (param_id) {		
		default:
   			/* We don't have any other property... */
    		G_OBJECT_WARN_INVALID_PROPERTY_ID(object,property_id,pspec);
    	break;
	}
}

static void
gnc_customer_get_property (GObject      *object,
                        guint         property_id,
                        GValue       *value,
                        GParamSpec   *pspec)
{
  GncCustomer *obj;
  
  obj = GNC_CUSTOMER (object);

  switch (property_id) {
  default:
    /* We don't have any other property... */
    G_OBJECT_WARN_INVALID_PROPERTY_ID(object,property_id,pspec);
    break;
  }
}


struct _GncCustomerPrivate
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

static QofLogModule log_module = GNC_MOD_BUSINESS;

#define _GNC_MOD_NAME        GNC_ID_CUSTOMER

/* ============================================================== */
/* misc inline funcs */

G_INLINE_FUNC void mark_customer (GncCustomer *customer);
void mark_customer (GncCustomer *customer)
{
  qof_instance_set_dirty(&customer->inst);
  qof_event_gen (&customer->inst.entity, QOF_EVENT_MODIFY, NULL);
}

/* ============================================================== */
/* Create/Destroy Functions */

GncCustomer *gncCustomerCreate (QofBook *book)
{
  GncCustomer *cust;

  if (!book) return NULL;

  cust = g_new0 (GncCustomer, 1);
  qof_instance_init (QOF_INSTANCE (cust), _GNC_MOD_NAME, book);

  cust->priv->id = CACHE_INSERT ("");
  cust->priv->name = CACHE_INSERT ("");
  cust->priv->notes = CACHE_INSERT ("");
  cust->priv->addr = gncAddressCreate (book, QOF_INSTANCE (cust));
  cust->priv->taxincluded = GNC_TAXINCLUDED_USEGLOBAL;
  cust->priv->active = TRUE;
  cust->priv->jobs = NULL;

  cust->priv->discount = gnc_numeric_zero();
  cust->priv->credit = gnc_numeric_zero();
  cust->priv->shipaddr = gncAddressCreate (book, QOF_INSTANCE (cust));

  qof_event_gen (QOF_INSTANCE (cust), QOF_EVENT_CREATE, NULL);

  return cust;
}

/** Create a copy of a customer, placing the copy into a new book. */
GncCustomer *
gncCloneCustomer (GncCustomer *from, QofBook *book)
{
  GList *node;
  GncCustomer *cust;

  cust = g_new0 (GncCustomer, 1);

  qof_instance_init (QOF_INSTANCE (cust), _GNC_MOD_NAME, book);
  qof_instance_gemini (QOF_INSTANCE (cust), &from->inst);

  cust->priv->id = CACHE_INSERT (from->id);
  cust->priv->name = CACHE_INSERT (from->name);
  cust->priv->notes = CACHE_INSERT (from->notes);
  cust->priv->discount = from->discount;
  cust->priv->credit = from->credit;
  cust->priv->taxincluded = from->taxincluded;
  cust->priv->active = from->active;
  cust->priv->taxtable_override = from->taxtable_override;

  cust->priv->addr = gncCloneAddress (from->addr, QOF_INSTANCE (cust), book);
  cust->priv->shipaddr = gncCloneAddress (from->shipaddr, QOF_INSTANCE (cust), book);

  /* Find the matching currency in the new book, assumes
   * currency has already been copied into new book. */
  cust->priv->currency = gnc_commodity_obtain_twin (from->currency, book);

  /* Find the matching bill term, tax table in the new book */
  cust->priv->terms = gncBillTermObtainTwin(from->terms, book);
  cust->priv->taxtable = gncTaxTableObtainTwin (from->taxtable, book);

  for (node=g_list_last(cust->priv->jobs); node; node=node->next)
  {
    GncJob *job = node->data;
    job = gncJobObtainTwin (job, book);
    cust->priv->jobs = g_list_prepend(cust->priv->jobs, job);
  }

  qof_event_gen (QOF_INSTANCE (cust), QOF_EVENT_CREATE, NULL);

  return cust;
}

void gncCustomerDestroy (GncCustomer *cust)
{
  if (!cust) return;
  cust->priv->inst.do_free = TRUE;
  qof_instance_set_dirty (QOF_INSTANCE (cust));
  gncCustomerCommitEdit (cust);
}

static void gncCustomerFree (GncCustomer *cust)
{
  if (!cust) return;

  qof_event_gen (QOF_INSTANCE (cust), QOF_EVENT_DESTROY, NULL);

  CACHE_REMOVE (cust->priv->id);
  CACHE_REMOVE (cust->priv->name);
  CACHE_REMOVE (cust->priv->notes);
  gncAddressDestroy (cust->priv->addr);
  gncAddressDestroy (cust->priv->shipaddr);
  g_list_free (cust->priv->jobs);

  if (cust->priv->terms)
    gncBillTermDecRef (cust->priv->terms);
  if (cust->priv->taxtable) {
    gncTaxTableDecRef (cust->priv->taxtable);
  }

  qof_instance_release (QOF_INSTANCE (cust));
  g_free (cust);
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
  SET_STR(cust, cust->priv->id, id);
  mark_customer (cust);
  gncCustomerCommitEdit (cust);
}

void gncCustomerSetName (GncCustomer *cust, const char *name)
{
  if (!cust) return;
  if (!name) return;
  SET_STR(cust, cust->priv->name, name);
  mark_customer (cust);
  gncCustomerCommitEdit (cust);
}

void gncCustomerSetNotes (GncCustomer *cust, const char *notes)
{
  if (!cust) return;
  if (!notes) return;
  SET_STR(cust, cust->priv->notes, notes);
  mark_customer (cust);
  gncCustomerCommitEdit (cust);
}

void gncCustomerSetTerms (GncCustomer *cust, GncBillTerm *terms)
{
  if (!cust) return;
  if (cust->priv->terms == terms) return;

  gncCustomerBeginEdit (cust);
  if (cust->priv->terms)
    gncBillTermDecRef (cust->priv->terms);
  cust->priv->terms = terms;
  if (cust->priv->terms)
    gncBillTermIncRef (cust->priv->terms);
  mark_customer (cust);
  gncCustomerCommitEdit (cust);
}

void gncCustomerSetTaxIncluded (GncCustomer *cust, GncTaxIncluded taxincl)
{
  if (!cust) return;
  if (taxincl == cust->priv->taxincluded) return;
  gncCustomerBeginEdit (cust);
  cust->priv->taxincluded = taxincl;
  mark_customer (cust);
  gncCustomerCommitEdit (cust);
}

void gncCustomerSetActive (GncCustomer *cust, gboolean active)
{
  if (!cust) return;
  if (active == cust->priv->active) return;
  gncCustomerBeginEdit (cust);
  cust->priv->active = active;
  mark_customer (cust);
  gncCustomerCommitEdit (cust);
}

void gncCustomerSetDiscount (GncCustomer *cust, gnc_numeric discount)
{
  if (!cust) return;
  if (gnc_numeric_equal (discount, cust->priv->discount)) return;
  gncCustomerBeginEdit (cust);
  cust->priv->discount = discount;
  mark_customer (cust);
  gncCustomerCommitEdit (cust);
}

void gncCustomerSetCredit (GncCustomer *cust, gnc_numeric credit)
{
  if (!cust) return;
  if (gnc_numeric_equal (credit, cust->priv->credit)) return;
  gncCustomerBeginEdit (cust);
  cust->priv->credit = credit;
  mark_customer (cust);
  gncCustomerCommitEdit (cust);
}

void gncCustomerSetCurrency (GncCustomer *cust, gnc_commodity *currency)
{
  if (!cust || !currency) return;
  if (cust->priv->currency && gnc_commodity_equal (cust->priv->currency, currency)) return;
  gncCustomerBeginEdit (cust);
  cust->priv->currency = currency;
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

  if (g_list_index(cust->priv->jobs, job) == -1)
    cust->priv->jobs = g_list_insert_sorted (cust->priv->jobs, job,
                                       (GCompareFunc)gncJobCompare);

  qof_event_gen (QOF_INSTANCE (cust), QOF_EVENT_MODIFY, NULL);
}

void gncCustomerRemoveJob (GncCustomer *cust, GncJob *job)
{
  GList *node;

  if (!cust) return;
  if (!job) return;

  node = g_list_find (cust->priv->jobs, job);
  if (!node) {
    /*    PERR ("split not in account"); */
  } else {
    cust->priv->jobs = g_list_remove_link (cust->priv->jobs, node);
    g_list_free_1 (node);
  }
  qof_event_gen (QOF_INSTANCE (cust), QOF_EVENT_MODIFY, NULL);
}

void gncCustomerBeginEdit (GncCustomer *cust)
{
  qof_begin_edit (QOF_INSTANCE (cust));
}

static void gncCustomerOnError (QofInstance *inst, QofBackendError errcode)
{
  PERR("Customer QofBackend Failure: %d", errcode);
}

static void gncCustomerOnDone (QofInstance *inst)
{
  GncCustomer *cust = (GncCustomer *) inst;
  gncAddressClearDirty (cust->priv->addr);
  gncAddressClearDirty (cust->priv->shipaddr);
}

static void cust_free (QofInstance *inst)
{
  GncCustomer *cust = (GncCustomer *) inst;
  gncCustomerFree (cust);
}

void gncCustomerCommitEdit (GncCustomer *cust)
{
  if (!qof_commit_edit (QOF_INSTANCE(cust))) return;
  qof_commit_edit_part2 (QOF_INSTANCE (cust), gncCustomerOnError,
                         gncCustomerOnDone, cust_free);
}

/* ============================================================== */
/* Get Functions */

const char * gncCustomerGetID (GncCustomer *cust)
{
  if (!cust) return NULL;
  return cust->priv->id;
}

const char * gncCustomerGetName (GncCustomer *cust)
{
  if (!cust) return NULL;
  return cust->priv->name;
}

GncAddress * gncCustomerGetAddr (GncCustomer *cust)
{
  if (!cust) return NULL;
  return cust->priv->addr;
}

static void
qofCustomerSetAddr (GncCustomer *cust, QofEntity *addr_ent)
{
	GncAddress *addr;

	if(!cust || !addr_ent) { return; }
	addr = (GncAddress*)addr_ent;
	if(addr == cust->priv->addr) { return; }
	if(cust->priv->addr != NULL) { gncAddressDestroy(cust->priv->addr); }
	gncCustomerBeginEdit(cust);
	cust->priv->addr = addr;
	gncCustomerCommitEdit(cust);
}

static void
qofCustomerSetShipAddr (GncCustomer *cust, QofEntity *ship_addr_ent)
{
	GncAddress *ship_addr;

	if(!cust || !ship_addr_ent) { return; }
	ship_addr = (GncAddress*)ship_addr_ent;
	if(ship_addr == cust->priv->shipaddr) { return; }
	if(cust->priv->shipaddr != NULL) { gncAddressDestroy(cust->priv->shipaddr); }
	gncCustomerBeginEdit(cust);
	cust->priv->shipaddr = ship_addr;
	gncCustomerCommitEdit(cust);
}

GncAddress * gncCustomerGetShipAddr (GncCustomer *cust)
{
  if (!cust) return NULL;
  return cust->priv->shipaddr;
}

const char * gncCustomerGetNotes (GncCustomer *cust)
{
  if (!cust) return NULL;
  return cust->priv->notes;
}

GncBillTerm * gncCustomerGetTerms (GncCustomer *cust)
{
  if (!cust) return NULL;
  return cust->priv->terms;
}

GncTaxIncluded gncCustomerGetTaxIncluded (GncCustomer *cust)
{
  if (!cust) return GNC_TAXINCLUDED_USEGLOBAL;
  return cust->priv->taxincluded;
}

gnc_commodity * gncCustomerGetCurrency (GncCustomer *cust)
{
  if (!cust) return NULL;
  return cust->priv->currency;
}

gboolean gncCustomerGetActive (GncCustomer *cust)
{
  if (!cust) return FALSE;
  return cust->priv->active;
}

gnc_numeric gncCustomerGetDiscount (GncCustomer *cust)
{
  if (!cust) return gnc_numeric_zero();
  return cust->priv->discount;
}

gnc_numeric gncCustomerGetCredit (GncCustomer *cust)
{
  if (!cust) return gnc_numeric_zero();
  return cust->priv->credit;
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
    return (g_list_copy (cust->priv->jobs));
  } else {
    GList *list = NULL, *iterator;
    for (iterator = cust->priv->jobs; iterator; iterator=iterator->next) {
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
  return (qof_instance_is_dirty(QOF_INSTANCE (cust)) ||
          gncAddressIsDirty (cust->priv->addr) ||
          gncAddressIsDirty (cust->priv->shipaddr));
}

/* Other functions */

int gncCustomerCompare (GncCustomer *a, GncCustomer *b)
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
