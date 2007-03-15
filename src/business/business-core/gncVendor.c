/********************************************************************\
 * gncVendor.c -- the Core Vendor Interface                         *
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
 * Copyright (C) 2001, 2002 Derek Atkins
 * Copyright (C) 2003 <linas@linas.org>
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#include "config.h"

#include <glib.h>
#include <string.h>

#include "gnc-commodity.h"
#include "gncAddressP.h"
#include "gncBillTermP.h"
#include "gncInvoice.h"
#include "gncJobP.h"
#include "gncTaxTableP.h"
#include "gncVendor.h"
#include "gncVendorP.h"

/* GObject declarations */

static void gnc_vendor_class_init(GncVendorClass *klass);
static void gnc_vendor_init(GncVendor *sp);
static void gnc_vendor_finalize(GObject *object);


typedef struct _GncVendorSignal GncVendorSignal;
typedef enum _GncVendorSignalType GncVendorSignalType;

struct _GncVendorPrivate
{
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
};


enum _GncVendorSignalType {
	/* Signals */
	LAST_SIGNAL
};

/* properties */
enum
{
        PROP_0
};

struct _GncVendorSignal {
	GncVendor *object;
};

static guint gnc_vendor_signals[LAST_SIGNAL] = { 0 };
static GObjectClass *parent_class = NULL;

GType
gnc_vendor_get_type()
{
	static GType type = 0;

	if(type == 0) {
		static const GTypeInfo our_info = {
			sizeof (GncVendorClass),
			NULL,
			NULL,
			(GClassInitFunc)gnc_vendor_class_init,
			NULL,
			NULL,
			sizeof (GncVendor),
			0,
			(GInstanceInitFunc)gnc_vendor_init,
		};

		type = g_type_register_static(QOF_TYPE_INSTANCE, 
			"GncVendor", &our_info, 0);
	}

	return type;
}

static void
gnc_vendor_class_init(GncVendorClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS(klass);

	parent_class = g_type_class_peek_parent(klass);
	object_class->finalize = gnc_vendor_finalize;
	object_class->set_property = gnc_vendor_set_property;
    object_class->get_property = gnc_vendor_get_property;

	/* Install properties */
	
	/* Create signals here:*/
 	
}

static void
gnc_vendor_init(GncVendor *obj)
{
	/* Initialize private members, etc. */
}

static void
gnc_vendor_finalize(GObject *object)
{
	
	/* Free private members, etc. */
	
	G_OBJECT_CLASS(parent_class)->finalize(object);
}

static void
gnc_vendor_set_property (GObject *object,
				  guint param_id,
				  const GValue *value,
				  GParamSpec *pspec)
{
	GncVendor *obj;
	
	obj = QOF_BOOK (object);
	switch (param_id) {		
		default:
   			/* We don't have any other property... */
    		G_OBJECT_WARN_INVALID_PROPERTY_ID(object,property_id,pspec);
    	break;
	}
}

static void
gnc_vendor_get_property (GObject      *object,
                        guint         property_id,
                        GValue       *value,
                        GParamSpec   *pspec)
{
  GncVendor *obj;
  
  obj = QOF_BOOK(object);

  switch (property_id) {
  default:
    /* We don't have any other property... */
    G_OBJECT_WARN_INVALID_PROPERTY_ID(object,property_id,pspec);
    break;
  }
}
/*************************************************************************************/

static QofLogModule log_module = GNC_MOD_BUSINESS;

#define _GNC_MOD_NAME        GNC_ID_VENDOR

/* ============================================================ */
/* Misc inline funcs */

G_INLINE_FUNC void mark_vendor (GncVendor *vendor);
void mark_vendor (GncVendor *vendor)
{
  qof_instance_set_dirty(QOF_INSTANCE (vendor));
  qof_event_gen (QOF_INSTANCE (vendor), QOF_EVENT_MODIFY, NULL);
}

/* ============================================================== */
/* Create/Destroy Functions */

GncVendor *gncVendorCreate (QofBook *book)
{
  GncVendor *vendor;

  if (!book) return NULL;

  vendor = g_new0 (GncVendor, 1);
  qof_instance_init (QOF_INSTANCE (vendor), _GNC_MOD_NAME, book);
  
  vendor->priv->id = CACHE_INSERT ("");
  vendor->priv->name = CACHE_INSERT ("");
  vendor->priv->notes = CACHE_INSERT ("");
  vendor->priv->addr = gncAddressCreate (book, QOF_INSTANCE (vendor));
  vendor->priv->taxincluded = GNC_TAXINCLUDED_USEGLOBAL;
  vendor->priv->active = TRUE;
  vendor->priv->jobs = NULL;

  qof_event_gen (QOF_INSTANCE (vendor), QOF_EVENT_CREATE, NULL);

  return vendor;
}

void gncVendorDestroy (GncVendor *vendor)
{
  if (!vendor) return;
  qof_instance_mark_free (QOF_INSTANCE (vendor));
  gncVendorCommitEdit (vendor);
}

static void gncVendorFree (GncVendor *vendor)
{
  if (!vendor) return;

  qof_event_gen (QOF_INSTANCE (vendor), QOF_EVENT_DESTROY, NULL);

  CACHE_REMOVE (vendor->priv->id);
  CACHE_REMOVE (vendor->priv->name);
  CACHE_REMOVE (vendor->priv->notes);
  gncAddressDestroy (vendor->priv->addr);
  g_list_free (vendor->priv->jobs);

  if (vendor->priv->terms)
    gncBillTermDecRef (vendor->priv->terms);
  if (vendor->priv->taxtable)
    gncTaxTableDecRef (vendor->priv->taxtable);

  qof_instance_release (QOF_INSTANCE (vendor));

}

/** Create a copy of a vendor, placing the copy into a new book. */
GncVendor *
gncCloneVendor (GncVendor *from, QofBook *book)
{
  GList *node;
  GncVendor *vendor;

  if (!book) return NULL;
  vendor = GNC_VENDOR ( g_object_new (GNC_TYPE_VENDOR, NULL));
  vendor->priv = g_new0 (GncVendorPrivate, 1);
  
  qof_instance_init (QOF_INSTANCE (vendor), _GNC_MOD_NAME, book);
  qof_instance_gemini (QOF_INSTANCE (vendor), &from->inst);
  
  vendor->priv->id = CACHE_INSERT (from->id);
  vendor->priv->name = CACHE_INSERT (from->name);
  vendor->priv->notes = CACHE_INSERT (from->notes);
  vendor->priv->addr = gncCloneAddress (from->addr, QOF_INSTANCE (vendor), book);
  vendor->priv->taxincluded = from->taxincluded;
  vendor->priv->taxtable_override = from->taxtable_override;
  vendor->priv->active = from->active;

  vendor->priv->terms = gncBillTermObtainTwin (from->terms, book);
  gncBillTermIncRef (vendor->priv->terms);

  vendor->priv->currency = gnc_commodity_obtain_twin (from->currency, book);

  vendor->priv->taxtable = gncTaxTableObtainTwin (from->taxtable, book);
  gncTaxTableIncRef (vendor->priv->taxtable);

  vendor->priv->jobs = NULL;
  for (node=g_list_last(from->jobs); node; node=node->prev)
  {
    GncJob *job = node->data;
    job = gncJobObtainTwin (job, book);
    vendor->priv->jobs = g_list_prepend(vendor->priv->jobs, job);
  }

  qof_event_gen (QOF_INSTANCE (vendor), QOF_EVENT_CREATE, NULL);

  return vendor;
}

GncVendor *
gncVendorObtainTwin (GncVendor *from, QofBook *book)
{
  GncVendor *vendor;
  if (!book) return NULL;

  vendor = (GncVendor *) qof_instance_lookup_twin (QOF_INSTANCE(from), book);
  if (!vendor)
  {
    vendor = gncCloneVendor (from, book);
  }

  return vendor;
}

/* ============================================================== */
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
  SET_STR(vendor, vendor->priv->id, id);
  mark_vendor (vendor);
  gncVendorCommitEdit (vendor);
}

void gncVendorSetName (GncVendor *vendor, const char *name)
{
  if (!vendor) return;
  if (!name) return;
  SET_STR(vendor, vendor->priv->name, name);
  mark_vendor (vendor);
  gncVendorCommitEdit (vendor);
}

void gncVendorSetNotes (GncVendor *vendor, const char *notes)
{
  if (!vendor) return;
  if (!notes) return;
  SET_STR(vendor,vendor->priv->notes, notes);
  mark_vendor (vendor);
  gncVendorCommitEdit (vendor);
}

void gncVendorSetTerms (GncVendor *vendor, GncBillTerm *terms)
{
  if (!vendor) return;
  if (vendor->priv->terms == terms) return;

  gncVendorBeginEdit (vendor);
  if (vendor->priv->terms)
    gncBillTermDecRef (vendor->priv->terms);
  vendor->priv->terms = terms;
  if (vendor->priv->terms)
    gncBillTermIncRef (vendor->priv->terms);
  mark_vendor (vendor);
  gncVendorCommitEdit (vendor);
}

void gncVendorSetTaxIncluded (GncVendor *vendor, GncTaxIncluded taxincl)
{
  if (!vendor) return;
  if (taxincl == vendor->priv->taxincluded) return;
  gncVendorBeginEdit (vendor);
  vendor->priv->taxincluded = taxincl;
  mark_vendor (vendor);
  gncVendorCommitEdit (vendor);
}

void gncVendorSetCurrency (GncVendor *vendor, gnc_commodity *currency)
{
  if (!vendor || !currency) return;
  if (vendor->priv->currency &&
      gnc_commodity_equal (vendor->priv->currency, currency))
    return;
  gncVendorBeginEdit (vendor);
  vendor->priv->currency = currency;
  mark_vendor (vendor);
  gncVendorCommitEdit (vendor);
}

void gncVendorSetActive (GncVendor *vendor, gboolean active)
{
  if (!vendor) return;
  if (active == vendor->priv->active) return;
  gncVendorBeginEdit (vendor);
  vendor->priv->active = active;
  mark_vendor (vendor);
  gncVendorCommitEdit (vendor);
}

void gncVendorSetTaxTableOverride (GncVendor *vendor, gboolean override)
{
  if (!vendor) return;
  if (vendor->priv->taxtable_override == override) return;
  gncVendorBeginEdit (vendor);
  vendor->priv->taxtable_override = override;
  mark_vendor (vendor);
  gncVendorCommitEdit (vendor);
}

void gncVendorSetTaxTable (GncVendor *vendor, GncTaxTable *table)
{
  if (!vendor) return;
  if (vendor->priv->taxtable == table) return;
  gncVendorBeginEdit (vendor);
  if (vendor->priv->taxtable)
    gncTaxTableDecRef (vendor->priv->taxtable);
  if (table)
    gncTaxTableIncRef (table);
  vendor->priv->taxtable = table;
  mark_vendor (vendor);
  gncVendorCommitEdit (vendor);
}

static void
qofVendorSetAddr (GncVendor *vendor, QofEntity *addr_ent)
{
	GncAddress *addr;

	if(!vendor || !addr_ent) { return; }
	addr = (GncAddress*)addr_ent;
	if(addr == vendor->priv->addr) { return; }
	if(vendor->priv->addr != NULL) { gncAddressDestroy(vendor->priv->addr); }
	gncVendorBeginEdit(vendor);
	vendor->priv->addr = addr;
	gncVendorCommitEdit(vendor);
}

static void
qofVendorSetTaxIncluded(GncVendor *vendor, const char* type_string)
{
	GncTaxIncluded inc;

	if(!gncTaxIncludedStringToType(type_string, &inc)) { return; }
	gncVendorBeginEdit(vendor);
	vendor->priv->taxincluded = inc;
	gncVendorCommitEdit(vendor);
}

/* ============================================================== */
/* Get Functions */

const char * gncVendorGetID (GncVendor *vendor)
{
  if (!vendor) return NULL;
  return vendor->priv->id;
}

const char * gncVendorGetName (GncVendor *vendor)
{
  if (!vendor) return NULL;
  return vendor->priv->name;
}

GncAddress * gncVendorGetAddr (GncVendor *vendor)
{
  if (!vendor) return NULL;
  return vendor->priv->addr;
}

const char * gncVendorGetNotes (GncVendor *vendor)
{
  if (!vendor) return NULL;
  return vendor->priv->notes;
}

GncBillTerm * gncVendorGetTerms (GncVendor *vendor)
{
  if (!vendor) return 0;
  return vendor->priv->terms;
}

GncTaxIncluded gncVendorGetTaxIncluded (GncVendor *vendor)
{
  if (!vendor) return GNC_TAXINCLUDED_USEGLOBAL;
  return vendor->priv->taxincluded;
}

gnc_commodity * gncVendorGetCurrency (GncVendor *vendor)
{
  if (!vendor) return NULL;
  return vendor->priv->currency;
}

gboolean gncVendorGetActive (GncVendor *vendor)
{
  if (!vendor) return FALSE;
  return vendor->priv->active;
}

gboolean gncVendorGetTaxTableOverride (GncVendor *vendor)
{
  if (!vendor) return FALSE;
  return vendor->priv->taxtable_override;
}

GncTaxTable* gncVendorGetTaxTable (GncVendor *vendor)
{
  if (!vendor) return NULL;
  return vendor->priv->taxtable;
}

static const char*
qofVendorGetTaxIncluded(GncVendor *vendor)
{
	return gncTaxIncludedTypeToString(vendor->priv->taxincluded);
}

/* Note that JobList changes do not affect the "dirtiness" of the vendor */
void gncVendorAddJob (GncVendor *vendor, GncJob *job)
{
  if (!vendor) return;
  if (!job) return;

  if (g_list_index(vendor->priv->jobs, job) == -1)
    vendor->priv->jobs = g_list_insert_sorted (vendor->priv->jobs, job,
                                         (GCompareFunc)gncJobCompare);

  qof_event_gen (QOF_INSTANCE (vendor), QOF_EVENT_MODIFY, NULL);
}

void gncVendorRemoveJob (GncVendor *vendor, GncJob *job)
{
  GList *node;

  if (!vendor) return;
  if (!job) return;

  node = g_list_find (vendor->priv->jobs, job);
  if (!node) {
    /*    PERR ("split not in account"); */
  } else {
    vendor->priv->jobs = g_list_remove_link (vendor->priv->jobs, node);
    g_list_free_1 (node);
  }

  qof_event_gen (QOF_INSTANCE (vendor), QOF_EVENT_MODIFY, NULL);
}

void gncVendorBeginEdit (GncVendor *vendor)
{
  QOF_BEGIN_EDIT (QOF_INSTANCE (vendor));
}

static void gncVendorOnError (QofInstance *vendor, QofBackendError errcode)
{
  PERR("Vendor QofBackend Failure: %d", errcode);
}

static void gncVendorOnDone (QofInstance *inst)
{
  GncVendor *vendor = (GncVendor *) inst;
  gncAddressClearDirty (vendor->priv->addr);
}

static void vendor_free (QofInstance *inst)
{
  GncVendor *vendor = (GncVendor *) inst;
  gncVendorFree (vendor);
}

void gncVendorCommitEdit (GncVendor *vendor)
{
  if (!qof_commit_edit (QOF_INSTANCE(vendor))) return;
  qof_commit_edit_part2 (QOF_INSTANCE (vendor), gncVendorOnError,
                         gncVendorOnDone, vendor_free);
}

/* ============================================================== */
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
    return (g_list_copy (vendor->priv->jobs));
  } else {
    GList *list = NULL, *iterator;
    for (iterator = vendor->priv->jobs; iterator; iterator=iterator->next) {
      GncJob *j = iterator->data;
      if (gncJobGetActive (j))
        list = g_list_append (list, j);
    }
    return list;
  }
}

gboolean gncVendorIsDirty (GncVendor *vendor)
{
  if (!vendor) return FALSE;
  return (qof_instance_is_dirty (QOF_INSTANCE (vendor)) || gncAddressIsDirty (vendor->priv->addr));
}

/* ============================================================== */
/* Package-Private functions */

static const char * _gncVendorPrintable (gpointer item)
{
  GncVendor *v = item;
  if (!item) return NULL;
  return v->name;
}

static QofObject gncVendorDesc = 
{
  interface_version:  QOF_OBJECT_VERSION,
  e_type:             _GNC_MOD_NAME,
  type_label:         "Vendor",
  create:             (gpointer)gncVendorCreate,
  book_begin:         NULL,
  book_end:           NULL,
  is_dirty:           qof_collection_is_dirty,
  mark_clean:         qof_collection_mark_clean,
  foreach:            qof_collection_foreach,
  printable:          _gncVendorPrintable,
  version_cmp:        (int (*)(gpointer, gpointer)) qof_instance_version_cmp,
};

gboolean gncVendorRegister (void)
{
  static QofParam params[] = {
    { VENDOR_ID, QOF_TYPE_STRING, (QofAccessFunc)gncVendorGetID, (QofSetterFunc)gncVendorSetID },
    { VENDOR_NAME, QOF_TYPE_STRING, (QofAccessFunc)gncVendorGetName, (QofSetterFunc)gncVendorSetName },
    { VENDOR_ADDR,    GNC_ID_ADDRESS, (QofAccessFunc)gncVendorGetAddr, (QofSetterFunc)qofVendorSetAddr },
    { VENDOR_NOTES,   QOF_TYPE_STRING, (QofAccessFunc)gncVendorGetNotes, (QofSetterFunc)gncVendorSetNotes },
    { VENDOR_TERMS,   GNC_ID_BILLTERM, (QofAccessFunc)gncVendorGetTerms, (QofSetterFunc)gncVendorSetTerms },
    { VENDOR_TAX_OVERRIDE, QOF_TYPE_BOOLEAN, (QofAccessFunc)gncVendorGetTaxTableOverride,
		(QofSetterFunc)gncVendorSetTaxTableOverride },
    { VENDOR_TAX_TABLE, GNC_ID_TAXTABLE, (QofAccessFunc)gncVendorGetTaxTable,
		(QofSetterFunc)gncVendorSetTaxTable },
    { VENDOR_TAX_INC, QOF_TYPE_STRING, (QofAccessFunc)qofVendorGetTaxIncluded, 
		(QofSetterFunc)qofVendorSetTaxIncluded},
    { QOF_PARAM_BOOK, QOF_ID_BOOK, (QofAccessFunc)qof_instance_get_book, NULL },
    { QOF_PARAM_GUID, QOF_TYPE_GUID, (QofAccessFunc)qof_instance_get_guid, NULL },
    { QOF_PARAM_ACTIVE, QOF_TYPE_BOOLEAN, (QofAccessFunc)gncVendorGetActive, NULL },
    { NULL },
  };

  if(!qof_choice_add_class(GNC_ID_INVOICE, GNC_ID_VENDOR, INVOICE_OWNER)) { return FALSE; }
  if(!qof_choice_add_class(GNC_ID_JOB, GNC_ID_VENDOR, JOB_OWNER)) { return FALSE; }

  qof_class_register (_GNC_MOD_NAME, (QofSortFunc)gncVendorCompare, params);

  return qof_object_register (&gncVendorDesc);
}

gint64 gncVendorNextID (QofBook *book)
{
  return qof_book_get_counter (book, _GNC_MOD_NAME);
}
