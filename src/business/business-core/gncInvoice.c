/*
 * gncInvoice.c -- the Core Business Invoice
 * Copyright (C) 2001 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#include "config.h"

#include <glib.h>

#include "Transaction.h"
#include "Account.h"
#include "messages.h"
#include "gnc-numeric.h"
#include "kvp_frame.h"
#include "gnc-engine-util.h"

#include "gncBusiness.h"
#include "gncCustomer.h"
#include "gncVendor.h"
#include "gncEntry.h"
#include "gncEntryP.h"
#include "gncInvoice.h"
#include "gncInvoiceP.h"

struct _gncInvoice {
  GncBusiness *business;
  
  GUID		guid;
  char *	id;
  char *	notes;
  GList * 	entries;
  GncInvoiceType type;
  union {
    GncCustomer *customer;
    GncVendor *	vendor;
  } owner;
  Timespec 	date_opened;
  Timespec 	date_due;
  Timespec 	date_closed;
  gint 		terms;

  Account * 	posted_acc;
  Transaction * posted_txn;

  gboolean 	active;

  gboolean	dirty;
};

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

/* Create/Destroy Functions */

GncInvoice *gncInvoiceCreate (GncBusiness *business, GncInvoiceType type)
{
  GncInvoice *invoice;

  if (!business) return NULL;
  if (type != GNC_INVOICE_CUSTOMER && type != GNC_INVOICE_VENDOR) return NULL;

  invoice = g_new0 (GncInvoice, 1);
  invoice->business = business;

  invoice->id = CACHE_INSERT ("");
  invoice->notes = CACHE_INSERT ("");

  invoice->active = TRUE;
  invoice->type = type;

  guid_new (&invoice->guid);
  gncBusinessAddEntity (business, GNC_INVOICE_MODULE_NAME, &invoice->guid,
			invoice);

  return invoice;
}

void gncInvoiceDestroy (GncInvoice *invoice)
{
  if (!invoice) return;

  CACHE_REMOVE (invoice->id);
  CACHE_REMOVE (invoice->notes);
  g_list_free (invoice->entries);
  gncBusinessRemoveEntity (invoice->business, GNC_INVOICE_MODULE_NAME,
			   &invoice->guid);
  g_free (invoice);
}

/* Set Functions */

void gncInvoiceSetGUID (GncInvoice *invoice, const GUID *guid)
{
  if (!invoice || !guid) return;
  gncBusinessRemoveEntity (invoice->business, GNC_INVOICE_MODULE_NAME,
			   &invoice->guid);
  invoice->guid = *guid;
  gncBusinessAddEntity (invoice->business, GNC_INVOICE_MODULE_NAME,
			&invoice->guid, invoice);
}

void gncInvoiceSetID (GncInvoice *invoice, const char *id)
{
  if (!invoice || !id) return;
  SET_STR (invoice->id, id);
  invoice->dirty = TRUE;
}

void gncInvoiceSetCustomer (GncInvoice *invoice, GncCustomer *customer)
{
  if (!invoice) return;
  if (invoice->type != GNC_INVOICE_CUSTOMER) return;
  invoice->owner.customer = customer;
  invoice->dirty = TRUE;
}

void gncInvoiceSetVendor (GncInvoice *invoice, GncVendor *vendor)
{
  if (!invoice) return;
  if (invoice->type != GNC_INVOICE_VENDOR) return;
  invoice->owner.vendor = vendor;
  invoice->dirty = TRUE;
}

void gncInvoiceSetDateOpened (GncInvoice *invoice, const Timespec *date)
{
  if (!invoice || !date) return;
  invoice->date_opened = *date;
  invoice->dirty = TRUE;
}

void gncInvoiceSetDateDue (GncInvoice *invoice, const Timespec *date)
{
  if (!invoice || !date) return;
  invoice->date_due = *date;
  invoice->dirty = TRUE;
}

void gncInvoiceSetDateClosed (GncInvoice *invoice, const Timespec *date)
{
  if (!invoice || !date) return;
  invoice->date_closed = *date;
  invoice->dirty = TRUE;
}

void gncInvoiceSetTerms (GncInvoice *invoice, gint terms)
{
  if (!invoice) return;
  invoice->terms = terms;
  invoice->dirty = TRUE;
}

void gncInvoiceSetNotes (GncInvoice *invoice, const char *notes)
{
  if (!invoice || !notes) return;
  SET_STR (invoice->notes, notes);
  invoice->dirty = TRUE;
}

void gncInvoiceSetActive (GncInvoice *invoice, gboolean active)
{
  if (!invoice) return;
  invoice->active = active;
  invoice->dirty = TRUE;
}

void gncInvoiceSetDirty (GncInvoice *invoice, gboolean dirty)
{
  if (!invoice) return;
  invoice->dirty = dirty;
}

void gncInvoiceAddEntry (GncInvoice *invoice, GncEntry *entry)
{
  GncInvoice *old;

  if (!invoice || !entry) return;

  old = gncEntryGetInvoice (entry);
  if (old == invoice) return;	/* I already own this one */
  if (old) gncInvoiceRemoveEntry (old, entry);

  gncEntrySetInvoice (entry, invoice);
  invoice->entries = g_list_append (invoice->entries, entry);
  invoice->dirty = TRUE;
}

void gncInvoiceRemoveEntry (GncInvoice *invoice, GncEntry *entry)
{
  if (!invoice || !entry) return;

  gncEntrySetInvoice (entry, NULL);
  invoice->entries = g_list_remove (invoice->entries, entry);
  invoice->dirty = TRUE;
}

/* Get Functions */

GncBusiness * gncInvoiceGetBusiness (GncInvoice *invoice)
{
  if (!invoice) return NULL;
  return invoice->business;
}

const GUID * gncInvoiceGetGUID (GncInvoice *invoice)
{
  if (!invoice) return NULL;
  return &(invoice->guid);
}

const char * gncInvoiceGetID (GncInvoice *invoice)
{
  if (!invoice) return NULL;
  return invoice->id;
}

GncInvoiceType gncInvoiceGetType (GncInvoice *invoice)
{
  if (!invoice) return GNC_INVOICE_NONE;
  return invoice->type;
}

GncCustomer * gncInvoiceGetCustomer (GncInvoice *invoice)
{
  if (!invoice) return NULL;
  if (invoice->type != GNC_INVOICE_CUSTOMER) return NULL;
  return invoice->owner.customer;
}

GncVendor * gncInvoiceGetVendor (GncInvoice *invoice)
{
  if (!invoice) return NULL;
  if (invoice->type != GNC_INVOICE_VENDOR) return NULL;
  return invoice->owner.vendor;
}

Timespec gncInvoiceGetDateOpened (GncInvoice *invoice)
{
  Timespec ts; ts.tv_sec = 0; ts.tv_nsec = 0;
  if (!invoice) return ts;
  return invoice->date_opened;
}

Timespec gncInvoiceGetDateDue (GncInvoice *invoice)
{
  Timespec ts; ts.tv_sec = 0; ts.tv_nsec = 0;
  if (!invoice) return ts;
  return invoice->date_due;
}

Timespec gncInvoiceGetDateClosed (GncInvoice *invoice)
{
  Timespec ts; ts.tv_sec = 0; ts.tv_nsec = 0;
  if (!invoice) return ts;
  return invoice->date_closed;
}

gint gncInvoiceGetTerms (GncInvoice *invoice)
{
  if (!invoice) return 0;
  return invoice->terms;
}

const char * gncInvoiceGetNotes (GncInvoice *invoice)
{
  if (!invoice) return NULL;
  return invoice->notes;
}

gboolean gncInvoiceGetActive (GncInvoice *invoice)
{
  if (!invoice) return FALSE;
  return invoice->active;
}

GList * gncInvoiceGetEntries (GncInvoice *invoice)
{
  if (!invoice) return NULL;
  return invoice->entries;
}

gboolean gncInvoiceIsDirty (GncInvoice *invoice)
{
  if (!invoice) return FALSE;
  return invoice->dirty;
}

void gncInvoiceBeginEdit (GncInvoice *invoice)
{
  if (!invoice) return;
}
void gncInvoiceCommitEdit (GncInvoice *invoice)
{
  if (!invoice) return;
}

static GncBusinessObject gncInvoiceDesc = {
  GNC_BUSINESS_VERSION,
  GNC_INVOICE_MODULE_NAME,
  "Purchase/Sales Invoice",
  NULL,				/* destroy */
  NULL,				/* get list */
  NULL				/* printable */
};

gboolean gncInvoiceRegister (void)
{
  return gncBusinessRegister (&gncInvoiceDesc);
}

static gint lastId = 187;	/* XXX */

gint gncInvoiceNextID (GncBusiness *business)
{
  return lastId++;
}
