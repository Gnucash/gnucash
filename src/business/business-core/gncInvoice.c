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

#define GNC_INVOICE_ID		"gncInvoice"
#define GNC_INVOICE_GUID	"invoice-guid"

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

void gncInvoiceSetPostedTxn (GncInvoice *invoice, Transaction *txn)
{
  if (!invoice) return;

  invoice->posted_txn = txn;
  invoice->dirty = TRUE;
}

void gncInvoiceSetPostedAcc (GncInvoice *invoice, Account *acc)
{
  if (!invoice) return;

  invoice->posted_acc = acc;
  invoice->dirty = TRUE;
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

Transaction * gncInvoiceGetPostedTxn (GncInvoice *invoice)
{
  if (!invoice) return NULL;
  return invoice->posted_txn;
}

Account * gncInvoiceGetPostedAcc (GncInvoice *invoice)
{
  if (!invoice) return NULL;
  return invoice->posted_acc;
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

void gncInvoiceAttachInvoiceToTxn (GncInvoice *invoice, Transaction *txn)
{
  kvp_frame *kvp;
  kvp_value *value;
  
  if (!invoice || !txn)
    return;

  if (invoice->posted_txn) return;	/* Cannot reset invoice's txn */

  xaccTransBeginEdit (txn);
  kvp = xaccTransGetSlots (txn);
  value = kvp_value_new_guid (gncInvoiceGetGUID (invoice));
  kvp_frame_set_slot_path (kvp, value, GNC_INVOICE_ID, GNC_INVOICE_GUID, NULL);
  kvp_value_delete (value);
  xaccTransCommitEdit (txn);

  gncInvoiceSetPostedTxn (invoice, txn);
}

#define GET_OR_ADD_ACCVAL(list,t_acc,res) { \
	GList *li; \
	res = NULL; \
    	for (li = list; li; li = li->next) { \
		res = li->data; \
      		if (res->acc == t_acc) \
			break; \
		res = NULL; \
    	} \
	if (!res) { \
		res = g_new0 (struct acct_val, 1); \
		res->acc = t_acc; \
		res->val = gnc_numeric_zero (); \
		list = g_list_append (list, res); \
	} \
}

Transaction * gncInvoicePostToAccount (GncInvoice *invoice, Account *acc,
				       Timespec *date)
{
  Transaction *txn;
  GList *item, *iter;
  GList *splitinfo = NULL;
  GNCSession *session;
  gnc_numeric total;
  gnc_commodity *commonCommodity = NULL;
  struct acct_val {
    Account *	acc;
    gnc_numeric val;
  } *acc_val;

  if (!invoice || !acc) return NULL;

  /* XXX: Need to obtain the session */
  txn = xaccMallocTransaction (session);
  xaccTransBeginEdit (txn);

  /* Figure out the common currency */
  /* XXX */

  /* Set Transaction Description (customer), Num (invoice ID), Currency */
  xaccTransSetDescription
    (txn, 
     ((gncInvoiceGetType (invoice) == GNC_INVOICE_CUSTOMER) ?
      gncCustomerGetName (gncInvoiceGetCustomer (invoice)) :
      gncVendorGetName (gncInvoiceGetVendor (invoice))));
			   
  xaccTransSetNum (txn, gncInvoiceGetID (invoice));
  xaccTransSetCurrency (txn, commonCommodity);

  /* Entered and Posted at date */
  if (date) {
    xaccTransSetDateEnteredTS (txn, date);
    xaccTransSetDatePostedTS (txn, date);
  }

  /* Iterate through the entries; sum up everything for each account.
   * then create the appropriate splits in this txn.
   */
  total = gnc_numeric_zero();
  for (iter = gncInvoiceGetEntries(invoice); iter; iter = iter->next) {
    GncEntry * entry = iter->data;
    Account *this_acc = gncEntryGetAccount (entry);
    gnc_numeric disc = gncEntryGetDiscount (entry);
    gnc_numeric subtotal = gnc_numeric_mul (gncEntryGetQuantity (entry),
					    gncEntryGetPrice (entry),
					    100, /* XXX */
					    GNC_RND_ROUND);

    /* Find the account value for this_acc.  If we haven't seen this
     * account before, create a new total and add to list
     */
    GET_OR_ADD_ACCVAL (splitinfo, this_acc, acc_val);

    /* Now compute the split value and add it to the totals */
    {
      gint disc_type = gncEntryGetDiscountType (entry);
      gnc_numeric value;

      if (GNC_ENTRY_INTERP_IS_PERCENT (disc_type))
	disc = gnc_numeric_mul (subtotal, disc, 100 /* XXX */, GNC_RND_ROUND);

      value = gnc_numeric_sub_fixed (subtotal, disc);
      if (disc_type & GNC_ENTRY_INTERP_PRETAX)
	subtotal = value;

      acc_val->val = gnc_numeric_add_fixed (acc_val->val, value);
      total = gnc_numeric_add_fixed (total, value);
    }

    /* Repeat for the Entry Tax */
    this_acc = gncEntryGetTaxAccount (entry);
    if (this_acc) {
      gnc_numeric tax = gncEntryGetTax (entry);
      gint tax_type = gncEntryGetTaxType (entry);

      GET_OR_ADD_ACCVAL (splitinfo, this_acc, acc_val);

      if (GNC_ENTRY_INTERP_IS_PERCENT (tax_type))
	tax = gnc_numeric_mul (subtotal, tax, 100 /* XXX */, GNC_RND_ROUND);

      acc_val->val = gnc_numeric_add_fixed (acc_val->val, tax);
      total = gnc_numeric_add_fixed (total, tax);
    }
  } /* for */

  /* Iterate through the splitinfo list and generate the splits */
  for (iter = splitinfo; iter; iter = iter->next) {
    Split *split;
    acc_val = iter->data;

    split = xaccMallocSplit (session);
    /* set action and memo? */

    xaccSplitSetBaseValue (split, acc_val->val, commonCommodity);
    xaccAccountBeginEdit (acc_val->acc);
    xaccAccountInsertSplit (acc_val->acc, split);
    xaccAccountCommitEdit (acc_val->acc);
    xaccTransAppendSplit (txn, split);
  }

  /* Now create the Posted split (which is negative -- it's a credit) */
  {
    Split *split = xaccMallocSplit (session);
    /* Set action/memo */
    xaccSplitSetBaseValue (split, gnc_numeric_neg (total), commonCommodity);
    xaccAccountBeginEdit (acc);
    xaccAccountInsertSplit (acc, split);
    xaccAccountCommitEdit (acc);
    xaccTransAppendSplit (txn, split);
  }

  gncInvoiceSetPostedAcc (invoice, acc);
  gncInvoiceSetPostedTxn (invoice, txn);

  xaccTransCommitEdit (txn);

  return txn;
}

GncInvoice * gncInvoiceGetInvoiceFromTxn (Transaction *txn)
{
  kvp_frame *kvp;
  kvp_value *value;
  GUID *guid;
  GncBusiness *business;

  if (!txn) return NULL;

  kvp = xaccTransGetSlots (txn);
  value = kvp_frame_get_slot_path (kvp, GNC_INVOICE_ID, GNC_INVOICE_GUID, NULL);
  if (!value) return NULL;

  guid = kvp_value_get_guid (value);
  /* XXX: Need to get GNCSession from Transaction */
  /* XXX: lookup invoice from session/guid */

  return gncBusinessLookupGUID (business, GNC_INVOICE_MODULE_NAME, guid);
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
