/*
 * gncInvoice.c -- the Core Business Invoice
 * Copyright (C) 2001,2002 Derek Atkins
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
#include "gnc-book-p.h"
#include "GNCIdP.h"
#include "QueryObject.h"
#include "gnc-event-p.h"
#include "gnc-lot.h"

#include "gncBusiness.h"
#include "gncEntry.h"
#include "gncEntryP.h"
#include "gncInvoice.h"
#include "gncInvoiceP.h"
#include "gncOwner.h"

struct _gncInvoice {
  GNCBook *book;
  
  GUID		guid;
  char *	id;
  char *	notes;
  char *	billing_id;
  char *	printname;
  GncBillTerm *	terms;
  GList * 	entries;
  GncOwner	owner;
  GncOwner	billto;
  GncJob *	job;
  Timespec 	date_opened;
  Timespec 	date_posted;

  gnc_commodity * common_commodity;

  Account * 	posted_acc;
  Transaction * posted_txn;
  GNCLot *	posted_lot;

  gboolean 	active;

  gboolean	dirty;
};

#define _GNC_MOD_NAME	GNC_INVOICE_MODULE_NAME

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

static void addObj (GncInvoice *invoice);
static void remObj (GncInvoice *invoice);

static void mark_invoice (GncInvoice *invoice);
static void
mark_invoice (GncInvoice *invoice)
{
  invoice->dirty = TRUE;

  gnc_engine_generate_event (&invoice->guid, GNC_EVENT_MODIFY);
}

/* Create/Destroy Functions */

GncInvoice *gncInvoiceCreate (GNCBook *book)
{
  GncInvoice *invoice;

  if (!book) return NULL;

  invoice = g_new0 (GncInvoice, 1);
  invoice->book = book;

  invoice->id = CACHE_INSERT ("");
  invoice->notes = CACHE_INSERT ("");
  invoice->billing_id = CACHE_INSERT ("");

  invoice->billto.type = GNC_OWNER_CUSTOMER;
  invoice->active = TRUE;

  xaccGUIDNew (&invoice->guid, book);
  addObj (invoice);

  gnc_engine_generate_event (&invoice->guid, GNC_EVENT_CREATE);

  return invoice;
}

void gncInvoiceDestroy (GncInvoice *invoice)
{
  if (!invoice) return;

  gnc_engine_generate_event (&invoice->guid, GNC_EVENT_DESTROY);

  CACHE_REMOVE (invoice->id);
  CACHE_REMOVE (invoice->notes);
  CACHE_REMOVE (invoice->billing_id);
  g_list_free (invoice->entries);
  remObj (invoice);

  if (invoice->printname) g_free (invoice->printname);

  g_free (invoice);
}

/* Set Functions */

void gncInvoiceSetGUID (GncInvoice *invoice, const GUID *guid)
{
  if (!invoice || !guid) return;
  if (guid_equal (guid, &invoice->guid)) return;

  remObj (invoice);
  invoice->guid = *guid;
  addObj (invoice);
}

void gncInvoiceSetID (GncInvoice *invoice, const char *id)
{
  if (!invoice || !id) return;
  SET_STR (invoice->id, id);
  mark_invoice (invoice);
}

void gncInvoiceSetOwner (GncInvoice *invoice, GncOwner *owner)
{
  if (!invoice || !owner) return;
  if (gncOwnerEqual (&invoice->owner, owner)) return;
  gncOwnerCopy (owner, &invoice->owner);
  mark_invoice (invoice);
}

void gncInvoiceSetDateOpened (GncInvoice *invoice, Timespec date)
{
  if (!invoice) return;
  if (timespec_equal (&invoice->date_opened, &date)) return;
  invoice->date_opened = date;
  mark_invoice (invoice);
}

void gncInvoiceSetDatePosted (GncInvoice *invoice, Timespec date)
{
  if (!invoice) return;
  if (timespec_equal (&invoice->date_posted, &date)) return;
  invoice->date_posted = date;
  mark_invoice (invoice);
}

void gncInvoiceSetTerms (GncInvoice *invoice, GncBillTerm *terms)
{
  if (!invoice) return;
  if (invoice->terms == terms) return;
  if (invoice->terms)
    gncBillTermDecRef (invoice->terms);
  invoice->terms = terms;
  if (invoice->terms)
    gncBillTermIncRef (invoice->terms);
  mark_invoice (invoice);
}

void gncInvoiceSetBillingID (GncInvoice *invoice, const char *billing_id)
{
  if (!invoice) return;
  SET_STR (invoice->billing_id, billing_id);
  mark_invoice (invoice);
}

void gncInvoiceSetNotes (GncInvoice *invoice, const char *notes)
{
  if (!invoice || !notes) return;
  SET_STR (invoice->notes, notes);
  mark_invoice (invoice);
}

void gncInvoiceSetActive (GncInvoice *invoice, gboolean active)
{
  if (!invoice) return;
  if (invoice->active == active) return;
  invoice->active = active;
  mark_invoice (invoice);
}

void gncInvoiceSetCommonCommodity (GncInvoice *invoice, gnc_commodity *com)
{
  if (!invoice || !com) return;
  if (invoice->common_commodity &&
      gnc_commodity_equal (invoice->common_commodity, com))
    return;
  invoice->common_commodity = com;
  mark_invoice (invoice);
}

void gncInvoiceSetBillTo (GncInvoice *invoice, GncOwner *billto)
{
  if (!invoice || !billto) return;
  if (gncOwnerEqual (&invoice->billto, billto)) return;

  gncOwnerCopy (billto, &invoice->billto);
  mark_invoice (invoice);
}

void gncInvoiceSetDirty (GncInvoice *invoice, gboolean dirty)
{
  if (!invoice) return;
  invoice->dirty = dirty;
}

void gncInvoiceSetPostedTxn (GncInvoice *invoice, Transaction *txn)
{
  if (!invoice) return;
  g_return_if_fail (invoice->posted_txn == NULL);

  invoice->posted_txn = txn;
  mark_invoice (invoice);
}

void gncInvoiceSetPostedLot (GncInvoice *invoice, GNCLot *lot)
{
  if (!invoice) return;
  g_return_if_fail (invoice->posted_lot == NULL);

  invoice->posted_lot = lot;
  mark_invoice (invoice);
}

void gncInvoiceSetPostedAcc (GncInvoice *invoice, Account *acc)
{
  if (!invoice) return;
  g_return_if_fail (invoice->posted_acc == NULL);

  invoice->posted_acc = acc;
  mark_invoice (invoice);
}

void gncInvoiceAddEntry (GncInvoice *invoice, GncEntry *entry)
{
  GncInvoice *old;

  if (!invoice || !entry) return;

  old = gncEntryGetInvoice (entry);
  if (old == invoice) return;	/* I already own this one */
  if (old) gncInvoiceRemoveEntry (old, entry);

  gncEntrySetInvoice (entry, invoice);
  invoice->entries = g_list_insert_sorted (invoice->entries, entry,
					   (GCompareFunc)gncEntryCompare);
  mark_invoice (invoice);
}

void gncInvoiceRemoveEntry (GncInvoice *invoice, GncEntry *entry)
{
  if (!invoice || !entry) return;

  gncEntrySetInvoice (entry, NULL);
  invoice->entries = g_list_remove (invoice->entries, entry);
  mark_invoice (invoice);
}

void gncBillAddEntry (GncInvoice *bill, GncEntry *entry)
{
  GncInvoice *old;

  if (!bill || !entry) return;

  old = gncEntryGetBill (entry);
  if (old == bill) return;	/* I already own this one */
  if (old) gncBillRemoveEntry (old, entry);

  gncEntrySetBill (entry, bill);
  bill->entries = g_list_insert_sorted (bill->entries, entry,
					   (GCompareFunc)gncEntryCompare);
  mark_invoice (bill);
}

void gncBillRemoveEntry (GncInvoice *bill, GncEntry *entry)
{
  if (!bill || !entry) return;

  gncEntrySetBill (entry, NULL);
  bill->entries = g_list_remove (bill->entries, entry);
  mark_invoice (bill);
}

/* Get Functions */

GNCBook * gncInvoiceGetBook (GncInvoice *invoice)
{
  if (!invoice) return NULL;
  return invoice->book;
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

GncOwner * gncInvoiceGetOwner (GncInvoice *invoice)
{
  if (!invoice) return NULL;
  return &invoice->owner;
}

Timespec gncInvoiceGetDateOpened (GncInvoice *invoice)
{
  Timespec ts; ts.tv_sec = 0; ts.tv_nsec = 0;
  if (!invoice) return ts;
  return invoice->date_opened;
}

Timespec gncInvoiceGetDatePosted (GncInvoice *invoice)
{
  Timespec ts; ts.tv_sec = 0; ts.tv_nsec = 0;
  if (!invoice) return ts;
  return invoice->date_posted;
}

Timespec gncInvoiceGetDateDue (GncInvoice *invoice)
{
  Transaction *txn;
  Timespec ts; ts.tv_sec = 0; ts.tv_nsec = 0;
  if (!invoice) return ts;
  txn = gncInvoiceGetPostedTxn (invoice);
  if (!txn) return ts;
  return xaccTransRetDateDueTS (txn);
}

GncBillTerm * gncInvoiceGetTerms (GncInvoice *invoice)
{
  if (!invoice) return 0;
  return invoice->terms;
}

const char * gncInvoiceGetBillingID (GncInvoice *invoice)
{
  if (!invoice) return 0;
  return invoice->billing_id;
}

const char * gncInvoiceGetNotes (GncInvoice *invoice)
{
  if (!invoice) return NULL;
  return invoice->notes;
}

static GncOwnerType gncInvoiceGetOwnerType (GncInvoice *invoice)
{
  GncOwner *owner;
  g_return_val_if_fail (invoice, GNC_OWNER_NONE);

  owner = gncOwnerGetEndOwner (gncInvoiceGetOwner (invoice));
  return (gncOwnerGetType (owner));
}

const char * gncInvoiceGetType (GncInvoice *invoice)
{
  if (!invoice) return NULL;

  switch (gncInvoiceGetOwnerType (invoice)) {
  case GNC_OWNER_CUSTOMER:
    return _("Invoice");
  case GNC_OWNER_VENDOR:
    return _("Bill");
  default:
    return NULL;
  }
}

gnc_commodity * gncInvoiceGetCommonCommodity (GncInvoice *invoice)
{
  if (!invoice) return NULL;
  return invoice->common_commodity;
}

GncOwner * gncInvoiceGetBillTo (GncInvoice *invoice)
{
  if (!invoice) return NULL;
  return &invoice->billto;
}

GNCLot * gncInvoiceGetPostedLot (GncInvoice *invoice)
{
  if (!invoice) return NULL;
  return invoice->posted_lot;
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

static void
gncInvoiceAttachInvoiceToLot (GncInvoice *invoice, GNCLot *lot)
{
  kvp_frame *kvp;
  kvp_value *value;
  
  if (!invoice || !lot)
    return;

  if (invoice->posted_lot) return;	/* Cannot reset invoice's lot */

  kvp = gnc_lot_get_slots (lot);
  value = kvp_value_new_guid (gncInvoiceGetGUID (invoice));
  kvp_frame_set_slot_path (kvp, value, GNC_INVOICE_ID, GNC_INVOICE_GUID, NULL);
  kvp_value_delete (value);
  gncInvoiceSetPostedLot (invoice, lot);
}

GncInvoice * gncInvoiceGetInvoiceFromLot (GNCLot *lot)
{
  kvp_frame *kvp;
  kvp_value *value;
  GUID *guid;
  GNCBook *book;

  if (!lot) return NULL;

  book = gnc_lot_get_book (lot);
  kvp = gnc_lot_get_slots (lot);
  value = kvp_frame_get_slot_path (kvp, GNC_INVOICE_ID, GNC_INVOICE_GUID, NULL);
  if (!value) return NULL;

  guid = kvp_value_get_guid (value);

  return xaccLookupEntity (gnc_book_get_entity_table (book),
			   guid, _GNC_MOD_NAME);
}

static void
gncInvoiceAttachInvoiceToTxn (GncInvoice *invoice, Transaction *txn)
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
  xaccTransSetTxnType (txn, TXN_TYPE_INVOICE);
  xaccTransCommitEdit (txn);
  gncInvoiceSetPostedTxn (invoice, txn);
}

GncInvoice * gncInvoiceGetInvoiceFromTxn (Transaction *txn)
{
  kvp_frame *kvp;
  kvp_value *value;
  GUID *guid;
  GNCBook *book;

  if (!txn) return NULL;

  book = xaccTransGetBook (txn);
  kvp = xaccTransGetSlots (txn);
  value = kvp_frame_get_slot_path (kvp, GNC_INVOICE_ID, GNC_INVOICE_GUID, NULL);
  if (!value) return NULL;

  guid = kvp_value_get_guid (value);

  return xaccLookupEntity (gnc_book_get_entity_table (book),
			   guid, _GNC_MOD_NAME);
}

struct lotmatch {
  GncOwner *owner;
  gboolean reverse;
};

static gboolean
gnc_lot_match_owner_payment (GNCLot *lot, gpointer user_data)
{
  struct lotmatch *lm = user_data;
  GncOwner owner_def, *owner;
  gnc_numeric balance = gnc_lot_get_balance (lot);

  /* Is this a payment lot */
  if (gnc_numeric_positive_p (lm->reverse ? balance :
			      gnc_numeric_neg (balance)))
    return FALSE;

  /* Is there an invoice attached? */
  if (gncInvoiceGetInvoiceFromLot (lot))
    return FALSE;

  /* Is it ours? */
  if (!gncOwnerGetOwnerFromLot (lot, &owner_def))
    return FALSE;
  owner = gncOwnerGetEndOwner (&owner_def);

  return gncOwnerEqual (owner, lm->owner);
}

Transaction * gncInvoicePostToAccount (GncInvoice *invoice, Account *acc,
				       Timespec *post_date, Timespec *due_date,
				       const char * memo)
{
  Transaction *txn;
  GNCLot *lot = NULL;
  GList *iter;
  GList *splitinfo = NULL;
  gnc_numeric total;
  gboolean reverse;
  const char *name;

  if (!invoice || !acc) return NULL;

  /* Stabilize the Billing Terms of this invoice */
  if (invoice->terms)
    gncInvoiceSetTerms (invoice,
			gncBillTermReturnChild (invoice->terms, TRUE));

  /* Figure out if we need to "reverse" the numbers. */
  reverse = (gncInvoiceGetOwnerType (invoice) == GNC_OWNER_CUSTOMER);

  /* Find an existing payment-lot for this owner */
  {
    LotList *lot_list;
    struct lotmatch lm;

    lm.reverse = reverse;
    lm.owner = gncOwnerGetEndOwner (gncInvoiceGetOwner (invoice));

    lot_list = xaccAccountFindOpenLots (acc, gnc_lot_match_owner_payment,
					&lm, NULL);
    if (lot_list)
      lot = lot_list->data;

    g_list_free (lot_list);
  }

  /* Create a new lot for this invoice, if we need to do so */
  if (!lot)
    lot = gnc_lot_new (invoice->book);

  /* Create a new transaction */
  txn = xaccMallocTransaction (invoice->book);
  xaccTransBeginEdit (txn);

  name = gncOwnerGetName (gncOwnerGetEndOwner (gncInvoiceGetOwner (invoice)));

  /* Set Transaction Description (Owner Name) , Num (invoice ID), Currency */
  xaccTransSetDescription (txn, name);
  xaccTransSetNum (txn, gncInvoiceGetID (invoice));
  xaccTransSetCurrency (txn, invoice->common_commodity);

  /* Entered and Posted at date */
  if (post_date) {
    xaccTransSetDateEnteredTS (txn, post_date);
    xaccTransSetDatePostedTS (txn, post_date);
    gncInvoiceSetDatePosted (invoice, *post_date);
  }

  if (due_date)
    xaccTransSetDateDueTS (txn, due_date);

  /* Iterate through the entries; sum up everything for each account.
   * then create the appropriate splits in this txn.
   */
  total = gnc_numeric_zero();
  for (iter = gncInvoiceGetEntries(invoice); iter; iter = iter->next) {
    gnc_numeric value, tax;
    GList *taxes;
    GncEntry * entry = iter->data;
    Account *this_acc;

    /* Stabilize the TaxTable in this entry */
    gncEntrySetTaxTable (entry,
			 gncTaxTableReturnChild (gncEntryGetTaxTable (entry),
						 TRUE));

    /* Obtain the Entry's Value and TaxValues */
    gncEntryGetValue (entry, &value, NULL, &tax, &taxes);

    /* add the value for the account split */
    this_acc = gncEntryGetAccount (entry);
    if (this_acc) {
      if (gnc_numeric_check (value) == GNC_ERROR_OK) {
	splitinfo = gncAccountValueAdd (splitinfo, this_acc, value);
	total = gnc_numeric_add (total, value, GNC_DENOM_AUTO, GNC_DENOM_LCD);
      } else
	g_warning ("bad value in our entry");
    }

    /* now merge in the TaxValues */
    splitinfo = gncAccountValueAddList (splitinfo, taxes);

    /* ... and add the tax total */
    if (gnc_numeric_check (tax) == GNC_ERROR_OK) 
      total = gnc_numeric_add (total, tax, GNC_DENOM_AUTO, GNC_DENOM_LCD);
    else
      g_warning ("bad tax in our entry");

  } /* for */

  /* Iterate through the splitinfo list and generate the splits */
  for (iter = splitinfo; iter; iter = iter->next) {
    Split *split;
    GncAccountValue *acc_val = iter->data;

    split = xaccMallocSplit (invoice->book);
    /* set action and memo? */

    xaccSplitSetMemo (split, memo);

    xaccSplitSetBaseValue (split, (reverse ? gnc_numeric_neg (acc_val->value)
				   : acc_val->value),
			   invoice->common_commodity);
    xaccAccountBeginEdit (acc_val->account);
    xaccAccountInsertSplit (acc_val->account, split);
    xaccAccountCommitEdit (acc_val->account);
    xaccTransAppendSplit (txn, split);
  }

  /* Now create the Posted split (which is negative -- it's a credit) */
  {
    Split *split = xaccMallocSplit (invoice->book);

    /* Set action/memo */
    xaccSplitSetMemo (split, memo);
    xaccSplitSetAction (split, gncInvoiceGetType (invoice));
			   
    xaccSplitSetBaseValue (split, (reverse ? total : gnc_numeric_neg (total)),
			   invoice->common_commodity);
    xaccAccountBeginEdit (acc);
    xaccAccountInsertSplit (acc, split);
    xaccAccountCommitEdit (acc);
    xaccTransAppendSplit (txn, split);

    /* add this split to the lot */
    gnc_lot_add_split (lot, split);
  }

  /* Now attach this invoice to the txn, lot, and account */
  gncInvoiceAttachInvoiceToLot (invoice, lot);
  gncInvoiceAttachInvoiceToTxn (invoice, txn);
  gncInvoiceSetPostedAcc (invoice, acc);

  xaccTransCommitEdit (txn);

  gncAccountValueDestroy (splitinfo);

  /* check the lot -- if we still look like a payment lot, then that
   * means we need to create a balancing split and create a new payment
   * lot for the next invoice
   */
  total = gnc_lot_get_balance (lot);
  if (!reverse)
    total = gnc_numeric_neg (total);

  if (gnc_numeric_negative_p (total)) {
    Transaction *t2;
    GNCLot *lot2;
    Split *split;
    char *memo2 = _("Automatic Payment Forward");

    t2 = xaccMallocTransaction (invoice->book);
    lot2 = gnc_lot_new (invoice->book);
    gncOwnerAttachToLot (gncOwnerGetEndOwner (gncInvoiceGetOwner (invoice)),
			 lot2);
    
    xaccTransBeginEdit (t2);
    xaccAccountBeginEdit (acc);

    /* Set Transaction Description (Owner Name), Currency */
    xaccTransSetDescription (t2, name);
    xaccTransSetCurrency (t2, invoice->common_commodity);

    /* Entered and Posted at date */
    if (post_date) {
      xaccTransSetDateEnteredTS (t2, post_date);
      xaccTransSetDatePostedTS (t2, post_date);
    }

    /* Balance out this lot */
    split = xaccMallocSplit (invoice->book);
    xaccSplitSetMemo (split, memo2);
    xaccSplitSetBaseValue (split, gnc_numeric_neg (total),
			   invoice->common_commodity);
    xaccAccountInsertSplit (acc, split);
    xaccTransAppendSplit (t2, split);
    gnc_lot_add_split (lot, split);

    /* And apply the pre-payment to a new lot */
    split = xaccMallocSplit (invoice->book);
    xaccSplitSetMemo (split, memo2);
    xaccSplitSetBaseValue (split, total, invoice->common_commodity);
    xaccAccountInsertSplit (acc, split);
    xaccTransAppendSplit (t2, split);
    gnc_lot_add_split (lot2, split);

    xaccTransCommitEdit (t2);
    xaccAccountCommitEdit (acc);
  }

  return txn;
}

static gboolean gncInvoiceDateExists (Timespec *date)
{
  g_return_val_if_fail (date, FALSE);
  if (date->tv_sec || date->tv_nsec) return TRUE;
  return FALSE;
}

gboolean gncInvoiceIsPosted (GncInvoice *invoice)
{
  if (!invoice) return FALSE;
  return gncInvoiceDateExists (&(invoice->date_posted));
}

GUID gncInvoiceRetGUID (GncInvoice *invoice)
{
  if (!invoice)
    return *xaccGUIDNULL();

  return invoice->guid;
}

GncInvoice * gncInvoiceLookupDirect (GUID guid, GNCBook *book)
{
  if (!book) return NULL;
  return gncInvoiceLookup (book, &guid);
}

GncInvoice * gncInvoiceLookup (GNCBook *book, const GUID *guid)
{
  if (!book || !guid) return NULL;
  return xaccLookupEntity (gnc_book_get_entity_table (book),
			   guid, _GNC_MOD_NAME);
}

void gncInvoiceBeginEdit (GncInvoice *invoice)
{
  if (!invoice) return;
}
void gncInvoiceCommitEdit (GncInvoice *invoice)
{
  if (!invoice) return;
  if (invoice->dirty)
    gncBusinessSetDirtyFlag (invoice->book, _GNC_MOD_NAME, TRUE);
  invoice->dirty = FALSE;
}

int gncInvoiceCompare (GncInvoice *a, GncInvoice *b)
{
  int compare;

  if (a == b) return 0;
  if (!a && b) return -1;
  if (a && !b) return 1;

  compare = safe_strcmp (a->id, b->id);
  if (compare) return compare;

  compare = timespec_cmp (&(a->date_opened), &(b->date_opened));
  if (compare) return compare;

  compare = timespec_cmp (&(a->date_posted), &(b->date_posted));
  if (compare) return compare;

  return guid_compare (&(a->guid), &(b->guid));
}

/* Package-Private functions */

static void addObj (GncInvoice *invoice)
{
  gncBusinessAddObject (invoice->book, _GNC_MOD_NAME, invoice, &invoice->guid);
}

static void remObj (GncInvoice *invoice)
{
  gncBusinessRemoveObject (invoice->book, _GNC_MOD_NAME, &invoice->guid);
}

static void _gncInvoiceCreate (GNCBook *book)
{
  gncBusinessCreate (book, _GNC_MOD_NAME);
}

static void _gncInvoiceDestroy (GNCBook *book)
{
  gncBusinessDestroy (book, _GNC_MOD_NAME);
}

static gboolean _gncInvoiceIsDirty (GNCBook *book)
{
  return gncBusinessIsDirty (book, _GNC_MOD_NAME);
}

static void _gncInvoiceMarkClean (GNCBook *book)
{
  gncBusinessSetDirtyFlag (book, _GNC_MOD_NAME, FALSE);
}

static void _gncInvoiceForeach (GNCBook *book, foreachObjectCB cb,
				gpointer user_data)
{
  gncBusinessForeach (book, _GNC_MOD_NAME, cb, user_data);
}

static const char * _gncInvoicePrintable (gpointer obj)
{
  GncInvoice *invoice = obj;

  g_return_val_if_fail (invoice, NULL);

  if (invoice->dirty || invoice->printname == NULL) {
    if (invoice->printname) g_free (invoice->printname);

    invoice->printname =
      g_strdup_printf ("%s%s", invoice->id,
		       gncInvoiceIsPosted (invoice) ? _(" (posted)") : "");
  }

  return invoice->printname;
}

static GncObject_t gncInvoiceDesc = {
  GNC_OBJECT_VERSION,
  _GNC_MOD_NAME,
  "Invoice",
  _gncInvoiceCreate,
  _gncInvoiceDestroy,
  _gncInvoiceIsDirty,
  _gncInvoiceMarkClean,
  _gncInvoiceForeach,
  _gncInvoicePrintable,
};

static void
reg_lot (void)
{
  static QueryObjectDef params[] = {
    { INVOICE_FROM_LOT, _GNC_MOD_NAME,
      (QueryAccess)gncInvoiceGetInvoiceFromLot },
    { NULL },
  };

  gncQueryObjectRegister (GNC_ID_LOT, NULL, params);
}

static void
reg_txn (void)
{
  static QueryObjectDef params[] = {
    { INVOICE_FROM_TXN, _GNC_MOD_NAME,
      (QueryAccess)gncInvoiceGetInvoiceFromTxn },
    { NULL },
  };

  gncQueryObjectRegister (GNC_ID_TRANS, NULL, params);
}

gboolean gncInvoiceRegister (void)
{
  static QueryObjectDef params[] = {
    { INVOICE_ID, QUERYCORE_STRING, (QueryAccess)gncInvoiceGetID },
    { INVOICE_OWNER, GNC_OWNER_MODULE_NAME, (QueryAccess)gncInvoiceGetOwner },
    { INVOICE_OPENED, QUERYCORE_DATE, (QueryAccess)gncInvoiceGetDateOpened },
    { INVOICE_DUE, QUERYCORE_DATE, (QueryAccess)gncInvoiceGetDateDue },
    { INVOICE_POSTED, QUERYCORE_DATE, (QueryAccess)gncInvoiceGetDatePosted },
    { INVOICE_IS_POSTED, QUERYCORE_BOOLEAN, (QueryAccess)gncInvoiceIsPosted },
    { INVOICE_BILLINGID, QUERYCORE_STRING, (QueryAccess)gncInvoiceGetBillingID },
    { INVOICE_NOTES, QUERYCORE_STRING, (QueryAccess)gncInvoiceGetNotes },
    { INVOICE_ACC, GNC_ID_ACCOUNT, (QueryAccess)gncInvoiceGetPostedAcc },
    { INVOICE_POST_TXN, GNC_ID_TRANS, (QueryAccess)gncInvoiceGetPostedTxn },
    { INVOICE_TYPE, QUERYCORE_STRING, (QueryAccess)gncInvoiceGetType },
    { INVOICE_TERMS, GNC_BILLTERM_MODULE_NAME, (QueryAccess)gncInvoiceGetTerms },
    { INVOICE_BILLTO, GNC_OWNER_MODULE_NAME, (QueryAccess)gncInvoiceGetBillTo },
    { QUERY_PARAM_BOOK, GNC_ID_BOOK, (QueryAccess)gncInvoiceGetBook },
    { QUERY_PARAM_GUID, QUERYCORE_GUID, (QueryAccess)gncInvoiceGetGUID },
    { NULL },
  };

  gncQueryObjectRegister (_GNC_MOD_NAME, (QuerySort)gncInvoiceCompare, params);
  reg_lot ();
  reg_txn ();

  return gncObjectRegister (&gncInvoiceDesc);
}

gint64 gncInvoiceNextID (GNCBook *book)
{
  return gnc_book_get_counter (book, _GNC_MOD_NAME);
}
