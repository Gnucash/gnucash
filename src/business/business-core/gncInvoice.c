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
  char *	terms;
  char *	printname;
  GList * 	entries;
  GncOwner	owner;
  Timespec 	date_opened;
  Timespec 	date_posted;
  Timespec 	date_due;
  Timespec 	date_paid;

  Account * 	posted_acc;
  Transaction * posted_txn;
  Transaction * paid_txn;

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

/* Create/Destroy Functions */

GncInvoice *gncInvoiceCreate (GNCBook *book)
{
  GncInvoice *invoice;

  if (!book) return NULL;

  invoice = g_new0 (GncInvoice, 1);
  invoice->book = book;

  invoice->id = CACHE_INSERT ("");
  invoice->notes = CACHE_INSERT ("");
  invoice->terms = CACHE_INSERT ("");

  invoice->active = TRUE;

  xaccGUIDNew (&invoice->guid, book);
  addObj (invoice);

  return invoice;
}

void gncInvoiceDestroy (GncInvoice *invoice)
{
  if (!invoice) return;

  CACHE_REMOVE (invoice->id);
  CACHE_REMOVE (invoice->notes);
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
  invoice->dirty = TRUE;
}

void gncInvoiceSetOwner (GncInvoice *invoice, GncOwner *owner)
{
  if (!invoice || !owner) return;
  gncOwnerCopy (owner, &invoice->owner);
  invoice->dirty = TRUE;
}

void gncInvoiceSetDateOpened (GncInvoice *invoice, const Timespec *date)
{
  if (!invoice || !date) return;
  invoice->date_opened = *date;
  invoice->dirty = TRUE;
}

void gncInvoiceSetDatePosted (GncInvoice *invoice, const Timespec *date)
{
  if (!invoice || !date) return;
  invoice->date_posted = *date;
  invoice->dirty = TRUE;
}

void gncInvoiceSetDateDue (GncInvoice *invoice, const Timespec *date)
{
  if (!invoice || !date) return;
  invoice->date_due = *date;
  invoice->dirty = TRUE;
}

void gncInvoiceSetDatePaid (GncInvoice *invoice, const Timespec *date)
{
  if (!invoice || !date) return;
  invoice->date_paid = *date;
  invoice->dirty = TRUE;
}

void gncInvoiceSetTerms (GncInvoice *invoice, const char *terms)
{
  if (!invoice) return;
  SET_STR (invoice->terms, terms);
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

void gncInvoiceSetPaidTxn (GncInvoice *invoice, Transaction *txn)
{
  if (!invoice) return;

  invoice->paid_txn = txn;
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
  Timespec ts; ts.tv_sec = 0; ts.tv_nsec = 0;
  if (!invoice) return ts;
  return invoice->date_due;
}

Timespec gncInvoiceGetDatePaid (GncInvoice *invoice)
{
  Timespec ts; ts.tv_sec = 0; ts.tv_nsec = 0;
  if (!invoice) return ts;
  return invoice->date_paid;
}

const char * gncInvoiceGetTerms (GncInvoice *invoice)
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

Transaction * gncInvoiceGetPaidTxn (GncInvoice *invoice)
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
gncInvoiceAttachInvoiceToTxn (GncInvoice *invoice, Transaction *txn, char type)
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
  xaccTransSetTxnType (txn, type);
  xaccTransCommitEdit (txn);

  switch (type) {
  case TXN_TYPE_PAYMENT:
    gncInvoiceSetPaidTxn (invoice, txn);
    break;
  case TXN_TYPE_INVOICE:
    gncInvoiceSetPostedTxn (invoice, txn);
    break;
  default:
    break;
  }
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
				       Timespec *date, gboolean reverse)
{
  Transaction *txn;
  GList *iter;
  GList *splitinfo = NULL;
  gnc_numeric total;
  gnc_commodity *commonCommodity = NULL; /* XXX: FIXME */
  struct acct_val {
    Account *	acc;
    gnc_numeric val;
  } *acc_val;

  if (!invoice || !acc) return NULL;

  /* XXX: Figure out the common currency */

  txn = xaccMallocTransaction (invoice->book);
  xaccTransBeginEdit (txn);

  /* Set Transaction Description (customer), Num (invoice ID), Currency */
  xaccTransSetDescription
    (txn, gncOwnerGetName (gncInvoiceGetOwner (invoice)));
			   
  xaccTransSetNum (txn, gncInvoiceGetID (invoice));
  xaccTransSetCurrency (txn, commonCommodity);

  /* Entered and Posted at date */
  if (date) {
    xaccTransSetDateEnteredTS (txn, date);
    xaccTransSetDatePostedTS (txn, date);
    gncInvoiceSetDatePosted (invoice, date);
  }

  /* Set the txn due date to be equal to the invoice */
  {
    Timespec ddue = gncInvoiceGetDateDue (invoice);
    xaccTransSetDateDueTS (txn, &ddue);
  }

  /* Iterate through the entries; sum up everything for each account.
   * then create the appropriate splits in this txn.
   */
  total = gnc_numeric_zero();
  for (iter = gncInvoiceGetEntries(invoice); iter; iter = iter->next) {
    gnc_numeric value, tax;
    GncEntry * entry = iter->data;
    Account *this_acc;

    /* Obtain the Entry Value and TaxValue */
    gncEntryGetValue (entry, &value, &tax);

    /* add the value for the account split */
    this_acc = gncEntryGetAccount (entry);
    if (this_acc) {
      /* Find the account value for this_acc.  If we haven't seen this
       * account before, create a new total and add to list
       */
      GET_OR_ADD_ACCVAL (splitinfo, this_acc, acc_val);

      acc_val->val = gnc_numeric_add_fixed (acc_val->val, value);
      total = gnc_numeric_add_fixed (total, value);
    }

    /* Repeat for the TaxValue */
    this_acc = gncEntryGetTaxAccount (entry);
    if (this_acc) {

      GET_OR_ADD_ACCVAL (splitinfo, this_acc, acc_val);
      acc_val->val = gnc_numeric_add_fixed (acc_val->val, tax);
      total = gnc_numeric_add_fixed (total, tax);
    }
  } /* for */

  /* Iterate through the splitinfo list and generate the splits */
  for (iter = splitinfo; iter; iter = iter->next) {
    Split *split;
    acc_val = iter->data;

    split = xaccMallocSplit (invoice->book);
    /* set action and memo? */

    xaccSplitSetBaseValue (split, (reverse ? gnc_numeric_neg (acc_val->val)
				   : acc_val->val),
			   commonCommodity);
    xaccAccountBeginEdit (acc_val->acc);
    xaccAccountInsertSplit (acc_val->acc, split);
    xaccAccountCommitEdit (acc_val->acc);
    xaccTransAppendSplit (txn, split);
  }

  /* Now create the Posted split (which is negative -- it's a credit) */
  {
    Split *split = xaccMallocSplit (invoice->book);
    /* Set action/memo */
    xaccSplitSetBaseValue (split, (reverse ? total : gnc_numeric_neg (total)),
			   commonCommodity);
    xaccAccountBeginEdit (acc);
    xaccAccountInsertSplit (acc, split);
    xaccAccountCommitEdit (acc);
    xaccTransAppendSplit (txn, split);
  }

  /* Now attach this invoice to the txn and account */
  gncInvoiceAttachInvoiceToTxn (invoice, txn, TXN_TYPE_INVOICE);
  gncInvoiceSetPostedAcc (invoice, acc);

  xaccTransCommitEdit (txn);

  return txn;
}

Transaction *
gncInvoicePayToAccount (GncInvoice *invoice, Account *acc,
			Timespec *paid_date)
{
  Transaction *txn;
  gnc_numeric total;
  gnc_commodity *commonCommodity = NULL; /* XXX: FIXME */
  Account *acct;

  if (!invoice || !acc) return NULL;

  /* Must have posted before you can pay */
  g_return_val_if_fail (gncInvoiceGetPostedTxn(invoice), NULL);
  acct = gncInvoiceGetPostedAcc (invoice);
  g_return_val_if_fail (acct, NULL);

  /* Determine the value for this payment..  Find the split into
   * the posted account and pull the value out of that.
   * XXX: Should the payment value be an argument here?
   */
  {
    GList *l = xaccTransGetSplitList (gncInvoiceGetPostedTxn (invoice));

    for (; l; l=l->next) {
      Split *s = l->data;

      if (xaccSplitGetAccount (s) == acct) {
	total = xaccSplitGetValue (s);
	break;
      }
    }

    /* Make sure we found the Split */
    g_return_val_if_fail (l, NULL);
  }

  /* XXX: Figure out the common currency */

  txn = xaccMallocTransaction (invoice->book);
  xaccTransBeginEdit (txn);

  /* Set Transaction Description (customer), Num (invoice ID), Currency */
  xaccTransSetDescription
    (txn, gncOwnerGetName (gncInvoiceGetOwner (invoice)));
			   
  xaccTransSetNum (txn, gncInvoiceGetID (invoice));
  xaccTransSetCurrency (txn, commonCommodity);

  /* Entered and Posted at date */
  if (paid_date) {
    xaccTransSetDateEnteredTS (txn, paid_date);
    xaccTransSetDatePostedTS (txn, paid_date);
    gncInvoiceSetDatePaid (invoice, paid_date);
  }

  /* create the split to the payment account */
  {
    Split *split = xaccMallocSplit (invoice->book);
    /* Set action/memo */
    xaccSplitSetBaseValue (split, total, commonCommodity);
    xaccAccountBeginEdit (acc);
    xaccAccountInsertSplit (acc, split);
    xaccAccountCommitEdit (acc);
    xaccTransAppendSplit (txn, split);
  }

  /* Now create the Payment split for the posted acc, reverse value */
  {
    Split *split = xaccMallocSplit (invoice->book);

    /* Set action/memo */
    xaccSplitSetBaseValue (split, gnc_numeric_neg (total), commonCommodity);
    xaccAccountBeginEdit (acct);
    xaccAccountInsertSplit (acct, split);
    xaccAccountCommitEdit (acct);
    xaccTransAppendSplit (txn, split);
  }

  /* Now attach this invoice to the txn and account */
  gncInvoiceAttachInvoiceToTxn (invoice, txn, TXN_TYPE_PAYMENT);

  xaccTransCommitEdit (txn);

  return txn;
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

gboolean gncInvoiceIsPaid (GncInvoice *invoice)
{
  if (!invoice) return FALSE;
  return gncInvoiceDateExists (&(invoice->date_paid));
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
}

int gncInvoiceCompare (GncInvoice *a, GncInvoice *b)
{
  int compare;

  if (a == b) return 0;
  if (!a && b) return -1;
  if (a && !b) return 1;

  compare = safe_strcmp (a->id, b->id);
  if (!compare) return compare;

  compare = timespec_cmp (&(a->date_opened), &(b->date_opened));
  if (!compare) return compare;

  compare = timespec_cmp (&(a->date_posted), &(b->date_posted));
  if (!compare) return compare;

  compare = timespec_cmp (&(a->date_paid), &(b->date_paid));
  if (!compare) return compare;

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
      g_strdup_printf ("%s%s%s", invoice->id,
		       gncInvoiceIsPosted (invoice) ? _(" (posted)") : "",
		       gncInvoiceIsPaid (invoice) ? _(" (paid)") : "");
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
  _gncInvoiceForeach,
  _gncInvoicePrintable,
};

gboolean gncInvoiceRegister (void)
{
  static QueryObjectDef params[] = {
    { INVOICE_ID, QUERYCORE_STRING, (QueryAccess)gncInvoiceGetID },
    { INVOICE_OWNER, GNC_OWNER_MODULE_NAME, (QueryAccess)gncInvoiceGetOwner },
    { INVOICE_OPENED, QUERYCORE_DATE, (QueryAccess)gncInvoiceGetDateOpened },
    { INVOICE_POSTED, QUERYCORE_DATE, (QueryAccess)gncInvoiceGetDatePosted },
    { INVOICE_DUE, QUERYCORE_DATE, (QueryAccess)gncInvoiceGetDateDue },
    { INVOICE_PAID, QUERYCORE_DATE, (QueryAccess)gncInvoiceGetDatePaid },
    { INVOICE_IS_POSTED, QUERYCORE_BOOLEAN, (QueryAccess)gncInvoiceIsPosted },
    { INVOICE_IS_PAID, QUERYCORE_BOOLEAN, (QueryAccess)gncInvoiceIsPaid },
    { INVOICE_NOTES, QUERYCORE_STRING, (QueryAccess)gncInvoiceGetNotes },
    { INVOICE_ACC, GNC_ID_ACCOUNT, (QueryAccess)gncInvoiceGetPostedAcc },
    { INVOICE_POST_TXN, GNC_ID_TRANS, (QueryAccess)gncInvoiceGetPostedTxn },
    { INVOICE_PD_TXN, GNC_ID_TRANS, (QueryAccess)gncInvoiceGetPaidTxn },
    { QUERY_PARAM_BOOK, GNC_ID_BOOK, (QueryAccess)gncInvoiceGetBook },
    { QUERY_PARAM_GUID, QUERYCORE_GUID, (QueryAccess)gncInvoiceGetGUID },
    { NULL },
  };

  gncQueryObjectRegister (_GNC_MOD_NAME, (QuerySort)gncInvoiceCompare, params);

  return gncObjectRegister (&gncInvoiceDesc);
}

static gint lastId = 187;	/* XXX */

gint gncInvoiceNextID (GNCBook *book)
{
  return lastId++;
}
