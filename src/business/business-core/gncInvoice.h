/*
 * gncInvoice.h -- the Core Business Invoice Interface
 * Copyright (C) 2001 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef GNC_INVOICE_H_
#define GNC_INVOICE_H_

struct _gncInvoice;
typedef struct _gncInvoice GncInvoice;

typedef enum {
  GNC_INVOICE_NONE = 0,
  GNC_INVOICE_CUSTOMER = 1,
  GNC_INVOICE_VENDOR = 2
} GncInvoiceType;

#include "gncCustomer.h"
#include "gncVendor.h"
#include "gncEntry.h"

#define GNC_INVOICE_MODULE_NAME "gncInvoice"

/* Create/Destroy Functions */

GncInvoice *gncInvoiceCreate (GNCBook *book, GncInvoiceType type);
void gncInvoiceDestroy (GncInvoice *invoice);

/* Set Functions */

void gncInvoiceSetID (GncInvoice *invoice, const char *id);
void gncInvoiceSetCustomer (GncInvoice *invoice, GncCustomer *customer);
void gncInvoiceSetVendor (GncInvoice *invoice, GncVendor *vendor);
void gncInvoiceSetDateOpened (GncInvoice *invoice, const Timespec *date);
void gncInvoiceSetDateDue (GncInvoice *invoice, const Timespec *date);
void gncInvoiceSetDateClosed (GncInvoice *invoice, const Timespec *date);
void gncInvoiceSetTerms (GncInvoice *invoice, gint terms);
void gncInvoiceSetNotes (GncInvoice *invoice, const char *notes);
void gncInvoiceSetActive (GncInvoice *invoice, gboolean active);

void gncInvoiceAddEntry (GncInvoice *invoice, GncEntry *entry);
void gncInvoiceRemoveEntry (GncInvoice *invoice, GncEntry *entry);

/* Get Functions */

GNCBook * gncInvoiceGetBook (GncInvoice *invoice);
const GUID * gncInvoiceGetGUID (GncInvoice *invoice);
const char * gncInvoiceGetID (GncInvoice *invoice);
GncInvoiceType gncInvoiceGetType (GncInvoice *invoice);
GncCustomer * gncInvoiceGetCustomer (GncInvoice *invoice);
GncVendor * gncInvoiceGetVendor (GncInvoice *invoice);
Timespec gncInvoiceGetDateOpened (GncInvoice *invoice);
Timespec gncInvoiceGetDateDue (GncInvoice *invoice);
Timespec gncInvoiceGetDateClosed (GncInvoice *invoice);
gint gncInvoiceGetTerms (GncInvoice *invoice);
const char * gncInvoiceGetNotes (GncInvoice *invoice);
gboolean gncInvoiceGetActive (GncInvoice *invoice);

Transaction * gncInvoiceGetPostedTxn (GncInvoice *invoice);
Account * gncInvoiceGetPostedAcc (GncInvoice *invoice);

GList * gncInvoiceGetEntries (GncInvoice *invoice);

/* Post this invoice to an account.  Returns the new Transaction
 * that is tied to this invoice.   The transaction is set with
 * the posted date.
 */
Transaction * gncInvoicePostToAccount (GncInvoice *invoice, Account *acc,
				       Timespec *date);

/* Given a transaction, find and return the Invoice */
GncInvoice * gncInvoiceGetInvoiceFromTxn (Transaction *txn);

GncInvoice * gncInvoiceLookup (GNCBook *book, const GUID *guid);
gboolean gncInvoiceIsDirty (GncInvoice *invoice);
void gncInvoiceCommitEdit (GncInvoice *invoice);

#endif /* GNC_INVOICE_H_ */
