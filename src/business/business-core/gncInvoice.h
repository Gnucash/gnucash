/*
 * gncInvoice.h -- the Core Business Invoice Interface
 * Copyright (C) 2001 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef GNC_INVOICE_H_
#define GNC_INVOICE_H_

struct _gncInvoice;
typedef struct _gncInvoice GncInvoice;

#include "gncEntry.h"
#include "gncOwner.h"

#define GNC_INVOICE_MODULE_NAME "gncInvoice"

/* Create/Destroy Functions */

GncInvoice *gncInvoiceCreate (GNCBook *book);
void gncInvoiceDestroy (GncInvoice *invoice);

/* Set Functions */

void gncInvoiceSetID (GncInvoice *invoice, const char *id);
void gncInvoiceSetOwner (GncInvoice *invoice, GncOwner *owner);
void gncInvoiceSetDateOpened (GncInvoice *invoice, Timespec date);
void gncInvoiceSetDatePosted (GncInvoice *invoice, Timespec date);
void gncInvoiceSetTerms (GncInvoice *invoice, const char *terms);
void gncInvoiceSetBillingID (GncInvoice *invoice, const char *billing_id);
void gncInvoiceSetNotes (GncInvoice *invoice, const char *notes);
void gncInvoiceSetCommonCommodity (GncInvoice *invoice, gnc_commodity *com);
void gncInvoiceSetActive (GncInvoice *invoice, gboolean active);

void gncInvoiceAddEntry (GncInvoice *invoice, GncEntry *entry);
void gncInvoiceRemoveEntry (GncInvoice *invoice, GncEntry *entry);

/* Get Functions */

GNCBook * gncInvoiceGetBook (GncInvoice *invoice);
const GUID * gncInvoiceGetGUID (GncInvoice *invoice);
const char * gncInvoiceGetID (GncInvoice *invoice);
GncOwner * gncInvoiceGetOwner (GncInvoice *invoice);
Timespec gncInvoiceGetDateOpened (GncInvoice *invoice);
Timespec gncInvoiceGetDatePosted (GncInvoice *invoice);
Timespec gncInvoiceGetDateDue (GncInvoice *invoice);
const char * gncInvoiceGetTerms (GncInvoice *invoice);
const char * gncInvoiceGetBillingID (GncInvoice *invoice);
const char * gncInvoiceGetNotes (GncInvoice *invoice);
gnc_commodity * gncInvoiceGetCommonCommodity (GncInvoice *invoice);
gboolean gncInvoiceGetActive (GncInvoice *invoice);

Transaction * gncInvoiceGetPostedTxn (GncInvoice *invoice);
Account * gncInvoiceGetPostedAcc (GncInvoice *invoice);

GList * gncInvoiceGetEntries (GncInvoice *invoice);

/* Post this invoice to an account.  Returns the new Transaction
 * that is tied to this invoice.   The transaction is set with
 * the posted date.  Set reverse to TRUE to reverse the sense of
 * the splits (necessary for posting to A/R accounts from Income
 * splits).
 */
Transaction *
gncInvoicePostToAccount (GncInvoice *invoice, Account *acc,
			 Timespec *posted_date, Timespec *due_date,
			 const char *description, gboolean reverse);


/* Given a transaction, find and return the Invoice */
GncInvoice * gncInvoiceGetInvoiceFromTxn (Transaction *txn);

GncInvoice * gncInvoiceLookup (GNCBook *book, const GUID *guid);
gboolean gncInvoiceIsDirty (GncInvoice *invoice);
void gncInvoiceBeginEdit (GncInvoice *invoice);
void gncInvoiceCommitEdit (GncInvoice *invoice);
int gncInvoiceCompare (GncInvoice *a, GncInvoice *b);
gboolean gncInvoiceIsPosted (GncInvoice *invoice);
gboolean gncInvoiceIsPaid (GncInvoice *invoice);

#define INVOICE_ID	"id"
#define INVOICE_OWNER	"owner"
#define INVOICE_OPENED	"date_opened"
#define INVOICE_POSTED	"date_posted"
#define INVOICE_DUE	"date_due"
#define INVOICE_IS_POSTED	"is_posted?"
#define INVOICE_TERMS	"terms"
#define INVOICE_BILLINGID	"billing_id"
#define INVOICE_NOTES	"notes"
#define INVOICE_ACC	"account"
#define INVOICE_POST_TXN	"posted_txn"

#endif /* GNC_INVOICE_H_ */
