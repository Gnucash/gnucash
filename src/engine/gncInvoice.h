/********************************************************************\
 * gncInvoice.h -- the Core Business Invoice Interface              *
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
/** @addtogroup Business
    @{ */
/** @addtogroup Invoice

An invoice holds a list of entries, a pointer to the customer,
and the job, the dates entered and posted, as well as the account,
transaction and lot for the posted invoice.
    @{ */
/** @file gncInvoice.h
    @brief  Business Invoice Interface
    @author Copyright (C) 2001,2006 Derek Atkins <warlord@MIT.EDU>
    @author Copyright (c) 2005 Neil Williams <linux@codehelp.co.uk>
*/

#ifndef GNC_INVOICE_H_
#define GNC_INVOICE_H_

struct _gncInvoice;
typedef struct _gncInvoice GncInvoice;
typedef struct _gncInvoiceClass GncInvoiceClass;
typedef GList GncInvoiceList;

#include "gncBillTerm.h"
#include "gncEntry.h"
#include "gncOwner.h"
#include "gnc-lot.h"
#include "qofbook.h"
#include "qofbook.h"
#include "gnc-pricedb.h"

#define GNC_ID_INVOICE    "gncInvoice"

typedef enum
{
    GNC_INVOICE_UNDEFINED ,
    GNC_INVOICE_CUST_INVOICE ,       /* Invoice */
    GNC_INVOICE_VEND_INVOICE ,       /* Bill */
    GNC_INVOICE_EMPL_INVOICE ,       /* Voucher */
    GNC_INVOICE_CUST_CREDIT_NOTE ,   /* Credit Note for a customer */
    GNC_INVOICE_VEND_CREDIT_NOTE ,   /* Credit Note from a vendor */
    GNC_INVOICE_EMPL_CREDIT_NOTE ,   /* Credit Note from an employee,
                                        not sure this makes sense,
                                        but all code is symmetrical
                                        so I've added it to prevent unexpected errors */
    GNC_INVOICE_NUM_TYPES
} GncInvoiceType;

/* --- type macros --- */
#define GNC_TYPE_INVOICE            (gnc_invoice_get_type ())
#define GNC_INVOICE(o)              \
     (G_TYPE_CHECK_INSTANCE_CAST ((o), GNC_TYPE_INVOICE, GncInvoice))
#define GNC_INVOICE_CLASS(k)        \
     (G_TYPE_CHECK_CLASS_CAST((k), GNC_TYPE_INVOICE, GncInvoiceClass))
#define GNC_IS_INVOICE(o)           \
     (G_TYPE_CHECK_INSTANCE_TYPE ((o), GNC_TYPE_INVOICE))
#define GNC_IS_INVOICE_CLASS(k)     \
     (G_TYPE_CHECK_CLASS_TYPE ((k), GNC_TYPE_INVOICE))
#define GNC_INVOICE_GET_CLASS(o)    \
     (G_TYPE_INSTANCE_GET_CLASS ((o), GNC_TYPE_INVOICE, GncInvoiceClass))
GType gnc_invoice_get_type(void);

/** @name Create/Destroy Functions
 @{ */
GncInvoice *gncInvoiceCreate (QofBook *book);

void gncInvoiceDestroy (GncInvoice *invoice);

/** Create a new GncInvoice object as a deep copy of the given other
 * invoice.
 *
 * The returned new invoice has everything copied from the other
 * invoice, including the ID string field. All GncEntries are newly
 * allocated copies of the original invoice's entries. */
GncInvoice *gncInvoiceCopy (const GncInvoice *other_invoice);
/** @} */

/** @name Set Functions
 @{ */
void gncInvoiceSetID (GncInvoice *invoice, const char *id);
void gncInvoiceSetOwner (GncInvoice *invoice, GncOwner *owner);
/** Set the DateOpened using a GDate argument. (Note: Internally this stores
the date in a Timespec as created through timespecCanonicalDayTime()). */
void gncInvoiceSetDateOpenedGDate (GncInvoice *invoice, const GDate *date);
void gncInvoiceSetDateOpened (GncInvoice *invoice, Timespec date);
void gncInvoiceSetDatePosted (GncInvoice *invoice, Timespec date);
void gncInvoiceSetTerms (GncInvoice *invoice, GncBillTerm *terms);
void gncInvoiceSetBillingID (GncInvoice *invoice, const char *billing_id);
void gncInvoiceSetNotes (GncInvoice *invoice, const char *notes);
void gncInvoiceSetCurrency (GncInvoice *invoice, gnc_commodity *currency);
void gncInvoiceSetActive (GncInvoice *invoice, gboolean active);
void gncInvoiceSetIsCreditNote (GncInvoice *invoice, gboolean credit_note);
void gncInvoiceSetBillTo (GncInvoice *invoice, GncOwner *billto);
void gncInvoiceSetToChargeAmount (GncInvoice *invoice, gnc_numeric amount);
/** @} */

void gncInvoiceAddEntry (GncInvoice *invoice, GncEntry *entry);
void gncInvoiceRemoveEntry (GncInvoice *invoice, GncEntry *entry);
void gncInvoiceAddPrice (GncInvoice *invoice, GNCPrice *price);

/** Call this function when adding an entry to a bill instead of an invoice */
void gncBillAddEntry (GncInvoice *bill, GncEntry *entry);
void gncBillRemoveEntry (GncInvoice *bill, GncEntry *entry);

/** Call this function when an Entry is changed and you want to
    re-sort the list of entries
*/
void gncInvoiceSortEntries (GncInvoice *invoice);

/** Remove all entries from an invoice. To be called before
 *  destroying an invoice.
*/
void gncInvoiceRemoveEntries (GncInvoice *invoice);

/** @name Get Functions
 @{ */
const char * gncInvoiceGetID (const GncInvoice *invoice);
const GncOwner * gncInvoiceGetOwner (const GncInvoice *invoice);
Timespec gncInvoiceGetDateOpened (const GncInvoice *invoice);
Timespec gncInvoiceGetDatePosted (const GncInvoice *invoice);
Timespec gncInvoiceGetDateDue (const GncInvoice *invoice);
GncBillTerm * gncInvoiceGetTerms (const GncInvoice *invoice);
const char * gncInvoiceGetBillingID (const GncInvoice *invoice);
const char * gncInvoiceGetNotes (const GncInvoice *invoice);
GncOwnerType gncInvoiceGetOwnerType (const GncInvoice *invoice);
GList * gncInvoiceGetTypeListForOwnerType (const GncOwnerType type);
GncInvoiceType gncInvoiceGetType (const GncInvoice *invoice);
const char * gncInvoiceGetTypeString (const GncInvoice *invoice);
gnc_commodity * gncInvoiceGetCurrency (const GncInvoice *invoice);
GncOwner * gncInvoiceGetBillTo (GncInvoice *invoice);
gnc_numeric gncInvoiceGetToChargeAmount (const GncInvoice *invoice);
gboolean gncInvoiceGetActive (const GncInvoice *invoice);
gboolean gncInvoiceGetIsCreditNote (const GncInvoice *invoice);

GNCLot * gncInvoiceGetPostedLot (const GncInvoice *invoice);
Transaction * gncInvoiceGetPostedTxn (const GncInvoice *invoice);
Account * gncInvoiceGetPostedAcc (const GncInvoice *invoice);
/** @} */

/** Return the "total" amount of the invoice as seen on the document
 *  (and shown to the user in the reports and invoice ledger). */
gnc_numeric gncInvoiceGetTotal (GncInvoice *invoice);
gnc_numeric gncInvoiceGetTotalOf (GncInvoice *invoice, GncEntryPaymentType type);
gnc_numeric gncInvoiceGetTotalSubtotal (GncInvoice *invoice);
gnc_numeric gncInvoiceGetTotalTax (GncInvoice *invoice);

typedef GList EntryList;
EntryList * gncInvoiceGetEntries (GncInvoice *invoice);
GNCPrice * gncInvoiceGetPrice(GncInvoice *invoice, gnc_commodity* commodity);

/** Depending on the invoice type, invoices have a different effect
 *  on the balance. Customer invoices increase the balance, while
 *  vendor bills decrease the balance. Credit notes have the opposite
 *  effect.
 *
 *  Returns TRUE if the invoice will increase the balance or FALSE
 *  otherwise.
 */
gboolean gncInvoiceAmountPositive (const GncInvoice *invoice);

/** Post this invoice to an account.  Returns the new Transaction
 * that is tied to this invoice.   The transaction is set with
 * the supplied posted date, due date, and memo.  The Transaction
 * description is set to the name of the company.
 *
 * If accumulate splits is TRUE, entries in the same account
 * will be merged into one single split in that account.
 * Otherwise each entry will be posted as a separate split,
 * possibly resulting in multiple splits in one account.
 *
 * If autopay is TRUE, the code will try to find pre-payments,
 * invoices or credit notes that can reduce the amount due for this
 * invoice, marking the invoice as fully or partially paid, depending
 * on the amounts on all documents involved. If autopay is FALSE,
 * it's the user's responsibility to explicitly pay the invoice.
 *
 */
Transaction *
gncInvoicePostToAccount (GncInvoice *invoice, Account *acc,
                         Timespec *posted_date, Timespec *due_date,
                         const char *memo, gboolean accumulatesplits,
                         gboolean autopay);

/**
 * Unpost this invoice.  This will destroy the posted transaction and
 * return the invoice to its unposted state.  It may leave empty lots
 * out there.  If reset_tax_tables is TRUE, then it will also revert
 * all the Tax Tables to the parent, which will potentially change the
 * total value of the invoice.  It may also leave some orphaned Tax
 * Table children.
 *
 * Returns TRUE if successful, FALSE if there is a problem.
 */
gboolean
gncInvoiceUnpost (GncInvoice *invoice, gboolean reset_tax_tables);

/**
 * Attempt to pay the invoice using open payment lots and
 * lots for documents of the opposite sign (credit notes versus
 * invoices).
 */
void
gncInvoiceAutoApplyPayments (GncInvoice *invoice);

/**
 * A convenience function to apply a payment to an invoice.
 * It creates a lot for a payment optionally based on an existing
 * transaction and then tries to balance it with
 * the given invoice.
 * Contrary to gncOwnerApplyPayment, no other open documents
 * or payments for the owner will be considered
 * to balance the payment.
 *
 * This code is actually a convenience wrapper around gncOwnerCreatePaymentLot
 * and gncOwnerAutoApplyPaymentsWithLots. See their descriptions for more
 * details on what happens exactly.
 */
void
gncInvoiceApplyPayment (const GncInvoice *invoice, Transaction *txn,
                        Account *xfer_acc, gnc_numeric amount,
                        gnc_numeric exch, Timespec date,
                        const char *memo, const char *num);


/** Given a transaction, find and return the Invoice */
GncInvoice * gncInvoiceGetInvoiceFromTxn (const Transaction *txn);

/** Given a LOT, find and return the Invoice attached to the lot */
GncInvoice * gncInvoiceGetInvoiceFromLot (GNCLot *lot);

/** Return a pointer to the instance gncInvoice that is identified
 *  by the guid, and is residing in the book. Returns NULL if the
 *  instance can't be found.
 *  Equivalent function prototype is
 *  GncInvoice * gncInvoiceLookup (QofBook *book, const GncGUID *guid);
 */
static inline GncInvoice * gncInvoiceLookup (const QofBook *book, const GncGUID *guid)
{
    QOF_BOOK_RETURN_ENTITY(book, guid, GNC_ID_INVOICE, GncInvoice);
}

void gncInvoiceBeginEdit (GncInvoice *invoice);
void gncInvoiceCommitEdit (GncInvoice *invoice);
int gncInvoiceCompare (const GncInvoice *a, const GncInvoice *b);
gboolean gncInvoiceIsPosted (const GncInvoice *invoice);
gboolean gncInvoiceIsPaid (const GncInvoice *invoice);

#define INVOICE_ID          "id"
#define INVOICE_OWNER       "owner"
#define INVOICE_OPENED      "date_opened"
#define INVOICE_POSTED      "date_posted"
#define INVOICE_DUE         "date_due"
#define INVOICE_IS_POSTED   "is_posted?"
#define INVOICE_IS_PAID     "is_paid?"
#define INVOICE_TERMS       "terms"
#define INVOICE_BILLINGID   "billing_id"
#define INVOICE_NOTES       "notes"
#define INVOICE_ACC         "account"
#define INVOICE_POST_TXN    "posted_txn"
#define INVOICE_POST_LOT    "posted_lot"
#define INVOICE_IS_CN       "credit_note"
#define INVOICE_TYPE        "type"
#define INVOICE_TYPE_STRING "type_string"
#define INVOICE_BILLTO      "bill-to"
#define INVOICE_ENTRIES     "list_of_entries"
#define INVOICE_JOB         "invoice_job"

#define INVOICE_FROM_LOT    "invoice-from-lot"
#define INVOICE_FROM_TXN    "invoice-from-txn"

QofBook *gncInvoiceGetBook(GncInvoice *x);

/** deprecated functions */
#define gncInvoiceGetGUID(x) qof_instance_get_guid(QOF_INSTANCE(x))
#define gncInvoiceRetGUID(x) (x ? *(qof_instance_get_guid(QOF_INSTANCE(x))) : *(guid_null()))
#define gncInvoiceLookupDirect(G,B) gncInvoiceLookup((B),&(G))

/** Test support function used by test-dbi-business-stuff.c */
gboolean gncInvoiceEqual(const GncInvoice *a, const GncInvoice *b);

#endif /* GNC_INVOICE_H_ */
/** @} */
/** @} */
