/*
 * gncEntry.h -- the Core Business Entry Interface
 * Copyright (C) 2001,2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef GNC_ENTRY_H_
#define GNC_ENTRY_H_

typedef struct _gncEntry GncEntry;

#include "date.h"
#include "gnc-book.h"
#include "gncTaxTable.h"
#include "gncOrder.h"
#include "gncInvoice.h"

#define GNC_ENTRY_MODULE_NAME "gncEntry"


/* How to apply the discount and taxes.  There are three distinct ways to
 * apply them:
 *
 * Type:	discount	tax
 * PRETAX	pretax		pretax-discount
 * SAMETIME	pretax		pretax
 * POSTTAX	pretax+tax	pretax
 */
typedef enum {
  GNC_DISC_PRETAX = 1,
  GNC_DISC_SAMETIME,
  GNC_DISC_POSTTAX
} GncDiscountHow;

const char * gncEntryDiscountHowToString (GncDiscountHow how);
gboolean gncEntryDiscountStringToHow (const char *str, GncDiscountHow *how);

/* Create/Destroy Functions */

GncEntry *gncEntryCreate (GNCBook *book);
void gncEntryDestroy (GncEntry *entry);

/* SET FUNCTIONS */

/* Generic (shared) data */
void gncEntrySetDate (GncEntry *entry, Timespec date);
void gncEntrySetDateEntered (GncEntry *entry, Timespec date);
void gncEntrySetDescription (GncEntry *entry, const char *desc);
void gncEntrySetAction (GncEntry *entry, const char *action);
void gncEntrySetNotes (GncEntry *entry, const char *notes);
void gncEntrySetQuantity (GncEntry *entry, gnc_numeric quantity);

/* Customer Invoices */
void gncEntrySetInvAccount (GncEntry *entry, Account *acc);
void gncEntrySetInvPrice (GncEntry *entry, gnc_numeric price);
void gncEntrySetInvTaxable (GncEntry *entry, gboolean taxable);
void gncEntrySetInvTaxIncluded (GncEntry *entry, gboolean tax_included);
void gncEntrySetInvTaxTable (GncEntry *entry, GncTaxTable *table);
void gncEntrySetInvDiscount (GncEntry *entry, gnc_numeric discount);
void gncEntrySetInvDiscountType (GncEntry *entry, GncAmountType type);
void gncEntrySetInvDiscountHow (GncEntry *entry, GncDiscountHow how);

/* Vendor Bills (and Employee Expenses) */
void gncEntrySetBillAccount (GncEntry *entry, Account *acc);
void gncEntrySetBillPrice (GncEntry *entry, gnc_numeric price);
void gncEntrySetBillTaxable (GncEntry *entry, gboolean taxable);
void gncEntrySetBillTaxIncluded (GncEntry *entry, gboolean tax_included);
void gncEntrySetBillTaxTable (GncEntry *entry, GncTaxTable *table);
void gncEntrySetBillable (GncEntry *entry, gboolean billable);
void gncEntrySetBillTo (GncEntry *entry, GncOwner *billto);

/* GET FUNCTIONS */
/* Generic (shared) data */
GNCBook * gncEntryGetBook (GncEntry *entry);
const GUID * gncEntryGetGUID (GncEntry *entry);
Timespec gncEntryGetDate (GncEntry *entry);
Timespec gncEntryGetDateEntered (GncEntry *entry);
const char * gncEntryGetDescription (GncEntry *entry);
const char * gncEntryGetAction (GncEntry *entry);
const char * gncEntryGetNotes (GncEntry *notes);
gnc_numeric gncEntryGetQuantity (GncEntry *entry);

/* Customer Invoices */
Account * gncEntryGetInvAccount (GncEntry *entry);
gnc_numeric gncEntryGetInvPrice (GncEntry *entry);
gnc_numeric gncEntryGetInvDiscount (GncEntry *entry);
GncAmountType gncEntryGetInvDiscountType (GncEntry *entry);
GncDiscountHow gncEntryGetInvDiscountHow (GncEntry *entry);
gboolean gncEntryGetInvTaxable (GncEntry *entry);
gboolean gncEntryGetInvTaxIncluded (GncEntry *entry);
GncTaxTable * gncEntryGetInvTaxTable (GncEntry *entry);

/* Vendor Bills (and Employee Expenses) */
Account * gncEntryGetBillAccount (GncEntry *entry);
gnc_numeric gncEntryGetBillPrice (GncEntry *entry);
gboolean gncEntryGetBillTaxable (GncEntry *entry);
gboolean gncEntryGetBillTaxIncluded (GncEntry *entry);
GncTaxTable * gncEntryGetBillTaxTable (GncEntry *entry);
gboolean gncEntryGetBillable (GncEntry *entry);
GncOwner *gncEntryGetBillTo (GncEntry *entry);


void gncEntryCopy (const GncEntry *src, GncEntry *dest);

/* The first three return the rounded values -- the last returns the
 * list of unrounded account-values.  The list belongs to the entry
 * and will be destroyed, so use it quickly.
 */
gnc_numeric gncEntryReturnValue (GncEntry *entry, gboolean is_inv);
gnc_numeric gncEntryReturnDiscountValue (GncEntry *entry, gboolean is_inv);
gnc_numeric gncEntryReturnTaxValue (GncEntry *entry, gboolean is_inv);
GList * gncEntryReturnTaxValues (GncEntry *entry, gboolean is_inv);

/* Compute the Entry value, tax-value, and discount_value, based on
 * the quantity, price, discount, tax-table, and types.  The value is
 * the amount the merchant gets, the taxes are what the gov't gets,
 * and the discount is how much the customer saved.
 *
 * The tax_values list is the property of the entry and will be
 * destroyed automatically, so use it quickly.  Note that all return
 * values from these two functions are NOT rounded.
 */
void gncEntryGetValue (GncEntry *entry, gboolean is_inv, gnc_numeric *value,
		       gnc_numeric *discount, gnc_numeric *tax_value,
		       GList **tax_values);

void gncEntryComputeValue (gnc_numeric qty, gnc_numeric price,
			   GncTaxTable *tax_table, gboolean tax_included,
			   gnc_numeric discount, GncAmountType discount_type,
			   GncDiscountHow discount_how,
			   /* return values */
			   gnc_numeric *value, gnc_numeric *discount_value,
			   GList **tax_values);

GncOrder * gncEntryGetOrder (GncEntry *entry);
GncInvoice * gncEntryGetInvoice (GncEntry *entry);
GncInvoice * gncEntryGetBill (GncEntry *entry);

GncEntry * gncEntryLookup (GNCBook *book, const GUID *guid);

gboolean gncEntryIsOpen (GncEntry *entry);
void gncEntryBeginEdit (GncEntry *entry);
void gncEntryCommitEdit (GncEntry *entry);
int gncEntryCompare (GncEntry *a, GncEntry *b);

#define ENTRY_DATE	"date"
#define ENTRY_DATE_ENTERED "date-entered"
#define ENTRY_DESC	"desc"
#define ENTRY_ACTION	"action"
#define ENTRY_NOTES	"notes"
#define ENTRY_QTY	"qty"

#define ENTRY_IPRICE	"iprice"
#define ENTRY_BPRICE	"bprice"
#define ENTRY_BILLABLE	"billable?"
#define ENTRY_BILLTO	"bill-to"

#define ENTRY_ORDER	"order"
#define ENTRY_INVOICE	"invoice"
#define ENTRY_BILL	"bill"

#endif /* GNC_ENTRY_H_ */
