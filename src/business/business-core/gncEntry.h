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

/* Set Functions */

void gncEntrySetDate (GncEntry *entry, Timespec date);
void gncEntrySetDateEntered (GncEntry *entry, Timespec date);
void gncEntrySetDescription (GncEntry *entry, const char *desc);
void gncEntrySetAction (GncEntry *entry, const char *action);
void gncEntrySetNotes (GncEntry *entry, const char *notes);
void gncEntrySetQuantity (GncEntry *entry, gnc_numeric quantity);
void gncEntrySetPrice (GncEntry *entry, gnc_numeric price);
void gncEntrySetDiscount (GncEntry *entry, gnc_numeric discount);
void gncEntrySetDiscountType (GncEntry *entry, GncAmountType type);
void gncEntrySetDiscountHow (GncEntry *entry, GncDiscountHow how);
void gncEntrySetTaxable (GncEntry *entry, gboolean taxable);
void gncEntrySetTaxIncluded (GncEntry *entry, gboolean tax_included);
void gncEntrySetTaxTable (GncEntry *entry, GncTaxTable *table);

void gncEntrySetAccount (GncEntry *entry, Account *acc);

/* Get Functions */

GNCBook * gncEntryGetBook (GncEntry *entry);
const GUID * gncEntryGetGUID (GncEntry *entry);
Timespec gncEntryGetDate (GncEntry *entry);
Timespec gncEntryGetDateEntered (GncEntry *entry);
const char * gncEntryGetDescription (GncEntry *entry);
const char * gncEntryGetAction (GncEntry *entry);
const char * gncEntryGetNotes (GncEntry *notes);
gnc_numeric gncEntryGetQuantity (GncEntry *entry);
gnc_numeric gncEntryGetPrice (GncEntry *entry);
gnc_numeric gncEntryGetDiscount (GncEntry *entry);
GncAmountType gncEntryGetDiscountType (GncEntry *entry);
GncDiscountHow gncEntryGetDiscountHow (GncEntry *entry);
gboolean gncEntryGetTaxable (GncEntry *entry);
gboolean gncEntryGetTaxIncluded (GncEntry *entry);
GncTaxTable * gncEntryGetTaxTable (GncEntry *entry);

void gncEntryCopy (const GncEntry *src, GncEntry *dest);

/* The first three return the rounded values -- the last returns the
 * list of unrounded account-values.  The list belongs to the entry
 * and will be destroyed, so use it quickly.
 */
gnc_numeric gncEntryReturnValue (GncEntry *entry);
gnc_numeric gncEntryReturnDiscountValue (GncEntry *entry);
gnc_numeric gncEntryReturnTaxValue (GncEntry *entry);
GList * gncEntryReturnTaxValues (GncEntry *entry);

/* Compute the Entry value, tax-value, and discount_value, based on
 * the quantity, price, discount, tax-table, and types.  The value is
 * the amount the merchant gets, the taxes are what the gov't gets,
 * and the discount is how much the customer saved.
 *
 * The tax_values list is the property of the entry and will be
 * destroyed automatically, so use it quickly.  Note that all return
 * values from these two functions are NOT rounded.
 */
void gncEntryGetValue (GncEntry *entry, gnc_numeric *value,
		       gnc_numeric *discount, gnc_numeric *tax_value,
		       GList **tax_values);
void gncEntryComputeValue (gnc_numeric qty, gnc_numeric price,
			   GncTaxTable *tax_table, gboolean tax_included,
			   gnc_numeric discount, GncAmountType discount_type,
			   GncDiscountHow discount_how,
			   /* return values */
			   gnc_numeric *value, gnc_numeric *discount_value,
			   GList **tax_values);

Account * gncEntryGetAccount (GncEntry *entry);

GncOrder * gncEntryGetOrder (GncEntry *entry);
GncInvoice * gncEntryGetInvoice (GncEntry *entry);

GncEntry * gncEntryLookup (GNCBook *book, const GUID *guid);

void gncEntryCommitEdit (GncEntry *entry);
int gncEntryCompare (GncEntry *a, GncEntry *b);

#define ENTRY_DATE	"date"
#define ENTRY_DESC	"desc"
#define ENTRY_ACTION	"action"
#define ENTRY_NOTES	"notes"
#define ENTRY_QTY	"qty"
#define ENTRY_PRICE	"price"

#define ENTRY_ORDER	"order"
#define ENTRY_INVOICE	"invoice"

#endif /* GNC_ENTRY_H_ */
