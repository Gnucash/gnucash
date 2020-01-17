/********************************************************************\
 * gncEntry.h -- the Core Business Entry Interface                  *
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
/** @addtogroup Entry
    @{ */
/** @file gncEntry.h
    @brief  Business Entry Interface
    @author Copyright (C) 2001,2002 Derek Atkins <warlord@MIT.EDU>
*/

#ifndef GNC_ENTRY_H_
#define GNC_ENTRY_H_

typedef struct _gncEntry GncEntry;
typedef struct _gncEntryClass GncEntryClass;

typedef enum
{
    GNC_PAYMENT_CASH = 1,
    GNC_PAYMENT_CARD
} GncEntryPaymentType;

typedef enum
{
    GNC_DISC_PRETAX = 1,
    GNC_DISC_SAMETIME,
    GNC_DISC_POSTTAX
} GncDiscountHow;

typedef GList AccountValueList;

#include "gncBusiness.h"
#include "gncInvoice.h"
#include "gncOrder.h"
#include "gncTaxTable.h"
#include "gncOwner.h"

#define GNC_ID_ENTRY "gncEntry"

/* --- type macros --- */
#define GNC_TYPE_ENTRY            (gnc_entry_get_type ())
#define GNC_ENTRY(o)              \
     (G_TYPE_CHECK_INSTANCE_CAST ((o), GNC_TYPE_ENTRY, GncEntry))
#define GNC_ENTRY_CLASS(k)        \
     (G_TYPE_CHECK_CLASS_CAST((k), GNC_TYPE_ENTRY, GncEntryClass))
#define GNC_IS_ENTRY(o)           \
     (G_TYPE_CHECK_INSTANCE_TYPE ((o), GNC_TYPE_ENTRY))
#define GNC_IS_ENTRY_CLASS(k)     \
     (G_TYPE_CHECK_CLASS_TYPE ((k), GNC_TYPE_ENTRY))
#define GNC_ENTRY_GET_CLASS(o)    \
     (G_TYPE_INSTANCE_GET_CLASS ((o), GNC_TYPE_ENTRY, GncEntryClass))
GType gnc_entry_get_type(void);

/** How to apply the discount and taxes.  There are three distinct ways to
 * apply them:
 *
 * Type:	discount	tax
 * PRETAX	pretax		pretax-discount
 * SAMETIME	pretax		pretax
 * POSTTAX	pretax+tax	pretax
 */

const char * gncEntryDiscountHowToString (GncDiscountHow how);
gboolean gncEntryDiscountStringToHow (const char *str, GncDiscountHow *how);

const char * gncEntryPaymentTypeToString (GncEntryPaymentType type);
gboolean gncEntryPaymentStringToType (const char *str, GncEntryPaymentType *type);

/** @name Create/Destroy Functions
 @{ */
GncEntry *gncEntryCreate (QofBook *book);
void gncEntryDestroy (GncEntry *entry);
/** @} */

/* SET FUNCTIONS */

/** @name Generic (shared) data
 @{ */
/** Set the date of this entry */
void gncEntrySetDateGDate (GncEntry *entry, const GDate* date);
/** DEPRECATED - use gncEntrySetDateGDate() instead! (Because the time-of-day
is a misleading extra information. We are only dealing with the day
information! */
void gncEntrySetDate (GncEntry *entry, time64 date);
void gncEntrySetDateEntered (GncEntry *entry, time64 date);
void gncEntrySetDescription (GncEntry *entry, const char *desc);
void gncEntrySetAction (GncEntry *entry, const char *action);
void gncEntrySetNotes (GncEntry *entry, const char *notes);
/** Set the internal quantity without any conversion.
 *  This distinction is made because credit notes store their quantity
 *  sign-reversed compared to how the quantity is written on the
 *  actual credit note (and hence how the ledger and reports show it
 *  to the user). */
void gncEntrySetQuantity (GncEntry *entry, gnc_numeric quantity);
/** Set the internal quantity converting from the quantity as
 *  visible on the physical document.
 *  This distinction is made because credit notes store their quantity
 *  sign-reversed compared to how the quantity is written on the
 *  actual credit note (and hence how the ledger and reports show it
 *  to the user). */
void gncEntrySetDocQuantity (GncEntry *entry, gnc_numeric quantity, gboolean is_cn);
/** @} */

/** @name Customer Invoices
 @{ */
void gncEntrySetInvAccount (GncEntry *entry, Account *acc);
void gncEntrySetInvPrice (GncEntry *entry, gnc_numeric price);
void gncEntrySetInvTaxable (GncEntry *entry, gboolean taxable);
void gncEntrySetInvTaxIncluded (GncEntry *entry, gboolean tax_included);
void gncEntrySetInvTaxTable (GncEntry *entry, GncTaxTable *table);
void gncEntrySetInvDiscount (GncEntry *entry, gnc_numeric discount);
void gncEntrySetInvDiscountType (GncEntry *entry, GncAmountType type);
void gncEntrySetInvDiscountHow (GncEntry *entry, GncDiscountHow how);
void qofEntrySetInvDiscType (GncEntry *entry, const char *type);
void qofEntrySetInvDiscHow  (GncEntry *entry, const char *type);
/** @} */

/** @name Vendor Bills (and Employee Expenses)
 @{ */
void gncEntrySetBillAccount (GncEntry *entry, Account *acc);
void gncEntrySetBillPrice (GncEntry *entry, gnc_numeric price);
void gncEntrySetBillTaxable (GncEntry *entry, gboolean taxable);
void gncEntrySetBillTaxIncluded (GncEntry *entry, gboolean tax_included);
void gncEntrySetBillTaxTable (GncEntry *entry, GncTaxTable *table);
void gncEntrySetBillable (GncEntry *entry, gboolean billable);
void gncEntrySetBillTo (GncEntry *entry, GncOwner *billto);
/** @} */

/** @name employee-stuff
 @{ */
void gncEntrySetBillPayment (GncEntry *entry, GncEntryPaymentType type);
/** @} */

/* GET FUNCTIONS */
/** @name Generic (shared) data
 @{ */
/** Returns the day of this entry */
GDate gncEntryGetDateGDate (const GncEntry *entry);
/** DEPRECATED - use gncEntryGetDateGDate() instead! (Because the time-of-day
is a misleading extra information. We are only dealing with the day
information! */
time64 gncEntryGetDate (const GncEntry *entry);
time64 gncEntryGetDateEntered (const GncEntry *entry);
const char * gncEntryGetDescription (const GncEntry *entry);
const char * gncEntryGetAction (const GncEntry *entry);
const char * gncEntryGetNotes (const GncEntry *notes);
/** Get the quantity as stored internally.
 *  This distinction is made because credit notes store their quantity
 *  sign-reversed compared to how the quantity is written on the
 *  actual credit note (and hence how the ledger and reports show it
 *  to the user). */
gnc_numeric gncEntryGetQuantity (const GncEntry *entry);
/** Get the quantity as on the physical document.
 *  This distinction is made because credit notes store their quantity
 *  sign-reversed compared to how the quantity is written on the
 *  actual credit note (and hence how the ledger and reports show it
 *  to the user). */
gnc_numeric gncEntryGetDocQuantity (const GncEntry *entry, gboolean is_cn);
/** @} */

/** @name Customer Invoices
 @{ */
Account * gncEntryGetInvAccount (const GncEntry *entry);
gnc_numeric gncEntryGetInvPrice (const GncEntry *entry);
gnc_numeric gncEntryGetPrice (const GncEntry *entry, const gboolean cust_doc, const gboolean net);
gnc_numeric gncEntryGetInvDiscount (const GncEntry *entry);
GncAmountType gncEntryGetInvDiscountType (const GncEntry *entry);
GncDiscountHow gncEntryGetInvDiscountHow (const GncEntry *entry);
char* qofEntryGetInvDiscType (const GncEntry *entry);
char* qofEntryGetInvDiscHow (const GncEntry *entry);
gboolean gncEntryGetInvTaxable (const GncEntry *entry);
gboolean gncEntryGetInvTaxIncluded (const GncEntry *entry);
GncTaxTable * gncEntryGetInvTaxTable (const GncEntry *entry);
/** @} */

/** @name Vendor Bills (and Employee Expenses)
 @{ */
Account * gncEntryGetBillAccount (const GncEntry *entry);
gnc_numeric gncEntryGetBillPrice (const GncEntry *entry);
gboolean gncEntryGetBillTaxable (const GncEntry *entry);
gboolean gncEntryGetBillTaxIncluded (const GncEntry *entry);
GncTaxTable * gncEntryGetBillTaxTable (const GncEntry *entry);
gboolean gncEntryGetBillable (const GncEntry *entry);
GncOwner *gncEntryGetBillTo (GncEntry *entry);

GncEntryPaymentType gncEntryGetBillPayment (const GncEntry* entry);
/** @} */

void gncEntryCopy (const GncEntry *src, GncEntry *dest, gboolean add_entry);

/** @name Getting Values
 *
 * An entry has three important values:
 * - entry value: the amount the merchant gets
 * - tax value: the amount the government gets
 * - discount value: the amount the customer saved
 *
 * These values can be retrieved in several variants. Depending on
 * how they will be used some sign reversals can be applied on
 * the values:
 * - Doc value: the value as listed on the document. This is usually
 *              a positive value, unless the document was a
 *              negative invoice/bill or negative credit note.
 *              Since credit note entry values are stored negatively
 *              internally, they will be sign-reversed before returning
 *              them.
 * - Bal value: the value as it will impact the balance. Customer
 *              invoices and vendor credit notes have a positive
 *              influence on the balance, so these values will be positive.
 *              For vendor bills and customer credit notes, the
 *              values will be negative.
 *
 * For tax there are TaxValue and TaxValues variants: the first one
 * returns to total tax value for this entry, meaning the sum of all
 * individual taxes. The second one returns a list of all the individual
 * tax values for this entry. This list holds unrounded values only, there's
 * no variant with rounded values.
 *
 * Note that this list is not owned by the entry. When no longer needed,
 * it should be freed with gncAccountValueDestroy.
 *
 * Finally, there are rounded and unrounded variants of most of
 * these functions.
 @{
*/
gnc_numeric gncEntryGetDocValue (GncEntry *entry, gboolean round, gboolean is_cust_doc, gboolean is_cn);
gnc_numeric gncEntryGetDocTaxValue (GncEntry *entry, gboolean round, gboolean is_cust_doc, gboolean is_cn);
/** Careful: the returned list is NOT owned by the entry and should be freed by the caller */
AccountValueList * gncEntryGetDocTaxValues (GncEntry *entry, gboolean is_cust_doc, gboolean is_cn);
gnc_numeric gncEntryGetDocDiscountValue (GncEntry *entry, gboolean round, gboolean is_cust_doc, gboolean is_cn);

gnc_numeric gncEntryGetBalValue (GncEntry *entry, gboolean round, gboolean is_cust_doc);
gnc_numeric gncEntryGetBalTaxValue (GncEntry *entry, gboolean round, gboolean is_cust_doc);
/** Careful: the returned list is NOT owned by the entry and should be freed by the caller */
AccountValueList * gncEntryGetBalTaxValues (GncEntry *entry, gboolean is_cust_doc);
gnc_numeric gncEntryGetBalDiscountValue (GncEntry *entry, gboolean round, gboolean is_cust_doc);

/** Compute the Entry value, tax_value, and discount_value, based on
 * the quantity, price, discount, tax_-table, and types.  The value is
 * the amount the merchant gets, the taxes are what the gov't gets,
 * and the discount is how much the customer saved.  The SCU is the
 * target denominator of the value and tax -- it should be the
 * account or commodity SCU of the target.
 *
 *  The return values are NOT rounded.
 *
 * The tax_values list is owned by the entry and will be
 * destroyed automatically, so use it quickly.
 */
void gncEntryComputeValue (gnc_numeric qty, gnc_numeric price,
                           const GncTaxTable *tax_table, gboolean tax_included,
                           gnc_numeric discount, GncAmountType discount_type,
                           GncDiscountHow discount_how, int SCU,
                           /* return values */
                           gnc_numeric *value, gnc_numeric *discount_value,
                           GList **tax_values);

/** @} */

GncOrder * gncEntryGetOrder (const GncEntry *entry);
GncInvoice * gncEntryGetInvoice (const GncEntry *entry);
GncInvoice * gncEntryGetBill (const GncEntry *entry);

/** Return a pointer to the instance gncEntry that is identified
 *  by the guid, and is residing in the book. Returns NULL if the
 *  instance can't be found.
 *  Equivalent function prototype is
 *  GncEntry * gncEntryLookup (QofBook *book, const GncGUID *guid);
 */
static inline GncEntry * gncEntryLookup (const QofBook *book, const GncGUID *guid)
{
    QOF_BOOK_RETURN_ENTITY(book, guid, GNC_ID_ENTRY, GncEntry);
}

gboolean gncEntryIsOpen (const GncEntry *entry);
void gncEntryBeginEdit (GncEntry *entry);
void gncEntryCommitEdit (GncEntry *entry);
int gncEntryCompare (const GncEntry *a, const GncEntry *b);

#define ENTRY_DATE			"date"
#define ENTRY_DATE_ENTERED 	"date-entered"
#define ENTRY_DESC			"desc"
#define ENTRY_ACTION		"action"
#define ENTRY_NOTES			"notes"
#define ENTRY_QTY			"qty"

#define ENTRY_IPRICE		"iprice"
#define ENTRY_IACCT			"invoice-account"
#define ENTRY_BACCT			"bill-account"
#define ENTRY_BPRICE		"bprice"
#define ENTRY_BILLABLE		"billable?"
#define ENTRY_BILLTO		"bill-to"

#define ENTRY_ORDER			"order"
#define ENTRY_INVOICE		"invoice"
#define ENTRY_BILL			"bill"

#define ENTRY_INV_DISC_TYPE		"discount-type"
#define ENTRY_INV_DISC_HOW		"discount-method"

#define ENTRY_INV_TAXABLE	"invoice-taxable"
#define ENTRY_BILL_TAXABLE	"bill-taxable"
#define ENTRY_INV_TAX_INC	"invoice-tax-included"
#define ENTRY_BILL_TAX_INC	"bill-tax-included"
#define ENTRY_INV_DISCOUNT	"invoice-discount"
#define ENTRY_BILL_PAY_TYPE "bill-payment-type"


/* deprecated functions, should be removed */
#define gncEntryGetGUID(x) qof_instance_get_guid(QOF_INSTANCE(x))

#endif /* GNC_ENTRY_H_ */
/** @} */
/** @} */
