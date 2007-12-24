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

typedef enum {
  GNC_PAYMENT_CASH = 1,
  GNC_PAYMENT_CARD
} GncEntryPaymentType;

typedef enum {
  GNC_DISC_PRETAX = 1,
  GNC_DISC_SAMETIME,
  GNC_DISC_POSTTAX
} GncDiscountHow;

#ifdef GNUCASH_MAJOR_VERSION
#include "gncBusiness.h"
#endif
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
void gncEntrySetDate (GncEntry *entry, Timespec date);
void gncEntrySetDateEntered (GncEntry *entry, Timespec date);
void gncEntrySetDescription (GncEntry *entry, const char *desc);
void gncEntrySetAction (GncEntry *entry, const char *action);
void gncEntrySetNotes (GncEntry *entry, const char *notes);
void gncEntrySetQuantity (GncEntry *entry, gnc_numeric quantity);
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
Timespec gncEntryGetDate (const GncEntry *entry);
Timespec gncEntryGetDateEntered (const GncEntry *entry);
const char * gncEntryGetDescription (const GncEntry *entry);
const char * gncEntryGetAction (const GncEntry *entry);
const char * gncEntryGetNotes (const GncEntry *notes);
gnc_numeric gncEntryGetQuantity (const GncEntry *entry);
/** @} */

/** @name Customer Invoices 
 @{ */
Account * gncEntryGetInvAccount (const GncEntry *entry);
gnc_numeric gncEntryGetInvPrice (const GncEntry *entry);
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

void gncEntryCopy (const GncEntry *src, GncEntry *dest);

/** @name Getting Values 

 * The first three return the rounded values -- the last returns the
 * list of unrounded account-values.  The list belongs to the entry
 * and will be destroyed, so use it quickly.
 @{
*/
gnc_numeric gncEntryReturnValue (GncEntry *entry, gboolean is_inv);
gnc_numeric gncEntryReturnDiscountValue (GncEntry *entry, gboolean is_inv);
gnc_numeric gncEntryReturnTaxValue (GncEntry *entry, gboolean is_inv);
typedef GList AccountValueList;
AccountValueList * gncEntryReturnTaxValues (GncEntry *entry, gboolean is_inv);

/** Compute the Entry value, tax-value, and discount_value, based on
 * the quantity, price, discount, tax-table, and types.  The value is
 * the amount the merchant gets, the taxes are what the gov't gets,
 * and the discount is how much the customer saved.  The SCU is the
 * target denominator of the value and tax -- it should be the
 * account or commodity SCU of the target.
 *
 * The tax_values list is the property of the entry and will be
 * destroyed automatically, so use it quickly.  Note that all return
 * values from these two functions are NOT rounded.
 */
void gncEntryGetValue (GncEntry *entry, gboolean is_inv, gnc_numeric *value,
		       gnc_numeric *discount, gnc_numeric *tax_value,
		       GList **tax_values);

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
 *  GncEntry * gncEntryLookup (QofBook *book, const GUID *guid);
 */
#define gncEntryLookup(book,guid)    \
       QOF_BOOK_LOOKUP_ENTITY((book),(guid),GNC_ID_ENTRY, GncEntry)

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
