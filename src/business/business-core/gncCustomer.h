/********************************************************************\
 * gncCustomer.h -- the Core Customer Interface                     *
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
/** @addtogroup Customer
    @{ */
/** @file gncCustomer.h
    @brief Core Customer Interface
    @author Copyright (C) 2001,2002 Derek Atkins <warlord@MIT.EDU>
*/

#ifndef GNC_CUSTOMER_H_
#define GNC_CUSTOMER_H_

/** @struct GncCustomer

credit, discount and shipaddr are unique to GncCustomer\n
id, name, notes, terms, addr, currency, taxtable, taxtable_override
taxincluded, active and jobs are identical to ::GncVendor.

@param	QofInstance     inst;
@param	char *          id;
@param	char *          name;
@param	char *          notes;
@param	GncBillTerm *   terms;
@param	GncAddress *    addr;
@param	gnc_commodity * currency;
@param	GncTaxTable*    taxtable;
@param	gboolean        taxtable_override;
@param	GncTaxIncluded  taxincluded;
@param	gboolean        active;
@param	GList *         jobs;
@param	gnc_numeric     credit;
@param	gnc_numeric     discount;
@param	GncAddress *    shipaddr;

*/
typedef struct _gncCustomer GncCustomer;
typedef struct _gncCustomerClass GncCustomerClass;

#include "gncAddress.h"
#include "gncBillTerm.h"
#include "gncTaxTable.h"
#include "gncJob.h"

#define GNC_ID_CUSTOMER       "gncCustomer"

/* --- type macros --- */
#define GNC_TYPE_CUSTOMER            (gnc_customer_get_type ())
#define GNC_CUSTOMER(o)              \
     (G_TYPE_CHECK_INSTANCE_CAST ((o), GNC_TYPE_CUSTOMER, GncCustomer))
#define GNC_CUSTOMER_CLASS(k)        \
     (G_TYPE_CHECK_CLASS_CAST((k), GNC_TYPE_CUSTOMER, GncCustomerClass))
#define GNC_IS_CUSTOMER(o)           \
     (G_TYPE_CHECK_INSTANCE_TYPE ((o), GNC_TYPE_CUSTOMER))
#define GNC_IS_CUSTOMER_CLASS(k)     \
     (G_TYPE_CHECK_CLASS_TYPE ((k), GNC_TYPE_CUSTOMER))
#define GNC_CUSTOMER_GET_CLASS(o)    \
     (G_TYPE_INSTANCE_GET_CLASS ((o), GNC_TYPE_CUSTOMER, GncCustomerClass))
GType gnc_customer_get_type(void);

/** @name Create/Destroy Functions
 @{ */
GncCustomer *gncCustomerCreate (QofBook *book);
void gncCustomerDestroy (GncCustomer *customer);
void gncCustomerBeginEdit (GncCustomer *customer);
void gncCustomerCommitEdit (GncCustomer *customer);
/** @} */

/** @name Set Functions
 @{ */

void gncCustomerSetID (GncCustomer *customer, const char *id);
void gncCustomerSetName (GncCustomer *customer, const char *name);
void gncCustomerSetNotes (GncCustomer *customer, const char *notes);
void gncCustomerSetTerms (GncCustomer *customer, GncBillTerm *term);
void gncCustomerSetTaxIncluded (GncCustomer *customer, GncTaxIncluded taxincl);
void gncCustomerSetActive (GncCustomer *customer, gboolean active);
void gncCustomerSetDiscount (GncCustomer *customer, gnc_numeric discount);
void gncCustomerSetCredit (GncCustomer *customer, gnc_numeric credit);
void gncCustomerSetCurrency (GncCustomer *customer, gnc_commodity *currency);

void gncCustomerSetTaxTableOverride (GncCustomer *customer, gboolean override);
void gncCustomerSetTaxTable (GncCustomer *customer, GncTaxTable *table);

void gncCustomerAddJob (GncCustomer *customer, GncJob *job);
void gncCustomerRemoveJob (GncCustomer *customer, GncJob *job);

/** @} */

/** @name Get Functions
 @{ */
/** Return a pointer to the instance gncCustomer that is identified
 *  by the guid, and is residing in the book. Returns NULL if the
 *  instance can't be found.
 *  Equivalent function prototype is
 *  GncCustomer * gncCustomerLookup (QofBook *book, const GUID *guid);
 */
#define gncCustomerLookup(book,guid)    \
       QOF_BOOK_LOOKUP_ENTITY((book),(guid),GNC_ID_CUSTOMER, GncCustomer)

const char * gncCustomerGetID (const GncCustomer *customer);
const char * gncCustomerGetName (const GncCustomer *customer);
GncAddress * gncCustomerGetAddr (const GncCustomer *customer);
GncAddress * gncCustomerGetShipAddr (const GncCustomer *customer);
const char * gncCustomerGetNotes (const GncCustomer *customer);
GncBillTerm * gncCustomerGetTerms (const GncCustomer *customer);
GncTaxIncluded gncCustomerGetTaxIncluded (const GncCustomer *customer);
gboolean gncCustomerGetActive (const GncCustomer *customer);
gnc_numeric gncCustomerGetDiscount (const GncCustomer *customer);
gnc_numeric gncCustomerGetCredit (const GncCustomer *customer);
gnc_commodity * gncCustomerGetCurrency (const GncCustomer *customer);

gboolean gncCustomerGetTaxTableOverride (const GncCustomer *customer);
GncTaxTable* gncCustomerGetTaxTable (const GncCustomer *customer);

GList * gncCustomerGetJoblist (const GncCustomer *customer, gboolean show_all);
/** @} */

gboolean gncCustomerIsDirty (GncCustomer *customer);
int gncCustomerCompare (const GncCustomer *a, const GncCustomer *b);

#define CUSTOMER_ID			"id"
#define CUSTOMER_NAME		"name"
#define CUSTOMER_ADDR		"addr"
#define CUSTOMER_SHIPADDR	"shipaddr"
#define CUSTOMER_NOTES 		"notes"
#define CUSTOMER_DISCOUNT 	"amount of discount"
#define CUSTOMER_CREDIT 	"amount of credit"
#define CUSTOMER_TT_OVER 	"tax table override"
#define CUSTOMER_TAX_INC    "customer_tax_included"
#define CUSTOMER_TERMS      "customer_terms"
#define CUSTOMER_ACTIVE     "customer_is_active"
#define CUSTOMER_SLOTS      "customer_values"

/** @deprecated functions, should be removed */
#define gncCustomerGetGUID(x) qof_instance_get_guid(QOF_INSTANCE(x))
#define gncCustomerRetGUID(x) (x ? *(qof_instance_get_guid(QOF_INSTANCE(x))) : *(guid_null()))
#define gncCustomerGetBook(x) qof_instance_get_book(QOF_INSTANCE(x))
#define gncCustomerLookupDirect(g,b) gncCustomerLookup((b), &(g))

#endif /* GNC_CUSTOMER_H_ */
/** @} */
/** @} */
