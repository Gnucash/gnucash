/********************************************************************\
 * gncVendor.h -- the Core Vendor Interface                         *
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
/** @addtogroup Vendor
    @{ */
/** @file gncVendor.h
    @brief  Vendor Interface
    @author Copyright (C) 2001,2002 Derek Atkins <warlord@MIT.EDU>
*/

#ifndef GNC_VENDOR_H_
#define GNC_VENDOR_H_

typedef struct _gncVendor GncVendor;
typedef struct _gncVendorClass GncVendorClass;

#include "gncAddress.h"
#include "gncBillTerm.h"
#include "gncTaxTable.h"
#include "gncJob.h"

#define GNC_ID_VENDOR       "gncVendor"

/* --- type macros --- */
#define GNC_TYPE_VENDOR            (gnc_vendor_get_type ())
#define GNC_VENDOR(o)              \
     (G_TYPE_CHECK_INSTANCE_CAST ((o), GNC_TYPE_VENDOR, GncVendor))
#define GNC_VENDOR_CLASS(k)        \
     (G_TYPE_CHECK_CLASS_CAST((k), GNC_TYPE_VENDOR, GncVendorClass))
#define GNC_IS_VENDOR(o)           \
     (G_TYPE_CHECK_INSTANCE_TYPE ((o), GNC_TYPE_VENDOR))
#define GNC_IS_VENDOR_CLASS(k)     \
     (G_TYPE_CHECK_CLASS_TYPE ((k), GNC_TYPE_VENDOR))
#define GNC_VENDOR_GET_CLASS(o)    \
     (G_TYPE_INSTANCE_GET_CLASS ((o), GNC_TYPE_VENDOR, GncVendorClass))
GType gnc_vendor_get_type(void);

/* Create/Destroy Functions */

GncVendor *gncVendorCreate (QofBook *book);
void gncVendorDestroy (GncVendor *vendor);

/** @name Set Functions
 @{
*/

void gncVendorSetID (GncVendor *vendor, const char *id);
void gncVendorSetName (GncVendor *vendor, const char *name);
void gncVendorSetNotes (GncVendor *vendor, const char *notes);
void gncVendorSetTerms (GncVendor *vendor, GncBillTerm *terms);
void gncVendorSetTaxIncluded (GncVendor *vendor, GncTaxIncluded taxincl);
void gncVendorSetCurrency (GncVendor *vendor, gnc_commodity *currency);
void gncVendorSetActive (GncVendor *vendor, gboolean active);
void gncVendorSetTaxTableOverride (GncVendor *vendor, gboolean override);
void gncVendorSetTaxTable (GncVendor *vendor, GncTaxTable *table);

/** @} */

void gncVendorAddJob (GncVendor *vendor, GncJob *job);
void gncVendorRemoveJob (GncVendor *vendor, GncJob *job);

void gncVendorBeginEdit (GncVendor *vendor);
void gncVendorCommitEdit (GncVendor *vendor);

/** @name Get Functions
@{
*/

const char * gncVendorGetID (const GncVendor *vendor);
const char * gncVendorGetName (const GncVendor *vendor);
GncAddress * gncVendorGetAddr (const GncVendor *vendor);
const char * gncVendorGetNotes (const GncVendor *vendor);
GncBillTerm * gncVendorGetTerms (const GncVendor *vendor);
GncTaxIncluded gncVendorGetTaxIncluded (const GncVendor *vendor);
gnc_commodity * gncVendorGetCurrency (const GncVendor *vendor);
gboolean gncVendorGetActive (const GncVendor *vendor);

gboolean gncVendorGetTaxTableOverride (const GncVendor *vendor);
GncTaxTable* gncVendorGetTaxTable (const GncVendor *vendor);

/** @} */
/** XXX should be renamed to RetJobList to be consistent with
 * other usage, since caller must free the copied list
 */
GList * gncVendorGetJoblist (const GncVendor *vendor, gboolean show_all);
gboolean gncVendorIsDirty (const GncVendor *vendor);
int gncVendorCompare (const GncVendor *a, const GncVendor *b);

/** Return a pointer to the instance gncVendor that is identified
 *  by the guid, and is residing in the book. Returns NULL if the
 *  instance can't be found.
 *  Equivalent function prototype is
 *  GncVendor * gncVendorLookup (QofBook *book, const GUID *guid);
 */
static inline GncVendor * gncVendorLookup (const QofBook *book, const GUID *guid)
{
    QOF_BOOK_RETURN_ENTITY(book, guid, GNC_ID_VENDOR, GncVendor);
}

#define VENDOR_ID	"id"
#define VENDOR_NAME	"name"
#define VENDOR_ADDR	"addr"
#define VENDOR_NOTES "vendor_notes"
#define VENDOR_TERMS "vendor_terms"
#define VENDOR_TAX_INC "vendor_tax_included"
#define VENDOR_ACTIVE "vendor_is_active"
#define VENDOR_TAX_OVERRIDE "override_tax_table"
#define VENDOR_TAX_TABLE "vendor_tax_table"

/** deprecated functions */
#define gncVendorGetBook(X) qof_instance_get_book (QOF_INSTANCE(X))
#define gncVendorGetGUID(X) qof_instance_get_guid (QOF_INSTANCE(X))
#define gncVendorRetGUID(X) (X ? *(qof_instance_get_guid (QOF_INSTANCE(X))) : *(guid_null()))
#define gncVendorLookupDirect(G,B) gncVendorLookup((B),&(G))

#endif /* GNC_VENDOR_H_ */
/** @} */
/** @} */
