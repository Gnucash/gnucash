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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
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

#include "gncAddress.h"
#include "gncBillTerm.h"
#include "gncTaxTable.h"
#include "gncJob.h"
#include "qofbook.h"
#include "qofid.h"
#include "qofinstance.h"

#define GNC_ID_VENDOR       "gncVendor"
#define GNC_IS_VENDOR(obj)  (QOF_CHECK_TYPE((obj), GNC_ID_VENDOR))
#define GNC_VENDOR(obj)     (QOF_CHECK_CAST((obj), GNC_ID_VENDOR, GncVendor))

/* Create/Destroy Functions */

GncVendor *gncVendorCreate (QofBook *book);
void gncVendorDestroy (GncVendor *vendor);

/* Set Functions */

void gncVendorSetID (GncVendor *vendor, const char *id);
void gncVendorSetName (GncVendor *vendor, const char *name);
void gncVendorSetNotes (GncVendor *vendor, const char *notes);
void gncVendorSetTerms (GncVendor *vendor, GncBillTerm *terms);
void gncVendorSetTaxIncluded (GncVendor *vendor, GncTaxIncluded taxincl);
void gncVendorSetCurrency (GncVendor *vendor, gnc_commodity *currency);
void gncVendorSetActive (GncVendor *vendor, gboolean active);

void gncVendorSetTaxTableOverride (GncVendor *vendor, gboolean override);
void gncVendorSetTaxTable (GncVendor *vendor, GncTaxTable *table);

void gncVendorAddJob (GncVendor *vendor, GncJob *job);
void gncVendorRemoveJob (GncVendor *vendor, GncJob *job);

void gncVendorBeginEdit (GncVendor *vendor);
void gncVendorCommitEdit (GncVendor *vendor);

/* Get Functions */

const char * gncVendorGetID (GncVendor *vendor);
const char * gncVendorGetName (GncVendor *vendor);
GncAddress * gncVendorGetAddr (GncVendor *vendor);
const char * gncVendorGetNotes (GncVendor *vendor);
GncBillTerm * gncVendorGetTerms (GncVendor *vendor);
GncTaxIncluded gncVendorGetTaxIncluded (GncVendor *vendor);
gnc_commodity * gncVendorGetCurrency (GncVendor *vendor);
gboolean gncVendorGetActive (GncVendor *vendor);

gboolean gncVendorGetTaxTableOverride (GncVendor *vendor);
GncTaxTable* gncVendorGetTaxTable (GncVendor *vendor);

/** XXX should be renamed to RetJobList to be consistent with
 * other usage, since caller must free the copied list 
 */
GList * gncVendorGetJoblist (GncVendor *vendor, gboolean show_all);
gboolean gncVendorIsDirty (GncVendor *vendor);
int gncVendorCompare (GncVendor *a, GncVendor *b);

/** Return a pointer to the instance gncVendor that is identified
 *  by the guid, and is residing in the book. Returns NULL if the 
 *  instance can't be found.
 *  Equivalent function prototype is
 *  GncVendor * gncVendorLookup (QofBook *book, const GUID *guid);
 */
#define gncVendorLookup(book,guid)    \
       QOF_BOOK_LOOKUP_ENTITY((book),(guid),GNC_ID_VENDOR, GncVendor)

#define VENDOR_ID	"id"
#define VENDOR_NAME	"name"
#define VENDOR_ADDR	"addr"

/** deprecated functions */
#define gncVendorGetBook(X) qof_instance_get_book (QOF_INSTANCE(X))
#define gncVendorGetGUID(X) qof_instance_get_guid (QOF_INSTANCE(X))
#define gncVendorRetGUID(X) (X ? *(qof_instance_get_guid (QOF_INSTANCE(X))) : *(guid_null()))
#define gncVendorLookupDirect(G,B) gncVendorLookup((B),&(G))

#endif /* GNC_VENDOR_H_ */
/** @} */
/** @} */
