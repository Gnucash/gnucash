/*
 * gncVendor.h -- the Core Vendor Interface
 * Copyright (C) 2001, 2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef GNC_VENDOR_H_
#define GNC_VENDOR_H_

typedef struct _gncVendor GncVendor;

#include "gnc-book.h"
#include "gncAddress.h"
#include "gncBillTerm.h"
#include "gncTaxTable.h"
#include "gncJob.h"

#define GNC_VENDOR_MODULE_NAME "gncVendor"

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

GUID gncVendorRetGUID (GncVendor *vendor);
GncVendor * gncVendorLookupDirect (GUID guid, QofBook *book);

GncVendor * gncVendorLookup (QofBook *book, const GUID *guid);
gboolean gncVendorIsDirty (GncVendor *vendor);
int gncVendorCompare (GncVendor *a, GncVendor *b);

#define VENDOR_ID	"id"
#define VENDOR_NAME	"name"
#define VENDOR_ADDR	"addr"

/** deprecated functions */
#define gncVendorGetBook(X) qof_instance_get_book (QOF_INSTANCE(X))
#define gncVendorGetGUID(X) qof_instance_get_guid (QOF_INSTANCE(X))

#endif /* GNC_VENDOR_H_ */
