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
#include "gncJob.h"

#define GNC_VENDOR_MODULE_NAME "gncVendor"

/* Create/Destroy Functions */

GncVendor *gncVendorCreate (GNCBook *book);
void gncVendorDestroy (GncVendor *vendor);

/* Set Functions */

void gncVendorSetID (GncVendor *vendor, const char *id);
void gncVendorSetName (GncVendor *vendor, const char *name);
void gncVendorSetNotes (GncVendor *vendor, const char *notes);
void gncVendorSetTerms (GncVendor *vendor, const char *terms);
void gncVendorSetTaxIncluded (GncVendor *vendor, gboolean taxincl);
void gncVendorSetCommodity (GncVendor *vendor, gnc_commodity *com);
void gncVendorSetActive (GncVendor *vendor, gboolean active);

void gncVendorAddJob (GncVendor *vendor, GncJob *job);
void gncVendorRemoveJob (GncVendor *vendor, GncJob *job);

void gncVendorCommitEdit (GncVendor *vendor);

/* Get Functions */

GNCBook * gncVendorGetBook (GncVendor *vendor);
const GUID * gncVendorGetGUID (GncVendor *vendor);
const char * gncVendorGetID (GncVendor *vendor);
const char * gncVendorGetName (GncVendor *vendor);
GncAddress * gncVendorGetAddr (GncVendor *vendor);
const char * gncVendorGetNotes (GncVendor *vendor);
const char * gncVendorGetTerms (GncVendor *vendor);
gboolean gncVendorGetTaxIncluded (GncVendor *vendor);
gnc_commodity * gncVendorGetCommodity (GncVendor *vendor);
gboolean gncVendorGetActive (GncVendor *vendor);

GList * gncVendorGetJoblist (GncVendor *vendor, gboolean show_all);

GUID gncVendorRetGUID (GncVendor *vendor);
GncVendor * gncVendorLookupDirect (GUID guid, GNCBook *book);

GncVendor * gncVendorLookup (GNCBook *book, const GUID *guid);
gboolean gncVendorIsDirty (GncVendor *vendor);
int gncVendorCompare (GncVendor *a, GncVendor *b);

#define VENDOR_ID	"id"
#define VENDOR_NAME	"name"
#define VENDOR_ADDR	"addr"

#endif /* GNC_VENDOR_H_ */
