/*
 * gncVendor.h -- the Core Vendor Interface
 * Copyright (C) 2001 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef GNC_VENDOR_H_
#define GNC_VENDOR_H_

typedef struct _gncVendor GncVendor;

#include "gnc-book.h"
#include "gncAddress.h"

#define GNC_VENDOR_MODULE_NAME "gncVendor"

/* Create/Destroy Functions */

GncVendor *gncVendorCreate (GNCBook *book);
void gncVendorDestroy (GncVendor *vendor);

/* Set Functions */

void gncVendorSetID (GncVendor *vendor, const char *id);
void gncVendorSetName (GncVendor *vendor, const char *name);
void gncVendorSetNotes (GncVendor *vendor, const char *notes);
void gncVendorSetTerms (GncVendor *vendor, gint terms);
void gncVendorSetTaxIncluded (GncVendor *vendor, gboolean taxincl);
void gncVendorSetActive (GncVendor *vendor, gboolean active);

void gncVendorCommitEdit (GncVendor *vendor);

/* Get Functions */

GNCBook * gncVendorGetBook (GncVendor *vendor);
const GUID * gncVendorGetGUID (GncVendor *vendor);
const char * gncVendorGetID (GncVendor *vendor);
const char * gncVendorGetName (GncVendor *vendor);
GncAddress * gncVendorGetAddr (GncVendor *vendor);
const char * gncVendorGetNotes (GncVendor *vendor);
gint gncVendorGetTerms (GncVendor *vendor);
gboolean gncVendorGetTaxIncluded (GncVendor *vendor);
gboolean gncVendorGetActive (GncVendor *vendor);

GncVendor * gncVendorLookup (GNCBook *book, const GUID *guid);
gboolean gncVendorIsDirty (GncVendor *vendor);

#endif /* GNC_VENDOR_H_ */
