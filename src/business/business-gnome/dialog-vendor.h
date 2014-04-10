/*
 * dialog-vendor.h -- Dialog(s) for Vendor search and entry
 * Copyright (C) 2001 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */


#ifndef GNC_DIALOG_VENDOR_H_
#define GNC_DIALOG_VENDOR_H_

typedef struct _vendor_window VendorWindow;

#include "gncVendor.h"
#include "dialog-search.h"

/* Create or Edit Vendors */
VendorWindow * gnc_ui_vendor_edit (GncVendor *vendor);
VendorWindow * gnc_ui_vendor_new (GNCBook *book);

/* Search for vendors */
GNCSearchWindow * gnc_vendor_search (GncVendor *start, GNCBook *book);

/*
 * These callbacks are for use with the gnc_general_search widget
 *
 * select() provides a Select Dialog and returns it.
 * edit() opens the existing vendor for editing and returns NULL.
 */
GNCSearchWindow * gnc_vendor_search_select (gpointer start, gpointer book);
GNCSearchWindow * gnc_vendor_search_edit (gpointer start, gpointer book);

#endif /* GNC_DIALOG_VENDOR_H_ */
