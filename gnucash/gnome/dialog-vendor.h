/*
 * dialog-vendor.h -- Dialog(s) for Vendor search and entry
 * Copyright (C) 2001 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */


#ifndef GNC_DIALOG_VENDOR_H_
#define GNC_DIALOG_VENDOR_H_

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _vendor_window VendorWindow;

#include "gncVendor.h"
#include "dialog-search.h"

/* Create or Edit Vendors */
VendorWindow * gnc_ui_vendor_edit (GtkWindow *parent, GncVendor *vendor);
VendorWindow * gnc_ui_vendor_new (GtkWindow *parent, QofBook *book);

/* Search for vendors */
GNCSearchWindow * gnc_vendor_search (GtkWindow *parent, GncVendor *start, QofBook *book);

/*
 * These callbacks are for use with the gnc_general_search widget
 *
 * select() provides a Select Dialog and returns it.
 * edit() opens the existing vendor for editing and returns NULL.
 */
GNCSearchWindow * gnc_vendor_search_select (GtkWindow *parent, gpointer start, gpointer book);
GNCSearchWindow * gnc_vendor_search_edit (GtkWindow *parent, gpointer start, gpointer book);

#ifdef __cplusplus
}
#endif

#endif /* GNC_DIALOG_VENDOR_H_ */
