/*
 * dialog-vendor.h -- Dialog(s) for Vendor search and entry
 * Copyright (C) 2001 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */


#ifndef GNC_DIALOG_VENDOR_H_
#define GNC_DIALOG_VENDOR_H_

/* Functions to create and edit vendors */
GncVendor * gnc_vendor_new (GtkWidget *parent, GNCBook *book);
void gnc_vendor_edit (GtkWidget *parent, GncVendor *vendor);

/* Callbacks to select a vendor that match the necessary functions
 * for use with the gnc_general_select widget.
 *
 * new_select provides a selection and the ability to create and edit
 *	vendors.
 * new_edit provides only the ability to edit the current selection
 */
gpointer        gnc_vendor_edit_new_select (gpointer book, gpointer c,
					      GtkWidget *toplevel);
gpointer	gnc_vendor_edit_new_edit (gpointer book, gpointer vendor,
					    GtkWidget *toplevel);

#endif /* GNC_DIALOG_VENDOR_H_ */
