/*
 * business-chooser.h -- General Selection Dialog for GNC Business Objects
 *
 * Written By: Derek Atkins <warlord@MIT.EDU>
 * Copyright (C) 2001
 */

#ifndef GNC_BUSINESS_CHOOSER_H_
#define GNC_BUSINESS_CHOOSER_H_

/*
 * Generic function to choose business objects.  This function
 * is only called from within the business objects, but is made
 * available here so other objects can use it.  You provide the
 * original selection, the Object Type Name (as registered),
 * callbacks to call to create a new object or to edit an existing
 * one, and a callback argument that is passed to the callbacks.
 *
 * Note that if a callback function is NULL, the associated button
 * will still be clickable but will do nothing. XXXX -- this is a bug
 */

typedef gpointer (*gnc_business_chooser_new_cb)(gpointer cbarg, GtkWidget *toplevel);
typedef void (*gnc_business_chooser_edit_cb)(gpointer cbarg, gpointer sel_obj, GtkWidget *toplevel);

gpointer
gnc_ui_business_chooser_new (GtkWidget * parent,
			     gpointer orig_sel,
			     GncBusiness *business, const char *type_name,
			     gnc_business_chooser_new_cb new_cb,
			     gnc_business_chooser_edit_cb edit_cb,
			     gpointer cbarg);


#endif /* GNC_BUSINESS_CHOOSER_H_ */
