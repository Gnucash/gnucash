/*
 * dialog-customer.h -- Dialog(s) for Customer search and entry
 * Copyright (C) 2001 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */


#ifndef GNC_DIALOG_CUSTOMER_H_
#define GNC_DIALOG_CUSTOMER_H_

typedef struct _customer_window CustomerWindow;

#include "gncCustomer.h"

CustomerWindow * gnc_ui_customer_window_create (GncCustomer *cust);
void gnc_customer_find (GncCustomer *start, GNCBook *book);

/* Functions to create and return a customer */
GncCustomer * gnc_customer_new (GtkWidget *parent, GNCBook *book);

GncCustomer *gnc_customer_choose (GtkWidget *parent, GncCustomer *start,
				  GNCBook *book);

/* Callbacks to select a customer that match the necessary functions
 * for use with the gnc_general_select widget.
 *
 * new_select provides a selection and the ability to create and edit
 *	customers.
 * new_edit provides only the ability to edit the current selection
 */
gpointer        gnc_customer_edit_new_select (gpointer book, gpointer c,
					      GtkWidget *toplevel);
gpointer	gnc_customer_edit_new_edit (gpointer book, gpointer cust,
					    GtkWidget *toplevel);

#endif /* GNC_DIALOG_CUSTOMER_H_ */
