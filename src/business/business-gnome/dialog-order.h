/*
 * dialog-order.h -- Dialog(s) for Order search and entry
 * Copyright (C) 2001 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */


#ifndef GNC_DIALOG_ORDER_H_
#define GNC_DIALOG_ORDER_H_

/* Functions to create and edit orders */
GncOrder * gnc_order_new (GtkWidget *parent, GNCBook *book);
void gnc_order_edit (GtkWidget *parent, GncOrder *order);

/* Callbacks to select a order that match the necessary functions
 * for use with the gnc_general_select widget.
 *
 * new_select provides a selection and the ability to create and edit
 *	orders.
 * new_edit provides only the ability to edit the current selection
 */
gpointer        gnc_order_edit_new_select (gpointer book, gpointer c,
					      GtkWidget *toplevel);
gpointer	gnc_order_edit_new_edit (gpointer book, gpointer order,
					    GtkWidget *toplevel);

#endif /* GNC_DIALOG_ORDER_H_ */
