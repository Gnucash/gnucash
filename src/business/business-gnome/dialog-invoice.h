/*
 * dialog-invoice.h -- Dialog(s) for Invoice search and entry
 * Copyright (C) 2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */


#ifndef GNC_DIALOG_INVOICE_H_
#define GNC_DIALOG_INVOICE_H_

/* Functions to create and edit invoices */
GncInvoice * gnc_invoice_new (GtkWidget *parent, GncOwner *owner, GNCBook *book);
void gnc_invoice_edit (GtkWidget *parent, GncInvoice *invoice);

/* Callbacks to select a invoice that match the necessary functions
 * for use with the gnc_general_select widget.
 *
 * new_select provides a selection and the ability to create and edit
 *	invoices.
 * new_edit provides only the ability to edit the current selection
 */
gpointer        gnc_invoice_edit_new_select (gpointer book, gpointer c,
					      GtkWidget *toplevel);
gpointer	gnc_invoice_edit_new_edit (gpointer book, gpointer invoice,
					    GtkWidget *toplevel);

#endif /* GNC_DIALOG_INVOICE_H_ */
