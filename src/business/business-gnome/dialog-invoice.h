/*
 * dialog-invoice.h -- Dialog(s) for Invoice search and entry
 * Copyright (C) 2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */


#ifndef GNC_DIALOG_INVOICE_H_
#define GNC_DIALOG_INVOICE_H_

typedef struct _invoice_window InvoiceWindow;

#include "gncInvoice.h"
#include "gncOwner.h"

/* Create an invoice window */
InvoiceWindow * gnc_ui_invoice_window_create (GncInvoice *invoice);
InvoiceWindow * gnc_ui_invoice_edit (GncInvoice *invoice);

void gnc_invoice_find (GncInvoice *start, GncOwner *owner, GNCBook *book);

/* Functions to create and return an invoice */
GncInvoice * gnc_invoice_new (GtkWidget *parent, GncOwner *owner, GNCBook *book);

GncInvoice * gnc_invoice_choose (GtkWidget *parent, GncInvoice *start,
				 GncOwner *owner, GNCBook *book);

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
