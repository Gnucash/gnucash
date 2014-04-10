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
#include "dialog-search.h"

/* Create and edit an invoice */
InvoiceWindow * gnc_ui_invoice_edit (GncInvoice *invoice);
InvoiceWindow * gnc_ui_invoice_new (GncOwner *owner, GNCBook *book);

/* Search for invoices */
GNCSearchWindow * gnc_invoice_search (GncInvoice *start, GncOwner *owner, GNCBook *book);

/*
 * These callbacks are for use with the gnc_general_search widget
 *
 * select() provides a Select Dialog and returns it.
 * edit() opens the existing invoice for editing and returns NULL.
 */
GNCSearchWindow * gnc_invoice_search_select (gpointer start, gpointer book);
GNCSearchWindow * gnc_invoice_search_edit (gpointer start, gpointer book);

void gnc_business_call_owner_report (GncOwner *owner, Account *acc);

#endif /* GNC_DIALOG_INVOICE_H_ */
