/*
 * dialog-invoice.h -- Dialog(s) for Invoice search and entry
 * Copyright (C) 2002 Derek Atkins
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
 * Boston, MA  02111-1307,  USA       gnu@gnu.org
 */


#ifndef GNC_DIALOG_INVOICE_H_
#define GNC_DIALOG_INVOICE_H_

typedef struct _invoice_window InvoiceWindow;

#include "qofbook.h"
#include "gncInvoice.h"
#include "gncOwner.h"
#include "dialog-search.h"
#include "dialog-query-list.h"

/* Create and edit an invoice */
InvoiceWindow * gnc_ui_invoice_edit (GncInvoice *invoice);
InvoiceWindow * gnc_ui_invoice_new (GncOwner *owner, QofBook *book);

/* Search for invoices */
GNCSearchWindow * gnc_invoice_search (GncInvoice *start, GncOwner *owner, QofBook *book);

/*
 * These callbacks are for use with the gnc_general_search widget
 *
 * select() provides a Select Dialog and returns it.
 * edit() opens the existing invoice for editing and returns NULL.
 */
GNCSearchWindow * gnc_invoice_search_select (gpointer start, gpointer book);
GNCSearchWindow * gnc_invoice_search_edit (gpointer start, gpointer book);

void gnc_business_call_owner_report (GncOwner *owner, Account *acc);

DialogQueryList *gnc_invoice_show_bills_due (QofBook *book, double days_in_advance);

#endif /* GNC_DIALOG_INVOICE_H_ */
