/*
 * dialog-invoice.h -- Dialog(s) for Invoice search and entry
 * Copyright (C) 2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 *
 * Copyright (c) 2005,2006 David Hampton <hampton@employees.org>
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


#ifndef GNC_DIALOG_INVOICE_H_
#define GNC_DIALOG_INVOICE_H_

typedef struct _invoice_window InvoiceWindow;

#include "qof.h"
#include "gncInvoice.h"
#include "gncOwner.h"
#include "dialog-search.h"
#include "dialog-query-view.h"

typedef enum
{
    INVSORT_BY_STANDARD = 0,
    INVSORT_BY_DATE,
    INVSORT_BY_DATE_ENTERED,
    INVSORT_BY_DESC,
    INVSORT_BY_QTY,
    INVSORT_BY_PRICE
} invoice_sort_type_t;


/* Create and edit an invoice */
InvoiceWindow * gnc_ui_invoice_edit (GtkWindow *parent, GncInvoice *invoice);
InvoiceWindow * gnc_ui_invoice_new (GtkWindow *parent, GncOwner *owner, QofBook *book);

/** Create a new invoice as a duplicate of the given existing invoice.
 *
 * \param old_invoice The invoice which is being duplicated
 * \param open_properties If TRUE, open the "invoice properties" dialog window after creating the new invoice
 * \param new_date If non-NULL, use this date as the date for the "opening date" and also as date for all invoice entries.
 *
 * \return The InvoiceWindow structure that contains a whole lot of things,
 * among others the "created_invoice" as a GncInvoice* pointer on the newly
 * created invoice.
 */
InvoiceWindow* gnc_ui_invoice_duplicate (GtkWindow* parent, GncInvoice* old_invoice, gboolean open_properties, const GDate* new_date);

/* Search for invoices */
GNCSearchWindow * gnc_invoice_search (GtkWindow *parent, GncInvoice *start, GncOwner *owner, QofBook *book);

void gnc_business_call_owner_report (GtkWindow* parent, GncOwner *owner, Account *acc);

void gnc_invoice_window_sort (InvoiceWindow *iw, invoice_sort_type_t sort_code);

GtkWidget * gnc_invoice_window_create_summary_bar (InvoiceWindow *iw);

void gnc_invoice_window_changed (InvoiceWindow *iw, GtkWidget *window);;

gchar *gnc_invoice_get_help (InvoiceWindow *iw);

gchar *gnc_invoice_get_title (InvoiceWindow *iw);

GncInvoiceType gnc_invoice_get_type_from_window(InvoiceWindow *iw);

#ifdef __GNC_PLUGIN_PAGE_H
#include "gnc-main-window.h"
GncPluginPage *gnc_invoice_recreate_page (GncMainWindow *window, GKeyFile *key_file, const gchar *group_name);
void gnc_invoice_save_page (InvoiceWindow *iw, GKeyFile *key_file, const gchar *group_name);
#endif

GtkWidget * gnc_invoice_create_page (InvoiceWindow *iw, gpointer page);

GtkWidget *gnc_invoice_get_register(InvoiceWindow *iw);
GtkWidget *gnc_invoice_get_notes(InvoiceWindow *iw);

/* definitions for CB functions */
void gnc_invoice_window_destroy_cb (GtkWidget *widget, gpointer data);

void gnc_invoice_window_new_invoice_cb (GtkWindow* parent, gpointer data);
void gnc_invoice_window_printCB (GtkWindow* parent, gpointer data);
void gnc_invoice_window_cut_cb (GtkWidget *widget, gpointer data);
void gnc_invoice_window_copy_cb (GtkWidget *widget, gpointer data);
void gnc_invoice_window_paste_cb (GtkWidget *widget, gpointer data);
void gnc_invoice_window_editCB (GtkWindow* parent, gpointer data);
void gnc_invoice_window_duplicateInvoiceCB (GtkWindow* parent, gpointer data);
void gnc_invoice_window_postCB (GtkWidget *widget, gpointer data);
void gnc_invoice_window_unpostCB (GtkWidget *widget, gpointer data);
void gnc_invoice_window_recordCB (GtkWidget *widget, gpointer data);
void gnc_invoice_window_cancelCB (GtkWidget *widget, gpointer data);
void gnc_invoice_window_deleteCB (GtkWidget *widget, gpointer data);
void gnc_invoice_window_blankCB (GtkWidget *widget, gpointer data);
void gnc_invoice_window_duplicateCB (GtkWidget *widget, gpointer data);
void gnc_invoice_window_payment_cb (GtkWindow *parent, gpointer data);
void gnc_invoice_window_report_owner_cb (GtkWindow *parent, gpointer data);

void gnc_invoice_window_entryUpCB (GtkWidget *widget, gpointer data);
void gnc_invoice_window_entryDownCB (GtkWidget *widget, gpointer data);

#endif /* GNC_DIALOG_INVOICE_H_ */
