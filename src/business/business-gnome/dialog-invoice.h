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
#include "dialog-query-list.h"

typedef enum
{
    BY_STANDARD = 0,
    BY_DATE,
    BY_DATE_ENTERED,
    BY_DESC,
    BY_QTY,
    BY_PRICE
} invoice_sort_type_t;


#define GCONF_SECTION_INVOICE "dialogs/business/invoice"
#define GCONF_SECTION_BILL    "dialogs/business/bill"
#define GCONF_SECTION_VOUCHER "dialogs/business/voucher"


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

void gnc_invoice_window_sort (InvoiceWindow *iw, invoice_sort_type_t sort_code);

GtkWidget * gnc_invoice_window_create_summary_bar (InvoiceWindow *iw);

void gnc_invoice_window_changed (InvoiceWindow *iw, GtkWidget *window);;

gchar *gnc_invoice_get_help (InvoiceWindow *iw);

gchar *gnc_invoice_get_title (InvoiceWindow *iw);

#ifdef __GNC_PLUGIN_PAGE_H
#include "gnc-main-window.h"
GncPluginPage *gnc_invoice_recreate_page (GncMainWindow *window, GKeyFile *key_file, const gchar *group_name);
void gnc_invoice_save_page (InvoiceWindow *iw, GKeyFile *key_file, const gchar *group_name);
#endif

GtkWidget * gnc_invoice_create_page (InvoiceWindow *iw, gpointer page);

DialogQueryList *gnc_invoice_show_bills_due (QofBook *book, double days_in_advance);

GtkWidget *gnc_invoice_get_register(InvoiceWindow *iw);

/* definitions for CB functions */
void gnc_invoice_window_destroy_cb (GtkWidget *widget, gpointer data);

void gnc_invoice_window_new_invoice_cb (GtkWidget *widget, gpointer data);
void gnc_invoice_window_printCB (GtkWidget *widget, gpointer data);
void gnc_invoice_window_cut_cb (GtkWidget *widget, gpointer data);
void gnc_invoice_window_copy_cb (GtkWidget *widget, gpointer data);
void gnc_invoice_window_paste_cb (GtkWidget *widget, gpointer data);
void gnc_invoice_window_editCB (GtkWidget *widget, gpointer data);
void gnc_invoice_window_postCB (GtkWidget *widget, gpointer data);
void gnc_invoice_window_unpostCB (GtkWidget *widget, gpointer data);
void gnc_invoice_window_recordCB (GtkWidget *widget, gpointer data);
void gnc_invoice_window_cancelCB (GtkWidget *widget, gpointer data);
void gnc_invoice_window_deleteCB (GtkWidget *widget, gpointer data);
void gnc_invoice_window_blankCB (GtkWidget *widget, gpointer data);
void gnc_invoice_window_duplicateCB (GtkWidget *widget, gpointer data);
void gnc_invoice_window_payment_cb (GtkWidget *widget, gpointer data);
void gnc_invoice_window_report_owner_cb (GtkWidget *widget, gpointer data);

#endif /* GNC_DIALOG_INVOICE_H_ */
