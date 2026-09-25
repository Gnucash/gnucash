/*
 * dialog-invoice.c -- Dialog for Invoice entry
 * Copyright (C) 2001,2002,2006 Derek Atkins
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

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <libguile.h>
#include "swig-runtime.h"

#include "qof.h"

#include "dialog-utils.h"
#include "gnc-component-manager.h"
#include "gnc-ui.h"
#include "gnc-gui-query.h"
#include "gnc-prefs.h"
#include "gnc-ui-util.h"
#include "gnc-date.h"
#include "gnc-date-edit.h"
#include "gnc-amount-edit.h"
#include "gnucash-sheet.h"
#include "gnucash-register.h"
#include "window-report.h"
#include "dialog-search.h"
#include "search-param.h"
#include "gnc-session.h"
#include "gncOwner.h"
#include "gncInvoice.h"
#include "gncInvoiceP.h"
#include <gnc-string-utils.h>

#include "gncEntryLedger.h"

#include "gnc-plugin-page.h"
#include "gnc-general-search.h"
#include "dialog-date-close.h"
#include "dialog-invoice.h"
#include "dialog-job.h"
#include "business-gnome-utils.h"
#include "dialog-payment.h"
#include "dialog-tax-table.h"
#include "dialog-billterms.h"
#include "dialog-account.h"
#include "guile-mappings.h"
#include "dialog-dup-trans.h"

#include "dialog-query-view.h"

#include "gnc-plugin-business.h"
#include "gnc-plugin-page-invoice.h"
#include "gnc-plugin-page-report.h"
#include "gnc-main-window.h"
#include "gnc-state.h"

#include "dialog-doclink.h"
#include "dialog-doclink-utils.h"
#include "dialog-transfer.h"
#include "gnc-uri-utils.h"
#include "gnc-report-combo.h"

#define DIALOG_NEW_INVOICE_CM_CLASS "dialog-new-invoice"
#define DIALOG_VIEW_INVOICE_CM_CLASS "dialog-view-invoice"

#define GNC_PREFS_GROUP_CUSTOMER  "dialogs.customer-due"
#define GNC_PREFS_GROUP_VENDOR    "dialogs.vendor-due"

#define GNC_PREFS_GROUP_SEARCH   "dialogs.business.invoice-search"
#define GNC_PREF_NOTIFY_WHEN_DUE "notify-when-due"
#define GNC_PREF_ACCUM_SPLITS    "accumulate-splits"
#define GNC_PREF_DAYS_IN_ADVANCE "days-in-advance"

void gnc_invoice_window_ok_cb (GtkWidget *widget, gpointer data);
void gnc_invoice_window_cancel_cb (GtkWidget *widget, gpointer data);
void gnc_invoice_window_help_cb (GtkWidget *widget, gpointer data);
void gnc_invoice_type_toggled_cb (GtkWidget *widget, gpointer data);
void gnc_invoice_id_changed_cb (GtkWidget *widget, gpointer data);
void gnc_invoice_terms_changed_cb (GtkDropDown *dropdown, GParamSpec *pspec, gpointer data);

#define ENUM_INVOICE_TYPE(_) \
  _(NEW_INVOICE, )  \
  _(MOD_INVOICE, )  \
  _(DUP_INVOICE, )  \
  _(EDIT_INVOICE, ) \
  _(VIEW_INVOICE, )

DEFINE_ENUM(InvoiceDialogType, ENUM_INVOICE_TYPE)
AS_STRING_DEC(InvoiceDialogType, ENUM_INVOICE_TYPE)
FROM_STRING_DEC(InvoiceDialogType, ENUM_INVOICE_TYPE)

FROM_STRING_FUNC(InvoiceDialogType, ENUM_INVOICE_TYPE)
AS_STRING_FUNC(InvoiceDialogType, ENUM_INVOICE_TYPE)

typedef enum
{
    DUE_FOR_VENDOR,     // show bills due
    DUE_FOR_CUSTOMER,   // show invoices due
} GncWhichDueType;

struct _invoice_select_window
{
    QofBook  *book;
    GncOwner *owner;
    QofQuery *q;
    GncOwner  owner_def;
};

#define UNUSED_VAR     __attribute__ ((unused))

static QofLogModule UNUSED_VAR log_module = G_LOG_DOMAIN; //G_LOG_BUSINESS;

/** This data structure does double duty.  It is used to maintain
 *  information for the "New Invoice" dialog, and it is also used to
 *  maintain information for the "Invoice Entry" page that is embedded
 *  into a main window.  Beware, as not all fields are used by both windows.
 */
struct _invoice_window
{
    GtkBuilder   * builder;

    GtkWidget  * dialog;         /* Used by 'New Invoice Window' */
    GncPluginPage *page;        /* Used by 'Edit Invoice' Page */
    const gchar * page_state_name;    /* Used for loading open state information */

    /* Summary Bar Widgets */
    GtkWidget  * total_label;
    GtkWidget  * total_cash_label;
    GtkWidget  * total_charge_label;
    GtkWidget  * total_subtotal_label;
    GtkWidget  * total_tax_label;

    /* Data Widgets */
    GtkWidget  * info_label; /*Default in glade is "Invoice Information"*/
    GtkWidget  * id_label; /* Default in glade is Invoice ID */
    GtkWidget  * type_label;
    GtkWidget  * type_label_hbox;
    GtkWidget  * type_hbox;
    GtkWidget  * type_choice;
    GtkWidget  * id_entry;
    GtkWidget  * notes_text;
    GtkWidget  * opened_date;
    GtkWidget  * posted_date_hbox;
    GtkWidget  * posted_date;
    GtkWidget  * active_check;
    GtkWidget  * paid_label;

    GtkWidget  * doclink_button;

    GtkWidget  * owner_box;
    GtkWidget  * owner_label;
    GtkWidget  * owner_choice;
    GtkWidget  * job_label;
    GtkWidget  * job_box;
    GtkWidget  * job_choice;
    GtkWidget  * billing_id_entry;
    GtkWidget  * terms_menu;

    /* Project Widgets (used for Bills only) */
    GtkWidget  * proj_frame;
    GtkWidget  * proj_cust_box;
    GtkWidget  * proj_cust_choice;
    GtkWidget  * proj_job_box;
    GtkWidget  * proj_job_choice;

    /* Expense Voucher Widgets */
    GtkWidget  * to_charge_frame;
    GtkWidget  * to_charge_edit;

    gint         width;

    GncBillTerm     * terms;
    GnucashRegister * reg;
    GncEntryLedger  * ledger;

    invoice_sort_type_t last_sort;

    InvoiceDialogType   dialog_type;
    GncGUID      invoice_guid;
    gboolean     is_credit_note;
    gint         component_id;
    QofBook    * book;
    GncInvoice * created_invoice;
    GncOwner     owner;
    GncOwner     job;

    GncOwner     proj_cust;
    GncOwner     proj_job;

    /* the cached reportPage for this invoice. note this is not saved
       into .gcm file therefore the invoice editor->report link is lost
       upon restart. */
    GncPluginPage *reportPage;

    /* for Unposting */
    gboolean     reset_tax_tables;
    gboolean     save_pending;
};

/* Forward definitions for CB functions */
void gnc_invoice_window_active_toggled_cb (GtkWidget *widget, gpointer data);
static void gnc_invoice_window_leave_notes_cb (GtkEventControllerFocus *controller,
                                               gpointer user_data);
DialogQueryView *gnc_invoice_show_docs_due (GtkWindow *parent, QofBook *book, double days_in_advance, GncWhichDueType duetype);

#define INV_WIDTH_PREFIX "invoice_reg"
#define BILL_WIDTH_PREFIX "bill_reg"
#define VOUCHER_WIDTH_PREFIX "voucher_reg"

static void gnc_invoice_update_window (InvoiceWindow *iw, GtkWidget *widget);
static InvoiceWindow * gnc_ui_invoice_modify (GtkWindow *parent, GncInvoice *invoice);

/*******************************************************************************/
/* FUNCTIONS FOR ACCESSING DATA STRUCTURE FIELDS */

static GtkWidget *
iw_get_window (InvoiceWindow *iw)
{
    if (iw->page)
        return gnc_plugin_page_get_window (iw->page);
    return iw->dialog;
}

GtkWidget *
gnc_invoice_get_register (InvoiceWindow *iw)
{
    if (iw)
        return (GtkWidget *)iw->reg;
    return NULL;
}

GtkWidget *
gnc_invoice_get_notes (InvoiceWindow *iw)
{
    if (iw)
        return (GtkWidget *)iw->notes_text;
    return NULL;
}

/*******************************************************************************/
/* FUNCTIONS FOR UNPOSTING */

static GncInvoice *iw_get_invoice (InvoiceWindow *iw);

typedef struct
{
    InvoiceWindow *iw;
    GWeakRef window;
    gulong destroy_handler;
    QofBook *book;
    GncGUID invoice_guid;
    GtkWindow *dialog;
    GtkCheckButton *reset_tax_tables;
    gboolean reset_tax_tables_selected;
    gboolean completed;
} InvoiceUnpostRequest;

static void invoice_unpost_complete (InvoiceUnpostRequest *request,
                                     gboolean approved);

static void
invoice_unpost_request_destroyed (GtkWidget *widget,
                                  InvoiceUnpostRequest *request)
{
    (void)widget;
    request->iw = NULL;
    request->destroy_handler = 0;
    invoice_unpost_complete (request, FALSE);
}

static void
invoice_unpost_request_free (InvoiceUnpostRequest *request)
{
    GtkWidget *window = g_weak_ref_get (&request->window);

    if (window && request->destroy_handler)
        g_signal_handler_disconnect (window, request->destroy_handler);
    g_clear_object (&window);
    g_weak_ref_clear (&request->window);
    g_free (request);
}

static void
invoice_unpost_destroy_dialog (InvoiceUnpostRequest *request)
{
    GtkWindow *dialog = g_steal_pointer (&request->dialog);

    if (!dialog)
        return;

    g_signal_handlers_disconnect_by_data (dialog, request);
    gtk_window_destroy (dialog);
    g_object_unref (dialog);
}

static void
invoice_unpost_complete (InvoiceUnpostRequest *request, gboolean approved)
{
    GncInvoice *invoice;
    gboolean result;

    if (!request || request->completed)
        return;

    request->completed = TRUE;
    if (approved && request->iw && !qof_book_shutting_down (request->book))
    {
        invoice = gncInvoiceLookup (request->book, &request->invoice_guid);
        if (invoice && iw_get_invoice (request->iw) == invoice)
        {
            request->iw->reset_tax_tables = request->reset_tax_tables_selected;
            gnc_suspend_gui_refresh ();
            result = gncInvoiceUnpost (invoice, request->iw->reset_tax_tables);
            gnc_resume_gui_refresh ();
            if (result)
            {
                request->iw->dialog_type = EDIT_INVOICE;
                gnc_entry_ledger_set_readonly (request->iw->ledger, FALSE);
                gnc_invoice_update_window (request->iw, NULL);
                gnc_table_refresh_gui (
                    gnc_entry_ledger_get_table (request->iw->ledger), FALSE);
            }
        }
    }

    invoice_unpost_destroy_dialog (request);
    invoice_unpost_request_free (request);
}

static void
invoice_unpost_accept_clicked_cb (GtkButton *button,
                                  InvoiceUnpostRequest *request)
{
    (void)button;
    request->reset_tax_tables_selected = gtk_check_button_get_active (
        request->reset_tax_tables);
    invoice_unpost_complete (request, TRUE);
}

static void
invoice_unpost_cancel_clicked_cb (GtkButton *button,
                                  InvoiceUnpostRequest *request)
{
    (void)button;
    invoice_unpost_complete (request, FALSE);
}

static gboolean
invoice_unpost_close_request_cb (GtkWindow *dialog,
                                 InvoiceUnpostRequest *request)
{
    (void)dialog;
    invoice_unpost_complete (request, FALSE);
    return TRUE;
}

static void
invoice_unpost_destroy_cb (GtkWidget *widget, InvoiceUnpostRequest *request)
{
    (void)widget;
    if (!request->completed)
        g_clear_object (&request->dialog);
    invoice_unpost_complete (request, FALSE);
}

static void
invoice_unpost_request (InvoiceWindow *iw)
{
    GncOwnerType owner_type;
    GtkWidget *window;
    GtkBuilder *builder;
    GtkWidget *dialog;
    GtkWidget *ok_button;
    GtkWidget *cancel_button;
    const gchar *style_label;
    InvoiceUnpostRequest *request;

    window = iw_get_window (iw);
    if (!window)
        return;

    request = g_new0 (InvoiceUnpostRequest, 1);
    request->iw = iw;
    request->book = iw->book;
    request->invoice_guid = iw->invoice_guid;
    g_weak_ref_init (&request->window, window);
    request->destroy_handler = g_signal_connect (
        window, "destroy", G_CALLBACK (invoice_unpost_request_destroyed), request);

    builder = gtk_builder_new ();
    if (!gnc_builder_add_from_file (builder, "dialog-invoice.glade",
                                    "unpost_message_dialog"))
    {
        g_object_unref (builder);
        invoice_unpost_request_free (request);
        return;
    }

    dialog = GTK_WIDGET (gtk_builder_get_object (builder, "unpost_message_dialog"));
    request->reset_tax_tables = GTK_CHECK_BUTTON (
        gtk_builder_get_object (builder, "yes_tt_reset"));
    ok_button = GTK_WIDGET (gtk_builder_get_object (builder, "okbutton1"));
    cancel_button = GTK_WIDGET (gtk_builder_get_object (builder, "cancelbutton1"));
    if (!dialog || !request->reset_tax_tables || !ok_button || !cancel_button)
    {
        g_object_unref (builder);
        invoice_unpost_request_free (request);
        return;
    }

    request->dialog = g_object_ref (GTK_WINDOW (dialog));
    owner_type = gncOwnerGetType (&iw->owner);
    switch (owner_type)
    {
    case GNC_OWNER_VENDOR:
        style_label = "gnc-class-vendors";
        break;
    case GNC_OWNER_EMPLOYEE:
        style_label = "gnc-class-employees";
        break;
    default:
        style_label = "gnc-class-customers";
        break;
    }
    gnc_widget_style_context_add_class (GTK_WIDGET (request->dialog), style_label);
    gtk_window_set_transient_for (request->dialog, GTK_WINDOW (window));
    gtk_window_set_modal (request->dialog, TRUE);
    gtk_window_set_default_widget (request->dialog, ok_button);
    g_signal_connect (ok_button, "clicked",
                      G_CALLBACK (invoice_unpost_accept_clicked_cb), request);
    g_signal_connect (cancel_button, "clicked",
                      G_CALLBACK (invoice_unpost_cancel_clicked_cb), request);
    g_signal_connect (request->dialog, "close-request",
                      G_CALLBACK (invoice_unpost_close_request_cb), request);
    g_signal_connect (request->dialog, "destroy",
                      G_CALLBACK (invoice_unpost_destroy_cb), request);
    g_object_unref (builder);

    gtk_window_present (request->dialog);
}
/*******************************************************************************/
/* INVOICE WINDOW */

static GncInvoice *
iw_get_invoice (InvoiceWindow *iw)
{
    if (!iw)
        return NULL;

    return gncInvoiceLookup (iw->book, &iw->invoice_guid);
}

GncInvoice *
gnc_invoice_window_get_invoice (InvoiceWindow *iw)
{
    if (!iw)
        return NULL;

    return iw_get_invoice (iw);
}

GtkWidget *
gnc_invoice_window_get_doclink_button (InvoiceWindow *iw)
{
    if (!iw)
        return NULL;

    return iw->doclink_button;
}

static void
set_gncEntry_switch_type (gpointer data, gpointer user_data)
{
    GncEntry *entry = data;
    //g_warning("Modifying date for entry with desc=\"%s\"", gncEntryGetDescription(entry));

    gncEntrySetQuantity (entry, gnc_numeric_neg (gncEntryGetQuantity (entry)));
}

static void
set_gncEntry_date(gpointer data, gpointer user_data)
{
    GncEntry *entry = data;
    time64 new_date = *(time64*) user_data;
    //g_warning("Modifying date for entry with desc=\"%s\"", gncEntryGetDescription(entry));

    gncEntrySetDate(entry, gnc_time64_get_day_neutral (new_date));
    /*gncEntrySetDateEntered(entry, *new_date); - don't modify this
     * because apparently it defines the ordering of the entries,
     * which we don't want to change. */
}

static void gnc_ui_to_invoice (InvoiceWindow *iw, GncInvoice *invoice)
{
    GtkTextBuffer* text_buffer;
    GtkTextIter start, end;
    gchar *text;
    time64 time;
    gboolean is_credit_note = gncInvoiceGetIsCreditNote (invoice);

    if (iw->dialog_type == VIEW_INVOICE)
        return;

    gnc_suspend_gui_refresh ();

    gncInvoiceBeginEdit (invoice);

    if (iw->active_check)
        gncInvoiceSetActive (invoice, gtk_check_button_get_active
                             (GTK_CHECK_BUTTON (iw->active_check)));

    text_buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW(iw->notes_text));
    gtk_text_buffer_get_bounds (text_buffer, &start, &end);
    text = gtk_text_buffer_get_text (text_buffer, &start, &end, FALSE);
    gncInvoiceSetNotes (invoice, text);

    if (iw->to_charge_edit)
        gncInvoiceSetToChargeAmount (invoice,
                                     gnc_amount_edit_get_amount
                                     (GNC_AMOUNT_EDIT (iw->to_charge_edit)));

    time = gnc_date_edit_get_date (GNC_DATE_EDIT (iw->opened_date));

    /* Only set these values for NEW/MOD INVOICE types */
    if (iw->dialog_type != EDIT_INVOICE)
    {
        gncInvoiceSetID (invoice, gnc_entry_get_text (GTK_ENTRY (iw->id_entry)));
        gncInvoiceSetBillingID (invoice, gnc_entry_get_text (GTK_ENTRY (iw->billing_id_entry)));
        gncInvoiceSetTerms (invoice, iw->terms);

        gncInvoiceSetDateOpened (invoice, time);

        gnc_owner_get_owner (iw->owner_choice, &(iw->owner));
        if (iw->job_choice)
            gnc_owner_get_owner (iw->job_choice, &(iw->job));

        /* Only set the job if we've actually got one */
        if (gncOwnerGetJob (&(iw->job)))
            gncInvoiceSetOwner (invoice, &(iw->job));
        else
            gncInvoiceSetOwner (invoice, &(iw->owner));

        /* Set the invoice currency based on the owner */
        gncInvoiceSetCurrency (invoice, gncOwnerGetCurrency (&iw->owner));

        /* Only set the BillTo if we've actually got one */
        if (gncOwnerGetJob (&iw->proj_job))
            gncInvoiceSetBillTo (invoice, &iw->proj_job);
        else
            gncInvoiceSetBillTo (invoice, &iw->proj_cust);
    }

    /* Document type can only be modified for a new or duplicated invoice/credit note */
    if (iw->dialog_type == NEW_INVOICE || iw->dialog_type == DUP_INVOICE)
    {
        /* Update the entry dates to match the invoice date. This only really
         * should happen for a duplicate invoice. However as a new invoice has
         * no entries we can run this unconditionally. */
        g_list_foreach(gncInvoiceGetEntries(invoice),
                    &set_gncEntry_date, &time);


        gncInvoiceSetIsCreditNote (invoice, iw->is_credit_note);
    }

    /* If the document type changed on a duplicated invoice,
     * its entries should be updated
     */
    if (iw->dialog_type == DUP_INVOICE && iw->is_credit_note != is_credit_note)
    {
        g_list_foreach(gncInvoiceGetEntries(invoice),
                       &set_gncEntry_switch_type, NULL);
    }

    gncInvoiceCommitEdit (invoice);
    gnc_resume_gui_refresh ();
    g_free (text);
}

static gboolean
gnc_invoice_window_verify_ok (InvoiceWindow *iw)
{
    const char *res;
    gchar *string;


    /* Check the Owner */
    gnc_owner_get_owner (iw->owner_choice, &(iw->owner));
    res = gncOwnerGetName (&(iw->owner));
    if (res == NULL || g_strcmp0 (res, "") == 0)
    {
        gnc_error_dialog (GTK_WINDOW (iw_get_window(iw)), "%s",
                          /* Translators: In this context,
                             'Billing information' maps to the
                             label in the frame and means
                             e.g. customer i.e. the company being
                             invoiced. */
                          _("You need to supply Billing Information."));
        return FALSE;
    }

    /* Check the ID; set one if necessary */
    res = gnc_entry_get_text (GTK_ENTRY (iw->id_entry));
    if (g_strcmp0 (res, "") == 0)
    {
        /* Invoices and bills have separate counters.
           Therefore we pass the GncOwer to gncInvoiceNextID
           so it knows whether we are creating a bill
           or an invoice. */
        string = gncInvoiceNextID(iw->book, &(iw->owner));
        gnc_entry_set_text (GTK_ENTRY (iw->id_entry), string);
        g_free(string);
    }

    return TRUE;
}

static gboolean
gnc_invoice_window_ok_save (InvoiceWindow *iw)
{
    if (!gnc_invoice_window_verify_ok (iw))
        return FALSE;

    {
        GncInvoice *invoice = iw_get_invoice (iw);
        if (invoice)
        {
            gnc_ui_to_invoice (iw, invoice);
        }
        /* Save the invoice to return it later. */
        iw->created_invoice = invoice;
    }
    return TRUE;
}

typedef struct
{
    InvoiceWindow *iw;
    GWeakRef window;
    gulong destroy_handler;
    QofBook *book;
    GncGUID invoice_guid;
} InvoiceSaveRequest;

static void
invoice_save_request_destroyed (GtkWidget *widget, InvoiceSaveRequest *request)
{
    (void)widget;
    request->iw = NULL;
    request->destroy_handler = 0;
}

static void
invoice_save_request_free (InvoiceSaveRequest *request)
{
    GtkWidget *window = g_weak_ref_get (&request->window);

    if (window && request->destroy_handler)
        g_signal_handler_disconnect (window, request->destroy_handler);
    g_clear_object (&window);
    g_weak_ref_clear (&request->window);
    g_free (request);
}

static InvoiceSaveRequest *
invoice_save_request_new (InvoiceWindow *iw)
{
    GtkWidget *window;
    InvoiceSaveRequest *request;

    if (!iw || !iw->ledger || !iw_get_invoice (iw) ||
        !(window = iw_get_window (iw)))
        return NULL;

    request = g_new0 (InvoiceSaveRequest, 1);
    request->iw = iw;
    request->book = iw->book;
    request->invoice_guid = iw->invoice_guid;
    g_weak_ref_init (&request->window, window);
    request->destroy_handler = g_signal_connect (
        window, "destroy", G_CALLBACK (invoice_save_request_destroyed), request);
    return request;
}

static gboolean
invoice_save_request_is_current (const InvoiceSaveRequest *request)
{
    GncInvoice *invoice;

    if (!request->iw || qof_book_shutting_down (request->book))
        return FALSE;

    invoice = gncInvoiceLookup (request->book, &request->invoice_guid);
    return invoice && iw_get_invoice (request->iw) == invoice;
}

static void
invoice_save_ledger_finished (GncEntryLedger *ledger, gboolean completed,
                              gpointer user_data)
{
    InvoiceSaveRequest *request = user_data;
    InvoiceWindow *iw = request->iw;

    (void)ledger;
    if (iw)
        iw->save_pending = FALSE;

    if (completed && invoice_save_request_is_current (request) &&
        gnc_invoice_window_ok_save (iw))
    {
        iw->invoice_guid = *guid_null ();
        if ((iw->dialog_type == NEW_INVOICE || iw->dialog_type == DUP_INVOICE) &&
            iw->created_invoice)
            gnc_ui_invoice_edit (gnc_ui_get_main_window (iw->dialog),
                                 iw->created_invoice);
        gnc_close_gui_component (iw->component_id);
    }

    invoice_save_request_free (request);
}

void
gnc_invoice_window_ok_cb (GtkWidget *widget, gpointer data)
{
    InvoiceWindow *iw = data;
    InvoiceSaveRequest *request;

    (void)widget;
    if (!iw || !iw->ledger || iw->save_pending)
        return;

    request = invoice_save_request_new (iw);
    if (!request)
        return;

    iw->save_pending = TRUE;
    gnc_entry_ledger_check_close_async (iw_get_window (iw), iw->ledger,
                                        invoice_save_ledger_finished, request);
}

void
gnc_invoice_window_cancel_cb (GtkWidget *widget, gpointer data)
{
    InvoiceWindow *iw = data;

    gnc_close_gui_component (iw->component_id);
}

void
gnc_invoice_window_help_cb (GtkWidget *widget, gpointer data)
{
    InvoiceWindow *iw = data;
    GncOwnerType owner_type = gncOwnerGetType (&iw->owner);

    switch(owner_type)
    {
        case GNC_OWNER_CUSTOMER:
           gnc_gnome_help (GTK_WINDOW(iw->dialog), DF_MANUAL, DL_USAGE_INVOICE);
           break;
        case GNC_OWNER_VENDOR:
           gnc_gnome_help (GTK_WINDOW(iw->dialog), DF_MANUAL, DL_USAGE_BILL);
           break;
        default:
           gnc_gnome_help (GTK_WINDOW(iw->dialog), DF_MANUAL, DL_USAGE_VOUCHER);
           break;
    }
}

static const gchar *
gnc_invoice_window_get_state_group (InvoiceWindow *iw)
{
    switch (gncOwnerGetType (gncOwnerGetEndOwner (&iw->owner)))
    {
        case GNC_OWNER_VENDOR:
            return "Vendor documents";
            break;
        case GNC_OWNER_EMPLOYEE:
            return "Employee documents";
            break;
        default:
            return "Customer documents";
            break;
    }
}

/* Save user state layout information for Invoice/Bill/Voucher
 * documents so it can be used for the default user set layout
 */
void
gnc_invoice_window_save_document_layout_to_user_state (InvoiceWindow *iw)
{
    Table *table = gnc_entry_ledger_get_table (iw->ledger);
    const gchar *group = gnc_invoice_window_get_state_group (iw);

    gnc_table_save_state (table, group);
}

/* Removes the user state layout information for Invoice/Bill/Voucher
 * documents and also resets the current layout to the built-in defaults
 */
void
gnc_invoice_window_reset_document_layout_and_clear_user_state (InvoiceWindow *iw)
{
    GnucashRegister *reg = iw->reg;
    const gchar *group = gnc_invoice_window_get_state_group (iw);

    gnucash_register_reset_sheet_layout (reg);
    gnc_state_drop_sections_for (group);
}

/* Checks to see if there is user state layout information for
 * Invoice/Bill/Voucher documents so it can be used for the
 * default user layout
 */
gboolean
gnc_invoice_window_document_has_user_state (InvoiceWindow *iw)
{
    GKeyFile *state_file = gnc_state_get_current ();
    const gchar *group = gnc_invoice_window_get_state_group (iw);
    return g_key_file_has_group (state_file, group);
}

void
gnc_invoice_window_destroy_cb (GtkWidget *widget, gpointer data)
{
    InvoiceWindow *iw = data;
    GncInvoice *invoice = iw_get_invoice (iw);

    /* This callback is also used by the embedded invoice page. The widget is
     * already in destruction, so this path only tears down controller state. */
    (void)widget;

    gnc_suspend_gui_refresh ();

    if ((iw->dialog_type == NEW_INVOICE || iw->dialog_type == DUP_INVOICE)
         && invoice != NULL)
    {
        gncInvoiceRemoveEntries (invoice);
        gncInvoiceBeginEdit (invoice);
        gncInvoiceDestroy (invoice);
        iw->invoice_guid = *guid_null ();
    }

    gnc_entry_ledger_destroy (iw->ledger);
    gnc_unregister_gui_component (iw->component_id);
    g_object_unref (G_OBJECT (iw->builder));
    gnc_resume_gui_refresh ();

    g_free (iw);
}

void
gnc_invoice_window_editCB (GtkWindow *parent, gpointer data)
{
    InvoiceWindow *iw = data;
    GncInvoice *invoice = iw_get_invoice (iw);

    if (invoice)
        gnc_ui_invoice_modify (parent, invoice);
}

void
gnc_invoice_window_duplicateInvoiceCB (GtkWindow *parent, gpointer data)
{
    InvoiceWindow *iw = data;
    GncInvoice *invoice = iw_get_invoice (iw);

    if (invoice)
        gnc_ui_invoice_duplicate (parent, invoice, TRUE, NULL);
}

void gnc_invoice_window_entryUpCB (GtkWidget *widget, gpointer data)
{
    InvoiceWindow *iw = data;
    if (!iw || !iw->ledger)
        return;

    gnc_entry_ledger_move_current_entry_updown(iw->ledger, TRUE);
}
void gnc_invoice_window_entryDownCB (GtkWidget *widget, gpointer data)
{
    InvoiceWindow *iw = data;
    if (!iw || !iw->ledger)
        return;

    gnc_entry_ledger_move_current_entry_updown(iw->ledger, FALSE);
}

static void
gnc_invoice_window_record_finished (GncEntryLedger *ledger, gboolean completed,
                                    gpointer user_data)
{
    InvoiceWindow *iw = user_data;

    (void)ledger;
    if (completed)
        gnucash_register_goto_next_virt_row (iw->reg);
}

void
gnc_invoice_window_recordCB (GtkWidget *widget, gpointer data)
{
    InvoiceWindow *iw = data;

    (void)widget;
    if (iw && iw->ledger)
        gnc_entry_ledger_commit_entry_async (iw->ledger,
                                             gnc_invoice_window_record_finished, iw);
}

void
gnc_invoice_window_cancelCB (GtkWidget *widget, gpointer data)
{
    InvoiceWindow *iw = data;

    if (!iw || !iw->ledger)
        return;

    gnc_entry_ledger_cancel_cursor_changes (iw->ledger);
}

typedef struct
{
    GWeakRef window;
    GncGUID book_guid;
    GncGUID invoice_guid;
    GncGUID entry_guid;
} InvoiceDeleteRequest;

static void
invoice_delete_request_free (InvoiceDeleteRequest *request)
{
    g_weak_ref_clear (&request->window);
    g_free (request);
}

static void
invoice_delete_response_cb (GObject *source, GAsyncResult *result,
                            gpointer user_data)
{
    InvoiceDeleteRequest *request = user_data;
    GError *error = NULL;
    gint response;
    GtkWidget *window;
    QofBook *book;
    GncInvoice *invoice;
    InvoiceWindow *iw;
    GncEntry *entry;

    response = gtk_alert_dialog_choose_finish (GTK_ALERT_DIALOG (source),
                                               result, &error);
    window = g_weak_ref_get (&request->window);
    book = gnc_get_current_book ();
    if (error || response != 1 || !window || !book ||
        !guid_equal (&request->book_guid,
                     qof_instance_get_guid (QOF_INSTANCE (book))))
        goto cleanup;

    invoice = gncInvoiceLookup (book, &request->invoice_guid);
    iw = invoice ? gnc_plugin_page_invoice_get_window (invoice) : NULL;
    if (!iw || iw_get_window (iw) != window || !iw->ledger)
        goto cleanup;

    entry = gnc_entry_ledger_get_current_entry (iw->ledger);
    if (entry && guid_equal (gncEntryGetGUID (entry), &request->entry_guid))
        gnc_entry_ledger_delete_current_entry (iw->ledger);

cleanup:
    g_clear_error (&error);
    g_clear_object (&window);
    invoice_delete_request_free (request);
}

void
gnc_invoice_window_deleteCB (GtkWidget *widget, gpointer data)
{
    InvoiceWindow *iw = data;
    GncEntry *entry;
    GtkWidget *window;
    InvoiceDeleteRequest *request;
    const char *message = _("Are you sure you want to delete the selected entry?");
    const char *order_warn = _("This entry is attached to an order and "
                               "will be deleted from that as well!");
    const char *buttons[] = { _("_Cancel"), _("_Delete"), NULL };
    char *detail;
    GtkAlertDialog *alert;

    if (!iw || !iw->ledger || !(window = iw_get_window (iw)))
        return;

    entry = gnc_entry_ledger_get_current_entry (iw->ledger);
    if (!entry)
    {
        gnc_entry_ledger_cancel_cursor_changes (iw->ledger);
        return;
    }

    if (entry == gnc_entry_ledger_get_blank_entry (iw->ledger))
    {
        gnc_entry_ledger_cancel_cursor_changes (iw->ledger);
        return;
    }

    request = g_new0 (InvoiceDeleteRequest, 1);
    request->book_guid = *qof_instance_get_guid (QOF_INSTANCE (iw->book));
    request->invoice_guid = iw->invoice_guid;
    request->entry_guid = *gncEntryGetGUID (entry);
    g_weak_ref_init (&request->window, window);

    detail = gncEntryGetOrder (entry) ? g_strdup (order_warn) : NULL;
    alert = gtk_alert_dialog_new ("%s", message);
    gtk_alert_dialog_set_detail (alert, detail);
    gtk_alert_dialog_set_buttons (alert, buttons);
    gtk_alert_dialog_set_cancel_button (alert, 0);
    gtk_alert_dialog_set_default_button (alert, 0);
    gtk_alert_dialog_choose (alert, GTK_WINDOW (window), NULL,
                             invoice_delete_response_cb, request);
    g_object_unref (alert);
    g_free (detail);
    (void)widget;
}

void
gnc_invoice_window_duplicateCB (GtkWidget *widget, gpointer data)
{
    InvoiceWindow *iw = data;

    if (!iw || !iw->ledger)
        return;

    gnc_entry_ledger_duplicate_current_entry (iw->ledger);
}

static void
gnc_invoice_window_blank_finished (GncEntryLedger *ledger, gboolean completed,
                                   gpointer user_data)
{
    InvoiceWindow *iw = user_data;
    VirtualCellLocation vcell_loc;
    GncEntry *blank;

    (void)ledger;
    if (!completed)
        return;

    blank = gnc_entry_ledger_get_blank_entry (iw->ledger);
    if (blank && gnc_entry_ledger_get_entry_virt_loc (iw->ledger, blank, &vcell_loc))
        gnucash_register_goto_virt_cell (iw->reg, vcell_loc);
}

void
gnc_invoice_window_blankCB (GtkWidget *widget, gpointer data)
{
    InvoiceWindow *iw = data;

    (void)widget;
    if (iw && iw->ledger)
        gnc_entry_ledger_commit_entry_async (iw->ledger,
                                             gnc_invoice_window_blank_finished, iw);
}

typedef void (*InvoiceReportTemplateCallback) (GtkWindow *parent,
                                               char *report_guid,
                                               gpointer user_data);

typedef struct
{
    GWeakRef parent;
    gboolean has_parent;
    gulong parent_destroy_handler;
    GtkWindow *dialog;
    GtkWidget *combo;
    GtkProgressBar *progress_bar;
    GtkEventController *key_controller;
    guint timeout_source;
    gdouble timeout;
    char *default_guid;
    InvoiceReportTemplateCallback completed;
    gpointer user_data;
    gboolean done;
} InvoiceReportTemplateRequest;

static void invoice_report_template_complete (
    InvoiceReportTemplateRequest *request, gboolean accepted);

static void
invoice_report_template_stop_timeout (InvoiceReportTemplateRequest *request)
{
    if (!request->timeout_source)
        return;

    g_source_remove (request->timeout_source);
    request->timeout_source = 0;
}

static void
invoice_report_template_request_free (InvoiceReportTemplateRequest *request)
{
    GtkWindow *parent = g_weak_ref_get (&request->parent);

    if (parent && request->parent_destroy_handler)
        g_signal_handler_disconnect (parent, request->parent_destroy_handler);
    g_clear_object (&parent);
    g_weak_ref_clear (&request->parent);
    g_free (request->default_guid);
    g_free (request);
}

static void
invoice_report_template_destroy_dialog (InvoiceReportTemplateRequest *request)
{
    GtkWindow *dialog = g_steal_pointer (&request->dialog);

    if (!dialog)
        return;

    if (request->combo)
        g_signal_handlers_disconnect_by_data (request->combo, request);
    if (request->key_controller)
        g_signal_handlers_disconnect_by_data (request->key_controller, request);
    g_signal_handlers_disconnect_by_data (dialog, request);
    gtk_window_destroy (dialog);
    g_object_unref (dialog);
}

static void
invoice_report_template_complete (InvoiceReportTemplateRequest *request,
                                  gboolean accepted)
{
    GtkWindow *parent;
    char *report_guid = NULL;

    if (!request || request->done)
        return;

    request->done = TRUE;
    invoice_report_template_stop_timeout (request);
    if (accepted)
    {
        if (request->combo)
            report_guid = gnc_report_combo_get_active_guid (
                GNC_REPORT_COMBO (request->combo));
        else
            report_guid = g_steal_pointer (&request->default_guid);
    }

    invoice_report_template_destroy_dialog (request);
    parent = g_weak_ref_get (&request->parent);
    if (request->has_parent && !parent)
        g_clear_pointer (&report_guid, g_free);
    request->completed (parent, report_guid, request->user_data);
    g_clear_object (&parent);
    invoice_report_template_request_free (request);
}

static void
invoice_report_template_parent_destroyed_cb (
    GtkWidget *widget, InvoiceReportTemplateRequest *request)
{
    (void)widget;
    request->parent_destroy_handler = 0;
    invoice_report_template_complete (request, FALSE);
}

static void
invoice_report_template_accept_clicked_cb (GtkButton *button,
                                           InvoiceReportTemplateRequest *request)
{
    (void)button;
    invoice_report_template_complete (request, TRUE);
}

static void
invoice_report_template_cancel_clicked_cb (GtkButton *button,
                                           InvoiceReportTemplateRequest *request)
{
    (void)button;
    invoice_report_template_complete (request, FALSE);
}

static gboolean
invoice_report_template_close_request_cb (
    GtkWindow *dialog, InvoiceReportTemplateRequest *request)
{
    (void)dialog;
    invoice_report_template_complete (request, FALSE);
    return TRUE;
}

static void
invoice_report_template_destroy_cb (
    GtkWidget *widget, InvoiceReportTemplateRequest *request)
{
    (void)widget;
    if (!request->done)
        g_clear_object (&request->dialog);
    invoice_report_template_complete (request, FALSE);
}

static gboolean
invoice_report_template_timeout_cb (gpointer user_data)
{
    InvoiceReportTemplateRequest *request = user_data;
    gdouble fraction = gtk_progress_bar_get_fraction (request->progress_bar);
    gdouble step = 0.1 / request->timeout;

    fraction -= step;
    if (fraction < step)
    {
        request->timeout_source = 0;
        invoice_report_template_complete (request, TRUE);
        return G_SOURCE_REMOVE;
    }

    gtk_progress_bar_set_fraction (request->progress_bar, fraction);
    return G_SOURCE_CONTINUE;
}

static gboolean
invoice_report_template_idle_cb (gpointer user_data)
{
    InvoiceReportTemplateRequest *request = user_data;

    request->timeout_source = 0;
    invoice_report_template_complete (request, TRUE);
    return G_SOURCE_REMOVE;
}

static void
invoice_report_template_combo_interacted_cb (GncReportCombo *combo,
                                              gpointer user_data)
{
    (void)combo;
    invoice_report_template_stop_timeout (user_data);
}

static gboolean
invoice_report_template_key_pressed_cb (
    GtkEventControllerKey *controller, guint keyval, guint keycode,
    GdkModifierType state, gpointer user_data)
{
    (void)controller;
    (void)keyval;
    (void)keycode;
    (void)state;
    invoice_report_template_stop_timeout (user_data);
    return FALSE;
}

static void
invoice_report_template_combo_changed_cb (GncReportCombo *combo,
                                          gpointer user_data)
{
    (void)combo;
    invoice_report_template_stop_timeout (user_data);
}

static InvoiceReportTemplateRequest *
invoice_report_template_request_new (
    GtkWindow *parent, InvoiceReportTemplateCallback completed, gpointer user_data)
{
    InvoiceReportTemplateRequest *request = g_new0 (
        InvoiceReportTemplateRequest, 1);

    request->completed = completed;
    request->user_data = user_data;
    request->has_parent = parent != NULL;
    g_weak_ref_init (&request->parent, parent);
    if (parent)
        request->parent_destroy_handler = g_signal_connect (
            parent, "destroy",
            G_CALLBACK (invoice_report_template_parent_destroyed_cb), request);
    return request;
}

static void
use_default_report_template_or_change_async (
    GtkWindow *parent, InvoiceReportTemplateCallback completed, gpointer user_data)
{
    InvoiceReportTemplateRequest *request;
    QofBook *book = gnc_get_current_book ();
    GtkBuilder *builder;
    GtkWidget *dialog;
    GtkWidget *ok_button;
    GtkWidget *cancel_button;
    GtkWidget *report_combo_hbox;
    GtkWidget *progress_bar;
    GtkWidget *label;
    GtkWidget *combo;
    gchar *report_guid;
    gchar *report_name;
    gboolean warning_visible;
    gdouble timeout;

    g_return_if_fail (completed != NULL);

    request = invoice_report_template_request_new (parent, completed, user_data);
    timeout = qof_book_get_default_invoice_report_timeout (book);
    request->timeout = timeout;
    combo = gnc_default_invoice_report_combo (
        "gnc:custom-report-invoice-template-guids");
    report_name = qof_book_get_default_invoice_report_name (book);
    report_guid = gnc_get_default_invoice_print_report ();
    gnc_report_combo_set_active (GNC_REPORT_COMBO (combo), report_guid,
                                 report_name);
    g_free (report_guid);
    g_free (report_name);

    warning_visible = gnc_report_combo_is_warning_visible_for_active (
        GNC_REPORT_COMBO (combo));
    if (timeout == 0 && !warning_visible)
    {
        g_object_unref (combo);
        request->default_guid = gnc_get_default_invoice_print_report ();
        request->timeout_source = g_idle_add (
            invoice_report_template_idle_cb, request);
        return;
    }

    builder = gtk_builder_new ();
    if (!gnc_builder_add_from_file (builder, "dialog-invoice.glade",
                                    "invoice_print_dialog"))
    {
        g_object_unref (builder);
        g_object_unref (combo);
        invoice_report_template_complete (request, FALSE);
        return;
    }

    dialog = GTK_WIDGET (gtk_builder_get_object (builder, "invoice_print_dialog"));
    ok_button = GTK_WIDGET (gtk_builder_get_object (builder, "ok_button"));
    cancel_button = GTK_WIDGET (gtk_builder_get_object (builder, "cancel_button"));
    report_combo_hbox = GTK_WIDGET (
        gtk_builder_get_object (builder, "report_combo_hbox"));
    progress_bar = GTK_WIDGET (gtk_builder_get_object (builder, "progress_bar"));
    label = GTK_WIDGET (gtk_builder_get_object (builder, "label"));
    if (!dialog || !ok_button || !cancel_button || !report_combo_hbox || !progress_bar || !label)
    {
        g_object_unref (builder);
        g_object_unref (combo);
        invoice_report_template_complete (request, FALSE);
        return;
    }

    request->dialog = g_object_ref (GTK_WINDOW (dialog));
    request->combo = combo;
    request->progress_bar = GTK_PROGRESS_BAR (progress_bar);
    if (parent)
        gtk_window_set_transient_for (request->dialog, parent);
    gtk_window_set_modal (request->dialog, TRUE);
    gtk_window_set_default_widget (request->dialog, ok_button);
    gtk_box_append (GTK_BOX (report_combo_hbox), combo);
    gtk_widget_grab_focus (ok_button);
    gtk_progress_bar_set_fraction (request->progress_bar, 1);

    request->key_controller = gtk_event_controller_key_new ();
    gtk_widget_add_controller (GTK_WIDGET (request->dialog),
                               request->key_controller);
    g_signal_connect (ok_button, "clicked",
                      G_CALLBACK (invoice_report_template_accept_clicked_cb), request);
    g_signal_connect (cancel_button, "clicked",
                      G_CALLBACK (invoice_report_template_cancel_clicked_cb), request);
    g_signal_connect (request->dialog, "close-request",
                      G_CALLBACK (invoice_report_template_close_request_cb), request);
    g_signal_connect (request->dialog, "destroy",
                      G_CALLBACK (invoice_report_template_destroy_cb), request);
    g_signal_connect (combo, "changed",
                      G_CALLBACK (invoice_report_template_combo_changed_cb), request);
    g_signal_connect (request->key_controller, "key-pressed",
                      G_CALLBACK (invoice_report_template_key_pressed_cb), request);
    g_signal_connect (combo, "interacted",
                      G_CALLBACK (invoice_report_template_combo_interacted_cb), request);

    if (warning_visible)
    {
        gtk_label_set_text (
            GTK_LABEL (label),
            _("Choose a different report template or Printable Invoice will be used"));
        gtk_widget_set_visible (progress_bar, FALSE);
    }
    else
        request->timeout_source = g_timeout_add (
            100, invoice_report_template_timeout_cb, request);

    g_object_unref (builder);
    gtk_window_present (request->dialog);
}

static GncPluginPage *
gnc_invoice_window_print_invoice (GtkWindow *parent, GncInvoice *invoice,
                                  const gchar *report_guid)
{
    SCM func, arg, arg2;
    SCM args = SCM_EOL;
    SCM is_invoice_guid;
    SCM scm_guid;
    int report_id;
    const gchar *use_report_guid = NULL;
    GncPluginPage *reportPage = NULL;

    g_return_val_if_fail (invoice, NULL);

    is_invoice_guid = scm_c_eval_string ("gnc:report-is-invoice-report?");
    scm_guid = scm_from_utf8_string (report_guid);

    if (scm_is_false (scm_call_1 (is_invoice_guid, scm_guid)))
        use_report_guid = gnc_get_builtin_default_invoice_print_report (); // fallback if the option lookup failed
    else
        use_report_guid = report_guid;

    func = scm_c_eval_string ("gnc:invoice-report-create");
    g_return_val_if_fail (scm_is_procedure (func), NULL);

    arg = SWIG_NewPointerObj(invoice, SWIG_TypeQuery("_p__gncInvoice"), 0);
    arg2 = scm_from_utf8_string (use_report_guid);
    args = scm_cons2 (arg, arg2, args);

    /* scm_gc_protect_object(func); */

    arg = scm_apply (func, args, SCM_EOL);
    g_return_val_if_fail (scm_is_exact (arg), NULL);
    report_id = scm_to_int (arg);

    /* scm_gc_unprotect_object(func); */
    if (report_id >= 0)
    {
        reportPage = gnc_plugin_page_report_new (report_id);
        gnc_main_window_open_page (GNC_MAIN_WINDOW (parent), reportPage);
    }
    return reportPage;
}

static gboolean
equal_fn (gpointer find_data, gpointer elt_data)
{
    return (find_data && (find_data == elt_data));
}

typedef struct
{
    InvoiceWindow *iw;
    GWeakRef window;
    gulong destroy_handler;
    QofBook *book;
    GncGUID invoice_guid;
} InvoicePrintRequest;

static void
invoice_print_request_destroyed (GtkWidget *widget, InvoicePrintRequest *request)
{
    (void)widget;
    request->iw = NULL;
    request->destroy_handler = 0;
}

static void
invoice_print_request_free (InvoicePrintRequest *request)
{
    GtkWidget *window = g_weak_ref_get (&request->window);

    if (window && request->destroy_handler)
        g_signal_handler_disconnect (window, request->destroy_handler);
    g_clear_object (&window);
    g_weak_ref_clear (&request->window);
    g_free (request);
}

static InvoicePrintRequest *
invoice_print_request_new (InvoiceWindow *iw)
{
    GtkWidget *window;
    InvoicePrintRequest *request;

    if (!iw || !iw_get_invoice (iw))
        return NULL;

    window = iw_get_window (iw);
    if (!window)
        return NULL;

    request = g_new0 (InvoicePrintRequest, 1);
    request->iw = iw;
    request->book = iw->book;
    request->invoice_guid = iw->invoice_guid;
    g_weak_ref_init (&request->window, window);
    request->destroy_handler = g_signal_connect (
        window, "destroy", G_CALLBACK (invoice_print_request_destroyed), request);
    return request;
}

static void
invoice_print_template_finished (GtkWindow *parent, char *report_guid,
                                 gpointer user_data)
{
    InvoicePrintRequest *request = user_data;
    GncInvoice *invoice;

    if (parent && report_guid && request->iw &&
        !qof_book_shutting_down (request->book))
    {
        invoice = gncInvoiceLookup (request->book, &request->invoice_guid);
        if (invoice && iw_get_invoice (request->iw) == invoice)
        {
            request->iw->reportPage = gnc_invoice_window_print_invoice (
                parent, invoice, report_guid);
            if (request->iw->reportPage)
                gnc_main_window_open_page (
                    GNC_MAIN_WINDOW (request->iw->dialog), request->iw->reportPage);
        }
    }

    g_free (report_guid);
    invoice_print_request_free (request);
}

/* From the invoice editor, open the invoice report. This will reuse the
   invoice report if generated from the current invoice editor. Note the
   link is lost when GnuCash is restarted. This link may be restored
   by: scan the current session tabs, identify reports, checking
   whereby report's report-type matches an invoice report, and the
   report's invoice option value matches the current invoice. */
void
gnc_invoice_window_printCB (GtkWindow *parent, gpointer data)
{
    InvoiceWindow *iw = data;
    InvoicePrintRequest *request;

    if (!iw)
        return;

    if (gnc_find_first_gui_component (WINDOW_REPORT_CM_CLASS, equal_fn,
                                      iw->reportPage))
    {
        gnc_plugin_page_report_reload (GNC_PLUGIN_PAGE_REPORT (iw->reportPage));
        return;
    }

    request = invoice_print_request_new (iw);
    if (!request)
        return;

    use_default_report_template_or_change_async (
        parent, invoice_print_template_finished, request);
}

struct post_invoice_params
{
    time64 ddue;            /* Due date */
    time64 postdate;        /* Date posted */
    char *memo;             /* Memo for posting transaction */
    Account *acc;           /* Account to post to */
    gboolean accumulate;    /* Whether to accumulate splits */
};

typedef struct
{
    InvoiceWindow *iw;
    GWeakRef window;
    gulong destroy_handler;
    QofBook *book;
    GncGUID invoice_guid;
    GList *invoice_guids;
    gboolean multiple;
} InvoicePostRequest;

static void gnc_invoice_post_after_ledger (
    InvoiceWindow *iw, struct post_invoice_params *post_params);
static void gnc_invoice_post (InvoiceWindow *iw,
                              struct post_invoice_params *post_params);

static void
post_invoice_params_clear (struct post_invoice_params *post_params)
{
    g_clear_pointer (&post_params->memo, g_free);
}

typedef struct
{
    InvoiceWindow *iw;
    GWeakRef window;
    gulong destroy_handler;
    QofBook *book;
    GncGUID invoice_guid;
    struct post_invoice_params post_params;
    gboolean has_post_params;
} InvoicePostLedgerRequest;

static void
invoice_post_ledger_request_destroyed (GtkWidget *widget,
                                       InvoicePostLedgerRequest *request)
{
    (void)widget;
    request->iw = NULL;
    request->destroy_handler = 0;
}

static void
invoice_post_ledger_request_free (InvoicePostLedgerRequest *request)
{
    GtkWidget *window = g_weak_ref_get (&request->window);

    if (window && request->destroy_handler)
        g_signal_handler_disconnect (window, request->destroy_handler);
    g_clear_object (&window);
    g_weak_ref_clear (&request->window);
    post_invoice_params_clear (&request->post_params);
    g_free (request);
}

static InvoicePostLedgerRequest *
invoice_post_ledger_request_new (InvoiceWindow *iw,
                                 const struct post_invoice_params *post_params)
{
    GtkWidget *window;
    InvoicePostLedgerRequest *request;

    if (!iw || !iw->ledger || !iw_get_invoice (iw) ||
        !(window = iw_get_window (iw)))
        return NULL;

    request = g_new0 (InvoicePostLedgerRequest, 1);
    request->iw = iw;
    request->book = iw->book;
    request->invoice_guid = iw->invoice_guid;
    request->has_post_params = post_params != NULL;
    if (post_params)
    {
        request->post_params = *post_params;
        request->post_params.memo = g_strdup (post_params->memo);
    }
    g_weak_ref_init (&request->window, window);
    request->destroy_handler = g_signal_connect (
        window, "destroy", G_CALLBACK (invoice_post_ledger_request_destroyed), request);
    return request;
}

static gboolean
invoice_post_ledger_request_is_current (const InvoicePostLedgerRequest *request)
{
    GncInvoice *invoice;

    if (!request->iw || qof_book_shutting_down (request->book))
        return FALSE;

    invoice = gncInvoiceLookup (request->book, &request->invoice_guid);
    return invoice && iw_get_invoice (request->iw) == invoice;
}

static void
invoice_post_ledger_finished (GncEntryLedger *ledger, gboolean completed,
                              gpointer user_data)
{
    InvoicePostLedgerRequest *request = user_data;

    (void)ledger;
    if (completed && invoice_post_ledger_request_is_current (request))
        gnc_invoice_post_after_ledger (
            request->iw, request->has_post_params ? &request->post_params : NULL);
    invoice_post_ledger_request_free (request);
}

static void
gnc_invoice_post (InvoiceWindow *iw, struct post_invoice_params *post_params)
{
    InvoicePostLedgerRequest *request = invoice_post_ledger_request_new (iw, post_params);

    if (!request)
        return;

    gnc_entry_ledger_check_close_async (
        iw_get_window (iw), iw->ledger, invoice_post_ledger_finished, request);
}

static void
invoice_post_request_destroyed (GtkWidget *widget, InvoicePostRequest *request)
{
    (void)widget;
    request->iw = NULL;
    request->destroy_handler = 0;
}

static void
invoice_post_request_free (InvoicePostRequest *request)
{
    GtkWidget *window = g_weak_ref_get (&request->window);

    if (window && request->destroy_handler)
        g_signal_handler_disconnect (window, request->destroy_handler);
    g_clear_object (&window);
    g_weak_ref_clear (&request->window);
    g_list_free_full (request->invoice_guids, g_free);
    g_free (request);
}

static InvoicePostRequest *
invoice_post_request_new (InvoiceWindow *iw, GList *invoice_guids)
{
    GtkWidget *window;
    InvoicePostRequest *request;

    if (!iw || !iw_get_invoice (iw))
        return NULL;

    window = iw_get_window (iw);
    if (!window)
        return NULL;

    request = g_new0 (InvoicePostRequest, 1);
    request->iw = iw;
    request->book = iw->book;
    request->invoice_guid = iw->invoice_guid;
    request->invoice_guids = invoice_guids;
    request->multiple = invoice_guids != NULL;
    g_weak_ref_init (&request->window, window);
    request->destroy_handler = g_signal_connect (
        window, "destroy", G_CALLBACK (invoice_post_request_destroyed), request);
    return request;
}

static gboolean
invoice_post_request_all_unposted (const InvoicePostRequest *request)
{
    GList *node;

    for (node = request->invoice_guids; node; node = node->next)
    {
        GncInvoice *invoice = gncInvoiceLookup (request->book, node->data);

        if (!invoice || gncInvoiceIsPosted (invoice))
            return FALSE;
    }

    return TRUE;
}
static gboolean
invoice_post_request_single_valid (const InvoicePostRequest *request)
{
    GncInvoice *invoice;

    if (!request->iw)
        return FALSE;

    invoice = gncInvoiceLookup (request->book, &request->invoice_guid);
    return invoice && !gncInvoiceIsPosted (invoice) &&
           iw_get_invoice (request->iw) == invoice;
}


static void
invoice_post_request_finished (GObject *source, GAsyncResult *result,
                               gpointer user_data)
{
    InvoicePostRequest *request = user_data;
    struct post_invoice_params post_params = { 0 };
    GError *error = NULL;
    GtkWidget *window = g_weak_ref_get (&request->window);

    (void)source;
    if (!gnc_dialog_dates_acct_question_parented_finish (
            result, &post_params.ddue, &post_params.postdate, &post_params.memo,
            &post_params.acc, &post_params.accumulate, &error))
        goto cleanup;

    if (!window || qof_book_shutting_down (request->book))
        goto cleanup;

    if (request->multiple)
    {
        GList *node;

        if (!invoice_post_request_all_unposted (request))
        {
            gnc_error_dialog (GTK_WINDOW (window), "%s",
                              _("One or more selected invoices have changed. "
                                "Re-check your selection."));
            goto cleanup;
        }

        gnc_suspend_gui_refresh ();
        for (node = request->invoice_guids; node; node = node->next)
        {
            GncInvoice *invoice = gncInvoiceLookup (request->book, node->data);
            InvoiceWindow *iw = gnc_ui_invoice_edit (GTK_WINDOW (window), invoice);

            if (iw)
                gnc_invoice_post (iw, &post_params);
        }
        gnc_resume_gui_refresh ();
    }
    else if (invoice_post_request_single_valid (request))
    {
        gnc_invoice_post (request->iw, &post_params);
    }

cleanup:
    g_clear_error (&error);
    g_clear_object (&window);
    post_invoice_params_clear (&post_params);
    invoice_post_request_free (request);
}

static gboolean
invoice_post_request_start (InvoicePostRequest *request, InvoiceWindow *iw,
                            const char *message)
{
    GncInvoice *invoice;
    GList *acct_types;
    GList *acct_commodities;
    QofInstance *owner_inst;
    EntryList *entries;
    EntryList *entries_iter;
    GncGUID *guid = NULL;
    Account *acc;
    time64 ddue;
    time64 postdate;
    gboolean accumulate;
    GtkWidget *window;

    invoice = iw_get_invoice (iw);
    if (!invoice)
        return FALSE;

    acct_types = gncOwnerGetAccountTypesList (&iw->owner);
    acct_commodities = gncOwnerGetCommoditiesList (&iw->owner);

    entries = gncInvoiceGetEntries (invoice);
    postdate = gnc_time (NULL);
    if (entries && ((gncInvoiceGetOwnerType (invoice) == GNC_OWNER_VENDOR) ||
                    (gncInvoiceGetOwnerType (invoice) == GNC_OWNER_EMPLOYEE)))
    {
        postdate = gncEntryGetDate (entries->data);
        for (entries_iter = entries; entries_iter;
             entries_iter = g_list_next (entries_iter))
        {
            time64 entrydate = gncEntryGetDate (entries_iter->data);

            if (entrydate > postdate)
                postdate = entrydate;
        }
    }

    ddue = postdate;
    owner_inst = qofOwnerGetOwner (gncOwnerGetEndOwner (&iw->owner));
    qof_instance_get (owner_inst, "invoice-last-posted-account", &guid, NULL);
    acc = xaccAccountLookup (guid, iw->book);
    accumulate = gnc_prefs_get_bool (GNC_PREFS_GROUP_INVOICE,
                                     GNC_PREF_ACCUM_SPLITS);
    window = g_weak_ref_get (&request->window);
    if (!window)
    {
        g_list_free (acct_types);
        g_list_free (acct_commodities);
        return FALSE;
    }

    gnc_dialog_dates_acct_question_parented_async (
        window, message, _("Due Date"), _("Post Date"), _("Post to Account"),
        _("Accumulate Splits?"), TRUE, TRUE, acct_types, acct_commodities,
        iw->book, iw->terms, ddue, postdate, NULL, acc, accumulate, NULL,
        invoice_post_request_finished, request);
    g_list_free (acct_commodities);
    g_object_unref (window);
    return TRUE;
}

static void
invoice_post_request (InvoiceWindow *iw, const char *message,
                      GList *invoice_guids)
{
    InvoicePostRequest *request = invoice_post_request_new (iw, invoice_guids);

    if (!request)
    {
        g_list_free_full (invoice_guids, g_free);
        return;
    }

    if (!invoice_post_request_start (request, iw, message))
        invoice_post_request_free (request);
}

typedef struct
{
    gnc_commodity *currency;
    gnc_numeric amount;
} InvoiceExchangeCurrency;

typedef struct
{
    GWeakRef window;
    InvoiceWindow *iw;
    QofBook *book;
    GncGUID invoice_guid;
    GncGUID account_guid;
    time64 ddue;
    time64 postdate;
    gboolean accumulate;
    gboolean is_customer;
    gchar *memo;
    GList *currencies;
    GList *current;
    gnc_numeric exch_rate;
    gulong destroy_handler;
} InvoiceExchangeRequest;

static void invoice_exchange_request_next (InvoiceExchangeRequest *request);

static void
invoice_exchange_request_destroyed (GtkWidget *widget, InvoiceExchangeRequest *request)
{
    (void)widget;
    request->iw = NULL;
    request->destroy_handler = 0;
}

static GncInvoice *
invoice_exchange_request_invoice (const InvoiceExchangeRequest *request)
{
    if (!request->iw || qof_book_shutting_down (request->book))
        return NULL;
    GncInvoice *invoice = gncInvoiceLookup (request->book, &request->invoice_guid);
    return invoice && iw_get_invoice (request->iw) == invoice ? invoice : NULL;
}

static void
invoice_exchange_request_free (InvoiceExchangeRequest *request)
{
    if (!request)
        return;
    GtkWidget *window = g_weak_ref_get (&request->window);
    if (window && request->destroy_handler)
        g_signal_handler_disconnect (window, request->destroy_handler);
    g_clear_object (&window);
    g_weak_ref_clear (&request->window);
    g_list_free_full (request->currencies, g_free);
    g_free (request->memo);
    g_free (request);
}

static void
invoice_exchange_request_complete (InvoiceExchangeRequest *request, gboolean post_ok)
{
    GtkWidget *window = g_weak_ref_get (&request->window);
    GncInvoice *invoice = invoice_exchange_request_invoice (request);
    Account *account = xaccAccountLookup (&request->account_guid, request->book);

    if (invoice && account && post_ok)
    {
        QofInstance *owner_inst = qofOwnerGetOwner (gncOwnerGetEndOwner (&request->iw->owner));
        const GncGUID *guid = qof_instance_get_guid (QOF_INSTANCE (account));
        qof_begin_edit (owner_inst);
        qof_instance_set (owner_inst, "invoice-last-posted-account", guid, NULL);
        qof_commit_edit (owner_inst);
        gboolean auto_pay = gnc_prefs_get_bool (request->is_customer ? GNC_PREFS_GROUP_INVOICE :
                                              GNC_PREFS_GROUP_BILL, GNC_PREF_AUTO_PAY);
        gncInvoicePostToAccount (invoice, account, request->postdate, request->ddue,
                                 request->memo, request->accumulate, auto_pay);
    }
    if (invoice)
        gncInvoiceCommitEdit (invoice);
    gnc_resume_gui_refresh ();

    if (window && invoice && request->iw)
    {
        if (post_ok)
        {
            request->iw->dialog_type = VIEW_INVOICE;
            gnc_entry_ledger_set_readonly (request->iw->ledger, TRUE);
        }
        else
            gnc_info_dialog (GTK_WINDOW (window), "%s",
                             _("The post action was canceled because not all exchange rates were given."));
        gnc_invoice_update_window (request->iw, NULL);
        gnc_table_refresh_gui (gnc_entry_ledger_get_table (request->iw->ledger), FALSE);
    }
    g_clear_object (&window);
    invoice_exchange_request_free (request);
}

static void
invoice_exchange_request_finished_cb (gboolean completed, gpointer user_data)
{
    InvoiceExchangeRequest *request = user_data;
    GncInvoice *invoice = invoice_exchange_request_invoice (request);
    InvoiceExchangeCurrency *item = request->current->data;
    if (!completed || !invoice)
    {
        invoice_exchange_request_complete (request, FALSE);
        return;
    }

    gnc_numeric rate = request->exch_rate;
    if (!gnc_numeric_zero_p (rate))
        rate = gnc_numeric_div ((gnc_numeric){1, 1}, rate, GNC_DENOM_AUTO,
                                GNC_HOW_RND_ROUND_HALF_UP);
    GNCPrice *convprice = gnc_price_create (request->book);
    gnc_price_begin_edit (convprice);
    gnc_price_set_commodity (convprice, item->currency);
    gnc_price_set_currency (convprice, gncInvoiceGetCurrency (invoice));
    gnc_price_set_time64 (convprice, request->postdate);
    gnc_price_set_source (convprice, PRICE_SOURCE_TEMP);
    gnc_price_set_typestr (convprice, PRICE_TYPE_LAST);
    gnc_price_set_value (convprice, rate);
    gncInvoiceAddPrice (invoice, convprice);
    gnc_price_commit_edit (convprice);
    request->current = request->current->next;
    invoice_exchange_request_next (request);
}

static void
invoice_exchange_request_next (InvoiceExchangeRequest *request)
{
    GtkWidget *window = g_weak_ref_get (&request->window);
    GncInvoice *invoice = invoice_exchange_request_invoice (request);
    if (!window || !invoice)
    {
        g_clear_object (&window);
        invoice_exchange_request_complete (request, FALSE);
        return;
    }
    if (!request->current)
    {
        g_object_unref (window);
        invoice_exchange_request_complete (request, TRUE);
        return;
    }

    Account *account = xaccAccountLookup (&request->account_guid, request->book);
    InvoiceExchangeCurrency *item = request->current->data;
    if (!account)
    {
        g_object_unref (window);
        invoice_exchange_request_complete (request, FALSE);
        return;
    }
    XferDialog *xfer = gnc_xfer_dialog (window, account);
    request->exch_rate = gnc_numeric_zero ();
    { GNCPrice *convprice = gncInvoiceGetPrice (invoice, item->currency); if (convprice)
    {
        request->exch_rate = gnc_price_get_value (convprice);
        if (!gnc_numeric_zero_p (request->exch_rate))
            request->exch_rate = gnc_numeric_div ((gnc_numeric){1, 1}, request->exch_rate,
                                                   GNC_DENOM_AUTO, GNC_HOW_RND_ROUND_HALF_UP);
    } }
    gnc_xfer_dialog_is_exchange_dialog (xfer, &request->exch_rate);
    gnc_xfer_dialog_select_to_currency (xfer, item->currency);
    gnc_xfer_dialog_set_date (xfer, request->postdate);
    gnc_xfer_dialog_set_amount (xfer, gnc_numeric_zero_p (item->amount)
                                       ? (gnc_numeric){1, 1} : item->amount);
    gnc_xfer_dialog_set_from_show_button_active (xfer, FALSE);
    gnc_xfer_dialog_set_to_show_button_active (xfer, FALSE);
    gnc_xfer_dialog_hide_from_account_tree (xfer);
    gnc_xfer_dialog_hide_to_account_tree (xfer);
    gnc_xfer_dialog_set_price_edit (xfer, request->exch_rate);
    gnc_xfer_dialog_run_async (xfer, invoice_exchange_request_finished_cb, request);
    g_object_unref (window);
}

static void
gnc_invoice_post_after_ledger (InvoiceWindow *iw,
                               struct post_invoice_params *post_params)
{
    if (!gnc_invoice_window_verify_ok (iw))
        return;
    GncInvoice *invoice = iw_get_invoice (iw);
    if (!invoice)
        return;
    if (!gncInvoiceGetEntries (invoice))
    {
        gnc_error_dialog (GTK_WINDOW (iw_get_window (iw)), "%s",
                          _("The Invoice must have at least one Entry."));
        return;
    }
    if (!post_params)
    {
        invoice_post_request (iw, _("Do you really want to post the invoice?"), NULL);
        return;
    }

    InvoiceExchangeRequest *request = g_new0 (InvoiceExchangeRequest, 1);
    request->iw = iw;
    request->book = iw->book;
    request->invoice_guid = iw->invoice_guid;
    request->account_guid = *xaccAccountGetGUID (post_params->acc);
    request->ddue = post_params->ddue;
    request->postdate = post_params->postdate;
    request->accumulate = post_params->accumulate;
    request->is_customer = gncInvoiceGetOwnerType (invoice) == GNC_OWNER_CUSTOMER;
    request->memo = g_strdup (post_params->memo);
    g_weak_ref_init (&request->window, iw_get_window (iw));
    request->destroy_handler = g_signal_connect (iw_get_window (iw), "destroy",
                                                 G_CALLBACK (invoice_exchange_request_destroyed), request);

    gnc_suspend_gui_refresh ();
    gncInvoiceBeginEdit (invoice);
    gnc_invoice_window_ok_save (iw);
    gncInvoiceSetCurrency (invoice, gncOwnerGetCurrency (gncInvoiceGetOwner (invoice)));
    GHashTable *foreign_currs = gncInvoiceGetForeignCurrencies (invoice);
    GHashTableIter iter;
    gpointer key, value;
    g_hash_table_iter_init (&iter, foreign_currs);
    while (g_hash_table_iter_next (&iter, &key, &value))
    {
        InvoiceExchangeCurrency *item = g_new0 (InvoiceExchangeCurrency, 1);
        item->currency = (gnc_commodity*)key;
        item->amount = *(gnc_numeric*)value;
        request->currencies = g_list_append (request->currencies, item);
    }
    g_hash_table_unref (foreign_currs);
    request->current = request->currencies;
    if (request->current)
        gnc_info_dialog (GTK_WINDOW (iw_get_window (iw)), "%s",
                         _("One or more of the entries are for accounts different from the invoice/bill currency. You will be asked to enter a conversion rate for each."));
    invoice_exchange_request_next (request);
}
void
gnc_invoice_window_postCB (GtkWidget *unused_widget, gpointer data)
{
    InvoiceWindow *iw =data;
    gnc_invoice_post(iw, NULL);
}

void
gnc_invoice_window_unpostCB (GtkWidget *widget, gpointer data)
{
    InvoiceWindow *iw = data;

    (void)widget;
    if (!iw_get_invoice (iw))
        return;

    invoice_unpost_request (iw);
}

void
gnc_invoice_window_cut_cb (GtkWidget *widget, gpointer data)
{
    InvoiceWindow *iw = data;
    gnucash_register_cut_clipboard (iw->reg);
}

void gnc_invoice_window_copy_cb (GtkWidget *widget, gpointer data)
{
    InvoiceWindow *iw = data;
    gnucash_register_copy_clipboard (iw->reg);
}

void gnc_invoice_window_paste_cb (GtkWidget *widget, gpointer data)
{
    InvoiceWindow *iw = data;
    gnucash_register_paste_clipboard (iw->reg);
}

void gnc_invoice_window_new_invoice_cb (GtkWindow *parent, gpointer data)
{
    InvoiceWindow *iw = data;
    if (gncOwnerGetJob (&iw->job))
    {
        gnc_ui_invoice_new (parent, &iw->job, iw->book);
    }
    else
    {
        gnc_ui_invoice_new (parent, &iw->owner, iw->book);
    }
}

void gnc_business_call_owner_report (GtkWindow *parent, GncOwner *owner, Account *acc)
{
    gnc_business_call_owner_report_with_enddate (parent, owner, acc, INT64_MAX);
}

void gnc_business_call_owner_report_with_enddate (GtkWindow *parent,
                                                  GncOwner *owner,
                                                  Account *acc,
                                                  time64 enddate)
{
    int id;
    SCM args;
    SCM func;
    SCM arg;

    g_return_if_fail (owner);

    args = SCM_EOL;

    func = scm_c_eval_string ("gnc:owner-report-create-with-enddate");
    g_return_if_fail (scm_is_procedure (func));

    /* set the enddate */
    arg = (enddate != INT64_MAX) ? scm_from_int64 (enddate) : SCM_BOOL_F;
    args = scm_cons (arg, args);

    if (acc)
    {
        swig_type_info * qtype = SWIG_TypeQuery("_p_Account");
        g_return_if_fail (qtype);

        arg = SWIG_NewPointerObj(acc, qtype, 0);
        g_return_if_fail (arg != SCM_UNDEFINED);
        args = scm_cons (arg, args);
    }
    else
    {
        args = scm_cons (SCM_BOOL_F, args);
    }

    arg = SWIG_NewPointerObj(owner, SWIG_TypeQuery("_p__gncOwner"), 0);
    g_return_if_fail (arg != SCM_UNDEFINED);
    args = scm_cons (arg, args);

    /* Apply the function to the args */
    arg = scm_apply (func, args, SCM_EOL);
    g_return_if_fail (scm_is_exact (arg));
    id = scm_to_int (arg);

    if (id >= 0)
        reportWindow (id, parent);
}

void gnc_invoice_window_report_owner_cb (GtkWindow *parent, gpointer data)
{
    InvoiceWindow *iw = data;
    gnc_business_call_owner_report (parent, &iw->owner, NULL);
}

void gnc_invoice_window_payment_cb (GtkWindow *parent, gpointer data)
{
    InvoiceWindow *iw = data;
    GncInvoice *invoice = iw_get_invoice(iw);

    if (gncOwnerGetJob (&iw->job))
        gnc_ui_payment_new_with_invoice (parent, &iw->job, iw->book, invoice);
    else
        gnc_ui_payment_new_with_invoice (parent, &iw->owner, iw->book, invoice);
}

/* Sorting callbacks */

void
gnc_invoice_window_sort (InvoiceWindow *iw, invoice_sort_type_t sort_code)
{
    QofQuery *query = gnc_entry_ledger_get_query (iw->ledger);
    GSList *p1 = NULL, *p2 = NULL, *p3 = NULL, *standard;

    if (iw->last_sort == sort_code)
        return;

    standard = g_slist_prepend (NULL, QUERY_DEFAULT_SORT);

    switch (sort_code)
    {
    case INVSORT_BY_STANDARD:
        p1 = standard;
        break;
    case INVSORT_BY_DATE:
        p1 = g_slist_prepend (p1, ENTRY_DATE);
        p2 = standard;
        break;
    case INVSORT_BY_DATE_ENTERED:
        p1 = g_slist_prepend (p1, ENTRY_DATE_ENTERED);
        p2 = standard;
        break;
    case INVSORT_BY_DESC:
        p1 = g_slist_prepend (p1, ENTRY_DESC);
        p2 = standard;
        break;
    case INVSORT_BY_QTY:
        p1 = g_slist_prepend (p1, ENTRY_QTY);
        p2 = standard;
        break;
    case INVSORT_BY_PRICE:
        p1 = g_slist_prepend (p1, ((iw->owner.type == GNC_OWNER_CUSTOMER) ?
                                   ENTRY_IPRICE : ENTRY_BPRICE));
        p2 = standard;
        break;
    default:
        g_slist_free (standard);
        g_return_if_fail (FALSE);
        break;
    }

    qof_query_set_sort_order (query, p1, p2, p3);
    iw->last_sort = sort_code;
    gnc_entry_ledger_display_refresh (iw->ledger);
}

/* Window configuration callbacks */

void
gnc_invoice_window_active_toggled_cb (GtkWidget *widget, gpointer data)
{
    InvoiceWindow *iw = data;
    GncInvoice *invoice = iw_get_invoice(iw);

    if (!invoice) return;

    gncInvoiceSetActive (invoice,
                         gtk_check_button_get_active (GTK_CHECK_BUTTON (widget)));
}

static void
gnc_invoice_window_leave_notes_cb (GtkEventControllerFocus *controller,
                                   gpointer user_data)
{
    InvoiceWindow *iw = user_data;
    GncInvoice *invoice = iw_get_invoice(iw);
    GtkTextBuffer* text_buffer;
    GtkTextIter start, end;
    gchar *text;

    (void) controller;

    if (!invoice)
        return;

    text_buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW(iw->notes_text));
    gtk_text_buffer_get_bounds (text_buffer, &start, &end);
    text = gtk_text_buffer_get_text (text_buffer, &start, &end, FALSE);
    gncInvoiceSetNotes (invoice, text);
    g_free (text);
}

static gboolean
gnc_invoice_window_leave_to_charge_cb (GtkEventControllerFocus *controller,
                                       gpointer user_data)
{
    gnc_amount_edit_evaluate (GNC_AMOUNT_EDIT(user_data), NULL);
    return FALSE;
}

static void
gnc_invoice_window_changed_to_charge_cb (GtkWidget *widget, gpointer data)
{
    InvoiceWindow *iw = data;
    GncInvoice *invoice = iw_get_invoice(iw);

    if (!invoice) return;

    gncInvoiceSetToChargeAmount (invoice, gnc_amount_edit_get_amount
                                 (GNC_AMOUNT_EDIT (widget)));
}

static GtkWidget *
add_summary_label (GtkWidget *summarybar, const char *label_str)
{
    GtkWidget *hbox;
    GtkWidget *label;

    hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 2);
    gtk_box_set_homogeneous (GTK_BOX (hbox), FALSE);
    gtk_box_append (GTK_BOX(summarybar), GTK_WIDGET(hbox));
    gtk_box_set_spacing (GTK_BOX(summarybar), 5);

    label = gtk_label_new (label_str);
    gnc_label_set_alignment (label, 1.0, 0.5);
    gtk_box_append (GTK_BOX(hbox), GTK_WIDGET(label));

    label = gtk_label_new ("");
    gnc_label_set_alignment (label, 1.0, 0.5);
    gtk_box_append (GTK_BOX(hbox), GTK_WIDGET(label));

    return label;
}

GtkWidget *
gnc_invoice_window_create_summary_bar (InvoiceWindow *iw)
{
    GtkWidget *summarybar;

    iw->total_label           = NULL;
    iw->total_cash_label      = NULL;
    iw->total_charge_label    = NULL;
    iw->total_subtotal_label  = NULL;
    iw->total_tax_label       = NULL;

    summarybar = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 4);
    gtk_box_set_homogeneous (GTK_BOX (summarybar), FALSE);
    gtk_widget_set_name (summarybar, "gnc-id-summarybar");

    iw->total_label           = add_summary_label (summarybar, _("Total:"));

    switch (gncOwnerGetType (&iw->owner))
    {
    case GNC_OWNER_CUSTOMER:
    case GNC_OWNER_VENDOR:
        iw->total_subtotal_label = add_summary_label (summarybar, _("Subtotal:"));
        iw->total_tax_label     = add_summary_label (summarybar, _("Tax:"));
        break;

    case GNC_OWNER_EMPLOYEE:
        iw->total_cash_label    = add_summary_label (summarybar, _("Total Cash:"));
        iw->total_charge_label  = add_summary_label (summarybar, _("Total Charge:"));
        break;

    default:
        break;
    }

    return summarybar;
}

static int
gnc_invoice_job_changed_cb (GtkWidget *widget, gpointer data)
{
    InvoiceWindow *iw = data;
    char const *msg = "";

    if (!iw)
        return FALSE;

    if (iw->dialog_type == VIEW_INVOICE)
        return FALSE;

    gnc_owner_get_owner (iw->job_choice, &(iw->job));

    if (iw->dialog_type == EDIT_INVOICE)
        return FALSE;

    msg = gncJobGetReference (gncOwnerGetJob (&(iw->job)));
    gnc_entry_set_text (GTK_ENTRY (iw->billing_id_entry), msg ? msg : "");

    return FALSE;

}

static GNCSearchWindow *
gnc_invoice_select_job_cb (GtkWindow *parent, gpointer jobp, gpointer user_data)
{
    GncJob *j = jobp;
    InvoiceWindow *iw = user_data;
    GncOwner owner, *ownerp;

    if (!iw) return NULL;

    if (j)
    {
        ownerp = gncJobGetOwner (j);
        gncOwnerCopy (ownerp, &owner);
    }
    else
        gncOwnerCopy (&(iw->owner), &owner);

    return gnc_job_search (parent, j, &owner, iw->book);
}

static void
gnc_invoice_update_job_choice (InvoiceWindow *iw)
{
    if (iw->job_choice)
        gtk_box_remove (GTK_BOX(iw->job_box), GTK_WIDGET(iw->job_choice));

    /* If we don't have a real owner, then we obviously can't have a job */
    if (iw->owner.owner.undefined == NULL)
    {
        iw->job_choice = NULL;
    }
    else
        switch (iw->dialog_type)
        {
        case VIEW_INVOICE:
        case EDIT_INVOICE:
            iw->job_choice =
                gnc_owner_edit_create (NULL, iw->job_box, iw->book, &(iw->job));
            break;
        case NEW_INVOICE:
        case MOD_INVOICE:
        case DUP_INVOICE:
            iw->job_choice =
                gnc_general_search_new (GNC_JOB_MODULE_NAME, _("Select…"), TRUE,
                                        gnc_invoice_select_job_cb, iw, iw->book);

            gnc_general_search_set_selected (GNC_GENERAL_SEARCH (iw->job_choice),
                                             gncOwnerGetJob (&iw->job));
            gnc_general_search_allow_clear (GNC_GENERAL_SEARCH (iw->job_choice),
                                            TRUE);
            gtk_box_append (GTK_BOX(iw->job_box), GTK_WIDGET(iw->job_choice));

            g_signal_connect (G_OBJECT (iw->job_choice), "changed",
                              G_CALLBACK (gnc_invoice_job_changed_cb), iw);
            break;
        }

}

static GNCSearchWindow *
gnc_invoice_select_proj_job_cb (GtkWindow *parent, gpointer jobp, gpointer user_data)
{
    GncJob *j = jobp;
    InvoiceWindow *iw = user_data;
    GncOwner owner, *ownerp;

    if (!iw) return NULL;

    if (j)
    {
        ownerp = gncJobGetOwner (j);
        gncOwnerCopy (ownerp, &owner);
    }
    else
        gncOwnerCopy (&(iw->proj_cust), &owner);

    return gnc_job_search (parent, j, &owner, iw->book);
}

static int
gnc_invoice_proj_job_changed_cb (GtkWidget *widget, gpointer data)
{
    InvoiceWindow *iw = data;

    if (!iw)
        return FALSE;

    if (iw->dialog_type == VIEW_INVOICE)
        return FALSE;

    gnc_owner_get_owner (iw->proj_job_choice, &(iw->proj_job));
    return FALSE;
}

static void
gnc_invoice_update_proj_job (InvoiceWindow *iw)
{
    if (iw->proj_job_choice)
        gtk_box_remove (GTK_BOX(iw->proj_job_box), GTK_WIDGET(iw->proj_job_choice));

    switch (iw->dialog_type)
    {
    case VIEW_INVOICE:
    case EDIT_INVOICE:
        iw->proj_job_choice =
            gnc_owner_edit_create (NULL, iw->proj_job_box, iw->book, &(iw->proj_job));
        break;
    case NEW_INVOICE:
    case MOD_INVOICE:
    case DUP_INVOICE:
        if (iw->proj_cust.owner.undefined == NULL)
        {
            iw->proj_job_choice = NULL;
        }
        else
        {
            iw->proj_job_choice =
                gnc_general_search_new (GNC_JOB_MODULE_NAME, _("Select…"), TRUE,
                                        gnc_invoice_select_proj_job_cb, iw, iw->book);

            gnc_general_search_set_selected (GNC_GENERAL_SEARCH(iw->proj_job_choice),
                                             gncOwnerGetJob (&iw->proj_job));
            gnc_general_search_allow_clear (GNC_GENERAL_SEARCH (iw->proj_job_choice),
                                            TRUE);
            gtk_box_append (GTK_BOX(iw->proj_job_box), GTK_WIDGET(iw->proj_job_choice));

            g_signal_connect (G_OBJECT (iw->proj_job_choice), "changed",
                              G_CALLBACK (gnc_invoice_proj_job_changed_cb), iw);
        }
        break;
    }

}

static int
gnc_invoice_owner_changed_cb (GtkWidget *widget, gpointer data)
{
    InvoiceWindow *iw = data;
    GncBillTerm *term = NULL;
    GncOwner owner;

    if (!iw)
        return FALSE;

    if (iw->dialog_type == VIEW_INVOICE)
        return FALSE;

    gncOwnerCopy (&(iw->owner), &owner);
    gnc_owner_get_owner (iw->owner_choice, &owner);

    /* If this owner really changed, then reset ourselves */
    if (!gncOwnerEqual (&owner, &(iw->owner)))
    {
        gncOwnerCopy (&owner, &(iw->owner));
        gncOwnerInitJob (&(iw->job), NULL);
        gnc_entry_ledger_reset_query (iw->ledger);
    }

    if (iw->dialog_type == EDIT_INVOICE)
        return FALSE;

    switch (gncOwnerGetType (&(iw->owner)))
    {
    case GNC_OWNER_CUSTOMER:
        term = gncCustomerGetTerms (gncOwnerGetCustomer (&(iw->owner)));
        break;
    case GNC_OWNER_VENDOR:
        term = gncVendorGetTerms (gncOwnerGetVendor (&(iw->owner)));
        break;
    case GNC_OWNER_EMPLOYEE:
        term = NULL;
        break;
    default:
        g_warning ("Unknown owner type: %d\n", gncOwnerGetType (&(iw->owner)));
        break;
    }

    /* XXX: I'm not sure -- should we change the terms if this happens? */
    iw->terms = term;
    gnc_simple_dropdown_set_value (GTK_DROP_DOWN(iw->terms_menu), iw->terms);

    gnc_invoice_update_job_choice (iw);

    return FALSE;
}

static int
gnc_invoice_proj_cust_changed_cb (GtkWidget *widget, gpointer data)
{
    InvoiceWindow *iw = data;
    GncOwner owner;

    if (!iw)
        return FALSE;

    if (iw->dialog_type == VIEW_INVOICE)
        return FALSE;

    gncOwnerCopy (&(iw->proj_cust), &owner);
    gnc_owner_get_owner (iw->proj_cust_choice, &owner);

    /* If this owner really changed, then reset ourselves */
    if (!gncOwnerEqual (&owner, &(iw->proj_cust)))
    {
        gncOwnerCopy (&owner, &(iw->proj_cust));
        gncOwnerInitJob (&(iw->proj_job), NULL);
    }

    if (iw->dialog_type == EDIT_INVOICE)
        return FALSE;

    gnc_invoice_update_proj_job (iw);

    return FALSE;
}

static void
gnc_invoice_dialog_close_handler (gpointer user_data)
{
    InvoiceWindow *iw = user_data;

    if (iw && GTK_IS_WINDOW (iw->dialog))
        gtk_window_destroy (GTK_WINDOW (iw->dialog));
}

static void
gnc_invoice_window_close_handler (gpointer user_data)
{
    InvoiceWindow *iw = user_data;

    if (iw)
    {
        gnc_main_window_close_page(iw->page);
        iw->page = NULL;
    }
}

static void
gnc_invoice_reset_total_label (GtkLabel *label, gnc_numeric amt, gnc_commodity *com)
{
    char string[256];
    gchar *bidi_string;

    amt = gnc_numeric_convert (amt, gnc_commodity_get_fraction(com), GNC_HOW_RND_ROUND_HALF_UP);
    xaccSPrintAmount (string, amt, gnc_commodity_print_info (com, TRUE));

    bidi_string = gnc_wrap_text_with_bidi_ltr_isolate (string);
    gtk_label_set_text (label, bidi_string);
    g_free (bidi_string);
}

static void
gnc_invoice_redraw_all_cb (GnucashRegister *g_reg, gpointer data)
{
    InvoiceWindow *iw = data;
    GncInvoice * invoice;
    gnc_commodity * currency;
    gnc_numeric amount, to_charge_amt = gnc_numeric_zero();

    if (!iw)
        return;

    //  if (iw)
    //    gnc_invoice_update_window (iw, NULL);

    invoice = iw_get_invoice (iw);
    if (!invoice)
        return;

    currency = gncInvoiceGetCurrency (invoice);

    if (iw->total_label)
    {
        amount = gncInvoiceGetTotal (invoice);
        gnc_invoice_reset_total_label (GTK_LABEL (iw->total_label), amount, currency);
    }

    if (iw->total_subtotal_label)
    {
        amount = gncInvoiceGetTotalSubtotal (invoice);
        gnc_invoice_reset_total_label (GTK_LABEL (iw->total_subtotal_label), amount, currency);
    }

    if (iw->total_tax_label)
    {
        amount = gncInvoiceGetTotalTax (invoice);
        gnc_invoice_reset_total_label (GTK_LABEL (iw->total_tax_label), amount, currency);
    }

    /* Deal with extra items for the expense voucher */

    if (iw->to_charge_edit)
    {
        gnc_amount_edit_evaluate (GNC_AMOUNT_EDIT (iw->to_charge_edit), NULL);
        to_charge_amt = gnc_amount_edit_get_amount(GNC_AMOUNT_EDIT(iw->to_charge_edit));
    }

    if (iw->total_cash_label)
    {
        amount = gncInvoiceGetTotalOf (invoice, GNC_PAYMENT_CASH);
        amount = gnc_numeric_sub (amount, to_charge_amt,
                                  gnc_commodity_get_fraction (currency), GNC_HOW_RND_ROUND_HALF_UP);
        gnc_invoice_reset_total_label (GTK_LABEL (iw->total_cash_label), amount, currency);
    }

    if (iw->total_charge_label)
    {
        amount = gncInvoiceGetTotalOf (invoice, GNC_PAYMENT_CARD);
        amount = gnc_numeric_add (amount, to_charge_amt,
                                  gnc_commodity_get_fraction (currency), GNC_HOW_RND_ROUND_HALF_UP);
        gnc_invoice_reset_total_label (GTK_LABEL (iw->total_charge_label), amount, currency);
    }
}

void
gnc_invoice_window_changed (InvoiceWindow *iw, GtkWidget *window)
{
    gnc_entry_ledger_set_parent(iw->ledger, window);
}

gchar *
gnc_invoice_get_help (InvoiceWindow *iw)
{
    if (!iw)
        return NULL;

    return gnc_table_get_help (gnc_entry_ledger_get_table (iw->ledger));
}

static void
gnc_invoice_window_refresh_handler (GHashTable *changes, gpointer user_data)
{
    InvoiceWindow *iw = user_data;
    const EventInfo *info;
    GncInvoice *invoice = iw_get_invoice (iw);
    const GncOwner *owner;

    /* If there isn't an invoice behind us, close down */
    if (!invoice)
    {
        gnc_close_gui_component (iw->component_id);
        return;
    }

    /* Next, close if this is a destroy event */
    if (changes)
    {
        info = gnc_gui_get_entity_events (changes, &iw->invoice_guid);
        if (info && (info->event_mask & QOF_EVENT_DESTROY))
        {
            gnc_close_gui_component (iw->component_id);
            return;
        }
    }

    /* Check the owners, and see if they have changed */
    owner = gncInvoiceGetOwner (invoice);

    /* Copy the owner information into our window */
    gncOwnerCopy (gncOwnerGetEndOwner (owner), &(iw->owner));
    gncOwnerInitJob (&(iw->job), gncOwnerGetJob (owner));

    /* Copy the billto information into our window */
    owner = gncInvoiceGetBillTo (invoice);
    gncOwnerCopy (gncOwnerGetEndOwner (owner), &iw->proj_cust);
    gncOwnerInitJob (&iw->proj_job, gncOwnerGetJob (owner));

    /* Ok, NOW let's refresh ourselves */
    gnc_invoice_update_window (iw, NULL);
}

/** Update the various widgets in the window/page based upon the data
 *  in the InvoiceWindow data structure.
 *
 *  @param iw A pointer to the InvoiceWindow data structure.
 *
 *  @param widget If set, this is the widget that will be used for the
 *  initial page creation, before all of the iw/page linkages have been set\r\n *  up.
 */
static void
gnc_invoice_update_window (InvoiceWindow *iw, GtkWidget *widget)
{
    GtkWidget *acct_entry;
    GncInvoice *invoice;
    gboolean is_posted = FALSE;
    gboolean can_unpost = FALSE;

    invoice = iw_get_invoice (iw);

    if (iw->owner_choice)
        gtk_box_remove (GTK_BOX(iw->owner_box), GTK_WIDGET(iw->owner_choice));

    if (iw->proj_cust_choice)
        gtk_box_remove (GTK_BOX(iw->proj_cust_box), GTK_WIDGET(iw->proj_cust_choice));

    switch (iw->dialog_type)
    {
    case VIEW_INVOICE:
    case EDIT_INVOICE:
        iw->owner_choice =
            gnc_owner_edit_create (iw->owner_label, iw->owner_box, iw->book,
                                   &(iw->owner));
        iw->proj_cust_choice =
            gnc_owner_edit_create (NULL, iw->proj_cust_box, iw->book,
                                   &(iw->proj_cust));
        break;
    case NEW_INVOICE:
    case MOD_INVOICE:
    case DUP_INVOICE:
        iw->owner_choice =
            gnc_owner_select_create (iw->owner_label, iw->owner_box, iw->book,
                                     &(iw->owner));
        iw->proj_cust_choice =
            gnc_owner_select_create (NULL, iw->proj_cust_box, iw->book,
                                     &(iw->proj_cust));

        g_signal_connect (G_OBJECT (iw->owner_choice), "changed",
                          G_CALLBACK (gnc_invoice_owner_changed_cb), iw);

        g_signal_connect (G_OBJECT (iw->proj_cust_choice), "changed",
                          G_CALLBACK (gnc_invoice_proj_cust_changed_cb), iw);

        break;
    }

    /* Set the type label */
    gtk_label_set_text (GTK_LABEL(iw->type_label), iw->is_credit_note ? _("Credit Note")
                        : gtk_label_get_text (GTK_LABEL(iw->type_label)));


    gnc_invoice_update_job_choice (iw);
    gnc_invoice_update_proj_job (iw);

    /* Hide the project frame for customer invoices */
    if (iw->owner.type == GNC_OWNER_CUSTOMER)
        gtk_widget_set_visible (GTK_WIDGET(iw->proj_frame), FALSE);

    /* Hide the "job" label and entry for employee invoices */
    if (iw->owner.type == GNC_OWNER_EMPLOYEE)
    {
        gtk_widget_set_visible (GTK_WIDGET(iw->job_label), FALSE);
        gtk_widget_set_visible (GTK_WIDGET(iw->job_box), FALSE);
    }

    acct_entry = GTK_WIDGET (gtk_builder_get_object (iw->builder, "acct_entry"));

    /* We know that "invoice" (and "owner") exist now */
    {
        GtkTextBuffer* text_buffer;
        const char *string;
        gchar * tmp_string;
        time64 time;

        gnc_entry_set_text (GTK_ENTRY (iw->id_entry), gncInvoiceGetID (invoice));

        gnc_entry_set_text (GTK_ENTRY (iw->billing_id_entry),
                            gncInvoiceGetBillingID (invoice));

        string = gncInvoiceGetNotes (invoice);
        text_buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW(iw->notes_text));
        gtk_text_buffer_set_text (text_buffer, string, -1);

        if (iw->active_check)
            gtk_check_button_set_active (GTK_CHECK_BUTTON (iw->active_check),
                                          gncInvoiceGetActive (invoice));

        time = gncInvoiceGetDateOpened (invoice);
        if (time == INT64_MAX)
        {
            gnc_date_edit_set_time (GNC_DATE_EDIT (iw->opened_date),
                                    gnc_time (NULL));
        }
        else
        {
            gnc_date_edit_set_time (GNC_DATE_EDIT (iw->opened_date), time);
        }

        /* fill in the terms text */
        iw->terms = gncInvoiceGetTerms (invoice);
        //DEBUG("iw->dialog_type: %d",iw->dialog_type);
        switch (iw->dialog_type)
        {
            case NEW_INVOICE:
            case MOD_INVOICE:
            case DUP_INVOICE: //??
                gnc_simple_dropdown_set_value (GTK_DROP_DOWN(iw->terms_menu), iw->terms);
                break;

            case EDIT_INVOICE:
            case VIEW_INVOICE:
                // Fill in the invoice view version
                if(gncBillTermGetName (iw->terms) != NULL)
                    gnc_entry_set_text (GTK_ENTRY (iw->terms_menu),gncBillTermGetName (iw->terms));
                else
                    gnc_entry_set_text (GTK_ENTRY (iw->terms_menu),"None");
                break;

            default:
                break;
        }

        /*
         * Next, figure out if we've been posted, and if so set the appropriate
         * bits of information... Then work on hiding or showing as
         * necessary.
         */
        is_posted = gncInvoiceIsPosted (invoice);
        if (is_posted)
        {
            Account *acct = gncInvoiceGetPostedAcc (invoice);

            /* Can we unpost this invoice?
             * XXX: right now we always can, but there
             * may be times in the future when we cannot.
             */
            can_unpost = TRUE;

            time = gncInvoiceGetDatePosted (invoice);
            gnc_date_edit_set_time (GNC_DATE_EDIT (iw->posted_date), time);

            tmp_string = gnc_account_get_full_name (acct);
            gnc_entry_set_text (GTK_ENTRY (acct_entry), tmp_string);
            g_free(tmp_string);
        }
    }

    gnc_invoice_id_changed_cb(NULL, iw);
    if (iw->dialog_type == NEW_INVOICE ||
            iw->dialog_type == DUP_INVOICE ||
            iw->dialog_type == MOD_INVOICE)
    {
        if (widget)
            gtk_widget_set_visible (GTK_WIDGET(widget), TRUE);
        else
            gtk_widget_set_visible (GTK_WIDGET(iw_get_window(iw)), TRUE);
        return;
    }

    /* Fill in the to_charge amount (only in VIEW/EDIT modes) */
    {
        gnc_numeric amount;

        amount = gncInvoiceGetToChargeAmount (invoice);
        gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (iw->to_charge_edit), amount);
    }

    /* Hide/show the appropriate widgets based on our posted/paid state */

    {
        GtkWidget *hide, *show;

        if (is_posted)
        {
            show = GTK_WIDGET (gtk_builder_get_object (iw->builder, "posted_label"));
            gtk_widget_set_visible (GTK_WIDGET(show), TRUE);
            gtk_widget_set_visible (GTK_WIDGET(iw->posted_date_hbox), TRUE);

            show = GTK_WIDGET (gtk_builder_get_object (iw->builder, "acct_label"));
            gtk_widget_set_visible (GTK_WIDGET(show), TRUE);
            gtk_widget_set_visible (GTK_WIDGET(acct_entry), TRUE);
        }
        else           /* ! posted */
        {
            hide = GTK_WIDGET (gtk_builder_get_object (iw->builder, "posted_label"));
            gtk_widget_set_visible (GTK_WIDGET(hide), FALSE);
            gtk_widget_set_visible (GTK_WIDGET(iw->posted_date_hbox), FALSE);

            hide = GTK_WIDGET (gtk_builder_get_object (iw->builder, "acct_label"));
            gtk_widget_set_visible (GTK_WIDGET(hide), FALSE);
            gtk_widget_set_visible (GTK_WIDGET(acct_entry), FALSE);
        }
    }

    /* Set the toolbar widgets sensitivity */
    if (iw->page)
        gnc_plugin_page_invoice_update_menus(iw->page, is_posted, can_unpost);

    /* Set the to_charge widget */
    gtk_widget_set_sensitive (iw->to_charge_edit, !is_posted);

    /* Hide the to_charge frame for all non-employee invoices,
     * or set insensitive if the employee does not have a charge card
     */
    if (iw->owner.type == GNC_OWNER_EMPLOYEE)
    {
        if (!gncEmployeeGetCCard (gncOwnerGetEmployee(&iw->owner)))
            gtk_widget_set_sensitive (iw->to_charge_edit, FALSE);
    }
    else
    {
        gtk_widget_set_visible (GTK_WIDGET(iw->to_charge_frame), FALSE);
    }

    if (is_posted)
    {
        //    GtkWidget *hide;

        /* Setup viewer for read-only access */
        gtk_widget_set_sensitive (acct_entry, FALSE);
        gtk_widget_set_sensitive (iw->id_entry, FALSE); /* XXX: why set FALSE and then TRUE? */
        gtk_widget_set_sensitive (iw->id_entry, TRUE);
        gtk_widget_set_sensitive (iw->terms_menu, FALSE);
        gtk_widget_set_sensitive (iw->owner_box, TRUE);
        gtk_widget_set_sensitive (iw->job_box, TRUE);
        gtk_widget_set_sensitive (iw->billing_id_entry, FALSE);
        gtk_widget_set_sensitive (iw->notes_text, TRUE);
    }
    else           /* ! posted */
    {
        gtk_widget_set_sensitive (acct_entry, TRUE);
        gtk_widget_set_sensitive (iw->terms_menu, TRUE);
        gtk_widget_set_sensitive (iw->owner_box, TRUE);
        gtk_widget_set_sensitive (iw->job_box, TRUE);
        gtk_widget_set_sensitive (iw->billing_id_entry, TRUE);
        gtk_widget_set_sensitive (iw->notes_text, TRUE);
    }

    /* Translators: This is a label to show whether the invoice is paid or not. */
    if(gncInvoiceIsPaid (invoice))
        gtk_label_set_text(GTK_LABEL(iw->paid_label),  _("PAID"));
    else
        gtk_label_set_text(GTK_LABEL(iw->paid_label),  _("UNPAID"));

    if (widget)
        gtk_widget_set_visible (GTK_WIDGET(widget), TRUE);
    else
        gtk_widget_set_visible (GTK_WIDGET(iw_get_window(iw)), TRUE);
}

GncInvoiceType
gnc_invoice_get_type_from_window (InvoiceWindow *iw)
{
    /* uses the same approach as gnc_invoice_get_title
       not called gnc_invoice_get_type because of name collisions
    */
    switch (gncOwnerGetType(&iw->owner))
    {
    case GNC_OWNER_CUSTOMER:
        return iw->is_credit_note ? GNC_INVOICE_CUST_CREDIT_NOTE
                                  : GNC_INVOICE_CUST_INVOICE;
        break;
    case GNC_OWNER_VENDOR:
        return iw->is_credit_note ? GNC_INVOICE_VEND_CREDIT_NOTE
                                  : GNC_INVOICE_VEND_INVOICE;
        break;
    case GNC_OWNER_EMPLOYEE:
        return iw->is_credit_note ? GNC_INVOICE_EMPL_CREDIT_NOTE
                                  : GNC_INVOICE_EMPL_INVOICE;
        break;
    default:
        return GNC_INVOICE_UNDEFINED;
        break;
    }
}

gchar *
gnc_invoice_get_title (InvoiceWindow *iw)
{
    char *wintitle = NULL;
    const char *id = NULL;

    if (!iw) return NULL;

    switch (gncOwnerGetType (&iw->owner))
    {
    case GNC_OWNER_CUSTOMER:
        switch (iw->dialog_type)
        {
        case NEW_INVOICE:
            wintitle = iw->is_credit_note ? _("New Credit Note")
                       : _("New Invoice");
            break;
        case MOD_INVOICE:
        case DUP_INVOICE:
        case EDIT_INVOICE:
            wintitle = iw->is_credit_note ? _("Edit Credit Note")
                       : _("Edit Invoice");
            break;
        case VIEW_INVOICE:
            wintitle = iw->is_credit_note ? _("View Credit Note")
                       : _("View Invoice");
            break;
        }
        break;
    case GNC_OWNER_VENDOR:
        switch (iw->dialog_type)
        {
        case NEW_INVOICE:
            wintitle = iw->is_credit_note ? _("New Credit Note")
                       : _("New Bill");
            break;
        case MOD_INVOICE:
        case DUP_INVOICE:
        case EDIT_INVOICE:
            wintitle = iw->is_credit_note ? _("Edit Credit Note")
                       : _("Edit Bill");
            break;
        case VIEW_INVOICE:
            wintitle = iw->is_credit_note ? _("View Credit Note")
                       : _("View Bill");
            break;
        }
        break;
    case GNC_OWNER_EMPLOYEE:
        switch (iw->dialog_type)
        {
        case NEW_INVOICE:
            wintitle = iw->is_credit_note ? _("New Credit Note")
                       : _("New Expense Voucher");
            break;
        case MOD_INVOICE:
        case DUP_INVOICE:
        case EDIT_INVOICE:
            wintitle = iw->is_credit_note ? _("Edit Credit Note")
                       : _("Edit Expense Voucher");
            break;
        case VIEW_INVOICE:
            wintitle = iw->is_credit_note ? _("View Credit Note")
                       : _("View Expense Voucher");
            break;
        }
        break;
    default:
        break;
    }

    if (iw->id_entry)
        id = gnc_entry_get_text (GTK_ENTRY (iw->id_entry));
    if (id && *id)
        return g_strconcat (wintitle, " - ", id, (char *)NULL);
    return g_strdup (wintitle);
}

void
gnc_invoice_type_toggled_cb (GtkWidget *widget, gpointer data)
{
    InvoiceWindow *iw = data;

    if (!iw) return;
    iw->is_credit_note = !gtk_check_button_get_active (GTK_CHECK_BUTTON (widget));
}

void
gnc_invoice_id_changed_cb (GtkWidget *unused, gpointer data)
{
    InvoiceWindow *iw = data;
    gchar *title;

    if (!iw) return;
    if (iw->page)
    {
        gnc_plugin_page_invoice_update_title (iw->page);
    }
    else
    {
        title = gnc_invoice_get_title (iw);
        gtk_window_set_title (GTK_WINDOW (iw->dialog), title);
        g_free (title);
    }
}

void
gnc_invoice_terms_changed_cb (GtkDropDown *dropdown, GParamSpec *pspec, gpointer data)
{
    (void)pspec;
    InvoiceWindow *iw = data;

    if (!iw) return;
    if (!dropdown) return;

    iw->terms = gnc_simple_dropdown_get_value (dropdown);
}


static gboolean
find_handler (gpointer find_data, gpointer user_data)
{
    const GncGUID *invoice_guid = find_data;
    InvoiceWindow *iw = user_data;

    return(iw && guid_equal(&iw->invoice_guid, invoice_guid));
}

static InvoiceWindow *
gnc_invoice_new_page (QofBook *bookp, InvoiceDialogType type,
                      GncInvoice *invoice, const GncOwner *owner,
                      GncMainWindow *window, const gchar *group_name)
{
    InvoiceWindow *iw;
    GncOwner *billto;
    GncPluginPage *new_page;

    g_assert (type != NEW_INVOICE && type != MOD_INVOICE && type != DUP_INVOICE);
    g_assert (invoice != NULL);

    /*
     * Find an existing window for this invoice.  If found, bring it to
     * the front.
     */
    if (invoice)
    {
        GncGUID invoice_guid;

        invoice_guid = *gncInvoiceGetGUID (invoice);
        iw = gnc_find_first_gui_component (DIALOG_VIEW_INVOICE_CM_CLASS,
                                           find_handler, &invoice_guid);
        if (iw)
        {
            gnc_main_window_display_page(iw->page);
            return(iw);
        }
    }

    /*
     * No existing invoice window found.  Build a new one.
     */
    iw = g_new0 (InvoiceWindow, 1);
    iw->book = bookp;
    iw->dialog_type = type;
    iw->invoice_guid = *gncInvoiceGetGUID (invoice);
    iw->is_credit_note = gncInvoiceGetIsCreditNote (invoice);
    iw->width = -1;
    iw->page_state_name = group_name;

    /* Save this for later */
    gncOwnerCopy (gncOwnerGetEndOwner (owner), &(iw->owner));
    gncOwnerInitJob (&(iw->job), gncOwnerGetJob (owner));

    billto = gncInvoiceGetBillTo (invoice);
    gncOwnerCopy (gncOwnerGetEndOwner (billto), &(iw->proj_cust));
    gncOwnerInitJob (&iw->proj_job, gncOwnerGetJob (billto));

    /* Now create the plugin page for this invoice and display it. */
    new_page = gnc_plugin_page_invoice_new (iw);

    if (!window)
        window = gnc_plugin_business_get_window ();

    gnc_main_window_open_page (window, new_page);

    /* Initialize the summary bar */
    gnc_invoice_redraw_all_cb(iw->reg, iw);

    return iw;
}

#define KEY_INVOICE_TYPE        "InvoiceType"
#define KEY_INVOICE_GUID        "InvoiceGUID"
#define KEY_OWNER_TYPE          "OwnerType"
#define KEY_OWNER_GUID          "OwnerGUID"

GncPluginPage *
gnc_invoice_recreate_page (GncMainWindow *window,
                           GKeyFile *key_file,
                           const gchar *group_name)
{
    InvoiceWindow *iw;
    GError *error = NULL;
    char *tmp_string = NULL, *owner_type = NULL;
    InvoiceDialogType type;
    GncInvoice *invoice;
    GncGUID guid;
    QofBook *book;
    GncOwner owner = { 0 };

    /* Get Invoice Type */
    tmp_string = g_key_file_get_string(key_file, group_name,
                                       KEY_INVOICE_TYPE, &error);
    if (error)
    {
        g_warning("Error reading group %s key %s: %s.",
                  group_name, KEY_INVOICE_TYPE, error->message);
        goto give_up;
    }
    type = InvoiceDialogTypefromString(tmp_string);
    g_free(tmp_string);

    /* Get Invoice GncGUID */
    tmp_string = g_key_file_get_string(key_file, group_name,
                                       KEY_INVOICE_GUID, &error);
    if (error)
    {
        g_warning("Error reading group %s key %s: %s.",
                  group_name, KEY_INVOICE_GUID, error->message);
        goto give_up;
    }
    if (!string_to_guid(tmp_string, &guid))
    {
        g_warning("Invalid invoice guid: %s.", tmp_string);
        goto give_up;
    }
    book = gnc_get_current_book();
    invoice = gncInvoiceLookup(gnc_get_current_book(), &guid);
    if (invoice == NULL)
    {
        g_warning("Can't find invoice %s in current book.", tmp_string);
        goto give_up;
    }
    g_free(tmp_string);
    tmp_string = NULL;

    /* Get Owner Type */
    owner_type = g_key_file_get_string(key_file, group_name,
                                       KEY_OWNER_TYPE, &error);
    if (error)
    {
        g_warning("Error reading group %s key %s: %s.",
                  group_name, KEY_OWNER_TYPE, error->message);
        goto give_up;
    }

    /* Get Owner GncGUID */
    tmp_string = g_key_file_get_string(key_file, group_name,
                                       KEY_OWNER_GUID, &error);
    if (error)
    {
        g_warning("Error reading group %s key %s: %s.",
                  group_name, KEY_OWNER_GUID, error->message);
        goto give_up;
    }
    if (!string_to_guid(tmp_string, &guid))
    {
        g_warning("Invalid owner guid: %s.", tmp_string);
        goto give_up;
    }

    if (!gncOwnerGetOwnerFromTypeGuid(book, &owner, owner_type, &guid))
    {
        g_warning("Can't find owner %s in current book.", tmp_string);
        goto give_up;
    }
    g_free(tmp_string);
    g_free(owner_type);

    iw = gnc_invoice_new_page (book, type, invoice, &owner, window, group_name);
    return iw->page;

give_up:
    g_warning("Giving up on restoring '%s'.", group_name);
    if (error)
        g_error_free(error);
    if (tmp_string)
        g_free(tmp_string);
    if (owner_type)
        g_free(owner_type);
    return NULL;
}

void
gnc_invoice_save_page (InvoiceWindow *iw,
                       GKeyFile *key_file,
                       const gchar *group_name)
{
    Table *table = gnc_entry_ledger_get_table (iw->ledger);
    gchar guidstr[GUID_ENCODING_LENGTH+1];
    guid_to_string_buff(&iw->invoice_guid, guidstr);
    g_key_file_set_string(key_file, group_name, KEY_INVOICE_TYPE,
                          InvoiceDialogTypeasString(iw->dialog_type));
    g_key_file_set_string(key_file, group_name, KEY_INVOICE_GUID, guidstr);

    if (gncOwnerGetJob (&(iw->job)))
    {
        g_key_file_set_string(key_file, group_name, KEY_OWNER_TYPE,
                          qofOwnerGetType(&iw->job));
        guid_to_string_buff(gncOwnerGetGUID(&iw->job), guidstr);
        g_key_file_set_string(key_file, group_name, KEY_OWNER_GUID, guidstr);
    }
    else
    {
        g_key_file_set_string(key_file, group_name, KEY_OWNER_TYPE,
                          qofOwnerGetType(&iw->owner));
        guid_to_string_buff(gncOwnerGetGUID(&iw->owner), guidstr);
        g_key_file_set_string(key_file, group_name, KEY_OWNER_GUID, guidstr);
    }
    // save the open table layout
    gnc_table_save_state (table, group_name);
}

static gboolean
doclink_button_cb (GtkLinkButton *button, InvoiceWindow *iw)
{
    GncInvoice *invoice = gncInvoiceLookup (iw->book, &iw->invoice_guid);
    gnc_doclink_open_uri (GTK_WINDOW(iw->dialog), gncInvoiceGetDocLink (invoice));

    return TRUE;
}

GtkWidget *
gnc_invoice_create_page (InvoiceWindow *iw, gpointer page)
{
    GncInvoice *invoice;
    GtkBuilder *builder;
    GtkWidget *dialog, *hbox;
    GncEntryLedger *entry_ledger = NULL;
    GncOwnerType owner_type;
    GncEntryLedgerType ledger_type;
    const gchar *prefs_group = NULL;
    gboolean is_credit_note = FALSE;
    const gchar *style_label = NULL;
    const gchar *doclink_uri;

    invoice = gncInvoiceLookup (iw->book, &iw->invoice_guid);
    is_credit_note = gncInvoiceGetIsCreditNote (invoice);

    iw->page = page;

    /* Find the dialog */
    iw->builder = builder = gtk_builder_new();
gnc_builder_add_from_file (builder, "dialog-invoice.glade", "invoice_entry_vbox");
    dialog = GTK_WIDGET (gtk_builder_get_object (builder, "invoice_entry_vbox"));

    /* Autoconnect all the signals */
gnc_builder_connect_signals_full (builder, gnc_builder_connect_full_func, iw);

    /* Grab the widgets */
    iw->id_label = GTK_WIDGET (gtk_builder_get_object (builder, "label3"));
    iw->type_label = GTK_WIDGET (gtk_builder_get_object (builder, "page_type_label"));
    iw->info_label = GTK_WIDGET (gtk_builder_get_object (builder, "label25"));
    iw->id_entry = GTK_WIDGET (gtk_builder_get_object (builder, "page_id_entry"));
    iw->billing_id_entry = GTK_WIDGET (gtk_builder_get_object (builder, "page_billing_id_entry"));
    iw->terms_menu = GTK_WIDGET (gtk_builder_get_object (builder, "page_terms_menu"));
    iw->notes_text = GTK_WIDGET (gtk_builder_get_object (builder, "page_notes_text"));
    GtkEventController *notes_focus_controller = gtk_event_controller_focus_new ();
    gtk_widget_add_controller (iw->notes_text, notes_focus_controller);
    g_signal_connect (notes_focus_controller, "leave",
                      G_CALLBACK (gnc_invoice_window_leave_notes_cb), iw);
    iw->active_check = GTK_WIDGET (gtk_builder_get_object (builder, "active_check"));
    iw->owner_box = GTK_WIDGET (gtk_builder_get_object (builder, "page_owner_hbox"));
    iw->owner_label = GTK_WIDGET (gtk_builder_get_object (builder, "page_owner_label"));
    iw->job_label = GTK_WIDGET (gtk_builder_get_object (builder, "page_job_label"));
    iw->job_box = GTK_WIDGET (gtk_builder_get_object (builder, "page_job_hbox"));
    iw->paid_label = GTK_WIDGET (gtk_builder_get_object (builder, "paid_label"));

    iw->doclink_button = GTK_WIDGET(gtk_builder_get_object (builder, "doclink_button"));
    g_signal_connect (G_OBJECT (iw->doclink_button), "activate-link",
                      G_CALLBACK (doclink_button_cb), iw);

    /* invoice doclink */
    doclink_uri = gncInvoiceGetDocLink (invoice);
    if (doclink_uri)
    {
        gchar *display_uri = gnc_doclink_get_unescaped_just_uri (doclink_uri);
        gtk_button_set_label (GTK_BUTTON (iw->doclink_button),
                              _("Open Linked Document:"));
        gtk_link_button_set_uri (GTK_LINK_BUTTON (iw->doclink_button),
                                 display_uri);
        gtk_widget_set_visible (GTK_WIDGET(iw->doclink_button), TRUE);
        g_free (display_uri);
    }
    else
        gtk_widget_set_visible (GTK_WIDGET(iw->doclink_button), FALSE);

    // Add a style context for this label so it can be easily manipulated with css
    gnc_widget_style_context_add_class (GTK_WIDGET(iw->paid_label), "gnc-class-highlight");

    /* grab the project widgets */
    iw->proj_frame = GTK_WIDGET (gtk_builder_get_object (builder, "page_proj_frame"));
    iw->proj_cust_box = GTK_WIDGET (gtk_builder_get_object (builder, "page_proj_cust_hbox"));
    iw->proj_job_box = GTK_WIDGET (gtk_builder_get_object (builder, "page_proj_job_hbox"));

    /* grab the to_charge widgets */
    {
        GtkWidget *edit;

        gnc_commodity *currency = gncInvoiceGetCurrency (invoice);
        GNCPrintAmountInfo print_info;

        iw->to_charge_frame = GTK_WIDGET (gtk_builder_get_object (builder, "to_charge_frame"));
        edit = gnc_amount_edit_new();
        print_info = gnc_commodity_print_info (currency, FALSE);
        gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT (edit), TRUE);
        gnc_amount_edit_set_print_info (GNC_AMOUNT_EDIT (edit), print_info);
        gnc_amount_edit_set_fraction (GNC_AMOUNT_EDIT (edit),
                                      gnc_commodity_get_fraction (currency));
        iw->to_charge_edit = edit;
        gtk_widget_set_visible (GTK_WIDGET(edit), TRUE);
        hbox = GTK_WIDGET (gtk_builder_get_object (builder, "to_charge_box"));
        gtk_box_append (GTK_BOX(hbox), GTK_WIDGET(edit));

        GtkEventController *event_controller = gtk_event_controller_focus_new ();
        gtk_widget_add_controller (GTK_WIDGET(edit), event_controller);
        g_signal_connect (G_OBJECT (event_controller), "leave",
                         G_CALLBACK(gnc_invoice_window_leave_to_charge_cb), edit);

        g_signal_connect(G_OBJECT(edit), "amount_changed",
                         G_CALLBACK(gnc_invoice_window_changed_to_charge_cb), iw);
    }

    hbox = GTK_WIDGET (gtk_builder_get_object (builder, "page_date_opened_hbox"));
    iw->opened_date = gnc_date_edit_new (gnc_time (NULL), FALSE, FALSE);
    gtk_widget_set_visible (GTK_WIDGET(iw->opened_date), TRUE);
    gtk_box_append (GTK_BOX(hbox), GTK_WIDGET(iw->opened_date));

    iw->posted_date_hbox = GTK_WIDGET (gtk_builder_get_object (builder, "date_posted_hbox"));
    iw->posted_date = gnc_date_edit_new (gnc_time (NULL), FALSE, FALSE);
    gtk_widget_set_visible (GTK_WIDGET(iw->posted_date), TRUE);
    gtk_box_append (GTK_BOX(iw->posted_date_hbox), GTK_WIDGET(iw->posted_date));

    /* Make the opened and posted dates insensitive in this window */
    gtk_widget_set_sensitive (iw->opened_date, FALSE);
    gtk_widget_set_sensitive (iw->posted_date, FALSE);
    /* Also the invoice ID */
    gtk_widget_set_sensitive (iw->id_entry, FALSE);

    /* Build the ledger */
    ledger_type = GNCENTRY_INVOICE_VIEWER;
    owner_type = gncOwnerGetType (&iw->owner);
    switch (iw->dialog_type)
    {
    case EDIT_INVOICE:
        switch (owner_type)
        {
        case GNC_OWNER_CUSTOMER:
            ledger_type = is_credit_note ? GNCENTRY_CUST_CREDIT_NOTE_ENTRY
                          : GNCENTRY_INVOICE_ENTRY;
            break;
        case GNC_OWNER_VENDOR:
            ledger_type = is_credit_note ? GNCENTRY_VEND_CREDIT_NOTE_ENTRY
                          : GNCENTRY_BILL_ENTRY;
            break;
        case GNC_OWNER_EMPLOYEE:
            ledger_type = is_credit_note ? GNCENTRY_EMPL_CREDIT_NOTE_ENTRY
                          : GNCENTRY_EXPVOUCHER_ENTRY;
            break;
        default:
            g_warning ("Invalid owner type");
            break;
        }
        break;
    case VIEW_INVOICE:
    default:
        switch (owner_type)
        {
        case GNC_OWNER_CUSTOMER:
            ledger_type = is_credit_note ? GNCENTRY_CUST_CREDIT_NOTE_VIEWER
                          : GNCENTRY_INVOICE_VIEWER;
            prefs_group   = GNC_PREFS_GROUP_INVOICE;
            break;
        case GNC_OWNER_VENDOR:
            ledger_type = is_credit_note ? GNCENTRY_VEND_CREDIT_NOTE_VIEWER
                          : GNCENTRY_BILL_VIEWER;
            prefs_group   = GNC_PREFS_GROUP_BILL;
            break;
        case GNC_OWNER_EMPLOYEE:
            ledger_type = is_credit_note ? GNCENTRY_EMPL_CREDIT_NOTE_VIEWER
                          : GNCENTRY_EXPVOUCHER_VIEWER;
            prefs_group   = GNC_PREFS_GROUP_BILL;
            break;
        default:
            g_warning ("Invalid owner type");
            break;
        }
        break;
    }
    /* Default labels are for invoices, change them if they are anything else. */
    switch (owner_type)
    {
        case GNC_OWNER_VENDOR:
            gtk_label_set_text (GTK_LABEL(iw->info_label),  _("Bill Information"));
            gtk_label_set_text (GTK_LABEL(iw->type_label),  _("Bill"));
            gtk_label_set_text (GTK_LABEL(iw->id_label),  _("Bill ID"));
            style_label = "gnc-class-vendors";
            break;
        case GNC_OWNER_EMPLOYEE:
            gtk_label_set_text (GTK_LABEL(iw->info_label),  _("Voucher Information"));
            gtk_label_set_text (GTK_LABEL(iw->type_label),  _("Voucher"));
            gtk_label_set_text (GTK_LABEL(iw->id_label),  _("Voucher ID"));
            style_label = "gnc-class-employees";
            break;
        default:
            style_label = "gnc-class-customers";
            break;
    }
    // Set a secondary style context for this page so it can be easily manipulated with css
    gnc_widget_style_context_add_class (GTK_WIDGET(dialog), style_label);

    entry_ledger = gnc_entry_ledger_new (iw->book, ledger_type);

    /* Save the ledger... */
    iw->ledger = entry_ledger;
    /* window will be updated in a callback */

    /* Set the entry_ledger's invoice */
    gnc_entry_ledger_set_default_invoice (entry_ledger, invoice);

    /* Set the preferences group */
    gnc_entry_ledger_set_prefs_group (entry_ledger, prefs_group);

    /* Setup initial values */
    iw->component_id =
        gnc_register_gui_component (DIALOG_VIEW_INVOICE_CM_CLASS,
                                    gnc_invoice_window_refresh_handler,
                                    gnc_invoice_window_close_handler,
                                    iw);

    gnc_gui_component_watch_entity_type (iw->component_id,
                                         GNC_INVOICE_MODULE_NAME,
                                         QOF_EVENT_MODIFY | QOF_EVENT_DESTROY);

    /* Create the register */
    {
        GtkWidget *regWidget, *frame, *window;
        const gchar *default_group = gnc_invoice_window_get_state_group (iw);
        const gchar *group;

        // if this is from a page recreate, use those settings
        if (iw->page_state_name)
            group = iw->page_state_name;
        else
            group = default_group;

        /* Watch the order of operations, here... */
        regWidget = gnucash_register_new (gnc_entry_ledger_get_table
                                          (entry_ledger), group);
        gtk_widget_set_visible (GTK_WIDGET(regWidget), TRUE);

        frame = GTK_WIDGET (gtk_builder_get_object (builder, "ledger_frame"));
        gtk_frame_set_child (GTK_FRAME(frame), GTK_WIDGET(regWidget));

        iw->reg = GNUCASH_REGISTER (regWidget);
        window = gnc_plugin_page_get_window(iw->page);
        gnucash_sheet_set_window (gnucash_register_get_sheet (iw->reg), window);

        g_signal_connect (G_OBJECT (regWidget), "activate_cursor",
                          G_CALLBACK (gnc_invoice_window_recordCB), iw);
        g_signal_connect (G_OBJECT (regWidget), "redraw_all",
                          G_CALLBACK (gnc_invoice_redraw_all_cb), iw);
    }

    gnc_table_realize_gui (gnc_entry_ledger_get_table (entry_ledger));

    /* Now fill in a lot of the pieces and display properly */
    gnc_invoice_update_window (iw, dialog);

    gnc_table_refresh_gui (gnc_entry_ledger_get_table (iw->ledger), TRUE);

    return dialog;
}

void
gnc_invoice_update_doclink_for_window (GncInvoice *invoice, const gchar *uri)
{
    InvoiceWindow *iw = gnc_plugin_page_invoice_get_window (invoice);

    if (iw)
    {
        GtkWidget *doclink_button = gnc_invoice_window_get_doclink_button (iw);

        if (g_strcmp0 (uri, "") == 0) // deleted uri
        {
            GAction *uri_action;

            // update the menu actions
            uri_action = gnc_plugin_page_get_action (GNC_PLUGIN_PAGE(iw->page), "BusinessLinkOpenAction");
            g_simple_action_set_enabled (G_SIMPLE_ACTION(uri_action), FALSE);

            gtk_widget_set_visible (GTK_WIDGET(doclink_button), FALSE);
        }
        else
        {
            gchar *display_uri = gnc_doclink_get_unescaped_just_uri (uri);
            gtk_link_button_set_uri (GTK_LINK_BUTTON (doclink_button),
                                     display_uri);
            gtk_widget_set_visible (GTK_WIDGET(doclink_button), TRUE);
            g_free (display_uri);
        }
    }
}

static InvoiceWindow *
gnc_invoice_window_new_invoice (GtkWindow *parent, InvoiceDialogType dialog_type, QofBook *bookp,
                                const GncOwner *owner, GncInvoice *invoice)
{
    InvoiceWindow *iw;
    GtkBuilder *builder;
    GtkWidget *hbox;
    GtkWidget *invoice_radio;
    GncOwner *billto;
    const GncOwner *start_owner;
    GncBillTerm *owner_terms = NULL;
    GncOwnerType owner_type;
    const gchar *style_label = NULL;

    g_assert (dialog_type == NEW_INVOICE || dialog_type == MOD_INVOICE || dialog_type == DUP_INVOICE);

    if (invoice)
    {
        /*
         * Try to find an existing window for this invoice.  If found,
         * bring it to the front.
         */
        GncGUID invoice_guid;

        invoice_guid = *gncInvoiceGetGUID (invoice);
        iw = gnc_find_first_gui_component (DIALOG_NEW_INVOICE_CM_CLASS,
                                           find_handler, &invoice_guid);
        if (iw)
        {
            gtk_window_set_transient_for (GTK_WINDOW(iw->dialog), parent);
            gtk_window_present (GTK_WINDOW(iw->dialog));
            return(iw);
        }
    }

    /*
     * No existing invoice window found.  Build a new one.
     */

    iw = g_new0 (InvoiceWindow, 1);
    iw->dialog_type = dialog_type;

    switch (dialog_type)
    {
    case NEW_INVOICE:
        g_assert (bookp);

        invoice = gncInvoiceCreate (bookp);
        gncInvoiceSetCurrency (invoice, gnc_default_currency ());
        iw->book = bookp;
        start_owner = owner;
        switch (gncOwnerGetType (gncOwnerGetEndOwner (owner)))
        {
        case GNC_OWNER_CUSTOMER:
            owner_terms = gncCustomerGetTerms (gncOwnerGetCustomer (gncOwnerGetEndOwner (owner)));
            break;
        case GNC_OWNER_VENDOR:
            owner_terms = gncVendorGetTerms (gncOwnerGetVendor (gncOwnerGetEndOwner (owner)));
            break;
        default:
            break;
        }
        if (owner_terms)
            gncInvoiceSetTerms (invoice, owner_terms);
        break;

    case MOD_INVOICE:
    case DUP_INVOICE:
        start_owner = gncInvoiceGetOwner (invoice);
        iw->book = gncInvoiceGetBook (invoice);
        break;
    default:
        /* The assert at the beginning of this function should prevent this switch case ! */
        return NULL;
    }

    /* Save this for later */
    gncOwnerCopy (gncOwnerGetEndOwner(start_owner), &(iw->owner));
    gncOwnerInitJob (&(iw->job), gncOwnerGetJob (start_owner));

    billto = gncInvoiceGetBillTo (invoice);
    gncOwnerCopy (gncOwnerGetEndOwner (billto), &(iw->proj_cust));
    gncOwnerInitJob (&iw->proj_job, gncOwnerGetJob (billto));

    /* Find the glade page layout */
    iw->builder = builder = gtk_builder_new();
gnc_builder_add_from_file (builder, "dialog-invoice.glade", "new_invoice_dialog");
    iw->dialog = GTK_WIDGET (gtk_builder_get_object (builder, "new_invoice_dialog"));
    gtk_window_set_transient_for (GTK_WINDOW(iw->dialog), parent);

    // Set the name for this dialog so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(iw->dialog), "gnc-id-invoice");

    g_object_set_data (G_OBJECT (iw->dialog), "dialog_info", iw);

    /* Grab the widgets */
    iw->type_label = GTK_WIDGET (gtk_builder_get_object (builder, "dialog_type_label"));
    iw->type_label_hbox = GTK_WIDGET (gtk_builder_get_object (builder, "dialog_type_label_hbox"));
    iw->id_label = GTK_WIDGET (gtk_builder_get_object (builder, "label14"));
    iw->info_label = GTK_WIDGET (gtk_builder_get_object (builder, "label1"));
    invoice_radio = GTK_WIDGET (gtk_builder_get_object (builder, "dialog_invoice_type"));

    iw->type_hbox = GTK_WIDGET (gtk_builder_get_object (builder, "dialog_type_choice_hbox"));
    iw->type_choice = GTK_WIDGET (gtk_builder_get_object (builder, "dialog_type_invoice"));

    /* The default GUI labels are for invoices, so change them if it isn't. */
    owner_type = gncOwnerGetType (&iw->owner);
    switch(owner_type)
    {
        case GNC_OWNER_VENDOR:
            gtk_label_set_text (GTK_LABEL(iw->info_label),  _("Bill Information"));
            gtk_label_set_text (GTK_LABEL(iw->type_label),  _("Bill"));
            gtk_button_set_label (GTK_BUTTON(invoice_radio),  _("Bill"));
            gtk_label_set_text (GTK_LABEL(iw->id_label),  _("Bill ID"));
            style_label = "gnc-class-vendors";
            break;
        case GNC_OWNER_EMPLOYEE:
            gtk_label_set_text (GTK_LABEL(iw->info_label),  _("Voucher Information"));
            gtk_label_set_text (GTK_LABEL(iw->type_label),  _("Voucher"));
            gtk_button_set_label (GTK_BUTTON(invoice_radio),  _("Voucher"));
            gtk_label_set_text (GTK_LABEL(iw->id_label),  _("Voucher ID"));
            style_label = "gnc-class-employees";
            break;
        default:
            style_label = "gnc-class-customers";
        break;
    }
    // Set a secondary style context for this page so it can be easily manipulated with css
    gnc_widget_style_context_add_class (GTK_WIDGET(iw->dialog), style_label);

    /* configure the type related widgets based on dialog type and invoice type */
    switch (dialog_type)
    {
    case NEW_INVOICE:
    case DUP_INVOICE:
        gtk_widget_set_visible (GTK_WIDGET(iw->type_label_hbox), FALSE);
        gtk_widget_set_visible (GTK_WIDGET(iw->type_label), FALSE);
        break;
    case MOD_INVOICE:
        gtk_widget_set_visible (GTK_WIDGET(iw->type_hbox), FALSE);
        gtk_widget_set_visible (GTK_WIDGET(iw->type_label_hbox), TRUE);
        gtk_widget_set_visible (GTK_WIDGET(iw->type_label), TRUE);
        break;
    default:
        break;
    }

    if (dialog_type == DUP_INVOICE)
    {
        GtkWidget *cn_radio = GTK_WIDGET (gtk_builder_get_object (builder, "dialog_creditnote_type"));

        gtk_check_button_set_active (GTK_CHECK_BUTTON(cn_radio), gncInvoiceGetIsCreditNote (invoice));
    }

    iw->id_entry = GTK_WIDGET (gtk_builder_get_object (builder, "dialog_id_entry"));
    iw->billing_id_entry = GTK_WIDGET (gtk_builder_get_object (builder, "dialog_billing_id_entry"));
    iw->terms_menu = GTK_WIDGET (gtk_builder_get_object (builder, "dialog_terms_menu"));
    iw->notes_text = GTK_WIDGET (gtk_builder_get_object (builder, "dialog_notes_text"));
    GtkEventController *notes_focus_controller = gtk_event_controller_focus_new ();
    gtk_widget_add_controller (iw->notes_text, notes_focus_controller);
    g_signal_connect (notes_focus_controller, "leave",
                      G_CALLBACK (gnc_invoice_window_leave_notes_cb), iw);
    iw->owner_box = GTK_WIDGET (gtk_builder_get_object (builder, "dialog_owner_hbox"));
    iw->owner_label = GTK_WIDGET (gtk_builder_get_object (builder, "dialog_owner_label"));
    iw->job_label = GTK_WIDGET (gtk_builder_get_object (builder, "dialog_job_label"));
    iw->job_box = GTK_WIDGET (gtk_builder_get_object (builder, "dialog_job_hbox"));

    /* Grab the project widgets */
    iw->proj_frame = GTK_WIDGET (gtk_builder_get_object (builder, "dialog_proj_frame"));
    iw->proj_cust_box = GTK_WIDGET (gtk_builder_get_object (builder, "dialog_proj_cust_hbox"));
    iw->proj_job_box = GTK_WIDGET (gtk_builder_get_object (builder, "dialog_proj_job_hbox"));

    hbox = GTK_WIDGET (gtk_builder_get_object (builder, "dialog_date_opened_hbox"));
    iw->opened_date = gnc_date_edit_new (gnc_time (NULL), FALSE, FALSE);
    gtk_widget_set_visible (GTK_WIDGET(iw->opened_date), TRUE);
    gtk_box_append (GTK_BOX(hbox), GTK_WIDGET(iw->opened_date));

    /* If this is a New Invoice, reset the Notes file to read/write */
    gtk_widget_set_sensitive (iw->notes_text,
                              (iw->dialog_type == NEW_INVOICE) ||
                              (iw->dialog_type == DUP_INVOICE));

    /* Setup signals */
    gnc_builder_connect_signals_full (builder, gnc_builder_connect_full_func,
                                      iw);

    /* Setup initial values */
    iw->reportPage = NULL;
    iw->invoice_guid = *gncInvoiceGetGUID (invoice);
    iw->is_credit_note = gncInvoiceGetIsCreditNote (invoice);

    iw->component_id =
        gnc_register_gui_component (DIALOG_NEW_INVOICE_CM_CLASS,
                                    gnc_invoice_window_refresh_handler,
                                    gnc_invoice_dialog_close_handler,
                                    iw);

    gnc_gui_component_watch_entity_type (iw->component_id,
                                         GNC_INVOICE_MODULE_NAME,
                                         QOF_EVENT_MODIFY | QOF_EVENT_DESTROY);

    /* Now fill in a lot of the pieces and display properly */
    switch(dialog_type)
    {
        case NEW_INVOICE:
        case MOD_INVOICE:
        case DUP_INVOICE:
            gnc_billterms_dropdown (GTK_DROP_DOWN(iw->terms_menu), iw->book, TRUE, iw->terms);
            break;
        case EDIT_INVOICE:
        case VIEW_INVOICE:
            // Fill in the invoice view version
            if(gncBillTermGetName (iw->terms) != NULL)
                gnc_entry_set_text (GTK_ENTRY (iw->terms_menu),gncBillTermGetName (iw->terms));
            else
                gnc_entry_set_text (GTK_ENTRY (iw->terms_menu),"None");
        break;
    }

    gnc_invoice_update_window (iw, iw->dialog);
    gnc_table_refresh_gui (gnc_entry_ledger_get_table (iw->ledger), TRUE);

    // The customer choice widget should have keyboard focus
    if (GNC_IS_GENERAL_SEARCH(iw->owner_choice))
    {
        gnc_general_search_grab_focus(GNC_GENERAL_SEARCH(iw->owner_choice));
    }

    return iw;
}

InvoiceWindow *
gnc_ui_invoice_edit (GtkWindow *parent, GncInvoice *invoice)
{
    InvoiceWindow *iw;
    InvoiceDialogType type;

    if (!invoice) return NULL;

    /* Immutable once we've been posted */
    if (gncInvoiceGetPostedAcc (invoice))
        type = VIEW_INVOICE;
    else
        type = EDIT_INVOICE;

    iw = gnc_invoice_new_page (gncInvoiceGetBook(invoice), type,
                               invoice, gncInvoiceGetOwner (invoice),
                               GNC_MAIN_WINDOW(gnc_ui_get_main_window (GTK_WIDGET (parent))), NULL);

    return iw;
}

static InvoiceWindow *
gnc_ui_invoice_modify (GtkWindow *parent, GncInvoice *invoice)
{
    InvoiceWindow *iw;
    if (!invoice) return NULL;

    iw = gnc_invoice_window_new_invoice (parent, MOD_INVOICE, NULL, NULL, invoice);
    return iw;
}


InvoiceWindow * gnc_ui_invoice_duplicate (GtkWindow *parent, GncInvoice *old_invoice, gboolean open_properties, const GDate *new_date)
{
    InvoiceWindow *iw = NULL;
    GncInvoice *new_invoice = NULL;
    time64 entry_date;

    g_assert(old_invoice);

    // Create a deep copy of the old invoice
    new_invoice = gncInvoiceCopy(old_invoice);

    // The new invoice is for sure active
    gncInvoiceSetActive(new_invoice, TRUE);

    // and unposted
    if (gncInvoiceIsPosted (new_invoice))
    {
        gboolean result = gncInvoiceUnpost(new_invoice, TRUE);
        if (!result)
        {
            g_warning("Oops, error when unposting the copied invoice; ignoring.");
        }
    }

    // Unset the invoice ID, let it get allocated later
    gncInvoiceSetID(new_invoice, "");

    // Modify the date to today
    if (new_date)
        entry_date = gnc_time64_get_day_neutral (gdate_to_time64 (*new_date));
    else
        entry_date = gnc_time64_get_day_neutral (gnc_time (NULL));
    gncInvoiceSetDateOpened(new_invoice, entry_date);

    // Also modify the date of all entries to today
    //g_warning("We have %d entries", g_list_length(gncInvoiceGetEntries(new_invoice)));
    g_list_foreach(gncInvoiceGetEntries(new_invoice),
                   &set_gncEntry_date, &entry_date);


    if (open_properties)
    {
        // Open the "properties" pop-up for the invoice...
        iw = gnc_invoice_window_new_invoice (parent, DUP_INVOICE, NULL, NULL, new_invoice);
    }
    else
    {
         // Open the newly created invoice in the "edit" window
        iw = gnc_ui_invoice_edit (parent, new_invoice);
        // Check the ID; set one if necessary
        if (g_strcmp0 (gnc_entry_get_text (GTK_ENTRY (iw->id_entry)), "") == 0)
        {
            gncInvoiceSetID (new_invoice, gncInvoiceNextID(iw->book, &(iw->owner)));
        }
    }
    return iw;
}

InvoiceWindow *
gnc_ui_invoice_new (GtkWindow *parent, GncOwner *owner, QofBook *book)
{
    InvoiceWindow *iw;
    GncOwner inv_owner;

    if (owner)
    {
        gncOwnerCopy (owner, &inv_owner);
    }
    else
        gncOwnerInitCustomer (&inv_owner, NULL); /* XXX: pass in the owner type? */

    /* Make sure required options exist */
    if (!book) return NULL;

    iw = gnc_invoice_window_new_invoice (parent, NEW_INVOICE, book, &inv_owner, NULL);

    return iw;
}

/* Functions for invoice selection widgets */

static void
edit_invoice_direct (GtkWindow *dialog, gpointer invoice, gpointer user_data)
{
    g_return_if_fail (invoice);
    gnc_ui_invoice_edit (gnc_ui_get_main_window (GTK_WIDGET (dialog)), invoice);
}

static void
edit_invoice_cb (GtkWindow *dialog, gpointer inv, gpointer user_data)
{
    GncInvoice *invoice = inv;
    g_return_if_fail (invoice && user_data);
    edit_invoice_direct (dialog, invoice, user_data);
}


struct multi_edit_invoice_data
{
    gpointer   user_data;
    GtkWindow *parent;
    gchar     *report_guid;
};

static void
multi_edit_invoice_one (gpointer inv, gpointer user_data)
{
    struct multi_edit_invoice_data *meid = user_data;
    edit_invoice_cb (meid->parent, inv, meid->user_data);
}

static void
multi_edit_invoice_cb (GtkWindow *dialog, GList *invoice_list, gpointer user_data)
{
    struct multi_edit_invoice_data meid;

    meid.user_data = user_data;
    meid.parent = dialog;
    g_list_foreach (invoice_list, multi_edit_invoice_one, &meid);
}

static void
pay_invoice_direct (GtkWindow *dialog, gpointer inv, gpointer user_data)
{
    GncInvoice *invoice = inv;

    g_return_if_fail (invoice);
    gnc_ui_payment_new_with_invoice (dialog, gncInvoiceGetOwner (invoice),
                                     gncInvoiceGetBook (invoice), invoice);
}

static void
pay_invoice_cb (GtkWindow *dialog, gpointer *invoice_p, gpointer user_data)
{
    g_return_if_fail (invoice_p && user_data);
    if (! *invoice_p)
        return;
    pay_invoice_direct (dialog, *invoice_p, user_data);
}

typedef struct
{
    GWeakRef parent;
    QofBook *book;
    GList *invoice_guids;
} MultiDuplicateInvoiceRequest;

static void
multi_duplicate_invoice_request_free (MultiDuplicateInvoiceRequest *request)
{
    g_weak_ref_clear (&request->parent);
    g_list_free_full (request->invoice_guids, g_free);
    g_free (request);
}

static void
multi_duplicate_invoice_date_finished (GncDupTransResult *result,
                                       gpointer user_data)
{
    MultiDuplicateInvoiceRequest *request = user_data;
    GtkWindow *parent = GTK_WINDOW (g_weak_ref_get (&request->parent));

    if (result && parent && request->book == gnc_get_current_book ())
    {
        for (GList *node = request->invoice_guids; node; node = node->next)
        {
            GncInvoice *invoice = gncInvoiceLookup (request->book, node->data);
            if (invoice)
                gnc_ui_invoice_duplicate (parent, invoice, FALSE, &result->gdate);
        }
    }

    g_clear_object (&parent);
    gnc_dup_trans_result_free (result);
    multi_duplicate_invoice_request_free (request);
}

static void
multi_duplicate_invoice_cb (GtkWindow *dialog, GList *invoice_list, gpointer user_data)
{
    (void)user_data;
    g_return_if_fail (invoice_list);
    switch (g_list_length (invoice_list))
    {
    case 0:
        return;
    case 1:
        gnc_ui_invoice_duplicate (dialog, invoice_list->data, TRUE, NULL);
        return;
    default:
    {
        GDate initial_date;
        MultiDuplicateInvoiceRequest *request = g_new0 (MultiDuplicateInvoiceRequest, 1);

        gnc_gdate_set_time64 (&initial_date, gnc_time (NULL));
        request->book = gnc_get_current_book ();
        g_weak_ref_init (&request->parent, dialog);
        for (GList *node = invoice_list; node; node = node->next)
        {
            GncInvoice *invoice = node->data;
            if (invoice && gncInvoiceGetBook (invoice) == request->book)
                request->invoice_guids = g_list_append (
                    request->invoice_guids, g_memdup2 (gncInvoiceGetGUID (invoice),
                                                       sizeof (GncGUID)));
        }
        if (!request->invoice_guids)
        {
            multi_duplicate_invoice_request_free (request);
            return;
        }
        gnc_dup_date_dialog_async (dialog, _("Date of duplicated entries"),
                                   &initial_date,
                                   multi_duplicate_invoice_date_finished, request);
        return;
    }
    }
}
static void gnc_invoice_is_posted(gpointer inv, gpointer test_value)
{
    GncInvoice *invoice = inv;
    gboolean *test = (gboolean*)test_value;

    if (gncInvoiceIsPosted (invoice))
    {
        *test = TRUE;
    }
}


static GList *
invoice_guid_list_copy (GList *invoice_list)
{
    GList *invoice_guids = NULL;
    GList *node;

    for (node = invoice_list; node; node = node->next)
    {
        GncInvoice *invoice = node->data;
        GncGUID *guid;

        if (!invoice)
            continue;
        guid = g_new (GncGUID, 1);
        *guid = *gncInvoiceGetGUID (invoice);
        invoice_guids = g_list_append (invoice_guids, guid);
    }

    return invoice_guids;
}

static void
multi_post_invoice_cb (GtkWindow *dialog, GList *invoice_list, gpointer user_data)
{
    GList *invoice_guids;
    gboolean test;
    InvoiceWindow *iw;

    (void)user_data;
    if (!gnc_list_length_cmp (invoice_list, 0))
        return;

    iw = gnc_ui_invoice_edit (dialog, invoice_list->data);
    if (!iw)
        return;

    test = FALSE;
    gnc_suspend_gui_refresh ();
    g_list_foreach (invoice_list, gnc_invoice_is_posted, &test);
    gnc_resume_gui_refresh ();
    if (test)
    {
        gnc_error_dialog (GTK_WINDOW (iw_get_window (iw)), "%s",
                          _("One or more selected invoices have already been posted.\n"
                            "Re-check your selection."));
        return;
    }

    invoice_guids = invoice_guid_list_copy (invoice_list);
    if (!invoice_guids)
        return;

    invoice_post_request (iw, _("Do you really want to post these invoices?"),
                          invoice_guids);
}

typedef struct
{
    QofBook *book;
    GList *invoice_guids;
} InvoiceMultiPrintRequest;

static void
invoice_multi_print_request_free (InvoiceMultiPrintRequest *request)
{
    g_list_free_full (request->invoice_guids, g_free);
    g_free (request);
}

static void
invoice_multi_print_template_finished (GtkWindow *parent, char *report_guid,
                                       gpointer user_data)
{
    InvoiceMultiPrintRequest *request = user_data;
    GList *node;

    if (parent && report_guid && !qof_book_shutting_down (request->book))
    {
        GtkWindow *print_parent = gnc_ui_get_main_window (GTK_WIDGET (parent));

        for (node = request->invoice_guids; node; node = node->next)
        {
            GncInvoice *invoice = gncInvoiceLookup (request->book, node->data);

            if (invoice)
                gnc_invoice_window_print_invoice (print_parent, invoice,
                                                  report_guid);
        }
    }

    g_free (report_guid);
    invoice_multi_print_request_free (request);
}

static void
multi_print_invoice_cb (GtkWindow *dialog, GList *invoice_list, gpointer user_data)
{
    GncInvoice *invoice;
    InvoiceMultiPrintRequest *request;

    (void)user_data;
    if (!gnc_list_length_cmp (invoice_list, 0))
        return;

    invoice = invoice_list->data;
    if (!invoice)
        return;

    request = g_new0 (InvoiceMultiPrintRequest, 1);
    request->book = qof_instance_get_book (QOF_INSTANCE (invoice));
    request->invoice_guids = invoice_guid_list_copy (invoice_list);
    if (!request->invoice_guids)
    {
        invoice_multi_print_request_free (request);
        return;
    }

    use_default_report_template_or_change_async (
        dialog, invoice_multi_print_template_finished, request);
}

static gpointer
new_invoice_cb (GtkWindow *dialog, gpointer user_data)
{
    struct _invoice_select_window *sw = user_data;
    InvoiceWindow *iw;

    g_return_val_if_fail (user_data, NULL);

    iw = gnc_ui_invoice_new (dialog, sw->owner, sw->book);
    return iw_get_invoice (iw);
}

static void
free_invoice_cb (gpointer user_data)
{
    struct _invoice_select_window *sw = user_data;

    g_return_if_fail (sw);

    qof_query_destroy (sw->q);
    g_free (sw);
}

GNCSearchWindow *
gnc_invoice_search (GtkWindow *parent, GncInvoice *start, GncOwner *owner, QofBook *book)
{
    QofIdType type = GNC_INVOICE_MODULE_NAME;
    struct _invoice_select_window *sw;
    QofQuery *q, *q2 = NULL;
    GncOwnerType owner_type = GNC_OWNER_CUSTOMER;
    static GList *inv_params = NULL, *bill_params = NULL, *emp_params = NULL, *params;
    static GList *columns = NULL;
    const gchar *title, *label, *style_class;
    static GNCSearchCallbackButton *buttons;
    static GNCSearchCallbackButton inv_buttons[] =
    {
        { N_("View/Edit Invoice"), NULL, multi_edit_invoice_cb, TRUE},
        { N_("Process Payment"), pay_invoice_cb, NULL, FALSE},
        { N_("Duplicate"), NULL, multi_duplicate_invoice_cb, FALSE},
        { N_("Post"), NULL, multi_post_invoice_cb, FALSE},
        { N_("Printable Report"), NULL, multi_print_invoice_cb, TRUE},
        { NULL },
    };
    static GNCSearchCallbackButton bill_buttons[] =
    {
        { N_("View/Edit Bill"), NULL, multi_edit_invoice_cb, TRUE},
        { N_("Process Payment"), pay_invoice_cb, NULL, FALSE},
        { N_("Duplicate"), NULL, multi_duplicate_invoice_cb, FALSE},
        { N_("Post"), NULL, multi_post_invoice_cb, FALSE},
        { N_("Printable Report"), NULL, multi_print_invoice_cb, TRUE},
        { NULL },
    };
    static GNCSearchCallbackButton emp_buttons[] =
    {
        /* Translators: The terms 'Voucher' and 'Expense Voucher' are used
           interchangeably in gnucash and mean the same thing. */
        { N_("View/Edit Voucher"), NULL, multi_edit_invoice_cb, TRUE},
        { N_("Process Payment"), pay_invoice_cb, NULL, FALSE},
        { N_("Duplicate"), NULL, multi_duplicate_invoice_cb, FALSE},
        { N_("Post"), NULL, multi_post_invoice_cb, FALSE},
        { N_("Printable Report"), NULL, multi_print_invoice_cb, TRUE},
        { NULL },
    };

    g_return_val_if_fail (book, NULL);

    /* Build parameter list in reverse order */
    if (inv_params == NULL)
    {
        inv_params = gnc_search_param_prepend (inv_params,
                                               _("Invoice Owner"), NULL, type,
                                               INVOICE_OWNER, NULL);
        inv_params = gnc_search_param_prepend (inv_params,
                                               _("Invoice Notes"), NULL, type,
                                               INVOICE_NOTES, NULL);
        inv_params = gnc_search_param_prepend (inv_params,
                                               _("Billing ID"), NULL, type,
                                               INVOICE_BILLINGID, NULL);
        inv_params = gnc_search_param_prepend (inv_params,
                                               _("Is Paid?"), NULL, type,
                                               INVOICE_IS_PAID, NULL);
        inv_params = gnc_search_param_prepend (inv_params,
                                               _("Date Posted"), NULL, type,
                                               INVOICE_POSTED, NULL);
        inv_params = gnc_search_param_prepend (inv_params,
                                               _("Is Posted?"), NULL, type,
                                               INVOICE_IS_POSTED, NULL);
        inv_params = gnc_search_param_prepend (inv_params,
                                               _("Date Opened"), NULL, type,
                                               INVOICE_OPENED, NULL);
        inv_params = gnc_search_param_prepend (inv_params,
                                               _("Due Date"), NULL, type,
                                               INVOICE_DUE, NULL);
        inv_params = gnc_search_param_prepend (inv_params,
                                               _("Company Name"), NULL, type,
                                               INVOICE_OWNER, OWNER_PARENT,
                                               OWNER_NAME, NULL);
        inv_params = gnc_search_param_prepend (inv_params,
                                               _("Invoice ID"), NULL, type,
                                               INVOICE_ID, NULL);
    }
    if (bill_params == NULL)
    {
        bill_params = gnc_search_param_prepend (bill_params,
                                                _("Bill Owner"), NULL, type,
                                                INVOICE_OWNER, NULL);
        bill_params = gnc_search_param_prepend (bill_params,
                                                _("Bill Notes"), NULL, type,
                                                INVOICE_NOTES, NULL);
        bill_params = gnc_search_param_prepend (bill_params,
                                                _("Billing ID"), NULL, type,
                                                INVOICE_BILLINGID, NULL);
        bill_params = gnc_search_param_prepend (bill_params,
                                                _("Is Paid?"), NULL, type,
                                                INVOICE_IS_PAID, NULL);
        bill_params = gnc_search_param_prepend (bill_params,
                                                _("Date Posted"), NULL, type,
                                                INVOICE_POSTED, NULL);
        bill_params = gnc_search_param_prepend (bill_params,
                                                _("Is Posted?"), NULL, type,
                                                INVOICE_IS_POSTED, NULL);
        bill_params = gnc_search_param_prepend (bill_params,
                                                _("Date Opened"), NULL, type,
                                                INVOICE_OPENED, NULL);
        bill_params = gnc_search_param_prepend (bill_params,
                                                _("Due Date"), NULL, type,
                                                INVOICE_DUE, NULL);
        bill_params = gnc_search_param_prepend (bill_params,
                                                _("Company Name"), NULL, type,
                                                INVOICE_OWNER, OWNER_PARENT,
                                                OWNER_NAME, NULL);
        bill_params = gnc_search_param_prepend (bill_params,
                                                _("Bill ID"), NULL, type,
                                                INVOICE_ID, NULL);
    }
    if (emp_params == NULL)
    {
        emp_params = gnc_search_param_prepend (emp_params,
                                               _("Voucher Owner"), NULL, type,
                                               INVOICE_OWNER, NULL);
        emp_params = gnc_search_param_prepend (emp_params,
                                               _("Voucher Notes"), NULL, type,
                                               INVOICE_NOTES, NULL);
        emp_params = gnc_search_param_prepend (emp_params,
                                               _("Billing ID"), NULL, type,
                                               INVOICE_BILLINGID, NULL);
        emp_params = gnc_search_param_prepend (emp_params,
                                               _("Is Paid?"), NULL, type,
                                               INVOICE_IS_PAID, NULL);
        emp_params = gnc_search_param_prepend (emp_params,
                                               _("Date Posted"), NULL, type,
                                               INVOICE_POSTED, NULL);
        emp_params = gnc_search_param_prepend (emp_params,
                                               _("Is Posted?"), NULL, type,
                                               INVOICE_IS_POSTED, NULL);
        emp_params = gnc_search_param_prepend (emp_params,
                                               _("Date Opened"), NULL, type,
                                               INVOICE_OPENED, NULL);
        emp_params = gnc_search_param_prepend (emp_params,
                                               _("Due Date"), NULL, type,
                                               INVOICE_DUE, NULL);
        emp_params = gnc_search_param_prepend (emp_params,
                                               _("Employee Name"), NULL, type,
                                               INVOICE_OWNER, OWNER_PARENT,
                                               OWNER_NAME, NULL);
        emp_params = gnc_search_param_prepend (emp_params,
                                               _("Voucher ID"), NULL, type,
                                               INVOICE_ID, NULL);
    }

    /* Build the column list in reverse order */
    if (columns == NULL)
    {
        columns = gnc_search_param_prepend (columns, _("Billing ID"), NULL, type,
                                            INVOICE_BILLINGID, NULL);
        columns = gnc_search_param_prepend (columns, _("Type"), NULL, type,
                                            INVOICE_TYPE_STRING, NULL);
        columns = gnc_search_param_prepend_with_justify (columns, _("Paid"),
                  GTK_JUSTIFY_CENTER, NULL, type,
                  INVOICE_IS_PAID, NULL);
        columns = gnc_search_param_prepend (columns, _("Posted"), NULL, type,
                                            INVOICE_POSTED, NULL);
        columns = gnc_search_param_prepend (columns, _("Company"), NULL, type,
                                            INVOICE_OWNER, OWNER_PARENT,
                                            OWNER_NAME, NULL);
        columns = gnc_search_param_prepend (columns, _("Due"), NULL, type,
                                            INVOICE_DUE, NULL);
        columns = gnc_search_param_prepend (columns, _("Opened"), NULL, type,
                                            INVOICE_OPENED, NULL);
        columns = gnc_search_param_prepend (columns, _("Num"), NULL, type,
                                            INVOICE_ID, NULL);
    }

    /* Build the queries */
    q = qof_query_create_for (type);
    qof_query_set_book (q, book);

    /* If owner is supplied, limit all searches to invoices who's owner
     * or end-owner is the supplied owner!  Show all invoices by this
     * owner.  If a Job is supplied, search for all invoices for that
     * job, but if a Customer is supplied, search for all invoices owned
     * by that Customer or any of that Customer's Jobs.  In other words,
     * match on <supplied-owner's guid> == Invoice->Owner->GncGUID or
     * Invoice->owner->parentGUID.
     */
    if (owner)
    {
        /* First, figure out the type of owner here.. */
        owner_type = gncOwnerGetType (gncOwnerGetEndOwner (owner));

        /* Then if there's an actual owner add it to the query
         * and limit the search to this owner
         * If there's only a type, limit the search to this type.
         */
        if (gncOwnerGetGUID (owner))
        {
            q2 = qof_query_create ();
            qof_query_add_guid_match (q2, g_slist_prepend
                                      (g_slist_prepend (NULL, QOF_PARAM_GUID),
                                       INVOICE_OWNER),
                                      gncOwnerGetGUID (owner), QOF_QUERY_OR);

            qof_query_add_guid_match (q2, g_slist_prepend
                                      (g_slist_prepend (NULL, OWNER_PARENTG),
                                       INVOICE_OWNER),
                                      gncOwnerGetGUID (owner), QOF_QUERY_OR);
            qof_query_merge_in_place (q, q2, QOF_QUERY_AND);
            qof_query_destroy (q2);

            /* Use this base query as pre-fill query.
             * This will pre-fill the search dialog with the query results
             */
            q2 = qof_query_copy (q);

        }
        else
        {
            QofQuery *q3 = qof_query_create ();
            QofQueryPredData *inv_type_pred = NULL;
            GList *type_list = NULL, *node = NULL;

            type_list = gncInvoiceGetTypeListForOwnerType(owner_type);
            for (node = type_list; node; node = node->next)
            {
                inv_type_pred = qof_query_int32_predicate(QOF_COMPARE_EQUAL,
                                GPOINTER_TO_INT(node->data));
                qof_query_add_term (q3, g_slist_prepend (NULL, INVOICE_TYPE), inv_type_pred, QOF_QUERY_OR);
            }
            qof_query_merge_in_place (q, q3, QOF_QUERY_AND);
            qof_query_destroy (q3);

            /* Don't set a pre-fill query in this case, the result set would be too long */
            q2 = NULL;
        }
    }

    /* Launch select dialog and return the result */
    sw = g_new0 (struct _invoice_select_window, 1);

    if (owner)
    {
        gncOwnerCopy (owner, &(sw->owner_def));
        sw->owner = &(sw->owner_def);
    }
    sw->book = book;
    sw->q = q;

    switch (owner_type)
    {
    case GNC_OWNER_VENDOR:
        title = _("Find Bill");
        label = _("Bill");
        style_class = "gnc-class-bills";
        params = bill_params;
        buttons = bill_buttons;
        break;
    case GNC_OWNER_EMPLOYEE:
        title = _("Find Expense Voucher");
        label = _("Expense Voucher");
        style_class = "gnc-class-vouchers";
        params = emp_params;
        buttons = emp_buttons;
        break;
    default:
        title = _("Find Invoice");
        label = _("Invoice");
        style_class = "gnc-class-invoices";
        params = inv_params;
        buttons = inv_buttons;
        break;
    }
    return gnc_search_dialog_create (parent, type, title, params, columns, q, q2,
                                     buttons, NULL, new_invoice_cb,
                                     sw, free_invoice_cb, GNC_PREFS_GROUP_SEARCH,
                                     label, style_class);
}

DialogQueryView *
gnc_invoice_show_docs_due (GtkWindow *parent, QofBook *book, double days_in_advance, GncWhichDueType duetype)
{
    QofIdType type = GNC_INVOICE_MODULE_NAME;
    Query *q;
    QofQueryPredData* pred_data;
    time64 end_date;
    GList *res;
    gchar *message, *title;
    gchar *prefs_group;
    DialogQueryView *dialog;
    gint len;
    static GList *param_list = NULL;
    static GNCDisplayViewButton vendorbuttons[] =
    {
        { N_("View/Edit Bill"), edit_invoice_direct },
        { N_("Process Payment"), pay_invoice_direct },
        { NULL },
    };
    static GNCDisplayViewButton customerbuttons[] =
    {
        { N_("View/Edit Invoice"), edit_invoice_direct },
        { N_("Process Payment"), pay_invoice_direct },
        { NULL },
    };

    if (!book)
    {
        PERR("No book, no due invoices.");
        return NULL;
    }

    /* Create the param list (in reverse order) */
    if (param_list == NULL)
    {
        param_list = gnc_search_param_prepend_with_justify (param_list, _("Amount"),
                                                            GTK_JUSTIFY_RIGHT, NULL, type,
                                                            INVOICE_POST_LOT, LOT_BALANCE, NULL);
        param_list = gnc_search_param_prepend (param_list, _("Type"), NULL, type,
                                               INVOICE_TYPE_STRING, NULL);
        param_list = gnc_search_param_prepend (param_list, _("Company"), NULL, type,
                                               INVOICE_OWNER, OWNER_PARENT, OWNER_NAME, NULL);
        param_list = gnc_search_param_prepend (param_list, _("Due"), NULL, type,
                                               INVOICE_DUE, NULL);
    }

    /* Create the query to search for invoices; set the book */
    q = qof_query_create();
    qof_query_search_for(q, GNC_INVOICE_MODULE_NAME);
    qof_query_set_book (q, book);

    /* For vendor bills we want to find all invoices where:
     *      invoice -> is_posted == TRUE
     * AND  invoice -> lot -> is_closed? == FALSE
     * AND  invoice -> type != customer invoice
     * AND  invoice -> type != customer credit note
     * AND  invoice -> due <= (today + days_in_advance)
     */

    /* For customer invoices we want to find all invoices where:
     *      invoice -> is_posted == TRUE
     * AND  invoice -> lot -> is_closed? == FALSE
     * AND  invoice -> type != vendor bill
     * AND  invoice -> type != vendor credit note
     * AND  invoice -> type != employee voucher
     * AND  invoice -> type != employee credit note
     * AND  invoice -> due <= (today + days_in_advance)
     * This could probably also be done by searching for customer invoices OR customer credit notes
     * but that would make a more complicated query to compose.
     */

    qof_query_add_boolean_match (q, g_slist_prepend(NULL, INVOICE_IS_POSTED), TRUE,
                                 QOF_QUERY_AND);

    qof_query_add_boolean_match (q, g_slist_prepend(g_slist_prepend(NULL, LOT_IS_CLOSED),
                                 INVOICE_POST_LOT), FALSE, QOF_QUERY_AND);


    if (duetype == DUE_FOR_VENDOR)
    {
        pred_data = qof_query_int32_predicate (QOF_COMPARE_NEQ, GNC_INVOICE_CUST_INVOICE);
        qof_query_add_term (q, g_slist_prepend(NULL, INVOICE_TYPE), pred_data, QOF_QUERY_AND);

        pred_data = qof_query_int32_predicate (QOF_COMPARE_NEQ, GNC_INVOICE_CUST_CREDIT_NOTE);
        qof_query_add_term (q, g_slist_prepend(NULL, INVOICE_TYPE), pred_data, QOF_QUERY_AND);
    }
    else
    {
        pred_data = qof_query_int32_predicate (QOF_COMPARE_NEQ, GNC_INVOICE_VEND_INVOICE);
        qof_query_add_term (q, g_slist_prepend(NULL, INVOICE_TYPE), pred_data, QOF_QUERY_AND);

        pred_data = qof_query_int32_predicate (QOF_COMPARE_NEQ, GNC_INVOICE_VEND_CREDIT_NOTE);
        qof_query_add_term (q, g_slist_prepend(NULL, INVOICE_TYPE), pred_data, QOF_QUERY_AND);

        pred_data = qof_query_int32_predicate (QOF_COMPARE_NEQ, GNC_INVOICE_EMPL_INVOICE);
        qof_query_add_term (q, g_slist_prepend(NULL, INVOICE_TYPE), pred_data, QOF_QUERY_AND);

        pred_data = qof_query_int32_predicate (QOF_COMPARE_NEQ, GNC_INVOICE_EMPL_CREDIT_NOTE);
        qof_query_add_term (q, g_slist_prepend(NULL, INVOICE_TYPE), pred_data, QOF_QUERY_AND);
    }

    end_date = gnc_time (NULL);
    if (days_in_advance < 0)
        days_in_advance = 0;
    end_date += days_in_advance * 60 * 60 * 24;

    pred_data = qof_query_date_predicate (QOF_COMPARE_LTE, QOF_DATE_MATCH_NORMAL, end_date);
    qof_query_add_term (q, g_slist_prepend(NULL, INVOICE_DUE), pred_data, QOF_QUERY_AND);

    res = qof_query_run(q);
    len = g_list_length (res);
    if (!res || len <= 0)
    {
        qof_query_destroy(q);
        return NULL;
    }

    if (duetype == DUE_FOR_VENDOR)
    {
        prefs_group = GNC_PREFS_GROUP_VENDOR;
        message = g_strdup_printf
                  (/* Translators: %d is the number of bills/credit notes due. This is a
                         ngettext(3) message. */
                      ngettext("The following vendor business item is due:",
                               "The following %d vendor business items are due:",
                               len),
                      len);
        title = _("Due Bills Reminder");
    }
    else
    {
        prefs_group = GNC_PREFS_GROUP_CUSTOMER;
        message = g_strdup_printf
                  (/* Translators: %d is the number of invoices/credit notes due. This is a
                         ngettext(3) message. */
                      ngettext("The following customer business item is due:",
                               "The following %d customer business items are due:",
                               len),
                      len);
        title = _("Due Invoices Reminder");
    }

    dialog = gnc_dialog_query_view_create(parent, param_list, q,
                                          title,
                                          message,
                                          TRUE, FALSE,
                                          1, GTK_SORT_ASCENDING,
                                          1, // columns start from 0
                                          duetype == DUE_FOR_VENDOR ?
                                                  vendorbuttons :
                                                  customerbuttons,
                                           prefs_group, NULL);

    g_free(message);
    qof_query_destroy(q);
    return dialog;
}

void
gnc_invoice_remind_bills_due (GtkWindow *parent)
{
    QofBook *book;
    gint days;

    if (!gnc_current_session_exist()) return;
    book = qof_session_get_book(gnc_get_current_session());
    days = gnc_prefs_get_float(GNC_PREFS_GROUP_BILL, GNC_PREF_DAYS_IN_ADVANCE);

    gnc_invoice_show_docs_due (parent, book, days, DUE_FOR_VENDOR);
}

void
gnc_invoice_remind_invoices_due (GtkWindow *parent)
{
    QofBook *book;
    gint days;

    if (!gnc_current_session_exist()) return;
    book = qof_session_get_book(gnc_get_current_session());
    days = gnc_prefs_get_float(GNC_PREFS_GROUP_INVOICE, GNC_PREF_DAYS_IN_ADVANCE);

    gnc_invoice_show_docs_due (parent, book, days, DUE_FOR_CUSTOMER);
}

void
gnc_invoice_remind_bills_due_cb (void)
{
    if (!gnc_prefs_get_bool(GNC_PREFS_GROUP_BILL, GNC_PREF_NOTIFY_WHEN_DUE))
        return;

    gnc_invoice_remind_bills_due (GTK_WINDOW(gnc_ui_get_main_window (NULL)));
}

void
gnc_invoice_remind_invoices_due_cb (void)
{
    if (!gnc_prefs_get_bool(GNC_PREFS_GROUP_INVOICE, GNC_PREF_NOTIFY_WHEN_DUE))
        return;

    gnc_invoice_remind_invoices_due (GTK_WINDOW(gnc_ui_get_main_window (NULL)));
}
