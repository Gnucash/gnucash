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
#include "gncOrder.h"
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
void gnc_invoice_terms_changed_cb (GtkWidget *widget, gpointer data);

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
typedef struct
{
    InvoiceWindow *owner;
    QofBook *book;
    QofSession *session;
    GncGUID book_guid;
    GncGUID invoice_guid;
    GncGUID entry_guid;
    GncGUID order_guid;
    gboolean has_order;
} InvoiceEntryDelete;

typedef struct _InvoiceRecordRequest InvoiceRecordRequest;

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
    InvoiceEntryDelete *entry_delete_request;
    struct _InvoiceLedgerSaveRequest *ledger_save_request;
    InvoiceRecordRequest *entry_commit_request;
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
};

struct _InvoiceRecordRequest
{
    InvoiceWindow *owner;
    gboolean blank;
};

typedef enum
{
    INVOICE_LEDGER_SAVE,
    INVOICE_LEDGER_POST
} InvoiceLedgerAction;

typedef struct _InvoiceLedgerSaveRequest
{
    InvoiceWindow *owner;
    InvoiceLedgerAction action;
    struct post_invoice_params *post_params;
    gboolean async_pending;
    GWeakRef book;
    GSourceFunc completed;
    gpointer completed_data;
} InvoiceLedgerSaveRequest;

static void gnc_invoice_check_ledger (InvoiceWindow *iw,
                                      InvoiceLedgerAction action,
                                      struct post_invoice_params *post_params);
static void invoice_post_dates_response (gboolean accepted,
                                         time64 due_date, time64 post_date,
                                         char *memo, Account *account,
                                         gboolean accumulate,
                                         gpointer user_data);

/* Forward definitions for CB functions */
void gnc_invoice_window_active_toggled_cb (GtkWidget *widget, gpointer data);
gboolean gnc_invoice_window_leave_notes_cb (GtkWidget *widget, GdkEventFocus *event, gpointer data);
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

typedef struct
{
    InvoiceWindow *iw;
    GtkToggleButton *reset_tax_tables;
    gboolean reset_tax_tables_value;
} InvoiceUnpostRequest;

static GncInvoice *iw_get_invoice (InvoiceWindow *iw);

static void
iw_ask_unpost_capture_response (GtkDialog *dialog, gint response,
                                InvoiceUnpostRequest *request)
{
    if (response == GTK_RESPONSE_OK)
        request->reset_tax_tables_value =
            gtk_toggle_button_get_active (request->reset_tax_tables);
}

static void
iw_ask_unpost_completed (GtkWindow *parent, gint response,
                         gpointer user_data)
{
    InvoiceUnpostRequest *request = user_data;
    InvoiceWindow *iw = request->iw;
    GncInvoice *invoice;
    gboolean result;

    if (!parent || !iw || parent != GTK_WINDOW (iw_get_window (iw)) ||
        response != GTK_RESPONSE_OK)
        goto unpost_done;

    invoice = iw_get_invoice (iw);
    if (!invoice)
        goto unpost_done;

    iw->reset_tax_tables = request->reset_tax_tables_value;
    gnc_suspend_gui_refresh ();
    result = gncInvoiceUnpost (invoice, iw->reset_tax_tables);
    gnc_resume_gui_refresh ();
    if (!result)
        goto unpost_done;

    /* If unposting succeeded, reset the ledger and redisplay. */
    iw->dialog_type = EDIT_INVOICE;
    gnc_entry_ledger_set_readonly (iw->ledger, FALSE);
    gnc_invoice_update_window (iw, NULL);
    gnc_table_refresh_gui (gnc_entry_ledger_get_table (iw->ledger), FALSE);

unpost_done:
    g_free (request);
}

static void
iw_ask_unpost (InvoiceWindow *iw)
{
    GtkWidget *dialog;
    GtkToggleButton *toggle;
    GtkBuilder *builder;
    InvoiceUnpostRequest *request;
    const gchar *style_label = NULL;
    GncOwnerType owner_type = gncOwnerGetType (&iw->owner);


    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-invoice.glade", "unpost_message_dialog");
    dialog = GTK_WIDGET (gtk_builder_get_object (builder, "unpost_message_dialog"));
    toggle = GTK_TOGGLE_BUTTON(gtk_builder_get_object (builder, "yes_tt_reset"));

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
    // Set a secondary style context for this page so it can be easily manipulated with css
    gnc_widget_style_context_add_class (GTK_WIDGET(dialog), style_label);

    gtk_window_set_transient_for (GTK_WINDOW(dialog),
                                  GTK_WINDOW(iw_get_window(iw)));

    request = g_new0 (InvoiceUnpostRequest, 1);
    request->iw = iw;
    request->reset_tax_tables = toggle;
    g_signal_connect (dialog, "response",
                      G_CALLBACK (iw_ask_unpost_capture_response), request);
    gnc_dialog_run_async (GTK_DIALOG (dialog), NULL,
                          iw_ask_unpost_completed, request);
    g_object_unref(G_OBJECT(builder));
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
        gncInvoiceSetActive (invoice, gtk_toggle_button_get_active
                             (GTK_TOGGLE_BUTTON (iw->active_check)));

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
        gncInvoiceSetID (invoice, gtk_entry_get_text (GTK_ENTRY (iw->id_entry)));
        gncInvoiceSetBillingID (invoice, gtk_entry_get_text (GTK_ENTRY (iw->billing_id_entry)));
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
        gnc_error_dialog_async (GTK_WINDOW (iw_get_window(iw)), "%s",
                                /* Translators: In this context,
                                   'Billing information' maps to the
                                   label in the frame and means
                                   e.g. customer i.e. the company being
                                   invoiced. */
                                _("You need to supply Billing Information."));
        return FALSE;
    }

    /* Check the ID; set one if necessary */
    res = gtk_entry_get_text (GTK_ENTRY (iw->id_entry));
    if (g_strcmp0 (res, "") == 0)
    {
        /* Invoices and bills have separate counters.
           Therefore we pass the GncOwer to gncInvoiceNextID
           so it knows whether we are creating a bill
           or an invoice. */
        string = gncInvoiceNextID(iw->book, &(iw->owner));
        gtk_entry_set_text (GTK_ENTRY (iw->id_entry), string);
        g_free(string);
    }

    return TRUE;
}

static gboolean
invoice_ledger_save_request_is_current (InvoiceLedgerSaveRequest *request,
                                       InvoiceWindow *iw)
{
    if (!request || !iw || request->owner != iw || iw->ledger_save_request != request)
        return FALSE;
    QofBook *book = g_weak_ref_get(&request->book);
    gboolean current = book && gnc_current_session_exist() &&
        book == gnc_get_current_book() && !qof_book_shutting_down(book);
    g_clear_object(&book);
    return current;
}

static gboolean
gnc_invoice_window_ok_save (InvoiceWindow *iw,
                            InvoiceLedgerSaveRequest *request)
{
    if (!gnc_invoice_window_verify_ok (iw))
        return FALSE;
    if (!invoice_ledger_save_request_is_current (request, iw))
        return FALSE;

    {
        GncInvoice *invoice = iw_get_invoice (iw);
        if (invoice)
        {
            gnc_ui_to_invoice (iw, invoice);
        }
        if (!invoice_ledger_save_request_is_current (request, iw))
            return FALSE;
        /* Save the invoice to return it later. */
        iw->created_invoice = invoice;
    }
    return TRUE;
}

void
gnc_invoice_window_ok_cb (GtkWidget *widget, gpointer data)
{
    InvoiceWindow *iw = data;
    if (!iw || iw->ledger_save_request)
        return;
    gnc_invoice_check_ledger (iw, INVOICE_LEDGER_SAVE, NULL);
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
    if (iw->entry_delete_request)
    {
        iw->entry_delete_request->owner = NULL;
        iw->entry_delete_request = NULL;
    }
    if (iw->ledger_save_request)
    {
        iw->ledger_save_request->owner = NULL;
        iw->ledger_save_request = NULL;
    }
    if (iw->entry_commit_request)
    {
        iw->entry_commit_request->owner = NULL;
        iw->entry_commit_request = NULL;
    }
    GncInvoice *invoice = iw_get_invoice (iw);

    gnc_suspend_gui_refresh ();

    if ((iw->dialog_type == NEW_INVOICE || iw->dialog_type == DUP_INVOICE)
         && invoice != NULL)
    {
        gncInvoiceRemoveEntries (invoice);
        gncInvoiceBeginEdit (invoice);
        gncInvoiceDestroy (invoice);
        iw->invoice_guid = *guid_null ();
    }

    gtk_widget_destroy(widget);
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
invoice_record_entry_completed (gboolean accepted, gpointer user_data)
{
    InvoiceRecordRequest *request = user_data;
    InvoiceWindow *iw = request->owner;
    if (iw && iw->entry_commit_request == request)
        iw->entry_commit_request = NULL;
    if (accepted && iw && iw->ledger && iw->reg)
    {
        if (!request->blank)
            gnucash_register_goto_next_virt_row (iw->reg);
        else
        {
            VirtualCellLocation vcell_loc;
            GncEntry *blank = gnc_entry_ledger_get_blank_entry (iw->ledger);
            if (blank && gnc_entry_ledger_get_entry_virt_loc (iw->ledger, blank,
                                                               &vcell_loc))
                gnucash_register_goto_virt_cell (iw->reg, vcell_loc);
        }
    }
    g_free (request);
}

void
gnc_invoice_window_recordCB (GtkWidget *widget, gpointer data)
{
    InvoiceWindow *iw = data;

    if (!iw || !iw->ledger || iw->entry_commit_request)
        return;
    InvoiceRecordRequest *request = g_new0 (InvoiceRecordRequest, 1);
    request->owner = iw;
    iw->entry_commit_request = request;
    gnc_entry_ledger_commit_entry_async (iw_get_window (iw), iw->ledger,
                                         invoice_record_entry_completed, request);
}

void
gnc_invoice_window_cancelCB (GtkWidget *widget, gpointer data)
{
    InvoiceWindow *iw = data;

    if (!iw || !iw->ledger || iw->entry_commit_request)
        return;

    gnc_entry_ledger_cancel_cursor_changes (iw->ledger);
}

static void
invoice_entry_delete_response (GtkWindow *parent, gint response, gpointer data)
{
    InvoiceEntryDelete *request = data;
    InvoiceWindow *iw = request->owner;
    GncInvoice *invoice;
    GncEntry *entry;
    GncOrder *order;

    if (iw)
        iw->entry_delete_request = NULL;
    if (!iw || !parent || response != GTK_RESPONSE_YES || !request->book ||
        !gnc_current_session_exist () ||
        gnc_get_current_session () != request->session ||
        qof_session_get_book (request->session) != request->book ||
        !qof_book_is_open (request->book) ||
        qof_book_shutting_down (request->book) ||
        qof_book_is_readonly (request->book) ||
        !guid_equal (qof_book_get_guid (request->book), &request->book_guid) ||
        iw->book != request->book || !iw->ledger ||
        !guid_equal (&iw->invoice_guid, &request->invoice_guid))
        goto cleanup;

    invoice = gncInvoiceLookup (request->book, &request->invoice_guid);
    entry = gncEntryLookup (request->book, &request->entry_guid);
    if (!invoice || !entry || gncInvoiceIsPosted (invoice) ||
        (gncEntryGetInvoice (entry) != invoice && gncEntryGetBill (entry) != invoice) ||
        gnc_entry_ledger_get_current_entry (iw->ledger) != entry ||
        gnc_entry_ledger_get_blank_entry (iw->ledger) == entry)
        goto cleanup;
    order = gncEntryGetOrder (entry);
    if ((order != NULL) != request->has_order ||
        (order && !guid_equal (gncOrderGetGUID (order), &request->order_guid)))
        goto cleanup;

    /* The existing ledger operation owns the deletion. Do not access iw
     * afterwards: its refresh callbacks may close the invoice page. */
    gnc_entry_ledger_delete_current_entry (iw->ledger);

cleanup:
    if (request->book)
        g_object_remove_weak_pointer (G_OBJECT (request->book),
                                      (gpointer *)&request->book);
    g_free (request);
}

void
gnc_invoice_window_deleteCB ([[maybe_unused]] GtkWidget *widget, gpointer data)
{
    InvoiceWindow *iw = data;
    GncEntry *entry;

    if (!iw || !iw->ledger || iw->entry_delete_request)
        return;

    /* get the current entry based on cursor position */
    entry = gnc_entry_ledger_get_current_entry (iw->ledger);
    if (!entry)
    {
        gnc_entry_ledger_cancel_cursor_changes (iw->ledger);
        return;
    }

    /* deleting the blank entry just cancels */
    if (entry == gnc_entry_ledger_get_blank_entry (iw->ledger))
    {
        gnc_entry_ledger_cancel_cursor_changes (iw->ledger);
        return;
    }

    /* Verify that the user really wants to delete this entry */
    {
        const char *message = _("Are you sure you want to delete the "
                                "selected entry?");
        const char *order_warn = _("This entry is attached to an order and "
                                   "will be deleted from that as well!");
        char *msg;
        InvoiceEntryDelete *request = g_new0 (InvoiceEntryDelete, 1);

        if (gncEntryGetOrder (entry))
            msg = g_strconcat (message, "\n\n", order_warn, (char *)NULL);
        else
            msg = g_strdup (message);

        request->owner = iw;
        request->book = iw->book;
        request->session = gnc_get_current_session ();
        request->book_guid = *qof_book_get_guid (iw->book);
        request->invoice_guid = iw->invoice_guid;
        request->entry_guid = *gncEntryGetGUID (entry);
        request->has_order = gncEntryGetOrder (entry) != NULL;
        if (request->has_order)
            request->order_guid = *gncOrderGetGUID (gncEntryGetOrder (entry));
        g_object_add_weak_pointer (G_OBJECT (request->book),
                                   (gpointer *)&request->book);
        iw->entry_delete_request = request;
        gnc_verify_dialog_async (GTK_WINDOW (iw_get_window (iw)), FALSE,
                                  invoice_entry_delete_response, request, "%s", msg);
        g_free (msg);
    }
}

void
gnc_invoice_window_duplicateCB (GtkWidget *widget, gpointer data)
{
    InvoiceWindow *iw = data;

    if (!iw || !iw->ledger)
        return;

    gnc_entry_ledger_duplicate_current_entry (iw->ledger);
}

void
gnc_invoice_window_blankCB (GtkWidget *widget, gpointer data)
{
    InvoiceWindow *iw = data;

    if (!iw || !iw->ledger)
        return;

    InvoiceRecordRequest *request = g_new0 (InvoiceRecordRequest, 1);
    request->owner = iw;
    request->blank = TRUE;
    iw->entry_commit_request = request;
    gnc_entry_ledger_commit_entry_async (iw_get_window (iw), iw->ledger,
                                         invoice_record_entry_completed, request);
}

typedef struct dialog_args
{
    GtkProgressBar  *pb;
    GtkWidget       *dialog;
    gdouble          timeout;
} dialog_args;

typedef struct
{
    InvoiceWindow *owner;
    GtkWidget *combo;
    dialog_args *timer;
    gchar *report_guid;
    GWeakRef book;
    GncGUID invoice;
    GArray *invoices;
    GtkWidget *dialog;
} InvoicePrintRequest;

static GncPluginPage *gnc_invoice_window_print_invoice (
    GtkWindow *parent, GncInvoice *invoice, const gchar *report_guid);

static gboolean
update_progress_bar (gpointer user_data)
{
    dialog_args     *args = user_data;
    GtkProgressBar  *pb = args->pb;
    gdouble          frac = gtk_progress_bar_get_fraction (pb);
    gdouble          step = 0.1 / (args->timeout);

    frac -= step;

    if (frac < step)
    {
        gtk_dialog_response (GTK_DIALOG(args->dialog), GTK_RESPONSE_OK);
        return FALSE;
    }
    gtk_progress_bar_set_fraction (pb, frac);
    return TRUE;
}

static void
combo_popped_cb (GObject    *gobject,
                 GParamSpec *pspec,
                 gpointer    user_data)
{
    gboolean popup_shown;

    g_object_get (G_OBJECT(gobject), "popup-shown", &popup_shown, NULL);

    if (popup_shown)
        g_source_remove_by_user_data (user_data);
}

static gboolean
dialog_key_press_event_cb (GtkWidget *widget, GdkEventKey *event,
                           gpointer user_data)
{
     g_source_remove_by_user_data (user_data);
     return FALSE;
}

static void
combo_changed_cb (GtkComboBox *widget, gpointer user_data)
{
    g_source_remove_by_user_data (user_data);
}

static void
invoice_print_capture_response (GtkDialog *dialog, gint response,
                                InvoicePrintRequest *request)
{
    if (response == GTK_RESPONSE_OK)
        request->report_guid = gnc_report_combo_get_active_guid (
            GNC_REPORT_COMBO (request->combo));
}

static gboolean equal_fn(gpointer find_data, gpointer elt_data);

static void
invoice_print_template_completed(GtkWindow *parent, gint response, gpointer data)
{
    InvoicePrintRequest *request = data;
    if (request->timer)
    {
        g_source_remove_by_user_data(request->timer);
        g_signal_handlers_disconnect_by_data(request->combo, request->timer);
        g_signal_handlers_disconnect_by_data(request->dialog, request->timer);
        g_free(request->timer);
    }
    if (request->dialog)
        g_signal_handlers_disconnect_by_data(request->dialog, request);
    QofBook *book = g_weak_ref_get(&request->book);
    if (parent && book && gnc_current_session_exist() && book == gnc_get_current_book() &&
        !qof_book_shutting_down(book) && response == GTK_RESPONSE_OK && request->report_guid)
    {
        if (request->invoices)
        {
            for (guint i = 0; i < request->invoices->len; ++i)
            {
                GncInvoice *invoice = gncInvoiceLookup(book,
                    &g_array_index(request->invoices, GncGUID, i));
                if (invoice)
                    gnc_invoice_window_print_invoice(gnc_ui_get_main_window(GTK_WIDGET(parent)),
                                                       invoice, request->report_guid);
            }
        }
        else
        {
            InvoiceWindow *iw = gnc_find_first_gui_component(DIALOG_VIEW_INVOICE_CM_CLASS,
                                                              equal_fn, request->owner);
            if (!iw)
                iw = gnc_find_first_gui_component(DIALOG_NEW_INVOICE_CM_CLASS,
                                                   equal_fn, request->owner);
            GncInvoice *invoice = gncInvoiceLookup(book, &request->invoice);
            if (iw && invoice && iw_get_invoice(iw) == invoice &&
                parent == GTK_WINDOW(iw_get_window(iw)))
            {
                iw->reportPage = gnc_invoice_window_print_invoice(parent, invoice, request->report_guid);
                if (iw->reportPage)
                    gnc_main_window_open_page(GNC_MAIN_WINDOW(iw->dialog), iw->reportPage);
            }
        }
    }
    g_clear_object(&book);
    g_clear_object(&request->dialog);
    g_clear_object(&request->combo);
    g_weak_ref_clear(&request->book);
    if (request->invoices) g_array_unref(request->invoices);
    g_free(request->report_guid);
    g_free(request);
}

/* Opens the default invoice report after the timeout, or the selected report
 * after an explicit response. Cancellation leaves the invoice window intact. */
static void
use_default_report_template_or_change_async (GtkWindow *parent,
                                             InvoiceWindow *iw, GArray *invoices)
{
    QofBook     *book = gnc_get_current_book ();
    GtkWidget   *combo;
    GtkBuilder  *builder;
    GtkWidget   *dialog;
    GtkWidget   *ok_button;
    GtkWidget   *report_combo_hbox;
    GtkWidget   *progress_bar;
    GtkWidget   *label;
    gchar       *rep_guid = NULL;
    gchar       *rep_name = NULL;
    gboolean     warning_visible = FALSE;
    gdouble      timeout;
    dialog_args *args;
    InvoicePrintRequest *request;

    request = g_new0(InvoicePrintRequest, 1);
    request->owner = iw;
    request->invoices = invoices;
    g_weak_ref_init(&request->book, book);
    if (iw && iw_get_invoice(iw)) request->invoice = *gncInvoiceGetGUID(iw_get_invoice(iw));
    timeout = qof_book_get_default_invoice_report_timeout (book);

    combo = gnc_default_invoice_report_combo ("gnc:custom-report-invoice-template-guids");

    rep_name = qof_book_get_default_invoice_report_name (book);
    rep_guid = gnc_get_default_invoice_print_report ();

    gnc_report_combo_set_active (GNC_REPORT_COMBO(combo),
                                 rep_guid,
                                 rep_name);
    g_free (rep_guid);
    g_free (rep_name);

    warning_visible = gnc_report_combo_is_warning_visible_for_active (GNC_REPORT_COMBO(combo));

    // When timeout is 0, only return if warning not visible
    if (timeout == 0 && !warning_visible)
    {
        request->combo = g_object_ref_sink(combo);
        request->report_guid = gnc_get_default_invoice_print_report();
        invoice_print_template_completed(parent, GTK_RESPONSE_OK, request);
        return;
    }

    builder = gtk_builder_new ();
    gnc_builder_add_from_file (builder, "dialog-invoice.glade", "invoice_print_dialog");

    dialog = GTK_WIDGET(gtk_builder_get_object (builder, "invoice_print_dialog"));

    gtk_window_set_transient_for (GTK_WINDOW(dialog), parent);

    gtk_dialog_set_default_response (GTK_DIALOG(dialog), GTK_RESPONSE_OK);

    ok_button = GTK_WIDGET(gtk_builder_get_object (builder, "ok_button"));
    report_combo_hbox = GTK_WIDGET(gtk_builder_get_object (builder, "report_combo_hbox"));
    progress_bar = GTK_WIDGET(gtk_builder_get_object (builder, "progress_bar"));
    label = GTK_WIDGET(gtk_builder_get_object (builder, "label"));

    gtk_box_pack_start (GTK_BOX(report_combo_hbox), GTK_WIDGET(combo), TRUE, TRUE, 0);

    gtk_widget_grab_focus (ok_button);

    gtk_progress_bar_set_fraction (GTK_PROGRESS_BAR(progress_bar), 1);

    args = g_malloc (sizeof(dialog_args));
    args->dialog = dialog;
    args->pb = GTK_PROGRESS_BAR(progress_bar);
    args->timeout = timeout;

    request->dialog = g_object_ref(dialog);
    request->combo = g_object_ref(combo);
    request->timer = args;

    gtk_widget_show_all (dialog);

    g_object_unref (G_OBJECT(builder));

    g_signal_connect (G_OBJECT(combo), "changed",
                      G_CALLBACK(combo_changed_cb), args);

    g_signal_connect (G_OBJECT(dialog), "key_press_event",
                      G_CALLBACK(dialog_key_press_event_cb), args);

    g_signal_connect (G_OBJECT(combo), "notify::popup-shown",
                      G_CALLBACK (combo_popped_cb), args);

    // if warning visible, do not add args timeout, wait for user
    if (warning_visible)
    {
        gtk_label_set_text (GTK_LABEL(label),
                            N_("Choose a different report template or Printable Invoice will be used"));
        gtk_widget_hide (GTK_WIDGET(progress_bar));
    }
    else
        g_timeout_add (100, update_progress_bar, args);

    g_signal_connect (dialog, "response",
                      G_CALLBACK (invoice_print_capture_response), request);
    gnc_dialog_run_async (GTK_DIALOG (dialog), NULL,
                          invoice_print_template_completed, request);
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

/* From the invoice editor, open the invoice report. This will reuse the
   invoice report if generated from the current invoice editor. Note the
   link is lost when GnuCash is restarted. This link may be restored
   by: scan the current session tabs, identify reports, checking
   whereby report's report-type matches an invoice report, and the
   report's invoice option value matches the current invoice. */
void
gnc_invoice_window_printCB (GtkWindow* parent, gpointer data)
{
    InvoiceWindow *iw = data;

    if (gnc_find_first_gui_component (WINDOW_REPORT_CM_CLASS, equal_fn,
                                      iw->reportPage))
        gnc_plugin_page_report_reload (GNC_PLUGIN_PAGE_REPORT (iw->reportPage));
    else
    {
        use_default_report_template_or_change_async (parent, iw, NULL);
        return;
    }
    if (iw->reportPage)
        gnc_main_window_open_page (GNC_MAIN_WINDOW (iw->dialog), iw->reportPage);
}

static void
gnc_dialog_post_invoice_query (InvoiceWindow *iw, const char *title,
                               GncDateCloseFormResponseCallback callback,
                               gpointer data)
{
    GncInvoice *invoice;
    char *ddue_label, *post_label, *acct_label, *question_label;
    GList * acct_types = NULL;
    GList * acct_commodities = NULL;
    QofInstance *owner_inst;
    EntryList *entries, *entries_iter;
    time64 due_date, post_date;
    Account *account = NULL;
    gboolean accumulate;

    invoice = iw_get_invoice (iw);
    if (!invoice)
        return;

    ddue_label = _("Due Date");
    post_label = _("Post Date");
    acct_label = _("Post to Account");
    question_label = _("Accumulate Splits?");

    /* Determine the type of account to post to */
    acct_types = gncOwnerGetAccountTypesList (&(iw->owner));

    /* Determine which commodity we're working with */
    acct_commodities = gncOwnerGetCommoditiesList(&(iw->owner));

    /* Get the invoice entries */
    entries = gncInvoiceGetEntries (invoice);

    /* Find the most suitable post date.
     * For Customer Invoices that would be today.
     * For Vendor Bills and Employee Vouchers
     * that would be the date of the most recent invoice entry.
     * Failing that, today is used as a fallback */
    post_date = gnc_time(NULL);

    if (entries && ((gncInvoiceGetOwnerType (invoice) == GNC_OWNER_VENDOR) ||
                    (gncInvoiceGetOwnerType (invoice) == GNC_OWNER_EMPLOYEE)))
    {
        post_date = gncEntryGetDate ((GncEntry*)entries->data);
        for (entries_iter = entries; entries_iter != NULL; entries_iter = g_list_next(entries_iter))
        {
            time64 entrydate = gncEntryGetDate ((GncEntry*)entries_iter->data);
            if (entrydate > post_date)
                post_date = entrydate;
        }
    }

    /* Get the due date and posted account */
    due_date = post_date;
    {
    GncGUID *guid = NULL;
    owner_inst = qofOwnerGetOwner (gncOwnerGetEndOwner (&(iw->owner)));
    qof_instance_get (owner_inst,
                      "invoice-last-posted-account", &guid,
                      NULL);
    account = xaccAccountLookup (guid, iw->book);
    }
    /* Get the default for the accumulate option */
    accumulate = gnc_prefs_get_bool(GNC_PREFS_GROUP_INVOICE,
                                     GNC_PREF_ACCUM_SPLITS);
    gnc_dialog_dates_acct_question_async_parented (
        iw_get_window (iw), title,
        ddue_label, post_label, acct_label, question_label, TRUE, TRUE,
        acct_types, acct_commodities, iw->book, iw->terms, due_date,
        post_date, account, accumulate, callback, data);
}

static void
gnc_dialog_post_invoice_async(InvoiceWindow *iw, InvoiceLedgerSaveRequest *request)
{
    request->async_pending = TRUE;
    gnc_dialog_post_invoice_query(iw, _("Do you really want to post the invoice?"),
                                  invoice_post_dates_response, request);
}

struct post_invoice_params
{
    time64 ddue;            /* Due date */
    time64 postdate;        /* Date posted */
    char *memo;             /* Memo for posting transaction */
    Account *acc;           /* Account to post to */
    gboolean accumulate;    /* Whether to accumulate splits */
    GtkWindow *parent;
};

static void invoice_ledger_save_request_finish(InvoiceLedgerSaveRequest *request);

typedef struct
{
    char *currency;
    gnc_numeric amount;
    gnc_numeric rate;
} InvoicePostingCurrency;

typedef struct
{
    InvoiceLedgerSaveRequest *ledger;
    GncGUID invoice;
    GncGUID account;
    char *invoice_currency;
    GPtrArray *currencies;
    guint index;
    gnc_numeric exchange_rate;
} InvoicePostingRequest;

static void invoice_posting_next(InvoicePostingRequest *request);

static void
invoice_posting_currency_free(gpointer data)
{
    InvoicePostingCurrency *currency = data;
    g_free(currency->currency);
    g_free(currency);
}

static GncInvoice *
invoice_posting_current(InvoicePostingRequest *request, Account **account)
{
    InvoiceWindow *iw = request->ledger->owner;
    if (!invoice_ledger_save_request_is_current(request->ledger, iw))
        return NULL;
    GncInvoice *invoice = gncInvoiceLookup(iw->book, &request->invoice);
    *account = xaccAccountLookup(&request->account, iw->book);
    if (!invoice || !*account || invoice != iw_get_invoice(iw) || gncInvoiceIsPosted(invoice))
        return NULL;
    return invoice;
}

static void
invoice_posting_finish(InvoicePostingRequest *request)
{
    InvoiceLedgerSaveRequest *ledger = request->ledger;
    g_ptr_array_unref(request->currencies);
    g_free(request->invoice_currency);
    g_free(request);
    invoice_ledger_save_request_finish(ledger);
}

static void
invoice_posting_rate_response(gboolean accepted, gpointer data)
{
    InvoicePostingRequest *request = data;
    Account *account;
    if (!accepted || !invoice_posting_current(request, &account))
    {
        InvoiceWindow *iw = request->ledger->owner;
        if (invoice_ledger_save_request_is_current(request->ledger, iw))
            gnc_info_dialog(GTK_WINDOW(iw_get_window(iw)), "%s",
                _("The post action was canceled because not all exchange rates were given."));
        invoice_posting_finish(request);
        return;
    }
    InvoicePostingCurrency *currency = g_ptr_array_index(request->currencies, request->index++);
    currency->rate = gnc_numeric_zero_p(request->exchange_rate) ? request->exchange_rate :
        gnc_numeric_div((gnc_numeric){1, 1}, request->exchange_rate,
                        GNC_DENOM_AUTO, GNC_HOW_RND_ROUND_HALF_UP);
    invoice_posting_next(request);
}

static void
invoice_posting_next(InvoicePostingRequest *request)
{
    Account *account;
    GncInvoice *invoice = invoice_posting_current(request, &account);
    if (!invoice)
    {
        invoice_posting_finish(request);
        return;
    }
    InvoiceWindow *iw = request->ledger->owner;
    struct post_invoice_params *params = request->ledger->post_params;
    if (request->index < request->currencies->len)
    {
        InvoicePostingCurrency *item = g_ptr_array_index(request->currencies, request->index);
        gnc_commodity *currency = gnc_commodity_table_lookup_unique(
            gnc_commodity_table_get_table(iw->book), item->currency);
        if (!currency)
        {
            invoice_posting_finish(request);
            return;
        }
        XferDialog *xfer = gnc_xfer_dialog(iw_get_window(iw), account);
        request->exchange_rate = gnc_numeric_zero();
        gnc_xfer_dialog_is_exchange_dialog(xfer, &request->exchange_rate);
        gnc_xfer_dialog_select_to_currency(xfer, currency);
        gnc_xfer_dialog_set_date(xfer, params->postdate);
        gnc_xfer_dialog_set_amount(xfer, gnc_numeric_zero_p(item->amount) ?
                                  (gnc_numeric){1, 1} : item->amount);
        GNCPrice *price = gncInvoiceGetPrice(invoice, currency);
        if (price && !gnc_numeric_zero_p(gnc_price_get_value(price)))
        {
            request->exchange_rate = gnc_numeric_div((gnc_numeric){1, 1},
                gnc_price_get_value(price), GNC_DENOM_AUTO, GNC_HOW_RND_ROUND_HALF_UP);
            gnc_xfer_dialog_set_price_edit(xfer, request->exchange_rate);
        }
        gnc_xfer_dialog_set_from_show_button_active(xfer, FALSE);
        gnc_xfer_dialog_set_to_show_button_active(xfer, FALSE);
        gnc_xfer_dialog_hide_from_account_tree(xfer);
        gnc_xfer_dialog_hide_to_account_tree(xfer);
        gnc_xfer_dialog_run_async(xfer, invoice_posting_rate_response, request);
        return;
    }

    // A different amount/currency invalidates the collected answers.
    GHashTable *foreign = gncInvoiceGetForeignCurrencies(invoice);
    gboolean unchanged = g_hash_table_size(foreign) == request->currencies->len &&
        !g_strcmp0(request->invoice_currency,
                   gnc_commodity_get_unique_name(gncInvoiceGetCurrency(invoice)));
    for (guint i = 0; unchanged && i < request->currencies->len; ++i)
    {
        InvoicePostingCurrency *item = g_ptr_array_index(request->currencies, i);
        gnc_commodity *currency = gnc_commodity_table_lookup_unique(
            gnc_commodity_table_get_table(iw->book), item->currency);
        gnc_numeric *amount = currency ? g_hash_table_lookup(foreign, currency) : NULL;
        unchanged = amount && gnc_numeric_equal(*amount, item->amount);
    }
    g_hash_table_unref(foreign);
    if (!unchanged)
    {
        invoice_posting_finish(request);
        return;
    }
    // Currency answers are collected before the edit. No QOF edit or GUI
    // refresh suspension spans a user decision.
    gnc_suspend_gui_refresh();
    gncInvoiceBeginEdit(invoice);
    for (guint i = 0; i < request->currencies->len; ++i)
    {
        InvoicePostingCurrency *item = g_ptr_array_index(request->currencies, i);
        gnc_commodity *currency = gnc_commodity_table_lookup_unique(
            gnc_commodity_table_get_table(iw->book), item->currency);
        GNCPrice *price = gnc_price_create(iw->book);
        gnc_price_begin_edit(price);
        gnc_price_set_commodity(price, currency);
        gnc_price_set_currency(price, gncInvoiceGetCurrency(invoice));
        gnc_price_set_time64(price, params->postdate);
        gnc_price_set_source(price, PRICE_SOURCE_TEMP);
        gnc_price_set_typestr(price, PRICE_TYPE_LAST);
        gnc_price_set_value(price, item->rate);
        gncInvoiceAddPrice(invoice, price);
        gnc_price_commit_edit(price);
    }
    QofInstance *owner = qofOwnerGetOwner(gncOwnerGetEndOwner(&iw->owner));
    qof_begin_edit(owner);
    qof_instance_set(owner, "invoice-last-posted-account", &request->account, NULL);
    qof_commit_edit(owner);
    gboolean customer = gncInvoiceGetOwnerType(invoice) == GNC_OWNER_CUSTOMER;
    gboolean auto_pay = gnc_prefs_get_bool(customer ? GNC_PREFS_GROUP_INVOICE :
                                          GNC_PREFS_GROUP_BILL, GNC_PREF_AUTO_PAY);
    gncInvoicePostToAccount(invoice, account, params->postdate, params->ddue,
                           params->memo, params->accumulate, auto_pay);
    gncInvoiceCommitEdit(invoice);
    gnc_resume_gui_refresh();
    iw = request->ledger->owner;
    if (invoice_ledger_save_request_is_current(request->ledger, iw))
    {
        iw->dialog_type = VIEW_INVOICE;
        gnc_entry_ledger_set_readonly(iw->ledger, TRUE);
        gnc_invoice_update_window(iw, NULL);
        if (invoice_ledger_save_request_is_current(request->ledger, iw))
            gnc_table_refresh_gui(gnc_entry_ledger_get_table(iw->ledger), FALSE);
    }
    invoice_posting_finish(request);
}

static gboolean
invoice_posting_start(gpointer data)
{
    invoice_posting_next(data);
    return G_SOURCE_REMOVE;
}

static void
gnc_invoice_post_after_ledger(InvoiceWindow *iw,
                             struct post_invoice_params *post_params,
                             InvoiceLedgerSaveRequest *ledger)
{
    GncInvoice *invoice = iw_get_invoice(iw);
    if (!invoice || !invoice_ledger_save_request_is_current(ledger, iw))
        return;
    if (!gncInvoiceGetEntries(invoice))
    {
        gnc_error_dialog_async(GTK_WINDOW(iw_get_window(iw)), "%s",
                               _("The Invoice must have at least one Entry."));
        return;
    }
    if (!post_params)
    {
        gnc_dialog_post_invoice_async(iw, ledger);
        return;
    }
    if (!post_params->acc || qof_instance_get_book(QOF_INSTANCE(post_params->acc)) != iw->book)
        return;
    if (!gnc_invoice_window_ok_save(iw, ledger) ||
        !invoice_ledger_save_request_is_current(ledger, iw))
        return;
    gncInvoiceBeginEdit(invoice);
    gncInvoiceSetCurrency(invoice, gncOwnerGetCurrency(gncInvoiceGetOwner(invoice)));
    gncInvoiceCommitEdit(invoice);
    if (!invoice_ledger_save_request_is_current(ledger, iw))
        return;
    InvoicePostingRequest *request = g_new0(InvoicePostingRequest, 1);
    request->ledger = ledger;
    request->invoice = *gncInvoiceGetGUID(invoice);
    request->account = *xaccAccountGetGUID(post_params->acc);
    request->invoice_currency = g_strdup(
        gnc_commodity_get_unique_name(gncInvoiceGetCurrency(invoice)));
    request->currencies = g_ptr_array_new_with_free_func(invoice_posting_currency_free);
    GHashTable *foreign = gncInvoiceGetForeignCurrencies(invoice);
    GHashTableIter iter;
    gpointer key, value;
    g_hash_table_iter_init(&iter, foreign);
    while (g_hash_table_iter_next(&iter, &key, &value))
    {
        InvoicePostingCurrency *item = g_new0(InvoicePostingCurrency, 1);
        item->currency = g_strdup(gnc_commodity_get_unique_name(key));
        item->amount = *(gnc_numeric *)value;
        g_ptr_array_add(request->currencies, item);
    }
    g_hash_table_unref(foreign);
    if (request->currencies->len)
        gnc_info_dialog(GTK_WINDOW(iw_get_window(iw)), "%s",
            _("One or more of the entries are for accounts different from the invoice/bill currency. You will be asked to enter a conversion rate for each."));
    ledger->async_pending = TRUE;
    g_idle_add(invoice_posting_start, request);
}

static void
invoice_ledger_save_request_free (InvoiceLedgerSaveRequest *request)
{
    if (request->post_params)
    {
        if (request->post_params->memo)
            g_free (request->post_params->memo);
        if (request->post_params->parent)
            g_object_unref (request->post_params->parent);
        g_free (request->post_params);
    }
    g_weak_ref_clear(&request->book);
    g_free (request);
}

static void
invoice_ledger_save_request_finish (InvoiceLedgerSaveRequest *request)
{
    InvoiceWindow *iw = request->owner;
    if (iw && iw->ledger_save_request == request)
        iw->ledger_save_request = NULL;
    GSourceFunc completed = request->completed;
    gpointer data = request->completed_data;
    invoice_ledger_save_request_free (request);
    if (completed) completed(data);
}

static void
invoice_post_dates_response (gboolean accepted, time64 due_date,
                             time64 post_date, char *memo,
                             Account *account, gboolean accumulate,
                             gpointer user_data)
{
    InvoiceLedgerSaveRequest *request = user_data;
    InvoiceWindow *iw = request->owner;

    request->async_pending = FALSE;
    if (accepted && iw && invoice_ledger_save_request_is_current (request, iw))
    {
        request->post_params = g_new0 (struct post_invoice_params, 1);
        request->post_params->ddue = due_date;
        request->post_params->postdate = post_date;
        request->post_params->memo = memo;
        request->post_params->acc = account;
        request->post_params->accumulate = accumulate;
        memo = NULL;
        gnc_invoice_post_after_ledger (iw, request->post_params, request);
        if (request->async_pending)
            return;
    }
    g_free (memo);
    invoice_ledger_save_request_finish (request);
}

static void
invoice_ledger_save_completed (gboolean accepted, gpointer user_data)
{
    InvoiceLedgerSaveRequest *request = user_data;
    InvoiceWindow *iw = request->owner;
    if (accepted && invoice_ledger_save_request_is_current (request, iw))
    {
        if (request->action == INVOICE_LEDGER_POST)
        {
            gnc_invoice_post_after_ledger (iw, request->post_params, request);
            if (request->async_pending)
                return;
        }
        else if (gnc_invoice_window_ok_save (iw, request) &&
                 invoice_ledger_save_request_is_current (request, iw))
        {
            iw->invoice_guid = *guid_null ();
            if ((iw->dialog_type == NEW_INVOICE ||
                 iw->dialog_type == DUP_INVOICE) && iw->created_invoice)
            {
                GncInvoice *created_invoice = iw->created_invoice;
                GtkWindow *dialog = GTK_WINDOW (iw->dialog);
                gnc_ui_invoice_edit (gnc_ui_get_main_window (GTK_WIDGET (dialog)),
                                     created_invoice);
            }
            if (!invoice_ledger_save_request_is_current (request, iw))
                goto invoice_save_complete;
            gnc_close_gui_component (iw->component_id);
        }
    }
invoice_save_complete:
    invoice_ledger_save_request_finish (request);
}

static void
gnc_invoice_check_ledger_full (InvoiceWindow *iw, InvoiceLedgerAction action,
                               struct post_invoice_params *post_params,
                               GSourceFunc completed, gpointer data)
{
    InvoiceLedgerSaveRequest *request;
    if (!iw || !iw->ledger || iw->ledger_save_request)
    {
        if (completed) completed(data);
        return;
    }
    request = g_new0 (InvoiceLedgerSaveRequest, 1);
    request->owner = iw;
    g_weak_ref_init(&request->book, iw->book);
    request->action = action;
    request->completed = completed;
    request->completed_data = data;
    if (post_params)
    {
        request->post_params = g_memdup2 (post_params, sizeof (*post_params));
        request->post_params->memo = g_strdup (post_params->memo);
        if (request->post_params->parent)
            g_object_ref (request->post_params->parent);
    }
    iw->ledger_save_request = request;
    gnc_entry_ledger_check_close_async (iw_get_window (iw), iw->ledger,
                                         invoice_ledger_save_completed, request);
}

static void
gnc_invoice_check_ledger(InvoiceWindow *iw, InvoiceLedgerAction action,
                         struct post_invoice_params *params)
{
    gnc_invoice_check_ledger_full(iw, action, params, NULL, NULL);
}

static void
gnc_invoice_post (InvoiceWindow *iw, struct post_invoice_params *post_params)
{
    gnc_invoice_check_ledger (iw, INVOICE_LEDGER_POST, post_params);
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
    if (iw_get_invoice (iw))
        iw_ask_unpost (iw);
}

void gnc_invoice_window_cut_cb (GtkWidget *widget, gpointer data)
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
                         gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (widget)));
}

gboolean
gnc_invoice_window_leave_notes_cb (GtkWidget *widget, GdkEventFocus *event,
                                   gpointer data)
{
    InvoiceWindow *iw = data;
    GncInvoice *invoice = iw_get_invoice(iw);
    GtkTextBuffer* text_buffer;
    GtkTextIter start, end;
    gchar *text;

    if (!invoice) return FALSE;

    text_buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW(iw->notes_text));
    gtk_text_buffer_get_bounds (text_buffer, &start, &end);
    text = gtk_text_buffer_get_text (text_buffer, &start, &end, FALSE);
    gncInvoiceSetNotes (invoice, text);
    g_free (text);
    return FALSE;
}

static gboolean
gnc_invoice_window_leave_to_charge_cb (GtkWidget *widget, GdkEventFocus *event,
                                       gpointer data)
{
    gnc_amount_edit_evaluate (GNC_AMOUNT_EDIT(data), NULL);
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
    gtk_box_pack_start (GTK_BOX(summarybar), hbox, FALSE, FALSE, 5);

    label = gtk_label_new (label_str);
    gnc_label_set_alignment (label, 1.0, 0.5);
    gtk_box_pack_start (GTK_BOX(hbox), label, FALSE, FALSE, 0);

    label = gtk_label_new ("");
    gnc_label_set_alignment (label, 1.0, 0.5);
    gtk_box_pack_start (GTK_BOX(hbox), label, FALSE, FALSE, 0);

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

    gtk_widget_show_all(summarybar);
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
    gtk_entry_set_text (GTK_ENTRY (iw->billing_id_entry), msg ? msg : "");

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
        gtk_container_remove (GTK_CONTAINER (iw->job_box), iw->job_choice);

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
            gtk_box_pack_start (GTK_BOX (iw->job_box), iw->job_choice,
                                TRUE, TRUE, 0);

            g_signal_connect (G_OBJECT (iw->job_choice), "changed",
                              G_CALLBACK (gnc_invoice_job_changed_cb), iw);
            break;
        }

    if (iw->job_choice)
        gtk_widget_show_all (iw->job_choice);
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
        gtk_container_remove (GTK_CONTAINER (iw->proj_job_box),
                              iw->proj_job_choice);

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
            gtk_box_pack_start (GTK_BOX (iw->proj_job_box), iw->proj_job_choice,
                                TRUE, TRUE, 0);

            g_signal_connect (G_OBJECT (iw->proj_job_choice), "changed",
                              G_CALLBACK (gnc_invoice_proj_job_changed_cb), iw);
        }
        break;
    }

    if (iw->proj_job_choice)
        gtk_widget_show_all (iw->proj_job_choice);
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
    gnc_simple_combo_set_value (GTK_COMBO_BOX(iw->terms_menu), iw->terms);

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

    if (iw)
    {
        gtk_widget_destroy (iw->dialog);
    }
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
 *  call to gtk_widget_show_all().  This is needed at window/page
 *  creation time when all of the iw/page linkages haven't been set up
 *  yet.
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
        gtk_container_remove (GTK_CONTAINER (iw->owner_box), iw->owner_choice);

    if (iw->proj_cust_choice)
        gtk_container_remove (GTK_CONTAINER (iw->proj_cust_box),
                              iw->proj_cust_choice);

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

    if (iw->owner_choice)
        gtk_widget_show_all (iw->owner_choice);
    if (iw->proj_cust_choice)
        gtk_widget_show_all (iw->proj_cust_choice);

    gnc_invoice_update_job_choice (iw);
    gnc_invoice_update_proj_job (iw);

    /* Hide the project frame for customer invoices */
    if (iw->owner.type == GNC_OWNER_CUSTOMER)
        gtk_widget_hide (iw->proj_frame);

    /* Hide the "job" label and entry for employee invoices */
    if (iw->owner.type == GNC_OWNER_EMPLOYEE)
    {
        gtk_widget_hide (iw->job_label);
        gtk_widget_hide (iw->job_box);
    }

    acct_entry = GTK_WIDGET (gtk_builder_get_object (iw->builder, "acct_entry"));

    /* We know that "invoice" (and "owner") exist now */
    {
        GtkTextBuffer* text_buffer;
        const char *string;
        gchar * tmp_string;
        time64 time;

        gtk_entry_set_text (GTK_ENTRY (iw->id_entry), gncInvoiceGetID (invoice));

        gtk_entry_set_text (GTK_ENTRY (iw->billing_id_entry),
                            gncInvoiceGetBillingID (invoice));

        string = gncInvoiceGetNotes (invoice);
        text_buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW(iw->notes_text));
        gtk_text_buffer_set_text (text_buffer, string, -1);

        if (iw->active_check)
            gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (iw->active_check),
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
                gnc_simple_combo_set_value (GTK_COMBO_BOX(iw->terms_menu), iw->terms);
                break;

            case EDIT_INVOICE:
            case VIEW_INVOICE:
                // Fill in the invoice view version
                if(gncBillTermGetName (iw->terms) != NULL)
                    gtk_entry_set_text (GTK_ENTRY (iw->terms_menu),gncBillTermGetName (iw->terms));
                else
                    gtk_entry_set_text (GTK_ENTRY (iw->terms_menu),"None");
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
            gtk_entry_set_text (GTK_ENTRY (acct_entry), tmp_string);
            g_free(tmp_string);
        }
    }

    gnc_invoice_id_changed_cb(NULL, iw);
    if (iw->dialog_type == NEW_INVOICE ||
            iw->dialog_type == DUP_INVOICE ||
            iw->dialog_type == MOD_INVOICE)
    {
        if (widget)
            gtk_widget_show (widget);
        else
            gtk_widget_show (iw_get_window(iw));
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
            gtk_widget_show (show);
            gtk_widget_show (iw->posted_date_hbox);
            show = GTK_WIDGET (gtk_builder_get_object (iw->builder, "acct_label"));
            gtk_widget_show (show);
            gtk_widget_show (acct_entry);
        }
        else           /* ! posted */
        {
            hide = GTK_WIDGET (gtk_builder_get_object (iw->builder, "posted_label"));
            gtk_widget_hide (hide);
            gtk_widget_hide (iw->posted_date_hbox);

            hide = GTK_WIDGET (gtk_builder_get_object (iw->builder, "acct_label"));
            gtk_widget_hide (hide);
            gtk_widget_hide (acct_entry);
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
        gtk_widget_hide (iw->to_charge_frame);
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
        gtk_widget_show (widget);
    else
        gtk_widget_show (iw_get_window(iw));
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
        id = gtk_entry_get_text (GTK_ENTRY (iw->id_entry));
    if (id && *id)
        return g_strconcat (wintitle, " - ", id, (char *)NULL);
    return g_strdup (wintitle);
}

void
gnc_invoice_type_toggled_cb (GtkWidget *widget, gpointer data)
{
    InvoiceWindow *iw = data;

    if (!iw) return;
    iw->is_credit_note = !gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (widget));
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
gnc_invoice_terms_changed_cb (GtkWidget *widget, gpointer data)
{
    GtkComboBox *cbox = GTK_COMBO_BOX (widget);
    InvoiceWindow *iw = data;

    if (!iw) return;
    if (!cbox) return;

    iw->terms = gnc_simple_combo_get_value (cbox);
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
    gnc_builder_add_from_file (builder, "dialog-invoice.glade", "terms_store");
    gnc_builder_add_from_file (builder, "dialog-invoice.glade", "invoice_entry_vbox");
    dialog = GTK_WIDGET (gtk_builder_get_object (builder, "invoice_entry_vbox"));

    /* Autoconnect all the signals */
    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, iw);

    /* Grab the widgets */
    iw->id_label = GTK_WIDGET (gtk_builder_get_object (builder, "label3"));
    iw->type_label = GTK_WIDGET (gtk_builder_get_object (builder, "page_type_label"));
    iw->info_label = GTK_WIDGET (gtk_builder_get_object (builder, "label25"));
    iw->id_entry = GTK_WIDGET (gtk_builder_get_object (builder, "page_id_entry"));
    iw->billing_id_entry = GTK_WIDGET (gtk_builder_get_object (builder, "page_billing_id_entry"));
    iw->terms_menu = GTK_WIDGET (gtk_builder_get_object (builder, "page_terms_menu"));
    iw->notes_text = GTK_WIDGET (gtk_builder_get_object (builder, "page_notes_text"));
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
        gtk_widget_show (GTK_WIDGET (iw->doclink_button));
        g_free (display_uri);
    }
    else
        gtk_widget_hide (GTK_WIDGET (iw->doclink_button));

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
        gtk_widget_show (edit);
        hbox = GTK_WIDGET (gtk_builder_get_object (builder, "to_charge_box"));
        gtk_box_pack_start (GTK_BOX (hbox), edit, TRUE, TRUE, 0);

        g_signal_connect(G_OBJECT(gnc_amount_edit_gtk_entry(GNC_AMOUNT_EDIT(edit))),
                         "focus-out-event",
                         G_CALLBACK(gnc_invoice_window_leave_to_charge_cb), edit);
        g_signal_connect(G_OBJECT(edit), "amount_changed",
                         G_CALLBACK(gnc_invoice_window_changed_to_charge_cb), iw);
    }

    hbox = GTK_WIDGET (gtk_builder_get_object (builder, "page_date_opened_hbox"));
    iw->opened_date = gnc_date_edit_new (gnc_time (NULL), FALSE, FALSE);
    gtk_widget_show(iw->opened_date);
    gtk_box_pack_start (GTK_BOX(hbox), iw->opened_date, TRUE, TRUE, 0);

    iw->posted_date_hbox = GTK_WIDGET (gtk_builder_get_object (builder, "date_posted_hbox"));
    iw->posted_date = gnc_date_edit_new (gnc_time (NULL), FALSE, FALSE);
    gtk_widget_show(iw->posted_date);
    gtk_box_pack_start (GTK_BOX(iw->posted_date_hbox), iw->posted_date,
                        TRUE, TRUE, 0);

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
        gtk_widget_show(regWidget);

        frame = GTK_WIDGET (gtk_builder_get_object (builder, "ledger_frame"));
        gtk_container_add (GTK_CONTAINER (frame), regWidget);

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

    /* Show the dialog */
    //  gtk_widget_show_all (dialog);

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

            gtk_widget_hide (doclink_button);
        }
        else
        {
            gchar *display_uri = gnc_doclink_get_unescaped_just_uri (uri);
            gtk_link_button_set_uri (GTK_LINK_BUTTON (doclink_button),
                                     display_uri);
            gtk_widget_show (GTK_WIDGET (doclink_button));
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
    gnc_builder_add_from_file (builder, "dialog-invoice.glade", "terms_store");
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
        gtk_widget_show_all (iw->type_hbox);
        gtk_widget_hide (iw->type_label_hbox);
        gtk_widget_hide (iw->type_label);
        break;
    case MOD_INVOICE:
        gtk_widget_hide (iw->type_hbox);
        gtk_widget_show (iw->type_label_hbox);
        gtk_widget_show (iw->type_label);
        break;
    default:
        break;
    }

    if (dialog_type == DUP_INVOICE)
    {
        GtkWidget *cn_radio = GTK_WIDGET (gtk_builder_get_object (builder, "dialog_creditnote_type"));

        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(cn_radio), gncInvoiceGetIsCreditNote (invoice));
    }

    iw->id_entry = GTK_WIDGET (gtk_builder_get_object (builder, "dialog_id_entry"));
    iw->billing_id_entry = GTK_WIDGET (gtk_builder_get_object (builder, "dialog_billing_id_entry"));
    iw->terms_menu = GTK_WIDGET (gtk_builder_get_object (builder, "dialog_terms_menu"));
    iw->notes_text = GTK_WIDGET (gtk_builder_get_object (builder, "dialog_notes_text"));
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
    gtk_widget_show(iw->opened_date);
    gtk_box_pack_start (GTK_BOX(hbox), iw->opened_date, TRUE, TRUE, 0);

    /* If this is a New Invoice, reset the Notes file to read/write */
    gtk_widget_set_sensitive (iw->notes_text,
                              (iw->dialog_type == NEW_INVOICE) ||
                              (iw->dialog_type == DUP_INVOICE));

    /* Setup signals */
    gtk_builder_connect_signals_full( builder,
                                      gnc_builder_connect_full_func,
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
            gnc_billterms_combo (GTK_COMBO_BOX(iw->terms_menu), iw->book, TRUE, iw->terms);
            break;
        case EDIT_INVOICE:
        case VIEW_INVOICE:
            // Fill in the invoice view version
            if(gncBillTermGetName (iw->terms) != NULL)
                gtk_entry_set_text (GTK_ENTRY (iw->terms_menu),gncBillTermGetName (iw->terms));
            else
                gtk_entry_set_text (GTK_ENTRY (iw->terms_menu),"None");
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
        if (g_strcmp0 (gtk_entry_get_text (GTK_ENTRY (iw->id_entry)), "") == 0)
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

struct multi_duplicate_invoice_data
{
    QofBook *book;
    GncGUID book_guid;
    GPtrArray *invoice_guids;
    GWeakRef parent;
};

static void
multi_duplicate_invoice_date_completed (GncDupTransResult *result,
                                        gpointer user_data)
{
    struct multi_duplicate_invoice_data *request = user_data;
    GtkWindow *parent = g_weak_ref_get (&request->parent);
    if (result && parent && !gtk_widget_in_destruction (GTK_WIDGET (parent)) &&
        qof_book_is_open (request->book) && gnc_get_current_book () == request->book &&
        guid_equal (&request->book_guid,
                    qof_instance_get_guid (QOF_INSTANCE (request->book))))
    {
        for (guint i = 0; i < request->invoice_guids->len; i++)
        {
            const GncGUID *guid = g_ptr_array_index (request->invoice_guids, i);
            GncInvoice *old_invoice = gncInvoiceLookup (request->book, guid);
            if (!old_invoice)
                continue;
            InvoiceWindow *iw = gnc_ui_invoice_duplicate (parent, old_invoice,
                                                          FALSE, &result->gdate);
            if (iw)
                g_assert (iw_get_invoice (iw));
        }
    }
    gnc_dup_trans_result_free (result);
    g_ptr_array_unref (request->invoice_guids);
    g_weak_ref_clear (&request->parent);
    g_clear_object (&parent);
    g_object_unref (request->book);
    g_free (request);
}

static void
multi_duplicate_invoice_cb (GtkWindow *dialog, GList *invoice_list, gpointer user_data)
{
    g_return_if_fail (invoice_list);
    switch (g_list_length(invoice_list))
    {
    case 0:
        return;
    case 1:
    {
        // Duplicate exactly one invoice
        GncInvoice *old_invoice = invoice_list->data;
        gnc_ui_invoice_duplicate(dialog, old_invoice, TRUE, NULL);
        return;
    }
    default:
    {
        // Duplicate multiple invoices. We ask for a date first.
        struct multi_duplicate_invoice_data *request;
        GncDupTransResult initial = { 0 };

        // Default date: Today
        request = g_new0 (struct multi_duplicate_invoice_data, 1);
        request->book = g_object_ref (gnc_get_current_book ());
        request->book_guid = *qof_instance_get_guid (QOF_INSTANCE (request->book));
        request->invoice_guids = g_ptr_array_new_with_free_func (g_free);
        g_weak_ref_init (&request->parent, G_OBJECT (dialog));
        for (GList *node = invoice_list; node; node = node->next)
        {
            GncInvoice *invoice = node->data;
            if (invoice && gncInvoiceGetBook (invoice) == request->book)
            {
                GncGUID *guid = g_new (GncGUID, 1);
                *guid = *qof_instance_get_guid (QOF_INSTANCE (invoice));
                g_ptr_array_add (request->invoice_guids, guid);
            }
        }
        gnc_gdate_set_time64 (&initial.gdate, gnc_time (NULL));
        gnc_dup_date_dialog_async (dialog, _("Date of duplicated entries"),
                                   &initial.gdate,
                                   multi_duplicate_invoice_date_completed,
                                   request);
        return;
    }
    }
}

typedef struct
{
    GWeakRef book;
    GtkWindow *parent;
    gboolean parent_destroyed;
    GArray *invoices;
    guint index;
    struct post_invoice_params params;
    GncGUID account;
} InvoiceBulkPosting;

static gboolean invoice_bulk_post_next(gpointer data);

static void
invoice_bulk_parent_destroyed(GtkWidget *widget, InvoiceBulkPosting *request)
{
    request->parent_destroyed = TRUE;
}

static void
invoice_bulk_post_free(InvoiceBulkPosting *request)
{
    g_signal_handlers_disconnect_by_data(request->parent, request);
    g_object_unref(request->parent);
    g_weak_ref_clear(&request->book);
    g_array_unref(request->invoices);
    g_free(request->params.memo);
    g_free(request);
}

static gboolean
invoice_bulk_post_next(gpointer data)
{
    InvoiceBulkPosting *request = data;
    QofBook *book = g_weak_ref_get(&request->book);
    if (!book || request->parent_destroyed || !gnc_current_session_exist() ||
        book != gnc_get_current_book() || qof_book_shutting_down(book))
    {
        g_clear_object(&book);
        invoice_bulk_post_free(request);
        return G_SOURCE_REMOVE;
    }
    request->params.acc = xaccAccountLookup(&request->account, book);
    if (!request->params.acc)
    {
        g_object_unref(book);
        invoice_bulk_post_free(request);
        return G_SOURCE_REMOVE;
    }
    while (request->index < request->invoices->len)
    {
        GncGUID *guid = &g_array_index(request->invoices, GncGUID, request->index++);
        GncInvoice *invoice = gncInvoiceLookup(book, guid);
        if (!invoice || gncInvoiceIsPosted(invoice))
            continue;
        InvoiceWindow *iw = gnc_ui_invoice_edit(request->parent, invoice);
        g_object_unref(book);
        gnc_invoice_check_ledger_full(iw, INVOICE_LEDGER_POST, &request->params,
                                      invoice_bulk_post_next, request);
        return G_SOURCE_REMOVE;
    }
    g_object_unref(book);
    invoice_bulk_post_free(request);
    return G_SOURCE_REMOVE;
}

static void
invoice_bulk_dates_response(gboolean accepted, time64 due, time64 posted,
                            char *memo, Account *account, gboolean accumulate,
                            gpointer data)
{
    InvoiceBulkPosting *request = data;
    if (!accepted || !account || request->parent_destroyed)
    {
        g_free(memo);
        invoice_bulk_post_free(request);
        return;
    }
    request->params.ddue = due;
    request->params.postdate = posted;
    request->params.memo = memo;
    request->params.accumulate = accumulate;
    request->params.parent = request->parent;
    request->account = *xaccAccountGetGUID(account);
    g_idle_add(invoice_bulk_post_next, request);
}

static void
multi_post_invoice_cb(GtkWindow *dialog, GList *invoices, gpointer user_data)
{
    if (!invoices)
        return;
    for (GList *item = invoices; item; item = item->next)
        if (gncInvoiceIsPosted(item->data))
        {
            gnc_error_dialog_async(dialog, "%s",
                _("One or more selected invoices have already been posted.\nRe-check your selection."));
            return;
        }
    InvoiceWindow *iw = gnc_ui_invoice_edit(dialog, invoices->data);
    InvoiceBulkPosting *request = g_new0(InvoiceBulkPosting, 1);
    g_weak_ref_init(&request->book, iw->book);
    request->parent = g_object_ref(dialog);
    request->invoices = g_array_new(FALSE, FALSE, sizeof(GncGUID));
    for (GList *item = invoices; item; item = item->next)
    {
        GncGUID guid = *gncInvoiceGetGUID(item->data);
        g_array_append_val(request->invoices, guid);
    }
    g_signal_connect(dialog, "destroy", G_CALLBACK(invoice_bulk_parent_destroyed), request);
    gnc_dialog_post_invoice_query(iw, _("Do you really want to post these invoices?"),
                                  invoice_bulk_dates_response, request);
}

static void
multi_print_invoice_cb(GtkWindow *dialog, GList *invoices, gpointer user_data)
{
    if (!invoices)
        return;
    GArray *guids = g_array_new(FALSE, FALSE, sizeof(GncGUID));
    for (GList *item = invoices; item; item = item->next)
    {
        GncGUID guid = *gncInvoiceGetGUID(item->data);
        g_array_append_val(guids, guid);
    }
    use_default_report_template_or_change_async(dialog, NULL, guids);
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
