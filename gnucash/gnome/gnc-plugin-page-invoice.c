/*
 * gnc-plugin-page-invoice.c --
 *
 * Copyright (C) 2005,2006 David Hampton <hampton@employees.org>
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

#include "gnc-plugin.h"
#include "business-gnome-utils.h"
#include "dialog-invoice.h"
#include "gnc-ledger-display.h"
#include "gnc-plugin-page-invoice.h"

#include "dialog-account.h"
#include "gnc-component-manager.h"
#include "gnc-gobject-utils.h"
#include "gnc-gnome-utils.h"
#include "gnc-icons.h"
#include "gnucash-register.h"
#include "gnc-prefs.h"
#include "gnc-ui-util.h"
#include "gnc-uri-utils.h"
#include "gnc-window.h"
#include "dialog-utils.h"
#include "dialog-doclink.h"
#include "dialog-doclink-utils.h"
#include "gncInvoice.h"
#include "gnc-ui.h"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

static void gnc_plugin_page_invoice_class_init (GncPluginPageInvoiceClass *klass);
static void gnc_plugin_page_invoice_init (GncPluginPageInvoice *plugin_page);
static void gnc_plugin_page_invoice_finalize (GObject *object);

static GtkWidget *gnc_plugin_page_invoice_create_widget (GncPluginPage *plugin_page);
static gboolean gnc_plugin_page_invoice_focus_widget (GncPluginPage *plugin_page);
static void gnc_plugin_page_invoice_destroy_widget (GncPluginPage *plugin_page);
static void gnc_plugin_page_invoice_save_page (GncPluginPage *plugin_page, GKeyFile *file, const gchar *group);
static GncPluginPage *gnc_plugin_page_invoice_recreate_page (GtkWidget *window, GKeyFile *file, const gchar *group);
static void gnc_plugin_page_invoice_window_changed (GncPluginPage *plugin_page, GtkWidget *window);

static void gnc_plugin_page_invoice_summarybar_position_changed (gpointer prefs, gchar* pref, gpointer user_data);

/* Command callbacks */
static void gnc_plugin_page_invoice_cmd_new_invoice (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_invoice_cmd_new_account (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_invoice_cmd_print (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_invoice_cmd_cut (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_invoice_cmd_copy (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_invoice_cmd_paste (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_invoice_cmd_edit (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_invoice_cmd_duplicateInvoice (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_invoice_cmd_post (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_invoice_cmd_unpost (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_invoice_cmd_refresh (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_invoice_cmd_sort_changed (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void gnc_plugin_page_invoice_cmd_enter (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_invoice_cmd_cancel (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_invoice_cmd_delete (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_invoice_cmd_blank (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_invoice_cmd_duplicateEntry (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_invoice_cmd_pay_invoice (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_invoice_cmd_save_layout (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_invoice_cmd_reset_layout (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_invoice_cmd_link (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_invoice_cmd_link_remove (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_invoice_cmd_link_open (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_invoice_cmd_company_report (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_invoice_cmd_entryUp (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_invoice_cmd_entryDown (GSimpleAction *simple, GVariant *paramter, gpointer user_data);
static void gnc_plugin_page_invoice_cmd_edit_tax (GSimpleAction *simple, GVariant *paramter, gpointer user_data);

static void gnc_plugin_page_redraw_help_cb (GnucashRegister *gsr, GncPluginPageInvoice *invoice_page);
static void gnc_plugin_page_invoice_refresh_cb (GHashTable *changes, gpointer user_data);


static void
radio_change_state (GSimpleAction *simple,
                    GVariant      *state,
                    gpointer       user_data)
{
   g_simple_action_set_state (simple, state);
}

/************************************************************
 *                          Actions                         *
 ************************************************************/
static GActionEntry gnc_plugin_page_invoice_actions [] =
{
    { "FileNewAccountAction", gnc_plugin_page_invoice_cmd_new_account, NULL, NULL, NULL },
    { "FilePrintAction", gnc_plugin_page_invoice_cmd_print, NULL, NULL, NULL },
    { "EditCutAction", gnc_plugin_page_invoice_cmd_cut, NULL, NULL, NULL },
    { "EditCopyAction", gnc_plugin_page_invoice_cmd_copy, NULL, NULL, NULL },
    { "EditPasteAction", gnc_plugin_page_invoice_cmd_paste, NULL, NULL, NULL },
    { "EditEditInvoiceAction", gnc_plugin_page_invoice_cmd_edit, NULL, NULL, NULL },
    { "EditDuplicateInvoiceAction", gnc_plugin_page_invoice_cmd_duplicateInvoice, NULL, NULL, NULL },
    { "EditPostInvoiceAction", gnc_plugin_page_invoice_cmd_post, NULL, NULL, NULL },
    { "EditUnpostInvoiceAction", gnc_plugin_page_invoice_cmd_unpost, NULL, NULL, NULL },
    { "EditTaxOptionsAction", gnc_plugin_page_invoice_cmd_edit_tax, NULL, NULL, NULL },
    { "ViewRefreshAction", gnc_plugin_page_invoice_cmd_refresh, NULL, NULL, NULL },
    { "ViewSaveLayoutAction", gnc_plugin_page_invoice_cmd_save_layout, NULL, NULL, NULL },
    { "ViewResetLayoutAction", gnc_plugin_page_invoice_cmd_reset_layout, NULL, NULL, NULL },
    { "RecordEntryAction", gnc_plugin_page_invoice_cmd_enter, NULL, NULL, NULL },
    { "CancelEntryAction", gnc_plugin_page_invoice_cmd_cancel, NULL, NULL, NULL },
    { "DeleteEntryAction", gnc_plugin_page_invoice_cmd_delete, NULL, NULL, NULL },
    { "BlankEntryAction", gnc_plugin_page_invoice_cmd_blank, NULL, NULL, NULL },
    { "DuplicateEntryAction", gnc_plugin_page_invoice_cmd_duplicateEntry, NULL, NULL, NULL },
    { "EntryUpAction", gnc_plugin_page_invoice_cmd_entryUp, NULL, NULL, NULL },
    { "EntryDownAction", gnc_plugin_page_invoice_cmd_entryDown, NULL, NULL, NULL },
    { "BusinessNewInvoiceAction", gnc_plugin_page_invoice_cmd_new_invoice, NULL, NULL, NULL },
    { "BusinessLinkAction", gnc_plugin_page_invoice_cmd_link, NULL, NULL, NULL },
    { "BusinessLinkOpenAction", gnc_plugin_page_invoice_cmd_link_open, NULL, NULL, NULL },
    { "ToolsProcessPaymentAction", gnc_plugin_page_invoice_cmd_pay_invoice, NULL, NULL, NULL },
    { "ReportsCompanyReportAction", gnc_plugin_page_invoice_cmd_company_report, NULL, NULL, NULL },
    { "SortOrderRadioAction", gnc_plugin_page_invoice_cmd_sort_changed, "i", "@i 0", radio_change_state },
};
static guint gnc_plugin_page_invoice_n_actions = G_N_ELEMENTS(gnc_plugin_page_invoice_actions);

static GncDisplayItem gnc_plugin_page_invoice_display_items [] =
{
    { "SortOrderAction", NULL, N_("Sort _Order"), NULL, NULL },
    /* File menu */
    {
        "FileNewAccountAction", GNC_ICON_NEW_ACCOUNT, N_("New _Account..."), NULL,
        N_("Create a new account")
    },
    {
        "FilePrintAction", "document-print", N_("_Print Invoice"), "<primary>p",
        N_("Make a printable invoice")
    },
    /* Edit menu */
    {
        "EditCutAction", "edit-cut", N_("_Cut"), "<primary>X",
        NULL
    },
    {
        "EditCopyAction", "edit-copy", N_("Copy"), "<primary>C",
        NULL
    },
    {
        "EditPasteAction", "edit-paste", N_("_Paste"), "<primary>V",
        NULL
    },
    {
        "EditEditInvoiceAction", GNC_ICON_INVOICE_EDIT, "_Edit Invoice", NULL,
        "Edit this invoice"
    },
    {
        "EditDuplicateInvoiceAction", GNC_ICON_INVOICE_DUPLICATE, "_Duplicate Invoice",
        NULL, "Create a new invoice as a duplicate of the current one"
    },
    {
        "EditPostInvoiceAction", GNC_ICON_INVOICE_POST, "_Post Invoice", NULL,
        "Post this invoice to your Chart of Accounts"
    },
    {
        "EditUnpostInvoiceAction", GNC_ICON_INVOICE_UNPOST, "_Unpost Invoice", NULL,
        "Unpost this invoice and make it editable"
    },
    /* View menu */
    {
        "ViewRefreshAction", "view-refresh", N_("_Refresh"), "<primary>r",
        N_("Refresh this window")
    },
    {
        "ViewSaveLayoutAction", NULL, "_Use as Default Layout for Customer Documents", NULL,
        "Use the current layout as default for all customer invoices and credit notes"
    },
    {
        "ViewResetLayoutAction", NULL, "_Reset Default Layout for Customer Documents", NULL,
        "Reset default layout for all customer invoices and credit notes back to built-in defaults and update the current page accordingly"
    },
    /* Actions menu */
    {
        "RecordEntryAction", "list-add", N_("_Enter"), NULL,
        N_("Record the current entry")
    },
    {
        "CancelEntryAction", "process-stop", N_("_Cancel"), NULL,
        N_("Cancel the current entry")
    },
    {
        "DeleteEntryAction", "edit-delete", N_("_Delete"), NULL,
        N_("Delete the current entry")
    },
    {
        "BlankEntryAction", "go-bottom", N_("_Blank"), NULL,
        "Move to the blank entry at the bottom of the Invoice"
    },
    {
        "DuplicateEntryAction", "edit-copy", N_("Dup_licate Entry"), NULL,
        N_("Make a copy of the current entry")
    },
    {
        "EntryUpAction", "pan-up-symbolic", N_("Move Entry _Up"), NULL,
        N_("Move the current entry one row upwards")
    },
    {
        "EntryDownAction", "pan-down-symbolic", N_("Move Entry Do_wn"), NULL,
        N_("Move the current entry one row downwards")
    },
    /* Business menu */
    {
        "BusinessNewInvoiceAction", GNC_ICON_INVOICE_NEW, "New _Invoice", "",
        "Create a new invoice for the same owner as the current one"
    },
    {
        "BusinessLinkAction", NULL, "_Manage Document Link...", NULL,
        "Manage link of an external document to this item."
    },
    {
        "BusinessLinkOpenAction", NULL, "_Open Linked Document", NULL,
        "Open the linked document"
    },
    {
        "ToolsProcessPaymentAction", GNC_ICON_INVOICE_PAY, "_Pay Invoice", NULL,
        "Enter a payment for the owner of this invoice"
    },
    /* Reports menu */
    {
        "ReportsCompanyReportAction", NULL, N_("_Company Report"), NULL,
        "Open a company report window for the owner of this invoice"
    },
    /* Sort Order */
    { "SortStandardAction", NULL, N_("_Standard"), NULL, N_("Keep normal invoice order") },
    { "SortDateAction", NULL, N_("_Date"), NULL, N_("Sort by date") },
    { "SortDateEntryAction", NULL, N_("Date of _Entry"), NULL, N_("Sort by the date of entry") },
    { "SortQuantityAction", NULL, N_("_Quantity"), NULL, N_("Sort by quantity") },
    { "SortPriceAction", NULL, N_("_Price"), NULL, N_("Sort by price") },
    { "SortDescriptionAction", NULL, N_("Descri_ption"), NULL, N_("Sort by description") },
};
/** The number of display items provided by this plugin. */
static guint gnc_plugin_page_invoice_n_display_items = G_N_ELEMENTS(gnc_plugin_page_invoice_display_items);

static GncAddSubMenu add_submenus [] =
{
    { "EditAction", TRUE },
    { "ViewAction", TRUE },
    { "TransactionAction", FALSE },
    { "ActionsAction", TRUE },
    { "ScheduledAction", FALSE },
    { "ReportsAction", TRUE },
};
static guint add_submenus_n_actions = G_N_ELEMENTS(add_submenus);

/* As only when loading the sub menus the menu items are updated,
 * any menu items out of that scope that are made visible need to be
 * added here */
static GncActionUpdate update_actions [] =
{
    { "FilePrintAction", N_("_Print"), TRUE, NULL },
    { "FilePrintPDFAction", NULL, FALSE, NULL },
    { "ReportExportAction", NULL, FALSE, NULL },
    { "ReportSaveAction", NULL, FALSE, NULL },
    { "ReportSaveAsAction", NULL, FALSE, NULL },

    { "ReportsCompanyReportAction", NULL, FALSE, NULL },
    { "OTVendorListingReportAction", NULL, FALSE, NULL },
    { "OTCustomerListingReportAction", NULL, FALSE, NULL },
    { "OTVendorReportAction", NULL, FALSE, NULL },
    { "OTCustomerReportAction", NULL, FALSE, NULL },
    { "OTEmployeeReportAction", NULL, FALSE, NULL },

    { "ReportsAccountReportAction", NULL, FALSE, NULL },
    { "ReportsAcctTransReportAction", NULL, FALSE, NULL },
    { "BusinessLinkAction", N_("_Manage Document Link..."), TRUE, N_("Manage Document Link") },
    { "BusinessLinkOpenAction", N_("_Open Linked Document"), TRUE, N_("Open Linked Document") },
};
static guint update_actions_n_actions = G_N_ELEMENTS(update_actions);

static const gchar *invoice_book_readwrite_actions[] =
{
    // Only insert actions here which are not yet in posted_actions and unposted_actions!
    "FileNewAccountAction",
    "EditDuplicateInvoiceAction",
    "BusinessNewInvoiceAction",
    "ToolsProcessPaymentAction",
    "BusinessLinkAction",
    NULL
};

static const gchar *posted_actions[] =
{
    NULL
};

static const gchar *unposted_actions[] =
{
    "EditCutAction",
    "EditPasteAction",
    "EditEditInvoiceAction",
    "EditPostInvoiceAction",
    "RecordEntryAction",
    "CancelEntryAction",
    "DeleteEntryAction",
    "DuplicateEntryAction",
    "EntryUpAction",
    "EntryDownAction",
    "BlankEntryAction",
    NULL
};

static const gchar *can_unpost_actions[] =
{
    "EditUnpostInvoiceAction",
    NULL
};

static GncActionUpdate invoice_action_labels[] =
{
    {"FilePrintAction", N_("_Print Invoice"), TRUE, N_("Make a printable invoice")},
    {"EditEditInvoiceAction", N_("_Edit Invoice"), TRUE, N_("Edit this invoice")},
    {"EditDuplicateInvoiceAction", N_("_Duplicate Invoice"), TRUE, N_("Edit this invoice")},
    {"EditPostInvoiceAction", N_("_Post Invoice"), TRUE, N_("Post this invoice to your Chart of Accounts")},
    {"EditUnpostInvoiceAction", N_("_Unpost Invoice"), TRUE, N_("Unpost this invoice and make it editable")},
    {"BusinessNewInvoiceAction", N_("New _Invoice"), TRUE, N_("Create a new invoice for the same owner as the current one")},
    {"BlankEntryAction", N_("Blank"), TRUE, N_("Move to the blank entry at the bottom of the invoice")},
    {"ToolsProcessPaymentAction", N_("_Pay Invoice"), TRUE, N_("Enter a payment for the owner of this invoice") },
    {"ReportsCompanyReportAction", N_("_Company Report"), TRUE, N_("Open a customer report window for the owner of this invoice") },
};
static guint invoice_action_labels_n_actions = G_N_ELEMENTS(invoice_action_labels);

static GncActionUpdate invoice_action_layout_labels[] =
{
    {"ViewSaveLayoutAction", N_("_Use as Default Layout for Customer Documents"), TRUE, N_("Use the current layout as default for all customer invoices and credit notes")},
    {"ViewResetLayoutAction", N_("_Reset Default Layout for Customer Documents"), TRUE, N_("Reset default layout for all customer invoices and credit notes back to built-in defaults and update the current page accordingly")},
};
static guint invoice_action_layout_labels_n_actions = G_N_ELEMENTS(invoice_action_layout_labels);

static GncActionUpdate bill_action_labels[] =
{
    {"FilePrintAction", N_("_Print Bill"), TRUE, N_("Make a printable bill")},
    {"EditEditInvoiceAction", N_("_Edit Bill"), TRUE, N_("Edit this bill")},
    {"EditDuplicateInvoiceAction", N_("_Duplicate Bill"), TRUE, N_("Create a new bill as a duplicate of the current one")},
    {"EditPostInvoiceAction", N_("_Post Bill"), TRUE, N_("Post this bill to your Chart of Accounts")},
    {"EditUnpostInvoiceAction", N_("_Unpost Bill"), TRUE, N_("Unpost this bill and make it editable")},
    {"BusinessNewInvoiceAction", N_("New _Bill"), TRUE, N_("Create a new bill for the same owner as the current one")},
    {"BlankEntryAction", N_("Blank"), TRUE, N_("Move to the blank entry at the bottom of the bill")},
    {"ToolsProcessPaymentAction", N_("_Pay Bill"), TRUE, N_("Enter a payment for the owner of this bill") },
    {"ReportsCompanyReportAction", N_("_Company Report"), TRUE, N_("Open a vendor report window for the owner of this bill") },
};
static guint bill_action_labels_n_actions = G_N_ELEMENTS(bill_action_labels);

static GncActionUpdate bill_action_layout_labels[] =
{
    {"ViewSaveLayoutAction", N_("_Use as Default Layout for Vendor Documents"), TRUE, N_("Use the current layout as default for all vendor bills and credit notes")},
    {"ViewResetLayoutAction", N_("_Reset Default Layout for Vendor Documents"), TRUE, N_("Reset default layout for all vendor bills and credit notes back to built-in defaults and update the current page accordingly")},
};
static guint bill_action_layout_labels_n_actions = G_N_ELEMENTS(bill_action_layout_labels);

static GncActionUpdate voucher_action_labels[] =
{
    {"FilePrintAction", N_("_Print Voucher"), TRUE, N_("Make a printable voucher")},
    {"EditEditInvoiceAction", N_("_Edit Voucher"), TRUE, N_("Edit this voucher")},
    {"EditDuplicateInvoiceAction", N_("_Duplicate Voucher"), TRUE, N_("Create a new voucher as a duplicate of the current one")},
    {"EditPostInvoiceAction", N_("_Post Voucher"), TRUE, N_("Post this voucher to your Chart of Accounts")},
    {"EditUnpostInvoiceAction", N_("_Unpost Voucher"), TRUE, N_("Unpost this voucher and make it editable")},
    {"BusinessNewInvoiceAction", N_("New _Voucher"), TRUE, N_("Create a new voucher for the same owner as the current one")},
    {"BlankEntryAction", N_("Blank"), TRUE, N_("Move to the blank entry at the bottom of the voucher")},
    {"ToolsProcessPaymentAction", N_("_Pay Voucher"), TRUE, N_("Enter a payment for the owner of this voucher") },
    {"ReportsCompanyReportAction", N_("_Company Report"), TRUE, N_("Open a employee report window for the owner of this voucher") },
};
static guint voucher_action_labels_n_actions = G_N_ELEMENTS(voucher_action_labels);

static GncActionUpdate voucher_action_layout_labels[] =
{
    {"ViewSaveLayoutAction", N_("_Use as Default Layout for Employee Documents"), TRUE, N_("Use the current layout as default for all employee vouchers and credit notes")},
    {"ViewResetLayoutAction", N_("_Reset Default Layout for Employee Documents"), TRUE, N_("Reset default layout for all employee vouchers and credit notes back to built-in defaults and update the current page accordingly")},
};
static guint voucher_action_layout_labels_n_actions = G_N_ELEMENTS(voucher_action_layout_labels);

static GncActionUpdate creditnote_action_labels[] =
{
    {"FilePrintAction", N_("_Print Credit Note"), TRUE, N_("Make a printable credit note")},
    {"EditEditInvoiceAction", N_("_Edit Credit Note"), TRUE, N_("Edit this credit note")},
    {"EditDuplicateInvoiceAction", N_("_Duplicate Credit Note"), TRUE, N_("Create a new credit note as a duplicate of the current one")},
    {"EditPostInvoiceAction", N_("_Post Credit Note"), TRUE, N_("Post this credit note to your Chart of Accounts")},
    {"EditUnpostInvoiceAction", N_("_Unpost Credit Note"), TRUE, N_("Unpost this credit note and make it editable")},
    {"BusinessNewInvoiceAction", N_("New _Credit Note"), TRUE, N_("Create a new credit note for the same owner as the current one")},
    {"BlankEntryAction", N_("Blank"), TRUE, N_("Move to the blank entry at the bottom of the credit note")},
    {"ToolsProcessPaymentAction", N_("_Pay Credit Note"), TRUE, N_("Enter a payment for the owner of this credit note") },
    {"ReportsCompanyReportAction", N_("_Company Report"), TRUE, N_("Open a company report window for the owner of this credit note") },
};
static guint creditnote_action_labels_n_actions = G_N_ELEMENTS(creditnote_action_labels);

/** Short labels for use on the toolbar buttons. */
static GncToolBarShortNames toolbar_labels[] = {
    {"RecordEntryAction",         N_("Enter")},
    {"CancelEntryAction",         N_("Cancel")},
    {"DeleteEntryAction",         N_("Delete")},
    {"DuplicateEntryAction",      N_("Duplicate")},
    {"EntryUpAction",             N_("Up")},
    {"EntryDownAction",           N_("Down")},
    {"BlankEntryAction",          N_("Blank")},
    {"EditPostInvoiceAction",     N_("Post")},
    {"EditUnpostInvoiceAction",   N_("Unpost")},
    {"ToolsProcessPaymentAction", N_("Pay")},
    {NULL, NULL},
};

/************************************************************/
/*                      Data Structures                     */
/************************************************************/

typedef struct GncPluginPageInvoicePrivate
{
    InvoiceWindow *iw;

    GtkWidget *widget;
    gboolean is_posted;
    gboolean can_unpost;
    gboolean is_readonly;

    gint component_manager_id;
} GncPluginPageInvoicePrivate;

G_DEFINE_TYPE_WITH_PRIVATE(GncPluginPageInvoice, gnc_plugin_page_invoice, GNC_TYPE_PLUGIN_PAGE)

#define GNC_PLUGIN_PAGE_INVOICE_GET_PRIVATE(o)  \
   ((GncPluginPageInvoicePrivate*)gnc_plugin_page_invoice_get_instance_private((GncPluginPageInvoice*)o))

static GObjectClass *parent_class = NULL;

/************************************************************/
/*                      Implementation                      */
/************************************************************/

InvoiceWindow *
gnc_plugin_page_invoice_get_window (GncInvoice *invoice)
{
    GncPluginPageInvoicePrivate *priv;
    GncPluginPageInvoice *invoice_page;
    const GList *item;

    /* Is there an existing page? */
    item = gnc_gobject_tracking_get_list (GNC_PLUGIN_PAGE_INVOICE_NAME);
    for ( ; item; item = g_list_next(item))
    {
        invoice_page = (GncPluginPageInvoice *)item->data;
        priv = GNC_PLUGIN_PAGE_INVOICE_GET_PRIVATE(invoice_page);

        if (gnc_invoice_window_get_invoice (priv->iw) == invoice)
            return priv->iw;
    }
    return NULL;
}

GncPluginPage *
gnc_plugin_page_invoice_new (InvoiceWindow *iw)
{
    GncPluginPageInvoicePrivate *priv;
    GncPluginPageInvoice *invoice_page;
    GncPluginPage *plugin_page;
    const GList *item;

    /* Is there an existing page? */
    item = gnc_gobject_tracking_get_list(GNC_PLUGIN_PAGE_INVOICE_NAME);
    for ( ; item; item = g_list_next(item))
    {
        invoice_page = (GncPluginPageInvoice *)item->data;
        priv = GNC_PLUGIN_PAGE_INVOICE_GET_PRIVATE(invoice_page);
        if (priv->iw == iw)
            return GNC_PLUGIN_PAGE(invoice_page);
    }

    invoice_page = g_object_new (GNC_TYPE_PLUGIN_PAGE_INVOICE, (char *)NULL);
    priv = GNC_PLUGIN_PAGE_INVOICE_GET_PRIVATE(invoice_page);
    priv->iw = iw;

    plugin_page = GNC_PLUGIN_PAGE(invoice_page);
    gnc_plugin_page_invoice_update_title(plugin_page);
    gnc_plugin_page_set_uri(plugin_page, "default:");

    priv->component_manager_id = 0;
    return plugin_page;
}

static void
gnc_plugin_page_invoice_class_init (GncPluginPageInvoiceClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);
    GncPluginPageClass *gnc_plugin_class = GNC_PLUGIN_PAGE_CLASS(klass);

    parent_class = g_type_class_peek_parent (klass);

    object_class->finalize = gnc_plugin_page_invoice_finalize;

    gnc_plugin_class->tab_icon        = NULL;
    gnc_plugin_class->plugin_name     = GNC_PLUGIN_PAGE_INVOICE_NAME;
    gnc_plugin_class->create_widget   = gnc_plugin_page_invoice_create_widget;
    gnc_plugin_class->destroy_widget  = gnc_plugin_page_invoice_destroy_widget;
    gnc_plugin_class->save_page       = gnc_plugin_page_invoice_save_page;
    gnc_plugin_class->recreate_page   = gnc_plugin_page_invoice_recreate_page;
    gnc_plugin_class->window_changed  = gnc_plugin_page_invoice_window_changed;
    gnc_plugin_class->focus_page_function = gnc_plugin_page_invoice_focus_widget;
}

static void
gnc_plugin_page_invoice_init (GncPluginPageInvoice *plugin_page)
{
    GncPluginPage *parent;
    GSimpleActionGroup *simple_action_group;
    gboolean use_new;

    /* Init parent declared variables */
    parent = GNC_PLUGIN_PAGE(plugin_page);
    use_new = gnc_prefs_get_bool (GNC_PREFS_GROUP_INVOICE, GNC_PREF_USE_NEW);
    g_object_set(G_OBJECT(plugin_page),
                 "page-name",      _("Invoice"),
                 "page-uri",       "default:",
                 "ui-description", "gnc-plugin-page-invoice.ui",
                 "use-new-window", use_new,
                 (char *)NULL);

    /* change me when the system supports multiple books */
    gnc_plugin_page_add_book (parent, gnc_get_current_book());

    /* Create menu and toolbar information */
    simple_action_group = gnc_plugin_page_create_action_group (parent,"GncPluginPageInvoiceActions");
    g_action_map_add_action_entries (G_ACTION_MAP(simple_action_group),
                                     gnc_plugin_page_invoice_actions,
                                     gnc_plugin_page_invoice_n_actions,
                                     plugin_page);
}

static void
gnc_plugin_page_invoice_finalize (GObject *object)
{
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_INVOICE (object));

    ENTER("object %p", object);
    G_OBJECT_CLASS (parent_class)->finalize (object);
    LEAVE(" ");
}

static void
update_doclink_actions (GncPluginPage *plugin_page, gboolean has_uri)
{
    GAction *uri_action = gnc_plugin_page_get_action (GNC_PLUGIN_PAGE(plugin_page),
                                                      "BusinessLinkOpenAction");
    g_simple_action_set_enabled (G_SIMPLE_ACTION(uri_action), has_uri);
}

static void
gnc_plugin_page_update_reset_layout_action (GncPluginPage *page)
{
    GncPluginPageInvoicePrivate *priv;
    GAction *layout_action;
    gboolean has_default = FALSE;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_INVOICE(page));

    priv = GNC_PLUGIN_PAGE_INVOICE_GET_PRIVATE(page);

    layout_action = gnc_plugin_page_get_action (page, "ViewResetLayoutAction");
    if (gnc_invoice_window_document_has_user_state (priv->iw))
        has_default = TRUE;
    g_simple_action_set_enabled (G_SIMPLE_ACTION(layout_action), has_default);
}

void
gnc_plugin_page_invoice_update_menus (GncPluginPage *page, gboolean is_posted, gboolean can_unpost)
{
    GSimpleActionGroup *simple_action_group;
    GncPluginPageInvoicePrivate *priv;
    GncInvoiceType invoice_type;
    GncInvoice *invoice;
    gint i, j;
    const GncActionUpdate *updates = NULL, *updates_lay = NULL;
    gint n_updates = 0, n_updates_lay = 0;
    gboolean has_uri = FALSE;
    GncMainWindow *window;
    GAction *action;
    gboolean is_readonly = qof_book_is_readonly(gnc_get_current_book());

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_INVOICE(page));

    window = (GncMainWindow*)gnc_plugin_page_get_window (GNC_PLUGIN_PAGE(page));

    if (gnc_main_window_get_current_page (window) != page)
        return;

    priv = GNC_PLUGIN_PAGE_INVOICE_GET_PRIVATE(page);
    invoice_type = gnc_invoice_get_type_from_window(priv->iw);

     // lets remember these settings
     priv->is_posted = is_posted;
     priv->can_unpost = can_unpost;
     priv->is_readonly = is_readonly;

    switch (invoice_type) {
        case GNC_INVOICE_CUST_INVOICE:
            updates = invoice_action_labels;
            n_updates = invoice_action_labels_n_actions;
            break;
        case GNC_INVOICE_VEND_INVOICE:
            updates = bill_action_labels;
            n_updates = bill_action_labels_n_actions;
            break;
        case GNC_INVOICE_EMPL_INVOICE:
            updates = voucher_action_labels;
            n_updates = voucher_action_labels_n_actions;
            break;
        case GNC_INVOICE_CUST_CREDIT_NOTE:  // fallthrough
        case GNC_INVOICE_VEND_CREDIT_NOTE:  // fallthrough
        case GNC_INVOICE_EMPL_CREDIT_NOTE:  // fallthrough
            updates = creditnote_action_labels;
            n_updates = creditnote_action_labels_n_actions;
            break;
        default: // catches GNC_INVOICE_UNDEFINED, use invoice by default
            updates = invoice_action_labels;
            n_updates = invoice_action_labels_n_actions;
    }

    // layout actions
    switch (invoice_type) {
        case GNC_INVOICE_CUST_INVOICE:
        case GNC_INVOICE_CUST_CREDIT_NOTE:
            updates_lay = invoice_action_layout_labels;
            n_updates_lay = invoice_action_layout_labels_n_actions;
            break;
        case GNC_INVOICE_VEND_INVOICE:
        case GNC_INVOICE_VEND_CREDIT_NOTE:
            updates_lay = bill_action_layout_labels;
            n_updates_lay = bill_action_layout_labels_n_actions;
            break;
        case GNC_INVOICE_EMPL_INVOICE:
        case GNC_INVOICE_EMPL_CREDIT_NOTE:
            updates_lay = voucher_action_layout_labels;
            n_updates_lay = voucher_action_layout_labels_n_actions;
            break;
        default: // catches GNC_INVOICE_UNDEFINED, use invoice by default
           updates_lay = invoice_action_layout_labels;
           n_updates_lay = invoice_action_layout_labels_n_actions;
    }

    if (is_readonly)
    {
        // Are we readonly? Then don't allow any actions.
        is_posted = TRUE;
        can_unpost = FALSE;
    }

    /* Enable the FilePrintAction */
    action = gnc_main_window_find_action (window, "FilePrintAction");
    g_simple_action_set_enabled (G_SIMPLE_ACTION(action), TRUE);

    /* update the action labels */
    gnc_main_window_update_action_labels (window, updates, n_updates);

    /* update the layout action labels */
    gnc_main_window_update_action_labels (window, updates_lay, n_updates_lay);

    simple_action_group = gnc_plugin_page_get_action_group (page);
    gnc_plugin_update_actions (simple_action_group, posted_actions,
                               "sensitive", is_posted);
    gnc_plugin_update_actions (simple_action_group, unposted_actions,
                               "sensitive", !is_posted);
    gnc_plugin_update_actions (simple_action_group, can_unpost_actions,
                               "sensitive", can_unpost);
    gnc_plugin_update_actions (simple_action_group, invoice_book_readwrite_actions,
                               "sensitive", !is_readonly);

    // if there is no default layout do not enable reset action
    gnc_plugin_page_update_reset_layout_action (page);

    // update doclink buttons
    invoice = gnc_invoice_window_get_invoice (priv->iw);
    if (gncInvoiceGetDocLink (invoice))
        has_uri = TRUE;

    update_doclink_actions (page, has_uri);
}


/**
 * Whenever the current page is changed, if an invoice page is
 * the current page, set focus on the sheet or notes field.
 */
static gboolean
gnc_plugin_page_invoice_focus_widget (GncPluginPage *invoice_plugin_page)
{
    if (GNC_IS_PLUGIN_PAGE_INVOICE(invoice_plugin_page))
    {
        GncPluginPageInvoicePrivate *priv = GNC_PLUGIN_PAGE_INVOICE_GET_PRIVATE(invoice_plugin_page);

        GtkWidget *regWidget = gnc_invoice_get_register(priv->iw);
        GtkWidget *notes = gnc_invoice_get_notes(priv->iw);
        GnucashSheet *sheet;

        gnc_main_window_add_sub_menus (GNC_MAIN_WINDOW(invoice_plugin_page->window), invoice_plugin_page,
                                       add_submenus, add_submenus_n_actions);

        gnc_main_window_update_action_labels (GNC_MAIN_WINDOW(invoice_plugin_page->window),
                                              update_actions, update_actions_n_actions);

        gnc_plugin_page_invoice_update_menus (invoice_plugin_page, priv->is_posted, priv->can_unpost);

        // if there is no default layout do not enable reset action
        gnc_plugin_page_update_reset_layout_action (invoice_plugin_page);

        if (!GNUCASH_IS_REGISTER(regWidget))
            return FALSE;

        sheet = gnucash_register_get_sheet (GNUCASH_REGISTER(regWidget));

        // Test for the sheet being read only
        if (!gnucash_sheet_is_read_only (sheet))
        {
            if (!gtk_widget_is_focus (GTK_WIDGET(sheet)))
                gtk_widget_grab_focus (GTK_WIDGET(sheet));
        }
        else // set focus to the notes field
        {
            if (!gtk_widget_is_focus (GTK_WIDGET(notes)))
                gtk_widget_grab_focus (GTK_WIDGET(notes));
        }
    }
    return FALSE;
}


/* Virtual Functions */

static GtkWidget *
gnc_plugin_page_invoice_create_widget (GncPluginPage *plugin_page)
{
    GncPluginPageInvoice *page;
    GncPluginPageInvoicePrivate *priv;
    GtkWidget *regWidget, *widget;

    ENTER("page %p", plugin_page);
    page = GNC_PLUGIN_PAGE_INVOICE (plugin_page);
    priv = GNC_PLUGIN_PAGE_INVOICE_GET_PRIVATE(page);
    if (priv->widget != NULL)
    {
        LEAVE("");
        return priv->widget;
    }

    priv->widget = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
    gtk_box_set_homogeneous (GTK_BOX (priv->widget), FALSE);

    // Set the name for this widget so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(priv->widget), "gnc-id-invoice-page");

    gtk_widget_show (priv->widget);

    widget = gnc_invoice_create_page(priv->iw, page);
    gtk_widget_show (widget);
    gtk_box_pack_start(GTK_BOX (priv->widget), widget, TRUE, TRUE, 0);

    plugin_page->summarybar = gnc_invoice_window_create_summary_bar(priv->iw);
    gtk_box_pack_start(GTK_BOX (priv->widget), plugin_page->summarybar, FALSE, FALSE, 0);
    gnc_plugin_page_invoice_summarybar_position_changed(NULL, NULL, page);
    gnc_prefs_register_cb (GNC_PREFS_GROUP_GENERAL,
                           GNC_PREF_SUMMARYBAR_POSITION_TOP,
                           gnc_plugin_page_invoice_summarybar_position_changed,
                           page);
    gnc_prefs_register_cb (GNC_PREFS_GROUP_GENERAL,
                           GNC_PREF_SUMMARYBAR_POSITION_BOTTOM,
                           gnc_plugin_page_invoice_summarybar_position_changed,
                           page);

    regWidget = gnc_invoice_get_register(priv->iw);
    if (regWidget)
    {
        g_signal_connect (G_OBJECT (regWidget), "redraw-help",
                          G_CALLBACK (gnc_plugin_page_redraw_help_cb), page);
    }

    priv->component_manager_id =
        gnc_register_gui_component(GNC_PLUGIN_PAGE_INVOICE_NAME,
                                   gnc_plugin_page_invoice_refresh_cb,
                                   NULL, page);

    g_signal_connect (G_OBJECT(plugin_page), "inserted",
                      G_CALLBACK(gnc_plugin_page_inserted_cb),
                      NULL);

    // add the display items to the main list
    gnc_main_window_add_to_display_list (GNC_MAIN_WINDOW(GNC_PLUGIN_PAGE(plugin_page)->window),
                                         gnc_plugin_page_invoice_display_items,
                                         gnc_plugin_page_invoice_n_display_items);

    // setup any short toolbar names
    gnc_main_window_init_short_names (GNC_MAIN_WINDOW(GNC_PLUGIN_PAGE(plugin_page)->window),
                                      toolbar_labels);

    LEAVE("");
    return priv->widget;
}

static void
gnc_plugin_page_invoice_destroy_widget (GncPluginPage *plugin_page)
{
    GncPluginPageInvoice *page;
    GncPluginPageInvoicePrivate *priv;

    ENTER("page %p", plugin_page);
    page = GNC_PLUGIN_PAGE_INVOICE (plugin_page);
    priv = GNC_PLUGIN_PAGE_INVOICE_GET_PRIVATE(page);

    gnc_prefs_remove_cb_by_func (GNC_PREFS_GROUP_GENERAL,
                                 GNC_PREF_SUMMARYBAR_POSITION_TOP,
                                 gnc_plugin_page_invoice_summarybar_position_changed,
                                 page);
    gnc_prefs_remove_cb_by_func (GNC_PREFS_GROUP_GENERAL,
                                 GNC_PREF_SUMMARYBAR_POSITION_BOTTOM,
                                 gnc_plugin_page_invoice_summarybar_position_changed,
                                 page);

    // Remove the page_changed signal callback
    gnc_plugin_page_disconnect_page_changed (GNC_PLUGIN_PAGE(plugin_page));

    // Remove the page focus idle function if present
    g_idle_remove_by_data (plugin_page);

    if (priv->widget == NULL)
    {
        LEAVE("");
        return;
    }

    if (priv->component_manager_id)
    {
        gnc_unregister_gui_component(priv->component_manager_id);
        priv->component_manager_id = 0;
    }

    gtk_widget_hide(priv->widget);
    gnc_invoice_window_destroy_cb(priv->widget, priv->iw);
    priv->widget = NULL;
    LEAVE("");
}

/** Save enough information about this invoice page that it can be
 *  recreated next time the user starts gnucash.
 *
 *  @param page The page to save.
 *
 *  @param key_file A pointer to the GKeyFile data structure where the
 *  page information should be written.
 *
 *  @param group_name The group name to use when saving data. */
static void
gnc_plugin_page_invoice_save_page (GncPluginPage *plugin_page,
                                   GKeyFile *key_file,
                                   const gchar *group_name)
{
    GncPluginPageInvoice *invoice;
    GncPluginPageInvoicePrivate *priv;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_INVOICE(plugin_page));
    g_return_if_fail (key_file != NULL);
    g_return_if_fail (group_name != NULL);

    ENTER("page %p, key_file %p, group_name %s", plugin_page, key_file,
          group_name);

    invoice = GNC_PLUGIN_PAGE_INVOICE(plugin_page);
    priv = GNC_PLUGIN_PAGE_INVOICE_GET_PRIVATE(invoice);

    gnc_invoice_save_page(priv->iw, key_file, group_name);
    LEAVE(" ");
}



/** Create a new invoice page based on the information saved during a
 *  previous instantiation of gnucash.
 *
 *  @param window The window where this page should be installed.
 *
 *  @param key_file A pointer to the GKeyFile data structure where the
 *  page information should be read.
 *
 *  @param group_name The group name to use when restoring data. */
static GncPluginPage *
gnc_plugin_page_invoice_recreate_page (GtkWidget *window,
                                       GKeyFile *key_file,
                                       const gchar *group_name)
{
    GncPluginPage *page;

    g_return_val_if_fail(GNC_IS_MAIN_WINDOW(window), NULL);
    g_return_val_if_fail(key_file, NULL);
    g_return_val_if_fail(group_name, NULL);
    ENTER("key_file %p, group_name %s", key_file, group_name);

    /* Create the new page. */
    page = gnc_invoice_recreate_page(GNC_MAIN_WINDOW(window),
                                     key_file, group_name);

    LEAVE(" ");
    return page;
}


static void
gnc_plugin_page_invoice_window_changed (GncPluginPage *plugin_page,
                                        GtkWidget *window)
{
    GncPluginPageInvoice *page;
    GncPluginPageInvoicePrivate *priv;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_INVOICE (plugin_page));

    page = GNC_PLUGIN_PAGE_INVOICE(plugin_page);
    priv = GNC_PLUGIN_PAGE_INVOICE_GET_PRIVATE(page);
    gnc_invoice_window_changed (priv->iw, window);
}


static void
gnc_plugin_page_invoice_summarybar_position_changed (gpointer prefs,
                                                     gchar *pref,
                                                     gpointer user_data)
{
    GncPluginPage *plugin_page;
    GncPluginPageInvoice *page;
    GncPluginPageInvoicePrivate *priv;
    GtkPositionType position = GTK_POS_BOTTOM;

    g_return_if_fail(user_data != NULL);

    plugin_page = GNC_PLUGIN_PAGE(user_data);
    page = GNC_PLUGIN_PAGE_INVOICE (user_data);
    priv = GNC_PLUGIN_PAGE_INVOICE_GET_PRIVATE(page);

    if (gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL, GNC_PREF_SUMMARYBAR_POSITION_TOP))
        position = GTK_POS_TOP;

    gtk_box_reorder_child(GTK_BOX(priv->widget),
                          plugin_page->summarybar,
                          (position == GTK_POS_TOP ? 0 : -1) );
}


/************************************************************/
/*                     Command callbacks                    */
/************************************************************/

static void
gnc_plugin_page_invoice_cmd_new_invoice (GSimpleAction *simple,
                                         GVariant *paramter,
                                         gpointer user_data)
{
    GncPluginPageInvoice *plugin_page = user_data;
    GncPluginPageInvoicePrivate *priv;
    GtkWindow *parent;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_INVOICE(plugin_page));

    ENTER("(action %p, plugin_page %p)", simple, plugin_page);
    priv = GNC_PLUGIN_PAGE_INVOICE_GET_PRIVATE(plugin_page);
    parent = GTK_WINDOW (gnc_plugin_page_get_window (GNC_PLUGIN_PAGE (plugin_page)));
    gnc_invoice_window_new_invoice_cb(parent, priv->iw);
    LEAVE(" ");
}

static void
gnc_plugin_page_invoice_cmd_new_account (GSimpleAction *simple,
                                         GVariant *paramter,
                                         gpointer user_data)
{
    GncPluginPageInvoice *plugin_page = user_data;
    GtkWindow *parent = NULL;
    g_return_if_fail(GNC_IS_PLUGIN_PAGE_INVOICE(plugin_page));
    parent = GTK_WINDOW (gnc_plugin_page_get_window (GNC_PLUGIN_PAGE (plugin_page)));
    ENTER("(action %p, plugin_page %p)", simple, plugin_page);
    gnc_ui_new_account_window (parent, gnc_get_current_book(), NULL);
    LEAVE(" ");
}

static void
gnc_plugin_page_invoice_cmd_print (GSimpleAction *simple,
                                   GVariant *paramter,
                                   gpointer user_data)
{
    GncPluginPageInvoice *plugin_page = user_data;
    GncPluginPageInvoicePrivate *priv;
    GtkWindow *parent;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_INVOICE(plugin_page));

    ENTER("(action %p, plugin_page %p)", simple, plugin_page);
    priv = GNC_PLUGIN_PAGE_INVOICE_GET_PRIVATE(plugin_page);
    parent = GTK_WINDOW (gnc_plugin_page_get_window (GNC_PLUGIN_PAGE (plugin_page)));
    gnc_invoice_window_printCB (parent, priv->iw);
    LEAVE(" ");
}

static void
gnc_plugin_page_invoice_cmd_cut (GSimpleAction *simple,
                                 GVariant *paramter,
                                 gpointer user_data)
{
    GncPluginPageInvoice *plugin_page = user_data;
    GncPluginPageInvoicePrivate *priv;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_INVOICE(plugin_page));

    ENTER("(action %p, plugin_page %p)", simple, plugin_page);
    priv = GNC_PLUGIN_PAGE_INVOICE_GET_PRIVATE(plugin_page);
    gnc_invoice_window_cut_cb(NULL, priv->iw);
    LEAVE(" ");
}

static void
gnc_plugin_page_invoice_cmd_copy (GSimpleAction *simple,
                                  GVariant *paramter,
                                  gpointer user_data)
{
    GncPluginPageInvoice *plugin_page = user_data;
    GncPluginPageInvoicePrivate *priv;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_INVOICE(plugin_page));

    ENTER("(action %p, plugin_page %p)", simple, plugin_page);
    priv = GNC_PLUGIN_PAGE_INVOICE_GET_PRIVATE(plugin_page);
    gnc_invoice_window_copy_cb(NULL, priv->iw);
    LEAVE(" ");
}

static void
gnc_plugin_page_invoice_cmd_paste (GSimpleAction *simple,
                                   GVariant *paramter,
                                   gpointer user_data)
{
    GncPluginPageInvoice *plugin_page = user_data;
    GncPluginPageInvoicePrivate *priv;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_INVOICE(plugin_page));

    ENTER("(action %p, plugin_page %p)", simple, plugin_page);
    priv = GNC_PLUGIN_PAGE_INVOICE_GET_PRIVATE(plugin_page);
    gnc_invoice_window_paste_cb(NULL, priv->iw);
    LEAVE(" ");
}

static void
gnc_plugin_page_invoice_cmd_edit (GSimpleAction *simple,
                                  GVariant *paramter,
                                  gpointer user_data)
{
    GncPluginPageInvoice *plugin_page = user_data;
    GncPluginPageInvoicePrivate *priv;
    GtkWindow *parent;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_INVOICE(plugin_page));

    ENTER("(action %p, plugin_page %p)", simple, plugin_page);
    priv = GNC_PLUGIN_PAGE_INVOICE_GET_PRIVATE(plugin_page);
    parent = GTK_WINDOW (gnc_plugin_page_get_window (GNC_PLUGIN_PAGE (plugin_page)));
    gnc_invoice_window_editCB (parent, priv->iw);
    LEAVE(" ");
}

static void
gnc_plugin_page_invoice_cmd_edit_tax (GSimpleAction *simple,
                                      GVariant *paramter,
                                      gpointer user_data)
{
    GncPluginPageInvoice *plugin_page = user_data;
    GtkWidget *parent;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_INVOICE(plugin_page));

    ENTER("(action %p, plugin_page %p)", simple, plugin_page);
    parent = GTK_WIDGET(gnc_plugin_page_get_window (GNC_PLUGIN_PAGE(plugin_page)));
    gnc_tax_info_dialog (parent, NULL);
    LEAVE(" ");
}

static void
gnc_plugin_page_invoice_cmd_duplicateInvoice (GSimpleAction *simple,
                                              GVariant *paramter,
                                              gpointer user_data)
{
    GncPluginPageInvoice *plugin_page = user_data;
    GncPluginPageInvoicePrivate *priv;
    GtkWindow *parent;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_INVOICE(plugin_page));

    ENTER("(action %p, plugin_page %p)", simple, plugin_page);
    priv = GNC_PLUGIN_PAGE_INVOICE_GET_PRIVATE(plugin_page);
    parent = GTK_WINDOW (gnc_plugin_page_get_window (GNC_PLUGIN_PAGE (plugin_page)));
    gnc_invoice_window_duplicateInvoiceCB(parent, priv->iw);
    LEAVE(" ");
}

static void
gnc_plugin_page_invoice_cmd_post (GSimpleAction *simple,
                                  GVariant *paramter,
                                  gpointer user_data)
{
    GncPluginPageInvoice *plugin_page = user_data;
    GncPluginPageInvoicePrivate *priv;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_INVOICE(plugin_page));

    ENTER("(action %p, plugin_page %p)", simple, plugin_page);
    priv = GNC_PLUGIN_PAGE_INVOICE_GET_PRIVATE(plugin_page);
    gnc_invoice_window_postCB(NULL, priv->iw);
    LEAVE(" ");
}

static void
gnc_plugin_page_invoice_cmd_unpost (GSimpleAction *simple,
                                    GVariant *paramter,
                                    gpointer user_data)
{
    GncPluginPageInvoice *plugin_page = user_data;
    GncPluginPageInvoicePrivate *priv;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_INVOICE(plugin_page));

    ENTER("(action %p, plugin_page %p)", simple, plugin_page);
    priv = GNC_PLUGIN_PAGE_INVOICE_GET_PRIVATE(plugin_page);
    gnc_invoice_window_unpostCB(NULL, priv->iw);
    LEAVE(" ");
}

static void
gnc_plugin_page_invoice_cmd_sort_changed (GSimpleAction *simple,
                                          GVariant *parameter,
                                          gpointer user_data)
{
    GncPluginPageInvoicePrivate *priv;
    GncPluginPageInvoice *plugin_page = user_data;
    gint item;

    g_return_if_fail (G_IS_SIMPLE_ACTION(simple));
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_INVOICE(plugin_page));

    ENTER("action %p, plugin_page (%p), item is %d",
           simple, plugin_page, g_variant_get_int32 (parameter));

    priv = GNC_PLUGIN_PAGE_INVOICE_GET_PRIVATE(plugin_page);
    item = g_variant_get_int32 (parameter);
    g_action_change_state (G_ACTION(simple), parameter);
    gnc_invoice_window_sort (priv->iw, item);
    LEAVE(" ");
}

static void
gnc_plugin_page_invoice_cmd_refresh (GSimpleAction *simple,
                                     GVariant *paramter,
                                     gpointer user_data)
{
    GncPluginPageInvoice *plugin_page = user_data;
    GncPluginPageInvoicePrivate *priv;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_INVOICE(plugin_page));

    ENTER("(action %p, plugin_page %p)", simple, plugin_page);
    priv = GNC_PLUGIN_PAGE_INVOICE_GET_PRIVATE(plugin_page);

    gtk_widget_queue_draw (priv->widget);
    LEAVE(" ");
}

static void
gnc_plugin_page_invoice_cmd_enter (GSimpleAction *simple,
                                   GVariant *paramter,
                                   gpointer user_data)
{
    GncPluginPageInvoice *plugin_page = user_data;
    GncPluginPageInvoicePrivate *priv;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_INVOICE(plugin_page));

    ENTER("(action %p, plugin_page %p)", simple, plugin_page);
    priv = GNC_PLUGIN_PAGE_INVOICE_GET_PRIVATE(plugin_page);
    gnc_invoice_window_recordCB(NULL, priv->iw);
    LEAVE(" ");
}

static void
gnc_plugin_page_invoice_cmd_cancel (GSimpleAction *simple,
                                    GVariant *paramter,
                                    gpointer user_data)
{
    GncPluginPageInvoice *plugin_page = user_data;
    GncPluginPageInvoicePrivate *priv;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_INVOICE(plugin_page));

    ENTER("(action %p, plugin_page %p)", simple, plugin_page);
    priv = GNC_PLUGIN_PAGE_INVOICE_GET_PRIVATE(plugin_page);
    gnc_invoice_window_cancelCB(NULL, priv->iw);
    LEAVE(" ");
}

static void
gnc_plugin_page_invoice_cmd_delete (GSimpleAction *simple,
                                    GVariant *paramter,
                                    gpointer user_data)
{
    GncPluginPageInvoice *plugin_page = user_data;
    GncPluginPageInvoicePrivate *priv;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_INVOICE(plugin_page));

    ENTER("(action %p, plugin_page %p)", simple, plugin_page);
    priv = GNC_PLUGIN_PAGE_INVOICE_GET_PRIVATE(plugin_page);
    gnc_invoice_window_deleteCB(NULL, priv->iw);
    LEAVE(" ");
}

static void
gnc_plugin_page_invoice_cmd_blank (GSimpleAction *simple,
                                   GVariant *paramter,
                                   gpointer user_data)
{
    GncPluginPageInvoice *plugin_page = user_data;
    GncPluginPageInvoicePrivate *priv;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_INVOICE(plugin_page));

    ENTER("(action %p, plugin_page %p)", simple, plugin_page);
    priv = GNC_PLUGIN_PAGE_INVOICE_GET_PRIVATE(plugin_page);
    gnc_invoice_window_blankCB(NULL, priv->iw);
    LEAVE(" ");
}

static void
gnc_plugin_page_invoice_cmd_duplicateEntry (GSimpleAction *simple,
                                            GVariant *paramter,
                                            gpointer user_data)
{
    GncPluginPageInvoice *plugin_page = user_data;
    GncPluginPageInvoicePrivate *priv;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_INVOICE(plugin_page));

    ENTER("(action %p, plugin_page %p)", simple, plugin_page);
    priv = GNC_PLUGIN_PAGE_INVOICE_GET_PRIVATE(plugin_page);
    gnc_invoice_window_duplicateCB(NULL, priv->iw);
    LEAVE(" ");
}

static void
gnc_plugin_page_invoice_cmd_entryUp (GSimpleAction *simple,
                                     GVariant *paramter,
                                     gpointer user_data)
{
    GncPluginPageInvoice *plugin_page = user_data;
    GncPluginPageInvoicePrivate *priv;
    g_return_if_fail(GNC_IS_PLUGIN_PAGE_INVOICE(plugin_page));

    ENTER("(action %p, plugin_page %p)", simple, plugin_page);
    priv = GNC_PLUGIN_PAGE_INVOICE_GET_PRIVATE(plugin_page);
    gnc_invoice_window_entryUpCB(NULL, priv->iw);
    LEAVE(" ");
}

static void
gnc_plugin_page_invoice_cmd_entryDown (GSimpleAction *simple,
                                       GVariant *paramter,
                                       gpointer user_data)
{
    GncPluginPageInvoice *plugin_page = user_data;
    GncPluginPageInvoicePrivate *priv;
    g_return_if_fail(GNC_IS_PLUGIN_PAGE_INVOICE(plugin_page));

    ENTER("(action %p, plugin_page %p)", simple, plugin_page);
    priv = GNC_PLUGIN_PAGE_INVOICE_GET_PRIVATE(plugin_page);
    gnc_invoice_window_entryDownCB(NULL, priv->iw);
    LEAVE(" ");
}

static void
gnc_plugin_page_invoice_cmd_pay_invoice (GSimpleAction *simple,
                                         GVariant *paramter,
                                         gpointer user_data)
{
    GncPluginPageInvoice *plugin_page = user_data;
    GncPluginPageInvoicePrivate *priv;
    GtkWindow *parent;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_INVOICE(plugin_page));

    ENTER("(action %p, plugin_page %p)", simple, plugin_page);
    priv = GNC_PLUGIN_PAGE_INVOICE_GET_PRIVATE(plugin_page);
    parent = GTK_WINDOW (gnc_plugin_page_get_window (GNC_PLUGIN_PAGE (plugin_page)));
    gnc_invoice_window_payment_cb (parent, priv->iw);
    LEAVE(" ");
}

static void
gnc_plugin_page_invoice_cmd_save_layout (GSimpleAction *simple,
                                         GVariant *paramter,
                                         gpointer user_data)
{
    GncPluginPageInvoice *plugin_page = user_data;
    GncPluginPageInvoicePrivate *priv;
    GtkWindow *parent;
    GAction *layout_action;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_INVOICE(plugin_page));

    ENTER("(action %p, plugin_page %p)", simple, plugin_page);
    priv = GNC_PLUGIN_PAGE_INVOICE_GET_PRIVATE(plugin_page);
    gnc_invoice_window_save_document_layout_to_user_state (priv->iw);

    layout_action = gnc_plugin_page_get_action (GNC_PLUGIN_PAGE(plugin_page),
                                                "ViewResetLayoutAction");
    g_simple_action_set_enabled (G_SIMPLE_ACTION(layout_action), TRUE);

    LEAVE(" ");
}

static void
gnc_plugin_page_invoice_cmd_reset_layout (GSimpleAction *simple,
                                          GVariant *paramter,
                                          gpointer user_data)
{
    GncPluginPageInvoice *plugin_page = user_data;
    GncPluginPageInvoicePrivate *priv;
    GtkWindow *parent;
    GAction *layout_action;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_INVOICE(plugin_page));

    ENTER("(action %p, plugin_page %p)", simple, plugin_page);
    priv = GNC_PLUGIN_PAGE_INVOICE_GET_PRIVATE(plugin_page);
    gnc_invoice_window_reset_document_layout_and_clear_user_state (priv->iw);

    layout_action = gnc_plugin_page_get_action (GNC_PLUGIN_PAGE(plugin_page),
                                                "ViewResetLayoutAction");
    g_simple_action_set_enabled (G_SIMPLE_ACTION(layout_action), FALSE);

    LEAVE(" ");
}

static void
gnc_plugin_page_invoice_cmd_link (GSimpleAction *simple,
                                  GVariant *paramter,
                                  gpointer user_data)
{
    GncPluginPageInvoice *plugin_page = user_data;
    GncPluginPageInvoicePrivate *priv;
    GtkWindow *parent;
    GncInvoice *invoice;
    const gchar *uri;
    gchar *ret_uri;
    gboolean has_uri = FALSE;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_INVOICE(plugin_page));
    ENTER("(action %p, plugin_page %p)", simple, plugin_page);
    priv = GNC_PLUGIN_PAGE_INVOICE_GET_PRIVATE(plugin_page);
    parent = GTK_WINDOW(gnc_plugin_page_get_window (GNC_PLUGIN_PAGE(plugin_page)));

    invoice = gnc_invoice_window_get_invoice (priv->iw);
    uri = gncInvoiceGetDocLink (invoice);

    ret_uri = gnc_doclink_get_uri_dialog (parent, _("Manage Document Link"), uri);

    if (ret_uri)
        has_uri = TRUE;

    if (ret_uri && g_strcmp0 (uri, ret_uri) != 0)
    {
        GtkWidget *doclink_button =
            gnc_invoice_window_get_doclink_button (priv->iw);

        if (g_strcmp0 (ret_uri, "") == 0)
        {
            has_uri = FALSE;
            if (doclink_button)
                gtk_widget_hide (GTK_WIDGET(doclink_button));
        }
        else
        {
            if (doclink_button)
            {
                gchar *display_uri =
                    gnc_doclink_get_unescaped_just_uri (ret_uri);
                gtk_link_button_set_uri (GTK_LINK_BUTTON(doclink_button),
                                         display_uri);
                gtk_widget_show (GTK_WIDGET(doclink_button));
                g_free (display_uri);
            }
        }
        gncInvoiceSetDocLink (invoice, ret_uri);
    }
    // update the menu actions
    update_doclink_actions (GNC_PLUGIN_PAGE(plugin_page), has_uri);

    g_free (ret_uri);
    LEAVE(" ");
}

static void
gnc_plugin_page_invoice_cmd_link_remove (GSimpleAction *simple,
                                         GVariant *paramter,
                                         gpointer user_data)
{
    GncPluginPageInvoice *plugin_page = user_data;
    GncPluginPageInvoicePrivate *priv;
    GtkWindow *parent;
    GncInvoice *invoice;
    GtkWidget *doclink_button;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_INVOICE(plugin_page));
    ENTER("(action %p, plugin_page %p)", simple, plugin_page);
    priv = GNC_PLUGIN_PAGE_INVOICE_GET_PRIVATE(plugin_page);
    parent = GTK_WINDOW(gnc_plugin_page_get_window (GNC_PLUGIN_PAGE(plugin_page)));

    invoice = gnc_invoice_window_get_invoice (priv->iw);
    gncInvoiceSetDocLink (invoice, "");

    doclink_button = gnc_invoice_window_get_doclink_button (priv->iw);

    if (doclink_button)
        gtk_widget_hide (GTK_WIDGET(doclink_button));

    // update the menu actions
    update_doclink_actions (GNC_PLUGIN_PAGE(plugin_page), FALSE);

    LEAVE(" ");
}

static void
gnc_plugin_page_invoice_cmd_link_open (GSimpleAction *simple,
                                       GVariant *paramter,
                                       gpointer user_data)
{
    GncPluginPageInvoice *plugin_page = user_data;
    GncPluginPageInvoicePrivate *priv;
    GtkWindow *parent;
    GncInvoice *invoice;
    const gchar *uri = NULL;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_INVOICE(plugin_page));
    ENTER("(action %p, plugin_page %p)", simple, plugin_page);
    priv = GNC_PLUGIN_PAGE_INVOICE_GET_PRIVATE(plugin_page);
    parent = GTK_WINDOW(gnc_plugin_page_get_window (GNC_PLUGIN_PAGE(plugin_page)));

    invoice = gnc_invoice_window_get_invoice (priv->iw);
    uri = gncInvoiceGetDocLink (invoice);

    if (uri)
        gnc_doclink_open_uri (parent, uri);

    LEAVE(" ");
}

static void
gnc_plugin_page_invoice_cmd_company_report (GSimpleAction *simple,
                                            GVariant *paramter,
                                            gpointer user_data)
{
    GncPluginPageInvoice *plugin_page = user_data;
    GncPluginPageInvoicePrivate *priv;
    GtkWindow *parent;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_INVOICE(plugin_page));

    ENTER("(action %p, plugin_page %p)", simple, plugin_page);
    priv = GNC_PLUGIN_PAGE_INVOICE_GET_PRIVATE(plugin_page);
    parent = GTK_WINDOW (gnc_plugin_page_get_window (GNC_PLUGIN_PAGE (plugin_page)));
    gnc_invoice_window_report_owner_cb (parent, priv->iw);
    LEAVE(" ");
}

/************************************************************/
/*                    Auxiliary functions                   */
/************************************************************/

static void
gnc_plugin_page_redraw_help_cb (GnucashRegister *g_reg,
                                GncPluginPageInvoice *invoice_page)
{
    GncPluginPageInvoicePrivate *priv;
    GncWindow *window;
    const char *status;
    char *help;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_INVOICE(invoice_page));

    window = GNC_WINDOW(GNC_PLUGIN_PAGE(invoice_page)->window);

    /* Get the text from the ledger */
    priv = GNC_PLUGIN_PAGE_INVOICE_GET_PRIVATE(invoice_page);
    help = gnc_invoice_get_help(priv->iw);
    status = help ? help : g_strdup("");
    gnc_window_set_status(window, GNC_PLUGIN_PAGE(invoice_page), status);
    g_free(help);
}


void
gnc_plugin_page_invoice_update_title (GncPluginPage *plugin_page)
{
    GncPluginPageInvoice *page;
    GncPluginPageInvoicePrivate *priv;
    gchar *title;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_INVOICE(plugin_page));

    page = GNC_PLUGIN_PAGE_INVOICE(plugin_page);
    priv = GNC_PLUGIN_PAGE_INVOICE_GET_PRIVATE(page);
    title = gnc_invoice_get_title(priv->iw);
    main_window_update_page_name(plugin_page, title);
    g_free(title);
}

static void
gnc_plugin_page_invoice_refresh_cb (GHashTable *changes, gpointer user_data)
{
    GncPluginPageInvoice *page = user_data;
    GncPluginPageInvoicePrivate *priv;
    GtkWidget *reg;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_INVOICE(page));

    /* We're only looking for forced updates here. */
    if (changes)
        return;

    priv = GNC_PLUGIN_PAGE_INVOICE_GET_PRIVATE(page);
    reg = gnc_invoice_get_register(priv->iw);
    gnucash_register_refresh_from_prefs(GNUCASH_REGISTER(reg));
    gtk_widget_queue_draw(priv->widget);
}
