/*
 * gnc-plugin-business.c --
 *
 * Author: Jan Arne Petersen <jpetersen@uni-bonn.de>
 * Copyright (C) 2003 Jan Arne Petersen
 * Copyright (C) 2005 David Hampton <hampton@employees.org>
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
#include <string.h>

#include "dialog-doclink.h"
#include "dialog-billterms.h"
#include "dialog-customer.h"
#include "dialog-employee.h"
#include "dialog-invoice.h"
#include "dialog-job.h"
#include "dialog-payment.h"
#include "dialog-tax-table.h"
#include "dialog-vendor.h"
#include "business-gnome-utils.h"
#include "gnc-plugin-business.h"
#include "gnc-plugin-page-invoice.h"
#include "gnc-plugin-page-owner-tree.h"
#include "gncOwner.h"
#include "gnc-ui-util.h"
#include "gnc-date.h"
#include "gnc-file.h"
#include "guile-mappings.h"
#include "gnc-session.h"
#include "gnc-icons.h" /* for GNC_ICON_INVOICE_NEW */

#include "gnc-prefs.h"
#include "gnc-main-window.h"

#include "gnc-plugin-page-register.h"

/* This static indicates the debugging module that this .o belongs to.  */
G_GNUC_UNUSED static QofLogModule log_module = G_LOG_DOMAIN;

/* g_object functions */
static void gnc_plugin_business_class_init (GncPluginBusinessClass *klass);
static void gnc_plugin_business_init (GncPluginBusiness *plugin);
static void gnc_plugin_business_finalize (GObject *object);
static void gnc_plugin_business_add_to_window (GncPlugin *plugin,
                                               GncMainWindow *window,
                                               GQuark type);

/* Command callbacks */
static void gnc_plugin_business_cmd_customer_page (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void gnc_plugin_business_cmd_customer_new_customer (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void gnc_plugin_business_cmd_customer_find_customer (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void gnc_plugin_business_cmd_customer_new_invoice (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void gnc_plugin_business_cmd_customer_find_invoice (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void gnc_plugin_business_cmd_customer_new_job (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void gnc_plugin_business_cmd_customer_find_job (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void gnc_plugin_business_cmd_customer_process_payment (GSimpleAction *simple, GVariant *parameter, gpointer user_data);

static void gnc_plugin_business_cmd_vendor_page (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void gnc_plugin_business_cmd_vendor_new_vendor (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void gnc_plugin_business_cmd_vendor_find_vendor (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void gnc_plugin_business_cmd_vendor_new_bill (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void gnc_plugin_business_cmd_vendor_find_bill (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void gnc_plugin_business_cmd_vendor_new_job (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void gnc_plugin_business_cmd_vendor_find_job (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void gnc_plugin_business_cmd_vendor_process_payment (GSimpleAction *simple, GVariant *parameter, gpointer user_data);

static void gnc_plugin_business_cmd_employee_page (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void gnc_plugin_business_cmd_employee_new_employee (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void gnc_plugin_business_cmd_employee_find_employee (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void gnc_plugin_business_cmd_employee_new_expense_voucher (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void gnc_plugin_business_cmd_employee_find_expense_voucher (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void gnc_plugin_business_cmd_employee_process_payment (GSimpleAction *simple, GVariant *parameter, gpointer user_data);

static void gnc_plugin_business_cmd_doclink (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void gnc_plugin_business_cmd_tax_tables (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void gnc_plugin_business_cmd_billing_terms (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void gnc_plugin_business_cmd_bills_due_reminder (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void gnc_plugin_business_cmd_invoices_due_reminder (GSimpleAction *simple, GVariant *parameter, gpointer user_data);

static void gnc_plugin_business_cmd_test_search (GSimpleAction *simple, GVariant *parameter, gpointer user_data);
static void gnc_plugin_business_cmd_test_init_data (GSimpleAction *simple, GVariant *parameter, gpointer user_data);

static void gnc_plugin_business_cmd_assign_payment (GSimpleAction *simple, GVariant *parameter, gpointer user_data);

static void update_inactive_actions (GncPluginPage *page);

#define PLUGIN_ACTIONS_NAME "gnc-plugin-business-actions"
#define PLUGIN_UI_FILENAME  "gnc-plugin-business.ui"

#define GNC_PREF_EXTRA_TOOLBUTTONS "enable-toolbuttons"
#define GNC_PREF_INV_PRINT_RPT     "invoice-printreport"

/** This variable maintains a pointer to the last window where a
 *  Business command was executed.  It is used to determine where new
 *  invoices will be placed.  This is a HACK done instead of trudging
 *  through several layers of dialog (search dialog, new invoice
 *  dialog) forcing them to track the original parent window.
 */
static GncMainWindow *last_window = NULL;

static GActionEntry gnc_plugin_actions [] =
{
    { "BusinessAction", NULL, NULL, NULL, NULL },
    { "CustomerMenuAction", NULL, NULL, NULL, NULL },
    { "CustomerOverviewPageAction", gnc_plugin_business_cmd_customer_page, NULL, NULL, NULL },
    { "CustomerNewCustomerOpenAction", gnc_plugin_business_cmd_customer_new_customer, NULL, NULL, NULL },
    { "CustomerFindCustomerOpenAction", gnc_plugin_business_cmd_customer_find_customer, NULL, NULL, NULL },
    { "CustomerNewInvoiceOpenAction", gnc_plugin_business_cmd_customer_new_invoice, NULL, NULL, NULL },
    { "CustomerFindInvoiceOpenAction", gnc_plugin_business_cmd_customer_find_invoice, NULL, NULL, NULL },
    { "CustomerNewJobOpenAction", gnc_plugin_business_cmd_customer_new_job, NULL, NULL, NULL },
    { "CustomerFindJobOpenAction", gnc_plugin_business_cmd_customer_find_job, NULL, NULL, NULL },
    { "CustomerProcessPaymentAction", gnc_plugin_business_cmd_customer_process_payment, NULL, NULL, NULL },

    { "VendorMenuAction", NULL, NULL, NULL, NULL },
    { "VendorOverviewPageAction", gnc_plugin_business_cmd_vendor_page, NULL, NULL, NULL },
    { "VendorNewVendorOpenAction", gnc_plugin_business_cmd_vendor_new_vendor, NULL, NULL, NULL },
    { "VendorFindVendorOpenAction", gnc_plugin_business_cmd_vendor_find_vendor, NULL, NULL, NULL },
    { "VendorNewBillOpenAction", gnc_plugin_business_cmd_vendor_new_bill, NULL, NULL, NULL },
    { "VendorFindBillOpenAction", gnc_plugin_business_cmd_vendor_find_bill, NULL, NULL, NULL },
    { "VendorNewJobOpenAction", gnc_plugin_business_cmd_vendor_new_job, NULL, NULL, NULL },
    { "VendorFindJobOpenAction", gnc_plugin_business_cmd_vendor_find_job, NULL, NULL, NULL },
    { "VendorProcessPaymentAction", gnc_plugin_business_cmd_vendor_process_payment, NULL, NULL, NULL },

    { "EmployeeMenuAction", NULL, NULL, NULL, NULL },
    { "EmployeeOverviewPageAction", gnc_plugin_business_cmd_employee_page, NULL, NULL, NULL },
    { "EmployeeNewEmployeeOpenAction", gnc_plugin_business_cmd_employee_new_employee, NULL, NULL, NULL },
    { "EmployeeFindEmployeeOpenAction", gnc_plugin_business_cmd_employee_find_employee, NULL, NULL, NULL },
    { "EmployeeNewExpenseVoucherOpenAction", gnc_plugin_business_cmd_employee_new_expense_voucher, NULL, NULL, NULL },
    { "EmployeeFindExpenseVoucherOpenAction", gnc_plugin_business_cmd_employee_find_expense_voucher, NULL, NULL, NULL },
    { "EmployeeProcessPaymentAction", gnc_plugin_business_cmd_employee_process_payment, NULL, NULL, NULL },

    { "BusinessLinkedDocsAction", gnc_plugin_business_cmd_doclink, NULL, NULL, NULL },
    { "TaxTablesOpenAction", gnc_plugin_business_cmd_tax_tables, NULL, NULL, NULL },
    { "BillingTermsOpenAction", gnc_plugin_business_cmd_billing_terms, NULL, NULL, NULL },
    { "BillsDueReminderOpenAction", gnc_plugin_business_cmd_bills_due_reminder, NULL, NULL, NULL },
    { "InvoicesDueReminderOpenAction", gnc_plugin_business_cmd_invoices_due_reminder, NULL, NULL, NULL },

    { "BusinessTestAction", NULL, NULL, NULL, NULL },
    { "BusinessTestSearchAction", gnc_plugin_business_cmd_test_search, NULL, NULL, NULL },
    { "BusinessTestInitDataAction", gnc_plugin_business_cmd_test_init_data, NULL, NULL, NULL },
    { "ToolbarNewInvoiceAction", gnc_plugin_business_cmd_customer_new_invoice, NULL, NULL, NULL },
    { "RegisterAssignPayment", gnc_plugin_business_cmd_assign_payment, NULL, NULL, NULL },
//FIXMEb    { "RegisterEditPayment", gnc_plugin_business_cmd_assign_payment, NULL, NULL, NULL },
};
/** The number of actions provided by this plugin. */
static guint gnc_plugin_n_actions = G_N_ELEMENTS(gnc_plugin_actions);

static GncDisplayItem gnc_plugin_display_items [] =
{
    /* Toplevel */
    { "BusinessAction", NULL, N_("_Business"), NULL, NULL },

    /* Customer submenu */
    { "CustomerMenuAction", NULL, N_("_Customer"), NULL, NULL },
    {
        "CustomerOverviewPageAction", NULL, N_("Customers Overview"), NULL,
        N_("Open a Customer overview page")
    },
    {
        "CustomerNewCustomerOpenAction", NULL, N_("_New Customer..."), NULL,
        N_("Open the New Customer dialog")
    },
    {
        "CustomerFindCustomerOpenAction", NULL, N_("_Find Customer..."), NULL,
        N_("Open the Find Customer dialog")
    },
    {
        "CustomerNewInvoiceOpenAction", NULL, N_("New _Invoice..."), NULL,
        N_("Open the New Invoice dialog")
    },
    {
        "CustomerFindInvoiceOpenAction", NULL, N_("Find In_voice..."), NULL,
        N_("Open the Find Invoice dialog")
    },
    {
        "CustomerNewJobOpenAction", NULL, N_("New _Job..."), NULL,
        N_("Open the New Job dialog")
    },
    {
        "CustomerFindJobOpenAction", NULL, N_("Find Jo_b..."), NULL,
        N_("Open the Find Job dialog")
    },
    {
        "CustomerProcessPaymentAction", NULL, N_("_Process Payment..."), NULL,
        N_("Open the Process Payment dialog")
    },

    /* Vendor submenu */
    {
        "VendorOverviewPageAction", NULL, N_("Vendors Overview"), NULL,
        N_("Open a Vendor overview page")
    },
    { "VendorMenuAction", NULL, N_("_Vendor"), NULL, NULL },
    {
        "VendorNewVendorOpenAction", NULL, N_("_New Vendor..."), NULL,
        N_("Open the New Vendor dialog")
    },
    {
        "VendorFindVendorOpenAction", NULL, N_("_Find Vendor..."), NULL,
        N_("Open the Find Vendor dialog")
    },
    {
        "VendorNewBillOpenAction", NULL, N_("New _Bill..."), NULL,
        N_("Open the New Bill dialog")
    },
    {
        "VendorFindBillOpenAction", NULL, N_("Find Bi_ll..."), NULL,
        N_("Open the Find Bill dialog")
    },
    {
        "VendorNewJobOpenAction", NULL, N_("New _Job..."), NULL,
        N_("Open the New Job dialog")
    },
    {
        "VendorFindJobOpenAction", NULL, N_("Find Jo_b..."), NULL,
        N_("Open the Find Job dialog")
    },
    {
        "VendorProcessPaymentAction", NULL, N_("_Process Payment..."), NULL,
        N_("Open the Process Payment dialog")
    },

    /* Employee submenu */
    {
        "EmployeeOverviewPageAction", NULL, N_("Employees Overview"), NULL,
        N_("Open a Employee overview page")
    },
    { "EmployeeMenuAction", NULL, N_("_Employee"), NULL, NULL },
    {
        "EmployeeNewEmployeeOpenAction", NULL, N_("_New Employee..."), NULL,
        N_("Open the New Employee dialog")
    },
    {
        "EmployeeFindEmployeeOpenAction", NULL, N_("_Find Employee..."), NULL,
        N_("Open the Find Employee dialog")
    },
    {
        "EmployeeNewExpenseVoucherOpenAction", NULL, N_("New _Expense Voucher..."), NULL,
        N_("Open the New Expense Voucher dialog")
    },
    {
        "EmployeeFindExpenseVoucherOpenAction", NULL, N_("Find Expense _Voucher..."), NULL,
        N_("Open the Find Expense Voucher dialog")
    },
    {
        "EmployeeProcessPaymentAction", NULL, N_("_Process Payment..."), NULL,
        N_("Open the Process Payment dialog")
    },

    /* Other menu items */
    {
        "BusinessLinkedDocsAction", NULL, N_("Business Linked Documents"), NULL,
        N_("View all Linked Business Documents")
    },
    {
        "TaxTablesOpenAction", NULL, N_("Sales _Tax Table"), NULL,
        N_("View and edit the list of Sales Tax Tables (GST/VAT)")
    },
    {
        "BillingTermsOpenAction", NULL, N_("_Billing Terms Editor"), NULL,
        N_("View and edit the list of Billing Terms")
    },
    {
        "BillsDueReminderOpenAction", NULL, N_("Bills _Due Reminder"), NULL,
        N_("Open the Bills Due Reminder dialog")
    },
    {
        "InvoicesDueReminderOpenAction", NULL, N_("Invoices _Due Reminder"), NULL,
        N_("Open the Invoices Due Reminder dialog")
    },
    { "ExportMenuAction", NULL, N_("E_xport"), NULL, NULL },

    /* Extensions Menu */
    { "BusinessTestAction", NULL, N_("_Business"), NULL, NULL },
    {
        "BusinessTestSearchAction", NULL, N_("Test Search Dialog"), NULL,
        N_("Test Search Dialog")
    },
    {
        "BusinessTestInitDataAction", NULL, N_("Initialize Test Data"), NULL,
        N_("Initialize Test Data")
    },

    /* Toolbar */
    {
        "ToolbarNewInvoiceAction", GNC_ICON_INVOICE_NEW, N_("New _Invoice..."), NULL,
        N_("Open the New Invoice dialog")
    },

    /* Register popup menu */
    {
        "RegisterAssignPayment", NULL, N_("Assign as payment..."), NULL,
        N_("Assign the selected transaction as payment")
    },
    {
        "RegisterEditPayment", NULL, N_("Edit payment..."), NULL,
        N_("Edit the payment this transaction is a part of")
    },
};
/** The number of display items provided by this plugin. */
static guint gnc_plugin_n_display_items = G_N_ELEMENTS(gnc_plugin_display_items);

/************************************************************
 *              Plugin Function Implementation              *
 ************************************************************/

typedef struct GncPluginBusinessPrivate
{
    GncOwner *last_customer;
    GncOwner *last_vendor;
    GncOwner *last_employee;
} GncPluginBusinessPrivate;

#define GNC_PLUGIN_BUSINESS_GET_PRIVATE(o)  \
   ((GncPluginBusinessPrivate*)gnc_plugin_business_get_instance_private((GncPluginBusiness*)o))

static GObjectClass *parent_class = NULL;

GncPlugin *
gnc_plugin_business_new (void)
{
    GncPluginBusiness *plugin;

    /* Reference the invoice page plugin to ensure it exists in
     * the gtk type system. */
    GNC_TYPE_PLUGIN_PAGE_INVOICE;
    GNC_TYPE_PLUGIN_PAGE_OWNER_TREE;

    plugin = g_object_new (GNC_TYPE_PLUGIN_BUSINESS,
                           (char *)NULL);

    return GNC_PLUGIN (plugin);
}

G_DEFINE_TYPE_WITH_PRIVATE(GncPluginBusiness, gnc_plugin_business, GNC_TYPE_PLUGIN)

static void
gnc_plugin_business_class_init (GncPluginBusinessClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);
    GncPluginClass *plugin_class = GNC_PLUGIN_CLASS (klass);

    parent_class = g_type_class_peek_parent (klass);

    object_class->finalize = gnc_plugin_business_finalize;

    /* plugin info */
    plugin_class->plugin_name  = GNC_PLUGIN_BUSINESS_NAME;

    /* function overrides */
    plugin_class->add_to_window = gnc_plugin_business_add_to_window;

    /* widget addition/removal */
    plugin_class->actions_name    = PLUGIN_ACTIONS_NAME;
    plugin_class->actionsb        = gnc_plugin_actions;
    plugin_class->n_actionsb      = gnc_plugin_n_actions;
    plugin_class->display_items   = gnc_plugin_display_items;
    plugin_class->n_display_items = gnc_plugin_n_display_items;
    plugin_class->ui_filename     = PLUGIN_UI_FILENAME;
}

static void
gnc_plugin_business_init (GncPluginBusiness *plugin)
{
    GncPluginBusinessPrivate *priv;

    priv = GNC_PLUGIN_BUSINESS_GET_PRIVATE (plugin);
    priv->last_customer = gncOwnerNew ();
    gncOwnerInitCustomer (priv->last_customer, NULL);

    priv->last_vendor = gncOwnerNew ();
    gncOwnerInitVendor (priv->last_vendor, NULL);

    priv->last_employee = gncOwnerNew ();
    gncOwnerInitEmployee (priv->last_employee, NULL);
}

static void
gnc_plugin_business_finalize (GObject *object)
{
    g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (object));

    G_OBJECT_CLASS (parent_class)->finalize (object);
}

/************************************************************
 *                     Helper Functions                     *
 ************************************************************/

GncMainWindow*
gnc_plugin_business_get_window()
{
    return last_window;
}

/************************************************************
 *                    Command Callbacks                     *
 ************************************************************/

static void
gnc_plugin_business_cmd_customer_page (GSimpleAction *simple,
                                       GVariant      *parameter,
                                       gpointer       user_data)
{
    GncMainWindowActionData *mw = user_data;
    GncPluginPage *page;

    g_return_if_fail (mw != NULL);
    g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

    page = gnc_plugin_page_owner_tree_new (GNC_OWNER_CUSTOMER);
    gnc_main_window_open_page (mw->window, page);
}

static void
gnc_plugin_business_cmd_customer_new_customer (GSimpleAction *simple,
                                               GVariant      *parameter,
                                               gpointer       user_data)
{
    GncMainWindowActionData *mw = user_data;

    g_return_if_fail (mw != NULL);
    g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

    gnc_ui_customer_new (GTK_WINDOW (mw->window), gnc_get_current_book ());
}

static void
gnc_plugin_business_cmd_customer_find_customer (GSimpleAction *simple,
                                                GVariant      *parameter,
                                                gpointer       user_data)
{
    GncMainWindowActionData *mw = user_data;
    GncPluginBusiness *plugin;
    GncPluginBusinessPrivate *priv;
    GncCustomer*customer;

    g_return_if_fail (mw != NULL);
    g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

    plugin = GNC_PLUGIN_BUSINESS (mw->data);
    priv = GNC_PLUGIN_BUSINESS_GET_PRIVATE (plugin);
    customer = gncOwnerGetCustomer (priv->last_customer);
    gnc_customer_search (GTK_WINDOW (mw->window), customer, gnc_get_current_book ());
}

static void
gnc_plugin_business_cmd_customer_new_invoice (GSimpleAction *simple,
                                              GVariant      *parameter,
                                              gpointer       user_data)
{
    GncMainWindowActionData *mw = user_data;
    GncPluginBusiness *plugin;
    GncPluginBusinessPrivate *priv;

    g_return_if_fail (mw != NULL);
    g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

    plugin = GNC_PLUGIN_BUSINESS (mw->data);
    priv = GNC_PLUGIN_BUSINESS_GET_PRIVATE (plugin);
    last_window = mw->window;
    gnc_ui_invoice_new (GTK_WINDOW (mw->window), priv->last_customer, gnc_get_current_book ());
}

static void
gnc_plugin_business_cmd_customer_find_invoice (GSimpleAction *simple,
                                               GVariant      *parameter,
                                               gpointer       user_data)
{
    GncMainWindowActionData *mw = user_data;
    GncPluginBusiness *plugin;
    GncPluginBusinessPrivate *priv;

    g_return_if_fail (mw != NULL);
    g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

    plugin = GNC_PLUGIN_BUSINESS (mw->data);
    priv = GNC_PLUGIN_BUSINESS_GET_PRIVATE (plugin);
    last_window = mw->window;
    gnc_invoice_search (GTK_WINDOW (mw->window), NULL, priv->last_customer, gnc_get_current_book ());
}

static void
gnc_plugin_business_cmd_customer_new_job (GSimpleAction *simple,
                                          GVariant      *parameter,
                                          gpointer       user_data)
{
    GncMainWindowActionData *mw = user_data;
    GncPluginBusiness *plugin;
    GncPluginBusinessPrivate *priv;

    g_return_if_fail (mw != NULL);
    g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

    plugin = GNC_PLUGIN_BUSINESS (mw->data);
    priv = GNC_PLUGIN_BUSINESS_GET_PRIVATE (plugin);
    gnc_ui_job_new (GTK_WINDOW (mw->window), priv->last_customer, gnc_get_current_book ());
}

static void
gnc_plugin_business_cmd_customer_find_job (GSimpleAction *simple,
                                           GVariant      *parameter,
                                           gpointer       user_data)
{
    GncMainWindowActionData *mw = user_data;
    GncPluginBusiness *plugin;
    GncPluginBusinessPrivate *priv;

    g_return_if_fail (mw != NULL);
    g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

    plugin = GNC_PLUGIN_BUSINESS (mw->data);
    priv = GNC_PLUGIN_BUSINESS_GET_PRIVATE (plugin);
    gnc_job_search (GTK_WINDOW (mw->window), NULL, priv->last_customer, gnc_get_current_book ());
}

static void
gnc_plugin_business_cmd_customer_process_payment (GSimpleAction *simple,
                                                  GVariant      *parameter,
                                                  gpointer       user_data)
{
    GncMainWindowActionData *mw = user_data;
    GncPluginBusiness *plugin;
    GncPluginBusinessPrivate *priv;

    g_return_if_fail (mw != NULL);
    g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

    plugin = GNC_PLUGIN_BUSINESS (mw->data);
    priv = GNC_PLUGIN_BUSINESS_GET_PRIVATE (plugin);
    gnc_ui_payment_new (GTK_WINDOW (mw->window), priv->last_customer, gnc_get_current_book ());
}

static void
gnc_plugin_business_cmd_vendor_page (GSimpleAction *simple,
                                     GVariant      *parameter,
                                     gpointer       user_data)
{
    GncMainWindowActionData *mw = user_data;
    GncPluginPage *page;

    g_return_if_fail (mw != NULL);
    g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

    page = gnc_plugin_page_owner_tree_new (GNC_OWNER_VENDOR);
    gnc_main_window_open_page (mw->window, page);
}

static void
gnc_plugin_business_cmd_vendor_new_vendor (GSimpleAction *simple,
                                           GVariant      *parameter,
                                           gpointer       user_data)
{
    GncMainWindowActionData *mw = user_data;
    g_return_if_fail (mw != NULL);
    g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

    gnc_ui_vendor_new (GTK_WINDOW (mw->window), gnc_get_current_book ());
}

static void
gnc_plugin_business_cmd_vendor_find_vendor (GSimpleAction *simple,
                                            GVariant      *parameter,
                                            gpointer       user_data)
{
    GncMainWindowActionData *mw = user_data;
    GncPluginBusiness *plugin;
    GncPluginBusinessPrivate *priv;
    GncVendor *vendor;

    g_return_if_fail (mw != NULL);
    g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

    plugin = GNC_PLUGIN_BUSINESS (mw->data);
    priv = GNC_PLUGIN_BUSINESS_GET_PRIVATE (plugin);
    vendor = gncOwnerGetVendor (priv->last_vendor);
    gnc_vendor_search (GTK_WINDOW (mw->window), vendor, gnc_get_current_book ());
}

static void
gnc_plugin_business_cmd_vendor_new_bill (GSimpleAction *simple,
                                         GVariant      *parameter,
                                         gpointer       user_data)
{
    GncMainWindowActionData *mw = user_data;
    GncPluginBusiness *plugin;
    GncPluginBusinessPrivate *priv;

    g_return_if_fail (mw != NULL);
    g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

    plugin = GNC_PLUGIN_BUSINESS (mw->data);
    priv = GNC_PLUGIN_BUSINESS_GET_PRIVATE (plugin);
    last_window = mw->window;
    gnc_ui_invoice_new (GTK_WINDOW (mw->window), priv->last_vendor, gnc_get_current_book ());
}

static void
gnc_plugin_business_cmd_vendor_find_bill (GSimpleAction *simple,
                                          GVariant      *parameter,
                                          gpointer       user_data)
{
    GncMainWindowActionData *mw = user_data;
    GncPluginBusiness *plugin;
    GncPluginBusinessPrivate *priv;

    g_return_if_fail (mw != NULL);
    g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

    plugin = GNC_PLUGIN_BUSINESS (mw->data);
    priv = GNC_PLUGIN_BUSINESS_GET_PRIVATE (plugin);
    last_window = mw->window;
    gnc_invoice_search (GTK_WINDOW (mw->window), NULL, priv->last_vendor, gnc_get_current_book ());
}

static void
gnc_plugin_business_cmd_vendor_new_job (GSimpleAction *simple,
                                        GVariant      *parameter,
                                        gpointer       user_data)
{
    GncMainWindowActionData *mw = user_data;
    GncPluginBusiness *plugin;
    GncPluginBusinessPrivate *priv;

    g_return_if_fail (mw != NULL);
    g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

    plugin = GNC_PLUGIN_BUSINESS (mw->data);
    priv = GNC_PLUGIN_BUSINESS_GET_PRIVATE (plugin);
    gnc_ui_job_new (GTK_WINDOW (mw->window), priv->last_vendor, gnc_get_current_book ());
}

static void
gnc_plugin_business_cmd_vendor_find_job (GSimpleAction *simple,
                                         GVariant      *parameter,
                                         gpointer       user_data)
{
    GncMainWindowActionData *mw = user_data;
    GncPluginBusiness *plugin;
    GncPluginBusinessPrivate *priv;

    g_return_if_fail (mw != NULL);
    g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

    plugin = GNC_PLUGIN_BUSINESS (mw->data);
    priv = GNC_PLUGIN_BUSINESS_GET_PRIVATE (plugin);
    gnc_job_search (GTK_WINDOW (mw->window), NULL, priv->last_vendor, gnc_get_current_book ());
}

static void
gnc_plugin_business_cmd_vendor_process_payment (GSimpleAction *simple,
                                                GVariant      *parameter,
                                                gpointer       user_data)
{
    GncMainWindowActionData *mw = user_data;
    GncPluginBusiness *plugin;
    GncPluginBusinessPrivate *priv;

    g_return_if_fail (mw != NULL);
    g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

    plugin = GNC_PLUGIN_BUSINESS (mw->data);
    priv = GNC_PLUGIN_BUSINESS_GET_PRIVATE (plugin);
    gnc_ui_payment_new (GTK_WINDOW (mw->window), priv->last_vendor, gnc_get_current_book ());
}

static void
gnc_plugin_business_cmd_employee_page (GSimpleAction *simple,
                                       GVariant      *parameter,
                                       gpointer       user_data)
{
    GncMainWindowActionData *mw = user_data;
    GncPluginPage *page;

    g_return_if_fail (mw != NULL);
    g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

    page = gnc_plugin_page_owner_tree_new (GNC_OWNER_EMPLOYEE);
    gnc_main_window_open_page (mw->window, page);
}

static void
gnc_plugin_business_cmd_employee_new_employee (GSimpleAction *simple,
                                               GVariant      *parameter,
                                               gpointer       user_data)
{
    GncMainWindowActionData *mw = user_data;
    g_return_if_fail (mw != NULL);
    g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

    gnc_ui_employee_new (GTK_WINDOW (mw->window), gnc_get_current_book ());
}

static void
gnc_plugin_business_cmd_employee_find_employee (GSimpleAction *simple,
                                                GVariant      *parameter,
                                                gpointer       user_data)
{
    GncMainWindowActionData *mw = user_data;
    GncPluginBusiness *plugin;
    GncPluginBusinessPrivate *priv;
    GncEmployee *employee;

    g_return_if_fail (mw != NULL);
    g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

    plugin = GNC_PLUGIN_BUSINESS (mw->data);
    priv = GNC_PLUGIN_BUSINESS_GET_PRIVATE (plugin);
    employee = gncOwnerGetEmployee (priv->last_employee);
    gnc_employee_search (GTK_WINDOW (mw->window), employee, gnc_get_current_book ());
}

static void
gnc_plugin_business_cmd_employee_new_expense_voucher (GSimpleAction *simple,
                                                      GVariant      *parameter,
                                                      gpointer       user_data)
{
    GncMainWindowActionData *mw = user_data;
    GncPluginBusiness *plugin;
    GncPluginBusinessPrivate *priv;

    g_return_if_fail (mw != NULL);
    g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

    plugin = GNC_PLUGIN_BUSINESS (mw->data);
    priv = GNC_PLUGIN_BUSINESS_GET_PRIVATE (plugin);
    last_window = mw->window;
    gnc_ui_invoice_new (GTK_WINDOW (mw->window), priv->last_employee, gnc_get_current_book ());
}

static void
gnc_plugin_business_cmd_employee_find_expense_voucher (GSimpleAction *simple,
                                                       GVariant      *parameter,
                                                       gpointer       user_data)
{
    GncMainWindowActionData *mw = user_data;
    GncPluginBusiness *plugin;
    GncPluginBusinessPrivate *priv;

    g_return_if_fail (mw != NULL);
    g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

    plugin = GNC_PLUGIN_BUSINESS (mw->data);
    priv = GNC_PLUGIN_BUSINESS_GET_PRIVATE (plugin);
    last_window = mw->window;
    gnc_invoice_search (GTK_WINDOW (mw->window), NULL, priv->last_employee, gnc_get_current_book ());
}

static void
gnc_plugin_business_cmd_employee_process_payment (GSimpleAction *simple,
                                                  GVariant      *parameter,
                                                  gpointer       user_data)
{
    GncMainWindowActionData *mw = user_data;
    GncPluginBusiness *plugin;
    GncPluginBusinessPrivate *priv;

    g_return_if_fail (mw != NULL);
    g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

    plugin = GNC_PLUGIN_BUSINESS (mw->data);
    priv = GNC_PLUGIN_BUSINESS_GET_PRIVATE (plugin);
    gnc_ui_payment_new (GTK_WINDOW (mw->window), priv->last_employee, gnc_get_current_book ());
}

static void
gnc_plugin_business_cmd_doclink (GSimpleAction *simple,
                                 GVariant      *parameter,
                                 gpointer       user_data)
{
    GncMainWindowActionData *mw = user_data;
    g_return_if_fail (mw != NULL);
    g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

    gnc_doclink_business_dialog (GTK_WINDOW (mw->window));
}

static void
gnc_plugin_business_cmd_tax_tables (GSimpleAction *simple,
                                    GVariant      *parameter,
                                    gpointer       user_data)
{
    GncMainWindowActionData *mw = user_data;
    g_return_if_fail (mw != NULL);
    g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

    gnc_ui_tax_table_window_new (GTK_WINDOW (mw->window), gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_billing_terms (GSimpleAction *simple,
                                       GVariant      *parameter,
                                       gpointer       user_data)
{
    GncMainWindowActionData *mw = user_data;
    g_return_if_fail (mw != NULL);
    g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

    gnc_ui_billterms_window_new (GTK_WINDOW (mw->window), gnc_get_current_book());
}


static void
gnc_plugin_business_cmd_bills_due_reminder (GSimpleAction *simple,
                                            GVariant      *parameter,
                                            gpointer       user_data)
{
    GncMainWindowActionData *mw = user_data;
    g_return_if_fail (mw != NULL);
    g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

    gnc_invoice_remind_bills_due (GTK_WINDOW (mw->window));
}


static void
gnc_plugin_business_cmd_invoices_due_reminder (GSimpleAction *simple,
                                               GVariant      *parameter,
                                               gpointer       user_data)
{
    GncMainWindowActionData *mw = user_data;
    g_return_if_fail (mw != NULL);
    g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

    gnc_invoice_remind_invoices_due (GTK_WINDOW (mw->window));
}

static void
gnc_plugin_business_cmd_test_search (GSimpleAction *simple,
                                     GVariant      *parameter,
                                     gpointer       user_data)
{
    GncMainWindowActionData *mw = user_data;
    gnc_search_dialog_test();
}

static void
gnc_business_assign_payment (GtkWindow *parent,
                             Transaction *trans,
                             GncOwner *owner)
{
    g_return_if_fail(trans);

    // Do nothing if we don't have more than one split (e.g. in the empty line at the end of the register)
    if (xaccTransCountSplits(trans) <= 1)
        return;

    //PINFO("Creating payment dialog with trans %p", trans);
    gnc_ui_payment_new_with_txn(parent, owner, trans);
}

static void
gnc_plugin_business_cmd_assign_payment (GSimpleAction *simple,
                                        GVariant      *parameter,
                                        gpointer       user_data)
{
    GncMainWindowActionData *mw = user_data;
    GncPluginBusiness *plugin_business;
    GncPluginBusinessPrivate *plugin_business_priv;
    GncPluginPage *plugin_page;
    GNCSplitReg *gsr;
    SplitRegister *reg;
    Split *split;
    Transaction *trans;
    gboolean have_owner;
    GncOwner owner;
    GncOwner *owner_p;

    g_return_if_fail (mw != NULL);
    g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

    plugin_page = gnc_main_window_get_current_page(mw->window);

    // We continue only if the current page is a plugin page and more
    // specifically a register plugin page
    if (!GNC_IS_PLUGIN_PAGE(plugin_page)
            || !GNC_IS_PLUGIN_PAGE_REGISTER(plugin_page))
        return;

    gsr = gnc_plugin_page_register_get_gsr(plugin_page);
    g_return_if_fail(gsr);

    reg = gnc_ledger_display_get_split_register( gsr->ledger );
    g_return_if_fail(reg);

    split = gnc_split_register_get_current_split(reg);
    g_return_if_fail(split);

    trans = xaccSplitGetParent(split);
    g_return_if_fail(trans);

    plugin_business = GNC_PLUGIN_BUSINESS (mw->data);
    plugin_business_priv = GNC_PLUGIN_BUSINESS_GET_PRIVATE (plugin_business);

    have_owner = gncOwnerGetOwnerFromTxn (trans, &owner);
    if (have_owner)
        owner_p = &owner;
    else if (gnc_ui_payment_is_customer_payment(trans))
        owner_p = plugin_business_priv->last_customer;
    else
        owner_p = plugin_business_priv->last_vendor;

    gnc_business_assign_payment (GTK_WINDOW (mw->window),
                                 trans, owner_p);
}

static const gchar *register_txn_actions[] =
{
    "RegisterAssignPayment",
    NULL
};

static const gchar *register_bus_txn_actions[] =
{
    "RegisterEditPayment",
    NULL
};

static void
gnc_plugin_business_update_menus (GncPluginPage *plugin_page)
{
    GncMainWindow  *window;
    GtkActionGroup *action_group;
    gboolean is_txn_register, is_bus_txn = FALSE, is_bus_doc = FALSE;

    // We continue only if the current page is a plugin page
    if (!plugin_page || !GNC_IS_PLUGIN_PAGE(plugin_page))
        return;

    // Check that this is a main window and not embedded sx
    if (!GNC_IS_MAIN_WINDOW(plugin_page->window))
        return;

    is_txn_register = GNC_IS_PLUGIN_PAGE_REGISTER(plugin_page);
    window = GNC_MAIN_WINDOW(plugin_page->window);
    g_return_if_fail(GNC_IS_MAIN_WINDOW(window));
    action_group = gnc_main_window_get_action_group(window, PLUGIN_ACTIONS_NAME);
    g_return_if_fail(GTK_IS_ACTION_GROUP(action_group));

    if (is_txn_register)
    {
        Transaction *trans = gnc_plugin_page_register_get_current_txn (GNC_PLUGIN_PAGE_REGISTER(plugin_page));
        if (trans && xaccTransCountSplits(trans) > 0)
            is_bus_txn = (xaccTransGetFirstAPARAcctSplit(trans, TRUE) != NULL);
        is_bus_doc = (xaccTransGetTxnType (trans) == TXN_TYPE_INVOICE);
    }
    // Change visibility and also sensitivity according to whether we are in a txn register
    gnc_plugin_update_actions (action_group, register_txn_actions,
                               "sensitive", is_txn_register && !is_bus_txn && !is_bus_doc);
    gnc_plugin_update_actions (action_group, register_txn_actions,
                               "visible", is_txn_register && !is_bus_txn && !is_bus_doc);
    gnc_plugin_update_actions (action_group, register_bus_txn_actions,
                               "sensitive", is_txn_register && is_bus_txn && !is_bus_doc);
    gnc_plugin_update_actions (action_group, register_bus_txn_actions,
                               "visible", is_txn_register && is_bus_txn && !is_bus_doc);
}


static void
gnc_plugin_business_main_window_page_changed (GncMainWindow *window,
                                              GncPluginPage *page,
                                              gpointer user_data)
{
    gnc_plugin_business_update_menus(page);
    update_inactive_actions(page);
}


void
gnc_plugin_business_split_reg_ui_update (GncPluginPage *plugin_page)
{
    gnc_plugin_business_main_window_page_changed(NULL, plugin_page, NULL);
}

static void
gnc_plugin_business_cmd_test_init_data (GSimpleAction *simple,
                                        GVariant      *parameter,
                                        gpointer       user_data)
{
    GncMainWindowActionData *mw = user_data;
    QofBook *book           = gnc_get_current_book();
    GncCustomer *customer   = gncCustomerCreate(book);
    GncAddress *address     = gncCustomerGetAddr(customer);
    GncInvoice *invoice     = gncInvoiceCreate(book);
    GncOwner *owner         = gncOwnerNew();
    GncJob *job             = gncJobCreate(book);
    Account *root           = gnc_book_get_root_account(book);
    Account *inc_acct       = xaccMallocAccount(book);
    Account *bank_acct      = xaccMallocAccount(book);
    Account *tax_acct       = xaccMallocAccount(book);
    Account *ar_acct        = xaccMallocAccount(book);

    // Create Customer
    gncCustomerSetID(customer, "000001");
    gncCustomerSetName(customer, "Test Customer");
    gncCustomerSetCurrency(customer, gnc_default_currency());
    gncAddressSetName(address, "Contact Person");
    gncAddressSetAddr1(address, "20 Customer Lane");
    gncAddressSetAddr2(address, "Customer M/S");
    gncAddressSetAddr3(address, "Addr3, XXX  12345");

    // Create the Owner
    gncOwnerInitCustomer(owner, customer);

    // Create the Invoice
    gncInvoiceSetID(invoice, "000012");
    gncInvoiceSetOwner(invoice, owner);
    gncInvoiceSetDateOpened(invoice, gnc_time (NULL));
    gncInvoiceSetCurrency(invoice, gnc_default_currency());

    // Create the Job
    gncJobSetID(job, "000025");
    gncJobSetName(job, "Test Job");
    gncJobSetReference(job, "Customer's ref#");
    gncJobSetOwner(job, owner);

    // MODIFY THE OWNER
    gncOwnerInitJob(owner, job);

    // Create the A/R account
    xaccAccountSetType(ar_acct, ACCT_TYPE_RECEIVABLE);
    xaccAccountSetName(ar_acct, "A/R");
    xaccAccountSetCommodity(ar_acct, gnc_default_currency());
    gnc_account_append_child(root, ar_acct);

    // Create the Income account
    xaccAccountSetType(inc_acct, ACCT_TYPE_INCOME);
    xaccAccountSetName(inc_acct, "Income");
    xaccAccountSetCommodity(inc_acct, gnc_default_currency());
    gnc_account_append_child(root, inc_acct);

    // Create the Bank account
    xaccAccountSetType(bank_acct, ACCT_TYPE_BANK);
    xaccAccountSetName(bank_acct, "Bank");
    xaccAccountSetCommodity(bank_acct, gnc_default_currency());
    gnc_account_append_child(root, bank_acct);

    // Create the Tax account
    xaccAccountSetType(tax_acct, ACCT_TYPE_LIABILITY);
    xaccAccountSetName(tax_acct, "Tax-Holding");
    xaccAccountSetCommodity(tax_acct, gnc_default_currency());
    gnc_account_append_child(root, tax_acct);

    // Launch the invoice editor
    gnc_ui_invoice_edit (GTK_WINDOW (mw->window), invoice);
}

/* This is the list of actions which are switched inactive in a read-only book. */
static const gchar* readonly_inactive_actions[] =
{
    "CustomerNewCustomerOpenAction",
    "CustomerNewInvoiceOpenAction",
    "CustomerNewInvoiceOpenAction",
    "CustomerNewJobOpenAction",
    "CustomerProcessPaymentAction",
    "VendorNewVendorOpenAction",
    "VendorNewBillOpenAction",
    "VendorNewJobOpenAction",
    "VendorProcessPaymentAction",
    "EmployeeNewEmployeeOpenAction",
    "EmployeeNewExpenseVoucherOpenAction",
    "EmployeeProcessPaymentAction",
    "ToolbarNewInvoiceAction",
    "RegisterAssignPayment",
    "RegisterEditPayment",
    NULL
};

static void
update_inactive_actions (GncPluginPage *plugin_page)
{
    GncMainWindow  *window;
    GtkActionGroup *action_group;

    // We are readonly - so we have to switch particular actions to inactive.
    gboolean is_readwrite = !qof_book_is_readonly(gnc_get_current_book());

    // We continue only if the current page is a plugin page
    if (!plugin_page || !GNC_IS_PLUGIN_PAGE(plugin_page))
        return;

    // Check that this is a main window and not embedded sx
    if (!GNC_IS_MAIN_WINDOW(plugin_page->window))
        return;

    window = GNC_MAIN_WINDOW(plugin_page->window);
    g_return_if_fail(GNC_IS_MAIN_WINDOW(window));
    action_group = gnc_main_window_get_action_group(window, PLUGIN_ACTIONS_NAME);
    g_return_if_fail(GTK_IS_ACTION_GROUP(action_group));

    /* Set the action's sensitivity */
    gnc_plugin_update_actions (action_group, readonly_inactive_actions,
                               "sensitive", is_readwrite);
}

/* This is the list of actions which are switched invisible or visible
 * depending on the preference "extra_toolbuttons". */
static const char* extra_toolbar_actions[] =
{
    "ToolbarNewInvoiceAction",
    NULL
};

/* Bind the visibility of the extra toolbar buttons to the
 * enable_toolbuttons preference. */
static void
bind_toolbuttons_visibility (GncMainWindow *mainwindow)
{
    GtkActionGroup *action_group;
    const char **iter;

    g_return_if_fail(mainwindow);
    g_return_if_fail(GNC_IS_MAIN_WINDOW(mainwindow));

    /* Get the action group */
//FIXMEb    action_group = gnc_main_window_get_action_group (mainwindow, PLUGIN_ACTIONS_NAME);
//    g_assert(action_group);

//    for (iter = extra_toolbar_actions; *iter; ++iter)
//    {
        /* Set the action's visibility */
//        GtkAction *action = gtk_action_group_get_action (action_group, *iter);
//        gnc_prefs_bind (GNC_PREFS_GROUP_INVOICE, GNC_PREF_EXTRA_TOOLBUTTONS, G_OBJECT (action), "visible");
//    }
}

/**
 * Called when this plugin is added to a main window.  Connect a few callbacks
 * here to track page changes.
 *
 * Update the toolbar button visibility each time our plugin is added
 * to a new GncMainWindow. */
static void
gnc_plugin_business_add_to_window (GncPlugin *plugin,
                                   GncMainWindow *mainwindow,
                                   GQuark type)
{
    bind_toolbuttons_visibility (mainwindow);

    g_signal_connect (mainwindow, "page_changed",
                      G_CALLBACK(gnc_plugin_business_main_window_page_changed),
                      plugin);
}

static const char* invoice_printreport_values[] =
{
    /* The list below are the guids of reports that can
     * be used to print an invoice.
     * Important: this list must be kept in sync with the one at the end
     * of business-prefs.glade
     */
    "5123a759ceb9483abf2182d01c140e8d", // "Printable Invoice"
    "0769e242be474010b4acf264a5512e6e", // "Tax Invoice"
    "67112f318bef4fc496bdc27d106bbda4", // "Easy Invoice"
    "3ce293441e894423a2425d7a22dd1ac6", // "Fancy Invoice"
    NULL
};

const char *
gnc_plugin_business_get_invoice_printreport (void)
{
    int value = gnc_prefs_get_int (GNC_PREFS_GROUP_INVOICE, GNC_PREF_INV_PRINT_RPT);
    if (value >= 0 && value < 4)
        return invoice_printreport_values[value];
    else
        return NULL;
}
