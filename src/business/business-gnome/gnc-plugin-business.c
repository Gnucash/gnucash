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

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <string.h>
#include <libguile.h>

#include "dialog-billterms.h"
#include "dialog-customer.h"
#include "dialog-employee.h"
#include "dialog-invoice.h"
#include "dialog-job.h"
#include "dialog-payment.h"
#include "dialog-tax-table.h"
#include "dialog-vendor.h"
#include "gnc-plugin-business.h"
#include "gnc-plugin-page-invoice.h"
#include "gncOwner.h"
#include "gnc-ui-util.h"
#include "gnc-date.h"
#include "gnc-file.h"
#include "guile-mappings.h"
#include "gnc-session.h"

/* g_object functions */
static void gnc_plugin_business_class_init (GncPluginBusinessClass *klass);
static void gnc_plugin_business_init (GncPluginBusiness *plugin);
static void gnc_plugin_business_finalize (GObject *object);

/* Command callbacks */
static void gnc_plugin_business_cmd_customer_new_customer    (GtkAction *action,
							      GncMainWindowActionData *data);
static void gnc_plugin_business_cmd_customer_find_customer   (GtkAction *action,
							      GncMainWindowActionData *data);
static void gnc_plugin_business_cmd_customer_new_invoice     (GtkAction *action,
							      GncMainWindowActionData *data);
static void gnc_plugin_business_cmd_customer_find_invoice    (GtkAction *action,
							      GncMainWindowActionData *data);
static void gnc_plugin_business_cmd_customer_new_job         (GtkAction *action,
							      GncMainWindowActionData *data);
static void gnc_plugin_business_cmd_customer_find_job        (GtkAction *action,
							      GncMainWindowActionData *data);
static void gnc_plugin_business_cmd_customer_process_payment (GtkAction *action,
							      GncMainWindowActionData *data);

static void gnc_plugin_business_cmd_vendor_new_vendor      (GtkAction *action,
							    GncMainWindowActionData *data);
static void gnc_plugin_business_cmd_vendor_find_vendor     (GtkAction *action,
							    GncMainWindowActionData *data);
static void gnc_plugin_business_cmd_vendor_new_bill        (GtkAction *action,
							    GncMainWindowActionData *data);
static void gnc_plugin_business_cmd_vendor_find_bill       (GtkAction *action,
							    GncMainWindowActionData *data);
static void gnc_plugin_business_cmd_vendor_new_job         (GtkAction *action,
							    GncMainWindowActionData *data);
static void gnc_plugin_business_cmd_vendor_find_job        (GtkAction *action,
							    GncMainWindowActionData *data);
static void gnc_plugin_business_cmd_vendor_process_payment (GtkAction *action,
							    GncMainWindowActionData *data);

static void gnc_plugin_business_cmd_employee_new_employee         (GtkAction *action,
								   GncMainWindowActionData *data);
static void gnc_plugin_business_cmd_employee_find_employee        (GtkAction *action,
								   GncMainWindowActionData *data);
static void gnc_plugin_business_cmd_employee_new_expense_voucher  (GtkAction *action,
								   GncMainWindowActionData *data);
static void gnc_plugin_business_cmd_employee_find_expense_voucher (GtkAction *action,
								   GncMainWindowActionData *data);
static void gnc_plugin_business_cmd_employee_process_payment      (GtkAction *action,
								   GncMainWindowActionData *data);

static void gnc_plugin_business_cmd_tax_tables         (GtkAction *action,
							GncMainWindowActionData *data);
static void gnc_plugin_business_cmd_billing_terms      (GtkAction *action,
							GncMainWindowActionData *data);
static void gnc_plugin_business_cmd_bills_due_reminder (GtkAction *action,
							GncMainWindowActionData *data);

static void gnc_plugin_business_cmd_export_invoice  (GtkAction *action,
						     GncMainWindowActionData *data);

static void gnc_plugin_business_cmd_export_customer (GtkAction *action,
						     GncMainWindowActionData *data);

static void gnc_plugin_business_cmd_export_vendor   (GtkAction *action,
						     GncMainWindowActionData *data);

static void gnc_plugin_business_cmd_export_employee (GtkAction *action,
						     GncMainWindowActionData *data);

static void gnc_plugin_business_cmd_test_search (GtkAction *action,
						 GncMainWindowActionData *data);

static void gnc_plugin_business_cmd_test_reload_invoice_report (GtkAction *action,
								GncMainWindowActionData *data);

static void gnc_plugin_business_cmd_test_reload_owner_report (GtkAction *action,
							      GncMainWindowActionData *data);

static void gnc_plugin_business_cmd_test_reload_receivable_report (GtkAction *action,
								   GncMainWindowActionData *data);

static void gnc_plugin_business_cmd_test_init_data (GtkAction *action,
						    GncMainWindowActionData *data);

/*static void gnc_plugin_business_cmd_export_report   (GtkAction *action,
						      GncMainWindowActionData *data);*/

#define PLUGIN_ACTIONS_NAME "gnc-plugin-business-actions"
#define PLUGIN_UI_FILENAME  "gnc-plugin-business-ui.xml"

/** This variable maintains a pointer to the last window where a
 *  Business command was executed.  It is used to determine where new
 *  invoices will be placed.  This is a HACK done instead of trudging
 *  through several layers of dialog (search dialog, new invoice
 *  dialog) forcing them to track the original parent window.
 */
static GncMainWindow *last_window = NULL;

static GtkActionEntry gnc_plugin_actions [] = 
{
	/* Toplevel */
	{ "BusinessAction", NULL, N_("_Business"), NULL, NULL, NULL },

	/* Customer submenu */
	{ "CustomerMenuAction", NULL, N_("_Customer"), NULL, NULL, NULL },
	{ "CustomerNewCustomerOpenAction", NULL, N_("_New Customer..."), NULL,
	  N_("Open the New Customer dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_customer_new_customer) },
	{ "CustomerFindCustomerOpenAction", NULL, N_("_Find Customer..."), NULL,
	  N_("Open the Find Customer dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_customer_find_customer) },
	{ "CustomerNewInvoiceOpenAction", NULL, N_("New _Invoice..."), NULL,
	  N_("Open the New Invoice dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_customer_new_invoice) },
	{ "CustomerFindInvoiceOpenAction", NULL, N_("Find In_voice..."), NULL,
	  N_("Open the Find Invoice dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_customer_find_invoice) },
	{ "CustomerNewJobOpenAction", NULL, N_("New _Job..."), NULL,
	  N_("Open the New Job dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_customer_new_job) },
	{ "CustomerFindJobOpenAction", NULL, N_("Find Jo_b..."), NULL,
	  N_("Open the Find Job dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_customer_find_job) },
	{ "CustomerProcessPaymentAction", NULL, N_("_Process Payment..."), NULL,
	  N_("Open the Process Payment dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_customer_process_payment) },

	/* Vendor submenu */
	{ "VendorMenuAction", NULL, N_("_Vendor"), NULL, NULL, NULL },
	{ "VendorNewVendorOpenAction", NULL, N_("_New Vendor..."), NULL,
	  N_("Open the New Vendor dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_vendor_new_vendor) },
	{ "VendorFindVendorOpenAction", NULL, N_("_Find Vendor..."), NULL,
	  N_("Open the Find Vendor dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_vendor_find_vendor) },
	{ "VendorNewBillOpenAction", NULL, N_("New _Bill..."), NULL,
	  N_("Open the New Bill dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_vendor_new_bill) },
	{ "VendorFindBillOpenAction", NULL, N_("Find Bi_ll..."), NULL,
	  N_("Open the Find Bill dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_vendor_find_bill) },
	{ "VendorNewJobOpenAction", NULL, N_("New _Job..."), NULL,
	  N_("Open the New Job dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_vendor_new_job) },
	{ "VendorFindJobOpenAction", NULL, N_("Find Jo_b..."), NULL,
	  N_("Open the Find Job dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_vendor_find_job) },
	{ "VendorProcessPaymentAction", NULL, N_("_Process Payment..."), NULL,
	  N_("Open the Process Payment dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_vendor_process_payment) },

	/* Employee submenu */
	{ "EmployeeMenuAction", NULL, N_("_Employee"), NULL, NULL, NULL },
	{ "EmployeeNewEmployeeOpenAction", NULL, N_("_New Employee..."), NULL,
	  N_("Open the New Employee dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_employee_new_employee) },
	{ "EmployeeFindEmployeeOpenAction", NULL, N_("_Find Employee..."), NULL,
	  N_("Open the Find Employee dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_employee_find_employee) },
	{ "EmployeeNewExpenseVoucherOpenAction", NULL, N_("New _Expense Voucher..."), NULL,
	  N_("Open the New Expense Voucher dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_employee_new_expense_voucher) },
	{ "EmployeeFindExpenseVoucherOpenAction", NULL, N_("Find Expense _Voucher..."), NULL,
	  N_("Open the Find Expense Voucher dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_employee_find_expense_voucher) },
	{ "EmployeeProcessPaymentAction", NULL, N_("_Process Payment..."), NULL,
	  N_("Open the Process Payment dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_employee_process_payment) },

	/* Other menu items */
	{ "TaxTablesOpenAction", NULL, N_("_Tax Table Editor"), NULL,
	  N_("View and edit the list of Tax Tables"),
	  G_CALLBACK (gnc_plugin_business_cmd_tax_tables) },
	{ "BillingTermsOpenAction", NULL, N_("_Billing Terms Editor"), NULL,
	  N_("View and edit the list of Billing Terms"),
	  G_CALLBACK (gnc_plugin_business_cmd_billing_terms) },
	{ "BillsDueReminderOpenAction", NULL, N_("Bills _Due Reminder"), NULL,
	  N_("Open the Bills Due Reminder dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_bills_due_reminder) },
	{ "ExportMenuAction", NULL, N_("E_xport"), NULL, NULL, NULL },
	{ "QSFInvoiceAction", NULL, N_("QSF _Invoice..."), NULL,
	  N_("Export one or more invoices to QSF"),
	  G_CALLBACK (gnc_plugin_business_cmd_export_invoice) },
	{ "QSFCustomerAction", NULL, N_("QSF _Customer..."), NULL,
	  N_("Export one or more customers to QSF"),
	  G_CALLBACK (gnc_plugin_business_cmd_export_customer) },
	{ "QSFVendorAction", NULL, N_("QSF _Vendor..."), NULL,
	  N_("Export one or more vendors to QSF"),
	  G_CALLBACK (gnc_plugin_business_cmd_export_vendor) },
	{ "QSFEmployeeAction", NULL, N_("QSF _Employee..."), NULL,
	  N_("Export one or more employees to QSF"),
	  G_CALLBACK (gnc_plugin_business_cmd_export_employee) },

	/* Extensions Menu */
	{ "BusinessTestAction", NULL, N_("_Business"), NULL, NULL, NULL },
	{ "BusinessTestSearchAction", NULL, N_("Test Search Dialog"), NULL,
	  N_("Test Search Dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_test_search) },
	{ "BusinessTestReloadInvoiceAction", NULL, N_("Reload invoice report"), NULL,
	  N_("Reload invoice report scheme file"),
	  G_CALLBACK (gnc_plugin_business_cmd_test_reload_invoice_report) },
	{ "BusinessTestReloadOwnerAction", NULL, N_("Reload owner report"), NULL,
	  N_("Reload owner report scheme file"),
	  G_CALLBACK (gnc_plugin_business_cmd_test_reload_owner_report) },
	{ "BusinessTestReloadReceivableAction", NULL, N_("Reload receivable report"), NULL,
	  N_("Reload receivable report scheme file"),
	  G_CALLBACK (gnc_plugin_business_cmd_test_reload_receivable_report) },
	{ "BusinessTestInitDataAction", NULL, N_("Initialize Test Data"), NULL,
	  N_("Initialize Test Data"),
	  G_CALLBACK (gnc_plugin_business_cmd_test_init_data) },
};
static guint gnc_plugin_n_actions = G_N_ELEMENTS (gnc_plugin_actions);


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
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_PLUGIN_BUSINESS, GncPluginBusinessPrivate))

static GObjectClass *parent_class = NULL;

GType
gnc_plugin_business_get_type (void)
{
	static GType gnc_plugin_business_type = 0;

	if (gnc_plugin_business_type == 0) {
		static const GTypeInfo our_info = {
			sizeof (GncPluginBusinessClass),
			NULL,		/* base_init */
			NULL,		/* base_finalize */
			(GClassInitFunc) gnc_plugin_business_class_init,
			NULL,		/* class_finalize */
			NULL,		/* class_data */
			sizeof (GncPluginBusiness),
			0,		/* n_preallocs */
			(GInstanceInitFunc) gnc_plugin_business_init
		};
		
		gnc_plugin_business_type = g_type_register_static (GNC_TYPE_PLUGIN,
								   "GncPluginBusiness",
								   &our_info, 0);
	}

	return gnc_plugin_business_type;
}

GncPlugin *
gnc_plugin_business_new (void)
{
	GncPluginBusiness *plugin;

	/* Reference the invoice page plugin to ensure it exists in
	 * the gtk type system. */
	GNC_TYPE_PLUGIN_PAGE_INVOICE;

	plugin = g_object_new (GNC_TYPE_PLUGIN_BUSINESS,
			      (char *)NULL);

	return GNC_PLUGIN (plugin);
}

static void
gnc_plugin_business_class_init (GncPluginBusinessClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	GncPluginClass *plugin_class = GNC_PLUGIN_CLASS (klass);

	parent_class = g_type_class_peek_parent (klass);

	object_class->finalize = gnc_plugin_business_finalize;

	/* plugin info */
	plugin_class->plugin_name  = GNC_PLUGIN_BUSINESS_NAME;

	/* widget addition/removal */
	plugin_class->actions_name = PLUGIN_ACTIONS_NAME;
	plugin_class->actions      = gnc_plugin_actions;
	plugin_class->n_actions    = gnc_plugin_n_actions;
	plugin_class->ui_filename  = PLUGIN_UI_FILENAME;

	g_type_class_add_private(klass, sizeof(GncPluginBusinessPrivate));
}

static void
gnc_plugin_business_init (GncPluginBusiness *plugin)
{
	GncPluginBusinessPrivate *priv;

	priv = GNC_PLUGIN_BUSINESS_GET_PRIVATE (plugin);
	priv->last_customer = gncOwnerCreate ();
	gncOwnerInitCustomer (priv->last_customer, NULL);

	priv->last_vendor = gncOwnerCreate ();
	gncOwnerInitVendor (priv->last_vendor, NULL);

	priv->last_employee = gncOwnerCreate ();
	gncOwnerInitEmployee (priv->last_employee, NULL);
}

static void
gnc_plugin_business_finalize (GObject *object)
{
	GncPluginBusiness *plugin;
	GncPluginBusinessPrivate *priv;

	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (object));

	plugin = GNC_PLUGIN_BUSINESS (object);
	priv = GNC_PLUGIN_BUSINESS_GET_PRIVATE (plugin);

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
gnc_plugin_business_cmd_customer_new_customer (GtkAction *action,
					       GncMainWindowActionData *mw)
{
	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	gnc_ui_customer_new (gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_customer_find_customer (GtkAction *action,
						GncMainWindowActionData *mw)
{
	GncPluginBusiness *plugin;
	GncPluginBusinessPrivate *priv;
	GncCustomer*customer;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	plugin = GNC_PLUGIN_BUSINESS (mw->data);
	priv = GNC_PLUGIN_BUSINESS_GET_PRIVATE (plugin);
	customer = gncOwnerGetCustomer (priv->last_customer);
	gnc_customer_search (customer, gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_customer_new_invoice (GtkAction *action,
					      GncMainWindowActionData *mw)
{
	GncPluginBusiness *plugin;
	GncPluginBusinessPrivate *priv;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	plugin = GNC_PLUGIN_BUSINESS (mw->data);
	priv = GNC_PLUGIN_BUSINESS_GET_PRIVATE (plugin);
	last_window = mw->window;
	gnc_ui_invoice_new (priv->last_customer, gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_customer_find_invoice (GtkAction *action,
					       GncMainWindowActionData *mw)
{
	GncPluginBusiness *plugin;
	GncPluginBusinessPrivate *priv;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	plugin = GNC_PLUGIN_BUSINESS (mw->data);
	priv = GNC_PLUGIN_BUSINESS_GET_PRIVATE (plugin);
	last_window = mw->window;
	gnc_invoice_search (NULL, priv->last_customer, gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_customer_new_job (GtkAction *action,
					  GncMainWindowActionData *mw)
{
	GncPluginBusiness *plugin;
	GncPluginBusinessPrivate *priv;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	plugin = GNC_PLUGIN_BUSINESS (mw->data);
	priv = GNC_PLUGIN_BUSINESS_GET_PRIVATE (plugin);
	gnc_ui_job_new (priv->last_customer, gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_customer_find_job (GtkAction *action,
					   GncMainWindowActionData *mw)
{
	GncPluginBusiness *plugin;
	GncPluginBusinessPrivate *priv;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	plugin = GNC_PLUGIN_BUSINESS (mw->data);
	priv = GNC_PLUGIN_BUSINESS_GET_PRIVATE (plugin);
	gnc_job_search (NULL, priv->last_customer, gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_customer_process_payment (GtkAction *action,
						  GncMainWindowActionData *mw)
{
	GncPluginBusiness *plugin;
	GncPluginBusinessPrivate *priv;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	plugin = GNC_PLUGIN_BUSINESS (mw->data);
	priv = GNC_PLUGIN_BUSINESS_GET_PRIVATE (plugin);
	gnc_ui_payment_new (priv->last_customer, gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_vendor_new_vendor (GtkAction *action,
					   GncMainWindowActionData *mw)
{
	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	gnc_ui_vendor_new (gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_vendor_find_vendor (GtkAction *action,
					    GncMainWindowActionData *mw)
{
	GncPluginBusiness *plugin;
	GncPluginBusinessPrivate *priv;
	GncVendor *vendor;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	plugin = GNC_PLUGIN_BUSINESS (mw->data);
	priv = GNC_PLUGIN_BUSINESS_GET_PRIVATE (plugin);
	vendor = gncOwnerGetVendor (priv->last_vendor);
	gnc_vendor_search (vendor, gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_vendor_new_bill (GtkAction *action,
					 GncMainWindowActionData *mw)
{
	GncPluginBusiness *plugin;
	GncPluginBusinessPrivate *priv;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	plugin = GNC_PLUGIN_BUSINESS (mw->data);
	priv = GNC_PLUGIN_BUSINESS_GET_PRIVATE (plugin);
	last_window = mw->window;
	gnc_ui_invoice_new (priv->last_vendor, gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_vendor_find_bill (GtkAction *action,
					  GncMainWindowActionData *mw)
{
	GncPluginBusiness *plugin;
	GncPluginBusinessPrivate *priv;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	plugin = GNC_PLUGIN_BUSINESS (mw->data);
	priv = GNC_PLUGIN_BUSINESS_GET_PRIVATE (plugin);
	last_window = mw->window;
	gnc_invoice_search (NULL, priv->last_vendor, gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_vendor_new_job (GtkAction *action,
					GncMainWindowActionData *mw)
{
	GncPluginBusiness *plugin;
	GncPluginBusinessPrivate *priv;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	plugin = GNC_PLUGIN_BUSINESS (mw->data);
	priv = GNC_PLUGIN_BUSINESS_GET_PRIVATE (plugin);
	gnc_ui_job_new (priv->last_vendor, gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_vendor_find_job (GtkAction *action,
					 GncMainWindowActionData *mw)
{
	GncPluginBusiness *plugin;
	GncPluginBusinessPrivate *priv;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	plugin = GNC_PLUGIN_BUSINESS (mw->data);
	priv = GNC_PLUGIN_BUSINESS_GET_PRIVATE (plugin);
	gnc_job_search (NULL, priv->last_vendor, gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_vendor_process_payment (GtkAction *action,
						GncMainWindowActionData *mw)
{
	GncPluginBusiness *plugin;
	GncPluginBusinessPrivate *priv;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	plugin = GNC_PLUGIN_BUSINESS (mw->data);
	priv = GNC_PLUGIN_BUSINESS_GET_PRIVATE (plugin);
	gnc_ui_payment_new (priv->last_vendor, gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_employee_new_employee (GtkAction *action,
					       GncMainWindowActionData *mw)
{
	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	gnc_ui_employee_new (gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_employee_find_employee (GtkAction *action,
						GncMainWindowActionData *mw)
{
	GncPluginBusiness *plugin;
	GncPluginBusinessPrivate *priv;
	GncEmployee *employee;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	plugin = GNC_PLUGIN_BUSINESS (mw->data);
	priv = GNC_PLUGIN_BUSINESS_GET_PRIVATE (plugin);
	employee = gncOwnerGetEmployee (priv->last_employee);
	gnc_employee_search (employee, gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_employee_new_expense_voucher (GtkAction *action,
						      GncMainWindowActionData *mw)
{
	GncPluginBusiness *plugin;
	GncPluginBusinessPrivate *priv;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	plugin = GNC_PLUGIN_BUSINESS (mw->data);
	priv = GNC_PLUGIN_BUSINESS_GET_PRIVATE (plugin);
	last_window = mw->window;
	gnc_ui_invoice_new (priv->last_employee, gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_employee_find_expense_voucher (GtkAction *action,
						       GncMainWindowActionData *mw)
{
	GncPluginBusiness *plugin;
	GncPluginBusinessPrivate *priv;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	plugin = GNC_PLUGIN_BUSINESS (mw->data);
	priv = GNC_PLUGIN_BUSINESS_GET_PRIVATE (plugin);
	last_window = mw->window;
	gnc_invoice_search (NULL, priv->last_employee, gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_employee_process_payment (GtkAction *action,
						  GncMainWindowActionData *mw)
{
	GncPluginBusiness *plugin;
	GncPluginBusinessPrivate *priv;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	plugin = GNC_PLUGIN_BUSINESS (mw->data);
	priv = GNC_PLUGIN_BUSINESS_GET_PRIVATE (plugin);
	gnc_ui_payment_new (priv->last_employee, gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_tax_tables (GtkAction *action,
				    GncMainWindowActionData *mw)
{
	GncPluginBusiness *plugin;
	GncPluginBusinessPrivate *priv;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	plugin = GNC_PLUGIN_BUSINESS (mw->data);
	priv = GNC_PLUGIN_BUSINESS_GET_PRIVATE (plugin);
	gnc_ui_tax_table_window_new (gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_billing_terms (GtkAction *action,
				       GncMainWindowActionData *mw)
{
	GncPluginBusiness *plugin;
	GncPluginBusinessPrivate *priv;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	plugin = GNC_PLUGIN_BUSINESS (mw->data);
	priv = GNC_PLUGIN_BUSINESS_GET_PRIVATE (plugin);
	gnc_ui_billterms_window_new (gnc_get_current_book());
}


static void
gnc_plugin_business_cmd_bills_due_reminder (GtkAction *action,
					    GncMainWindowActionData *mw)
{
	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	gnc_invoice_remind_bills_due();
}

/**************************************************************
 * QSF export routines
 **************************************************************/

static void
gnc_plugin_business_cmd_export_invoice (GtkAction *action, GncMainWindowActionData *mw)
{
	QofSession *current_session, *chart_session;
	QofBook *book;
	QofCollection *coll;
	gchar *filename;
	gboolean success;

	current_session = gnc_get_current_session();
	book = qof_session_get_book(current_session);
	chart_session = qof_session_new();
	success = FALSE;
	filename = gnc_file_dialog(_("Export Invoices to XML"), NULL, 
			NULL, GNC_FILE_DIALOG_EXPORT);
	if (filename)
	{
	    gchar* url = g_strdup_printf( "qsf:%s", filename );
		qof_session_begin(chart_session, url, TRUE, TRUE);
		coll = qof_book_get_collection(book, GNC_ID_INVOICE);
		success = qof_instance_copy_coll_r(chart_session, coll);
		/* Need to get the GList of GncEntry's - KVP */
		coll = qof_book_get_collection(book, GNC_ID_CUSTOMER);
		success = qof_instance_copy_coll_r(chart_session, coll);
		if(success) 
		{ 
			qof_session_save(chart_session, NULL);
		}
		g_free(url);
	}
	show_session_error(qof_session_get_error(chart_session), filename,
			   GNC_FILE_DIALOG_EXPORT);
	g_free(filename);
	qof_session_end(chart_session);
	gnc_set_current_session(current_session);
}

static void
gnc_plugin_business_cmd_export_customer (GtkAction *action, GncMainWindowActionData *mw)
{
	QofSession *current_session, *chart_session;
	QofBook *book;
	QofCollection *coll;
	gchar *filename;
	gboolean success;

	current_session = gnc_get_current_session();
	book = qof_session_get_book(current_session);
	chart_session = qof_session_new();
	success = FALSE;
	filename = gnc_file_dialog(_("Export Customers to XML"), NULL, 
			NULL, GNC_FILE_DIALOG_EXPORT);
	if (filename)
	{
	    gchar* url = g_strdup_printf( "qsf:%s", filename );
		qof_session_begin(chart_session, url, TRUE, TRUE);
		coll = qof_book_get_collection(book, GNC_ID_CUSTOMER);
		success = qof_instance_copy_coll_r(chart_session, coll);
		if(success) 
		{ 
			qof_session_save(chart_session, NULL);
		}
		g_free(url);
	}
	show_session_error(qof_session_get_error(chart_session), filename,
			   GNC_FILE_DIALOG_EXPORT);
	qof_session_end(chart_session);
	g_free(filename);
	gnc_set_current_session(current_session);
}

static void
gnc_plugin_business_cmd_export_vendor (GtkAction *action, GncMainWindowActionData *mw)
{
	QofSession *current_session, *chart_session;
	QofBook *book;
	QofCollection *coll;
	gchar *filename;
	gboolean success;

	current_session = gnc_get_current_session();
	book = qof_session_get_book(current_session);
	chart_session = qof_session_new();
	success = FALSE;
	filename = gnc_file_dialog(_("Export Vendors to XML"), NULL, 
			NULL, GNC_FILE_DIALOG_EXPORT);
	if (filename)
	{
	    gchar* url = g_strdup_printf( "qsf:%s", filename );
		qof_session_begin(chart_session, url, TRUE, TRUE);
		coll = qof_book_get_collection(book, GNC_ID_VENDOR);
		success = qof_instance_copy_coll_r(chart_session, coll);
		if(success) 
		{ 
			qof_session_save(chart_session, NULL);
		}
		g_free(url);
	}
	show_session_error(qof_session_get_error(chart_session), filename,
			   GNC_FILE_DIALOG_EXPORT);
	qof_session_end(chart_session);
	g_free(filename);
	gnc_set_current_session(current_session);
}

static void
gnc_plugin_business_cmd_export_employee (GtkAction *action, GncMainWindowActionData *mw)
{
	QofSession *current_session, *chart_session;
	QofBook *book;
	QofCollection *coll;
	gchar *filename;
	gboolean success;

	current_session = gnc_get_current_session();
	book = qof_session_get_book(current_session);
	chart_session = qof_session_new();
	success = FALSE;
	filename = gnc_file_dialog(_("Export Employees to XML"), NULL, 
			NULL, GNC_FILE_DIALOG_EXPORT);
	if (filename)
	{
	    gchar* url = g_strdup_printf( "qsf:%s", filename );
		qof_session_begin(chart_session, url, TRUE, TRUE);
		coll = qof_book_get_collection(book, GNC_ID_EMPLOYEE);
		success = qof_instance_copy_coll_r(chart_session, coll);
		if(success) 
		{ 
			qof_session_save(chart_session, NULL);
		}
		g_free(url);
	}
	show_session_error(qof_session_get_error(chart_session), filename,
			   GNC_FILE_DIALOG_EXPORT);
	qof_session_end(chart_session);
	g_free(filename);
	gnc_set_current_session(current_session);
}

static void
gnc_plugin_business_cmd_test_search (GtkAction *action,
				     GncMainWindowActionData *data)
{
	gnc_search_dialog_test();
}

static void
gnc_plugin_business_reload_module (const gchar *name)
{
	SCM file_scm;

	file_scm = scm_makfrom0str (name);
	scm_call_1(scm_c_eval_string("gnc:reload-module"), file_scm);
}

static void
gnc_plugin_business_cmd_test_reload_invoice_report (GtkAction *action,
						    GncMainWindowActionData *data)
{
	gnc_plugin_business_reload_module("gnucash/report/invoice.scm");
}

static void
gnc_plugin_business_cmd_test_reload_owner_report (GtkAction *action,
						  GncMainWindowActionData *data)
{
	gnc_plugin_business_reload_module("gnucash/report/owner-report.scm");
}

static void
gnc_plugin_business_cmd_test_reload_receivable_report (GtkAction *action,
						       GncMainWindowActionData *data)
{
	gnc_plugin_business_reload_module("gnucash/report/receivable-report.scm");
}

static void
gnc_plugin_business_cmd_test_init_data (GtkAction *action,
					GncMainWindowActionData *data)
{
	QofBook *book		= gnc_get_current_book();
	GncCustomer *customer	= gncCustomerCreate(book);
	GncAddress *address	= gncCustomerGetAddr(customer);
	GncInvoice *invoice	= gncInvoiceCreate(book);
	GncOwner *owner		= gncOwnerCreate();
	GncJob *job		= gncJobCreate(book);
	Account *root		= gnc_book_get_root_account(book);
	Account *inc_acct	= xaccMallocAccount(book);
	Account *bank_acct	= xaccMallocAccount(book);
	Account *tax_acct	= xaccMallocAccount(book);
	Account *ar_acct	= xaccMallocAccount(book);
	Timespec now;

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
	timespecFromTime_t(&now, time(NULL));
	gncInvoiceSetID(invoice, "000012");
	gncInvoiceSetOwner(invoice, owner);
	gncInvoiceSetDateOpened(invoice, now);
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
	gnc_ui_invoice_edit(invoice);
}
