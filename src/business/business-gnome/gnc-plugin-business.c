/* 
 * gnc-plugin-business.c -- 
 *
 * Copyright (C) 2003 Jan Arne Petersen
 * Author: Jan Arne Petersen <jpetersen@uni-bonn.de>
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

#include "config.h"

#include <string.h>
#include <gtk/gtk.h>

#include "dialog-billterms.h"
#include "dialog-customer.h"
#include "dialog-employee.h"
#include "dialog-invoice.h"
#include "dialog-job.h"
#include "dialog-payment.h"
#include "dialog-tax-table.h"
#include "dialog-vendor.h"
#include "gnc-plugin-business.h"
#include "gncOwner.h"
#include "messages.h"
#include "gnc-ui-util.h"
#include "gnc-file-dialog.h"
#include "gnc-file.h"

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

/*static void gnc_plugin_business_cmd_export_report   (GtkAction *action,
						      GncMainWindowActionData *data);*/

#define PLUGIN_ACTIONS_NAME "gnc-plugin-business-actions"
#define PLUGIN_UI_FILENAME  "gnc-plugin-business-ui.xml"

static GtkActionEntry gnc_plugin_actions [] = 
{
	/* Toplevel */
	{ "BusinessAction", NULL, N_("_Business"), NULL, NULL, NULL },

	/* Customer submenu */
	{ "CustomerMenuAction", NULL, N_("_Customer"), NULL, NULL, NULL },
	{ "CustomerNewCustomerOpenAction", NULL, N_("New Customer"), NULL,
	  N_("Open the New Customer dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_customer_new_customer) },
	{ "CustomerFindCustomerOpenAction", NULL, N_("Find Customer"), NULL,
	  N_("Open the Find Customer dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_customer_find_customer) },
	{ "CustomerNewInvoiceOpenAction", NULL, N_("New Invoice"), NULL,
	  N_("Open the New Invoice dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_customer_new_invoice) },
	{ "CustomerFindInvoiceOpenAction", NULL, N_("Find Invoice"), NULL,
	  N_("Open the Find Invoice dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_customer_find_invoice) },
	{ "CustomerNewJobOpenAction", NULL, N_("New Job"), NULL,
	  N_("Open the New Job dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_customer_new_job) },
	{ "CustomerFindJobOpenAction", NULL, N_("Find Job"), NULL,
	  N_("Open the Find Job dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_customer_find_job) },
	{ "CustomerProcessPaymentAction", NULL, N_("Process Payment"), NULL,
	  N_("Open the Process Payment dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_customer_process_payment) },

	/* Vendor submenu */
	{ "VendorMenuAction", NULL, N_("_Vendor"), NULL, NULL, NULL },
	{ "VendorNewVendorOpenAction", NULL, N_("New Vendor"), NULL,
	  N_("Open the New Vendor dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_vendor_new_vendor) },
	{ "VendorFindVendorOpenAction", NULL, N_("Find Vendor"), NULL,
	  N_("Open the Find Vendor dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_vendor_find_vendor) },
	{ "VendorNewBillOpenAction", NULL, N_("New Bill"), NULL,
	  N_("Open the New Bill dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_vendor_new_bill) },
	{ "VendorFindBillOpenAction", NULL, N_("Find Bill"), NULL,
	  N_("Open the Find Bill dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_vendor_find_bill) },
	{ "VendorNewJobOpenAction", NULL, N_("New Job"), NULL,
	  N_("Open the New Job dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_vendor_new_job) },
	{ "VendorFindJobOpenAction", NULL, N_("Find Job"), NULL,
	  N_("Open the Find Job dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_vendor_find_job) },
	{ "VendorProcessPaymentAction", NULL, N_("Process Payment"), NULL,
	  N_("Open the Process Payment dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_vendor_process_payment) },

	/* Employee submenu */
	{ "EmployeeMenuAction", NULL, N_("_Employee"), NULL, NULL, NULL },
	{ "EmployeeNewEmployeeOpenAction", NULL, N_("New Employee"), NULL,
	  N_("Open the New Employee dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_employee_new_employee) },
	{ "EmployeeFindEmployeeOpenAction", NULL, N_("Find Employee"), NULL,
	  N_("Open the Find Employee dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_employee_find_employee) },
	{ "EmployeeNewExpenseVoucherOpenAction", NULL, N_("New Expense Voucher"), NULL,
	  N_("Open the New Expense Voucher dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_employee_new_expense_voucher) },
	{ "EmployeeFindExpenseVoucherOpenAction", NULL, N_("Find Expense Voucher"), NULL,
	  N_("Open the Find Expense Voucher dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_employee_find_expense_voucher) },
	{ "EmployeeProcessPaymentAction", NULL, N_("Process Payment"), NULL,
	  N_("Open the Process Payment dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_employee_process_payment) },

	/* Other menu items */
	{ "TaxTablesOpenAction", NULL, N_("Tax Tables"), NULL,
	  N_("Open the Tax Tables dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_tax_tables) },
	{ "BillingTermsOpenAction", NULL, N_("Billing Terms"), NULL,
	  N_("Open the Billing Terms dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_billing_terms) },
	{ "BillsDueReminderOpenAction", NULL, N_("Bills Due Reminder"), NULL,
	  N_("Open the Bills Due Reminder dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_bills_due_reminder) },
	{ "ExportMenuAction", NULL, N_("E_xport"), NULL, NULL, NULL },
	{ "QSFInvoiceAction", NULL, N_("QSF _Invoice"), NULL,
	  N_("Export one or more invoices to QSF"),
	  G_CALLBACK (gnc_plugin_business_cmd_export_invoice) },
	{ "QSFCustomerAction", NULL, N_("QSF _Customer"), NULL,
	  N_("Export one or more customers to QSF"),
	  G_CALLBACK (gnc_plugin_business_cmd_export_customer) },
	{ "QSFVendorAction", NULL, N_("QSF _Vendor"), NULL,
	  N_("Export one or more vendors to QSF"),
	  G_CALLBACK (gnc_plugin_business_cmd_export_vendor) },
	{ "QSFEmployeeAction", NULL, N_("QSF _Employee"), NULL,
	  N_("Export one or more employees to QSF"),
	  G_CALLBACK (gnc_plugin_business_cmd_export_employee) },
};
static guint gnc_plugin_n_actions = G_N_ELEMENTS (gnc_plugin_actions);


struct GncPluginBusinessPrivate
{
	GncOwner *last_customer;
	GncOwner *last_vendor;
	GncOwner *last_employee;
};

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

	plugin = g_object_new (GNC_TYPE_PLUGIN_BUSINESS,
			      NULL);

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
}

static void
gnc_plugin_business_init (GncPluginBusiness *plugin)
{
	GncPluginBusinessPrivate *priv;

	priv = g_new0 (GncPluginBusinessPrivate, 1);
	plugin->priv = priv;

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

	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (object));

	plugin = GNC_PLUGIN_BUSINESS (object);
	g_return_if_fail (plugin->priv != NULL);

	g_free (plugin->priv);

	G_OBJECT_CLASS (parent_class)->finalize (object);
}

/************************************************************
 *              Plugin Function Implementation              *
 ************************************************************/

/************************************************************
 *                    Command Callbacks                     *
 ************************************************************/

static void
gnc_plugin_business_cmd_customer_new_customer (GtkAction *action,
					       GncMainWindowActionData *mw)
{
	GncPluginBusinessPrivate *priv;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	priv = GNC_PLUGIN_BUSINESS (mw->data)->priv;
	gnc_ui_customer_new (gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_customer_find_customer (GtkAction *action,
						GncMainWindowActionData *mw)
{
	GncPluginBusinessPrivate *priv;
	GncCustomer*customer;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	priv = GNC_PLUGIN_BUSINESS (mw->data)->priv;
	customer = gncOwnerGetCustomer (priv->last_customer);
	gnc_customer_search (customer, gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_customer_new_invoice (GtkAction *action,
					      GncMainWindowActionData *mw)
{
	GncPluginBusinessPrivate *priv;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	priv = GNC_PLUGIN_BUSINESS (mw->data)->priv;
	gnc_ui_invoice_new (priv->last_customer, gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_customer_find_invoice (GtkAction *action,
					       GncMainWindowActionData *mw)
{
	GncPluginBusinessPrivate *priv;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	priv = GNC_PLUGIN_BUSINESS (mw->data)->priv;
	gnc_invoice_search (NULL, priv->last_customer, gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_customer_new_job (GtkAction *action,
					  GncMainWindowActionData *mw)
{
	GncPluginBusinessPrivate *priv;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	priv = GNC_PLUGIN_BUSINESS (mw->data)->priv;
	gnc_ui_job_new (priv->last_customer, gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_customer_find_job (GtkAction *action,
					   GncMainWindowActionData *mw)
{
	GncPluginBusinessPrivate *priv;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	priv = GNC_PLUGIN_BUSINESS (mw->data)->priv;
	gnc_job_search (NULL, priv->last_customer, gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_customer_process_payment (GtkAction *action,
						  GncMainWindowActionData *mw)
{
	GncPluginBusinessPrivate *priv;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	priv = GNC_PLUGIN_BUSINESS (mw->data)->priv;
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
	GncPluginBusinessPrivate *priv;
	GncVendor *vendor;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	priv = GNC_PLUGIN_BUSINESS (mw->data)->priv;
	vendor = gncOwnerGetVendor (priv->last_vendor);
	gnc_vendor_search (vendor, gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_vendor_new_bill (GtkAction *action,
					 GncMainWindowActionData *mw)
{
	GncPluginBusinessPrivate *priv;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	priv = GNC_PLUGIN_BUSINESS (mw->data)->priv;
	gnc_ui_invoice_new (priv->last_vendor, gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_vendor_find_bill (GtkAction *action,
					  GncMainWindowActionData *mw)
{
	GncPluginBusinessPrivate *priv;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	priv = GNC_PLUGIN_BUSINESS (mw->data)->priv;
	gnc_invoice_search (NULL, priv->last_vendor, gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_vendor_new_job (GtkAction *action,
					GncMainWindowActionData *mw)
{
	GncPluginBusinessPrivate *priv;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	priv = GNC_PLUGIN_BUSINESS (mw->data)->priv;
	gnc_ui_job_new (priv->last_vendor, gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_vendor_find_job (GtkAction *action,
					 GncMainWindowActionData *mw)
{
	GncPluginBusinessPrivate *priv;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	priv = GNC_PLUGIN_BUSINESS (mw->data)->priv;
	gnc_job_search (NULL, priv->last_vendor, gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_vendor_process_payment (GtkAction *action,
						GncMainWindowActionData *mw)
{
	GncPluginBusinessPrivate *priv;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	priv = GNC_PLUGIN_BUSINESS (mw->data)->priv;
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
	GncPluginBusinessPrivate *priv;
	GncEmployee *employee;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	priv = GNC_PLUGIN_BUSINESS (mw->data)->priv;
	employee = gncOwnerGetEmployee (priv->last_employee);
	gnc_employee_search (employee, gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_employee_new_expense_voucher (GtkAction *action,
						      GncMainWindowActionData *mw)
{
	GncPluginBusinessPrivate *priv;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	priv = GNC_PLUGIN_BUSINESS (mw->data)->priv;
	gnc_ui_invoice_new (priv->last_employee, gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_employee_find_expense_voucher (GtkAction *action,
						       GncMainWindowActionData *mw)
{
	GncPluginBusinessPrivate *priv;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	priv = GNC_PLUGIN_BUSINESS (mw->data)->priv;
	gnc_invoice_search (NULL, priv->last_employee, gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_employee_process_payment (GtkAction *action,
						  GncMainWindowActionData *mw)
{
	GncPluginBusinessPrivate *priv;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	priv = GNC_PLUGIN_BUSINESS (mw->data)->priv;
	gnc_ui_payment_new (priv->last_employee, gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_tax_tables (GtkAction *action,
				    GncMainWindowActionData *mw)
{
	GncPluginBusinessPrivate *priv;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	priv = GNC_PLUGIN_BUSINESS (mw->data)->priv;
	gnc_ui_tax_table_window_new (gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_billing_terms (GtkAction *action,
				       GncMainWindowActionData *mw)
{
	GncPluginBusinessPrivate *priv;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	priv = GNC_PLUGIN_BUSINESS (mw->data)->priv;
	gnc_ui_billterms_window_new (gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_bills_due_reminder (GtkAction *action,
					    GncMainWindowActionData *mw)
{
#if 0
	GncPluginBusinessPrivate *priv;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	priv = GNC_PLUGIN_BUSINESS (mw->data)->priv;
	gnc_remind_bills_due gnc_ui_payment_new (priv->last_employee, gnc_get_current_book());
#endif
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

	current_session = qof_session_get_current_session();
	book = qof_session_get_book(current_session);
	chart_session = qof_session_new();
	success = FALSE;
	filename = gnc_file_dialog(_("Export Invoices to XML"), NULL, "/tmp/qsf-invoices.xml");
	if (filename)
	{
		qof_session_begin(chart_session, filename, TRUE, TRUE);
		coll = qof_book_get_collection(book, GNC_ID_INVOICE);
		success = qof_entity_copy_coll(chart_session, coll);
		if(success) 
		{ 
			qof_session_save(chart_session, NULL);
		}
	}
	show_session_error(qof_session_get_error(chart_session), filename);
	g_free(filename);
	qof_session_end(chart_session);
	qof_session_set_current_session(current_session);
}

static void
gnc_plugin_business_cmd_export_customer (GtkAction *action, GncMainWindowActionData *mw)
{
	QofSession *current_session, *chart_session;
	QofBook *book;
	QofCollection *coll;
	gchar *filename;
	gboolean success;

	current_session = qof_session_get_current_session();
	book = qof_session_get_book(current_session);
	chart_session = qof_session_new();
	success = FALSE;
	filename = gnc_file_dialog(_("Export Customers to XML"), NULL, "/tmp/qsf-customers.xml");
	if (filename)
	{
		qof_session_begin(chart_session, filename, TRUE, TRUE);
		coll = qof_book_get_collection(book, GNC_ID_CUSTOMER);
		success = qof_entity_copy_coll(chart_session, coll);
		if(success) 
		{ 
			qof_session_save(chart_session, NULL);
		}
	}
	show_session_error(qof_session_get_error(chart_session), filename);
	qof_session_end(chart_session);
	g_free(filename);
	qof_session_set_current_session(current_session);
}

static void
gnc_plugin_business_cmd_export_vendor (GtkAction *action, GncMainWindowActionData *mw)
{
	QofSession *current_session, *chart_session;
	QofBook *book;
	QofCollection *coll;
	gchar *filename;
	gboolean success;

	current_session = qof_session_get_current_session();
	book = qof_session_get_book(current_session);
	chart_session = qof_session_new();
	success = FALSE;
	filename = gnc_file_dialog(_("Export Vendors to XML"), NULL, "/tmp/qsf-vendors.xml");
	if (filename)
	{
		qof_session_begin(chart_session, filename, TRUE, TRUE);
		coll = qof_book_get_collection(book, GNC_ID_VENDOR);
		success = qof_entity_copy_coll(chart_session, coll);
		if(success) 
		{ 
			qof_session_save(chart_session, NULL);
		}
	}
	show_session_error(qof_session_get_error(chart_session), filename);
	qof_session_end(chart_session);
	g_free(filename);
	qof_session_set_current_session(current_session);
}

static void
gnc_plugin_business_cmd_export_employee (GtkAction *action, GncMainWindowActionData *mw)
{
	QofSession *current_session, *chart_session;
	QofBook *book;
	QofCollection *coll;
	gchar *filename;
	gboolean success;

	current_session = qof_session_get_current_session();
	book = qof_session_get_book(current_session);
	chart_session = qof_session_new();
	success = FALSE;
	filename = gnc_file_dialog(_("Export Employees to XML"), NULL, "/tmp/qsf-employee.xml");
	if (filename)
	{
		qof_session_begin(chart_session, filename, TRUE, TRUE);
		coll = qof_book_get_collection(book, GNC_ID_EMPLOYEE);
		success = qof_entity_copy_coll(chart_session, coll);
		if(success) 
		{ 
			qof_session_save(chart_session, NULL);
		}
	}
	show_session_error(qof_session_get_error(chart_session), filename);
	qof_session_end(chart_session);
	g_free(filename);
	qof_session_set_current_session(current_session);
}
