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


/* g_object functions */
static void gnc_plugin_business_class_init (GncPluginBusinessClass *klass);
static void gnc_plugin_business_init (GncPluginBusiness *plugin);
static void gnc_plugin_business_finalize (GObject *object);

/* Command callbacks */
static void gnc_plugin_business_cmd_customer_new_customer    (EggAction *action,
							      GncMainWindowActionData *data);
static void gnc_plugin_business_cmd_customer_find_customer   (EggAction *action,
							      GncMainWindowActionData *data);
static void gnc_plugin_business_cmd_customer_new_invoice     (EggAction *action,
							      GncMainWindowActionData *data);
static void gnc_plugin_business_cmd_customer_find_invoice    (EggAction *action,
							      GncMainWindowActionData *data);
static void gnc_plugin_business_cmd_customer_new_job         (EggAction *action,
							      GncMainWindowActionData *data);
static void gnc_plugin_business_cmd_customer_find_job        (EggAction *action,
							      GncMainWindowActionData *data);
static void gnc_plugin_business_cmd_customer_process_payment (EggAction *action,
							      GncMainWindowActionData *data);

static void gnc_plugin_business_cmd_vendor_new_vendor      (EggAction *action,
							    GncMainWindowActionData *data);
static void gnc_plugin_business_cmd_vendor_find_vendor     (EggAction *action,
							    GncMainWindowActionData *data);
static void gnc_plugin_business_cmd_vendor_new_bill        (EggAction *action,
							    GncMainWindowActionData *data);
static void gnc_plugin_business_cmd_vendor_find_bill       (EggAction *action,
							    GncMainWindowActionData *data);
static void gnc_plugin_business_cmd_vendor_new_job         (EggAction *action,
							    GncMainWindowActionData *data);
static void gnc_plugin_business_cmd_vendor_find_job        (EggAction *action,
							    GncMainWindowActionData *data);
static void gnc_plugin_business_cmd_vendor_process_payment (EggAction *action,
							    GncMainWindowActionData *data);

static void gnc_plugin_business_cmd_employee_new_employee         (EggAction *action,
								   GncMainWindowActionData *data);
static void gnc_plugin_business_cmd_employee_find_employee        (EggAction *action,
								   GncMainWindowActionData *data);
static void gnc_plugin_business_cmd_employee_new_expense_voucher  (EggAction *action,
								   GncMainWindowActionData *data);
static void gnc_plugin_business_cmd_employee_find_expense_voucher (EggAction *action,
								   GncMainWindowActionData *data);
static void gnc_plugin_business_cmd_employee_process_payment      (EggAction *action,
								   GncMainWindowActionData *data);

static void gnc_plugin_business_cmd_tax_tables         (EggAction *action,
							GncMainWindowActionData *data);
static void gnc_plugin_business_cmd_billing_terms      (EggAction *action,
							GncMainWindowActionData *data);
static void gnc_plugin_business_cmd_bills_due_reminder (EggAction *action,
							GncMainWindowActionData *data);


#define PLUGIN_ACTIONS_NAME "gnc-plugin-business-actions"
#define PLUGIN_UI_FILENAME  "gnc-plugin-business-ui.xml"

static EggActionEntry gnc_plugin_actions [] = 
{
	/* Toplevel */
	{ "BusinessAction", N_("_Business"), NULL, NULL, NULL, NULL },

	/* Customer submenu */
	{ "CustomerMenuAction", N_("_Customer"), NULL, NULL, NULL, NULL },
	{ "CustomerNewCustomerOpenAction", N_("New Customer"), NULL, NULL,
	  N_("Open the New Customer dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_customer_new_customer) },
	{ "CustomerFindCustomerOpenAction", N_("Find Customer"), NULL, NULL,
	  N_("Open the Find Customer dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_customer_find_customer) },
	{ "CustomerNewInvoiceOpenAction", N_("New Invoice"), NULL, NULL,
	  N_("Open the New Invoice dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_customer_new_invoice) },
	{ "CustomerFindInvoiceOpenAction", N_("Find Invoice"), NULL, NULL,
	  N_("Open the Find Invoice dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_customer_find_invoice) },
	{ "CustomerNewJobOpenAction", N_("New Job"), NULL, NULL,
	  N_("Open the New Job dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_customer_new_job) },
	{ "CustomerFindJobOpenAction", N_("Find Job"), NULL, NULL,
	  N_("Open the Find Job dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_customer_find_job) },
	{ "CustomerProcessPaymentAction", N_("Process Payment"), NULL, NULL,
	  N_("Open the Process Payment dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_customer_process_payment) },

	/* Vendor submenu */
	{ "VendorMenuAction", N_("_Vendor"), NULL, NULL, NULL, NULL },
	{ "VendorNewVendorOpenAction", N_("New Vendor"), NULL, NULL,
	  N_("Open the New Vendor dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_vendor_new_vendor) },
	{ "VendorFindVendorOpenAction", N_("Find Vendor"), NULL, NULL,
	  N_("Open the Find Vendor dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_vendor_find_vendor) },
	{ "VendorNewBillOpenAction", N_("New Bill"), NULL, NULL,
	  N_("Open the New Bill dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_vendor_new_bill) },
	{ "VendorFindBillOpenAction", N_("Find Bill"), NULL, NULL,
	  N_("Open the Find Bill dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_vendor_find_bill) },
	{ "VendorNewJobOpenAction", N_("New Job"), NULL, NULL,
	  N_("Open the New Job dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_vendor_new_job) },
	{ "VendorFindJobOpenAction", N_("Find Job"), NULL, NULL,
	  N_("Open the Find Job dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_vendor_find_job) },
	{ "VendorProcessPaymentAction", N_("Process Payment"), NULL, NULL,
	  N_("Open the Process Payment dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_vendor_process_payment) },

	/* Employee submenu */
	{ "EmployeeMenuAction", N_("_Employee"), NULL, NULL, NULL, NULL },
	{ "EmployeeNewEmployeeOpenAction", N_("New Employee"), NULL, NULL,
	  N_("Open the New Employee dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_employee_new_employee) },
	{ "EmployeeFindEmployeeOpenAction", N_("Find Employee"), NULL, NULL,
	  N_("Open the Find Employee dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_employee_find_employee) },
	{ "EmployeeNewExpenseVoucherOpenAction", N_("New Expense Voucher"), NULL, NULL,
	  N_("Open the New Expense Voucher dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_employee_new_expense_voucher) },
	{ "EmployeeFindExpenseVoucherOpenAction", N_("Find Expense Voucher"), NULL, NULL,
	  N_("Open the Find Expense Voucher dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_employee_find_expense_voucher) },
	{ "EmployeeProcessPaymentAction", N_("Process Payment"), NULL, NULL,
	  N_("Open the Process Payment dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_employee_process_payment) },

	/* Other menu items */
	{ "TaxTablesOpenAction", N_("Tax Tables"), NULL, NULL,
	  N_("Open the Tax Tables dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_tax_tables) },
	{ "BillingTermsOpenAction", N_("Billing Terms"), NULL, NULL,
	  N_("Open the Billing Terms dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_billing_terms) },
	{ "BillsDueReminderOpenAction", N_("Bills Due Reminder"), NULL, NULL,
	  N_("Open the Bills Due Reminder dialog"),
	  G_CALLBACK (gnc_plugin_business_cmd_bills_due_reminder) },
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
gnc_plugin_business_cmd_customer_new_customer (EggAction *action,
					       GncMainWindowActionData *mw)
{
	GncPluginBusinessPrivate *priv;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	priv = GNC_PLUGIN_BUSINESS (mw->data)->priv;
	gnc_ui_customer_new (gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_customer_find_customer (EggAction *action,
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
gnc_plugin_business_cmd_customer_new_invoice (EggAction *action,
					      GncMainWindowActionData *mw)
{
	GncPluginBusinessPrivate *priv;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	priv = GNC_PLUGIN_BUSINESS (mw->data)->priv;
	gnc_ui_invoice_new (priv->last_customer, gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_customer_find_invoice (EggAction *action,
					       GncMainWindowActionData *mw)
{
	GncPluginBusinessPrivate *priv;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	priv = GNC_PLUGIN_BUSINESS (mw->data)->priv;
	gnc_invoice_search (NULL, priv->last_customer, gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_customer_new_job (EggAction *action,
					  GncMainWindowActionData *mw)
{
	GncPluginBusinessPrivate *priv;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	priv = GNC_PLUGIN_BUSINESS (mw->data)->priv;
	gnc_ui_job_new (priv->last_customer, gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_customer_find_job (EggAction *action,
					   GncMainWindowActionData *mw)
{
	GncPluginBusinessPrivate *priv;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	priv = GNC_PLUGIN_BUSINESS (mw->data)->priv;
	gnc_job_search (NULL, priv->last_customer, gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_customer_process_payment (EggAction *action,
						  GncMainWindowActionData *mw)
{
	GncPluginBusinessPrivate *priv;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	priv = GNC_PLUGIN_BUSINESS (mw->data)->priv;
	gnc_ui_payment_new (priv->last_customer, gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_vendor_new_vendor (EggAction *action,
					   GncMainWindowActionData *mw)
{
	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	gnc_ui_vendor_new (gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_vendor_find_vendor (EggAction *action,
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
gnc_plugin_business_cmd_vendor_new_bill (EggAction *action,
					 GncMainWindowActionData *mw)
{
	GncPluginBusinessPrivate *priv;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	priv = GNC_PLUGIN_BUSINESS (mw->data)->priv;
	gnc_ui_invoice_new (priv->last_vendor, gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_vendor_find_bill (EggAction *action,
					  GncMainWindowActionData *mw)
{
	GncPluginBusinessPrivate *priv;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	priv = GNC_PLUGIN_BUSINESS (mw->data)->priv;
	gnc_invoice_search (NULL, priv->last_vendor, gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_vendor_new_job (EggAction *action,
					GncMainWindowActionData *mw)
{
	GncPluginBusinessPrivate *priv;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	priv = GNC_PLUGIN_BUSINESS (mw->data)->priv;
	gnc_ui_job_new (priv->last_vendor, gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_vendor_find_job (EggAction *action,
					 GncMainWindowActionData *mw)
{
	GncPluginBusinessPrivate *priv;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	priv = GNC_PLUGIN_BUSINESS (mw->data)->priv;
	gnc_job_search (NULL, priv->last_vendor, gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_vendor_process_payment (EggAction *action,
						GncMainWindowActionData *mw)
{
	GncPluginBusinessPrivate *priv;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	priv = GNC_PLUGIN_BUSINESS (mw->data)->priv;
	gnc_ui_payment_new (priv->last_vendor, gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_employee_new_employee (EggAction *action,
					       GncMainWindowActionData *mw)
{
	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	gnc_ui_employee_new (gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_employee_find_employee (EggAction *action,
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
gnc_plugin_business_cmd_employee_new_expense_voucher (EggAction *action,
						      GncMainWindowActionData *mw)
{
	GncPluginBusinessPrivate *priv;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	priv = GNC_PLUGIN_BUSINESS (mw->data)->priv;
	gnc_ui_invoice_new (priv->last_employee, gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_employee_find_expense_voucher (EggAction *action,
						       GncMainWindowActionData *mw)
{
	GncPluginBusinessPrivate *priv;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	priv = GNC_PLUGIN_BUSINESS (mw->data)->priv;
	gnc_invoice_search (NULL, priv->last_employee, gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_employee_process_payment (EggAction *action,
						  GncMainWindowActionData *mw)
{
	GncPluginBusinessPrivate *priv;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	priv = GNC_PLUGIN_BUSINESS (mw->data)->priv;
	gnc_ui_payment_new (priv->last_employee, gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_tax_tables (EggAction *action,
				    GncMainWindowActionData *mw)
{
	GncPluginBusinessPrivate *priv;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	priv = GNC_PLUGIN_BUSINESS (mw->data)->priv;
	gnc_ui_tax_table_window_new (gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_billing_terms (EggAction *action,
				       GncMainWindowActionData *mw)
{
	GncPluginBusinessPrivate *priv;

	g_return_if_fail (mw != NULL);
	g_return_if_fail (GNC_IS_PLUGIN_BUSINESS (mw->data));

	priv = GNC_PLUGIN_BUSINESS (mw->data)->priv;
	gnc_ui_billterms_window_new (gnc_get_current_book());
}

static void
gnc_plugin_business_cmd_bills_due_reminder (EggAction *action,
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
