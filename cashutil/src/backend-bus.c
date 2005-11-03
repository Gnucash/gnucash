/***************************************************************************
 *            backend-bus.c
 *
 *  Sun Sep 25 15:59:50 2005
 *  Copyright  2005  Neil Williams
 *  linux@codehelp.co.uk
 ****************************************************************************/

/*
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */
 
#include <libxml/xmlmemory.h>
#include <libxml/tree.h>
#include <libxml/parser.h>
#include <libxml/xmlschemas.h>

#include "gnc-address-xml-v2.h"
#include "gnc-bill-term-xml-v2.h"
#include "gnc-customer-xml-v2.h"
#include "gnc-employee-xml-v2.h"
#include "gnc-entry-xml-v2.h"
#include "gnc-invoice-xml-v2.h"
#include "gnc-job-xml-v2.h"
#include "gnc-order-xml-v2.h"
#include "gnc-owner-xml-v2.h"
#include "gnc-tax-table-xml-v2.h"
#include "gnc-vendor-xml-v2.h"

void
backend_business_add (void)
{
	gnc_address_xml_initialize ();
	gnc_billterm_xml_initialize ();
	gnc_customer_xml_initialize ();
	gnc_employee_xml_initialize ();
	gnc_entry_xml_initialize ();
	gnc_invoice_xml_initialize ();
	gnc_job_xml_initialize ();
	gnc_order_xml_initialize ();
	gnc_owner_xml_initialize ();
	gnc_taxtable_xml_initialize ();
	gnc_vendor_xml_initialize ();
}


gboolean bus_cashobjects_register(void)
{
	g_return_val_if_fail(gncInvoiceRegister(), FALSE);
	g_return_val_if_fail ( gncJobRegister (),  FALSE);
	g_return_val_if_fail(gncBillTermRegister(), FALSE);
	g_return_val_if_fail(gncCustomerRegister(), FALSE);
	g_return_val_if_fail(gncAddressRegister(), FALSE);
	g_return_val_if_fail(gncEmployeeRegister(), FALSE);
	g_return_val_if_fail ( gncEntryRegister (), FALSE);
	g_return_val_if_fail (gncVendorRegister (), FALSE);
	g_return_val_if_fail(gncTaxTableRegister(), FALSE);
	g_return_val_if_fail ( gncOrderRegister (), FALSE);
	return TRUE;
}
