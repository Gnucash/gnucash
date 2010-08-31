/**      utils.c
*
*      This program is free software; you can redistribute it and/or modify
*      it under the terms of the GNU General Public License as published by
*      the Free Software Foundation; either version 2 of the License, or
*      (at your option) any later version.
*
*      This program is distributed in the hope that it will be useful,
*      but WITHOUT ANY WARRANTY; without even the implied warranty of
*      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*      GNU General Public License for more details.
*
*      You should have received a copy of the GNU General Public License
*      along with this program; if not, write to the Free Software
*      Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*      MA 02110-1301, USA.
*
* Developed (aka copied?) from code written by Sebastian Held <sebastian.held@gmx.de>
* as part of his GnuCash invoice importer module
* Mike Evans <mikee@saxicola.co.uk>
*
**********************************************************************/

#include "gncIDSearch.h"

static void * search(QofBook * book, const gchar *id, void * object, GNCIdType type);
/***********************************************************************
 * Search the book for a Customer/Invoice/Bill with the same ID.
 * If it exists return a valid object, if not then returns NULL.
 @param QofBook	The book
 @param gchar ID of the Customer
 @return GncCustomer * Pointer to the customer or NULL of there is no customer
 **********************************************************************/
GncCustomer *
gnc_search_customer_on_id (QofBook * book, const gchar *id)
{
	GncCustomer *customer = NULL;
	GNCIdType type = GNC_CUSTOMER_MODULE_NAME;
	customer = (GncCustomer*)search(book, id, customer, type);
	return customer;
}

GncInvoice *
gnc_search_invoice_on_id (QofBook * book, const gchar *id)
{
	GncInvoice *invoice = NULL;
	GNCIdType type = GNC_INVOICE_MODULE_NAME;
	invoice = (GncInvoice*)search(book,id, invoice, type);
	return invoice;
		}

/* Essentially identical to above.*/
GncInvoice *
gnc_search_bill_on_id (QofBook * book, const gchar *id)
{
	GncInvoice *bill =  NULL;
	GNCIdType type = GNC_INVOICE_MODULE_NAME;
	bill = (GncInvoice*)search(book, id, bill, type);
	return bill;
	}

GncVendor *
gnc_search_vendor_on_id (QofBook * book, const gchar *id)
{
	GncVendor *vendor =  NULL;
	GNCIdType type = GNC_VENDOR_MODULE_NAME;
	vendor = (GncVendor*)search(book, id, vendor, type);
	return vendor;
}


/******************************************************************
 * Generic search called after setting up stuff
 * DO NOT call directly but type tests should fail anyway
 ****************************************************************/
static void * search(QofBook * book, const gchar *id, void * object, GNCIdType type)
{
	void *c;
	GList *result;
	QueryNew *q;
	gint len;
	QueryPredData_t string_pred_data;
	g_return_val_if_fail (type, NULL);
	g_return_val_if_fail (id, NULL);
	g_return_val_if_fail (book, NULL);

	// Build the query
	q = gncQueryCreateFor (type);
	gncQuerySetBook (q, book);

	// Search only the id field
	string_pred_data = gncQueryStringPredicate (COMPARE_EQUAL, id, STRING_MATCH_NORMAL, FALSE);

	if (strcmp(type,GNC_CUSTOMER_MODULE_NAME))
	{
		GncCustomer *c = NULL;
		gncQueryAddTerm (q, gncQueryBuildParamList(CUSTOMER_ID), string_pred_data, QUERY_AND);
	}
	else if (strcmp(type,GNC_INVOICE_MODULE_NAME))
	{
		GncInvoice *c = NULL;
	gncQueryAddTerm (q, gncQueryBuildParamList(INVOICE_ID), string_pred_data, QUERY_AND);
	}
	else if (strcmp(type,GNC_VENDOR_MODULE_NAME))
	{
		GncVendor *c = NULL;
		gncQueryAddTerm (q, gncQueryBuildParamList(VENDOR_ID), string_pred_data, QUERY_AND);
	}

	// Run the query
	result = gncQueryRun (q);

	// now compare _exactly_
	len = g_list_length (result);
	if (result && (len>0)) {
		result = g_list_first (result);
		while (result) {
			c = result->data;
			if (strcmp(id,gncCustomerGetID(c)) == 0) {
				// correct id found
				object = c;
				break;
			}
			result = g_list_next (result);
		}
	}
	gncQueryDestroy (q);
	return object;
}
