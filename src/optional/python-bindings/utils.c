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

#include "utils.h"


/***********************************************************************
 * Search the book for a Customer with the same ID.  If it exists return a
 * Customer object, if nit then return NULL.
 @param QofBook	The book
 @param gchar ID of the Customer
 @return GncCustomer * Pointer to the customer or NULL of there is no customer
 *
 **********************************************************************/
GncCustomer *
search_customer_on_id (QofBook * book, const gchar *id)
{
	QueryNew *q;
	GNCIdType type = GNC_CUSTOMER_MODULE_NAME;
	GList *result; // do not free this!
	QueryPredData_t string_pred_data;
	GncCustomer *customer = NULL;
	gint len;

	g_return_val_if_fail (id, NULL);
	g_return_val_if_fail (book, NULL);

	// Build the query
	q = gncQueryCreateFor (type);
	gncQuerySetBook (q, book);

	// Search only the customer id field
	string_pred_data = gncQueryStringPredicate (COMPARE_EQUAL, id, STRING_MATCH_NORMAL, FALSE);
	gncQueryAddTerm (q, gncQueryBuildParamList(CUSTOMER_ID), string_pred_data, QUERY_AND);

	// Run the query
	result = gncQueryRun (q);

	// now compare _exactly_
	len = g_list_length (result);
	if (result && (len>0)) {
		result = g_list_first (result);
		while (result) {
			GncCustomer *c = result->data;
			if (strcmp(id,gncCustomerGetID(c)) == 0) {
				// correct id found
				customer = c;
				break;
			}
			result = g_list_next (result);
		}
	}

	gncQueryDestroy (q);
	return customer;
}

/***********************************************************************
 * Search the book for an Invoice with the same ID.  If it exists return an
 * Invoice object, if not then return NULL.
 @param QofBook	The book
 @param gchar ID of the invoice
 @return GncCustomer * Pointer to the Invoice or NULL of there is no customer
 *
 **********************************************************************/
GncInvoice *
search_invoice_on_id(QofBook *book, const gchar *id)
{
	QueryNew *q;
	GNCIdType type = GNC_INVOICE_MODULE_NAME;
	GList *result; // do not free this!
	QueryPredData_t string_pred_data;
	GncInvoice *invoice = NULL;
	gint len;

	g_return_val_if_fail (id, NULL);
	g_return_val_if_fail (book, NULL);

	// Build the query
	q = gncQueryCreateFor (type);
	gncQuerySetBook (q, book);

	// Search only the invoice id field
	string_pred_data = gncQueryStringPredicate (COMPARE_EQUAL, id, STRING_MATCH_NORMAL, FALSE);
	gncQueryAddTerm (q, gncQueryBuildParamList(INVOICE_ID), string_pred_data, QUERY_AND);

	// Run the query
	result = gncQueryRun (q);

	// now compare _exactly_
	len = g_list_length (result);
	if (result && (len>0)) {
		result = g_list_first (result);
		while (result) {
			GncInvoice *c = result->data;
			if (strcmp(id,gncInvoiceGetID(c)) == 0) {
				// correct id found
				invoice = c;
				break;
			}
			result = g_list_next (result);
		}
	}

	gncQueryDestroy (q);
	return invoice;
}


/***********************************************************************
 * Search the book for a Bill with the same ID.  If it exists return an
 * Invoice object, if not then return NULL.
 @param QofBook	The book
 @param gchar ID of the invoice
 @return GncCustomer * Pointer to the Invoice or NULL of there is no customer
 *
 **********************************************************************/
GncInvoice *
search_bill_on_id(QofBook *book, const gchar *id)
{
	QueryNew *q;
	GNCIdType type = GNC_INVOICE_MODULE_NAME;
	GList *result; // do not free this!
	QueryPredData_t string_pred_data;
	GncInvoice *bill = NULL;
	gint len;

	g_return_val_if_fail (id, NULL);
	g_return_val_if_fail (book, NULL);

	// Build the query
	q = gncQueryCreateFor (type);
	gncQuerySetBook (q, book);

	// Search only the invoice id field
	string_pred_data = gncQueryStringPredicate (COMPARE_EQUAL, id, STRING_MATCH_NORMAL, FALSE);
	gncQueryAddTerm (q, gncQueryBuildParamList(INVOICE_ID), string_pred_data, QUERY_AND);

	// Run the query
	result = gncQueryRun (q);

	// now compare _exactly_
	len = g_list_length (result);
	if (result && (len>0)) {
		result = g_list_first (result);
		while (result) {
			GncInvoice *c = result->data;
			if (strcmp(id,gncInvoiceGetID(c)) == 0) {
				// correct id found
				bill = c;
				break;
			}
			result = g_list_next (result);
		}
	}

	gncQueryDestroy (q);
	return bill;
}
