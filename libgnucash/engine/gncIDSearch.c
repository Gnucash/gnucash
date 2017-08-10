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

typedef enum
{   UNDEFINED,
    CUSTOMER,
    VENDOR,
    INVOICE,
    BILL
}GncSearchType;

static void * search(QofBook * book, const gchar *id, void * object, GncSearchType type);
static QofLogModule log_module = G_LOG_DOMAIN;
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
    GncSearchType type = CUSTOMER;
    customer = (GncCustomer*)search(book, id, customer, type);
    return customer;
}

GncInvoice *
gnc_search_invoice_on_id (QofBook * book, const gchar *id)
{
    GncInvoice *invoice = NULL;
    GncSearchType type = INVOICE;
    invoice = (GncInvoice*)search(book, id, invoice, type);
    return invoice;
}

/* Essentially identical to above.*/
GncInvoice *
gnc_search_bill_on_id (QofBook * book, const gchar *id)
{
    GncInvoice *bill =  NULL;
    GncSearchType type = BILL;
    bill = (GncInvoice*)search(book, id, bill, type);
    return bill;
}

GncVendor *
gnc_search_vendor_on_id (QofBook * book, const gchar *id)
{
    GncVendor *vendor =  NULL;
    GncSearchType type = VENDOR;
    vendor = (GncVendor*)search(book, id, vendor, type);
    return vendor;
}


/******************************************************************
 * Generic search called after setting up stuff
 * DO NOT call directly but type tests should fail anyway
 ****************************************************************/
static void * search(QofBook * book, const gchar *id, void * object, GncSearchType type)
{
    void *c;
    GList *result;
    QofQuery *q;
    gint len;
    QofQueryPredData* string_pred_data;
    
    PINFO("Type = %d", type);
    g_return_val_if_fail (type, NULL);
    g_return_val_if_fail (id, NULL);
    g_return_val_if_fail (book, NULL);

    // Build the query
    q = qof_query_create ();
    qof_query_set_book (q, book);
    // Search only the id field
    string_pred_data = qof_query_string_predicate (QOF_COMPARE_EQUAL, id, QOF_STRING_MATCH_NORMAL, FALSE);
    if (type == CUSTOMER)
    {
        qof_query_search_for(q,GNC_CUSTOMER_MODULE_NAME);
        qof_query_add_term (q, qof_query_build_param_list("CUSTOMER_ID"), string_pred_data, QOF_QUERY_AND);
    }
    else if (type ==  INVOICE || type ==  BILL)
    {
        qof_query_search_for(q,GNC_INVOICE_MODULE_NAME);
        qof_query_add_term (q, qof_query_build_param_list("INVOICE_ID"), string_pred_data, QOF_QUERY_AND);
    }
    else if (type == VENDOR)
    {
        qof_query_search_for(q,GNC_VENDOR_MODULE_NAME);
        qof_query_add_term (q, qof_query_build_param_list("VENDOR_ID"), string_pred_data, QOF_QUERY_AND);
    }


    // Run the query
    result = qof_query_run (q);

    // now compare _exactly_
    len = g_list_length (result);
    if (result && (len > 0))
    {
        result = g_list_first (result);

        while (result)
        {
            c = result->data;
            
            if (type == CUSTOMER && strcmp(id, gncCustomerGetID(c)) == 0)
            {
                // correct id found
                object = c;
                break;
            }
            else if (type == INVOICE && strcmp(id, gncInvoiceGetID(c)) == 0 
                        && gncInvoiceGetType(c) == GNC_INVOICE_CUST_INVOICE)
            {
                object = c;
                break;
            }
            else if (type == BILL && strcmp(id, gncInvoiceGetID(c)) == 0 
                        && gncInvoiceGetType(c) == GNC_INVOICE_VEND_INVOICE)
            {
                object = c;
                break;
            }
            else if (type == VENDOR && strcmp(id, gncVendorGetID(c)) == 0)
            {
                object = c;
                break;
            }
            result = g_list_next (result);
        }
    }
    qof_query_destroy (q);
    return object;
}
