/***************************************************************************
 *            test-dbi-business-stuff.c
 *
 *  Tests saving and loading business objects to a dbi/sqlite3 db
 *
 *  Copyright (C) 2010  Phil Longstaff <plongstaff@rogers.com>
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
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301, USA.
 */

#include "config.h"
#include "qof.h"
#include "cashobjects.h"
#include "test-engine-stuff.h"
#include "test-stuff.h"
#include "test-dbi-stuff.h"
#include "test-dbi-business-stuff.h"

#include "Account.h"
#include "Split.h"
#include "Transaction.h"
#include "gnc-commodity.h"
#include "gncCustomer.h"
#include "gncInvoice.h"
#include "gncEmployee.h"
#include "gncVendor.h"

static QofLogModule log_module = "test-dbi";

static void
compare_single_customer( QofInstance* inst, gpointer user_data )
{
    CompareInfoStruct* info = (CompareInfoStruct*)user_data;
    GncCustomer* cust_1 = GNC_CUSTOMER(inst);
    GncCustomer* cust_2 = gncCustomerLookup( info->book_2, qof_instance_get_guid(inst) );

    if (!gncCustomerEqual( cust_1, cust_2 ))
    {
        info->result = FALSE;
    }
}

static void
compare_customers( QofBook* book_1, QofBook* book_2 )
{
    do_compare( book_1, book_2, GNC_ID_CUSTOMER, compare_single_customer, "Customer lists match" );
}

static void
compare_single_employee( QofInstance* inst, gpointer user_data )
{
    CompareInfoStruct* info = (CompareInfoStruct*)user_data;
    GncEmployee* emp_1 = GNC_EMPLOYEE(inst);
    GncEmployee* emp_2 = gncEmployeeLookup( info->book_2, qof_instance_get_guid(inst) );

    if (!gncEmployeeEqual( emp_1, emp_2 ))
    {
        info->result = FALSE;
    }
}

static void
compare_employees( QofBook* book_1, QofBook* book_2 )
{
    do_compare( book_1, book_2, GNC_ID_EMPLOYEE, compare_single_employee, "Employee lists match" );
}

static void
compare_single_invoice( QofInstance* inst, gpointer user_data )
{
    CompareInfoStruct* info = (CompareInfoStruct*)user_data;
    GncInvoice* inv_1 = GNC_INVOICE(inst);
    GncInvoice* inv_2 = gncInvoiceLookup( info->book_2, qof_instance_get_guid(inst) );

    if (!gncInvoiceEqual( inv_1, inv_2 ))
    {
        info->result = FALSE;
    }
}

static void
compare_invoices( QofBook* book_1, QofBook* book_2 )
{
    do_compare( book_1, book_2, GNC_ID_INVOICE, compare_single_invoice, "Invoice lists match" );
}

static void
compare_single_job( QofInstance* inst, gpointer user_data )
{
    CompareInfoStruct* info = (CompareInfoStruct*)user_data;
    GncJob* job_1 = GNC_JOB(inst);
    GncJob* job_2 = gncJobLookup( info->book_2, qof_instance_get_guid(inst) );

    if (!gncJobEqual( job_1, job_2 ))
    {
        info->result = FALSE;
    }
}

static void
compare_jobs( QofBook* book_1, QofBook* book_2 )
{
    do_compare( book_1, book_2, GNC_ID_JOB, compare_single_job, "Job lists match" );
}

static void
compare_single_vendor( QofInstance* inst, gpointer user_data )
{
    CompareInfoStruct* info = (CompareInfoStruct*)user_data;
    GncVendor* vendor_1 = GNC_VENDOR(inst);
    GncVendor* vendor_2 = gncVendorLookup( info->book_2, qof_instance_get_guid(inst) );

    if (!gncVendorEqual( vendor_1, vendor_2 ))
    {
        info->result = FALSE;
    }
}

static void
compare_vendors( QofBook* book_1, QofBook* book_2 )
{
    do_compare( book_1, book_2, GNC_ID_VENDOR, compare_single_vendor, "Vendor lists match" );
}

static void
compare_single_billterm( QofInstance* inst, gpointer user_data )
{
    CompareInfoStruct* info = (CompareInfoStruct*)user_data;
    GncBillTerm* bt_1 = GNC_BILLTERM(inst);
    GncBillTerm* bt_2 = gncBillTermLookup( info->book_2, qof_instance_get_guid(inst) );

    if (!gncBillTermEqual( bt_1, bt_2 ))
    {
        info->result = FALSE;
    }
}

static void
compare_billterms( QofBook* book_1, QofBook* book_2 )
{
    do_compare( book_1, book_2, GNC_ID_BILLTERM, compare_single_billterm, "Billterms lists match" );
}

static void
compare_single_taxtable( QofInstance* inst, gpointer user_data )
{
    CompareInfoStruct* info = (CompareInfoStruct*)user_data;
    GncTaxTable* tt_1 = GNC_TAXTABLE(inst);
    GncTaxTable* tt_2 = gncTaxTableLookup( info->book_2, qof_instance_get_guid(inst) );

    if (!gncTaxTableEqual( tt_1, tt_2 ))
    {
        info->result = FALSE;
    }
}

static void
compare_taxtables( QofBook* book_1, QofBook* book_2 )
{
    do_compare( book_1, book_2, GNC_ID_TAXTABLE, compare_single_taxtable, "TaxTable lists match" );
}

static void
compare_books( QofBook* book_1, QofBook* book_2 )
{
    compare_billterms( book_1, book_2 );
    compare_taxtables( book_1, book_2 );

    compare_customers( book_1, book_2 );
    compare_employees( book_1, book_2 );
    compare_invoices( book_1, book_2 );
    compare_jobs( book_1, book_2 );
    compare_vendors( book_1, book_2 );
}

void
test_dbi_business_store_and_reload( const gchar* driver, QofSession* session_1, const gchar* url )
{
    QofSession* session_2;
    QofSession* session_3;

    printf( "Testing %s\n", driver );

    // Save the session data
    session_2 = qof_session_new();
    qof_session_begin( session_2, url, FALSE, TRUE );
    qof_session_swap_data( session_1, session_2 );
    qof_session_save( session_2, NULL );

    // Reload the session data
    session_3 = qof_session_new();
    qof_session_begin( session_3, url, TRUE, FALSE );
    qof_session_load( session_3, NULL );

    // Compare with the original data
    compare_books( qof_session_get_book( session_2 ), qof_session_get_book( session_3 ) );
    qof_session_end( session_1 );
    qof_session_destroy( session_1 );
    qof_session_end( session_2 );
    qof_session_destroy( session_2 );
    g_print(" You may ignore the warning about the lock file having no entries: We had to ignore locking to run two sessions on the same database\n");
    qof_session_end( session_3 );
    qof_session_destroy( session_3 );
}
