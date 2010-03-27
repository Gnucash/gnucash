/***************************************************************************
 *            test-dbi-business.c
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
#include "test-dbi-business-stuff.h"

#include "Account.h"
#include "Transaction.h"
#include "Split.h"
#include "gnc-commodity.h"
#include "gncAddress.h"
#include "gncCustomer.h"

#include "gnc-backend-sql.h"

#include "gnc-address-sql.h"
#include "gnc-bill-term-sql.h"
#include "gnc-customer-sql.h"
#include "gnc-employee-sql.h"
#include "gnc-entry-sql.h"
#include "gnc-invoice-sql.h"
#include "gnc-job-sql.h"
#include "gnc-order-sql.h"
#include "gnc-owner-sql.h"
#include "gnc-tax-table-sql.h"
#include "gnc-vendor-sql.h"

#define FILE_NAME "sqlite3:///tmp/test-sqlite3-file"
#define GNC_LIB_NAME "gncmod-backend-dbi"

static QofSession*
create_session(void)
{
    QofSession* session = qof_session_new();
    QofBook* book = qof_session_get_book( session );
    Account* root = gnc_book_get_root_account( book );
    Account* acct1;
    Account* acct2;
    gnc_commodity_table* table;
    gnc_commodity* currency;
    GncAddress* addr;
    GncCustomer* cust;

    table = gnc_commodity_table_get_table( book );
    currency = gnc_commodity_table_lookup( table, GNC_COMMODITY_NS_CURRENCY, "CAD" );

    acct1 = xaccMallocAccount( book );
    xaccAccountSetType( acct1, ACCT_TYPE_BANK );
    xaccAccountSetName( acct1, "Bank 1" );
    xaccAccountSetCommodity( acct1, currency );

    gnc_account_append_child( root, acct1 );

    acct2 = xaccMallocAccount( book );
    xaccAccountSetType( acct2, ACCT_TYPE_BANK );
    xaccAccountSetName( acct2, "Bank 1" );

    cust = gncCustomerCreate( book );
    gncCustomerSetID( cust, "0001" );
    gncCustomerSetName( cust, "MyCustomer" );
    gncCustomerSetNotes( cust, "Here are some notes" );
    gncCustomerSetCurrency( cust, currency );
    addr = gncAddressCreate( book, QOF_INSTANCE(cust) );
    gncAddressSetName( addr, "theAddress" );
    gncAddressSetAddr1( addr, "Address line #1" );
    gncAddressSetAddr2( addr, "Address line #2" );
    gncAddressSetAddr3( addr, "Address line #3" );
    gncAddressSetAddr4( addr, "Address line #4" );
    gncAddressSetPhone( addr, "(123) 555-1212" );
    gncAddressSetPhone( addr, "(123) 555-2121" );
    gncAddressSetEmail( addr, "cust@mycustomer.com" );

    return session;
}

/* Order in which business objects need to be loaded */
static const gchar* fixed_load_order[] =
{ GNC_ID_BILLTERM, GNC_ID_TAXTABLE, NULL };

static void
init_business_sql(void)
{
    /* Initialize our pointers into the backend subsystem */
    gnc_address_sql_initialize();
    gnc_billterm_sql_initialize();
    gnc_customer_sql_initialize();
    gnc_employee_sql_initialize();
    gnc_entry_sql_initialize();
    gnc_invoice_sql_initialize();
    gnc_job_sql_initialize();
    gnc_order_sql_initialize();
    gnc_owner_sql_initialize();
    gnc_taxtable_sql_initialize();
    gnc_vendor_sql_initialize();

    gnc_sql_set_load_order( fixed_load_order );
}

int main (int argc, char ** argv)
{
    gchar* filename;
    QofSession* session_1;

    qof_init();
    cashobjects_register();
    gnc_module_init_business_core_init();
    qof_load_backend_library ("../../../../backend/dbi/.libs/", GNC_LIB_NAME);
    init_business_sql();

    // Create a session with data
    session_1 = create_session();
    filename = tempnam( "/tmp", "test-sqlite3-" );
    printf( "Using filename: %s\n", filename );
    test_dbi_business_store_and_reload( "sqlite3", session_1, filename );
#if 0
    printf( "TEST_MYSQL_URL='%s'\n", TEST_MYSQL_URL );
    if ( strlen( TEST_MYSQL_URL ) > 0 )
    {
        session_1 = create_session();
        test_dbi_store_and_reload( "mysql", session_1, TEST_MYSQL_URL );
    }
    printf( "TEST_PGSQL_URL='%s'\n", TEST_PGSQL_URL );
    if ( strlen( TEST_PGSQL_URL ) > 0 )
    {
        session_1 = create_session();
        test_dbi_store_and_reload( "pgsql", session_1, TEST_PGSQL_URL );
    }
#endif
    print_test_results();
    qof_close();
    exit(get_rv());
}

