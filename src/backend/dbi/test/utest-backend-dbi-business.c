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

/*
 * utest-gnc-prefs-gconf.c
 *
 *  Created on: 2011-04-23
 *      Author: phil
 */

#include "config.h"

#include <stdio.h>

#include "unittest-support.h"
#include "test-stuff.h"
#include "test-dbi-stuff.h"

static const gchar* suitename = "/backend/dbi";
void test_suite_gnc_backend_dbi_business(void);

void do_test_business_sqlite(void);

#define DBI_TEST_XML_FILENAME "test-dbi.xml"
#define FILE_NAME "sqlite3:///tmp/test-sqlite3-file"
#define GNC_LIB_NAME "gncmod-backend-dbi"
#include "config.h"
#include "qof.h"
#include "cashobjects.h"
#include "test-engine-stuff.h"
#include "test-stuff.h"
#include "test-dbi-business-stuff.h"

#include "Account.h"
#include <TransLog.h>
#include "Transaction.h"
#include "Split.h"
#include "gnc-commodity.h"
#include "gncAddress.h"
#include "gncCustomer.h"
#include "gncInvoice.h"

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
create_business_session(void)
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
    GncEmployee* emp;
    GncTaxTable* tt;
    GncTaxTableEntry* tte;

    table = gnc_commodity_table_get_table( book );
    currency = gnc_commodity_table_lookup( table, GNC_COMMODITY_NS_CURRENCY, "CAD" );

    acct1 = xaccMallocAccount( book );
    xaccAccountSetType( acct1, ACCT_TYPE_BANK );
    xaccAccountSetName( acct1, "Bank 1" );
    xaccAccountSetCommodity( acct1, currency );
    xaccAccountSetHidden( acct1, FALSE );
    xaccAccountSetPlaceholder( acct1, FALSE );
    gnc_account_append_child( root, acct1 );

    acct2 = xaccMallocAccount( book );
    xaccAccountSetType( acct2, ACCT_TYPE_BANK );
    xaccAccountSetName( acct2, "Bank 2" );
    xaccAccountSetCommodity( acct2, currency );
    xaccAccountSetHidden( acct2, FALSE );
    xaccAccountSetPlaceholder( acct2, FALSE );
    gnc_account_append_child( root, acct2 );

    tt = gncTaxTableCreate( book );
    gncTaxTableSetName( tt, "tt" );
    tte = gncTaxTableEntryCreate();
    gncTaxTableEntrySetAccount( tte, acct1 );
    gncTaxTableEntrySetType( tte, GNC_AMT_TYPE_VALUE );
    gncTaxTableEntrySetAmount( tte, gnc_numeric_zero() );
    gncTaxTableAddEntry( tt, tte );
    tte = gncTaxTableEntryCreate();
    gncTaxTableEntrySetAccount( tte, acct2 );
    gncTaxTableEntrySetType( tte, GNC_AMT_TYPE_PERCENT );
    gncTaxTableEntrySetAmount( tte, gnc_numeric_zero() );
    gncTaxTableAddEntry( tt, tte );

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

    emp = gncEmployeeCreate( book );
    gncEmployeeSetID( emp, "0001" );
    gncEmployeeSetUsername( emp, "gnucash" );
    gncEmployeeSetLanguage( emp, "english" );
    gncEmployeeSetCurrency( emp, currency );

    return session;
}

/* Order in which business objects need to be loaded */
static const gchar* fixed_load_order[] =
{ GNC_ID_BILLTERM, GNC_ID_TAXTABLE, NULL };

G_GNUC_UNUSED static void
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

static gboolean handler(const gchar* log_domain, GLogLevelFlags log_level, const gchar* message, gpointer user_data)
{
    printf("domain=%s level=%d message=%s\n", log_domain, log_level, message);
    return FALSE;
}

void
do_test_business_sqlite(void)
{
    gchar* filename;
    QofSession* session_1;

    g_test_log_set_fatal_handler(handler, 0);

    // Create a session with data
    session_1 = create_business_session();
    filename = tempnam( "/tmp", "test-sqlite3-" );
    g_test_message ( "Using filename: %s\n", filename );
    test_dbi_business_store_and_reload( "sqlite3", session_1, filename );
}

void
test_suite_gnc_backend_dbi_business(void)
{
    GNC_TEST_ADD_FUNC(suitename, "gnc dbi test sqlite (business)", do_test_business_sqlite);
}
