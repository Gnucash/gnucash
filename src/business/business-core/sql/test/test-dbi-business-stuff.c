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
compare_books( QofBook* book_1, QofBook* book_2 )
{
    compare_customers( book_1, book_2 );
//    compare_invoices( book_1, book_2 );
//    compare_jobs( book_1, book_2 );
//    compare_vendors( book_1, book_2 );
//    compare_billterms( book_1, book_2 );
//    compare_employees( book_1, book_2 );
}

void
test_dbi_business_store_and_reload( const gchar* driver, QofSession* session_1, const gchar* url )
{
    QofSession* session_2;
    QofSession* session_3;

    printf( "Testing %s\n", driver );

    // Save the session data
    session_2 = qof_session_new();
    qof_session_begin( session_2, url, TRUE, TRUE );
    qof_session_swap_data( session_1, session_2 );
    qof_session_save( session_2, NULL );

    // Reload the session data
    session_3 = qof_session_new();
    qof_session_begin( session_3, url, FALSE, FALSE );
    qof_session_load( session_3, NULL );

    // Compare with the original data
    compare_books( qof_session_get_book( session_2 ), qof_session_get_book( session_3 ) );
    qof_session_end( session_1 );
    qof_session_destroy( session_1 );
    qof_session_end( session_2 );
    qof_session_destroy( session_2 );
    qof_session_end( session_3 );
    qof_session_destroy( session_3 );
}
