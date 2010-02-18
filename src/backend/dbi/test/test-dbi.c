/***************************************************************************
 *            test-dbi.c
 *
 *  Tests saving and loading to a dbi/sqlite3 db
 *
 *  Copyright (C) 2009  Phil Longstaff <plongstaff@rogers.com>
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

#include "Account.h"
#include "Split.h"
#include "gnc-commodity.h"

#define DBI_TEST_XML_FILENAME "test-dbi.xml"
#define FILE_NAME "sqlite3:///tmp/test-sqlite3-file"
#define GNC_LIB_NAME "gncmod-backend-dbi"

int main (int argc, char ** argv)
{
    gchar* filename;
    QofSession* session_1;

    qof_init();
    cashobjects_register();
    qof_load_backend_library ("../.libs/", GNC_LIB_NAME);

    // Create a session with data
    session_1 = qof_session_new();
    qof_session_begin( session_1, DBI_TEST_XML_FILENAME, FALSE, FALSE );
    qof_session_load( session_1, NULL );

    filename = tempnam( "/tmp", "test-sqlite3-" );
    printf( "Using filename: %s\n", filename );
    test_dbi_store_and_reload( "sqlite3", session_1, filename );

    printf( "TEST_MYSQL_URL='%s'\n", TEST_MYSQL_URL );
    if ( strlen( TEST_MYSQL_URL ) > 0 )
    {
        session_1 = qof_session_new();
        qof_session_begin( session_1, DBI_TEST_XML_FILENAME, FALSE, FALSE );
        qof_session_load( session_1, NULL );
        test_dbi_store_and_reload( "mysql", session_1, TEST_MYSQL_URL );
    }

    printf( "TEST_PGSQL_URL='%s'\n", TEST_PGSQL_URL );
    if ( strlen( TEST_PGSQL_URL ) > 0 )
    {
        session_1 = qof_session_new();
        qof_session_begin( session_1, DBI_TEST_XML_FILENAME, FALSE, FALSE );
        qof_session_load( session_1, NULL );
        test_dbi_store_and_reload( "pgsql", session_1, TEST_PGSQL_URL );
    }
    print_test_results();
    qof_close();
    exit(get_rv());
}
