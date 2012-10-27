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
void test_suite_gnc_backend_dbi_basic(void);

void do_test_sqlite(void);
void do_test_mysql(void);
void do_test_pgsql(void);

#define DBI_TEST_XML_FILENAME "test-dbi.xml"
#define FILE_NAME "sqlite3:///tmp/test-sqlite3-file"

typedef struct
{
     QofSession *session;

void
do_test_sqlite(void)
{
    gchar* filename;
    QofSession* session_1;

    g_test_log_set_fatal_handler(handler, 0);

    // Create a session with data
    session_1 = qof_session_new();
    qof_session_begin( session_1, DBI_TEST_XML_FILENAME, FALSE, FALSE, FALSE );
    qof_session_load( session_1, NULL );

    filename = tempnam( "/tmp", "test-sqlite3-" );
    g_test_message ( "Using filename: %s\n", filename );
    test_dbi_store_and_reload( "sqlite3", session_1, filename );
}

void
do_test_mysql(void)
{
    gchar* filename;
    QofSession* session_1;


    // Create a session with data
    session_1 = qof_session_new();
    qof_session_begin( session_1, DBI_TEST_XML_FILENAME, FALSE, FALSE, FALSE );
    qof_session_load( session_1, NULL );

    g_test_message ( "Using database: %s\n", TEST_MYSQL_URL );
    test_dbi_store_and_reload( "mysql", session_1, TEST_MYSQL_URL );
}

void
do_test_pgsql(void)
{
    gchar* filename;
    QofSession* session_1;

    // Create a session with data
    session_1 = qof_session_new();
    qof_session_begin( session_1, DBI_TEST_XML_FILENAME, FALSE, FALSE, FALSE );
    qof_session_load( session_1, NULL );

    g_test_message ( "Using database: %s\n", TEST_PGSQL_URL );
    test_dbi_store_and_reload( "postgres", session_1, TEST_PGSQL_URL );
}

void
test_suite_gnc_backend_dbi_basic(void)
{
    GNC_TEST_ADD_FUNC(suitename, "gnc dbi test sqlite", do_test_sqlite);
    if ( strlen( TEST_MYSQL_URL ) > 0 )
        GNC_TEST_ADD_FUNC(suitename, "gnc dbi test mysql", do_test_mysql);
    if ( strlen( TEST_PGSQL_URL ) > 0 )
        GNC_TEST_ADD_FUNC(suitename, "gnc dbi test pgsql", do_test_pgsql);
}
