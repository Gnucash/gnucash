/*
 * utest-gnc-prefs-gconf.c
 *
 *  Created on: 2011-04-23
 *      Author: phil
 */

#include "config.h"

#include <glib/gstdio.h>

#include "unittest-support.h"
#include "test-stuff.h"
#include "test-dbi-stuff.h"

static const gchar* suitename = "/backend/dbi";
void test_suite_gnc_backend_dbi_basic(void);

void do_test_sqlite(void);
void do_test_mysql(void);
void do_test_pgsql(void);

#define DBI_TEST_XML_FILENAME "test-dbi.xml"

typedef struct
{
     QofSession *session;
     gchar *filename;
} Fixture;

static void
setup (Fixture *fixture, gconstpointer pData)
{
    fixture->session = qof_session_new();
    qof_session_begin( fixture->session, DBI_TEST_XML_FILENAME, FALSE,
		       FALSE, FALSE );
    g_assert_cmpint (qof_session_get_error (fixture->session), ==,
		     ERR_BACKEND_NO_ERR);
    qof_session_load( fixture->session, NULL );

    fixture->filename = tempnam( "/tmp", "test-sqlite3-" );
    g_test_message ( "Using filename: %s\n", fixture->filename );
}

static void
teardown (Fixture *fixture, gconstpointer pData)
{
     qof_session_end (fixture->session);
     qof_session_destroy (fixture->session);
     g_unlink (fixture->filename);
}


static void
test_sqlite_store_and_reload (Fixture *fixture, gconstpointer pData)
{
    // Create a session with data
    test_dbi_store_and_reload( "sqlite3", fixture->session, fixture->filename );
}

static void
test_mysql_store_and_reload (Fixture *fixture, gconstpointer pData)
{
    g_assert (strlen (TEST_MYSQL_URL) > 0);
    test_dbi_store_and_reload( TEST_MYSQL_URL, fixture->session,
			       fixture->filename );
}

static void
test_pgsql_store_and_reload (Fixture *fixture, gconstpointer pData)
{
    g_assert (strlen (TEST_PGSQL_URL) > 0);
    test_dbi_store_and_reload( TEST_PGSQL_URL, fixture->session,
			       fixture->filename );
}

void
test_suite_gnc_backend_dbi_basic(void)
{
     GNC_TEST_ADD (suitename, "store_and_reload/sqlite", Fixture, NULL, setup, test_sqlite_store_and_reload, teardown);
     if (strlen (TEST_MYSQL_URL) > 0)
         GNC_TEST_ADD (suitename, "store_and_reload/mysql", Fixture, NULL, setup, test_mysql_store_and_reload, teardown);
     if (strlen (TEST_PGSQL_URL) > 0)
         GNC_TEST_ADD (suitename, "store_and_reload/postgres", Fixture, NULL, setup, test_pgsql_store_and_reload, teardown);

}
