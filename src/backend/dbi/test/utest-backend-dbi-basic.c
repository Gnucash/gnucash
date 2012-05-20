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

#define DBI_TEST_XML_FILENAME "test-dbi.xml"
#define FILE_NAME "sqlite3:///tmp/test-sqlite3-file"

static gboolean handler(const gchar* log_domain, GLogLevelFlags log_level, const gchar* message, gpointer user_data)
{
    printf("domain=%s level=%d message=%s\n", log_domain, log_level, message);
    return FALSE;
}

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
test_suite_gnc_backend_dbi_basic(void)
{
    GNC_TEST_ADD_FUNC(suitename, "gnc dbi test sqlite", do_test_sqlite);
}
