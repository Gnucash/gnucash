/*
 * utest-backend-dbi-basic.c
 *
 *  Created on: 2011-04-23
 *      Author: phil
 */

#include "config.h"

#include <sys/types.h>
#include <unistd.h>
#include <glib/gstdio.h>

#include <qof.h>
#include <unittest-support.h>
#include <test-stuff.h>
#include <test-dbi-stuff.h>
#include <test-dbi-business-stuff.h>
/* For cleaning up the database */
#include <dbi/dbi.h>
#include <gnc-uri-utils.h>
/* For setup_business */
#include "Account.h"
#include <TransLog.h>
#include "Transaction.h"
#include "Split.h"
#include "gnc-commodity.h"
#include "gncAddress.h"
#include "gncCustomer.h"
#include "gncInvoice.h"
/* For test_conn_index_functions */
#include "../gnc-backend-dbi-priv.h"
/* For version_control */
#include <gnc-prefs.h>
#include <qofsession-p.h>

#if LIBDBI_VERSION >= 900
#define HAVE_LIBDBI_R 1
static dbi_inst dbi_instance = NULL;
#else
#define HAVE_LIBDBI_R 0
#endif

static const gchar* suitename = "/backend/dbi";
void test_suite_gnc_backend_dbi (void);

typedef struct
{
    QofSession *session;
    gchar *filename;
    GSList *hdlrs;
} Fixture;

static void
setup (Fixture *fixture, gconstpointer pData)
{
    gchar *url = (gchar *)pData;
    fixture->session = qof_session_new();
    /* When running distcheck the source directory is read-only, which
     * prevents creating the lock file. Force the session to get
     * around that.
     */
    qof_session_begin (fixture->session, DBI_TEST_XML_FILENAME, TRUE,
                       FALSE, TRUE);
    g_assert_cmpint (qof_session_get_error (fixture->session), ==,
                     ERR_BACKEND_NO_ERR);
    qof_session_load (fixture->session, NULL);

    if (g_strcmp0 (url, "sqlite3") == 0)
        fixture->filename = g_strdup_printf ("/tmp/test-sqlite-%d", getpid());
    else
        fixture->filename = NULL;
}

static void
setup_memory (Fixture *fixture, gconstpointer pData)
{
    QofSession* session = qof_session_new();
    gchar *url = (gchar*)pData;
    QofBook* book;
    Account *root, *acct1, *acct2;
    KvpFrame* frame;
    Transaction* tx;
    Split *spl1, *spl2;
    gnc_commodity_table* table;
    gnc_commodity* currency;

    session = qof_session_new();
    book = qof_session_get_book (session);
    root = gnc_book_get_root_account (book);

    table = gnc_commodity_table_get_table (book);
    currency = gnc_commodity_table_lookup (table, GNC_COMMODITY_NS_CURRENCY, "CAD");

    acct1 = xaccMallocAccount (book);
    xaccAccountSetType (acct1, ACCT_TYPE_BANK);
    xaccAccountSetName (acct1, "Bank 1");
    xaccAccountSetCommodity (acct1, currency);

    frame = qof_instance_get_slots (QOF_INSTANCE(acct1));
    kvp_frame_set_gint64 (frame, "int64-val", 100);
    kvp_frame_set_double (frame, "double-val", 3.14159);
    kvp_frame_set_numeric (frame, "numeric-val", gnc_numeric_zero());

    kvp_frame_set_timespec (frame, "timespec-val", timespec_now ());

    kvp_frame_set_string (frame, "string-val", "abcdefghijklmnop");
    kvp_frame_set_guid (frame, "guid-val", qof_instance_get_guid (QOF_INSTANCE(acct1)));

    gnc_account_append_child (root, acct1);

    acct2 = xaccMallocAccount (book);
    xaccAccountSetType (acct2, ACCT_TYPE_BANK);
    xaccAccountSetName (acct2, "Bank 1");

    tx = xaccMallocTransaction (book);
    xaccTransBeginEdit (tx);
    xaccTransSetCurrency (tx, currency);
    spl1 = xaccMallocSplit (book);
    xaccTransAppendSplit (tx, spl1);
    spl2 = xaccMallocSplit (book);
    xaccTransAppendSplit (tx, spl2);
    xaccTransCommitEdit (tx);

    fixture->session = session;
    if (g_strcmp0 (url, "sqlite3") == 0)
        fixture->filename = g_strdup_printf ("/tmp/test-sqlite-%d", getpid());
    else
        fixture->filename = NULL;
}

static void
setup_business (Fixture *fixture, gconstpointer pData)
{
    QofSession* session = qof_session_new();
    gchar *url = (gchar*)pData;
    QofBook* book = qof_session_get_book (session);
    Account* root = gnc_book_get_root_account (book);
    Account* acct1;
    Account* acct2;
    gnc_commodity_table* table;
    gnc_commodity* currency;
    GncAddress* addr;
    GncCustomer* cust;
    GncEmployee* emp;
    GncTaxTable* tt;
    GncTaxTableEntry* tte;

    table = gnc_commodity_table_get_table (book);
    currency = gnc_commodity_table_lookup (table, GNC_COMMODITY_NS_CURRENCY, "CAD");

    acct1 = xaccMallocAccount (book);
    xaccAccountSetType (acct1, ACCT_TYPE_BANK);
    xaccAccountSetName (acct1, "Bank 1");
    xaccAccountSetCommodity (acct1, currency);
    xaccAccountSetHidden (acct1, FALSE);
    xaccAccountSetPlaceholder (acct1, FALSE);
    gnc_account_append_child (root, acct1);

    acct2 = xaccMallocAccount (book);
    xaccAccountSetType (acct2, ACCT_TYPE_BANK);
    xaccAccountSetName (acct2, "Bank 2");
    xaccAccountSetCommodity (acct2, currency);
    xaccAccountSetHidden (acct2, FALSE);
    xaccAccountSetPlaceholder (acct2, FALSE);
    gnc_account_append_child (root, acct2);

    tt = gncTaxTableCreate (book);
    gncTaxTableSetName (tt, "tt");
    tte = gncTaxTableEntryCreate();
    gncTaxTableEntrySetAccount (tte, acct1);
    gncTaxTableEntrySetType (tte, GNC_AMT_TYPE_VALUE);
    gncTaxTableEntrySetAmount (tte, gnc_numeric_zero());
    gncTaxTableAddEntry (tt, tte);
    tte = gncTaxTableEntryCreate();
    gncTaxTableEntrySetAccount (tte, acct2);
    gncTaxTableEntrySetType (tte, GNC_AMT_TYPE_PERCENT);
    gncTaxTableEntrySetAmount (tte, gnc_numeric_zero());
    gncTaxTableAddEntry (tt, tte);

    cust = gncCustomerCreate (book);
    gncCustomerSetID (cust, "0001");
    gncCustomerSetName (cust, "MyCustomer");
    gncCustomerSetNotes (cust, "Here are some notes");
    gncCustomerSetCurrency (cust, currency);
    addr = gncAddressCreate (book, QOF_INSTANCE(cust));
    gncAddressSetName (addr, "theAddress");
    gncAddressSetAddr1 (addr, "Address line #1");
    gncAddressSetAddr2 (addr, "Address line #2");
    gncAddressSetAddr3 (addr, "Address line #3");
    gncAddressSetAddr4 (addr, "Address line #4");
    gncAddressSetPhone (addr, "(123) 555-1212");
    gncAddressSetPhone (addr, "(123) 555-2121");
    gncAddressSetEmail (addr, "cust@mycustomer.com");

    emp = gncEmployeeCreate (book);
    gncEmployeeSetID (emp, "0001");
    gncEmployeeSetUsername (emp, "gnucash");
    gncEmployeeSetLanguage (emp, "english");
    gncEmployeeSetCurrency (emp, currency);

    fixture->session = session;
    if (g_strcmp0 (url, "sqlite3") == 0)
        fixture->filename = g_strdup_printf ("/tmp/test-sqlite-%d", getpid());
    else
        fixture->filename = NULL;
}

static void
drop_table (gconstpointer tdata, gconstpointer cdata)
{
    gchar *table = (gchar*)tdata;
    dbi_conn conn = (dbi_conn)cdata;
    gchar *query = g_strdup_printf ("DROP TABLE %s", table);
    dbi_result rslt = dbi_conn_query (conn, query);
    g_free (query);
}

static void
destroy_database (gchar* url)
{
    gchar *protocol = NULL;
    gchar *host = NULL;
    gchar *dbname = NULL;
    gchar *username = NULL;
    gchar *password = NULL;
    gchar *basename = NULL;
    gint portnum = 0;
    gchar *port = NULL;
    gchar *pgsql = "pgsql";
    dbi_conn conn = NULL;
    gchar *errfmt = "Unable to delete tables in %s: %s";
    gint fail = 0;
    dbi_result tables;
    GSList *list = NULL;

    gnc_uri_get_components  (url, &protocol, &host, &portnum,
                             &username, &password, &dbname);
    if (g_strcmp0 (protocol, "postgres") == 0)
        #if HAVE_LIBDBI_R
        conn = dbi_conn_new_r( pgsql, dbi_instance );
        #else
        conn = dbi_conn_new( pgsql );
        #endif
    else
        #if HAVE_LIBDBI_R
        conn = dbi_conn_new_r (protocol, dbi_instance);
        #else
        conn = dbi_conn_new (protocol);
        #endif
    port = g_strdup_printf ("%d", portnum);
    if (conn == NULL)
    {
        g_printf (errfmt, url, "failed to create connection");
        return;
    }
    fail = dbi_conn_set_option (conn, "host", host);
    if (!fail && portnum)
        fail = dbi_conn_set_option (conn, "port", port);
    if (!fail)
        fail = dbi_conn_set_option (conn, "dbname", dbname);
    if (!fail)
        fail = dbi_conn_set_option (conn, "username", username);
    if (!fail)
        fail = dbi_conn_set_option (conn, "password", password);
    if (!fail)
        fail = dbi_conn_set_option (conn, "encoding", "UTF-8");
    g_free (port);
    if (fail != 0)
    {
        g_printf (errfmt, url, "failed to set an option");
        dbi_conn_close (conn);
        return;
    }
    fail = dbi_conn_connect (conn);
    if (fail != 0)
    {
        const gchar *error;
        gint errnum = dbi_conn_error (conn, &error);
        g_printf (errfmt, url, error);
        dbi_conn_close (conn);
        return;
    }
    tables = dbi_conn_get_table_list (conn, dbname, NULL);
    while (dbi_result_next_row (tables) != 0)
    {
        const gchar *table = dbi_result_get_string_idx (tables, 1);
        list = g_slist_prepend (list, g_strdup (table));
    }
    dbi_result_free (tables);
    g_slist_foreach (list, (GFunc)drop_table, (gpointer)conn);
    g_slist_free_full (list, (GDestroyNotify)g_free);
}

static void
teardown (Fixture *fixture, gconstpointer pData)
{
    gchar *lockfile = g_strdup_printf ("%s/test-dbi.xml.LCK",
                                       g_path_get_dirname (DBI_TEST_XML_FILENAME));
    gchar *msg = g_strdup_printf ("[xml_session_end()] Error on g_unlink(%s): 2: No such file or directory", lockfile);
    gchar *logdomain = "gnc.backend";
    guint loglevel = G_LOG_LEVEL_WARNING | G_LOG_FLAG_FATAL;
    TestErrorStruct *check = test_error_struct_new (logdomain, loglevel, msg);
    fixture->hdlrs = test_log_set_fatal_handler (fixture->hdlrs, check,
                     (GLogFunc)test_checked_handler);
    qof_session_end (fixture->session);
    qof_session_destroy (fixture->session);
    if (fixture->filename)
    {
        g_unlink (fixture->filename);
	g_free (fixture->filename);
    }
    else
        destroy_database ((gchar*)pData);

    g_free (msg);
    g_free (lockfile);
    g_slist_free_full (fixture->hdlrs, test_free_log_handler);
    test_clear_error_list();
}


static void
test_conn_index_functions (QofBackend *qbe)
{
    GncDbiBackend *be = (GncDbiBackend*)qbe;
    GncDbiSqlConnection *conn = (GncDbiSqlConnection*)(be->sql_be.conn);
    GSList *index_list, *iter;

    index_list = conn->provider->get_index_list (be->conn);
    g_test_message  ("Returned from index list\n");
    g_assert (index_list != NULL);
    g_assert_cmpint (g_slist_length (index_list), ==, 4);
    for  (iter = index_list; iter != NULL; iter = g_slist_next (iter))
    {
        const char *errmsg;
        conn->provider->drop_index (be->conn, iter->data);
        g_assert (DBI_ERROR_NONE == dbi_conn_error (conn->conn, &errmsg));
    }

    g_slist_free (index_list);
}

/* Given a synthetic session, use the same logic as
 * QofSession::save_as to save it to a specified sql url, then load it
 * back and compare. */
static void
test_dbi_store_and_reload (Fixture *fixture, gconstpointer pData)
{

    const gchar* url = (const gchar*)pData;
    QofSession* session_2;
    QofSession* session_3;
    QofBackend *be;

    gchar *msg = "[gnc_dbi_unlock()] There was no lock entry in the Lock table";
    gchar *log_domain = "gnc.backend.dbi";
    guint loglevel = G_LOG_LEVEL_WARNING | G_LOG_FLAG_FATAL;
    TestErrorStruct *check = test_error_struct_new (log_domain, loglevel, msg);
    fixture->hdlrs = test_log_set_fatal_handler (fixture->hdlrs, check,
                     (GLogFunc)test_checked_handler);
    if (fixture->filename)
        url = fixture->filename;

    // Save the session data
    session_2 = qof_session_new();
    qof_session_begin (session_2, url, FALSE, TRUE, TRUE);
    g_assert (session_2 != NULL);
    g_assert_cmpint (qof_session_get_error (session_2), ==, ERR_BACKEND_NO_ERR);
    qof_session_swap_data (fixture->session, session_2);
    qof_session_save (session_2, NULL);
    g_assert (session_2 != NULL);
    g_assert_cmpint (qof_session_get_error (session_2), ==, ERR_BACKEND_NO_ERR);

    // Reload the session data
    session_3 = qof_session_new();
    g_assert (session_3 != NULL);
    qof_session_begin (session_3, url, TRUE, FALSE, FALSE);
    g_assert (session_3 != NULL);
    g_assert_cmpint (qof_session_get_error (session_3), ==, ERR_BACKEND_NO_ERR);
    qof_session_load (session_3, NULL);
    g_assert (session_3 != NULL);
    g_assert_cmpint (qof_session_get_error (session_3), ==, ERR_BACKEND_NO_ERR);
    // Compare with the original data
    compare_books (qof_session_get_book (session_2),
                   qof_session_get_book (session_3));
    /* fixture->session belongs to the fixture and teardown() will clean it up */
    qof_session_end (session_2);
    qof_session_destroy (session_2);
    qof_session_end (session_3);
    qof_session_destroy (session_3);
}

/** Test the safe_save mechanism.  Beware that this test used on its
 * own doesn't ensure that the resave is done safely, only that the
 * database is intact and unchanged after the save. To observe the
 * safety one must run the test in a debugger and break after the
 * rename step of gnc_dbi_safe_sync, then examine the database in the
 * appropriate shell.
 */
static void
test_dbi_safe_save (Fixture *fixture, gconstpointer pData)
{
    gchar *url = (gchar*)pData;
    QofSession *session_1 = NULL, *session_2 = NULL;
    QofBackend *be;

    gchar *msg = "[gnc_dbi_unlock()] There was no lock entry in the Lock table";
    gchar *log_domain = "gnc.backend.dbi";
    guint loglevel = G_LOG_LEVEL_WARNING | G_LOG_FLAG_FATAL;
    TestErrorStruct *check = test_error_struct_new (log_domain, loglevel, msg);

    if (fixture->filename)
        url = fixture->filename;

    // Load the session data
    session_1 = qof_session_new ();
    qof_session_begin (session_1, url, FALSE, TRUE, TRUE);
    if (session_1 &&
            qof_session_get_error (session_1) != ERR_BACKEND_NO_ERR)
    {
        g_warning ("Session Error: %d, %s", qof_session_get_error (session_1),
                   qof_session_get_error_message (session_1));
        g_test_message ("DB Session Creation Failed");
        g_assert (FALSE);
        goto cleanup;
    }
    qof_session_swap_data (fixture->session, session_1);
    qof_session_save (session_1, NULL);
    /* Do a safe save */
    qof_session_safe_save (session_1, NULL);
    if (session_1 && qof_session_get_error(session_1) != ERR_BACKEND_NO_ERR)
    {
        g_warning ("Session Error: %s",
                   qof_session_get_error_message(session_1));
        g_test_message ("DB Session Safe Save Failed");
        g_assert (FALSE);
        goto cleanup;
    }
    /* Destroy the session and reload it */

    session_2 = qof_session_new ();
    qof_session_begin (session_2, url, TRUE, FALSE, FALSE);
    if (session_2 &&
            qof_session_get_error (session_2) != ERR_BACKEND_NO_ERR)
    {
        g_warning ("Session Error: %d, %s", qof_session_get_error(session_2),
                   qof_session_get_error_message(session_2));
        g_test_message ("DB Session re-creation Failed");
        g_assert (FALSE);
        goto cleanup;
    }
    qof_session_load (session_2, NULL);
    compare_books (qof_session_get_book (session_1),
                   qof_session_get_book (session_2));
    be = qof_book_get_backend (qof_session_get_book (session_2));
    test_conn_index_functions (be);

cleanup:
    fixture->hdlrs = test_log_set_fatal_handler (fixture->hdlrs, check,
                     (GLogFunc)test_checked_handler);
    if (session_2 != NULL)
    {
        qof_session_end (session_2);
        qof_session_destroy (session_2);
    }
    if (session_1 != NULL)
    {
        qof_session_end (session_1);
        qof_session_destroy (session_1);
    }
    return;
}
/* Test the gnc_dbi_load logic that forces a newer database to be
 * opened read-only and an older one to be safe-saved. Again, it would
 * be better to do this starting from a fresh file, but instead we're
 * being lazy and using an existing one. */
static void
test_dbi_version_control (Fixture *fixture, gconstpointer pData)
{
    gchar *url = (gchar*)pData;
    QofSession *sess;
    QofBook *book;
    QofBackend *qbe;
    QofBackendError err;
    gint ourversion = gnc_prefs_get_long_version();

    // Load the session data
    if (fixture->filename)
        url = fixture->filename;
    sess = qof_session_new();
    qof_session_begin (sess, url, FALSE, TRUE, TRUE);
    if (sess && qof_session_get_error(sess) != ERR_BACKEND_NO_ERR)
    {
        g_warning ("Session Error: %d, %s", qof_session_get_error(sess),
                   qof_session_get_error_message(sess));
        g_test_message ("DB Session Creation Failed");
        g_assert (FALSE);
        goto cleanup;
    }
    qof_session_swap_data (fixture->session, sess);
    qof_session_save (sess, NULL);
    qbe = qof_session_get_backend (sess);
    book = qof_session_get_book (sess);
    qof_book_begin_edit (book);
    gnc_sql_set_table_version ((GncSqlBackend*)qbe,
                               "Gnucash", GNUCASH_RESAVE_VERSION - 1);
    qof_book_commit_edit (book);
    qof_session_end (sess);
    qof_session_destroy (sess);
    sess = qof_session_new();
    qof_session_begin (sess, url, TRUE, FALSE, FALSE);
    qof_session_load (sess, NULL);
    err = qof_session_pop_error (sess);
    g_assert_cmpint (err, ==, ERR_SQL_DB_TOO_OLD);
    qbe = qof_session_get_backend (sess);
    book = qof_session_get_book (sess);
    qof_book_begin_edit (book);
    gnc_sql_set_table_version ((GncSqlBackend*)qbe,
                               "Gnucash", ourversion);
    gnc_sql_set_table_version ((GncSqlBackend*)qbe,
                               "Gnucash-Resave", ourversion + 1);
    qof_book_commit_edit (book);
    qof_session_end (sess);
    qof_session_destroy (sess);
    sess = qof_session_new();
    qof_session_begin (sess, url, TRUE, FALSE, FALSE);
    qof_session_load (sess, NULL);
    qof_session_ensure_all_data_loaded (sess);
    err = qof_session_pop_error (sess);
    g_assert_cmpint (err, ==, ERR_SQL_DB_TOO_NEW);
cleanup:
    qbe = qof_session_get_backend (sess);
    book = qof_session_get_book (sess);
    qof_book_begin_edit (book);
    gnc_sql_set_table_version ((GncSqlBackend*)qbe,
                               "Gnucash-Resave", GNUCASH_RESAVE_VERSION);
    qof_book_commit_edit (book);
    qof_session_end (sess);
    qof_session_destroy (sess);
}

static void
test_dbi_business_store_and_reload (Fixture *fixture, gconstpointer pData)
{
    QofSession* session_2;
    QofSession* session_3;
    const gchar* url = (gchar*)pData;

    gchar *msg = "[gnc_dbi_unlock()] There was no lock entry in the Lock table";
    gchar *log_domain = "gnc.backend.dbi";
    guint loglevel = G_LOG_LEVEL_WARNING | G_LOG_FLAG_FATAL;
    TestErrorStruct *check = test_error_struct_new (log_domain, loglevel, msg);
    if (fixture->filename)
        url = fixture->filename;
    // Save the session data
    session_2 = qof_session_new();
    qof_session_begin (session_2, url, FALSE, TRUE, TRUE);
    qof_session_swap_data (fixture->session, session_2);
    qof_session_save (session_2, NULL);

    // Reload the session data
    session_3 = qof_session_new();
    qof_session_begin (session_3, url, TRUE, FALSE, FALSE);
    qof_session_load (session_3, NULL);

    // Compare with the original data
    compare_business_books (qof_session_get_book (session_2), qof_session_get_book (session_3));
    qof_session_end (session_2);
    qof_session_destroy (session_2);

    fixture->hdlrs = test_log_set_fatal_handler (fixture->hdlrs, check,
                     (GLogFunc)test_checked_handler);
    qof_session_end (session_3);
    qof_session_destroy (session_3);
}

static void
create_dbi_test_suite (gchar *dbm_name, gchar *url)
{
    gchar *subsuite = g_strdup_printf ("%s/%s", suitename, dbm_name);
    GNC_TEST_ADD (subsuite, "store_and_reload", Fixture, url, setup,
                  test_dbi_store_and_reload, teardown);
    GNC_TEST_ADD (subsuite, "safe_save", Fixture, url, setup_memory,
                  test_dbi_safe_save, teardown);
    GNC_TEST_ADD (subsuite, "version_control", Fixture, url, setup_memory,
                  test_dbi_version_control, teardown);
    GNC_TEST_ADD (subsuite, "business_store_and_reload", Fixture, url,
                  setup_business, test_dbi_version_control, teardown);
    g_free (subsuite);

}

void
test_suite_gnc_backend_dbi (void)
{
    dbi_driver driver = NULL;
    GList *drivers = NULL;
    #if HAVE_LIBDBI_R
    if (dbi_instance == NULL)
      dbi_initialize_r (NULL, &dbi_instance);
    while ((driver = dbi_driver_list_r (driver, dbi_instance)))
    #else
    dbi_initialize (NULL);
    while ((driver = dbi_driver_list (driver)))
    #endif
    {
        drivers = g_list_prepend (drivers,
                                  (gchar*)dbi_driver_get_name (driver));
    }
    if (g_list_find_custom (drivers, "sqlite3", (GCompareFunc)g_strcmp0))
        create_dbi_test_suite ("sqlite3", "sqlite3");
    if (strlen (TEST_MYSQL_URL) > 0 &&
            g_list_find_custom (drivers, "mysql", (GCompareFunc)g_strcmp0))
        create_dbi_test_suite ("mysql", TEST_MYSQL_URL);
    if (strlen (TEST_PGSQL_URL) > 0 &&
            g_list_find_custom (drivers, "pgsql", (GCompareFunc)g_strcmp0))
    {
        g_setenv ("PGOPTIONS", "-c client_min_messages=WARNING", FALSE);
        create_dbi_test_suite ("postgres", TEST_PGSQL_URL);
    }

}
