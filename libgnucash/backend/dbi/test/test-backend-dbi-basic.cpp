/*
 * utest-backend-dbi-basic.c
 *
 *  Created on: 2011-04-23
 *      Author: phil
 */
/********************************************************************\
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#include <kvp-frame.hpp>

extern "C"
{
#include <config.h>

#include <sys/types.h>
#include <unistd.h>
#include <glib/gstdio.h>
#include <glib/gi18n.h>

#include <qof.h>
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
    /* For version_control */
#include <gnc-prefs.h>
}
/* For test_conn_index_functions */
#include "../gnc-backend-dbi.hpp"
extern "C"
{
#include <unittest-support.h>
#include <test-stuff.h>
}

#include <string>
#include <vector>
#include <algorithm>

#include "test-dbi-stuff.h"
#include "test-dbi-business-stuff.h"

#if LIBDBI_VERSION >= 900
#define HAVE_LIBDBI_R 1
static dbi_inst dbi_instance = NULL;
#else
#define HAVE_LIBDBI_R 0
#endif

static const gchar* suitename = "/backend/dbi";
void test_suite_gnc_backend_dbi (void);

using StrVec = std::vector<std::string>;

typedef struct
{
    QofSession* session;
    gchar* filename;
    GSList* hdlrs;
} Fixture;

static void
setup (Fixture* fixture, gconstpointer pData)
{
    gchar* url = (gchar*)pData;
    fixture->session = qof_session_new ();
    /* When running distcheck the source directory is read-only, which
     * prevents creating the lock file. Force the session to get
     * around that.
     */
    qof_session_begin (fixture->session, DBI_TEST_XML_FILENAME, TRUE,
                       FALSE, TRUE);
    g_assert_cmpint (qof_session_get_error (fixture->session), == ,
                     ERR_BACKEND_NO_ERR);
    qof_session_load (fixture->session, NULL);

    if (g_strcmp0 (url, "sqlite3") == 0)
        fixture->filename = g_strdup_printf ("/tmp/test-sqlite-%d", getpid ());
    else
        fixture->filename = NULL;
}

static void
setup_memory (Fixture* fixture, gconstpointer pData)
{
    QofSession* session = qof_session_new ();
    gchar* url = (gchar*)pData;
    QofBook* book;
    Account* root, *acct1, *acct2;
    Transaction* tx;
    Split* spl1, *spl2;
    gnc_commodity_table* table;
    gnc_commodity* currency;

    session = qof_session_new ();
    book = qof_session_get_book (session);
    root = gnc_book_get_root_account (book);

    table = gnc_commodity_table_get_table (book);
    currency = gnc_commodity_table_lookup (table, GNC_COMMODITY_NS_CURRENCY,
                                           "CAD");

    acct1 = xaccMallocAccount (book);
    xaccAccountSetType (acct1, ACCT_TYPE_BANK);
    xaccAccountSetName (acct1, "Bank 1");
    xaccAccountSetCommodity (acct1, currency);

    auto frame = qof_instance_get_slots (QOF_INSTANCE (acct1));
    frame->set ({"int64-val"}, new KvpValue (INT64_C (100)));
    frame->set ({"double-val"}, new KvpValue (3.14159));
    frame->set ({"numeric-val"}, new KvpValue (gnc_numeric_zero ()));
    frame->set ({"timespec-val"}, new KvpValue (timespec_now ()));
    frame->set ({"string-val"}, new KvpValue ("abcdefghijklmnop"));
    auto guid = qof_instance_get_guid (QOF_INSTANCE (acct1));
    frame->set ({"guid-val"}, new KvpValue (const_cast<GncGUID*> (guid_copy (
            guid))));

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
        fixture->filename = g_strdup_printf ("/tmp/test-sqlite-%d", getpid ());
    else
        fixture->filename = NULL;
}

static void
setup_business (Fixture* fixture, gconstpointer pData)
{
    QofSession* session = qof_session_new ();
    gchar* url = (gchar*)pData;
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
    currency = gnc_commodity_table_lookup (table, GNC_COMMODITY_NS_CURRENCY,
                                           "CAD");

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
    tte = gncTaxTableEntryCreate ();
    gncTaxTableEntrySetAccount (tte, acct1);
    gncTaxTableEntrySetType (tte, GNC_AMT_TYPE_VALUE);
    gncTaxTableEntrySetAmount (tte, gnc_numeric_zero ());
    gncTaxTableAddEntry (tt, tte);
    tte = gncTaxTableEntryCreate ();
    gncTaxTableEntrySetAccount (tte, acct2);
    gncTaxTableEntrySetType (tte, GNC_AMT_TYPE_PERCENT);
    gncTaxTableEntrySetAmount (tte, gnc_numeric_zero ());
    gncTaxTableAddEntry (tt, tte);

    cust = gncCustomerCreate (book);
    gncCustomerSetID (cust, "0001");
    gncCustomerSetName (cust, "MyCustomer");
    gncCustomerSetNotes (cust, "Here are some notes");
    gncCustomerSetCurrency (cust, currency);
    addr = gncAddressCreate (book, QOF_INSTANCE (cust));
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
        fixture->filename = g_strdup_printf ("/tmp/test-sqlite-%d", getpid ());
    else
        fixture->filename = NULL;
}

static void
destroy_database (gchar* url)
{
    gchar* protocol = NULL;
    gchar* host = NULL;
    gchar* dbname = NULL;
    gchar* username = NULL;
    gchar* password = NULL;
    gchar* basename = NULL;
    gint portnum = 0;
    gchar* port = NULL;
    auto pgsql = "pgsql";
    dbi_conn conn = NULL;
    auto errfmt = "Unable to delete tables in %s: %s";
    gint fail = 0;
    dbi_result tables;
    StrVec tblnames;

    gnc_uri_get_components (url, &protocol, &host, &portnum,
                            &username, &password, &dbname);
    if (g_strcmp0 (protocol, "postgres") == 0)
#if HAVE_LIBDBI_R
        conn = dbi_conn_new_r (pgsql, dbi_instance);
#else
        conn = dbi_conn_new (pgsql);
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
        const gchar* error;
        gint errnum = dbi_conn_error (conn, &error);
        g_printf (errfmt, url, error);
        dbi_conn_close (conn);
        return;
    }
    tables = dbi_conn_get_table_list (conn, dbname, NULL);
    while (dbi_result_next_row (tables) != 0)
    {
        const std::string table{dbi_result_get_string_idx (tables, 1)};
        tblnames.push_back(table);
    }
    dbi_result_free (tables);
    std::for_each(tblnames.begin(), tblnames.end(),
                 [conn](std::string table) {
                     std::string query{"DROP TABLE "};
                     query += table;
                     dbi_result rslt = dbi_conn_query (conn, query.c_str());
                  });
}

static void
teardown (Fixture* fixture, gconstpointer pData)
{
    auto lockfile = g_strdup_printf ("%s/test-dbi.xml.LCK",
                                     g_path_get_dirname (DBI_TEST_XML_FILENAME));
    auto msg =
        g_strdup_printf ("[GncXmlBackend::session_end()] Error on g_unlink(%s): 2: No such file or directory",
                         lockfile);
    auto logdomain = "gnc.backend";
    auto loglevel = static_cast<GLogLevelFlags> (G_LOG_LEVEL_WARNING |
                                                 G_LOG_FLAG_FATAL);
    TestErrorStruct* check = test_error_struct_new (logdomain, loglevel, msg);
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
    test_clear_error_list ();
}

#if 0 //temporarily disable test pending refactor.
static void
test_conn_index_functions (QofBackend* qof_be)
{
    GncDbiBackend* dbi_be = reinterpret_cast<decltype(dbi_be)>(qof_be);

    auto index_list = conn->provider()->get_index_list (dbi_be->conn);
    g_test_message ("Returned from index list\n");
    g_assert_cmpint (index_list.size(), == , 4);
    for (auto index : index_list)
    {
        const char* errmsg;
        conn->provider()->drop_index (dbi_be->conn, index);
        g_assert (DBI_ERROR_NONE == dbi_conn_error (conn->conn(), &errmsg));
    }

}
#endif
/* Given a synthetic session, use the same logic as
 * QofSession::save_as to save it to a specified sql url, then load it
 * back and compare. */
static void
test_dbi_store_and_reload (Fixture* fixture, gconstpointer pData)
{

    const gchar* url = (const gchar*)pData;
    QofSession* session_2;
    QofSession* session_3;

    auto msg = "[GncDbiSqlConnection::unlock_database()] There was no lock entry in the Lock table";
    auto log_domain = nullptr;
    auto loglevel = static_cast<GLogLevelFlags> (G_LOG_LEVEL_WARNING |
                                                 G_LOG_FLAG_FATAL);
    TestErrorStruct* check = test_error_struct_new (log_domain, loglevel, msg);
    fixture->hdlrs = test_log_set_fatal_handler (fixture->hdlrs, check,
                                                 (GLogFunc)test_checked_handler);
    if (fixture->filename)
        url = fixture->filename;

    // Save the session data
    session_2 = qof_session_new ();
    qof_session_begin (session_2, url, FALSE, TRUE, TRUE);
    g_assert (session_2 != NULL);
    g_assert_cmpint (qof_session_get_error (session_2), == , ERR_BACKEND_NO_ERR);
    qof_session_swap_data (fixture->session, session_2);
    qof_session_save (session_2, NULL);
    g_assert (session_2 != NULL);
    g_assert_cmpint (qof_session_get_error (session_2), == , ERR_BACKEND_NO_ERR);

    // Reload the session data
    session_3 = qof_session_new ();
    g_assert (session_3 != NULL);
    qof_session_begin (session_3, url, TRUE, FALSE, FALSE);
    g_assert (session_3 != NULL);
    g_assert_cmpint (qof_session_get_error (session_3), == , ERR_BACKEND_NO_ERR);
    qof_session_load (session_3, NULL);
    g_assert (session_3 != NULL);
    g_assert_cmpint (qof_session_get_error (session_3), == , ERR_BACKEND_NO_ERR);
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
test_dbi_safe_save (Fixture* fixture, gconstpointer pData)
{
    auto url = (gchar*)pData;
    QofSession* session_1 = NULL, *session_2 = NULL;

    auto msg = "[GncDbiSqlConnection::unlock_database()] There was no lock entry in the Lock table";
    auto log_domain = nullptr;
    auto loglevel = static_cast<GLogLevelFlags> (G_LOG_LEVEL_WARNING |
                                                 G_LOG_FLAG_FATAL);
    TestErrorStruct* check = test_error_struct_new (log_domain, loglevel, msg);

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
    if (session_1 && qof_session_get_error (session_1) != ERR_BACKEND_NO_ERR)
    {
        g_warning ("Session Error: %s",
                   qof_session_get_error_message (session_1));
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
        g_warning ("Session Error: %d, %s", qof_session_get_error (session_2),
                   qof_session_get_error_message (session_2));
        g_test_message ("DB Session re-creation Failed");
        g_assert (FALSE);
        goto cleanup;
    }
    qof_session_load (session_2, NULL);
    compare_books (qof_session_get_book (session_1),
                   qof_session_get_book (session_2));
//    auto qof_be = qof_book_get_backend (qof_session_get_book (session_2));
//    test_conn_index_functions (qof_be);

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
test_dbi_version_control (Fixture* fixture, gconstpointer pData)
{
    auto url = (gchar*)pData;
    QofSession* sess;
    QofBook* book;
    QofBackendError err;
    gint ourversion = gnc_prefs_get_long_version ();
    GncSqlBackend* sql_be = nullptr;

    // Load the session data
    if (fixture->filename)
        url = fixture->filename;
    sess = qof_session_new ();
    qof_session_begin (sess, url, FALSE, TRUE, TRUE);
    if (sess && qof_session_get_error (sess) != ERR_BACKEND_NO_ERR)
    {
        g_warning ("Session Error: %d, %s", qof_session_get_error (sess),
                   qof_session_get_error_message (sess));
        g_test_message ("DB Session Creation Failed");
        g_assert (FALSE);
        goto cleanup;
    }
    qof_session_swap_data (fixture->session, sess);
    qof_session_save (sess, NULL);
    sql_be = reinterpret_cast<decltype(sql_be)>(qof_session_get_backend (sess));
    book = qof_session_get_book (sess);
    qof_book_begin_edit (book);
    sql_be->set_table_version ("Gnucash", GNUCASH_RESAVE_VERSION - 1);
    qof_book_commit_edit (book);
    qof_session_end (sess);
    qof_session_destroy (sess);
    sess = qof_session_new ();
    qof_session_begin (sess, url, TRUE, FALSE, FALSE);
    qof_session_load (sess, NULL);
    err = qof_session_pop_error (sess);
    g_assert_cmpint (err, == , ERR_SQL_DB_TOO_OLD);
    sql_be = reinterpret_cast<decltype(sql_be)>(qof_session_get_backend (sess));
    book = qof_session_get_book (sess);
    qof_book_begin_edit (book);
    sql_be->set_table_version ("Gnucash", ourversion);
    sql_be->set_table_version ("Gnucash-Resave", ourversion + 1);
    qof_book_commit_edit (book);
    qof_session_end (sess);
    qof_session_destroy (sess);
    sess = qof_session_new ();
    qof_session_begin (sess, url, TRUE, FALSE, FALSE);
    qof_session_load (sess, NULL);
    qof_session_ensure_all_data_loaded (sess);
    err = qof_session_pop_error (sess);
    g_assert_cmpint (err, == , ERR_SQL_DB_TOO_NEW);
cleanup:
    sql_be = reinterpret_cast<decltype(sql_be)>(qof_session_get_backend (sess));
    book = qof_session_get_book (sess);
    qof_book_begin_edit (book);
    sql_be->set_table_version ("Gnucash-Resave", GNUCASH_RESAVE_VERSION);
    qof_book_commit_edit (book);
    qof_session_end (sess);
    qof_session_destroy (sess);
}

static void
test_dbi_business_store_and_reload (Fixture* fixture, gconstpointer pData)
{
    QofSession* session_2;
    QofSession* session_3;
    const gchar* url = (gchar*)pData;

    auto msg = "[GncDbiSqlConnection::unlock_database()] There was no lock entry in the Lock table";
    auto log_domain = nullptr;
    auto loglevel = static_cast<GLogLevelFlags> (G_LOG_LEVEL_WARNING |
                                                 G_LOG_FLAG_FATAL);
    TestErrorStruct* check = test_error_struct_new (log_domain, loglevel, msg);
    if (fixture->filename)
        url = fixture->filename;
    // Save the session data
    session_2 = qof_session_new ();
    qof_session_begin (session_2, url, FALSE, TRUE, TRUE);
    qof_session_swap_data (fixture->session, session_2);
    qof_session_save (session_2, NULL);

    // Reload the session data
    session_3 = qof_session_new ();
    qof_session_begin (session_3, url, TRUE, FALSE, FALSE);
    qof_session_load (session_3, NULL);

    // Compare with the original data
    compare_business_books (qof_session_get_book (session_2),
                            qof_session_get_book (session_3));
    qof_session_end (session_2);
    qof_session_destroy (session_2);

    fixture->hdlrs = test_log_set_fatal_handler (fixture->hdlrs, check,
                                                 (GLogFunc)test_checked_handler);
    qof_session_end (session_3);
    qof_session_destroy (session_3);
}

static void
test_adjust_sql_options_string (void)
{
    const char* in[] = {
        "NO_ZERO_DATE",
        "NO_ZERO_DATE,something_else",
        "something,NO_ZERO_DATE",
        "something,NO_ZERO_DATE,something_else",
        "NO_ZERO_DATExx",
        "NO_ZERO_DATExx,something_ else",
        "something,NO_ZERO_DATExx",
        "something,NO_ZERO_DATExx,something_ else",
        "fred,jim,john"
    };
    const char* out[] = {
        "",
        "something_else",
        "something",
        "something,something_else",
        "NO_ZERO_DATExx",
        "NO_ZERO_DATExx,something_ else",
        "something,NO_ZERO_DATExx",
        "something,NO_ZERO_DATExx,something_ else",
        "fred,jim,john"
    };

    for (size_t i = 0; i < sizeof(in) / sizeof(char*); i++)
    {
        std::string adjusted_str = adjust_sql_options_string(in[i]);
        g_assert_cmpstr(out[i], ==, adjusted_str.c_str());
    }
}

static void
create_dbi_test_suite (const char* dbm_name, const char* url)
{
    auto subsuite = g_strdup_printf ("%s/%s", suitename, dbm_name);
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
    dbi_driver driver = nullptr;
    StrVec drivers;
#if HAVE_LIBDBI_R
    if (dbi_instance == NULL)
        dbi_initialize_r (NULL, &dbi_instance);
    while ((driver = dbi_driver_list_r (driver, dbi_instance)))
#else
    dbi_initialize (NULL);
    while ((driver = dbi_driver_list (driver)))
#endif
    {
        drivers.push_back(dbi_driver_get_name (driver));
    }
    for (auto name : drivers)
    {
        if (name == "sqlite3")
            create_dbi_test_suite ("sqlite3", "sqlite3");
        if (strlen (TEST_MYSQL_URL) > 0 && name == "mysql")
            create_dbi_test_suite ("mysql", TEST_MYSQL_URL);
        if (strlen (TEST_PGSQL_URL) > 0 && name == "pgsql")
        {
            g_setenv ("PGOPTIONS", "-c client_min_messages=WARNING", FALSE);
            create_dbi_test_suite ("postgres", TEST_PGSQL_URL);
        }
    }

    GNC_TEST_ADD_FUNC( suitename, "adjust sql options string localtime", 
        test_adjust_sql_options_string );
}
