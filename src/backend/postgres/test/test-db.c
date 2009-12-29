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
#include <glib.h>
#include <libpq-fe.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "AccountP.h"
#include "qof.h"
#include "PostgresBackend.h"
#include "TransactionP.h"
#include "TransLog.h"
#include "gnc-engine.h"
#include "gncquery.h"
#include "test-stuff.h"
#include "test-engine-stuff.h"

#define PG_LIB_NAME "gnc-backend-postgres"

static QofLogModule log_module = GNC_MOD_TEST;

struct _dbinfo
{
    char *host;
    char *port;
    char *dbname;
    char *mode;
    PGconn *conn;
};

typedef struct _dbinfo DbInfo;

static void
save_xml_file(QofSession * session, const char *filename_base)
{
    QofBackendError io_err;
    gchar *cwd, *filename;

    g_return_if_fail(session && filename_base);

    cwd = g_get_current_dir();
    filename = g_strdup_printf("file:/%s/%s", cwd, filename_base);

    qof_session_begin(session, filename, FALSE, TRUE);

    io_err = qof_session_get_error(session);
    g_return_if_fail(io_err == ERR_BACKEND_NO_ERR);

    qof_session_save(session, NULL);
    io_err = qof_session_get_error(session);
    g_return_if_fail(io_err == ERR_BACKEND_NO_ERR);

    qof_session_end(session);
    io_err = qof_session_get_error(session);
    g_return_if_fail(io_err == ERR_BACKEND_NO_ERR);

    g_free(filename);
    g_free(cwd);
}

static void
save_xml_files(QofSession * session_1, QofSession * session_2)
{
    g_return_if_fail(session_1 && session_2);

    save_xml_file(session_1, "test_file_1");
    save_xml_file(session_2, "test_file_2");
}

static char *
db_file_url(DbInfo *dbinfo)
{
    char *db_socket_dir;
    gchar *url;

    g_return_val_if_fail(dbinfo->dbname && dbinfo->mode, NULL);

    if ((!g_ascii_strncasecmp(dbinfo->port, "7777", 4)) &&
            (!g_ascii_strncasecmp(dbinfo->host, "localhost", 8)))
    {
        /* TEST_DB_SOCKET_DIR must be an absolute path */
        db_socket_dir = getenv("TEST_DB_SOCKET_DIR");
        if (!db_socket_dir)
            g_warning("Couldn't getenv TEST_DB_SOCKET_DIR");
        g_return_val_if_fail(db_socket_dir, NULL);
        url = g_strdup_printf("postgres://%s:7777/%s?mode=%s",
                              db_socket_dir, dbinfo->dbname, dbinfo->mode);
    }
    else
    {
        url = g_strdup_printf("postgres://%s:%s/%s?mode=%s",
                              dbinfo->host, dbinfo->port,
                              dbinfo->dbname, dbinfo->mode);
    }
    return url;
}

static gboolean
save_db_file(QofSession * session, DbInfo *dbinfo)
{
    QofBackendError io_err;
    char *filename;

    g_return_val_if_fail(session && dbinfo->dbname && dbinfo->mode, FALSE);

    filename = db_file_url(dbinfo);
    qof_session_begin(session, filename, FALSE, TRUE);
    io_err = qof_session_get_error(session);
    if (!do_test_args(io_err == ERR_BACKEND_NO_ERR,
                      "Beginning db session",
                      __FILE__, __LINE__,
                      "can't begin session for %s in mode %s", dbinfo->dbname, dbinfo->mode))
        return FALSE;

    qof_session_save(session, NULL);
    io_err = qof_session_get_error(session);
    if (!do_test_args(io_err == ERR_BACKEND_NO_ERR,
                      "Saving db session",
                      __FILE__, __LINE__,
                      "can't save session for %s in mode %s", dbinfo->dbname, dbinfo->mode))
        return FALSE;

    qof_session_end(session);
    io_err = qof_session_get_error(session);
    if (!do_test_args(io_err == ERR_BACKEND_NO_ERR,
                      "Ending db session",
                      __FILE__, __LINE__,
                      "can't end session for %s in mode %s", dbinfo->dbname, dbinfo->mode))
        return FALSE;

    do_test(qof_session_get_url(session) == NULL, "session url not NULL");

    g_free(filename);

    return TRUE;
}

static gboolean
load_db_file(QofSession * session, DbInfo *dbinfo, gboolean end_session)
{
    QofBackendError io_err;
    QofBook *book;
    PGBackend *be;
    char *filename;

    g_return_val_if_fail(session && dbinfo->dbname && dbinfo->mode, FALSE);

    filename = db_file_url(dbinfo);

    qof_session_begin(session, filename, FALSE, FALSE);
    book = qof_session_get_book(session);
    be = (PGBackend *)qof_book_get_backend(book);
    dbinfo->conn = be->connection;

    io_err = qof_session_get_error(session);
    if (!do_test_args(io_err == ERR_BACKEND_NO_ERR,
                      "Beginning db session",
                      __FILE__, __LINE__,
                      "can't begin session for %s in mode %s",
                      dbinfo->dbname, dbinfo->mode))
        return FALSE;

    qof_session_load(session, NULL);
    io_err = qof_session_get_error(session);
    if (!do_test_args(io_err == ERR_BACKEND_NO_ERR,
                      "Loading db session",
                      __FILE__, __LINE__,
                      "can't load session for %s in mode %s",
                      dbinfo->dbname, dbinfo->mode))
        return FALSE;

    if (end_session)
    {
        qof_session_end(session);
        io_err = qof_session_get_error(session);
        if (!do_test_args(io_err == ERR_BACKEND_NO_ERR,
                          "Ending db session",
                          __FILE__, __LINE__,
                          "can't end session for %s in mode %s",
                          dbinfo->dbname, dbinfo->mode))
            return FALSE;

        do_test(qof_session_get_url(session) == NULL, "session url not NULL");
    }

    g_free(filename);

    return TRUE;
}

static gboolean
test_access(DbInfo *dbinfo, gboolean multi_user)
{
    QofBackendError io_err;
    QofSession *session_1;
    QofSession *session_2;
    char *filename;

    g_return_val_if_fail(dbinfo->dbname && dbinfo->mode, FALSE);

    filename = db_file_url(dbinfo);

    session_1 = qof_session_new();

    qof_session_begin(session_1, filename, FALSE, FALSE);
    io_err = qof_session_get_error(session_1);
    if (!do_test_args(io_err == ERR_BACKEND_NO_ERR,
                      "Beginning db session",
                      __FILE__, __LINE__,
                      "can't begin session for %s in mode %s",
                      dbinfo->dbname, dbinfo->mode))
        return FALSE;

    session_2 = qof_session_new();

    qof_session_begin(session_2, filename, FALSE, FALSE);
    io_err = qof_session_get_error(session_2);

    if (multi_user)
    {
        if (!do_test_args(io_err == ERR_BACKEND_NO_ERR,
                          "Beginning second multi-user db session",
                          __FILE__, __LINE__,
                          "can't begin second session for %s in mode %s",
                          dbinfo->dbname, dbinfo->mode))
            return FALSE;
    }
    else
    {
        if (!do_test_args(io_err != ERR_BACKEND_NO_ERR,
                          "Beginning second single-user db session",
                          __FILE__, __LINE__,
                          "began second session for %s in mode %s",
                          dbinfo->dbname, dbinfo->mode))
            return FALSE;
    }

    qof_session_destroy(session_1);
    qof_session_destroy(session_2);

    return TRUE;
}

static void
mark_account_commodities(Account * a, gpointer data)
{
    GHashTable *hash = data;

    g_hash_table_insert(hash, xaccAccountGetCommodity(a), hash);
}

static int
mark_transaction_commodities(Transaction * t, void *data)
{
    GHashTable *hash = data;

    g_hash_table_insert(hash, xaccTransGetCurrency(t), hash);

    return 0;
}

static gboolean
mark_price_commodities(GNCPrice * p, gpointer data)
{
    GHashTable *hash = data;

    g_hash_table_insert(hash, gnc_price_get_commodity(p), hash);
    g_hash_table_insert(hash, gnc_price_get_currency(p), hash);

    return TRUE;
}

typedef struct
{
    GHashTable *hash;
    GList *to_delete;
} CommodityDeleteInfo;

static gboolean
add_commodity_to_delete(gnc_commodity * com, gpointer data)
{
    CommodityDeleteInfo *cdi = data;

    if (!g_hash_table_lookup(cdi->hash, com) && !gnc_commodity_is_iso(com))
        cdi->to_delete = g_list_prepend(cdi->to_delete, com);

    return TRUE;
}

static void
remove_unneeded_commodities(QofSession * session)
{
    CommodityDeleteInfo cdi;
    QofBook *book;
    GList *node;

    g_return_if_fail(session);

    cdi.hash = g_hash_table_new(g_direct_hash, g_direct_equal);

    book = qof_session_get_book(session);

    gnc_account_foreach_descendant(gnc_book_get_root_account(book),
                                   mark_account_commodities, cdi.hash);

    xaccAccountTreeForEachTransaction(gnc_book_get_root_account(book),
                                      mark_transaction_commodities, cdi.hash);

    gnc_pricedb_foreach_price(gnc_book_get_pricedb(book),
                              mark_price_commodities, cdi.hash, FALSE);

    cdi.to_delete = NULL;

    gnc_commodity_table_foreach_commodity(gnc_book_get_commodity_table(book),
                                          add_commodity_to_delete, &cdi);

    for (node = cdi.to_delete; node; node = node->next)
        gnc_commodity_table_remove(gnc_book_get_commodity_table(book),
                                   node->data);

    g_list_free(cdi.to_delete);
    g_hash_table_destroy(cdi.hash);
}

static Query *
make_get_all_query(QofSession * session)
{
    Query *q;

    g_return_val_if_fail(session, NULL);

    q = xaccMallocQuery();

    xaccQuerySetBook(q, qof_session_get_book(session));

    xaccQueryAddClearedMatch(q,
                             CLEARED_NO |
                             CLEARED_CLEARED |
                             CLEARED_RECONCILED |
                             CLEARED_FROZEN | CLEARED_VOIDED, QOF_QUERY_AND);

    return q;
}

static void
multi_user_get_everything(QofSession * session, QofSession * base)
{
    Query *q;

    g_return_if_fail(session);

    q = make_get_all_query(session);

    xaccQueryGetSplits(q);

    xaccFreeQuery(q);

    /* load in prices from base */
    if (base)
        gnc_pricedb_equal(gnc_book_get_pricedb(qof_session_get_book(base)),
                          gnc_book_get_pricedb(qof_session_get_book
                                               (session)));
}

static gboolean
test_updates(QofSession * session, DbInfo *dbinfo, gboolean multi_user)
{
    QofBackendError io_err;
    QofSession *session_2;
    char *filename;
    char str1[GUID_ENCODING_LENGTH+1];
    char str2[GUID_ENCODING_LENGTH+1];
    gboolean ok;

    g_return_val_if_fail(session && dbinfo->dbname && dbinfo->mode, FALSE);

    filename = db_file_url(dbinfo);

    qof_session_begin(session, filename, FALSE, FALSE);
    io_err = qof_session_get_error(session);
    if (!do_test_args(io_err == ERR_BACKEND_NO_ERR,
                      "Beginning db update session",
                      __FILE__, __LINE__,
                      "can't begin session for %s in mode %s", dbinfo->dbname, dbinfo->mode))
        return FALSE;

    make_random_changes_to_session(session);

    if (!multi_user)
    {
        qof_session_end(session);
        io_err = qof_session_get_error(session);
        if (!do_test_args(io_err == ERR_BACKEND_NO_ERR,
                          "Ending db session",
                          __FILE__, __LINE__,
                          "can't end session for %s in mode %s",
                          dbinfo->dbname, dbinfo->mode))
            return FALSE;
    }

    session_2 = qof_session_new();

    if (!load_db_file(session_2, dbinfo, !multi_user))
        return FALSE;

    if (multi_user)
        multi_user_get_everything(session_2, session);

    remove_unneeded_commodities(session);
    remove_unneeded_commodities(session_2);

    ok = qof_book_equal(qof_session_get_book(session),
                        qof_session_get_book(session_2));

    guid_to_string_buff(qof_book_get_guid(qof_session_get_book(session)), str1);
    guid_to_string_buff(qof_book_get_guid(qof_session_get_book(session_2)), str2);
    do_test_args(ok, "Books equal after update", __FILE__, __LINE__,
                 "Books not equal for session %s in mode %si\n"
                 "book 1: %s,\nbook 2: %s",
                 dbinfo->dbname, dbinfo->mode, str1, str2);

    if (multi_user)
    {
        qof_session_end(session);
        io_err = qof_session_get_error(session);
        if (!do_test_args(io_err == ERR_BACKEND_NO_ERR,
                          "Ending db session",
                          __FILE__, __LINE__,
                          "can't end session for %s in mode %s",
                          dbinfo->dbname, dbinfo->mode))
            return FALSE;

        qof_session_end(session_2);
        io_err = qof_session_get_error(session_2);
        if (!do_test_args(io_err == ERR_BACKEND_NO_ERR,
                          "Ending db session",
                          __FILE__, __LINE__,
                          "can't end session for %s in mode %s",
                          dbinfo->dbname, dbinfo->mode))
            return FALSE;
    }

    if (!ok)
    {
        save_xml_files(session, session_2);
        return FALSE;
    }

    qof_session_destroy(session_2);
    g_free(filename);

    return TRUE;
}

static gboolean
num_trans_helper(Transaction * trans, gpointer data)
{
    int *num = data;

    *num += 1;

    return TRUE;
}

static int
session_num_trans(QofSession * session)
{
    Account *root;
    QofBook *book;
    int num = 0;

    g_return_val_if_fail(session, 0);

    book = qof_session_get_book(session);
    root = gnc_book_get_root_account(book);

    xaccAccountTreeForEachTransaction(root, num_trans_helper, &num);

    return num;
}

typedef struct
{
    QofSession *session_base;
    DbInfo *dbinfo;
    gint loaded;
    gint total;
} QueryTestData;

static gboolean
test_raw_query(QofSession * session, Query * q)
{
    const char *sql_query_string;
    PGresult *result;
    QofBook *book;
    PGBackend *be;
    sqlQuery *sq;
    gboolean ok;
    QofQuery *qn = q;

    g_return_val_if_fail(session && q, FALSE);
    book = qof_session_get_book(session);
    be = (PGBackend *) qof_book_get_backend(book);

    if (qof_log_check(log_module, QOF_LOG_DEBUG))
        qof_query_print(qn);

    sq = sqlQuery_new();
    sql_query_string = sqlQuery_build(sq, q);

    result = PQexec(be->connection, sql_query_string);

    ok = (result && PQresultStatus(result) == PGRES_TUPLES_OK);
    if (!ok)
    {
        failure_args("Raw query failed",
                     __FILE__, __LINE__,
                     "Error: %s\nQuery: %s",
                     PQresultErrorMessage(result),
                     sql_query_string);
        /* failure("raw query failed: %s", sql_query_string); */
    }
    else
    {
        ok = ok && (PQntuples(result) == 1);
        if (!ok)
            failure_args("number returned test",
                         __FILE__, __LINE__,
                         "query returned %d tuples", PQntuples(result));
    }

    if (ok)
    {
        success("raw query succeeded");
    }

    PQclear(result);
    sql_Query_destroy(sq);

    return ok;
}

static gboolean
test_trans_query(Transaction * trans, gpointer data)
{
    QueryTestData *qtd = data;
    QofBackendError io_err;
    QofSession *session;
    char *filename;
    QofBook *book;
    GList *list;
    Query *q;

    filename = db_file_url(qtd->dbinfo);

    session = qof_session_new();

    qof_session_begin(session, filename, FALSE, FALSE);
    io_err = qof_session_get_error(session);
    if (!do_test_args(io_err == ERR_BACKEND_NO_ERR,
                      "Beginning db session",
                      __FILE__, __LINE__,
                      "can't begin session for %s", filename))
        return FALSE;

    qof_session_load(session, NULL);
    io_err = qof_session_get_error(session);
    if (!do_test_args(io_err == ERR_BACKEND_NO_ERR,
                      "Loading db session",
                      __FILE__, __LINE__,
                      "can't load session for %s", filename))
        return FALSE;

    book = qof_session_get_book(session);

    q = make_trans_query(trans, get_random_query_type() | GUID_QT);
    xaccQuerySetBook(q, book);

    if (!test_raw_query(session, q))
    {
        failure("raw query failed");
        return FALSE;
    }

    list = xaccQueryGetTransactions(q, QUERY_TXN_MATCH_ANY);
    if (g_list_length(list) != 1)
    {
        failure_args("test num returned", __FILE__, __LINE__,
                     "number of matching transactions %d not 1",
                     g_list_length(list));
        g_list_free(list);
        return FALSE;
    }

    qtd->loaded += session_num_trans(session);
    qtd->total += session_num_trans(qtd->session_base);

    if (!xaccTransEqual(trans, list->data, TRUE, TRUE, TRUE, FALSE))
    {
        failure("matching transaction is wrong");
        g_list_free(list);
        return FALSE;
    }

    success("found right transaction");

    xaccFreeQuery(q);
    qof_session_destroy(session);
    g_free(filename);
    g_list_free(list);

    return TRUE;
}

static gboolean
compare_balances(QofSession * session_1, QofSession * session_2)
{
    QofBook *book_1 = qof_session_get_book(session_1);
    QofBook *book_2 = qof_session_get_book(session_2);
    GList *list;
    GList *node;
    gboolean ok;

    g_return_val_if_fail(session_1, FALSE);
    g_return_val_if_fail(session_2, FALSE);

    ok = TRUE;

    list = gnc_account_get_descendants(gnc_book_get_root_account(book_1));
    for (node = list; node; node = node->next)
    {
        Account *account_1 = node->data;
        Account *account_2;

        account_2 = xaccAccountLookup(xaccAccountGetGUID(account_1), book_2);
        if (!account_2)
        {
            failure_args("", __FILE__, __LINE__,
                         "session_1 has account %s but not session_2",
                         guid_to_string(xaccAccountGetGUID(account_1)));
            return FALSE;
        }

        if (!gnc_numeric_equal(xaccAccountGetBalance(account_1),
                               xaccAccountGetBalance(account_2)))
        {
            failure_args("", __FILE__, __LINE__,
                         "balances not equal for account %s",
                         guid_to_string(xaccAccountGetGUID(account_1)));
            ok = FALSE;
        }

        if (!gnc_numeric_equal(xaccAccountGetClearedBalance(account_1),
                               xaccAccountGetClearedBalance(account_2)))
        {
            failure_args("", __FILE__, __LINE__,
                         "cleared balances not equal for account %s",
                         guid_to_string(xaccAccountGetGUID(account_1)));
            ok = FALSE;
        }

        if (!gnc_numeric_equal(xaccAccountGetReconciledBalance(account_1),
                               xaccAccountGetReconciledBalance(account_2)))
        {
            failure_args("", __FILE__, __LINE__,
                         "reconciled balances not equal for account %s",
                         guid_to_string(xaccAccountGetGUID(account_1)));
            ok = FALSE;
        }

        if (!ok)
            break;

        success("balances equal");
    }
    g_list_free(list);

    return ok;
}

static gboolean
test_queries(QofSession * session_base, DbInfo *dbinfo)
{
    QueryTestData qtd;
    Account *root;
    QofBook *book;
    gboolean ok;

    g_return_val_if_fail(dbinfo->dbname && dbinfo->mode, FALSE);

    book = qof_session_get_book(session_base);
    root = gnc_book_get_root_account(book);

    qtd.session_base = session_base;
    qtd.dbinfo = dbinfo;
    qtd.loaded = 0;
    qtd.total = 0;

    ok = xaccAccountTreeForEachTransaction(root, test_trans_query, &qtd);

#if 0
    g_warning("average percentage loaded = %3.2f%%",
              (qtd.loaded / (double)qtd.total) * 100.0);
#endif

    return ok;
}

typedef struct
{
    QofSession *session_1;
    QofSession *session_2;

    QofBook *book_1;
    QofBook *book_2;

    Account *root_1;
    Account *root_2;

    GList *accounts_1;
    GList *accounts_2;
} UpdateTestData;

static gboolean
test_trans_update(Transaction * trans, gpointer data)
{
    UpdateTestData *td = data;
    QofBackendError io_err;
    Transaction *trans_2;
    QofBook *book_1;
    QofBook *book_2;
    GUID guid;
    gboolean ok;

    /* FIXME remove */
    return TRUE;

    book_1 = qof_session_get_book(td->session_1);
    book_2 = qof_session_get_book(td->session_2);

    guid = *xaccTransGetGUID(trans);

    xaccTransBeginEdit(trans);
    make_random_changes_to_transaction_and_splits(book_1, trans,
            td->accounts_1);
    xaccTransCommitEdit(trans);

    io_err = qof_session_get_error(td->session_1);
    if (!do_test_args(io_err == ERR_BACKEND_NO_ERR,
                      "changing transaction in session 1",
                      __FILE__, __LINE__,
                      "error changing transaction: %d", io_err))
        return FALSE;

    trans = xaccTransLookup(&guid, book_1);
    trans_2 = xaccTransLookup(&guid, book_2);

    /* This should get rolled back. */
    if (trans_2)
    {
        xaccTransBeginEdit(trans_2);
        make_random_changes_to_transaction_and_splits(book_2, trans_2,
                td->accounts_2);
        xaccTransCommitEdit(trans_2);
    }

    trans_2 = xaccTransLookup(&guid, book_2);

    ok = xaccTransEqual(trans, trans_2, TRUE, TRUE, TRUE, FALSE);
    if (trans && trans_2)
        ok = ok && (qof_instance_compare_version(trans, trans_2));

    /*
       ok = ok && (qof_session_get_error (td->session_2) == ERR_BACKEND_MODIFIED);
     */

    if (!do_test_args(ok,
                      "test trans rollback",
                      __FILE__, __LINE__,
                      "transaction not rolled back properly"))
        return FALSE;

    return TRUE;
}

static gboolean
add_trans_helper(Transaction * trans, gpointer data)
{
    GList **list = data;

    *list = g_list_prepend(*list, trans);

    return TRUE;
}

static gboolean
drop_database(DbInfo *dbinfo)
{
    gchar *dropdb = NULL;
    int rc;

    if (!g_ascii_strncasecmp(dbinfo->port, "7777", 4))
    {
        dropdb = g_strdup_printf("dropdb -p %s %s",
                                 dbinfo->port, dbinfo->dbname);
    }
    else
    {
        dropdb = g_strdup_printf("dropdb -p %s -h %s %s",
                                 dbinfo->port, dbinfo->host,
                                 dbinfo->dbname);
    }

    /* Make sure everything is logged off */
    sleep(5);
    rc = system(dropdb);
    printf("Executed %s,\nreturn code was %d\n", dropdb, rc);
    if (rc)
    {
        printf("Please run the command\n"
               "\t%s\nwhen this process completes\n", dropdb);
    }
    g_free(dropdb);
    dropdb = NULL;
    return rc == 0 ? TRUE : FALSE;
}

static gboolean
test_updates_2(QofSession * session_base, DbInfo *dbinfo)
{
    UpdateTestData td;
    char *filename;
    GList *transes;
    GList *node;
    gboolean ok;

    g_return_val_if_fail(session_base && dbinfo->dbname && dbinfo->mode, FALSE);

    filename = db_file_url(dbinfo);

    if (!load_db_file(session_base, dbinfo, FALSE))
        return FALSE;

    multi_user_get_everything(session_base, NULL);

    td.session_1 = session_base;
    td.book_1 = qof_session_get_book(session_base);
    td.root_1 = gnc_book_get_root_account(td.book_1);
    td.accounts_1 = gnc_account_get_descendants(td.root_1);

    td.session_2 = qof_session_new();

    if (!load_db_file(td.session_2, dbinfo, FALSE))
        return FALSE;

    multi_user_get_everything(td.session_2, NULL);

    td.book_2 = qof_session_get_book(td.session_2);
    td.root_2 = gnc_book_get_root_account(td.book_2);
    td.accounts_2 = gnc_account_get_descendants(td.root_2);

    ok = TRUE;
    transes = NULL;
    xaccAccountTreeForEachTransaction(td.root_1, add_trans_helper, &transes);
    for (node = transes; node; node = node->next)
    {
        ok = test_trans_update(node->data, &td);
        if (!ok)
            return FALSE;
    }
    g_list_free(transes);

#if 0
    for (node = td.accounts_1; node; node = node->next)
    {
        Account *account_1 = node->data;
        Account *account_2 =
            xaccAccountLookup(xaccAccountGetGUID(account_1), td.book_2);

        make_random_changes_to_account(td.book_1, account_1);
        make_random_changes_to_account(td.book_2, account_2);

        ok = xaccAccountEqual(account_1, account_2, TRUE);
        if (account_1 && account_2)
            ok = ok && (account_1->version == account_2->version);

        ok = ok
             && (qof_session_get_error(td.session_2) == ERR_BACKEND_MODIFIED);

        if (!do_test_args(ok,
                          "test account rollback",
                          __FILE__, __LINE__,
                          "account not rolled back properly"))
            return FALSE;
    }
#endif

    {
        Account *account = get_random_account(td.book_1);
        Account *child = get_random_account(td.book_1);
        Transaction *trans = get_random_transaction(td.book_1);
        Query *q = xaccMallocQuery();
        int count = 0;

        xaccAccountBeginEdit(account);
        xaccAccountBeginEdit(child);
        gnc_account_append_child(td.root_1, account);
        gnc_account_append_child(account, child);
        xaccAccountCommitEdit(child);
        xaccAccountCommitEdit(account);

        xaccTransBeginEdit(trans);
        for (node = xaccTransGetSplitList(trans); node; node = node->next)
        {
            Split *split = node->data;

            xaccAccountInsertSplit(child, split);
            count++;
        }
        xaccTransCommitEdit(trans);

        xaccQueryAddGUIDMatch(q, xaccAccountGetGUID(child),
                              GNC_ID_ACCOUNT, QOF_QUERY_AND);

        xaccQuerySetBook(q, td.book_2);

        ok = (g_list_length(xaccQueryGetSplits(q)) == count);

        xaccFreeQuery(q);

        if (ok)
        {
            Transaction *trans_2;
            Account *account_2;
            Account *child_2;

            trans_2 = xaccTransLookup(xaccTransGetGUID(trans), td.book_2);
            account_2 =
                xaccAccountLookup(xaccAccountGetGUID(account), td.book_2);
            child_2 = xaccAccountLookup(xaccAccountGetGUID(child), td.book_2);

            ok = ok && xaccTransEqual(trans, trans_2, TRUE, TRUE, TRUE, FALSE);
            ok = ok && xaccAccountEqual(account, account_2, TRUE);
            ok = ok && xaccAccountEqual(child, child_2, TRUE);
        }

        if (!do_test_args(ok,
                          "test new account",
                          __FILE__, __LINE__,
                          "new accounts not loaded properly"))
            return FALSE;
    }

    qof_session_end(td.session_1);
    qof_session_end(td.session_2);
    qof_session_destroy(td.session_2);

    g_list_free(td.accounts_1);
    g_list_free(td.accounts_2);

    return ok;
}

static gboolean
test_mode(DbInfo *dbinfo, gboolean updates, gboolean multi_user)
{
    QofSession *session;
    QofSession *session_db;
    gboolean ok;
    gchar *modesave = dbinfo->mode;
    gchar *sumode = "single-update";

    session = get_random_session();

    add_random_transactions_to_book(qof_session_get_book(session), 20);

    dbinfo->mode = sumode;
    if (!save_db_file(session, dbinfo))
        return FALSE;
    dbinfo->mode = modesave;

    session_db = qof_session_new();

    if (!load_db_file(session_db, dbinfo, !multi_user))
        return FALSE;

    if (multi_user)
    {
        if (!compare_balances(session, session_db))
            return FALSE;

        multi_user_get_everything(session_db, session);
    }

    ok = qof_book_equal(qof_session_get_book(session),
                        qof_session_get_book(session_db));

    do_test_args(ok, "Books equal", __FILE__, __LINE__,
                 "Books not equal for session %s in mode %s",
                 dbinfo->dbname, dbinfo->mode);

    if (multi_user)
    {
        QofBackendError io_err;

        qof_session_end(session_db);
        io_err = qof_session_get_error(session_db);
        if (!do_test_args(io_err == ERR_BACKEND_NO_ERR,
                          "Ending db session",
                          __FILE__, __LINE__,
                          "can't end session for %s in mode %s",
                          dbinfo->dbname, dbinfo->mode))
            return FALSE;
    }

    if (!ok)
    {
        save_xml_files(session, session_db);
        return FALSE;
    }

    if (!test_access(dbinfo, multi_user))
        return FALSE;

    if (updates && !test_updates(session_db, dbinfo, multi_user))
        return FALSE;

    if (multi_user && !test_queries(session_db, dbinfo))
        return FALSE;

    if (updates && !test_updates_2(session_db, dbinfo))
        return FALSE;

    qof_session_destroy(session);
    qof_session_destroy(session_db);

    return ok;
}

static void
run_test(DbInfo *dbinfo)
{

#if 0
    if (!test_mode("single_file", "single-file", FALSE, FALSE))
        return;
#endif

#if 0
    if (!test_mode("single_update", "single-update", TRUE, FALSE))
        return;
#endif

#if 0
    if (!test_mode("multi_user", "multi-user", TRUE, TRUE))
        return;
#endif

#if 1
    dbinfo->dbname = "multi_user_poll";
    dbinfo->mode = "multi-user-poll";
    /** Account for previous failed checks */
    drop_database(dbinfo);
    test_mode(dbinfo, TRUE, TRUE);
#endif
    drop_database(dbinfo);

}

#if 0
static void
test_performance(DbInfo *dbinfo)
{
    QofSession *session;
    gchar *modesave = dbinfo->mode;
    gchar *sumode = "single-update";

    session = get_random_session();

    qof_log_set_level(MOD_TEST, QOF_LOG_WARNING);

    dbinfo->mode = sumode;
    START_CLOCK(0, "Starting to save session");
    if (!save_db_file(session, dbinfo))
        return;
    REPORT_CLOCK(0, "Finished saving session");
    dbinfo->mode = modesave;

    qof_session_destroy(session);
    session = qof_session_new();

    if (!load_db_file(session, dbinfo, FALSE))
        return;

    qof_log_set_level(MOD_TEST, QOF_LOG_INFO);

    START_CLOCK(0, "Starting to save transactions");
    add_random_transactions_to_book(qof_session_get_book(session), 100);
    REPORT_CLOCK(0, "Finished saving transactions");

    REPORT_CLOCK_TOTAL(0, "total");
    REPORT_CLOCK_TOTAL(1, "deleting kvp");
    REPORT_CLOCK_TOTAL(2, "storing kvp");

    qof_session_end(session);
    qof_session_destroy(session);
}
#endif

int
main (int argc, char **argv)
{
    DbInfo *dbinfo;

    qof_init();
    do_test(qof_load_backend_library ("../.libs/", PG_LIB_NAME),
            " loading gnc-backend-postgres GModule failed");

    dbinfo = g_new0(DbInfo, 1);

    if (argc >= 2)
        dbinfo->host = argv[1];
    else
    {
        dbinfo->host = getenv("PGHOST");
        if (!dbinfo->host)
            dbinfo->host = "localhost";
    }

    if (argc >= 3)
        dbinfo->port = argv[2];
    else
    {
        dbinfo->port = getenv("PGPORT");
        if (!dbinfo->port)
            dbinfo->port = "5432";
    }

    /* g_log_set_always_fatal (G_LOG_LEVEL_CRITICAL | G_LOG_LEVEL_WARNING); */

    kvp_exclude_type(KVP_TYPE_BINARY);
    kvp_exclude_type(KVP_TYPE_GLIST);

    /* The random double generator is making values
     * that postgres doesn't like. */
    kvp_exclude_type(KVP_TYPE_DOUBLE);

    set_max_kvp_depth(3);
    set_max_kvp_frame_elements(3);

    set_max_account_tree_depth(3);
    set_max_accounts_per_level(3);

    random_timespec_zero_nsec(TRUE);

    /* Querying on exact prices is problematic. */
    trans_query_include_price(FALSE);

    xaccLogDisable();

    run_test(dbinfo);

    print_test_results();

    return 0;
}
