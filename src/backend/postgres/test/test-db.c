#include <glib.h>
#include <guile/gh.h>
#include <libpq-fe.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "Backend.h"
#include "PostgresBackend.h"
#include "TransactionP.h"
#include "TransLog.h"
#include "gnc-book.h"
#include "gnc-engine.h"
#include "gnc-engine-util.h"
#include "gnc-module.h"
#include "gnc-session-p.h"
#include "gncquery.h"

#include "test-stuff.h"
#include "test-engine-stuff.h"

static void
save_xml_file (GNCSession *session, const char *filename_base)
{
  GNCBackendError io_err;
  char cwd[1024];
  char *filename;

  g_return_if_fail (session && filename_base);

  getcwd (cwd, sizeof (cwd));

  filename = g_strdup_printf ("file:/%s/%s", cwd, filename_base);

  gnc_session_begin (session, filename, FALSE, TRUE);

  io_err = gnc_session_get_error (session);
  g_return_if_fail (io_err == ERR_BACKEND_NO_ERR);

  gnc_session_save (session);
  io_err = gnc_session_get_error (session);
  g_return_if_fail (io_err == ERR_BACKEND_NO_ERR);

  gnc_session_end (session);
  io_err = gnc_session_get_error (session);
  g_return_if_fail (io_err == ERR_BACKEND_NO_ERR);

  g_free (filename);
}

static void
save_xml_files (GNCSession *session_1, GNCSession *session_2)
{
  g_return_if_fail (session_1 && session_2);

  save_xml_file (session_1, "test_file_1");
  save_xml_file (session_2, "test_file_2");
}

static char *
db_file_url (const char *db_name, const char *mode)
{
  char *db_socket_dir;

  g_return_val_if_fail (db_name && mode, NULL);

  /* TEST_DB_SOCKET_DIR must be an absolute path */
  db_socket_dir = getenv("TEST_DB_SOCKET_DIR");
  if(! db_socket_dir) g_warning("Couldn't getenv TEST_DB_SOCKET_DIR");
  g_return_val_if_fail (db_socket_dir, NULL);  

  return g_strdup_printf ("postgres://%s:7777/%s?mode=%s",
                          db_socket_dir, db_name, mode);
}

static gboolean
save_db_file (GNCSession *session, const char *db_name, const char *mode)
{
  GNCBackendError io_err;
  char *filename;

  g_return_val_if_fail (session && db_name && mode, FALSE);

  filename = db_file_url (db_name, mode);

  gnc_session_begin (session, filename, FALSE, TRUE);
  io_err = gnc_session_get_error (session);
  if (!do_test_args (io_err == ERR_BACKEND_NO_ERR,
                     "Beginning db session",
                     __FILE__, __LINE__,
                     "can't begin session for %s in mode %s",
                     db_name, mode))
    return FALSE;

  gnc_session_save (session);
  io_err = gnc_session_get_error (session);
  if (!do_test_args (io_err == ERR_BACKEND_NO_ERR,
                     "Saving db session",
                     __FILE__, __LINE__,
                     "can't save session for %s in mode %s",
                     db_name, mode))
    return FALSE;

  gnc_session_end (session);
  io_err = gnc_session_get_error (session);
  if (!do_test_args (io_err == ERR_BACKEND_NO_ERR,
                     "Ending db session",
                     __FILE__, __LINE__,
                     "can't end session for %s in mode %s",
                     db_name, mode))
    return FALSE;

  do_test (gnc_session_get_url (session) == NULL, "session url not NULL");

  g_free (filename);

  return TRUE;
}

static gboolean
load_db_file (GNCSession *session, const char *db_name, const char *mode,
              gboolean end_session)
{
  GNCBackendError io_err;
  char *filename;

  g_return_val_if_fail (session && db_name && mode, FALSE);

  filename = db_file_url (db_name, mode);

  gnc_session_begin (session, filename, FALSE, FALSE);
  io_err = gnc_session_get_error (session);
  if (!do_test_args (io_err == ERR_BACKEND_NO_ERR,
                     "Beginning db session",
                     __FILE__, __LINE__,
                     "can't begin session for %s in mode %s",
                     db_name, mode))
    return FALSE;

  gnc_session_load (session);
  io_err = gnc_session_get_error (session);
  if (!do_test_args (io_err == ERR_BACKEND_NO_ERR,
                     "Loading db session",
                     __FILE__, __LINE__,
                     "can't load session for %s in mode %s",
                     db_name, mode))
    return FALSE;

  if (end_session)
  {
    gnc_session_end (session);
    io_err = gnc_session_get_error (session);
    if (!do_test_args (io_err == ERR_BACKEND_NO_ERR,
                       "Ending db session",
                       __FILE__, __LINE__,
                       "can't end session for %s in mode %s",
                       db_name, mode))
      return FALSE;

    do_test (gnc_session_get_url (session) == NULL, "session url not NULL");
  }

  g_free (filename);

  return TRUE;
}

static gboolean
test_access (const char *db_name, const char *mode, gboolean multi_user)
{
  GNCBackendError io_err;
  GNCSession *session_1;
  GNCSession *session_2;
  char *filename;

  g_return_val_if_fail (db_name && mode, FALSE);

  filename = db_file_url (db_name, mode);

  session_1 = gnc_session_new ();

  gnc_session_begin (session_1, filename, FALSE, FALSE);
  io_err = gnc_session_get_error (session_1);
  if (!do_test_args (io_err == ERR_BACKEND_NO_ERR,
                     "Beginning db session",
                     __FILE__, __LINE__,
                     "can't begin session for %s in mode %s",
                     db_name, mode))
    return FALSE;

  session_2 = gnc_session_new ();

  gnc_session_begin (session_2, filename, FALSE, FALSE);
  io_err = gnc_session_get_error (session_2);

  if (multi_user)
  {
    if (!do_test_args (io_err == ERR_BACKEND_NO_ERR,
                       "Beginning second multi-user db session",
                       __FILE__, __LINE__,
                       "can't begin second session for %s in mode %s",
                       db_name, mode))
      return FALSE;
  }
  else
  {
    if (!do_test_args (io_err != ERR_BACKEND_NO_ERR,
                       "Beginning second single-user db session",
                       __FILE__, __LINE__,
                       "began second session for %s in mode %s",
                       db_name, mode))
      return FALSE;
  }

  gnc_session_destroy (session_1);
  gnc_session_destroy (session_2);

  return TRUE;
}

static gpointer
mark_account_commodities (Account *a, gpointer data)
{
  GHashTable *hash = data;

  g_hash_table_insert (hash, xaccAccountGetCommodity (a), hash);

  return NULL;
}

static int
mark_transaction_commodities (Transaction *t, void *data)
{
  GHashTable *hash = data;

  g_hash_table_insert (hash, xaccTransGetCurrency (t), hash);

  return TRUE;
}

static gboolean
mark_price_commodities (GNCPrice *p, gpointer data)
{
  GHashTable *hash = data;

  g_hash_table_insert (hash, gnc_price_get_commodity (p), hash);
  g_hash_table_insert (hash, gnc_price_get_currency (p), hash);

  return TRUE;
}

typedef struct
{
  GHashTable *hash;
  GList *to_delete;
} CommodityDeleteInfo;

static gboolean
add_commodity_to_delete (gnc_commodity *com, gpointer data)
{
  CommodityDeleteInfo *cdi = data;

  if (!g_hash_table_lookup (cdi->hash, com) &&
      safe_strcmp (gnc_commodity_get_namespace (com),
                   GNC_COMMODITY_NS_ISO) != 0)
    cdi->to_delete = g_list_prepend (cdi->to_delete, com);

  return TRUE;
}

static void
remove_unneeded_commodities (GNCSession *session)
{
  CommodityDeleteInfo cdi;
  GNCBook *book;
  GList *node;

  g_return_if_fail (session);

  cdi.hash = g_hash_table_new (g_direct_hash, g_direct_equal);

  book = gnc_session_get_book (session);

  xaccGroupForEachAccount (gnc_book_get_group (book),
                           mark_account_commodities,
                           cdi.hash, TRUE);

  xaccGroupForEachTransaction (gnc_book_get_group (book),
                               mark_transaction_commodities,
                               cdi.hash);

  gnc_pricedb_foreach_price (gnc_book_get_pricedb (book),
                             mark_price_commodities,
                             cdi.hash, FALSE);

  cdi.to_delete = NULL;

  gnc_commodity_table_foreach_commodity (gnc_book_get_commodity_table (book),
                                         add_commodity_to_delete, &cdi);

  for (node = cdi.to_delete; node; node = node->next)
    gnc_commodity_table_remove (gnc_book_get_commodity_table (book),
                                node->data);

  g_list_free (cdi.to_delete);
  g_hash_table_destroy (cdi.hash);
}

static Query *
make_get_all_query (GNCSession * session)
{
  Query *q;

  g_return_val_if_fail (session, NULL);

  q = xaccMallocQuery ();

  xaccQueryAddClearedMatch (q,
                            CLEARED_NO |
                            CLEARED_CLEARED |
                            CLEARED_RECONCILED |
                            CLEARED_FROZEN |
                            CLEARED_VOIDED,
                            QUERY_AND);

  xaccQuerySetGroup (q, gnc_book_get_group (gnc_session_get_book (session)));

  return q;
}

static void
multi_user_get_everything (GNCSession *session, GNCSession *base)
{
  Query *q;

  g_return_if_fail (session);

  q = make_get_all_query (session);

  xaccQueryGetSplits (q);

  xaccFreeQuery (q);

  /* load in prices from base */
  if (base)
    gnc_pricedb_equal (gnc_book_get_pricedb (gnc_session_get_book (base)),
                       gnc_book_get_pricedb (gnc_session_get_book (session)));
}

static gboolean
test_updates (GNCSession *session, const char *db_name, const char *mode,
              gboolean multi_user)
{
  GNCBackendError io_err;
  GNCSession *session_2;
  char *filename;
  gboolean ok;

  g_return_val_if_fail (session && db_name && mode, FALSE);

  filename = db_file_url (db_name, mode);

  gnc_session_begin (session, filename, FALSE, FALSE);
  io_err = gnc_session_get_error (session);
  if (!do_test_args (io_err == ERR_BACKEND_NO_ERR,
                     "Beginning db update session",
                     __FILE__, __LINE__,
                     "can't begin session for %s in mode %s",
                     db_name, mode))
    return FALSE;

  make_random_changes_to_session (session);

  if (!multi_user)
  {
    gnc_session_end (session);
    io_err = gnc_session_get_error (session);
    if (!do_test_args (io_err == ERR_BACKEND_NO_ERR,
                       "Ending db session",
                       __FILE__, __LINE__,
                       "can't end session for %s in mode %s",
                       db_name, mode))
      return FALSE;
  }

  session_2 = gnc_session_new ();

  if (!load_db_file (session_2, db_name, mode, !multi_user))
    return FALSE;

  if (multi_user)
    multi_user_get_everything (session_2, session);

  remove_unneeded_commodities (session);
  remove_unneeded_commodities (session_2);

  ok = gnc_book_equal (gnc_session_get_book (session),
                       gnc_session_get_book (session_2));

  do_test_args (ok, "Books equal after update", __FILE__, __LINE__,
                "Books not equal for session %s in mode %s",
                db_name, mode);

  if (multi_user)
  {
    gnc_session_end (session);
    io_err = gnc_session_get_error (session);
    if (!do_test_args (io_err == ERR_BACKEND_NO_ERR,
                       "Ending db session",
                       __FILE__, __LINE__,
                       "can't end session for %s in mode %s",
                       db_name, mode))
      return FALSE;

    gnc_session_end (session_2);
    io_err = gnc_session_get_error (session_2);
    if (!do_test_args (io_err == ERR_BACKEND_NO_ERR,
                       "Ending db session",
                       __FILE__, __LINE__,
                       "can't end session for %s in mode %s",
                       db_name, mode))
      return FALSE;
  }

  if (!ok)
  {
    save_xml_files (session, session_2);
    return FALSE;
  }

  gnc_session_destroy (session_2);
  g_free (filename);

  return TRUE;
}

static gboolean
num_trans_helper (Transaction *trans, gpointer data)
{
  int *num = data;

  *num += 1;

  return TRUE;
}

static int
session_num_trans (GNCSession *session)
{
  AccountGroup *group;
  GNCBook *book;
  int num = 0;

  g_return_val_if_fail (session, 0);

  book = gnc_session_get_book (session);
  group = gnc_book_get_group (book);

  xaccGroupForEachTransaction (group, num_trans_helper, &num);

  return num;
}

typedef struct
{
  GNCSession *session_base;
  const char *db_name;
  const char *mode;
  gint loaded;
  gint total;
} QueryTestData;

static gboolean
test_raw_query (GNCSession *session, Query *q)
{
  const char *sql_query_string;
  PGresult *result;
  PGBackend *be;
  sqlQuery *sq;
  gboolean ok;

  g_return_val_if_fail (session && q, FALSE);

  be = (PGBackend *) gnc_session_get_backend(session);

  sq = sqlQuery_new();
  sql_query_string = sqlQuery_build (sq, q, gnc_session_get_book(session));

#if 0
  fputs (sql_query_string, stderr);
  fputs ("\n", stderr);
#endif

  result = PQexec (be->connection, sql_query_string);

  ok = (result && PQresultStatus (result) == PGRES_TUPLES_OK);
  if (!ok)
  {
    failure ("raw query failed");
  }
  else
  {
    ok = ok && (PQntuples (result) == 1);
    if (!ok)
      failure_args ("number returned test",
                    __FILE__, __LINE__,
                    "query returned %d tuples",
                    PQntuples (result));
  }

  if (ok)
  {
    success ("raw query succeeded");
  }

  PQclear (result);
  sql_Query_destroy (sq);

  return ok;
}

static gboolean
test_trans_query (Transaction *trans, gpointer data)
{
  QueryTestData *qtd = data;
  GNCBackendError io_err;
  GNCSession *session;
  AccountGroup *group;
  char *filename;
  GNCBook *book;
  GList *list;
  Query *q;

  filename = db_file_url (qtd->db_name, qtd->mode);

  session = gnc_session_new ();

  gnc_session_begin (session, filename, FALSE, FALSE);
  io_err = gnc_session_get_error (session);
  if (!do_test_args (io_err == ERR_BACKEND_NO_ERR,
                     "Beginning db session",
                     __FILE__, __LINE__,
                     "can't begin session for %s",
                     filename))
    return FALSE;

  gnc_session_load (session);
  io_err = gnc_session_get_error (session);
  if (!do_test_args (io_err == ERR_BACKEND_NO_ERR,
                     "Loading db session",
                     __FILE__, __LINE__,
                     "can't load session for %s",
                     filename))
    return FALSE;

  book = gnc_session_get_book (session);
  group = gnc_book_get_group (book);

  q = make_trans_query (trans, get_random_query_type () | GUID_QT);
  xaccQuerySetGroup (q, group);

  if (!test_raw_query (session, q))
  {
    failure ("raw query failed");
    return FALSE;
  }

  list = xaccQueryGetTransactions (q, QUERY_MATCH_ANY);
  if (g_list_length (list) != 1)
  {
    failure_args ("test num returned", __FILE__, __LINE__,
                  "number of matching transactions %d not 1",
                  g_list_length (list));
    return FALSE;
  }

  qtd->loaded += session_num_trans (session);
  qtd->total += session_num_trans (qtd->session_base);

  if (!xaccTransEqual (trans, list->data, TRUE, TRUE))
  {
    failure ("matching transaction is wrong");
    return FALSE;
  }

  success ("found right transaction");

  xaccFreeQuery (q);
  gnc_session_destroy (session);
  g_free (filename);

  return TRUE;
}

static gboolean
compare_balances (GNCSession *session_1, GNCSession *session_2)
{
  GNCBook * book_1 = gnc_session_get_book (session_1);
  GNCBook * book_2 = gnc_session_get_book (session_2);
  GList * list;
  GList * node;
  gboolean ok;

  g_return_val_if_fail (session_1, FALSE);
  g_return_val_if_fail (session_2, FALSE);

  ok = TRUE;

  list = xaccGroupGetSubAccounts (gnc_book_get_group (book_1));
  for (node = list; node; node = node->next)
  {
    Account * account_1 = node->data;
    Account * account_2;

    account_2 = xaccAccountLookup (xaccAccountGetGUID (account_1), book_2);
    if (!account_2)
    {
      failure_args ("", __FILE__, __LINE__,
                    "session_1 has account %s but not session_2",
                    guid_to_string (xaccAccountGetGUID (account_1)));
      return FALSE;
    }

    if (!gnc_numeric_equal (xaccAccountGetBalance (account_1),
                            xaccAccountGetBalance (account_2)))
    {
      failure_args ("", __FILE__, __LINE__,
                    "balances not equal for account %s",
                    guid_to_string (xaccAccountGetGUID (account_1)));
      ok = FALSE;
    }

    if (!gnc_numeric_equal (xaccAccountGetClearedBalance (account_1),
                            xaccAccountGetClearedBalance (account_2)))
    {
      failure_args ("", __FILE__, __LINE__,
                    "cleared balances not equal for account %s",
                    guid_to_string (xaccAccountGetGUID (account_1)));
      ok = FALSE;
    }

    if (!gnc_numeric_equal (xaccAccountGetReconciledBalance (account_1),
                            xaccAccountGetReconciledBalance (account_2)))
    {
      failure_args ("", __FILE__, __LINE__,
                    "reconciled balances not equal for account %s",
                    guid_to_string (xaccAccountGetGUID (account_1)));
      ok = FALSE;
    }

    if (!ok)
      break;

    success ("balances equal");
  }
  g_list_free (list);

  return ok;
}

static gboolean
test_queries (GNCSession *session_base, const char *db_name, const char *mode)
{
  QueryTestData qtd;
  AccountGroup *group;
  GNCBook *book;
  gboolean ok;

  /* FIXME REMOVE */
  return TRUE;

  g_return_val_if_fail (db_name && mode, FALSE);

  book = gnc_session_get_book (session_base);
  group = gnc_book_get_group (book);

  qtd.session_base = session_base;
  qtd.db_name = db_name;
  qtd.mode = mode;
  qtd.loaded = 0;
  qtd.total = 0;

  ok = xaccGroupForEachTransaction (group, test_trans_query, &qtd);

#if 0
  g_warning ("average percentage loaded = %3.2f%%",
             (qtd.loaded / (double) qtd.total) * 100.0);
#endif

  return ok;
}

typedef struct
{
  GNCSession *session_1;
  GNCSession *session_2;
  GList * accounts_1;
  GList * accounts_2;
} UpdateTestData;

static gboolean
test_trans_update (Transaction *trans, gpointer data)
{
  UpdateTestData *td = data;
  GNCBackendError io_err;
  Transaction * trans_2;
  GNCBook * book_1;
  GNCBook * book_2;
  GUID guid;
  gboolean ok;

  book_1 = gnc_session_get_book (td->session_1);
  book_2 = gnc_session_get_book (td->session_2);

  guid = *xaccTransGetGUID (trans);

  xaccTransBeginEdit (trans);
  make_random_changes_to_transaction_and_splits (book_1, trans,
                                                 td->accounts_1);
  xaccTransCommitEdit (trans);

  io_err = gnc_session_get_error (td->session_1);
  if (!do_test_args (io_err == ERR_BACKEND_NO_ERR,
                     "changing transaction in session 1",
                     __FILE__, __LINE__,
                     "error changing transaction: %d", io_err))
    return FALSE;

  trans = xaccTransLookup (&guid, book_1);
  trans_2 = xaccTransLookup (&guid, book_2);

  /* This should get rolled back. */
  if (trans_2)
  {
    xaccTransBeginEdit (trans_2);
    make_random_changes_to_transaction_and_splits (book_2, trans_2,
                                                   td->accounts_2);
    xaccTransCommitEdit (trans_2);
  }

  trans_2 = xaccTransLookup (&guid, book_2);

  ok = xaccTransEqual (trans, trans_2, TRUE, TRUE);
  if (trans && trans_2)
    ok = ok && trans->version == trans_2->version;

  if (!do_test_args (ok,
                     "test rollback",
                     __FILE__, __LINE__,
                     "transaction not rolled back properly"))
    return FALSE;

  return TRUE;
}

static gboolean
add_trans_helper (Transaction *trans, gpointer data)
{
  GList **list = data;

  *list = g_list_prepend (*list, trans);

  return TRUE;
}

static gboolean
test_updates_2 (GNCSession *session_base,
                const char *db_name, const char *mode)
{
  AccountGroup *group;
  UpdateTestData td;
  char * filename;
  GList * transes;
  GList * node;
  GNCBook *book;
  gboolean ok;

  g_return_val_if_fail (session_base && db_name && mode, FALSE);

  filename = db_file_url (db_name, mode);

  if (!load_db_file (session_base, db_name, mode, FALSE))
    return FALSE;

  multi_user_get_everything (session_base, NULL);

  book = gnc_session_get_book (session_base);
  group = gnc_book_get_group (book);

  td.session_1 = session_base;
  td.session_2 = gnc_session_new ();
  td.accounts_1 = xaccGroupGetSubAccounts (group);

  if (!load_db_file (td.session_2, db_name, mode, FALSE))
    return FALSE;

  multi_user_get_everything (td.session_2, NULL);

  td.accounts_2 = xaccGroupGetSubAccounts
    (gnc_book_get_group (gnc_session_get_book (td.session_2)));

  ok = TRUE;
  transes = NULL;
  xaccGroupForEachTransaction (group, add_trans_helper, &transes);
  for (node = transes; node; node = node->next)
  {
    ok = test_trans_update (node->data, &td);
    if (!ok)
      break;
  }
  g_list_free (transes);

  gnc_session_end (session_base);

  gnc_session_end (td.session_2);
  gnc_session_destroy (td.session_2);
  g_list_free (td.accounts_1);
  g_list_free (td.accounts_2);

  return ok;
}

static gboolean
test_mode (const char *db_name, const char *mode,
           gboolean updates, gboolean multi_user)
{
  GNCSession *session;
  GNCSession *session_db;
  gboolean ok;

  session = get_random_session ();

  add_random_transactions_to_book (gnc_session_get_book(session), 20);

  if (!save_db_file (session, db_name, "single-update"))
    return FALSE;

  session_db = gnc_session_new ();

  if (!load_db_file (session_db, db_name, mode, !multi_user))
    return FALSE;

  if (multi_user)
  {
    if (!compare_balances (session, session_db))
      return FALSE;

    multi_user_get_everything (session_db, session);
  }

  ok = gnc_book_equal (gnc_session_get_book (session),
                       gnc_session_get_book (session_db));

  do_test_args (ok, "Books equal", __FILE__, __LINE__,
                "Books not equal for session %s in mode %s",
                db_name, mode);

  if (multi_user)
  {
    GNCBackendError io_err;

    gnc_session_end (session_db);
    io_err = gnc_session_get_error (session_db);
    if (!do_test_args (io_err == ERR_BACKEND_NO_ERR,
                       "Ending db session",
                       __FILE__, __LINE__,
                       "can't end session for %s in mode %s",
                       db_name, mode))
      return FALSE;
  }

  if (!ok)
  {
    save_xml_files (session, session_db);
    return FALSE;
  }

  if (!test_access (db_name, mode, multi_user))
    return FALSE;

  if (updates && !test_updates (session_db, db_name, mode, multi_user))
    return FALSE;

  if (multi_user && !test_queries (session_db, db_name, mode))
    return FALSE;

  if (updates && !test_updates_2 (session_db, db_name, mode))
    return FALSE;

  gnc_session_destroy (session);
  gnc_session_destroy (session_db);

  return ok;
}

static void
run_test (void)
{
#if 0
  if (!test_mode ("single_file", "single-file", FALSE, FALSE))
    return;
#endif

#if 0
  if (!test_mode ("single_update", "single-update", TRUE, FALSE))
    return;
#endif

#if 0
  if (!test_mode ("multi_user", "multi-user", TRUE, TRUE))
    return;
#endif

#if 1
  if (!test_mode ("multi_user_poll", "multi-user-poll", TRUE, TRUE))
    return;
#endif
}

static void
guile_main (int argc, char **argv)
{
  gnc_module_system_init ();
  gnc_module_load ("gnucash/engine", 0);

  /* g_log_set_always_fatal (G_LOG_LEVEL_CRITICAL | G_LOG_LEVEL_WARNING); */

  kvp_exclude_type (KVP_TYPE_BINARY);
  kvp_exclude_type (KVP_TYPE_GLIST);

  /* The random double generator is making values
   * that postgres doesn't like. */
  kvp_exclude_type (KVP_TYPE_DOUBLE);

  set_max_kvp_depth (3);
  set_max_kvp_frame_elements (3);

  set_max_group_depth (3);
  set_max_group_accounts (3);

  random_timespec_zero_nsec (TRUE);

  /* Querying on exact prices is problematic. */
  trans_query_include_price (FALSE);

  xaccLogDisable ();

  run_test ();

  print_test_results ();
  exit (get_rv ());
}

int
main (int argc, char ** argv)
{
  gh_enter (argc, argv, guile_main);
  return 0;
}
