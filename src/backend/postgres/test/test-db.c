#include <glib.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "Backend.h"
#include "TransLog.h"
#include "gnc-engine.h"
#include "gnc-module.h"
#include "gnc-session.h"

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
  g_return_val_if_fail (db_name && mode, NULL);

  return g_strdup_printf ("postgres://localhost:7777/%s?mode=%s",
                          db_name, mode);
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
load_db_file (GNCSession *session, const char *db_name, const char *mode)
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

  gnc_session_begin (session_1, filename, FALSE, FALSE);
  io_err = gnc_session_get_error (session_1);

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

  if (!load_db_file (session_2, db_name, mode))
    return FALSE;

  remove_unneeded_commodities (session);
  remove_unneeded_commodities (session_2);

  ok = gnc_book_equal (gnc_session_get_book (session),
                       gnc_session_get_book (session_2));

  do_test_args (ok, "Books equal after update", __FILE__, __LINE__,
                "Books not equal for session %s in mode %s",
                db_name, mode);

  if (!ok)
  {
    save_xml_files (session, session_2);
    return FALSE;
  }

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
  }

  gnc_session_destroy (session_2);
  g_free (filename);
}

static gboolean
test_mode (const char *db_name, const char *mode,
           gboolean updates, gboolean multi_user)
{
  GNCSession *session;
  GNCSession *session_db;
  gboolean ok;

  session = get_random_session ();

  add_random_transactions_to_session (session,
                                      get_random_int_in_range (10, 20));

  if (!save_db_file (session, db_name, mode))
    return FALSE;

  session_db = gnc_session_new ();

  if (!load_db_file (session_db, db_name, mode))
    return FALSE;

  ok = gnc_book_equal (gnc_session_get_book (session),
                       gnc_session_get_book (session_db));

  do_test_args (ok, "Books equal", __FILE__, __LINE__,
                "Books not equal for session %s in mode %s",
                db_name, mode);

  if (!ok)
  {
    save_xml_files (session, session_db);
    return FALSE;
  }

  ok = test_access (db_name, mode, multi_user);

  if (updates && !test_updates (session_db, db_name, mode, multi_user))
    return FALSE;

  gnc_session_destroy (session);
  gnc_session_destroy (session_db);

  return ok;
}

static void
run_test (void)
{
#if 1
  if (!test_mode ("single_file", "single-file", FALSE, FALSE))
    return;
#endif

  if (!test_mode ("single_update", "single-update", TRUE, FALSE))
    return;
}

static void
guile_main (int argc, char **argv)
{
  gnc_module_system_init ();
  gnc_module_load ("gnucash/engine", 0);

  /* g_log_set_always_fatal (G_LOG_LEVEL_CRITICAL | G_LOG_LEVEL_WARNING); */

  glist_exclude_type (KVP_TYPE_BINARY);
  glist_exclude_type (KVP_TYPE_GLIST);

  /* The random double generator is making values
   * that postgres doesn't like. */
  glist_exclude_type (KVP_TYPE_DOUBLE);

  set_max_kvp_depth (3);
  set_max_kvp_frame_elements (3);

  set_max_group_depth (3);
  set_max_group_accounts (5);

  random_timespec_zero_nsec (TRUE);

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
