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

static gboolean
test_mode (const char *db_name, const char *mode,
           gboolean updates, gboolean multi_user)
{
  GNCSession *session;
  GNCSession *session_db;
  gboolean ok;

  session = get_random_session ();

  add_random_transactions_to_session (session,
                                      get_random_int_in_range (1, 20));

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

  gnc_session_destroy (session);
  gnc_session_destroy (session_db);

  return ok;
}

static void
run_test (void)
{
  if (!test_mode ("single_file", "single-file", FALSE, FALSE))
    return;

  if (!test_mode ("single_update", "single-update", FALSE, FALSE))
    return;
}

static void
guile_main (int argc, char **argv)
{
  gnc_module_system_init ();
  gnc_module_load ("gnucash/engine", 0);

  g_log_set_always_fatal (G_LOG_LEVEL_CRITICAL | G_LOG_LEVEL_WARNING);

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
  /* getchar (); */

  gh_enter (argc, argv, guile_main);

  return 0;
}
