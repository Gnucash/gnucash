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

  g_return_if_fail (session && filename);

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

static void
run_test (void)
{
  GNCSession *session;
  GNCSession *session_db;
  GNCBackendError io_err;
  char *filename;
  gboolean ok;

  session = get_random_session ();

  filename = g_strdup ("postgres://localhost:7777/gnc_test?mode=single-file");

  gnc_session_begin (session, filename, FALSE, TRUE);
  io_err = gnc_session_get_error (session);
  if (!do_test (io_err == ERR_BACKEND_NO_ERR, "Beginning gnc_test"))
    return;

  gnc_session_save (session);
  io_err = gnc_session_get_error (session);
  if (!do_test (io_err == ERR_BACKEND_NO_ERR, "Saving gnc_test"))
    return;

  gnc_session_end (session);
  io_err = gnc_session_get_error (session);
  if (!do_test (io_err == ERR_BACKEND_NO_ERR, "Ending gnc_test"))
    return;

  if (!do_test (gnc_session_get_url (session) == NULL, "session url not NULL"))
    return;

  session_db = gnc_session_new ();

  gnc_session_begin (session_db, filename, FALSE, FALSE);
  io_err = gnc_session_get_error (session_db);
  if (!do_test (io_err == ERR_BACKEND_NO_ERR, "Beginning gnc_test load"))
    return;

  gnc_session_load (session_db);
  io_err = gnc_session_get_error (session_db);
  if (!do_test (io_err == ERR_BACKEND_NO_ERR, "Loading gnc_test"))
    return;

  gnc_session_end (session_db);
  io_err = gnc_session_get_error (session_db);
  if (!do_test (io_err == ERR_BACKEND_NO_ERR, "Ending gnc_test load"))
    return;

  ok = gnc_book_equal (gnc_session_get_book (session),
                       gnc_session_get_book (session_db));

  do_test (ok, "Books not equal");

  if (!ok)
    save_xml_files (session, session_db);

  g_free (filename);
}

static void
guile_main (int argc, char **argv)
{
  gnc_module_system_init ();
  gnc_module_load ("gnucash/engine", 0);

  glist_exclude_type (KVP_TYPE_BINARY);
  glist_exclude_type (KVP_TYPE_GLIST);

  /* The random double generator is making values
   * that postgres doesn't like. */
  glist_exclude_type (KVP_TYPE_DOUBLE);

  set_max_kvp_depth (3);
  set_max_kvp_frame_elements (3);

  set_max_group_depth (3);
  set_max_group_accounts (5);

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
