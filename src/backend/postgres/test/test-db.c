#include <glib.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "Backend.h"
#include "TransLog.h"
#include "gnc-engine.h"
#include "gnc-module.h"
#include "gnc-session.h"

#include "test-stuff.h"
#include "test-engine-stuff.h"

static void
run_test (void)
{
  GNCBook *book;
  GNCBook *book_db;
  GNCSession *session;
  GNCSession *session_db;
  GNCBackendError io_err;
  char *filename;

  session = gnc_session_new ();

  book = get_random_book (session);

  gnc_session_set_book (session, book);

  filename = g_strdup ("postgres://localhost:7777/gnc_test?mode=single-file");
  gnc_session_begin (session, filename, FALSE, TRUE);

  io_err = gnc_session_get_error (session);
  if (!do_test (io_err == ERR_BACKEND_NO_ERR, "Beginning gnc_test"))
    return;

  gnc_session_save (session);
  if (!do_test (io_err == ERR_BACKEND_NO_ERR, "Saving gnc_test"))
    return;

  gnc_session_end (session);
  if (!do_test (io_err == ERR_BACKEND_NO_ERR, "Ending gnc_test"))
    return;

  if (!do_test (gnc_session_get_url (session) == NULL, "session url not NULL"))
    return;

  session_db = gnc_session_new ();

  gnc_session_begin (session_db, filename, FALSE, FALSE);
  g_free (filename);

  io_err = gnc_session_get_error (session_db);
  if (!do_test (io_err == ERR_BACKEND_NO_ERR, "Beginning gnc_test load"))
    return;

  gnc_session_load (session_db);
  if (!do_test (io_err == ERR_BACKEND_NO_ERR, "Loading gnc_test"))
    return;

  gnc_session_end (session_db);
  if (!do_test (io_err == ERR_BACKEND_NO_ERR, "Ending gnc_test load"))
    return;

  book_db = gnc_session_get_book (session_db);

  do_test (gnc_book_equal (book, book_db), "Books not equal");
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

  random_character_include_funky_chars (FALSE);

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
