#include <glib.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "Backend.h"
#include "TransLog.h"
#include "gnc-book.h"
#include "gnc-engine.h"
#include "gnc-module.h"

#include "test-stuff.h"
#include "test-engine-stuff.h"

static void
run_test (void)
{
  GNCBook *book;
  GNCBackendError io_err;
  char cwd[1024];
  char *filename;

  book = get_random_book ();

  getcwd (cwd, sizeof (cwd));

  filename = g_strconcat ("file:/", cwd, "/test-file-1", NULL);
  gnc_book_begin (book, filename, FALSE, TRUE);
  g_free (filename);

  io_err = gnc_book_get_error (book);
  if (!do_test (io_err == ERR_BACKEND_NO_ERR, "Beginning test-file-1"))
    return;

  gnc_book_save (book);
  if (!do_test (io_err == ERR_BACKEND_NO_ERR, "Saving test-file-1"))
    return;

  gnc_book_end (book);
  if (!do_test (io_err == ERR_BACKEND_NO_ERR, "Ending test-file-1"))
    return;

  if (!do_test (gnc_book_get_url (book) == NULL, "book url not NULL"))
    return;

  filename = g_strdup ("postgres://localhost:7777/gnc_test?mode=single-file");
  gnc_book_begin (book, filename, FALSE, TRUE);

  io_err = gnc_book_get_error (book);
  if (!do_test (io_err == ERR_BACKEND_NO_ERR, "Beginning gnc_test"))
    return;

  gnc_book_save (book);
  if (!do_test (io_err == ERR_BACKEND_NO_ERR, "Saving gnc_test"))
    return;

  gnc_book_end (book);
  if (!do_test (io_err == ERR_BACKEND_NO_ERR, "Ending gnc_test"))
    return;

  gnc_book_destroy (book);

  book = gnc_book_new ();

  gnc_book_begin (book, filename, FALSE, FALSE);
  g_free (filename);

  io_err = gnc_book_get_error (book);
  if (!do_test (io_err == ERR_BACKEND_NO_ERR, "Beginning gnc_test load"))
    return;

  gnc_book_load (book);
  if (!do_test (io_err == ERR_BACKEND_NO_ERR, "Loading gnc_test"))
    return;

  gnc_book_end (book);
  if (!do_test (io_err == ERR_BACKEND_NO_ERR, "Ending gnc_test load"))
    return;

  filename = g_strconcat ("file:/", cwd, "/test-file-2", NULL);
  gnc_book_begin (book, filename, FALSE, TRUE);
  g_free (filename);

  io_err = gnc_book_get_error (book);
  if (!do_test (io_err == ERR_BACKEND_NO_ERR, "Beginning test-file-2"))
    return;

  gnc_book_save (book);
  if (!do_test (io_err == ERR_BACKEND_NO_ERR, "Saving test-file-2"))
    return;

  gnc_book_end (book);
  if (!do_test (io_err == ERR_BACKEND_NO_ERR, "Ending test-file-2"))
    return;
}

static void
guile_main (int argc, char **argv)
{
  GNCBook *book;

  gnc_module_system_init ();
  gnc_module_load ("gnucash/engine", 0);

  glist_exclude_type (KVP_TYPE_BINARY);
  glist_exclude_type (KVP_TYPE_GLIST);
  /* The random double generator is making values
   * that postgres doesn't like. */
  glist_exclude_type (KVP_TYPE_DOUBLE);
  set_max_kvp_depth (3);
  set_max_kvp_frame_elements (3);

  xaccLogDisable ();

  run_test ();

  print_test_results ();
  exit (get_rv ());
}

int
main (int argc, char ** argv)
{
  getchar ();

  gh_enter (argc, argv, guile_main);

  return 0;
}
