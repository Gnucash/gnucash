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
test_book (GNCBook *book)
{
  GNCBackendError io_err;
  char cwd[1024];
  char *filename;

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
  g_free (filename);

  io_err = gnc_book_get_error (book);
  if (!do_test (io_err == ERR_BACKEND_NO_ERR, "Beginning gnc_test"))
    return;

  gnc_book_save (book);
  if (!do_test (io_err == ERR_BACKEND_NO_ERR, "Saving gnc_test"))
    return;

  gnc_book_end (book);
  if (!do_test (io_err == ERR_BACKEND_NO_ERR, "Ending gnc_test"))
    return;
}

static void
guile_main (int argc, char **argv)
{
  GNCBook *book;

  gnc_module_system_init ();
  gnc_module_load ("gnucash/engine", 0);

  xaccLogDisable ();

  book = get_random_book ();

  test_book (book);

  gnc_book_destroy (book);

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
