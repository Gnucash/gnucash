
/* test file created by Linas Vepstas <linas@linas.org>
 * December 2001
 * License: GPL
 */

#include <ctype.h>
#include <glib.h>
#include <guile/gh.h>
#include <time.h>

#include "Period.h"
#include "gnc-book.h"
#include "gnc-book-p.h"
#include "gnc-module.h"
#include "test-stuff.h"
#include "test-engine-stuff.h"



static void
run_test (void)
{
  AccountGroup *grp;
  GNCBook *book;
  Timespec ts;

  int ok = 1;
  if (!ok)
  {
    failure ("its borken");
  }

  book = gnc_book_new ();
  if (!book)
  {
    failure("book not created");
    exit(get_rv());
  }

  grp = get_random_group (book);
  if(!grp)
  {
    failure("group not created");
    exit(get_rv());
  }

  gnc_book_set_group (book, grp);

  success ("periods work but wern't really tested yet");
}

static void
main_helper (int argc, char **argv)
{
  gnc_module_load("gnucash/engine", 0);

  run_test ();

  success ("periods aren't realy tested yet");

  print_test_results();
  exit(get_rv());
}

int
main (int argc, char **argv)
{
  gh_enter (argc, argv, main_helper);
  return 0;
}
