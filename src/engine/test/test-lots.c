
/* Test file created by Linas Vepstas <linas@linas.org>
 * Minimal test to see if automatic lot scrubbing works.
 * April 2003
 * License: GPL
 */

#include <ctype.h>
#include <glib.h>

#include "Account.h"
#include "Group.h"
#include "Scrub3.h"
#include "gnc-engine-util.h"
#include "gnc-module.h"
#include "test-stuff.h"
#include "test-engine-stuff.h"
#include "Transaction.h"



static void
run_test (void)
{
  QofSession *sess;
  QofBook *book;
  AccountGroup *grp;

  if(!gnc_module_load("gnucash/engine", 0))
  {
    failure("couldn't load gnucash/engine");
    exit(get_rv());
  }

  /* --------------------------------------------------------- */
  /* In the first test, we will merely try to see if we can run
   * without crashing.  We don't check to see if data is good. */
  sess = get_random_session ();
  book = qof_session_get_book (sess);
  if (!book)
  {
    failure("book not created");
    exit(get_rv());
  }

  add_random_transactions_to_book (book, 720);

  grp = xaccGetAccountGroup (book);
  xaccGroupScrubLots (grp);

  /* --------------------------------------------------------- */
  /* In the second test, we create an account with unrealized gains,
   * and see if that gets fixed correctly, with the correct balances,
   * and etc.
   * XXX not inplemented 
   */
  success ("automatic lot scrubbing lightly tested and seem to work");
}

static void
main_helper (void *closure, int argc, char **argv)
{
  run_test ();

  print_test_results();
  exit(get_rv());
}

int
main (int argc, char **argv)
{
  scm_boot_guile(argc, argv, main_helper, NULL);
  return 0;
}
