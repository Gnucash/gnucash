
#include <glib.h>
#include <guile/gh.h>

#include "engine-helpers.h"
#include "gnc-module.h"
#include "test-engine-stuff.h"
#include "test-stuff.h"
#include "Query.h"


static void
test_query (Query *q)
{
  SCM scm_q;
  Query *q2;

  scm_q = gnc_query2scm (q);

  q2 = gnc_scm2query (scm_q);

  if (!xaccQueryEqual (q, q2))
  {
    failure ("queries don't match");
    gh_display (scm_q); gh_newline ();
    scm_q = gnc_query2scm (q2);
    gh_display (scm_q); gh_newline ();
    exit (1);
  }
  else
  {
    success ("queries match");
  }

  xaccFreeQuery (q2);
}

static void
run_tests (void)
{
  Query *q;
  int i;

  test_query (NULL);

  q = xaccMallocQuery ();
  test_query (q);
  xaccFreeQuery (q);

  for (i = 0; i < 10; i++)
  {
    q = get_random_query ();
    test_query (q);
    xaccFreeQuery (q);
  }
}

static void
main_helper (int argc, char **argv)
{
  gnc_module_load("gnucash/engine", 0);
  run_tests ();

  print_test_results ();

  exit (get_rv ());
}

int
main (int argc, char **argv)
{
  gh_enter (argc, argv, main_helper);
  return 0;
}
