
#include <glib.h>
#include <guile/gh.h>

#include "engine-helpers.h"
#include "gnc-module.h"
#include "test-engine-stuff.h"
#include "test-stuff.h"
#include "Query.h"
#include "TransLog.h"


static void
test_query (Query *q)
{
  SCM scm_q;
  scm_q = gnc_query2scm (q);
  gh_display (scm_q); gh_newline (); gh_newline ();
}

static void
run_tests (int count)
{
  Query *q;
  int i;

  for (i = 0; i < count; i++) {
    q = get_random_query ();
    test_query (q);
    xaccFreeQuery (q);
  }
  success ("");
}

static void
main_helper (int argc, char **argv)
{
  int count = 50;

  gnc_module_load("gnucash/engine", 0);

  if (argc > 1)
    count = atoi (argv[1]);

  if (count < 0)
    count = 0;

  xaccLogDisable ();

  /* scm conversion doesn't handle binary atm */
  kvp_exclude_type (KVP_TYPE_BINARY);

  run_tests (count);

  print_test_results ();

  exit (get_rv ());
}

int
main (int argc, char **argv)
{
  gh_enter (argc, argv, main_helper);
  return 0;
}
