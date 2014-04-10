
#include <glib.h>
#include <guile/gh.h>

#include "engine-helpers.h"
#include "gnc-module.h"
#include "test-engine-stuff.h"
#include "test-stuff.h"
#include "Query.h"
#include "TransLog.h"


static void
test_query (Query *q, SCM val2str)
{
  SCM scm_q;
  SCM str_q;
  SCM args = SCM_EOL;

  scm_q = gnc_query2scm (q);
  args = gh_cons (scm_q, SCM_EOL);
  str_q = gh_apply (val2str, args);

  args = gh_cons (gh_str02scm ("'"), gh_cons (str_q, SCM_EOL));
  str_q = scm_string_append (args);

  gh_display (str_q); gh_newline (); gh_newline ();
}

static void
run_tests (int count)
{
  Query *q;
  SCM val2str;
  int i;

  val2str = gh_eval_str ("gnc:value->string");
  g_return_if_fail (gh_procedure_p (val2str));

  for (i = 0; i < count; i++) {
    q = get_random_query ();
    test_query (q, val2str);
    xaccFreeQuery (q);
  }
  success ("");
}

static void
main_helper (int argc, char **argv)
{
  int count = 50;

  gnc_module_load("gnucash/engine", 0);
  gnc_module_load("gnucash/app-utils", 0);

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
