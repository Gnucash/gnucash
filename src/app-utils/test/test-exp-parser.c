#include <glib.h>
#include <stdlib.h>

#include "gnc-exp-parser.h"
#include "gnc-numeric.h"
#include "test-stuff.h"

static GList *tests = NULL;

typedef struct
{
  const char * test_name;

  const char * exp;

  gboolean should_succeed;

  gnc_numeric expected_result;

  int expected_error_offset;
} TestNode;

static void
add_pass_test (const char *test_name, const char *exp, gnc_numeric result)
{
  TestNode *node = g_new0 (TestNode, 1);

  node->test_name = test_name;
  node->exp = exp ? exp : test_name;
  node->should_succeed = TRUE;
  node->expected_result = result;

  tests = g_list_append (tests, node);
}

static void
add_fail_test (const char *test_name, const char *exp,
               int expected_error_offset)
{
  TestNode *node = g_new0 (TestNode, 1);

  node->test_name = test_name;
  node->exp = exp;
  node->should_succeed = FALSE;
  node->expected_error_offset = expected_error_offset;

  tests = g_list_append (tests, node);
}

static void
run_parser_test (TestNode *node)
{
  gboolean succeeded;
  gnc_numeric result;
  char *error_loc;

  succeeded = gnc_exp_parser_parse (node->exp, &result, &error_loc);

  if (succeeded != node->should_succeed)
  {
    failure_args (node->test_name, __FILE__, __LINE__,
                  "parser %s on \"%s\"", 
                  succeeded ? "succeeded" : "failed",
                  node->exp);
    return;
  }

  if (succeeded)
  {
    if (!gnc_numeric_equal (result, node->expected_result))
    {
      failure_args (node->test_name, __FILE__, __LINE__, "wrong result");
      return;
    }
  }
  else if (node->expected_error_offset != -1)
  {
    if (error_loc != node->exp + node->expected_error_offset)
    {
      failure_args (node->test_name, __FILE__, __LINE__, "wrong offset");
      return;
    }
  }

  success (node->test_name);
}

static void
run_parser_tests (void)
{
  GList *node;

  for (node = tests; node; node = node->next)
    run_parser_test (node->data);
}

static void
test_parser (void)
{
  gnc_exp_parser_init ();
  success ("initialize expression parser");

  add_fail_test ("null expression", NULL, -1);
  add_fail_test ("empty expression", "", 0);
  add_fail_test ("whitespace", "  \t\n", 4);
  add_fail_test ("bad expression", "\\", 0);
  add_fail_test ("bad expression", "1 +", 3);
  add_fail_test ("bad expression", "  (5 + 23)/   ", 14);
  add_fail_test ("bad expression", "  ((((5 + 23)/   ", 17);
  add_fail_test ("divide by zero", "  4 / (1 - 1)", -1);

  add_pass_test ("zero", "0", gnc_numeric_zero ());
  add_pass_test ("zero with whitespace", "\n\t   0  ", gnc_numeric_zero ());
  add_pass_test ("1 + 2", NULL, gnc_numeric_create (3, 1));
  add_pass_test ("17.3 - 12.3000", NULL, gnc_numeric_create (5, 1));
  add_pass_test ("5 * 6", NULL, gnc_numeric_create (30, 1));
  add_pass_test (" 34 / (22) ", NULL, gnc_numeric_create (34, 22));
  add_pass_test (" (4 + 5 * 2) - 7 / 3", NULL, gnc_numeric_create (35, 3));

  run_parser_tests ();

  gnc_exp_parser_shutdown ();
  success ("shutdown expression parser");
}

int
main (int argc, char **argv)
{
  /* set_should_print_success (TRUE); */
  test_parser();
  print_test_results();
  exit(get_rv());
}
