#include <glib.h>
#include <stdlib.h>
#include <stdio.h>

#include "guile/gh.h"

#include "gnc-exp-parser.h"
#include "gnc-numeric.h"
#include "test-stuff.h"

void real_main (int argc, char **argv);

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

  result = gnc_numeric_error( -1 );
  printf( "Running test \"%s\" =  ", node->test_name );
  succeeded = gnc_exp_parser_parse (node->exp, &result, &error_loc);
  {
    int pass;
    pass = (succeeded == node->should_succeed);
    if ( pass && node->should_succeed ) {
      pass &= gnc_numeric_equal( result, node->expected_result );
    }
    printf( "%0.4f [%s]\n",
            gnc_numeric_to_double( result ),
            (pass ? "PASS" : "FAIL" ) );
  }

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
  add_pass_test ("1 + 2 * 3 + 4 + 5 * 6 * 7", NULL, gnc_numeric_create(221, 1) );
  add_pass_test( "1 - 2 * 3 + 4 - 5 * 6 * 7", NULL,
                 gnc_numeric_create(-211, 1) );
  add_pass_test( "Conrad's bug",
                 "22.32 * 2 + 16.8 + 34.2 * 2 + 18.81 + 85.44"
                 "- 42.72 + 13.32 + 15.48 + 23.4 + 115.4",
                 gnc_numeric_create(35897, 100) );

  gh_eval_str( "(define (gnc:plus a b) (+ a b))" );
  add_pass_test( "plus( 1 : 2 ) + 3", NULL, gnc_numeric_create( 6, 1 ) );
  add_pass_test( "plus( 1 : 2 ) * 3", NULL, gnc_numeric_create( 9, 1 ) );
  add_pass_test( "plus( 1 + 2 : 3 ) * 5", NULL, gnc_numeric_create( 30, 1 ) );
  add_pass_test( "plus( ( 1 + 2 ) * 3 : 4 ) + 5", NULL, gnc_numeric_create( 18, 1) );
  add_pass_test( "5 + plus( ( 1 + 2 ) * 3 : 4 )", NULL, gnc_numeric_create( 18, 1) );
  add_pass_test( "plus( plus( 1 : 2 ) : 3 )", NULL, gnc_numeric_create( 6, 1 ) );
  add_pass_test( "plus( 4 : plus( plus( 1 : 2 ) : 3))", NULL, gnc_numeric_create( 10, 1 ) );

  gh_eval_str( "(define (gnc:foo a b) (- a b))" );
  add_pass_test( "foo( 1 : 2 ) + 4", NULL, gnc_numeric_create( 3, 1 ) );

  add_pass_test( "foo( (1 + 2 * 3) : 4 ) + 5",
                 NULL, gnc_numeric_create( 8, 1 ) );
  add_pass_test( "foo( 1 : 2 ) + foo( 3 : 4 ) + 5",
                 NULL, gnc_numeric_create( 3, 1 ) );
  add_pass_test( "foo( a = 42 : foo( plus( 1 : 2 ) : 6 * 7 )) + a",
                 NULL, gnc_numeric_create( 123, 1 ) );

  gh_eval_str( "(define (gnc:test_str str b)"
               "  (+ b (cond ((equal? str \"one\") 1)"
               "             ((equal? str \"two\") 2)"
               "             ((equal? str \"three\") 3)"
               "             (0))))" );
  add_pass_test( "test_str( \"one\" : 1 )",  NULL, gnc_numeric_create( 2, 1 ) );
  add_pass_test( "test_str( \"two\" : 2 )",  NULL, gnc_numeric_create( 4, 1 ) );
  add_fail_test( "test_str( 3 : \"three\" )", NULL, 0 );
  add_pass_test( "test_str( \"asdf\" : 1 )", NULL, gnc_numeric_create( 1, 1 ) );
  add_fail_test( "\"asdf\" + 0", NULL, 0 );

  gh_eval_str( "(define (gnc:blindreturn val) val)" );
  add_pass_test( "blindreturn( 123.1 )", NULL, gnc_numeric_create( 1231, 10 ) );
  add_pass_test( "blindreturn( 123.01 )", NULL, gnc_numeric_create( 12301, 100 ) );
  add_pass_test( "blindreturn( 123.001 )", NULL, gnc_numeric_create( 123001, 1000 ) );

  run_parser_tests ();

  gnc_exp_parser_shutdown ();
  success ("shutdown expression parser");
}

void
real_main (int argc, char **argv)
{
  /* set_should_print_success (TRUE); */
  test_parser();
  print_test_results();
  exit(get_rv());
}

int main( int argc, char **argv )
{
  /* do things this way so we can test scheme function calls from expressions */
  gh_enter( argc, argv, real_main );
  return 0;
}
