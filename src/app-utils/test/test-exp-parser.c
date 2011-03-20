#include "config.h"
#include <glib.h>
#include <stdlib.h>
#include <stdio.h>

#include <libguile.h>
#include "gnc-gconf-utils.h"
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
    const char * file;
    int line;
} TestNode;

#define add_pass_test(n, e, r) _add_pass_test((n), (e), (r), __FILE__, __LINE__)

static void
_add_pass_test (const char *test_name, const char *exp, gnc_numeric result, char *file, int line)
{
    TestNode *node = g_new0 (TestNode, 1);

    node->test_name = test_name;
    node->exp = exp ? exp : test_name;
    node->should_succeed = TRUE;
    node->expected_result = result;
    node->file = file;
    node->line = line;

    tests = g_list_append (tests, node);
}


#define add_fail_test(n,e,o) _add_fail_test((n), (e), (o), __FILE__, __LINE__)

static void
_add_fail_test (const char *test_name, const char *exp, int expected_error_offset, char *file, int line)
{
    TestNode *node = g_new0 (TestNode, 1);

    node->test_name = test_name;
    node->exp = exp ? exp : test_name;
    node->should_succeed = FALSE;
    node->expected_error_offset = expected_error_offset;
    node->file = file;
    node->line = line;

    tests = g_list_append (tests, node);
}

static void
run_parser_test (TestNode *node)
{
    gboolean succeeded;
    gnc_numeric result;
    char *error_loc;

    result = gnc_numeric_error( -1 );
    printf("Running test \"%s\" [%s] = ", node->test_name, node->exp);
    succeeded = gnc_exp_parser_parse (node->exp, &result, &error_loc);
    {
        int pass;
        pass = (succeeded == node->should_succeed);
        if ( pass && node->should_succeed )
        {
            pass &= gnc_numeric_equal( result, node->expected_result );
        }
        printf( "%0.4f [%s]\n",
                gnc_numeric_to_double( result ),
                (pass ? "PASS" : "FAIL" ) );
    }

    if (succeeded != node->should_succeed)
    {
        failure_args (node->test_name, node->file, node->line,
                      "parser %s on \"%s\"",
                      succeeded ? "succeeded" : "failed",
                      node->exp);
        return;
    }

    if (succeeded)
    {
        if (!gnc_numeric_equal (result, node->expected_result))
        {
            failure_args (node->test_name, node->file, node->line, "wrong result");
            return;
        }
    }
    else if (node->expected_error_offset != -1)
    {
        if (error_loc != node->exp + node->expected_error_offset)
        {
            failure_args (node->test_name, node->file, node->line, "wrong offset; expected %d, got %d",
                          node->expected_error_offset, (error_loc - node->exp));
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
    /* Bug#334811 - http://bugzilla.gnome.org/show_bug.cgi?id=334811 */
    add_fail_test ("bad expression", "1 2", 3);
    /* Bug#308554 - http://bugzilla.gnome.org/show_bug.cgi?id=308554 */
    add_fail_test ("bad expression", "1 ç", 2);
    add_fail_test ("bad expression", "ç 1", 0);
    add_fail_test ("bad expression", "1 asdf", 6);
    add_fail_test ("bad expression", "asdf 1", 6);
    add_fail_test ("bad expression", "asdf jkl", 8);
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
    add_pass_test( "(a = 42) + (b = 12) - a", NULL, gnc_numeric_create( 12, 1 ) );
    add_fail_test( "AUD $1.23", NULL, 4);
    add_fail_test( "AUD $0.0", NULL, 4);
    add_fail_test( "AUD 1.23", NULL, 8);
    add_fail_test( "AUD 0.0", NULL, 7);
    add_fail_test( "AUD 1.2 + CAN 2.3", NULL, 7);
    add_fail_test( "AUD $1.2 + CAN $2.3", NULL, 4);

    add_pass_test( "1 + 2 * 3 + 4 + 5 * 6 * 7", NULL, gnc_numeric_create(221, 1) );
    add_pass_test( "1 - 2 * 3 + 4 - 5 * 6 * 7", NULL, gnc_numeric_create(-211, 1) );
    add_pass_test( "Conrad's bug",
                   "22.32 * 2 + 16.8 + 34.2 * 2 + 18.81 + 85.44"
                   "- 42.72 + 13.32 + 15.48 + 23.4 + 115.4",
                   gnc_numeric_create(35897, 100) );

    /* This must be defined for the function-parsing to work. */
    scm_c_eval_string("(define (gnc:error->string tag args)   (define (write-error port)     (if (and (list? args) (not (null? args)))         (let ((func (car args)))           (if func               (begin                 (display \"Function: \" port)                 (display func port)                 (display \", \" port)                 (display tag port)                 (display \"\n\n\" port)))))     (false-if-exception      (apply display-error (fluid-ref the-last-stack) port args))     (display-backtrace (fluid-ref the-last-stack) port)     (force-output port))   (false-if-exception    (call-with-output-string write-error)))");

    scm_c_eval_string( "(define (gnc:plus a b) (+ a b))" );
    add_pass_test("plus(2 : 1)", NULL, gnc_numeric_create(3, 1));
    add_fail_test("plus(1:2) plus(3:4)", NULL, 15);
    add_pass_test( "plus( 1 : 2 ) + 3", NULL, gnc_numeric_create( 6, 1 ) );
    add_pass_test( "plus( 1 : 2 ) * 3", NULL, gnc_numeric_create( 9, 1 ) );
    add_pass_test( "plus( 1 + 2 : 3 ) * 5", NULL, gnc_numeric_create( 30, 1 ) );
    add_pass_test( "plus( ( 1 + 2 ) * 3 : 4 ) + 5", NULL, gnc_numeric_create( 18, 1) );
    add_pass_test( "5 + plus( ( 1 + 2 ) * 3 : 4 )", NULL, gnc_numeric_create( 18, 1) );
    add_pass_test( "plus( plus( 1 : 2 ) : 3 )", NULL, gnc_numeric_create( 6, 1 ) );
    add_pass_test( "plus( 4 : plus( plus( 1 : 2 ) : 3))", NULL, gnc_numeric_create( 10, 1 ) );

    scm_c_eval_string( "(define (gnc:sub a b) (- a b))" );
    add_pass_test( "sub( 1 : 2 ) + 4", NULL, gnc_numeric_create( 3, 1 ) );

    add_pass_test( "sub( (1 + 2 * 3) : 4 ) + 5",
                   NULL, gnc_numeric_create( 8, 1 ) );
    add_pass_test( "sub( 1 : 2 ) + sub( 3 : 4 ) + 5",
                   NULL, gnc_numeric_create( 3, 1 ) );
    add_pass_test( "sub( a = 42 : sub( plus( 1 : 2 ) : 6 * 7 )) + a",
                   NULL, gnc_numeric_create( 123, 1 ) );

    scm_c_eval_string( "(define (gnc:test_str str b)"
                       "  (+ b (cond ((equal? str \"one\") 1)"
                       "             ((equal? str \"two\") 2)"
                       "             ((equal? str \"three\") 3)"
                       "             (0))))" );
    add_pass_test( "test_str( \"one\" : 1 )",  NULL, gnc_numeric_create( 2, 1 ) );
    add_pass_test( "test_str( \"two\" : 2 )",  NULL, gnc_numeric_create( 4, 1 ) );
    add_fail_test( "test_str( 3 : \"three\" )", NULL, 23 );
    add_pass_test( "test_str( \"asdf\" : 1 )", NULL, gnc_numeric_create( 1, 1 ) );
    add_fail_test("\"asdf\" + 0", NULL, 8);

    scm_c_eval_string( "(define (gnc:blindreturn val) val)" );
    add_pass_test( "blindreturn( 123.1 )", NULL, gnc_numeric_create( 1231, 10 ) );
    add_pass_test( "blindreturn( 123.01 )", NULL, gnc_numeric_create( 12301, 100 ) );
    add_pass_test( "blindreturn( 123.001 )", NULL, gnc_numeric_create( 123001, 1000 ) );

    run_parser_tests ();

    gnc_exp_parser_shutdown ();
    success ("shutdown expression parser");
}

static void
test_variable_expressions()
{
    gnc_numeric num;
    gchar *errLoc = NULL;
    GHashTable *vars = g_hash_table_new(g_str_hash, g_str_equal);
    do_test(gnc_exp_parser_parse_separate_vars("123 + a", &num, &errLoc, vars), "parsing");
    do_test(g_hash_table_size(vars) == 1, "'a' is the variable; good job, gnc-exp-parser!");
    success("variable found");
}

static void
real_main (void *closure, int argc, char **argv)
{
    /* set_should_print_success (TRUE); */
    test_parser();
    test_variable_expressions();
    print_test_results();
    exit(get_rv());
}

int main ( int argc, char **argv )
{
    /* do things this way so we can test scheme function calls from expressions */
    scm_boot_guile( argc, argv, real_main, NULL );
    return 0;
}
