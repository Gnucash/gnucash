/********************************************************************\
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#include <config.h>
#include <glib.h>
#include <stdlib.h>
#include <stdio.h>

#include <libguile.h>
#include "gnc-exp-parser.h"
#include "gnc-numeric.h"
#include "test-stuff.h"
#include <unittest-support.h>

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
    gchar *msg = "[func_op()] function eval error: [[func_op(]\n";
    guint loglevel = G_LOG_LEVEL_CRITICAL, hdlr;
    TestErrorStruct check = { loglevel, "gnc.gui", msg };

    result = gnc_numeric_error( -1 );
    hdlr = g_log_set_handler ("gnc.gui", loglevel,
                              (GLogFunc)test_checked_handler, &check);
    g_test_message ("Running test \"%s\" [%s] = ", node->test_name, node->exp);
    succeeded = gnc_exp_parser_parse (node->exp, &result, &error_loc);
    g_log_remove_handler ("gnc.gui", hdlr);
    {
        int pass;
        pass = (succeeded == node->should_succeed);
        if ( pass && node->should_succeed )
        {
            pass &= gnc_numeric_equal( result, node->expected_result );
        }
        g_test_message ( "%0.4f [%s]\n",
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

    /* gnc:apply-with-error-handling must be defined because it's used
     * indirectly through gfec_apply by the expression parser */
    scm_c_eval_string("(define (gnc:apply-with-error-handling cmd args)  (let ((captured-stack #f)  (captured-error #f)  (result #f))  (catch #t  (lambda ()  (if (procedure? cmd)  (set! result (apply cmd args)))  (if (string? cmd)  (set! result (eval-string cmd))))  (lambda (key . parameters)  (let* ((str-port (open-output-string)))  (display-backtrace captured-stack str-port)  (display \"\n\" str-port)  (print-exception str-port #f key parameters)  (set! captured-error (get-output-string str-port))))  (lambda (key . parameters)  (set! captured-stack (make-stack #t 3))))  (list result captured-error)))");
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
