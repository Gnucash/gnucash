/* Modified by bstanley 20010320
 * Added do_test macro, do_test_call and do_test_call_args,
 * print_test_results, set_success_print.
 *
 * Modified by bstanley 20010323
 * removed testing functionality which depends on the rest of gnucash -
 * sepearated into gnc-test-stuff.h
 *
 */

/* Outline of a test program using the new testing functions:
#include "test-stuff.h"
int main( int argc, char* argv[] )
{
	int a, b;
	g_log_set_always_fatal( G_LOG_LEVEL_CRITICAL | G_LOG_LEVEL_WARNING );
	a = b = 1;
	do_test( a == b, "integer equality" );
	do_test( a != b, "integer inequality? (should fail)" );

	do_test_args( a == b, "fancy info", __FILE__, __LINE__, "a = %d, b = %b", a, b );

	print_test_results();
	return get_rv();
}
*/
/* If you want to see test passes, use
set_success_print(TRUE);
before you execute the tests.
Otherwise, only failures are printed out.
*/


#ifndef TEST_STUFF_H
#define TEST_STUFF_H

#include <glib.h>
#include <stdlib.h>

/**
 * Use this to indicate the result of a test.
 * The result is TRUE for success, FALSE for failure.
 * title describes the test
 * Tests are automatically identified by their source file and line.
 */
#define do_test( result, title ) do_test_call( result, title, __FILE__, __LINE__ )
#define success( title ) success_call( title, __FILE__, __LINE__ );
#define failure( title ) failure_call( title, __FILE__, __LINE__ );

/** This one doesn't work because macros can't take a variable number of arguments.
 * well, apparently gcc can, but it's non-standard.
 * Apparently C99 can, too, but it's not exactly standard either.
#define do_test_args( result, title, format ) do_test_call( result, title, __FILE__, __LINE__, format, ... );
*/

/* Privately used to indicate a test result. You may use these if you
 * wish, but it's easier to use the do_test macro above.
 */
gboolean do_test_call(
    gboolean result,
    const char* test_title,
    const char* filename,
    int line );
gboolean do_test_args(
    gboolean result,
    const char* test_title,
    const char* filename,
    int line,
    const char* format, ... );


/**
 * Prints out the number of tests passed and failed.
 */
void print_test_results(void);

/**
 * Use this to set whether successful tests
 * should print a message.
 * Default is false.
 * Successful test messages are useful while initally constructing the
 * test suite, but when it's completed, no news is good news.
 * A successful test run will be indicated by the message
 * from print_test_results().
 */
void set_success_print( gboolean in_should_print );

/* Value to return from main. Set to 1 if there were any fails, 0 otherwise. */
int get_rv(void);

/** Testing primitives.
 * Sometimes you just have to put the results of
 * a test into different forks of the code.
 */
void success_call(
    const char *test_title,
    const char *file,
    int line );

void success_args(
    const char *test_title,
    const char *file,
    int line,
    const char *format,
    ... );

void failure_call(
    const char *test_title,
    const char *file,
    int line);

void failure_args(
    const char *test_title,
    const char *file,
    int line,
    const char *format,
    ... );

gboolean get_random_boolean(void);
gint get_random_int_in_range(int start, int end);
void random_character_include_funky_chars (gboolean use_funky_chars);
gchar get_random_character(void);
gchar* get_random_string(void);
gchar * get_random_string_length_in_range(int minlen, int maxlen);
gchar* get_random_string_without(const char *exclude_chars);
gint64 get_random_gint64(void);
double get_random_double(void);
const char* get_random_string_in_array(const char* str_list[]);

#endif /* TEST_STUFF_H */
