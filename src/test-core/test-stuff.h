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
#include <qof.h>

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

/**
 * Use this macro to format informative test path output when using g_test_add.
 * Suite stands for tests' pack, while path for individual test name.
*/

#define GNC_TEST_ADD( suite, path, fixture, data, setup, test, teardown )\
{\
    gchar *testpath = g_strdup_printf( "%s/%s", suite, path );\
    g_test_add( testpath, fixture, data, setup, test, teardown );\
    g_free( testpath );\
}

/**
 * Use this macro to format informative test path output when using g_test_add_func.
 * Suite stands for tests' pack, while path for individual test name.
*/

#define GNC_TEST_ADD_FUNC( suite, path, test )\
{\
    gchar *testpath = g_strdup_printf( "%s/%s", suite, path );\
    g_test_add_func( testpath, test );\
    g_free( testpath );\
}

/**
 * Suppressing Expected Errors
 *
 * Functions for suppressing expected errors during tests. Pass
 *
 * Note that you need to call both g_log_set_handler *and*
 * g_test_log_set_fatal_handler to both avoid the assertion and
 * suppress the error message. The callbacks work in either role, just
 * cast them appropriately for the use.
 */

/**
 * Struct to pass as user_data for the handlers. Setting a parameter
 * to NULL or 0 will match any value in the error, so if you have the
 * same message and log level being issued in two domains you can
 * match both of them by setting log_domain = NULL.
 *
 */

typedef struct
{
    GLogLevelFlags log_level;
    gchar *log_domain;
    gchar *msg;
} TestErrorStruct;

/**
 * Check the user_data against the actual error and assert on any
 * differences.  Displays the error (and asserts if G_LOG_FLAG_FATAL
 * is TRUE) if NULL is passed as user_data, but a NULL or 0 value
 * member matches anything.
 */
gboolean test_checked_handler (const char *log_domain, GLogLevelFlags log_level,
                               const gchar *msg, gpointer user_data);

/**
 * Just print the log message. Since GLib has a habit of eating its
 * log messages, it's sometimes useful to call
 * g_test_log_set_fatal_handler() with this to make sure that
 * g_return_if_fail() error messages make it to the surface.
 */
gboolean test_log_handler (const char *log_domain, GLogLevelFlags log_level,
			   const gchar *msg, gpointer user_data);
/**
 * Just returns FALSE or suppresses the message regardless of what the
 * error is. Use this only as a last resort.
 */
gboolean test_null_handler (const char *log_domain, GLogLevelFlags log_level,
                            const gchar *msg, gpointer user_data );
/**
 * Maintains an internal list of TestErrorStructs which are each
 * checked by the list handler. If an error matches any entry on the
 * list, test_list_handler will return FALSE, blocking the error from
 * halting the program.
 *
 * Call test_add_error for each TestErrorStruct to check against and
 * test_clear_error_list when you no longer expect the errors.
 */
void test_add_error (TestErrorStruct *error);
void test_clear_error_list (void);

/**
 * Checks received errors against the list created by
 * test_add_error. If the list is empty or nothing matches, passes
 * control on to test_checked_handler, giving the opportunity for an
 * additional check that's not in the list (set user_data to NULL if
 * you want test_checked_handler to immediately print the error).
 */
gboolean test_list_handler (const char *log_domain,
                            GLogLevelFlags log_level,
                            const gchar *msg, gpointer user_data );
/**
 * Call this from a mock object to indicate that the mock has in fact
 * been called
 */
void test_set_called( const gboolean val );

/**
 * Destructively tests (meaning that it resets called to FALSE) and
 * returns the value of called.
 */
gboolean test_reset_called( void );

/**
 * Set the test data pointer with the what you expect your mock to be
 * called with.
 */
void test_set_data( gpointer data );

/**
 * Destructively retrieves the test data pointer. Call from your mock
 * to ensure that it received the expected data.
 */
gpointer test_reset_data( void );

/**
 * A handy function to use to free memory from lists of simple
 * pointers. Call g_list_free_full(list, (GDestroyNotify)*test_free).
 */
void test_free( gpointer data );

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

/* TestSignal is an opaque struct used to mock handling signals
 * emitted by functions-under-test. It registers a handler and counts
 * how many times it is called with the right instance and type.  The
 * struct is allocated using g_slice_new, and it registers a
 * qof_event_handler; test_signal_free cleans up at the end of the
 * test function (or sooner, if you want to reuse a TestSignal).  If
 * event_data isn't NULL, the mock signal handler will test that it
 * matches the event_data passed with the signal and assert if it
 * isn't the same object (pointer comparison). If the actual event
 * data is a local variable, it won't be accessible, so the event_data
 * passed to test_signal_new should be NULL to avoid the test.
 */
typedef gpointer TestSignal;
TestSignal test_signal_new (QofInstance *entity, QofEventId eventType,
                            gpointer event_data);
/* test_signal_return_hits gets the number of times the TestSignal has
 * been called.
 */
guint test_signal_return_hits (TestSignal sig);

/* test_signal_assert_hits is a convenience macro which wraps
 * test_signal_return_hits with and equality assertion.
 */

#define test_signal_assert_hits(sig, hits) \
    g_assert_cmpint (test_signal_return_hits (sig), ==, hits)

void test_signal_free (TestSignal sig);

/* test_object_checked_destroy unrefs obj and returns true if its finalize
 * method was called.
 */

gboolean test_object_checked_destroy (GObject *obj);

/**
 * test_destroy() ensures that a GObject is still alive at the time
 * it's called and that it is finalized. The first assertion will
 * trigger if you pass it a ponter which isn't a GObject -- which
 * could be the case if the object has already been finalized. Then it
 * calls test_object_checked_destroy() on it, asserting if the
 * finalize method wasn't called (which indicates a leak).
 */

#define test_destroy(obj) \
    g_assert (obj != NULL && G_IS_OBJECT (obj));		\
    g_assert (test_object_checked_destroy (G_OBJECT (obj)))

/* For Scheme testing access:
void gnc_log_init_filename_special (gchar *filename);
void gnc_log_shutdown (void);
void gnc_log_set_handler (guint logdomain, gchar *logdomain, GLogFunc * func, gpointer data);
*/

#endif /* TEST_STUFF_H */
