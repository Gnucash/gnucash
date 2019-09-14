/********************************************************************
 * test_submodule.c: Example GLib g_test test suite.		    *
 * Copyright 2011 John Ralls <jralls@ceridwen.us>		    *
 *                                                                  *
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
\********************************************************************/

/* This is a template test suite. Copy it to the test directory and name it for
 * the corresponding file you're testing with a "utest-" prefix. Add it to the
 * test_program_SOURCES in Makefile.am.
 */
#include <config.h>
#include <glib.h>
#include <unittest-support.h>
/* Header for this module and any others that you need */
#include <module_1.h>

/* Declare the test path for the suite. g_test_add_func automatically
 * creates a nested set of test suites for us based on this path. */
static const gchar *suitename = "module/module_1";

/* Test fixture: A struct, a setup function, and a teardown function are passed
 * to g_test_add(); add getters, setters, and whatever other functions you need
 * to call from your test functions.
 */
typedef struct
{
    gint foo;
    gdouble bar;
} Fixture;

static void
setup (Fixture *fixture, gconstpointer pData)
{
    /* Whatever is needed to populate the fixture and initialize your module for
     * running a single test.
     */
}

static void
teardown (Fixture *fixture, gconstpointer pData)
{
    /* Whatever cleanup is needed at the end of the test. */
}

static void
test_function( void )
{
    /* A simple test function */
}

static void
test_function_with_data( gconstpointer data )
{
    /* a more complicated function that needs arguments at invocation */
}

static void
test_function_with_fixture( Fixture *fixture, gconstpointer pData )
{
    /* A really complicated function that needs an external test fixture */
}

static void
test_performance_function( void )
{
    /* A slow function that measures performance of some critical
     * routine. Note g_test_timer functions for simple performance
     * measurements. */
}

/* Assert macros that you can use in your test functions. "cmp" is a
 * comparison operator, one of ==, !=, <, >, <=, >=.
 *
 *  g_assert( boolean_expression )
 *  g_assert_not_reached()
 *  g_assert_cmpstr( gchar *s1, cmp, gchar *s2 )
 *  g_assert_cmpint( int s1, cmp, int s2 )
 *  g_assert_cmpuint( unsigned int s1, cmp, unsigned int s2 )
 *  g_assert_cmphex( unsigned int s1, cmp, unsigned int s2 )
 *  g_assert_cmpfloat( double s1, cmp, double s2 )
 *  g_assert_no_error( GError *err )
 *  g_assert_error( GError *err, GQuark domain, gint code )
 *
 * You can also emit arbitrary messages into the test report with
 *  g_test_message( const char* format, ... )
 */
void
test_suite_module1 (void)
{
    Datatype data = something();
    GNC_TEST_ADD_FUNC (suitename, "Test Name 1", test_function);
    {
        gchar *testpath = g_strdup_printf ("%s/Test Name 2", suitename);
        g_test_add_data_func( suitename, (gconstpointer)(&data),
                              test_function_with_data );
        g_free (testpath);
    }
    GNC_TEST_ADD (suitename, "Test Name 3", Fixture, data, setup,
                  test_function_with_fixture, teardown);
    /* Other conditionals are g_test_quick(), g_test_slow(), and
     * g_test_thorough() */
    if ( g_test_perf() )
    {
        GNC_TEST_ADD_FUNC (suitename, "Test Name 4", test_performance_func);
    }
}
