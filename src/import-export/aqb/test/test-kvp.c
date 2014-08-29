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

#include <config.h>
#include <glib.h>
/* This is optional; you only need it if you have external fixtures or mocks. */
//#include "test_module_support.h"
/* Header for this module and any others that you need */
//#include <module_1.h>
#include "gnc-ab-kvp.h"

/* Declare the test path for the suite. g_test_add_func automatically
 * creates a nested set of test suites for us based on this path. */
static const gchar *suitename = "aqbanking/kvp";

/* Test fixture: A struct, a setup function, and a teardown function are passed to g_test_add(); add getters, setters, and whatever other functions you need to call from your test functions. */
typedef struct
{
    gint foo;
    gdouble bar;
} Fixture;

static void
setup_module_test(Fixture *fixture, gconstpointer pData)
{
    /* Do something useful */
}

static void
teardown_module_test(Fixture *fixture, gconstpointer pData)
{
    /* Clean up after ourselves */
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
GTestSuite*
test_suite_kvp ( void )
{
#define TOP_SRCDIR "../../../../../"
    /* load the accounts from the users datafile */
    /* but first, check to make sure we've got a session going. */
    QofSession *new_session;
    const char* newfile;
    QofBackendError io_err;

#if 0
    // A file with no content at all, but a valid XML file
    new_session = qof_session_new ();
    newfile = "file://" TOP_SRCDIR "src/import-export/aqb/test/file-book.gnucash";
    qof_session_begin (new_session, newfile, TRUE, FALSE, FALSE);
    io_err = qof_session_get_error (new_session);
    printf("io_err1 = %d\n", io_err);
    g_assert(io_err != ERR_BACKEND_NO_HANDLER); // Do not have no handler

    g_assert(io_err != ERR_BACKEND_NO_SUCH_DB); // DB must exist
    g_assert(io_err != ERR_BACKEND_LOCKED);
    g_assert(io_err == 0);

    {
        GList *mylist = gnc_ab_get_book_template_list(qof_session_get_book(new_session));
        g_assert(mylist == 0);
    }
#endif

    //qof_session_destroy(new_session);

#if 0
    // A file with no content except for the book_template_list kvp
    // slot
    new_session = qof_session_new ();
    newfile = "file://" TOP_SRCDIR "src/import-export/aqb/test/file-book-hbcislot.gnucash";
    qof_session_begin (new_session, newfile, TRUE, FALSE, FALSE);
    io_err = qof_session_get_error (new_session);
    printf("io_err1 = %d\n", io_err);
    g_assert(io_err != ERR_BACKEND_NO_HANDLER); // Do not have no handler

    g_assert(io_err != ERR_BACKEND_NO_SUCH_DB); // DB must exist
    g_assert(io_err != ERR_BACKEND_LOCKED);
    g_assert(io_err == 0);

    {
        GList *mylist = gnc_ab_get_book_template_list(qof_session_get_book(new_session));
        g_assert(mylist != 0); // do we have the slot?!
    }

#endif

#if 0
    g_test_add_func( suitename, test_function );
    g_test_add_data_func( suitename, (gconstpointer)(&data),
                          test_function_with_data );
    g_test_add( suitename, Fixture,
                data,
                setup_module_test,
                test_function_with_fixture,
                teardown_module_test);
#endif
}
