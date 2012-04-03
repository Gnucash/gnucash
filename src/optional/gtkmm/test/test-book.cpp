/********************************************************************
 * test-book.cpp: GLib g_test test suite for gnc::Book.		    *
 * Copyright 2011 Christian Stimming
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

#include "config.h"

#include <string.h>
#include <glib.h>

#include "gncmm/Book.hpp"

extern "C" {
#include <qofbook-p.h>
#include <qofbookslots.h>
#include <unittest-support.h>
}

static const gchar *suitename = "/optional/gtkmm";
void test_suite_gtkmm_book();

typedef struct
{
    QofBook *book;
} Fixture;

static struct
{
    guint param;
    gpointer data;
    gboolean called;
    gchar* msg;
} test_struct;

static struct
{
    gboolean col1_called;
    gboolean col2_called;
    gpointer data;
} col_struct;

static void
setup( Fixture *fixture, gconstpointer pData )
{
    fixture->book = qof_book_new();
}

static void
teardown( Fixture *fixture, gconstpointer pData )
{
    qof_book_destroy( fixture->book );
}

/* use g_free on test_struct.msg after this function been called */
static gboolean
handle_faults ( const char * log_domain, GLogLevelFlags log_level, const gchar *msg, gpointer user_data)
{
    test_struct.msg = (gchar *) g_strdup( msg );
    return FALSE;
}

/* mock dirty callback function */
static void
mock_dirty_cb (QofBook *book, gboolean dirty, gpointer user_data)
{
    test_struct.called = TRUE;
    g_test_message( "Checking if book is valid" );
    g_assert( book );
    g_assert( QOF_IS_BOOK( book ) );
    g_test_message( "Checking parameters" );
    g_assert( dirty );
    g_assert( user_data == test_struct.data );
}

/* mock callback for qof_book_foreach_collection testing */
static void
mock_foreach_collection (QofCollection *col, gpointer user_data)
{
    g_test_message( "Checking if collection and data passed correctly" );
    g_assert( col );
    g_assert( user_data == col_struct.data );
    if ( g_strcmp0( qof_collection_get_type(col), "my_type" ) == 0 )
        col_struct.col1_called = TRUE;
    else if ( g_strcmp0( qof_collection_get_type(col), "my_type2" ) == 0 )
        col_struct.col2_called = TRUE;
}

/* mock final callback function */
static void
mock_final_cb (QofBook *book, gpointer key, gpointer user_data)
{
    test_struct.called = TRUE;
    g_assert( book );
    g_assert( QOF_IS_BOOK( book ) );
    g_test_message( "Checking parameters" );
    g_assert_cmpstr( (gchar*)key, == , "key" );
    g_assert_cmpstr( (gchar*)user_data, == , "data" );
}

static void
test_book_readonly( Fixture *fixture, gconstpointer pData )
{
    g_assert( fixture->book != NULL );
    g_assert( !qof_book_is_readonly( fixture->book ) );
    qof_book_mark_readonly( fixture->book );
    g_assert( qof_book_is_readonly( fixture->book ) );
}
static void
test_book_wrap( Fixture *fixture, gconstpointer pData )
{
    g_assert( fixture->book != NULL );
    {
        // WATCH OUT: The second "true" argument is very important, as it says
        // the Glib::RefPtr must not take ownership of the original object.
        Glib::RefPtr<gnc::Book> book = Glib::wrap(fixture->book, true);
        g_assert( book->gobj() == fixture->book );
        g_assert(G_IS_OBJECT(fixture->book));
    }
    g_assert(G_IS_OBJECT(fixture->book)); // All is fine due to the "true" above.
    // The setup() and teardown() indeed uses the GObject ref counting correctly.

    qof_book_mark_readonly( fixture->book );
    g_assert( qof_book_is_readonly( fixture->book ) );
}
static void
test_book_wrap_readonly( Fixture *fixture, gconstpointer pData )
{
    Glib::RefPtr<gnc::Book> book = Glib::wrap(fixture->book, true); // "true" is important!
    g_assert( book );
    g_assert( book->gobj() == fixture-> book); // indeed the identical object
    g_assert( !book->is_readonly() );
    g_assert( !qof_book_is_readonly( fixture->book ) );
    book->mark_readonly();
    g_assert( book->is_readonly() );
    g_assert( qof_book_is_readonly( fixture->book ) ); // indeed the identical object
}
static void
test_book_get_string_option( Fixture *fixture, gconstpointer pData )
{
    Glib::ustring opt_name("Option Name");
    Glib::ustring opt_value("Option Value");
    Glib::ustring opt_name_notset("Not Set");
    Glib::RefPtr<gnc::Book> book = Glib::wrap(fixture->book, true); // "true" is important!
    g_assert( book );
    book->string_option_set( opt_name, opt_value);
    g_assert_cmpstr( book->string_option_get( opt_name ).c_str(), == , opt_value.c_str());
    g_assert( book->string_option_exists(opt_name) == true);
    g_assert( book->string_option_get( opt_name_notset ).empty());
    g_assert( book->string_option_exists( opt_name_notset ) == false);
}

void test_suite_gtkmm_book()
{
    GNC_TEST_ADD( suitename, "readonly", Fixture, NULL, setup, test_book_readonly, teardown );
    GNC_TEST_ADD( suitename, "wrap", Fixture, NULL, setup, test_book_wrap, teardown );
    GNC_TEST_ADD( suitename, "wrapped-readonly", Fixture, NULL, setup, test_book_wrap_readonly, teardown );
    GNC_TEST_ADD( suitename, "wrapped-get_string_option", Fixture, NULL, setup, test_book_get_string_option, teardown );
}
