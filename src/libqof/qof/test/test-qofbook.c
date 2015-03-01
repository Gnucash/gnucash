/********************************************************************
 * test_qofbook.c: GLib g_test test suite for qofbook.		    *
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
#ifdef __cplusplus
extern "C"
{
#endif

#include "config.h"
#include <string.h>
#include <glib.h>
#include <unittest-support.h>
#ifdef __cplusplus
}
#endif

#include "../qof.h"
#include "../qofbook-p.h"
#include "../qofbookslots.h"

#ifdef HAVE_GLIB_2_38
#define _Q "'"
#else
#define _Q "`"
#endif

static const gchar *suitename = "/qof/qofbook";
void test_suite_qofbook ( void );

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
test_book_validate_counter( void )
{
    gchar *r;
    g_test_bug("644036");

    /* Test for detection of missing format conversion */
    r = qof_book_validate_counter_format("This string is missing the conversion specifier");
    g_assert(r);
    if (r && g_test_verbose())
    {
        g_test_message("Counter format validation correctly failed: %s", r);
    }
    g_free(r);

    /* Test the usual Linux/Unix G_GINT64_FORMAT */
    r = qof_book_validate_counter_format_internal("Test - %li", "li");
    if (r && g_test_verbose())
    {
        g_test_message("Counter format validation erroneously failed: %s", r);
    }
    g_assert(r == NULL);
    g_free(r);

    /* Test the Windows G_GINT64_FORMAT */
    r = qof_book_validate_counter_format_internal("Test - %I64i", "I64i");
    if (r && g_test_verbose())
    {
        g_test_message("Counter format validation erroneously failed: %s", r);
    }
    g_assert(r == NULL);
    g_free(r);

    /* Test the system's GINT64_FORMAT */
    r = qof_book_validate_counter_format("Test - %" G_GINT64_FORMAT);
    if (r && g_test_verbose())
    {
        g_test_message("Counter format validation erroneously failed: %s", r);
    }
    g_assert(r == NULL);
    g_free(r);

    /* Test an erroneous Windows G_GINT64_FORMAT */
    r = qof_book_validate_counter_format_internal("Test - %li", "I64i");
    if (r && g_test_verbose())
    {
        g_test_message("Counter format validation correctly failed: %s", r);
    }
    g_assert(r);
    g_free(r);

    /* Test an erroneous Linux G_GINT64_FORMAT */
    r = qof_book_validate_counter_format_internal("Test - %I64i", "li");
    if (r && g_test_verbose())
    {
        g_test_message("Counter format validation correctly failed: %s", r);
    }
    g_assert(r);
    g_free(r);
}

static void
test_book_get_string_option( Fixture *fixture, gconstpointer pData )
{
    const char *opt_name = "Option Name";
    const char *opt_value = "Option Value";
    const char *opt_name_notset = "Not Set";
    g_assert( fixture->book != NULL );
    qof_book_set_string_option( fixture->book, opt_name, opt_value);
    g_assert_cmpstr( qof_book_get_string_option( fixture->book, opt_name ), == , opt_value);
    g_assert_cmpstr( qof_book_get_string_option( fixture->book, opt_name_notset ), == , NULL );
}

static void
test_book_set_string_option( Fixture *fixture, gconstpointer pData )
{
    const char *opt_name = "Option Name";
    const char *opt_value = "Option Value";
    g_assert( fixture->book != NULL );
    qof_book_set_string_option( fixture->book, opt_name, opt_value);
    g_assert( qof_instance_is_dirty (QOF_INSTANCE (fixture->book)) );
}

static void
test_book_session_not_saved( Fixture *fixture, gconstpointer pData )
{
    g_assert( fixture->book != NULL );
    g_assert( !qof_book_session_not_saved( fixture->book ) );
    qof_book_mark_session_saved( fixture->book );
    g_assert( !qof_book_session_not_saved( fixture->book ) );
    qof_book_mark_session_dirty( fixture-> book );
    g_assert( qof_book_session_not_saved( fixture->book ) );
}

static void
test_book_mark_session_saved( Fixture *fixture, gconstpointer pData )
{
    time64 dirty_time, clean_time;

    qof_book_mark_session_dirty( fixture-> book );
    g_assert( qof_book_session_not_saved( fixture->book ) );
    dirty_time = qof_book_get_session_dirty_time( fixture->book );
    qof_book_mark_session_saved( fixture->book );
    clean_time = qof_book_get_session_dirty_time( fixture->book );
    g_assert( !qof_book_session_not_saved( fixture->book ) );
    g_assert( dirty_time != clean_time );
    g_assert( clean_time == 0);
}

static void
test_book_get_counter( Fixture *fixture, gconstpointer pData )
{
    const char *counter_name = "Counter name";
    const char *err_no_book = "No book";
    const char *err_invalid_cnt = "Invalid counter name";
    gint64 counter;

    /* need this as long as we have fatal warnings enabled */
    g_test_log_set_fatal_handler ( ( GTestLogFatalFunc )handle_faults, NULL );

    counter = qof_book_get_counter( NULL, counter_name );
    g_assert_cmpint( counter, == , -1 );
    g_assert( g_strrstr( test_struct.msg, err_no_book ) != NULL );
    g_free( test_struct.msg );

    counter = qof_book_get_counter( fixture->book, NULL );
    g_assert_cmpint( counter, == , -1 );
    g_assert( g_strrstr( test_struct.msg, err_invalid_cnt ) != NULL );
    g_free( test_struct.msg );

    counter = qof_book_get_counter( fixture->book, NULL );
    g_assert_cmpint( counter, == , -1 );
    g_assert( g_strrstr( test_struct.msg, err_invalid_cnt ) != NULL );
    g_free( test_struct.msg );

    counter = qof_book_get_counter( fixture->book, counter_name );
    g_assert_cmpint( counter, == , 0 );

    qof_book_increment_and_format_counter( fixture->book, counter_name );
    counter = qof_book_get_counter( fixture->book, counter_name );
    g_assert_cmpint( counter, == , 1 );
}

static void
test_book_get_counter_format ( Fixture *fixture, gconstpointer pData )
{
    const char *counter_name = "Counter name";
    const char *err_no_book = "No book";
    const char *err_invalid_cnt = "Invalid counter name";
    const char *r;

    /* need this as long as we have fatal warnings enabled */
    g_test_log_set_fatal_handler ( ( GTestLogFatalFunc )handle_faults, NULL );

    g_test_message( "Testing counter format when book is null" );
    r = qof_book_get_counter_format( NULL, counter_name );
    g_assert_cmpstr( r, == , NULL );
    g_assert( g_strrstr( test_struct.msg, err_no_book ) != NULL );
    g_free( test_struct.msg );

    g_test_message( "Testing counter format when counter name is null" );
    r = qof_book_get_counter_format( fixture->book, NULL );
    g_assert_cmpstr( r, == , NULL );
    g_assert( g_strrstr( test_struct.msg, err_invalid_cnt ) != NULL );
    g_free( test_struct.msg );

    g_test_message( "Testing counter format when counter name is empty string" );
    r = qof_book_get_counter_format( fixture->book, NULL );
    g_assert_cmpstr( r, == , NULL );
    g_assert( g_strrstr( test_struct.msg, err_invalid_cnt ) != NULL );
    g_free( test_struct.msg );

    g_test_message( "Testing counter format with existing counter" );
    r = qof_book_get_counter_format( fixture->book, counter_name );
    g_assert_cmpstr( r, == , "%.6" G_GINT64_FORMAT);

    g_test_message( "Testing counter format for default value" );
    r = qof_book_get_counter_format( fixture->book, counter_name );
    g_assert_cmpstr( r, == , "%.6" G_GINT64_FORMAT);
}

static void
test_book_increment_and_format_counter ( Fixture *fixture, gconstpointer pData )
{
    const char *counter_name = "Counter name";
    const char *err_no_book = "No book";
    const char *err_invalid_cnt = "Invalid counter name";
    const char *format;
    char *r;
    gint64 counter;

    /* need this as long as we have fatal warnings enabled */
    g_test_log_set_fatal_handler ( ( GTestLogFatalFunc )handle_faults, NULL );

    g_test_message( "Testing increment and format when book is null" );
    r = qof_book_increment_and_format_counter( NULL, counter_name );
    g_assert_cmpstr( r, == , NULL );
    g_free( r );
    g_assert( g_strrstr( test_struct.msg, err_no_book ) != NULL );
    g_free( test_struct.msg );

    g_test_message( "Testing increment and format when counter name is null" );
    r = qof_book_increment_and_format_counter( fixture->book, NULL );
    g_assert_cmpstr( r, == , NULL );
    g_free( r );
    g_assert( g_strrstr( test_struct.msg, err_invalid_cnt ) != NULL );
    g_free( test_struct.msg );

    g_test_message( "Testing increment and format when counter name is empty string" );
    r = qof_book_increment_and_format_counter( fixture->book, NULL );
    g_assert_cmpstr( r, == , NULL );
    g_free( r );
    g_assert( g_strrstr( test_struct.msg, err_invalid_cnt ) != NULL );
    g_free( test_struct.msg );

    g_test_message( "Testing increment and format with new counter" );
    r = qof_book_increment_and_format_counter( fixture->book, counter_name );
    counter = qof_book_get_counter( fixture->book, counter_name );
    format = qof_book_get_counter_format( fixture->book, counter_name );
    g_assert_cmpint( counter, == , 1 );
    g_assert( qof_instance_is_dirty (QOF_INSTANCE (fixture->book)) );
    g_assert_cmpstr( r, == , g_strdup_printf( format, counter ));
    g_free( r );

    g_test_message( "Testing increment and format with existing counter" );
    r = qof_book_increment_and_format_counter( fixture->book, counter_name );
    counter = qof_book_get_counter( fixture->book, counter_name );
    format = qof_book_get_counter_format( fixture->book, counter_name );
    g_assert_cmpint( counter, == , 2 );
    g_assert_cmpstr( r, == , g_strdup_printf( format, counter ));
    g_free( r );
}

/* keep this testing of trading accounts, while adding testing of currency-
   accounting-based trading accounts, so that files prior to version 2.7
   can be read/processed */
static void
test_book_use_trading_accounts( Fixture *fixture, gconstpointer pData )
{
    g_assert( qof_book_use_trading_accounts( fixture-> book ) == FALSE );

    g_test_message( "Testing with existing trading accounts set to true - t" );
    qof_book_begin_edit (fixture->book);
    qof_instance_set (QOF_INSTANCE (fixture->book),
		      "trading-accts", "t",
		      NULL);
    g_assert( qof_book_use_trading_accounts( fixture-> book ) == TRUE );

    g_test_message( "Testing with existing trading accounts and incorrect value - tt" );
    qof_instance_set (QOF_INSTANCE (fixture->book),
		      "trading-accts", "tt",
		      NULL);
    g_assert( qof_book_use_trading_accounts( fixture-> book ) == FALSE );
    qof_book_commit_edit (fixture->book);

}

static void
test_book_use_trading_accounts_currency_accounting( Fixture *fixture, gconstpointer pData )
{
    g_assert( qof_book_use_trading_accounts( fixture-> book ) == FALSE );

    g_test_message( "Testing with existing currency-accounting set to 'trading'" );
    qof_book_begin_edit (fixture->book);
    qof_instance_set (QOF_INSTANCE (fixture->book),
		      "currency-accounting", "trading",
		      NULL);
    g_assert( qof_book_use_trading_accounts( fixture-> book ) == TRUE );

    g_test_message( "Testing with existing currency-accounting set to 'book-currency'" );
    qof_instance_set (QOF_INSTANCE (fixture->book),
		      "currency-accounting", "book-currency",
		      NULL);
    g_assert( qof_book_use_trading_accounts( fixture-> book ) == FALSE );
    qof_book_commit_edit (fixture->book);

}

static void
test_book_use_book_currency( Fixture *fixture, gconstpointer pData )
{
    const char *cur;

    g_assert( qof_book_use_book_currency( fixture-> book ) == FALSE );
    g_assert( qof_book_get_book_currency_unique_name( fixture-> book ) == FALSE );

    qof_book_begin_edit (fixture->book);
    g_test_message( "Testing with currency-accounting set to 'trading' and no book-currency" );
    qof_instance_set (QOF_INSTANCE (fixture->book),
		      "currency-accounting", "trading",
		      NULL);
    g_assert( qof_book_use_book_currency( fixture-> book ) == FALSE );
    g_assert( qof_book_get_book_currency_unique_name( fixture-> book ) == FALSE );

    g_test_message( "Testing with currency-accounting set to 'book-currency' and no book-currency set" );
    qof_instance_set (QOF_INSTANCE (fixture->book),
		      "currency-accounting", "book-currency",
		      NULL);
    g_assert( qof_book_use_book_currency( fixture-> book ) == FALSE );
    g_assert( qof_book_get_book_currency_unique_name( fixture-> book ) == FALSE );

    g_test_message( "Testing with currency-accounting set to 'book-currency' and  book-currency set" );
    qof_instance_set (QOF_INSTANCE (fixture->book),
		      "book-currency", "CURRENCY::USD",
		      NULL);
    g_assert( qof_book_use_book_currency( fixture-> book ) == TRUE );
    cur = qof_book_get_book_currency_unique_name( fixture->book );
    g_assert_cmpstr( cur, == , "CURRENCY::USD");

    g_test_message( "Testing with currency-accounting set to 'trading' and book-currency still set" );
    qof_instance_set (QOF_INSTANCE (fixture->book),
		      "currency-accounting", "trading",
		      NULL);
    g_assert( qof_book_use_book_currency( fixture-> book ) == FALSE );
    g_assert( qof_book_get_book_currency_unique_name( fixture-> book ) == FALSE );
    qof_book_commit_edit (fixture->book);

}

static void
test_book_get_num_days_autofreeze( Fixture *fixture, gconstpointer pData )
{
    g_test_message( "Testing default: No auto-freeze days are set" );
    g_assert( qof_book_uses_autoreadonly( fixture-> book ) == FALSE );
    g_assert( qof_book_get_num_days_autoreadonly( fixture-> book ) == 0 );

    g_assert( qof_book_uses_autoreadonly( fixture-> book ) == FALSE );
    g_assert( qof_book_get_num_days_autoreadonly( fixture-> book ) == 0 );

    qof_book_begin_edit (fixture->book);
    qof_instance_set (QOF_INSTANCE (fixture->book),
		      "autoreadonly-days", (gdouble)17,
		      NULL);
    g_assert( qof_book_uses_autoreadonly( fixture-> book ) == TRUE );
    g_assert( qof_book_get_num_days_autoreadonly( fixture-> book ) == 17 );

    g_test_message( "Testing when setting this correctly to zero again" );

    qof_instance_set (QOF_INSTANCE (fixture->book),
		      "autoreadonly-days", (gdouble)0,
		      NULL);
    g_assert( qof_book_uses_autoreadonly( fixture-> book ) == FALSE );
    g_assert( qof_book_get_num_days_autoreadonly( fixture-> book ) == 0 );

    qof_instance_set (QOF_INSTANCE (fixture->book),
		      "autoreadonly-days", (gdouble)32,
		      NULL);
    g_assert( qof_book_uses_autoreadonly( fixture-> book ) == TRUE );
    g_assert( qof_book_get_num_days_autoreadonly( fixture-> book ) == 32 );

    qof_book_commit_edit (fixture->book);
}

static void
test_book_use_split_action_for_num_field( Fixture *fixture, gconstpointer pData )
{
    g_test_message( "Testing default: No selection has been specified" );
    g_assert( qof_book_use_split_action_for_num_field( fixture-> book ) == FALSE );

    g_test_message( "Testing with existing use split action for num set to true - t" );

    qof_book_begin_edit (fixture->book);
    qof_instance_set (QOF_INSTANCE (fixture->book),
		      "split-action-num-field", "t",
		      NULL);
    g_assert( qof_book_use_split_action_for_num_field( fixture-> book ) == TRUE );

    g_test_message( "Testing with existing use split action for num and incorrect value - tt" );
    qof_instance_set (QOF_INSTANCE (fixture->book),
		      "split-action-num-field", "tt",
		      NULL);
    g_assert( qof_book_use_split_action_for_num_field( fixture-> book ) == FALSE );
    qof_book_commit_edit (fixture->book);
}

static void
test_book_mark_session_dirty( Fixture *fixture, gconstpointer pData )
{
    QofBook *_empty = NULL;
    time64 before, after;
    guint param = (guint) g_test_rand_int();

    g_test_message( "Testing when book is NULL" );
    qof_book_mark_session_dirty( _empty );
    g_assert( _empty == NULL );

    g_test_message( "Testing when book is not dirty and dirty_cb is null" );
    g_assert_cmpint( qof_book_get_session_dirty_time( fixture->book ), == , 0);
    g_assert( fixture->book->dirty_cb == NULL );
    g_assert( qof_book_session_not_saved( fixture->book ) == FALSE );
    before = gnc_time (NULL);
    qof_book_mark_session_dirty( fixture->book );
    after = gnc_time (NULL);
    g_assert_cmpint( qof_book_get_session_dirty_time( fixture->book ), >= , before);
    g_assert_cmpint( qof_book_get_session_dirty_time( fixture->book ), <= , after);
    g_assert( qof_book_session_not_saved( fixture->book ) == TRUE );

    g_test_message( "Testing when book is not dirty and dirty_cb is not null" );
    /* prepare conditions */
    qof_book_mark_session_saved( fixture->book );
    qof_book_set_dirty_cb( fixture->book, mock_dirty_cb, (gpointer) (&param) );
    test_struct.data = (gpointer) (&param);
    test_struct.called = FALSE;
    g_assert( fixture->book->dirty_cb != NULL );
    g_assert_cmpint( qof_book_get_session_dirty_time( fixture->book ), == , 0);
    g_assert( qof_book_session_not_saved( fixture->book ) == FALSE );
    /* run FUT */
    before = gnc_time (NULL);
    qof_book_mark_session_dirty( fixture->book );
    after = gnc_time (NULL);
    /* test output */
    g_assert_cmpint( qof_book_get_session_dirty_time( fixture->book ), >= , before);
    g_assert_cmpint( qof_book_get_session_dirty_time( fixture->book ), <= , after);
    g_assert( qof_book_session_not_saved( fixture->book ) == TRUE );
    g_assert( test_struct.called );

    g_test_message( "Testing when book is dirty" );
    g_assert( qof_book_session_not_saved( fixture->book ) == TRUE );
    before = qof_book_get_session_dirty_time( fixture->book );
    qof_book_mark_session_dirty( fixture->book );
    g_assert( qof_book_session_not_saved( fixture->book ) == TRUE );
    after = qof_book_get_session_dirty_time( fixture->book );
    g_assert_cmpint( before, == , after );
}

static void
test_book_get_session_dirty_time( Fixture *fixture, gconstpointer pData )
{
    time64 before, after;

    g_test_message( "Testing time on saved book = 0" );
    g_assert( qof_book_session_not_saved( fixture->book ) == FALSE );
    g_assert_cmpint( qof_book_get_session_dirty_time( fixture->book ), == , 0);

    g_test_message( "Testing time on dirty book is correct" );
    before = gnc_time (NULL);
    qof_book_mark_session_dirty( fixture->book );
    after = gnc_time (NULL);
    g_assert_cmpint( qof_book_get_session_dirty_time( fixture->book ), >= , before);
    g_assert_cmpint( qof_book_get_session_dirty_time( fixture->book ), <= , after);

}

static void
test_book_set_dirty_cb( Fixture *fixture, gconstpointer pData )
{
    const char * error_msg = "Already existing callback";

    g_test_message( "Testing when callback is previously not set" );
    g_assert( fixture->book->dirty_cb == NULL );
    qof_book_set_dirty_cb( fixture->book, mock_dirty_cb, (gpointer) (&test_struct) );
    g_assert( fixture->book->dirty_cb == mock_dirty_cb );
    g_assert( fixture->book->dirty_data == &test_struct );

    /* need this as long as we have fatal warnings enabled */
    g_test_log_set_fatal_handler ( ( GTestLogFatalFunc )handle_faults, NULL );

    g_test_message( "Testing when callback was previously set" );
    g_assert( fixture->book->dirty_cb != NULL );
    qof_book_set_dirty_cb( fixture->book, NULL, NULL );
    g_assert( g_strrstr( test_struct.msg, error_msg ) != NULL );
    g_assert( fixture->book->dirty_cb == NULL );
    g_assert( fixture->book->dirty_data == NULL );
    g_free( test_struct.msg );
}

static void
test_book_shutting_down( Fixture *fixture, gconstpointer pData )
{
    g_test_message( "Testing when book is null" );
    g_assert( qof_book_shutting_down( NULL ) == FALSE );
    g_test_message( "Testing when shutting down is true" );
    fixture->book->shutting_down = TRUE;
    g_assert( qof_book_shutting_down( fixture->book ) == TRUE );
    g_test_message( "Testing when shutting down is false" );
    fixture->book->shutting_down = FALSE;
    g_assert( qof_book_shutting_down( fixture->book ) == FALSE );
}

static void
test_book_set_get_data( Fixture *fixture, gconstpointer pData )
{
    const char *key = "key";
    const char *data = "data";

    g_assert( fixture->book->data_tables != NULL );
    g_test_message( "Testing when book is null" );
    qof_book_set_data( NULL, key, (gpointer) data );
    g_assert( qof_book_get_data( NULL, key ) == NULL );

    g_test_message( "Testing when key is null" );
    qof_book_set_data( fixture->book, NULL, (gpointer) data );
    g_assert( qof_book_get_data( fixture->book, NULL) == NULL );

    g_test_message( "Testing with book key not null, data null" );
    qof_book_set_data( fixture->book, key, NULL );
    g_assert( qof_book_get_data( fixture->book, key ) == NULL );

    g_test_message( "Testing with book key data not null" );
    qof_book_set_data( fixture->book, key, (gpointer) data );
    g_assert_cmpstr( (const char *)qof_book_get_data( fixture->book, key ), == , data );
}

static void
test_book_get_collection( Fixture *fixture, gconstpointer pData )
{
    QofIdType my_type = "my type";
    QofCollection *m_col, *m_col2;

    g_test_message( "Testing when book is null" );
    g_assert( qof_book_get_collection( NULL, my_type ) == NULL );

    g_test_message( "Testing when entity type is null" );
    g_assert( qof_book_get_collection( fixture->book, NULL ) == NULL );

    g_test_message( "Testing when collection does not exist" );
    g_assert( fixture->book->hash_of_collections != NULL );
    g_assert( g_hash_table_lookup ( fixture->book->hash_of_collections, my_type ) == NULL );
    m_col = qof_book_get_collection( fixture->book, my_type );
    g_assert( m_col != NULL );

    g_test_message( "Testing with existing collection" );
    g_assert( g_hash_table_lookup ( fixture->book->hash_of_collections, my_type ) != NULL );
    m_col2 = qof_book_get_collection( fixture->book, my_type );
    g_assert( m_col2 != NULL );
    g_assert( m_col == m_col2 );
}

static void
test_book_foreach_collection( Fixture *fixture, gconstpointer pData )
{
    G_GNUC_UNUSED QofCollection *m_col, *m_col2;
    QofIdType my_type = "my_type", my_type2 = "my_type2";
    guint param = (guint) g_test_rand_int();
    /* GLib assertion messages which aren't filtered to make clang's output like gcc's */
#if defined(__clang__)
#define _func "void qof_book_foreach_collection(const QofBook *, QofCollectionForeachCB, gpointer)"
#else
#define _func "void qof_book_foreach_collection(const QofBook*, QofCollectionForeachCB, gpointer)"
//#define _func "qof_book_foreach_collection"
#endif
    gchar *msg1 = _func ": assertion " _Q "book' failed";
    gchar *msg2 = _func ": assertion " _Q "cb' failed";
#undef _func
    gchar *log_domain = "qof";
    guint loglevel = G_LOG_LEVEL_CRITICAL | G_LOG_FLAG_FATAL, hdlr;
    TestErrorStruct check1 = { loglevel, log_domain, msg1 };
    TestErrorStruct check2 = { loglevel, log_domain, msg2 };

    /* need this as long as we have fatal warnings enabled */
    g_test_log_set_fatal_handler ( ( GTestLogFatalFunc )handle_faults, NULL );
    test_add_error (&check1);
    test_add_error (&check2);
    hdlr = g_log_set_handler (log_domain, loglevel,
                              (GLogFunc)test_list_handler, NULL);

    g_test_message( "Testing when book is null" );
    m_col = qof_book_get_collection( fixture->book, my_type );
    m_col2 = qof_book_get_collection( fixture->book, my_type2 );
    col_struct.col1_called = FALSE;
    col_struct.col2_called = FALSE;
    col_struct.data = (gpointer) (&param);
    /* launch foreach make sure callback was not called and check warning msg */
    qof_book_foreach_collection( NULL, mock_foreach_collection, (gpointer)(&param) );
    g_assert( !col_struct.col1_called );
    g_assert( !col_struct.col2_called );
    g_assert_cmpstr( test_struct.msg, == , msg1);
    g_free( test_struct.msg );

    g_test_message( "Testing when cb is null" );
    /* launching with empty cb should still fail and produce warning */
    qof_book_foreach_collection( fixture->book, NULL, (gpointer)(&param) );
    g_assert( !col_struct.col1_called );
    g_assert( !col_struct.col2_called );
    g_assert_cmpstr( test_struct.msg, == , msg2);
    g_free( test_struct.msg );
    g_log_remove_handler (log_domain, hdlr);
    test_clear_error_list ();

    g_test_message( "Testing when book and cb not null, user_data provided" );
    /* both cols have to be called */
    qof_book_foreach_collection( fixture->book, mock_foreach_collection, (gpointer)(&param) );
    g_assert( col_struct.col1_called );
    g_assert( col_struct.col2_called );
}

static void
test_book_set_data_fin( void )
{
    QofBook *book;
    const char *key = "key";
    const char *data = "data";

    /* init */
    book = qof_book_new();
    g_assert_cmpint( g_hash_table_size( book->data_tables ), == , 0 );
    g_assert_cmpint( g_hash_table_size( book->data_table_finalizers ), == , 0 );

    g_test_message( "Testing when book is null" );
    qof_book_set_data_fin( NULL, key, (gpointer) data, mock_final_cb );
    /* assert nothing was set */
    g_assert_cmpint( g_hash_table_size( book->data_tables ), == , 0 );
    g_assert_cmpint( g_hash_table_size( book->data_table_finalizers ), == , 0 );

    g_test_message( "Testing when key is null" );
    qof_book_set_data_fin( book, NULL, (gpointer) data, mock_final_cb );
    /* nothing set as well */
    g_assert_cmpint( g_hash_table_size( book->data_tables ), == , 0 );
    g_assert_cmpint( g_hash_table_size( book->data_table_finalizers ), == , 0 );

    g_test_message( "Testing with book key not null, cb null" );
    qof_book_set_data_fin( book, key, (gpointer) data, NULL );
    /* now data is set cb not set */
    g_assert_cmpint( g_hash_table_size( book->data_tables ), == , 1 );
    g_assert_cmpint( g_hash_table_size( book->data_table_finalizers ), == , 0 );
    g_assert_cmpstr( (const char *)qof_book_get_data( book, key ), == , data );

    g_test_message( "Testing with all data set" );
    qof_book_set_data_fin( book, key, (gpointer) data, mock_final_cb );
    /* now we have all set */
    g_assert_cmpint( g_hash_table_size( book->data_tables ), == , 1 );
    g_assert_cmpint( g_hash_table_size( book->data_table_finalizers ), == , 1 );
    g_assert_cmpstr( (const char *)qof_book_get_data( book, key ), == , data );
    g_assert( g_hash_table_lookup ( book->data_table_finalizers, (gpointer)key ) == mock_final_cb );

    /* get rid of book make sure final cb is called */
    test_struct.called = FALSE;
    qof_book_destroy( book );
    g_assert( test_struct.called );
}

static void
test_book_mark_closed( Fixture *fixture, gconstpointer pData )
{
    g_test_message( "Testing when book is null" );
    g_assert_cmpstr( &fixture->book->book_open, == , "y" );
    qof_book_mark_closed( NULL );
    g_assert_cmpstr( &fixture->book->book_open, == , "y" );

    g_test_message( "Testing when book is not null" );
    qof_book_mark_closed( fixture->book );
    g_assert_cmpstr( &fixture->book->book_open, == , "n" );
}

static void
test_book_new_destroy( void )
{
    QofBook *book;
    const char *key = "key";
    const char *data = "data";

    g_test_message( "Testing book creation and initial setup" );
    book = qof_book_new();
    g_assert( book );
    g_assert( QOF_IS_BOOK( book ) );

    g_test_message( "Testing book initial setup" );
    g_assert( book->hash_of_collections );
    g_assert( book->data_tables );
    g_assert( book->data_table_finalizers );
    g_assert_cmpint( g_hash_table_size( book->hash_of_collections ), == , 1 );
    g_assert( g_hash_table_lookup ( book->hash_of_collections, QOF_ID_BOOK ) != NULL );
    g_assert_cmpint( g_hash_table_size( book->data_tables ), == , 0 );
    g_assert_cmpint( g_hash_table_size( book->data_table_finalizers ), == , 0 );
    g_assert_cmpstr( &book->book_open, == , "y");
    g_assert( !book->read_only );
    g_assert_cmpint( book->version, == , 0 );

    /* set finalizer */
    qof_book_set_data_fin( book, key, (gpointer) data, mock_final_cb );
    test_struct.called = FALSE;

    g_test_message( "Testing book destroy" );
    qof_book_destroy( book );
    g_assert( qof_book_shutting_down( book ) );
    g_assert( test_struct.called );
}

void
test_suite_qofbook ( void )
{
    GNC_TEST_ADD( suitename, "readonly", Fixture, NULL, setup, test_book_readonly, teardown );
    GNC_TEST_ADD_FUNC( suitename, "validate counter", test_book_validate_counter );
    GNC_TEST_ADD( suitename, "get string option", Fixture, NULL, setup, test_book_get_string_option, teardown );
    GNC_TEST_ADD( suitename, "set string option", Fixture, NULL, setup, test_book_set_string_option, teardown );
    GNC_TEST_ADD( suitename, "session not saved", Fixture, NULL, setup, test_book_session_not_saved, teardown );
    GNC_TEST_ADD( suitename, "session mark saved", Fixture, NULL, setup, test_book_mark_session_saved, teardown );
    GNC_TEST_ADD( suitename, "get counter", Fixture, NULL, setup, test_book_get_counter, teardown );
    GNC_TEST_ADD( suitename, "get counter format", Fixture, NULL, setup, test_book_get_counter_format, teardown );
    GNC_TEST_ADD( suitename, "increment and format counter", Fixture, NULL, setup, test_book_increment_and_format_counter, teardown );
    GNC_TEST_ADD( suitename, "use trading accounts", Fixture, NULL, setup, test_book_use_trading_accounts, teardown );
    GNC_TEST_ADD( suitename, "use trading accounts - currency accounting", Fixture, NULL, setup, test_book_use_trading_accounts_currency_accounting, teardown );
    GNC_TEST_ADD( suitename, "use book-currency", Fixture, NULL, setup, test_book_use_book_currency, teardown );
    GNC_TEST_ADD( suitename, "get autofreeze days", Fixture, NULL, setup, test_book_get_num_days_autofreeze, teardown );
    GNC_TEST_ADD( suitename, "use split action for num field", Fixture, NULL, setup, test_book_use_split_action_for_num_field, teardown );
    GNC_TEST_ADD( suitename, "mark session dirty", Fixture, NULL, setup, test_book_mark_session_dirty, teardown );
    GNC_TEST_ADD( suitename, "session dirty time", Fixture, NULL, setup, test_book_get_session_dirty_time, teardown );
    GNC_TEST_ADD( suitename, "set dirty callback", Fixture, NULL, setup, test_book_set_dirty_cb, teardown );
    GNC_TEST_ADD( suitename, "shutting down", Fixture, NULL, setup, test_book_shutting_down, teardown );
    GNC_TEST_ADD( suitename, "set get data", Fixture, NULL, setup, test_book_set_get_data, teardown );
    GNC_TEST_ADD( suitename, "get collection", Fixture, NULL, setup, test_book_get_collection, teardown );
    GNC_TEST_ADD( suitename, "foreach collection", Fixture, NULL, setup, test_book_foreach_collection, teardown );
    GNC_TEST_ADD_FUNC( suitename, "set data finalizers", test_book_set_data_fin );
    GNC_TEST_ADD( suitename, "mark closed", Fixture, NULL, setup, test_book_mark_closed, teardown );
    GNC_TEST_ADD_FUNC( suitename, "book new and destroy", test_book_new_destroy );
}
