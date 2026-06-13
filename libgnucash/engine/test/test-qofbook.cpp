/********************************************************************
 * test_qofbook.c: google test test suite for qofbook.		    *
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
#include <gtest/gtest.h>
#include <config.h>
#include <string.h>
#include <glib.h>
#include <inttypes.h>
#include <unittest-support.h>

#include "../qof.h"
#include "../gnc-features.h"
#include "../qofbook-p.hpp"
#include "../qofbookslots.h"
/* For gnc_account_create_root() */
#include "../Account.h"

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

class QOFBookFixture: public ::testing::Test
{
public:
    QOFBookFixture();
    ~QOFBookFixture();
protected:
    QofBook* m_book;
};

QOFBookFixture::QOFBookFixture()
{
    m_book = qof_book_new();
}

QOFBookFixture::~QOFBookFixture()
{
    Account *root = gnc_book_get_root_account (m_book);
    xaccAccountBeginEdit (root);
    xaccAccountDestroy (root);
    qof_book_destroy( m_book );
}

/* mock dirty callback function */
static void
mock_dirty_cb (QofBook *book, gboolean dirty, gpointer user_data)
{
    test_struct.called = TRUE;
    printf("# Checking if book is valid\n");
    EXPECT_TRUE( book );
    EXPECT_TRUE( QOF_IS_BOOK( book ) );
    printf("# Checking parameters\n");
    EXPECT_TRUE( dirty );
    EXPECT_EQ( user_data, test_struct.data );
}

/* mock callback for qof_book_foreach_collection testing */
static void
mock_foreach_collection (QofCollection *col, gpointer user_data)
{
    printf("# Checking if collection and data passed correctly\n");
    EXPECT_TRUE( col );
    EXPECT_EQ( user_data, col_struct.data );
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
    EXPECT_TRUE( book );
    EXPECT_TRUE( QOF_IS_BOOK( book ) );
    printf("# Checking parameters\n");
    EXPECT_STREQ( (gchar*)key, "key" );
    EXPECT_STREQ( (gchar*)user_data, "data" );
}

TEST_F(QOFBookFixture, readonly)
{
    EXPECT_NE( m_book, nullptr );
    EXPECT_TRUE( !qof_book_is_readonly( m_book ) );
    qof_book_mark_readonly( m_book );
    EXPECT_TRUE( qof_book_is_readonly( m_book ) );
}

TEST(QOFBook, normalize_counter)
{
    gchar *r, *err_msg = nullptr;
    printf("# Bug Reference: 644036\n");
    printf("# Bug Reference: 728722\n");

    /* Test for detection of missing format conversion */
    r = qof_book_normalize_counter_format("This string is missing the conversion specifier", &err_msg);
    EXPECT_TRUE(!r);
    EXPECT_TRUE(err_msg);
    g_free(err_msg);
    err_msg = nullptr;

    /* Test the usual Linux/Unix G_GINT64_FORMAT */
    r = qof_book_normalize_counter_format("Test - %li", &err_msg);
    EXPECT_STREQ( r, "Test - %" PRIi64);
    EXPECT_EQ(err_msg, nullptr);
    g_free(r);

    /* Test the Windows G_GINT64_FORMAT */
    r = qof_book_normalize_counter_format("Test - %I64i", &err_msg);
    EXPECT_STREQ( r, "Test - %" PRIi64);
    EXPECT_EQ(err_msg, nullptr);
    g_free(r);

    /* Test the system's G_INT64_FORMAT */
    r = qof_book_normalize_counter_format("Test - %" G_GINT64_FORMAT, &err_msg);
    EXPECT_STREQ( r, "Test - %" PRIi64);
    EXPECT_EQ(err_msg, nullptr);
    g_free(r);

    /* Test the posix' PRIi64 */
    r = qof_book_normalize_counter_format("Test - %" PRIi64, &err_msg);
    EXPECT_STREQ( r, "Test - %" PRIi64);
    EXPECT_EQ(err_msg, nullptr);
    g_free(r);

    /* Test the posix' PRIi64 with precision field */
    r = qof_book_normalize_counter_format("Test - %.3" PRIi64, &err_msg);
    EXPECT_STREQ( r, "Test - %.3" PRIi64);
    EXPECT_EQ(err_msg, nullptr);
    g_free(r);

    /* Test the posix' PRIi64 with width field */
    r = qof_book_normalize_counter_format("Test - %5" PRIi64, &err_msg);
    EXPECT_STREQ( r, "Test - %5" PRIi64);
    EXPECT_EQ(err_msg, nullptr);
    g_free(r);

    /* Test the posix' PRIi64 with width and precision field */
    r = qof_book_normalize_counter_format("Test - %5.4" PRIi64, &err_msg);
    EXPECT_STREQ( r, "Test - %5.4" PRIi64);
    EXPECT_EQ(err_msg, nullptr);
    g_free(r);

    /* Test the usual Linux/Unix G_GINT64_FORMAT */
    r = qof_book_normalize_counter_format_internal("Test - %li", "li", &err_msg);
    EXPECT_STREQ( r, "Test - %" PRIi64);
    EXPECT_EQ(err_msg, nullptr);
    g_free(r);

    /* Test the Windows G_GINT64_FORMAT */
    r = qof_book_normalize_counter_format_internal("Test - %I64i", "I64i", &err_msg);
    EXPECT_STREQ( r, "Test - %" PRIi64);
    EXPECT_EQ(err_msg, nullptr);
    g_free(r);

    /* Test an erroneous Windows G_GINT64_FORMAT */
    r = qof_book_normalize_counter_format_internal("Test - %li", "I64i", &err_msg);
    EXPECT_TRUE(!r);
    EXPECT_TRUE(err_msg);
    g_free(err_msg);
    err_msg = nullptr;

    /* Test an erroneous Linux G_GINT64_FORMAT */
    r = qof_book_normalize_counter_format_internal("Test - %I64i", "li", &err_msg);
    EXPECT_TRUE(!r);
    EXPECT_TRUE(err_msg);
    g_free(err_msg);
}

TEST_F(QOFBookFixture, get_string_option)
{
    const char *opt_name = "Option Name";
    const char *opt_value = "Option Value";
    const char *opt_name_notset = "Not Set";
    EXPECT_NE( m_book, nullptr );
    qof_book_set_string_option( m_book, opt_name, opt_value);
    EXPECT_STREQ( qof_book_get_string_option( m_book, opt_name ), opt_value);
    EXPECT_STREQ( qof_book_get_string_option( m_book, opt_name_notset ), nullptr );
}

TEST_F(QOFBookFixture, set_string_option)
{
    const char *opt_name = "Option Name";
    const char *opt_value = "Option Value";
    EXPECT_NE( m_book, nullptr );
    qof_book_set_string_option( m_book, opt_name, opt_value);
    EXPECT_TRUE( qof_instance_is_dirty (QOF_INSTANCE (m_book)) );
}

TEST_F(QOFBookFixture, session_not_saved)
{
    EXPECT_NE( m_book, nullptr );
    EXPECT_TRUE( !qof_book_session_not_saved( m_book ) );
    qof_book_mark_session_saved( m_book );
    EXPECT_TRUE( !qof_book_session_not_saved( m_book ) );
    gnc_account_create_root (m_book);
    qof_book_mark_session_dirty( m_book );
    EXPECT_TRUE( qof_book_session_not_saved( m_book ) );
}

TEST_F(QOFBookFixture, mark_session_saved)
{
    time64 dirty_time, clean_time;

    gnc_account_create_root (m_book);
    qof_book_mark_session_dirty( m_book );
    EXPECT_TRUE( qof_book_session_not_saved( m_book ) );
    dirty_time = qof_book_get_session_dirty_time( m_book );
    qof_book_mark_session_saved( m_book );
    clean_time = qof_book_get_session_dirty_time( m_book );
    EXPECT_TRUE( !qof_book_session_not_saved( m_book ) );
    EXPECT_NE( dirty_time, clean_time );
    EXPECT_EQ( clean_time, 0);
}

TEST_F(QOFBookFixture, get_counter)
{
    const char *counter_name = "Counter name";

    /* need this as long as we have fatal warnings enabled */
    CLogHandlerScoped check("qof.engine", G_LOG_LEVEL_WARNING, nullptr);

    check.SetMsg("[qof_book_get_counter()] No book!!!");
    auto counter = qof_book_get_counter( nullptr, counter_name );
    EXPECT_EQ( counter, -1 );
    EXPECT_EQ(check.Hits(), 1u);

    check.SetMsg("[qof_book_get_counter()] Invalid counter name.");
    counter = qof_book_get_counter( m_book, nullptr );
    EXPECT_EQ( counter, -1 );
    EXPECT_EQ(check.Hits(), 2u);
    counter = qof_book_get_counter( m_book, nullptr );
    EXPECT_EQ( counter, -1 );
    EXPECT_EQ(check.Hits(), 3u);

    counter = qof_book_get_counter( m_book, counter_name );
    EXPECT_EQ( counter, 0 );

    auto r = qof_book_increment_and_format_counter( m_book, counter_name );
    counter = qof_book_get_counter( m_book, counter_name );
    EXPECT_EQ( counter, 1 );
    g_free (r);
}

TEST_F(QOFBookFixture, get_counter_format)
{
    const char *counter_name = "Counter name";

    /* need this as long as we have fatal warnings enabled */
    CLogHandlerScoped check("qof.engine", G_LOG_LEVEL_WARNING, nullptr);

    printf("# Testing counter format when book is null\n");
    check.SetMsg("[qof_book_get_counter_format()] No book!!!");
    auto r = qof_book_get_counter_format( nullptr, counter_name );
    EXPECT_STREQ( r, nullptr );
    EXPECT_EQ(check.Hits(), 1u);

    printf("# Testing counter format when counter name is null\n");
    check.SetMsg("[qof_book_get_counter_format()] Invalid counter name.");
    r = qof_book_get_counter_format( m_book, nullptr );
    EXPECT_STREQ( r, nullptr );
    EXPECT_EQ(check.Hits(), 2u);
    printf("# Testing counter format when counter name is empty string\n");
    r = qof_book_get_counter_format( m_book, "" );
    EXPECT_STREQ( r, nullptr );
    EXPECT_EQ(check.Hits(), 3u);

    printf("# Testing counter format with existing counter\n");
    r = qof_book_get_counter_format( m_book, counter_name );
    EXPECT_STREQ( r, "%.6" PRIi64);
    g_free (r);

    printf("# Testing counter format for default value\n");
    r = qof_book_get_counter_format( m_book, counter_name );
    EXPECT_STREQ( r, "%.6" PRIi64);
    g_free (r);
}

TEST_F(QOFBookFixture, increment_and_format_counter)
{
    const char *counter_name = "Counter name";

    /* need this as long as we have fatal warnings enabled */
    CLogHandlerScoped check("qof.engine", G_LOG_LEVEL_WARNING, nullptr);

    printf("# Testing increment and format when book is null\n");
    check.SetMsg("[qof_book_increment_and_format_counter()] No book!!!");
    auto r = qof_book_increment_and_format_counter( nullptr, counter_name );
    EXPECT_STREQ( r, nullptr );
    g_free( r );
    EXPECT_EQ(check.Hits(), 1u);

    printf("# Testing increment and format when counter name is null\n");
    check.SetMsg("[qof_book_increment_and_format_counter()] Invalid counter name.");
    r = qof_book_increment_and_format_counter( m_book, nullptr );
    EXPECT_STREQ( r, nullptr );
    g_free( r );
    EXPECT_EQ(check.Hits(), 2u);
    printf("# Testing increment and format when counter name is empty string\n");
    r = qof_book_increment_and_format_counter( m_book, "" );
    EXPECT_STREQ( r, nullptr );
    g_free( r );
    EXPECT_EQ(check.Hits(), 3u);

    printf("# Testing increment and format with new counter\n");
    r = qof_book_increment_and_format_counter( m_book, counter_name );
    auto counter = qof_book_get_counter( m_book, counter_name );
    auto format = qof_book_get_counter_format( m_book, counter_name );
    auto format_str = g_strdup_printf (format, counter);
    EXPECT_EQ( counter, 1 );
    EXPECT_TRUE( qof_instance_is_dirty (QOF_INSTANCE (m_book)) );
    EXPECT_STREQ( r, format_str);
    g_free( r );
    g_free (format);
    g_free (format_str);

    printf("# Testing increment and format with existing counter\n");
    r = qof_book_increment_and_format_counter( m_book, counter_name );
    counter = qof_book_get_counter( m_book, counter_name );
    format = qof_book_get_counter_format( m_book, counter_name );
    format_str = g_strdup_printf (format, counter);
    EXPECT_EQ( counter, 2 );
    EXPECT_STREQ( r, format_str);
    g_free( r );
    g_free (format);
    g_free (format_str);
}

TEST_F(QOFBookFixture, get_default_report_guid)
{
    /* need this as long as we have fatal warnings enabled */
    CLogHandlerScoped check("qof.engine", G_LOG_LEVEL_WARNING, nullptr);

    printf("# Testing default report guid when book is null\n");
    check.SetMsg("[qof_book_get_default_invoice_report_guid()] No book!!!");
    auto r = qof_book_get_default_invoice_report_guid ( nullptr );
    EXPECT_STREQ( r, nullptr );
    EXPECT_EQ(check.Hits(), 1u);

    printf("# Testing default report guid for default value\n");
    r = qof_book_get_default_invoice_report_guid ( m_book );
    EXPECT_STREQ( r, nullptr );
}

TEST_F(QOFBookFixture, get_default_report_name)
{
    /* need this as long as we have fatal warnings enabled */
    CLogHandlerScoped check("qof.engine", G_LOG_LEVEL_WARNING, nullptr);

    printf("# Testing default report name when book is null\n");
    check.SetMsg("[qof_book_get_default_invoice_report_name()] No book!!!");
    auto r = qof_book_get_default_invoice_report_name ( nullptr );
    EXPECT_STREQ( r, nullptr );
    EXPECT_EQ(check.Hits(), 1u);

    printf("# Testing default report name for default value\n");
    r = qof_book_get_default_invoice_report_name ( m_book );
    EXPECT_STREQ( r, nullptr );
}

TEST_F(QOFBookFixture, get_default_report_timeout)
{
    /* need this as long as we have fatal warnings enabled */
    CLogHandlerScoped check("qof.engine", G_LOG_LEVEL_WARNING, nullptr);

    printf("# Testing default report timeout when book is null\n");
    check.SetMsg("[qof_book_get_default_invoice_report_timeout()] No book!!!");
    auto r = qof_book_get_default_invoice_report_timeout ( nullptr );
    EXPECT_EQ( r, 0 );
    EXPECT_EQ(check.Hits(), 1u);

    printf("# Testing default report timeout for default value\n");
    r = qof_book_get_default_invoice_report_timeout ( m_book );
    EXPECT_EQ( r, 0 );
}

TEST_F(QOFBookFixture, set_default_report)
{
    const char *test_guid1 = "5123a759ceb9483abf2182d01c140eff";
    const char *test_guid2 = "5123a759ceb9483abf2182d01c140eee";
    const char *test_name = "My Invoice Report";

    /* need this as long as we have fatal warnings enabled */
    CLogHandlerScoped check("qof.engine", G_LOG_LEVEL_WARNING, nullptr);

    printf("# Testing setting default report when book is null\n");
    check.SetMsg("[qof_book_set_default_invoice_report()] No book!!!");
    qof_book_set_default_invoice_report ( nullptr, test_guid1, test_name );
    auto r = qof_book_get_default_invoice_report_guid ( m_book );
    EXPECT_STREQ( r, nullptr );
    EXPECT_EQ(check.Hits(), 1u);

    printf("# Testing setting default report when guid is null\n");
    check.SetMsg("[qof_book_set_default_invoice_report()] No guid!!!");
    qof_book_set_default_invoice_report ( m_book, nullptr, test_name );
    r = qof_book_get_default_invoice_report_guid ( m_book );
    EXPECT_STREQ( r, nullptr );
    EXPECT_EQ(check.Hits(), 2u);

    printf("# Testing setting default report when name is null\n");
    check.SetMsg("[qof_book_set_default_invoice_report()] No name!!!");
    qof_book_set_default_invoice_report ( m_book, test_guid1, nullptr );
    r = qof_book_get_default_invoice_report_guid ( m_book );
    EXPECT_STREQ( r, nullptr );
    EXPECT_EQ(check.Hits(), 3u);

    printf("# Testing setting default report when name is empty string\n");
    qof_book_set_default_invoice_report ( m_book, test_guid1, "" );
    r = qof_book_get_default_invoice_report_guid ( m_book );
    EXPECT_STREQ( r, test_guid1 );
    g_free (r);
    r = qof_book_get_default_invoice_report_name ( m_book );
    EXPECT_STREQ( r, "" );
    g_free (r);

    printf("# Testing setting default report with guid and name\n");
    qof_book_set_default_invoice_report ( m_book, test_guid2, test_name );
    r = qof_book_get_default_invoice_report_guid ( m_book );
    EXPECT_STREQ( r, test_guid2 );
    g_free (r);
    r = qof_book_get_default_invoice_report_name ( m_book );
    EXPECT_STREQ( r, test_name );
    g_free (r);
}

TEST_F(QOFBookFixture, use_trading_accounts)
{
    EXPECT_EQ( qof_book_use_trading_accounts( m_book ), FALSE );

    printf("# Testing with existing trading accounts set to true - t\n");
    qof_book_begin_edit (m_book);
    qof_instance_set (QOF_INSTANCE (m_book),
		      "trading-accts", "t",
		      nullptr);
    EXPECT_EQ( qof_book_use_trading_accounts( m_book ), TRUE );

    printf("# Testing with existing trading accounts and incorrect value - tt\n");
    qof_instance_set (QOF_INSTANCE (m_book),
		      "trading-accts", "tt",
		      nullptr);
    EXPECT_EQ( qof_book_use_trading_accounts( m_book ), FALSE );
    qof_book_commit_edit (m_book);

}

TEST_F(QOFBookFixture, get_num_days_autofreeze)
{
    printf("# Testing default: No auto-freeze days are set\n");
    EXPECT_EQ( qof_book_uses_autoreadonly( m_book ), FALSE );
    EXPECT_EQ( qof_book_get_num_days_autoreadonly( m_book ), 0 );

    EXPECT_EQ( qof_book_uses_autoreadonly( m_book ), FALSE );
    EXPECT_EQ( qof_book_get_num_days_autoreadonly( m_book ), 0 );

    qof_book_begin_edit (m_book);
    qof_instance_set (QOF_INSTANCE (m_book),
		      "autoreadonly-days", (gdouble)17,
		      nullptr);
    EXPECT_EQ( qof_book_uses_autoreadonly( m_book ), TRUE );
    EXPECT_EQ( qof_book_get_num_days_autoreadonly( m_book ), 17 );

    printf("# Testing when setting this correctly to zero again\n");

    qof_instance_set (QOF_INSTANCE (m_book),
		      "autoreadonly-days", (gdouble)0,
		      nullptr);
    EXPECT_EQ( qof_book_uses_autoreadonly( m_book ), FALSE );
    EXPECT_EQ( qof_book_get_num_days_autoreadonly( m_book ), 0 );

    qof_instance_set (QOF_INSTANCE (m_book),
		      "autoreadonly-days", (gdouble)32,
		      nullptr);
    EXPECT_EQ( qof_book_uses_autoreadonly( m_book ), TRUE );
    EXPECT_EQ( qof_book_get_num_days_autoreadonly( m_book ), 32 );

    qof_book_commit_edit (m_book);
}

TEST_F(QOFBookFixture, use_split_action_for_num_field)
{
    printf("# Testing default: No selection has been specified\n");
    EXPECT_EQ( qof_book_use_split_action_for_num_field( m_book ), FALSE );

    printf("# Testing with existing use split action for num set to true - t\n");

    qof_book_begin_edit (m_book);
    qof_instance_set (QOF_INSTANCE (m_book),
		      "split-action-num-field", "t",
		      nullptr);
    EXPECT_EQ( qof_book_use_split_action_for_num_field( m_book ), TRUE );

    printf("# Testing with existing use split action for num and incorrect value - tt\n");
    qof_instance_set (QOF_INSTANCE (m_book),
		      "split-action-num-field", "tt",
		      nullptr);
    EXPECT_EQ( qof_book_use_split_action_for_num_field( m_book ), FALSE );
    qof_book_commit_edit (m_book);
}

TEST_F(QOFBookFixture, mark_session_dirty)
{
    QofBook *_empty = nullptr;
    time64 before, after;
    guint param = (guint) 48623u;

    printf("# Testing when book is nullptr\n");
    qof_book_mark_session_dirty( _empty );
    EXPECT_EQ( _empty, nullptr );

    printf("# Testing when book is not dirty and dirty_cb is null\n");
    EXPECT_EQ( qof_book_get_session_dirty_time( m_book ), 0);
    EXPECT_EQ( m_book->dirty_cb, nullptr );
    EXPECT_EQ( qof_book_session_not_saved( m_book ), FALSE );
    before = gnc_time (nullptr);
    gnc_account_create_root (m_book);
    qof_book_mark_session_dirty( m_book );
    after = gnc_time (nullptr);
    EXPECT_GE( qof_book_get_session_dirty_time( m_book ), before);
    EXPECT_LE( qof_book_get_session_dirty_time( m_book ), after);
    EXPECT_EQ( qof_book_session_not_saved( m_book ), TRUE );

    printf("# Testing when book is not dirty and dirty_cb is not null\n");
    /* prepare conditions */
    qof_book_mark_session_saved( m_book );
    qof_book_set_dirty_cb( m_book, mock_dirty_cb, (gpointer) (&param) );
    test_struct.data = (gpointer) (&param);
    test_struct.called = FALSE;
    EXPECT_NE( m_book->dirty_cb, nullptr );
    EXPECT_EQ( qof_book_get_session_dirty_time( m_book ), 0);
    EXPECT_EQ( qof_book_session_not_saved( m_book ), FALSE );
    /* run FUT */
    before = gnc_time (nullptr);
    qof_book_mark_session_dirty( m_book );
    after = gnc_time (nullptr);
    /* test output */
    EXPECT_GE( qof_book_get_session_dirty_time( m_book ), before);
    EXPECT_LE( qof_book_get_session_dirty_time( m_book ), after);
    EXPECT_EQ( qof_book_session_not_saved( m_book ), TRUE );
    EXPECT_TRUE( test_struct.called );

    printf("# Testing when book is dirty\n");
    EXPECT_EQ( qof_book_session_not_saved( m_book ), TRUE );
    before = qof_book_get_session_dirty_time( m_book );
    qof_book_mark_session_dirty( m_book );
    EXPECT_EQ( qof_book_session_not_saved( m_book ), TRUE );
    after = qof_book_get_session_dirty_time( m_book );
    EXPECT_EQ( before, after );
}

TEST_F(QOFBookFixture, get_session_dirty_time)
{
    time64 before, after;

    printf("# Testing time on saved book = 0\n");
    EXPECT_EQ( qof_book_session_not_saved( m_book ), FALSE );
    EXPECT_EQ( qof_book_get_session_dirty_time( m_book ), 0);

    printf("# Testing time on dirty book is correct\n");
    before = gnc_time (nullptr);
    qof_book_mark_session_dirty( m_book );
    after = gnc_time (nullptr);
    EXPECT_GE( qof_book_get_session_dirty_time( m_book ), before);
    EXPECT_LE( qof_book_get_session_dirty_time( m_book ), after);

}

TEST_F(QOFBookFixture, set_dirty_cb)
{
    printf("# Testing when callback is previously not set\n");
    EXPECT_EQ( m_book->dirty_cb, nullptr );
    qof_book_set_dirty_cb( m_book, mock_dirty_cb, (gpointer) (&test_struct) );
    EXPECT_EQ( m_book->dirty_cb, (gpointer)mock_dirty_cb );
    EXPECT_EQ( m_book->dirty_data, &test_struct );

    /* need this as long as we have fatal warnings enabled */
    CLogHandlerScoped check("qof.engine", G_LOG_LEVEL_WARNING, nullptr);

    printf("# Testing when callback was previously set\n");
    EXPECT_NE( m_book->dirty_cb, nullptr );
    qof_book_set_dirty_cb( m_book, nullptr, nullptr );
    EXPECT_EQ(check.Hits(), 1u);
    EXPECT_EQ( m_book->dirty_cb, nullptr );
    EXPECT_EQ( m_book->dirty_data, nullptr );
}

TEST_F(QOFBookFixture, shutting_down)
{
    printf("# Testing when book is null\n");
    EXPECT_EQ( qof_book_shutting_down( nullptr ), FALSE );
    printf("# Testing when shutting down is true\n");
    m_book->shutting_down = TRUE;
    EXPECT_EQ( qof_book_shutting_down( m_book ), TRUE );
    printf("# Testing when shutting down is false\n");
    m_book->shutting_down = FALSE;
    EXPECT_EQ( qof_book_shutting_down( m_book ), FALSE );
}

TEST_F(QOFBookFixture, set_get_data)
{
    const char *key = "key";
    const char *data = "data";

    EXPECT_NE( m_book->data_tables, nullptr );
    printf("# Testing when book is null\n");
    qof_book_set_data( nullptr, key, (gpointer) data );
    EXPECT_EQ( qof_book_get_data( nullptr, key ), nullptr );

    printf("# Testing when key is null\n");
    qof_book_set_data( m_book, nullptr, (gpointer) data );
    EXPECT_EQ( qof_book_get_data( m_book, nullptr), nullptr );

    printf("# Testing with book key not null, data null\n");
    qof_book_set_data( m_book, key, nullptr );
    EXPECT_EQ( qof_book_get_data( m_book, key ), nullptr );

    printf("# Testing with book key data not null\n");
    qof_book_set_data( m_book, key, (gpointer) data );
    EXPECT_STREQ( (const char *)qof_book_get_data( m_book, key ), data );
}

TEST_F(QOFBookFixture, get_collection)
{
    QofIdType my_type = "my type";
    QofCollection *m_col, *m_col2;

    printf("# Testing when book is null\n");
    EXPECT_EQ( qof_book_get_collection( nullptr, my_type ), nullptr );

    printf("# Testing when entity type is null\n");
    EXPECT_EQ( qof_book_get_collection( m_book, nullptr ), nullptr );

    printf("# Testing when collection does not exist\n");
    EXPECT_NE( m_book->hash_of_collections, nullptr );
    EXPECT_EQ( g_hash_table_lookup ( m_book->hash_of_collections, my_type ), nullptr );
    m_col = qof_book_get_collection( m_book, my_type );
    EXPECT_NE( m_col, nullptr );

    printf("# Testing with existing collection\n");
    EXPECT_NE( g_hash_table_lookup ( m_book->hash_of_collections, my_type ), nullptr );
    m_col2 = qof_book_get_collection( m_book, my_type );
    EXPECT_NE( m_col2, nullptr );
    EXPECT_EQ( m_col, m_col2 );
}


TEST_F(QOFBookFixture, features)
{
    char* msg;
    printf("# Testing book features\n");

    EXPECT_EQ(gnc_features_test_unknown (m_book), nullptr);
    EXPECT_FALSE(gnc_features_check_used (m_book, "Credit Notes"));

    gnc_features_set_used (m_book, "Credit Notes");
    EXPECT_EQ(gnc_features_test_unknown (m_book), nullptr);
    EXPECT_TRUE(gnc_features_check_used (m_book, "Credit Notes"));

    gnc_features_set_unused (m_book, "Credit Notes");
    EXPECT_EQ(gnc_features_test_unknown (m_book), nullptr);
    EXPECT_FALSE(gnc_features_check_used (m_book, "Credit Notes"));

    qof_book_set_feature(m_book, "Use a Book-Currency", "Random string, doesn't matter");
    EXPECT_EQ(gnc_features_test_unknown (m_book), nullptr);
    EXPECT_FALSE(gnc_features_check_used (m_book, "Use a Book-Currency"));


    /* cannot use gnc_features_set_used to set an unknown feature: it bails out.
     * use qof_book_set_feature instead. */
    qof_book_set_feature (m_book, "Nanotech", "With Quantum Computing");
    EXPECT_TRUE(gnc_features_check_used (m_book, "Nanotech"));
    msg = gnc_features_test_unknown (m_book);
    EXPECT_STREQ(msg, "This Dataset contains features not \
supported by this version of GnuCash. You must use a newer version \
of GnuCash in order to support the following features:\n* With Quantum Computing");
    g_free (msg);

    qof_book_unset_feature (m_book, "Nanotech");
    EXPECT_EQ(gnc_features_test_unknown (m_book), nullptr);
}

TEST_F(QOFBookFixture, foreach_collection)
{
    G_GNUC_UNUSED QofCollection *m_col, *m_col2;
    QofIdType my_type = "my_type", my_type2 = "my_type2";
    guint param = (guint) 34215u;
    /* GLib assertion messages which aren't filtered to make clang's output like gcc's */
#if defined(__clang__)
#define _func "void qof_book_foreach_collection(const QofBook *, QofCollectionForeachCB, gpointer)"
#else
#define _func "void qof_book_foreach_collection(const QofBook*, QofCollectionForeachCB, gpointer)"
#endif
    gchar msg1[] = _func ": assertion 'book' failed";
    gchar msg2[] = _func ": assertion 'cb' failed";
#undef _func
    gchar log_domain[] = "gnc.engine";
    auto loglevel = GLogLevelFlags(G_LOG_LEVEL_CRITICAL | G_LOG_FLAG_FATAL);
    guint hdlr;
    TestErrorStruct check1 = { loglevel, log_domain, msg1 };
    TestErrorStruct check2 = { loglevel, log_domain, msg2 };

    /* need this as long as we have fatal warnings enabled */
    test_add_error (&check1);
    test_add_error (&check2);
    hdlr = g_log_set_handler (log_domain, loglevel,
                              (GLogFunc)test_list_handler, nullptr);

    printf("# Testing when book is null\n");
    m_col = qof_book_get_collection( m_book, my_type );
    m_col2 = qof_book_get_collection( m_book, my_type2 );
    col_struct.col1_called = FALSE;
    col_struct.col2_called = FALSE;
    col_struct.data = (gpointer) (&param);
    /* launch foreach make sure callback was not called and check warning msg */
    qof_book_foreach_collection( nullptr, mock_foreach_collection, (gpointer)(&param) );
    EXPECT_TRUE( !col_struct.col1_called );
    EXPECT_TRUE( !col_struct.col2_called );
    EXPECT_EQ(check1.hits, 1u);

    printf("# Testing when cb is null\n");
    /* launching with empty cb should still fail and produce warning */
    qof_book_foreach_collection( m_book, nullptr, (gpointer)(&param) );
    EXPECT_TRUE( !col_struct.col1_called );
    EXPECT_TRUE( !col_struct.col2_called );
    EXPECT_EQ(check2.hits, 1u);
    g_log_remove_handler (log_domain, hdlr);
    test_clear_error_list ();

    printf("# Testing when book and cb not null, user_data provided\n");
    /* both cols have to be called */
    qof_book_foreach_collection( m_book, mock_foreach_collection, (gpointer)(&param) );
    EXPECT_TRUE( col_struct.col1_called );
    EXPECT_TRUE( col_struct.col2_called );
}

TEST(QOFBook, set_data_fin)
{
    QofBook *book;
    const char *key = "key";
    const char *data = "data";

    /* init */
    book = qof_book_new();
    EXPECT_EQ( g_hash_table_size( book->data_tables ), 0u );
    EXPECT_EQ( g_hash_table_size( book->data_table_finalizers ), 0u );

    printf("# Testing when book is null\n");
    qof_book_set_data_fin( nullptr, key, (gpointer) data, mock_final_cb );
    /* assert nothing was set */
    EXPECT_EQ( g_hash_table_size( book->data_tables ), 0u );
    EXPECT_EQ( g_hash_table_size( book->data_table_finalizers ), 0u );

    printf("# Testing when key is null\n");
    qof_book_set_data_fin( book, nullptr, (gpointer) data, mock_final_cb );
    /* nothing set as well */
    EXPECT_EQ( g_hash_table_size( book->data_tables ), 0u );
    EXPECT_EQ( g_hash_table_size( book->data_table_finalizers ), 0u );

    printf("# Testing with book key not null, cb null\n");
    qof_book_set_data_fin( book, key, (gpointer) data, nullptr );
    /* now data is set cb not set */
    EXPECT_EQ( g_hash_table_size( book->data_tables ), 1u );
    EXPECT_EQ( g_hash_table_size( book->data_table_finalizers ), 0u );
    EXPECT_STREQ( (const char *)qof_book_get_data( book, key ), data );

    printf("# Testing with all data set\n");
    qof_book_set_data_fin( book, key, (gpointer) data, mock_final_cb );
    /* now we have all set */
    EXPECT_EQ( g_hash_table_size( book->data_tables ), 1u );
    EXPECT_EQ( g_hash_table_size( book->data_table_finalizers ), 1u );
    EXPECT_STREQ( (const char *)qof_book_get_data( book, key ), data );
    EXPECT_EQ( g_hash_table_lookup ( book->data_table_finalizers, (gpointer)key ), (gpointer)mock_final_cb );

    /* get rid of book make sure final cb is called */
    test_struct.called = FALSE;
    qof_book_destroy( book );
    EXPECT_TRUE( test_struct.called );
}

TEST_F(QOFBookFixture, mark_closed)
{
    printf("# Testing when book is null\n");
    EXPECT_STREQ( &m_book->book_open, "y" );
    qof_book_mark_closed( nullptr );
    EXPECT_STREQ( &m_book->book_open, "y" );

    printf("# Testing when book is not null\n");
    qof_book_mark_closed( m_book );
    EXPECT_STREQ( &m_book->book_open, "n" );
}

TEST(QOFBook, new_destroy)
{
    QofBook *book;
    const char *key = "key";
    const char *data = "data";
    QofBookTestFunctions* test_funcs = _utest_qofbook_fill_functions ();

    printf("# Testing book creation and initial setup\n");
    book = qof_book_new();
    EXPECT_TRUE( book );
    EXPECT_TRUE( QOF_IS_BOOK( book ) );

    printf("# Testing book initial setup\n");
    EXPECT_TRUE( book->hash_of_collections );
    EXPECT_TRUE( book->data_tables );
    EXPECT_TRUE( book->data_table_finalizers );
    EXPECT_EQ( g_hash_table_size( book->hash_of_collections ), 1u );
    EXPECT_NE( g_hash_table_lookup ( book->hash_of_collections, QOF_ID_BOOK ), nullptr );
    EXPECT_EQ( g_hash_table_size( book->data_tables ), 0u );
    EXPECT_EQ( g_hash_table_size( book->data_table_finalizers ), 0u );
    EXPECT_STREQ( &book->book_open, "y");
    EXPECT_TRUE( !book->read_only );
    EXPECT_EQ( book->version, 0 );

    /* set finalizer */
    qof_book_set_data_fin( book, key, (gpointer) data, mock_final_cb );
    test_struct.called = FALSE;

    qof_book_destroy( book );

    g_free (test_funcs);
}

int main(int argc, char **argv) {
    testing::InitGoogleTest(&argc, argv);

    setlocale (LC_ALL, "");
    qof_init(); 			/* Initialize the GObject system */
    qof_log_init_filename_special("stderr"); /* Init the log system */

    return RUN_ALL_TESTS();
}
