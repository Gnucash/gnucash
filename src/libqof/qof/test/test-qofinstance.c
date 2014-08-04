/********************************************************************
 * test_qofinstance.c: GLib g_test test suite for qofinstance.	    *
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
#include <unittest-support.h>
#include "../qof.h"
#include "../qofbackend-p.h"

static const gchar *suitename = "/qof/qofinstance";
void test_suite_qofinstance ( void );
static gchar* error_message;
static gboolean is_called;

#ifdef HAVE_GLIB_2_38
#define _Q "'"
#else
#define _Q "`"
#endif

typedef struct
{
    QofInstance *inst;
} Fixture;

/* use g_free on error_message after this function been called */
static gboolean
fatal_handler ( const char * log_domain,
                GLogLevelFlags log_level,
                const gchar *msg,
                gpointer user_data)
{
    error_message = (gchar *) g_strdup( msg );
    return FALSE;
}

static void
setup( Fixture *fixture, gconstpointer pData )
{
    fixture->inst = g_object_new(QOF_TYPE_INSTANCE, NULL);
}

static void
teardown( Fixture *fixture, gconstpointer pData )
{
    g_object_unref(fixture->inst);
}

static void
test_instance_set_get_book( Fixture *fixture, gconstpointer pData )
{
    QofBook *book;

    /* set up */
    book = qof_book_new();

    g_assert( QOF_IS_INSTANCE( fixture->inst ) );

    g_test_message( "Setting and getting book" );
    qof_instance_set_book( fixture->inst, book );
    g_assert( book == qof_instance_get_book( fixture->inst ) );

    g_test_message( "Getting book when instance is null" );
    g_assert( qof_instance_get_book( NULL ) == NULL );

    /* Clean up */
    qof_book_destroy( book );
}

static void
test_instance_set_get_guid( Fixture *fixture, gconstpointer pData )
{
    GncGUID *gncGuid;

    /* on null instance deprecated getter returns empty guid
     * while instance_get_guid returns null
     */
    g_assert( !qof_instance_get_guid( NULL ) );
    g_assert( qof_entity_get_guid( NULL ) == guid_null() );

    /* set up */
    gncGuid = guid_new();
    g_assert( QOF_IS_INSTANCE( fixture->inst ) );
    g_assert( gncGuid );

    /* guid already exists after instance init */
    g_test_message( "Setting new guid" );
    g_assert( qof_instance_get_guid( fixture->inst ) );
    g_assert( !guid_equal( gncGuid, qof_instance_get_guid( fixture->inst ) ) );
    qof_instance_set_guid( fixture->inst, gncGuid );
    g_assert( guid_equal( gncGuid, qof_instance_get_guid( fixture->inst ) ) );
    g_assert( guid_equal( gncGuid, qof_entity_get_guid( fixture->inst ) ) );

    /* Clean up */
    guid_free( gncGuid );
}

static void
test_instance_new_destroy( void )
{
    /* qofinstance var */
    QofInstance *inst;
    QofInstanceClass *klass;
    /* test var */
    Timespec *timespec_priv;
    gchar *msg1 = "qof_instance_get_collection: assertion " _Q "QOF_IS_INSTANCE(ptr)' failed";
    gchar *msg2 = "qof_instance_get_editlevel: assertion " _Q "QOF_IS_INSTANCE(ptr)' failed";
    gchar *msg3 = "qof_instance_get_destroying: assertion " _Q "QOF_IS_INSTANCE(ptr)' failed";
    gchar *msg4 = "qof_instance_get_dirty_flag: assertion " _Q "QOF_IS_INSTANCE(ptr)' failed";
    gchar *log_domain = "qof";
    guint loglevel = G_LOG_LEVEL_CRITICAL | G_LOG_FLAG_FATAL, hdlr;
    TestErrorStruct check = { loglevel, log_domain, msg1 };

    g_test_message( "Testing qofinstance object initialization" );
    inst = g_object_new(QOF_TYPE_INSTANCE, NULL);
    g_assert( QOF_IS_INSTANCE( inst ) );
    /* test class fields */
    klass = QOF_INSTANCE_GET_CLASS( inst );
    g_assert( QOF_IS_INSTANCE_CLASS( klass ) );
    g_assert( klass->get_display_name == NULL );
    g_assert( klass->refers_to_object == NULL );
    g_assert( klass->get_typed_referring_object_list == NULL );
    /* testing initial values */
    g_assert( qof_instance_get_guid( inst ) );
    g_assert( !qof_instance_get_collection( inst ) );
    g_assert( qof_instance_get_book( inst ) == NULL );
    g_assert( inst->kvp_data );
    g_object_get( inst, "last-update", &timespec_priv, NULL);
    g_assert_cmpint( timespec_priv->tv_sec, == , 0 );
    g_assert_cmpint( timespec_priv->tv_nsec, == , -1 );
    g_assert_cmpint( qof_instance_get_editlevel( inst ), == , 0 );
    g_assert( !qof_instance_get_destroying( inst ) );
    g_assert( !qof_instance_get_dirty_flag( inst ) );
    g_assert( qof_instance_get_infant( inst ) );
    g_assert_cmpint( qof_instance_get_version( inst ), == , 0 );
    g_assert_cmpint( qof_instance_get_version_check( inst ), == , 0 );
    g_assert_cmpint( qof_instance_get_idata( inst ), == , 0 );

    g_test_message( "Testing object destruction" );
    g_object_unref( inst );
    /* test fields were deinitialized */
    g_assert( inst );
    g_assert( !QOF_IS_INSTANCE( inst ) );
    /* set fatal handler */
    g_test_log_set_fatal_handler ( ( GTestLogFatalFunc )fatal_handler, NULL );
    hdlr = g_log_set_handler (log_domain, loglevel,
                              (GLogFunc)test_checked_handler, &check);
    g_assert( qof_instance_get_collection( inst ) == NULL );
    g_assert( g_strrstr( error_message, "assertion " _Q "QOF_IS_INSTANCE(ptr)' failed" ) != NULL );
    g_free( error_message );

    check.msg = msg2;
    g_assert_cmpint( qof_instance_get_editlevel( inst ), == , 0 );
    g_assert( g_strrstr( error_message, "assertion " _Q "QOF_IS_INSTANCE(ptr)' failed" ) != NULL );
    g_free( error_message );

    check.msg = msg3;
    g_assert( !qof_instance_get_destroying( inst ) );
    g_assert( g_strrstr( error_message, "assertion " _Q "QOF_IS_INSTANCE(ptr)' failed" ) != NULL );
    g_free( error_message );

    check.msg = msg4;
    g_assert( !qof_instance_get_dirty_flag( inst ) );
    g_assert( g_strrstr( error_message, "assertion " _Q "QOF_IS_INSTANCE(ptr)' failed" ) != NULL );
    g_free( error_message );
    g_log_remove_handler (log_domain, hdlr);
}

static void
test_instance_init_data( void )
{
    QofInstance *inst;
    QofIdType test_type = "test type";
    QofBook *book;
    QofCollection *col;
    const GncGUID *gncguid;
    char guid_id_before[GUID_ENCODING_LENGTH + 1];
    char guid_id_after[GUID_ENCODING_LENGTH + 1];

    /* set up */
    inst = g_object_new( QOF_TYPE_INSTANCE, NULL );
    g_assert( QOF_IS_INSTANCE( inst ) );
    book = qof_book_new();
    g_assert( QOF_IS_BOOK( book ) );
    /* set fatal handler */
    g_test_log_set_fatal_handler ( ( GTestLogFatalFunc )fatal_handler, NULL );

    g_test_message( "Running test with correct initial data" );
    gncguid = qof_instance_get_guid( inst );
    g_assert( gncguid );
    guid_to_string_buff( gncguid, guid_id_before );
    g_assert( qof_instance_get_book( inst ) == NULL );
    g_assert( qof_instance_get_collection( inst ) == NULL );
    /* run init */
    qof_instance_init_data( inst, test_type, book );

    g_assert( qof_instance_get_book( inst ) == book );
    guid_to_string_buff( gncguid, guid_id_after );
    g_assert_cmpstr( guid_id_before, != , guid_id_after );
    g_assert( qof_instance_get_collection( inst ) != NULL );
    col = qof_book_get_collection( book, test_type );
    g_assert( col );
    g_assert( col == qof_instance_get_collection( inst ) );
    g_assert_cmpstr( inst->e_type, == , test_type );
    g_assert( qof_collection_lookup_entity( qof_instance_get_collection( inst ), gncguid ) == inst );

    /* clean up */
    g_object_unref( inst );
    qof_book_destroy( book );
}

static void
test_instance_get_set_slots( Fixture *fixture, gconstpointer pData )
{
    KvpFrame *kvp_frame, *kvp_frame2;

    /* set up */
    g_assert( fixture->inst );
    kvp_frame = qof_instance_get_slots( fixture->inst );
    g_assert( kvp_frame );

    g_test_message( "Test when kvp frame is the same" );
    qof_instance_set_slots( fixture->inst, kvp_frame );
    g_assert( kvp_frame == qof_instance_get_slots( fixture->inst ) );
    g_assert( qof_instance_get_dirty_flag( fixture->inst ) );

    g_test_message( "Test when kvp frame is not the same" );
    kvp_frame2 = kvp_frame_new();
    g_assert( kvp_frame != kvp_frame2 );
    qof_instance_set_slots( fixture->inst, kvp_frame2 );
    g_assert( kvp_frame2 == qof_instance_get_slots( fixture->inst ) );
    g_assert( qof_instance_get_dirty_flag( fixture->inst ) );

    g_test_message( "Test when kvp frame is null" );
    qof_instance_set_slots( fixture->inst, NULL );
    g_assert( NULL == qof_instance_get_slots( fixture->inst ) );
    g_assert( qof_instance_get_dirty_flag( fixture->inst ) );

}

static void
test_instance_version_cmp( void )
{
    QofInstance *left, *right;
    int result;
    Timespec timespec_left, timespec_right;

    /* set up*/
    left = g_object_new( QOF_TYPE_INSTANCE, NULL );
    right = g_object_new( QOF_TYPE_INSTANCE, NULL );

    g_test_message( "Test both null" );
    result = qof_instance_version_cmp( NULL, NULL );
    g_assert_cmpint( result, == , 0 );

    g_test_message( "Test left null" );
    result = qof_instance_version_cmp( NULL, right );
    g_assert_cmpint( result, == , -1 );

    g_test_message( "Test right null" );
    result = qof_instance_version_cmp( left, NULL );
    g_assert_cmpint( result, == , 1 );

    g_test_message( "Test left tv_sec lesser than right" );
    timespec_left.tv_sec = 0;
    timespec_right.tv_sec = 1;
    qof_instance_set_last_update( left, timespec_left );
    qof_instance_set_last_update( right, timespec_right );
    result = qof_instance_version_cmp( left, right );
    g_assert_cmpint( result, == , -1 );

    g_test_message( "Test right tv_sec lesser than left" );
    timespec_left.tv_sec = 1;
    timespec_right.tv_sec = 0;
    qof_instance_set_last_update( left, timespec_left );
    qof_instance_set_last_update( right, timespec_right );
    result = qof_instance_version_cmp( left, right );
    g_assert_cmpint( result, == , 1 );

    g_test_message( "Test left tv_nsec lesser than right" );
    timespec_left.tv_sec = 1;
    timespec_left.tv_nsec = 0;
    timespec_right.tv_sec = 1;
    timespec_right.tv_nsec = 1;
    qof_instance_set_last_update( left, timespec_left );
    qof_instance_set_last_update( right, timespec_right );
    result = qof_instance_version_cmp( left, right );
    g_assert_cmpint( result, == , -1 );

    g_test_message( "Test right tv_sec lesser than left" );
    timespec_left.tv_sec = 1;
    timespec_left.tv_nsec = 1;
    timespec_right.tv_sec = 1;
    timespec_right.tv_nsec = 0;
    qof_instance_set_last_update( left, timespec_left );
    qof_instance_set_last_update( right, timespec_right );
    result = qof_instance_version_cmp( left, right );
    g_assert_cmpint( result, == , 1 );

    g_test_message( "Test both equal" );
    timespec_left.tv_sec = 1;
    timespec_left.tv_nsec = 1;
    timespec_right.tv_sec = 1;
    timespec_right.tv_nsec = 1;
    qof_instance_set_last_update( left, timespec_left );
    qof_instance_set_last_update( right, timespec_right );
    result = qof_instance_version_cmp( left, right );
    g_assert_cmpint( result, == , 0 );

    /* clear */
    g_object_unref( left );
    g_object_unref( right );
}

static void
test_instance_get_set_dirty( Fixture *fixture, gconstpointer pData )
{
    QofIdType type = "test type";
    QofCollection *col;

    /* setup */
    col = qof_collection_new ( type );
    qof_instance_set_collection( fixture->inst, col );
    g_assert( qof_instance_get_collection( fixture->inst ) );

    g_test_message( "Test get_dirty on empty instance returns false" );
    g_assert( qof_instance_get_dirty( NULL ) == FALSE );

    g_test_message( "Test dirty in normal mode" );
    g_assert( !qof_get_alt_dirty_mode() );
    g_assert( !qof_instance_get_dirty_flag( fixture->inst ) );
    g_assert( !qof_collection_is_dirty( col ) );
    g_assert( !qof_instance_get_dirty( fixture->inst ) );
    qof_instance_set_dirty( fixture->inst );
    g_assert( qof_instance_get_dirty_flag( fixture->inst ) );
    g_assert( qof_collection_is_dirty( col ) );
    g_assert( qof_instance_get_dirty( fixture->inst ) );


    g_test_message( "Test dirty in alternate mode" );
    qof_set_alt_dirty_mode ( TRUE );
    /* restore */
    qof_collection_mark_clean( col );
    qof_instance_set_dirty_flag( fixture->inst, FALSE );

    g_assert( qof_get_alt_dirty_mode() );
    g_assert( !qof_instance_get_dirty_flag( fixture->inst ) );
    g_assert( !qof_collection_is_dirty( col ) );
    g_assert( !qof_instance_get_dirty( fixture->inst ) );
    qof_instance_set_dirty( fixture->inst );
    g_assert( qof_instance_get_dirty_flag( fixture->inst ) );
    g_assert( !qof_collection_is_dirty( col ) );
    g_assert( qof_instance_get_dirty( fixture->inst ) );

    /* clean up */
    qof_instance_set_collection( fixture->inst, NULL );
    qof_collection_destroy( col );
}

/* mock display name function */
static gchar*
mock_get_display_name(const QofInstance* inst)
{
    gchar *display_name;

    g_assert( inst );
    g_assert( QOF_INSTANCE_GET_CLASS( inst )->get_display_name == mock_get_display_name );
    is_called = TRUE;
    display_name = g_strdup_printf("Mock display name %p", inst );
    return display_name;
}

static void
test_instance_display_name( Fixture *fixture, gconstpointer pData )
{
    QofIdType type = "test type";
    QofCollection *col;
    gchar *display_name, *default_display_name, *mock_display_name;

    /* setup */
    g_assert( fixture->inst );
    is_called = FALSE;
    col = qof_collection_new ( type );
    g_assert( col );
    qof_instance_set_collection( fixture->inst, col );
    g_assert( qof_instance_get_collection( fixture->inst ) );
    default_display_name = g_strdup_printf( "Object %s %p", type, fixture->inst );
    mock_display_name = g_strdup_printf( "Mock display name %p", fixture->inst );

    g_test_message( "Test instance when display name not set" );
    g_assert( QOF_INSTANCE_GET_CLASS( fixture->inst )->get_display_name == NULL );
    display_name = qof_instance_get_display_name( fixture->inst );
    g_assert( !is_called );
    g_assert_cmpstr( display_name, == , default_display_name );
    g_free( display_name );

    g_test_message( "Test instance when display name is set" );
    QOF_INSTANCE_GET_CLASS( fixture->inst )->get_display_name = mock_get_display_name;
    display_name = qof_instance_get_display_name( fixture->inst );
    g_assert( is_called );
    g_assert_cmpstr( display_name, == , mock_display_name );
    g_free( display_name );

    /* clean up */
    g_free( default_display_name );
    g_free( mock_display_name );
    qof_instance_set_collection( fixture->inst, NULL );
    qof_collection_destroy( col );
}

static void
mock_backend_begin( QofBackend *be, QofInstance *inst )
{
    g_assert( be );
    g_assert( inst );
    g_assert( be->begin == mock_backend_begin );
    g_assert_cmpstr( inst->e_type, == , "test type" );
    is_called = TRUE;
}

static void
test_instance_begin_edit( Fixture *fixture, gconstpointer pData )
{
    QofBackend *be;
    QofBook *book;
    gboolean result;

    /* setup */
    be = g_new0( QofBackend, 1 );
    g_assert( be );
    qof_backend_init( be );
    book = qof_book_new();
    g_assert( book );
    g_assert( QOF_IS_BOOK( book ) );
    qof_book_set_backend( book, be );
    g_assert( fixture->inst );
    fixture->inst->e_type = "test type";
    g_assert( qof_instance_get_dirty_flag( fixture->inst ) == FALSE );
    g_assert_cmpint( qof_instance_get_editlevel( fixture->inst ), == , 0 );

    g_test_message( "Test when instance is null" );
    result = qof_begin_edit( NULL );
    g_assert( result == FALSE );

    g_test_message( "Test when instance's editlevel is >= 1" );
    qof_instance_increase_editlevel( fixture->inst );
    result = qof_begin_edit( fixture->inst );
    g_assert( result == FALSE );
    g_assert_cmpint( qof_instance_get_editlevel( fixture->inst ), == , 2 );

    g_test_message( "Test when instance's editlevel is <= 0 and backend not set" );
    qof_instance_reset_editlevel( fixture->inst );
    result = qof_begin_edit( fixture->inst );
    g_assert( result == TRUE );
    g_assert_cmpint( qof_instance_get_editlevel( fixture->inst ), == , 1 );
    g_assert( qof_instance_get_dirty_flag( fixture->inst ) == TRUE );

    g_test_message( "Test when instance's editlevel is <= 0 and backend is set" );
    result = FALSE;
    is_called = FALSE;
    qof_instance_reset_editlevel( fixture->inst );
    qof_instance_set_dirty_flag( fixture->inst, FALSE );
    qof_instance_set_book( fixture->inst, book );
    be->begin = mock_backend_begin;
    result = qof_begin_edit( fixture->inst );
    g_assert( result == TRUE );
    g_assert_cmpint( qof_instance_get_editlevel( fixture->inst ), == , 1 );
    g_assert( qof_instance_get_dirty_flag( fixture->inst ) == FALSE );
    g_assert( is_called );

    /* clean up */
    qof_book_set_backend( book, NULL );
    qof_book_destroy( book );
    qof_backend_destroy( be );
    g_free( be );
}

static void
test_instance_commit_edit( Fixture *fixture, gconstpointer pData )
{
    gboolean result;
    gchar *msg = "[qof_commit_edit()] unbalanced call - resetting (was -2)";
    gchar *log_domain = "qof.engine";
    guint loglevel = G_LOG_LEVEL_CRITICAL | G_LOG_FLAG_FATAL, hdlr;
    TestErrorStruct check = { loglevel, log_domain, msg };

    g_test_message( "Test when instance set to null" );
    result = qof_commit_edit( NULL );
    g_assert( !result );

    g_test_message( "Test when instance's editlevel >= 2" );
    qof_instance_increase_editlevel( fixture->inst );
    qof_instance_increase_editlevel( fixture->inst );
    g_assert_cmpint( qof_instance_get_editlevel( fixture->inst ), == , 2 );
    result = qof_commit_edit( fixture->inst );
    g_assert_cmpint( qof_instance_get_editlevel( fixture->inst ), == , 1 );
    g_assert( !result );

    g_test_message( "Test when instance's editlevel = 1" );
    result = qof_commit_edit( fixture->inst );
    g_assert_cmpint( qof_instance_get_editlevel( fixture->inst ), == , 0 );
    g_assert( result );

    g_test_message( "Test when instance's editlevel < 0" );
    g_test_log_set_fatal_handler ( ( GTestLogFatalFunc )fatal_handler, NULL );
    hdlr = g_log_set_handler (log_domain, loglevel,
                              (GLogFunc)test_checked_handler, &check);
    qof_instance_decrease_editlevel( fixture->inst );
    g_assert_cmpint( qof_instance_get_editlevel( fixture->inst ), == , -1 );
    result = qof_commit_edit( fixture->inst );
    g_assert_cmpint( qof_instance_get_editlevel( fixture->inst ), == , 0 );
    g_assert_cmpstr( error_message, == , "[qof_commit_edit()] unbalanced call - resetting (was -2)" );
    g_free( error_message );
    g_log_remove_handler (log_domain, hdlr);
}

/* backend commit test start */

static struct
{
    gpointer inst;
    gpointer be;

    gboolean commit_called;
    gboolean commit_with_err_called;
    gboolean on_error_called;
    gboolean on_free_called;
    gboolean on_done_called;
    QofBackendError err;
} commit_test_part2;

static void
mock_backend_commit( QofBackend *be, QofInstance *inst )
{
    g_assert( inst );
    g_assert( be );
    g_assert( QOF_IS_INSTANCE( inst ) );
    g_assert( commit_test_part2.inst == inst );
    g_assert( commit_test_part2.be == be );
    commit_test_part2.commit_called = TRUE;
}

static void
mock_backend_commit_with_error( QofBackend *be, QofInstance *inst )
{
    g_assert( inst );
    g_assert( be );
    g_assert( QOF_IS_INSTANCE( inst ) );
    g_assert( commit_test_part2.inst == inst );
    g_assert( commit_test_part2.be == be );
    qof_backend_set_error( be, ERR_BACKEND_NO_HANDLER );
    commit_test_part2.err = ERR_BACKEND_NO_HANDLER;
    commit_test_part2.commit_with_err_called = TRUE;
}


static void
mock_on_error( QofInstance *inst, QofBackendError be_error )
{
    g_assert( inst );
    g_assert( QOF_IS_INSTANCE( inst ) );
    g_assert( commit_test_part2.err == be_error );
    commit_test_part2.on_error_called = TRUE;
}

static void
mock_on_done( QofInstance *inst )
{
    g_assert( inst );
    g_assert( QOF_IS_INSTANCE( inst ) );
    g_assert( commit_test_part2.inst == inst );
    commit_test_part2.on_done_called = TRUE;
}

static void
mock_on_free( QofInstance *inst )
{
    g_assert( inst );
    g_assert( QOF_IS_INSTANCE( inst ) );
    g_assert( commit_test_part2.inst == inst );
    commit_test_part2.on_free_called = TRUE;
}

static void
test_instance_commit_edit_part2( Fixture *fixture, gconstpointer pData )
{
    QofBackend *be;
    QofBook *book;
    gboolean result;

    /* setup */
    be = g_new0( QofBackend, 1 );
    g_assert( be );
    qof_backend_init( be );
    book = qof_book_new();
    g_assert( book );
    g_assert( QOF_IS_BOOK( book ) );
    qof_book_set_backend( book, be );

    /* init */
    result = FALSE;
    commit_test_part2.commit_called = FALSE;
    commit_test_part2.commit_with_err_called = FALSE;
    commit_test_part2.on_error_called = FALSE;
    commit_test_part2.on_free_called = FALSE;
    commit_test_part2.on_done_called = FALSE;
    commit_test_part2.inst = fixture->inst;
    commit_test_part2.be = be;
    qof_instance_set_dirty_flag( fixture->inst, TRUE );

    g_test_message( "Test when instance's backend not set, callbacks not set" );
    g_assert( qof_instance_get_infant( fixture->inst ) );
    g_assert( !qof_instance_get_destroying( fixture->inst ) );
    result = qof_commit_edit_part2( fixture->inst, NULL, NULL, NULL );
    g_assert( result );
    g_assert( qof_instance_get_dirty_flag( fixture->inst ) );
    g_assert( !qof_instance_get_infant( fixture->inst ) );
    g_assert( !commit_test_part2.commit_called );
    g_assert( !commit_test_part2.commit_with_err_called );
    g_assert( !commit_test_part2.on_error_called );
    g_assert( !commit_test_part2.on_free_called );
    g_assert( !commit_test_part2.on_done_called );

    g_test_message( "Test when instance's backend not set, do_free is true" );
    qof_instance_set_destroying( fixture->inst, TRUE );
    result = qof_commit_edit_part2( fixture->inst, mock_on_error, mock_on_done, mock_on_free );
    g_assert( result );
    g_assert( qof_instance_get_dirty_flag( fixture->inst ) );
    g_assert( !commit_test_part2.commit_called );
    g_assert( !commit_test_part2.commit_with_err_called );
    g_assert( !commit_test_part2.on_error_called );
    g_assert( commit_test_part2.on_free_called );
    g_assert( !commit_test_part2.on_done_called );

    g_test_message( "Test when instance's backend not set, do_free is false" );
    qof_instance_set_destroying( fixture->inst, FALSE );
    commit_test_part2.on_free_called = FALSE;
    result = qof_commit_edit_part2( fixture->inst, mock_on_error, mock_on_done, mock_on_free );
    g_assert( result );
    g_assert( qof_instance_get_dirty_flag( fixture->inst ) );
    g_assert( !commit_test_part2.commit_called );
    g_assert( !commit_test_part2.commit_with_err_called );
    g_assert( !commit_test_part2.on_error_called );
    g_assert( !commit_test_part2.on_free_called );
    g_assert( commit_test_part2.on_done_called );

    g_test_message( "Test when instance's backend is set, all cb set, no error produced" );
    qof_instance_set_book( fixture->inst, book );
    qof_instance_set_destroying( fixture->inst, FALSE );
    commit_test_part2.on_done_called = FALSE;
    be->commit = mock_backend_commit;
    result = qof_commit_edit_part2( fixture->inst, mock_on_error, mock_on_done, mock_on_free );
    g_assert( result );
    g_assert( !qof_instance_get_dirty_flag( fixture->inst ) );
    g_assert( commit_test_part2.commit_called );
    g_assert( !commit_test_part2.commit_with_err_called );
    g_assert( !commit_test_part2.on_error_called );
    g_assert( !commit_test_part2.on_free_called );
    g_assert( commit_test_part2.on_done_called );

    g_test_message( "Test when instance's backend is set, all cb set, error produced" );
    commit_test_part2.commit_called = FALSE;
    commit_test_part2.on_done_called = FALSE;
    be->commit = mock_backend_commit_with_error;
    qof_instance_set_dirty_flag( fixture->inst, TRUE );
    qof_instance_set_destroying( fixture->inst, TRUE );
    result = qof_commit_edit_part2( fixture->inst, mock_on_error, mock_on_done, mock_on_free );
    g_assert( !result );
    g_assert( qof_instance_get_dirty_flag( fixture->inst ) );
    g_assert( !qof_instance_get_destroying( fixture->inst ) );
    g_assert( !commit_test_part2.commit_called );
    g_assert( commit_test_part2.commit_with_err_called );
    g_assert( commit_test_part2.on_error_called );
    g_assert( !commit_test_part2.on_free_called );
    g_assert( !commit_test_part2.on_done_called );

    /* clean up */
    qof_book_set_backend( book, NULL );
    qof_book_destroy( book );
    qof_backend_destroy( be );
    g_free( be );
}

/* backend commit test end */

/* object reference tests */

static struct
{
    gpointer inst;
    gpointer ref;

    gboolean refers_to_object_called;
} refers_test_struct;

static gboolean
mock_refers_to_object( const QofInstance* inst, const QofInstance* ref )
{
    g_assert( inst );
    g_assert( ref );
    g_assert( refers_test_struct.inst == inst );
    g_assert( refers_test_struct.ref == ref );
    refers_test_struct.refers_to_object_called = TRUE;
    return TRUE;
}

static void
test_instance_refers_to_object( Fixture *fixture, gconstpointer pData )
{
    QofInstance * ref;

    ref = g_object_new( QOF_TYPE_INSTANCE, NULL );
    g_assert( fixture->inst );
    g_assert( ref );
    g_assert( QOF_INSTANCE_GET_CLASS( fixture->inst )->refers_to_object == NULL );
    refers_test_struct.refers_to_object_called = FALSE;
    refers_test_struct.inst = fixture->inst;
    refers_test_struct.ref = ref;

    g_test_message( "Test when refers to object not set" );
    g_assert( !qof_instance_refers_to_object( fixture->inst, ref ) );
    g_assert( !refers_test_struct.refers_to_object_called );

    g_test_message( "Test when refers to object set" );
    QOF_INSTANCE_GET_CLASS( fixture->inst )->refers_to_object = mock_refers_to_object;
    g_assert( qof_instance_refers_to_object( fixture->inst, ref ) );
    g_assert( refers_test_struct.refers_to_object_called );

    g_object_unref( ref );
}

static struct
{
    GList *list;
    gpointer ref;
    guint call_count;
} refers_test_struct_from_col;

static gboolean
mock_refers_to_object_from_col( const QofInstance* inst, const QofInstance* ref )
{
    g_assert( inst );
    g_assert( ref );
    g_assert( g_list_find( refers_test_struct_from_col.list, inst ) );
    g_assert( refers_test_struct_from_col.ref == ref );
    refers_test_struct_from_col.call_count++;
    refers_test_struct.refers_to_object_called = TRUE;
    return TRUE;
}

static void
test_instance_get_referring_object_list_from_collection( void )
{
    QofIdType type = "test type";
    QofBook *book;
    GList *inst_list = NULL;
    GList *result = NULL;
    QofCollection *coll;
    QofInstance *ref;
    /* randomly init number of entities >=0 and < 10 */
    gint32 list_length = g_test_rand_int_range( 0, 10 );
    int i;

    /* setup book and ref instance */
    book = qof_book_new();
    g_assert( book );
    g_assert( QOF_IS_BOOK( book ) );
    ref =  g_object_new( QOF_TYPE_INSTANCE, NULL );
    g_assert( ref );
    g_assert( QOF_IS_INSTANCE( ref ) );
    QOF_INSTANCE_GET_CLASS( ref )->refers_to_object = NULL;
    refers_test_struct_from_col.call_count = 0;
    /* init list of entities of one type,
     * put them into book collection and
     * save in the list
     */
    for (i = 0; i < list_length; i++ )
    {
        QofInstance *inst = g_object_new( QOF_TYPE_INSTANCE, NULL );
        g_assert( inst );
        qof_instance_init_data( inst, type, book );
        inst_list = g_list_append ( inst_list, inst );
        g_assert_cmpint( g_list_length( inst_list ), == , (i + 1) );
    }
    g_assert_cmpint( list_length, == , g_list_length( inst_list ) );

    g_test_message( "Test when refers to object not set" );
    coll = qof_book_get_collection( book, type );
    g_assert( coll );
    result = qof_instance_get_referring_object_list_from_collection( coll, ref );
    g_assert( !result );
    g_assert_cmpint( refers_test_struct_from_col.call_count, == , 0 );

    g_test_message( "Test when refers to object is set" );
    QOF_INSTANCE_GET_CLASS( ref )->refers_to_object = mock_refers_to_object_from_col;
    refers_test_struct_from_col.list = inst_list;
    refers_test_struct_from_col.ref = ref;
    result = qof_instance_get_referring_object_list_from_collection( coll, ref );
    if ( list_length == 0 )
        g_assert( !result );
    else
        g_assert( result );
    g_assert_cmpint( g_list_length( inst_list ), == , g_list_length( result ) );
    g_assert_cmpint( g_list_length( inst_list ), == , refers_test_struct_from_col.call_count );

    /* clean up list and destroy book */
    g_list_foreach( inst_list, (GFunc) g_object_unref, NULL );
    g_list_free( inst_list );
    g_list_free( result );
    qof_book_destroy( book );
    g_object_unref( ref );
}

static struct
{
    gpointer inst;
    gpointer ref;

    gboolean get_typed_referring_object_list_called;
} get_typed_referring_object_list_struct;

static GList*
mock_get_typed_referring_object_list( const QofInstance* inst, const QofInstance* ref )
{
    GList* result = NULL;

    g_assert( inst );
    g_assert( ref );
    g_assert( get_typed_referring_object_list_struct.inst == inst );
    g_assert( get_typed_referring_object_list_struct.ref == ref );
    get_typed_referring_object_list_struct.get_typed_referring_object_list_called = TRUE;
    return g_list_append( result, (gpointer) inst );
}

static void
test_instance_get_typed_referring_object_list( void )
{
    QofInstance *inst;
    QofInstance *ref;
    QofBook *book;
    GList* result = NULL;

    /* setup */
    inst = g_object_new( QOF_TYPE_INSTANCE, NULL );
    ref = g_object_new( QOF_TYPE_INSTANCE, NULL );
    book = qof_book_new();
    g_assert( inst );
    g_assert( ref );
    g_assert( book );
    QOF_INSTANCE_GET_CLASS( inst )->refers_to_object = NULL;
    QOF_INSTANCE_GET_CLASS( inst )->get_typed_referring_object_list = NULL;
    qof_instance_init_data( inst, "test type", book );
    get_typed_referring_object_list_struct.get_typed_referring_object_list_called = FALSE;

    /*
     * cases when refers to object is set are not tested in current function
     * as they are checked in the previous tests
     */
    g_test_message( "Test when get typed referring object list is not set" );
    result = qof_instance_get_typed_referring_object_list( inst, ref );
    g_assert( !result );
    g_assert( !get_typed_referring_object_list_struct.get_typed_referring_object_list_called );
    g_list_free( result );

    g_test_message( "Test when get typed referring object list is set" );
    QOF_INSTANCE_GET_CLASS( inst )->get_typed_referring_object_list = mock_get_typed_referring_object_list;
    get_typed_referring_object_list_struct.inst = inst;
    get_typed_referring_object_list_struct.ref = ref;
    result = qof_instance_get_typed_referring_object_list( inst, ref );
    g_assert( result );
    g_assert_cmpint( g_list_length( result ), == , 1 );
    g_assert( get_typed_referring_object_list_struct.get_typed_referring_object_list_called );
    g_list_free( result );

    /* clean */
    g_object_unref( inst );
    g_object_unref( ref );
    qof_book_destroy( book );
}

static struct
{
    guint refers_to_object_call_count;
    guint get_typed_referring_object_list_count;
} get_referring_object_list_struct;

static gboolean
mock_simple_refers_to_object( const QofInstance* inst, const QofInstance* ref )
{
    g_assert( inst );
    g_assert( ref );
    if ( inst->e_type == ref->e_type )
    {
        get_referring_object_list_struct.refers_to_object_call_count++;
        return TRUE;
    }
    return FALSE;
}

static GList*
mock_simple_get_typed_referring_object_list(const QofInstance* inst, const QofInstance* ref)
{
    g_assert( inst );
    g_assert( ref );
    get_referring_object_list_struct.get_typed_referring_object_list_count++;
    return qof_instance_get_referring_object_list_from_collection(qof_instance_get_collection(inst), ref);
}

static void
test_instance_get_referring_object_list( void )
{
    /* walk through the book's each collection's each instance */
    QofInstance *ref1;
    QofInstance *ref2;
    QofBook *book;
    QofIdType type1 = "type1";
    QofIdType type2 = "type2";
    gint32 col1_length = g_test_rand_int_range( 0, 10 );
    gint32 col2_length = g_test_rand_int_range( 0, 10 );
    GList* inst_list1 = NULL;
    GList* inst_list2 = NULL;
    GList* result = NULL;
    int i, j;

    /* setup */
    book = qof_book_new();
    g_assert( book );
    g_assert( QOF_IS_BOOK( book ) );
    ref1 = g_object_new( QOF_TYPE_INSTANCE, NULL );
    g_assert( ref1 );
    ref2 = g_object_new( QOF_TYPE_INSTANCE, NULL );
    g_assert( ref2 );
    qof_instance_init_data( ref1, type1, book );
    qof_instance_init_data( ref2, type2, book );
    QOF_INSTANCE_GET_CLASS( ref1 )->refers_to_object = NULL;
    QOF_INSTANCE_GET_CLASS( ref1 )->get_typed_referring_object_list = NULL;
    g_assert_cmpint( qof_collection_count( qof_book_get_collection( book, type1 ) ), == , 1 );
    g_assert_cmpint( qof_collection_count( qof_book_get_collection( book, type2 ) ), == , 1 );
    get_referring_object_list_struct.refers_to_object_call_count = 0;
    get_referring_object_list_struct.get_typed_referring_object_list_count = 0;
    /*
     * fill two collections with different types
     * and random number of elements
     */
    for (i = 0; i < col1_length; i++ )
    {
        QofInstance *inst = g_object_new( QOF_TYPE_INSTANCE, NULL );
        g_assert( inst );
        qof_instance_init_data( inst, type1, book );
        inst_list1 = g_list_append ( inst_list1, inst );
        g_assert_cmpint( g_list_length( inst_list1 ), == , (i + 1) );
    }
    g_assert_cmpint( qof_collection_count( qof_book_get_collection( book, type1 ) ), == , col1_length + 1 );
    g_assert_cmpint( g_list_length( inst_list1 ), == , col1_length );

    for (j = 0; j < col2_length; j++ )
    {
        QofInstance *inst = g_object_new( QOF_TYPE_INSTANCE, NULL );
        g_assert( inst );
        qof_instance_init_data( inst, type2, book );
        inst_list2 = g_list_append ( inst_list2, inst );
        g_assert_cmpint( g_list_length( inst_list2 ), == , (j + 1) );
    }
    g_assert_cmpint( qof_collection_count( qof_book_get_collection( book, type2 ) ), == , col2_length + 1 );
    g_assert_cmpint( g_list_length( inst_list2 ), == , col2_length );

    g_test_message( "Test object list returned for ref1 instance by default" );
    result = qof_instance_get_referring_object_list( ref1 );
    g_assert( !result );
    g_assert_cmpint( get_referring_object_list_struct.refers_to_object_call_count, == , 0 );

    g_test_message( "Test object list returned for ref2 instance by default" );
    result = qof_instance_get_referring_object_list( ref2 );
    g_assert( !result );
    g_assert_cmpint( get_referring_object_list_struct.refers_to_object_call_count, == , 0 );

    /*
     * refers to object is made simple as it is tested enough
     * it checks if instance types are equal
     * that is for ref1 we should get all elements from collection with type1
     * for ref2 we should get all elements from collection with type2
     */
    g_test_message( "Test object list returned for ref1 instance when refers_to_object is set" );
    QOF_INSTANCE_GET_CLASS( ref1 )->refers_to_object = mock_simple_refers_to_object;
    result = qof_instance_get_referring_object_list( ref1 );
    g_assert( result );
    g_assert_cmpint( g_list_length( result ), == , col1_length + 1 );
    g_assert_cmpint( get_referring_object_list_struct.refers_to_object_call_count, == , col1_length + 1 );
    g_list_free( result );

    g_test_message( "Test object list returned for ref2 instance when refers_to_object is set" );
    get_referring_object_list_struct.refers_to_object_call_count = 0;
    result = qof_instance_get_referring_object_list( ref2 );
    g_assert( result );
    g_assert_cmpint( g_list_length( result ), == , col2_length + 1 );
    g_assert_cmpint( get_referring_object_list_struct.refers_to_object_call_count, == , col2_length + 1 );
    g_list_free( result );

    g_test_message( "Test object list returned for ref1 instance when refers_to_object is set and get typed set" );
    QOF_INSTANCE_GET_CLASS( ref1 )->get_typed_referring_object_list = mock_simple_get_typed_referring_object_list;
    get_referring_object_list_struct.refers_to_object_call_count = 0;
    get_referring_object_list_struct.get_typed_referring_object_list_count = 0;
    result = qof_instance_get_referring_object_list( ref1 );
    g_assert( result );
    g_assert_cmpint( g_list_length( result ), == , col1_length + 1 );
    g_assert_cmpint( get_referring_object_list_struct.refers_to_object_call_count, == , col1_length + 1 );
    g_assert_cmpint( get_referring_object_list_struct.get_typed_referring_object_list_count, == , 2 );
    g_list_free( result );

    g_test_message( "Test object list returned for ref2 instance when refers_to_object is set and get typed set" );
    get_referring_object_list_struct.refers_to_object_call_count = 0;
    get_referring_object_list_struct.get_typed_referring_object_list_count = 0;
    result = qof_instance_get_referring_object_list( ref2 );
    g_assert( result );
    g_assert_cmpint( g_list_length( result ), == , col2_length + 1 );
    g_assert_cmpint( get_referring_object_list_struct.refers_to_object_call_count, == , col2_length + 1 );
    g_assert_cmpint( get_referring_object_list_struct.get_typed_referring_object_list_count, == , 2 );
    g_list_free( result );

    /* clean */
    g_object_unref( ref1 );
    g_object_unref( ref2 );
    g_list_foreach( inst_list1, (GFunc) g_object_unref, NULL );
    g_list_foreach( inst_list2, (GFunc) g_object_unref, NULL );
    g_list_free( inst_list1 );
    g_list_free( inst_list2 );
    qof_book_destroy( book );
}

void
test_suite_qofinstance ( void )
{
    GNC_TEST_ADD( suitename, "set get book", Fixture, NULL, setup, test_instance_set_get_book, teardown );
    GNC_TEST_ADD( suitename, "set get guid", Fixture, NULL, setup, test_instance_set_get_guid, teardown );
    GNC_TEST_ADD_FUNC( suitename, "instance new and destroy", test_instance_new_destroy );
    GNC_TEST_ADD_FUNC( suitename, "init data", test_instance_init_data );
    GNC_TEST_ADD( suitename, "get set slots", Fixture, NULL, setup, test_instance_get_set_slots, teardown );
    GNC_TEST_ADD_FUNC( suitename, "version compare", test_instance_version_cmp );
    GNC_TEST_ADD( suitename, "get set dirty", Fixture, NULL, setup, test_instance_get_set_dirty, teardown );
    GNC_TEST_ADD( suitename, "display name", Fixture, NULL, setup, test_instance_display_name, teardown );
    GNC_TEST_ADD( suitename, "begin edit", Fixture, NULL, setup, test_instance_begin_edit, teardown );
    GNC_TEST_ADD( suitename, "commit edit", Fixture, NULL, setup, test_instance_commit_edit, teardown );
    GNC_TEST_ADD( suitename, "commit edit part 2", Fixture, NULL, setup, test_instance_commit_edit_part2, teardown );
    GNC_TEST_ADD( suitename, "instance refers to object", Fixture, NULL, setup, test_instance_refers_to_object, teardown );
    GNC_TEST_ADD_FUNC( suitename, "instance get referring object list from collection", test_instance_get_referring_object_list_from_collection );
    GNC_TEST_ADD_FUNC( suitename, "instance get typed referring object list", test_instance_get_typed_referring_object_list);
    GNC_TEST_ADD_FUNC( suitename, "instance get referring object list", test_instance_get_referring_object_list );
}
