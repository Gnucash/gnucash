/********************************************************************
 * test-kvp_frame.c: GLib g_test test suite for kvp_frame.c.		    *
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
********************************************************************/
#ifdef __cplusplus
extern "C"
{
#endif

#include "config.h"
#include <string.h>
#include <glib.h>
#include "unittest-support.h"

#ifdef __cplusplus
}
#endif

#include "qof.h"

static const gchar *suitename = "/qof/kvp_frame";
void test_suite_kvp_frame ( void );

typedef struct
{
    KvpFrame *frame;
    GSList *hdlrs;
} Fixture;

static void
setup( Fixture *fixture, gconstpointer pData )
{
    fixture->frame = kvp_frame_new();
    fixture->hdlrs = NULL;
}

static void
teardown( Fixture *fixture, gconstpointer pData )
{
    kvp_frame_delete( fixture->frame );
    g_slist_free_full (fixture->hdlrs, test_free_log_handler);
    test_clear_error_list ();
}

static gchar* glist_to_string( const GList *list )
{
    KvpValue * val = kvp_value_new_glist( list );
    gchar * ret = kvp_value_to_string( val );
    kvp_value_delete( val );
    return ret;
}

extern KvpFrame* ( *p_get_trailer_make )( KvpFrame *frame, const char *key_path, char **end_key );
extern KvpFrame* ( *p_get_or_make )( KvpFrame *fr, const char * key );
extern const KvpFrame* ( *p_kvp_frame_get_frame_or_null_slash_trash )( const KvpFrame *frame, char *key_path );
extern const KvpFrame* ( *p_get_trailer_or_null )( const KvpFrame * frame, const char * key_path, char **end_key );

extern void init_static_test_pointers( void );

static void
setup_static( Fixture *fixture, gconstpointer pData )
{
    fixture->frame = kvp_frame_new();
    fixture->hdlrs = NULL;
    init_static_test_pointers();
    g_assert( p_get_trailer_make && p_get_or_make && p_get_trailer_or_null && 
              p_kvp_frame_get_frame_or_null_slash_trash );
}

static void
teardown_static( Fixture *fixture, gconstpointer pData )
{
    kvp_frame_delete( fixture->frame );
    g_slist_free_full (fixture->hdlrs, test_free_log_handler);
    test_clear_error_list ();
    p_get_trailer_make = NULL;
    p_get_or_make = NULL;
    p_kvp_frame_get_frame_or_null_slash_trash = NULL;
    p_get_trailer_or_null = NULL;
}

static GncGUID*
populate_frame (KvpFrame *frame)
{
    GList *list = NULL;
    Timespec ts;
    GncGUID *guid;
    GDate gdate;

    ts.tv_sec = 1;
    ts.tv_nsec = 1;
    guid = guid_new ();
    g_date_set_dmy (&gdate, 26, 1, 1957);

    kvp_frame_set_gint64( frame, "gint64-type", 100 );
    kvp_frame_set_double( frame, "double-type", 3.14159 );
    kvp_frame_set_numeric( frame, "numeric-type", gnc_numeric_zero() );
    kvp_frame_set_timespec( frame, "timespec-type", ts );
    kvp_frame_set_string( frame, "string-type", "abcdefghijklmnop" );
    kvp_frame_set_guid( frame, "guid-type", guid );
    kvp_frame_set_value_nc (frame, "gdate-type", kvp_value_new_gdate (gdate));
    kvp_frame_set_frame( frame, "frame-type", kvp_frame_new() );

    list = g_list_prepend (list, kvp_value_new_guid (guid));
    list = g_list_prepend (list, kvp_value_new_string ("qrstuvwxyz"));
    list = g_list_prepend (list, kvp_value_new_timespec (ts));
    list = g_list_prepend (list, kvp_value_new_numeric (gnc_numeric_create (256, 120)));
    list = g_list_prepend (list, kvp_value_new_double (0.4342944819));
    list = g_list_prepend (list, kvp_value_new_gint64 (0x1f2e3d4c5b6a79LL));
    kvp_frame_set_value (frame, "list-type", kvp_value_new_glist_nc (list));

    return guid;
}

static void
test_kvp_frame_new_delete( void )
{
    KvpFrame *frame;
    GncGUID *guid;

    frame = kvp_frame_new();
    g_assert( frame );
    g_assert( kvp_frame_is_empty( frame ) );

    guid = populate_frame (frame);

    g_assert( !kvp_frame_is_empty( frame ) );

    kvp_frame_delete( frame );
    g_assert( frame );
    guid_free (guid);
}

static void
test_kvp_frame_copy( Fixture *fixture, gconstpointer pData )
{
    KvpFrame *to_copy = NULL;
    gint64 test_gint64, copy_gint64;
    double test_double, copy_double;
    gnc_numeric test_gnc_numeric, copy_gnc_numeric;
    Timespec test_ts, copy_ts;
    const char* test_str, *copy_str;
    GncGUID *test_guid, *copy_guid;
    KvpFrame *test_frame, *copy_frame;

    /* init data in source frame */
    test_gint64 = 111;
    test_double = 1.1;
    test_gnc_numeric = gnc_numeric_zero();
    test_ts.tv_sec = 1;
    test_ts.tv_nsec = 1;
    test_str = "abcdefghijklmnop";
    test_guid = guid_new();
    test_frame = kvp_frame_new();

    g_assert( fixture->frame );
    kvp_frame_set_gint64( fixture->frame, "gint64-type", test_gint64 );
    kvp_frame_set_double( fixture->frame, "double-type", test_double );
    kvp_frame_set_numeric( fixture->frame, "numeric-type", test_gnc_numeric );
    kvp_frame_set_timespec( fixture->frame, "timespec-type", test_ts );
    kvp_frame_set_string( fixture->frame, "string-type", test_str );
    kvp_frame_set_guid( fixture->frame, "guid-type", test_guid );
    kvp_frame_set_frame( fixture->frame, "frame-type", test_frame );
    g_assert( !kvp_frame_is_empty( fixture->frame ) );

    g_test_message( "Test frame copy" );
    to_copy = kvp_frame_copy( fixture->frame );
    g_assert( to_copy );
    g_assert( !kvp_frame_is_empty( to_copy ) );
    g_assert( to_copy != fixture->frame );

    g_assert_cmpint( kvp_frame_compare( fixture->frame, to_copy ), == , 0 );
    copy_gint64 = kvp_frame_get_gint64( to_copy, "gint64-type" );
    g_assert( &copy_gint64 != &test_gint64 );
    g_assert_cmpint( copy_gint64, == , test_gint64 );
    copy_double = kvp_frame_get_double( to_copy, "double-type" );
    g_assert( &copy_double != &test_double );
    g_assert_cmpfloat( copy_double, == , test_double );
    copy_gnc_numeric = kvp_frame_get_numeric( to_copy, "numeric-type" );
    g_assert( &copy_gnc_numeric != &test_gnc_numeric );
    g_assert_cmpfloat( copy_gnc_numeric.num, == , test_gnc_numeric.num );
    g_assert_cmpfloat( copy_gnc_numeric.denom, == , test_gnc_numeric.denom );
    copy_ts = kvp_frame_get_timespec( to_copy, "timespec-type" );
    g_assert( &copy_ts != &test_ts );
    g_assert_cmpfloat( copy_ts.tv_sec, == , test_ts.tv_sec );
    g_assert_cmpfloat( copy_ts.tv_nsec, == , test_ts.tv_nsec );
    copy_str = kvp_frame_get_string( to_copy, "string-type" );
    g_assert( copy_str != test_str );
    g_assert_cmpstr( copy_str, == , test_str );
    copy_guid = kvp_frame_get_guid( to_copy, "guid-type" );
    g_assert( copy_guid != test_guid );
    g_assert( guid_equal( copy_guid, test_guid ) );
    copy_frame = kvp_frame_get_frame( to_copy, "frame-type");
    g_assert( copy_frame );
    g_assert( kvp_frame_is_empty( copy_frame ) );
    g_assert( copy_frame != test_frame );
    g_assert_cmpint( kvp_frame_compare( copy_frame, test_frame ), == , 0 );

    kvp_frame_delete( to_copy );
    guid_free( test_guid );
}

static void
test_kvp_frame_set_foo( Fixture *fixture, gconstpointer pData )
{
    gnc_numeric test_gnc_numeric, copy_gnc_numeric;
    Timespec test_ts, copy_ts;
    GncGUID *test_guid, *copy_guid;

    test_gnc_numeric = gnc_numeric_zero();
    test_ts.tv_sec = 1;
    test_ts.tv_nsec = 1;
    test_guid = guid_new();

    g_assert( fixture->frame );
    g_assert( kvp_frame_is_empty( fixture->frame ) );

    g_test_message( "Test gint64 setup and replace, test frame is created" );
    g_assert( kvp_frame_get_frame( fixture->frame, "/test" ) == NULL );
    kvp_frame_set_gint64( fixture->frame, "/test/gint64", 1 );
    g_assert( kvp_frame_get_frame( fixture->frame, "/test" ) != NULL );
    g_assert_cmpint( kvp_frame_get_gint64( fixture->frame, "/test/gint64" ), == , 1 );
    kvp_frame_set_gint64( fixture->frame, "/test/gint64", 5 );
    g_assert_cmpint( kvp_frame_get_gint64( fixture->frame, "/test/gint64" ), == , 5 );

    g_test_message( "Test double setup and replace, test2 frame is created" );
    g_assert( kvp_frame_get_frame( fixture->frame, "/test2" ) == NULL );
    kvp_frame_set_double( fixture->frame, "/test2/double", 1.1 );
    g_assert( kvp_frame_get_frame( fixture->frame, "/test2" ) != NULL );
    g_assert_cmpfloat( kvp_frame_get_double( fixture->frame, "/test2/double" ), == , 1.1 );
    kvp_frame_set_double( fixture->frame, "/test2/double", 5.5 );
    g_assert_cmpfloat( kvp_frame_get_double( fixture->frame, "/test2/double" ), == , 5.5 );

    g_test_message( "Test double setup and replace, test3 frame is created" );
    g_assert( kvp_frame_get_frame( fixture->frame, "/test3" ) == NULL );
    kvp_frame_set_numeric( fixture->frame, "/test3/numeric", test_gnc_numeric );
    g_assert( kvp_frame_get_frame( fixture->frame, "/test3" ) != NULL );
    copy_gnc_numeric = kvp_frame_get_numeric( fixture->frame, "/test3/numeric" );
    g_assert_cmpint( copy_gnc_numeric.num, == , 0 );
    g_assert_cmpint( copy_gnc_numeric.denom, == , 1 );
    test_gnc_numeric.num = 2;
    test_gnc_numeric.denom = 3;
    kvp_frame_set_numeric( fixture->frame, "/test3/numeric", test_gnc_numeric );
    copy_gnc_numeric = kvp_frame_get_numeric( fixture->frame, "/test3/numeric" );
    g_assert_cmpint( copy_gnc_numeric.num, == , 2 );
    g_assert_cmpint( copy_gnc_numeric.denom, == , 3 );

    g_test_message( "Test timespec setup and replace, test4 frame is created" );
    g_assert( kvp_frame_get_frame( fixture->frame, "/test4" ) == NULL );
    kvp_frame_set_timespec( fixture->frame, "/test4/timespec", test_ts );
    g_assert( kvp_frame_get_frame( fixture->frame, "/test4" ) != NULL );
    copy_ts = kvp_frame_get_timespec( fixture->frame, "/test4/timespec" );
    g_assert_cmpint( copy_ts.tv_sec, == , 1 );
    g_assert_cmpint( copy_ts.tv_nsec, == , 1 );
    test_ts.tv_sec = 7;
    test_ts.tv_nsec = 13;
    kvp_frame_set_timespec( fixture->frame, "/test4/timespec", test_ts );
    copy_ts = kvp_frame_get_timespec( fixture->frame, "/test4/timespec" );
    g_assert_cmpint( copy_ts.tv_sec, == , 7 );
    g_assert_cmpint( copy_ts.tv_nsec, == , 13 );

    g_test_message( "Test string setup and replace, test5 frame is created" );
    g_assert( kvp_frame_get_frame( fixture->frame, "/test5" ) == NULL );
    kvp_frame_set_string( fixture->frame, "/test5/string", "one string" );
    g_assert( kvp_frame_get_frame( fixture->frame, "/test5" ) != NULL );
    g_assert_cmpstr( kvp_frame_get_string( fixture->frame, "/test5/string" ), == , "one string" );
    kvp_frame_set_string( fixture->frame, "/test5/string", "another string" );
    g_assert_cmpstr( kvp_frame_get_string( fixture->frame, "/test5/string" ), == , "another string" );

    g_test_message( "Test guid setup and replace, test6 frame is created" );
    g_assert( kvp_frame_get_frame( fixture->frame, "/test6" ) == NULL );
    kvp_frame_set_guid( fixture->frame, "/test6/guid", test_guid );
    g_assert( kvp_frame_get_frame( fixture->frame, "/test6" ) != NULL );
    copy_guid = kvp_frame_get_guid( fixture->frame, "/test6/guid" );
    g_assert( guid_equal( copy_guid, test_guid ) );
    kvp_frame_set_guid( fixture->frame, "/test6/guid", guid_null() );
    copy_guid = kvp_frame_get_guid( fixture->frame, "/test6/guid" );
    g_assert( guid_equal( copy_guid, guid_null() ) );

    g_test_message( "Test frame setup and replace, test7 frame is created" );
    g_assert( kvp_frame_get_frame( fixture->frame, "/test7" ) == NULL );
    kvp_frame_set_frame( fixture->frame, "/test7", kvp_frame_new() );
    g_assert( kvp_frame_get_frame( fixture->frame, "/test7" ) != NULL );
    kvp_frame_set_frame( fixture->frame, "/test7", NULL );
    g_assert( kvp_frame_get_frame( fixture->frame, "/test7" ) == NULL );
}

static void
test_kvp_frame_get_frame_slash( Fixture *fixture, gconstpointer pData )
{
    KvpFrame *result_frame = NULL;
    /* Mostly testing static routine kvp_frmae_get_frame_slash_trash */
    g_assert( fixture->frame );

    g_test_message( "Test path with one slash same frame should be returned" );
    result_frame = kvp_frame_get_frame_slash( fixture->frame, "/" );
    g_assert( result_frame );
    g_assert( result_frame == fixture->frame );

    g_test_message( "Test path with trailing slash same frame should be returned" );
    result_frame = kvp_frame_get_frame_slash( fixture->frame, "/////" );
    g_assert( result_frame );
    g_assert( result_frame == fixture->frame );

    g_test_message( "Test new frame is created" );
    result_frame = kvp_frame_get_frame_slash( fixture->frame, "/test" );
    g_assert( result_frame );
    g_assert( result_frame != fixture->frame );
    g_assert( result_frame == kvp_frame_get_frame( fixture->frame, "/test" ) );

    g_test_message( "Test trailing slashes are ignored and frame created" );
    result_frame = kvp_frame_get_frame_slash( fixture->frame, "////test2/////" );
    g_assert( result_frame );
    g_assert( result_frame != fixture->frame );
    g_assert( result_frame == kvp_frame_get_frame( fixture->frame, "/test2" ) );
    g_assert( result_frame != kvp_frame_get_frame( fixture->frame, "/test" ) );

    g_test_message( "Test frames are created along the path if not exist and last frame is returned" );
    result_frame = kvp_frame_get_frame_slash( fixture->frame, "////test3/////test4//////" );
    g_assert( result_frame );
    g_assert( result_frame != fixture->frame );
    g_assert( result_frame != kvp_frame_get_frame( fixture->frame, "/test2" ) );
    g_assert( result_frame != kvp_frame_get_frame( fixture->frame, "/test" ) );
    g_assert( kvp_frame_get_frame( fixture->frame, "/test3" ) != NULL );
    g_assert( result_frame != kvp_frame_get_frame( fixture->frame, "/test3" ) );
    g_assert( result_frame == kvp_frame_get_frame( fixture->frame, "/test3/test4" ) );

    g_test_message( "Test existing frame is returned" );
    g_assert( result_frame == kvp_frame_get_frame_slash( fixture->frame, "////test3/////test4//////" ) );
}

static void
test_kvp_frame_get_slot_path( Fixture *fixture, gconstpointer pData )
{
    KvpValue *result_value = NULL ;

    g_assert( fixture->frame );
    g_assert( kvp_frame_is_empty( fixture->frame ) );

    g_test_message( "Test with non existing path should return NULL" );
    result_value = kvp_frame_get_slot_path( fixture->frame, "test", "test2", NULL );
    g_assert( !result_value );

    g_test_message( "Test with existing value set to current frame" );
    kvp_frame_set_gint64( fixture->frame, "/test", 1 );
    result_value = kvp_frame_get_slot_path( fixture->frame, "test", NULL );
    g_assert( result_value );
    g_assert( kvp_value_get_type( result_value ) == KVP_TYPE_GINT64 );
    g_assert_cmpint( kvp_value_get_gint64( result_value ), == , 1 );

    g_test_message( "Test should return null as test is not a frame" );
    kvp_frame_set_gint64( fixture->frame, "/test/test2", 2 );
    result_value = kvp_frame_get_slot_path( fixture->frame, "test", "test2", NULL );
    g_assert( !result_value );

    g_test_message( "Test should return last value in the path" );
    kvp_frame_set_gint64( fixture->frame, "/test2/test3", 2 );
    result_value = kvp_frame_get_slot_path( fixture->frame, "test2", "test3", NULL );
    g_assert( result_value );
    g_assert( kvp_value_get_type( result_value ) == KVP_TYPE_GINT64 );
    g_assert_cmpint( kvp_value_get_gint64( result_value ), == , 2 );

    g_test_message( "Test should return null as last value in the path does not exist" );
    result_value = kvp_frame_get_slot_path( fixture->frame, "test2", "test3", "test4", NULL );
    g_assert( !result_value );
}

static void
test_kvp_frame_get_slot_path_gslist( Fixture *fixture, gconstpointer pData )
{
    /* similar to previous test except path is passed as GSList*/
    GSList *path_list = NULL;
    KvpValue *result_value = NULL ;

    g_assert( fixture->frame );
    g_assert( kvp_frame_is_empty( fixture->frame ) );

    g_test_message( "Test with non existing path should return NULL" );
    path_list = g_slist_append (path_list, "test");
    path_list = g_slist_append (path_list, "test2");
    result_value = kvp_frame_get_slot_path_gslist( fixture->frame, path_list );
    g_assert( !result_value );

    g_test_message( "Test with existing value set to current frame" );
    path_list = g_slist_remove( path_list, "test2" );
    kvp_frame_set_gint64( fixture->frame, "/test", 1 );
    result_value = kvp_frame_get_slot_path_gslist( fixture->frame, path_list );
    g_assert( result_value );
    g_assert( kvp_value_get_type( result_value ) == KVP_TYPE_GINT64 );
    g_assert_cmpint( kvp_value_get_gint64( result_value ), == , 1 );

    g_test_message( "Test should return null as test is not a frame" );
    path_list = g_slist_append (path_list, "test2");
    kvp_frame_set_gint64( fixture->frame, "/test/test2", 2 );
    result_value = kvp_frame_get_slot_path_gslist( fixture->frame, path_list );
    g_assert( !result_value );

    g_test_message( "Test should return last value in the path" );
    path_list = g_slist_remove( path_list, "test" );
    path_list = g_slist_append (path_list, "test3");
    kvp_frame_set_gint64( fixture->frame, "/test2/test3", 2 );
    result_value = kvp_frame_get_slot_path_gslist( fixture->frame, path_list );
    g_assert( result_value );
    g_assert( kvp_value_get_type( result_value ) == KVP_TYPE_GINT64 );
    g_assert_cmpint( kvp_value_get_gint64( result_value ), == , 2 );

    g_test_message( "Test should return null as last value in the path does not exist" );
    path_list = g_slist_append (path_list, "test4");
    result_value = kvp_frame_get_slot_path_gslist( fixture->frame, path_list );
    g_assert( !result_value );
    g_slist_free( path_list );
}

static void
test_kvp_frame_add_frame_nc( Fixture *fixture, gconstpointer pData )
{
    /* basically we test static function kvp_frame_add_value_nc
     * if path not exist it's created
     * if gslist exist on the path new value added to the list
     * if any other value exist it's converted to gslist and newvalue added
     */
    KvpFrame *test_frame = NULL,
              *test_frame2 = NULL,
               *test_frame3 = NULL,
                *result_frame = NULL;
    KvpValue *result_value = NULL;
    GList *result_list = NULL;

    g_assert( fixture->frame );
    g_assert( kvp_frame_is_empty( fixture->frame ) );

    g_test_message( "Test when path does not exist it is created" );
    result_frame = kvp_frame_get_frame( fixture->frame, "/test/test2/test3" );
    g_assert( !result_frame );
    test_frame = kvp_frame_new();
    kvp_frame_add_frame_nc( fixture->frame, "/test/test2/test3", test_frame );
    result_frame = kvp_frame_get_frame( fixture->frame, "/test/test2/test3" );
    g_assert( result_frame );
    g_assert( result_frame == test_frame ); /* no copying done */
    result_frame = kvp_frame_get_frame( fixture->frame, "/test/test2" );
    g_assert( result_frame != test_frame );
    result_frame = kvp_frame_get_frame( fixture->frame, "/test" );
    g_assert( result_frame != test_frame );

    g_test_message( "Test when value exist on the path it's converted to bag and new value added" );
    test_frame2 = kvp_frame_new();
    kvp_frame_add_frame_nc( fixture->frame, "/test/test2/test3", test_frame2 );
    result_value = kvp_frame_get_value( fixture->frame, "/test/test2/test3" );
    result_list = kvp_value_get_glist( result_value );
    g_assert( result_list );
    g_assert_cmpint( g_list_length( result_list ), == , 2 );
    result_value = g_list_first( result_list )->data;
    g_assert( result_value );
    g_assert( kvp_value_get_type( result_value ) == KVP_TYPE_FRAME );
    g_assert( kvp_value_get_frame( result_value ) == test_frame );
    result_value = g_list_next( result_list )->data;
    g_assert( result_value );
    g_assert( kvp_value_get_type( result_value ) == KVP_TYPE_FRAME );
    g_assert( kvp_value_get_frame( result_value ) == test_frame2 );

    g_test_message( "Test when bag exists on the path new values are added to it" );
    test_frame3 = kvp_frame_new();
    kvp_frame_add_frame_nc( fixture->frame, "/test/test2/test3", test_frame3 );
    result_value = kvp_frame_get_value( fixture->frame, "/test/test2/test3" );
    g_assert( result_list == kvp_value_get_glist( result_value ) ); /* same list used */
    g_assert_cmpint( g_list_length( result_list ), == , 3 );
    result_value = g_list_first( result_list )->data;
    g_assert( result_value );
    g_assert( kvp_value_get_type( result_value ) == KVP_TYPE_FRAME );
    g_assert( kvp_value_get_frame( result_value ) == test_frame );
    result_value = g_list_next( result_list )->data;
    g_assert( result_value );
    g_assert( kvp_value_get_type( result_value ) == KVP_TYPE_FRAME );
    g_assert( kvp_value_get_frame( result_value ) == test_frame2 );
    result_value = g_list_last( result_list )->data;
    g_assert( result_value );
    g_assert( kvp_value_get_type( result_value ) == KVP_TYPE_FRAME );
    g_assert( kvp_value_get_frame( result_value ) == test_frame3 );
}

static void
test_kvp_value_copy( void )
{
    KvpValue *gint64_orig_value, *gint64_copy_value;
    KvpValue *double_orig_value, *double_copy_value;
    KvpValue *numeric_orig_value, *numeric_copy_value;
    KvpValue *string_orig_value, *string_copy_value;
    KvpValue *guid_orig_value, *guid_copy_value;
    KvpValue *timespec_orig_value, *timespec_copy_value;
    KvpValue *glist_orig_value, *glist_copy_value;
    KvpValue *frame_orig_value, *frame_copy_value;

    /* data init */
    gnc_numeric gnc_numeric_orig, gnc_numeric_copy;
    GncGUID *guid_orig, *guid_copy;
    Timespec ts_orig, ts_copy;
    GList *list_orig, *list_copy;
    KvpFrame *frame_orig, *frame_copy;

    gnc_numeric_orig = gnc_numeric_zero();
    guid_orig = guid_new();
    ts_orig.tv_sec = 1;
    ts_orig.tv_nsec = 1;
    list_orig = NULL;
    list_orig = g_list_append( list_orig, kvp_value_new_string( "abcdefghijklmnop" ) );
    frame_orig = kvp_frame_new();

    g_test_message( "Test creates original values and checks copies of them" );
    gint64_orig_value = kvp_value_new_gint64( 2 );
    double_orig_value = kvp_value_new_double( 3.3 );
    numeric_orig_value = kvp_value_new_gnc_numeric( gnc_numeric_orig );
    string_orig_value = kvp_value_new_string( "abcdefghijklmnop" );
    guid_orig_value = kvp_value_new_guid( guid_orig );
    timespec_orig_value = kvp_value_new_timespec( ts_orig );
    glist_orig_value = kvp_value_new_glist( list_orig );
    frame_orig_value = kvp_value_new_frame( frame_orig );
    g_assert( gint64_orig_value && double_orig_value && numeric_orig_value && string_orig_value
              && guid_orig_value && timespec_orig_value && glist_orig_value && frame_orig_value );

    /* copy values */
    gint64_copy_value = kvp_value_copy( gint64_orig_value );
    g_assert( gint64_copy_value );
    g_assert( gint64_copy_value != gint64_orig_value );
    g_assert( kvp_value_get_type( gint64_copy_value ) == KVP_TYPE_GINT64 );
    g_assert_cmpint( kvp_value_get_gint64( gint64_copy_value ), == , 2 );

    double_copy_value = kvp_value_copy( double_orig_value );
    g_assert( double_copy_value );
    g_assert( double_copy_value != double_orig_value );
    g_assert( kvp_value_get_type( double_copy_value ) == KVP_TYPE_DOUBLE );
    g_assert_cmpfloat( kvp_value_get_double( double_copy_value ), == , 3.3 );

    numeric_copy_value = kvp_value_copy( numeric_orig_value );
    g_assert( numeric_copy_value );
    g_assert( numeric_copy_value != numeric_orig_value );
    g_assert( kvp_value_get_type( numeric_copy_value ) == KVP_TYPE_NUMERIC );
    gnc_numeric_copy = kvp_value_get_numeric( numeric_copy_value );
    g_assert_cmpfloat( gnc_numeric_copy.num, == , gnc_numeric_orig.num );
    g_assert_cmpfloat( gnc_numeric_copy.denom, == , gnc_numeric_orig.denom );

    string_copy_value = kvp_value_copy( string_orig_value );
    g_assert( string_copy_value );
    g_assert( string_copy_value != string_orig_value );
    g_assert( kvp_value_get_type( string_copy_value ) == KVP_TYPE_STRING );
    g_assert_cmpstr( kvp_value_get_string( string_copy_value ), == , "abcdefghijklmnop" );

    guid_copy_value = kvp_value_copy( guid_orig_value );
    g_assert( guid_copy_value );
    g_assert( guid_copy_value != guid_orig_value );
    g_assert( kvp_value_get_type( guid_copy_value ) == KVP_TYPE_GUID );
    guid_copy = kvp_value_get_guid( guid_copy_value );
    g_assert( guid_orig != guid_copy );
    g_assert( guid_equal( guid_orig, guid_copy ) );

    timespec_copy_value = kvp_value_copy( timespec_orig_value );
    g_assert( timespec_copy_value );
    g_assert( timespec_copy_value != timespec_orig_value );
    g_assert( kvp_value_get_type( timespec_copy_value ) == KVP_TYPE_TIMESPEC );
    ts_copy = kvp_value_get_timespec( timespec_copy_value );
    g_assert_cmpfloat( ts_copy.tv_sec, == , ts_orig.tv_sec );
    g_assert_cmpfloat( ts_copy.tv_nsec, == , ts_orig.tv_nsec );

    glist_copy_value = kvp_value_copy( glist_orig_value );
    g_assert( glist_copy_value );
    g_assert( glist_copy_value != glist_orig_value );
    g_assert( kvp_value_get_type( glist_copy_value ) == KVP_TYPE_GLIST );
    list_copy = kvp_value_get_glist( glist_copy_value );
    g_assert( list_copy != list_orig );
    g_assert_cmpint( g_list_length( list_copy ), == , g_list_length( list_orig ) );
    g_assert_cmpint( kvp_glist_compare( list_orig, list_copy ), == , 0 );

    frame_copy_value = kvp_value_copy( frame_orig_value );
    g_assert( frame_copy_value );
    g_assert( frame_copy_value != frame_orig_value );
    g_assert( kvp_value_get_type( frame_copy_value ) == KVP_TYPE_FRAME );
    frame_copy = kvp_value_get_frame( frame_copy_value );
    g_assert_cmpint( kvp_frame_compare( frame_orig, frame_copy ), == , 0 );

    /* destroy objects */
    kvp_value_delete( gint64_orig_value );
    kvp_value_delete( double_orig_value );
    kvp_value_delete( numeric_orig_value );
    kvp_value_delete( string_orig_value );
    kvp_value_delete( guid_orig_value );
    kvp_value_delete( timespec_orig_value );
    kvp_value_delete( glist_orig_value );
    kvp_value_delete( frame_orig_value );

    kvp_value_delete( gint64_copy_value );
    kvp_value_delete( double_copy_value );
    kvp_value_delete( numeric_copy_value );
    kvp_value_delete( string_copy_value );
    kvp_value_delete( guid_copy_value );
    kvp_value_delete( timespec_copy_value );
    kvp_value_delete( glist_copy_value );
    kvp_value_delete( frame_copy_value );
}

static void
test_kvp_glist_copy( void )
{
    GList *value_list = NULL, *copy_list = NULL, *lp1 = NULL, *lp2 = NULL;
    KvpValue *gint64_value;
    KvpValue *double_value;
    KvpValue *numeric_value;
    KvpValue *string_value;
    KvpValue *guid_value;
    KvpValue *timespec_value;
    KvpValue *glist_value;
    KvpValue *frame_value;

    gnc_numeric gnc_numeric_orig;
    GncGUID *guid_orig;
    Timespec ts_orig;
    GList *list_orig;
    KvpFrame *frame_orig;

    gnc_numeric_orig = gnc_numeric_zero();
    guid_orig = guid_new();
    ts_orig.tv_sec = 1;
    ts_orig.tv_nsec = 1;
    list_orig = NULL;
    list_orig = g_list_append( list_orig, kvp_value_new_string( "abcdefghijklmnop" ) );
    frame_orig = kvp_frame_new();

    gint64_value = kvp_value_new_gint64( 2 );
    double_value = kvp_value_new_double( 3.3 );
    numeric_value = kvp_value_new_gnc_numeric( gnc_numeric_orig );
    string_value = kvp_value_new_string( "abcdefghijklmnop" );
    guid_value = kvp_value_new_guid( guid_orig );
    timespec_value = kvp_value_new_timespec( ts_orig );
    glist_value = kvp_value_new_glist( list_orig );
    frame_value = kvp_value_new_frame( frame_orig );

    value_list = g_list_append( value_list, gint64_value );
    value_list = g_list_append( value_list, double_value );
    value_list = g_list_append( value_list, numeric_value );
    value_list = g_list_append( value_list, string_value );
    value_list = g_list_append( value_list, guid_value );
    value_list = g_list_append( value_list, timespec_value );
    value_list = g_list_append( value_list, glist_value );
    value_list = g_list_append( value_list, frame_value );
    g_assert( value_list );
    g_assert_cmpint( g_list_length( value_list ), == , 8 );

    g_test_message( "Test list and all values are copied to new list" );
    copy_list = kvp_glist_copy( value_list );
    g_assert( copy_list );
    g_assert( copy_list != value_list );
    g_assert_cmpint( g_list_length( copy_list ), == , 8 );
    lp1 = value_list;
    lp2 = copy_list;
    while (lp1 && lp2)
    {
        KvpValue *v1 = (KvpValue *) lp1->data;
        KvpValue *v2 = (KvpValue *) lp2->data;
        g_assert( v1 != v2 );
        g_assert_cmpint( kvp_value_compare(v1, v2), == , 0 );
        lp1 = lp1->next;
        lp2 = lp2->next;
    }
    g_assert_cmpint( kvp_glist_compare( value_list, copy_list ), == , 0 );

    /* destroy */
    kvp_glist_delete( value_list );
    kvp_glist_delete( copy_list );
}

static void
test_kvp_glist_compare( void )
{
    GList *list1 = NULL, *list2 = NULL;

    KvpValue *gint64_value;
    KvpValue *double_value;
    KvpValue *numeric_value;
    KvpValue *string_value;
    KvpValue *guid_value;
    KvpValue *timespec_value;
    KvpValue *glist_value;
    KvpValue *frame_value;

    gnc_numeric gnc_numeric_orig;
    GncGUID *guid_orig;
    Timespec ts_orig;
    GList *list_orig;
    KvpFrame *frame_orig;

    gnc_numeric_orig = gnc_numeric_zero();
    guid_orig = guid_new();
    ts_orig.tv_sec = 1;
    ts_orig.tv_nsec = 1;
    list_orig = NULL;
    list_orig = g_list_append( list_orig, kvp_value_new_string( "abcdefghijklmnop" ) );
    frame_orig = kvp_frame_new();

    gint64_value = kvp_value_new_gint64( 2 );
    double_value = kvp_value_new_double( 3.3 );
    numeric_value = kvp_value_new_gnc_numeric( gnc_numeric_orig );
    string_value = kvp_value_new_string( "abcdefghijklmnop" );
    guid_value = kvp_value_new_guid( guid_orig );
    timespec_value = kvp_value_new_timespec( ts_orig );
    glist_value = kvp_value_new_glist( list_orig );
    frame_value = kvp_value_new_frame( frame_orig );

    /* init list 1 */
    list1 = g_list_append( list1, gint64_value );
    list1 = g_list_append( list1, double_value );
    list1 = g_list_append( list1, numeric_value );
    list1 = g_list_append( list1, string_value );
    list1 = g_list_append( list1, guid_value );
    list1 = g_list_append( list1, timespec_value );
    list1 = g_list_append( list1, glist_value );
    list1 = g_list_append( list1, frame_value );
    g_assert( list1 );
    g_assert_cmpint( g_list_length( list1 ), == , 8 );

    g_test_message( "Test when list is the same" );
    list2 = list1;
    g_assert_cmpint( kvp_glist_compare( list1, list2 ), == , 0 );

    g_test_message( "Test when list1 is null" );
    g_assert_cmpint( kvp_glist_compare( NULL, list2 ), == , -1 );

    g_test_message( "Test when list2 is null" );
    g_assert_cmpint( kvp_glist_compare( list1, NULL ), == , 1 );

    g_test_message( "Copy list and test they are equal" );
    list2 = kvp_glist_copy( list1 );
    g_assert( list1 != list2 );
    g_assert_cmpint( g_list_length( list1 ), == , g_list_length( list2 ) );
    g_assert_cmpint( kvp_glist_compare( list1, list2 ), == , 0 );

    g_test_message( "Test when list 1 is shorter lists are not equal" );
    list1 = g_list_remove( list1, frame_value );
    g_assert_cmpint( g_list_length( list1 ), == , 7 );
    g_assert_cmpint( g_list_length( list2 ), == , 8 );
    g_assert_cmpint( kvp_glist_compare( list1, list2 ), == , -1 );

    g_test_message( "Test when list 2 is shorter lists are not equal" );
    list1 = g_list_append( list1, frame_value );
    list1 = g_list_append( list1, frame_value );
    g_assert_cmpint( g_list_length( list1 ), == , 9 );
    g_assert_cmpint( g_list_length( list2 ), == , 8 );
    g_assert_cmpint( kvp_glist_compare( list1, list2 ), == , 1 );

    g_test_message( "Test when data is not equal lists are not equal" );
    list1 = g_list_remove( list1, frame_value );
    g_assert_cmpint( g_list_length( list1 ), == , g_list_length( list2 ) );
    g_assert_cmpint( kvp_glist_compare( list1, list2 ), == , 0 );
    list1 = g_list_remove( list1, gint64_value );
    kvp_value_delete( gint64_value );
    list1 = g_list_prepend( list1, kvp_value_new_gint64( 5 ) );
    g_assert_cmpint( g_list_length( list1 ), == , g_list_length( list2 ) );
    g_assert_cmpint( kvp_glist_compare( list1, list2 ), != , 0 );

    /* delete lists */
    kvp_glist_delete( list1 );
    kvp_glist_delete( list2 );
}

static void
test_kvp_value_compare( void )
{
    KvpValue *gint64_orig_value, *gint64_copy_value;
    KvpValue *double_orig_value, *double_copy_value;
    KvpValue *numeric_orig_value, *numeric_copy_value;
    KvpValue *string_orig_value, *string_copy_value;
    KvpValue *guid_orig_value, *guid_copy_value;
    KvpValue *timespec_orig_value, *timespec_copy_value;
    KvpValue *glist_orig_value, *glist_copy_value;
    KvpValue *frame_orig_value, *frame_copy_value;

    /* data init */
    gnc_numeric gnc_numeric_orig, gnc_numeric_copy;
    GncGUID *guid_orig, *guid_copy;
    Timespec ts_orig, ts_copy;
    GList *list_orig, *list_copy;
    KvpFrame *frame_orig, *frame_copy;

    gnc_numeric_orig = gnc_numeric_zero();
    gnc_numeric_copy = gnc_numeric_zero();
    guid_orig = guid_new();
    guid_copy = guid_new();
    ts_orig.tv_sec = 1;
    ts_orig.tv_nsec = 1;
    ts_copy.tv_sec = 2;
    ts_copy.tv_nsec = 2;
    list_orig = NULL;
    list_orig = g_list_append( list_orig, kvp_value_new_string( "abcdefghijklmnop" ) );
    list_copy = NULL;
    list_copy = g_list_append( list_copy, kvp_value_new_string( "abcdefg" ) );
    frame_orig = kvp_frame_new();
    frame_copy = kvp_frame_new();

    gint64_orig_value = kvp_value_new_gint64( 2 );
    gint64_copy_value = kvp_value_new_gint64( 5 );
    double_orig_value = kvp_value_new_double( 3.3 );
    double_copy_value = kvp_value_new_double( 3.5 );
    numeric_orig_value = kvp_value_new_gnc_numeric( gnc_numeric_orig );
    numeric_copy_value = kvp_value_new_gnc_numeric( gnc_numeric_copy );
    string_orig_value = kvp_value_new_string( "abcdefghijklmnop" );
    string_copy_value = kvp_value_new_string( "abcdefghijklmnop" );
    guid_orig_value = kvp_value_new_guid( guid_orig );
    guid_copy_value = kvp_value_new_guid( guid_copy );
    timespec_orig_value = kvp_value_new_timespec( ts_orig );
    timespec_copy_value = kvp_value_new_timespec( ts_copy );
    glist_orig_value = kvp_value_new_glist( list_orig );
    glist_copy_value = kvp_value_new_glist( list_copy );
    frame_orig_value = kvp_value_new_frame( frame_orig );
    frame_copy_value = kvp_value_new_frame( frame_copy );

    g_test_message( "Test the same kvpvalue is equal" );
    g_assert_cmpint( kvp_value_compare( gint64_orig_value, gint64_orig_value ), == , 0 );

    g_test_message( "Test first value is null" );
    g_assert_cmpint( kvp_value_compare( NULL, gint64_orig_value ), == , -1 );

    g_test_message( "Test second value is null" );
    g_assert_cmpint( kvp_value_compare( gint64_orig_value, NULL ), == , 1 );

    g_test_message( "Test diffrent data types first is lesser" );
    g_assert_cmpint( kvp_value_compare( gint64_orig_value, double_orig_value ), == , -1 );

    g_test_message( "Test diffrent data types second is lesser" );
    g_assert_cmpint( kvp_value_compare( double_orig_value, gint64_orig_value ), == , 1 );

    /* testing all different cases of data equality is not the aim
     * of this test. Rather we check that all data types are being compared.
     */
    g_test_message( "Test different kvpvalues of all the types" );
    g_assert_cmpint( kvp_value_compare( gint64_orig_value, gint64_copy_value ), == , -1 );
    g_assert_cmpint( kvp_value_compare( gint64_copy_value, gint64_orig_value ), == , 1 );
    g_assert_cmpint( kvp_value_compare( double_orig_value, double_copy_value ), == , -1 );
    g_assert_cmpint( kvp_value_compare( numeric_orig_value, numeric_copy_value ), == , gnc_numeric_compare( gnc_numeric_orig, gnc_numeric_copy ) );
    g_assert_cmpint( kvp_value_compare( string_orig_value, string_copy_value ), == , strcmp( "abcdefghijklmnop", "abcdefghijklmnop" ) );
    g_assert_cmpint( kvp_value_compare( guid_orig_value, guid_copy_value ), == , guid_compare( guid_orig, guid_copy ) );
    g_assert_cmpint( kvp_value_compare( timespec_orig_value, timespec_copy_value ), == , timespec_cmp( &ts_orig, &ts_copy ) );
    g_assert_cmpint( kvp_value_compare( glist_orig_value, glist_copy_value ), == , kvp_glist_compare( list_orig, list_copy ) );
    g_assert_cmpint( kvp_value_compare( frame_orig_value, frame_copy_value ), == , kvp_frame_compare( frame_orig, frame_copy ) );

    /* destroy objects */
    kvp_value_delete( gint64_orig_value );
    kvp_value_delete( double_orig_value );
    kvp_value_delete( numeric_orig_value );
    kvp_value_delete( string_orig_value );
    kvp_value_delete( guid_orig_value );
    kvp_value_delete( timespec_orig_value );
    kvp_value_delete( glist_orig_value );
    kvp_value_delete( frame_orig_value );

    kvp_value_delete( gint64_copy_value );
    kvp_value_delete( double_copy_value );
    kvp_value_delete( numeric_copy_value );
    kvp_value_delete( string_copy_value );
    kvp_value_delete( guid_copy_value );
    kvp_value_delete( timespec_copy_value );
    kvp_value_delete( glist_copy_value );
    kvp_value_delete( frame_copy_value );
}

static void
test_kvp_value_new_foo_nc( void )
{
    KvpValue *glist_value_nc, *frame_value_nc;
    void *val;
    guint64 size;
    GList *list = NULL;
    KvpFrame *frame = NULL;

    g_test_message( "Test new glist is not copied" );
    list = g_list_append( list, kvp_value_new_gint64( 2 ) );
    g_assert_cmpint( g_list_length( list ), == , 1 );
    glist_value_nc = kvp_value_new_glist_nc( list );
    g_assert( glist_value_nc );
    g_assert( kvp_value_get_type( glist_value_nc ) == KVP_TYPE_GLIST );
    g_assert( kvp_value_get_glist( glist_value_nc ) == list );

    g_test_message( "Test new frame is not copied" );
    frame = kvp_frame_new();
    frame_value_nc = kvp_value_new_frame_nc( frame );
    g_assert( frame_value_nc );
    g_assert( kvp_value_get_type( frame_value_nc ) == KVP_TYPE_FRAME );
    g_assert( kvp_value_get_frame( frame_value_nc ) == frame );

    kvp_value_delete( glist_value_nc );
    kvp_value_delete( frame_value_nc );
}

static void
test_kvp_frame_compare( Fixture *fixture, gconstpointer pData )
{
    KvpFrame *cmp_frame = NULL;

    cmp_frame = kvp_frame_new();
    g_assert( cmp_frame );

    g_test_message( "Test the same frame is equal with itself" );
    g_assert_cmpint( kvp_frame_compare( fixture->frame, fixture->frame ), == , 0 );

    g_test_message( "Test first frame null second not null" );
    g_assert_cmpint( kvp_frame_compare( NULL, fixture->frame ), == , -1 );

    g_test_message( "Test first frame not null second null" );
    g_assert_cmpint( kvp_frame_compare( fixture->frame, NULL ), == , 1 );

    g_test_message( "Test first frame is empty second not empty" );
    kvp_frame_set_gint64( fixture->frame, "/test/test2", 64 );
    g_assert( !kvp_frame_is_empty( fixture->frame ) );
    g_assert( kvp_frame_is_empty( cmp_frame ) );
    g_assert_cmpint( kvp_frame_compare( cmp_frame, fixture->frame ), == , -1 );

    g_test_message( "Test first frame is not empty second is empty" );
    g_assert_cmpint( kvp_frame_compare( fixture->frame, cmp_frame ), == , 1 );

    g_test_message( "Test when frames are equal" );
    kvp_frame_set_gint64( cmp_frame, "/test/test2", 64 );
    g_assert( !kvp_frame_is_empty( cmp_frame ) );
    g_assert_cmpint( kvp_frame_compare( fixture->frame, cmp_frame ), == , 0 );

    g_test_message( "Test when frames have equal data but second frame has additional slot set" );
    kvp_frame_set_string( fixture->frame, "/test/test3", "abcdefghijklmnop" );
    g_assert_cmpint( kvp_frame_compare( cmp_frame, fixture->frame ), == , -1 );

    g_test_message( "Test when frames have equal data but first frame has additional slot set" );
    g_assert_cmpint( kvp_frame_compare( fixture->frame, cmp_frame ), == , 1 );

    g_test_message( "Test when frames have equal number of slots second frame has different data in one slot" );
    kvp_frame_set_string( cmp_frame, "/test/test3", "abcdefg" );
    g_assert_cmpint( kvp_frame_compare( cmp_frame, fixture->frame ), < , 0 );

    g_test_message( "Test when frames have equal number of slots second frame has different data in one slot" );
    g_assert_cmpint( kvp_frame_compare( fixture->frame, cmp_frame ), > , 0 );

    kvp_frame_delete( cmp_frame );
}

static void
test_kvp_value_to_string( void )
{
    gchar guidstr[GUID_ENCODING_LENGTH+1];
    gchar *str_tmp2, *str_tmp3;
    gchar *result;
    KvpValue *gint64_value;
    KvpValue *double_value;
    KvpValue *numeric_value;
    KvpValue *string_value;
    KvpValue *guid_value;
    KvpValue *timespec_value;
    KvpValue *glist_value;
    KvpValue *frame_value;

    gnc_numeric gnc_numeric_orig;
    GncGUID *guid_orig;
    Timespec ts_orig;
    GList *list_orig;
    KvpFrame *frame_orig;

    gnc_numeric_orig = gnc_numeric_zero();
    guid_orig = guid_new();
    ts_orig.tv_sec = 1;
    ts_orig.tv_nsec = 1;
    list_orig = NULL;
    list_orig = g_list_append( list_orig, kvp_value_new_string( "abcdefghijklmnop" ) );
    frame_orig = kvp_frame_new();

    gint64_value = kvp_value_new_gint64( 2 );
    double_value = kvp_value_new_double( 3.3 );
    numeric_value = kvp_value_new_gnc_numeric( gnc_numeric_orig );
    string_value = kvp_value_new_string( "abcdefghijklmnop" );
    guid_value = kvp_value_new_guid( guid_orig );
    timespec_value = kvp_value_new_timespec( ts_orig );
    glist_value = kvp_value_new_glist( list_orig );
    frame_value = kvp_value_new_frame( frame_orig );

    g_test_message( "Test value string representation with different data types" );
    result = kvp_value_to_string( gint64_value );
    g_assert( result );
    g_assert_cmpstr( result, == , "KVP_VALUE_GINT64(2)" );
    g_free( result );

    result = kvp_value_to_string( double_value );
    g_assert( result );
    g_assert_cmpstr( result, == , "KVP_VALUE_DOUBLE(3.3)" );
    g_free( result );

    result = kvp_value_to_string( numeric_value );
    g_assert( result );
    g_assert_cmpstr( result, == , "KVP_VALUE_NUMERIC(0/1)" );
    g_free( result );

    result = kvp_value_to_string( string_value );
    g_assert( result );
    g_assert_cmpstr( result, == , "KVP_VALUE_STRING(abcdefghijklmnop)" );
    g_free( result );

    result = kvp_value_to_string( guid_value );
    g_assert( result );
    guid_to_string_buff( kvp_value_get_guid( guid_value ), guidstr);
    str_tmp2 = g_strdup_printf("KVP_VALUE_GUID(%s)", guidstr);
    g_assert_cmpstr( result, == , str_tmp2 );
    g_free( result );
    g_free( str_tmp2 );

    result = kvp_value_to_string( timespec_value );
    g_assert( result );
    str_tmp2 = g_new0 (char, 40);
    gnc_timespec_to_iso8601_buff( kvp_value_get_timespec( timespec_value ), str_tmp2 );
    str_tmp3 = g_strdup_printf("KVP_VALUE_TIMESPEC(%s)", str_tmp2);
    g_assert_cmpstr( result, == , str_tmp3 );
    g_free( result );
    g_free( str_tmp2 );
    g_free( str_tmp3 );

    result = kvp_value_to_string( glist_value );
    g_assert( result );
    g_assert_cmpstr( result, == , "KVP_VALUE_GLIST([  KVP_VALUE_STRING(abcdefghijklmnop), ])" );
    g_free( result );

    result = kvp_value_to_string( frame_value );
    g_assert( result );
    g_assert_cmpstr( result, == , "KVP_VALUE_FRAME({\n}\n)" );
    g_free( result );

    kvp_value_delete( gint64_value );
    kvp_value_delete( double_value );
    kvp_value_delete( numeric_value );
    kvp_value_delete( string_value );
    kvp_value_delete( guid_value );
    kvp_value_delete( timespec_value );
    kvp_value_delete( glist_value );
    kvp_value_delete( frame_value );
}

static void
test_kvp_frame_to_string( Fixture *fixture, gconstpointer pData )
{
    gchar *result;
    gnc_numeric test_gnc_numeric;
    GncGUID *test_guid;
    Timespec test_ts;
    KvpFrame *test_frame;

    test_gnc_numeric = gnc_numeric_zero();
    test_guid = guid_new();
    test_ts.tv_sec = 1;
    test_ts.tv_nsec = 1;
    test_frame = kvp_frame_new();

    g_assert( fixture->frame );
    g_assert( kvp_frame_is_empty( fixture->frame ) );

    g_test_message( "Test empty frame" );
    result = kvp_frame_to_string( fixture->frame );
    g_assert_cmpstr( result, == , "{\n}\n" );
    g_free( result );

    /* slots can be randomly distributed in hash table
     * instead of checking the whole return string we rather check if certain entries exist in it
     */
    g_test_message( "Test with all data types and nested frames" );
    kvp_frame_set_gint64( fixture->frame, "/gint64-type", 2 );
    result = kvp_frame_to_string( fixture->frame );
    g_assert( g_strrstr( result, "    gint64-type => KVP_VALUE_GINT64(2),\n" ) != NULL );
    g_free( result );

    kvp_frame_set_double( fixture->frame, "/double-type", 3.3 );
    result = kvp_frame_to_string( fixture->frame );
    g_assert( g_strrstr( result, "    double-type => KVP_VALUE_DOUBLE(3.3),\n" ) != NULL );
    g_free( result );

    kvp_frame_set_numeric( fixture->frame, "/numeric-type", test_gnc_numeric );
    result = kvp_frame_to_string( fixture->frame );
    g_assert( g_strrstr( result, "    numeric-type => KVP_VALUE_NUMERIC(0/1),\n" ) != NULL );
    g_free( result );

    kvp_frame_set_timespec( fixture->frame, "/timespec-type", test_ts );
    result = kvp_frame_to_string( fixture->frame );
    g_assert( g_strrstr( result, "    timespec-type => KVP_VALUE_TIMESPEC" ) != NULL );
    g_free( result );

    kvp_frame_set_string( fixture->frame, "/string-type", "abcdefghijklmnop" );
    result = kvp_frame_to_string( fixture->frame );
    g_assert( g_strrstr( result, "    string-type => KVP_VALUE_STRING(abcdefghijklmnop),\n" ) != NULL );
    g_free( result );

    kvp_frame_set_guid( fixture->frame, "/guid-type", test_guid );
    result = kvp_frame_to_string( fixture->frame );
    g_assert( g_strrstr( result, "    guid-type => KVP_VALUE_GUID" ) != NULL );
    g_free( result );

    kvp_frame_set_frame( fixture->frame, "/nested/frame-type", test_frame );
    result = kvp_frame_to_string( fixture->frame );
    g_assert( g_strrstr( result, "    nested => KVP_VALUE_FRAME({\n    frame-type => KVP_VALUE_FRAME({\n}\n),\n}\n),\n" ) != NULL );
    g_free( result );
}

static void
test_kvp_frame_set_slot_path( Fixture *fixture, gconstpointer pData )
{
    KvpValue *input_value, *output_value;

    g_assert( fixture->frame );
    g_assert( kvp_frame_is_empty( fixture->frame ) );

    g_test_message( "Test with a simple value added to the empty frame" );
    input_value = kvp_value_new_gint64( 2 );
    kvp_frame_set_slot_path( fixture->frame, input_value, "test", NULL );
    output_value = kvp_frame_get_slot_path( fixture->frame, "test", NULL );
    g_assert( output_value );
    g_assert( input_value != output_value ); /* copied */
    g_assert_cmpint( kvp_value_compare( output_value, input_value ), == , 0 );
    kvp_value_delete( input_value );

    g_test_message( "Test when value is being replaced" );
    input_value = kvp_value_new_double( 3.3 );
    kvp_frame_set_slot_path( fixture->frame, input_value, "test", NULL );
    output_value = kvp_frame_get_slot_path( fixture->frame, "test", NULL );
    g_assert( output_value );
    g_assert( input_value != output_value ); /* copied */
    g_assert_cmpint( kvp_value_compare( output_value, input_value ), == , 0 ); /* old value removed */
    kvp_value_delete( input_value );

    g_test_message( "Test when existing path elements are not frames" );
    input_value = kvp_value_new_string( "abcdefghijklmnop" );
    kvp_frame_set_slot_path( fixture->frame, input_value, "test", "test2", NULL );
    g_assert( kvp_frame_get_slot_path( fixture->frame, "test2", NULL ) == NULL );/* was not added */
    g_assert_cmpint( kvp_value_compare( output_value, kvp_frame_get_slot_path( fixture->frame, "test", NULL ) ), == , 0 ); /* nothing changed */
    kvp_value_delete( input_value );

    g_test_message( "Test frames are created along the path when needed" );
    input_value = kvp_value_new_string( "abcdefghijklmnop" );
    kvp_frame_set_slot_path( fixture->frame, input_value, "test2", "test3", NULL );
    output_value = kvp_frame_get_slot_path( fixture->frame, "test2", NULL );
    g_assert( output_value );
    g_assert( kvp_value_get_type( output_value ) == KVP_TYPE_FRAME );
    output_value = kvp_frame_get_slot_path( fixture->frame, "test2", "test3", NULL );
    g_assert( output_value );
    g_assert( input_value != output_value ); /* copied */
    g_assert_cmpint( kvp_value_compare( output_value, input_value ), == , 0 );
    kvp_value_delete( input_value );
}

static void
test_kvp_frame_set_slot_path_gslist( Fixture *fixture, gconstpointer pData )
{
    /* similar to previous test except path is passed as GSList*/
    GSList *path_list = NULL;
    KvpValue *input_value, *output_value;

    g_assert( fixture->frame );
    g_assert( kvp_frame_is_empty( fixture->frame ) );

    g_test_message( "Test with a simple value added to the empty frame" );
    path_list = g_slist_append( path_list, "test" );
    input_value = kvp_value_new_gint64( 2 );
    kvp_frame_set_slot_path_gslist( fixture->frame, input_value, path_list );
    output_value = kvp_frame_get_slot_path( fixture->frame, "test", NULL );
    g_assert( output_value );
    g_assert( input_value != output_value ); /* copied */
    g_assert_cmpint( kvp_value_compare( output_value, input_value ), == , 0 );
    kvp_value_delete( input_value );

    g_test_message( "Test when value is being replaced" );
    input_value = kvp_value_new_double( 3.3 );
    kvp_frame_set_slot_path_gslist( fixture->frame, input_value, path_list );
    output_value = kvp_frame_get_slot_path( fixture->frame, "test", NULL );
    g_assert( output_value );
    g_assert( input_value != output_value ); /* copied */
    g_assert_cmpint( kvp_value_compare( output_value, input_value ), == , 0 ); /* old value removed */
    kvp_value_delete( input_value );

    g_test_message( "Test when existing path elements are not frames" );
    path_list = g_slist_append( path_list, "test2");
    input_value = kvp_value_new_string( "abcdefghijklmnop" );
    kvp_frame_set_slot_path_gslist( fixture->frame, input_value, path_list );
    g_assert( kvp_frame_get_slot_path( fixture->frame, "test2", NULL ) == NULL );/* was not added */
    g_assert_cmpint( kvp_value_compare( output_value, kvp_frame_get_slot_path( fixture->frame, "test", NULL ) ), == , 0 ); /* nothing changed */
    kvp_value_delete( input_value );

    g_test_message( "Test frames are created along the path when needed" );
    path_list = g_slist_remove( path_list, "test" );
    path_list = g_slist_append( path_list, "test3");
    input_value = kvp_value_new_string( "abcdefghijklmnop" );
    kvp_frame_set_slot_path_gslist( fixture->frame, input_value, path_list );
    output_value = kvp_frame_get_slot_path( fixture->frame, "test2", NULL );
    g_assert( output_value );
    g_assert( kvp_value_get_type( output_value ) == KVP_TYPE_FRAME );
    output_value = kvp_frame_get_slot_path( fixture->frame, "test2", "test3", NULL );
    g_assert( output_value );
    g_assert( input_value != output_value ); /* copied */
    g_assert_cmpint( kvp_value_compare( output_value, input_value ), == , 0 );
    kvp_value_delete( input_value );

    g_slist_free( path_list );
}

static void
test_kvp_frame_replace_slot_nc( Fixture *fixture, gconstpointer pData )
{
    KvpValue *orig_value, *orig_value2;
    const char ** keys;
    /* test indirectly static function kvp_frame_replace_slot_nc */
    g_assert( fixture->frame );
    g_assert( kvp_frame_is_empty( fixture->frame ) );

    g_test_message( "Test when new value is created frame hash init and value stored in hash" );
    orig_value = kvp_value_new_gint64( 2 );
    kvp_frame_set_slot( fixture->frame, "test", orig_value );
    g_assert( !kvp_frame_is_empty( fixture->frame ) );
    keys = kvp_frame_get_keys( fixture->frame );
    g_assert( keys );
    g_assert( !keys[1] );
    g_assert( orig_value != kvp_frame_get_value( fixture->frame, keys[0] ) );
    g_assert_cmpint( kvp_value_compare( kvp_frame_get_value( fixture->frame, keys[0] ), orig_value ), ==, 0 );
    g_free( keys );

    g_test_message( "Test when value is replaced" );
    orig_value2 = kvp_value_new_gint64( 5 );
    kvp_frame_set_slot( fixture->frame, "test", orig_value2 );
    keys = kvp_frame_get_keys( fixture->frame );
    g_assert( keys );
    g_assert( !keys[1] );
    g_assert( orig_value != kvp_frame_get_value( fixture->frame, keys[0] ) );
    g_assert_cmpint( kvp_value_compare( kvp_frame_get_value( fixture->frame, keys[0] ), orig_value2 ), ==, 0 );
    g_assert_cmpint( kvp_value_compare( kvp_frame_get_value( fixture->frame, keys[0] ), orig_value ), !=, 0 );
    g_free( keys );

    kvp_value_delete( orig_value );
    kvp_value_delete( orig_value2 );
}

static void
test_get_trailer_make( Fixture *fixture, gconstpointer pData )
{
    char *last_key = NULL;
    KvpValue *frame_value = NULL;
    KvpFrame *frame = NULL, *frame2 = NULL;

    g_test_message( "Test null frame and empty string checks" );
    g_assert( p_get_trailer_make( NULL, "test", &last_key ) == NULL );
    g_assert( !last_key );
    g_assert( p_get_trailer_make( fixture->frame, NULL, &last_key ) == NULL );
    g_assert( !last_key );
    g_assert( p_get_trailer_make( fixture->frame, "", &last_key ) == NULL );
    g_assert( !last_key );

    g_test_message( "Test single frame on the path with no slash" );
    g_assert( p_get_trailer_make( fixture->frame, "test", &last_key ) == fixture->frame );
    g_assert_cmpstr( last_key, == , "test" );

    g_test_message( "Test single frame on the path with slash" );
    last_key = NULL;
    g_assert( p_get_trailer_make( fixture->frame, "/test", &last_key ) == fixture->frame );
    g_assert_cmpstr( last_key, == , "test" );

    g_test_message( "Test path of trailing slash" );
    last_key = NULL;
    g_assert( p_get_trailer_make( fixture->frame, "test/", &last_key ) == NULL );
    g_assert( !last_key );

    g_test_message( "Test path of two entries: frame for test should be created" );
    /* test is considered to be last frame on the path
     * and it is returned. Currently it doesn't exist and will be created
     * test2 is stripped away and returned as last entry of the path
     */
    last_key = NULL;
    frame = p_get_trailer_make( fixture->frame, "/test/test2", &last_key );
    g_assert( frame );
    g_assert( frame != fixture->frame );
    frame_value = kvp_frame_get_slot( fixture->frame, "test" );
    g_assert( frame_value );
    g_assert( kvp_value_get_frame( frame_value ) == frame );
    frame_value = kvp_frame_get_slot( frame, "test2" );
    g_assert( !frame_value );
    g_assert_cmpstr( last_key, == , "test2" );

    g_test_message( "Test path of two entries: test frame already exist" );
    /* here test frame already exist and should be returned
     */
    last_key = NULL;
    g_assert( frame == p_get_trailer_make( fixture->frame, "/test/test2", &last_key ) );
    g_assert_cmpstr( last_key, == , "test2" );

    g_test_message( "Test path of three entries: neither frame exist" );
    /* test3 and test4 frames will be created. test4 will be created inside test3 frame
     * while test3 inside fixture->frame. test4 will be returned
     * test5 stripped away and returned in last_key
     */
    last_key = NULL;
    frame = p_get_trailer_make( fixture->frame, "/test3/test4/test5", &last_key );
    g_assert( frame );
    g_assert( frame != fixture->frame );
    frame_value = kvp_frame_get_slot( fixture->frame, "test3" );
    g_assert( frame_value );
    frame2 = kvp_value_get_frame( frame_value );
    g_assert( frame2 != frame );
    g_assert( frame2 != fixture->frame );
    frame_value = kvp_frame_get_slot( frame2, "test4" );
    g_assert( frame_value );
    g_assert( kvp_value_get_frame( frame_value ) == frame );
    frame_value = kvp_frame_get_slot( frame, "test5" );
    g_assert( !frame_value );
    g_assert_cmpstr( last_key, == , "test5" );
}

static void
test_kvp_value_glist_to_string( Fixture *fixture, gconstpointer pData )
{
    /*
     * kvp_value_glist_to_string and kvp_value_to_string call each other
     */
    GList *value_list = NULL;
    gchar *result;

    gnc_numeric gnc_numeric_orig;
    GList *list_orig;
    KvpFrame *frame_orig;

    gnc_numeric_orig = gnc_numeric_zero();
    list_orig = NULL;
    list_orig = g_list_append( list_orig, kvp_value_new_string( "abcdefghijklmnop" ) );
    frame_orig = kvp_frame_new();

    g_test_message( "Test empty list" );
    result = glist_to_string( value_list );
    g_assert_cmpstr( result, == , "" );
    g_free( result );

    g_test_message( "Test list with simple and complex values" );
    value_list = g_list_append( value_list, kvp_value_new_gint64( 2 ) );
    value_list = g_list_append( value_list, kvp_value_new_double( 3.3 ) );
    value_list = g_list_append( value_list, kvp_value_new_gnc_numeric( gnc_numeric_orig ) );
    value_list = g_list_append( value_list, kvp_value_new_string( "abcdefghijklmnop" ) );
    value_list = g_list_append( value_list, kvp_value_new_glist( list_orig ) );
    value_list = g_list_append( value_list, kvp_value_new_frame( frame_orig ) );
    g_assert( value_list );
    g_assert_cmpint( g_list_length( value_list ), == , 6 );
    result = glist_to_string( value_list );

    g_assert_cmpstr( result, == , "KVP_VALUE_GLIST([  KVP_VALUE_GINT64(2), KVP_VALUE_DOUBLE(3.3), KVP_VALUE_NUMERIC(0/1), KVP_VALUE_STRING(abcdefghijklmnop), KVP_VALUE_GLIST([  KVP_VALUE_STRING(abcdefghijklmnop), ]), KVP_VALUE_FRAME({\n}\n), ])" );
    g_free( result );

    kvp_glist_delete( value_list );
}

static void
test_get_or_make( Fixture *fixture, gconstpointer pData )
{
    KvpFrame *test_frame = NULL;

    g_assert( fixture->frame );
    g_assert( kvp_frame_is_empty( fixture->frame ) );

    g_test_message( "Test new frame is created" );
    test_frame = p_get_or_make( fixture->frame, "test" );
    g_assert( test_frame );
    g_assert( test_frame != fixture->frame );
    g_assert( kvp_frame_get_frame( fixture->frame, "test" ) == test_frame );

    g_test_message( "Test existing frame is returned" );
    g_assert( test_frame == p_get_or_make( fixture->frame, "test" ) );
    g_assert( kvp_frame_get_frame( fixture->frame, "test" ) == test_frame );
}

static void
test_kvp_frame_get_frame_or_null_slash_trash( Fixture *fixture, gconstpointer pData )
{
    g_test_message( "Test null checks" );
    g_assert( p_kvp_frame_get_frame_or_null_slash_trash( NULL, "test" ) == NULL );
    g_assert( p_kvp_frame_get_frame_or_null_slash_trash( fixture->frame, NULL ) == NULL );

    g_test_message( "Test single slash and trailing slash path" );
    g_assert( p_kvp_frame_get_frame_or_null_slash_trash( fixture->frame, "/" ) == fixture->frame );
    g_assert( p_kvp_frame_get_frame_or_null_slash_trash( fixture->frame, "////" ) == fixture->frame );

    g_test_message( "Test non existing path" );
    g_assert( p_kvp_frame_get_frame_or_null_slash_trash( fixture->frame, "/test" ) == NULL );

    g_test_message( "Test existing path when value is not frame" );
    kvp_frame_set_gint64( fixture->frame, "/test", 2 );
    g_assert( p_kvp_frame_get_frame_or_null_slash_trash( fixture->frame, "/test" ) == NULL );

    g_test_message( "Test existing path when value is frame" );
    kvp_frame_set_frame( fixture->frame, "/test2", kvp_frame_new() );
    g_assert( p_kvp_frame_get_frame_or_null_slash_trash( fixture->frame, "/test2" ) != NULL );
}

static void
test_get_trailer_or_null( Fixture *fixture, gconstpointer pData )
{
    char *last_key = NULL;
    KvpFrame *frame = NULL;
    const KvpFrame* frame2 = NULL;

    g_test_message( "Test null frame and empty string checks" );
    g_assert( p_get_trailer_or_null( NULL, "test", &last_key ) == NULL );
    g_assert( !last_key );
    g_assert( p_get_trailer_or_null( fixture->frame, NULL, &last_key ) == NULL );
    g_assert( !last_key );
    g_assert( p_get_trailer_or_null( fixture->frame, "", &last_key ) == NULL );
    g_assert( !last_key );

    g_test_message( "Test single frame on the path with no slash" );
    g_assert( p_get_trailer_or_null( fixture->frame, "test", &last_key ) == fixture->frame );
    g_assert_cmpstr( last_key, == , "test" );

    g_test_message( "Test single frame on the path with slash" );
    last_key = NULL;
    g_assert( p_get_trailer_or_null( fixture->frame, "/test", &last_key ) == fixture->frame );
    g_assert_cmpstr( last_key, == , "test" );

    g_test_message( "Test path of trailing slash" );
    last_key = NULL;
    g_assert( p_get_trailer_or_null( fixture->frame, "test/", &last_key ) == NULL );
    g_assert( !last_key );

    g_test_message( "Test with non existing path" );
    last_key = NULL;
    g_assert( p_get_trailer_or_null( fixture->frame, "/test/test2", &last_key ) == NULL );
    g_assert_cmpstr( last_key, == , "test2" );

    g_test_message( "Test with existing path" );
    last_key = NULL;
    frame = kvp_frame_new();
    kvp_frame_set_frame( fixture->frame, "/test/test2", frame );
    frame2 = p_get_trailer_or_null( fixture->frame, "/test/test2", &last_key );
    g_assert( kvp_frame_get_frame( fixture->frame, "/test") == frame2 );
    g_assert_cmpstr( last_key, == , "test2" );
}

static void
test_kvp_frame_get_gvalue (Fixture *fixture, gconstpointer pData)
{
    KvpFrame *frame = fixture->frame;
    GValue *value;
    Timespec ts = {1, 1};
    GncGUID *guid = populate_frame (frame);
    GDate date;
    gchar *log_domain = "qof.kvp";
    gint log_level = G_LOG_LEVEL_WARNING | G_LOG_FLAG_FATAL;
    gchar *msg1 = "[gvalue_from_kvp_value()] Error! Attempt to transfer KvpFrame!";
    gchar *msg2 = "[gvalue_from_kvp_value()] Error! Invalid KVP Transfer Request!";
#undef _func
    TestErrorStruct *check1 = test_error_struct_new (log_domain, log_level,
							msg1);
    TestErrorStruct *check2 = test_error_struct_new (log_domain, log_level,
							msg2);
    fixture->hdlrs = test_log_set_fatal_handler (fixture->hdlrs, check1,
						 (GLogFunc)test_list_handler);
    test_add_error (check1);
    test_add_error (check2);

    g_date_clear (&date, 1);
    g_date_set_dmy (&date, 26, 1, 1957);

    value = kvp_frame_get_gvalue (frame, "gint64-type");
    g_assert (value != NULL);
    g_assert (G_VALUE_HOLDS_INT64 (value));
    g_assert_cmpint (g_value_get_int64 (value), ==, 100);
    gnc_gvalue_free (value);

    value = kvp_frame_get_gvalue (frame, "double-type");
    g_assert (value != NULL);
    g_assert (G_VALUE_HOLDS_DOUBLE (value));
    g_assert_cmpfloat (g_value_get_double (value), ==, 3.14159);
    gnc_gvalue_free (value);

    value = kvp_frame_get_gvalue (frame, "numeric-type");
    g_assert (value != NULL);
    g_assert_cmpint (G_VALUE_TYPE (value), ==, GNC_TYPE_NUMERIC);
    g_assert (gnc_numeric_zero_p (*(gnc_numeric*)g_value_get_boxed (value)));
    gnc_gvalue_free (value);

    value = kvp_frame_get_gvalue (frame, "timespec-type");
    g_assert (value != NULL);
    g_assert_cmpint (G_VALUE_TYPE (value), ==, GNC_TYPE_TIMESPEC);
    g_assert (timespec_equal (&ts, (Timespec*)g_value_get_boxed (value)));
    gnc_gvalue_free (value);

    value = kvp_frame_get_gvalue (frame, "string-type");
    g_assert (value != NULL);
    g_assert (G_VALUE_HOLDS_STRING (value));
    g_assert_cmpstr (g_value_get_string (value), ==, "abcdefghijklmnop");
    gnc_gvalue_free (value);

    value = kvp_frame_get_gvalue (frame, "guid-type");
    g_assert (value != NULL);
    g_assert_cmpint (G_VALUE_TYPE (value), ==, GNC_TYPE_GUID);
    g_assert (guid_equal (guid, (GncGUID*)g_value_get_boxed (value)));
    gnc_gvalue_free (value);

    value = kvp_frame_get_gvalue (frame, "gdate-type");
    g_assert (value != NULL);
    g_assert_cmpint (G_VALUE_TYPE (value), ==, G_TYPE_DATE);
    g_assert_cmpint (g_date_compare (&date, (GDate*)g_value_get_boxed (value)), ==, 0);
    gnc_gvalue_free (value);

    value = kvp_frame_get_gvalue (frame, "frame-type");
    g_assert (value == NULL);
    g_assert_cmpint (check1->hits, ==, 1);
    g_assert_cmpint (check2->hits, ==, 1);

    value = kvp_frame_get_gvalue (frame, "list-type");
    g_assert (value != NULL);
    g_assert_cmpint (G_VALUE_TYPE (value), ==, GNC_TYPE_VALUE_LIST);
    {
	GList *list = (GList*)g_value_get_boxed (value);
	GValue *value = NULL;

	value = (GValue*)(list->data);
	g_assert (G_VALUE_HOLDS_INT64 (value));
	g_assert (g_value_get_int64 (value) == 0x1f2e3d4c5b6a79LL);
	list = g_list_next (list);

	value = (GValue*)(list->data);
	g_assert (G_VALUE_HOLDS_DOUBLE (value));
	g_assert_cmpfloat (g_value_get_double (value), ==, 0.4342944819);
	list = g_list_next (list);

	value = (GValue*)(list->data);
	g_assert_cmpint (G_VALUE_TYPE (value), ==, GNC_TYPE_NUMERIC);
	g_assert (gnc_numeric_eq (*(gnc_numeric*)g_value_get_boxed (value),
				  gnc_numeric_create (256, 120)));
	list = g_list_next (list);

	value = (GValue*)(list->data);
	g_assert_cmpint (G_VALUE_TYPE (value), ==, GNC_TYPE_TIMESPEC);
	g_assert (timespec_equal (&ts, (Timespec*)g_value_get_boxed (value)));
	list = g_list_next (list);

	value = (GValue*)(list->data);
	g_assert (G_VALUE_HOLDS_STRING (value));
	g_assert_cmpstr (g_value_get_string (value), ==, "qrstuvwxyz");
	list = g_list_next (list);

	value = (GValue*)(list->data);
	g_assert_cmpint (G_VALUE_TYPE (value), ==, GNC_TYPE_GUID);
	g_assert (guid_equal (guid, (GncGUID*)g_value_get_boxed (value)));
	list = g_list_next (list);

	g_assert (list == NULL);

    }
    gnc_gvalue_free (value);
}

static void
test_kvp_frame_set_gvalue (Fixture *fixture, gconstpointer pData)
{
/* Bit of a shortcut: We'll use kvp_frame_get_item to make our KvpItem
 * and feed it into a new frame; something of a round-trip test.
 */
    KvpFrame *o_frame = fixture->frame;
    KvpFrame *n_frame = kvp_frame_new ();
    GValue *value;
    GList *o_list, *n_list;

    populate_frame (o_frame);

    value = kvp_frame_get_gvalue (o_frame, "gint64-type");
    g_assert (value != NULL);
    kvp_frame_set_gvalue (n_frame, "gint64-type", value);
    g_assert_cmpint (kvp_frame_get_gint64 (o_frame, "gint64-type"), ==,
		     kvp_frame_get_gint64 (n_frame, "gint64-type"));

    value = kvp_frame_get_gvalue (o_frame, "double-type");
    g_assert (value != NULL);
    kvp_frame_set_gvalue (n_frame, "double-type", value);
    g_assert_cmpint (kvp_frame_get_double (o_frame, "double-type"), ==,
		     kvp_frame_get_double (n_frame, "double-type"));

    value = kvp_frame_get_gvalue (o_frame, "numeric-type");
    g_assert (value != NULL);
    kvp_frame_set_gvalue (n_frame, "numeric-type", value);
    g_assert (gnc_numeric_equal (kvp_frame_get_numeric (o_frame, "numeric-type"),
				 kvp_frame_get_numeric (n_frame, "numeric-type")));

    value = kvp_frame_get_gvalue (o_frame, "timespec-type");
    g_assert (value != NULL);
    kvp_frame_set_gvalue (n_frame, "timespec-type", value);
    {
	Timespec o_ts = kvp_frame_get_timespec (o_frame, "timespec-type");
	Timespec n_ts = kvp_frame_get_timespec (n_frame, "timespec-type");
	g_assert (timespec_equal (&o_ts, &n_ts));
    }

    value = kvp_frame_get_gvalue (o_frame, "string-type");
    g_assert (value != NULL);
    kvp_frame_set_gvalue (n_frame, "string-type", value);
    g_assert_cmpstr (kvp_frame_get_string (o_frame, "string-type"), ==,
		     kvp_frame_get_string (n_frame, "string-type"));

    value = kvp_frame_get_gvalue (o_frame, "gdate-type");
    g_assert (value != NULL);
    kvp_frame_set_gvalue (n_frame, "gdate-type", value);
    {
	GDate o_date = kvp_value_get_gdate (kvp_frame_get_slot (o_frame,
								"gdate-type"));
	GDate n_date = kvp_value_get_gdate (kvp_frame_get_slot (n_frame,
								"gdate-type"));
	g_assert_cmpint (g_date_compare (&o_date, &n_date), ==, 0);
    }

    value = kvp_frame_get_gvalue (o_frame, "guid-type");
    g_assert (value != NULL);
    kvp_frame_set_gvalue (n_frame, "guid-type", value);
    g_assert (guid_equal (kvp_frame_get_guid (o_frame, "guid-type"),
			  kvp_frame_get_guid (n_frame, "guid-type")));

    value = kvp_frame_get_gvalue (o_frame, "list-type");
    g_assert (value != NULL);
    kvp_frame_set_gvalue (n_frame, "list-type", value);
    o_list = kvp_value_get_glist (kvp_frame_get_slot (o_frame, "list_type"));
    n_list = kvp_value_get_glist (kvp_frame_get_slot (n_frame, "list_type"));

    g_assert_cmpint (g_list_length (o_list), ==, g_list_length (n_list));
    while (o_list && n_list)
    {
	g_assert_cmpint (kvp_value_compare ((KvpValue*)o_list->data,
					    (KvpValue*)n_list->data), ==, 0);
	o_list = g_list_next (o_list);
	n_list = g_list_next (n_list);
    }
    kvp_frame_delete (n_frame);
}

static void
test_kvp_frame_get_keys( void )
{
    KvpFrame * frame = kvp_frame_new();
    const char * key1 = "number one";
    const char * key2 = "number one/number two";
    const char * key3 = "number three";
    const char * val1 = "Value1";
    const char * val2 = "Value2";
    const char * val3 = "Value3";
    unsigned int spot = 0;
    const char ** keys;
    kvp_frame_set_string(frame, key1, val1);
    kvp_frame_set_string(frame, key2, val2);
    kvp_frame_set_string(frame, key3, val3);
    keys = kvp_frame_get_keys(frame);

    g_assert(keys);
    g_assert(keys[spot]);
    g_assert(strcmp(keys[spot++], val1));
    g_assert(keys[spot]);
    g_assert(strcmp(keys[spot++], val3));
    g_assert(!keys[spot]);

    g_free(keys);
    kvp_frame_delete(frame);
}

void
test_suite_kvp_frame( void )
{
    GNC_TEST_ADD_FUNC( suitename, "kvp frame new and delete", test_kvp_frame_new_delete );
    GNC_TEST_ADD( suitename, "kvp frame copy", Fixture, NULL, setup, test_kvp_frame_copy, teardown );
    GNC_TEST_ADD( suitename, "kvp frame set foo", Fixture, NULL, setup, test_kvp_frame_set_foo, teardown );
    GNC_TEST_ADD( suitename, "kvp frame get frame slash", Fixture, NULL, setup, test_kvp_frame_get_frame_slash, teardown );
    GNC_TEST_ADD( suitename, "kvp frame get slot path", Fixture, NULL, setup, test_kvp_frame_get_slot_path, teardown );
    GNC_TEST_ADD( suitename, "kvp frame get slot path gslist", Fixture, NULL, setup, test_kvp_frame_get_slot_path_gslist, teardown );
    GNC_TEST_ADD( suitename, "kvp frame add frame nc", Fixture, NULL, setup, test_kvp_frame_add_frame_nc, teardown );
    GNC_TEST_ADD_FUNC( suitename, "kvp value copy", test_kvp_value_copy );
    GNC_TEST_ADD_FUNC( suitename, "kvp glist copy", test_kvp_glist_copy );
    GNC_TEST_ADD_FUNC( suitename, "kvp glist compare", test_kvp_glist_compare );
    GNC_TEST_ADD_FUNC( suitename, "kvp value compare", test_kvp_value_compare );
    GNC_TEST_ADD_FUNC( suitename, "kvp value new foo no copy", test_kvp_value_new_foo_nc );
    GNC_TEST_ADD( suitename, "kvp frame compare", Fixture, NULL, setup, test_kvp_frame_compare, teardown );
    GNC_TEST_ADD_FUNC( suitename, "kvp value to string", test_kvp_value_to_string );
    GNC_TEST_ADD( suitename, "kvp frame to string", Fixture, NULL, setup, test_kvp_frame_to_string, teardown );
    GNC_TEST_ADD( suitename, "kvp frame set slot path", Fixture, NULL, setup, test_kvp_frame_set_slot_path, teardown );
    GNC_TEST_ADD( suitename, "kvp frame set slot path gslist", Fixture, NULL, setup, test_kvp_frame_set_slot_path_gslist, teardown );
    GNC_TEST_ADD( suitename, "kvp frame replace slot nc", Fixture, NULL, setup, test_kvp_frame_replace_slot_nc, teardown );
    GNC_TEST_ADD_FUNC( suitename, "kvp frame get keys", test_kvp_frame_get_keys );
    GNC_TEST_ADD( suitename, "get trailer make", Fixture, NULL, setup_static, test_get_trailer_make, teardown_static );
    GNC_TEST_ADD( suitename, "kvp value glist to string", Fixture, NULL, setup_static, test_kvp_value_glist_to_string, teardown_static );
    GNC_TEST_ADD( suitename, "get or make", Fixture, NULL, setup_static, test_get_or_make, teardown_static );
    GNC_TEST_ADD( suitename, "kvp frame get frame or null slash trash", Fixture, NULL, setup_static, test_kvp_frame_get_frame_or_null_slash_trash, teardown_static );
    GNC_TEST_ADD( suitename, "get trailer or null", Fixture, NULL, setup_static, test_get_trailer_or_null, teardown_static );
    GNC_TEST_ADD ( suitename, "kvp frame get gvalue", Fixture, NULL, setup, test_kvp_frame_get_gvalue, teardown);
    GNC_TEST_ADD ( suitename, "kvp frame set gvalue", Fixture, NULL, setup, test_kvp_frame_set_gvalue, teardown);
}
