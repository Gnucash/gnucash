/********************************************************************
 * test-qofobject.c: GLib g_test test suite for qofobject.c.	    *
 * Copyright 2011 Muslim Chochlov <muslim.chochlov@gmail.com>	    *
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
#include "config.h"
#include <string.h>
#include <glib.h>
#include "test-stuff.h"
#include "qof.h"
#include "qofobject-p.h"

static const gchar *suitename = "/qof/qofobject";
void test_suite_qofobject ( void );

typedef struct
{
    QofObject *qofobject;
} Fixture;

typedef enum
{
    MOCK_OBJECT_BOOK_BEGIN = 1,
    MOCK_OBJECT_BOOK_END,
    MOCK_OBJECT_DIRTY,
    MOCK_OBJECT_MARK_CLEAN,
    EMPTY
} MockFields;

static void mock_object_book_begin( QofBook *book );
static gboolean mock_object_dirty( const QofCollection *col );
static void mock_object_mark_clean( QofCollection *col );

static QofObject*
new_object( QofIdType e_type, const char *type_label, MockFields field)
{
    QofObject *object = NULL;
    
    object = g_new0( QofObject, 1 );
    g_assert( object );
    object->interface_version = QOF_OBJECT_VERSION;
    object->e_type = e_type;
    object->type_label = type_label;
    switch( field )
    {
    case MOCK_OBJECT_BOOK_BEGIN:
	object->book_begin = mock_object_book_begin;
	break;
    case MOCK_OBJECT_BOOK_END:
	object->book_end = mock_object_book_begin;
	break;
    case MOCK_OBJECT_DIRTY:
	object->is_dirty = mock_object_dirty;
	break;
    case MOCK_OBJECT_MARK_CLEAN:
	object->mark_clean = mock_object_mark_clean;
    case EMPTY:
	break;
    }
    return object;
}

extern gboolean get_object_is_initialized( void );
extern GList* get_object_modules( void );
extern GList* get_book_list( void );
extern GHashTable* get_backend_data( void );

static void
setup( Fixture *fixture, gconstpointer pData )
{
    qof_object_initialize();
    fixture->qofobject = new_object( "my type object", "object desc", EMPTY );  
}

static void
teardown( Fixture *fixture, gconstpointer pData )
{
    g_free( fixture->qofobject );
    qof_object_shutdown();
}

/*
 * Safely generates objects and registers them
 * 
 * Input: min_objects - minimum number of objects to be generated (should be between 0 and 5)
 * 	  mock_filed - function in qofobject to be mocked
 * Output: number of generated objects
 */
static gint32
generate_and_register_objects( guint min_objects, MockFields mock_field )
{
    gint32 list_length = g_test_rand_int_range( min_objects, 5 );
    const char *types[5] = {"type1", "type2", "type3", "type4", "type5"};
    int i;
    
    g_assert_cmpint( min_objects, >=, 0 );
    g_assert_cmpint( min_objects, <, 5 );
    for (i = 0; i < list_length; i++ )
    {
	QofObject *object = new_object( types[i], "desc", mock_field );
	g_assert( object );
	g_assert( qof_object_register( object ) );
	g_assert_cmpint( g_list_length( get_object_modules() ), ==, (i + 1) );
    }
    g_assert_cmpint( list_length, ==, g_list_length( get_object_modules() ) );
    
    return list_length;
}

/*
 * TESTS 
 */
static struct
{
    GList *books;
    guint call_count;
} book_begin_struct;

static void
mock_book_begin( QofBook *book )
{
    g_assert( book );
    g_assert( book == book_begin_struct.books->data );
    book_begin_struct.books = book_begin_struct.books->next;
    book_begin_struct.call_count++;
}

static void
test_qof_object_register( Fixture *fixture, gconstpointer pData )
{
    GList *books = NULL;
    gint32 list_length = g_test_rand_int_range( 0, 5 );
    int i;
    QofObject *simple_object = NULL;    
    
    for (i = 0; i < list_length; i++ )
    {
	QofBook *book = qof_book_new();
	g_assert( book );
	books = g_list_prepend ( books, book );
	g_assert_cmpint( g_list_length( books ), ==, (i + 1) );
    }
    g_assert_cmpint( list_length, ==, g_list_length( books ) );
    
    g_test_message( "Test null check" );
    g_assert( qof_object_register( NULL ) == FALSE );
    
    g_test_message( "Test new object register with book_begin specified" );
    fixture->qofobject->book_begin = mock_book_begin;
    book_begin_struct.books = books;
    book_begin_struct.call_count = 0;
    g_assert( qof_object_register( fixture->qofobject ) == TRUE );
    g_assert( qof_object_lookup( "my type object" ) == fixture->qofobject );
    g_assert_cmpint( book_begin_struct.call_count, ==, list_length );
    
    g_test_message( "Test registering the same object one more time" );
    book_begin_struct.call_count = 0;
    g_assert( qof_object_register( fixture->qofobject ) == FALSE );
    g_assert( qof_object_lookup( "my type object" ) == fixture->qofobject );
    g_assert_cmpint( book_begin_struct.call_count, ==, 0 );
    
    g_test_message( "Test new object register without book_begin specified" );
    simple_object = new_object( "my type simple", "simple desc", EMPTY );
    g_assert( qof_object_register( simple_object ) == TRUE );
    g_assert( qof_object_lookup( "my type simple" ) == simple_object );
    g_assert_cmpint( book_begin_struct.call_count, ==, 0 );
    
    g_test_message( "Test register simple object one more time" );
    g_assert( qof_object_register( simple_object ) == FALSE );
    g_assert( qof_object_lookup( "my type simple" ) == simple_object );
      
    g_test_message( "Test book begin is called only one time when object is registered" );
    simple_object->book_begin = mock_book_begin;
    book_begin_struct.books = books;
    book_begin_struct.call_count = 0;
    g_assert( qof_object_register( simple_object ) == FALSE );
    g_assert_cmpint( book_begin_struct.call_count, ==, 0 );
    
    g_list_foreach( books, (GFunc) qof_book_destroy, NULL );
    g_list_free( books );
    g_free( simple_object );
}

static void
test_qof_object_lookup( Fixture *fixture, gconstpointer pData )
{
    g_test_message( "Test null check" );
    g_assert( qof_object_lookup( NULL ) == NULL );
    
    g_test_message( "Test existing object lookup" );
    g_assert( qof_object_register( fixture->qofobject ) == TRUE );
    g_assert( qof_object_lookup( "my type object" ) == fixture->qofobject );
    
    g_test_message( "Test non existing object lookup" );
    g_assert( qof_object_lookup( "anytype" ) == NULL );
}

static struct
{
  gpointer data1;
  gpointer data2;
} be_data;

static void
test_qof_object_backend_register_lookup( Fixture *fixture, gconstpointer pData )
{
    g_test_message( "Test register and lookup null checks" );
    g_assert( qof_object_register_backend( NULL, "test", &be_data ) == FALSE );
    g_assert( qof_object_register_backend( "", "test", &be_data ) == FALSE );
    g_assert( qof_object_register_backend( "test", NULL, &be_data ) == FALSE );
    g_assert( qof_object_register_backend( "test", "", &be_data ) == FALSE );
    g_assert( qof_object_register_backend( "test", "test", NULL ) == FALSE );
    g_assert( qof_object_lookup_backend( NULL, "test" ) == NULL );
    g_assert( qof_object_lookup_backend( "", "test" ) == NULL );
    g_assert( qof_object_lookup_backend( "test", NULL ) == NULL );
    g_assert( qof_object_lookup_backend( "test", "" ) == NULL );
    
    g_test_message( "Test new backend and type insert" );
    g_assert( qof_object_lookup_backend( "type", "backend" ) == NULL );
    g_assert( qof_object_register_backend( "type", "backend", &be_data.data1 ) == TRUE );
    g_assert( qof_object_lookup_backend( "type", "backend" ) == &be_data.data1 );
    
    g_test_message( "Test type insert into existing backend" );
    g_assert( qof_object_register_backend( "type2", "backend", &be_data.data2 ) == TRUE );
    g_assert( qof_object_lookup_backend( "type", "backend" ) == &be_data.data1 );
    g_assert( qof_object_lookup_backend( "type2", "backend" ) == &be_data.data2 );
}

static void
test_qof_object_get_type_label( Fixture *fixture, gconstpointer pData )
{
    g_assert( qof_object_get_type_label( NULL ) == NULL );
    
    g_test_message( "Test with non existing object" );
    g_assert( qof_object_get_type_label( "anytype" ) == NULL );
    
    g_test_message( "Test with existing registered object" );
    g_assert( qof_object_register( fixture->qofobject ) == TRUE );
    g_assert_cmpstr( qof_object_get_type_label( "my type object" ), ==, "object desc" );
}

static struct
{
    gpointer param;
} printable_struct;

static const char *
mock_printable( gpointer instance )
{
    g_assert( instance );
    g_assert( instance == printable_struct.param );
    return "printable was called";
}

static void
test_qof_object_printable( Fixture *fixture, gconstpointer pData )
{
    gint param;
  
    g_test_message( "Test null checks" );
    g_assert( qof_object_printable( NULL, (gpointer)&param ) == NULL );
    g_assert( qof_object_printable( "test", NULL ) == NULL );
    
    g_test_message( "Test with non registered object" );
    g_assert( qof_object_printable( "test", (gpointer)&param ) == NULL );
    
    g_test_message( "Test with registered object and printable not set" );
    g_assert( qof_object_register( fixture->qofobject ) == TRUE );
    g_assert( qof_object_printable( "my type object", (gpointer)&param ) == NULL );
    
    g_test_message( "Test with registered object and printable set" );
    fixture->qofobject->printable = mock_printable;
    printable_struct.param = (gpointer)&param;
    g_assert_cmpstr( qof_object_printable( "my type object", (gpointer)&param ), ==, "printable was called" );
}

static struct
{
    QofBook *book;
    guint call_count;
} object_book_begin_struct;

static void
mock_object_book_begin( QofBook *book )
{
    g_assert( book );
    g_assert( book == object_book_begin_struct.book );
    object_book_begin_struct.call_count++;
}

static void
test_qof_object_book_begin( Fixture *fixture, gconstpointer pData )
{
    QofBook *book = NULL, *book2 = NULL;
    gint32 list_length;
    
    g_test_message( "Test book begin with no objects" );
    g_assert_cmpint( 0, ==, g_list_length( get_book_list() ) );
    object_book_begin_struct.call_count = 0;
    book = g_object_new(QOF_TYPE_BOOK, NULL);
    g_assert( book );
    qof_object_book_begin( book );
    g_assert_cmpint( 1, ==, g_list_length( get_book_list() ) );
    g_assert_cmpint( g_list_index( get_book_list(), (gconstpointer) book), !=, -1 );
    g_assert_cmpint( object_book_begin_struct.call_count, ==, 0 );
    
    qof_book_destroy( book );
    
    list_length = generate_and_register_objects( 1, MOCK_OBJECT_BOOK_BEGIN );
    
    g_test_message( "Test book begin with random objects registered and book begin set up" );
    g_assert_cmpint( 0, ==, g_list_length( get_book_list() ) );
    book2 = g_object_new(QOF_TYPE_BOOK, NULL);
    g_assert( book2 );
    object_book_begin_struct.book = book2;
    qof_object_book_begin( book2 );
    g_assert_cmpint( 1, ==, g_list_length( get_book_list() ) );
    g_assert_cmpint( g_list_index( get_book_list(), (gconstpointer) book2 ), !=, -1 );
    g_assert_cmpint( object_book_begin_struct.call_count, ==, list_length );
    
    qof_book_destroy( book2 );
}

static void
test_qof_object_book_end( Fixture *fixture, gconstpointer pData )
{
    QofBook *book = NULL, *book2 = NULL;
    gint32 list_length;    
    
    g_test_message( "Test book with no objects" );
    book = qof_book_new();
    g_assert( book );
    object_book_begin_struct.call_count = 0;
    g_assert_cmpint( 1, ==, g_list_length( get_book_list() ) );
    g_assert_cmpint( g_list_index( get_book_list(), (gconstpointer) book), !=, -1 );
    qof_book_destroy( book ); /* calls object_book_end */
    g_assert_cmpint( object_book_begin_struct.call_count, ==, 0 );
    g_assert_cmpint( 0, ==, g_list_length( get_book_list() ) );
    
    list_length = generate_and_register_objects( 1, MOCK_OBJECT_BOOK_END );
    
    g_test_message( "Test book end with random objects registered and book end set up" );
    book2 = qof_book_new();
    g_assert( book2 );
    object_book_begin_struct.book = book2;
    g_assert_cmpint( 1, ==, g_list_length( get_book_list() ) );
    g_assert_cmpint( g_list_index( get_book_list(), (gconstpointer) book2 ), !=, -1 );
    qof_book_destroy( book2 ); /* calls object_book_end */
    g_assert_cmpint( object_book_begin_struct.call_count, ==, list_length );
    g_assert_cmpint( 0, ==, g_list_length( get_book_list() ) );
}

static struct
{
    GList *objects;
    guint call_count;
    gboolean result;
} object_dirty_struct;

static gboolean
mock_object_dirty( const QofCollection *col )
{
    QofObject *obj = NULL;
    
    g_assert( col );
    obj = object_dirty_struct.objects->data;
    object_dirty_struct.objects = object_dirty_struct.objects->next;
    g_assert( obj );
    g_assert_cmpstr( qof_collection_get_type( col ), ==, obj->e_type );
    object_dirty_struct.call_count++;
    return object_dirty_struct.result;
}

static void
test_qof_object_is_dirty( Fixture *fixture, gconstpointer pData )
{
    QofBook *book = NULL;
    gint32 list_length;

    g_test_message( "Test null check returns false" );
    g_assert( qof_object_is_dirty( NULL ) == FALSE );
    
    g_test_message( "Test with no objects" );
    book = qof_book_new();
    g_assert( book );
    object_dirty_struct.call_count = 0;
    g_assert( qof_object_is_dirty( book ) == FALSE );
    g_assert_cmpint( object_dirty_struct.call_count, ==, 0 );
    
    list_length = generate_and_register_objects( 1, MOCK_OBJECT_DIRTY );
    
    g_test_message( "Test with registered objects and suppose all collections are clean" );
    object_dirty_struct.objects = get_object_modules();
    object_dirty_struct.result = FALSE;
    g_assert( qof_object_is_dirty( book ) == FALSE );
    g_assert_cmpint( object_dirty_struct.call_count, ==, list_length );
    
    g_test_message( "Test with registered objects and suppose first collection is dirty" );
    object_dirty_struct.objects = get_object_modules();
    object_dirty_struct.result = TRUE;
    object_dirty_struct.call_count = 0;
    g_assert( qof_object_is_dirty( book ) == TRUE );
    g_assert_cmpint( object_dirty_struct.call_count, ==, 1 ); /* should break on first */
    
    qof_book_destroy( book );
}

static struct
{
    GList *objects;
    guint call_count;
} object_mark_clean_struct;

static void
mock_object_mark_clean( QofCollection *col )
{
    QofObject *obj = NULL;
    
    g_assert( col );
    obj = object_mark_clean_struct.objects->data;
    object_mark_clean_struct.objects = object_mark_clean_struct.objects->next;
    g_assert( obj );
    g_assert_cmpstr( qof_collection_get_type( col ), ==, obj->e_type );
    object_mark_clean_struct.call_count++;
}

static void
test_qof_object_mark_clean( Fixture *fixture, gconstpointer pData )
{
    QofBook *book = NULL;
    gint32 list_length;

    g_test_message( "Test with no objects" );
    book = qof_book_new();
    g_assert( book );
    object_mark_clean_struct.call_count = 0;
    g_assert_cmpint( g_list_length( get_object_modules() ), ==, 0 );
    qof_object_mark_clean( book );
    g_assert_cmpint( object_mark_clean_struct.call_count, ==, 0 );
    
    list_length = generate_and_register_objects( 1, MOCK_OBJECT_MARK_CLEAN );
    
    g_test_message( "Test with registered objects and mark clean set up" );
    object_mark_clean_struct.objects = get_object_modules();
    qof_object_mark_clean( book );
    g_assert_cmpint( object_mark_clean_struct.call_count, ==, list_length );

    qof_book_destroy( book );
}

static struct
{
    QofBook *book;
    QofInstance *inst;
    gboolean is_called;
} object_create_struct;

static gpointer
mock_object_create( QofBook *book )
{
    QofInstance *inst = NULL;
    
    inst = g_object_new(QOF_TYPE_INSTANCE, NULL);
    g_assert( inst );
    g_assert( QOF_IS_INSTANCE( inst ) );
    g_assert( book );
    g_assert( book == object_create_struct.book );
    object_create_struct.is_called = TRUE;
    object_create_struct.inst = inst;
    return inst;
}

static void
test_qof_object_new_instance( Fixture *fixture, gconstpointer pData )
{
    QofBook *book = NULL;
    QofInstance *inst = NULL;
    
    book = qof_book_new();
    g_assert( book );
    
    g_test_message( "Test null check" );
    g_assert( qof_object_new_instance( NULL, book ) == NULL );
    
    g_test_message( "Test non existing object type" );
    g_assert( qof_object_new_instance( "non existing type", book ) == NULL );
    
    g_test_message( "Test with registered object type and create not set" );
    g_assert( qof_object_register( fixture->qofobject ) );
    g_assert( qof_object_new_instance( fixture->qofobject->e_type, book ) == NULL );

    g_test_message( "Test with registered object type and create set" );
    object_create_struct.is_called = FALSE;
    object_create_struct.book = book;
    object_create_struct.inst = NULL;
    fixture->qofobject->create = mock_object_create;
    inst = qof_object_new_instance( fixture->qofobject->e_type, book );
    g_assert( inst );
    g_assert( object_create_struct.is_called == TRUE );
    g_assert( object_create_struct.inst == inst );

    g_object_unref( inst );
    qof_book_destroy( book );
}

static void
mock_object_foreach( const QofCollection *col, QofInstanceForeachCB cb, gpointer data)
{
}

static void
test_qof_object_compliance( Fixture *fixture, gconstpointer pData )
{
    g_assert( qof_object_register( fixture->qofobject ) );
    
    g_test_message( "Test when neither create nor foreach set" );
    g_assert( qof_object_compliance( fixture->qofobject->e_type, FALSE ) == FALSE );
    g_assert( qof_object_compliance( fixture->qofobject->e_type, TRUE ) == FALSE );
    
    g_test_message( "Test when only create set" );
    fixture->qofobject->create = mock_object_create;
    g_assert( qof_object_compliance( fixture->qofobject->e_type, FALSE ) == FALSE );
    g_assert( qof_object_compliance( fixture->qofobject->e_type, TRUE ) == FALSE );
    
    g_test_message( "Test when only foreach set" );
    fixture->qofobject->create = NULL;
    fixture->qofobject->foreach = mock_object_foreach;
    g_assert( qof_object_compliance( fixture->qofobject->e_type, FALSE ) == FALSE );
    g_assert( qof_object_compliance( fixture->qofobject->e_type, TRUE ) == FALSE );
    
    g_test_message( "Test when both set" );
    fixture->qofobject->create = mock_object_create;
    fixture->qofobject->foreach = mock_object_foreach;
    g_assert( qof_object_compliance( fixture->qofobject->e_type, FALSE ) == TRUE );
    g_assert( qof_object_compliance( fixture->qofobject->e_type, TRUE ) == TRUE );
}

static struct
{
    GList *objects;
    gpointer user_data;
    guint call_count;
} foreach_type_cb_struct;

static void
mock_foreach_type_cb( QofObject *object, gpointer user_data )
{
    g_assert( object );
    g_assert( user_data );
    g_assert( object == foreach_type_cb_struct.objects->data );
    g_assert( user_data == foreach_type_cb_struct.user_data );
    foreach_type_cb_struct.objects = foreach_type_cb_struct.objects->next;
    foreach_type_cb_struct.call_count++;
}

static void
test_qof_object_foreach_type( Fixture *fixture, gconstpointer pData )
{
    gint user_data;
    gint32 list_length;

    g_test_message( "Test with no objects" );
    foreach_type_cb_struct.call_count = 0;
    g_assert_cmpint( g_list_length( get_object_modules() ), ==, 0 );
    qof_object_foreach_type( mock_foreach_type_cb, ( gpointer ) &user_data );
    g_assert_cmpint( foreach_type_cb_struct.call_count, ==, 0 );
    
    list_length = generate_and_register_objects( 1, EMPTY );
    
    g_test_message( "Test foreach cb" );
    foreach_type_cb_struct.objects = get_object_modules();
    foreach_type_cb_struct.user_data = ( gpointer ) &user_data;
    foreach_type_cb_struct.call_count = 0;
    qof_object_foreach_type( mock_foreach_type_cb, ( gpointer ) &user_data );
    g_assert_cmpint( foreach_type_cb_struct.call_count, ==, list_length );
}

static struct
{
    gpointer user_data;
    QofInstanceForeachCB cb;
    QofCollection *col;
    gboolean is_called;
} foreach_cb_struct;

static void 
mock_instance_foreach_cb( QofInstance *inst, gpointer user_data )
{
}

static void
mock_foreach( const QofCollection *col, QofInstanceForeachCB cb, gpointer user_data )
{
    g_assert( col );
    g_assert( cb );
    g_assert( user_data );
    g_assert( col == foreach_cb_struct.col );
    g_assert( user_data == foreach_cb_struct.user_data );
    g_assert( cb == foreach_cb_struct.cb );
    foreach_cb_struct.is_called = TRUE;
}

static void
test_qof_object_foreach( Fixture *fixture, gconstpointer pData )
{
    gint user_data;
    QofBook *book = NULL;
    QofCollection *col = NULL;
    
    /* setup */
    book = qof_book_new();
    g_assert( book );    
    g_assert_cmpint( g_list_length( get_object_modules() ), ==, 0 );
    qof_object_register( fixture->qofobject );
    g_assert_cmpint( g_list_length( get_object_modules() ), ==, 1 );
    col = qof_book_get_collection( book, fixture->qofobject->e_type ); /* make col already exist */
    g_assert( col );

    g_test_message( "Test foreach and data" );
    foreach_cb_struct.user_data = ( gpointer ) &user_data;
    foreach_cb_struct.is_called = FALSE;
    foreach_cb_struct.col = col;
    foreach_cb_struct.cb = mock_instance_foreach_cb;
    fixture->qofobject->foreach = mock_foreach;
    qof_object_foreach( fixture->qofobject->e_type, book, mock_instance_foreach_cb, ( gpointer ) &user_data );
    g_assert( foreach_cb_struct.is_called == TRUE );
    
    qof_book_destroy( book );
}

static struct
{
    GList *instances;
    gpointer user_data;
    guint call_count;
} foreach_for_sorted_struct;

static void
mock_foreach_for_sorted( const QofCollection *col, QofInstanceForeachCB cb, gpointer user_data )
{
    GList *iter;
    
    g_assert( col );
    g_assert( cb );
    g_assert( user_data );
    
    for (iter = foreach_for_sorted_struct.instances; iter; iter = iter->next)
    {
        cb( iter->data, user_data );
    }
}

static void 
mock_instance_foreach_cb_for_sorted( QofInstance *inst, gpointer user_data )
{
    g_assert( inst );
    g_assert( user_data );
    g_assert_cmpint( g_list_index( foreach_for_sorted_struct.instances, (gconstpointer) inst ), !=, -1 );
    g_assert( user_data == foreach_for_sorted_struct.user_data );
    foreach_for_sorted_struct.call_count++;
}

static void
test_qof_object_foreach_sorted( Fixture *fixture, gconstpointer pData )
{
    int i;
    gint32 list_length = g_test_rand_int_range( 0, 5 );
    gint user_data;
    QofBook *book = NULL;
    QofCollection *col = NULL;
    foreach_for_sorted_struct.instances = NULL;
    
    /* setup */
    book = qof_book_new();
    g_assert( book );    
    g_assert_cmpint( g_list_length( get_object_modules() ), ==, 0 );
    qof_object_register( fixture->qofobject );
    g_assert_cmpint( g_list_length( get_object_modules() ), ==, 1 );

    fixture->qofobject->foreach = mock_foreach_for_sorted;
    /* init instances */
    col = qof_book_get_collection( book, fixture->qofobject->e_type );
    for (i = 0; i < list_length; i++ )
    {
	QofInstance * inst = g_object_new( QOF_TYPE_INSTANCE, NULL );
	g_assert( QOF_IS_INSTANCE( inst ) );
	foreach_for_sorted_struct.instances = g_list_append( foreach_for_sorted_struct.instances, inst );
	qof_collection_insert_entity( col, inst );
    }
    g_assert_cmpint( list_length, ==, g_list_length( foreach_for_sorted_struct.instances ) );    
    
    foreach_for_sorted_struct.call_count = 0;
    foreach_for_sorted_struct.user_data = &user_data;
    qof_object_foreach_sorted( fixture->qofobject->e_type, book, mock_instance_foreach_cb_for_sorted, ( gpointer ) &user_data );
    g_assert_cmpint( list_length, ==, foreach_for_sorted_struct.call_count );
    
    qof_book_destroy( book );
    g_list_free( foreach_for_sorted_struct.instances );
}

static struct
{
    QofIdTypeConst type;
    gpointer backend_data;
    gpointer user_data;
    guint call_count;
} foreach_backend_struct;

static void
mock_foreach_backend( QofIdTypeConst type, gpointer backend_data, gpointer user_data)
{
    g_assert( type );
    g_assert( backend_data );
    g_assert( user_data );
    g_assert_cmpstr( type, ==, foreach_backend_struct.type );
    g_assert( backend_data == foreach_backend_struct.backend_data );
    g_assert( user_data == foreach_backend_struct.user_data );
    foreach_backend_struct.call_count++;
}

static void
test_qof_object_foreach_backend( Fixture *fixture, gconstpointer pData )
{	
    gint backend_data;
    gint user_data;
    
    g_assert_cmpint( g_hash_table_size( get_backend_data() ), ==, 0 );
    qof_object_register_backend( "type1", "backend", (gpointer) &backend_data ); /* register backend */
    g_assert_cmpint( g_hash_table_size( get_backend_data() ), ==, 1 );
    
    foreach_backend_struct.call_count = 0;
    foreach_backend_struct.backend_data = (gpointer) &backend_data;
    foreach_backend_struct.user_data = (gpointer) &user_data;
    foreach_backend_struct.type = "type1";
    qof_object_foreach_backend ( "backend", mock_foreach_backend, (gpointer) &user_data);
    g_assert_cmpint( foreach_backend_struct.call_count, ==, 1 );
}

void
test_suite_qofobject (void)
{
    GNC_TEST_ADD( suitename, "qof object register", Fixture, NULL, setup, test_qof_object_register, teardown );
    GNC_TEST_ADD( suitename, "qof object lookup", Fixture, NULL, setup, test_qof_object_lookup, teardown );
    GNC_TEST_ADD( suitename, "qof object register and lookup backend", Fixture, NULL, setup, test_qof_object_backend_register_lookup, teardown );
    GNC_TEST_ADD( suitename, "qof object get type label", Fixture, NULL, setup, test_qof_object_get_type_label, teardown );
    GNC_TEST_ADD( suitename, "qof object printable", Fixture, NULL, setup, test_qof_object_printable, teardown );
    GNC_TEST_ADD( suitename, "qof object book begin", Fixture, NULL, setup, test_qof_object_book_begin, teardown );
    GNC_TEST_ADD( suitename, "qof object book end", Fixture, NULL, setup, test_qof_object_book_end, teardown );
    GNC_TEST_ADD( suitename, "qof object is dirty", Fixture, NULL, setup, test_qof_object_is_dirty, teardown );
    GNC_TEST_ADD( suitename, "qof object mark clean", Fixture, NULL, setup, test_qof_object_mark_clean, teardown );
    GNC_TEST_ADD( suitename, "qof object new instance", Fixture, NULL, setup, test_qof_object_new_instance, teardown );
    GNC_TEST_ADD( suitename, "qof object compliance", Fixture, NULL, setup, test_qof_object_compliance, teardown );
    GNC_TEST_ADD( suitename, "qof object foreach type", Fixture, NULL, setup, test_qof_object_foreach_type, teardown );
    GNC_TEST_ADD( suitename, "qof object foreach", Fixture, NULL, setup, test_qof_object_foreach, teardown );
    GNC_TEST_ADD( suitename, "qof object foreach sorted", Fixture, NULL, setup, test_qof_object_foreach_sorted, teardown );
    GNC_TEST_ADD( suitename, "qof object foreach backend", Fixture, NULL, setup, test_qof_object_foreach_backend, teardown );
}
