/********************************************************************
 * test-qofobject.c: google test test suite for qofobject.c.	    *
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
#include <gtest/gtest.h>
#include <config.h>
#include <string.h>
#include <glib.h>
#include <unittest-support.h>

#include "../qof.h"
#include "../qofobject-p.h"

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
    QofObject *object = nullptr;

    object = g_new0( QofObject, 1 );
    EXPECT_TRUE( object );
    object->interface_version = QOF_OBJECT_VERSION;
    object->e_type = e_type;
    object->type_label = type_label;
    switch ( field )
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

GList* get_object_modules( void );
GList* get_book_list( void );

class QOFObjectFixture: public ::testing::Test
{
public:
    QOFObjectFixture();
    ~QOFObjectFixture();
protected:
    QofObject* m_qofobject;
};

QOFObjectFixture::QOFObjectFixture()
{
    qof_object_initialize();
    m_qofobject = new_object( "my type object", "object desc", EMPTY );
}

QOFObjectFixture::~QOFObjectFixture()
{
    g_free( m_qofobject );
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
    guint32 list_length = g_test_rand_int_range( min_objects, 5 );
    const char *types[5] = {"type1", "type2", "type3", "type4", "type5"};

    EXPECT_GE( min_objects, 0u );
    EXPECT_LT( min_objects, 5u );
    for (size_t i = 0; i < list_length; i++ )
    {
        QofObject *object = new_object( types[i], "desc", mock_field );
        EXPECT_TRUE( object );
        EXPECT_TRUE( qof_object_register( object ) );
        EXPECT_EQ( g_list_length( get_object_modules() ), (i + 1) );
    }
    EXPECT_EQ( list_length, g_list_length( get_object_modules() ) );

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
    EXPECT_TRUE( book );
    EXPECT_EQ( book, book_begin_struct.books->data );
    book_begin_struct.books = book_begin_struct.books->next;
    book_begin_struct.call_count++;
}

TEST_F(QOFObjectFixture, object_register)
{
    GList *books = nullptr;
    guint32 list_length = g_test_rand_int_range( 0, 5 );
    QofObject *simple_object = nullptr;

    for (size_t i = 0; i < list_length; i++ )
    {
        QofBook *book = qof_book_new();
        EXPECT_TRUE( book );
        books = g_list_prepend ( books, book );
        EXPECT_EQ( g_list_length( books ), (i + 1) );
    }
    EXPECT_EQ( list_length, g_list_length( books ) );

    g_test_message( "Test null check" );
    EXPECT_EQ( qof_object_register( nullptr ), FALSE );

    g_test_message( "Test new object register with book_begin specified" );
    m_qofobject->book_begin = mock_book_begin;
    book_begin_struct.books = books;
    book_begin_struct.call_count = 0;
    EXPECT_EQ( qof_object_register( m_qofobject ), TRUE );
    EXPECT_EQ( qof_object_lookup( "my type object" ), m_qofobject );
    EXPECT_EQ( book_begin_struct.call_count, list_length );

    g_test_message( "Test registering the same object one more time" );
    book_begin_struct.call_count = 0;
    EXPECT_EQ( qof_object_register( m_qofobject ), FALSE );
    EXPECT_EQ( qof_object_lookup( "my type object" ), m_qofobject );
    EXPECT_EQ( book_begin_struct.call_count, 0u );

    g_test_message( "Test new object register without book_begin specified" );
    simple_object = new_object( "my type simple", "simple desc", EMPTY );
    EXPECT_EQ( qof_object_register( simple_object ), TRUE );
    EXPECT_EQ( qof_object_lookup( "my type simple" ), simple_object );
    EXPECT_EQ( book_begin_struct.call_count, 0u );

    g_test_message( "Test register simple object one more time" );
    EXPECT_EQ( qof_object_register( simple_object ), FALSE );
    EXPECT_EQ( qof_object_lookup( "my type simple" ), simple_object );

    g_test_message( "Test book begin is called only one time when object is registered" );
    simple_object->book_begin = mock_book_begin;
    book_begin_struct.books = books;
    book_begin_struct.call_count = 0;
    EXPECT_EQ( qof_object_register( simple_object ), FALSE );
    EXPECT_EQ( book_begin_struct.call_count, 0u );

    g_list_foreach( books, (GFunc) qof_book_destroy, nullptr );
    g_list_free( books );
    g_free( simple_object );
}

TEST_F(QOFObjectFixture, object_lookup)
{
    g_test_message( "Test null check" );
    EXPECT_EQ( qof_object_lookup( nullptr ), nullptr );

    g_test_message( "Test existing object lookup" );
    EXPECT_EQ( qof_object_register( m_qofobject ), TRUE );
    EXPECT_EQ( qof_object_lookup( "my type object" ), m_qofobject );

    g_test_message( "Test non existing object lookup" );
    EXPECT_EQ( qof_object_lookup( "anytype" ), nullptr );
}


TEST_F(QOFObjectFixture, object_get_type_label)
{
    EXPECT_EQ( qof_object_get_type_label( nullptr ), nullptr );

    g_test_message( "Test with non existing object" );
    EXPECT_EQ( qof_object_get_type_label( "anytype" ), nullptr );

    g_test_message( "Test with existing registered object" );
    EXPECT_EQ( qof_object_register( m_qofobject ), TRUE );
    EXPECT_STREQ( qof_object_get_type_label( "my type object" ), "object desc" );
}

static struct
{
    gpointer param;
} printable_struct;

static const char *
mock_printable( gpointer instance )
{
    EXPECT_TRUE( instance );
    EXPECT_EQ( instance, printable_struct.param );
    return "printable was called";
}

TEST_F(QOFObjectFixture, object_printable)
{
    gint param;

    g_test_message( "Test null checks" );
    EXPECT_EQ( qof_object_printable( nullptr, (gpointer)&param ), nullptr );
    EXPECT_EQ( qof_object_printable( "test", nullptr ), nullptr );

    g_test_message( "Test with non registered object" );
    EXPECT_EQ( qof_object_printable( "test", (gpointer)&param ), nullptr );

    g_test_message( "Test with registered object and printable not set" );
    EXPECT_EQ( qof_object_register( m_qofobject ), TRUE );
    EXPECT_EQ( qof_object_printable( "my type object", (gpointer)&param ), nullptr );

    g_test_message( "Test with registered object and printable set" );
    m_qofobject->printable = mock_printable;
    printable_struct.param = (gpointer)&param;
    EXPECT_STREQ( qof_object_printable( "my type object", (gpointer)&param ), "printable was called" );
}

static struct
{
    QofBook *book;
    guint call_count;
} object_book_begin_struct;

static void
mock_object_book_begin( QofBook *book )
{
    EXPECT_TRUE( book );
    EXPECT_EQ( book, object_book_begin_struct.book );
    object_book_begin_struct.call_count++;
}

TEST_F(QOFObjectFixture, object_book_begin)
{
    QofBook *book = nullptr, *book2 = nullptr;
    guint32 list_length;

    g_test_message( "Test book begin with no objects" );
    EXPECT_EQ( 0u, g_list_length( get_book_list() ) );
    object_book_begin_struct.call_count = 0;
    book = static_cast<QofBook*>(g_object_new(QOF_TYPE_BOOK, nullptr));
    EXPECT_TRUE( book );
    qof_object_book_begin( book );
    EXPECT_EQ( 1u, g_list_length( get_book_list() ) );
    EXPECT_NE( g_list_index( get_book_list(), (gconstpointer) book), -1 );
    EXPECT_EQ( object_book_begin_struct.call_count, 0u );

    qof_book_destroy( book );

    list_length = generate_and_register_objects( 1, MOCK_OBJECT_BOOK_BEGIN );

    g_test_message( "Test book begin with random objects registered and book begin set up" );
    EXPECT_EQ( 0u, g_list_length( get_book_list() ) );
    book2 = static_cast<QofBook*>(g_object_new(QOF_TYPE_BOOK, nullptr));
    EXPECT_TRUE( book2 );
    object_book_begin_struct.book = book2;
    qof_object_book_begin( book2 );
    EXPECT_EQ( 1u, g_list_length( get_book_list() ) );
    EXPECT_NE( g_list_index( get_book_list(), (gconstpointer) book2 ), -1 );
    EXPECT_EQ( object_book_begin_struct.call_count, list_length );

    qof_book_destroy( book2 );
    qof_object_foreach_type ((QofForeachTypeCB)g_free, nullptr);
}

TEST_F(QOFObjectFixture, object_book_end)
{
    QofBook *book = nullptr, *book2 = nullptr;
    guint32 list_length;

    g_test_message( "Test book with no objects" );
    book = qof_book_new();
    EXPECT_TRUE( book );
    object_book_begin_struct.call_count = 0;
    EXPECT_EQ( 1u, g_list_length( get_book_list() ) );
    EXPECT_NE( g_list_index( get_book_list(), (gconstpointer) book), -1 );
    qof_book_destroy( book ); /* calls object_book_end */
    EXPECT_EQ( object_book_begin_struct.call_count, 0u );
    EXPECT_EQ( 0u, g_list_length( get_book_list() ) );

    list_length = generate_and_register_objects( 1, MOCK_OBJECT_BOOK_END );

    g_test_message( "Test book end with random objects registered and book end set up" );
    book2 = qof_book_new();
    EXPECT_TRUE( book2 );
    object_book_begin_struct.book = book2;
    EXPECT_EQ( 1u, g_list_length( get_book_list() ) );
    EXPECT_NE( g_list_index( get_book_list(), (gconstpointer) book2 ), -1 );
    qof_book_destroy( book2 ); /* calls object_book_end */
    EXPECT_EQ( object_book_begin_struct.call_count, list_length );
    EXPECT_EQ( 0u, g_list_length( get_book_list() ) );
    qof_object_foreach_type ((QofForeachTypeCB)g_free, nullptr);
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
    QofObject *obj = nullptr;

    EXPECT_TRUE( col );
    obj = static_cast<QofObject*>(object_dirty_struct.objects->data);
    object_dirty_struct.objects = object_dirty_struct.objects->next;
    EXPECT_TRUE( obj );
    EXPECT_STREQ( qof_collection_get_type( col ), obj->e_type );
    object_dirty_struct.call_count++;
    return object_dirty_struct.result;
}

TEST_F(QOFObjectFixture, object_is_dirty)
{
    QofBook *book = nullptr;
    guint32 list_length;

    g_test_message( "Test null check returns false" );
    EXPECT_EQ( qof_object_is_dirty( nullptr ), FALSE );

    g_test_message( "Test with no objects" );
    book = qof_book_new();
    EXPECT_TRUE( book );
    object_dirty_struct.call_count = 0;
    EXPECT_EQ( qof_object_is_dirty( book ), FALSE );
    EXPECT_EQ( object_dirty_struct.call_count, 0u );

    list_length = generate_and_register_objects( 1, MOCK_OBJECT_DIRTY );

    g_test_message( "Test with registered objects and suppose all collections are clean" );
    object_dirty_struct.objects = get_object_modules();
    object_dirty_struct.result = FALSE;
    EXPECT_EQ( qof_object_is_dirty( book ), FALSE );
    EXPECT_EQ( object_dirty_struct.call_count, list_length );

    g_test_message( "Test with registered objects and suppose first collection is dirty" );
    object_dirty_struct.objects = get_object_modules();
    object_dirty_struct.result = TRUE;
    object_dirty_struct.call_count = 0;
    EXPECT_EQ( qof_object_is_dirty( book ), TRUE );
    EXPECT_EQ( object_dirty_struct.call_count, 1u ); /* should break on first */

    qof_book_destroy( book );
    qof_object_foreach_type ((QofForeachTypeCB)g_free, nullptr);
}

static struct
{
    GList *objects;
    guint call_count;
} object_mark_clean_struct;

static void
mock_object_mark_clean( QofCollection *col )
{
    QofObject *obj = nullptr;

    EXPECT_TRUE( col );
    obj = static_cast<QofObject*>(object_mark_clean_struct.objects->data);
    object_mark_clean_struct.objects = object_mark_clean_struct.objects->next;
    EXPECT_TRUE( obj );
    EXPECT_STREQ( qof_collection_get_type( col ), obj->e_type );
    object_mark_clean_struct.call_count++;
}

TEST_F(QOFObjectFixture, object_mark_clean)
{
    QofBook *book = nullptr;
    guint32 list_length;

    g_test_message( "Test with no objects" );
    book = qof_book_new();
    EXPECT_TRUE( book );
    object_mark_clean_struct.call_count = 0;
    EXPECT_EQ( g_list_length( get_object_modules() ), 0u );
    qof_object_mark_clean( book );
    EXPECT_EQ( object_mark_clean_struct.call_count, 0u );

    list_length = generate_and_register_objects( 1, MOCK_OBJECT_MARK_CLEAN );

    g_test_message( "Test with registered objects and mark clean set up" );
    object_mark_clean_struct.objects = get_object_modules();
    qof_object_mark_clean( book );
    EXPECT_EQ( object_mark_clean_struct.call_count, list_length );

    qof_book_destroy( book );
    qof_object_foreach_type ((QofForeachTypeCB)g_free, nullptr);
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
    QofInstance *inst = nullptr;

    inst = static_cast<QofInstance*>(g_object_new(QOF_TYPE_INSTANCE, nullptr));
    EXPECT_TRUE( inst );
    EXPECT_TRUE( QOF_IS_INSTANCE( inst ) );
    EXPECT_TRUE( book );
    EXPECT_EQ( book, object_create_struct.book );
    object_create_struct.is_called = TRUE;
    object_create_struct.inst = inst;
    return inst;
}

TEST_F(QOFObjectFixture, object_new_instance)
{
    QofBook *book = nullptr;
    QofInstance *inst = nullptr;

    book = qof_book_new();
    EXPECT_TRUE( book );

    g_test_message( "Test null check" );
    EXPECT_EQ( qof_object_new_instance( nullptr, book ), nullptr );

    g_test_message( "Test non existing object type" );
    EXPECT_EQ( qof_object_new_instance( "non existing type", book ), nullptr );

    g_test_message( "Test with registered object type and create not set" );
    EXPECT_TRUE( qof_object_register( m_qofobject ) );
    EXPECT_EQ( qof_object_new_instance( m_qofobject->e_type, book ), nullptr );

    g_test_message( "Test with registered object type and create set" );
    object_create_struct.is_called = FALSE;
    object_create_struct.book = book;
    object_create_struct.inst = nullptr;
    m_qofobject->create = mock_object_create;
    inst = static_cast<QofInstance*>(qof_object_new_instance( m_qofobject->e_type, book ));
    EXPECT_TRUE( inst );
    EXPECT_EQ( object_create_struct.is_called, TRUE );
    EXPECT_EQ( object_create_struct.inst, inst );

    g_object_unref( inst );
    qof_book_destroy( book );
}

static void
mock_object_foreach( const QofCollection *col, QofInstanceForeachCB cb, gpointer data)
{
}

TEST_F(QOFObjectFixture, object_compliance)
{
    EXPECT_TRUE( qof_object_register( m_qofobject ) );

    g_test_message( "Test when neither create nor foreach set" );
    EXPECT_EQ( qof_object_compliance( m_qofobject->e_type, FALSE ), FALSE );
    EXPECT_EQ( qof_object_compliance( m_qofobject->e_type, TRUE ), FALSE );

    g_test_message( "Test when only create set" );
    m_qofobject->create = mock_object_create;
    EXPECT_EQ( qof_object_compliance( m_qofobject->e_type, FALSE ), FALSE );
    EXPECT_EQ( qof_object_compliance( m_qofobject->e_type, TRUE ), FALSE );

    g_test_message( "Test when only foreach set" );
    m_qofobject->create = nullptr;
    m_qofobject->foreach = mock_object_foreach;
    EXPECT_EQ( qof_object_compliance( m_qofobject->e_type, FALSE ), FALSE );
    EXPECT_EQ( qof_object_compliance( m_qofobject->e_type, TRUE ), FALSE );

    g_test_message( "Test when both set" );
    m_qofobject->create = mock_object_create;
    m_qofobject->foreach = mock_object_foreach;
    EXPECT_EQ( qof_object_compliance( m_qofobject->e_type, FALSE ), TRUE );
    EXPECT_EQ( qof_object_compliance( m_qofobject->e_type, TRUE ), TRUE );
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
    EXPECT_TRUE( object );
    EXPECT_TRUE( user_data );
    EXPECT_EQ( object, foreach_type_cb_struct.objects->data );
    EXPECT_EQ( user_data, foreach_type_cb_struct.user_data );
    foreach_type_cb_struct.objects = foreach_type_cb_struct.objects->next;
    foreach_type_cb_struct.call_count++;
}

TEST_F(QOFObjectFixture, object_foreach_type)
{
    gint user_data;
    guint32 list_length;

    g_test_message( "Test with no objects" );
    foreach_type_cb_struct.call_count = 0;
    EXPECT_EQ( g_list_length( get_object_modules() ), 0u );
    qof_object_foreach_type( mock_foreach_type_cb, ( gpointer ) &user_data );
    EXPECT_EQ( foreach_type_cb_struct.call_count, 0u );

    list_length = generate_and_register_objects( 1, EMPTY );

    g_test_message( "Test foreach cb" );
    foreach_type_cb_struct.objects = get_object_modules();
    foreach_type_cb_struct.user_data = ( gpointer ) &user_data;
    foreach_type_cb_struct.call_count = 0;
    qof_object_foreach_type( mock_foreach_type_cb, ( gpointer ) &user_data );
    EXPECT_EQ( foreach_type_cb_struct.call_count, list_length );
    qof_object_foreach_type ((QofForeachTypeCB)g_free, nullptr);
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
    EXPECT_TRUE( col );
    EXPECT_TRUE( cb );
    EXPECT_TRUE( user_data );
    EXPECT_EQ( col, foreach_cb_struct.col );
    EXPECT_EQ( user_data, foreach_cb_struct.user_data );
    EXPECT_EQ( cb, foreach_cb_struct.cb );
    foreach_cb_struct.is_called = TRUE;
}

TEST_F(QOFObjectFixture, object_foreach)
{
    gint user_data;
    QofBook *book = nullptr;
    QofCollection *col = nullptr;

    /* setup */
    book = qof_book_new();
    EXPECT_TRUE( book );
    EXPECT_EQ( g_list_length( get_object_modules() ), 0u );
    qof_object_register( m_qofobject );
    EXPECT_EQ( g_list_length( get_object_modules() ), 1u );
    col = qof_book_get_collection( book, m_qofobject->e_type ); /* make col already exist */
    EXPECT_TRUE( col );

    g_test_message( "Test foreach and data" );
    foreach_cb_struct.user_data = ( gpointer ) &user_data;
    foreach_cb_struct.is_called = FALSE;
    foreach_cb_struct.col = col;
    foreach_cb_struct.cb = mock_instance_foreach_cb;
    m_qofobject->foreach = mock_foreach;
    qof_object_foreach( m_qofobject->e_type, book, mock_instance_foreach_cb, ( gpointer ) &user_data );
    EXPECT_EQ( foreach_cb_struct.is_called, TRUE );

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

    EXPECT_TRUE( col );
    EXPECT_TRUE( cb );
    EXPECT_TRUE( user_data );

    for (iter = foreach_for_sorted_struct.instances; iter; iter = iter->next)
    {
        cb( static_cast<QofInstance*>(iter->data), user_data );
    }
}

static void
mock_instance_foreach_cb_for_sorted( QofInstance *inst, gpointer user_data )
{
    EXPECT_TRUE( inst );
    EXPECT_TRUE( user_data );
    EXPECT_NE( g_list_index( foreach_for_sorted_struct.instances, (gconstpointer) inst ), -1 );
    EXPECT_EQ( user_data, foreach_for_sorted_struct.user_data );
    foreach_for_sorted_struct.call_count++;
}

TEST_F(QOFObjectFixture, object_foreach_sorted)
{
    guint32 list_length = g_test_rand_int_range( 0, 5 );
    gint user_data;
    QofBook *book = nullptr;
    QofCollection *col = nullptr;
    foreach_for_sorted_struct.instances = nullptr;

    /* setup */
    book = qof_book_new();
    EXPECT_TRUE( book );
    EXPECT_EQ( g_list_length( get_object_modules() ), 0u );
    qof_object_register( m_qofobject );
    EXPECT_EQ( g_list_length( get_object_modules() ), 1u );

    m_qofobject->foreach = mock_foreach_for_sorted;
    /* init instances */
    col = qof_book_get_collection( book, m_qofobject->e_type );
    for (size_t i = 0; i < list_length; i++ )
    {
        QofInstance * inst = static_cast<QofInstance*>(g_object_new( QOF_TYPE_INSTANCE, nullptr ));
        EXPECT_TRUE( QOF_IS_INSTANCE( inst ) );
        foreach_for_sorted_struct.instances = g_list_append( foreach_for_sorted_struct.instances, inst );
        qof_collection_insert_entity( col, inst );
    }
    EXPECT_EQ( list_length, g_list_length( foreach_for_sorted_struct.instances ) );

    foreach_for_sorted_struct.call_count = 0;
    foreach_for_sorted_struct.user_data = &user_data;
    qof_object_foreach_sorted( m_qofobject->e_type, book, mock_instance_foreach_cb_for_sorted, ( gpointer ) &user_data );
    EXPECT_EQ( list_length, foreach_for_sorted_struct.call_count );

    qof_book_destroy( book );
    g_list_free_full (foreach_for_sorted_struct.instances, g_object_unref);
}

