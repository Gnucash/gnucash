/********************************************************************
 * test_qofinstance.c: google test test suite for qofinstance.	    *
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
#include <glib.h>
#include <guid.hpp>

#include <config.h>
#include <unittest-support.h>
#include "../qof.h"
#include "../qof-backend.hpp"
#include "../kvp-frame.hpp"
static gboolean is_called;

static struct
{
    QofInstance *m_inst = nullptr;
    QofBackend *m_be = nullptr;

    bool m_commit_called = false;
    bool m_commit_with_err_called = false;
    bool m_on_error_called = false;
    bool m_on_free_called = false;
    bool m_on_done_called = false;
    QofBackendError m_err = ERR_BACKEND_NO_ERR;
} commit_test;


class QofInstMockBackend : public QofBackend
{
public:
    QofInstMockBackend() : m_qof_error{ERR_BACKEND_NO_ERR} {
        commit_test.m_be = this;
    }
    void session_begin(QofSession* sess, const char* uri,
                       SessionOpenMode mode) override {}
    void session_end() override {}
    void load(QofBook*, QofBackendLoadType) override {}
    void sync(QofBook* book) override {}
    void safe_sync(QofBook* book) override {}
    void begin(QofInstance* inst) override {
        EXPECT_TRUE(inst);
        EXPECT_TRUE(QOF_IS_INSTANCE(inst));
        commit_test.m_inst = inst;
    }
    void set_error(QofBackendError err) {
        QofBackend::set_error(err);
        commit_test.m_err = err;
    }
    void commit(QofInstance* inst) override {
        EXPECT_NE( inst, nullptr );
        EXPECT_TRUE( QOF_IS_INSTANCE( inst ) );
        EXPECT_EQ( commit_test.m_inst, inst );
        EXPECT_EQ( commit_test.m_be, this );
        commit_test.m_commit_called = true;
        if (qof_instance_is_dirty(inst))
            qof_instance_mark_clean(inst);
        set_error(m_qof_error);

    }
    void rollback(QofInstance* inst) override {}
    void inject_error(QofBackendError err) {
        m_qof_error = err;
    }
private:
    QofBackendError m_qof_error;

};

class OqfInstanceTest: public ::testing::Test
{
public:
    OqfInstanceTest() : m_inst(static_cast<QofInstance*>(g_object_new(QOF_TYPE_INSTANCE, nullptr))) {}
    ~OqfInstanceTest()
        {
            g_object_unref(m_inst);
        }
protected:
    QofInstance* m_inst;
};

static void
on_error(QofInstance* inst, QofBackendError err) {
    EXPECT_TRUE( inst );
    EXPECT_TRUE( QOF_IS_INSTANCE( inst ) );
    EXPECT_EQ( commit_test.m_err, err );
    commit_test.m_on_error_called = true;
}
static void
on_done(QofInstance* inst) {
    EXPECT_TRUE( inst );
    EXPECT_TRUE( QOF_IS_INSTANCE( inst ) );
    EXPECT_EQ( commit_test.m_inst, inst );
    commit_test.m_on_done_called = true;
}
static void
on_free(QofInstance* inst) {
    EXPECT_TRUE( inst );
    EXPECT_TRUE( QOF_IS_INSTANCE( inst ) );
    EXPECT_EQ(commit_test.m_inst, inst );
    commit_test.m_on_free_called = true;
}

TEST_F(OqfInstanceTest, set_get_book)
{
    QofBook *book;

    /* set up */
    book = qof_book_new();

    EXPECT_TRUE( QOF_IS_INSTANCE( m_inst ) );

    printf("# Setting and getting book\n");
    qof_instance_set_book( m_inst, book );
    EXPECT_EQ( book, qof_instance_get_book( m_inst ) );

    printf("# Getting book when instance is null\n");
    EXPECT_EQ( qof_instance_get_book( nullptr ), nullptr );

    /* Clean up */
    qof_book_destroy( book );
}

TEST_F(OqfInstanceTest, set_get_guid)
{
    GncGUID *gncGuid;

    /* on null instance deprecated getter returns empty guid
     * while instance_get_guid returns null
     */
    EXPECT_TRUE( !qof_instance_get_guid( nullptr ) );
    EXPECT_EQ( qof_entity_get_guid( nullptr ), guid_null() );

    /* set up */
    gncGuid = guid_new();
    EXPECT_TRUE( QOF_IS_INSTANCE( m_inst ) );
    EXPECT_TRUE( gncGuid );

    /* guid already exists after instance init */
    printf("# Setting new guid\n");
    EXPECT_TRUE( qof_instance_get_guid( m_inst ) );
    EXPECT_TRUE( !guid_equal( gncGuid, qof_instance_get_guid( m_inst ) ) );
    qof_instance_set_guid( m_inst, gncGuid );
    EXPECT_TRUE( guid_equal( gncGuid, qof_instance_get_guid( m_inst ) ) );
    EXPECT_TRUE( guid_equal( gncGuid, qof_entity_get_guid( m_inst ) ) );

    /* Clean up */
    guid_free( gncGuid );
}

TEST_F(OqfInstanceTest, new_destroy)
{
    /* qofinstance var */
    QofInstance *inst;
    QofInstanceClass *klass;
    /* test var */
    Time64 *time_priv;
    const char *msg1 = "qof_instance_get_collection: assertion 'QOF_IS_INSTANCE(ptr)' failed";
    const char *log_domain = "qof";
    auto loglevel = static_cast<GLogLevelFlags>(G_LOG_LEVEL_CRITICAL | G_LOG_FLAG_FATAL);
    auto check = test_error_struct_new(log_domain, loglevel, msg1);

    printf("# Testing qofinstance object initialization\n");
    inst = static_cast<QofInstance*>(g_object_new(QOF_TYPE_INSTANCE, nullptr));
    EXPECT_TRUE( QOF_IS_INSTANCE( inst ) );
    /* test class fields */
    klass = QOF_INSTANCE_GET_CLASS( inst );
    EXPECT_TRUE( QOF_IS_INSTANCE_CLASS( klass ) );
    EXPECT_EQ( klass->get_display_name, nullptr );
    EXPECT_EQ( klass->refers_to_object, nullptr );
    EXPECT_EQ( klass->get_typed_referring_object_list, nullptr );
    /* testing initial values */
    EXPECT_TRUE( qof_instance_get_guid( inst ) );
    EXPECT_TRUE( !qof_instance_get_collection( inst ) );
    EXPECT_EQ( qof_instance_get_book( inst ), nullptr );
    EXPECT_TRUE( inst->kvp_data );
    g_object_get( inst, "last-update", &time_priv, nullptr);
    EXPECT_EQ( time_priv->t, 0 );
    EXPECT_EQ( qof_instance_get_editlevel( inst ), 0 );
    EXPECT_TRUE( !qof_instance_get_destroying( inst ) );
    EXPECT_TRUE( !qof_instance_get_dirty_flag( inst ) );
    EXPECT_TRUE( qof_instance_get_infant( inst ) );
    EXPECT_EQ( qof_instance_get_version( inst ), 0 );
    EXPECT_EQ( qof_instance_get_version_check( inst ), 0u );
    EXPECT_EQ( qof_instance_get_idata( inst ), 0u );

    printf("# Testing object destruction\n");
    g_object_unref( inst );
    test_error_struct_free(check);
}

TEST_F(OqfInstanceTest, init_data)
{
    QofInstance *inst;
    QofIdType test_type = "test type";
    QofBook *book;
    QofCollection *col;
    const GncGUID *gncguid;
    char guid_id_before[GUID_ENCODING_LENGTH + 1];
    char guid_id_after[GUID_ENCODING_LENGTH + 1];

    /* set up */
    inst = static_cast<QofInstance*>(g_object_new( QOF_TYPE_INSTANCE, nullptr ));
    EXPECT_TRUE( QOF_IS_INSTANCE( inst ) );
    book = qof_book_new();
    EXPECT_TRUE( QOF_IS_BOOK( book ) );

    printf("# Running test with correct initial data\n");
    gncguid = qof_instance_get_guid( inst );
    EXPECT_TRUE( gncguid );
    guid_to_string_buff( gncguid, guid_id_before );
    EXPECT_EQ( qof_instance_get_book( inst ), nullptr );
    EXPECT_EQ( qof_instance_get_collection( inst ), nullptr );
    /* run init */
    qof_instance_init_data( inst, test_type, book );

    EXPECT_EQ( qof_instance_get_book( inst ), book );
    guid_to_string_buff( gncguid, guid_id_after );
    EXPECT_STRNE( guid_id_before, guid_id_after );
    EXPECT_TRUE( qof_instance_get_collection( inst ) != nullptr );
    col = qof_book_get_collection( book, test_type );
    EXPECT_TRUE( col );
    EXPECT_EQ( col, qof_instance_get_collection( inst ) );
    EXPECT_STREQ( inst->e_type, test_type );
    EXPECT_EQ( qof_collection_lookup_entity( qof_instance_get_collection( inst ), gncguid ), inst );

    /* clean up */
    g_object_unref( inst );
    qof_book_destroy( book );
}

TEST_F(OqfInstanceTest, get_set_slots)
{
    KvpFrame *kvp_frame, *kvp_frame2;

    /* set up */
    EXPECT_TRUE( m_inst );
    kvp_frame = qof_instance_get_slots( m_inst );
    EXPECT_TRUE( kvp_frame );

    printf("# Test when kvp frame is the same\n");
    qof_instance_set_slots( m_inst, kvp_frame );
    EXPECT_EQ( kvp_frame, qof_instance_get_slots( m_inst ) );
    EXPECT_TRUE( qof_instance_get_dirty_flag( m_inst ) );

    printf("# Test when kvp frame is not the same\n");
    kvp_frame2 = new KvpFrame;
    EXPECT_TRUE( kvp_frame != kvp_frame2 );
    qof_instance_set_slots( m_inst, kvp_frame2 );
    EXPECT_EQ( kvp_frame2, qof_instance_get_slots( m_inst ) );
    EXPECT_TRUE( qof_instance_get_dirty_flag( m_inst ) );

    printf("# Test when kvp frame is null\n");
    qof_instance_set_slots( m_inst, nullptr );
    EXPECT_EQ( nullptr, qof_instance_get_slots( m_inst ) );
    EXPECT_TRUE( qof_instance_get_dirty_flag( m_inst ) );

}

TEST_F(OqfInstanceTest, version_cmp)
{
    QofInstance *left, *right;
    int result;
    time64 time_left = 0, time_right = 1;

    /* set up*/
    left = static_cast<QofInstance*>(g_object_new( QOF_TYPE_INSTANCE, nullptr ));
    right = static_cast<QofInstance*>(g_object_new( QOF_TYPE_INSTANCE, nullptr ));

    printf("# Test both null\n");
    result = qof_instance_version_cmp( nullptr, nullptr );
    EXPECT_EQ( result, 0 );

    printf("# Test left null\n");
    result = qof_instance_version_cmp( nullptr, right );
    EXPECT_EQ( result, -1 );

    printf("# Test right null\n");
    result = qof_instance_version_cmp( left, nullptr );
    EXPECT_EQ( result, 1 );

    printf("# Test left tv_sec lesser than right\n");
    qof_instance_set_last_update( left, time_left );
    qof_instance_set_last_update( right, time_right );
    result = qof_instance_version_cmp( left, right );
    EXPECT_EQ( result, -1 );

    printf("# Test right tv_sec lesser than left\n");
    time_left = 1;
    time_right = 0;
    qof_instance_set_last_update( left, time_left );
    qof_instance_set_last_update( right, time_right );
    result = qof_instance_version_cmp( left, right );
    EXPECT_EQ( result, 1 );

    printf("# Test both equal\n");
    time_left = 1;
    time_right = 1;
    qof_instance_set_last_update( left, time_left );
    qof_instance_set_last_update( right, time_right );
    result = qof_instance_version_cmp( left, right );
    EXPECT_EQ( result, 0 );

    /* clear */
    g_object_unref( left );
    g_object_unref( right );
}

TEST_F(OqfInstanceTest, get_set_dirty)
{
    QofIdType type = "test type";
    QofCollection *col;

    /* setup */
    col = qof_collection_new ( type );
    qof_instance_set_collection( m_inst, col );
    EXPECT_TRUE( qof_instance_get_collection( m_inst ) );

    printf("# Test get_dirty on empty instance returns false\n");
    EXPECT_FALSE( qof_instance_get_dirty( nullptr ));

    printf("# Test dirty\n");
    EXPECT_TRUE( !qof_instance_get_dirty_flag( m_inst ) );
    EXPECT_TRUE( !qof_collection_is_dirty( col ) );
    EXPECT_TRUE( !qof_instance_get_dirty( m_inst ) );
    qof_instance_set_dirty( m_inst );
    EXPECT_TRUE( qof_instance_get_dirty_flag( m_inst ) );
    EXPECT_TRUE( !qof_collection_is_dirty( col ) );
    EXPECT_TRUE( qof_instance_get_dirty( m_inst ) );

    /* clean up */
    qof_instance_set_collection( m_inst, nullptr );
    qof_collection_destroy( col );
}

/* mock display name function */
static gchar*
mock_get_display_name(const QofInstance* inst)
{
    gchar *display_name;

    EXPECT_TRUE( inst );
    EXPECT_EQ( QOF_INSTANCE_GET_CLASS( inst )->get_display_name, (gpointer)mock_get_display_name );
    is_called = TRUE;
    display_name = g_strdup_printf("Mock display name %p", inst );
    return display_name;
}

TEST_F(OqfInstanceTest, display_name)
{
    QofIdType type = "test type";
    QofCollection *col;
    gchar *display_name, *default_display_name, *mock_display_name;

    /* setup */
    EXPECT_TRUE( m_inst );
    is_called = FALSE;
    col = qof_collection_new ( type );
    EXPECT_TRUE( col );
    qof_instance_set_collection( m_inst, col );
    EXPECT_TRUE( qof_instance_get_collection( m_inst ) );
    default_display_name = g_strdup_printf( "Object %s %p", type, m_inst );
    mock_display_name = g_strdup_printf( "Mock display name %p", m_inst );

    printf("# Test instance when display name not set\n");
    EXPECT_EQ( QOF_INSTANCE_GET_CLASS( m_inst )->get_display_name, nullptr );
    display_name = qof_instance_get_display_name( m_inst );
    EXPECT_TRUE( !is_called );
    EXPECT_STREQ( display_name, default_display_name );
    g_free( display_name );

    printf("# Test instance when display name is set\n");
    QOF_INSTANCE_GET_CLASS( m_inst )->get_display_name = mock_get_display_name;
    display_name = qof_instance_get_display_name( m_inst );
    EXPECT_TRUE( is_called );
    EXPECT_STREQ( display_name, mock_display_name );
    g_free( display_name );

    /* clean up */
    g_free( default_display_name );
    g_free( mock_display_name );
    qof_instance_set_collection( m_inst, nullptr );
    qof_collection_destroy( col );
}

TEST_F(OqfInstanceTest, begin_edit)
{
    QofBook *book;
    gboolean result;

    /* setup */
    auto be = new QofInstMockBackend;
    EXPECT_TRUE( be );
    book = qof_book_new();
    EXPECT_TRUE( book );
    EXPECT_TRUE( QOF_IS_BOOK( book ) );
    qof_book_set_backend( book, be );
    EXPECT_TRUE( m_inst );
    m_inst->e_type = "test type";
    EXPECT_FALSE( qof_instance_get_dirty_flag( m_inst ) );
    EXPECT_EQ( qof_instance_get_editlevel( m_inst ), 0 );

    printf("# Test when instance is null\n");
    result = qof_begin_edit( nullptr );
    EXPECT_FALSE( result );

    printf("# Test when instance's editlevel is >= 1\n");
    qof_instance_increase_editlevel( m_inst );
    result = qof_begin_edit( m_inst );
    EXPECT_FALSE( result );
    EXPECT_EQ( qof_instance_get_editlevel( m_inst ), 2 );

    printf("# Test when instance's editlevel is <= 0 and backend not set\n");
    qof_instance_reset_editlevel( m_inst );
    result = qof_begin_edit( m_inst );
    EXPECT_TRUE( result );
    EXPECT_EQ( qof_instance_get_editlevel( m_inst ), 1 );
    EXPECT_TRUE( qof_instance_get_dirty_flag( m_inst ));

    printf("# Test when instance's editlevel is <= 0 and backend is set\n");
    result = FALSE;
    qof_instance_reset_editlevel( m_inst );
    qof_instance_set_dirty_flag( m_inst, FALSE );
    qof_instance_set_book( m_inst, book );
    result = qof_begin_edit( m_inst );
    EXPECT_TRUE( result );
    EXPECT_EQ( qof_instance_get_editlevel( m_inst ), 1 );
    EXPECT_FALSE( qof_instance_get_dirty_flag( m_inst ) );

    /* clean up */
    qof_book_set_backend( book, nullptr );
    qof_book_destroy( book );
    delete be;

}

TEST_F(OqfInstanceTest, commit_edit)
{
    gboolean result;
    const gchar *msg = "[qof_commit_edit()] unbalanced call - resetting (was -2)";
    const gchar *log_domain = "qof.engine";
    auto loglevel = G_LOG_LEVEL_CRITICAL;
    auto check = test_error_struct_new(log_domain, loglevel, msg);

    printf("# Test when instance set to null\n");
    result = qof_commit_edit( nullptr );
    EXPECT_TRUE( !result );

    printf("# Test when instance's editlevel >= 2\n");
    qof_instance_increase_editlevel( m_inst );
    qof_instance_increase_editlevel( m_inst );
    EXPECT_EQ( qof_instance_get_editlevel( m_inst ), 2 );
    result = qof_commit_edit( m_inst );
    EXPECT_EQ( qof_instance_get_editlevel( m_inst ), 1 );
    EXPECT_TRUE( !result );

    printf("# Test when instance's editlevel = 1\n");
    result = qof_commit_edit( m_inst );
    EXPECT_EQ( qof_instance_get_editlevel( m_inst ), 0 );
    EXPECT_TRUE( result );

    printf("# Test when instance's editlevel < 0\n");
    auto hdlr = g_log_set_handler (log_domain, loglevel,
                                       (GLogFunc)test_checked_handler, check);
    qof_instance_decrease_editlevel( m_inst );
    EXPECT_EQ( qof_instance_get_editlevel( m_inst ), -1 );
    result = qof_commit_edit( m_inst );
    EXPECT_EQ( qof_instance_get_editlevel( m_inst ), 0 );
    EXPECT_EQ(check->hits, 1u);
    g_log_remove_handler (log_domain, hdlr);
    test_error_struct_free(check);
}

/* backend commit test start */


TEST_F(OqfInstanceTest, commit_edit_part2)
{
    QofBook *book;
    gboolean result;

    /* setup */
    auto be = new QofInstMockBackend;
    EXPECT_TRUE( be );
    book = qof_book_new();
    EXPECT_TRUE( book );
    EXPECT_TRUE( QOF_IS_BOOK( book ) );
    qof_book_set_backend( book, be );

    /* init */
    result = FALSE;
    commit_test.m_commit_called = false;
    commit_test.m_commit_with_err_called = false;
    commit_test.m_on_error_called = false;
    commit_test.m_on_free_called = false;
    commit_test.m_on_done_called = false;
    commit_test.m_inst = m_inst;
    commit_test.m_be = be;
    qof_instance_set_dirty_flag( m_inst, TRUE );

    printf("# Test when instance's backend not set, callbacks not set\n");
    EXPECT_TRUE( qof_instance_get_infant( m_inst ) );
    EXPECT_TRUE( !qof_instance_get_destroying( m_inst ) );
    result = qof_commit_edit_part2( m_inst, nullptr, nullptr, nullptr );
    EXPECT_TRUE( result );
    EXPECT_TRUE( qof_instance_get_dirty_flag( m_inst ) );
    EXPECT_TRUE( qof_instance_get_infant( m_inst ) );
    EXPECT_TRUE( !commit_test.m_commit_called );
    EXPECT_TRUE( !commit_test.m_commit_with_err_called );
    EXPECT_TRUE( !commit_test.m_on_error_called );
    EXPECT_TRUE( !commit_test.m_on_free_called );
    EXPECT_TRUE( !commit_test.m_on_done_called );

    printf("# Test when instance's backend not set, do_free is false\n");
    qof_instance_set_destroying( m_inst, TRUE );
    result = qof_commit_edit_part2( m_inst, on_error, on_done, on_free );
    EXPECT_TRUE( result );
    EXPECT_TRUE( qof_instance_get_dirty_flag( m_inst ) );
    EXPECT_TRUE( qof_instance_get_infant( m_inst ) );
    EXPECT_TRUE( !commit_test.m_commit_called );
    EXPECT_TRUE( !commit_test.m_commit_with_err_called );
    EXPECT_TRUE( !commit_test.m_on_error_called );
    EXPECT_TRUE( commit_test.m_on_free_called );
    EXPECT_TRUE( !commit_test.m_on_done_called );

    printf("# Test when instance's backend not set, do_free is false\n");
    qof_instance_set_destroying( m_inst, FALSE );
    commit_test.m_on_free_called = false;
    result = qof_commit_edit_part2( m_inst, on_error, on_done, on_free );
    EXPECT_TRUE( result );
    EXPECT_TRUE( qof_instance_get_dirty_flag( m_inst ) );
    EXPECT_TRUE( qof_instance_get_infant( m_inst ) );
    EXPECT_TRUE( !commit_test.m_commit_called );
    EXPECT_TRUE( !commit_test.m_commit_with_err_called );
    EXPECT_TRUE( !commit_test.m_on_error_called );
    EXPECT_TRUE( !commit_test.m_on_free_called );
    EXPECT_TRUE( commit_test.m_on_done_called );

    printf("# Test when instance's backend is set, all cb set, no error produced\n");
    qof_instance_set_book( m_inst, book );
    qof_instance_set_destroying( m_inst, FALSE );
    commit_test.m_on_done_called = false;
    result = qof_commit_edit_part2( m_inst, on_error, on_done, on_free );
    EXPECT_TRUE( result );
    EXPECT_TRUE( !qof_instance_get_dirty_flag( m_inst ) );
    EXPECT_TRUE( !qof_instance_get_infant( m_inst ) );
    EXPECT_TRUE( commit_test.m_commit_called );
    EXPECT_TRUE( !commit_test.m_commit_with_err_called );
    EXPECT_TRUE( !commit_test.m_on_error_called );
    EXPECT_TRUE( !commit_test.m_on_free_called );
    EXPECT_TRUE( commit_test.m_on_done_called );

    printf("# Test when instance's backend is set, all cb set, error produced\n");
    commit_test.m_commit_called = false;
    commit_test.m_on_done_called = false;
    be->inject_error(ERR_BACKEND_NO_HANDLER);
    qof_instance_set_dirty_flag( m_inst, TRUE );
    qof_instance_set_destroying( m_inst, TRUE );
    result = qof_commit_edit_part2( m_inst, on_error, on_done, on_free );
    EXPECT_TRUE( !result );
    EXPECT_TRUE( !qof_instance_get_dirty_flag( m_inst ) );
    EXPECT_TRUE( !qof_instance_get_destroying( m_inst ) );
    EXPECT_TRUE( commit_test.m_commit_called );
    EXPECT_TRUE( commit_test.m_on_error_called );
    EXPECT_TRUE( !commit_test.m_on_free_called );
    EXPECT_TRUE( !commit_test.m_on_done_called );

    /* clean up */
    qof_book_set_backend( book, nullptr );
    qof_book_destroy( book );
    delete be;
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
    EXPECT_TRUE( inst );
    EXPECT_TRUE( ref );
    EXPECT_EQ( refers_test_struct.inst, inst );
    EXPECT_EQ( refers_test_struct.ref, ref );
    refers_test_struct.refers_to_object_called = TRUE;
    return TRUE;
}

TEST_F(OqfInstanceTest, refers_to_object)
{
    QofInstance * ref;

    ref = static_cast<QofInstance*>(g_object_new( QOF_TYPE_INSTANCE, nullptr ));
    EXPECT_TRUE( m_inst );
    EXPECT_TRUE( ref );
    EXPECT_EQ( QOF_INSTANCE_GET_CLASS( m_inst )->refers_to_object, nullptr );
    refers_test_struct.refers_to_object_called = FALSE;
    refers_test_struct.inst = m_inst;
    refers_test_struct.ref = ref;

    printf("# Test when refers to object not set\n");
    EXPECT_TRUE( !qof_instance_refers_to_object( m_inst, ref ) );
    EXPECT_TRUE( !refers_test_struct.refers_to_object_called );

    printf("# Test when refers to object set\n");
    QOF_INSTANCE_GET_CLASS( m_inst )->refers_to_object = mock_refers_to_object;
    EXPECT_TRUE( qof_instance_refers_to_object( m_inst, ref ) );
    EXPECT_TRUE( refers_test_struct.refers_to_object_called );

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
    EXPECT_TRUE( inst );
    EXPECT_TRUE( ref );
    EXPECT_TRUE( g_list_find( refers_test_struct_from_col.list, inst ) );
    EXPECT_EQ( refers_test_struct_from_col.ref, ref );
    refers_test_struct_from_col.call_count++;
    refers_test_struct.refers_to_object_called = TRUE;
    return TRUE;
}

TEST_F(OqfInstanceTest, get_referring_object_list_from_collection)
{
    QofIdType type = "test type";
    QofBook *book;
    GList *inst_list = nullptr;
    GList *result = nullptr;
    QofCollection *coll;
    QofInstance *ref;
    guint32 list_length = 5;

    /* setup book and ref instance */
    book = qof_book_new();
    EXPECT_TRUE( book );
    EXPECT_TRUE( QOF_IS_BOOK( book ) );
    ref =  static_cast<QofInstance*>(g_object_new( QOF_TYPE_INSTANCE, nullptr ));
    EXPECT_TRUE( ref );
    EXPECT_TRUE( QOF_IS_INSTANCE( ref ) );
    QOF_INSTANCE_GET_CLASS( ref )->refers_to_object = nullptr;
    refers_test_struct_from_col.call_count = 0;
    /* init list of entities of one type,
     * put them into book collection and
     * save in the list
     */
    EXPECT_EQ( 0u, g_list_length( inst_list ) );
    for (auto i = 0u; i < list_length; i++ )
    {
        auto inst = static_cast<QofInstance*>(g_object_new( QOF_TYPE_INSTANCE, nullptr ));
        EXPECT_TRUE( inst );
        qof_instance_init_data( inst, type, book );
        inst_list = g_list_append ( inst_list, inst );
        EXPECT_EQ( g_list_length( inst_list ), (i + 1) );
    }
    EXPECT_EQ( list_length, g_list_length( inst_list ) );

    printf("# Test when refers to object not set\n");
    coll = qof_book_get_collection( book, type );
    EXPECT_TRUE( coll );
    result = qof_instance_get_referring_object_list_from_collection( coll, ref );
    EXPECT_TRUE( !result );
    EXPECT_EQ( refers_test_struct_from_col.call_count, 0u );

    printf("# Test when refers to object is set\n");
    QOF_INSTANCE_GET_CLASS( ref )->refers_to_object = mock_refers_to_object_from_col;
    refers_test_struct_from_col.list = inst_list;
    refers_test_struct_from_col.ref = ref;
    result = qof_instance_get_referring_object_list_from_collection( coll, ref );
    if ( list_length == 0 )
        EXPECT_TRUE( !result );
    else
        EXPECT_TRUE( result );
    EXPECT_EQ( g_list_length( inst_list ), g_list_length( result ) );
    EXPECT_EQ( g_list_length( inst_list ), refers_test_struct_from_col.call_count );

    /* clean up list and destroy book */
    g_list_foreach( inst_list, (GFunc) g_object_unref, nullptr );
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
    GList* result = nullptr;

    EXPECT_TRUE( inst );
    EXPECT_TRUE( ref );
    EXPECT_EQ( get_typed_referring_object_list_struct.inst, inst );
    EXPECT_EQ( get_typed_referring_object_list_struct.ref, ref );
    get_typed_referring_object_list_struct.get_typed_referring_object_list_called = TRUE;
    return g_list_append( result, (gpointer) inst );
}

TEST_F(OqfInstanceTest, get_typed_referring_object_list)
{
    QofInstance *inst;
    QofInstance *ref;
    QofBook *book;
    GList* result = nullptr;

    /* setup */
    inst = static_cast<QofInstance*>(g_object_new( QOF_TYPE_INSTANCE, nullptr ));
    ref = static_cast<QofInstance*>(g_object_new( QOF_TYPE_INSTANCE, nullptr ));
    book = qof_book_new();
    EXPECT_TRUE( inst );
    EXPECT_TRUE( ref );
    EXPECT_TRUE( book );
    QOF_INSTANCE_GET_CLASS( inst )->refers_to_object = nullptr;
    QOF_INSTANCE_GET_CLASS( inst )->get_typed_referring_object_list = nullptr;
    qof_instance_init_data( inst, "test type", book );
    get_typed_referring_object_list_struct.get_typed_referring_object_list_called = FALSE;

    /*
     * cases when refers to object is set are not tested in current function
     * as they are checked in the previous tests
     */
    printf("# Test when get typed referring object list is not set\n");
    result = qof_instance_get_typed_referring_object_list( inst, ref );
    EXPECT_TRUE( !result );
    EXPECT_TRUE( !get_typed_referring_object_list_struct.get_typed_referring_object_list_called );
    g_list_free( result );

    printf("# Test when get typed referring object list is set\n");
    QOF_INSTANCE_GET_CLASS( inst )->get_typed_referring_object_list = mock_get_typed_referring_object_list;
    get_typed_referring_object_list_struct.inst = inst;
    get_typed_referring_object_list_struct.ref = ref;
    result = qof_instance_get_typed_referring_object_list( inst, ref );
    EXPECT_TRUE( result );
    EXPECT_EQ( g_list_length( result ), 1u );
    EXPECT_TRUE( get_typed_referring_object_list_struct.get_typed_referring_object_list_called );
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
    EXPECT_TRUE( inst );
    EXPECT_TRUE( ref );
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
    EXPECT_TRUE( inst );
    EXPECT_TRUE( ref );
    get_referring_object_list_struct.get_typed_referring_object_list_count++;
    return qof_instance_get_referring_object_list_from_collection(qof_instance_get_collection(inst), ref);
}

TEST_F(OqfInstanceTest, get_referring_object_list)
{
    /* walk through the book's each collection's each instance */
    QofInstance *ref1;
    QofInstance *ref2;
    QofBook *book;
    QofIdType type1 = "type1";
    QofIdType type2 = "type2";
    guint32 col1_length = g_test_rand_int_range( 0, 10 );
    guint32 col2_length = g_test_rand_int_range( 0, 10 );
    GList* inst_list1 = nullptr;
    GList* inst_list2 = nullptr;
    GList* result = nullptr;

    /* setup */
    book = qof_book_new();
    EXPECT_TRUE( book );
    EXPECT_TRUE( QOF_IS_BOOK( book ) );
    ref1 = static_cast<QofInstance*>(g_object_new( QOF_TYPE_INSTANCE, nullptr ));
    EXPECT_TRUE( ref1 );
    ref2 = static_cast<QofInstance*>(g_object_new( QOF_TYPE_INSTANCE, nullptr ));
    EXPECT_TRUE( ref2 );
    qof_instance_init_data( ref1, type1, book );
    qof_instance_init_data( ref2, type2, book );
    QOF_INSTANCE_GET_CLASS( ref1 )->refers_to_object = nullptr;
    QOF_INSTANCE_GET_CLASS( ref1 )->get_typed_referring_object_list = nullptr;
    EXPECT_EQ( qof_collection_count( qof_book_get_collection( book, type1 ) ), 1u );
    EXPECT_EQ( qof_collection_count( qof_book_get_collection( book, type2 ) ), 1u );
    get_referring_object_list_struct.refers_to_object_call_count = 0;
    get_referring_object_list_struct.get_typed_referring_object_list_count = 0;
    /*
     * fill two collections with different types
     * and random number of elements
     */
    for (auto i = 0u; i < col1_length; i++ )
    {
        auto inst = static_cast<QofInstance*>(g_object_new( QOF_TYPE_INSTANCE, nullptr ));
        EXPECT_TRUE( inst );
        qof_instance_init_data( inst, type1, book );
        inst_list1 = g_list_append ( inst_list1, inst );
        EXPECT_EQ( g_list_length( inst_list1 ), (i + 1) );
    }
    EXPECT_EQ( qof_collection_count( qof_book_get_collection( book, type1 ) ), col1_length + 1 );
    EXPECT_EQ( g_list_length( inst_list1 ), col1_length );

    for (auto j = 0u; j < col2_length; j++ )
    {
        auto inst = static_cast<QofInstance*>(g_object_new( QOF_TYPE_INSTANCE, nullptr ));
        EXPECT_TRUE( inst );
        qof_instance_init_data( inst, type2, book );
        inst_list2 = g_list_append ( inst_list2, inst );
        EXPECT_EQ( g_list_length( inst_list2 ), (j + 1) );
    }
    EXPECT_EQ( qof_collection_count( qof_book_get_collection( book, type2 ) ), col2_length + 1 );
    EXPECT_EQ( g_list_length( inst_list2 ), col2_length );

    printf("# Test object list returned for ref1 instance by default\n");
    result = qof_instance_get_referring_object_list( ref1 );
    EXPECT_TRUE( !result );
    EXPECT_EQ( get_referring_object_list_struct.refers_to_object_call_count, 0u );

    printf("# Test object list returned for ref2 instance by default\n");
    result = qof_instance_get_referring_object_list( ref2 );
    EXPECT_TRUE( !result );
    EXPECT_EQ( get_referring_object_list_struct.refers_to_object_call_count, 0u );

    /*
     * refers to object is made simple as it is tested enough
     * it checks if instance types are equal
     * that is for ref1 we should get all elements from collection with type1
     * for ref2 we should get all elements from collection with type2
     */
    printf("# Test object list returned for ref1 instance when refers_to_object is set\n");
    QOF_INSTANCE_GET_CLASS( ref1 )->refers_to_object = mock_simple_refers_to_object;
    result = qof_instance_get_referring_object_list( ref1 );
    EXPECT_TRUE( result );
    EXPECT_EQ( g_list_length( result ), col1_length + 1 );
    EXPECT_EQ( get_referring_object_list_struct.refers_to_object_call_count, col1_length + 1 );
    g_list_free( result );

    printf("# Test object list returned for ref2 instance when refers_to_object is set\n");
    get_referring_object_list_struct.refers_to_object_call_count = 0;
    result = qof_instance_get_referring_object_list( ref2 );
    EXPECT_TRUE( result );
    EXPECT_EQ( g_list_length( result ), col2_length + 1 );
    EXPECT_EQ( get_referring_object_list_struct.refers_to_object_call_count, col2_length + 1 );
    g_list_free( result );

    printf("# Test object list returned for ref1 instance when refers_to_object is set and get typed set\n");
    QOF_INSTANCE_GET_CLASS( ref1 )->get_typed_referring_object_list = mock_simple_get_typed_referring_object_list;
    get_referring_object_list_struct.refers_to_object_call_count = 0;
    get_referring_object_list_struct.get_typed_referring_object_list_count = 0;
    result = qof_instance_get_referring_object_list( ref1 );
    EXPECT_TRUE( result );
    EXPECT_EQ( g_list_length( result ), col1_length + 1 );
    EXPECT_EQ( get_referring_object_list_struct.refers_to_object_call_count, col1_length + 1 );
    EXPECT_EQ( get_referring_object_list_struct.get_typed_referring_object_list_count, 2u );
    g_list_free( result );

    printf("# Test object list returned for ref2 instance when refers_to_object is set and get typed set\n");
    get_referring_object_list_struct.refers_to_object_call_count = 0;
    get_referring_object_list_struct.get_typed_referring_object_list_count = 0;
    result = qof_instance_get_referring_object_list( ref2 );
    EXPECT_TRUE( result );
    EXPECT_EQ( g_list_length( result ), col2_length + 1 );
    EXPECT_EQ( get_referring_object_list_struct.refers_to_object_call_count, col2_length + 1 );
    EXPECT_EQ( get_referring_object_list_struct.get_typed_referring_object_list_count, 2u );
    g_list_free( result );

    /* clean */
    g_object_unref( ref1 );
    g_object_unref( ref2 );
    g_list_foreach( inst_list1, (GFunc) g_object_unref, nullptr );
    g_list_foreach( inst_list2, (GFunc) g_object_unref, nullptr );
    g_list_free( inst_list1 );
    g_list_free( inst_list2 );
    qof_book_destroy( book );
}

int main(int argc, char **argv) {
    testing::InitGoogleTest(&argc, argv);

    setlocale (LC_ALL, "");
    qof_init(); 			/* Initialize the GObject system */
    qof_log_init_filename_special("stderr"); /* Init the log system */

    return RUN_ALL_TESTS();
}
