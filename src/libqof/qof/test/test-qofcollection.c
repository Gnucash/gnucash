/********************************************************************
 * test_qofcollection.c: GLib g_test test suite for qofsession.     *
 * Copyright 2014 Chenxiong Qi <qcxhome@gmail.com>                  *
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
#include <glib.h>
#include <unittest-support.h>

#ifdef __cplusplus
}
#endif

#define GNC_TEST_TYPE "test type"

#include "../qofid.h"
#include "../qofinstance-p.h"

static const gchar *suitename = "/qof/qofcollection";
static QofIdType type = "test type";

void test_suite_qofcollection ( void );

#ifdef __cplusplus
extern "C"
{
#endif


#ifdef __cplusplus
}
#endif

typedef struct
{
    QofCollection *collection;
    QofCollection *collection1;
    /* used for referencing collection created during test */
    QofCollection *collection2;

    QofInstance *instance0;
    QofInstance *instance1;
    QofInstance *instance2;
    QofInstance *instance3;
    QofInstance *instance4;

    /* used for testing qof_collection_from_glist */
    GList *instances;
} Fixture;

typedef struct
{
    QofCollection *coll_same0;
    QofCollection *coll_same1;
    QofCollection *coll_diff_instances;
    QofCollection *coll_diff_type;

    QofInstance *instance0;
    QofInstance *instance1;
    QofInstance *instance2;
    QofInstance *instance3;
} CompareFixture;

/* === Helper functions === */

static QofInstance *new_qof_instance (QofIdType type)
{
    QofInstance *inst = NULL;

    inst = g_object_new (QOF_TYPE_INSTANCE, NULL);
    inst->e_type = type;
    qof_instance_set_a_guid (inst);
    return inst;
}

static void
add_qof_instance_to_collection (QofCollection *col, QofInstance *ent)
{
    gboolean ret = FALSE;

    ret = qof_collection_add_entity (col, ent);
    g_assert_true (ret);

    qof_instance_set_collection (ent, col);
}

/* === end of help functions === */

static void
setup (Fixture *fixture, gconstpointer pData)
{
    QofIdType e_type;

    fixture->collection = qof_collection_new (type);
    fixture->collection1 = qof_collection_new (type);
    fixture->collection2 = NULL;

    e_type = qof_collection_get_type(fixture->collection);
    fixture->instance0 = new_qof_instance (e_type);
    fixture->instance1 = new_qof_instance (e_type);
    fixture->instance2 = new_qof_instance (e_type);
    fixture->instance3 = new_qof_instance (e_type);
    fixture->instance4 = new_qof_instance (e_type);

    add_qof_instance_to_collection (fixture->collection1, fixture->instance3);
    add_qof_instance_to_collection (fixture->collection1, fixture->instance4);

    fixture->instances = g_list_append (fixture->instances, new_qof_instance (e_type));
    fixture->instances = g_list_append (fixture->instances, new_qof_instance (e_type));
    fixture->instances = g_list_append (fixture->instances, new_qof_instance (e_type));
    fixture->instances = g_list_append (fixture->instances, new_qof_instance (e_type));
    fixture->instances = g_list_append (fixture->instances, new_qof_instance (e_type));
}

static void
teardown (Fixture *fixture, gconstpointer pData)
{
    g_object_unref (fixture->instance0);
    g_object_unref (fixture->instance1);
    g_object_unref (fixture->instance2);
    g_object_unref (fixture->instance3);
    g_object_unref (fixture->instance4);

    qof_collection_destroy (fixture->collection);
    qof_collection_destroy (fixture->collection1);
    if (fixture->collection2)
    {
        qof_collection_destroy (fixture->collection2);
        fixture->collection2 = NULL;
    }
    fixture->collection = NULL;
    fixture->collection1 = NULL;

    g_list_free_full (fixture->instances, g_object_unref);
}


/*
 * Prepare fixture for testing qof_collection_compare
 *
 * To create three QofCollections. Two are same and another different one.
 */
static void
setup_for_compare (CompareFixture *fixture, gconstpointer data)
{
    QofIdType e_type;

    fixture->coll_diff_type = qof_collection_new ("different type");

    fixture->coll_same0 = qof_collection_new (type);
    e_type = qof_collection_get_type (fixture->coll_same0);

    fixture->instance0 = new_qof_instance (e_type);
    fixture->instance1 = new_qof_instance (e_type);
    fixture->instance2 = new_qof_instance (e_type);
    fixture->instance3 = new_qof_instance (e_type);

    add_qof_instance_to_collection (fixture->coll_same0, fixture->instance0);
    add_qof_instance_to_collection (fixture->coll_same0, fixture->instance2);

    fixture->coll_same1 = qof_collection_new (e_type);
    add_qof_instance_to_collection (fixture->coll_same1, fixture->instance0);
    add_qof_instance_to_collection (fixture->coll_same1, fixture->instance2);

    fixture->coll_diff_instances = qof_collection_new (e_type);
    add_qof_instance_to_collection (fixture->coll_diff_instances, fixture->instance0);
    add_qof_instance_to_collection (fixture->coll_diff_instances, fixture->instance1);
    add_qof_instance_to_collection (fixture->coll_diff_instances, fixture->instance3);
}

static void
teardown_for_compare (CompareFixture *fixture, gconstpointer data)
{
    g_object_unref (fixture->instance0);
    g_object_unref (fixture->instance1);
    g_object_unref (fixture->instance2);
    g_object_unref (fixture->instance3);

    qof_collection_destroy (fixture->coll_same0);
    qof_collection_destroy (fixture->coll_same1);
    qof_collection_destroy (fixture->coll_diff_instances);
    qof_collection_destroy (fixture->coll_diff_type);
}

static void
test_qof_collection_new_destroy (void)
{
    QofCollection *col = NULL;

    g_test_message ("Test collection initialization");

    col = qof_collection_new (GNC_TEST_TYPE);
    g_assert_nonnull (col);

    g_test_message ("Test collection finalization");
    qof_collection_destroy (col);
}

static void
test_qof_collection_lookup_entity (Fixture *fixture, gconstpointer pData)
{
    const GncGUID *guid = NULL, *another_guid = NULL;
    QofInstance *inst = NULL;

    qof_collection_add_entity (fixture->collection, fixture->instance0);
    qof_collection_add_entity (fixture->collection, fixture->instance1);
    qof_collection_add_entity (fixture->collection, fixture->instance2);

    guid = qof_instance_get_guid (fixture->instance1);
    inst = qof_collection_lookup_entity (fixture->collection, guid);
    g_assert_nonnull (inst);

    another_guid = qof_instance_get_guid (inst);
    g_assert_true (guid_equal (guid, another_guid));

    another_guid = guid_new ();
    inst = qof_collection_lookup_entity (fixture->collection, another_guid);
    g_assert_null (inst);
}

static void
test_qof_collection_add_entity (Fixture *fixture, gconstpointer pData)
{
    gboolean ret;
    const GncGUID *guid = NULL;
    QofInstance *instance = NULL;
    guint count = 0;

    ret = qof_collection_add_entity (fixture->collection, fixture->instance0);
    g_assert_true (ret);
    guid = qof_instance_get_guid (fixture->instance0);
    instance = qof_collection_lookup_entity (fixture->collection, guid);
    g_assert_true (fixture->instance0 == instance);

    ret = qof_collection_add_entity (fixture->collection, fixture->instance1);
    g_assert_true (ret);
    guid = qof_instance_get_guid (fixture->instance1);
    instance = qof_collection_lookup_entity (fixture->collection, guid);
    g_assert_true (fixture->instance1 == instance);

    count = qof_collection_count (fixture->collection);
    g_assert_cmpint (count, ==, 2);
}

static void
test_qof_collection_remove_entity (Fixture *fixture, gconstpointer pData)
{
    const GncGUID *guid = NULL;
    QofInstance *inst = NULL;
    QofCollection *col = NULL;

    g_test_message ("Test removing an entity from QofCollection.");

    add_qof_instance_to_collection (fixture->collection, fixture->instance0);
    add_qof_instance_to_collection (fixture->collection, fixture->instance1);

    col = qof_instance_get_collection (fixture->instance0);
    g_print ("instance' collection should not be NULL");
    g_assert_nonnull (col);

    qof_collection_remove_entity (fixture->instance0);

    guid = qof_instance_get_guid (fixture->instance0);
    inst = qof_collection_lookup_entity (fixture->collection, guid);
    g_assert_null (inst);
}

static void
test_qof_collection_compare (CompareFixture *fixture, gconstpointer pData)
{
    gint ret = 0;

    ret = qof_collection_compare (NULL, NULL);
    g_assert_cmpint (ret, ==, 0);
    ret = qof_collection_compare (NULL, fixture->coll_same0);
    g_assert_cmpint (ret, ==, -1);
    ret = qof_collection_compare (fixture->coll_same0, NULL);
    g_assert_cmpint (ret, ==, 1);
    ret = qof_collection_compare (fixture->coll_same0, fixture->coll_same0);
    g_assert_cmpint (ret, ==, 0);

    g_test_message ("Test two collections that have different types.");
    ret = qof_collection_compare (fixture->coll_same0, fixture->coll_diff_type);
    g_assert_cmpint (ret, ==, -1);

    g_test_message ("Test two same collections, with same type and contains same instances.");
    ret = qof_collection_compare (fixture->coll_same0, fixture->coll_same1);
    g_assert_cmpint (ret, ==, 0);

    g_test_message ("Test two collections with same type, that have different instances.");
    ret = qof_collection_compare (fixture->coll_same0, fixture->coll_same1);
    g_assert_cmpint (ret, ==, 0);

    g_test_message ("Test two collections with same type, and one instance has invalid guid.");
    qof_instance_set_guid (fixture->instance0, guid_null ());
    ret = qof_collection_compare (fixture->coll_same0, fixture->coll_same1);
    g_assert_cmpint (ret, ==, -1);
}

static void
test_qof_collection_from_glist (Fixture *fixture, gconstpointer pData)
{
    QofIdType type;
    gpointer data = NULL;
    const GncGUID *guid = NULL;

    g_test_message ("Test creation from an existing instances within a GList.");

    type = qof_collection_get_type (fixture->collection);
    fixture->collection2 = qof_collection_from_glist (type, fixture->instances);
    g_assert_nonnull (fixture->collection2);
    g_assert_cmpint (qof_collection_count (fixture->collection2), ==, 5);

    g_test_message ("Test arbitrary instance existance.");

    data = g_list_nth_data (fixture->instances, 3);
    g_assert_nonnull (data);
    guid = qof_instance_get_guid ((QofInstance *)data);
    g_assert_nonnull (qof_collection_lookup_entity (fixture->collection2, guid));
}

static void
test_qof_collection_insert_entity (Fixture *fixture, gconstpointer pData)
{
    const GncGUID *guid = NULL;

    g_assert_cmpint (qof_collection_count (fixture->collection), ==, 0);

    g_test_message ("Test add a QofInstance that is not added to another QofCollection yet.");

    qof_collection_insert_entity (fixture->collection, fixture->instance0);
    g_assert_cmpint (qof_collection_count (fixture->collection), ==, 1);
    qof_collection_insert_entity (fixture->collection, fixture->instance1);
    g_assert_cmpint (qof_collection_count (fixture->collection), ==, 2);

    g_test_message ("Test add a QofInstance that is already added to another QofCollection.");
    qof_collection_insert_entity (fixture->collection, fixture->instance3);
    g_assert_cmpint (qof_collection_count (fixture->collection), ==, 3);

    g_test_message ("instance3 should be removed from original collection.");
    guid = qof_instance_get_guid (fixture->instance3);
    g_assert_null (qof_collection_lookup_entity (fixture->collection1, guid));
}

void
test_suite_qofcollection ( void )
{
    GNC_TEST_ADD_FUNC (suitename, "qof collection new and destroy", test_qof_collection_new_destroy);

    GNC_TEST_ADD (suitename, "qof collection add entity", Fixture, NULL,
                  setup, test_qof_collection_add_entity, teardown);
    GNC_TEST_ADD (suitename, "qof collection compare", CompareFixture, NULL,
                  setup_for_compare, test_qof_collection_compare, teardown_for_compare);
    GNC_TEST_ADD (suitename, "qof collection from glist", Fixture, NULL,
                  setup, test_qof_collection_from_glist, teardown);
    GNC_TEST_ADD (suitename, "qof collection inset entity", Fixture, NULL,
                  setup, test_qof_collection_insert_entity, teardown);
    GNC_TEST_ADD (suitename, "qof collection lookup entity", Fixture, NULL,
                  setup, test_qof_collection_lookup_entity, teardown);
    GNC_TEST_ADD (suitename, "qof collection remove entity", Fixture, NULL,
                  setup, test_qof_collection_remove_entity, teardown);
}
