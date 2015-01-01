/********************************************************************
 * test_qofcollection-p.cpp: test suite for qofcollection C++ class.*
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

using namespace gnucash::qof;

static const gchar *suitename = "/qof/qofcollection-class";
static QofIdType type = "test type";

#ifdef __cplusplus
extern "C"
{
#endif

void test_suite_qofcollection_class ( void );

#ifdef __cplusplus
}
#endif

typedef struct
{
    QofCollectionClass *collection;
    QofCollectionClass *collection1;
    /* used for referencing collection created during test */
    QofCollectionClass *collection2;

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
    QofCollectionClass *coll_same0;
    QofCollectionClass *coll_same1;
    QofCollectionClass *coll_diff_instances;
    QofCollectionClass *coll_diff_type;

    QofInstance *instance0;
    QofInstance *instance1;
    QofInstance *instance2;
    QofInstance *instance3;
} CompareFixture;

/* === Helper functions === */

static QofInstance *new_qof_instance (QofIdType type)
{
    QofInstance *inst = NULL;

    inst = static_cast<QofInstance *>(g_object_new (QOF_TYPE_INSTANCE, NULL));
    inst->e_type = type;
    qof_instance_set_a_guid (inst);
    return inst;
}

static void
add_qof_instance_to_collection (QofCollectionClass *col, QofInstance *ent)
{
    gboolean ret = FALSE;

    ret = col->add_entity (ent);
    g_assert_true (ret);
}

/* === end of help functions === */

static void
setup (Fixture *fixture, gconstpointer pData)
{
    QofIdType e_type;

    fixture->collection = new QofCollectionClass (type);
    fixture->collection1 = new QofCollectionClass (type);
    fixture->collection2 = NULL;

    e_type = fixture->collection->get_type ();
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

    delete fixture->collection;
    delete fixture->collection1;
    if (fixture->collection2)
    {
        delete fixture->collection2;
        fixture->collection2 = NULL;
    }
    fixture->collection = NULL;
    fixture->collection1 = NULL;

    g_list_free_full (fixture->instances, g_object_unref);
}


/*
 * Prepare fixture for testing compare_to
 *
 * To create three QofCollectionClass. Two are same and another different one.
 */
static void
setup_for_compare (CompareFixture *fixture, gconstpointer data)
{
    QofIdType e_type;

    fixture->coll_diff_type = new QofCollectionClass ("different type");

    fixture->coll_same0 = new QofCollectionClass (type);
    e_type = fixture->coll_same0->get_type ();

    fixture->instance0 = new_qof_instance (e_type);
    fixture->instance1 = new_qof_instance (e_type);
    fixture->instance2 = new_qof_instance (e_type);
    fixture->instance3 = new_qof_instance (e_type);

    add_qof_instance_to_collection (fixture->coll_same0, fixture->instance0);
    add_qof_instance_to_collection (fixture->coll_same0, fixture->instance2);

    fixture->coll_same1 = new QofCollectionClass (e_type);
    add_qof_instance_to_collection (fixture->coll_same1, fixture->instance0);
    add_qof_instance_to_collection (fixture->coll_same1, fixture->instance2);

    fixture->coll_diff_instances = new QofCollectionClass (e_type);
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

    delete fixture->coll_same0;
    delete fixture->coll_same1;
    delete fixture->coll_diff_instances;
    delete fixture->coll_diff_type;
}

static void
test_qof_collection_new_destroy (void)
{
    QofCollectionClass *col = NULL;

    g_test_message ("Test collection initialization");

    col = new QofCollectionClass (GNC_TEST_TYPE);
    g_assert_nonnull (col);

    g_test_message ("Test collection finalization");
    delete col;
}

static void
test_qof_collection_lookup_entity (Fixture *fixture, gconstpointer pData)
{
    const GncGUID *guid = NULL, *another_guid = NULL;
    QofInstance *inst = NULL;
    QofCollectionClass *col = fixture->collection;

    col->add_entity (fixture->instance0);
    col->add_entity (fixture->instance1);
    col->add_entity (fixture->instance2);

    guid = qof_instance_get_guid (fixture->instance1);
    inst = col->lookup_entity (guid);
    g_assert_nonnull (inst);

    another_guid = qof_instance_get_guid (inst);
    g_assert_true (guid_equal (guid, another_guid));

    another_guid = guid_new ();
    inst = col->lookup_entity (another_guid);
    g_assert_null (inst);
}

static void
test_qof_collection_add_entity (Fixture *fixture, gconstpointer pData)
{
    gboolean ret;
    const GncGUID *guid = NULL;
    QofInstance *instance = NULL;
    guint count = 0;

    ret = fixture->collection->add_entity (fixture->instance0);
    g_assert_true (ret);
    guid = qof_instance_get_guid (fixture->instance0);
    instance = fixture->collection->lookup_entity (guid);
    g_assert_true (fixture->instance0 == instance);

    ret = fixture->collection->add_entity (fixture->instance1);
    g_assert_true (ret);
    guid = qof_instance_get_guid (fixture->instance1);
    instance = fixture->collection->lookup_entity (guid);
    g_assert_true (fixture->instance1 == instance);

    count = fixture->collection->count();
    g_assert_cmpint (count, ==, 2);
}

static void
test_qof_collection_remove_entity (Fixture *fixture, gconstpointer pData)
{
    const GncGUID *guid = NULL;
    QofInstance *inst = NULL;
    QofCollectionClass *col = NULL;

    g_test_message ("Test removing an entity from QofCollectionClass.");

    add_qof_instance_to_collection (fixture->collection, fixture->instance0);
    add_qof_instance_to_collection (fixture->collection, fixture->instance1);

    fixture->collection->remove_entity (fixture->instance0);

    guid = qof_instance_get_guid (fixture->instance0);
    inst = fixture->collection->lookup_entity (guid);
    g_assert_null (inst);
}

static void
test_qof_collection_compare (CompareFixture *fixture, gconstpointer pData)
{
    gint ret = 0;

    ret = fixture->coll_same0->compare_to (NULL);
    g_assert_cmpint (ret, ==, 1);
    ret = fixture->coll_same0->compare_to (fixture->coll_same0);
    g_assert_cmpint (ret, ==, 0);

    g_test_message ("Test two collections that have different types.");
    ret = fixture->coll_same0->compare_to (fixture->coll_diff_type);
    g_assert_cmpint (ret, ==, -1);

    g_test_message ("Test two same collections, with same type and contains same instances.");
    ret = fixture->coll_same0->compare_to (fixture->coll_same1);
    g_assert_cmpint (ret, ==, 0);

    g_test_message ("Test two collections with same type, that have different instances.");
    ret = fixture->coll_same0->compare_to (fixture->coll_diff_instances);
    g_assert_cmpint (ret, ==, 1);

    g_test_message ("Test two collections with same type, and one instance has invalid guid.");
    qof_instance_set_guid (fixture->instance0, guid_null ());
    ret = fixture->coll_same0->compare_to (fixture->coll_same1);
    g_assert_cmpint (ret, ==, -1);
}

static void
test_qof_collection_from_glist (Fixture *fixture, gconstpointer pData)
{
    QofIdType type;
    gpointer data = NULL;
    const GncGUID *guid = NULL;

    g_test_message ("Test creation from an existing instances within a GList.");

    type = fixture->collection->get_type ();
    fixture->collection2 = QofCollectionClass::from_glist (type, fixture->instances);
    g_assert_nonnull (fixture->collection2);
    g_assert_cmpint (fixture->collection2->count (), ==, 5);

    g_test_message ("Test arbitrary instance existance.");

    data = g_list_nth_data (fixture->instances, 3);
    g_assert_nonnull (data);
    guid = qof_instance_get_guid ((QofInstance *)data);
    g_assert_nonnull (fixture->collection2->lookup_entity (guid));
}

static void
test_qof_collection_insert_entity (Fixture *fixture, gconstpointer pData)
{
    const GncGUID *guid = NULL;
    QofCollectionClass *col = fixture->collection;

    g_assert_cmpint (col->count (), ==, 0);

    g_test_message ("Test add a QofInstance that is not added to another QofCollectionClass yet.");

    col->insert_entity (fixture->instance0);
    g_assert_cmpint (col->count (), ==, 1);
    col->insert_entity (fixture->instance1);
    g_assert_cmpint (col->count (), ==, 2);

    g_test_message ("Test add a QofInstance that is already added to another QofCollectionClass.");
    col->insert_entity (fixture->instance3);
    g_assert_cmpint (col->count (), ==, 3);

    g_test_message ("instance3 should be removed from original collection.");
    guid = qof_instance_get_guid (fixture->instance3);
    g_assert_nonnull (fixture->collection1->lookup_entity (guid));
}

void
test_suite_qofcollection_class ( void )
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
