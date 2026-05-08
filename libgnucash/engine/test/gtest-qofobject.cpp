/********************************************************************\
 * gtest-qofobject.cpp -- Unit tests for qofobject.cpp              *
 *                                                                  *
 * Copyright 2024 Jules                                             *
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
 *                                                                  *
 \ *********************************************************************/

#include <config.h>
#include <glib.h>
#include "../qof.h"
#include "../qofobject-p.h"
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcpp"
#include <gtest/gtest.h>
#pragma GCC diagnostic pop

class QofObjectTest : public ::testing::Test
{
protected:
    void SetUp() override
    {
        qof_object_initialize();
    }

    void TearDown() override
    {
        qof_object_shutdown();
    }
};

TEST_F(QofObjectTest, RegistrationAndLookup)
{
    static QofObject obj{};
    obj.interface_version = QOF_OBJECT_VERSION;
    obj.e_type = (char*)"test-type";
    obj.type_label = "Test Object";

    EXPECT_TRUE(qof_object_register(&obj));
    EXPECT_EQ(qof_object_lookup("test-type"), &obj);
    EXPECT_STREQ(qof_object_get_type_label("test-type"), "Test Object");

    // Registering again should fail
    EXPECT_FALSE(qof_object_register(&obj));
}

TEST_F(QofObjectTest, NewInstance)
{
    auto mock_create = [](QofBook *book) -> gpointer {
        return (gpointer)0x1234;
    };

    static QofObject obj{};
    obj.interface_version = QOF_OBJECT_VERSION;
    obj.e_type = (char*)"create-type";
    obj.create = mock_create;

    qof_object_register(&obj);

    QofBook *book = qof_book_new();
    EXPECT_EQ(qof_object_new_instance("create-type", book), (gpointer)0x1234);
    qof_book_destroy(book);
}

TEST_F(QofObjectTest, ForeachSorted)
{
    static std::vector<QofInstance*> mock_instances;
    mock_instances.clear();

    auto mock_foreach = [](const QofCollection *col, QofInstanceForeachCB cb, gpointer data)
    {
        for (auto inst : mock_instances)
            cb(inst, data);
    };

    static QofObject obj{};
    obj.interface_version = QOF_OBJECT_VERSION;
    obj.e_type = (char*)"sorted-type";
    obj.foreach = mock_foreach;

    qof_object_register(&obj);

    QofBook *book = qof_book_new();

    // Create some instances with unique GUIDs
    for (int i = 0; i < 3; ++i)
    {
        auto inst = static_cast<QofInstance*>(g_object_new(QOF_TYPE_INSTANCE, nullptr));
        GncGUID guid = guid_new();
        qof_instance_set_guid(inst, &guid);
        mock_instances.push_back(inst);
    }

    static std::vector<QofInstance*> called_instances;
    called_instances.clear();
    auto cb = [](QofInstance* inst, gpointer data) {
        called_instances.push_back(inst);
    };

    qof_object_foreach_sorted("sorted-type", book, cb, nullptr);

    EXPECT_EQ(called_instances.size(), 3);

    // Verify sorted order
    std::vector<QofInstance*> expected_sorted = mock_instances;
    std::sort(expected_sorted.begin(), expected_sorted.end(), [](QofInstance* a, QofInstance* b) {
        return qof_instance_guid_compare(a, b) < 0;
    });

    for (size_t i = 0; i < 3; ++i)
    {
        EXPECT_EQ(called_instances[i], expected_sorted[i]);
    }

    for (auto inst : mock_instances)
        g_object_unref(inst);
    mock_instances.clear();
    qof_book_destroy(book);
}

TEST_F(QofObjectTest, BookLifecycle)
{
    static int begin_calls = 0;
    static int end_calls = 0;
    static QofBook *expected_book = nullptr;

    begin_calls = 0;
    end_calls = 0;
    expected_book = nullptr;

    auto mock_begin = [](QofBook *book) {
        begin_calls++;
        if (expected_book)
        {
            EXPECT_EQ(book, expected_book);
        }
    };
    auto mock_end = [](QofBook *book) {
        end_calls++;
        if (expected_book)
        {
            EXPECT_EQ(book, expected_book);
        }
    };

    static QofObject obj{};
    obj.interface_version = QOF_OBJECT_VERSION;
    obj.e_type = (char*)"lifecycle-type";
    obj.book_begin = mock_begin;
    obj.book_end = mock_end;

    qof_object_register(&obj);

    expected_book = qof_book_new();
    begin_calls = 0; // Reset as register might have called it if book_list was not empty
    qof_object_book_begin(expected_book);
    EXPECT_EQ(begin_calls, 1);

    qof_object_book_end(expected_book);
    EXPECT_EQ(end_calls, 1);

    qof_book_destroy(expected_book);
}

TEST_F(QofObjectTest, Compliance)
{
    static QofObject obj{};
    obj.interface_version = QOF_OBJECT_VERSION;
    obj.e_type = (char*)"compliance-type";

    qof_object_register(&obj);

    // Not compliant yet (missing create and foreach)
    EXPECT_FALSE(qof_object_compliance("compliance-type", FALSE));

    obj.create = [](QofBook*) -> gpointer { return nullptr; };
    EXPECT_FALSE(qof_object_compliance("compliance-type", FALSE));

    obj.foreach = [](const QofCollection*, QofInstanceForeachCB, gpointer) {};
    EXPECT_TRUE(qof_object_compliance("compliance-type", FALSE));
}

TEST_F(QofObjectTest, DirtyAndClean)
{
    static bool is_dirty_val = false;
    static bool mark_clean_called = false;

    is_dirty_val = false;
    mark_clean_called = false;

    auto mock_is_dirty = [](const QofCollection* col) -> gboolean {
        return is_dirty_val ? TRUE : FALSE;
    };
    auto mock_mark_clean = [](QofCollection* col) {
        mark_clean_called = true;
    };

    static QofObject obj{};
    obj.interface_version = QOF_OBJECT_VERSION;
    obj.e_type = (char*)"dirty-type";
    obj.is_dirty = mock_is_dirty;
    obj.mark_clean = mock_mark_clean;

    qof_object_register(&obj);

    QofBook *book = qof_book_new();

    is_dirty_val = false;
    EXPECT_FALSE(qof_object_is_dirty(book));

    is_dirty_val = true;
    EXPECT_TRUE(qof_object_is_dirty(book));

    mark_clean_called = false;
    qof_object_mark_clean(book);
    EXPECT_TRUE(mark_clean_called);

    qof_book_destroy(book);
}

TEST_F(QofObjectTest, ForeachType)
{
    static QofObject obj1{}, obj2{};
    obj1.interface_version = QOF_OBJECT_VERSION;
    obj1.e_type = (char*)"type1";
    obj2.interface_version = QOF_OBJECT_VERSION;
    obj2.e_type = (char*)"type2";

    qof_object_register(&obj1);
    qof_object_register(&obj2);

    static int count = 0;
    count = 0;
    auto cb = [](QofObject *type, gpointer user_data) {
        count++;
    };

    qof_object_foreach_type(cb, nullptr);
    EXPECT_EQ(count, 2);
}

TEST_F(QofObjectTest, Printable)
{
    auto mock_printable = [](gpointer instance) -> const char* {
        return "printed";
    };

    static QofObject obj{};
    obj.interface_version = QOF_OBJECT_VERSION;
    obj.e_type = (char*)"printable-type";
    obj.printable = mock_printable;

    qof_object_register(&obj);

    EXPECT_STREQ(qof_object_printable("printable-type", (gpointer)1), "printed");
}

TEST_F(QofObjectTest, Foreach)
{
    static const QofCollection *expected_col = nullptr;
    static QofInstanceForeachCB expected_cb = nullptr;
    static gpointer expected_data = nullptr;
    static bool foreach_called = false;

    foreach_called = false;
    auto mock_foreach = [](const QofCollection *col, QofInstanceForeachCB cb, gpointer data)
    {
        foreach_called = true;
        EXPECT_EQ(col, expected_col);
        EXPECT_EQ(cb, expected_cb);
        EXPECT_EQ(data, expected_data);
    };

    static QofObject obj{};
    obj.interface_version = QOF_OBJECT_VERSION;
    obj.e_type = (char*)"foreach-type";
    obj.foreach = mock_foreach;

    qof_object_register(&obj);

    QofBook *book = qof_book_new();
    expected_col = qof_book_get_collection(book, "foreach-type");
    expected_cb = [](QofInstance*, gpointer) {};
    expected_data = (gpointer)0x5678;

    qof_object_foreach("foreach-type", book, expected_cb, expected_data);
    EXPECT_TRUE(foreach_called);

    qof_book_destroy(book);
}
