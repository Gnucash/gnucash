/********************************************************************
 * test-qofid.c: GLib g_test test suite for qofobject.c.	    *
 * Copyright 2025 Stefan Koch <stefan.koch.micro@gmail.com>	    *
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
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcpp"
#include <gtest/gtest.h>
#pragma GCC diagnostic pop
#include <glib.h>
#include <algorithm>
#include <vector>

#include "qofid.h"
#include "qofid-p.h"
#include "qofbook.h"
#include "gncJob.h"
#include "qofinstance-p.h"

// This is the error handler that does nothing but count how many times it is
// called.  The handler_count is incremented every time.
static void
test_count_handler (const char *log_domain, GLogLevelFlags log_level,
    const gchar *msg, void* user_data )
{
    auto pint = (int*)user_data;
    *pint += 1;
}

TEST(QofIDTest, collection_new)
{
    auto col = qof_collection_new (QOF_ID_BOOK);
    EXPECT_STREQ(qof_collection_get_type(col), QOF_ID_BOOK);
    EXPECT_EQ(qof_collection_count(col), 0u);

    EXPECT_FALSE(qof_collection_is_dirty(col));
    qof_collection_mark_dirty(col);
    EXPECT_TRUE(qof_collection_is_dirty(col));
    qof_collection_mark_clean(col);
    EXPECT_FALSE(qof_collection_is_dirty(col));

    EXPECT_EQ(qof_collection_get_data(col), nullptr);
    int i = 0;
    gpointer data = &i;
    qof_collection_set_data(col, data);
    EXPECT_EQ(qof_collection_get_data(col), data);
    qof_collection_set_data(col, nullptr);

    qof_collection_destroy(col);
}


TEST(QofIDTest, collection_add)
{
    auto col = qof_collection_new(QOF_ID_BOOK);
    auto book = qof_book_new();
    auto job = gncJobCreate(book);
    gncJobBeginEdit(job);

    EXPECT_FALSE(qof_collection_add_entity(nullptr, nullptr));
    EXPECT_FALSE(qof_collection_add_entity(col, nullptr));
    EXPECT_FALSE(qof_collection_add_entity(nullptr, QOF_INSTANCE(book)));

    EXPECT_TRUE(qof_collection_add_entity(col, QOF_INSTANCE(book)));
    EXPECT_FALSE(qof_collection_add_entity(col, QOF_INSTANCE(book)));

    auto old_guid = qof_instance_get_guid(QOF_INSTANCE(book));
    qof_instance_set_guid(QOF_INSTANCE(book), guid_null());
    EXPECT_FALSE(qof_collection_add_entity(col, QOF_INSTANCE(book)));
    qof_instance_set_guid(QOF_INSTANCE(book), old_guid);


    auto assert_count = 0;
    auto oldlogger = g_log_set_default_handler(test_count_handler, &assert_count);
    EXPECT_FALSE(qof_collection_add_entity(col, QOF_INSTANCE(job)));
    EXPECT_EQ(assert_count, 1);
    g_log_set_default_handler(oldlogger, nullptr);

    gncJobDestroy(job);
    qof_collection_destroy(col);
}

TEST(QofIDTest, collection_compare)
{
    auto target = qof_collection_new(QOF_ID_BOOK);
    auto merge = qof_collection_new(QOF_ID_BOOK);
    auto incompat = qof_collection_new(GNC_ID_JOB);
    auto book1 = qof_book_new();
    auto book2 = qof_book_new();
    auto book3 = qof_book_new();
    auto book4 = qof_book_new();

    // Special cases.
    EXPECT_EQ(0, qof_collection_compare(nullptr, nullptr));
    EXPECT_EQ(0, qof_collection_compare(target, target));
    EXPECT_EQ(-1, qof_collection_compare(nullptr, merge));
    EXPECT_EQ(1, qof_collection_compare(target, nullptr));
    EXPECT_EQ(-1, qof_collection_compare(target, incompat));

    EXPECT_EQ(0, qof_collection_compare(target, merge));
    qof_collection_add_entity(target, QOF_INSTANCE(book1));
    qof_collection_add_entity(merge, QOF_INSTANCE(book1));
    EXPECT_EQ(0, qof_collection_compare(target, merge));

    qof_collection_add_entity(target, QOF_INSTANCE(book2));
    EXPECT_EQ(1, qof_collection_compare(target, merge));
    qof_collection_add_entity(merge, QOF_INSTANCE(book2));
    qof_collection_add_entity(merge, QOF_INSTANCE(book3));
    EXPECT_EQ(1, qof_collection_compare(target, merge));

    auto old_guid = qof_instance_get_guid(QOF_INSTANCE(book3));
    qof_instance_set_guid(QOF_INSTANCE(book3), guid_null());
    EXPECT_EQ(-1, qof_collection_compare(target, merge));
    qof_instance_set_guid(QOF_INSTANCE(book3), old_guid);

    qof_collection_add_entity(target, QOF_INSTANCE(book4));
    old_guid = qof_instance_get_guid(QOF_INSTANCE(book4));
    qof_instance_set_guid(QOF_INSTANCE(book4), guid_null());
    EXPECT_EQ(-1, qof_collection_compare(target, merge));
    qof_instance_set_guid(QOF_INSTANCE(book4), old_guid);

    qof_collection_destroy(target);
    qof_collection_destroy(merge);
    qof_collection_destroy(incompat);
}

static void cb_visit(GncJob* job, std::vector<const char*>* results_ptr)
{
    results_ptr->push_back(gncJobGetID(job));
}

static gint cb_compare(GncJob* job1, GncJob* job2)
{
    return g_strcmp0(gncJobGetID(job1), gncJobGetID(job2));
}

static bool is_in(const char* str, const std::vector<const char*>& str_vect)
{
    return std::find_if(str_vect.begin(), str_vect.end(),
        [str](auto id)->bool{ return !g_strcmp0(id, str); }) != str_vect.end();
}

TEST(QofIDTest, collection_foreach)
{
    auto col = qof_collection_new(GNC_ID_JOB);
    auto book = qof_book_new();
    std::vector<const char*> job_ids{"zzz", "ggg", "qqq", "aaa"};
    std::vector<GncJob*> jobs;
    for(auto id : job_ids)
    {
        auto job = gncJobCreate(book);
        gncJobSetID(job, id);
        jobs.push_back(job);
        qof_collection_add_entity(col, QOF_INSTANCE(job));
    }

    std::vector<const char*> results;
    qof_collection_foreach(col, (QofInstanceForeachCB)&cb_visit, &results);
    EXPECT_EQ(job_ids.size(), results.size());
    for( auto id : job_ids)
        EXPECT_TRUE(is_in(id, results));

    results.clear();
    qof_collection_foreach_sorted(
        col, (QofInstanceForeachCB)&cb_visit, &results, (GCompareFunc)&cb_compare);
    EXPECT_EQ(job_ids.size(), results.size());
    EXPECT_STREQ(results[0], "aaa");
    EXPECT_STREQ(results[1], "ggg");
    EXPECT_STREQ(results[2], "qqq");
    EXPECT_STREQ(results[3], "zzz");

    qof_collection_destroy(col);
}

TEST(QofIDTest, collection_print_dirty)
{
    auto col = qof_collection_new(QOF_ID_BOOK);
    auto book = qof_book_new();
    qof_collection_add_entity(col, QOF_INSTANCE(book));

    testing::internal::CaptureStdout();
    qof_collection_print_dirty(col, nullptr);
    std::string output = testing::internal::GetCapturedStdout();
    EXPECT_STREQ("", output.c_str());


    qof_collection_mark_dirty(col);
    testing::internal::CaptureStdout();
    qof_collection_print_dirty(col, nullptr);
    output = testing::internal::GetCapturedStdout();
    EXPECT_EQ(output.find("Book collection is dirty."), size_t(0));
    qof_collection_mark_clean(col);
    qof_collection_destroy(col);
}
