/********************************************************************
 * gtest-qoflog.cpp -- unit tests for qof_log_foreach_line          *
 *                                                                  *
 * Copyright 2026 GnuCash contributors                              *
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
 * along with this program.  If not, see                            *
 * <https://www.gnu.org/licenses/>.                                 *
 *******************************************************************/

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcpp"
#include <gtest/gtest.h>
#pragma GCC diagnostic pop

#include <config.h>
#include <glib.h>
#include <glib/gstdio.h>
#include <string>
#include <vector>
#include "qoflog.h"

static QofLogModule log_module = "gnc.test.qoflog";

using Lines = std::vector<std::string>;

static gboolean
collect_line (const char *line, gpointer user_data)
{
    static_cast<Lines*> (user_data)->emplace_back (line);
    return TRUE;
}

static bool
contains (const Lines& lines, const char *needle)
{
    for (const auto& l : lines)
        if (l.find (needle) != std::string::npos)
            return true;
    return false;
}

/* A logged line must be readable back through qof_log_foreach_line(). */
TEST(qoflog, foreach_line_returns_logged_lines)
{
    gchar *path = g_build_filename (g_get_tmp_dir (), "gnc-qoflog-test-lines", nullptr);
    qof_log_init ();
    qof_log_init_filename (path);
    qof_log_set_level (log_module, QOF_LOG_DEBUG);

    PWARN ("tracelog-marker-ABC123");

    Lines lines;
    ASSERT_TRUE (qof_log_foreach_line (collect_line, &lines));
    EXPECT_FALSE (lines.empty ());
    EXPECT_TRUE (contains (lines, "tracelog-marker-ABC123"));

    g_remove (path);
    g_free (path);
}

/* A re-read must reflect newly appended lines, and the positional read must not
   disturb the logger's write offset (so the second message still appends). */
TEST(qoflog, foreach_line_sees_appended_lines_via_positional_read)
{
    gchar *path = g_build_filename (g_get_tmp_dir (), "gnc-qoflog-test-reread", nullptr);
    qof_log_init ();
    qof_log_init_filename (path);
    qof_log_set_level (log_module, QOF_LOG_DEBUG);

    PWARN ("first-line-marker");
    Lines first;
    ASSERT_TRUE (qof_log_foreach_line (collect_line, &first));
    EXPECT_TRUE (contains (first, "first-line-marker"));
    EXPECT_FALSE (contains (first, "second-line-marker"));

    PWARN ("second-line-marker");
    Lines second;
    ASSERT_TRUE (qof_log_foreach_line (collect_line, &second));
    EXPECT_TRUE (contains (second, "first-line-marker"));
    EXPECT_TRUE (contains (second, "second-line-marker"));
    EXPECT_EQ (second.size (), first.size () + 1);

    g_remove (path);
    g_free (path);
}

/* When logging is directed to a console stream there is no readable file, so
   qof_log_foreach_line() returns FALSE and never calls back. */
TEST(qoflog, foreach_line_false_for_console_sink)
{
    qof_log_init_filename_special ("stderr");

    Lines lines;
    EXPECT_FALSE (qof_log_foreach_line (collect_line, &lines));
    EXPECT_TRUE (lines.empty ());
}

/* Returning FALSE from the callback stops iteration; the call still returns TRUE. */
static gboolean
stop_after_one (const char *line, gpointer user_data)
{
    ++*static_cast<int*> (user_data);
    return FALSE;
}

TEST(qoflog, foreach_line_stops_when_callback_returns_false)
{
    gchar *path = g_build_filename (g_get_tmp_dir (), "gnc-qoflog-test-stop", nullptr);
    qof_log_init ();
    qof_log_init_filename (path);
    qof_log_set_level (log_module, QOF_LOG_DEBUG);

    PWARN ("stop-line-1");
    PWARN ("stop-line-2");
    PWARN ("stop-line-3");

    int calls = 0;
    EXPECT_TRUE (qof_log_foreach_line (stop_after_one, &calls));
    EXPECT_EQ (calls, 1);

    g_remove (path);
    g_free (path);
}

/* A line longer than the internal 4096-byte read chunk is delivered whole, once. */
TEST(qoflog, foreach_line_handles_line_longer_than_read_chunk)
{
    gchar *path = g_build_filename (g_get_tmp_dir (), "gnc-qoflog-test-long", nullptr);
    qof_log_init ();
    qof_log_init_filename (path);
    qof_log_set_level (log_module, QOF_LOG_DEBUG);

    std::string payload (10000, 'x');
    PWARN ("long-start %s long-end", payload.c_str ());
    PWARN ("after-long-line");

    Lines lines;
    ASSERT_TRUE (qof_log_foreach_line (collect_line, &lines));
    ASSERT_EQ (lines.size (), static_cast<size_t> (2));
    EXPECT_GE (lines[0].size (), payload.size ());
    EXPECT_NE (lines[0].find ("long-start"), std::string::npos);
    EXPECT_NE (lines[0].find ("long-end"), std::string::npos);
    EXPECT_NE (lines[1].find ("after-long-line"), std::string::npos);

    g_remove (path);
    g_free (path);
}

/* Logging from inside the callback is safe: the new lines land after the
   snapshot, are not visited, and are present on a later pass. */
static gboolean
warn_per_line (const char *line, gpointer user_data)
{
    ++*static_cast<int*> (user_data);
    PWARN ("from-callback");
    return TRUE;
}

TEST(qoflog, foreach_line_allows_logging_from_callback)
{
    gchar *path = g_build_filename (g_get_tmp_dir (), "gnc-qoflog-test-reentrant", nullptr);
    qof_log_init ();
    qof_log_init_filename (path);
    qof_log_set_level (log_module, QOF_LOG_DEBUG);

    PWARN ("reentrant-1");
    PWARN ("reentrant-2");

    int visited = 0;
    ASSERT_TRUE (qof_log_foreach_line (warn_per_line, &visited));
    EXPECT_EQ (visited, 2);

    Lines lines;
    ASSERT_TRUE (qof_log_foreach_line (collect_line, &lines));
    EXPECT_EQ (lines.size (), static_cast<size_t> (4));
    EXPECT_TRUE (contains (lines, "reentrant-1"));
    EXPECT_TRUE (contains (lines, "reentrant-2"));
    EXPECT_TRUE (contains (lines, "from-callback"));

    g_remove (path);
    g_free (path);
}
