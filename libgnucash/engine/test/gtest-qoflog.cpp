/********************************************************************
 * gtest-qoflog.cpp -- unit tests for qof_log_read_current          *
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
#include "qoflog.h"

static QofLogModule log_module = "gnc.test.qoflog";

/* A logged line must be readable back through qof_log_read_current(). */
TEST(qoflog, read_current_returns_logged_lines)
{
    gchar *path = g_build_filename (g_get_tmp_dir (), "gnc-qoflog-test-lines", nullptr);
    qof_log_init ();
    qof_log_init_filename (path);
    qof_log_set_level (log_module, QOF_LOG_DEBUG);

    PWARN ("tracelog-marker-ABC123");

    gsize len = 0;
    gchar *content = qof_log_read_current (&len);
    ASSERT_NE (content, nullptr);
    EXPECT_GT (len, static_cast<gsize> (0));
    EXPECT_NE (g_strstr_len (content, -1, "tracelog-marker-ABC123"), nullptr);

    g_free (content);
    g_remove (path);
    g_free (path);
}

/* A re-read must reflect newly appended lines, and the positional read must not
   disturb the logger's write offset (so the second message still appends). */
TEST(qoflog, read_current_sees_appended_lines_via_positional_read)
{
    gchar *path = g_build_filename (g_get_tmp_dir (), "gnc-qoflog-test-reread", nullptr);
    qof_log_init ();
    qof_log_init_filename (path);
    qof_log_set_level (log_module, QOF_LOG_DEBUG);

    PWARN ("first-line-marker");
    gchar *first = qof_log_read_current (nullptr);
    ASSERT_NE (first, nullptr);
    EXPECT_NE (g_strstr_len (first, -1, "first-line-marker"), nullptr);
    EXPECT_EQ (g_strstr_len (first, -1, "second-line-marker"), nullptr);
    g_free (first);

    PWARN ("second-line-marker");
    gchar *second = qof_log_read_current (nullptr);
    ASSERT_NE (second, nullptr);
    EXPECT_NE (g_strstr_len (second, -1, "first-line-marker"), nullptr);
    EXPECT_NE (g_strstr_len (second, -1, "second-line-marker"), nullptr);
    g_free (second);

    g_remove (path);
    g_free (path);
}

/* When logging is directed to a console stream there is no readable file, so
   qof_log_read_current() reports nothing. */
TEST(qoflog, read_current_null_for_console_sink)
{
    qof_log_init_filename_special ("stderr");

    gsize len = 999;
    gchar *content = qof_log_read_current (&len);
    EXPECT_EQ (content, nullptr);
    EXPECT_EQ (len, static_cast<gsize> (0));
}
