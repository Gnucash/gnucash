/* Copyright (C) 2026 GnuCash contributors
 * SPDX-License-Identifier: GPL-2.0-or-later */
#include <config.h>
#include <gtk/gtk.h>
#include <gtest/gtest.h>
#include "googletest-glib-log-handler.hpp"
#include "dialog-utils.h"
#include "gnc-session.h"
#include "gnc-ui-util.h"
#include "qofbook.h"

class DateRangeValidation : public ::testing::Test
{
protected:
    void TearDown () override { gnc_clear_current_session (); }
    bool valid (guint day, GDateMonth month, guint year)
    {
        auto date = g_date_new_dmy (day, month, year);
        auto result = gnc_gdate_in_valid_range (date, FALSE);
        g_date_free (date);
        return result;
    }
};
TEST_F (DateRangeValidation, DefaultBoundaries)
{
    EXPECT_FALSE (valid (31, G_DATE_DECEMBER, 1399));
    EXPECT_TRUE (valid (1, G_DATE_JANUARY, 1400));
    EXPECT_TRUE (valid (1, G_DATE_JANUARY, 2026));
    EXPECT_TRUE (valid (31, G_DATE_DECEMBER, 9999));
    EXPECT_FALSE (valid (1, G_DATE_JANUARY, 10000));
}
TEST_F (DateRangeValidation, AutoReadonlyThresholdAndWarnSemantics)
{
    auto book = gnc_get_current_book ();
    qof_book_begin_edit (book);
    qof_instance_set (QOF_INSTANCE (book), "autoreadonly-days", (gdouble)30, nullptr);
    qof_book_commit_edit (book);
    ASSERT_TRUE (qof_book_uses_autoreadonly (book));
    auto threshold = qof_book_get_autoreadonly_gdate (book);
    ASSERT_NE (threshold, nullptr);
    EXPECT_TRUE (gnc_gdate_in_valid_range (threshold, FALSE));
    g_date_subtract_days (threshold, 1);
    EXPECT_FALSE (gnc_gdate_in_valid_range (threshold, FALSE));
    EXPECT_TRUE (gnc_gdate_in_valid_range (threshold, TRUE));
    g_date_free (threshold);
    EXPECT_TRUE (valid (31, G_DATE_DECEMBER, 9999));
    EXPECT_FALSE (valid (1, G_DATE_JANUARY, 10000));
}
int main (int argc, char **argv)
{
    ::testing::InitGoogleTest (&argc, argv);
    gtk_init (&argc, &argv);
    return RUN_ALL_TESTS ();
}
