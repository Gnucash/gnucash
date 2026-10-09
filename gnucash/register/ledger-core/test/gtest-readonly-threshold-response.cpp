/* gtest-readonly-threshold-response.cpp -- Read-only date warning response.
 * SPDX-License-Identifier: GPL-2.0-or-later
 *
 * Copyright (C) 2026 GnuCash contributors
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 */

#include <config.h>
#include <cstdint>

#include <gtk/gtk.h>
#include <gtest/gtest.h>
#include "googletest-glib-log-handler.hpp"

#include "datecell.h"
#include "gnc-date.h"
#include "gnc-session.h"
#include "gnc-ui-util.h"
#include "qofbook.h"

static void
warning_destroyed ([[maybe_unused]] GtkWidget *widget, gpointer data)
{
    ++*static_cast<std::uint32_t *> (data);
}

class ReadonlyThresholdTest : public ::testing::Test
{
protected:
    void SetUp () override
    {
        book = gnc_get_current_book ();
        qof_book_begin_edit (book);
        qof_instance_set (QOF_INSTANCE (book), "autoreadonly-days", (gdouble)30,
                          NULL);
        qof_book_commit_edit (book);
    }

    void TearDown () override
    {
        if (cell)
            gnc_basic_cell_destroy (cell);
        gnc_clear_current_session ();
    }

    QofBook *book{};
    BasicCell *cell{};
};

TEST_F (ReadonlyThresholdTest, DateAdjustmentShowsSingleModalWarning)
{
    cell = gnc_date_cell_new ();
    ASSERT_NE (cell, nullptr);
    auto date_cell = reinterpret_cast<DateCell *> (cell);
    char old_date[MAX_DATE_LENGTH + 1];
    qof_print_date_dmy_buff (old_date, MAX_DATE_LENGTH, 1, 1, 2000);
    g_free (cell->value);
    cell->value = g_strdup (old_date);

    auto threshold = qof_book_get_autoreadonly_gdate (book);
    ASSERT_NE (threshold, nullptr);
    time64 actual{};
    gnc_date_cell_get_date (date_cell, &actual, TRUE);
    /* Register dates use local midnight; gdate_to_time64 uses the neutral
     * time convention. The read-only boundary is a calendar date. */
    auto actual_date = time64_to_gdate (actual);
    EXPECT_EQ (g_date_compare (&actual_date, threshold), 0);
    g_date_free (threshold);

    GtkWidget *warning = nullptr;
    auto windows = gtk_window_list_toplevels ();
    for (auto item = windows; item; item = item->next)
    {
        if (GTK_IS_MESSAGE_DIALOG (item->data))
        {
            warning = GTK_WIDGET (item->data);
            break;
        }
    }
    g_list_free (windows);
    ASSERT_NE (warning, nullptr);
    EXPECT_TRUE (gtk_window_get_modal (GTK_WINDOW (warning)));
    EXPECT_TRUE (gtk_window_get_destroy_with_parent (GTK_WINDOW (warning)));
    std::uint32_t destroy_count = 0;
    g_signal_connect (warning, "destroy", G_CALLBACK (warning_destroyed),
                      &destroy_count);
    g_object_ref (warning);
    GtkWidget *weak_warning = warning;
    g_object_add_weak_pointer (G_OBJECT (warning),
                               reinterpret_cast<gpointer *> (&weak_warning));
    gtk_dialog_response (GTK_DIALOG (warning), GTK_RESPONSE_OK);
    EXPECT_EQ (destroy_count, 1u);
    gtk_dialog_response (GTK_DIALOG (warning), GTK_RESPONSE_OK);
    EXPECT_EQ (destroy_count, 1u);
    g_object_unref (warning);
    EXPECT_EQ (weak_warning, nullptr);
}

int
main (int argc, char **argv)
{
    ::testing::InitGoogleTest (&argc, argv);
    if (!gtk_init_check (&argc, &argv))
        g_error ("GTK display is required for the read-only threshold test");
    gnc::test::initialize_logging ();
    auto result = RUN_ALL_TESTS ();
    return result;
}
