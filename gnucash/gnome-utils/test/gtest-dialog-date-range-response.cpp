/* Copyright (C) 2026 GnuCash contributors
 * SPDX-License-Identifier: GPL-2.0-or-later
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */

#include <config.h>
#include <cstdint>
#include <gtk/gtk.h>

#include <gtest/gtest.h>
#include "googletest-glib-log-handler.hpp"

#include "dialog-utils.h"
#include "gnc-session.h"

namespace
{
static GtkWidget *
find_warning ()
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *warning = nullptr;
    for (auto node = windows; node; node = node->next)
        if (GTK_IS_MESSAGE_DIALOG (node->data))
        {
            EXPECT_EQ (warning, nullptr);
            warning = GTK_WIDGET (node->data);
        }
    g_list_free (windows);
    return warning;
}

static void
warning_destroyed ([[maybe_unused]] GtkWidget *dialog, gpointer data)
{
    ++*static_cast<std::uint32_t *> (data);
}

class DateRangeResponseTest : public ::testing::Test
{
protected:
    void TearDown () override
    {
        if (auto dialog = find_warning ())
            gtk_widget_destroy (dialog);
        if (m_parent)
            gtk_widget_destroy (m_parent);
        gnc_clear_current_session ();
    }

    GtkWidget *m_parent{};
    std::uint32_t m_destroy_count{};

    GtkWidget *create_warning ()
    {
        auto date = g_date_new_dmy (1, G_DATE_JANUARY, 1300);
        EXPECT_FALSE (gnc_gdate_in_valid_range (date, TRUE));
        g_date_free (date);
        return find_warning ();
    }

    void connect_destroy_handler (GtkWidget *dialog)
    {
        g_signal_connect (dialog, "destroy", G_CALLBACK (warning_destroyed),
                          &m_destroy_count);
    }

    void expect_warning_destroyed (unsigned iterations = 1)
    {
        for (unsigned iteration = 0; iteration < iterations; ++iteration)
            g_main_context_iteration (nullptr, FALSE);

        EXPECT_EQ (m_destroy_count, 1u);
        EXPECT_EQ (find_warning (), nullptr);
    }
};

TEST_F (DateRangeResponseTest, ValidDateRangeDoesNotShowWarning)
{
    auto date = g_date_new_dmy (1, G_DATE_JANUARY, 2026);
    EXPECT_TRUE (gnc_gdate_in_valid_range (date, FALSE));
    g_date_set_year (date, 1300);
    EXPECT_FALSE (gnc_gdate_in_valid_range (date, FALSE));
    g_date_free (date);
    EXPECT_EQ (find_warning (), nullptr);
}

TEST_F (DateRangeResponseTest, WarningClosesAfterResponse)
{
    auto dialog = create_warning ();
    ASSERT_NE (dialog, nullptr);
    ASSERT_TRUE (gtk_window_get_modal (GTK_WINDOW (dialog)));
    ASSERT_TRUE (gtk_window_get_destroy_with_parent (GTK_WINDOW (dialog)));
    g_object_ref (dialog);
    connect_destroy_handler (dialog);

    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    expect_warning_destroyed ();
    g_object_unref (dialog);
}

TEST_F (DateRangeResponseTest, WarningClosesWithWindow)
{
    auto dialog = create_warning ();
    ASSERT_NE (dialog, nullptr);
    ASSERT_TRUE (gtk_window_get_modal (GTK_WINDOW (dialog)));
    ASSERT_TRUE (gtk_window_get_destroy_with_parent (GTK_WINDOW (dialog)));
    g_object_ref (dialog);
    connect_destroy_handler (dialog);

    gtk_window_close (GTK_WINDOW (dialog));
    expect_warning_destroyed (2);
    g_object_unref (dialog);
}

TEST_F (DateRangeResponseTest, WarningClosesWhenParentIsDestroyed)
{
    m_parent = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    auto parent = GTK_WINDOW (m_parent);
    auto dialog = create_warning ();
    ASSERT_NE (dialog, nullptr);
    ASSERT_TRUE (gtk_window_get_modal (GTK_WINDOW (dialog)));
    ASSERT_TRUE (gtk_window_get_destroy_with_parent (GTK_WINDOW (dialog)));
    g_object_ref (dialog);
    connect_destroy_handler (dialog);

    gtk_window_set_transient_for (GTK_WINDOW (dialog), parent);
    gtk_widget_destroy (GTK_WIDGET (parent));
    m_parent = nullptr;
    expect_warning_destroyed (2);
    g_object_unref (dialog);
}
} // namespace

int
main (int argc, char **argv)
{
    ::testing::InitGoogleTest (&argc, argv);
    if (!gtk_init_check (&argc, &argv))
        g_error ("A graphical display is required for date-range response tests");
    gnc::test::initialize_logging ();
    return RUN_ALL_TESTS ();
}
