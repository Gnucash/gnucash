/* Copyright (C) 2026 GnuCash contributors
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */

#include <config.h>
#include <gtk/gtk.h>

#include <gtest/gtest.h>

#include "cashobjects.h"
#include "dialog-utils.h"
#include "gnc-session.h"
#include "qofbook.h"

namespace
{
GtkWidget *
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

void
warning_destroyed ([[maybe_unused]] GtkWidget *dialog, gpointer data)
{
    ++*static_cast<guint *> (data);
}

class DateRangeResponseTest : public ::testing::Test
{
protected:
    static void SetUpTestSuite ()
    {
        qof_init ();
        ASSERT_TRUE (cashobjects_register ());
    }

    static void TearDownTestSuite ()
    {
        qof_close ();
    }

    void SetUp () override
    {
        m_session = qof_session_new (qof_book_new ());
        gnc_set_current_session (m_session);
    }

    void TearDown () override
    {
        if (auto dialog = find_warning ())
            gtk_widget_destroy (dialog);
        if (m_parent)
            gtk_widget_destroy (m_parent);
        auto session = gnc_exchange_current_session (nullptr);
        EXPECT_EQ (session, m_session);
        qof_session_destroy (session);
        m_session = nullptr;
    }

    QofSession *m_session{};
    GtkWidget *m_parent{};
    guint m_destroy_count{};

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

    void expect_warning_destroyed ()
    {
        for (guint attempts = 0; m_destroy_count == 0u && attempts < 1000; ++attempts)
        {
            while (g_main_context_iteration (nullptr, FALSE))
                ;
            if (m_destroy_count == 0u)
                g_usleep (1000);
        }

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
    expect_warning_destroyed ();
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
    expect_warning_destroyed ();
    g_object_unref (dialog);
}
} // namespace

int
main (int argc, char **argv)
{
    ::testing::InitGoogleTest (&argc, argv);
    if (!gtk_init_check (&argc, &argv))
        g_error ("A graphical display is required for date-range response tests");
    g_log_set_always_fatal (static_cast<GLogLevelFlags> (
        G_LOG_FATAL_MASK | G_LOG_LEVEL_WARNING | G_LOG_LEVEL_CRITICAL));
    return RUN_ALL_TESTS ();
}
