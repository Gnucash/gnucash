/* gtest-dialog-object-references-response.cpp -- GTK3 response lifecycle.
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

#include "dialog-object-references.h"

namespace
{
static GtkWidget *
find_references_dialog ()
{
    GList *windows = gtk_window_list_toplevels ();
    GtkWidget *dialog = nullptr;

    for (GList *node = windows; node; node = node->next)
    {
        auto window = static_cast<GtkWidget *> (node->data);

        if (g_strcmp0 (gtk_widget_get_name (window),
                       "gnc-id-object-reference") == 0)
        {
            dialog = window;
            break;
        }
    }
    g_list_free (windows);
    return dialog;
}

static void
dialog_destroyed ([[maybe_unused]] GtkWidget *dialog, gpointer user_data)
{
    ++*static_cast<std::uint32_t *> (user_data);
}

class ObjectReferencesResponseTest : public ::testing::Test
{
protected:
    void SetUp () override
    {
        gnc_ui_object_references_show ("References", nullptr);
        m_dialog = find_references_dialog ();
        if (m_dialog)
            g_object_ref (m_dialog);
        ASSERT_TRUE (GTK_IS_DIALOG (m_dialog));
        m_destroy_handler = g_signal_connect (
            m_dialog, "destroy", G_CALLBACK (dialog_destroyed), &m_destroy_count);
        ASSERT_TRUE (gtk_window_get_modal (GTK_WINDOW (m_dialog)));
    }

    void TearDown () override
    {
        if (m_dialog)
        {
            if (!gtk_widget_in_destruction (m_dialog))
                gtk_widget_destroy (m_dialog);
            EXPECT_EQ (m_destroy_count, 1u);
            if (m_destroy_handler &&
                g_signal_handler_is_connected (m_dialog, m_destroy_handler))
            {
                g_signal_handler_disconnect (m_dialog, m_destroy_handler);
                m_destroy_handler = 0;
            }
            g_clear_object (&m_dialog);
        }
    }

    bool wait_for_destroy ()
    {
        for (std::uint32_t attempts = 0; m_destroy_count == 0u && attempts < 1000;
             ++attempts)
        {
            while (g_main_context_iteration (nullptr, FALSE))
                ;
            if (m_destroy_count == 0u)
                g_usleep (1000);
        }
        return m_destroy_count == 1 && find_references_dialog () == nullptr;
    }

    GtkWidget *m_dialog{};
    std::uint32_t m_destroy_count{};
    gulong m_destroy_handler{};
};

TEST_F (ObjectReferencesResponseTest, ClosesAfterAcceptResponse)
{
    gtk_dialog_response (GTK_DIALOG (m_dialog), GTK_RESPONSE_OK);
    EXPECT_TRUE (wait_for_destroy ());
    EXPECT_EQ (m_destroy_count, 1u);
}

TEST_F (ObjectReferencesResponseTest, ClosesWithWindow)
{
    gtk_window_close (GTK_WINDOW (m_dialog));
    EXPECT_TRUE (wait_for_destroy ());
    EXPECT_EQ (m_destroy_count, 1u);
}

TEST_F (ObjectReferencesResponseTest, PendingDialogLeavesCleanupToFixture)
{
    while (g_main_context_iteration (nullptr, FALSE))
        ;

    EXPECT_EQ (m_destroy_count, 0u);
    EXPECT_TRUE (gtk_widget_get_visible (m_dialog));
    EXPECT_EQ (find_references_dialog (), m_dialog);
}
} // namespace

int
main (int argc, char **argv)
{
    ::testing::InitGoogleTest (&argc, argv);
    if (!gtk_init_check (&argc, &argv))
        g_error ("A graphical display is required for object-reference response tests");
    gnc::test::initialize_logging ();
    return RUN_ALL_TESTS ();
}
