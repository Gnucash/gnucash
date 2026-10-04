/* Copyright (C) 2026 GnuCash contributors
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */
#include <config.h>
#include <cstdint>
#include <gtk/gtk.h>
#include <glib/gstdio.h>
#include <unistd.h>

#include <gtest/gtest.h>
#include "test-logging.hpp"

#include "gnc-file.h"

namespace
{
struct Result
{
    std::uint32_t calls{};
    GSList *filenames{};
};

static void
completed (GSList *filenames, gpointer user_data)
{
    auto result = static_cast<Result *> (user_data);
    ++result->calls;
    result->filenames = filenames;
}

class FileChooserResponseTest : public ::testing::Test
{
protected:
    void SetUp () override
    {
        m_parent = GTK_WINDOW (gtk_window_new (GTK_WINDOW_TOPLEVEL));
        g_object_ref_sink (m_parent);
    }

    void TearDown () override
    {
        if (m_chooser)
            gtk_widget_destroy (m_chooser);
        g_clear_object (&m_chooser);
        if (m_parent)
        {
            gtk_widget_destroy (GTK_WIDGET (m_parent));
            g_object_unref (m_parent);
        }
        g_slist_free_full (m_result.filenames, g_free);
        if (m_path)
            g_unlink (m_path);
        if (m_directory)
            g_rmdir (m_directory);
        g_clear_pointer (&m_path, g_free);
        g_clear_pointer (&m_directory, g_free);
    }

    GtkWidget *start_chooser (const char *directory = nullptr)
    {
        gnc_file_dialog_async (m_parent, "Choose test file", nullptr, directory,
                               GNC_FILE_DIALOG_OPEN, false, completed,
                               &m_result, nullptr);
        auto windows = gtk_window_list_toplevels ();
        for (auto node = windows; node; node = node->next)
            if (GTK_IS_FILE_CHOOSER_DIALOG (node->data) &&
                gtk_window_get_transient_for (GTK_WINDOW (node->data)) == m_parent)
            {
                EXPECT_EQ (m_chooser, nullptr);
                m_chooser = GTK_WIDGET (node->data);
                g_object_ref (m_chooser);
            }
        g_list_free (windows);
        EXPECT_NE (m_chooser, nullptr);
        return m_chooser;
    }

    GtkWindow *m_parent{};
    GtkWidget *m_chooser{};
    Result m_result{};
    gchar *m_directory{};
    gchar *m_path{};
};

TEST_F (FileChooserResponseTest, AcceptReturnsSelectedFileOnce)
{
    GError *error = nullptr;
    m_directory = g_dir_make_tmp ("gnc-file-chooser-XXXXXX", &error);
    ASSERT_EQ (error, nullptr);
    ASSERT_NE (m_directory, nullptr);
    m_path = g_build_filename (m_directory, "selected.gnucash", nullptr);
    ASSERT_TRUE (g_file_set_contents (m_path, "", 0, &error));
    ASSERT_EQ (error, nullptr);

    auto chooser = start_chooser (m_directory);
    ASSERT_NE (chooser, nullptr);
    gtk_file_chooser_set_filename (GTK_FILE_CHOOSER (chooser), m_path);
    bool selected = false;
    const std::int64_t deadline = g_get_monotonic_time () + 2 * G_USEC_PER_SEC;
    while (!selected && g_get_monotonic_time () < deadline)
    {
        g_main_context_iteration (nullptr, false);
        gchar *current = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (chooser));
        selected = g_strcmp0 (current, m_path) == 0;
        g_free (current);
        if (!selected)
            g_usleep (1000);
    }
    ASSERT_TRUE (selected);

    gtk_dialog_response (GTK_DIALOG (chooser), GTK_RESPONSE_ACCEPT);
    ASSERT_EQ (m_result.calls, 1u);
    ASSERT_NE (m_result.filenames, nullptr);
    EXPECT_STREQ (static_cast<const char *> (m_result.filenames->data), m_path);
    gtk_dialog_response (GTK_DIALOG (chooser), GTK_RESPONSE_ACCEPT);
    EXPECT_EQ (m_result.calls, 1u);
}

TEST_F (FileChooserResponseTest, CancelReturnsNoFiles)
{
    auto chooser = start_chooser ();
    ASSERT_NE (chooser, nullptr);
    gtk_dialog_response (GTK_DIALOG (chooser), GTK_RESPONSE_CANCEL);
    EXPECT_EQ (m_result.calls, 1u);
    EXPECT_EQ (m_result.filenames, nullptr);
}

TEST_F (FileChooserResponseTest, DestroyingOwnerCompletesOnceAndIgnoresLateResponse)
{
    auto chooser = start_chooser ();
    ASSERT_NE (chooser, nullptr);
    gtk_widget_destroy (GTK_WIDGET (m_parent));
    EXPECT_EQ (m_result.calls, 1u);
    EXPECT_EQ (m_result.filenames, nullptr);
    gtk_dialog_response (GTK_DIALOG (chooser), GTK_RESPONSE_ACCEPT);
    EXPECT_EQ (m_result.calls, 1u);
}
} // namespace

int
main (int argc, char **argv)
{
    ::testing::InitGoogleTest (&argc, argv);
    if (!gtk_init_check (&argc, &argv))
        g_error ("A graphical display is required for file chooser response tests");
    gnc::test::initialize_logging ();
    return RUN_ALL_TESTS ();
}
