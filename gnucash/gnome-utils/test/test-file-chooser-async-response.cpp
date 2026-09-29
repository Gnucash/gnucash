/* Copyright (C) 2026 GnuCash contributors
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */
#include <config.h>
#include <gtk/gtk.h>
#include <glib/gstdio.h>
#include <unistd.h>

#include "gnc-file.h"

namespace
{
gboolean display_available;

struct Result
{
    guint calls = 0;
    GSList *filenames = nullptr;
};

void
completed (GSList *filenames, gpointer user_data)
{
    auto result = static_cast<Result *> (user_data);
    ++result->calls;
    result->filenames = filenames;
}

GtkWidget *
find_chooser (GtkWindow *parent)
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *chooser = nullptr;
    for (auto node = windows; node; node = node->next)
        if (GTK_IS_FILE_CHOOSER_DIALOG (node->data) &&
            gtk_window_get_transient_for (GTK_WINDOW (node->data)) == parent)
        {
            g_assert_null (chooser);
            chooser = GTK_WIDGET (node->data);
        }
    g_list_free (windows);
    return chooser;
}

void
test_accept_and_late_response ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    GError *error = nullptr;
    /* Enumerate only this fixture, not the user's temporary directory and
     * its unrelated files, permissions and concurrently running programs. */
    auto directory = g_dir_make_tmp("gnc-file-chooser-XXXXXX", &error);
    g_assert_no_error (error);
    g_assert_nonnull(directory);
    auto path = g_build_filename(directory, "selected.gnucash", nullptr);
    g_assert_true(g_file_set_contents(path, "", 0, &error));
    g_assert_no_error(error);

    auto parent = GTK_WINDOW (gtk_window_new (GTK_WINDOW_TOPLEVEL));
    Result result;
    gnc_file_dialog_async (parent, "Choose test file", nullptr, directory,
                           GNC_FILE_DIALOG_OPEN, FALSE, completed, &result, nullptr);
    auto chooser = find_chooser (parent);
    g_assert_nonnull (chooser);
    g_object_ref (chooser);
    gtk_file_chooser_set_filename (GTK_FILE_CHOOSER (chooser), path);
    gboolean selected = FALSE;
    const gint64 deadline = g_get_monotonic_time () + 2 * G_USEC_PER_SEC;
    while (!selected && g_get_monotonic_time () < deadline)
    {
        g_main_context_iteration (nullptr, FALSE);
        gchar *current = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (chooser));
        selected = g_strcmp0 (current, path) == 0;
        g_free (current);
        if (!selected)
            g_usleep (1000);
    }
    g_assert_true (selected);
    gtk_dialog_response (GTK_DIALOG (chooser), GTK_RESPONSE_ACCEPT);
    g_assert_cmpuint (result.calls, ==, 1);
    g_assert_nonnull (result.filenames);
    g_assert_cmpstr (static_cast<const char *> (result.filenames->data), ==, path);

    gtk_dialog_response (GTK_DIALOG (chooser), GTK_RESPONSE_ACCEPT);
    g_assert_cmpuint (result.calls, ==, 1);
    g_slist_free_full (result.filenames, g_free);
    gtk_widget_destroy (GTK_WIDGET (parent));
    g_object_unref (chooser);
    g_unlink (path);
    g_rmdir(directory);
    g_free (path);
    g_free(directory);
}

void
test_cancel ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    auto parent = GTK_WINDOW (gtk_window_new (GTK_WINDOW_TOPLEVEL));
    Result result;
    gnc_file_dialog_async (parent, "Choose test file", nullptr, nullptr,
                           GNC_FILE_DIALOG_OPEN, FALSE, completed, &result, nullptr);
    auto chooser = find_chooser (parent);
    g_assert_nonnull (chooser);
    gtk_dialog_response (GTK_DIALOG (chooser), GTK_RESPONSE_CANCEL);
    g_assert_cmpuint (result.calls, ==, 1);
    g_assert_null (result.filenames);
    gtk_widget_destroy (GTK_WIDGET (parent));
}

void
test_owner_destroy_and_late_response ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    auto parent = GTK_WINDOW (gtk_window_new (GTK_WINDOW_TOPLEVEL));
    Result result;
    gnc_file_dialog_async (parent, "Choose test file", nullptr, nullptr,
                           GNC_FILE_DIALOG_OPEN, FALSE, completed, &result, nullptr);
    auto chooser = find_chooser (parent);
    g_assert_nonnull (chooser);
    g_object_ref (chooser);
    gtk_widget_destroy (GTK_WIDGET (parent));
    g_assert_cmpuint (result.calls, ==, 1);
    g_assert_null (result.filenames);
    gtk_dialog_response (GTK_DIALOG (chooser), GTK_RESPONSE_ACCEPT);
    g_assert_cmpuint (result.calls, ==, 1);
    g_object_unref (chooser);
}
}

int
main (int argc, char **argv)
{
    g_test_init (&argc, &argv, nullptr);
    display_available = gtk_init_check (&argc, &argv);
    if (g_getenv ("GNC_REQUIRE_DISPLAY"))
        g_assert_true (display_available);
    g_test_add_func ("/gnome-utils/file-chooser/accept-late-response",
                     test_accept_and_late_response);
    g_test_add_func ("/gnome-utils/file-chooser/cancel", test_cancel);
    g_test_add_func ("/gnome-utils/file-chooser/owner-destroy-late-response",
                     test_owner_destroy_and_late_response);
    return g_test_run ();
}
