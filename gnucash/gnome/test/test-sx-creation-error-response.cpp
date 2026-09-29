/* Copyright (C) 2026 GnuCash contributors
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */

#include <config.h>
#include <gtk/gtk.h>
#include "dialog-sx-since-last-run.h"

static gboolean display_available;

static void
test_error_list_consumed_before_response ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    GList *errors = g_list_append (nullptr, g_strdup ("Synthetic creation error"));
    gnc_ui_sx_creation_error_dialog (&errors);
    g_assert_null (errors);
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *dialog = nullptr;
    for (auto node = windows; node; node = node->next)
        if (GTK_IS_MESSAGE_DIALOG (node->data))
        {
            g_assert_null (dialog);
            dialog = GTK_WIDGET (node->data);
        }
    g_list_free (windows);
    g_assert_nonnull (dialog);
    gchar *message = nullptr;
    g_object_get (dialog, "secondary-text", &message, nullptr);
    g_assert_cmpstr (message, ==, "Synthetic creation error");
    g_free (message);
    gpointer weak_dialog = dialog;
    g_object_add_weak_pointer (G_OBJECT (dialog), &weak_dialog);
    /* A second call on the consumed list is harmless. */
    gnc_ui_sx_creation_error_dialog (&errors);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_CLOSE);
    g_assert_null (weak_dialog);
}

int
main (int argc, char **argv)
{
    g_test_init (&argc, &argv, nullptr);
    display_available = gtk_init_check (&argc, &argv);
    if (g_getenv ("GNC_REQUIRE_DISPLAY"))
        g_assert_true (display_available);
    g_test_add_func ("/gnome/sx/creation-errors-consumed-before-response",
                     test_error_list_consumed_before_response);
    return g_test_run ();
}
