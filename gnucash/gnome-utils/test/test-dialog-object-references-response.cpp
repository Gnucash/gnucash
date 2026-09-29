/* test-dialog-object-references-response.cpp -- GTK3 response lifecycle.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 */

#include <config.h>

#include <gtk/gtk.h>

#include "dialog-object-references.h"

static gboolean display_available;

static GtkWidget *
find_references_dialog (void)
{
    GList *windows = gtk_window_list_toplevels ();
    GtkWidget *dialog = NULL;

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
    auto destroy_count = static_cast<guint *> (user_data);

    ++*destroy_count;
}

static void
test_dialog_closes (gconstpointer data)
{
    gboolean answer_button = GPOINTER_TO_INT (data);
    GtkWidget *dialog;
    guint destroy_count = 0;

    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }

    gnc_ui_object_references_show ("References", NULL);
    dialog = find_references_dialog ();
    g_assert_true (GTK_IS_DIALOG (dialog));
    g_assert_true (gtk_window_get_modal (GTK_WINDOW (dialog)));
    g_signal_connect (dialog, "destroy", G_CALLBACK (dialog_destroyed),
                      &destroy_count);

    if (answer_button)
        gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    else
        gtk_window_close (GTK_WINDOW (dialog));

    /* gtk_window_close() dispatches a delete event through the main context. */
    for (guint attempts = 0; destroy_count == 0 && attempts < 1000; ++attempts)
    {
        while (g_main_context_iteration (NULL, FALSE))
            ;
        if (destroy_count == 0)
            g_usleep (1000);
    }

    g_assert_cmpuint (destroy_count, ==, 1);
    g_assert_null (find_references_dialog ());
}

int
main (int argc, char **argv)
{
    g_test_init (&argc, &argv, NULL);
    display_available = gtk_init_check (&argc, &argv);
    if (g_getenv ("GNC_REQUIRE_DISPLAY"))
        g_assert_true (display_available);

    g_test_add_data_func ("/gnome-utils/object-references/response",
                          GINT_TO_POINTER (TRUE), test_dialog_closes);
    g_test_add_data_func ("/gnome-utils/object-references/window-close",
                          GINT_TO_POINTER (FALSE), test_dialog_closes);
    return g_test_run ();
}
