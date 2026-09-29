/* Copyright (C) 2026 GnuCash contributors
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 */

#include <config.h>
#include <gtk/gtk.h>

extern "C"
{
#include "search-string.h"
}

static gboolean display_available;

static GtkWidget *
find_warning ()
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *dialog = nullptr;
    for (auto window = windows; window; window = window->next)
        if (GTK_IS_MESSAGE_DIALOG (window->data))
        {
            g_assert_null (dialog);
            dialog = GTK_WIDGET (window->data);
        }
    g_list_free (windows);
    return dialog;
}

static void
test_validation (gconstpointer data)
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    auto parent = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    auto search = gnc_search_string_new ();
    auto core = GNC_SEARCH_CORE_TYPE (search);
    gnc_search_core_type_pass_parent (core, parent);
    auto mode = GPOINTER_TO_INT (data);
    gnc_search_string_set_value (search, mode == 0 ? "" : "[");
    if (mode != 0)
        gnc_search_string_set_how (search, SEARCH_STRING_MATCHES_REGEX);
    g_assert_false (gnc_search_core_type_validate (core));
    auto dialog = find_warning ();
    g_assert_nonnull (dialog);
    g_assert_true (gtk_window_get_modal (GTK_WINDOW (dialog)));
    g_assert_true (gtk_window_get_destroy_with_parent (GTK_WINDOW (dialog)));
    /* The notice owns its text, not the validator or its input storage. */
    gnc_search_string_set_value (search, "valid");
    g_assert_true (gnc_search_core_type_validate (core));
    g_object_unref (search);
    if (mode == 2)
        gtk_widget_destroy (parent);
    else
    {
        gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
        gtk_widget_destroy (parent);
    }
    g_assert_null (find_warning ());
}

int
main (int argc, char **argv)
{
    g_test_init (&argc, &argv, nullptr);
    display_available = gtk_init_check (&argc, &argv);
    if (g_getenv ("GNC_REQUIRE_DISPLAY"))
        g_assert_true (display_available);
    g_test_add_data_func ("/gnome-search/string/empty", GINT_TO_POINTER (0), test_validation);
    g_test_add_data_func ("/gnome-search/string/regex", GINT_TO_POINTER (1), test_validation);
    g_test_add_data_func ("/gnome-search/string/parent-destroyed", GINT_TO_POINTER (2), test_validation);
    return g_test_run ();
}
