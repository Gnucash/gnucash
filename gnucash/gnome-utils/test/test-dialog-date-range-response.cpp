/* Copyright (C) 2026 GnuCash contributors
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 */

#include <config.h>
#include <gtk/gtk.h>

#include "cashobjects.h"
#include "dialog-utils.h"
#include "gnc-session.h"
#include "qofbook.h"

static gboolean display_available;

static GtkWidget *
find_warning ()
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *warning = nullptr;
    for (auto node = windows; node; node = node->next)
        if (GTK_IS_MESSAGE_DIALOG (node->data))
        {
            g_assert_null (warning);
            warning = GTK_WIDGET (node->data);
        }
    g_list_free (windows);
    return warning;
}

static void
warning_destroyed ([[maybe_unused]] GtkWidget *dialog, gpointer data)
{
    ++*static_cast<guint *> (data);
}

static void
test_validation ()
{
    auto date = g_date_new_dmy (1, G_DATE_JANUARY, 2026);
    g_assert_true (gnc_gdate_in_valid_range (date, FALSE));
    g_date_set_year (date, 1300);
    g_assert_false (gnc_gdate_in_valid_range (date, FALSE));
    g_date_free (date);
    if (display_available)
        g_assert_null (find_warning ());
}

static void
test_warning (gconstpointer data)
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }

    auto date = g_date_new_dmy (1, G_DATE_JANUARY, 1300);
    g_assert_false (gnc_gdate_in_valid_range (date, TRUE));
    g_date_free (date);
    auto dialog = find_warning ();
    g_assert_nonnull (dialog);
    g_assert_true (gtk_window_get_modal (GTK_WINDOW (dialog)));
    g_assert_true (gtk_window_get_destroy_with_parent (GTK_WINDOW (dialog)));

    guint destroy_count = 0;
    g_signal_connect (dialog, "destroy", G_CALLBACK (warning_destroyed),
                      &destroy_count);
    if (GPOINTER_TO_INT (data) == 2)
    {
        auto parent = gtk_window_new (GTK_WINDOW_TOPLEVEL);
        gtk_window_set_transient_for (GTK_WINDOW (dialog), GTK_WINDOW (parent));
        gtk_widget_destroy (parent);
    }
    else if (GPOINTER_TO_INT (data) == 1)
        gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    else
        gtk_window_close (GTK_WINDOW (dialog));

    for (guint attempts = 0; destroy_count == 0 && attempts < 1000; ++attempts)
    {
        while (g_main_context_iteration (nullptr, FALSE))
            ;
        if (destroy_count == 0)
            g_usleep (1000);
    }
    g_assert_cmpuint (destroy_count, ==, 1);
    g_assert_null (find_warning ());
}

int
main (int argc, char **argv)
{
    g_test_init (&argc, &argv, nullptr);
    display_available = gtk_init_check (&argc, &argv);
    if (g_getenv ("GNC_REQUIRE_DISPLAY"))
        g_assert_true (display_available);
    qof_init ();
    g_assert_true (cashobjects_register ());
    gnc_set_current_session (qof_session_new (qof_book_new ()));
    g_test_add_func ("/gnome-utils/date-range/validation", test_validation);
    g_test_add_data_func ("/gnome-utils/date-range/response",
                          GINT_TO_POINTER (TRUE), test_warning);
    g_test_add_data_func ("/gnome-utils/date-range/window-close",
                          GINT_TO_POINTER (FALSE), test_warning);
    g_test_add_data_func ("/gnome-utils/date-range/parent-destroyed",
                          GINT_TO_POINTER (2), test_warning);
    auto result = g_test_run ();
    gnc_clear_current_session ();
    qof_close ();
    return result;
}
