/*
 * test-gnc-window-application.c -- GtkApplication window ownership tests
 * Copyright (C) 2026 GnuCash Developers
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 */

#include <config.h>

#include <gtk/gtk.h>

#include "gnc-gtk-utils.h"

typedef struct
{
    GtkWindow *parent;
    GtkWindow *window;
} TestWindowApplicationState;

static void
application_finalized_cb (gpointer user_data, GObject *where_the_object_was)
{
    gboolean *finalized = user_data;

    (void)where_the_object_was;
    *finalized = TRUE;
}

static gboolean
quit_application_cb (gpointer user_data)
{
    g_application_quit (G_APPLICATION (user_data));
    return G_SOURCE_REMOVE;
}

static void
application_activate_cb (GApplication *application, gpointer user_data)
{
    (void)application;
    (void)user_data;
}

static void
application_startup_cb (GApplication *application,
                        TestWindowApplicationState *state)
{
    GList *windows;

    state->parent = GTK_WINDOW (gtk_window_new ());
    state->window = GTK_WINDOW (gtk_window_new ());
    gtk_window_set_transient_for (state->window, state->parent);
    gnc_window_bind_to_application (state->window);
    g_assert_true (gtk_window_get_application (state->window) == GTK_APPLICATION (application));
    g_assert_true (gtk_window_get_transient_for (state->window) == state->parent);

    gnc_window_bind_to_application (state->window);
    windows = gtk_application_get_windows (GTK_APPLICATION (application));
    g_assert_cmpuint (g_list_length (windows), ==, 1);

    g_idle_add (quit_application_cb, application);
}

static void
test_window_ignores_non_gtk_default_application (void)
{
    GApplication *application;
    GtkWindow *window;

    application = g_application_new (
        "org.gnucash.GnuCash.Test.WindowApplication.NoGtk",
        G_APPLICATION_NON_UNIQUE);
    g_application_set_default (application);

    window = GTK_WINDOW (gtk_window_new ());
    gnc_window_bind_to_application (window);
    g_assert_null (gtk_window_get_application (window));

    gtk_window_destroy (window);
    g_application_set_default (NULL);
    g_object_unref (application);
}

static void
test_window_uses_default_gtk_application (void)
{
    GtkApplication *application;
    TestWindowApplicationState state = { NULL, NULL };
    gchar *argv[] = { (gchar *)"test-gnc-window-application", NULL };
    gboolean application_finalized = FALSE;

    application = gtk_application_new (
        "org.gnucash.GnuCash.Test.WindowApplication",
        G_APPLICATION_NON_UNIQUE);
    g_signal_connect (application, "activate", G_CALLBACK (application_activate_cb),
                      NULL);
    g_signal_connect (application, "startup", G_CALLBACK (application_startup_cb),
                      &state);
    g_assert_cmpint (g_application_run (G_APPLICATION (application), 1, argv), ==, 0);
    g_assert_nonnull (state.window);
    g_assert_true (gtk_window_get_application (state.window) == application);

    g_object_weak_ref (G_OBJECT (application), application_finalized_cb,
                       &application_finalized);
    g_application_set_default (NULL);
    g_object_unref (application);
    g_assert_false (application_finalized);

    gtk_window_destroy (state.window);
    g_assert_true (application_finalized);

    gtk_window_destroy (state.parent);
}

int
main (int argc, char **argv)
{
    g_test_init (&argc, &argv, NULL);
    gtk_init ();

    g_test_add_func ("/gnome-utils/window/non-gtk-default",
                     test_window_ignores_non_gtk_default_application);
    g_test_add_func ("/gnome-utils/window/default-application",
                     test_window_uses_default_gtk_application);

    return g_test_run ();
}
