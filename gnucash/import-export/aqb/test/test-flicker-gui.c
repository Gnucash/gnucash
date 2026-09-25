/*
 * test-flicker-gui.c -- GtkDrawingArea regression test for Flicker TAN input
 * Copyright (C) 2026 GnuCash Developers
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 */

#include <config.h>

#include <gtk/gtk.h>

#include "gnc-flicker-gui.h"

static gboolean
quit_settle_loop (gpointer user_data)
{
    g_main_loop_quit (user_data);
    return G_SOURCE_REMOVE;
}

static void
settle_layout (void)
{
    GMainLoop *loop = g_main_loop_new (NULL, FALSE);

    g_timeout_add (10, quit_settle_loop, loop);
    g_main_loop_run (loop);
    g_main_loop_unref (loop);
}

static void
test_flicker_initializes_gtk4_drawing_areas (void)
{
    GtkWindow *window = GTK_WINDOW (gtk_window_new ());
    GtkWidget *content = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
    GtkWidget *challenge = gtk_drawing_area_new ();
    GtkWidget *marker = gtk_drawing_area_new ();
    GtkWidget *entry = gtk_entry_new ();
    GtkWidget *barwidth = gtk_spin_button_new (NULL, 1.0, 0);
    GtkWidget *delay = gtk_spin_button_new (NULL, 1.0, 0);
    GncFlickerGui gui =
    {
        .dialog = GTK_WIDGET (window),
        .input_entry = entry,
        .flicker_challenge = challenge,
        .flicker_marker = marker,
        .flicker_hbox = content,
        .spin_barwidth = GTK_SPIN_BUTTON (barwidth),
        .spin_delay = GTK_SPIN_BUTTON (delay),
    };

    gtk_box_append (GTK_BOX (content), challenge);
    gtk_box_append (GTK_BOX (content), marker);
    gtk_box_append (GTK_BOX (content), entry);
    gtk_box_append (GTK_BOX (content), barwidth);
    gtk_box_append (GTK_BOX (content), delay);
    gtk_window_set_child (window, content);

    /* In GTK4, connecting the removed GtkWidget::draw signal is a warning.
     * The fatal-warning setting below makes this exercise of the production
     * initializer fail before the window is mapped if that regression returns. */
    ini_flicker_gui ("1234", &gui);
    gtk_window_present (window);
    settle_layout ();

    gtk_widget_queue_draw (challenge);
    gtk_widget_queue_draw (marker);
    settle_layout ();

    gtk_window_destroy (window);
}

int
main (int argc, char **argv)
{
    g_test_init (&argc, &argv, NULL);
    g_log_set_always_fatal (G_LOG_LEVEL_WARNING | G_LOG_LEVEL_CRITICAL);
    gtk_init ();

    g_test_add_func ("/import-export/aqb/flicker/gtk4-drawing-areas",
                     test_flicker_initializes_gtk4_drawing_areas);

    return g_test_run ();
}
