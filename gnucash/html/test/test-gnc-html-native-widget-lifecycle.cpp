/********************************************************************
 * test-gnc-html-native-widget-lifecycle.cpp -- GTK cleanup tests *
 *                                                                  *
 * Copyright (C) 2026 The GnuCash Project                           *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 ********************************************************************/

#include <gtk/gtk.h>

#include "gnc-html-native-widget-lifecycle.hpp"

static void
increment (GtkWidget *, gpointer user_data)
{
    ++*static_cast<guint *> (user_data);
}

static void
test_dispose_while_parent_keeps_host (void)
{
    auto window = gtk_window_new ();
    auto parent = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
    auto host = gtk_drawing_area_new ();
    g_object_ref_sink (window);
    gtk_window_set_child (GTK_WINDOW (window), parent);
    gtk_box_append (GTK_BOX (parent), host);

    guint map_count = 0;
    GncHtmlNativeWidgetLifecycle lifecycle (host);
    lifecycle.add_signal (G_OBJECT (host),
                          g_signal_connect (host, "map", G_CALLBACK (increment), &map_count));
    lifecycle.set_tick_callback (gtk_widget_add_tick_callback (
        host, [] (GtkWidget *, GdkFrameClock *, gpointer) -> gboolean { return G_SOURCE_CONTINUE; },
        nullptr, nullptr));

    auto focus = gtk_event_controller_focus_new ();
    auto weak_focus = focus;
    g_object_add_weak_pointer (G_OBJECT (focus), reinterpret_cast<gpointer *> (&weak_focus));
    gtk_widget_add_controller (host, focus);
    lifecycle.add_controller (focus);

    gtk_widget_set_visible (window, TRUE);
    while (g_main_context_iteration (nullptr, FALSE))
        ;
    g_assert_cmpuint (map_count, ==, 1);
    g_assert_true (gtk_widget_get_parent (host) == parent);

    lifecycle.clear ();
    g_assert_cmpuint (lifecycle.tick_callback (), ==, 0);
    g_assert_null (weak_focus);
    g_assert_true (gtk_widget_get_parent (host) == parent);

    gtk_widget_set_visible (host, FALSE);
    g_assert_false (gtk_widget_get_mapped (host));
    gtk_widget_set_visible (host, TRUE);
    g_assert_true (gtk_widget_get_mapped (host));
    while (g_main_context_iteration (nullptr, FALSE))
        ;
    g_assert_cmpuint (map_count, ==, 1);

    gtk_window_destroy (GTK_WINDOW (window));
    g_object_unref (window);
}

static void
test_toplevel_remap (void)
{
    auto window = gtk_window_new ();
    auto host = gtk_drawing_area_new ();
    g_object_ref_sink (window);
    gtk_window_set_child (GTK_WINDOW (window), host);

    gtk_widget_set_visible (window, TRUE);
    while (g_main_context_iteration (nullptr, FALSE))
        ;
    g_assert_true (gtk_widget_get_mapped (window));

    for (unsigned int cycle = 0; cycle < 3; ++cycle)
    {
        gtk_widget_set_visible (window, FALSE);
        g_assert_false (gtk_widget_get_mapped (window));
        gtk_widget_set_visible (window, TRUE);
        g_assert_true (gtk_widget_get_mapped (window));
        while (g_main_context_iteration (nullptr, FALSE))
            ;
    }

    gtk_window_destroy (GTK_WINDOW (window));
    g_object_unref (window);
}

int
main (int argc, char **argv)
{
    gtk_init ();
    g_test_init (&argc, &argv, nullptr);
    g_test_add_func ("/html/native-widget-lifecycle/dispose-parented-host",
                     test_dispose_while_parent_keeps_host);
    g_test_add_func ("/html/native-widget-lifecycle/toplevel-remap", test_toplevel_remap);
    return g_test_run ();
}
