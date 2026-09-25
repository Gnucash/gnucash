/********************************************************************
 * test-gnc-html-webview2-visibility.cpp -- WebView2 visibility tests *
 *                                                                  *
 * Copyright (C) 2026 The GnuCash Project                           *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 ********************************************************************/

#include <config.h>

#include <gtk/gtk.h>

#include "gnc-html-webview2-visibility.hpp"

static void
drain_main_context ()
{
    for (guint turn = 0; turn < 100; turn++)
        g_main_context_iteration (nullptr, FALSE);
}

struct MappingState
{
    guint transitions = 0;
    gboolean mapped = FALSE;
};

static void
mapping_changed (GtkWidget *host, gpointer user_data)
{
    auto state = static_cast<MappingState *> (user_data);

    state->transitions++;
    state->mapped = gnc_html_webview2_host_is_mapped (host);
}

static void
test_host_mapping_drives_webview_visibility ()
{
    auto window = GTK_WINDOW (gtk_window_new ());
    auto stack = gtk_stack_new ();
    auto host = gtk_drawing_area_new ();
    auto other = gtk_label_new ("Other tab");
    MappingState state;

    g_object_ref_sink (window);
    gtk_stack_add_named (GTK_STACK (stack), host, "host");
    gtk_stack_add_named (GTK_STACK (stack), other, "other");
    gtk_window_set_child (window, stack);
    g_signal_connect_after (host, "map", G_CALLBACK (mapping_changed), &state);
    g_signal_connect_after (host, "unmap", G_CALLBACK (mapping_changed), &state);
    g_assert_false (gnc_html_webview2_host_is_mapped (host));

    gtk_window_present (window);
    drain_main_context ();
    g_assert_true (gnc_html_webview2_host_is_mapped (host));
    g_assert_true (state.mapped);

    gtk_stack_set_visible_child_name (GTK_STACK (stack), "other");
    drain_main_context ();
    g_assert_true (gtk_widget_get_visible (host));
    g_assert_false (gnc_html_webview2_host_is_mapped (host));
    g_assert_false (state.mapped);

    gtk_stack_set_visible_child_name (GTK_STACK (stack), "host");
    drain_main_context ();
    g_assert_true (gtk_widget_get_visible (host));
    g_assert_true (gnc_html_webview2_host_is_mapped (host));
    g_assert_true (state.mapped);
    g_assert_cmpuint (state.transitions, ==, 3);

    gtk_window_destroy (window);
    g_object_unref (window);
}

int
main (int argc, char **argv)
{
    g_test_init (&argc, &argv, nullptr);
    gtk_init ();

    g_test_add_func ("/html/webview2-visibility/host-mapping",
                     test_host_mapping_drives_webview_visibility);
    return g_test_run ();
}
