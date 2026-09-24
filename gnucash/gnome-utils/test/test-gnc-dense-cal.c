/*
 * test-gnc-dense-cal.c -- GtkDrawingArea resize regression tests
 * Copyright (C) 2026 GnuCash Developers
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 */

#include <config.h>

#include <gtk/gtk.h>

#include "gnc-dense-cal.h"

static GtkWidget *
find_widget (GtkWidget *widget, GType type)
{
    if (G_TYPE_CHECK_INSTANCE_TYPE (widget, type))
        return widget;

    for (GtkWidget *child = gtk_widget_get_first_child (widget); child;
         child = gtk_widget_get_next_sibling (child))
    {
        GtkWidget *found = find_widget (child, type);

        if (found)
            return found;
    }

    return NULL;
}

typedef struct
{
    GMainLoop *loop;
    GtkWidget *background;
    GtkWidget *drawing_area;
    GtkWidget *previous_child;
    int previous_width;
    guint poll_source;
    guint timeout_source;
    gboolean require_rebuild;
    gboolean ready;
} SceneWait;

static gboolean
scene_is_ready (const SceneWait *wait)
{
    if (!gtk_widget_get_first_child (wait->background) ||
        gtk_widget_get_width (wait->drawing_area) < 1)
        return FALSE;

    return !wait->require_rebuild ||
           (wait->previous_child == NULL &&
            gtk_widget_get_width (wait->drawing_area) != wait->previous_width);
}

static gboolean
poll_scene (gpointer user_data)
{
    SceneWait *wait = user_data;

    if (!scene_is_ready (wait))
        return G_SOURCE_CONTINUE;

    wait->ready = TRUE;
    wait->poll_source = 0;
    if (wait->timeout_source)
    {
        g_source_remove (wait->timeout_source);
        wait->timeout_source = 0;
    }
    g_main_loop_quit (wait->loop);
    return G_SOURCE_REMOVE;
}

static gboolean
scene_wait_timed_out (gpointer user_data)
{
    SceneWait *wait = user_data;

    wait->timeout_source = 0;
    if (wait->poll_source)
    {
        g_source_remove (wait->poll_source);
        wait->poll_source = 0;
    }
    g_main_loop_quit (wait->loop);
    return G_SOURCE_REMOVE;
}

static gboolean
wait_for_scene (GtkWidget *background, GtkWidget *drawing_area,
                GtkWidget *previous_child, int previous_width)
{
    SceneWait wait = {
        .loop = g_main_loop_new (NULL, FALSE),
        .background = background,
        .drawing_area = drawing_area,
        .previous_child = previous_child,
        .previous_width = previous_width,
        .require_rebuild = previous_child != NULL,
    };

    if (wait.previous_child)
        g_object_add_weak_pointer (G_OBJECT (wait.previous_child),
                                   (gpointer *)&wait.previous_child);

    if (scene_is_ready (&wait))
        wait.ready = TRUE;
    else
    {
        wait.poll_source = g_timeout_add (10, poll_scene, &wait);
        wait.timeout_source = g_timeout_add_seconds (5, scene_wait_timed_out,
                                                     &wait);
        g_main_loop_run (wait.loop);
    }

    if (wait.poll_source)
        g_source_remove (wait.poll_source);
    if (wait.timeout_source)
        g_source_remove (wait.timeout_source);
    if (wait.previous_child)
        g_object_remove_weak_pointer (G_OBJECT (wait.previous_child),
                                      (gpointer *)&wait.previous_child);
    g_main_loop_unref (wait.loop);
    return wait.ready;
}

static void
test_resize_rebuilds_calendar_scene (void)
{
    GtkWindow *window = GTK_WINDOW (gtk_window_new ());
    GtkWidget *calendar = gnc_dense_cal_new (window);
    GtkWidget *background;
    GtkWidget *drawing_area;
    GtkWidget *initial_child;
    int initial_width;

    gtk_window_set_default_size (window, 640, 360);
    gtk_window_set_child (window, calendar);
    background = find_widget (calendar, GTK_TYPE_FIXED);
    drawing_area = find_widget (calendar, GTK_TYPE_DRAWING_AREA);
    g_assert_nonnull (background);
    g_assert_nonnull (drawing_area);

    gtk_window_present (window);
    g_assert_true (wait_for_scene (background, drawing_area, NULL, -1));
    initial_child = gtk_widget_get_first_child (background);
    initial_width = gtk_widget_get_width (drawing_area);

    gtk_widget_set_size_request (calendar, 900, 520);
    g_assert_true (wait_for_scene (background, drawing_area, initial_child,
                                   initial_width));

    gtk_window_destroy (window);
}

int
main (int argc, char **argv)
{
    g_test_init (&argc, &argv, NULL);
    g_log_set_always_fatal (G_LOG_LEVEL_WARNING | G_LOG_LEVEL_CRITICAL);
    gtk_init ();

    g_test_add_func ("/gnome-utils/dense-cal/resize-rebuilds-scene",
                     test_resize_rebuilds_calendar_scene);

    return g_test_run ();
}
