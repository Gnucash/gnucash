/*
 * test-dialog-sx-since-last-run-ownership.c -- since-last-run column view lifetime
 *
 * This program is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 */

#include <config.h>

#include <gtk/gtk.h>
#include <libguile.h>
#include <stdlib.h>

#include "dialog-sx-since-last-run.h"
#include "gnc-component-manager.h"
#include "gnc-date.h"
#include "gnc-engine.h"
#include "gnc-prefs-utils.h"
#include "gnc-session.h"
#include "gnc-sx-instance-model.h"
#include "qof.h"
#include "test-engine-stuff.h"
#include "test-sx-variable-fixture.h"

#define SLR_COLUMN_COUNT 3

static void
object_finalized (gpointer data, GObject *object)
{
    gboolean *finalized = data;

    *finalized = TRUE;
    (void)object;
}

typedef struct
{
    GtkWindow *window;
    gboolean closed;
    guint updates;
} CloseOnSxUpdate;

static void
close_on_sx_update (GncSxInstanceModel *instances, SchedXaction *sx,
                    CloseOnSxUpdate *close)
{
    if (!close->closed)
    {
        close->closed = TRUE;
        gtk_window_close (close->window);
    }
    close->updates++;
    (void)instances;
    (void)sx;
}

static void
drain_main_context (void)
{
    while (g_main_context_pending (NULL))
        g_main_context_iteration (NULL, FALSE);
}

typedef struct
{
    GMainLoop *loop;
    gboolean frame_seen;
    gboolean timed_out;
    guint tick_id;
    GdkFrameClock *clock;
    gulong after_paint_id;
} FrameWait;

static void
frame_wait_after_paint_cb (GdkFrameClock *clock, gpointer user_data)
{
    FrameWait *wait = user_data;

    wait->frame_seen = TRUE;
    if (g_main_loop_is_running (wait->loop))
        g_main_loop_quit (wait->loop);
    (void)clock;
}

static gboolean
frame_wait_tick_cb (GtkWidget *widget, GdkFrameClock *clock, gpointer user_data)
{
    FrameWait *wait = user_data;

    wait->tick_id = 0;
    wait->clock = GDK_FRAME_CLOCK (g_object_ref (clock));
    wait->after_paint_id = g_signal_connect (clock, "after-paint",
                                              G_CALLBACK (frame_wait_after_paint_cb), wait);
    gdk_frame_clock_request_phase (clock, GDK_FRAME_CLOCK_PHASE_AFTER_PAINT);
    (void)widget;
    return G_SOURCE_REMOVE;
}

static gboolean
frame_wait_timeout_cb (gpointer user_data)
{
    FrameWait *wait = user_data;

    wait->timed_out = TRUE;
    if (g_main_loop_is_running (wait->loop))
        g_main_loop_quit (wait->loop);
    return G_SOURCE_REMOVE;
}

static void
present_and_wait_for_frame (GtkWindow *window)
{
    GMainLoop *loop = g_main_loop_new (NULL, FALSE);
    FrameWait wait = { loop, FALSE, FALSE, 0, NULL, 0 };
    guint timeout_id = 0;

    wait.tick_id = gtk_widget_add_tick_callback (GTK_WIDGET (window), frame_wait_tick_cb,
                                                  &wait, NULL);
    gtk_widget_queue_draw (GTK_WIDGET (window));
    gtk_window_present (window);
    if (!wait.frame_seen)
    {
        timeout_id = g_timeout_add (1000, frame_wait_timeout_cb, &wait);
        g_main_loop_run (loop);
    }
    if (wait.tick_id)
        gtk_widget_remove_tick_callback (GTK_WIDGET (window), wait.tick_id);
    if (timeout_id && !wait.timed_out)
        g_source_remove (timeout_id);
    if (wait.after_paint_id)
        g_signal_handler_disconnect (wait.clock, wait.after_paint_id);
    g_clear_object (&wait.clock);
    g_main_loop_unref (loop);

    g_assert_true (wait.frame_seen);
    g_assert_false (wait.timed_out);
}

typedef gboolean (*TestCondition) (gpointer data);

static gboolean
wait_for_condition (TestCondition condition, gpointer data)
{
    gint64 deadline = g_get_monotonic_time () + 2 * G_TIME_SPAN_SECOND;

    while (g_get_monotonic_time () < deadline)
    {
        if (condition (data))
            return TRUE;
        if (g_main_context_pending (NULL))
            g_main_context_iteration (NULL, FALSE);
        else
            g_usleep (1000);
    }
    return condition (data);
}

static GtkColumnView *
find_column_view (GtkWidget *widget)
{
    if (GTK_IS_COLUMN_VIEW (widget))
        return GTK_COLUMN_VIEW (widget);

    for (GtkWidget *child = gtk_widget_get_first_child (widget); child;
         child = gtk_widget_get_next_sibling (child))
    {
        GtkColumnView *view = find_column_view (child);

        if (view)
            return view;
    }
    return NULL;
}

static void
count_factory_widgets (GtkWidget *widget, guint *drop_downs, guint *entries)
{
    if (GTK_IS_DROP_DOWN (widget))
        (*drop_downs)++;
    if (GTK_IS_ENTRY (widget))
        (*entries)++;

    for (GtkWidget *child = gtk_widget_get_first_child (widget); child;
         child = gtk_widget_get_next_sibling (child))
        count_factory_widgets (child, drop_downs, entries);
}

static GtkDropDown *
find_visible_drop_down (GtkWidget *widget)
{
    if (GTK_IS_DROP_DOWN (widget) && gtk_widget_get_visible (widget))
        return GTK_DROP_DOWN (widget);

    for (GtkWidget *child = gtk_widget_get_first_child (widget); child;
         child = gtk_widget_get_next_sibling (child))
    {
        GtkDropDown *drop_down = find_visible_drop_down (child);

        if (drop_down)
            return drop_down;
    }
    return NULL;
}

static GtkEntry *
find_visible_entry (GtkWidget *widget)
{
    if (GTK_IS_ENTRY (widget) && gtk_widget_get_visible (widget))
        return GTK_ENTRY (widget);

    for (GtkWidget *child = gtk_widget_get_first_child (widget); child;
         child = gtk_widget_get_next_sibling (child))
    {
        GtkEntry *entry = find_visible_entry (child);

        if (entry)
            return entry;
    }
    return NULL;
}

static GtkWindow *
find_since_last_run_window (void)
{
    GListModel *toplevels = gtk_window_get_toplevels ();

    for (guint index = 0; index < g_list_model_get_n_items (toplevels); index++)
    {
        GtkWindow *window = GTK_WINDOW (g_list_model_get_item (toplevels, index));

        if (g_strcmp0 (gtk_widget_get_name (GTK_WIDGET (window)),
                       "gnc-id-sx-since-last-run") == 0)
            return window;
        g_object_unref (window);
    }
    return NULL;
}

static gboolean
since_last_run_factory_widgets_ready (gpointer data)
{
    GtkWindow *window = GTK_WINDOW (data);
    GtkColumnView *view = find_column_view (GTK_WIDGET (window));
    guint drop_downs = 0;
    guint entries = 0;

    if (!view || !gtk_column_view_get_model (view) ||
        g_list_model_get_n_items (gtk_column_view_get_columns (view)) != SLR_COLUMN_COUNT)
        return FALSE;
    count_factory_widgets (GTK_WIDGET (window), &drop_downs, &entries);
    return drop_downs > 0 && entries > 0 &&
           find_visible_entry (GTK_WIDGET (window)) != NULL;
}

static void
watch_columns (GtkColumnView *view, gboolean *finalized, guint n_columns)
{
    GListModel *columns = gtk_column_view_get_columns (view);

    g_assert_cmpuint (g_list_model_get_n_items (columns), ==, n_columns);
    for (guint index = 0; index < n_columns; index++)
    {
        GtkColumnViewColumn *column = GTK_COLUMN_VIEW_COLUMN (
            g_list_model_get_item (columns, index));

        g_object_weak_ref (G_OBJECT (column), object_finalized, &finalized[index]);
        g_object_unref (column);
    }
}

static void
test_since_last_run_column_view_quiesces_before_adapter_release (void)
{
    QofSession *session = qof_session_new (qof_book_new ());
    GDate today;
    GDate yesterday;
    SchedXaction *sx;
    GncSxInstanceModel *instances;
    GncSxInstanceModel *held_instances;
    GtkWindow *window;
    GtkWindow *held_window;
    GtkColumnView *view;
    GtkSelectionModel *selection;
    GtkDropDown *drop_down;
    GtkEntry *entry;
    CloseOnSxUpdate close;
    gulong updated_id;
    gboolean window_finalized = FALSE;
    gboolean columns_finalized[SLR_COLUMN_COUNT] = { FALSE, FALSE, FALSE };

    gnc_set_current_session (session);
    g_date_clear (&today, 1);
    gnc_gdate_set_today (&today);
    yesterday = today;
    g_date_subtract_days (&yesterday, 1);
    sx = add_daily_sx_with_variable ("Since last run ownership", &yesterday);
    g_test_message ("SLR lifetime phase: fixture created");
    instances = gnc_sx_get_current_instances ();
    held_instances = g_object_ref (instances);

    /* Use the public command path: it owns instances and presents the real
     * dialog, whose factories bind only while GTK has an attached model. */
    gnc_ui_sx_since_last_run_dialog (instances);
    g_test_message ("SLR lifetime phase: dialog created");
    drain_main_context ();
    window = find_since_last_run_window ();
    g_assert_nonnull (window);
    view = find_column_view (GTK_WIDGET (window));
    g_assert_nonnull (view);
    g_assert_true (wait_for_condition (since_last_run_factory_widgets_ready,
                                       window));
    g_object_ref (view);
    g_assert_nonnull (gtk_column_view_get_model (view));
    watch_columns (view, columns_finalized, SLR_COLUMN_COUNT);

    /* The populated SLR tree makes GTK run the Status and Value factories in
     * their normal setup/bind phase; this test never emits factory signals. */
    drop_down = find_visible_drop_down (GTK_WIDGET (window));
    g_assert_nonnull (drop_down);
    g_assert_cmpuint (gtk_drop_down_get_selected (drop_down), <,
                      SX_INSTANCE_STATE_CREATED);
    /* The visible-tree search returns a borrowed child. Changing its selected
     * property synchronously updates the instance model and can rebuild the
     * list item that owns this child, so retain it for the whole GTK call. */
    g_object_ref (drop_down);
    gtk_drop_down_set_selected (drop_down, SX_INSTANCE_STATE_REMINDER);
    g_object_unref (drop_down);
    g_test_message ("SLR lifetime phase: reminder selected");
    present_and_wait_for_frame (window);
    g_assert_true (wait_for_condition (since_last_run_factory_widgets_ready,
                                       window));
    drop_down = find_visible_drop_down (GTK_WIDGET (window));
    entry = find_visible_entry (GTK_WIDGET (window));
    g_assert_nonnull (drop_down);
    g_assert_nonnull (entry);
    g_assert_cmpuint (gtk_drop_down_get_selected (drop_down), ==,
                      SX_INSTANCE_STATE_REMINDER);
    g_object_ref (drop_down);
    g_object_ref (entry);
    selection = g_object_ref (gtk_column_view_get_model (view));
    g_object_weak_ref (G_OBJECT (window), object_finalized, &window_finalized);

    /* A bound variable on a Reminder first changes the instance state and then
     * updates its value. An external observer closes the real window during
     * that first synchronous update, so no second mutation may use released
     * row or adapter data. */
    held_window = g_object_ref (window);
    close.window = held_window;
    close.closed = FALSE;
    close.updates = 0;
    updated_id = g_signal_connect (held_instances, "updated",
                                   G_CALLBACK (close_on_sx_update), &close);
    g_test_message ("SLR lifetime phase: before variable activation");
    gtk_editable_set_text (GTK_EDITABLE (entry), "1");
    /* GtkEntry forwards its text child's Enter signal, but does not register
     * an activation signal for gtk_widget_activate(). */
    g_signal_emit_by_name (entry, "activate");
    g_test_message ("SLR lifetime phase: after reentrant close");
    g_assert_true (close.closed);
    g_assert_cmpuint (close.updates, ==, 1);
    g_signal_handler_disconnect (held_instances, updated_id);
    g_object_unref (window);
    drain_main_context ();

    g_assert_null (gtk_column_view_get_model (view));

    /* Retaining the former selection is supported. With the view detached,
     * changing it cannot re-enter a factory with the released adapter. */
    gtk_single_selection_set_can_unselect (GTK_SINGLE_SELECTION (selection), TRUE);
    gtk_single_selection_set_selected (GTK_SINGLE_SELECTION (selection),
                                       GTK_INVALID_LIST_POSITION);
    gtk_drop_down_set_selected (drop_down,
                                gtk_drop_down_get_selected (drop_down) == SX_INSTANCE_STATE_IGNORED
                                ? SX_INSTANCE_STATE_POSTPONED : SX_INSTANCE_STATE_IGNORED);
    gtk_editable_set_text (GTK_EDITABLE (entry), "1");
    g_signal_emit_by_name (entry, "activate");
    g_object_unref (selection);
    g_object_unref (drop_down);
    g_object_unref (entry);
    g_object_unref (view);
    g_object_unref (held_window);
    drain_main_context ();
    g_test_message ("SLR lifetime phase: retained widgets released");

    g_assert_true (window_finalized);
    for (guint index = 0; index < SLR_COLUMN_COUNT; index++)
        g_assert_true (columns_finalized[index]);

    g_object_unref (held_instances);
    g_test_message ("SLR lifetime phase: before fixture removal");
    remove_sx (sx);
    gnc_clear_current_session ();
}

static void
run_tests_with_guile (void *closure, int argc, char **argv)
{
    int status;

    gtk_init ();
    gnc_engine_init (0, NULL);
    gnc_prefs_init ();
    gnc_component_manager_init ();
    g_test_add_func ("/gnome/dialog-sx-since-last-run/column-view-quiesces-on-close",
                     test_since_last_run_column_view_quiesces_before_adapter_release);
    status = g_test_run ();
    gnc_component_manager_shutdown ();
    gnc_prefs_remove_registered ();
    gnc_engine_shutdown ();
    exit (status);
}

int
main (int argc, char **argv)
{
    g_setenv ("GSETTINGS_BACKEND", "memory", TRUE);
    g_test_init (&argc, &argv, NULL);
    scm_boot_guile (argc, argv, run_tests_with_guile, NULL);
    return 0;
}
