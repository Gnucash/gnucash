/*
 * test-budget-view-column-ownership.c -- budget view column ownership
 *
 * This program is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 */

#include <config.h>

#include <gtk/gtk.h>

#include "Account.h"
#include "gnc-budget-view.h"
#include "gnc-commodity.h"
#include "gnc-component-manager.h"
#include "gnc-engine.h"
#include "gnc-prefs-utils.h"
#include "gnc-session.h"
#include "gnc-tree-view-account.h"
#include "qof.h"

typedef struct
{
    GObject *column;
    GObject *factory;
    GWeakRef weak_column;
    GWeakRef weak_factory;
} WatchedColumn;

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

    if (wait.timed_out)
        g_test_message ("Budget view frame timeout: window mapped=%d realized=%d size=%dx%d",
                        gtk_widget_get_mapped (GTK_WIDGET (window)),
                        gtk_widget_get_realized (GTK_WIDGET (window)),
                        gtk_widget_get_width (GTK_WIDGET (window)),
                        gtk_widget_get_height (GTK_WIDGET (window)));
    g_assert_true (wait.frame_seen);
    g_assert_false (wait.timed_out);
}

static void
collect_column_views (GtkWidget *widget, GPtrArray *views)
{
    if (GTK_IS_COLUMN_VIEW (widget))
        g_ptr_array_add (views, widget);

    for (GtkWidget *child = gtk_widget_get_first_child (widget); child;
         child = gtk_widget_get_next_sibling (child))
        collect_column_views (child, views);
}

static GtkEditableLabel *
find_account_period_label (GtkWidget *widget, Account *account)
{
    if (GTK_IS_EDITABLE_LABEL (widget) &&
        g_object_get_data (G_OBJECT (widget), "gnc-budget-account") == account)
        return GTK_EDITABLE_LABEL (g_object_ref (widget));

    for (GtkWidget *child = gtk_widget_get_first_child (widget); child;
         child = gtk_widget_get_next_sibling (child))
    {
        GtkEditableLabel *label = find_account_period_label (child, account);

        if (label)
            return label;
    }
    return NULL;
}

static GPtrArray *
hold_budget_label_controllers (GtkEditableLabel *label)
{
    GListModel *controllers = gtk_widget_observe_controllers (GTK_WIDGET (label));
    GPtrArray *held = g_ptr_array_new_with_free_func (g_object_unref);
    guint key_controllers = 0;
    guint focus_controllers = 0;

    for (guint index = 0; index < g_list_model_get_n_items (controllers); index++)
    {
        GtkEventController *controller = GTK_EVENT_CONTROLLER (
            g_list_model_get_item (controllers, index));

        if (GTK_IS_EVENT_CONTROLLER_KEY (controller))
            key_controllers++;
        else if (GTK_IS_EVENT_CONTROLLER_FOCUS (controller))
            focus_controllers++;
        g_ptr_array_add (held, controller);
    }
    g_object_unref (controllers);
    g_assert_cmpuint (key_controllers, >=, 1);
    g_assert_cmpuint (focus_controllers, >=, 1);
    return held;
}

static WatchedColumn *
watched_column_new (GtkColumnViewColumn *column)
{
    WatchedColumn *watched = g_new0 (WatchedColumn, 1);
    GtkListItemFactory *factory = gtk_column_view_column_get_factory (column);

    g_assert_nonnull (factory);
    watched->column = G_OBJECT (column);
    watched->factory = g_object_ref (G_OBJECT (factory));
    g_weak_ref_init (&watched->weak_column, watched->column);
    g_weak_ref_init (&watched->weak_factory, watched->factory);
    return watched;
}

static void
watched_column_release (WatchedColumn *watched)
{
    g_clear_object (&watched->column);
    g_clear_object (&watched->factory);
}

static gboolean
weak_ref_is_finalized (GWeakRef *weak_ref)
{
    GObject *object = g_weak_ref_get (weak_ref);

    if (!object)
        return TRUE;

    g_object_unref (object);
    return FALSE;
}

static void
report_retained_budget_view (GWeakRef *view_ref)
{
    GObject *view = g_weak_ref_get (view_ref);

    if (view)
        g_test_message ("Disposed budget view remains referenced (ref_count=%u including diagnostic reference)",
                        view->ref_count);

    g_clear_object (&view);
}

static void
watched_column_free (WatchedColumn *watched)
{
    watched_column_release (watched);
    g_weak_ref_clear (&watched->weak_column);
    g_weak_ref_clear (&watched->weak_factory);
    g_free (watched);
}

static GPtrArray *
watch_budget_columns (GncBudgetView *budget_view)
{
    GPtrArray *views = g_ptr_array_new ();
    GPtrArray *watched = g_ptr_array_new_with_free_func ((GDestroyNotify)watched_column_free);

    collect_column_views (GTK_WIDGET (budget_view), views);
    g_assert_cmpuint (views->len, ==, 2);

    for (guint view_index = 0; view_index < views->len; view_index++)
    {
        GListModel *model = gtk_column_view_get_columns (
            GTK_COLUMN_VIEW (g_ptr_array_index (views, view_index)));

        for (guint column_index = 0;
             column_index < g_list_model_get_n_items (model); column_index++)
        {
            GtkColumnViewColumn *column = GTK_COLUMN_VIEW_COLUMN (
                g_list_model_get_item (model, column_index));

            if (gtk_column_view_column_get_fixed_width (column) == 125)
                g_ptr_array_add (watched, watched_column_new (column));
            else
                g_object_unref (column);
        }
    }

    g_ptr_array_unref (views);
    return watched;
}

static void
release_watched_columns (GPtrArray *watched)
{
    for (guint index = 0; index < watched->len; index++)
        watched_column_release (g_ptr_array_index (watched, index));
}

static void
assert_watched_columns_detached (GPtrArray *watched)
{
    for (guint index = 0; index < watched->len; index++)
    {
        WatchedColumn *column = g_ptr_array_index (watched, index);

        /* GnuCash must remove its columns from the view. Finalization of a
         * removed column or its factory also depends on references held by
         * GTK, so it isn't a GnuCash ownership assertion. */
        g_assert_null (gtk_column_view_column_get_column_view (
            GTK_COLUMN_VIEW_COLUMN (column->column)));
    }
}

static void
report_watched_columns_lifetime (GPtrArray *watched)
{
    for (guint index = 0; index < watched->len; index++)
    {
        WatchedColumn *column = g_ptr_array_index (watched, index);

        if (!weak_ref_is_finalized (&column->weak_column))
        {
            GObject *col_obj = g_weak_ref_get (&column->weak_column);
            if (col_obj)
            {
                g_test_message ("Detached budget column %u remains referenced (ptr=%p, title='%s', ref_count=%u)",
                                index,
                                (void*)col_obj,
                                gtk_column_view_column_get_title (GTK_COLUMN_VIEW_COLUMN (col_obj)),
                                col_obj->ref_count);
                g_assert_null (gtk_column_view_column_get_column_view (
                    GTK_COLUMN_VIEW_COLUMN (col_obj)));
                g_object_unref (col_obj);
            }
        }
        if (!weak_ref_is_finalized (&column->weak_factory))
            g_test_message ("Budget column factory %u remains referenced", index);
    }
}

typedef struct
{
    GncBudgetView *view;
    gboolean disposed;
} ReentrantDispose;

static void
dispose_view_on_columns_changed (GListModel *model, guint position,
                                 guint removed, guint added, gpointer data)
{
    ReentrantDispose *state = data;

    if (!state->disposed)
    {
        state->disposed = TRUE;
        g_object_run_dispose (G_OBJECT (state->view));
    }
    (void)model;
    (void)position;
    (void)removed;
    (void)added;
}

static void
test_budget_columns_release_on_rebuild_and_dispose (void)
{
    QofSession *session = qof_session_new (qof_book_new ());
    QofBook *book = qof_session_get_book (session);
    AccountFilterDialog filter = { 0 };
    GncBudget *budget;
    GncBudgetView *budget_view;
    gnc_commodity_table *commodities;
    gnc_commodity *commodity;
    Account *root;
    Account *account;
    GtkWindow *window;
    GPtrArray *first_columns;
    GPtrArray *rebuilt_columns;
    GtkEditableLabel *rebuild_label;
    GtkEditableLabel *close_label;
    GPtrArray *rebuild_controllers;
    GPtrArray *close_controllers;
    GWeakRef weak_view;

    gnc_set_current_session (session);
    gnc_account_create_root (book);
    commodities = gnc_commodity_table_get_table (book);
    commodity = gnc_commodity_table_lookup (commodities, GNC_COMMODITY_NS_CURRENCY,
                                             "USD");
    g_assert_nonnull (commodity);
    root = gnc_book_get_root_account (book);
    account = xaccMallocAccount (book);
    xaccAccountBeginEdit (account);
    xaccAccountSetName (account, "Budget ownership account");
    xaccAccountSetType (account, ACCT_TYPE_EXPENSE);
    xaccAccountSetCommodity (account, commodity);
    gnc_account_append_child (root, account);
    xaccAccountCommitEdit (account);
    budget = gnc_budget_new (book);
    gnc_budget_set_num_periods (budget, 2);
    filter.visible_types = G_MAXUINT32;
    filter.show_hidden = TRUE;
    filter.show_zero_total = TRUE;
    filter.show_unused = TRUE;

    budget_view = gnc_budget_view_new (budget, &filter);
    window = GTK_WINDOW (gtk_window_new ());
    g_object_ref (window);
    gtk_window_set_child (window, GTK_WIDGET (budget_view));
    present_and_wait_for_frame (window);
    gnc_tree_view_account_expand_to_account (
        GNC_TREE_VIEW_ACCOUNT (gnc_budget_view_get_account_tree_view (budget_view)), account);
    present_and_wait_for_frame (window);

    first_columns = watch_budget_columns (budget_view);
    g_assert_cmpuint (first_columns->len, ==, 6);

    rebuild_label = find_account_period_label (GTK_WIDGET (budget_view), account);
    g_assert_nonnull (rebuild_label);

    rebuild_controllers = hold_budget_label_controllers (rebuild_label);

    gtk_editable_label_start_editing (rebuild_label);
    g_assert_true (gtk_editable_label_get_editing (rebuild_label));

    gtk_editable_set_text (GTK_EDITABLE (rebuild_label), "37");

    gnc_budget_view_refresh (budget_view);

    present_and_wait_for_frame (window);

    g_assert_null (g_object_get_data (G_OBJECT (rebuild_label), "gnc-budget-account"));
    g_assert_false (gtk_editable_label_get_editing (rebuild_label));
    for (guint period = 0; period < gnc_budget_get_num_periods (budget); period++)
        g_assert_false (gnc_budget_is_account_period_value_set (budget, account, period));

    assert_watched_columns_detached (first_columns);
    release_watched_columns (first_columns);
    drain_main_context ();
    report_watched_columns_lifetime (first_columns);

    rebuilt_columns = watch_budget_columns (budget_view);
    g_assert_cmpuint (rebuilt_columns->len, ==, 6);
    close_label = find_account_period_label (GTK_WIDGET (budget_view), account);
    g_assert_nonnull (close_label);
    close_controllers = hold_budget_label_controllers (close_label);
    gtk_editable_label_start_editing (close_label);
    g_assert_true (gtk_editable_label_get_editing (close_label));
    gtk_editable_set_text (GTK_EDITABLE (close_label), "43");
    g_weak_ref_init (&weak_view, budget_view);

    /* Keep the view alive so this tests GnuCash's dispose contract even when
     * other references outlive the view's parent. */
    g_object_ref (budget_view);
    gtk_window_set_child (window, NULL);
    present_and_wait_for_frame (window);
    gtk_window_destroy (window);
    g_object_unref (window);
    drain_main_context ();

    /* Disposal must release GnuCash's columns regardless of other references
     * that may still keep the view alive. */
    g_object_run_dispose (G_OBJECT (budget_view));
    assert_watched_columns_detached (rebuilt_columns);
    g_assert_null (g_object_get_data (G_OBJECT (close_label), "gnc-budget-account"));
    g_assert_false (gtk_editable_label_get_editing (close_label));
    gtk_editable_label_start_editing (close_label);
    gtk_editable_label_stop_editing (close_label, TRUE);
    for (guint period = 0; period < gnc_budget_get_num_periods (budget); period++)
        g_assert_false (gnc_budget_is_account_period_value_set (budget, account, period));

    g_ptr_array_unref (close_controllers);
    g_ptr_array_unref (rebuild_controllers);
    g_object_unref (close_label);
    g_object_unref (rebuild_label);
    drain_main_context ();

    release_watched_columns (rebuilt_columns);
    drain_main_context ();
    report_watched_columns_lifetime (rebuilt_columns);

    g_ptr_array_unref (rebuilt_columns);
    g_ptr_array_unref (first_columns);
    g_object_unref (budget_view);
    drain_main_context ();
    report_retained_budget_view (&weak_view);
    g_weak_ref_clear (&weak_view);
    gnc_budget_destroy (budget);
    gnc_clear_current_session ();
}

static void
test_budget_refresh_handles_reentrant_dispose (void)
{
    QofSession *session = qof_session_new (qof_book_new ());
    QofBook *book = qof_session_get_book (session);
    AccountFilterDialog filter = { 0 };
    GncBudget *budget;
    GncBudgetView *budget_view;
    GtkWindow *window;
    GPtrArray *views;
    GtkAdjustment *totals_adjustment;
    GListModel *columns;
    GPtrArray *watched;
    ReentrantDispose state = { NULL, FALSE };
    GWeakRef weak_view;
    gulong changed_handler;

    gnc_set_current_session (session);
    gnc_account_create_root (book);
    budget = gnc_budget_new (book);
    gnc_budget_set_num_periods (budget, 2);
    filter.visible_types = G_MAXUINT32;
    filter.show_hidden = TRUE;
    filter.show_zero_total = TRUE;
    filter.show_unused = TRUE;

    budget_view = gnc_budget_view_new (budget, &filter);
    window = GTK_WINDOW (gtk_window_new ());
    g_object_ref (window);
    gtk_window_set_child (window, GTK_WIDGET (budget_view));
    present_and_wait_for_frame (window);
    watched = watch_budget_columns (budget_view);
    g_assert_cmpuint (watched->len, ==, 6);

    views = g_ptr_array_new ();
    collect_column_views (GTK_WIDGET (budget_view), views);
    g_assert_cmpuint (views->len, ==, 2);
    totals_adjustment = g_object_ref (gtk_scrollable_get_hadjustment (
        GTK_SCROLLABLE (g_ptr_array_index (views, 1))));
    columns = G_LIST_MODEL (g_object_ref (gtk_column_view_get_columns (
        GTK_COLUMN_VIEW (g_ptr_array_index (views, 0)))));
    g_ptr_array_unref (views);

    state.view = GNC_BUDGET_VIEW (g_object_ref (budget_view));
    g_weak_ref_init (&weak_view, budget_view);
    changed_handler = g_signal_connect (columns, "items-changed",
                                        G_CALLBACK (dispose_view_on_columns_changed),
                                        &state);
    gnc_budget_view_refresh (budget_view);
    if (g_signal_handler_is_connected (columns, changed_handler))
        g_signal_handler_disconnect (columns, changed_handler);
    g_object_unref (columns);
    g_assert_true (state.disposed);
    assert_watched_columns_detached (watched);
    g_assert_null (gtk_widget_get_first_child (GTK_WIDGET (state.view)));

    gtk_window_destroy (window);
    g_object_unref (window);
    release_watched_columns (watched);
    g_object_unref (state.view);
    drain_main_context ();
    report_retained_budget_view (&weak_view);
    report_watched_columns_lifetime (watched);

    /* A separately retained scroller must not call into the former owner. */
    gtk_adjustment_configure (totals_adjustment, 40, 0, 100, 1, 10, 10);
    g_assert_cmpfloat (gtk_adjustment_get_value (totals_adjustment), ==, 40);
    g_object_unref (totals_adjustment);

    g_weak_ref_clear (&weak_view);
    g_ptr_array_unref (watched);
    gnc_budget_destroy (budget);
    gnc_clear_current_session ();
}

int
main (int argc, char **argv)
{
    int status;

    g_setenv ("GSETTINGS_BACKEND", "memory", TRUE);
    g_test_init (&argc, &argv, NULL);
    gtk_init ();
    gnc_engine_init_static (argc, argv);
    gnc_prefs_init ();
    gnc_component_manager_init ();
    g_test_add_func ("/gnome/budget-view/columns-release-on-rebuild-and-dispose",
                     test_budget_columns_release_on_rebuild_and_dispose);
    g_test_add_func ("/gnome/budget-view/refresh-handles-reentrant-dispose",
                     test_budget_refresh_handles_reentrant_dispose);
    status = g_test_run ();
    gnc_component_manager_shutdown ();
    gnc_prefs_remove_registered ();
    gnc_engine_shutdown ();
    return status;
}
