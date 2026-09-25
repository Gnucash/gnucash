/*
 * This program is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 */

/* test-tree-view-row-ownership.c -- TreeListRow item ownership regression tests. */

#include <config.h>

#include <gtk/gtk.h>

#include "Account.h"
#include "gnc-commodity.h"
#include "gnc-component-manager.h"
#include "gnc-engine.h"
#include "gnc-prefs-utils.h"
#include "gnc-pricedb.h"
#include "gnc-session.h"
#include "gnc-query-view.h"
#include "search-param.h"
#include "gnc-tree-view-account.h"
#include "gnc-tree-model-commodity.h"
#include "gnc-tree-model-price.h"
#include "gnc-tree-view-commodity.h"
#include "gnc-tree-view-owner.h"
#include "gnc-tree-view-price.h"
#include "qof.h"
#include "qofquery-p.h"
#include "Split.h"

static void
object_finalized (gpointer data, GObject *object)
{
    gboolean *finalized = data;

    *finalized = TRUE;
    (void)object;
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
        g_test_message ("Account tree frame timeout: window mapped=%d realized=%d size=%dx%d",
                        gtk_widget_get_mapped (GTK_WIDGET (window)),
                        gtk_widget_get_realized (GTK_WIDGET (window)),
                        gtk_widget_get_width (GTK_WIDGET (window)),
                        gtk_widget_get_height (GTK_WIDGET (window)));
    g_assert_true (wait.frame_seen);
    g_assert_false (wait.timed_out);
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

typedef struct
{
    GtkListItem *list_item;
} FactoryBindCapture;

typedef struct
{
    GObject *view;
    gboolean invoked;
} DisposeOnSelectionChange;

static void
capture_factory_bind (GtkListItemFactory *factory, GtkListItem *list_item,
                      FactoryBindCapture *capture)
{
    if (!capture->list_item)
        capture->list_item = g_object_ref (list_item);
    (void)factory;
}

static void
dispose_view_on_selection_changed (GtkSelectionModel *selection, guint position,
                                   guint n_items,
                                   DisposeOnSelectionChange *context)
{
    if (!context->invoked)
    {
        context->invoked = TRUE;
        g_object_run_dispose (context->view);
    }
    (void)selection;
    (void)position;
    (void)n_items;
}

static void
dispose_view_on_items_changed (GListModel *model, guint position,
                               guint removed, guint added,
                               DisposeOnSelectionChange *context)
{
    if (!context->invoked)
    {
        context->invoked = TRUE;
        g_object_run_dispose (context->view);
    }
    (void)model;
    (void)position;
    (void)removed;
    (void)added;
}

static void
watch_column_owners (GtkColumnView *column_view, guint expected_columns,
                     guint retained_index, gboolean *columns_finalized,
                     GtkListItemFactory **retained_factory,
                     GtkSorter **retained_sorter,
                     FactoryBindCapture *capture, gulong *bind_id)
{
    GListModel *columns = gtk_column_view_get_columns (column_view);

    g_assert_cmpuint (g_list_model_get_n_items (columns), ==, expected_columns);
    for (guint index = 0; index < expected_columns; index++)
    {
        GtkColumnViewColumn *column = g_list_model_get_item (columns, index);

        g_assert_nonnull (column);
        g_object_weak_ref (G_OBJECT (column), object_finalized,
                           &columns_finalized[index]);
        if (index == retained_index)
        {
            GtkListItemFactory *factory = gtk_column_view_column_get_factory (column);
            GtkSorter *sorter = gtk_column_view_column_get_sorter (column);

            g_assert_nonnull (factory);
            g_assert_nonnull (sorter);
            *retained_factory = g_object_ref (factory);
            *retained_sorter = g_object_ref (sorter);
            *bind_id = g_signal_connect (factory, "bind",
                                         G_CALLBACK (capture_factory_bind), capture);
        }
        g_object_unref (column);
    }
}

static void
assert_all_finalized (const gboolean *finalized, guint count)
{
    for (guint index = 0; index < count; index++)
        g_assert_true (finalized[index]);
}

static void
query_row_selected (GNCQueryView *view, gpointer count, gpointer user_data)
{
    guint *emissions = user_data;

    (*emissions)++;
    (void)view;
    (void)count;
}

static GObject *
selected_tree_row_item (GtkSelectionModel *selection)
{
    guint position;
    GObject *tree_row = NULL;
    GObject *item;

    for (position = 0;
         position < g_list_model_get_n_items (G_LIST_MODEL (selection));
         position++)
        if (gtk_selection_model_is_selected (selection, position))
        {
            tree_row = g_list_model_get_item (G_LIST_MODEL (selection), position);
            break;
        }
    g_assert_nonnull (tree_row);
    item = gtk_tree_list_row_get_item (GTK_TREE_LIST_ROW (tree_row));
    g_object_unref (tree_row);
    return item;
}

static GtkTreeListRow *
selected_tree_row (GtkSelectionModel *selection)
{
    guint position;

    for (position = 0;
         position < g_list_model_get_n_items (G_LIST_MODEL (selection));
         position++)
        if (gtk_selection_model_is_selected (selection, position))
            return GTK_TREE_LIST_ROW (g_list_model_get_item
                                      (G_LIST_MODEL (selection), position));
    g_assert_not_reached ();
    return NULL;
}

static GtkColumnViewColumn *
column_by_id (GtkColumnView *view, const gchar *id)
{
    GListModel *columns = gtk_column_view_get_columns (view);

    for (guint index = 0; index < g_list_model_get_n_items (columns); index++)
    {
        GtkColumnViewColumn *column = g_list_model_get_item (columns, index);

        if (g_strcmp0 (gtk_column_view_column_get_id (column), id) == 0)
            return column;
        g_object_unref (column);
    }
    g_assert_not_reached ();
    return NULL;
}

static void
assert_selected_ancestors_expanded (GtkSelectionModel *selection,
                                    guint expected_ancestors)
{
    GtkTreeListRow *row = selected_tree_row (selection);
    guint ancestors = 0;

    for (GtkTreeListRow *parent = gtk_tree_list_row_get_parent (row); parent;)
    {
        GtkTreeListRow *next;

        g_assert_true (gtk_tree_list_row_get_expanded (parent));
        ancestors++;
        next = gtk_tree_list_row_get_parent (parent);
        g_object_unref (parent);
        parent = next;
    }
    g_object_unref (row);
    g_assert_cmpuint (ancestors, ==, expected_ancestors);
}

static void
assert_account_root_order (GtkSelectionModel *selection, Account *first,
                           Account *second)
{
    guint first_position = G_MAXUINT;
    guint second_position = G_MAXUINT;

    for (guint position = 0;
         position < g_list_model_get_n_items (G_LIST_MODEL (selection)); position++)
    {
        GtkTreeListRow *tree_row = GTK_TREE_LIST_ROW (g_list_model_get_item
                                                       (G_LIST_MODEL (selection),
                                                        position));
        GObject *item;

        if (gtk_tree_list_row_get_depth (tree_row) != 0)
        {
            g_object_unref (tree_row);
            continue;
        }
        item = gtk_tree_list_row_get_item (tree_row);
        if (item == G_OBJECT (first))
            first_position = position;
        else if (item == G_OBJECT (second))
            second_position = position;
        g_clear_object (&item);
        g_object_unref (tree_row);
    }
    g_assert_cmpuint (first_position, !=, G_MAXUINT);
    g_assert_cmpuint (second_position, !=, G_MAXUINT);
    g_assert_cmpuint (first_position, <, second_position);
}

static void
assert_commodity_order (GtkSelectionModel *selection, gnc_commodity *first,
                        gnc_commodity *second)
{
    guint first_position = G_MAXUINT;
    guint second_position = G_MAXUINT;

    for (guint position = 0;
         position < g_list_model_get_n_items (G_LIST_MODEL (selection)); position++)
    {
        GtkTreeListRow *tree_row = GTK_TREE_LIST_ROW (g_list_model_get_item
                                                       (G_LIST_MODEL (selection),
                                                        position));
        GncTreeModelCommodityRow *row = GNC_TREE_MODEL_COMMODITY_ROW
            (gtk_tree_list_row_get_item (tree_row));
        gnc_commodity *commodity = gnc_tree_model_commodity_row_get_commodity (row);

        if (commodity == first)
            first_position = position;
        else if (commodity == second)
            second_position = position;
        g_object_unref (row);
        g_object_unref (tree_row);
    }
    g_assert_cmpuint (first_position, !=, G_MAXUINT);
    g_assert_cmpuint (second_position, !=, G_MAXUINT);
    g_assert_cmpuint (first_position, <, second_position);
}

static void
assert_price_order (GtkSelectionModel *selection, GNCPrice *first,
                    GNCPrice *second)
{
    guint first_position = G_MAXUINT;
    guint second_position = G_MAXUINT;

    for (guint position = 0;
         position < g_list_model_get_n_items (G_LIST_MODEL (selection)); position++)
    {
        GtkTreeListRow *tree_row = GTK_TREE_LIST_ROW (g_list_model_get_item
                                                       (G_LIST_MODEL (selection),
                                                        position));
        GncTreeModelPriceRow *row = GNC_TREE_MODEL_PRICE_ROW
            (gtk_tree_list_row_get_item (tree_row));
        GNCPrice *price = gnc_tree_model_price_row_get_price (row);

        if (price == first)
            first_position = position;
        else if (price == second)
            second_position = position;
        g_object_unref (row);
        g_object_unref (tree_row);
    }
    g_assert_cmpuint (first_position, !=, G_MAXUINT);
    g_assert_cmpuint (second_position, !=, G_MAXUINT);
    g_assert_cmpuint (first_position, <, second_position);
}

static guint64
selection_size (GtkSelectionModel *selection)
{
    GtkBitset *selected = gtk_selection_model_get_selection (selection);
    guint64 size = gtk_bitset_get_size (selected);

    gtk_bitset_unref (selected);
    return size;
}

static guint
commodity_position (GtkSelectionModel *selection, gnc_commodity *commodity)
{
    for (guint position = 0;
         position < g_list_model_get_n_items (G_LIST_MODEL (selection)); position++)
    {
        GtkTreeListRow *tree_row = GTK_TREE_LIST_ROW (g_list_model_get_item
                                                       (G_LIST_MODEL (selection),
                                                        position));
        GncTreeModelCommodityRow *row = GNC_TREE_MODEL_COMMODITY_ROW
            (gtk_tree_list_row_get_item (tree_row));
        gnc_commodity *candidate = gnc_tree_model_commodity_row_get_commodity (row);

        g_object_unref (row);
        g_object_unref (tree_row);
        if (candidate == commodity)
            return position;
    }
    return GTK_INVALID_LIST_POSITION;
}

static guint
price_position (GtkSelectionModel *selection, GNCPrice *price)
{
    for (guint position = 0;
         position < g_list_model_get_n_items (G_LIST_MODEL (selection)); position++)
    {
        GtkTreeListRow *tree_row = GTK_TREE_LIST_ROW (g_list_model_get_item
                                                       (G_LIST_MODEL (selection),
                                                        position));
        GncTreeModelPriceRow *row = GNC_TREE_MODEL_PRICE_ROW
            (gtk_tree_list_row_get_item (tree_row));
        GNCPrice *candidate = gnc_tree_model_price_row_get_price (row);

        g_object_unref (row);
        g_object_unref (tree_row);
        if (candidate == price)
            return position;
    }
    return GTK_INVALID_LIST_POSITION;
}

/* The view drops its selection model during dispose. Keeping the model alive
 * externally proves that its tree-list model still owns the selected item
 * until that final external reference is released. Calling dispose twice also
 * exercises the view's idempotent cleanup while restore_state is still queued.
 * The caller determines whether the underlying item is model- or book-owned.
 */
static void
dispose_with_retained_selection (GtkWidget *widget, GtkSelectionModel *selection,
                                 GtkTreeListRow *parent,
                                 gboolean *item_finalized)
{
    gboolean selection_finalized = FALSE;
    guint parent_position = gtk_tree_list_row_get_position (parent);

    g_object_ref (selection);
    g_object_weak_ref (G_OBJECT (selection), object_finalized, &selection_finalized);
    g_object_run_dispose (G_OBJECT (widget));
    g_object_run_dispose (G_OBJECT (widget));
    gtk_tree_list_row_set_expanded (parent, FALSE);
    gtk_tree_list_row_set_expanded (parent, TRUE);
    g_assert_true (gtk_selection_model_select_item (selection, parent_position, TRUE));
    drain_main_context ();
    g_assert_false (selection_finalized);
    g_assert_false (*item_finalized);

    g_object_unref (widget);
    gtk_tree_list_row_set_expanded (parent, FALSE);
    gtk_tree_list_row_set_expanded (parent, TRUE);
    g_assert_true (gtk_selection_model_select_item (selection, parent_position, TRUE));
    drain_main_context ();
    g_assert_false (selection_finalized);
    g_assert_false (*item_finalized);

    g_object_unref (selection);
    g_object_unref (parent);
    drain_main_context ();
    g_assert_true (selection_finalized);
}

static void
test_account_lookup_releases_tree_item (void)
{
    QofSession *session = qof_session_new (qof_book_new ());
    QofBook *book = qof_session_get_book (session);
    Account *root = gnc_account_create_root (book);
    Account *child = xaccMallocAccount (book);
    GtkWidget *widget;
    GncTreeViewAccount *view;
    GObject *item;
    GtkTreeListRow *selected_row;
    GtkTreeListRow *parent_row;
    gboolean finalized = FALSE;

    gnc_set_current_session (session);
    xaccAccountSetName (child, "Ownership child");
    xaccAccountSetType (child, ACCT_TYPE_BANK);
    gnc_account_append_child (root, child);
    widget = gnc_tree_view_account_new_with_root (root, TRUE);
    g_object_ref_sink (widget);
    view = GNC_TREE_VIEW_ACCOUNT (widget);
    gnc_tree_view_account_set_selected_account (view, child);
    drain_main_context ();
    g_assert_true (gnc_tree_view_account_get_cursor_account (view) == child);
    for (guint round = 0; round < 8; round++)
    {
        gnc_tree_view_account_set_selection_mode (view, GTK_SELECTION_MULTIPLE);
        gnc_tree_view_account_set_selected_account (view, child);
        drain_main_context ();
        g_assert_true (gnc_tree_view_account_get_cursor_account (view) == child);
        gnc_tree_view_account_set_selection_mode (view, GTK_SELECTION_SINGLE);
        gnc_tree_view_account_set_selected_account (view, child);
        drain_main_context ();
        g_assert_true (gnc_tree_view_account_get_account_at (view, 1) == child);
    }

    gnc_tree_view_account_refilter (view);
    drain_main_context ();
    item = selected_tree_row_item (gnc_tree_view_account_get_selection_model (view));
    g_object_weak_ref (item, object_finalized, &finalized);
    g_object_unref (item);
    selected_row = selected_tree_row (gnc_tree_view_account_get_selection_model (view));
    parent_row = gtk_tree_list_row_get_parent (selected_row);
    g_object_unref (selected_row);
    g_assert_nonnull (parent_row);
    gnc_tree_view_account_set_selected_account (view, child);
    dispose_with_retained_selection (widget,
                                    gnc_tree_view_account_get_selection_model (view),
                                    parent_row,
                                    &finalized);
    g_assert_false (finalized);
    gnc_clear_current_session ();
    g_assert_true (finalized);
}

static void
test_account_dispose_quiesces_retained_row (void)
{
    QofSession *session = qof_session_new (qof_book_new ());
    QofBook *book = qof_session_get_book (session);
    Account *root = gnc_account_create_root (book);
    Account *child = xaccMallocAccount (book);
    Account *grandchild = xaccMallocAccount (book);
    GtkWidget *widget;
    GtkWindow *window;
    GncTreeViewAccount *view;
    GtkSelectionModel *selection;
    GtkTreeListRow *selected_row;
    GtkTreeListRow *parent_row;
    GtkTreeListRow *root_row;

    gnc_set_current_session (session);
    xaccAccountSetName (child, "Dispose callback child");
    xaccAccountSetType (child, ACCT_TYPE_BANK);
    gnc_account_append_child (root, child);
    xaccAccountSetName (grandchild, "Dispose callback grandchild");
    xaccAccountSetType (grandchild, ACCT_TYPE_BANK);
    gnc_account_append_child (child, grandchild);
    widget = gnc_tree_view_account_new_with_root (root, TRUE);
    g_object_ref_sink (widget);
    view = GNC_TREE_VIEW_ACCOUNT (widget);
    selection = gnc_tree_view_account_get_selection_model (view);
    gnc_tree_view_account_set_selected_account (view, grandchild);
    drain_main_context ();
    selected_row = selected_tree_row (selection);
    parent_row = gtk_tree_list_row_get_parent (selected_row);
    g_object_unref (selected_row);
    g_assert_nonnull (parent_row);
    root_row = gtk_tree_list_row_get_parent (parent_row);
    g_assert_nonnull (root_row);
    gtk_tree_list_row_set_expanded (root_row, TRUE);
    gtk_tree_list_row_set_expanded (parent_row, TRUE);

    window = GTK_WINDOW (g_object_ref_sink (gtk_window_new ()));
    gtk_window_set_default_size (window, 640, 480);
    gtk_window_set_child (window, widget);
    present_and_wait_for_frame (window);
    g_assert_nonnull (g_object_get_data (G_OBJECT (parent_row),
                                         "gnc-account-expansion-listener"));
    gtk_window_set_child (window, NULL);
    gtk_window_destroy (window);
    g_object_unref (window);

    gtk_tree_list_row_set_expanded (root_row, FALSE);
    drain_main_context ();
    g_assert_cmpuint (g_list_model_get_n_items (G_LIST_MODEL (selection)), ==, 1);

    g_object_ref (selection);
    g_object_run_dispose (G_OBJECT (widget));
    g_object_run_dispose (G_OBJECT (widget));
    gtk_tree_list_row_set_expanded (parent_row, FALSE);
    gtk_tree_list_row_set_expanded (parent_row, TRUE);
    g_assert_true (gtk_selection_model_select_item (selection, 0, TRUE));
    drain_main_context ();

    g_object_unref (widget);
    gtk_tree_list_row_set_expanded (parent_row, FALSE);
    gtk_tree_list_row_set_expanded (parent_row, TRUE);
    g_object_unref (selection);
    g_object_unref (root_row);
    g_object_unref (parent_row);
    gnc_clear_current_session ();
}

static void
test_account_column_owners_outlive_disposed_view (void)
{
    QofSession *session = qof_session_new (qof_book_new ());
    QofBook *book = qof_session_get_book (session);
    Account *root = gnc_account_create_root (book);
    Account *child = xaccMallocAccount (book);
    GtkWidget *widget;
    GtkWindow *window;
    GncTreeViewAccount *view;
    GtkColumnView *column_view;
    GListModel *columns;
    GtkColumnViewColumn *column;
    GtkListItemFactory *factory;
    GtkSorter *sorter;
    GtkSelectionModel *selection;
    GtkTreeListRow *first_row;
    GtkTreeListRow *second_row;
    GtkWidget *cell;
    GtkWidget *label;
    FactoryBindCapture capture = { NULL };
    gulong bind_id;
    gboolean column_finalized = FALSE;
    gboolean factory_finalized = FALSE;
    gboolean sorter_finalized = FALSE;
    gboolean cell_finalized = FALSE;
    gboolean first_row_finalized = FALSE;
    gboolean second_row_finalized = FALSE;

    gnc_set_current_session (session);
    xaccAccountSetName (root, "Column owner root");
    xaccAccountSetName (child, "Column owner child");
    xaccAccountSetType (child, ACCT_TYPE_BANK);
    gnc_account_append_child (root, child);
    widget = gnc_tree_view_account_new_with_root (root, TRUE);
    g_object_ref_sink (widget);
    view = GNC_TREE_VIEW_ACCOUNT (widget);
    selection = gnc_tree_view_account_get_selection_model (view);
    first_row = GTK_TREE_LIST_ROW (g_list_model_get_item (G_LIST_MODEL (selection), 0));
    g_assert_nonnull (first_row);
    gtk_tree_list_row_set_expanded (first_row, TRUE);
    drain_main_context ();
    g_assert_cmpuint (g_list_model_get_n_items (G_LIST_MODEL (selection)), ==, 2);
    second_row = GTK_TREE_LIST_ROW (g_list_model_get_item (G_LIST_MODEL (selection), 1));
    g_assert_nonnull (second_row);
    g_assert_true (first_row != second_row);
    column_view = gnc_tree_view_account_get_column_view (view);
    columns = gtk_column_view_get_columns (column_view);
    column = g_list_model_get_item (columns, 0);
    g_assert_nonnull (column);
    factory = gtk_column_view_column_get_factory (column);
    sorter = gtk_column_view_column_get_sorter (column);
    g_assert_nonnull (factory);
    g_assert_nonnull (sorter);
    factory = g_object_ref (factory);
    sorter = g_object_ref (sorter);
    bind_id = g_signal_connect (factory, "bind", G_CALLBACK (capture_factory_bind),
                                &capture);

    window = GTK_WINDOW (g_object_ref_sink (gtk_window_new ()));
    gtk_window_set_default_size (window, 640, 480);
    gtk_window_set_child (window, widget);
    present_and_wait_for_frame (window);
    g_assert_nonnull (capture.list_item);
    cell = g_object_ref (gtk_list_item_get_child (capture.list_item));
    g_assert_nonnull (cell);
    label = gtk_tree_expander_get_child (GTK_TREE_EXPANDER (cell));
    g_assert_nonnull (label);
    g_assert_cmpint (gtk_sorter_compare (sorter, first_row, second_row), !=,
                     GTK_ORDERING_EQUAL);
    g_object_weak_ref (G_OBJECT (column), object_finalized, &column_finalized);
    g_object_weak_ref (G_OBJECT (factory), object_finalized, &factory_finalized);
    g_object_weak_ref (G_OBJECT (sorter), object_finalized, &sorter_finalized);
    g_object_weak_ref (G_OBJECT (cell), object_finalized, &cell_finalized);
    g_object_weak_ref (G_OBJECT (first_row), object_finalized, &first_row_finalized);
    g_object_weak_ref (G_OBJECT (second_row), object_finalized, &second_row_finalized);
    g_object_unref (column);
    g_signal_handler_disconnect (factory, bind_id);

    gtk_window_set_child (window, NULL);
    gtk_window_destroy (window);
    g_object_unref (window);
    g_object_run_dispose (G_OBJECT (widget));
    g_object_run_dispose (G_OBJECT (widget));
    drain_main_context ();
    g_assert_true (column_finalized);
    g_assert_false (factory_finalized);
    g_assert_false (sorter_finalized);
    g_assert_false (cell_finalized);

    g_object_unref (widget);
    g_assert_cmpint (gtk_sorter_compare (sorter, first_row, second_row), ==,
                     GTK_ORDERING_EQUAL);
    g_object_notify (G_OBJECT (label), "editing");

    g_object_unref (first_row);
    g_object_unref (second_row);
    g_object_unref (cell);
    g_object_unref (capture.list_item);
    g_object_unref (factory);
    g_object_unref (sorter);
    drain_main_context ();
    g_assert_true (factory_finalized);
    g_assert_true (sorter_finalized);
    g_assert_true (cell_finalized);
    g_assert_true (first_row_finalized);
    g_assert_true (second_row_finalized);
    gnc_clear_current_session ();
}

static void
test_account_selection_modes_preserve_semantics (void)
{
    QofSession *session = qof_session_new (qof_book_new ());
    QofBook *book = qof_session_get_book (session);
    Account *root = gnc_account_create_root (book);
    Account *first = xaccMallocAccount (book);
    Account *second = xaccMallocAccount (book);
    GtkWidget *widget;
    GncTreeViewAccount *view;
    GtkSelectionModel *selection;
    gboolean old_selection_finalized = FALSE;
    GList *accounts = NULL;
    GList *selected;

    gnc_set_current_session (session);
    xaccAccountSetName (first, "First account");
    xaccAccountSetType (first, ACCT_TYPE_BANK);
    gnc_account_append_child (root, first);
    xaccAccountSetName (second, "Second account");
    xaccAccountSetType (second, ACCT_TYPE_BANK);
    gnc_account_append_child (root, second);
    widget = gnc_tree_view_account_new_with_root (root, FALSE);
    g_object_ref_sink (widget);
    view = GNC_TREE_VIEW_ACCOUNT (widget);
    drain_main_context ();

    selection = gnc_tree_view_account_get_selection_model (view);
    g_assert_true (GTK_IS_SINGLE_SELECTION (selection));
    g_assert_false (gtk_single_selection_get_autoselect (
                        GTK_SINGLE_SELECTION (selection)));
    g_assert_true (gtk_single_selection_get_can_unselect (
                       GTK_SINGLE_SELECTION (selection)));
    g_assert_null (gnc_tree_view_account_get_selected_account (view));

    gnc_tree_view_account_set_selected_account (view, second);
    g_object_weak_ref (G_OBJECT (selection), object_finalized,
                       &old_selection_finalized);
    gnc_tree_view_account_set_selection_mode (view, GTK_SELECTION_MULTIPLE);
    drain_main_context ();
    g_assert_true (old_selection_finalized);
    g_assert_true (gnc_tree_view_account_get_selected_account (view) == second);
    gnc_tree_view_account_set_selection_mode (view, GTK_SELECTION_SINGLE);
    drain_main_context ();
    g_assert_true (gnc_tree_view_account_get_selected_account (view) == second);

    gnc_tree_view_account_set_selected_account (view, first);
    drain_main_context ();
    g_assert_true (gnc_tree_view_account_get_selected_account (view) == first);

    gnc_tree_view_account_set_selection_mode (view, GTK_SELECTION_MULTIPLE);
    drain_main_context ();
    selection = gnc_tree_view_account_get_selection_model (view);
    g_assert_true (GTK_IS_MULTI_SELECTION (selection));
    g_assert_true (gnc_tree_view_account_get_selected_account (view) == first);

    gnc_tree_view_account_set_selection_mode (view, GTK_SELECTION_SINGLE);
    drain_main_context ();
    g_assert_true (gnc_tree_view_account_get_selected_account (view) == first);
    gnc_tree_view_account_set_selected_account (view, NULL);
    drain_main_context ();
    g_assert_null (gnc_tree_view_account_get_selected_account (view));

    gnc_tree_view_account_set_selection_mode (view, GTK_SELECTION_BROWSE);
    drain_main_context ();
    selection = gnc_tree_view_account_get_selection_model (view);
    g_assert_true (GTK_IS_SINGLE_SELECTION (selection));
    g_assert_true (gtk_single_selection_get_autoselect (
                       GTK_SINGLE_SELECTION (selection)));
    g_assert_false (gtk_single_selection_get_can_unselect (
                        GTK_SINGLE_SELECTION (selection)));
    g_assert_nonnull (gnc_tree_view_account_get_selected_account (view));
    gnc_tree_view_account_set_selected_account (view, second);
    drain_main_context ();
    g_assert_true (gnc_tree_view_account_get_selected_account (view) == second);
    gnc_tree_view_account_set_selected_account (view, NULL);
    drain_main_context ();
    g_assert_true (gnc_tree_view_account_get_selected_account (view) == second);

    gnc_tree_view_account_set_selection_mode (view, GTK_SELECTION_MULTIPLE);
    drain_main_context ();
    g_assert_true (gnc_tree_view_account_get_selected_account (view) == second);
    gnc_tree_view_account_set_selection_mode (view, GTK_SELECTION_BROWSE);
    drain_main_context ();
    g_assert_true (gnc_tree_view_account_get_selected_account (view) == second);
    gnc_tree_view_account_set_selected_account (view, NULL);
    drain_main_context ();
    g_assert_true (gnc_tree_view_account_get_selected_account (view) == second);

    gnc_tree_view_account_set_selection_mode (view, GTK_SELECTION_SINGLE);
    drain_main_context ();
    g_assert_true (gnc_tree_view_account_get_selected_account (view) == second);

    gnc_tree_view_account_set_selection_mode (view, GTK_SELECTION_NONE);
    drain_main_context ();
    g_assert_true (GTK_IS_NO_SELECTION (
        gnc_tree_view_account_get_selection_model (view)));
    g_assert_null (gnc_tree_view_account_get_selected_account (view));
    gnc_tree_view_account_set_selected_account (view, first);
    drain_main_context ();
    g_assert_null (gnc_tree_view_account_get_selected_account (view));

    gnc_tree_view_account_set_selection_mode (view, GTK_SELECTION_MULTIPLE);
    drain_main_context ();
    selection = gnc_tree_view_account_get_selection_model (view);
    g_assert_true (GTK_IS_MULTI_SELECTION (selection));
    g_assert_null (gnc_tree_view_account_get_selected_account (view));
    accounts = g_list_append (accounts, first);
    accounts = g_list_append (accounts, second);
    gnc_tree_view_account_set_selected_accounts (view, accounts, FALSE);
    g_list_free (accounts);
    drain_main_context ();
    selected = gnc_tree_view_account_get_selected_accounts (view);
    g_assert_cmpuint (g_list_length (selected), ==, 2);
    g_list_free (selected);

    /* The GTK4 model has no GTK3 anchor object. Keep a deterministic account
     * from the existing selection instead of inventing an unrelated row. */
    gnc_tree_view_account_set_selection_mode (view, GTK_SELECTION_SINGLE);
    drain_main_context ();
    selection = gnc_tree_view_account_get_selection_model (view);
    g_assert_true (GTK_IS_SINGLE_SELECTION (selection));
    g_assert_true (gnc_tree_view_account_get_selected_account (view) == first);
    gnc_tree_view_account_set_selected_account (view, second);
    drain_main_context ();
    g_assert_true (gnc_tree_view_account_get_selected_account (view) == second);

    g_object_unref (widget);
    gnc_clear_current_session ();
}

static void
test_account_model_owns_replaced_root (void)
{
    QofSession *session = qof_session_new (qof_book_new ());
    QofBook *book = qof_session_get_book (session);
    Account *root = gnc_account_create_root (book);
    Account *replacement = xaccMallocAccount (book);
    GtkWidget *widget;
    gboolean root_finalized = FALSE;

    gnc_set_current_session (session);
    widget = gnc_tree_view_account_new_with_root (root, FALSE);
    g_object_ref_sink (widget);
    g_object_weak_ref (G_OBJECT (root), object_finalized, &root_finalized);

    xaccAccountSetType (replacement, ACCT_TYPE_ROOT);
    gnc_book_set_root_account (book, replacement);
    g_assert_false (root_finalized);

    g_object_unref (widget);
    drain_main_context ();
    g_assert_true (root_finalized);
    gnc_clear_current_session ();
}

static void
test_commodity_lookup_releases_tree_item (void)
{
    QofSession *session = qof_session_new (qof_book_new ());
    QofBook *book = qof_session_get_book (session);
    gnc_commodity_table *table = gnc_commodity_table_get_table (book);
    gnc_commodity *commodity;
    GtkWidget *widget;
    GncTreeViewCommodity *view;
    GtkSelectionModel *selection;
    GObject *item;
    GtkTreeListRow *selected_row;
    GtkTreeListRow *parent_row;
    gboolean finalized = FALSE;

    gnc_set_current_session (session);
    commodity = gnc_commodity_new (book, "Ownership currency",
                                   GNC_COMMODITY_NS_CURRENCY, "OWN", "", 100);
    gnc_commodity_table_insert (table, commodity);
    widget = gnc_tree_view_commodity_new (book, NULL);
    g_object_ref_sink (widget);
    view = GNC_TREE_VIEW_COMMODITY (widget);
    selection = gnc_tree_view_commodity_get_selection_model (view);
    gnc_tree_view_commodity_select_commodity (view, commodity);
    drain_main_context ();
    g_assert_true (gnc_tree_view_commodity_get_cursor_commodity (view) == commodity);
    for (guint round = 0; round < 8; round++)
        g_assert_true (gnc_tree_view_commodity_get_cursor_commodity (view) == commodity);

    gnc_tree_view_commodity_refilter (view);
    drain_main_context ();
    item = selected_tree_row_item (selection);
    g_object_weak_ref (item, object_finalized, &finalized);
    g_object_unref (item);
    selected_row = selected_tree_row (selection);
    parent_row = gtk_tree_list_row_get_parent (selected_row);
    g_object_unref (selected_row);
    g_assert_nonnull (parent_row);
    gnc_tree_view_commodity_select_commodity (view, commodity);
    dispose_with_retained_selection (widget, selection, parent_row, &finalized);
    g_assert_true (finalized);
    gnc_clear_current_session ();
}

static void
test_commodity_column_owners_outlive_disposed_view (void)
{
    enum { COMMODITY_COLUMN_COUNT = 11, RETAINED_COLUMN = 1 };
    QofSession *session = qof_session_new (qof_book_new ());
    QofBook *book = qof_session_get_book (session);
    gnc_commodity_table *table = gnc_commodity_table_get_table (book);
    gnc_commodity *first;
    gnc_commodity *second;
    GtkWidget *widget;
    GtkWindow *window;
    GncTreeViewCommodity *view;
    GtkSelectionModel *selection;
    GtkColumnView *column_view;
    GtkListItemFactory *factory = NULL;
    GtkSorter *sorter = NULL;
    GtkTreeListRow *first_row;
    GtkTreeListRow *second_row;
    GtkWidget *cell;
    FactoryBindCapture capture = { NULL };
    DisposeOnSelectionChange dispose_context;
    gulong bind_id = 0;
    gulong dispose_id;
    gboolean columns_finalized[COMMODITY_COLUMN_COUNT] = { FALSE };
    gboolean factory_finalized = FALSE;
    gboolean sorter_finalized = FALSE;
    gboolean cell_finalized = FALSE;
    gboolean first_row_finalized = FALSE;
    gboolean second_row_finalized = FALSE;

    gnc_set_current_session (session);
    first = gnc_commodity_new (book, "First lifetime commodity", "LIFETIME",
                               "ONE", "", 100);
    second = gnc_commodity_new (book, "Second lifetime commodity", "LIFETIME",
                                "TWO", "", 100);
    gnc_commodity_table_insert (table, first);
    gnc_commodity_table_insert (table, second);
    widget = gnc_tree_view_commodity_new (book, NULL);
    g_object_ref_sink (widget);
    view = GNC_TREE_VIEW_COMMODITY (widget);
    selection = g_object_ref (gnc_tree_view_commodity_get_selection_model (view));
    gnc_tree_view_commodity_select_commodity (view, first);
    drain_main_context ();
    first_row = selected_tree_row (selection);
    gnc_tree_view_commodity_select_commodity (view, second);
    drain_main_context ();
    second_row = selected_tree_row (selection);
    g_assert_true (first_row != second_row);

    column_view = gnc_tree_view_commodity_get_column_view (view);
    watch_column_owners (column_view, COMMODITY_COLUMN_COUNT, RETAINED_COLUMN,
                         columns_finalized, &factory, &sorter, &capture, &bind_id);
    window = GTK_WINDOW (g_object_ref_sink (gtk_window_new ()));
    gtk_window_set_default_size (window, 640, 480);
    gtk_window_set_child (window, widget);
    present_and_wait_for_frame (window);
    g_assert_nonnull (capture.list_item);
    cell = g_object_ref (gtk_list_item_get_child (capture.list_item));
    g_assert_nonnull (cell);
    g_assert_cmpint (gtk_sorter_compare (sorter, first_row, second_row), !=,
                     GTK_ORDERING_EQUAL);
    g_object_weak_ref (G_OBJECT (factory), object_finalized, &factory_finalized);
    g_object_weak_ref (G_OBJECT (sorter), object_finalized, &sorter_finalized);
    g_object_weak_ref (G_OBJECT (cell), object_finalized, &cell_finalized);
    g_object_weak_ref (G_OBJECT (first_row), object_finalized, &first_row_finalized);
    g_object_weak_ref (G_OBJECT (second_row), object_finalized, &second_row_finalized);
    g_signal_handler_disconnect (factory, bind_id);

    gtk_window_set_child (window, NULL);
    gtk_window_destroy (window);
    g_object_unref (window);
    dispose_context.view = G_OBJECT (widget);
    dispose_context.invoked = FALSE;
    dispose_id = g_signal_connect (selection, "selection-changed",
                                   G_CALLBACK (dispose_view_on_selection_changed),
                                   &dispose_context);
    gnc_tree_view_commodity_refilter (view);
    g_assert_true (dispose_context.invoked);
    g_signal_handler_disconnect (selection, dispose_id);
    g_object_run_dispose (G_OBJECT (widget));
    drain_main_context ();
    assert_all_finalized (columns_finalized, COMMODITY_COLUMN_COUNT);
    g_assert_false (factory_finalized);
    g_assert_false (sorter_finalized);
    g_assert_false (cell_finalized);
    g_assert_false (first_row_finalized);
    g_assert_false (second_row_finalized);
    g_assert_cmpint (gtk_sorter_compare (sorter, first_row, second_row), !=,
                     GTK_ORDERING_EQUAL);

    g_object_unref (widget);
    g_assert_cmpint (gtk_sorter_compare (sorter, first_row, second_row), !=,
                     GTK_ORDERING_EQUAL);
    g_object_unref (first_row);
    g_object_unref (second_row);
    g_object_unref (cell);
    g_object_unref (capture.list_item);
    g_object_unref (factory);
    g_object_unref (sorter);
    g_object_unref (selection);
    drain_main_context ();
    g_assert_true (factory_finalized);
    g_assert_true (sorter_finalized);
    g_assert_true (cell_finalized);
    g_assert_true (first_row_finalized);
    g_assert_true (second_row_finalized);
    gnc_clear_current_session ();
}

static void
test_price_lookup_releases_tree_item (void)
{
    QofSession *session = qof_session_new (qof_book_new ());
    QofBook *book = qof_session_get_book (session);
    gnc_commodity_table *table = gnc_commodity_table_get_table (book);
    gnc_commodity *currency;
    gnc_commodity *security;
    GNCPrice *price;
    GtkWidget *widget;
    GncTreeViewPrice *view;
    GtkSelectionModel *selection;
    GObject *item;
    GNCPrice *expected_price;
    GtkTreeListRow *selected_row;
    GtkTreeListRow *parent_row;
    gboolean finalized = FALSE;

    gnc_set_current_session (session);
    currency = gnc_commodity_new (book, "Ownership currency", GNC_COMMODITY_NS_CURRENCY,
                                  "OWN", "", 100);
    security = gnc_commodity_new (book, "Ownership security", "NYSE", "OWS", "", 1000);
    gnc_commodity_table_insert (table, currency);
    gnc_commodity_table_insert (table, security);
    price = gnc_price_create (book);
    gnc_price_begin_edit (price);
    gnc_price_set_commodity (price, security);
    gnc_price_set_currency (price, currency);
    gnc_price_set_time64 (price, 1);
    gnc_price_set_value (price, gnc_numeric_create (1, 1));
    gnc_pricedb_add_price (gnc_pricedb_get_db (book), price);
    gnc_price_commit_edit (price);

    widget = gnc_tree_view_price_new (book, NULL);
    g_object_ref_sink (widget);
    view = GNC_TREE_VIEW_PRICE (widget);
    selection = gnc_tree_view_price_get_selection_model (view);
    gnc_tree_view_price_set_selected_price (view, price);
    drain_main_context ();
    g_assert_true (gnc_tree_view_price_get_cursor_price (view) == price);
    expected_price = price;
    gnc_price_unref (price);
    for (guint round = 0; round < 8; round++)
        g_assert_true (gnc_tree_view_price_get_cursor_price (view) == expected_price);

    gnc_tree_view_price_set_filter (view, NULL, NULL, NULL, NULL, NULL);
    drain_main_context ();
    item = selected_tree_row_item (selection);
    g_object_weak_ref (item, object_finalized, &finalized);
    g_object_unref (item);
    selected_row = selected_tree_row (selection);
    parent_row = gtk_tree_list_row_get_parent (selected_row);
    g_object_unref (selected_row);
    g_assert_nonnull (parent_row);
    gnc_tree_view_price_set_selected_price (view, expected_price);
    dispose_with_retained_selection (widget, selection, parent_row, &finalized);
    g_assert_true (finalized);
    gnc_clear_current_session ();
}

static void
test_price_column_owners_outlive_disposed_view (void)
{
    enum { PRICE_COLUMN_COUNT = 6, RETAINED_COLUMN = 2 };
    QofSession *session = qof_session_new (qof_book_new ());
    QofBook *book = qof_session_get_book (session);
    gnc_commodity_table *table = gnc_commodity_table_get_table (book);
    gnc_commodity *currency;
    gnc_commodity *security;
    GNCPrice *first;
    GNCPrice *second;
    GtkWidget *widget;
    GtkWindow *window;
    GncTreeViewPrice *view;
    GtkSelectionModel *selection;
    GtkColumnView *column_view;
    GtkListItemFactory *factory = NULL;
    GtkSorter *sorter = NULL;
    GtkTreeListRow *first_row;
    GtkTreeListRow *second_row;
    GtkWidget *cell;
    FactoryBindCapture capture = { NULL };
    DisposeOnSelectionChange dispose_context;
    gulong bind_id = 0;
    gulong dispose_id;
    gboolean columns_finalized[PRICE_COLUMN_COUNT] = { FALSE };
    gboolean factory_finalized = FALSE;
    gboolean sorter_finalized = FALSE;
    gboolean cell_finalized = FALSE;
    gboolean first_row_finalized = FALSE;
    gboolean second_row_finalized = FALSE;

    gnc_set_current_session (session);
    currency = gnc_commodity_new (book, "Lifetime currency",
                                  GNC_COMMODITY_NS_CURRENCY, "LFC", "", 100);
    security = gnc_commodity_new (book, "Lifetime security", "LIFETIME",
                                  "LFS", "", 1000);
    gnc_commodity_table_insert (table, currency);
    gnc_commodity_table_insert (table, security);
    first = gnc_price_create (book);
    gnc_price_begin_edit (first);
    gnc_price_set_commodity (first, security);
    gnc_price_set_currency (first, currency);
    gnc_price_set_time64 (first, 1);
    gnc_price_set_value (first, gnc_numeric_create (1, 1));
    gnc_price_commit_edit (first);
    g_assert_true (gnc_pricedb_add_price (gnc_pricedb_get_db (book), first));
    second = gnc_price_create (book);
    gnc_price_begin_edit (second);
    gnc_price_set_commodity (second, security);
    gnc_price_set_currency (second, currency);
    gnc_price_set_time64 (second, 86401);
    gnc_price_set_value (second, gnc_numeric_create (2, 1));
    gnc_price_commit_edit (second);
    g_assert_true (gnc_pricedb_add_price (gnc_pricedb_get_db (book), second));

    widget = gnc_tree_view_price_new (book, NULL);
    g_object_ref_sink (widget);
    view = GNC_TREE_VIEW_PRICE (widget);
    selection = g_object_ref (gnc_tree_view_price_get_selection_model (view));
    gnc_tree_view_price_set_selected_price (view, first);
    drain_main_context ();
    g_assert_true (gnc_tree_view_price_get_selected_price (view) == first);
    first_row = selected_tree_row (selection);
    gnc_tree_view_price_set_selected_price (view, second);
    drain_main_context ();
    g_assert_true (gnc_tree_view_price_get_selected_price (view) == second);
    second_row = selected_tree_row (selection);
    g_assert_true (first_row != second_row);
    gnc_price_unref (first);
    gnc_price_unref (second);

    column_view = gnc_tree_view_price_get_column_view (view);
    watch_column_owners (column_view, PRICE_COLUMN_COUNT, RETAINED_COLUMN,
                         columns_finalized, &factory, &sorter, &capture, &bind_id);
    window = GTK_WINDOW (g_object_ref_sink (gtk_window_new ()));
    gtk_window_set_default_size (window, 640, 480);
    gtk_window_set_child (window, widget);
    present_and_wait_for_frame (window);
    g_assert_nonnull (capture.list_item);
    cell = g_object_ref (gtk_list_item_get_child (capture.list_item));
    g_assert_nonnull (cell);
    g_assert_cmpint (gtk_sorter_compare (sorter, first_row, second_row), !=,
                     GTK_ORDERING_EQUAL);
    g_object_weak_ref (G_OBJECT (factory), object_finalized, &factory_finalized);
    g_object_weak_ref (G_OBJECT (sorter), object_finalized, &sorter_finalized);
    g_object_weak_ref (G_OBJECT (cell), object_finalized, &cell_finalized);
    g_object_weak_ref (G_OBJECT (first_row), object_finalized, &first_row_finalized);
    g_object_weak_ref (G_OBJECT (second_row), object_finalized, &second_row_finalized);
    g_signal_handler_disconnect (factory, bind_id);

    gtk_window_set_child (window, NULL);
    gtk_window_destroy (window);
    g_object_unref (window);
    dispose_context.view = G_OBJECT (widget);
    dispose_context.invoked = FALSE;
    dispose_id = g_signal_connect (selection, "selection-changed",
                                   G_CALLBACK (dispose_view_on_selection_changed),
                                   &dispose_context);
    gnc_tree_view_price_set_filter (view, NULL, NULL, NULL, NULL, NULL);
    g_assert_true (dispose_context.invoked);
    g_signal_handler_disconnect (selection, dispose_id);
    g_object_run_dispose (G_OBJECT (widget));
    drain_main_context ();
    assert_all_finalized (columns_finalized, PRICE_COLUMN_COUNT);
    g_assert_false (factory_finalized);
    g_assert_false (sorter_finalized);
    g_assert_false (cell_finalized);
    g_assert_false (first_row_finalized);
    g_assert_false (second_row_finalized);
    g_assert_cmpint (gtk_sorter_compare (sorter, first_row, second_row), ==,
                     GTK_ORDERING_EQUAL);

    g_object_unref (widget);
    g_assert_cmpint (gtk_sorter_compare (sorter, first_row, second_row), ==,
                     GTK_ORDERING_EQUAL);
    g_object_unref (first_row);
    g_object_unref (second_row);
    g_object_unref (cell);
    g_object_unref (capture.list_item);
    g_object_unref (factory);
    g_object_unref (sorter);
    g_object_unref (selection);
    drain_main_context ();
    g_assert_true (factory_finalized);
    g_assert_true (sorter_finalized);
    g_assert_true (cell_finalized);
    g_assert_true (first_row_finalized);
    g_assert_true (second_row_finalized);
    gnc_clear_current_session ();
}

static void
test_commodity_selection_restore_contract (void)
{
    QofSession *session = qof_session_new (qof_book_new ());
    QofBook *book = qof_session_get_book (session);
    gnc_commodity_table *table = gnc_commodity_table_get_table (book);
    gnc_commodity *first;
    gnc_commodity *second;
    GtkWidget *widget;
    GncTreeViewCommodity *view;
    GtkSelectionModel *selection;
    GtkColumnView *column_view;
    GtkColumnViewColumn *name_column;
    gnc_commodity_namespace *initial_namespace;
    DisposeOnSelectionChange dispose_context;
    gulong dispose_id;
    guint first_position;
    guint second_position;

    gnc_set_current_session (session);
    first = gnc_commodity_new (book, "Zulu selection commodity", "SELECTION",
                               "AAA", "", 100);
    second = gnc_commodity_new (book, "Alpha selection commodity", "SELECTION",
                                "ZZZ", "", 100);
    gnc_commodity_table_insert (table, first);
    gnc_commodity_table_insert (table, second);
    widget = gnc_tree_view_commodity_new (book, NULL);
    g_object_ref_sink (widget);
    view = GNC_TREE_VIEW_COMMODITY (widget);
    selection = g_object_ref (gnc_tree_view_commodity_get_selection_model (view));
    column_view = gnc_tree_view_commodity_get_column_view (view);
    name_column = column_by_id (column_view, "name");

    g_assert_true (GTK_IS_SINGLE_SELECTION (selection));
    g_assert_false (gtk_single_selection_get_autoselect
                        (GTK_SINGLE_SELECTION (selection)));
    g_assert_true (gtk_single_selection_get_can_unselect
                       (GTK_SINGLE_SELECTION (selection)));
    g_assert_cmpuint (selection_size (selection), ==, 0);
    g_assert_null (gnc_tree_view_commodity_get_selected_commodity (view));

    /* The first real user selection must be recorded even though the initial
     * roots were built before the selection model existed. */
    g_assert_true (gtk_selection_model_select_item (selection, 0, TRUE));
    initial_namespace = gnc_tree_view_commodity_get_selected_namespace (view);
    g_assert_nonnull (initial_namespace);
    gnc_tree_view_commodity_refilter (view);
    drain_main_context ();
    g_assert_cmpuint (selection_size (selection), ==, 1);
    g_assert_true (gnc_tree_view_commodity_get_selected_namespace (view) ==
                   initial_namespace);
    g_assert_true (gtk_selection_model_unselect_all (selection));
    drain_main_context ();
    g_assert_cmpuint (selection_size (selection), ==, 0);

    gnc_tree_view_commodity_select_commodity (view, first);
    drain_main_context ();
    first_position = commodity_position (selection, first);
    g_assert_cmpuint (first_position, !=, GTK_INVALID_LIST_POSITION);
    g_assert_cmpuint (selection_size (selection), ==, 1);
    g_assert_true (gtk_selection_model_is_selected (selection, first_position));
    g_assert_true (gnc_tree_view_commodity_get_selected_commodity (view) == first);

    gnc_tree_view_commodity_select_commodity (view, second);
    drain_main_context ();
    first_position = commodity_position (selection, first);
    second_position = commodity_position (selection, second);
    g_assert_cmpuint (selection_size (selection), ==, 1);
    g_assert_false (gtk_selection_model_is_selected (selection, first_position));
    g_assert_true (gtk_selection_model_is_selected (selection, second_position));
    g_assert_true (gnc_tree_view_commodity_get_selected_commodity (view) == second);

    gnc_tree_view_commodity_select_commodity (view, NULL);
    drain_main_context ();
    g_assert_cmpuint (selection_size (selection), ==, 1);
    g_assert_true (gnc_tree_view_commodity_get_selected_commodity (view) == second);

    g_assert_true (gtk_selection_model_unselect_all (selection));
    drain_main_context ();
    g_assert_cmpuint (selection_size (selection), ==, 0);
    g_assert_null (gnc_tree_view_commodity_get_selected_commodity (view));

    gnc_tree_view_commodity_select_commodity (view, first);
    drain_main_context ();
    g_assert_cmpuint (selection_size (selection), ==, 1);
    g_assert_true (gnc_tree_view_commodity_get_selected_commodity (view) == first);

    gnc_tree_view_commodity_refilter (view);
    drain_main_context ();
    g_assert_cmpuint (selection_size (selection), ==, 1);
    g_assert_true (gtk_selection_model_is_selected
                       (selection, commodity_position (selection, first)));
    g_assert_true (gnc_tree_view_commodity_get_selected_commodity (view) == first);

    gtk_column_view_sort_by_column (column_view, name_column,
                                    GTK_SORT_DESCENDING);
    drain_main_context ();
    g_assert_cmpuint (selection_size (selection), ==, 1);
    g_assert_true (gtk_selection_model_is_selected
                       (selection, commodity_position (selection, first)));
    g_assert_true (gnc_tree_view_commodity_get_selected_commodity (view) == first);

    g_object_unref (name_column);
    dispose_context.view = G_OBJECT (widget);
    dispose_context.invoked = FALSE;
    dispose_id = g_signal_connect (selection, "selection-changed",
                                   G_CALLBACK (dispose_view_on_selection_changed),
                                   &dispose_context);
    gnc_tree_view_commodity_select_commodity (view, second);
    drain_main_context ();
    g_assert_true (dispose_context.invoked);
    g_signal_handler_disconnect (selection, dispose_id);

    g_object_unref (selection);
    g_object_unref (widget);
    gnc_clear_current_session ();
}

static void
test_price_selection_restore_contract (void)
{
    QofSession *session = qof_session_new (qof_book_new ());
    QofBook *book = qof_session_get_book (session);
    gnc_commodity_table *table = gnc_commodity_table_get_table (book);
    gnc_commodity *currency;
    gnc_commodity *security;
    GNCPrice *first;
    GNCPrice *second;
    GtkWidget *widget;
    GncTreeViewPrice *view;
    GtkSelectionModel *selection;
    GtkColumnView *column_view;
    GtkColumnViewColumn *date_column;
    DisposeOnSelectionChange dispose_context;
    gulong dispose_id;
    guint first_position;
    guint second_position;
    GList *prices;

    gnc_set_current_session (session);
    currency = gnc_commodity_new (book, "Selection currency",
                                  GNC_COMMODITY_NS_CURRENCY, "SEC", "", 100);
    security = gnc_commodity_new (book, "Selection security", "SELECTION",
                                  "SES", "", 1000);
    gnc_commodity_table_insert (table, currency);
    gnc_commodity_table_insert (table, security);
    first = gnc_price_create (book);
    gnc_price_begin_edit (first);
    gnc_price_set_commodity (first, security);
    gnc_price_set_currency (first, currency);
    gnc_price_set_time64 (first, 1);
    gnc_price_set_value (first, gnc_numeric_create (1, 1));
    gnc_price_commit_edit (first);
    g_assert_true (gnc_pricedb_add_price (gnc_pricedb_get_db (book), first));
    second = gnc_price_create (book);
    gnc_price_begin_edit (second);
    gnc_price_set_commodity (second, security);
    gnc_price_set_currency (second, currency);
    gnc_price_set_time64 (second, 86401);
    gnc_price_set_value (second, gnc_numeric_create (2, 1));
    gnc_price_commit_edit (second);
    g_assert_true (gnc_pricedb_add_price (gnc_pricedb_get_db (book), second));

    widget = gnc_tree_view_price_new (book, NULL);
    g_object_ref_sink (widget);
    view = GNC_TREE_VIEW_PRICE (widget);
    selection = g_object_ref (gnc_tree_view_price_get_selection_model (view));
    column_view = gnc_tree_view_price_get_column_view (view);
    date_column = column_by_id (column_view, "date");

    gnc_tree_view_price_set_selected_price (view, first);
    drain_main_context ();
    first_position = price_position (selection, first);
    g_assert_cmpuint (first_position, !=, GTK_INVALID_LIST_POSITION);
    g_assert_cmpuint (selection_size (selection), ==, 1);
    g_assert_true (gtk_selection_model_is_selected (selection, first_position));
    g_assert_true (gnc_tree_view_price_get_selected_price (view) == first);

    gnc_tree_view_price_set_selected_price (view, second);
    drain_main_context ();
    first_position = price_position (selection, first);
    second_position = price_position (selection, second);
    g_assert_cmpuint (selection_size (selection), ==, 1);
    g_assert_false (gtk_selection_model_is_selected (selection, first_position));
    g_assert_true (gtk_selection_model_is_selected (selection, second_position));
    g_assert_true (gnc_tree_view_price_get_selected_price (view) == second);

    g_assert_true (gtk_selection_model_select_item (selection, first_position, FALSE));
    drain_main_context ();
    prices = gnc_tree_view_price_get_selected_prices (view);
    g_assert_cmpuint (selection_size (selection), ==, 2);
    g_assert_cmpuint (g_list_length (prices), ==, 2);
    g_assert_nonnull (g_list_find (prices, first));
    g_assert_nonnull (g_list_find (prices, second));
    g_list_free (prices);

    gnc_tree_view_price_set_filter (view, NULL, NULL, NULL, NULL, NULL);
    drain_main_context ();
    g_assert_cmpuint (selection_size (selection), ==, 2);
    g_assert_true (gtk_selection_model_is_selected
                       (selection, price_position (selection, first)));
    g_assert_true (gtk_selection_model_is_selected
                       (selection, price_position (selection, second)));

    gtk_column_view_sort_by_column (column_view, date_column,
                                    GTK_SORT_ASCENDING);
    drain_main_context ();
    g_assert_cmpuint (selection_size (selection), ==, 2);
    g_assert_true (gtk_selection_model_is_selected
                       (selection, price_position (selection, first)));
    g_assert_true (gtk_selection_model_is_selected
                       (selection, price_position (selection, second)));

    gnc_tree_view_price_set_selected_price (view, NULL);
    drain_main_context ();
    g_assert_cmpuint (selection_size (selection), ==, 0);
    g_assert_null (gnc_tree_view_price_get_selected_price (view));
    prices = gnc_tree_view_price_get_selected_prices (view);
    g_assert_null (prices);

    gnc_tree_view_price_set_selected_price (view, first);
    drain_main_context ();
    g_assert_cmpuint (selection_size (selection), ==, 1);
    g_assert_true (gnc_tree_view_price_get_selected_price (view) == first);

    g_object_unref (date_column);
    dispose_context.view = G_OBJECT (widget);
    dispose_context.invoked = FALSE;
    dispose_id = g_signal_connect (selection, "selection-changed",
                                   G_CALLBACK (dispose_view_on_selection_changed),
                                   &dispose_context);
    gnc_tree_view_price_set_selected_price (view, NULL);
    drain_main_context ();
    g_assert_true (dispose_context.invoked);
    g_signal_handler_disconnect (selection, dispose_id);

    g_object_unref (selection);
    g_object_unref (widget);
    gnc_price_unref (first);
    gnc_price_unref (second);
    gnc_clear_current_session ();
}

static void
test_account_native_column_sorting (void)
{
    QofSession *session = qof_session_new (qof_book_new ());
    QofBook *book = qof_session_get_book (session);
    Account *root = gnc_account_create_root (book);
    Account *by_code = xaccMallocAccount (book);
    Account *by_name = xaccMallocAccount (book);
    Account *selected = xaccMallocAccount (book);
    GtkWidget *widget;
    GncTreeViewAccount *view;
    GtkColumnView *column_view;
    GtkColumnViewColumn *name_column;
    GtkColumnViewColumn *code_column;
    GtkSelectionModel *selection;
    GtkSorter *view_sorter;
    GtkWindow *first_window;
    GtkWindow *second_window;

    gnc_set_current_session (session);
    xaccAccountSetName (by_code, "Zulu by name");
    xaccAccountSetCode (by_code, "100");
    xaccAccountSetType (by_code, ACCT_TYPE_BANK);
    gnc_account_append_child (root, by_code);
    xaccAccountSetName (by_name, "Alpha by name");
    xaccAccountSetCode (by_name, "900");
    xaccAccountSetType (by_name, ACCT_TYPE_BANK);
    gnc_account_append_child (root, by_name);
    xaccAccountSetName (selected, "Selected child");
    xaccAccountSetType (selected, ACCT_TYPE_BANK);
    gnc_account_append_child (by_code, selected);

    widget = gnc_tree_view_account_new_with_root (root, FALSE);
    g_object_ref_sink (widget);
    view = GNC_TREE_VIEW_ACCOUNT (widget);
    column_view = gnc_tree_view_account_get_column_view (view);
    selection = gnc_tree_view_account_get_selection_model (view);
    name_column = column_by_id (column_view, "name");
    code_column = column_by_id (column_view, "account-code");
    view_sorter = g_object_ref (gtk_column_view_get_sorter (column_view));
    gnc_tree_view_account_set_selected_account (view, selected);
    drain_main_context ();

    gtk_column_view_sort_by_column (column_view, name_column,
                                    GTK_SORT_ASCENDING);
    drain_main_context ();
    assert_account_root_order (selection, by_name, by_code);
    g_assert_true (gnc_tree_view_account_get_selected_account (view) == selected);
    assert_selected_ancestors_expanded (selection, 1);
    gtk_column_view_sort_by_column (column_view, name_column,
                                    GTK_SORT_DESCENDING);
    drain_main_context ();
    assert_account_root_order (selection, by_code, by_name);
    g_assert_true (gnc_tree_view_account_get_selected_account (view) == selected);
    assert_selected_ancestors_expanded (selection, 1);
    gtk_column_view_sort_by_column (column_view, code_column,
                                    GTK_SORT_ASCENDING);
    drain_main_context ();
    assert_account_root_order (selection, by_code, by_name);
    g_assert_true (gnc_tree_view_account_get_selected_account (view) == selected);
    assert_selected_ancestors_expanded (selection, 1);
    gtk_column_view_sort_by_column (column_view, code_column,
                                    GTK_SORT_DESCENDING);
    drain_main_context ();
    assert_account_root_order (selection, by_name, by_code);
    g_assert_true (gnc_tree_view_account_get_selected_account (view) == selected);
    assert_selected_ancestors_expanded (selection, 1);

    first_window = GTK_WINDOW (g_object_ref_sink (gtk_window_new ()));
    second_window = GTK_WINDOW (g_object_ref_sink (gtk_window_new ()));
    gtk_window_set_default_size (first_window, 640, 480);
    gtk_window_set_default_size (second_window, 640, 480);
    gtk_window_set_child (first_window, widget);
    present_and_wait_for_frame (first_window);
    gtk_window_set_child (first_window, NULL);
    gtk_window_set_child (second_window, widget);
    present_and_wait_for_frame (second_window);

    gtk_column_view_sort_by_column (column_view, name_column,
                                    GTK_SORT_DESCENDING);
    drain_main_context ();
    assert_account_root_order (selection, by_code, by_name);
    g_assert_true (gnc_tree_view_account_get_selected_account (view) == selected);

    gtk_window_set_child (second_window, NULL);
    gtk_window_destroy (first_window);
    gtk_window_destroy (second_window);
    g_object_unref (first_window);
    g_object_unref (second_window);

    g_object_unref (name_column);
    g_object_unref (code_column);
    g_object_run_dispose (G_OBJECT (widget));
    g_object_run_dispose (G_OBJECT (widget));
    gtk_sorter_changed (view_sorter, GTK_SORTER_CHANGE_DIFFERENT);
    g_object_unref (view_sorter);
    g_object_unref (widget);
    gnc_clear_current_session ();
}

static void
test_account_sort_rebuild_survives_dispose (void)
{
    QofSession *session = qof_session_new (qof_book_new ());
    QofBook *book = qof_session_get_book (session);
    Account *root = gnc_account_create_root (book);
    Account *first = xaccMallocAccount (book);
    Account *second = xaccMallocAccount (book);
    GtkWidget *widget;
    GncTreeViewAccount *view;
    GtkColumnView *column_view;
    GtkColumnViewColumn *code_column;
    GtkSelectionModel *selection;
    GtkTreeListModel *rows;
    GListModel *roots;
    DisposeOnSelectionChange dispose_context;
    gulong dispose_id;

    gnc_set_current_session (session);
    xaccAccountSetName (first, "Rebuild first");
    xaccAccountSetCode (first, "100");
    xaccAccountSetType (first, ACCT_TYPE_BANK);
    gnc_account_append_child (root, first);
    xaccAccountSetName (second, "Rebuild second");
    xaccAccountSetCode (second, "200");
    xaccAccountSetType (second, ACCT_TYPE_BANK);
    gnc_account_append_child (root, second);
    widget = gnc_tree_view_account_new_with_root (root, FALSE);
    g_object_ref_sink (widget);
    view = GNC_TREE_VIEW_ACCOUNT (widget);
    column_view = g_object_ref (gnc_tree_view_account_get_column_view (view));
    code_column = column_by_id (column_view, "account-code");
    selection = gnc_tree_view_account_get_selection_model (view);
    rows = GTK_TREE_LIST_MODEL (gtk_single_selection_get_model
                                (GTK_SINGLE_SELECTION (selection)));
    roots = g_object_ref (gtk_tree_list_model_get_model (rows));
    dispose_context.view = G_OBJECT (widget);
    dispose_context.invoked = FALSE;
    dispose_id = g_signal_connect (roots, "items-changed",
                                   G_CALLBACK (dispose_view_on_items_changed),
                                   &dispose_context);

    gtk_column_view_sort_by_column (column_view, code_column,
                                    GTK_SORT_DESCENDING);
    g_assert_true (dispose_context.invoked);
    g_signal_handler_disconnect (roots, dispose_id);
    g_object_run_dispose (G_OBJECT (widget));

    g_object_unref (roots);
    g_object_unref (code_column);
    g_object_unref (column_view);
    g_object_unref (widget);
    gnc_clear_current_session ();
}

static void
test_account_sort_restore_survives_dispose (void)
{
    QofSession *session = qof_session_new (qof_book_new ());
    QofBook *book = qof_session_get_book (session);
    Account *root = gnc_account_create_root (book);
    Account *parent = xaccMallocAccount (book);
    Account *selected = xaccMallocAccount (book);
    Account *other = xaccMallocAccount (book);
    GtkWidget *widget;
    GncTreeViewAccount *view;
    GtkColumnView *column_view;
    GtkColumnViewColumn *code_column;
    GtkSelectionModel *selection;
    DisposeOnSelectionChange dispose_context;
    gulong dispose_id;

    gnc_set_current_session (session);
    xaccAccountSetName (parent, "Restore parent");
    xaccAccountSetCode (parent, "100");
    xaccAccountSetType (parent, ACCT_TYPE_BANK);
    gnc_account_append_child (root, parent);
    xaccAccountSetName (selected, "Restore selected child");
    xaccAccountSetType (selected, ACCT_TYPE_BANK);
    gnc_account_append_child (parent, selected);
    xaccAccountSetName (other, "Restore other");
    xaccAccountSetCode (other, "200");
    xaccAccountSetType (other, ACCT_TYPE_BANK);
    gnc_account_append_child (root, other);
    widget = gnc_tree_view_account_new_with_root (root, FALSE);
    g_object_ref_sink (widget);
    view = GNC_TREE_VIEW_ACCOUNT (widget);
    column_view = g_object_ref (gnc_tree_view_account_get_column_view (view));
    code_column = column_by_id (column_view, "account-code");
    selection = g_object_ref (gnc_tree_view_account_get_selection_model (view));
    gnc_tree_view_account_set_selected_account (view, selected);
    drain_main_context ();
    g_assert_true (gnc_tree_view_account_get_selected_account (view) == selected);

    gtk_column_view_sort_by_column (column_view, code_column,
                                    GTK_SORT_DESCENDING);
    dispose_context.view = G_OBJECT (widget);
    dispose_context.invoked = FALSE;
    dispose_id = g_signal_connect (selection, "selection-changed",
                                   G_CALLBACK (dispose_view_on_selection_changed),
                                   &dispose_context);
    drain_main_context ();
    g_assert_true (dispose_context.invoked);
    g_signal_handler_disconnect (selection, dispose_id);
    g_object_run_dispose (G_OBJECT (widget));

    g_object_unref (selection);
    g_object_unref (code_column);
    g_object_unref (column_view);
    g_object_unref (widget);
    gnc_clear_current_session ();
}

static void
test_commodity_native_column_sorting (void)
{
    QofSession *session = qof_session_new (qof_book_new ());
    QofBook *book = qof_session_get_book (session);
    gnc_commodity_table *table = gnc_commodity_table_get_table (book);
    gnc_commodity *by_symbol;
    gnc_commodity *by_name;
    GtkWidget *widget;
    GncTreeViewCommodity *view;
    GtkColumnView *column_view;
    GtkColumnViewColumn *name_column;
    GtkColumnViewColumn *symbol_column;
    GtkSelectionModel *selection;
    GtkSorter *view_sorter;

    gnc_set_current_session (session);
    by_symbol = gnc_commodity_new (book, "Zulu by name", "SORT-CONTRACT",
                                   "AAA", "", 100);
    by_name = gnc_commodity_new (book, "Alpha by name", "SORT-CONTRACT",
                                 "ZZZ", "", 100);
    gnc_commodity_table_insert (table, by_symbol);
    gnc_commodity_table_insert (table, by_name);
    widget = gnc_tree_view_commodity_new (book, NULL);
    g_object_ref_sink (widget);
    view = GNC_TREE_VIEW_COMMODITY (widget);
    column_view = gnc_tree_view_commodity_get_column_view (view);
    selection = gnc_tree_view_commodity_get_selection_model (view);
    name_column = column_by_id (column_view, "name");
    symbol_column = column_by_id (column_view, "symbol");
    view_sorter = g_object_ref (gtk_column_view_get_sorter (column_view));
    gnc_tree_view_commodity_select_commodity (view, by_symbol);
    drain_main_context ();

    gtk_column_view_sort_by_column (column_view, name_column,
                                    GTK_SORT_ASCENDING);
    drain_main_context ();
    assert_commodity_order (selection, by_name, by_symbol);
    g_assert_true (gnc_tree_view_commodity_get_selected_commodity (view) == by_symbol);
    assert_selected_ancestors_expanded (selection, 1);
    gtk_column_view_sort_by_column (column_view, name_column,
                                    GTK_SORT_DESCENDING);
    drain_main_context ();
    assert_commodity_order (selection, by_symbol, by_name);
    g_assert_true (gnc_tree_view_commodity_get_selected_commodity (view) == by_symbol);
    assert_selected_ancestors_expanded (selection, 1);
    gtk_column_view_sort_by_column (column_view, symbol_column,
                                    GTK_SORT_ASCENDING);
    drain_main_context ();
    assert_commodity_order (selection, by_symbol, by_name);
    g_assert_true (gnc_tree_view_commodity_get_selected_commodity (view) == by_symbol);
    assert_selected_ancestors_expanded (selection, 1);
    gtk_column_view_sort_by_column (column_view, symbol_column,
                                    GTK_SORT_DESCENDING);
    drain_main_context ();
    assert_commodity_order (selection, by_name, by_symbol);
    g_assert_true (gnc_tree_view_commodity_get_selected_commodity (view) == by_symbol);
    assert_selected_ancestors_expanded (selection, 1);

    g_object_unref (name_column);
    g_object_unref (symbol_column);
    g_object_run_dispose (G_OBJECT (widget));
    g_object_run_dispose (G_OBJECT (widget));
    gtk_sorter_changed (view_sorter, GTK_SORTER_CHANGE_DIFFERENT);
    g_object_unref (view_sorter);
    g_object_unref (widget);
    gnc_clear_current_session ();
}

static void
test_price_native_column_sorting (void)
{
    QofSession *session = qof_session_new (qof_book_new ());
    QofBook *book = qof_session_get_book (session);
    gnc_commodity_table *table = gnc_commodity_table_get_table (book);
    gnc_commodity *currency;
    gnc_commodity *security;
    GNCPrice *older;
    GNCPrice *newer;
    GtkWidget *widget;
    GncTreeViewPrice *view;
    GtkColumnView *column_view;
    GtkColumnViewColumn *date_column;
    GtkColumnViewColumn *value_column;
    GtkSelectionModel *selection;
    GtkSorter *view_sorter;

    gnc_set_current_session (session);
    currency = gnc_commodity_new (book, "Sort currency",
                                  GNC_COMMODITY_NS_CURRENCY, "SOC", "", 100);
    security = gnc_commodity_new (book, "Sort security", "SORT-CONTRACT",
                                  "SOS", "", 1000);
    gnc_commodity_table_insert (table, currency);
    gnc_commodity_table_insert (table, security);
    older = gnc_price_create (book);
    gnc_price_begin_edit (older);
    gnc_price_set_commodity (older, security);
    gnc_price_set_currency (older, currency);
    gnc_price_set_time64 (older, 1);
    gnc_price_set_value (older, gnc_numeric_create (1, 1));
    gnc_price_commit_edit (older);
    g_assert_true (gnc_pricedb_add_price (gnc_pricedb_get_db (book), older));
    newer = gnc_price_create (book);
    gnc_price_begin_edit (newer);
    gnc_price_set_commodity (newer, security);
    gnc_price_set_currency (newer, currency);
    gnc_price_set_time64 (newer, 86401);
    gnc_price_set_value (newer, gnc_numeric_create (2, 1));
    gnc_price_commit_edit (newer);
    g_assert_true (gnc_pricedb_add_price (gnc_pricedb_get_db (book), newer));

    widget = gnc_tree_view_price_new (book, NULL);
    g_object_ref_sink (widget);
    view = GNC_TREE_VIEW_PRICE (widget);
    column_view = gnc_tree_view_price_get_column_view (view);
    selection = gnc_tree_view_price_get_selection_model (view);
    date_column = column_by_id (column_view, "date");
    value_column = column_by_id (column_view, "price");
    view_sorter = g_object_ref (gtk_column_view_get_sorter (column_view));
    gnc_tree_view_price_set_selected_price (view, older);
    drain_main_context ();

    gtk_column_view_sort_by_column (column_view, date_column,
                                    GTK_SORT_ASCENDING);
    drain_main_context ();
    assert_price_order (selection, newer, older);
    g_assert_true (gnc_tree_view_price_get_selected_price (view) == older);
    assert_selected_ancestors_expanded (selection, 2);
    gtk_column_view_sort_by_column (column_view, date_column,
                                    GTK_SORT_DESCENDING);
    drain_main_context ();
    assert_price_order (selection, older, newer);
    g_assert_true (gnc_tree_view_price_get_selected_price (view) == older);
    assert_selected_ancestors_expanded (selection, 2);
    gtk_column_view_sort_by_column (column_view, value_column,
                                    GTK_SORT_ASCENDING);
    drain_main_context ();
    assert_price_order (selection, older, newer);
    g_assert_true (gnc_tree_view_price_get_selected_price (view) == older);
    assert_selected_ancestors_expanded (selection, 2);
    gtk_column_view_sort_by_column (column_view, value_column,
                                    GTK_SORT_DESCENDING);
    drain_main_context ();
    assert_price_order (selection, newer, older);
    g_assert_true (gnc_tree_view_price_get_selected_price (view) == older);
    assert_selected_ancestors_expanded (selection, 2);

    g_object_unref (date_column);
    g_object_unref (value_column);
    g_object_run_dispose (G_OBJECT (widget));
    g_object_run_dispose (G_OBJECT (widget));
    gtk_sorter_changed (view_sorter, GTK_SORTER_CHANGE_DIFFERENT);
    g_object_unref (view_sorter);
    g_object_unref (widget);
    gnc_price_unref (older);
    gnc_price_unref (newer);
    gnc_clear_current_session ();
}

static void
test_owner_selection_survives_view_dispose (void)
{
    GtkWidget *widget = gnc_tree_view_owner_new (GNC_OWNER_CUSTOMER);
    GtkSelectionModel *selection;
    gboolean finalized = FALSE;

    g_object_ref_sink (widget);
    selection = g_object_ref (gnc_tree_view_owner_get_selection_model
                              (GNC_TREE_VIEW_OWNER (widget)));
    g_object_weak_ref (G_OBJECT (selection), object_finalized, &finalized);
    g_object_run_dispose (G_OBJECT (widget));
    g_object_run_dispose (G_OBJECT (widget));
    g_signal_emit_by_name (selection, "selection-changed", 0u, 0u);
    g_object_unref (widget);
    g_signal_emit_by_name (selection, "selection-changed", 0u, 0u);
    g_object_unref (selection);
    drain_main_context ();
    g_assert_true (finalized);
}

static void
test_query_selection_switch_disconnects_old_model (void)
{
    GtkWidget *widget = GTK_WIDGET (g_object_new (GNC_TYPE_QUERY_VIEW, NULL));
    GtkColumnView *column_view;
    GtkSelectionModel *old_selection;
    gboolean finalized = FALSE;
    guint emissions = 0;

    g_object_ref_sink (widget);
    column_view = find_column_view (widget);
    g_assert_nonnull (column_view);
    old_selection = g_object_ref (gtk_column_view_get_model (column_view));
    g_object_weak_ref (G_OBJECT (old_selection), object_finalized, &finalized);
    g_signal_connect (widget, "row-selected", G_CALLBACK (query_row_selected),
                      &emissions);
    gnc_query_view_set_selection_mode (GNC_QUERY_VIEW (widget),
                                       GTK_SELECTION_MULTIPLE);
    g_signal_emit_by_name (old_selection, "selection-changed", 0u, 0u);
    g_assert_cmpuint (emissions, ==, 0);
    g_object_run_dispose (G_OBJECT (widget));
    g_object_run_dispose (G_OBJECT (widget));
    g_signal_emit_by_name (old_selection, "selection-changed", 0u, 0u);
    g_assert_cmpuint (emissions, ==, 0);
    g_object_unref (widget);
    g_object_unref (old_selection);
    drain_main_context ();
    g_assert_true (finalized);
}

static void
assert_query_primary_sort (GNCQueryView *view, gboolean increasing)
{
    QofQuerySort *primary = NULL;

    qof_query_get_sorts (view->query, &primary, NULL, NULL);
    g_assert_nonnull (primary);
    g_assert_cmpint (qof_query_sort_get_increasing (primary), ==, increasing);
}

static void
test_query_native_column_sorting (void)
{
    QofSession *session = qof_session_new (qof_book_new ());
    QofBook *book = qof_session_get_book (session);
    GNCSearchParamSimple *memo_param = gnc_search_param_simple_new ();
    GNCSearchParamSimple *amount_param = gnc_search_param_simple_new ();
    GNCSearchParamSimple *passive_param = gnc_search_param_simple_new ();
    GSList *path = NULL;
    GList *params = NULL;
    Query *query = qof_query_create_for (GNC_ID_SPLIT);
    GtkWidget *widget;
    GNCQueryView *view;
    GtkColumnView *column_view;
    GListModel *columns;
    GtkColumnViewColumn *memo_column;
    GtkColumnViewColumn *amount_column;
    GtkColumnViewColumn *passive_column;
    GtkSorter *view_sorter;

    gnc_set_current_session (session);
    qof_query_set_book (query, book);

    path = g_slist_append (path, (gpointer)SPLIT_MEMO);
    gnc_search_param_set_param_path (memo_param, GNC_ID_SPLIT, path);
    g_slist_free (path);
    gnc_search_param_set_title (GNC_SEARCH_PARAM (memo_param), "Memo");

    path = g_slist_append (NULL, (gpointer)SPLIT_AMOUNT);
    gnc_search_param_set_param_path (amount_param, GNC_ID_SPLIT, path);
    g_slist_free (path);
    gnc_search_param_set_title (GNC_SEARCH_PARAM (amount_param), "Amount");

    path = g_slist_append (NULL, (gpointer)SPLIT_MEMO);
    gnc_search_param_set_param_path (passive_param, GNC_ID_SPLIT, path);
    g_slist_free (path);
    gnc_search_param_set_title (GNC_SEARCH_PARAM (passive_param), "Passive");
    gnc_search_param_set_passive (GNC_SEARCH_PARAM (passive_param), TRUE);

    params = g_list_append (params, memo_param);
    params = g_list_append (params, amount_param);
    params = g_list_append (params, passive_param);
    widget = gnc_query_view_new (params, query);
    g_object_ref_sink (widget);
    view = GNC_QUERY_VIEW (widget);
    column_view = find_column_view (widget);
    g_assert_nonnull (column_view);
    g_assert_null (gtk_column_view_get_header_factory (column_view));

    columns = gtk_column_view_get_columns (column_view);
    memo_column = g_list_model_get_item (columns, 0);
    amount_column = g_list_model_get_item (columns, 1);
    passive_column = g_list_model_get_item (columns, 2);
    g_assert_nonnull (gtk_column_view_column_get_sorter (memo_column));
    g_assert_nonnull (gtk_column_view_column_get_sorter (amount_column));
    g_assert_null (gtk_column_view_column_get_sorter (passive_column));
    g_assert_cmpint (gtk_sorter_compare
                     (gtk_column_view_column_get_sorter (memo_column),
                      memo_param, amount_param), ==, GTK_ORDERING_EQUAL);

    gtk_column_view_sort_by_column (column_view, memo_column,
                                    GTK_SORT_ASCENDING);
    g_assert_cmpint (view->sort_column, ==, 0);
    g_assert_true (view->increasing);
    assert_query_primary_sort (view, TRUE);
    gtk_column_view_sort_by_column (column_view, memo_column,
                                    GTK_SORT_DESCENDING);
    g_assert_false (view->increasing);
    assert_query_primary_sort (view, FALSE);

    gnc_query_view_set_numerics (view, FALSE, TRUE);
    gtk_column_view_sort_by_column (column_view, amount_column,
                                    GTK_SORT_ASCENDING);
    g_assert_cmpint (view->sort_column, ==, 1);
    g_assert_true (view->increasing);
    assert_query_primary_sort (view, FALSE);

    gnc_query_sort_order (view, 1, GTK_SORT_ASCENDING);
    view_sorter = g_object_ref (gtk_column_view_get_sorter (column_view));
    g_assert_true (gtk_column_view_sorter_get_primary_sort_column
                   (GTK_COLUMN_VIEW_SORTER (view_sorter)) == memo_column);
    g_assert_cmpint (gtk_column_view_sorter_get_primary_sort_order
                     (GTK_COLUMN_VIEW_SORTER (view_sorter)), ==,
                     GTK_SORT_ASCENDING);
    g_assert_true (view->increasing);
    assert_query_primary_sort (view, TRUE);

    g_object_run_dispose (G_OBJECT (widget));
    g_object_run_dispose (G_OBJECT (widget));
    gtk_sorter_changed (view_sorter, GTK_SORTER_CHANGE_DIFFERENT);
    g_object_unref (view_sorter);
    g_object_unref (memo_column);
    g_object_unref (amount_column);
    g_object_unref (passive_column);
    g_object_unref (widget);
    qof_query_destroy (query);
    g_list_free_full (params, g_object_unref);
    gnc_clear_current_session ();
}

int
main (int argc, char **argv)
{
    int status;

    g_setenv ("GSETTINGS_BACKEND", "memory", TRUE);
    g_test_init (&argc, &argv, NULL);
    gtk_init ();
    qof_log_init_filename_special ("stderr");
    qof_log_set_level ("gnc", (QofLogLevel)G_LOG_LEVEL_DEBUG);
    gnc_engine_init_static (argc, argv);
    gnc_prefs_init ();
    gnc_component_manager_init ();

    g_test_add_func ("/gnome-utils/tree-view-row-ownership/account",
                     test_account_lookup_releases_tree_item);
    g_test_add_func ("/gnome-utils/tree-view-row-ownership/account-dispose-row-callback",
                     test_account_dispose_quiesces_retained_row);
    g_test_add_func ("/gnome-utils/tree-view-row-ownership/account-column-owners",
                     test_account_column_owners_outlive_disposed_view);
    g_test_add_func ("/gnome-utils/tree-view-row-ownership/account-selection-modes",
                     test_account_selection_modes_preserve_semantics);
    g_test_add_func ("/gnome-utils/tree-view-row-ownership/account-model-root-ownership",
                     test_account_model_owns_replaced_root);
    g_test_add_func ("/gnome-utils/tree-view-row-ownership/account-native-sorting",
                     test_account_native_column_sorting);
    g_test_add_func ("/gnome-utils/tree-view-row-ownership/account-sort-rebuild-dispose",
                     test_account_sort_rebuild_survives_dispose);
    g_test_add_func ("/gnome-utils/tree-view-row-ownership/account-sort-restore-dispose",
                     test_account_sort_restore_survives_dispose);
    g_test_add_func ("/gnome-utils/tree-view-row-ownership/commodity",
                     test_commodity_lookup_releases_tree_item);
    g_test_add_func ("/gnome-utils/tree-view-row-ownership/commodity-column-owners",
                     test_commodity_column_owners_outlive_disposed_view);
    g_test_add_func ("/gnome-utils/tree-view-row-ownership/commodity-selection-contract",
                     test_commodity_selection_restore_contract);
    g_test_add_func ("/gnome-utils/tree-view-row-ownership/commodity-native-sorting",
                     test_commodity_native_column_sorting);
    g_test_add_func ("/gnome-utils/tree-view-row-ownership/price",
                     test_price_lookup_releases_tree_item);
    g_test_add_func ("/gnome-utils/tree-view-row-ownership/price-column-owners",
                     test_price_column_owners_outlive_disposed_view);
    g_test_add_func ("/gnome-utils/tree-view-row-ownership/price-selection-contract",
                     test_price_selection_restore_contract);
    g_test_add_func ("/gnome-utils/tree-view-row-ownership/price-native-sorting",
                     test_price_native_column_sorting);
    g_test_add_func ("/gnome-utils/tree-view-row-ownership/owner",
                     test_owner_selection_survives_view_dispose);
    g_test_add_func ("/gnome-utils/tree-view-row-ownership/query",
                     test_query_selection_switch_disconnects_old_model);
    g_test_add_func ("/gnome-utils/tree-view-row-ownership/query-native-sorting",
                     test_query_native_column_sorting);
    status = g_test_run ();

    gnc_component_manager_shutdown ();
    gnc_prefs_remove_registered ();
    gnc_engine_shutdown ();
    return status;
}
