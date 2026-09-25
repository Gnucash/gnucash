/* test-tree-view-column-ownership.c -- Tree view column ownership tests.
 *
 * This program is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 */

#include <config.h>

#include <gtk/gtk.h>

#include "gnc-component-manager.h"
#include "gnc-engine.h"
#include "gnc-prefs-utils.h"
#include "gnc-session.h"
#include "gnc-sx-instance-model.h"
#include "gnc-tree-view-owner.h"
#include "gnc-tree-view-sx-list.h"
#include "qof.h"

#define OWNER_COLUMN_COUNT 16
#define SX_LIST_COLUMN_COUNT 6

typedef struct
{
    GWeakRef columns[OWNER_COLUMN_COUNT];
    GWeakRef factories[OWNER_COLUMN_COUNT];
    guint count;
} ColumnWatch;

static void
drain_main_context (void)
{
    while (g_main_context_pending (NULL))
        g_main_context_iteration (NULL, FALSE);
}

static void
watch_columns_and_factories (GtkColumnView *view, guint expected_count,
                             ColumnWatch *watch)
{
    GListModel *model = gtk_column_view_get_columns (view);

    g_assert_cmpuint (expected_count, <=, OWNER_COLUMN_COUNT);
    g_assert_cmpuint (g_list_model_get_n_items (model), ==, expected_count);
    for (guint index = 0; index < expected_count; index++)
    {
        GtkColumnViewColumn *column = GTK_COLUMN_VIEW_COLUMN (
            g_list_model_get_item (model, index));
        GtkListItemFactory *factory;

        g_assert_nonnull (column);
        factory = gtk_column_view_column_get_factory (column);
        if (!factory)
        {
            g_object_unref (column);
            g_assert_not_reached ();
        }
        g_weak_ref_init (&watch->columns[index], G_OBJECT (column));
        g_weak_ref_init (&watch->factories[index], G_OBJECT (factory));
        g_object_unref (column);
    }
    watch->count = expected_count;
}

static void
assert_watch_object_alive (GWeakRef *weak_ref)
{
    GObject *object = g_weak_ref_get (weak_ref);
    gboolean alive = object != NULL;

    g_clear_object (&object);
    g_assert_true (alive);
}

static void
assert_watched_objects_finalized (ColumnWatch *watch)
{
    gboolean all_finalized = TRUE;

    for (guint index = 0; index < watch->count; index++)
    {
        GObject *column = g_weak_ref_get (&watch->columns[index]);
        GObject *factory = g_weak_ref_get (&watch->factories[index]);

        all_finalized = all_finalized && !column && !factory;
        g_clear_object (&column);
        g_clear_object (&factory);
        g_weak_ref_clear (&watch->columns[index]);
        g_weak_ref_clear (&watch->factories[index]);
    }
    watch->count = 0;
    g_assert_true (all_finalized);
}

static void
test_owner_columns_release_on_view_finalize (void)
{
    QofSession *session = qof_session_new (qof_book_new ());
    GtkWidget *widget;
    ColumnWatch watch = { 0 };

    gnc_set_current_session (session);
    widget = gnc_tree_view_owner_new (GNC_OWNER_CUSTOMER);
    g_assert_nonnull (widget);
    g_object_ref_sink (widget);
    watch_columns_and_factories
        (GTK_COLUMN_VIEW (gtk_widget_get_first_child (widget)),
         OWNER_COLUMN_COUNT, &watch);

    g_object_unref (widget);
    drain_main_context ();
    assert_watched_objects_finalized (&watch);
    gnc_clear_current_session ();
}

static void
test_sx_list_columns_release_on_view_finalize (void)
{
    QofSession *session = qof_session_new (qof_book_new ());
    GncSxInstanceModel *instances;
    GtkColumnView *view;
    GtkColumnViewColumn *enabled_column;
    ColumnWatch watch = { 0 };

    gnc_set_current_session (session);
    instances = gnc_sx_get_current_instances ();
    g_assert_nonnull (instances);
    view = gnc_sx_list_view_new (instances);
    g_assert_nonnull (view);
    g_object_ref_sink (view);
    watch_columns_and_factories (view, SX_LIST_COLUMN_COUNT, &watch);

    enabled_column = GTK_COLUMN_VIEW_COLUMN (g_weak_ref_get (&watch.columns[1]));
    g_assert_nonnull (enabled_column);
    gtk_column_view_remove_column (view, enabled_column);
    g_object_unref (enabled_column);
    /* enabled_column is held by GncSxListViewData until the view is finalized. */
    assert_watch_object_alive (&watch.columns[1]);

    g_object_unref (view);
    drain_main_context ();
    assert_watched_objects_finalized (&watch);
    g_object_unref (instances);
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
    g_test_add_func ("/gnome-utils/tree-view/owner-columns-release-on-finalize",
                     test_owner_columns_release_on_view_finalize);
    g_test_add_func ("/gnome-utils/tree-view/sx-list-columns-release-on-finalize",
                     test_sx_list_columns_release_on_view_finalize);
    status = g_test_run ();
    gnc_component_manager_shutdown ();
    gnc_prefs_remove_registered ();
    gnc_engine_shutdown ();
    return status;
}
