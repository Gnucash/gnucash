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

/* test-gnc-tree-view.c -- GTK4 ColumnView preference integration tests. */

#include <config.h>

#include <gtk/gtk.h>

#include "gnc-prefs-p.h"
#include "gnc-tree-view.h"

#define GNC_PREFS_GROUP_GENERAL "general"
#define GNC_PREF_GRID_LINES_HORIZONTAL "grid-lines-horizontal"
#define GNC_PREF_GRID_LINES_VERTICAL "grid-lines-vertical"

typedef void (*PrefsCallback) (gpointer prefs, gchar *pref_name,
                                gpointer user_data);

typedef struct
{
    gboolean horizontal;
    gboolean vertical;
    PrefsCallback horizontal_callback;
    PrefsCallback vertical_callback;
    gpointer horizontal_data;
    gpointer vertical_data;
} TestPrefs;

static TestPrefs prefs;

static gulong
register_callback (const char *group, const gchar *pref_name, gpointer func,
                   gpointer user_data)
{
    g_assert_cmpstr (group, ==, GNC_PREFS_GROUP_GENERAL);
    if (g_strcmp0 (pref_name, GNC_PREF_GRID_LINES_HORIZONTAL) == 0)
    {
        prefs.horizontal_callback = (PrefsCallback)func;
        prefs.horizontal_data = user_data;
        return 1;
    }
    g_assert_cmpstr (pref_name, ==, GNC_PREF_GRID_LINES_VERTICAL);
    prefs.vertical_callback = (PrefsCallback)func;
    prefs.vertical_data = user_data;
    return 2;
}

static void
remove_callback (const gchar *group, const gchar *pref_name, gpointer func,
                 gpointer user_data)
{
    g_assert_cmpstr (group, ==, GNC_PREFS_GROUP_GENERAL);
    if (g_strcmp0 (pref_name, GNC_PREF_GRID_LINES_HORIZONTAL) == 0)
    {
        if (prefs.horizontal_callback == (PrefsCallback)func &&
            prefs.horizontal_data == user_data)
        {
            prefs.horizontal_callback = NULL;
            prefs.horizontal_data = NULL;
        }
        return;
    }
    g_assert_cmpstr (pref_name, ==, GNC_PREF_GRID_LINES_VERTICAL);
    if (prefs.vertical_callback == (PrefsCallback)func &&
        prefs.vertical_data == user_data)
    {
        prefs.vertical_callback = NULL;
        prefs.vertical_data = NULL;
    }
}

static gboolean
get_bool (const gchar *group, const gchar *pref_name)
{
    g_assert_cmpstr (group, ==, GNC_PREFS_GROUP_GENERAL);
    if (g_strcmp0 (pref_name, GNC_PREF_GRID_LINES_HORIZONTAL) == 0)
        return prefs.horizontal;
    g_assert_cmpstr (pref_name, ==, GNC_PREF_GRID_LINES_VERTICAL);
    return prefs.vertical;
}

static void
emit_preference (const gchar *pref_name)
{
    if (g_strcmp0 (pref_name, GNC_PREF_GRID_LINES_HORIZONTAL) == 0)
        prefs.horizontal_callback (NULL, (gchar *)pref_name, prefs.horizontal_data);
    else
        prefs.vertical_callback (NULL, (gchar *)pref_name, prefs.vertical_data);
}

static void
test_grid_line_preferences (void)
{
    PrefsBackend backend = { 0 };
    PrefsBackend *saved_backend = prefsbackend;
    GncTreeView *tree_view;
    GtkColumnView *column_view;

    backend.register_cb = register_callback;
    backend.remove_cb_by_func = remove_callback;
    backend.get_bool = get_bool;
    prefsbackend = &backend;
    prefs.horizontal = FALSE;
    prefs.vertical = FALSE;

    tree_view = g_object_new (GNC_TYPE_TREE_VIEW, NULL);
    g_object_ref_sink (tree_view);
    column_view = gnc_tree_view_get_column_view (tree_view);
    g_assert_false (gtk_column_view_get_show_row_separators (column_view));
    g_assert_false (gtk_column_view_get_show_column_separators (column_view));
    g_assert_nonnull (prefs.horizontal_callback);
    g_assert_nonnull (prefs.vertical_callback);

    prefs.horizontal = TRUE;
    emit_preference (GNC_PREF_GRID_LINES_HORIZONTAL);
    g_assert_true (gtk_column_view_get_show_row_separators (column_view));
    g_assert_false (gtk_column_view_get_show_column_separators (column_view));

    prefs.vertical = TRUE;
    emit_preference (GNC_PREF_GRID_LINES_VERTICAL);
    g_assert_true (gtk_column_view_get_show_row_separators (column_view));
    g_assert_true (gtk_column_view_get_show_column_separators (column_view));

    g_object_run_dispose (G_OBJECT (tree_view));
    g_assert_null (prefs.horizontal_callback);
    g_assert_null (prefs.vertical_callback);
    g_object_unref (tree_view);
    prefsbackend = saved_backend;
}

int
main (int argc, char **argv)
{
    g_test_init (&argc, &argv, NULL);
    gtk_init ();
    g_test_add_func ("/gnome-utils/tree-view/grid-line-preferences",
                     test_grid_line_preferences);
    return g_test_run ();
}
