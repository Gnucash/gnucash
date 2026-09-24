/*
 * test-dialog-tax-table-column-ownership.c -- tax table column ownership
 *
 * This program is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 */

#include <config.h>

#include <gtk/gtk.h>

#include "dialog-tax-table.h"
#include "gnc-component-manager.h"
#include "gnc-engine.h"
#include "gnc-prefs-utils.h"
#include "gnc-session.h"
#include "qof.h"

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

static void
collect_column_views (GtkWidget *widget, GPtrArray *views)
{
    if (GTK_IS_COLUMN_VIEW (widget))
        g_ptr_array_add (views, widget);
    for (GtkWidget *child = gtk_widget_get_first_child (widget); child;
         child = gtk_widget_get_next_sibling (child))
        collect_column_views (child, views);
}

static GtkWindow *
find_tax_table_window (void)
{
    GListModel *toplevels = gtk_window_get_toplevels ();

    for (guint index = 0; index < g_list_model_get_n_items (toplevels); index++)
    {
        GtkWindow *window = GTK_WINDOW (g_list_model_get_item (toplevels, index));
        GPtrArray *views = g_ptr_array_new ();

        collect_column_views (GTK_WIDGET (window), views);
        if (views->len == 2)
        {
            g_ptr_array_unref (views);
            return window;
        }
        g_ptr_array_unref (views);
        g_object_unref (window);
    }
    return NULL;
}

static guint
watch_columns_and_factories (GtkWindow *window, gboolean columns[3],
                             gboolean factories[3])
{
    GPtrArray *views = g_ptr_array_new ();
    guint watched = 0;

    collect_column_views (GTK_WIDGET (window), views);
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
            GtkListItemFactory *factory = gtk_column_view_column_get_factory (column);

            g_assert_cmpuint (watched, <, 3);
            g_assert_nonnull (factory);
            g_object_weak_ref (G_OBJECT (column), object_finalized,
                               &columns[watched]);
            g_object_weak_ref (G_OBJECT (factory), object_finalized,
                               &factories[watched]);
            g_object_unref (column);
            watched++;
        }
    }
    g_ptr_array_unref (views);
    return watched;
}

static void
test_tax_table_columns_release_on_close (void)
{
    QofSession *session;
    QofBook *book;
    TaxTableWindow *tax_table;
    GtkWindow *window;
    gboolean window_finalized = FALSE;
    gboolean columns_finalized[3] = { FALSE, FALSE, FALSE };
    gboolean factories_finalized[3] = { FALSE, FALSE, FALSE };

    session = qof_session_new (qof_book_new ());
    book = qof_session_get_book (session);
    gnc_set_current_session (session);
    tax_table = gnc_ui_tax_table_window_new (NULL, book);
    g_assert_nonnull (tax_table);
    window = find_tax_table_window ();
    g_assert_nonnull (window);
    g_assert_cmpuint (watch_columns_and_factories (window, columns_finalized,
                                                    factories_finalized), ==, 3);

    g_object_weak_ref (G_OBJECT (window), object_finalized, &window_finalized);
    gtk_window_destroy (window);
    g_object_unref (window);
    drain_main_context ();
    g_assert_true (window_finalized);
    for (guint index = 0; index < 3; index++)
    {
        g_assert_true (columns_finalized[index]);
        g_assert_true (factories_finalized[index]);
    }
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
    g_test_add_func ("/gnome-utils/tax-table/columns-release-on-close",
                     test_tax_table_columns_release_on_close);
    status = g_test_run ();
    gnc_component_manager_shutdown ();
    gnc_prefs_remove_registered ();
    gnc_engine_shutdown ();
    return status;
}
