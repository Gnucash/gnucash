/* Copyright (C) 2026 GnuCash contributors
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 */

#include <config.h>

#include <gtk/gtk.h>

#include "cashobjects.h"
#include "gnc-commodity.h"
#include "gnc-component-manager.h"
#include "gnc-gsettings.h"
#include "gnc-session.h"
#include "gnc-tree-model-commodity.h"
#include "gnc-tree-view-commodity.h"
#include "qof.h"
#include "gnc-ui.h"

static gboolean display_available;

static GtkWidget *
find_buildable (GtkWidget *widget, const gchar *name)
{
    if (GTK_IS_BUILDABLE (widget) &&
        g_strcmp0 (gtk_buildable_get_name (GTK_BUILDABLE (widget)), name) == 0)
        return widget;
    if (!GTK_IS_CONTAINER (widget))
        return nullptr;
    auto children = gtk_container_get_children (GTK_CONTAINER (widget));
    GtkWidget *found = nullptr;
    for (auto node = children; node && !found; node = node->next)
        found = find_buildable (GTK_WIDGET (node->data), name);
    g_list_free (children);
    return found;
}

static GtkWidget *
find_window_named (const gchar *name)
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *found = nullptr;
    for (auto node = windows; node && !found; node = node->next)
    {
        auto widget = GTK_WIDGET (node->data);
        if (g_strcmp0 (gtk_widget_get_name (widget), name) == 0)
            found = widget;
    }
    g_list_free (windows);
    return found;
}

static GtkWidget *
find_commodity_view (GtkWidget *widget)
{
    if (GNC_IS_TREE_VIEW_COMMODITY (widget))
        return widget;
    if (!GTK_IS_CONTAINER (widget))
        return nullptr;
    auto children = gtk_container_get_children (GTK_CONTAINER (widget));
    GtkWidget *found = nullptr;
    for (auto node = children; node && !found; node = node->next)
        found = find_commodity_view (GTK_WIDGET (node->data));
    g_list_free (children);
    return found;
}

static gboolean
select_namespace (GncTreeViewCommodity *view,
                  gnc_commodity_namespace *target)
{
    auto model = gtk_tree_view_get_model (GTK_TREE_VIEW (view));
    auto selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (view));
    GtkTreeIter iter;
    if (!gtk_tree_model_get_iter_first (model, &iter))
        return FALSE;
    do
    {
        auto path = gtk_tree_model_get_path (model, &iter);
        gtk_tree_selection_select_path (selection, path);
        gtk_tree_path_free (path);
        if (gnc_tree_view_commodity_get_selected_namespace (view) == target)
            return TRUE;
    }
    while (gtk_tree_model_iter_next (model, &iter));
    return FALSE;
}

static void
test_rename_response_and_parent_destroy ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }

    auto book = qof_book_new ();
    auto table = gnc_commodity_table_get_table (book);
    gnc_commodity_table_add_namespace (table, "OLDNS", book);
    gnc_commodity_table_add_namespace (table, "EXISTS", book);
    gnc_commodity_table_insert (
        table, gnc_commodity_new (book, "Old namespace test", "OLDNS", "OLD",
                                  nullptr, 100));
    gnc_commodity_table_insert (
        table, gnc_commodity_new (book, "Existing namespace test", "EXISTS",
                                  "EXS", nullptr, 100));
    gnc_set_current_session (qof_session_new (book));

    auto parent = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    gtk_widget_realize (parent);
    gnc_commodities_dialog (parent);
    auto commodities_window = find_window_named ("gnc-id-commodity");
    g_assert_true (GTK_IS_WINDOW (commodities_window));
    auto view = GNC_TREE_VIEW_COMMODITY (find_commodity_view (commodities_window));
    g_assert_true (GNC_IS_TREE_VIEW_COMMODITY (view));
    auto original_namespace = gnc_commodity_table_find_namespace (table, "OLDNS");
    g_assert_true (select_namespace (view, original_namespace));
    auto rename_button = find_buildable (commodities_window, "rename_namespace_button");
    g_assert_true (GTK_IS_BUTTON (rename_button));

    gtk_button_clicked (GTK_BUTTON (rename_button));
    auto dialog = find_window_named ("gnc-id-rename-namespace");
    g_assert_true (GTK_IS_DIALOG (dialog));
    g_assert_true (gtk_window_get_modal (GTK_WINDOW (dialog)));
    g_assert_true (gtk_window_get_destroy_with_parent (GTK_WINDOW (dialog)));
    auto entry = GTK_ENTRY (find_buildable (dialog, "rename_entry"));
    auto label = GTK_LABEL (find_buildable (dialog, "rename_label"));
    g_assert_true (GTK_IS_ENTRY (entry));
    g_assert_true (GTK_IS_LABEL (label));

    gtk_entry_set_text (entry, "");
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    g_assert_true (GTK_IS_DIALOG (find_window_named ("gnc-id-rename-namespace")));
    g_assert_cmpstr (gtk_label_get_text (label), ==, "No new name");

    gtk_entry_set_text (entry, "EXISTS");
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    g_assert_true (GTK_IS_DIALOG (find_window_named ("gnc-id-rename-namespace")));

    gtk_entry_set_text (entry, "NEWNS");
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    g_assert_true (gnc_commodity_table_find_namespace (table, "NEWNS") ==
                   original_namespace);
    g_assert_null (gnc_commodity_table_find_namespace (table, "OLDNS"));
    g_assert_null (find_window_named ("gnc-id-rename-namespace"));

    g_assert_true (select_namespace (view, original_namespace));
    gtk_button_clicked (GTK_BUTTON (rename_button));
    dialog = find_window_named ("gnc-id-rename-namespace");
    g_assert_true (GTK_IS_DIALOG (dialog));
    g_object_ref (dialog);
    gtk_widget_destroy (commodities_window);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    g_assert_null (find_window_named ("gnc-id-rename-namespace"));
    g_object_unref (dialog);
    gtk_widget_destroy (parent);
}

int
main (int argc, char **argv)
{
    g_test_init (&argc, &argv, nullptr);
    display_available = gtk_init_check (&argc, &argv);
    if (g_getenv ("GNC_REQUIRE_DISPLAY"))
        g_assert_true (display_available);
    qof_init ();
    g_assert_true (cashobjects_register ());
    gnc_component_manager_init ();
    gnc_gsettings_load_backend ();
    g_test_add_func ("/gnome/commodities/namespace-response-parent-destroy",
                     test_rename_response_and_parent_destroy);
    auto result = g_test_run ();
    gnc_gsettings_shutdown ();
    gnc_component_manager_shutdown ();
    gnc_clear_current_session ();
    qof_close ();
    return result;
}
