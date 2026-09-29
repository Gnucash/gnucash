/* Copyright (C) 2026 GnuCash contributors
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */

#include <config.h>
#include <gtk/gtk.h>

#include "cashobjects.h"
#include "gnc-component-manager.h"
#include "gnc-gsettings.h"
#include "gnc-session.h"
#include "gncTaxTable.h"
#include "qof.h"
#include "qofevent.h"
extern "C"
{
#include "dialog-tax-table.h"
}

namespace
{
gboolean display_available;
QofBook *test_book;
QofSession *test_session;
GncGUID test_table_guid;
GtkWidget *tax_table_window;
GncGUID monitored_table_guid;
guint table_modify_count;

GtkWidget *
find_buildable (GtkWidget *root, const char *name)
{
    if (GTK_IS_BUILDABLE (root) &&
        g_strcmp0 (gtk_buildable_get_name (GTK_BUILDABLE (root)), name) == 0)
        return root;
    if (!GTK_IS_CONTAINER (root))
        return nullptr;

    auto children = gtk_container_get_children (GTK_CONTAINER (root));
    GtkWidget *result = nullptr;
    for (auto node = children; node && !result; node = node->next)
        result = find_buildable (GTK_WIDGET (node->data), name);
    g_list_free (children);
    return result;
}

GtkWidget *
find_tax_table_window ()
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *result = nullptr;
    for (auto node = windows; node; node = node->next)
    {
        auto widget = GTK_WIDGET (node->data);
        if (g_strcmp0 (gtk_widget_get_name (widget),
                       "gnc-id-new-tax-table") == 0)
        {
            g_assert_null (result);
            result = widget;
        }
    }
    g_list_free (windows);
    return result;
}

GtkWidget *
find_entry (GtkWidget *root);

GtkWidget *
find_rename_dialog (GtkWidget *parent)
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *result = nullptr;
    for (auto node = windows; node; node = node->next)
    {
        auto widget = GTK_WIDGET (node->data);
        if (GTK_IS_DIALOG (widget) &&
            gtk_window_get_transient_for (GTK_WINDOW (widget)) ==
            GTK_WINDOW (parent) && find_entry (widget))
        {
            g_assert_null (result);
            result = widget;
        }
    }
    g_list_free (windows);
    return result;
}

GtkWidget *
find_entry (GtkWidget *root)
{
    if (GTK_IS_ENTRY (root))
        return root;
    if (!GTK_IS_CONTAINER (root))
        return nullptr;
    auto children = gtk_container_get_children (GTK_CONTAINER (root));
    GtkWidget *result = nullptr;
    for (auto node = children; node && !result; node = node->next)
        result = find_entry (GTK_WIDGET (node->data));
    g_list_free (children);
    return result;
}

void
destroy_tax_table_parent (GtkWidget *, gpointer user_data)
{
    gtk_widget_destroy (GTK_WIDGET (user_data));
}

void
destroy_current_session (GtkWidget *, gpointer)
{
    gnc_close_gui_component_by_session (test_session);
    gnc_clear_current_session ();
    test_session = nullptr;
}

void
count_table_modification (QofInstance *entity, QofEventId event_type,
                          gpointer user_data, gpointer event_data)
{
    if ((event_type & QOF_EVENT_MODIFY) &&
        guid_equal (qof_instance_get_guid (entity), &monitored_table_guid))
        ++table_modify_count;
}

void
select_tax_table (GtkWidget *window)
{
    auto view = find_buildable (window, "tax_tables_view");
    g_assert_true (GTK_IS_TREE_VIEW (view));
    auto model = gtk_tree_view_get_model (GTK_TREE_VIEW (view));
    GtkTreeIter iter;
    g_assert_true (gtk_tree_model_get_iter_first (model, &iter));
    auto path = gtk_tree_model_get_path (model, &iter);
    gtk_tree_selection_select_path (
        gtk_tree_view_get_selection (GTK_TREE_VIEW (view)), path);
    gtk_tree_path_free (path);
}

void
test_public_rename_and_parent_destroy ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }

    test_book = qof_book_new ();
    test_session = qof_session_new (test_book);
    gnc_set_current_session (test_session);
    auto table = gncTaxTableCreate (test_book);
    gncTaxTableSetName (table, "Tax table before response");
    test_table_guid = *gncTaxTableGetGUID (table);

    auto owner = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    gtk_widget_realize (owner);
    g_assert_nonnull (gnc_ui_tax_table_window_new (GTK_WINDOW (owner), test_book));
    tax_table_window = find_tax_table_window ();
    g_assert_nonnull (tax_table_window);
    select_tax_table (tax_table_window);
    auto rename_button = find_buildable (tax_table_window,
                                         "rename_table_button");
    g_assert_true (GTK_IS_BUTTON (rename_button));

    gtk_button_clicked (GTK_BUTTON (rename_button));
    auto dialog = find_rename_dialog (tax_table_window);
    g_assert_true (GTK_IS_DIALOG (dialog));
    g_assert_true (gtk_window_get_modal (GTK_WINDOW (dialog)));
    g_assert_true (gtk_window_get_destroy_with_parent (GTK_WINDOW (dialog)));
    auto entry = find_entry (dialog);
    g_assert_true (GTK_IS_ENTRY (entry));
    gtk_entry_set_text (GTK_ENTRY (entry), "Renamed asynchronously");
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    table = gncTaxTableLookup (test_book, &test_table_guid);
    g_assert_nonnull (table);
    g_assert_cmpstr (gncTaxTableGetName (table), ==,
                     "Renamed asynchronously");
    g_assert_null (find_rename_dialog (tax_table_window));

    gtk_button_clicked (GTK_BUTTON (rename_button));
    dialog = find_rename_dialog (tax_table_window);
    g_assert_true (GTK_IS_DIALOG (dialog));
    entry = find_entry (dialog);
    gtk_entry_set_text (GTK_ENTRY (entry), "Must not be applied");
    g_signal_connect (dialog, "destroy",
                      G_CALLBACK (destroy_tax_table_parent), tax_table_window);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);

    table = gncTaxTableLookup (test_book, &test_table_guid);
    g_assert_nonnull (table);
    g_assert_cmpstr (gncTaxTableGetName (table), ==,
                     "Renamed asynchronously");
    g_assert_null (find_tax_table_window ());
    tax_table_window = nullptr;

    /* Repeat with a live prompt, but destroy the original session from the
       prompt's destroy signal. The table is then gone, so the engine event
       counter proves that no rename was committed before shutdown completed. */
    gnc_clear_current_session ();
    test_book = qof_book_new ();
    test_session = qof_session_new (test_book);
    gnc_set_current_session (test_session);
    table = gncTaxTableCreate (test_book);
    gncTaxTableSetName (table, "Tax table before session close");
    monitored_table_guid = *gncTaxTableGetGUID (table);
    auto event_handler = qof_event_register_handler (
        count_table_modification, nullptr);
    table_modify_count = 0;

    g_assert_nonnull (gnc_ui_tax_table_window_new (GTK_WINDOW (owner), test_book));
    tax_table_window = find_tax_table_window ();
    g_assert_nonnull (tax_table_window);
    select_tax_table (tax_table_window);
    rename_button = find_buildable (tax_table_window, "rename_table_button");
    gtk_button_clicked (GTK_BUTTON (rename_button));
    dialog = find_rename_dialog (tax_table_window);
    g_assert_true (GTK_IS_DIALOG (dialog));
    entry = find_entry (dialog);
    gtk_entry_set_text (GTK_ENTRY (entry), "Must not survive session close");
    g_signal_connect (dialog, "destroy",
                      G_CALLBACK (destroy_current_session), nullptr);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    qof_event_unregister_handler (event_handler);
    g_assert_cmpuint (table_modify_count, ==, 0);
    g_assert_false (gnc_current_session_exist ());
    g_assert_null (find_tax_table_window ());
    tax_table_window = nullptr;
    gtk_widget_destroy (owner);
}
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
    g_test_add_func ("/gnome/tax-table/rename-response-parent-destroy",
                     test_public_rename_and_parent_destroy);
    auto result = g_test_run ();
    gnc_gsettings_shutdown ();
    gnc_component_manager_shutdown ();
    gnc_clear_current_session ();
    qof_close ();
    return result;
}
