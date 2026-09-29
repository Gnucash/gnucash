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
#include "dialog-commodity.h"
#include "gnc-session.h"
#include "gnc-ui.h"
#include "qof.h"

static gboolean display_available;

struct Completion
{
    guint calls{};
    QofBook *book{};
    gnc_commodity *commodity{};
};

static void
completed (QofBook *book, gnc_commodity *commodity, gpointer data)
{
    auto result = static_cast<Completion*>(data);
    ++result->calls;
    result->book = book;
    result->commodity = commodity;
}

static GtkDialog *
find_child_dialog (GtkWindow *parent)
{
    auto windows = gtk_window_list_toplevels ();
    GtkDialog *found = nullptr;
    for (auto node = windows; node && !found; node = node->next)
    {
        auto window = GTK_WINDOW (node->data);
        if (GTK_IS_DIALOG (window) &&
            gtk_window_get_transient_for (window) == parent)
            found = GTK_DIALOG (window);
    }
    g_list_free (windows);
    return found;
}

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

static void
destroy_parent_on_picker_change ([[maybe_unused]] GtkComboBox *picker,
                                 GtkWidget *parent)
{
    gtk_widget_destroy (parent);
}

static void
test_new_cancel_and_parent_destroy ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }

    auto book = qof_book_new ();
    gnc_set_current_session (qof_session_new (book));
    auto parent = GTK_WINDOW (gtk_window_new (GTK_WINDOW_TOPLEVEL));
    Completion result;
    gnc_ui_new_commodity_async_full ("NYSE", GTK_WIDGET (parent), nullptr,
                                     nullptr, nullptr, nullptr, 100,
                                     completed, &result);
    auto dialog = find_child_dialog (parent);
    g_assert_true (GTK_IS_DIALOG (dialog));
    g_assert_true (gtk_window_get_modal (GTK_WINDOW (dialog)));
    g_assert_true (gtk_window_get_destroy_with_parent (GTK_WINDOW (dialog)));

    /* Help opens the external manual viewer, so it remains an E2E check. */
    g_object_ref (dialog);
    gtk_dialog_response (dialog, GTK_RESPONSE_CANCEL);
    g_assert_cmpuint (result.calls, ==, 1);
    g_assert_null (result.book);
    g_assert_null (result.commodity);
    gtk_dialog_response (dialog, GTK_RESPONSE_OK);
    g_assert_cmpuint (result.calls, ==, 1);
    g_object_unref (dialog);
    gtk_widget_destroy (GTK_WIDGET (parent));

    Completion destroyed;
    parent = GTK_WINDOW (gtk_window_new (GTK_WINDOW_TOPLEVEL));
    gnc_ui_new_commodity_async_full ("NYSE", GTK_WIDGET (parent), nullptr,
                                     nullptr, nullptr, nullptr, 100,
                                     completed, &destroyed);
    dialog = find_child_dialog (parent);
    g_assert_true (GTK_IS_DIALOG (dialog));
    gtk_widget_destroy (GTK_WIDGET (parent));
    g_assert_cmpuint (destroyed.calls, ==, 1);
    g_assert_null (destroyed.book);
    g_assert_null (destroyed.commodity);
    gnc_clear_current_session ();
}

static void
test_selector_new_child_cancel_and_repeat_guard ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }

    auto book = qof_book_new ();
    auto table = gnc_commodity_table_get_table (book);
    auto commodity = gnc_commodity_new (book, "Response test", "NYSE", "RSP",
                                        nullptr, 100);
    gnc_commodity_table_insert (table, commodity);
    gnc_set_current_session (qof_session_new (book));
    auto parent = GTK_WINDOW (gtk_window_new (GTK_WINDOW_TOPLEVEL));
    Completion result;
    gnc_ui_select_commodity_async_full (commodity, GTK_WIDGET (parent),
                                        DIAG_COMM_ALL, nullptr, nullptr, nullptr,
                                        nullptr, completed, &result);
    auto selector = find_child_dialog (parent);
    g_assert_true (GTK_IS_DIALOG (selector));
    gtk_dialog_response (selector, GNC_RESPONSE_NEW);
    auto child = find_child_dialog (GTK_WINDOW (selector));
    g_assert_true (GTK_IS_DIALOG (child));
    gtk_dialog_response (selector, GNC_RESPONSE_NEW);
    auto windows = gtk_window_list_toplevels ();
    guint child_count = 0;
    for (auto node = windows; node; node = node->next)
        if (GTK_IS_DIALOG (node->data) &&
            gtk_window_get_transient_for (GTK_WINDOW (node->data)) ==
                GTK_WINDOW (selector))
            ++child_count;
    g_list_free (windows);
    g_assert_cmpuint (child_count, ==, 1);

    gtk_dialog_response (child, GTK_RESPONSE_CANCEL);
    g_assert_cmpuint (result.calls, ==, 0);
    gtk_dialog_response (selector, GTK_RESPONSE_CANCEL);
    g_assert_cmpuint (result.calls, ==, 1);
    g_assert_null (result.book);
    g_assert_null (result.commodity);
    gtk_widget_destroy (GTK_WIDGET (parent));
    gnc_clear_current_session ();
}

static void
test_child_success_parent_destroy_during_picker_update ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    auto book = qof_book_new ();
    auto table = gnc_commodity_table_get_table (book);
    gnc_commodity_table_add_namespace (table, "NYSE", book);
    auto original = gnc_commodity_new (book, "Original", "NYSE", "ORG",
                                       nullptr, 100);
    gnc_commodity_table_insert (table, original);
    gnc_set_current_session (qof_session_new (book));
    auto parent = GTK_WIDGET (gtk_window_new (GTK_WINDOW_TOPLEVEL));
    g_object_ref (parent);
    Completion result;
    gnc_ui_select_commodity_async_full (original, parent, DIAG_COMM_ALL,
                                        nullptr, nullptr, nullptr, nullptr,
                                        completed, &result);
    auto selector = find_child_dialog (GTK_WINDOW (parent));
    g_assert_true (GTK_IS_DIALOG (selector));
    auto namespace_picker = find_buildable (GTK_WIDGET (selector), "ss_namespace_cbwe");
    g_assert_true (GTK_IS_COMBO_BOX (namespace_picker));
    g_signal_connect (namespace_picker, "changed",
                      G_CALLBACK (destroy_parent_on_picker_change), parent);
    gtk_dialog_response (selector, GNC_RESPONSE_NEW);
    auto child = find_child_dialog (GTK_WINDOW (selector));
    g_assert_true (GTK_IS_DIALOG (child));
    auto fullname = find_buildable (GTK_WIDGET (child), "fullname_entry");
    auto mnemonic = find_buildable (GTK_WIDGET (child), "mnemonic_entry");
    g_assert_true (GTK_IS_ENTRY (fullname));
    g_assert_true (GTK_IS_ENTRY (mnemonic));
    gtk_entry_set_text (GTK_ENTRY (fullname), "Child created");
    gtk_entry_set_text (GTK_ENTRY (mnemonic), "CHD");
    gtk_dialog_response (child, GTK_RESPONSE_OK);
    g_assert_cmpuint (result.calls, ==, 1);
    g_assert_null (result.book);
    g_assert_null (result.commodity);
    g_object_unref (parent);
    gnc_clear_current_session ();
}

static void
test_create_then_edit_commit ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }

    auto book = qof_book_new ();
    auto table = gnc_commodity_table_get_table (book);
    gnc_commodity_table_add_namespace (table, "NYSE", book);
    gnc_set_current_session (qof_session_new (book));
    auto parent = GTK_WINDOW (gtk_window_new (GTK_WINDOW_TOPLEVEL));
    Completion created;
    gnc_ui_new_commodity_async_full ("NYSE", GTK_WIDGET (parent), nullptr,
                                     "New response test", "NRT", "NRT", 100,
                                     completed, &created);
    auto dialog = find_child_dialog (parent);
    g_assert_true (GTK_IS_DIALOG (dialog));
    gtk_dialog_response (dialog, GTK_RESPONSE_OK);
    g_assert_cmpuint (created.calls, ==, 1);
    g_assert_true (created.book == book);
    g_assert_nonnull (created.commodity);
    g_assert_true (gnc_commodity_table_lookup (table, "NYSE", "NRT") ==
                   created.commodity);

    Completion edited;
    gnc_ui_edit_commodity_async (created.commodity, GTK_WIDGET (parent),
                                 completed, &edited);
    dialog = find_child_dialog (parent);
    g_assert_true (GTK_IS_DIALOG (dialog));
    auto fullname = find_buildable (GTK_WIDGET (dialog), "fullname_entry");
    g_assert_true (GTK_IS_ENTRY (fullname));
    gtk_entry_set_text (GTK_ENTRY (fullname), "Edited response test");
    gtk_dialog_response (dialog, GTK_RESPONSE_OK);
    g_assert_cmpuint (edited.calls, ==, 1);
    g_assert_true (edited.book == book);
    g_assert_true (edited.commodity == created.commodity);
    g_assert_cmpstr (gnc_commodity_get_fullname (created.commodity), ==,
                     "Edited response test");
    gtk_widget_destroy (GTK_WIDGET (parent));
    gnc_clear_current_session ();
}

struct NamespaceChange
{
    gint mode;
    guint calls{};
};

static void
replace_namespace_context (GtkComboBox *combo, NamespaceChange *change)
{
    ++change->calls;
    g_signal_handlers_disconnect_by_data (combo, change);
    if (change->mode == 1)
    {
        gnc_clear_current_session ();
        gnc_set_current_session (qof_session_new (qof_book_new ()));
    }
    else if (change->mode == 2)
        qof_book_mark_closed (qof_session_get_book (gnc_get_current_session ()));
    else
    {
        auto replacement = gtk_list_store_new (1, G_TYPE_STRING);
        gtk_combo_box_set_model (combo, GTK_TREE_MODEL (replacement));
        g_object_unref (replacement);
    }
}

static void
test_namespace_context_change (gconstpointer data)
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    auto book = qof_book_new ();
    gnc_set_current_session (qof_session_new (book));
    auto commodity = gnc_commodity_new (book, "Original", "NYSE", "ORG",
                                       nullptr, 100);
    gnc_commodity_table_insert (gnc_commodity_table_get_table (book), commodity);
    auto picker = gtk_combo_box_text_new ();
    g_object_ref_sink (picker);
    gtk_combo_box_text_append_text (GTK_COMBO_BOX_TEXT (picker), "old");
    gtk_combo_box_set_active (GTK_COMBO_BOX (picker), 0);
    auto original_model = gtk_combo_box_get_model (GTK_COMBO_BOX (picker));
    g_object_ref (original_model);
    NamespaceChange change {GPOINTER_TO_INT (data)};
    g_signal_connect (picker, "changed",
                      G_CALLBACK (replace_namespace_context), &change);

    gnc_ui_update_namespace_picker (picker,
        gnc_commodity_get_namespace (commodity), DIAG_COMM_ALL);

    g_assert_cmpuint (change.calls, ==, 1);
    g_assert_cmpint (gtk_tree_model_iter_n_children (original_model, nullptr), ==, 0);
    auto current_model = gtk_combo_box_get_model (GTK_COMBO_BOX (picker));
    g_assert_cmpint (gtk_tree_model_iter_n_children (current_model, nullptr), ==, 0);
    if (change.mode != 0)
        g_assert_true (current_model == original_model);
    else
        g_assert_true (current_model != original_model);
    gtk_widget_destroy (picker);
    g_object_unref (picker);
    g_object_unref (original_model);
    gnc_clear_current_session ();
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
    g_test_add_func ("/gnome-utils/commodity/new-cancel-parent-destroy",
                     test_new_cancel_and_parent_destroy);
    g_test_add_func ("/gnome-utils/commodity/selector-new-child-cancel",
                     test_selector_new_child_cancel_and_repeat_guard);
    g_test_add_func ("/gnome-utils/commodity/child-success-parent-destroy",
                     test_child_success_parent_destroy_during_picker_update);
    g_test_add_func ("/gnome-utils/commodity/create-edit-commit",
                     test_create_then_edit_commit);
    g_test_add_data_func ("/gnome-utils/commodity/namespace-session-switch",
                          GINT_TO_POINTER (1), test_namespace_context_change);
    g_test_add_data_func ("/gnome-utils/commodity/namespace-model-replacement",
                          GINT_TO_POINTER (0), test_namespace_context_change);
    g_test_add_data_func ("/gnome-utils/commodity/namespace-book-closed",
                          GINT_TO_POINTER (2), test_namespace_context_change);
    auto result = g_test_run ();
    gnc_clear_current_session ();
    qof_close ();
    return result;
}
