/* Copyright (C) 2026 GnuCash contributors
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */

#include <config.h>
#include <gtk/gtk.h>
#include <libguile.h>
#include <cstdlib>

#include "Account.h"
#include "cashobjects.h"
#include "dialog-tax-table.h"
#include "gnc-amount-edit.h"
#include "gnc-component-manager.h"
#include "gnc-gsettings.h"
#include "gnc-commodity.h"
#include "gnc-session.h"
#include "gnc-tree-view-account.h"
#include "qof.h"

namespace
{
gboolean display_available;

struct Completion
{
    guint calls = 0;
    GtkWindow *parent = nullptr;
    GncTaxTable *table = nullptr;
};

struct ShowDestroy
{
    GtkWindow *owner;
    gboolean fired = FALSE;
};

void
completed (GtkWindow *parent, GncTaxTable *table, gpointer user_data)
{
    auto result = static_cast<Completion *> (user_data);
    ++result->calls;
    result->parent = parent;
    result->table = table;
}

gboolean
destroy_owner_on_editor_show ([[maybe_unused]] GSignalInvocationHint *hint,
                              guint n_values, const GValue *values,
                              gpointer user_data)
{
    if (n_values == 0)
        return TRUE;
    auto request = static_cast<ShowDestroy *> (user_data);
    auto widget = GTK_WIDGET (g_value_get_object (&values[0]));
    if (!request->fired &&
        g_strcmp0 (gtk_widget_get_name (widget), "gnc-id-new-tax-table") == 0)
    {
        request->fired = TRUE;
        gtk_widget_destroy (GTK_WIDGET (request->owner));
    }
    return TRUE;
}

GtkWidget *
find_named_window (const char *name, GtkWindow *transient = nullptr)
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *found = nullptr;
    for (auto node = windows; node; node = node->next)
    {
        auto window = GTK_WIDGET (node->data);
        if (g_strcmp0 (gtk_widget_get_name (window), name) == 0 &&
            (!transient || gtk_window_get_transient_for (GTK_WINDOW (window)) == transient))
        {
            g_assert_null (found);
            found = window;
        }
    }
    g_list_free (windows);
    return found;
}

GtkWidget *
find_account_tree (GtkWidget *widget)
{
    if (GNC_IS_TREE_VIEW_ACCOUNT (widget))
        return widget;
    if (!GTK_IS_CONTAINER (widget))
        return nullptr;
    auto children = gtk_container_get_children (GTK_CONTAINER (widget));
    GtkWidget *found = nullptr;
    for (auto node = children; node && !found; node = node->next)
        found = find_account_tree (GTK_WIDGET (node->data));
    g_list_free (children);
    return found;
}

GtkWidget *
find_amount_edit (GtkWidget *widget)
{
    if (GNC_IS_AMOUNT_EDIT (widget))
        return widget;
    if (!GTK_IS_CONTAINER (widget))
        return nullptr;
    auto children = gtk_container_get_children (GTK_CONTAINER (widget));
    GtkWidget *found = nullptr;
    for (auto node = children; node && !found; node = node->next)
        found = find_amount_edit (GTK_WIDGET (node->data));
    g_list_free (children);
    return found;
}

void
test_create_cancel_and_parent_destroy ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }

    for (int scenario = 0; scenario != 2; ++scenario)
    {
        auto book = qof_book_new ();
        gnc_set_current_session (qof_session_new (book));
        auto owner = GTK_WINDOW (gtk_window_new (GTK_WINDOW_TOPLEVEL));
        gtk_widget_realize (GTK_WIDGET (owner));
        Completion result;
        gnc_ui_tax_table_new_from_name_async (owner, book, "Async tax",
                                              completed, &result);
        auto editor = find_named_window ("gnc-id-new-tax-table");
        g_assert_nonnull (editor);
        auto dialog = find_named_window ("gnc-id-tax-table", GTK_WINDOW (editor));
        g_assert_nonnull (dialog);
        g_assert_true (gtk_window_get_modal (GTK_WINDOW (dialog)));
        g_assert_cmpuint (result.calls, ==, 0);

        if (scenario == 0)
            gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_CANCEL);
        else
        {
            g_object_ref (dialog);
            gtk_widget_destroy (GTK_WIDGET (owner));
            g_assert_cmpuint (result.calls, ==, 1);
            g_assert_null (result.parent);
            g_assert_null (result.table);
            gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
            g_assert_cmpuint (result.calls, ==, 1);
            g_object_unref (dialog);
        }

        if (scenario == 0)
        {
            g_assert_cmpuint (result.calls, ==, 1);
            g_assert_true (result.parent == owner);
            g_assert_null (result.table);
            gtk_widget_destroy (GTK_WIDGET (owner));
        }
        if (gtk_widget_get_visible (editor))
            gtk_widget_destroy (editor);
        gnc_clear_current_session ();
    }
}

void
test_create_accept_returns_live_table_once ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    auto book = qof_book_new ();
    gnc_set_current_session (qof_session_new (book));
    auto root = gnc_account_create_root (book);
    auto currency = gnc_commodity_new (book, "Test currency", "CURRENCY",
                                       "TST", "", 100);
    currency = gnc_commodity_table_insert (gnc_commodity_table_get_table (book),
                                           currency);
    auto account = xaccMallocAccount (book);
    xaccAccountSetName (account, "Tax account");
    xaccAccountSetType (account, ACCT_TYPE_EXPENSE);
    xaccAccountSetCommodity (account, currency);
    gnc_account_append_child (root, account);

    auto owner = GTK_WINDOW (gtk_window_new (GTK_WINDOW_TOPLEVEL));
    gtk_widget_realize (GTK_WIDGET (owner));
    Completion result;
    gnc_ui_tax_table_new_from_name_async (owner, book, "Async tax",
                                          completed, &result);
    auto editor = find_named_window ("gnc-id-new-tax-table");
    auto dialog = find_named_window ("gnc-id-tax-table", GTK_WINDOW (editor));
    g_assert_nonnull (dialog);
    /* The asynchronous entry point pre-fills the name. Select a real account
     * so the accept path exercises the engine commit and GUID re-resolution. */
    auto tree = find_account_tree (dialog);
    g_assert_nonnull (tree);
    gnc_tree_view_account_set_selected_account (GNC_TREE_VIEW_ACCOUNT (tree), account);
    auto amount = find_amount_edit (dialog);
    g_assert_nonnull (amount);
    gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (amount), gnc_numeric_zero ());
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    g_assert_cmpuint (result.calls, ==, 1);
    g_assert_nonnull (result.table);
    g_assert_true (result.parent == owner);
    g_assert_cmpstr (gncTaxTableGetName (result.table), ==, "Async tax");
    auto found = gncTaxTableLookupByName (book, "Async tax");
    g_assert_true (found == result.table);
    gtk_widget_destroy (GTK_WIDGET (owner));
    if (gtk_widget_get_visible (editor))
        gtk_widget_destroy (editor);
    gnc_clear_current_session ();
}

void
test_owner_destroy_during_editor_show ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    auto book = qof_book_new ();
    gnc_set_current_session (qof_session_new (book));
    auto owner = GTK_WINDOW (gtk_window_new (GTK_WINDOW_TOPLEVEL));
    gtk_widget_realize (GTK_WIDGET (owner));
    g_object_ref (owner);
    Completion result;
    ShowDestroy show_destroy { owner };
    auto show_signal = g_signal_lookup ("show", GTK_TYPE_WIDGET);
    g_assert_cmpuint (show_signal, !=, 0);
    auto hook = g_signal_add_emission_hook (show_signal, 0,
                                            destroy_owner_on_editor_show,
                                            &show_destroy, nullptr);
    gnc_ui_tax_table_new_from_name_async (owner, book, "Async tax",
                                          completed, &result);
    g_signal_remove_emission_hook (show_signal, hook);
    g_assert_true (show_destroy.fired);
    g_assert_cmpuint (result.calls, ==, 1);
    g_assert_null (result.parent);
    g_assert_null (result.table);
    g_assert_null (find_named_window ("gnc-id-new-tax-table"));
    g_object_unref (owner);
    gnc_clear_current_session ();
}
}

static int
run_tests (int argc, char **argv)
{
    g_setenv("GNC_UNINSTALLED", "YES", TRUE);
    g_setenv("GSETTINGS_BACKEND", "memory", TRUE);
    g_test_init (&argc, &argv, nullptr);
    display_available = gtk_init_check (&argc, &argv);
    if (g_getenv ("GNC_REQUIRE_DISPLAY")) g_assert_true (display_available);
    qof_init ();
    g_assert_true (cashobjects_register ());
    gnc_component_manager_init ();
    gnc_gsettings_load_backend ();
    g_test_add_func ("/gnome-utils/tax-table/create-cancel-parent-destroy",
                     test_create_cancel_and_parent_destroy);
    g_test_add_func ("/gnome-utils/tax-table/create-accept",
                     test_create_accept_returns_live_table_once);
    g_test_add_func ("/gnome-utils/tax-table/owner-destroy-during-show",
                     test_owner_destroy_during_editor_show);
    auto result = g_test_run ();
    gnc_gsettings_shutdown ();
    gnc_component_manager_shutdown ();
    gnc_clear_current_session ();
    qof_close ();
    return result;
}

static void
guile_main ([[maybe_unused]] void *data, int argc, char **argv)
{
    std::exit (run_tests (argc, argv));
}

int
main (int argc, char **argv)
{
    scm_boot_guile (argc, argv, guile_main, nullptr);
    return 0;
}
