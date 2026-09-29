/* Copyright (C) 2026 GnuCash contributors
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 */

#include <config.h>

#include <gtk/gtk.h>

#include "Account.h"
#include "cashobjects.h"
#include "gnc-gsettings.h"
#include "gnc-session.h"
#include "gnc-tree-view-account.h"
#include "qof.h"
#include "search-account.h"
#include "search-core-type.h"

static gboolean display_available;

static GtkWidget *
find_widget (GtkWidget *root, gboolean (*match)(GtkWidget *))
{
    if (match (root))
        return root;
    if (!GTK_IS_CONTAINER (root))
        return nullptr;
    auto children = gtk_container_get_children (GTK_CONTAINER (root));
    GtkWidget *found = nullptr;
    for (auto node = children; node && !found; node = node->next)
        found = find_widget (GTK_WIDGET (node->data), match);
    g_list_free (children);
    return found;
}

static gboolean is_button (GtkWidget *widget) { return GTK_IS_BUTTON (widget); }
static gboolean is_account_view (GtkWidget *widget)
{
    return GNC_IS_TREE_VIEW_ACCOUNT (widget);
}

static const gchar *
button_text (GtkWidget *button)
{
    auto label = gtk_bin_get_child (GTK_BIN (button));
    return GTK_IS_LABEL (label) ? gtk_label_get_text (GTK_LABEL (label)) : nullptr;
}

static GtkWidget *
find_selection_dialog ()
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *found = nullptr;
    for (auto node = windows; node && !found; node = node->next)
    {
        auto widget = GTK_WIDGET (node->data);
        if (GTK_IS_DIALOG (widget) &&
            g_strcmp0 (gtk_window_get_title (GTK_WINDOW (widget)),
                       "Select the Accounts to Compare") == 0)
            found = widget;
    }
    g_list_free (windows);
    return found;
}

static void
test_public_widget_response_lifetimes ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }

    auto book = qof_book_new ();
    auto root = gnc_account_create_root (book);
    auto account = xaccMallocAccount (book);
    xaccAccountSetName (account, "Search selection target");
    gnc_account_append_child (root, account);
    gnc_set_current_session (qof_session_new (book));

    auto parent = GTK_WINDOW (gtk_window_new (GTK_WINDOW_TOPLEVEL));
    auto contents = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
    gtk_container_add (GTK_CONTAINER (parent), contents);
    auto search = gnc_search_account_new ();
    gnc_search_core_type_pass_parent (GNC_SEARCH_CORE_TYPE (search), parent);
    auto widget = gnc_search_core_type_get_widget (GNC_SEARCH_CORE_TYPE (search));
    gtk_container_add (GTK_CONTAINER (contents), widget);
    auto button = find_widget (widget, is_button);
    g_assert_true (GTK_IS_BUTTON (button));
    gtk_button_clicked (GTK_BUTTON (button));
    auto dialog = find_selection_dialog ();
    g_assert_true (GTK_IS_DIALOG (dialog));
    g_assert_true (gtk_window_get_modal (GTK_WINDOW (dialog)));
    g_assert_true (gtk_window_get_destroy_with_parent (GTK_WINDOW (dialog)));
    auto view = find_widget (dialog, is_account_view);
    g_assert_true (GNC_IS_TREE_VIEW_ACCOUNT (view));

    GList selected{account, nullptr, nullptr};
    gnc_tree_view_account_set_selected_accounts (
        GNC_TREE_VIEW_ACCOUNT (view), &selected, FALSE);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    auto predicate = gnc_search_core_type_get_predicate (
        GNC_SEARCH_CORE_TYPE (search));
    g_assert_nonnull (predicate);
    qof_query_core_predicate_free (predicate);
    g_assert_cmpstr (button_text (button), ==,
                     "Selected Accounts");

    gtk_button_clicked (GTK_BUTTON (button));
    dialog = find_selection_dialog ();
    g_assert_true (GTK_IS_DIALOG (dialog));
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_CANCEL);
    g_assert_cmpstr (button_text (button), ==,
                     "Selected Accounts");

    auto owner = gnc_search_account_new ();
    gnc_search_core_type_pass_parent (GNC_SEARCH_CORE_TYPE (owner), parent);
    auto owner_widget = gnc_search_core_type_get_widget (
        GNC_SEARCH_CORE_TYPE (owner));
    gtk_container_add (GTK_CONTAINER (contents), owner_widget);
    auto owner_button = find_widget (owner_widget, is_button);
    gtk_button_clicked (GTK_BUTTON (owner_button));
    auto owner_dialog = find_selection_dialog ();
    g_assert_true (GTK_IS_DIALOG (owner_dialog));
    g_object_ref (owner_dialog);
    g_object_unref (owner);
    gtk_dialog_response (GTK_DIALOG (owner_dialog), GTK_RESPONSE_OK);
    g_assert_cmpstr (button_text (owner_button), ==, "Choose Accounts");
    g_object_unref (owner_dialog);

    gtk_button_clicked (GTK_BUTTON (button));
    dialog = find_selection_dialog ();
    g_object_ref (dialog);
    auto label = gtk_bin_get_child (GTK_BIN (button));
    g_object_ref (label);
    gtk_widget_destroy (GTK_WIDGET (parent));
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    g_assert_cmpstr (gtk_label_get_text (GTK_LABEL (label)), ==,
                     "Selected Accounts");
    g_object_unref (label);
    g_object_unref (dialog);
    g_object_unref (search);
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
    gnc_gsettings_load_backend ();
    g_test_add_func ("/gnome-search/account/public-widget-response-lifetimes",
                     test_public_widget_response_lifetimes);
    auto result = g_test_run ();
    gnc_gsettings_shutdown ();
    qof_close ();
    return result;
}
