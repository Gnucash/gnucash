/* Copyright (C) 2026 GnuCash contributors
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#include <config.h>
#include <gtk/gtk.h>
#include <libguile.h>
#include <cstdlib>

#include "cashobjects.h"
#include "gnc-component-manager.h"
#include "gnc-gsettings.h"
#include "gnc-amount-edit.h"
#include "gnc-session.h"
#include "gnc-tree-view-account.h"
#include "gncTaxTable.h"
#include "gncTaxTableP.h"
#include "gnc-commodity.h"
#include "Account.h"
#include "qof.h"
extern "C"
{
#include "dialog-tax-table.h"
}

namespace
{
gboolean display_available;
QofBook *book;
QofSession *session;
GtkWidget *table_window;
GncTaxTable *table;
GncGUID account_guid;

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
find_entry_dialog ()
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *result = nullptr;
    for (auto node = windows; node; node = node->next)
    {
        auto widget = GTK_WIDGET (node->data);
        if (GTK_IS_DIALOG (widget) &&
            gtk_window_get_transient_for (GTK_WINDOW (widget)) ==
            GTK_WINDOW (table_window) &&
            g_strcmp0 (gtk_widget_get_name (widget), "gnc-id-tax-table") == 0)
        {
            g_assert_null (result);
            result = widget;
        }
    }
    g_list_free (windows);
    return result;
}

GtkWidget *
find_account_tree (GtkWidget *root)
{
    if (GNC_IS_TREE_VIEW_ACCOUNT (root))
        return root;
    if (!GTK_IS_CONTAINER (root))
        return nullptr;
    auto children = gtk_container_get_children (GTK_CONTAINER (root));
    GtkWidget *result = nullptr;
    for (auto node = children; node && !result; node = node->next)
        result = find_account_tree (GTK_WIDGET (node->data));
    g_list_free (children);
    return result;
}

GtkWidget *
find_amount_edit (GtkWidget *root)
{
    if (GNC_IS_AMOUNT_EDIT (root))
        return root;
    if (!GTK_IS_CONTAINER (root))
        return nullptr;
    auto children = gtk_container_get_children (GTK_CONTAINER (root));
    GtkWidget *result = nullptr;
    for (auto node = children; node && !result; node = node->next)
        result = find_amount_edit (GTK_WIDGET (node->data));
    g_list_free (children);
    return result;
}

void
select_first_row (GtkWidget *widget)
{
    auto view = GTK_TREE_VIEW (widget);
    GtkTreeIter iter;
    auto model = gtk_tree_view_get_model (view);
    g_assert_true (gtk_tree_model_get_iter_first (model, &iter));
    auto path = gtk_tree_model_get_path (model, &iter);
    gtk_tree_selection_select_path (gtk_tree_view_get_selection (view), path);
    gtk_tree_path_free (path);
}

void
destroy_table_window (GtkWidget *, gpointer)
{
    if (table_window)
        gtk_widget_destroy (table_window);
}

void
test_add_cancel_and_late_response ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    auto owner = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    gtk_widget_realize (owner);
    g_assert_nonnull (gnc_ui_tax_table_window_new (GTK_WINDOW (owner), book));
    auto windows = gtk_window_list_toplevels ();
    for (auto node = windows; node; node = node->next)
        if (g_strcmp0 (gtk_widget_get_name (GTK_WIDGET (node->data)),
                       "gnc-id-new-tax-table") == 0)
            table_window = GTK_WIDGET (node->data);
    g_list_free (windows);
    g_assert_nonnull (table_window);

    auto add_button = find_buildable (table_window, "new_entry_button");
    g_assert_true (GTK_IS_BUTTON (add_button));
    auto new_button = find_buildable (table_window, "new_table_button");
    g_assert_true (GTK_IS_BUTTON (new_button));
    gtk_button_clicked (GTK_BUTTON (new_button));
    auto new_dialog = find_entry_dialog ();
    g_assert_nonnull (new_dialog);
    gtk_entry_set_text (GTK_ENTRY (find_buildable (new_dialog, "name_entry")),
                        "Z response table");
    gnc_tree_view_account_set_selected_account (
        GNC_TREE_VIEW_ACCOUNT (find_account_tree (new_dialog)),
        xaccAccountLookup (&account_guid, book));
    gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (find_amount_edit (new_dialog)),
                                gnc_numeric_create (2, 1));
    gtk_button_clicked (GTK_BUTTON (find_buildable (new_dialog, "ok_button")));
    auto created_table = gncTaxTableLookupByName (book, "Z response table");
    g_assert_nonnull (created_table);
    g_assert_cmpuint (g_list_length (gncTaxTableGetEntries (created_table)), ==, 1);
    select_first_row (find_buildable (table_window, "tax_tables_view"));
    gtk_button_clicked (GTK_BUTTON (add_button));
    auto dialog = find_entry_dialog ();
    g_assert_true (GTK_IS_DIALOG (dialog));
    g_assert_true (gtk_window_get_modal (GTK_WINDOW (dialog)));
    g_assert_true (gtk_window_get_destroy_with_parent (GTK_WINDOW (dialog)));
    auto cancel = find_buildable (dialog, "cancel_button");
    g_assert_true (GTK_IS_BUTTON (cancel));
    gtk_button_clicked (GTK_BUTTON (cancel));
    g_assert_null (find_entry_dialog ());
    g_assert_null (gncTaxTableGetEntries (table));

    gtk_button_clicked (GTK_BUTTON (add_button));
    dialog = find_entry_dialog ();
    g_assert_true (GTK_IS_DIALOG (dialog));
    auto account_tree = find_account_tree (dialog);
    g_assert_nonnull (account_tree);
    gnc_tree_view_account_set_selected_account (
        GNC_TREE_VIEW_ACCOUNT (account_tree),
        xaccAccountLookup (&account_guid, book));
    auto amount_edit = find_amount_edit (dialog);
    g_assert_nonnull (amount_edit);
    gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (amount_edit),
                                gnc_numeric_create (5, 1));
    gtk_button_clicked (GTK_BUTTON (find_buildable (dialog, "ok_button")));
    auto entries = gncTaxTableGetEntries (table);
    g_assert_cmpuint (g_list_length (entries), ==, 1);
    auto original_entry = static_cast<GncTaxTableEntry *> (entries->data);
    g_assert_cmpint (gnc_numeric_compare (
                         gncTaxTableEntryGetAmount (original_entry),
                         gnc_numeric_create (5, 1)), ==, 0);

    select_first_row (find_buildable (table_window, "tax_table_entries"));
    auto edit_button = find_buildable (table_window, "edit_entry_button");
    g_assert_true (GTK_IS_BUTTON (edit_button));
    gtk_button_clicked (GTK_BUTTON (edit_button));
    dialog = find_entry_dialog ();
    g_assert_nonnull (dialog);
    gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (find_amount_edit (dialog)),
                                gnc_numeric_create (7, 1));
    gtk_button_clicked (GTK_BUTTON (find_buildable (dialog, "ok_button")));
    g_assert_null (find_entry_dialog ());
    g_assert_cmpuint (g_list_length (gncTaxTableGetEntries (table)), ==, 1);
    g_assert_cmpint (gnc_numeric_compare (
                         gncTaxTableEntryGetAmount (original_entry),
                         gnc_numeric_create (7, 1)), ==, 0);

    select_first_row (find_buildable (table_window, "tax_table_entries"));
    gtk_button_clicked (GTK_BUTTON (edit_button));
    dialog = find_entry_dialog ();
    g_assert_nonnull (dialog);
    gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (find_amount_edit (dialog)),
                                gnc_numeric_create (9, 1));
    gncTaxTableEntrySetAmount (original_entry, gnc_numeric_create (8, 1));
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    g_assert_null (find_entry_dialog ());
    g_assert_cmpint (gnc_numeric_compare (
                         gncTaxTableEntryGetAmount (original_entry),
                         gnc_numeric_create (8, 1)), ==, 0);

    gtk_button_clicked (GTK_BUTTON (add_button));
    dialog = find_entry_dialog ();
    g_assert_true (GTK_IS_DIALOG (dialog));
    g_object_ref (dialog);
    g_signal_connect (dialog, "destroy", G_CALLBACK (destroy_table_window),
                      nullptr);
    gtk_widget_destroy (table_window);
    table_window = nullptr;
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    g_assert_cmpuint (g_list_length (gncTaxTableGetEntries (table)), ==, 1);
    g_assert_cmpint (gnc_numeric_compare (
                         gncTaxTableEntryGetAmount (original_entry),
                         gnc_numeric_create (8, 1)), ==, 0);
    g_object_unref (dialog);
    gtk_widget_destroy (owner);
}
}

static int
run_tests (int argc, char **argv)
{
    g_setenv ("GNC_UNINSTALLED", "YES", TRUE);
    g_setenv ("GSETTINGS_BACKEND", "memory", TRUE);
    g_test_init (&argc, &argv, nullptr);
    display_available = gtk_init_check (&argc, &argv);
    if (g_getenv ("GNC_REQUIRE_DISPLAY"))
        g_assert_true (display_available);
    qof_init ();
    g_assert_true (cashobjects_register ());
    gnc_component_manager_init ();
    gnc_gsettings_load_backend ();
    book = qof_book_new ();
    session = qof_session_new (book);
    gnc_set_current_session (session);
    table = gncTaxTableCreate (book);
    gncTaxTableSetName (table, "Response test table");
    auto root = gnc_account_create_root (book);
    auto commodity_table = gnc_commodity_table_get_table (book);
    gnc_commodity_table_add_namespace (commodity_table, "CURRENCY", book);
    auto currency = gnc_commodity_new (book, "Test Currency", "CURRENCY",
                                       "TST", nullptr, 100);
    currency = gnc_commodity_table_insert (commodity_table, currency);
    auto account = xaccMallocAccount (book);
    xaccAccountSetName (account, "Tax account");
    xaccAccountSetType (account, ACCT_TYPE_INCOME);
    xaccAccountSetCommodity (account, currency);
    gnc_account_append_child (root, account);
    account_guid = *qof_instance_get_guid (QOF_INSTANCE (account));
    g_test_add_func ("/gnome/tax-table-entry/cancel-late-response",
                     test_add_cancel_and_late_response);
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
