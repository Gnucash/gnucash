/* Copyright (C) 2026 GnuCash contributors
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#include <config.h>
#include "test-logging.hpp"
#include <gtk/gtk.h>
#include "test/gnome-response-test-fixture.h"
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
class TaxTableEntryResponseTest : public GnomeResponseTest
{
protected:
    void SetUp () override;
    void TearDown () override;

    QofBook *book{};
    QofSession *session{};
    GtkWidget *owner{};
    GtkWidget *table_window{};
    GncTaxTable *table{};
    GncGUID account_guid{};
};

static GtkWidget *
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

static GtkWidget *
find_entry_dialog (GtkWidget *table_window)
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
            if (result)
            {
                g_list_free (windows);
                return nullptr;
            }
            result = widget;
        }
    }
    g_list_free (windows);
    return result;
}

static GtkWidget *
find_tax_table_window ()
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *result = nullptr;
    for (auto node = windows; node; node = node->next)
        if (g_strcmp0 (gtk_widget_get_name (GTK_WIDGET (node->data)),
                       "gnc-id-new-tax-table") == 0)
        {
            if (result)
            {
                g_list_free (windows);
                return nullptr;
            }
            result = GTK_WIDGET (node->data);
        }
    g_list_free (windows);
    return result;
}

static GtkWidget *
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

static GtkWidget *
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

static bool
select_first_row (GtkWidget *widget)
{
    if (!GTK_IS_TREE_VIEW (widget))
        return false;
    auto view = GTK_TREE_VIEW (widget);
    GtkTreeIter iter;
    auto model = gtk_tree_view_get_model (view);
    if (!gtk_tree_model_get_iter_first (model, &iter))
        return false;
    auto path = gtk_tree_model_get_path (model, &iter);
    gtk_tree_selection_select_path (gtk_tree_view_get_selection (view), path);
    gtk_tree_path_free (path);
    return true;
}

static void
destroy_table_window (GtkWidget *, gpointer window)
{
    if (window)
        gtk_widget_destroy (GTK_WIDGET (window));
}

void
TaxTableEntryResponseTest::SetUp ()
{
    GnomeResponseTest::SetUp ();
    book = qof_book_new ();
    ASSERT_NE (book, nullptr);
    session = qof_session_new (book);
    ASSERT_NE (session, nullptr);
    gnc_set_current_session (session);
    table = gncTaxTableCreate (book);
    ASSERT_NE (table, nullptr);
    gncTaxTableSetName (table, "Response test table");
    auto root = gnc_account_create_root (book);
    ASSERT_NE (root, nullptr);
    auto commodity_table = gnc_commodity_table_get_table (book);
    gnc_commodity_table_add_namespace (commodity_table, "CURRENCY", book);
    auto currency = gnc_commodity_new (book, "Test Currency", "CURRENCY",
                                       "TST", nullptr, 100);
    ASSERT_NE (currency, nullptr);
    currency = gnc_commodity_table_insert (commodity_table, currency);
    ASSERT_NE (currency, nullptr);
    auto account = xaccMallocAccount (book);
    ASSERT_NE (account, nullptr);
    xaccAccountSetName (account, "Tax account");
    xaccAccountSetType (account, ACCT_TYPE_INCOME);
    xaccAccountSetCommodity (account, currency);
    gnc_account_append_child (root, account);
    account_guid = *qof_instance_get_guid (QOF_INSTANCE (account));

    owner = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    ASSERT_NE (owner, nullptr);
    gtk_widget_realize (owner);
    ASSERT_NE (gnc_ui_tax_table_window_new (GTK_WINDOW (owner), book), nullptr);
    table_window = find_tax_table_window ();
    ASSERT_NE (table_window, nullptr);
}

void
TaxTableEntryResponseTest::TearDown ()
{
    if (session)
        gnc_close_gui_component_by_session (session);
    GnomeResponseTest::TearDown ();
    auto current = gnc_exchange_current_session (nullptr);
    if (current)
        qof_session_destroy (current);
    if (session && session != current)
        qof_session_destroy (session);
    session = nullptr;
}
}

TEST_F (TaxTableEntryResponseTest, NewTaxTableDialogCreatesTableWithEntry)
{
    auto new_button = find_buildable (table_window, "new_table_button");
    ASSERT_TRUE (GTK_IS_BUTTON (new_button));
    gtk_button_clicked (GTK_BUTTON (new_button));
    auto dialog = find_entry_dialog (table_window);
    ASSERT_NE (dialog, nullptr);
    auto name_entry = GTK_ENTRY (find_buildable (dialog, "name_entry"));
    ASSERT_TRUE (GTK_IS_ENTRY (name_entry));
    gtk_entry_set_text (name_entry, "Z response table");
    auto account_tree = find_account_tree (dialog);
    ASSERT_NE (account_tree, nullptr);
    auto account = xaccAccountLookup (&account_guid, book);
    ASSERT_NE (account, nullptr);
    gnc_tree_view_account_set_selected_account (
        GNC_TREE_VIEW_ACCOUNT (account_tree),
        account);
    auto amount_edit = find_amount_edit (dialog);
    ASSERT_NE (amount_edit, nullptr);
    gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (amount_edit),
                                gnc_numeric_create (2, 1));
    auto ok = find_buildable (dialog, "ok_button");
    ASSERT_TRUE (GTK_IS_BUTTON (ok));
    gtk_button_clicked (GTK_BUTTON (ok));

    auto created_table = gncTaxTableLookupByName (book, "Z response table");
    ASSERT_NE (created_table, nullptr);
    EXPECT_EQ (g_list_length (gncTaxTableGetEntries (created_table)), 1u);
}

TEST_F (TaxTableEntryResponseTest, CancelledAddLeavesTableEmpty)
{
    ASSERT_TRUE (select_first_row (
        find_buildable (table_window, "tax_tables_view")));
    auto add_button = find_buildable (table_window, "new_entry_button");
    ASSERT_TRUE (GTK_IS_BUTTON (add_button));
    gtk_button_clicked (GTK_BUTTON (add_button));
    auto dialog = find_entry_dialog (table_window);
    ASSERT_TRUE (GTK_IS_DIALOG (dialog));
    EXPECT_TRUE (gtk_window_get_modal (GTK_WINDOW (dialog)));
    EXPECT_TRUE (gtk_window_get_destroy_with_parent (GTK_WINDOW (dialog)));
    auto cancel = find_buildable (dialog, "cancel_button");
    ASSERT_TRUE (GTK_IS_BUTTON (cancel));
    gtk_button_clicked (GTK_BUTTON (cancel));

    EXPECT_EQ (find_entry_dialog (table_window), nullptr);
    EXPECT_EQ (gncTaxTableGetEntries (table), nullptr);
}

TEST_F (TaxTableEntryResponseTest, AddAndEditEntryUpdatesAmount)
{
    ASSERT_TRUE (select_first_row (
        find_buildable (table_window, "tax_tables_view")));
    auto add_button = find_buildable (table_window, "new_entry_button");
    ASSERT_TRUE (GTK_IS_BUTTON (add_button));
    gtk_button_clicked (GTK_BUTTON (add_button));
    auto dialog = find_entry_dialog (table_window);
    ASSERT_NE (dialog, nullptr);
    auto account_tree = find_account_tree (dialog);
    ASSERT_NE (account_tree, nullptr);
    auto account = xaccAccountLookup (&account_guid, book);
    ASSERT_NE (account, nullptr);
    gnc_tree_view_account_set_selected_account (
        GNC_TREE_VIEW_ACCOUNT (account_tree),
        account);
    auto amount_edit = find_amount_edit (dialog);
    ASSERT_NE (amount_edit, nullptr);
    gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (amount_edit),
                                gnc_numeric_create (5, 1));
    auto ok = find_buildable (dialog, "ok_button");
    ASSERT_TRUE (GTK_IS_BUTTON (ok));
    gtk_button_clicked (GTK_BUTTON (ok));
    auto entries = gncTaxTableGetEntries (table);
    ASSERT_EQ (g_list_length (entries), 1u);
    auto entry = static_cast<GncTaxTableEntry *> (entries->data);
    EXPECT_EQ (gnc_numeric_compare (gncTaxTableEntryGetAmount (entry),
                                    gnc_numeric_create (5, 1)), 0);

    ASSERT_TRUE (select_first_row (
        find_buildable (table_window, "tax_table_entries")));
    auto edit_button = find_buildable (table_window, "edit_entry_button");
    ASSERT_TRUE (GTK_IS_BUTTON (edit_button));
    gtk_button_clicked (GTK_BUTTON (edit_button));
    dialog = find_entry_dialog (table_window);
    ASSERT_NE (dialog, nullptr);
    amount_edit = find_amount_edit (dialog);
    ASSERT_NE (amount_edit, nullptr);
    gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (amount_edit),
                                gnc_numeric_create (7, 1));
    ok = find_buildable (dialog, "ok_button");
    ASSERT_TRUE (GTK_IS_BUTTON (ok));
    gtk_button_clicked (GTK_BUTTON (ok));

    EXPECT_EQ (find_entry_dialog (table_window), nullptr);
    EXPECT_EQ (g_list_length (gncTaxTableGetEntries (table)), 1u);
    EXPECT_EQ (gnc_numeric_compare (gncTaxTableEntryGetAmount (entry),
                                    gnc_numeric_create (7, 1)), 0);
}

TEST_F (TaxTableEntryResponseTest, ExternalEditWinsOverStaleDialogResponse)
{
    ASSERT_TRUE (select_first_row (
        find_buildable (table_window, "tax_tables_view")));
    auto add_button = find_buildable (table_window, "new_entry_button");
    ASSERT_TRUE (GTK_IS_BUTTON (add_button));
    gtk_button_clicked (GTK_BUTTON (add_button));
    auto dialog = find_entry_dialog (table_window);
    ASSERT_NE (dialog, nullptr);
    auto account_tree = find_account_tree (dialog);
    ASSERT_NE (account_tree, nullptr);
    auto account = xaccAccountLookup (&account_guid, book);
    ASSERT_NE (account, nullptr);
    gnc_tree_view_account_set_selected_account (
        GNC_TREE_VIEW_ACCOUNT (account_tree),
        account);
    auto amount_edit = find_amount_edit (dialog);
    ASSERT_NE (amount_edit, nullptr);
    gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (amount_edit),
                                gnc_numeric_create (5, 1));
    auto ok = find_buildable (dialog, "ok_button");
    ASSERT_TRUE (GTK_IS_BUTTON (ok));
    gtk_button_clicked (GTK_BUTTON (ok));
    auto entries = gncTaxTableGetEntries (table);
    ASSERT_EQ (g_list_length (entries), 1u);
    auto entry = static_cast<GncTaxTableEntry *> (entries->data);

    ASSERT_TRUE (select_first_row (
        find_buildable (table_window, "tax_table_entries")));
    auto edit_button = find_buildable (table_window, "edit_entry_button");
    ASSERT_TRUE (GTK_IS_BUTTON (edit_button));
    gtk_button_clicked (GTK_BUTTON (edit_button));
    dialog = find_entry_dialog (table_window);
    ASSERT_NE (dialog, nullptr);
    amount_edit = find_amount_edit (dialog);
    ASSERT_NE (amount_edit, nullptr);
    gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (amount_edit),
                                gnc_numeric_create (9, 1));
    gncTaxTableEntrySetAmount (entry, gnc_numeric_create (8, 1));
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);

    EXPECT_EQ (find_entry_dialog (table_window), nullptr);
    EXPECT_EQ (gnc_numeric_compare (gncTaxTableEntryGetAmount (entry),
                                    gnc_numeric_create (8, 1)), 0);
}

TEST_F (TaxTableEntryResponseTest, DestroyedManagerIgnoresLateAddResponse)
{
    ASSERT_TRUE (select_first_row (
        find_buildable (table_window, "tax_tables_view")));
    auto add_button = find_buildable (table_window, "new_entry_button");
    ASSERT_TRUE (GTK_IS_BUTTON (add_button));
    gtk_button_clicked (GTK_BUTTON (add_button));
    auto dialog = find_entry_dialog (table_window);
    ASSERT_NE (dialog, nullptr);
    auto account_tree = find_account_tree (dialog);
    ASSERT_NE (account_tree, nullptr);
    auto account = xaccAccountLookup (&account_guid, book);
    ASSERT_NE (account, nullptr);
    gnc_tree_view_account_set_selected_account (
        GNC_TREE_VIEW_ACCOUNT (account_tree),
        account);
    auto amount_edit = find_amount_edit (dialog);
    ASSERT_NE (amount_edit, nullptr);
    gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (amount_edit),
                                gnc_numeric_create (5, 1));
    auto ok = find_buildable (dialog, "ok_button");
    ASSERT_TRUE (GTK_IS_BUTTON (ok));
    gtk_button_clicked (GTK_BUTTON (ok));
    auto entries = gncTaxTableGetEntries (table);
    ASSERT_EQ (g_list_length (entries), 1u);
    auto entry = static_cast<GncTaxTableEntry *> (entries->data);
    gncTaxTableEntrySetAmount (entry, gnc_numeric_create (8, 1));

    gtk_button_clicked (GTK_BUTTON (add_button));
    dialog = find_entry_dialog (table_window);
    ASSERT_TRUE (GTK_IS_DIALOG (dialog));
    g_object_ref (dialog);
    g_signal_connect (dialog, "destroy", G_CALLBACK (destroy_table_window),
                      table_window);
    gtk_widget_destroy (table_window);
    table_window = nullptr;
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);

    EXPECT_EQ (g_list_length (gncTaxTableGetEntries (table)), 1u);
    EXPECT_EQ (gnc_numeric_compare (gncTaxTableEntryGetAmount (entry),
                                    gnc_numeric_create (8, 1)), 0);
    g_object_unref (dialog);
}

static int
run_tests (int argc, char **argv)
{
    g_setenv ("GNC_UNINSTALLED", "YES", true);
    g_setenv ("GSETTINGS_BACKEND", "memory", true);
    ::testing::InitGoogleTest (&argc, argv);
    if (!gtk_init_check (&argc, &argv))
    {
        g_printerr ("GTK display initialization failed; GUI tests require a display.\n");
        return 1;
    }
    qof_init ();
    if (!cashobjects_register ())
        g_error ("Failed to register cash objects for tax table entry tests");
    gnc_component_manager_init ();
    gnc_gsettings_load_backend ();
    gnc::test::initialize_logging ();
    auto result = RUN_ALL_TESTS ();
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
