/* Copyright (C) 2026 GnuCash contributors
 * SPDX-License-Identifier: GPL-2.0-or-later
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */

#include <config.h>
#include <cstdint>
#include <gtk/gtk.h>
#include <gtest/gtest.h>
#include "googletest-glib-log-handler.hpp"
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
struct Completion
{
    std::uint32_t calls = 0;
    GtkWindow *parent = nullptr;
    GncTaxTable *table = nullptr;
};

struct ShowDestroy
{
    GtkWindow *owner;
    bool fired = false;
};

static GtkWidget *find_named_window (const char *name, GtkWindow *transient = nullptr);

class TaxTableCreateResponseTest : public ::testing::Test
{
protected:
    void SetUp () override
    {
        m_session = qof_session_new (qof_book_new ());
        gnc_set_current_session (m_session);
        m_book = qof_session_get_book (m_session);
        m_root = gnc_account_create_root (m_book);
        auto currency = gnc_commodity_new (m_book, "Test currency", "CURRENCY",
                                          "TST", "", 100);
        m_currency = gnc_commodity_table_insert (
            gnc_commodity_table_get_table (m_book), currency);
        m_account = xaccMallocAccount (m_book);
        xaccAccountSetName (m_account, "Tax account");
        xaccAccountSetType (m_account, ACCT_TYPE_EXPENSE);
        xaccAccountSetCommodity (m_account, m_currency);
        gnc_account_append_child (m_root, m_account);
        m_owner = GTK_WINDOW (gtk_window_new (GTK_WINDOW_TOPLEVEL));
        g_object_ref_sink (m_owner);
        gtk_widget_realize (GTK_WIDGET (m_owner));
        m_show_destroy.owner = m_owner;
    }
    void TearDown () override
    {
        auto editor = find_named_window ("gnc-id-new-tax-table");
        if (editor && gtk_widget_get_visible (editor))
            gtk_widget_destroy (editor);
        if (m_owner)
        {
            gtk_widget_destroy (GTK_WIDGET (m_owner));
            g_object_unref (m_owner);
        }
        if (gnc_current_session_exist ())
            gnc_clear_current_session ();
    }
    QofSession *m_session{};
    QofBook *m_book{};
    Account *m_root{};
    Account *m_account{};
    gnc_commodity *m_currency{};
    GtkWindow *m_owner{};
    Completion m_result{};
    ShowDestroy m_show_destroy{};
};

static void
completed (GtkWindow *parent, GncTaxTable *table, gpointer user_data)
{
    auto result = static_cast<Completion *> (user_data);
    ++result->calls;
    result->parent = parent;
    result->table = table;
}

static gboolean
destroy_owner_on_editor_show ([[maybe_unused]] GSignalInvocationHint *hint,
                              guint n_values, const GValue *values,
                              gpointer user_data)
{
    if (n_values == 0u)
        return true;
    auto request = static_cast<ShowDestroy *> (user_data);
    auto widget = GTK_WIDGET (g_value_get_object (&values[0]));
    if (!request->fired &&
        g_strcmp0 (gtk_widget_get_name (widget), "gnc-id-new-tax-table") == 0)
    {
        request->fired = true;
        gtk_widget_destroy (GTK_WIDGET (request->owner));
    }
    return true;
}

static GtkWidget *
find_named_window (const char *name, GtkWindow *transient)
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *found = nullptr;
    for (auto node = windows; node; node = node->next)
    {
        auto window = GTK_WIDGET (node->data);
        if (g_strcmp0 (gtk_widget_get_name (window), name) == 0 &&
            (!transient || gtk_window_get_transient_for (GTK_WINDOW (window)) == transient))
        {
            EXPECT_EQ (found, nullptr);
            found = window;
        }
    }
    g_list_free (windows);
    return found;
}

static GtkWidget *
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

static GtkWidget *
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

TEST_F (TaxTableCreateResponseTest, CancelCompletesWithLiveOwner)
{
    gnc_ui_tax_table_new_from_name_async (m_owner, m_book, "Async tax",
                                          completed, &m_result);
    auto editor = find_named_window ("gnc-id-new-tax-table");
    ASSERT_NE (editor, nullptr);
    auto dialog = find_named_window ("gnc-id-tax-table", GTK_WINDOW (editor));
    ASSERT_NE (dialog, nullptr);
    EXPECT_TRUE (gtk_window_get_modal (GTK_WINDOW (dialog)));
    EXPECT_EQ (m_result.calls, 0u);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_CANCEL);
    EXPECT_EQ (m_result.calls, 1u);
    EXPECT_EQ (m_result.parent, m_owner);
    EXPECT_EQ (m_result.table, nullptr);
}

TEST_F (TaxTableCreateResponseTest, ParentDestroyCancelsAndMakesLateResponseInert)
{
    gnc_ui_tax_table_new_from_name_async (m_owner, m_book, "Async tax",
                                          completed, &m_result);
    auto editor = find_named_window ("gnc-id-new-tax-table");
    ASSERT_NE (editor, nullptr);
    auto dialog = find_named_window ("gnc-id-tax-table", GTK_WINDOW (editor));
    ASSERT_NE (dialog, nullptr);
    g_object_ref (dialog);
    gtk_widget_destroy (GTK_WIDGET (m_owner));
    EXPECT_EQ (m_result.calls, 1u);
    EXPECT_EQ (m_result.parent, nullptr);
    EXPECT_EQ (m_result.table, nullptr);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    EXPECT_EQ (m_result.calls, 1u);
    g_object_unref (dialog);
}

TEST_F (TaxTableCreateResponseTest, AcceptReturnsLiveTableOnce)
{
    gnc_ui_tax_table_new_from_name_async (m_owner, m_book, "Async tax",
                                          completed, &m_result);
    auto editor = find_named_window ("gnc-id-new-tax-table");
    ASSERT_NE (editor, nullptr);
    auto dialog = find_named_window ("gnc-id-tax-table", GTK_WINDOW (editor));
    ASSERT_NE (dialog, nullptr);
    auto tree = find_account_tree (dialog);
    ASSERT_NE (tree, nullptr);
    gnc_tree_view_account_set_selected_account (GNC_TREE_VIEW_ACCOUNT (tree), m_account);
    auto amount = find_amount_edit (dialog);
    ASSERT_NE (amount, nullptr);
    gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (amount), gnc_numeric_zero ());
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    EXPECT_EQ (m_result.calls, 1u);
    ASSERT_NE (m_result.table, nullptr);
    EXPECT_EQ (m_result.parent, m_owner);
    EXPECT_STREQ (gncTaxTableGetName (m_result.table), "Async tax");
    EXPECT_EQ (gncTaxTableLookupByName (m_book, "Async tax"), m_result.table);
}

TEST_F (TaxTableCreateResponseTest, OwnerDestroyedDuringEditorShowCancels)
{
    auto show_signal = g_signal_lookup ("show", GTK_TYPE_WIDGET);
    ASSERT_NE (show_signal, 0u);
    auto hook = g_signal_add_emission_hook (show_signal, 0,
                                            destroy_owner_on_editor_show,
                                            &m_show_destroy, nullptr);
    gnc_ui_tax_table_new_from_name_async (m_owner, m_book, "Async tax",
                                          completed, &m_result);
    g_signal_remove_emission_hook (show_signal, hook);
    EXPECT_TRUE (m_show_destroy.fired);
    EXPECT_EQ (m_result.calls, 1u);
    EXPECT_EQ (m_result.parent, nullptr);
    EXPECT_EQ (m_result.table, nullptr);
    EXPECT_EQ (find_named_window ("gnc-id-new-tax-table"), nullptr);
}
}

static int
run_tests (int argc, char **argv)
{
    g_setenv("GNC_UNINSTALLED", "YES", TRUE);
    g_setenv("GSETTINGS_BACKEND", "memory", TRUE);
    ::testing::InitGoogleTest (&argc, argv);
    if (!gtk_init_check (&argc, &argv))
        g_error ("A graphical display is required for tax table tests");
    qof_init ();
    if (!cashobjects_register ())
        g_error ("Failed to register cash objects");
    gnc_component_manager_init ();
    gnc_gsettings_load_backend ();
    gnc::test::initialize_logging ();
    auto result = RUN_ALL_TESTS ();
    gnc_gsettings_shutdown ();
    gnc_component_manager_shutdown ();
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
