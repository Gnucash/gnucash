/* Copyright (C) 2026 GnuCash contributors
 * SPDX-License-Identifier: GPL-2.0-or-later
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 */

#include <config.h>
#include <cstdint>

#include <gtk/gtk.h>
#include "gtk-test-utils.hpp"


#include <gtest/gtest.h>
#include "googletest-glib-log-handler.hpp"

#include <vector>

#include "cashobjects.h"
#include "dialog-commodity.h"
#include "gnc-session.h"
#include "gnc-ui.h"
#include "qof.h"

struct Completion
{
    std::uint32_t calls{};
    QofBook *book{};
    gnc_commodity *commodity{};
};

class CommodityResponseTest : public ::testing::Test
{
protected:
    static void SetUpTestSuite ()
    {
        qof_init ();
        ASSERT_TRUE (cashobjects_register ());
    }

    static void TearDownTestSuite ()
    {
        qof_close ();
    }

    void SetUp () override
    {
        m_session = qof_session_new (qof_book_new ());
        m_book = qof_session_get_book (m_session);
        gnc_set_current_session (m_session);
    }

    void TearDown () override
    {
        for (auto parent : m_parents)
        {
            gtk_widget_destroy (GTK_WIDGET (parent));
            g_object_unref (parent);
        }
        for (auto dialog : m_retained_dialogs)
            g_object_unref (dialog);
        m_retained_dialogs.clear ();
        auto current = gnc_exchange_current_session (nullptr);
        if (current)
            qof_session_destroy (current);
        if (m_session && m_session != current)
            qof_session_destroy (m_session);
    }

    GtkWindow *create_parent ()
    {
        auto parent = GTK_WINDOW (gtk_window_new (GTK_WINDOW_TOPLEVEL));
        g_object_ref_sink (parent);
        m_parents.push_back (parent);
        return parent;
    }
    void retain_dialog (GtkWidget *dialog)
    {
        m_retained_dialogs.push_back (GTK_WIDGET (g_object_ref (dialog)));
    }

    QofSession *m_session{};
    QofBook *m_book{};
    std::vector<GtkWindow *> m_parents;
    std::vector<GtkWidget *> m_retained_dialogs;
    Completion m_primary{};
    Completion m_secondary{};
};

struct NamespaceChange
{
    std::uint32_t calls{};
    QofBook *book{};
    QofSession *replacement_session{};
};

static void
switch_session_on_namespace_change (GtkComboBox *combo, gpointer data)
{
    auto change = static_cast<NamespaceChange *> (data);
    ++change->calls;
    g_signal_handlers_disconnect_by_data (combo, change);
    change->replacement_session = qof_session_new (qof_book_new ());
    gnc_exchange_current_session (change->replacement_session);
}

static void
close_book_on_namespace_change (GtkComboBox *combo, gpointer data)
{
    auto change = static_cast<NamespaceChange *> (data);
    ++change->calls;
    g_signal_handlers_disconnect_by_data (combo, change);
    qof_book_mark_closed (change->book);
}

static void
replace_model_on_namespace_change (GtkComboBox *combo, gpointer data)
{
    auto change = static_cast<NamespaceChange *> (data);
    ++change->calls;
    g_signal_handlers_disconnect_by_data (combo, change);
    auto replacement = gtk_list_store_new (1, G_TYPE_STRING);
    gtk_combo_box_set_model (combo, GTK_TREE_MODEL (replacement));
    g_object_unref (replacement);
}

class NamespacePickerTest : public CommodityResponseTest
{
protected:
    void SetUp () override
    {
        CommodityResponseTest::SetUp ();
        m_book = qof_session_get_book (m_session);
        m_commodity = gnc_commodity_new (m_book, "Original", "NYSE", "ORG",
                                          nullptr, 100);
        gnc_commodity_table_insert (gnc_commodity_table_get_table (m_book),
                                   m_commodity);
        m_picker = gtk_combo_box_text_new ();
        g_object_ref_sink (m_picker);
        gtk_combo_box_text_append_text (GTK_COMBO_BOX_TEXT (m_picker), "old");
        gtk_combo_box_set_active (GTK_COMBO_BOX (m_picker), 0);
        m_original_model = gtk_combo_box_get_model (GTK_COMBO_BOX (m_picker));
        g_object_ref (m_original_model);
        m_change.book = m_book;
    }

    void TearDown () override
    {
        if (m_picker)
            gtk_widget_destroy (m_picker);
        g_clear_object (&m_picker);
        g_clear_object (&m_original_model);
        CommodityResponseTest::TearDown ();
    }

    gnc_commodity *m_commodity{};
    GtkWidget *m_picker{};
    GtkTreeModel *m_original_model{};
    NamespaceChange m_change{};
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

static void
destroy_parent_on_picker_change ([[maybe_unused]] GtkComboBox *picker,
                                 GtkWidget *parent)
{
    gtk_widget_destroy (parent);
}

TEST_F (CommodityResponseTest, CancelAndParentDestroyCompleteWithoutCommodity)
{
    auto parent = create_parent ();
    auto &result = m_primary;
    gnc_ui_new_commodity_async_full ("NYSE", GTK_WIDGET (parent), nullptr,
                                     nullptr, nullptr, nullptr, 100,
                                     completed, &result);
    auto dialog = find_child_dialog (parent);
    ASSERT_TRUE (GTK_IS_DIALOG (dialog));
    EXPECT_TRUE (gtk_window_get_modal (GTK_WINDOW (dialog)));
    EXPECT_TRUE (gtk_window_get_destroy_with_parent (GTK_WINDOW (dialog)));

    /* Help opens the external manual viewer, so it remains an E2E check. */
    retain_dialog (GTK_WIDGET (dialog));
    gtk_dialog_response (dialog, GTK_RESPONSE_CANCEL);
    EXPECT_EQ (result.calls, 1u);
    EXPECT_EQ (result.book, nullptr);
    EXPECT_EQ (result.commodity, nullptr);
    gtk_dialog_response (dialog, GTK_RESPONSE_OK);
    EXPECT_EQ (result.calls, 1u);
    gtk_widget_destroy (GTK_WIDGET (parent));

    auto &destroyed = m_secondary;
    parent = create_parent ();
    gnc_ui_new_commodity_async_full ("NYSE", GTK_WIDGET (parent), nullptr,
                                     nullptr, nullptr, nullptr, 100,
                                     completed, &destroyed);
    dialog = find_child_dialog (parent);
    ASSERT_TRUE (GTK_IS_DIALOG (dialog));
    retain_dialog (GTK_WIDGET (dialog));
    gtk_widget_destroy (GTK_WIDGET (parent));
    EXPECT_EQ (destroyed.calls, 1u);
    EXPECT_EQ (destroyed.book, nullptr);
    EXPECT_EQ (destroyed.commodity, nullptr);
}

TEST_F (CommodityResponseTest, SelectorNewChildCancelAndRepeatGuard)
{
    auto book = m_book;
    auto table = gnc_commodity_table_get_table (book);
    auto commodity = gnc_commodity_new (book, "Response test", "NYSE", "RSP",
                                        nullptr, 100);
    gnc_commodity_table_insert (table, commodity);
    auto parent = create_parent ();
    auto &result = m_primary;
    gnc_ui_select_commodity_async_full (commodity, GTK_WIDGET (parent),
                                        DIAG_COMM_ALL, nullptr, nullptr, nullptr,
                                        nullptr, completed, &result);
    auto selector = find_child_dialog (parent);
    ASSERT_TRUE (GTK_IS_DIALOG (selector));
    retain_dialog (GTK_WIDGET (selector));
    gtk_dialog_response (selector, GNC_RESPONSE_NEW);
    auto child = find_child_dialog (GTK_WINDOW (selector));
    ASSERT_TRUE (GTK_IS_DIALOG (child));
    retain_dialog (GTK_WIDGET (child));
    gtk_dialog_response (selector, GNC_RESPONSE_NEW);
    auto windows = gtk_window_list_toplevels ();
    std::uint32_t child_count = 0;
    for (auto node = windows; node; node = node->next)
        if (GTK_IS_DIALOG (node->data) &&
            gtk_window_get_transient_for (GTK_WINDOW (node->data)) ==
                GTK_WINDOW (selector))
            ++child_count;
    g_list_free (windows);
    EXPECT_EQ (child_count, 1u);

    gtk_dialog_response (child, GTK_RESPONSE_CANCEL);
    EXPECT_EQ (result.calls, 0u);
    gtk_dialog_response (selector, GTK_RESPONSE_CANCEL);
    EXPECT_EQ (result.calls, 1u);
    EXPECT_EQ (result.book, nullptr);
    EXPECT_EQ (result.commodity, nullptr);
    gtk_widget_destroy (GTK_WIDGET (parent));
}

TEST_F (CommodityResponseTest, ChildSuccessWithParentDestroyedDuringPickerUpdate)
{
    auto book = m_book;
    auto table = gnc_commodity_table_get_table (book);
    gnc_commodity_table_add_namespace (table, "NYSE", book);
    auto original = gnc_commodity_new (book, "Original", "NYSE", "ORG",
                                       nullptr, 100);
    gnc_commodity_table_insert (table, original);
    auto parent = GTK_WIDGET (create_parent ());
    auto &result = m_primary;
    gnc_ui_select_commodity_async_full (original, parent, DIAG_COMM_ALL,
                                        nullptr, nullptr, nullptr, nullptr,
                                        completed, &result);
    auto selector = find_child_dialog (GTK_WINDOW (parent));
    ASSERT_TRUE (GTK_IS_DIALOG (selector));
    retain_dialog (GTK_WIDGET (selector));
    auto namespace_picker = gnc::test::find_widget_by_buildable_name (GTK_WIDGET (selector), "ss_namespace_cbwe");
    ASSERT_TRUE (GTK_IS_COMBO_BOX (namespace_picker));
    g_signal_connect (namespace_picker, "changed",
                      G_CALLBACK (destroy_parent_on_picker_change), parent);
    gtk_dialog_response (selector, GNC_RESPONSE_NEW);
    auto child = find_child_dialog (GTK_WINDOW (selector));
    ASSERT_TRUE (GTK_IS_DIALOG (child));
    retain_dialog (GTK_WIDGET (child));
    auto fullname = gnc::test::find_widget_by_buildable_name (GTK_WIDGET (child), "fullname_entry");
    auto mnemonic = gnc::test::find_widget_by_buildable_name (GTK_WIDGET (child), "mnemonic_entry");
    ASSERT_TRUE (GTK_IS_ENTRY (fullname));
    ASSERT_TRUE (GTK_IS_ENTRY (mnemonic));
    gtk_entry_set_text (GTK_ENTRY (fullname), "Child created");
    gtk_entry_set_text (GTK_ENTRY (mnemonic), "CHD");
    gtk_dialog_response (child, GTK_RESPONSE_OK);
    EXPECT_EQ (result.calls, 1u);
    EXPECT_EQ (result.book, nullptr);
    EXPECT_EQ (result.commodity, nullptr);
}

TEST_F (CommodityResponseTest, CreateThenEditCommits)
{
    auto book = m_book;
    auto table = gnc_commodity_table_get_table (book);
    gnc_commodity_table_add_namespace (table, "NYSE", book);
    auto parent = create_parent ();
    auto &created = m_primary;
    gnc_ui_new_commodity_async_full ("NYSE", GTK_WIDGET (parent), nullptr,
                                     "New response test", "NRT", "NRT", 100,
                                     completed, &created);
    auto dialog = find_child_dialog (parent);
    ASSERT_TRUE (GTK_IS_DIALOG (dialog));
    retain_dialog (GTK_WIDGET (dialog));
    gtk_dialog_response (dialog, GTK_RESPONSE_OK);
    EXPECT_EQ (created.calls, 1u);
    EXPECT_TRUE (created.book == book);
    ASSERT_NE (created.commodity, nullptr);
    EXPECT_TRUE (gnc_commodity_table_lookup (table, "NYSE", "NRT") ==
                   created.commodity);

    auto &edited = m_secondary;
    gnc_ui_edit_commodity_async (created.commodity, GTK_WIDGET (parent),
                                 completed, &edited);
    dialog = find_child_dialog (parent);
    ASSERT_TRUE (GTK_IS_DIALOG (dialog));
    retain_dialog (GTK_WIDGET (dialog));
    auto fullname = gnc::test::find_widget_by_buildable_name (GTK_WIDGET (dialog), "fullname_entry");
    ASSERT_TRUE (GTK_IS_ENTRY (fullname));
    gtk_entry_set_text (GTK_ENTRY (fullname), "Edited response test");
    gtk_dialog_response (dialog, GTK_RESPONSE_OK);
    EXPECT_EQ (edited.calls, 1u);
    EXPECT_TRUE (edited.book == book);
    EXPECT_TRUE (edited.commodity == created.commodity);
    EXPECT_STREQ (gnc_commodity_get_fullname (created.commodity), "Edited response test");
    gtk_widget_destroy (GTK_WIDGET (parent));
}

TEST_F (NamespacePickerTest, SessionSwitchCancelsNamespaceUpdate)
{
    g_signal_connect (m_picker, "changed",
                      G_CALLBACK (switch_session_on_namespace_change), &m_change);
    gnc_ui_update_namespace_picker (
        m_picker, gnc_commodity_get_namespace (m_commodity),
        DIAG_COMM_ALL);

    EXPECT_EQ (m_change.calls, 1u);
    EXPECT_EQ (gtk_tree_model_iter_n_children (m_original_model, nullptr), 0);
    auto current_model = gtk_combo_box_get_model (GTK_COMBO_BOX (m_picker));
    EXPECT_EQ (gtk_tree_model_iter_n_children (current_model, nullptr), 0);
    EXPECT_EQ (current_model, m_original_model);
}

TEST_F (NamespacePickerTest, ModelReplacementCancelsNamespaceUpdate)
{
    g_signal_connect (m_picker, "changed",
                      G_CALLBACK (replace_model_on_namespace_change), &m_change);
    gnc_ui_update_namespace_picker (
        m_picker, gnc_commodity_get_namespace (m_commodity),
        DIAG_COMM_ALL);

    EXPECT_EQ (m_change.calls, 1u);
    EXPECT_EQ (gtk_tree_model_iter_n_children (m_original_model, nullptr), 0);
    auto current_model = gtk_combo_box_get_model (GTK_COMBO_BOX (m_picker));
    EXPECT_EQ (gtk_tree_model_iter_n_children (current_model, nullptr), 0);
    EXPECT_NE (current_model, m_original_model);
}

TEST_F (NamespacePickerTest, ClosedBookCancelsNamespaceUpdate)
{
    g_signal_connect (m_picker, "changed",
                      G_CALLBACK (close_book_on_namespace_change), &m_change);
    gnc_ui_update_namespace_picker (
        m_picker, gnc_commodity_get_namespace (m_commodity),
        DIAG_COMM_ALL);

    EXPECT_EQ (m_change.calls, 1u);
    EXPECT_EQ (gtk_tree_model_iter_n_children (m_original_model, nullptr), 0);
    auto current_model = gtk_combo_box_get_model (GTK_COMBO_BOX (m_picker));
    EXPECT_EQ (gtk_tree_model_iter_n_children (current_model, nullptr), 0);
    EXPECT_EQ (current_model, m_original_model);
}

int
main (int argc, char **argv)
{
    ::testing::InitGoogleTest (&argc, argv);
    if (!gtk_init_check (&argc, &argv))
        g_error ("A graphical display is required for commodity response tests");
    gnc::test::initialize_logging ();
    return RUN_ALL_TESTS ();
}
