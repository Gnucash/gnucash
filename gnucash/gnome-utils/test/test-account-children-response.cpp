/* Copyright (C) 2026 GnuCash contributors
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

#include "Account.h"
#include "cashobjects.h"
#include "dialog-account.h"
#include "gnc-commodity.h"
#include "gnc-component-manager.h"
#include "gnc-gsettings.h"
#include "gnc-session.h"
#include "gnc-tree-model-account-types.h"
#include "qof.h"

namespace
{
static GtkWidget *
find_control (GtkWidget *root, const char *name)
{
    if (GTK_IS_BUILDABLE (root) &&
        g_strcmp0 (gtk_buildable_get_name (GTK_BUILDABLE (root)), name) == 0)
        return root;
    if (!GTK_IS_CONTAINER (root))
        return nullptr;
    auto children = gtk_container_get_children (GTK_CONTAINER (root));
    GtkWidget *result = nullptr;
    for (auto node = children; node && !result; node = node->next)
        result = find_control (GTK_WIDGET (node->data), name);
    g_list_free (children);
    return result;
}

static GtkWidget *
find_window (GtkWindow *parent = nullptr)
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *result = nullptr;
    for (auto node = windows; node; node = node->next)
    {
        auto widget = GTK_WIDGET (node->data);
        if ((parent && GTK_IS_DIALOG (widget) &&
             gtk_window_get_transient_for (GTK_WINDOW (widget)) == parent) ||
            (!parent && g_strcmp0 (gtk_widget_get_name (widget),
                                   "gnc-id-account") == 0))
        {
            EXPECT_EQ (result, nullptr);
            result = widget;
        }
    }
    g_list_free (windows);
    return result;
}

struct CreationResult
{
    std::uint32_t calls{};
    Account *account{};
};

static void
creation_completed (Account *account, gpointer data)
{
    auto result = static_cast<CreationResult *> (data);
    ++result->calls;
    result->account = account;
}

class AccountChildrenResponseTest : public ::testing::Test
{
protected:
    static void SetUpTestSuite ()
    {
        g_setenv ("GNC_UNINSTALLED", "YES", true);
        g_setenv ("GSETTINGS_BACKEND", "memory", true);
        qof_init ();
        ASSERT_TRUE (cashobjects_register ());
        gnc_component_manager_init ();
        gnc_gsettings_load_backend ();
    }

    static void TearDownTestSuite ()
    {
        gnc_gsettings_shutdown ();
        gnc_component_manager_shutdown ();
        qof_close ();
    }

    void SetUp () override
    {
        m_session = qof_session_new (qof_book_new ());
        gnc_set_current_session (m_session);
        m_book = qof_session_get_book (m_session);
        auto root = gnc_account_create_root (m_book);
        m_currency = gnc_commodity_new (m_book, "Test currency", "CURRENCY",
                                        "TST", "", 100);
        m_currency = gnc_commodity_table_insert (
            gnc_commodity_table_get_table (m_book), m_currency);
        m_account = xaccMallocAccount (m_book);
        m_child = xaccMallocAccount (m_book);
        xaccAccountSetName (m_account, "Confirmed parent");
        xaccAccountSetName (m_child, "Confirmed child");
        xaccAccountSetType (m_account, ACCT_TYPE_BANK);
        xaccAccountSetType (m_child, ACCT_TYPE_BANK);
        xaccAccountSetCommodity (m_account, m_currency);
        xaccAccountSetCommodity (m_child, m_currency);
        gnc_account_append_child (root, m_account);
        gnc_account_append_child (m_account, m_child);

        m_owner = GTK_WINDOW (gtk_window_new (GTK_WINDOW_TOPLEVEL));
        g_object_ref_sink (m_owner);
        gtk_widget_realize (GTK_WIDGET (m_owner));
    }

    void TearDown () override
    {
        if (m_question)
            gtk_widget_destroy (GTK_WIDGET (m_question));
        g_clear_object (&m_question);
        if (m_edit_dialog)
            gtk_widget_destroy (m_edit_dialog);
        g_clear_object (&m_edit_dialog);
        if (m_creation_dialog)
            gtk_widget_destroy (GTK_WIDGET (m_creation_dialog));
        g_clear_object (&m_creation_dialog);
        if (m_owner)
        {
            gtk_widget_destroy (GTK_WIDGET (m_owner));
            g_object_unref (m_owner);
        }
        auto current_session = gnc_exchange_current_session (nullptr);
        if (current_session == m_session)
            m_session = nullptr;
        else if (current_session == m_replacement_session)
            m_replacement_session = nullptr;
        if (current_session)
            qof_session_destroy (current_session);
        if (m_replacement_session)
            qof_session_destroy (m_replacement_session);
        if (m_session)
            qof_session_destroy (m_session);
    }

    GtkWidget *start_type_change_confirmation ()
    {
        gnc_ui_edit_account_window (m_owner, m_account);
        m_edit_dialog = find_window ();
        if (m_edit_dialog)
            g_object_ref (m_edit_dialog);
        if (!GTK_IS_DIALOG (m_edit_dialog))
        {
            ADD_FAILURE () << "Account editor dialog was not created";
            return nullptr;
        }
        auto type = find_control (m_edit_dialog, "account_type_combo");
        if (!GTK_IS_COMBO_BOX (type))
        {
            ADD_FAILURE () << "Account type combo was not found";
            return nullptr;
        }
        gnc_tree_model_account_types_set_active_combo (
            GTK_COMBO_BOX (type), 1 << ACCT_TYPE_INCOME);
        gtk_dialog_response (GTK_DIALOG (m_edit_dialog), GTK_RESPONSE_OK);
        m_question = GTK_DIALOG (find_window (GTK_WINDOW (m_edit_dialog)));
        if (!m_question)
            ADD_FAILURE () << "Confirmation dialog was not created";
        else
            g_object_ref (m_question);
        return GTK_WIDGET (m_question);
    }

    void expect_account_types (GNCAccountType expected)
    {
        EXPECT_EQ (xaccAccountGetType (m_account), expected);
        EXPECT_EQ (xaccAccountGetType (m_child), expected);
    }

    GtkDialog *start_account_creation ()
    {
        auto types = g_list_prepend (nullptr, GINT_TO_POINTER (ACCT_TYPE_BANK));
        auto name = g_strdup ("Created child");
        gnc_ui_new_accounts_from_name_with_defaults_async (
            m_owner, name, types, m_currency, m_account, creation_completed,
            &m_creation_result);
        g_free (name);
        g_list_free (types);
        m_creation_dialog = GTK_DIALOG (find_window (m_owner));
        if (!GTK_IS_DIALOG (m_creation_dialog))
        {
            ADD_FAILURE () << "Account creation dialog was not created";
            return nullptr;
        }
        g_object_ref (m_creation_dialog);
        EXPECT_TRUE (gtk_window_get_modal (GTK_WINDOW (m_creation_dialog)));
        EXPECT_TRUE (gtk_window_get_destroy_with_parent (
                         GTK_WINDOW (m_creation_dialog)));
        EXPECT_EQ (m_creation_result.calls, 0u);
        return m_creation_dialog;
    }

    QofSession *m_session{};
    QofSession *m_replacement_session{};
    QofBook *m_book{};
    gnc_commodity *m_currency{};
    Account *m_account{};
    Account *m_child{};
    GtkWindow *m_owner{};
    GtkWidget *m_edit_dialog{};
    GtkDialog *m_question{};
    GtkDialog *m_creation_dialog{};
    CreationResult m_creation_result{};
};

TEST_F (AccountChildrenResponseTest, CancelLeavesParentAndChildTypesUnchanged)
{
    auto question = start_type_change_confirmation ();
    ASSERT_NE (question, nullptr);
    expect_account_types (ACCT_TYPE_BANK);
    gtk_dialog_response (GTK_DIALOG (question), GTK_RESPONSE_CANCEL);
    expect_account_types (ACCT_TYPE_BANK);
}

TEST_F (AccountChildrenResponseTest, AcceptChangesParentAndChildTypes)
{
    auto question = start_type_change_confirmation ();
    ASSERT_NE (question, nullptr);
    expect_account_types (ACCT_TYPE_BANK);
    gtk_dialog_response (GTK_DIALOG (question), GTK_RESPONSE_OK);
    expect_account_types (ACCT_TYPE_INCOME);
}

TEST_F (AccountChildrenResponseTest, LateConfirmationAfterParentDestroyIsIgnored)
{
    auto question = start_type_change_confirmation ();
    ASSERT_NE (question, nullptr);
    gtk_widget_destroy (m_edit_dialog);
    gtk_dialog_response (GTK_DIALOG (question), GTK_RESPONSE_OK);
    expect_account_types (ACCT_TYPE_BANK);
}

TEST_F (AccountChildrenResponseTest, ReadOnlyBookRejectsAcceptedTypeChange)
{
    auto question = start_type_change_confirmation ();
    ASSERT_NE (question, nullptr);
    qof_book_mark_readonly (m_book);
    gtk_dialog_response (GTK_DIALOG (question), GTK_RESPONSE_OK);
    expect_account_types (ACCT_TYPE_BANK);
}

TEST_F (AccountChildrenResponseTest, AcceptCreatesChildOnce)
{
    auto dialog = start_account_creation ();
    ASSERT_NE (dialog, nullptr);
    g_object_ref (dialog);
    gtk_dialog_response (dialog, GTK_RESPONSE_OK);
    ASSERT_EQ (m_creation_result.calls, 1u);
    ASSERT_NE (m_creation_result.account, nullptr);
    EXPECT_STREQ (xaccAccountGetName (m_creation_result.account), "Created child");
    EXPECT_EQ (gnc_account_get_parent (m_creation_result.account), m_account);
    EXPECT_EQ (gnc_account_get_book (m_creation_result.account), m_book);
    gtk_dialog_response (dialog, GTK_RESPONSE_OK);
    EXPECT_EQ (m_creation_result.calls, 1u);
    g_object_unref (dialog);
}

TEST_F (AccountChildrenResponseTest, CancelDoesNotCreateChild)
{
    auto dialog = start_account_creation ();
    ASSERT_NE (dialog, nullptr);
    g_object_ref (dialog);
    gtk_dialog_response (dialog, GTK_RESPONSE_CANCEL);
    EXPECT_EQ (m_creation_result.calls, 1u);
    EXPECT_EQ (m_creation_result.account, nullptr);
    gtk_dialog_response (dialog, GTK_RESPONSE_OK);
    EXPECT_EQ (m_creation_result.calls, 1u);
    g_object_unref (dialog);
}

TEST_F (AccountChildrenResponseTest, DestroyingOwnerCompletesCreationOnce)
{
    auto dialog = start_account_creation ();
    ASSERT_NE (dialog, nullptr);
    g_object_ref (dialog);
    gtk_widget_destroy (GTK_WIDGET (m_owner));
    EXPECT_EQ (m_creation_result.calls, 1u);
    EXPECT_EQ (m_creation_result.account, nullptr);
    gtk_dialog_response (dialog, GTK_RESPONSE_OK);
    EXPECT_EQ (m_creation_result.calls, 1u);
    g_object_unref (dialog);
}

TEST_F (AccountChildrenResponseTest, SessionSwitchRejectsStaleCreation)
{
    auto dialog = start_account_creation ();
    ASSERT_NE (dialog, nullptr);
    g_object_ref (dialog);
    m_replacement_session = qof_session_new (qof_book_new ());
    EXPECT_EQ (gnc_exchange_current_session (m_replacement_session), m_session);
    gtk_dialog_response (dialog, GTK_RESPONSE_OK);
    EXPECT_EQ (m_creation_result.calls, 1u);
    EXPECT_EQ (m_creation_result.account, nullptr);
    gtk_dialog_response (dialog, GTK_RESPONSE_OK);
    EXPECT_EQ (m_creation_result.calls, 1u);
    g_object_unref (dialog);
}
} // namespace

int
main (int argc, char **argv)
{
    ::testing::InitGoogleTest (&argc, argv);
    if (!gtk_init_check (&argc, &argv))
        g_error ("A graphical display is required for account response tests");
    g_log_set_always_fatal (static_cast<GLogLevelFlags> (
        G_LOG_FATAL_MASK | G_LOG_LEVEL_WARNING | G_LOG_LEVEL_CRITICAL));
    return RUN_ALL_TESTS ();
}
