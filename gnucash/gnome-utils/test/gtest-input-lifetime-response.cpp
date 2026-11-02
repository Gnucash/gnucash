/* Copyright (C) 2026 GnuCash contributors
 * SPDX-License-Identifier: GPL-2.0-or-later
 */
#include <config.h>
#include <cstdint>
#include <gtk/gtk.h>
#include "gtk-test-utils.hpp"

#include <gtest/gtest.h>
#include "googletest-glib-log-handler.hpp"
#include <string>
#include "dialog-dup-trans.h"
#include "dialog-transfer.h"
#include "cashobjects.h"
#include "Account.h"
#include "gnc-commodity.h"
#include "gnc-gsettings.h"
#include "gnc-component-manager.h"
#include "gnc-session.h"
#include "gnc-ui.h"

enum class InputKind { credentials, duplicate };
enum class InputAction { accept, cancel, owner_destroy, dialog_destroy, destroy_owner_on_response };
struct InputCase { InputKind kind; InputAction action; };
enum class TransferAction { cancel, owner_destroy, dialog_destroy, close };

struct Result
{
    std::uint32_t calls{};
    bool accepted{};
    gchar *username{};
    gchar *password{};
    GncDupTransResult *duplicate{};
};

static void credentials_finished (gboolean accepted, gchar *user, gchar *password, gpointer data)
{
    auto result = static_cast<Result *> (data);
    ++result->calls;
    result->accepted = accepted;
    result->username = user;
    result->password = password;
}

static void duplicate_finished (GncDupTransResult *duplicate, gpointer data)
{
    auto result = static_cast<Result *> (data);
    ++result->calls;
    result->accepted = duplicate != nullptr;
    result->duplicate = duplicate;
}

static void close_parent (GtkWidget *, gpointer parent)
{
    gtk_widget_destroy (GTK_WIDGET (parent));
}

class InputLifetimeTest : public ::testing::TestWithParam<InputCase>
{
protected:
    void SetUp () override
    {
        m_parent = GTK_WINDOW (gtk_window_new (GTK_WINDOW_TOPLEVEL));
        g_object_ref_sink (m_parent);
        m_result = {};
        m_dialog = nullptr;
    }
    void TearDown () override
    {
        if (m_parent)
            gtk_widget_destroy (GTK_WIDGET (m_parent));
        if (m_dialog)
            g_object_unref (m_dialog);
        if (m_parent)
            g_object_unref (m_parent);
        g_free (m_result.username);
        g_free (m_result.password);
        gnc_dup_trans_result_free (m_result.duplicate);
    }
    GtkWindow *m_parent{};
    GtkDialog *m_dialog{};
    Result m_result{};
};

TEST_P (InputLifetimeTest, InputCompletion)
{
    const auto test_case = GetParam ();
    const auto duplicate = test_case.kind == InputKind::duplicate;
    const auto action = test_case.action;
    auto parent = m_parent;
    auto &result = m_result;
    if (duplicate)
        gnc_dup_trans_dialog_async (parent, "Duplicate", "Test", TRUE,
            1700000000, "10", "20", "test-link", duplicate_finished, &result);
    else
        gnc_get_username_password_async (parent, "Test", "Zähler", "synthetic-password",
                                         credentials_finished, &result);
    EXPECT_EQ (result.calls, 0u);
    auto windows = gtk_window_list_toplevels ();
    GtkDialog *dialog = nullptr;
    for (auto node = windows; node; node = node->next)
        if (GTK_IS_DIALOG (node->data) &&
            gtk_window_get_transient_for (GTK_WINDOW (node->data)) == parent)
        {
            EXPECT_EQ (dialog, nullptr);
            dialog = GTK_DIALOG (node->data);
        }
    g_list_free (windows);
    ASSERT_NE (dialog, nullptr);
    m_dialog = dialog;
    g_object_ref (m_dialog);
    EXPECT_TRUE (gtk_window_get_modal (GTK_WINDOW (dialog)));
    if (duplicate)
    {
        auto num_entry = gnc::test::find_widget_by_buildable_name (GTK_WIDGET (dialog), "num_entry");
        auto tnum_entry = gnc::test::find_widget_by_buildable_name (GTK_WIDGET (dialog), "tnum_entry");
        auto link_check = gnc::test::find_widget_by_buildable_name (GTK_WIDGET (dialog), "link_check_button");
        ASSERT_NE (num_entry, nullptr);
        ASSERT_NE (tnum_entry, nullptr);
        ASSERT_NE (link_check, nullptr);
        gtk_entry_set_text (GTK_ENTRY (num_entry), "11");
        gtk_entry_set_text (GTK_ENTRY (tnum_entry), "21");
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (link_check), TRUE);
    }
    if (action == InputAction::owner_destroy)
        gtk_widget_destroy (GTK_WIDGET (parent));
    else if (action == InputAction::dialog_destroy)
        gtk_widget_destroy (GTK_WIDGET (dialog));
    else
    {
        if (action == InputAction::destroy_owner_on_response)
            g_signal_connect (dialog, "destroy", G_CALLBACK (close_parent), parent);
        gtk_dialog_response (dialog, action == InputAction::cancel ? GTK_RESPONSE_CANCEL : GTK_RESPONSE_OK);
    }
    ASSERT_EQ (result.calls, 1u);
    EXPECT_EQ (result.accepted, action == InputAction::accept);
    if (action == InputAction::accept && duplicate)
    {
        ASSERT_NE (result.duplicate, nullptr);
        EXPECT_STREQ (result.duplicate->num, "11");
        EXPECT_STREQ (result.duplicate->tnum, "21");
        EXPECT_STREQ (result.duplicate->doclink, "test-link");
        EXPECT_TRUE (g_date_valid (&result.duplicate->gdate));
    }
    else if (action == InputAction::accept)
    {
        EXPECT_STREQ (result.username, "Zähler");
        EXPECT_STREQ (result.password, "synthetic-password");
    }
    else
    {
        EXPECT_EQ (result.username, nullptr);
        EXPECT_EQ (result.password, nullptr);
        EXPECT_EQ (result.duplicate, nullptr);
    }
    gtk_dialog_response (dialog, GTK_RESPONSE_OK);
    EXPECT_EQ (result.calls, 1u);
}

static std::string
input_case_name (const ::testing::TestParamInfo<InputCase> &info)
{
        const char *kinds[] = {"Credentials", "Duplicate"};
        const char *actions[] = {"Accept", "Cancel", "OwnerDestroy",
                                 "DialogDestroy", "DestroyOwnerOnResponse"};
        return std::string (kinds[static_cast<int> (info.param.kind)]) +
               actions[static_cast<int> (info.param.action)];

}

static std::string
transfer_case_name (const ::testing::TestParamInfo<TransferAction> &info)
{
        const char *names[] = {"Cancel", "OwnerDestroy", "DialogDestroy", "Close"};
        return names[static_cast<int> (info.param)];

}

INSTANTIATE_TEST_SUITE_P (
    CredentialsAndDuplicateTransactions, InputLifetimeTest,
    ::testing::Values (
        InputCase {InputKind::credentials, InputAction::accept},
        InputCase {InputKind::credentials, InputAction::cancel},
        InputCase {InputKind::credentials, InputAction::owner_destroy},
        InputCase {InputKind::credentials, InputAction::dialog_destroy},
        InputCase {InputKind::credentials, InputAction::destroy_owner_on_response},
        InputCase {InputKind::duplicate, InputAction::accept},
        InputCase {InputKind::duplicate, InputAction::cancel},
        InputCase {InputKind::duplicate, InputAction::owner_destroy},
        InputCase {InputKind::duplicate, InputAction::dialog_destroy},
        InputCase {InputKind::duplicate, InputAction::destroy_owner_on_response}),
    input_case_name);

static void transfer_finished (gboolean accepted, gpointer data)
{
    auto result = static_cast<Result *> (data);
    ++result->calls;
    result->accepted = accepted;
}

class TransferLifetimeTest : public ::testing::TestWithParam<TransferAction>
{
protected:
    void SetUp () override
    {
        m_session = qof_session_new (qof_book_new ());
        gnc_set_current_session (m_session);
        m_book = qof_session_get_book (m_session);
        auto root = gnc_account_create_root (m_book);
        m_account = xaccMallocAccount (m_book);
        xaccAccountBeginEdit (m_account);
        xaccAccountSetName (m_account, "Cash");
        xaccAccountSetType (m_account, ACCT_TYPE_ASSET);
        m_currency = gnc_commodity_table_lookup (
            gnc_commodity_table_get_table (m_book), "CURRENCY", "EUR");
        ASSERT_NE (m_currency, nullptr);
        xaccAccountSetCommodity (m_account, m_currency);
        gnc_account_append_child (root, m_account);
        xaccAccountCommitEdit (m_account);
        m_parent = GTK_WINDOW (gtk_window_new (GTK_WINDOW_TOPLEVEL));
        g_object_ref_sink (m_parent);
        m_result = {};
        m_dialog = nullptr;
        m_transfer = nullptr;
    }
    void TearDown () override
    {
        if (m_parent)
            gtk_widget_destroy (GTK_WIDGET (m_parent));
        if (m_dialog)
            g_object_unref (m_dialog);
        if (m_parent)
            g_object_unref (m_parent);
        if (gnc_current_session_exist ())
            gnc_clear_current_session ();
    }
    GtkWindow *m_parent{};
    GtkDialog *m_dialog{};
    QofSession *m_session{};
    QofBook *m_book{};
    Account *m_account{};
    gnc_commodity *m_currency{};
    XferDialog *m_transfer{};
    Result m_result{};
};

TEST_P (TransferLifetimeTest, CancellationCompletesOnce)
{
    auto action = GetParam ();
    auto account = m_account;
    auto parent = m_parent;
    gtk_widget_show (GTK_WIDGET (parent));
    m_transfer = gnc_xfer_dialog (GTK_WIDGET (parent), account);
    gnc_xfer_dialog_run_async (m_transfer, transfer_finished, &m_result);
    auto &result = m_result;
    EXPECT_EQ (result.calls, 0u);
    auto windows = gtk_window_list_toplevels ();
    GtkDialog *dialog = nullptr;
    for (auto node = windows; node; node = node->next)
        if (GTK_IS_DIALOG (node->data) &&
            gtk_window_get_transient_for (GTK_WINDOW (node->data)) == parent)
            dialog = GTK_DIALOG (node->data);
    g_list_free (windows);
    ASSERT_NE (dialog, nullptr);
    m_dialog = dialog;
    g_object_ref (m_dialog);
    if (action == TransferAction::owner_destroy) gtk_widget_destroy (GTK_WIDGET (parent));
    else if (action == TransferAction::dialog_destroy) gtk_widget_destroy (GTK_WIDGET (dialog));
    else if (action == TransferAction::close) gnc_xfer_dialog_close (m_transfer);
    else gtk_dialog_response (dialog, GTK_RESPONSE_CANCEL);
    EXPECT_EQ (result.calls, 1u);
    EXPECT_FALSE (result.accepted);
    gtk_dialog_response (dialog, GTK_RESPONSE_OK);
    EXPECT_EQ (result.calls, 1u);
    m_transfer = nullptr;
}

INSTANTIATE_TEST_SUITE_P (
    TransferCancellation, TransferLifetimeTest,
    ::testing::Values (TransferAction::cancel, TransferAction::owner_destroy,
                       TransferAction::dialog_destroy, TransferAction::close),
    transfer_case_name);

int main (int argc, char **argv)
{
    ::testing::InitGoogleTest (&argc, argv);
    if (!gtk_init_check (&argc, &argv))
        g_error ("A graphical display is required for input lifetime tests");
    qof_init ();
    if (!cashobjects_register ())
        g_error ("Failed to register cash objects");
    gnc_component_manager_init ();
    gnc_gsettings_load_backend ();
    gnc::test::initialize_logging ();
    auto status = RUN_ALL_TESTS ();
    gnc_component_manager_shutdown ();
    gnc_gsettings_shutdown ();
    qof_close ();
    return status;
}
