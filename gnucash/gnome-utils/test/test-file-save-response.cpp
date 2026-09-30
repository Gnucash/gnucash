/* Copyright (C) 2026 GnuCash contributors
 * SPDX-License-Identifier: GPL-2.0-or-later
 */
#include <config.h>
#include <gtk/gtk.h>
#include <gtest/gtest.h>
#include <vector>
#include "Account.h"
#include "cashobjects.h"
#include "qof-backend.hpp"
#include "gnc-backend-prov.hpp"
#include "gnc-component-manager.h"
#include "gnc-engine.h"
#include "gnc-file.h"
#include "gnc-gsettings.h"
#include "gnc-session.h"
#include "gnc-prefs.h"
#include "gnc-uri-utils.h"

namespace
{
bool fail_write;
bool require_overwrite;
guint writes;
QofBook *expected_book;

class SaveBackend : public QofBackend
{
public:
    void session_begin(QofSession *, const char *, SessionOpenMode mode) override
    {
        if (require_overwrite && mode == SESSION_NEW_STORE)
            set_error(ERR_BACKEND_STORE_EXISTS);
    }
    void session_end() override {}
    void load(QofBook *, QofBackendLoadType) override {}
    void sync(QofBook *book) override
    {
        ++writes;
        EXPECT_TRUE (gnc_get_current_session() != nullptr);
        EXPECT_TRUE (qof_session_get_book(gnc_get_current_session()) == book);
        EXPECT_TRUE (book == expected_book);
        if (fail_write)
            set_error(ERR_FILEIO_WRITE_ERROR);
        else
            qof_book_mark_session_saved(book);
    }
    void safe_sync(QofBook *book) override { sync(book); }
};

class SaveProvider : public QofBackendProvider
{
public:
    SaveProvider() : QofBackendProvider("Response save test", "xml") {}
    QofBackend *create_backend() override { return new SaveBackend; }
    bool type_check(const char *) override { return true; }
};

struct Result
{
    guint calls = 0;
    gboolean saved = FALSE;
};

QofBook *dirty_book()
{
    auto book = qof_book_new();
    auto root = gnc_account_create_root(book);
    auto account = xaccMallocAccount(book);
    xaccAccountBeginEdit(account);
    xaccAccountSetName(account, "Save response account");
    gnc_account_append_child(root, account);
    xaccAccountCommitEdit(account);
    qof_book_mark_session_dirty(book);
    return book;
}

void completed(gboolean saved, gpointer data)
{
    auto result = static_cast<Result *>(data);
    ++result->calls;
    result->saved = saved;
}

GtkDialog *find_dialog(GtkWindow *parent, bool chooser = false)
{
    GtkDialog *found = nullptr;
    auto windows = gtk_window_list_toplevels();
    for (auto node = windows; node; node = node->next)
        if (GTK_IS_DIALOG(node->data) &&
            gtk_window_get_transient_for(GTK_WINDOW(node->data)) == parent &&
            (chooser ? GTK_IS_FILE_CHOOSER_DIALOG(node->data) :
                       GTK_IS_MESSAGE_DIALOG(node->data)))
        {
            EXPECT_EQ (found, nullptr);
            found = GTK_DIALOG(node->data);
        }
    g_list_free(windows);
    return found;
}

void close_dialogs(GtkWindow *parent)
{
    auto windows = gtk_window_list_toplevels();
    for (auto node = windows; node; node = node->next)
        if (GTK_IS_DIALOG(node->data) &&
            gtk_window_get_transient_for(GTK_WINDOW(node->data)) == parent)
            gtk_widget_destroy(GTK_WIDGET(node->data));
    g_list_free(windows);
}

enum class SaveAsCase { Success, WriteError, Overwrite, Cancel, OwnerDestroy, SessionChange };
enum class QueryCase { Discard, Cancel, OwnerDestroy, SessionChange, SaveCancelRetry, WindowClose };
enum class RecoveryCase { Cancel, OwnerDestroy, SessionChange, WindowClose };

class FileSaveFixture : public ::testing::Test
{
protected:
    void SetUp () override
    {
        fail_write = false;
        require_overwrite = false;
        writes = 0;
        book = dirty_book ();
        expected_book = book;
        original = qof_session_new (book);
        initial_session = original;
        ASSERT_EQ (gnc_exchange_current_session (original), nullptr);
        parent = GTK_WINDOW (gtk_window_new (GTK_WINDOW_TOPLEVEL));
        g_object_ref_sink (parent);
        filename = g_build_filename (g_get_tmp_dir (), "gnc-response-save-test.gnucash", nullptr);
    }

    void TearDown () override
    {
        if (parent)
        {
            close_dialogs (parent);
            gtk_widget_destroy (GTK_WIDGET (parent));
        }
        for (auto dialog : retained_dialogs)
            g_object_unref (dialog);
        g_clear_object (&parent);
        auto current = gnc_exchange_current_session (nullptr);
        if (original && original != current)
            qof_session_destroy (original);
        if (current)
            qof_session_destroy (current);
        g_free (filename);
        expected_book = nullptr;
    }

    void retain (GtkDialog *dialog)
    {
        g_object_ref (dialog);
        retained_dialogs.push_back (dialog);
    }

    void switch_session ()
    {
        other = qof_session_new (qof_book_new ());
        EXPECT_EQ (gnc_exchange_current_session (other), original);
    }

    QofBook *book{};
    QofSession *original{};
    QofSession *initial_session{};
    QofSession *other{};
    GtkWindow *parent{};
    gchar *filename{};
    Result result{};
    std::vector<GtkDialog *> retained_dialogs;
};

class SaveAsResponseTest : public FileSaveFixture,
                           public ::testing::WithParamInterface<SaveAsCase> {};
class SaveQueryResponseTest : public FileSaveFixture,
                              public ::testing::WithParamInterface<QueryCase> {};
class SaveRecoveryResponseTest : public FileSaveFixture,
                                 public ::testing::WithParamInterface<RecoveryCase> {};
TEST_P (SaveAsResponseTest, CompletesOnceAndPreservesDisplayedBook)
{
    auto scenario = GetParam ();
    fail_write = scenario == SaveAsCase::WriteError;
    require_overwrite = scenario != SaveAsCase::Success && !fail_write;
    gnc_file_do_save_as_async(parent, filename, completed, &result);
    if (require_overwrite)
    {
        auto question = find_dialog(parent);
        ASSERT_NE (question, nullptr);
        retain (question);
        EXPECT_EQ (result.calls, 0u);
        EXPECT_TRUE (gnc_file_save_in_progress());
        EXPECT_TRUE (gnc_get_current_session() == original);
        EXPECT_TRUE (qof_session_get_book(original) == book);
        Result duplicate;
        gnc_file_save_async(parent, completed, &duplicate);
        EXPECT_EQ (duplicate.calls, 1u);
        EXPECT_FALSE (duplicate.saved);
        // A rejected concurrent command must not terminate the first operation.
        EXPECT_TRUE (gnc_file_save_in_progress());
        if (scenario == SaveAsCase::OwnerDestroy)
            gtk_widget_destroy(GTK_WIDGET(parent));
        else
        {
            if (scenario == SaveAsCase::SessionChange)
            {
                switch_session ();
            }
            gtk_dialog_response(question, scenario == SaveAsCase::Cancel ?
                                 GTK_RESPONSE_CANCEL : GTK_RESPONSE_YES);
        }
        EXPECT_EQ (result.calls, 1u);
        gtk_dialog_response(question, GTK_RESPONSE_YES);
        EXPECT_EQ (result.calls, 1u);
    }
    if (result.saved) original = nullptr;
    EXPECT_EQ (result.calls, 1u);
    EXPECT_FALSE (gnc_file_save_in_progress());
    auto success = scenario == SaveAsCase::Success || scenario == SaveAsCase::Overwrite;
    EXPECT_EQ (result.saved, success);
    EXPECT_EQ (writes, (success || scenario == SaveAsCase::WriteError) ? 1u : 0u);
    if (success)
    {
        EXPECT_TRUE (gnc_get_current_session() != initial_session);
        EXPECT_TRUE (qof_session_get_book(gnc_get_current_session()) == book);
        EXPECT_FALSE (qof_book_session_not_saved(book));
    }
    else
    {
        EXPECT_TRUE (gnc_get_current_session() == (other ? other : original));
        EXPECT_TRUE (qof_session_get_book(original) == book);
        EXPECT_TRUE (qof_book_session_not_saved(book));
    }
}

TEST_F (FileSaveFixture, ChoosingCancelKeepsUnsavedBook)
{
    gnc_file_save_async(parent, completed, &result);
    auto chooser = find_dialog(parent, true);
    ASSERT_NE (chooser, nullptr);
    EXPECT_EQ (result.calls, 0u);
    gtk_dialog_response(chooser, GTK_RESPONSE_CANCEL);
    EXPECT_EQ (result.calls, 1u);
    EXPECT_FALSE (result.saved);
    EXPECT_TRUE (gnc_get_current_session() == original);
    EXPECT_TRUE (qof_book_session_not_saved(book));
}

TEST_P (SaveQueryResponseTest, DestructiveContinuationRequiresDecision)
{
    auto scenario = GetParam ();
    gnc_file_query_save_async(parent, TRUE, completed, &result);
    auto question = find_dialog(parent);
    ASSERT_NE (question, nullptr);
    retain (question);
    EXPECT_EQ (result.calls, 0u);
    if (scenario == QueryCase::OwnerDestroy)
        gtk_widget_destroy(GTK_WIDGET(parent));
    else if (scenario == QueryCase::SessionChange)
    {
        switch_session ();
        gtk_dialog_response(question, GTK_RESPONSE_OK);
    }
    else if (scenario == QueryCase::SaveCancelRetry)
    {
        // Save has no destination: cancellation must return to the query,
        // rather than allow the destructive continuation to run.
        gtk_dialog_response(question, GTK_RESPONSE_YES);
        auto chooser = find_dialog(parent, true);
        ASSERT_NE (chooser, nullptr);
        EXPECT_EQ (result.calls, 0u);
        gtk_dialog_response(chooser, GTK_RESPONSE_CANCEL);
        auto retry = find_dialog(parent);
        ASSERT_NE (retry, nullptr);
        EXPECT_EQ (result.calls, 0u);
        EXPECT_TRUE (qof_book_session_not_saved(book));
        gtk_dialog_response(retry, GTK_RESPONSE_OK);
    }
    else if (scenario == QueryCase::WindowClose)
    {
        gtk_window_close(GTK_WINDOW(question));
        auto deadline = g_get_monotonic_time() + 5 * G_USEC_PER_SEC;
        while (!result.calls && g_get_monotonic_time() < deadline)
        {
            g_main_context_iteration(nullptr, false);
            g_usleep(1000);
        }
    }
    else
        gtk_dialog_response(question, scenario == QueryCase::Discard ? GTK_RESPONSE_OK : GTK_RESPONSE_CANCEL);
    EXPECT_EQ (result.calls, 1u);
    EXPECT_EQ (result.saved, scenario == QueryCase::Discard || scenario == QueryCase::SaveCancelRetry);
    gtk_dialog_response(question, GTK_RESPONSE_OK);
    EXPECT_EQ (result.calls, 1u);
    EXPECT_TRUE (qof_session_get_book(original) == book);
    EXPECT_TRUE (qof_book_session_not_saved(book));
}

TEST_P (SaveRecoveryResponseTest, FailedWriteKeepsUnsavedBook)
{
    auto scenario = GetParam ();
    fail_write = true;
    auto uri = gnc_uri_create_uri ("xml", nullptr, 0, nullptr, nullptr,
                                   filename);
    ASSERT_NE (uri, nullptr);
    qof_session_begin (original, uri, SESSION_NORMAL_OPEN);
    g_free (uri);
    ASSERT_EQ (qof_session_get_error (original), ERR_BACKEND_NO_ERR);
    gnc_file_save_async(parent, completed, &result);
    auto error = find_dialog(parent);
    ASSERT_NE (error, nullptr);
    retain (error);
    EXPECT_EQ (writes, 1u);
    EXPECT_EQ (result.calls, 0u);
    EXPECT_TRUE (gnc_file_save_in_progress());
    EXPECT_TRUE (qof_book_session_not_saved(book));
    if (scenario == RecoveryCase::OwnerDestroy)
        gtk_widget_destroy(GTK_WIDGET(parent));
    else
    {
        if (scenario == RecoveryCase::SessionChange)
        {
            switch_session ();
        }
        if (scenario == RecoveryCase::WindowClose)
        {
            gtk_window_close(GTK_WINDOW(error));
            auto deadline = g_get_monotonic_time() + 5 * G_USEC_PER_SEC;
            while (!find_dialog(parent, true) && g_get_monotonic_time() < deadline)
            {
                g_main_context_iteration(nullptr, false);
                g_usleep(1000);
            }
        }
        else
            gtk_dialog_response(error, GTK_RESPONSE_CLOSE);
        if (scenario == RecoveryCase::Cancel || scenario == RecoveryCase::WindowClose)
        {
            auto chooser = find_dialog(parent, true);
            ASSERT_NE (chooser, nullptr);
            EXPECT_EQ (result.calls, 0u);
            gtk_dialog_response(chooser, GTK_RESPONSE_CANCEL);
        }
    }
    EXPECT_EQ (result.calls, 1u);
    EXPECT_FALSE (result.saved);
    EXPECT_FALSE (gnc_file_save_in_progress());
    EXPECT_TRUE (qof_book_session_not_saved(book));
    gtk_dialog_response(error, GTK_RESPONSE_CLOSE);
    EXPECT_EQ (result.calls, 1u);
}

INSTANTIATE_TEST_SUITE_P (Responses, SaveAsResponseTest,
    ::testing::Values (SaveAsCase::Success, SaveAsCase::WriteError, SaveAsCase::Overwrite, SaveAsCase::Cancel, SaveAsCase::OwnerDestroy, SaveAsCase::SessionChange),
    [] (const auto &info) {
        switch (info.param)
        {
        case SaveAsCase::Success: return "Success";
        case SaveAsCase::WriteError: return "WriteError";
        case SaveAsCase::Overwrite: return "Overwrite";
        case SaveAsCase::Cancel: return "Cancel";
        case SaveAsCase::OwnerDestroy: return "OwnerDestroy";
        case SaveAsCase::SessionChange: return "SessionChange";
        }
        return "Unknown";
    });

INSTANTIATE_TEST_SUITE_P (Responses, SaveQueryResponseTest,
    ::testing::Values (QueryCase::Discard, QueryCase::Cancel, QueryCase::OwnerDestroy, QueryCase::SessionChange, QueryCase::SaveCancelRetry, QueryCase::WindowClose),
    [] (const auto &info) {
        switch (info.param)
        {
        case QueryCase::Discard: return "Discard";
        case QueryCase::Cancel: return "Cancel";
        case QueryCase::OwnerDestroy: return "OwnerDestroy";
        case QueryCase::SessionChange: return "SessionChange";
        case QueryCase::SaveCancelRetry: return "SaveCancelRetry";
        case QueryCase::WindowClose: return "WindowClose";
        }
        return "Unknown";
    });

INSTANTIATE_TEST_SUITE_P (Responses, SaveRecoveryResponseTest,
    ::testing::Values (RecoveryCase::Cancel, RecoveryCase::OwnerDestroy, RecoveryCase::SessionChange, RecoveryCase::WindowClose),
    [] (const auto &info) {
        switch (info.param)
        {
        case RecoveryCase::Cancel: return "Cancel";
        case RecoveryCase::OwnerDestroy: return "OwnerDestroy";
        case RecoveryCase::SessionChange: return "SessionChange";
        case RecoveryCase::WindowClose: return "WindowClose";
        }
        return "Unknown";
    });
}

int main (int argc, char **argv)
{
    ::testing::InitGoogleTest (&argc, argv);
    if (!gtk_init_check (&argc, &argv))
    {
        g_printerr ("GTK display initialization failed for file save response tests.\n");
        return 1;
    }
    qof_init ();
    if (!cashobjects_register ())
        g_error ("Could not register cash objects for file save tests");
    gnc_component_manager_init ();
    gnc_gsettings_load_backend ();
    qof_backend_unregister_all_providers ();
    qof_backend_register_provider (QofBackendProvider_ptr{new SaveProvider});
    g_log_set_always_fatal (static_cast<GLogLevelFlags> (
        G_LOG_FATAL_MASK | G_LOG_LEVEL_WARNING | G_LOG_LEVEL_CRITICAL));
    auto result = RUN_ALL_TESTS ();
    qof_backend_unregister_all_providers ();
    gnc_component_manager_shutdown ();
    gnc_gsettings_shutdown ();
    qof_close ();
    return result;
}
