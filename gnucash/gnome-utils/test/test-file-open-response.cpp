/* Copyright (C) 2026 GnuCash contributors
 * SPDX-License-Identifier: GPL-2.0-or-later
 */
#include <config.h>
#include <cstdint>
#include <gtk/gtk.h>
#include <gtest/gtest.h>
#include <vector>
#include "Account.h"
#include "cashobjects.h"
#include "qof-backend.hpp"
#include "gnc-backend-prov.hpp"
#include "gnc-component-manager.h"
#include "gnc-file.h"
#include "gnc-gsettings.h"
#include "gnc-gnome-utils.h"
#include "gnc-session.h"

namespace
{
QofBackendError begin_error;
QofBackendError load_error;
unsigned loads;

class OpenBackend : public QofBackend
{
public:
    void session_begin (QofSession *, const char *, SessionOpenMode mode) override
    {
        if (mode == SESSION_NORMAL_OPEN && begin_error != ERR_BACKEND_NO_ERR)
            set_error (begin_error);
    }
    void session_end () override {}
    void load (QofBook *book, QofBackendLoadType) override
    {
        ++loads;
        gnc_account_create_root (book);
        qof_book_mark_session_saved (book);
        if (load_error != ERR_BACKEND_NO_ERR)
            set_error (load_error);
    }
    void sync (QofBook *book) override { qof_book_mark_session_saved (book); }
    void safe_sync (QofBook *book) override { sync (book); }
};

class OpenProvider : public QofBackendProvider
{
public:
    OpenProvider () : QofBackendProvider ("Response open test", "xml") {}
    QofBackend *create_backend () override { return new OpenBackend; }
    bool type_check (const char *) override { return true; }
};

struct Result { unsigned calls = 0; bool opened = false; };

static void completed (gboolean opened, gpointer data)
{
    auto result = static_cast<Result *>(data);
    ++result->calls;
    result->opened = opened;
}

static GtkDialog *find_dialog (GtkWindow *parent)
{
    auto windows = gtk_window_list_toplevels ();
    GtkDialog *found = nullptr;
    for (auto node = windows; node; node = node->next)
        if (GTK_IS_MESSAGE_DIALOG (node->data) &&
            gtk_window_get_transient_for (GTK_WINDOW (node->data)) == parent)
        {
            EXPECT_EQ (found, nullptr);
            found = GTK_DIALOG (node->data);
        }
    g_list_free (windows);
    return found;
}

enum class OpenCase
{
    LockAccept, LockCancel, OwnerDestroy, SessionChange,
    SourceBookDestroy, DialogDestroy, OldFormatAccept, OldFormatReject
};

class FileOpenFixture : public ::testing::Test
{
protected:
    void SetUp () override
    {
        original = qof_session_new (qof_book_new ());
        book = qof_session_get_book (original);
        gnc_account_create_root (book);
        qof_book_mark_session_saved (book);
        gnc_set_current_session (original);
        parent = GTK_WINDOW (gtk_window_new (GTK_WINDOW_TOPLEVEL));
        g_object_ref_sink (parent);
        begin_error = ERR_BACKEND_NO_ERR;
        load_error = ERR_BACKEND_NO_ERR;
        loads = 0;
    }

    void TearDown () override
    {
        gtk_widget_destroy (GTK_WIDGET (parent));
        for (auto lease : leases)
            gnc_gui_end_session_operation (lease);
        g_clear_object (&retained_dialog);
        g_clear_object (&parent);
        auto current = gnc_exchange_current_session (nullptr);
        if (original && original != current)
            qof_session_destroy (original);
        if (current)
            qof_session_destroy (current);
    }

    QofSession *original{};
    QofSession *other{};
    QofBook *book{};
    GtkWindow *parent{};
    GtkDialog *retained_dialog{};
    Result result{};
    std::vector<std::uint32_t> leases;
};

class FileOpenResponseTest : public FileOpenFixture,
                             public ::testing::WithParamInterface<OpenCase>
{
};

TEST_P (FileOpenResponseTest, PreservesSessionUntilTerminalResponse)
{
    auto scenario = GetParam ();
    bool locked = scenario != OpenCase::OldFormatAccept &&
                  scenario != OpenCase::OldFormatReject;
    begin_error = locked ? ERR_BACKEND_LOCKED : ERR_BACKEND_NO_ERR;
    load_error = !locked ? ERR_FILEIO_FILE_TOO_OLD : ERR_BACKEND_NO_ERR;
    loads = 0;

    gnc_file_open_file_async (parent, "xml:///synthetic-response/open.gnucash", false,
                              completed, &result);
    EXPECT_EQ (result.calls, 0u);
    ASSERT_TRUE (gnc_get_current_session () == original);
    ASSERT_TRUE (qof_session_get_book (original) == book);
    retained_dialog = find_dialog (parent);
    auto dialog = retained_dialog;
    ASSERT_NE (dialog, nullptr);
    g_object_ref (dialog);

    if (scenario == OpenCase::OwnerDestroy)
        gtk_widget_destroy (GTK_WIDGET (parent));
    else
    {
        if (scenario == OpenCase::SessionChange || scenario == OpenCase::SourceBookDestroy)
        {
            other = qof_session_new (qof_book_new ());
            gnc_account_create_root (qof_session_get_book (other));
            qof_book_mark_session_saved (qof_session_get_book (other));
            ASSERT_TRUE (gnc_exchange_current_session (other) == original);
            if (scenario == OpenCase::SourceBookDestroy)
            {
                qof_session_destroy (original);
                original = nullptr;
            }
        }
        if (scenario == OpenCase::DialogDestroy)
            gtk_widget_destroy (GTK_WIDGET (dialog));
        else
            gtk_dialog_response (dialog, scenario == OpenCase::LockCancel ? GTK_RESPONSE_CANCEL :
                locked ? 2 /* Open Anyway */ :
                scenario == OpenCase::OldFormatAccept ? GTK_RESPONSE_YES : GTK_RESPONSE_NO);
    }
    EXPECT_EQ (result.calls, 1u);
    bool expected = scenario == OpenCase::LockAccept || scenario == OpenCase::OldFormatAccept;
    if (result.opened)
        original = nullptr;
    EXPECT_EQ (result.opened, expected);
    if (!expected)
    {
        ASSERT_TRUE (gnc_get_current_session () == (other ? other : original));
        if (scenario != OpenCase::SourceBookDestroy)
        {
            ASSERT_TRUE (qof_session_get_book (original) == book);
        }
    }
    gtk_dialog_response (dialog, GTK_RESPONSE_YES);
    EXPECT_EQ (result.calls, 1u);
}

TEST_F (FileOpenFixture, SessionOperationGateRejectsOverlappingCommands)
{
    auto first = gnc_gui_begin_session_operation (book);
    auto second = gnc_gui_begin_session_operation (book);
    leases = {first, second};
    EXPECT_NE (first, 0u);
    EXPECT_NE (second, first);
    ASSERT_TRUE (gnc_gui_session_operation_pending ());
    loads = 0;
    Result open, save, query;
    gnc_file_open_file_async (parent, "xml:///synthetic-response/open.gnucash", false,
                              completed, &open);
    gnc_file_save_async (parent, completed, &save);
    gnc_file_query_save_async (parent, true, completed, &query);
    for (auto result : {&open, &save, &query})
    {
        EXPECT_EQ (result->calls, 1u);
        EXPECT_FALSE (result->opened);
    }
    EXPECT_EQ (find_dialog (parent), nullptr);
    EXPECT_EQ (loads, 0u);
    ASSERT_TRUE (gnc_get_current_session () == original);
    gnc_gui_end_session_operation (first);
    gnc_gui_end_session_operation (first);
    ASSERT_TRUE (gnc_gui_session_operation_pending ());
    gnc_gui_end_session_operation (second);
    EXPECT_FALSE (gnc_gui_session_operation_pending ());

    begin_error = ERR_BACKEND_LOCKED;
    load_error = ERR_BACKEND_NO_ERR;
    gnc_file_open_file_async (parent, "xml:///synthetic-response/open.gnucash", false,
                              completed, &result);
    EXPECT_EQ (result.calls, 0u);
    EXPECT_EQ (gnc_gui_begin_session_operation (book), 0u);
    auto dialog = find_dialog (parent);
    ASSERT_NE (dialog, nullptr);
    gtk_dialog_response (dialog, GTK_RESPONSE_CANCEL);
    EXPECT_EQ (result.calls, 1u);
    auto next = gnc_gui_begin_session_operation (book);
    leases.push_back (next);
    EXPECT_NE (next, 0u);
    gnc_gui_end_session_operation (next);
}
INSTANTIATE_TEST_SUITE_P (Responses, FileOpenResponseTest,
    ::testing::Values (OpenCase::LockAccept, OpenCase::LockCancel,
        OpenCase::OwnerDestroy, OpenCase::SessionChange,
        OpenCase::SourceBookDestroy, OpenCase::DialogDestroy,
        OpenCase::OldFormatAccept, OpenCase::OldFormatReject),
    [] (const auto &info) {
        switch (info.param)
        {
        case OpenCase::LockAccept: return "LockAccept";
        case OpenCase::LockCancel: return "LockCancel";
        case OpenCase::OwnerDestroy: return "OwnerDestroy";
        case OpenCase::SessionChange: return "SessionChange";
        case OpenCase::SourceBookDestroy: return "SourceBookDestroy";
        case OpenCase::DialogDestroy: return "DialogDestroy";
        case OpenCase::OldFormatAccept: return "OldFormatAccept";
        case OpenCase::OldFormatReject: return "OldFormatReject";
        }
        return "Unknown";
    });
}

int main (int argc, char **argv)
{
    ::testing::InitGoogleTest (&argc, argv);
    if (!gtk_init_check (&argc, &argv))
    {
        g_printerr ("GTK display initialization failed for file open response tests.\n");
        return 1;
    }
    qof_init ();
    if (!cashobjects_register ())
        g_error ("Could not register cash objects for file open tests");
    gnc_component_manager_init ();
    gnc_gsettings_load_backend ();
    qof_backend_unregister_all_providers ();
    qof_backend_register_provider (QofBackendProvider_ptr {new OpenProvider});
    g_log_set_always_fatal (static_cast<GLogLevelFlags> (
        G_LOG_FATAL_MASK | G_LOG_LEVEL_WARNING | G_LOG_LEVEL_CRITICAL));
    auto status = RUN_ALL_TESTS ();
    qof_backend_unregister_all_providers ();
    gnc_component_manager_shutdown ();
    gnc_gsettings_shutdown ();
    qof_close ();
    return status;
}
