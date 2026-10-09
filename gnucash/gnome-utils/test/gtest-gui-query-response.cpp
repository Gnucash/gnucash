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
#include <gtest/gtest.h>
#include "googletest-glib-log-handler.hpp"
#include <string>
#include "dialog-utils.h"
#include "gnc-gsettings.h"
#include "gnc-prefs.h"
#include "gnc-ui.h"

static constexpr const char *remembered_response_key = "inv-entry-dup";

enum class QueryFamily { ok_cancel, verify, action };
enum class QueryAction { accept, cancel, window_close, parent_destroy,
                         dialog_destroy, invalid_response,
                         parent_destroy_during_completion };
struct QueryCase { QueryFamily family; QueryAction action; };
enum class NoticeFamily { error, warning, info };
enum class NoticeAction { response, window_close, parent_destroy, dialog_destroy };
struct NoticeCase { NoticeFamily family; NoticeAction action; };
enum class ErrorListCase { empty, one_owned, two_owned, parent_destroy, two_borrowed };
enum class Info2Action { accept, parent_destroy, dialog_destroy };
enum class InfoResponseAction { close, parent_destroy };
enum class DialogRunCase { permanent_accept, temporary_reject, permanent_cancel,
                           remembered_reject, notification_destroys_parent,
                           no_remembered_response, modal_notify_destroys_dialog };
enum class InputQueryCase { entry_accept, entry_cancel, entry_parent_destroy,
                            text_accept, text_cancel, text_parent_destroy };
enum class RadioQueryCase { accept_first, cancel, parent_destroy };

struct Completion
{
    std::uint32_t count = 0;
    std::int32_t response = GTK_RESPONSE_NONE;
    GtkWindow *parent = nullptr;
};

struct Info2Completion
{
    std::uint32_t calls{0};
    GtkWindow *parent{nullptr};
    std::int32_t response{GTK_RESPONSE_NONE};
};

struct DialogRunResult
{
    std::uint32_t count = 0;
    std::int32_t response = GTK_RESPONSE_NONE;
    GtkWindow *parent = nullptr;
};

struct InputResult
{
    unsigned count = 0;
    gchar *text = nullptr;
};

template <typename Param>
class GuiQueryFixture : public ::testing::TestWithParam<Param>
{
protected:
    void SetUp () override
    {
        parent = GTK_WINDOW (gtk_window_new (GTK_WINDOW_TOPLEVEL));
        g_object_ref_sink (parent);
        completion = {};
        info2_completion = {};
        dialog_run_result = {};
        input_result = {};
        destroy_count = 0;
        retained_dialog = nullptr;
    }
    void TearDown () override
    {
        gtk_widget_destroy (GTK_WIDGET (parent));
        if (weak_pointer_registered && weak_dialog)
            g_object_remove_weak_pointer (G_OBJECT (weak_dialog),
                                          reinterpret_cast<gpointer *> (&weak_dialog));
        weak_pointer_registered = false;
        if (retained_dialog)
            g_object_unref (retained_dialog);
        g_object_unref (parent);
        g_free (input_result.text);
    }
    void retain_dialog (GtkWidget *dialog)
    {
        retained_dialog = dialog;
        g_object_ref (retained_dialog);
    }
    void release_dialog ()
    {
        if (retained_dialog)
        {
            g_object_unref (retained_dialog);
            retained_dialog = nullptr;
        }
    }
    GtkWindow *parent{};
    GtkWidget *retained_dialog{};
    Completion completion{};
    Info2Completion info2_completion{};
    DialogRunResult dialog_run_result{};
    InputResult input_result{};
    std::uint32_t destroy_count{};
    GtkWidget *weak_dialog{};
    bool weak_pointer_registered{};
};

static void
completed (GtkWindow *parent, gint response, gpointer user_data)
{
    auto result = static_cast<Completion *> (user_data);
    ++result->count;
    result->response = response;
    result->parent = parent;
}

static GtkWidget *
find_query (GtkWindow *parent)
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *dialog = nullptr;
    for (auto node = windows; node; node = node->next)
        if (GTK_IS_MESSAGE_DIALOG (node->data) &&
            gtk_window_get_transient_for (GTK_WINDOW (node->data)) == parent)
        {
            EXPECT_EQ (dialog, nullptr);
            dialog = GTK_WIDGET (node->data);
        }
    g_list_free (windows);
    return dialog;
}

static void
destroy_parent_on_query_destroy (GtkWidget *, gpointer parent)
{
    gtk_widget_destroy (GTK_WIDGET (parent));
}

class QueryResponseTest : public GuiQueryFixture<QueryCase> {};

TEST_P (QueryResponseTest, CompletesOnceForEveryResponsePath)
{
    const auto test_case = GetParam ();
    const auto family = test_case.family;
    const auto action = test_case.action;
    auto parent = this->parent;
    auto &result = completion;
    std::int32_t accept = GTK_RESPONSE_OK;
    std::int32_t cancel = GTK_RESPONSE_CANCEL;
    if (family == QueryFamily::ok_cancel)
        gnc_ok_cancel_dialog_async (parent, GTK_RESPONSE_CANCEL, completed,
                                    &result, "query %d", 1);
    else if (family == QueryFamily::verify)
    {
        accept = GTK_RESPONSE_YES;
        cancel = GTK_RESPONSE_NO;
        gnc_verify_dialog_async (parent, FALSE, completed, &result,
                                 "query %d", 2);
    }
    else
    {
        accept = GTK_RESPONSE_ACCEPT;
        gnc_action_dialog_async (parent, "Action", FALSE, completed,
                                 &result, "query %d", 3);
    }
    EXPECT_EQ (result.count, 0u);
    auto dialog = find_query (parent);
    ASSERT_NE (dialog, nullptr);
    EXPECT_TRUE (gtk_window_get_modal (GTK_WINDOW (dialog)));
    EXPECT_TRUE (gtk_window_get_destroy_with_parent (GTK_WINDOW (dialog)));

    /* Retain only the object, not its callback state: a late response after
     * completion must not call the consumer or touch the freed request. */
    retain_dialog (dialog);
    switch (action)
    {
    case QueryAction::accept: gtk_dialog_response (GTK_DIALOG (dialog), accept); break;
    case QueryAction::cancel: gtk_dialog_response (GTK_DIALOG (dialog), cancel); break;
    case QueryAction::window_close: gtk_window_close (GTK_WINDOW (dialog)); break;
    case QueryAction::parent_destroy: gtk_widget_destroy (GTK_WIDGET (parent)); break;
    case QueryAction::dialog_destroy: gtk_widget_destroy (dialog); break;
    case QueryAction::invalid_response: gtk_dialog_response (GTK_DIALOG (dialog), 12345); break;
    case QueryAction::parent_destroy_during_completion:
        g_object_ref (parent);
        g_signal_connect (dialog, "destroy",
                          G_CALLBACK (destroy_parent_on_query_destroy), parent);
        gtk_dialog_response (GTK_DIALOG (dialog), accept);
        break;
    }
    for (std::uint32_t attempt = 0; result.count == 0u && attempt < 1000; ++attempt)
    {
        while (g_main_context_iteration (nullptr, FALSE))
            ;
        if (result.count == 0u)
            g_usleep (1000);
    }
    EXPECT_EQ (result.count, 1u);
    EXPECT_EQ (result.response, action == QueryAction::accept ? accept : cancel);
    if (action == QueryAction::parent_destroy ||
        action == QueryAction::parent_destroy_during_completion)
        EXPECT_EQ (result.parent, nullptr);
    else
        EXPECT_TRUE (result.parent == parent);
    gtk_dialog_response (GTK_DIALOG (dialog), accept);
    EXPECT_EQ (result.count, 1u);
    if (action == QueryAction::parent_destroy_during_completion)
        g_object_unref (parent);
    else if (action != QueryAction::parent_destroy)
        gtk_widget_destroy (GTK_WIDGET (parent));
}

static std::string
query_case_name (const ::testing::TestParamInfo<QueryCase> &info)
{
        const char *families[] = {"OkCancel", "Verify", "Action"};
        const char *actions[] = {"Accept", "Cancel", "WindowClose", "ParentDestroy",
                                 "DialogDestroy", "InvalidResponse",
                                 "ParentDestroyDuringCompletion"};
        return std::string (families[static_cast<int> (info.param.family)]) +
               actions[static_cast<int> (info.param.action)];

}

static std::string
notice_case_name (const ::testing::TestParamInfo<NoticeCase> &info)
{
        const char *families[] = {"Error", "Warning", "Info"};
        const char *actions[] = {"Response", "WindowClose", "ParentDestroy", "DialogDestroy"};
        return std::string (families[static_cast<int> (info.param.family)]) +
               actions[static_cast<int> (info.param.action)];

}

static std::string
error_list_case_name (const ::testing::TestParamInfo<ErrorListCase> &info)
{
        const char *names[] = {"Empty", "OneOwned", "TwoOwned",
                               "ParentDestroy", "TwoBorrowed"};
        return names[static_cast<int> (info.param)];

}

static std::string
info2_case_name (const ::testing::TestParamInfo<Info2Action> &info)
{
        const char *names[] = {"Accept", "ParentDestroy", "DialogDestroy"};
        return names[static_cast<int> (info.param)];

}

static std::string
info_summary_case_name (const ::testing::TestParamInfo<InfoResponseAction> &info)
{
        const char *names[] = {"Close", "ParentDestroy"};
        return names[static_cast<int> (info.param)];

}

static std::string
remembered_dialog_case_name (const ::testing::TestParamInfo<DialogRunCase> &info)
{
        const char *names[] = {"PermanentAccept", "TemporaryReject",
                               "PermanentCancel", "RememberedReject",
                               "NotificationDestroysParent", "NoRememberedResponse",
                               "ModalNotifyDestroysDialog"};
        return names[static_cast<int> (info.param)];

}

static std::string
input_query_case_name (const ::testing::TestParamInfo<InputQueryCase> &info)
{
        const char *names[] = {"EntryAccept", "EntryCancel", "EntryParentDestroy",
                               "TextAccept", "TextCancel", "TextParentDestroy"};
        return names[static_cast<int> (info.param)];

}

static std::string
radio_query_case_name (const ::testing::TestParamInfo<RadioQueryCase> &info)
{
        const char *names[] = {"AcceptFirst", "Cancel", "ParentDestroy"};
        return names[static_cast<int> (info.param)];

}

INSTANTIATE_TEST_SUITE_P (
    QueryFamilies, QueryResponseTest,
    ::testing::Values (
        QueryCase {QueryFamily::ok_cancel, QueryAction::accept},
        QueryCase {QueryFamily::ok_cancel, QueryAction::cancel},
        QueryCase {QueryFamily::ok_cancel, QueryAction::window_close},
        QueryCase {QueryFamily::ok_cancel, QueryAction::parent_destroy},
        QueryCase {QueryFamily::ok_cancel, QueryAction::dialog_destroy},
        QueryCase {QueryFamily::ok_cancel, QueryAction::invalid_response},
        QueryCase {QueryFamily::ok_cancel, QueryAction::parent_destroy_during_completion},
        QueryCase {QueryFamily::verify, QueryAction::accept},
        QueryCase {QueryFamily::verify, QueryAction::cancel},
        QueryCase {QueryFamily::verify, QueryAction::window_close},
        QueryCase {QueryFamily::verify, QueryAction::parent_destroy},
        QueryCase {QueryFamily::verify, QueryAction::dialog_destroy},
        QueryCase {QueryFamily::verify, QueryAction::invalid_response},
        QueryCase {QueryFamily::verify, QueryAction::parent_destroy_during_completion},
        QueryCase {QueryFamily::action, QueryAction::accept},
        QueryCase {QueryFamily::action, QueryAction::cancel},
        QueryCase {QueryFamily::action, QueryAction::window_close},
        QueryCase {QueryFamily::action, QueryAction::parent_destroy},
        QueryCase {QueryFamily::action, QueryAction::dialog_destroy},
        QueryCase {QueryFamily::action, QueryAction::invalid_response},
        QueryCase {QueryFamily::action, QueryAction::parent_destroy_during_completion}),
    query_case_name);

static void
notice_destroyed (GtkWidget *, gpointer user_data)
{
    ++*static_cast<std::uint32_t *> (user_data);
}

class NoticeResponseTest : public GuiQueryFixture<NoticeCase> {};

TEST_P (NoticeResponseTest, CopiesMessageAndClosesOnce)
{
    const auto test_case = GetParam ();
    const auto family = test_case.family;
    const auto action = test_case.action;
    auto parent = this->parent;
    auto detail = g_strdup ("borrowed detail");
    if (family == NoticeFamily::error)
        gnc_error_dialog_async (parent, "Notice %d: %s", 42, detail);
    else if (family == NoticeFamily::warning)
        gnc_warning_dialog_async (parent, "Notice %d: %s", 42, detail);
    else
        gnc_info_dialog_async (parent, "Notice %d: %s", 42, detail);
    g_free (detail);
    auto dialog = find_query (parent);
    ASSERT_NE (dialog, nullptr);
    EXPECT_TRUE (gtk_window_get_modal (GTK_WINDOW (dialog)));
    EXPECT_TRUE (gtk_window_get_destroy_with_parent (GTK_WINDOW (dialog)));
    gchar *message = nullptr;
    std::int32_t type = GTK_MESSAGE_OTHER;
    g_object_get (dialog, "text", &message, "message-type", &type, nullptr);
    const GtkMessageType types[] = {GTK_MESSAGE_ERROR, GTK_MESSAGE_WARNING,
                                    GTK_MESSAGE_INFO};
    EXPECT_STREQ (message, "Notice 42: borrowed detail");
    EXPECT_EQ (type, types[static_cast<int> (family)]);
    g_free (message);

    g_signal_connect (dialog, "destroy", G_CALLBACK (notice_destroyed), &destroy_count);
    retain_dialog (dialog);
    weak_dialog = dialog;
    g_object_add_weak_pointer (G_OBJECT (dialog),
                               reinterpret_cast<gpointer *> (&weak_dialog));
    weak_pointer_registered = true;
    switch (action)
    {
    case NoticeAction::response: gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_CLOSE); break;
    case NoticeAction::window_close: gtk_window_close (GTK_WINDOW (dialog)); break;
    case NoticeAction::parent_destroy: gtk_widget_destroy (GTK_WIDGET (parent)); break;
    case NoticeAction::dialog_destroy: gtk_widget_destroy (dialog); break;
    }
    for (std::uint32_t attempt = 0; destroy_count == 0u && attempt < 1000; ++attempt)
    {
        while (g_main_context_iteration (nullptr, FALSE))
            ;
        if (destroy_count == 0u)
            g_usleep (1000);
    }
    EXPECT_EQ (destroy_count, 1u);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_CLOSE);
    EXPECT_EQ (destroy_count, 1u);
    release_dialog ();
    EXPECT_EQ (weak_dialog, nullptr);
    if (action != NoticeAction::parent_destroy)
        gtk_widget_destroy (GTK_WIDGET (parent));
}

INSTANTIATE_TEST_SUITE_P (
    NoticeFamilies, NoticeResponseTest,
    ::testing::Values (
        NoticeCase {NoticeFamily::error, NoticeAction::response},
        NoticeCase {NoticeFamily::error, NoticeAction::window_close},
        NoticeCase {NoticeFamily::error, NoticeAction::parent_destroy},
        NoticeCase {NoticeFamily::error, NoticeAction::dialog_destroy},
        NoticeCase {NoticeFamily::warning, NoticeAction::response},
        NoticeCase {NoticeFamily::warning, NoticeAction::window_close},
        NoticeCase {NoticeFamily::warning, NoticeAction::parent_destroy},
        NoticeCase {NoticeFamily::warning, NoticeAction::dialog_destroy},
        NoticeCase {NoticeFamily::info, NoticeAction::response},
        NoticeCase {NoticeFamily::info, NoticeAction::window_close},
        NoticeCase {NoticeFamily::info, NoticeAction::parent_destroy},
        NoticeCase {NoticeFamily::info, NoticeAction::dialog_destroy}),
    notice_case_name);

class ErrorListResponseTest : public GuiQueryFixture<ErrorListCase> {};

TEST_P (ErrorListResponseTest, CopiesOwnedAndBorrowedMessages)
{
    const auto scenario = GetParam ();
    auto parent = this->parent;
    GList *errors = nullptr;
    if (scenario == ErrorListCase::two_borrowed)
    {
        errors = g_list_append (errors, const_cast<char *> ("First: 100%"));
        errors = g_list_append (errors, const_cast<char *> ("Second: äöü"));
    }
    else if (scenario != ErrorListCase::empty)
    {
        errors = g_list_append (errors, g_strdup ("First: 100%"));
        if (scenario != ErrorListCase::one_owned)
            errors = g_list_append (errors, g_strdup ("Second: äöü"));
    }
    gnc_error_dialog_async_list (parent, errors);
    if (scenario == ErrorListCase::two_borrowed)
        g_list_free (errors);
    else
        g_list_free_full (errors, g_free);
    auto dialog = find_query (parent);
    if (scenario == ErrorListCase::empty)
        EXPECT_EQ (dialog, nullptr);
    else
    {
        ASSERT_NE (dialog, nullptr);
        gchar *text = nullptr;
        g_object_get (dialog, "text", &text, nullptr);
        EXPECT_STREQ (text, scenario == ErrorListCase::one_owned ? "First: 100%" :
                         "First: 100%\n\nSecond: äöü");
        g_free (text);
        EXPECT_TRUE (gtk_window_get_modal (GTK_WINDOW (dialog)));
        destroy_count = 0;
        g_signal_connect (dialog, "destroy", G_CALLBACK (notice_destroyed),
                          &destroy_count);
        retain_dialog (dialog);
        if (scenario == ErrorListCase::parent_destroy)
            gtk_widget_destroy (GTK_WIDGET (parent));
        else
            gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_CLOSE);
        EXPECT_EQ (destroy_count, 1u);
        gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_CLOSE);
        EXPECT_EQ (destroy_count, 1u);
    }
    if (scenario != ErrorListCase::parent_destroy)
        gtk_widget_destroy (GTK_WIDGET (parent));
}

INSTANTIATE_TEST_SUITE_P (
    ErrorListInputs, ErrorListResponseTest,
    ::testing::Values (ErrorListCase::empty, ErrorListCase::one_owned,
                       ErrorListCase::two_owned, ErrorListCase::parent_destroy,
                       ErrorListCase::two_borrowed),
    error_list_case_name);

static void
info2_completed (GtkWindow *parent, gint response, gpointer user_data)
{
    auto result = static_cast<Info2Completion *> (user_data);
    ++result->calls;
    result->parent = parent;
    result->response = response;
}

static GtkWidget *
find_info2_dialog (GtkWindow *parent)
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *dialog = nullptr;
    for (auto node = windows; node; node = node->next)
    {
        auto widget = GTK_WIDGET (node->data);
        if (GTK_IS_DIALOG (widget) &&
            gtk_window_get_transient_for (GTK_WINDOW (widget)) == parent &&
            g_strcmp0 (gtk_window_get_title (GTK_WINDOW (widget)),
                       "Copied title") == 0)
        {
            EXPECT_EQ (dialog, nullptr);
            dialog = widget;
        }
    }
    g_list_free (windows);
    return dialog;
}

class Info2ResponseTest : public GuiQueryFixture<Info2Action> {};

TEST_P (Info2ResponseTest, CopiesStringsAndCompletesOnce)
{
    const auto scenario = GetParam ();
    auto parent = this->parent;
    auto title = g_strdup ("Copied title");
    auto text = g_strdup ("Copied message");
    auto &result = info2_completion;
    gnc_info2_dialog_async (GTK_WIDGET (parent), title, text,
                            info2_completed, &result);
    g_free (title);
    g_free (text);

    auto dialog = find_info2_dialog (parent);
    ASSERT_NE (dialog, nullptr);
    EXPECT_TRUE (gtk_window_get_modal (GTK_WINDOW (dialog)));
    EXPECT_TRUE (gtk_window_get_destroy_with_parent (GTK_WINDOW (dialog)));
    auto children = gtk_container_get_children (
        GTK_CONTAINER (gtk_dialog_get_content_area (GTK_DIALOG (dialog))));
    ASSERT_NE (children, nullptr);
    auto view = gtk_bin_get_child (GTK_BIN (children->data));
    g_list_free (children);
    EXPECT_TRUE (GTK_IS_TEXT_VIEW (view));
    GtkTextIter start, end;
    auto buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (view));
    gtk_text_buffer_get_bounds (buffer, &start, &end);
    auto copied_text = gtk_text_buffer_get_text (buffer, &start, &end, FALSE);
    EXPECT_STREQ (copied_text, "Copied message");
    g_free (copied_text);

    retain_dialog (dialog);
    if (scenario == Info2Action::accept)
        gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_ACCEPT);
    else if (scenario == Info2Action::parent_destroy)
        gtk_widget_destroy (GTK_WIDGET (parent));
    else
        gtk_widget_destroy (dialog);
    EXPECT_EQ (result.calls, 1u);
    EXPECT_TRUE (result.parent == (scenario == Info2Action::parent_destroy ? nullptr : parent));
    EXPECT_EQ (result.response, scenario == Info2Action::accept ? GTK_RESPONSE_ACCEPT : GTK_RESPONSE_CANCEL);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_ACCEPT);
    EXPECT_EQ (result.calls, 1u);
    if (scenario != Info2Action::parent_destroy)
        gtk_widget_destroy (GTK_WIDGET (parent));
}

INSTANTIATE_TEST_SUITE_P (
    Info2Completion, Info2ResponseTest,
    ::testing::Values (Info2Action::accept, Info2Action::parent_destroy,
                       Info2Action::dialog_destroy),
    info2_case_name);

class InfoDialogResponseTest : public GuiQueryFixture<InfoResponseAction> {};

TEST_P (InfoDialogResponseTest, CopiesSummaryAndCompletesOnce)
{
    const auto scenario = GetParam ();
    auto parent = this->parent;
    auto text = g_strdup ("Copied summary");
    auto &result = info2_completion;
    gnc_info_dialog_async_response (parent, info2_completed, &result,
                                    "%s", text);
    g_free (text);

    auto dialog = find_query (parent);
    ASSERT_NE (dialog, nullptr);
    EXPECT_TRUE (gtk_window_get_modal (GTK_WINDOW (dialog)));
    EXPECT_TRUE (gtk_window_get_destroy_with_parent (GTK_WINDOW (dialog)));
    gchar *message = nullptr;
    std::int32_t type = GTK_MESSAGE_OTHER;
    g_object_get (dialog, "text", &message, "message-type", &type, nullptr);
    EXPECT_STREQ (message, "Copied summary");
    EXPECT_EQ (type, GTK_MESSAGE_INFO);
    g_free (message);

    retain_dialog (dialog);
    if (scenario == InfoResponseAction::close)
        gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_CLOSE);
    else
        gtk_widget_destroy (GTK_WIDGET (parent));
    EXPECT_EQ (result.calls, 1u);
    EXPECT_TRUE (result.parent == (scenario == InfoResponseAction::close ? parent : nullptr));
    EXPECT_EQ (result.response, scenario == InfoResponseAction::close ? GTK_RESPONSE_CLOSE : GTK_RESPONSE_CANCEL);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_CLOSE);
    EXPECT_EQ (result.calls, 1u);
    if (scenario == InfoResponseAction::close)
        gtk_widget_destroy (GTK_WIDGET (parent));
}

INSTANTIATE_TEST_SUITE_P (
    InfoSummaryCompletion, InfoDialogResponseTest,
    ::testing::Values (InfoResponseAction::close,
                       InfoResponseAction::parent_destroy),
    info_summary_case_name);

static void
dialog_run_completed (GtkWindow *parent, gint response, gpointer user_data)
{
    auto result = static_cast<DialogRunResult *> (user_data);
    ++result->count;
    result->response = response;
    result->parent = parent;
}

static GtkWidget *
find_check_button (GtkWidget *widget, const gchar *label)
{
    if (GTK_IS_CHECK_BUTTON (widget) &&
        g_strcmp0 (gtk_button_get_label (GTK_BUTTON (widget)), label) == 0)
        return widget;
    if (!GTK_IS_CONTAINER (widget))
        return nullptr;
    auto children = gtk_container_get_children (GTK_CONTAINER (widget));
    GtkWidget *found = nullptr;
    for (auto node = children; node && !found; node = node->next)
        found = find_check_button (GTK_WIDGET (node->data), label);
    g_list_free (children);
    return found;
}

static void
destroy_parent_on_pref_change ([[maybe_unused]] gpointer prefs,
                               [[maybe_unused]] gchar *pref, gpointer user_data)
{
    gtk_widget_destroy (GTK_WIDGET (user_data));
}

static void
destroy_dialog_on_modal_notify (GtkWidget *dialog,
                                [[maybe_unused]] GParamSpec *pspec,
                                [[maybe_unused]] gpointer user_data)
{
    EXPECT_TRUE (gtk_window_get_modal (GTK_WINDOW (dialog)));
    gtk_widget_destroy (dialog);
}

class RememberedDialogResponseTest : public GuiQueryFixture<DialogRunCase> {};

TEST_P (RememberedDialogResponseTest, AppliesRememberedResponseSafely)
{
    const auto scenario = GetParam ();
    gnc_prefs_set_int (GNC_PREFS_GROUP_WARNINGS_PERM,
                       remembered_response_key, 0);
    gnc_prefs_set_int (GNC_PREFS_GROUP_WARNINGS_TEMP,
                       remembered_response_key, 0);
    if (scenario == DialogRunCase::remembered_reject)
        gnc_prefs_set_int (GNC_PREFS_GROUP_WARNINGS_TEMP,
                           remembered_response_key, GTK_RESPONSE_REJECT);

    auto parent = this->parent;
    auto dialog = GTK_DIALOG (gtk_message_dialog_new (
        parent, static_cast<GtkDialogFlags> (
            GTK_DIALOG_DESTROY_WITH_PARENT |
            ((scenario == DialogRunCase::no_remembered_response ||
              scenario == DialogRunCase::modal_notify_destroys_dialog) ? 0 : GTK_DIALOG_MODAL)),
        GTK_MESSAGE_QUESTION, GTK_BUTTONS_NONE, "%s", "Record this entry?"));
    gtk_dialog_add_button (dialog, "Cancel", GTK_RESPONSE_CANCEL);
    gtk_dialog_add_button (dialog, "Don't Record", GTK_RESPONSE_REJECT);
    gtk_dialog_add_button (dialog, "Record", GTK_RESPONSE_ACCEPT);
    auto &result = dialog_run_result;
    if (scenario == DialogRunCase::remembered_reject)
        retain_dialog (GTK_WIDGET (dialog));
    if (scenario == DialogRunCase::notification_destroys_parent)
        gnc_prefs_register_cb (GNC_PREFS_GROUP_WARNINGS_PERM,
                               remembered_response_key,
                               (gpointer)destroy_parent_on_pref_change, parent);
    if (scenario == DialogRunCase::modal_notify_destroys_dialog)
        g_signal_connect (dialog, "notify::modal",
                          G_CALLBACK (destroy_dialog_on_modal_notify), nullptr);
    if (scenario == DialogRunCase::modal_notify_destroys_dialog)
        retain_dialog (GTK_WIDGET (dialog));

    gnc_dialog_run_async (dialog,
                          scenario == DialogRunCase::no_remembered_response ? nullptr : remembered_response_key,
                          dialog_run_completed, &result);
    if (scenario == DialogRunCase::remembered_reject)
    {
        EXPECT_EQ (result.count, 1u);
        EXPECT_TRUE (result.parent == parent);
        EXPECT_EQ (result.response, GTK_RESPONSE_REJECT);
        gtk_dialog_response (dialog, GTK_RESPONSE_ACCEPT);
        EXPECT_EQ (result.count, 1u);
        gnc_prefs_set_int (GNC_PREFS_GROUP_WARNINGS_TEMP,
                           remembered_response_key, 0);
        return;
    }
    if (scenario == DialogRunCase::modal_notify_destroys_dialog)
    {
        EXPECT_EQ (result.count, 1u);
        EXPECT_TRUE (result.parent == parent);
        EXPECT_EQ (result.response, GTK_RESPONSE_CANCEL);
        gtk_dialog_response (dialog, GTK_RESPONSE_ACCEPT);
        EXPECT_EQ (result.count, 1u);
        return;
    }

    EXPECT_EQ (result.count, 0u);
    EXPECT_TRUE (gtk_window_get_modal (GTK_WINDOW (dialog)));
    if (!retained_dialog)
        retain_dialog (GTK_WIDGET (dialog));
    if (scenario == DialogRunCase::no_remembered_response)
    {
        gtk_dialog_response (dialog, GTK_RESPONSE_CLOSE);
        EXPECT_EQ (result.count, 1u);
        EXPECT_TRUE (result.parent == parent);
        EXPECT_EQ (result.response, GTK_RESPONSE_CLOSE);
        gtk_dialog_response (dialog, GTK_RESPONSE_ACCEPT);
        EXPECT_EQ (result.count, 1u);
        return;
    }
    auto permanent = find_check_button (GTK_WIDGET (dialog),
                                        "Remember and don't _ask me again.");
    auto temporary = find_check_button (GTK_WIDGET (dialog),
                                        "Remember and don't ask me again this _session.");
    ASSERT_NE (permanent, nullptr);
    ASSERT_NE (temporary, nullptr);
    if (scenario == DialogRunCase::permanent_accept ||
        scenario == DialogRunCase::permanent_cancel ||
        scenario == DialogRunCase::notification_destroys_parent)
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (permanent), TRUE);
    else if (scenario == DialogRunCase::temporary_reject)
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (temporary), TRUE);
    std::int32_t selected_response = scenario == DialogRunCase::temporary_reject ? GTK_RESPONSE_REJECT :
                             scenario == DialogRunCase::permanent_cancel ? GTK_RESPONSE_CANCEL :
                             GTK_RESPONSE_ACCEPT;
    gtk_dialog_response (dialog, selected_response);
    EXPECT_EQ (result.count, 1u);
    if (scenario == DialogRunCase::notification_destroys_parent)
    {
        EXPECT_EQ (result.parent, nullptr);
        EXPECT_EQ (result.response, GTK_RESPONSE_CANCEL);
        gnc_prefs_remove_cb_by_func (GNC_PREFS_GROUP_WARNINGS_PERM,
                                     remembered_response_key,
                                     (gpointer)destroy_parent_on_pref_change,
                                     parent);
    }
    else
    {
        EXPECT_TRUE (result.parent == parent);
        EXPECT_EQ (result.response, selected_response);
    }
    gtk_dialog_response (dialog, GTK_RESPONSE_ACCEPT);
    EXPECT_EQ (result.count, 1u);

    std::int32_t permanent_value = gnc_prefs_get_int (GNC_PREFS_GROUP_WARNINGS_PERM,
                                               remembered_response_key);
    std::int32_t temporary_value = gnc_prefs_get_int (GNC_PREFS_GROUP_WARNINGS_TEMP,
                                               remembered_response_key);
    /* The preference is committed before its notification destroys the parent.
     * Only the caller's action is cancelled; do not roll back a notified value. */
    if (scenario == DialogRunCase::permanent_accept ||
        scenario == DialogRunCase::notification_destroys_parent)
        EXPECT_EQ (permanent_value, GTK_RESPONSE_ACCEPT);
    else if (scenario == DialogRunCase::temporary_reject)
        EXPECT_EQ (temporary_value, GTK_RESPONSE_REJECT);
    else
    {
        EXPECT_EQ (permanent_value, 0);
        EXPECT_EQ (temporary_value, 0);
    }
    gnc_prefs_set_int (GNC_PREFS_GROUP_WARNINGS_PERM,
                       remembered_response_key, 0);
    gnc_prefs_set_int (GNC_PREFS_GROUP_WARNINGS_TEMP,
                       remembered_response_key, 0);
    if (scenario != DialogRunCase::notification_destroys_parent)
        gtk_widget_destroy (GTK_WIDGET (parent));
}

INSTANTIATE_TEST_SUITE_P (
    DialogRunModes, RememberedDialogResponseTest,
    ::testing::Values (DialogRunCase::permanent_accept,
                       DialogRunCase::temporary_reject,
                       DialogRunCase::permanent_cancel,
                       DialogRunCase::remembered_reject,
                       DialogRunCase::notification_destroys_parent,
                       DialogRunCase::no_remembered_response,
                       DialogRunCase::modal_notify_destroys_dialog),
    remembered_dialog_case_name);

static GtkWidget *
find_child_type (GtkWidget *widget, GType type)
{
    if (g_type_is_a (G_OBJECT_TYPE (widget), type))
        return widget;
    if (!GTK_IS_CONTAINER (widget))
        return nullptr;
    auto children = gtk_container_get_children (GTK_CONTAINER (widget));
    GtkWidget *found = nullptr;
    for (auto node = children; node && !found; node = node->next)
        found = find_child_type (GTK_WIDGET (node->data), type);
    g_list_free (children);
    return found;
}

static GtkDialog *
find_plain_dialog (GtkWindow *parent)
{
    auto windows = gtk_window_list_toplevels ();
    GtkDialog *found = nullptr;
    for (auto node = windows; node; node = node->next)
        if (GTK_IS_DIALOG (node->data) &&
            gtk_window_get_transient_for (GTK_WINDOW (node->data)) == parent)
        {
            EXPECT_EQ (found, nullptr);
            found = GTK_DIALOG (node->data);
        }
    g_list_free (windows);
    return found;
}

class InputQueryResponseTest : public GuiQueryFixture<InputQueryCase> {};

TEST_P (InputQueryResponseTest, EntryAndTextResponsesCompleteOnce)
{
    const auto scenario = GetParam ();
    auto parent = this->parent;
    auto &result = input_result;
    auto callback = +[](GtkWindow *, gchar *text, gpointer data) {
        auto result = static_cast<InputResult *>(data);
        ++result->count;
        result->text = text;
    };
    const bool is_entry = scenario == InputQueryCase::entry_accept ||
                          scenario == InputQueryCase::entry_cancel ||
                          scenario == InputQueryCase::entry_parent_destroy;
    const bool accepts = scenario == InputQueryCase::entry_accept ||
                         scenario == InputQueryCase::text_accept;
    const bool destroys_parent = scenario == InputQueryCase::entry_parent_destroy ||
                                 scenario == InputQueryCase::text_parent_destroy;
    if (is_entry)
        gnc_input_dialog_with_entry_async (GTK_WIDGET (parent), "Input", "Value", "initial", callback, &result);
    else
        gnc_input_dialog_async (GTK_WIDGET (parent), "Input", "Value", "initial", callback, &result);
    EXPECT_EQ (result.count, 0u);
    auto dialog = find_plain_dialog (parent);
    ASSERT_NE (dialog, nullptr);
    retain_dialog (GTK_WIDGET (dialog));
    auto view = find_child_type (GTK_WIDGET (dialog), is_entry ? GTK_TYPE_ENTRY : GTK_TYPE_TEXT_VIEW);
    ASSERT_NE (view, nullptr);
    if (is_entry)
        gtk_entry_set_text (GTK_ENTRY (view), "response value");
    else
        gtk_text_buffer_set_text (gtk_text_view_get_buffer (GTK_TEXT_VIEW (view)), "response value", -1);
    if (destroys_parent)
        gtk_widget_destroy (GTK_WIDGET (parent));
    else
        gtk_dialog_response (dialog, accepts ? GTK_RESPONSE_ACCEPT : GTK_RESPONSE_REJECT);
    EXPECT_EQ (result.count, 1u);
    if (accepts)
        EXPECT_STREQ (result.text, "response value");
    else
        EXPECT_EQ (result.text, nullptr);
    gtk_dialog_response (dialog, GTK_RESPONSE_ACCEPT);
    EXPECT_EQ (result.count, 1u);
}

INSTANTIATE_TEST_SUITE_P (
    EntryAndText, InputQueryResponseTest,
    ::testing::Values (InputQueryCase::entry_accept, InputQueryCase::entry_cancel,
                       InputQueryCase::entry_parent_destroy, InputQueryCase::text_accept,
                       InputQueryCase::text_cancel, InputQueryCase::text_parent_destroy),
    input_query_case_name);

class RadioQueryResponseTest : public GuiQueryFixture<RadioQueryCase> {};

TEST_P (RadioQueryResponseTest, RetainedRadioIsDisconnectedAfterCompletion)
{
    const auto scenario = GetParam ();
    auto parent = this->parent;
    auto labels = g_list_append (nullptr, const_cast<char *> ("First"));
    labels = g_list_append (labels, const_cast<char *> ("Second"));
    auto &result = completion;
    gnc_choose_radio_option_dialog_async (GTK_WIDGET (parent), "Choice", "Choose", nullptr,
                                          1, labels, completed, &result);
    g_list_free (labels);
    EXPECT_EQ (result.count, 0u);
    auto dialog = find_plain_dialog (parent);
    ASSERT_NE (dialog, nullptr);
    retain_dialog (GTK_WIDGET (dialog));
    auto radio = find_child_type (GTK_WIDGET (dialog), GTK_TYPE_RADIO_BUTTON);
    ASSERT_NE (radio, nullptr);
    g_object_ref (radio);
    if (scenario == RadioQueryCase::parent_destroy)
        gtk_widget_destroy (GTK_WIDGET (parent));
    else
        gtk_dialog_response (dialog, scenario == RadioQueryCase::accept_first ? GTK_RESPONSE_OK : GTK_RESPONSE_CANCEL);
    EXPECT_EQ (result.count, 1u);
    EXPECT_EQ (result.response, scenario == RadioQueryCase::accept_first ? 1 : -1);
    /* Retained child objects must no longer point at the freed selection. */
    gtk_button_clicked (GTK_BUTTON (radio));
    gtk_dialog_response (dialog, GTK_RESPONSE_OK);
    EXPECT_EQ (result.count, 1u);
    g_object_unref (radio);
}

INSTANTIATE_TEST_SUITE_P (
    RadioResponses, RadioQueryResponseTest,
    ::testing::Values (RadioQueryCase::accept_first, RadioQueryCase::cancel,
                       RadioQueryCase::parent_destroy),
    radio_query_case_name);

int
main (int argc, char **argv)
{
    g_setenv ("GSETTINGS_BACKEND", "memory", TRUE);
    const gchar *builddir = g_getenv ("GNC_BUILDDIR");
    if (builddir)
    {
        auto schema_dir = g_build_filename (builddir, "share",
                                            "glib-2.0", "schemas", nullptr);
        g_setenv ("GSETTINGS_SCHEMA_DIR", schema_dir, TRUE);
        g_free (schema_dir);
    }
    ::testing::InitGoogleTest (&argc, argv);
    if (!gtk_init_check (&argc, &argv))
        g_error ("A graphical display is required for GUI query tests");
    gnc_gsettings_load_backend ();
    gnc::test::initialize_logging ();
    auto status = RUN_ALL_TESTS ();
    gnc_gsettings_shutdown ();
    return status;
}
