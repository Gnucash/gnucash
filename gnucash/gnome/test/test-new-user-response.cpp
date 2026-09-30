/* Copyright (C) 2026 GnuCash contributors
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#include <config.h>
#include <gtk/gtk.h>
#include <gtest/gtest.h>
#include "test/gnome-response-test-fixture.h"

#include "dialog-new-user.h"
#include "gnc-prefs-p.h"

namespace
{
struct PreferenceState
{
    guint writes{};
    gboolean first_startup{TRUE};
    GtkWidget *destroy_on_write{};
};

PreferenceState *active_state{};

gboolean
set_bool (const gchar *group, const gchar *name, gboolean value)
{
    if (!active_state)
    {
        ADD_FAILURE () << "Preference callback ran without an active fixture";
        return FALSE;
    }
    EXPECT_STREQ (group, GNC_PREFS_GROUP_NEW_USER);
    EXPECT_STREQ (name, GNC_PREF_FIRST_STARTUP);
    ++active_state->writes;
    active_state->first_startup = value;
    if (active_state->destroy_on_write)
    {
        auto window = active_state->destroy_on_write;
        active_state->destroy_on_write = nullptr;
        gtk_widget_destroy (window);
    }
    return TRUE;
}

GtkWidget *
find_named (GtkWidget *widget, const char *name)
{
    if (GTK_IS_BUILDABLE (widget) &&
        g_strcmp0 (gtk_buildable_get_name (GTK_BUILDABLE (widget)), name) == 0)
        return widget;
    if (!GTK_IS_CONTAINER (widget))
        return nullptr;
    auto children = gtk_container_get_children (GTK_CONTAINER (widget));
    GtkWidget *found = nullptr;
    for (auto child = children; child && !found; child = child->next)
        found = find_named (GTK_WIDGET (child->data), name);
    g_list_free (children);
    return found;
}

GtkWidget *
find_window (const char *name)
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *found = nullptr;
    for (auto window = windows; window; window = window->next)
        if (g_strcmp0 (gtk_buildable_get_name (GTK_BUILDABLE (window->data)),
                       name) == 0)
        {
            EXPECT_EQ (found, nullptr);
            if (found)
            {
                g_list_free (windows);
                return nullptr;
            }
            found = GTK_WIDGET (window->data);
        }
    g_list_free (windows);
    return found;
}

class NewUserResponseTest : public GnomeResponseTest
{
protected:
    void SetUp () override
    {
        GnomeResponseTest::SetUp ();
        state = {};
        active_state = &state;
        PrefsBackend memory_backend{};
        memory_backend.set_bool = set_bool;
        saved_backend = prefsbackend;
        /* Keep the injected backend alive for the complete test case. */
        backend = memory_backend;
        prefsbackend = &backend;
        gnc_ui_new_user_dialog ();
        window = find_window ("new_user_window");
        ASSERT_NE (window, nullptr);
    }

    void TearDown () override
    {
        if (auto question = find_window ("new_user_cancel_dialog"))
            gtk_widget_destroy (question);
        if (window)
            gtk_widget_destroy (window);
        while (g_main_context_iteration (nullptr, FALSE))
            ;
        prefsbackend = saved_backend;
        GnomeResponseTest::TearDown ();
        EXPECT_EQ (find_window ("new_user_window"), nullptr);
        EXPECT_EQ (find_window ("new_user_cancel_dialog"), nullptr);
        for (auto widget : retained_widgets)
            g_object_unref (widget);
        active_state = nullptr;
    }

    GtkWidget *window{};
    PrefsBackend backend{};
    PrefsBackend *saved_backend{};
    PreferenceState state{};
    std::vector<GtkWidget *> retained_widgets;

    void retain_widget (GtkWidget *widget)
    {
        g_object_ref (widget);
        retained_widgets.push_back (widget);
    }

    GtkWidget *open_cancel_question ()
    {
        auto cancel = find_named (window, "cancel_but");
        EXPECT_NE (cancel, nullptr);
        if (!cancel)
            return nullptr;
        gtk_button_clicked (GTK_BUTTON (cancel));
        auto question = find_window ("new_user_cancel_dialog");
        EXPECT_NE (question, nullptr);
        if (!question)
            return nullptr;
        EXPECT_EQ (state.writes, 0u);
        EXPECT_TRUE (gtk_window_get_modal (GTK_WINDOW (question)));
        EXPECT_TRUE (gtk_window_get_destroy_with_parent (GTK_WINDOW (question)));
        gtk_button_clicked (GTK_BUTTON (cancel));
        EXPECT_EQ (find_window ("new_user_cancel_dialog"), question);
        return question;
    }

    void expect_finished (gboolean expected_first_startup)
    {
        EXPECT_EQ (state.writes, 1u);
        EXPECT_EQ (state.first_startup, expected_first_startup);
        EXPECT_EQ (find_window ("new_user_window"), nullptr);
        EXPECT_EQ (find_window ("new_user_cancel_dialog"), nullptr);
        window = nullptr;
    }
};

TEST_F (NewUserResponseTest, NoKeepsFirstStartupDisabled)
{
    auto question = open_cancel_question ();
    ASSERT_NE (question, nullptr);
    gtk_dialog_response (GTK_DIALOG (question), GTK_RESPONSE_NO);
    expect_finished (FALSE);
}

TEST_F (NewUserResponseTest, YesConfirmsFirstStartup)
{
    auto question = open_cancel_question ();
    ASSERT_NE (question, nullptr);
    gtk_dialog_response (GTK_DIALOG (question), GTK_RESPONSE_YES);
    expect_finished (TRUE);
}

TEST_F (NewUserResponseTest, ClosingMainWindowBeforeIdleDoesNotLeaveDialog)
{
    gtk_widget_destroy (window);
    window = nullptr;
    while (g_main_context_iteration (nullptr, FALSE))
        ;
    EXPECT_EQ (state.writes, 1u);
    EXPECT_FALSE (state.first_startup);
    EXPECT_EQ (find_window ("new_user_window"), nullptr);
    EXPECT_EQ (find_window ("new_user_cancel_dialog"), nullptr);
}

TEST_F (NewUserResponseTest, DestroyingParentClosesCancellationQuestion)
{
    auto question = open_cancel_question ();
    ASSERT_NE (question, nullptr);
    gtk_widget_destroy (window);
    window = nullptr;
    expect_finished (FALSE);
}

TEST_F (NewUserResponseTest, PreferenceCallbackMayDestroyMainWindow)
{
    auto question = open_cancel_question ();
    ASSERT_NE (question, nullptr);
    state.destroy_on_write = window;
    gtk_dialog_response (GTK_DIALOG (question), GTK_RESPONSE_YES);
    window = nullptr;
    expect_finished (TRUE);
}

TEST_F (NewUserResponseTest, DestroyedQuestionIgnoresLateResponse)
{
    auto question = open_cancel_question ();
    ASSERT_NE (question, nullptr);
    retain_widget (question);
    gtk_widget_destroy (question);
    gtk_dialog_response (GTK_DIALOG (question), GTK_RESPONSE_YES);
    expect_finished (FALSE);
}
}

int
main (int argc, char **argv)
{
    ::testing::InitGoogleTest (&argc, argv);
    if (!gtk_init_check (&argc, &argv))
    {
        g_printerr ("GTK display initialization failed for new-user response tests.\n");
        return 1;
    }
    g_log_set_always_fatal (static_cast<GLogLevelFlags> (
        G_LOG_FATAL_MASK | G_LOG_LEVEL_WARNING | G_LOG_LEVEL_CRITICAL));
    return RUN_ALL_TESTS ();
}
