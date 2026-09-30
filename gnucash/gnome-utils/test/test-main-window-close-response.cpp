/* Copyright (C) 2026 GnuCash contributors
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#include <config.h>
#include <gtk/gtk.h>
#include <gtest/gtest.h>
#include <vector>
#include "Account.h"
#include "gnc-gsettings.h"
#include "gnc-file.h"
#include "gnc-main-window.h"
#include "gnc-prefs.h"
#include "gnc-warnings.h"

#include "cashobjects.h"
#include "gnc-autosave.h"
#include "gnc-component-manager.h"
#include "gnc-session.h"

namespace
{
GncMainWindow *suite_sentinel{};

void count_destroy (GtkWidget *, gpointer data)
{
    ++*static_cast<guint *>(data);
}

GtkDialog *find_question (GtkWindow *parent)
{
    GtkDialog *question = nullptr;
    auto windows = gtk_window_list_toplevels();
    for (auto node = windows; node; node = node->next)
        if (GTK_IS_MESSAGE_DIALOG(node->data) &&
            gtk_window_get_transient_for(GTK_WINDOW(node->data)) == parent)
        {
            EXPECT_EQ(question, nullptr);
            question = GTK_DIALOG(node->data);
        }
    g_list_free(windows);
    return question;
}

GtkDialog *find_save_chooser (GtkWindow *parent)
{
    GtkDialog *chooser = nullptr;
    auto windows = gtk_window_list_toplevels();
    for (auto node = windows; node; node = node->next)
        if (GTK_IS_FILE_CHOOSER_DIALOG(node->data) &&
            gtk_window_get_transient_for(GTK_WINDOW(node->data)) == parent)
        {
            EXPECT_EQ(chooser, nullptr);
            chooser = GTK_DIALOG(node->data);
        }
    g_list_free(windows);
    return chooser;
}

void request_close (GncMainWindow *window)
{
    gboolean handled = FALSE;
    g_signal_emit_by_name(window, "delete-event", nullptr, &handled);
    EXPECT_TRUE(handled);
}

class MainWindowCloseTest : public ::testing::Test
{
protected:
    void SetUp () override
    {
        m_session = qof_session_new (qof_book_new ());
        m_previous_session = gnc_exchange_current_session (m_session);
        m_book = qof_session_get_book (m_session);
        m_sentinel = suite_sentinel;
        m_window = gnc_main_window_new ();
        g_object_ref_sink (m_window);
        g_signal_connect (m_window, "destroy", G_CALLBACK (count_destroy),
                          &m_destroyed);
        gnc_prefs_set_int (GNC_PREFS_GROUP_WARNINGS_PERM,
                           GNC_PREF_WARN_CLOSING_WINDOW_QUESTION, 0);
        gnc_prefs_set_int (GNC_PREFS_GROUP_WARNINGS_TEMP,
                           GNC_PREF_WARN_CLOSING_WINDOW_QUESTION, 0);
    }
    void TearDown () override
    {
        /* Recreate the suite baseline immediately after the last-window case,
         * before destroying this case's remaining main window. */
        if (!suite_sentinel)
        {
            suite_sentinel = gnc_main_window_new ();
            g_object_ref_sink (suite_sentinel);
        }
        m_sentinel = suite_sentinel;
        if (m_window)
        {
            gtk_widget_destroy (GTK_WIDGET (m_window));
            g_signal_handlers_disconnect_by_data (m_window, &m_destroyed);
            g_object_unref (m_window);
        }
        for (auto widget : m_retained)
            g_object_unref (widget);
        m_retained.clear ();
        auto current = gnc_exchange_current_session (m_previous_session);
        if (current && current != m_previous_session)
            qof_session_destroy (current);
        if (m_session && m_session != current && m_session != m_previous_session)
            qof_session_destroy (m_session);
    }
    void destroy_sentinel ()
    {
        gtk_widget_destroy (GTK_WIDGET (m_sentinel));
        g_object_unref (m_sentinel);
        m_sentinel = nullptr;
        suite_sentinel = nullptr;
    }
    void retain (GtkWidget *widget)
    {
        m_retained.push_back (GTK_WIDGET (g_object_ref (widget)));
    }
    QofSession *m_session{};
    QofSession *m_previous_session{};
    QofBook *m_book{};
    GncMainWindow *m_sentinel{};
    GncMainWindow *m_window{};
    guint m_destroyed{};
    std::vector<GtkWidget *> m_retained;
};

TEST_F (MainWindowCloseTest, AcceptClosesAdditionalWindow)
{
    request_close (m_window);
    auto question = find_question (GTK_WINDOW (m_window));
    ASSERT_NE (question, nullptr);
    retain (GTK_WIDGET (question));
    EXPECT_TRUE (gtk_window_get_modal (GTK_WINDOW (question)));
    request_close (m_window);
    EXPECT_EQ (find_question (GTK_WINDOW (m_window)), question);
    gtk_dialog_response (question, GTK_RESPONSE_YES);
    EXPECT_EQ (m_destroyed, 1u);
    gtk_dialog_response (question, GTK_RESPONSE_CANCEL);
    EXPECT_EQ (m_destroyed, 1u);
}

TEST_F (MainWindowCloseTest, CancelReleasesGuardForRetry)
{
    request_close (m_window);
    auto question = find_question (GTK_WINDOW (m_window));
    ASSERT_NE (question, nullptr);
    retain (GTK_WIDGET (question));
    request_close (m_window);
    EXPECT_EQ (find_question (GTK_WINDOW (m_window)), question);
    gtk_dialog_response (question, GTK_RESPONSE_CANCEL);
    EXPECT_EQ (m_destroyed, 0u);
    EXPECT_EQ (g_object_get_data (G_OBJECT (m_window), "gnc-window-close-pending"), nullptr);
    request_close (m_window);
    auto retry = find_question (GTK_WINDOW (m_window));
    ASSERT_NE (retry, nullptr);
    gtk_dialog_response (retry, GTK_RESPONSE_CANCEL);
    gtk_dialog_response (question, GTK_RESPONSE_YES);
    EXPECT_EQ (m_destroyed, 0u);
}

TEST_F (MainWindowCloseTest, DestroyingWindowCancelsQuestion)
{
    request_close (m_window);
    auto question = find_question (GTK_WINDOW (m_window));
    ASSERT_NE (question, nullptr);
    retain (GTK_WIDGET (question));
    gtk_widget_destroy (GTK_WIDGET (m_window));
    EXPECT_EQ (m_destroyed, 1u);
    gtk_dialog_response (question, GTK_RESPONSE_YES);
    EXPECT_EQ (m_destroyed, 1u);
}

TEST_F (MainWindowCloseTest, RememberedAnswerClosesWithoutQuestion)
{
    gnc_prefs_set_int (GNC_PREFS_GROUP_WARNINGS_PERM,
                       GNC_PREF_WARN_CLOSING_WINDOW_QUESTION, GTK_RESPONSE_YES);
    request_close (m_window);
    EXPECT_EQ (m_destroyed, 1u);
    EXPECT_EQ (find_question (GTK_WINDOW (m_window)), nullptr);
}

TEST_F (MainWindowCloseTest, LastWindowSaveCancelAndRetry)
{

    /* The last-window path must run without the helper window used by the
     * four additional-window cases above. */
    destroy_sentinel ();

    gnc_prefs_set_bool(GNC_PREFS_GROUP_GENERAL, "save-on-close-expires", FALSE);
    auto book = m_book;
    auto root = gnc_account_create_root(book);
    auto account = xaccMallocAccount(book);
    xaccAccountBeginEdit(account);
    xaccAccountSetName(account, "Unsaved close test account");
    gnc_account_append_child(root, account);
    xaccAccountCommitEdit(account);
    EXPECT_TRUE (qof_book_session_not_saved(book));

    auto window = m_window;
    gtk_widget_show(GTK_WIDGET(window));

    request_close(window);
    auto question = find_question(GTK_WINDOW(window));
    ASSERT_NE (question, nullptr);
    retain(GTK_WIDGET(question));
    EXPECT_FALSE (gnc_main_window_is_quitting(window));
    EXPECT_TRUE (gtk_widget_get_visible(GTK_WIDGET(window)));
    EXPECT_FALSE (gtk_widget_in_destruction(GTK_WIDGET(window)));

    gtk_dialog_response(question, GTK_RESPONSE_APPLY);
    auto chooser = find_save_chooser(GTK_WINDOW(window));
    ASSERT_NE (chooser, nullptr);
    retain(GTK_WIDGET(chooser));
    EXPECT_TRUE (gnc_file_save_in_progress());
    EXPECT_TRUE (qof_book_session_not_saved(book));
    EXPECT_FALSE (gnc_main_window_is_quitting(window));
    EXPECT_TRUE (gtk_widget_get_visible(GTK_WIDGET(window)));
    EXPECT_FALSE (gtk_widget_in_destruction(GTK_WIDGET(window)));

    gtk_dialog_response(chooser, GTK_RESPONSE_CANCEL);
    EXPECT_FALSE (gnc_file_save_in_progress());
    EXPECT_TRUE (qof_book_session_not_saved(book));
    EXPECT_FALSE (gnc_main_window_is_quitting(window));
    EXPECT_TRUE (gtk_widget_get_visible(GTK_WIDGET(window)));
    EXPECT_FALSE (gtk_widget_in_destruction(GTK_WIDGET(window)));
    EXPECT_EQ (g_object_get_data(G_OBJECT(window), "gnc-save-close-pending"), nullptr);

    /* Cancellation must release the pending-close guard so the user can
     * close again and cancel the new save question explicitly. */
    request_close(window);
    auto retry = find_question(GTK_WINDOW(window));
    ASSERT_NE (retry, nullptr);
    retain(GTK_WIDGET(retry));
    EXPECT_FALSE (gnc_main_window_is_quitting(window));
    gtk_dialog_response(retry, GTK_RESPONSE_CANCEL);
    EXPECT_TRUE (qof_book_session_not_saved(book));
    EXPECT_FALSE (gnc_main_window_is_quitting(window));
    EXPECT_TRUE (gtk_widget_get_visible(GTK_WIDGET(window)));
    EXPECT_FALSE (gtk_widget_in_destruction(GTK_WIDGET(window)));

    gnc_autosave_remove_timer(book);
}
}

int main (int argc, char **argv)
{
    g_setenv("GNC_UNINSTALLED", "YES", TRUE);
    g_setenv("GSETTINGS_BACKEND", "memory", TRUE);
    ::testing::InitGoogleTest(&argc, argv);
    if (!gtk_init_check(&argc, &argv))
        g_error("A graphical display is required for main-window close tests");
    qof_init();
    if (!cashobjects_register())
        g_error("Failed to register cash objects");
    gnc_component_manager_init();
    gnc_gsettings_load_backend();
    gnc_get_current_session();
    suite_sentinel = gnc_main_window_new ();
    g_object_ref_sink (suite_sentinel);
    g_log_set_always_fatal (static_cast<GLogLevelFlags> (
        G_LOG_FATAL_MASK | G_LOG_LEVEL_WARNING | G_LOG_LEVEL_CRITICAL));
    auto result = RUN_ALL_TESTS();
    gtk_widget_destroy (GTK_WIDGET (suite_sentinel));
    g_object_unref (suite_sentinel);
    suite_sentinel = nullptr;
    gnc_gsettings_shutdown();
    gnc_component_manager_shutdown();
    gnc_clear_current_session();
    qof_close();
    return result;
}
