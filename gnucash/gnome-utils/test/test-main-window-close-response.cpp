/* Copyright (C) 2026 GnuCash contributors
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#include <config.h>
#include <gtk/gtk.h>
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
gboolean display_available;
GncMainWindow *sentinel;

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
            g_assert_null(question);
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
            g_assert_null(chooser);
            chooser = GTK_DIALOG(node->data);
        }
    g_list_free(windows);
    return chooser;
}

void request_close (GncMainWindow *window)
{
    gboolean handled = FALSE;
    g_signal_emit_by_name(window, "delete-event", nullptr, &handled);
    g_assert_true(handled);
}

void test_close_response (gconstpointer data)
{
    if (!display_available)
    {
        g_test_skip("No graphical display is available");
        return;
    }
    /* Keep another real main window alive: these cases close an additional
     * window, not the application's last-window/shutdown path. */
    if (!sentinel)
    {
        sentinel = gnc_main_window_new();
        g_object_ref_sink(sentinel);
    }
    auto window = gnc_main_window_new();
    g_object_ref_sink(window);
    guint destroyed = 0;
    g_signal_connect(window, "destroy", G_CALLBACK(count_destroy), &destroyed);
    auto scenario = GPOINTER_TO_INT(data);
    gnc_prefs_set_int(GNC_PREFS_GROUP_WARNINGS_PERM,
                      GNC_PREF_WARN_CLOSING_WINDOW_QUESTION,
                      scenario == 3 ? GTK_RESPONSE_YES : 0);
    gnc_prefs_set_int(GNC_PREFS_GROUP_WARNINGS_TEMP,
                      GNC_PREF_WARN_CLOSING_WINDOW_QUESTION, 0);
    request_close(window);
    if (scenario == 3)
        g_assert_cmpuint(destroyed, ==, 1);
    else
    {
        auto question = find_question(GTK_WINDOW(window));
        g_assert_nonnull(question);
        g_object_ref(question);
        g_assert_true(gtk_window_get_modal(GTK_WINDOW(question)));
        request_close(window);
        g_assert_true(find_question(GTK_WINDOW(window)) == question);
        if (scenario == 2)
            gtk_widget_destroy(GTK_WIDGET(window));
        else
            gtk_dialog_response(question, scenario == 0 ?
                                 GTK_RESPONSE_YES : GTK_RESPONSE_CANCEL);
        g_assert_cmpuint(destroyed, ==, scenario == 1 ? 0 : 1);
        if (scenario == 1)
        {
            g_assert_null(g_object_get_data(G_OBJECT(window), "gnc-window-close-pending"));
            request_close(window);
            auto retry = find_question(GTK_WINDOW(window));
            g_assert_nonnull(retry);
            gtk_dialog_response(retry, GTK_RESPONSE_CANCEL);
        }
        gtk_dialog_response(question, GTK_RESPONSE_YES);
        g_assert_cmpuint(destroyed, ==, scenario == 1 ? 0 : 1);
        g_object_unref(question);
    }
    if (!destroyed)
        gtk_widget_destroy(GTK_WIDGET(window));
    g_signal_handlers_disconnect_by_data(window, &destroyed);
    g_object_unref(window);
    gnc_prefs_set_int(GNC_PREFS_GROUP_WARNINGS_PERM,
                      GNC_PREF_WARN_CLOSING_WINDOW_QUESTION, 0);
}

void test_last_window_save_cancel_and_retry ()
{
    if (!display_available)
    {
        g_test_skip("No graphical display is available");
        return;
    }

    /* The last-window path must run without the helper window used by the
     * four additional-window cases above. */
    if (sentinel)
    {
        gtk_widget_destroy(GTK_WIDGET(sentinel));
        g_object_unref(sentinel);
        sentinel = nullptr;
    }

    gnc_prefs_set_bool(GNC_PREFS_GROUP_GENERAL, "save-on-close-expires", FALSE);
    auto book = qof_book_new();
    auto root = gnc_account_create_root(book);
    auto account = xaccMallocAccount(book);
    xaccAccountBeginEdit(account);
    xaccAccountSetName(account, "Unsaved close test account");
    gnc_account_append_child(root, account);
    xaccAccountCommitEdit(account);
    qof_book_mark_session_dirty(book);
    g_assert_true(qof_book_session_not_saved(book));

    auto session = qof_session_new(book);
    auto previous_session = gnc_exchange_current_session(session);
    auto window = gnc_main_window_new();
    g_object_ref_sink(window);
    gtk_widget_show(GTK_WIDGET(window));

    request_close(window);
    auto question = find_question(GTK_WINDOW(window));
    g_assert_nonnull(question);
    g_object_ref(question);
    g_assert_false(gnc_main_window_is_quitting(window));
    g_assert_true(gtk_widget_get_visible(GTK_WIDGET(window)));
    g_assert_false(gtk_widget_in_destruction(GTK_WIDGET(window)));

    gtk_dialog_response(question, GTK_RESPONSE_APPLY);
    auto chooser = find_save_chooser(GTK_WINDOW(window));
    g_assert_nonnull(chooser);
    g_object_ref(chooser);
    g_assert_true(gnc_file_save_in_progress());
    g_assert_true(qof_book_session_not_saved(book));
    g_assert_false(gnc_main_window_is_quitting(window));
    g_assert_true(gtk_widget_get_visible(GTK_WIDGET(window)));
    g_assert_false(gtk_widget_in_destruction(GTK_WIDGET(window)));

    gtk_dialog_response(chooser, GTK_RESPONSE_CANCEL);
    g_assert_false(gnc_file_save_in_progress());
    g_assert_true(qof_book_session_not_saved(book));
    g_assert_false(gnc_main_window_is_quitting(window));
    g_assert_true(gtk_widget_get_visible(GTK_WIDGET(window)));
    g_assert_false(gtk_widget_in_destruction(GTK_WIDGET(window)));
    g_assert_null(g_object_get_data(G_OBJECT(window), "gnc-save-close-pending"));

    /* Cancellation must release the pending-close guard so the user can
     * close again and cancel the new save question explicitly. */
    request_close(window);
    auto retry = find_question(GTK_WINDOW(window));
    g_assert_nonnull(retry);
    g_object_ref(retry);
    g_assert_false(gnc_main_window_is_quitting(window));
    gtk_dialog_response(retry, GTK_RESPONSE_CANCEL);
    g_assert_true(qof_book_session_not_saved(book));
    g_assert_false(gnc_main_window_is_quitting(window));
    g_assert_true(gtk_widget_get_visible(GTK_WIDGET(window)));
    g_assert_false(gtk_widget_in_destruction(GTK_WIDGET(window)));

    g_object_unref(retry);
    g_object_unref(chooser);
    g_object_unref(question);
    gnc_autosave_remove_timer(book);
    gtk_widget_destroy(GTK_WIDGET(window));
    g_object_unref(window);
    auto replaced_session = gnc_exchange_current_session(previous_session);
    g_assert_true(replaced_session == session);
    qof_session_destroy(replaced_session);
}
}

int main (int argc, char **argv)
{
    g_setenv("GNC_UNINSTALLED", "YES", TRUE);
    g_setenv("GSETTINGS_BACKEND", "memory", TRUE);
    g_test_init(&argc, &argv, nullptr);
    display_available = gtk_init_check(&argc, &argv);
    if (g_getenv("GNC_REQUIRE_DISPLAY"))
        g_assert_true(display_available);
    qof_init();
    g_assert_true(cashobjects_register());
    gnc_component_manager_init();
    gnc_gsettings_load_backend();
    gnc_get_current_session();
    const char *cases[] = {"accept", "cancel-and-retry", "parent-destroy", "remembered-answer"};
    for (guint i = 0; i < G_N_ELEMENTS(cases); ++i)
    {
        auto path = g_strdup_printf("/gnome-utils/main-window/close/%s", cases[i]);
        g_test_add_data_func(path, GINT_TO_POINTER(i), test_close_response);
        g_free(path);
    }
    g_test_add_func("/gnome-utils/main-window/close/last-window-save-cancel-retry",
                    test_last_window_save_cancel_and_retry);
    auto result = g_test_run();
    if (sentinel)
    {
        gtk_widget_destroy(GTK_WIDGET(sentinel));
        g_object_unref(sentinel);
    }
    gnc_gsettings_shutdown();
    gnc_component_manager_shutdown();
    gnc_clear_current_session();
    qof_close();
    return result;
}
