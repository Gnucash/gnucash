/* Copyright (C) 2026 GnuCash contributors
 * SPDX-License-Identifier: GPL-2.0-or-later
 */
#include <config.h>
#include <gtk/gtk.h>
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

namespace
{
gboolean display_available;
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
        g_assert_true(gnc_get_current_session() != nullptr);
        g_assert_true(qof_session_get_book(gnc_get_current_session()) == book);
        g_assert_true(book == expected_book);
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
    g_assert_true(qof_book_session_not_saved(book));
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
            g_assert_null(found);
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

void test_save_as(gconstpointer data)
{
    if (!display_available)
    {
        g_test_skip("No graphical display is available");
        return;
    }
    auto scenario = GPOINTER_TO_INT(data);
    fail_write = scenario == 1;
    require_overwrite = scenario >= 2;
    writes = 0;
    auto book = dirty_book();
    expected_book = book;
    qof_book_mark_session_dirty(book);
    auto original = qof_session_new(book);
    g_assert_null(gnc_exchange_current_session(original));
    auto parent = GTK_WINDOW(gtk_window_new(GTK_WINDOW_TOPLEVEL));
    g_object_ref_sink(parent);
    Result result;
    auto filename = g_build_filename(g_get_tmp_dir(), "gnc-response-save-test.gnucash", nullptr);
    gnc_file_do_save_as_async(parent, filename, completed, &result);
    QofSession *other = nullptr;
    if (require_overwrite)
    {
        auto question = find_dialog(parent);
        g_assert_nonnull(question);
        g_object_ref(question);
        g_assert_cmpuint(result.calls, ==, 0);
        g_assert_true(gnc_file_save_in_progress());
        g_assert_true(gnc_get_current_session() == original);
        g_assert_true(qof_session_get_book(original) == book);
        Result duplicate;
        gnc_file_save_async(parent, completed, &duplicate);
        g_assert_cmpuint(duplicate.calls, ==, 1);
        g_assert_false(duplicate.saved);
        // A rejected concurrent command must not terminate the first operation.
        g_assert_true(gnc_file_save_in_progress());
        if (scenario == 4)
            gtk_widget_destroy(GTK_WIDGET(parent));
        else
        {
            if (scenario == 5)
            {
                other = qof_session_new(qof_book_new());
                g_assert_true(gnc_exchange_current_session(other) == original);
            }
            gtk_dialog_response(question, scenario == 3 ?
                                 GTK_RESPONSE_CANCEL : GTK_RESPONSE_YES);
        }
        g_assert_cmpuint(result.calls, ==, 1);
        gtk_dialog_response(question, GTK_RESPONSE_YES);
        g_assert_cmpuint(result.calls, ==, 1);
        g_object_unref(question);
    }
    g_assert_cmpuint(result.calls, ==, 1);
    g_assert_false(gnc_file_save_in_progress());
    auto success = scenario == 0 || scenario == 2;
    g_assert_cmpint(result.saved, ==, success);
    g_assert_cmpuint(writes, ==, scenario <= 2 ? 1 : 0);
    if (success)
    {
        g_assert_true(gnc_get_current_session() != original);
        g_assert_true(qof_session_get_book(gnc_get_current_session()) == book);
        g_assert_false(qof_book_session_not_saved(book));
    }
    else
    {
        g_assert_true(gnc_get_current_session() == (other ? other : original));
        g_assert_true(qof_session_get_book(original) == book);
        g_assert_true(qof_book_session_not_saved(book));
    }
    close_dialogs(parent);
    gtk_widget_destroy(GTK_WIDGET(parent));
    g_object_unref(parent);
    gnc_clear_current_session();
    if (other)
        qof_session_destroy(original);
    g_free(filename);
    expected_book = nullptr;
}

void test_choose_cancel()
{
    if (!display_available)
    {
        g_test_skip("No graphical display is available");
        return;
    }
    auto book = dirty_book();
    qof_book_mark_session_dirty(book);
    auto original = qof_session_new(book);
    g_assert_null(gnc_exchange_current_session(original));
    auto parent = GTK_WINDOW(gtk_window_new(GTK_WINDOW_TOPLEVEL));
    Result result;
    gnc_file_save_async(parent, completed, &result);
    auto chooser = find_dialog(parent, true);
    g_assert_nonnull(chooser);
    g_assert_cmpuint(result.calls, ==, 0);
    gtk_dialog_response(chooser, GTK_RESPONSE_CANCEL);
    g_assert_cmpuint(result.calls, ==, 1);
    g_assert_false(result.saved);
    g_assert_true(gnc_get_current_session() == original);
    g_assert_true(qof_book_session_not_saved(book));
    gtk_widget_destroy(GTK_WIDGET(parent));
    gnc_clear_current_session();
}

void test_query(gconstpointer data)
{
    if (!display_available)
    {
        g_test_skip("No graphical display is available");
        return;
    }
    auto scenario = GPOINTER_TO_INT(data);
    auto book = dirty_book();
    expected_book = book;
    auto session = qof_session_new(book);
    g_assert_null(gnc_exchange_current_session(session));
    qof_book_mark_session_dirty(book);
    auto parent = GTK_WINDOW(gtk_window_new(GTK_WINDOW_TOPLEVEL));
    g_object_ref_sink(parent);
    Result result;
    gnc_file_query_save_async(parent, TRUE, completed, &result);
    auto question = find_dialog(parent);
    g_assert_nonnull(question);
    g_object_ref(question);
    g_assert_cmpuint(result.calls, ==, 0);
    QofSession *other = nullptr;
    if (scenario == 2)
        gtk_widget_destroy(GTK_WIDGET(parent));
    else if (scenario == 3)
    {
        other = qof_session_new(qof_book_new());
        g_assert_true(gnc_exchange_current_session(other) == session);
        gtk_dialog_response(question, GTK_RESPONSE_OK);
    }
    else if (scenario == 4)
    {
        // Save has no destination: cancellation must return to the query,
        // rather than allow the destructive continuation to run.
        gtk_dialog_response(question, GTK_RESPONSE_YES);
        auto chooser = find_dialog(parent, true);
        g_assert_nonnull(chooser);
        g_assert_cmpuint(result.calls, ==, 0);
        gtk_dialog_response(chooser, GTK_RESPONSE_CANCEL);
        auto retry = find_dialog(parent);
        g_assert_nonnull(retry);
        g_assert_cmpuint(result.calls, ==, 0);
        g_assert_true(qof_book_session_not_saved(book));
        gtk_dialog_response(retry, GTK_RESPONSE_OK);
    }
    else if (scenario == 5)
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
        gtk_dialog_response(question, scenario == 0 ? GTK_RESPONSE_OK : GTK_RESPONSE_CANCEL);
    g_assert_cmpuint(result.calls, ==, 1);
    g_assert_cmpint(result.saved, ==, scenario == 0 || scenario == 4);
    gtk_dialog_response(question, GTK_RESPONSE_OK);
    g_assert_cmpuint(result.calls, ==, 1);
    g_object_unref(question);
    g_assert_true(qof_session_get_book(session) == book);
    g_assert_true(qof_book_session_not_saved(book));
    close_dialogs(parent);
    gtk_widget_destroy(GTK_WIDGET(parent));
    g_object_unref(parent);
    gnc_clear_current_session();
    if (other)
        qof_session_destroy(session);
    expected_book = nullptr;
}

void test_save_recovery(gconstpointer data)
{
    if (!display_available)
    {
        g_test_skip("No graphical display is available");
        return;
    }
    auto scenario = GPOINTER_TO_INT(data);
    fail_write = true;
    require_overwrite = false;
    writes = 0;
    auto book = dirty_book();
    expected_book = book;
    auto original = qof_session_new(book);
    qof_session_begin(original, "xml:///C:/gnc-save-recovery-test.gnucash",
                      SESSION_NORMAL_OPEN);
    g_assert_cmpint(qof_session_get_error(original), ==, ERR_BACKEND_NO_ERR);
    g_assert_null(gnc_exchange_current_session(original));
    auto parent = GTK_WINDOW(gtk_window_new(GTK_WINDOW_TOPLEVEL));
    g_object_ref_sink(parent);
    Result result;
    gnc_file_save_async(parent, completed, &result);
    auto error = find_dialog(parent);
    g_assert_nonnull(error);
    g_object_ref(error);
    g_assert_cmpuint(writes, ==, 1);
    g_assert_cmpuint(result.calls, ==, 0);
    g_assert_true(gnc_file_save_in_progress());
    g_assert_true(qof_book_session_not_saved(book));
    QofSession *other = nullptr;
    if (scenario == 1)
        gtk_widget_destroy(GTK_WIDGET(parent));
    else
    {
        if (scenario == 2)
        {
            other = qof_session_new(qof_book_new());
            g_assert_true(gnc_exchange_current_session(other) == original);
        }
        if (scenario == 3)
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
        if (scenario == 0 || scenario == 3)
        {
            auto chooser = find_dialog(parent, true);
            g_assert_nonnull(chooser);
            g_assert_cmpuint(result.calls, ==, 0);
            gtk_dialog_response(chooser, GTK_RESPONSE_CANCEL);
        }
    }
    g_assert_cmpuint(result.calls, ==, 1);
    g_assert_false(result.saved);
    g_assert_false(gnc_file_save_in_progress());
    g_assert_true(qof_book_session_not_saved(book));
    gtk_dialog_response(error, GTK_RESPONSE_CLOSE);
    g_assert_cmpuint(result.calls, ==, 1);
    g_object_unref(error);
    close_dialogs(parent);
    gtk_widget_destroy(GTK_WIDGET(parent));
    g_object_unref(parent);
    gnc_clear_current_session();
    if (other)
        qof_session_destroy(original);
    expected_book = nullptr;
}
}

int main(int argc, char **argv)
{
    g_test_init(&argc, &argv, nullptr);
    display_available = gtk_init_check(&argc, &argv);
    if (g_getenv("GNC_REQUIRE_DISPLAY"))
        g_assert_true(display_available);
    qof_init();
    g_assert_true(cashobjects_register());
    gnc_component_manager_init();
    gnc_gsettings_load_backend();
    qof_backend_unregister_all_providers();
    qof_backend_register_provider(QofBackendProvider_ptr{new SaveProvider});
    g_test_add_data_func("/gnome-utils/save-as/success", GINT_TO_POINTER(0), test_save_as);
    g_test_add_data_func("/gnome-utils/save-as/rollback", GINT_TO_POINTER(1), test_save_as);
    g_test_add_data_func("/gnome-utils/save-as/overwrite", GINT_TO_POINTER(2), test_save_as);
    g_test_add_data_func("/gnome-utils/save-as/cancel", GINT_TO_POINTER(3), test_save_as);
    g_test_add_data_func("/gnome-utils/save-as/owner-destroy", GINT_TO_POINTER(4), test_save_as);
    g_test_add_data_func("/gnome-utils/save-as/session-change", GINT_TO_POINTER(5), test_save_as);
    g_test_add_func("/gnome-utils/save/chooser-cancel", test_choose_cancel);
    g_test_add_data_func("/gnome-utils/save-query/discard", GINT_TO_POINTER(0), test_query);
    g_test_add_data_func("/gnome-utils/save-query/cancel", GINT_TO_POINTER(1), test_query);
    g_test_add_data_func("/gnome-utils/save-query/owner-destroy", GINT_TO_POINTER(2), test_query);
    g_test_add_data_func("/gnome-utils/save-query/session-change", GINT_TO_POINTER(3), test_query);
    g_test_add_data_func("/gnome-utils/save-query/save-cancel-retry", GINT_TO_POINTER(4), test_query);
    g_test_add_data_func("/gnome-utils/save-query/window-close", GINT_TO_POINTER(5), test_query);
    g_test_add_data_func("/gnome-utils/save/recovery-cancel", GINT_TO_POINTER(0), test_save_recovery);
    g_test_add_data_func("/gnome-utils/save/recovery-owner-destroy", GINT_TO_POINTER(1), test_save_recovery);
    g_test_add_data_func("/gnome-utils/save/recovery-session-change", GINT_TO_POINTER(2), test_save_recovery);
    g_test_add_data_func("/gnome-utils/save/recovery-window-close", GINT_TO_POINTER(3), test_save_recovery);
    auto result = g_test_run();
    qof_backend_unregister_all_providers();
    gnc_component_manager_shutdown();
    gnc_gsettings_shutdown();
    qof_close();
    return result;
}
