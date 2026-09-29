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
#include "gnc-file.h"
#include "gnc-gsettings.h"
#include "gnc-gnome-utils.h"
#include "gnc-session.h"

namespace
{
bool display_available;
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

struct Result { unsigned calls = 0; gboolean opened = FALSE; };

void completed (gboolean opened, gpointer data)
{
    auto result = static_cast<Result *>(data);
    ++result->calls;
    result->opened = opened;
}

GtkDialog *find_dialog (GtkWindow *parent)
{
    auto windows = gtk_window_list_toplevels ();
    GtkDialog *found = nullptr;
    for (auto node = windows; node; node = node->next)
        if (GTK_IS_MESSAGE_DIALOG (node->data) &&
            gtk_window_get_transient_for (GTK_WINDOW (node->data)) == parent)
        {
            g_assert_null (found);
            found = GTK_DIALOG (node->data);
        }
    g_list_free (windows);
    return found;
}

void test_open (gconstpointer data)
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    auto scenario = GPOINTER_TO_INT (data);
    auto original = qof_session_new (qof_book_new ());
    auto book = qof_session_get_book (original);
    gnc_account_create_root (book);
    qof_book_mark_session_saved (book);
    gnc_set_current_session (original);
    auto parent = GTK_WINDOW (gtk_window_new (GTK_WINDOW_TOPLEVEL));
    g_object_ref (parent);
    begin_error = scenario < 6 ? ERR_BACKEND_LOCKED : ERR_BACKEND_NO_ERR;
    load_error = scenario >= 6 ? ERR_FILEIO_FILE_TOO_OLD : ERR_BACKEND_NO_ERR;
    loads = 0;
    Result result;
    gnc_file_open_file_async (parent, "xml:///synthetic-response/open.gnucash", FALSE,
                              completed, &result);
    g_assert_cmpuint (result.calls, ==, 0);
    g_assert_true (gnc_get_current_session () == original);
    g_assert_true (qof_session_get_book (original) == book);
    auto dialog = find_dialog (parent);
    g_assert_nonnull (dialog);
    g_object_ref (dialog);
    QofSession *other = nullptr;
    if (scenario == 2)
        gtk_widget_destroy (GTK_WIDGET (parent));
    else
    {
        if (scenario == 3 || scenario == 4)
        {
            other = qof_session_new (qof_book_new ());
            gnc_account_create_root (qof_session_get_book (other));
            qof_book_mark_session_saved (qof_session_get_book (other));
            g_assert_true (gnc_exchange_current_session (other) == original);
            if (scenario == 4)
                qof_session_destroy (original);
        }
        if (scenario == 5)
            gtk_widget_destroy (GTK_WIDGET (dialog));
        else
            gtk_dialog_response (dialog, scenario == 1 ? GTK_RESPONSE_CANCEL :
                scenario < 6 ? 2 /* Open Anyway */ :
                scenario == 6 ? GTK_RESPONSE_YES : GTK_RESPONSE_NO);
    }
    g_assert_cmpuint (result.calls, ==, 1);
    bool expected = scenario == 0 || scenario == 6;
    g_assert_cmpint (result.opened, ==, expected);
    if (!expected)
    {
        g_assert_true (gnc_get_current_session () == (other ? other : original));
        if (scenario != 4)
            g_assert_true (qof_session_get_book (original) == book);
    }
    gtk_dialog_response (dialog, GTK_RESPONSE_YES);
    g_assert_cmpuint (result.calls, ==, 1);
    g_object_unref (dialog);
    gtk_widget_destroy (GTK_WIDGET (parent));
    g_object_unref (parent);
    gnc_clear_current_session ();
    if (other && scenario != 4)
        qof_session_destroy (original);
}

void test_session_operation_gate ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    auto session = qof_session_new (qof_book_new ());
    auto book = qof_session_get_book (session);
    gnc_account_create_root (book);
    qof_book_mark_session_saved (book);
    gnc_set_current_session (session);
    auto parent = GTK_WINDOW (gtk_window_new (GTK_WINDOW_TOPLEVEL));
    g_object_ref (parent);
    auto first = gnc_gui_begin_session_operation (book);
    auto second = gnc_gui_begin_session_operation (book);
    g_assert_cmpuint (first, !=, 0);
    g_assert_cmpuint (second, !=, first);
    g_assert_true (gnc_gui_session_operation_pending ());
    loads = 0;
    Result open, save, query;
    gnc_file_open_file_async (parent, "xml:///synthetic-response/open.gnucash", FALSE,
                              completed, &open);
    gnc_file_save_async (parent, completed, &save);
    gnc_file_query_save_async (parent, TRUE, completed, &query);
    for (auto result : {&open, &save, &query})
    {
        g_assert_cmpuint (result->calls, ==, 1);
        g_assert_false (result->opened);
    }
    g_assert_null (find_dialog (parent));
    g_assert_cmpuint (loads, ==, 0);
    g_assert_true (gnc_get_current_session () == session);
    gnc_gui_end_session_operation (first);
    gnc_gui_end_session_operation (first);
    g_assert_true (gnc_gui_session_operation_pending ());
    gnc_gui_end_session_operation (second);
    g_assert_false (gnc_gui_session_operation_pending ());

    begin_error = ERR_BACKEND_LOCKED;
    load_error = ERR_BACKEND_NO_ERR;
    Result pending;
    gnc_file_open_file_async (parent, "xml:///synthetic-response/open.gnucash", FALSE,
                              completed, &pending);
    g_assert_cmpuint (pending.calls, ==, 0);
    g_assert_cmpuint (gnc_gui_begin_session_operation (book), ==, 0);
    auto dialog = find_dialog (parent);
    g_assert_nonnull (dialog);
    gtk_dialog_response (dialog, GTK_RESPONSE_CANCEL);
    g_assert_cmpuint (pending.calls, ==, 1);
    auto next = gnc_gui_begin_session_operation (book);
    g_assert_cmpuint (next, !=, 0);
    gnc_gui_end_session_operation (next);
    gtk_widget_destroy (GTK_WIDGET (parent));
    g_object_unref (parent);
    gnc_clear_current_session ();
}
}

int main (int argc, char **argv)
{
    g_test_init (&argc, &argv, nullptr);
    display_available = gtk_init_check (&argc, &argv);
    if (g_getenv ("GNC_REQUIRE_DISPLAY")) g_assert_true (display_available);
    qof_init ();
    g_assert_true (cashobjects_register ());
    gnc_component_manager_init ();
    gnc_gsettings_load_backend ();
    qof_backend_unregister_all_providers ();
    qof_backend_register_provider (QofBackendProvider_ptr {new OpenProvider});
    const char *cases[] = {"lock-accept", "lock-cancel", "owner-destroy", "session-change",
                          "source-book-destroy", "dialog-destroy", "old-format-accept",
                          "old-format-reject"};
    for (unsigned i = 0; i < G_N_ELEMENTS (cases); ++i)
    {
        auto path = g_strdup_printf ("/gnome-utils/file-open/%s", cases[i]);
        g_test_add_data_func (path, GINT_TO_POINTER (i), test_open);
        g_free (path);
    }
    g_test_add_func ("/gnome-utils/file-open/session-operation-gate", test_session_operation_gate);
    auto status = g_test_run ();
    qof_backend_unregister_all_providers ();
    gnc_component_manager_shutdown ();
    gnc_gsettings_shutdown ();
    qof_close ();
    return status;
}
