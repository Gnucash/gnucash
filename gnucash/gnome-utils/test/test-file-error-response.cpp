/* Copyright (C) 2026 GnuCash contributors
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */
#include <config.h>
#include <gtk/gtk.h>
#include <cstring>
#include "gnc-file.h"

namespace
{
gboolean display_available;

GtkWidget *
find_notice (GtkWindow *parent)
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *notice = nullptr;
    for (auto node = windows; node; node = node->next)
        if (GTK_IS_MESSAGE_DIALOG (node->data) &&
            gtk_window_get_transient_for (GTK_WINDOW (node->data)) == parent)
        {
            g_assert_null (notice);
            notice = GTK_WIDGET (node->data);
        }
    g_list_free (windows);
    return notice;
}

void
test_terminal_error (gconstpointer data)
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    auto parent = GTK_WINDOW (gtk_window_new (GTK_WINDOW_TOPLEVEL));
    auto filename = g_strdup ("/synthetic-test/missing-book.gnucash");
    auto code = static_cast<QofBackendError> (GPOINTER_TO_INT (data));
    if (code == static_cast<QofBackendError> (10000))
        g_test_expect_message ("gnc.gui", G_LOG_LEVEL_CRITICAL,
                               "*Unhandled error 10000*");
    gnc_file_show_session_error_async (parent, code, filename,
                                       GNC_FILE_DIALOG_OPEN, nullptr, nullptr);
    if (code == static_cast<QofBackendError> (10000))
        g_test_assert_expected_messages ();
    g_free (filename);
    auto notice = find_notice (parent);
    g_assert_nonnull (notice); // Product call returned before an answer.
    gchar *message = nullptr;
    g_object_get (notice, "text", &message, nullptr);
    g_assert_nonnull (message);
    g_assert_cmpuint (std::strlen (message), >, 0);
    g_free (message);
    g_object_ref (notice);
    gtk_widget_destroy (GTK_WIDGET (parent));
    gtk_dialog_response (GTK_DIALOG (notice), GTK_RESPONSE_OK);
    g_object_unref (notice);
}
}

int
main (int argc, char **argv)
{
    g_test_init (&argc, &argv, nullptr);
    display_available = gtk_init_check (&argc, &argv);
    if (g_getenv ("GNC_REQUIRE_DISPLAY")) g_assert_true (display_available);
    const struct { const char *name; QofBackendError code; } cases[] = {
        {"no-handler", ERR_BACKEND_NO_HANDLER},
        {"no-backend", ERR_BACKEND_NO_BACKEND},
        {"bad-url", ERR_BACKEND_BAD_URL},
        {"connect", ERR_BACKEND_CANT_CONNECT},
        {"connection-lost", ERR_BACKEND_CONN_LOST},
        {"too-new", ERR_BACKEND_TOO_NEW},
        {"readonly", ERR_BACKEND_READONLY},
        {"corrupt", ERR_BACKEND_DATA_CORRUPT},
        {"server", ERR_BACKEND_SERVER_ERR},
        {"permission", ERR_BACKEND_PERM},
        {"misc", ERR_BACKEND_MISC},
        {"parse", ERR_FILEIO_PARSE_ERROR},
        {"empty", ERR_FILEIO_FILE_EMPTY},
        {"unknown-type", ERR_FILEIO_UNKNOWN_FILE_TYPE},
        {"backup", ERR_FILEIO_BACKUP_ERROR},
        {"write", ERR_FILEIO_WRITE_ERROR},
        {"file-access", ERR_FILEIO_FILE_EACCES},
        {"reserved-write", ERR_FILEIO_RESERVED_WRITE},
        {"database-busy", ERR_SQL_DB_BUSY},
        {"database-library", ERR_SQL_BAD_DBI},
        {"database-test", ERR_SQL_DBI_UNTESTABLE},
        {"unknown", static_cast<QofBackendError> (10000)},
        {"database-too-new-warning", ERR_SQL_DB_TOO_NEW},
        {"upgrade-warning", ERR_FILEIO_FILE_UPGRADE},
    };
    for (const auto &item : cases)
    {
        auto path = g_strdup_printf ("/gnome-utils/file-error/%s", item.name);
        g_test_add_data_func (path, GINT_TO_POINTER (item.code), test_terminal_error);
        g_free (path);
    }
    return g_test_run ();
}
