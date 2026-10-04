/* Copyright (C) 2026 GnuCash contributors
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */
#include <config.h>
#include <gtk/gtk.h>
#include <gtest/gtest.h>
#include "test-logging.hpp"
#include <cstring>
#include <string>
#include "gnc-file.h"

namespace
{
static GtkWidget *
find_notice (GtkWindow *parent)
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *notice = nullptr;
    for (auto node = windows; node; node = node->next)
        if (GTK_IS_MESSAGE_DIALOG (node->data) &&
            gtk_window_get_transient_for (GTK_WINDOW (node->data)) == parent)
        {
            EXPECT_EQ (notice, nullptr);
            notice = GTK_WIDGET (node->data);
        }
    g_list_free (windows);
    return notice;
}

struct ErrorCase
{
    const char *name;
    QofBackendError code;
};

class FileErrorResponseTest : public ::testing::TestWithParam<ErrorCase>
{
protected:
    void SetUp () override
    {
        parent = GTK_WINDOW (gtk_window_new (GTK_WINDOW_TOPLEVEL));
        g_object_ref_sink (parent);
        filename = g_strdup ("/synthetic-test/missing-book.gnucash");
    }

    void TearDown () override
    {
        gtk_widget_destroy (GTK_WIDGET (parent));
        g_clear_object (&notice);
        g_clear_object (&parent);
        g_free (filename);
    }

    GtkWindow *parent{};
    GtkWidget *notice{};
    gchar *filename{};
    unsigned int completion_count{};

    static void completed ([[maybe_unused]] GtkWindow *parent,
                           [[maybe_unused]] gint response, gpointer data)
    {
        ++static_cast<FileErrorResponseTest *> (data)->completion_count;
    }
};

TEST_P (FileErrorResponseTest, NoticeOwnsMessageAndIgnoresLateParentResponse)
{
    auto code = GetParam ().code;
    if (code == static_cast<QofBackendError> (10000))
        g_test_expect_message ("gnc.gui", G_LOG_LEVEL_CRITICAL,
                               "*Unhandled error 10000*");
    gnc_file_show_session_error_async (parent, code, filename,
                                       GNC_FILE_DIALOG_OPEN, completed, this);
    if (code == static_cast<QofBackendError> (10000))
        g_test_assert_expected_messages ();
    g_free (filename);
    filename = nullptr;
    notice = find_notice (parent);
    ASSERT_NE (notice, nullptr); // Product call returned before an answer.
    EXPECT_EQ (completion_count, 0u);
    g_object_ref (notice);
    gchar *message = nullptr;
    g_object_get (notice, "text", &message, nullptr);
    ASSERT_NE (message, nullptr);
    EXPECT_GT (std::strlen (message), 0u);
    g_free (message);
    gtk_widget_destroy (GTK_WIDGET (parent));
    EXPECT_EQ (completion_count, 1u);
    gtk_dialog_response (GTK_DIALOG (notice), GTK_RESPONSE_OK);
    EXPECT_EQ (completion_count, 1u);
}
const ErrorCase cases[] = {
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

INSTANTIATE_TEST_SUITE_P (BackendErrors, FileErrorResponseTest,
                         ::testing::ValuesIn (cases),
                         [] (const auto &info) {
                             std::string name{info.param.name};
                             for (auto &character : name)
                                 if (character == '-') character = '_';
                             return name;
                         });
}

int
main (int argc, char **argv)
{
    ::testing::InitGoogleTest (&argc, argv);
    if (!gtk_init_check (&argc, &argv))
    {
        g_printerr ("GTK display initialization failed for file error response tests.\n");
        return 1;
    }
    gnc::test::initialize_logging ();
    return RUN_ALL_TESTS ();
}
