/* Copyright (C) 2026 GnuCash contributors
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 */

#include <config.h>
#include <gtk/gtk.h>
#include <glib/gstdio.h>
#include <gtest/gtest.h>
#include "test-logging.hpp"
#include "qof.h"

extern "C"
{
#if defined(TEST_BI_IMPORT)
#include "dialog-bi-import.h"
#elif defined(TEST_CUSTOMER_IMPORT)
#include "dialog-customer-import.h"
#else
#include "csv-account-import.h"
#endif
}

static GtkWidget *
find_warning ()
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *dialog = nullptr;
    for (auto window = windows; window; window = window->next)
        if (GTK_IS_MESSAGE_DIALOG (window->data))
        {
            if (dialog)
            {
                g_list_free (windows);
                return nullptr;
            }
            dialog = GTK_WIDGET (window->data);
        }
    g_list_free (windows);
    return dialog;
}

class InvalidRegexImportTest : public ::testing::Test
{
protected:
    void SetUp () override
    {
        GError *error = nullptr;
        directory = g_dir_make_tmp ("gnucash-regex-response-XXXXXX", &error);
        ASSERT_EQ (error, nullptr);
        ASSERT_NE (directory, nullptr);
        filename = g_build_filename (directory, "synthetic.csv", nullptr);
        ASSERT_TRUE (g_file_set_contents (filename, "synthetic\n", -1, &error));
        ASSERT_EQ (error, nullptr);
        store = gtk_list_store_new (1, G_TYPE_STRING);
        parent = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    }

    void TearDown () override
    {
        if (store)
            g_object_unref (store);
        if (parent)
            gtk_widget_destroy (parent);
        if (filename)
        {
            g_remove (filename);
            g_free (filename);
        }
        if (directory)
        {
            g_rmdir (directory);
            g_free (directory);
        }
    }

    gchar *directory{};
    gchar *filename{};
    GtkListStore *store{};
    GtkWidget *parent{};
};

TEST_F (InvalidRegexImportTest, ReportsErrorAndReleasesInput)
{
#if defined(TEST_BI_IMPORT)
    bi_import_stats stats{};
    EXPECT_EQ (gnc_bi_import_read_file (filename, "[", store, 0, &stats),
               RESULT_ERROR_IN_REGEXP);
    EXPECT_EQ (stats.ignored_lines, nullptr);
#elif defined(TEST_CUSTOMER_IMPORT)
    customer_import_stats stats{};
    EXPECT_EQ (gnc_customer_import_read_file (filename, "[", store, 0, &stats),
               CI_RESULT_ERROR_IN_REGEXP);
    EXPECT_EQ (stats.ignored_lines, nullptr);
#else
    EXPECT_EQ (csv_import_read_file (GTK_WINDOW (parent), filename, "[", store, 0),
               RESULT_ERROR_IN_REGEXP);
#endif
    auto dialog = find_warning ();
    ASSERT_NE (dialog, nullptr);
    EXPECT_EQ (gtk_tree_model_iter_n_children (GTK_TREE_MODEL (store), nullptr), 0);
    /* The reader has released its input and owns no delayed file/model work. */
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    EXPECT_EQ (find_warning (), nullptr);
}

int
main (int argc, char **argv)
{
    ::testing::InitGoogleTest (&argc, argv);
    if (!gtk_init_check (&argc, &argv))
        g_error ("GTK display is required for regex response tests");
    qof_init ();
    gnc::test::initialize_logging ();
    auto result = RUN_ALL_TESTS ();
    qof_close ();
    return result;
}
