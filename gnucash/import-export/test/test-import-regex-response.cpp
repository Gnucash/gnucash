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

static gboolean display_available;

static GtkWidget *
find_warning ()
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *dialog = nullptr;
    for (auto window = windows; window; window = window->next)
        if (GTK_IS_MESSAGE_DIALOG (window->data))
        {
            g_assert_null (dialog);
            dialog = GTK_WIDGET (window->data);
        }
    g_list_free (windows);
    return dialog;
}

static void
test_invalid_regex ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    GError *error = nullptr;
    auto directory = g_dir_make_tmp ("gnucash-regex-response-XXXXXX", &error);
    g_assert_no_error (error);
    auto filename = g_build_filename (directory, "synthetic.csv", nullptr);
    g_assert_true (g_file_set_contents (filename, "synthetic\n", -1, &error));
    g_assert_no_error (error);
    auto store = gtk_list_store_new (1, G_TYPE_STRING);
    auto parent = gtk_window_new (GTK_WINDOW_TOPLEVEL);
#if defined(TEST_BI_IMPORT)
    bi_import_stats stats{};
    g_assert_cmpint (gnc_bi_import_read_file (filename, "[", store, 0, &stats),
                     ==, RESULT_ERROR_IN_REGEXP);
    g_assert_null (stats.ignored_lines);
#elif defined(TEST_CUSTOMER_IMPORT)
    customer_import_stats stats{};
    g_assert_cmpint (gnc_customer_import_read_file (filename, "[", store, 0, &stats),
                     ==, CI_RESULT_ERROR_IN_REGEXP);
    g_assert_null (stats.ignored_lines);
#else
    g_assert_cmpint (csv_import_read_file (GTK_WINDOW (parent), filename, "[", store, 0),
                     ==, RESULT_ERROR_IN_REGEXP);
#endif
    auto dialog = find_warning ();
    g_assert_nonnull (dialog);
    g_assert_cmpint (gtk_tree_model_iter_n_children (GTK_TREE_MODEL (store), nullptr), ==, 0);
    /* The reader has released its input and owns no delayed file/model work. */
    g_object_unref (store);
    g_assert_cmpint (g_remove (filename), ==, 0);
    g_assert_cmpint (g_rmdir (directory), ==, 0);
    g_free (filename);
    g_free (directory);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    g_assert_null (find_warning ());
    gtk_widget_destroy (parent);
}

int
main (int argc, char **argv)
{
    g_test_init (&argc, &argv, nullptr);
    display_available = gtk_init_check (&argc, &argv);
    if (g_getenv ("GNC_REQUIRE_DISPLAY"))
        g_assert_true (display_available);
    g_test_add_func ("/import/regex/response-and-cleanup", test_invalid_regex);
    return g_test_run ();
}
