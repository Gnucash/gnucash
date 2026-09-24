/********************************************************************
 * test-gnc-html-report-document.cpp -- report document root tests  *
 *                                                                  *
 * Copyright 2026 GnuCash Contributors                              *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 ********************************************************************/

#include <gtk/gtk.h>
#include <glib/gstdio.h>

#include "gnc-html.h"

static void
test_report_documents_are_private_and_unique (void)
{
    GError *error = nullptr;
    const gchar *root = gnc_html_get_report_document_root ();
    gchar *first;
    gchar *second;

    g_assert_nonnull (root);
    g_assert_true (g_file_test (root, G_FILE_TEST_IS_DIR));

    first = gnc_html_create_report_document (&error);
    g_assert_no_error (error);
    second = gnc_html_create_report_document (&error);
    g_assert_no_error (error);

    g_assert_nonnull (first);
    g_assert_nonnull (second);
    g_autofree gchar *first_parent = g_path_get_dirname (first);
    g_autofree gchar *second_parent = g_path_get_dirname (second);

    g_assert_cmpstr (first_parent, ==, root);
    g_assert_cmpstr (second_parent, ==, root);
    g_assert_cmpstr (first, !=, second);
    g_assert_true (g_file_test (first, G_FILE_TEST_IS_REGULAR));
    g_assert_true (g_file_test (second, G_FILE_TEST_IS_REGULAR));

    g_assert_cmpint (g_remove (first), ==, 0);
    g_assert_cmpint (g_remove (second), ==, 0);
    g_free (first);
    g_free (second);
}

int
main (int argc, char **argv)
{
    g_test_init (&argc, &argv, NULL);
    g_test_add_func ("/html/report-document/private-and-unique",
                     test_report_documents_are_private_and_unique);
    return g_test_run ();
}