/* test-readonly-threshold-response.cpp -- Read-only date warning response.
 *
 * Copyright (C) 2026 GnuCash contributors
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 */

#include <config.h>

#include <gtk/gtk.h>

#include "cashobjects.h"
#include "datecell.h"
#include "gnc-date.h"
#include "gnc-session.h"
#include "qofbook.h"
#include "qofsession.h"

static gboolean display_available;

static void
warning_destroyed (G_GNUC_UNUSED GtkWidget *widget, gpointer data)
{
    ++*static_cast<guint *> (data);
}

static void
test_datecell_threshold_adjustment_and_response (void)
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }

    auto session = qof_session_new (qof_book_new ());
    auto book = qof_session_get_book (session);
    qof_book_begin_edit (book);
    qof_instance_set (QOF_INSTANCE (book), "autoreadonly-days", (gdouble)30,
                      NULL);
    qof_book_commit_edit (book);
    gnc_set_current_session (session);

    auto cell = gnc_date_cell_new ();
    auto date_cell = reinterpret_cast<DateCell *> (cell);
    char old_date[MAX_DATE_LENGTH + 1];
    qof_print_date_dmy_buff (old_date, MAX_DATE_LENGTH, 1, 1, 2000);
    g_free (cell->value);
    cell->value = g_strdup (old_date);

    auto threshold = qof_book_get_autoreadonly_gdate (book);
    g_assert_nonnull (threshold);
    time64 actual{};
    gnc_date_cell_get_date (date_cell, &actual, TRUE);
    /* Register dates use local midnight; gdate_to_time64 uses the neutral
     * time convention. The read-only boundary is a calendar date. */
    auto actual_date = time64_to_gdate (actual);
    g_assert_cmpint (g_date_compare (&actual_date, threshold), ==, 0);
    g_date_free (threshold);

    GtkWidget *warning = nullptr;
    auto windows = gtk_window_list_toplevels ();
    for (auto item = windows; item; item = item->next)
    {
        if (GTK_IS_MESSAGE_DIALOG (item->data))
        {
            warning = GTK_WIDGET (item->data);
            break;
        }
    }
    g_assert_nonnull (warning);
    g_assert_true (gtk_window_get_modal (GTK_WINDOW (warning)));
    g_assert_true (gtk_window_get_destroy_with_parent (GTK_WINDOW (warning)));
    guint destroy_count = 0;
    g_signal_connect (warning, "destroy", G_CALLBACK (warning_destroyed),
                      &destroy_count);
    g_object_ref (warning);
    GtkWidget *weak_warning = warning;
    g_object_add_weak_pointer (G_OBJECT (warning),
                               reinterpret_cast<gpointer *> (&weak_warning));
    gtk_dialog_response (GTK_DIALOG (warning), GTK_RESPONSE_OK);
    g_assert_cmpuint (destroy_count, ==, 1);
    gtk_dialog_response (GTK_DIALOG (warning), GTK_RESPONSE_OK);
    g_assert_cmpuint (destroy_count, ==, 1);
    g_object_unref (warning);
    g_assert_null (weak_warning);
    g_list_free (windows);

    gnc_basic_cell_destroy (cell);
    gnc_clear_current_session ();
}

int
main (int argc, char **argv)
{
    g_test_init (&argc, &argv, NULL);
    display_available = gtk_init_check (&argc, &argv);
    if (g_getenv ("GNC_REQUIRE_DISPLAY"))
        g_assert_true (display_available);
    qof_init ();
    g_assert_true (cashobjects_register ());
    g_test_add_func ("/register/datecell/readonly-threshold-response",
                     test_datecell_threshold_adjustment_and_response);
    auto result = g_test_run ();
    qof_close ();
    return result;
}
