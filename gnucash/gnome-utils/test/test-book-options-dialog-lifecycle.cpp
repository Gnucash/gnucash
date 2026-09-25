/*
 * test-book-options-dialog-lifecycle.cpp -- Book Options dialog lifetime tests
 *
 * This program is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 */

#include <config.h>

#include <gtk/gtk.h>
#include <libguile.h>

#include <cstdlib>

#include "Account.h"
#include "business-options-gnome.h"
#include "gnc-component-manager.h"
#include "gnc-engine.h"
#include "gnc-main-window.h"
#include "gnc-optiondb.h"
#include "gnc-prefs-utils.h"
#include "gnc-session.h"
#include "qof.h"

static void
object_finalized (gpointer data, GObject *object)
{
    gboolean *finalized = static_cast<gboolean *>(data);

    *finalized = TRUE;
    (void)object;
}

static void
drain_main_context (void)
{
    while (g_main_context_pending (NULL))
        g_main_context_iteration (NULL, FALSE);
}

static GtkWidget *
find_buildable_widget (GtkWidget *widget, const gchar *buildable_id)
{
    if (GTK_IS_BUILDABLE (widget) &&
        g_strcmp0 (gtk_buildable_get_buildable_id (GTK_BUILDABLE (widget)),
                    buildable_id) == 0)
        return widget;

    for (auto child = gtk_widget_get_first_child (widget); child;
         child = gtk_widget_get_next_sibling (child))
    {
        auto result = find_buildable_widget (child, buildable_id);

        if (result)
            return result;
    }
    return nullptr;
}

static GtkWidget *
open_book_options (void)
{
    auto dialog = gnc_book_options_dialog_cb (FALSE, nullptr, nullptr);

    g_assert_nonnull (dialog);
    g_object_ref (dialog);
    return dialog;
}

static void
assert_closed_and_finalized (GtkWidget *dialog, gboolean use_window_close)
{
    gboolean finalized = FALSE;

    g_object_weak_ref (G_OBJECT (dialog), object_finalized, &finalized);
    if (use_window_close)
        gtk_window_close (GTK_WINDOW (dialog));
    else
    {
        auto cancel = find_buildable_widget (dialog, "cancelbutton");

        g_assert_nonnull (cancel);
        g_signal_emit_by_name (cancel, "clicked");
    }
    drain_main_context ();
    g_object_unref (dialog);
    drain_main_context ();
    g_assert_true (finalized);
}

static void
test_book_options_dialog_lifecycle (void)
{
    QofSession *session = qof_session_new (qof_book_new ());
    QofBook *book = qof_session_get_book (session);
    GtkWidget *dialog;
    GtkWidget *apply;

    gnc_set_current_session (session);
    gnc_account_create_root (book);

    dialog = open_book_options ();
    for (guint attempt = 0; attempt < 3; attempt++)
        g_assert_null (gnc_book_options_dialog_cb (FALSE, nullptr, nullptr));

    apply = find_buildable_widget (dialog, "applybutton");
    g_assert_nonnull (apply);
    g_signal_emit_by_name (apply, "clicked");
    g_assert_true (GPOINTER_TO_INT (g_object_get_data
                                    (G_OBJECT (dialog),
                                     "gnc-options-dialog-applied")));
    assert_closed_and_finalized (dialog, TRUE);

    dialog = open_book_options ();
    assert_closed_and_finalized (dialog, FALSE);

    for (guint round = 0; round < 2; round++)
    {
        dialog = open_book_options ();
        assert_closed_and_finalized (dialog, TRUE);
    }

    gnc_clear_current_session ();
}

static void
run_tests_with_guile (void*, int, char **)
{
    int status;

    gtk_init ();
    gnc_engine_init (0, nullptr);
    gnc_prefs_init ();
    gnc_component_manager_init ();
    scm_c_use_module ("gnucash reports");
    scm_c_use_module ("gnucash report report-core");
    gnc_business_options_gnome_initialize ();

    g_test_add_func ("/gnome-utils/book-options/dialog-lifecycle",
                     test_book_options_dialog_lifecycle);
    status = g_test_run ();

    gnc_component_manager_shutdown ();
    gnc_prefs_remove_registered ();
    gnc_engine_shutdown ();
    exit (status);
}

int
main (int argc, char **argv)
{
    g_setenv ("GSETTINGS_BACKEND", "memory", TRUE);
    g_test_init (&argc, &argv, NULL);
    scm_boot_guile (argc, argv, run_tests_with_guile, nullptr);
    return 0;
}
