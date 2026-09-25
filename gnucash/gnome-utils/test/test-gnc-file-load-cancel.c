/*
 * test-gnc-file-load-cancel.c -- file-open lifecycle cancellation tests
 * Copyright (C) 2026 GnuCash Developers
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 */

#include <config.h>

#include <gtk/gtk.h>

#include "gnc-engine.h"
#include "gnc-file.h"
#include "gnc-hooks.h"
#include "gnc-session.h"
#include "gnc-session-transition.h"

static gchar *
large_xml_fixture (void)
{
    const gchar *directory = g_getenv ("GNC_TEST_FILES");

    g_assert_nonnull (directory);
    return g_build_filename (directory, "ms-money.gml2", NULL);
}

static void
pump_default_context (void)
{
    /* Without cancellation this is ample time for the fixture to be loaded
     * and installed as the current session. Keep the pump test-only and
     * bounded; product code never owns a nested main loop. */
    for (guint turn = 0; turn < 20000; ++turn)
        g_main_context_iteration (NULL, FALSE);
}

static void
assert_no_current_session (void)
{
    pump_default_context ();
    g_assert_false (gnc_current_session_exist ());
}

static void
test_parent_destroy_cancels_active_load (void)
{
    GtkWindow *parent;
    gchar *filename;

    if (gnc_current_session_exist ())
        gnc_clear_current_session ();
    parent = GTK_WINDOW (gtk_window_new ());
    filename = large_xml_fixture ();

    g_assert_cmpint (gnc_file_open_file (parent, filename, TRUE), ==,
                     GNC_FILE_OPEN_STARTED);
    gtk_window_destroy (parent);
    assert_no_current_session ();

    /* A completed cancellation must also have removed its registry entry. */
    gnc_hook_run (HOOK_UI_SHUTDOWN, NULL);
    assert_no_current_session ();
    g_free (filename);
}

static void
test_ui_shutdown_cancels_active_load (void)
{
    gchar *filename;

    if (gnc_current_session_exist ())
        gnc_clear_current_session ();
    filename = large_xml_fixture ();

    g_assert_cmpint (gnc_file_open_file (NULL, filename, TRUE), ==,
                     GNC_FILE_OPEN_STARTED);
    g_assert_cmpint (gnc_file_open_file (NULL, filename, TRUE), ==,
                     GNC_FILE_OPEN_QUEUED);

    /* Production shutdown seals the transition queue before the UI shutdown
     * hook cancels active loads. Otherwise completing the active load would
     * synchronously start the queued request after the hook took its snapshot. */
    gnc_session_transition_begin_shutdown (NULL);
    gnc_hook_run (HOOK_UI_SHUTDOWN, NULL);
    assert_no_current_session ();

    /* Running shutdown again verifies exactly-once terminal registry removal. */
    gnc_hook_run (HOOK_UI_SHUTDOWN, NULL);
    assert_no_current_session ();
    g_free (filename);
}

int
main (int argc, char **argv)
{
    int status;

    g_setenv ("GNC_UNINSTALLED", "1", TRUE);
    g_test_init (&argc, &argv, NULL);
    gtk_init ();
    gnc_engine_init (argc, argv);

    g_test_add_func ("/gnome-utils/file-open/parent-destroy-cancels-load",
                     test_parent_destroy_cancels_active_load);
    g_test_add_func ("/gnome-utils/file-open/ui-shutdown-cancels-load",
                     test_ui_shutdown_cancels_active_load);

    status = g_test_run ();
    if (gnc_current_session_exist ())
        gnc_clear_current_session ();
    gnc_engine_shutdown ();
    return status;
}
