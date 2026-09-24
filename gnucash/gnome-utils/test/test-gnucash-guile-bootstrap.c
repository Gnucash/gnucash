/*
 * test-gnucash-guile-bootstrap.c -- Guile/GApplication lifecycle test
 * Copyright (C) 2026 GnuCash Developers
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 */

#include <config.h>

#include <gtk/gtk.h>
#include <libguile.h>
#include <string.h>

#include "gnucash-guile-bootstrap.h"

#define CHILD_ARGUMENT "--guile-application-child"
#define SUCCESS_MARKER "GUILE_APPLICATION_LIFECYCLE_OK"
#define GUILE_EXIT_FAILURE "Cannot exit gracefully when init is in progress"

typedef struct
{
    gboolean activated_in_guile;
} GuileApplicationState;

static gchar *test_program_path;

static gboolean
quit_application_cb (gpointer user_data)
{
    g_application_quit (G_APPLICATION (user_data));
    return G_SOURCE_REMOVE;
}

static void
application_activate_cb (GApplication *application, gpointer user_data)
{
    GuileApplicationState *state = user_data;
    SCM value = scm_c_eval_string ("(+ 20 22)");

    state->activated_in_guile = scm_to_int (value) == 42;
    g_idle_add (quit_application_cb, application);
}

static int
run_child_application (int argc, char **argv, void *user_data)
{
    GuileApplicationState state = { FALSE };
    GtkApplication *application;
    gchar *application_argv[] = { (gchar *)"test-gnucash-guile-bootstrap", NULL };
    SCM value;
    int status;

    (void)argc;
    (void)argv;
    (void)user_data;

    gtk_init ();
    application = gtk_application_new (
        "org.gnucash.GnuCash.Test.GuileApplication",
        G_APPLICATION_NON_UNIQUE);
    g_signal_connect (application, "activate",
                      G_CALLBACK (application_activate_cb), &state);

    status = g_application_run (G_APPLICATION (application), 1,
                                application_argv);
    value = scm_c_eval_string ("(+ 40 2)");
    g_object_unref (application);

    if (status != 0 || !state.activated_in_guile || scm_to_int (value) != 42)
        return 1;

    g_print (SUCCESS_MARKER "\n");
    return 0;
}

static void
test_application_lifecycle_stays_in_guile (void)
{
    gchar *child_argv[] = { test_program_path, (gchar *)CHILD_ARGUMENT, NULL };
    gchar *standard_output = NULL;
    gchar *standard_error = NULL;
    gint wait_status = 0;
    GError *error = NULL;

    g_assert_true (g_spawn_sync (NULL, child_argv, NULL, G_SPAWN_DEFAULT,
                                 NULL, NULL, &standard_output, &standard_error,
                                 &wait_status, &error));
    g_assert_no_error (error);
    g_assert_true (g_spawn_check_wait_status (wait_status, &error));
    g_assert_no_error (error);
    g_assert_nonnull (strstr (standard_output, SUCCESS_MARKER));
    g_assert_null (strstr (standard_error, GUILE_EXIT_FAILURE));

    g_free (standard_error);
    g_free (standard_output);
}

int
main (int argc, char **argv)
{
    int status;

    if (argc == 2 && g_strcmp0 (argv[1], CHILD_ARGUMENT) == 0)
        gnc_run_with_guile (argc, argv, run_child_application, NULL);

    test_program_path = g_canonicalize_filename (argv[0], NULL);
    g_test_init (&argc, &argv, NULL);
    g_test_add_func ("/gnucash/guile/application-lifecycle",
                     test_application_lifecycle_stays_in_guile);
    status = g_test_run ();
    g_free (test_program_path);
    return status;
}
