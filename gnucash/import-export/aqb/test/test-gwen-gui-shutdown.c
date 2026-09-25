/*
 * test-gwen-gui-shutdown.c -- Gwen synchronous GUI wait lifecycle tests
 * Copyright (C) 2026 GnuCash Developers
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 */

#include <config.h>

#include <gtk/gtk.h>
#include <gwenhywfar/gwenhywfar.h>
#include <gwenhywfar/configmgr.h>
#include <gwenhywfar/i18n.h>
#include <gwenhywfar/gui.h>
#include <gwenhywfar/pathmanager.h>

#include "gnc-ab-utils.h"
#include "gnc-component-manager.h"
#include "gnc-engine.h"
#include "gnc-gwen-gui.h"
#include "gnc-hooks.h"
#include "gnc-module.h"
#include "qof.h"
#include "gnc-session.h"

typedef struct
{
    guint init_wrapper_calls;
    guint fini_wrapper_calls;
    guint raw_init_calls;
    guint raw_fini_calls;
    guint library_clients;
    guint abi_frames;
    guint wait_count;
    guint finalize_source_calls;
    guint finalize_destroy_calls;
    guint gui_finalize_calls;
    guint component_register_calls;
    guint component_unregister_calls;
    guint finalize_source_id;
    guint application_barrier_connect_calls;
    guint application_barrier_disconnect_calls;
    gulong application_barrier_handler_id;
} GwenLifecycleState;

void gnc_gwen_gui_test_get_state (guint*, guint*, guint*, guint*, guint*,
                                  guint*, guint*, guint*, guint*, guint*,
                                  guint*, guint*, guint*, guint*, guint*,
                                  gulong*);
void gnc_gwen_gui_test_take_permanent_certs (GWEN_DB_NODE *certs);

typedef struct
{
    const gchar *title;
    GApplication *application;
    gint input_result;
    guint shutdown_calls;
    guint32 post_shutdown_showbox;
    guint32 post_shutdown_progress;
    gint post_shutdown_progress_advance;
    gint post_shutdown_progress_end;
} WaitTestState;

static GtkWindow *
find_window (const gchar *title)
{
    GListModel *windows = gtk_window_get_toplevels ();

    for (guint index = 0; index < g_list_model_get_n_items (windows); index++)
    {
        GtkWindow *window = g_list_model_get_item (windows, index);

        if (g_strcmp0 (gtk_window_get_title (window), title) == 0)
            return window;
        g_object_unref (window);
    }
    return NULL;
}

static gboolean
activate_default_window_cb (gpointer user_data)
{
    const gchar *title = user_data;
    GtkWindow *window = find_window (title);
    GtkWidget *button;

    g_assert_nonnull (window);
    button = gtk_window_get_default_widget (window);
    g_assert_true (GTK_IS_BUTTON (button));
    g_assert_true (gtk_widget_activate (button));
    g_object_unref (window);
    return G_SOURCE_REMOVE;
}

static gboolean
close_window_cb (gpointer user_data)
{
    GtkWindow *window = find_window (user_data);

    g_assert_nonnull (window);
    gtk_widget_set_visible (GTK_WIDGET (window), FALSE);
    gtk_window_destroy (window);
    g_object_unref (window);
    return G_SOURCE_REMOVE;
}

static gboolean
shutdown_active_waits_cb (gpointer user_data)
{
    WaitTestState *state = user_data;

    state->shutdown_calls++;
    gnc_hook_run (HOOK_UI_SHUTDOWN, NULL);
    g_application_quit (state->application);
    G_GNUC_BEGIN_IGNORE_DEPRECATIONS
    state->post_shutdown_showbox = GWEN_Gui_ShowBox (
        0, "Post-shutdown showbox", "Must not be shown", 0);
    G_GNUC_END_IGNORE_DEPRECATIONS
    state->post_shutdown_progress = GWEN_Gui_ProgressStart (
        GWEN_GUI_PROGRESS_SHOW_PROGRESS, "Post-shutdown progress",
        "Must not be started", 100, 0);
    state->post_shutdown_progress_advance = GWEN_Gui_ProgressAdvance (
        state->post_shutdown_progress, GWEN_GUI_PROGRESS_NONE);
    state->post_shutdown_progress_end = GWEN_Gui_ProgressEnd (
        state->post_shutdown_progress);
    gnc_GWEN_Gui_shutdown ();
    gnc_GWEN_Gui_shutdown ();
    return G_SOURCE_REMOVE;
}

static gboolean
submit_short_input_cb (gpointer user_data)
{
    WaitTestState *state = user_data;
    GtkWindow *window = find_window (state->title);
    GtkWidget *button;

    g_assert_nonnull (window);
    button = gtk_window_get_default_widget (window);
    g_assert_true (GTK_IS_BUTTON (button));
    g_idle_add (shutdown_active_waits_cb, state);
    g_assert_true (gtk_widget_activate (button));
    g_object_unref (window);
    return G_SOURCE_REMOVE;
}

static gboolean
start_nested_alert_wait_cb (gpointer user_data)
{
    WaitTestState *state = user_data;
    gchar input[16] = "x";

    g_idle_add (submit_short_input_cb, state);
    state->input_result = GWEN_Gui_InputBox (
        GWEN_GUI_INPUT_FLAGS_SHOW, state->title, "Enter at least three characters",
        input, 3, sizeof (input), 0);
    return G_SOURCE_REMOVE;
}

static void
assert_window_absent (const gchar *title)
{
    GtkWindow *window = find_window (title);

    g_assert_null (window);
}

static GwenLifecycleState
get_lifecycle_state (void)
{
    GwenLifecycleState state = { 0 };

    gnc_gwen_gui_test_get_state (
        &state.init_wrapper_calls, &state.fini_wrapper_calls,
        &state.raw_init_calls, &state.raw_fini_calls,
        &state.library_clients, &state.abi_frames, &state.wait_count,
        &state.finalize_source_calls, &state.finalize_destroy_calls,
        &state.gui_finalize_calls, &state.component_register_calls,
        &state.component_unregister_calls, &state.finalize_source_id,
        &state.application_barrier_connect_calls,
        &state.application_barrier_disconnect_calls,
        &state.application_barrier_handler_id);
    return state;
}

static guint
gwen_component_count (void)
{
    GList *components = gnc_find_gui_components ("dialog-hbcilog", NULL, NULL);
    guint count = g_list_length (components);

    g_list_free (components);
    return count;
}

static void
init_gwen_for_test (void)
{
    gnc_GWEN_Init ();
    gnc_GWEN_Init ();
#ifdef GNC_TEST_GWEN_CONFIGMGR_DIR
    g_assert_cmpint (GWEN_PathManager_AddPath (
                         GWEN_PM_LIBNAME, GWEN_PM_LIBNAME,
                         GWEN_CONFIGMGR_PLUGIN_NAME,
                         GNC_TEST_GWEN_CONFIGMGR_DIR),
                     ==, 0);
#endif
}

static GncGWENGui *
create_gwen_gui_for_test (void)
{
    GncGWENGui *gui;
    gboolean expect_missing_i18n;

    expect_missing_i18n = GWEN_I18N_BindTextDomain_Dir (
        "gnucash-gwen-test-probe", ".") == GWEN_ERROR_NOT_SUPPORTED;
    if (expect_missing_i18n)
        g_test_expect_message (AQBANKING_LOGDOMAIN, G_LOG_LEVEL_CRITICAL,
                               "*Could not bind textdomain (-68)*");
    gui = gnc_GWEN_Gui_get (NULL);
    if (expect_missing_i18n)
        g_test_assert_expected_messages ();

    return gui;
}

static void
run_gwen_wait_scenario_cb (GApplication *application, gpointer user_data)
{
    const gchar *response_title = "Gwen response wait";
    const gchar *close_title = "Gwen external close wait";
    const gchar *outer_title = "Gwen outer shutdown wait";
    WaitTestState *state = user_data;
    GwenLifecycleState lifecycle;
    gint result;

    state->application = application;
    init_gwen_for_test ();
    g_assert_cmpuint (gwen_component_count (), ==, 0);
    /* The GUI owns and frees this after shutdown. The lifecycle test must not
     * touch AqBanking's configuration just to populate an empty cert store. */
    gnc_gwen_gui_test_take_permanent_certs (GWEN_DB_Group_new ("certs"));
    g_assert_nonnull (create_gwen_gui_for_test ());
    g_assert_cmpuint (gwen_component_count (), ==, 1);

    g_idle_add (activate_default_window_cb, (gpointer)response_title);
    result = GWEN_Gui_MessageBox (0, response_title, "Choose the response",
                                  "First", "Second", NULL, 0);
    g_assert_cmpint (result, ==, 1);
    assert_window_absent (response_title);

    g_idle_add (close_window_cb, (gpointer)close_title);
    result = GWEN_Gui_MessageBox (0, close_title, "Close this window",
                                  "Close", NULL, NULL, 0);
    g_assert_cmpint (result, ==, 0);
    assert_window_absent (close_title);

    g_idle_add (start_nested_alert_wait_cb, state);
    result = GWEN_Gui_MessageBox (0, outer_title, "Wait while input is requested",
                                  "Close", NULL, NULL, 0);
    g_assert_cmpint (result, ==, 0);
    g_assert_cmpint (state->input_result, ==, -1);
    g_assert_cmpuint (state->shutdown_calls, ==, 1);
    g_assert_cmpuint (state->post_shutdown_showbox, ==, 0);
    g_assert_cmpuint (state->post_shutdown_progress, ==, 0);
    g_assert_cmpint (state->post_shutdown_progress_advance, !=, 0);
    g_assert_cmpint (state->post_shutdown_progress_end, ==, 0);
    assert_window_absent (outer_title);
    assert_window_absent (state->title);
    assert_window_absent ("Post-shutdown showbox");
    assert_window_absent ("Post-shutdown progress");

    /* Wait-unregister owns only the registry entry. The full foreign ABI
     * frame has returned, but neither the idle finalizer nor the application
     * shutdown barrier can run before this activate callback returns. */
    lifecycle = get_lifecycle_state ();
    g_assert_cmpuint (lifecycle.init_wrapper_calls, ==, 2);
    g_assert_cmpuint (lifecycle.fini_wrapper_calls, ==, 0);
    g_assert_cmpuint (lifecycle.raw_init_calls, ==, 1);
    g_assert_cmpuint (lifecycle.raw_fini_calls, ==, 0);
    g_assert_cmpuint (lifecycle.library_clients, ==, 2);
    g_assert_cmpuint (lifecycle.abi_frames, ==, 0);
    g_assert_cmpuint (lifecycle.wait_count, ==, 0);
    g_assert_cmpuint (lifecycle.finalize_source_calls, ==, 0);
    g_assert_cmpuint (lifecycle.finalize_destroy_calls, ==, 0);
    g_assert_cmpuint (lifecycle.gui_finalize_calls, ==, 0);
    g_assert_cmpuint (lifecycle.component_register_calls, ==, 1);
    g_assert_cmpuint (lifecycle.component_unregister_calls, ==, 0);
    g_assert_cmpuint (lifecycle.finalize_source_id, !=, 0);
    g_assert_cmpuint (lifecycle.application_barrier_connect_calls, ==, 1);
    g_assert_cmpuint (lifecycle.application_barrier_disconnect_calls, ==, 0);
    g_assert_cmpuint (lifecycle.application_barrier_handler_id, !=, 0);
}

static void
test_gwen_wait_lifecycle (void)
{
    WaitTestState state = { .title = "Gwen short input" };
    GtkApplication *application = gtk_application_new (
        "org.gnucash.GwenShutdownTest", G_APPLICATION_NON_UNIQUE);
    GwenLifecycleState lifecycle;
    gint run_status;

    g_signal_connect (application, "activate",
                      G_CALLBACK (run_gwen_wait_scenario_cb), &state);
    run_status = g_application_run (G_APPLICATION (application), 0, NULL);
    g_assert_cmpint (run_status, ==, 0);

    /* g_application_quit() prevented another idle turn. The application's
     * shutdown signal therefore removed the pending source; its DestroyNotify
     * completed the owner synchronously and exactly once. */
    lifecycle = get_lifecycle_state ();
    g_assert_cmpuint (lifecycle.finalize_source_calls, ==, 0);
    g_assert_cmpuint (lifecycle.finalize_destroy_calls, ==, 1);
    g_assert_cmpuint (lifecycle.gui_finalize_calls, ==, 1);
    g_assert_cmpuint (lifecycle.component_unregister_calls, ==, 1);
    g_assert_cmpuint (lifecycle.finalize_source_id, ==, 0);
    g_assert_cmpuint (lifecycle.application_barrier_connect_calls, ==, 1);
    g_assert_cmpuint (lifecycle.application_barrier_disconnect_calls, ==, 1);
    g_assert_cmpuint (lifecycle.application_barrier_handler_id, ==, 0);
    g_assert_cmpuint (gwen_component_count (), ==, 0);
    gnc_close_gui_component_by_session (gnc_get_current_session ());
    g_assert_cmpuint (gwen_component_count (), ==, 0);

    /* A late logging request has no library ownership and cannot reactivate
     * the permanently quiescent GUI. A late Init is explicitly rejected. */
    gnc_GWEN_Gui_log_init ();
    g_assert_null (GWEN_Gui_GetGui ());
    lifecycle = get_lifecycle_state ();
    g_assert_cmpuint (lifecycle.raw_init_calls, ==, 1);
    g_assert_cmpuint (lifecycle.library_clients, ==, 2);
    g_test_expect_message ("gnc.import.aqbanking", G_LOG_LEVEL_WARNING,
                           "*cannot be reinitialized after UI shutdown*");
    gnc_GWEN_Init ();
    g_test_assert_expected_messages ();
    lifecycle = get_lifecycle_state ();
    g_assert_cmpuint (lifecycle.init_wrapper_calls, ==, 3);
    g_assert_cmpuint (lifecycle.raw_init_calls, ==, 1);
    g_assert_cmpuint (lifecycle.library_clients, ==, 2);

    /* Two client Init calls require two matching Fini calls. With no active
     * Wait or ABI frame the second Fini synchronously releases the unique raw
     * owner; it must not depend on another main-context turn. */
    gnc_GWEN_Fini ();
    lifecycle = get_lifecycle_state ();
    g_assert_cmpuint (lifecycle.library_clients, ==, 1);
    g_assert_cmpuint (lifecycle.raw_fini_calls, ==, 0);
    g_assert_cmpuint (lifecycle.finalize_source_id, ==, 0);
    gnc_GWEN_Fini ();
    lifecycle = get_lifecycle_state ();
    g_assert_cmpuint (lifecycle.library_clients, ==, 0);
    g_assert_cmpuint (lifecycle.fini_wrapper_calls, ==, 2);
    g_assert_cmpuint (lifecycle.raw_fini_calls, ==, 1);
    g_assert_cmpuint (lifecycle.finalize_source_calls, ==, 0);
    g_assert_cmpuint (lifecycle.finalize_destroy_calls, ==, 1);
    g_assert_cmpuint (lifecycle.gui_finalize_calls, ==, 1);
    g_assert_cmpuint (lifecycle.finalize_source_id, ==, 0);
    g_assert_cmpuint (lifecycle.application_barrier_connect_calls, ==, 1);
    g_assert_cmpuint (lifecycle.application_barrier_disconnect_calls, ==, 1);
    g_assert_cmpuint (lifecycle.application_barrier_handler_id, ==, 0);

    g_object_unref (application);
}

int
main (int argc, char **argv)
{
    int status;

    g_setenv ("GNC_UNINSTALLED", "1", TRUE);
    qof_init ();
    qof_log_init_filename_special ("stderr");
    g_test_init (&argc, &argv, NULL);
    gtk_init ();
    gnc_module_system_init ();
    gnc_engine_init (argc, argv);

    g_test_add_func ("/import-export/aqb/gwen-gui/wait-lifecycle",
                     test_gwen_wait_lifecycle);

    status = g_test_run ();
    gnc_engine_shutdown ();
    return status;
}
