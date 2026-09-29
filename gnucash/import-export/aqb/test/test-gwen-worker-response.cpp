/* Copyright (C) 2026 GnuCash contributors
 * SPDX-License-Identifier: GPL-2.0-or-later
 */
#include <config.h>
#include <gtk/gtk.h>
#include <atomic>
#include <thread>
#include <vector>

#include "cashobjects.h"
#include "gnc-ab-utils.h"
#include "gnc-component-manager.h"
#include "gnc-gsettings.h"
#include "gnc-gwen-gui.h"
#include "qof.h"
#include <gwenhywfar/gui.h>

static gboolean display_available;

struct State
{
    GThread *gtk_thread{};
    std::atomic<int> running{0};
    std::atomic<int> max_running{0};
    std::atomic<int> workers{0};
    guint completed{};
    guint destroyed{};
    gboolean callbacks_on_gtk{};
};

struct Request
{
    State *state;
};

static gboolean gui_initialized;

struct InputRequest : Request
{
    gint first_result{G_MININT};
    gint second_result{G_MININT};
};

struct OperationState
{
    GThread *gtk_thread{};
    guint first_token{};
    guint second_token{};
    guint third_token{};
    guint acquisitions{};
    gboolean completed{};
    std::vector<int> events;
};

static gboolean release_first_operation (gpointer user_data)
{
    auto state = static_cast<OperationState *> (user_data);
    g_assert_true (g_thread_self () == state->gtk_thread);
    state->events.push_back (2);
    gnc_ab_operation_release (state->first_token);
    return G_SOURCE_REMOVE;
}

static gboolean release_second_operation (gpointer user_data)
{
    auto state = static_cast<OperationState *> (user_data);
    g_assert_true (g_thread_self () == state->gtk_thread);
    state->events.push_back (4);
    gnc_ab_operation_release (state->second_token);
    return G_SOURCE_REMOVE;
}

static void operation_acquired (guint token, gpointer user_data)
{
    auto state = static_cast<OperationState *> (user_data);
    g_assert_true (g_thread_self () == state->gtk_thread);
    ++state->acquisitions;
    if (state->acquisitions == 1)
    {
        state->first_token = token;
        state->events.push_back (1);
        g_timeout_add (40, release_first_operation, state);
        return;
    }
    if (state->acquisitions == 2)
    {
        state->second_token = token;
        state->events.push_back (3);
        /* A duplicate release of the previous token must not release this lease. */
        gnc_ab_operation_release (state->first_token);
        g_timeout_add (40, release_second_operation, state);
        return;
    }
    g_assert_cmpuint (state->acquisitions, ==, 3);
    state->third_token = token;
    state->events.push_back (5);
    gnc_ab_operation_release (token);
    state->events.push_back (6);
    state->completed = TRUE;
}

static void test_operation_slot ()
{
    OperationState state;
    state.gtk_thread = g_thread_self ();
    gnc_ab_operation_release (0);
    gnc_ab_operation_acquire_async (operation_acquired, &state);
    gnc_ab_operation_acquire_async (operation_acquired, &state);
    gnc_ab_operation_acquire_async (operation_acquired, &state);
    g_assert_true (state.events.empty ());
    const gint64 deadline = g_get_monotonic_time () + 5 * G_TIME_SPAN_SECOND;
    while (!state.completed && g_get_monotonic_time () < deadline)
    {
        while (g_main_context_iteration (nullptr, FALSE))
            ;
        g_usleep (1000);
    }
    g_assert_true (state.completed);
    g_assert_cmpuint (state.acquisitions, ==, 3);
    g_assert_cmpuint (state.first_token, !=, 0);
    g_assert_cmpuint (state.second_token, !=, 0);
    g_assert_cmpuint (state.first_token, !=, state.second_token);
    g_assert_cmpuint (state.third_token, !=, 0);
    g_assert_cmpuint (state.second_token, !=, state.third_token);
    g_assert_cmpuint (state.events.size (), ==, 6);
    g_assert_cmpint (state.events[0], ==, 1);
    g_assert_cmpint (state.events[1], ==, 2);
    g_assert_cmpint (state.events[2], ==, 3);
    g_assert_cmpint (state.events[3], ==, 4);
    g_assert_cmpint (state.events[4], ==, 5);
    g_assert_cmpint (state.events[5], ==, 6);
}

static void worker (GncGWENGui *, gpointer user_data)
{
    auto request = static_cast<Request *> (user_data);
    auto state = request->state;
    g_assert_true (g_thread_self () != state->gtk_thread);
    int current = ++state->running;
    int observed = state->max_running.load ();
    while (current > observed &&
           !state->max_running.compare_exchange_weak (observed, current))
        ;
    ++state->workers;
    g_usleep (30000);
    --state->running;
}

static void completed (gpointer user_data)
{
    auto request = static_cast<Request *> (user_data);
    ++request->state->completed;
    request->state->callbacks_on_gtk &=
        g_thread_self () == request->state->gtk_thread;
}

static void destroyed (gpointer user_data)
{
    auto request = static_cast<Request *> (user_data);
    ++request->state->destroyed;
    request->state->callbacks_on_gtk &=
        g_thread_self () == request->state->gtk_thread;
}

static void input_worker (GncGWENGui *, gpointer user_data)
{
    auto request = static_cast<InputRequest *> (user_data);
    char input[32]{};
    g_assert_true (g_thread_self () != request->state->gtk_thread);
    ++request->state->workers;
    request->first_result = GWEN_Gui_InputBox (
        GWEN_GUI_INPUT_FLAGS_SHOW, "Synthetic Gwen input", "Test input",
        input, 1, sizeof input, 0);
    request->second_result = GWEN_Gui_InputBox (
        GWEN_GUI_INPUT_FLAGS_SHOW, "Must not open after parent destroy",
        "Test cancellation", input, 1, sizeof input, 0);
}

static GtkWidget *find_input_dialog (const char *title)
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *found = nullptr;
    for (auto node = windows; node && !found; node = node->next)
        if (GTK_IS_WINDOW (node->data) &&
            g_strcmp0 (gtk_window_get_title (GTK_WINDOW (node->data)), title) == 0)
            found = GTK_WIDGET (node->data);
    g_list_free (windows);
    return found;
}

static void test_input_parent_destroy ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    State state;
    state.gtk_thread = g_thread_self ();
    state.callbacks_on_gtk = TRUE;
    auto parent = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    g_object_ref_sink (parent);
    gtk_widget_show (parent);
    gtk_widget_realize (parent);
    auto gui = gnc_GWEN_Gui_get (parent);
    g_assert_nonnull (gui);
    gui_initialized = TRUE;
    InputRequest request;
    request.state = &state;
    gnc_GWEN_Gui_run_job_async (gui, input_worker, completed, &request,
                                destroyed);

    const gint64 deadline = g_get_monotonic_time () + 5 * G_TIME_SPAN_SECOND;
    GtkWidget *dialog = nullptr;
    while (!dialog && g_get_monotonic_time () < deadline)
    {
        while (g_main_context_iteration (nullptr, FALSE))
            ;
        dialog = find_input_dialog ("Synthetic Gwen input");
        if (!dialog)
            g_usleep (1000);
    }
    g_assert_nonnull (dialog);
    g_object_ref (dialog);
    gtk_widget_destroy (parent);

    while (state.completed != 1 && g_get_monotonic_time () < deadline)
    {
        while (g_main_context_iteration (nullptr, FALSE))
            ;
        g_usleep (1000);
    }
    g_assert_cmpuint (state.completed, ==, 1);
    g_assert_cmpuint (state.destroyed, ==, 1);
    g_assert_true (state.callbacks_on_gtk);
    g_assert_cmpint (request.first_result, ==, -1);
    g_assert_cmpint (request.second_result, ==, -1);
    g_assert_null (find_input_dialog (
        "Must not open after parent destroy"));
    g_object_unref (dialog);
    gnc_GWEN_Gui_release (gui);
    g_object_unref (parent);
}

static void test_serialized_workers ()
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }

    State state;
    state.gtk_thread = g_thread_self ();
    state.callbacks_on_gtk = TRUE;
    auto parent = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    g_object_ref_sink (parent);
    gtk_widget_show (parent);
    gtk_widget_realize (parent);
    auto gui1 = gnc_GWEN_Gui_get (parent);
    auto gui2 = gnc_GWEN_Gui_get (parent);
    gui_initialized = gui1 && gui2;
    g_assert_nonnull (gui1);
    g_assert_nonnull (gui2);
    g_assert_true (gui1 != gui2);

    Request first{&state};
    Request second{&state};
    gnc_GWEN_Gui_run_job_async (gui1, worker, completed, &first, destroyed);
    gnc_GWEN_Gui_run_job_async (gui2, worker, completed, &second, destroyed);

    const gint64 deadline = g_get_monotonic_time () + 5 * G_TIME_SPAN_SECOND;
    while (state.completed != 2 && g_get_monotonic_time () < deadline)
    {
        while (g_main_context_iteration (nullptr, FALSE))
            ;
        g_usleep (1000);
    }
    g_assert_cmpuint (state.completed, ==, 2);
    g_assert_cmpuint (state.destroyed, ==, 2);
    g_assert_true (state.callbacks_on_gtk);
    g_assert_cmpint (state.workers.load (), ==, 2);
    g_assert_cmpint (state.max_running.load (), ==, 1);

    /* A later context turn must not repeat completion or destruction. */
    for (guint i = 0; i < 20; ++i)
    {
        while (g_main_context_iteration (nullptr, FALSE))
            ;
        g_usleep (1000);
    }
    g_assert_cmpuint (state.completed, ==, 2);
    g_assert_cmpuint (state.destroyed, ==, 2);
    gnc_GWEN_Gui_release (gui1);
    gnc_GWEN_Gui_release (gui2);
    gtk_widget_destroy (parent);
    g_object_unref (parent);
}

int main (int argc, char **argv)
{
    g_test_init (&argc, &argv, nullptr);
    display_available = gtk_init_check (&argc, &argv);
    if (g_getenv ("GNC_REQUIRE_DISPLAY"))
        g_assert_true (display_available);
    qof_init ();
    if (!cashobjects_register ())
        return 1;
    gnc_component_manager_init ();
    gnc_gsettings_load_backend ();
    g_test_add_func ("/aqb/operation-slot", test_operation_slot);
    g_test_add_func ("/aqb/gwen/serialized-workers", test_serialized_workers);
    g_test_add_func ("/aqb/gwen/input-parent-destroy", test_input_parent_destroy);
    int status = g_test_run ();
    if (gui_initialized)
        gnc_GWEN_Gui_shutdown ();
    gnc_component_manager_shutdown ();
    gnc_gsettings_shutdown ();
    qof_close ();
    return status;
}
