/* Copyright (C) 2026 GnuCash contributors
 * SPDX-License-Identifier: GPL-2.0-or-later
 */
#include <config.h>
#include <gtk/gtk.h>
#include <gtest/gtest.h>
#include "googletest-glib-log-handler.hpp"
#include <cstdint>
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

struct State
{
    GThread *gtk_thread{};
    std::atomic<int> running{0};
    std::atomic<int> max_running{0};
    std::atomic<int> workers{0};
    std::uint32_t completed{};
    std::uint32_t destroyed{};
    bool callbacks_on_gtk{};
    bool callbacks_on_worker{true};
};

struct Request
{
    State *state;
};

static bool gui_initialized;

struct InputRequest : Request
{
    std::int32_t first_result{G_MININT};
    std::int32_t second_result{G_MININT};
};

struct OperationState
{
    GThread *gtk_thread{};
    std::uint32_t first_token{};
    std::uint32_t second_token{};
    std::uint32_t third_token{};
    std::uint32_t acquisitions{};
    bool completed{};
    std::vector<int> events;
};

static gboolean release_first_operation (gpointer user_data)
{
    auto state = static_cast<OperationState *> (user_data);
    EXPECT_EQ (g_thread_self (), state->gtk_thread);
    state->events.push_back (2);
    gnc_ab_operation_release (state->first_token);
    return G_SOURCE_REMOVE;
}

static gboolean release_second_operation (gpointer user_data)
{
    auto state = static_cast<OperationState *> (user_data);
    EXPECT_EQ (g_thread_self (), state->gtk_thread);
    state->events.push_back (4);
    gnc_ab_operation_release (state->second_token);
    return G_SOURCE_REMOVE;
}

static void operation_acquired (guint token, gpointer user_data)
{
    auto state = static_cast<OperationState *> (user_data);
    EXPECT_EQ (g_thread_self (), state->gtk_thread);
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
    EXPECT_EQ (state->acquisitions, 3u);
    state->third_token = token;
    state->events.push_back (5);
    gnc_ab_operation_release (token);
    state->events.push_back (6);
    state->completed = true;
}

TEST (AqbOperationSlotTest, SerializesLeasesAndIgnoresDuplicateRelease)
{
    OperationState state;
    state.gtk_thread = g_thread_self ();
    gnc_ab_operation_release (0);
    gnc_ab_operation_acquire_async (operation_acquired, &state);
    gnc_ab_operation_acquire_async (operation_acquired, &state);
    gnc_ab_operation_acquire_async (operation_acquired, &state);
    EXPECT_TRUE (state.events.empty ());
    const std::int64_t deadline = g_get_monotonic_time () + 5 * G_TIME_SPAN_SECOND;
    while (!state.completed && g_get_monotonic_time () < deadline)
    {
        while (g_main_context_iteration (nullptr, FALSE))
            ;
        g_usleep (1000);
    }
    EXPECT_TRUE (state.completed);
    EXPECT_EQ (state.acquisitions, 3u);
    EXPECT_NE (state.first_token, 0u);
    EXPECT_NE (state.second_token, 0u);
    EXPECT_NE (state.first_token, state.second_token);
    EXPECT_NE (state.third_token, 0u);
    EXPECT_NE (state.second_token, state.third_token);
    ASSERT_EQ (state.events.size (), 6u);
    EXPECT_EQ (state.events[0], 1);
    EXPECT_EQ (state.events[1], 2);
    EXPECT_EQ (state.events[2], 3);
    EXPECT_EQ (state.events[3], 4);
    EXPECT_EQ (state.events[4], 5);
    EXPECT_EQ (state.events[5], 6);
}

static void worker (GncGWENGui *, gpointer user_data)
{
    auto request = static_cast<Request *> (user_data);
    auto state = request->state;
    state->callbacks_on_worker &= g_thread_self () != state->gtk_thread;
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
    request->state->callbacks_on_worker &=
        g_thread_self () != request->state->gtk_thread;
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

class GwenWorkerResponseTest : public ::testing::Test
{
protected:
    void SetUp () override
    {
        state.gtk_thread = g_thread_self ();
        state.callbacks_on_gtk = true;
        parent = gtk_window_new (GTK_WINDOW_TOPLEVEL);
        g_object_ref_sink (parent);
        gtk_widget_show (parent);
        gtk_widget_realize (parent);
    }

    void TearDown () override
    {
        if (parent)
            gtk_widget_destroy (parent);
        const std::int64_t deadline = g_get_monotonic_time () + 5 * G_TIME_SPAN_SECOND;
        while (state.destroyed < expected_destroyed &&
               g_get_monotonic_time () < deadline)
        {
            while (g_main_context_iteration (nullptr, FALSE))
                ;
            g_usleep (1000);
        }
        if (gui1)
            gnc_GWEN_Gui_release (gui1);
        if (gui2)
            gnc_GWEN_Gui_release (gui2);
        if (parent)
        {
            g_object_unref (parent);
            parent = nullptr;
        }
    }

    State state;
    GtkWidget *parent{};
    GncGWENGui *gui1{};
    GncGWENGui *gui2{};
    InputRequest input_request{};
    Request first_request{};
    Request second_request{};
    std::uint32_t expected_destroyed{};
};

TEST_F (GwenWorkerResponseTest, ParentDestroyCancelsInputAndLaterRequest)
{
    auto& state = this->state;
    gui1 = gnc_GWEN_Gui_get (parent);
    ASSERT_NE (gui1, nullptr);
    gui_initialized = true;
    input_request.state = &state;
    expected_destroyed = 1;
    gnc_GWEN_Gui_run_job_async (gui1, input_worker, completed, &input_request,
                                destroyed);

    const std::int64_t deadline = g_get_monotonic_time () + 5 * G_TIME_SPAN_SECOND;
    GtkWidget *dialog = nullptr;
    while (!dialog && g_get_monotonic_time () < deadline)
    {
        while (g_main_context_iteration (nullptr, FALSE))
            ;
        dialog = find_input_dialog ("Synthetic Gwen input");
        if (!dialog)
            g_usleep (1000);
    }
    ASSERT_NE (dialog, nullptr);
    g_object_ref (dialog);
    gtk_widget_destroy (parent);

    while (state.completed != 1 && g_get_monotonic_time () < deadline)
    {
        while (g_main_context_iteration (nullptr, FALSE))
            ;
        g_usleep (1000);
    }
    EXPECT_EQ (state.completed, 1u);
    EXPECT_EQ (state.destroyed, 1u);
    EXPECT_TRUE (state.callbacks_on_gtk);
    EXPECT_EQ (input_request.first_result, -1);
    EXPECT_EQ (input_request.second_result, -1);
    EXPECT_TRUE (state.callbacks_on_worker);
    EXPECT_EQ (find_input_dialog ("Must not open after parent destroy"), nullptr);
    g_object_unref (dialog);
}

TEST_F (GwenWorkerResponseTest, SerializesWorkersAndCompletesOnceOnGtkThread)
{
    auto& state = this->state;
    gui1 = gnc_GWEN_Gui_get (parent);
    gui2 = gnc_GWEN_Gui_get (parent);
    gui_initialized = gui1 && gui2;
    ASSERT_NE (gui1, nullptr);
    ASSERT_NE (gui2, nullptr);
    EXPECT_NE (gui1, gui2);

    first_request.state = &state;
    second_request.state = &state;
    expected_destroyed = 2;
    gnc_GWEN_Gui_run_job_async (gui1, worker, completed, &first_request, destroyed);
    gnc_GWEN_Gui_run_job_async (gui2, worker, completed, &second_request, destroyed);

    const std::int64_t deadline = g_get_monotonic_time () + 5 * G_TIME_SPAN_SECOND;
    while (state.completed != 2 && g_get_monotonic_time () < deadline)
    {
        while (g_main_context_iteration (nullptr, FALSE))
            ;
        g_usleep (1000);
    }
    EXPECT_EQ (state.completed, 2u);
    EXPECT_EQ (state.destroyed, 2u);
    EXPECT_TRUE (state.callbacks_on_gtk);
    EXPECT_EQ (state.workers.load (), 2);
    EXPECT_EQ (state.max_running.load (), 1);
    EXPECT_TRUE (state.callbacks_on_worker);

    /* A later context turn must not repeat completion or destruction. */
    for (std::uint32_t i = 0; i < 20; ++i)
    {
        while (g_main_context_iteration (nullptr, FALSE))
            ;
        g_usleep (1000);
    }
    EXPECT_EQ (state.completed, 2u);
    EXPECT_EQ (state.destroyed, 2u);
}

int main (int argc, char **argv)
{
    ::testing::InitGoogleTest (&argc, argv);
    if (!gtk_init_check (&argc, &argv))
        g_error ("GTK display is required for Gwen worker response tests");
    qof_init ();
    if (!cashobjects_register ())
        return 1;
    gnc_component_manager_init ();
    gnc_gsettings_load_backend ();
    gnc::test::initialize_logging ();
    int status = RUN_ALL_TESTS ();
    if (gui_initialized)
        gnc_GWEN_Gui_shutdown ();
    gnc_component_manager_shutdown ();
    gnc_gsettings_shutdown ();
    qof_close ();
    return status;
}
