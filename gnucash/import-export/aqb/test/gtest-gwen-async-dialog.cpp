/* Copyright (C) 2026 GnuCash contributors
 * SPDX-License-Identifier: GPL-2.0-or-later
 */
#include <config.h>
#include <gtk/gtk.h>
#include <gtest/gtest.h>
#include "googletest-glib-log-handler.hpp"
#include <cstdint>
#include <cstddef>

#include <gwenhywfar/dialog.h>
#include <gwenhywfar/db.h>
#include <gwenhywfar/gui.h>
#include <gwenhywfar/gui_be.h>

#include "cashobjects.h"
#include "gnc-component-manager.h"
#include "gnc-gsettings.h"
#include "gnc-gwen-gui.h"
#include "qof.h"

static int GWENHYWFAR_CB read_dialog_prefs (GWEN_GUI *, const char *,
                                            const char *, GWEN_DB_NODE **db)
{
    *db = GWEN_DB_Group_new ("preferences");
    return 0;
}

static int GWENHYWFAR_CB write_dialog_prefs (GWEN_GUI *, const char *,
                                             GWEN_DB_NODE *)
{
    return 0;
}

enum class Action { Accept, Reject, ParentDestroy };

struct DialogScenario
{
    Action action;
    bool worker_path;
};

static constexpr DialogScenario scenarios[] = {
    {Action::Accept, false},
    {Action::Reject, false},
    {Action::ParentDestroy, false},
    {Action::Accept, true},
    {Action::ParentDestroy, true}
};

struct DialogRun
{
    GThread *gtk_thread{};
    GtkWidget *parent{};
    GncGWENGui *gui{};
    GWEN_DIALOG *dialog{};
    Action action{};
    bool worker_path{};
    bool worker_thread_ok{};
    std::int32_t worker_result{};
    std::uint32_t calls{};
    bool accepted{};
    bool completed_on_gtk{};
    bool finished{};
    bool started{};
    bool parent_destroyed{};
    bool parent_checked{};
    std::uint32_t driver_source{};
};

static int GWENHYWFAR_CB dialog_signal (GWEN_DIALOG *,
                                       GWEN_DIALOG_EVENTTYPE event,
                                       const char *sender)
{
    if (event != GWEN_DialogEvent_TypeActivated)
        return GWEN_DialogEvent_ResultNotHandled;
    if (g_strcmp0 (sender, "accept") == 0)
        return GWEN_DialogEvent_ResultAccept;
    if (g_strcmp0 (sender, "reject") == 0)
        return GWEN_DialogEvent_ResultReject;
    return GWEN_DialogEvent_ResultNotHandled;
}

static GtkWidget *find_button (GtkWidget *widget, const gchar *label)
{
    if (GTK_IS_BUTTON (widget) &&
        g_strcmp0 (gtk_button_get_label (GTK_BUTTON (widget)), label) == 0)
        return widget;
    if (!GTK_IS_CONTAINER (widget))
        return nullptr;
    auto children = gtk_container_get_children (GTK_CONTAINER (widget));
    GtkWidget *found = nullptr;
    for (auto node = children; node && !found; node = node->next)
        found = find_button (GTK_WIDGET (node->data), label);
    g_list_free (children);
    return found;
}

static GtkWidget *find_async_dialog_window ()
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *found = nullptr;
    for (auto node = windows; node && !found; node = node->next)
    {
        auto window = GTK_WIDGET (node->data);
        if (GTK_IS_WINDOW (window) &&
            find_button (window, "Accept") && find_button (window, "Reject"))
            found = window;
    }
    g_list_free (windows);
    return found;
}

static void dialog_finished (gboolean accepted, gpointer user_data)
{
    auto run = static_cast<DialogRun *> (user_data);
    ++run->calls;
    run->accepted = accepted;
    run->completed_on_gtk = g_thread_self () == run->gtk_thread;
    run->finished = true;
    GWEN_Dialog_free (run->dialog);
    run->dialog = nullptr;
    gnc_GWEN_Gui_release (run->gui);
    run->gui = nullptr;
}

static void worker_exec_dialog (GncGWENGui *, gpointer user_data)
{
    auto run = static_cast<DialogRun *> (user_data);
    run->worker_thread_ok = g_thread_self () != run->gtk_thread;
    run->worker_result = GWEN_Gui_ExecDialog (run->dialog, 0);
}

static void worker_exec_completed (gpointer user_data)
{
    auto run = static_cast<DialogRun *> (user_data);
    ++run->calls;
    run->accepted = run->worker_result == 1;
    run->completed_on_gtk = g_thread_self () == run->gtk_thread;
    run->finished = true;
    GWEN_Dialog_free (run->dialog);
    run->dialog = nullptr;
    gnc_GWEN_Gui_release (run->gui);
    run->gui = nullptr;
}

static gboolean drive_dialog (gpointer user_data)
{
    auto run = static_cast<DialogRun *> (user_data);
    if (run->finished)
        return G_SOURCE_REMOVE;
    auto window = find_async_dialog_window ();
    if (!window)
        return G_SOURCE_CONTINUE;
    if (!run->parent_checked)
    {
        EXPECT_EQ (gtk_window_get_transient_for (GTK_WINDOW (window)),
                   GTK_WINDOW (run->parent));
        run->parent_checked = true;
        gtk_widget_show (run->parent);
    }
    if (run->action == Action::ParentDestroy)
    {
        if (!run->parent_destroyed)
        {
            run->parent_destroyed = true;
            gtk_widget_destroy (run->parent);
        }
        return G_SOURCE_CONTINUE;
    }
    auto button = find_button (window,
        run->action == Action::Accept ? "Accept" : "Reject");
    if (!button)
    {
        ADD_FAILURE () << "Expected Gwen action button was not found";
        return G_SOURCE_CONTINUE;
    }
    gtk_button_clicked (GTK_BUTTON (button));
    return G_SOURCE_CONTINUE;
}

class GwenAsyncDialogTest : public ::testing::TestWithParam<DialogScenario>
{
protected:
    void SetUp () override
    {
        run = {};
        run.gtk_thread = g_thread_self ();
        run.action = GetParam ().action;
        run.worker_path = GetParam ().worker_path;
        run.parent = gtk_window_new (GTK_WINDOW_TOPLEVEL);
        g_object_ref_sink (run.parent);
        gtk_widget_realize (run.parent);
        // Leave the parent unmapped until the dialog opens: its association
        // must not depend on desktop focus or Gwen's active-window heuristic.
        ASSERT_FALSE (gtk_window_is_active (GTK_WINDOW (run.parent)));
        run.gui = gnc_GWEN_Gui_get (run.parent);
        ASSERT_NE (run.gui, nullptr);
        auto gwen_gui = GWEN_Gui_GetGui ();
        ASSERT_NE (gwen_gui, nullptr);
        GWEN_Gui_SetReadDialogPrefsFn (gwen_gui, read_dialog_prefs);
        GWEN_Gui_SetWriteDialogPrefsFn (gwen_gui, write_dialog_prefs);
        run.dialog = GWEN_Dialog_new ("async_test");
        ASSERT_NE (run.dialog, nullptr);
        GWEN_Dialog_SetSignalHandler (run.dialog, dialog_signal);
        const auto source_dir = g_getenv ("SRCDIR");
        ASSERT_NE (source_dir, nullptr);
        auto definition = g_build_filename (source_dir,
                                            "gtest-gwen-async-dialog.dlg", nullptr);
        auto read_result = GWEN_Dialog_ReadXmlFile (run.dialog, definition);
        g_free (definition);
        ASSERT_GE (read_result, 0);
    }

    void TearDown () override
    {
        if (run.started && !run.finished && run.parent)
        {
            gtk_widget_destroy (run.parent);
            const std::int64_t deadline =
                g_get_monotonic_time () + 5 * G_TIME_SPAN_SECOND;
            while (!run.finished && g_get_monotonic_time () < deadline)
            {
                while (g_main_context_iteration (nullptr, FALSE))
                    ;
                g_usleep (1000);
            }
            if (!run.finished)
                g_error ("Gwen dialog operation did not stop before fixture cleanup");
        }
        if (run.driver_source &&
            g_main_context_find_source_by_id (nullptr, run.driver_source))
            g_source_remove (run.driver_source);
        if (run.dialog)
            GWEN_Dialog_free (run.dialog);
        if (run.gui)
            gnc_GWEN_Gui_release (run.gui);
        if (run.parent)
        {
            gtk_widget_destroy (run.parent);
            g_object_unref (run.parent);
        }
    }

    DialogRun run;
};

TEST_P (GwenAsyncDialogTest, CompletesOnGtkThread)
{
    g_test_expect_message ("gwenhywfar", G_LOG_LEVEL_CRITICAL,
                          "*No active window found*");
    run.started = true;
    if (run.worker_path)
        gnc_GWEN_Gui_run_job_async (run.gui, worker_exec_dialog,
                                    worker_exec_completed, &run, nullptr);
    else
        gnc_GWEN_Gui_exec_dialog_async (run.gui, run.dialog,
                                        dialog_finished, &run);
    EXPECT_EQ (run.calls, 0u);
    run.driver_source = g_timeout_add (5, drive_dialog, &run);
    const std::int64_t deadline = g_get_monotonic_time () + 5 * G_TIME_SPAN_SECOND;
    while (!run.finished && g_get_monotonic_time () < deadline)
    {
        while (g_main_context_iteration (nullptr, FALSE))
            ;
        g_usleep (1000);
    }
    ASSERT_TRUE (run.finished);
    g_test_assert_expected_messages ();
    EXPECT_TRUE (run.parent_checked);
    EXPECT_EQ (run.calls, 1u);
    EXPECT_TRUE (run.completed_on_gtk);
    if (run.worker_path)
    {
        EXPECT_TRUE (run.worker_thread_ok);
    }
    EXPECT_EQ (run.accepted, run.action == Action::Accept);
}

INSTANTIATE_TEST_SUITE_P (DialogAndWorkerPaths, GwenAsyncDialogTest,
                          ::testing::ValuesIn (scenarios),
                          [] (const auto& info)
                          {
                              const auto& scenario = info.param;
                              if (scenario.worker_path)
                                  return scenario.action == Action::Accept ?
                                      "WorkerAccept" : "WorkerParentDestroy";
                              switch (scenario.action)
                              {
                              case Action::Accept: return "Accept";
                              case Action::Reject: return "Reject";
                              case Action::ParentDestroy: return "ParentDestroy";
                              }
                              return "Unknown";
                          });

int main (int argc, char **argv)
{
    ::testing::InitGoogleTest (&argc, argv);
    if (!gtk_init_check (&argc, &argv))
        g_error ("GTK display is required for Gwen asynchronous dialog tests");
    qof_init ();
    if (!cashobjects_register ())
        g_error ("Failed to register cash objects for Gwen dialog tests");
    gnc_component_manager_init ();
    gnc_gsettings_load_backend ();
    gnc_GWEN_Gui_log_init ();
    gnc::test::initialize_logging ();
    auto result = RUN_ALL_TESTS ();
    gnc_GWEN_Gui_shutdown ();
    gnc_component_manager_shutdown ();
    gnc_gsettings_shutdown ();
    qof_close ();
    return result;
}
