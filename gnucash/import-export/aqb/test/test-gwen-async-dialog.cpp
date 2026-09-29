/* Copyright (C) 2026 GnuCash contributors
 * SPDX-License-Identifier: GPL-2.0-or-later
 */
#include <config.h>
#include <gtk/gtk.h>

#include <gwenhywfar/dialog.h>
#include <gwenhywfar/db.h>
#include <gwenhywfar/gui.h>
#include <gwenhywfar/gui_be.h>

#include "cashobjects.h"
#include "gnc-component-manager.h"
#include "gnc-gsettings.h"
#include "gnc-gwen-gui.h"
#include "qof.h"

static gboolean display_available;

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

struct DialogRun
{
    GThread *gtk_thread{};
    GtkWidget *parent{};
    GncGWENGui *gui{};
    GWEN_DIALOG *dialog{};
    Action action{};
    gboolean worker_path{};
    gboolean worker_thread_ok{};
    gint worker_result{};
    guint calls{};
    gboolean accepted{};
    gboolean completed_on_gtk{};
    gboolean finished{};
    gboolean parent_destroyed{};
    guint driver_source{};
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
    run->finished = TRUE;
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
    run->finished = TRUE;
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
    if (run->action == Action::ParentDestroy)
    {
        if (!run->parent_destroyed)
        {
            run->parent_destroyed = TRUE;
            gtk_widget_destroy (run->parent);
        }
        return G_SOURCE_CONTINUE;
    }
    auto button = find_button (window,
        run->action == Action::Accept ? "Accept" : "Reject");
    g_assert_nonnull (button);
    gtk_button_clicked (GTK_BUTTON (button));
    return G_SOURCE_CONTINUE;
}

static void test_async_dialog (gconstpointer test_data)
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    DialogRun run;
    run.gtk_thread = g_thread_self ();
    const auto scenario = GPOINTER_TO_INT (test_data);
    run.worker_path = scenario >= 3;
    run.action = scenario == 0 || scenario == 3 ? Action::Accept :
        scenario == 1 ? Action::Reject : Action::ParentDestroy;
    run.parent = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    g_object_ref_sink (run.parent);
    gtk_widget_show (run.parent);
    gtk_widget_realize (run.parent);
    gtk_window_present (GTK_WINDOW (run.parent));
    const gint64 activation_deadline =
        g_get_monotonic_time () + 2 * G_TIME_SPAN_SECOND;
    while (!gtk_window_is_active (GTK_WINDOW (run.parent)) &&
           g_get_monotonic_time () < activation_deadline)
    {
        while (g_main_context_iteration (nullptr, FALSE))
            ;
        g_usleep (1000);
    }
    g_assert_true (gtk_window_is_active (GTK_WINDOW (run.parent)));
    run.gui = gnc_GWEN_Gui_get (run.parent);
    g_assert_nonnull (run.gui);
    auto gwen_gui = GWEN_Gui_GetGui ();
    g_assert_nonnull (gwen_gui);
    GWEN_Gui_SetReadDialogPrefsFn (gwen_gui, read_dialog_prefs);
    GWEN_Gui_SetWriteDialogPrefsFn (gwen_gui, write_dialog_prefs);
    run.dialog = GWEN_Dialog_new ("async_test");
    g_assert_nonnull (run.dialog);
    GWEN_Dialog_SetSignalHandler (run.dialog, dialog_signal);
    const auto source_dir = g_getenv ("SRCDIR");
    g_assert_nonnull (source_dir);
    auto definition = g_build_filename (source_dir,
                                        "test-gwen-async-dialog.dlg", nullptr);
    g_assert_cmpint (GWEN_Dialog_ReadXmlFile (run.dialog, definition), >=, 0);
    g_free (definition);

    if (run.worker_path)
        gnc_GWEN_Gui_run_job_async (run.gui, worker_exec_dialog,
                                    worker_exec_completed, &run, nullptr);
    else
        gnc_GWEN_Gui_exec_dialog_async (run.gui, run.dialog,
                                        dialog_finished, &run);
    g_assert_cmpuint (run.calls, ==, 0);
    run.driver_source = g_timeout_add (5, drive_dialog, &run);
    const gint64 deadline = g_get_monotonic_time () + 5 * G_TIME_SPAN_SECOND;
    while (!run.finished && g_get_monotonic_time () < deadline)
    {
        while (g_main_context_iteration (nullptr, FALSE))
            ;
        g_usleep (1000);
    }
    g_assert_true (run.finished);
    g_assert_cmpuint (run.calls, ==, 1);
    g_assert_true (run.completed_on_gtk);
    if (run.worker_path)
        g_assert_true (run.worker_thread_ok);
    g_assert_cmpint (run.accepted, ==, run.action == Action::Accept);
    if (run.driver_source)
        g_source_remove (run.driver_source);
    gtk_widget_destroy (run.parent);
    g_object_unref (run.parent);
}

int main (int argc, char **argv)
{
    g_test_init (&argc, &argv, nullptr);
    display_available = gtk_init_check (&argc, &argv);
    if (g_getenv ("GNC_REQUIRE_DISPLAY"))
        g_assert_true (display_available);
    qof_init ();
    g_assert_true (cashobjects_register ());
    gnc_component_manager_init ();
    gnc_gsettings_load_backend ();
    const char *names[] = {"accept", "reject", "parent-destroy",
                           "worker-accept", "worker-parent-destroy"};
    for (guint i = 0; i < G_N_ELEMENTS (names); ++i)
        g_test_add_data_func (g_strdup_printf (
            "/import-export/aqb/gwen-async-dialog/%s", names[i]),
            GINT_TO_POINTER (i), test_async_dialog);
    auto result = g_test_run ();
    gnc_GWEN_Gui_shutdown ();
    return result;
}
