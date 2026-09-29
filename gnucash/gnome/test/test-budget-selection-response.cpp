/* Copyright (C) 2026 GnuCash contributors
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#include <config.h>
#include <gtk/gtk.h>
#include <libguile.h>
#include <cstdlib>
#include "cashobjects.h"
#include "gnc-plugin-budget.h"
#include "gnc-plugin-page-budget.h"
#include "gnc-main-window.h"
#include "gnc-budget-view.h"
#include "gnc-tree-view-account.h"
#include "gnc-component-manager.h"
#include "gnc-gsettings.h"
#include "gnc-session.h"
#include "gnc-tree-model-budget.h"
#include "qofevent.h"

struct Result
{
    guint calls{};
    GncBudget *budget{};
};

static gboolean display_available;

static void
assert_budget_value (GncBudget *budget, Account *account, guint period,
                     gint64 expected)
{
    g_assert_cmpint (gnc_numeric_compare (
                         gnc_budget_get_account_period_value (budget, account, period),
                         gnc_numeric_create (expected, 1)), ==, 0);
}

struct BudgetModifyClose
{
    GtkWidget *window;
    GncBudget *budget;
    guint calls{};
};

static void
close_window_on_budget_modify (QofInstance *instance, QofEventId event,
                               gpointer data, [[maybe_unused]] gpointer event_data)
{
    auto state = static_cast<BudgetModifyClose *> (data);
    if (instance != QOF_INSTANCE (state->budget) || event != QOF_EVENT_MODIFY)
        return;
    ++state->calls;
    g_assert_cmpstr (gnc_budget_get_name (state->budget), ==, "Updated budget");
    g_assert_cmpuint (gnc_budget_get_num_periods (state->budget), ==, 3);
    gtk_widget_destroy (state->window);
}

static void
selected ([[maybe_unused]] GtkWindow *parent, GncBudget *budget, gpointer data)
{
    auto result = static_cast<Result *> (data);
    ++result->calls;
    result->budget = budget;
}

static GtkWidget *
find_tree (GtkWidget *widget)
{
    if (GTK_IS_TREE_VIEW (widget))
        return widget;
    if (!GTK_IS_CONTAINER (widget))
        return nullptr;
    auto children = gtk_container_get_children (GTK_CONTAINER (widget));
    GtkWidget *found = nullptr;
    for (auto node = children; node && !found; node = node->next)
        found = find_tree (GTK_WIDGET (node->data));
    g_list_free (children);
    return found;
}

static GtkWidget *
find_text_view (GtkWidget *widget)
{
    if (GTK_IS_TEXT_VIEW (widget))
        return widget;
    if (!GTK_IS_CONTAINER (widget))
        return nullptr;
    auto children = gtk_container_get_children (GTK_CONTAINER (widget));
    GtkWidget *found = nullptr;
    for (auto node = children; node && !found; node = node->next)
        found = find_text_view (GTK_WIDGET (node->data));
    g_list_free (children);
    return found;
}

static GtkDialog *
find_budget_note_dialog (GtkWindow *parent)
{
    auto windows = gtk_window_list_toplevels ();
    GtkDialog *found = nullptr;
    for (auto node = windows; node && !found; node = node->next)
    {
        auto window = GTK_WIDGET (node->data);
        if (GTK_IS_DIALOG (window) &&
            gtk_window_get_transient_for (GTK_WINDOW (window)) == parent &&
            find_text_view (window))
            found = GTK_DIALOG (window);
    }
    g_list_free (windows);
    return found;
}

static GtkTreePath *
find_account_path (GtkTreeView *view, GtkTreeModel *model, GtkTreeIter *parent,
                   Account *account)
{
    GtkTreeIter iter;
    auto valid = parent ? gtk_tree_model_iter_children (model, &iter, parent)
                        : gtk_tree_model_get_iter_first (model, &iter);
    while (valid)
    {
        auto path = gtk_tree_model_get_path (model, &iter);
        if (gnc_tree_view_account_get_account_from_path (
                GNC_TREE_VIEW_ACCOUNT (view), path) == account)
            return path;
        auto nested = find_account_path (view, model, &iter, account);
        if (nested)
        {
            gtk_tree_path_free (path);
            return nested;
        }
        gtk_tree_path_free (path);
        valid = gtk_tree_model_iter_next (model, &iter);
    }
    return nullptr;
}

static GtkWidget *
find_builder_widget (GtkWidget *widget, const gchar *name)
{
    if (GTK_IS_BUILDABLE (widget) &&
        g_strcmp0 (gtk_buildable_get_name (GTK_BUILDABLE (widget)), name) == 0)
        return widget;
    if (!GTK_IS_CONTAINER (widget))
        return nullptr;
    auto children = gtk_container_get_children (GTK_CONTAINER (widget));
    GtkWidget *found = nullptr;
    for (auto node = children; node && !found; node = node->next)
        found = find_builder_widget (GTK_WIDGET (node->data), name);
    g_list_free (children);
    return found;
}

static GtkDialog *
find_builder_dialog (GtkWindow *parent, const gchar *name)
{
    auto windows = gtk_window_list_toplevels ();
    GtkDialog *found = nullptr;
    for (auto node = windows; node && !found; node = node->next)
    {
        auto window = GTK_WIDGET (node->data);
        if (GTK_IS_DIALOG (window) &&
            gtk_window_get_transient_for (GTK_WINDOW (window)) == parent &&
            find_builder_widget (window, name))
            found = GTK_DIALOG (window);
    }
    g_list_free (windows);
    return found;
}

static void
test_budget_note_action (gconstpointer data)
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    auto scenario = GPOINTER_TO_INT (data);
    auto session = qof_session_new (qof_book_new ());
    gnc_set_current_session (session);
    auto book = qof_session_get_book (session);
    auto root = gnc_account_create_root (book);
    auto account = xaccMallocAccount (book);
    auto other_account = xaccMallocAccount (book);
    xaccAccountSetName (account, "Budget note target");
    xaccAccountSetName (other_account, "Unrelated budget account");
    gnc_account_append_child (root, account);
    gnc_account_append_child (root, other_account);
    auto budget = gnc_budget_new (book);
    gnc_budget_set_name (budget, "Budget note response test");
    gnc_budget_set_num_periods (budget, 2);
    gnc_budget_set_account_period_note (budget, account, 1, "original");
    gnc_budget_set_account_period_note (budget, other_account, 1, "untouched");

    auto window = gnc_main_window_new ();
    g_object_ref_sink (window);
    gtk_widget_realize (GTK_WIDGET (window));
    auto page = gnc_plugin_page_budget_new (budget);
    /* open_page transfers the initial page reference to the window; retain a
     * fixture reference so close_page or parent destruction cannot free it. */
    g_object_ref (page);
    gnc_main_window_open_page (window, page);
    gboolean page_installed = TRUE;
    gboolean window_destroyed = FALSE;
    auto view = GTK_TREE_VIEW (gnc_budget_view_get_account_tree_view (
        GNC_BUDGET_VIEW (page->notebook_page)));
    g_assert_nonnull (view);
    auto model = GTK_TREE_MODEL (gtk_tree_view_get_model (view));
    auto path = find_account_path (view, model, nullptr, account);
    g_assert_nonnull (path);
    GtkTreeViewColumn *target_column = nullptr;
    auto columns = gtk_tree_view_get_columns (view);
    for (auto node = columns; node; node = node->next)
    {
        auto column = GTK_TREE_VIEW_COLUMN (node->data);
        if (GPOINTER_TO_UINT (g_object_get_data (G_OBJECT (column), "period_num")) == 1)
            target_column = column;
    }
    g_list_free (columns);
    g_assert_nonnull (target_column);
    gtk_tree_view_expand_all (view);
    gtk_tree_view_set_cursor (view, path, target_column, FALSE);
    gtk_tree_path_free (path);

    auto action = gnc_plugin_page_get_action (page, "BudgetNoteAction");
    g_assert_nonnull (action);
    g_action_activate (action, nullptr);
    auto dialog = find_budget_note_dialog (GTK_WINDOW (window));
    g_assert_nonnull (dialog);
    g_object_ref (dialog);
    auto text_view = GTK_TEXT_VIEW (find_text_view (GTK_WIDGET (dialog)));
    g_assert_nonnull (text_view);
    if (scenario == 0)
    {
        auto buffer = gtk_text_view_get_buffer (text_view);
        gtk_text_buffer_set_text (buffer, "captured note", -1);
    }
    else if (scenario == 2)
    {
        gnc_main_window_close_page (page);
        page_installed = FALSE;
        g_object_unref (page);
        page = nullptr;
    }
    else if (scenario == 3)
    {
        gtk_widget_destroy (GTK_WIDGET (window));
        page_installed = FALSE;
        window_destroyed = TRUE;
    }

    gtk_dialog_response (dialog, scenario == 0 ? GTK_RESPONSE_OK : GTK_RESPONSE_CANCEL);
    g_assert_cmpstr (gnc_budget_get_account_period_note (budget, account, 1), ==,
                     scenario == 0 ? "captured note" : "original");
    g_assert_cmpstr (gnc_budget_get_account_period_note (budget, other_account, 1), ==,
                     "untouched");
    gtk_dialog_response (dialog, GTK_RESPONSE_OK);
    g_assert_cmpstr (gnc_budget_get_account_period_note (budget, account, 1), ==,
                     scenario == 0 ? "captured note" : "original");
    g_object_unref (dialog);

    if (page)
    {
        if (page_installed)
            gnc_main_window_close_page (page);
        g_object_unref (page);
    }
    if (!window_destroyed)
        gtk_widget_destroy (GTK_WIDGET (window));
    g_object_unref (window);
    gnc_clear_current_session ();
}

static void
test_budget_mutation_action (gconstpointer data)
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    auto scenario = GPOINTER_TO_INT (data);
    const gboolean options = scenario < 2 || scenario == 6;
    const gboolean estimate = scenario >= 4;
    auto session = qof_session_new (qof_book_new ());
    gnc_set_current_session (session);
    auto book = qof_session_get_book (session);
    auto root = gnc_account_create_root (book);
    auto account = xaccMallocAccount (book);
    auto other_account = xaccMallocAccount (book);
    xaccAccountSetName (account, "Mutation target");
    xaccAccountSetName (other_account, "Mutation bystander");
    gnc_account_append_child (root, account);
    gnc_account_append_child (root, other_account);
    auto budget = gnc_budget_new (book);
    gnc_budget_set_name (budget, "Original budget");
    gnc_budget_set_num_periods (budget, 2);
    gnc_budget_set_account_period_value (budget, account, 0, gnc_numeric_create (10, 1));
    gnc_budget_set_account_period_value (budget, account, 1, gnc_numeric_create (15, 1));
    gnc_budget_set_account_period_value (budget, other_account, 0, gnc_numeric_create (40, 1));

    auto window = gnc_main_window_new ();
    g_object_ref_sink (window);
    gtk_widget_realize (GTK_WIDGET (window));
    auto page = gnc_plugin_page_budget_new (budget);
    /* open_page transfers the initial page reference to the window; retain a
     * fixture reference across close and reentrant parent destruction. */
    g_object_ref (page);
    gnc_main_window_open_page (window, page);
    gboolean page_installed = TRUE;
    gboolean window_destroyed = FALSE;
    auto view = GTK_TREE_VIEW (gnc_budget_view_get_account_tree_view (
        GNC_BUDGET_VIEW (page->notebook_page)));
    auto model = GTK_TREE_MODEL (gtk_tree_view_get_model (view));
    auto account_path = find_account_path (view, model, nullptr, account);
    g_assert_nonnull (account_path);
    gtk_tree_selection_select_path (gtk_tree_view_get_selection (view), account_path);
    gtk_tree_path_free (account_path);

    auto action_name = options ? "OptionsBudgetAction" :
        estimate ? "EstimateBudgetAction" : "AllPeriodsBudgetAction";
    auto action = gnc_plugin_page_get_action (page, action_name);
    g_assert_nonnull (action);
    g_action_activate (action, nullptr);
    auto dialog_name = options ? "budget_options_container_dialog" :
        estimate ? "budget_estimate_dialog" : "budget_allperiods_dialog";
    auto dialog = find_builder_dialog (GTK_WINDOW (window), dialog_name);
    g_assert_nonnull (dialog);
    g_object_ref (dialog);
    if (options)
    {
        auto name = GTK_ENTRY (find_builder_widget (GTK_WIDGET (dialog), "BudgetName"));
        g_assert_nonnull (name);
        gtk_entry_set_text (name, "Updated budget");
        if (scenario == 6)
        {
            auto periods = GTK_SPIN_BUTTON (
                find_builder_widget (GTK_WIDGET (dialog), "BudgetNumPeriods"));
            g_assert_nonnull (periods);
            gtk_spin_button_set_value (periods, 3);
        }
    }
    else if (estimate)
    {
        auto average = GTK_TOGGLE_BUTTON (
            find_builder_widget (GTK_WIDGET (dialog), "UseAverage"));
        g_assert_nonnull (average);
        gtk_toggle_button_set_active (average, TRUE);
    }
    else
    {
        auto value = GTK_ENTRY (find_builder_widget (GTK_WIDGET (dialog), "Value"));
        auto add = GTK_TOGGLE_BUTTON (
            find_builder_widget (GTK_WIDGET (dialog), "RB_Add"));
        g_assert_nonnull (value);
        g_assert_nonnull (add);
        gtk_entry_set_text (value, "7");
        gtk_toggle_button_set_active (add, TRUE);
    }
    BudgetModifyClose close_state{GTK_WIDGET (window), budget};
    auto handler_id = scenario == 6
        ? qof_event_register_handler (close_window_on_budget_modify, &close_state)
        : 0;
    auto accept = scenario == 0 || scenario == 2 || scenario == 4 || scenario == 6;
    gtk_dialog_response (dialog, accept ? GTK_RESPONSE_OK : GTK_RESPONSE_CANCEL);
    if (handler_id)
    {
        qof_event_unregister_handler (handler_id);
        g_assert_cmpuint (close_state.calls, ==, 1);
        page_installed = FALSE;
        window_destroyed = TRUE;
    }
    if (options)
        g_assert_cmpstr (gnc_budget_get_name (budget), ==,
                         accept ? "Updated budget" : "Original budget");
    else if (estimate)
    {
        assert_budget_value (budget, account, 0, accept ? 0 : 10);
        assert_budget_value (budget, account, 1, accept ? 0 : 15);
        assert_budget_value (budget, other_account, 0, 40);
    }
    else
    {
        assert_budget_value (budget, account, 0, accept ? 17 : 10);
        assert_budget_value (budget, account, 1, accept ? 22 : 15);
        assert_budget_value (budget, other_account, 0, 40);
    }
    gtk_dialog_response (dialog, GTK_RESPONSE_OK);
    if (options)
        g_assert_cmpstr (gnc_budget_get_name (budget), ==,
                         accept ? "Updated budget" : "Original budget");
    else if (estimate)
        assert_budget_value (budget, other_account, 0, 40);
    else
    {
        assert_budget_value (budget, account, 0, accept ? 17 : 10);
        assert_budget_value (budget, account, 1, accept ? 22 : 15);
        assert_budget_value (budget, other_account, 0, 40);
    }
    g_object_unref (dialog);

    if (page_installed)
        gnc_main_window_close_page (page);
    g_object_unref (page);
    if (!window_destroyed)
        gtk_widget_destroy (GTK_WIDGET (window));
    g_object_unref (window);
    gnc_clear_current_session ();
}

static void
test_selection (gconstpointer data)
{
    if (!display_available)
    {
        g_test_skip ("No graphical display is available");
        return;
    }
    auto scenario = GPOINTER_TO_INT (data);
    auto session = qof_session_new (qof_book_new ());
    gnc_set_current_session (session);
    auto book = qof_session_get_book (session);
    auto budget = gnc_budget_new (book);
    auto parent = GTK_WINDOW (gtk_window_new (GTK_WINDOW_TOPLEVEL));
    Result result;
    gnc_budget_gui_select_budget_async (parent, book, selected, &result);
    g_assert_cmpuint (result.calls, ==, 0);
    auto windows = gtk_window_list_toplevels ();
    GtkDialog *dialog = nullptr;
    for (auto node = windows; node; node = node->next)
        if (GTK_IS_DIALOG (node->data) &&
            gtk_window_get_transient_for (GTK_WINDOW (node->data)) == parent)
            dialog = GTK_DIALOG (node->data);
    g_list_free (windows);
    g_assert_nonnull (dialog);
    g_object_ref (dialog);
    auto view = GTK_TREE_VIEW (find_tree (GTK_WIDGET (dialog)));
    g_assert_nonnull (view);
    GtkTreeIter iter;
    g_assert_true (gnc_tree_model_budget_get_iter_for_budget (
        gtk_tree_view_get_model (view), &iter, budget));
    gtk_tree_selection_select_iter (gtk_tree_view_get_selection (view), &iter);

    if (scenario == 2)
        gtk_widget_destroy (GTK_WIDGET (parent));
    else if (scenario == 3)
        gnc_set_current_session (qof_session_new (qof_book_new ()));
    else if (scenario == 4)
        gnc_budget_destroy (budget);
    else if (scenario == 5)
        qof_book_mark_closed (book);

    gtk_dialog_response (dialog, scenario == 1 ? GTK_RESPONSE_CANCEL : GTK_RESPONSE_OK);
    g_assert_cmpuint (result.calls, ==, 1);
    if (scenario == 0)
        g_assert_true (result.budget == budget);
    else
        g_assert_null (result.budget);
    gtk_dialog_response (dialog, GTK_RESPONSE_OK);
    g_assert_cmpuint (result.calls, ==, 1);
    g_object_unref (dialog);
    if (scenario != 2)
        gtk_widget_destroy (GTK_WIDGET (parent));
    gnc_clear_current_session ();
    if (scenario == 3)
        qof_session_destroy (session);
}

static int
run_tests (int argc, char **argv)
{
    g_setenv ("GNC_UNINSTALLED", "YES", TRUE);
    g_setenv ("GSETTINGS_BACKEND", "memory", TRUE);
    g_test_init (&argc, &argv, nullptr);
    display_available = gtk_init_check (&argc, &argv);
    if (g_getenv ("GNC_REQUIRE_DISPLAY"))
        g_assert_true (display_available);
    qof_init ();
    g_assert_true (cashobjects_register ());
    gnc_component_manager_init ();
    gnc_gsettings_load_backend ();
    const char *names[] = {"accept", "cancel", "parent-destroy", "session-switch", "deleted-budget", "closed-book"};
    for (guint i = 0; i < G_N_ELEMENTS (names); ++i)
    {
        auto path = g_strdup_printf ("/gnome/budget-selection/%s", names[i]);
        g_test_add_data_func (path, GINT_TO_POINTER (i), test_selection);
        g_free (path);
    }
    const char *note_names[] = {"ok-original-target", "cancel", "closed-page", "destroyed-parent"};
    for (guint i = 0; i < G_N_ELEMENTS (note_names); ++i)
    {
        auto path = g_strdup_printf ("/gnome/budget-note/%s", note_names[i]);
        g_test_add_data_func (path, GINT_TO_POINTER (i), test_budget_note_action);
        g_free (path);
    }
    const char *mutation_names[] = {"options-ok", "options-cancel",
                                    "all-periods-ok", "all-periods-cancel",
                                    "estimate-ok", "estimate-cancel",
                                    "options-modify-closes-owner"};
    for (guint i = 0; i < G_N_ELEMENTS (mutation_names); ++i)
    {
        auto path = g_strdup_printf ("/gnome/budget-mutation/%s", mutation_names[i]);
        g_test_add_data_func (path, GINT_TO_POINTER (i), test_budget_mutation_action);
        g_free (path);
    }
    auto status = g_test_run ();
    gnc_gsettings_shutdown ();
    gnc_component_manager_shutdown ();
    gnc_clear_current_session ();
    qof_close ();
    return status;
}

static void
guile_main ([[maybe_unused]] void *data, int argc, char **argv)
{
    std::exit (run_tests (argc, argv));
}

int
main (int argc, char **argv)
{
    scm_boot_guile (argc, argv, guile_main, nullptr);
    return 0;
}
