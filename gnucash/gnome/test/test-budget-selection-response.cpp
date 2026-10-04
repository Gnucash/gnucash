/* Copyright (C) 2026 GnuCash contributors
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#include <config.h>
#include <cstdint>
#include <gtk/gtk.h>
#include <libguile.h>
#include <gtest/gtest.h>
#include "test-logging.hpp"
#include <string>
#include "test/gnome-response-test-fixture.h"
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

namespace
{
struct Result
{
    std::uint32_t calls{};
    GncBudget *budget{};
};

QofSession *window_sentinel_session{};
GncMainWindow *window_sentinel{};

static void
create_window_sentinel ()
{
    window_sentinel_session = qof_session_new (qof_book_new ());
    gnc_set_current_session (window_sentinel_session);
    window_sentinel = gnc_main_window_new ();
    g_object_ref_sink (window_sentinel);
    gnc_exchange_current_session (nullptr);
}

static void
destroy_window_sentinel ()
{
    if (window_sentinel)
    {
        gtk_widget_destroy (GTK_WIDGET (window_sentinel));
        g_object_unref (window_sentinel);
        window_sentinel = nullptr;
    }
    if (window_sentinel_session)
    {
        qof_session_destroy (window_sentinel_session);
        window_sentinel_session = nullptr;
    }
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

class BudgetSelectionResponseTest : public GnomeResponseTest
{
protected:
    void SetUp () override
    {
        GnomeResponseTest::SetUp ();
        session = qof_session_new (qof_book_new ());
        gnc_set_current_session (session);
        book = qof_session_get_book (session);
        budget = gnc_budget_new (book);
        parent = GTK_WINDOW (gtk_window_new (GTK_WINDOW_TOPLEVEL));
        g_object_ref_sink (parent);
    }

    void TearDown () override
    {
        if (parent)
        {
            gtk_widget_destroy (GTK_WIDGET (parent));
            g_object_unref (parent);
        }
        if (dialog)
            g_object_unref (dialog);
        GnomeResponseTest::TearDown ();
        gnc_clear_current_session ();
        if (replacement_session)
            qof_session_destroy (session);
    }

    QofSession *session{};
    QofSession *replacement_session{};
    QofBook *book{};
    GncBudget *budget{};
    GtkWindow *parent{};
    GtkDialog *dialog{};
    Result result{};
};

static GtkDialog *
find_budget_selection_dialog (GtkWindow *parent)
{
    auto windows = gtk_window_list_toplevels ();
    GtkDialog *dialog = nullptr;
    for (auto node = windows; node; node = node->next)
        if (GTK_IS_DIALOG (node->data) &&
            gtk_window_get_transient_for (GTK_WINDOW (node->data)) == parent)
        {
            EXPECT_EQ (dialog, nullptr);
            if (dialog)
            {
                g_list_free (windows);
                return nullptr;
            }
            dialog = GTK_DIALOG (node->data);
    }
    g_list_free (windows);
    if (dialog)
        g_object_ref (dialog);
    return dialog;
}

TEST_F (BudgetSelectionResponseTest, AcceptReturnsSelectedBudget)
{
    gnc_budget_gui_select_budget_async (parent, book, selected, &result);
    EXPECT_EQ (result.calls, 0u);
    dialog = find_budget_selection_dialog (parent);
    ASSERT_NE (dialog, nullptr);
    EXPECT_EQ (result.calls, 0u);
    auto view = GTK_TREE_VIEW (find_tree (GTK_WIDGET (dialog)));
    ASSERT_TRUE (GTK_IS_TREE_VIEW (view));
    GtkTreeIter iter;
    ASSERT_TRUE (gnc_tree_model_budget_get_iter_for_budget (
        gtk_tree_view_get_model (view), &iter, budget));
    gtk_tree_selection_select_iter (gtk_tree_view_get_selection (view), &iter);
    gtk_dialog_response (dialog, GTK_RESPONSE_OK);
    EXPECT_EQ (result.calls, 1u);
    EXPECT_EQ (result.budget, budget);
    gtk_dialog_response (dialog, GTK_RESPONSE_OK);
    EXPECT_EQ (result.calls, 1u);
}

TEST_F (BudgetSelectionResponseTest, CancelReturnsNoBudget)
{
    gnc_budget_gui_select_budget_async (parent, book, selected, &result);
    EXPECT_EQ (result.calls, 0u);
    dialog = find_budget_selection_dialog (parent);
    ASSERT_NE (dialog, nullptr);
    auto view = GTK_TREE_VIEW (find_tree (GTK_WIDGET (dialog)));
    ASSERT_TRUE (GTK_IS_TREE_VIEW (view));
    GtkTreeIter iter;
    ASSERT_TRUE (gnc_tree_model_budget_get_iter_for_budget (
        gtk_tree_view_get_model (view), &iter, budget));
    gtk_tree_selection_select_iter (gtk_tree_view_get_selection (view), &iter);
    gtk_dialog_response (dialog, GTK_RESPONSE_CANCEL);
    EXPECT_EQ (result.calls, 1u);
    EXPECT_EQ (result.budget, nullptr);
    gtk_dialog_response (dialog, GTK_RESPONSE_OK);
    EXPECT_EQ (result.calls, 1u);
}

TEST_F (BudgetSelectionResponseTest, ParentDestructionReturnsNoBudget)
{
    gnc_budget_gui_select_budget_async (parent, book, selected, &result);
    EXPECT_EQ (result.calls, 0u);
    dialog = find_budget_selection_dialog (parent);
    ASSERT_NE (dialog, nullptr);
    auto view = GTK_TREE_VIEW (find_tree (GTK_WIDGET (dialog)));
    ASSERT_TRUE (GTK_IS_TREE_VIEW (view));
    GtkTreeIter iter;
    ASSERT_TRUE (gnc_tree_model_budget_get_iter_for_budget (
        gtk_tree_view_get_model (view), &iter, budget));
    gtk_tree_selection_select_iter (gtk_tree_view_get_selection (view), &iter);
    gtk_widget_destroy (GTK_WIDGET (parent));
    gtk_dialog_response (dialog, GTK_RESPONSE_OK);
    EXPECT_EQ (result.calls, 1u);
    EXPECT_EQ (result.budget, nullptr);
    gtk_dialog_response (dialog, GTK_RESPONSE_OK);
    EXPECT_EQ (result.calls, 1u);
}

TEST_F (BudgetSelectionResponseTest, SessionSwitchReturnsNoBudget)
{
    gnc_budget_gui_select_budget_async (parent, book, selected, &result);
    EXPECT_EQ (result.calls, 0u);
    dialog = find_budget_selection_dialog (parent);
    ASSERT_NE (dialog, nullptr);
    auto view = GTK_TREE_VIEW (find_tree (GTK_WIDGET (dialog)));
    ASSERT_TRUE (GTK_IS_TREE_VIEW (view));
    GtkTreeIter iter;
    ASSERT_TRUE (gnc_tree_model_budget_get_iter_for_budget (
        gtk_tree_view_get_model (view), &iter, budget));
    gtk_tree_selection_select_iter (gtk_tree_view_get_selection (view), &iter);
    replacement_session = qof_session_new (qof_book_new ());
    gnc_set_current_session (replacement_session);
    gtk_dialog_response (dialog, GTK_RESPONSE_OK);
    EXPECT_EQ (result.calls, 1u);
    EXPECT_EQ (result.budget, nullptr);
    gtk_dialog_response (dialog, GTK_RESPONSE_OK);
    EXPECT_EQ (result.calls, 1u);
}

TEST_F (BudgetSelectionResponseTest, DeletedBudgetReturnsNoBudget)
{
    gnc_budget_gui_select_budget_async (parent, book, selected, &result);
    EXPECT_EQ (result.calls, 0u);
    dialog = find_budget_selection_dialog (parent);
    ASSERT_NE (dialog, nullptr);
    auto view = GTK_TREE_VIEW (find_tree (GTK_WIDGET (dialog)));
    ASSERT_TRUE (GTK_IS_TREE_VIEW (view));
    GtkTreeIter iter;
    ASSERT_TRUE (gnc_tree_model_budget_get_iter_for_budget (
        gtk_tree_view_get_model (view), &iter, budget));
    gtk_tree_selection_select_iter (gtk_tree_view_get_selection (view), &iter);
    gnc_budget_destroy (budget);
    gtk_dialog_response (dialog, GTK_RESPONSE_OK);
    EXPECT_EQ (result.calls, 1u);
    EXPECT_EQ (result.budget, nullptr);
    gtk_dialog_response (dialog, GTK_RESPONSE_OK);
    EXPECT_EQ (result.calls, 1u);
}

TEST_F (BudgetSelectionResponseTest, ClosedBookReturnsNoBudget)
{
    gnc_budget_gui_select_budget_async (parent, book, selected, &result);
    EXPECT_EQ (result.calls, 0u);
    dialog = find_budget_selection_dialog (parent);
    ASSERT_NE (dialog, nullptr);
    auto view = GTK_TREE_VIEW (find_tree (GTK_WIDGET (dialog)));
    ASSERT_TRUE (GTK_IS_TREE_VIEW (view));
    GtkTreeIter iter;
    ASSERT_TRUE (gnc_tree_model_budget_get_iter_for_budget (
        gtk_tree_view_get_model (view), &iter, budget));
    gtk_tree_selection_select_iter (gtk_tree_view_get_selection (view), &iter);
    qof_book_mark_closed (book);
    gtk_dialog_response (dialog, GTK_RESPONSE_OK);
    EXPECT_EQ (result.calls, 1u);
    EXPECT_EQ (result.budget, nullptr);
    gtk_dialog_response (dialog, GTK_RESPONSE_OK);
    EXPECT_EQ (result.calls, 1u);
}

struct BudgetModifyClose
{
    GtkWidget *window{};
    GncBudget *budget{};
    std::uint32_t calls{};
};

static void
close_window_on_budget_modify (QofInstance *instance, QofEventId event,
                               gpointer data, [[maybe_unused]] gpointer event_data)
{
    auto state = static_cast<BudgetModifyClose *> (data);
    if (instance != QOF_INSTANCE (state->budget) || event != QOF_EVENT_MODIFY)
        return;
    ++state->calls;
    EXPECT_STREQ (gnc_budget_get_name (state->budget), "Updated budget");
    EXPECT_EQ (gnc_budget_get_num_periods (state->budget), 3u);
    gtk_widget_destroy (state->window);
}

class BudgetPageResponseTest : public GnomeResponseTest
{
protected:
    void SetUp () override
    {
        GnomeResponseTest::SetUp ();
        session = qof_session_new (qof_book_new ());
        gnc_set_current_session (session);
        book = qof_session_get_book (session);
        auto root = gnc_account_create_root (book);
        account = xaccMallocAccount (book);
        other_account = xaccMallocAccount (book);
        xaccAccountSetName (account, "Budget response target");
        xaccAccountSetName (other_account, "Budget response bystander");
        gnc_account_append_child (root, account);
        gnc_account_append_child (root, other_account);
        budget = gnc_budget_new (book);
        gnc_budget_set_name (budget, "Original budget");
        gnc_budget_set_num_periods (budget, 2);
        gnc_budget_set_account_period_value (budget, account, 0,
                                             gnc_numeric_create (10, 1));
        gnc_budget_set_account_period_value (budget, account, 1,
                                             gnc_numeric_create (15, 1));
        gnc_budget_set_account_period_value (budget, other_account, 0,
                                             gnc_numeric_create (40, 1));
        gnc_budget_set_account_period_note (budget, account, 1, "original");
        gnc_budget_set_account_period_note (budget, other_account, 1,
                                           "untouched");

        window = gnc_main_window_new ();
        g_object_ref_sink (window);
        gtk_widget_realize (GTK_WIDGET (window));
        page = gnc_plugin_page_budget_new (budget);
        ASSERT_NE (page, nullptr);
        g_object_ref (page);
        gnc_main_window_open_page (window, page);
    }

    void TearDown () override
    {
        if (event_handler)
            qof_event_unregister_handler (event_handler);
        if (page)
        {
            if (page_installed)
                gnc_main_window_close_page (page);
            g_object_unref (page);
            page = nullptr;
        }
        if (window)
        {
            gtk_widget_destroy (GTK_WIDGET (window));
            g_object_unref (window);
            window = nullptr;
        }
        for (auto widget : retained_widgets)
            g_object_unref (widget);
        GnomeResponseTest::TearDown ();
        gnc_clear_current_session ();
    }

    QofSession *session{};
    QofBook *book{};
    Account *account{};
    Account *other_account{};
    GncBudget *budget{};
    GncMainWindow *window{};
    GncPluginPage *page{};
    bool page_installed{true};
    gulong event_handler{};
    BudgetModifyClose close_state{};
    std::vector<GtkWidget *> retained_widgets;

    void retain_widget (GtkWidget *widget)
    {
        g_object_ref (widget);
        retained_widgets.push_back (widget);
    }

    bool select_account_period_one ()
    {
        auto view = GTK_TREE_VIEW (gnc_budget_view_get_account_tree_view (
            GNC_BUDGET_VIEW (page->notebook_page)));
        if (!GTK_IS_TREE_VIEW (view))
        {
            ADD_FAILURE () << "Budget page has no account tree view";
            return false;
        }
        auto model = GTK_TREE_MODEL (gtk_tree_view_get_model (view));
        auto path = find_account_path (view, model, nullptr, account);
        if (!path)
        {
            ADD_FAILURE () << "Budget target account is missing from the view";
            return false;
        }
        GtkTreeViewColumn *target_column = nullptr;
        auto columns = gtk_tree_view_get_columns (view);
        for (auto node = columns; node; node = node->next)
        {
            auto column = GTK_TREE_VIEW_COLUMN (node->data);
            if (GPOINTER_TO_UINT (g_object_get_data (G_OBJECT (column),
                                                     "period_num")) == 1)
                target_column = column;
        }
        g_list_free (columns);
        if (!target_column)
        {
            ADD_FAILURE () << "Budget period-one column is missing";
            gtk_tree_path_free (path);
            return false;
        }
        gtk_tree_view_expand_all (view);
        gtk_tree_view_set_cursor (view, path, target_column, false);
        gtk_tree_path_free (path);
        return true;
    }
};

struct BudgetNoteScenario
{
    const char *name;
    bool accept;
    bool close_page;
    bool destroy_parent;
};

class BudgetNoteResponseTest : public BudgetPageResponseTest,
                               public ::testing::WithParamInterface<BudgetNoteScenario>
{};

TEST_P (BudgetNoteResponseTest, Response)
{
    ASSERT_TRUE (select_account_period_one ());
    auto action = gnc_plugin_page_get_action (page, "BudgetNoteAction");
    ASSERT_NE (action, nullptr);
    g_action_activate (action, nullptr);
    auto dialog = find_budget_note_dialog (GTK_WINDOW (window));
    ASSERT_NE (dialog, nullptr);
    retain_widget (GTK_WIDGET (dialog));
    auto text_view = GTK_TEXT_VIEW (find_text_view (GTK_WIDGET (dialog)));
    ASSERT_TRUE (GTK_IS_TEXT_VIEW (text_view));

    const auto scenario = GetParam ();
    if (scenario.accept)
    {
        auto buffer = gtk_text_view_get_buffer (text_view);
        gtk_text_buffer_set_text (buffer, "captured note", -1);
    }
    if (scenario.close_page)
    {
        gnc_main_window_close_page (page);
        page_installed = false;
    }
    if (scenario.destroy_parent)
    {
        gtk_widget_destroy (GTK_WIDGET (window));
        page_installed = false;
    }

    gtk_dialog_response (dialog, scenario.accept ? GTK_RESPONSE_OK :
                         GTK_RESPONSE_CANCEL);
    EXPECT_STREQ (gnc_budget_get_account_period_note (budget, account, 1),
                  scenario.accept ? "captured note" : "original");
    EXPECT_STREQ (gnc_budget_get_account_period_note (budget, other_account, 1),
                  "untouched");
    gtk_dialog_response (dialog, GTK_RESPONSE_OK);
    EXPECT_STREQ (gnc_budget_get_account_period_note (budget, account, 1),
                  scenario.accept ? "captured note" : "original");
}

static std::string
note_scenario_name (const ::testing::TestParamInfo<BudgetNoteScenario> &info)
{
    return info.param.name;
}

INSTANTIATE_TEST_SUITE_P (Responses, BudgetNoteResponseTest,
    ::testing::Values (BudgetNoteScenario{"AcceptUpdatesSelectedAccount", true,
                                         false, false},
                       BudgetNoteScenario{"CancelLeavesNotesUnchanged", false,
                                         false, false},
                       BudgetNoteScenario{"PageCloseIgnoresResponse", false,
                                         true, false},
                       BudgetNoteScenario{"ParentDestroyIgnoresResponse", false,
                                         false, true}),
    note_scenario_name);

enum class BudgetMutationKind { Options, AllPeriods, Estimate };

struct BudgetMutationScenario
{
    const char *name;
    BudgetMutationKind kind;
    bool accept;
};

class BudgetMutationResponseTest : public BudgetPageResponseTest,
                                   public ::testing::WithParamInterface<BudgetMutationScenario>
{};

TEST_P (BudgetMutationResponseTest, Response)
{
    ASSERT_TRUE (select_account_period_one ());
    const auto scenario = GetParam ();
    const char *action_name = scenario.kind == BudgetMutationKind::Options ?
        "OptionsBudgetAction" :
        scenario.kind == BudgetMutationKind::Estimate ? "EstimateBudgetAction" :
        "AllPeriodsBudgetAction";
    const char *dialog_name = scenario.kind == BudgetMutationKind::Options ?
        "budget_options_container_dialog" :
        scenario.kind == BudgetMutationKind::Estimate ? "budget_estimate_dialog" :
        "budget_allperiods_dialog";
    auto action = gnc_plugin_page_get_action (page, action_name);
    ASSERT_NE (action, nullptr);
    g_action_activate (action, nullptr);
    auto dialog = find_builder_dialog (GTK_WINDOW (window), dialog_name);
    ASSERT_NE (dialog, nullptr);
    retain_widget (GTK_WIDGET (dialog));

    if (scenario.kind == BudgetMutationKind::Options)
    {
        auto name = GTK_ENTRY (find_builder_widget (GTK_WIDGET (dialog),
                                                    "BudgetName"));
        ASSERT_TRUE (GTK_IS_ENTRY (name));
        gtk_entry_set_text (name, "Updated budget");
    }
    else if (scenario.kind == BudgetMutationKind::Estimate)
    {
        auto average = GTK_TOGGLE_BUTTON (find_builder_widget (
            GTK_WIDGET (dialog), "UseAverage"));
        ASSERT_TRUE (GTK_IS_TOGGLE_BUTTON (average));
        gtk_toggle_button_set_active (average, true);
    }
    else
    {
        auto value = GTK_ENTRY (find_builder_widget (GTK_WIDGET (dialog),
                                                      "Value"));
        auto add = GTK_TOGGLE_BUTTON (find_builder_widget (
            GTK_WIDGET (dialog), "RB_Add"));
        ASSERT_TRUE (GTK_IS_ENTRY (value));
        ASSERT_TRUE (GTK_IS_TOGGLE_BUTTON (add));
        gtk_entry_set_text (value, "7");
        gtk_toggle_button_set_active (add, true);
    }

    gtk_dialog_response (dialog, scenario.accept ? GTK_RESPONSE_OK :
                         GTK_RESPONSE_CANCEL);
    if (scenario.kind == BudgetMutationKind::Options)
        EXPECT_STREQ (gnc_budget_get_name (budget),
                      scenario.accept ? "Updated budget" :
                                        "Original budget");
    else if (scenario.kind == BudgetMutationKind::Estimate)
    {
        EXPECT_EQ (gnc_numeric_compare (
                      gnc_budget_get_account_period_value (budget, account, 0),
                      gnc_numeric_create (scenario.accept ? 0 : 10, 1)), 0);
        EXPECT_EQ (gnc_numeric_compare (
                      gnc_budget_get_account_period_value (budget, account, 1),
                      gnc_numeric_create (scenario.accept ? 0 : 15, 1)), 0);
        EXPECT_EQ (gnc_numeric_compare (
                      gnc_budget_get_account_period_value (budget, other_account, 0),
                      gnc_numeric_create (40, 1)), 0);
    }
    else
    {
        EXPECT_EQ (gnc_numeric_compare (
                      gnc_budget_get_account_period_value (budget, account, 0),
                      gnc_numeric_create (scenario.accept ? 17 : 10, 1)), 0);
        EXPECT_EQ (gnc_numeric_compare (
                      gnc_budget_get_account_period_value (budget, account, 1),
                      gnc_numeric_create (scenario.accept ? 22 : 15, 1)), 0);
        EXPECT_EQ (gnc_numeric_compare (
                      gnc_budget_get_account_period_value (budget, other_account, 0),
                      gnc_numeric_create (40, 1)), 0);
    }

    gtk_dialog_response (dialog, GTK_RESPONSE_OK);
    if (scenario.kind == BudgetMutationKind::Options)
        EXPECT_STREQ (gnc_budget_get_name (budget),
                      scenario.accept ? "Updated budget" :
                                        "Original budget");
    else if (scenario.kind == BudgetMutationKind::Estimate)
        EXPECT_EQ (gnc_numeric_compare (
                      gnc_budget_get_account_period_value (budget, other_account, 0),
                      gnc_numeric_create (40, 1)), 0);
    else
    {
        EXPECT_EQ (gnc_numeric_compare (
                      gnc_budget_get_account_period_value (budget, account, 0),
                      gnc_numeric_create (scenario.accept ? 17 : 10, 1)), 0);
        EXPECT_EQ (gnc_numeric_compare (
                      gnc_budget_get_account_period_value (budget, account, 1),
                      gnc_numeric_create (scenario.accept ? 22 : 15, 1)), 0);
        EXPECT_EQ (gnc_numeric_compare (
                      gnc_budget_get_account_period_value (budget, other_account, 0),
                      gnc_numeric_create (40, 1)), 0);
    }
}

static std::string
mutation_scenario_name (
    const ::testing::TestParamInfo<BudgetMutationScenario> &info)
{
    return info.param.name;
}

INSTANTIATE_TEST_SUITE_P (Responses, BudgetMutationResponseTest,
    ::testing::Values (BudgetMutationScenario{"OptionsAccept",
                         BudgetMutationKind::Options, true},
                       BudgetMutationScenario{"OptionsCancel",
                         BudgetMutationKind::Options, false},
                       BudgetMutationScenario{"AllPeriodsAccept",
                         BudgetMutationKind::AllPeriods, true},
                       BudgetMutationScenario{"AllPeriodsCancel",
                         BudgetMutationKind::AllPeriods, false},
                       BudgetMutationScenario{"EstimateAccept",
                         BudgetMutationKind::Estimate, true},
                       BudgetMutationScenario{"EstimateCancel",
                         BudgetMutationKind::Estimate, false}),
    mutation_scenario_name);

TEST_F (BudgetPageResponseTest, ModifyEventMayCloseOwner)
{
    ASSERT_TRUE (select_account_period_one ());
    auto action = gnc_plugin_page_get_action (page, "OptionsBudgetAction");
    ASSERT_NE (action, nullptr);
    g_action_activate (action, nullptr);
    auto dialog = find_builder_dialog (GTK_WINDOW (window),
                                       "budget_options_container_dialog");
    ASSERT_NE (dialog, nullptr);
    retain_widget (GTK_WIDGET (dialog));
    auto name = GTK_ENTRY (find_builder_widget (GTK_WIDGET (dialog),
                                                "BudgetName"));
    auto periods = GTK_SPIN_BUTTON (find_builder_widget (
        GTK_WIDGET (dialog), "BudgetNumPeriods"));
    ASSERT_TRUE (GTK_IS_ENTRY (name));
    ASSERT_TRUE (GTK_IS_SPIN_BUTTON (periods));
    gtk_entry_set_text (name, "Updated budget");
    gtk_spin_button_set_value (periods, 3);

    close_state = {GTK_WIDGET (window), budget, 0};
    event_handler = qof_event_register_handler (close_window_on_budget_modify,
                                                &close_state);
    gtk_dialog_response (dialog, GTK_RESPONSE_OK);
    qof_event_unregister_handler (event_handler);
    event_handler = 0;
    page_installed = false;
    EXPECT_EQ (close_state.calls, 1u);
    EXPECT_STREQ (gnc_budget_get_name (budget), "Updated budget");
    EXPECT_EQ (gnc_budget_get_num_periods (budget), 3u);
    gtk_dialog_response (dialog, GTK_RESPONSE_OK);
    EXPECT_STREQ (gnc_budget_get_name (budget), "Updated budget");
    EXPECT_EQ (gnc_budget_get_num_periods (budget), 3u);
}
}

static int
run_tests (int argc, char **argv)
{
    g_setenv ("GNC_UNINSTALLED", "YES", true);
    g_setenv ("GSETTINGS_BACKEND", "memory", true);
    ::testing::InitGoogleTest (&argc, argv);
    if (!gtk_init_check (&argc, &argv))
    {
        g_printerr ("GTK display initialization failed; GUI tests require a display.\n");
        return 1;
    }
    qof_init ();
    g_assert_true (cashobjects_register ());
    gnc_component_manager_init ();
    gnc_gsettings_load_backend ();
    create_window_sentinel ();
    gnc::test::initialize_logging ();
    auto status = RUN_ALL_TESTS ();
    destroy_window_sentinel ();
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
