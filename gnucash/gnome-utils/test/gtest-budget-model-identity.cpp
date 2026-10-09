/* Copyright (C) 2026 GnuCash contributors
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#include <config.h>
#include <gtk/gtk.h>

#include <gtest/gtest.h>
#include "googletest-glib-log-handler.hpp"

#include "cashobjects.h"
#include "gnc-budget.h"
#include "gnc-session.h"
#include "gnc-tree-model-budget.h"

namespace
{
class BudgetModelIdentityTest : public ::testing::Test
{
protected:
    static void SetUpTestSuite ()
    {
        qof_init ();
        ASSERT_TRUE (cashobjects_register ());
    }

    static void TearDownTestSuite ()
    {
        qof_close ();
    }

    void SetUp () override
    {
        m_session = qof_session_new (qof_book_new ());
        gnc_set_current_session (m_session);
        m_book = qof_session_get_book (m_session);
        m_budget = gnc_budget_new (m_book);
        m_expected = *gnc_budget_get_guid (m_budget);
        m_model = gnc_tree_model_budget_new (m_book);
        ASSERT_TRUE (gnc_tree_model_budget_get_iter_for_budget (
                         m_model, &m_iter, m_budget));
        ASSERT_EQ (gnc_tree_model_budget_get_budget (m_model, &m_iter),
                   m_budget);
        ASSERT_EQ (gtk_tree_model_get_column_type (GTK_TREE_MODEL (m_model),
                                                   BUDGET_GUID_COLUMN),
                   GNC_TYPE_GUID);
    }

    void TearDown () override
    {
        g_clear_object (&m_model);
        auto current_session = gnc_exchange_current_session (nullptr);
        if (current_session == m_session)
            m_session = nullptr;
        else if (current_session == m_replacement_session)
            m_replacement_session = nullptr;
        if (current_session)
            qof_session_destroy (current_session);
        if (m_replacement_session)
            qof_session_destroy (m_replacement_session);
        if (m_session)
            qof_session_destroy (m_session);
    }

    void expect_snapshot_keeps_identity_but_not_live_budget ()
    {
        GncGUID *stored = nullptr;
        gtk_tree_model_get (GTK_TREE_MODEL (m_model), &m_iter,
                            BUDGET_GUID_COLUMN, &stored, -1);
        ASSERT_NE (stored, nullptr);
        EXPECT_TRUE (guid_equal (stored, &m_expected));
        guid_free (stored);
        EXPECT_EQ (gnc_tree_model_budget_get_budget (m_model, &m_iter),
                   nullptr);
    }

    QofSession *m_session{};
    QofSession *m_replacement_session{};
    QofBook *m_book{};
    GncBudget *m_budget{};
    GtkTreeModel *m_model{};
    GncGUID m_expected{};
    GtkTreeIter m_iter{};
};

TEST_F (BudgetModelIdentityTest, DeletedBudgetRetainsGuidSnapshot)
{
    gnc_budget_destroy (m_budget);
    expect_snapshot_keeps_identity_but_not_live_budget ();
}

TEST_F (BudgetModelIdentityTest, SessionSwitchInvalidatesLiveBudget)
{
    m_replacement_session = qof_session_new (qof_book_new ());
    EXPECT_EQ (gnc_exchange_current_session (m_replacement_session), m_session);
    expect_snapshot_keeps_identity_but_not_live_budget ();
}

TEST_F (BudgetModelIdentityTest, ClearedCurrentSessionInvalidatesLiveBudget)
{
    gnc_clear_current_session ();
    m_session = nullptr; // Clearing the current session destroys it.
    expect_snapshot_keeps_identity_but_not_live_budget ();
}

TEST_F (BudgetModelIdentityTest, ClosedBookInvalidatesLiveBudget)
{
    qof_book_mark_closed (m_book);
    expect_snapshot_keeps_identity_but_not_live_budget ();
}
} // namespace

int
main (int argc, char **argv)
{
    ::testing::InitGoogleTest (&argc, argv);
    gnc::test::initialize_logging ();
    return RUN_ALL_TESTS ();
}
