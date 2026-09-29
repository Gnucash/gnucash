/* Copyright (C) 2026 GnuCash contributors
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#include <config.h>
#include <gtk/gtk.h>
#include "cashobjects.h"
#include "gnc-budget.h"
#include "gnc-session.h"
#include "gnc-tree-model-budget.h"

static void
test_snapshot_identity (gconstpointer data)
{
    const auto scenario = GPOINTER_TO_INT (data);
    auto session = qof_session_new (qof_book_new ());
    gnc_set_current_session (session);
    auto book = qof_session_get_book (session);
    auto budget = gnc_budget_new (book);
    const auto expected = *gnc_budget_get_guid (budget);
    auto model = gnc_tree_model_budget_new (book);
    GtkTreeIter iter;
    g_assert_true (gnc_tree_model_budget_get_iter_for_budget (model, &iter, budget));
    g_assert_true (gnc_tree_model_budget_get_budget (model, &iter) == budget);
    g_assert_cmpuint (gtk_tree_model_get_column_type (model, BUDGET_GUID_COLUMN),
                      ==, GNC_TYPE_GUID);

    if (scenario == 0)
        gnc_budget_destroy (budget);
    else if (scenario == 1)
        gnc_set_current_session (qof_session_new (qof_book_new ()));
    else if (scenario == 3)
        qof_book_mark_closed (book);
    else
        gnc_clear_current_session ();

    GncGUID *stored = nullptr;
    gtk_tree_model_get (model, &iter, BUDGET_GUID_COLUMN, &stored, -1);
    g_assert_nonnull (stored);
    g_assert_true (guid_equal (stored, &expected));
    guid_free (stored);
    g_assert_null (gnc_tree_model_budget_get_budget (model, &iter));
    g_object_unref (model);
    if (scenario == 1)
    {
        gnc_clear_current_session ();
        qof_session_destroy (session);
    }
    else if (scenario == 0 || scenario == 3)
        gnc_clear_current_session ();
}

int
main (int argc, char **argv)
{
    g_test_init (&argc, &argv, nullptr);
    qof_init ();
    g_assert_true (cashobjects_register ());
    g_test_add_data_func ("/gnome-utils/budget-model/deleted-budget", GINT_TO_POINTER (0),
                          test_snapshot_identity);
    g_test_add_data_func ("/gnome-utils/budget-model/session-switch", GINT_TO_POINTER (1),
                          test_snapshot_identity);
    g_test_add_data_func ("/gnome-utils/budget-model/closed-book", GINT_TO_POINTER (2),
                          test_snapshot_identity);
    g_test_add_data_func ("/gnome-utils/budget-model/marked-closed-book", GINT_TO_POINTER (3),
                          test_snapshot_identity);
    auto status = g_test_run ();
    qof_close ();
    return status;
}
