/* Verify that a deferred confirmation advances a reconcile cell only once
 * after an explicit accepted completion. */

#include <config.h>

#include "recncell.h"

static RecnCellConfirmResult
defer_confirmation (G_GNUC_UNUSED char old_flag,
                    G_GNUC_UNUSED gpointer user_data)
{
    return GNC_RECN_CELL_CONFIRM_DEFERRED;
}

static void
test_deferred_confirmation (void)
{
    auto cell = reinterpret_cast<RecnCell *> (gnc_recn_cell_new ());
    int cursor = 0;
    int start = 0;
    int end = 0;

    gnc_recn_cell_set_valid_flags (cell, "ncy", 'n');
    gnc_recn_cell_set_flag_order (cell, "ncy");
    gnc_recn_cell_set_flag (cell, 'y');
    gnc_recn_cell_set_confirm_cb (cell, defer_confirmation, nullptr);

    g_assert_true (cell->cell.enter_cell (&cell->cell, &cursor, &start, &end));
    g_assert_cmpint (gnc_recn_cell_get_flag (cell), ==, 'y');
    g_assert_true (cell->confirm_pending);
    g_assert_true (gnc_recn_cell_complete_confirm (cell, TRUE));
    g_assert_cmpint (gnc_recn_cell_get_flag (cell), ==, 'n');
    g_assert_false (cell->confirm_pending);
    g_assert_false (gnc_recn_cell_complete_confirm (cell, TRUE));

    g_assert_true (cell->cell.enter_cell (&cell->cell, &cursor, &start, &end));
    g_assert_true (cell->confirm_pending);
    g_assert_false (gnc_recn_cell_complete_confirm (cell, FALSE));
    g_assert_cmpint (gnc_recn_cell_get_flag (cell), ==, 'n');

    gnc_basic_cell_destroy (&cell->cell);
}

int
main (int argc, char **argv)
{
    g_test_init (&argc, &argv, nullptr);
    g_test_add_func ("/register/recncell/deferred-confirmation",
                     test_deferred_confirmation);
    return g_test_run ();
}
