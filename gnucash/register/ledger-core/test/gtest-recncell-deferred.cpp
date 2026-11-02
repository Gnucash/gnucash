/* Verify that a deferred confirmation advances a reconcile cell only once
 * SPDX-License-Identifier: GPL-2.0-or-later
 * after an explicit accepted completion. */

#include <config.h>
#include <gtest/gtest.h>
#include "googletest-glib-log-handler.hpp"

#include "recncell.h"

static RecnCellConfirmResult
defer_confirmation ([[maybe_unused]] char old_flag,
                    [[maybe_unused]] gpointer user_data)
{
    return GNC_RECN_CELL_CONFIRM_DEFERRED;
}

class ReconcileCellTest : public ::testing::Test
{
protected:
    void SetUp () override
    {
        cell = reinterpret_cast<RecnCell *> (gnc_recn_cell_new ());
        ASSERT_NE (cell, nullptr);
        gnc_recn_cell_set_valid_flags (cell, "ncy", 'n');
        gnc_recn_cell_set_flag_order (cell, "ncy");
        gnc_recn_cell_set_flag (cell, 'y');
        gnc_recn_cell_set_confirm_cb (cell, defer_confirmation, nullptr);
    }

    void TearDown () override
    {
        if (cell)
            gnc_basic_cell_destroy (&cell->cell);
    }

    RecnCell *cell;
};

TEST_F (ReconcileCellTest, AcceptedConfirmationAdvancesOnce)
{
    int cursor = 0;
    int start = 0;
    int end = 0;
    ASSERT_TRUE (cell->cell.enter_cell (&cell->cell, &cursor, &start, &end));
    EXPECT_EQ (gnc_recn_cell_get_flag (cell), 'y');
    EXPECT_TRUE (cell->confirm_pending);
    EXPECT_TRUE (gnc_recn_cell_complete_confirm (cell, TRUE));
    EXPECT_EQ (gnc_recn_cell_get_flag (cell), 'n');
    EXPECT_FALSE (cell->confirm_pending);
    EXPECT_FALSE (gnc_recn_cell_complete_confirm (cell, TRUE));

}

TEST_F (ReconcileCellTest, CancelledConfirmationKeepsFlag)
{
    int cursor = 0;
    int start = 0;
    int end = 0;
    gnc_recn_cell_set_flag (cell, 'n');
    ASSERT_TRUE (cell->cell.enter_cell (&cell->cell, &cursor, &start, &end));
    EXPECT_TRUE (cell->confirm_pending);
    EXPECT_FALSE (gnc_recn_cell_complete_confirm (cell, FALSE));
    EXPECT_EQ (gnc_recn_cell_get_flag (cell), 'n');
}

int
main (int argc, char **argv)
{
    ::testing::InitGoogleTest (&argc, argv);
    gnc::test::initialize_logging ();
    return RUN_ALL_TESTS ();
}
