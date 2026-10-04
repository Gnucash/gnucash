/* Verify that a deferred table confirmation never acts like acceptance and
 * that its continuation is released exactly once on completion or teardown. */

#include <config.h>
#include <cstdint>
#include <glib.h>
#include <gtest/gtest.h>
#include "test-logging.hpp"

extern "C"
{
#include "table-allgui.h"
}

namespace
{
struct Probe
{
    std::uint32_t handler_calls;
    std::uint32_t replay_calls;
    std::uint32_t destroy_calls;
};

static GncTableConfirmResult
defer_confirmation ([[maybe_unused]] VirtualLocation location,
                    gpointer user_data);

class TableDeferredConfirmTest : public ::testing::Test
{
protected:
    void SetUp () override
    {
        table = gnc_table_new (gnc_table_layout_new (), gnc_table_model_new (),
                               gnc_table_control_new ());
        ASSERT_NE (table, nullptr);
        gnc_table_model_set_default_confirm_handler (table->model,
                                                     defer_confirmation);
        table->model->handler_user_data = &probe;
    }

    void TearDown () override
    {
        if (table)
            gnc_table_destroy (table);
    }

    Probe probe{};
    Table *table{};
};

static GncTableConfirmResult
defer_confirmation ([[maybe_unused]] VirtualLocation location,
                    gpointer user_data)
{
    ++static_cast<Probe *> (user_data)->handler_calls;
    return GNC_TABLE_CONFIRM_DEFERRED;
}

static void
replay_continuation ([[maybe_unused]] Table *table, gpointer user_data)
{
    ++static_cast<Probe *> (user_data)->replay_calls;
}

static void
destroy_continuation (gpointer user_data)
{
    ++static_cast<Probe *> (user_data)->destroy_calls;
}

static GncTableConfirmResult
begin_deferred_change (Table *table, Probe *probe)
{
    auto result = gnc_table_confirm_change (table, table->current_cursor_loc);
    gnc_table_confirm_change_set_replay (table, replay_continuation,
                                         probe,
                                         destroy_continuation);
    return result;
}

TEST_F (TableDeferredConfirmTest, AcceptedChangeReplaysOnce)
{
    auto result = begin_deferred_change (table, &probe);
    ASSERT_EQ (result, GNC_TABLE_CONFIRM_DEFERRED);
    EXPECT_NE (result, GNC_TABLE_CONFIRM_ACCEPT);
    EXPECT_TRUE (table->confirm_pending);
    EXPECT_TRUE (gnc_table_control_input_suspended (table->control));
    EXPECT_EQ (probe.replay_calls, 0u);

    EXPECT_EQ (gnc_table_confirm_change (table, table->current_cursor_loc),
               GNC_TABLE_CONFIRM_REJECT);
    EXPECT_EQ (probe.handler_calls, 1u);
    EXPECT_TRUE (gnc_table_confirm_change_complete (table, true));
    EXPECT_FALSE (table->confirm_pending);
    EXPECT_FALSE (gnc_table_control_input_suspended (table->control));
    EXPECT_EQ (probe.replay_calls, 1u);
    EXPECT_EQ (probe.destroy_calls, 1u);
    EXPECT_FALSE (gnc_table_confirm_change_complete (table, true));
    EXPECT_EQ (probe.replay_calls, 1u);
}

TEST_F (TableDeferredConfirmTest, CancelDiscardsReplay)
{
    auto result = begin_deferred_change (table, &probe);
    ASSERT_EQ (result, GNC_TABLE_CONFIRM_DEFERRED);
    EXPECT_TRUE (table->confirm_pending);

    EXPECT_FALSE (gnc_table_confirm_change_complete (table, false));
    EXPECT_FALSE (table->confirm_pending);
    EXPECT_FALSE (gnc_table_control_input_suspended (table->control));
    EXPECT_EQ (probe.replay_calls, 0u);
    EXPECT_EQ (probe.destroy_calls, 1u);
}

TEST_F (TableDeferredConfirmTest, TeardownDiscardsReplay)
{
    auto result = begin_deferred_change (table, &probe);
    ASSERT_EQ (result, GNC_TABLE_CONFIRM_DEFERRED);

    gnc_table_destroy (table);
    table = nullptr;
    EXPECT_EQ (probe.replay_calls, 0u);
    EXPECT_EQ (probe.destroy_calls, 1u);
}
}

int
main (int argc, char **argv)
{
    ::testing::InitGoogleTest (&argc, argv);
    gnc::test::initialize_logging ();
    return RUN_ALL_TESTS ();
}
