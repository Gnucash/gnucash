/* Verify that a deferred table confirmation never acts like acceptance and
 * that its continuation is released exactly once on completion or teardown. */

#include <config.h>
#include <glib.h>

extern "C"
{
#include "table-allgui.h"
}

namespace
{
struct Probe
{
    guint handler_calls{};
    guint replay_calls{};
    guint destroy_calls{};
};

static GncTableConfirmResult
defer_confirmation (G_GNUC_UNUSED VirtualLocation location, gpointer user_data)
{
    ++static_cast<Probe *> (user_data)->handler_calls;
    return GNC_TABLE_CONFIRM_DEFERRED;
}

static void
replay_continuation (G_GNUC_UNUSED Table *table, gpointer user_data)
{
    ++static_cast<Probe *> (user_data)->replay_calls;
}

static void
destroy_continuation (gpointer user_data)
{
    ++static_cast<Probe *> (user_data)->destroy_calls;
}

static Table *
make_table (Probe *probe)
{
    auto table = gnc_table_new (gnc_table_layout_new (), gnc_table_model_new (),
                                gnc_table_control_new ());
    gnc_table_model_set_default_confirm_handler (table->model,
                                                 defer_confirmation);
    table->model->handler_user_data = probe;
    return table;
}

static void
begin_deferred_change (Table *table, Probe *probe)
{
    auto result = gnc_table_confirm_change (table, table->current_cursor_loc);
    g_assert_cmpint (result, ==, GNC_TABLE_CONFIRM_DEFERRED);
    g_assert_cmpint (result, !=, GNC_TABLE_CONFIRM_ACCEPT);
    g_assert_true (table->confirm_pending);
    g_assert_true (gnc_table_control_input_suspended (table->control));
    g_assert_cmpuint (probe->replay_calls, ==, 0);
    gnc_table_confirm_change_set_replay (table, replay_continuation, probe,
                                         destroy_continuation);
}

static void
test_accept_replays_once (void)
{
    Probe probe;
    auto table = make_table (&probe);
    begin_deferred_change (table, &probe);

    g_assert_cmpint (gnc_table_confirm_change (table, table->current_cursor_loc),
                     ==, GNC_TABLE_CONFIRM_REJECT);
    g_assert_cmpuint (probe.handler_calls, ==, 1);
    g_assert_true (gnc_table_confirm_change_complete (table, TRUE));
    g_assert_false (table->confirm_pending);
    g_assert_false (gnc_table_control_input_suspended (table->control));
    g_assert_cmpuint (probe.replay_calls, ==, 1);
    g_assert_cmpuint (probe.destroy_calls, ==, 1);
    g_assert_false (gnc_table_confirm_change_complete (table, TRUE));
    g_assert_cmpuint (probe.replay_calls, ==, 1);
    gnc_table_destroy (table);
}

static void
test_cancel_discards_replay (void)
{
    Probe probe;
    auto table = make_table (&probe);
    begin_deferred_change (table, &probe);

    g_assert_false (gnc_table_confirm_change_complete (table, FALSE));
    g_assert_false (table->confirm_pending);
    g_assert_false (gnc_table_control_input_suspended (table->control));
    g_assert_cmpuint (probe.replay_calls, ==, 0);
    g_assert_cmpuint (probe.destroy_calls, ==, 1);
    gnc_table_destroy (table);
}

static void
test_table_teardown_discards_replay (void)
{
    Probe probe;
    auto table = make_table (&probe);
    begin_deferred_change (table, &probe);

    gnc_table_destroy (table);
    g_assert_cmpuint (probe.replay_calls, ==, 0);
    g_assert_cmpuint (probe.destroy_calls, ==, 1);
}
}

int
main (int argc, char **argv)
{
    g_test_init (&argc, &argv, nullptr);
    g_test_add_func ("/register/table/deferred-confirm/accept-once",
                     test_accept_replays_once);
    g_test_add_func ("/register/table/deferred-confirm/cancel-discards",
                     test_cancel_discards_replay);
    g_test_add_func ("/register/table/deferred-confirm/teardown-discards",
                     test_table_teardown_discards_replay);
    return g_test_run ();
}
