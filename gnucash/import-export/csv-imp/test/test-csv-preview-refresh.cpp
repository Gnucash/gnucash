/********************************************************************
 * test-csv-preview-refresh.cpp -- CSV preview idle lifecycle tests *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:  +1-617-542-5942         *
 *                                                                  *
 ********************************************************************/

#include "../gnc-csv-preview-refresh.hpp"

#include <gtest/gtest.h>

#include <memory>

namespace
{

void
run_until_idle_barrier ()
{
    bool barrier_reached = false;
    g_idle_add_full (G_PRIORITY_LOW, +[] (gpointer user_data) {
        *static_cast<bool *> (user_data) = true;
        return G_SOURCE_REMOVE;
    }, &barrier_reached, nullptr);
    while (!barrier_reached)
        ASSERT_TRUE (g_main_context_iteration (nullptr, FALSE));
}

struct RefreshState
{
    guint calls {0};
    bool requeue {false};
    CsvPreviewRefreshIdle *idle {nullptr};
};

void
refresh (gpointer user_data)
{
    auto state = static_cast<RefreshState *> (user_data);

    state->calls++;
    if (state->requeue)
    {
        state->requeue = false;
        state->idle->queue ();
    }
}

struct SelfDeletingOwner
{
    explicit SelfDeletingOwner (bool *called) :
        m_called {called},
        m_idle {destroy_from_refresh, this}
    {
    }

    static void destroy_from_refresh (gpointer user_data)
    {
        auto owner = static_cast<SelfDeletingOwner *> (user_data);

        *owner->m_called = true;
        delete owner;
    }

    bool *m_called;
    CsvPreviewRefreshIdle m_idle;
};

TEST (CsvPreviewRefreshIdleTest, runs_a_deferred_refresh)
{
    RefreshState state;
    CsvPreviewRefreshIdle idle {refresh, &state};
    state.idle = &idle;

    idle.queue ();
    EXPECT_EQ (0u, state.calls);

    run_until_idle_barrier ();
    EXPECT_EQ (1u, state.calls);
}

TEST (CsvPreviewRefreshIdleTest, cancel_is_idempotent_and_allows_a_new_request)
{
    RefreshState state;
    CsvPreviewRefreshIdle idle {refresh, &state};
    state.idle = &idle;

    idle.queue ();
    idle.cancel ();
    idle.cancel ();
    idle.queue ();

    run_until_idle_barrier ();

    EXPECT_EQ (1u, state.calls);
}

TEST (CsvPreviewRefreshIdleTest, destruction_before_the_main_loop_prevents_refresh)
{
    RefreshState state;
    {
        auto idle = std::make_unique<CsvPreviewRefreshIdle> (refresh, &state);
        state.idle = idle.get ();
        idle->queue ();
    }

    run_until_idle_barrier ();

    EXPECT_EQ (0u, state.calls);
}

TEST (CsvPreviewRefreshIdleTest, coalesces_requests_and_preserves_a_requeue)
{
    RefreshState state;
    CsvPreviewRefreshIdle idle {refresh, &state};
    state.idle = &idle;

    idle.queue ();
    idle.queue ();
    idle.queue ();
    state.requeue = true;

    run_until_idle_barrier ();
    EXPECT_EQ (2u, state.calls);
}

TEST (CsvPreviewRefreshIdleTest, allows_owner_destruction_from_the_refresh)
{
    bool called = false;
    auto owner = new SelfDeletingOwner {&called};

    owner->m_idle.queue ();
    run_until_idle_barrier ();

    EXPECT_TRUE (called);
}

} // namespace
