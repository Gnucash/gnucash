/* Copyright (C) 2026 GnuCash Developers
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 */

#include <config.h>

#include <glib.h>

#include "gnc-session-load-executor.h"

typedef struct
{
    guint run_count;
    guint discard_count;
} ExecutorCounts;

typedef struct
{
    GncSessionLoadExecutor *executor;
    ExecutorCounts counts;
} ReentrantExecutorCounts;

typedef struct
{
    GncSessionLoadExecutor *executor;
    const QofSessionLoadExecutor *qof_executor;
    guint remaining;
    guint run_count;
    guint discard_count;
    guint max_task_count;
    uintptr_t first_handle;
    uintptr_t last_handle;
} ChainedExecutorState;

static void
task_run (gpointer user_data)
{
    ExecutorCounts *counts = user_data;

    counts->run_count++;
}

static void
task_discard (gpointer user_data)
{
    ExecutorCounts *counts = user_data;

    counts->discard_count++;
}

static void
task_run_and_free_owner (gpointer user_data)
{
    ReentrantExecutorCounts *state = user_data;
    GncSessionLoadExecutor *executor = state->executor;

    state->counts.run_count++;
    state->executor = NULL;
    gnc_session_load_executor_free (executor);
}

static void
task_discard_and_free_owner (gpointer user_data)
{
    ReentrantExecutorCounts *state = user_data;
    GncSessionLoadExecutor *executor = state->executor;

    state->counts.discard_count++;
    state->executor = NULL;
    gnc_session_load_executor_free (executor);
}

static void chained_task_run (gpointer user_data);

static void
chained_task_discard (gpointer user_data)
{
    ChainedExecutorState *state = user_data;

    state->discard_count++;
}

static void
schedule_chained_task (ChainedExecutorState *state)
{
    uintptr_t handle = state->qof_executor->schedule (
        state->qof_executor->user_data, chained_task_run,
        chained_task_discard, state);

    g_assert_cmpuint (handle, !=, 0);
    g_assert_cmpuint (handle, >, state->last_handle);
    if (state->first_handle == 0)
        state->first_handle = handle;
    state->last_handle = handle;
    state->max_task_count = MAX (
        state->max_task_count,
        gnc_session_load_executor_get_task_count (state->executor));
}

static void
chained_task_run (gpointer user_data)
{
    ChainedExecutorState *state = user_data;

    g_assert_cmpuint (
        gnc_session_load_executor_get_task_count (state->executor), ==, 0);
    state->run_count++;
    state->remaining--;
    if (state->remaining > 0)
        schedule_chained_task (state);
}

static void
test_executor_runs_asynchronously (void)
{
    GncSessionLoadExecutor *executor = gnc_session_load_executor_new ();
    const QofSessionLoadExecutor *qof_executor =
        gnc_session_load_executor_get (executor);
    ExecutorCounts counts = { 0, 0 };
    uintptr_t handle = qof_executor->schedule (
        qof_executor->user_data, task_run, task_discard, &counts);

    g_assert_cmpuint (handle, !=, 0);
    g_assert_cmpuint (counts.run_count, ==, 0);
    g_assert_cmpuint (counts.discard_count, ==, 0);
    g_main_context_iteration (NULL, TRUE);
    g_assert_cmpuint (counts.run_count, ==, 1);
    g_assert_cmpuint (counts.discard_count, ==, 0);

    qof_executor->cancel (qof_executor->user_data, handle);
    g_assert_cmpuint (counts.run_count, ==, 1);
    g_assert_cmpuint (counts.discard_count, ==, 0);
    gnc_session_load_executor_free (executor);
}

static void
test_executor_cancels_pending_task (void)
{
    GncSessionLoadExecutor *executor = gnc_session_load_executor_new ();
    const QofSessionLoadExecutor *qof_executor =
        gnc_session_load_executor_get (executor);
    ExecutorCounts counts = { 0, 0 };
    uintptr_t handle = qof_executor->schedule (
        qof_executor->user_data, task_run, task_discard, &counts);

    g_assert_cmpuint (handle, !=, 0);
    qof_executor->cancel (qof_executor->user_data, handle);
    g_assert_cmpuint (counts.run_count, ==, 0);
    g_assert_cmpuint (counts.discard_count, ==, 1);
    while (g_main_context_iteration (NULL, FALSE))
        ;
    g_assert_cmpuint (counts.run_count, ==, 0);
    g_assert_cmpuint (counts.discard_count, ==, 1);

    qof_executor->cancel (qof_executor->user_data, handle);
    g_assert_cmpuint (counts.discard_count, ==, 1);
    gnc_session_load_executor_free (executor);
}

static void
test_executor_free_discards_pending_task (void)
{
    GncSessionLoadExecutor *executor = gnc_session_load_executor_new ();
    const QofSessionLoadExecutor *qof_executor =
        gnc_session_load_executor_get (executor);
    ExecutorCounts counts = { 0, 0 };

    g_assert_cmpuint (qof_executor->schedule (
                          qof_executor->user_data, task_run, task_discard,
                          &counts),
                      !=, 0);
    gnc_session_load_executor_free (executor);
    g_assert_cmpuint (counts.run_count, ==, 0);
    g_assert_cmpuint (counts.discard_count, ==, 1);
    while (g_main_context_iteration (NULL, FALSE))
        ;
    g_assert_cmpuint (counts.run_count, ==, 0);
    g_assert_cmpuint (counts.discard_count, ==, 1);
}

static void
test_run_callback_frees_owner (void)
{
    ReentrantExecutorCounts state = {
        gnc_session_load_executor_new (), { 0, 0 }
    };
    const QofSessionLoadExecutor *qof_executor =
        gnc_session_load_executor_get (state.executor);

    g_assert_cmpuint (qof_executor->schedule (
                          qof_executor->user_data, task_run_and_free_owner,
                          task_discard, &state),
                      !=, 0);
    g_main_context_iteration (NULL, TRUE);
    g_assert_null (state.executor);
    g_assert_cmpuint (state.counts.run_count, ==, 1);
    g_assert_cmpuint (state.counts.discard_count, ==, 0);
}

static void
test_discard_callback_frees_owner (void)
{
    ReentrantExecutorCounts state = {
        gnc_session_load_executor_new (), { 0, 0 }
    };
    const QofSessionLoadExecutor *qof_executor =
        gnc_session_load_executor_get (state.executor);
    uintptr_t handle = qof_executor->schedule (
        qof_executor->user_data, task_run, task_discard_and_free_owner, &state);

    g_assert_cmpuint (handle, !=, 0);
    qof_executor->cancel (qof_executor->user_data, handle);
    g_assert_null (state.executor);
    g_assert_cmpuint (state.counts.run_count, ==, 0);
    g_assert_cmpuint (state.counts.discard_count, ==, 1);
}

static void
test_executor_does_not_retain_completed_tasks (void)
{
    ChainedExecutorState state = { 0 };

    state.executor = gnc_session_load_executor_new ();
    state.qof_executor = gnc_session_load_executor_get (state.executor);
    state.remaining = 4096;
    schedule_chained_task (&state);

    while (state.run_count < 4096)
        g_main_context_iteration (NULL, TRUE);

    g_assert_cmpuint (state.discard_count, ==, 0);
    g_assert_cmpuint (state.max_task_count, ==, 1);
    g_assert_cmpuint (
        gnc_session_load_executor_get_task_count (state.executor), ==, 0);

    state.qof_executor->cancel (state.qof_executor->user_data,
                                state.first_handle);
    state.qof_executor->cancel (state.qof_executor->user_data,
                                state.last_handle);
    g_assert_cmpuint (state.discard_count, ==, 0);
    gnc_session_load_executor_free (state.executor);
}

int
main (int argc, char **argv)
{
    g_test_init (&argc, &argv, NULL);
    g_test_add_func ("/gnucash/session-load-executor/asynchronous-run",
                     test_executor_runs_asynchronously);
    g_test_add_func ("/gnucash/session-load-executor/cancel-pending",
                     test_executor_cancels_pending_task);
    g_test_add_func ("/gnucash/session-load-executor/free-discards-pending",
                     test_executor_free_discards_pending_task);
    g_test_add_func ("/gnucash/session-load-executor/run-frees-owner",
                     test_run_callback_frees_owner);
    g_test_add_func ("/gnucash/session-load-executor/discard-frees-owner",
                     test_discard_callback_frees_owner);
    g_test_add_func ("/gnucash/session-load-executor/bounded-completed-tasks",
                     test_executor_does_not_retain_completed_tasks);
    return g_test_run ();
}
