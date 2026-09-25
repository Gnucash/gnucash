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

int
main (void)
{
    GncSessionLoadExecutor *executor = gnc_session_load_executor_new ();
    const QofSessionLoadExecutor *qof_executor =
        gnc_session_load_executor_get (executor);
    ExecutorCounts counts = { 0, 0 };
    uintptr_t handle = qof_executor->schedule (
        qof_executor->user_data, task_run, task_discard, &counts);

    if (handle == 0 || counts.run_count != 0 || counts.discard_count != 0)
        return 1;

    gnc_session_load_executor_free (executor);
    if (counts.run_count != 0 || counts.discard_count != 1)
        return 2;

    while (g_main_context_iteration (NULL, FALSE))
        ;
    if (counts.run_count != 0 || counts.discard_count != 1)
        return 3;
    return 0;
}
