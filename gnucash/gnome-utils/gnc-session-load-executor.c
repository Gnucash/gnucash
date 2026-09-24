/* Copyright (C) 2026 GnuCash Developers
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 */

#include <config.h>

#include "gnc-session-load-executor.h"

typedef struct
{
    GncSessionLoadExecutor *executor;
    QofSessionLoadTaskFunc run;
    QofSessionLoadTaskFunc discard;
    gpointer task_data;
    uintptr_t handle;
    guint source_id;
    gboolean pending;
} GncSessionLoadTask;

struct _GncSessionLoadExecutor
{
    QofSessionLoadExecutor qof_executor;
    GHashTable *tasks;
    uintptr_t next_handle;
    guint ref_count;
    gboolean owner_released;
};

static void
gnc_session_load_executor_unref (GncSessionLoadExecutor *executor)
{
    g_return_if_fail (executor != NULL);
    g_return_if_fail (executor->ref_count > 0);

    executor->ref_count--;
    if (executor->ref_count > 0)
        return;
    g_hash_table_destroy (executor->tasks);
    g_free (executor);
}

static gboolean
gnc_session_load_task_run (gpointer user_data)
{
    GncSessionLoadTask *task = user_data;
    GncSessionLoadExecutor *executor = task->executor;
    QofSessionLoadTaskFunc run = task->run;
    gpointer task_data = task->task_data;

    task->source_id = 0;
    task->pending = FALSE;
    g_hash_table_steal (executor->tasks, (gpointer)task->handle);
    task->run = NULL;
    task->discard = NULL;
    task->task_data = NULL;
    run (task_data);
    g_free (task);
    gnc_session_load_executor_unref (executor);
    return G_SOURCE_REMOVE;
}

static uintptr_t
gnc_session_load_executor_schedule (gpointer executor_data,
                                    QofSessionLoadTaskFunc run,
                                    QofSessionLoadTaskFunc discard,
                                    gpointer task_data)
{
    GncSessionLoadExecutor *executor = executor_data;
    GncSessionLoadTask *task;

    g_return_val_if_fail (executor != NULL, 0);
    g_return_val_if_fail (!executor->owner_released, 0);
    g_return_val_if_fail (run != NULL, 0);
    g_return_val_if_fail (discard != NULL, 0);
    if (executor->next_handle == 0)
        return 0;

    task = g_new0 (GncSessionLoadTask, 1);
    task->executor = executor;
    task->run = run;
    task->discard = discard;
    task->task_data = task_data;
    task->handle = executor->next_handle;
    if (executor->next_handle == UINTPTR_MAX)
        executor->next_handle = 0;
    else
        executor->next_handle++;
    task->pending = TRUE;
    task->source_id = g_idle_add_full (G_PRIORITY_DEFAULT_IDLE,
                                       gnc_session_load_task_run, task, NULL);
    if (task->source_id == 0)
    {
        g_free (task);
        return 0;
    }

    executor->ref_count++;
    g_hash_table_insert (executor->tasks, (gpointer)task->handle, task);
    return task->handle;
}

static void
gnc_session_load_executor_cancel (gpointer executor_data,
                                  uintptr_t task_handle)
{
    GncSessionLoadExecutor *executor = executor_data;
    GncSessionLoadTask *task;
    QofSessionLoadTaskFunc discard;
    gpointer task_data;

    g_return_if_fail (executor != NULL);
    if (task_handle == 0)
        return;
    task = g_hash_table_lookup (executor->tasks, (gpointer)task_handle);
    if (!task || !task->pending)
        return;

    discard = task->discard;
    task_data = task->task_data;
    task->pending = FALSE;
    g_hash_table_steal (executor->tasks, (gpointer)task_handle);
    task->run = NULL;
    task->discard = NULL;
    task->task_data = NULL;
    if (task->source_id)
    {
        g_source_remove (task->source_id);
        task->source_id = 0;
    }
    discard (task_data);
    g_free (task);
    gnc_session_load_executor_unref (executor);
}

GncSessionLoadExecutor *
gnc_session_load_executor_new (void)
{
    GncSessionLoadExecutor *executor = g_new0 (GncSessionLoadExecutor, 1);

    executor->qof_executor.user_data = executor;
    executor->qof_executor.schedule = gnc_session_load_executor_schedule;
    executor->qof_executor.cancel = gnc_session_load_executor_cancel;
    executor->tasks = g_hash_table_new (g_direct_hash, g_direct_equal);
    executor->next_handle = 1;
    executor->ref_count = 1;
    return executor;
}

const QofSessionLoadExecutor *
gnc_session_load_executor_get (GncSessionLoadExecutor *executor)
{
    g_return_val_if_fail (executor != NULL, NULL);
    return &executor->qof_executor;
}

guint
gnc_session_load_executor_get_task_count (GncSessionLoadExecutor *executor)
{
    g_return_val_if_fail (executor != NULL, 0);
    return g_hash_table_size (executor->tasks);
}

void
gnc_session_load_executor_free (GncSessionLoadExecutor *executor)
{
    if (!executor || executor->owner_released)
        return;
    executor->owner_released = TRUE;

    while (g_hash_table_size (executor->tasks) > 0)
    {
        GHashTableIter iter;
        gpointer key;
        gboolean found;

        g_hash_table_iter_init (&iter, executor->tasks);
        found = g_hash_table_iter_next (&iter, &key, NULL);
        if (!found)
            break;
        gnc_session_load_executor_cancel (executor, (uintptr_t)key);
    }
    gnc_session_load_executor_unref (executor);
}
