/********************************************************************\
 * qof-load-executor.h -- deferred session-load task executor       *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
\********************************************************************/

#ifndef QOF_LOAD_EXECUTOR_H
#define QOF_LOAD_EXECUTOR_H

#include <stdint.h>

#ifdef __cplusplus
extern "C"
{
#endif

/** A deferred task submitted by a session load executor. */
typedef void (*QofSessionLoadTaskFunc) (void *task_data);

/**
 * Schedule @a task_data for later execution.
 *
 * This function must not invoke @a run or @a discard before returning. A zero
 * return rejects the task and leaves its ownership with the caller. A nonzero
 * return transfers exactly one task ownership reference to the executor, which
 * must later invoke exactly one of @a run or @a discard on the session-owning
 * thread. Handles must not be reused during the executor's lifetime.
 */
typedef uintptr_t (*QofSessionLoadExecutorSchedule) (
    void *executor_data, QofSessionLoadTaskFunc run,
    QofSessionLoadTaskFunc discard, void *task_data);

/**
 * Cancel a scheduled task. A still-pending task must invoke its discard
 * function synchronously before this function returns. An already running or
 * terminal task makes this a no-op and must not produce a second callback.
 */
typedef void (*QofSessionLoadExecutorCancel) (
    void *executor_data, uintptr_t task_handle);

/**
 * Caller-owned executor for deferred session-load work.
 *
 * The executor is copied, but its @c user_data context must remain valid until
 * the load terminalizes. All executor calls and task callbacks run on the
 * session-owning thread. A load may schedule multiple sequential tasks and may
 * schedule its successor before the current run callback returns.
 */
typedef struct
{
    void *user_data;
    QofSessionLoadExecutorSchedule schedule;
    QofSessionLoadExecutorCancel cancel;
} QofSessionLoadExecutor;

#ifdef __cplusplus
}
#endif

#endif /* QOF_LOAD_EXECUTOR_H */
