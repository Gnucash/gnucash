/* Copyright (C) 2026 GnuCash Developers
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 */

#ifndef GNC_SESSION_LOAD_EXECUTOR_H
#define GNC_SESSION_LOAD_EXECUTOR_H

#include <glib.h>

#include "qof-load-executor.h"

G_BEGIN_DECLS

typedef struct _GncSessionLoadExecutor GncSessionLoadExecutor;

/** Create a per-load executor that dispatches work on the default GLib main
 * context. Scheduled work never runs before schedule() returns. Handles are
 * monotonically increasing and remain safe to cancel after their task has
 * completed; completed tasks are not retained by the executor.
 */
GncSessionLoadExecutor *gnc_session_load_executor_new (void);

/** Borrow the engine-facing executor. It remains valid until @executor is
 * freed after the load's terminal callback.
 */
const QofSessionLoadExecutor *gnc_session_load_executor_get (
    GncSessionLoadExecutor *executor);

/** Return the number of tasks that are still pending. This internal helper is
 * primarily useful for asserting the executor's bounded-lifetime invariant.
 */
guint gnc_session_load_executor_get_task_count (
    GncSessionLoadExecutor *executor);

/** Cancel any still-queued work and release the executor owner. */
void gnc_session_load_executor_free (GncSessionLoadExecutor *executor);

G_END_DECLS

#endif /* GNC_SESSION_LOAD_EXECUTOR_H */
