/********************************************************************\
 * gnc-scrub-job-runner.h -- Main-context runner for resumable scrubs *
 * Copyright (C) 2026                                              *
\********************************************************************/
#ifndef GNC_SCRUB_JOB_RUNNER_H
#define GNC_SCRUB_JOB_RUNNER_H

#include <gio/gio.h>
#include "Scrub.h"

G_BEGIN_DECLS

typedef struct GncScrubJobRunner GncScrubJobRunner;
typedef void (*GncScrubJobRunnerProgressFunc) (GncScrubJobRunner *runner,
                                                guint completed, guint total,
                                                GncScrubJobKind kind,
                                                GncScrubJobPhase phase,
                                                gpointer user_data);
typedef void (*GncScrubJobRunnerDoneFunc) (GncScrubJobRunner *runner,
                                            GncScrubJobState state,
                                            gpointer user_data);

/* The runner owns job and calls done_cb, then frees job, exactly once. */
GncScrubJobRunner *gnc_scrub_job_runner_start (
    GncScrubJob *job, GObject *owner, GCancellable *cancellable,
    guint max_transactions_per_idle, GncScrubJobRunnerProgressFunc progress_cb,
    GncScrubJobRunnerDoneFunc done_cb, gpointer user_data,
    GDestroyNotify user_data_destroy);
/* Production seam used by account-tree and reconcile callers. */
GncScrubJobRunner *gnc_scrub_job_runner_start_lots (
    Account *account, gboolean descendants, GObject *owner,
    GCancellable *cancellable, guint max_transactions_per_idle,
    GncScrubJobRunnerProgressFunc progress_cb,
    GncScrubJobRunnerDoneFunc done_cb, gpointer user_data,
    GDestroyNotify user_data_destroy);
GncScrubJobRunner *gnc_scrub_job_runner_ref (GncScrubJobRunner *runner);
void gnc_scrub_job_runner_unref (GncScrubJobRunner *runner);
void gnc_scrub_job_runner_cancel (GncScrubJobRunner *runner);
gboolean gnc_scrub_job_runner_is_finished (const GncScrubJobRunner *runner);

G_END_DECLS
#endif