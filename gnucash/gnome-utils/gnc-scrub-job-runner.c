/********************************************************************\
 * gnc-scrub-job-runner.c -- Main-context runner for resumable scrubs *
 * Copyright (C) 2026                                              *
\********************************************************************/
#include <config.h>
#include "gnc-scrub-job-runner.h"

struct GncScrubJobRunner
{
    gatomicrefcount ref_count;
    GncScrubJob *job;
    GCancellable *cancellable;
    GApplication *application;
    GObject *owner; /* weak; owner_registered holds a runner reference */
    guint source_id;
    guint max_transactions_per_idle;
    gboolean owner_registered;
    gboolean in_source_callback;
    gboolean finished;
    GncScrubJobRunnerProgressFunc progress_cb;
    GncScrubJobRunnerDoneFunc done_cb;
    gpointer user_data;
    GDestroyNotify user_data_destroy;
};

static void runner_finish (GncScrubJobRunner *runner, GncScrubJobState state);
static void owner_gone (gpointer data, GObject *where_the_owner_was);

GncScrubJobRunner *
gnc_scrub_job_runner_ref (GncScrubJobRunner *runner)
{
    g_return_val_if_fail (runner != NULL, NULL);
    g_atomic_ref_count_inc (&runner->ref_count);
    return runner;
}

static void
runner_dispose (GncScrubJobRunner *runner)
{
    g_clear_object (&runner->cancellable);
    if (runner->owner_registered)
        g_object_weak_unref (runner->owner, owner_gone, runner);
    if (runner->application)
    {
        g_application_release (runner->application);
        g_clear_object (&runner->application);
    }
    if (runner->job)
        gnc_scrub_job_free (runner->job);
    if (runner->user_data_destroy)
        runner->user_data_destroy (runner->user_data);
    g_free (runner);
}

void
gnc_scrub_job_runner_unref (GncScrubJobRunner *runner)
{
    g_return_if_fail (runner != NULL);
    if (g_atomic_ref_count_dec (&runner->ref_count))
        runner_dispose (runner);
}

static void
owner_gone (gpointer data, GObject *where_the_owner_was)
{
    GncScrubJobRunner *runner = data;
    (void)where_the_owner_was;
    runner->owner_registered = FALSE;
    runner->owner = NULL;
    gnc_scrub_job_runner_cancel (runner);
    gnc_scrub_job_runner_unref (runner);
}

static void
runner_finish (GncScrubJobRunner *runner, GncScrubJobState state)
{
    gboolean release_owner_ref = FALSE;
    if (runner->finished)
        return;

    /* Completion callbacks may release every caller-owned reference. */
    gnc_scrub_job_runner_ref (runner);
    runner->finished = TRUE;

    if (runner->owner_registered)
    {
        g_object_weak_unref (runner->owner, owner_gone, runner);
        runner->owner_registered = FALSE;
        runner->owner = NULL;
        release_owner_ref = TRUE;
    }
    if (runner->application)
    {
        g_application_release (runner->application);
        g_clear_object (&runner->application);
    }
    if (runner->done_cb)
        runner->done_cb (runner, state, runner->user_data);
    if (runner->job)
    {
        gnc_scrub_job_free (runner->job);
        runner->job = NULL;
    }
    if (release_owner_ref)
        gnc_scrub_job_runner_unref (runner);
    gnc_scrub_job_runner_unref (runner);
}

static gboolean
runner_idle (gpointer data)
{
    GncScrubJobRunner *runner = data;
    GncScrubJobState state;
    GncScrubJobKind kind;
    GncScrubJobPhase phase;
    runner->in_source_callback = TRUE;
    if (runner->finished)
    {
        runner->in_source_callback = FALSE;
        return G_SOURCE_REMOVE;
    }
    if (runner->cancellable && g_cancellable_is_cancelled (runner->cancellable))
        gnc_scrub_job_cancel (runner->job);
    kind = gnc_scrub_job_get_kind (runner->job);
    /*
     * A composite job advances its phase immediately after its last
     * transaction. Capture the phase before the step so that the callback's
     * completed value and phase describe the same bounded phase, including
     * the terminal callback for the ORPHANS phase.
     */
    phase = gnc_scrub_job_get_phase (runner->job);
    state = gnc_scrub_job_step (runner->job, runner->max_transactions_per_idle);
    if (runner->progress_cb)
        runner->progress_cb (runner, gnc_scrub_job_get_completed (runner->job),
                             gnc_scrub_job_get_total (runner->job), kind, phase,
                             runner->user_data);
    runner->in_source_callback = FALSE;
    if (runner->finished)
        return G_SOURCE_REMOVE;
    if (state != GNC_SCRUB_JOB_RUNNING)
    {
        runner_finish (runner, state);
        return G_SOURCE_REMOVE;
    }
    return G_SOURCE_CONTINUE;
}

static void
source_destroy (gpointer data)
{
    GncScrubJobRunner *runner = data;
    runner->source_id = 0;
    if (!runner->finished)
    {
        gnc_scrub_job_cancel (runner->job);
        runner_finish (runner, gnc_scrub_job_get_state (runner->job));
    }
    gnc_scrub_job_runner_unref (runner);
}

GncScrubJobRunner *
gnc_scrub_job_runner_start (GncScrubJob *job, GObject *owner,
                            GCancellable *cancellable,
                            guint max_transactions_per_idle,
                            GncScrubJobRunnerProgressFunc progress_cb,
                            GncScrubJobRunnerDoneFunc done_cb,
                            gpointer user_data,
                            GDestroyNotify user_data_destroy)
{
    GncScrubJobRunner *runner;
    GApplication *application;
    g_return_val_if_fail (job != NULL, NULL);
    g_return_val_if_fail (owner == NULL || G_IS_OBJECT (owner), NULL);
    g_return_val_if_fail (cancellable == NULL || G_IS_CANCELLABLE (cancellable), NULL);

    runner = g_new0 (GncScrubJobRunner, 1);
    g_atomic_ref_count_init (&runner->ref_count);
    runner->job = job;
    runner->max_transactions_per_idle = max_transactions_per_idle ?: 1;
    runner->progress_cb = progress_cb;
    runner->done_cb = done_cb;
    runner->user_data = user_data;
    runner->user_data_destroy = user_data_destroy;
    if (cancellable)
    {
        runner->cancellable = g_object_ref (cancellable);
        /* Cancellation is observed from the idle callback on this main context. */
    }
    if (owner)
    {
        runner->owner = owner;
        runner->owner_registered = TRUE;
        gnc_scrub_job_runner_ref (runner);
        g_object_weak_ref (owner, owner_gone, runner);
    }
    application = g_application_get_default ();
    if (application && G_IS_APPLICATION (application))
    {
        runner->application = g_object_ref (application);
        g_application_hold (runner->application);
    }
    runner->source_id = g_idle_add_full (G_PRIORITY_DEFAULT_IDLE, runner_idle,
        gnc_scrub_job_runner_ref (runner), source_destroy);
    return runner;
}

GncScrubJobRunner *
gnc_scrub_job_runner_start_lots (
    Account *account, gboolean descendants, GObject *owner,
    GCancellable *cancellable, guint max_transactions_per_idle,
    GncScrubJobRunnerProgressFunc progress_cb,
    GncScrubJobRunnerDoneFunc done_cb, gpointer user_data,
    GDestroyNotify user_data_destroy)
{
    GncScrubJob *job;
    GncScrubJobRunner *runner;

    g_return_val_if_fail (account != NULL, NULL);
    job = gnc_scrub_lots_job_begin (account, descendants);
    if (!job)
        return NULL;
    runner = gnc_scrub_job_runner_start (
        job, owner, cancellable, max_transactions_per_idle, progress_cb,
        done_cb, user_data, user_data_destroy);
    if (!runner)
        gnc_scrub_job_free (job);
    return runner;
}

void
gnc_scrub_job_runner_cancel (GncScrubJobRunner *runner)
{
    GncScrubJobState state;
    guint source_id;
    g_return_if_fail (runner != NULL);
    if (runner->finished)
        return;
    gnc_scrub_job_cancel (runner->job);
    state = gnc_scrub_job_get_state (runner->job);
    runner_finish (runner, state);
    if (!runner->in_source_callback && runner->source_id)
    {
        source_id = runner->source_id;
        runner->source_id = 0;
        g_source_remove (source_id);
    }
}

gboolean
gnc_scrub_job_runner_is_finished (const GncScrubJobRunner *runner)
{
    g_return_val_if_fail (runner != NULL, TRUE);
    return runner->finished;
}