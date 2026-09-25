/*
 * This program is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 */

#include <config.h>

#include <glib.h>

#include "gnc-scrub-job-runner.h"
#include "Account.h"
#include "Split.h"
#include "gnc-lot.h"
#include "Transaction.h"
#include "gnc-commodity.h"
#include "gnc-session.h"
#include "qofbook.h"
#include "qofsession.h"

typedef struct
{
    guint progress_calls;
    guint done_calls;
    GncScrubJobState state;
    guint completed[16];
    guint total[16];
    GncScrubJobKind kind[16];
    GncScrubJobPhase phase[16];
    guint lots_phase_calls;
    guint gains_phase_calls;
    guint stored_calls;
} Observer;

typedef struct
{
    QofSession *session;
    QofBook *book;
    Account *account;
    Transaction *first_transaction;
    gchar *old_auto_lots;
} LotsFixture;

static Split *
add_lots_split (LotsFixture *fixture, gint64 amount, gint64 value, time64 date)
{
    Transaction *transaction = xaccMallocTransaction (fixture->book);
    Split *split = xaccMallocSplit (fixture->book);

    xaccTransBeginEdit (transaction);
    xaccTransSetCurrency (transaction, xaccAccountGetCommodity (
        gnc_account_get_root (fixture->account)));
    xaccTransSetDatePostedSecsNormalized (transaction, date);
    xaccSplitSetParent (split, transaction);
    xaccSplitSetAccount (split, fixture->account);
    xaccSplitSetAmount (split, gnc_numeric_create (amount, 1));
    xaccSplitSetValue (split, gnc_numeric_create (value, 1));
    xaccTransCommitEdit (transaction);
    return split;
}

static LotsFixture
make_lots_fixture (void)
{
    LotsFixture fixture = { 0 };
    Account *root;
    gnc_commodity *currency;
    gnc_commodity *stock;

    if (gnc_current_session_exist ())
        gnc_clear_current_session ();
    fixture.old_auto_lots = g_strdup (g_getenv ("GNC_AUTO_SCRUB_LOTS"));
    g_unsetenv ("GNC_AUTO_SCRUB_LOTS");
    fixture.session = qof_session_new (qof_book_new ());
    fixture.book = qof_session_get_book (fixture.session);
    currency = gnc_commodity_new (fixture.book, "Runner lot currency",
                                  "CURRENCY", "RLC", "", 100);
    stock = gnc_commodity_new (fixture.book, "Runner lot stock", "NYSE",
                               "RLS", "", 1000);
    root = gnc_account_create_root (fixture.book);
    xaccAccountSetCommodity (root, currency);
    fixture.account = xaccMallocAccount (fixture.book);
    xaccAccountBeginEdit (fixture.account);
    xaccAccountSetName (fixture.account, "Runner lot account");
    xaccAccountSetType (fixture.account, ACCT_TYPE_STOCK);
    xaccAccountSetCommodity (fixture.account, stock);
    gnc_account_append_child (root, fixture.account);
    xaccAccountCommitEdit (fixture.account);

    fixture.first_transaction = xaccSplitGetParent (
        add_lots_split (&fixture, 10, 10, 86400));
    add_lots_split (&fixture, 10, 10, 172800);
    add_lots_split (&fixture, -25, -50, 259200);
    gnc_set_current_session (fixture.session);
    g_setenv ("GNC_AUTO_SCRUB_LOTS", "1", TRUE);
    return fixture;
}

static void
destroy_lots_fixture (LotsFixture *fixture)
{
    if (gnc_get_current_session () == fixture->session)
        gnc_clear_current_session ();
    else if (fixture->session)
        qof_session_destroy (fixture->session);
    if (fixture->old_auto_lots)
        g_setenv ("GNC_AUTO_SCRUB_LOTS", fixture->old_auto_lots, TRUE);
    else
        g_unsetenv ("GNC_AUTO_SCRUB_LOTS");
    g_free (fixture->old_auto_lots);
}

static GncScrubJob *
make_job (guint transaction_count, GncScrubJobKind kind)
{
    QofSession *session = qof_session_new (qof_book_new ());
    QofBook *book = qof_session_get_book (session);
    Account *root = gnc_account_create_root (book);
    Account *account = xaccMallocAccount (book);
    gnc_commodity *currency = gnc_commodity_new (book, "Runner test currency",
                                                   "CURRENCY", "RUN", "", 100);

    gnc_set_current_session (session);
    xaccAccountBeginEdit (account);
    xaccAccountSetName (account, "Runner test account");
    xaccAccountSetType (account, ACCT_TYPE_BANK);
    xaccAccountSetCommodity (account, currency);
    gnc_account_append_child (root, account);
    xaccAccountCommitEdit (account);

    for (guint index = 0; index < transaction_count; ++index)
    {
        Transaction *transaction = xaccMallocTransaction (book);
        Split *attached = xaccMallocSplit (book);
        Split *orphan = xaccMallocSplit (book);

        xaccTransBeginEdit (transaction);
        xaccTransSetCurrency (transaction, currency);
        xaccSplitSetParent (attached, transaction);
        xaccSplitSetAccount (attached, account);
        xaccSplitSetParent (orphan, transaction);
        xaccTransCommitEdit (transaction);
    }

    if (kind == GNC_SCRUB_JOB_ACCOUNT)
        return gnc_scrub_account_job_begin (account, FALSE);
    return gnc_scrub_orphans_job_begin (account, FALSE);
}

static GncScrubJob *
make_orphan_job (guint transaction_count)
{
    return make_job (transaction_count, GNC_SCRUB_JOB_ORPHANS);
}

static void
progress_cb (GncScrubJobRunner *runner, guint completed, guint total,
             GncScrubJobKind kind, GncScrubJobPhase phase, gpointer user_data)
{
    Observer *observer = user_data;

    (void)runner;
    g_assert_cmpuint (completed, <=, total);
    if (phase == GNC_SCRUB_JOB_PHASE_LOTS)
        observer->lots_phase_calls++;
    if (phase == GNC_SCRUB_JOB_PHASE_GAINS)
        observer->gains_phase_calls++;
    if (observer->stored_calls < G_N_ELEMENTS (observer->completed))
    {
        guint index = observer->stored_calls++;
        observer->completed[index] = completed;
        observer->total[index] = total;
        observer->kind[index] = kind;
        observer->phase[index] = phase;
    }
    observer->progress_calls++;
}

static void
done_cb (GncScrubJobRunner *runner, GncScrubJobState state, gpointer user_data)
{
    Observer *observer = user_data;

    (void)runner;
    observer->done_calls++;
    observer->state = state;
}

static void
iterate_until (Observer *observer, guint progress_calls)
{
    for (guint attempt = 0; attempt < 32 && observer->progress_calls < progress_calls;
         ++attempt)
        g_main_context_iteration (NULL, FALSE);

    g_assert_cmpuint (observer->progress_calls, >=, progress_calls);
}

static void
iterate_until_done (Observer *observer, guint max_iterations)
{
    for (guint attempt = 0;
         attempt < max_iterations && observer->done_calls == 0; ++attempt)
        g_main_context_iteration (NULL, FALSE);
    g_assert_cmpuint (observer->done_calls, ==, 1);
}

static void
iterate_until_gains (Observer *observer, guint max_iterations)
{
    for (guint attempt = 0;
         attempt < max_iterations && observer->gains_phase_calls == 0; ++attempt)
        g_main_context_iteration (NULL, FALSE);
    g_assert_cmpuint (observer->gains_phase_calls, >, 0);
    g_assert_cmpuint (observer->done_calls, ==, 0);
}

static void
test_runs_one_bounded_step_per_idle (void)
{
    Observer observer = { 0 };
    GncScrubJobRunner *runner = gnc_scrub_job_runner_start (
        make_orphan_job (3), NULL, NULL, 1, progress_cb, done_cb, &observer, NULL);

    iterate_until (&observer, 1);
    g_assert_cmpuint (observer.done_calls, ==, 0);
    iterate_until (&observer, 2);
    g_assert_cmpuint (observer.done_calls, ==, 0);
    iterate_until (&observer, 3);
    g_assert_cmpuint (observer.done_calls, ==, 1);
    g_assert_cmpint (observer.state, ==, GNC_SCRUB_JOB_DONE);
    g_assert_cmpint (observer.kind[0], ==, GNC_SCRUB_JOB_ORPHANS);
    g_assert_cmpint (observer.phase[0], ==, GNC_SCRUB_JOB_PHASE_ORPHANS);
    g_assert_true (gnc_scrub_job_runner_is_finished (runner));
    gnc_scrub_job_runner_unref (runner);
    gnc_clear_current_session ();
}

static void
test_cancellable_completes_once_between_steps (void)
{
    Observer observer = { 0 };
    GCancellable *cancellable = g_cancellable_new ();
    GncScrubJobRunner *runner = gnc_scrub_job_runner_start (
        make_orphan_job (3), NULL, cancellable, 1, progress_cb, done_cb,
        &observer, NULL);

    iterate_until (&observer, 1);
    g_assert_cmpuint (observer.done_calls, ==, 0);
    g_cancellable_cancel (cancellable);
    g_assert_cmpuint (observer.done_calls, ==, 0);
    iterate_until (&observer, 2);
    g_assert_cmpuint (observer.done_calls, ==, 1);
    g_assert_cmpint (observer.state, ==, GNC_SCRUB_JOB_CANCELLED);
    g_assert_cmpint (observer.kind[1], ==, GNC_SCRUB_JOB_ORPHANS);
    g_assert_cmpint (observer.phase[1], ==, GNC_SCRUB_JOB_PHASE_ORPHANS);
    for (guint attempt = 0; attempt < 4; ++attempt)
        g_main_context_iteration (NULL, FALSE);
    g_assert_cmpuint (observer.done_calls, ==, 1);
    g_assert_true (gnc_scrub_job_runner_is_finished (runner));
    gnc_scrub_job_runner_unref (runner);
    g_object_unref (cancellable);
    gnc_clear_current_session ();
}

static void
test_owner_destruction_completes_once (void)
{
    Observer observer = { 0 };
    GObject *owner = g_object_new (G_TYPE_OBJECT, NULL);
    GncScrubJobRunner *runner = gnc_scrub_job_runner_start (
        make_orphan_job (3), owner, NULL, 1, progress_cb, done_cb, &observer, NULL);

    iterate_until (&observer, 1);
    g_object_unref (owner);
    g_assert_cmpuint (observer.done_calls, ==, 1);
    g_assert_cmpint (observer.state, ==, GNC_SCRUB_JOB_CANCELLED);
    g_assert_cmpint (observer.kind[0], ==, GNC_SCRUB_JOB_ORPHANS);
    g_assert_cmpint (observer.phase[0], ==, GNC_SCRUB_JOB_PHASE_ORPHANS);
    for (guint attempt = 0; attempt < 4; ++attempt)
        g_main_context_iteration (NULL, FALSE);
    g_assert_cmpuint (observer.done_calls, ==, 1);
    gnc_scrub_job_runner_unref (runner);
    gnc_clear_current_session ();
}
static void
test_pre_cancelled_cancellable_completes_once (void)
{
    Observer observer = { 0 };
    GCancellable *cancellable = g_cancellable_new ();
    GncScrubJobRunner *runner;

    g_cancellable_cancel (cancellable);
    runner = gnc_scrub_job_runner_start (make_orphan_job (1), NULL, cancellable,
                                         1, progress_cb, done_cb, &observer, NULL);
    iterate_until (&observer, 1);
    g_assert_cmpuint (observer.done_calls, ==, 1);
    g_assert_cmpint (observer.state, ==, GNC_SCRUB_JOB_CANCELLED);
    g_assert_cmpint (observer.kind[0], ==, GNC_SCRUB_JOB_ORPHANS);
    g_assert_cmpint (observer.phase[0], ==, GNC_SCRUB_JOB_PHASE_ORPHANS);
    g_assert_true (gnc_scrub_job_runner_is_finished (runner));
    gnc_scrub_job_runner_unref (runner);
    g_object_unref (cancellable);
    gnc_clear_current_session ();
}

static void
test_composite_job_reports_phase_progress (void)
{
    Observer observer = { 0 };
    GncScrubJobRunner *runner = gnc_scrub_job_runner_start (
        make_job (2, GNC_SCRUB_JOB_ACCOUNT), NULL, NULL, 1, progress_cb,
        done_cb, &observer, NULL);

    iterate_until (&observer, 4);
    g_assert_cmpuint (observer.done_calls, ==, 1);
    g_assert_cmpint (observer.state, ==, GNC_SCRUB_JOB_DONE);
    for (guint index = 0; index < 4; ++index)
    {
        g_assert_cmpuint (observer.completed[index], ==, index + 1);
        g_assert_cmpuint (observer.total[index], ==, 4);
        g_assert_cmpint (observer.kind[index], ==, GNC_SCRUB_JOB_ACCOUNT);
    }
    g_assert_cmpint (observer.phase[0], ==, GNC_SCRUB_JOB_PHASE_ORPHANS);
    g_assert_cmpint (observer.phase[1], ==, GNC_SCRUB_JOB_PHASE_ORPHANS);
    g_assert_cmpint (observer.phase[2], ==, GNC_SCRUB_JOB_PHASE_IMBALANCE);
    g_assert_cmpint (observer.phase[3], ==, GNC_SCRUB_JOB_PHASE_IMBALANCE);
    g_assert_true (gnc_scrub_job_runner_is_finished (runner));
    gnc_scrub_job_runner_unref (runner);
    gnc_clear_current_session ();
}

static void
test_real_lots_runner_keeps_nested_gains_resumable (void)
{
    LotsFixture fixture = make_lots_fixture ();
    Observer observer = { 0 };
    GncScrubJobRunner *runner = gnc_scrub_job_runner_start (
        gnc_scrub_lots_job_begin (fixture.account, FALSE), NULL, NULL, 1,
        progress_cb, done_cb, &observer, NULL);

    g_assert_nonnull (runner);
    iterate_until_done (&observer, 8192);
    g_assert_cmpint (observer.state, ==, GNC_SCRUB_JOB_DONE);
    g_assert_cmpuint (observer.progress_calls, >, 20);
    g_assert_cmpuint (observer.lots_phase_calls, >, 1);
    g_assert_cmpuint (observer.gains_phase_calls, >, 1);
    g_assert_true (gnc_scrub_job_runner_is_finished (runner));
    for (guint attempt = 0; attempt < 8; ++attempt)
        g_main_context_iteration (NULL, FALSE);
    g_assert_cmpuint (observer.done_calls, ==, 1);

    GncScrubContext *verify = gnc_scrub_context_begin (fixture.book);
    g_assert_nonnull (verify);
    g_assert_cmpuint (gnc_scrub_deferred_commit_pending_count (
        verify, GNC_SCRUB_DEFERRED_COMMIT_GAINS), ==, 0);
    gnc_scrub_context_end (verify);
    gnc_scrub_context_unref (verify);
    gnc_scrub_job_runner_unref (runner);
    destroy_lots_fixture (&fixture);
}

static void
test_real_owner_destroy_preserves_fifo_head (void)
{
    LotsFixture fixture = make_lots_fixture ();
    Observer observer = { 0 };
    GObject *owner = g_object_new (G_TYPE_OBJECT, NULL);
    GncScrubJobRunner *runner = gnc_scrub_job_runner_start (
        gnc_scrub_lots_job_begin (fixture.account, FALSE), owner, NULL, 1,
        progress_cb, done_cb, &observer, NULL);

    g_assert_nonnull (runner);
    iterate_until_gains (&observer, 4096);
    g_object_unref (owner);
    g_assert_cmpuint (observer.done_calls, ==, 1);
    g_assert_cmpint (observer.state, ==, GNC_SCRUB_JOB_CANCELLED);
    for (guint attempt = 0; attempt < 8; ++attempt)
        g_main_context_iteration (NULL, FALSE);
    g_assert_cmpuint (observer.done_calls, ==, 1);

    GncScrubContext *handoff = gnc_scrub_context_begin (fixture.book);
    GncGUID head;
    g_assert_nonnull (handoff);
    g_assert_cmpuint (gnc_scrub_deferred_commit_pending_count (
        handoff, GNC_SCRUB_DEFERRED_COMMIT_GAINS), >, 0);
    g_assert_true (gnc_scrub_deferred_commit_peek (
        handoff, GNC_SCRUB_DEFERRED_COMMIT_GAINS, &head));
    g_assert_true (guid_equal (&head,
                               xaccTransGetGUID (fixture.first_transaction)));
    gnc_scrub_context_end (handoff);
    gnc_scrub_context_unref (handoff);
    gnc_scrub_job_runner_unref (runner);
    destroy_lots_fixture (&fixture);
}

static void
test_real_external_generation_drift_preserves_fifo (void)
{
    LotsFixture fixture = make_lots_fixture ();
    Observer observer = { 0 };
    GncScrubJobRunner *runner = gnc_scrub_job_runner_start (
        gnc_scrub_lots_job_begin (fixture.account, FALSE), NULL, NULL, 1,
        progress_cb, done_cb, &observer, NULL);
    Split *split;

    g_assert_nonnull (runner);
    iterate_until (&observer, 2);
    split = xaccTransFindSplitByAccount (fixture.first_transaction,
                                         fixture.account);
    g_assert_nonnull (split);
    xaccTransBeginEdit (fixture.first_transaction);
    xaccSplitSetAmount (split, gnc_numeric_create (11, 1));
    xaccTransCommitEdit (fixture.first_transaction);
    iterate_until_done (&observer, 8);
    g_assert_cmpint (observer.state, ==, GNC_SCRUB_JOB_FAILED);

    GncScrubContext *handoff = gnc_scrub_context_begin (fixture.book);
    GncGUID head;
    g_assert_nonnull (handoff);
    g_assert_cmpuint (gnc_scrub_deferred_commit_pending_count (
        handoff, GNC_SCRUB_DEFERRED_COMMIT_GAINS), >, 0);
    g_assert_true (gnc_scrub_deferred_commit_peek (
        handoff, GNC_SCRUB_DEFERRED_COMMIT_GAINS, &head));
    g_assert_true (guid_equal (&head,
                               xaccTransGetGUID (fixture.first_transaction)));
    gnc_scrub_context_end (handoff);
    gnc_scrub_context_unref (handoff);
    gnc_scrub_job_runner_unref (runner);
    destroy_lots_fixture (&fixture);
}

static void
test_real_runner_rejects_session_and_book_drift (void)
{
    LotsFixture fixture = make_lots_fixture ();
    Observer observer = { 0 };
    QofSession *foreign = qof_session_new (qof_book_new ());
    QofBook *current_book = fixture.book;
    QofBook *foreign_book = qof_session_get_book (foreign);
    guint64 generation = gnc_current_session_get_generation ();
    GncScrubJobRunner *runner = gnc_scrub_job_runner_start (
        gnc_scrub_lots_job_begin (fixture.account, FALSE), NULL, NULL, 1,
        progress_cb, done_cb, &observer, NULL);

    g_assert_nonnull (runner);
    g_test_expect_message ("gnc.engine", G_LOG_LEVEL_WARNING,
                           "*Refusing to replace the current session while it is leased*");
    gnc_set_current_session (foreign);
    g_test_assert_expected_messages ();
    g_assert_true (gnc_get_current_session () == fixture.session);
    g_assert_cmpuint (gnc_current_session_get_generation (), ==, generation);
    g_test_expect_message ("qof.session", G_LOG_LEVEL_WARNING,
                           "*Refusing legacy swap-data while an operation lease owns session*");
    qof_session_swap_data (fixture.session, foreign);
    g_test_assert_expected_messages ();
    g_assert_true (qof_session_get_book (fixture.session) == current_book);
    g_assert_true (qof_session_get_book (foreign) == foreign_book);
    iterate_until_done (&observer, 8192);
    g_assert_cmpint (observer.state, ==, GNC_SCRUB_JOB_DONE);
    g_assert_true (gnc_get_current_session () == fixture.session);
    g_assert_cmpuint (gnc_current_session_get_generation (), ==, generation);
    g_assert_true (qof_session_get_book (fixture.session) == current_book);
    gnc_scrub_job_runner_unref (runner);
    qof_session_destroy (foreign);
    destroy_lots_fixture (&fixture);
}

int
main (int argc, char **argv)
{
    g_test_init (&argc, &argv, NULL);
    gnc_engine_init (argc, argv);
    g_test_add_func ("/gnome-utils/scrub-job-runner/bounded-idle",
                     test_runs_one_bounded_step_per_idle);
    g_test_add_func ("/gnome-utils/scrub-job-runner/cancellable",
                     test_cancellable_completes_once_between_steps);
    g_test_add_func ("/gnome-utils/scrub-job-runner/owner-destroyed",
                     test_owner_destruction_completes_once);
    g_test_add_func ("/gnome-utils/scrub-job-runner/pre-cancelled",
                     test_pre_cancelled_cancellable_completes_once);
    g_test_add_func ("/gnome-utils/scrub-job-runner/composite-phases",
                     test_composite_job_reports_phase_progress);
    g_test_add_func ("/gnome-utils/scrub-job-runner/real-lots-nested-gains",
                     test_real_lots_runner_keeps_nested_gains_resumable);
    g_test_add_func ("/gnome-utils/scrub-job-runner/real-owner-fifo-handoff",
                     test_real_owner_destroy_preserves_fifo_head);
    g_test_add_func ("/gnome-utils/scrub-job-runner/real-generation-drift",
                     test_real_external_generation_drift_preserves_fifo);
    g_test_add_func ("/gnome-utils/scrub-job-runner/real-session-book-drift",
                     test_real_runner_rejects_session_and_book_drift);

    int status = g_test_run ();
    gnc_engine_shutdown ();
    return status;
}
