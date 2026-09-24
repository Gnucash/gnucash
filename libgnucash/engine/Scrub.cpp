/********************************************************************\
 * Scrub.c -- convert single-entry accounts into clean double-entry *
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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

/*
 * FILE:
 * Scrub.c
 *
 * FUNCTION:
 * Provides a set of functions and utilities for scrubbing clean
 * single-entry accounts so that they can be promoted into
 * self-consistent, clean double-entry accounts.
 *
 * HISTORY:
 * Created by Linas Vepstas December 1998
 * Copyright (c) 1998-2000, 2003 Linas Vepstas <linas@linas.org>
 * Copyright (c) 2002 Christian Stimming
 * Copyright (c) 2006 David Hampton
 */

#include <config.h>

#include <glib.h>
#include <glib/gi18n.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <atomic>
#include <deque>
#include <new>
#include <unordered_set>
#include <vector>

#include "Account.h"
#include "AccountP.hpp"
#include "Account.hpp"
#include "Scrub.h"
#include "Scrub3.h"
#include "ScrubP.h"
#include "Transaction.h"
#include "TransactionP.hpp"
#include "gnc-commodity.h"
#include "gnc-lot.h"
#include "guid.hpp"
#include "qofbook.h"
#include "qofinstance-p.h"
#include "gnc-session.h"

#undef G_LOG_DOMAIN
#define G_LOG_DOMAIN "gnc.engine.scrub"

static QofLogModule log_module = G_LOG_DOMAIN;

struct GncScrubContext
{
    std::atomic_uint ref_count{1};
    QofSession *session{};
    QofBook *book{};
    QofSessionOperationLease *lease{};
    guint64 operation_id{};
    std::atomic_bool cancelled{};
};

struct GncScrubJob
{
    GncScrubContext *context;
    QofBook *book;
    std::vector<GncGUID> transaction_guids;
    size_t cursor;
    GncScrubJobState state;
    GncScrubJobKind kind;
    GncScrubJobPhase phase;
    guint phase_count;
    GncTransactionGainsPlan *gains_child;
    GncGUID gains_head;
    guint gains_completed;
    GncAccountLotsPlan *account_lots_child;
    GncLotScrubPlan *lot_child;
    guint structural_completed;
    gboolean structural_changed;
};

struct GncScrubDeferredCommitWork
{
    std::deque<GncGUID> fifo;
    std::unordered_set<GncGUID> queued;
};

struct GncScrubDeferredCommitQueue
{
    QofSession *session;
    guint64 operation_id;
    guint64 operation_generation;
    guint enabled_kinds;
    GncScrubDeferredCommitWork imbalance;
    GncScrubDeferredCommitWork gains;
};

static constexpr char deferred_commit_queue_key[] =
    "gnc-scrub-deferred-commit-queue";

static void
deferred_commit_queue_destroy (QofBook *, gpointer, gpointer data)
{
    delete static_cast<GncScrubDeferredCommitQueue *> (data);
}

static GncScrubDeferredCommitQueue *
deferred_commit_queue (QofBook *book, gboolean create)
{
    if (!book)
        return nullptr;

    auto queue = static_cast<GncScrubDeferredCommitQueue *> (
        qof_book_get_data (book, deferred_commit_queue_key));
    if (queue || !create)
        return queue;

    queue = new (std::nothrow) GncScrubDeferredCommitQueue {};
    if (!queue)
        return nullptr;

    qof_book_set_data_fin (book, deferred_commit_queue_key, queue,
                           deferred_commit_queue_destroy);
    return queue;
}

static GncScrubDeferredCommitWork *
deferred_commit_work (GncScrubDeferredCommitQueue *queue,
                      GncScrubDeferredCommitKind kind)
{
    if (!queue)
        return nullptr;
    switch (kind)
    {
    case GNC_SCRUB_DEFERRED_COMMIT_IMBALANCE:
        return &queue->imbalance;
    case GNC_SCRUB_DEFERRED_COMMIT_GAINS:
        return &queue->gains;
    }
    return nullptr;
}

static guint
deferred_commit_kind_bit (GncScrubDeferredCommitKind kind)
{
    switch (kind)
    {
    case GNC_SCRUB_DEFERRED_COMMIT_IMBALANCE:
        return 1u << GNC_SCRUB_DEFERRED_COMMIT_IMBALANCE;
    case GNC_SCRUB_DEFERRED_COMMIT_GAINS:
        return 1u << GNC_SCRUB_DEFERRED_COMMIT_GAINS;
    }
    return 0;
}

static gboolean
deferred_commit_context_valid (const GncScrubContext *context)
{
    return context && !gnc_scrub_context_is_cancelled (context) &&
           gnc_scrub_context_owns_book (context, context->book);
}

static void
deferred_commit_context_deactivate (const GncScrubContext *context)
{
    if (!context || !context->book)
        return;

    auto queue = deferred_commit_queue (context->book, FALSE);
    if (!queue || queue->session != context->session ||
        queue->operation_id != context->operation_id)
        return;

    queue->session = nullptr;
    queue->operation_id = 0;
    queue->operation_generation = 0;
    queue->enabled_kinds = 0;
}

static gboolean
deferred_commit_queue_is_active (const GncScrubDeferredCommitQueue *queue,
                                 const QofBook *book,
                                 GncScrubDeferredCommitKind kind)
{
    auto kind_bit = deferred_commit_kind_bit (kind);
    if (!queue || !kind_bit || !(queue->enabled_kinds & kind_bit) ||
        !queue->session || !queue->operation_id ||
        !queue->operation_generation || !book || !gnc_current_session_exist ())
        return FALSE;

    auto session = gnc_get_current_session ();
    return session == queue->session && qof_session_get_book (session) == book &&
           qof_session_has_active_operation_kind (
               session, QOF_SESSION_OPERATION_SCRUB) &&
           qof_session_get_operation_generation (session) ==
               queue->operation_generation;
}


static Account* xaccScrubUtilityGetOrMakeAccount (Account *root,
                                                  gnc_commodity* currency,
                                                  const char* accname,
                                                  GNCAccountType acctype,
                                                  gboolean placeholder,
                                                  gboolean checkname);
static void TransScrubCurrency (Transaction *trans,
                                GncScrubContext *context);
static void TransScrubOrphansFast (Transaction *trans, Account *root,
                                   GncScrubContext *context);
static void AccountScrubCommodity (Account *account);
static void TransScrubSplits (Transaction *trans, GncScrubContext *context);
static void SplitScrub (Split *split, GncScrubContext *context);

GncScrubContext *
gnc_scrub_context_begin (QofBook *book)
{
    if (!book || !gnc_current_session_exist ())
        return nullptr;

    auto session = gnc_get_current_session ();
    if (qof_session_get_book (session) != book)
    {
        PWARN ("Refusing scrub context for a book outside the current session");
        return nullptr;
    }

    auto lease = qof_session_operation_lease_acquire_for (
        session, QOF_SESSION_OPERATION_SCRUB);
    if (!lease)
        return nullptr;

    auto context = new (std::nothrow) GncScrubContext;
    if (!context)
    {
        qof_session_operation_lease_release (lease);
        return nullptr;
    }
    context->session = session;
    context->book = book;
    context->lease = lease;
    context->operation_id = qof_session_operation_lease_get_id (lease);
    return context;
}

GncScrubContext *
gnc_scrub_context_ref (GncScrubContext *context)
{
    if (context)
        context->ref_count.fetch_add (1, std::memory_order_relaxed);
    return context;
}

gboolean
gnc_scrub_context_is_active (const GncScrubContext *context)
{
    if (!context || !context->lease ||
        !gnc_current_session_exist ())
        return FALSE;

    auto current = gnc_get_current_session ();
    return current && current == context->session &&
           qof_session_get_book (current) == context->book &&
           qof_session_operation_lease_get_id (context->lease) ==
               context->operation_id &&
           qof_session_operation_lease_get_kind (context->lease) ==
               QOF_SESSION_OPERATION_SCRUB;
}

gboolean
gnc_scrub_context_owns_book (const GncScrubContext *context,
                             const QofBook *book)
{
    return book && context && context->book == book &&
           gnc_scrub_context_is_active (context);
}

void
gnc_scrub_context_cancel (GncScrubContext *context)
{
    if (gnc_scrub_context_is_active (context))
    {
        context->cancelled.store (true, std::memory_order_release);
        deferred_commit_context_deactivate (context);
    }
}

gboolean
gnc_scrub_context_is_cancelled (const GncScrubContext *context)
{
    return context && context->cancelled.load (std::memory_order_acquire);
}

void
gnc_scrub_context_end (GncScrubContext *context)
{
    if (!context || !context->lease)
        return;

    if (gnc_scrub_context_is_active (context))
        deferred_commit_context_deactivate (context);

    auto lease = context->lease;
    context->lease = nullptr;
    qof_session_operation_lease_release (lease);
}

void
gnc_scrub_context_unref (GncScrubContext *context)
{
    if (!context || context->ref_count.fetch_sub (
                        1, std::memory_order_acq_rel) != 1)
        return;

    gnc_scrub_context_end (context);
    delete context;
}

gboolean
gnc_scrub_context_validate_for_book (const GncScrubContext *context,
                                     const QofBook *book,
                                     const char *operation)
{
    if (gnc_scrub_context_owns_book (context, book))
        return TRUE;

    PWARN ("Refusing %s without its active book-bound scrub context",
           operation ? operation : "scrub");
    return FALSE;
}

gboolean
gnc_scrub_context_enable_commit_deferral (GncScrubContext *context,
                                           GncScrubDeferredCommitKind kind)
{
    auto kind_bit = deferred_commit_kind_bit (kind);
    if (!kind_bit || !deferred_commit_context_valid (context))
        return FALSE;

    auto queue = deferred_commit_queue (context->book, TRUE);
    if (!queue)
        return FALSE;

    queue->session = context->session;
    queue->operation_id = context->operation_id;
    queue->operation_generation = qof_session_get_operation_generation (
        context->session);
    queue->enabled_kinds |= kind_bit;
    return TRUE;
}

gboolean
gnc_scrub_context_commit_deferral_enabled (
    const GncScrubContext *context, GncScrubDeferredCommitKind kind)
{
    auto kind_bit = deferred_commit_kind_bit (kind);
    if (!kind_bit || !deferred_commit_context_valid (context))
        return FALSE;

    auto queue = deferred_commit_queue (context->book, FALSE);
    return queue && queue->session == context->session &&
           queue->operation_id == context->operation_id &&
           queue->operation_generation == qof_session_get_operation_generation (
               context->session) &&
           (queue->enabled_kinds & kind_bit);
}

guint
gnc_scrub_deferred_commit_pending_count (
    const GncScrubContext *context, GncScrubDeferredCommitKind kind)
{
    if (!deferred_commit_context_valid (context))
        return 0;

    auto work = deferred_commit_work (
        deferred_commit_queue (context->book, FALSE), kind);
    return work ? static_cast<guint> (work->fifo.size ()) : 0;
}

gboolean
gnc_scrub_deferred_commit_peek (
    const GncScrubContext *context, GncScrubDeferredCommitKind kind,
    GncGUID *guid)
{
    if (!guid || !deferred_commit_context_valid (context))
        return FALSE;

    auto work = deferred_commit_work (
        deferred_commit_queue (context->book, FALSE), kind);
    if (!work || work->fifo.empty ())
        return FALSE;

    *guid = work->fifo.front ();
    return TRUE;
}

gboolean
gnc_scrub_deferred_commit_ack (
    const GncScrubContext *context, GncScrubDeferredCommitKind kind,
    const GncGUID *guid)
{
    if (!guid || !deferred_commit_context_valid (context))
        return FALSE;

    auto work = deferred_commit_work (
        deferred_commit_queue (context->book, FALSE), kind);
    if (!work || work->fifo.empty () ||
        !guid_equal (&work->fifo.front (), guid))
        return FALSE;

    work->queued.erase (work->fifo.front ());
    work->fifo.pop_front ();
    return TRUE;
}

gboolean
gnc_scrub_defer_commit_hook (QofBook *book, const GncGUID *guid,
                             GncScrubDeferredCommitKind kind)
{
    if (!guid)
        return FALSE;

    auto queue = deferred_commit_queue (book, FALSE);
    if (!deferred_commit_queue_is_active (queue, book, kind))
        return FALSE;

    auto work = deferred_commit_work (queue, kind);
    if (!work)
        return FALSE;

    if (work->queued.insert (*guid).second)
        work->fifo.push_back (*guid);
    return TRUE;
}

gboolean
gnc_scrub_legacy_operation_allowed (const QofBook *book,
                                    const char *operation)
{
    if (!gnc_current_session_exist ())
        return TRUE;

    auto session = gnc_get_current_session ();
    if (qof_session_get_book (session) != book ||
        !qof_session_has_active_operation_kind (
            session, QOF_SESSION_OPERATION_SCRUB))
        return TRUE;

    PWARN ("Refusing legacy %s while an explicit scrub context owns book %p",
           operation ? operation : "scrub", book);
    return FALSE;
}

/* ================================================================ */

using TransSet = std::unordered_set<Transaction*>;

static TransSet
get_all_transactions (Account *account, bool descendants)
{
    TransSet set;
    auto add_transactions = [&set](auto a)
    { gnc_account_foreach_split (a, [&set](auto s){ set.insert (xaccSplitGetParent (s)); }); };
    add_transactions (account);
    if (descendants)
        gnc_account_foreach_descendant (account, add_transactions);
    return set;
}

static void
gnc_scrub_job_finish (GncScrubJob *job, GncScrubJobState state)
{
    if (!job || job->state != GNC_SCRUB_JOB_RUNNING)
        return;

    job->state = state;
    gnc_scrub_context_end (job->context);
}

static GncScrubJob *
gnc_scrub_job_begin (Account *account, gboolean descendants,
                     GncScrubJobKind kind, guint phase_count)
{
    if (!account)
        return nullptr;

    auto book = qof_instance_get_book (QOF_INSTANCE (account));
    auto context = gnc_scrub_context_begin (book);
    if (!context)
        return nullptr;

    auto phase = kind == GNC_SCRUB_JOB_IMBALANCE
        ? GNC_SCRUB_JOB_PHASE_IMBALANCE : GNC_SCRUB_JOB_PHASE_ORPHANS;
    auto job = new GncScrubJob{context, book, {}, 0, GNC_SCRUB_JOB_RUNNING,
                               kind, phase, phase_count};
    auto transactions = get_all_transactions (account, descendants);
    job->transaction_guids.reserve (transactions.size ());
    for (auto transaction : transactions)
        job->transaction_guids.push_back (*xaccTransGetGUID (transaction));
    return job;
}

GncScrubJob *
gnc_scrub_orphans_job_begin (Account *account, gboolean descendants)
{
    return gnc_scrub_job_begin (account, descendants, GNC_SCRUB_JOB_ORPHANS,
                                1);
}

GncScrubJob *
gnc_scrub_imbalance_job_begin (Account *account, gboolean descendants)
{
    return gnc_scrub_job_begin (account, descendants,
                                GNC_SCRUB_JOB_IMBALANCE, 1);
}

GncScrubJob *
gnc_scrub_account_job_begin (Account *account, gboolean descendants)
{
    return gnc_scrub_job_begin (account, descendants, GNC_SCRUB_JOB_ACCOUNT,
                                2);
}

GncScrubJob *
gnc_scrub_deferred_gains_job_begin (QofBook *book)
{
    if (!book)
        return nullptr;
    auto context = gnc_scrub_context_begin (book);
    if (!context)
        return nullptr;
    if (!gnc_scrub_context_enable_commit_deferral (
            context, GNC_SCRUB_DEFERRED_COMMIT_GAINS))
    {
        gnc_scrub_context_unref (context);
        return nullptr;
    }
    return new GncScrubJob {
        context, book, {}, 0, GNC_SCRUB_JOB_RUNNING, GNC_SCRUB_JOB_GAINS,
        GNC_SCRUB_JOB_PHASE_GAINS, 1, nullptr, *guid_null (), 0,
        nullptr, nullptr, 0, FALSE};
}

static GncScrubJob *
gnc_scrub_structural_job_begin (QofBook *book, GncScrubJobKind kind)
{
    auto context = gnc_scrub_context_begin (book);
    if (!context)
        return nullptr;
    if (!gnc_scrub_context_enable_commit_deferral (
            context, GNC_SCRUB_DEFERRED_COMMIT_GAINS))
    {
        gnc_scrub_context_unref (context);
        return nullptr;
    }
    return new GncScrubJob {
        context, book, {}, 0, GNC_SCRUB_JOB_RUNNING, kind,
        GNC_SCRUB_JOB_PHASE_LOTS, 1, nullptr, *guid_null (), 0,
        nullptr, nullptr, 0, FALSE};
}

GncScrubJob *
gnc_scrub_lots_job_begin (Account *account, gboolean descendants)
{
    if (!account)
        return nullptr;
    auto book = qof_instance_get_book (QOF_INSTANCE (account));
    auto job = gnc_scrub_structural_job_begin (book, GNC_SCRUB_JOB_LOTS);
    if (!job)
        return nullptr;
    job->account_lots_child = gnc_account_lots_plan_begin (
        account, descendants, job->context);
    if (!job->account_lots_child)
    {
        gnc_scrub_job_free (job);
        return nullptr;
    }
    return job;
}

GncScrubJob *
gnc_scrub_lot_job_begin (GNCLot *lot)
{
    if (!lot)
        return nullptr;
    auto account = gnc_lot_get_account (lot);
    if (!account || xaccAccountIsAPARType (xaccAccountGetType (account)))
        return nullptr;
    auto book = qof_instance_get_book (QOF_INSTANCE (lot));
    auto job = gnc_scrub_structural_job_begin (book, GNC_SCRUB_JOB_LOT);
    if (!job)
        return nullptr;
    job->lot_child = gnc_lot_scrub_plan_begin (lot, job->context);
    if (!job->lot_child)
    {
        gnc_scrub_job_free (job);
        return nullptr;
    }
    return job;
}

static gboolean
gnc_scrub_job_process_transaction (GncScrubJob *job, Transaction *transaction)
{
    switch (job->phase)
    {
    case GNC_SCRUB_JOB_PHASE_ORPHANS:
        xaccTransScrubOrphansWithContext (transaction, job->context);
        return TRUE;
    case GNC_SCRUB_JOB_PHASE_IMBALANCE:
    {
        auto root = gnc_book_get_root_account (job->book);
        if (!root)
            return FALSE;
        TransScrubOrphansFast (transaction, root, job->context);
        if (gnc_scrub_context_is_cancelled (job->context))
            return TRUE;
        TransScrubCurrency (transaction, job->context);
        if (gnc_scrub_context_is_cancelled (job->context))
            return TRUE;
        xaccTransScrubImbalanceWithContext (transaction, root, nullptr,
                                            job->context);
        return TRUE;
    }
    case GNC_SCRUB_JOB_PHASE_GAINS:
    case GNC_SCRUB_JOB_PHASE_LOTS:
        return FALSE;
    }
    return FALSE;
}

static gboolean
gnc_scrub_job_advance_phase (GncScrubJob *job)
{
    if (job->phase_count != 2 ||
        job->phase != GNC_SCRUB_JOB_PHASE_ORPHANS)
        return FALSE;

    job->phase = GNC_SCRUB_JOB_PHASE_IMBALANCE;
    job->cursor = 0;
    return TRUE;
}

static GncScrubJobState
gnc_scrub_gains_job_step (GncScrubJob *job, guint max_work)
{
    if (!job->gains_child)
    {
        if (!gnc_scrub_deferred_commit_peek (
                job->context, GNC_SCRUB_DEFERRED_COMMIT_GAINS,
                &job->gains_head))
        {
            gnc_scrub_job_finish (job, GNC_SCRUB_JOB_DONE);
            return job->state;
        }
        auto transaction = xaccTransLookup (&job->gains_head, job->book);
        if (!transaction)
        {
            if (!gnc_scrub_deferred_commit_ack (
                    job->context, GNC_SCRUB_DEFERRED_COMMIT_GAINS,
                    &job->gains_head))
                gnc_scrub_job_finish (job, GNC_SCRUB_JOB_FAILED);
            else
                ++job->gains_completed;
            return job->state;
        }
        job->gains_child = gnc_transaction_gains_plan_begin (
            transaction, nullptr, job->context);
        if (!job->gains_child)
        {
            gnc_scrub_job_finish (job, GNC_SCRUB_JOB_FAILED);
            return job->state;
        }
    }

    auto child_state = gnc_transaction_gains_plan_step (job->gains_child,
                                                         max_work);
    if (child_state == GNC_TRANSACTION_GAINS_PLAN_RUNNING)
        return job->state;
    gnc_transaction_gains_plan_free (job->gains_child);
    job->gains_child = nullptr;
    if (child_state == GNC_TRANSACTION_GAINS_PLAN_CANCELLED)
        gnc_scrub_job_finish (job, GNC_SCRUB_JOB_CANCELLED);
    else if (child_state != GNC_TRANSACTION_GAINS_PLAN_DONE ||
             !gnc_scrub_deferred_commit_ack (
                 job->context, GNC_SCRUB_DEFERRED_COMMIT_GAINS,
                 &job->gains_head))
        gnc_scrub_job_finish (job, GNC_SCRUB_JOB_FAILED);
    else
        ++job->gains_completed;
    return job->state;
}

static GncScrubJobState
gnc_scrub_structural_job_step (GncScrubJob *job, guint max_work)
{
    guint remaining = max_work;
    while (remaining && job->state == GNC_SCRUB_JOB_RUNNING)
    {
        if (job->phase == GNC_SCRUB_JOB_PHASE_GAINS)
        {
            gnc_scrub_gains_job_step (job, 1);
            --remaining;
            continue;
        }

        if (job->kind == GNC_SCRUB_JOB_LOTS)
        {
            auto state = gnc_account_lots_plan_step (
                job->account_lots_child, 1);
            --remaining;
            job->structural_completed = gnc_account_lots_plan_get_completed (
                job->account_lots_child);
            if (state == GNC_ACCOUNT_LOTS_PLAN_RUNNING)
                continue;
            gnc_account_lots_plan_free (job->account_lots_child);
            job->account_lots_child = nullptr;
            if (state == GNC_ACCOUNT_LOTS_PLAN_DONE)
                job->phase = GNC_SCRUB_JOB_PHASE_GAINS;
            else if (state == GNC_ACCOUNT_LOTS_PLAN_CANCELLED)
                gnc_scrub_job_finish (job, GNC_SCRUB_JOB_CANCELLED);
            else
                gnc_scrub_job_finish (job, GNC_SCRUB_JOB_FAILED);
            continue;
        }

        auto state = gnc_lot_scrub_plan_step (job->lot_child, 1);
        --remaining;
        if (state == GNC_LOT_SCRUB_PLAN_RUNNING)
            continue;
        job->structural_changed =
            gnc_lot_scrub_plan_get_splits_deleted (job->lot_child);
        gnc_lot_scrub_plan_free (job->lot_child);
        job->lot_child = nullptr;
        if (state == GNC_LOT_SCRUB_PLAN_DONE)
        {
            job->structural_completed = 1;
            job->phase = GNC_SCRUB_JOB_PHASE_GAINS;
        }
        else if (state == GNC_LOT_SCRUB_PLAN_CANCELLED)
            gnc_scrub_job_finish (job, GNC_SCRUB_JOB_CANCELLED);
        else
            gnc_scrub_job_finish (job, GNC_SCRUB_JOB_FAILED);
    }
    return job->state;
}

GncScrubJobState
gnc_scrub_job_step (GncScrubJob *job, guint max_transactions)
{
    if (!job)
        return GNC_SCRUB_JOB_FAILED;
    if (job->state != GNC_SCRUB_JOB_RUNNING)
        return job->state;
    if (max_transactions == 0)
    {
        gnc_scrub_job_finish (job, GNC_SCRUB_JOB_FAILED);
        return job->state;
    }
    if (gnc_scrub_context_is_cancelled (job->context))
    {
        gnc_scrub_job_finish (job, GNC_SCRUB_JOB_CANCELLED);
        return job->state;
    }
    if (!gnc_scrub_context_is_active (job->context))
    {
        gnc_scrub_job_finish (job, GNC_SCRUB_JOB_FAILED);
        return job->state;
    }

    if (job->kind == GNC_SCRUB_JOB_GAINS)
        return gnc_scrub_gains_job_step (job, max_transactions);

    if (job->kind == GNC_SCRUB_JOB_LOTS ||
        job->kind == GNC_SCRUB_JOB_LOT)
        return gnc_scrub_structural_job_step (job, max_transactions);

    guint processed = 0;
    while (processed < max_transactions &&
           job->cursor < job->transaction_guids.size ())
    {
        if (gnc_scrub_context_is_cancelled (job->context))
        {
            gnc_scrub_job_finish (job, GNC_SCRUB_JOB_CANCELLED);
            return job->state;
        }
        if (!gnc_scrub_context_is_active (job->context))
        {
            gnc_scrub_job_finish (job, GNC_SCRUB_JOB_FAILED);
            return job->state;
        }

        auto transaction = xaccTransLookup (
            &job->transaction_guids[job->cursor], job->book);
        ++job->cursor;
        ++processed;
        if (transaction && !gnc_scrub_job_process_transaction (job, transaction))
        {
            gnc_scrub_job_finish (job, GNC_SCRUB_JOB_FAILED);
            return job->state;
        }
    }

    if (job->cursor == job->transaction_guids.size ())
    {
        if (gnc_scrub_job_advance_phase (job))
            return job->state;
        gnc_scrub_job_finish (job, GNC_SCRUB_JOB_DONE);
    }
    return job->state;
}

void
gnc_scrub_job_cancel (GncScrubJob *job)
{
    if (!job || job->state != GNC_SCRUB_JOB_RUNNING)
        return;

    if (job->gains_child)
        gnc_transaction_gains_plan_cancel (job->gains_child);
    if (job->account_lots_child)
        gnc_account_lots_plan_cancel (job->account_lots_child);
    if (job->lot_child)
        gnc_lot_scrub_plan_cancel (job->lot_child);
    gnc_scrub_context_cancel (job->context);
    gnc_scrub_job_finish (job, GNC_SCRUB_JOB_CANCELLED);
}

GncScrubJobState
gnc_scrub_job_get_state (const GncScrubJob *job)
{
    return job ? job->state : GNC_SCRUB_JOB_FAILED;
}

GncScrubJobKind
gnc_scrub_job_get_kind (const GncScrubJob *job)
{
    return job ? job->kind : GNC_SCRUB_JOB_ORPHANS;
}

GncScrubJobPhase
gnc_scrub_job_get_phase (const GncScrubJob *job)
{
    return job ? job->phase : GNC_SCRUB_JOB_PHASE_ORPHANS;
}

guint
gnc_scrub_job_get_total (const GncScrubJob *job)
{
    if (job && job->kind == GNC_SCRUB_JOB_GAINS)
        return job->gains_completed + gnc_scrub_deferred_commit_pending_count (
            job->context, GNC_SCRUB_DEFERRED_COMMIT_GAINS);
    if (job && (job->kind == GNC_SCRUB_JOB_LOTS ||
                job->kind == GNC_SCRUB_JOB_LOT))
    {
        auto structural_total = job->structural_completed;
        if (job->phase == GNC_SCRUB_JOB_PHASE_LOTS)
            ++structural_total;
        return structural_total + job->gains_completed +
               gnc_scrub_deferred_commit_pending_count (
                   job->context, GNC_SCRUB_DEFERRED_COMMIT_GAINS);
    }
    return job ? static_cast<guint> (job->transaction_guids.size () *
                                     job->phase_count) : 0;
}

guint
gnc_scrub_job_get_completed (const GncScrubJob *job)
{
    if (job && job->kind == GNC_SCRUB_JOB_GAINS)
        return job->gains_completed;
    if (job && (job->kind == GNC_SCRUB_JOB_LOTS ||
                job->kind == GNC_SCRUB_JOB_LOT))
        return job->structural_completed + job->gains_completed;
    return job ? static_cast<guint> (job->cursor +
                                     (job->kind == GNC_SCRUB_JOB_ACCOUNT &&
                                      job->phase == GNC_SCRUB_JOB_PHASE_IMBALANCE
                                      ? job->transaction_guids.size () : 0)) : 0;
}

gboolean
gnc_scrub_job_get_changed (const GncScrubJob *job)
{
    return job && job->structural_changed;
}

void
gnc_scrub_job_free (GncScrubJob *job)
{
    if (!job)
        return;

    gnc_scrub_job_cancel (job);
    gnc_transaction_gains_plan_free (job->gains_child);
    gnc_account_lots_plan_free (job->account_lots_child);
    gnc_lot_scrub_plan_free (job->lot_child);
    gnc_scrub_context_unref (job->context);
    delete job;
}

/* ================================================================ */

static void
TransScrubOrphansFast (Transaction *trans, Account *root,
                       GncScrubContext *context)
{
    g_return_if_fail (trans && trans->common_currency && root);

    for (GList *node = trans->splits; node; node = node->next)
    {
        Split *split = GNC_SPLIT(node->data);
        if (gnc_scrub_context_is_cancelled (context)) break;

        if (split->acc) continue;

        DEBUG ("Found an orphan\n");

        gchar *accname = g_strconcat
            (_("Orphan"), "-", gnc_commodity_get_mnemonic (trans->common_currency),
             nullptr);

        Account *orph = xaccScrubUtilityGetOrMakeAccount
            (root, trans->common_currency, accname, ACCT_TYPE_BANK, false, true);

        g_free (accname);
        if (!orph) continue;

        xaccSplitSetAccount(split, orph);
    }
}

static void
AccountScrubOrphans (Account *acc, bool descendants,
                     QofPercentageFunc percentagefunc,
                     GncScrubContext *context)
{
    if (!acc) return;
    auto book = qof_instance_get_book (QOF_INSTANCE (acc));
    if (context && !gnc_scrub_context_validate_for_book (
                       context, book, "account orphan scrub"))
        return;

    auto transactions = get_all_transactions (acc, descendants);
    auto total_trans = transactions.size();
    const char *message = _("Looking for orphans in transaction: %u of %zu");
    guint current_trans = 0;

    for (auto trans : transactions)
    {
        if (current_trans % 10 == 0)
        {
            char *progress_msg = g_strdup_printf (message, current_trans, total_trans);
            (percentagefunc)(progress_msg, (100 * current_trans) / total_trans);
            g_free (progress_msg);
            if (gnc_scrub_context_is_cancelled (context)) break;
        }

        TransScrubOrphansFast (trans, gnc_account_get_root (acc), context);
        current_trans++;
    }
    (percentagefunc)(nullptr, -1.0);
}

void
xaccAccountScrubOrphans (Account *acc, QofPercentageFunc percentagefunc)
{
    if (!acc || !gnc_scrub_legacy_operation_allowed (
                    qof_instance_get_book (QOF_INSTANCE (acc)),
                    "account orphan scrub"))
        return;
    AccountScrubOrphans (acc, false, percentagefunc, nullptr);
}

void
xaccAccountTreeScrubOrphans (Account *acc, QofPercentageFunc percentagefunc)
{
    if (!acc || !gnc_scrub_legacy_operation_allowed (
                    qof_instance_get_book (QOF_INSTANCE (acc)),
                    "account-tree orphan scrub"))
        return;
    AccountScrubOrphans (acc, true, percentagefunc, nullptr);
}

static void
TransScrubOrphans (Transaction *trans, GncScrubContext *context)
{
    SplitList *node;
    Account *root = nullptr;

    if (!trans) return;
    auto book = xaccTransGetBook (trans);
    if (context && !gnc_scrub_context_validate_for_book (
                       context, book, "transaction orphan scrub"))
        return;

    for (node = trans->splits; node; node = node->next)
    {
        Split *split = GNC_SPLIT(node->data);
        if (gnc_scrub_context_is_cancelled (context)) break;

        if (split->acc)
        {
            TransScrubOrphansFast (trans, gnc_account_get_root(split->acc),
                                   context);
            return;
        }
    }

    /* If we got to here, then *none* of the splits belonged to an
     * account.  Not a happy situation.  We should dig an account
     * out of the book the transaction belongs to.
     * XXX we should probably *always* to this, instead of the above loop!
     */
    PINFO ("Free Floating Transaction!");
    root = gnc_book_get_root_account (book);
    TransScrubOrphansFast (trans, root, context);
}

void
xaccTransScrubOrphans (Transaction *trans)
{
    if (!trans || !gnc_scrub_legacy_operation_allowed (
                      xaccTransGetBook (trans), "transaction orphan scrub"))
        return;
    TransScrubOrphans (trans, nullptr);
}

void
xaccTransScrubOrphansWithContext (Transaction *trans,
                                  GncScrubContext *context)
{
    TransScrubOrphans (trans, context);
}

void
xaccAccountScrubOrphansWithContext (Account *acc,
                                    QofPercentageFunc percentagefunc,
                                    GncScrubContext *context)
{
    AccountScrubOrphans (acc, false, percentagefunc, context);
}

void
xaccAccountTreeScrubOrphansWithContext (Account *acc,
                                        QofPercentageFunc percentagefunc,
                                        GncScrubContext *context)
{
    AccountScrubOrphans (acc, true, percentagefunc, context);
}

/* ================================================================ */

void
xaccAccountScrubSplits (Account *account)
{
    if (!account || !gnc_scrub_legacy_operation_allowed (
                        qof_instance_get_book (QOF_INSTANCE (account)),
                        "account split scrub"))
        return;
    for (auto split : xaccAccountGetSplits (account))
        SplitScrub (split, nullptr);
}

void
xaccAccountTreeScrubSplits (Account *account)
{
    if (!account || !gnc_scrub_legacy_operation_allowed (
                        qof_instance_get_book (QOF_INSTANCE (account)),
                        "account-tree split scrub"))
        return;

    for (auto split : xaccAccountGetSplits (account))
        SplitScrub (split, nullptr);
    gnc_account_foreach_descendant (
        account,
        [] (Account *descendant, gpointer)
        {
            for (auto split : xaccAccountGetSplits (descendant))
                SplitScrub (split, nullptr);
        },
        nullptr);
}

/* if dry_run is true, this function will analyze the split and
   return true if the split will be modified during the actual scrub. */
static bool
split_scrub_or_dry_run (Split *split, bool dry_run,
                        GncScrubContext *context)
{
    Account *account;
    Transaction *trans;
    gnc_numeric value, amount;
    gnc_commodity *currency, *acc_commodity;
    int scu;

    if (!split) return false;
    ENTER ("(split=%p)", split);

    trans = xaccSplitGetParent (split);
    if (!trans)
    {
        LEAVE("no trans");
        return false;
    }

    account = xaccSplitGetAccount (split);

    /* If there's no account, this split is an orphan.
     * We need to fix that first, before proceeding.
     */
    if (!account)
    {
        if (dry_run)
            return true;
        else
            TransScrubOrphans (trans, context);
        account = xaccSplitGetAccount (split);
    }

    /* Grrr... the register gnc_split_register_load() line 203 of
     *  src/register/ledger-core/split-register-load.c will create
     * free-floating bogus transactions. Ignore these for now ...
     */
    if (!account)
    {
        PINFO ("Free Floating Transaction!");
        LEAVE ("no account");
        return false;
    }

    /* Split amounts and values should be valid numbers */
    value = xaccSplitGetValue (split);
    if (gnc_numeric_check (value))
    {
        value = gnc_numeric_zero();
        if (dry_run)
            return true;
        else
            xaccSplitSetValue (split, value);
    }

    amount = xaccSplitGetAmount (split);
    if (gnc_numeric_check (amount))
    {
        amount = gnc_numeric_zero();
        if (dry_run)
            return true;
        else
            xaccSplitSetAmount (split, amount);
    }

    currency = xaccTransGetCurrency (trans);

    /* If the account doesn't have a commodity,
     * we should attempt to fix that first.
     */
    acc_commodity = xaccAccountGetCommodity(account);
    if (!acc_commodity)
    {
        if (dry_run)
            return true;
        else
            AccountScrubCommodity (account);
    }
    if (!acc_commodity || !gnc_commodity_equiv(acc_commodity, currency))
    {
        LEAVE ("(split=%p) inequiv currency", split);
        return false;
    }

    scu = MIN (xaccAccountGetCommoditySCU (account),
               gnc_commodity_get_fraction (currency));

    if (gnc_numeric_same (amount, value, scu, GNC_HOW_RND_ROUND_HALF_UP))
    {
        LEAVE("(split=%p) different values", split);
        return false;
    }

    if (dry_run)
        return true;

    /*
     * This will be hit every time you answer yes to the dialog "The
     * current transaction has changed. Would you like to record it.
     */
    PINFO ("Adjusted split with mismatched values, desc=\"%s\" memo=\"%s\""
           " old amount %s %s, new amount %s",
           trans->description, split->memo,
           gnc_num_dbg_to_string (xaccSplitGetAmount(split)),
           gnc_commodity_get_mnemonic (currency),
           gnc_num_dbg_to_string (xaccSplitGetValue(split)));

    xaccTransBeginEdit (trans);
    xaccSplitSetAmount (split, value);
    xaccTransCommitEdit (trans);
    LEAVE ("(split=%p)", split);
    return true;
}

/* ================================================================ */


static void
AccountScrubImbalance (Account *acc, bool descendants,
                       QofPercentageFunc percentagefunc,
                       GncScrubContext *context)
{
    const char *message = _("Looking for imbalances in transaction date %s: %u of %zu");

    if (!acc) return;

    auto book = qof_instance_get_book (QOF_INSTANCE (acc));
    if (context && !gnc_scrub_context_validate_for_book (
                       context, book, "account imbalance scrub"))
        return;
    Account *root = gnc_book_get_root_account (book);
    auto transactions = get_all_transactions (acc, descendants);
    auto count = transactions.size();
    auto curr_trans = 0;

    for (auto trans : transactions)
    {
        if (gnc_scrub_context_is_cancelled (context)) break;

        PINFO("Start processing transaction %d of %zu", curr_trans + 1, count);

        if (curr_trans % 10 == 0)
        {
            char *date = qof_print_date (xaccTransGetDate (trans));
            char *progress_msg = g_strdup_printf (message, date, curr_trans, count);
            (percentagefunc)(progress_msg, (100 * curr_trans) / count);
            g_free (progress_msg);
            g_free (date);
        }

        TransScrubOrphansFast (trans, root, context);
        TransScrubCurrency (trans, context);
        xaccTransScrubImbalanceInternal (trans, root, nullptr, context);

        PINFO("Finished processing transaction %d of %zu", curr_trans + 1, count);
        curr_trans++;
    }
    (percentagefunc)(nullptr, -1.0);
}

static void
TransScrubSplits (Transaction *trans, GncScrubContext *context)
{
    if (!trans) return;

    gnc_commodity *currency = xaccTransGetCurrency (trans);
    if (!currency)
        PERR ("Transaction doesn't have a currency!");

    bool must_scrub = false;

    for (GList *n = xaccTransGetSplitList (trans); !must_scrub && n; n = g_list_next (n))
        if (split_scrub_or_dry_run (GNC_SPLIT(n->data), true, context))
            must_scrub = true;

    if (!must_scrub)
        return;

    xaccTransBeginEdit(trans);
    /* The split scrub expects the transaction to have a currency! */

    for (GList *n = xaccTransGetSplitList (trans); n; n = g_list_next (n))
        SplitScrub (GNC_SPLIT(n->data), context);

    xaccTransCommitEdit(trans);
}

/* ================================================================ */

void
xaccTransScrubSplits (Transaction *trans)
{
    if (!trans || !gnc_scrub_legacy_operation_allowed (
                      xaccTransGetBook (trans), "transaction split scrub"))
        return;
    TransScrubSplits (trans, nullptr);
}

static void
SplitScrub (Split *split, GncScrubContext *context)
{
    split_scrub_or_dry_run (split, false, context);
}

void
xaccSplitScrub (Split *split)
{
    if (!split || !gnc_scrub_legacy_operation_allowed (
                      qof_instance_get_book (QOF_INSTANCE (split)),
                      "split scrub"))
        return;
    SplitScrub (split, nullptr);
}

/* ================================================================ */


void
xaccAccountTreeScrubImbalance (Account *acc, QofPercentageFunc percentagefunc)
{
    if (!acc || !gnc_scrub_legacy_operation_allowed (
                    qof_instance_get_book (QOF_INSTANCE (acc)),
                    "account-tree imbalance scrub"))
        return;
    AccountScrubImbalance (acc, true, percentagefunc, nullptr);
}

void
xaccAccountScrubImbalance (Account *acc, QofPercentageFunc percentagefunc)
{
    if (!acc || !gnc_scrub_legacy_operation_allowed (
                    qof_instance_get_book (QOF_INSTANCE (acc)),
                    "account imbalance scrub"))
        return;
    AccountScrubImbalance (acc, false, percentagefunc, nullptr);
}

void
xaccAccountScrubImbalanceWithContext (Account *acc,
                                      QofPercentageFunc percentagefunc,
                                      GncScrubContext *context)
{
    AccountScrubImbalance (acc, false, percentagefunc, context);
}

void
xaccAccountTreeScrubImbalanceWithContext (Account *acc,
                                          QofPercentageFunc percentagefunc,
                                          GncScrubContext *context)
{
    AccountScrubImbalance (acc, true, percentagefunc, context);
}

static Split *
get_balance_split (Transaction *trans, Account *root, Account *account,
                   gnc_commodity *commodity)
{
    Split *balance_split;
    gchar *accname;

    if (!account ||
        !gnc_commodity_equiv (commodity, xaccAccountGetCommodity(account)))
    {
        if (!root)
        {
            root = gnc_book_get_root_account (xaccTransGetBook (trans));
            if (nullptr == root)
            {
                /* This can't occur, things should be in books */
                PERR ("Bad data corruption, no root account in book");
                return nullptr;
            }
        }
        accname = g_strconcat (_("Imbalance"), "-",
                               gnc_commodity_get_mnemonic (commodity), nullptr);
        account = xaccScrubUtilityGetOrMakeAccount (root, commodity,
                                                    accname, ACCT_TYPE_BANK,
                                                    FALSE, TRUE);
        g_free (accname);
        if (!account)
        {
            PERR ("Can't get balancing account");
            return nullptr;
        }
    }

    balance_split = xaccTransFindSplitByAccount(trans, account);

    /* Put split into account before setting split value */
    if (!balance_split)
    {
        balance_split = xaccMallocSplit (qof_instance_get_book(trans));

        xaccTransBeginEdit (trans);
        xaccSplitSetParent(balance_split, trans);
        xaccSplitSetAccount(balance_split, account);
        xaccTransCommitEdit (trans);
    }

    return balance_split;
}

static gnc_commodity*
find_root_currency(void)
{
    QofSession *sess = gnc_get_current_session ();
    Account *root = gnc_book_get_root_account (qof_session_get_book (sess));
    gnc_commodity *root_currency = xaccAccountGetCommodity (root);

    /* Some older books may not have a currency set on the root
     * account. In that case find the first top-level INCOME account
     * and use its currency. */
    if (!root_currency)
    {
         GList *children = gnc_account_get_children (root);
         for (GList *node = children; node && !root_currency;
              node = g_list_next (node))
         {
              Account *child = GNC_ACCOUNT (node->data);
              if (xaccAccountGetType (child) == ACCT_TYPE_INCOME)
                   root_currency = xaccAccountGetCommodity (child);
         }
         g_list_free (children);
    }
    return root_currency;
}

/* Get the trading split for a given commodity, creating it (and the
   necessary parent accounts) if it doesn't exist. */
static Split *
get_trading_split (Transaction *trans, Account *base,
                   gnc_commodity *commodity)
{
    Split *balance_split;
    Account *trading_account;
    Account *ns_account;
    Account *account;
    Account* root = gnc_book_get_root_account (xaccTransGetBook (trans));

    trading_account = xaccScrubUtilityGetOrMakeAccount (root,
                                                        nullptr,
                                                        _("Trading"),
                                                        ACCT_TYPE_TRADING,
                                                        TRUE, FALSE);
    if (!trading_account)
    {
        PERR ("Can't get trading account");
        return nullptr;
    }

    ns_account = xaccScrubUtilityGetOrMakeAccount (trading_account,
                                                   nullptr,
                                                   gnc_commodity_get_namespace(commodity),
                                                   ACCT_TYPE_TRADING,
                                                   TRUE, TRUE);
    if (!ns_account)
    {
        PERR ("Can't get namespace account");
        return nullptr;
    }

    account = xaccScrubUtilityGetOrMakeAccount (ns_account, commodity,
                                                gnc_commodity_get_mnemonic(commodity),
                                                ACCT_TYPE_TRADING,
                                                FALSE, FALSE);
    if (!account)
    {
        PERR ("Can't get commodity account");
        return nullptr;
    }


    balance_split = xaccTransFindSplitByAccount(trans, account);

    /* Put split into account before setting split value */
    if (!balance_split)
    {
        balance_split = xaccMallocSplit (qof_instance_get_book(trans));
        xaccDisableDataScrubbing();

        xaccTransBeginEdit (trans);
        xaccSplitSetParent(balance_split, trans);
        xaccSplitSetAccount(balance_split, account);
        xaccTransCommitEdit (trans);
        xaccEnableDataScrubbing();
    }

    return balance_split;
}

static void
add_balance_split (Transaction *trans, gnc_numeric imbalance,
                   Account *root, Account *account,
                   GncScrubContext *context)
{
    const gnc_commodity *commodity;
    gnc_numeric old_value, new_value;
    Split *balance_split;
    gnc_commodity *currency = xaccTransGetCurrency (trans);

    balance_split = get_balance_split(trans, root, account, currency);
    if (!balance_split)
    {
        /* Error already logged */
        LEAVE("");
        return;
    }

    old_value = xaccSplitGetValue (balance_split);

    /* Note: We have to round for the commodity's fraction, NOT any
     * already existing denominator (bug #104343), because either one
     * of the denominators might already be reduced.  */
    new_value = gnc_numeric_sub (old_value, imbalance,
                                 gnc_commodity_get_fraction(currency),
                                 GNC_HOW_RND_ROUND_HALF_UP);

    if (gnc_numeric_zero_p (new_value))
    {
        const char *p;
        p = xaccSplitGetMemo (balance_split);
        if (!p || !*p)
        {
            p = xaccSplitGetAction (balance_split);
            if (!p || !*p)
            {
                xaccSplitDestroy (balance_split);
                return;
            }
        }
    }

    xaccTransBeginEdit (trans);
    xaccSplitSetValue (balance_split, new_value);
        
    account = xaccSplitGetAccount(balance_split);
    commodity = xaccAccountGetCommodity (account);
    if (gnc_commodity_equiv (currency, commodity))
    {
        xaccSplitSetAmount (balance_split, new_value);
    }

    SplitScrub (balance_split, context);
    xaccTransCommitEdit (trans);
}

/* Balance a transaction without trading accounts. */
static void
gnc_transaction_balance_no_trading (Transaction *trans, Account *root,
                                    Account *account,
                                    GncScrubContext *context)
{
    gnc_numeric imbalance  = xaccTransGetImbalanceValue (trans);

    /* Make the value sum to zero */
    if (! gnc_numeric_zero_p (imbalance))
    {
        PINFO ("Value unbalanced transaction");

        add_balance_split (trans, imbalance, root, account, context);
    }

}

static gnc_numeric
gnc_transaction_get_commodity_imbalance (Transaction *trans,
                                         gnc_commodity *commodity)
{
    /* Find the value imbalance in this commodity */
    gnc_numeric val_imbalance = gnc_numeric_zero();
    GList *splits = nullptr;
    for (splits = trans->splits; splits; splits = splits->next)
    {
        Split *split = GNC_SPLIT(splits->data);
        gnc_commodity *split_commodity =
            xaccAccountGetCommodity(xaccSplitGetAccount(split));
        if (xaccTransStillHasSplit (trans, split) &&
            gnc_commodity_equal (commodity, split_commodity))
            val_imbalance = gnc_numeric_add (val_imbalance,
                                             xaccSplitGetValue (split),
                                             GNC_DENOM_AUTO,
                                             GNC_HOW_DENOM_EXACT);
    }
    return val_imbalance;
}

/* GFunc wrapper for xaccSplitDestroy */
static void
destroy_split (void* ptr)
{
    Split *split = GNC_SPLIT (ptr);
    if (split)
        xaccSplitDestroy (split);
}

/* Balancing transactions with trading accounts works best when
 * starting with no trading splits.
 */
static void
xaccTransClearTradingSplits (Transaction *trans)
{
    GList *trading_splits = nullptr;

    for (GList* node = trans->splits; node; node = node->next)
    {
         Split* split = GNC_SPLIT(node->data);
         Account* acc = nullptr;
         if (!split)
              continue;
         acc = xaccSplitGetAccount(split);
         if (acc && xaccAccountGetType(acc) == ACCT_TYPE_TRADING)
            trading_splits = g_list_prepend (trading_splits, node->data);
    }

    if (!trading_splits)
        return;

    xaccTransBeginEdit (trans);
    /* destroy_splits doesn't actually free the splits but this gets
     * the list itself freed.
     */
    g_list_free_full (trading_splits, destroy_split);
    xaccTransCommitEdit (trans);
}

static void
gnc_transaction_balance_trading (Transaction *trans, Account *root,
                                 GncScrubContext *context)
{
    MonetaryList *imbal_list;
    MonetaryList *imbalance_commod;
    Split *balance_split = nullptr;

    /* If the transaction is balanced, nothing more to do */
    imbal_list = xaccTransGetImbalance (trans);
    if (!imbal_list)
    {
        LEAVE("transaction is balanced");
        return;
    }

    PINFO ("Currency unbalanced transaction");

    for (imbalance_commod = imbal_list; imbalance_commod;
         imbalance_commod = imbalance_commod->next)
    {
        auto imbal_mon = static_cast<gnc_monetary*>(imbalance_commod->data);
        gnc_commodity *commodity;
        gnc_numeric old_amount, new_amount;
        const gnc_commodity *txn_curr = xaccTransGetCurrency (trans);

        commodity = gnc_monetary_commodity (*imbal_mon);

        balance_split = get_trading_split(trans, root, commodity);
        if (!balance_split)
        {
            /* Error already logged */
            gnc_monetary_list_free(imbal_list);
            LEAVE("");
            return;
        }

        xaccTransBeginEdit (trans);

        old_amount = xaccSplitGetAmount (balance_split);
        new_amount = gnc_numeric_sub (old_amount, gnc_monetary_value(*imbal_mon),
                                      gnc_commodity_get_fraction(commodity),
                                      GNC_HOW_RND_ROUND_HALF_UP);

        xaccSplitSetAmount (balance_split, new_amount);

        if (gnc_commodity_equal (txn_curr, commodity))
        {
            /* Imbalance commodity is the transaction currency, value in the
               split must be the same as the amount */
            xaccSplitSetValue (balance_split, new_amount);
        }
        else
        {
            gnc_numeric val_imbalance = gnc_transaction_get_commodity_imbalance (trans,            commodity);

            gnc_numeric old_value = xaccSplitGetValue (balance_split);
            gnc_numeric new_value = gnc_numeric_sub (old_value, val_imbalance,
                                         gnc_commodity_get_fraction(txn_curr),
                                         GNC_HOW_RND_ROUND_HALF_UP);

            xaccSplitSetValue (balance_split, new_value);
        }

        SplitScrub (balance_split, context);
        xaccTransCommitEdit (trans);
    }

    gnc_monetary_list_free(imbal_list);
}

/** Balance the transaction by adding more trading splits. This shouldn't
 * ordinarily be necessary.
 * @param trans the transaction to balance
 * @param root the root account
 */
static void
gnc_transaction_balance_trading_more_splits (Transaction *trans, Account *root,
                                             GncScrubContext *context)
{
    /* Copy the split list so we don't see the splits we're adding */
    GList *splits_dup = g_list_copy(trans->splits), *splits = nullptr;
    const gnc_commodity  *txn_curr = xaccTransGetCurrency (trans);
    for (splits = splits_dup; splits; splits = splits->next)
    {
        Split *split = GNC_SPLIT(splits->data);
        if (! xaccTransStillHasSplit(trans, split)) continue;
        if (!gnc_numeric_zero_p(xaccSplitGetValue(split)) &&
            gnc_numeric_zero_p(xaccSplitGetAmount(split)))
        {
            gnc_commodity *commodity;
            gnc_numeric old_value, new_value;
            Split *balance_split;

            commodity = xaccAccountGetCommodity(xaccSplitGetAccount(split));
            if (!commodity)
            {
                PERR("Split has no commodity");
                continue;
            }
            balance_split = get_trading_split(trans, root, commodity);
            if (!balance_split)
            {
                /* Error already logged */
                LEAVE("");
                return;
            }
            xaccTransBeginEdit (trans);

            old_value = xaccSplitGetValue (balance_split);
            new_value = gnc_numeric_sub (old_value, xaccSplitGetValue(split),
                                         gnc_commodity_get_fraction(txn_curr),
                                         GNC_HOW_RND_ROUND_HALF_UP);
            xaccSplitSetValue (balance_split, new_value);

            /* Don't change the balance split's amount since the amount
               is zero in the split we're working on */

            SplitScrub (balance_split, context);
            xaccTransCommitEdit (trans);
        }
    }

    g_list_free(splits_dup);
}

/** Correct transaction imbalances.
 * @param trans The Transaction
 * @param root The (hidden) root account, for the book default currency.
 * @param account The account whose currency in which to balance.
 */

void
xaccTransScrubImbalanceInternal (Transaction *trans, Account *root,
                                 Account *account,
                                 GncScrubContext *context)
{
    gnc_numeric imbalance;

    if (!trans) return;
    auto book = xaccTransGetBook (trans);
    if (context && !gnc_scrub_context_validate_for_book (
                       context, book, "transaction imbalance scrub"))
        return;
    if (gnc_scrub_context_is_cancelled (context))
        return;

    ENTER ("()");

    /* Must look for orphan splits even if there is no imbalance. */
    TransScrubSplits (trans, context);

    /* Return immediately if things are balanced. */
    if (xaccTransIsBalanced (trans))
    {
        LEAVE ("transaction is balanced");
        return;
    }

    if (! xaccTransUseTradingAccounts (trans))
    {
        gnc_transaction_balance_no_trading (trans, root, account, context);
        LEAVE ("transaction balanced, no managed trading accounts");
        return;
    }

    xaccTransClearTradingSplits (trans);
    imbalance = xaccTransGetImbalanceValue (trans);
    if (! gnc_numeric_zero_p (imbalance))
    {
        PINFO ("Value unbalanced transaction");

        add_balance_split (trans, imbalance, root, account, context);
    }

    gnc_transaction_balance_trading (trans, root, context);
    if (gnc_numeric_zero_p(xaccTransGetImbalanceValue(trans)))
    {
        LEAVE ("()");
        return;
    }
    /* If the transaction is still not balanced, it's probably because there
       are splits with zero amount and non-zero value.  These are usually
       realized gain/loss splits.  Add a reversing split for each of them to
       balance the value. */

    gnc_transaction_balance_trading_more_splits (trans, root, context);
    if (!gnc_numeric_zero_p(xaccTransGetImbalanceValue(trans)))
        PERR("Balancing currencies unbalanced value");

}

void
xaccTransScrubImbalance (Transaction *trans, Account *root, Account *account)
{
    if (!trans || !gnc_scrub_legacy_operation_allowed (
                      xaccTransGetBook (trans), "transaction imbalance scrub"))
        return;
    xaccTransScrubImbalanceInternal (trans, root, account, nullptr);
}

void
xaccTransScrubImbalanceWithContext (Transaction *trans, Account *root,
                                    Account *account,
                                    GncScrubContext *context)
{
    xaccTransScrubImbalanceInternal (trans, root, account, context);
}

/* ================================================================ */
/* The xaccTransFindCommonCurrency () method returns
 *    a gnc_commodity indicating a currency denomination that all
 *    of the splits in this transaction have in common, using the
 *    old/obsolete currency/security fields of the split accounts.
 */

static gnc_commodity *
FindCommonExclSCurrency (SplitList *splits,
                         gnc_commodity * ra, gnc_commodity * rb,
                         Split *excl_split)
{
    GList *node;

    if (!splits) return nullptr;

    for (node = splits; node; node = node->next)
    {
        Split *s = GNC_SPLIT(node->data);
        gnc_commodity * sa, * sb;

        if (s == excl_split) continue;

        g_return_val_if_fail (s->acc, nullptr);

        sa = DxaccAccountGetCurrency (s->acc);
        sb = xaccAccountGetCommodity (s->acc);

        if (ra && rb)
        {
            int aa = !gnc_commodity_equiv(ra, sa);
            int ab = !gnc_commodity_equiv(ra, sb);
            int ba = !gnc_commodity_equiv(rb, sa);
            int bb = !gnc_commodity_equiv(rb, sb);

            if ( (!aa) && bb) rb = nullptr;
            else if ( (!ab) && ba) rb = nullptr;
            else if ( (!ba) && ab) ra = nullptr;
            else if ( (!bb) && aa) ra = nullptr;
            else if ( aa && bb && ab && ba )
            {
                ra = nullptr;
                rb = nullptr;
            }

            if (!ra)
            {
                ra = rb;
                rb = nullptr;
            }
        }
        else if (ra && !rb)
        {
            int aa = !gnc_commodity_equiv(ra, sa);
            int ab = !gnc_commodity_equiv(ra, sb);
            if ( aa && ab ) ra = nullptr;
        }
        else if (!ra && rb)
        {
            int aa = !gnc_commodity_equiv(rb, sa);
            int ab = !gnc_commodity_equiv(rb, sb);
            ra = ( aa && ab ) ? nullptr : rb;
        }

        if ((!ra) && (!rb)) return nullptr;
    }

    return (ra);
}

/* This is the wrapper for those calls (i.e. the older ones) which
 * don't exclude one split from the splitlist when looking for a
 * common currency.
 */
static gnc_commodity *
FindCommonCurrency (GList *splits, gnc_commodity * ra, gnc_commodity * rb)
{
    return FindCommonExclSCurrency(splits, ra, rb, nullptr);
}

static gnc_commodity *
xaccTransFindOldCommonCurrency (Transaction *trans, QofBook *book)
{
    gnc_commodity *ra, *rb, *retval;
    Split *split;

    if (!trans) return nullptr;

    if (trans->splits == nullptr) return nullptr;

    g_return_val_if_fail (book, nullptr);

    split = GNC_SPLIT(trans->splits->data);

    if (!split || nullptr == split->acc) return nullptr;

    ra = DxaccAccountGetCurrency (split->acc);
    rb = xaccAccountGetCommodity (split->acc);

    retval = FindCommonCurrency (trans->splits, ra, rb);

    if (retval && !gnc_commodity_is_currency(retval))
        retval = nullptr;

    return retval;
}

/* Test the currency of the splits and find the most common and return
 * it, or nullptr if there is no currency more common than the
 * others -- or none at all.
 */
typedef struct
{
    gnc_commodity *commodity;
    unsigned int count;
} CommodityCount;

static gint
commodity_equal (gconstpointer a, gconstpointer b)
{
    CommodityCount *cc = (CommodityCount*)a;
    gnc_commodity *com = (gnc_commodity*)b;
    if ( cc == nullptr || cc->commodity == nullptr ||
         !GNC_IS_COMMODITY( cc->commodity ) ) return -1;
    if ( com == nullptr || !GNC_IS_COMMODITY( com ) ) return 1;
    if ( gnc_commodity_equal(cc->commodity, com) )
        return 0;
    return 1;
}

static gint
commodity_compare( gconstpointer a, gconstpointer b)
{
    CommodityCount *ca = (CommodityCount*)a, *cb = (CommodityCount*)b;
    if (ca == nullptr || ca->commodity == nullptr ||
        !GNC_IS_COMMODITY( ca->commodity ) )
    {
        if (cb == nullptr || cb->commodity == nullptr ||
            !GNC_IS_COMMODITY( cb->commodity ) )
            return 0;
        return -1;
    }
    if (cb == nullptr || cb->commodity == nullptr ||
        !GNC_IS_COMMODITY( cb->commodity ) )
        return 1;
    if (ca->count == cb->count)
        return 0;
    return ca->count > cb->count ? 1 : -1;
}

/* Find the commodities in the account of each of the splits of a
 * transaction, and rank them by how many splits in which they
 * occur. Commodities which are currencies count more than those which
 * aren't, because for simple buy and sell transactions it makes
 * slightly more sense for the transaction commodity to be the
 * currency -- to the extent that it makes sense for a transaction to
 * have a currency at all. jralls, 2010-11-02 */

static gnc_commodity *
xaccTransFindCommonCurrency (Transaction *trans, QofBook *book)
{
    gnc_commodity *com_scratch;
    GList *node = nullptr;
    GSList *comlist = nullptr, *found = nullptr;

    if (!trans) return nullptr;

    if (trans->splits == nullptr) return nullptr;

    g_return_val_if_fail (book, nullptr);

    /* Find the most commonly used currency among the splits.  If a given split
       is in a non-currency commodity, then look for an ancestor account in a
       currency, but prefer currencies used directly in splits.  Ignore trading
       account splits in this whole process, they don't add any value to this algorithm. */
    for (node = trans->splits; node; node = node->next)
    {
        Split *s = GNC_SPLIT(node->data);
        unsigned int curr_weight;

        if (s == nullptr || s->acc == nullptr) continue;
        if (xaccAccountGetType(s->acc) == ACCT_TYPE_TRADING) continue;
        com_scratch = xaccAccountGetCommodity(s->acc);
        if (com_scratch && gnc_commodity_is_currency(com_scratch))
        {
            curr_weight = 3;
        }
        else
        {
            com_scratch = gnc_account_get_currency_or_parent(s->acc);
            if (com_scratch == nullptr) continue;
            curr_weight = 1;
        }
        if ( comlist )
        {
            found = g_slist_find_custom(comlist, com_scratch, commodity_equal);
        }
        if (comlist == nullptr || found == nullptr)
        {
            CommodityCount *count = g_slice_new0(CommodityCount);
            count->commodity = com_scratch;
            count->count = curr_weight;
            comlist = g_slist_append(comlist, count);
        }
        else
        {
            CommodityCount *count = (CommodityCount*)(found->data);
            count->count += curr_weight;
        }
    }
    found = g_slist_sort( comlist, commodity_compare);

    if ( found && found->data && (((CommodityCount*)(found->data))->commodity != nullptr))
    {
        return ((CommodityCount*)(found->data))->commodity;
    }
    /* We didn't find a currency in the current account structure, so try
     * an old one. */
    return xaccTransFindOldCommonCurrency( trans, book );
}

/* ================================================================ */

static void
TransScrubCurrency (Transaction *trans, GncScrubContext *context)
{
    SplitList *node;
    gnc_commodity *currency;

    if (!trans) return;

    /* If there are any orphaned splits in a transaction, then the
     * this routine will fail.  Therefore, we want to make sure that
     * there are no orphans (splits without parent account).
     */
    TransScrubOrphans (trans, context);

    currency = xaccTransGetCurrency (trans);
    if (currency && gnc_commodity_is_currency(currency)) return;

    currency = xaccTransFindCommonCurrency (trans, qof_instance_get_book(trans));
    if (currency)
    {
        xaccTransBeginEdit (trans);
        xaccTransSetCurrency (trans, currency);
        xaccTransCommitEdit (trans);
    }
    else
    {
        if (nullptr == trans->splits)
        {
            PWARN ("Transaction \"%s\" has no splits in it!", trans->description);
        }
        else
        {
            SplitList *node;
            char guid_str[GUID_ENCODING_LENGTH + 1];
            guid_to_string_buff(xaccTransGetGUID(trans), guid_str);
            PWARN ("no common transaction currency found for trans=\"%s\" (%s);",
                   trans->description, guid_str);

            for (node = trans->splits; node; node = node->next)
            {
                Split *split = GNC_SPLIT(node->data);
                if (nullptr == split->acc)
                {
                    PWARN (" split=\"%s\" is not in any account!", split->memo);
                }
                else
                {
                    gnc_commodity *currency = xaccAccountGetCommodity(split->acc);
                    PWARN ("setting to split=\"%s\" account=\"%s\" commodity=\"%s\"",
                           split->memo, xaccAccountGetName(split->acc),
                           gnc_commodity_get_mnemonic(currency));

                    xaccTransBeginEdit (trans);
                    xaccTransSetCurrency (trans, currency);
                    xaccTransCommitEdit (trans);
                    return;
                }
            }
        }
        return;
    }

    for (node = trans->splits; node; node = node->next)
    {
        Split *sp = GNC_SPLIT(node->data);

        if (!gnc_numeric_equal(xaccSplitGetAmount (sp),
                               xaccSplitGetValue (sp)))
        {
            gnc_commodity *acc_currency;

            acc_currency = sp->acc ? xaccAccountGetCommodity(sp->acc) : nullptr;
            if (acc_currency == currency)
            {
                /* This Split needs fixing: The transaction-currency equals
                 * the account-currency/commodity, but the amount/values are
                 * inequal i.e. they still correspond to the security
                 * (amount) and the currency (value). In the new model, the
                 * value is the amount in the account-commodity -- so it
                 * needs to be set to equal the amount (since the
                 * account-currency doesn't exist anymore).
                 *
                 * Note: Nevertheless we lose some information here. Namely,
                 * the information that the 'amount' in 'account-old-security'
                 * was worth 'value' in 'account-old-currency'. Maybe it would
                 * be better to store that information in the price database?
                 * But then, for old currency transactions there is still the
                 * 'other' transaction, which is going to keep that
                 * information. So I don't bother with that here. -- cstim,
                 * 2002/11/20. */

                PWARN ("Adjusted split with mismatched values, desc=\"%s\" memo=\"%s\""
                       " old amount %s %s, new amount %s",
                       trans->description, sp->memo,
                       gnc_num_dbg_to_string (xaccSplitGetAmount(sp)),
                       gnc_commodity_get_mnemonic (currency),
                       gnc_num_dbg_to_string (xaccSplitGetValue(sp)));
                xaccTransBeginEdit (trans);
                xaccSplitSetAmount (sp, xaccSplitGetValue(sp));
                xaccTransCommitEdit (trans);
            }
            /*else
              {
              PINFO ("Ok: Split '%s' Amount %s %s, value %s %s",
              xaccSplitGetMemo (sp),
              gnc_num_dbg_to_string (amount),
              gnc_commodity_get_mnemonic (currency),
              gnc_num_dbg_to_string (value),
              gnc_commodity_get_mnemonic (acc_currency));
              }*/
        }
    }

}

void
xaccTransScrubCurrency (Transaction *trans)
{
    if (!trans || !gnc_scrub_legacy_operation_allowed (
                      xaccTransGetBook (trans), "transaction currency scrub"))
        return;
    TransScrubCurrency (trans, nullptr);
}

/* ================================================================ */

static void
AccountScrubCommodity (Account *account)
{
    gnc_commodity *commodity;

    if (!account) return;
    if (xaccAccountGetType(account) == ACCT_TYPE_ROOT) return;

    commodity = xaccAccountGetCommodity (account);
    if (commodity) return;

    /* Use the 'obsolete' routines to try to figure out what the
     * account commodity should have been. */
    commodity = xaccAccountGetCommodity (account);
    if (commodity)
    {
        xaccAccountSetCommodity (account, commodity);
        return;
    }

    commodity = DxaccAccountGetCurrency (account);
    if (commodity)
    {
        xaccAccountSetCommodity (account, commodity);
        return;
    }

    PERR ("Account \"%s\" does not have a commodity!",
          xaccAccountGetName(account));
}

void
xaccAccountScrubCommodity (Account *account)
{
    if (!account || !gnc_scrub_legacy_operation_allowed (
                        qof_instance_get_book (QOF_INSTANCE (account)),
                        "account commodity scrub"))
        return;
    AccountScrubCommodity (account);
}

/* ================================================================ */

/* EFFECTIVE FRIEND FUNCTION declared in qofinstance-p.h */
extern void qof_instance_set_dirty (QofInstance*);

static void
xaccAccountDeleteOldData (Account *account)
{
    if (!account) return;
    xaccAccountBeginEdit (account);
    qof_instance_set_kvp (QOF_INSTANCE (account), nullptr, 1, "old-currency");
    qof_instance_set_kvp (QOF_INSTANCE (account), nullptr, 1, "old-security");
    qof_instance_set_kvp (QOF_INSTANCE (account), nullptr, 1, "old-currency-scu");
    qof_instance_set_kvp (QOF_INSTANCE (account), nullptr, 1, "old-security-scu");
    qof_instance_set_dirty (QOF_INSTANCE (account));
    xaccAccountCommitEdit (account);
}

static int
scrub_trans_currency_helper (Transaction *t, gpointer data)
{
    xaccTransScrubCurrency (t);
    return 0;
}

static void
scrub_account_commodity_helper (Account *account, gpointer data)
{
    AccountScrubCommodity (account);
    xaccAccountDeleteOldData (account);
}

void
xaccAccountTreeScrubCommodities (Account *acc)
{
    if (!acc || !gnc_scrub_legacy_operation_allowed (
                    qof_instance_get_book (QOF_INSTANCE (acc)),
                    "account-tree commodity scrub"))
        return;
    xaccAccountTreeForEachTransaction (acc, scrub_trans_currency_helper, nullptr);

    scrub_account_commodity_helper (acc, nullptr);
    gnc_account_foreach_descendant (acc, scrub_account_commodity_helper, nullptr);
}

/* ================================================================ */

static gboolean
check_quote_source (gnc_commodity *com, gpointer data)
{
    gboolean *commodity_has_quote_src = (gboolean *)data;
    if (com && !gnc_commodity_is_iso(com))
        *commodity_has_quote_src |= gnc_commodity_get_quote_flag(com);
    return TRUE;
}

static void
move_quote_source (Account *account, gpointer data)
{
    gnc_commodity *com;
    gnc_quote_source *quote_source;
    gboolean new_style = GPOINTER_TO_INT(data);
    const char *source, *tz;

    com = xaccAccountGetCommodity(account);
    if (!com)
        return;

    if (!new_style)
    {
        source = dxaccAccountGetPriceSrc(account);
        if (!source || !*source)
            return;
        tz = dxaccAccountGetQuoteTZ(account);

        PINFO("to %8s from %s", gnc_commodity_get_mnemonic(com),
              xaccAccountGetName(account));
        gnc_commodity_set_quote_flag(com, TRUE);
        quote_source = gnc_quote_source_lookup_by_internal(source);
        if (!quote_source)
            quote_source = gnc_quote_source_add_new(source, FALSE);
        gnc_commodity_set_quote_source(com, quote_source);
        gnc_commodity_set_quote_tz(com, tz);
    }

    dxaccAccountSetPriceSrc(account, nullptr);
    dxaccAccountSetQuoteTZ(account, nullptr);
    return;
}


void
xaccAccountTreeScrubQuoteSources (Account *root, gnc_commodity_table *table)
{
    gboolean new_style = FALSE;
    ENTER(" ");

    if (!root || !table)
    {
        LEAVE("Oops");
        return;
    }
    if (!gnc_scrub_legacy_operation_allowed (
            qof_instance_get_book (QOF_INSTANCE (root)),
            "account-tree quote-source scrub"))
        return;
    gnc_commodity_table_foreach_commodity (table, check_quote_source, &new_style);

    move_quote_source(root, GINT_TO_POINTER(new_style));
    gnc_account_foreach_descendant (root, move_quote_source,
                                    GINT_TO_POINTER(new_style));
    LEAVE("Migration done");
}

/* ================================================================ */

void
xaccAccountScrubKvp (Account *account)
{
    GValue v = G_VALUE_INIT;
    gchar *str2;

    if (!account || !gnc_scrub_legacy_operation_allowed (
                        qof_instance_get_book (QOF_INSTANCE (account)),
                        "account KVP scrub"))
        return;

    qof_instance_get_kvp (QOF_INSTANCE (account), &v, 1, "notes");
    if (G_VALUE_HOLDS_STRING (&v))
    {
        str2 = g_strstrip(g_value_dup_string(&v));
        if (strlen(str2) == 0)
            qof_instance_slot_delete (QOF_INSTANCE (account), "notes");
        g_free(str2);
    }

    qof_instance_get_kvp (QOF_INSTANCE (account), &v, 1, "placeholder");
    if ((G_VALUE_HOLDS_STRING (&v) &&
        strcmp(g_value_get_string (&v), "false") == 0) ||
        (G_VALUE_HOLDS_BOOLEAN (&v) && ! g_value_get_boolean (&v)))
        qof_instance_slot_delete (QOF_INSTANCE (account), "placeholder");

    g_value_unset (&v);
    qof_instance_slot_delete_if_empty (QOF_INSTANCE (account), "hbci");
}

/* ================================================================ */

void
xaccAccountScrubColorNotSet (QofBook *book)
{
    GValue value_s = G_VALUE_INIT;
    gboolean already_scrubbed;

    // get the run-once value
    qof_instance_get_kvp (QOF_INSTANCE (book), &value_s, 1, "remove-color-not-set-slots");

    already_scrubbed = (G_VALUE_HOLDS_STRING (&value_s) &&
                        !g_strcmp0 (g_value_get_string (&value_s), "true"));
    g_value_unset (&value_s);

    if (already_scrubbed)
        return;
    else
    {
        GValue value_b = G_VALUE_INIT;
        Account *root = gnc_book_get_root_account (book);
        GList *accts = gnc_account_get_descendants_sorted (root);
        GList *ptr;

        for (ptr = accts; ptr; ptr = g_list_next (ptr))
        {
            auto acct = GNC_ACCOUNT(ptr->data);
            auto color = xaccAccountGetColor (acct);

            if (g_strcmp0 (color, "Not Set") == 0)
                xaccAccountSetColor (acct, "");
        }
        g_list_free (accts);

        g_value_init (&value_b, G_TYPE_BOOLEAN);
        g_value_set_boolean (&value_b, TRUE);

        // set the run-once value
        qof_instance_set_kvp (QOF_INSTANCE (book),  &value_b, 1, "remove-color-not-set-slots");
        g_value_unset (&value_b);
    }
}

/* ================================================================ */

static Account*
construct_account (Account *root, gnc_commodity *currency, const char *accname,
                   GNCAccountType acctype, gboolean placeholder)
{
    gnc_commodity* root_currency = find_root_currency ();
    Account *acc = xaccMallocAccount(gnc_account_get_book (root));
    xaccAccountBeginEdit (acc);
    if (accname && *accname)
         xaccAccountSetName (acc, accname);
    if (currency || root_currency)
         xaccAccountSetCommodity (acc, currency ? currency : root_currency);
    xaccAccountSetType (acc, acctype);
    xaccAccountSetPlaceholder (acc, placeholder);

    /* Hang the account off the root. */
    gnc_account_append_child (root, acc);
    xaccAccountCommitEdit (acc);
    return acc;
}

static Account*
find_root_currency_account_in_list (GList *acc_list)
{
    gnc_commodity* root_currency = find_root_currency();
    for (GList *node = acc_list; node; node = g_list_next (node))
    {
        Account *acc = GNC_ACCOUNT (node->data);
        gnc_commodity *acc_commodity = nullptr;
        if (G_UNLIKELY (!acc)) continue;
        acc_commodity = xaccAccountGetCommodity(acc);
        if (gnc_commodity_equiv (acc_commodity, root_currency))
            return acc;
    }

    return nullptr;
}

static Account*
find_account_matching_name_in_list (GList *acc_list, const char* accname)
{
    for (GList* node = acc_list; node; node = g_list_next(node))
    {
        Account *acc = GNC_ACCOUNT (node->data);
        if (G_UNLIKELY (!acc)) continue;
        if (g_strcmp0 (accname, xaccAccountGetName (acc)) == 0)
            return acc;
    }
    return nullptr;
}

Account *
xaccScrubUtilityGetOrMakeAccount (Account *root, gnc_commodity * currency,
                                  const char *accname, GNCAccountType acctype,
                                  gboolean placeholder, gboolean checkname)
{
    GList* acc_list;
    Account *acc = nullptr;

    g_return_val_if_fail (root, nullptr);

    acc_list =
        gnc_account_lookup_by_type_and_commodity (root,
                                                  checkname ? accname : nullptr,
                                                  acctype, currency);

    if (!acc_list)
        return construct_account (root, currency, accname,
                                  acctype, placeholder);

    if (g_list_next(acc_list))
    {
        if (!currency)
            acc = find_root_currency_account_in_list (acc_list);

        if (!acc)
            acc = find_account_matching_name_in_list (acc_list, accname);
    }

    if (!acc)
        acc = GNC_ACCOUNT (acc_list->data);

    g_list_free (acc_list);
    return acc;
}

void
xaccTransScrubPostedDate (Transaction *trans)
{
    time64 orig = xaccTransGetDate(trans);
    if(orig == INT64_MAX)
    {
	GDate date = xaccTransGetDatePostedGDate(trans);
	time64 time = gdate_to_time64(date);
	if(time != INT64_MAX)
	{
	    // xaccTransSetDatePostedSecs handles committing the change.
	    xaccTransSetDatePostedSecs(trans, time);
	}
    }
}

/* ==================== END OF FILE ==================== */
