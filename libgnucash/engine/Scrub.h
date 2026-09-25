/********************************************************************\
 * Scrub.h -- convert single-entry accounts to clean double-entry   *
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

/** @addtogroup Engine
    @{ */
/** @addtogroup Scrub
    Data scrubbing, repairing and forward migration routines.
    These routines check and repair data, making sure that it
    is in a format that the current version of the GnuCash
    Engine likes.  These routines serve both to provide backwards
    compatibility with older versions of GnuCash, and to fix
    or at least paper over possible current problems.

    It is typically expected that the scrub routines are run
    over newly imported data, as well as during data file input.

    In some cases, it is entirely appropriate to invoke these
    routines from the GUI, to validate that the user input
    through the GUI is in a format that the system likes.
    This includes things like balancing individual transactions,
    or assigning splits to lots, so that capital gains can be
    computed.
    @{ */

/** @file Scrub.h
 *  @brief convert single-entry accounts to clean double-entry
 *  @author Created by Linas Vepstas December 1998
 *  @author Copyright (c) 1998-2000, 2003 Linas Vepstas <linas@linas.org>
 */

#ifndef XACC_SCRUB_H
#define XACC_SCRUB_H

#include "gnc-engine.h"

#ifdef __cplusplus
extern "C" {
#endif

/** @name Double-Entry Scrubbing
    Convert single-entry accounts to clean double-entry

    Provides a set of functions and utilities for checking and
    repairing (formerly called 'scrubbing clean') single-entry accounts
    so that they can be promoted into self-consistent, clean
    double-entry accounts. Basically and additionally, this file
    collects all functions that turn old (deprecated) data structures
    into the current new data model.

    The ScrubOrphans() methods search for transacations that contain
    splits that do not have a parent account. These "orphaned splits"
    are placed into an "orphan account" which the user will have to
    go into and clean up.  Kind of like the unix "Lost+Found" directory
    for orphaned inodes.
    @{  */

/** Opaque, book-bound authority for one synchronous GUI scrub operation. */
typedef struct GncScrubContext GncScrubContext;

/** Opaque, resumable engine scrub operation. */
typedef struct GncScrubJob GncScrubJob;

/** Terminal and non-terminal states returned by gnc_scrub_job_step(). */
typedef enum
{
    GNC_SCRUB_JOB_RUNNING,
    GNC_SCRUB_JOB_DONE,
    GNC_SCRUB_JOB_CANCELLED,
    GNC_SCRUB_JOB_FAILED,
} GncScrubJobState;

/** The scrub operation executed by a GncScrubJob. */
typedef enum
{
    GNC_SCRUB_JOB_ORPHANS,
    GNC_SCRUB_JOB_IMBALANCE,
    /** Run the orphan phase before the imbalance phase from one snapshot. */
    GNC_SCRUB_JOB_ACCOUNT,
    /** Drain the book's deferred transaction-gains FIFO. */
    GNC_SCRUB_JOB_GAINS,
    /** Scrub lots in one account or account tree, then drain gains. */
    GNC_SCRUB_JOB_LOTS,
    /** Scrub one non-business lot, then drain gains. */
    GNC_SCRUB_JOB_LOT,
} GncScrubJobKind;

/** The currently executing phase of a resumable scrub job. */
typedef enum
{
    GNC_SCRUB_JOB_PHASE_ORPHANS,
    GNC_SCRUB_JOB_PHASE_IMBALANCE,
    GNC_SCRUB_JOB_PHASE_GAINS,
    GNC_SCRUB_JOB_PHASE_LOTS,
} GncScrubJobPhase;

/**
 * Acquire the current session's exclusive SCRUB lease for @a book.
 *
 * Acquisition fails unless @a book belongs to the current session and no other
 * operation owns that session. The caller must call
 * gnc_scrub_context_end() before returning to the main loop and then release
 * its reference with gnc_scrub_context_unref().
 */
GncScrubContext *gnc_scrub_context_begin (QofBook *book);

/** Retain/release a context reference for an asynchronous cancel callback. */
GncScrubContext *gnc_scrub_context_ref (GncScrubContext *context);
void gnc_scrub_context_unref (GncScrubContext *context);

/** Cancel only this operation. Cancellation of an ended context is a no-op. */
void gnc_scrub_context_cancel (GncScrubContext *context);
gboolean gnc_scrub_context_is_cancelled (const GncScrubContext *context);

/** Return whether the context still owns its original current session and book. */
gboolean gnc_scrub_context_is_active (const GncScrubContext *context);
gboolean gnc_scrub_context_owns_book (const GncScrubContext *context,
                                      const QofBook *book);

/** Release the SCRUB lease exactly once without dropping context references. */
void gnc_scrub_context_end (GncScrubContext *context);

/** The automatic Transaction commit hook to defer. Kinds have independent
 * FIFO/dedupe queues. */
typedef enum
{
    GNC_SCRUB_DEFERRED_COMMIT_IMBALANCE,
    GNC_SCRUB_DEFERRED_COMMIT_GAINS,
} GncScrubDeferredCommitKind;

/**
 * Enable central commit-hook deferral for one hook kind in this active,
 * non-cancelled context. Kinds are enabled independently. Every mode is off by
 * default and all modes are removed when the context is cancelled or ends.
 * Pending GUID work remains attached to the book for a later valid context.
 */
gboolean gnc_scrub_context_enable_commit_deferral (
    GncScrubContext *context, GncScrubDeferredCommitKind kind);

/** Return whether @a kind is actively deferred by this context. Pending work
 * handed off from an earlier context does not make this return true. */
gboolean gnc_scrub_context_commit_deferral_enabled (
    const GncScrubContext *context, GncScrubDeferredCommitKind kind);

/** Return the pending GUID count for one hook kind in @a context's book. */
guint gnc_scrub_deferred_commit_pending_count (
    const GncScrubContext *context, GncScrubDeferredCommitKind kind);

/**
 * Prepare one bounded unit without removing it. Call ack() only after the
 * corresponding work completed; cancellation or context end before ack()
 * leaves the GUID available to a later valid context.
 */
gboolean gnc_scrub_deferred_commit_peek (
    const GncScrubContext *context, GncScrubDeferredCommitKind kind,
    GncGUID *guid);

/** Acknowledge the FIFO head returned by peek() after completing its work. */
gboolean gnc_scrub_deferred_commit_ack (
    const GncScrubContext *context, GncScrubDeferredCommitKind kind,
    const GncGUID *guid);

/**
 * Start a resumable orphan-scrub pass for @a account.
 *
 * The job snapshots the affected transaction GUIDs while acquiring the
 * current session's SCRUB lease. It never retains transaction pointers across
 * steps. Call gnc_scrub_job_step() until it returns a terminal state, then
 * call gnc_scrub_job_free().
 */
GncScrubJob *gnc_scrub_orphans_job_begin (Account *account,
                                          gboolean descendants);

/** Start a resumable imbalance-scrub pass for @a account. */
GncScrubJob *gnc_scrub_imbalance_job_begin (Account *account,
                                            gboolean descendants);

/**
 * Start the core account scrub phases used by Account and Account Tree.
 *
 * The job snapshots the selected transactions once, then processes that same
 * snapshot in the ORPHANS phase followed by the IMBALANCE phase without
 * releasing its SCRUB lease between phases. get_completed()/get_total() count
 * phase units, so a non-empty job has twice as many total units as snapshot
 * transactions and each phase accounts for one complete snapshot pass. Lots
 * and Business scrubs are intentionally not part of this job.
 */
GncScrubJob *gnc_scrub_account_job_begin (Account *account,
                                          gboolean descendants);

/** Acquire a SCRUB lease, activate gains commit deferral, and drain the
 * existing and newly appended FIFO with a nested transaction gains plan.
 * The FIFO head is acknowledged only after every nested phase reaches DONE. */
GncScrubJob *gnc_scrub_deferred_gains_job_begin (QofBook *book);

/** Scrub lot assignment and every non-business lot in @a account (and,
 * optionally, its descendants), then drain all deferred gains work before
 * releasing the SCRUB lease. */
GncScrubJob *gnc_scrub_lots_job_begin (Account *account,
                                       gboolean descendants);

/** Scrub one non-AP/AR lot and drain all deferred gains work generated by it.
 * AP/AR lots remain owned by the business scrub path and are rejected. */
GncScrubJob *gnc_scrub_lot_job_begin (GNCLot *lot);

/**
 * Process at most @a max_transactions bounded primitive units in this turn.
 * Composite lot jobs pass unused units from their structural plan to their
 * nested deferred-gains plan in the same call.
 * A zero limit is invalid and terminates the job as failed.
 */
GncScrubJobState gnc_scrub_job_step (GncScrubJob *job,
                                     guint max_transactions);

/** Cancel a job and release its SCRUB lease without waiting for another step. */
void gnc_scrub_job_cancel (GncScrubJob *job);

GncScrubJobState gnc_scrub_job_get_state (const GncScrubJob *job);
/** Return whether a structural primitive deleted at least one peer split. */
gboolean gnc_scrub_job_get_changed (const GncScrubJob *job);
GncScrubJobKind gnc_scrub_job_get_kind (const GncScrubJob *job);
GncScrubJobPhase gnc_scrub_job_get_phase (const GncScrubJob *job);
guint gnc_scrub_job_get_total (const GncScrubJob *job);
guint gnc_scrub_job_get_completed (const GncScrubJob *job);

/** Release a job. Releasing a running job cancels it first. */
void gnc_scrub_job_free (GncScrubJob *job);

/** Context-aware variants share cancellation and authority through recursion. */
void xaccTransScrubOrphansWithContext (Transaction *trans,
                                       GncScrubContext *context);
void xaccAccountScrubOrphansWithContext (Account *acc,
                                         QofPercentageFunc percentagefunc,
                                         GncScrubContext *context);
void xaccAccountTreeScrubOrphansWithContext (Account *acc,
                                             QofPercentageFunc percentagefunc,
                                             GncScrubContext *context);
void xaccTransScrubImbalanceWithContext (Transaction *trans, Account *root,
                                         Account *parent,
                                         GncScrubContext *context);
void xaccAccountScrubImbalanceWithContext (Account *acc,
                                           QofPercentageFunc percentagefunc,
                                           GncScrubContext *context);
void xaccAccountTreeScrubImbalanceWithContext (Account *acc,
                                               QofPercentageFunc percentagefunc,
                                               GncScrubContext *context);

/** The xaccTransScrubOrphans() method scrubs only the splits in the
 *    given transaction.
 */
void xaccTransScrubOrphans (Transaction *trans);

/** The xaccAccountScrubOrphans() method performs this scrub only for the
 *    indicated account, and not for any of its children.
 */
void xaccAccountScrubOrphans (Account *acc, QofPercentageFunc percentagefunc);

/** The xaccAccountTreeScrubOrphans() method performs this scrub for the
 *    indicated account and its children.
 */
void xaccAccountTreeScrubOrphans (Account *acc, QofPercentageFunc percentagefunc);

/** The xaccSplitScrub method ensures that if this split has the same
 *   commodity and currency, then it will have the same amount and value.
 *   If the commodity is the currency, the split->amount is set to the
 *   split value.  In addition, if this split is an orphan, that is
 *   fixed first.  If the split account doesn't have a commodity declared,
 *   an attempt is made to fix that first.
 */
void xaccSplitScrub (Split *split);

/** The xacc*ScrubSplits() calls xaccSplitScrub() on each split
 *    in the respective structure: transaction, account,
 *    account & it's children, account-group.
 */
void xaccTransScrubSplits (Transaction *trans);
void xaccAccountScrubSplits (Account *account);
void xaccAccountTreeScrubSplits (Account *account);

/** The xaccScrubImbalance() method searches for transactions that do
 *    not balance to zero. If any such transactions are found, a split
 *    is created to offset this amount and is added to an "imbalance"
 *    account.
 */
void xaccTransScrubImbalance (Transaction *trans, Account *root,
                              Account *parent);
void xaccAccountScrubImbalance (Account *acc, QofPercentageFunc percentagefunc);
void xaccAccountTreeScrubImbalance (Account *acc, QofPercentageFunc percentagefunc);

/** The xaccTransScrubCurrency method fixes transactions without a
 * common_currency by looking for the most commonly used currency
 * among all the splits in the transaction.  If this fails it falls
 * back to using the old account currency and security
 * fields of the parent accounts of the transaction's splits. */
void xaccTransScrubCurrency (Transaction *trans);

/** The xaccAccountScrubCommodity method fixed accounts without
 * a commodity by using the old account currency and security. */
void xaccAccountScrubCommodity (Account *account);

/** The xaccAccountTreeScrubCommodities will scrub the
 * currency/commodity of all accounts & transactions in the specified
 * account or any child account. */
void xaccAccountTreeScrubCommodities (Account *acc);

/** This routine will migrate the information about price quote
 *  sources from the account data structures to the commodity data
 *  structures.  It first checks to see if this is necessary since,
 *  for the time being, the quote information will still be written
 *  out as part of the account.  Just in case anyone needs to fall
 *  back from CVS to a production version of code.
 *
 *  @param root A pointer to the root account containing all
 *  accounts in the current book.
 *
 *  @param table A pointer to the commodity table for the current
 *  book.
 */
void xaccAccountTreeScrubQuoteSources (Account *root, gnc_commodity_table *table);

/** Removes empty "notes", "placeholder", and "hbci" KVP slots from Accounts. */
void xaccAccountScrubKvp (Account *account);

/** Remove color slots that have a "Not Set" value, since 2.4.0, fixed in 3.4
 *  This should only be run once on a book
 */
void xaccAccountScrubColorNotSet (QofBook *book);

/** Changes Transaction date_posted timestamps from 00:00 local to 11:00 UTC.
 * 11:00 UTC is the same day local time in almost all timezones, the exceptions
 * being the -12, +13, and +14 timezones along the International Date Line. If
 * Local time is set to one of these timezones then the new date_posted time
 * will be adjusted as needed to ensure that the date doesn't change there. This
 * change was made for v2.6.14 to partially resolve bug 137017.
 */
void xaccTransScrubPostedDate (Transaction *trans);

#ifdef __cplusplus
}
#endif

#endif /* XACC_SCRUB_H */
/** @} */
/** @} */
/** @} */
