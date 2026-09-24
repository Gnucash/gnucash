/********************************************************************\
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


#include "qof.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct GncSessionOperationContext GncSessionOperationContext;

QofSession * gnc_get_current_session (void);
void gnc_clear_current_session(void);

/** Destroy and clear the current session when @a lease owns it. */
gboolean gnc_clear_current_session_with_lease (
    QofSessionOperationLease *lease);

void gnc_set_current_session (QofSession *session);
gboolean gnc_current_session_exist(void);

/**
 * Return the generation of the process-wide current-session slot.
 *
 * The generation changes whenever the slot changes identity. Session
 * operation leases capture it so that a continuation from an earlier current
 * session cannot mutate a later one.
 */
guint64 gnc_current_session_get_generation(void);

/**
 * Capture a lease-free workflow snapshot for repeated synchronous mutation
 * sections on @a book. Capture fails unless @a book belongs to the current
 * session and that session is not already leased.
 *
 * The context stores identity and generation snapshots, not mutation
 * authority. Each synchronous section must call
 * gnc_session_operation_context_begin() and end it before returning to the
 * main loop.
 */
GncSessionOperationContext *
gnc_session_operation_context_new (QofBook *book,
                                   QofSessionOperationKind kind);

GncSessionOperationContext *
gnc_session_operation_context_ref (GncSessionOperationContext *context);
void gnc_session_operation_context_unref (
    GncSessionOperationContext *context);

/** Acquire the context's exclusive lease for one synchronous section.
 * Recursive calls on the same context share the one lease. */
gboolean gnc_session_operation_context_begin (
    GncSessionOperationContext *context);

/** Acquire a short lease for terminal rollback/destruction.
 *
 * Unlike gnc_session_operation_context_begin(), this may recover from an
 * operation-generation drift when the context still identifies the exact
 * current session and book. Successful cleanup permanently prevents normal
 * continuation through this context. Recursive and later cleanup sections
 * remain possible so that one abort can release independently-owned temporary
 * objects without carrying authority across a main-loop turn. */
gboolean gnc_session_operation_context_begin_cleanup (
    GncSessionOperationContext *context);

/** End one synchronous section and release on the outermost matching call. */
void gnc_session_operation_context_end (
    GncSessionOperationContext *context);

/** Return whether the lease-free snapshot still names the current book. */
gboolean gnc_session_operation_context_is_current (
    const GncSessionOperationContext *context);

/** Return whether @a context still identifies the exact live current session
 * and book, ignoring only operation-generation drift and terminal state. */
gboolean gnc_session_operation_context_identifies_current_book (
    const GncSessionOperationContext *context);

/** Return whether this context presently owns its synchronous lease. */
gboolean gnc_session_operation_context_has_lease (
    const GncSessionOperationContext *context);

/** Return the most recently completed operation id, or zero before the first
 * completed section. */
guint64 gnc_session_operation_context_get_last_id (
    const GncSessionOperationContext *context);

#ifdef __cplusplus
}
#endif
