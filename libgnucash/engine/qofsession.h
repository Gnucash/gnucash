/********************************************************************\
 * qofsession.h -- session access (connection to backend)           *
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

/** @addtogroup Backend
 *
 * The QOF Session
 * encapsulates a connection to a storage backend.  That is, it
 * manages the connection to a persistent data store; whereas
 * the backend is the thing that performs the actual datastore
 * access.
 *
 * This class provides several important services:
 *
 * 1) It resolves and loads the appropriate backend, based on
 *    the URL.
 *
 * 2) It reports backend errors (e.g. network errors, storage
 *    corruption errors) through a single, backend-independent
 *    API.
 *
 * 3) It reports non-error events received from the backend.
 *
 * 4) It helps manage global dataset locks.  For example, for the
 *    file backend, the lock prevents multiple users from editing
 *    the same file at the same time, thus avoiding lost data due
 *    to race conditions.  Thus, an open session implies that the
 *    associated file is locked.
 *
 * 5) Misc utilities, such as a search path for the file to be
 *    edited, and/or other URL resolution utilities.  This should
 *    simplify install & maintenance problems for naive users who
 *    may not have a good grasp on what a file system is, or where
 *    they want to keep their data files.
 *
 * 6) In the future, this class is probably a good place to manage
 *    a portion of the user authentication process, and hold user
 *    credentials/cookies/keys/tokens.  This is because at the
 *    coarsest level, authorization can happen at the datastore
 *    level: i.e. does this user even have the authority to connect
 *    to and open this datastore?
 *
 * A brief note about books & sessions:
 * A book encapsulates the datasets manipulated by QOF.  A book
 * holds the actual data.  By contrast, the session mediates the
 * connection between a book (the thing that lives in virtual memory
 * in the local process) and the datastore (the place where book
 * data lives permanently, e.g., file, database).
 *
 * In the current design, a session may hold multiple books.  For
 * now, exactly what this means is somewhat vague, and code in
 * various places makes some implicit assumptions: first, only
 * one book is 'current' and open for editing.  Next, its assumed
 * that all of the books in a session are related in some way.
 * i.e. that they are all earlier accounting periods of the
 * currently open book.  In particular, the backends probably
 * make that assumption, in order to store the different accounting
 * periods in a clump so that one can be found, given another.
 *
   If you want multiple books that are unrelated to each other,
   use multiple sessions.

   The session now calls QofBackendProvider->check_data_type
   to check that the incoming path contains data that the
   backend provider can open. The backend provider should
   also check if it can contact it's storage media (disk,
   network, server, etc.) and abort if it can't.  Malformed
   file URL's would be handled the same way.

 @{
 */

/** @file qofsession.h
 * @brief Encapsulates a connection to a backend (persistent store)
 * @author Copyright (c) 1998, 1999, 2001, 2002 Linas Vepstas <linas@linas.org>
 * @author Copyright (c) 2000 Dave Peticolas
 * @author Copyright (c) 2005 Neil Williams <linux@codehelp.co.uk>
 */

#ifndef QOF_SESSION_H
#define QOF_SESSION_H

#include "qofbackend.h"
#include "qofbook.h"
#include "qofclass.h"
#include "qof-load-executor.h"
#include "qofobject.h"

#ifdef __cplusplus
extern "C"
{
#endif

#define QOF_MOD_SESSION "qof.session"
/**
 * Mode for opening sessions. 
 */
/* This replaces three booleans that were passed in order: ignore_lock, create,
 * and force. It's structured so that one can use it as a bit field with the
 * values in the same order, i.e. ignore_lock = 1 << 2, create = 1 << 1, and
 * force = 1.
 */
typedef enum
{
    SESSION_NORMAL_OPEN = 0,    // All False
    /** Open will fail if the URI doesn't exist or is locked. */
    SESSION_NEW_STORE = 2,      // False, True, False (create)
    /** Create a new store at the URI. It will fail if the store already exists and is found to contain data that would be overwritten. */
    SESSION_NEW_OVERWRITE = 3,  // False, True, True (create | force)
    /** Create a new store at the URI even if a store already exists there. */
    SESSION_READ_ONLY = 4,      // True, False, False (ignore_lock)
    /** Open the session read-only, ignoring any existing lock and not creating one if the URI isn't locked. */
    SESSION_BREAK_LOCK = 5     // True, False, True (ignore_lock | force)
    /** Open the session, taking over any existing lock. */
} SessionOpenMode;

/* PROTOTYPES ******************************************************/

typedef struct QofSessionImpl QofSession;
typedef struct QofSessionOperationLease QofSessionOperationLease;

/** Semantic type of an exclusive session operation. */
typedef enum
{
    QOF_SESSION_OPERATION_GENERIC = 0,
    QOF_SESSION_OPERATION_OPEN,
    QOF_SESSION_OPERATION_SAVE,
    QOF_SESSION_OPERATION_SAVE_AS,
    QOF_SESSION_OPERATION_EXPORT,
    QOF_SESSION_OPERATION_CLOSE,
    QOF_SESSION_OPERATION_SCRUB,
    QOF_SESSION_OPERATION_IMPORT,
    QOF_SESSION_OPERATION_LOAD
} QofSessionOperationKind;

/**
 * Acquire the exclusive mutation lease for @a session.
 *
 * The returned opaque token is bound to the session, its current book, a
 * unique operation id, the session generation, and the current-session
 * generation. It must be released with
 * qof_session_operation_lease_release(). Acquisition fails while another
 * valid lease owns the session.
 */
QofSessionOperationLease *
qof_session_operation_lease_acquire (QofSession *session);

/** Acquire an exclusive mutation lease tagged with @a kind. */
QofSessionOperationLease *
qof_session_operation_lease_acquire_for (QofSession *session,
                                         QofSessionOperationKind kind);

/** Return whether @a lease still owns @a session and its current book. */
gboolean qof_session_operation_lease_is_valid (
    const QofSessionOperationLease *lease, const QofSession *session);

/** Return the unique operation id, or zero for an invalid token. */
guint64 qof_session_operation_lease_get_id (
    const QofSessionOperationLease *lease);

/** Return the operation kind, or GENERIC for an invalid token. */
QofSessionOperationKind qof_session_operation_lease_get_kind (
    const QofSessionOperationLease *lease);

/**
 * Release and free @a lease.
 *
 * The token may already have been invalidated by a book replacement, current
 * session change, swap, or session destruction. Releasing such a token is
 * still safe.
 */
void qof_session_operation_lease_release (QofSessionOperationLease *lease);

/** Return whether @a session currently has a valid exclusive lease. */
gboolean qof_session_has_active_operation_lease (const QofSession *session);

/** Return whether @a session is owned by a valid lease of @a kind. */
gboolean qof_session_has_active_operation_kind (
    const QofSession *session, QofSessionOperationKind kind);

/** Return the generation of @a session's mutation-lease state. It changes
 * whenever a lease is invalidated or released. */
guint64 qof_session_get_operation_generation (const QofSession *session);

QofSession * qof_session_new (QofBook* book);
void         qof_session_destroy (QofSession *session);

/** Destroy @a session if @a lease owns it. The lease is invalidated but must
 * still be released by its caller. */
gboolean qof_session_destroy_with_lease (
    QofSession *session, QofSessionOperationLease *lease);

/** The qof_session_swap_data () method swaps the book of
 *    the two given sessions. It is useful
 *    for 'Save As' type functionality. */
void qof_session_swap_data (QofSession *session_1, QofSession *session_2);

/** Swap two sessions atomically under their respective leases. A successful
 * swap invalidates both tokens because their bound books changed. */
gboolean qof_session_swap_data_with_leases (
    QofSession *session_1, QofSessionOperationLease *lease_1,
    QofSession *session_2, QofSessionOperationLease *lease_2);

/** Begins a new session.
 *
 * @param session Newly-allocated with qof_session_new.
 *
 * @param uri must be a string in the form of a URI/URL. The access method
 * specified depends on the loaded backends. Paths may be relative or
 * absolute.  If the path is relative, that is if the argument is
 * "file://somefile.xml", then the current working directory is
 * assumed. Customized backends can choose to search other
 * application-specific directories or URI schemes as well.
 *
 * @param mode The SessionOpenMode.
 *
 * @par ==== SessionOpenMode ====
 * `SESSION_NORMAL_OPEN`: Find an existing file or database at the provided uri and
 * open it if it is unlocked. If it is locked post a QOF_BACKEND_LOCKED error.
 * @par
 * `SESSION_NEW_STORE`: Check for an existing file or database at the provided
 * uri and if none is found, create it. If the file or database exists post a
 * QOF_BACKED_STORE_EXISTS and return.
 * @par
 * `SESSION_NEW_OVERWRITE`: Create a new file or database at the provided uri,
 * deleting any existing file or database.
 * @par
 * `SESSION_READ_ONLY`: Find an existing file or database and open it without
 * disturbing the lock if it exists or setting one if not. This will also set a
 * flag on the book that will prevent many elements from being edited and will
 * prevent the backend from saving any edits.
 * @par
 * `SESSION_BREAK_LOCK`: Find an existing file or database, lock it, and open
 * it. If there is already a lock replace it with a new one for this session.
 *
 * @par ==== Errors ====
 * This function signals failure by queuing errors. After it completes use
 * qof_session_get_error() and test that the value is `ERROR_BACKEND_NONE` to
 * determine that the session began successfully.
 */
void qof_session_begin (QofSession *session, const char * new_uri,
                        SessionOpenMode mode);

/** Token-aware variant. TRUE means that the lease authorized the call; backend
 * errors remain available through qof_session_get_error(). */
gboolean qof_session_begin_with_lease (
    QofSession *session, const QofSessionOperationLease *lease,
    const char *new_uri, SessionOpenMode mode);

/**
 * The qof_session_load() method causes the QofBook to be made ready to
 *    to use with this URL/datastore.   When the URL points at a file,
 *    then this routine would load the data from the file.  With remote
 *    backends, e.g. network or SQL, this would load only enough data
 *    to make the book actually usable; it would not cause *all* of the
 *    data to be loaded.
 *
 * XXX the current design tries to accommodate multiple calls to 'load'
 * for each session, each time wiping out the old books; this seems
 * wrong to me, and should be restricted to allow only one load per
 * session.
 */
typedef void (*QofPercentageFunc) (const char *message, double percent);
void qof_session_load (QofSession *session,
                       QofPercentageFunc percentage_func);

/** Terminal result of a deferred session load. */
typedef enum
{
    QOF_SESSION_LOAD_COMPLETED,
    QOF_SESSION_LOAD_CANCELLED,
    QOF_SESSION_LOAD_STALE,
    QOF_SESSION_LOAD_ERROR
} QofSessionLoadAsyncStatus;

typedef void (*QofSessionLoadAsyncCallback) (
    QofSession *session, QofSessionLoadAsyncStatus status,
    gpointer user_data);

/**
 * Start a deferred load after a successful BEGIN.
 *
 * @a lease must be the valid, exclusive LOAD lease for @a session. BEGIN and
 * LOAD are deliberately separate operations: this function rejects a lease
 * of any other kind, a session without a begun backend, and a non-empty book.
 * @a executor supplies deferred execution to both resumable and legacy
 * backends; its scheduling contract is described by QofSessionLoadExecutor.
 *
 * On TRUE the function consumes @a lease and releases it exactly once before
 * invoking @a callback. The caller must not release or otherwise use the
 * token after a successful call. @a callback is deferred exactly once and may
 * destroy the session. On FALSE ownership remains with the caller and no
 * callback is made.
 */
gboolean qof_session_load_async_with_lease (
    QofSession *session, QofSessionOperationLease *lease,
    QofPercentageFunc percentage_func, const QofSessionLoadExecutor *executor,
    QofSessionLoadAsyncCallback callback, gpointer user_data);

/**
 * Request terminal cancellation of the active LOAD operation. The request is
 * idempotent; its callback reports QOF_SESSION_LOAD_CANCELLED after parser
 * cleanup. It never cancels a different operation kind.
 */
gboolean qof_session_cancel_active_load (QofSession *session);

/** Token-aware load. A backend replacement of the bound book invalidates the
 * lease before this function returns. */
gboolean qof_session_load_with_lease (
    QofSession *session, QofSessionOperationLease *lease,
    QofPercentageFunc percentage_func);

/** @name Session Errors
 @{ */
/** The qof_session_get_error() routine can be used to obtain the reason
 *    for any failure.  Calling this routine returns the current error.
 */
QofBackendError qof_session_get_error (QofSession *session);
const char * qof_session_get_error_message(const QofSession *session);

/**
 * The qof_session_pop_error() routine can be used to obtain the reason
 *    for any failure.  Calling this routine resets the error value.
 *
 *    This routine allows an implementation of multiple error values,
 *    e.g. in a stack, where this routine pops the top value. The current
 *    implementation has a stack that is one-deep.
 *
 *    See qofbackend.h for a listing of returned errors.
 */
QofBackendError qof_session_pop_error (QofSession *session);
/** @} */

/** Returns the QofBook of this session. */
QofBook * qof_session_get_book (const QofSession *session);

/**
 *    The qof_session_get_file_path() routine returns the fully-qualified file
 *    path for the session. That is, if a relative or partial filename
 *    was for the session, then it had to have been fully resolved to
 *    open the session. This routine returns the result of this resolution.
 *    The path is always guaranteed to reside in the local file system,
 *    even if the session itself was opened as a URL.  (currently, the
 *    filepath is derived from the url by substituting commas for
 *    slashes).
 *
 *    The qof_session_get_url() routine returns the url that was opened.
 *    URL's for local files take the form of
 *    file:/some/where/some/file.gml
 */
const char * qof_session_get_file_path (const QofSession *session);

const char * qof_session_get_url (const QofSession *session);

/**
 * The qof_session_not_saved() subroutine will return TRUE
 *    if any data in the session hasn't been saved to long-term storage.
 */
/* gboolean qof_session_not_saved(const QofSession *session); <- unimplemented */
gboolean qof_session_save_in_progress(const QofSession *session);

/**
 * Returns the qof session's backend.
 */
QofBackend * qof_session_get_backend(const QofSession *session);

/** The qof_session_save() method will commit all changes that have been
 *    made to the session. For the file backend, this is nothing
 *    more than a write to the file of the current Accounts & etc.
 *    For the SQL backend, this is typically a no-op (since all data
 *    has already been written out to the database.
 */
void     qof_session_save (QofSession *session,
                           QofPercentageFunc percentage_func);
gboolean qof_session_save_with_lease (
    QofSession *session, const QofSessionOperationLease *lease,
    QofPercentageFunc percentage_func);

/**
 * A special version of save used in the sql backend which moves the
 * existing tables aside, then saves everything to new tables, then
 * deletes the old tables after the save is completed without
 * error. If there are errors, it removes the old tables and renames
 * the new tables back.
 */
void     qof_session_safe_save (QofSession *session,
                                QofPercentageFunc percentage_func);
gboolean qof_session_safe_save_with_lease (
    QofSession *session, const QofSessionOperationLease *lease,
    QofPercentageFunc percentage_func);

/**
 * The qof_session_end() method will release the session lock. For the
 *    file backend, it will *not* save the data to a file. Thus,
 *    this method acts as an "abort" or "rollback" primitive.  However,
 *    for other backends, such as the sql backend, the data would have
 *    been written out before this, and so this routines wouldn't
 *    roll-back anything; it would just shut the connection.
 */
void     qof_session_end  (QofSession *session);
gboolean qof_session_end_with_lease (
    QofSession *session, const QofSessionOperationLease *lease);

/** @}
*/

/** @name Event Handling

  @{ */
/** The qof_session_events_pending() method will return TRUE if the
 *  backend has pending events which must be processed to bring
 *  the engine up to date with the backend.
 */
gboolean qof_session_events_pending (const QofSession *session);

/**  The qof_session_process_events() method will process any events
 *   indicated by the qof_session_events_pending() method. It returns
 *   TRUE if the engine was modified while engine events were suspended.
 */
gboolean qof_session_process_events (QofSession *session);
gboolean qof_session_process_events_with_lease (
    QofSession *session, const QofSessionOperationLease *lease,
    gboolean *engine_modified);
/** @} */

gboolean qof_session_export (QofSession *tmp_session,
                             QofSession *real_session,
                             QofPercentageFunc percentage_func);
gboolean qof_session_export_with_leases (
    QofSession *tmp_session, const QofSessionOperationLease *tmp_lease,
    QofSession *real_session, const QofSessionOperationLease *real_lease,
    QofPercentageFunc percentage_func, gboolean *exported);

/** Return a list of strings for the registered access methods. The owner is
 *  responsible for freeing the list but not the strings.
 */
GList* qof_backend_get_registered_access_method_list(void);

/** Ensure all of the data is loaded from the session.
 */
void qof_session_ensure_all_data_loaded(QofSession* session);
gboolean qof_session_ensure_all_data_loaded_with_lease (
    QofSession *session, const QofSessionOperationLease *lease);

#ifdef __cplusplus
}
#endif

#endif /* QOF_SESSION_H */
/** @} */
