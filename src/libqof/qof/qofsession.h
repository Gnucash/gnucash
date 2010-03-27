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
 * manages the connection to a persistant data store; whereas
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
#include "qofobject.h"

#define QOF_MOD_SESSION "qof.session"

/* PROTOTYPES ******************************************************/

typedef struct _QofSession    QofSession;

QofSession * qof_session_new (void);
void         qof_session_destroy (QofSession *session);

/** The qof_session_swap_data () method swaps the book of
 *    the two given sessions. It is useful
 *    for 'Save As' type functionality. */
void qof_session_swap_data (QofSession *session_1, QofSession *session_2);

/** The qof_session_begin () method begins a new session.
 *    It takes as an argument the book id. The book id must be a string
 *    in the form of a URI/URL. The access method specified depends
 *    on the loaded backends. In the absence of a customised backend,
 *    only QSF XML would be accepted). Paths may be relative or absolute.
 *    If the path is relative; that is, if the argument is "file:somefile.xml"
 *    then the current working directory is assumed. Customised backends can
 *    choose to search other, application-specific, directories as well.
 *
 *    The 'ignore_lock' argument, if set to TRUE, will cause this routine
 *    to ignore any global-datastore locks (e.g. file locks) that it finds.
 *    If set to FALSE, then file/database-global locks will be tested and
 *    obeyed.
 *
 *    If the datastore exists, can be reached (e.g over the net),
 *    connected to, opened and read, and a lock can be obtained then
 *    a lock will be obtained.   Note that multi-user datastores
 *    (e.g. the SQL backend) typically will not need to get a global
 *    lock, and thus, the user will not be locked out.  That's the
 *    whole point of 'multi-user'.
 *
 *    If the file/database doesn't exist, and the create_if_nonexistent
 *    flag is set to TRUE, then the database is created.
 *
 *    If an error occurs, it will be pushed onto the session error
 *    stack, and that is where it should be examined.
 */
void qof_session_begin (QofSession *session, const char * book_id,
                        gboolean ignore_lock, gboolean create_if_nonexistent);


/**
 * The qof_session_load() method causes the QofBook to be made ready to
 *    to use with this URL/datastore.   When the URL points at a file,
 *    then this routine would load the data from the file.  With remote
 *    backends, e.g. network or SQL, this would load only enough data
 *    to make the book actually usable; it would not cause *all* of the
 *    data to be loaded.
 *
 * XXX the current design tries to accomodate multiple calls to 'load'
 * for each session, each time wiping out the old books; this seems
 * wrong to me, and should be restricted to allow only one load per
 * session.
 */
typedef void (*QofPercentageFunc) (const char *message, double percent);
void qof_session_load (QofSession *session,
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

/** The qof_session_add_book() allows additional books to be added to
 *    a session.
 * XXX Under construction, clarify the following when done:
 * XXX There must already be an open book in the session already!?
 * XXX Only one open book at a time per session is allowed!?
 * XXX each book gets its own unique backend ???
 */
void qof_session_add_book (QofSession *session, QofBook *book);

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

/** Allows the backend to warn the user if a dataset already exists. */
gboolean qof_session_save_may_clobber_data (const QofSession *session);

/** The qof_session_save() method will commit all changes that have been
 *    made to the session. For the file backend, this is nothing
 *    more than a write to the file of the current Accounts & etc.
 *    For the SQL backend, this is typically a no-op (since all data
 *    has already been written out to the database.
 */
void     qof_session_save (QofSession *session,
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

/** @name Copying entities between sessions.

Only certain backends can cope with selective copying of
entities and only fully defined QOF entities can be copied
between sessions - see the \ref QSF (QSF) documentation
(::qsf_write_file) for more information.

The recommended backend for the new session is QSF or a future
SQL backend. Using any of these entity copy functions sets a
flag in the backend that this is now a partial QofBook. See \ref Reference.
When you save a session containing a partial QofBook,
the session will check that the backend is able to handle the
partial book. If not, the backend will be replaced by one that
can handle partial books, preferably one using the same
::access_method. Currently, this means that a book
using the GnuCash XML v2 file backend will be switched to QSF.

Copied entities are identical to the source entity, all parameters
defined with ::QofAccessFunc and ::QofSetterFunc in QOF are copied
and the ::GncGUID of the original ::QofInstance is set in the new entity.
Sessions containing copied entities are intended for use
as mechanisms for data export.

It is acceptable to add entities to new_session in batches. Note that
any of these calls will fail if an entity already exists in new_session
with the same GncGUID as any entity to be copied.

To merge a whole QofBook or where there is any possibility
of collisions or requirement for user intervention,
see \ref BookMerge

@{

*/

/** \brief Copy a single QofInstance to another session

Checks first that no entity in the session book contains
the GncGUID of the source entity.

 @param new_session - the target session
 @param original - the QofInstance* to copy

@return FALSE without copying if the session contains an entity
with the same GncGUID already, otherwise TRUE.
*/

gboolean qof_instance_copy_to_session(QofSession* new_session, QofInstance* original);

/** @brief Copy a GList of entities to another session

The QofBook in the new_session must \b not contain any entities
with the same GncGUID as any of the source entities - there is
no support for handling collisions, instead use \ref BookMerge

Note that the GList (e.g. from ::qof_sql_query_run) can contain
QofInstance pointers of any ::QofIdType, in any sequence. As long
as all members of the list are ::QofInstance*, and all GncGUID's are
unique, the list can be copied.

 @param new_session - the target session
 @param entity_list - a GList of QofInstance pointers of any type(s).

@return FALSE, without copying, if new_session contains any entities
with the same GncGUID. Otherwise TRUE.

*/
gboolean qof_instance_copy_list(QofSession *new_session, GList *entity_list);

/** @brief Copy a QofCollection of entities.

The QofBook in the new_session must \b not contain any entities
with the same GncGUID as any entities in the collection - there is
no support for handling collisions - instead, use \ref BookMerge

@param new_session - the target session
@param entity_coll - a QofCollection of any QofIdType.

@return FALSE, without copying, if new_session contains any entities
with the same GncGUID. Otherwise TRUE.
*/

gboolean qof_instance_copy_coll(QofSession *new_session, QofCollection *entity_coll);

/** \brief Recursively copy a collection of entities to a session.

\note This function creates a <b>partial QofBook</b>. See
::qof_instance_copy_to_session for more information.

The QofBook in the new_session must \b not contain any entities
with the same GncGUID as any entities to be copied - there is
no support for handling collisions - instead, use \ref BookMerge

Objects can be defined solely in terms of QOF data types or
as a mix of data types and other objects, which may in turn
include other objects. These references can be copied recursively
down to the third level. See ::QofInstanceReference.

\note This is a deep recursive copy - every referenced entity is copied
to the new session, including all parameters. The starting point is all
entities in the top level collection. It can take some time.

@param coll A QofCollection of entities that may or may not have
references.

@param new_session The QofSession to receive the copied entities.

@return TRUE on success; if any individual copy fails, returns FALSE.
<b>Note</b> : Some entities may have been copied successfully even if
one of the references fails to copy.

*/
gboolean
qof_instance_copy_coll_r(QofSession *new_session, QofCollection *coll);

/** \brief Recursively copy a single entity to a new session.

Copy the single entity and all referenced entities to the second level.

Only entities that are directly referenced by the top level entity are
copied.

This is a deep copy - all parameters of all referenced entities are copied. If
the top level entity has no references, this is identical to
::qof_instance_copy_to_session.

@param ent A single entity that may or may not have references.

@param new_session The QofSession to receive the copied entities.

@return TRUE on success; if any individual copy fails, returns FALSE.
<b>Note</b> : Some entities may have been copied successfully even if
one of the references fails to copy.
*/
gboolean
qof_instance_copy_one_r(QofSession *new_session, QofInstance *ent);

/** @}
*/

/** \brief Allow session data to be printed to stdout

book_id can't be NULL and we do need to have an access_method,
so use one to solve the other.

To print a session to stdout, use ::qof_session_begin. Example:

\a qof_session_begin(session,QOF_STDOUT,TRUE,FALSE);

When you call qof_session_save(session, NULL), the output will appear
on stdout and can be piped or redirected to other processes.

Currently, only the QSF backend supports writing to stdout, other
backends may return a ::QofBackendError.
*/
#define QOF_STDOUT ">"

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
/** @} */

/** Register a function to be called just before a session is closed.
 *
 *  @param fn The function to be called.  The function definition must
 *  be func(gpointer session, gpointer user_data);
 *
 *  @param data The data to be passed to the function. */
void qof_session_add_close_hook (GFunc fn, gpointer data);

/** Call all registered session close hooks, informing them that the
 *  specified session is about to be closed.
 *
 *  @param session A pointer to the session being closed. */
void qof_session_call_close_hooks (QofSession *session);

gboolean qof_session_export (QofSession *tmp_session,
                             QofSession *real_session,
                             QofPercentageFunc percentage_func);

/** Return a list of strings for the registered access methods. The owner is
 *  responsible for freeing the list but not the strings.
 */
GList* qof_backend_get_registered_access_method_list(void);

/** Ensure all of the data is loaded from the session.
 */
void qof_session_ensure_all_data_loaded(QofSession* session);

#endif /* QOF_SESSION_H */
/** @} */
