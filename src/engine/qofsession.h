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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

/** @addtogroup Engine
 *     @{ */
/** @file qofsession.h
 * @brief Encapsulates a connection to a backednd (persistent store)
 * @author Copyright (c) 1998, 1999, 2001, 2002 Linas Vepstas <linas@linas.org>
 * @author Copyright (c) 2000 Dave Peticolas
 *
 * FUNCTION:
 * Encapsulates a connection to a storage backend.  That is, it
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
 * A breif note about books & sessions:
 * A book encapsulates the datasets manipulated by GnuCash.  A book
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
 */

#ifndef QOF_SESSION_H
#define QOF_SESSION_H

#include "qofbackend.h"
#include "qofbook.h"

/* PROTOTYPES ******************************************************/

typedef struct _QofSession    QofSession;

QofSession * qof_session_new (void);
void         qof_session_destroy (QofSession *session);
QofSession * qof_session_get_current_session (void);
void	       qof_session_set_current_session (QofSession *session);

/** The qof_session_swap_data () method swaps the book of
 *    the two given sessions. It is useful
 *    for 'Save As' type functionality. */
void qof_session_swap_data (QofSession *session_1, QofSession *session_2);

/** The qof_session_begin () method begins a new session.
 *    It takes as an argument the book id. The book id must be a string
 *    in the form of a URI/URL.
 *    In the current implementation, the following URL's are supported
 *    -- File URI of the form 
 *       "file:/home/somewhere/somedir/file.xac"
 *       The path part must be a valid path.  The file-part must be 
 *       a valid old-style-xacc or new-style-gnucash-format file. Paths
 *       may be relative or absolute. If the path is relative; that is, 
 *       if the argument is  "file:somefile.xac" then a sequence of 
 *       search paths are checked for a file of this name.
 *
 *    -- Postgres URI of the form
 *       "postgres://hostname.com/dbname"
 *       See the src/backend/postgres subdirectory for more info.
 *
 *    -- RPC URI of the form rpc://hostname.com/rpcserver.
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
 */
typedef void (*QofPercentageFunc) (const char *message, double percent);
void qof_session_load (QofSession *session,
		       QofPercentageFunc percentage_func);

/* XXX session_export really doesn't belong here */
gboolean qof_session_export (QofSession *tmp_session,
			     QofSession *real_session,
			     QofPercentageFunc percentage_func);

/** The qof_session_get_error() routine can be used to obtain the reason
 *    for any failure.  Calling this routine returns the current error.
 *
 * The qof_session_pop_error() routine can be used to obtain the reason
 *    for any failure.  Calling this routine resets the error value.  
 *
 *    This routine allows an implementation of multiple error values, 
 *    e.g. in a stack, where this routine pops the top value. The current 
 *    implementation has a stack that is one-deep.
 *
 *    See qofbackend.h for a listing of returned errors.
 */
QofBackendError qof_session_get_error (QofSession *session);
const char * qof_session_get_error_message(QofSession *session);
QofBackendError qof_session_pop_error (QofSession *session);

/** The qof_session_add_book() allows additional books to be added to
 *    a session. 
 * XXX Under construction, clarify the following when done:
 * XXX There must already be an open book in the session already!?
 * XXX Only one open bok at a time per session is alowed!?
 * XXX each book gets its own unique backend ???
 */
void qof_session_add_book (QofSession *session, QofBook *book);

QofBook * qof_session_get_book (QofSession *session);

/** The qof_session_get_file_path() routine returns the fully-qualified file
 *    path for the session. That is, if a relative or partial filename
 *    was for the session, then it had to have been fully resolved to
 *    open the session. This routine returns the result of this resolution.
 *    The path is always guarenteed to reside in the local file system, 
 *    even if the session itself was opened as a URL.  (currently, the
 *    filepath is derived from the url by substituting commas for
 *    slashes).
 *
 * The qof_session_get_url() routine returns the url that was opened.
 *    URL's for local files take the form of 
 *    file:/some/where/some/file.gml
 */
const char * qof_session_get_file_path (QofSession *session);
const char * qof_session_get_url (QofSession *session);

/**
 * The qof_session_not_saved() subroutine will return TRUE
 *    if any data in the session hasn't been saved to long-term storage.
 */
gboolean qof_session_not_saved(QofSession *session);

/** FIXME: This isn't as thorough as we might want it to be... */
gboolean qof_session_save_may_clobber_data (QofSession *session);

/** The qof_session_save() method will commit all changes that have been
 *    made to the session. For the file backend, this is nothing
 *    more than a write to the file of the current AccountGroup & etc.
 *    For the SQL backend, this is typically a no-op (since all data
 *    has already been written out to the database.
 *
 * The qof_session_end() method will release the session lock. For the
 *    file backend, it will *not* save the account group to a file. Thus, 
 *    this method acts as an "abort" or "rollback" primitive.  However,
 *    for other backends, such as the sql backend, the data would have
 *    been written out before this, and so this routines wouldn't 
 *    roll-back anything; it would just shut the connection.
 */
void     qof_session_save (QofSession *session,
			   QofPercentageFunc percentage_func);
void     qof_session_end  (QofSession *session);

/** The qof_session_events_pending() method will return TRUE if the backend
 *    has pending events which must be processed to bring the engine
 *    up to date with the backend.
 *
 * The qof_session_process_events() method will process any events indicated
 *    by the qof_session_events_pending() method. It returns TRUE if the
 *    engine was modified while engine events were suspended.
 */
gboolean qof_session_events_pending (QofSession *session);
gboolean qof_session_process_events (QofSession *session);

/** The xaccResolveFilePath() routine is a utility that will accept
 *    a fragmentary filename as input, and resolve it into a fully
 *    qualified path in the file system, i.e. a path that begins with
 *    a leading slash.  First, the current working directory is
 *    searched for the file.  Next, the directory $HOME/.gnucash/data,
 *    and finally, a list of other (configurable) paths.  If the file
 *    is not found, then the path $HOME/.gnucash/data is used.  If
 *    $HOME is not defined, then the current working directory is
 *    used.
 */
char * xaccResolveFilePath (const char * filefrag);
char * xaccResolveURL (const char * pathfrag);

/** Run the RPC Server */
void gnc_run_rpc_server (void);

#endif /* QOF_SESSION_H */
/** @} */
