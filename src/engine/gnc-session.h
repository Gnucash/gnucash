/********************************************************************\
 * gnc-session.h -- session access (connection to backend)          *
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

/*
 * FILE:
 * gnc-session.h
 *
 * FUNCTION:
 * Encapsulate a connection to a GnuCash backend.
 *
 * This class provides several important services:
 *
 * 1) Prevents multiple users from editing the same file at the same
 *    time, thus avoiding lost data due to race conditions.  Thus
 *    an open session implies that the associated file is locked.
 *
 * 2) Provides a search path for the file to be edited.  This should 
 *    simplify install & maintenance problems for naive users who
 *    may not have a good grasp on what a file system is, or where
 *    they want to keep their data files.
 *
 * The current implementations assumes the use of files and file
 * locks; however, the API was designed to be general enough to
 * allow the use of generic URL's, and/or implementation on top
 * of SQL or other database/persistant object technology.
 *
 * HISTORY:
 * Created by Linas Vepstas December 1998
 * Copyright (c) 1998, 1999 Linas Vepstas
 * Copyright (c) 2000 Dave Peticolas
 */

#ifndef GNC_SESSION_H
#define GNC_SESSION_H

#include "gnc-book.h"
#include "gnc-engine.h"

/** PROTOTYPES ******************************************************/

GNCSession * gnc_session_new (void);
void         gnc_session_destroy (GNCSession *session);

/* The gnc_session_swap_data () method swaps the book and
 *    entity tables of the two given sessions. It is useful
 *    for 'Save As' type functionality. */
void gnc_session_swap_data (GNCSession *session_1, GNCSession *session_2);

/* The gnc_session_begin () method begins a new session.
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
 *       See the sql subdirectory for more info.
 *
 *    The 'ignore_lock' argument, if set to TRUE, will cause this routine
 *    to ignore any file locks that it finds.  If set to FALSE, then
 *    file locks will be tested and obeyed.
 *
 *    If the file exists, can be opened and read, and a lock can be obtained
 *    then a lock will be obtained and the function returns TRUE. 
 *
 *    If the file/database doesn't exist, and the create_if_nonexistent
 *    flag is set to TRUE, then the database is created.
 *
 *    Otherwise the function returns FALSE.
 */
gboolean gnc_session_begin (GNCSession *session, const char * book_id,
                         gboolean ignore_lock, gboolean create_if_nonexistent);


/* The gnc_session_load() method loads the data associated with the session.
 *    The function returns TRUE on success.
 */
gboolean gnc_session_load (GNCSession *session);

/* The gnc_session_get_error() routine can be used to obtain the reason
 *    for any failure.  Calling this routine returns the current error.
 *
 * The gnc_session_pop_error() routine can be used to obtain the reason
 *    for any failure.  Calling this routine resets the error value.  
 *
 *    This routine allows an implementation of multiple error values, 
 *    e.g. in a stack, where this routine pops the top value. The current 
 *    implementation has a stack that is one-deep.
 *
 *    See Backend.h for a listing of returned errors.
 */
GNCBackendError gnc_session_get_error (GNCSession *session);
const char * gnc_session_get_error_message(GNCSession *session);
GNCBackendError gnc_session_pop_error (GNCSession *session);


GNCBook * gnc_session_get_book (GNCSession *session);
void gnc_session_set_book (GNCSession *session, GNCBook *book);


/* The gnc_session_get_file_path() routine returns the fully-qualified file
 *    path for the session. That is, if a relative or partial filename
 *    was for the session, then it had to have been fully resolved to
 *    open the session. This routine returns the result of this resolution.
 *    The path is always guarenteed to reside in the local file system, 
 *    even if the session itself was opened as a URL.  (currently, the
 *    filepath is derived from the url by substituting commas for
 *    slashes).
 *
 * The gnc_session_get_url() routine returns the url that was opened.
 *    URL's for local files take the form of 
 *    file:/some/where/some/file.gml
 */
const char * gnc_session_get_file_path (GNCSession *session);
const char * gnc_session_get_url (GNCSession *session);

/*
 * The gnc_session_not_saved() subroutine will return TRUE
 *    if any data in the session hasn't been saved to long-term storage.
 */
gboolean gnc_session_not_saved(GNCSession *session);

/* FIXME: This isn't as thorough as we might want it to be... */
gboolean gnc_session_save_may_clobber_data (GNCSession *session);

/* The gnc_session_save() method will commit all changes that have been
 *    made to the session. In the current implementation, this is nothing
 *    more than a write to the file of the current AccountGroup of the
 *    session.
 *
 * The gnc_session_end() method will release the session lock. It will *not*
 *    save the account group to a file. Thus, this method acts as an "abort"
 *    or "rollback" primitive.
 */
void     gnc_session_save (GNCSession *session);
void     gnc_session_end  (GNCSession *session);

/* The gnc_session_events_pending() method will return TRUE if the backend
 *    has pending events which must be processed to bring the engine
 *    up to date with the backend.
 *
 * The gnc_session_process_events() method will process any events indicated
 *    by the gnc_session_events_pending() method. It returns TRUE if the
 *    engine was modified while engine events were suspended.
 */
gboolean gnc_session_events_pending (GNCSession *session);
gboolean gnc_session_process_events (GNCSession *session);

/* The xaccResolveFilePath() routine is a utility that will accept
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

/* Run the RPC Server */
void gnc_run_rpc_server (void);

#endif /* GNC_SESSION_H */
