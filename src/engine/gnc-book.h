/********************************************************************\
 * gnc-book.h -- dataset access (set of accounting books)           *
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
 * gnc-book.h
 *
 * FUNCTION:
 * Encapsulate all the information about a gnucash dataset, including
 * the methods used to read and write them to datastores.
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

#ifndef __GNC_BOOK_H__
#define __GNC_BOOK_H__

#include "Group.h"
#include "Backend.h"
#include "gnc-pricedb.h"

/** TYPES **********************************************************/

typedef struct _gnc_book GNCBook;

/** PROTOTYPES ******************************************************/

GNCBook * gnc_book_new (void);
void      gnc_book_destroy (GNCBook *book);

/* The gnc_book_begin () method begins a new book. It takes as an argument
 *    the book id.  The book id must be a string in the form of a URI/URL.
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
gboolean gnc_book_begin (GNCBook *book, const char * book_id,
                         gboolean ignore_lock, gboolean create_if_nonexistent);


/* The gnc_book_load() method loads the data associated with the book.
 *    The function returns TRUE on success.
 */
gboolean gnc_book_load (GNCBook *book);

/* The gnc_book_get_error() routine can be used to obtain the reason
 *    for any failure.  Calling this routine returns the current error.
 *
 * The gnc_book_pop_error() routine can be used to obtain the reason
 *    for any failure.  Calling this routine resets the error value.  
 *
 *    This routine allows an implementation of multiple error values, 
 *    e.g. in a stack, where this routine pops the top value. The current 
 *    implementation has a stack that is one-deep.
 *
 *    See Backend.h for a listing of returned errors.
 */
GNCBackendError gnc_book_get_error (GNCBook *book);
const char * gnc_book_get_error_message(GNCBook *book);
GNCBackendError gnc_book_pop_error (GNCBook *book);


AccountGroup *gnc_book_get_group (GNCBook *book);
void gnc_book_set_group(GNCBook *book, AccountGroup *group);
GNCPriceDB   *gnc_book_get_pricedb (GNCBook *book);

guint gnc_book_count_transactions(GNCBook *book);

/*
 * gnc_book_get_commodity_table returns the commodity table associated with
 * the BOOK.  At the moment this just returns the global commodity table,
 * but if we get everything using this we can make it a non-global table :)
 */
gnc_commodity_table* gnc_book_get_commodity_table(GNCBook *book);

/**
 * Returns the list of scheduled transactions.
 **/
GList * gnc_book_get_schedxactions( GNCBook *book );
void gnc_book_set_schedxactions( GNCBook *book, GList *newList );

AccountGroup *gnc_book_get_template_group( GNCBook *book );
void gnc_book_set_template_group( GNCBook *book, AccountGroup *templateGroup );

/* The gnc_book_get_file_path() routine returns the fully-qualified file
 *    path for the book. That is, if a relative or partial filename
 *    was for the book, then it had to have been fully resolved to
 *    open the book. This routine returns the result of this resolution.
 *    The path is always guarenteed to reside in the local file system, 
 *    even if the book itself was opened as a URL.  (currently, the
 *    filepath is derived from the url by substituting commas for
 *    slashes).
 *
 * The gnc_book_get_url() routine returns the url that was opened.
 *    URL's for local files take the form of 
 *    file:/some/where/some/file.gml
 */
const char * gnc_book_get_file_path (GNCBook *book);
const char * gnc_book_get_url (GNCBook *book);

/*
 * The gnc_book_not_saved() subroutine will return TRUE
 *    if any data in the book hasn't been saved to long-term storage.
 */
gboolean gnc_book_not_saved(GNCBook *book);

/* FIXME: This isn't as thorough as we might want it to be... */
gboolean gnc_book_save_may_clobber_data (GNCBook *book);

/* The gnc_book_save() method will commit all changes that have been
 *    made to the book. In the current implementation, this is nothing
 *    more than a write to the file of the current AccountGroup of the
 *    book.
 *
 * The gnc_book_end() method will release the session lock. It will *not*
 *    save the account group to a file. Thus, this method acts as an "abort"
 *    or "rollback" primitive.
 */
void     gnc_book_save (GNCBook *book);
void     gnc_book_end  (GNCBook *book);

/* The gnc_book_events_pending() method will return TRUE if the backend
 *    has pending events which must be processed to bring the engine
 *    up to date with the backend.
 *
 * The gnc_book_process_events() method will process any events indicated
 *    by the gnc_book_events_pending() method. It returns TRUE if the
 *    engine was modified while engine events were suspended.
 */
gboolean gnc_book_events_pending (GNCBook *book);
gboolean gnc_book_process_events (GNCBook *book);

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

/*
 * Determine the load file type
 */
typedef enum 
{
    GNC_BOOK_NOT_OURS,
    GNC_BOOK_BIN_FILE,
    GNC_BOOK_XML1_FILE,
    GNC_BOOK_XML2_FILE,
} GNCBookFileType;
GNCBookFileType gnc_book_determine_file_type(GNCBook *book);

/* Run the RPC Server */
void gnc_run_rpc_server (void);

#endif /* __GNC_BOOK_H__ */
