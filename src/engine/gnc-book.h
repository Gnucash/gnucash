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
#include "FileIO.h"


/** TYPEDEFS ********************************************************/
typedef struct _gnc_book GNCBook;

typedef gboolean (*GNCBookLockFailHandler) (const char *file);


/** PROTOTYPES ******************************************************/

GNCBook * gnc_book_new (void);
void      gnc_book_destroy (GNCBook *book);

/* The gnc_book_begin () method begins a new book. It takes as an argument
 *    the book id.  The book id must be a string in the form of a URI/URL.
 *    In the current implementation, only one type of URI is supported, and
 *    that is the file URI: anything of the form 
 *       "file:/home/somewhere/somedir/file.xac"
 *    The path part must be a valid path.  The file-part must be a valid
 *    xacc/gnucash-format file. Paths may be relative or absolute. If the
 *    path is relative; that is, if the argument is
 *       "file:somefile.xac"
 *    then a sequence of search paths are checked for a file of this name.
 *
 *    If the file exists, can be opened and read, and a lock can be obtained
 *    then a lock will be obtained and the function returns TRUE. Otherwise
 *    the function returns FALSE.
 */
gboolean gnc_book_begin (GNCBook *book, const char * book_id);

/* The gnc_book_begin_file() routine is identical to the gnc_book_begin()
 *    routine, except that the argument is a filename (i.e. the five
 *    letters "file:" should not be prepended) and there is an additional
 *    function argument. This function is called if gnc_book_begin_file
 *    fails to obtain a lock for the file. If it returns TRUE, the file
 *    is loaded anyway. If it returns FALSE, or the handler is NULL, a
 *    failed lock attempt will abort the load. The lock fail handler is
 *    passed the filename of the data file being loaded.
 */
gboolean gnc_book_begin_file (GNCBook *book, const char * filename,
                              GNCBookLockFailHandler handler);

/* The gnc_book_load() method loads the data associated with the book.
 *    The function returns TRUE on success.
 */
gboolean gnc_book_load (GNCBook *book);

/* The gnc_book_get_error() routine can be used to obtain the reason
 *    for any failure. Standard errno values are used.  Calling this
 *    routine resets the error value.  This routine allows an
 *    implementation of multiple error values, e.g. in a stack, where
 *    this routine pops the top value. The current implementation has
 *    a stack that is one-deep.
 * 
 *    Some important error values:
 *       EINVAL  -- bad argument
 *       ETXTBSY -- book id is in use; it's locked by someone else.
 *       ENOSYS  -- unsupported URI type.
 *       ERANGE  -- file path too long
 *       ENOLCK  -- book not open when gnc_book_save() was called.
 */
int gnc_book_get_error (GNCBook *book);

/* FIXME: This is a hack. I'm trying to move us away from static
 * global vars.  This may be a temp fix if we decide to integrate
 * FileIO errors into GNCBook errors. This just returns the last
 * FileIO error, but it doesn't clear it.
 */
GNCFileIOError gnc_book_get_file_error (GNCBook *book);

/* The gnc_book_get_group() method will return the current top-level 
 *    account group.
 * 
 * The gnc_book_set_group() method will set the topgroup to a new value.
 */
AccountGroup * gnc_book_get_group (GNCBook *book);
void           gnc_book_set_group (GNCBook *book, AccountGroup *topgroup);

/* The gnc_book_get_file_path() routine returns the fully-qualified file
 *    path for the book. That is, if a relative or partial filename
 *    was for the book, then it had to have been fully resolved to
 *    open the book. This routine returns the result of this resolution.
 */
const char * gnc_book_get_file_path (GNCBook *book);

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

#endif /* __GNC_BOOK_H__ */
