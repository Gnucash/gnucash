/*
 * FILE:
 * Session.h
 *
 * FUNCTION:
 * Provide wrappers for initiating/concluding a file-editing session.
 *
 * HISTORY:
 * Created by Linas Vepstas December 1998
 * Copyright (c) 1998 Linas Vepstas
 */

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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
\********************************************************************/

#ifndef __XACC_SESSION_H__
#define __XACC_SESSION_H__

#include "Group.h"

/** STRUCTS *********************************************************/
typedef struct _session Session;

/** PROTOTYPES ******************************************************/
/*
 * The xaccMallocSession() routine simply mallocs memory for a session object.
 * The xaccInitSession() routine initializes memry for a session object.
 * The xaccSessionDestroy() routine frees the associated memory.
 *    Note that this routine does *not* free the account group!
 */
Session  * xaccMallocSession (void);
void       xaccInitSession (Session *);
void       xaccSessionDestroy (Session *);

/*
 * The xaccSessionBegin() method begins a new session.  It takes as an argument
 *    the sessionid.  The session id must be a string in the form of a URI/URL.
 *    In the current implementation, only one type of URI is supported, and that
 *    is the file URI: anything of the form 
 *       "file:/home/somewhere/somedir/file.xac"
 *    The path part must be a valid path.  The file-part must be a valid
 *    xacc/gnucash-format file.  Paths may be relativ or absolute.  If the 
 *    path is relative; that is, if the argument is
 *       "file:somefile.xac"
 *    then a sequence of search paths are checked for a file of this name.
 *
 *    If the file exists, can be opened and read, then the AccountGroup
 *    will be returned.  Otherwise, NULL is returned, and the reason for 
 *    the NULL can be gotten with xaccSessionGetError().  Note that NULL
 *    does not always indicate an error ... e.g. if a new file is created,
 *    NULL is returned, and xaccSessionGetError returns 0.  That's OK.
 *
 *    If the file is succesfully opened and read, then a lock will have been
 *    obtained, and all other access to the file will be denied.  This feature
 *    is intended to be a brute-force global lock to avoid multiple writers.
 *
 * The xaccSessionBeginFile() routine is identical to the xaccSessionBegin()
 *    routine, except that the argument is a filename (i.e. the five
 *    letters "file:" should not be prepended).
 *
 * The xaccSessionGetFilePath() routine returns the fully-qualified file
 *    path for the session.  That is, if a relative or partial filename
 *    was for the session, then it had to have been fully resolved to
 *    open the session.  This routine returns the result of this resolution.
 * 
 * The xaccSessionGetError() routine can be used to obtain the reason for
 *    any failure. Standard errno values are used.  Calling this routine resets
 *    the error value.  This routine allows an implementation of multiple 
 *    error values, e.g. in a stack, where this routine pops the top value.
 *    The current implementation has a stack that is one-deep.
 * 
 *    Some important error values:
 *       EINVAL  -- bad argument
 *       ETXTBSY -- session id is in use; its locked by someone else.
 *       ENOSYS  -- unsupported URI type.
 *       ERANGE  -- file path too long
 *       ENOLCK  -- session not open when SessionSave() was called.
 * 
 * 
 * The xaccSessionGetGroup() method will return the current top-level 
 *    account group.
 * 
 * The xaccSessionSetGroup() method will set the topgroup to a new value.
 *
 * The xaccSessionSave() method will commit all changes that have been made to
 *    the top-level account group. In the current implementation, this is nothing
 *    more than a write to the file of the current AccountGroup of the session. 
 *    If the current AccountGroup is NULL, then the file will be deleted.
 *    This routine will never release the lock on the file under any
 *    circustances. 
 *
 * The xaccSessionEnd() method will release the session lock. It will *not*
 *    save the account group to a file.  Thus, this method acts as an "abort" or
 *    "rollback" primitive.
 *
 * The xaccResolveFilePath() routine is a utility that will accept a
 *    fragmentary filename as input, and resolve it into a fully-qualified path
 *    in the file system, i.e. a path that begins with a leading slash.
 *    First, the current working directory is searched for the file.
 *    Next, the directory $HOME/.gnucash/data, and finally, a list of other
 *    (configurable) paths.  If the file is not found, then the path 
 *    $HOME/.gnucash/data is used.  If $HOME is not defined, then the current
 *    working directory is used.
 *
 * EXAMPLE USAGE:
 * To read, modify and save an existing file:
 * ------------------------------------------
 *    Session *sess = xaccMallocSession ();
 *    xaccSessionBegin (sess, "file:/some/file.xac");
 *    AccountGroup *grp = xaccSessionGetGroup (sess);
 *    if (grp) { 
 *        ... edit the session ... 
 *        xaccSessionSave (sess);
 *    } else { 
 *        int err = xaccSessionGetError (sess); 
 *        printf ("%s\n", strerror (err)); 
 *    }
 *    xaccSessionEnd (sess);
 *    xaccSessionDestroy (sess);
 *
 * To save an existing session to a file:
 * --------------------------------------
 *    AccountGroup * grp = ...
 *    ... edit accounts, transactions, etc ...
 *    Session *sess = xaccMallocSession ();
 *    xaccSessionBegin (sess, "file:/some/file.xac");
 *    xaccSessionSetGroup (sess, grp);
 *    int err = xaccSessionGetError (sess); 
 *    if (0 == err) {
 *        xaccSessionSave (sess);
 *    } else { 
 *        printf ("%s\n", strerror (err)); 
 *    }
 *    xaccSessionEnd (sess);
 *    xaccSessionDestroy (sess);
 *
 */

AccountGroup * xaccSessionBegin       (Session *, char * sessionid);
AccountGroup * xaccSessionBeginFile   (Session *, char * filename);
int            xaccSessionGetError    (Session *);
AccountGroup * xaccSessionGetGroup    (Session *);
void           xaccSessionSetGroup    (Session *, AccountGroup *topgroup);
char         * xaccSessionGetFilePath (Session *);

void           xaccSessionSave (Session *);
void           xaccSessionEnd  (Session *);

char         * xaccResolveFilePath (const char * filefrag);

#endif /* __XACC_SESSION_H__ */
/* ==================== END OF FILE ================== */
