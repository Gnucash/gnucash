/*
 * FILE:
 * Session.c
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
 *    If the file exists, can be opened and read, then an AccountGroup
 *    will be returned by xaccSessionGetGroup().  Otherwise, it returns NULL,
 *    and the reason for failure can be gotten with xaccSessionGetError().
 *
 *    If the file is succesfully opened and read, then a lock will have been
 *    obtained, and all other access to the file will be denied.  This feature
 *    is intended to be a brute-force global lock to avoid multiple writers.
 * 
 * The xaccSessionGetError() routine can be used to obtain the reason for
 *    any failure. Standard errno values are used.  Calling this routine resets
 *    the error value.  This routine allows an implementation of multiple 
 *    error values, e.g. in a stack, where this routine pops the top value.
 *    The current implementation has a stack that is one-deep.
 * 
 * The xaccSessionGetGroup() method will return the current top-level 
 *    account group.
 * 
 * The xaccSessionSetGroup() method will set the topgroup to a new value.
 *
 * The xaccSessionSave() method will commit all changes that have been made to
 *    the top-level account group. In the current implementation, this is nothing
 *    more than a write to the file of the top group.  A save does *not* 
 *    release the lock on the file.
 *
 * The xaccSessionEnd() method will release the session lock. It will *not*
 *    save the account group to a file.  Thus, this method acts as an "abort" or
 *    "rollback" primitive.
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

void           xaccSessionBegin (Session *, char * sessionid);
int            xaccSessionGetError (Session *);
AccountGroup * xaccSessionGetGroup (Session *);
void           xaccSessionSetGroup (Session *, AccountGroup *topgroup);

void           xaccSessionSave (Session *);
void           xaccSessionEnd (Session *);


#endif /* __XACC_SESSION_H__ */

#include <errno.h>
#include <limits.h>
#include <nana.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

#include "util.h"

struct _session {
   AccountGroup *topgroup;

   /* the requested session id, in the form or a URI, such as
    * file:/some/where
    */
   char *sessionid;

   /* the fully-resolved path to the file */
   char *fullpath;

   /* if session failed, this records the failure reason 
    * (file not found, etc).
    * the standard errno values are used.
    */
   int errtype;
};

/* ============================================================== */

Session *
xaccMallocSession (void)
{
   Session *sess;
   sess =  (Session *) malloc (sizeof (Session));

   xaccInitSession (sess);
   return sess;
}

void 
xaccInitSession (Session *sess)
{
  if (!sess) return;
  sess->topgroup = NULL;
  sess->errtype = 0;
  sess->sessionid = NULL;
  sess->fullpath = NULL;
};
  
/* ============================================================== */

int
xaccSessionGetError (Session * sess)
{
   int retval;

   if (!sess) return EBADSLT;
   retval = sess->errtype;
   sess->errtype = 0;
   return (retval);
}

/* ============================================================== */

AccountGroup * 
xaccSessionGetGroup (Session *sess)
{
   if (!sess) return NULL;
   return (sess->topgroup);
}

void 
xaccSessionSetGroup (Session *sess, AccountGroup *grp)
{
   if (!sess) return;
   sess->topgroup = grp;
}

/* ============================================================== */

/* hack alert -- we should be yanking this out of 
 * some config file 
 */
static char * searchpaths[] = {
   "/usr/share/gnucash/",
   NULL,
};

void
xaccSessionBegin (Session *sess, char * sid)
{
   char * filefrag;
   if (!sess) return;

   /* clear the error condition of previous errors */
   sess->errtype = 0;

   /* seriously invalid */
   if (!sid) {
      sess->errtype = EINVAL;
      return;
   }

   /* check to see if this session is already open */
   if (sess->sessionid) {
      sess->errtype = EEXIST;
      return;
   }

   /* check to see if this is a type we know how to handle */
   if (strncmp (sid, "file:", 5)) {
      sess->errtype = ENOSYS;
      return;
   }

   /* ---------------------------------------------------- */
   /* OK, now we try to find or build an absolute file path */

   /* check for an absolute file path */
   filefrag = sess->sessionid + 5;  /* space past 'file:' */
   if ('/' == *filefrag) {
      sess->fullpath = strdup (filefrag);
   } else {
      int i, rc;
      struct stat statbuf;
      char pathbuf[PATH_MAX];
      char * path = NULL;
      int namelen, len;

      namelen = strlen (filefrag) + 2;

      for (i=-2; 1 ; i++) 
      {
         switch (i) {
            case -2: {
               /* try to find a file by this name in the cwd ... */
               path = getcwd (pathbuf, PATH_MAX);
               if (!path) continue;
               len = strlen (path) + namelen;
               if (PATH_MAX <= len) continue;
               strcat (path, "/");
               break;
            }
            case -1: {
               /* look for something in $HOME/.gnucash/data */
               path = getenv ("HOME");
               if (!path) continue;
               len = strlen (path) + namelen + 20;
               if (PATH_MAX <= len) continue;
               strcpy (pathbuf, path);
               strcat (pathbuf, "/.gnucash/data/");
               path = pathbuf;
               break;
            }
            default: {
               /* OK, check the user-configured paths */
               path = searchpaths[i];
               len = strlen (path) + namelen;
               if (PATH_MAX <= len) continue;
               strcpy (pathbuf, path);
               path = pathbuf;
            }
         }

         if (!path) break;

         /* lets see if we found the file here ... */
         strcat (path, filefrag);
         rc = stat (path, &statbuf);
         if (!rc) {
            sess->fullpath = strdup (path);
            break;
         }
      }

      /* OK, we didn't find the file */
      /* Lets try creating a new file in $HOME/.gnucash/data */
      if (!(sess->fullpath)) 
      {
         /* let the user know that we're creating a new file */
         sess->errtype = ENOENT;

         path = getenv ("HOME");
         if (path) {
            len = strlen (path) + namelen + 20;
            if (PATH_MAX > len) {
               strcpy (pathbuf, path);
               strcat (pathbuf, "/.gnucash/data/");
               strcat (pathbuf, filefrag);
               sess->fullpath = strdup (pathbuf);
            }
         } 
      }

      /* OK, we still didn't find the file */
      /* Lets try creating a new file in the cwd */
      if (!(sess->fullpath)) 
      {
         /* let the user know that we're creating a new file */
         sess->errtype = ENOENT;

         /* create a new file in the cwd */
         path = getcwd (pathbuf, PATH_MAX);
         if (!path) {
            sess->errtype = ERANGE;  
            return;    /* ouch */
         }
         len = strlen (path) + namelen;
         if (PATH_MAX <= len) {
            sess->errtype = ERANGE;  
            return;    /* ouch */
         }
         strcat (path, "/");
         strcat (path, filefrag);
         sess->fullpath = strdup (path);
      }
   }
   assert (sess->fullpath);  /* no one fucked with the code, yeah? */
      
   /* OK, we've got a good string ... */
   sess->sessionid = strdup (sid);

   /* ---------------------------------------------------- */
   /* Yow! OK, after all of that, we've finnaly got a fully 
    * resolved path name.  Lets see if we can get a lock on it */
}

/* ==================== END OF FILE ================== */
