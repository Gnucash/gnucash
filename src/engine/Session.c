/*
 * FILE:
 * Session.c
 *
 * FUNCTION:
 * Provide wrappers for initiating/concluding a file-editing session.
 *
 * HISTORY:
 * Created by Linas Vepstas December 1998
 * Copyright (c) 1998-2000 Linas Vepstas
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
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
\********************************************************************/

#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "BackendP.h"
#include "FileIO.h"
#include "FileIOP.h"
#include "Group.h"
#include "Session.h"
#include "util.h"

struct _session {
   AccountGroup *topgroup;

   /* the requested session id, in the form or a URI, such as
    * file:/some/where, or sql:server.host.com:555
    */
   char *sessionid;

   /* if session failed, this records the failure reason 
    * (file not found, etc).
    * the standard errno values are used.
    */
   int errtype;

   /* ---------------------------------------------------- */
   /* teh following struct members apply only for file-io */
   /* the fully-resolved path to the file */
   char *fullpath;

   /* name of lockfile, and filedescr */
   char * lockfile;
   char * linkfile;
   int lockfd;

   /* ---------------------------------------------------- */
   /* this struct member applies only for SQL i/o */
   Backend *backend;

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
  sess->lockfile = NULL;
  sess->linkfile = NULL;
  sess->lockfd = -1;
  sess->backend = NULL;
};
  
/* ============================================================== */

int
xaccSessionGetError (Session * sess)
{
   int retval;

   if (!sess) return EINVAL;
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

Backend * 
xaccSessionGetBackend (Session *sess)
{
   if (!sess) return NULL;
   return (sess->backend);
}

/* ============================================================== */

char * 
xaccSessionGetFilePath (Session *sess)
{
   if (!sess) return NULL;
   return (sess->fullpath);
}

/* ============================================================== */

AccountGroup *
xaccSessionBegin (Session *sess, const char * sid)
{
   AccountGroup *retval;

   if (!sess) return NULL;

   /* clear the error condition of previous errors */
   sess->errtype = 0;

   /* check to see if this session is already open */
   if (sess->sessionid) {
      sess->errtype = ETXTBSY;
      return NULL;
   }

   /* seriously invalid */
   if (!sid) {
      sess->errtype = EINVAL;
      return NULL;
   }

   /* check to see if this is a type we know how to handle */
   if (strncmp (sid, "file:", 5)) {
      sess->errtype = ENOSYS;
      return NULL;
   }

   /* add 5 to space past 'file:' */
   retval = xaccSessionBeginFile (sess, sid+5);

   return retval;
}

/* ============================================================== */

AccountGroup *
xaccSessionBeginSQL (Session *sess, const char * dbname)
{
   Backend *be = NULL;
   AccountGroup *grp = NULL;

   if (!sess) return NULL;

// #define SQLHACK
#ifdef SQLHACK
   {
     /* for testing the sql, just a hack, remove later ... */
extern Backend * pgendNew (void);
     be = pgendNew ();
   }
#endif

   sess->backend = be;

   if (be && be->session_begin) {
      grp = (be->session_begin) (sess, dbname);
   }
   // comment out until testing done, else clobber file ...
   // sess->topgroup = grp;
   xaccGroupSetBackend (sess->topgroup, be);

   return (sess->topgroup);
}

/* ============================================================== */

AccountGroup *
xaccSessionBeginFile (Session *sess, const char * filefrag)
{
   struct stat statbuf;
   char pathbuf[PATH_MAX];
   char *path = NULL;
   int rc;

   if (!sess) return NULL;

   /* clear the error condition of previous errors */
   sess->errtype = 0;

   /* check to see if this session is already open */
   if (sess->sessionid) {
      sess->errtype = ETXTBSY;
      return NULL;
   }

   /* seriously invalid */
   if (!filefrag) {
      sess->errtype = EINVAL;
      return NULL;
   }

   /* ---------------------------------------------------- */
   /* OK, now we try to find or build an absolute file path */

   sess->fullpath = xaccResolveFilePath (filefrag);
   if (! (sess->fullpath)) {
      sess->errtype = ERANGE;  
      return NULL;    /* ouch */
   }

   /* Store the sessionid URL also ... */
   strcpy (pathbuf, "file:");
   strcat (pathbuf, filefrag);
   sess->sessionid = strdup (pathbuf);

   /* ---------------------------------------------------- */
   /* We should now have a fully resolved path name.
    * Lets see if we can get a lock on it. 
    */

   sess->lockfile = malloc (strlen (sess->fullpath) + 5);
   strcpy (sess->lockfile, sess->fullpath);
   strcat (sess->lockfile, ".LCK");
  
   rc = stat (sess->lockfile, &statbuf);
   if (!rc) {
      /* oops .. file is all locked up  .. */
      sess->errtype = ETXTBSY;  
      free (sess->sessionid); sess->sessionid = NULL;
      free (sess->fullpath);  sess->fullpath = NULL;
      free (sess->lockfile);  sess->lockfile = NULL;
      return NULL;    
   }
   sess->lockfd = open (sess->lockfile, O_RDWR | O_CREAT | O_EXCL , 0);
   if (0 > sess->lockfd) {
      /* oops .. file is all locked up  .. */
      sess->errtype = ETXTBSY;  
      free (sess->sessionid); sess->sessionid = NULL;
      free (sess->fullpath);  sess->fullpath = NULL;
      free (sess->lockfile);  sess->lockfile = NULL;
      return NULL;    
   }

   /* OK, now work around some NFS atomic lock race condition 
    * mumbo-jumbo.  We do this by linking a unique file, and 
    * then examing the link count.  At least that's what the 
    * NFS programmers guide suggests. 
    * Note: the "unique filename" must be unique for the
    * triplet filename-host-process, otherwise accidental 
    * aliases can occur.
    */
   /* appearently, even this code may not work for some NFS
    * implementations.  In the long run, I am told that 
    *  ftp.debian.org
    * /pub/debian/dists/unstable/main/source/libs/liblockfile_0.1-6.tar.gz
    * provides a better long-term solution.
    */
   strcpy (pathbuf, sess->lockfile);
   path = strrchr (pathbuf, '.');
   sprintf (path, ".%lx.%d.LNK", gethostid(), getpid());
   link (sess->lockfile, pathbuf);
   rc = stat (sess->lockfile, &statbuf);
   if (rc) {
      /* oops .. stat failed!  This can't happen! */
      sess->errtype = ETXTBSY;  
      unlink (pathbuf);
      close (sess->lockfd);
      unlink (sess->lockfile);
      free (sess->sessionid); sess->sessionid = NULL;
      free (sess->fullpath);  sess->fullpath = NULL;
      free (sess->lockfile);  sess->lockfile = NULL;
      return NULL;    
   }

   if (2 != statbuf.st_nlink) {
      /* oops .. stat failed!  This can't happen! */
      sess->errtype = ETXTBSY;  
      unlink (pathbuf);
      close (sess->lockfd);
      unlink (sess->lockfile);
      free (sess->sessionid); sess->sessionid = NULL;
      free (sess->fullpath);  sess->fullpath = NULL;
      free (sess->lockfile);  sess->lockfile = NULL;
      return NULL;    
   }
   sess->linkfile = strdup (pathbuf);

   /* ---------------------------------------------------- */
   /* OK, if we've gotten this far, then we've succesfully obtained 
    * an atomic lock on the file.  Go read the file contents ... 
    * well, read it only if it exists ... 
    */

   sess->errtype = 0;
   sess->topgroup = NULL;
   rc = stat (sess->fullpath, &statbuf);
   if (!rc) {
      sess->topgroup = xaccReadAccountGroupFile (sess->fullpath);
   }

#ifdef SQLHACK
/* for testing the sql, just a hack, remove later ... */
/* this should never ever appear here ...  */
xaccSessionBeginSQL (sess, "postgres://localhost/gnc_bogus");
#endif

   return (sess->topgroup);
}

/* ============================================================== */

void 
xaccSessionSave (Session *sess)
{
   if (!sess) return;

   /* if the fullpath doesn't exist, either the user failed to initialize,
    * or the lockfile was never obtained ... either way, we can't write. */
   sess->errtype = 0;
   if (!(sess->fullpath)) {
      sess->errtype = ENOLCK;
      return;
   }
   if (sess->topgroup) {
      xaccWriteAccountGroupFile (sess->fullpath, sess->topgroup);
   } else {
      /* hmm ... no topgroup means delete file */
      unlink (sess->fullpath);
   }
}

/* ============================================================== */

void
xaccSessionEnd (Session *sess)
{
   if (!sess) return;
   sess->errtype = 0;

   if (sess->linkfile) unlink (sess->linkfile);
   if (0 < sess->lockfd) close (sess->lockfd);
   if (sess->lockfile) unlink (sess->lockfile);
   if (sess->sessionid) free (sess->sessionid); sess->sessionid = NULL;
   if (sess->fullpath) free (sess->fullpath);  sess->fullpath = NULL;
   if (sess->lockfile) free (sess->lockfile);  sess->lockfile = NULL;
   if (sess->linkfile) free (sess->linkfile);  sess->linkfile = NULL;
   sess->topgroup = NULL;

   return;    
}

void 
xaccSessionDestroy (Session *sess) 
{
   if (!sess) return;
   xaccSessionEnd (sess);
   free (sess);
}


/* ============================================================== */
/* 
 * If $HOME/.gnucash/data directory doesn't exist, then create it.
 */

static void 
MakeHomeDir (void) 
{
   int rc;
   struct stat statbuf;
   char *home, *path;

   /* Punt. Can't figure out where home is. */
   home = getenv ("HOME");
   if (!home) return;

   path = alloca (strlen (home) +50);
   strcpy (path, home);
   strcat (path, "/.gnucash");

   rc = stat (path, &statbuf);
   if (rc) {
      /* assume that the stat failed only because the dir is absent,
       * and not because its read-protected or other error.
       * Go ahead and make it. Don't bother much with checking mkdir 
       * for errors; seems pointless ...  */
      mkdir (path, S_IRWXU);   /* perms = S_IRWXU = 0700 */
   }

   strcat (path, "/data");
   rc = stat (path, &statbuf);
   if (rc)
      mkdir (path, S_IRWXU);
}

/* ============================================================== */

/* XXX hack alert -- we should be yanking this out of 
 * some config file 
 */
static char * searchpaths[] = {
   "/usr/share/gnucash/data/",
   NULL,
};

char * 
xaccResolveFilePath (const char * filefrag)
{
   struct stat statbuf;
   char pathbuf[PATH_MAX];
   char *path = NULL;
   int namelen, len;
   int i, rc;

   /* seriously invalid */
   if (!filefrag) return NULL;

   /* ---------------------------------------------------- */
   /* OK, now we try to find or build an absolute file path */

   /* check for an absolute file path */
   if ('/' == *filefrag) {
      return (strdup (filefrag));
   } 

   /* get conservative on the length so that sprintf(getpid()) works ... */
   /* strlen ("/.LCK") + sprintf (%x%d) */
   namelen = strlen (filefrag) + 25; 

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
            if (path) {
               len = strlen (path) + namelen;
               if (PATH_MAX <= len) continue;
               strcpy (pathbuf, path);
               path = pathbuf;
            }
         }
      }
      if (!path) break;

      /* lets see if we found the file here ... */
      /* haral: if !S_ISREG: there is something with that name 
       * but it's not a regular file */
      strcat (path, filefrag);
      rc = stat (path, &statbuf);
      if ((!rc) && (S_ISREG(statbuf.st_mode))) {
          return (strdup (path));
      }
   }

   /* make sure that the gnucash home dir exists. */
   MakeHomeDir();

   /* OK, we didn't find the file. */
   /* If the user specified a simple filename (i.e. no slashes in it)
    * then create the file.  But if it has slashes in it, then creating
    * a bnuch of directories seems like a bad idea; more likely, the user
    * specified a bad filename.  So return with error. */
   if (strchr (filefrag, '/')) {
      return NULL;
   }
   
   /* Lets try creating a new file in $HOME/.gnucash/data */
   path = getenv ("HOME");
   if (path) {
      len = strlen (path) + namelen + 50;
      if (PATH_MAX > len) {
         strcpy (pathbuf, path);
         strcat (pathbuf, "/.gnucash/data/");
         strcat (pathbuf, filefrag);
         return (strdup (pathbuf));
      }
   } 

   /* OK, we still didn't find the file */
   /* Lets try creating a new file in the cwd */
   path = getcwd (pathbuf, PATH_MAX);
   if (path) {
      len = strlen (path) + namelen;
      if (PATH_MAX > len) {
         strcat (path, "/");
         strcat (path, filefrag);
         return (strdup (path));
      }
   }

   return NULL;
}

/* ==================== END OF FILE ================== */
