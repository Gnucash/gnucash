/********************************************************************\
 * gnc-book.c -- dataset access (set of accounting books)           *
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
\********************************************************************/

/*
 * FILE:
 * gnc-book.c
 *
 * FUNCTION:
 * Encapsulate all the information about a gnucash dataset, including
 * the methods used to read and write them to datastores.
 *
 * HISTORY:
 * Created by Linas Vepstas December 1998
 * Copyright (c) 1998-2000 Linas Vepstas
 * Copyright (c) 2000 Dave Peticolas
 */

#include <dlfcn.h>
#include <fcntl.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "Backend.h"
#include "BackendP.h"
#include "FileIO.h"
#include "Group.h"
#include "NetIO.h"
#include "Scrub.h"
#include "TransLog.h"
#include "gnc-book.h"
#include "gnc-engine-util.h"

static short module = MOD_IO;

struct _gnc_book
{
  AccountGroup *topgroup;

  /* the requested book id, in the form or a URI, such as
   * file:/some/where, or sql:server.host.com:555
   */
  char *book_id;

  /* if any book subroutine failed, this records the failure reason 
   * (file not found, etc).
   * This is a 'stack' that is one deep.
   * FIXME: This is a hack.  I'm trying to move us away from static
   * global vars. This may be a temp fix if we decide to integrate
   * FileIO errors into GNCBook errors. */
  GNCBackendError last_err;

  /* ---------------------------------------------------- */
  /* the following struct members apply only for file-io */
  /* the fully-resolved path to the file */
  char *fullpath;

  /* name of lockfile, and filedescr */
  char * lockfile;
  char * linkfile;
  int lockfd;

  /* ---------------------------------------------------- */
  /* This struct member applies for network and SQL i/o */
  /* It is not currently used for file i/o, but maybe it should be ?? */
  Backend *backend;
};

/* ============================================================== */

static void
gnc_book_clear_error (GNCBook *book)
{
  book->last_err = ERR_BACKEND_NO_ERR;
}

static void
gnc_book_push_error (GNCBook *book, GNCBackendError err)
{
  book->last_err = err;
}

/* ============================================================== */

GNCBackendError
gnc_book_get_error (GNCBook * book)
{
  if (!book) return ERR_BACKEND_NO_BACKEND;
  return book->last_err;
}

GNCBackendError
gnc_book_pop_error (GNCBook * book)
{
  GNCBackendError err;
  if (!book) return ERR_BACKEND_NO_BACKEND;
  err = book->last_err;
  book->last_err = ERR_BACKEND_NO_ERR;
  return err;
}

/* ============================================================== */

static void
gnc_book_init (GNCBook *book)
{
  if (!book) return;

  book->topgroup = xaccMallocAccountGroup ();
  gnc_book_clear_error (book);
  book->lockfd = -1;
};

GNCBook *
gnc_book_new (void)
{
  GNCBook *book;

  book = g_new0(GNCBook, 1);

  gnc_book_init (book);

  return book;
}

/* ============================================================== */

AccountGroup * 
gnc_book_get_group (GNCBook *book)
{
   if (!book) return NULL;
   return book->topgroup;
}

void
gnc_book_set_group (GNCBook *book, AccountGroup *grp)
{
   if (!book) return;
   book->topgroup = grp;
}

/* ============================================================== */

Backend * 
xaccGNCBookGetBackend (GNCBook *book)
{
   if (!book) return NULL;
   return book->backend;
}

/* ============================================================== */

const char *
gnc_book_get_file_path (GNCBook *book)
{
   if (!book) return NULL;
   return book->fullpath;
}

/* ============================================================== */

const char *
gnc_book_get_url (GNCBook *book)
{
   if (!book) return NULL;
   return book->book_id;
}

/* ============================================================== */

static gboolean
gnc_book_get_file_lock (GNCBook *book)
{
  struct stat statbuf;
  char pathbuf[PATH_MAX];
  char *path = NULL;
  int rc;

  rc = stat (book->lockfile, &statbuf);
  if (!rc)
  {
    /* oops .. file is all locked up  .. */
    gnc_book_push_error (book, ERR_BACKEND_LOCKED);
    return FALSE;
  }

  book->lockfd = open (book->lockfile, O_RDWR | O_CREAT | O_EXCL , 0);
  if (book->lockfd < 0)
  {
    /* oops .. file is all locked up  .. */
    gnc_book_push_error (book, ERR_BACKEND_LOCKED);
    return FALSE;
  }

  /* OK, now work around some NFS atomic lock race condition 
   * mumbo-jumbo.  We do this by linking a unique file, and 
   * then examing the link count.  At least that's what the 
   * NFS programmers guide suggests. 
   * Note: the "unique filename" must be unique for the
   * triplet filename-host-process, otherwise accidental 
   * aliases can occur.
   */

  /* apparently, even this code may not work for some NFS
   * implementations. In the long run, I am told that 
   * ftp.debian.org
   *  /pub/debian/dists/unstable/main/source/libs/liblockfile_0.1-6.tar.gz
   * provides a better long-term solution.
   */

  strcpy (pathbuf, book->lockfile);
  path = strrchr (pathbuf, '.');
  sprintf (path, ".%lx.%d.LNK", gethostid(), getpid());
  link (book->lockfile, pathbuf);
  rc = stat (book->lockfile, &statbuf);
  if (rc)
  {
    /* oops .. stat failed!  This can't happen! */
    gnc_book_push_error (book, ERR_BACKEND_LOCKED);
    unlink (pathbuf);
    close (book->lockfd);
    unlink (book->lockfile);
    return FALSE;
  }

  if (statbuf.st_nlink != 2)
  {
    /* oops .. stat failed!  This can't happen! */
    gnc_book_push_error (book, ERR_BACKEND_LOCKED);
    unlink (pathbuf);
    close (book->lockfd);
    unlink (book->lockfile);
    return FALSE;
  }

  book->linkfile = g_strdup (pathbuf);

  return TRUE;
}

/* ============================================================== */

static gboolean
gnc_book_begin_file (GNCBook *book, const char * filefrag,
                     gboolean ignore_lock)
{
  ENTER (" filefrag=%s", filefrag);

  /* Try to find or build an absolute file path */

  book->fullpath = xaccResolveFilePath (filefrag);
  if (!book->fullpath)
  {
    gnc_book_push_error (book, ERR_FILEIO_FILE_NOT_FOUND);
    return FALSE;    /* ouch */
  }

  /* Store the sessionid URL also ... */
  book->book_id = g_strconcat ("file:", filefrag, NULL);

  /* ---------------------------------------------------- */
  /* We should now have a fully resolved path name.
   * Lets see if we can get a lock on it. */

  book->lockfile = g_strconcat(book->fullpath, ".LCK", NULL);

  if (!ignore_lock && !gnc_book_get_file_lock (book))
  {
    gnc_book_push_error (book, ERR_BACKEND_LOCKED);
    g_free (book->book_id);  book->book_id = NULL;
    g_free (book->fullpath); book->fullpath = NULL;
    g_free (book->lockfile); book->lockfile = NULL;
    return FALSE;
  }

  LEAVE (" ");
  return TRUE;
}

/* ============================================================== */

gboolean
gnc_book_begin (GNCBook *book, const char * book_id, 
                gboolean ignore_lock, gboolean create_if_nonexistent)
{
  int rc;

  if (!book) return FALSE;
  ENTER (" ignore_lock=%d, book-id=%s", ignore_lock, book_id);

  /* clear the error condition of previous errors */
  gnc_book_clear_error (book);

  /* check to see if this session is already open */
  if (book->book_id)
  {
    gnc_book_push_error (book, ERR_BACKEND_LOCKED);
    return FALSE;
  }

  /* seriously invalid */
  if (!book_id)
  {
    gnc_book_push_error (book, ERR_BACKEND_NO_BACKEND);
    return FALSE;
  }

  /* check to see if this is a type we know how to handle */
  if (!strncmp(book_id, "file:", 5))
  {
    /* add 5 to space past 'file:' */
    rc = gnc_book_begin_file (book, book_id + 5, ignore_lock);
    return rc;
  }

  /* -------------------------------------------------- */
  if ((!strncmp(book_id, "http://", 7)) ||
      (!strncmp(book_id, "https://", 8)) ||
      (!strncmp(book_id, "postgres://", 11)))
  {
    char *p, *filefrag;

    /* Store the sessionid URL  */
    book->book_id = g_strdup (book_id);

    /* We create a local filepath that correspnds to the URL.
     * This filepath is handy for logs, lock-files and other 
     * goodies that need to be stored in the local filesystem.
     * The local filename is generated by converting slashes 
     * in the URL to commas.
     */
    filefrag = g_strdup (book_id);
    p = strchr (filefrag, '/');
    while (p) {
       *p = ',';
       p = strchr (filefrag, '/');
    }
    book->fullpath = xaccResolveFilePath (filefrag);
    g_free (filefrag);
    if (!book->fullpath)
    {
      gnc_book_push_error (book, ERR_FILEIO_FILE_NOT_FOUND);
      return FALSE;    /* ouch */
    }
    PINFO ("filepath=%s\n", book->fullpath);


    /* load different backend based on URL.  We should probably
     * dynamically load these based on some config file ... */
    if ((!strncmp(book_id, "http://", 7)) ||
        (!strncmp(book_id, "https://", 8)))
    {
      /* create the backend */
      book->backend = xmlendNew();
    } else 
    if (!strncmp(book_id, "postgres://", 11))
    {
      char * dll_err;
      void * dll_handle;
      Backend * (*pg_new)(void);
 
      /* open and resolve all symbols now (we don't want mystery 
       * failure later) */
      dll_handle = dlopen ("libgnc_postgres.so", RTLD_NOW);
      if (! dll_handle) 
      {
        dll_err = dlerror();
        PWARN (" can't load library: %s\n", dll_err);
        g_free(book->fullpath);
        book->fullpath = NULL;
        g_free(book->book_id);
        book->book_id = NULL;
        gnc_book_push_error (book, ERR_BACKEND_NO_BACKEND);
        return FALSE;
      }

      /* For the postgres backend, do the equivalent of 
       * the statically loaded
       * book->backend = pgendNew (); */
      pg_new = dlsym (dll_handle, "pgendNew");
      dll_err = dlerror();
      if (dll_err) 
      {
        PWARN (" can't find symbol: %s\n", dll_err);
        g_free(book->fullpath);
        book->fullpath = NULL;
        g_free(book->book_id);
        book->book_id = NULL;
        gnc_book_push_error (book, ERR_BACKEND_NO_BACKEND);
        return FALSE;
      }

      book->backend = (*pg_new) ();
    }

    /* if there's a begin method, call that. */
    if (book->backend->book_begin)
    {
      GNCBackendError err;

      (book->backend->book_begin)(book, book->book_id, ignore_lock,
                                  create_if_nonexistent);

      err = xaccBackendGetError(book->backend);
      if (ERR_BACKEND_NO_ERR != err)
      {
        g_free(book->fullpath);
        book->fullpath = NULL;
        g_free(book->book_id);
        book->book_id = NULL;
        gnc_book_push_error (book, err);
        return FALSE;
      }
    }
    return TRUE;
  }

  /* -------------------------------------------------- */

  /* otherwise, lets just assume its a file. */
  rc = gnc_book_begin_file (book, book_id, ignore_lock);
  return rc;
}

/* ============================================================== */

gboolean
gnc_book_load (GNCBook *book)
{
  GNCBackendError retval;

  if (!book) return FALSE;
  if (!book->book_id) return FALSE;

  ENTER ("book_id=%s", book->book_id);

  if (strncmp(book->book_id, "file:", 5) == 0)
  {
    /* file: */

    if (!book->lockfile)
    {
      gnc_book_push_error (book, ERR_BACKEND_LOCKED);
      return FALSE;
    }
      
    /* At this point, we should are supposed to have a valid book 
     * id and a lock on the file. */

    xaccLogDisable();
    xaccFreeAccountGroup (book->topgroup);
    book->topgroup = NULL;

    xaccLogSetBaseName(book->fullpath);
    gnc_book_clear_error (book);
    book->topgroup = xaccReadAccountGroupFile (book->fullpath,
                                               &retval);
    if (ERR_BACKEND_NO_ERR != retval) gnc_book_push_error (book, retval);
    xaccLogEnable();

    if (!book->topgroup || (gnc_book_get_error(book) != ERR_BACKEND_NO_ERR))
    {
      return FALSE;
    }

    xaccGroupScrubSplits (book->topgroup);

    LEAVE("book_id=%s", book->book_id);
    return TRUE;
  }

  else if ((strncmp(book->book_id, "http://", 7) == 0) ||
           (strncmp(book->book_id, "https://", 8) == 0) ||
           (strncmp(book->book_id, "postgres://", 11) == 0))
  {
    /* This code should be sufficient to initiliaze *any* backend,
     * whether http, postgres, or anything else that might come along.
     * Basically, the idea is that by now, a backend has already been
     * created & set up.  At this point, we only need to get the
     * top-level account group out of the backend, and that is a
     * generic, backend-independent operation.
     */
    Backend *be = book->backend;
    xaccLogDisable();
    xaccFreeAccountGroup (book->topgroup);
    xaccLogEnable();
    book->topgroup = NULL;

    gnc_book_clear_error (book);

    /* starting the session should result in a bunch of accounts
     * and currencies being downloaded, but probably no transactions;
     * The GUI will need to do a query for that.
     */
    if (be && be->book_load) 
    {
       xaccLogDisable();
       xaccLogSetBaseName(book->fullpath);

       book->topgroup = (be->book_load) (be);

       xaccGroupSetBackend (book->topgroup, be);

       gnc_book_push_error(book, xaccBackendGetError(be));
       xaccLogEnable();
    }

    LEAVE("book_id=%s", book->book_id);
    return TRUE;
  }
  else
  {
    gnc_book_push_error (book, ERR_BACKEND_NO_BACKEND);
    return FALSE;
  }  
}

/* ============================================================== */

gboolean
gnc_book_save_may_clobber_data (GNCBook *book)
{
  /* FIXME: Make sure this doesn't need more sophisticated semantics
   * in the face of special file, devices, pipes, symlinks, etc. */

  struct stat statbuf;

  if (!book) return FALSE;
  if (!book->fullpath) return FALSE;
  if (stat(book->fullpath, &statbuf) == 0) return TRUE;

  return FALSE;
}

/* ============================================================== */

void
gnc_book_save (GNCBook *book)
{
  GNCBackendError retval;
  Backend *be;
  if (!book) return;

  ENTER ("book_id=%s", book->book_id);
  /* if there is a backend, and the backend is reachablele
   * (i.e. we can communicate with it), then synchronize with 
   * the backend.  If we cannot contact the backend (e.g.
   * because we've gone offline, the network has crashed, etc.)
   * then give the user the option to save to disk. 
   */
  be = book->backend;
  if (be && be->sync && book->topgroup)
  {
     (be->sync)(be, book->topgroup);
     retval = xaccBackendGetError(be);

     if (ERR_BACKEND_NO_ERR != retval) 
     {
       gnc_book_push_error (book, retval);

       /* we close the backend here ... isn't this a bit harsh ??? */
       if (be->book_end)
       {
          (be->book_end)(be);
       }
     }
     return;
  } 

  /* if the fullpath doesn't exist, either the user failed to initialize,
   * or the lockfile was never obtained. Either way, we can't write. */
  gnc_book_clear_error (book);

  if (!book->fullpath)
  {
    gnc_book_push_error (book, ERR_BACKEND_MISC);
    return;
  }

  if (book->topgroup)
  {
    xaccWriteAccountGroupFile (book->fullpath, book->topgroup,
                                               TRUE, &retval);
    if (ERR_BACKEND_NO_ERR != retval) gnc_book_push_error (book, retval);
  }
  LEAVE(" ");
}

/* ============================================================== */

void
gnc_book_end (GNCBook *book)
{
  if (!book) return;

  ENTER ("book_id=%s", book->book_id);
  /* close down the backend first */
  if (book->backend && book->backend->book_end)
  {
    (book->backend->book_end)(book->backend);
  }

  gnc_book_clear_error (book);

  if (book->linkfile)
    unlink (book->linkfile);

  if (book->lockfd > 0)
    close (book->lockfd);

  if (book->lockfile)
    unlink (book->lockfile);

  g_free (book->book_id);
  book->book_id = NULL;

  g_free (book->fullpath);
  book->fullpath = NULL;

  g_free (book->lockfile);
  book->lockfile = NULL;

  g_free (book->linkfile);
  book->linkfile = NULL;
  LEAVE(" ");
}

void 
gnc_book_destroy (GNCBook *book) 
{
  if (!book) return;
  ENTER ("book_id=%s", book->book_id);

  xaccLogDisable();
  gnc_book_end (book);

  /* destroy the backend */
  if (book->backend) g_free(book->backend);
  xaccGroupSetBackend (book->topgroup, NULL);

  xaccFreeAccountGroup (book->topgroup);
  book->topgroup = NULL;
  xaccLogEnable();

  g_free (book);
  LEAVE(" ");
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
  char *home;
  char *path;
  char *data;

  /* Punt. Can't figure out where home is. */
  home = getenv ("HOME");
  if (!home) return;

  path = g_strconcat(home, "/.gnucash", NULL);

  rc = stat (path, &statbuf);
  if (rc)
  {
    /* assume that the stat failed only because the dir is absent,
     * and not because its read-protected or other error.
     * Go ahead and make it. Don't bother much with checking mkdir 
     * for errors; seems pointless. */
    mkdir (path, S_IRWXU);   /* perms = S_IRWXU = 0700 */
  }

  data = g_strconcat (path, "/data", NULL);
  rc = stat (data, &statbuf);
  if (rc)
    mkdir (data, S_IRWXU);

  g_free (path);
  g_free (data);
}

/* ============================================================== */
/* XXX hack alert -- we should be yanking this out of some config file */
static char * searchpaths[] =
{
   "/usr/share/gnucash/data/",
   "/usr/local/share/gnucash/data/",
   "/usr/share/gnucash/accounts/",
   "/usr/local/share/gnucash/accounts/",
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
  ENTER ("filefrag=%s", filefrag);

  /* ---------------------------------------------------- */
  /* OK, now we try to find or build an absolute file path */

  /* check for an absolute file path */
  if ('/' == *filefrag)
    return g_strdup (filefrag);

  /* get conservative on the length so that sprintf(getpid()) works ... */
  /* strlen ("/.LCK") + sprintf (%x%d) */
  namelen = strlen (filefrag) + 25; 

  for (i = -2; TRUE ; i++) 
  {
    switch (i)
    {
      case -2:
        /* try to find a file by this name in the cwd ... */
        path = getcwd (pathbuf, PATH_MAX);
        if (!path)
          continue;

        len = strlen (path) + namelen;
        if (PATH_MAX <= len)
          continue;

        strcat (path, "/");
        break;

      case -1:
        /* look for something in $HOME/.gnucash/data */
        path = getenv ("HOME");
        if (!path)
          continue;

        len = strlen (path) + namelen + 20;
        if (PATH_MAX <= len)
          continue;

        strcpy (pathbuf, path);
        strcat (pathbuf, "/.gnucash/data/");
        path = pathbuf;
        break;

      default:
        /* OK, check the user-configured paths */
        path = searchpaths[i];
        if (path)
        {
          len = strlen (path) + namelen;
          if (PATH_MAX <= len)
            continue;

          strcpy (pathbuf, path);
          path = pathbuf;
        }
        break;
    }

    if (!path) break;

    /* lets see if we found the file here ... */
    /* haral: if !S_ISREG: there is something with that name 
     * but it's not a regular file */
    strcat (path, filefrag);
    rc = stat (path, &statbuf);
    if ((!rc) && (S_ISREG(statbuf.st_mode)))
      return (g_strdup (path));
  }

  /* make sure that the gnucash home dir exists. */
  MakeHomeDir();

  /* OK, we didn't find the file. */
  /* If the user specified a simple filename (i.e. no slashes in it)
   * then create the file.  But if it has slashes in it, then creating
   * a bunch of directories seems like a bad idea; more likely, the user
   * specified a bad filename.  So return with error. */
  if (strchr (filefrag, '/'))
    return NULL;

  /* Lets try creating a new file in $HOME/.gnucash/data */
  path = getenv ("HOME");
  if (path)
  {
    len = strlen (path) + namelen + 50;
    if (PATH_MAX > len)
    {
      strcpy (pathbuf, path);
      strcat (pathbuf, "/.gnucash/data/");
      strcat (pathbuf, filefrag);
      return (g_strdup (pathbuf));
    }
  } 

  /* OK, we still didn't find the file */
  /* Lets try creating a new file in the cwd */
  path = getcwd (pathbuf, PATH_MAX);
  if (path)
  {
    len = strlen (path) + namelen;
    if (PATH_MAX > len)
    {
      strcat (path, "/");
      strcat (path, filefrag);
      return (g_strdup (path));
    }
  }

  return NULL;
}

/* ============================================================== */

char * 
xaccResolveURL (const char * pathfrag)
{
  /* seriously invalid */
  if (!pathfrag) return NULL;

  /* At this stage of checking, URL's are always, by definition,
   * resolved.  If there's an error connecting, we'll find out later.
   *
   * FIXME -- we should probably use  ghttp_uri_validate
   * to make sure hte uri is in good form...
   */

  if (!strncmp (pathfrag, "http://", 7)      ||
      !strncmp (pathfrag, "https://", 8)     ||
      !strncmp (pathfrag, "postgres://", 11) ) 
  {
    return g_strdup(pathfrag);
  }

  if (!strncmp (pathfrag, "file:", 5)) {
    return (xaccResolveFilePath (pathfrag+5));
  }

  return (xaccResolveFilePath (pathfrag));
}

/* ==================== END OF FILE ================== */
