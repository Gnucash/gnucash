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

#include "config.h"

#include <dlfcn.h>
#include <fcntl.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <errno.h>

#include "Backend.h"
#include "BackendP.h"
#include "Group.h"
#include "NetIO.h"
#include "Scrub.h"
#include "TransLog.h"
#include "gnc-engine-util.h"
#include "gnc-pricedb-p.h"
#include "DateUtils.h"
#include "io-gncxml.h"
#include "io-gncbin.h"
#include "io-gncxml-v2.h"

#include "gnc-book.h"
#include "gnc-book-p.h"
#include "gnc-engine.h"
#include "gnc-engine-util.h"

static short module = MOD_IO;

struct _gnc_book
{
  AccountGroup *topgroup;
  GNCPriceDB *pricedb;

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
  char *error_message;
    
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

/* ---------------------------------------------------------------------- */

static void
gnc_book_clear_error (GNCBook *book)
{
  book->last_err = ERR_BACKEND_NO_ERR;
  if(book->error_message)
  {
      g_free(book->error_message);
      book->error_message = NULL;
  }
}

static void
gnc_book_push_error (GNCBook *book, GNCBackendError err, char *message)
{
  book->last_err = err;
  book->error_message = message;
}

/* ---------------------------------------------------------------------- */

GNCBackendError
gnc_book_get_error (GNCBook * book)
{
  if (!book) return ERR_BACKEND_NO_BACKEND;
  return book->last_err;
}

static const char *
get_default_error_message(GNCBackendError err)
{
    return "";
}

const char *
gnc_book_get_error_message(GNCBook *book)
{
    if(!book) return "";
    if(!book->error_message) return get_default_error_message(book->last_err);
    return book->error_message;
}

GNCBackendError
gnc_book_pop_error (GNCBook * book)
{
  GNCBackendError err;
  if (!book) return ERR_BACKEND_NO_BACKEND;
  err = book->last_err;
  gnc_book_clear_error(book);
  return err;
}

/* ---------------------------------------------------------------------- */

static void
gnc_book_init (GNCBook *book)
{
  if(!book) return;

  book->topgroup = xaccMallocAccountGroup();
  book->pricedb = gnc_pricedb_create();
  book->book_id = NULL;
  gnc_book_clear_error (book);
  book->fullpath = NULL;
  book->lockfile = NULL;
  book->linkfile = NULL;
  book->lockfd = -1;
  book->backend = NULL;
}

GNCBook *
gnc_book_new (void)
{
  GNCBook *book = g_new0(GNCBook, 1);
  gnc_book_init(book);
  return book;
}

/* ---------------------------------------------------------------------- */

gnc_commodity_table*
gnc_book_get_commodity_table(GNCBook *book)
{
    return gnc_engine_commodities();
}

AccountGroup * 
gnc_book_get_group (GNCBook *book)
{
   if (!book) return NULL;
   return book->topgroup;
}

void
gnc_book_set_group (GNCBook *book, AccountGroup *grp)
{
  if(!book) return;
  if(book->topgroup) xaccFreeAccountGroup(book->topgroup);
  book->topgroup = grp;
}

/* ---------------------------------------------------------------------- */

static int
counter_thunk(Transaction *t, void *data)
{
    (*((guint*)data))++;
    return 0;
}

guint
gnc_book_count_transactions(GNCBook *book)
{
    guint count = 0;
    xaccGroupForEachTransaction(gnc_book_get_group(book),
                                counter_thunk, (void*)&count);
    return count;
}

/* ---------------------------------------------------------------------- */

GNCPriceDB *
gnc_book_get_pricedb(GNCBook *book)
{
  if(!book) return NULL;
  return book->pricedb;
}

void
gnc_book_set_pricedb(GNCBook *book, GNCPriceDB *db)
{
  if(!book) return;
  book->pricedb = db;
}

/* ---------------------------------------------------------------------- */

Backend * 
xaccGNCBookGetBackend (GNCBook *book)
{
   if (!book) return NULL;
   return book->backend;
}

/* ---------------------------------------------------------------------- */

const char *
gnc_book_get_file_path (GNCBook *book)
{
   if (!book) return NULL;
   return book->fullpath;
}

/* ---------------------------------------------------------------------- */

const char *
gnc_book_get_url (GNCBook *book)
{
   if (!book) return NULL;
   return book->book_id;
}

/* ---------------------------------------------------------------------- */

void
gnc_book_mark_saved(GNCBook *book)
{
  xaccGroupMarkSaved(gnc_book_get_group(book));
  gnc_pricedb_mark_clean(gnc_book_get_pricedb(book));
}

/* ---------------------------------------------------------------------- */
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
    gnc_book_push_error (book, ERR_BACKEND_LOCKED, NULL);
    return FALSE;
  }

  book->lockfd = open (book->lockfile, O_RDWR | O_CREAT | O_EXCL , 0);
  if (book->lockfd < 0)
  {
    /* oops .. file is all locked up  .. */
    gnc_book_push_error (book, ERR_BACKEND_LOCKED, NULL);
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
    gnc_book_push_error (book, ERR_BACKEND_LOCKED, NULL);
    unlink (pathbuf);
    close (book->lockfd);
    unlink (book->lockfile);
    return FALSE;
  }

  if (statbuf.st_nlink != 2)
  {
    /* oops .. stat failed!  This can't happen! */
    gnc_book_push_error (book, ERR_BACKEND_LOCKED, NULL);
    unlink (pathbuf);
    close (book->lockfd);
    unlink (book->lockfile);
    return FALSE;
  }

  book->linkfile = g_strdup (pathbuf);

  return TRUE;
}

/* ---------------------------------------------------------------------- */

GNCBookFileType
gnc_book_determine_file_type(GNCBook *book)
{
    const gchar *name = gnc_book_get_file_path(book);
    if(gnc_is_xml_data_file_v2(name)) {
        return GNC_BOOK_XML2_FILE;
    } else if(gnc_is_xml_data_file(name)) {
        return GNC_BOOK_XML1_FILE;
    } else {
        return GNC_BOOK_BIN_FILE;
    }
}


/* Load financial data from a file into the book, automtically
   detecting the format of the file, if possible.  Return FALSE on
   error, and set the error parameter to indicate what went wrong if
   it's not NULL.  This function does not manage file locks in any
   way. */

static gboolean
happy_or_push_error(GNCBook *book, gboolean errret, GNCBackendError errcode)
{
    if(errret) {
        return TRUE;
    } else {
        gnc_book_push_error(book, errcode, NULL);
        return FALSE;
    }
}

static gboolean
gnc_book_load_from_file(GNCBook *book)
{
  const gchar *name = gnc_book_get_file_path(book);
  if(!name) return FALSE;

  switch (gnc_book_determine_file_type(book))
  {
  case GNC_BOOK_XML2_FILE:
      return happy_or_push_error(book,
                                 gnc_book_load_from_xml_file_v2(book, NULL),
                                 ERR_BACKEND_MISC);
  case GNC_BOOK_XML1_FILE:
      return happy_or_push_error(book,
                                 gnc_book_load_from_xml_file(book),
                                 ERR_BACKEND_MISC);
  case GNC_BOOK_BIN_FILE:
  {
    /* presume it's an old-style binary file */
    GNCBackendError error;

    gnc_book_load_from_binfile(book);
    error = gnc_book_get_binfile_io_error();

    if(error == ERR_BACKEND_NO_ERR) {
      return TRUE;
    } else {
      gnc_book_push_error(book, error, NULL);
      return FALSE;
    }
  }
  default:
      g_warning("File not any known type");
      gnc_book_push_error(book, ERR_FILEIO_UNKNOWN_FILE_TYPE, NULL);
      return FALSE;
      break;
  }
}

/* ---------------------------------------------------------------------- */

/* Write the financial data in a book to a file, returning FALSE on
   error and setting the error_result to indicate what went wrong if
   it's not NULL.  This function does not manage file locks in any
   way.

  If make_backup is true, write out a time-stamped copy of the file
  into the same directory as the indicated file, with a filename of
  "file.YYYYMMDDHHMMSS.xac" where YYYYMMDDHHMMSS is replaced with the
  current year/month/day/hour/minute/second. */

static gboolean
gnc_book_backup_file(GNCBook *book)
{
    char *timestamp;
    char *backup;
    const char *datafile = gnc_book_get_file_path(book);
    struct stat statbuf;
    int rc;

    rc = stat (datafile, &statbuf);
    if (rc)
      return (errno == ENOENT);

    if(gnc_book_determine_file_type(book) == GNC_BOOK_BIN_FILE)
    {
        /* make a more permament safer backup */
        const char *back = "-binfmt.bkup";
        char *bin_bkup = g_new(char, strlen(datafile) + strlen(back) + 1);
        strcpy(bin_bkup, datafile);
        strcat(bin_bkup, back);
        if(link(datafile, bin_bkup) != 0)
        {
            gnc_book_push_error(
                book, ERR_BACKEND_MISC,
                g_strdup_printf("unable to make bin file backup: %s",
                                strerror(errno) ? strerror(errno) : ""));
            g_free(bin_bkup);
            return FALSE;
        }
        g_free(bin_bkup);
    }
    
    timestamp = xaccDateUtilGetStampNow ();
    backup = g_new (char, strlen (datafile) + strlen (timestamp) + 6);
    strcpy (backup, datafile);
    strcat (backup, ".");
    strcat (backup, timestamp);
    strcat (backup, ".xac");
    free (timestamp);
    if(link(datafile, backup) != 0)
    {
        gnc_book_push_error(
            book, ERR_BACKEND_MISC,
            g_strdup_printf("unable to link backup file: %s",
                            strerror(errno) ? strerror(errno) : ""));
        g_free(backup);
        return FALSE;
    }
    
    g_free(backup);

    return TRUE;
}

static gboolean
gnc_book_write_to_file(GNCBook *book,
                       gboolean make_backup)
{
  const gchar *datafile = gnc_book_get_file_path(book);
  char *tmp_name;

  tmp_name = g_new(char, strlen(datafile) + 12);
  strcpy(tmp_name, datafile);
  strcat(tmp_name, ".tmp-XXXXXX");

  if(!mktemp(tmp_name))
  {
      gnc_book_push_error(book, ERR_BACKEND_MISC,
                          g_strdup("unable to create temporary file name"));
      return FALSE;
  }
  
  if(make_backup)
  {
      if(!gnc_book_backup_file(book))
      {
          return FALSE;
      }
  }
  
  if(gnc_book_write_to_xml_file_v2(book, tmp_name)) 
  {
      if(unlink(datafile) != 0 && errno != ENOENT)
      {
          gnc_book_push_error(
              book, ERR_BACKEND_MISC,
              g_strdup_printf("unable to unlink filename %s: %s",
                              datafile ? datafile : "(null)",
                              strerror(errno) ? strerror(errno) : ""));
          g_free(tmp_name);
          return FALSE;
      }
      if(link(tmp_name, datafile) != 0)
      {
          gnc_book_push_error(
              book, ERR_BACKEND_MISC,
              g_strdup_printf("unable to link from temp filename %s to "
                              "real filename %s: %s",
                              tmp_name ? tmp_name : "(null)",
                              datafile ? datafile : "(null)",
                              strerror(errno) ? strerror(errno) : ""));
          g_free(tmp_name);
          return FALSE;
      }
      if(unlink(tmp_name) != 0)
      {
          gnc_book_push_error(
              book, ERR_BACKEND_MISC,
              g_strdup_printf("unable to unlink temp filename %s: %s",
                              tmp_name ? tmp_name : "(null)",
                              strerror(errno) ? strerror(errno) : ""));
          g_free(tmp_name);
          return FALSE;
      }
      g_free(tmp_name);
      return TRUE;
  }
  else
  {
      if(unlink(tmp_name) != 0)
      {
          gnc_book_push_error(
              book, ERR_BACKEND_MISC,
              g_strdup_printf("unable to unlink temp_filename %s: %s",
                              tmp_name ? tmp_name : "(null)",
                              strerror(errno) ? strerror(errno) : ""));
          /* already in an error just flow on through */
      }
      g_free(tmp_name);
      gnc_book_push_error(book, ERR_BACKEND_MISC, NULL);
      return FALSE;
  }
}

/* ---------------------------------------------------------------------- */

static gboolean
gnc_book_begin_file (GNCBook *book, const char * filefrag,
                     gboolean ignore_lock)
{
  ENTER (" filefrag=%s", filefrag ? filefrag : "(null)");

  /* Try to find or build an absolute file path */

  book->fullpath = xaccResolveFilePath (filefrag);
  if (!book->fullpath)
  {
    gnc_book_push_error (book, ERR_FILEIO_FILE_NOT_FOUND, NULL);
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
    gnc_book_push_error (book, ERR_BACKEND_LOCKED, NULL);
    g_free (book->book_id);  book->book_id = NULL;
    g_free (book->fullpath); book->fullpath = NULL;
    g_free (book->lockfile); book->lockfile = NULL;
    return FALSE;
  }

  LEAVE (" ");
  return TRUE;
}

/* ---------------------------------------------------------------------- */

gboolean
gnc_book_begin (GNCBook *book, const char * book_id, 
                gboolean ignore_lock, gboolean create_if_nonexistent)
{
  int rc;

  if (!book) return FALSE;
  ENTER (" ignore_lock=%d, book-id=%s", ignore_lock,
         book_id ? book_id : "(null)");

  /* clear the error condition of previous errors */
  gnc_book_clear_error (book);

  /* check to see if this session is already open */
  if (book->book_id)
  {
    gnc_book_push_error (book, ERR_BACKEND_LOCKED, NULL);
    return FALSE;
  }

  /* seriously invalid */
  if (!book_id)
  {
    gnc_book_push_error (book, ERR_BACKEND_NO_BACKEND, NULL);
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
      (!strncmp(book_id, "postgres://", 11)) ||
      (!strncmp(book_id, "rpc://", 6)))
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
      gnc_book_push_error (book, ERR_FILEIO_FILE_NOT_FOUND, NULL);
      return FALSE;    /* ouch */
    }

    PINFO ("filepath=%s\n", book->fullpath ? book->fullpath : "(null)");

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
        PWARN (" can't load library: %s\n", dll_err ? dll_err : "");
        g_free(book->fullpath);
        book->fullpath = NULL;
        g_free(book->book_id);
        book->book_id = NULL;
        gnc_book_push_error (book, ERR_BACKEND_NO_BACKEND, NULL);
        return FALSE;
      }

      /* For the postgres backend, do the equivalent of 
       * the statically loaded
       * book->backend = pgendNew (); */
      pg_new = dlsym (dll_handle, "pgendNew");
      dll_err = dlerror();
      if (dll_err) 
      {
        PWARN (" can't find symbol: %s\n", dll_err ? dll_err : "");
        g_free(book->fullpath);
        book->fullpath = NULL;
        g_free(book->book_id);
        book->book_id = NULL;
        gnc_book_push_error (book, ERR_BACKEND_NO_BACKEND, NULL);
        return FALSE;
      }

      book->backend = (*pg_new) ();
    } else
    if (!strncmp(book_id, "rpc://", 6))
    {
      char * dll_err;
      void * dll_handle;
      Backend * (*rpc_new)(void);
 
      /* open and resolve all symbols now (we don't want mystery 
       * failure later) */
      dll_handle = dlopen ("libgnc_rpc.so", RTLD_NOW);
      if (! dll_handle) 
      {
        dll_err = dlerror();
        PWARN (" can't load library: %s\n", dll_err ? dll_err : "");
        g_free(book->fullpath);
        book->fullpath = NULL;
        g_free(book->book_id);
        book->book_id = NULL;
        gnc_book_push_error (book, ERR_BACKEND_NO_BACKEND, NULL);
        return FALSE;
      }

      /* For the rpc backend, do the equivalent of 
       * the statically loaded
       * book->backend = pgendNew (); */
      rpc_new = dlsym (dll_handle, "rpcendNew");
      dll_err = dlerror();
      if (dll_err) 
      {
        PWARN (" can't find symbol: %s\n", dll_err ? dll_err : "");
        g_free(book->fullpath);
        book->fullpath = NULL;
        g_free(book->book_id);
        book->book_id = NULL;
        gnc_book_push_error (book, ERR_BACKEND_NO_BACKEND, NULL);
        return FALSE;
      }

      book->backend = (*rpc_new) ();
    }

    /* if there's a begin method, call that. */
    if (book->backend && book->backend->book_begin)
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
        gnc_book_push_error (book, err, NULL);
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

/* ---------------------------------------------------------------------- */

gboolean
gnc_book_load (GNCBook *book)
{
  GNCBackendError backend_err;

  if (!book) return FALSE;
  if (!book->book_id) return FALSE;

  ENTER ("book_id=%s", book->book_id ? book->book_id : "(null)");

  if (strncmp(book->book_id, "file:", 5) == 0)
  {
    /* file: */

    if (!book->lockfile)
    {
      gnc_book_push_error (book, ERR_BACKEND_LOCKED, NULL);
      return FALSE;
    }
      
    /* At this point, we should are supposed to have a valid book 
     * id and a lock on the file. */

    xaccLogDisable();
    xaccGroupMarkDoFree (book->topgroup);
    xaccFreeAccountGroup (book->topgroup);
    book->topgroup = NULL;
    gnc_pricedb_destroy(book->pricedb);
    book->pricedb = NULL;

    xaccLogSetBaseName(book->fullpath);

    gnc_book_clear_error (book);
    gnc_book_load_from_file(book);

    xaccLogEnable();

    if (!book->topgroup) return FALSE;
    if (!book->pricedb) return FALSE;
    if (gnc_book_get_error(book) != ERR_BACKEND_NO_ERR) return FALSE;

    xaccGroupScrubSplits (book->topgroup);

    LEAVE("book_id=%s", book->book_id ? book->book_id : "(null)");
    return TRUE;
  }
  else if ((strncmp(book->book_id, "http://", 7) == 0) ||
           (strncmp(book->book_id, "https://", 8) == 0) ||
           (strncmp(book->book_id, "postgres://", 11) == 0) ||
	   (strncmp(book->book_id, "rpc://", 6)) == 0)
  {
    /* This code should be sufficient to initialize *any* backend,
     * whether http, postgres, or anything else that might come along.
     * Basically, the idea is that by now, a backend has already been
     * created & set up.  At this point, we only need to get the
     * top-level account group out of the backend, and that is a
     * generic, backend-independent operation.
     */
    Backend *be = book->backend;
    xaccLogDisable();
    xaccGroupMarkDoFree (book->topgroup);
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

       gnc_book_push_error(book, xaccBackendGetError(be), NULL);
       xaccLogEnable();
    }

    LEAVE("book_id=%s", book->book_id ? book->book_id : "(null)");
    return TRUE;
  } 
  else
  {
    gnc_book_push_error (book, ERR_BACKEND_NO_BACKEND, NULL);
    return FALSE;
  }  
}

/* ---------------------------------------------------------------------- */

gboolean
gnc_book_not_saved(GNCBook *book)
{
  if(!book) return FALSE;

  return(xaccGroupNotSaved(book->topgroup)
         ||
         gnc_pricedb_dirty(book->pricedb));
}

/* ---------------------------------------------------------------------- */

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

/* ---------------------------------------------------------------------- */

void
gnc_book_save (GNCBook *book)
{
  Backend *be;

  if (!book) return;

  ENTER ("book_id=%s", book->book_id ? book->book_id : "(null)");

  /* if there is a backend, and the backend is reachablele
   * (i.e. we can communicate with it), then synchronize with 
   * the backend.  If we cannot contact the backend (e.g.
   * because we've gone offline, the network has crashed, etc.)
   * then give the user the option to save to disk. 
   */
  be = book->backend;
  if (be && be->sync && book->topgroup) {
    GNCBackendError err;
    
    /* if invoked as SaveAs(), then backend not yet set */
    xaccGroupSetBackend (book->topgroup, be);

    (be->sync)(be, book->topgroup);
    err = xaccBackendGetError(be);
    
    if (ERR_BACKEND_NO_ERR != err) {
      gnc_book_push_error (book, err, NULL);
      
      /* we close the backend here ... isn't this a bit harsh ??? */
      if (be->book_end) {
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
    gnc_book_push_error (book, ERR_BACKEND_MISC, NULL);
    return;
  }

  if (book->topgroup) {
    if(!gnc_book_write_to_file(book, TRUE)) {
      gnc_book_push_error (book, ERR_BACKEND_MISC, NULL);
    }
  }
  LEAVE(" ");
}

/* ---------------------------------------------------------------------- */

void
gnc_book_end (GNCBook *book)
{
  if (!book) return;

  ENTER ("book_id=%s", book->book_id ? book->book_id : "(null)");

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

  ENTER ("book_id=%s", book->book_id ? book->book_id : "(null)");

  xaccLogDisable();
  gnc_book_end (book);

  /* destroy the backend */
  if (book->backend) g_free(book->backend);
  xaccGroupSetBackend (book->topgroup, NULL);

  /* mark the accounts as being freed
   * to avoid tons of balance recomputations. */
  xaccGroupMarkDoFree (book->topgroup);

  xaccFreeAccountGroup (book->topgroup);
  book->topgroup = NULL;

  gnc_pricedb_destroy (book->pricedb);
  book->pricedb = NULL;

  xaccLogEnable();

  g_free (book);
  LEAVE(" ");
}

gboolean
gnc_book_events_pending (GNCBook *book)
{
  if (!book) return FALSE;
  if (!book->backend) return FALSE;
  if (!book->backend->events_pending) return FALSE;

  return book->backend->events_pending (book->backend);
}

gboolean
gnc_book_process_events (GNCBook *book)
{
  if (!book) return FALSE;
  if (!book->backend) return FALSE;
  if (!book->backend->process_events) return FALSE;

  return book->backend->process_events (book->backend);
}

/* ---------------------------------------------------------------------- */
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

/* ---------------------------------------------------------------------- */
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

/* ---------------------------------------------------------------------- */

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
      !strncmp (pathfrag, "postgres://", 11) ||
      !strncmp (pathfrag, "rpc://", 6))
  {
    return g_strdup(pathfrag);
  }

  if (!strncmp (pathfrag, "file:", 5)) {
    return (xaccResolveFilePath (pathfrag+5));
  }

  return (xaccResolveFilePath (pathfrag));
}


void
gnc_run_rpc_server (void)
{
  char * dll_err;
  void * dll_handle;
  int (*rpc_run)(short);
  int ret;
 
  /* open and resolve all symbols now (we don't want mystery 
   * failure later) */
  dll_handle = dlopen ("libgnc_rpc.so", RTLD_NOW);
  if (! dll_handle) 
  {
    dll_err = dlerror();
    PWARN (" can't load library: %s\n", dll_err ? dll_err : "");
    return;
  }
  
  rpc_run = dlsym (dll_handle, "rpc_server_run");
  dll_err = dlerror();
  if (dll_err) 
  {
    dll_err = dlerror();
    PWARN (" can't find symbol: %s\n", dll_err ? dll_err : "");
    return;
  }
  
  ret = (*rpc_run)(0);

  /* XXX How do we force an exit? */
}
