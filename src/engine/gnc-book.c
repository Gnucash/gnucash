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
 * Copyright (c) 1998-2001 Linas Vepstas
 * Copyright (c) 2000 Dave Peticolas
 */

#include "config.h"

#include <dlfcn.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include <glib.h>

#include "Backend.h"
#include "BackendP.h"
#include "Group.h"
#include "NetIO.h"
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

void
gnc_book_push_error (GNCBook *book, GNCBackendError err, char *message)
{
  book->last_err = err;
  book->error_message = message;
}

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

const char *TEMPLATE_ACCOUNT_NAME = "__account for template transactions__";

static void
gnc_book_init (GNCBook *book)
{
  Account *template_acct;

  if(!book) return;

  book->topgroup = xaccMallocAccountGroup();
  book->pricedb = gnc_pricedb_create();

  book->sched_xactions = NULL;
  book->sx_notsaved = FALSE;
  book->template_group = xaccMallocAccountGroup();

  book->book_id = NULL;
  gnc_book_clear_error (book);
  book->fullpath = NULL;
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

  /* Do not free the old topgroup here unless you also fix
   * all the other uses of gnc_book_set_group! */

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

GList *
gnc_book_get_schedxactions( GNCBook *book )
{
        if ( book == NULL ) return NULL;
        return book->sched_xactions;
}

void
gnc_book_set_schedxactions( GNCBook *book, GList *newList )
{
  if ( book == NULL ) return;
  book->sched_xactions = newList;
  book->sx_notsaved = TRUE;
  return;
}

AccountGroup *
gnc_book_get_template_group( GNCBook *book )
{
        if ( book == NULL ) return NULL;
        return book->template_group;
}

void
gnc_book_set_template_group( GNCBook *book, AccountGroup *templateGroup )
{
        if ( book == NULL ) return;
        book->template_group = templateGroup;
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

static void
mark_sx_clean(gpointer data, gpointer user_data)
{
  SchedXaction *sx = (SchedXaction *) data;
  xaccSchedXactionSetDirtyness(sx, FALSE);
  return;
}

static void
book_sxns_mark_saved(GNCBook *book)
{
  book->sx_notsaved = FALSE;
  g_list_foreach(gnc_book_get_schedxactions(book),
		 mark_sx_clean, 
		 NULL);
  return;
}
void
gnc_book_mark_saved(GNCBook *book)
{
  /* FIXME: is this the right behaviour if book == NULL? */
  g_return_if_fail(book);
  xaccGroupMarkSaved(gnc_book_get_group(book));
  gnc_pricedb_mark_clean(gnc_book_get_pricedb(book));
  
  xaccGroupMarkSaved(gnc_book_get_template_group(book));
  book_sxns_mark_saved(book);
  
  return;
}


/* ---------------------------------------------------------------------- */

static void
gnc_book_int_backend_load_error(GNCBook *book, char *message, char *dll_err)
{
    PWARN (message, dll_err ? dll_err : "");
    g_free(book->fullpath);
    book->fullpath = NULL;
    g_free(book->book_id);
    book->book_id = NULL;
    gnc_book_push_error (book, ERR_BACKEND_NO_BACKEND, NULL);
}

static void
gnc_book_int_load_backend(GNCBook *book, char *libname, char* funcname,
                          char *libloaderrmsg)
{
    
    char * dll_err;
    void * dll_handle;
    Backend * (*be_new)(void);
 
    /* open and resolve all symbols now (we don't want mystery 
     * failure later) */
    dll_handle = dlopen (libname, RTLD_NOW);
    if (! dll_handle) 
    {
        char *errmsg;

        errmsg = g_strdup_printf(" can't load library: %%s\n%s",
                                 libloaderrmsg);
        dll_err = dlerror();
        gnc_book_int_backend_load_error(book, errmsg, dll_err);
        g_free(errmsg);
        return;
    }

    be_new = dlsym (dll_handle, funcname);
    dll_err = dlerror();
    if (dll_err) 
    {
        gnc_book_int_backend_load_error(book, " can't find symbol: %s\n",
                                        dll_err);
        return;
    }

    book->backend = (*be_new) ();
}

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
  if (gnc_book_get_url(book))
  {
    gnc_book_push_error (book, ERR_BACKEND_LOCKED, NULL);
    LEAVE("bad book url");
    return FALSE;
  }

  /* seriously invalid */
  if (!book_id)
  {
    gnc_book_push_error (book, ERR_BACKEND_NO_BACKEND, NULL);
    LEAVE("bad book_id");
    return FALSE;
  }
  /* Store the sessionid URL  */
  book->book_id = g_strdup (book_id);

  book->fullpath = xaccResolveFilePath(book_id);
  if (!book->fullpath)
  {
      gnc_book_push_error (book, ERR_FILEIO_FILE_NOT_FOUND, NULL);
      LEAVE("bad fullpath");
      return FALSE;    /* ouch */
  }
  PINFO ("filepath=%s", book->fullpath ? book->fullpath : "(null)");
  
  /* check to see if this is a type we know how to handle */
  if (!g_strncasecmp(book_id, "file:", 5) ||
      *book_id == '/')
  {
      book->backend = gncBackendInit_file(book_id, NULL);
  }
  /* load different backend based on URL.  We should probably
   * dynamically load these based on some config file ... */
  else if ((!g_strncasecmp(book_id, "http://", 7)) ||
           (!g_strncasecmp(book_id, "https://", 8)))
  {
      /* create the backend */
      book->backend = xmlendNew();
  }
  else if (!g_strncasecmp(book_id, "postgres://", 11))
  {
      gnc_book_int_load_backend(
          book, "libgnc_postgres.so", "pgendNew",
          "Maybe you don't have postgres installed?\n"
          "The postgres backend can't be used until this "
          "config problem is fixed");
  }
  else if (!g_strncasecmp(book_id, "rpc://", 6))
  {
      gnc_book_int_load_backend( book, "libgnc_rpc.so", "rpcendNew", "");
  }

  /* if there's a begin method, call that. */
  if (book->backend && book->backend->book_begin)
  {
      int err;
      (book->backend->book_begin)(book->backend, book,
                                  gnc_book_get_url(book), ignore_lock,
                                  create_if_nonexistent);
      PINFO("Run book_begin on backend");
      err = xaccBackendGetError(book->backend);
      if (err != ERR_BACKEND_NO_ERR)
      {
          g_free(book->fullpath);
          book->fullpath = NULL;
          g_free(book->book_id);
          book->book_id = NULL;
          gnc_book_push_error (book, err, NULL);
          LEAVE("backend error");
          return FALSE;
      }
  }
  LEAVE(" ");
  return TRUE;
}

/* ---------------------------------------------------------------------- */

gboolean
gnc_book_load (GNCBook *book)
{
  GNCBackendError backend_err;
  Backend *be;

  if (!book) return FALSE;
  if (!gnc_book_get_url(book)) return FALSE;

  ENTER ("book_id=%s", gnc_book_get_url(book)
         ? gnc_book_get_url(book) : "(null)");

  /* At this point, we should are supposed to have a valid book 
   * id and a lock on the file. */

  xaccLogDisable();
  xaccGroupMarkDoFree (book->topgroup);
  xaccFreeAccountGroup (book->topgroup);
  book->topgroup = NULL;
  gnc_pricedb_destroy(book->pricedb);
  book->pricedb = NULL;

  xaccLogSetBaseName(book->fullpath);
  xaccLogEnable();

  gnc_book_clear_error (book);

  /* This code should be sufficient to initialize *any* backend,
   * whether http, postgres, or anything else that might come along.
   * Basically, the idea is that by now, a backend has already been
   * created & set up.  At this point, we only need to get the
   * top-level account group out of the backend, and that is a
   * generic, backend-independent operation.
   */
  be = book->backend;

  /* Starting the session should result in a bunch of accounts
   * and currencies being downloaded, but probably no transactions;
   * The GUI will need to do a query for that.
   */
  if (be)
  {
      xaccLogDisable();
      if(be->book_load) 
      {
          xaccLogSetBaseName(book->fullpath);
          
          book->topgroup = (be->book_load) (be);
          xaccGroupSetBackend (book->topgroup, be);
          gnc_book_push_error(book, xaccBackendGetError(be), NULL);
      }
    
      if (be->price_load) 
      {
          book->pricedb = (be->price_load) (be);
          
          /* we just got done loading, it can't possibly be dirty !! */
          gnc_book_mark_saved(book);
          
          xaccPriceDBSetBackend (book->pricedb, be);
          gnc_book_push_error(book, xaccBackendGetError(be), NULL);
      }
      xaccLogEnable();
  }

  if (!book->topgroup)
  {
      LEAVE("topgroup NULL");
      return FALSE;
  }
  
  if (!book->pricedb)
  {
      LEAVE("pricedb NULL");
      return FALSE;
  }

  if (gnc_book_get_error(book) != ERR_BACKEND_NO_ERR)
  {
      LEAVE("error from backend %d", gnc_book_get_error(book));
      return FALSE;
  }

  LEAVE("book_id=%s", gnc_book_get_url(book)
        ? gnc_book_get_url(book) : "(null)");
  return TRUE;
}



static gboolean
book_sxlist_notsaved(GNCBook *book)
{
  GList *sxlist;
  SchedXaction *sx;
  if(book->sx_notsaved
     ||
     xaccGroupNotSaved(book->template_group)) return TRUE;
 
  for(sxlist = book->sched_xactions;
      sxlist != NULL;
      sxlist = g_list_next(sxlist))
  {
    sx = (SchedXaction *) (sxlist->data);
    if (xaccSchedXactionIsDirty( sx ))
      return TRUE;
  }

  return FALSE;
}
  
/* ---------------------------------------------------------------------- */

gboolean
gnc_book_not_saved(GNCBook *book)
{
  if(!book) return FALSE;

  return(xaccGroupNotSaved(book->topgroup)
         ||
         gnc_pricedb_dirty(book->pricedb)
	 ||
	 book_sxlist_notsaved(book));
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

static gboolean
save_error_handler(Backend *be, GNCBook *book)
{
    int err;
    err = xaccBackendGetError(be);
    
    if (ERR_BACKEND_NO_ERR != err)
    {
        gnc_book_push_error (book, err, NULL);
      
        /* we close the backend here ... isn't this a bit harsh ??? */
        if (be->book_end)
        {
            (be->book_end)(be);
        }
        return TRUE;
    }
    return FALSE;
}

void
gnc_book_save (GNCBook *book)
{
  Backend *be;

  if (!book) return;

  ENTER ("book_id=%s", gnc_book_get_url(book)
         ? gnc_book_get_url(book) : "(null)");

  /* if there is a backend, and the backend is reachablele
   * (i.e. we can communicate with it), then synchronize with 
   * the backend.  If we cannot contact the backend (e.g.
   * because we've gone offline, the network has crashed, etc.)
   * then give the user the option to save to disk. 
   */
  be = book->backend;
  if (be) {
    
    /* if invoked as SaveAs(), then backend not yet set */
    xaccGroupSetBackend (book->topgroup, be);
    xaccPriceDBSetBackend (book->pricedb, be);

    if(be->all_sync)
    {
        (be->all_sync)(be, book->topgroup, book->pricedb);
        if(save_error_handler(be, book))
            return;
    }
    else
    {
        if (be->sync && book->topgroup) {
            (be->sync)(be, book->topgroup);
            if(save_error_handler(be, book))
                return;
        }

        if (be->sync_price && book->pricedb) {
            (be->sync_price)(be, book->pricedb);
            if(save_error_handler(be, book))
                return;
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

  LEAVE(" ");
}

/* ---------------------------------------------------------------------- */

void
gnc_book_end (GNCBook *book)
{
  if (!book) return;

  ENTER ("book_id=%s", gnc_book_get_url(book)
         ? gnc_book_get_url(book) : "(null)");

  /* close down the backend first */
  if (book->backend && book->backend->book_end)
  {
    (book->backend->book_end)(book->backend);
  }

  gnc_book_clear_error (book);

  LEAVE(" ");
}

void 
gnc_book_destroy (GNCBook *book) 
{
  if (!book) return;

  ENTER ("book_id=%s", gnc_book_get_url(book)
         ? gnc_book_get_url(book) : "(null)");

  xaccLogDisable();
  gnc_book_end (book);

  /* destroy the backend */
  if (book->backend && book->backend->destroy_backend)
  {
      book->backend->destroy_backend(book->backend);
  }
  else
  {
      g_free(book->backend);
  }
  
  xaccGroupSetBackend (book->topgroup, NULL);
  xaccPriceDBSetBackend (book->pricedb, NULL);

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

typedef gboolean (*pathGenerator)(char *pathbuf, int which);

static gboolean
xaccAddEndPath(char *pathbuf, const char *ending, int len)
{
    if(len + strlen(pathbuf) >= PATH_MAX)
        return FALSE;
          
    strcat (pathbuf, ending);
    return TRUE;
}

static gboolean
xaccCmdPathGenerator(char *pathbuf, int which)
{
    if(which != 0)
    {
        return FALSE;
    }
    else
    {
        /* try to find a file by this name in the cwd ... */
        if (getcwd (pathbuf, PATH_MAX) == NULL)
            return FALSE;

        strcat (pathbuf, "/");
        return TRUE;
    }
}

static gboolean
xaccDataPathGenerator(char *pathbuf, int which)
{
    char *path;
    
    if(which != 0)
    {
        return FALSE;
    }
    else
    {
        path = getenv ("HOME");
        if (!path)
            return FALSE;

        if (PATH_MAX <= (strlen (path) + 20))
            return FALSE;

        strcpy (pathbuf, path);
        strcat (pathbuf, "/.gnucash/data/");
        return TRUE;
    }
}

static gboolean
xaccUserPathPathGenerator(char *pathbuf, int which)
{
    char *path = NULL;
    
    if(searchpaths[which] == NULL)
    {
        return FALSE;
    }
    else
    {
        path = searchpaths[which];
        
        if (PATH_MAX <= strlen(path))
            return FALSE;

        strcpy (pathbuf, path);
        return TRUE;
    }
}

char * 
xaccResolveFilePath (const char * filefrag_tmp)
{
  struct stat statbuf;
  char pathbuf[PATH_MAX];
  char *filefrag;
  pathGenerator gens[4];
  int namelen;
  int i;

  /* seriously invalid */
  if (!filefrag_tmp)
  {
      PERR("filefrag is NULL");
      return NULL;
  }

  ENTER ("filefrag=%s", filefrag_tmp);

  /* ---------------------------------------------------- */
  /* OK, now we try to find or build an absolute file path */

  /* check for an absolute file path */
  if (*filefrag_tmp == '/')
    return g_strdup (filefrag_tmp);

  if (!g_strncasecmp(filefrag_tmp, "file:", 5))
  {
      char *ret = g_new(char, strlen(filefrag_tmp) - 5 + 1);
      strcpy(ret, filefrag_tmp + 5);
      return ret;
  }

  {
    char *p;
      
    filefrag = g_strdup (filefrag_tmp);
    p = strchr (filefrag, '/');
    while (p) {
        *p = ',';
        p = strchr (filefrag, '/');
    }
  }
  
  /* get conservative on the length so that sprintf(getpid()) works ... */
  /* strlen ("/.LCK") + sprintf (%x%d) */
  namelen = strlen (filefrag) + 25; 

  gens[0] = xaccCmdPathGenerator;
  gens[1] = xaccDataPathGenerator;
  gens[2] = xaccUserPathPathGenerator;
  gens[3] = NULL;

  for (i = 0; gens[i] != NULL; i++) 
  {
      int j;
      for(j = 0; gens[i](pathbuf, j) ; j++)
      {
          if(xaccAddEndPath(pathbuf, filefrag, namelen))
          {
              int rc = stat (pathbuf, &statbuf);
              if ((!rc) && (S_ISREG(statbuf.st_mode)))
              {
                  g_free(filefrag);
                  return (g_strdup (pathbuf));
              }
          }
      }
  }
  /* OK, we didn't find the file. */

  /* make sure that the gnucash home dir exists. */
  MakeHomeDir();

  /* If the user specified a simple filename (i.e. no slashes in it)
   * then create the file.  But if it has slashes in it, then creating
   * a bunch of directories seems like a bad idea; more likely, the user
   * specified a bad filename.  So return with error. */
  if (strchr (filefrag, '/'))
    return NULL;

  /* Lets try creating a new file in $HOME/.gnucash/data */
  if (xaccDataPathGenerator(pathbuf, 0))
  {
      if(xaccAddEndPath(pathbuf, filefrag, namelen))
          return (g_strdup (pathbuf));
  } 

  /* OK, we still didn't find the file */
  /* Lets try creating a new file in the cwd */
  if (xaccCmdPathGenerator(pathbuf, 0))
  {
      if(xaccAddEndPath(pathbuf, filefrag, namelen))
          return (g_strdup (pathbuf));
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

  if (!g_strncasecmp (pathfrag, "http://", 7)      ||
      !g_strncasecmp (pathfrag, "https://", 8)     ||
      !g_strncasecmp (pathfrag, "postgres://", 11) ||
      !g_strncasecmp (pathfrag, "rpc://", 6))
  {
    return g_strdup(pathfrag);
  }

  if (!g_strncasecmp (pathfrag, "file:", 5)) {
    return (xaccResolveFilePath (pathfrag+5));
  }

  return (xaccResolveFilePath (pathfrag));
}

/* ---------------------------------------------------------------------- */

/* this should go in a separate binary to create a rpc server */

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
