/********************************************************************\
 * gnc-sesssion.c -- session access (connection to backend)         *
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
 * gnc-session.c
 *
 * FUNCTION:
 * Encapsulate a connection to a GnuCash backend.
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

#include "BackendP.h"
#include "TransLog.h"
#include "gnc-engine-util.h"
#include "DateUtils.h"
#include "gnc-book-p.h"
#include "gnc-engine.h"
#include "gnc-engine-util.h"
#include "gnc-module.h"
#include "gnc-session-p.h"

static short module = MOD_IO;

/* ---------------------------------------------------------------------- */

static void
gnc_session_clear_error (GNCSession *session)
{
  session->last_err = ERR_BACKEND_NO_ERR;
  g_free(session->error_message);
  session->error_message = NULL;
}

void
gnc_session_push_error (GNCSession *session, GNCBackendError err,
                        const char *message)
{
  if (!session) return;

  g_free (session->error_message);

  session->last_err = err;
  session->error_message = g_strdup (message);
}

GNCBackendError
gnc_session_get_error (GNCSession * session)
{
  if (!session) return ERR_BACKEND_NO_BACKEND;
  return session->last_err;
}

static const char *
get_default_error_message(GNCBackendError err)
{
    return "";
}

const char *
gnc_session_get_error_message(GNCSession *session)
{
    if(!session) return "";
    if(!session->error_message)
      return get_default_error_message(session->last_err);
    return session->error_message;
}

GNCBackendError
gnc_session_pop_error (GNCSession * session)
{
  GNCBackendError err;

  if (!session) return ERR_BACKEND_NO_BACKEND;

  err = session->last_err;
  gnc_session_clear_error(session);

  return err;
}

/* ---------------------------------------------------------------------- */

static void
gnc_session_init (GNCSession *session)
{
  if (!session) return;

  session->entity_table = xaccEntityTableNew ();
  session->book = gnc_book_new (session);
  session->book_id = NULL;
  session->fullpath = NULL;
  session->logpath = NULL;
  session->backend = NULL;

  session->kvp_data = kvp_frame_new ();

  gnc_session_clear_error (session);
}

GNCSession *
gnc_session_new (void)
{
  GNCSession *session = g_new0(GNCSession, 1);
  gnc_session_init(session);
  return session;
}

GNCBook *
gnc_session_get_book (GNCSession *session)
{
   if (!session) return NULL;
   return session->book;
}

void
gnc_session_set_book (GNCSession *session, GNCBook *book)
{
  if (!session) return;

  /* Do not free the old book here unless you also fix
   * all the other uses of gnc_session_set_book! */

  if (session->book == book)
    return;

  session->book = book;

  gnc_book_set_backend (book, session->backend);
}

kvp_frame *
gnc_session_get_slots (GNCSession *session)
{
  if (!session) return NULL;
  return session->kvp_data;
}

GNCEntityTable *
gnc_session_get_entity_table (GNCSession *session)
{
  if (!session) return NULL;
  return session->entity_table;
}

Backend * 
gnc_session_get_backend (GNCSession *session)
{
   if (!session) return NULL;
   return session->backend;
}

const char *
gnc_session_get_file_path (GNCSession *session)
{
   if (!session) return NULL;
   return session->fullpath;
}

const char *
gnc_session_get_url (GNCSession *session)
{
   if (!session) return NULL;
   return session->book_id;
}

static void
gnc_session_int_backend_load_error(GNCSession *session,
                                   char *message, char *dll_err)
{
    PWARN (message, dll_err ? dll_err : "");

    g_free(session->fullpath);
    session->fullpath = NULL;

    g_free(session->logpath);
    session->logpath = NULL;

    g_free(session->book_id);
    session->book_id = NULL;

    gnc_session_push_error (session, ERR_BACKEND_NO_BACKEND, NULL);
}


/* FIXME : reinstate better error messages with gnc_module errors */
static void
gnc_session_load_backend(GNCSession * session, char * backend_name)
{
  GNCModule  mod = 0;
  Backend    *(* be_new_func)(void);
  char       * mod_name = g_strdup_printf("gnucash/backend/%s", backend_name);

  /* FIXME: this needs to be smarter with version numbers. */
  /* FIXME: this should use dlopen(), instead of guile/scheme, 
   *    to load the modules.  Right now, this requires the engine to
   *    link to scheme, which is an obvious architecture flaw. */
  mod = gnc_module_load(mod_name, 0);

  if (mod) 
  {
    be_new_func = gnc_module_lookup(mod, "gnc_backend_new");

    if(be_new_func) 
    {
      session->backend = be_new_func();

      gnc_book_set_backend (session->book, session->backend);
    }
    else
    {
      gnc_session_int_backend_load_error(session, " can't find backend_new ",
                                         "");
    }      
  }
  else
  {
    gnc_session_int_backend_load_error(session,
                                       " failed to load '%s' backend", 
                                       backend_name);
  }

  g_free(mod_name);
}

gboolean
gnc_session_begin (GNCSession *session, const char * book_id, 
                   gboolean ignore_lock, gboolean create_if_nonexistent)
{
  int rc;

  if (!session) return FALSE;
  ENTER (" ignore_lock=%d, book-id=%s", ignore_lock,
         book_id ? book_id : "(null)");

  /* clear the error condition of previous errors */
  gnc_session_clear_error (session);

  /* check to see if this session is already open */
  if (gnc_session_get_url(session))
  {
    gnc_session_push_error (session, ERR_BACKEND_LOCKED, NULL);
    LEAVE("bad book url");
    return FALSE;
  }

  /* seriously invalid */
  if (!book_id)
  {
    gnc_session_push_error (session, ERR_BACKEND_NO_BACKEND, NULL);
    LEAVE("bad book_id");
    return FALSE;
  }
  /* Store the sessionid URL  */
  session->book_id = g_strdup (book_id);

  session->fullpath = xaccResolveURL(book_id);
  if (!session->fullpath)
  {
    gnc_session_push_error (session, ERR_FILEIO_FILE_NOT_FOUND, NULL);
    LEAVE("bad fullpath");
    return FALSE;    /* ouch */
  }
  PINFO ("filepath=%s", session->fullpath ? session->fullpath : "(null)");

  session->logpath = xaccResolveFilePath(session->fullpath);
  PINFO ("logpath=%s", session->logpath ? session->logpath : "(null)");

  /* destroy the old backend */
  if (session->backend && session->backend->destroy_backend)
  {
      session->backend->destroy_backend(session->backend);
  }
  else
  {
      g_free(session->backend);
  }

  session->backend = NULL;

  /* check to see if this is a type we know how to handle */
  if (!g_strncasecmp(book_id, "file:", 5) ||
      *session->fullpath == '/')
  {
    gnc_session_load_backend(session, "file" ); 
  }
#if 0
  /* load different backend based on URL.  We should probably
   * dynamically load these based on some config file ... */
  else if ((!g_strncasecmp(book_id, "http://", 7)) ||
           (!g_strncasecmp(book_id, "https://", 8)))
  {
      /* create the backend */
      session->backend = xmlendNew();
  }
#endif
  else if (!g_strncasecmp(book_id, "postgres://", 11))
  {
    gnc_session_load_backend(session, "postgres");
  }
  else if (!g_strncasecmp(book_id, "rpc://", 6))
  {
    gnc_session_load_backend(session, "rpc");
  }

  /* if there's a begin method, call that. */
  if (session->backend && session->backend->session_begin)
  {
      int err;
      (session->backend->session_begin)(session->backend, session,
                                  gnc_session_get_url(session), ignore_lock,
                                  create_if_nonexistent);
      PINFO("Run session_begin on backend");
      err = xaccBackendGetError(session->backend);
      if (err != ERR_BACKEND_NO_ERR)
      {
          g_free(session->fullpath);
          session->fullpath = NULL;
          g_free(session->logpath);
          session->logpath = NULL;
          g_free(session->book_id);
          session->book_id = NULL;
          gnc_session_push_error (session, err, NULL);
          LEAVE("backend error");
          return FALSE;
      }
  }
  LEAVE(" ");
  return TRUE;
}

/* ---------------------------------------------------------------------- */

gboolean
gnc_session_load (GNCSession *session)
{
  GNCBackendError backend_err;
  Backend *be;

  if (!session) return FALSE;
  if (!gnc_session_get_url(session)) return FALSE;

  ENTER ("book_id=%s", gnc_session_get_url(session)
         ? gnc_session_get_url(session) : "(null)");

  /* At this point, we should are supposed to have a valid book 
   * id and a lock on the file. */

  xaccLogDisable();

  gnc_book_destroy (session->book);
  session->book = gnc_book_new (session);

  xaccLogSetBaseName(session->logpath);
  xaccLogEnable();

  gnc_session_clear_error (session);

  /* This code should be sufficient to initialize *any* backend,
   * whether http, postgres, or anything else that might come along.
   * Basically, the idea is that by now, a backend has already been
   * created & set up.  At this point, we only need to get the
   * top-level account group out of the backend, and that is a
   * generic, backend-independent operation.
   */
  be = session->backend;

  /* Starting the session should result in a bunch of accounts
   * and currencies being downloaded, but probably no transactions;
   * The GUI will need to do a query for that.
   */
  if (be)
  {
      xaccLogDisable();

      if (be->book_load) 
      {
          xaccLogSetBaseName(session->logpath);

          be->book_load (be);

          gnc_session_push_error (session, xaccBackendGetError(be), NULL);
      }

      if (be->price_load) 
      {
          be->price_load (be);

          gnc_session_push_error(session, xaccBackendGetError(be), NULL);
      }

      gnc_book_set_backend (session->book, be);

      /* we just got done loading, it can't possibly be dirty !! */
      gnc_book_mark_saved (session->book);

      xaccLogEnable();
  }

  if (!gnc_book_get_group (session->book))
  {
      LEAVE("topgroup NULL");
      return FALSE;
  }
  
  if (!gnc_book_get_pricedb (session->book))
  {
      LEAVE("pricedb NULL");
      return FALSE;
  }

  if (gnc_session_get_error(session) != ERR_BACKEND_NO_ERR)
  {
      LEAVE("error from backend %d", gnc_session_get_error(session));
      return FALSE;
  }

  LEAVE ("book_id=%s", gnc_session_get_url(session)
         ? gnc_session_get_url(session) : "(null)");

  return TRUE;
}

gboolean
gnc_session_save_may_clobber_data (GNCSession *session)
{
  /* FIXME: Make sure this doesn't need more sophisticated semantics
   * in the face of special file, devices, pipes, symlinks, etc. */

  struct stat statbuf;

  if (!session) return FALSE;
  if (!session->fullpath) return FALSE;
  if (stat(session->fullpath, &statbuf) == 0) return TRUE;

  return FALSE;
}

static gboolean
save_error_handler(Backend *be, GNCSession *session)
{
    int err;
    err = xaccBackendGetError(be);
    
    if (ERR_BACKEND_NO_ERR != err)
    {
        gnc_session_push_error (session, err, NULL);
      
        /* we close the backend here ... isn't this a bit harsh ??? */
        if (be->session_end)
        {
            (be->session_end)(be);
        }
        return TRUE;
    }
    return FALSE;
}

void
gnc_session_save (GNCSession *session)
{
  Backend *be;

  if (!session) return;

  ENTER ("book_id=%s", gnc_session_get_url(session)
         ? gnc_session_get_url(session) : "(null)");

  /* if there is a backend, and the backend is reachablele
   * (i.e. we can communicate with it), then synchronize with 
   * the backend.  If we cannot contact the backend (e.g.
   * because we've gone offline, the network has crashed, etc.)
   * then give the user the option to save to disk. 
   */
  be = session->backend;
  if (be)
  {
    /* if invoked as SaveAs(), then backend not yet set */
    gnc_book_set_backend (session->book, be);

    if (be->sync_all)
    {
      (be->sync_all)(be, session->book);
      if (save_error_handler(be, session))
        return;
    }

    if (be->sync_group && gnc_book_get_group (session->book))
    {
      (be->sync_group)(be, gnc_book_get_group (session->book));
      if (save_error_handler(be, session))
        return;
    }

    if (be->sync_price && gnc_book_get_pricedb (session->book))
    {
      (be->sync_price)(be, gnc_book_get_pricedb (session->book));
      if(save_error_handler(be, session))
        return;
    }

    return;
  } 

  /* if the fullpath doesn't exist, either the user failed to initialize,
   * or the lockfile was never obtained. Either way, we can't write. */
  gnc_session_clear_error (session);

  if (!session->fullpath)
  {
    gnc_session_push_error (session, ERR_BACKEND_MISC, NULL);
    return;
  }

  LEAVE(" ");
}

/* ---------------------------------------------------------------------- */

void
gnc_session_end (GNCSession *session)
{
  if (!session) return;

  ENTER ("book_id=%s", gnc_session_get_url(session)
         ? gnc_session_get_url(session) : "(null)");

  /* close down the backend first */
  if (session->backend && session->backend->session_end)
  {
    (session->backend->session_end)(session->backend);
  }

  gnc_session_clear_error (session);

  g_free (session->fullpath);
  session->fullpath = NULL;

  g_free (session->logpath);
  session->logpath = NULL;

  g_free (session->book_id);
  session->book_id = NULL;

  LEAVE(" ");
}

void 
gnc_session_destroy (GNCSession *session) 
{
  if (!session) return;

  ENTER ("book_id=%s", gnc_session_get_url(session)
         ? gnc_session_get_url(session) : "(null)");

  xaccLogDisable();
  gnc_session_end (session);

  /* destroy the backend */
  if (session->backend && session->backend->destroy_backend)
  {
      session->backend->destroy_backend(session->backend);
  }
  else
  {
      g_free(session->backend);
  }

  gnc_book_set_backend (session->book, NULL);

  gnc_book_destroy (session->book);
  session->book = NULL;

  xaccEntityTableDestroy (session->entity_table);
  session->entity_table = NULL;

  kvp_frame_delete (session->kvp_data);
  session->kvp_data = NULL;

  xaccLogEnable();

  g_free (session);

  LEAVE(" ");
}

void
gnc_session_swap_data (GNCSession *session_1, GNCSession *session_2)
{
  GNCBook *book_1, *book_2;
  GNCEntityTable *entity_table_1, *entity_table_2;
  kvp_frame *kvp_1, *kvp_2;

  if (session_1 == session_2) return;
  if (!session_1 || !session_2) return;

  book_1 = session_1->book;
  entity_table_1 = session_1->entity_table;
  kvp_1 = session_1->kvp_data;

  book_2 = session_2->book;
  entity_table_2 = session_2->entity_table;
  kvp_2 = session_2->kvp_data;

  session_1->book = book_2;
  session_1->entity_table = entity_table_2;
  session_1->kvp_data = kvp_2;

  session_2->book = book_1;
  session_2->entity_table = entity_table_1;
  session_2->kvp_data = kvp_1;

  gnc_book_set_backend (book_1, session_2->backend);
  gnc_book_set_backend (book_2, session_1->backend);
}

gboolean
gnc_session_events_pending (GNCSession *session)
{
  if (!session) return FALSE;
  if (!session->backend) return FALSE;
  if (!session->backend->events_pending) return FALSE;

  return session->backend->events_pending (session->backend);
}

gboolean
gnc_session_process_events (GNCSession *session)
{
  if (!session) return FALSE;
  if (!session->backend) return FALSE;
  if (!session->backend->process_events) return FALSE;

  return session->backend->process_events (session->backend);
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
xaccCwdPathGenerator(char *pathbuf, int which)
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
xaccResolveFilePath (const char * filefrag)
{
  struct stat statbuf;
  char pathbuf[PATH_MAX];
  pathGenerator gens[4];
  char *filefrag_dup;
  int namelen;
  int i;

  /* seriously invalid */
  if (!filefrag)
  {
      PERR("filefrag is NULL");
      return NULL;
  }

  ENTER ("filefrag=%s", filefrag);

  /* ---------------------------------------------------- */
  /* OK, now we try to find or build an absolute file path */

  /* check for an absolute file path */
  if (*filefrag == '/')
    return g_strdup (filefrag);

  if (!g_strncasecmp(filefrag, "file:", 5))
  {
      char *ret = g_new(char, strlen(filefrag) - 5 + 1);
      strcpy(ret, filefrag + 5);
      return ret;
  }

  /* get conservative on the length so that sprintf(getpid()) works ... */
  /* strlen ("/.LCK") + sprintf (%x%d) */
  namelen = strlen (filefrag) + 25; 

  gens[0] = xaccCwdPathGenerator;
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
                  return (g_strdup (pathbuf));
              }
          }
      }
  }
  /* OK, we didn't find the file. */

  /* make sure that the gnucash home dir exists. */
  MakeHomeDir();

  filefrag_dup = g_strdup (filefrag);

  /* Replace '/' with ',' for non file backends */
  if (strstr (filefrag, "://"))
  {
    char *p;

    p = strchr (filefrag_dup, '/');
    while (p) {
      *p = ',';
      p = strchr (filefrag_dup, '/');
    }
  }

  /* Lets try creating a new file in $HOME/.gnucash/data */
  if (xaccDataPathGenerator(pathbuf, 0))
  {
      if(xaccAddEndPath(pathbuf, filefrag_dup, namelen))
      {
          g_free (filefrag_dup);
          return (g_strdup (pathbuf));
      }
  } 

  /* OK, we still didn't find the file */
  /* Lets try creating a new file in the cwd */
  if (xaccCwdPathGenerator(pathbuf, 0))
  {
      if(xaccAddEndPath(pathbuf, filefrag_dup, namelen))
      {
          g_free (filefrag_dup);
          return (g_strdup (pathbuf));
      }
  }

  g_free (filefrag_dup);

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
   * to make sure the uri is in good form.
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
