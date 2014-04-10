/********************************************************************\
 * qofsesssion.c -- session access (connection to backend)          *
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
 * qofsession.c
 *
 * FUNCTION:
 * Encapsulate a connection to a storage backend.
 *
 * HISTORY:
 * Created by Linas Vepstas December 1998
 * Copyright (c) 1998-2002 Linas Vepstas <linas@linas.org>
 * Copyright (c) 2000 Dave Peticolas
 */

  /* TODO: XXX we should probably move this resolve function to the
	* file backend.  I think the idea would be to open the backend
	* and then ask it if it can contact it's storage media (disk,
	* network, server, etc.) and abort if it can't.  Mal-formed
	* file URL's would be handled the same way!
	*/

#include "config.h"

#include <dlfcn.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include <glib.h>

#include "gnc-event.h"
#include "gnc-trace.h"
#include "qofbackend-p.h"
#include "qofbook.h"
#include "qofbook-p.h"
#include "qofsession.h"
#include "qofsession-p.h"

/* Some gnucash-specific code */
#ifdef GNUCASH
#include "gnc-module.h"
#include "TransLog.h"
#else
#define xaccLogSetBaseName(x)
#define xaccLogEnable()
#define xaccLogDisable()
#endif /* GNUCASH */

static QofSession * current_session = NULL;
static short module = MOD_IO;

/* ====================================================================== */
/* error handling routines */

static void
qof_session_clear_error (QofSession *session)
{
  QofBackendError err;

  session->last_err = ERR_BACKEND_NO_ERR;
  g_free(session->error_message);
  session->error_message = NULL;

  /* pop the stack on the backend as well. */
  if (session->backend)
  {
    do
    {
       err = qof_backend_get_error (session->backend);
    } while (ERR_BACKEND_NO_ERR != err);
  }
}

void
qof_session_push_error (QofSession *session, QofBackendError err,
                        const char *message)
{
  if (!session) return;

  g_free (session->error_message);

  session->last_err = err;
  session->error_message = g_strdup (message);
}

QofBackendError
qof_session_get_error (QofSession * session)
{
  QofBackendError err;

  if (!session) return ERR_BACKEND_NO_BACKEND;

  /* if we have a local error, return that. */
  if (ERR_BACKEND_NO_ERR != session->last_err)
  {
    return session->last_err;
  }

  /* maybe we should return a no-backend error ??? */
  if (! session->backend) return ERR_BACKEND_NO_ERR;

  err = qof_backend_get_error (session->backend);
  session->last_err = err;
  return err;
}

static const char *
get_default_error_message(QofBackendError err)
{
    return "";
}

const char *
qof_session_get_error_message(QofSession *session)
{
    if(!session) return "";
    if(!session->error_message)
      return get_default_error_message(session->last_err);
    return session->error_message;
}

QofBackendError
qof_session_pop_error (QofSession * session)
{
  QofBackendError err;

  if (!session) return ERR_BACKEND_NO_BACKEND;

  err = qof_session_get_error(session);
  qof_session_clear_error(session);

  return err;
}

/* ====================================================================== */

static void
qof_session_init (QofSession *session)
{
  if (!session) return;

  session->books = g_list_append (NULL, qof_book_new ());
  session->book_id = NULL;
  session->fullpath = NULL;
  session->logpath = NULL;
  session->backend = NULL;

  qof_session_clear_error (session);
}

QofSession *
qof_session_new (void)
{
  QofSession *session = g_new0(QofSession, 1);
  qof_session_init(session);
  return session;
}

QofSession *
qof_session_get_current_session (void)
{
  if (!current_session)
  {
    gnc_engine_suspend_events ();
    current_session = qof_session_new ();
    gnc_engine_resume_events ();
  }

  return current_session;
}

void
qof_session_set_current_session (QofSession *session)
{
  current_session = session;
}

QofBook *
qof_session_get_book (QofSession *session)
{
   GList *node;
   if (!session) return NULL;

   for (node=session->books; node; node=node->next)
   {
      QofBook *book = node->data;
      if ('y' == book->book_open) return book;
   }
   return NULL;
}

void
qof_session_set_book (QofSession *session, QofBook *addbook)
{
  GList *node;
  if (!session) return;

  ENTER (" sess=%p book=%p", session, addbook);

  /* See if this book is already there ... */
  for (node=session->books; node; node=node->next)
  {
     QofBook *book = node->data;
     if (addbook == book) return;
  }

  if ('y' == addbook->book_open)
  {
    /* hack alert -- someone should free all the books in the list,
     * but it should probably not be us ... since the books backends
     * should be shutdown first, etc */
    g_list_free (session->books);
    session->books = g_list_append (NULL, addbook);
  }
  else 
  {
    session->books = g_list_append (session->books, addbook);
  }

  qof_book_set_backend (addbook, session->backend);
  LEAVE (" ");
}

QofBackend * 
qof_session_get_backend (QofSession *session)
{
   if (!session) return NULL;
   return session->backend;
}

const char *
qof_session_get_file_path (QofSession *session)
{
   if (!session) return NULL;
   return session->fullpath;
}

const char *
qof_session_get_url (QofSession *session)
{
   if (!session) return NULL;
   return session->book_id;
}

/* ====================================================================== */

static void
qof_session_int_backend_load_error(QofSession *session,
                                   char *message, char *dll_err)
{
    PWARN (message, dll_err ? dll_err : "");

    g_free(session->fullpath);
    session->fullpath = NULL;

    g_free(session->logpath);
    session->logpath = NULL;

    g_free(session->book_id);
    session->book_id = NULL;

    qof_session_push_error (session, ERR_BACKEND_NO_BACKEND, NULL);
}


#ifdef GNUCASH 

/* Gnucash uses its module system to load a backend; other users
 * use traditional dlopen calls.
 */
static void
qof_session_load_backend(QofSession * session, char * backend_name)
{
  GNCModule  mod = 0;
  QofBackend    *(* be_new_func)(void);
  char       * mod_name = g_strdup_printf("gnucash/backend/%s", backend_name);

  /* FIXME : reinstate better error messages with gnc_module errors */
  ENTER (" ");
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
      GList *node;
      session->backend = be_new_func();

      for (node=session->books; node; node=node->next)
      {
         QofBook *book = node->data;
         qof_book_set_backend (book, session->backend);
      }
    }
    else
    {
      qof_session_int_backend_load_error(session, " can't find backend_new ",
                                         "");
    }      
  }
  else
  {
    qof_session_int_backend_load_error(session,
                                       " failed to load '%s' backend", 
                                       backend_name);
  }

  g_free(mod_name);
  LEAVE (" ");
}

#else /* GNUCASH */

static void
qof_session_load_backend(QofSession * session, char * backend_name)
{
  ENTER (" ");
  LEAVE (" ");
}
#endif /* GNUCASH */

/* ====================================================================== */

static void
qof_session_destroy_backend (QofSession *session)
{
  g_return_if_fail (session);

  if (session->backend)
  {
    /* clear any error message */
    char * msg = qof_backend_get_message (session->backend);
    g_free (msg);

    /* Then destroy the backend */
    if (session->backend->destroy_backend)
    {
      session->backend->destroy_backend(session->backend);
    }
    else
    {
      g_free(session->backend);
    }
  }

  session->backend = NULL;
}

void
qof_session_begin (QofSession *session, const char * book_id, 
                   gboolean ignore_lock, gboolean create_if_nonexistent)
{
  if (!session) return;

  ENTER (" sess=%p ignore_lock=%d, book-id=%s", 
         session, ignore_lock,
         book_id ? book_id : "(null)");

  /* clear the error condition of previous errors */
  qof_session_clear_error (session);

  /* check to see if this session is already open */
  if (qof_session_get_url(session))
  {
    qof_session_push_error (session, ERR_BACKEND_LOCKED, NULL);
    LEAVE("push error book is already open ");
    return;
  }

  /* seriously invalid */
  if (!book_id)
  {
    qof_session_push_error (session, ERR_BACKEND_BAD_URL, NULL);
    LEAVE("push error missing book_id");
    return;
  }
  /* Store the sessionid URL  */
  session->book_id = g_strdup (book_id);

  /* XXX we should probably move this resolve function to the
	* file backend.  I think the idea would be to open the backend
	* and then ask it if it can contact it's storage media (disk,
	* network, server, etc.) and abort if it can't.  Mal-formed
	* file URL's would be handled the same way!
	*/
  /* ResolveURL tries to find the file in the file system. */
  session->fullpath = xaccResolveURL(book_id);
  if (!session->fullpath)
  {
    qof_session_push_error (session, ERR_FILEIO_FILE_NOT_FOUND, NULL);
    LEAVE("push error: can't resolve file path");
    return;  
  }
  PINFO ("filepath=%s", session->fullpath ? session->fullpath : "(null)");

  session->logpath = xaccResolveFilePath(session->fullpath);
  PINFO ("logpath=%s", session->logpath ? session->logpath : "(null)");

  /* destroy the old backend */
  qof_session_destroy_backend(session);

  /* check to see if this is a type we know how to handle */
  if (!g_strncasecmp(book_id, "file:", 5) ||
      *session->fullpath == '/')
  {
    qof_session_load_backend(session, "file" ); 
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
    qof_session_load_backend(session, "postgres");
  }
  else if (!g_strncasecmp(book_id, "rpc://", 6))
  {
    qof_session_load_backend(session, "rpc");
  }

  /* if there's a begin method, call that. */
  if (session->backend && session->backend->session_begin)
  {
      int err;
      char * msg;
      
      (session->backend->session_begin)(session->backend, session,
                                  qof_session_get_url(session), ignore_lock,
                                  create_if_nonexistent);
      PINFO("Done running session_begin on backend");
      err = qof_backend_get_error(session->backend);
      msg = qof_backend_get_message(session->backend);
      if (err != ERR_BACKEND_NO_ERR)
      {
          g_free(session->fullpath);
          session->fullpath = NULL;
          g_free(session->logpath);
          session->logpath = NULL;
          g_free(session->book_id);
          session->book_id = NULL;
          qof_session_push_error (session, err, msg);
          LEAVE("backend error %d", err);
          return;
      }
      if (msg != NULL) {
          PWARN_GUI(msg);
          g_free(msg);
      }
  }

  /* No backend was found. That's bad. */
  if (NULL == session->backend)
  {
    qof_session_push_error (session, ERR_BACKEND_BAD_URL, NULL);
  }
  LEAVE (" sess=%p book-id=%s", 
         session,  book_id ? book_id : "(null)");
}

/* ====================================================================== */

void
qof_session_load (QofSession *session,
		  QofPercentageFunc percentage_func)
{
  QofBook *newbook;
  QofBookList *oldbooks, *node;
  QofBackend *be;
  QofBackendError err;

  if (!session) return;
  if (!qof_session_get_url(session)) return;

  ENTER ("sess=%p book_id=%s", session, qof_session_get_url(session)
         ? qof_session_get_url(session) : "(null)");


  /* At this point, we should are supposed to have a valid book 
   * id and a lock on the file. */

  oldbooks = session->books;
  newbook = qof_book_new();
  session->books = g_list_append (NULL, newbook);
  PINFO ("new book=%p", newbook);

  xaccLogSetBaseName(session->logpath);

  qof_session_clear_error (session);

  /* This code should be sufficient to initialize *any* backend,
   * whether http, postgres, or anything else that might come along.
   * Basically, the idea is that by now, a backend has already been
   * created & set up.  At this point, we only need to get the
   * top-level account group out of the backend, and that is a
   * generic, backend-independent operation.
   */
  be = session->backend;
  qof_book_set_backend(newbook, be);

  /* Starting the session should result in a bunch of accounts
   * and currencies being downloaded, but probably no transactions;
   * The GUI will need to do a query for that.
   */
  if (be)
  {
      xaccLogDisable();
      be->percentage = percentage_func;

      if (be->load) 
      {
          be->load (be, newbook);
          qof_session_push_error (session, qof_backend_get_error(be), NULL);
      }

      /* we just got done loading, it can't possibly be dirty !! */
      qof_book_mark_saved (newbook);

      xaccLogEnable();
  }

  err = qof_session_get_error(session);
  if ((err != ERR_BACKEND_NO_ERR) &&
      (err != ERR_FILEIO_FILE_TOO_OLD) &&
      (err != ERR_SQL_DB_TOO_OLD))
  {
      /* Something broke, put back the old stuff */
      xaccLogDisable();
      qof_book_set_backend (newbook, NULL);
      qof_book_destroy (newbook);
      g_list_free (session->books);
      session->books = oldbooks;
      LEAVE("error from backend %d", qof_session_get_error(session));
      xaccLogEnable();
      return;
  }

  xaccLogDisable();
  for (node=oldbooks; node; node=node->next)
  {
     QofBook *ob = node->data;
     qof_book_set_backend (ob, NULL);
     qof_book_destroy (ob);
  }
  xaccLogEnable();

  LEAVE ("sess = %p, book_id=%s", session, qof_session_get_url(session)
         ? qof_session_get_url(session) : "(null)");
}

/* ====================================================================== */

gboolean
qof_session_save_may_clobber_data (QofSession *session)
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
save_error_handler(QofBackend *be, QofSession *session)
{
    int err;
    err = qof_backend_get_error(be);
    
    if (ERR_BACKEND_NO_ERR != err)
    {
        qof_session_push_error (session, err, NULL);
      
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
qof_session_save (QofSession *session,
		  QofPercentageFunc percentage_func)
{
  GList *node;
  QofBackend *be;

  if (!session) return;

  ENTER ("sess=%p book_id=%s", 
         session, 
         qof_session_get_url(session)
         ? qof_session_get_url(session) : "(null)");

  /* If there is a backend, and the backend is reachable
   * (i.e. we can communicate with it), then synchronize with 
   * the backend.  If we cannot contact the backend (e.g.
   * because we've gone offline, the network has crashed, etc.)
   * then give the user the option to save to disk. 
   *
   * hack alert -- FIXME -- XXX the code below no longer
   * does what the words above say.  This needs fixing.
   */
  be = session->backend;
  if (be)
  {
    for (node = session->books; node; node=node->next)
    {
      QofBook *abook = node->data;

      /* if invoked as SaveAs(), then backend not yet set */
      qof_book_set_backend (abook, be);
      be->percentage = percentage_func;
  
      if (be->sync)
      {
        (be->sync)(be, abook);
        if (save_error_handler(be, session)) return;
      }
		
	 	/* XXX The backend should really be calling this, not us. */
		qof_book_mark_saved (abook);
    }

    
    /* If we got to here, then the backend saved everything 
     * just fine, and we are done. So return. */
    return;
  } 

  /* If the fullpath doesn't exist, either the user failed to initialize,
   * or the lockfile was never obtained. Either way, we can't write. */
  qof_session_clear_error (session);

  if (!session->fullpath)
  {
    qof_session_push_error (session, ERR_BACKEND_MISC, NULL);
    return;
  }

  LEAVE(" ");
}

/* ====================================================================== */
/* XXX what does this function do ?? */

gboolean
qof_session_export (QofSession *tmp_session,
		    QofSession *real_session,
		    QofPercentageFunc percentage_func)
{
  QofBook *book;
  QofBackend *be;

  if ((!tmp_session) || (!real_session)) return FALSE;

  book = qof_session_get_book (real_session);
  ENTER ("tmp_session=%p real_session=%p book=%p book_id=%s", 
         tmp_session, real_session, book,
         qof_session_get_url(tmp_session)
         ? qof_session_get_url(tmp_session) : "(null)");

  /* There must be a backend or else.  (It should always be the file
   * backend too.)
   */
  be = tmp_session->backend;
  if (!be)
    return FALSE;

  be->percentage = percentage_func;
  if (be->export)
    {

      (be->export)(be, book);
      if (save_error_handler(be, tmp_session)) return FALSE;
    }

  return TRUE;
}

/* ====================================================================== */

void
qof_session_end (QofSession *session)
{
  if (!session) return;

  ENTER ("sess=%p book_id=%s", session, qof_session_get_url(session)
         ? qof_session_get_url(session) : "(null)");

  /* close down the backend first */
  if (session->backend && session->backend->session_end)
  {
    (session->backend->session_end)(session->backend);
  }

  qof_session_clear_error (session);

  g_free (session->fullpath);
  session->fullpath = NULL;

  g_free (session->logpath);
  session->logpath = NULL;

  g_free (session->book_id);
  session->book_id = NULL;

  LEAVE ("sess=%p book_id=%s", session, qof_session_get_url(session)
         ? qof_session_get_url(session) : "(null)");
}

void 
qof_session_destroy (QofSession *session) 
{
  GList *node;
  if (!session) return;

  ENTER ("sess=%p book_id=%s", session, 
         qof_session_get_url(session)
         ? qof_session_get_url(session) : "(null)");

  xaccLogDisable();
  qof_session_end (session);

  /* destroy the backend */
  qof_session_destroy_backend(session);

  for (node=session->books; node; node=node->next)
  {
    QofBook *book = node->data;
    qof_book_set_backend (book, NULL);
    qof_book_destroy (book);
  }

  session->books  = NULL;
  if (session == current_session)
    current_session = NULL;

  xaccLogEnable();

  g_free (session);

  LEAVE ("sess=%p", session);
}

/* ====================================================================== */
/* this call is weird. */

void
qof_session_swap_data (QofSession *session_1, QofSession *session_2)
{
  GList *books_1, *books_2, *node;

  if (session_1 == session_2) return;
  if (!session_1 || !session_2) return;

  ENTER ("sess1=%p sess2=%p", session_1, session_2);

  books_1 = session_1->books;
  books_2 = session_2->books;

  session_1->books = books_2;
  session_2->books = books_1;

  for (node=books_1; node; node=node->next)
  {
    QofBook *book_1 = node->data;
    qof_book_set_backend (book_1, session_2->backend);
  }
  for (node=books_2; node; node=node->next)
  {
    QofBook *book_2 = node->data;
    qof_book_set_backend (book_2, session_1->backend);
  }

  LEAVE (" ");
}

/* ====================================================================== */

gboolean
qof_session_events_pending (QofSession *session)
{
  if (!session) return FALSE;
  if (!session->backend) return FALSE;
  if (!session->backend->events_pending) return FALSE;

  return session->backend->events_pending (session->backend);
}

gboolean
qof_session_process_events (QofSession *session)
{
  if (!session) return FALSE;
  if (!session->backend) return FALSE;
  if (!session->backend->process_events) return FALSE;

  return session->backend->process_events (session->backend);
}

/* ====================================================================== */
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

/* ====================================================================== */

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

/* ====================================================================== */

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

/* ====================================================================== */

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
    return (xaccResolveFilePath (pathfrag));
  }

  return (xaccResolveFilePath (pathfrag));
}

/* ====================================================================== */

/* this should go in a separate binary to create a rpc server */

void
gnc_run_rpc_server (void)
{
  const char * dll_err;
  void * dll_handle;
  int (*rpc_run)(short);
  int ret;
 
  /* open and resolve all symbols now (we don't want mystery 
   * failure later) */
#ifndef RTLD_NOW
# ifdef RTLD_LAZY
#  define RTLD_NOW RTLD_LAZY
# endif
#endif
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

/* =================== END OF FILE ====================================== */
