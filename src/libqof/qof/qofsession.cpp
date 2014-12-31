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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/

/**
 * @file qofsession.c
 * @brief Encapsulate a connection to a storage backend.
 *
 * HISTORY:
 * Created by Linas Vepstas December 1998

 @author Copyright (c) 1998-2004 Linas Vepstas <linas@linas.org>
 @author Copyright (c) 2000 Dave Peticolas
 @author Copyright (c) 2005 Neil Williams <linux@codehelp.co.uk>
   */

#include "config.h"

#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#else
# ifdef __GNUC__
#  warning "<unistd.h> required."
# endif
#endif

#include <glib.h>
#include "qof.h"
#include "qofbackend-p.h"
#include "qofbook-p.h"
#include "qofsession-p.h"
#include "qofobject-p.h"

static GHookList * session_closed_hooks = NULL;
static QofLogModule log_module = QOF_MOD_SESSION;
static GSList *provider_list = NULL;
static gboolean qof_providers_initialized = FALSE;

/*
 * These getters are used in tests to reach static vars from outside
 * They should be removed when no longer needed
 */

#ifdef __cplusplus
extern "C"
{
#endif

GHookList* get_session_closed_hooks (void );
GSList* get_provider_list (void );
gboolean get_qof_providers_initialized (void );
void unregister_all_providers (void );

#ifdef __cplusplus
}
#endif

GHookList*
get_session_closed_hooks (void)
{
    return session_closed_hooks;
}

GSList*
get_provider_list (void)
{
    return provider_list;
}

gboolean
get_qof_providers_initialized (void)
{
    return qof_providers_initialized;
}

void
unregister_all_providers (void)
{
    if (provider_list)
    {
        g_slist_foreach (provider_list, (GFunc) g_free, NULL);
        g_slist_free (provider_list);
        provider_list = NULL;
    }
}

/* ====================================================================== */

void
qof_backend_register_provider (QofBackendProvider *prov)
{
    provider_list = g_slist_append (provider_list, prov);
}

GList*
qof_backend_get_registered_access_method_list(void)
{
    GList* list = NULL;
    GSList* node;

    for ( node = provider_list; node != NULL; node = node->next )
    {
        QofBackendProvider *prov = static_cast<QofBackendProvider*>(node->data);
        list = g_list_append( list, (gchar*)prov->access_method );
    }

    return list;
}

/* ====================================================================== */

/* hook routines */

void
qof_session_add_close_hook (GFunc fn, gpointer data)
{
    GHook *hook;

    if (session_closed_hooks == NULL)
    {
        session_closed_hooks = static_cast<GHookList*>(malloc(sizeof(GHookList))); /* LEAKED */
        g_hook_list_init (session_closed_hooks, sizeof(GHook));
    }

    hook = g_hook_alloc(session_closed_hooks);
    if (!hook)
        return;

    hook->func = reinterpret_cast<void*>(fn);
    hook->data = data;
    g_hook_append(session_closed_hooks, hook);
}

void
qof_session_call_close_hooks (QofSession *session)
{
    GHook *hook;
    GFunc fn;

    if (session_closed_hooks == NULL)
        return;

    hook = g_hook_first_valid (session_closed_hooks, FALSE);
    while (hook)
    {
        fn = (GFunc)hook->func;
        fn(session, hook->data);
        hook = g_hook_next_valid (session_closed_hooks, hook, FALSE);
    }
}

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
        }
        while (ERR_BACKEND_NO_ERR != err);
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
qof_session_get_error_message(const QofSession *session)
{
    if (!session) return "";
    if (!session->error_message)
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

    session->entity.e_type = QOF_ID_SESSION;
    session->book = qof_book_new ();
    session->book_id = NULL;
    session->backend = NULL;
    session->lock = 1;

    qof_session_clear_error (session);
}

QofSession *
qof_session_new (void)
{
    QofSession *session = g_new0(QofSession, 1);
    qof_session_init(session);
    return session;
}

QofBook *
qof_session_get_book (const QofSession *session)
{
    if (!session) return NULL;
    if (!session->book) return NULL;

    if ('y' == session->book->book_open)
    {
        return session->book;
    }
    else
    {
        return NULL;
    }
}

QofBackend *
qof_session_get_backend (const QofSession *session)
{
    if (!session) return NULL;
    return session->backend;
}

const char *
qof_session_get_file_path (const QofSession *session)
{
    if (!session) return NULL;
    if (!session->backend) return NULL;
    return session->backend->fullpath;
}

const char *
qof_session_get_url (const QofSession *session)
{
    if (!session) return NULL;
    return session->book_id;
}

void
qof_session_ensure_all_data_loaded (QofSession *session)
{
    QofBackend* backend;

    if (session == NULL) return;
    backend = qof_session_get_backend(session);
    if (backend == NULL) return;

    if (backend->load == NULL) return;
    backend->load(backend, qof_session_get_book(session), LOAD_TYPE_LOAD_ALL);
    qof_session_push_error (session, qof_backend_get_error(backend), NULL);
}

/* ====================================================================== */

/** Programs that use their own backends also need to call
the default QOF ones. The backends specified here are
loaded only by applications that do not have their own. */
struct backend_providers
{
    const char *libdir;
    const char *filename;
};

static void
qof_session_load_backend(QofSession * session, const char * access_method)
{
    GSList *p;
    QofBackendProvider *prov;
    char *msg;
    gboolean prov_type;
    gboolean (*type_check) (const char*);

    ENTER (" list=%d, initted=%s", g_slist_length(provider_list),
           qof_providers_initialized ? "true" : "false");
    prov_type = FALSE;
    if (!qof_providers_initialized)
    {
        qof_providers_initialized = TRUE;
    }
    p = provider_list;
    while (p != NULL)
    {
        prov = static_cast<QofBackendProvider*>(p->data);
        /* Does this provider handle the desired access method? */
        if (0 == g_ascii_strcasecmp (access_method, prov->access_method))
        {
            /* More than one backend could provide this
            access method, check file type compatibility. */
            type_check = (gboolean (*)(const char*)) prov->check_data_type;
            if (type_check)
            {
                prov_type = (type_check)(session->book_id);
                if (!prov_type)
                {
                    PINFO(" %s not usable", prov->provider_name);
                    p = p->next;
                    continue;
                }
            }
            PINFO (" selected %s", prov->provider_name);
            if (NULL == prov->backend_new)
            {
                p = p->next;
                continue;
            }
            /* Use the providers creation callback */
            session->backend = (*(prov->backend_new))();
            session->backend->provider = prov;
            /* Tell the book about the backend that they'll be using. */
            qof_book_set_backend (session->book, session->backend);
            LEAVE (" ");
            return;
        }
        p = p->next;
    }
    msg = g_strdup_printf("failed to load '%s' using access_method", access_method);
    qof_session_push_error (session, ERR_BACKEND_NO_HANDLER, msg);
    g_free(msg);
    LEAVE (" ");
}

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
                   gboolean ignore_lock, gboolean create, gboolean force)
{
    gchar *scheme = NULL, *filename = NULL;

    if (!session) return;

    ENTER (" sess=%p ignore_lock=%d, book-id=%s",
           session, ignore_lock,
           book_id ? book_id : "(null)");

    /* Clear the error condition of previous errors */
    qof_session_clear_error (session);

    /* Check to see if this session is already open */
    if (session->book_id)
    {
        if (ERR_BACKEND_NO_ERR != qof_session_get_error(session))
            qof_session_push_error (session, ERR_BACKEND_LOCKED, NULL);
        LEAVE("push error book is already open ");
        return;
    }

    /* seriously invalid */
    if (!book_id)
    {
        if (ERR_BACKEND_NO_ERR != qof_session_get_error(session))
            qof_session_push_error (session, ERR_BACKEND_BAD_URL, NULL);
        LEAVE("push error missing book_id");
        return;
    }
    scheme = g_uri_parse_scheme (book_id);
    if (g_strcmp0 (scheme, "file") == 0)
        filename = g_filename_from_uri (book_id, NULL, NULL);
    else if (!scheme)
        filename = g_strdup (book_id);

    if (filename && g_file_test (filename, G_FILE_TEST_IS_DIR))
    {
        if (ERR_BACKEND_NO_ERR == qof_session_get_error(session))
            qof_session_push_error (session, ERR_BACKEND_BAD_URL, NULL);
        g_free (filename);
        g_free (scheme);
        LEAVE("Can't open a directory");
        return;
    }


    /* destroy the old backend */
    qof_session_destroy_backend(session);

    /* Store the session URL  */
    session->book_id = g_strdup (book_id);

    if (filename)
        qof_session_load_backend(session, "file");
    else                       /* access method found, load appropriate backend */
        qof_session_load_backend(session, scheme);
    g_free (filename);
    g_free (scheme);

    /* No backend was found. That's bad. */
    if (NULL == session->backend)
    {
        g_free(session->book_id);
        session->book_id = NULL;
        if (ERR_BACKEND_NO_ERR == qof_session_get_error(session))
            qof_session_push_error (session, ERR_BACKEND_BAD_URL, NULL);
        LEAVE (" BAD: no backend: sess=%p book-id=%s",
               session,  book_id ? book_id : "(null)");
        return;
    }

    /* If there's a begin method, call that. */
    if (session->backend->session_begin)
    {
        char *msg;
        QofBackendError err;

        (session->backend->session_begin)(session->backend, session,
                                          session->book_id, ignore_lock,
                                          create, force);
        PINFO("Done running session_begin on backend");
        err = qof_backend_get_error(session->backend);
        msg = qof_backend_get_message(session->backend);
        if (err != ERR_BACKEND_NO_ERR)
        {
            g_free(session->book_id);
            session->book_id = NULL;
            qof_session_push_error (session, err, msg);
            LEAVE(" backend error %d %s", err, msg ? msg : "(null)");
            return;
        }
        if (msg != NULL)
        {
            PWARN("%s", msg);
            g_free(msg);
        }
    }

    LEAVE (" sess=%p book-id=%s",
           session,  book_id ? book_id : "(null)");
}

/* ====================================================================== */

void
qof_session_load (QofSession *session,
                  QofPercentageFunc percentage_func)
{
    QofBook *newbook, *oldbook;
    QofBackend *be;
    QofBackendError err;

    if (!session) return;
    if (!session->book_id) return;

    ENTER ("sess=%p book_id=%s", session, session->book_id
           ? session->book_id : "(null)");

    /* At this point, we should are supposed to have a valid book
    * id and a lock on the file. */

    oldbook = session->book;

    /* XXX why are we creating a book here? I think the books
    * need to be handled by the backend ... especially since
    * the backend may need to load multiple books ... XXX. FIXME.
    */
    newbook = qof_book_new();
    session->book = newbook;
    PINFO ("new book=%p", newbook);

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
        be->percentage = percentage_func;

        if (be->load)
        {
            be->load (be, newbook, LOAD_TYPE_INITIAL_LOAD);
            qof_session_push_error (session, qof_backend_get_error(be), NULL);
        }
    }

    /* XXX if the load fails, then we try to restore the old set of books;
    * however, we don't undo the session id (the URL).  Thus if the
    * user attempts to save after a failed load, they weill be trying to
    * save to some bogus URL.   This is wrong. XXX  FIXME.
    */
    err = qof_session_get_error(session);
    if ((err != ERR_BACKEND_NO_ERR) &&
            (err != ERR_FILEIO_FILE_TOO_OLD) &&
            (err != ERR_FILEIO_NO_ENCODING) &&
            (err != ERR_FILEIO_FILE_UPGRADE) &&
            (err != ERR_SQL_DB_TOO_OLD) &&
            (err != ERR_SQL_DB_TOO_NEW))
    {
        /* Something broke, put back the old stuff */
        qof_book_set_backend (newbook, NULL);
        qof_book_destroy (newbook);
        session->book = oldbook;
        LEAVE("error from backend %d", qof_session_get_error(session));
        return;
    }
    qof_book_set_backend (oldbook, NULL);
    qof_book_destroy (oldbook);

    LEAVE ("sess = %p, book_id=%s", session, session->book_id
           ? session->book_id : "(null)");
}

/* ====================================================================== */

static gboolean
save_error_handler(QofBackend *be, QofSession *session)
{
    QofBackendError err;
    err = qof_backend_get_error(be);

    if (ERR_BACKEND_NO_ERR != err)
    {
        qof_session_push_error (session, err, NULL);
        return TRUE;
    }
    return FALSE;
}

void
qof_session_save (QofSession *session,
                  QofPercentageFunc percentage_func)
{
    QofBackend *be;

    if (!session) return;
    if (!g_atomic_int_dec_and_test(&session->lock))
        goto leave;
    ENTER ("sess=%p book_id=%s",
           session, session->book_id ? session->book_id : "(null)");

    /* If there is a backend, and the backend is reachable
    * (i.e. we can communicate with it), then synchronize with
    * the backend.  If we cannot contact the backend (e.g.
    * because we've gone offline, the network has crashed, etc.)
    * then give the user the option to save to the local disk.
    *
    * hack alert -- FIXME -- XXX the code below no longer
    * does what the words above say.  This needs fixing.
    */
    be = session->backend;
    if (be)
    {
        /* if invoked as SaveAs(), then backend not yet set */
        qof_book_set_backend (session->book, be);
        be->percentage = percentage_func;
        if (be->sync)
        {
            (be->sync)(be, session->book);
            if (save_error_handler(be, session))
                goto leave;
        }

        /* If we got to here, then the backend saved everything
        * just fine, and we are done. So return. */
        /* Return the book_id to previous value. */
        qof_session_clear_error (session);
        LEAVE("Success");
        goto leave;
    }
    else
    {
        if (ERR_BACKEND_NO_ERR != qof_session_get_error(session))
        {
	    /* push_error strdups, stack const is fine. */
	    const char *msg = "failed to load backend";
            qof_session_push_error(session, ERR_BACKEND_NO_HANDLER, msg);
        }
    }
    LEAVE("error -- No backend!");
leave:
    g_atomic_int_inc(&session->lock);
    return;
}

void
qof_session_safe_save(QofSession *session, QofPercentageFunc percentage_func)
{
    QofBackend *be = session->backend;
    QofBackendError err;
    char *msg = NULL;
    g_return_if_fail( be != NULL );
    g_return_if_fail( be->safe_sync != NULL );
    be->percentage = percentage_func;
    (be->safe_sync)( be, qof_session_get_book( session ));
    err = qof_backend_get_error(session->backend);
    msg = qof_backend_get_message(session->backend);
    if (err != ERR_BACKEND_NO_ERR)
    {
        g_free(session->book_id);
        session->book_id = NULL;
        qof_session_push_error (session, err, msg);
        /* qof_backend_get_message transfers ownership. */
	g_free(msg);
    }
}


/* ====================================================================== */
gboolean
qof_session_save_in_progress(const QofSession *session)
{
    return (session && g_atomic_int_get(&session->lock) != 1);
}

void
qof_session_end (QofSession *session)
{
    if (!session) return;

    ENTER ("sess=%p book_id=%s", session, session->book_id
           ? session->book_id : "(null)");

    /* close down the backend first */
    if (session->backend && session->backend->session_end)
    {
        (session->backend->session_end)(session->backend);
    }

    qof_session_clear_error (session);

    g_free (session->book_id);
    session->book_id = NULL;

    LEAVE ("sess=%p book_id=%s", session, session->book_id
           ? session->book_id : "(null)");
}

void
qof_session_destroy (QofSession *session)
{
    if (!session) return;

    ENTER ("sess=%p book_id=%s", session, session->book_id
           ? session->book_id : "(null)");

    qof_session_end (session);

    /* destroy the backend */
    qof_session_destroy_backend(session);

    qof_book_set_backend (session->book, NULL);
    qof_book_destroy (session->book);
    session->book  = NULL;

    g_free (session);

    LEAVE ("sess=%p", session);
}

/* ====================================================================== */
/* this call is weird. */

void
qof_session_swap_data (QofSession *session_1, QofSession *session_2)
{
    QofBook *book_1, *book_2;
    gboolean tmp;

    if (session_1 == session_2) return;
    if (!session_1 || !session_2) return;

    ENTER ("sess1=%p sess2=%p", session_1, session_2);

    book_1 = session_1->book;
    book_2 = session_2->book;

    // Swap the read_only flags backwards.
    tmp = book_1->read_only;
    book_1->read_only = book_2->read_only;
    book_2->read_only = tmp;

    session_1->book = book_2;
    session_2->book = book_1;

    qof_book_set_backend (book_1, session_2->backend);
    qof_book_set_backend (book_2, session_1->backend);

    LEAVE (" ");
}

/* ====================================================================== */

gboolean
qof_session_events_pending (const QofSession *session)
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

/* XXX This exports the list of accounts to a file.  It does not
 * export any transactions.  It's a place-holder until full
 * book-closing is implemented.
 */
gboolean
qof_session_export (QofSession *tmp_session,
                    QofSession *real_session,
                    QofPercentageFunc percentage_func)
{
    QofBook *book, *book2;
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
    book2 = qof_session_get_book(tmp_session);
    be = qof_book_get_backend(book2);
    if (!be)
        return FALSE;

    be->percentage = percentage_func;
    if (be->export_fn)
    {
        int err;

        (be->export_fn)(be, book);
        err = qof_backend_get_error(be);

        if (ERR_BACKEND_NO_ERR != err)
        {
            return FALSE;
        }
    }

    return TRUE;
}

/* ================= Static function access for testing ================= */

#ifdef __cplusplus
extern "C"
{
#endif

void init_static_qofsession_pointers (void);

void (*p_qof_session_load_backend) (QofSession * session, const char * access_method);
void (*p_qof_session_clear_error) (QofSession *session);
void (*p_qof_session_destroy_backend) (QofSession *session);

#ifdef __cplusplus
}
#endif

void
init_static_qofsession_pointers (void)
{
    p_qof_session_load_backend = qof_session_load_backend;
    p_qof_session_clear_error = qof_session_clear_error;
    p_qof_session_destroy_backend = qof_session_destroy_backend;
}

/* =================== END OF FILE ====================================== */
