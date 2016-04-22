/********************************************************************\
 * qofsesssion.cpp -- session access (connection to backend)        *
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
 @author Copyright (c) 2016 Aaron Laws
   */

#include "config.h"

#include <platform.h>
#if PLATFORM(WINDOWS)
#include <windows.h>
#endif

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
#include "qofsession.hpp"
#include "qofobject-p.h"
#include <vector>

static QofLogModule log_module = QOF_MOD_SESSION;
static std::vector<QofBackendProvider *> provider_list;

/*
 * These getters are used in tests to reach static vars from outside
 * They should be removed when no longer needed
 */

#ifdef __cplusplus
extern "C"
{
#endif

gboolean get_qof_providers_initialized (void );
void unregister_all_providers (void );

#ifdef __cplusplus
}
#endif

void
unregister_all_providers (void)
{
    provider_list.clear ();
}

/* ====================================================================== */

void
qof_backend_register_provider (QofBackendProvider *prov)
{
    provider_list.push_back (prov);
}

GList*
qof_backend_get_registered_access_method_list(void)
{
    GList* list = NULL;
    GSList* node;

    for (auto const & temp_provider : provider_list)
        list = g_list_append (list, (gchar*)temp_provider->access_method);

    return list;
}

/* ====================================================================== */
/* error handling routines */

void
QofSessionImpl::clear_error () noexcept
{
    this->last_err = ERR_BACKEND_NO_ERR;
    g_free (this->error_message);
    this->error_message = nullptr;

    /* pop the stack on the backend as well. */
    if (backend)
    {
        QofBackendError err;
        do
            err = qof_backend_get_error (backend);
        while (ERR_BACKEND_NO_ERR != err);
    }
}

static void
qof_session_clear_error (QofSession *session)
{
    if (!session)
        return;
    session->clear_error ();
}

void
QofSessionImpl::push_error (QofBackendError const err, const char * message) noexcept
{
    this->last_err = err;
    g_free (error_message);
    this->error_message = g_strdup (message);
}

void
qof_session_push_error (QofSession *session, QofBackendError err,
                        const char *message)
{
    if (!session)
        return;
    session->push_error (err, message);
}

QofBackendError
QofSessionImpl::get_error () noexcept
{
    /* if we have a local error, return that. */
    if (ERR_BACKEND_NO_ERR != this->last_err)
    {
        return this->last_err;
    }

    if (!this->backend) return ERR_BACKEND_NO_ERR;

    auto err = qof_backend_get_error (this->backend);
    this->last_err = err;
    return err;
}

QofBackendError
qof_session_get_error (QofSession * session)
{
    if (!session) return ERR_BACKEND_NO_BACKEND;
    return session->get_error();
}

const char *
QofSessionImpl::get_error_message () const noexcept
{
    if (!this->error_message)
        return "";
    return this->error_message;
}

const char *
qof_session_get_error_message (const QofSession * session)
{
    if (!session) return "";
    return session->get_error_message ();
}

QofBackendError
QofSessionImpl::pop_error () noexcept
{
    QofBackendError err {this->get_error ()};
    this->clear_error ();
    return err;
}

QofBackendError
qof_session_pop_error (QofSession * session)
{
    if (!session) return ERR_BACKEND_NO_BACKEND;
    return session->pop_error ();
}

/* ====================================================================== */

QofSessionImpl::QofSessionImpl () noexcept
    : book {qof_book_new ()},
    book_id {nullptr},
    error_message {nullptr},
    backend {nullptr},
    saving {false}
{
    entity.e_type = QOF_ID_SESSION;
    clear_error ();
}

void
qof_session_destroy (QofSession * session)
{
    delete session;
}

QofSession *
qof_session_new (void)
{
    return new QofSessionImpl;
}

QofBook *
QofSessionImpl::get_book () const noexcept
{
    if (!this->book) return nullptr;
    if ('y' == this->book->book_open)
        return this->book;
    return nullptr;
}

QofBook *
qof_session_get_book (const QofSession *session)
{
    if (!session) return NULL;
    return session->get_book ();
}

void
qof_session_set_book_id (QofSession *session, char *book_id)
{
    if (!session) return;
    session->book_id = book_id;
}

void
qof_session_set_backend(QofSession *session, QofBackend * backend)
{
    if (!session) return;
    session->backend = backend;
}

QofBackend *
qof_session_get_backend (const QofSession *session)
{
    if (!session) return NULL;
    return session->backend;
}

QofInstance
qof_session_get_entity (const QofSession *session)
{
    if (!session) return {};
    return session->entity;
}

const char *
qof_session_get_book_id (const QofSession *session)
{
    if (!session) return NULL;
    return session->book_id;
}

const char *
QofSessionImpl::get_file_path () const noexcept
{
    if (!this->backend) return nullptr;
    return this->backend->fullpath;
}

const char *
qof_session_get_file_path (const QofSession *session)
{
    if (!session) return NULL;
    return session->get_file_path ();
}

const char *
qof_session_get_url (const QofSession *session)
{
    if (!session) return NULL;
    return session->book_id;
}

void
QofSessionImpl::ensure_all_data_loaded () noexcept
{
    if (!this->backend) return;
    if (!this->backend->load) return;

    this->backend->load(this->backend, this->get_book (), LOAD_TYPE_LOAD_ALL);
    this->push_error (qof_backend_get_error (this->backend), nullptr);
}

void
qof_session_ensure_all_data_loaded (QofSession *session)
{
    if (session == nullptr) return;
    return session->ensure_all_data_loaded ();
}

/* ====================================================================== */

void
QofSessionImpl::load_backend (const char * access_method) noexcept
{
    ENTER (" list=%lu", provider_list.size ());
    bool prov_type {false};
    for (auto const & prov : provider_list)
    {
        /* Does this provider handle the desired access method? */
        if (g_ascii_strcasecmp (access_method, prov->access_method))
            continue;
        /* More than one backend could provide this
        access method, check file type compatibility. */
        auto type_check = prov->check_data_type;
        if (type_check)
        {
            gboolean type_check_result = (type_check) (this->book_id);
            bool prov_type {type_check_result != 0};
            if (!prov_type)
            {
                PINFO(" %s not usable", prov->provider_name);
                continue;
            }
        }
        PINFO (" selected %s", prov->provider_name);
        if (!prov->backend_new)
        {
            PINFO ("  no constructor defined.");
            continue;
        }
        /* Use the providers creation callback */
        this->backend = (*(prov->backend_new))();
        this->backend->provider = prov;
        /* Tell the book about the backend that he'll be using. */
        qof_book_set_backend (this->book, this->backend);
        LEAVE (" ");
        return;
    }
    char * msg {g_strdup_printf ("failed to load '%s' using access_method", access_method)};
    this->push_error (ERR_BACKEND_NO_HANDLER, msg);
    g_free(msg);
    LEAVE (" ");
}

static void
qof_session_load_backend(QofSession * session, const char * access_method)
{
    if (!session)
        return;
    session->load_backend (access_method);
}

/* ====================================================================== */

void
QofSessionImpl::destroy_backend () noexcept
{
    if (this->backend)
    {
        clear_error ();

        if (this->backend->destroy_backend)
            this->backend->destroy_backend (this->backend);
        else
            g_free(this->backend);

        this->backend = nullptr;
    }
}

static void
qof_session_destroy_backend (QofSession *session)
{
    if (!session) return;
    session->destroy_backend ();
}

void
QofSessionImpl::begin (const char * new_book_id, bool ignore_lock, bool create, bool force) noexcept
{
    ENTER (" sess=%p ignore_lock=%d, book-id=%s",
           this, ignore_lock,
           new_book_id ? new_book_id : "(null)");

    clear_error ();

    /* Check to see if this session is already open */
    if (this->book_id)
    {
        if (ERR_BACKEND_NO_ERR != get_error ())
            push_error (ERR_BACKEND_LOCKED, nullptr);
        LEAVE("push error book is already open ");
        return;
    }

    /* seriously invalid */
    if (!new_book_id)
    {
        if (ERR_BACKEND_NO_ERR != get_error ())
            push_error (ERR_BACKEND_BAD_URL, nullptr);
        LEAVE("push error missing new_book_id");
        return;
    }

    char * scheme {g_uri_parse_scheme (new_book_id)};
    char * filename {nullptr};
    if (g_strcmp0 (scheme, "file") == 0)
        filename = g_filename_from_uri (new_book_id, nullptr, nullptr);
    else if (!scheme)
        filename = g_strdup (new_book_id);

    if (filename && g_file_test (filename, G_FILE_TEST_IS_DIR))
    {
        if (ERR_BACKEND_NO_ERR == get_error ())
            push_error (ERR_BACKEND_BAD_URL, nullptr);
        g_free (filename);
        g_free (scheme);
        LEAVE("Can't open a directory");
        return;
    }

    /* destroy the old backend */
    destroy_backend ();

    /* Store the session URL  */
    book_id = g_strdup (new_book_id);

    if (filename)
        load_backend ("file");
    else                       /* access method found, load appropriate backend */
        load_backend (scheme);
    g_free (filename);
    g_free (scheme);

    /* No backend was found. That's bad. */
    if (nullptr == backend)
    {
        g_free(book_id);
        book_id = nullptr;
        if (ERR_BACKEND_NO_ERR == get_error ())
            push_error (ERR_BACKEND_BAD_URL, nullptr);
        LEAVE (" BAD: no backend: sess=%p book-id=%s",
               this,  new_book_id ? new_book_id : "(null)");
        return;
    }

    /* If there's a begin method, call that. */
    if (backend->session_begin)
    {
        (backend->session_begin) (backend, this, book_id, ignore_lock, create, force);
        PINFO ("Done running session_begin on backend");
        QofBackendError const err {qof_backend_get_error (backend)};
        char * msg {qof_backend_get_message (backend)};
        if (err != ERR_BACKEND_NO_ERR)
        {
            g_free(book_id);
            book_id = nullptr;
            push_error (err, msg);
            LEAVE(" backend error %d %s", err, msg ? msg : "(null)");
            return;
        }
        if (msg != nullptr)
        {
            PWARN("%s", msg);
            g_free(msg);
        }
    }

    LEAVE (" sess=%p book-id=%s", this,  new_book_id ? new_book_id : "(null)");
}

void
qof_session_begin (QofSession *session, const char * book_id,
                   gboolean ignore_lock, gboolean create, gboolean force)
{
    if (!session) return;
    session->begin(book_id, ignore_lock, create, force);
}

/* ====================================================================== */

void
QofSessionImpl::load (QofPercentageFunc percentage_func) noexcept
{
    if (!this->book_id) return;

    ENTER ("sess=%p book_id=%s", this, this->book_id
           ? this->book_id : "(null)");

    /* At this point, we should are supposed to have a valid book
    * id and a lock on the file. */

    QofBook * oldbook {this->book};

    QofBook * newbook {qof_book_new ()};
    this->book = newbook;
    PINFO ("new book=%p", newbook);

    clear_error ();

    /* This code should be sufficient to initialize *any* backend,
    * whether http, postgres, or anything else that might come along.
    * Basically, the idea is that by now, a backend has already been
    * created & set up.  At this point, we only need to get the
    * top-level account group out of the backend, and that is a
    * generic, backend-independent operation.
    */
    QofBackend * be {this->backend};
    qof_book_set_backend (newbook, be);

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
            this->push_error (qof_backend_get_error(be), nullptr);
        }
    }

    /* XXX if the load fails, then we try to restore the old set of books;
    * however, we don't undo the session id (the URL).  Thus if the
    * user attempts to save after a failed load, they weill be trying to
    * save to some bogus URL.   This is wrong. XXX  FIXME.
    */
    auto err = this->get_error ();
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
        this->book = oldbook;
        LEAVE ("error from backend %d", this->get_error ());
        return;
    }
    qof_book_set_backend (oldbook, NULL);
    qof_book_destroy (oldbook);

    LEAVE ("sess = %p, book_id=%s", this, this->book_id
           ? this->book_id : "(null)");
}

void
qof_session_load (QofSession *session,
                  QofPercentageFunc percentage_func)
{
    if (!session) return;
    session->load (percentage_func);
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
QofSessionImpl::save (QofPercentageFunc percentage_func) noexcept
{
    saving = true;
    ENTER ("sess=%p book_id=%s",
           this, this->book_id ? this->book_id : "(null)");

    /* If there is a backend, and the backend is reachable
    * (i.e. we can communicate with it), then synchronize with
    * the backend.  If we cannot contact the backend (e.g.
    * because we've gone offline, the network has crashed, etc.)
    * then give the user the option to save to the local disk.
    *
    * hack alert -- FIXME -- XXX the code below no longer
    * does what the words above say.  This needs fixing.
    */
    QofBackend * be = this->backend;
    if (be)
    {
        /* if invoked as SaveAs(), then backend not yet set */
        qof_book_set_backend (this->book, be);
        be->percentage = percentage_func;
        if (be->sync)
        {
            (be->sync)(be, this->book);
            if (save_error_handler(be, this))
                goto leave;
        }

        /* If we got to here, then the backend saved everything
        * just fine, and we are done. So return. */
        /* Return the book_id to previous value. */
        this->clear_error ();
        LEAVE("Success");
        goto leave;
    }
    else
    {
        if (ERR_BACKEND_NO_ERR != this->get_error ())
        {
            /* push_error strdups, stack const is fine. */
            const char *msg = "failed to load backend";
            this->push_error (ERR_BACKEND_NO_HANDLER, msg);
        }
    }
    LEAVE("error -- No backend!");
leave:
    saving = false;
    return;
}

void
qof_session_save (QofSession *session,
                  QofPercentageFunc percentage_func)
{
    if (!session) return;
    session->save (percentage_func);
}

void
QofSessionImpl::safe_save (QofPercentageFunc percentage_func) noexcept
{
    QofBackend *be = this->backend;
    if (!be) return;
    if (!be->safe_sync) return;
    be->percentage = percentage_func;
    (be->safe_sync) (be, this->get_book ());
    QofBackendError err = qof_backend_get_error (this->backend);
    char * msg = qof_backend_get_message (this->backend);
    if (err != ERR_BACKEND_NO_ERR)
    {
        g_free (this->book_id);
        this->book_id = nullptr;
        push_error (err, msg);
    }
    g_free(msg);
}

void
qof_session_safe_save(QofSession *session, QofPercentageFunc percentage_func)
{
    if (!session) return;
    session->safe_save (percentage_func);
}


/* ====================================================================== */

gboolean
qof_session_save_in_progress(const QofSession *session)
{
    if (!session) return false;
    return session->saving;
}

void
QofSessionImpl::end () noexcept
{
    ENTER ("sess=%p book_id=%s", this, this->book_id
           ? this->book_id : "(null)");

    /* close down the backend first */
    if (this->backend && this->backend->session_end)
        (this->backend->session_end) (this->backend);

    this->clear_error ();

    g_free (this->book_id);
    this->book_id = nullptr;

    LEAVE ("sess=%p book_id=%s", this, this->book_id
           ? this->book_id : "(null)");
}

void
qof_session_end (QofSession *session)
{
    if (!session) return;
    session->end ();
}

QofSessionImpl::~QofSessionImpl () noexcept
{
    ENTER ("sess=%p book_id=%s", this, book_id
           ? book_id : "(null)");
    end ();
    destroy_backend ();
    qof_book_set_backend (this->book, nullptr);
    qof_book_destroy (this->book);
    this->book = nullptr;
    LEAVE ("sess=%p", this);
}

void
QofSessionImpl::swap_books (QofSessionImpl & other) noexcept
{
    ENTER ("sess1=%p sess2=%p", this, &other);

    QofBook * book_1 {book};
    QofBook * book_2 {other.book};

    // Swap the read_only flags backwards.
    bool tmp = book_1->read_only;
    book_1->read_only = book_2->read_only;
    book_2->read_only = tmp;

    book = book_2;
    other.book = book_1;

    qof_book_set_backend (book_1, other.backend);
    qof_book_set_backend (book_2, backend);

    LEAVE (" ");
}
/* ====================================================================== */
/* this call is weird. */

void
qof_session_swap_data (QofSession *session_1, QofSession *session_2)
{
    if (session_1 == session_2) return;
    if (!session_1 || !session_2) return;
    session_1->swap_books (*session_2);
}

/* ====================================================================== */

bool
QofSessionImpl::events_pending () const noexcept
{
    if (!this->backend) return false;
    if (!this->backend->events_pending) return false;
    return this->backend->events_pending (this->backend);
}

gboolean
qof_session_events_pending (const QofSession *session)
{
    if (!session) return false;
    return session->events_pending ();
}

bool
QofSessionImpl::process_events () const noexcept
{
    if (!this->backend) return false;
    if (!this->backend->process_events) return false;

    return this->backend->process_events (this->backend);
}

gboolean
qof_session_process_events (QofSession *session)
{
    if (!session) return FALSE;
    return session->process_events ();
}

bool
QofSessionImpl::export_session (QofSessionImpl & real_session, QofPercentageFunc percentage_func) noexcept
{
    //QofBook *book, *book2;
    //QofBackend *be;

    QofBook * book = real_session.get_book ();
    ENTER ("tmp_session=%p real_session=%p book=%p book_id=%s",
           this, &real_session, book,
           this->book_id ? this->book_id : "(null)");

    /* There must be a backend or else.  (It should always be the file
     * backend too.)
     */
    QofBook * book2 = this->get_book ();
    QofBackend * be2 = qof_book_get_backend(book2);
    if (!be2) return false;

    be2->percentage = percentage_func;
    if (!be2->export_fn) return true;

    (be2->export_fn)(be2, book);
    int err = qof_backend_get_error(be2);
    if (ERR_BACKEND_NO_ERR != err)
        return false;
    return true;
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
    if ((!tmp_session) || (!real_session)) return FALSE;
    return tmp_session->export_session (*real_session, percentage_func);
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
