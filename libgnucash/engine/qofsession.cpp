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

extern "C"
{

#include "config.h"

#include <stdlib.h>
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
#include "qofobject-p.h"

static QofLogModule log_module = QOF_MOD_SESSION;
} //extern 'C'

#include "qofbook-p.h"
#include "qof-backend.hpp"
#include "qofsession.hpp"
#include "gnc-backend-prov.hpp"

#include <vector>
#include <boost/algorithm/string.hpp>
#include <vector>
#include <algorithm>
#include <string>
#include <sstream>

using ProviderVec =  std::vector<QofBackendProvider_ptr>;
static ProviderVec s_providers;
/*
 * These getters are used in tests to reach static vars from outside
 * They should be removed when no longer needed
 */

ProviderVec& get_providers (void );
bool get_providers_initialized (void );

ProviderVec&
get_providers (void)
{
    return s_providers;
}

bool
get_providers_initialized (void)
{
    return !s_providers.empty();
}

void
qof_backend_register_provider (QofBackendProvider_ptr&& prov)
{
    s_providers.emplace_back(std::move(prov));
}

void
qof_backend_unregister_all_providers ()
{
    s_providers.clear ();
}

/* Called from C so we have to keep the GList for now. */
GList*
qof_backend_get_registered_access_method_list(void)
{
    GList* list = NULL;
    std::for_each(s_providers.begin(), s_providers.end(),
                  [&list](QofBackendProvider_ptr& provider) {
                      gpointer method = reinterpret_cast<gpointer>(const_cast<char*>(provider->access_method));
                      list = g_list_prepend(list, method);
                  });
    return list;
}

/* QofSessionImpl */
/* ====================================================================== */
/* Constructor/Destructor ----------------------------------*/

QofSessionImpl::QofSessionImpl () noexcept
    : m_book {qof_book_new ()},
    m_book_id {},
    m_saving {false},
    m_last_err {},
    m_error_message {}
{
    clear_error ();
}

QofSessionImpl::~QofSessionImpl () noexcept
{
    ENTER ("sess=%p book_id=%s", this, m_book_id.c_str ());
    end ();
    destroy_backend ();
    qof_book_set_backend (m_book, nullptr);
    qof_book_destroy (m_book);
    m_book = nullptr;
    LEAVE ("sess=%p", this);
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

void
QofSessionImpl::destroy_backend () noexcept
{
    auto backend = qof_book_get_backend (m_book);
    if (backend)
    {
        clear_error ();
        delete backend;
        qof_book_set_backend (m_book, nullptr);
    }
}

/* ====================================================================== */

void
QofSessionImpl::load_backend (std::string access_method) noexcept
{
    std::ostringstream s;
    s << " list=" << s_providers.size();
    ENTER ("%s", s.str().c_str());
    for (auto const & prov : s_providers)
    {
        if (!boost::iequals (access_method, prov->access_method))
        {
            PINFO ("The provider providers access_method, %s, but we're loading for access_method, %s. Skipping.",
                    prov->access_method, access_method.c_str ());
            continue;
        }
        PINFO (" Selected provider %s", prov->provider_name);
        if (!prov->type_check (m_book_id.c_str ()))
        {
            PINFO("Provider, %s, reported not being usable for book, %s.",
                    prov->provider_name, m_book_id.c_str ());
            continue;
        }
        auto backend = prov->create_backend();
        qof_book_set_backend (m_book, backend);
        LEAVE (" ");
        return;
    }
    std::string msg {"failed to get_backend using access method \"" + access_method + "\""};
    push_error (ERR_BACKEND_NO_HANDLER, msg);
    LEAVE (" ");
}

void
QofSessionImpl::load (QofPercentageFunc percentage_func) noexcept
{
    if (!m_book_id.size ()) return;
    ENTER ("sess=%p book_id=%s", this, m_book_id.c_str ());

    /* At this point, we should are supposed to have a valid book
    * id and a lock on the file. */
    QofBook * oldbook {m_book};

    QofBook * newbook {qof_book_new ()};
    m_book = newbook;
    PINFO ("new book=%p", newbook);
    clear_error ();

    /* This code should be sufficient to initialize *any* backend,
    * whether http, postgres, or anything else that might come along.
    * Basically, the idea is that by now, a backend has already been
    * created & set up.  At this point, we only need to get the
    * top-level account group out of the backend, and that is a
    * generic, backend-independent operation.
    */
    auto be (qof_book_get_backend (oldbook));
    qof_book_set_backend (newbook, be);

    /* Starting the session should result in a bunch of accounts
    * and currencies being downloaded, but probably no transactions;
    * The GUI will need to do a query for that.
    */
    if (be)
    {
        be->set_percentage(percentage_func);
        be->load (newbook, LOAD_TYPE_INITIAL_LOAD);
        push_error (be->get_error(), {});
    }

    /* XXX if the load fails, then we try to restore the old set of books;
    * however, we don't undo the session id (the URL).  Thus if the
    * user attempts to save after a failed load, they weill be trying to
    * save to some bogus URL.   This is wrong. XXX  FIXME.
    */
    auto err = get_error ();
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
        m_book = oldbook;
        LEAVE ("error from backend %d", get_error ());
        return;
    }
    qof_book_set_backend (oldbook, NULL);
    qof_book_destroy (oldbook);

    LEAVE ("sess = %p, book_id=%s", this, m_book_id.c_str ());
}

void
QofSessionImpl::begin (std::string new_book_id, bool ignore_lock,
                       bool create, bool force) noexcept
{
    ENTER (" sess=%p ignore_lock=%d, book-id=%s",
           this, ignore_lock, new_book_id.c_str ());
    clear_error ();
    /* Check to see if this session is already open */
    if (m_book_id.size ())
    {
        if (ERR_BACKEND_NO_ERR != get_error ())
            push_error (ERR_BACKEND_LOCKED, {});
        LEAVE("push error book is already open ");
        return;
    }

    /* seriously invalid */
    if (!new_book_id.size ())
    {
        if (ERR_BACKEND_NO_ERR != get_error ())
            push_error (ERR_BACKEND_BAD_URL, {});
        LEAVE("push error missing new_book_id");
        return;
    }

    char * scheme {g_uri_parse_scheme (new_book_id.c_str ())};
    char * filename {nullptr};
    if (g_strcmp0 (scheme, "file") == 0)
        filename = g_filename_from_uri (new_book_id.c_str (), nullptr, nullptr);
    else if (!scheme)
        filename = g_strdup (new_book_id.c_str ());

    if (filename && g_file_test (filename, G_FILE_TEST_IS_DIR))
    {
        if (ERR_BACKEND_NO_ERR == get_error ())
            push_error (ERR_BACKEND_BAD_URL, {});
        g_free (filename);
        g_free (scheme);
        LEAVE("Can't open a directory");
        return;
    }
    /* destroy the old backend */
    destroy_backend ();
    /* Store the session URL  */
    m_book_id = new_book_id;
    if (filename)
        load_backend ("file");
    else                       /* access method found, load appropriate backend */
        load_backend (scheme);
    g_free (filename);
    g_free (scheme);

    /* No backend was found. That's bad. */
    auto backend = qof_book_get_backend (m_book);
    if (backend == nullptr)
    {
        m_book_id = {};
        if (ERR_BACKEND_NO_ERR == get_error ())
            push_error (ERR_BACKEND_BAD_URL, {});
        LEAVE (" BAD: no backend: sess=%p book-id=%s",
               this,  new_book_id.c_str ());
        return;
    }

    /* If there's a begin method, call that. */
    backend->session_begin(this, m_book_id.c_str(), ignore_lock, create, force);
    PINFO ("Done running session_begin on backend");
    QofBackendError const err {backend->get_error()};
    auto msg (backend->get_message());
    if (err != ERR_BACKEND_NO_ERR)
    {
        m_book_id = {};
        push_error (err, msg);
        LEAVE (" backend error %d %s", err, msg.empty() ? "(null)" : msg.c_str());
        return;
    }
    if (!msg.empty())
    {
        PWARN("%s", msg.c_str());
    }

    LEAVE (" sess=%p book-id=%s", this,  new_book_id.c_str ());
}

void
QofSessionImpl::end () noexcept
{
    ENTER ("sess=%p book_id=%s", this, m_book_id.c_str ());
    auto backend = qof_book_get_backend (m_book);
    if (backend != nullptr)
        backend->session_end();
    clear_error ();
    m_book_id.clear();
    LEAVE ("sess=%p book_id=%s", this, m_book_id.c_str ());
}

/* error handling functions --------------------------------*/

void
QofSessionImpl::clear_error () noexcept
{
    m_last_err = ERR_BACKEND_NO_ERR;
    m_error_message = {};

    /* pop the stack on the backend as well. */
    if (auto backend = qof_book_get_backend (m_book))
    {
        QofBackendError err = ERR_BACKEND_NO_ERR;
        do
            err = backend->get_error();
        while (err != ERR_BACKEND_NO_ERR);
    }
}

void
QofSessionImpl::push_error (QofBackendError const err, std::string message) noexcept
{
    m_last_err = err;
    m_error_message = message;
}

QofBackendError
QofSessionImpl::get_error () noexcept
{
    /* if we have a local error, return that. */
    if (m_last_err != ERR_BACKEND_NO_ERR)
        return m_last_err;
    auto qof_be = qof_book_get_backend (m_book);
    if (qof_be == nullptr) return ERR_BACKEND_NO_ERR;

    m_last_err = qof_be->get_error();
    return m_last_err;
}

std::string
QofSessionImpl::get_error_message () const noexcept
{
    return m_error_message;
}

QofBackendError
QofSessionImpl::pop_error () noexcept
{
    QofBackendError err {get_error ()};
    clear_error ();
    return err;
}

/* Accessors (getters/setters) -----------------------------*/

QofBook *
QofSessionImpl::get_book () const noexcept
{
    if (!m_book) return nullptr;
    if ('y' == m_book->book_open)
        return m_book;
    return nullptr;
}

QofBackend *
QofSession::get_backend () const noexcept
{
    return qof_book_get_backend (m_book);
}

std::string
QofSessionImpl::get_file_path () const noexcept
{
    auto backend = qof_book_get_backend (m_book);
    if (!backend) return nullptr;
    return backend->get_uri();
}

std::string const &
QofSessionImpl::get_book_id () const noexcept
{
    return m_book_id;
}

bool
QofSessionImpl::is_saving () const noexcept
{
    return m_saving;
}

/* Manipulators (save, load, etc.) -------------------------*/

void
QofSessionImpl::save (QofPercentageFunc percentage_func) noexcept
{
    m_saving = true;
    ENTER ("sess=%p book_id=%s", this, m_book_id.c_str ());

    /* If there is a backend, and the backend is reachable
    * (i.e. we can communicate with it), then synchronize with
    * the backend.  If we cannot contact the backend (e.g.
    * because we've gone offline, the network has crashed, etc.)
    * then give the user the option to save to the local disk.
    *
    * hack alert -- FIXME -- XXX the code below no longer
    * does what the words above say.  This needs fixing.
    */
    auto backend = qof_book_get_backend (m_book);
    if (backend)
    {
        /* if invoked as SaveAs(), then backend not yet set */
        qof_book_set_backend (m_book, backend);
        backend->set_percentage(percentage_func);
        backend->sync(m_book);
        auto err = backend->get_error();
        if (err != ERR_BACKEND_NO_ERR)
        {
            push_error (err, {});
            m_saving = false;
            return;
        }
        /* If we got to here, then the backend saved everything
        * just fine, and we are done. So return. */
        clear_error ();
        LEAVE("Success");
    }
    else
    {
        push_error (ERR_BACKEND_NO_HANDLER, "failod to load backend");
        LEAVE("error -- No backend!");
    }
    m_saving = false;
}

void
QofSessionImpl::safe_save (QofPercentageFunc percentage_func) noexcept
{
    auto backend = qof_book_get_backend (m_book);
    if (!backend) return;
    backend->set_percentage(percentage_func);
    backend->safe_sync(get_book ());
    auto err = backend->get_error();
    auto msg = backend->get_message();
    if (err != ERR_BACKEND_NO_ERR)
    {
        m_book_id = nullptr;
        push_error (err, msg);
    }
}

void
QofSessionImpl::ensure_all_data_loaded () noexcept
{
    auto backend = qof_book_get_backend (m_book);
    if (!backend) return;
    backend->load(m_book, LOAD_TYPE_LOAD_ALL);
    push_error (backend->get_error(), {});
}

void
QofSessionImpl::swap_books (QofSessionImpl & other) noexcept
{
    ENTER ("sess1=%p sess2=%p", this, &other);
    // don't swap (that is, double-swap) read_only flags
    std::swap (m_book->read_only, other.m_book->read_only);
    std::swap (m_book, other.m_book);
    auto mybackend = qof_book_get_backend (m_book);
    qof_book_set_backend (m_book, qof_book_get_backend (other.m_book));
    qof_book_set_backend (other.m_book, mybackend);
    LEAVE (" ");
}

bool
QofSessionImpl::events_pending () const noexcept
{
    return false;
}

bool
QofSessionImpl::process_events () const noexcept
{
    return false;
}

/* XXX This exports the list of accounts to a file.  It does not
 * export any transactions.  It's a place-holder until full
 * book-closing is implemented.
 */
bool
QofSessionImpl::export_session (QofSessionImpl & real_session,
                                QofPercentageFunc percentage_func) noexcept
{
    auto real_book = real_session.get_book ();
    ENTER ("tmp_session=%p real_session=%p book=%p book_id=%s",
           this, &real_session, real_book, m_book_id.c_str ());

    /* There must be a backend or else.  (It should always be the file
     * backend too.)
     */
    auto backend2 = qof_book_get_backend(m_book);
    if (!backend2) return false;

    backend2->set_percentage(percentage_func);

    backend2->export_coa(real_book);
    auto err = backend2->get_error();
    if (err != ERR_BACKEND_NO_ERR)
        return false;
    return true;
}

/* C Wrapper Functions */
/* ====================================================================== */

const char *
qof_session_get_error_message (const QofSession * session)
{
    if (!session) return "";
    return session->get_error_message ().c_str ();
}

QofBackendError
qof_session_pop_error (QofSession * session)
{
    if (!session) return ERR_BACKEND_NO_BACKEND;
    return session->pop_error ();
}

QofBook *
qof_session_get_book (const QofSession *session)
{
    if (!session) return NULL;
    return session->get_book ();
}

const char *
qof_session_get_file_path (const QofSession *session)
{
    if (!session) return NULL;
    return session->get_file_path ().c_str ();
}

void
qof_session_ensure_all_data_loaded (QofSession *session)
{
    if (session == nullptr) return;
    return session->ensure_all_data_loaded ();
}

const char *
qof_session_get_url (const QofSession *session)
{
    if (!session) return NULL;
    return session->get_book_id ().c_str ();
}

QofBackend *
qof_session_get_backend (const QofSession *session)
{
    if (!session) return NULL;
    return session->get_backend ();
}

void
qof_session_begin (QofSession *session, const char * book_id,
                   gboolean ignore_lock, gboolean create, gboolean force)
{
    if (!session) return;
    session->begin((book_id ? book_id : ""), ignore_lock, create, force);
}

void
qof_session_load (QofSession *session,
                  QofPercentageFunc percentage_func)
{
    if (!session) return;
    session->load (percentage_func);
}

void
qof_session_save (QofSession *session,
                  QofPercentageFunc percentage_func)
{
    if (!session) return;
    session->save (percentage_func);
}

void
qof_session_safe_save(QofSession *session, QofPercentageFunc percentage_func)
{
    if (!session) return;
    session->safe_save (percentage_func);
}

gboolean
qof_session_save_in_progress(const QofSession *session)
{
    if (!session) return false;
    return session->is_saving ();
}

void
qof_session_end (QofSession *session)
{
    if (!session) return;
    session->end ();
}

void
qof_session_swap_data (QofSession *session_1, QofSession *session_2)
{
    if (session_1 == session_2) return;
    if (!session_1 || !session_2) return;
    session_1->swap_books (*session_2);
}

gboolean
qof_session_events_pending (const QofSession *session)
{
    if (!session) return false;
    return session->events_pending ();
}

gboolean
qof_session_process_events (QofSession *session)
{
    if (!session) return FALSE;
    return session->process_events ();
}

gboolean
qof_session_export (QofSession *tmp_session,
                    QofSession *real_session,
                    QofPercentageFunc percentage_func)
{
    if ((!tmp_session) || (!real_session)) return FALSE;
    return tmp_session->export_session (*real_session, percentage_func);
}

/* ================= Static function access for testing ================= */

void init_static_qofsession_pointers (void);

void qof_session_load_backend (QofSession * session, const char * access_method)
{
    session->load_backend (access_method);
}

void qof_session_clear_error (QofSession * session)
{
    session->clear_error ();
}

void qof_session_destroy_backend (QofSession * session)
{
    session->destroy_backend ();
}

void qof_session_set_book_id (QofSession * session, char const * book_id)
{
    if (!book_id)
        session->m_book_id = "";
    else
        session->m_book_id = book_id;
}

void (*p_qof_session_load_backend) (QofSession *, const char * access_method);
void (*p_qof_session_clear_error) (QofSession *);
void (*p_qof_session_destroy_backend) (QofSession *);
void (*p_qof_session_set_book_id) (QofSession *, char const * book_id);

void
init_static_qofsession_pointers (void)
{
    p_qof_session_load_backend = &qof_session_load_backend;
    p_qof_session_clear_error = &qof_session_clear_error;
    p_qof_session_destroy_backend = &qof_session_destroy_backend;
    p_qof_session_set_book_id = &qof_session_set_book_id;
}

QofBackendError
qof_session_get_error (QofSession * session)
{
    if (!session) return ERR_BACKEND_NO_BACKEND;
    return session->get_error();
}
