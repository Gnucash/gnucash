/********************************************************************\
 * qofsession.cpp -- session access (connection to backend)        *
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
 * @file qofsession.cpp
 * @brief Encapsulate a connection to a storage backend.
 *
 * HISTORY:
 * Created by Linas Vepstas December 1998

 @author Copyright (c) 1998-2004 Linas Vepstas <linas@linas.org>
 @author Copyright (c) 2000 Dave Peticolas
 @author Copyright (c) 2005 Neil Williams <linux@codehelp.co.uk>
 @author Copyright (c) 2016 Aaron Laws
   */
#include <glib.h>

#include <config.h>

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

#include "qof.h"
#include "gnc-session.h"
#include "qofobject-p.h"

static QofLogModule log_module = QOF_MOD_SESSION;

#include "qofbook-p.hpp"
#include "qof-backend.hpp"
#include "qofsession.hpp"
#include "gnc-backend-prov.hpp"
#include "gnc-uri.hpp"

#include <vector>
#include <boost/algorithm/string.hpp>
#include <vector>
#include <algorithm>
#include <atomic>
#include <string>
#include <sstream>
#include <new>

using ProviderVec =  std::vector<QofBackendProvider_ptr>;
static ProviderVec s_providers;
static const std::string empty_string{};

struct QofSessionOperationLease
{
    QofSession *session{};
    QofBook *book{};
    guint64 operation_id{};
    guint64 session_generation{};
    guint64 current_session_generation{};
    QofSessionOperationKind kind{QOF_SESSION_OPERATION_GENERIC};
    bool active{};
};

struct QofSessionAsyncLoad
{
    std::atomic_uint ref_count{1};
    QofSessionImpl *session{};
    QofSessionOperationLease *lease{};
    QofPercentageFunc percentage{};
    QofSessionLoadExecutor executor{};
    QofSessionLoadAsyncCallback callback{};
    gpointer user_data{};
    std::atomic_uintptr_t task_handle{};
    std::atomic_bool cancelled{};
    bool sync_dispatched{};
    std::atomic_bool terminal_dispatched{};
};

static QofSessionAsyncLoad *
qof_session_async_load_ref (QofSessionAsyncLoad *load)
{
    if (load)
        load->ref_count.fetch_add (1, std::memory_order_relaxed);
    return load;
}

static void
qof_session_async_load_unref (QofSessionAsyncLoad *load)
{
    if (load && load->ref_count.fetch_sub (
                    1, std::memory_order_acq_rel) == 1)
        delete load;
}

static gboolean qof_session_async_load_guard (gpointer user_data);
static void qof_session_async_load_complete (gboolean success,
                                             gpointer user_data);
static void qof_session_async_load_legacy_step (void *user_data);
static void qof_session_async_load_legacy_discard (void *user_data);
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

QofSessionImpl::QofSessionImpl (QofBook* book) noexcept
  : m_backend {},
    m_book {book},
    m_uri {},
    m_saving {false},
    m_creating {false},
    m_operation_generation {1},
    m_next_operation_id {1},
    m_operation_lease {},
    m_async_load {},
    m_last_err {},
    m_error_message {}
{
}

QofSessionOperationLease *
QofSessionImpl::acquire_operation_lease (QofSessionOperationKind kind) noexcept
{
    if (m_operation_lease &&
        !operation_lease_is_valid (m_operation_lease))
        invalidate_operation_lease ();

    if (m_operation_lease)
        return nullptr;

    auto lease = new (std::nothrow) QofSessionOperationLease;
    if (!lease)
        return nullptr;

    lease->session = this;
    lease->book = m_book;
    lease->operation_id = m_next_operation_id++;
    if (m_next_operation_id == 0)
        m_next_operation_id = 1;
    lease->session_generation = m_operation_generation;
    lease->current_session_generation =
        gnc_current_session_get_generation ();
    lease->kind = kind;
    lease->active = true;
    m_operation_lease = lease;
    return lease;
}

bool
QofSessionImpl::operation_lease_is_valid (
    const QofSessionOperationLease *lease) const noexcept
{
    return lease && lease->active && lease->session == this &&
           m_operation_lease == lease && lease->book == m_book &&
           lease->session_generation == m_operation_generation &&
           lease->current_session_generation ==
               gnc_current_session_get_generation ();
}

bool
QofSessionImpl::has_active_operation_lease () const noexcept
{
    return operation_lease_is_valid (m_operation_lease);
}

bool
QofSessionImpl::has_active_operation_kind (QofSessionOperationKind kind) const noexcept
{
    return operation_lease_is_valid (m_operation_lease) &&
           m_operation_lease->kind == kind;
}

guint64
QofSessionImpl::operation_generation () const noexcept
{
    return m_operation_generation;
}

void
QofSessionImpl::invalidate_operation_lease () noexcept
{
    m_operation_generation++;
    if (m_operation_generation == 0)
        m_operation_generation = 1;

    auto lease = m_operation_lease;
    m_operation_lease = nullptr;
    if (lease)
    {
        lease->active = false;
        lease->session = nullptr;
        lease->book = nullptr;
    }
}

void
QofSessionImpl::release_operation_lease (
    QofSessionOperationLease *lease) noexcept
{
    if (!lease)
        return;

    if (m_operation_lease == lease)
        invalidate_operation_lease ();
    else
    {
        lease->active = false;
        lease->session = nullptr;
        lease->book = nullptr;
    }
    delete lease;
}

static bool
legacy_operation_allowed (const QofSession *session,
                          const char *operation) noexcept
{
    if (!session)
        return false;
    /* An invalidated lease doesn't mean that its deferred LOAD has finished.
     * The task still holds a pointer to this session until terminality. */
    if (session->has_async_load ())
    {
        PWARN ("Refusing legacy %s while an asynchronous load owns session %p",
               operation, session);
        return false;
    }
    if (!session->has_active_operation_lease ())
        return true;

    PWARN ("Refusing legacy %s while an operation lease owns session %p",
           operation, session);
    return false;
}

QofSessionImpl::~QofSessionImpl () noexcept
{
    ENTER ("sess=%p uri=%s", this, m_uri.c_str ());
    invalidate_operation_lease ();
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
    if (!legacy_operation_allowed (session, "destroy"))
        return;
    delete session;
}

gboolean
qof_session_destroy_with_lease (QofSession *session,
                                QofSessionOperationLease *lease)
{
    if (!session || !session->operation_lease_is_valid (lease))
        return FALSE;

    /* A deferred LOAD owns callback storage that refers to this session.
     * Its terminal callback, not an eager destroy, closes the staging
     * session after source teardown. */
    if (session->has_async_load ())
    {
        (void)session->cancel_active_load ();
        return FALSE;
    }

    session->invalidate_operation_lease ();
    delete session;
    return TRUE;
}

QofSession *
qof_session_new (QofBook* book)
{
    return new QofSessionImpl(book);
}

QofSessionOperationLease *
qof_session_operation_lease_acquire (QofSession *session)
{
    return qof_session_operation_lease_acquire_for (
        session, QOF_SESSION_OPERATION_GENERIC);
}

QofSessionOperationLease *
qof_session_operation_lease_acquire_for (QofSession *session,
                                         QofSessionOperationKind kind)
{
    return session ? session->acquire_operation_lease (kind) : nullptr;
}

gboolean
qof_session_operation_lease_is_valid (
    const QofSessionOperationLease *lease, const QofSession *session)
{
    return session && session->operation_lease_is_valid (lease);
}

guint64
qof_session_operation_lease_get_id (const QofSessionOperationLease *lease)
{
    if (!lease || !lease->session ||
        !lease->session->operation_lease_is_valid (lease))
        return 0;
    return lease->operation_id;
}

QofSessionOperationKind
qof_session_operation_lease_get_kind (const QofSessionOperationLease *lease)
{
    if (!lease || !lease->session ||
        !lease->session->operation_lease_is_valid (lease))
        return QOF_SESSION_OPERATION_GENERIC;
    return lease->kind;
}

void
qof_session_operation_lease_release (QofSessionOperationLease *lease)
{
    if (!lease)
        return;
    if (lease->session)
        lease->session->release_operation_lease (lease);
    else
        delete lease;
}

gboolean
qof_session_has_active_operation_lease (const QofSession *session)
{
    return session && session->has_active_operation_lease ();
}

gboolean
qof_session_has_active_operation_kind (const QofSession *session,
                                       QofSessionOperationKind kind)
{
    return session && session->has_active_operation_kind (kind);
}

guint64
qof_session_get_operation_generation (const QofSession *session)
{
    return session ? session->operation_generation () : 0;
}

void
QofSessionImpl::destroy_backend () noexcept
{
    if (m_backend)
    {
        clear_error ();
        delete m_backend;
        m_backend = nullptr;
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
        // Only do a type check when trying to open an existing file
        // When saving over an existing file the contents of the original file don't matter
        if (!m_creating && !prov->type_check (m_uri.c_str ()))
        {
            PINFO("Provider, %s, reported not being usable for book, %s.",
                    prov->provider_name, m_uri.c_str ());
            continue;
        }
        m_backend = prov->create_backend();
        LEAVE (" ");
        return;
    }
    std::string msg {"failed to get_backend using access method \"" + access_method + "\""};
    push_error (ERR_BACKEND_NO_HANDLER, msg);
    LEAVE (" ");
}

static bool
qof_session_load_error_requires_reset (QofBackendError error) noexcept
{
    return error != ERR_BACKEND_NO_ERR &&
           error != ERR_FILEIO_FILE_TOO_OLD &&
           error != ERR_FILEIO_NO_ENCODING &&
           error != ERR_FILEIO_FILE_UPGRADE &&
           error != ERR_SQL_DB_TOO_OLD &&
           error != ERR_SQL_DB_TOO_NEW;
}

void
QofSessionImpl::load (QofPercentageFunc percentage_func) noexcept
{
    /* We must have an empty book to load into or bad things will happen. */
    g_return_if_fail(m_book && qof_book_empty(m_book));

    if (!m_uri.size ()) return;
    ENTER ("sess=%p uri=%s", this, m_uri.c_str ());

    /* At this point, we should are supposed to have a valid book
     * id and a lock on the file. */
    clear_error ();

    /* This code should be sufficient to initialize *any* backend,
     * whether http, postgres, or anything else that might come along.
     * Basically, the idea is that by now, a backend has already been
     * created & set up.  At this point, we only need to get the
     * top-level account group out of the backend, and that is a
     * generic, backend-independent operation.
     */
    qof_book_set_backend (m_book, m_backend);

    /* Starting the session should result in a bunch of accounts
     * and currencies being downloaded, but probably no transactions;
     * The GUI will need to do a query for that.
     */
    if (m_backend)
    {
        m_backend->set_percentage(percentage_func);
        m_backend->load (m_book, LOAD_TYPE_INITIAL_LOAD);
        push_error (m_backend->get_error(), {});
    }

    auto err = get_error ();
    if (qof_session_load_error_requires_reset (err))
    {
        /* A deferred load's terminal path exclusively owns staging rollback
         * and status classification. The synchronous API retains its legacy
         * eager reset contract. */
        if (m_async_load)
            return;
        destroy_backend();
        invalidate_operation_lease ();
        qof_book_destroy (m_book);
        m_book = qof_book_new();
        LEAVE ("error from backend %d", get_error ());
        return;
    }

    LEAVE ("sess = %p, uri=%s", this, m_uri.c_str ());
}

bool
QofSessionImpl::async_load_is_current (
    const QofSessionAsyncLoad *load) const noexcept
{
    return load && load == m_async_load &&
           !load->cancelled.load (std::memory_order_acquire) &&
           operation_lease_is_valid (load->lease);
}

bool
QofSessionImpl::has_async_load () const noexcept
{
    return m_async_load != nullptr;
}

static void
qof_session_async_load_legacy_step (void *user_data)
{
    auto load = static_cast<QofSessionAsyncLoad *> (user_data);
    if (!load || !load->session)
    {
        qof_session_async_load_unref (load);
        return;
    }
    load->task_handle.store (0, std::memory_order_release);

    auto session = load->session;
    if (!session->async_load_is_current (load))
    {
        session->complete_async_load (load, FALSE);
        qof_session_async_load_unref (load);
        return;
    }

    /* Legacy backends retain their synchronous implementation, but never run
     * it reentrantly from the caller that started the LOAD lease. */
    load->sync_dispatched = true;
    session->load (load->percentage);
    auto success = session->operation_lease_is_valid (load->lease) &&
                   !qof_session_load_error_requires_reset (
                       session->get_error ());
    session->complete_async_load (load, success);
    qof_session_async_load_unref (load);
}

static gboolean
qof_session_async_load_guard (gpointer user_data)
{
    auto load = static_cast<QofSessionAsyncLoad *> (user_data);
    return load && load->session && load->session->async_load_is_current (load);
}

static void
qof_session_async_load_legacy_discard (void *user_data)
{
    auto load = static_cast<QofSessionAsyncLoad *> (user_data);
    if (!load)
        return;
    load->task_handle.store (0, std::memory_order_release);
    if (!load->terminal_dispatched.load (std::memory_order_acquire) &&
        load->session)
        load->session->complete_async_load (load, FALSE);
    qof_session_async_load_unref (load);
}

static void
qof_session_async_load_complete (gboolean success, gpointer user_data)
{
    auto load = static_cast<QofSessionAsyncLoad *> (user_data);
    if (load && load->session)
        load->session->complete_async_load (load, success);
}

bool
QofSessionImpl::start_async_load (QofSessionOperationLease *lease,
                                  QofPercentageFunc percentage_func,
                                  const QofSessionLoadExecutor *executor,
                                  QofSessionLoadAsyncCallback callback,
                                  gpointer user_data) noexcept
{
    if (!lease || !callback || m_async_load || !m_backend || !m_book ||
        !qof_book_empty (m_book) || m_uri.empty () ||
        !operation_lease_is_valid (lease) ||
        lease->kind != QOF_SESSION_OPERATION_LOAD)
        return false;

    auto load = new (std::nothrow) QofSessionAsyncLoad;
    if (!load)
        return false;

    load->session = this;
    load->lease = lease;
    load->percentage = percentage_func;
    if (executor)
        load->executor = *executor;
    load->callback = callback;
    load->user_data = user_data;
    m_async_load = load;

    clear_error ();
    qof_book_set_backend (m_book, m_backend);
    m_backend->set_percentage (percentage_func);
    if (m_backend->load_async (m_book, LOAD_TYPE_INITIAL_LOAD, executor,
                               qof_session_async_load_guard, load,
                               qof_session_async_load_complete, load))
        return true;

    if (!load->executor.schedule || !load->executor.cancel)
    {
        m_async_load = nullptr;
        load->session = nullptr;
        qof_session_async_load_unref (load);
        return false;
    }

    auto task_load = qof_session_async_load_ref (load);
    auto task_handle = load->executor.schedule (
        load->executor.user_data, qof_session_async_load_legacy_step,
        qof_session_async_load_legacy_discard, task_load);
    if (!task_handle)
    {
        qof_session_async_load_unref (task_load);
        m_async_load = nullptr;
        load->session = nullptr;
        qof_session_async_load_unref (load);
        return false;
    }
    load->task_handle.store (task_handle, std::memory_order_release);
    return true;
}

bool
QofSessionImpl::cancel_active_load () noexcept
{
    if (!m_async_load || m_async_load->lease->kind != QOF_SESSION_OPERATION_LOAD)
        return false;

    auto load = m_async_load;
    auto task_handle = load->task_handle.load (std::memory_order_acquire);
    auto executor = load->executor;
    /* Cancellation must still terminalise a generation-stale LOAD. */
    load->cancelled.store (true, std::memory_order_release);
    if (task_handle)
    {
        /* The executor's synchronous discard owns terminal callback dispatch.
         * That callback may destroy this session, so touch no member after
         * cancelling the task. */
        executor.cancel (executor.user_data, task_handle);
        return true;
    }

    /* Native backend cancellation is a separate ownership path and may invoke
     * the terminal callback synchronously. */
    if (m_backend)
        m_backend->cancel_load_async ();
    return true;
}

void
QofSessionImpl::complete_async_load (QofSessionAsyncLoad *load,
                                     gboolean success) noexcept
{
    QofSessionLoadAsyncStatus status;
    QofSessionOperationLease *lease;
    QofSessionLoadAsyncCallback callback;
    gpointer user_data;

    if (!load || load != m_async_load ||
        load->terminal_dispatched.exchange (true, std::memory_order_acq_rel))
        return;

    load->task_handle.store (0, std::memory_order_release);
    m_async_load = nullptr;
    if (load->cancelled.load (std::memory_order_acquire))
    {
        /* A cancelled staging book must never escape as a usable partial
         * result. The token is invalidated before the bound book is gone. */
        destroy_backend ();
        invalidate_operation_lease ();
        qof_book_destroy (m_book);
        m_book = qof_book_new ();
        status = QOF_SESSION_LOAD_CANCELLED;
    }
    else if (!operation_lease_is_valid (load->lease))
    {
        /* A stale staging book has the same publication contract as an
         * explicit cancellation: destroy it before reporting terminality. */
        destroy_backend ();
        qof_book_destroy (m_book);
        m_book = qof_book_new ();
        status = QOF_SESSION_LOAD_STALE;
    }
    else if (!success)
    {
        auto error = m_backend ? m_backend->get_error () : ERR_BACKEND_MISC;
        if (error == ERR_BACKEND_NO_ERR)
            error = ERR_BACKEND_MISC;
        destroy_backend ();
        invalidate_operation_lease ();
        qof_book_destroy (m_book);
        m_book = qof_book_new ();
        if (!load->sync_dispatched)
            push_error (error, {});
        status = QOF_SESSION_LOAD_ERROR;
    }
    else
    {
        auto error = m_backend ? m_backend->get_error () : ERR_BACKEND_NO_ERR;
        if (!load->sync_dispatched && error != ERR_BACKEND_NO_ERR)
            push_error (error, {});
        status = QOF_SESSION_LOAD_COMPLETED;
    }

    lease = load->lease;
    callback = load->callback;
    user_data = load->user_data;
    load->session = nullptr;
    qof_session_operation_lease_release (lease);
    callback (this, status, user_data);
    qof_session_async_load_unref (load);
}

void
QofSessionImpl::begin (const char* new_uri, SessionOpenMode mode) noexcept
{


    ENTER (" sess=%p mode=%d, URI=%s", this, mode, new_uri);
    clear_error ();
    /* Check to see if this session is already open */
    if (m_uri.size ())
    {
        if (ERR_BACKEND_NO_ERR != get_error ())
            push_error (ERR_BACKEND_LOCKED, {});
        LEAVE("push error book is already open ");
        return;
    }

    /* seriously invalid */
    if (!new_uri)
    {
        if (ERR_BACKEND_NO_ERR != get_error ())
            push_error (ERR_BACKEND_BAD_URL, {});
        LEAVE("push error missing new_uri");
        return;
    }

    char * scheme {g_uri_parse_scheme (new_uri)};
    char * filename {nullptr};
    if (g_strcmp0 (scheme, "file") == 0)
    {
        if (auto path = GncUri{new_uri}.path())
            filename = g_strdup (path->c_str());
    }
    else if (!scheme)
        filename = g_strdup (new_uri);

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
    m_uri = new_uri;
    m_creating = mode == SESSION_NEW_STORE || mode == SESSION_NEW_OVERWRITE;
    if (filename)
        load_backend ("file");
    else                       /* access method found, load appropriate backend */
        load_backend (scheme);
    g_free (filename);
    g_free (scheme);

    /* No backend was found. That's bad. */
    if (m_backend == nullptr)
    {
        m_uri = {};
        if (ERR_BACKEND_NO_ERR == get_error ())
            push_error (ERR_BACKEND_BAD_URL, {});
        LEAVE (" BAD: no backend: sess=%p book-id=%s",
               this,  new_uri);
        return;
    }

    /* If there's a begin method, call that. */
    m_backend->session_begin(this, m_uri.c_str(), mode);
    PINFO ("Done running session_begin on backend");
    QofBackendError const err {m_backend->get_error()};
    auto msg (m_backend->get_message());
    if (err != ERR_BACKEND_NO_ERR)
    {
        m_uri = {};
        push_error (err, msg);
        LEAVE (" backend error %d %s", err, msg.empty() ? "(null)" : msg.c_str());
        return;
    }
    if (!msg.empty())
    {
        PWARN("%s", msg.c_str());
    }

    LEAVE (" sess=%p book-id=%s", this,  new_uri);
}

void
QofSessionImpl::end () noexcept
{
    ENTER ("sess=%p uri=%s", this, m_uri.c_str ());
    if (m_backend != nullptr)
        m_backend->session_end();
    clear_error ();
    m_uri.clear();
    LEAVE ("sess=%p uri=%s", this, m_uri.c_str ());
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

const std::string&
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
    if (qof_book_is_open (m_book))
        return m_book;
    return nullptr;
}

QofBackend *
QofSession::get_backend () const noexcept
{
    return m_backend;
}

const std::string&
QofSessionImpl::get_file_path () const noexcept
{
    auto backend = qof_book_get_backend (m_book);
    if (!backend) return empty_string;
    return backend->get_uri();
}

std::string const &
QofSessionImpl::get_uri () const noexcept
{
    return m_uri;
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
    if (!qof_book_session_not_saved (m_book)) //Clean book, nothing to do.
        return;
    m_saving = true;
    ENTER ("sess=%p uri=%s", this, m_uri.c_str ());

    /* If there is a backend, the book is dirty, and the backend is reachable
     * (i.e. we can communicate with it), then synchronize with the backend.  If
     * we cannot contact the backend (e.g.  because we've gone offline, the
     * network has crashed, etc.)  then raise an error so that the controlling
     * dialog can offer the user a chance to save in a different way.
     */
    if (m_backend)
    {
        /* if invoked as SaveAs(), then backend not yet set */
        if (qof_book_get_backend (m_book) != m_backend)
            qof_book_set_backend (m_book, m_backend);
        m_backend->set_percentage(percentage_func);
        m_backend->sync(m_book);
        auto err = m_backend->get_error();
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
        push_error (ERR_BACKEND_NO_HANDLER, "failed to load backend");
        LEAVE("error -- No backend!");
    }
    m_saving = false;
}

void
QofSessionImpl::safe_save (QofPercentageFunc percentage_func) noexcept
{
    if (!(m_backend && m_book)) return;
    if (qof_book_get_backend (m_book) != m_backend)
        qof_book_set_backend (m_book, m_backend);
    m_backend->set_percentage(percentage_func);
    m_backend->safe_sync(get_book ());
    auto err = m_backend->get_error();
    auto msg = m_backend->get_message();
    if (err != ERR_BACKEND_NO_ERR)
    {
        m_uri = "";
        push_error (err, msg);
    }
}

void
QofSessionImpl::ensure_all_data_loaded () noexcept
{
    if (!(m_backend && m_book)) return;
    if (qof_book_get_backend (m_book) != m_backend)
        qof_book_set_backend (m_book, m_backend);
    m_backend->load(m_book, LOAD_TYPE_LOAD_ALL);
    push_error (m_backend->get_error(), {});
}

void
QofSessionImpl::swap_books (QofSessionImpl & other) noexcept
{
    ENTER ("sess1=%p sess2=%p", this, &other);
    // don't swap (that is, double-swap) read_only flags
    if (m_book && other.m_book)
        qof_book_swap_books_readonly (m_book, other.m_book);
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
    ENTER ("tmp_session=%p real_session=%p book=%p uri=%s",
           this, &real_session, real_book, m_uri.c_str ());

    /* There must be a backend or else.  (It should always be the file
     * backend too.)
     */
    if (!m_backend) return false;

    m_backend->set_percentage(percentage_func);

    m_backend->export_coa(real_book);
    auto err = m_backend->get_error();
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
    if (!session) return nullptr;
    auto& path{session->get_file_path()};
    return path.empty() ? nullptr : path.c_str ();
}

void
qof_session_ensure_all_data_loaded (QofSession *session)
{
    if (!legacy_operation_allowed (session, "ensure-all-data-loaded"))
        return;
    session->ensure_all_data_loaded ();
}

gboolean
qof_session_ensure_all_data_loaded_with_lease (
    QofSession *session, const QofSessionOperationLease *lease)
{
    if (!session || !session->operation_lease_is_valid (lease))
        return FALSE;
    session->ensure_all_data_loaded ();
    return TRUE;
}

const char *
qof_session_get_url (const QofSession *session)
{
    if (!session) return NULL;
    return session->get_uri ().c_str ();
}

QofBackend *
qof_session_get_backend (const QofSession *session)
{
    if (!session) return NULL;
    return session->get_backend ();
}

void
qof_session_begin (QofSession *session, const char * uri, SessionOpenMode mode)
{
    if (!legacy_operation_allowed (session, "begin"))
        return;
    session->begin(uri, mode);
}

gboolean
qof_session_begin_with_lease (QofSession *session,
                              const QofSessionOperationLease *lease,
                              const char *uri, SessionOpenMode mode)
{
    if (!session || !session->operation_lease_is_valid (lease))
        return FALSE;
    session->begin (uri, mode);
    return TRUE;
}

void
qof_session_load (QofSession *session,
                  QofPercentageFunc percentage_func)
{
    if (!legacy_operation_allowed (session, "load"))
        return;
    session->load (percentage_func);
}

gboolean
qof_session_load_async_with_lease (
    QofSession *session, QofSessionOperationLease *lease,
    QofPercentageFunc percentage_func, const QofSessionLoadExecutor *executor,
    QofSessionLoadAsyncCallback callback, gpointer user_data)
{
    return session && session->start_async_load (lease, percentage_func,
                                                 executor, callback, user_data);
}

gboolean
qof_session_cancel_active_load (QofSession *session)
{
    return session && session->cancel_active_load ();
}

gboolean
qof_session_load_with_lease (QofSession *session,
                             QofSessionOperationLease *lease,
                             QofPercentageFunc percentage_func)
{
    if (!session || !session->operation_lease_is_valid (lease))
        return FALSE;
    session->load (percentage_func);
    if (!session->operation_lease_is_valid (lease))
        session->invalidate_operation_lease ();
    return TRUE;
}

void
qof_session_save (QofSession *session,
                  QofPercentageFunc percentage_func)
{
    if (!legacy_operation_allowed (session, "save"))
        return;
    session->save (percentage_func);
}

gboolean
qof_session_save_with_lease (QofSession *session,
                             const QofSessionOperationLease *lease,
                             QofPercentageFunc percentage_func)
{
    if (!session || !session->operation_lease_is_valid (lease))
        return FALSE;
    session->save (percentage_func);
    return TRUE;
}

void
qof_session_safe_save(QofSession *session, QofPercentageFunc percentage_func)
{
    if (!legacy_operation_allowed (session, "safe-save"))
        return;
    session->safe_save (percentage_func);
}

gboolean
qof_session_safe_save_with_lease (
    QofSession *session, const QofSessionOperationLease *lease,
    QofPercentageFunc percentage_func)
{
    if (!session || !session->operation_lease_is_valid (lease))
        return FALSE;
    session->safe_save (percentage_func);
    return TRUE;
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
    if (!legacy_operation_allowed (session, "end"))
        return;
    session->end ();
}

gboolean
qof_session_end_with_lease (QofSession *session,
                            const QofSessionOperationLease *lease)
{
    if (!session || !session->operation_lease_is_valid (lease))
        return FALSE;
    session->end ();
    return TRUE;
}

void
qof_session_swap_data (QofSession *session_1, QofSession *session_2)
{
    if (session_1 == session_2) return;
    if (!legacy_operation_allowed (session_1, "swap-data") ||
        !legacy_operation_allowed (session_2, "swap-data"))
        return;
    session_1->swap_books (*session_2);
    session_1->invalidate_operation_lease ();
    session_2->invalidate_operation_lease ();
}

gboolean
qof_session_swap_data_with_leases (
    QofSession *session_1, QofSessionOperationLease *lease_1,
    QofSession *session_2, QofSessionOperationLease *lease_2)
{
    if (!session_1 || !session_2 || session_1 == session_2 ||
        !session_1->operation_lease_is_valid (lease_1) ||
        !session_2->operation_lease_is_valid (lease_2))
        return FALSE;

    session_1->swap_books (*session_2);
    session_1->invalidate_operation_lease ();
    session_2->invalidate_operation_lease ();
    return TRUE;
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
    if (!legacy_operation_allowed (session, "process-events"))
        return FALSE;
    return session->process_events ();
}

gboolean
qof_session_process_events_with_lease (
    QofSession *session, const QofSessionOperationLease *lease,
    gboolean *engine_modified)
{
    if (engine_modified)
        *engine_modified = FALSE;
    if (!session || !engine_modified ||
        !session->operation_lease_is_valid (lease))
        return FALSE;
    *engine_modified = session->process_events ();
    return TRUE;
}

gboolean
qof_session_export (QofSession *tmp_session,
                    QofSession *real_session,
                    QofPercentageFunc percentage_func)
{
    if (!legacy_operation_allowed (tmp_session, "export") ||
        !legacy_operation_allowed (real_session, "export"))
        return FALSE;
    return tmp_session->export_session (*real_session, percentage_func);
}

gboolean
qof_session_export_with_leases (
    QofSession *tmp_session, const QofSessionOperationLease *tmp_lease,
    QofSession *real_session, const QofSessionOperationLease *real_lease,
    QofPercentageFunc percentage_func, gboolean *exported)
{
    if (exported)
        *exported = FALSE;
    if (!tmp_session || !real_session || tmp_session == real_session ||
        !exported ||
        !tmp_session->operation_lease_is_valid (tmp_lease) ||
        !real_session->operation_lease_is_valid (real_lease))
        return FALSE;
    *exported = tmp_session->export_session (*real_session, percentage_func);
    return TRUE;
}

/* ================= Static function access for testing ================= */

void init_static_qofsession_pointers (void);

void qof_session_load_backend (QofSession * session, const char * access_method)
{
    session->load_backend (access_method);
}

static void
qof_session_clear_error (QofSession * session)
{
    session->clear_error ();
}

static void
qof_session_destroy_backend (QofSession * session)
{
    session->destroy_backend ();
}

void qof_session_set_uri (QofSession * session, char const * uri)
{
    if (!uri)
        session->m_uri = "";
    else
        session->m_uri = uri;
}

void (*p_qof_session_load_backend) (QofSession *, const char * access_method);
void (*p_qof_session_clear_error) (QofSession *);
void (*p_qof_session_destroy_backend) (QofSession *);
void (*p_qof_session_set_uri) (QofSession *, char const * uri);

void
init_static_qofsession_pointers (void)
{
    p_qof_session_load_backend = &qof_session_load_backend;
    p_qof_session_clear_error = &qof_session_clear_error;
    p_qof_session_destroy_backend = &qof_session_destroy_backend;
    p_qof_session_set_uri = &qof_session_set_uri;
}

QofBackendError
qof_session_get_error (QofSession * session)
{
    if (!session) return ERR_BACKEND_NO_BACKEND;
    return session->get_error();
}
