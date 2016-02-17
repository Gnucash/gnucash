/********************************************************************
 * test_qofsession.c: GLib g_test test suite for qofsession.        *
 * Copyright 2011 John Ralls <jralls@ceridwen.us>                   *
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

extern "C"
{
#include "config.h"
#include <glib.h>
#include <unittest-support.h>
}

#include "../qof.h"
#include "../qofbackend-p.h"
#include "../qofsession-p.h"
#include "../qofclass-p.h"
#include "../gnc-backend-prov.hpp"
#include <vector>

static const gchar *suitename = "/qof/qofsession";
extern "C" void test_suite_qofsession ( void );

extern void (*p_qof_session_load_backend) (QofSession * session, const char * access_method);
extern void (*p_qof_session_clear_error) (QofSession * session);
extern void (*p_qof_session_destroy_backend) (QofSession * session);

void init_static_qofsession_pointers (void);

using ProviderVec =  std::vector<QofBackendProvider_ptr>;
extern ProviderVec& get_providers (void);
extern bool get_providers_initialized (void);
extern void unregister_all_providers (void);

typedef struct
{
    QofSession *session;
} Fixture;

static void
safe_sync( QofBackend *be, QofBook *book )
{
    qof_backend_set_error( be, ERR_BACKEND_DATA_CORRUPT );
    qof_backend_set_message( be, "Just Kidding!" );
}

static void
percentage_fn ( const char* message, double percent )
{
    g_print( "%s %f complete", message, percent );
}

static void
setup( Fixture *fixture, gconstpointer pData )
{
    fixture->session = qof_session_new();
    init_static_qofsession_pointers ();
    g_assert (p_qof_session_clear_error && p_qof_session_destroy_backend && p_qof_session_load_backend);
}

static void
teardown( Fixture *fixture, gconstpointer pData )
{
    qof_session_destroy( fixture->session );
    p_qof_session_clear_error = NULL;
    p_qof_session_destroy_backend = NULL;
    p_qof_session_load_backend = NULL;
}

static void
test_qof_session_new_destroy (void)
{
    QofSession *session = NULL;
    QofBook *book = NULL;

    g_test_message ("Test session initialization");
    session = qof_session_new ();
    g_assert (session);
    g_assert_cmpstr (session->entity.e_type, == , QOF_ID_SESSION);
    g_assert (session->book);
    book = (QofBook*) session->book;
    g_assert (book);
    g_assert (QOF_IS_BOOK (book));
    g_assert (!session->book_id);
    g_assert (!session->backend);
    g_assert_cmpint (session->lock, == , 1);
    g_assert_cmpint (qof_session_get_error (session), == , ERR_BACKEND_NO_ERR);

    g_test_message ("Test session destroy");
    qof_session_destroy (session);
    /* all data structures of session get deallocated so we can't really test this place
     * instead qof_session_destroy_backend and qof_session_end are tested
     */
}

static void
test_session_safe_save( Fixture *fixture, gconstpointer pData )
{
    fixture->session->backend = g_new0 (QofBackend, 1);
    fixture->session->backend->safe_sync = safe_sync;
    qof_session_safe_save( fixture->session, percentage_fn );
    g_assert_cmpint( ERR_BACKEND_DATA_CORRUPT, == ,
                     qof_session_get_error( fixture->session ));
    g_assert( NULL == qof_session_get_url( fixture->session ));
}

static struct
{
    QofBackend *be;
    bool data_compatible;
    bool check_data_type_called;
    bool backend_new_called;
} load_backend_struct;

struct QofMockLoadBackendProvider : public QofBackendProvider
{
    QofMockLoadBackendProvider(const char *name, const char* type) :
        QofBackendProvider{name, type} {}
    QofBackend* create_backend(void);
    bool type_check(const char* type);
};

bool
QofMockLoadBackendProvider::type_check (const char* book_id)
{
    g_assert (book_id);
    g_assert_cmpstr (book_id, ==, "my book");
    load_backend_struct.check_data_type_called = true;
    return load_backend_struct.data_compatible;
}

QofBackend*
QofMockLoadBackendProvider::create_backend (void)
{
    QofBackend *be = NULL;

    be = g_new0 (QofBackend, 1);
    g_assert (be);
    load_backend_struct.be = be;
    load_backend_struct.backend_new_called = TRUE;
    return be;
}

static void
test_qof_session_load_backend (Fixture *fixture, gconstpointer pData)
{
    QofBook *book = NULL;

    /* init */

    g_test_message ("Test when no provider is registered");
    g_assert (!get_providers_initialized ());
    g_assert (get_providers ().empty());
    p_qof_session_load_backend (fixture->session, "file");
    g_assert (!get_providers_initialized ());
    g_assert_cmpint (qof_session_get_error (fixture->session), == , ERR_BACKEND_NO_HANDLER);
    g_assert_cmpstr (qof_session_get_error_message (fixture->session), == , "Failed to load 'file' using access_method");
    p_qof_session_clear_error (fixture->session);

    g_test_message ("Test with provider registered but access method not supported");
    auto prov = QofBackendProvider_ptr(new QofMockLoadBackendProvider("Mock Backend", "unsupported"));
    qof_backend_register_provider (std::move(prov));
    g_assert (!get_providers().empty());
    g_assert_cmpint (get_providers().size(), == , 1);
    p_qof_session_load_backend (fixture->session, "file");
    g_assert_cmpint (qof_session_get_error (fixture->session), == , ERR_BACKEND_NO_HANDLER);
    g_assert_cmpstr (qof_session_get_error_message (fixture->session), == , "Failed to load 'file' using access_method");
    p_qof_session_clear_error (fixture->session);

    g_test_message ("Test with access method supported but type incompatible");
    prov = QofBackendProvider_ptr(new QofMockLoadBackendProvider("Mock Backend",
                                                                 "file"));
    qof_backend_register_provider (std::move(prov));
    load_backend_struct.data_compatible = FALSE;
    load_backend_struct.check_data_type_called = FALSE;
    fixture->session->book_id = g_strdup ("my book");
    p_qof_session_load_backend (fixture->session, "file");
    g_assert (load_backend_struct.check_data_type_called);
    g_assert_cmpint (qof_session_get_error (fixture->session), == , ERR_BACKEND_NO_HANDLER);
    g_assert_cmpstr (qof_session_get_error_message (fixture->session), == , "Failed to load 'file' using access_method");
    p_qof_session_clear_error (fixture->session);


    g_test_message ("Test with type compatible backend_new set");
    load_backend_struct.be = NULL;
    load_backend_struct.data_compatible = TRUE;
    load_backend_struct.check_data_type_called = FALSE;
    load_backend_struct.backend_new_called = FALSE;
    g_assert (fixture->session->backend == NULL);
    book = qof_session_get_book (fixture->session);
    g_assert (book);
    g_assert (qof_book_get_backend (book) == NULL);
    p_qof_session_load_backend (fixture->session, "file");
    g_assert (load_backend_struct.check_data_type_called);
    g_assert (load_backend_struct.backend_new_called);
    g_assert (load_backend_struct.be);
    g_assert (load_backend_struct.be == fixture->session->backend);
    g_assert (qof_book_get_backend (book) == load_backend_struct.be);
    g_assert_cmpint (qof_session_get_error (fixture->session), == , ERR_BACKEND_NO_ERR);

    unregister_all_providers ();
    g_assert_cmpint (get_providers().size(), == , 0);
}

static struct
{
    QofBackend *be;
    QofBook *oldbook;
    gboolean error;
    gboolean load_called;
} load_session_struct;

static void
mock_load (QofBackend *be, QofBook *book, QofBackendLoadType type)
{
    g_assert (be);
    g_assert (book);
    g_assert (be == load_session_struct.be);
    g_assert (book != load_session_struct.oldbook);
    g_assert (qof_book_get_backend (book) == be);
    if (load_session_struct.error)
        qof_backend_set_error (be, ERR_BACKEND_DATA_CORRUPT); /* just any valid error */
    load_session_struct.load_called = TRUE;
}

static void
test_qof_session_load (Fixture *fixture, gconstpointer pData)
{
    /* Method initializes a new book and loads data into it
     * if load fails old books are restored
     */
    QofBackend *be = NULL;
    QofBook *newbook = NULL;

    /* init */
    fixture->session->book_id = g_strdup ("my book");
    be = g_new0 (QofBackend, 1);
    g_assert (be);
    fixture->session->backend = be;
    be->load = mock_load;

    g_test_message ("Test when no error is produced");
    g_assert (be->percentage == NULL);
    load_session_struct.be = be;
    load_session_struct.oldbook = qof_session_get_book (fixture->session);
    g_assert (fixture->session->book);
    load_session_struct.error = FALSE;
    load_session_struct.load_called = FALSE;
    qof_session_load (fixture->session, percentage_fn);
    newbook = qof_session_get_book (fixture->session);
    g_assert (newbook);
    g_assert (load_session_struct.oldbook != newbook);
    g_assert (fixture->session->book);
    g_assert (load_session_struct.load_called);

    g_test_message ("Test when no is produced");
    load_session_struct.oldbook = qof_session_get_book (fixture->session);
    g_assert (fixture->session->book);
    load_session_struct.error = TRUE;
    load_session_struct.load_called = FALSE;
    qof_session_load (fixture->session, percentage_fn);
    newbook = qof_session_get_book (fixture->session);
    g_assert (newbook);
    g_assert (load_session_struct.oldbook == newbook);
    g_assert (fixture->session->book);
    g_assert (load_session_struct.load_called);
}

static struct
{
    QofBackend *be;
    QofSession *session;
    const char *book_id;
    gboolean backend_new_called;
    gboolean session_begin_called;
    gboolean produce_error;
} session_begin_struct;

static void
mock_session_begin (QofBackend *be, QofSession *session, const char *book_id,
                    gboolean ignore_lock, gboolean create, gboolean force)
{
    g_assert (be);
    g_assert (be == session_begin_struct.be);
    g_assert (session);
    g_assert (session == session_begin_struct.session);
    g_assert (book_id);
    g_assert_cmpstr (book_id, == , session_begin_struct.book_id);
    g_assert (ignore_lock);
    g_assert (!create);
    g_assert (force);
    if (session_begin_struct.produce_error)
    {
        qof_backend_set_error (be, ERR_BACKEND_DATA_CORRUPT);
        qof_backend_set_message (be, "push any error");
    }
    session_begin_struct.session_begin_called = TRUE;
}
struct QofMockSessBackendProvider : public QofBackendProvider
{
    QofMockSessBackendProvider(const char *name, const char* type) :
        QofBackendProvider{name, type} {}
    QofBackend* create_backend(void);
    bool type_check(const char* type);
};

bool
QofMockSessBackendProvider::type_check (const char* book_id)
{
    g_assert (book_id);
    return true;
}

QofBackend*
QofMockSessBackendProvider::create_backend (void)
{
    QofBackend *be = NULL;

    be = g_new0 (QofBackend, 1);
    g_assert (be);
    be->session_begin = mock_session_begin;
    session_begin_struct.be = be;
    session_begin_struct.backend_new_called = TRUE;
    return be;
}

static void
test_qof_session_begin (Fixture *fixture, gconstpointer pData)
{
    gboolean ignore_lock, create, force;
    QofBackend *be = NULL;

    /* setup */
    ignore_lock = TRUE;
    create = FALSE;
    force = TRUE;

    be = g_new0 (QofBackend, 1);
    g_assert (be);
    g_assert_cmpint (get_providers().size(), == , 0);

    /* run tests */
    g_test_message ("Test when book_id is set backend is not changed");
    fixture->session->backend = be;
    fixture->session->book_id = g_strdup ("my book");
    qof_session_begin (fixture->session, "my book", ignore_lock, create, force);
    g_assert (fixture->session->backend == be);

    g_test_message ("Test when session book_id is not set and book_id passed is null backend is not changed");
    g_free (fixture->session->book_id);
    fixture->session->book_id = NULL;
    qof_session_begin (fixture->session, NULL, ignore_lock, create, force);
    g_assert (fixture->session->backend == be);

    g_test_message ("Test default access_method parsing");
    /* routine will destroy old backend
     * parse access_method as 'file' and try to find backend
     * as there is no backend registered error will be raised
     */
    qof_session_begin (fixture->session, "default_should_be_file", ignore_lock, create, force);
    g_assert (fixture->session->backend == NULL);
    g_assert (fixture->session->book_id == NULL);
    g_assert_cmpint (qof_session_get_error (fixture->session), == , ERR_BACKEND_NO_HANDLER);
    g_assert_cmpstr (qof_session_get_error_message (fixture->session), == , "Failed to load 'file' using access_method");

    g_test_message ("Test access_method parsing");
    qof_session_begin (fixture->session, "postgres://localhost:8080", ignore_lock, create, force);
    g_assert (fixture->session->backend == NULL);
    g_assert (fixture->session->book_id == NULL);
    g_assert_cmpint (qof_session_get_error (fixture->session), == , ERR_BACKEND_NO_HANDLER);
    g_assert_cmpstr (qof_session_get_error_message (fixture->session), == , "Failed to load 'postgres' using access_method");

    g_test_message ("Test with valid backend returned and session begin set; error is produced");
    session_begin_struct.session = fixture->session;
    session_begin_struct.book_id = "postgres://localhost:8080";
    session_begin_struct.backend_new_called = FALSE;
    session_begin_struct.session_begin_called = FALSE;
    session_begin_struct.produce_error = TRUE;
    auto prov = QofBackendProvider_ptr(new QofMockSessBackendProvider("Mock Backend",
                                                                  "postgres"));
    qof_backend_register_provider (std::move(prov));

    qof_session_begin (fixture->session, "postgres://localhost:8080", ignore_lock, create, force);
    g_assert (fixture->session->backend);
    g_assert (session_begin_struct.be == fixture->session->backend);
    g_assert (session_begin_struct.backend_new_called == TRUE);
    g_assert (session_begin_struct.session_begin_called == TRUE);
    g_assert (fixture->session->book_id == NULL);
    g_assert_cmpint (qof_session_get_error (fixture->session), == , ERR_BACKEND_DATA_CORRUPT);
    g_assert_cmpstr (qof_session_get_error_message (fixture->session), == , "push any error");

    g_test_message ("Test normal session_begin execution");
    session_begin_struct.backend_new_called = FALSE;
    session_begin_struct.session_begin_called = FALSE;
    session_begin_struct.produce_error = FALSE;
    qof_session_begin (fixture->session, "postgres://localhost:8080", ignore_lock, create, force);
    g_assert (fixture->session->backend);
    g_assert (session_begin_struct.be == fixture->session->backend);
    g_assert (session_begin_struct.backend_new_called == TRUE);
    g_assert (session_begin_struct.session_begin_called == TRUE);
    g_assert (fixture->session->book_id);
    g_assert_cmpstr (fixture->session->book_id, == , "postgres://localhost:8080");
    g_assert_cmpint (qof_session_get_error (fixture->session), == , ERR_BACKEND_NO_ERR);

    unregister_all_providers ();
}

static struct
{
    QofBackend *be;
    QofBook *book;
    QofSession *session;
    const char *book_id;
    gboolean sync_called;
    gboolean backend_new_called;
    gboolean session_begin_called;
} session_save_struct;

static void
mock_sync (QofBackend *be, QofBook *book)
{
    g_assert (be);
    g_assert (book);
    g_assert (be == session_save_struct.be);
    g_assert (book == session_save_struct.book);
    session_save_struct.sync_called = TRUE;
}

static void
test_qof_session_save (Fixture *fixture, gconstpointer pData)
{
    QofBook *book = NULL;
    QofBackend *be = NULL;
    QofBackendProvider *prov = NULL;

    g_test_message ("Test when backend not set");
    g_assert (fixture->session->backend == NULL);
    book = qof_session_get_book (fixture->session);
    g_assert (book);
    qof_session_push_error (fixture->session, ERR_BACKEND_DATA_CORRUPT, "push any error");
    g_assert_cmpint (fixture->session->lock, == , 1);
    qof_session_save (fixture->session, NULL);
    g_assert_cmpint (qof_session_get_error (fixture->session), == , ERR_BACKEND_NO_HANDLER);
    g_assert_cmpstr (qof_session_get_error_message (fixture->session), == , "failed to load backend");
    g_assert_cmpint (fixture->session->lock, == , 1);

    g_test_message ("Test when backend set; imitate error");
    be = g_new0 (QofBackend, 1);
    g_assert (be);
    be->sync = mock_sync;
    fixture->session->backend = be;
    g_assert_cmpint (fixture->session->lock, == , 1);
    session_save_struct.sync_called = FALSE;
    session_save_struct.be = be;
    session_save_struct.book = book;
    qof_backend_set_error (be, ERR_BACKEND_DATA_CORRUPT);
    qof_backend_set_message (be, "push any error");
    qof_session_save (fixture->session, percentage_fn);
    g_assert (qof_book_get_backend (book) == be);
    g_assert (be->percentage == percentage_fn);
    g_assert (session_save_struct.sync_called);
    g_assert_cmpint (fixture->session->lock, == , 1);
    g_assert_cmpint (qof_session_get_error (fixture->session), == , ERR_BACKEND_DATA_CORRUPT);
    g_assert_cmpstr (qof_session_get_error_message (fixture->session), == , "");

    g_test_message ("Test when backend set; successful save");
    g_assert_cmpint (fixture->session->lock, == , 1);
    session_save_struct.sync_called = FALSE;
    qof_session_save (fixture->session, percentage_fn);
    g_assert (qof_book_get_backend (book) == be);
    g_assert (be->percentage == percentage_fn);
    g_assert (session_save_struct.sync_called);
    g_assert_cmpint (fixture->session->lock, == , 1);
    g_assert_cmpint (qof_session_get_error (fixture->session), == , ERR_BACKEND_NO_ERR);

    /* change backend testing
     * code probably should be moved to separate routine or some existing code can be reused
     * for example: qof_session_load_backend
     */

    unregister_all_providers ();
    g_free (prov);
}

static struct
{
    QofBackend *be;
    gboolean called;
} destroy_backend_struct;

static void
mock_destroy_backend (QofBackend *be)
{
    g_assert (be);
    g_assert (destroy_backend_struct.be == be);
    destroy_backend_struct.called = TRUE;
}

static void
test_qof_session_destroy_backend (Fixture *fixture, gconstpointer pData)
{
    QofBackend *be = NULL;

    g_test_message ("Test with destroy backend callback not set");
    be = g_new0 (QofBackend, 1);
    g_assert (be);
    fixture->session->backend = be;
    p_qof_session_destroy_backend (fixture->session);
    g_assert (!fixture->session->backend);

    g_test_message ("Test with destroy backend callback set");
    be = g_new0 (QofBackend, 1);
    g_assert (be);
    be->destroy_backend = mock_destroy_backend;
    fixture->session->backend = be;
    destroy_backend_struct.called = FALSE;
    destroy_backend_struct.be = be;
    p_qof_session_destroy_backend (fixture->session);
    g_assert (!fixture->session->backend);
    g_assert (destroy_backend_struct.called);
}

static struct
{
    QofBackend *be;
    gboolean called;
} session_end_struct;

static void
mock_session_end (QofBackend *be)
{
    g_assert (be);
    g_assert (session_end_struct.be == be);
    session_end_struct.called = TRUE;
}

static void
test_qof_session_end (Fixture *fixture, gconstpointer pData)
{
    QofBackend *be = NULL;

    g_test_message ("Test backend is closed, errors cleared and book_id removed");
    be = g_new0 (QofBackend, 1);
    g_assert (be);
    be->session_end = mock_session_end;
    fixture->session->backend = be;
    qof_session_push_error (fixture->session, ERR_BACKEND_DATA_CORRUPT, "push any error");
    fixture->session->book_id = g_strdup ("my book");
    session_end_struct.called = FALSE;
    session_end_struct.be = be;
    qof_session_end (fixture->session);
    g_assert (session_end_struct.called);
    g_assert_cmpint (qof_session_get_error (fixture->session), == , ERR_BACKEND_NO_ERR);
    g_assert (!fixture->session->book_id);
}

static struct
{
    QofBackend *be;
    QofBook *book;
    gboolean called;
} session_export_struct;

static void
mock_export (QofBackend *be, QofBook *book)
{
    g_assert (be);
    g_assert (session_export_struct.be == be);
    g_assert (book);
    g_assert (session_export_struct.book == book);
    session_export_struct.called = TRUE;
}

static void
test_qof_session_export (Fixture *fixture, gconstpointer pData)
{
    QofSession *real_session = NULL;
    QofBook *tmp_book = NULL, *real_book = NULL;
    QofBackend *be = NULL;

    real_session = qof_session_new ();
    g_assert (real_session);

    g_test_message ("Test null checks");
    g_assert (!qof_session_export (NULL, real_session, percentage_fn));
    g_assert (!qof_session_export (fixture->session, NULL, percentage_fn));

    g_test_message ("Test with backend not set");
    tmp_book = qof_session_get_book (fixture->session);
    g_assert (tmp_book);
    be = qof_book_get_backend (tmp_book);
    g_assert (!be);
    g_assert (!qof_session_export (fixture->session, real_session, percentage_fn));

    g_test_message ("Test with backend set");
    be = g_new0 (QofBackend, 1);
    g_assert (be);
    fixture->session->backend = be;
    qof_book_set_backend (tmp_book, be);
    g_assert (!be->percentage);
    g_assert (qof_session_export (fixture->session, real_session, percentage_fn));
    g_assert (be->percentage == percentage_fn);

    g_test_message ("Test with backend export function set and error is produced");
    be->export_fn = mock_export;
    qof_backend_set_error (be, ERR_BACKEND_DATA_CORRUPT);
    qof_backend_set_message (be, "push any error");
    session_export_struct.called = FALSE;
    real_book = qof_session_get_book (real_session);
    g_assert (real_book);
    session_export_struct.be = be;
    session_export_struct.book = real_book;
    g_assert (!qof_session_export (fixture->session, real_session, percentage_fn));
    g_assert (session_export_struct.called);

    g_test_message ("Test with backend export function set and no error produced");
    p_qof_session_clear_error (fixture->session);
    session_export_struct.called = FALSE;
    g_assert (qof_session_export (fixture->session, real_session, percentage_fn));
    g_assert (session_export_struct.called);

    qof_session_destroy (real_session);
}

static void
test_qof_session_swap_data (Fixture *fixture, gconstpointer pData)
{
    QofSession *session2 = NULL;
    QofBackend *be1 = NULL, *be2 = NULL;
    QofBook *book1 = NULL, *book2 = NULL;

    /* init */
    g_assert (fixture->session);
    session2 = qof_session_new ();
    g_assert (session2);
    g_assert (fixture->session != session2);
    be1 = g_new0 (QofBackend, 1);
    g_assert (be1);
    be2 = g_new0 (QofBackend, 1);
    g_assert (be2);
    fixture->session->backend = be1;
    session2->backend = be2;
    book1 = fixture->session->book;
    book2 = session2->book;
    g_assert (book1);
    g_assert (book2);
    qof_book_set_backend (book1, fixture->session->backend);
    qof_book_set_backend (book2, session2->backend);


    g_test_message ("Test book lists are swapped and backend for each book is swapped");
    qof_session_swap_data (fixture->session, session2);
    g_assert (fixture->session->book == book2);
    g_assert (session2->book == book1);

    qof_session_destroy (session2);
}

static struct
{
    QofBackend *be;
    gboolean called;
} events_struct;

static gboolean
mock_events_fn (QofBackend *be)
{
    g_assert (be);
    g_assert (be == events_struct.be);
    events_struct.called = TRUE;
    return TRUE;
}

static void
test_qof_session_events (Fixture *fixture, gconstpointer pData)
{
    QofBackend *be = NULL;

    g_test_message ("Test pending events null checks");
    g_assert (!qof_session_events_pending (NULL));
    g_assert (!fixture->session->backend);
    g_assert (!qof_session_events_pending (fixture->session));
    be = g_new0 (QofBackend, 1);
    g_assert (be);
    be->events_pending = NULL;
    fixture->session->backend = be;
    g_assert (!qof_session_events_pending (fixture->session));

    g_test_message ("Test pending events callback");
    be->events_pending = mock_events_fn;
    events_struct.called = FALSE;
    events_struct.be = be;
    g_assert (qof_session_events_pending (fixture->session));
    g_assert (events_struct.called);

    g_test_message ("Test process events null checks");
    g_assert (!qof_session_process_events (NULL));
    fixture->session->backend = NULL;
    g_assert (!qof_session_process_events (fixture->session));
    be->process_events = NULL;
    fixture->session->backend = be;
    g_assert (!qof_session_process_events (fixture->session));

    g_test_message ("Test process events callback");
    be->process_events = mock_events_fn;
    events_struct.called = FALSE;
    events_struct.be = be;
    g_assert (qof_session_process_events (fixture->session));
    g_assert (events_struct.called);
}

static struct
{
    QofBackend *be;
    QofBook *book;
    gboolean called;
} data_load_struct;

static void
mock_all_data_load (QofBackend *be, QofBook *book, QofBackendLoadType type)
{
    g_assert (be);
    g_assert (book);
    g_assert (be == data_load_struct.be);
    g_assert (book == data_load_struct.book);
    g_assert_cmpint (type, == , LOAD_TYPE_LOAD_ALL);
    qof_backend_set_error (be, ERR_BACKEND_DATA_CORRUPT);
    data_load_struct.called = TRUE;
}

static void
test_qof_session_data_loaded (Fixture *fixture, gconstpointer pData)
{
    QofBackend *be = NULL;

    be = g_new0 (QofBackend, 1);
    g_assert (be);
    be->load = mock_all_data_load;
    fixture->session->backend = be;

    g_test_message ("Test load callback and artificial error");
    data_load_struct.be = be;
    data_load_struct.book = qof_session_get_book (fixture->session);
    data_load_struct.called = FALSE;
    qof_session_ensure_all_data_loaded (fixture->session);
    g_assert (data_load_struct.called);
    g_assert_cmpint (qof_session_get_error (fixture->session), == , ERR_BACKEND_DATA_CORRUPT);
    g_assert_cmpstr (qof_session_get_error_message (fixture->session), == , "");
}

static void
test_qof_session_get_book (Fixture *fixture, gconstpointer pData)
{
    QofBook *book = NULL;

    g_test_message ("Test null check");
    g_assert (!qof_session_get_book (NULL));

    g_test_message ("Test open book is returned");
    g_assert (fixture->session->book);
    book = qof_session_get_book (fixture->session);
    g_assert (book);
    g_assert_cmpuint (book->book_open, == , 'y');

    g_test_message ("Test when book is closed null returned");
    qof_book_mark_closed (book);
    g_assert (!qof_session_get_book (fixture->session));

}

static void
test_qof_session_get_error (Fixture *fixture, gconstpointer pData)
{
    QofBackend *be = NULL;

    g_test_message ("Test if session is null");
    g_assert_cmpint (qof_session_get_error (NULL), == , ERR_BACKEND_NO_BACKEND);

    g_test_message ("Test when there is a local error");
    fixture->session->last_err = ERR_BACKEND_DATA_CORRUPT; /* just any error */
    g_assert_cmpint (qof_session_get_error (fixture->session), == , ERR_BACKEND_DATA_CORRUPT);

    g_test_message ("Test if session backend is null");
    g_assert (!fixture->session->backend);
    fixture->session->last_err = ERR_BACKEND_NO_ERR;
    g_assert_cmpint (qof_session_get_error (fixture->session), == , ERR_BACKEND_NO_ERR);

    g_test_message ("Test for backend error");
    be = g_new0 (QofBackend, 1);
    g_assert (be);
    qof_backend_set_error (be, ERR_BACKEND_CANT_CONNECT);
    fixture->session->backend = be;
    g_assert_cmpint (qof_session_get_error (fixture->session), == , ERR_BACKEND_CANT_CONNECT);
}

static void
test_qof_session_clear_error (Fixture *fixture, gconstpointer pData)
{
    QofBackend *be = NULL;

    be = g_new0 (QofBackend, 1);
    g_assert (be);

    g_test_message ("Test session and backend errors are cleared");
    qof_session_push_error (fixture->session, ERR_BACKEND_NO_SUCH_DB, "push any error");
    fixture->session->backend = be;
    qof_backend_set_error (be, ERR_BACKEND_CANT_CONNECT);
    p_qof_session_clear_error (fixture->session);
    g_assert_cmpint (qof_session_get_error (fixture->session), == , ERR_BACKEND_NO_ERR);
    g_assert_cmpstr (qof_session_get_error_message (fixture->session), == , "");
    g_assert (!fixture->session->error_message);
    g_assert_cmpint (qof_backend_get_error (be), == , ERR_BACKEND_NO_ERR);
}

static struct
{
    QofSession *session;
    gpointer data1;
    gpointer data2;
    gpointer data3;
    guint call_count;
} hooks_struct;

static void
mock_hook_fn (gpointer data, gpointer user_data)
{
    QofSession *session;

    g_assert (data);
    g_assert (user_data);
    session = (QofSession*) data;
    g_assert (session == hooks_struct.session);
    if (hooks_struct.call_count == 0)
        g_assert (hooks_struct.data1 == user_data);
    if (hooks_struct.call_count == 1)
        g_assert (hooks_struct.data2 == user_data);
    if (hooks_struct.call_count == 2)
        g_assert (hooks_struct.data3 == user_data);
    hooks_struct.call_count++;
}

void
test_suite_qofsession ( void )
{
    GNC_TEST_ADD_FUNC (suitename, "qof session new and destroy", test_qof_session_new_destroy);
    GNC_TEST_ADD (suitename, "qof session safe save", Fixture, NULL, setup, test_session_safe_save, teardown);
    GNC_TEST_ADD (suitename, "qof session load backend", Fixture, NULL, setup, test_qof_session_load_backend, teardown);
    GNC_TEST_ADD (suitename, "qof session load", Fixture, NULL, setup, test_qof_session_load, teardown);
    GNC_TEST_ADD (suitename, "qof session begin", Fixture, NULL, setup, test_qof_session_begin, teardown);
    GNC_TEST_ADD (suitename, "qof session save", Fixture, NULL, setup, test_qof_session_save, teardown);
    GNC_TEST_ADD (suitename, "qof session destroy backend", Fixture, NULL, setup, test_qof_session_destroy_backend, teardown);
    GNC_TEST_ADD (suitename, "qof session end", Fixture, NULL, setup, test_qof_session_end, teardown);
    GNC_TEST_ADD (suitename, "qof session export", Fixture, NULL, setup, test_qof_session_export, teardown);
    GNC_TEST_ADD (suitename, "qof session swap data", Fixture, NULL, setup, test_qof_session_swap_data, teardown);
    GNC_TEST_ADD (suitename, "qof session events", Fixture, NULL, setup, test_qof_session_events, teardown);
    GNC_TEST_ADD (suitename, "qof session data loaded", Fixture, NULL, setup, test_qof_session_data_loaded, teardown);
    GNC_TEST_ADD (suitename, "qof session get book", Fixture, NULL, setup, test_qof_session_get_book, teardown);
    GNC_TEST_ADD (suitename, "qof session get error", Fixture, NULL, setup, test_qof_session_get_error, teardown);
    GNC_TEST_ADD (suitename, "qof session clear error", Fixture, NULL, setup, test_qof_session_clear_error, teardown);
}
