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

#include "config.h"
#include <glib.h>
#include <unittest-support.h>
#include <stdlib.h>

#include "../qof.h"
#include "../qofbackend-p.h"
#include "../qofclass-p.h"

static const gchar *suitename = "/qof/qofsession";
void test_suite_qofsession ( void );

static struct
{
    QofBackend *be;
    QofSession *session;
    const char *book_id;
    gboolean backend_new_called;
    gboolean session_begin_called;
    QofBackendError  error;
} session_begin_struct;

static struct
{
    QofBackend *be;
    QofBook *oldbook;
    gboolean error;
    gboolean load_called;
} load_session_struct;

typedef struct
{
    QofSession *session;
    QofBackendProvider * provider;
} Fixture;

static void
mock_session_begin (QofBackend *be, QofSession *session, const char *book_id,
                    gboolean ignore_lock, gboolean create, gboolean force)
{
    qof_backend_set_error (be, session_begin_struct.error);
    session_begin_struct.session_begin_called = TRUE;
}

static void
mock_load (QofBackend *be, QofBook *book, QofBackendLoadType type)
{
    g_assert (be);
    g_assert (book);
    g_assert (qof_book_get_backend (book) == be);
    if (load_session_struct.error)
        qof_backend_set_error (be, ERR_BACKEND_DATA_CORRUPT);
    load_session_struct.load_called = TRUE;
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
safe_sync (QofBackend *be, QofBook *book)
{
    qof_backend_set_error (be, ERR_BACKEND_DATA_CORRUPT);
    qof_backend_set_message (be, "Just Kidding!");
}

static void
percentage_fn ( const char* message, double percent )
{
    g_print( "%s %f complete", message, percent );
}

static QofBackend * last_backend_created = NULL;
static QofBackendProvider * last_provider_created = NULL;

static QofBackend *
test_backend_new (void)
{
    QofBackend * ret = (QofBackend*) malloc (sizeof (QofBackend));
    ret->session_begin = &mock_session_begin;
    ret->session_end = NULL;
    ret->destroy_backend = NULL;
    ret->load = &mock_load;
    ret->begin = NULL;
    ret->commit = NULL;
    ret->rollback = NULL;
    ret->compile_query = NULL;
    ret->free_query = NULL;
    ret->run_query = NULL;
    ret->sync = &mock_sync;
    ret->safe_sync = &safe_sync;
    ret->events_pending = NULL;
    ret->process_events = NULL;
    ret->percentage = NULL;
    ret->provider = last_provider_created;
    ret->last_err = ERR_BACKEND_NO_ERR;
    ret->error_msg = NULL;
    ret->config_count = 0;
    ret->fullpath = NULL;
    ret->price_lookup = NULL;
    ret->export_fn = NULL;
    last_backend_created = ret;
    session_begin_struct.backend_new_called = TRUE;
    return ret;
}

static QofBackendProvider *
create_backend_provider (void)
{
    QofBackendProvider* prov = (QofBackendProvider*) malloc (sizeof (QofBackendProvider));
    prov->provider_name = NULL;
    prov->access_method = "file";
    prov->backend_new = &test_backend_new;
    prov->check_data_type = NULL;
    prov->provider_free = NULL;
    last_provider_created = prov;
    return prov;
}

static void
setup (Fixture *fixture, gconstpointer pData)
{
    fixture->session = qof_session_new ();
    fixture->provider = create_backend_provider ();
    qof_backend_register_provider (fixture->provider);
}

static void
teardown (Fixture *fixture, gconstpointer pData)
{
    qof_session_destroy (fixture->session);
    qof_backend_unregister_all_providers ();
}

static void
test_qof_session_new_destroy (void)
{
    QofSession *session = NULL;
    QofBook *book = NULL;

    g_test_message ("Test session initialization");
    session = qof_session_new ();
    g_assert (session);
    book = qof_session_get_book(session);
    g_assert (book);
    g_assert (QOF_IS_BOOK (book));
    g_assert_cmpint (qof_session_get_error (session), == , ERR_BACKEND_NO_ERR);

    g_test_message ("Test session destroy");
    qof_session_destroy (session);
    /* all data structures of session get deallocated so we can't really test this place
     * instead qof_session_destroy_backend and qof_session_end are tested
     */
}

static void
test_qof_session_begin (Fixture *fixture, gconstpointer pData)
{
    gboolean ignore_lock, create, force;
    QofBackend *be = NULL;
    QofBackendProvider *prov = NULL;

    ignore_lock = TRUE;
    create = FALSE;
    force = TRUE;

    be = qof_session_get_backend (fixture->session);
    qof_session_begin (fixture->session, NULL, ignore_lock, create, force);
    g_assert (qof_session_get_backend (fixture->session) == be);

    qof_backend_unregister_all_providers ();
    qof_session_begin (fixture->session, "default_should_be_file", ignore_lock, create, force);
    g_assert_cmpint (qof_session_get_error (fixture->session), == , ERR_BACKEND_NO_HANDLER);

    g_test_message ("Test access_method parsing");
    qof_session_begin (fixture->session, "postgres://localhost:8080", ignore_lock, create, force);
    g_assert_cmpint (qof_session_get_error (fixture->session), == , ERR_BACKEND_NO_HANDLER);

    g_test_message ("Test with valid backend returned and session begin set; error is produced");
    /* Unregistering frees the backend providers. We need to create a new one now.*/
    fixture->provider = create_backend_provider ();
    session_begin_struct.session = fixture->session;
    session_begin_struct.backend_new_called = FALSE;
    session_begin_struct.session_begin_called = FALSE;
    session_begin_struct.error = ERR_BACKEND_DATA_CORRUPT;
    fixture->provider->access_method = "postgres";
    qof_backend_register_provider (fixture->provider);
    qof_session_begin (fixture->session, "postgres://localhost:8080", ignore_lock, create, force);
    g_assert (qof_session_get_backend (fixture->session) == last_backend_created);
    g_assert (session_begin_struct.backend_new_called == TRUE);
    g_assert (session_begin_struct.session_begin_called == TRUE);
    g_assert_cmpint (qof_session_get_error (fixture->session), == , ERR_BACKEND_DATA_CORRUPT);

    g_test_message ("Test normal session_begin execution");
    session_begin_struct.backend_new_called = FALSE;
    session_begin_struct.session_begin_called = FALSE;
    session_begin_struct.error = ERR_BACKEND_NO_ERR;
    qof_session_begin (fixture->session, "postgres://localhost:8080", ignore_lock, create, force);
    g_assert (qof_session_get_backend (fixture->session) == last_backend_created);
    g_assert (session_begin_struct.backend_new_called == TRUE);
    g_assert (session_begin_struct.session_begin_called == TRUE);
    g_assert (qof_session_get_book (fixture->session));
    g_assert_cmpstr (qof_session_get_url (fixture->session), == , "postgres://localhost:8080");
    g_assert_cmpint (qof_session_get_error (fixture->session), == , ERR_BACKEND_NO_ERR);
}

static void
test_qof_session_save (Fixture *fixture, gconstpointer pData)
{
    g_test_message ("Test when backend set; imitate error");
    qof_session_begin (fixture->session, "file", FALSE, FALSE, FALSE);
    g_assert_cmpint (qof_session_save_in_progress (fixture->session), == , FALSE);
    session_save_struct.sync_called = FALSE;
    session_save_struct.be = last_backend_created;
    session_save_struct.book = qof_session_get_book (fixture->session);
    qof_backend_set_error (last_backend_created, ERR_BACKEND_DATA_CORRUPT);
    qof_backend_set_message (last_backend_created, "push any error");
    qof_session_save (fixture->session, percentage_fn);
    g_assert (qof_book_get_backend (qof_session_get_book (fixture->session)) == last_backend_created);
    g_assert (last_backend_created->percentage == percentage_fn);
    g_assert (session_save_struct.sync_called);
    g_assert_cmpint (qof_session_save_in_progress (fixture->session), == , FALSE);
    g_assert_cmpint (qof_session_get_error (fixture->session), == , ERR_BACKEND_DATA_CORRUPT);

    g_test_message ("Test when backend set; successful save");
    g_assert_cmpint (qof_session_save_in_progress (fixture->session), == , FALSE);
    session_save_struct.sync_called = FALSE;
    qof_session_save (fixture->session, percentage_fn);
    g_assert (qof_book_get_backend (qof_session_get_book (fixture->session)) == last_backend_created);
    g_assert (last_backend_created->percentage == percentage_fn);
    g_assert (session_save_struct.sync_called);
    g_assert_cmpint (qof_session_save_in_progress (fixture->session), == , FALSE);
    g_assert_cmpint (qof_session_get_error (fixture->session), == , ERR_BACKEND_NO_ERR);
}

/*
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
    qof_session_set_backend (fixture->session, be);
    p_qof_session_destroy_backend (fixture->session);
    g_assert (!qof_session_get_backend (fixture->session));

    g_test_message ("Test with destroy backend callback set");
    be = g_new0 (QofBackend, 1);
    g_assert (be);
    be->destroy_backend = mock_destroy_backend;
    qof_session_set_backend (fixture->session, be);
    destroy_backend_struct.called = FALSE;
    destroy_backend_struct.be = be;
    p_qof_session_destroy_backend (fixture->session);
    g_assert (!qof_session_get_backend (fixture->session));
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
    qof_session_set_backend (fixture->session, be);
    qof_session_push_error (fixture->session, ERR_BACKEND_DATA_CORRUPT, "push any error");
    qof_session_set_book_id (fixture->session, g_strdup ("my book"));
    session_end_struct.called = FALSE;
    session_end_struct.be = be;
    qof_session_end (fixture->session);
    g_assert (session_end_struct.called);
    g_assert_cmpint (qof_session_get_error (fixture->session), == , ERR_BACKEND_NO_ERR);
    g_assert (!qof_session_get_book_id (fixture->session));
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
    qof_session_set_backend (fixture->session, be);
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

    g_assert (fixture->session);
    session2 = qof_session_new ();
    g_assert (session2);
    g_assert (fixture->session != session2);
    be1 = g_new0 (QofBackend, 1);
    g_assert (be1);
    be2 = g_new0 (QofBackend, 1);
    g_assert (be2);
    qof_session_set_backend (fixture->session, be1);
    qof_session_set_backend (session2, be2);
    book1 = qof_session_get_book (fixture->session);
    book2 = qof_session_get_book (session2);
    g_assert (book1);
    g_assert (book2);
    qof_book_set_backend (book1, qof_session_get_backend (fixture->session));
    qof_book_set_backend (book2, qof_session_get_backend (session2));


    g_test_message ("Test book lists are swapped and backend for each book is swapped");
    qof_session_swap_data (fixture->session, session2);
    g_assert (qof_session_get_book (fixture->session) == book2);
    g_assert (qof_session_get_book (session2) == book1);

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
    g_assert (!qof_session_get_backend (fixture->session));
    g_assert (!qof_session_events_pending (fixture->session));
    be = g_new0 (QofBackend, 1);
    g_assert (be);
    be->events_pending = NULL;
    qof_session_set_backend (fixture->session, be);
    g_assert (!qof_session_events_pending (fixture->session));

    g_test_message ("Test pending events callback");
    be->events_pending = mock_events_fn;
    events_struct.called = FALSE;
    events_struct.be = be;
    g_assert (qof_session_events_pending (fixture->session));
    g_assert (events_struct.called);

    g_test_message ("Test process events null checks");
    g_assert (!qof_session_process_events (NULL));
    qof_session_set_backend (fixture->session, NULL);
    g_assert (!qof_session_process_events (fixture->session));
    be->process_events = NULL;
    qof_session_set_backend (fixture->session, be);
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
    qof_session_set_backend (fixture->session, be);

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
test_qof_backend_get_access_method_list (Fixture *fixture, gconstpointer pData)
{
    GList *list = NULL;
    const char *access_methods[4] = { "file", "http", "postgres", "sqlite" };
    int i;

    for (i = 0; i < 4; i++)
    {
        QofBackendProvider *prov = g_new0 (QofBackendProvider, 1);
        g_assert (prov);
        prov->access_method = access_methods[ i ];
        qof_backend_register_provider (prov);
        g_assert_cmpint (g_slist_length (get_provider_list ()), == , (i + 1));
    }
    g_assert_cmpint (g_slist_length (get_provider_list ()), == , 4);

    g_test_message ("Test list of access methods is returned");
    list = qof_backend_get_registered_access_method_list ();
    g_assert (list);
    g_assert_cmpint (g_list_length (list), == , 4);
    g_assert_cmpstr (g_list_nth_data (list, 0), == , "file");
    g_assert_cmpstr (g_list_nth_data (list, 1), == , "http");
    g_assert_cmpstr (g_list_nth_data (list, 2), == , "postgres");
    g_assert_cmpstr (g_list_nth_data (list, 3), == , "sqlite");

    g_list_free (list);
    unregister_all_providers ();
}


static void
test_qof_session_get_book (Fixture *fixture, gconstpointer pData)
{
    QofBook *book = NULL;

    g_test_message ("Test null check");
    g_assert (!qof_session_get_book (NULL));

    g_test_message ("Test open book is returned");
    g_assert (qof_session_get_book (fixture->session));
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
    qof_session_push_error (fixture->session, ERR_BACKEND_DATA_CORRUPT, "oops");
    g_assert_cmpint (qof_session_get_error (fixture->session), == , ERR_BACKEND_DATA_CORRUPT);

    g_test_message ("Test if session backend is null");
    g_assert (!qof_session_get_backend (fixture->session));
    qof_session_push_error (fixture->session, ERR_BACKEND_NO_ERR, "oops");
    g_assert_cmpint (qof_session_get_error (fixture->session), == , ERR_BACKEND_NO_ERR);

    g_test_message ("Test for backend error");
    be = g_new0 (QofBackend, 1);
    g_assert (be);
    qof_backend_set_error (be, ERR_BACKEND_CANT_CONNECT);
    qof_session_set_backend (fixture->session, be);
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
    qof_session_set_backend (fixture->session, be);
    qof_backend_set_error (be, ERR_BACKEND_CANT_CONNECT);
    p_qof_session_clear_error (fixture->session);
    g_assert_cmpint (qof_session_get_error (fixture->session), == , ERR_BACKEND_NO_ERR);
    g_assert_cmpstr (qof_session_get_error_message (fixture->session), == , "");
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
*/
void
test_suite_qofsession ( void )
{
    GNC_TEST_ADD_FUNC (suitename, "qof session new and destroy", test_qof_session_new_destroy);

    GNC_TEST_ADD (suitename, "qof session begin", Fixture, NULL, setup, test_qof_session_begin, teardown);
    GNC_TEST_ADD (suitename, "qof session save", Fixture, NULL, setup, test_qof_session_save, teardown);
    /*
    GNC_TEST_ADD (suitename, "qof session destroy backend", Fixture, NULL, setup, test_qof_session_destroy_backend, teardown);
    GNC_TEST_ADD (suitename, "qof session end", Fixture, NULL, setup, test_qof_session_end, teardown);
    GNC_TEST_ADD (suitename, "qof session export", Fixture, NULL, setup, test_qof_session_export, teardown);
    GNC_TEST_ADD (suitename, "qof session swap data", Fixture, NULL, setup, test_qof_session_swap_data, teardown);
    GNC_TEST_ADD (suitename, "qof session events", Fixture, NULL, setup, test_qof_session_events, teardown);
    GNC_TEST_ADD (suitename, "qof session data loaded", Fixture, NULL, setup, test_qof_session_data_loaded, teardown);
    GNC_TEST_ADD (suitename, "qof backend access method list", Fixture, NULL, setup, test_qof_backend_get_access_method_list, teardown);
    GNC_TEST_ADD (suitename, "qof session get book", Fixture, NULL, setup, test_qof_session_get_book, teardown);
    GNC_TEST_ADD (suitename, "qof session get error", Fixture, NULL, setup, test_qof_session_get_error, teardown);
    GNC_TEST_ADD (suitename, "qof session clear error", Fixture, NULL, setup, test_qof_session_clear_error, teardown);
    */
}
