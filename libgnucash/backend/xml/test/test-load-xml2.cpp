/***************************************************************************
 *            test-load-xml2.c
 *
 *  Fri Oct  7 20:51:46 2005
 *  Copyright  2005  Neil Williams
 *  linux@codehelp.co.uk
 ****************************************************************************/
/*
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301, USA.
 */

/* @file test-load-xml2.c
 * @brief test the loading of a version-2 gnucash XML file
 */
#include <glib.h>
#include <glib-object.h>
#include <glib/gstdio.h>

#include <config.h>
#include <stdlib.h>
#include <algorithm>
#include <deque>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dirent.h>
#include <string.h>

#include <cashobjects.h>
#include <TransLog.h>
#include <gnc-engine.h>
#include <gnc-session.h>
#include <gnc-prefs.h>
#include <gnc-uri.hpp>

#include <unittest-support.h>
#include <test-engine-stuff.h>

#include "../gnc-backend-xml.h"
#include "../gnc-xml-backend.hpp"
#include "../io-gncxml-v2.h"
#include "test-file-stuff.h"
#include <test-stuff.h>

#define GNC_LIB_NAME "gncmod-backend-xml"
#define GNC_LIB_REL_PATH "xml"

static void
remove_files_pattern (const char* beginning, const char* ending)
{
}

static void
remove_locks (const char* filename)
{
    GStatBuf buf;
    char* to_remove;

    {
        to_remove = g_strdup_printf ("%s.LCK", filename);
        if (g_stat (to_remove, &buf) != -1)
        {
            g_unlink (to_remove);
        }
        g_free (to_remove);
    }

    remove_files_pattern (filename, ".LCK");
}

static void
test_load_file (const char* filename)
{
    gboolean ignore_lock;
    const char* logdomain = "backend.xml";
    GLogLevelFlags loglevel = static_cast<decltype (loglevel)>
                              (G_LOG_LEVEL_WARNING);
    TestErrorStruct check = { loglevel, const_cast<char*> (logdomain), NULL };
    g_log_set_handler (logdomain, loglevel,
                       (GLogFunc)test_checked_handler, &check);

    auto book = qof_book_new();
    auto session = qof_session_new (book);
    auto url = GncUri { filename }.try_str (false);

    remove_locks (filename);

    ignore_lock = (g_strcmp0 (g_getenv ("SRCDIR"), ".") != 0);
    /*    gnc_prefs_set_file_save_compressed(FALSE); */
    qof_session_begin (session, url ? url->c_str () : nullptr,
                       ignore_lock ? SESSION_READ_ONLY : SESSION_NORMAL_OPEN);

    qof_session_load (session, NULL);

    auto root = gnc_book_get_root_account (book);
    do_test (gnc_account_get_book (root) == book,
             "book and root account don't match");

    do_test (qof_instance_get_editlevel(root) == 0,
             "root account editlevel is not 0");

    do_test_args (qof_session_get_error (session) == ERR_BACKEND_NO_ERR,
                  "session load xml2", __FILE__, __LINE__,
                  "qof error=%d for file [%s]",
                  qof_session_get_error (session), filename);
    /* Uncomment the line below to generate corrected files */
    /*    qof_session_save( session, NULL ); */
    qof_session_end (session);
    qof_book_destroy (book);
}

struct XmlTestLoadTask
{
    uintptr_t handle{};
    QofSessionLoadTaskFunc run{};
    QofSessionLoadTaskFunc discard{};
    void *task_data{};
};

struct XmlTestLoadExecutor
{
    QofSessionLoadExecutor api{};
    std::deque<XmlTestLoadTask> tasks{};
    uintptr_t next_handle{1};
    guint schedule_calls{};
    guint reject_schedule_call{};
    guint max_pending{};

    XmlTestLoadExecutor ()
    {
        api.user_data = this;
        api.schedule = schedule;
        api.cancel = cancel;
    }

    static uintptr_t schedule (void *executor_data, QofSessionLoadTaskFunc run,
                               QofSessionLoadTaskFunc discard, void *task_data)
    {
        auto self = static_cast<XmlTestLoadExecutor *> (executor_data);
        ++self->schedule_calls;
        if (self->reject_schedule_call == self->schedule_calls)
            return 0;
        auto handle = self->next_handle++;
        if (!handle)
            handle = self->next_handle++;
        self->tasks.push_back ({handle, run, discard, task_data});
        self->max_pending = std::max (
            self->max_pending, static_cast<guint> (self->tasks.size ()));
        return handle;
    }

    static void cancel (void *executor_data, uintptr_t handle)
    {
        auto self = static_cast<XmlTestLoadExecutor *> (executor_data);
        auto task = std::find_if (
            self->tasks.begin (), self->tasks.end (),
            [handle] (const auto& pending) { return pending.handle == handle; });
        if (task == self->tasks.end ())
            return;
        auto discarded = *task;
        self->tasks.erase (task);
        discarded.discard (discarded.task_data);
    }

    bool run_next ()
    {
        if (tasks.empty ())
            return false;
        auto task = tasks.front ();
        tasks.pop_front ();
        task.run (task.task_data);
        return true;
    }
};

struct AsyncLoadResult
{
    XmlTestLoadExecutor executor;
    gboolean completed {FALSE};
    QofSessionLoadAsyncStatus status {QOF_SESSION_LOAD_ERROR};
    guint callbacks {};
    guint executor_turns {};
};

static void
async_load_finished (QofSession *, QofSessionLoadAsyncStatus status,
                     gpointer user_data)
{
    auto result = static_cast<AsyncLoadResult *> (user_data);
    result->status = status;
    result->completed = TRUE;
    ++result->callbacks;
}

static void
async_load_finished_destroy_session (QofSession *session,
                                     QofSessionLoadAsyncStatus status,
                                     gpointer user_data)
{
    async_load_finished (session, status, user_data);
    qof_session_destroy (session);
}

static QofSession *progress_cancel_session;
static guint progress_cancel_calls;

static void
cancel_load_from_progress (const char *, double)
{
    auto session = progress_cancel_session;
    if (!session)
        return;
    progress_cancel_session = nullptr;
    ++progress_cancel_calls;
    (void)qof_session_cancel_active_load (session);
}

static gboolean
pump_async_load (AsyncLoadResult *result)
{
    /* Bounded test-owned executor: product scheduling remains outside XML. */
    for (guint turns = 0; turns < 10000 && !result->completed; ++turns)
    {
        if (!result->executor.run_next ())
            break;
        ++result->executor_turns;
    }
    return result->completed;
}

static QofSession *
start_async_load (const char *filename, AsyncLoadResult *result,
                  QofPercentageFunc percentage = nullptr,
                  QofSessionLoadAsyncCallback callback = async_load_finished)
{
    auto session = qof_session_new (qof_book_new ());
    auto url = GncUri { filename }.try_str (false);
    auto begin_lease = qof_session_operation_lease_acquire_for (
        session, QOF_SESSION_OPERATION_OPEN);
    auto began = begin_lease && qof_session_begin_with_lease (
        session, begin_lease, url ? url->c_str () : nullptr,
        SESSION_READ_ONLY);
    qof_session_operation_lease_release (begin_lease);
    if (!began || qof_session_get_error (session) != ERR_BACKEND_NO_ERR)
    {
        qof_session_destroy (session);
        return NULL;
    }

    auto load_lease = qof_session_operation_lease_acquire_for (
        session, QOF_SESSION_OPERATION_LOAD);
    auto started = load_lease && qof_session_load_async_with_lease (
        session, load_lease, percentage, &result->executor.api,
        callback, result);
    if (!started)
    {
        qof_session_operation_lease_release (load_lease);
        qof_session_destroy (session);
        return NULL;
    }
    return session;
}

static void
test_load_file_async (const char *filename)
{
    AsyncLoadResult result;
    remove_locks (filename);
    auto session = start_async_load (filename, &result);
    do_test (session != NULL, "start public XML-v2 async load");
    if (!session)
        return;
    do_test (pump_async_load (&result), "bounded XML-v2 async success pump");
    do_test (result.status == QOF_SESSION_LOAD_COMPLETED,
             "public XML-v2 async load completed");
    do_test (result.callbacks == 1, "XML-v2 success callback exactly once");
    auto root = gnc_book_get_root_account (qof_session_get_book (session));
    do_test (root != NULL, "async XML-v2 load published a root account");
    qof_session_destroy (session);
}

static void
test_load_file_async_cancel (const char *filename)
{
    AsyncLoadResult result;
    remove_locks (filename);
    auto session = start_async_load (filename, &result);
    do_test (session != NULL, "start cancellable XML-v2 async load");
    if (!session)
        return;
    auto staging_book = qof_session_get_book (session);
    auto foreign_book = qof_book_new ();
    do_test (xaccTransLogSuppressedForBook (staging_book),
             "active XML load suppresses only its staging-book recovery log");
    do_test (!xaccTransLogSuppressedForBook (foreign_book),
             "active XML load does not globally suppress recovery logs");
    do_test (qof_session_cancel_active_load (session),
             "cancel active XML-v2 LOAD lease");
    do_test (result.completed,
             "source destruction terminalizes XML cancellation immediately");
    do_test (pump_async_load (&result), "bounded XML-v2 cancel pump");
    do_test (result.status == QOF_SESSION_LOAD_CANCELLED,
             "cancelled XML-v2 load is terminal");
    do_test (result.callbacks == 1, "XML-v2 cancel callback exactly once");
    do_test (qof_book_empty (qof_session_get_book (session)),
             "cancelled XML-v2 load leaves no partial staging book");
    do_test (!xaccTransLogSuppressedForBook (qof_session_get_book (session)),
             "XML cancel releases staging-book recovery-log suppression");
    qof_book_destroy (foreign_book);
    qof_session_destroy (session);
}

static void
test_load_file_async_error (void)
{
    static const char malformed[] =
        "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
        "<gnc-v2 xmlns:gnc=\"http://www.gnucash.org/XML/gnc\">";
    auto filename = g_strdup_printf ("%s/gnc-xml-async-%u.gnucash",
                                     g_get_tmp_dir (), g_random_int ());
    AsyncLoadResult result;
    g_file_set_contents (filename, malformed, -1, NULL);
    auto session = start_async_load (filename, &result);
    do_test (session != NULL, "start malformed XML-v2 async load");
    if (session)
    {
        do_test (pump_async_load (&result), "bounded XML-v2 error pump");
        do_test (result.status == QOF_SESSION_LOAD_ERROR,
                 "malformed XML-v2 load reports terminal error");
        do_test (result.callbacks == 1, "XML-v2 error callback exactly once");
        do_test (qof_book_empty (qof_session_get_book (session)),
                 "failed XML-v2 load leaves no partial staging book");
        qof_session_destroy (session);
    }
    g_remove (filename);
    g_free (filename);
}

static gchar *
create_many_accounts_file (gboolean compressed, guint account_count)
{
    auto filename = g_strdup_printf ("%s/gnc-xml-async-many-%u.gnucash",
                                     g_get_tmp_dir (), g_random_int ());
    auto book = qof_book_new ();
    GncXmlBackend backend;
    qof_book_set_backend (book, &backend);
    auto root = gnc_account_create_root (book);
    auto commodity = gnc_commodity_new (book, "Bounded Test Currency",
                                         "GNC_TEST", "BTCU", "", 100);
    gnc_commodity_table_insert (gnc_commodity_table_get_table (book), commodity);

    for (guint index = 0; index < account_count; ++index)
    {
        auto account = xaccMallocAccount (book);
        auto name = g_strdup_printf ("Bounded account %u", index);
        xaccAccountBeginEdit (account);
        xaccAccountSetName (account, name);
        xaccAccountSetType (account, ACCT_TYPE_BANK);
        xaccAccountSetCommodity (account, commodity);
        gnc_account_append_child (root, account);
        xaccAccountCommitEdit (account);
        g_free (name);
    }

    if (!gnc_book_write_to_xml_file_v2 (book, filename, compressed))
    {
        g_remove (filename);
        g_clear_pointer (&filename, g_free);
    }
    qof_book_set_backend (book, nullptr);
    qof_book_destroy (book);
    return filename;
}

static void
remove_generated_file (gchar *filename)
{
    if (!filename)
        return;
    remove_locks (filename);
    for (guint attempt = 0; attempt < 1000 && g_remove (filename) != 0;
         ++attempt)
    {
        g_thread_yield ();
        g_usleep (1000);
    }
    g_free (filename);
}

#if defined(__linux__)
static gboolean
gzip_producer_has_open_file (const gchar *filename)
{
    auto fd_dir = g_dir_open ("/proc/self/fd", 0, NULL);
    if (!fd_dir)
        return FALSE;

    gboolean found = FALSE;
    const gchar *entry = NULL;
    while (!found && (entry = g_dir_read_name (fd_dir)))
    {
        auto fd_name = g_build_filename ("/proc/self/fd", entry, NULL);
        auto target = g_file_read_link (fd_name, NULL);
        found = target && g_strcmp0 (target, filename) == 0;
        g_free (target);
        g_free (fd_name);
    }
    g_dir_close (fd_dir);
    return found;
}

static gboolean
wait_for_gzip_producer_file (const gchar *filename, gboolean expected_open)
{
    for (guint attempt = 0; attempt < 1000; ++attempt)
    {
        if (gzip_producer_has_open_file (filename) == expected_open)
            return TRUE;
        g_usleep (1000);
    }
    return gzip_producer_has_open_file (filename) == expected_open;
}
#endif

static void
test_load_file_async_stale (const char *filename)
{
    if (gnc_current_session_exist ())
        gnc_clear_current_session ();
    auto marker = qof_session_new (qof_book_new ());
    gnc_set_current_session (marker);
    AsyncLoadResult result;
    remove_locks (filename);
    auto session = start_async_load (filename, &result);
    do_test (session != NULL, "start generation-guarded XML-v2 async load");
    if (!session)
    {
        gnc_clear_current_session ();
        return;
    }

    /* Advance far enough to prove that STALE rolls back parsed state instead
     * of merely rejecting a still-empty staging book. Finalization remains
     * far from terminal even for a small fixture because it is object-bounded. */
    for (guint turn = 0;
         turn < 128 && qof_book_empty (qof_session_get_book (session)) &&
         !result.completed;
         ++turn)
    {
        result.executor.run_next ();
        ++result.executor_turns;
    }
    do_test (!result.completed, "XML-v2 load remains active before STALE");
    do_test (!qof_book_empty (qof_session_get_book (session)),
             "XML-v2 STALE test reached a partial staging book");
    gnc_clear_current_session ();
    do_test (pump_async_load (&result), "bounded XML-v2 stale pump");
    do_test (result.status == QOF_SESSION_LOAD_STALE,
             "current-session generation drift reports STALE");
    do_test (result.callbacks == 1, "XML-v2 STALE callback exactly once");
    do_test (qof_book_empty (qof_session_get_book (session)),
             "STALE XML-v2 load rolls back the partial staging book");
    qof_session_destroy (session);
}

static void
test_load_file_async_bounded_finalization (void)
{
    constexpr guint account_count = 48;
    auto filename = create_many_accounts_file (FALSE, account_count);
    do_test (filename != NULL, "create many-account XML-v2 fixture");
    if (!filename)
        return;

    AsyncLoadResult result;
    auto session = start_async_load (filename, &result);
    do_test (session != NULL, "start many-account XML-v2 async load");
    if (session)
    {
        do_test (pump_async_load (&result), "bounded many-account XML-v2 pump");
        do_test (result.status == QOF_SESSION_LOAD_COMPLETED,
                 "many-account XML-v2 load completed");
        do_test (result.callbacks == 1,
                 "many-account XML-v2 callback exactly once");
        do_test (result.executor_turns > account_count * 4,
                 "many objects require many bounded executor tasks");
        do_test (result.executor.schedule_calls == result.executor_turns,
                 "each XML step owns one executor task");
        do_test (result.executor.max_pending == 1,
                 "XML load retains at most one pending executor task");
        qof_session_destroy (session);
    }
    remove_generated_file (filename);
}

static void
test_load_file_async_gzip_cancel (void)
{
#ifdef G_OS_WIN32
    constexpr guint account_count = 48;
#else
    constexpr guint account_count = 4096;
#endif
    auto filename = create_many_accounts_file (TRUE, account_count);
    do_test (filename != NULL, "create compressed XML-v2 fixture");
    if (!filename)
        return;

    AsyncLoadResult result;
    auto session = start_async_load (filename, &result);
    do_test (session != NULL, "start compressed XML-v2 async load");
    if (session)
    {
#ifndef G_OS_WIN32
        /* Let the large compressed input start producing pipe data before
         * cancellation closes the reader. This exercises the EPIPE path. */
        for (guint turn = 0; turn < 8 && !result.completed; ++turn)
        {
            result.executor.run_next ();
            ++result.executor_turns;
        }
        do_test (!result.completed,
                 "large compressed XML-v2 load remains active before cancellation");
#if defined(__linux__)
        if (g_file_test ("/proc/self/fd", G_FILE_TEST_IS_DIR))
            do_test (wait_for_gzip_producer_file (filename, TRUE),
                     "gzip producer opened the compressed input before cancellation");
#endif
#endif
        do_test (qof_session_cancel_active_load (session),
                 "cancel compressed XML-v2 load");
        do_test (result.completed && result.callbacks == 1,
                 "gzip pipe close terminalizes cancellation exactly once");
        do_test (result.status == QOF_SESSION_LOAD_CANCELLED,
                 "compressed XML-v2 load reports cancellation");
        do_test (qof_book_empty (qof_session_get_book (session)),
                 "compressed cancellation rolls back staging book");
#if defined(__linux__)
        if (g_file_test ("/proc/self/fd", G_FILE_TEST_IS_DIR))
            do_test (wait_for_gzip_producer_file (filename, FALSE),
                     "gzip producer released compressed input after cancellation");
#endif
        qof_session_destroy (session);
    }
    remove_generated_file (filename);
}

static void
test_load_file_async_followup_schedule_rejected (void)
{
    constexpr guint account_count = 8;
    auto filename = create_many_accounts_file (FALSE, account_count);
    do_test (filename != NULL, "create follow-up rejection XML-v2 fixture");
    if (!filename)
        return;

    AsyncLoadResult result;
    result.executor.reject_schedule_call = 2;
    auto session = start_async_load (filename, &result);
    do_test (session != NULL, "initial XML executor task accepted");
    if (session)
    {
        do_test (result.executor.run_next (), "run first XML executor task");
        do_test (result.completed,
                 "rejected follow-up schedule terminalizes XML load");
        do_test (result.status == QOF_SESSION_LOAD_ERROR,
                 "rejected follow-up schedule reports terminal error");
        do_test (result.callbacks == 1,
                 "follow-up schedule rejection calls back exactly once");
        do_test (result.executor.schedule_calls == 2,
                 "follow-up schedule, not initial schedule, was rejected");
        do_test (result.executor.tasks.empty (),
                 "schedule rejection retains no executor task");
        do_test (qof_book_empty (qof_session_get_book (session)),
                 "schedule rejection rolls back the staging book");
        qof_session_destroy (session);
    }
    remove_generated_file (filename);
}

static void
test_load_file_async_cancel_from_running_step (void)
{
    constexpr guint account_count = 48;
    auto filename = create_many_accounts_file (FALSE, account_count);
    do_test (filename != NULL, "create running-step cancel XML-v2 fixture");
    if (!filename)
        return;

    AsyncLoadResult result;
    progress_cancel_calls = 0;
    auto session = start_async_load (filename, &result,
                                     cancel_load_from_progress);
    do_test (session != NULL, "start progress-cancelled XML-v2 load");
    if (session)
    {
        progress_cancel_session = session;
        do_test (pump_async_load (&result),
                 "running XML step observes deferred cancellation");
        progress_cancel_session = nullptr;
        do_test (progress_cancel_calls == 1,
                 "progress callback requests cancellation once");
        do_test (result.status == QOF_SESSION_LOAD_CANCELLED,
                 "running-step cancellation reports cancelled");
        do_test (result.callbacks == 1,
                 "running-step cancellation calls back exactly once");
        do_test (result.executor.tasks.empty (),
                 "running-step cancellation schedules no successor");
        do_test (qof_book_empty (qof_session_get_book (session)),
                 "running-step cancellation rolls back staging data");
        qof_session_destroy (session);
    }
    progress_cancel_session = nullptr;
    remove_generated_file (filename);
}

static void
test_load_file_async_callback_destroys_session (const char *filename)
{
    AsyncLoadResult result;
    remove_locks (filename);
    auto session = start_async_load (filename, &result, nullptr,
                                     async_load_finished_destroy_session);
    do_test (session != NULL, "start self-destroying XML-v2 async load");
    if (!session)
        return;

    do_test (pump_async_load (&result),
             "XML terminal callback may destroy session and backend");
    do_test (result.status == QOF_SESSION_LOAD_COMPLETED,
             "self-destroying XML-v2 load completed");
    do_test (result.callbacks == 1,
             "self-destroying XML-v2 callback ran exactly once");
    do_test (result.executor.tasks.empty (),
             "self-destroying terminal callback retains no task");
}

int
main (int argc, char** argv)
{
    g_setenv ("GNC_UNINSTALLED", "1", TRUE);
    const char* location = g_getenv ("GNC_TEST_FILES");
    int files_tested = 0;
    GDir* xml2_dir;

    qof_init ();
    cashobjects_register ();
    do_test (qof_load_backend_library (GNC_LIB_REL_PATH, GNC_LIB_NAME),
             " loading gnc-backend-xml GModule failed");

    if (!location)
    {
        location = "test-files/xml2";
    }

    if ((xml2_dir = g_dir_open (location, 0, NULL)) == NULL)
    {
        failure ("unable to open xml2 directory");
    }
    else
    {
        const gchar* entry;

        while ((entry = g_dir_read_name (xml2_dir)) != NULL)
        {
            if (g_str_has_suffix (entry, ".gml2"))
            {
                gchar* to_open = g_build_filename (location, entry, (gchar*)NULL);
                if (!g_file_test (to_open, G_FILE_TEST_IS_DIR))
                {
                    if (files_tested == 0)
                        test_load_file_async (to_open);
                    if (files_tested == 0)
                        test_load_file_async_cancel (to_open);
                    if (files_tested == 0)
                        test_load_file_async_stale (to_open);
                    if (files_tested == 0)
                        test_load_file_async_callback_destroys_session (to_open);
                    test_load_file (to_open);
                    files_tested++;
                }
                g_free (to_open);
            }
        }
    }

    g_dir_close (xml2_dir);
    test_load_file_async_error ();
    test_load_file_async_bounded_finalization ();
    test_load_file_async_followup_schedule_rejected ();
    test_load_file_async_cancel_from_running_step ();
    test_load_file_async_gzip_cancel ();

    if (files_tested == 0)
    {
        failure ("handled 0 files in test-load-xml2");
    }

    print_test_results ();
    qof_close ();
    exit (get_rv ());
}
