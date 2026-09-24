/********************************************************************
 * test-qofsession.cpp: A Google Test suite for Qof Session.        *
 * Copyright 2016 Aaron Laws                                        *
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
 * along with this program; if not, you can retrieve it from        *
 * https://www.gnu.org/licenses/old-licenses/gpl-2.0.html            *
 * or contact:                                                      *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 ********************************************************************/

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcpp"
#include <gtest/gtest.h>
#pragma GCC diagnostic pop

#include "../guid.hpp"
#include <qofsession.hpp>
#include <gnc-session.h>
#include <Scrub.h>
#include <ScrubP.h>
#include <qof-backend.hpp>
#include <cstdlib>
#include "../gnc-backend-prov.hpp"
#include "../Account.h"
#include "../Split.h"
#include "../SplitP.hpp"
#include "../Transaction.h"
#include "../TransactionP.hpp"
#include "../gnc-commodity.h"
#include <vector>

static QofBook * exported_book {nullptr};
static bool safe_sync_called {false};
static bool sync_called {false};
static bool load_error {true};
static bool data_loaded {false};
static bool observe_current_session_during_end {false};
static bool current_session_was_detached_on_end {false};
static QofSession *cancel_session_during_load {nullptr};

struct DestroyAccount
{
    void operator()(Account *acct)
    {
        xaccAccountBeginEdit (acct);
        xaccAccountDestroy (acct);
    }
};

using AccountPtr = std::unique_ptr<Account, DestroyAccount>;

class QofSessionMockBackend : public QofBackend
{
    AccountPtr m_root;
public:
    QofSessionMockBackend() = default;
    QofSessionMockBackend(const QofSessionMockBackend&) = delete;
    QofSessionMockBackend(const QofSessionMockBackend&&) = delete;
    virtual ~QofSessionMockBackend() = default;
    void session_begin(QofSession*, const char*, SessionOpenMode) {}
    void session_end()
    {
        if (observe_current_session_during_end)
            current_session_was_detached_on_end =
                !gnc_current_session_exist ();
    }
    void load(QofBook*, QofBackendLoadType);
    void sync(QofBook*);
    void safe_sync(QofBook*);
    void export_coa(QofBook*);
};

void QofSessionMockBackend::load (QofBook *book, QofBackendLoadType)
{
    if (cancel_session_during_load)
    {
        auto session = cancel_session_during_load;
        cancel_session_during_load = nullptr;
        qof_session_cancel_active_load (session);
    }
    if (load_error)
        set_error(ERR_BACKEND_NO_BACKEND);
    else
        m_root = AccountPtr{gnc_account_create_root (book)};
    data_loaded = true;
}

void QofSessionMockBackend::safe_sync (QofBook *)
{
    safe_sync_called = true;
}

void QofSessionMockBackend::sync (QofBook *)
{
    sync_called = true;
}

void QofSessionMockBackend::export_coa(QofBook * book)
{
    exported_book = book;
}

static QofBackend*
test_backend_factory ()
{
    return new QofSessionMockBackend;
}

struct MockProvider : public QofBackendProvider
{
    MockProvider (char const * name, char const * access_method)
        : QofBackendProvider {name, access_method} {}
    QofBackend * create_backend (void) {return test_backend_factory ();}
    bool type_check (char const * type) {return true;}
};

static QofBackendProvider_ptr
get_provider ()
{
    return QofBackendProvider_ptr {new MockProvider {"Mock Backend", "file"}};
}

struct ScrubJobBook
{
    QofBook *book;
    Account *root;
    Account *account;
    gnc_commodity *currency;
    std::vector<Split *> orphan_splits;

    ScrubJobBook (QofBook *target_book, guint transaction_count)
        : book {target_book}
        , root {gnc_account_create_root (book)}
        , account {xaccMallocAccount (book)}
        , currency {gnc_commodity_new (book, "Test Currency", "CURRENCY",
                                       "TST", "", 100)}
    {
        xaccAccountBeginEdit (account);
        xaccAccountSetName (account, "Test account");
        xaccAccountSetType (account, ACCT_TYPE_BANK);
        xaccAccountSetCommodity (account, currency);
        gnc_account_append_child (root, account);
        xaccAccountCommitEdit (account);

        for (guint i = 0; i < transaction_count; ++i)
        {
            auto transaction = xaccMallocTransaction (book);
            auto attached = xaccMallocSplit (book);
            auto orphan = xaccMallocSplit (book);
            xaccTransBeginEdit (transaction);
            xaccTransSetCurrency (transaction, currency);
            xaccSplitSetParent (attached, transaction);
            xaccSplitSetAccount (attached, account);
            xaccSplitSetParent (orphan, transaction);
            xaccTransCommitEdit (transaction);
            orphan_splits.push_back (orphan);
        }
    }
};

struct ScopedEnvironment
{
    const char *key;
    char *previous;

    ScopedEnvironment (const char *target_key, const char *value)
        : key {target_key}
        , previous {g_strdup (g_getenv (target_key))}
    {
        if (value)
            g_setenv (key, value, TRUE);
        else
            g_unsetenv (key);
    }

    ~ScopedEnvironment ()
    {
        if (previous)
            g_setenv (key, previous, TRUE);
        else
            g_unsetenv (key);
        g_free (previous);
    }
};

struct ScopedDataScrubbingDisabled
{
    ScopedDataScrubbingDisabled ()
    {
        xaccDisableDataScrubbing ();
    }

    ~ScopedDataScrubbingDisabled ()
    {
        xaccEnableDataScrubbing ();
    }
};

struct CommitDeferralBook
{
    QofBook *book;
    Account *root;
    Account *account;
    gnc_commodity *currency;

    explicit CommitDeferralBook (QofBook *target_book)
        : book {target_book}
        , root {gnc_account_create_root (book)}
        , account {xaccMallocAccount (book)}
        , currency {gnc_commodity_new (book, "Commit Deferral Currency",
                                       "CURRENCY", "CDF", "", 100)}
    {
        xaccAccountBeginEdit (account);
        xaccAccountSetName (account, "Commit deferral account");
        xaccAccountSetType (account, ACCT_TYPE_BANK);
        xaccAccountSetCommodity (account, currency);
        gnc_account_append_child (root, account);
        xaccAccountCommitEdit (account);
    }

    Transaction *commit_unbalanced (guint value)
    {
        auto transaction = xaccMallocTransaction (book);
        auto split = xaccMallocSplit (book);
        auto amount = gnc_numeric_create (value, 1);
        xaccTransBeginEdit (transaction);
        xaccTransSetCurrency (transaction, currency);
        xaccSplitSetParent (split, transaction);
        xaccSplitSetAccount (split, account);
        split->value = amount;
        split->amount = amount;
        xaccTransCommitEdit (transaction);
        return transaction;
    }
};

static void
scrub_job_progress (const char *, double)
{
}

static Account *
expect_orphans_scrubbed (const ScrubJobBook& fixture)
{
    Account *destination = nullptr;
    for (auto split : fixture.orphan_splits)
    {
        auto account = xaccSplitGetAccount (split);
        EXPECT_NE (account, nullptr);
        if (!destination)
            destination = account;
        else
            EXPECT_EQ (account, destination);
    }
    if (destination)
    {
        EXPECT_EQ (xaccAccountGetType (destination), ACCT_TYPE_BANK);
        EXPECT_EQ (xaccAccountGetCommodity (destination), fixture.currency);
    }
    return destination;
}

TEST (QofSessionTest, swap_books)
{
    qof_backend_register_provider (get_provider ());
    QofSession s1(qof_book_new());
    s1.begin ("book1", SESSION_NORMAL_OPEN);
    QofSession s2(qof_book_new());
    s2.begin ("book2", SESSION_NORMAL_OPEN);
    QofBook * b1 {s1.get_book ()};
    QofBook * b2 {s2.get_book ()};
    ASSERT_NE (b1, b2);
    s1.swap_books (s2);
    EXPECT_EQ (s1.get_book (), b2);
    EXPECT_EQ (s2.get_book (), b1);
    qof_backend_unregister_all_providers ();
}

TEST (QofSessionTest, ensure_all_data_loaded)
{
    qof_backend_register_provider (get_provider ());
    QofSession s(qof_book_new());
    s.begin ("book1", SESSION_NORMAL_OPEN);
    data_loaded = false;
    s.ensure_all_data_loaded ();
    EXPECT_EQ (data_loaded, true);
    qof_backend_unregister_all_providers ();
}

TEST (QofSessionTest, get_error)
{
    qof_backend_register_provider (get_provider ());
    QofSession s(qof_book_new());
    s.begin ("book1", SESSION_NORMAL_OPEN);
    s.ensure_all_data_loaded ();
    EXPECT_NE (s.get_error (), ERR_BACKEND_NO_ERR);
    //get_error should not clear the error.
    EXPECT_NE (s.get_error (), ERR_BACKEND_NO_ERR);
    qof_backend_unregister_all_providers ();
}

TEST (QofSessionTest, pop_error)
{
    qof_backend_register_provider (get_provider ());
    QofSession s(qof_book_new());
    s.begin ("book1", SESSION_NORMAL_OPEN);
    //We run the test first, and make sure there is an error condition.
    s.ensure_all_data_loaded ();
    EXPECT_NE (s.pop_error (), ERR_BACKEND_NO_ERR);
    EXPECT_EQ (s.get_error (), ERR_BACKEND_NO_ERR);
    qof_backend_unregister_all_providers ();
}

TEST (QofSessionTest, clear_error)
{
    qof_backend_register_provider (get_provider ());
    QofSession s(qof_book_new());
    s.begin ("book1", SESSION_NORMAL_OPEN);
    //We run the test first, and make sure there is an error condition.
    s.ensure_all_data_loaded ();
    EXPECT_NE (s.get_error (), ERR_BACKEND_NO_ERR);
    //Now we run it, and clear_error to make sure the error is actually cleared.
    s.ensure_all_data_loaded ();
    s.clear_error ();
    EXPECT_EQ (s.get_error (), ERR_BACKEND_NO_ERR);
    qof_backend_unregister_all_providers ();
}

TEST (QofSessionTest, load)
{
    /* We register a provider that gives a backend that throws an error on load.
     * This error during load should cause the qof session to destroy the book
     * and create a new one.
     */
    qof_backend_register_provider (get_provider ());
    QofSession s{qof_book_new()};
    s.begin ("book1", SESSION_NORMAL_OPEN);
    char *guidstr1 = guid_to_string(qof_instance_get_guid(s.get_book ()));
    s.load (nullptr);
    char *guidstr2 = guid_to_string(qof_instance_get_guid(s.get_book ()));
    EXPECT_STRNE (guidstr1, guidstr2);
    g_free(guidstr1);
    g_free(guidstr2);

    /* Now we'll do the load without returning an error from the backend,
     * and ensure that it's the new book from the previous test.
     */
    load_error = false;
    guidstr1 = guid_to_string(qof_instance_get_guid(s.get_book ()));
    s.load (nullptr);
    guidstr2 = guid_to_string(qof_instance_get_guid(s.get_book ()));
    EXPECT_STREQ (guidstr1, guidstr2);
    g_free(guidstr1);
    g_free(guidstr2);
    EXPECT_EQ (s.get_error(), ERR_BACKEND_NO_ERR);
    //But it's still empty, to the book shouldn't need saving
    EXPECT_FALSE(qof_book_session_not_saved (s.get_book ()));
    // I'll put load_error back just to be tidy.
    load_error = true;
    qof_backend_unregister_all_providers ();
}

struct AsyncSessionLoadResult
{
    guint callbacks{};
    QofSessionLoadAsyncStatus status{QOF_SESSION_LOAD_ERROR};
};

static void
async_session_load_finished (QofSession *, QofSessionLoadAsyncStatus status,
                             gpointer user_data)
{
    auto result = static_cast<AsyncSessionLoadResult *> (user_data);
    ++result->callbacks;
    result->status = status;
}

struct ManualSessionLoadExecutor
{
    QofSessionLoadTaskFunc run{};
    QofSessionLoadTaskFunc discard{};
    void *task_data{};
    uintptr_t handle{};
    uintptr_t next_handle{1};
    bool running{};
    bool reject{};

    QofSessionLoadExecutor api ()
    {
        return {this, schedule_cb, cancel_cb};
    }

    bool dispatch ()
    {
        if (!handle || running)
            return false;

        auto task = run;
        auto data = task_data;
        clear ();
        running = true;
        task (data);
        running = false;
        return true;
    }

private:
    static uintptr_t schedule_cb (void *executor_data,
                                  QofSessionLoadTaskFunc run,
                                  QofSessionLoadTaskFunc discard,
                                  void *task_data)
    {
        auto executor = static_cast<ManualSessionLoadExecutor *> (executor_data);
        if (!executor || executor->reject || executor->handle ||
            executor->running || !run || !discard)
            return 0;

        executor->run = run;
        executor->discard = discard;
        executor->task_data = task_data;
        executor->handle = executor->next_handle++;
        return executor->handle;
    }

    static void cancel_cb (void *executor_data, uintptr_t task_handle)
    {
        auto executor = static_cast<ManualSessionLoadExecutor *> (executor_data);
        if (!executor || !task_handle || task_handle != executor->handle ||
            executor->running)
            return;

        auto task = executor->discard;
        auto data = executor->task_data;
        executor->clear ();
        task (data);
    }

    void clear ()
    {
        run = nullptr;
        discard = nullptr;
        task_data = nullptr;
        handle = 0;
    }
};

static void
async_session_load_finished_and_destroy (
    QofSession *session, QofSessionLoadAsyncStatus status, gpointer user_data)
{
    async_session_load_finished (session, status, user_data);
    qof_session_destroy (session);
}

static QofSession *
start_legacy_async_load (AsyncSessionLoadResult *result,
                         ManualSessionLoadExecutor *executor,
                         QofSessionLoadAsyncCallback callback =
                             async_session_load_finished,
                         QofSessionOperationLease **load_lease_out = nullptr)
{
    auto session = qof_session_new (qof_book_new ());
    auto open_lease = qof_session_operation_lease_acquire_for (
        session, QOF_SESSION_OPERATION_OPEN);
    auto began = open_lease && qof_session_begin_with_lease (
        session, open_lease, "book1", SESSION_NORMAL_OPEN);
    qof_session_operation_lease_release (open_lease);
    if (!began)
    {
        qof_session_destroy (session);
        return nullptr;
    }

    auto load_lease = qof_session_operation_lease_acquire_for (
        session, QOF_SESSION_OPERATION_LOAD);
    auto executor_api = executor ? executor->api () : QofSessionLoadExecutor {};
    if (!load_lease || !qof_session_load_async_with_lease (
            session, load_lease, nullptr, executor ? &executor_api : nullptr,
            callback, result))
    {
        qof_session_operation_lease_release (load_lease);
        qof_session_destroy (session);
        return nullptr;
    }
    if (load_lease_out)
        *load_lease_out = load_lease;
    return session;
}

TEST (QofSessionTest, legacy_async_load_success_is_deferred_and_exactly_once)
{
    qof_backend_register_provider (get_provider ());
    load_error = false;
    data_loaded = false;
    AsyncSessionLoadResult result;
    ManualSessionLoadExecutor executor;
    auto session = start_legacy_async_load (&result, &executor);
    ASSERT_NE (session, nullptr);

    EXPECT_EQ (result.callbacks, 0u);
    EXPECT_FALSE (data_loaded);
    EXPECT_TRUE (executor.dispatch ());
    EXPECT_TRUE (data_loaded);
    EXPECT_EQ (result.callbacks, 1u);
    EXPECT_EQ (result.status, QOF_SESSION_LOAD_COMPLETED);
    EXPECT_FALSE (qof_session_has_active_operation_lease (session));
    EXPECT_FALSE (executor.dispatch ());
    EXPECT_EQ (result.callbacks, 1u);

    qof_session_destroy (session);
    load_error = true;
    qof_backend_unregister_all_providers ();
}

TEST (QofSessionTest, legacy_async_load_cancel_discards_task_exactly_once)
{
    qof_backend_register_provider (get_provider ());
    load_error = false;
    data_loaded = false;
    AsyncSessionLoadResult result;
    ManualSessionLoadExecutor executor;
    auto session = start_legacy_async_load (&result, &executor);
    ASSERT_NE (session, nullptr);

    EXPECT_TRUE (qof_session_cancel_active_load (session));
    EXPECT_EQ (result.callbacks, 1u);
    EXPECT_EQ (result.status, QOF_SESSION_LOAD_CANCELLED);
    EXPECT_FALSE (data_loaded);
    EXPECT_TRUE (qof_book_empty (qof_session_get_book (session)));
    EXPECT_FALSE (qof_session_has_active_operation_lease (session));
    EXPECT_FALSE (qof_session_cancel_active_load (session));
    EXPECT_FALSE (executor.dispatch ());
    EXPECT_EQ (result.callbacks, 1u);

    qof_session_destroy (session);
    load_error = true;
    qof_backend_unregister_all_providers ();
}

TEST (QofSessionTest, legacy_async_load_teardown_cancels_before_destroy)
{
    qof_backend_register_provider (get_provider ());
    load_error = false;
    data_loaded = false;
    AsyncSessionLoadResult result;
    ManualSessionLoadExecutor executor;
    QofSessionOperationLease *load_lease = nullptr;
    auto session = start_legacy_async_load (
        &result, &executor, async_session_load_finished, &load_lease);
    ASSERT_NE (session, nullptr);
    ASSERT_NE (load_lease, nullptr);

    /* An attempted owner teardown must first discard the deferred task. The
     * session deliberately remains alive until its caller retries destruction
     * after the terminal callback has released the LOAD lease. */
    EXPECT_FALSE (qof_session_destroy_with_lease (session, load_lease));
    EXPECT_EQ (result.callbacks, 1u);
    EXPECT_EQ (result.status, QOF_SESSION_LOAD_CANCELLED);
    EXPECT_FALSE (data_loaded);
    EXPECT_TRUE (qof_book_empty (qof_session_get_book (session)));
    EXPECT_FALSE (qof_session_has_active_operation_lease (session));
    EXPECT_FALSE (executor.dispatch ());
    EXPECT_EQ (result.callbacks, 1u);

    qof_session_destroy (session);
    load_error = true;
    qof_backend_unregister_all_providers ();
}

TEST (QofSessionTest, legacy_async_load_error_rolls_back_staging_book)
{
    qof_backend_register_provider (get_provider ());
    load_error = true;
    data_loaded = false;
    AsyncSessionLoadResult result;
    ManualSessionLoadExecutor executor;
    auto session = start_legacy_async_load (&result, &executor);
    ASSERT_NE (session, nullptr);

    EXPECT_TRUE (executor.dispatch ());
    EXPECT_TRUE (data_loaded);
    EXPECT_EQ (result.callbacks, 1u);
    EXPECT_EQ (result.status, QOF_SESSION_LOAD_ERROR);
    EXPECT_TRUE (qof_book_empty (qof_session_get_book (session)));
    EXPECT_FALSE (qof_session_has_active_operation_lease (session));

    qof_session_destroy (session);
    qof_backend_unregister_all_providers ();
}

TEST (QofSessionTest, legacy_async_load_generation_drift_is_stale)
{
    if (gnc_current_session_exist ())
        gnc_clear_current_session ();
    qof_backend_register_provider (get_provider ());
    load_error = false;
    data_loaded = false;
    auto marker = qof_session_new (qof_book_new ());
    gnc_set_current_session (marker);
    AsyncSessionLoadResult result;
    ManualSessionLoadExecutor executor;
    auto session = start_legacy_async_load (&result, &executor);
    ASSERT_NE (session, nullptr);

    /* Detaching the unrelated current session advances the generation that
     * the LOAD lease captured without explicitly cancelling the load. */
    gnc_clear_current_session ();
    EXPECT_TRUE (executor.dispatch ());
    EXPECT_FALSE (data_loaded);
    EXPECT_EQ (result.callbacks, 1u);
    EXPECT_EQ (result.status, QOF_SESSION_LOAD_STALE);
    EXPECT_TRUE (qof_book_empty (qof_session_get_book (session)));
    EXPECT_FALSE (qof_session_has_active_operation_lease (session));

    qof_session_destroy (session);
    load_error = true;
    qof_backend_unregister_all_providers ();
}

TEST (QofSessionTest, legacy_async_load_generation_drift_blocks_legacy_destroy)
{
    if (gnc_current_session_exist ())
        gnc_clear_current_session ();
    qof_backend_register_provider (get_provider ());
    load_error = false;
    data_loaded = false;
    auto marker = qof_session_new (qof_book_new ());
    gnc_set_current_session (marker);
    AsyncSessionLoadResult result;
    ManualSessionLoadExecutor executor;
    auto session = start_legacy_async_load (
        &result, &executor, async_session_load_finished_and_destroy);
    ASSERT_NE (session, nullptr);

    /* A generation change invalidates the lease, but must not make a queued
     * LOAD task's session pointer disposable before terminal dispatch. */
    gnc_clear_current_session ();
    qof_session_destroy (session);
    EXPECT_EQ (result.callbacks, 0u);
    EXPECT_TRUE (executor.dispatch ());
    EXPECT_FALSE (data_loaded);
    EXPECT_EQ (result.callbacks, 1u);
    EXPECT_EQ (result.status, QOF_SESSION_LOAD_STALE);
    EXPECT_FALSE (executor.dispatch ());

    load_error = true;
    qof_backend_unregister_all_providers ();
}

TEST (QofSessionTest, legacy_async_load_callback_may_destroy_session)
{
    qof_backend_register_provider (get_provider ());
    load_error = false;
    data_loaded = false;
    AsyncSessionLoadResult result;
    ManualSessionLoadExecutor executor;
    auto session = start_legacy_async_load (
        &result, &executor, async_session_load_finished_and_destroy);
    ASSERT_NE (session, nullptr);

    EXPECT_TRUE (executor.dispatch ());
    EXPECT_TRUE (data_loaded);
    EXPECT_EQ (result.callbacks, 1u);
    EXPECT_EQ (result.status, QOF_SESSION_LOAD_COMPLETED);
    EXPECT_FALSE (executor.dispatch ());

    load_error = true;
    qof_backend_unregister_all_providers ();
}

TEST (QofSessionTest, legacy_async_load_cancel_while_running_is_exactly_once)
{
    qof_backend_register_provider (get_provider ());
    load_error = false;
    data_loaded = false;
    AsyncSessionLoadResult result;
    ManualSessionLoadExecutor executor;
    auto session = start_legacy_async_load (&result, &executor);
    ASSERT_NE (session, nullptr);
    cancel_session_during_load = session;

    EXPECT_TRUE (executor.dispatch ());
    EXPECT_EQ (cancel_session_during_load, nullptr);
    EXPECT_EQ (result.callbacks, 1u);
    EXPECT_EQ (result.status, QOF_SESSION_LOAD_CANCELLED);
    EXPECT_TRUE (qof_book_empty (qof_session_get_book (session)));
    EXPECT_FALSE (qof_session_has_active_operation_lease (session));
    EXPECT_FALSE (executor.dispatch ());

    qof_session_destroy (session);
    load_error = true;
    qof_backend_unregister_all_providers ();
}

TEST (QofSessionTest, legacy_async_load_rejected_schedule_keeps_lease_ownership)
{
    qof_backend_register_provider (get_provider ());
    load_error = false;
    data_loaded = false;
    AsyncSessionLoadResult result;
    ManualSessionLoadExecutor executor;
    executor.reject = true;

    EXPECT_EQ (start_legacy_async_load (&result, &executor), nullptr);
    EXPECT_EQ (result.callbacks, 0u);
    EXPECT_FALSE (data_loaded);
    EXPECT_FALSE (executor.dispatch ());

    load_error = true;
    qof_backend_unregister_all_providers ();
}

TEST (QofSessionTest, save)
{
    qof_backend_register_provider (get_provider ());
    QofSession s(qof_book_new());
    s.begin ("book1", SESSION_NORMAL_OPEN);
    load_error = false;
    s.load (nullptr);
    qof_book_mark_session_dirty (s.get_book ());
    s.save (nullptr);
    EXPECT_EQ (sync_called, true);
    qof_backend_unregister_all_providers ();
    sync_called = false;
    load_error = true;
}

TEST (QofSessionTest, safe_save)
{
    qof_backend_register_provider (get_provider ());
    QofSession s(qof_book_new());
    s.begin ("book1", SESSION_NORMAL_OPEN);
    s.safe_save (nullptr);
    EXPECT_EQ (safe_sync_called, true);
    qof_backend_unregister_all_providers ();
    safe_sync_called = false;
}

TEST (QofSessionTest, export_session)
{
    qof_backend_register_provider (get_provider ());
    auto b1 = qof_book_new();
    QofSession s1(b1);
    s1.begin ("book1", SESSION_NORMAL_OPEN);
    qof_book_set_backend(b1, s1.get_backend());
    auto b2 = qof_book_new();
    QofSession s2(b2);
    s2.begin ("book2", SESSION_NORMAL_OPEN);
    qof_book_set_backend(b2, s2.get_backend());
    s2.export_session (s1, nullptr);
    EXPECT_EQ (exported_book, b1);

    qof_backend_unregister_all_providers ();
}

TEST (QofSessionOperationLeaseTest, is_exclusive_and_session_bound)
{
    auto session_1 = qof_session_new (qof_book_new ());
    auto session_2 = qof_session_new (qof_book_new ());
    auto book_1 = qof_session_get_book (session_1);
    auto book_2 = qof_session_get_book (session_2);
    auto lease = qof_session_operation_lease_acquire (session_1);

    ASSERT_NE (lease, nullptr);
    auto operation_id = qof_session_operation_lease_get_id (lease);
    EXPECT_NE (operation_id, 0u);
    EXPECT_TRUE (qof_session_operation_lease_is_valid (lease, session_1));
    EXPECT_FALSE (qof_session_operation_lease_is_valid (lease, session_2));
    EXPECT_EQ (qof_session_operation_lease_acquire (session_1), nullptr);
    EXPECT_FALSE (qof_session_save_with_lease (session_2, lease, nullptr));

    qof_session_swap_data (session_1, session_2);
    EXPECT_EQ (qof_session_get_book (session_1), book_1);
    EXPECT_EQ (qof_session_get_book (session_2), book_2);
    EXPECT_TRUE (qof_session_operation_lease_is_valid (lease, session_1));

    qof_session_operation_lease_release (lease);
    EXPECT_FALSE (qof_session_has_active_operation_lease (session_1));

    auto next_lease = qof_session_operation_lease_acquire (session_1);
    ASSERT_NE (next_lease, nullptr);
    EXPECT_NE (qof_session_operation_lease_get_id (next_lease), operation_id);
    qof_session_operation_lease_release (next_lease);

    qof_session_swap_data (session_1, session_2);
    EXPECT_EQ (qof_session_get_book (session_1), book_2);
    EXPECT_EQ (qof_session_get_book (session_2), book_1);

    qof_session_destroy (session_1);
    qof_session_destroy (session_2);
}

TEST (QofSessionOperationLeaseTest, exposes_exclusive_operation_kind)
{
    auto session = qof_session_new (qof_book_new ());
    auto lease = qof_session_operation_lease_acquire_for (
        session, QOF_SESSION_OPERATION_SAVE_AS);

    ASSERT_NE (lease, nullptr);
    EXPECT_EQ (qof_session_operation_lease_get_kind (lease),
               QOF_SESSION_OPERATION_SAVE_AS);
    EXPECT_TRUE (qof_session_has_active_operation_kind (
        session, QOF_SESSION_OPERATION_SAVE_AS));
    EXPECT_FALSE (qof_session_has_active_operation_kind (
        session, QOF_SESSION_OPERATION_SAVE));
    EXPECT_EQ (qof_session_operation_lease_acquire_for (
        session, QOF_SESSION_OPERATION_EXPORT), nullptr);

    qof_session_operation_lease_release (lease);
    EXPECT_FALSE (qof_session_has_active_operation_kind (
        session, QOF_SESSION_OPERATION_SAVE_AS));
    qof_session_destroy (session);
}

TEST (QofSessionOperationLeaseTest, clears_current_session_only_for_owner)
{
    if (gnc_current_session_exist ())
        gnc_clear_current_session ();

    qof_backend_register_provider (get_provider ());
    auto current = qof_session_new (qof_book_new ());
    auto foreign = qof_session_new (qof_book_new ());
    qof_session_begin (current, "book1", SESSION_NORMAL_OPEN);
    qof_book_set_backend (qof_session_get_book (current),
                          qof_session_get_backend (current));
    gnc_set_current_session (current);
    auto generation = gnc_current_session_get_generation ();
    auto current_lease = qof_session_operation_lease_acquire_for (
        current, QOF_SESSION_OPERATION_CLOSE);
    auto foreign_lease = qof_session_operation_lease_acquire_for (
        foreign, QOF_SESSION_OPERATION_CLOSE);

    ASSERT_NE (current_lease, nullptr);
    ASSERT_NE (foreign_lease, nullptr);
    EXPECT_FALSE (gnc_clear_current_session_with_lease (foreign_lease));
    EXPECT_TRUE (gnc_current_session_exist ());
    EXPECT_EQ (gnc_get_current_session (), current);
    EXPECT_EQ (gnc_current_session_get_generation (), generation);

    observe_current_session_during_end = true;
    current_session_was_detached_on_end = false;
    EXPECT_TRUE (gnc_clear_current_session_with_lease (current_lease));
    observe_current_session_during_end = false;
    EXPECT_TRUE (current_session_was_detached_on_end);
    EXPECT_FALSE (gnc_current_session_exist ());
    EXPECT_NE (gnc_current_session_get_generation (), generation);
    EXPECT_EQ (qof_session_operation_lease_get_id (current_lease), 0u);

    qof_session_operation_lease_release (current_lease);
    qof_session_operation_lease_release (foreign_lease);
    qof_session_destroy (foreign);
    qof_backend_unregister_all_providers ();
}

TEST (QofSessionOperationLeaseTest, protects_session_operations)
{
    qof_backend_register_provider (get_provider ());
    auto session_1 = qof_session_new (qof_book_new ());
    auto session_2 = qof_session_new (qof_book_new ());
    auto lease_1 = qof_session_operation_lease_acquire (session_1);
    auto lease_2 = qof_session_operation_lease_acquire (session_2);
    ASSERT_NE (lease_1, nullptr);
    ASSERT_NE (lease_2, nullptr);

    EXPECT_FALSE (qof_session_begin_with_lease (
        session_1, lease_2, "book1", SESSION_NORMAL_OPEN));
    EXPECT_TRUE (qof_session_operation_lease_is_valid (lease_1, session_1));

    qof_session_begin (session_1, "book1", SESSION_NORMAL_OPEN);
    EXPECT_STREQ (qof_session_get_url (session_1), "");
    EXPECT_TRUE (qof_session_begin_with_lease (
        session_1, lease_1, "book1", SESSION_NORMAL_OPEN));
    EXPECT_STREQ (qof_session_get_url (session_1), "book1");

    load_error = false;
    ASSERT_TRUE (qof_session_load_with_lease (session_1, lease_1, nullptr));
    ASSERT_TRUE (qof_session_operation_lease_is_valid (lease_1, session_1));

    sync_called = false;
    qof_book_mark_session_dirty (qof_session_get_book (session_1));
    qof_session_save (session_1, nullptr);
    EXPECT_FALSE (sync_called);
    EXPECT_TRUE (qof_session_save_with_lease (session_1, lease_1, nullptr));
    EXPECT_TRUE (sync_called);

    safe_sync_called = false;
    qof_session_safe_save (session_1, nullptr);
    EXPECT_FALSE (safe_sync_called);
    EXPECT_TRUE (qof_session_safe_save_with_lease (
        session_1, lease_1, nullptr));
    EXPECT_TRUE (safe_sync_called);

    qof_session_end (session_1);
    EXPECT_STREQ (qof_session_get_url (session_1), "book1");
    EXPECT_TRUE (qof_session_end_with_lease (session_1, lease_1));
    EXPECT_STREQ (qof_session_get_url (session_1), "");

    EXPECT_FALSE (qof_session_destroy_with_lease (session_1, lease_2));
    EXPECT_TRUE (qof_session_operation_lease_is_valid (lease_1, session_1));
    EXPECT_TRUE (qof_session_destroy_with_lease (session_1, lease_1));
    EXPECT_EQ (qof_session_operation_lease_get_id (lease_1), 0u);
    qof_session_operation_lease_release (lease_1);

    qof_session_operation_lease_release (lease_2);
    qof_session_destroy (session_2);
    qof_backend_unregister_all_providers ();
    sync_called = false;
    safe_sync_called = false;
    load_error = true;
}

TEST (QofSessionOperationLeaseTest, current_lease_blocks_set_and_legacy_clear)
{
    if (gnc_current_session_exist ())
        gnc_clear_current_session ();

    auto session_1 = qof_session_new (qof_book_new ());
    auto session_2 = qof_session_new (qof_book_new ());
    gnc_set_current_session (session_1);
    auto generation = gnc_current_session_get_generation ();
    auto lease = qof_session_operation_lease_acquire (session_1);
    ASSERT_NE (lease, nullptr);

    gnc_set_current_session (session_1);
    EXPECT_EQ (gnc_current_session_get_generation (), generation);
    EXPECT_EQ (gnc_get_current_session (), session_1);

    gnc_set_current_session (session_2);
    gnc_clear_current_session ();
    EXPECT_EQ (gnc_current_session_get_generation (), generation);
    EXPECT_EQ (gnc_get_current_session (), session_1);
    EXPECT_TRUE (qof_session_operation_lease_is_valid (lease, session_1));

    qof_session_operation_lease_release (lease);
    gnc_set_current_session (session_2);
    EXPECT_NE (gnc_current_session_get_generation (), generation);
    EXPECT_EQ (gnc_get_current_session (), session_2);
    gnc_clear_current_session ();
    EXPECT_FALSE (gnc_current_session_exist ());
    qof_session_destroy (session_1);
}

TEST (QofSessionOperationLeaseTest, leased_replacement_cannot_become_current)
{
    if (gnc_current_session_exist ())
        gnc_clear_current_session ();

    auto current = qof_session_new (qof_book_new ());
    auto replacement = qof_session_new (qof_book_new ());
    gnc_set_current_session (current);
    auto generation = gnc_current_session_get_generation ();
    auto replacement_lease = qof_session_operation_lease_acquire (
        replacement);
    ASSERT_NE (replacement_lease, nullptr);

    gnc_set_current_session (replacement);
    EXPECT_EQ (gnc_current_session_get_generation (), generation);
    EXPECT_EQ (gnc_get_current_session (), current);
    EXPECT_TRUE (qof_session_operation_lease_is_valid (
        replacement_lease, replacement));

    qof_session_operation_lease_release (replacement_lease);
    gnc_set_current_session (replacement);
    EXPECT_NE (gnc_current_session_get_generation (), generation);
    EXPECT_EQ (gnc_get_current_session (), replacement);
    gnc_clear_current_session ();
    EXPECT_FALSE (gnc_current_session_exist ());
    qof_session_destroy (current);
}

TEST (QofSessionOperationLeaseTest, swap_requires_both_leases_and_invalidates_them)
{
    auto session_1 = qof_session_new (qof_book_new ());
    auto session_2 = qof_session_new (qof_book_new ());
    auto book_1 = qof_session_get_book (session_1);
    auto book_2 = qof_session_get_book (session_2);
    auto lease_1 = qof_session_operation_lease_acquire (session_1);
    auto lease_2 = qof_session_operation_lease_acquire (session_2);
    ASSERT_NE (lease_1, nullptr);
    ASSERT_NE (lease_2, nullptr);

    EXPECT_FALSE (qof_session_swap_data_with_leases (
        session_1, lease_2, session_2, lease_1));
    EXPECT_TRUE (qof_session_operation_lease_is_valid (lease_1, session_1));
    EXPECT_TRUE (qof_session_operation_lease_is_valid (lease_2, session_2));
    EXPECT_EQ (qof_session_get_book (session_1), book_1);
    EXPECT_EQ (qof_session_get_book (session_2), book_2);

    EXPECT_TRUE (qof_session_swap_data_with_leases (
        session_1, lease_1, session_2, lease_2));
    EXPECT_EQ (qof_session_get_book (session_1), book_2);
    EXPECT_EQ (qof_session_get_book (session_2), book_1);
    EXPECT_FALSE (qof_session_operation_lease_is_valid (lease_1, session_1));
    EXPECT_FALSE (qof_session_operation_lease_is_valid (lease_2, session_2));

    qof_session_operation_lease_release (lease_1);
    qof_session_operation_lease_release (lease_2);
    qof_session_destroy (session_1);
    qof_session_destroy (session_2);
}

TEST (QofSessionOperationLeaseTest, book_replacement_invalidates_lease)
{
    qof_backend_register_provider (get_provider ());
    auto session = qof_session_new (qof_book_new ());
    auto lease = qof_session_operation_lease_acquire (session);
    ASSERT_NE (lease, nullptr);
    ASSERT_TRUE (qof_session_begin_with_lease (
        session, lease, "book1", SESSION_NORMAL_OPEN));

    load_error = true;
    EXPECT_TRUE (qof_session_load_with_lease (session, lease, nullptr));
    EXPECT_FALSE (qof_session_operation_lease_is_valid (lease, session));
    EXPECT_EQ (qof_session_operation_lease_get_id (lease), 0u);

    qof_session_operation_lease_release (lease);
    qof_session_destroy (session);
    qof_backend_unregister_all_providers ();
}

TEST (QofSessionOperationLeaseTest, import_context_is_exclusive_and_reentrant)
{
    if (gnc_current_session_exist ())
        gnc_clear_current_session ();

    auto current = qof_session_new (qof_book_new ());
    auto foreign = qof_session_new (qof_book_new ());
    auto current_book = qof_session_get_book (current);
    auto foreign_book = qof_session_get_book (foreign);
    gnc_set_current_session (current);

    EXPECT_EQ (gnc_session_operation_context_new (
        foreign_book, QOF_SESSION_OPERATION_IMPORT), nullptr);
    auto context = gnc_session_operation_context_new (
        current_book, QOF_SESSION_OPERATION_IMPORT);
    ASSERT_NE (context, nullptr);
    EXPECT_TRUE (gnc_session_operation_context_is_current (context));

    ASSERT_TRUE (gnc_session_operation_context_begin (context));
    EXPECT_TRUE (gnc_session_operation_context_has_lease (context));
    EXPECT_TRUE (qof_session_has_active_operation_kind (
        current, QOF_SESSION_OPERATION_IMPORT));
    EXPECT_EQ (qof_session_operation_lease_acquire_for (
        current, QOF_SESSION_OPERATION_SAVE), nullptr);

    ASSERT_TRUE (gnc_session_operation_context_begin (context));
    gnc_session_operation_context_end (context);
    EXPECT_TRUE (gnc_session_operation_context_has_lease (context));
    gnc_session_operation_context_end (context);
    EXPECT_FALSE (gnc_session_operation_context_has_lease (context));
    EXPECT_FALSE (qof_session_has_active_operation_lease (current));
    EXPECT_TRUE (gnc_session_operation_context_is_current (context));

    ASSERT_TRUE (gnc_session_operation_context_begin (context));
    gnc_session_operation_context_end (context);
    EXPECT_FALSE (qof_session_has_active_operation_lease (current));

    gnc_session_operation_context_unref (context);
    gnc_clear_current_session ();
    qof_session_destroy (foreign);
}

TEST (QofSessionOperationLeaseTest,
      import_cleanup_rebases_only_same_current_book_and_is_terminal)
{
    if (gnc_current_session_exist ())
        gnc_clear_current_session ();

    auto current = qof_session_new (qof_book_new ());
    auto foreign = qof_session_new (qof_book_new ());
    auto current_book = qof_session_get_book (current);
    gnc_set_current_session (current);
    auto context = gnc_session_operation_context_new (
        current_book, QOF_SESSION_OPERATION_IMPORT);
    ASSERT_NE (context, nullptr);

    auto drift = qof_session_operation_lease_acquire_for (
        current, QOF_SESSION_OPERATION_SAVE);
    ASSERT_NE (drift, nullptr);
    EXPECT_FALSE (gnc_session_operation_context_begin_cleanup (context));
    EXPECT_FALSE (gnc_session_operation_context_is_current (context));
    EXPECT_FALSE (gnc_session_operation_context_begin (context));
    qof_session_operation_lease_release (drift);
    EXPECT_FALSE (gnc_session_operation_context_is_current (context));
    EXPECT_TRUE (gnc_session_operation_context_identifies_current_book (
        context));
    EXPECT_FALSE (gnc_session_operation_context_begin (context));

    ASSERT_TRUE (gnc_session_operation_context_begin_cleanup (context));
    EXPECT_TRUE (qof_session_has_active_operation_kind (
        current, QOF_SESSION_OPERATION_IMPORT));
    EXPECT_EQ (qof_session_operation_lease_acquire_for (
        current, QOF_SESSION_OPERATION_SAVE), nullptr);
    ASSERT_TRUE (gnc_session_operation_context_begin_cleanup (context));
    gnc_session_operation_context_end (context);
    EXPECT_TRUE (gnc_session_operation_context_has_lease (context));
    gnc_session_operation_context_end (context);
    EXPECT_FALSE (qof_session_has_active_operation_lease (current));

    EXPECT_FALSE (gnc_session_operation_context_is_current (context));
    EXPECT_FALSE (gnc_session_operation_context_begin (context));
    ASSERT_TRUE (gnc_session_operation_context_begin_cleanup (context));
    gnc_session_operation_context_end (context);
    EXPECT_FALSE (qof_session_has_active_operation_lease (current));

    gnc_set_current_session (foreign);
    EXPECT_FALSE (gnc_session_operation_context_identifies_current_book (
        context));
    EXPECT_FALSE (gnc_session_operation_context_begin_cleanup (context));
    EXPECT_FALSE (qof_session_has_active_operation_lease (current));
    EXPECT_FALSE (qof_session_has_active_operation_lease (foreign));

    gnc_session_operation_context_unref (context);
    gnc_clear_current_session ();
    qof_session_destroy (current);
}

TEST (QofSessionOperationLeaseTest, scrub_context_is_current_book_bound)
{
    if (gnc_current_session_exist ())
        gnc_clear_current_session ();

    auto current = qof_session_new (qof_book_new ());
    auto foreign = qof_session_new (qof_book_new ());
    auto current_book = qof_session_get_book (current);
    auto foreign_book = qof_session_get_book (foreign);
    gnc_set_current_session (current);
    auto generation = gnc_current_session_get_generation ();

    EXPECT_EQ (gnc_scrub_context_begin (foreign_book), nullptr);
    auto context = gnc_scrub_context_begin (current_book);
    ASSERT_NE (context, nullptr);
    EXPECT_TRUE (gnc_scrub_context_is_active (context));
    EXPECT_TRUE (gnc_scrub_context_owns_book (context, current_book));
    EXPECT_FALSE (gnc_scrub_context_owns_book (context, foreign_book));
    EXPECT_TRUE (qof_session_has_active_operation_kind (
        current, QOF_SESSION_OPERATION_SCRUB));
    EXPECT_EQ (qof_session_operation_lease_acquire_for (
        current, QOF_SESSION_OPERATION_SAVE), nullptr);
    EXPECT_FALSE (gnc_scrub_legacy_operation_allowed (
        current_book, "test scrub"));
    EXPECT_TRUE (gnc_scrub_legacy_operation_allowed (
        foreign_book, "test scrub"));

    gnc_scrub_context_cancel (context);
    EXPECT_TRUE (gnc_scrub_context_is_cancelled (context));
    gnc_set_current_session (foreign);
    EXPECT_EQ (gnc_get_current_session (), current);
    EXPECT_EQ (gnc_current_session_get_generation (), generation);
    EXPECT_TRUE (gnc_scrub_context_is_active (context));

    gnc_scrub_context_end (context);
    EXPECT_FALSE (gnc_scrub_context_is_active (context));
    EXPECT_FALSE (qof_session_has_active_operation_kind (
        current, QOF_SESSION_OPERATION_SCRUB));
    EXPECT_TRUE (gnc_scrub_legacy_operation_allowed (
        current_book, "test scrub"));
    gnc_scrub_context_unref (context);

    gnc_set_current_session (foreign);
    EXPECT_EQ (gnc_get_current_session (), foreign);
    gnc_clear_current_session ();
    qof_session_destroy (current);
}

TEST (QofSessionOperationLeaseTest, scrub_checks_do_not_create_current_session)
{
    if (gnc_current_session_exist ())
        gnc_clear_current_session ();

    auto generation = gnc_current_session_get_generation ();
    auto book = qof_book_new ();
    ASSERT_FALSE (gnc_current_session_exist ());

    EXPECT_EQ (gnc_scrub_context_begin (book), nullptr);
    EXPECT_FALSE (gnc_current_session_exist ());
    EXPECT_EQ (gnc_current_session_get_generation (), generation);
    EXPECT_TRUE (gnc_scrub_legacy_operation_allowed (
        book, "headless scrub"));
    EXPECT_FALSE (gnc_current_session_exist ());
    EXPECT_EQ (gnc_current_session_get_generation (), generation);

    qof_book_destroy (book);
}

TEST (QofSessionOperationLeaseTest, scrub_context_releases_exactly_once)
{
    if (gnc_current_session_exist ())
        gnc_clear_current_session ();

    auto session = qof_session_new (qof_book_new ());
    gnc_set_current_session (session);
    auto context = gnc_scrub_context_begin (qof_session_get_book (session));
    ASSERT_NE (context, nullptr);
    auto retained = gnc_scrub_context_ref (context);

    gnc_scrub_context_end (context);
    gnc_scrub_context_end (context);
    EXPECT_FALSE (gnc_scrub_context_is_active (context));
    EXPECT_FALSE (qof_session_has_active_operation_lease (session));
    gnc_scrub_context_unref (context);

    auto next_lease = qof_session_operation_lease_acquire_for (
        session, QOF_SESSION_OPERATION_SAVE);
    ASSERT_NE (next_lease, nullptr);
    qof_session_operation_lease_release (next_lease);
    gnc_clear_current_session ();
    auto generation = gnc_current_session_get_generation ();
    ASSERT_FALSE (gnc_current_session_exist ());

    EXPECT_FALSE (gnc_scrub_context_is_active (retained));
    EXPECT_FALSE (gnc_current_session_exist ());
    EXPECT_EQ (gnc_current_session_get_generation (), generation);
    gnc_scrub_context_unref (retained);
}

TEST (QofSessionOperationLeaseTest, orphan_scrub_job_is_incremental_and_matches_sync)
{
    if (gnc_current_session_exist ())
        gnc_clear_current_session ();

    auto session = qof_session_new (qof_book_new ());
    ScrubJobBook incremental {qof_session_get_book (session), 3};
    auto sync_book = qof_book_new ();
    ScrubJobBook synchronous {sync_book, 3};
    gnc_set_current_session (session);

    auto job = gnc_scrub_orphans_job_begin (incremental.account, FALSE);
    ASSERT_NE (job, nullptr);
    EXPECT_EQ (gnc_scrub_job_get_total (job), 3u);
    EXPECT_EQ (gnc_scrub_job_get_completed (job), 0u);
    EXPECT_TRUE (qof_session_has_active_operation_kind (
        session, QOF_SESSION_OPERATION_SCRUB));
    EXPECT_EQ (qof_session_operation_lease_acquire_for (
        session, QOF_SESSION_OPERATION_SAVE), nullptr);

    EXPECT_EQ (gnc_scrub_job_step (job, 1), GNC_SCRUB_JOB_RUNNING);
    EXPECT_EQ (gnc_scrub_job_get_completed (job), 1u);
    EXPECT_TRUE (qof_session_has_active_operation_kind (
        session, QOF_SESSION_OPERATION_SCRUB));
    EXPECT_EQ (gnc_scrub_job_step (job, 1), GNC_SCRUB_JOB_RUNNING);
    EXPECT_EQ (gnc_scrub_job_get_completed (job), 2u);
    EXPECT_EQ (gnc_scrub_job_step (job, 1), GNC_SCRUB_JOB_DONE);
    EXPECT_EQ (gnc_scrub_job_get_completed (job), 3u);
    EXPECT_FALSE (qof_session_has_active_operation_lease (session));
    auto incremental_destination = expect_orphans_scrubbed (incremental);

    xaccAccountScrubOrphans (synchronous.account, scrub_job_progress);
    auto synchronous_destination = expect_orphans_scrubbed (synchronous);
    ASSERT_NE (incremental_destination, nullptr);
    ASSERT_NE (synchronous_destination, nullptr);
    EXPECT_STREQ (xaccAccountGetName (incremental_destination),
                  xaccAccountGetName (synchronous_destination));

    gnc_scrub_job_free (job);
    gnc_clear_current_session ();
    qof_book_destroy (sync_book);
}

TEST (QofSessionOperationLeaseTest, orphan_scrub_job_cancel_releases_lease_once)
{
    if (gnc_current_session_exist ())
        gnc_clear_current_session ();

    auto session = qof_session_new (qof_book_new ());
    ScrubJobBook fixture {qof_session_get_book (session), 2};
    gnc_set_current_session (session);
    auto job = gnc_scrub_orphans_job_begin (fixture.account, FALSE);
    ASSERT_NE (job, nullptr);
    EXPECT_EQ (gnc_scrub_job_step (job, 1), GNC_SCRUB_JOB_RUNNING);

    gnc_scrub_job_cancel (job);
    EXPECT_EQ (gnc_scrub_job_get_state (job), GNC_SCRUB_JOB_CANCELLED);
    EXPECT_EQ (gnc_scrub_job_step (job, 1), GNC_SCRUB_JOB_CANCELLED);
    EXPECT_FALSE (qof_session_has_active_operation_lease (session));

    auto next_lease = qof_session_operation_lease_acquire_for (
        session, QOF_SESSION_OPERATION_SAVE);
    ASSERT_NE (next_lease, nullptr);
    qof_session_operation_lease_release (next_lease);
    gnc_scrub_job_cancel (job);
    EXPECT_FALSE (qof_session_has_active_operation_lease (session));

    gnc_scrub_job_free (job);
    gnc_clear_current_session ();
}

TEST (QofSessionOperationLeaseTest, orphan_scrub_job_zero_step_fails_and_releases_lease)
{
    if (gnc_current_session_exist ())
        gnc_clear_current_session ();

    auto session = qof_session_new (qof_book_new ());
    ScrubJobBook fixture {qof_session_get_book (session), 1};
    gnc_set_current_session (session);
    auto job = gnc_scrub_orphans_job_begin (fixture.account, FALSE);
    ASSERT_NE (job, nullptr);

    EXPECT_EQ (gnc_scrub_job_step (job, 0), GNC_SCRUB_JOB_FAILED);
    EXPECT_FALSE (qof_session_has_active_operation_lease (session));
    gnc_scrub_job_free (job);
    gnc_clear_current_session ();
}

TEST (QofSessionOperationLeaseTest, orphan_scrub_job_free_cancels_and_releases_lease_once)
{
    if (gnc_current_session_exist ())
        gnc_clear_current_session ();

    auto session = qof_session_new (qof_book_new ());
    ScrubJobBook fixture {qof_session_get_book (session), 2};
    gnc_set_current_session (session);
    auto job = gnc_scrub_orphans_job_begin (fixture.account, FALSE);
    ASSERT_NE (job, nullptr);
    EXPECT_EQ (gnc_scrub_job_step (job, 1), GNC_SCRUB_JOB_RUNNING);

    gnc_scrub_job_free (job);
    EXPECT_FALSE (qof_session_has_active_operation_lease (session));
    auto next_lease = qof_session_operation_lease_acquire_for (
        session, QOF_SESSION_OPERATION_SAVE);
    ASSERT_NE (next_lease, nullptr);
    qof_session_operation_lease_release (next_lease);
    gnc_clear_current_session ();
}

TEST (QofSessionOperationLeaseTest, orphan_scrub_job_prevents_session_or_book_staleness)
{
    if (gnc_current_session_exist ())
        gnc_clear_current_session ();

    auto current = qof_session_new (qof_book_new ());
    auto foreign = qof_session_new (qof_book_new ());
    auto current_book = qof_session_get_book (current);
    ScrubJobBook fixture {current_book, 1};
    gnc_set_current_session (current);
    auto job = gnc_scrub_orphans_job_begin (fixture.account, FALSE);
    ASSERT_NE (job, nullptr);

    gnc_set_current_session (foreign);
    EXPECT_EQ (gnc_get_current_session (), current);
    qof_session_swap_data (current, foreign);
    EXPECT_EQ (qof_session_get_book (current), current_book);
    EXPECT_EQ (gnc_scrub_job_step (job, 1), GNC_SCRUB_JOB_DONE);
    EXPECT_FALSE (qof_session_has_active_operation_lease (current));

    gnc_scrub_job_free (job);
    gnc_clear_current_session ();
    qof_session_destroy (foreign);
}

struct ImbalanceScrubJobBook
{
    QofBook *book;
    Account *root;
    Account *account;
    Account *descendant;
    gnc_commodity *currency;
    std::vector<Transaction *> transactions;

    ImbalanceScrubJobBook (QofBook *target_book, guint account_count,
                            guint descendant_count)
        : book {target_book}
        , root {gnc_account_create_root (book)}
        , account {xaccMallocAccount (book)}
        , descendant {xaccMallocAccount (book)}
        , currency {gnc_commodity_new (book, "Imbalance Test Currency",
                                       "CURRENCY", "IMB", "", 100)}
    {
        xaccAccountBeginEdit (account);
        xaccAccountSetName (account, "Imbalance account");
        xaccAccountSetType (account, ACCT_TYPE_BANK);
        xaccAccountSetCommodity (account, currency);
        gnc_account_append_child (root, account);
        xaccAccountCommitEdit (account);

        xaccAccountBeginEdit (descendant);
        xaccAccountSetName (descendant, "Imbalance descendant");
        xaccAccountSetType (descendant, ACCT_TYPE_BANK);
        xaccAccountSetCommodity (descendant, currency);
        gnc_account_append_child (account, descendant);
        xaccAccountCommitEdit (descendant);

        for (guint i = 0; i < account_count; ++i)
            add_imbalanced_transaction (account, i + 1);
        for (guint i = 0; i < descendant_count; ++i)
            add_imbalanced_transaction (descendant, i + account_count + 1);
    }

    void add_imbalanced_transaction (Account *target, guint value)
    {
        auto transaction = xaccMallocTransaction (book);
        auto split = xaccMallocSplit (book);
        auto amount = gnc_numeric_create (value, 1);
        xaccTransBeginEdit (transaction);
        xaccTransSetCurrency (transaction, currency);
        xaccSplitSetParent (split, transaction);
        xaccSplitSetAccount (split, target);
        split->value = amount;
        split->amount = amount;
        xaccTransCommitEdit (transaction);

        /* Commit registers the transaction but also repairs its imbalance.
         * Deliberately corrupt the committed data afterwards, as a loader can.
         * No further CommitEdit runs before the scrub under test. */
        GList *balance_node {nullptr};
        for (auto node = transaction->splits; node; node = node->next)
            if (node->data != split)
            {
                balance_node = node;
                break;
            }
        g_assert_nonnull (balance_node);
        auto balance_split = static_cast<Split *> (balance_node->data);
        transaction->splits = g_list_delete_link (transaction->splits,
                                                  balance_node);
        balance_split->parent = nullptr;
        xaccFreeSplit (balance_split);
        split->value = amount;
        split->amount = amount;
        transactions.push_back (transaction);
    }
};

static void
expect_imbalance_scrubbed (const ImbalanceScrubJobBook& fixture)
{
    for (auto transaction : fixture.transactions)
    {
        EXPECT_TRUE (xaccTransIsBalanced (transaction));
        EXPECT_TRUE (gnc_numeric_zero_p (xaccTransGetImbalanceValue (transaction)));
        EXPECT_EQ (xaccTransCountSplits (transaction), 2);
    }
}

TEST (QofSessionOperationLeaseTest,
      imbalance_scrub_job_is_incremental_matches_sync_and_includes_descendants)
{
    if (gnc_current_session_exist ())
        gnc_clear_current_session ();

    auto session = qof_session_new (qof_book_new ());
    ImbalanceScrubJobBook incremental {qof_session_get_book (session), 1, 2};
    auto sync_book = qof_book_new ();
    ImbalanceScrubJobBook synchronous {sync_book, 1, 2};
    gnc_set_current_session (session);

    for (auto transaction : incremental.transactions)
    {
        EXPECT_FALSE (xaccTransIsBalanced (transaction));
        EXPECT_FALSE (gnc_numeric_zero_p (xaccTransGetImbalanceValue (transaction)));
        EXPECT_EQ (xaccTransCountSplits (transaction), 1);
    }
    for (auto transaction : synchronous.transactions)
    {
        EXPECT_FALSE (xaccTransIsBalanced (transaction));
        EXPECT_FALSE (gnc_numeric_zero_p (xaccTransGetImbalanceValue (transaction)));
        EXPECT_EQ (xaccTransCountSplits (transaction), 1);
    }
    auto job = gnc_scrub_imbalance_job_begin (incremental.account, TRUE);
    ASSERT_NE (job, nullptr);
    EXPECT_EQ (gnc_scrub_job_get_kind (job), GNC_SCRUB_JOB_IMBALANCE);
    EXPECT_EQ (gnc_scrub_job_get_total (job), 3u);
    EXPECT_EQ (gnc_scrub_job_step (job, 1), GNC_SCRUB_JOB_RUNNING);
    EXPECT_EQ (gnc_scrub_job_get_completed (job), 1u);
    EXPECT_EQ (gnc_scrub_job_step (job, 1), GNC_SCRUB_JOB_RUNNING);
    EXPECT_EQ (gnc_scrub_job_step (job, 1), GNC_SCRUB_JOB_DONE);
    expect_imbalance_scrubbed (incremental);

    xaccAccountTreeScrubImbalance (synchronous.account, scrub_job_progress);
    expect_imbalance_scrubbed (synchronous);
    ASSERT_EQ (incremental.transactions.size (), synchronous.transactions.size ());
    for (size_t i = 0; i < incremental.transactions.size (); ++i)
        EXPECT_EQ (xaccTransCountSplits (incremental.transactions[i]),
                   xaccTransCountSplits (synchronous.transactions[i]));

    gnc_scrub_job_free (job);
    gnc_clear_current_session ();
    qof_book_destroy (sync_book);
}

TEST (QofSessionOperationLeaseTest,
      account_composite_scrub_job_runs_tree_phases_under_one_lease)
{
    if (gnc_current_session_exist ())
        gnc_clear_current_session ();

    auto session = qof_session_new (qof_book_new ());
    ImbalanceScrubJobBook incremental {qof_session_get_book (session), 1, 2};
    auto sync_book = qof_book_new ();
    ImbalanceScrubJobBook synchronous {sync_book, 1, 2};
    gnc_set_current_session (session);

    auto job = gnc_scrub_account_job_begin (incremental.account, TRUE);
    ASSERT_NE (job, nullptr);
    EXPECT_EQ (gnc_scrub_job_get_kind (job), GNC_SCRUB_JOB_ACCOUNT);
    EXPECT_EQ (gnc_scrub_job_get_phase (job), GNC_SCRUB_JOB_PHASE_ORPHANS);
    EXPECT_EQ (gnc_scrub_job_get_total (job), 6u);
    EXPECT_EQ (gnc_scrub_job_get_completed (job), 0u);

    EXPECT_EQ (gnc_scrub_job_step (job, 1), GNC_SCRUB_JOB_RUNNING);
    EXPECT_EQ (gnc_scrub_job_get_completed (job), 1u);
    EXPECT_EQ (gnc_scrub_job_get_phase (job), GNC_SCRUB_JOB_PHASE_ORPHANS);
    EXPECT_EQ (gnc_scrub_job_step (job, 1), GNC_SCRUB_JOB_RUNNING);
    EXPECT_EQ (gnc_scrub_job_get_completed (job), 2u);
    EXPECT_EQ (gnc_scrub_job_step (job, 1), GNC_SCRUB_JOB_RUNNING);
    EXPECT_EQ (gnc_scrub_job_get_completed (job), 3u);
    EXPECT_EQ (gnc_scrub_job_get_phase (job), GNC_SCRUB_JOB_PHASE_IMBALANCE);
    EXPECT_TRUE (qof_session_has_active_operation_kind (
        session, QOF_SESSION_OPERATION_SCRUB));
    EXPECT_EQ (qof_session_operation_lease_acquire_for (
        session, QOF_SESSION_OPERATION_SAVE), nullptr);

    EXPECT_EQ (gnc_scrub_job_step (job, 1), GNC_SCRUB_JOB_RUNNING);
    EXPECT_EQ (gnc_scrub_job_get_completed (job), 4u);
    EXPECT_EQ (gnc_scrub_job_step (job, 1), GNC_SCRUB_JOB_RUNNING);
    EXPECT_EQ (gnc_scrub_job_get_completed (job), 5u);
    EXPECT_EQ (gnc_scrub_job_step (job, 1), GNC_SCRUB_JOB_DONE);
    EXPECT_EQ (gnc_scrub_job_get_completed (job), 6u);
    EXPECT_FALSE (qof_session_has_active_operation_lease (session));
    expect_imbalance_scrubbed (incremental);

    xaccAccountTreeScrubOrphans (synchronous.account, scrub_job_progress);
    xaccAccountTreeScrubImbalance (synchronous.account, scrub_job_progress);
    expect_imbalance_scrubbed (synchronous);
    ASSERT_EQ (incremental.transactions.size (), synchronous.transactions.size ());
    for (size_t i = 0; i < incremental.transactions.size (); ++i)
        EXPECT_EQ (xaccTransCountSplits (incremental.transactions[i]),
                   xaccTransCountSplits (synchronous.transactions[i]));

    gnc_scrub_job_free (job);
    gnc_clear_current_session ();
    qof_book_destroy (sync_book);
}

TEST (QofSessionOperationLeaseTest,
      account_composite_scrub_job_excludes_descendants_when_requested)
{
    if (gnc_current_session_exist ())
        gnc_clear_current_session ();

    auto session = qof_session_new (qof_book_new ());
    ImbalanceScrubJobBook incremental {qof_session_get_book (session), 1, 1};
    auto sync_book = qof_book_new ();
    ImbalanceScrubJobBook synchronous {sync_book, 1, 1};
    gnc_set_current_session (session);

    auto job = gnc_scrub_account_job_begin (incremental.account, FALSE);
    ASSERT_NE (job, nullptr);
    EXPECT_EQ (gnc_scrub_job_get_total (job), 2u);
    EXPECT_EQ (gnc_scrub_job_step (job, 1), GNC_SCRUB_JOB_RUNNING);
    EXPECT_EQ (gnc_scrub_job_get_phase (job), GNC_SCRUB_JOB_PHASE_IMBALANCE);
    EXPECT_EQ (gnc_scrub_job_step (job, 1), GNC_SCRUB_JOB_DONE);
    EXPECT_TRUE (xaccTransIsBalanced (incremental.transactions[0]));
    EXPECT_FALSE (xaccTransIsBalanced (incremental.transactions[1]));

    xaccAccountScrubOrphans (synchronous.account, scrub_job_progress);
    xaccAccountScrubImbalance (synchronous.account, scrub_job_progress);
    EXPECT_TRUE (xaccTransIsBalanced (synchronous.transactions[0]));
    EXPECT_FALSE (xaccTransIsBalanced (synchronous.transactions[1]));
    EXPECT_EQ (xaccTransCountSplits (incremental.transactions[0]),
               xaccTransCountSplits (synchronous.transactions[0]));
    EXPECT_EQ (xaccTransCountSplits (incremental.transactions[1]),
               xaccTransCountSplits (synchronous.transactions[1]));

    gnc_scrub_job_free (job);
    gnc_clear_current_session ();
    qof_book_destroy (sync_book);
}

TEST (QofSessionOperationLeaseTest,
      account_composite_scrub_job_cancel_at_phase_boundary_releases_once)
{
    if (gnc_current_session_exist ())
        gnc_clear_current_session ();

    auto session = qof_session_new (qof_book_new ());
    ScrubJobBook fixture {qof_session_get_book (session), 1};
    gnc_set_current_session (session);

    auto job = gnc_scrub_account_job_begin (fixture.account, FALSE);
    ASSERT_NE (job, nullptr);
    EXPECT_EQ (gnc_scrub_job_step (job, 1), GNC_SCRUB_JOB_RUNNING);
    EXPECT_EQ (gnc_scrub_job_get_completed (job), 1u);
    EXPECT_EQ (gnc_scrub_job_get_phase (job), GNC_SCRUB_JOB_PHASE_IMBALANCE);
    EXPECT_TRUE (qof_session_has_active_operation_kind (
        session, QOF_SESSION_OPERATION_SCRUB));
    EXPECT_EQ (qof_session_operation_lease_acquire_for (
        session, QOF_SESSION_OPERATION_SAVE), nullptr);
    expect_orphans_scrubbed (fixture);

    gnc_scrub_job_cancel (job);
    EXPECT_EQ (gnc_scrub_job_get_state (job), GNC_SCRUB_JOB_CANCELLED);
    EXPECT_FALSE (qof_session_has_active_operation_lease (session));
    auto next_lease = qof_session_operation_lease_acquire_for (
        session, QOF_SESSION_OPERATION_SAVE);
    ASSERT_NE (next_lease, nullptr);
    qof_session_operation_lease_release (next_lease);
    gnc_scrub_job_cancel (job);
    EXPECT_FALSE (qof_session_has_active_operation_lease (session));

    gnc_scrub_job_free (job);
    gnc_clear_current_session ();
}

TEST (QofSessionOperationLeaseTest,
      transaction_split_cursor_is_bounded_and_cancellable)
{
    if (gnc_current_session_exist ())
        gnc_clear_current_session ();

    auto session = qof_session_new (qof_book_new ());
    ScrubJobBook fixture {qof_session_get_book (session), 1};
    gnc_set_current_session (session);
    auto context = gnc_scrub_context_begin (fixture.book);
    ASSERT_NE (context, nullptr);

    auto transaction = xaccSplitGetParent (fixture.orphan_splits.front ());
    ASSERT_NE (transaction, nullptr);
    auto cursor = gnc_transaction_split_cursor_begin (transaction, context);
    ASSERT_NE (cursor, nullptr);

    GncGUID guid;
    EXPECT_EQ (gnc_transaction_split_cursor_next (cursor, &guid),
               GNC_TRANSACTION_SPLIT_CURSOR_NEXT);
    EXPECT_TRUE (guid_equal (&guid, qof_instance_get_guid (
        QOF_INSTANCE (xaccTransGetSplit (transaction, 0)))));
    EXPECT_EQ (gnc_transaction_split_cursor_next (cursor, &guid),
               GNC_TRANSACTION_SPLIT_CURSOR_NEXT);
    EXPECT_TRUE (guid_equal (&guid, qof_instance_get_guid (
        QOF_INSTANCE (xaccTransGetSplit (transaction, 1)))));
    EXPECT_EQ (gnc_transaction_split_cursor_next (cursor, &guid),
               GNC_TRANSACTION_SPLIT_CURSOR_DONE);
    gnc_transaction_split_cursor_free (cursor);

    cursor = gnc_transaction_split_cursor_begin (transaction, context);
    ASSERT_NE (cursor, nullptr);
    xaccTransBeginEdit (transaction);
    xaccTransCommitEdit (transaction);
    EXPECT_EQ (gnc_transaction_split_cursor_next (cursor, &guid),
               GNC_TRANSACTION_SPLIT_CURSOR_STALE);
    gnc_transaction_split_cursor_free (cursor);

    cursor = gnc_transaction_split_cursor_begin (transaction, context);
    ASSERT_NE (cursor, nullptr);
    gnc_scrub_context_cancel (context);
    EXPECT_EQ (gnc_transaction_split_cursor_next (cursor, &guid),
               GNC_TRANSACTION_SPLIT_CURSOR_CANCELLED);
    gnc_transaction_split_cursor_free (cursor);
    gnc_scrub_context_unref (context);
    gnc_clear_current_session ();
}

TEST (QofSessionOperationLeaseTest,
      transaction_imbalance_collector_returns_nontrading_value_imbalance)
{
    if (gnc_current_session_exist ())
        gnc_clear_current_session ();

    auto session = qof_session_new (qof_book_new ());
    auto book = qof_session_get_book (session);
    ImbalanceScrubJobBook fixture {book, 1, 0};
    gnc_set_current_session (session);
    auto context = gnc_scrub_context_begin (book);
    ASSERT_NE (context, nullptr);
    auto transaction = fixture.transactions.front ();
    auto collector = gnc_transaction_imbalance_collector_begin (transaction, context);
    ASSERT_NE (collector, nullptr);

    auto split = xaccTransGetSplit (transaction, 0);
    ASSERT_NE (split, nullptr);
    EXPECT_TRUE (gnc_transaction_imbalance_collector_consume (collector, split));
    auto imbalance = gnc_transaction_imbalance_collector_finish (collector);
    ASSERT_NE (imbalance, nullptr);
    ASSERT_EQ (imbalance->next, nullptr);
    auto monetary = static_cast<gnc_monetary *> (imbalance->data);
    ASSERT_NE (monetary, nullptr);
    EXPECT_EQ (gnc_monetary_commodity (*monetary), fixture.currency);
    EXPECT_TRUE (gnc_numeric_equal (gnc_monetary_value (*monetary),
                                    xaccTransGetImbalanceValue (transaction)));

    gnc_monetary_list_free (imbalance);
    gnc_transaction_imbalance_collector_free (collector);

    collector = gnc_transaction_imbalance_collector_begin (transaction, context);
    ASSERT_NE (collector, nullptr);
    EXPECT_TRUE (gnc_transaction_imbalance_collector_consume (collector, split));
    xaccTransBeginEdit (transaction);
    xaccTransCommitEdit (transaction);
    EXPECT_FALSE (gnc_transaction_imbalance_collector_consume (collector, split));
    EXPECT_EQ (gnc_transaction_imbalance_collector_get_count (collector), 0u);
    EXPECT_EQ (gnc_transaction_imbalance_collector_finish (collector), nullptr);
    gnc_transaction_imbalance_collector_free (collector);

    collector = gnc_transaction_imbalance_collector_begin (transaction, context);
    ASSERT_NE (collector, nullptr);
    gnc_scrub_context_cancel (context);
    EXPECT_FALSE (gnc_transaction_imbalance_collector_consume (collector, split));
    EXPECT_EQ (gnc_transaction_imbalance_collector_finish (collector), nullptr);
    gnc_transaction_imbalance_collector_free (collector);
    gnc_scrub_context_unref (context);
    gnc_clear_current_session ();
}

TEST (QofSessionOperationLeaseTest,
      transaction_imbalance_collector_preserves_trading_encounter_order)
{
    if (gnc_current_session_exist ())
        gnc_clear_current_session ();

    auto session = qof_session_new (qof_book_new ());
    auto book = qof_session_get_book (session);
    auto root = gnc_account_create_root (book);
    auto currency_a = gnc_commodity_new (book, "Currency A", "CURRENCY", "TCA", "", 100);
    auto currency_b = gnc_commodity_new (book, "Currency B", "CURRENCY", "TCB", "", 100);
    auto account_a = xaccMallocAccount (book);
    auto account_b = xaccMallocAccount (book);
    xaccAccountSetCommodity (account_a, currency_a);
    xaccAccountSetCommodity (account_b, currency_b);
    gnc_account_append_child (root, account_a);
    gnc_account_append_child (root, account_b);
    qof_book_begin_edit (book);
    qof_instance_set (QOF_INSTANCE (book), "trading-accts", "t", nullptr);
    qof_book_commit_edit (book);

    auto transaction = xaccMallocTransaction (book);
    auto split_a = xaccMallocSplit (book);
    auto split_b = xaccMallocSplit (book);
    transaction->common_currency = currency_a;
    split_a->parent = transaction;
    split_a->acc = account_a;
    split_a->amount = gnc_numeric_create (1, 1);
    split_a->value = gnc_numeric_create (1, 1);
    split_b->parent = transaction;
    split_b->acc = account_b;
    split_b->amount = gnc_numeric_create (-2, 1);
    split_b->value = gnc_numeric_create (-1, 1);
    transaction->splits = g_list_append (transaction->splits, split_a);
    transaction->splits = g_list_append (transaction->splits, split_b);

    gnc_set_current_session (session);
    auto context = gnc_scrub_context_begin (book);
    ASSERT_NE (context, nullptr);
    auto collector = gnc_transaction_imbalance_collector_begin (transaction, context);
    ASSERT_NE (collector, nullptr);
    EXPECT_TRUE (gnc_transaction_imbalance_collector_consume (collector, split_a));
    EXPECT_TRUE (gnc_transaction_imbalance_collector_consume (collector, split_b));

    GncGUID commodity_guid;
    gnc_numeric amount;
    gnc_numeric value;
    ASSERT_EQ (gnc_transaction_imbalance_collector_get_count (collector), 2u);
    ASSERT_TRUE (gnc_transaction_imbalance_collector_get_entry (
        collector, 0, &commodity_guid, &amount, &value));
    EXPECT_TRUE (guid_equal (&commodity_guid, qof_instance_get_guid (QOF_INSTANCE (currency_a))));
    EXPECT_TRUE (gnc_numeric_equal (amount, gnc_numeric_create (1, 1)));
    EXPECT_TRUE (gnc_numeric_equal (value, gnc_numeric_create (1, 1)));
    ASSERT_TRUE (gnc_transaction_imbalance_collector_get_entry (
        collector, 1, &commodity_guid, &amount, &value));
    EXPECT_TRUE (guid_equal (&commodity_guid, qof_instance_get_guid (QOF_INSTANCE (currency_b))));
    EXPECT_TRUE (gnc_numeric_equal (amount, gnc_numeric_create (-2, 1)));
    EXPECT_TRUE (gnc_numeric_equal (value, gnc_numeric_create (-1, 1)));

    auto imbalance = gnc_transaction_imbalance_collector_finish (collector);
    ASSERT_NE (imbalance, nullptr);
    auto first = static_cast<gnc_monetary *> (imbalance->data);
    auto second = static_cast<gnc_monetary *> (imbalance->next->data);
    ASSERT_NE (first, nullptr);
    ASSERT_NE (second, nullptr);
    EXPECT_EQ (gnc_monetary_commodity (*first), currency_b);
    EXPECT_TRUE (gnc_numeric_equal (gnc_monetary_value (*first),
                                    gnc_numeric_create (-2, 1)));
    EXPECT_EQ (gnc_monetary_commodity (*second), currency_a);
    EXPECT_TRUE (gnc_numeric_equal (gnc_monetary_value (*second),
                                    gnc_numeric_create (1, 1)));

    gnc_monetary_list_free (imbalance);
    gnc_transaction_imbalance_collector_free (collector);
    gnc_scrub_context_unref (context);
    gnc_clear_current_session ();
}

TEST (QofSessionOperationLeaseTest,
      commit_deferral_is_off_until_explicitly_enabled)
{
    if (gnc_current_session_exist ())
        gnc_clear_current_session ();

    ScopedEnvironment lots {"GNC_AUTO_SCRUB_LOTS", nullptr};
    auto session = qof_session_new (qof_book_new ());
    auto book = qof_session_get_book (session);
    CommitDeferralBook fixture {book};
    gnc_set_current_session (session);
    auto context = gnc_scrub_context_begin (book);
    ASSERT_NE (context, nullptr);

    auto transaction = fixture.commit_unbalanced (1);
    EXPECT_TRUE (xaccTransIsBalanced (transaction));
    EXPECT_EQ (xaccTransCountSplits (transaction), 2);
    EXPECT_EQ (gnc_scrub_deferred_commit_pending_count (
                   context, GNC_SCRUB_DEFERRED_COMMIT_IMBALANCE), 0u);
    EXPECT_EQ (gnc_scrub_deferred_commit_pending_count (
                   context, GNC_SCRUB_DEFERRED_COMMIT_GAINS), 0u);

    gnc_scrub_context_end (context);
    gnc_scrub_context_unref (context);
    gnc_clear_current_session ();
}

TEST (QofSessionOperationLeaseTest,
      commit_deferral_kinds_are_enabled_independently)
{
    if (gnc_current_session_exist ())
        gnc_clear_current_session ();

    ScopedEnvironment lots_on {"GNC_AUTO_SCRUB_LOTS", "1"};
    {
        auto session = qof_session_new (qof_book_new ());
        auto book = qof_session_get_book (session);
        CommitDeferralBook fixture {book};
        gnc_set_current_session (session);
        auto context = gnc_scrub_context_begin (book);
        ASSERT_NE (context, nullptr);
        ASSERT_TRUE (gnc_scrub_context_enable_commit_deferral (
            context, GNC_SCRUB_DEFERRED_COMMIT_GAINS));

        auto transaction = fixture.commit_unbalanced (1);
        EXPECT_TRUE (xaccTransIsBalanced (transaction));
        EXPECT_EQ (xaccTransCountSplits (transaction), 2);
        EXPECT_EQ (gnc_scrub_deferred_commit_pending_count (
                       context, GNC_SCRUB_DEFERRED_COMMIT_IMBALANCE), 0u);
        EXPECT_EQ (gnc_scrub_deferred_commit_pending_count (
                       context, GNC_SCRUB_DEFERRED_COMMIT_GAINS), 1u);

        GncGUID guid;
        ASSERT_TRUE (gnc_scrub_deferred_commit_peek (
            context, GNC_SCRUB_DEFERRED_COMMIT_GAINS, &guid));
        EXPECT_TRUE (guid_equal (&guid, xaccTransGetGUID (transaction)));
        EXPECT_TRUE (gnc_scrub_deferred_commit_ack (
            context, GNC_SCRUB_DEFERRED_COMMIT_GAINS, &guid));

        gnc_scrub_context_end (context);
        gnc_scrub_context_unref (context);
        gnc_clear_current_session ();
    }

    {
        auto session = qof_session_new (qof_book_new ());
        auto book = qof_session_get_book (session);
        CommitDeferralBook fixture {book};
        gnc_set_current_session (session);
        auto context = gnc_scrub_context_begin (book);
        ASSERT_NE (context, nullptr);
        ASSERT_TRUE (gnc_scrub_context_enable_commit_deferral (
            context, GNC_SCRUB_DEFERRED_COMMIT_IMBALANCE));

        auto transaction = fixture.commit_unbalanced (2);
        EXPECT_FALSE (xaccTransIsBalanced (transaction));
        EXPECT_EQ (xaccTransCountSplits (transaction), 1);
        EXPECT_EQ (gnc_scrub_deferred_commit_pending_count (
                       context, GNC_SCRUB_DEFERRED_COMMIT_IMBALANCE), 1u);
        EXPECT_EQ (gnc_scrub_deferred_commit_pending_count (
                       context, GNC_SCRUB_DEFERRED_COMMIT_GAINS), 0u);
        EXPECT_FALSE (gnc_scrub_defer_commit_hook (
            book, xaccTransGetGUID (transaction),
            GNC_SCRUB_DEFERRED_COMMIT_GAINS));

        GncGUID guid;
        ASSERT_TRUE (gnc_scrub_deferred_commit_peek (
            context, GNC_SCRUB_DEFERRED_COMMIT_IMBALANCE, &guid));
        EXPECT_TRUE (gnc_scrub_deferred_commit_ack (
            context, GNC_SCRUB_DEFERRED_COMMIT_IMBALANCE, &guid));

        gnc_scrub_context_end (context);
        gnc_scrub_context_unref (context);
        gnc_clear_current_session ();
    }
}

TEST (QofSessionOperationLeaseTest,
      commit_deferral_is_fifo_deduped_and_survives_context_handoff)
{
    if (gnc_current_session_exist ())
        gnc_clear_current_session ();

    ScopedEnvironment lots_off {"GNC_AUTO_SCRUB_LOTS", nullptr};
    auto session = qof_session_new (qof_book_new ());
    auto book = qof_session_get_book (session);
    CommitDeferralBook fixture {book};
    gnc_set_current_session (session);
    auto context = gnc_scrub_context_begin (book);
    ASSERT_NE (context, nullptr);
    ASSERT_TRUE (gnc_scrub_context_enable_commit_deferral (
        context, GNC_SCRUB_DEFERRED_COMMIT_IMBALANCE));
    ASSERT_TRUE (gnc_scrub_context_enable_commit_deferral (
        context, GNC_SCRUB_DEFERRED_COMMIT_GAINS));

    auto first = fixture.commit_unbalanced (1);
    auto second = fixture.commit_unbalanced (2);
    EXPECT_FALSE (xaccTransIsBalanced (first));
    EXPECT_EQ (xaccTransCountSplits (first), 1);
    EXPECT_EQ (gnc_scrub_deferred_commit_pending_count (
                   context, GNC_SCRUB_DEFERRED_COMMIT_IMBALANCE), 2u);

    xaccTransBeginEdit (first);
    xaccTransCommitEdit (first);
    EXPECT_EQ (gnc_scrub_deferred_commit_pending_count (
                   context, GNC_SCRUB_DEFERRED_COMMIT_IMBALANCE), 2u);

    GncGUID guid;
    auto first_guid = *xaccTransGetGUID (first);
    auto second_guid = *xaccTransGetGUID (second);
    ASSERT_TRUE (gnc_scrub_deferred_commit_peek (
        context, GNC_SCRUB_DEFERRED_COMMIT_IMBALANCE, &guid));
    EXPECT_TRUE (guid_equal (&guid, &first_guid));
    EXPECT_FALSE (gnc_scrub_deferred_commit_ack (
        context, GNC_SCRUB_DEFERRED_COMMIT_IMBALANCE, &second_guid));
    EXPECT_TRUE (gnc_scrub_deferred_commit_ack (
        context, GNC_SCRUB_DEFERRED_COMMIT_IMBALANCE, &first_guid));
    ASSERT_TRUE (gnc_scrub_deferred_commit_peek (
        context, GNC_SCRUB_DEFERRED_COMMIT_IMBALANCE, &guid));
    EXPECT_TRUE (guid_equal (&guid, &second_guid));

    Transaction *third {nullptr};
    {
        ScopedEnvironment lots_on {"GNC_AUTO_SCRUB_LOTS", "1"};
        third = fixture.commit_unbalanced (3);
        EXPECT_EQ (gnc_scrub_deferred_commit_pending_count (
                       context, GNC_SCRUB_DEFERRED_COMMIT_IMBALANCE), 2u);
        EXPECT_EQ (gnc_scrub_deferred_commit_pending_count (
                       context, GNC_SCRUB_DEFERRED_COMMIT_GAINS), 1u);
        ASSERT_TRUE (gnc_scrub_deferred_commit_peek (
            context, GNC_SCRUB_DEFERRED_COMMIT_GAINS, &guid));
        EXPECT_TRUE (guid_equal (&guid, xaccTransGetGUID (third)));
        EXPECT_TRUE (gnc_scrub_deferred_commit_ack (
            context, GNC_SCRUB_DEFERRED_COMMIT_GAINS, &guid));
    }

    EXPECT_TRUE (gnc_scrub_deferred_commit_ack (
        context, GNC_SCRUB_DEFERRED_COMMIT_IMBALANCE, &second_guid));
    auto third_guid = *xaccTransGetGUID (third);
    ASSERT_TRUE (gnc_scrub_deferred_commit_peek (
        context, GNC_SCRUB_DEFERRED_COMMIT_IMBALANCE, &guid));
    EXPECT_TRUE (guid_equal (&guid, &third_guid));

    auto foreign_book = qof_book_new ();
    EXPECT_FALSE (gnc_scrub_defer_commit_hook (
        foreign_book, &third_guid, GNC_SCRUB_DEFERRED_COMMIT_IMBALANCE));
    qof_book_destroy (foreign_book);

    gnc_scrub_context_cancel (context);
    EXPECT_FALSE (gnc_scrub_deferred_commit_peek (
        context, GNC_SCRUB_DEFERRED_COMMIT_IMBALANCE, &guid));
    EXPECT_EQ (gnc_scrub_deferred_commit_pending_count (
                   context, GNC_SCRUB_DEFERRED_COMMIT_IMBALANCE), 0u);
    gnc_scrub_context_end (context);
    EXPECT_FALSE (gnc_scrub_context_enable_commit_deferral (
        context, GNC_SCRUB_DEFERRED_COMMIT_IMBALANCE));
    gnc_scrub_context_unref (context);

    auto later_context = gnc_scrub_context_begin (book);
    ASSERT_NE (later_context, nullptr);
    EXPECT_EQ (gnc_scrub_deferred_commit_pending_count (
                   later_context, GNC_SCRUB_DEFERRED_COMMIT_IMBALANCE), 1u);
    ASSERT_TRUE (gnc_scrub_deferred_commit_peek (
        later_context, GNC_SCRUB_DEFERRED_COMMIT_IMBALANCE, &guid));
    EXPECT_TRUE (guid_equal (&guid, &third_guid));
    EXPECT_TRUE (gnc_scrub_deferred_commit_ack (
        later_context, GNC_SCRUB_DEFERRED_COMMIT_IMBALANCE, &guid));
    EXPECT_EQ (gnc_scrub_deferred_commit_pending_count (
                   later_context, GNC_SCRUB_DEFERRED_COMMIT_IMBALANCE), 0u);

    gnc_scrub_context_end (later_context);
    gnc_scrub_context_unref (later_context);
    gnc_clear_current_session ();
}

TEST (QofSessionOperationLeaseTest,
      data_scrub_suspension_is_book_scoped_and_default_off)
{
    if (gnc_current_session_exist ())
        gnc_clear_current_session ();

    ScopedEnvironment lots {"GNC_AUTO_SCRUB_LOTS", nullptr};
    auto session = qof_session_new (qof_book_new ());
    auto book_a = qof_session_get_book (session);
    auto book_b = qof_book_new ();
    CommitDeferralBook fixture_a {book_a};
    CommitDeferralBook fixture_b {book_b};
    gnc_set_current_session (session);

    auto before_suspension = fixture_a.commit_unbalanced (1);
    EXPECT_TRUE (xaccTransIsBalanced (before_suspension));
    EXPECT_EQ (xaccTransCountSplits (before_suspension), 2);

    auto suspension = xaccDataScrubSuspendForBook (book_a);
    ASSERT_NE (suspension, nullptr);
    EXPECT_TRUE (xaccDataScrubbingSuspendedForBook (book_a));
    EXPECT_FALSE (xaccDataScrubbingSuspendedForBook (book_b));

    auto transaction_a = fixture_a.commit_unbalanced (2);
    auto transaction_b = fixture_b.commit_unbalanced (3);
    EXPECT_FALSE (xaccTransIsBalanced (transaction_a));
    EXPECT_EQ (xaccTransCountSplits (transaction_a), 1);
    EXPECT_TRUE (xaccTransIsBalanced (transaction_b));
    EXPECT_EQ (xaccTransCountSplits (transaction_b), 2);

    xaccDataScrubSuspensionRelease (suspension);
    EXPECT_FALSE (xaccDataScrubbingSuspendedForBook (book_a));
    auto after_release = fixture_a.commit_unbalanced (4);
    EXPECT_TRUE (xaccTransIsBalanced (after_release));
    EXPECT_EQ (xaccTransCountSplits (after_release), 2);

    gnc_clear_current_session ();
    qof_book_destroy (book_b);
}

TEST (QofSessionOperationLeaseTest,
      data_scrub_suspension_is_nested_and_global_switch_remains_global)
{
    if (gnc_current_session_exist ())
        gnc_clear_current_session ();

    ScopedEnvironment lots {"GNC_AUTO_SCRUB_LOTS", nullptr};
    auto session = qof_session_new (qof_book_new ());
    auto book_a = qof_session_get_book (session);
    auto book_b = qof_book_new ();
    CommitDeferralBook fixture_a {book_a};
    CommitDeferralBook fixture_b {book_b};
    gnc_set_current_session (session);

    auto outer = xaccDataScrubSuspendForBook (book_a);
    auto inner = xaccDataScrubSuspendForBook (book_a);
    ASSERT_NE (outer, nullptr);
    ASSERT_NE (inner, nullptr);
    auto while_both_held = fixture_a.commit_unbalanced (1);
    EXPECT_FALSE (xaccTransIsBalanced (while_both_held));

    xaccDataScrubSuspensionRelease (inner);
    EXPECT_TRUE (xaccDataScrubbingSuspendedForBook (book_a));
    auto while_outer_held = fixture_a.commit_unbalanced (2);
    EXPECT_FALSE (xaccTransIsBalanced (while_outer_held));

    xaccDataScrubSuspensionRelease (outer);
    EXPECT_FALSE (xaccDataScrubbingSuspendedForBook (book_a));
    auto after_nested_release = fixture_a.commit_unbalanced (3);
    EXPECT_TRUE (xaccTransIsBalanced (after_nested_release));

    {
        ScopedDataScrubbingDisabled globally_disabled;
        auto disabled_a = fixture_a.commit_unbalanced (4);
        auto disabled_b = fixture_b.commit_unbalanced (5);
        EXPECT_FALSE (xaccTransIsBalanced (disabled_a));
        EXPECT_FALSE (xaccTransIsBalanced (disabled_b));
    }

    auto globally_reenabled_a = fixture_a.commit_unbalanced (6);
    auto globally_reenabled_b = fixture_b.commit_unbalanced (7);
    EXPECT_TRUE (xaccTransIsBalanced (globally_reenabled_a));
    EXPECT_TRUE (xaccTransIsBalanced (globally_reenabled_b));

    gnc_clear_current_session ();
    qof_book_destroy (book_b);
}

TEST (QofSessionOperationLeaseTest,
      data_scrub_suspension_handles_null_and_book_destroy_before_release)
{
    EXPECT_EQ (xaccDataScrubSuspendForBook (nullptr), nullptr);
    EXPECT_FALSE (xaccDataScrubbingSuspendedForBook (nullptr));
    xaccDataScrubSuspensionRelease (nullptr);

    auto book = qof_book_new ();
    auto suspension = xaccDataScrubSuspendForBook (book);
    ASSERT_NE (suspension, nullptr);
    EXPECT_TRUE (xaccDataScrubbingSuspendedForBook (book));
    qof_book_destroy (book);
    xaccDataScrubSuspensionRelease (suspension);
}

TEST (QofSessionOperationLeaseTest,
      commit_deferral_survives_lease_end_without_cancellation)
{
    if (gnc_current_session_exist ())
        gnc_clear_current_session ();

    ScopedEnvironment lots {"GNC_AUTO_SCRUB_LOTS", nullptr};
    auto session = qof_session_new (qof_book_new ());
    auto book = qof_session_get_book (session);
    CommitDeferralBook fixture {book};
    gnc_set_current_session (session);
    auto context = gnc_scrub_context_begin (book);
    ASSERT_NE (context, nullptr);
    ASSERT_TRUE (gnc_scrub_context_enable_commit_deferral (
        context, GNC_SCRUB_DEFERRED_COMMIT_IMBALANCE));

    auto transaction = fixture.commit_unbalanced (1);
    auto transaction_guid = *xaccTransGetGUID (transaction);
    EXPECT_EQ (gnc_scrub_deferred_commit_pending_count (
                   context, GNC_SCRUB_DEFERRED_COMMIT_IMBALANCE), 1u);
    gnc_scrub_context_end (context);

    GncGUID guid;
    EXPECT_FALSE (gnc_scrub_deferred_commit_peek (
        context, GNC_SCRUB_DEFERRED_COMMIT_IMBALANCE, &guid));
    gnc_scrub_context_unref (context);

    auto later_context = gnc_scrub_context_begin (book);
    ASSERT_NE (later_context, nullptr);
    EXPECT_EQ (gnc_scrub_deferred_commit_pending_count (
                   later_context, GNC_SCRUB_DEFERRED_COMMIT_IMBALANCE), 1u);
    ASSERT_TRUE (gnc_scrub_deferred_commit_peek (
        later_context, GNC_SCRUB_DEFERRED_COMMIT_IMBALANCE, &guid));
    EXPECT_TRUE (guid_equal (&guid, &transaction_guid));
    EXPECT_TRUE (gnc_scrub_deferred_commit_ack (
        later_context, GNC_SCRUB_DEFERRED_COMMIT_IMBALANCE, &guid));

    gnc_scrub_context_end (later_context);
    gnc_scrub_context_unref (later_context);
    gnc_clear_current_session ();
}
