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

#include <gtest/gtest.h>
#include "../guid.hpp"
#include <qofsession.hpp>
#include <qof-backend.hpp>
#include <cstdlib>
#include "../gnc-backend-prov.hpp"
#include "../Account.h"

static QofBook * exported_book {nullptr};
static bool safe_sync_called {false};
static bool sync_called {false};
static bool load_error {true};
static bool hook_called {false};
static bool data_loaded {false};

class QofSessionMockBackend : public QofBackend
{
public:
    QofSessionMockBackend() = default;
    QofSessionMockBackend(const QofSessionMockBackend&) = delete;
    QofSessionMockBackend(const QofSessionMockBackend&&) = delete;
    virtual ~QofSessionMockBackend() = default;
    void session_begin(QofSession*, const char*, bool, bool, bool) {}
    void session_end() {}
    void load(QofBook*, QofBackendLoadType);
    void sync(QofBook*);
    void safe_sync(QofBook*);
    void export_coa(QofBook*);
};

static void
example_hook (QofSession & session)
{
    hook_called = true;
}

void QofSessionMockBackend::load (QofBook *book, QofBackendLoadType)
{
    if (load_error)
        set_error(ERR_BACKEND_NO_BACKEND);
    else
        auto root = gnc_account_create_root (book);
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

TEST (QofSessionTest, swap_books)
{
    qof_backend_register_provider (get_provider ());
    QofSession s1;
    s1.begin ("book1", false, false, false);
    QofSession s2;
    s2.begin ("book2", false, false, false);
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
    QofSession s;
    s.begin ("book1", false, false, false);
    data_loaded = false;
    s.ensure_all_data_loaded ();
    EXPECT_EQ (data_loaded, true);
    qof_backend_unregister_all_providers ();
}

TEST (QofSessionTest, get_error)
{
    qof_backend_register_provider (get_provider ());
    QofSession s;
    s.begin ("book1", false, false, false);
    s.ensure_all_data_loaded ();
    EXPECT_NE (s.get_error (), ERR_BACKEND_NO_ERR);
    //get_error should not clear the error.
    EXPECT_NE (s.get_error (), ERR_BACKEND_NO_ERR);
    qof_backend_unregister_all_providers ();
}

TEST (QofSessionTest, pop_error)
{
    qof_backend_register_provider (get_provider ());
    QofSession s;
    s.begin ("book1", false, false, false);
    //We run the test first, and make sure there is an error condition.
    s.ensure_all_data_loaded ();
    EXPECT_NE (s.pop_error (), ERR_BACKEND_NO_ERR);
    EXPECT_EQ (s.get_error (), ERR_BACKEND_NO_ERR);
    qof_backend_unregister_all_providers ();
}

TEST (QofSessionTest, clear_error)
{
    qof_backend_register_provider (get_provider ());
    QofSession s;
    s.begin ("book1", false, false, false);
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
    QofSession s;
    s.begin ("book1", false, false, false);
    auto book = s.get_book ();
    s.load (nullptr);
    EXPECT_NE (book, s.get_book ());

    /* Now we'll do the load without returning an error from the backend,
     * and ensure that it's the new book from the previous test.
     */
    load_error = false;
    book = s.get_book();
    s.load (nullptr);
    EXPECT_EQ (book, s.get_book ());
    EXPECT_EQ (s.get_error(), ERR_BACKEND_NO_ERR);
    //But it's still empty, to the book shouldn't need saving
    EXPECT_FALSE(qof_book_session_not_saved (s.get_book ()));
    // I'll put load_error back just to be tidy.
    load_error = true;
    qof_backend_unregister_all_providers ();
}

TEST (QofSessionTest, save)
{
    qof_backend_register_provider (get_provider ());
    QofSession s;
    s.begin ("book1", false, false, false);
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
    QofSession s;
    s.begin ("book1", false, false, false);
    s.safe_save (nullptr);
    EXPECT_EQ (safe_sync_called, true);
    qof_backend_unregister_all_providers ();
    safe_sync_called = false;
}

TEST (QofSessionTest, export_session)
{
    qof_backend_register_provider (get_provider ());
    QofSession s1;
    s1.begin ("book1", false, false, false);
    QofSession s2;
    s2.begin ("book2", false, false, false);
    QofBook * b1 = s1.get_book ();
    QofBook * b2 = s2.get_book ();
    b1->backend = s1.get_backend ();
    b2->backend = s2.get_backend ();
    s2.export_session (s1, nullptr);
    EXPECT_EQ (exported_book, b1);

    qof_backend_unregister_all_providers ();
}
