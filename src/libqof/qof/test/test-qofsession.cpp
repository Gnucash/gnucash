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
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html            *
 * or contact:                                                      *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 ********************************************************************/

#include <gtest/gtest.h>
#include <qofsession.hpp>
#include "qofbackend-p.h"
#include <cstdlib>

static std::vector<QofBackendProvider*> providers;
static QofBook * exported_book {nullptr};
static bool safe_sync_called {false};
static bool sync_called {false};
static bool load_error {true};
static bool hook_called {false};
static bool data_loaded {false};
void example_hook (QofSession & session)
{
    hook_called = true;
}

void test_load (QofBackend * be, QofBook *, QofBackendLoadType)
{
    if (load_error) be->last_err = ERR_BACKEND_NO_BACKEND;
    data_loaded = true;
}

void test_safe_sync (QofBackend *, QofBook *)
{
    safe_sync_called = true;
}

void test_sync (QofBackend *, QofBook *)
{
    sync_called = true;
}

void test_export_fn (QofBackend *, QofBook * book)
{
    exported_book = book;
    std::cout << "exported book called\n" << std::flush;
}

QofBackend * test_backend_factory ()
{
    QofBackend * ret = (QofBackend*) std::malloc (sizeof (QofBackend));
    std::cout << "getting backend " << ret << " ...\n";
    ret->session_begin = nullptr;
    ret->session_end = nullptr;
    ret->destroy_backend = nullptr;
    ret->load = &test_load;
    ret->sync = &test_sync;
    ret->safe_sync = &test_safe_sync;
    ret->export_fn = &test_export_fn;
    ret->error_msg = nullptr;
    ret->fullpath = nullptr;
    ret->last_err = ERR_BACKEND_NO_ERR;
    return ret;
}

void test_free_provider (QofBackendProvider * provider)
{
    std::free (provider);
}

QofBackendProvider * get_provider ()
{
    QofBackendProvider * ret = (QofBackendProvider*) std::malloc (sizeof (QofBackendProvider));
    if (!ret) return ret;
    ret->provider_name = "test provider";
    ret->access_method = "file";
    ret->backend_new = &test_backend_factory;
    ret->provider_free = &test_free_provider;
    ret->check_data_type = nullptr;
    return ret;
}

void register_provider (void)
{
    auto prov = get_provider ();
    qof_backend_register_provider (prov);
    providers.push_back (prov);
}

void unregister_providers (void)
{
    for (auto prov : providers)
        qof_backend_unregister_provider (prov);
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
    unregister_providers ();
}

TEST (QofSessionTest, ensure_all_data_loaded)
{
    register_provider ();
    QofSession s;
    s.begin ("book1", false, false, false);
    data_loaded = false;
    s.ensure_all_data_loaded ();
    EXPECT_EQ (data_loaded, true);
    unregister_providers ();
}

TEST (QofSessionTest, get_error)
{
    register_provider ();
    QofSession s;
    s.begin ("book1", false, false, false);
    s.ensure_all_data_loaded ();
    EXPECT_NE (s.get_error (), ERR_BACKEND_NO_ERR);
    //get_error should not clear the error.
    EXPECT_NE (s.get_error (), ERR_BACKEND_NO_ERR);
    unregister_providers ();
}

TEST (QofSessionTest, pop_error)
{
    register_provider ();
    QofSession s;
    s.begin ("book1", false, false, false);
    //We run the test first, and make sure there is an error condition.
    s.ensure_all_data_loaded ();
    EXPECT_NE (s.pop_error (), ERR_BACKEND_NO_ERR);
    EXPECT_EQ (s.get_error (), ERR_BACKEND_NO_ERR);
    unregister_providers ();
}

TEST (QofSessionTest, clear_error)
{
    register_provider ();
    QofSession s;
    s.begin ("book1", false, false, false);
    //We run the test first, and make sure there is an error condition.
    s.ensure_all_data_loaded ();
    EXPECT_NE (s.get_error (), ERR_BACKEND_NO_ERR);
    //Now we run it, and clear_error to make sure the error is actually cleared.
    s.ensure_all_data_loaded ();
    s.clear_error ();
    EXPECT_EQ (s.get_error (), ERR_BACKEND_NO_ERR);
    unregister_providers ();
}

TEST (QofSessionTest, load)
{
    // We register a provider that gives a backend that
    // throws an error on load.
    // This error during load should cause the qof session to
    // "roll back" the book load.
    register_provider ();
    QofSession s;
    s.begin ("book1", false, false, false);
    auto book = s.get_book ();
    s.load (nullptr);
    EXPECT_EQ (book, s.get_book ());

    // Now we'll do the load without returning an error from the backend,
    // and ensure that the book changed to a new book.
    load_error = false;
    s.load (nullptr);
    EXPECT_NE (book, s.get_book ());
    // I'll put load_error back just to be tidy.
    load_error = true;
    unregister_providers ();
}

TEST (QofSessionTest, save)
{
    register_provider ();
    QofSession s;
    s.begin ("book1", false, false, false);
    s.save (nullptr);
    EXPECT_EQ (sync_called, true);
    unregister_providers ();
    sync_called = false;
}

TEST (QofSessionTest, safe_save)
{
    register_provider ();
    QofSession s;
    s.begin ("book1", false, false, false);
    s.safe_save (nullptr);
    EXPECT_EQ (safe_sync_called, true);
    unregister_providers ();
    safe_sync_called = false;
}

TEST (QofSessionTest, export_session)
{
    register_provider ();
    QofSession s1;
    s1.begin ("book1", false, false, false);
    QofSession s2;
    s2.begin ("book2", false, false, false);
    QofBook * b1 = s1.get_book ();
    QofBook * b2 = s2.get_book ();
    std::cout << "b1 = " << b1 << " b2 = " << b2 << '\n';
    std::cout << "be1 = " << qof_book_get_backend (b1) << " be2 = " << qof_book_get_backend (b2) << '\n';
    b1->backend = s1.get_backend ();
    b2->backend = s2.get_backend ();
    s2.export_session (s1, nullptr);
    EXPECT_EQ (exported_book, b1);

    unregister_providers ();
}
