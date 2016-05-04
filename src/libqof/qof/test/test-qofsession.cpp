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

QofBackend * test_backend_factory ()
{
    QofBackend * ret = (QofBackend*) std::malloc (sizeof (QofBackend));
    ret->session_begin = nullptr;
    ret->session_end = nullptr;
    ret->destroy_backend = nullptr;
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

TEST (QofSessionTest, swap_books)
{
    qof_backend_register_provider (get_provider ());
    QofSession s1;
    QofSession s2;
    s1.begin ("book1", false, false, false);
    s2.begin ("book2", false, false, false);
    QofBook * b1 {s1.get_book ()};
    QofBook * b2 {s2.get_book ()};
    ASSERT_NE (b1, b2);
    s1.swap_books (s2);
    EXPECT_EQ (s1.get_book (), b2);
    EXPECT_EQ (s2.get_book (), b1);
}

