/********************************************************************\
 * gtest-gnc-uri.cpp -- Unit tests for the GncUri C++ class.        *
 *                                                                  *
 * Copyright (C) 2026 Brent McBride <mcbridebt@hotmail.com>         *
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
 *                                                                  *
\********************************************************************/

#include <config.h>
#include <glib.h>
#include <optional>
#include <stdexcept>
#include <string>
#include "qof.h"
#include "gnc-backend-prov.hpp"
#include "gnc-uri-utils.h"
#include "gnc-uri.hpp"
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcpp"
#include <gtest/gtest.h>
#pragma GCC diagnostic pop

/* Parse a file uri into its components. */
TEST(GncUri, ParseFileUri)
{
    GncUri f { "file:///test/path/file.gnucash" };
    ASSERT_TRUE (f.scheme().has_value());
    EXPECT_EQ (*f.scheme(), "file");
    EXPECT_FALSE (f.hostname().has_value());
    ASSERT_TRUE (f.path().has_value());
    EXPECT_EQ (*f.path(), "/test/path/file.gnucash");
    EXPECT_TRUE (f.is_file_uri());
    EXPECT_TRUE (f.targets_local_fs());
}

/* Parse a database uri with userinfo and a port. */
TEST(GncUri, ParseDatabaseUri)
{
    GncUri d { "postgres://dbuser:dbpass@www.gnucash.org:744/gnucash" };
    ASSERT_TRUE (d.scheme().has_value());
    EXPECT_EQ (*d.scheme(), "postgres");
    ASSERT_TRUE (d.hostname().has_value());
    EXPECT_EQ (*d.hostname(), "www.gnucash.org");
    ASSERT_TRUE (d.username().has_value());
    EXPECT_EQ (*d.username(), "dbuser");
    ASSERT_TRUE (d.password().has_value());
    EXPECT_EQ (*d.password(), "dbpass");
    EXPECT_EQ (d.port(), 744);
    EXPECT_FALSE (d.is_file_uri());
    EXPECT_FALSE (d.targets_local_fs());
}

/* Compose a uri, with and without the password. */
TEST(GncUri, ComposeWithAndWithoutPassword)
{
    GncUri d { "postgres://dbuser:dbpass@www.gnucash.org:744/gnucash" };
    EXPECT_EQ (d.str (true),  "postgres://dbuser:dbpass@www.gnucash.org:744/gnucash");
    EXPECT_EQ (d.str (false), "postgres://dbuser@www.gnucash.org:744/gnucash");
    EXPECT_EQ (d.try_str (false).value_or (""),
               "postgres://dbuser@www.gnucash.org:744/gnucash");
}

/* A bare path has no scheme but still targets the local fs. */
TEST(GncUri, BarePath)
{
    GncUri p { "/test/path/file.gnucash" };
    EXPECT_FALSE (p.scheme().has_value());
    ASSERT_TRUE (p.path().has_value());
    EXPECT_EQ (*p.path(), "/test/path/file.gnucash");
    EXPECT_FALSE (p.is_file_uri());    /* no scheme -> not a file *uri* */
    EXPECT_TRUE (p.targets_local_fs());
}

/* An empty uri yields an object with no components set. */
TEST(GncUri, EmptyUri)
{
    GncUri e { std::string {} };
    EXPECT_FALSE (e.scheme().has_value());
    EXPECT_FALSE (e.hostname().has_value());
    EXPECT_FALSE (e.path().has_value());
    EXPECT_EQ (e.port(), 0);
    EXPECT_FALSE (e.is_file_uri());
}

/* A file uri carrying a Windows-style drive letter (file:///N:/...) has its
 * leading slash stripped before the path is resolved. The shape of the string
 * is what triggers this, so it exercises the branch on any platform. */
TEST(GncUri, WindowsStyleDriveLetterPath)
{
    GncUri w { "file:///N:/test/path/file.gnucash" };
    ASSERT_TRUE (w.scheme().has_value());
    EXPECT_EQ (*w.scheme(), "file");
    EXPECT_TRUE (w.path().has_value());
    EXPECT_TRUE (w.is_file_uri());
}

/* Compose from individual components (mirrors gnc_uri_create_uri). With no
 * backend registered "xml" is an unknown scheme, so the relative path is used
 * as is. */
TEST(GncUri, ComposeFromComponents)
{
    GncUri c { std::string {"xml"}, std::nullopt, 0, std::nullopt, std::nullopt,
               std::string {"relative/path/file.gnucash"} };
    EXPECT_EQ (c.str(), "xml:///relative/path/file.gnucash");
}

/* The component constructor rejects parts that can't form a valid uri: no path
 * at all, or a non-file scheme with a path but no hostname. */
TEST(GncUri, ComponentCtorThrowsWhenIncomplete)
{
    EXPECT_THROW ((GncUri { std::nullopt, std::nullopt, 0, std::nullopt,
                            std::nullopt, std::nullopt }),
                  std::invalid_argument);
    EXPECT_THROW ((GncUri { std::string {"postgres"}, std::nullopt, 0,
                            std::nullopt, std::nullopt,
                            std::string {"gnucash"} }),
                  std::invalid_argument);
}

/* The parsing ctor stays permissive, so a parsed uri can still lack a path (a
 * network scheme with a host but no path). Such an object can't be turned back
 * into a locator: try_str() returns nullopt and str() throws. */
TEST(GncUri, ParsedUriWithoutPathCannotStringify)
{
    GncUri incomplete { "postgres://www.gnucash.org" };
    EXPECT_FALSE (incomplete.try_str().has_value());
    EXPECT_THROW (incomplete.str(), std::invalid_argument);
}

/* A minimal backend provider used only to populate the list of registered
 * access methods that the uri code consults. */
struct UriTestProvider : public QofBackendProvider
{
    UriTestProvider (const char* name, const char* method)
        : QofBackendProvider {name, method} {}
    QofBackend* create_backend (void) override { return nullptr; }
    bool type_check (const char*) override { return false; }
};

/* Registering a provider whose access method matches a file-type scheme
 * ("xml") makes the known-scheme lookup find a match, so a uri composed for
 * that scheme has its path resolved instead of being used as is. An absolute
 * path resolves to itself, keeping the result predictable. */
TEST(GncUri, KnownScheme)
{
    qof_backend_register_provider (
        QofBackendProvider_ptr {new UriTestProvider {"Test Backend", "xml"}});

    GncUri uri { std::string {"xml"}, std::nullopt, 0, std::nullopt,
                 std::nullopt, std::string {"/test/path/file.gnucash"} };
    EXPECT_EQ (uri.str(), "xml:///test/path/file.gnucash");

    qof_backend_unregister_all_providers ();
}
