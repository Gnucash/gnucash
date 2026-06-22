/***************************************************************************
 *            test-gnc-uri-utils.c
 *
 *  Thu Sep 29 22:48:57 2005
 *  Copyright  2005  GnuCash team
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

#include <config.h>
#include <stdlib.h>
#include <string.h>

#include <glib.h>
#include "qof.h"
#include <unittest-support.h>
#include "gnc-filepath-utils.h"
#include "gnc-uri-utils.h"

static const gchar *suitename = "/engine/uri-utils";
void test_suite_gnc_uri_utils(void);

struct test_strings_struct
{
    gchar *uri;
    gboolean want_password;
    gchar *protocol;
    gchar *hostname;
    gchar *username;
    gchar *password;
    gchar *path;
    gint32 port;
    gchar *created_uri;
    gchar *normalized_uri;
    gboolean is_file_protocol;
};

typedef struct test_strings_struct test_strings;

test_strings strs[] =
{
#ifndef G_OS_WIN32
    /* basic file tests in posix like environment */
    {
        "/test/path/file.gnucash", FALSE,
        NULL, NULL, NULL, NULL, "/test/path/file.gnucash", 0,
        "file:///test/path/file.gnucash",
        "file:///test/path/file.gnucash", FALSE
    },
    {
        "file:///test/path/file.gnucash", FALSE,
        "file", NULL, NULL, NULL, "/test/path/file.gnucash", 0,
        "file:///test/path/file.gnucash",
        "file:///test/path/file.gnucash", TRUE
    },
    {
        "xml:///test/path/file.gnucash", FALSE,
        "xml", NULL, NULL, NULL, "/test/path/file.gnucash", 0,
        "xml:///test/path/file.gnucash",
        "xml:///test/path/file.gnucash", TRUE
    },
    {
        "sqlite3:///test/path/file.gnucash", FALSE,
        "sqlite3", NULL, NULL, NULL, "/test/path/file.gnucash", 0,
        "sqlite3:///test/path/file.gnucash",
        "sqlite3:///test/path/file.gnucash", TRUE
    },
#else
    /* basic file tests in windows environment */
    {
        "c:\\test\\path\\file.gnucash", FALSE,
        NULL, NULL, NULL, NULL, "c:\\test\\path\\file.gnucash", 0,
        "file://c:\\test\\path\\file.gnucash",
        "file://c:\\test\\path\\file.gnucash", FALSE
    },
    {
        "file://c:\\test\\path\\file.gnucash", FALSE,
        "file", NULL, NULL, NULL, "c:\\test\\path\\file.gnucash", 0,
        "file://c:\\test\\path\\file.gnucash",
        "file://c:\\test\\path\\file.gnucash", TRUE
    },
    {
        "xml://c:\\test\\path\\file.gnucash", FALSE,
        "xml", NULL, NULL, NULL, "c:\\test\\path\\file.gnucash", 0,
        "xml://c:\\test\\path\\file.gnucash",
        "xml://c:\\test\\path\\file.gnucash", TRUE
    },
    {
        "sqlite3://c:\\test\\path\\file.gnucash", FALSE,
        "sqlite3", NULL, NULL, NULL, "c:\\test\\path\\file.gnucash", 0,
        "sqlite3://c:\\test\\path\\file.gnucash",
        "sqlite3://c:\\test\\path\\file.gnucash", TRUE
    },
#endif
    /* basic database tests */
    {
        "mysql://www.gnucash.org/gnucash", FALSE,
        "mysql", "www.gnucash.org", NULL, NULL, "gnucash", 0,
        "mysql://www.gnucash.org/gnucash",
        "mysql://www.gnucash.org/gnucash", FALSE
    },
    {
        "mysql://www.gnucash.org/gnucash", TRUE,
        "mysql", "www.gnucash.org", NULL, NULL, "gnucash", 0,
        "mysql://www.gnucash.org/gnucash",
        "mysql://www.gnucash.org/gnucash", FALSE
    },
    {
        "mysql://dbuser@www.gnucash.org/gnucash", FALSE,
        "mysql", "www.gnucash.org", "dbuser", NULL, "gnucash", 0,
        "mysql://dbuser@www.gnucash.org/gnucash",
        "mysql://dbuser@www.gnucash.org/gnucash", FALSE
    },
    {
        "mysql://dbuser@www.gnucash.org/gnucash", TRUE,
        "mysql", "www.gnucash.org", "dbuser", NULL, "gnucash", 0,
        "mysql://dbuser@www.gnucash.org/gnucash",
        "mysql://dbuser@www.gnucash.org/gnucash", FALSE
    },
    {
        "mysql://dbuser:dbpass@www.gnucash.org/gnucash", FALSE,
        "mysql", "www.gnucash.org", "dbuser", "dbpass", "gnucash", 0,
        "mysql://dbuser:dbpass@www.gnucash.org/gnucash",
        "mysql://dbuser@www.gnucash.org/gnucash", FALSE
    },
    {
        "mysql://dbuser:dbpass@www.gnucash.org/gnucash", TRUE,
        "mysql", "www.gnucash.org", "dbuser", "dbpass", "gnucash", 0,
        "mysql://dbuser:dbpass@www.gnucash.org/gnucash",
        "mysql://dbuser:dbpass@www.gnucash.org/gnucash", FALSE
    },
    {
        "postgres://www.gnucash.org/gnucash", FALSE,
        "postgres", "www.gnucash.org", NULL, NULL, "gnucash", 0,
        "postgres://www.gnucash.org/gnucash",
        "postgres://www.gnucash.org/gnucash", FALSE
    },
    {
        "postgres://www.gnucash.org/gnucash", TRUE,
        "postgres", "www.gnucash.org", NULL, NULL, "gnucash", 0,
        "postgres://www.gnucash.org/gnucash",
        "postgres://www.gnucash.org/gnucash", FALSE
    },
    {
        "postgres://dbuser@www.gnucash.org/gnucash", FALSE,
        "postgres", "www.gnucash.org", "dbuser", NULL, "gnucash", 0,
        "postgres://dbuser@www.gnucash.org/gnucash",
        "postgres://dbuser@www.gnucash.org/gnucash", FALSE
    },
    {
        "postgres://dbuser@www.gnucash.org/gnucash", TRUE,
        "postgres", "www.gnucash.org", "dbuser", NULL, "gnucash", 0,
        "postgres://dbuser@www.gnucash.org/gnucash",
        "postgres://dbuser@www.gnucash.org/gnucash", FALSE
    },
    {
        "postgres://dbuser:dbpass@www.gnucash.org/gnucash", FALSE,
        "postgres", "www.gnucash.org", "dbuser", "dbpass", "gnucash", 0,
        "postgres://dbuser:dbpass@www.gnucash.org/gnucash",
        "postgres://dbuser@www.gnucash.org/gnucash", FALSE
    },
    {
        "postgres://dbuser:dbpass@www.gnucash.org/gnucash", TRUE,
        "postgres", "www.gnucash.org", "dbuser", "dbpass", "gnucash", 0,
        "postgres://dbuser:dbpass@www.gnucash.org/gnucash",
        "postgres://dbuser:dbpass@www.gnucash.org/gnucash", FALSE
    },
    /* password with lots of ugly characters in it (potentially conflicting with uri syntax) */
    {
        "postgres://dbuser:*#bad35:@xx@www.gnucash.org/gnucash", TRUE,
        "postgres", "www.gnucash.org", "dbuser", "*#bad35:@xx", "gnucash", 0,
        "postgres://dbuser:*#bad35:@xx@www.gnucash.org/gnucash",
        "postgres://dbuser:*#bad35:@xx@www.gnucash.org/gnucash", FALSE
    },
    /* uri with custom port number, and hide password in normalized uri */
    {
        "postgres://dbuser:dbpass@www.gnucash.org:744/gnucash", FALSE,
        "postgres", "www.gnucash.org", "dbuser", "dbpass", "gnucash", 744,
        "postgres://dbuser:dbpass@www.gnucash.org:744/gnucash",
        "postgres://dbuser@www.gnucash.org:744/gnucash", FALSE
    },
    /* TODO Figure out how to write tests that actually verify the relative
     * pathname resolution. The above tests only test absolute pathnames */
    { NULL, FALSE, NULL, NULL, NULL, NULL, NULL, 0, NULL, FALSE },
};

/* TEST: gnc_uri_get_components */
static void
test_gnc_uri_get_components()
{
    int i;
    for (i = 0; strs[i].uri != NULL; i++)
    {
        gchar *tprotocol = NULL;
        gchar *thostname = NULL;
        gchar *tusername = NULL;
        gchar *tpassword = NULL;
        gchar *tpath     = NULL;
        gint32 tport     = 0;

        gnc_uri_get_components( strs[i].uri, &tprotocol, &thostname,
                                &tport, &tusername, &tpassword, &tpath );
        g_assert_cmpstr ( tprotocol, ==, strs[i].protocol );
        g_assert_cmpstr ( thostname, ==, strs[i].hostname );
        g_assert_cmpstr ( tusername, ==, strs[i].username );
        g_assert_cmpstr ( tpassword, ==, strs[i].password );
        g_assert_cmpstr ( tpath, ==, strs[i].path );
        g_assert_cmpint ( tport, ==, strs[i].port );

        g_free(tprotocol);
        g_free(thostname);
        g_free(tusername);
        g_free(tpassword);
        g_free(tpath);
    }
}

/* TEST: gnc_uri_get_scheme */
static void
test_gnc_uri_get_scheme()
{
    int i;
    for (i = 0; strs[i].uri != NULL; i++)
    {
        gchar *tprotocol = NULL;

        tprotocol = gnc_uri_get_scheme( strs[i].uri );
        g_assert_cmpstr ( tprotocol, ==, strs[i].protocol );
        g_free(tprotocol);
    }
}

/* TEST: gnc_uri_get_path */
static void
test_gnc_uri_get_path()
{
    int i;
    for (i = 0; strs[i].uri != NULL; i++)
    {
        gchar *tpath = NULL;

        tpath = gnc_uri_get_path( strs[i].uri );
        g_assert_cmpstr ( tpath, ==, strs[i].path );
        g_free(tpath);
    }
}

/* TEST: gnc_uri_create_uri */
static void
test_gnc_uri_create_uri()
{
    int i;
    for (i = 0; strs[i].uri != NULL; i++)
    {
        gchar *turi = NULL;

        turi = gnc_uri_create_uri( strs[i].protocol, strs[i].hostname, strs[i].port,
                                   strs[i].username, strs[i].password, strs[i].path );
        g_assert_cmpstr ( turi, ==, strs[i].created_uri );
        g_free(turi);
    }

    /* A file-type scheme with a non-absolute path exercises the "scheme:///path"
     * branch that inserts an extra slash. In the test environment no backends
     * are registered, so an (unknown) file-type scheme leaves the path
     * unresolved, making the result predictable. */
    {
        gchar *turi = gnc_uri_create_uri( "xml", NULL, 0, NULL, NULL,
                                          "relative/path/file.gnucash" );
        g_assert_cmpstr ( turi, ==, "xml:///relative/path/file.gnucash" );
        g_free(turi);
    }
}

/* TEST: gnc_uri_normalize_uri */
static void
test_gnc_uri_normalize_uri()
{
    int i;

    /* TEST: gnc_uri_normalize_uri */
    for (i = 0; strs[i].uri != NULL; i++)
    {
        gchar *turi = NULL;

        turi = gnc_uri_normalize_uri( strs[i].uri, strs[i].want_password );
        g_assert_cmpstr ( turi, ==, strs[i].normalized_uri );
        g_free(turi);
    }
}

/* TEST: gnc_uri_is_file_scheme */
static void
test_gnc_uri_is_file_scheme()
{
    int i;
    for (i = 0; strs[i].uri != NULL; i++)
    {
        gboolean tis_file_protocol;

        tis_file_protocol = gnc_uri_is_file_scheme( strs[i].protocol );
        g_assert_true ( tis_file_protocol == strs[i].is_file_protocol );
    }
}

/* TEST: gnc_uri_is_file_uri */
static void
test_gnc_uri_is_file_uri()
{
    int i;
    for (i = 0; strs[i].uri != NULL; i++)
    {
        gboolean tis_file_uri;

        tis_file_uri = gnc_uri_is_file_uri( strs[i].uri );
        g_assert_true ( tis_file_uri == strs[i].is_file_protocol );
    }
}

/* TEST: gnc_uri_targets_local_fs */
static void
test_gnc_uri_targets_local_fs()
{
    struct test_local_fs_struct
    {
        const gchar *uri;
        gboolean     targets_local_fs;
    } cases[] =
    {
#ifndef G_OS_WIN32
        { "/test/path/file.gnucash",          TRUE },  /* no scheme   -> local */
        { "file:///test/path/file.gnucash",   TRUE },  /* file scheme -> local */
        { "xml:///test/path/file.gnucash",    TRUE },
        { "sqlite3:///test/path/file.gnucash", TRUE },
#else
        { "c:\\test\\path\\file.gnucash",          TRUE },  /* no scheme   -> local */
        { "file://c:\\test\\path\\file.gnucash",   TRUE },  /* file scheme -> local */
        { "xml://c:\\test\\path\\file.gnucash",    TRUE },
        { "sqlite3://c:\\test\\path\\file.gnucash", TRUE },
#endif
        { "mysql://www.gnucash.org/gnucash",                  FALSE }, /* db scheme -> not local */
        { "postgres://dbuser:dbpass@www.gnucash.org/gnucash", FALSE },
        { NULL, FALSE },
    };

    int i;
    for (i = 0; cases[i].uri != NULL; i++)
        g_assert_true ( gnc_uri_targets_local_fs (cases[i].uri) == cases[i].targets_local_fs );
}

/* TEST: gnc_uri_add_extension */
static void
test_gnc_uri_add_extension()
{
    gchar *result;

    /* A NULL uri returns NULL (and logs a critical from g_return_val_if_fail) */
    if (g_test_undefined ())
    {
        g_test_expect_message ("gnc.engine", G_LOG_LEVEL_CRITICAL,
                               "*assertion 'uri != nullptr' failed*");
        g_assert_null ( gnc_uri_add_extension (NULL, GNC_DATAFILE_EXT) );
        g_test_assert_expected_messages ();
    }

    /* A non-file uri is never modified, only duplicated */
    result = gnc_uri_add_extension ( "mysql://www.gnucash.org/gnucash", GNC_DATAFILE_EXT );
    g_assert_cmpstr ( result, ==, "mysql://www.gnucash.org/gnucash" );
    g_free (result);

#ifndef G_OS_WIN32
    /* A file uri without the extension gets it appended */
    result = gnc_uri_add_extension ( "file:///test/path/file", GNC_DATAFILE_EXT );
    g_assert_cmpstr ( result, ==, "file:///test/path/file" GNC_DATAFILE_EXT );
    g_free (result);

    /* A file uri that already ends in the extension is left unchanged */
    result = gnc_uri_add_extension ( "file:///test/path/file" GNC_DATAFILE_EXT, GNC_DATAFILE_EXT );
    g_assert_cmpstr ( result, ==, "file:///test/path/file" GNC_DATAFILE_EXT );
    g_free (result);

    /* A NULL extension leaves the uri unchanged, only duplicated */
    result = gnc_uri_add_extension ( "file:///test/path/file", NULL );
    g_assert_cmpstr ( result, ==, "file:///test/path/file" );
    g_free (result);
#endif
}

void
test_suite_gnc_uri_utils(void)
{
    GNC_TEST_ADD_FUNC(suitename, "gnc_uri_get_components()", test_gnc_uri_get_components);
    GNC_TEST_ADD_FUNC(suitename, "gnc_uri_get_scheme()", test_gnc_uri_get_scheme);
    GNC_TEST_ADD_FUNC(suitename, "gnc_uri_get_path()", test_gnc_uri_get_path);
    GNC_TEST_ADD_FUNC(suitename, "gnc_uri_create_uri()", test_gnc_uri_create_uri);
    GNC_TEST_ADD_FUNC(suitename, "gnc_uri_normalize_uri()", test_gnc_uri_normalize_uri);
    GNC_TEST_ADD_FUNC(suitename, "gnc_uri_is_file_scheme()", test_gnc_uri_is_file_scheme);
    GNC_TEST_ADD_FUNC(suitename, "gnc_uri_is_file_uri()", test_gnc_uri_is_file_uri);
    GNC_TEST_ADD_FUNC(suitename, "gnc_uri_targets_local_fs()", test_gnc_uri_targets_local_fs);
    GNC_TEST_ADD_FUNC(suitename, "gnc_uri_add_extension()", test_gnc_uri_add_extension);
}
