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

#include "config.h"
#include <stdlib.h>
#include <string.h>

#include <glib.h>
#include "qof.h"
#include "test-stuff.h"
#include "gnc-uri-utils.h"

struct test_strings_struct
{
    gchar *uri;
    gchar *protocol;
    gchar *hostname;
    gchar *username;
    gchar *password;
    gchar *path;
    guint32 port;
    gchar *norm_uri;
    gboolean want_password;
};

typedef struct test_strings_struct test_strings;

test_strings strs[] =
{
#ifndef G_OS_WIN32
    /* basic file tests in posix like environment */
    {
        "/test/path/file.xacc",
        "file", NULL, NULL, NULL, "/test/path/file.xacc", 0,
        "file:///test/path/file.xacc", FALSE
    },
    {
        "file:///test/path/file.xacc",
        "file", NULL, NULL, NULL, "/test/path/file.xacc", 0,
        "file:///test/path/file.xacc", FALSE
    },
    {
        "xml:///test/path/file.xacc",
        "xml", NULL, NULL, NULL, "/test/path/file.xacc", 0,
        "xml:///test/path/file.xacc", FALSE
    },
    {
        "sqlite3:///test/path/file.xacc",
        "sqlite3", NULL, NULL, NULL, "/test/path/file.xacc", 0,
        "sqlite3:///test/path/file.xacc", FALSE
    },
#else
    /* basic file tests in windows environment */
    {
        "c:\\test\\path\\file.gnucash",
        "file", NULL, NULL, NULL, "c:\\test\\path\\file.gnucash", 0,
        "file://c:\\test\\path\\file.gnucash", FALSE
    },
    {
        "file://c:\\test\\path\\file.gnucash",
        "file", NULL, NULL, NULL, "c:\\test\\path\\file.gnucash", 0,
        "file://c:\\test\\path\\file.gnucash", FALSE
    },
    {
        "xml://c:\\test\\path\\file.gnucash",
        "xml", NULL, NULL, NULL, "c:\\test\\path\\file.gnucash", 0,
        "xml://c:\\test\\path\\file.gnucash", FALSE
    },
    {
        "sqlite3://c:\\test\\path\\file.gnucash",
        "sqlite3", NULL, NULL, NULL, "c:\\test\\path\\file.gnucash", 0,
        "sqlite3://c:\\test\\path\\file.gnucash", FALSE
    },
#endif
    /* basic database tests */
    {
        "mysql://www.gnucash.org/gnucash",
        "mysql", "www.gnucash.org", NULL, NULL, "gnucash", 0,
        "mysql://www.gnucash.org/gnucash", FALSE
    },
    {
        "mysql://www.gnucash.org/gnucash",
        "mysql", "www.gnucash.org", NULL, NULL, "gnucash", 0,
        "mysql://www.gnucash.org/gnucash", TRUE
    },
    {
        "mysql://dbuser@www.gnucash.org/gnucash",
        "mysql", "www.gnucash.org", "dbuser", NULL, "gnucash", 0,
        "mysql://dbuser@www.gnucash.org/gnucash", FALSE
    },
    {
        "mysql://dbuser@www.gnucash.org/gnucash",
        "mysql", "www.gnucash.org", "dbuser", NULL, "gnucash", 0,
        "mysql://dbuser@www.gnucash.org/gnucash", TRUE
    },
    {
        "mysql://dbuser:dbpass@www.gnucash.org/gnucash",
        "mysql", "www.gnucash.org", "dbuser", "dbpass", "gnucash", 0,
        "mysql://dbuser@www.gnucash.org/gnucash", FALSE
    },
    {
        "mysql://dbuser:dbpass@www.gnucash.org/gnucash",
        "mysql", "www.gnucash.org", "dbuser", "dbpass", "gnucash", 0,
        "mysql://dbuser:dbpass@www.gnucash.org/gnucash", TRUE
    },
    {
        "postgres://www.gnucash.org/gnucash",
        "postgres", "www.gnucash.org", NULL, NULL, "gnucash", 0,
        "postgres://www.gnucash.org/gnucash", FALSE
    },
    {
        "postgres://www.gnucash.org/gnucash",
        "postgres", "www.gnucash.org", NULL, NULL, "gnucash", 0,
        "postgres://www.gnucash.org/gnucash", TRUE
    },
    {
        "postgres://dbuser@www.gnucash.org/gnucash",
        "postgres", "www.gnucash.org", "dbuser", NULL, "gnucash", 0,
        "postgres://dbuser@www.gnucash.org/gnucash", FALSE
    },
    {
        "postgres://dbuser@www.gnucash.org/gnucash",
        "postgres", "www.gnucash.org", "dbuser", NULL, "gnucash", 0,
        "postgres://dbuser@www.gnucash.org/gnucash", TRUE
    },
    {
        "postgres://dbuser:dbpass@www.gnucash.org/gnucash",
        "postgres", "www.gnucash.org", "dbuser", "dbpass", "gnucash", 0,
        "postgres://dbuser@www.gnucash.org/gnucash", FALSE
    },
    {
        "postgres://dbuser:dbpass@www.gnucash.org/gnucash",
        "postgres", "www.gnucash.org", "dbuser", "dbpass", "gnucash", 0,
        "postgres://dbuser:dbpass@www.gnucash.org/gnucash", TRUE
    },
    /* password with lots of ugly characters in it test */
    {
        "postgres://dbuser:*#bad35:@xx@www.gnucash.org/gnucash",
        "postgres", "www.gnucash.org", "dbuser", "*#bad35:@xx", "gnucash", 0,
        "postgres://dbuser:*#bad35:@xx@www.gnucash.org/gnucash", TRUE
    },
    /* TODO Figure out how to write tests that actually verify the relative
     * pathname resolution. The above tests only test absolute pathnames */
    { NULL, NULL, NULL, NULL, NULL, NULL, 0, NULL, FALSE },
};

int
main(int argc, char **argv)
{
    int i;

    qof_init();

    /* gnc_uri_get_components */
    for (i = 0; strs[i].uri != NULL; i++)
    {
        gchar *tprotocol = NULL;
        gchar *thostname = NULL;
        gchar *tusername = NULL;
        gchar *tpassword = NULL;
        gchar *tpath     = NULL;
        guint32 tport    = 0;
        gboolean testresult;

        gnc_uri_get_components( strs[i].uri, &tprotocol, &thostname,
                                tport, &tusername, &tpassword, &tpath );
        testresult = ( safe_strcmp ( tprotocol, strs[i].protocol ) == 0 ) &
                     ( safe_strcmp ( thostname, strs[i].hostname ) == 0 ) &
                     ( safe_strcmp ( tusername, strs[i].username ) == 0 ) &
                     ( safe_strcmp ( tpassword, strs[i].password ) == 0 ) &
                     ( safe_strcmp ( tpath, strs[i].path ) == 0 ) &
                     ( tport == strs[i].port );
        do_test_args(testresult,
                     "gnc_uri_get_components",
                     __FILE__, __LINE__,
                     "\n  %s:\n"
                     "    Expected: %s, %s, %s, %s, %s, %d\n"
                     "    Got     : %s, %s, %s, %s, %s, %d\n",
                     strs[i].uri, strs[i].protocol, strs[i].hostname,
                     strs[i].username, strs[i].password, strs[i].path, strs[i].port,
                     tprotocol, thostname, tusername, tpassword, tpath, tport);
        g_free(tprotocol);
        g_free(thostname);
        g_free(tusername);
        g_free(tpassword);
        g_free(tpath);
    }
    print_test_results();
    return get_rv();
}
