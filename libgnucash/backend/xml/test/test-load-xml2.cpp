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
extern "C"
{
#include "config.h"
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dirent.h>
#include <string.h>
#include <glib.h>
#include <glib-object.h>
#include <glib/gstdio.h>

#include <cashobjects.h>
#include <TransLog.h>
#include <gnc-engine.h>
#include <gnc-prefs.h>

#include <unittest-support.h>
#include <test-engine-stuff.h>
}

#include "../gnc-backend-xml.h"
#include "../io-gncxml-v2.h"
#include "test-file-stuff.h"
#include <test-stuff.h>

#define GNC_LIB_NAME "gncmod-backend-xml"
#define GNC_LIB_REL_PATH "xml"

static void
remove_files_pattern (const char* begining, const char* ending)
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
    QofSession* session;
    QofBook* book;
    Account* root;
    gboolean ignore_lock;
    const char* logdomain = "backend.xml";
    GLogLevelFlags loglevel = static_cast<decltype (loglevel)>
                              (G_LOG_LEVEL_WARNING);
    TestErrorStruct check = { loglevel, const_cast<char*> (logdomain), NULL };
    g_log_set_handler (logdomain, loglevel,
                       (GLogFunc)test_checked_handler, &check);

    session = qof_session_new ();

    remove_locks (filename);

    ignore_lock = (g_strcmp0 (g_getenv ("SRCDIR"), ".") != 0);
    /*    gnc_prefs_set_file_save_compressed(FALSE); */
    qof_session_begin (session, filename, ignore_lock, FALSE, TRUE);

    qof_session_load (session, NULL);
    book = qof_session_get_book (session);

    root = gnc_book_get_root_account (book);
    do_test (gnc_account_get_book (root) == book,
             "book and root account don't match");

    do_test_args (qof_session_get_error (session) == ERR_BACKEND_NO_ERR,
                  "session load xml2", __FILE__, __LINE__,
                  "qof error=%d for file [%s]",
                  qof_session_get_error (session), filename);
    /* Uncomment the line below to generate corrected files */
    /*    qof_session_save( session, NULL ); */
    qof_session_end (session);
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

    xaccLogDisable ();

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
                    test_load_file (to_open);
                    files_tested++;
                }
                g_free (to_open);
            }
        }
    }

    g_dir_close (xml2_dir);

    if (files_tested == 0)
    {
        failure ("handled 0 files in test-load-xml2");
    }

    print_test_results ();
    qof_close ();
    exit (get_rv ());
}
