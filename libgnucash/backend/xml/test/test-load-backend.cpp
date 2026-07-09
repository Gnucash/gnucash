/***************************************************************************
 *            test-load-backend.c
 *
 *  Replaces the guile version to test the GModule file backend loading.
 *
 *  Sun Oct  9 18:58:47 2005
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
#include <config.h>
#include <filesystem>
#include "qof.h"
#include "cashobjects.h"
#include "test-stuff.h"

#define GNC_LIB_NAME "gncmod-backend-xml"
#define GNC_LIB_REL_PATH "xml"

static void
test_new_store_end_releases_lock (void)
{
    gchar *dir = g_dir_make_tmp ("gnc-xml-lock-XXXXXX", NULL);
    if (!do_test (dir != NULL, "could not create a temp directory"))
        return;
    gchar *path = g_build_filename (dir, "book.gnucash", NULL);
    gchar *lockfile = g_strconcat (path, ".LCK", NULL);
    gchar *uri = g_strconcat ("xml://", path, NULL);
    QofSession *session = qof_session_new (qof_book_new ());

    qof_session_begin (session, uri, SESSION_NEW_STORE);
    do_test (qof_session_get_error (session) == ERR_BACKEND_NO_ERR,
             "creating a new XML store failed");
    qof_session_end (session);
    do_test (!g_file_test (lockfile, G_FILE_TEST_EXISTS),
             "qof_session_end() did not remove the .LCK lockfile");
    qof_session_destroy (session);

    std::filesystem::remove (lockfile);
    std::filesystem::remove (path);
    std::filesystem::remove (dir);
    g_free (uri);
    g_free (lockfile);
    g_free (path);
    g_free (dir);
}

int main (int argc, char** argv)
{
    g_setenv ("GNC_UNINSTALLED", "1", TRUE);
    qof_init ();
    cashobjects_register ();
    do_test (
        qof_load_backend_library (GNC_LIB_REL_PATH, GNC_LIB_NAME),
        " loading gnc-backend-xml GModule failed");
    test_new_store_end_releases_lock ();
    print_test_results ();
    qof_close ();
    exit (get_rv ());
}
