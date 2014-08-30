/********************************************************************
 * test_submodule.c: Example GLib g_test test suite.		    *
 * Copyright 2011 John Ralls <jralls@ceridwen.us>		    *
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
\********************************************************************/

#include <config.h>
#include <glib.h>

// for the gnc_ab_get_book_template_list() et al. functions
#include "import-export/aqb/gnc-ab-kvp.h"
#include "engine/gnc-hooks.h"

static char* get_filepath(const char* filename)
{
    char *result;

    const char *srcdir = g_getenv("SRCDIR");
    if (!srcdir)
    {
        srcdir = ".";
    }

    result = g_strdup_printf("%s/%s", srcdir, filename);

    g_test_message("Using file path %s\n", result);

    // Test whether the file really exists
    g_assert(g_file_test(result, G_FILE_TEST_EXISTS));

    return result;
}

void
test_qofsession_aqb_kvp( void )
{
    /* load the accounts from the users datafile */
    /* but first, check to make sure we've got a session going. */
    QofBackendError io_err;
    char *file1 = get_filepath("file-book.gnucash");
    char *file2 = get_filepath("file-book-hbcislot.gnucash");

    if (1)
    {
        // A file with no content at all, but a valid XML file
        QofSession *new_session = qof_session_new ();
        char *newfile = g_strdup_printf("file://%s", file1);

        qof_session_begin (new_session, newfile, TRUE, FALSE, FALSE);
        io_err = qof_session_get_error (new_session);
        //printf("io_err1 = %d\n", io_err);
        g_assert(io_err != ERR_BACKEND_NO_HANDLER); // Do not have no handler

        g_assert(io_err != ERR_BACKEND_NO_SUCH_DB); // DB must exist
        g_assert(io_err != ERR_BACKEND_LOCKED);
        g_assert(io_err == 0);

        qof_session_load (new_session, NULL);
        io_err = qof_session_get_error (new_session);
        //printf("io_err2 = %d\n", io_err);
        g_assert(io_err == 0);

        // No HBCI slot exists, of course

        {
            // No HBCI slot exists, of course
            GList *mylist = gnc_ab_get_book_template_list(qof_session_get_book(new_session));
            g_assert(mylist == 0);
        }

        g_free(newfile);
        g_free(file1);

        gnc_hook_run(HOOK_BOOK_CLOSED, new_session);
        //qof_session_destroy(new_session); // tries to delete the LCK file but it wasn't created in the first place
    }

    if (0)
    {
        // A file with no content except for the book_template_list kvp
        // slot
        QofSession *new_session = qof_session_new ();
        char *newfile = g_strdup_printf("file://%s", file2);

        qof_session_begin (new_session, newfile, TRUE, FALSE, FALSE);
        io_err = qof_session_get_error (new_session);
        //printf("io_err1 = %d\n", io_err);
        g_assert(io_err != ERR_BACKEND_NO_HANDLER); // Do not have no handler

        g_assert(io_err != ERR_BACKEND_NO_SUCH_DB); // DB must exist
        g_assert(io_err != ERR_BACKEND_LOCKED);
        g_assert(io_err == 0);

        qof_session_load (new_session, NULL);
        io_err = qof_session_get_error (new_session);
        //printf("io_err2 = %d\n", io_err);
        g_assert(io_err == 0);

        {
            GList *mylist = gnc_ab_get_book_template_list(qof_session_get_book(new_session));
            g_assert(mylist != 0); // do we have the slot?!
        }

        g_free(newfile);
        g_free(file2);

        gnc_hook_run(HOOK_BOOK_CLOSED, new_session);
        //qof_session_destroy(new_session); // tries to delete the LCK file but it wasn't created in the first place
    }


}
