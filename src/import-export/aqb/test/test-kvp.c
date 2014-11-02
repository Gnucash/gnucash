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
#include "import-export/aqb/gnc-ab-trans-templ.h"
#include "engine/gnc-hooks.h"

static char* get_filepath(const char* filename)
{
    char *result;

    const char *srcdir = g_getenv("SRCDIR");
    if (!srcdir)
    {
        g_test_message("No env variable SRCDIR exists, assuming \".\"\n");
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

    if (1)
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
            GList *templ_list;
            GncABTransTempl *templ;
            QofBook *book = qof_session_get_book(new_session);
            const char* ORIGINAL_NAME = "Some Name";
            const char* CHANGED_NAME = "Some Changed Name";

            GList *kvp_list = gnc_ab_get_book_template_list(book);
            g_assert(kvp_list != 0); // do we have the slot?!
            g_assert_cmpint(g_list_length(kvp_list), ==, 1);

            templ_list = gnc_ab_trans_templ_list_new_from_kvp_list(kvp_list);
            g_assert_cmpint(g_list_length(templ_list), ==, 1);

            templ = templ_list->data;
            g_assert_cmpstr(gnc_ab_trans_templ_get_name(templ), ==, ORIGINAL_NAME); // ok, name from file is here

            // Now we change the name into something else and verify it can be saved
            gnc_ab_trans_templ_set_name(templ, CHANGED_NAME);
            {
                GList *kvp_list_new = gnc_ab_trans_templ_list_to_kvp_list(templ_list);
                gnc_ab_trans_templ_list_free(templ_list);
                g_assert(!qof_instance_get_dirty(QOF_INSTANCE(book))); // not yet dirty

                // Here we save the changed kvp
                gnc_ab_set_book_template_list(book, kvp_list_new);
                g_assert(qof_instance_get_dirty(QOF_INSTANCE(book))); // yup, now dirty
            }

            {
                GList *mylist = gnc_ab_get_book_template_list(book);
                g_assert(mylist != 0);
                g_assert_cmpint(g_list_length(mylist), ==, 1);

                templ_list = gnc_ab_trans_templ_list_new_from_kvp_list(mylist);
                g_assert_cmpint(g_list_length(templ_list), ==, 1);

                templ = templ_list->data;
                g_assert_cmpstr(gnc_ab_trans_templ_get_name(templ), ==, CHANGED_NAME); // ok, the change has been saved!
                gnc_ab_trans_templ_list_free(templ_list);
            }
        }

        {
            // Check the kvp slots of a aqbanking-enabled account
            QofBook *book = qof_session_get_book(new_session);
            Account* account = gnc_book_get_root_account(book);
            GDate retrieved_date, original_date;
            gchar buff[MAX_DATE_LENGTH];

            g_assert(account);

            // The interesting test case here: Can we read the correct date
            // from the xml file?
            if (1)
            {
                Timespec retrieved_ts = gnc_ab_get_account_trans_retrieval(account);
                g_test_message("retrieved_ts=%s\n", gnc_print_date(retrieved_ts));
                //printf("Time=%s\n", gnc_print_date(retrieved_ts));

                retrieved_date = timespec_to_gdate(retrieved_ts);
                g_date_set_dmy(&original_date, 29, 8, 2014);

                g_assert_cmpint(g_date_compare(&retrieved_date, &original_date), ==, 0);
            }

            // A lower-level test here: Can we write and read again the
            // trans_retrieval date? This wouldn't need this particular
            // Account, just a general Account object.
            if (0)
            {
                Timespec original_ts = timespec_now(), retrieved_ts;

                // Check whether the "ab-trans-retrieval" property of Account
                // is written and read again correctly.
                gnc_ab_set_account_trans_retrieval(account, original_ts);
                retrieved_ts = gnc_ab_get_account_trans_retrieval(account);

//                printf("original_ts=%s = %d  retrieved_ts=%s = %d\n",
//                       gnc_print_date(original_ts), original_ts.tv_sec,
//                       gnc_print_date(retrieved_ts), retrieved_ts.tv_sec);

                original_date = timespec_to_gdate(original_ts);
                retrieved_date = timespec_to_gdate(retrieved_ts);

                qof_print_gdate (buff, sizeof (buff), &original_date);
                //printf("original_date=%s\n", buff);
                qof_print_gdate (buff, sizeof (buff), &retrieved_date);
                //printf("retrieved_date=%s\n", buff);

                // Is the retrieved date identical to the one written
                g_assert_cmpint(g_date_compare(&retrieved_date, &original_date), ==, 0);
            }

        }

        g_free(newfile);
        g_free(file2);

        gnc_hook_run(HOOK_BOOK_CLOSED, new_session);
        //qof_session_destroy(new_session); // tries to delete the LCK file but it wasn't created in the first place
    }


}
