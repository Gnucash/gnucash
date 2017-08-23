/***************************************************************************
 *            test-resolve-file-path.c
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
#include "test-stuff.h"
#include "gnc-filepath-utils.h"

struct usr_confpath_strings_struct
{
    int func_num;
    char *funcname;
    char *output;
};

typedef struct usr_confpath_strings_struct usr_confpath_strings;

usr_confpath_strings strs2[] =
{
    {
        0, "gnc_build_userdata_path",
        PACKAGE_NAME
    },
    {
        1, "gnc_build_book_path",
        PACKAGE_NAME G_DIR_SEPARATOR_S "books"
    },
    {
        2, "gnc_build_translog_path",
        PACKAGE_NAME G_DIR_SEPARATOR_S "translog"
    },
    {
        3, "gnc_build_data_path",
        PACKAGE_NAME G_DIR_SEPARATOR_S "data"
    },
    { 0, NULL, NULL },
};

int
main(int argc, char **argv)
{
    int i;
    char *home_dir = NULL;
    const char *userdata_dir = NULL;
    const char *tmp_dir = g_get_tmp_dir();

    if (argc > 1)
        /* One can pass a homedir on the command line. This
         * will most likely cause the test to fail, but it can be
         * used to pass invalid home directories manually. The
         * test error messages should then show the system's temporary
         * directory to be used instead */
        home_dir = argv[1];
    else
        /* Set up a fake home directory to play with */
        home_dir = g_dir_make_tmp("gnucashXXXXXX", NULL);

    /* Run usr conf dir tests with a valid and writable homedir */
    g_setenv("HOME", home_dir, TRUE);

    /* First run, before calling gnc_filepath_init */
    for (i = 0; strs2[i].funcname != NULL; i++)
    {
        char *daout;
        char *wantout;

        if (strs2[i].func_num == 0)
        {
            wantout = g_build_filename(tmp_dir, strs2[i].output, "foo",
                                       (gchar *)NULL);
            daout = gnc_build_userdata_path("foo");
        }
        else if (strs2[i].func_num == 1)
        {
            wantout = g_build_filename(tmp_dir, strs2[i].output, "foo",
                                       (gchar *)NULL);
            daout = gnc_build_book_path("foo");
        }
        else if (strs2[i].func_num == 2)
        {
            wantout = g_build_filename(tmp_dir, strs2[i].output, "foo",
                                       (gchar *)NULL);
            daout = gnc_build_translog_path("foo");
        }
        else // if (strs2[i].prefix_home == 3)
        {
            wantout = g_build_filename(tmp_dir, strs2[i].output, "foo",
                                       (gchar *)NULL);
            daout = gnc_build_data_path("foo");
        }

        do_test_args(g_strcmp0(daout, wantout) == 0,
                     "gnc_build_x_path",
                     __FILE__, __LINE__,
                     "%s (%s) vs %s", daout, strs2[i].funcname, wantout);
        g_free(wantout);
        g_free(daout);
    }

    /* Second run, after properly having called gnc_filepath_init */
    gnc_filepath_init(TRUE);
    userdata_dir = g_get_user_data_dir();
    for (i = 0; strs2[i].funcname != NULL; i++)
    {
        char *daout;
        char *wantout;

        if (strs2[i].func_num == 0)
        {
            wantout = g_build_filename(userdata_dir, strs2[i].output, "foo",
                                       (gchar *)NULL);
            daout = gnc_build_userdata_path("foo");
        }
        else if (strs2[i].func_num == 1)
        {
            wantout = g_build_filename(userdata_dir, strs2[i].output, "foo",
                                       (gchar *)NULL);
            daout = gnc_build_book_path("foo");
        }
        else if (strs2[i].func_num == 2)
        {
            wantout = g_build_filename(userdata_dir, strs2[i].output, "foo",
                                       (gchar *)NULL);
            daout = gnc_build_translog_path("foo");
        }
        else // if (strs2[i].prefix_home == 3)
        {
            wantout = g_build_filename(userdata_dir, strs2[i].output, "foo",
                                       (gchar *)NULL);
            daout = gnc_build_data_path("foo");
        }

        do_test_args(g_strcmp0(daout, wantout) == 0,
                     "gnc_build_x_path",
                     __FILE__, __LINE__,
                     "%s (%s) vs %s", daout, strs2[i].funcname, wantout);
        g_free(wantout);
        g_free(daout);
    }

    print_test_results();
    return get_rv();
}
