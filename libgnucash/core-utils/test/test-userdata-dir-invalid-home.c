/***************************************************************************
 *            test-userdata-dir-invalid-home.c
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
        PROJECT_NAME
    },
    {
        1, "gnc_build_book_path",
        PROJECT_NAME G_DIR_SEPARATOR_S "books"
    },
    {
        2, "gnc_build_translog_path",
        PROJECT_NAME G_DIR_SEPARATOR_S "translog"
    },
    {
        3, "gnc_build_data_path",
        PROJECT_NAME G_DIR_SEPARATOR_S "data"
    },
    { 0, NULL, NULL },
};

int
main(G_GNUC_UNUSED int argc, G_GNUC_UNUSED char **argv)
{
/* Don't run this test on Windows or OS X. This
   test attempts to fool the code into using a non-existent
   home directory, but the way this is done only works on linux
   */
#ifndef MAC_INTEGRATION
#ifndef G_OS_WIN32
    int i;
    const char *tmp_dir = g_get_tmp_dir();
    const char *builddir = g_getenv ("GNC_BUILDDIR");
    char *homedir = g_build_filename (builddir, "notexist", NULL);

    /* Assume we're not in a build environment to test
     * the function's actual behaviour in a real world use case, using
     * a non-existent homedir. */
    g_unsetenv("GNC_BUILDDIR");
    g_unsetenv("GNC_UNINSTALLED");

    /* Run usr conf dir tests with an invalid homedir
     * The code should fall back to using the temporary
     * directory in that case. */
    g_setenv("HOME", homedir, TRUE);
    for (i = 0; strs2[i].funcname != NULL; i++)
    {
        char *daout;
        char *wantout;

        if (strs2[i].func_num == 0)
        {
            wantout = g_build_filename(tmp_dir, g_get_user_name (), strs2[i].output, "foo",
                                       (gchar *)NULL);
            daout = gnc_build_userdata_path("foo");
        }
        else if (strs2[i].func_num == 1)
        {
            wantout = g_build_filename(tmp_dir, g_get_user_name (), strs2[i].output, "foo",
                                       (gchar *)NULL);
            daout = gnc_build_book_path("foo");
        }
        else if (strs2[i].func_num == 2)
        {
            wantout = g_build_filename(tmp_dir, g_get_user_name (), strs2[i].output, "foo",
                                       (gchar *)NULL);
            daout = gnc_build_translog_path("foo");
        }
        else // if (strs2[i].prefix_home == 3)
        {
            wantout = g_build_filename(tmp_dir, g_get_user_name (), strs2[i].output, "foo",
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
#endif
#endif
}
