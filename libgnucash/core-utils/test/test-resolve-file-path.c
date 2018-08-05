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

#include <config.h>
#include <stdlib.h>
#include <string.h>

#include <glib.h>
#include "test-stuff.h"
#include "gnc-filepath-utils.h"

struct relpath_strings_struct
{
    char *input;
    char *output;
    int prefix_home;
};

typedef struct relpath_strings_struct relpath_strings;

relpath_strings strs[] =
{
    {
        G_DIR_SEPARATOR_S ".gnucash" G_DIR_SEPARATOR_S "test-account-name",
        G_DIR_SEPARATOR_S ".gnucash" G_DIR_SEPARATOR_S "test-account-name", 1
    },
    {
        G_DIR_SEPARATOR_S "tmp" G_DIR_SEPARATOR_S "test-account-name2",
        G_DIR_SEPARATOR_S "tmp" G_DIR_SEPARATOR_S "test-account-name2", 0
    },
    /* TODO Figure out how to write tests that actually verify the relative
     * pathname resolution. The above tests only test absolute pathnames */
    { NULL, NULL, 0 },
};

int
main(int argc, char **argv)
{
    int i;
    char *home_dir = NULL;

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

    for (i = 0; strs[i].input != NULL; i++)
    {
        char *daout;
        char *dain;
        char *wantout;

        if (strs[i].prefix_home == 1)
        {
            dain = g_build_filename(home_dir, strs[i].input,
                                    (gchar *)NULL);
            wantout = g_build_filename(home_dir, strs[i].output,
                                       (gchar *)NULL);
        }
        else if (strs[i].prefix_home == 2)
        {
            dain = g_strdup(strs[i].input);
            wantout = g_build_filename(home_dir, strs[i].output,
                                       (gchar *)NULL);
        }
        else
        {
            dain = g_strdup(strs[i].input);
            wantout = g_strdup(strs[i].output);
        }

        daout = gnc_resolve_file_path(dain);
        do_test_args(g_strcmp0(daout, wantout) == 0,
                     "gnc_resolve_file_path",
                     __FILE__, __LINE__,
                     "%s (%s) vs %s", daout, dain, wantout);
        g_free(dain);
        g_free(wantout);
        g_free(daout);
    }

    print_test_results();
    return get_rv();
}
