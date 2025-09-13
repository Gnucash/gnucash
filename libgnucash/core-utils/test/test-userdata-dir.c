/***************************************************************************
 *            test-userdata-dir.c
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
#include <glib/gstdio.h>
#include "test-stuff.h"
#include "gnc-filepath-utils.h"
#ifdef MAC_INTEGRATION
#include <Foundation/Foundation.h>
#endif

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
        0, "gnc_build_userdata_path", ""
    },
    {
        1, "gnc_build_book_path", "books"
    },
    {
        2, "gnc_build_translog_path", "translog"
    },
    {
        3, "gnc_build_data_path", "data"
    },
    { 0, NULL, NULL },
};

#if defined G_OS_WIN32 ||defined MAC_INTEGRATION
const char *path_package = PACKAGE_NAME;
#else
const char *path_package = PROJECT_NAME;
#endif


static char*
test_get_userdatadir ()
{
#ifdef MAC_INTEGRATION
     char *retval = NULL;
     NSFileManager*fm = [NSFileManager defaultManager];
     NSArray* appSupportDir = [fm URLsForDirectory:NSApplicationSupportDirectory
                               inDomains:NSUserDomainMask];
     if ([appSupportDir count] > 0)
     {
          NSURL* dirUrl = [appSupportDir objectAtIndex:0];
          NSString* dirPath = [dirUrl path];
          retval = g_strdup([dirPath UTF8String]);
     }
     return retval;
#else
     return g_strdup(g_get_user_data_dir());
#endif
}

int
main(int argc, char **argv)
{
    int i;
    char *home_dir = NULL;
    char *tmp_dir = NULL;
    char *userdata_dir = NULL;
    char *gnc_data_home_dir = NULL;
    const char *builddir = g_getenv("GNC_BUILDDIR");
    const char *uninstalled = g_getenv("GNC_UNINSTALLED");

    if (argc > 1)
    {
        /* One can pass a homedir on the command line. This
         * will most likely cause the test to fail, but it can be
         * used to pass invalid home directories manually. The
         * test error messages should then show the system's temporary
         * directory to be used instead */
        home_dir = g_strdup(argv[1]);
    }
    else
    {
        /* Set up a fake home directory to play with */
#ifdef MAC_INTEGRATION
        home_dir = test_get_userdatadir();
#else
        home_dir = g_dir_make_tmp("gnucashXXXXXX", NULL);
#endif
    }
    /* Run usr conf dir tests with a valid and writable fake homedir */
    g_setenv("HOME", home_dir, TRUE);

    /* First run, assuming GNC_BUILDDIR and GNC_UNINSTALLED are set */
    if (builddir && uninstalled)
    {
        for (i = 0; strs2[i].funcname != NULL; i++)
        {
            char *daout;
            char *wantout;

            if (strs2[i].func_num == 0)
            {
                wantout = g_build_filename(builddir, "gnc_data_home", "foo",
                                        (gchar *)NULL);
                daout = gnc_build_userdata_path("foo");
            }
            else if (strs2[i].func_num == 1)
            {
                wantout = g_build_filename(builddir, "gnc_data_home", strs2[i].output, "foo",
                                        (gchar *)NULL);
                daout = gnc_build_book_path("foo");
            }
            else if (strs2[i].func_num == 2)
            {
                wantout = g_build_filename(builddir, "gnc_data_home", strs2[i].output, "foo",
                                        (gchar *)NULL);
                daout = gnc_build_translog_path("foo");
            }
            else // if (strs2[i].prefix_home == 3)
            {
                wantout = g_build_filename(builddir, "gnc_data_home", strs2[i].output, "foo",
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
    }

    /* Further tests should be run assuming we're not in a build environment to test
     * the function's actual behaviour in a real world use case, but still using
     * the fake, writable homedir. */
    g_unsetenv("GNC_BUILDDIR");
    g_unsetenv("GNC_UNINSTALLED");


    /* Second run, with XDG_DATA_HOME set and with existing home_dir, but
     * without the XDG_DATA_HOME subdirectories.
       This test can not be run on OS X or Windows, as our code is not using
       XDG_DATA_HOME on these platforms */
#ifndef MAC_INTEGRATION
#ifndef G_OS_WIN32
    g_mkdir_with_parents(home_dir, 0750);
    userdata_dir = g_build_filename(home_dir, ".local", "share", (gchar *)NULL);
    g_setenv("XDG_DATA_HOME", userdata_dir, TRUE);
    gnc_filepath_init();
    for (i = 0; strs2[i].funcname != NULL; i++)
    {
        char *daout;
        char *wantout;

        if (strs2[i].func_num == 0)
        {
            wantout = g_build_filename(userdata_dir, path_package, "foo",
                                       (gchar *)NULL);
            daout = gnc_build_userdata_path("foo");
        }
        else if (strs2[i].func_num == 1)
        {
            wantout = g_build_filename(userdata_dir, path_package, strs2[i].output, "foo",
                                       (gchar *)NULL);
            daout = gnc_build_book_path("foo");
        }
        else if (strs2[i].func_num == 2)
        {
            wantout = g_build_filename(userdata_dir, path_package, strs2[i].output, "foo",
                                       (gchar *)NULL);
            daout = gnc_build_translog_path("foo");
        }
        else // if (strs2[i].prefix_home == 3)
        {
            wantout = g_build_filename(userdata_dir, path_package, strs2[i].output, "foo",
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
    g_unsetenv("XDG_DATA_HOME");
    /* Remove intermediate directories again */
    tmp_dir = g_build_filename(userdata_dir, path_package, "data", (gchar *)NULL);
    g_rmdir (tmp_dir);
    g_free (tmp_dir);
    tmp_dir = g_build_filename(userdata_dir, path_package, "translog", (gchar *)NULL);
    g_rmdir (tmp_dir);
    g_free (tmp_dir);
    tmp_dir = g_build_filename(userdata_dir, path_package, "books", (gchar *)NULL);
    g_rmdir (tmp_dir);
    g_free (tmp_dir);
    tmp_dir = g_build_filename(userdata_dir, path_package, (gchar *)NULL);
    g_rmdir (tmp_dir);
    g_free (tmp_dir);
    g_rmdir(userdata_dir);
    tmp_dir = g_path_get_dirname(userdata_dir);
    g_rmdir(tmp_dir);
    g_free (tmp_dir);
    g_free(userdata_dir);
#endif
#endif

    /* Third run, with neither XDG_DATA_HOME nor GNC_DATA_HOME set */
    g_unsetenv("GNC_DATA_HOME");
    userdata_dir = test_get_userdatadir();
    gnc_filepath_init();
    for (i = 0; strs2[i].funcname != NULL; i++)
    {
        char *daout;
        char *wantout;

        if (strs2[i].func_num == 0)
        {
            wantout = g_build_filename(userdata_dir, path_package, "foo",
                                       (gchar *)NULL);
            daout = gnc_build_userdata_path("foo");
        }
        else if (strs2[i].func_num == 1)
        {
            wantout = g_build_filename(userdata_dir, path_package, strs2[i].output, "foo",
                                       (gchar *)NULL);
            daout = gnc_build_book_path("foo");
        }
        else if (strs2[i].func_num == 2)
        {
            wantout = g_build_filename(userdata_dir, path_package, strs2[i].output, "foo",
                                       (gchar *)NULL);
            daout = gnc_build_translog_path("foo");
        }
        else // if (strs2[i].prefix_home == 3)
        {
            wantout = g_build_filename(userdata_dir, path_package, strs2[i].output, "foo",
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
    /* Remove intermediate directories again */
    tmp_dir = g_build_filename(userdata_dir, path_package, "data", (gchar *)NULL);
    g_rmdir (tmp_dir);
    g_free (tmp_dir);
    tmp_dir = g_build_filename(userdata_dir, path_package, "translog", (gchar *)NULL);
    g_rmdir (tmp_dir);
    g_free (tmp_dir);
    tmp_dir = g_build_filename(userdata_dir, path_package, "books", (gchar *)NULL);
    g_rmdir (tmp_dir);
    g_free (tmp_dir);
#ifndef MAC_INTEGRATION
#ifndef G_OS_WIN32
    /* Don't delete these on OS X or Windows. They may point at real user directories */
    tmp_dir = g_build_filename(userdata_dir, path_package, (gchar *)NULL);
    g_rmdir (tmp_dir);
    g_free (tmp_dir);
    g_rmdir(userdata_dir);
    tmp_dir = g_path_get_dirname(userdata_dir);
    g_rmdir(tmp_dir);
    g_free (tmp_dir);
#endif
#endif
    g_free(userdata_dir);

    /* Fourth run, with GNC_DATA_HOME */
    gnc_data_home_dir = g_build_filename(home_dir, "Test", NULL);
    g_setenv("GNC_DATA_HOME", gnc_data_home_dir, TRUE);
    gnc_filepath_init();
    for (i = 0; strs2[i].funcname != NULL; i++)
    {
        char *daout;
        char *wantout;

        if (strs2[i].func_num == 0)
        {
            wantout = g_build_filename(gnc_data_home_dir, "foo",
                                       (gchar *)NULL);
            daout = gnc_build_userdata_path("foo");
        }
        else if (strs2[i].func_num == 1)
        {
            wantout = g_build_filename(gnc_data_home_dir, strs2[i].output, "foo",
                                       (gchar *)NULL);
            daout = gnc_build_book_path("foo");
        }
        else if (strs2[i].func_num == 2)
        {
            wantout = g_build_filename(gnc_data_home_dir, strs2[i].output, "foo",
                                       (gchar *)NULL);
            daout = gnc_build_translog_path("foo");
        }
        else // if (strs2[i].prefix_home == 3)
        {
            wantout = g_build_filename(gnc_data_home_dir, strs2[i].output, "foo",
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

    /* Clean up the temporaries that were created for the GNC_DATA_HOME test run */
    g_free (home_dir);
    tmp_dir = g_build_filename(gnc_data_home_dir, "data", (gchar *)NULL);
    g_rmdir (tmp_dir);
    g_free (tmp_dir);
    tmp_dir = g_build_filename(gnc_data_home_dir, "translog", (gchar *)NULL);
    g_rmdir (tmp_dir);
    g_free (tmp_dir);
    tmp_dir = g_build_filename(gnc_data_home_dir, "books", (gchar *)NULL);
    g_rmdir (tmp_dir);
    g_free (tmp_dir);
    g_rmdir (gnc_data_home_dir);
    g_free(gnc_data_home_dir);

    print_test_results();
    return get_rv();
}
