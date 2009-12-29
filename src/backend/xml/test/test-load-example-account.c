/***************************************************************************
 *            test-load-example-account.c
 *
 *  Thu Sep 29 22:52:32 2005
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
#include <glib.h>
#include <libguile.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dirent.h>

#include "gnc-module.h"
#include "gnc-engine.h"
#include "io-gncxml-v2.h"

#include "io-example-account.h"

#include "test-stuff.h"
#include "test-engine-stuff.h"
#include "test-file-stuff.h"

static const gchar *da_ending = ".gnucash-xea";

static void
test_load_file(const char *filename)
{
    GncExampleAccount *gea;

    gea = gnc_read_example_account(filename);

    if (gea != NULL)
    {
        success("example account load");
        gnc_destroy_example_account(gea);
    }
    else
    {
        failure_args("example account load", __FILE__, __LINE__, "for file %s",
                     filename);
    }
}

static void
guile_main (void *closure, int argc, char **argv)
{
    const char *location = g_getenv("GNC_ACCOUNT_PATH");
    GSList *list = NULL;
    GDir *ea_dir;

    if (!location)
    {
        location = "../../../../accounts/C";
    }

    gnc_module_system_init();
    gnc_module_load("gnucash/engine", 0);

    if ((ea_dir = g_dir_open(location, 0, NULL)) == NULL)
    {
        failure("unable to open ea directory");
    }
    else
    {
        const gchar *entry;

        while ((entry = g_dir_read_name(ea_dir)) != NULL)
        {
            if (g_str_has_suffix(entry, da_ending))
            {
                gchar *to_open = g_build_filename(location, entry, (gchar*)NULL);
                if (!g_file_test(to_open, G_FILE_TEST_IS_DIR))
                {
                    test_load_file(to_open);
                }
                g_free(to_open);
            }
        }
    }
    g_dir_close(ea_dir);

    {
        list = gnc_load_example_account_list(location);

        do_test(list != NULL, "gnc_load_example_account_list");

        gnc_free_example_account_list(list);
    }


    print_test_results();
    exit(get_rv());
}

int
main (int argc, char ** argv)
{
    scm_boot_guile (argc, argv, guile_main, NULL);
    exit(get_rv());
}
