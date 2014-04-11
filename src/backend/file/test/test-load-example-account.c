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
test_load_file(QofBook *book, const char *filename)
{
    GncExampleAccount *gea;

    gea = gnc_read_example_account(book, filename);

    if(gea != NULL)
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
    const char *location = getenv("GNC_ACCOUNT_PATH");
    GSList *list = NULL;
    DIR *ea_dir;
    QofBook *book;

    if (!location)
    {
        location = "../../../../accounts/C";
    }

    gnc_module_system_init();
    gnc_module_load("gnucash/engine", 0);

    book = qof_book_new ();

    if((ea_dir = opendir(location)) == NULL)
    {
        failure("unable to open ea directory");
    }
    else
    {
        struct dirent *entry;

        while((entry = readdir(ea_dir)) != NULL)
        {
            struct stat file_info;
            if(strstr(entry->d_name, da_ending) != NULL)
            {
                char *to_open = g_strdup_printf("%s/%s", location,
                                                entry->d_name);
                if(stat(to_open, &file_info) != 0)
                {
                    failure("unable to stat file");
                }
                else
                {
                    if(!S_ISDIR(file_info.st_mode))
                    {
                        test_load_file(book, to_open);
                    }
                }
                g_free(to_open);
            }
        }
    }
    closedir(ea_dir);
    
    {
        list = gnc_load_example_account_list(book, location);

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
