#include <glib.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dirent.h>

#include "gnc-engine-util.h"
#include "io-gncxml-v2.h"
#include "gnc-engine.h"
#include "gnc-book.h"

#include "io-example-account.h"

#include "test-stuff.h"
#include "test-engine-stuff.h"
#include "test-file-stuff.h"

static const gchar *da_ending = ".gnucash-xea";

static void
test_load_file(GNCSession *session, const char *filename)
{
    GncExampleAccount *gea;

    gea = gnc_read_example_account(session, filename);

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
guile_main(int argc, char **argv)
{
    const char *location = "../../../../accounts/C";
    GSList *list = NULL;
    DIR *ea_dir;
    GNCSession *session;

    gnc_module_system_init();
    gnc_module_load("gnucash/engine", 0);

    session = gnc_session_new ();

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
                        test_load_file(session, to_open);
                    }
                }
                g_free(to_open);
            }
        }
    }
    closedir(ea_dir);
    
    {
        list = gnc_load_example_account_list(session, location);

        do_test(list != NULL, "gnc_load_example_account_list");
        
        gnc_free_example_account_list(list);
    }
    
    
    print_test_results();
    exit(get_rv());
}

int
main(int argc, char ** argv)
{
  gh_enter (argc, argv, guile_main);
  return 0;
}
