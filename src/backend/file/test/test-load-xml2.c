#include <glib.h>
#include <guile/gh.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dirent.h>
#include <string.h>

#include "Backend.h"
#include "Group.h"
#include "TransLog.h"
#include "gnc-session.h"
#include "gnc-engine.h"
#include "gnc-module.h"
#include "io-gncxml-v2.h"

#include "test-stuff.h"
#include "test-engine-stuff.h"
#include "test-file-stuff.h"

static void
remove_files_pattern(const char *begining, const char *ending)
{
}

static void
remove_locks(const char *filename)
{
    struct stat buf;
    char *to_remove;
    
    {
        to_remove = g_strdup_printf("%s.LCK", filename);
        if(stat(to_remove, &buf) != -1)
        {
            unlink(to_remove);
        }
        g_free(to_remove);
    }
    
    remove_files_pattern(filename, ".LCK");
}

static void
test_load_file(const char *filename)
{
    GNCSession *session;
    GNCBook *book;

    session = gnc_session_new();

    remove_locks(filename);

    gnc_session_begin(session, filename, TRUE, FALSE);

    gnc_session_load_from_xml_file_v2(session, NULL);

    book = gnc_session_get_book (session);

    do_test (xaccGroupGetBook (gnc_book_get_group (book)) == book,
             "book and group don't match");

    do_test_args(
        gnc_session_get_error(session) == ERR_BACKEND_NO_ERR,
        "session load xml2", __FILE__, __LINE__, "%d for file %s",
        gnc_session_get_error(session), filename);

    gnc_session_destroy(session);
}

static void
guile_main(int argc, char **argv)
{
    const char *location = getenv("GNC_TEST_FILES");
    DIR *xml2_dir;

    if (!location)
    {
	location = "test-files/xml2";
    }

    gnc_module_system_init();
    gnc_module_load("gnucash/engine", 0);

    xaccLogDisable();
    
    if((xml2_dir = opendir(location)) == NULL)
    {
        failure("unable to open xml2 directory");
    }
    else
    {
        struct dirent *entry;

        while((entry = readdir(xml2_dir)) != NULL)
        {
            if(strstr(entry->d_name, ".gml2") != NULL)
            {
                struct stat file_info;
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
                        test_load_file(to_open);
                    }
                }
                g_free(to_open);
            }
        }
    }

    closedir(xml2_dir);

    print_test_results();
    exit(get_rv());
}

int
main(int argc, char ** argv)
{
  /*  getchar (); */

  gh_enter(argc, argv, guile_main);

  return 0;
}
