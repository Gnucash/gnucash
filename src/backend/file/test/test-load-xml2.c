#include <glib.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dirent.h>
#include <string.h>

#include "Backend.h"
#include "TransLog.h"
#include "gnc-book.h"
#include "gnc-book.h"
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
    GNCBook *book;

    book = gnc_book_new();

    remove_locks(filename);
    
    gnc_book_begin(book, filename, FALSE, FALSE);
    
    gnc_book_load_from_xml_file_v2(book, NULL);

    do_test_args(
        gnc_book_get_error(book) == ERR_BACKEND_NO_ERR,
        "book load xml2", __FILE__, __LINE__, "%d for file %s",
        gnc_book_get_error(book), filename);

    gnc_book_destroy(book);
}

static void
guile_main(int argc, char **argv)
{
    const char *location = "test-files/xml2";
    DIR *xml2_dir;
    
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
main(int argc, char ** argv) {
  gh_enter(argc, argv, guile_main);
  return 0;
}
