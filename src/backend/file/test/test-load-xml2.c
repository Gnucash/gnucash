
/* @file test-load-xml2.c
 * @breif test the loading of a vrsion-2 gnucash XML file
 */

#include <glib.h>
#include <libguile.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dirent.h>
#include <string.h>

#include "qofsession.h"
#include "Group.h"
#include "TransLog.h"
#include "gnc-engine.h"
#include "gnc-backend-file.h"
#include "qofsession-p.h"
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
    QofSession *session;
    QofBook *book;
    QofBackend *be;
    gboolean ignore_lock;

    session = qof_session_new();

    remove_locks(filename);

    ignore_lock = (strcmp(getenv("SRCDIR"), ".") != 0);
    qof_session_begin(session, filename, ignore_lock, FALSE);

    book = qof_session_get_book (session);
    be = qof_session_get_backend (session);
    qof_session_load_from_xml_file_v2((FileBackend *)be, book);

    do_test (xaccGroupGetBook (xaccGetAccountGroup (book)) == book,
             "book and group don't match");

    do_test_args(
        qof_session_get_error(session) == ERR_BACKEND_NO_ERR,
        "session load xml2", __FILE__, __LINE__, "%d for file %s",
        qof_session_get_error(session), filename);

    qof_session_destroy(session);
}

static void
guile_main (void *closure, int argc, char **argv)
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
main (int argc, char ** argv)
{
  /*  getchar (); */

  scm_boot_guile(argc, argv, guile_main, NULL);

  return 0;
}
