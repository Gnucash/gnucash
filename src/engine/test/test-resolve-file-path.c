#include <stdlib.h>
#include <string.h>

#include <glib.h>

#include "test-stuff.h"
#include "gnc-engine-util.h"
#include "gnc-book.h"

struct test_strings_struct
{
    char *input;
    char *output;
    int prefix_home;
};

typedef struct test_strings_struct test_strings;

test_strings strs[] = {
    { "/.gnucash/test-account-name", "/.gnucash/test-account-name", 1 },
    { "/tmp/test-account-name2", "/tmp/test-account-name2", 0 },
    { "postgres://localhost/foo/bar", "/.gnucash/data/postgres:,,localhost,foo,bar", 2 },
    { "file:/tmp/test-account-name3", "/tmp/test-account-name3", 0 },
    { NULL, NULL, 0 },
};

static const char*
get_home_dir()
{
    return getenv("HOME");
}

int
main(int argc, char **argv)
{
    int i;
    
    for(i = 0; strs[i].input != NULL; i++)
    {
        char *daout;
        char *dain;
        char *wantout;
        
        if(strs[i].prefix_home == 1) 
        {
            dain = g_strdup_printf("%s/%s", get_home_dir(), strs[i].input);
            wantout = g_strdup_printf("%s/%s", get_home_dir(),
                                      strs[i].output);
        }
        else if(strs[i].prefix_home == 2)
        {
            dain = g_strdup(strs[i].input);
            wantout = g_strdup_printf("%s%s", get_home_dir(),
                                      strs[i].output);
        }
         else
        {
            dain = g_strdup(strs[i].input);
            wantout = g_strdup(strs[i].output);
        }

        daout = xaccResolveFilePath(dain);
        do_test_args(safe_strcmp(daout, wantout) == 0,
                     "xaccResolveFilePath",
                     __FILE__, __LINE__,
                     "%s (%s) vs %s", daout, dain, wantout);
        g_free(dain);
        g_free(wantout);
        g_free(daout);
    }
    print_test_results();
    return get_rv();
}

