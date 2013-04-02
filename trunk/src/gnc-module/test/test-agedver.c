#include "config.h"
#include <stdio.h>
#include <stdlib.h>
#include <libguile.h>

#include "gnc-module.h"
#include <unittest-support.h>

static void
guile_main(void *closure, int argc, char ** argv)
{
    GNCModule foo;
    gchar *msg = "Module '../../../src/gnc-module/test/misc-mods/.libs/libgncmod_futuremodsys.so' requires newer module system\n";
    gchar *logdomain = "gnc.module";
    guint loglevel = G_LOG_LEVEL_WARNING;
    TestErrorStruct check = { loglevel, logdomain, msg };
    g_log_set_handler (logdomain, loglevel,
                       (GLogFunc)test_checked_handler, &check);
    g_test_message("  test-agedver.c:  asking for an old but supported interface ...");

    gnc_module_system_init();

    foo = gnc_module_load("gnucash/agedver", 5);

    if (foo)
    {
        printf("  ok\n");
        exit(0);
    }
    else
    {
        printf(" failed\n");
        exit(-1);
    }
}

int
main(int argc, char ** argv)
{
    scm_boot_guile(argc, argv, guile_main, NULL);
    return 0;
}
