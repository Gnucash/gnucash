#include "config.h"
#include <stdio.h>
#include <stdlib.h>
#include <libguile.h>
#include <unittest-support.h>

#include "gnc-module.h"

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

    g_test_message("  test-load-c.c: testing module load/unload from C ... ");

    gnc_module_system_init();

    foo = gnc_module_load("gnucash/foo", 0);

    if (!foo)
    {
        g_test_message("  Failed to load foo\n");
        exit(-1);
    }

    if (!gnc_module_unload(foo))
    {
        g_test_message("  Failed to unload foo\n");
        exit(-1);
    }
    g_test_message(" successful.\n");

    exit(0);
}

int
main(int argc, char ** argv)
{
    scm_boot_guile(argc, argv, guile_main, NULL);
    return 0;
}
