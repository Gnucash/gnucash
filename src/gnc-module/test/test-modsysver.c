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
    gchar *msg1 = "Module '../../../src/gnc-module/test/misc-mods/.libs/libgncmod_futuremodsys.so' requires newer module system\n";
    gchar *msg2 = "Could not locate module gnucash/futuremodsys interface v.0";
    gchar *logdomain = "gnc.module";
    guint loglevel = G_LOG_LEVEL_WARNING;
    TestErrorStruct check1 = { loglevel, logdomain, msg1 };
    TestErrorStruct check2 = { loglevel, logdomain, msg2 };
    test_add_error (&check1);
    test_add_error (&check2);
    g_log_set_handler (logdomain, loglevel,
                       (GLogFunc)test_list_handler, NULL);

    g_test_message("  test-modsysver.c: checking for a module we shouldn't find ...\n");

    gnc_module_system_init();

    foo = gnc_module_load("gnucash/futuremodsys", 0);

    if (!foo)
    {
        printf("  ok\n");
        exit(0);
    }
    else
    {
        printf("  oops! loaded incompatible module\n");
        exit(-1);
    }
    test_clear_error_list ();
}

int
main(int argc, char ** argv)
{
    scm_boot_guile(argc, argv, guile_main, NULL);
    return 0;
}
