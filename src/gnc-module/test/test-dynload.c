/*********************************************************************
 * test-dynload.c
 * test the ability to dlopen the gnc_module library and initialize
 * it via dlsym
 *********************************************************************/

#include "config.h"
#include <stdio.h>
#include <gmodule.h>
#include <libguile.h>
#include <unittest-support.h>

#include "gnc-module.h"

static void
guile_main(void *closure, int argc, char ** argv)
{
    GModule *gmodule;
    gchar *msg = "Module '../../../src/gnc-module/test/misc-mods/.libs/libgncmod_futuremodsys.so' requires newer module system\n";
    gchar *logdomain = "gnc.module";
    guint loglevel = G_LOG_LEVEL_WARNING;
    TestErrorStruct check = { loglevel, logdomain, msg };
    g_log_set_handler (logdomain, loglevel,
                       (GLogFunc)test_checked_handler, &check);

    g_test_message("  test-dynload.c: testing dynamic linking of libgnc-module ...");
    gmodule = g_module_open("libgnc-module", 0);

    /* Maybe MacOS? */
    if (!gmodule)
        gmodule = g_module_open("libgnc-module.dylib", 0);

    if (gmodule)
    {
        gpointer ptr;
        if (g_module_symbol(gmodule, "gnc_module_system_init", &ptr))
        {
            void (* fn)(void) = ptr;
            fn();
            printf(" OK\n");
            exit(0);
        }
        else
        {
            printf(" failed to find gnc_module_system_init\n");
            exit(-1);
        }
    }
    else
    {
        printf(" failed to open library.\n");
        printf("%s\n", g_module_error());
        exit(-1);
    }
}

int
main(int argc, char ** argv)
{
    scm_boot_guile(argc, argv, guile_main, NULL);
    return 0;
}

