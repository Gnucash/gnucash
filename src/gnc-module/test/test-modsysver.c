#include <stdio.h>
#include <stdlib.h>
#include <libguile.h>

#include "gnc-module.h"

static void
guile_main(void *closure, int argc, char ** argv)
{
    GNCModule foo;

    printf("  test-modsysver.c: checking for a module we shouldn't find ...\n");

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
}

int
main(int argc, char ** argv)
{
    scm_boot_guile(argc, argv, guile_main, NULL);
    return 0;
}
