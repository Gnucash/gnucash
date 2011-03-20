#include <stdio.h>
#include <stdlib.h>
#include <libguile.h>

#include "gnc-module.h"

static void
guile_main(void *closure, int argc, char ** argv)
{
    GNCModule foo;

    printf("  test-agedver.c:  asking for an old but supported interface ...");

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
