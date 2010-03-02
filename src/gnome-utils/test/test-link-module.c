#include <stdlib.h>
#include <libguile.h>
#include <gnc-module.h>

static void
guile_main(void *closure, int argc, char ** argv)
{
    gnc_module_system_init();
    /*  gnc_module_load("gnucash/gnome-utils", 0); */
    exit(0);
}

int
main(int argc, char ** argv)
{
    scm_boot_guile(argc, argv, guile_main, NULL);
    return 0;
}
