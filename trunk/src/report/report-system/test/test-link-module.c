#include <stdlib.h>
#include <libguile.h>
#include <gnc-module.h>

static void
guile_main(void *closure, int argc, char ** argv)
{
    GNCModule mod;
    gnc_module_system_init();
    mod = gnc_module_load("gnucash/report/report-system", 0);

    exit(mod == NULL);
}

int
main(int argc, char ** argv)
{
    g_setenv ("GNC_UNINSTALLED", "1", TRUE);
    scm_boot_guile(argc, argv, guile_main, NULL);
    return 0;
}

