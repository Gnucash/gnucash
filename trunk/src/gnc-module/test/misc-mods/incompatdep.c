/* incompatdep.c : a gnucash module that depends on an incompatible
 * version of another module. the initialization should fail. */

#include <stdio.h>
#include <gmodule.h>

#include "gnc-module.h"
#include "gnc-module-api.h"

GNC_MODULE_API_DECL(libgncmod_incompatdep)

int libgncmod_incompatdep_gnc_module_system_interface = 0;

int libgncmod_incompatdep_gnc_module_current = 0;
int libgncmod_incompatdep_gnc_module_age = 0;
int libgncmod_incompatdep_gnc_module_revision = 0;


char *
libgncmod_incompatdep_gnc_module_path(void)
{
    return g_strdup("gnucash/incompatdep");
}

char *
libgncmod_incompatdep_gnc_module_description(void)
{
    return g_strdup("this is a broken module");
}

int
libgncmod_incompatdep_gnc_module_init(int refcount)
{
    if (gnc_module_load("gnucash/foo", 25))
    {
        return TRUE;
    }
    else
    {
        return FALSE;
    }
}
