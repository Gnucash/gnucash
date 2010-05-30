/* agedver.c : testing module age? */

#include <stdio.h>
#include <gmodule.h>

#include "gnc-module.h"
#include "gnc-module-api.h"

GNC_MODULE_API_DECL(libgncmod_agedver)

int libgncmod_agedver_gnc_module_system_interface = 0;

int libgncmod_agedver_gnc_module_current = 12;
int libgncmod_agedver_gnc_module_age = 9;
int libgncmod_agedver_gnc_module_revision = 0;

char *
libgncmod_agedver_gnc_module_path(void)
{
    return g_strdup("gnucash/agedver");
}

char *
libgncmod_agedver_gnc_module_description(void)
{
    return g_strdup("this is a frequently extended module");
}

int
libgncmod_agedver_gnc_module_init(int refcount)
{
    return TRUE;
}
