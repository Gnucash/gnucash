/* futuremodsys.c : a gnucash module compiled with a future version of
 * the module system.  gnucash should not be able to load it.  but if
 * it doesn't notice that, the actual interface is compatible with
 * version 0 so it will load all the way before failing. */

#include <stdio.h>
#include <gmodule.h>

#include "gnc-module-api.h"
GNC_MODULE_API_DECL(libfuturemodsys)

int libfuturemodsys_gnc_module_system_interface = 123456;

int libfuturemodsys_gnc_module_current = 0;
int libfuturemodsys_gnc_module_age = 0;
int libfuturemodsys_gnc_module_revision = 0;


char *
libfuturemodsys_gnc_module_path(void)
{
    return g_strdup("gnucash/futuremodsys");
}

char *
libfuturemodsys_gnc_module_description(void)
{
    return g_strdup("this is a broken future module");
}

int
libfuturemodsys_gnc_module_init(int refcount)
{
    return TRUE;
}
