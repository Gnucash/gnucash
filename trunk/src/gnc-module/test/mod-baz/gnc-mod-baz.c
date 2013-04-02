/* gnc-mod-baz.c : the Gnucash plugin that wraps the library
 * 'libbaz.so'. it does this by being linked against libbaz.so */

#include "config.h"
#include <stdio.h>
#include <gmodule.h>
#include <libguile.h>

#include "gnc-module.h"
#include "gnc-module-api.h"
#include "swig-baz.c"

GNC_MODULE_API_DECL(libgncmodbaz)

int libgncmodbaz_gnc_module_system_interface = 0;

int libgncmodbaz_gnc_module_current = 0;
int libgncmodbaz_gnc_module_age = 0;
int libgncmodbaz_gnc_module_revision = 0;

char *
libgncmodbaz_gnc_module_path(void)
{
    return g_strdup("gnucash/baz");
}

char *
libgncmodbaz_gnc_module_description(void)
{
    return g_strdup("this is the baz module");
}

int
libgncmodbaz_gnc_module_init(int refcount)
{
    /* load libfoo */
    if (gnc_module_load("gnucash/foo", 0))
    {
        /* publish the wrapped Scheme bindings for libbaz */
        scm_init_sw_baz_module();
        scm_c_eval_string("(use-modules (sw_baz))");

        /* use the Scheme "baz" module */
        scm_c_eval_string("(use-modules (gnucash baz))");

        return TRUE;
    }
    else
    {
        return FALSE;
    }
}
