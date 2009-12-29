/* gnc-mod-bar.c : the Gnucash plugin that wraps the library
 * 'libbar.so'. it does this by being linked against libbar.so */

#include "config.h"
#include <stdio.h>
#include <gmodule.h>
#include <libguile.h>

#include "gnc-module-api.h"
#include "swig-bar.c"

GNC_MODULE_API_DECL(libgncmodbar)

int libgncmodbar_gnc_module_system_interface = 0;

int libgncmodbar_gnc_module_current = 0;
int libgncmodbar_gnc_module_age = 0;
int libgncmodbar_gnc_module_revision = 0;

char *
libgncmodbar_gnc_module_path(void)
{
    return g_strdup("gnucash/bar");
}

char *
libgncmodbar_gnc_module_description(void)
{
    return g_strdup("this is a bar module");
}

int
libgncmodbar_gnc_module_init(int refcount)
{
    /* publish the wrapped Scheme bindings for libbar */
    scm_init_sw_bar_module();
    scm_c_eval_string("(use-modules (sw_bar))");

    /* use the Scheme "bar" module */
    scm_c_eval_string("(use-modules (gnucash bar))");

    return TRUE;
}
