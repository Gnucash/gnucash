/* gnc-mod-bar.c : the Gnucash plugin that wraps the library
 * 'libbar.so'. it does this by being linked against libbar.so */
/********************************************************************\
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/


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
