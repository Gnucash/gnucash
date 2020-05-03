/* gnc-mod-baz.c : the Gnucash plugin that wraps the library
 * 'libbaz.so'. it does this by being linked against libbaz.so */
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


#include <config.h>
#include <stdio.h>
#include <gmodule.h>
#include <libguile.h>

#include "gnc-module.h"
#include "gnc-module-api.h"
#include "swig-baz.c"

GNC_MODULE_API_DECL(libgncmod_baz)

int libgncmod_baz_gnc_module_system_interface = 0;

int libgncmod_baz_gnc_module_current = 0;
int libgncmod_baz_gnc_module_age = 0;
int libgncmod_baz_gnc_module_revision = 0;

char *
libgncmod_baz_gnc_module_path(void)
{
    return g_strdup("gnucash/baz");
}

char *
libgncmod_baz_gnc_module_description(void)
{
    return g_strdup("this is the baz module");
}

int
libgncmod_baz_gnc_module_init(int refcount)
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
