/* futuremodsys.c : a gnucash module compiled with a future version of
 * the module system.  gnucash should not be able to load it.  but if
 * it doesn't notice that, the actual interface is compatible with
 * version 0 so it will load all the way before failing. */
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


#include <stdio.h>
#include <gmodule.h>

#include "gnc-module-api.h"
GNC_MODULE_API_DECL(libgncmod_futuremodsys)

int libgncmod_futuremodsys_gnc_module_system_interface = 123456;

int libgncmod_futuremodsys_gnc_module_current = 0;
int libgncmod_futuremodsys_gnc_module_age = 0;
int libgncmod_futuremodsys_gnc_module_revision = 0;


char *
libgncmod_futuremodsys_gnc_module_path(void)
{
    return g_strdup("gnucash/futuremodsys");
}

char *
libgncmod_futuremodsys_gnc_module_description(void)
{
    return g_strdup("this is a broken future module");
}

int
libgncmod_futuremodsys_gnc_module_init(int refcount)
{
    return TRUE;
}
