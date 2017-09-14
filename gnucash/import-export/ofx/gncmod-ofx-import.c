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
\********************************************************************/
/** @addtogroup Import_Export
    @{ */
/**@internal
@file gncmod-ofx-import.c
@brief module definition/initialization for the ofx importer
@author Copyright (c) 2002 Benoit Grégoire bock@step.polymtl.ca
*/
#include "config.h"

#include <gmodule.h>

#include "gnc-ofx-import.h"
#include "gnc-module.h"
#include "gnc-module-api.h"
#include "gnc-plugin-ofx.h"

GNC_MODULE_API_DECL(libgncmod_ofx)

/* version of the gnc module system interface we require */
int libgncmod_ofx_gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
int libgncmod_ofx_gnc_module_current  = 0;
int libgncmod_ofx_gnc_module_revision = 0;
int libgncmod_ofx_gnc_module_age      = 0;

//static GNCModule bus_core;
//static GNCModule file;


char *
libgncmod_ofx_gnc_module_path(void)
{
    return g_strdup("gnucash/import-export/ofx");
}

char *
libgncmod_ofx_gnc_module_description(void)
{
    return g_strdup("Gnome GUI and C code for OFX importer using libofx");
}

int
libgncmod_ofx_gnc_module_init(int refcount)
{
    if (!gnc_module_load("gnucash/engine", 0))
    {
        return FALSE;
    }
    if (!gnc_module_load("gnucash/app-utils", 0))
    {
        return FALSE;
    }
    if (!gnc_module_load("gnucash/gnome-utils", 0))
    {
        return FALSE;
    }
    if (!gnc_module_load("gnucash/import-export", 0))
    {
        return FALSE;
    }

    /* Add menu items with C callbacks */
    gnc_plugin_ofx_create_plugin();

    return TRUE;
}

int
libgncmod_ofx_gnc_module_end(int refcount)
{
    return TRUE;
}
/** @}*/
