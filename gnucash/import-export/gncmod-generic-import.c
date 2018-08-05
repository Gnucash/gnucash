/**@internal
@file
\brief module definition/initialization for the generic import infrastructure
\author Copyright (c) 2002 Benoit Grégoire bock@step.polymtl.ca
*/
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
#include <gmodule.h>
#include <glib/gi18n.h>

#include "dialog-preferences.h"

#include "gnc-module.h"
#include "gnc-module-api.h"

GNC_MODULE_API_DECL(libgncmod_generic_import)

/* version of the gnc module system interface we require */
int libgncmod_generic_import_gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
int libgncmod_generic_import_gnc_module_current  = 0;
int libgncmod_generic_import_gnc_module_revision = 0;
int libgncmod_generic_import_gnc_module_age      = 0;

char *
libgncmod_generic_import_gnc_module_path(void)
{
    return g_strdup("gnucash/import-export");
}

char *
libgncmod_generic_import_gnc_module_description(void)
{
    return g_strdup("Gnome GUI and C code for the generic import functions");
}

int
libgncmod_generic_import_gnc_module_init(int refcount)
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

    if (!refcount)
    {
        /* Add to preferences under Online Banking */
        /* The parameters are; glade file, items to add from glade file - last being the dialog, preference tab name */
        gnc_preferences_add_to_page("dialog-import.glade", "atm_fee_adj,auto_add_adj,auto_clear_adj,match_adj,matcher_prefs",
                                    _("Online Banking"));
    }

    return TRUE;
}

int
libgncmod_generic_import_gnc_module_end(int refcount)
{
    return TRUE;
}
