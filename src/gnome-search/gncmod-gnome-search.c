/*********************************************************************
 * gncmod-gnome-search
 * GNC Module initialization for the Gnome Search UI
 *
 * Copyright (c) 2002 Derek Atkins <warlord@MIT.EDU>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 *********************************************************************/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <gtk/gtk.h>
#include <gmodule.h>
#include <libguile.h>

#include "gnc-module.h"
#include "gnc-module-api.h"

#include "search-core-type.h"

GNC_MODULE_API_DECL(libgncmod_gnome_search)

/* version of the gnc module system interface we require */
int libgncmod_gnome_search_gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
int libgncmod_gnome_search_gnc_module_current  = 0;
int libgncmod_gnome_search_gnc_module_revision = 0;
int libgncmod_gnome_search_gnc_module_age      = 0;


char *
libgncmod_gnome_search_gnc_module_path(void)
{
    return g_strdup("gnucash/gnome-search");
}

char *
libgncmod_gnome_search_gnc_module_description(void)
{
    return g_strdup("The GnuCash Gnome Search UI");
}

int
libgncmod_gnome_search_gnc_module_init(int refcount)
{
    /* load the engine (we depend on it) */
    if (!gnc_module_load("gnucash/engine", 0))
    {
        return FALSE;
    }

    if (!gnc_module_load("gnucash/gnome-utils", 0))
    {
        return FALSE;
    }

    if (refcount == 0)
    {
        /* initialize known types */
        gnc_search_core_initialize ();
    }

    return TRUE;
}

int
libgncmod_gnome_search_gnc_module_end(int refcount)
{
    /* XXX Unload the other modules */

    if (refcount == 0)
    {
        /* Shutdown */
        gnc_search_core_finalize ();
    }

    return TRUE;
}
