/*********************************************************************
 * gncmod-bi_import.c
 * module definition/initialization for the bi_import GNOME UI module
 *
 * Copyright (c) 2009 Sebastian Held <sebastian.held@gmx.de>
 * Copyright (c) 2001 Derek Atkins <warlord@MIT.EDU>
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
 *
 *********************************************************************/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <gmodule.h>
#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <libguile.h>

#include "gnc-hooks.h"
#include "gnc-module.h"
#include "gnc-module-api.h"

#include "gnc-plugin-manager.h"
#include "gnc-plugin-bi_import.h"

GNC_MODULE_API_DECL(libgncmod_bi_import);

/* version of the gnc module system interface we require */
int libgncmod_bi_import_gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
int libgncmod_bi_import_gnc_module_current  = 0;
int libgncmod_bi_import_gnc_module_revision = 0;
int libgncmod_bi_import_gnc_module_age      = 0;


char *
libgncmod_bi_import_gnc_module_path (void)
{
    return g_strdup("gnucash/plugins/bi_import");
}

char *
libgncmod_bi_import_gnc_module_description (void)
{
    return g_strdup("The GnuCash bi_import plugin");
}

int
libgncmod_bi_import_gnc_module_init (int refcount)
{
    if (!gnc_module_load ("gnucash/app-utils", 0)) {
        return FALSE;
    }
    if (!gnc_module_load ("gnucash/gnome-utils", 0)) {
        return FALSE;
    }
    if (!gnc_module_load ("gnucash/business-core", 0)) {
        return FALSE;
    }
    if (!gnc_module_load ("gnucash/engine", 0)) {
        return FALSE;
    }

    if (refcount == 0) {
        /* this is the first time the module is loaded */

        gnc_plugin_manager_add_plugin ( gnc_plugin_manager_get (),
                   gnc_plugin_bi_import_new ());
    }

    return TRUE;
}

int
libgncmod_bi_import_gnc_module_end (int refcount)
{
    if (refcount == 0) {
        /* this is the last time the module is unloaded */
    }

    return TRUE;
}
