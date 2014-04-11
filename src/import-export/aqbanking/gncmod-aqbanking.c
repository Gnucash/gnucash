/*
 * gncmod-aqbanking.c --
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
 */

/**
 * @internal
 * @file gncmod-aqbanking.c
 * @brief Module definition/initialization for AqBanking support
 * @author Copyright (C) 2002 Christian Stimming <stimming@tuhh.de>
 * @author Copyright (C) 2008 Andreas Koehler <andi5.py@gmx.net>
 */

#include "config.h"

#include "gnc-ab-utils.h"
#include "gnc-module.h"
#include "gnc-module-api.h"
#include "gnc-plugin-aqbanking.h"
#include "dialog-preferences.h"

GNC_MODULE_API_DECL(libgncmod_aqbanking)

/* version of the gnc module system interface we require */
gint libgncmod_aqbanking_gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
gint libgncmod_aqbanking_gnc_module_current  = 0;
gint libgncmod_aqbanking_gnc_module_revision = 0;
gint libgncmod_aqbanking_gnc_module_age      = 0;

gchar *
libgncmod_aqbanking_gnc_module_path(void)
{
    return g_strdup("gnucash/import-export/aqbanking");
}

gchar *
libgncmod_aqbanking_gnc_module_description(void) {
    return g_strdup("Support for Online Banking protocols");
}

gint
libgncmod_aqbanking_gnc_module_init(gint refcount)
{
    /* Load modules we depend on */
    if(!gnc_module_load("gnucash/engine", 0)
       || !gnc_module_load("gnucash/app-utils", 0)
       || !gnc_module_load("gnucash/gnome-utils", 0)
       || !gnc_module_load("gnucash/import-export", 0)) {
        return FALSE;
    }

    /* Add menu items with C callbacks */
    gnc_plugin_aqbanking_create_plugin();

    gnc_preferences_add_to_page("aqbanking.glade", "aqbanking_prefs",
                                "Online Banking");

    /* Initialize gwen library */
    gnc_GWEN_Init();

    return 1;
}

gint
libgncmod_aqbanking_gnc_module_end(gint refcount) {
    /* Delete the shared AB_BANKING object */
    gnc_AB_BANKING_delete(NULL);

    /* Finalize gwen library */
    gnc_GWEN_Fini();

    return 1;
}
