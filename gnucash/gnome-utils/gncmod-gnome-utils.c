/*********************************************************************
 * gncmod-gnome-utils.c
 * module definition/initialization for the gnome utilities
 *
 * Copyright (c) 2001 Linux Developers Group, Inc.
 *********************************************************************/
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
#include <libguile.h>

#include "gnc-module-api.h"

#include "gnc-component-manager.h"
#include "dialog-options.h"

GNC_MODULE_API_DECL(libgncmod_gnome_utils)

/* version of the gnc module system interface we require */
int libgncmod_gnome_utils_gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
int libgncmod_gnome_utils_gnc_module_current  = 0;
int libgncmod_gnome_utils_gnc_module_revision = 0;
int libgncmod_gnome_utils_gnc_module_age      = 0;


char *
libgncmod_gnome_utils_gnc_module_path(void)
{
    return g_strdup("gnucash/gnome-utils");
}

char *
libgncmod_gnome_utils_gnc_module_description(void)
{
    return g_strdup("Utilities for using Gnome/Gtk with GnuCash");
}

extern SCM scm_init_sw_gnome_utils_module(void);

int
libgncmod_gnome_utils_gnc_module_init(int refcount)
{
    scm_init_sw_gnome_utils_module();
    scm_c_use_module ("sw_gnome_utils");
    scm_c_use_module("gnucash gnome-utils");

    /* Initialize the options-ui database */
    if (refcount == 0)
    {
        gnc_component_manager_init ();
        gnc_options_ui_initialize ();
    }

    return TRUE;
}

int
libgncmod_gnome_utils_gnc_module_end(int refcount)
{
    if (refcount == 0)
        gnc_component_manager_shutdown ();

    return TRUE;
}
