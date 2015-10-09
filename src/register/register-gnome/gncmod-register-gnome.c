/*********************************************************************
 * gncmod-register-gnome.c
 * module definition/initialization for Gnome parts of register
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


#include "config.h"

#include <gmodule.h>

#include "combocell.h"
#include "datecell.h"
#include "gnc-module-api.h"
#include "gnc-module.h"
#include "formulacell-gnome.h"
#include "pricecell-gnome.h"
#include "quickfillcell-gnome.h"
#include "register-common.h"
#include "table-gnome.h"

GNC_MODULE_API_DECL(libgncmod_register_gnome)

/* version of the gnc module system interface we require */
int libgncmod_register_gnome_gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
int libgncmod_register_gnome_gnc_module_current  = 0;
int libgncmod_register_gnome_gnc_module_revision = 0;
int libgncmod_register_gnome_gnc_module_age      = 0;


char *
libgncmod_register_gnome_gnc_module_path(void)
{
    return g_strdup("gnucash/register/register-gnome");
}

char *
libgncmod_register_gnome_gnc_module_description(void)
{
    return g_strdup("Gnome GUI for ledger-like table displays");
}

int
libgncmod_register_gnome_gnc_module_init(int refcount)
{
    if (!gnc_module_load("gnucash/register/register-core", 0))
    {
        return FALSE;
    }

    if (!gnc_module_load("gnucash/gnome-utils", 0))
    {
        return FALSE;
    }

    if (refcount == 0)
    {
        gnc_register_add_cell_type (COMBO_CELL_TYPE_NAME, gnc_combo_cell_new);

        gnc_register_add_cell_type (DATE_CELL_TYPE_NAME, gnc_date_cell_new);

        gnc_register_add_cell_type (PRICE_CELL_TYPE_NAME,
                                    gnc_price_cell_gnome_new);

        gnc_register_add_cell_type (QUICKFILL_CELL_TYPE_NAME,
                                    gnc_quickfill_cell_gnome_new);

        gnc_register_add_cell_type( FORMULA_CELL_TYPE_NAME,
                                    gnc_formula_cell_gnome_new );

        gnc_table_gnome_init ();
    }

    return TRUE;
}
