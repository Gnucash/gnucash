/*********************************************************************
 * gncmod-dialog-tax-table.c
 * module definition/initialization for the Business Tax Table Dialog module
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

#include "config.h"
#include <gmodule.h>
#include <libguile.h>

#include "gnc-module.h"
#include "gnc-module-api.h"

GNC_MODULE_API_DECL(libgncmod_dialog_tax_table)

extern SCM scm_init_sw_dialog_tax_table_module(void);
/* version of the gnc module system interface we require */
int libgncmod_dialog_tax_table_gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
int libgncmod_dialog_tax_table_gnc_module_current  = 0;
int libgncmod_dialog_tax_table_gnc_module_revision = 0;
int libgncmod_dialog_tax_table_gnc_module_age      = 0;


char *
libgncmod_dialog_tax_table_gnc_module_path(void)
{
    return g_strdup("gnucash/dialog-tax-table");
}

char *
libgncmod_dialog_tax_table_gnc_module_description(void)
{
    return g_strdup("The GnuCash tax-table GNOME UI module");
}

int
libgncmod_dialog_tax_table_gnc_module_init(int refcount)
{
    /* load business-core: we depend on it -- and it depends on the engine */
    if (!gnc_module_load ("gnucash/business-core", 0))
    {
        return FALSE;
    }
    /* We also depend on app-utils and gnome-utils modules */
    if (!gnc_module_load ("gnucash/app-utils", 0))
    {
        return FALSE;
    }
    if (!gnc_module_load ("gnucash/gnome-utils", 0))
    {
        return FALSE;
    }

    scm_init_sw_dialog_tax_table_module();
    scm_c_eval_string("(use-modules (sw_dialog_tax_table))");

    return TRUE;
}

int
libgncmod_dialog_tax_table_gnc_module_end(int refcount)
{
    return TRUE;
}
