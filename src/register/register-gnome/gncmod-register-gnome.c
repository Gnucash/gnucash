/*********************************************************************
 * gncmod-register-gnome.c
 * module definition/initialization for Gnome parts of register
 *
 * Copyright (c) 2001 Linux Developers Group, Inc.
 *********************************************************************/

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
