/********************************************************************\
 * register-common.c -- Common functions for the register           *
 * Copyright (c) 2001 Dave Peticolas                                *
 *                                                                  *
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

#include "basiccell.h"
#include "cell-factory.h"
#include "combocell.h"
#include "datecell.h"
#include "formulacell.h"
#include "numcell.h"
#include "pricecell.h"
#include "recncell.h"
#include "checkboxcell.h"
#include "register-common.h"
#include "quickfillcell.h"


static gboolean register_inited = FALSE;
static CellFactory *global_factory = NULL;

void
gnc_register_init (void)
{
    if (register_inited)
        return;

    register_inited = TRUE;

    global_factory = gnc_cell_factory_new ();

    gnc_register_add_cell_type (BASIC_CELL_TYPE_NAME, gnc_basic_cell_new);

    gnc_register_add_cell_type (NUM_CELL_TYPE_NAME, gnc_num_cell_new);

    gnc_register_add_cell_type (PRICE_CELL_TYPE_NAME, gnc_price_cell_new);

    gnc_register_add_cell_type (RECN_CELL_TYPE_NAME, gnc_recn_cell_new);

    gnc_register_add_cell_type (QUICKFILL_CELL_TYPE_NAME,
                                gnc_quickfill_cell_new);

    gnc_register_add_cell_type (FORMULA_CELL_TYPE_NAME,
                                gnc_formula_cell_new);

    gnc_register_add_cell_type (CHECKBOX_CELL_TYPE_NAME, gnc_checkbox_cell_new);
}

void
gnc_register_shutdown (void)
{
    if (!register_inited)
        return;

    gnc_cell_factory_destroy (global_factory);
    global_factory = NULL;
}

void
gnc_register_add_cell_type (const char *cell_type_name,
                            CellCreateFunc cell_creator)
{
    gnc_register_init ();

    gnc_cell_factory_add_cell_type (global_factory,
                                    cell_type_name, cell_creator);
}

BasicCell *
gnc_register_make_cell (const char *cell_type_name)
{
    gnc_register_init ();

    return gnc_cell_factory_make_cell (global_factory, cell_type_name);
}

gboolean
virt_cell_loc_equal (VirtualCellLocation vcl1, VirtualCellLocation vcl2)
{
    return ((vcl1.virt_row == vcl2.virt_row) &&
            (vcl1.virt_col == vcl2.virt_col));
}

gboolean
virt_loc_equal (VirtualLocation vl1, VirtualLocation vl2)
{
    return (virt_cell_loc_equal (vl1.vcell_loc, vl2.vcell_loc) &&
            (vl1.phys_row_offset == vl2.phys_row_offset) &&
            (vl1.phys_col_offset == vl2.phys_col_offset));
}
