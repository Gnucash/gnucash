/********************************************************************\
 * register-common.h -- Common declarations for the register        *
 * Copyright (c) 2000 Dave Peticolas                                *
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#ifndef REGISTER_COMMON_H
#define REGISTER_COMMON_H

#include <glib.h>

#include "basiccell.h"

#define BASIC_CELL_TYPE_NAME     "basic-cell"
#define COMBO_CELL_TYPE_NAME     "combo-cell"
#define DATE_CELL_TYPE_NAME      "date-cell"
#define INACTIVE_DATE_CELL_TYPE_NAME "inactive-date-cell"
#define NUM_CELL_TYPE_NAME       "num-cell"
#define PRICE_CELL_TYPE_NAME     "price-cell"
#define RECN_CELL_TYPE_NAME      "recn-cell"
#define QUICKFILL_CELL_TYPE_NAME "quickfill-cell"

void gnc_register_init (void);
void gnc_register_shutdown (void);

void gnc_register_add_cell_type (const char *cell_type_name,
                                 CellCreateFunc cell_creator);
BasicCell * gnc_register_make_cell (const char *cell_type_name);


/* The VirtualCellLocation structure contains the virtual
 * location of a virtual cell.
 */
typedef struct _VirtualCellLocation VirtualCellLocation;
struct _VirtualCellLocation {
  short virt_row;
  short virt_col;
};


gboolean virt_cell_loc_equal (VirtualCellLocation vcl1,
                              VirtualCellLocation vcl2);


/* The VirtualLocation structure contains the virtual
 * location of a physical cell.
 *
 * There is one instance of Locator for each physical cell.
 * The virt_row and virt_col members identify the corresponding
 * cellblock/virtual cell that this physical cell is a member of.
 * The two phys_offsets provide the location of the physical cell
 * as an offset from the cell block origin.  That is, the offsets
 * should never be less than zero, or greater than the size of
 * the cell block.
 */
typedef struct _VirtualLocation VirtualLocation;
struct _VirtualLocation {
  VirtualCellLocation vcell_loc;
  short phys_row_offset;
  short phys_col_offset;
};


gboolean virt_loc_equal (VirtualLocation vl1, VirtualLocation vl2);

#endif
