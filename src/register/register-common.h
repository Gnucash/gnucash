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

#ifndef __REGISTER_COMMON_H__
#define __REGISTER_COMMON_H__

#include <glib.h>


/* The VirtualCellLocation structure contains the virtual
 * location of a virtual cell.
 */
typedef struct _VirtualCellLocation VirtualCellLocation;
struct _VirtualCellLocation {
  short virt_row;
  short virt_col;
};


G_INLINE_FUNC gboolean
virt_cell_loc_equal (VirtualCellLocation vcl1, VirtualCellLocation vcl2);
G_INLINE_FUNC gboolean
virt_cell_loc_equal (VirtualCellLocation vcl1, VirtualCellLocation vcl2)
{
  return ((vcl1.virt_row == vcl2.virt_row) &&
          (vcl1.virt_col == vcl2.virt_col));
}


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


G_INLINE_FUNC gboolean
virt_loc_equal (VirtualLocation vl1, VirtualLocation vl2);
G_INLINE_FUNC gboolean
virt_loc_equal (VirtualLocation vl1, VirtualLocation vl2)
{
  return (virt_cell_loc_equal (vl1.vcell_loc, vl2.vcell_loc) &&
          (vl1.phys_row_offset == vl2.phys_row_offset) &&
          (vl1.phys_col_offset == vl2.phys_col_offset));
}

#endif
