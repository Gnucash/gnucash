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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#include "register-common.h"


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
