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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/
/** @addtogroup GUI
 *  @{
 */
/** @addtogroup Register Registers, Ledgers and Journals
 *  @{
 */
/** @addtogroup RegisterCore Register Core
 *  @brief An infrastructure for building a modular matrix of cells like a
 *  spreadsheet or checkbook register.
 * 
 *  @details Each cell may be specialized to perform a particular function,
 *  e.g. to read dates, numerical amounts, or text. The register core has been
 *  designed to be easy to extend, modular, easy to maintain, and memory
 *  efficient. It is intended to be used for building financial apps and
 *  spreadsheets.
 *
 *  The register core is built from several types of components: Cells,
 *  Cellblocks, Cursors, and the Table.
 *
 *  The register core should not have any 'knowledge' of the accounting model
 *  of GnuCash or of the workings of the main application. It should not be
 *  specific to a particular GUI (such as Gnome/GTK). It should be possible
 *  to use the register core in a stand-alone fashion.
 *
 *  For information on the GnuCash-specific register implementation that has
 *  been built atop this core, see @ref SplitRegister.
 *
 *  @{
 */
/** @file register-common.h
 *  @brief Common declarations for the register core
 *  @author Copyright (c) 2000 Dave Peticolas
 */

#ifndef REGISTER_COMMON_H
#define REGISTER_COMMON_H

#include <glib.h>

#include "basiccell.h"

#define BASIC_CELL_TYPE_NAME     "basic-cell"
#define COMBO_CELL_TYPE_NAME     "combo-cell"
#define DATE_CELL_TYPE_NAME      "date-cell"
#define NUM_CELL_TYPE_NAME       "num-cell"
#define PRICE_CELL_TYPE_NAME     "price-cell"
#define RECN_CELL_TYPE_NAME      "recn-cell"
#define QUICKFILL_CELL_TYPE_NAME "quickfill-cell"
#define FORMULA_CELL_TYPE_NAME   "formula-cell"
#define CHECKBOX_CELL_TYPE_NAME	 "checkbox-cell"

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
  int virt_row;
  int virt_col;
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
  int phys_row_offset;
  int phys_col_offset;
};


gboolean virt_loc_equal (VirtualLocation vl1, VirtualLocation vl2);

#endif
/** @} */
/** @} */
/** @} */
