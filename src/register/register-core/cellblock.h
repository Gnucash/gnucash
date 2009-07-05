/********************************************************************\
 * cellblock.h -- group of cells that act as cursor within a table  *
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
 *  @{
 */
/** @file cellblock.h
 *  @brief Declarations for the CellBlock object
 *  @author Copyright (c) 1988 Linas Vepstas
 *  @author Copyright (c) 2000-2001 Dave Peticolas <dave@krondo.com>
 *
 *  @details
 *  The CellBlock struct is a rectangular grid of cells that 
 *  define an arrangement of cells.  It is typically used to
 *  define a virtual cursor within a larger table of cells.
 */

#ifndef XACC_CELL_BLOCK_H
#define XACC_CELL_BLOCK_H

#include "basiccell.h"

#include "gtable.h"

typedef struct
{
  short num_rows;
  short num_cols;

  short start_col;
  short stop_col;

  char *cursor_name;

  GPtrArray *cells; /* Holds the CellBlockCell table */
} CellBlock;


CellBlock * gnc_cellblock_new (int rows, int cols, const char *cursor_name);

void        gnc_cellblock_destroy (CellBlock *cellblock);

void        gnc_cellblock_set_cell (CellBlock *cellblock,
                                    int row, int col,
                                    BasicCell *cell);

BasicCell * gnc_cellblock_get_cell (CellBlock *cellblock,
                                    int row, int col);

/** Searches by name for a particular cell in a CellBlock. Parameters @row
 *  and/or @col may be @c NULL.
 *
 *  @param cellblock a ::CellBlock to search
 *
 *  @param cell_name the name of the cell to find
 *
 *  @param row pointer for returning the row in which the cell was
 *  found, or @c NULL
 *
 *  @param col pointer for returning the column in which the cell was
 *  found, or @c NULL
 *
 *  @return the matching cell, or @c NULL
 */
BasicCell * gnc_cellblock_get_cell_by_name(CellBlock *cellblock,
                                           const char *cell_name,
                                           int *row, int *col);

/* Return number of changed cells. */
int         gnc_cellblock_changed (CellBlock *cursor,
                                   gboolean include_conditional);

void        gnc_cellblock_clear_changes (CellBlock *cursor);

#endif
/** @} */
/** @} */
/** @} */
