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
/** @addtogroup RegisterCore Register Core
 * @{
 * @addtogroup Cellblock Cellblock
 * @brief A "Cellblock" is an array of active cells. The cells are laid out in
 * rows and columns. The cellblock serves as a convenient container for
 * organizing active cells in an array. Through the mechanism of Cursors
 * (defined below), it allows a group of cells to be treated as a single
 * transactional entity. That is, the cursor/cellblock allows all edits to a
 * groups of cells to be simultaneously committed or rejected by underlying
 * engines. This makes it appropriate for use as a GUI for
 * transaction-processing applications with two-phase commit requirements.
 * @{
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


/** Create a new CellBlock on the heap.
 * @param rows Number of rows.
 * @param cols Number of columns.
 * @param cursor_name A string name for the CellBlock. It will be copied with a
 * new string on the heap.
 * @return a newly-allocated CellBlock which should be deleted with
 * gnc_cellblock_destroy.
 */
CellBlock * gnc_cellblock_new (int rows, int cols, const char *cursor_name);

/** Delete a CellBlock and its Cells.
 * @param cellblock The CellBlock to destroy.
 */
void        gnc_cellblock_destroy (CellBlock *cellblock);

/** Add a cell to the CellBlock at the specified coordinates. The CellBlock
 * takes ownership of the Cell. If there's already a Cell at the location it
 * will be leaked, so callers should first call gnc_cellblock_get_cell() and
 * delete the result if it's not NULL.
 * @param cellblock The CellBlock
 * @param row The row at which to add the cell
 * @param col The column at which to add the cell
 * @param cell The cell to place at the coordinates.
 */
void        gnc_cellblock_set_cell (CellBlock *cellblock,
                                    int row, int col,
                                    BasicCell *cell);

/** Retrieve the Cell at the specified coordinates.
 * @param cellblock The CellBlock
 * @param row The row of the requested Cell
 * @param col The column of the requested Cell
 * @return A pointer to the requested Cell.
 */
BasicCell * gnc_cellblock_get_cell (CellBlock *cellblock,
                                    int row, int col);

/** Searches by name for a particular cell in a CellBlock. Parameters @c row
 *  and/or @c col may be @c NULL.
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

/** Return number of changed cells.
 * @param cursor The cellblock to query
 * @param include_conditional If TRUE counts conditionally-changed cells
 * @return The number of changed cells found.
 */
int         gnc_cellblock_changed (CellBlock *cursor,
                                   gboolean include_conditional);

/** Sets all cells in the cellblock to not changed.
 * @param cursor The cellblock.
 */
void        gnc_cellblock_clear_changes (CellBlock *cursor);

#endif
/** @} */
/** @} */
