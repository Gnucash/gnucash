/* 
 * FILE:
 * cellblock.h
 *
 * FUNCTION:
 * The CellBlock struct is a rectangular grid of cells that 
 * define an arrangement of cells.  It is typically used to
 * define a virtual cursor within a larger table of cells.
 *
 * The CellBlock also has utilities to define a tab group.
 * A tab group is an ordered group of cells that are traversed
 * when the user presses the tab key (and/or uses the arrow 
 * keys).
 *
 * The xaccNextRight() method can be used to declare the
 * traversal order from cell to cell.  If the indicated cell
 * has the current input focus, then the next cell that will
 * be traversed to will be the one indicated.  Traversing
 * to the rright is usally performed with the tab key.
 * Special traversal order to the left, up or down are not
 * currently implemented.  
 *
 * To traverse out of the table entirely,
 * the next_row&col should be set to negative values.  If
 * a traversal back into the table occurs, then the cell that 
 * will be entered will be the one with the negative values 
 * minus one.  Thus to traverse out of the table, then back
 * back to (m,n), set the next row-col to (-m-1,-n-1).
 *
 * MEMBERS:
 * The right_traverse array indicates which cell chould be
 * traversed to when the tab key is pressed.
 */
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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
\********************************************************************/

#ifndef __XACC_CELL_BLOCK_H__
#define __XACC_CELL_BLOCK_H__

#include "basiccell.h"

typedef struct _CellBlock {

  short numRows;
  short numCols;

  BasicCell ***cells;  /* row-col array */

  short     **right_traverse_r;
  short     **right_traverse_c;

  void * user_data;

  /* private, cached utility data */
  short         *widths;        /* column widths */
  unsigned char *alignments;    /* column text alignments */

} CellBlock;


CellBlock * xaccMallocCellBlock (int numrows, int numcols);
void        xaccInitCellBlock (CellBlock *, int numrows, int numcols);
void        xaccDestroyCellBlock (CellBlock *);

/* add a cell to the array */
void        xaccAddCell (CellBlock *, BasicCell *, int row, int col);

/* define next cell to traverse to */
void        xaccNextRight (CellBlock *, int row,      int col, 
                                        int next_row, int next_col);

#endif /* __XACC_CELL_BLOCK_H__ */
