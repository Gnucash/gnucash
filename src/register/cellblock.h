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
 * MEMBERS:
 * The right_traverse array indicates which cell chould be
 * traversed to when the tab key is pressed.
 */

#ifndef __XACC_CELL_BLOCK_H__
#define __XACC_CELL_BLOCK_H__

#include "basiccell.h"

typedef struct _CellBlock {

  short numRows;
  short numCols;

  BasicCell ***cells;  /* row-col array */

  short     **right_traverse_r;
  short     **right_traverse_c;

  /* private, utility cahced data */
  short         *widths;        /* column widths */
  unsigned char *alignments;    /* column text alignments */

} CellBlock;


CellBlock * xaccMallocCellBlock (int numrows, int numcols);
void        xaccInitCellBlock (CellBlock *, int numrows, int numcols);

/* add a cell to the array */
void        xaccAddCell (CellBlock *, BasicCell *, int row, int col);

/* define next cell to traverse to */
void        xaccNextRight (CellBlock *, int row,      int col, 
                                        int next_row, int next_col);

#endif __XACC_CELL_BLOCK_H__
