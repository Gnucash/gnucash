
#ifndef __XACC_CELL_BLOCK_H__
#define __XACC_CELL_BLOCK_H__

#include "basiccell.h"

/* 
 * The CellBlock is a rectangular grid of cells that define
 * a traversal group for one entry in the register 
 *
 * The right_traverse array indicates which cell chould be
 * traversed to when the tab key is pressed.
 */

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
