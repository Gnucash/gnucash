
#ifndef __XACC_CELL_H__
#define __XACC_CELL_H__

#include "single.h"

/* a cell array is a traversal group for one entry in the register */

typedef struct _CellArray {

  short numRows;
  short numCols;

  SingleCell ***cells;  /* row-col array */
} CellArray;


CellArray * xaccMallocCellArray (int numrows, int numcols);
void        xaccInitCellArray (CellArray *, int numrows, int numcols);

/* add a cell to the array */
void        xaccAddCell (CellArray *, SingleCell *);

#endif __XACC_CELL_H__
