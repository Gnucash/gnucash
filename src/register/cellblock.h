
#ifndef __XACC_CELL_H__
#define __XACC_CELL_H__

#include "single.h"

/* a cell array is a traversal group for one entry in the register */

typedef struct _CellBlock {

  short numRows;
  short numCols;

  SingleCell ***cells;  /* row-col array */

  /* private, utility cahced data */
  short         *widths;        /* column widths */
  unsigned char *alignments;    /* column text alignments */

} CellBlock;


CellBlock * xaccMallocCellBlock (int numrows, int numcols);
void        xaccInitCellBlock (CellBlock *, int numrows, int numcols);

/* add a cell to the array */
void        xaccAddCell (CellBlock *, SingleCell *);

#endif __XACC_CELL_H__
