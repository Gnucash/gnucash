
#ifndef __XACC_CELL_H__
#define __XACC_CELL_H__

#include <Xm/Xm.h>
#include "single.h"

typedef struct _CellArray {

  short numRows;
  short numCols;

  SingleCell ***cells;  /* row-col array */

  Widget reg;          /* the XbaeMatrix */
} CellArray;


CellArray * xaccMallocCellArray (int numrows, int numcols);
void        xaccInitCellArray (CellArray *, int numrows, int numcols);
void        xaccDestroyCellArray (CellArray *);

/* add a cell to the array */
void        xaccAddCell (CellArray *, SingleCell *);

#endif __XACC_CELL_H__
