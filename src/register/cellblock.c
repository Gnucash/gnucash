
#include <Xm/Xm.h>
#include "single.h"

typedef struct _CellArray {

  short numRows;
  short numCols;

  SingleCell **cells;  /* row-col array */

  Widget reg;          /* the XbaeMatrix */
} CellArray;


CellArray * xaccMallocCellArray (void);
void        xaccInitCellArray (CellArray *, int numrows, int numcols);
void        xaccDestroyCellArray (CellArray *);

/* add a cell to the array */
void        xaccAddCell (SingleCell *);


