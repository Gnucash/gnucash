
#include <stdlib.h>
#include "cell.h"

CellArray * xaccMallocCellArray (int numrows, int numcols)
{

   CellArray *arr;
   arr = (CellArray *) malloc (sizeof (CellArray *));

   arr->cells = NULL;
   xaccInitCellArray (arr, numrows, numcols);

   return arr;
}

/* =================================================== */

void        
xaccInitCellArray (CellArray *arr, int numrows, int numcols)
{
   int i;

   arr->numRows = numrows;
   arr->numCols = numcols;

   if (arr->cells) {
      for (i=0; i<numrows; i++) {
         if (arr->cells[i]) free (arr->cells[i]);
      }
      free (arr->cells);
   }

   arr->cells = (SingleCell ***) malloc (numrows * sizeof (SingleCell **));
   for (i=0; i<numrows; i++) {
      arr->cells[i] = (SingleCell **) malloc (numcols * sizeof (SingleCell *));
   }
}

/* =================================================== */

void        
xaccAddCell (CellArray *arr, SingleCell *cell) 
{
   int i,j;

   i = cell->row;
   j = cell->col;

   arr->cells[i][j] = cell;
}

/* --------------- end of file ----------------- */
