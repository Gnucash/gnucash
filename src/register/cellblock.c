
#include <stdlib.h>
#include "cell.h"

CellBlock * xaccMallocCellBlock (int numrows, int numcols)
{

   CellBlock *arr;
   arr = (CellBlock *) malloc (sizeof (CellBlock));

   arr->cells = NULL;
   arr->widths = NULL;
   arr->alignments = NULL;
   xaccInitCellBlock (arr, numrows, numcols);

   return arr;
}

/* =================================================== */

void        
xaccInitCellBlock (CellBlock *arr, int numrows, int numcols)
{
   int i;

   if (!arr) return;

   arr->numRows = numrows;
   arr->numCols = numcols;

   /* free old cell array, if any */
   if (arr->cells) {
      for (i=0; i<numrows; i++) {
         if (arr->cells[i]) free (arr->cells[i]);
      }
      free (arr->cells);
   }

   /* malloc new cell array */

   arr->cells = (SingleCell ***) malloc (numrows * sizeof (SingleCell **));
   for (i=0; i<numrows; i++) {
      (arr->cells)[i] = (SingleCell **) malloc (numcols * sizeof (SingleCell *));
   }

   /* free old  widths, alignments */
   if (arr->widths) free (arr->widths);
   if (arr->alignments) free (arr->alignments);

   arr->widths = (short *) malloc (numcols * sizeof(short));
   arr->alignments = (unsigned char *) malloc (numcols * sizeof(unsigned char));
   
   for (i=0; i<numcols; i++) {
      arr->widths[i] = 0;
      arr->alignments[i] = 0;
   }
}

/* =================================================== */

void        
xaccAddCell (CellBlock *arr, SingleCell *cell, int row, int col) 
{
   if (!arr) return;
   if (!cell) return;

   cell->row = row;
   cell->col = col;

   /* avoid embarrasement if cell incorrectly specified */
   if ((0 > row) || (0 > col)) return;
   if ((row >= arr->numRows) || (col >= arr->numCols)) return;

   arr->cells[row][col] = cell;
   arr->widths[col] = cell->width;
   arr->alignments[col] = cell->alignment;

   /* install back-pointer to this container */
   cell->block = (struct _CellBlock *) arr;
}

/* --------------- end of file ----------------- */
