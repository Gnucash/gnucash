
#include <stdlib.h>
#include "cell.h"

CellBlock * xaccMallocCellBlock (int numrows, int numcols)
{

   CellBlock *arr;
   arr = (CellBlock *) malloc (sizeof (CellBlock *));

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
{
char * tmp;
printf ("%d  %d \n", numrows, sizeof (SingleCell **));
tmp = malloc (numrows * sizeof (SingleCell **));
tmp = malloc (2);
arr->cells = (SingleCell ***) tmp;
printf ("%p %p %p %d \n", arr, tmp, arr->cells, numrows);
}

   arr->cells = (SingleCell ***) malloc (numrows * sizeof (SingleCell **));
printf ("%p %p %d \n", arr, arr->cells, numrows);
   for (i=0; i<numrows; i++) {
printf ("%d %p %p \n", i, arr->cells, arr->cells[i]);
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
xaccAddCell (CellBlock *arr, SingleCell *cell) 
{
   int i,j;

   if (!arr) return;
   if (!cell) return;

   i = cell->row;
   j = cell->col;

   /* avoid embarrasement if cell incorrectly specified */
   if ((0 > i) || (0 > j)) return;
   if ((i >= arr->numRows) || (j >= arr->numCols)) return;

   arr->cells[i][j] = cell;
   arr->widths[j] = cell->width;
   arr->alignments[j] = cell->alignment;
}

/* --------------- end of file ----------------- */
