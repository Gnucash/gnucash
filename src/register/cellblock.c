
#include <stdlib.h>
#include "cellblock.h"

CellBlock * xaccMallocCellBlock (int numrows, int numcols)
{

   CellBlock *arr;
   arr = (CellBlock *) malloc (sizeof (CellBlock));

   arr->numRows = 0;
   arr->numCols = 0;

   arr->cells = NULL;
   arr->right_traverse_r = NULL;
   arr->right_traverse_c = NULL;
   arr->widths = NULL;
   arr->alignments = NULL;
   xaccInitCellBlock (arr, numrows, numcols);

   return arr;
}

/* =================================================== */

void        
xaccInitCellBlock (CellBlock *arr, int numrows, int numcols)
{
   int i, j;
   int oldrows, oldcols;

   if (!arr) return;

   oldrows = arr->numRows;
   oldcols = arr->numCols;

   /* free old cell array, if any */
   if (arr->cells) {
      for (i=0; i<oldrows; i++) {
         if (arr->cells[i]) free (arr->cells[i]);
      }
      free (arr->cells);
   }

   /* free old traversal chain */
   if (arr->right_traverse_r) {
      for (i=0; i<oldrows; i++) {
         if (arr->right_traverse_r[i]) free (arr->right_traverse_r[i]);
      }
   }
   if (arr->right_traverse_c) {
      for (i=0; i<oldrows; i++) {
         if (arr->right_traverse_c[i]) free (arr->right_traverse_c[i]);
      }
   }

   /* free old widths, alignments */
   if (arr->widths) free (arr->widths);
   if (arr->alignments) free (arr->alignments);

   /* -------------------------------------------------- */
   /* record new size */
   arr->numRows = numrows;
   arr->numCols = numcols;

   /* malloc new cell array */
   arr->cells = (BasicCell ***) malloc (numrows * sizeof (BasicCell **));
   for (i=0; i<numrows; i++) {
      (arr->cells)[i] = (BasicCell **) malloc (numcols * sizeof (BasicCell *));
      for (j=0; j<numcols; j++) {
         (arr->cells)[i][j] = NULL;
      }
   }

   /* malloc new traversal arrays */
   arr->right_traverse_r = (short **) malloc (numrows * sizeof (short *));
   arr->right_traverse_c = (short **) malloc (numrows * sizeof (short *));
   for (i=0; i<numrows; i++) {
      (arr->right_traverse_r)[i] = (short *) malloc (numcols * sizeof (short));
      (arr->right_traverse_c)[i] = (short *) malloc (numcols * sizeof (short));
      for (j=0; j<numcols-1; j++) {
         /* default traversal is same row, next column */
         (arr->right_traverse_r)[i][j] = i;
         (arr->right_traverse_c)[i][j] = j+1;
      }
      /* at end of row, wrap to next row */
      (arr->right_traverse_r)[i][numcols-1] = i+1;
      (arr->right_traverse_c)[i][numcols-1] = 0;
   }
   /* at end of block, wrap back to begining */
   (arr->right_traverse_r)[numrows-1][numcols-1] = 0;
   (arr->right_traverse_c)[numrows-1][numcols-1] = 0;

   arr->widths = (short *) malloc (numcols * sizeof(short));
   arr->alignments = (unsigned char *) malloc (numcols * sizeof(unsigned char));
   
   for (j=0; j<numcols; j++) {
      arr->widths[j] = 0;
      arr->alignments[j] = 0;
   }
}

/* =================================================== */

void        
xaccAddCell (CellBlock *arr, BasicCell *cell, int row, int col) 
{
   if (!arr) return;
   if (!cell) return;

   /* avoid embarrasement if cell incorrectly specified */
   if ((0 > row) || (0 > col)) return;
   if ((row >= arr->numRows) || (col >= arr->numCols)) return;

   arr->cells[row][col] = cell;
   arr->widths[col] = cell->width;
   arr->alignments[col] = cell->alignment;

   /* install back-pointer to this container */
   cell->block = (struct _CellBlock *) arr;
}

/* =================================================== */

void        
xaccNextRight (CellBlock *arr, int row,      int col, 
                               int next_row, int next_col)
{
   if (!arr) return;

   /* avoid embarrasement if cell incorrectly specified */
   if ((0 > row) || (0 > col)) return;
   if ((row >= arr->numRows) || (col >= arr->numCols)) return;

   /* -1 is a valid value for next ... it signifies 
    * that traversal should go to next tab group */
   /* if ((0 > next_row) || (0 > next_col)) return; */
   if ((next_row >= arr->numRows) || (next_col >= arr->numCols)) return;

   (arr->right_traverse_r)[row][col] = next_row;
   (arr->right_traverse_c)[row][col] = next_col;


}

/* --------------- end of file ----------------- */
