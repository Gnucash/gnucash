/*
 * FILE:
 * cellblock.c
 * 
 * FUNCTION:
 * implements a rectangular array of cells. See the header file for
 * additional documentation.
 *
 * HISTORY:
 * Copyright (c) 1998 Linas Vepstas
 */
/********************************************************************\
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
\********************************************************************/

#include <stdlib.h>
#include "cellblock.h"

/* =================================================== */

CellBlock * xaccMallocCellBlock (int numrows, int numcols)
{

   CellBlock *arr;
   arr = (CellBlock *) malloc (sizeof (CellBlock));

   arr->numRows = 0;
   arr->numCols = 0;

   arr->active_bg_color = 0xffffff; /* white */
   arr->passive_bg_color = 0xffffff; /* white */

   arr->user_data = NULL;
   arr->cells = NULL;
   arr->cell_types = NULL;
   arr->right_traverse_r = NULL;
   arr->right_traverse_c = NULL;
   arr->left_traverse_r = NULL;
   arr->left_traverse_c = NULL;
   arr->widths = NULL;
   arr->alignments = NULL;
   xaccInitCellBlock (arr, numrows, numcols);

   return arr;
}

/* =================================================== */

static void        
FreeCellBlockMem (CellBlock *arr)
{
   int i;
   int oldrows, oldcols;

   oldrows = arr->numRows;
   oldcols = arr->numCols;

   /* free cell array, if any */
   if (arr->cells) {
      for (i=0; i<oldrows; i++) {
         if (arr->cells[i]) free (arr->cells[i]);
      }
      free (arr->cells);
   }

   /* free label array, if any */
   if (arr->cell_types) {
      for (i=0; i<oldrows; i++) {
         if (arr->cell_types[i]) free (arr->cell_types[i]);
      }
   }
   
   /* free right traversal chain */
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

   /* free left traversal chain */
   if (arr->left_traverse_r) {
      for (i=0; i<oldrows; i++) {
         if (arr->left_traverse_r[i]) free (arr->left_traverse_r[i]);
      }
   }
   if (arr->left_traverse_c) {
      for (i=0; i<oldrows; i++) {
         if (arr->left_traverse_c[i]) free (arr->left_traverse_c[i]);
      }
   }

   /* free widths, alignments */
   if (arr->widths) free (arr->widths);
   if (arr->alignments) free (arr->alignments);
}

/* =================================================== */

void        
xaccInitCellBlock (CellBlock *arr, int numrows, int numcols)
{
   int i, j;
   if (!arr) return;

   FreeCellBlockMem (arr);

   /* record new size */
   arr->numRows = numrows;
   arr->numCols = numcols;

   /* malloc new cell array */
   arr->cells = (BasicCell ***) malloc (numrows * sizeof (BasicCell **));
   arr->cell_types = (short **) malloc (numrows * sizeof (short *));
   for (i=0; i<numrows; i++) {
      (arr->cells)[i] = (BasicCell **) malloc (numcols * sizeof (BasicCell *));
      (arr->cell_types)[i] = (short *) malloc (numcols * sizeof (short));
      for (j=0; j<numcols; j++) {
         (arr->cells)[i][j] = NULL;
         (arr->cell_types)[i][j] = -1;         
      }
   }

   /* malloc new right traversal arrays */
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

   /* last is last ... */
   arr->last_reenter_traverse_row = numrows-1;
   arr->last_reenter_traverse_col = numcols-1;

   /* malloc new left traversal arrays */
   arr->left_traverse_r = (short **) malloc (numrows * sizeof (short *));
   arr->left_traverse_c = (short **) malloc (numrows * sizeof (short *));
   for (i=0; i<numrows; i++) {
      (arr->left_traverse_r)[i] = (short *) malloc (numcols * sizeof (short));
      (arr->left_traverse_c)[i] = (short *) malloc (numcols * sizeof (short));
      for (j=0; j<numcols-1; j++) {
         /* default traversal is same row, previous column */
         (arr->left_traverse_r)[i][j] = i;
         (arr->left_traverse_c)[i][j] = j-1;
      }
      /* at start of row, wrap to previous row */
      (arr->left_traverse_r)[i][numcols-1] = i-1;
      (arr->left_traverse_c)[i][numcols-1] = numcols-1;
   }
   /* at start of block, wrap back to end */
   (arr->right_traverse_r)[0][0] = numrows-1;
   (arr->right_traverse_c)[0][0] = numcols-1;

   /* first is last ... */
   arr->last_left_reenter_traverse_row = 0;
   arr->last_left_reenter_traverse_col = 0;
   
   arr->widths = (short *) malloc (numcols * sizeof(short));
   arr->alignments = (Alignments *) malloc (numcols * sizeof(Alignments));
   
   for (j=0; j<numcols; j++) {
      arr->widths[j] = 0;
      arr->alignments[j] = ALIGN_RIGHT;
   }
}

/* =================================================== */

void        
xaccDestroyCellBlock (CellBlock *arr)
{
   if (!arr) return;

   FreeCellBlockMem (arr);

   /* finally, free this object itself */
   free (arr);
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

   /* -1 is a valid value for next ... it signifies that traversal
    * should go to next tab group, so do not check for neg values.
    * if ((0 > next_row) || (0 > next_col)) return; 
    */

   /* if the "next" location to hop to is larger than the cursor, that
    * just means that we should hop to the next cursor.  Thus, large
    * values for next *are* valid.
    * if ((next_row >= arr->numRows) || (next_col >= arr->numCols)) return; 
    */

   (arr->right_traverse_r)[row][col] = next_row;
   (arr->right_traverse_c)[row][col] = next_col;

   /* if traversing out (neg values) record this as the last ... */
   if ((0 > next_row) || (0 > next_col)) {
      arr->last_reenter_traverse_row = row;
      arr->last_reenter_traverse_col = col;
   }

}


void        
xaccNextLeft (CellBlock *arr, int row,      int col, 
                              int next_row, int next_col)
{
   if (!arr) return;

   /* avoid embarrasement if cell incorrectly specified */
   if ((0 > row) || (0 > col)) return;
   if ((row >= arr->numRows) || (col >= arr->numCols)) return;

   /* -1 is a valid value for next ... it signifies that traversal
    * should go to next tab group, so do not check for neg values.
    * if ((0 > next_row) || (0 > next_col)) return; 
    */

   /* if the "next" location to hop to is larger than the cursor, that
    * just means that we should hop to the next cursor.  Thus, large
    * values for next *are* valid.
    * if ((next_row >= arr->numRows) || (next_col >= arr->numCols)) return; 
    */

   (arr->left_traverse_r)[row][col] = next_row;
   (arr->left_traverse_c)[row][col] = next_col;

   /* if traversing out (neg values) record this as the last ... */
   if ((0 > next_row) || (0 > next_col)) {
      arr->last_left_reenter_traverse_row = row;
      arr->last_left_reenter_traverse_col = col;
   }

}

/* --------------- end of file ----------------- */
