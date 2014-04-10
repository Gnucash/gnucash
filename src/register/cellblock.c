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
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

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
 * Copyright (c) 2000 Dave Peticolas
 */

#include <stdlib.h>
#include "cellblock.h"

/* =================================================== */

CellBlock *
xaccMallocCellBlock (int numrows, int numcols)
{

   CellBlock *cellblock;

   cellblock = g_new(CellBlock, 1);

   cellblock->numRows = 0;
   cellblock->numCols = 0;

   cellblock->active_bg_color   = 0xffffff; /* white */
   cellblock->passive_bg_color  = 0xffffff; /* white */
   cellblock->passive_bg_color2 = 0xffffff; /* white */

   cellblock->user_data = NULL;
   cellblock->cells = NULL;
   cellblock->cell_types = NULL;
   cellblock->right_traverse_r = NULL;
   cellblock->right_traverse_c = NULL;
   cellblock->left_traverse_r = NULL;
   cellblock->left_traverse_c = NULL;
   cellblock->widths = NULL;
   cellblock->alignments = NULL;

   xaccInitCellBlock (cellblock, numrows, numcols);

   return cellblock;
}

/* =================================================== */

static void        
FreeCellBlockMem (CellBlock *cellblock)
{
   int oldrows, oldcols;
   int i;

   oldrows = cellblock->numRows;
   oldcols = cellblock->numCols;

   /* free cell array, if any */
   if (cellblock->cells)
   {
      for (i = 0; i < oldrows; i++)
        g_free (cellblock->cells[i]);
      g_free (cellblock->cells);
      cellblock->cells = NULL;
   }

   /* free cell type array, if any */
   if (cellblock->cell_types)
   {
      for (i = 0; i < oldrows; i++)
        g_free (cellblock->cell_types[i]);
      g_free (cellblock->cell_types);
      cellblock->cell_types = NULL;
   }

   /* free right traversal chain */
   if (cellblock->right_traverse_r)
   {
      for (i = 0; i < oldrows; i++)
        g_free (cellblock->right_traverse_r[i]);
      g_free(cellblock->right_traverse_r);
      cellblock->right_traverse_r = NULL;
   }
   if (cellblock->right_traverse_c)
   {
      for (i = 0; i < oldrows; i++)
        g_free (cellblock->right_traverse_c[i]);
      g_free(cellblock->right_traverse_c);
      cellblock->right_traverse_c = NULL;
   }

   /* free left traversal chain */
   if (cellblock->left_traverse_r)
   {
      for (i = 0; i < oldrows; i++)
        g_free (cellblock->left_traverse_r[i]);
      g_free(cellblock->left_traverse_r);
      cellblock->left_traverse_r = NULL;
   }
   if (cellblock->left_traverse_c)
   {
      for (i = 0; i < oldrows; i++)
        g_free (cellblock->left_traverse_c[i]);
      g_free(cellblock->left_traverse_c);
      cellblock->left_traverse_c = NULL;
   }

   /* free widths, alignments */
   g_free (cellblock->widths);
   cellblock->widths = NULL;

   g_free (cellblock->alignments);
   cellblock->alignments = NULL;
}

/* =================================================== */

void        
xaccInitCellBlock (CellBlock *cellblock, int numrows, int numcols)
{
   int i, j;

   if (!cellblock) return;

   FreeCellBlockMem (cellblock);

   /* record new size */
   cellblock->numRows = numrows;
   cellblock->numCols = numcols;

   /* malloc new cell array */
   cellblock->cells = g_new(BasicCell **, numrows);
   cellblock->cell_types = g_new(short *, numrows);
   for (i = 0; i < numrows; i++) {
      (cellblock->cells)[i] = g_new(BasicCell *, numcols);
      (cellblock->cell_types)[i] = g_new(short, numcols);
      for (j=0; j<numcols; j++) {
         (cellblock->cells)[i][j] = NULL;
         (cellblock->cell_types)[i][j] = -1;         
      }
   }

   /* malloc new right traversal arrays */
   cellblock->right_traverse_r = g_new(short *, numrows);
   cellblock->right_traverse_c = g_new(short *, numrows);
   for (i = 0; i < numrows; i++) {
      (cellblock->right_traverse_r)[i] = g_new(short, numcols);
      (cellblock->right_traverse_c)[i] = g_new(short, numcols);
      for (j = 0; j < numcols - 1; j++) {
         /* default traversal is same row, next column */
         (cellblock->right_traverse_r)[i][j] = i;
         (cellblock->right_traverse_c)[i][j] = j+1;
      }
      /* at end of row, wrap to next row */
      (cellblock->right_traverse_r)[i][numcols-1] = i+1;
      (cellblock->right_traverse_c)[i][numcols-1] = 0;
   }
   /* at end of block, wrap back to begining */
   (cellblock->right_traverse_r)[numrows-1][numcols-1] = 0;
   (cellblock->right_traverse_c)[numrows-1][numcols-1] = 0;

   /* last is last ... */
   cellblock->last_reenter_traverse_row = numrows - 1;
   cellblock->last_reenter_traverse_col = numcols - 1;

   /* malloc new left traversal arrays */
   cellblock->left_traverse_r = g_new(short *, numrows);
   cellblock->left_traverse_c = g_new(short *, numrows);
   for (i = 0; i < numrows; i++) {
      (cellblock->left_traverse_r)[i] = g_new(short, numcols);
      (cellblock->left_traverse_c)[i] = g_new(short, numcols);
      for (j = 0; j < numcols-1; j++) {
         /* default traversal is same row, previous column */
         (cellblock->left_traverse_r)[i][j] = i;
         (cellblock->left_traverse_c)[i][j] = j-1;
      }
      /* at start of row, wrap to previous row */
      (cellblock->left_traverse_r)[i][numcols-1] = i-1;
      (cellblock->left_traverse_c)[i][numcols-1] = numcols-1;
   }
   /* at start of block, wrap back to end */
   (cellblock->right_traverse_r)[0][0] = numrows-1;
   (cellblock->right_traverse_c)[0][0] = numcols-1;

   /* first is last ... */
   cellblock->last_left_reenter_traverse_row = 0;
   cellblock->last_left_reenter_traverse_col = 0;

   cellblock->widths = g_new(short, numcols);
   cellblock->alignments = g_new(Alignments, numcols);

   for (j = 0; j < numcols; j++) {
      cellblock->widths[j] = 0;
      cellblock->alignments[j] = ALIGN_RIGHT;
   }
}

/* =================================================== */

void        
xaccDestroyCellBlock (CellBlock *cellblock)
{
   if (!cellblock) return;

   FreeCellBlockMem (cellblock);

   /* finally, free this object itself */
   g_free (cellblock);
}

/* =================================================== */

void        
xaccNextRight (CellBlock *cellblock,
               int row,      int col, 
               int next_row, int next_col)
{
   if (!cellblock) return;

   /* avoid embarrasement if cell incorrectly specified */
   if ((0 > row) || (0 > col)) return;
   if ((row >= cellblock->numRows) || (col >= cellblock->numCols)) return;

   /* -1 is a valid value for next ... it signifies that traversal
    * should go to next tab group, so do not check for neg values.
    */

   /* if the "next" location to hop to is larger than the cursor, that
    * just means that we should hop to the next cursor.  Thus, large
    * values for next *are* valid.
    */

   (cellblock->right_traverse_r)[row][col] = next_row;
   (cellblock->right_traverse_c)[row][col] = next_col;

   /* if traversing out (neg values) record this as the last ... */
   if ((0 > next_row) || (0 > next_col)) {
      cellblock->last_reenter_traverse_row = row;
      cellblock->last_reenter_traverse_col = col;
   }

}


void        
xaccNextLeft (CellBlock *cellblock, int row,      int col, 
                              int next_row, int next_col)
{
   if (!cellblock) return;

   /* avoid embarrasement if cell incorrectly specified */
   if ((0 > row) || (0 > col)) return;
   if ((row >= cellblock->numRows) || (col >= cellblock->numCols)) return;

   /* -1 is a valid value for next ... it signifies that traversal
    * should go to next tab group, so do not check for neg values.
    */

   /* if the "next" location to hop to is larger than the cursor, that
    * just means that we should hop to the next cursor.  Thus, large
    * values for next *are* valid.
    */

   (cellblock->left_traverse_r)[row][col] = next_row;
   (cellblock->left_traverse_c)[row][col] = next_col;

   /* if traversing out (neg values) record this as the last ... */
   if ((0 > next_row) || (0 > next_col)) {
      cellblock->last_left_reenter_traverse_row = row;
      cellblock->last_left_reenter_traverse_col = col;
   }

}

/* --------------- end of file ----------------- */
