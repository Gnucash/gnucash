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

#include "cellblock.h"

static void gnc_cellblock_init (CellBlock *cellblock, int rows, int cols);


/* =================================================== */

CellBlock *
gnc_cellblock_new (int rows, int cols)
{
  CellBlock *cellblock;

  cellblock = g_new0(CellBlock, 1);

  gnc_cellblock_init (cellblock, rows, cols);

  return cellblock;
}

/* =================================================== */

static gpointer
gnc_cellblock_cell_new (void)
{
  CellBlockCell *cb_cell;

  cb_cell = g_new0(CellBlockCell, 1);

  cb_cell->cell_type = -1;
  cb_cell->alignment = CELL_ALIGN_LEFT;
  cb_cell->expandable = FALSE;
  cb_cell->span = FALSE;

  return cb_cell;
}

/* =================================================== */

static void
gnc_cellblock_cell_free (gpointer _cb_cell)
{
  CellBlockCell *cb_cell = _cb_cell;

  if (cb_cell == NULL)
    return;

  g_free(cb_cell->sample_text);
  cb_cell->sample_text = NULL;

  g_free(cb_cell);

  return;
}

/* =================================================== */

static gpointer
gnc_cell_traverse_info_new (void)
{
  CellTraverseInfo *ct_info;

  ct_info = g_new0(CellTraverseInfo, 1);

  return ct_info;
}

/* =================================================== */

static void
gnc_cell_traverse_info_free (gpointer ct_info)
{
  g_free (ct_info);
}

/* =================================================== */

static void        
gnc_cellblock_init (CellBlock *cellblock, int rows, int cols)
{
  CellTraverseInfo *ct_info;
  int row, col;

  if (!cellblock) return;

  /* init colors */
  cellblock->active_bg_color   = 0xffffff; /* white */
  cellblock->passive_bg_color  = 0xffffff; /* white */
  cellblock->passive_bg_color2 = 0xffffff; /* white */

  /* record new size */
  cellblock->num_rows = rows;
  cellblock->num_cols = cols;

  /* malloc new cell table */
  cellblock->cb_cells = g_table_new (gnc_cellblock_cell_new,
                                     gnc_cellblock_cell_free);
  g_table_resize (cellblock->cb_cells, rows, cols);

  /* malloc new traversal table */
  cellblock->traverse_info = g_table_new (gnc_cell_traverse_info_new,
                                          gnc_cell_traverse_info_free);
  g_table_resize (cellblock->traverse_info, rows, cols);

  for (row = 0; row < rows; row++)
  {
    for (col = 0; col < cols; col++)
    {
      ct_info = g_table_index (cellblock->traverse_info, row, col);

      /* default right traversal is same row, next column */
      ct_info->right_traverse_row = row;
      ct_info->right_traverse_col = col + 1;

      /* default left traversal is same row, previous column */
      ct_info->left_traverse_row = row;
      ct_info->left_traverse_col = col - 1;
    }

    /* at end of row, wrap to next row */
    ct_info = g_table_index (cellblock->traverse_info, row, cols - 1);
    ct_info->right_traverse_row = row + 1;
    ct_info->right_traverse_col = 0;

    /* at start of row, wrap to previous row */
    ct_info = g_table_index (cellblock->traverse_info, row, 0);
    ct_info->left_traverse_row = row - 1;
    ct_info->left_traverse_col = cols - 1;
  }

  /* at end of block, wrap back to begining */
  ct_info = g_table_index (cellblock->traverse_info, rows - 1, cols - 1);
  ct_info->right_traverse_row = 0;
  ct_info->right_traverse_col = 0;

  /* at start of block, wrap back to end */
  ct_info = g_table_index (cellblock->traverse_info, 0, 0);
  ct_info->left_traverse_row = rows - 1;
  ct_info->left_traverse_col = cols - 1;
}

/* =================================================== */

void        
gnc_cellblock_destroy (CellBlock *cellblock)
{
   if (!cellblock) return;

   g_table_destroy (cellblock->cb_cells);
   cellblock->cb_cells = NULL;

   g_table_destroy (cellblock->traverse_info);
   cellblock->traverse_info = NULL;

   g_free (cellblock);
}

/* =================================================== */

CellBlockCell *
gnc_cellblock_get_cell (CellBlock *cellblock, int row, int col)
{
  if (cellblock == NULL)
    return NULL;

  return g_table_index (cellblock->cb_cells, row, col);
}

/* =================================================== */

CellTraverseInfo *
gnc_cellblock_get_traverse (CellBlock *cellblock, int row, int col)
{
  if (cellblock == NULL)
    return NULL;

  return g_table_index (cellblock->traverse_info, row, col);
}

/* =================================================== */

void        
gnc_cellblock_next_right (CellBlock *cellblock,
                          int row,      int col, 
                          int next_row, int next_col)
{
  CellTraverseInfo *ct_info;

  if (!cellblock) return;

  /* avoid embarrasement if cell incorrectly specified */
  if ((0 > row) || (0 > col)) return;
  if ((row >= cellblock->num_rows) || (col >= cellblock->num_cols)) return;

  ct_info = gnc_cellblock_get_traverse (cellblock, row, col);

  /* -1 is a valid value for next_*, signifying that traversal should
   * go to next tab group, so do not check for neg values.  */

  /* if the "next" location to hop to is larger than the cursor, that
   * just means that we should hop to the next cursor.  Thus, large
   * values for next *are* valid.  */

  ct_info->right_traverse_row = next_row;
  ct_info->right_traverse_col = next_col;
}

void        
gnc_cellblock_next_left (CellBlock *cellblock,
                         int row,      int col, 
                         int next_row, int next_col)
{
  CellTraverseInfo *ct_info;

  if (!cellblock) return;

  /* avoid embarrasement if cell incorrectly specified */
  if ((0 > row) || (0 > col)) return;
  if ((row >= cellblock->num_rows) || (col >= cellblock->num_cols)) return;

  ct_info = gnc_cellblock_get_traverse (cellblock, row, col);

  /* -1 is a valid value for next ... it signifies that traversal
   * should go to next tab group, so do not check for neg values.  */

  /* if the "next" location to hop to is larger than the cursor, that
   * just means that we should hop to the next cursor.  Thus, large
   * values for next *are* valid.  */

  ct_info->left_traverse_row = next_row;
  ct_info->left_traverse_col = next_col;
}

/* --------------- end of file ----------------- */
