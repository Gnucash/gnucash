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
gnc_cellblock_cell_new (gpointer user_data)
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
gnc_cellblock_cell_free (gpointer _cb_cell, gpointer user_data)
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

static void        
gnc_cellblock_init (CellBlock *cellblock, int rows, int cols)
{
  /* init colors */
  cellblock->active_bg_color   = 0xffffff; /* white */
  cellblock->passive_bg_color  = 0xffffff; /* white */
  cellblock->passive_bg_color2 = 0xffffff; /* white */

  /* record new size */
  cellblock->num_rows = rows;
  cellblock->num_cols = cols;

  /* malloc new cell table */
  cellblock->cb_cells = g_table_new (gnc_cellblock_cell_new,
                                     gnc_cellblock_cell_free, NULL);
  g_table_resize (cellblock->cb_cells, rows, cols);
}

/* =================================================== */

void        
gnc_cellblock_destroy (CellBlock *cellblock)
{
   if (!cellblock) return;

   g_table_destroy (cellblock->cb_cells);
   cellblock->cb_cells = NULL;

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

/* --------------- end of file ----------------- */
