/********************************************************************\
 * cellblock.h -- group of cells that act as cursor within a table  *
 *                                                                  *
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
 * cellblock.h
 *
 * FUNCTION:
 * The CellBlock struct is a rectangular grid of cells that 
 * define an arrangement of cells.  It is typically used to
 * define a virtual cursor within a larger table of cells.
 *
 * HISTORY:
 * Copyright (c) 1988 Linas Vepstas
 * Copyright (c) 2000-2001 Dave Peticolas <dave@krondo.com>
 */

#ifndef XACC_CELL_BLOCK_H
#define XACC_CELL_BLOCK_H

#include "basiccell.h"

#include "gtable.h"

typedef struct
{
  short num_rows;
  short num_cols;

  short start_col;
  short stop_col;

  char *cursor_name;

  GPtrArray *cells; /* Holds the CellBlockCell table */
} CellBlock;


CellBlock * gnc_cellblock_new (int rows, int cols, const char *cursor_name);

void        gnc_cellblock_destroy (CellBlock *cellblock);

void        gnc_cellblock_set_cell (CellBlock *cellblock,
                                    int row, int col,
                                    BasicCell *cell);

BasicCell * gnc_cellblock_get_cell (CellBlock *cellblock,
                                    int row, int col);

gboolean    gnc_cellblock_changed (CellBlock *cursor,
                                   gboolean include_conditional);

void        gnc_cellblock_clear_changes (CellBlock *cursor);

#endif
