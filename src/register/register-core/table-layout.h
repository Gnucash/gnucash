/********************************************************************\
 * table-layout.h -- 2D table layout                                *
 * Copyright (c) 2001 Free Software Foundation                      *
 * Author: Dave Peticolas <dave@krondo.com>                         *
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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#ifndef TABLE_LAYOUT_H
#define TABLE_LAYOUT_H

#include "basiccell.h"
#include "cellblock.h"


/** Standard Cursor Names ********************************************/
#define CURSOR_HEADER "cursor-header"


/** Type definitions *************************************************/
typedef struct table_layout_struct TableLayout;
typedef struct cursor_buffer_struct CursorBuffer;


/** API Declarations *************************************************/
TableLayout * gnc_table_layout_new (void);
void gnc_table_layout_destroy (TableLayout *layout);

void gnc_table_layout_add_cell (TableLayout *layout, BasicCell *cell);

BasicCell * gnc_table_layout_get_cell (TableLayout *layout,
                                       const char *cell_name);
const char * gnc_table_layout_get_cell_value (TableLayout *layout,
        const char *cell_name);
gboolean gnc_table_layout_get_cell_changed (TableLayout *layout,
        const char *cell_name,
        gboolean include_conditional);

GList * gnc_table_layout_get_cells (TableLayout *layout);

void gnc_table_layout_add_cursor (TableLayout *layout,
                                  CellBlock *cursor);

CellBlock * gnc_table_layout_get_cursor (TableLayout *layout,
        const char *cursor_name);

GList * gnc_table_layout_get_cursors (TableLayout *layout);

void gnc_table_layout_set_primary_cursor (TableLayout *layout,
        CellBlock *cursor);

void gnc_table_layout_set_cell (TableLayout *layout,
                                CellBlock *cursor,
                                const char *cell_name,
                                int row, int col);

CursorBuffer * gnc_cursor_buffer_new (void);
void gnc_cursor_buffer_destroy (CursorBuffer *buffer);

void gnc_table_layout_save_cursor (TableLayout *layout,
                                   CellBlock *cursor,
                                   CursorBuffer *buffer);
void gnc_table_layout_restore_cursor (TableLayout *layout,
                                      CellBlock *cursor,
                                      CursorBuffer *buffer);

#endif
