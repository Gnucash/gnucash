/********************************************************************\
 * table-layout.c -- 2D table layout                                *
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#include "config.h"

#include <glib.h>

#include "basiccell.h"
#include "table-layout.h"


struct table_layout_struct
{
  GList *cells;  /* The cells in the table */
};

typedef struct cell_node
{
  int cell_type;
  BasicCell *cell;
} CellNode;

typedef struct _CellBuffer CellBuffer;
struct _CellBuffer
{
  int cell_type;
  char * value;
  guint32 changed;
  guint32 conditionally_changed;
};

struct cursor_buffer_struct
{
  GList *cell_buffers;
};


TableLayout *
gnc_table_layout_new (void)
{
  TableLayout *layout;

  layout = g_new0 (TableLayout, 1);

  return layout;
}

void
gnc_table_layout_destroy (TableLayout *layout)
{
  GList *node;

  if (!layout)
    return;

  for (node = layout->cells; node; node = node->next)
  {
    CellNode *cn = node->data;

    gnc_basic_cell_destroy (cn->cell);
    g_free (cn);
  }

  g_list_free (layout->cells);
  layout->cells = NULL;
}

void
gnc_table_layout_add_cell (TableLayout *layout,
                           int cell_type,
                           BasicCell *cell)
{
  CellNode *node;

  g_return_if_fail (layout != NULL);
  g_return_if_fail (cell != NULL);

  node = g_new0 (CellNode, 1);

  node->cell_type = cell_type;
  node->cell = cell;

  layout->cells = g_list_prepend (layout->cells, node);
}

BasicCell *
gnc_table_layout_get_cell (TableLayout *layout, int cell_type)
{
  GList *node;

  g_return_val_if_fail (layout != NULL, NULL);

  for (node = layout->cells; node; node = node->next)
  {
    CellNode *cn = node->data;

    if (cn->cell_type == cell_type)
      return cn->cell;
  }

  return NULL;
}

int
gnc_table_layout_get_cell_type (TableLayout *layout, BasicCell *cell)
{
  GList *node;

  if (!layout || !cell)
    return -1;

  for (node = layout->cells; node; node = node->next)
  {
    CellNode *cn = node->data;

    if (cn->cell == cell)
      return cn->cell_type;
  }

  return -1;
}

CursorBuffer *
gnc_cursor_buffer_new (void)
{
  CursorBuffer *buffer;

  buffer = g_new0 (CursorBuffer, 1);

  return buffer;
}

static void
destroy_cell_buffer (CellBuffer *cb)
{
  if (cb == NULL)
    return;

  g_free (cb->value);
  cb->value = NULL;

  g_free (cb);
}

static void
gnc_cursor_buffer_clear (CursorBuffer *buffer)
{
  GList *node;

  if (!buffer) return;

  for (node = buffer->cell_buffers; node; node = node->next)
  {
    CellBuffer *cb = node->data;

    destroy_cell_buffer (cb);
  }

  g_list_free (buffer->cell_buffers);
  buffer->cell_buffers = NULL;
}

void
gnc_cursor_buffer_destroy (CursorBuffer *buffer)
{
  if (!buffer) return;

  gnc_cursor_buffer_clear (buffer);

  g_free (buffer);
}

static CellBuffer *
save_cell (BasicCell *bcell)
{
  CellBuffer *cb;

  if (!bcell)
    return NULL;

  cb = g_new0 (CellBuffer, 1);

  cb->value = g_strdup (bcell->value);
  cb->changed = bcell->changed;
  cb->conditionally_changed = bcell->conditionally_changed;

  return cb;
}

void
gnc_table_layout_save_cursor (TableLayout *layout,
                              CellBlock *cursor,
                              CursorBuffer *buffer)
{
  GList *node;

  if (!layout || !cursor || !buffer)
    return;

  gnc_cursor_buffer_clear (buffer);

  for (node = layout->cells; node; node = node->next)
  {
    CellNode *cn = node->data;
    CellBuffer *cb;

    if (!gnc_basic_cell_get_changed (cn->cell) &&
        !gnc_basic_cell_get_conditionally_changed (cn->cell))
      continue;

    cb = save_cell (cn->cell);
    cb->cell_type = cn->cell_type;

    buffer->cell_buffers = g_list_prepend (buffer->cell_buffers, cb);
  }
}

static void
restore_cell (BasicCell *bcell, CellBuffer *cb, CellBlock *cursor)
{
  int r, c;

  if (!bcell || !cb || !cursor)
    return;

  if (!cb->changed && !cb->conditionally_changed)
    return;

  /* only restore if it's in the current cursor */
  for (r = 0; r < cursor->num_rows; r++)
    for (c = 0; c < cursor->num_cols; c++)
    {
      CellBlockCell *cb_cell;

      cb_cell = gnc_cellblock_get_cell (cursor, r, c);
      if (cb_cell == NULL)
        continue;

      if (cb_cell->cell == bcell)
      {
        xaccSetBasicCellValue (bcell, cb->value);
        bcell->changed = cb->changed;
        bcell->conditionally_changed = cb->conditionally_changed;
        return;
      }
    }
}

void
gnc_table_layout_restore_cursor (TableLayout *layout,
                                 CellBlock *cursor,
                                 CursorBuffer *buffer)
{
  GList *node;

  if (!layout || !cursor || !buffer)
    return;

  for (node = buffer->cell_buffers; node; node = node->next)
  {
    CellBuffer *cb = node->data;
    BasicCell *cell;

    cell = gnc_table_layout_get_cell (layout, cb->cell_type);

    restore_cell (cell, cb, cursor);
  }
}
