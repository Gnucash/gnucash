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
#include <string.h>

#include "basiccell.h"
#include "cellblock.h"
#include "table-layout.h"


struct table_layout_struct
{
  GList *cells;   /* The cells in the table */
  GList *cursors; /* The cursors in the table */
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

  for (node = layout->cursors; node; node = node->next)
  {
    CellBlock *cursor = node->data;

    gnc_cellblock_destroy (cursor);
  }
  g_list_free (layout->cursors);
  layout->cursors = NULL;

  g_free (layout);
}

void
gnc_table_layout_add_cell (TableLayout *layout,
                           int cell_type,
                           BasicCell *cell)
{
  CellNode *cell_node = NULL;
  GList *node;

  g_return_if_fail (layout != NULL);
  g_return_if_fail (cell != NULL);

  for (node = layout->cells; node; node = node->next)
  {
    cell_node = node->data;

    if (cell_node->cell_type == cell_type)
    {
      if (cell_node->cell == cell)
        return;

      gnc_basic_cell_destroy (cell_node->cell);
      cell_node->cell = NULL;
      break;
    }

    cell_node = NULL;
  }

  if (!cell_node)
  {
    cell_node = g_new0 (CellNode, 1);
    layout->cells = g_list_append (layout->cells, cell_node);
  }

  cell_node->cell_type = cell_type;
  cell_node->cell = cell;
}

BasicCell *
gnc_table_layout_get_cell (TableLayout *layout, int cell_type)
{
  GList *node;

  g_return_val_if_fail (layout != NULL, NULL);

  for (node = layout->cells; node; node = node->next)
  {
    CellNode *cell_node = node->data;

    if (cell_node->cell_type == cell_type)
      return cell_node->cell;
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
    CellNode *cell_node = node->data;

    if (cell_node->cell == cell)
      return cell_node->cell_type;
  }

  return -1;
}

GList *
gnc_table_layout_get_cells (TableLayout *layout)
{
  GList *cells = NULL;
  GList *node;

  if (!layout)
    return NULL;

  for (node = layout->cells; node; node = node->next)
  {
    CellNode *cell_node = node->data;

    cells = g_list_prepend (cells, cell_node->cell);
  }

  return g_list_reverse (cells);
}

void
gnc_table_layout_add_cursor (TableLayout *layout,
                             CellBlock *cursor)
{
  GList *node;

  g_return_if_fail (layout != NULL);
  g_return_if_fail (cursor != NULL);

  if (g_list_find (layout->cursors, cursor))
    return;

  for (node = layout->cursors; node; node = node->next)
  {
    CellBlock *list_cursor = node->data;

    if (strcmp (list_cursor->cursor_name, cursor->cursor_name) == 0)
    {
      layout->cursors = g_list_remove (layout->cursors, list_cursor);
      gnc_cellblock_destroy (list_cursor);
      break;
    }
  }

  layout->cursors = g_list_append (layout->cursors, cursor);
}

CellBlock *
gnc_table_layout_get_cursor (TableLayout *layout,
                             const char *cursor_name)
{
  GList *node;

  g_return_val_if_fail (layout != NULL, NULL);

  if (!cursor_name)
    return NULL;

  for (node = layout->cursors; node; node = node->next)
  {
    CellBlock *cursor = node->data;

    if (strcmp (cursor_name, cursor->cursor_name) == 0)
      return cursor;
  }

  return NULL;
}

GList *
gnc_table_layout_get_cursors (TableLayout *layout)
{
  g_return_val_if_fail (layout != NULL, NULL);
  return layout->cursors;
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
    CellNode *cell_node = node->data;
    CellBuffer *cb;

    if (!gnc_basic_cell_get_changed (cell_node->cell) &&
        !gnc_basic_cell_get_conditionally_changed (cell_node->cell))
      continue;

    cb = save_cell (cell_node->cell);
    cb->cell_type = cell_node->cell_type;

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
