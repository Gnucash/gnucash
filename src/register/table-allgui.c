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
 * table-allgui.c
 *
 * FUNCTION:
 * Implements the gui-independent parts of the table infrastructure.
 *
 * HISTORY:
 * Copyright (c) 1998,1999,2000 Linas Vepstas
 * Copyright (c) 2000 Dave Peticolas
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <glib.h>

#include "table-allgui.h"
#include "cellblock.h"
#include "util.h"


/** Datatypes **********************************************************/

/* The generic TableCell is used for memory allocation purposes. */
typedef union _TableCell TableCell;
union _TableCell
{
  VirtualCell virt_cell;
  PhysicalCell phys_cell;
};


/** Static Globals *****************************************************/

/* This static indicates the debugging module that this .o belongs to. */
static short module = MOD_REGISTER;

static GMemChunk *cell_mem_chunk = NULL;


/** Prototypes *********************************************************/
static void gnc_table_init (Table * table);
static void gnc_table_free_data (Table * table);
static void gnc_virtual_cell_free (TableCell *tcell);
static void gnc_physical_cell_free (TableCell *tcell);
static void gnc_table_resize (Table * table,
                              int new_phys_rows, int new_phys_cols,
                              int new_virt_rows, int new_virt_cols);


/** Implementation *****************************************************/

Table * 
gnc_table_new (void)
{
   Table *table;

   /* Do this here for lower overhead. */
   if (cell_mem_chunk == NULL)
     cell_mem_chunk = g_mem_chunk_create(TableCell, 2048, G_ALLOC_AND_FREE);

   table = g_new0(Table, 1);

   gnc_table_init (table);

   table->virt_cells = g_ptr_array_new();
   table->phys_cells = g_ptr_array_new();

   return table;
}

/* ==================================================== */

static void
gnc_table_init (Table * table)
{
   table->num_phys_rows = -1;
   table->num_phys_cols = -1;
   table->num_virt_rows = -1;
   table->num_virt_cols = -1;

   table->current_cursor = NULL;
   table->current_cursor_virt_row = -1;
   table->current_cursor_virt_col = -1;
   table->current_cursor_phys_row = -1;
   table->current_cursor_phys_col = -1;

   table->move_cursor = NULL;
   table->traverse = NULL;
   table->set_help = NULL;
   table->user_data = NULL;

   table->alternate_bg_colors = FALSE;


   /* initialize private data */

   table->virt_cells = NULL;
   table->phys_cells = NULL;

   /* invalidate the "previous" traversed cell */
   table->prev_phys_traverse_row = -1;
   table->prev_phys_traverse_col = -1;

   table->ui_data = NULL;
   table->destroy = NULL;
}

/* ==================================================== */

void 
gnc_table_destroy (Table * table)
{
   /* invoke destroy callback */
   if (table->destroy)
     table->destroy(table);

   /* free the dynamic structures */
   gnc_table_free_data (table);

   /* free the pointer arrays */
   if (table->virt_cells != NULL)
     g_ptr_array_free(table->virt_cells, FALSE);
   if (table->phys_cells != NULL)
     g_ptr_array_free(table->phys_cells, FALSE);

   /* intialize vars to null value so that any access is voided. */
   gnc_table_init (table);

   g_free (table);
}

/* ==================================================== */

VirtualCell *
gnc_table_get_virtual_cell (Table *table, int virt_row, int virt_col)
{
  TableCell *tcell;
  guint index;

  if (table == NULL)
    return NULL;

  if ((virt_row < 0) || (virt_row >= table->num_virt_rows) ||
      (virt_col < 0) || (virt_col >= table->num_virt_cols))
    return NULL;

  index = (virt_row * table->num_virt_cols) + virt_col;

  tcell = table->virt_cells->pdata[index];

  return &tcell->virt_cell;
}

/* ==================================================== */

PhysicalCell *
gnc_table_get_physical_cell (Table *table, int phys_row, int phys_col)
{
  TableCell *tcell;
  guint index;

  if (table == NULL)
    return NULL;

  if ((phys_row < 0) || (phys_row >= table->num_phys_rows) ||
      (phys_col < 0) || (phys_col >= table->num_phys_cols))
    return NULL;

  index = (phys_row * table->num_phys_cols) + phys_col;

  tcell = table->phys_cells->pdata[index];

  return &tcell->phys_cell;
}

/* ==================================================== */

VirtualCell *
gnc_table_get_header_cell (Table *table)
{
  return gnc_table_get_virtual_cell (table, 0, 0);
}

/* ==================================================== */

void 
gnc_table_set_size (Table * table,
                    int phys_rows, int phys_cols,
                    int virt_rows, int virt_cols)
{
   /* Invalidate the current cursor position, if the array is
    * shrinking. This must be done since the table is probably
    * shrinking because some rows were deleted, and there's a pretty
    * good chance (100% with current design) that the cursor is
    * located on the deleted rows.  */
   if ((virt_rows < table->num_virt_rows) ||
       (virt_cols < table->num_virt_cols) ||
       (phys_rows < table->num_phys_rows) ||
       (phys_cols < table->num_phys_cols)) 
   {
      CellBlock *curs;

      table->current_cursor_virt_row = -1;
      table->current_cursor_virt_col = -1;
      table->current_cursor_phys_row = -1;
      table->current_cursor_phys_col = -1;

      curs = table->current_cursor;

      if (curs)
        curs->user_data = NULL;

      table->current_cursor = NULL;
   }

   gnc_table_resize (table, phys_rows, phys_cols, virt_rows, virt_cols);

   /* invalidate the "previous" traversed cell */
   table->prev_phys_traverse_row = -1;
   table->prev_phys_traverse_col = -1;
}

/* ==================================================== */

static void
gnc_table_free_data (Table * table)
{
  if (table == NULL)
    return;

  if (table->virt_cells != NULL)
  {
    gpointer *tcp;
    guint len;

    tcp = table->virt_cells->pdata;
    len = table->virt_cells->len;

    while (len > 0)
    {
      gnc_virtual_cell_free(*tcp);
      tcp++;
      len--;
    }

    g_ptr_array_set_size(table->virt_cells, 0);
  }

  if (table->phys_cells != NULL)
  {
    gpointer *tcp;
    guint len;

    tcp = table->phys_cells->pdata;
    len = table->phys_cells->len;

    while (len > 0)
    {
      gnc_physical_cell_free(*tcp);
      tcp++;
      len--;
    }

    g_ptr_array_set_size(table->phys_cells, 0);
  }
}

/* ==================================================== */

static void
gnc_virtual_location_init (VirtualLocation *vloc)
{
  if (vloc == NULL)
    return;

  vloc->phys_row_offset = -1;
  vloc->phys_col_offset = -1;
  vloc->virt_row = -1;
  vloc->virt_col = -1;
}

/* ==================================================== */

static void
gnc_physical_location_init (PhysicalLocation *ploc)
{
  if (ploc == NULL)
    return;

  ploc->phys_row = -1;
  ploc->phys_col = -1;
}

/* ==================================================== */

static TableCell *
gnc_virtual_cell_new (void)
{
  TableCell *tcell;
  VirtualCell *vcell;

  tcell = g_chunk_new(TableCell, cell_mem_chunk);

  vcell = &tcell->virt_cell;

  vcell->cellblock = NULL;
  vcell->user_data = NULL;

  gnc_physical_location_init(&vcell->phys_loc);

  return tcell;
}

/* ==================================================== */

static void
gnc_virtual_cell_free (TableCell *tcell)
{
  if (tcell == NULL)
    return;

  g_mem_chunk_free(cell_mem_chunk, tcell);
}

/* ==================================================== */

static TableCell *
gnc_physical_cell_new (void)
{
  TableCell *tcell;
  PhysicalCell *pcell;

  tcell = g_chunk_new(TableCell, cell_mem_chunk);

  pcell = &tcell->phys_cell;

  pcell->entry = strdup("");
  assert(pcell->entry != NULL);

  pcell->fg_color = 0x000000; /* black */
  pcell->bg_color = 0xffffff; /* white */

  gnc_virtual_location_init(&pcell->virt_loc);

  return tcell;
}

/* ==================================================== */

static void
gnc_physical_cell_free (TableCell *tcell)
{
  if (tcell == NULL)
    return;

  if (tcell->phys_cell.entry != NULL)
    free(tcell->phys_cell.entry);

  tcell->phys_cell.entry = NULL;

  g_mem_chunk_free(cell_mem_chunk, tcell);
}

/* ==================================================== */

typedef TableCell * (*CellAllocator)   (void);
typedef void        (*CellDeAllocator) (TableCell *);

static void
gnc_array_resize (GPtrArray *array,
                  int rows, int cols,
                  CellAllocator allocator,
                  CellDeAllocator deallocator)
{
  guint old_len = array->len;
  guint new_len = rows * cols;

  if (new_len == old_len)
    return;

  /* If shrinking, free extra cells */
  if (new_len < old_len)
  {
    gpointer *tcp;
    guint i;

    tcp = &array->pdata[new_len];
    for (i = new_len; i < old_len; i++, tcp++)
      deallocator(*tcp);
  }

  /* Change the size */
  g_ptr_array_set_size(array, new_len);

  /* If expanding, create the new cells */
  if (new_len > old_len)
  {
    gpointer *tcp;
    guint i;

    tcp = &array->pdata[old_len];
    for (i = old_len; i < new_len; i++, tcp++)
      *tcp = allocator();
  }
}

/* ==================================================== */

static void 
gnc_table_resize (Table * table,
                  int new_phys_rows, int new_phys_cols,
                  int new_virt_rows, int new_virt_cols)
{
  if (!table) return;

  if ((new_phys_rows < new_virt_rows) ||
      (new_phys_cols < new_virt_cols))
  {
    FATAL ("xaccTableResize(): the number of physical rows (%d %d)"
           "must equal or exceed the number of virtual rows (%d %d)\n",
           new_phys_rows, new_phys_cols,
           new_virt_rows, new_virt_cols);
    exit (1);
  }

  gnc_array_resize (table->virt_cells,
                    new_virt_rows, new_virt_cols,
                    gnc_virtual_cell_new,
                    gnc_virtual_cell_free);

  gnc_array_resize (table->phys_cells,
                    new_phys_rows, new_phys_cols,
                    gnc_physical_cell_new,
                    gnc_physical_cell_free);

  table->num_phys_rows = new_phys_rows;
  table->num_phys_cols = new_phys_cols;

  table->num_virt_rows = new_virt_rows;
  table->num_virt_cols = new_virt_cols;
}

/* ==================================================== */

void
gnc_table_set_cursor (Table *table, CellBlock *curs,
                      int phys_row_origin, int phys_col_origin,
                      int virt_row, int virt_col)
{
  VirtualCell *vcell;
  int cell_row, cell_col;

  if ((table == NULL) || (curs == NULL))
    return;

  if ((phys_row_origin < 0) || (phys_row_origin >= table->num_phys_rows) ||
      (phys_col_origin < 0) || (phys_col_origin >= table->num_phys_cols))
    return;

  vcell = gnc_table_get_virtual_cell (table, virt_row, virt_col);
  if (vcell == NULL)
    return;

  /* this cursor is the handler for this block */
  vcell->cellblock = curs;

  vcell->phys_loc.phys_row = phys_row_origin;
  vcell->phys_loc.phys_col = phys_col_origin;

  /* intialize the mapping so that we will be able to find
   * the handler, given this range of physical cell addresses */
  for (cell_row = 0; cell_row < curs->numRows; cell_row++)
    for (cell_col = 0; cell_col < curs->numCols; cell_col++)
    {
      PhysicalCell *pcell;

      pcell = gnc_table_get_physical_cell (table,
                                           phys_row_origin + cell_row,
                                           phys_col_origin + cell_col);

      pcell->virt_loc.phys_row_offset = cell_row;
      pcell->virt_loc.phys_col_offset = cell_col;
      pcell->virt_loc.virt_row = virt_row;
      pcell->virt_loc.virt_col = virt_col;
    }
}

/* ==================================================== */

/* Change the cell background colors to their "passive" values.  This
 * denotes that the cursor has left this location (which means more or
 * less the same thing as "the current location is no longer being
 * edited.") */
static void 
gnc_table_make_passive (Table *table)
{
  CellBlock *curs;
  PhysicalCell *pcell;
  int phys_row_origin, phys_col_origin;
  int cell_row, cell_col;
  int virt_row;

  pcell = gnc_table_get_physical_cell (table,
                                       table->current_cursor_phys_row,
                                       table->current_cursor_phys_col);
  if (pcell == NULL)
    return;

  phys_row_origin = table->current_cursor_phys_row;
  phys_col_origin = table->current_cursor_phys_col;
  phys_row_origin -= pcell->virt_loc.phys_row_offset;
  phys_col_origin -= pcell->virt_loc.phys_col_offset;

  virt_row = pcell->virt_loc.virt_row;

  curs = table->current_cursor;

  for (cell_row=0; cell_row < curs->numRows; cell_row++)
    for (cell_col = 0; cell_col < curs->numCols; cell_col++)
    {
      BasicCell *cell;
      guint32 color;

      pcell = gnc_table_get_physical_cell (table,
                                           phys_row_origin + cell_row,
                                           phys_col_origin + cell_col);

      if (table->alternate_bg_colors)
      {
        if ((virt_row % 2) == 1)
          color = curs->passive_bg_color;
        else
          color = curs->passive_bg_color2;
      }
      else if (cell_row == 0)
        color = curs->passive_bg_color;
      else
        color = curs->passive_bg_color2;

      pcell->bg_color = color;

      cell = curs->cells[cell_row][cell_col];
      if (cell)
      {
        if (cell->use_bg_color)
          pcell->bg_color = cell->bg_color;
        if (cell->use_fg_color)
          pcell->fg_color = cell->fg_color;
      }
    }
}


/* ==================================================== */

static void 
gnc_table_move_cursor_internal (Table *table,
                                int new_phys_row, int new_phys_col,
                                gboolean do_move_gui)
{
  int cell_row, cell_col;
  int phys_row_origin, phys_col_origin;
  int new_virt_row, new_virt_col;
  PhysicalCell *pcell;
  VirtualCell *vcell;
  CellBlock *curs;

  ENTER("new_phys=(%d %d) do_move_gui=%d\n", 
        new_phys_row, new_phys_col, do_move_gui);

  /* Change the cell background colors to their "passive" values.
   * This denotes that the cursor has left this location (which means
   * more or less the same thing as "the current location is no longer
   * being edited.") */
  gnc_table_make_passive (table);

  /* call the callback, allowing the app to commit any changes
   * associated with the current location of the cursor. Note that
   * this callback may recursively call this routine. */
  if (table->move_cursor)
  {
    (table->move_cursor) (table, &new_phys_row, &new_phys_col);

    /* The above callback can cause this routine to be called
     * recursively. As a result of this recursion, the cursor may
     * have gotten repositioned. We need to make sure we make
     * passive again. */
    gnc_table_make_passive (table);
    if (do_move_gui)
      gnc_table_refresh_current_cursor_gui (table, FALSE);
  }

  pcell = gnc_table_get_physical_cell (table, new_phys_row, new_phys_col);
  if (pcell == NULL)
  {
    new_virt_row = -1;
    new_virt_col = -1;
  }
  else
  {
    new_virt_row = pcell->virt_loc.virt_row;
    new_virt_col = pcell->virt_loc.virt_col;
  }

  /* invalidate the cursor for now; we'll fix it back up below */
  table->current_cursor_phys_row = -1;
  table->current_cursor_phys_col = -1;
  table->current_cursor_virt_row = -1;
  table->current_cursor_virt_col = -1;
  table->prev_phys_traverse_row = -1;
  table->prev_phys_traverse_col = -1;
  curs = table->current_cursor;
  if (curs)
    curs->user_data = NULL;
  table->current_cursor = NULL;

  /* check for out-of-bounds conditions (which may be deliberate) */
  if ((0 > new_virt_row) || (0 > new_virt_col))
  {
    /* if the location is invalid, then we should take this 
     * as a command to unmap the cursor gui.  So do it .. */
    if (do_move_gui && curs)
    {
      for (cell_row = 0; cell_row < curs->numRows; cell_row++)
        for (cell_col = 0; cell_col < curs->numCols; cell_col++)
        {
          BasicCell *cell;

          cell = curs->cells[cell_row][cell_col];
          if (cell)
          {
            cell->changed = 0;
            if (cell->move)
              (cell->move) (cell, -1, -1);
          }
        }
    }
    LEAVE("out of bounds\n");
    return;
  }

  if (new_virt_row >= table->num_virt_rows) return;
  if (new_virt_col >= table->num_virt_cols) return;
  if (new_phys_row >= table->num_phys_rows) return;
  if (new_phys_col >= table->num_phys_cols) return;

  /* ok, we now have a valid position.  Find the new cursor to use,
   * and initialize its cells */
  vcell = gnc_table_get_virtual_cell (table, new_virt_row, new_virt_col);
  curs = vcell->cellblock;
  table->current_cursor = curs;

  /* record the new position */
  table->current_cursor_virt_row = new_virt_row;
  table->current_cursor_virt_col = new_virt_col;
  table->current_cursor_phys_row = new_phys_row;
  table->current_cursor_phys_col = new_phys_col;

  /* compute some useful offsets */
  phys_row_origin = new_phys_row;
  phys_row_origin -= pcell->virt_loc.phys_row_offset;

  phys_col_origin = new_phys_col;
  phys_col_origin -= pcell->virt_loc.phys_col_offset;

  /* setting the previous traversal value to the last of a traversal chain
   * will guarantee that the first entry into a register will occur at the
   * first cell */
  table->prev_phys_traverse_row = (phys_row_origin +
                                   curs->last_reenter_traverse_row);
  table->prev_phys_traverse_col = (phys_col_origin +
                                   curs->last_reenter_traverse_col);

  /* update the cell values to reflect the new position */
  for (cell_row = 0; cell_row < curs->numRows; cell_row++)
    for (cell_col = 0; cell_col < curs->numCols; cell_col++)
    {
      BasicCell *cell;

      pcell = gnc_table_get_physical_cell (table,
                                           phys_row_origin + cell_row,
                                           phys_col_origin + cell_col);

      /* change the cursor row to the active color */
      pcell->bg_color = curs->active_bg_color;

      cell = curs->cells[cell_row][cell_col];
      if (cell)
      {
        if (do_move_gui) {
          /* if a cell has a GUI, move that first, before setting
           * the cell value.  Otherwise, we'll end up putting the 
           * new values in the old cell locations, and that would 
           * lead to confusion of all sorts. */
          if (cell->move)
            (cell->move) (cell,
                          phys_row_origin + cell_row,
                          phys_col_origin + cell_col);
        }

        /* OK, now copy the string value from the table at large 
         * into the cell handler. */
        if (XACC_CELL_ALLOW_SHADOW & (cell->input_output))
        {
          xaccSetBasicCellValue (cell, pcell->entry);
          cell->changed = 0;
        }

        if (cell->use_bg_color)
          pcell->bg_color = cell->bg_color;
        if (cell->use_fg_color)
          pcell->fg_color = cell->fg_color;
      }
    }

  curs->user_data = vcell->user_data;

  LEAVE("did move\n");
}

/* ==================================================== */

void
gnc_table_move_cursor (Table *table, int new_phys_row, int new_phys_col)
{
  if (!table) return;

  gnc_table_move_cursor_internal (table, new_phys_row, new_phys_col, FALSE);
}

/* same as above, but be sure to deal with GUI elements as well */
void
gnc_table_move_cursor_gui (Table *table, int new_phys_row, int new_phys_col)
{
  if (!table) return;

  gnc_table_move_cursor_internal (table, new_phys_row, new_phys_col, TRUE);
}

/* ==================================================== */

void
gnc_table_commit_cursor (Table *table)
{
  CellBlock *curs;
  VirtualCell *vcell;
  PhysicalCell *pcell;
  int cell_row, cell_col;
  int virt_row, virt_col;
  int phys_row, phys_col;
  int phys_row_origin, phys_col_origin;

  if (!table) return;

  curs = table->current_cursor;
  if (!curs) return;

  virt_row = table->current_cursor_virt_row;
  virt_col = table->current_cursor_virt_col;

  /* can't commit if cursor is bad */
  if ((0 > virt_row) || (0 > virt_col)) return;
  if (virt_row >= table->num_virt_rows) return;
  if (virt_col >= table->num_virt_cols) return;

  /* compute the true origin of the cell block */
  phys_row = table->current_cursor_phys_row;
  phys_col = table->current_cursor_phys_col;

  pcell = gnc_table_get_physical_cell (table, phys_row, phys_col);
  if (pcell == NULL) return;

  phys_row_origin = table->current_cursor_phys_row;
  phys_col_origin = table->current_cursor_phys_col;
  phys_row_origin -= pcell->virt_loc.phys_row_offset;
  phys_col_origin -= pcell->virt_loc.phys_col_offset;

  for (cell_row = 0; cell_row < curs->numRows; cell_row++)
    for (cell_col = 0; cell_col < curs->numCols; cell_col++)
    {
      BasicCell *cell;

      cell = curs->cells[cell_row][cell_col];
      if (cell)
      {
        pcell = gnc_table_get_physical_cell (table,
                                             phys_row_origin + cell_row,
                                             phys_col_origin + cell_col);

        if (pcell->entry)
          free (pcell->entry);
        pcell->entry = strdup (cell->value);
        if (cell->use_bg_color)
          pcell->bg_color = cell->bg_color;
        if (cell->use_fg_color)
          pcell->fg_color = cell->fg_color;
      }
    }

  vcell = gnc_table_get_virtual_cell (table, virt_row, virt_col);
  vcell->user_data = curs->user_data;
}

/* ==================================================== */

void
gnc_table_refresh_header (Table *table)
{
  int cell_row, cell_col;
  VirtualCell *vcell;
  CellBlock *cb;

  if (!table) return;

  vcell = gnc_table_get_header_cell (table);
  if (vcell == NULL) return;

  cb = vcell->cellblock;
  if (cb == NULL) return;

  for (cell_row = 0; cell_row < cb->numRows; cell_row++)
    for (cell_col = 0; cell_col < cb->numCols; cell_col++)
    {
      PhysicalCell *pcell;
      BasicCell *cell;

      /* Assumes header starts at physical (0, 0) */
      pcell = gnc_table_get_physical_cell (table, cell_row, cell_col);

      if (pcell->entry)
        free(pcell->entry);

      cell = cb->cells[cell_row][cell_col];

      if (cell && cell->value)
        pcell->entry = strdup (cell->value);
      else
        pcell->entry = strdup ("");
    }
}

/* ==================================================== */

/* gnc_table_verify_cursor_position checks the location of the cursor
 * with respect to a row/column position, and repositions the cursor
 * if necessary. This includes saving any uncommited data in the old
 * cursor, and then moving the cursor and its GUI. Returns true if the
 * cursor was repositioned. */
gboolean
gnc_table_verify_cursor_position (Table *table, int phys_row, int phys_col)
{
  gboolean do_commit = FALSE;
  gboolean moved_cursor = FALSE;

  if (!table) return FALSE;

  /* Someone may be trying to intentionally invalidate the cursor, in
   * which case the physical addresses could be out of bounds. For
   * example, in order to unmap it in preparation for a reconfig.
   * So, if the specified location is out of bounds, then the cursor
   * MUST be moved. */

  if ((0 > phys_row) || (0 > phys_col)) do_commit = TRUE;
  if (phys_row >= table->num_phys_rows) do_commit = TRUE;
  if (phys_col >= table->num_phys_cols) do_commit = TRUE;

  /* Physical position is valid. Check the virtual position. */
  if (!do_commit)
  {
    PhysicalCell *pcell;
    int virt_row, virt_col;

    pcell = gnc_table_get_physical_cell (table, phys_row, phys_col);

    virt_row = pcell->virt_loc.virt_row;
    virt_col = pcell->virt_loc.virt_col;

    if ((virt_row != table->current_cursor_virt_row) ||
        (virt_col != table->current_cursor_virt_col))
      do_commit = TRUE;
  }

  if (do_commit)
  {
    /* before leaving the current virtual position, commit any edits
     * that have been accumulated in the cursor */
    gnc_table_commit_cursor (table);
    gnc_table_move_cursor_gui (table, phys_row, phys_col);
    moved_cursor = TRUE;
  }
  else
  {
    /* The request might be to move to a cell that is one column over.
     * If so, then do_commit will be zero, as there will have been no
     * reason to actually move a cursor.  However, we want to keep
     * positions accurate, so record the new location.  (The move may
     * may also be one row up or down, which, for a two-row cursor,
     * also might not require a cursor movement). */
    if (table->current_cursor_phys_row != phys_row)
    {
      table->current_cursor_phys_row = phys_row;
      moved_cursor = TRUE;
    }

    if (table->current_cursor_phys_col != phys_col)
    {
      table->current_cursor_phys_col = phys_col;
      moved_cursor = TRUE;
    }
  }

  return moved_cursor;
}

/* ==================================================== */

void *
gnc_table_get_user_data_physical (Table *table, int phys_row, int phys_col)
{
  PhysicalCell *pcell;
  VirtualCell *vcell;
  int virt_row, virt_col;

  if (!table) return NULL;

  pcell = gnc_table_get_physical_cell (table, phys_row, phys_col);
  if (pcell == NULL)
    return NULL;

  virt_row = pcell->virt_loc.virt_row;
  virt_col = pcell->virt_loc.virt_col;

  vcell = gnc_table_get_virtual_cell (table, virt_row, virt_col);
  if (vcell == NULL)
    return NULL;

  return vcell->user_data;
}

/* ==================================================== */

void *
gnc_table_get_user_data_virtual (Table *table, int virt_row, int virt_col)
{
  VirtualCell *vcell;

  if (!table) return NULL;

  vcell = gnc_table_get_virtual_cell (table, virt_row, virt_col);
  if (vcell == NULL)
    return NULL;

  return vcell->user_data;
}

/* ==================================================== */

/* if any of the cells have GUI specific components that need 
 * initialization, initialize them now. The realize() callback
 * on the cursor cell is how we inform the cell handler that 
 * now is the time to initialize its GUI.  */
void
gnc_table_create_cursor (Table * table, CellBlock *curs)
{
  int cell_row, cell_col;

  if (!curs || !table) return;  
  if (!table->ui_data) return;
  
  for (cell_row = 0; cell_row < curs->numRows; cell_row++)
    for (cell_col = 0; cell_col < curs->numCols; cell_col++)
    {
      BasicCell *cell;
      cell = curs->cells[cell_row][cell_col];
      if (cell && cell->realize)
        cell->realize (cell, table->ui_data);
    }
}

/* ==================================================== */

void
gnc_table_wrap_verify_cursor_position (Table *table,
                                       int phys_row, int phys_col)
{
   CellBlock *save_curs = table->current_cursor;
   const int save_phys_row = table->current_cursor_phys_row;
   const int save_phys_col = table->current_cursor_phys_col;
   gboolean moved_cursor;

   if (!table) return;

   ENTER("(%d %d)", phys_row, phys_col);

   /* VerifyCursor will do all sorts of gui-independent machinations */
   moved_cursor = gnc_table_verify_cursor_position (table, phys_row, phys_col);

   if (moved_cursor)
   {
      /* make sure *both* the old and the new cursor rows get redrawn */
      gnc_table_refresh_current_cursor_gui (table, TRUE);
      gnc_table_refresh_cursor_gui (table, save_curs,
                                    save_phys_row, save_phys_col, FALSE);
   }

   LEAVE ("\n");
}

/* ==================================================== */

void        
gnc_table_refresh_current_cursor_gui (Table * table, gboolean do_scroll)
{
  if (!table) return;

  gnc_table_refresh_cursor_gui (table, table->current_cursor,
                                table->current_cursor_phys_row,
                                table->current_cursor_phys_col,
                                do_scroll);
}

/* ==================================================== */

gboolean
gnc_table_physical_cell_valid(Table *table,
                              int phys_row, int phys_col,
                              gboolean exact_cell)
{
  CellBlock *cb;
  VirtualCell *vcell;
  PhysicalCell *pcell;
  gboolean invalid = FALSE;
  int cell_row, cell_col;
  int virt_row, virt_col;
  int io_flag;

  if (!table) return FALSE;

  pcell = gnc_table_get_physical_cell(table, phys_row, phys_col);

  /* can't edit outside of the physical space */
  if (pcell == NULL)
    return FALSE;

  /* header rows cannot be modified */
  vcell = gnc_table_get_header_cell (table);
  if (vcell == NULL)
    return FALSE;

  invalid = invalid || (phys_row < vcell->cellblock->numRows);

  /* compute the cell location */
  virt_row = pcell->virt_loc.virt_row;
  virt_col = pcell->virt_loc.virt_col;

  cell_row = pcell->virt_loc.phys_row_offset;
  cell_col = pcell->virt_loc.phys_col_offset;

  /* verify that offsets are valid. This may occur if the app that is
   * using the table has a paritally initialized cursor. (probably due
   * to a programming error, but maybe they meant to do this). */
  invalid = invalid || (0 > virt_row);
  invalid = invalid || (0 > virt_col);
  invalid = invalid || (0 > cell_row);
  invalid = invalid || (0 > cell_col);

  if (invalid) return FALSE;

  cb = vcell->cellblock;

  /* check for a cell handler, but only if cell address is valid */
  if (cb == NULL) return FALSE;
  if (cb->cells[cell_row][cell_col] == NULL) return FALSE;

  /* if cell is marked as output-only, you can't enter */
  io_flag = cb->cells[cell_row][cell_col]->input_output;
  if (0 == (XACC_CELL_ALLOW_INPUT & io_flag)) return FALSE;

  /* if cell is pointer only and this is not an exact pointer test,
   * it cannot be entered. */
  if (!exact_cell && ((XACC_CELL_ALLOW_EXACT_ONLY & io_flag) != 0))
    return FALSE;

  if (invalid) return FALSE;

  return TRUE;
}

/* ==================================================== */

/* Handle the non gui-specific parts of a cell enter callback */
const char *
gnc_table_enter_update(Table *table,
                       int phys_row, int phys_col,
                       int *cursor_position,
                       int *start_selection,
                       int *end_selection)
{
  const char *retval = NULL;
  PhysicalCell *pcell;
  CellEnterFunc enter;
  CellBlock *cb;
  int cell_row;
  int cell_col;

  if (table == NULL)
    return NULL;

  cb = table->current_cursor;

  pcell = gnc_table_get_physical_cell (table, phys_row, phys_col);
  if (pcell == NULL)
    return NULL;

  cell_row = pcell->virt_loc.phys_row_offset;
  cell_col = pcell->virt_loc.phys_col_offset;

  ENTER("enter %d %d (relrow=%d relcol=%d) cell=%p val=%s\n", 
        phys_row, phys_col, cell_row, cell_col, 
        cb->cells[cell_row][cell_col], pcell->entry);

  /* OK, if there is a callback for this cell, call it */
  enter = cb->cells[cell_row][cell_col]->enter_cell;

  if (enter)
  {
    const char *val;

    DEBUG("gnc_table_enter_update(): %d %d has enter handler\n",
          cell_row, cell_col);

    val = pcell->entry;
    retval = enter(cb->cells[cell_row][cell_col], val,
                   cursor_position, start_selection, end_selection);

    /* enter() might return null, or it might return a pointer to val,
     * or it might return a new pointer (to newly malloc memory).
     * Replace the old pointer with a new one only if the new one is
     * different, freeing the old one.  (Doing a strcmp would leak
     * memory). */
    if (retval && (val != retval))
    {
      if (safe_strcmp(retval, val) != 0)
        (cb->cells[cell_row][cell_col])->changed = GNC_CELL_CHANGED;
      if (pcell->entry)
        free (pcell->entry);
      pcell->entry = (char *) retval;
    }
    else
      retval = NULL;
  }

  if (table->set_help)
  {
    BasicCell *cell;
    char *help_str;

    cell = cb->cells[cell_row][cell_col];
    help_str = xaccBasicCellGetHelp(cell);

    table->set_help(table, help_str);

    if (help_str != NULL)
      free(help_str);
  }

  /* record this position as the cell that will be
   * traversed out of if a traverse even happens */
  table->prev_phys_traverse_row = phys_row;
  table->prev_phys_traverse_col = phys_col;

  LEAVE("return %s\n", retval);

  return retval;
}

/* ==================================================== */

const char *
gnc_table_leave_update(Table *table,
                       int phys_row, int phys_col,
                       const char *callback_text)
{
  const char *retval = NULL;
  PhysicalCell *pcell;
  CellLeaveFunc leave;
  CellBlock *cb;
  int cell_row;
  int cell_col;

  if (table == NULL)
    return NULL;

  cb = table->current_cursor;

  pcell = gnc_table_get_physical_cell (table, phys_row, phys_col);
  if (pcell == NULL)
    return NULL;

  cell_row = pcell->virt_loc.phys_row_offset;
  cell_col = pcell->virt_loc.phys_col_offset;

  ENTER("proposed (%d %d) rel(%d %d) \"%s\"\n",
        phys_row, phys_col, cell_row, cell_col, callback_text);

  if (!callback_text)
    callback_text = "";

  /* OK, if there is a callback for this cell, call it */
  leave = cb->cells[cell_row][cell_col]->leave_cell;
  if (leave)
  {
    retval = leave(cb->cells[cell_row][cell_col], callback_text);

    /* leave() might return null, or it might return a pointer to
     * callback_text, or it might return a new pointer (to newly
     * malloced memory). */
    if (retval == callback_text)
      retval = NULL;
  }

  if (!retval)
    retval = strdup (callback_text);

  /* save whatever was returned; but lets check for  
   * changes to avoid roiling the cells too much. */
  if (pcell->entry)
  {
    if (safe_strcmp (pcell->entry, retval))
    {
      free (pcell->entry);
      pcell->entry = (char *) retval;
      (cb->cells[cell_row][cell_col])->changed = GNC_CELL_CHANGED;
    }
    else
    {
      /* leave() allocated memory, which we will not be using */
      free ((char *) retval);
      retval = NULL;
    }
  }
  else
  {
    pcell->entry = (char *) retval;
    (cb->cells[cell_row][cell_col])->changed = GNC_CELL_CHANGED;
  }

  /* return the result of the final decisionmaking */
  if (safe_strcmp (pcell->entry, callback_text))
    retval = pcell->entry;
  else
    retval = NULL;

  LEAVE("return %s\n", retval);

  return retval;
}

/* ==================================================== */

/* returned result should not be touched by the caller.
 * NULL return value means the edit was rejected. */
const char *
gnc_table_modify_update(Table *table,
                        int phys_row, int phys_col,
                        const char *oldval,
                        const char *change,
                        char *newval,
                        int *cursor_position,
                        int *start_selection,
                        int *end_selection)
{
  const char *retval = NULL;
  CellModifyVerifyFunc mv;
  PhysicalCell *pcell;
  CellBlock *cb;
  int cell_row;
  int cell_col;

  if (table == NULL)
    return NULL;

  cb = table->current_cursor;

  pcell = gnc_table_get_physical_cell (table, phys_row, phys_col);
  if (pcell == NULL)
    return NULL;

  cell_row = pcell->virt_loc.phys_row_offset;
  cell_col = pcell->virt_loc.phys_col_offset;

  ENTER ("\n");

  /* OK, if there is a callback for this cell, call it */
  mv = cb->cells[cell_row][cell_col]->modify_verify;
  if (mv)
  {
    retval = mv (cb->cells[cell_row][cell_col],
                 oldval, change, newval, cursor_position,
                 start_selection, end_selection);

    /* if the callback returned a non-null value, allow the edit */
    if (retval)
    {
      /* update data. bounds check done earlier */
      if (pcell->entry)
        free (pcell->entry);
      pcell->entry = (char *) retval;
      (cb->cells[cell_row][cell_col])->changed = GNC_CELL_CHANGED;
    }
  }
  else
  {
    /* update data. bounds check done earlier */
    if (pcell->entry)
      free (pcell->entry);
    pcell->entry = newval;
    retval = newval;
    (cb->cells[cell_row][cell_col])->changed = GNC_CELL_CHANGED;
  }

  if (table->set_help)
  {
    BasicCell *cell;
    char *help_str;

    cell = cb->cells[cell_row][cell_col];
    help_str = xaccBasicCellGetHelp(cell);

    table->set_help(table, help_str);

    if (help_str != NULL)
      free(help_str);
  }

  LEAVE ("change %d %d (relrow=%d relcol=%d) cell=%p val=%s\n", 
         phys_row, phys_col, cell_row, cell_col, 
         cb->cells[cell_row][cell_col], pcell->entry);

  return retval;
}

/* ==================================================== */

gboolean
gnc_table_direct_update(Table *table,
                        int phys_row, int phys_col,
                        const char *oldval,
                        char **newval_ptr,
                        int *cursor_position,
                        int *start_selection,
                        int *end_selection,
                        void *gui_data)
{
  PhysicalCell *pcell;
  gboolean result;
  BasicCell *cell;
  CellBlock *cb;
  int cell_row;
  int cell_col;

  if (table == NULL)
    return FALSE;

  cb = table->current_cursor;

  pcell = gnc_table_get_physical_cell (table, phys_row, phys_col);
  if (pcell == NULL)
    return FALSE;

  cell_row = pcell->virt_loc.phys_row_offset;
  cell_col = pcell->virt_loc.phys_col_offset;

  cell = cb->cells[cell_row][cell_col];

  ENTER ("\n");

  if (cell->direct_update == NULL)
    return FALSE;

  result = cell->direct_update(cell, oldval, newval_ptr, cursor_position,
                               start_selection, end_selection, gui_data);

  if ((*newval_ptr != oldval) && (*newval_ptr != NULL)) {
    if (pcell->entry)
      free (pcell->entry);
    pcell->entry = *newval_ptr;
    cell->changed = GNC_CELL_CHANGED;
  }

  if (table->set_help)
  {
    char *help_str;

    help_str = xaccBasicCellGetHelp(cell);

    table->set_help(table, help_str);

    if (help_str != NULL)
      free(help_str);
  }

  return result;
}

/* ==================================================== */

gboolean
gnc_table_find_valid_cell_horiz(Table *table,
                                int *phys_row, int *phys_col,
                                gboolean exact_cell)
{
  int left = *phys_col - 1;
  int right = *phys_col + 1;

  if ((*phys_row < 0) || (*phys_col < 0) ||
      (*phys_row >= table->num_phys_rows) ||
      (*phys_col >= table->num_phys_cols))
    return FALSE;

  if (gnc_table_physical_cell_valid(table, *phys_row, *phys_col, exact_cell))
    return TRUE;

  while (left >= 0 || right < table->num_phys_cols)
  {
    if (gnc_table_physical_cell_valid(table, *phys_row, right, FALSE))
    {
      *phys_col = right;
      return TRUE;
    }

    if (gnc_table_physical_cell_valid(table, *phys_row, left, FALSE))
    {
      *phys_col = left;
      return TRUE;
    }

    left--;
    right++;
  }

  return FALSE;
}

/* ==================================================== */

gboolean
gnc_table_traverse_update(Table *table,
                          int phys_row, int phys_col,
                          gncTableTraversalDir dir,
                          int *dest_row,
                          int *dest_col) 
{
  CellBlock *cb;

  if (table == NULL)
    return FALSE;

  cb = table->current_cursor;

  ENTER("proposed (%d %d) -> (%d %d)\n",
        phys_row, phys_col, *dest_row, *dest_col);

  /* first, make sure our destination cell is valid.  If it is out of
   * bounds report an error. I don't think this ever happens. */
  if ((*dest_row >= table->num_phys_rows) || (*dest_row < 0) ||
      (*dest_col >= table->num_phys_cols) || (*dest_col < 0)) 
  {
    PERR("destination (%d, %d) out of bounds (%d, %d)\n",
         *dest_row, *dest_col, table->num_phys_rows, table->num_phys_cols);
    return TRUE;
  }

  /* next, check the current row and column.  If they are out of bounds
   * we can recover by treating the traversal as a mouse point. This can
   * occur whenever the register widget is resized smaller, maybe?. */
  if ((phys_row >= table->num_phys_rows) || (phys_row < 0) ||
      (phys_col >= table->num_phys_cols) || (phys_col < 0))
  {

    PINFO("source (%d, %d) out of bounds (%d, %d)\n",
	  phys_row, phys_col, table->num_phys_rows, table->num_phys_cols);
    table->prev_phys_traverse_row = *dest_row;
    table->prev_phys_traverse_col = *dest_col;
    dir = GNC_TABLE_TRAVERSE_POINTER;
  }

  /* process forward-moving traversals */
  switch(dir)
  {
    case GNC_TABLE_TRAVERSE_RIGHT:
    case GNC_TABLE_TRAVERSE_LEFT:      
      {
        PhysicalCell *pcell;
        int cell_row, cell_col;

	/* cannot compute the cell location until we have checked that
	 * row and column have valid values. compute the cell
	 * location. */
        pcell = gnc_table_get_physical_cell (table, phys_row, phys_col);
        if (pcell == NULL)
          return FALSE;

        cell_row = pcell->virt_loc.phys_row_offset;
        cell_col = pcell->virt_loc.phys_col_offset;

        if (dir == GNC_TABLE_TRAVERSE_RIGHT)
        {
          *dest_row = (phys_row - cell_row +
                       cb->right_traverse_r[cell_row][cell_col]);
          *dest_col = (phys_col - cell_col +
                       cb->right_traverse_c[cell_row][cell_col]);
        }
        else
        {
          *dest_row = (phys_row - cell_row +
                       cb->left_traverse_r[cell_row][cell_col]);
          *dest_col = (phys_col - cell_col +
                       cb->left_traverse_c[cell_row][cell_col]);
        }
      }

      break;

    case GNC_TABLE_TRAVERSE_UP:
    case GNC_TABLE_TRAVERSE_DOWN:
      {
        VirtualCell *vcell = gnc_table_get_header_cell (table);
	CellBlock *header = vcell->cellblock;
	int new_row = *dest_row;
	int increment;

	/* Keep going in the specified direction until we find a valid
	 * row to land on, or we hit the end of the table. At the end,
	 * turn around and go back until we find a valid row or we get
	 * to where we started. If we still can't find anything, try
	 * going left and right. */
	increment = (dir == GNC_TABLE_TRAVERSE_DOWN) ? 1 : -1;

	while (!gnc_table_physical_cell_valid(table,
                                              new_row, *dest_col, FALSE))
	{
	  if (new_row == phys_row)
          {
            new_row = *dest_row;
            gnc_table_find_valid_cell_horiz(table, &new_row, dest_col, FALSE);
            break;
          }

	  if ((new_row < header->numRows) || (new_row >= table->num_phys_rows))
	  {
	    increment *= -1;
	    new_row = *dest_row;
	  }

	  new_row += increment;
	}

	*dest_row = new_row;
      }

      if (!gnc_table_physical_cell_valid(table, *dest_row, *dest_col, FALSE))
	return TRUE;

      break;

    case GNC_TABLE_TRAVERSE_POINTER:
      if (!gnc_table_find_valid_cell_horiz(table, dest_row, dest_col, TRUE))
        return TRUE;

      break;

    default:
      /* shouldn't be reached */
      assert(0);
      break;
  }

  /* Call the table traverse callback for any modifications. */
  if (table->traverse)
    (table->traverse) (table, dest_row, dest_col, dir);

  table->prev_phys_traverse_row = *dest_row;
  table->prev_phys_traverse_col = *dest_col;

  LEAVE("dest_row = %d, dest_col = %d\n",
        *dest_row, *dest_col);

  return FALSE;
}

/* ================== end of file ======================= */
