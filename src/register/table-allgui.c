/*
 * FILE:
 * table-allgui.c
 *
 * FUNCTION:
 * Implements the gui-independent parts of the table infrastructure.
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cellblock.h"
#include "table-allgui.h"

static void xaccFreeTableEntries (Table * table);
static void xaccTableResize (Table * table,
                 int new_phys_rows, int new_phys_cols,
                 int new_virt_rows, int new_virt_cols);

/* ==================================================== */

Table * 
xaccMallocTable (void)
{
   Table *table;
   table = (Table *) malloc (sizeof (Table));
   xaccInitTable (table);
   return table;
}

/* ==================================================== */

void 
xaccInitTable (Table * table)
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
   table->client_data = NULL;

   table->entries = NULL;
   table->bg_colors = NULL;
   table->fg_colors = NULL;
   table->locators = NULL;
   table->user_data = NULL;
   table->handlers = NULL;

   /* invalidate the "previous" traversed cell */
   table->prev_phys_traverse_row = -1;
   table->prev_phys_traverse_col = -1;

   /* call the "derived" class constructor */
   TABLE_PRIVATE_DATA_INIT (table);
}

/* ==================================================== */

void 
xaccDestroyTable (Table * table)
{
   /* call derived class destructor */
   TABLE_PRIVATE_DATA_DESTROY (table);

   /* free the gui-independent parts */
   xaccFreeTableEntries (table);

   /* intialize vars to null value so that any access is voided. */
   xaccInitTable (table);
   free (table);
}

/* ==================================================== */

void 
xaccSetTableSize (Table * table, int phys_rows, int phys_cols,
                                 int virt_rows, int virt_cols)
{
   /* invalidate the current cursor position, if the array
    * is shrinking.  This must be done since the table is probably
    * shrinking because some rows were deleted, and there's a 
    * pretty good chance (100% with current design) that the
    * cursor is located on the deleted rows.
    */
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
      if (curs) curs->user_data = NULL;
      table->current_cursor = NULL;
   }
   xaccTableResize (table, phys_rows, phys_cols, virt_rows, virt_cols);

   /* invalidate the "previous" traversed cell */
   table->prev_phys_traverse_row = -1;
   table->prev_phys_traverse_col = -1;
}

/* ==================================================== */

#define NOOP(x)   /* a big old no-op */
#define FREEUP(x)  { if(x) free(x); }

#define FREE_ARR(nrows,ncols,arrname,freeup,killval)			\
{									\
   int i,j;								\
   /* free the arrname */						\
   if (table->arrname) {						\
      for (i=0; i<nrows; i++) {						\
         if (table->arrname[i]) {					\
            for (j=0; j<ncols; j++) {					\
               freeup (table->arrname[i][j]);				\
               table->arrname[i][j] = killval;				\
            }								\
            free (table->arrname[i]);					\
         }								\
         table->arrname[i] = NULL;					\
      }									\
      free (table->arrname);						\
   }									\
   table->arrname = NULL;						\
}

#define FREE_PARR(arrname,freeup,killval)				\
   FREE_ARR (table->num_phys_rows, table->num_phys_cols, arrname,freeup,killval);

#define FREE_VARR(arrname,freeup,killval)				\
   FREE_ARR (table->num_virt_rows, table->num_virt_cols, arrname,freeup,killval);


static void
xaccFreeTableEntries (Table * table)
{
   /* free the entries */
   FREE_PARR (entries, FREEUP, NULL);

   /* free the locators */
   FREE_PARR (locators, FREEUP, NULL);

   /* free the foreground and background color arrays */
   FREE_PARR (bg_colors, NOOP, 0xffffff);
   FREE_PARR (fg_colors, NOOP, 0x0);

   /* null out user data and handlers */
   FREE_VARR (handlers,  NOOP, NULL);
   FREE_VARR (user_data, NOOP, NULL);
}

/* ==================================================== */

static Locator *
xaccMallocLocator (void)
{
   Locator *loc;
   loc = (Locator *) malloc (sizeof (Locator));
   loc->phys_row_offset = -1;
   loc->phys_col_offset = -1;
   loc->virt_row = -1;
   loc->virt_col = -1;

   return (loc);
}

/* ==================================================== */

static void 
xaccTableResize (Table * table,
                 int new_phys_rows, int new_phys_cols,
                 int new_virt_rows, int new_virt_cols)
{
   if (!table) return;
   if ((new_phys_rows < new_virt_rows) ||
       (new_phys_cols < new_virt_cols)) {
      printf ("Internal Error: xaccTableResize(): the number of "
         "physical rows must equal or exceed the number of virtual rows \n");
      exit (1);
   }

   /* resize the string data array */
   XACC_RESIZE_ARRAY ((table->num_phys_rows),
               (table->num_phys_cols),
               new_phys_rows,
               new_phys_cols,
               (table->entries),
               char *,
               (strdup ("")),
               free);

   /* resize the locator array */
   XACC_RESIZE_ARRAY ((table->num_phys_rows),
               (table->num_phys_cols),
               new_phys_rows,
               new_phys_cols,
               (table->locators),
               Locator *,
               (xaccMallocLocator ()),
               free);

   /* resize the bg color array (white background)  */
   XACC_RESIZE_ARRAY ((table->num_phys_rows),
               (table->num_phys_cols),
               new_phys_rows,
               new_phys_cols,
               (table->bg_colors),
               uint32,
               ((uint32) 0xffffff),  /* white */
               NOOP);                /* no-op */

   /* resize the foreground color array (black text) */
   XACC_RESIZE_ARRAY ((table->num_phys_rows),
               (table->num_phys_cols),
               new_phys_rows,
               new_phys_cols,
               (table->fg_colors),
               uint32,
               ((uint32) 0x0),      /* black */
               NOOP);               /* no-op */


   /* resize the user-data hooks */
   XACC_RESIZE_ARRAY ((table->num_virt_rows),
               (table->num_virt_cols),
               new_virt_rows,
               new_virt_cols,
               (table->user_data),
               void *,
               (NULL),
               NOOP);        /* no-op */

   /* resize the handler array */
   XACC_RESIZE_ARRAY ((table->num_virt_rows),
               (table->num_virt_cols),
               new_virt_rows,
               new_virt_cols,
               (table->handlers),
               CellBlock *,
               (NULL),
               NOOP);         /* no-op */

   /* call the "derived" class resize method */
   TABLE_PRIVATE_DATA_RESIZE (table,
                 new_phys_rows, new_phys_cols,
                 new_virt_rows, new_virt_cols);

   /* we are done with the physical dimensions. 
    * record them for posterity. */
   table->num_phys_rows = new_phys_rows;
   table->num_phys_cols = new_phys_cols;

   /* we are done with the virtual dimensions. 
    * record them for posterity. */
   table->num_virt_rows = new_virt_rows;
   table->num_virt_cols = new_virt_cols;

}

/* ==================================================== */

void
xaccSetCursor (Table *table, CellBlock *curs,
               int phys_row_origin, int phys_col_origin,
               int virt_row, int virt_col)
{
   int i,j;

   /* reject various bad values that might get passed in */
   if ((!table) || (!curs)) return;
   if ((0 > phys_row_origin) || (0 > phys_col_origin)) return;
   if (phys_row_origin >= table->num_phys_rows) return;
   if (phys_col_origin >= table->num_phys_cols) return;

   if ((0 > virt_row) || (0 > virt_col)) return;
   if (virt_row >= table->num_virt_rows) return;
   if (virt_col >= table->num_virt_cols) return;

   /* this cursor is the handler for this block */
   table->handlers[virt_row][virt_col] = curs;

   /* intialize the mapping so that we will be able to find
    * the handler, given this range of physical cell addressses */
   for (i=0; i<curs->numRows; i++) {
      for (j=0; j<curs->numCols; j++) {
         Locator *loc;
         loc = table->locators[phys_row_origin+i][phys_col_origin+j];
         loc->phys_row_offset = i;
         loc->phys_col_offset = j;
         loc->virt_row = virt_row;
         loc->virt_col = virt_col;
      }
   }
}

/* ==================================================== */

static void 
doMoveCursor (Table *table, int new_phys_row, int new_phys_col, int do_move_gui)
{
   int i,j;
   int phys_row_origin, phys_col_origin;
   int new_virt_row, new_virt_col;
   CellBlock *curs;

   /* Change the cell background colors to thier "passive" values.
    * This denotes that the cursor has left this location (which means more or
    * less the same thing as "the current location is no longer being edited.")
    * (But only do this if the cursor has a valid current location) 
    */
   if ((0 <= table->current_cursor_phys_row) &&
       (0 <= table->current_cursor_phys_col)) 
   {
      int r_origin = table->current_cursor_phys_row;
      int c_origin = table->current_cursor_phys_col;
      curs = table->current_cursor;

      for (i=0; i<curs->numRows; i++) {
         for (j=0; j<curs->numCols; j++) {
            BasicCell *cell;
         
            table->bg_colors[i+r_origin][j+c_origin] = curs->passive_bg_color;
            cell = curs->cells[i][j];
            if (cell) {
               if (cell->use_bg_color) {
                  table->bg_colors[i+r_origin][j+c_origin] = cell->bg_color;
               }
               if (cell->use_fg_color) {
                  table->fg_colors[i+r_origin][j+c_origin] = cell->fg_color;
               }
            }
         }
      }
   }

   /* call the callback, allowing the app to commit any changes 
    * associated with the current location of the cursor.   */
   if (table->move_cursor) {
      (table->move_cursor) (table, new_phys_row, new_phys_col, 
                            table->client_data);
   }

   /* check for out-of-bounds conditions (which may be deliberate) */
   if ((0 > new_phys_row) || (0 > new_phys_col) ||
      (new_phys_row >= table->num_phys_rows) ||
      (new_phys_col >= table->num_phys_cols)) {
      new_virt_row = -1;
      new_virt_col = -1;
   } else {
      new_virt_row = table->locators[new_phys_row][new_phys_col]->virt_row;
      new_virt_col = table->locators[new_phys_row][new_phys_col]->virt_col;
   }

   /* invalidate the cursor for now; we'll fix it back up below */
   table->current_cursor_phys_row = -1;
   table->current_cursor_phys_col = -1;
   table->current_cursor_virt_row = -1;
   table->current_cursor_virt_col = -1;
   table->prev_phys_traverse_row = -1;
   table->prev_phys_traverse_col = -1;
   curs = table->current_cursor;
   if (curs) curs->user_data = NULL;
   table->current_cursor = NULL;

   /* check for out-of-bounds conditions (which may be deliberate) */
   if ((0 > new_virt_row) || (0 > new_virt_col)) {
      /* if the location is invalid, then we should take this 
       * as a command to unmap the cursor gui.  So do it .. */
      if (do_move_gui && curs) {
         for (i=0; i<curs->numRows; i++) {
            for (j=0; j<curs->numCols; j++) {
               BasicCell *cell;
               cell = curs->cells[i][j];
               if (cell) {
                  cell->changed = 0;
                  if (cell->move) {
                     (cell->move) (cell, -1, -1);
                  }
               }
            }
         }
      }
      return;
   }

   if (new_virt_row >= table->num_virt_rows) return;
   if (new_virt_col >= table->num_virt_cols) return;
   if (new_phys_row >= table->num_phys_rows) return;
   if (new_phys_col >= table->num_phys_cols) return;

   /* ok, we now have a valid position.  Find the new cursor to use,
    * and initialize it's cells */
   curs = table->handlers[new_virt_row][new_virt_col];
   table->current_cursor = curs;

   /* record the new virtual position ... */
   table->current_cursor_virt_row = new_virt_row;
   table->current_cursor_virt_col = new_virt_col;

   /* compute some useful offsets ... */
   phys_row_origin = new_phys_row;
   phys_row_origin -= table->locators[new_phys_row][new_phys_col]->phys_row_offset;

   phys_col_origin = new_phys_col;
   phys_col_origin -= table->locators[new_phys_row][new_phys_col]->phys_col_offset;

   table->current_cursor_phys_row = phys_row_origin;
   table->current_cursor_phys_col = phys_col_origin;

   /* setting the previous traversal value to the last of a traversal chain will
    * gaurentee that first entry into a register will occur at the first cell */
   table->prev_phys_traverse_row  = phys_row_origin + curs->last_reenter_traverse_row;
   table->prev_phys_traverse_col  = phys_col_origin + curs->last_reenter_traverse_col; 

   /* update the cell values to reflect the new position */
   for (i=0; i<curs->numRows; i++) {
      for (j=0; j<curs->numCols; j++) {
         BasicCell *cell;
         
         /* change the cursor row to the active color */
         table->bg_colors[i+phys_row_origin][j+phys_col_origin] = curs->active_bg_color;

         cell = curs->cells[i][j];
         if (cell) {
            char * cell_val = table->entries[i+phys_row_origin][j+phys_col_origin];

            if (do_move_gui) {
               /* if a cell has a GUI, move that first, before setting
                * the cell value.  Otherwise, we'll end up putting the 
                * new values in the old cell locations, and that would 
                * lead to confusion of all sorts. */
               if (cell->move) {
                  (cell->move) (cell, i+phys_row_origin, j+phys_col_origin);
               }
            }

            /* OK, now copy the string value from the table at large 
             * into the cell handler. */
            if (XACC_CELL_ALLOW_SHADOW & (cell->input_output)) {
               xaccSetBasicCellValue (cell, cell_val);
               cell->changed = 0;
            }

            if (cell->use_bg_color) {
               table->bg_colors[i+phys_row_origin][j+phys_col_origin] = cell->bg_color;
            }
            if (cell->use_fg_color) {
               table->fg_colors[i+phys_row_origin][j+phys_col_origin] = cell->fg_color;
            }
         }
      }
   }

   curs->user_data = table->user_data[new_virt_row][new_virt_col];
}

/* ==================================================== */

void xaccMoveCursor (Table *table, int new_phys_row, int new_phys_col)
{
   doMoveCursor (table, new_phys_row, new_phys_col, 0);
}

/* same as above, but be sure to deal with GUI elements as well */
void xaccMoveCursorGUI (Table *table, int new_phys_row, int new_phys_col)
{
   doMoveCursor (table, new_phys_row, new_phys_col, 1);
}

/* ==================================================== */

void xaccCommitCursor (Table *table)
{
   int i,j;
   int virt_row, virt_col;
   CellBlock *curs;

   curs = table->current_cursor;
   if (!curs) return;

   virt_row = table->current_cursor_virt_row;
   virt_col = table->current_cursor_virt_col;

   /* can't commit if cursor is bad */
   if ((0 > virt_row) || (0 > virt_col)) return;
   if (virt_row >= table->num_virt_rows) return;
   if (virt_col >= table->num_virt_cols) return;

   for (i=0; i<curs->numRows; i++) {
      for (j=0; j<curs->numCols; j++) {
         BasicCell *cell;
         
         cell = curs->cells[i][j];
         if (cell) {
            int iphys = i + table->current_cursor_phys_row;
            int jphys = j + table->current_cursor_phys_col;
            if (table->entries[iphys][jphys]) {
               free (table->entries[iphys][jphys]);
            }
            table->entries[iphys][jphys] = strdup (cell->value);
            table->bg_colors[iphys][jphys] = cell->bg_color;
            table->fg_colors[iphys][jphys] = cell->fg_color;
         }
      }
   }

   table->user_data[virt_row][virt_col] = curs->user_data;
}

/* ==================================================== */
/* hack alert -- will core dump if numrows has changed, etc. */
/* hack alert -- assumes that first block is header. */
/* hack alert -- this routine is *just like* that above,
 * except that its's committing the very first cursor.
 * with cleverness we could probably eliminate this routine 
 * entirely ... */

void
xaccRefreshHeader (Table *table)
{
   int i,j;
   CellBlock *arr;

   if (!(table->entries)) return;

   /* copy header data into entries cache */
   arr = table->handlers[0][0];
   if (arr) {
      for (i=0; i<arr->numRows; i++) {
         for (j=0; j<arr->numCols; j++) {
            if (table->entries[i][j]) free (table->entries[i][j]);
            if (arr->cells[i][j]) {
               if ((arr->cells[i][j])->value) {

                  table->entries[i][j] = strdup ((arr->cells[i][j])->value);
               } else {
                  table->entries[i][j] = strdup ("");
               }
            } else {
               table->entries[i][j] = strdup ("");
            }
         }
      }
   }
}

/* ==================================================== */
/* verifyCursorPosition checks the location of the cursor 
 * with respect to a row/column position, and repositions 
 * the cursor if necessary.  This includes saving any uncomited
 * data in the old cursor, and then moving the cursor and it's
 * GUI.
 */

void
xaccVerifyCursorPosition (Table *table, int phys_row, int phys_col)
{
   int virt_row, virt_col;
   int do_commit = 0;

   if (!table) return;

   /* Someone may be trying to intentionally invalidate the cursor, 
    * in which case the physical addresses could be out of bounds.
    * For example, in order to unmap it in preparation for a reconfig. 
    * So, if the specified location is out of bounds, then
    * the cursor MUST be moved. 
    */

   if ((0 > phys_row) || (0 > phys_col)) do_commit = 1;
   if (phys_row >= table->num_phys_rows) do_commit = 1;
   if (phys_col >= table->num_phys_cols) do_commit = 1;

   /* Hmm, phys position is valid. Check the virtual position. */
   if (!do_commit) {
      virt_row = table->locators[phys_row][phys_col]->virt_row;
      virt_col = table->locators[phys_row][phys_col]->virt_col;

      if ((virt_row != table->current_cursor_virt_row) ||
          (virt_col != table->current_cursor_virt_col)) do_commit = 1;
   }

   if (do_commit) {
      /* before leaving the current virtual position,
       * commit any edits that have been accumulated 
       * in the cursor */
      xaccCommitCursor (table);
      xaccMoveCursorGUI (table, phys_row, phys_col);
   }
}

/* ==================================================== */

void * 
xaccGetUserData (Table *table, int phys_row, int phys_col)
{
   int virt_row, virt_col;

   /* check for out-of-bounds conditions */
   if ((0 > phys_row) || (0 > phys_col) ||
      (phys_row >= table->num_phys_rows) ||
      (phys_col >= table->num_phys_cols)) {
      return NULL;
   }

   virt_row = table->locators[phys_row][phys_col]->virt_row;
   virt_col = table->locators[phys_row][phys_col]->virt_col;

   return (table->user_data[virt_row][virt_col]);
}

/* ================== end of file ======================= */
