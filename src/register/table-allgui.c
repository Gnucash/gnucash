/*
 * FILE:
 * table-allgui.c
 *
 * FUNCTION:
 * Implements the gui-independent parts of the table infrastructure.
 *
 * HISTORY:
 * Copyright (c) 1998,1999 Linas Vepstas
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
#include <unistd.h>

#include "util.h"
#include "cellblock.h"
#include "table-allgui.h"

#include "util.h"

#ifdef KDE
#define TRUE (1==1)
#endif

static void xaccFreeTableEntries (Table * table);
static void xaccTableResize (Table * table,
                 int new_phys_rows, int new_phys_cols,
                 int new_virt_rows, int new_virt_cols);

/* This static indicates the debugging module that this .o belongs to. */
static short module = MOD_REGISTER;

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
   table->traverse = NULL;
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

   table->reverify_phys_row = -1;
   table->reverify_phys_col = -1;

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
      FATAL ("xaccTableResize(): the number of physical rows (%d %d)"
             "must equal or exceed the number of virtual rows (%d %d)\n",
             new_phys_rows, new_phys_cols,
             new_virt_rows, new_virt_cols);
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
makePassive (Table *table)
{
   int i,j;
   CellBlock *curs;
   int phys_row = table->current_cursor_phys_row;
   int phys_col = table->current_cursor_phys_col;
   int r_origin, c_origin;


   /* Change the cell background colors to thier "passive" values.
    * This denotes that the cursor has left this location (which means more or
    * less the same thing as "the current location is no longer being edited.")
    * (But only do this if the cursor has a valid current location) 
    */
   if ((0 > phys_row) || (0 > phys_col)) return;

   r_origin = phys_row;
   c_origin = phys_col;
   r_origin -= table->locators[phys_row][phys_col]->phys_row_offset;
   c_origin -= table->locators[phys_row][phys_col]->phys_col_offset;

   curs = table->current_cursor;

   for (i=0; i<curs->numRows; i++) {
      for (j=0; j<curs->numCols; j++) {
         BasicCell *cell;
      
/* yooooo hack alert -- the color capabilities for the cursor should
 * be per-cell, not per cursor; so we do a quickie hack ughhh.
 * first line is whatever was speced, the second line is white.
 */
if (0==i) {
         table->bg_colors[i+r_origin][j+c_origin] = curs->passive_bg_color;
} else {
table->bg_colors[i+r_origin][j+c_origin] = 0xffffff;
}

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


/* ==================================================== */

static void 
doMoveCursor (Table *table, int new_phys_row, int new_phys_col, int do_move_gui)
{
   int i,j;
   int phys_row_origin, phys_col_origin;
   int new_virt_row, new_virt_col;
   CellBlock *curs;

   ENTER("doMoveCursor(): new_phys=(%d %d) do_move_gui=%d\n", 
       new_phys_row, new_phys_col, do_move_gui);
   /* Change the cell background colors to thier "passive" values.
    * This denotes that the cursor has left this location (which means more or
    * less the same thing as "the current location is no longer being edited.")
    */
   makePassive (table);

   /* call the callback, allowing the app to commit any changes 
    * associated with the current location of the cursor.  
    * Note that this callback may recursively call this routine. */
   if (table->move_cursor) {
      (table->move_cursor) (table, &new_phys_row, &new_phys_col, 
                            table->client_data);

      /* The above callback can cause this routine to be called recursively.
       * As a result of this recursion, the cursor may have gotten repositioned. 
       * we need to make sure we make passive again.
       */
      makePassive (table);
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
      LEAVE("doMoveCursor(): out of bounds\n");
      return;
   }

   if (new_virt_row >= table->num_virt_rows) return;
   if (new_virt_col >= table->num_virt_cols) return;
   if (new_phys_row >= table->num_phys_rows) return;
   if (new_phys_col >= table->num_phys_cols) return;

   /* ok, we now have a valid position.  Find the new cursor to use,
    * and initialize its cells */
   curs = table->handlers[new_virt_row][new_virt_col];
   table->current_cursor = curs;

   /* record the new position ... */
   table->current_cursor_virt_row = new_virt_row;
   table->current_cursor_virt_col = new_virt_col;
   table->current_cursor_phys_row = new_phys_row;
   table->current_cursor_phys_col = new_phys_col;

   /* compute some useful offsets ... */
   phys_row_origin = new_phys_row;
   phys_row_origin -= table->locators[new_phys_row][new_phys_col]->phys_row_offset;

   phys_col_origin = new_phys_col;
   phys_col_origin -= table->locators[new_phys_row][new_phys_col]->phys_col_offset;

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
   LEAVE("doMoveCursor(): did move\n");
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
   int phys_row, phys_col;
   int phys_row_origin, phys_col_origin;

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
   phys_row_origin = table->current_cursor_phys_row;
   phys_col_origin = table->current_cursor_phys_col;
   phys_row_origin -= table->locators[phys_row][phys_col]->phys_row_offset;
   phys_col_origin -= table->locators[phys_row][phys_col]->phys_col_offset;

   for (i=0; i<curs->numRows; i++) {
      for (j=0; j<curs->numCols; j++) {
         BasicCell *cell;
         
         cell = curs->cells[i][j];
         if (cell) {
            int iphys = i + phys_row_origin;
            int jphys = j + phys_col_origin;
            /*PINFO ("xaccCommitCursor(): rowcol (%d,%d) oldval=%s newval=%s\n",
              iphys, jphys, table->entries[iphys][jphys], cell->value);*/
            if (table->entries[iphys][jphys]) {
               free (table->entries[iphys][jphys]);
            }
            table->entries[iphys][jphys] = strdup (cell->value);
            if (cell->use_bg_color) {
               table->bg_colors[iphys][jphys] = cell->bg_color;
            }
            if (cell->use_fg_color) {
               table->fg_colors[iphys][jphys] = cell->fg_color;
            }
         }
      }
   }

   table->user_data[virt_row][virt_col] = curs->user_data;
}

/* ==================================================== */
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
 * data in the old cursor, and then moving the cursor and its
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
   } else {

      /* The request might be to move to a cell that is one column over.
       * If so, then do_commit will be zero, as there will have been no 
       * reason to actually move a cursor.  However, we want to keep 
       * positions accurate, so record the new location.  (The move may 
       * may also be one row up or down, which, for a two-row cursor,
       * also might not require a cursor movement).
       */
      table->current_cursor_phys_row = phys_row;
      table->current_cursor_phys_col = phys_col;
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

/* ==================================================== */
/* if any of the cells have GUI specific components that need 
 * initialization, initialize them now. The realize() callback
 * on the cursor cell is how we inform the cell handler that 
 * now is the time to initialize its GUI.  */

void        
xaccCreateCursor (Table * table, CellBlock *curs) { 
  int i,j;
  if (!curs || !table) return;  
  if (!table->table_widget) return;
  
  for (i=0; i<curs->numRows; i++) {
    for (j=0; j<curs->numCols; j++) {
      BasicCell *cell;
      cell = curs->cells[i][j];
      if (cell) {
        void (*realize) (BasicCell *, void *gui, int pixel_width);
        realize = cell->realize;
        if (realize) {
          realize (cell,
                   ((void *) table->table_widget),
                   gnc_table_column_width(table, j));
        }
      }
    }
  }
}

/* ==================================================== */

void 
wrapVerifyCursorPosition (Table *table, int row, int col)
{
   CellBlock *save_curs = table->current_cursor;
   const int save_phys_row = table->current_cursor_phys_row;
   const int save_phys_col = table->current_cursor_phys_col;

   ENTER("wrapVerifyCursorPosition(): (%d %d) val=%s\n", 
         row,col, table->entries[row][col]);
   /* VerifyCursor will do all sorts of gui-independent machinations */
   xaccVerifyCursorPosition (table, row, col);

   if ((save_phys_row != table->current_cursor_phys_row) ||
       (save_phys_col != table->current_cursor_phys_col))
   {
      /* make sure *both* the old and the new cursor rows get redrawn */
      xaccRefreshCursorGUI (table);
      doRefreshCursorGUI (table, save_curs, save_phys_row, save_phys_col);
   }
   LEAVE ("wrapVerifyCursorPosition()\n");
}

/* ==================================================== */

void        
xaccRefreshCursorGUI (Table * table)
{
   doRefreshCursorGUI (table, table->current_cursor,
      table->current_cursor_phys_row,
      table->current_cursor_phys_col);
}

/* ==================================================== */

int
gnc_register_cell_valid(Table *table, int row, int col) 
{
  int invalid = 0;
  int rel_row, rel_col;
  CellBlock *arr, *header;
  
  /* can't edit outside of the physical space */
  invalid = (0 > row) || (0 > col) ;
  invalid = invalid || (row >= table->num_phys_rows);
  invalid = invalid || (col >= table->num_phys_cols);
  
  /* gtksheet may call cell_changed after table has been destroyed.
     Are we papering over a real bug here?
     This is probably a good check anyway. */
  invalid = invalid || (table->handlers == NULL);

  if(invalid) return(invalid);

  /* header rows cannot be modified */
  /* hack alert -- assumes that header is first cell */
  /* if 0,0 is not a headr  row, then trouble ... */
  header = table->handlers[0][0];
  invalid = invalid || (row < header->numRows);

  /* GNOME needs this.  Trying to find out why... */
  /* if(invalid) return(invalid); */
    
  /* compute the cell location */
  rel_row = table->locators[row][col]->phys_row_offset;
  rel_col = table->locators[row][col]->phys_col_offset;
  
  /* verify that cursor offsets are valid.  This may occur if
   * the app that is using the table has a paritally initialized
   * cursor. (probably due to a prograing error, but maybe they
   * meant to do this). */
  invalid = invalid || (0 > rel_row);
  invalid = invalid || (0 > rel_col);
  
  /* check for a cell handler, but only if cell adress is valid */
  arr = table->current_cursor;
  if (arr && !invalid) {
    if (! (arr->cells[rel_row][rel_col])) {
      invalid = TRUE;
    } else {
      /* if cell is marked as output-only,
       * then don't call callbacks */
      if (0 == (XACC_CELL_ALLOW_INPUT &
                ((arr->cells[rel_row][rel_col])->input_output))) 
        {
          invalid = TRUE;
        }
    }
    
  } else {
    invalid = TRUE;
  }
  return(!invalid);
}

/* ========================================================================
   Handle the non gui-specific parts of a cell enter callback
*/

const char *
gnc_table_enter_update(Table *table,
                       int row, int col)
{
  /* If text should be changed, then new_text will be set to non-null
     on return */

  CellBlock *arr = table->current_cursor;
  const int rel_row = table->locators[row][col]->phys_row_offset;
  const int rel_col = table->locators[row][col]->phys_col_offset;
  char *retval = NULL;
  
  const char * (*enter) (BasicCell *, const char *);
  
  ENTER("gnc_table_enter_update(): "
        "enter %d %d (relrow=%d relcol=%d) cell=%p val=%s\n", 
         row, col, rel_row, rel_col, 
         arr->cells[rel_row][rel_col], table->entries[row][col]);
  
  
  /* OK, if there is a callback for this cell, call it */
  enter = arr->cells[rel_row][rel_col]->enter_cell;


  if (enter) {
    const char *val;

    DEBUG("gnc_table_enter_update(): %d %d has enter handler\n", rel_row, rel_col);
    
    val = table->entries[row][col];
    retval = (char *) enter(arr->cells[rel_row][rel_col], val);

    /* enter() might return null, or it might return a pointer to
     * val, or it might return a new pointer (to newly malloc memory). 
     * Replace the old pointer with a new one only if the new one is 
     * different, freeing the old one.  (Doing a strcmp would leak memory). 
     */
    if (retval && (val != retval)) {
      if (table->entries[row][col]) free (table->entries[row][col]);
      table->entries[row][col] = retval;
      (arr->cells[rel_row][rel_col])->changed = 0xffffffff;
    } else {
       retval = NULL;
    }
  }
  
  /* record this position as the cell that will be
   * traversed out of if a traverse even happens */
  table->prev_phys_traverse_row = row;
  table->prev_phys_traverse_col = col;

  LEAVE("gnc_table_enter_update(): return %s\n", retval);
  return retval;
}

/* ==================================================== */

const char *
gnc_table_leave_update(Table *table, int row, int col,
                       const char* callback_text)
{
  CellBlock *arr = table->current_cursor;
  const int rel_row = table->locators[row][col]->phys_row_offset;
  const int rel_col = table->locators[row][col]->phys_col_offset;
  const char * (*leave) (BasicCell *, const char *);
  const char *retval = NULL;
  
  ENTER("gnc_table_leave_update(): proposed (%d %d) rel(%d %d) \"%s\"\n",
        row, col, rel_row, rel_col, callback_text);

  if (!callback_text) callback_text = "";

  /* OK, if there is a callback for this cell, call it */
  leave = arr->cells[rel_row][rel_col]->leave_cell;
  if (leave) {
    retval = leave(arr->cells[rel_row][rel_col], callback_text);
    
    /* leave() might return null, or it might return a pointer to
     * callback_text, or it might return a new pointer (to newly 
     * malloced memory). 
     */
    if (retval == callback_text) {
       retval = NULL;
    }
  }

  if (!retval) retval = strdup (callback_text);
  
  /* save whatever was returned; but lets check for  
   * changes to avoid roiling the cells too much */
  if (table->entries[row][col]) {
    if (strcmp (table->entries[row][col], retval)) {
      free (table->entries[row][col]);
      table->entries[row][col] = (char *) retval;
      (arr->cells[rel_row][rel_col])->changed = 0xffffffff;
    } else {
      /* leave() allocated memory, which we will not be using ... */
      free ((char *) retval);
      retval = NULL;
    }
  } else {
    table->entries[row][col] = (char *) retval;
    (arr->cells[rel_row][rel_col])->changed = 0xffffffff;
  }
  
  /* Do the verify last, which in general asumes that cell handlers
     are up to date (i.e. the leave has been processed.) */

  wrapVerifyCursorPosition (table,
                            table->reverify_phys_row,
                            table->reverify_phys_col);

  /* return the result of the final decisionmaking */
  if (strcmp (table->entries[row][col], callback_text)) {
     retval =  table->entries[row][col];
  } else {
     retval = NULL;
  }
  LEAVE("gnc_table_leave_update(): return %s\n", retval);
  return retval;
}

/* ==================================================== */

const char *
gnc_table_modify_update(Table *table, int row, int col,
                        const char *oldval,
                        const char *change,
                        char *newval) 
{
  /* returned result should not be touched by the caller */
  /* NULL return value means the edit was rejected */

  CellBlock *arr = table->current_cursor;
  
  /* compute the cell location */
  const int rel_row = table->locators[row][col]->phys_row_offset;
  const int rel_col = table->locators[row][col]->phys_col_offset;

  const char * (*mv) (BasicCell *, const char *, const char *, const char *);
  const char *retval = NULL;
  ENTER ("gnc_table_modify_update()\n");
  
  /* OK, if there is a callback for this cell, call it */
  mv = arr->cells[rel_row][rel_col]->modify_verify;
  if (mv) {
    retval = (*mv) (arr->cells[rel_row][rel_col], oldval, change, newval);

    /* if the callback returned a non-null value, allow the edit */
    if (retval) {
      /* update data. bounds check done earlier */
      if (table->entries[row][col]) free (table->entries[row][col]);
      table->entries[row][col] = (char *) retval;
      (arr->cells[rel_row][rel_col])->changed = 0xffffffff;
    }
  } else {
    /* update data. bounds check done earlier */
    if (table->entries[row][col]) free (table->entries[row][col]);
    table->entries[row][col] = newval;
    (arr->cells[rel_row][rel_col])->changed = 0xffffffff;
  }
  LEAVE ("gnc_table_modify_update(): "
         "change %d %d (relrow=%d relcol=%d) cell=%p val=%s\n", 
         row, col, rel_row, rel_col, 
         arr->cells[rel_row][rel_col], table->entries[row][col]);
  
  return(retval);
}

/* ==================================================== */

gncBoolean
gnc_table_traverse_update(Table *table, int row, int col,
                          gncTableTraversalDir dir,
                          int *dest_row,
                          int *dest_col) 
{
  gncBoolean exit_register = FALSE;
  CellBlock *arr = table->current_cursor;
  
  ENTER("gnc_table_traverse_update(): proposed (%d %d %s) -> (%d %d %s)\n",
        row, col, table->entries[row][col],
        *dest_row, *dest_col, table->entries[*dest_row][*dest_col]);

  /* process forward-moving traversals */
  switch(dir) {
    case GNC_TABLE_TRAVERSE_RIGHT:
      /* Don't do a thing unless we verify that the row and column
       * are in bounds. Ordinarily, they are always in bounds, except 
       * in an unusual, arguably buggy situation: If the table has 
       * been recently resized smaller, then the Xbae code might report
       * a traverse out of a cell that was in the larger array, but not
       * in the smaller array.  This is probably an Xbae bug. It 
       * will core dump array access.
       */
      if ((row >= table->num_phys_rows) || 
          (col >= table->num_phys_cols)) {
        
        assert (0);
        table->prev_phys_traverse_row = *dest_row;
        table->prev_phys_traverse_col = *dest_col;
        LEAVE("gnc_table_traverse_update(): out of bounds\n");
        return FALSE;
      }
      
      {
        /* cannot compute the cell location until we have checked that
         * row and colu have valid values.  compute the cell location
         * */
        const int rel_row = table->locators[row][col]->phys_row_offset;
        const int rel_col = table->locators[row][col]->phys_col_offset;
        
        int next_row = arr->right_traverse_r[rel_row][rel_col];
        int next_col = arr->right_traverse_c[rel_row][rel_col];
        
        /* if we are at the end of the traversal chain, hop out of this
         * tab group, and into the next.  */
        if ((0 > next_row) || (0 > next_col)) {
          /* reverse the sign of next_row, col to be positive. */
          *dest_row = row - rel_row - next_row - 1; 
          *dest_col = col - rel_col - next_col - 1;
          exit_register = TRUE;
        } else {
          *dest_row = row - rel_row + next_row; 
          *dest_col = col - rel_col + next_col;
        }
      }
      break;
  
    default:
      /* FIXME: Right now we don't handle anything but forward
         traversals, but in the future, here's how it should go:

         by default, we *accept* the proposed traversal.
         
         if its a left moving tab, you should move left.
         if its an upwards moving tab you should move up, etc.
         if its a mouse pointer, we should allow user to go to that cell.
         all we are doing here is *not* overriding the proposed values.
         
         for right moving tab, we over-rode the proposed values to
         make tabbing prettier. but some day we may want to make
         moving up and down prettier too,.  */

      break;
  }
  
  /* OK, now we do a fancy trick to get the auto-expanding registers
     to work right.  The trick is that as one transaction is expanded
     or collapsed, the rows all get renumbered.  Now, we can't tell
     the code to directly hope to the new renumbered position, because
     we haven't completed all of our work yet.  In particular, we
     haven't left the current cell, and this means we haven't yet
     saved those cell contents.  Only after saving, can we reconfigure
     the table.  And only after we reconfigure the table can we move
     the since only then will we know where & how to move it to.  Sooo
     ...  here's what we do. We compute where we should have hopped to
     in the reconfigured table, and save that off in the "reverify"
     fields.  Then we hop to the boring old place we would have hopped
     to if there had been no reconfiguring going on.  Later on, after
     we've reconfigured, we will move the cursor to the "reverify"
     position, and viola, we'll be in the right place.  */
  
  if (table->traverse) {
    int nr = *dest_row;
    int nc = *dest_col;
    table->reverify_phys_row = nr;
    table->reverify_phys_col = nc;
    (table->traverse) (table, &nr, &nc, table->client_data);
    *dest_row = nr;
    *dest_col = nc;
  }
  
  table->prev_phys_traverse_row = *dest_row;
  table->prev_phys_traverse_col = *dest_col;

  LEAVE("gnc_table_traverse_update(): exit_register=%d\n", exit_register);
  return(exit_register);
}

/* ================== end of file ======================= */
