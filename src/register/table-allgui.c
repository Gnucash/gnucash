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
   xaccTableResize (table, phys_rows, phys_cols, virt_rows, virt_cols);

   /* invalidate the "previous" traversed cell */
   table->prev_phys_traverse_row = -1;
   table->prev_phys_traverse_col = -1;

   /* invalidate the current cursor position, if needed */
   if ((table->current_cursor_virt_row >= table->num_virt_rows) ||
       (table->current_cursor_virt_col >= table->num_virt_cols)) {
      table->current_cursor_virt_row = -1;
      table->current_cursor_virt_col = -1;
      table->current_cursor_phys_row = -1;
      table->current_cursor_phys_col = -1;
      table->current_cursor = NULL;
   }
}

/* ==================================================== */
/* in C, we don't have templates. So cook up a $define that acts like a
 * template.  This one will resize a 2D array.
 */

#define RESIZE_ARR(table_rows,table_cols,new_rows,new_cols,arr,type,null_val,do_free_cell_contents) \
{									\
   int old_rows, old_cols;						\
   int i,j;								\
									\
   /* save old table size */						\
   old_rows = table_rows;						\
   old_cols = table_cols;						\
   if (0 > old_rows) old_rows = 0;					\
   if (0 > old_cols) old_cols = 0;					\
									\
   /* realloc to get the new table size.  Note that the */		\
   /* new table may be wider or slimmer, taller or shorter. */		\
   if (old_rows >= new_rows) {						\
      if (old_cols >= new_cols) {					\
									\
         /* if we are here, new table has fewer cols */			\
         /* simply truncate columns */					\
         for (i=0; i<new_rows; i++) {					\
            for (j=new_cols; j<old_cols; j++) {				\
               if (do_free_cell_contents) free (arr[i][j]);		\
               arr[i][j] = NULL;					\
            }								\
         }								\
      } else {								\
									\
         /* if we are here, the new table has more */			\
         /* columns. Realloc the columns.  */				\
         for (i=0; i<new_rows; i++) {					\
            type **old_row;						\
									\
            old_row = arr[i];						\
            arr[i] = (type **) malloc (new_cols * sizeof (type *));	\
            for (j=0; j<old_cols; j++) {				\
               arr[i][j] = old_row[j];					\
            }								\
            for (j=old_cols; j<new_cols; j++) {				\
               arr[i][j] = null_val;					\
            }								\
            free (old_row);						\
         }								\
      }									\
									\
      /* new table has fewer rows.  Simply truncate the rows */		\
      for (i=new_rows; i<old_rows; i++) {				\
         for (j=0; j<old_cols; j++) {					\
            if (do_free_cell_contents) free (arr[i][j]);		\
         }								\
         free (arr[i]);							\
         arr[i] = NULL;							\
      }									\
									\
   } else {								\
      type ***old_entries;						\
									\
      /* if we are here, there are more new than old rows */		\
      if (old_cols >= new_cols) {					\
									\
         /* new table has fewer columns. */ 				\
         /* Simply truncate the columns  */				\
         for (i=0; i<old_rows; i++) {					\
            for (j=new_cols; j<old_cols; j++) {				\
               if (do_free_cell_contents) free (arr[i][j]);		\
               arr[i][j] = NULL;					\
            }								\
         }								\
      } else {								\
									\
         /* if we are here, the new table has more */			\
         /* columns. Realloc the columns.  */				\
         for (i=0; i<old_rows; i++) {					\
            type **old_row;						\
									\
            old_row = arr[i];						\
            arr[i] = (type **) malloc (new_cols * sizeof (type *));	\
            for (j=0; j<old_cols; j++) {				\
               arr[i][j] = old_row[j];					\
            }								\
            for (j=old_cols; j<new_cols; j++) {				\
               arr[i][j] = null_val;					\
            }								\
            free (old_row);						\
         }								\
      }									\
									\
      /* now, add all new rows */					\
      old_entries = arr;						\
      arr = (type ***) malloc (new_rows * sizeof (type **));		\
      for (i=0; i<old_rows; i++) {					\
         arr[i] = old_entries[i];					\
      }									\
      if (old_entries) free (old_entries);				\
									\
      for (i=old_rows; i<new_rows; i++) {				\
         arr[i] = (type **) malloc (new_cols * sizeof (type *));	\
         for (j=0; j<new_cols; j++) {					\
            arr[i][j] = null_val;					\
         }								\
      }									\
   }									\
}

/* ==================================================== */

void
xaccFreeTableEntries (Table * table)
{
   int i,j;
   /* free the entries and locators */
   for (i=0; i<table->num_phys_rows; i++) {
      for (j=0; j<table->num_phys_cols; j++) {
         free (table->entries[i][j]);
         table->entries[i][j] = NULL;

         free (table->locators[i][j]);
         table->locators[i][j] = NULL;
      }
      free (table->entries[i]);
      table->entries[i] = NULL;

      free (table->locators[i]);
      table->locators[i] = NULL;
   }
   free (table->entries);
   table->entries = NULL;

   free (table->locators);
   table->locators = NULL;

   /* null out user data and handlers */
   for (i=0; i<table->num_virt_rows; i++) {
      for (j=0; j<table->num_virt_cols; j++) {
         table->handlers = NULL;
         table->user_data = NULL;
      }
   }
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

void 
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
   RESIZE_ARR ((table->num_phys_rows),
               (table->num_phys_cols),
               new_phys_rows,
               new_phys_cols,
               (table->entries),
               char,
               (strdup ("")),
               1);

   /* resize the locator array */
   RESIZE_ARR ((table->num_phys_rows),
               (table->num_phys_cols),
               new_phys_rows,
               new_phys_cols,
               (table->locators),
               Locator,
               (xaccMallocLocator ()),
               1);

   /* we are done with the physical dimensions. 
    * record them for posterity. */
   table->num_phys_rows = new_phys_rows;
   table->num_phys_cols = new_phys_cols;


   /* resize the user-data hooks */
   RESIZE_ARR ((table->num_virt_rows),
               (table->num_virt_cols),
               new_virt_rows,
               new_virt_cols,
               (table->user_data),
               void,
               (NULL),
               0);

   /* resize the handler array */
   RESIZE_ARR ((table->num_virt_rows),
               (table->num_virt_cols),
               new_virt_rows,
               new_virt_cols,
               (table->handlers),
               CellBlock,
               (NULL),
               0);

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

   /* hack alert -- should check to make sure that no null pointers
    * or bad values were passed in ... */

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

void xaccMoveCursor (Table *table, int new_phys_row, int new_phys_col)
{
   int i,j;
   int phys_row_origin, phys_col_origin;
   int new_virt_row, new_virt_col;
   CellBlock *curs;

   /* call the callback, allowing the app to commit any changes */
   if (table->move_cursor) {
      (table->move_cursor) (table, table->client_data);
   }

   /* check for out-of-bounds conditions (which may be deliberate) */
   if ((0 > new_phys_row) || (0 > new_phys_col)) {
      new_virt_row = -1;
      new_virt_col = -1;
   } else {
      new_virt_row = table->locators[new_phys_row][new_phys_col]->virt_row;
      new_virt_col = table->locators[new_phys_row][new_phys_col]->virt_col;
   }

   /* invalidate the cursor for now; we'll set it the the correct values below */
   table->current_cursor_phys_row = -1;
   table->current_cursor_phys_col = -1;
   table->current_cursor_virt_row = -1;
   table->current_cursor_virt_col = -1;
   curs = table->current_cursor;
   if (curs) curs->user_data = NULL;
   table->current_cursor = NULL;

   /* check for out-of-bounds conditions (which may be deliberate) */
   if ((0 > new_virt_row) || (0 > new_virt_col)) return;
   if (new_virt_row >= table->num_virt_rows) return;
   if (new_virt_col >= table->num_virt_cols) return;

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

   /* update the cell values to reflect the new position */
   for (i=0; i<curs->numRows; i++) {
      for (j=0; j<curs->numCols; j++) {
         BasicCell *cell;
         
         cell = curs->cells[i][j];
         if (cell) {
            char * cell_val = table->entries[i+phys_row_origin][j+phys_col_origin];
            xaccSetBasicCellValue (cell, cell_val);
            cell->changed = 0;
         }
      }
   }

   curs->user_data = table->user_data[new_virt_row][new_virt_col];
}

/* ==================================================== */
/* same as above, but be sure to deal with GUI elements as well */

void xaccMoveCursorGUI (Table *table, int new_phys_row, int new_phys_col)
{
   int i,j;
   int phys_row_origin, phys_col_origin;
   int new_virt_row, new_virt_col;
   CellBlock *curs;

   /* call the callback, allowing the app to commit any changes */
   if (table->move_cursor) {
      (table->move_cursor) (table, table->client_data);
   }

   /* check for out-of-bounds conditions (which may be deliberate) */
   if ((0 > new_phys_row) || (0 > new_phys_col)) {
      new_virt_row = -1;
      new_virt_col = -1;
   } else {
      new_virt_row = table->locators[new_phys_row][new_phys_col]->virt_row;
      new_virt_col = table->locators[new_phys_row][new_phys_col]->virt_col;
   }

   curs = table->current_cursor;

   /* invalidate the cursor for now; we'll set it the the correct values below */
   table->current_cursor_phys_row = -1;
   table->current_cursor_phys_col = -1;
   table->current_cursor_virt_row = -1;
   table->current_cursor_virt_col = -1;
   if (curs) curs->user_data = NULL;
   table->current_cursor = NULL;

   /* check for out-of-bounds conditions (which may be deliberate) */
   if ((0 > new_virt_row) || (0 > new_virt_col)) {
      /* if the location is invalid, then we should take this 
       * as a command to unmap the cursor gui.  So do it .. */
      if (curs) {
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

   /* update the cell values to reflect the new position */
   for (i=0; i<curs->numRows; i++) {
      for (j=0; j<curs->numCols; j++) {
         BasicCell *cell;
         
         cell = curs->cells[i][j];
         if (cell) {
            char * cell_val = table->entries[i+phys_row_origin][j+phys_col_origin];
            /* if a cell has a GUI, move that first, before setting
             * the cell value.  Otherwise, we'll end up putting the 
             * new values in the old cell locations, and that would 
             * lead to confusion of all sorts. */
            if (cell->move) {
               (cell->move) (cell, i+phys_row_origin, j+phys_col_origin);
            }

            /* OK, now set the cell value, after the move */
            xaccSetBasicCellValue (cell, cell_val);
            cell->changed = 0;

         }
      }
   }

   curs->user_data = table->user_data[new_virt_row][new_virt_col];
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

   /* cant commit if cursor is bad */
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
 * data in the old cursor, and then oving the cursor and it's
 * GUI.
 *
 */

void
xaccVerifyCursorPosition (Table *table, int phys_row, int phys_col)
{
   int virt_row, virt_col;

   /* compute the virtual position */
   virt_row = table->locators[phys_row][phys_col]->virt_row;
   virt_col = table->locators[phys_row][phys_col]->virt_col;

   if ((virt_row != table->current_cursor_virt_row) ||
       (virt_col != table->current_cursor_virt_col)) {

      /* before leaving the current virtual position,
       * commit any edits that have been accumulated 
       * in the cursor */
      xaccCommitCursor (table);
      xaccMoveCursorGUI (table, phys_row, phys_col);
   }
}

/* ================== end of file ======================= */
