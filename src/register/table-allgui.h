/*
 * FILE:
 * table-allgui.h
 *
 * FUNCTION:
 * The Table object defines the structure and the GUI required
 * to display a two-dimensional grid.  It provides several 
 * important functions:
 * -- an array of strings, one for each cell of the displayed table.
 *    These strings are kept in sync with what the user sees in 
 *    the GUI (although they may be out of sync in  currently 
 *    edited row(s)).
 * -- an array of cell-block handlers.  The handlers provide
 *    the actual GUI editing infrastructure: the handlers 
 *    ake sure that only allowed edits are made to a block
 *    of cells.
 * -- The "cursor", which defines the region of cells that 
 *    are currently being edited.
 * -- A lookup table that maps physical row/column addresses
 *    to the cellblock handlers that know how to handle edits
 *    to the physical, display cells.
 * -- A table of user-defined data hooks that can be associated 
 *    with each cell block.  By "user" we ean the prograer who
 *    makes use of this object.
 * -- Tab-traversing mechanism so that operator can tab in a
 *    predefined order between cells.
 *
 * Please see the file "design.txt" for additional information.
 *
 * This implements the gui-independent parts of the table 
 * infrastructure.  Additional, GUI-dependent parts are implemented
 * in table-gtk.c and table-motif.c
 *
 * CONCEPTS:
 * The following apply to the rows in a table:
 * -- a phys row can belong to only one virt row at a time.
 * -- a cursor is always the same size as the virt row its on,
 * -- there is only one cursor for a given virt row.
 * -- there is no overlap; a phys row can only belong to one virt row.
 *
 * Lets say there are three cursors T(rans),S(plit), and B(lank).  
 * Lets say that these are used to 'print' the following table layout:
 * 
 *       virt row 1   T
 *       virt row 2   S
 *       virt row 3   B
 *       virt row 4   T
 *       virt row 5   S
 *       virt row 6   S
 *       virt row 7   S
 *       virt row 8   S
 *       virt row 9   B
 *
 * You can see there is only one cursor per virt row.  There is no overlap
 * between cursors and virt rows, the correspondence is one to one.  Note
 * that the three cursors T,S and B may consist of one, or more phys rows,
 * e.g. B and S may be one line each, but T may be two lines.  Thus, we 
 * have the following physical layout:
 *
 *      phys row 1    virt row 1   T
 *      phys row 2    virt row 1   T
 *      phys row 3    virt row 2   S
 *      phys row 4    virt row 3   B
 *      phys row 5    virt row 4   T
 *      phys row 6    virt row 4   T
 *      phys row 7    virt row 5   S
 *      phys row 8    virt row 6   S
 *      phys row 9    virt row 7   S
 *      phys row 10   virt row 8   S
 *      phys row 11   virt row 9   B
 *
 * This layout remains static until the next time that the table is 
 * re-'printed'.
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

#ifndef __XACC_TABLE_ALLGUI_H__
#define __XACC_TABLE_ALLGUI_H__

#ifdef MOTIF
#include "table-motif.h"
#endif 

#ifdef GNOME
#include "table-gnome.h"
#endif 

#ifdef KDE
#include "table-qt.h"
#endif 

#include "gnc-common.h"

#include "basiccell.h"
#include "cellblock.h"

typedef enum {
  GNC_TABLE_TRAVERSE_POINTER,
  GNC_TABLE_TRAVERSE_LEFT,
  GNC_TABLE_TRAVERSE_RIGHT,
  GNC_TABLE_TRAVERSE_UP,
  GNC_TABLE_TRAVERSE_DOWN
} gncTableTraversalDir;

/* the Locator structure is used provide a mapping from
 * the physical array of cells to the logical array of 
 * virtual cell blocks.
 *
 * There is one instance of Locator for each physical cell.
 * The virt_row and virt_col members identify the corresponding
 * cellblock/virtual cell that this physical cell is a member of.
 * The two phys_offsets provide the location of the physical cell
 * as an offset from the cell block origin.  That is, the offsets
 * should never be less than zero, or greater than the size of
 * the cell block.
 */
struct _Locator {
  short phys_row_offset;
  short phys_col_offset;
  short virt_row;
  short virt_col;
};

typedef struct _Locator Locator;

/*  The RevLocator gives a reverse mapping from a virtual
 *  cell block to the origin of the block in physical coordinates.
 *
 *  There is one instance of a RevLocator for each virtual cell.
 */

struct _RevLocator {
  short phys_row;
  short phys_col;
};

typedef struct _RevLocator RevLocator;


/* The number of "physical" rows/cols is the number
 * of displayed one-line gui rows/cols in the table.
 * The number of physical rows can differ from the 
 * number of "virtual" rows because each virtual row 
 * consist of one or more physical rows.
 *
 * Given the location of a physical row & col, the corresponding 
 * virtual row & col can be found by looking it up in the 
 * "locators" member.  The locator will provide the matching 
 * virtual row and column.  
 *
 * Given the location of the virtual row and column, the
 * corresponding GUI handler, and any associated user data can 
 * be directly accessed.
 */

struct _Table {

  int num_phys_rows;
  int num_phys_cols;
  int num_virt_rows;
  int num_virt_cols;

  /* The position of the current cursor in "virtual" space
   * is given by the virt_row and virt_col fields below.
   * The fields termed "phys_row" and "phys_col" would
   * be better termed phys row and column "origins", as the
   * cursor extends down and to the right from the location
   * given by the phys values.
   */
  CellBlock *current_cursor;
  int current_cursor_phys_row;
  int current_cursor_phys_col;
  int current_cursor_virt_row;
  int current_cursor_virt_col;

  /* callback that is called when the cursor is moved */
  /* hack alert -- this should be a callback list, actually */
  void (*move_cursor) (Table *, int *p_new_phys_row, 
                                int *p_new_phys_col, 
                                void *client_data);

  /* callback that is called to determine traversal */
  void  (*traverse)  (Table *,  int *p_new_phys_row, 
                                int *p_new_phys_col, 
                                void *client_data);

  void * client_data;

  /* string values for each cell, 
   * of dimension num_phys_rows * num_phys_cols */
  char ***entries;

  /* background colors for each cell, format ARGB, 
   * and foreground (text) colors, format ARGB,
   * of dimension num_phys_rows * num_phys_cols */
  uint32 **bg_colors;
  uint32 **fg_colors;

  /* handler locators for each cell, 
   * of dimension num_phys_rows * num_phys_cols */
  Locator ***locators;

  /* reverse locators for each cell,
     of dimension num_virt_rows * num_virt_cols */
  RevLocator ***rev_locators;

  /* user hooks, of dimension num_virt_rows * num_virt_cols */
  void ***user_data;

  /* cell blocks, of dimension num_virt_rows * num_virt_cols */
  CellBlock ***handlers;

  /* private data, caches, etc. */
  /* This is black-box stuff that no user of this class 
   * should ever want to access */

  /* This class implements tab-key and arrow key 
   * traversal through the cells of the table.  
   * To perform this traversal, the location
   * of the "previous" cell having input focus 
   * is required.
   */
  int prev_phys_traverse_row;
  int prev_phys_traverse_col;

  /* Since we are using C not C++, but we need inheritance, 
   * cock it up with a #defined thingy that the "derived class" 
   * can specify.
   */
  TABLE_PRIVATE_DATA

};


Table     * xaccMallocTable (void);
void        xaccInitTable (Table *);
void        xaccDestroyTable (Table *);


/* The xaccSetTableSize() method will resize the table to the 
 * indicated dimensions. 
 */
void        xaccSetTableSize (Table * table, int phys_rows, int phys_cols,
                                             int virt_rows, int virt_cols);

/* The xaccCreateCursor() method can be called whenever a reconfig
 *    of the cursor may require new gui elements to be initialized.
 */
void        xaccCreateCursor (Table *, CellBlock *);

/* indicate what handler should be used for a given virtual block */
void 
xaccSetCursor (Table *table, CellBlock *curs,
              int phys_row_origin, int phys_col_origin,
              int virt_row, int virt_col);


/* The xaccMoveCursor() method will move the cursor (but not the 
 *    cursor GUI) to the indicated location.  This function is useful 
 *    when loading the table from the cursor: data can be loaded into
 *    the cursor, then committed to the table, all without the annoying
 *    screen flashing associated with GUI redraw.
 */
void        xaccMoveCursor (Table *, int phys_row, int phys_col);

/* The xaccMoveCursorGUI() method will move the cursor and its GUI 
 *    to the indicated location.   Through a series of callbacks,
 *    all GUI elements get repositioned.
 */
void        xaccMoveCursorGUI (Table *, int phys_row, int phys_col);

/* The xaccCommitCursor() method will copy text in the cursor cells
 *    into the table.  This function is useful during the initial 
 *    load of the table with data: the cursor can be used as an
 *    intermediary to format, fix up, and otherwise control the data,
 *    and, when ready, it is commited from the cursor into the table.
 */
void        xaccCommitCursor (Table *);

/* hack alert --
 * for all practical purposes, RefreshHeader is identical
 * to CommitCursor(), except that it acts on cellblock 0,0.
 * it should probably be made obsolete.
 */
void        xaccRefreshHeader (Table *);


/* The xaccVerifyCursorPosition() method  checks the location of 
 *    the cursor with respect to a physical row/column position, 
 *    and if the resulting virtual position has changed, commits 
 *    the changes in the old position, and the repositions the 
 *    cursor & gui to the new position. Returns true if the
 *    cursor was repositioned.
 */

gncBoolean
xaccVerifyCursorPosition (Table *table, int phys_row, int phys_col);

/*
 * The xaccGetUserData() method is a convenience function that 
 *    simplifies the lookup of the any user data that is hooked 
 *    to the indicated row and column.  It returns NULL if the 
 *    row and column are out of bounds.
 */

void * xaccGetUserData (Table *table, int phys_row, int phys_col);


/* ==================================================== */
/* these are used internally by table-{motif,gtk}.c
   perhaps these should go in a table-allguiP.h  
*/

int
gnc_table_column_width(Table *table, int col);

void 
wrapVerifyCursorPosition (Table *table, int row, int col);

gncBoolean
gnc_register_cell_valid(Table *table, int row, int col,
                        gncBoolean exact_pointer);

void        
doRefreshCursorGUI (Table * table, CellBlock *curs,
                    int from_row, int from_col, gncBoolean do_scroll);

void        
xaccRefreshCursorGUI (Table * table, gncBoolean do_scroll);

/* 
 * gnc_table_enter_update() is a utility function used to determine
 * how the gui will respond.  If it returns NULL, then the GUI will
 * map an editing widget onto this cell, and allow user input. If 
 * it returns non-null, then the returned value will be used as the 
 * new cell value, and an editor for the cell will not be mapped
 * (viz, the user will be prevented from updating the cell)
 *
 * Note: since this is an internal-use-only routine, if you do not 
 * like this semantic, cut&paste this code and change it to suit you. 
 * However, don't just change it, because it will break functional code.
 */
const char *
gnc_table_enter_update(Table *table, int row, int col);

const char *
gnc_table_leave_update(Table *table, int row, int col,
                       const char* old_text);

const char *
gnc_table_modify_update(Table *table, int row, int col,
                        const char *oldval,
                        const char *change,
                        char *newval,
                        int *cursor_position);
gncBoolean
gnc_table_traverse_update(Table *table, int row, int col,
                          gncTableTraversalDir dir,
                          int *dest_row,
                          int *dest_col);

/* Find the closest valid horizontal cell */
gncBoolean
gnc_table_find_valid_cell_horiz(Table *table, int *row, int *col,
                                gncBoolean exact_cell);


/* ==================================================== */
/* 
 * In C, we don't have things like C++ templates. 
 * So cook up a #define that acts like a template.  
 * This one will resize a 2D array in a reasonably 
 * efficient manner.
 */


#define XACC_RESIZE_ARRAY(table_rows,table_cols,new_rows,new_cols,arr,type,null_val,free_cell_op) \
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
               free_cell_op (arr[i][j]);				\
               arr[i][j] = 0x0; /* plain null, not null_val */		\
            }								\
         }								\
      } else {								\
									\
         /* if we are here, the new table has more */			\
         /* columns. Realloc the columns.  */				\
         for (i=0; i<new_rows; i++) {					\
            type *old_row;						\
									\
            old_row = arr[i];						\
            arr[i] = (type *) malloc (new_cols * sizeof (type));	\
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
            free_cell_op (arr[i][j]);					\
         }								\
         free (arr[i]);							\
         arr[i] = NULL;							\
      }									\
									\
   } else {								\
      type **old_entries;						\
									\
      /* if we are here, there are more new than old rows */		\
      if (old_cols >= new_cols) {					\
									\
         /* new table has fewer columns. */ 				\
         /* Simply truncate the columns  */				\
         for (i=0; i<old_rows; i++) {					\
            for (j=new_cols; j<old_cols; j++) {				\
               free_cell_op (arr[i][j]);				\
               arr[i][j] = 0x0; /* plain null, not null_val */		\
            }								\
         }								\
      } else {								\
									\
         /* if we are here, the new table has more */			\
         /* columns. Realloc the columns.  */				\
         for (i=0; i<old_rows; i++) {					\
            type *old_row;						\
									\
            old_row = arr[i];						\
            arr[i] = (type *) malloc (new_cols * sizeof (type));	\
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
      arr = (type **) malloc (new_rows * sizeof (type *));		\
      for (i=0; i<old_rows; i++) {					\
         arr[i] = old_entries[i];					\
      }									\
      if (old_entries) free (old_entries);				\
									\
      for (i=old_rows; i<new_rows; i++) {				\
         arr[i] = (type *) malloc (new_cols * sizeof (type));		\
         for (j=0; j<new_cols; j++) {					\
            arr[i][j] = null_val;					\
         }								\
      }									\
   }									\
}

/* ==================================================== */

#endif /* __XACC_TABLE_ALLGUI_H__ */

/* ================== end of file ======================= */
