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

#ifndef __XACC_TABLE_ALLGUI_H__
#define __XACC_TABLE_ALLGUI_H__

#ifdef MOTIF
#include "table-motif.h"
#endif 

#ifdef GNOME
#include "table-gtk.h"
#endif 

#include "basiccell.h"
#include "cellblock.h"

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

  /* the current cursor row/col is the virt row/col */
  CellBlock *current_cursor;
  int current_cursor_phys_row;
  int current_cursor_phys_col;
  int current_cursor_virt_row;
  int current_cursor_virt_col;

  /* callback that is called when the cursor is moved */
  /* hack alert -- this should be a callback list, actually */
  void (*move_cursor) (Table *, void *client_data);
  void * client_data;

  /* string values for each cell, 
   * of dimension num_phys_rows * num_phys_cols */
  char ***entries;

  /* background colors for each cell, format ARGB, 
   * and foreground (text) colors, format ARGB,
   * of dimension num_phys_rows * num_phys_cols */
  int **bg_colors;
  int **fg_colors;

  /* handler locators for each cell, 
   * of dimension num_phys_rows * num_phys_cols */
  Locator ***locators;

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
  TABLE_PRIVATE_DATA;

};


Table     * xaccMallocTable (void);
void        xaccInitTable (Table *);
void        xaccDestroyTable (Table *);


/* The xaccSetTableSize() method will resize the table to the 
 * indicated dimensions. 
 */
void        xaccSetTableSize (Table * table, int phys_rows, int phys_cols,
                                             int virt_rows, int virt_cols);

/* indicate what handler should be used for a given virtual block */
void 
xaccSetCursor (Table *table, CellBlock *curs,
              int phys_row_origin, int phys_col_origin,
              int virt_row, int virt_col);


/* move the cursor (but not the GUI) to the indicated location. */
void        xaccMoveCursor (Table *, int phys_row, int phys_col);

/* move the cursor GUI to the indicated location. */
void        xaccMoveCursorGUI (Table *, int phys_row, int phys_col);

/* copy text in the cursor cells to the table */
void        xaccCommitCursor (Table *);

/* hack alert --
 * for all practical purposes, RefreshHeader is identical
 * tp CommitCursor(), except that it acts on cellblock 0,0.
 * it should probably be made obsolete.
 */
void        xaccRefreshHeader (Table *);


/* xaccVerifyCursorPosition checks the location of the cursor 
 * with respect to a physical row/column position, and if the 
 * resulting virtual position has changed, commits the changes 
 * in the old position, and the repositions the cursor & gui 
 * to the new position.
 */

void
xaccVerifyCursorPosition (Table *table, int phys_row, int phys_col);

#endif /* __XACC_TABLE_ALLGUI_H__ */

/* ================== end of file ======================= */
