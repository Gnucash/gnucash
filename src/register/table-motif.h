/*
 * FILE:
 * table-motif.h
 *
 * FUNCTION:
 * The table is the complete, displayed table. 
 * It consists of a header, followed by a simple 
 * list of repeated entries.
 *
 * It provides the mechanism to handle tab-trversing.
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


#ifndef __XACC_TABLE_H__
#define __XACC_TABLE_H__

#include <Xm/Xm.h>
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

typedef struct _Table {

  /* The number of "physical" rows/cols is the number
   * of displayed one-line gui rows/cols in the table.
   * The number of physical rows can differ from the 
   * number of "virtual" rows because each virtual row 
   * consist of one or more physical rows.
   */

  int num_phys_rows;
  int num_phys_cols;
  int num_virt_rows;
  int num_virt_cols;

  CellBlock *header;

  CellBlock *current_cursor;
  int current_cursor_row;
  int current_cursor_col;

  /* callback that is called when the cursor is moved */
  /* hack alert -- this should be a callback list, actually */
  void (*move_cursor) (struct _Table *, void *client_data);
  void * client_data;

  /* string values for each cell, 
   * of dimension num_phys_rows * num_phys_cols */
  char ***entries;

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
   
  /* Motif-only date below, gui-independent data above */

  /* protected data -- vital for the implementation, 
   * but not something we want to generally expose */
  Widget table_widget;          /* the XbaeMatrix */
  Widget next_tab_group;        /* where to traverse in the end */

} Table;


Table     * xaccMallocTable (void);
void        xaccInitTable (Table *);

/* rsize the table to the indicated dimensions.
 * calls the gui-independent xaccTableResize() routine,
 * and then does some motif-specific cleanup.
 */
void        xaccSetTableSize (Table * table, int phys_rows, int phys_cols,
                                             int virt_rows, int virt_cols);

/* create the widget */
Widget      xaccCreateTable (Table *, Widget parent, char * name);
void        xaccNextTabGroup (Table *, Widget);

void        xaccDestroyTable (Table *);

/* redraw the table GUI */
void        xaccRefreshTableGUI (Table *);

/* copy text in the cursor cells to the table */
void        xaccCommitCursor (Table *);

#endif __XACC_TABLE_H__
/* ================== end of file ======================= */
