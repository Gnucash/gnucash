/*
 * FILE:
 * table.h
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

#ifndef __XACC_TABLE_H__
#define __XACC_TABLE_H__

#include <Xm/Xm.h>
#include "basiccell.h"
#include "cellblock.h"

typedef struct _Table {

  /* num rows and cols are the number of times the 
   * cursor can tile the table in the vertical and 
   * horizontal directions */
  int num_rows;
  int num_cols;

  CellBlock *header;
  CellBlock *cursor;

  int current_cursor_row;
  int current_cursor_col;

  char ***entries;

  /* protected data -- vital for the implementation, 
   * but not something we want to generally expose */
  Widget table_widget;          /* the XbaeMatrix */
  Widget next_tab_group;        /* where to traverse in the end */

  /* private data, caches, etc. */
  /* This is black-box stuff that no user of this class 
   * should ever want to access */

  /* the "tile size" is the number of rows & 
   * cols in the cursor */
  int tile_width;
  int tile_height;

  /* the "physical" rows/cols are equal to
   * the size of the tile times the number 
   * of tile rows/cols
   */

  int num_header_rows;
  int num_phys_rows;
  int num_phys_cols;

  /* This class implements tab-key and arrow key 
   * traversal through the cells of the table.  
   * To perform this traversal, the location
   * of the "previous" cell having input focus 
   * is required.
   */
  int prev_phys_traverse_row;
  int prev_phys_traverse_col;
   
} Table;


Table     * xaccMallocTable (int tile_rows, int tile_cols);
void        xaccInitTable (Table *, int tile_rows, int tile_cols);

/* create the widget */
Widget      xaccCreateTable (Table *, Widget parent, char * name);
void        xaccNextTabGroup (Table *, Widget);

void        xaccDestroyTable (Table *);

/* redraw the table */
void        xaccRefreshTable (Table *);

/* Make the indicated cell block be the cursor for this table */
void        xaccSetCursor (Table *, CellBlock *);

/* move the cursor (but not the GUI) to the indicated location. */
void        xaccMoveCursor (Table *, int virt_row, int virt_col);

/* move the cursor GUI to the indicated location. */
void        xaccMoveCursorGUI (Table *, int virt_row, int virt_col);

/* copy text in the cursor cells to the table */
void        xaccCommitEdits (Table *);

#endif __XACC_TABLE_H__
/* ================== end of file ======================= */
