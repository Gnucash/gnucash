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
 * table-allgui.h
 *
 * FUNCTION:
 * The Table object defines the structure and the GUI required
 * to display a two-dimensional grid.  It provides several 
 * important functions:
 * -- an array of strings, one for each cell of the displayed table.
 *    These strings are kept in sync with what the user sees in 
 *    the GUI (although they may be out of sync in currently 
 *    edited row(s)).
 * -- an array of cell-block handlers.  The handlers provide
 *    the actual GUI editing infrastructure: the handlers 
 *    make sure that only allowed edits are made to a block
 *    of cells.
 * -- The "cursor", which defines the region of cells that 
 *    are currently being edited.
 * -- A lookup table that maps physical row/column addresses
 *    to the cellblock handlers that know how to handle edits
 *    to the physical, display cells.
 * -- A table of user-defined data hooks that can be associated 
 *    with each cell block.  By "user" we mean the programmer who
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
 * Copyright (c) 1998,1999,2000 Linas Vepstas
 * Copyright (c) 2000 Dave Peticolas
 */

#ifndef __TABLE_ALLGUI_H__
#define __TABLE_ALLGUI_H__

#include <glib.h>

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


/* The VirtualCell structure holds information about each virtual cell. */
typedef struct _VirtualCell VirtualCell;
struct _VirtualCell
{
  CellBlock *cellblock;       /* Array of physical cells */

  PhysicalLocation phys_loc;  /* Physical location of cell (0, 0) */

  void *user_data;            /* Used by higher-level code */
};


/* The PhysicalCell structure holds information for each physical location. */
typedef struct _PhysicalCell PhysicalCell;
struct _PhysicalCell
{
  char *entry;              /* The cell data */

  VirtualLocation virt_loc; /* Cell virtual location */

  guint32 fg_color;         /* Cell foreground ARGB */
  guint32 bg_color;         /* Cell background ARGB */
};


typedef struct _Table Table;

typedef void (*TableMoveFunc) (Table *table,
                               PhysicalLocation *new_phys_loc);

typedef void (*TableTraverseFunc) (Table *table,
                                   PhysicalLocation *new_phys_loc,
                                   gncTableTraversalDir dir);

typedef void (*TableSetHelpFunc) (Table *table,
                                  const char *help_str);

typedef void (*TableDestroyFunc) (Table *table);


/* The number of "physical" rows/cols is the number
 * of displayed one-line gui rows/cols in the table.
 * The number of physical rows can differ from the 
 * number of "virtual" rows because each virtual row 
 * consist of one or more physical rows.
 *
 * Given the location of a physical row & col, the corresponding 
 * virtual row & col can be found by looking it up in the 
 * Location member. The Location will provide the matching 
 * virtual row and column.  
 *
 * Given the location of the virtual row and column, the
 * corresponding GUI handler, and any associated user data
 * can be directly accessed.
 */
struct _Table
{
  short num_phys_rows;
  short num_phys_cols;
  short num_virt_rows;
  short num_virt_cols;

  /* The position of the current cursor in "virtual" space
   * is given by the virt_row and virt_col fields below.
   * The fields termed "phys_row" and "phys_col" would
   * be better termed phys row and column "origins", as the
   * cursor extends down and to the right from the location
   * given by the physical values. */
  CellBlock *current_cursor;

  PhysicalLocation    current_cursor_phys_loc;
  VirtualCellLocation current_cursor_virt_loc;

  /* callback that is called when the cursor is moved */
  TableMoveFunc move_cursor;

  /* callback that is called to determine traversal */
  TableTraverseFunc traverse;

  /* callback to set a help string associated with a cell */
  TableSetHelpFunc set_help;

  /* This value is initialized to NULL and never touched afterwards.
   * It can be used by higher-level code. */
  void * user_data;

  /* Determines whether the passive background
   * colors alternate between odd and even virt
   * rows, or between the first and non-first
   * physical rows within cellblocks. */
  gboolean alternate_bg_colors;


  /* private data */

  /* This is black-box stuff that higher-level code should not access */

  /* The virtual and physical cell information */
  GPtrArray *virt_cells;
  GPtrArray *phys_cells;

  /* This class implements tab-key and arrow key traversal through the
   * cells of the table. To perform this traversal, the location of
   * the "previous" cell having input focus is required. */
  PhysicalLocation prev_phys_traverse_loc;

  void * ui_data;

  TableDestroyFunc destroy;
};


/* Functions to create and destroy Tables.  */
Table     * gnc_table_new (void);
void        gnc_table_destroy (Table *);

/* These functions check the bounds of virtal and physical locations
 * in the table and return TRUE if they are out of bounds. If possible,
 * they are compiled inline. */
G_INLINE_FUNC
gboolean gnc_table_virtual_cell_out_of_bounds (Table *table,
                                               VirtualCellLocation vcell_loc);
G_INLINE_FUNC gboolean
gnc_table_virtual_cell_out_of_bounds (Table *table,
                                      VirtualCellLocation vcell_loc)
{
  if (!table)
    return TRUE;

  return ((vcell_loc.virt_row < 0) ||
          (vcell_loc.virt_row >= table->num_virt_rows) ||
          (vcell_loc.virt_col < 0) ||
          (vcell_loc.virt_col >= table->num_virt_cols));
}

G_INLINE_FUNC gboolean
gnc_table_physical_cell_out_of_bounds (Table *table,
                                       PhysicalLocation phys_loc);
G_INLINE_FUNC gboolean
gnc_table_physical_cell_out_of_bounds (Table *table,
                                       PhysicalLocation phys_loc)
{
  if (!table)
    return TRUE;

  return ((phys_loc.phys_row < 0) ||
          (phys_loc.phys_row >= table->num_phys_rows) ||
          (phys_loc.phys_col < 0) ||
          (phys_loc.phys_col >= table->num_phys_cols));
}

/* These functions return the virtual/physical cell associated with a
 *   particular virtual/physical row & column pair. If the pair is out
 *   of bounds, NULL is returned. */
VirtualCell *  gnc_table_get_virtual_cell (Table *table,
                                           VirtualCellLocation vcell_loc);

PhysicalCell * gnc_table_get_physical_cell (Table *table,
                                            PhysicalLocation phys_loc);

/* Return the virtual cell of the header */
VirtualCell *  gnc_table_get_header_cell (Table *table);

/* The gnc_table_set_size() method will resize the table to the
 *   indicated dimensions.  */
void        gnc_table_set_size (Table * table,
                                int phys_rows, int phys_cols,
                                int virt_rows, int virt_cols);

/* The gnc_table_create_cursor() method can be called whenever a
 *   reconfig of the cursor may require new gui elements to be
 *   initialized. */
void        gnc_table_create_cursor (Table *, CellBlock *);

/* indicate what handler should be used for a given virtual block */
void        gnc_table_set_cursor (Table *table, CellBlock *curs,
                                  PhysicalLocation phys_origin,
                                  VirtualCellLocation vcell_loc);

/* The gnc_table_move_cursor() method will move the cursor (but not
 *   the cursor GUI) to the indicated location. This function is
 *   useful when loading the table from the cursor: data can be loaded
 *   into the cursor, then committed to the table, all without the
 *   annoying screen flashing associated with GUI redraw. */
void        gnc_table_move_cursor (Table *table, PhysicalLocation phys_loc);

/* The gnc_table_move_cursor_gui() method will move the cursor and its
 *   GUI to the indicated location. Through a series of callbacks, all
 *   GUI elements get repositioned. */
void        gnc_table_move_cursor_gui (Table *table,
                                       PhysicalLocation phys_loc);

/* The gnc_table_commit_cursor() method will copy text in the cursor
 *   cells into the table.  This function is useful during the initial
 *   load of the table with data: the cursor can be used as an
 *   intermediary to format, fix up, and otherwise control the data,
 *   and, when ready, it is commited from the cursor into the
 *   table. */
void        gnc_table_commit_cursor (Table *table);

/* Refresh the table header.  */
void        gnc_table_refresh_header (Table *table);

/* The gnc_table_verify_cursor_position() method checks the location
 *   of the cursor with respect to a physical row/column position, and
 *   if the resulting virtual position has changed, commits the
 *   changes in the old position, and the repositions the cursor
 *   and gui to the new position. Returns true if the cursor was
 *   repositioned. */
gboolean    gnc_table_verify_cursor_position (Table *table,
                                              PhysicalLocation phys_loc);

/* The gnc_table_get_user_data_physical() method returns the user data
 *   associated with a cursor located at the given physical coords, or
 *   NULL if the coords are out of bounds. */
void *      gnc_table_get_user_data_physical (Table *table,
                                              PhysicalLocation phys_loc);

/* The gnc_table_get_user_data_virtual() method returns the user data
 *   associated with a cursor located at the given virtual coords, or
 *   NULL if the coords are out of bounds. */
void *      gnc_table_get_user_data_virtual (Table *table,
                                             VirtualCellLocation vcell_loc);

/* Find the closest valid horizontal cell. If exact_cell is true,
 *   cells that must be explicitly selected by the user (as opposed
 *   to just tabbing into), are considered valid cells. */
gboolean    gnc_table_find_valid_cell_horiz(Table *table,
                                            PhysicalLocation *phys_loc,
                                            gboolean exact_cell);


/* ==================================================== */
/* UI-specific functions */

/* Initialize the GUI from a table */
void        gnc_table_init_gui (gncUIWidget widget, void *data);

/* Refresh the current cursor gui */
void        gnc_table_refresh_current_cursor_gui (Table * table,
                                                  gboolean do_scroll);

/* Refresh the whole GUI from the table. */
void        gnc_table_refresh_gui (Table *table);


/* ==================================================== */
/* Functions used internally by table-gnome.c.
 * perhaps these should go in a table-allguiP.h */

void       gnc_table_wrap_verify_cursor_position (Table *table,
                                                  PhysicalLocation phys_loc);

gboolean   gnc_table_physical_cell_valid(Table *table,
                                         PhysicalLocation phys_loc,
                                         gboolean exact_pointer);

void       gnc_table_refresh_cursor_gui (Table * table, CellBlock *curs,
                                         PhysicalLocation phys_loc,
                                         gboolean do_scroll);


/* gnc_table_enter_update() is a utility function used to determine
 *   how the gui will respond.  If it returns NULL, then the GUI will
 *   map an editing widget onto this cell, and allow user input. If it
 *   returns non-null, then the returned value will be used as the new
 *   cell value, and an editor for the cell will not be mapped (viz,
 *   the user will be prevented from updating the cell). The function
 *   is also passed pointers to the current cursor position, start
 *   selection position, and end selection position. If the function
 *   returns NULL, then it may change any of those values and the
 *   mapped editing widget will be modified accordingly.
 *
 * Note: since this is an internal-use-only routine, if you do not 
 * like this semantic, cut&paste this code and change it to suit you. 
 * However, don't just change it, because it will break functional code. */
const char * gnc_table_enter_update(Table *table,
                                    PhysicalLocation phys_loc,
                                    int *cursor_position,
                                    int *start_selection,
                                    int *end_selection);

const char * gnc_table_leave_update(Table *table,
                                    PhysicalLocation phys_loc,
                                    const char *old_text);

const char * gnc_table_modify_update(Table *table,
                                     PhysicalLocation phys_loc,
                                     const char *oldval,
                                     const char *change,
                                     char *newval,
                                     int *cursor_position,
                                     int *start_selection,
                                     int *end_selection);

gboolean     gnc_table_direct_update(Table *table,
                                     PhysicalLocation phys_loc,
                                     const char *oldval,
                                     char **newval_ptr,
                                     int *cursor_position,
                                     int *start_selection,
                                     int *end_selection,
                                     void *gui_data);

gboolean     gnc_table_traverse_update(Table *table,
                                       PhysicalLocation phys_loc,
                                       gncTableTraversalDir dir,
                                       PhysicalLocation *dest_loc);

#endif /* __TABLE_ALLGUI_H__ */

/* ================== end of file ======================= */
