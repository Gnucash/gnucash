/********************************************************************\
 * table-allgui.h -- 2D grid table object, embeds cells for i/o     *
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

/*
 * FILE:
 * table-allgui.h
 *
 * FUNCTION:
 * The Table object defines the structure and the GUI required
 * to display a two-dimensional grid. It provides several 
 * important functions:
 * -- An array of virtual cells. These cells contain:
 *    -- the cellblock handler for that virtual cell.
 *    -- a user data pointer
 * -- Tab-traversing mechanism so that operator can tab in a
 *    predefined order between cells.
 *
 * Please see src/doc/design/gnucash-design.info for additional information.
 *
 * This implements the gui-independent parts of the table 
 * infrastructure. Additional, GUI-dependent parts are implemented
 * in table-gnome.c.
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
 * Copyright (c) 2000-2001 Dave Peticolas
 */

#ifndef TABLE_ALLGUI_H
#define TABLE_ALLGUI_H

#include <glib.h>

#include "basiccell.h"
#include "cellblock.h"
#include "gnc-common.h"
#include "gtable.h"
#include "register-common.h"
#include "table-control.h"
#include "table-layout.h"
#include "table-model.h"


/* The VirtualCell structure holds information about each virtual cell. */
typedef struct _VirtualCell VirtualCell;
struct _VirtualCell
{
  CellBlock *cellblock;  /* Array of physical cells */
  gpointer   vcell_data; /* Used by higher-level code */

  /* flags */
  unsigned int visible : 1;             /* visible in the GUI */
  unsigned int start_primary_color : 1; /* color usage flag */
};

typedef struct table Table;

typedef void (*TableRedrawHelpFunc) (Table *table);
typedef void (*TableDestroyFunc) (Table *table);

struct table
{
  TableControl *control;
  TableLayout *layout;
  TableModel *model;

  int num_virt_rows;
  int num_virt_cols;

  CellBlock *current_cursor;

  VirtualLocation current_cursor_loc;

  /* private data */

  /* The virtual cell table */
  GTable *virt_cells;

  TableRedrawHelpFunc ui_redraw_help;
  TableDestroyFunc ui_destroy;
  gpointer ui_data;
};


/* Functions to create and destroy Tables.  */
Table *     gnc_table_new (TableControl *control, TableModel *model);

void        gnc_table_save_state (Table *table);
void        gnc_table_destroy (Table *table);


/* Functions to work with current cursor */
gboolean    gnc_table_current_cursor_changed (Table *table,
                                              gboolean include_conditional);

void        gnc_table_clear_current_cursor_changes (Table *table);

void        gnc_table_save_current_cursor (Table *table, CursorBuffer *buffer);
void        gnc_table_restore_current_cursor (Table *table,
                                              CursorBuffer *buffer);


/* This function checks the given location and returns true
 * if it is out of bounds of the table. */
gboolean gnc_table_virtual_cell_out_of_bounds (Table *table,
                                               VirtualCellLocation vcell_loc);

/* This function returns the virtual cell associated with a particular
 *   virtual location. If the location is out of bounds, NULL is *
 *   returned. */
VirtualCell *  gnc_table_get_virtual_cell (Table *table,
                                           VirtualCellLocation vcell_loc);

const char *   gnc_table_get_entry (Table *table, VirtualLocation virt_loc);

const char *   gnc_table_get_label (Table *table, VirtualLocation virt_loc);

CellIOFlags    gnc_table_get_io_flags (Table *table, VirtualLocation virt_loc);

guint32        gnc_table_get_fg_color (Table *table, VirtualLocation virt_loc);

guint32        gnc_table_get_bg_color (Table *table, VirtualLocation virt_loc,
                                       gboolean *hatching);

void           gnc_table_get_borders (Table *table, VirtualLocation virt_loc,
                                      PhysicalCellBorders *borders);

CellAlignment  gnc_table_get_align (Table *table, VirtualLocation virt_loc);

gboolean       gnc_table_is_popup (Table *table, VirtualLocation virt_loc);

char *         gnc_table_get_help (Table *table);


/* Return the virtual cell of the header */
VirtualCell *  gnc_table_get_header_cell (Table *table);

/* The gnc_table_set_size() method will resize the table to the
 *   indicated dimensions.  */
void        gnc_table_set_size (Table * table, int virt_rows, int virt_cols);

/* Indicate what handler should be used for a given virtual block */
void        gnc_table_set_vcell (Table *table, CellBlock *cursor,
                                 gconstpointer vcell_data,
                                 gboolean visible,
                                 gboolean start_primary_color,
                                 VirtualCellLocation vcell_loc);

/* Set the virtual cell data for a particular location. */
void        gnc_table_set_virt_cell_data (Table *table,
                                          VirtualCellLocation vcell_loc,
                                          gconstpointer vcell_data);

/* Set the visibility flag for a particular location. */
void        gnc_table_set_virt_cell_visible (Table *table,
                                             VirtualCellLocation vcell_loc,
                                             gboolean visible);

/* Set the cellblock handler for a virtual cell. */
void        gnc_table_set_virt_cell_cursor (Table *table,
                                            VirtualCellLocation vcell_loc,
                                            CellBlock *cursor);

/* The gnc_table_move_cursor() method will move the cursor (but not
 *   the cursor GUI) to the indicated location. This function is
 *   useful when loading the table from the cursor: data can be loaded
 *   into the cursor, then committed to the table, all without the
 *   annoying screen flashing associated with GUI redraw. */
void        gnc_table_move_cursor (Table *table, VirtualLocation virt_loc);

/* The gnc_table_move_cursor_gui() method will move the cursor and its
 *   GUI to the indicated location. Through a series of callbacks, all
 *   GUI elements get repositioned. */
void        gnc_table_move_cursor_gui (Table *table, VirtualLocation virt_loc);

/* The gnc_table_verify_cursor_position() method checks the location
 *   of the cursor with respect to a virtual location position, and if
 *   the resulting virtual location has changed, repositions the
 *   cursor and gui to the new position. Returns true if the cell
 *   cursor was repositioned. */
gboolean    gnc_table_verify_cursor_position (Table *table,
                                              VirtualLocation virt_loc);

/* The gnc_table_get_vcell_data() method returns the virtual cell data
 *   associated with a cursor located at the given virtual coords, or
 *   NULL if the coords are out of bounds. */
gpointer    gnc_table_get_vcell_data (Table *table,
                                      VirtualCellLocation vcell_loc);

/* Find a close valid cell. If exact_cell is true, cells that must
 *   be explicitly selected by the user (as opposed to just tabbing
 *   into), are considered valid cells. */
gboolean    gnc_table_find_close_valid_cell (Table *table,
                                             VirtualLocation *virt_loc,
                                             gboolean exact_cell);

/* ==================================================== */
/* UI-specific functions */

/* Initialize the GUI from a table */
void        gnc_table_init_gui (gncUIWidget widget, gpointer data);

void        gnc_table_realize_gui (Table *table);

/* Refresh the current cursor gui */
void        gnc_table_refresh_current_cursor_gui (Table * table,
                                                  gboolean do_scroll);

/* Refresh the whole GUI from the table. */
void        gnc_table_refresh_gui (Table *table, gboolean do_scroll);

/* Try to show the whole range in the register. */
void        gnc_table_show_range (Table *table,
                                  VirtualCellLocation start_loc,
                                  VirtualCellLocation end_loc);

/* ==================================================== */

void         gnc_table_wrap_verify_cursor_position (Table *table,
                                                    VirtualLocation virt_loc);

gboolean     gnc_table_virtual_loc_valid(Table *table,
                                         VirtualLocation virt_loc,
                                         gboolean exact_pointer);

void         gnc_table_refresh_cursor_gui (Table * table,
                                           VirtualCellLocation vcell_loc,
                                           gboolean do_scroll);

gboolean     gnc_table_move_tab (Table *table,
                                 VirtualLocation *virt_loc,
                                 gboolean move_right);

gboolean     gnc_table_move_vertical_position (Table *table,
                                               VirtualLocation *virt_loc,
                                               int phys_row_offset);

gboolean     gnc_table_enter_update(Table *table,
                                    VirtualLocation virt_loc,
                                    int *cursor_position,
                                    int *start_selection,
                                    int *end_selection);

void         gnc_table_leave_update(Table *table, VirtualLocation virt_loc);

gboolean     gnc_table_confirm_change(Table *table, VirtualLocation virt_loc);

const char * gnc_table_modify_update(Table *table,
                                     VirtualLocation virt_loc,
                                     const GdkWChar *change,
                                     int change_len,
                                     const GdkWChar *newval,
                                     int newval_len,
                                     int *cursor_position,
                                     int *start_selection,
                                     int *end_selection,
                                     gboolean *cancelled);

gboolean     gnc_table_direct_update(Table *table,
                                     VirtualLocation virt_loc,
                                     char **newval_ptr,
                                     int *cursor_position,
                                     int *start_selection,
                                     int *end_selection,
                                     gpointer gui_data);

gboolean     gnc_table_traverse_update(Table *table,
                                       VirtualLocation virt_loc,
                                       gncTableTraversalDir dir,
                                       VirtualLocation *dest_loc);

#endif /* TABLE_ALLGUI_H */
