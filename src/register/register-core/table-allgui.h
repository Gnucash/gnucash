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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/
/** @addtogroup GUI
 *  @{
 */
/** @addtogroup Register Registers, Ledgers and Journals
 *  @{
 */
/** @addtogroup RegisterCore Register Core
 *  @{
 */
/** @file table-allgui.h
 *  @brief Declarations for the Table object
 *  @author Copyright (c) 1998,1999,2000 Linas Vepstas
 *  @author Copyright (c) 2000-2001 Dave Peticolas
 *
 *  @details
 *  The Table object defines the structure and the GUI required
 *  to display a two-dimensional grid. It provides several 
 *  important functions:
 *  - An array of virtual cells. These cells contain:
 *     - the cellblock handler for that virtual cell.
 *     - a user data pointer
 *  - Tab-traversing mechanism so that operator can tab in a
 *     predefined order between cells.
 *
 *  Please see src/doc/design/gnucash-design.info for additional information.
 *
 *  This implements the gui-independent parts of the table 
 *  infrastructure. Additional, GUI-dependent parts are implemented
 *  in table-gnome.c.
 *
 *  The following concepts apply to the rows in a table:
 *  - a cursor is always the same size as the virt row its on,
 *  - there is only one cursor for a given virt row.
 *  - there is no overlap; a phys row can only belong to one virt row.
 *
 *  Lets say there are three cursors T(rans), S(plit), and B(lank).  
 *  Lets say that these are used to 'print' the following table layout:
 * 
@verbatim
virtual row 1   T
virtual row 2   S
virtual row 3   B
virtual row 4   T
virtual row 5   S
virtual row 6   S
virtual row 7   S
virtual row 8   S
virtual row 9   B
@endverbatim
 *
 *  You can see there is only one cursor per virtual row. There is no overlap
 *  between cursors and virtual rows; the correspondence is one to one. Note
 *  that the three cursors T, S and B may consist of one, or more physical rows,
 *  e.g. B and S may be one line each, but T may be two lines. Thus, we 
 *  have the following physical layout:
 *
@verbatim
physical row 1    virtual row 1   T
physical row 2    virtual row 1   T
physical row 3    virtual row 2   S
physical row 4    virtual row 3   B
physical row 5    virtual row 4   T
physical row 6    virtual row 4   T
physical row 7    virtual row 5   S
physical row 8    virtual row 6   S
physical row 9    virtual row 7   S
physical row 10   virtual row 8   S
physical row 11   virtual row 9   B
@endverbatim
 *
 *  This layout remains static until the next time that the table is 
 *  re-'printed'.
 */

#ifndef TABLE_ALLGUI_H
#define TABLE_ALLGUI_H

#include <glib.h>

#include "basiccell.h"
#include "cellblock.h"
#include "gtable.h"
#include "register-common.h"
#include "table-control.h"
#include "table-layout.h"
#include "table-model.h"

/* The VirtualCell structure holds information about each virtual cell. */
typedef struct
{
  CellBlock *cellblock;  /* Array of physical cells */
  gpointer   vcell_data; /* Used by higher-level code */

  /* flags */
  unsigned int visible : 1;             /* visible in the GUI */
  unsigned int start_primary_color : 1; /* color usage flag */
} VirtualCell;

typedef struct table Table;

typedef void (*TableCursorRefreshCB) (Table *table,
                                      VirtualCellLocation vcell_loc,
                                      gboolean do_scroll);

typedef void (*TableRedrawHelpCB) (Table *table);
typedef void (*TableDestroyCB) (Table *table);

typedef struct
{
  TableCursorRefreshCB cursor_refresh;

  TableRedrawHelpCB redraw_help;
  TableDestroyCB destroy;
} TableGUIHandlers;

struct table
{
  TableLayout  *layout;
  TableModel   *model;
  TableControl *control;

  int num_virt_rows;
  int num_virt_cols;

  CellBlock *current_cursor;

  VirtualLocation current_cursor_loc;

  /* private data */

  /* The virtual cell table */
  GTable *virt_cells;

  TableGUIHandlers gui_handlers;
  gpointer ui_data;
};


/* Set the default gui handlers used by new tables. */
void gnc_table_set_default_gui_handlers (TableGUIHandlers *gui_handlers);

/* Functions to create and destroy Tables.  */
Table *     gnc_table_new (TableLayout *layout,
                           TableModel *model,
                           TableControl *control);
void        gnc_virtual_location_init (VirtualLocation *vloc);

void        gnc_table_save_state (Table *table);
void        gnc_table_destroy (Table *table);


/* Functions to work with current cursor */
int         gnc_table_current_cursor_changed (Table *table,
                                              gboolean include_conditional);

void        gnc_table_clear_current_cursor_changes (Table *table);

void        gnc_table_save_current_cursor (Table *table, CursorBuffer *buffer);
void        gnc_table_restore_current_cursor (Table *table,
                                              CursorBuffer *buffer);

const char * gnc_table_get_current_cell_name (Table *table);

gboolean    gnc_table_get_current_cell_location (Table *table,
                                                 const char *cell_name,
                                                 VirtualLocation *virt_loc);


/* This function checks the given location and returns true
 * if it is out of bounds of the table. */
gboolean gnc_table_virtual_cell_out_of_bounds (Table *table,
                                               VirtualCellLocation vcell_loc);

gboolean gnc_table_virtual_location_in_header (Table *table,
                                               VirtualLocation virt_loc);


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
guint32        gnc_table_get_gtkrc_bg_color (Table *table, VirtualLocation virt_loc,
					     gboolean *hatching);

void           gnc_table_get_borders (Table *table, VirtualLocation virt_loc,
                                      PhysicalCellBorders *borders);

CellAlignment  gnc_table_get_align (Table *table, VirtualLocation virt_loc);

gboolean       gnc_table_is_popup (Table *table, VirtualLocation virt_loc);

char *         gnc_table_get_help (Table *table);

BasicCell *    gnc_table_get_cell (Table *table, VirtualLocation virt_loc);

const char *   gnc_table_get_cell_name (Table *table,
                                        VirtualLocation virt_loc);

gboolean       gnc_table_get_cell_location (Table *table,
                                            const char * cell_name,
                                            VirtualCellLocation vcell_loc,
                                            VirtualLocation *virt_loc);

void           gnc_table_save_cells (Table *table, gpointer save_data);


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

/** UI-specific functions *******************************/

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

/* Refresh the cursor in the given location. If do_scroll is TRUE,
 * scroll the register so the location is in view. */
void        gnc_table_refresh_cursor_gui (Table * table,
                                          VirtualCellLocation vcell_loc,
                                          gboolean do_scroll);

/* ==================================================== */

void         gnc_table_wrap_verify_cursor_position (Table *table,
                                                    VirtualLocation virt_loc);

gboolean     gnc_table_virtual_loc_valid(Table *table,
                                         VirtualLocation virt_loc,
                                         gboolean exact_pointer);

gboolean     gnc_table_move_tab (Table *table,
                                 VirtualLocation *virt_loc,
                                 gboolean move_right);

/** Moves away from virtual location @a virt_loc by @a phys_row_offset physical
 *  rows. Virtual cells that are not visible are skipped over.
 *
 *  @param table The table
 *
 *  @param virt_loc The virtual location to start from. If the move succeeds,
 *  it is updated with the new location.
 *
 *  @param phys_row_offset The number of physical rows to move. A positive
 *  number moves down and a negative number moves up.
 *
 *  @return TRUE if the location changed, FALSE otherwise
 */
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
                                     const char *change,
                                     int change_len,
                                     const char *newval,
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
/** @} */
/** @} */
/** @} */
