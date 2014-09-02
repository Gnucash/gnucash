/********************************************************************\
 * table-allgui.c -- 2D grid table object, embeds cells for i/o     *
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

/*
 * FILE:
 * table-allgui.c
 *
 * FUNCTION:
 * Implements the gui-independent parts of the table infrastructure.
 *
 * HISTORY:
 * Copyright (c) 1998,1999,2000 Linas Vepstas
 * Copyright (c) 2000 Dave Peticolas
 */

#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <glib.h>

#include "table-allgui.h"
#include "cellblock.h"
#include "gnc-engine.h"


/** Static Globals *****************************************************/

static TableGUIHandlers default_gui_handlers;

/* This static indicates the debugging module that this .o belongs to. */
static QofLogModule log_module = GNC_MOD_REGISTER;


/** Prototypes *********************************************************/
static void gnc_table_init (Table * table);
static void gnc_table_free_data (Table * table);
static void gnc_virtual_cell_construct (gpointer vcell, gpointer user_data);
static void gnc_virtual_cell_destroy (gpointer vcell, gpointer user_data);
static void gnc_table_resize (Table * table, int virt_rows, int virt_cols);


/** Implementation *****************************************************/

void
gnc_table_set_default_gui_handlers (TableGUIHandlers *gui_handlers)
{
    if (!gui_handlers)
        memset (&default_gui_handlers, 0, sizeof (default_gui_handlers));
    else
        default_gui_handlers = *gui_handlers;
}

Table *
gnc_table_new (TableLayout *layout, TableModel *model, TableControl *control)
{
    Table *table;

    g_return_val_if_fail (layout != NULL, NULL);
    g_return_val_if_fail (model != NULL, NULL);
    g_return_val_if_fail (control != NULL, NULL);

    table = g_new0 (Table, 1);

    table->layout = layout;
    table->model = model;
    table->control = control;

    table->gui_handlers = default_gui_handlers;

    gnc_table_init (table);

    table->virt_cells = g_table_new (sizeof (VirtualCell),
                                     gnc_virtual_cell_construct,
                                     gnc_virtual_cell_destroy, table);

    return table;
}

static void
gnc_table_init (Table * table)
{
    table->num_virt_rows = -1;
    table->num_virt_cols = -1;

    table->current_cursor = NULL;

    gnc_virtual_location_init (&table->current_cursor_loc);

    /* initialize private data */

    table->virt_cells = NULL;
    table->ui_data = NULL;
}

void
gnc_table_destroy (Table * table)
{
    /* invoke destroy callback */
    if (table->gui_handlers.destroy)
        table->gui_handlers.destroy (table);

    /* free the dynamic structures */
    gnc_table_free_data (table);

    /* free the cell tables */
    g_table_destroy (table->virt_cells);

    gnc_table_layout_destroy (table->layout);
    table->layout = NULL;

    gnc_table_control_destroy (table->control);
    table->control = NULL;

    gnc_table_model_destroy (table->model);
    table->model = NULL;

    /* intialize vars to null value so that any access is voided. */
    gnc_table_init (table);

    g_free (table);
}

int
gnc_table_current_cursor_changed (Table *table,
                                  gboolean include_conditional)
{
    if (!table)
        return FALSE;

    return gnc_cellblock_changed (table->current_cursor, include_conditional);
}

void
gnc_table_clear_current_cursor_changes (Table *table)
{
    if (!table)
        return;

    gnc_cellblock_clear_changes (table->current_cursor);
}

void
gnc_table_save_current_cursor (Table *table, CursorBuffer *buffer)
{
    if (!table || !buffer)
        return;

    gnc_table_layout_save_cursor (table->layout, table->current_cursor, buffer);
}

void
gnc_table_restore_current_cursor (Table *table,
                                  CursorBuffer *buffer)
{
    if (!table || !buffer)
        return;

    gnc_table_layout_restore_cursor (table->layout,
                                     table->current_cursor, buffer);
}

const char *
gnc_table_get_current_cell_name (Table *table)
{
    if (table == NULL)
        return NULL;

    return gnc_table_get_cell_name (table, table->current_cursor_loc);
}

gboolean
gnc_table_get_current_cell_location (Table *table,
                                     const char *cell_name,
                                     VirtualLocation *virt_loc)
{
    if (table == NULL)
        return FALSE;

    return gnc_table_get_cell_location (table, cell_name,
                                        table->current_cursor_loc.vcell_loc,
                                        virt_loc);
}

gboolean
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

gboolean
gnc_table_virtual_location_in_header (Table *table,
                                      VirtualLocation virt_loc)
{
    return (virt_loc.vcell_loc.virt_row == 0);
}

VirtualCell *
gnc_table_get_virtual_cell (Table *table, VirtualCellLocation vcell_loc)
{
    if (table == NULL)
        return NULL;

    return g_table_index (table->virt_cells,
                          vcell_loc.virt_row, vcell_loc.virt_col);
}

VirtualCell *
gnc_table_get_header_cell (Table *table)
{
    VirtualCellLocation vcell_loc = { 0, 0 };

    return gnc_table_get_virtual_cell (table, vcell_loc);
}

static const char *
gnc_table_get_entry_internal (Table *table, VirtualLocation virt_loc,
                              gboolean *conditionally_changed)
{
    TableGetEntryHandler entry_handler;
    const char *cell_name;
    const char *entry;

    cell_name = gnc_table_get_cell_name (table, virt_loc);

    entry_handler = gnc_table_model_get_entry_handler (table->model, cell_name);
    if (!entry_handler) return "";

    entry = entry_handler (virt_loc, FALSE,
                           conditionally_changed,
                           table->model->handler_user_data);
    if (!entry)
        entry = "";

    return entry;
}

const char *
gnc_table_get_entry (Table *table, VirtualLocation virt_loc)
{
    TableGetEntryHandler entry_handler;
    const char *entry;
    BasicCell *cell;

    cell = gnc_table_get_cell (table, virt_loc);
    if (!cell || !cell->cell_name)
        return "";

    if (virt_cell_loc_equal (table->current_cursor_loc.vcell_loc,
                             virt_loc.vcell_loc))
    {
        CellIOFlags io_flags;

        io_flags = gnc_table_get_io_flags (table, virt_loc);

        if (io_flags & XACC_CELL_ALLOW_INPUT)
            return cell->value;
    }

    entry_handler = gnc_table_model_get_entry_handler (table->model,
                    cell->cell_name);
    if (!entry_handler) return "";

    entry = entry_handler (virt_loc, TRUE, NULL,
                           table->model->handler_user_data);
    if (!entry)
        entry = "";

    return entry;
}

CellIOFlags
gnc_table_get_io_flags (Table *table, VirtualLocation virt_loc)
{
    TableGetCellIOFlagsHandler io_flags_handler;
    const char *cell_name;
    CellIOFlags flags;

    if (!table || !table->model)
        return XACC_CELL_ALLOW_NONE;

    cell_name = gnc_table_get_cell_name (table, virt_loc);

    io_flags_handler = gnc_table_model_get_io_flags_handler (table->model,
                       cell_name);
    if (!io_flags_handler)
        return XACC_CELL_ALLOW_NONE;

    flags = io_flags_handler (virt_loc, table->model->handler_user_data);

    if (gnc_table_model_read_only (table->model))
        flags &= XACC_CELL_ALLOW_SHADOW;

    return flags;
}

const char *
gnc_table_get_label (Table *table, VirtualLocation virt_loc)
{
    TableGetLabelHandler label_handler;
    const char *cell_name;
    const char *label;

    if (!table || !table->model)
        return "";

    cell_name = gnc_table_get_cell_name (table, virt_loc);

    label_handler = gnc_table_model_get_label_handler (table->model, cell_name);
    if (!label_handler)
        return "";

    label = label_handler (virt_loc, table->model->handler_user_data);
    if (!label)
        return "";

    return label;
}

static guint32
gnc_table_get_fg_color_internal (Table *table, VirtualLocation virt_loc,
                                 gboolean want_gtkrc)
{
    TableGetFGColorHandler fg_color_handler;
    const char *handler_name = "gtkrc";

    if (!table || !table->model)
        return 0x0; /* black */

    if (!want_gtkrc)
        handler_name = gnc_table_get_cell_name (table, virt_loc);

    fg_color_handler = gnc_table_model_get_fg_color_handler (table->model,
                       handler_name);
    if (!fg_color_handler)
        return 0x0;

    return fg_color_handler (virt_loc, table->model->handler_user_data);
}

guint32
gnc_table_get_fg_color (Table *table, VirtualLocation virt_loc)
{
    return gnc_table_get_fg_color_internal (table, virt_loc, FALSE);
}

guint32
gnc_table_get_gtkrc_fg_color (Table *table, VirtualLocation virt_loc)
{
    return gnc_table_get_fg_color_internal (table, virt_loc, TRUE);
}

static guint32
gnc_table_get_bg_color_internal (Table *table, VirtualLocation virt_loc,
                                 gboolean *hatching,
                                 gboolean want_gtkrc)
{
    TableGetBGColorHandler bg_color_handler;
    const char *handler_name = "gtkrc";

    if (hatching)
        *hatching = FALSE;

    if (!table || !table->model)
        return 0xffffff; /* white */

    if (!want_gtkrc)
        handler_name = gnc_table_get_cell_name (table, virt_loc);

    bg_color_handler = gnc_table_model_get_bg_color_handler (table->model,
            handler_name);
    if (!bg_color_handler)
        return 0xffffff;

    return bg_color_handler (virt_loc, hatching,
                             table->model->handler_user_data);
}

guint32
gnc_table_get_bg_color (Table *table, VirtualLocation virt_loc,
                        gboolean *hatching)
{
    return gnc_table_get_bg_color_internal (table, virt_loc, hatching, FALSE);
}

guint32
gnc_table_get_gtkrc_bg_color (Table *table, VirtualLocation virt_loc,
                              gboolean *hatching)
{
    return gnc_table_get_bg_color_internal (table, virt_loc, hatching, TRUE);
}

void
gnc_table_get_borders (Table *table, VirtualLocation virt_loc,
                       PhysicalCellBorders *borders)
{
    TableGetCellBorderHandler cell_border_handler;
    const char *cell_name;

    if (!table || !table->model)
        return;

    cell_name = gnc_table_get_cell_name (table, virt_loc);

    cell_border_handler = gnc_table_model_get_cell_border_handler (table->model,
                          cell_name);
    if (!cell_border_handler)
        return;

    cell_border_handler (virt_loc, borders, table->model->handler_user_data);
}

CellAlignment
gnc_table_get_align (Table *table, VirtualLocation virt_loc)
{
    BasicCell *cell;

    cell = gnc_table_get_cell (table, virt_loc);
    if (!cell)
        return CELL_ALIGN_RIGHT;

    return cell->alignment;
}

gboolean
gnc_table_is_popup (Table *table, VirtualLocation virt_loc)
{
    BasicCell *cell;

    cell = gnc_table_get_cell (table, virt_loc);
    if (!cell)
        return FALSE;

    return cell->is_popup;
}

char *
gnc_table_get_help (Table *table)
{
    TableGetHelpHandler help_handler;
    VirtualLocation virt_loc;
    const char * cell_name;

    if (!table)
        return NULL;

    virt_loc = table->current_cursor_loc;

    cell_name = gnc_table_get_cell_name (table, virt_loc);

    help_handler = gnc_table_model_get_help_handler (table->model, cell_name);
    if (!help_handler)
        return NULL;

    return help_handler (virt_loc, table->model->handler_user_data);
}

BasicCell *
gnc_table_get_cell (Table *table, VirtualLocation virt_loc)
{
    VirtualCell *vcell;

    if (!table)
        return NULL;

    vcell = gnc_table_get_virtual_cell (table, virt_loc.vcell_loc);
    if (!vcell)
        return NULL;

    return gnc_cellblock_get_cell (vcell->cellblock,
                                   virt_loc.phys_row_offset,
                                   virt_loc.phys_col_offset);
}

const char *
gnc_table_get_cell_name (Table *table, VirtualLocation virt_loc)
{
    BasicCell *cell;

    cell = gnc_table_get_cell (table, virt_loc);
    if (cell == NULL)
        return NULL;

    return cell->cell_name;
}

const gchar *
gnc_table_get_cell_type_name (Table *table, VirtualLocation virt_loc)
{
    BasicCell *cell;

    cell = gnc_table_get_cell (table, virt_loc);
    if (cell == NULL)
        return NULL;

    return cell->cell_type_name;
}


gboolean
gnc_table_get_cell_location (Table *table,
                             const char *cell_name,
                             VirtualCellLocation vcell_loc,
                             VirtualLocation *virt_loc)
{
    VirtualCell *vcell;
    CellBlock *cellblock;
    int cell_row, cell_col;

    if (table == NULL)
        return FALSE;

    vcell = gnc_table_get_virtual_cell (table, vcell_loc);
    if (vcell == NULL)
        return FALSE;

    cellblock = vcell->cellblock;

    for (cell_row = 0; cell_row < cellblock->num_rows; cell_row++)
        for (cell_col = 0; cell_col < cellblock->num_cols; cell_col++)
        {
            BasicCell *cell;

            cell = gnc_cellblock_get_cell (cellblock, cell_row, cell_col);
            if (!cell)
                continue;

            if (gnc_basic_cell_has_name (cell, cell_name))
            {
                if (virt_loc != NULL)
                {
                    virt_loc->vcell_loc = vcell_loc;

                    virt_loc->phys_row_offset = cell_row;
                    virt_loc->phys_col_offset = cell_col;
                }

                return TRUE;
            }
        }

    return FALSE;
}

void
gnc_table_save_cells (Table *table, gpointer save_data)
{
    TableSaveHandler save_handler;
    GList * cells;
    GList * node;

    g_return_if_fail (table);

    /* ignore any changes to read-only tables */
    if (gnc_table_model_read_only (table->model))
        return;

    // gnc_table_leave_update (table, table->current_cursor_loc);

    save_handler = gnc_table_model_get_pre_save_handler (table->model);
    if (save_handler)
        save_handler (save_data, table->model->handler_user_data);

    cells = gnc_table_layout_get_cells (table->layout);
    for (node = cells; node; node = node->next)
    {
        BasicCell * cell = node->data;
        TableSaveCellHandler save_cell_handler;

        if (!cell) continue;

        if (!gnc_table_layout_get_cell_changed (table->layout,
                                                cell->cell_name, TRUE))
            continue;

        save_cell_handler = gnc_table_model_get_save_handler (table->model,
                            cell->cell_name);
        if (save_cell_handler)
            save_cell_handler (cell, save_data, table->model->handler_user_data);
    }

    save_handler = gnc_table_model_get_post_save_handler (table->model);
    if (save_handler)
        save_handler (save_data, table->model->handler_user_data);
}

void
gnc_table_set_size (Table * table, int virt_rows, int virt_cols)
{
    /* Invalidate the current cursor position, if the array is
     * shrinking. This must be done since the table is probably
     * shrinking because some rows were deleted, and the cursor
     * could be on the deleted rows. */
    if ((virt_rows < table->num_virt_rows) ||
            (virt_cols < table->num_virt_cols))
    {
        gnc_virtual_location_init (&table->current_cursor_loc);
        table->current_cursor = NULL;
    }

    gnc_table_resize (table, virt_rows, virt_cols);
}

static void
gnc_table_free_data (Table * table)
{
    if (table == NULL)
        return;

    g_table_resize (table->virt_cells, 0, 0);
}

void
gnc_virtual_location_init (VirtualLocation *vloc)
{
    if (vloc == NULL)
        return;

    vloc->phys_row_offset = -1;
    vloc->phys_col_offset = -1;
    vloc->vcell_loc.virt_row = -1;
    vloc->vcell_loc.virt_col = -1;
}

static void
gnc_virtual_cell_construct (gpointer _vcell, gpointer user_data)
{
    VirtualCell *vcell = _vcell;
    Table *table = user_data;

    vcell->cellblock = NULL;

    if (table && table->model->cell_data_allocator)
        vcell->vcell_data = table->model->cell_data_allocator ();
    else
        vcell->vcell_data = NULL;

    vcell->visible = 1;
}

static void
gnc_virtual_cell_destroy (gpointer _vcell, gpointer user_data)
{
    VirtualCell *vcell = _vcell;
    Table *table = user_data;

    if (vcell->vcell_data && table && table->model->cell_data_deallocator)
        table->model->cell_data_deallocator (vcell->vcell_data);

    vcell->vcell_data = NULL;
}

static void
gnc_table_resize (Table * table, int new_virt_rows, int new_virt_cols)
{
    if (!table) return;

    g_table_resize (table->virt_cells, new_virt_rows, new_virt_cols);

    table->num_virt_rows = new_virt_rows;
    table->num_virt_cols = new_virt_cols;
}

void
gnc_table_set_vcell (Table *table,
                     CellBlock *cursor,
                     gconstpointer vcell_data,
                     gboolean visible,
                     gboolean start_primary_color,
                     VirtualCellLocation vcell_loc)
{
    VirtualCell *vcell;

    if ((table == NULL) || (cursor == NULL))
        return;

    if ((vcell_loc.virt_row >= table->num_virt_rows) ||
            (vcell_loc.virt_col >= table->num_virt_cols))
        gnc_table_resize (table,
                          MAX (table->num_virt_rows, vcell_loc.virt_row + 1),
                          MAX (table->num_virt_cols, vcell_loc.virt_col + 1));

    vcell = gnc_table_get_virtual_cell (table, vcell_loc);
    if (vcell == NULL)
        return;

    /* this cursor is the handler for this block */
    vcell->cellblock = cursor;

    /* copy the vcell user data */
    if (table->model->cell_data_copy)
        table->model->cell_data_copy (vcell->vcell_data, vcell_data);
    else
        vcell->vcell_data = (gpointer) vcell_data;

    vcell->visible = visible ? 1 : 0;
    vcell->start_primary_color = start_primary_color ? 1 : 0;
}

void
gnc_table_set_virt_cell_data (Table *table,
                              VirtualCellLocation vcell_loc,
                              gconstpointer vcell_data)
{
    VirtualCell *vcell;

    if (table == NULL)
        return;

    vcell = gnc_table_get_virtual_cell (table, vcell_loc);
    if (vcell == NULL)
        return;

    if (table->model->cell_data_copy)
        table->model->cell_data_copy (vcell->vcell_data, vcell_data);
    else
        vcell->vcell_data = (gpointer) vcell_data;
}

void
gnc_table_set_virt_cell_visible (Table *table,
                                 VirtualCellLocation vcell_loc,
                                 gboolean visible)
{
    VirtualCell *vcell;

    if (table == NULL)
        return;

    vcell = gnc_table_get_virtual_cell (table, vcell_loc);
    if (vcell == NULL)
        return;

    vcell->visible = visible ? 1 : 0;
}

void
gnc_table_set_virt_cell_cursor (Table *table,
                                VirtualCellLocation vcell_loc,
                                CellBlock *cursor)
{
    VirtualCell *vcell;

    if (table == NULL)
        return;

    vcell = gnc_table_get_virtual_cell (table, vcell_loc);
    if (vcell == NULL)
        return;

    vcell->cellblock = cursor;
}

static void
gnc_table_move_cursor_internal (Table *table,
                                VirtualLocation new_virt_loc,
                                gboolean do_move_gui)
{
    int cell_row, cell_col;
    VirtualLocation virt_loc;
    VirtualCell *vcell;
    CellBlock *curs;

    ENTER("new_virt=(%d %d) do_move_gui=%d\n",
          new_virt_loc.vcell_loc.virt_row,
          new_virt_loc.vcell_loc.virt_col, do_move_gui);

    /* call the callback, allowing the app to commit any changes
     * associated with the current location of the cursor. Note that
     * this callback may recursively call this routine. */
    if (table->control->move_cursor && table->control->allow_move)
    {
        table->control->move_cursor (&new_virt_loc, table->control->user_data);

        /* The above callback can cause this routine to be called
         * recursively. As a result of this recursion, the cursor may
         * have gotten repositioned. We need to make sure we make
         * passive again. */
        if (do_move_gui)
            gnc_table_refresh_current_cursor_gui (table, FALSE);
    }

    /* invalidate the cursor for now; we'll fix it back up below */
    gnc_virtual_location_init (&table->current_cursor_loc);

    curs = table->current_cursor;
    table->current_cursor = NULL;

    /* check for out-of-bounds conditions (which may be deliberate) */
    if ((new_virt_loc.vcell_loc.virt_row < 0) ||
            (new_virt_loc.vcell_loc.virt_col < 0))
    {
        /* if the location is invalid, then we should take this
         * as a command to unmap the cursor gui. */
        if (do_move_gui && curs)
        {
            for (cell_row = 0; cell_row < curs->num_rows; cell_row++)
                for (cell_col = 0; cell_col < curs->num_cols; cell_col++)
                {
                    BasicCell *cell;

                    cell = gnc_cellblock_get_cell (curs, cell_row, cell_col);
                    if (cell)
                    {
                        cell->changed = FALSE;
                        cell->conditionally_changed = FALSE;

                        if (cell->gui_move)
                            cell->gui_move (cell);
                    }
                }
        }

        LEAVE("out of bounds\n");
        return;
    }

    if (!gnc_table_virtual_loc_valid (table, new_virt_loc, TRUE))
    {
        PWARN("bad table location");
        LEAVE("");
        return;
    }

    /* ok, we now have a valid position. Find the new cursor to use,
     * and initialize its cells */
    vcell = gnc_table_get_virtual_cell (table, new_virt_loc.vcell_loc);
    curs = vcell->cellblock;
    table->current_cursor = curs;

    /* record the new position */
    table->current_cursor_loc = new_virt_loc;

    virt_loc.vcell_loc = new_virt_loc.vcell_loc;

    /* update the cell values to reflect the new position */
    for (cell_row = 0; cell_row < curs->num_rows; cell_row++)
        for (cell_col = 0; cell_col < curs->num_cols; cell_col++)
        {
            BasicCell *cell;
            CellIOFlags io_flags;

            virt_loc.phys_row_offset = cell_row;
            virt_loc.phys_col_offset = cell_col;

            cell = gnc_cellblock_get_cell(curs, cell_row, cell_col);
            if (cell)
            {
                /* if a cell has a GUI, move that first, before setting
                 * the cell value.  Otherwise, we'll end up putting the
                 * new values in the old cell locations, and that would
                 * lead to confusion of all sorts. */
                if (do_move_gui && cell->gui_move)
                    cell->gui_move (cell);

                /* OK, now copy the string value from the table at large
                 * into the cell handler. */
                io_flags = gnc_table_get_io_flags (table, virt_loc);
                if (io_flags & XACC_CELL_ALLOW_SHADOW)
                {
                    const char *entry;
                    gboolean conditionally_changed = FALSE;

                    entry = gnc_table_get_entry_internal (table, virt_loc,
                                                          &conditionally_changed);

                    gnc_basic_cell_set_value (cell, entry);

                    cell->changed = FALSE;
                    cell->conditionally_changed = conditionally_changed;
                }
            }
        }

    LEAVE("did move\n");
}

void
gnc_table_move_cursor (Table *table, VirtualLocation new_virt_loc)
{
    if (!table) return;

    gnc_table_move_cursor_internal (table, new_virt_loc, FALSE);
}

/* same as above, but be sure to deal with GUI elements as well */
void
gnc_table_move_cursor_gui (Table *table, VirtualLocation new_virt_loc)
{
    if (!table) return;

    gnc_table_move_cursor_internal (table, new_virt_loc, TRUE);
}

/* gnc_table_verify_cursor_position checks the location of the cursor
 * with respect to a virtual location, and repositions the cursor
 * if necessary. Returns true if the cell cursor was repositioned. */
gboolean
gnc_table_verify_cursor_position (Table *table, VirtualLocation virt_loc)
{
    gboolean do_move = FALSE;
    gboolean moved_cursor = FALSE;

    if (!table) return FALSE;

    /* Someone may be trying to intentionally invalidate the cursor, in
     * which case the physical addresses could be out of bounds. For
     * example, in order to unmap it in preparation for a reconfig.
     * So, if the specified location is out of bounds, then the cursor
     * MUST be moved. */
    if (gnc_table_virtual_cell_out_of_bounds (table, virt_loc.vcell_loc))
        do_move = TRUE;

    if (!virt_cell_loc_equal (virt_loc.vcell_loc,
                              table->current_cursor_loc.vcell_loc))
        do_move = TRUE;

    if (do_move)
    {
        gnc_table_move_cursor_gui (table, virt_loc);
        moved_cursor = TRUE;
    }
    else if (!virt_loc_equal (virt_loc, table->current_cursor_loc))
    {
        table->current_cursor_loc = virt_loc;
        moved_cursor = TRUE;
    }

    return moved_cursor;
}

gpointer
gnc_table_get_vcell_data (Table *table, VirtualCellLocation vcell_loc)
{
    VirtualCell *vcell;

    if (!table) return NULL;

    vcell = gnc_table_get_virtual_cell (table, vcell_loc);
    if (vcell == NULL)
        return NULL;

    return vcell->vcell_data;
}

/* If any of the cells have GUI specific components that need
 * initialization, initialize them now. The realize() callback
 * on the cursor cell is how we inform the cell handler that
 * now is the time to initialize its GUI.  */
void
gnc_table_realize_gui (Table * table)
{
    GList *cells;
    GList *node;

    if (!table) return;
    if (!table->ui_data) return;

    cells = gnc_table_layout_get_cells (table->layout);

    for (node = cells; node; node = node->next)
    {
        BasicCell *cell = node->data;

        if (cell->gui_realize)
            cell->gui_realize (cell, table->ui_data);
    }
}

void
gnc_table_wrap_verify_cursor_position (Table *table, VirtualLocation virt_loc)
{
    VirtualLocation save_loc;
    gboolean moved_cursor;

    if (!table) return;

    ENTER("(%d %d)", virt_loc.vcell_loc.virt_row, virt_loc.vcell_loc.virt_col);

    save_loc = table->current_cursor_loc;

    /* VerifyCursor will do all sorts of gui-independent machinations */
    moved_cursor = gnc_table_verify_cursor_position (table, virt_loc);

    if (moved_cursor)
    {
        /* make sure *both* the old and the new cursor rows get redrawn */
        gnc_table_refresh_current_cursor_gui (table, TRUE);
        gnc_table_refresh_cursor_gui (table, save_loc.vcell_loc, FALSE);
    }

    LEAVE ("");
}

void
gnc_table_refresh_current_cursor_gui (Table * table, gboolean do_scroll)
{
    if (!table) return;

    gnc_table_refresh_cursor_gui (table, table->current_cursor_loc.vcell_loc,
                                  do_scroll);
}

gboolean
gnc_table_virtual_loc_valid(Table *table,
                            VirtualLocation virt_loc,
                            gboolean exact_pointer)
{
    VirtualCell *vcell;
    CellIOFlags io_flags;

    if (!table) return FALSE;

    /* header rows cannot be modified */
    if (virt_loc.vcell_loc.virt_row == 0)
        return FALSE;

    vcell = gnc_table_get_virtual_cell(table, virt_loc.vcell_loc);
    if (vcell == NULL)
        return FALSE;

    if (!vcell->visible)
        return FALSE;

    /* verify that offsets are valid. This may occur if the app that is
     * using the table has a paritally initialized cursor. (probably due
     * to a programming error, but maybe they meant to do this). */
    if ((0 > virt_loc.phys_row_offset) || (0 > virt_loc.phys_col_offset))
        return FALSE;

    /* check for a cell handler, but only if cell address is valid */
    if (vcell->cellblock == NULL) return FALSE;

    /* if table is read-only, any cell is ok :) */
    if (gnc_table_model_read_only (table->model)) return TRUE;

    io_flags = gnc_table_get_io_flags (table, virt_loc);

    /* if the cell allows ENTER, then it is ok */
    if (io_flags & XACC_CELL_ALLOW_ENTER) return TRUE;

    /* if cell is marked as output-only, you can't enter */
    if (0 == (XACC_CELL_ALLOW_INPUT & io_flags)) return FALSE;

    /* if cell is pointer only and this is not an exact pointer test,
     * it cannot be entered. */
    if (!exact_pointer && ((XACC_CELL_ALLOW_EXACT_ONLY & io_flags) != 0))
        return FALSE;

    return TRUE;
}

/* Handle the non gui-specific parts of a cell enter callback */
gboolean
gnc_table_enter_update (Table *table,
                        VirtualLocation virt_loc,
                        int *cursor_position,
                        int *start_selection,
                        int *end_selection)
{
    gboolean can_edit = TRUE;
    CellEnterFunc enter;
    BasicCell *cell;
    CellBlock *cb;
    int cell_row;
    int cell_col;
    CellIOFlags io_flags;

    if (table == NULL)
        return FALSE;

    cb = table->current_cursor;

    cell_row = virt_loc.phys_row_offset;
    cell_col = virt_loc.phys_col_offset;

    ENTER("enter %d %d (relrow=%d relcol=%d)",
          virt_loc.vcell_loc.virt_row,
          virt_loc.vcell_loc.virt_col,
          cell_row, cell_col);

    /* OK, if there is a callback for this cell, call it */
    cell = gnc_cellblock_get_cell (cb, cell_row, cell_col);
    if (!cell)
    {
        LEAVE("no cell");
        return FALSE;
    }

    io_flags = gnc_table_get_io_flags (table, virt_loc);
    if (io_flags == XACC_CELL_ALLOW_READ_ONLY)
    {
        LEAVE("read only cell");
        return FALSE;
    }

    enter = cell->enter_cell;

    if (enter)
    {
        char * old_value;

        DEBUG("gnc_table_enter_update(): %d %d has enter handler\n",
              cell_row, cell_col);

        old_value = g_strdup (cell->value);

        can_edit = enter (cell, cursor_position, start_selection, end_selection);

        if (g_strcmp0 (old_value, cell->value) != 0)
        {
            if (gnc_table_model_read_only (table->model))
            {
                PWARN ("enter update changed read-only table");
            }

            cell->changed = TRUE;
        }

        g_free (old_value);
    }

    if (table->gui_handlers.redraw_help)
        table->gui_handlers.redraw_help (table);

    LEAVE("return %d\n", can_edit);
    return can_edit;
}

void
gnc_table_leave_update (Table *table, VirtualLocation virt_loc)
{
    CellLeaveFunc leave;
    BasicCell *cell;
    CellBlock *cb;
    int cell_row;
    int cell_col;

    if (table == NULL)
        return;

    cb = table->current_cursor;

    cell_row = virt_loc.phys_row_offset;
    cell_col = virt_loc.phys_col_offset;

    ENTER("proposed (%d %d) rel(%d %d)\n",
          virt_loc.vcell_loc.virt_row,
          virt_loc.vcell_loc.virt_col,
          cell_row, cell_col);

    /* OK, if there is a callback for this cell, call it */
    cell = gnc_cellblock_get_cell (cb, cell_row, cell_col);
    if (!cell)
    {
        LEAVE("no cell");
        return;
    }

    leave = cell->leave_cell;

    if (leave)
    {
        char * old_value;

        old_value = g_strdup (cell->value);

        leave (cell);

        if (g_strcmp0 (old_value, cell->value) != 0)
        {
            if (gnc_table_model_read_only (table->model))
            {
                PWARN ("leave update changed read-only table");
            }

            cell->changed = TRUE;
        }

        g_free (old_value);
    }
    LEAVE("");
}

gboolean
gnc_table_confirm_change (Table *table, VirtualLocation virt_loc)
{
    TableConfirmHandler confirm_handler;
    const char *cell_name;

    if (!table || !table->model)
        return TRUE;

    cell_name = gnc_table_get_cell_name (table, virt_loc);

    confirm_handler = gnc_table_model_get_confirm_handler (table->model,
                      cell_name);
    if (!confirm_handler)
        return TRUE;

    return confirm_handler (virt_loc, table->model->handler_user_data);
}

/* Returned result should not be touched by the caller.
 * NULL return value means the edit was rejected. */
const char *
gnc_table_modify_update (Table *table,
                         VirtualLocation virt_loc,
                         const char *change,
                         int change_len,
                         const char *newval,
                         int newval_len,
                         int *cursor_position,
                         int *start_selection,
                         int *end_selection,
                         gboolean *cancelled)
{
    gboolean changed = FALSE;
    CellModifyVerifyFunc mv;
    BasicCell *cell;
    CellBlock *cb;
    int cell_row;
    int cell_col;
    char * old_value;

    g_return_val_if_fail (table, NULL);
    g_return_val_if_fail (table->model, NULL);

    if (gnc_table_model_read_only (table->model))
    {
        PWARN ("change to read-only table");
        return NULL;
    }

    cb = table->current_cursor;

    cell_row = virt_loc.phys_row_offset;
    cell_col = virt_loc.phys_col_offset;

    ENTER ("");

    if (!gnc_table_confirm_change (table, virt_loc))
    {
        if (cancelled)
            *cancelled = TRUE;

        LEAVE("change cancelled");
        return NULL;
    }

    if (cancelled)
        *cancelled = FALSE;

    /* OK, if there is a callback for this cell, call it */
    cell = gnc_cellblock_get_cell (cb, cell_row, cell_col);
    if (!cell)
    {
        LEAVE("no cell");
        return NULL;
    }

    mv = cell->modify_verify;

    old_value = g_strdup (cell->value);

    if (mv)
    {
        mv (cell, change, change_len, newval, newval_len,
            cursor_position, start_selection, end_selection);
    }
    else
    {
        gnc_basic_cell_set_value (cell, newval);
    }

    if (g_strcmp0 (old_value, cell->value) != 0)
    {
        changed = TRUE;
        cell->changed = TRUE;
    }

    g_free (old_value);

    if (table->gui_handlers.redraw_help)
        table->gui_handlers.redraw_help (table);

    LEAVE ("change %d %d (relrow=%d relcol=%d) val=%s\n",
           virt_loc.vcell_loc.virt_row,
           virt_loc.vcell_loc.virt_col,
           cell_row, cell_col,
           cell->value ? cell->value : "(null)");

    if (changed)
        return cell->value;
    else
        return NULL;
}

gboolean
gnc_table_direct_update (Table *table,
                         VirtualLocation virt_loc,
                         char **newval_ptr,
                         int *cursor_position,
                         int *start_selection,
                         int *end_selection,
                         gpointer gui_data)
{
    gboolean result;
    BasicCell *cell;
    CellBlock *cb;
    int cell_row;
    int cell_col;
    char * old_value;

    g_return_val_if_fail (table, FALSE);
    g_return_val_if_fail (table->model, FALSE);

    if (gnc_table_model_read_only (table->model))
    {
        PWARN ("input to read-only table");
        return FALSE;
    }

    cb = table->current_cursor;

    cell_row = virt_loc.phys_row_offset;
    cell_col = virt_loc.phys_col_offset;

    cell = gnc_cellblock_get_cell (cb, cell_row, cell_col);
    if (!cell)
        return FALSE;

    ENTER ("");

    if (cell->direct_update == NULL)
    {
        LEAVE("no direct update");
        return FALSE;
    }

    old_value = g_strdup (cell->value);

    result = cell->direct_update (cell, cursor_position, start_selection,
                                  end_selection, gui_data);

    if (g_strcmp0 (old_value, cell->value) != 0)
    {
        if (!gnc_table_confirm_change (table, virt_loc))
        {
            gnc_basic_cell_set_value (cell, old_value);
            *newval_ptr = NULL;
            result = TRUE;
        }
        else
        {
            cell->changed = TRUE;
            *newval_ptr = cell->value;
        }
    }
    else
        *newval_ptr = NULL;

    g_free (old_value);

    if (table->gui_handlers.redraw_help)
        table->gui_handlers.redraw_help (table);

    LEAVE("");
    return result;
}

static gboolean gnc_table_find_valid_cell_horiz (Table *table,
        VirtualLocation *virt_loc,
        gboolean exact_cell);

static gboolean
gnc_table_find_valid_row_vert (Table *table, VirtualLocation *virt_loc)
{
    VirtualLocation vloc;
    VirtualCell *vcell = NULL;
    int top;
    int bottom;

    if (table == NULL)
        return FALSE;

    if (virt_loc == NULL)
        return FALSE;

    vloc = *virt_loc;

    if (vloc.vcell_loc.virt_row < 1)
        vloc.vcell_loc.virt_row = 1;
    if (vloc.vcell_loc.virt_row >= table->num_virt_rows)
        vloc.vcell_loc.virt_row = table->num_virt_rows - 1;

    top  = vloc.vcell_loc.virt_row;
    bottom = vloc.vcell_loc.virt_row + 1;

    while (top >= 1 || bottom < table->num_virt_rows)
    {
        vloc.vcell_loc.virt_row = top;
        vcell = gnc_table_get_virtual_cell (table, vloc.vcell_loc);
        if (vcell && vcell->cellblock && vcell->visible)
        {
            vloc.phys_row_offset = 0;
            vloc.phys_col_offset = 0;

            if (gnc_table_find_valid_cell_horiz (table, &vloc, FALSE))
                break;
        }

        vloc.vcell_loc.virt_row = bottom;
        vcell = gnc_table_get_virtual_cell (table, vloc.vcell_loc);
        if (vcell && vcell->cellblock && vcell->visible)
        {
            vloc.phys_row_offset = 0;
            vloc.phys_col_offset = 0;

            if (gnc_table_find_valid_cell_horiz (table, &vloc, FALSE))
                break;
        }

        top--;
        bottom++;
    }

    if (!vcell || !vcell->cellblock || !vcell->visible)
        return FALSE;

    if (vloc.phys_row_offset < 0)
        vloc.phys_row_offset = 0;
    if (vloc.phys_row_offset >= vcell->cellblock->num_rows)
        vloc.phys_row_offset = vcell->cellblock->num_rows - 1;

    virt_loc->vcell_loc = vloc.vcell_loc;

    return TRUE;
}

static gboolean
gnc_table_find_valid_cell_horiz (Table *table,
                                 VirtualLocation *virt_loc,
                                 gboolean exact_cell)
{
    VirtualLocation vloc;
    VirtualCell *vcell;
    int left;
    int right;

    if (table == NULL)
        return FALSE;

    if (virt_loc == NULL)
        return FALSE;

    if (gnc_table_virtual_cell_out_of_bounds (table, virt_loc->vcell_loc))
        return FALSE;

    if (gnc_table_virtual_loc_valid (table, *virt_loc, exact_cell))
        return TRUE;

    vloc = *virt_loc;

    vcell = gnc_table_get_virtual_cell (table, vloc.vcell_loc);
    if (vcell == NULL)
        return FALSE;
    if (vcell->cellblock == NULL)
        return FALSE;

    if (vloc.phys_col_offset < 0)
        vloc.phys_col_offset = 0;
    if (vloc.phys_col_offset >= vcell->cellblock->num_cols)
        vloc.phys_col_offset = vcell->cellblock->num_cols - 1;

    left  = vloc.phys_col_offset - 1;
    right = vloc.phys_col_offset + 1;

    while (left >= 0 || right < vcell->cellblock->num_cols)
    {
        vloc.phys_col_offset = right;
        if (gnc_table_virtual_loc_valid(table, vloc, FALSE))
        {
            *virt_loc = vloc;
            return TRUE;
        }

        vloc.phys_col_offset = left;
        if (gnc_table_virtual_loc_valid(table, vloc, FALSE))
        {
            *virt_loc = vloc;
            return TRUE;
        }

        left--;
        right++;
    }

    return FALSE;
}

gboolean
gnc_table_find_close_valid_cell (Table *table, VirtualLocation *virt_loc,
                                 gboolean exact_pointer)
{
    if (!gnc_table_find_valid_row_vert (table, virt_loc))
        return FALSE;

    return gnc_table_find_valid_cell_horiz (table, virt_loc, exact_pointer);
}

void
gnc_table_refresh_cursor_gui (Table * table,
                              VirtualCellLocation vcell_loc,
                              gboolean do_scroll)
{
    g_return_if_fail (table != NULL);
    g_return_if_fail (table->gui_handlers.cursor_refresh != NULL);

    table->gui_handlers.cursor_refresh (table, vcell_loc, do_scroll);
}

gboolean
gnc_table_move_tab (Table *table,
                    VirtualLocation *virt_loc,
                    gboolean move_right)
{
    VirtualCell *vcell;
    VirtualLocation vloc;
    BasicCell *cell;

    if ((table == NULL) || (virt_loc == NULL))
        return FALSE;

    vloc = *virt_loc;

    vcell = gnc_table_get_virtual_cell (table, vloc.vcell_loc);
    if ((vcell == NULL) || (vcell->cellblock == NULL) || !vcell->visible)
        return FALSE;

    while (1)
    {
        CellIOFlags io_flags;

        if (move_right)
        {
            vloc.phys_col_offset++;

            if (vloc.phys_col_offset >= vcell->cellblock->num_cols)
            {
                if (!gnc_table_move_vertical_position (table, &vloc, 1))
                    return FALSE;

                vloc.phys_col_offset = 0;
            }
        }
        else
        {
            vloc.phys_col_offset--;

            if (vloc.phys_col_offset < 0)
            {
                if (!gnc_table_move_vertical_position (table, &vloc, -1))
                    return FALSE;

                vloc.phys_col_offset = vcell->cellblock->num_cols - 1;
            }
        }

        vcell = gnc_table_get_virtual_cell (table, vloc.vcell_loc);
        if ((vcell == NULL) || (vcell->cellblock == NULL) || !vcell->visible)
            return FALSE;

        cell = gnc_cellblock_get_cell (vcell->cellblock,
                                       vloc.phys_row_offset,
                                       vloc.phys_col_offset);
        if (!cell)
            continue;

        io_flags = gnc_table_get_io_flags (table, vloc);

        if (!(io_flags & XACC_CELL_ALLOW_INPUT))
            continue;

        if (io_flags & XACC_CELL_ALLOW_EXACT_ONLY)
            continue;

        break;
    }

    {
        gboolean changed = !virt_loc_equal (vloc, *virt_loc);

        *virt_loc = vloc;

        return changed;
    }
}

gboolean
gnc_table_move_vertical_position (Table *table,
                                  VirtualLocation *virt_loc,
                                  int phys_row_offset)
{
    VirtualLocation vloc;
    VirtualCell *vcell;
    gint last_visible_row;

    if ((table == NULL) || (virt_loc == NULL))
        return FALSE;

    vloc = *virt_loc;
    last_visible_row = vloc.vcell_loc.virt_row;

    vcell = gnc_table_get_virtual_cell (table, vloc.vcell_loc);
    if ((vcell == NULL) || (vcell->cellblock == NULL))
        return FALSE;

    while (phys_row_offset != 0)
    {
        /* going up */
        if (phys_row_offset < 0)
        {
            phys_row_offset++;

            /* room left in the current cursor */
            if (vloc.phys_row_offset > 0)
            {
                vloc.phys_row_offset--;
                continue;
            }

            /* end of the line */
            if (vloc.vcell_loc.virt_row == 1)
                break;

            do
            {
                vloc.vcell_loc.virt_row--;

                vcell = gnc_table_get_virtual_cell (table, vloc.vcell_loc);
            }
            while (vcell && vcell->cellblock && !vcell->visible);

            if (!vcell || !vcell->cellblock)
                break;

            last_visible_row = vloc.vcell_loc.virt_row;
            vloc.phys_row_offset = vcell->cellblock->num_rows - 1;
        }
        /* going down */
        else
        {
            phys_row_offset--;

            /* room left in the current cursor */
            if (vloc.phys_row_offset < (vcell->cellblock->num_rows - 1))
            {
                vloc.phys_row_offset++;
                continue;
            }

            /* end of the line */
            if (vloc.vcell_loc.virt_row == (table->num_virt_rows - 1))
                break;

            do
            {
                vloc.vcell_loc.virt_row++;

                vcell = gnc_table_get_virtual_cell (table, vloc.vcell_loc);
            }
            while (vcell && vcell->cellblock && !vcell->visible);

            if (!vcell || !vcell->cellblock)
                break;

            last_visible_row = vloc.vcell_loc.virt_row;
            vloc.phys_row_offset = 0;
        }
    }

    vloc.vcell_loc.virt_row = last_visible_row;

    {
        gboolean changed = !virt_loc_equal (vloc, *virt_loc);

        *virt_loc = vloc;

        return changed;
    }
}

gboolean
gnc_table_traverse_update(Table *table,
                          VirtualLocation virt_loc,
                          gncTableTraversalDir dir,
                          VirtualLocation *dest_loc)
{
    gboolean abort_move;

    if ((table == NULL) || (dest_loc == NULL))
        return FALSE;

    ENTER("proposed (%d %d) -> (%d %d)\n",
          virt_loc.vcell_loc.virt_row, virt_loc.vcell_loc.virt_row,
          dest_loc->vcell_loc.virt_row, dest_loc->vcell_loc.virt_col);

    /* first, make sure our destination cell is valid. If it is out
     * of bounds report an error. I don't think this ever happens. */
    if (gnc_table_virtual_cell_out_of_bounds (table, dest_loc->vcell_loc))
    {
        PERR("destination (%d, %d) out of bounds (%d, %d)\n",
             dest_loc->vcell_loc.virt_row, dest_loc->vcell_loc.virt_col,
             table->num_virt_rows, table->num_virt_cols);
        LEAVE("");
        return TRUE;
    }

    /* next, check the current row and column.  If they are out of bounds
     * we can recover by treating the traversal as a mouse point. This can
     * occur whenever the register widget is resized smaller, maybe?. */
    if (!gnc_table_virtual_loc_valid (table, virt_loc, TRUE))
    {
        PINFO("source (%d, %d) out of bounds (%d, %d)\n",
              virt_loc.vcell_loc.virt_row, virt_loc.vcell_loc.virt_col,
              table->num_virt_rows, table->num_virt_cols);

        dir = GNC_TABLE_TRAVERSE_POINTER;
    }

    /* process forward-moving traversals */
    switch (dir)
    {
    case GNC_TABLE_TRAVERSE_RIGHT:
    case GNC_TABLE_TRAVERSE_LEFT:
        gnc_table_find_valid_cell_horiz(table, dest_loc, FALSE);

        break;

    case GNC_TABLE_TRAVERSE_UP:
    case GNC_TABLE_TRAVERSE_DOWN:
    {
        VirtualLocation new_loc = *dest_loc;
        int increment;
        int col_offset = 0;
        gboolean second_traversal = FALSE;

        /* Keep going in the specified direction until we find a valid
         * row to land on, or we hit the end of the table. At the end,
         * turn around and go back until we find a valid row or we get
         * to where we started. If we still can't find anything, try
         * going left and right. */
        increment = (dir == GNC_TABLE_TRAVERSE_DOWN) ? 1 : -1;

        while (!gnc_table_virtual_loc_valid(table, new_loc, FALSE))
        {
            if (virt_loc_equal (new_loc, virt_loc))
            {
                new_loc = *dest_loc;
                gnc_table_find_valid_cell_horiz(table, &new_loc, FALSE);
                break;
            }

            if (!gnc_table_move_vertical_position (table, &new_loc, increment))
            {
                /* Special case: if there is no valid cell at all in the column
                 * we are scanning, (both up and down directions didn't work)
                 * attempt to do the same in the next column.
                 * Hack alert: there is no check to see if there really is a
                 * valid next column. However this situation so far only happens
                 * after a pagedown/pageup key event in the SX transaction editor
                 * which always tests the first column to start (which has no
                 * editable cells) and in that situation there is a valid next column.
                 */
                if (!second_traversal)
                    second_traversal = TRUE;
                else
                {
                    second_traversal = FALSE;
                    col_offset++;
                }
                increment *= -1;
                new_loc = *dest_loc;
                new_loc.phys_col_offset = new_loc.phys_col_offset + col_offset;
            }
        }

        *dest_loc = new_loc;
    }

    if (!gnc_table_virtual_loc_valid(table, *dest_loc, FALSE))
    {
        LEAVE("");
        return TRUE;
    }

    break;

    case GNC_TABLE_TRAVERSE_POINTER:
        if (!gnc_table_find_valid_cell_horiz(table, dest_loc, TRUE))
        {
            LEAVE("");
            return TRUE;
        }

        break;

    default:
        g_return_val_if_fail (FALSE, TRUE);
        break;
    }

    /* Call the table traverse callback for any modifications. */
    if (table->control->traverse)
        abort_move = table->control->traverse (dest_loc, dir,
                                               table->control->user_data);
    else
        abort_move = FALSE;

    LEAVE("dest_row = %d, dest_col = %d\n",
          dest_loc->vcell_loc.virt_row, dest_loc->vcell_loc.virt_col);

    return abort_move;
}
