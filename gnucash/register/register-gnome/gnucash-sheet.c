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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

/*
 * The Gnucash Sheet widget
 *
 *  Based heavily on the Gnumeric Sheet widget.
 *
 * Authors:
 *     Heath Martin <martinh@pegasus.cc.ucf.edu>
 *     Dave Peticolas <dave@krondo.com>
 */

#include <config.h>
#include <glib.h>
#include <gdk/gdk.h>

#include "gnucash-sheet.h"
#include "gnucash-sheetP.h"

#include "dialog-utils.h"
#include "gnc-gtk-utils.h"
#include "gnc-prefs.h"
#include "gnucash-color.h"
#include "gnucash-cursor.h"
#include "gnucash-style.h"
#include "gnucash-header.h"
#include "gnucash-item-edit.h"
#include "split-register.h"
#include "gnc-engine.h"     // For debugging, e.g. ENTER(), LEAVE()

#define DEFAULT_SHEET_HEIGHT 400
#define DEFAULT_SHEET_WIDTH  400
/* Used to calculate the minimum preferred height of the sheet layout: */
#define DEFAULT_SHEET_INITIAL_ROWS 10


/* Register signals */
enum
{
    ACTIVATE_CURSOR,
    REDRAW_ALL,
    REDRAW_HELP,
    LAST_SIGNAL
};


/** Static Globals *****************************************************/

/* This static indicates the debugging module that this .o belongs to. */
static QofLogModule log_module = G_LOG_DOMAIN;

/** Prototypes *********************************************************/

static void gnucash_sheet_start_editing_at_cursor (GnucashSheet *sheet);

static gboolean gnucash_sheet_cursor_move (GnucashSheet *sheet,
                                           VirtualLocation virt_loc);

static void gnucash_sheet_deactivate_cursor_cell (GnucashSheet *sheet);
static void gnucash_sheet_activate_cursor_cell (GnucashSheet *sheet,
                                                gboolean changed_cells);
static void gnucash_sheet_stop_editing (GnucashSheet *sheet);
static void gnucash_sheet_adjustment_changed (GtkAdjustment *adjustment,
                                               GnucashSheet *sheet);
static gboolean gnucash_sheet_handle_pointer_press (GnucashSheet *sheet,
                                                      guint button,
                                                      gint n_press,
                                                      double x,
                                                      double y);
static float clamp_scrollable_value (float value, GtkAdjustment *adjustment);
static gboolean gnucash_sheet_handle_scroll (GnucashSheet *sheet,
                                              double dx, double dy);

/** Implementation *****************************************************/

enum
{
    PROP_0,
    PROP_HADJUSTMENT,
    PROP_VADJUSTMENT,
    PROP_HSCROLL_POLICY,
    PROP_VSCROLL_POLICY,
};

static void gnucash_sheet_scrollable_init (GtkScrollableInterface *iface);

G_DEFINE_TYPE_WITH_CODE (GnucashSheet, gnucash_sheet, GTK_TYPE_FIXED,
                         G_IMPLEMENT_INTERFACE (GTK_TYPE_SCROLLABLE,
                                                gnucash_sheet_scrollable_init))

/* gtk_editable_set_position sets both current_pos and selection_bound to the
 * supplied value. gtk_editable_select_region(start, end) sets current_pos to
 * end and selection_bound to start; if either is < 0 it's changed to length.
 *
 * That's a bit orthogonal to the way GncTable sees things, so the following
 * functions translate between the two.
 */

static inline void
gnucash_sheet_set_entry_selection (GnucashSheet *sheet)
{
    DEBUG("Set entry selection to sheet: %d:%d", sheet->bound, sheet->pos);
    gtk_editable_select_region (GTK_EDITABLE(sheet->entry),
                                sheet->bound, sheet->pos);
}

static inline void
gnucash_sheet_set_selection_from_entry (GnucashSheet *sheet)
{
    gtk_editable_get_selection_bounds (GTK_EDITABLE(sheet->entry),
                                       &sheet->bound, &sheet->pos);
}

static inline void
gnucash_sheet_set_selection (GnucashSheet *sheet, int pos, int bound)
{
    DEBUG("Set sheet selection %d:%d", bound, pos);
    sheet->pos = pos;
    sheet->bound = bound;
    gnucash_sheet_set_entry_selection (sheet);
}

// The variable names here are intended to match the GncTable usage.
static inline void
gnucash_sheet_set_position_and_selection (GnucashSheet* sheet, int pos,
                                          int start, int end)
{
    if (pos == end || start == -1)
        gnucash_sheet_set_selection (sheet, pos, start);
    else if (pos == start || end == -1)
        gnucash_sheet_set_selection (sheet, start, end);
    else if (start == end)
        gnucash_sheet_set_selection (sheet, pos, pos);
    else
        gnucash_sheet_set_selection (sheet, pos, end);
}

static inline void
gnucash_sheet_set_position (GnucashSheet* sheet, int pos)
{
    gnucash_sheet_set_position_and_selection (sheet, pos, pos, pos);
}

static inline void
gnucash_sheet_set_entry_value (GnucashSheet *sheet, const char* value)
{
    g_signal_handler_block (G_OBJECT(sheet->entry),
                            sheet->insert_signal);
    g_signal_handler_block (G_OBJECT(sheet->entry),
                            sheet->delete_signal);

    gtk_editable_set_text (GTK_EDITABLE (sheet->entry), value);

    g_signal_handler_unblock (G_OBJECT(sheet->entry),
                              sheet->delete_signal);
    g_signal_handler_unblock (G_OBJECT(sheet->entry),
                              sheet->insert_signal);

}

static inline gboolean
gnucash_sheet_virt_cell_out_of_bounds (GnucashSheet *sheet,
                                       VirtualCellLocation vcell_loc)
{
    return (vcell_loc.virt_row < 1 ||
            vcell_loc.virt_row >= sheet->num_virt_rows ||
            vcell_loc.virt_col < 0 ||
            vcell_loc.virt_col >= sheet->num_virt_cols);
}

static gboolean
gnucash_sheet_cell_valid (GnucashSheet *sheet, VirtualLocation virt_loc)
{
    gboolean valid;
    SheetBlockStyle *style;

    valid = !gnucash_sheet_virt_cell_out_of_bounds (sheet,
                                                    virt_loc.vcell_loc);

    if (valid)
    {
        style = gnucash_sheet_get_style (sheet, virt_loc.vcell_loc);

        valid = (virt_loc.phys_row_offset >= 0 &&
                 virt_loc.phys_row_offset < style->nrows &&
                 virt_loc.phys_col_offset >= 0 &&
                 virt_loc.phys_col_offset < style->ncols);
    }

    return valid;
}


static void
gnucash_sheet_cursor_set (GnucashSheet *sheet, VirtualLocation virt_loc)
{
    g_return_if_fail (sheet != NULL);
    g_return_if_fail (GNUCASH_IS_SHEET(sheet));

    g_return_if_fail (virt_loc.vcell_loc.virt_row >= 0 ||
                      virt_loc.vcell_loc.virt_row <= sheet->num_virt_rows);
    g_return_if_fail (virt_loc.vcell_loc.virt_col >= 0 ||
                      virt_loc.vcell_loc.virt_col <= sheet->num_virt_cols);

    gtk_widget_queue_draw (GTK_WIDGET (sheet));

    gnucash_cursor_set (GNUCASH_CURSOR(sheet->cursor), virt_loc);

    gtk_widget_queue_draw (GTK_WIDGET (sheet));
}

void
gnucash_sheet_cursor_set_from_table (GnucashSheet *sheet, gboolean do_scroll)
{
    Table *table;
    VirtualLocation v_loc;

    g_return_if_fail (sheet != NULL);
    g_return_if_fail (GNUCASH_IS_SHEET(sheet));

    table = sheet->table;
    v_loc = table->current_cursor_loc;

    g_return_if_fail (gnucash_sheet_cell_valid (sheet, v_loc));

    gnucash_sheet_cursor_set (sheet, v_loc);

    if (do_scroll)
        gnucash_sheet_make_cell_visible (sheet, v_loc);
}


void
gnucash_sheet_set_popup (GnucashSheet *sheet, GtkWidget *popup, gpointer data)
{
    if (popup)
        g_object_ref (popup);

    if (sheet->popup)
        g_object_unref (sheet->popup);

    sheet->popup = popup;
    sheet->popup_data = data;
}


static void
gnucash_sheet_hide_editing_cursor (GnucashSheet *sheet)
{
    if (sheet->item_editor == NULL)
        return;

    gtk_widget_set_visible (sheet->item_editor, FALSE);
    gnc_item_edit_hide_popup (GNC_ITEM_EDIT(sheet->item_editor));
}

static void
gnucash_sheet_stop_editing (GnucashSheet *sheet)
{
    /* Rollback an uncommitted string if it exists   *
     * *before* disconnecting signal handlers.       */

    if (sheet->insert_signal != 0)
        g_signal_handler_disconnect (G_OBJECT(sheet->entry),
                                     sheet->insert_signal);
    if (sheet->delete_signal != 0)
        g_signal_handler_disconnect (G_OBJECT(sheet->entry),
                                     sheet->delete_signal);
    sheet->insert_signal = 0;
    sheet->delete_signal = 0;

    gnucash_sheet_hide_editing_cursor (sheet);

    sheet->editing = FALSE;
    sheet->input_cancelled = FALSE;
}


static void
gnucash_sheet_deactivate_cursor_cell (GnucashSheet *sheet)
{
    VirtualLocation virt_loc;

    gnucash_cursor_get_virt (GNUCASH_CURSOR(sheet->cursor), &virt_loc);

    gnucash_sheet_stop_editing (sheet);

    if (!gnc_table_model_read_only (sheet->table->model))
        gnc_table_leave_update (sheet->table, virt_loc);

    gnucash_sheet_redraw_block (sheet, virt_loc.vcell_loc);
}

void
gnucash_sheet_set_text_bounds (GnucashSheet *sheet, GdkRectangle *rect,
                               gint x, gint y, gint width, gint height)
{
    GncItemEdit *item_edit = GNC_ITEM_EDIT(sheet->item_editor);

    rect->x = x + gnc_item_edit_get_margin (item_edit, left);
    rect->y = y + gnc_item_edit_get_margin (item_edit, top);
    rect->width = MAX (0, width - gnc_item_edit_get_margin (item_edit, left_right));
    rect->height = height - gnc_item_edit_get_margin (item_edit, top_bottom);
}

gint
gnucash_sheet_get_text_offset (GnucashSheet *sheet, const VirtualLocation virt_loc,
                               gint rect_width, gint logical_width)
{
    GncItemEdit *item_edit = GNC_ITEM_EDIT(sheet->item_editor);
    Table *table = sheet->table;
    gint x_offset = 0;

    // Get the alignment of the cell
    switch (gnc_table_get_align (table, virt_loc))
    {
    default:
    case CELL_ALIGN_LEFT:
        x_offset = gnc_item_edit_get_padding_border (item_edit, left);
        break;

    case CELL_ALIGN_RIGHT:
        x_offset = rect_width - gnc_item_edit_get_padding_border (item_edit, right) - logical_width - 1;
        break;

    case CELL_ALIGN_CENTER:
        if (logical_width > rect_width)
            x_offset = 0;
        else
            x_offset = (rect_width - logical_width) / 2;
        break;
    }
    return x_offset;
}

static void
gnucash_sheet_activate_cursor_cell (GnucashSheet *sheet,
                                    gboolean changed_cells)
{
    Table *table = sheet->table;
    VirtualLocation virt_loc;
    SheetBlockStyle *style;
    int cursor_pos, start_sel, end_sel;
    gboolean allow_edits;

    /* Sanity check */
    if (sheet->editing)
        gnucash_sheet_deactivate_cursor_cell (sheet);

    gnucash_cursor_get_virt (GNUCASH_CURSOR(sheet->cursor), &virt_loc);

    /* This should be a no-op */
    gnc_table_wrap_verify_cursor_position (table, virt_loc);

    gnucash_cursor_get_virt (GNUCASH_CURSOR(sheet->cursor), &virt_loc);

    if (!gnc_table_virtual_loc_valid (table, virt_loc, TRUE))
        return;

    style = gnucash_sheet_get_style (sheet, virt_loc.vcell_loc);
    if (strcmp (style->cursor->cursor_name, CURSOR_HEADER) == 0)
        return;

    cursor_pos = -1;
    start_sel = 0;
    end_sel = 0;

    if (gnc_table_model_read_only (table->model))
        allow_edits = FALSE;
    else
        allow_edits = gnc_table_enter_update (table, virt_loc,
                                              &cursor_pos,
                                              &start_sel, &end_sel);

    if (!allow_edits)
        gnucash_sheet_redraw_block (sheet, virt_loc.vcell_loc);
    else
    {
        gtk_entry_reset_im_context (GTK_ENTRY (sheet->entry));
        gnucash_sheet_start_editing_at_cursor (sheet);

        // Came here by keyboard, select text, otherwise text cursor to
        // mouse position
        if (sheet->button != 1)
        {
            gnucash_sheet_set_position_and_selection (sheet, cursor_pos,
                                                      start_sel, end_sel);
        }
        else
        {
            GncItemEdit *item_edit = GNC_ITEM_EDIT(sheet->item_editor);
            Table *table = sheet->table;
            const char *text = gnc_table_get_entry (table, virt_loc);
            PangoLayout *layout;
            PangoRectangle logical_rect;
            GdkRectangle rect;
            gint x, y, width, height;
            gint index = 0, trailing = 0;
            gint x_offset = 0;

            if (text && *text)
            {
                // Get the item_edit position
                gnc_item_edit_get_pixel_coords (item_edit, &x, &y,
                                                &width, &height);
                layout = gtk_widget_create_pango_layout (GTK_WIDGET (sheet),
                                                         text);
                // We don't need word wrap or line wrap
                pango_layout_set_width (layout, -1);
                pango_layout_get_pixel_extents (layout, NULL, &logical_rect);
                gnucash_sheet_set_text_bounds (sheet, &rect, x, y,
                                               width, height);
                x_offset = gnucash_sheet_get_text_offset (sheet, virt_loc,
                                                          rect.width,
                                                          logical_rect.width);
                pango_layout_xy_to_index (layout,
                                          PANGO_SCALE * (sheet->button_x - rect.x - x_offset),
                                          PANGO_SCALE * (height/2), &index, &trailing);
                g_object_unref (layout);
            }
            gnucash_sheet_set_position (sheet, index + trailing);
        }
    }
    // when a gui refresh is called, we end up here so only grab the focus
    // if the sheet is showing on the current plugin_page
    if (sheet->sheet_has_focus)
        gtk_widget_grab_focus (sheet->editing ? sheet->entry :
                                             GTK_WIDGET (sheet));
}


static gboolean
gnucash_sheet_cursor_move (GnucashSheet *sheet, VirtualLocation virt_loc)
{
    VirtualLocation old_virt_loc;
    gboolean changed_cells;
    Table *table;

    table = sheet->table;

    /* Get the old cursor position */
    gnucash_cursor_get_virt (GNUCASH_CURSOR(sheet->cursor), &old_virt_loc);

    /* Turn off the editing controls */
    gnucash_sheet_deactivate_cursor_cell (sheet);

    /* Do the move. This may result in table restructuring due to
     * commits, auto modes, etc. */
    gnc_table_wrap_verify_cursor_position (table, virt_loc);

    /* A complete reload can leave us with editing back on */
    if (sheet->editing)
        gnucash_sheet_deactivate_cursor_cell (sheet);

    /* Find out where we really landed. We have to get the new
     * physical position as well, as the table may have been
     * restructured. */
    gnucash_cursor_get_virt (GNUCASH_CURSOR(sheet->cursor), &virt_loc);

    gnucash_sheet_cursor_set (sheet, virt_loc);

    /* We should be at our new location now. Show it on screen and
     * configure the cursor. */
    gnucash_sheet_make_cell_visible (sheet, virt_loc);

    changed_cells = !virt_loc_equal (virt_loc, old_virt_loc);

    /* If we've changed cells, redraw the headers and sheet */
    if (changed_cells)
    {
        gnc_header_request_redraw (GNC_HEADER(sheet->header_item));
        gtk_widget_queue_draw (GTK_WIDGET(sheet));
    }

    /* Now turn on the editing controls. */
    gnucash_sheet_activate_cursor_cell (sheet, changed_cells);

    if (sheet->moved_cb)
        (sheet->moved_cb)(sheet, sheet->moved_cb_data);
    return changed_cells;
}


static gint
gnucash_sheet_y_pixel_to_block (GnucashSheet *sheet, int y)
{
    VirtualCellLocation vcell_loc = { 1, 0 };

    for (;
            vcell_loc.virt_row < sheet->num_virt_rows;
            vcell_loc.virt_row++)
    {
        SheetBlock *block;

        block = gnucash_sheet_get_block (sheet, vcell_loc);
        if (!block || !block->visible)
            continue;

        if (block->origin_y + block->style->dimensions->height > y)
            break;
    }
    return vcell_loc.virt_row;
}


void
gnucash_sheet_compute_visible_range (GnucashSheet *sheet)
{
    VirtualCellLocation vcell_loc;
    GtkAdjustment *adj;
    gint height;
    gint cy;
    gint top_block;

    g_return_if_fail (sheet != NULL);
    g_return_if_fail (GNUCASH_IS_SHEET(sheet));

    height = gtk_widget_get_height (GTK_WIDGET (sheet));

    adj = gtk_scrollable_get_vadjustment (GTK_SCROLLABLE(sheet));
    cy = gtk_adjustment_get_value (adj);

    top_block = gnucash_sheet_y_pixel_to_block (sheet, cy);

    sheet->num_visible_blocks = 0;
    sheet->num_visible_phys_rows = 0;

    for (vcell_loc.virt_row = top_block, vcell_loc.virt_col = 0;
         vcell_loc.virt_row < sheet->num_virt_rows;
         vcell_loc.virt_row++)
    {
        SheetBlock *block;

        block = gnucash_sheet_get_block (sheet, vcell_loc);
        if (!block->visible)
            continue;

        sheet->num_visible_blocks++;
        sheet->num_visible_phys_rows += block->style->nrows;

        if (block->origin_y - cy + block->style->dimensions->height
                >= height)
            break;
    }
}


static void
gnucash_sheet_show_row (GnucashSheet *sheet, gint virt_row)
{
    VirtualCellLocation vcell_loc = { virt_row, 0 };
    SheetBlock *block;
    GtkAdjustment *adj;
    gint block_height;
    gint height;
    gint cx, cy;
    gint x, y;

    g_return_if_fail (virt_row >= 0);
    g_return_if_fail (sheet != NULL);
    g_return_if_fail (GNUCASH_IS_SHEET(sheet));

    vcell_loc.virt_row = MAX (vcell_loc.virt_row, 1);
    vcell_loc.virt_row = MIN (vcell_loc.virt_row,
                              sheet->num_virt_rows - 1);

    adj = gtk_scrollable_get_hadjustment (GTK_SCROLLABLE(sheet));
    cx = gtk_adjustment_get_value (adj);
    adj = gtk_scrollable_get_vadjustment (GTK_SCROLLABLE(sheet));
    cy = gtk_adjustment_get_value (adj);
    x = cx;

    height = gtk_widget_get_height (GTK_WIDGET (sheet));

    block = gnucash_sheet_get_block (sheet, vcell_loc);
    if (!block)
        return;
    y = block->origin_y;
    block_height = block->style->dimensions->height;

    if ((cy <= y) && (cy + height >= y + block_height))
    {
        gnucash_sheet_compute_visible_range (sheet);
        return;
    }

    if (y > cy)
        y -= height - MIN (block_height, height);

    if ((sheet->height - y) < height)
        y = sheet->height - height;

    if (y < 0)
        y = 0;

    if (y != cy)
        gtk_adjustment_set_value (sheet->vadj, y);
    if (x != cx)
        gtk_adjustment_set_value (sheet->hadj, x);

    gnucash_sheet_compute_visible_range (sheet);
    gnucash_sheet_update_adjustments (sheet);
}


void
gnucash_sheet_make_cell_visible (GnucashSheet *sheet, VirtualLocation virt_loc)
{
    g_return_if_fail (sheet != NULL);
    g_return_if_fail (GNUCASH_IS_SHEET(sheet));

    if (!gnucash_sheet_cell_valid (sheet, virt_loc))
        return;

    gnucash_sheet_show_row (sheet, virt_loc.vcell_loc.virt_row);

    gnucash_sheet_update_adjustments (sheet);
}


void
gnucash_sheet_show_range (GnucashSheet *sheet,
                          VirtualCellLocation start_loc,
                          VirtualCellLocation end_loc)
{
    SheetBlock *start_block;
    SheetBlock *end_block;
    GtkAdjustment *adj;
    gint block_height;
    gint height;
    gint cx, cy;
    gint x, y;

    g_return_if_fail (sheet != NULL);
    g_return_if_fail (GNUCASH_IS_SHEET(sheet));

    start_loc.virt_row = MAX(start_loc.virt_row, 1);
    start_loc.virt_row = MIN(start_loc.virt_row,
                             sheet->num_virt_rows - 1);

    end_loc.virt_row = MAX(end_loc.virt_row, 1);
    end_loc.virt_row = MIN(end_loc.virt_row,
                           sheet->num_virt_rows - 1);

    adj = gtk_scrollable_get_hadjustment (GTK_SCROLLABLE(sheet));
    cx = gtk_adjustment_get_value (adj);
    adj = gtk_scrollable_get_vadjustment (GTK_SCROLLABLE(sheet));
    cy = gtk_adjustment_get_value (adj);
    x = cx;

    height = gtk_widget_get_height (GTK_WIDGET (sheet));

    start_block = gnucash_sheet_get_block (sheet, start_loc);
    end_block = gnucash_sheet_get_block (sheet, end_loc);
    if (!(start_block && end_block))
        return;

    y = start_block->origin_y;
    block_height = (end_block->origin_y +
                    end_block->style->dimensions->height) - y;

    if ((cy <= y) && (cy + height >= y + block_height))
    {
        gnucash_sheet_compute_visible_range (sheet);
        return;
    }

    if (y > cy)
        y -= height - MIN(block_height, height);

    if ((sheet->height - y) < height)
        y = sheet->height - height;

    if (y < 0)
        y = 0;

    if (y != cy)
        gtk_adjustment_set_value (sheet->vadj, y);
    if (x != cx)
        gtk_adjustment_set_value (sheet->hadj, x);

    gnucash_sheet_compute_visible_range (sheet);
    gnucash_sheet_update_adjustments (sheet);
}


void
gnucash_sheet_update_adjustments (GnucashSheet *sheet)
{
    gdouble h_value;
    gdouble v_value;
    gint page_width;
    gint page_height;
    gint content_width;
    gint content_height;

    g_return_if_fail (GNUCASH_IS_SHEET (sheet));
    g_return_if_fail (sheet->hadj != NULL && sheet->vadj != NULL);

    page_width = MAX (1, gtk_widget_get_width (GTK_WIDGET (sheet)));
    page_height = MAX (1, gtk_widget_get_height (GTK_WIDGET (sheet)));
    content_width = MAX (sheet->width, page_width);
    content_height = MAX (sheet->height, page_height);
    h_value = clamp_scrollable_value (gtk_adjustment_get_value (sheet->hadj),
                                      sheet->hadj);
    v_value = clamp_scrollable_value (gtk_adjustment_get_value (sheet->vadj),
                                      sheet->vadj);

    gtk_adjustment_configure (sheet->hadj, MIN (h_value, content_width - page_width),
                              0, content_width, 1, page_width * 0.9, page_width);
    gtk_adjustment_configure (sheet->vadj, MIN (v_value, content_height - page_height),
                              0, content_height, 1, page_height * 0.9, page_height);
    if (sheet->num_visible_blocks > 0)
        gtk_adjustment_set_step_increment
            (sheet->vadj, (gdouble) page_height / sheet->num_visible_blocks);
    else
        gtk_adjustment_set_step_increment (sheet->vadj, 1);
}





void
gnucash_sheet_redraw_all (GnucashSheet *sheet)
{
    g_return_if_fail (sheet != NULL);
    g_return_if_fail (GNUCASH_IS_SHEET(sheet));

    gtk_widget_queue_draw (GTK_WIDGET(sheet));

    g_signal_emit_by_name (sheet->reg, "redraw_all");
}

void
gnucash_sheet_redraw_help (GnucashSheet *sheet)
{
    g_return_if_fail (sheet != NULL);
    g_return_if_fail (GNUCASH_IS_SHEET(sheet));

    g_signal_emit_by_name (sheet->reg, "redraw_help");
}

void
gnucash_sheet_redraw_block (GnucashSheet *sheet,
                            VirtualCellLocation vcell_loc)
{
    g_return_if_fail (GNUCASH_IS_SHEET (sheet));

    if (!gnucash_sheet_get_block (sheet, vcell_loc))
        return;
    gtk_widget_queue_draw (GTK_WIDGET (sheet));
}
gboolean
gnucash_sheet_is_read_only (GnucashSheet *sheet)
{
    g_return_val_if_fail (sheet != NULL, TRUE);
    g_return_val_if_fail (GNUCASH_IS_SHEET(sheet), TRUE);
    return gnc_table_model_read_only (sheet->table->model);
}

void
gnucash_sheet_set_has_focus (GnucashSheet *sheet, gboolean has_focus)
{
    sheet->sheet_has_focus = has_focus;
}

typedef struct
{
    gint x;
    gint y;
} SheetOverlayPosition;

#define SHEET_OVERLAY_POSITION_KEY "gnucash-sheet-overlay-position"

static void
sheet_overlay_reposition_child (GnucashSheet *sheet, GtkWidget *child)
{
    SheetOverlayPosition *position;
    double x_offset;
    double y_offset;

    position = g_object_get_data (G_OBJECT (child),
                                  SHEET_OVERLAY_POSITION_KEY);
    if (!position)
        return;

    x_offset = sheet->hadj ? gtk_adjustment_get_value (sheet->hadj) : 0;
    y_offset = sheet->vadj ? gtk_adjustment_get_value (sheet->vadj) : 0;
    gtk_fixed_move (GTK_FIXED (sheet), child,
                    position->x - x_offset, position->y - y_offset);
}

static void
sheet_overlay_reposition_all (GnucashSheet *sheet)
{
    GtkWidget *child;

    for (child = gtk_widget_get_first_child (GTK_WIDGET (sheet)); child;
         child = gtk_widget_get_next_sibling (child))
        sheet_overlay_reposition_child (sheet, child);
}

void
gnucash_sheet_overlay_put (GnucashSheet *sheet, GtkWidget *child,
                           gint x, gint y)
{
    SheetOverlayPosition *position;

    g_return_if_fail (GNUCASH_IS_SHEET (sheet));
    g_return_if_fail (GTK_IS_WIDGET (child));
    g_return_if_fail (gtk_widget_get_parent (child) == NULL);

    position = g_new (SheetOverlayPosition, 1);
    position->x = x;
    position->y = y;
    g_object_set_data_full (G_OBJECT (child), SHEET_OVERLAY_POSITION_KEY,
                            position, g_free);
    gtk_fixed_put (GTK_FIXED (sheet), child, x, y);
    sheet_overlay_reposition_child (sheet, child);
}

void
gnucash_sheet_overlay_move (GnucashSheet *sheet, GtkWidget *child,
                            gint x, gint y)
{
    SheetOverlayPosition *position;

    g_return_if_fail (GNUCASH_IS_SHEET (sheet));
    g_return_if_fail (GTK_IS_WIDGET (child));
    g_return_if_fail (gtk_widget_get_parent (child) == GTK_WIDGET (sheet));

    position = g_object_get_data (G_OBJECT (child),
                                  SHEET_OVERLAY_POSITION_KEY);
    if (!position)
    {
        position = g_new (SheetOverlayPosition, 1);
        g_object_set_data_full (G_OBJECT (child), SHEET_OVERLAY_POSITION_KEY,
                                position, g_free);
    }
    position->x = x;
    position->y = y;
    sheet_overlay_reposition_child (sheet, child);
}

void
gnucash_sheet_overlay_remove (GnucashSheet *sheet, GtkWidget *child)
{
    g_return_if_fail (GNUCASH_IS_SHEET (sheet));
    g_return_if_fail (GTK_IS_WIDGET (child));

    if (gtk_widget_get_parent (child) == GTK_WIDGET (sheet))
        gtk_fixed_remove (GTK_FIXED (sheet), child);
}

static void
gnucash_sheet_adjustment_changed (G_GNUC_UNUSED GtkAdjustment *adjustment,
                                  GnucashSheet *sheet)
{
    if (!sheet->table || !sheet->blocks)
        return;

    sheet_overlay_reposition_all (sheet);
    gnucash_sheet_compute_visible_range (sheet);
    gtk_widget_queue_draw (GTK_WIDGET (sheet));
}

static void
gnucash_sheet_set_adjustment (GnucashSheet *sheet,
                              GtkAdjustment **target,
                              gulong *handler,
                              GtkAdjustment *adjustment,
                              const char *property_name)
{
    GtkAdjustment *replacement = adjustment;

    if (!replacement)
        replacement = gtk_adjustment_new (0.0, 0.0, 0.0, 0.0, 0.0, 0.0);
    else
        g_object_ref (replacement);

    if (*target == replacement)
    {
        g_object_unref (replacement);
        return;
    }

    if (*target && *handler)
        g_signal_handler_disconnect (*target, *handler);
    *handler = 0;
    g_clear_object (target);
    *target = replacement;
    *handler = g_signal_connect (*target, "value-changed",
                                 G_CALLBACK (gnucash_sheet_adjustment_changed),
                                 sheet);
    g_object_notify (G_OBJECT (sheet), property_name);
}

static void
gnucash_sheet_get_property (GObject *object, guint property_id,
                            GValue *value, GParamSpec *pspec)
{
    GnucashSheet *sheet = GNUCASH_SHEET (object);

    switch (property_id)
    {
    case PROP_HADJUSTMENT:
        g_value_set_object (value, sheet->hadj);
        break;
    case PROP_VADJUSTMENT:
        g_value_set_object (value, sheet->vadj);
        break;
    case PROP_HSCROLL_POLICY:
        g_value_set_enum (value, sheet->hscroll_policy);
        break;
    case PROP_VSCROLL_POLICY:
        g_value_set_enum (value, sheet->vscroll_policy);
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
        break;
    }
}

static void
gnucash_sheet_set_property (GObject *object, guint property_id,
                            const GValue *value, GParamSpec *pspec)
{
    GnucashSheet *sheet = GNUCASH_SHEET (object);

    switch (property_id)
    {
    case PROP_HADJUSTMENT:
        gnucash_sheet_set_adjustment (sheet, &sheet->hadj,
                                      &sheet->hadj_handler,
                                      g_value_get_object (value), "hadjustment");
        break;
    case PROP_VADJUSTMENT:
        gnucash_sheet_set_adjustment (sheet, &sheet->vadj,
                                      &sheet->vadj_handler,
                                      g_value_get_object (value), "vadjustment");
        break;
    case PROP_HSCROLL_POLICY:
        sheet->hscroll_policy = g_value_get_enum (value);
        g_object_notify (object, "hscroll-policy");
        break;
    case PROP_VSCROLL_POLICY:
        sheet->vscroll_policy = g_value_get_enum (value);
        g_object_notify (object, "vscroll-policy");
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
        break;
    }
}

static void
gnucash_sheet_scrollable_init (G_GNUC_UNUSED GtkScrollableInterface *iface)
{
}
static void
gnucash_sheet_finalize (GObject *object)
{
    GnucashSheet *sheet;

    sheet = GNUCASH_SHEET(object);

    if (sheet->blocks)
    {
        g_table_resize (sheet->blocks, 0, 0);
        g_table_destroy (sheet->blocks);
        sheet->blocks = NULL;
    }

    gnucash_sheet_clear_styles (sheet);
    g_clear_pointer (&sheet->cursor_styles, g_hash_table_destroy);
    g_clear_pointer (&sheet->dimensions_hash_table, g_hash_table_destroy);
    g_clear_object (&sheet->cursor);
    g_clear_object (&sheet->popup);
    if (sheet->hadj && sheet->hadj_handler)
        g_signal_handler_disconnect (sheet->hadj, sheet->hadj_handler);
    if (sheet->vadj && sheet->vadj_handler)
        g_signal_handler_disconnect (sheet->vadj, sheet->vadj_handler);
    sheet->hadj_handler = 0;
    sheet->vadj_handler = 0;
    g_clear_object (&sheet->hadj);
    g_clear_object (&sheet->vadj);

    G_OBJECT_CLASS (gnucash_sheet_parent_class)->finalize (object);
}


static GnucashSheet *
gnucash_sheet_create (Table *table)
{
    GnucashSheet *sheet;

    ENTER("table=%p", table);

    sheet = g_object_new (GNUCASH_TYPE_SHEET, NULL);
    sheet->table = table;
    sheet->entry = NULL;

    LEAVE("%p", sheet);
    return sheet;
}

const char *
gnucash_sheet_modify_current_cell (GnucashSheet *sheet, const gchar *new_text)
{
    GtkEditable *editable;
    Table *table = sheet->table;
    VirtualLocation virt_loc;
    int new_text_len = 0;
    const char *retval;
    int cursor_position, start_sel, end_sel;

    gnucash_cursor_get_virt (GNUCASH_CURSOR(sheet->cursor), &virt_loc);

    if (!gnc_table_virtual_loc_valid (table, virt_loc, TRUE))
        return NULL;

    if (gnc_table_model_read_only (table->model))
        return NULL;

    editable = GTK_EDITABLE(sheet->entry);

    cursor_position = gtk_editable_get_position (editable);
    gtk_editable_get_selection_bounds (editable, &start_sel, &end_sel);

    if (new_text)
         new_text_len = strlen (new_text);

    retval = gnc_table_modify_update (table, virt_loc,
                                      new_text, new_text_len,
                                      new_text, new_text_len,
                                      &cursor_position,
                                      &start_sel, &end_sel,
                                      NULL);


    if (retval)
    {
        DEBUG("%s", retval ? retval : "nothing");
        gnucash_sheet_set_entry_value (sheet, retval);
        gnucash_sheet_set_position_and_selection (sheet, cursor_position,
                                                  start_sel, end_sel);
    }
    return retval;
}

typedef struct
{
    GtkEditable *editable;
    int start_sel;
    int end_sel;

} select_info;

static gboolean
gnucash_sheet_direct_keyval (GnucashSheet *sheet, guint keyval,
                             GdkModifierType state)
{
    GtkEditable *editable;
    VirtualLocation virt_loc;
    gboolean result;
    char *new_text = NULL;
    int cursor_position, start_sel, end_sel;
    int new_position, new_start, new_end;
    GncRegisterInput input;

    gnc_register_input_from_keyval (&input, keyval, state);
    gnucash_cursor_get_virt (GNUCASH_CURSOR (sheet->cursor), &virt_loc);
    if (!gnc_table_virtual_loc_valid (sheet->table, virt_loc, TRUE) ||
        gnc_table_model_read_only (sheet->table->model))
        return FALSE;

    editable = GTK_EDITABLE (sheet->entry);
    cursor_position = gtk_editable_get_position (editable);
    gtk_editable_get_selection_bounds (editable, &start_sel, &end_sel);
    new_position = cursor_position;
    new_start = start_sel;
    new_end = end_sel;
    result = gnc_table_direct_update (sheet->table, virt_loc, &new_text,
                                      &new_position, &new_start, &new_end,
                                      &input);
    if (result)
    {
        if (new_text)
            gnucash_sheet_set_entry_value (sheet, new_text);
        gnucash_sheet_set_position_and_selection (sheet, new_position,
                                                  new_start, new_end);
    }
    g_free (new_text);
    return result;
}
static inline void
normalize_selection_bounds (int *pos, int *bound, int length)
{
    *bound = *bound < 0 ? length : *bound;
    *pos = *pos < 0 ? length : *pos;

    if (*pos > *bound)
    {
        int temp = *pos;
        *pos = *bound;
        *bound = temp;
    }
}

static inline char*
insert_text (const char* old_text, const char* new_text, int pos, int bound)
{
    int old_len = g_utf8_strlen (old_text, -1);
    char* begin = g_utf8_substring (old_text, 0, pos);
    char* end = g_utf8_substring (old_text, bound, old_len);
    char *retval = g_strdup_printf ("%s%s%s", begin, new_text, end);
    g_free (begin);
    g_free (end);
    return retval;
}

static char*
make_new_text (GnucashSheet *sheet, const char* new_text, int *position)
{
    GtkEditable* editable = (GTK_EDITABLE(sheet->entry));
    int pos, bound;
    const char* old_text = gtk_editable_get_text (GTK_EDITABLE (sheet->entry));
    int old_length = g_utf8_strlen (old_text, -1);
    int insert_length = g_utf8_strlen (new_text, -1);

    if (!old_text || old_length == 0)
    {
        *position = insert_length;
        return g_strdup (new_text);
    }

    gtk_editable_get_selection_bounds (editable, &bound, &pos);
    normalize_selection_bounds (&pos, &bound, old_length);

    if (*position != pos)
        bound = pos = *position;

    if (pos == 0 && bound == old_length) // Full replacement
    {
        *position = insert_length;
        return g_strdup (new_text);
    }

    if (pos == bound)
    {
        if (pos == 0) //prepend
        {
            *position = insert_length;
            return g_strdup_printf ("%s%s", new_text, old_text);
        }
        else if (pos == old_length) //append
        {
            *position = old_length + insert_length;
            return g_strdup_printf ("%s%s", old_text, new_text);
        }
    }

    *position = pos + insert_length;
    return insert_text (old_text, new_text, pos, bound);
}

static void
gnucash_sheet_insert_cb (GtkEditable *editable,
                         const gchar *insert_text,
                         const gint insert_text_len,
                         gint *position,
                         GnucashSheet *sheet)
{

    Table *table = sheet->table;
    VirtualLocation virt_loc;
    char *new_text = NULL;
    glong new_text_len = 0;
    const char *retval;
    int start_sel = 0, end_sel = 0;
    int old_position = *position;
    const char* old_text = gtk_editable_get_text (GTK_EDITABLE (sheet->entry));

    g_assert (GTK_WIDGET(editable) == sheet->entry);
    if (gnc_table_control_input_suspended (table->control))
    {
        g_signal_stop_emission_by_name (G_OBJECT (sheet->entry), "insert_text");
        return;
    }
    if (sheet->input_cancelled)
    {
        g_signal_stop_emission_by_name (G_OBJECT(sheet->entry),
                                        "insert_text");
        return;
    }

    if (insert_text_len <= 0)
        return;

    gnucash_cursor_get_virt (GNUCASH_CURSOR(sheet->cursor), &virt_loc);

    if (!gnc_table_virtual_loc_valid (table, virt_loc, FALSE))
        return;

    if (gnc_table_model_read_only (table->model))
        return;

    new_text = make_new_text (sheet, insert_text, position);
    new_text_len = strlen (new_text);


    retval = gnc_table_modify_update (table, virt_loc,
                                      insert_text, insert_text_len,
                                      new_text, new_text_len,
                                      position, &start_sel, &end_sel,
                                      &sheet->input_cancelled);

    if (retval)
    {
        /* After the insert event the GtkEntry may handle signals from the
         * IMContext that would reset the selection, and we may need to keep it
         * so save it in the sheet values.
         */
        DEBUG("%s, got %s", new_text, retval);
        gnucash_sheet_set_position_and_selection (sheet, *position, start_sel,
                                                  end_sel);

        if ((strcmp (retval, new_text) != 0) || (*position != old_position))
        {
            gnucash_sheet_set_entry_value (sheet, retval);
            g_signal_stop_emission_by_name (G_OBJECT(sheet->entry),
                                            "insert_text");
        }
    }
    else if (retval == NULL)
    {
        retval = old_text;

        /* reset IMContext if disallowed chars */
        gtk_entry_reset_im_context (GTK_ENTRY(sheet->entry));
        /* the entry was disallowed, so we stop the insert signal */
        g_signal_stop_emission_by_name (G_OBJECT(sheet->entry),
                                        "insert_text");
    }

    g_free (new_text);
}

static char*
delete_text (GnucashSheet *sheet, int pos, int bound)
{
    const char* old_text = gtk_editable_get_text (GTK_EDITABLE (sheet->entry));
    int old_length = g_utf8_strlen (old_text, -1);
    char* begin, *end;
    char *retval = NULL;

    normalize_selection_bounds (&pos, &bound, old_length);
    if (pos == bound)
        return g_strdup (old_text); // Nothing to delete.

    if (pos == 0 && bound == old_length) // Full delete
        return g_strdup ("");

    if (bound == old_length)
        return g_utf8_substring (old_text, 0, pos);

    if (pos == 0)
        return g_utf8_substring (old_text, bound, old_length);

    begin = g_utf8_substring (old_text, 0, pos);
    end = g_utf8_substring (old_text, bound, old_length);
    retval = g_strdup_printf ("%s%s", begin, end);
    g_free (begin);
    g_free (end);
    return retval;
}

static void
gnucash_sheet_delete_cb (GtkWidget *widget,
                         const gint start_pos,
                         const gint end_pos,
                         GnucashSheet *sheet)
{
    GtkEditable *editable;
    Table *table = sheet->table;
    VirtualLocation virt_loc;
    char *new_text = NULL;
    glong new_text_len;
    const char *retval;
    int cursor_position = start_pos;
    int start_sel, end_sel;

    if (gnc_table_control_input_suspended (table->control))

    {

        g_signal_stop_emission_by_name (G_OBJECT (sheet->entry), "delete_text");

        return;

    }


    gnucash_cursor_get_virt (GNUCASH_CURSOR(sheet->cursor), &virt_loc);

    if (!gnc_table_virtual_loc_valid (table, virt_loc, FALSE))
        return;

    if (gnc_table_model_read_only (table->model))
        return;

    new_text = delete_text (sheet, start_pos, end_pos);
    new_text_len = strlen (new_text);
    editable = GTK_EDITABLE(sheet->entry);
    gtk_editable_get_selection_bounds (editable, &start_sel, &end_sel);
    retval = gnc_table_modify_update (table, virt_loc,
                                      NULL, 0,
                                      new_text, new_text_len,
                                      &cursor_position,
                                      &start_sel, &end_sel,
                                      &sheet->input_cancelled);

    if (retval)
        gnucash_sheet_set_entry_value (sheet, retval);

    g_signal_stop_emission_by_name (G_OBJECT(sheet->entry), "delete_text");

    DEBUG("%s", retval ? retval : "nothing");
    gnucash_sheet_set_position_and_selection (sheet, cursor_position,
                                              start_sel, end_sel);
}

static void
gnucash_sheet_snapshot (GtkWidget *widget, GtkSnapshot *snapshot)
{
    GnucashSheet *sheet = GNUCASH_SHEET (widget);
    GtkAllocation allocation = { 0, 0, gtk_widget_get_width (widget),
                                 gtk_widget_get_height (widget) };
    graphene_rect_t bounds;
    cairo_t *cr;

    graphene_rect_init (&bounds, 0, 0, allocation.width, allocation.height);
    gtk_snapshot_append_color (snapshot, &gn_white, &bounds);
    cr = gtk_snapshot_append_cairo (snapshot, &bounds);
    if (sheet->table && sheet->blocks)
    {
        gnucash_sheet_draw_internal (sheet, cr, &allocation);
        gnucash_sheet_draw_cursor (sheet->cursor, cr);
    }
    cairo_destroy (cr);

    GTK_WIDGET_CLASS (gnucash_sheet_parent_class)->snapshot (widget, snapshot);
}
static void
gnucash_sheet_measure (GtkWidget *widget, GtkOrientation orientation,
                       G_GNUC_UNUSED int for_size, int *minimum, int *natural,
                       int *minimum_baseline, int *natural_baseline)
{
    GnucashSheet *sheet = GNUCASH_SHEET (widget);
    gint size = orientation == GTK_ORIENTATION_HORIZONTAL ? DEFAULT_SHEET_WIDTH :
                                                            DEFAULT_SHEET_HEIGHT;

    if (orientation == GTK_ORIENTATION_VERTICAL && sheet->table)
    {
        SheetBlockStyle *style = gnucash_sheet_get_style_from_cursor
            (sheet, CURSOR_HEADER);
        CellDimensions *dimensions = style ?
            gnucash_style_get_cell_dimensions (style, 0, 0) : NULL;
        if (dimensions)
            size = dimensions->pixel_height * DEFAULT_SHEET_INITIAL_ROWS;
    }

    *minimum = size;
    *natural = size;
    if (minimum_baseline)
        *minimum_baseline = -1;
    if (natural_baseline)
        *natural_baseline = -1;
}

static void
gnucash_sheet_size_allocate (GtkWidget *widget, gint width, gint height,
                             gint baseline)
{
    GnucashSheet *sheet = GNUCASH_SHEET (widget);

    GTK_WIDGET_CLASS (gnucash_sheet_parent_class)->size_allocate (widget, width,
                                                                    height, baseline);

    if (height == sheet->window_height && width == sheet->window_width)
        return;

    if (width != sheet->window_width && sheet->table)
    {
        gnucash_sheet_styles_set_dimensions (sheet, width);
        gnucash_sheet_recompute_block_offsets (sheet);
    }

    sheet->window_height = height;
    sheet->window_width = width;

    if (!sheet->table || !sheet->header_item || !sheet->item_editor)
        return;

    gnucash_cursor_configure (GNUCASH_CURSOR (sheet->cursor));
    gnc_header_reconfigure (GNC_HEADER (sheet->header_item));
    gnucash_sheet_set_scroll_region (sheet);
    gnc_item_edit_configure (GNC_ITEM_EDIT (sheet->item_editor));
    gnucash_sheet_update_adjustments (sheet);

    if (gnucash_sheet_cell_valid (sheet, sheet->table->current_cursor_loc))
        gnucash_sheet_show_row (sheet,
                                sheet->table->current_cursor_loc.vcell_loc.virt_row);
    gnc_header_request_redraw (GNC_HEADER (sheet->header_item));
}

static void
gnucash_sheet_focus_enter_cb (G_GNUC_UNUSED GtkEventControllerFocus *controller,
                              GnucashSheet *sheet)
{
    gnc_item_edit_focus_in (GNC_ITEM_EDIT (sheet->item_editor));
}

static void
gnucash_sheet_focus_leave_cb (G_GNUC_UNUSED GtkEventControllerFocus *controller,
                              GnucashSheet *sheet)
{
    gnc_item_edit_focus_out (GNC_ITEM_EDIT (sheet->item_editor));
}
static void
gnucash_sheet_start_editing_at_cursor (GnucashSheet *sheet)
{
    const char *text;
    VirtualLocation virt_loc;

    g_return_if_fail (sheet != NULL);
    g_return_if_fail (GNUCASH_IS_SHEET(sheet));

    gnucash_cursor_get_virt (GNUCASH_CURSOR(sheet->cursor), &virt_loc);

    text = gnc_table_get_entry (sheet->table, virt_loc);

    gnc_item_edit_configure (GNC_ITEM_EDIT(sheet->item_editor));
    gtk_widget_set_visible (GTK_WIDGET (sheet->item_editor), TRUE);

    gtk_editable_set_text (GTK_EDITABLE (sheet->entry), text);

    sheet->editing = TRUE;

    /* set up the signals */
    sheet->insert_signal =
        g_signal_connect (G_OBJECT(sheet->entry), "insert_text",
                          G_CALLBACK(gnucash_sheet_insert_cb), sheet);
    sheet->delete_signal =
        g_signal_connect (G_OBJECT(sheet->entry), "delete_text",
                          G_CALLBACK(gnucash_sheet_delete_cb), sheet);
}

static float
clamp_scrollable_value (float value, GtkAdjustment *adjustment)
{
    float lower = gtk_adjustment_get_lower (adjustment);
    float upper = gtk_adjustment_get_upper (adjustment);
    float page_size = gtk_adjustment_get_page_size (adjustment);

    return CLAMP (value, lower, MAX (lower, upper - page_size));
}

static gboolean
gnucash_sheet_handle_scroll (GnucashSheet *sheet, double dx, double dy)
{
    float h_value;
    float v_value;
    GtkAdjustment *vadj = sheet->vadj;

    h_value = gtk_adjustment_get_value (sheet->hadj) + dx;
    h_value = clamp_scrollable_value (h_value, sheet->hadj);
    gtk_adjustment_set_value (sheet->hadj, h_value);

    v_value = gtk_adjustment_get_value (vadj);
#if defined MAC_INTEGRATION
    v_value += dy;
#else
    if (dy > 0)
        v_value += gtk_adjustment_get_step_increment (vadj);
    else if (dy < 0)
        v_value -= gtk_adjustment_get_step_increment (vadj);
#endif
    v_value = clamp_scrollable_value (v_value, vadj);
    gtk_adjustment_set_value (vadj, v_value);
    return dx != 0 || dy != 0;
}

static void
gnucash_sheet_show_popup_at (GnucashSheet *sheet, double x, double y)
{
    GdkRectangle pointing_to = { (gint) x, (gint) y, 1, 1 };

    if (!sheet->popup || !GTK_IS_POPOVER (sheet->popup))
        return;

    gtk_popover_set_pointing_to (GTK_POPOVER (sheet->popup), &pointing_to);
    gtk_popover_popup (GTK_POPOVER (sheet->popup));
}

static gboolean
gnucash_sheet_handle_pointer_press (GnucashSheet *sheet, guint button,
                                    gint n_press, double x, double y)
{
    VirtualCell *vcell;
    VirtualLocation cur_virt_loc;
    VirtualLocation new_virt_loc;
    gboolean abort_move;
    gboolean primary_button;
    gboolean do_popup;
    double content_x;
    double content_y;

    if (gnc_table_control_input_suspended (sheet->table->control))

        return TRUE;


    if (n_press != 1 || (sheet->button && sheet->button != button))
        return FALSE;

    if (!gtk_widget_has_focus (GTK_WIDGET (sheet)))
        gtk_widget_grab_focus (GTK_WIDGET (sheet));

    primary_button = button == GDK_BUTTON_PRIMARY;
    do_popup = button == GDK_BUTTON_SECONDARY && sheet->popup;
    if (button == GDK_BUTTON_MIDDLE)
    {
        gnc_item_edit_paste_clipboard (GNC_ITEM_EDIT (sheet->item_editor));
        return TRUE;
    }
    if (!primary_button && !do_popup)
        return FALSE;

    content_x = x + gtk_adjustment_get_value (sheet->hadj);
    content_y = y + gtk_adjustment_get_value (sheet->vadj);
    sheet->button = primary_button ? button : 0;
    sheet->button_x = content_x;
    sheet->button_y = content_y;

    if (!gnucash_sheet_find_loc_by_pixel (sheet, content_x, content_y,
                                          &new_virt_loc))
        return TRUE;

    vcell = gnc_table_get_virtual_cell (sheet->table, new_virt_loc.vcell_loc);
    if (!vcell)
        return TRUE;

    gnucash_cursor_get_virt (GNUCASH_CURSOR (sheet->cursor), &cur_virt_loc);
    if (virt_loc_equal (new_virt_loc, cur_virt_loc) && sheet->editing && do_popup)
    {
        gnucash_sheet_show_popup_at (sheet, x, y);
        return TRUE;
    }

    abort_move = gnc_table_traverse_update (sheet->table, cur_virt_loc,
                                            GNC_TABLE_TRAVERSE_POINTER,
                                            &new_virt_loc);
    if (abort_move)
        return TRUE;

    gnucash_sheet_cursor_move (sheet, new_virt_loc);
    if (g_strcmp0 (gnc_table_get_cell_name (sheet->table, new_virt_loc),
                   DOCLINK_CELL) == 0 && sheet->open_doclink_cb)
        sheet->open_doclink_cb (sheet->open_doclink_cb_data, NULL);

    if (do_popup)
        gnucash_sheet_show_popup_at (sheet, x, y);

    return TRUE;
}

static void
gnucash_sheet_click_pressed_cb (GtkGestureClick *gesture, gint n_press,
                                double x, double y, GnucashSheet *sheet)
{
    guint button = gtk_gesture_single_get_current_button (GTK_GESTURE_SINGLE (gesture));

    if (gnucash_sheet_handle_pointer_press (sheet, button, n_press, x, y))
        gtk_gesture_set_state (GTK_GESTURE (gesture),
                               GTK_EVENT_SEQUENCE_CLAIMED);
}

static void
gnucash_sheet_click_released_cb (GtkGestureClick *gesture,
                                 G_GNUC_UNUSED gint n_press,
                                 G_GNUC_UNUSED double x,
                                 G_GNUC_UNUSED double y,
                                 GnucashSheet *sheet)
{
    guint button = gtk_gesture_single_get_current_button (GTK_GESTURE_SINGLE (gesture));

    if (sheet->button == button)
        sheet->button = 0;
}

static gboolean
gnucash_sheet_scroll_cb (G_GNUC_UNUSED GtkEventControllerScroll *controller,
                         double dx, double dy, GnucashSheet *sheet)
{
    return gnucash_sheet_handle_scroll (sheet, dx, dy);
}
void
gnucash_sheet_refresh_from_prefs (GnucashSheet *sheet)
{
    static const char *classes[] =
    {
        "gnc-class-register-foreground", "gnc-class-register-cursor",
        "gnc-class-user-register-cursor", "view", NULL
    };
    GncItemEdit *item_edit;

    g_return_if_fail (GNUCASH_IS_SHEET (sheet));
    sheet->use_gnc_color_theme = gnc_prefs_get_bool
        (GNC_PREFS_GROUP_GENERAL_REGISTER, GNC_PREF_USE_GNUCASH_COLOR_THEME);
    sheet->use_horizontal_lines = gnc_prefs_get_bool
        (GNC_PREFS_GROUP_GENERAL_REGISTER, GNC_PREF_DRAW_HOR_LINES);
    sheet->use_vertical_lines = gnc_prefs_get_bool
        (GNC_PREFS_GROUP_GENERAL_REGISTER, GNC_PREF_DRAW_VERT_LINES);

    item_edit = GNC_ITEM_EDIT (sheet->item_editor);
    for (guint i = 0; classes[i]; i++)
    {
        gtk_widget_remove_css_class (item_edit->editor, classes[i]);
        gtk_widget_remove_css_class (GTK_WIDGET (item_edit), classes[i]);
    }
    gtk_widget_add_css_class (item_edit->editor,
                              sheet->use_gnc_color_theme ?
                              "gnc-class-register-foreground" : "view");
    gtk_widget_add_css_class (item_edit->editor,
                              sheet->use_gnc_color_theme ?
                              "gnc-class-register-cursor" :
                              "gnc-class-user-register-cursor");
    gtk_widget_add_css_class (GTK_WIDGET (item_edit),
                              sheet->use_gnc_color_theme ?
                              "gnc-class-register-cursor" :
                              "gnc-class-user-register-cursor");
}
static gboolean
gnucash_sheet_clipboard_key (GnucashSheet *sheet, guint keyval,
                             GdkModifierType state)
{
    GncItemEdit *item_edit = GNC_ITEM_EDIT (sheet->item_editor);

    switch (keyval)
    {
    case GDK_KEY_C:
    case GDK_KEY_c:
        if (state & gtk_accelerator_get_default_mod_mask ())
        {
            gnc_item_edit_copy_clipboard (item_edit);
            return TRUE;
        }
        break;
    case GDK_KEY_X:
    case GDK_KEY_x:
        if (state & gtk_accelerator_get_default_mod_mask ())
        {
            gnc_item_edit_cut_clipboard (item_edit);
            return TRUE;
        }
        break;
    case GDK_KEY_V:
    case GDK_KEY_v:
        if (state & gtk_accelerator_get_default_mod_mask ())
        {
            gnc_item_edit_paste_clipboard (item_edit);
            return TRUE;
        }
        break;
    case GDK_KEY_Insert:
        if (state & GDK_SHIFT_MASK)
        {
            gnc_item_edit_paste_clipboard (item_edit);
            return TRUE;
        }
        if (state & gtk_accelerator_get_default_mod_mask ())
        {
            gnc_item_edit_copy_clipboard (item_edit);
            return TRUE;
        }
        break;
    }
    return FALSE;
}

static void
gnucash_sheet_need_horizontal_scroll (GnucashSheet *sheet,
                                      VirtualLocation *new_virt_loc)
{
    gint hscroll_val;
    gint cell_width = 0;
    gint offset;

    if (sheet->window_width == sheet->width)
        return;

    hscroll_val = (gint) gtk_adjustment_get_value (sheet->hadj);
    offset = gnc_header_get_cell_offset (GNC_HEADER (sheet->header_item),
                                         new_virt_loc->phys_col_offset,
                                         &cell_width);
    if ((offset + cell_width) > sheet->window_width || offset < hscroll_val)
        gtk_adjustment_set_value (sheet->hadj, offset);
}

typedef struct
{
    GWeakRef sheet;
    VirtualLocation virt_loc;
} GnucashSheetPopupReplay;

static void
gnucash_sheet_popup_replay_destroy (gpointer user_data)
{
    GnucashSheetPopupReplay *replay = user_data;

    g_weak_ref_clear (&replay->sheet);
    g_free (replay);
}

static void
gnucash_sheet_popup_replay (Table *table, gpointer user_data)
{
    GnucashSheetPopupReplay *replay = user_data;
    GnucashSheet *sheet = GNUCASH_SHEET (g_weak_ref_get (&replay->sheet));

    if (sheet && sheet->table == table &&
        virt_loc_equal (table->current_cursor_loc, replay->virt_loc) &&
        sheet->item_editor && GNC_IS_ITEM_EDIT (sheet->item_editor))
        gnc_item_edit_show_popup (GNC_ITEM_EDIT (sheet->item_editor));
    g_clear_object (&sheet);
}

static void
gnucash_sheet_defer_popup_replay (GnucashSheet *sheet,
                                  VirtualLocation virt_loc)
{
    GnucashSheetPopupReplay *replay = g_new0 (GnucashSheetPopupReplay, 1);

    replay->virt_loc = virt_loc;
    g_weak_ref_init (&replay->sheet, sheet);
    gnc_table_confirm_change_set_replay (sheet->table,
                                         gnucash_sheet_popup_replay, replay,
                                         gnucash_sheet_popup_replay_destroy);
}

static gboolean
process_motion_keys (GnucashSheet *sheet, guint keyval,
                     GdkModifierType state, gboolean *pass_on,
                     gncTableTraversalDir *direction,
                     VirtualLocation *new_virt_loc)
{
    int distance;
    VirtualLocation cur_virt_loc = *new_virt_loc;

    switch (keyval)
    {
    case GDK_KEY_Return:
    case GDK_KEY_KP_Enter:
        g_signal_emit_by_name (sheet->reg, "activate_cursor");
        sheet->pos = sheet->bound;
        return TRUE;
    case GDK_KEY_Tab:
    case GDK_KEY_ISO_Left_Tab:
        if (state & GDK_SHIFT_MASK)
        {
            *direction = GNC_TABLE_TRAVERSE_LEFT;
            gnc_table_move_tab (sheet->table, new_virt_loc, FALSE);
        }
        else
        {
            *direction = GNC_TABLE_TRAVERSE_RIGHT;
            gnc_table_move_tab (sheet->table, new_virt_loc, TRUE);
        }
        break;
    case GDK_KEY_KP_Page_Up:
    case GDK_KEY_Page_Up:
        *direction = GNC_TABLE_TRAVERSE_UP;
        new_virt_loc->phys_col_offset = 0;
        if (state & GDK_SHIFT_MASK)
            new_virt_loc->vcell_loc.virt_row = 1;
        else
        {
            distance = sheet->num_visible_phys_rows - 1;
            gnc_table_move_vertical_position (sheet->table, new_virt_loc,
                                              -distance);
        }
        break;
    case GDK_KEY_KP_Page_Down:
    case GDK_KEY_Page_Down:
        *direction = GNC_TABLE_TRAVERSE_DOWN;
        new_virt_loc->phys_col_offset = 0;
        if (state & GDK_SHIFT_MASK)
            new_virt_loc->vcell_loc.virt_row = sheet->num_virt_rows - 1;
        else
        {
            distance = sheet->num_visible_phys_rows - 1;
            gnc_table_move_vertical_position (sheet->table, new_virt_loc,
                                              distance);
        }
        break;
    case GDK_KEY_KP_Up:
    case GDK_KEY_Up:
        *direction = GNC_TABLE_TRAVERSE_UP;
        gnc_table_move_vertical_position (sheet->table, new_virt_loc, -1);
        break;
    case GDK_KEY_KP_Down:
    case GDK_KEY_Down:
    case GDK_KEY_Menu:
        if (keyval == GDK_KEY_Menu ||
            (state & gtk_accelerator_get_default_mod_mask ()))
        {
            GncItemEdit *item_edit = GNC_ITEM_EDIT (sheet->item_editor);

            GncTableConfirmResult confirmation =
                gnc_table_confirm_change (sheet->table, cur_virt_loc);

            if (confirmation == GNC_TABLE_CONFIRM_ACCEPT)
                gnc_item_edit_show_popup (item_edit);
            else if (confirmation == GNC_TABLE_CONFIRM_DEFERRED)
                gnucash_sheet_defer_popup_replay (sheet, cur_virt_loc);
            sheet->pos = sheet->bound;
            return TRUE;
        }
        *direction = GNC_TABLE_TRAVERSE_DOWN;
        gnc_table_move_vertical_position (sheet->table, new_virt_loc, 1);
        break;
    case GDK_KEY_KP_Right:
    case GDK_KEY_Right:
    case GDK_KEY_KP_Left:
    case GDK_KEY_Left:
    case GDK_KEY_Home:
    case GDK_KEY_End:
        sheet->pos = sheet->bound;
        *pass_on = TRUE;
        break;
    default:
        if (gnucash_sheet_clipboard_key (sheet, keyval, state))
        {
            sheet->pos = sheet->bound;
            return TRUE;
        }
        *pass_on = TRUE;
        break;
    }

    gnucash_sheet_need_horizontal_scroll (sheet, new_virt_loc);
    return FALSE;
}

static gboolean
sheet_keyval_is_modifier (guint keyval)
{
    switch (keyval)
    {
    case GDK_KEY_Shift_L:
    case GDK_KEY_Shift_R:
    case GDK_KEY_Control_L:
    case GDK_KEY_Control_R:
    case GDK_KEY_Alt_L:
    case GDK_KEY_Alt_R:
    case GDK_KEY_Meta_L:
    case GDK_KEY_Meta_R:
    case GDK_KEY_Super_L:
    case GDK_KEY_Super_R:
        return TRUE;
    default:
        return FALSE;
    }
}
gboolean
gnucash_sheet_handle_key (GnucashSheet *sheet, guint keyval,
                          G_GNUC_UNUSED guint keycode,
                          GdkModifierType state)
{
    gboolean pass_on = FALSE;
    gboolean abort_move;
    VirtualLocation cur_virt_loc;
    VirtualLocation new_virt_loc;
    gncTableTraversalDir direction = 0;
    GdkModifierType modifiers = gtk_accelerator_get_default_mod_mask ();

    g_return_val_if_fail (GNUCASH_IS_SHEET (sheet), FALSE);
    if (gnc_table_control_input_suspended (sheet->table->control))
        return TRUE;
    if (sheet_keyval_is_modifier (keyval))
        return FALSE;

    gnucash_sheet_set_selection_from_entry (sheet);
    if (gnucash_sheet_direct_keyval (sheet, keyval, state))
        return TRUE;

    gnucash_cursor_get_virt (GNUCASH_CURSOR (sheet->cursor), &cur_virt_loc);
    new_virt_loc = cur_virt_loc;
    if (state & modifiers)
        pass_on = TRUE;
    else if (process_motion_keys (sheet, keyval, state, &pass_on,
                                  &direction, &new_virt_loc))
        return TRUE;

    if (pass_on)
        return FALSE;

    abort_move = gnc_table_traverse_update (sheet->table, cur_virt_loc,
                                            direction, &new_virt_loc);
    if (abort_move)
    {
        if (!gtk_widget_has_focus (GTK_WIDGET (sheet)))
            gtk_widget_grab_focus (GTK_WIDGET (sheet));
        return TRUE;
    }

    sheet->pos = sheet->bound;
    gnucash_sheet_cursor_move (sheet, new_virt_loc);
    return TRUE;
}

static gboolean
gnucash_sheet_key_pressed_cb (G_GNUC_UNUSED GtkEventControllerKey *controller,
                              guint keyval, guint keycode,
                              GdkModifierType state, GnucashSheet *sheet)
{
    return gnucash_sheet_handle_key (sheet, keyval, keycode, state);
}
void
gnucash_sheet_goto_virt_loc (GnucashSheet *sheet, VirtualLocation virt_loc)
{
    Table *table;
    gboolean abort_move;
    VirtualLocation cur_virt_loc;

    g_return_if_fail (GNUCASH_IS_SHEET(sheet));

    table = sheet->table;
    if (gnc_table_control_input_suspended (table->control))
        return;

    gnucash_cursor_get_virt (GNUCASH_CURSOR(sheet->cursor), &cur_virt_loc);

    /* It's not really a pointer traverse, but it seems the most
     * appropriate here. */
    abort_move = gnc_table_traverse_update (table, cur_virt_loc,
                                            GNC_TABLE_TRAVERSE_POINTER,
                                            &virt_loc);

    if (abort_move)
        return;

    // does the sheet need horizontal scrolling
    gnucash_sheet_need_horizontal_scroll (sheet, &virt_loc);

    gnucash_sheet_cursor_move (sheet, virt_loc);
}

SheetBlock *
gnucash_sheet_get_block (GnucashSheet *sheet, VirtualCellLocation vcell_loc)
{
    g_return_val_if_fail (sheet != NULL, NULL);
    g_return_val_if_fail (GNUCASH_IS_SHEET(sheet), NULL);

    return g_table_index (sheet->blocks,
                          vcell_loc.virt_row,
                          vcell_loc.virt_col);
}

GncItemEdit *gnucash_sheet_get_item_edit (GnucashSheet *sheet)
{
    g_return_val_if_fail (sheet != NULL, NULL);
    g_return_val_if_fail (GNUCASH_IS_SHEET(sheet), NULL);

    if (sheet->item_editor == NULL)
        return NULL;
    else
        return GNC_ITEM_EDIT(sheet->item_editor);
}


void gnucash_sheet_set_window (GnucashSheet *sheet, GtkWidget *window)
{
    g_return_if_fail (sheet != NULL);
    g_return_if_fail (GNUCASH_IS_SHEET(sheet));

    if (window)
        g_return_if_fail (GTK_IS_WIDGET(window));

    sheet->window = window;
}


/* This fills up a block from the table; it sets the style and returns
 * true if the style changed. */
gboolean
gnucash_sheet_block_set_from_table (GnucashSheet *sheet,
                                    VirtualCellLocation vcell_loc)
{
    Table *table;
    SheetBlock *block;
    SheetBlockStyle *style;
    VirtualCell *vcell;

    block = gnucash_sheet_get_block (sheet, vcell_loc);
    style = gnucash_sheet_get_style_from_table (sheet, vcell_loc);

    if (!block)
        return FALSE;

    table = sheet->table;

    vcell = gnc_table_get_virtual_cell (table, vcell_loc);

    if (block->style && (block->style != style))
    {
        gnucash_sheet_style_unref (sheet, block->style);
        block->style = NULL;
    }

    block->visible = (vcell) ? vcell->visible : TRUE;

    if (block->style == NULL)
    {
        block->style = style;
        gnucash_sheet_style_ref (sheet, block->style);
        return TRUE;
    }
    return FALSE;
}


gint
gnucash_sheet_col_max_width (GnucashSheet *sheet, gint virt_col, gint cell_col)
{
    int virt_row;
    int cell_row;
    int max = 0;
    int width;
    SheetBlock *block;
    SheetBlockStyle *style;
    PangoLayout *layout = gtk_widget_create_pango_layout (GTK_WIDGET (sheet), "");
    GncItemEdit *item_edit = GNC_ITEM_EDIT(sheet->item_editor);
    const gchar *type_name;

    g_return_val_if_fail (virt_col >= 0, 0);
    g_return_val_if_fail (virt_col < sheet->num_virt_cols, 0);
    g_return_val_if_fail (cell_col >= 0, 0);

    for (virt_row = 0; virt_row < sheet->num_virt_rows ; virt_row++)
    {
        VirtualCellLocation vcell_loc = { virt_row, virt_col };

        block = gnucash_sheet_get_block (sheet, vcell_loc);
        if (!block)
            continue;

        style = block->style;

        if (!style)
            continue;

        if (cell_col < style->ncols)
        {
            for (cell_row = 0; cell_row < style->nrows; cell_row++)
            {
                VirtualLocation virt_loc;
                const char *text;

                if (virt_row == 0)
                    virt_loc.vcell_loc = sheet->table->current_cursor_loc.vcell_loc;
                else
                    virt_loc.vcell_loc = vcell_loc;

                virt_loc.phys_row_offset = cell_row;
                virt_loc.phys_col_offset = cell_col;

                if (virt_row == 0)
                {
                    text = gnc_table_get_label
                           (sheet->table, virt_loc);
                }
                else
                {
                    text = gnc_table_get_entry
                           (sheet->table, virt_loc);
                }

                pango_layout_set_text (layout, text, strlen (text));
                pango_layout_get_pixel_size (layout, &width, NULL);

                width += (gnc_item_edit_get_margin (item_edit, left_right) +
                          gnc_item_edit_get_padding_border (item_edit, left_right));

                // get the cell type so we can add the button width to the
                // text width if required.
                type_name = gnc_table_get_cell_type_name (sheet->table, virt_loc);
                if ((g_strcmp0 (type_name, DATE_CELL_TYPE_NAME) == 0)
                    || (g_strcmp0 (type_name, COMBO_CELL_TYPE_NAME) == 0))
                {
                    width += gnc_item_edit_get_button_width (item_edit) + 2; // add 2 for the button margin
                }
                max = MAX(max, width);
            }
        }
    }

    g_object_unref (layout);

    return max;
}

void
gnucash_sheet_set_scroll_region (GnucashSheet *sheet)
{
    g_return_if_fail (GNUCASH_IS_SHEET (sheet));

    if (!sheet->header_item || !GNC_HEADER (sheet->header_item)->style)
        return;

    /* GtkFixed has no virtual canvas extent. The explicit adjustments below
     * are the single source of truth for both the snapshot viewport and the
     * two register scrollbars. */
    gnucash_sheet_update_adjustments (sheet);
}

static void
gnucash_sheet_block_destroy (gpointer _block, gpointer user_data)
{
    SheetBlock *block = _block;
    GnucashSheet *sheet = GNUCASH_SHEET(user_data);

    if (block == NULL)
        return;

    if (block->style)
    {
        gnucash_sheet_style_unref (sheet, block->style);
        /* Don't free the block itself here. It's managed by the block table */
    }
}

static void
gnucash_sheet_block_construct (gpointer _block, gpointer user_data)
{
    SheetBlock *block = _block;

    block->style = NULL;
    block->visible = TRUE;
}

static void
gnucash_sheet_resize (GnucashSheet *sheet)
{
    g_return_if_fail (sheet != NULL);
    g_return_if_fail (GNUCASH_IS_SHEET(sheet));

    if (sheet->table->num_virt_cols > 1)
        g_warning ("num_virt_cols > 1");

    sheet->num_virt_cols = 1;

    g_table_resize (sheet->blocks, sheet->table->num_virt_rows, 1);

    sheet->num_virt_rows = sheet->table->num_virt_rows;
}

void
gnucash_sheet_recompute_block_offsets (GnucashSheet *sheet)
{
    Table *table;
    SheetBlock *block;
    gint i, j;
    gint height;
    gint width;

    g_return_if_fail (sheet != NULL);
    g_return_if_fail (GNUCASH_IS_SHEET(sheet));
    g_return_if_fail (sheet->table != NULL);

    table = sheet->table;

    height = 0;
    block = NULL;
    for (i = 0; i < table->num_virt_rows; i++)
    {
        width = 0;

        for (j = 0; j < table->num_virt_cols; j++)
        {
            VirtualCellLocation vcell_loc = { i, j };

            block = gnucash_sheet_get_block (sheet, vcell_loc);

            if (!block)
                continue;

            block->origin_x = width;
            block->origin_y = height;

            if (block->visible)
                width += block->style->dimensions->width;
        }

        if (i > 0 && block && block->visible)
            height += block->style->dimensions->height;
    }
    sheet->height = height;
}

void
gnucash_sheet_table_load (GnucashSheet *sheet, gboolean do_scroll)
{
    Table *table;
    gint num_header_phys_rows;
    gint i, j;

    g_return_if_fail (sheet != NULL);
    g_return_if_fail (GNUCASH_IS_SHEET(sheet));
    g_return_if_fail (sheet->table != NULL);

    table = sheet->table;

    gnucash_sheet_stop_editing (sheet);

    gnucash_sheet_resize (sheet);

    num_header_phys_rows = 0;

    /* fill it up */
    for (i = 0; i < table->num_virt_rows; i++)
        for (j = 0; j < table->num_virt_cols; j++)
        {
            VirtualCellLocation vcell_loc = { i, j };
            VirtualCell *vcell;

            gnucash_sheet_block_set_from_table (sheet, vcell_loc);

            vcell = gnc_table_get_virtual_cell (table, vcell_loc);

            num_header_phys_rows =
                MAX (num_header_phys_rows,
                     vcell->cellblock->num_rows);
        }

    gnc_header_set_header_rows (GNC_HEADER(sheet->header_item),
                                num_header_phys_rows);
    gnc_header_reconfigure (GNC_HEADER(sheet->header_item));

    gnucash_sheet_recompute_block_offsets (sheet);

    gnucash_sheet_set_scroll_region (sheet);

    if (do_scroll)
    {
        VirtualLocation virt_loc;

        virt_loc = table->current_cursor_loc;

        if (gnucash_sheet_cell_valid (sheet, virt_loc))
            gnucash_sheet_show_row (sheet,
                                    virt_loc.vcell_loc.virt_row);
    }

    gnucash_sheet_cursor_set_from_table (sheet, do_scroll);
    gnucash_sheet_activate_cursor_cell (sheet, TRUE);
}

/*************************************************************/

/** CSS classes are now applied to persistent GTK4 widgets. The snapshot
 * renderer keeps its own palette and must not mutate a shared StyleContext
 * while painting individual virtual cells. */
void
gnucash_get_style_classes (G_GNUC_UNUSED GnucashSheet *sheet,
                           G_GNUC_UNUSED GtkStyleContext *stylectxt,
                           G_GNUC_UNUSED RegisterColor field_type,
                           G_GNUC_UNUSED gboolean use_neg_class)
{
}
/*************************************************************/

static void
gnucash_sheet_class_init (GnucashSheetClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);
    GtkWidgetClass *widget_class = GTK_WIDGET_CLASS (klass);

    gtk_widget_class_set_css_name (widget_class, "gnc-id-sheet");
    gtk_widget_class_set_accessible_role (widget_class, GTK_ACCESSIBLE_ROLE_GRID);
    object_class->finalize = gnucash_sheet_finalize;
    object_class->get_property = gnucash_sheet_get_property;
    object_class->set_property = gnucash_sheet_set_property;
    g_object_class_override_property (object_class, PROP_HADJUSTMENT,
                                      "hadjustment");
    g_object_class_override_property (object_class, PROP_VADJUSTMENT,
                                      "vadjustment");
    g_object_class_override_property (object_class, PROP_HSCROLL_POLICY,
                                      "hscroll-policy");
    g_object_class_override_property (object_class, PROP_VSCROLL_POLICY,
                                      "vscroll-policy");

    widget_class->measure = gnucash_sheet_measure;
    widget_class->size_allocate = gnucash_sheet_size_allocate;
    widget_class->snapshot = gnucash_sheet_snapshot;
}

static void
gnucash_sheet_init (GnucashSheet *sheet)
{
    GtkAdjustment *adjustment;
    GtkEventController *focus_controller;
    GtkEventController *key_controller;
    GtkEventController *scroll_controller;
    GtkGesture *click_gesture;

    gtk_widget_set_focusable (GTK_WIDGET (sheet), TRUE);
    sheet->num_visible_blocks = 1;
    sheet->num_visible_phys_rows = 1;
    sheet->input_cancelled = FALSE;
    sheet->popup = NULL;
    sheet->num_virt_rows = 0;
    sheet->num_virt_cols = 0;
    sheet->item_editor = NULL;
    sheet->entry = NULL;
    sheet->editing = FALSE;
    sheet->button = 0;
    sheet->window_width = -1;
    sheet->window_height = -1;
    sheet->width = 0;
    sheet->height = 0;
    sheet->hadj = NULL;
    sheet->vadj = NULL;
    sheet->hadj_handler = 0;
    sheet->vadj_handler = 0;
    sheet->hscroll_policy = GTK_SCROLL_MINIMUM;
    sheet->vscroll_policy = GTK_SCROLL_MINIMUM;

    adjustment = gtk_adjustment_new (0.0, 0.0, 0.0, 0.0, 0.0, 0.0);
    gnucash_sheet_set_adjustment (sheet, &sheet->hadj, &sheet->hadj_handler,
                                  adjustment, "hadjustment");
    g_object_unref (adjustment);
    adjustment = gtk_adjustment_new (0.0, 0.0, 0.0, 0.0, 0.0, 0.0);
    gnucash_sheet_set_adjustment (sheet, &sheet->vadj, &sheet->vadj_handler,
                                  adjustment, "vadjustment");
    g_object_unref (adjustment);

    sheet->cursor_styles = g_hash_table_new (g_str_hash, g_str_equal);
    sheet->blocks = g_table_new (sizeof (SheetBlock),
                                 gnucash_sheet_block_construct,
                                 gnucash_sheet_block_destroy, sheet);
    sheet->bound = sheet->pos = 0;

    focus_controller = gtk_event_controller_focus_new ();
    g_signal_connect (focus_controller, "enter",
                      G_CALLBACK (gnucash_sheet_focus_enter_cb), sheet);
    g_signal_connect (focus_controller, "leave",
                      G_CALLBACK (gnucash_sheet_focus_leave_cb), sheet);
    gtk_widget_add_controller (GTK_WIDGET (sheet), focus_controller);

    key_controller = gtk_event_controller_key_new ();
    gtk_event_controller_set_propagation_phase (key_controller, GTK_PHASE_CAPTURE);
    g_signal_connect (key_controller, "key-pressed",
                      G_CALLBACK (gnucash_sheet_key_pressed_cb), sheet);
    gtk_widget_add_controller (GTK_WIDGET (sheet), key_controller);

    click_gesture = gtk_gesture_click_new ();
    g_signal_connect (click_gesture, "pressed",
                      G_CALLBACK (gnucash_sheet_click_pressed_cb), sheet);
    g_signal_connect (click_gesture, "released",
                      G_CALLBACK (gnucash_sheet_click_released_cb), sheet);
    gtk_widget_add_controller (GTK_WIDGET (sheet),
                               GTK_EVENT_CONTROLLER (click_gesture));

    scroll_controller = gtk_event_controller_scroll_new
        (GTK_EVENT_CONTROLLER_SCROLL_BOTH_AXES);
    g_signal_connect (scroll_controller, "scroll",
                      G_CALLBACK (gnucash_sheet_scroll_cb), sheet);
    gtk_widget_add_controller (GTK_WIDGET (sheet), scroll_controller);
}
static gboolean
gnucash_sheet_tooltip (GtkWidget  *widget, gint x, gint y,
                       gboolean    keyboard_mode,
                       GtkTooltip *tooltip,
                       gpointer    user_data)
{
    GnucashSheet *sheet = GNUCASH_SHEET(widget);
    Table *table = sheet->table;
    VirtualLocation virt_loc;
    gchar *tooltip_text;
    gint cx, cy, cw, ch;
    GdkRectangle rect;
    SheetBlock *block;
    gint bx, by;
    gint hscroll_val, vscroll_val;

    if (keyboard_mode)
        return FALSE;

    // get the scroll window values
    hscroll_val = (gint) gtk_adjustment_get_value (sheet->hadj);
    vscroll_val = (gint) gtk_adjustment_get_value (sheet->vadj);

    if (!gnucash_sheet_find_loc_by_pixel (sheet, x + hscroll_val, y + vscroll_val, &virt_loc))
        return FALSE;

    tooltip_text = gnc_table_get_tooltip (table, virt_loc);

    // if tooltip_text empty, clear tooltip and return FALSE
    if (!tooltip_text || (g_strcmp0 (tooltip_text,"") == 0))
    {
        gtk_tooltip_set_text (tooltip, NULL);
        return FALSE;
    }

    block = gnucash_sheet_get_block (sheet, virt_loc.vcell_loc);
    if (!block)
    {
        g_free (tooltip_text);
        return FALSE;
    }

    bx = block->origin_x;
    by = block->origin_y;

    // get the cell location and dimensions
    gnucash_sheet_style_get_cell_pixel_rel_coords (block->style,
            virt_loc.phys_row_offset, virt_loc.phys_col_offset,
            &cx, &cy, &cw, &ch);

    rect.x = cx + bx - hscroll_val;
    rect.y = cy + by - vscroll_val;
    rect.width = cw;
    rect.height = ch;

    gtk_tooltip_set_tip_area (tooltip, &rect);
    gtk_tooltip_set_text (tooltip, tooltip_text);
    g_free (tooltip_text);
    return TRUE;
}

static void
dimensions_destroy (BlockDimensions *dimensions)
{
    if (dimensions)
    {
        g_table_destroy (dimensions->cell_dimensions);
        g_free (dimensions);
    }
}

GtkWidget *
gnucash_sheet_new (Table *table)
{
    GnucashSheet *sheet;

    g_return_val_if_fail (table != NULL, NULL);

    sheet = gnucash_sheet_create (table);

    /* on create, the sheet can grab the focus */
    sheet->sheet_has_focus = TRUE;

    /* The cursor */
    sheet->cursor = gnucash_cursor_new (sheet);

    /* set up the editor */
    sheet->item_editor = gnc_item_edit_new (sheet);

    /* some register data */
    sheet->dimensions_hash_table = g_hash_table_new_full (g_int_hash,
                                   g_int_equal,
                                   g_free, (GDestroyNotify)dimensions_destroy);

    /* add tooltips to sheet */
    gtk_widget_set_has_tooltip (GTK_WIDGET(sheet), TRUE);
    g_signal_connect (G_OBJECT(sheet), "query-tooltip",
                      G_CALLBACK(gnucash_sheet_tooltip), NULL);

    gnucash_sheet_refresh_from_prefs (sheet);

    return GTK_WIDGET(sheet);
}
