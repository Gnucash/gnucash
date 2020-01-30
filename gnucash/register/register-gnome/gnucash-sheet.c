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
#include <glib/gprintf.h>
#include <gdk/gdkkeysyms.h>

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

#ifdef G_OS_WIN32
# include <gdk/gdkwin32.h>
#endif

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
static QofLogModule log_module = GNC_MOD_REGISTER;
static GtkLayout *sheet_parent_class;


/** Prototypes *********************************************************/

static void gnucash_sheet_start_editing_at_cursor (GnucashSheet *sheet);

static gboolean gnucash_sheet_cursor_move (GnucashSheet *sheet,
        VirtualLocation virt_loc);

static void gnucash_sheet_deactivate_cursor_cell (GnucashSheet *sheet);
static void gnucash_sheet_activate_cursor_cell (GnucashSheet *sheet,
        gboolean changed_cells);
static void gnucash_sheet_stop_editing (GnucashSheet *sheet);
static void gnucash_sheet_im_context_reset (GnucashSheet *sheet);
static void gnucash_sheet_commit_cb (GtkIMContext *context, const gchar *str,
                                     GnucashSheet *sheet);
static void gnucash_sheet_preedit_changed_cb (GtkIMContext *context,
        GnucashSheet *sheet);
static gboolean gnucash_sheet_retrieve_surrounding_cb (GtkIMContext *context,
        GnucashSheet *sheet);
static gboolean gnucash_sheet_delete_surrounding_cb (GtkIMContext *context,
        gint offset,
        gint n_chars,
        GnucashSheet *sheet);
static gboolean gnucash_sheet_check_direct_update_cell(GnucashSheet *sheet,
        const VirtualLocation virt_loc);
gboolean gnucash_sheet_draw_cb (GtkWidget *widget, cairo_t *cr,
                                G_GNUC_UNUSED gpointer data);

/** Implementation *****************************************************/

static inline gboolean
gnucash_sheet_virt_cell_out_of_bounds (GnucashSheet *sheet,
                                       VirtualCellLocation vcell_loc);
gboolean
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


void
gnucash_sheet_cursor_set (GnucashSheet *sheet, VirtualLocation virt_loc)
{
    g_return_if_fail (sheet != NULL);
    g_return_if_fail (GNUCASH_IS_SHEET (sheet));

    g_return_if_fail (virt_loc.vcell_loc.virt_row >= 0 ||
                      virt_loc.vcell_loc.virt_row <= sheet->num_virt_rows);
    g_return_if_fail (virt_loc.vcell_loc.virt_col >= 0 ||
                      virt_loc.vcell_loc.virt_col <= sheet->num_virt_cols);

    gtk_widget_queue_draw_area (GTK_WIDGET(sheet),
                                sheet->cursor->x, sheet->cursor->y,
                                sheet->cursor->w, sheet->cursor->h);

    gnucash_cursor_set (GNUCASH_CURSOR(sheet->cursor), virt_loc);

    gtk_widget_queue_draw_area (GTK_WIDGET(sheet),
                                sheet->cursor->x, sheet->cursor->y,
                                sheet->cursor->w, sheet->cursor->h);
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

    gtk_widget_hide (sheet->item_editor);
    gnc_item_edit_hide_popup (GNC_ITEM_EDIT(sheet->item_editor));
}

static void
gnucash_sheet_stop_editing (GnucashSheet *sheet)
{
    /* Rollback an uncommitted string if it exists   *
     * *before* disconnecting signal handlers.       */
    gnucash_sheet_im_context_reset(sheet);

    if (sheet->insert_signal != 0)
        g_signal_handler_disconnect (G_OBJECT(sheet->entry),
                                     sheet->insert_signal);
    if (sheet->delete_signal != 0)
        g_signal_handler_disconnect (G_OBJECT(sheet->entry),
                                     sheet->delete_signal);
    if (sheet->commit_signal != 0)
        g_signal_handler_disconnect (G_OBJECT(sheet->im_context),
                                     sheet->commit_signal);
    if (sheet->preedit_changed_signal != 0)
        g_signal_handler_disconnect (G_OBJECT(sheet->im_context),
                                     sheet->preedit_changed_signal);
    if (sheet->retrieve_surrounding_signal != 0)
        g_signal_handler_disconnect (G_OBJECT(sheet->im_context),
                                     sheet->retrieve_surrounding_signal);
    if (sheet->delete_surrounding_signal != 0)
        g_signal_handler_disconnect (G_OBJECT(sheet->im_context),
                                     sheet->delete_surrounding_signal);
    sheet->insert_signal = 0;
    sheet->delete_signal = 0;
    sheet->commit_signal = 0;
    sheet->preedit_changed_signal = 0;
    sheet->retrieve_surrounding_signal = 0;
    sheet->delete_surrounding_signal = 0;
    sheet->direct_update_cell = FALSE;

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

static gint
gnucash_sheet_get_text_cursor_position (GnucashSheet *sheet, const VirtualLocation virt_loc)
{
    GncItemEdit *item_edit = GNC_ITEM_EDIT(sheet->item_editor);
    Table *table = sheet->table;
    const char *text = gnc_table_get_entry (table, virt_loc);
    PangoLayout *layout;
    PangoRectangle logical_rect;
    GdkRectangle rect;
    gint x, y, width, height;
    gint index, trailing;
    gboolean result;
    gint x_offset = 0;

    if ((text == NULL) || (*text == '\0'))
        return 0;

    // Get the item_edit position
    gnc_item_edit_get_pixel_coords (item_edit, &x, &y, &width, &height);

    layout = gtk_widget_create_pango_layout (GTK_WIDGET (sheet), text);

    // We don't need word wrap or line wrap
    pango_layout_set_width (layout, -1);

    pango_layout_get_pixel_extents (layout, NULL, &logical_rect);

    gnucash_sheet_set_text_bounds (sheet, &rect, x, y, width, height);

    x_offset = gnucash_sheet_get_text_offset (sheet, virt_loc,
                                              rect.width, logical_rect.width);

    pango_layout_xy_to_index (layout,
                 PANGO_SCALE * (sheet->button_x - rect.x - x_offset),
                 PANGO_SCALE * (height/2), &index, &trailing);

    g_object_unref (layout);

    return index + trailing;
}

static void
gnucash_sheet_activate_cursor_cell (GnucashSheet *sheet,
                                    gboolean changed_cells)
{
    Table *table = sheet->table;
    VirtualLocation virt_loc;
    SheetBlockStyle *style;
    GtkEditable *editable;
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

    editable = GTK_EDITABLE(sheet->entry);

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
        gnucash_sheet_im_context_reset(sheet);
        gnucash_sheet_start_editing_at_cursor (sheet);

        // Came here by keyboard, select text, otherwise text cursor to
        // mouse position
        if (sheet->button != 1)
        {
            gtk_editable_set_position (editable, cursor_pos);
            gtk_editable_select_region (editable, start_sel, end_sel);
        }
        else
            gtk_editable_set_position (editable,
                gnucash_sheet_get_text_cursor_position (sheet, virt_loc));

        sheet->direct_update_cell =
            gnucash_sheet_check_direct_update_cell (sheet, virt_loc);
    }
    // when a gui refresh is called, we end up here so only grab the focus
    // if the sheet is showing on the current plugin_page
    if (sheet->sheet_has_focus)
        gtk_widget_grab_focus (GTK_WIDGET(sheet));
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
    GtkAllocation alloc;
    GtkAdjustment *adj;
    gint height;
    gint cy;
    gint top_block;
//    gint old_visible_blocks, old_visible_rows;

    g_return_if_fail (sheet != NULL);
    g_return_if_fail (GNUCASH_IS_SHEET (sheet));

    gtk_widget_get_allocation (GTK_WIDGET(sheet), &alloc);
    height = alloc.height;

    adj = gtk_scrollable_get_vadjustment (GTK_SCROLLABLE(sheet));
    cy = gtk_adjustment_get_value(adj);

    top_block = gnucash_sheet_y_pixel_to_block (sheet, cy);

//    old_visible_blocks = sheet->num_visible_blocks;
//    old_visible_rows = sheet->num_visible_phys_rows;
    sheet->num_visible_blocks = 0;
    sheet->num_visible_phys_rows = 0;

    for ( vcell_loc.virt_row = top_block, vcell_loc.virt_col = 0;
            vcell_loc.virt_row < sheet->num_virt_rows;
            vcell_loc.virt_row++ )
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
    GtkAllocation alloc;
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
    cx = gtk_adjustment_get_value(adj);
    adj = gtk_scrollable_get_vadjustment (GTK_SCROLLABLE(sheet));
    cy = gtk_adjustment_get_value(adj);
    x = cx;

    gtk_widget_get_allocation (GTK_WIDGET(sheet), &alloc);
    height = alloc.height;

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
    g_return_if_fail (GNUCASH_IS_SHEET (sheet));

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
    GtkAllocation alloc;
    GtkAdjustment *adj;
    gint block_height;
    gint height;
    gint cx, cy;
    gint x, y;

    g_return_if_fail (sheet != NULL);
    g_return_if_fail (GNUCASH_IS_SHEET(sheet));

    start_loc.virt_row = MAX (start_loc.virt_row, 1);
    start_loc.virt_row = MIN (start_loc.virt_row,
                              sheet->num_virt_rows - 1);

    end_loc.virt_row = MAX (end_loc.virt_row, 1);
    end_loc.virt_row = MIN (end_loc.virt_row,
                            sheet->num_virt_rows - 1);

    adj = gtk_scrollable_get_hadjustment (GTK_SCROLLABLE(sheet));
    cx = gtk_adjustment_get_value(adj);
    adj = gtk_scrollable_get_vadjustment (GTK_SCROLLABLE(sheet));
    cy = gtk_adjustment_get_value(adj);
    x = cx;

    gtk_widget_get_allocation (GTK_WIDGET(sheet), &alloc);
    height = alloc.height;

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
gnucash_sheet_update_adjustments (GnucashSheet *sheet)
{
    GtkAdjustment *vadj;

    g_return_if_fail (sheet != NULL);
    g_return_if_fail (GNUCASH_IS_SHEET (sheet));
    g_return_if_fail (sheet->vadj != NULL);

    vadj = sheet->vadj;

    if (sheet->num_visible_blocks > 0)
        gtk_adjustment_set_step_increment (vadj,
            gtk_adjustment_get_page_size (vadj) / sheet->num_visible_blocks);
    else
        gtk_adjustment_set_step_increment (vadj, 0);
}


static void
gnucash_sheet_vadjustment_value_changed (GtkAdjustment *adj,
        GnucashSheet *sheet)
{
    gnucash_sheet_compute_visible_range (sheet);
}


void
gnucash_sheet_redraw_all (GnucashSheet *sheet)
{
    g_return_if_fail (sheet != NULL);
    g_return_if_fail (GNUCASH_IS_SHEET(sheet));

    gtk_widget_queue_draw (GTK_WIDGET (sheet));

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
gnucash_sheet_redraw_block (GnucashSheet *sheet, VirtualCellLocation vcell_loc)
{
    gint x, y, w, h;
    SheetBlock *block;
    GtkAllocation alloc;

    g_return_if_fail (sheet != NULL);
    g_return_if_fail (GNUCASH_IS_SHEET(sheet));

    block = gnucash_sheet_get_block (sheet, vcell_loc);
    if (!block || !block->style)
        return;

    x = block->origin_x;
    y = block->origin_y;

    gtk_widget_get_allocation (GTK_WIDGET(sheet), &alloc);
    h = block->style->dimensions->height;
    w = MIN(block->style->dimensions->width,
            alloc.width);

    gtk_widget_queue_draw_area (GTK_WIDGET(sheet), x, y, w + 1, h + 1);
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

static void
gnucash_sheet_finalize (GObject *object)
{
    GnucashSheet *sheet;

    sheet = GNUCASH_SHEET (object);

    g_table_resize (sheet->blocks, 0, 0);
    g_table_destroy (sheet->blocks);
    sheet->blocks = NULL;

    gnucash_sheet_clear_styles (sheet);

    g_hash_table_destroy (sheet->cursor_styles);
    g_hash_table_destroy (sheet->dimensions_hash_table);

    if (G_OBJECT_CLASS (sheet_parent_class)->finalize)
        (*G_OBJECT_CLASS (sheet_parent_class)->finalize)(object);

    /* Clean up IMContext and unref */
    gnucash_sheet_im_context_reset(sheet);
    g_object_unref (sheet->im_context);
}


static GnucashSheet *
gnucash_sheet_create (Table *table)
{
    GnucashSheet *sheet;

    ENTER("table=%p", table);

    sheet = g_object_new (GNUCASH_TYPE_SHEET, NULL);
    sheet->table = table;
    sheet->entry = NULL;
    sheet->vadj = gtk_scrollable_get_vadjustment (GTK_SCROLLABLE(sheet));
    sheet->hadj = gtk_scrollable_get_hadjustment (GTK_SCROLLABLE(sheet));

    g_signal_connect (G_OBJECT (sheet->vadj), "value_changed",
                      G_CALLBACK (gnucash_sheet_vadjustment_value_changed), sheet);
    g_signal_connect (G_OBJECT (sheet), "draw",
                      G_CALLBACK (gnucash_sheet_draw_cb), sheet);

    LEAVE("%p", sheet);
    return sheet;
}

static void
gnucash_sheet_get_preferred_width (G_GNUC_UNUSED GtkWidget *widget,
                                   gint *minimal_width,
                                   gint *natural_width)
{
    *minimal_width = *natural_width = DEFAULT_SHEET_WIDTH;
}


/* Compute the height needed to show DEFAULT_REGISTER_INITIAL_ROWS rows */
static void
gnucash_sheet_get_preferred_height (G_GNUC_UNUSED GtkWidget *widget,
                                    gint *minimal_width,
                                    gint *natural_width)
{
    GnucashSheet *sheet = GNUCASH_SHEET(widget);
    SheetBlockStyle *style;
    CellDimensions *cd;
    gint row_height;

    *minimal_width = *natural_width = DEFAULT_SHEET_HEIGHT;

    if (!sheet)
        return;

    style = gnucash_sheet_get_style_from_cursor (sheet, CURSOR_HEADER);
    if (!style)
        return;

    cd = gnucash_style_get_cell_dimensions (style, 0, 0);
    if (cd == NULL)
        return;

    row_height = cd->pixel_height;

    *minimal_width = *natural_width =  row_height * DEFAULT_SHEET_INITIAL_ROWS;
}

const char *
gnucash_sheet_modify_current_cell (GnucashSheet *sheet, const gchar *new_text)
{
    GtkEditable *editable;
    Table *table = sheet->table;
    VirtualLocation virt_loc;
    int new_text_len;

    const char *retval;

    int cursor_position, start_sel, end_sel;

    gnucash_cursor_get_virt(GNUCASH_CURSOR(sheet->cursor), &virt_loc);

    if (!gnc_table_virtual_loc_valid (table, virt_loc, TRUE))
        return NULL;

    if (gnc_table_model_read_only (table->model))
        return NULL;

    editable = GTK_EDITABLE(sheet->entry);

    cursor_position = gtk_editable_get_position (editable);
    gtk_editable_get_selection_bounds (editable, &start_sel, &end_sel);

    new_text_len = strlen (new_text);

    retval = gnc_table_modify_update (table, virt_loc,
                                      new_text, new_text_len,
                                      new_text, new_text_len,
                                      &cursor_position,
                                      &start_sel, &end_sel,
                                      NULL);


    if (retval)
    {
        g_signal_handler_block (G_OBJECT (sheet->entry),
                                sheet->insert_signal);
        g_signal_handler_block (G_OBJECT (sheet->entry),
                                sheet->delete_signal);

        gtk_entry_set_text (GTK_ENTRY (sheet->entry), retval);

        g_signal_handler_unblock (G_OBJECT (sheet->entry),
                                  sheet->delete_signal);
        g_signal_handler_unblock (G_OBJECT (sheet->entry),
                                  sheet->insert_signal);
    }

    gtk_editable_set_position (editable, cursor_position);
    gtk_editable_select_region(editable, start_sel, end_sel);

    return retval;
}

typedef struct
{
    GtkEditable *editable;
    int start_sel;
    int end_sel;

} select_info;

static void
gnucash_sheet_insert_cb (GtkWidget *widget,
                         const gchar *insert_text,
                         const gint insert_text_len,
                         gint *position,
                         GnucashSheet *sheet)
{
    GtkEditable *editable;
    Table *table = sheet->table;
    VirtualLocation virt_loc;

    char *change_text;
    GString *change_text_gs;

    int new_text_len;
    int change_text_len;

    const char *old_text;
    const char *retval;
    char *new_text;
    GString *new_text_gs;

    int start_sel, end_sel;
    int old_position;
    int i;
    const char *c;
    gunichar uc;

    if (sheet->input_cancelled)
    {
        g_signal_stop_emission_by_name (G_OBJECT (sheet->entry),
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

    change_text_gs = g_string_new_len (insert_text, insert_text_len);

    old_text = gtk_entry_get_text (GTK_ENTRY(sheet->entry));
    if (old_text == NULL)
        old_text = "";

    old_position = *position;

    /* we set new_text_gs to what the entry contents would be if
       the insert was processed */
    new_text_gs = g_string_new ("");

    i = 0;
    c = old_text;
    //Copy old text up to insert position
    while (*c && (i < old_position))
    {
        uc = g_utf8_get_char (c);
        g_string_append_unichar (new_text_gs, uc);
        c = g_utf8_next_char (c);
        i++;
    }

    //Copy inserted text
    g_string_append (new_text_gs, change_text_gs->str);

    //Copy old text after insert position
    while (*c)
    {
        uc = g_utf8_get_char (c);
        g_string_append_unichar (new_text_gs, uc);
        c = g_utf8_next_char (c);
    }

    new_text = new_text_gs->str;
    new_text_len = new_text_gs->len;

    change_text = change_text_gs->str;
    change_text_len = change_text_gs->len;

    editable = GTK_EDITABLE (sheet->entry);

    gtk_editable_get_selection_bounds (editable, &start_sel, &end_sel);

    retval = gnc_table_modify_update (table, virt_loc,
                                      change_text, change_text_len,
                                      new_text, new_text_len,
                                      position, &start_sel, &end_sel,
                                      &sheet->input_cancelled);

    if (retval &&
            ((strcmp (retval, new_text) != 0) ||
             (*position != old_position)))
    {
        g_signal_handler_block (G_OBJECT (sheet->entry),
                                sheet->insert_signal);
        g_signal_handler_block (G_OBJECT (sheet->entry),
                                sheet->delete_signal);

        gtk_entry_set_text (GTK_ENTRY (sheet->entry), retval);

        g_signal_handler_unblock (G_OBJECT (sheet->entry),
                                  sheet->delete_signal);
        g_signal_handler_unblock (G_OBJECT (sheet->entry),
                                  sheet->insert_signal);

        g_signal_stop_emission_by_name (G_OBJECT(sheet->entry),
                                        "insert_text");
    }
    else if (retval == NULL)
    {
        retval = old_text;

        /* reset IMContext if disallowed chars and clear preedit*/
        gnucash_sheet_im_context_reset(sheet);
        /* the entry was disallowed, so we stop the insert signal */
        g_signal_stop_emission_by_name (G_OBJECT (sheet->entry),
                                        "insert_text");
    }

    /* sync cursor position and selection to preedit if it exists */
    if (sheet->preedit_length)
    {
        gtk_editable_set_position (editable,
                                   sheet->preedit_start_position
                                   + sheet->preedit_cursor_position);
    }
    else if (*position < 0)
        *position = g_utf8_strlen(retval, -1);

    if (start_sel != end_sel)
        gtk_editable_select_region(editable, start_sel, end_sel);
    /* Save the selected region in case the input module eats it. */
    sheet->start_sel = start_sel;
    sheet->end_sel = end_sel;

    g_string_free (new_text_gs, TRUE);
    g_string_free (change_text_gs, TRUE);
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

    int new_text_len;

    const char *old_text;
    const char *retval;
    char *new_text;
    GString *new_text_gs;

    int cursor_position = start_pos;
    int start_sel, end_sel;
    int i;
    const char *c;
    gunichar uc;

    if (end_pos <= start_pos)
        return;

    gnucash_cursor_get_virt (GNUCASH_CURSOR (sheet->cursor), &virt_loc);

    if (!gnc_table_virtual_loc_valid (table, virt_loc, FALSE))
        return;

    if (gnc_table_model_read_only (table->model))
        return;

    old_text = gtk_entry_get_text (GTK_ENTRY(sheet->entry));
    if (!old_text)
        old_text = "";

    new_text_gs = g_string_new ("");
    i = 0;
    c = old_text;
    while (*c && (i < start_pos))
    {
        uc = g_utf8_get_char (c);
        g_string_append_unichar (new_text_gs, uc);
        c = g_utf8_next_char (c);
        i++;
    }

    c = g_utf8_offset_to_pointer (old_text, end_pos);
    while (*c)
    {
        uc = g_utf8_get_char (c);
        g_string_append_unichar (new_text_gs, uc);
        c = g_utf8_next_char (c);
    }

    new_text = new_text_gs->str;
    new_text_len = new_text_gs->len;

    editable = GTK_EDITABLE (sheet->entry);

    gtk_editable_get_selection_bounds (editable, &start_sel, &end_sel);

    retval = gnc_table_modify_update (table, virt_loc,
                                      NULL, 0,
                                      new_text, new_text_len,
                                      &cursor_position,
                                      &start_sel, &end_sel,
                                      &sheet->input_cancelled);

    if (retval && (strcmp (retval, new_text) != 0))
    {
        g_signal_handler_block (G_OBJECT (sheet->entry),
                                sheet->insert_signal);
        g_signal_handler_block (G_OBJECT (sheet->entry),
                                sheet->delete_signal);

        gtk_entry_set_text (GTK_ENTRY (sheet->entry), retval);

        g_signal_handler_unblock (G_OBJECT (sheet->entry),
                                  sheet->delete_signal);
        g_signal_handler_unblock (G_OBJECT (sheet->entry),
                                  sheet->insert_signal);

        g_signal_stop_emission_by_name (G_OBJECT(sheet->entry),
                                        "delete_text");
    }
    else if (retval == NULL)
    {
        /* the entry was disallowed, so we stop the delete signal */
        g_signal_stop_emission_by_name (G_OBJECT(sheet->entry),
                                        "delete_text");
    }

    gtk_editable_set_position (editable, cursor_position);

    if (start_sel != end_sel)
        gtk_editable_select_region (editable, start_sel, end_sel);

    g_string_free (new_text_gs, TRUE);
}

gboolean
gnucash_sheet_draw_cb (GtkWidget *widget, cairo_t *cr, G_GNUC_UNUSED gpointer data)
{
    GnucashSheet *sheet = GNUCASH_SHEET (widget);
    GtkStyleContext *context = gtk_widget_get_style_context (widget);
    GtkAllocation alloc;
    gboolean result; //FIXME

    gtk_widget_get_allocation(widget, &alloc);

    gtk_style_context_save (context);
    gtk_style_context_add_class (context, GTK_STYLE_CLASS_BACKGROUND);
    gtk_render_background (context, cr, 0, 0, alloc.width, alloc.height);
    gtk_style_context_restore (context);

//FIXME what should be done with result being TRUE or FALSE
    gnucash_sheet_draw_internal (sheet, cr, &alloc);
    gnucash_sheet_draw_cursor (sheet->cursor, cr);

    return FALSE;
}


static void
gnucash_sheet_size_allocate (GtkWidget *widget, GtkAllocation *allocation)
{
    GnucashSheet *sheet = GNUCASH_SHEET(widget);

    ENTER("widget=%p, allocation=%p", widget, allocation);

    if (GTK_WIDGET_CLASS(sheet_parent_class)->size_allocate)
        (*GTK_WIDGET_CLASS (sheet_parent_class)->size_allocate)
        (widget, allocation);

    if (allocation->height == sheet->window_height &&
            allocation->width == sheet->window_width)
    {
        LEAVE("size unchanged");
        return;
    }

    if (allocation->width != sheet->window_width)
    {
        gnucash_sheet_styles_set_dimensions (sheet, allocation->width);
        gnucash_sheet_recompute_block_offsets (sheet);
    }

    sheet->window_height = allocation->height;
    sheet->window_width  = allocation->width;

    gnucash_cursor_configure (GNUCASH_CURSOR (sheet->cursor));
    gnc_header_reconfigure (GNC_HEADER(sheet->header_item));
    gnucash_sheet_set_scroll_region (sheet);

    gnc_item_edit_configure (GNC_ITEM_EDIT(sheet->item_editor));
    gnucash_sheet_update_adjustments (sheet);

    if (sheet->table)
    {
        VirtualLocation virt_loc;

        virt_loc = sheet->table->current_cursor_loc;

        if (gnucash_sheet_cell_valid (sheet, virt_loc))
            gnucash_sheet_show_row (sheet,
                                    virt_loc.vcell_loc.virt_row);
    }
    gnc_header_request_redraw (GNC_HEADER(sheet->header_item));
    LEAVE(" ");
}

static gboolean
gnucash_sheet_focus_in_event (GtkWidget *widget, GdkEventFocus *event)
{
    GnucashSheet *sheet = GNUCASH_SHEET(widget);

    if (GTK_WIDGET_CLASS(sheet_parent_class)->focus_in_event)
        (*GTK_WIDGET_CLASS (sheet_parent_class)->focus_in_event)
        (widget, event);

    gnc_item_edit_focus_in (GNC_ITEM_EDIT(sheet->item_editor));
    gtk_im_context_focus_in(sheet->im_context);

#ifdef G_OS_WIN32
    gnucash_sheet_im_context_reset(sheet);
#endif /* G_OS_WIN32 */

    return FALSE;
}

static gboolean
gnucash_sheet_focus_out_event (GtkWidget *widget, GdkEventFocus *event)
{
    GnucashSheet *sheet = GNUCASH_SHEET(widget);

    if (GTK_WIDGET_CLASS(sheet_parent_class)->focus_out_event)
        (*GTK_WIDGET_CLASS (sheet_parent_class)->focus_out_event)
        (widget, event);

#ifdef G_OS_WIN32
    gnucash_sheet_im_context_reset(sheet);
#endif /* G_OS_WIN32 */

    gtk_im_context_focus_out (sheet->im_context);
    gnc_item_edit_focus_out (GNC_ITEM_EDIT(sheet->item_editor));
    return FALSE;
}

static gboolean
gnucash_sheet_check_direct_update_cell(GnucashSheet *sheet,
                                       const VirtualLocation virt_loc)
{
    const gchar *type_name;

    type_name = gnc_table_get_cell_type_name (sheet->table, virt_loc);

    if ( (g_strcmp0 (type_name, DATE_CELL_TYPE_NAME) == 0)
            || (g_strcmp0 (type_name, COMBO_CELL_TYPE_NAME) == 0)
            || (g_strcmp0 (type_name, NUM_CELL_TYPE_NAME) == 0)
            || (g_strcmp0 (type_name, PRICE_CELL_TYPE_NAME) == 0)
            || (g_strcmp0 (type_name, FORMULA_CELL_TYPE_NAME) == 0)) return TRUE;

    return FALSE;
}

static void
gnucash_sheet_start_editing_at_cursor (GnucashSheet *sheet)
{
    const char *text;
    VirtualLocation virt_loc;

    g_return_if_fail (sheet != NULL);
    g_return_if_fail (GNUCASH_IS_SHEET (sheet));

    gnucash_cursor_get_virt (GNUCASH_CURSOR(sheet->cursor), &virt_loc);

    text = gnc_table_get_entry (sheet->table, virt_loc);

    gnc_item_edit_configure (GNC_ITEM_EDIT(sheet->item_editor));
    gtk_widget_show (GTK_WIDGET (sheet->item_editor));

    gtk_entry_set_text (GTK_ENTRY(sheet->entry), text);

    sheet->editing = TRUE;

    /* set up the signals */
    sheet->insert_signal =
        g_signal_connect(G_OBJECT(sheet->entry), "insert_text",
                         G_CALLBACK(gnucash_sheet_insert_cb), sheet);
    sheet->delete_signal =
        g_signal_connect(G_OBJECT(sheet->entry), "delete_text",
                         G_CALLBACK(gnucash_sheet_delete_cb), sheet);

    sheet->commit_signal =
        g_signal_connect (G_OBJECT (sheet->im_context), "commit",
                          G_CALLBACK (gnucash_sheet_commit_cb), sheet);
    sheet->preedit_changed_signal =
        g_signal_connect (G_OBJECT (sheet->im_context), "preedit_changed",
                          G_CALLBACK (gnucash_sheet_preedit_changed_cb),
                          sheet);
    sheet->retrieve_surrounding_signal =
        g_signal_connect (G_OBJECT (sheet->im_context),
                          "retrieve_surrounding",
                          G_CALLBACK (gnucash_sheet_retrieve_surrounding_cb),
                          sheet);
    sheet->delete_surrounding_signal =
        g_signal_connect (G_OBJECT (sheet->im_context), "delete_surrounding",
                          G_CALLBACK (gnucash_sheet_delete_surrounding_cb),
                          sheet);
}

static gboolean
gnucash_sheet_button_release_event (GtkWidget *widget, GdkEventButton *event)
{
    GnucashSheet *sheet;

    g_return_val_if_fail(widget != NULL, TRUE);
    g_return_val_if_fail(GNUCASH_IS_SHEET(widget), TRUE);
    g_return_val_if_fail(event != NULL, TRUE);

    sheet = GNUCASH_SHEET (widget);

    if (sheet->button != event->button)
        return FALSE;

    sheet->button = 0;

    if (event->button != 1)
        return FALSE;

    gtk_grab_remove (widget);
    sheet->grabbed = FALSE;

    return TRUE;
}

static gboolean
gnucash_scroll_event (GtkWidget *widget, GdkEventScroll *event)
{
    GnucashSheet *sheet;
    GtkAdjustment *vadj;
    gfloat v_value;

    g_return_val_if_fail(widget != NULL, TRUE);
    g_return_val_if_fail(GNUCASH_IS_SHEET(widget), TRUE);
    g_return_val_if_fail(event != NULL, TRUE);

    sheet = GNUCASH_SHEET (widget);
    vadj = sheet->vadj;
    v_value = gtk_adjustment_get_value (vadj);

    switch (event->direction)
    {
    case GDK_SCROLL_UP:
        v_value -= gtk_adjustment_get_step_increment (vadj);
        break;
    case GDK_SCROLL_DOWN:
        v_value += gtk_adjustment_get_step_increment (vadj);
        break;
    case GDK_SCROLL_SMOOTH:
        if (event->delta_y < 0)
            v_value -= gtk_adjustment_get_step_increment (vadj);
        if (event->delta_y > 0)
            v_value += gtk_adjustment_get_step_increment (vadj);
        break;
    default:
        return FALSE;
    }
    v_value = CLAMP(v_value, gtk_adjustment_get_lower (vadj),
              gtk_adjustment_get_upper (vadj) - gtk_adjustment_get_page_size (vadj));

    gtk_adjustment_set_value(vadj, v_value);

    if (event->delta_y == 0)
    {
        // There are problems with the slider not tracking the value so
        // when delta_y is 0 hide and showing the scrollbar seems to fix it
        gtk_widget_hide (GTK_WIDGET(sheet->vscrollbar));
        gtk_widget_show (GTK_WIDGET(sheet->vscrollbar));
    }
    return TRUE;
}

static void
gnucash_sheet_check_grab (GnucashSheet *sheet)
{
    GdkModifierType mods;
    GdkDevice *device;
#if GTK_CHECK_VERSION(3,20,0)
    GdkSeat *seat;
#else
    GdkDeviceManager *device_manager;
#endif

    if (!sheet->grabbed)
        return;

#if GTK_CHECK_VERSION(3,20,0)
    seat = gdk_display_get_default_seat (gdk_display_get_default());
    device = gdk_seat_get_pointer (seat);
#else
    device_manager = gdk_display_get_device_manager (gdk_display_get_default());
    device = gdk_device_manager_get_client_pointer (device_manager);
#endif

    gdk_device_get_state (device, gtk_widget_get_window (GTK_WIDGET(sheet)),
                          0, &mods);

    if (!(mods & GDK_BUTTON1_MASK))
    {
        gtk_grab_remove (GTK_WIDGET(sheet));
        sheet->grabbed = FALSE;
    }
}

static gboolean
gnucash_sheet_button_press_event (GtkWidget *widget, GdkEventButton *event)
{
    GnucashSheet *sheet;
    VirtualCell *vcell;
    gboolean changed_cells; //FIXME

    VirtualLocation cur_virt_loc;
    VirtualLocation new_virt_loc;

    Table *table;
    gboolean abort_move;
    gboolean button_1;
    gboolean do_popup;

    g_return_val_if_fail(widget != NULL, TRUE);
    g_return_val_if_fail(GNUCASH_IS_SHEET(widget), TRUE);
    g_return_val_if_fail(event != NULL, TRUE);

    sheet = GNUCASH_SHEET (widget);
    table = sheet->table;

    if (sheet->button && (sheet->button != event->button))
        return FALSE;

    sheet->button = event->button;
    if (sheet->button == 3)
        sheet->button = 0;

    if (!gtk_widget_has_focus(widget))
        gtk_widget_grab_focus(widget);

    button_1 = FALSE;
    do_popup = FALSE;

    switch (event->button)
    {
    case 1:
        button_1 = TRUE;
        break;
    case 2:
        if (event->type != GDK_BUTTON_PRESS)
            return FALSE;
        gnc_item_edit_paste_clipboard (GNC_ITEM_EDIT(sheet->item_editor));
        return TRUE;
    case 3:
        do_popup = (sheet->popup != NULL);
        break;
    default:
        return FALSE;
    }

    gnucash_cursor_get_virt (GNUCASH_CURSOR(sheet->cursor), &cur_virt_loc);

    sheet->button_x = -1;
    sheet->button_y = -1;

    if (!gnucash_sheet_find_loc_by_pixel(sheet,
                                        event->x, event->y, &new_virt_loc))
        return TRUE;

    sheet->button_x = event->x;
    sheet->button_y = event->y;

    vcell = gnc_table_get_virtual_cell (table, new_virt_loc.vcell_loc);
    if (vcell == NULL)
        return TRUE;

    if (event->type != GDK_BUTTON_PRESS)
        return FALSE;

    if (button_1)
    {
        gtk_grab_add(widget);
        sheet->grabbed = TRUE;
    }

    if (virt_loc_equal (new_virt_loc, cur_virt_loc) &&
        sheet->editing && do_popup)
    {
#if GTK_CHECK_VERSION(3,22,0)
        gtk_menu_popup_at_pointer (GTK_MENU(sheet->popup), (GdkEvent *) event);
#else
        gtk_menu_popup(GTK_MENU(sheet->popup), NULL, NULL, NULL,
                       sheet->popup_data, event->button, event->time);
#endif

        return TRUE;
    }

    /* and finally...process this as a POINTER_TRAVERSE */
    abort_move = gnc_table_traverse_update (table,
                                            cur_virt_loc,
                                            GNC_TABLE_TRAVERSE_POINTER,
                                            &new_virt_loc);

    if (button_1)
        gnucash_sheet_check_grab (sheet);

    if (abort_move)
        return TRUE;

//FIXME does something need to be done if changed_cells is true or false ?
    gnucash_sheet_cursor_move (sheet, new_virt_loc);

    if (button_1)
        gnucash_sheet_check_grab (sheet);

    if (do_popup)
#if GTK_CHECK_VERSION(3,22,0)
        gtk_menu_popup_at_pointer (GTK_MENU(sheet->popup), (GdkEvent *) event);
#else
        gtk_menu_popup(GTK_MENU(sheet->popup), NULL, NULL, NULL,
                       sheet->popup_data, event->button, event->time);
#endif
    return button_1 || do_popup;
}

void
gnucash_sheet_refresh_from_prefs (GnucashSheet *sheet)
{
    g_return_if_fail(sheet != NULL);
    g_return_if_fail(GNUCASH_IS_SHEET(sheet));

    sheet->use_gnc_color_theme = gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL_REGISTER,
                                                     GNC_PREF_USE_GNUCASH_COLOR_THEME);
    sheet->use_horizontal_lines = gnc_prefs_get_bool(GNC_PREFS_GROUP_GENERAL_REGISTER,
                                                     GNC_PREF_DRAW_HOR_LINES);
    sheet->use_vertical_lines = gnc_prefs_get_bool(GNC_PREFS_GROUP_GENERAL_REGISTER,
                                                   GNC_PREF_DRAW_VERT_LINES);
}

static gboolean
gnucash_sheet_clipboard_event (GnucashSheet *sheet, GdkEventKey *event)
{
    GncItemEdit *item_edit;
    gboolean handled = FALSE;

    item_edit = GNC_ITEM_EDIT(sheet->item_editor);

    switch (event->keyval)
    {
    case GDK_KEY_C:
    case GDK_KEY_c:
        if (event->state & GDK_MODIFIER_INTENT_PRIMARY_ACCELERATOR)
        {
            gnc_item_edit_copy_clipboard(item_edit);
            handled = TRUE;
        }
        break;
    case GDK_KEY_X:
    case GDK_KEY_x:
        if (event->state & GDK_MODIFIER_INTENT_PRIMARY_ACCELERATOR)
        {
            gnc_item_edit_cut_clipboard(item_edit);
            handled = TRUE;
        }
        break;
    case GDK_KEY_V:
    case GDK_KEY_v:
        if (event->state & GDK_MODIFIER_INTENT_PRIMARY_ACCELERATOR)
        {
            gnc_item_edit_paste_clipboard (item_edit);
            handled = TRUE;
        }
        break;
    case GDK_KEY_Insert:
        if (event->state & GDK_SHIFT_MASK)
        {
            gnc_item_edit_paste_clipboard (item_edit);
            handled = TRUE;
        }
        else if (event->state & GDK_MODIFIER_INTENT_PRIMARY_ACCELERATOR)
        {
            gnc_item_edit_copy_clipboard(item_edit);
            handled = TRUE;
        }
        break;
    }

    return handled;
}

static gboolean
gnucash_sheet_direct_event(GnucashSheet *sheet, GdkEvent *event)
{
    GtkEditable *editable;
    Table *table = sheet->table;
    VirtualLocation virt_loc;
//    gboolean changed;
    gboolean result;

    char *new_text = NULL;

    int cursor_position, start_sel, end_sel;
    int new_position, new_start, new_end;

    gnucash_cursor_get_virt(GNUCASH_CURSOR(sheet->cursor), &virt_loc);

    if (!gnc_table_virtual_loc_valid (table, virt_loc, TRUE))
        return FALSE;

    if (gnc_table_model_read_only (table->model))
        return FALSE;

    editable = GTK_EDITABLE(sheet->entry);

    cursor_position = gtk_editable_get_position (editable);
    gtk_editable_get_selection_bounds (editable, &start_sel, &end_sel);

    new_position = cursor_position;
    new_start = start_sel;
    new_end = end_sel;

    result = gnc_table_direct_update (table, virt_loc,
                                      &new_text,
                                      &new_position,
                                      &new_start, &new_end,
                                      event);

//    changed = FALSE;

    if (new_text != NULL)
    {
        g_signal_handler_block (G_OBJECT (sheet->entry),
                                sheet->insert_signal);
        g_signal_handler_block (G_OBJECT (sheet->entry),
                                sheet->delete_signal);

        gtk_entry_set_text (GTK_ENTRY (sheet->entry), new_text);

        g_signal_handler_unblock (G_OBJECT (sheet->entry),
                                  sheet->delete_signal);
        g_signal_handler_unblock (G_OBJECT (sheet->entry),
                                  sheet->insert_signal);

//        changed = TRUE;
    }

    if (new_position != cursor_position)
    {
        gtk_editable_set_position (editable, new_position);
//        changed = TRUE;
    }

    if ((new_start != start_sel) || (new_end != end_sel))
    {
        gtk_editable_select_region(editable, new_start, new_end);
//        changed = TRUE;
    }

    return result;
}

static gint
gnucash_sheet_key_press_event_internal (GtkWidget *widget, GdkEventKey *event)
{
    Table *table;
    GnucashSheet *sheet;
    gboolean pass_on = FALSE;
    gboolean abort_move;
    VirtualLocation cur_virt_loc;
    VirtualLocation new_virt_loc;
    gncTableTraversalDir direction = 0;
    int distance;
    GdkModifierType modifiers = gtk_accelerator_get_default_mod_mask ();

    g_return_val_if_fail(widget != NULL, TRUE);
    g_return_val_if_fail(GNUCASH_IS_SHEET(widget), TRUE);
    g_return_val_if_fail(event != NULL, TRUE);

    sheet = GNUCASH_SHEET (widget);
    table = sheet->table;

    if (gnucash_sheet_direct_event(sheet, (GdkEvent *) event))
        return TRUE;

    gnucash_cursor_get_virt (GNUCASH_CURSOR(sheet->cursor), &cur_virt_loc);
    new_virt_loc = cur_virt_loc;

    /* Don't process any keystrokes where a modifier key (Alt,
     * Meta, etc.) is being held down.  This shouldn't include
         * MOD2, aka NUM LOCK. */
    if (event->state & modifiers & (GDK_MODIFIER_INTENT_DEFAULT_MOD_MASK))
        pass_on = TRUE;

    /* Calculate tentative physical values */
    if (!pass_on)
    {
        switch (event->keyval)
        {
        case GDK_KEY_Return:
        case GDK_KEY_KP_Enter:
            g_signal_emit_by_name(sheet->reg, "activate_cursor");
	    /* Clear the saved selection. */
	    sheet->end_sel = sheet->start_sel;
            return TRUE;
            break;
        case GDK_KEY_Tab:
        case GDK_KEY_ISO_Left_Tab:
            if (event->state & GDK_SHIFT_MASK)
            {
                direction = GNC_TABLE_TRAVERSE_LEFT;
                gnc_table_move_tab (table, &new_virt_loc,
                                    FALSE);
            }
            else
            {
                direction = GNC_TABLE_TRAVERSE_RIGHT;
                gnc_table_move_tab (table, &new_virt_loc,
                                    TRUE);
            }
            break;
        case GDK_KEY_KP_Page_Up:
        case GDK_KEY_Page_Up:
            direction = GNC_TABLE_TRAVERSE_UP;
            new_virt_loc.phys_col_offset = 0;
            if (event->state & GDK_SHIFT_MASK)
                new_virt_loc.vcell_loc.virt_row = 1;
            else
            {
                distance = sheet->num_visible_phys_rows - 1;
                gnc_table_move_vertical_position
                (table, &new_virt_loc, -distance);
            }
            break;
        case GDK_KEY_KP_Page_Down:
        case GDK_KEY_Page_Down:
            direction = GNC_TABLE_TRAVERSE_DOWN;
            new_virt_loc.phys_col_offset = 0;
            if (event->state & GDK_SHIFT_MASK)
                new_virt_loc.vcell_loc.virt_row =
                    sheet->num_virt_rows - 1;
            else
            {
                distance = sheet->num_visible_phys_rows - 1;
                gnc_table_move_vertical_position
                (table, &new_virt_loc, distance);
            }
            break;
        case GDK_KEY_KP_Up:
        case GDK_KEY_Up:
            direction = GNC_TABLE_TRAVERSE_UP;
            gnc_table_move_vertical_position (table,
                                              &new_virt_loc, -1);
            break;
        case GDK_KEY_KP_Down:
        case GDK_KEY_Down:
        case GDK_KEY_Menu:
            if (event->keyval == GDK_KEY_Menu ||
                    (event->state & GDK_MODIFIER_INTENT_PRIMARY_ACCELERATOR))
            {
                GncItemEdit *item_edit;

                item_edit = GNC_ITEM_EDIT (sheet->item_editor);

                if (gnc_table_confirm_change (table,
                                              cur_virt_loc))
                    gnc_item_edit_show_popup (item_edit);
		/* Clear the saved selection for the new cell. */
		sheet->end_sel = sheet->start_sel;

                return TRUE;
            }

            direction = GNC_TABLE_TRAVERSE_DOWN;
            gnc_table_move_vertical_position (table,
                                              &new_virt_loc, 1);
            break;
        case GDK_KEY_Control_L:
        case GDK_KEY_Control_R:
        case GDK_KEY_Shift_L:
        case GDK_KEY_Shift_R:
        case GDK_KEY_Alt_L:
        case GDK_KEY_Alt_R:
            pass_on = TRUE;
            break;
        case GDK_KEY_KP_Right:
        case GDK_KEY_Right:
        case GDK_KEY_KP_Left:
        case GDK_KEY_Left:
        case GDK_KEY_Home:
        case GDK_KEY_End:
	    /* Clear the saved selection, we're not using it. */
	    sheet->end_sel = sheet->start_sel;
	    pass_on = TRUE;
	    break;
        default:
            if (gnucash_sheet_clipboard_event(sheet, event))
	    {
		/* Clear the saved selection. */
		sheet->end_sel = sheet->start_sel;
                return TRUE;
	    }
            pass_on = TRUE;
            break;
        }
    }

    /* Forward the keystroke to the input line */
    if (pass_on)
    {
        gboolean result = FALSE;
        GtkEditable *editable = GTK_EDITABLE(sheet->entry);

        // If sheet is readonly, entry is not realized
        if (gtk_widget_get_realized (GTK_WIDGET(editable)))
	{
            result = gtk_widget_event (GTK_WIDGET(editable), (GdkEvent*)event);
        /* Restore the stored selection in case it was eaten by the input
         * module.
         */
	    if (sheet->start_sel != sheet->end_sel)
	    {
		gtk_editable_select_region(editable, sheet->start_sel,
					   sheet->end_sel);
		sheet->end_sel = sheet->start_sel;
	    }
	}
        return result;
    }

    abort_move = gnc_table_traverse_update (table, cur_virt_loc,
                                            direction, &new_virt_loc);

    /* If that would leave the register, abort */
    if (abort_move)
    {
        // Make sure the sheet is the focus
        if (!gtk_widget_has_focus(GTK_WIDGET (sheet)))
            gtk_widget_grab_focus (GTK_WIDGET (sheet));
        return TRUE;
    }

    /* Clear the saved selection for the new cell. */
    sheet->end_sel = sheet->start_sel;
    gnucash_sheet_cursor_move (sheet, new_virt_loc);

    /* return true because we handled the key press */
    return TRUE;
}

static gint
gnucash_sheet_key_press_event (GtkWidget *widget, GdkEventKey *event)
{
    GnucashSheet *sheet;
    GtkEditable *editable = NULL;
    int start_sel = 0, end_sel = 0;

    g_return_val_if_fail(widget != NULL, TRUE);
    g_return_val_if_fail(GNUCASH_IS_SHEET(widget), TRUE);
    g_return_val_if_fail(event != NULL, TRUE);

    sheet = GNUCASH_SHEET (widget);
    editable = GTK_EDITABLE(sheet->entry);
    /* bug#60582 comment#27 2
           save shift state to enable <shift minus> and <shift equal>
       bug#618434
           save keyval to handle GDK_KEY_KP_Decimal event
     */
#ifdef G_OS_WIN32
    /* gdk never sends GDK_KEY_KP_Decimal on win32. See #486658 */
    if (event->hardware_keycode == VK_DECIMAL)
        event->keyval = GDK_KEY_KP_Decimal;
#endif
    if (sheet->preedit_length)
    {
        sheet->shift_state = 0;
        sheet->keyval_state = 0;
    }
    else
    {
        sheet->shift_state = event->state & GDK_SHIFT_MASK;
        sheet->keyval_state = (event->keyval == GDK_KEY_KP_Decimal) ? GDK_KEY_KP_Decimal : 0;
    }

    gtk_editable_get_selection_bounds (editable, &start_sel, &end_sel);

    if (gtk_im_context_filter_keypress (sheet->im_context, event))
    {
        sheet->need_im_reset = TRUE;
        return TRUE;
    }

    return gnucash_sheet_key_press_event_internal (widget, event);
}

static gint
gnucash_sheet_key_release_event(GtkWidget *widget, GdkEventKey *event)
{
    GnucashSheet *sheet;

    g_return_val_if_fail(widget != NULL, TRUE);
    g_return_val_if_fail(GNUCASH_IS_SHEET(widget), TRUE);
    g_return_val_if_fail(event != NULL, TRUE);

    sheet = GNUCASH_SHEET (widget);

    if (gtk_im_context_filter_keypress (sheet->im_context, event))
    {
        sheet->need_im_reset = TRUE;
        return TRUE;
    }

    return FALSE;
}

static void
gnucash_sheet_im_context_reset_flags(GnucashSheet *sheet)
{
    sheet->preedit_length = 0;
    sheet->preedit_char_length = 0;
    sheet->preedit_start_position = -1;
    sheet->preedit_cursor_position = 0;
    sheet->preedit_selection_length = 0;
}

static void
gnucash_sheet_im_context_reset(GnucashSheet *sheet)
{
    if (sheet->need_im_reset)
    {
        if (sheet->preedit_attrs)
        {
            pango_attr_list_unref (sheet->preedit_attrs);
            sheet->preedit_attrs = NULL;
        }
        gtk_im_context_reset (sheet->im_context);
        sheet->need_im_reset = FALSE;
    }
    gnucash_sheet_im_context_reset_flags(sheet);
}

static void
gnucash_sheet_commit_cb (GtkIMContext *context, const gchar *str,
                         GnucashSheet *sheet)
{
    GtkEditable *editable;
    gint tmp_pos, sel_start, sel_end;

    g_return_if_fail(strlen(str) > 0);
    g_return_if_fail(sheet->editing == TRUE);

    editable = GTK_EDITABLE (sheet->entry);

    if (strlen(str) == 1 && sheet->direct_update_cell)
    {
        /* Reconstruct keyevent and direct update */
        GdkEvent *event;
        GdkEventKey *keyevent;
        gboolean result;

        event = gdk_event_new (GDK_KEY_PRESS);
        keyevent = (GdkEventKey *) event;
        keyevent->keyval =
            sheet->keyval_state ? sheet->keyval_state
            : gdk_unicode_to_keyval(str[0]);
        keyevent->state |= sheet->shift_state;
        result = gnucash_sheet_direct_event(sheet, event);
        gdk_event_free(event);

        if (result)
        {
            gnucash_sheet_im_context_reset_flags(sheet);
            return;
        }
    }

    /* delete preedit string from editable*/
    if (sheet->preedit_length)
    {
        g_signal_handler_block (G_OBJECT (sheet->entry),
                                sheet->delete_signal);
        gtk_editable_delete_text (editable, sheet->preedit_start_position,
                                  sheet->preedit_start_position
                                  + sheet->preedit_char_length);
        g_signal_handler_unblock (G_OBJECT (sheet->entry),
                                  sheet->delete_signal);
    }

    if (gtk_editable_get_selection_bounds (editable, &sel_start, &sel_end))
    {
        if (sel_start != sel_end)
        {
            sheet->preedit_selection_length = 0;
            gtk_editable_delete_selection (editable);
        }
    }

    tmp_pos = (sheet->preedit_start_position == -1) ?
              gtk_editable_get_position (editable)
              : sheet->preedit_start_position;
    gtk_editable_insert_text (editable, str, strlen (str), &tmp_pos);

    /* insert_cb may have changed the selection, but gtk_editable_set_position
       (erroneously?) clears it.  If a selection is set, preserve it. */
    gtk_editable_get_selection_bounds (editable, &sel_start, &sel_end);
    gtk_editable_set_position (editable, tmp_pos);
    if (sel_start != sel_end)
        gtk_editable_select_region (editable, sel_start, sel_end);

    gnucash_sheet_im_context_reset_flags(sheet);
}

static void
gnucash_sheet_preedit_changed_cb (GtkIMContext *context, GnucashSheet *sheet)
{
    gchar *preedit_string;
    GtkEditable *editable;

    g_return_if_fail(context != NULL);
    g_return_if_fail(sheet->editing == TRUE);

    editable = GTK_EDITABLE (sheet->entry);

    /* save preedit start position and selection */
    if (sheet->preedit_length == 0)
    {
        int start_pos, end_pos;
        if ( gtk_editable_get_selection_bounds (editable, &start_pos, &end_pos))
        {
            sheet->preedit_start_position = start_pos;
            sheet->preedit_selection_length = end_pos - start_pos;
        }
        else
        {
            sheet->preedit_start_position =
                gtk_editable_get_position (editable);
        }
    }
#ifdef G_OS_WIN32
    else  /* sheet->preedit_length != 0 */
    {
        /* On Windows, gtk_im_context_reset() in gnucash_sheet_key_press_event()
         * always returns FALSE because Windows IME handles key press at the
         * top level window. So sheet->need_im_reset = TRUE here. */
        sheet->need_im_reset = TRUE;
    }
#endif /* G_OS_WIN32 */

    if (sheet->preedit_attrs)
        pango_attr_list_unref (sheet->preedit_attrs);

    gtk_im_context_get_preedit_string (sheet->im_context, &preedit_string,
                                       &sheet->preedit_attrs, &(sheet->preedit_cursor_position));

    if (sheet->preedit_length)
    {
        g_signal_handler_block (G_OBJECT (sheet->entry),
                                sheet->delete_signal);
        gtk_editable_delete_text (editable, sheet->preedit_start_position,
                                  sheet->preedit_start_position
                                  + sheet->preedit_char_length);
        g_signal_handler_unblock (G_OBJECT (sheet->entry),
                                  sheet->delete_signal);
    }

    sheet->preedit_length = strlen (preedit_string);
    sheet->preedit_char_length = g_utf8_strlen(preedit_string, -1);

    if (sheet->preedit_length)
    {
        int tmp_pos = sheet->preedit_start_position;
        g_signal_handler_block (G_OBJECT (sheet->entry),
                                sheet->insert_signal);
        gtk_editable_insert_text (editable, preedit_string, sheet->preedit_length,
                                  &tmp_pos);
        g_signal_handler_unblock (G_OBJECT (sheet->entry),
                                  sheet->insert_signal);

        gtk_editable_set_position (editable, sheet->preedit_start_position
                                   + sheet->preedit_cursor_position);

        if ( sheet->preedit_selection_length != 0)
        {
            gtk_editable_select_region (editable,
                                        sheet->preedit_start_position
                                        + sheet->preedit_char_length,
                                        sheet->preedit_start_position
                                        + sheet->preedit_char_length
                                        + sheet->preedit_selection_length);
        }
    }
    else
    {
        gnucash_sheet_im_context_reset_flags(sheet);
    }

    g_free (preedit_string);
}

static gboolean
gnucash_sheet_retrieve_surrounding_cb (GtkIMContext *context, GnucashSheet *sheet)
{
    GtkEditable *editable;
    gchar *surrounding;
    gint   cur_pos;

    editable = GTK_EDITABLE (sheet->entry);
    surrounding = gtk_editable_get_chars (editable, 0, -1);
    cur_pos = gtk_editable_get_position (editable);

    gtk_im_context_set_surrounding (context,
                                    surrounding, strlen (surrounding),
                                    g_utf8_offset_to_pointer (surrounding, cur_pos) - surrounding);
    g_free (surrounding);
    return TRUE;
}

static gboolean
gnucash_sheet_delete_surrounding_cb (GtkIMContext *context, gint offset,
                                     gint n_chars, GnucashSheet *sheet)
{
    GtkEditable *editable;
    gint cur_pos;

    editable = GTK_EDITABLE (sheet->entry);
    cur_pos = gtk_editable_get_position (editable);

    gtk_editable_delete_text (editable,
                              cur_pos + offset,
                              cur_pos + offset + n_chars);
    return TRUE;
}


void
gnucash_sheet_goto_virt_loc (GnucashSheet *sheet, VirtualLocation virt_loc)
{
    Table *table;
    gboolean abort_move;
    VirtualLocation cur_virt_loc;

    g_return_if_fail(GNUCASH_IS_SHEET(sheet));

    table = sheet->table;

    gnucash_cursor_get_virt (GNUCASH_CURSOR(sheet->cursor), &cur_virt_loc);

    /* It's not really a pointer traverse, but it seems the most
     * appropriate here. */
    abort_move = gnc_table_traverse_update (table, cur_virt_loc,
                                            GNC_TABLE_TRAVERSE_POINTER,
                                            &virt_loc);

    if (abort_move)
        return;

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
        return GNC_ITEM_EDIT (sheet->item_editor);
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
        gnucash_sheet_style_ref(sheet, block->style);
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
                max = MAX (max, width);
            }
        }
    }

    g_object_unref (layout);

    return max;
}

void
gnucash_sheet_set_scroll_region (GnucashSheet *sheet)
{
    guint new_h, new_w;
//    GtkWidget *widget;
    GtkAllocation alloc;
    guint old_h, old_w;

    if (!sheet)
        return;

//    widget = GTK_WIDGET(sheet);

    if (!sheet->header_item || !GNC_HEADER(sheet->header_item)->style)
        return;

    gtk_layout_get_size (GTK_LAYOUT(sheet), &old_w, &old_h);

    gtk_widget_get_allocation (GTK_WIDGET(sheet), &alloc);
    new_h = MAX (sheet->height, alloc.height);
    new_w  = MAX (sheet->width, alloc.width);

    if (new_w != old_w || new_h != old_h)
        gtk_layout_set_size (GTK_LAYOUT(sheet), new_w, new_h);
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

    gnc_header_set_header_rows (GNC_HEADER (sheet->header_item),
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

/** Map a cell color type to a css style class. */
void
gnucash_get_style_classes (GnucashSheet *sheet, GtkStyleContext *stylectxt,
                           RegisterColor field_type)
{
    gchar *full_class, *style_class = NULL;

    if (field_type >= COLOR_NEGATIVE) // Require a Negative fg color
    {
        gtk_style_context_add_class (stylectxt, "negative-numbers");
        field_type -= COLOR_NEGATIVE;
    }
    else
    {
        if (sheet->use_gnc_color_theme) // only add this class if builtin colors used
            gtk_style_context_add_class (stylectxt, "register-foreground");
    }

    switch (field_type)
    {
    default:
    case COLOR_UNDEFINED:
        gtk_style_context_add_class (stylectxt, GTK_STYLE_CLASS_BACKGROUND);
        return;

    case COLOR_HEADER:
        style_class = "header";
        break;

    case COLOR_PRIMARY:
        style_class = "primary";
        break;

    case COLOR_PRIMARY_ACTIVE:
    case COLOR_SECONDARY_ACTIVE:
    case COLOR_SPLIT_ACTIVE:
        gtk_style_context_set_state (stylectxt, GTK_STATE_FLAG_SELECTED);
        style_class = "cursor";
        break;

    case COLOR_SECONDARY:
        style_class = "secondary";
        break;

    case COLOR_SPLIT:
        style_class = "split";
        break;
    }

    if (sheet->use_gnc_color_theme)
        full_class = g_strconcat ("register-", style_class, NULL);
    else
    {
        gtk_style_context_add_class (stylectxt, GTK_STYLE_CLASS_VIEW);
        full_class = g_strconcat (style_class, "-color", NULL);
    }

    gtk_style_context_add_class (stylectxt, full_class);

    g_free (full_class);
}

/*************************************************************/

static void
gnucash_sheet_class_init (GnucashSheetClass *klass)
{
    GObjectClass *gobject_class;
    GtkWidgetClass *widget_class;

    gobject_class = G_OBJECT_CLASS (klass);
    widget_class = GTK_WIDGET_CLASS (klass);

#if GTK_CHECK_VERSION(3,20,0)
    gtk_widget_class_set_css_name (GTK_WIDGET_CLASS(klass), "sheet");
#endif

    sheet_parent_class = g_type_class_peek_parent (klass);

    /* Method override */
    gobject_class->finalize = gnucash_sheet_finalize;

    widget_class->get_preferred_width = gnucash_sheet_get_preferred_width;
    widget_class->get_preferred_height = gnucash_sheet_get_preferred_height;
    widget_class->size_allocate = gnucash_sheet_size_allocate;

    widget_class->focus_in_event = gnucash_sheet_focus_in_event;
    widget_class->focus_out_event = gnucash_sheet_focus_out_event;

    widget_class->key_press_event = gnucash_sheet_key_press_event;
    widget_class->key_release_event = gnucash_sheet_key_release_event;
    widget_class->button_press_event = gnucash_sheet_button_press_event;
    widget_class->button_release_event = gnucash_sheet_button_release_event;
    widget_class->scroll_event = gnucash_scroll_event;
}


static void
gnucash_sheet_init (GnucashSheet *sheet)
{
    gtk_widget_set_can_focus (GTK_WIDGET(sheet), TRUE);
    gtk_widget_set_can_default (GTK_WIDGET(sheet), TRUE);

    // This sets a style class for when Gtk+ version is less than 3.20
    gnc_widget_set_css_name (GTK_WIDGET(sheet), "sheet");

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
    sheet->grabbed = FALSE;
    sheet->window_width = -1;
    sheet->window_height = -1;
    sheet->width = 0;
    sheet->height = 0;

    sheet->cursor_styles = g_hash_table_new (g_str_hash, g_str_equal);

    sheet->blocks = g_table_new (sizeof (SheetBlock),
                                 gnucash_sheet_block_construct,
                                 gnucash_sheet_block_destroy, sheet);

    gtk_widget_add_events(GTK_WIDGET(sheet), (GDK_EXPOSURE_MASK
    | GDK_BUTTON_PRESS_MASK
    | GDK_BUTTON_RELEASE_MASK
    | GDK_POINTER_MOTION_MASK
    | GDK_POINTER_MOTION_HINT_MASK));

    /* setup IMContext */
    sheet->im_context = gtk_im_multicontext_new ();
    sheet->preedit_length = 0;
    sheet->preedit_char_length = 0;
    sheet->preedit_start_position = -1;
    sheet->preedit_cursor_position = 0;
    sheet->preedit_selection_length = 0;
    sheet->preedit_attrs = NULL;
    sheet->direct_update_cell = FALSE;
    sheet->need_im_reset = FALSE;
    sheet->commit_signal = 0;
    sheet->preedit_changed_signal = 0;
    sheet->retrieve_surrounding_signal = 0;
    sheet->delete_surrounding_signal = 0;
    sheet->shift_state = 0;
    sheet->keyval_state = 0;
    sheet->start_sel = sheet->end_sel = 0;
}


GType
gnucash_sheet_get_type (void)
{
    static GType gnucash_sheet_type = 0;

    if (!gnucash_sheet_type)
    {
        static const GTypeInfo gnucash_sheet_info =
        {
            sizeof (GnucashSheetClass),
            NULL,       /* base_init */
            NULL,       /* base_finalize */
            (GClassInitFunc) gnucash_sheet_class_init,
            NULL,       /* class_finalize */
            NULL,       /* class_data */
            sizeof (GnucashSheet),
            0,      /* n_preallocs */
            (GInstanceInitFunc) gnucash_sheet_init
        };

        gnucash_sheet_type =
            g_type_register_static (GTK_TYPE_LAYOUT,
                                    "GnucashSheet",
                                    &gnucash_sheet_info, 0);
    }

    return gnucash_sheet_type;
}


static gboolean
gnucash_sheet_tooltip (GtkWidget  *widget, gint x, gint y,
               gboolean    keyboard_mode, GtkTooltip *tooltip,
               gpointer    user_data)
{
    GnucashSheet *sheet = GNUCASH_SHEET (widget);
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
    sheet->item_editor = gnc_item_edit_new(sheet);

    /* some register data */
    sheet->dimensions_hash_table = g_hash_table_new_full (g_int_hash,
                                   g_int_equal,
                                   g_free, g_free);

    /* add tooltips to sheet */
    gtk_widget_set_has_tooltip (GTK_WIDGET(sheet), TRUE);
    g_signal_connect(G_OBJECT(sheet), "query-tooltip",
                     G_CALLBACK(gnucash_sheet_tooltip), NULL);

    gnucash_sheet_refresh_from_prefs(sheet);

    return GTK_WIDGET(sheet);
}
