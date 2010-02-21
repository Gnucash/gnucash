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

#include "config.h"

#include "gnucash-sheet.h"

#include "dialog-utils.h"
#include "gnc-gconf-utils.h"
#include "gnucash-color.h"
#include "gnucash-grid.h"
#include "gnucash-cursor.h"
#include "gnucash-style.h"
#include "gnucash-header.h"
#include "gnucash-item-edit.h"
#include "gnc-engine.h"		// For debugging, e.g. ENTER(), LEAVE()

#define DEFAULT_REGISTER_HEIGHT 400
#define DEFAULT_REGISTER_WIDTH  400


/* FIXME: at least broken on gtk 2.4.14 */
/* jsled: and 2.6.8 */
/* jsled: and 2.8.8 */
/* jsled: and 2.9.{0,1}, as per http://bugzilla.gnome.org/show_bug.cgi?id=342182 */
#define GTK_ALLOWED_SELECTION_WITHIN_INSERT_SIGNAL (GTK_MINOR_VERSION < 4)

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
static guint gnucash_register_initial_rows = 15;
static GnomeCanvasClass *sheet_parent_class;
static GtkTableClass *register_parent_class;
static guint register_signals[LAST_SIGNAL];


/** Prototypes *********************************************************/

static void gnucash_sheet_start_editing_at_cursor (GnucashSheet *sheet);

static gboolean gnucash_sheet_cursor_move (GnucashSheet *sheet,
                                           VirtualLocation virt_loc);

static void gnucash_sheet_deactivate_cursor_cell (GnucashSheet *sheet);
static void gnucash_sheet_activate_cursor_cell (GnucashSheet *sheet,
                                                gboolean changed_cells);
static void gnucash_sheet_stop_editing (GnucashSheet *sheet);


/** Implementation *****************************************************/

void
gnucash_register_set_initial_rows (guint num_rows)
{
        gnucash_register_initial_rows = num_rows;
}

G_INLINE_FUNC gboolean
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

        gnucash_cursor_set (GNUCASH_CURSOR(sheet->cursor), virt_loc);
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


static void
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

        gnome_canvas_item_hide (GNOME_CANVAS_ITEM (sheet->item_editor));
        gnc_item_edit_hide_popup (GNC_ITEM_EDIT(sheet->item_editor));
}

static void
gnucash_sheet_stop_editing (GnucashSheet *sheet)
{
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
		gnucash_sheet_start_editing_at_cursor (sheet);

                gtk_editable_set_position (editable, cursor_pos);

                gtk_editable_select_region (editable, start_sel, end_sel);
        }

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
        Table *table;
        VirtualCellLocation vcell_loc;
        gint height;
        gint cy;
	gint old_visible_blocks, old_visible_rows;

        g_return_if_fail (sheet != NULL);
        g_return_if_fail (GNUCASH_IS_SHEET (sheet));

        table = sheet->table;
        height = GTK_WIDGET(sheet)->allocation.height;

        gnome_canvas_get_scroll_offsets (GNOME_CANVAS(sheet), NULL, &cy);

        sheet->top_block = gnucash_sheet_y_pixel_to_block (sheet, cy);

	old_visible_blocks = sheet->num_visible_blocks;
	old_visible_rows = sheet->num_visible_phys_rows;
        sheet->num_visible_blocks = 0;
        sheet->num_visible_phys_rows = 0;

        for ( vcell_loc.virt_row = sheet->top_block, vcell_loc.virt_col = 0;
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

        sheet->bottom_block = vcell_loc.virt_row;

        /* FIXME */
        sheet->left_block = 0;
        sheet->right_block = 0;

	if ((old_visible_blocks > sheet->num_visible_blocks) ||
	    (old_visible_rows > sheet->num_visible_phys_rows)) {
		/* Reach up and tell the parent widget to redraw as
		 * well.  The sheet doesn't occupy all the visible
		 * area in the notebook page, and this will cause the
		 * parent to color in the empty grey space after the
		 * area occupied by the sheet. */
		gtk_widget_queue_draw(gtk_widget_get_parent(GTK_WIDGET(sheet)));
	}
}


static void
gnucash_sheet_show_row (GnucashSheet *sheet, gint virt_row)
{
        VirtualCellLocation vcell_loc = { virt_row, 0 };
        SheetBlock *block;
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

        gnome_canvas_get_scroll_offsets (GNOME_CANVAS(sheet), &cx, &cy);
        x = cx;

        height = GTK_WIDGET(sheet)->allocation.height;

        block = gnucash_sheet_get_block (sheet, vcell_loc);

        y = block->origin_y;
        block_height = block->style->dimensions->height;

        if ((cy <= y) && (cy + height >= y + block_height)) {
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

        gnome_canvas_get_scroll_offsets (GNOME_CANVAS(sheet), &cx, &cy);
        x = cx;

        height = GTK_WIDGET(sheet)->allocation.height;

        start_block = gnucash_sheet_get_block (sheet, start_loc);
        end_block = gnucash_sheet_get_block (sheet, end_loc);

        y = start_block->origin_y;
        block_height = (end_block->origin_y +
                        end_block->style->dimensions->height) - y;

        if ((cy <= y) && (cy + height >= y + block_height)) {
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
                vadj->step_increment =
                        vadj->page_size / sheet->num_visible_blocks;
        else
                vadj->step_increment = 0;

        gtk_adjustment_changed(vadj);
}


static void
gnucash_sheet_vadjustment_value_changed (GtkAdjustment *adj,
					 GnucashSheet *sheet)
{
        gnucash_sheet_compute_visible_range (sheet);
}


static void
gnucash_sheet_hadjustment_changed (GtkAdjustment *adj,
                                   GnucashSheet *sheet)
{
        GnucashRegister *reg;

        g_return_if_fail (sheet != NULL);
        g_return_if_fail (GNUCASH_IS_SHEET(sheet));
        reg = GNUCASH_REGISTER(sheet->reg);
        g_return_if_fail (reg != NULL);

        if (adj->upper - adj->lower > adj->page_size)
        {
                if (!reg->hscrollbar_visible)
                {
                        gtk_widget_show(reg->hscrollbar);
                        reg->hscrollbar_visible = TRUE;
                }
        } else {
                if (reg->hscrollbar_visible)
                {
                        gtk_widget_hide(reg->hscrollbar);
                        reg->hscrollbar_visible = FALSE;
                }
        }
}


void
gnucash_sheet_redraw_all (GnucashSheet *sheet)
{
        g_return_if_fail (sheet != NULL);
        g_return_if_fail (GNUCASH_IS_SHEET(sheet));

        gnome_canvas_request_redraw (GNOME_CANVAS (sheet), 0, 0,
                                     sheet->width + 1, sheet->height + 1);

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
        GnomeCanvas *canvas;
        SheetBlock *block;

        g_return_if_fail (sheet != NULL);
        g_return_if_fail (GNUCASH_IS_SHEET(sheet));

        canvas = GNOME_CANVAS(sheet);

        block = gnucash_sheet_get_block (sheet, vcell_loc);
        if (!block || !block->style)
                return;

        x = block->origin_x;
        y = block->origin_y;

        h = block->style->dimensions->height;
        w = MIN(block->style->dimensions->width,
                GTK_WIDGET(sheet)->allocation.width);

        gnome_canvas_request_redraw (canvas, x, y, x+w+1, y+h+1);
}

static void
gnucash_sheet_finalize (GObject *object)
{
        GnucashSheet *sheet;

        sheet = GNUCASH_SHEET (object);

        g_table_destroy (sheet->blocks);
        sheet->blocks = NULL;

        gnucash_sheet_clear_styles (sheet);

        g_hash_table_destroy (sheet->cursor_styles);
        g_hash_table_destroy (sheet->dimensions_hash_table);

        if (G_OBJECT_CLASS (sheet_parent_class)->finalize)
                (*G_OBJECT_CLASS (sheet_parent_class)->finalize)(object);

	/* This has to come after the parent destroy, so the item edit
	   destruction can do its disconnects. */
        g_object_unref (sheet->entry);
}


static void
gnucash_sheet_realize (GtkWidget *widget)
{
        GdkWindow *window;

        if (GTK_WIDGET_CLASS (sheet_parent_class)->realize)
                (*GTK_WIDGET_CLASS (sheet_parent_class)->realize)(widget);

        window = widget->window;
        gdk_window_set_back_pixmap (GTK_LAYOUT (widget)->bin_window,
				    NULL, FALSE);
}


static GnucashSheet *
gnucash_sheet_create (Table *table)
{
        GnucashSheet *sheet;
        GnomeCanvas  *canvas;

	ENTER("table=%p", table);

        sheet = g_object_new (GNUCASH_TYPE_SHEET, NULL);
        canvas = GNOME_CANVAS (sheet);

        sheet->table = table;
        sheet->entry = NULL;

        sheet->vadj = gtk_layout_get_vadjustment (GTK_LAYOUT(canvas));
        sheet->hadj = gtk_layout_get_hadjustment (GTK_LAYOUT(canvas));

        g_signal_connect (G_OBJECT (sheet->vadj), "value_changed",
                G_CALLBACK (gnucash_sheet_vadjustment_value_changed), sheet);
        g_signal_connect (G_OBJECT (sheet->hadj), "changed",
                G_CALLBACK (gnucash_sheet_hadjustment_changed), sheet);

	LEAVE("%p", sheet);
        return sheet;
}

static gint
compute_optimal_width (GnucashSheet *sheet)
{
        return DEFAULT_REGISTER_WIDTH;
}


/* Compute the height needed to show DEFAULT_REGISTER_ROWS rows */
static gint
compute_optimal_height (GnucashSheet *sheet)
{
        SheetBlockStyle *style;
        CellDimensions *cd;
        gint row_height;

        if (!sheet)
                return DEFAULT_REGISTER_HEIGHT;

        style = gnucash_sheet_get_style_from_cursor (sheet, CURSOR_HEADER);
        if (!style)
                return DEFAULT_REGISTER_HEIGHT;

        cd = gnucash_style_get_cell_dimensions (style, 0, 0);
        if (cd == NULL)
                return DEFAULT_REGISTER_HEIGHT;

        row_height = cd->pixel_height;

        return row_height * gnucash_register_initial_rows;
}


static void
gnucash_sheet_size_request (GtkWidget *widget, GtkRequisition *requisition)
{
        GnucashSheet *sheet = GNUCASH_SHEET(widget);

        requisition->width = compute_optimal_width (sheet);
        requisition->height = compute_optimal_height (sheet);
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


        if (retval) {
                gnc_item_edit_reset_offset (GNC_ITEM_EDIT(sheet->item_editor));

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

#if !GTK_ALLOWED_SELECTION_WITHIN_INSERT_SIGNAL
typedef struct {
	GtkEditable *editable;
        int start_sel;
	int end_sel;

} select_info;

/*
 * This function is needed because gtk_entry_insert_text() sets the
 * cursor position after emitting the "insert+text" signal.  This
 * means that the gtk_editable_select_region() call cannot be in the
 * insert_cb function because it will just be cancelled out after
 * the function finishes running.
 */
static gboolean
gnucash_sheet_select_data_cb (select_info *info)
{
        gtk_editable_select_region (info->editable,
				    info->start_sel, info->end_sel);
	g_free(info);
	return FALSE; /* This is a one shot function */
}
#endif

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
        int old_text_len;
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
        old_text_len = strlen (old_text);

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

                /* the entry was disallowed, so we stop the insert signal */
                g_signal_stop_emission_by_name (G_OBJECT (sheet->entry),
                                                "insert_text");
        }
        if (*position < 0)
                *position = g_utf8_strlen(retval, -1);

#if GTK_ALLOWED_SELECTION_WITHIN_INSERT_SIGNAL
        gtk_editable_select_region (editable, start_sel, end_sel);
#else
        {
                select_info *info;
                if (start_sel != end_sel) {
                        info = g_malloc(sizeof(*info));
                        info->editable = editable;
                        info->start_sel = start_sel;
                        info->end_sel = end_sel;
                        g_timeout_add(/*ASAP*/ 1,
                                (GSourceFunc)gnucash_sheet_select_data_cb,
                                info);
                }
        }
#endif
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
		gtk_editable_select_region(editable, start_sel, end_sel);

        g_string_free (new_text_gs, TRUE);
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

        return FALSE;
}

static gboolean
gnucash_sheet_focus_out_event (GtkWidget *widget, GdkEventFocus *event)
{
        GnucashSheet *sheet = GNUCASH_SHEET(widget);

        if (GTK_WIDGET_CLASS(sheet_parent_class)->focus_out_event)
                (*GTK_WIDGET_CLASS (sheet_parent_class)->focus_out_event)
                        (widget, event);

        gnc_item_edit_focus_out (GNC_ITEM_EDIT(sheet->item_editor));

        return FALSE;
}

static void
gnucash_sheet_start_editing_at_cursor (GnucashSheet *sheet)
{
        GnomeCanvas *canvas;
        const char *text;
        VirtualLocation virt_loc;

        g_return_if_fail (sheet != NULL);
        g_return_if_fail (GNUCASH_IS_SHEET (sheet));

        canvas = GNOME_CANVAS(sheet);

        gnucash_cursor_get_virt (GNUCASH_CURSOR(sheet->cursor), &virt_loc);

        text = gnc_table_get_entry (sheet->table, virt_loc);

        gnc_item_edit_configure (GNC_ITEM_EDIT(sheet->item_editor));
        gnome_canvas_item_show (GNOME_CANVAS_ITEM (sheet->item_editor));

        gtk_entry_set_text (GTK_ENTRY(sheet->entry), text);

        sheet->editing = TRUE;

        /* set up the signals */
        sheet->insert_signal =
                g_signal_connect(G_OBJECT(sheet->entry), "insert_text",
                                 G_CALLBACK(gnucash_sheet_insert_cb), sheet);
        sheet->delete_signal =
                g_signal_connect(G_OBJECT(sheet->entry), "delete_text",
                                 G_CALLBACK(gnucash_sheet_delete_cb), sheet);
}

static gboolean
gnucash_motion_event (GtkWidget *widget, GdkEventMotion *event)
{
        GnucashSheet *sheet;
        VirtualLocation virt_loc;

        g_return_val_if_fail(widget != NULL, TRUE);
        g_return_val_if_fail(GNUCASH_IS_SHEET(widget), TRUE);
        g_return_val_if_fail(event != NULL, TRUE);

        sheet = GNUCASH_SHEET(widget);

        if (!(event->state & GDK_BUTTON1_MASK) && sheet->grabbed)
        {
                gtk_grab_remove (widget);
                sheet->grabbed = FALSE;
        }

        if (sheet->button != 1)
                return FALSE;

        if (!sheet->editing || event->type != GDK_MOTION_NOTIFY)
                return FALSE;

        if (!(event->state & GDK_BUTTON1_MASK))
                return FALSE;

        gnucash_cursor_get_virt (GNUCASH_CURSOR(sheet->cursor), &virt_loc);

        gnc_item_edit_set_cursor_pos (GNC_ITEM_EDIT(sheet->item_editor),
                                  virt_loc, event->x, FALSE, TRUE);

        return TRUE;
}

static gboolean
gnucash_button_release_event (GtkWidget *widget, GdkEventButton *event)
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

        gnc_item_edit_set_has_selection(GNC_ITEM_EDIT(sheet->item_editor), FALSE);
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
        v_value = vadj->value;

        switch (event->direction)
        {
                case GDK_SCROLL_UP:
                        v_value -= vadj->step_increment;
                        break;
                case GDK_SCROLL_DOWN:
                        v_value += vadj->step_increment;
                        break;
                default:
                        return FALSE;
        }

        v_value = CLAMP(v_value, vadj->lower, vadj->upper - vadj->page_size);

        gtk_adjustment_set_value(vadj, v_value);

        return TRUE;
}

static void
gnucash_sheet_check_grab (GnucashSheet *sheet)
{
        GdkModifierType mods;
	GdkDevice *device;

        if (!sheet->grabbed)
                return;

	device = gdk_device_get_core_pointer ();

	gdk_device_get_state (device, GTK_WIDGET(sheet)->window,
			      0, &mods);

        if (!(mods & GDK_BUTTON1_MASK))
        {
                gtk_grab_remove (GTK_WIDGET(sheet));
                sheet->grabbed = FALSE;
        }
}

static gboolean
gnucash_button_press_event (GtkWidget *widget, GdkEventButton *event)
{
        GnucashSheet *sheet;
        GtkEditable *editable;
        VirtualCell *vcell;
        gboolean changed_cells;

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

        if (!GTK_WIDGET_HAS_FOCUS(widget))
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
                        gnc_item_edit_paste_primary(GNC_ITEM_EDIT(sheet->item_editor),
                                                event->time);
                        return TRUE;
                case 3:
                        do_popup = (sheet->popup != NULL);
                        break;
                default:
                        return FALSE;
        }

        gnucash_cursor_get_virt (GNUCASH_CURSOR(sheet->cursor), &cur_virt_loc);

	if (!gnucash_grid_find_loc_by_pixel(GNUCASH_GRID(sheet->grid),
                                            event->x, event->y, &new_virt_loc))
		return TRUE;

        vcell = gnc_table_get_virtual_cell (table, new_virt_loc.vcell_loc);
        if (vcell == NULL)
                return TRUE;

        if (virt_loc_equal (new_virt_loc, cur_virt_loc) && button_1 &&
            (event->type == GDK_2BUTTON_PRESS))
        {
                gnc_item_edit_set_cursor_pos (GNC_ITEM_EDIT(sheet->item_editor),
                                          cur_virt_loc, event->x, FALSE, FALSE);

		editable = GTK_EDITABLE(sheet->entry);
                gtk_editable_set_position(editable, -1);
                gtk_editable_select_region(editable, 0, -1);
                return TRUE;
        }

        if (event->type != GDK_BUTTON_PRESS)
                return FALSE;

        if (button_1)
        {
                gtk_grab_add(widget);
                sheet->grabbed = TRUE;
                gnc_item_edit_set_has_selection
                        (GNC_ITEM_EDIT(sheet->item_editor), TRUE);
        }

        if (virt_loc_equal (new_virt_loc, cur_virt_loc) && sheet->editing)
        {
                gboolean extend_selection = event->state & GDK_SHIFT_MASK;

                gnc_item_edit_set_cursor_pos (GNC_ITEM_EDIT(sheet->item_editor),
                                          cur_virt_loc, event->x, FALSE,
                                          extend_selection);

                if (do_popup)
                        gtk_menu_popup(GTK_MENU(sheet->popup), NULL, NULL, NULL,
				       sheet->popup_data, event->button, event->time);

                return button_1 || do_popup;
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

        changed_cells = gnucash_sheet_cursor_move (sheet, new_virt_loc);

        if (button_1)
                gnucash_sheet_check_grab (sheet);

        gnucash_cursor_get_virt (GNUCASH_CURSOR(sheet->cursor), &new_virt_loc);

        gnc_item_edit_set_cursor_pos (GNC_ITEM_EDIT(sheet->item_editor),
                                  new_virt_loc, event->x, changed_cells, FALSE);

        if (do_popup)
		gtk_menu_popup(GTK_MENU(sheet->popup), NULL, NULL, NULL,
			       sheet->popup_data, event->button, event->time);
        return button_1 || do_popup;
}

gboolean
gnucash_register_has_selection (GnucashRegister *reg)
{
        GnucashSheet *sheet;
        GncItemEdit *item_edit;

        g_return_val_if_fail((reg != NULL), FALSE);
        g_return_val_if_fail(GNUCASH_IS_REGISTER(reg), FALSE);

        sheet = GNUCASH_SHEET(reg->sheet);
        item_edit = GNC_ITEM_EDIT(sheet->item_editor);

        return gnc_item_edit_get_has_selection(item_edit);
}

void
gnucash_register_cut_clipboard (GnucashRegister *reg)
{
        GnucashSheet *sheet;
        GncItemEdit *item_edit;

        g_return_if_fail(reg != NULL);
        g_return_if_fail(GNUCASH_IS_REGISTER(reg));

        sheet = GNUCASH_SHEET(reg->sheet);
        item_edit = GNC_ITEM_EDIT(sheet->item_editor);

        gnc_item_edit_cut_clipboard(item_edit, GDK_CURRENT_TIME);
}

void
gnucash_register_copy_clipboard (GnucashRegister *reg)
{
        GnucashSheet *sheet;
        GncItemEdit *item_edit;

        g_return_if_fail(reg != NULL);
        g_return_if_fail(GNUCASH_IS_REGISTER(reg));

        sheet = GNUCASH_SHEET(reg->sheet);
        item_edit = GNC_ITEM_EDIT(sheet->item_editor);

        gnc_item_edit_copy_clipboard(item_edit, GDK_CURRENT_TIME);
}

void
gnucash_register_paste_clipboard (GnucashRegister *reg)
{
        GnucashSheet *sheet;
        GncItemEdit *item_edit;

        g_return_if_fail(reg != NULL);
        g_return_if_fail(GNUCASH_IS_REGISTER(reg));

        sheet = GNUCASH_SHEET(reg->sheet);
        item_edit = GNC_ITEM_EDIT(sheet->item_editor);

        gnc_item_edit_paste_clipboard(item_edit, GDK_CURRENT_TIME);
}

static void
gnucash_sheet_refresh_from_gconf (GnucashSheet *sheet)
{
        g_return_if_fail(sheet != NULL);
        g_return_if_fail(GNUCASH_IS_SHEET(sheet));

	sheet->use_theme_colors = gnc_gconf_get_bool(GCONF_GENERAL_REGISTER,
						     "use_theme_colors", NULL);
	sheet->use_horizontal_lines = gnc_gconf_get_bool(GCONF_GENERAL_REGISTER,
							 "draw_horizontal_lines", NULL);
	sheet->use_vertical_lines = gnc_gconf_get_bool(GCONF_GENERAL_REGISTER,
						       "draw_vertical_lines", NULL);
}

void
gnucash_register_refresh_from_gconf (GnucashRegister *reg)
{
        GnucashSheet *sheet;

        g_return_if_fail(reg != NULL);
        g_return_if_fail(GNUCASH_IS_REGISTER(reg));

        sheet = GNUCASH_SHEET(reg->sheet);
	gnucash_sheet_refresh_from_gconf(sheet);
}

static gboolean
gnucash_sheet_clipboard_event (GnucashSheet *sheet, GdkEventKey *event)
{
        GncItemEdit *item_edit;
        gboolean handled = FALSE;
        guint32 time;

        item_edit = GNC_ITEM_EDIT(sheet->item_editor);
        time = event->time;

        switch (event->keyval) {
                case GDK_C:
                case GDK_c:
                        if (event->state & GDK_CONTROL_MASK)
                        {
                                gnc_item_edit_copy_clipboard(item_edit, time);
                                handled = TRUE;
                        }
                        break;
                case GDK_X:
                case GDK_x:
                        if (event->state & GDK_CONTROL_MASK)
                        {
                                gnc_item_edit_cut_clipboard(item_edit, time);
                                handled = TRUE;
                        }
                        break;
                case GDK_V:
                case GDK_v:
                        if (event->state & GDK_CONTROL_MASK)
                        {
                                gnc_item_edit_paste_clipboard(item_edit, time);
                                handled = TRUE;
                        }
                        break;
                case GDK_Insert:
                        if (event->state & GDK_SHIFT_MASK)
                        {
                                gnc_item_edit_paste_clipboard(item_edit, time);
                                handled = TRUE;
                        }
                        else if (event->state & GDK_CONTROL_MASK)
                        {
                                gnc_item_edit_copy_clipboard(item_edit, time);
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
        gboolean changed;
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

        changed = FALSE;

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

                changed = TRUE;
        }

        if (new_position != cursor_position)
        {
                gtk_editable_set_position (editable, new_position);
                changed = TRUE;
        }

        if ((new_start != start_sel) || (new_end != end_sel))
        {
                gtk_editable_select_region(editable, new_start, new_end);
                changed = TRUE;
        }

        if (changed)
                gnc_item_edit_redraw(GNC_ITEM_EDIT(sheet->item_editor));

	return result;
}

static gint
gnucash_sheet_key_press_event (GtkWidget *widget, GdkEventKey *event)
{
        Table *table;
        GnucashSheet *sheet;
	CellBlock *header;
	gboolean pass_on = FALSE;
        gboolean abort_move;
        VirtualLocation cur_virt_loc;
        VirtualLocation new_virt_loc;
	gncTableTraversalDir direction = 0;
        int distance;

        g_return_val_if_fail(widget != NULL, TRUE);
        g_return_val_if_fail(GNUCASH_IS_SHEET(widget), TRUE);
        g_return_val_if_fail(event != NULL, TRUE);

        sheet = GNUCASH_SHEET (widget);
        table = sheet->table;
	header = gnc_table_get_header_cell(table)->cellblock;

        if (gnucash_sheet_direct_event(sheet, (GdkEvent *) event))
            return TRUE;

        gnucash_cursor_get_virt (GNUCASH_CURSOR(sheet->cursor), &cur_virt_loc);
        new_virt_loc = cur_virt_loc;

	/* Don't process any keystrokes where a modifier key (Alt,
	 * Meta, etc.) is being held down.  This should't include
	 * MOD2, aka NUM LOCK. */
	if (event->state & (GDK_MOD1_MASK | GDK_MOD3_MASK |
			    GDK_MOD4_MASK | GDK_MOD5_MASK))
		pass_on = TRUE;

	/* Calculate tentative physical values */
	if (!pass_on) {
            switch (event->keyval) {
                case GDK_Return:
                case GDK_KP_Enter:
                        g_signal_emit_by_name(sheet->reg, "activate_cursor");
                        return TRUE;
                        break;
		case GDK_Tab:
                case GDK_ISO_Left_Tab:
                        if (event->state & GDK_SHIFT_MASK) {
                                direction = GNC_TABLE_TRAVERSE_LEFT;
                                gnc_table_move_tab (table, &new_virt_loc,
                                                    FALSE);
                        }
                        else {
                                direction = GNC_TABLE_TRAVERSE_RIGHT;
                                gnc_table_move_tab (table, &new_virt_loc,
                                                    TRUE);
                        }
                        break;
                case GDK_KP_Page_Up:
		case GDK_Page_Up:
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
                case GDK_KP_Page_Down:
		case GDK_Page_Down:
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
                case GDK_KP_Up:
		case GDK_Up:
			direction = GNC_TABLE_TRAVERSE_UP;
                        gnc_table_move_vertical_position (table,
                                                          &new_virt_loc, -1);
			break;
                case GDK_KP_Down:
                case GDK_Down:
                case GDK_Menu:
                        if (event->keyval == GDK_Menu ||
                            event->state & GDK_CONTROL_MASK)
                        {
                                GncItemEdit *item_edit;

                                item_edit = GNC_ITEM_EDIT (sheet->item_editor);

                                if (gnc_table_confirm_change (table,
                                                              cur_virt_loc))
                                        gnc_item_edit_show_popup (item_edit);

                                return TRUE;
                        }

			direction = GNC_TABLE_TRAVERSE_DOWN;
                        gnc_table_move_vertical_position (table,
                                                          &new_virt_loc, 1);
			break;
                case GDK_Control_L:
                case GDK_Control_R:
		case GDK_Shift_L:
		case GDK_Shift_R:
		case GDK_Alt_L:
		case GDK_Alt_R:
                        pass_on = TRUE;
                        break;
		default:
                        if (gnucash_sheet_clipboard_event(sheet, event))
                                return TRUE;

			pass_on = TRUE;
			break;
            }
	}

	/* Forward the keystroke to the input line */
	if (pass_on)
		return gtk_widget_event (sheet->entry, (GdkEvent *) event);

	abort_move = gnc_table_traverse_update (table, cur_virt_loc,
                                                direction, &new_virt_loc);

	/* If that would leave the register, abort */
	if (abort_move)
                return TRUE;

	gnucash_sheet_cursor_move (sheet, new_virt_loc);

        /* return true because we handled the key press */
        return TRUE;
}


static void
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


void
gnucash_register_goto_virt_cell (GnucashRegister *reg,
                                 VirtualCellLocation vcell_loc)
{
        GnucashSheet *sheet;
        VirtualLocation virt_loc;

        g_return_if_fail(reg != NULL);
        g_return_if_fail(GNUCASH_IS_REGISTER(reg));

        sheet = GNUCASH_SHEET(reg->sheet);

        virt_loc.vcell_loc = vcell_loc;
        virt_loc.phys_row_offset = 0;
        virt_loc.phys_col_offset = 0;

        gnucash_sheet_goto_virt_loc(sheet, virt_loc);
}

void
gnucash_register_goto_virt_loc (GnucashRegister *reg,
                                VirtualLocation virt_loc)
{
        GnucashSheet *sheet;

        g_return_if_fail(reg != NULL);
        g_return_if_fail(GNUCASH_IS_REGISTER(reg));

        sheet = GNUCASH_SHEET(reg->sheet);

        gnucash_sheet_goto_virt_loc(sheet, virt_loc);
}

void
gnucash_register_goto_next_virt_row (GnucashRegister *reg)
{
        GnucashSheet *sheet;
        VirtualLocation virt_loc;
        int start_virt_row;

        g_return_if_fail (reg != NULL);
        g_return_if_fail (GNUCASH_IS_REGISTER(reg));

        sheet = GNUCASH_SHEET(reg->sheet);

        gnucash_cursor_get_virt (GNUCASH_CURSOR(sheet->cursor), &virt_loc);

        /* Move down one physical row at a time until we
         * reach the next visible virtual cell. */
        start_virt_row = virt_loc.vcell_loc.virt_row;
        do
        {
          if (!gnc_table_move_vertical_position (sheet->table, &virt_loc, 1))
            return;
        } while (start_virt_row == virt_loc.vcell_loc.virt_row);

        if (virt_loc.vcell_loc.virt_row >= sheet->num_virt_rows)
                return;

        virt_loc.phys_row_offset = 0;
        virt_loc.phys_col_offset = 0;

        gnucash_sheet_goto_virt_loc (sheet, virt_loc);
}

void
gnucash_register_goto_next_matching_row (GnucashRegister *reg,
                                         VirtualLocationMatchFunc match,
                                         gpointer user_data)
{
        GnucashSheet *sheet;
        SheetBlockStyle *style;
        VirtualLocation virt_loc;

        g_return_if_fail (reg != NULL);
        g_return_if_fail (GNUCASH_IS_REGISTER(reg));
        g_return_if_fail (match != NULL);

        sheet = GNUCASH_SHEET (reg->sheet);

        gnucash_cursor_get_virt (GNUCASH_CURSOR(sheet->cursor), &virt_loc);

        do
        {
                if (!gnc_table_move_vertical_position (sheet->table,
                                                       &virt_loc, 1))
                        return;

                if (virt_loc.vcell_loc.virt_row >= sheet->num_virt_rows)
                        return;

                style = gnucash_sheet_get_style (sheet, virt_loc.vcell_loc);
                if (!style || !style->cursor)
                        return;
        } while (!match (virt_loc, user_data));

        virt_loc.phys_row_offset = 0;
        virt_loc.phys_col_offset = 0;

        gnucash_sheet_goto_virt_loc (sheet, virt_loc);
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

        if (block == NULL)
                return FALSE;

        table = sheet->table;

        vcell = gnc_table_get_virtual_cell (table, vcell_loc);

        if (block->style && (block->style != style))
        {
                gnucash_style_unref (block->style);
                block->style = NULL;
        }

        block->visible = (vcell) ? vcell->visible : TRUE;

        if (block->style == NULL)
        {
                block->style = style;
                gnucash_style_ref(block->style);
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

        g_return_val_if_fail (virt_col >= 0, 0);
        g_return_val_if_fail (virt_col < sheet->num_virt_cols, 0);
        g_return_val_if_fail (cell_col >= 0, 0);

        for (virt_row = 0; virt_row < sheet->num_virt_rows ; virt_row++) {
                VirtualCellLocation vcell_loc = { virt_row, virt_col };

                block = gnucash_sheet_get_block (sheet, vcell_loc);
                style = block->style;

                if (!style)
                        continue;

                if (cell_col < style->ncols)
                        for (cell_row = 0; cell_row < style->nrows; cell_row++)
                        {
                                VirtualLocation virt_loc;
                                const char *text;

                                virt_loc.vcell_loc = vcell_loc;
                                virt_loc.phys_row_offset = cell_row;
                                virt_loc.phys_col_offset = cell_col;

                                if (virt_row == 0) {
                                        text = gnc_table_get_label
                                                (sheet->table, virt_loc);
                                }
                                else {
                                        text = gnc_table_get_entry
                                                (sheet->table, virt_loc);
                                }

				pango_layout_set_text (layout, text, strlen (text));
				pango_layout_get_pixel_size (layout, &width, NULL);

                                width += 2 * CELL_HPADDING;

                                max = MAX (max, width);
                        }
        }

        g_object_unref (layout);

        return max;
}

void
gnucash_sheet_set_scroll_region (GnucashSheet *sheet)
{
        int height, width;
        GtkWidget *widget;
        double x, y;

        if (!sheet)
                return;

        widget = GTK_WIDGET(sheet);

        if (!sheet->header_item || !GNC_HEADER(sheet->header_item)->style)
                return;

        gnome_canvas_get_scroll_region (GNOME_CANVAS(sheet),
                                        NULL, NULL, &x, &y);

        height = MAX (sheet->height, widget->allocation.height);
        width  = MAX (sheet->width, widget->allocation.width);

        if (width != (int)x || height != (int)y)
                gnome_canvas_set_scroll_region (GNOME_CANVAS(sheet),
                                                0, 0, width, height);
}

static void
gnucash_sheet_block_destroy (gpointer _block, gpointer user_data)
{
        SheetBlock *block = _block;

        if (block == NULL)
                return;

        if (block->style) {
                gnucash_style_unref (block->style);
                block->style = NULL;
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
        for (i = 0; i < table->num_virt_rows; i++) {
                width = 0;

                for (j = 0; j < table->num_virt_cols; j++) {
                        VirtualCellLocation vcell_loc = { i, j };

                        block = gnucash_sheet_get_block (sheet, vcell_loc);

                        block->origin_x = width;
                        block->origin_y = height;

                        if (block->visible)
                                width += block->style->dimensions->width;
                }

                if (i > 0 && block->visible)
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

static gboolean
gnucash_sheet_selection_clear (GtkWidget          *widget,
                               GdkEventSelection  *event)
{
        GnucashSheet *sheet;

        g_return_val_if_fail(widget != NULL, FALSE);
        g_return_val_if_fail(GNUCASH_IS_SHEET(widget), FALSE);

        sheet = GNUCASH_SHEET(widget);

        return gnc_item_edit_selection_clear(GNC_ITEM_EDIT(sheet->item_editor), event);
}

static void
gnucash_sheet_selection_get (GtkWidget         *widget,
                             GtkSelectionData  *selection_data,
                             guint              info,
                             guint              time)
{
        GnucashSheet *sheet;

        g_return_if_fail(widget != NULL);
        g_return_if_fail(GNUCASH_IS_SHEET(widget));

        sheet = GNUCASH_SHEET(widget);

        gnc_item_edit_selection_get(GNC_ITEM_EDIT(sheet->item_editor),
                                selection_data, info, time);
}

static void
gnucash_sheet_selection_received (GtkWidget          *widget,
                                  GtkSelectionData   *selection_data,
                                  guint               time)
{
        GnucashSheet *sheet;

        g_return_if_fail(widget != NULL);
        g_return_if_fail(GNUCASH_IS_SHEET(widget));

        sheet = GNUCASH_SHEET(widget);

        gnc_item_edit_selection_received(GNC_ITEM_EDIT(sheet->item_editor),
                                     selection_data, time);
}

static void
gnucash_sheet_realize_entry (GnucashSheet *sheet, GtkWidget *entry)
{
	gtk_widget_realize (entry);
}

/*************************************************************/

/* This code is one big hack to use gtkrc to set cell colors in a
 * register.  Because the cells are just boxes drawn on a gnome
 * canvas, there's no way to specify the individual cells in a gtkrc
 * file.  This code creates four hidden GtkEntry widgets and names
 * them so that they *can* be specified in gtkrc.  It then looks up
 * the colors specified on these hidden widgets and uses it for the
 * cells drawn on the canvas.  This code should all go away whenever
 * the register is rewritten.
 */

/** Map a cell type to a gtkrc specified color. */
GdkColor *
get_gtkrc_color (GnucashSheet *sheet,
		 RegisterColor field_type)
{
	GtkWidget *widget = NULL;
	GtkStyle *style;
	GdkColor *white;
	GdkColor *color = NULL;

	white = gnucash_color_argb_to_gdk (0xFFFFFF);
	switch (field_type) {
	  default:
		return white;

	  case COLOR_HEADER:
		widget = sheet->header_color;
		break;

	  case COLOR_PRIMARY:
	  case COLOR_PRIMARY_ACTIVE:
		widget = sheet->primary_color;
		break;

	  case COLOR_SECONDARY:
	  case COLOR_SECONDARY_ACTIVE:
		widget = sheet->secondary_color;
		break;

	  case COLOR_SPLIT:
	  case COLOR_SPLIT_ACTIVE:
		widget = sheet->split_color;
		break;
	}

	style = gtk_widget_get_style(widget);
	if (!style)
		return white;

	switch (field_type) {
	  default:
		return white;

	  case COLOR_HEADER:
	  case COLOR_PRIMARY:
	  case COLOR_SECONDARY:
	  case COLOR_SPLIT:
		color = &style->base[GTK_STATE_NORMAL];
		break;

	  case COLOR_PRIMARY_ACTIVE:
	  case COLOR_SECONDARY_ACTIVE:
	  case COLOR_SPLIT_ACTIVE:
		color = &style->base[GTK_STATE_SELECTED];
		break;
	}

        gnucash_color_alloc_gdk(color);
	return color;
}

/** Create the entries used for nameing register colors in gtkrc. */
static void
gnucash_sheet_create_color_hack(GnucashSheet *sheet)
{
	sheet->header_color    = gtk_entry_new();
	sheet->primary_color   = gtk_entry_new();
	sheet->secondary_color = gtk_entry_new();
	sheet->split_color     = gtk_entry_new();

	gtk_widget_set_name(sheet->header_color,    "header_color");
	gtk_widget_set_name(sheet->primary_color,   "primary_color");
	gtk_widget_set_name(sheet->secondary_color, "secondary_color");
	gtk_widget_set_name(sheet->split_color,     "split_color");

	g_signal_connect_after(sheet, "realize",
			       G_CALLBACK(gnucash_sheet_realize_entry),
			       sheet->header_color);
	g_signal_connect_after(sheet, "realize",
			       G_CALLBACK(gnucash_sheet_realize_entry),
			       sheet->primary_color);
	g_signal_connect_after(sheet, "realize",
			       G_CALLBACK(gnucash_sheet_realize_entry),
			       sheet->secondary_color);
	g_signal_connect_after(sheet, "realize",
			       G_CALLBACK(gnucash_sheet_realize_entry),
			       sheet->split_color);
}

/*************************************************************/

static void
gnucash_sheet_class_init (GnucashSheetClass *class)
{
        GObjectClass *gobject_class;
        GtkWidgetClass *widget_class;
        GnomeCanvasClass *canvas_class;

        gobject_class = G_OBJECT_CLASS (class);
        widget_class = GTK_WIDGET_CLASS (class);
        canvas_class = GNOME_CANVAS_CLASS (class);

        sheet_parent_class = g_type_class_peek_parent (class);

        /* Method override */
        gobject_class->finalize = gnucash_sheet_finalize;

        widget_class->realize = gnucash_sheet_realize;

        widget_class->size_request = gnucash_sheet_size_request;
        widget_class->size_allocate = gnucash_sheet_size_allocate;

        widget_class->focus_in_event = gnucash_sheet_focus_in_event;
        widget_class->focus_out_event = gnucash_sheet_focus_out_event;

        widget_class->key_press_event = gnucash_sheet_key_press_event;
        widget_class->button_press_event = gnucash_button_press_event;
        widget_class->button_release_event = gnucash_button_release_event;
        widget_class->motion_notify_event = gnucash_motion_event;
        widget_class->scroll_event = gnucash_scroll_event;

        widget_class->selection_clear_event = gnucash_sheet_selection_clear;
        widget_class->selection_received = gnucash_sheet_selection_received;
        widget_class->selection_get = gnucash_sheet_selection_get;
}


static void
gnucash_sheet_init (GnucashSheet *sheet)
{
        GnomeCanvas *canvas = GNOME_CANVAS (sheet);

        GTK_WIDGET_SET_FLAGS (canvas, GTK_CAN_FOCUS);
        GTK_WIDGET_SET_FLAGS (canvas, GTK_CAN_DEFAULT);

        sheet->top_block = 1;
        sheet->bottom_block = 1;
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
                                     gnucash_sheet_block_destroy, NULL);
}


GType
gnucash_sheet_get_type (void)
{
        static GType gnucash_sheet_type = 0;

        if (!gnucash_sheet_type){
                static const GTypeInfo gnucash_sheet_info = {
                        sizeof (GnucashSheetClass),
			NULL,		/* base_init */
			NULL,		/* base_finalize */
                        (GClassInitFunc) gnucash_sheet_class_init,
			NULL,		/* class_finalize */
			NULL,		/* class_data */
                        sizeof (GnucashSheet),
			0,		/* n_preallocs */
                        (GInstanceInitFunc) gnucash_sheet_init
                };

                gnucash_sheet_type =
			g_type_register_static (gnome_canvas_get_type (),
						"GnucashSheet",
						&gnucash_sheet_info, 0);
        }

        return gnucash_sheet_type;
}

GtkWidget *
gnucash_sheet_new (Table *table)
{
        GnucashSheet *sheet;
        GnomeCanvas *sheet_canvas;
        GnomeCanvasItem *item;
        GnomeCanvasGroup *sheet_group;

        g_return_val_if_fail (table != NULL, NULL);

        sheet = gnucash_sheet_create (table);

        /* handy shortcuts */
        sheet_canvas = GNOME_CANVAS (sheet);
        sheet_group = gnome_canvas_root (GNOME_CANVAS(sheet));

        /* The grid */
        item = gnome_canvas_item_new (sheet_group,
                                      gnucash_grid_get_type (),
                                      "sheet", sheet,
                                      NULL);
        sheet->grid = item;

        /* some register data */
        sheet->dimensions_hash_table = g_hash_table_new_full (g_int_hash,
							      g_int_equal,
							      g_free, NULL);

        /* The cursor */
        sheet->cursor = gnucash_cursor_new (sheet_group);
        gnome_canvas_item_set (sheet->cursor,
                               "sheet", sheet,
                               "grid", sheet->grid,
                               NULL);

        /* The entry widget */
        sheet->entry = gtk_entry_new ();
        g_object_ref_sink(sheet->entry);

        /*gtk_layout_put (GTK_LAYOUT (sheet), sheet->entry, 0, 0);*/

        /* set up the editor */
        sheet->item_editor = gnc_item_edit_new(sheet_group, sheet, sheet->entry);

        gnome_canvas_item_hide (GNOME_CANVAS_ITEM(sheet->item_editor));

	/* The GtkEntry must be realized in order to send events to
	 * it.  We don't want to show it on the screen, but can't
	 * realize it until after the window and all its parent
	 * widgets have been realized. Thus this callback. */
	g_signal_connect_after(sheet, "realize",
			       G_CALLBACK(gnucash_sheet_realize_entry),
			       sheet->entry);

	gnucash_sheet_refresh_from_gconf(sheet);
	gnucash_sheet_create_color_hack(sheet);

        return GTK_WIDGET(sheet);
}


static void
gnucash_register_class_init (GnucashRegisterClass *class)
{
	GObjectClass *gobject_class;
        GtkWidgetClass *widget_class;
        GtkTableClass *table_class;

	gobject_class = G_OBJECT_CLASS (class);
        widget_class = (GtkWidgetClass *) class;
        table_class = (GtkTableClass *) class;

        register_parent_class = g_type_class_peek_parent (class);

        register_signals[ACTIVATE_CURSOR] =
                g_signal_new("activate_cursor",
			     G_TYPE_FROM_CLASS(gobject_class),
			     G_SIGNAL_RUN_LAST,
			     G_STRUCT_OFFSET(GnucashRegisterClass,
					     activate_cursor),
			     NULL, NULL,
			     g_cclosure_marshal_VOID__VOID,
			     G_TYPE_NONE, 0);

        register_signals[REDRAW_ALL] =
                g_signal_new("redraw_all",
			     G_TYPE_FROM_CLASS(gobject_class),
			     G_SIGNAL_RUN_LAST,
			     G_STRUCT_OFFSET(GnucashRegisterClass,
					     redraw_all),
			     NULL, NULL,
			     g_cclosure_marshal_VOID__VOID,
			     G_TYPE_NONE, 0);

        register_signals[REDRAW_HELP] =
                g_signal_new("redraw_help",
			     G_TYPE_FROM_CLASS(gobject_class),
			     G_SIGNAL_RUN_LAST,
			     G_STRUCT_OFFSET(GnucashRegisterClass,
					     redraw_help),
			     NULL, NULL,
			     g_cclosure_marshal_VOID__VOID,
			     G_TYPE_NONE, 0);

        class->activate_cursor = NULL;
        class->redraw_all = NULL;
        class->redraw_help = NULL;
}


static void
gnucash_register_init (GnucashRegister *g_reg)
{
        GtkTable *table = GTK_TABLE(g_reg);

        GTK_WIDGET_UNSET_FLAGS (table, GTK_CAN_FOCUS);
        GTK_WIDGET_UNSET_FLAGS (table, GTK_CAN_DEFAULT);

        gtk_table_set_homogeneous (table, FALSE);
        gtk_table_resize (table, 3, 2);
}


GType
gnucash_register_get_type (void)
{
        static GType gnucash_register_type = 0;

        if (!gnucash_register_type) {
                static const GTypeInfo gnucash_register_info = {
                        sizeof (GnucashRegisterClass),
			NULL,		/* base_init */
			NULL,		/* base_finalize */
                        (GClassInitFunc) gnucash_register_class_init,
			NULL,		/* class_finalize */
			NULL,		/* class_data */
                        sizeof (GnucashRegister),
			0,		/* n_preallocs */
                        (GInstanceInitFunc) gnucash_register_init,
                };

                gnucash_register_type = g_type_register_static
			(gtk_table_get_type (),
			 "GnucashRegister",
			 &gnucash_register_info, 0);
        }

        return gnucash_register_type;
}


void
gnucash_register_attach_popup (GnucashRegister *reg,
                               GtkWidget *popup,
                               gpointer data)
{
        g_return_if_fail (GNUCASH_IS_REGISTER(reg));
        g_return_if_fail (reg->sheet != NULL);
	if (popup)
	  g_return_if_fail (GTK_IS_WIDGET(popup));

        gnucash_sheet_set_popup (GNUCASH_SHEET (reg->sheet), popup, data);
}


GtkWidget *
gnucash_register_new (Table *table)
{
        GnucashRegister *reg;
        GtkWidget *header_canvas;
        GtkWidget *widget;
        GtkWidget *sheet;
        GtkWidget *scrollbar;
	GtkWidget *box;

        reg = g_object_new (GNUCASH_TYPE_REGISTER, NULL);
        widget = GTK_WIDGET(reg);

        sheet = gnucash_sheet_new (table);
        reg->sheet = sheet;
        GNUCASH_SHEET(sheet)->reg = widget;

        header_canvas = gnc_header_new (GNUCASH_SHEET(sheet));
        reg->header_canvas = header_canvas;

        gtk_table_attach (GTK_TABLE(widget), header_canvas,
                          0, 1, 0, 1,
                          GTK_FILL | GTK_EXPAND | GTK_SHRINK,
                          GTK_FILL,
                          0, 0);
        gtk_widget_show(header_canvas);

        gtk_table_attach (GTK_TABLE(widget), sheet,
                          0, 1, 1, 2,
                          GTK_FILL | GTK_EXPAND | GTK_SHRINK,
                          GTK_FILL | GTK_EXPAND | GTK_SHRINK,
                          0, 0);
        gtk_widget_show(sheet);

        gtk_table_attach (GTK_TABLE(widget), GNUCASH_SHEET(sheet)->entry,
                          0, 1, 2, 3,
                          GTK_FILL | GTK_EXPAND | GTK_SHRINK,
                          GTK_FILL | GTK_EXPAND | GTK_SHRINK,
                          0, 0);
        gtk_widget_hide(GNUCASH_SHEET(sheet)->entry);
	gtk_widget_set_no_show_all(GNUCASH_SHEET(sheet)->entry, TRUE);

        scrollbar = gtk_vscrollbar_new(GNUCASH_SHEET(sheet)->vadj);
        gtk_table_attach (GTK_TABLE(widget), GTK_WIDGET(scrollbar),
                          1, 2, 0, 3,
                          GTK_FILL,
                          GTK_EXPAND | GTK_SHRINK | GTK_FILL,
                          0, 0);
        reg->vscrollbar = scrollbar;
        gtk_widget_show(scrollbar);

        scrollbar = gtk_hscrollbar_new(GNUCASH_SHEET(sheet)->hadj);
        gtk_table_attach (GTK_TABLE(widget), GTK_WIDGET(scrollbar),
                          0, 1, 3, 4,
                          GTK_EXPAND | GTK_SHRINK | GTK_FILL,
                          GTK_FILL,
                          0, 0);
        reg->hscrollbar = scrollbar;
        gtk_widget_show(scrollbar);
        reg->hscrollbar_visible = TRUE;

	/* The gtkrc color helper widgets need to be part of a window
	 * hierarchy so they can be realized. Stick them in a box
	 * underneath the register, but don't show the box to the
	 * user. */
	box = gtk_hbox_new(FALSE, 0);
	gtk_widget_set_no_show_all(GTK_WIDGET(box), TRUE);
	gtk_box_pack_start_defaults(GTK_BOX(box),
				    GNUCASH_SHEET(sheet)->header_color);
	gtk_box_pack_start_defaults(GTK_BOX(box),
				    GNUCASH_SHEET(sheet)->primary_color);
	gtk_box_pack_start_defaults(GTK_BOX(box),
				    GNUCASH_SHEET(sheet)->secondary_color);
	gtk_box_pack_start_defaults(GTK_BOX(box),
				    GNUCASH_SHEET(sheet)->split_color);

        gtk_table_attach (GTK_TABLE(widget), box,
                          0, 1, 4, 5,
                          GTK_FILL | GTK_EXPAND | GTK_SHRINK,
                          GTK_FILL | GTK_EXPAND | GTK_SHRINK,
                          0, 0);

        return widget;
}


void gnucash_register_set_moved_cb (GnucashRegister *reg,
				    GFunc cb, gpointer cb_data)
{
	GnucashSheet *sheet;

	if (!reg || !reg->sheet)
		return;
	sheet = GNUCASH_SHEET(reg->sheet);
	sheet->moved_cb = cb;
	sheet->moved_cb_data = cb_data;
}


/*
  Local Variables:
  c-basic-offset: 8
  End:
*/
