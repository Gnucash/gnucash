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

/*
 * The Gnucash Sheet widget
 *
 *  Based heavily on the Gnumeric Sheet widget.
 *
 * Author:
 *     Heath Martin <martinh@pegasus.cc.ucf.edu>
 */


#include "gnucash-sheet.h"

#include "gnucash-color.h"
#include "gnucash-grid.h"
#include "gnucash-cursor.h"
#include "gnucash-style.h"
#include "gnucash-header.h"
#include "gnucash-item-edit.h"
#include "util.h"

#define DEFAULT_REGISTER_HEIGHT 400
#define DEFAULT_REGISTER_WIDTH 640


static void gnucash_sheet_cell_set_from_table (GnucashSheet *sheet,
					       gint virt_row, gint virt_col,
                                               gint cell_row, gint cell_col);

static const char *gnucash_sheet_block_get_text (GnucashSheet *sheet,
						 gint virt_row, gint virt_col,
						 gint cell_row, gint cell_col);

static void gnucash_sheet_start_editing_at_cursor (GnucashSheet *sheet,
                                                   gboolean blankp,
						   gboolean cursorp);

static void gnucash_sheet_cursor_move (GnucashSheet *sheet,
                                       gint virt_row, gint virt_col,
                                       gint cell_row, gint cell_col,
                                       gboolean changed_cells);

static void gnucash_sheet_deactivate_cursor_cell (GnucashSheet *sheet);
static void gnucash_sheet_activate_cursor_cell (GnucashSheet *sheet,
                                                gboolean changed_cells);
static void gnucash_sheet_stop_editing (GnucashSheet *sheet);
static void gnucash_sheet_block_destroy (GnucashSheet *sheet, gint virt_row,
					 gint virt_col);

static void gnucash_sheet_update_adjustments (GnucashSheet *sheet);


static GnomeCanvasClass *sheet_parent_class;
static GtkTableClass *register_parent_class;


gint
gnucash_sheet_cell_valid (GnucashSheet *sheet, gint virt_row, gint virt_col,
                          gint cell_row, gint cell_col)
{
        gint valid;
        SheetBlockStyle *style;
#if 0
        Table *table = sheet->table;
        gint p_row, p_col;
#endif

        valid = ( virt_row >= 1 && virt_row < sheet->num_virt_rows
                  && virt_col >= 0 && virt_col  < sheet->num_virt_cols );

        if (valid) {
                style = gnucash_sheet_get_style (sheet, virt_row, virt_col);

                valid = valid && ( cell_row >= 0 && cell_row < style->nrows &&
                                   cell_col >= 0 && cell_col < style->ncols);
        }
#if 0
        if (valid) {
                p_row = table->rev_locators[virt_row][virt_col]->phys_row +
			cell_row;
                p_col = table->rev_locators[virt_row][virt_col]->phys_col +
			cell_col;

                valid = valid && gnc_register_cell_valid(sheet->table,
							 p_row, p_col);
        }
#endif

        return valid;
}


/* virt_row, virt_col is the position of the current cursor block,
   cell_row, cell_col is the position of the active cell within the block,
   as an offset from the block origin
*/
void
gnucash_sheet_cursor_set (GnucashSheet *sheet, int virt_row, int virt_col,
			  int cell_row, int cell_col)
{
        g_return_if_fail (sheet != NULL);
        g_return_if_fail (GNUCASH_IS_SHEET (sheet));

        g_return_if_fail (virt_row >= 0 || virt_row <= sheet->num_virt_rows);
        g_return_if_fail (virt_col >= 0 || virt_col <= sheet->num_virt_cols);

        gnucash_cursor_set (GNUCASH_CURSOR(sheet->cursor), virt_row, virt_col,
			    cell_row, cell_col);
}

void
gnucash_sheet_cursor_set_from_table (GnucashSheet *sheet)
{
        Table *table;
        gint cell_row, cell_col;
        gint p_row, p_col;
        GnucashCursor *cursor;

        g_return_if_fail (sheet != NULL);
        g_return_if_fail (GNUCASH_IS_SHEET(sheet));

        table = sheet->table;
        cursor = GNUCASH_CURSOR(sheet->cursor);
        p_row = table->current_cursor_phys_row;
        p_col = table->current_cursor_phys_col;

        if (p_row >= 0 && p_row < table->num_phys_rows
            && p_col >= 0 && p_col < table->num_phys_cols) {

                cell_row = table->locators[p_row][p_col]->phys_row_offset;
                cell_col = table->locators[p_row][p_col]->phys_col_offset;

                gnucash_sheet_make_cell_visible(sheet,
						table->current_cursor_virt_row,
						table->current_cursor_virt_col,
						cell_row,
						cell_col);

                gnucash_sheet_update_adjustments(sheet);
                
                gnucash_sheet_cursor_set(sheet,
					 table->current_cursor_virt_row,
					 table->current_cursor_virt_col,
					 cell_row,
					 cell_col);
        }
}


void
gnucash_sheet_hide_editing_cursor (GnucashSheet *sheet)
{
        if (sheet->item_editor != NULL) {
                gnome_canvas_item_hide(GNOME_CANVAS_ITEM (sheet->item_editor));
		item_edit_hide_list(ITEM_EDIT(sheet->item_editor));
	}
}

static void
gnucash_sheet_stop_editing (GnucashSheet *sheet)
{

        if (sheet->insert_signal  > 0)
                gtk_signal_disconnect (GTK_OBJECT(sheet->entry),
				       sheet->insert_signal);
        if (sheet->delete_signal  > 0)
                gtk_signal_disconnect (GTK_OBJECT(sheet->entry),
				       sheet->delete_signal);

        sheet->insert_signal = 0;
        sheet->delete_signal = 0;

        gnucash_sheet_hide_editing_cursor (sheet);

        sheet->editing = FALSE;
}

        
static void
gnucash_sheet_deactivate_cursor_cell (GnucashSheet *sheet)
{
        int p_row, p_col;
        Table *table = sheet->table;
        const char *new_text = NULL;
        const char *old_text;
        int virt_row, virt_col, cell_row, cell_col;

        gnucash_cursor_get_phys (GNUCASH_CURSOR(sheet->cursor),
				 &p_row, &p_col);

        gnucash_cursor_get_virt (GNUCASH_CURSOR(sheet->cursor),
                                 &virt_row, &virt_col, &cell_row, &cell_col);

        gnucash_sheet_stop_editing (sheet);

        if (gnc_register_cell_valid (table, p_row, p_col)) {
		old_text = gnucash_sheet_block_get_text(sheet, virt_row,
							virt_col, cell_row,
							cell_col);

		new_text = gnc_table_leave_update(table, p_row, p_col,
						  old_text);

		if (new_text)
			gnucash_sheet_cell_set_from_table (sheet, virt_row,
							   virt_col, cell_row,
							   cell_col);
        }

        gnucash_sheet_redraw_block (sheet, virt_row, virt_col);
}


static void
gnucash_sheet_activate_cursor_cell (GnucashSheet *sheet,
                                    gboolean changed_cells)
{
        int p_row, p_col;
        Table *table = sheet->table;
        const char *new_text = NULL;
        int virt_row, virt_col, cell_row, cell_col;
        SheetBlockStyle *style;


        /* Hmm, this shouldn't happen, but let's be sure */
        if (sheet->editing)
                gnucash_sheet_deactivate_cursor_cell (sheet);

        gnucash_cursor_get_phys (GNUCASH_CURSOR(sheet->cursor),
				 &p_row, &p_col);
        gnucash_cursor_get_virt (GNUCASH_CURSOR(sheet->cursor),
                                 &virt_row, &virt_col, &cell_row, &cell_col);

        wrapVerifyCursorPosition (table, p_row, p_col);

        style = gnucash_sheet_get_style (sheet, virt_row, virt_col);
        if (style->cursor_type == GNUCASH_CURSOR_HEADER ||
            !gnc_register_cell_valid (table, p_row, p_col) )
                return;

        new_text = gnc_table_enter_update (table, p_row, p_col);

	if (new_text != NULL)
		gnucash_sheet_cell_set_from_table (sheet, virt_row, virt_col,
						   cell_row, cell_col);
	else
		gnucash_sheet_start_editing_at_cursor (sheet, FALSE,
                                                       !changed_cells);

        gtk_widget_grab_focus (GTK_WIDGET(sheet));
}


static void
gnucash_sheet_cursor_move (GnucashSheet *sheet, gint virt_row, gint virt_col,
                           gint cell_row, gint cell_col,
                           gboolean changed_cells)
{
        if (!gnucash_sheet_cell_valid (sheet,
                                       virt_row, virt_col,
                                       cell_row, cell_col))
                return;

        gnucash_sheet_deactivate_cursor_cell (sheet);

        gnucash_sheet_make_cell_visible (sheet,
                                         virt_row, virt_col,
                                         cell_row, cell_col);

        gnucash_sheet_cursor_set (sheet,
                                  virt_row, virt_col,
                                  cell_row, cell_col);

        gnucash_sheet_activate_cursor_cell (sheet, changed_cells);
}


int
gnucash_sheet_can_move_cursor (GnucashSheet *gsheet)
{
        return TRUE;
}


/* assumes a valid top block */
void
gnucash_sheet_compute_visible_range (GnucashSheet *sheet)
{
        SheetBlockStyle *style;
        gint block;
        Table *table;
        gint y;
        gint height;
        
        g_return_if_fail (sheet != NULL);
        g_return_if_fail (GNUCASH_IS_SHEET (sheet));

        table = sheet->table;
        height = GTK_WIDGET(sheet)->allocation.height;

        block = sheet->top_block;
        y = 0;
        
        do {
                style = gnucash_sheet_get_style (sheet, block, 0);
                if (y + style->height >= height)
                        break;
                y += style->height;
                block++;
        } while (block < sheet->num_virt_rows - 1);

        sheet->bottom_block = block;

        /* FIXME */
        sheet->left_block = 0;
        sheet->right_block = 0;
}


void
gnucash_sheet_block_pixel_origin (GnucashSheet *sheet, gint vrow, gint vcol,
				  gint *x, gint *y)
{
        gint i;
        gint wx = 0;
        gint wy = 0;
        SheetBlockStyle *style;

        g_return_if_fail (sheet != NULL);
        g_return_if_fail (GNUCASH_IS_SHEET(sheet));

        if ( vrow <= 0 || vrow > sheet->num_virt_rows ||
             vcol < 0 || vcol > sheet->num_virt_cols)
                return;

        for (i = 1; i < vrow; i++) {
                style = gnucash_sheet_get_style (sheet, i, 0);
                if (!style)
                        return;
                wy += style->height;
        }

        for (i = 0; i < vcol; i++) {
                style = gnucash_sheet_get_style (sheet, vrow, 0);
                if (!style)
                        return;
                wx += style->width;
        }

        if (x)
                *x = wx;
        if (y)
                *y = wy;
}


void
gnucash_sheet_set_top_row (GnucashSheet *sheet, gint new_top_row, gint align)
{
        gint x, y;
        gint cx, cy;
        gint diff = 0;

        g_return_if_fail (sheet != NULL);
        g_return_if_fail (GNUCASH_IS_SHEET(sheet));

        new_top_row = MAX (new_top_row, 1);
        new_top_row = MIN (new_top_row, sheet->num_virt_rows - 1);

        if (align != GNUCASH_ALIGN_SAME)
                sheet->alignment = align;

        gnome_canvas_get_scroll_offsets (GNOME_CANVAS(sheet), &cx, &cy);

        x = cx;

        gnucash_sheet_block_pixel_origin (sheet, new_top_row, 0, NULL, &y);

        if (sheet->alignment == GNUCASH_ALIGN_BOTTOM) {
                gint height = GTK_WIDGET(sheet)->allocation.height;
                gint distance;

                distance = gnucash_sheet_row_get_distance
			(sheet, sheet->top_block, sheet->bottom_block + 1);

                if (distance > height)
                        diff = distance - height;
        }
        y += diff;

        sheet->top_block_offset = -diff;


        if (x != cx || y != cy) {
                sheet->top_block = new_top_row;
                gnucash_sheet_compute_visible_range(sheet);
                gnome_canvas_scroll_to (GNOME_CANVAS(sheet), x, y);
        }
        gnucash_sheet_update_adjustments (sheet);        
}


void
gnucash_sheet_make_cell_visible (GnucashSheet *sheet,
                                 gint virt_row, gint virt_col,
                                 gint cell_row, gint cell_col)
{
        g_return_if_fail (sheet != NULL);
        g_return_if_fail (GNUCASH_IS_SHEET (sheet));

        if (!gnucash_sheet_cell_valid (sheet, virt_row, virt_col,
				       cell_row, cell_col))
                return;

        if (virt_row <= sheet->top_block)
                gnucash_sheet_set_top_row (sheet, virt_row, GNUCASH_ALIGN_TOP);
        else if (virt_row >= sheet->bottom_block)
                gnucash_sheet_set_top_row (sheet,
                                           sheet->top_block +
					   (virt_row - sheet->bottom_block),
                                           GNUCASH_ALIGN_BOTTOM);

        sheet->vadj->value = sheet->top_block;
        gnucash_sheet_update_adjustments (sheet);
}


/* FIXME:  do the horizontal adjustment, too? */
static void
gnucash_sheet_update_adjustments (GnucashSheet *sheet)
{
        GtkAdjustment *adj;

        g_return_if_fail (sheet != NULL);
        g_return_if_fail (GNUCASH_IS_SHEET (sheet));
        g_return_if_fail (sheet->vadj != NULL);

        adj = sheet->vadj;

        adj->lower = 1;
        adj->upper = MAX(sheet->bottom_block, sheet->num_virt_rows);
        adj->page_size = sheet->bottom_block - sheet->top_block + 1;
	adj->page_increment = adj->page_size - 1;

        gtk_signal_emit_by_name (GTK_OBJECT(adj), "changed");
}


void
gnucash_sheet_vadjustment_value_changed (GtkAdjustment *adj,
					 GnucashSheet *sheet)
{
        gint new_top_row;

        new_top_row = (gint) adj->value;

        if (new_top_row == sheet->top_block)
                return;

        gnucash_sheet_set_top_row (sheet, new_top_row, GNUCASH_ALIGN_SAME);
}


gint
gnucash_sheet_row_get_distance (GnucashSheet *sheet, int row_a, int row_b)
{
        SheetBlockStyle *style;
        int a, b;
        int distance = 0;
        int sign = 1;

        g_return_val_if_fail (sheet != NULL, 0);
        g_return_val_if_fail (GNUCASH_IS_SHEET (sheet), 0);

        if (row_a < row_b) {
                a = row_a;
                b = row_b;
        }
        else {
                a = row_b;
                b = row_a;
                sign = -1;
        }

        a = MAX (a, 1);
        b = MIN (b, sheet->num_virt_rows);

        for ( ; a < b; a++) {
                style = gnucash_sheet_get_style(sheet, a, 0);
                distance += style->height;
        }
        
        return sign * distance;
}

gint
gnucash_sheet_col_get_distance (GnucashSheet *sheet, int col_a, int col_b)
{
        SheetBlockStyle *style;
        int a, b;
        int distance = 0;
        int sign = 1;

        g_return_val_if_fail (sheet != NULL, 0);
        g_return_val_if_fail (GNUCASH_IS_SHEET (sheet), 0);

        if (col_a < col_b) {
                a = col_a;
                b = col_b;
        }
        else {
                a = col_b;
                b = col_a;
                sign = -1;
        }

        a = MAX (a, 0);
        b = MIN (b, sheet->num_virt_cols);

        if (b <= a)
                return 0;
        
        for ( ; a < b; a++) {
                style = gnucash_sheet_get_style(sheet, 0, a);
                if (!style)
                        return distance;
                distance += style->width;
        }


        return sign * distance;
}


void
gnucash_sheet_redraw_all (GnucashSheet *sheet)
{
        g_return_if_fail (sheet != NULL);
        g_return_if_fail (GNUCASH_IS_SHEET(sheet));

        gnome_canvas_request_redraw (
                GNOME_CANVAS (sheet), 0, 0, INT_MAX, INT_MAX);
}


void
gnucash_sheet_redraw_block (GnucashSheet *sheet, gint row, gint col)
{
        gint x, y, w, h;
        GnomeCanvas *canvas;
        SheetBlockStyle *style;

        g_return_if_fail (sheet != NULL);
        g_return_if_fail (GNUCASH_IS_SHEET(sheet));

        canvas = GNOME_CANVAS(sheet);

        if (row < sheet->top_block || row > sheet->bottom_block)
                return;

        style = gnucash_sheet_get_style (sheet, row, col);

        if (style) {
                y = gnucash_sheet_row_get_distance (sheet, sheet->top_block,
						    row);
                /* FIXME:  get_col_distance */
                x = 0;
                
                x += canvas->layout.xoffset - canvas->zoom_xofs;
                y += canvas->layout.yoffset - canvas->zoom_yofs;
                
                h = style->height;
                w = MIN(style->width, GTK_WIDGET(sheet)->allocation.width);

                gnome_canvas_request_redraw (canvas, x, y, x+w, y+h);
        }
}


static void
gnucash_sheet_destroy (GtkObject *object)
{
        GnucashSheet *sheet;
        gint i, j;

        sheet = GNUCASH_SHEET (object);

        for (i = 0; i < sheet->num_virt_rows; i++)
                for (j = 0; j < sheet->num_virt_cols; j++)
                        gnucash_sheet_block_destroy(sheet, i, j);

        for (i = GNUCASH_CURSOR_HEADER; i < GNUCASH_CURSOR_LAST; i++)
                gnucash_sheet_style_destroy(sheet->cursor_style[i]);

        if (GTK_OBJECT_CLASS (sheet_parent_class)->destroy)
                (*GTK_OBJECT_CLASS (sheet_parent_class)->destroy)(object);

	/* This has to come after the parent destroy, so the item edit
	   destruction can do its disconnects. */
        gtk_widget_unref (sheet->entry);
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

        sheet = gtk_type_new (gnucash_sheet_get_type ());
        canvas = GNOME_CANVAS (sheet);

        sheet->table = table;
        sheet->entry = NULL;
        sheet->split_register = NULL;

        sheet->vadj = GTK_ADJUSTMENT(gtk_adjustment_new (0.0, 0.0, 0.0,
							 1.0, 1.0, 1.0));
        sheet->hadj = gtk_layout_get_hadjustment (GTK_LAYOUT(canvas));

        gtk_signal_connect(GTK_OBJECT(sheet->vadj), "value_changed",
			   GTK_SIGNAL_FUNC(gnucash_sheet_vadjustment_value_changed), sheet);

        return sheet;
}

/*
 *  Hmmm, maybe first make a style based on the header cell block, then
 *  compute width/height of the register so it exactly fits width-wise, and
 *  maybe just default the height?
 */
static gint
compute_optimal_width (Table *table)
{
        return DEFAULT_REGISTER_WIDTH;
}


static gint
compute_optimal_height (Table *table)
{
        return DEFAULT_REGISTER_HEIGHT;
}


static void
gnucash_sheet_size_request (GtkWidget *widget, GtkRequisition *requisition)
{
        GnucashSheet *sheet = GNUCASH_SHEET(widget);

        requisition->width = compute_optimal_width (sheet->table);
        requisition->height = compute_optimal_height (sheet->table);
}


const char *
gnucash_sheet_modify_current_cell(GnucashSheet *sheet, const gchar *new_text)
{
        Table *table = sheet->table;
        int v_row, v_col, c_row, c_col;
        int p_row, p_col;

        const char *old_text;
        const char *retval;
        char *newval;
	char *change;

        gnucash_cursor_get_phys(GNUCASH_CURSOR(sheet->cursor), &p_row, &p_col);

        gnucash_cursor_get_virt(GNUCASH_CURSOR(sheet->cursor),
				&v_row, &v_col, &c_row, &c_col);

        if (!gnc_register_cell_valid (table, p_row, p_col))
                return NULL;

        old_text = gtk_entry_get_text (GTK_ENTRY(sheet->entry));
        if (old_text == NULL)
                old_text = "";

	newval = strdup(new_text);
	change = strdup(new_text);
	assert((newval != NULL) && (change != NULL));

        retval = gnc_table_modify_update (table, p_row, p_col,
					  old_text, change, newval);

        gnucash_sheet_cell_set_from_table (sheet, v_row, v_col, c_row, c_col);

        if (retval != NULL) {
                gtk_signal_handler_block (GTK_OBJECT (sheet->entry),
					  sheet->insert_signal);

                gtk_entry_set_text (GTK_ENTRY (sheet->entry), retval);

                gtk_signal_handler_unblock (GTK_OBJECT (sheet->entry),
					    sheet->insert_signal);

		if (retval != newval)
			free (newval);
        }

	if (retval == NULL)
		free (newval);

	free(change);

	return retval;
}


static void
gnucash_sheet_insert_cb (GtkWidget *widget, const gchar *new_text,
                         const gint new_text_length,
                         gint *position,
                         GnucashSheet *sheet)
{
        Table *table = sheet->table;
        int p_row, p_col;
        int v_row, v_col, c_row, c_col;

        const char *old_text;
        char *newval = NULL;
        char *change = NULL;
        const char *retval;

        if (!new_text_length)
                return;
        
        gnucash_cursor_get_phys (GNUCASH_CURSOR(sheet->cursor),
				 &p_row, &p_col);
        gnucash_cursor_get_virt (GNUCASH_CURSOR(sheet->cursor), &v_row,
				 &v_col, &c_row, &c_col);
        
        if (!gnc_register_cell_valid (table, p_row, p_col))
                return;

        old_text = gtk_entry_get_text (GTK_ENTRY(sheet->entry));
        if (old_text == NULL)
                old_text = "";

        /* we set newval to what the entry contents would be if
           the insert was processed */
        newval = calloc (strlen(old_text) + new_text_length + 1, sizeof(char));
	assert (newval != NULL);

        strncat (newval, old_text, *position);
        strncat (newval, new_text, new_text_length);
        strcat (newval, &old_text[*position + new_text_length - 1]);

        change = g_new0 (char, new_text_length  + 1);
        strncpy (change, new_text, new_text_length);

        retval = gnc_table_modify_update (table, p_row, p_col, old_text,
					  change, newval);

        gnucash_sheet_cell_set_from_table (sheet, v_row, v_col, c_row, c_col);

        if (retval && (retval != newval )) {
                /* this means that the edit was allowed, but now the
                   cell contents differ from what the entry contents
                   would be after the insert is processed.  So we synchronize
                   the entry contents, and stop the insert signal from
                   being processed further */
                
                gtk_signal_handler_block(GTK_OBJECT (sheet->entry),
					 sheet->insert_signal);
                gtk_entry_set_text (GTK_ENTRY (sheet->entry), retval);
                gtk_editable_set_position (GTK_EDITABLE(sheet->entry),
					   *position + new_text_length + 1);
                gtk_signal_handler_unblock ( GTK_OBJECT (sheet->entry),
					     sheet->insert_signal);

                gtk_signal_emit_stop_by_name (GTK_OBJECT(sheet->entry),
					      "insert_text");
                free (newval);
        }
        else if (!retval) {
                /* the entry was disallowed, so we stop the insert signal */
                gtk_signal_emit_stop_by_name (GTK_OBJECT(sheet->entry),
					      "insert_text");
		free (newval);
        }

        g_free (change);
}


static void
gnucash_sheet_delete_cb (GtkWidget *widget,
                         const gint start_pos,
                         const gint end_pos,
                         GnucashSheet *sheet)
{
        Table *table = sheet->table;

        int v_row, v_col, c_row, c_col;
        int p_row, p_col;

        const char *old_text;
        char *newval = NULL;
        const char *retval = NULL;

        gnucash_cursor_get_phys (GNUCASH_CURSOR(sheet->cursor),
				 &p_row, &p_col);
        gnucash_cursor_get_virt (GNUCASH_CURSOR (sheet->cursor),
				 &v_row, &v_col, &c_row, &c_col);

        if (!gnc_register_cell_valid (table, p_row, p_col))
                return;
        
        old_text = gtk_entry_get_text (GTK_ENTRY(sheet->entry));

        if (old_text == NULL)
                old_text = "";

        newval = calloc (strlen(old_text) - (end_pos - start_pos) + 1,
                         sizeof(char));
	assert (newval != NULL);

        strncat (newval, old_text, start_pos);
        strcat (newval, &old_text[end_pos]);

        retval = gnc_table_modify_update (table, p_row, p_col, old_text,
					  NULL, newval);

        gnucash_sheet_cell_set_from_table (sheet, v_row, v_col, c_row, c_col);
        
        if (retval && (retval != newval )) {
                /* this means that the edit was allowed, but now the
                   cell contents differ from what the entry contents
                   would be after the delete is processed.  So we synchronize
                   the entry contents, and stop the delete signal from
                   being processed further */
                
                gtk_signal_handler_block ( GTK_OBJECT (sheet->entry),
					   sheet->insert_signal);
                gtk_entry_set_text (GTK_ENTRY (sheet->entry), retval);
                gtk_signal_handler_unblock ( GTK_OBJECT (sheet->entry),
					     sheet->insert_signal);

                gtk_signal_emit_stop_by_name (GTK_OBJECT(sheet->entry),
					      "delete_text");
                free (newval);
        }
        else if (!retval) {
                /* the entry was disallowed, so we stop the delete signal */
                gtk_signal_emit_stop_by_name (GTK_OBJECT(sheet->entry),
					      "delete_text");
		free (newval);
        }
}


static void
gnucash_sheet_size_allocate (GtkWidget *widget, GtkAllocation *allocation)
{
        GnucashSheet *sheet = GNUCASH_SHEET(widget);
        gint i;
        
        if (GTK_WIDGET_CLASS(sheet_parent_class)->size_allocate)
                (*GTK_WIDGET_CLASS (sheet_parent_class)->size_allocate)
			(widget, allocation);

        for (i = GNUCASH_CURSOR_HEADER; i < GNUCASH_CURSOR_LAST; i++)
                gnucash_sheet_style_set_dimensions(sheet,
						   sheet->cursor_style[i]);

        gnucash_cursor_configure (GNUCASH_CURSOR (sheet->cursor));
        gnucash_header_reconfigure (GNUCASH_HEADER(sheet->header_item));
        gnucash_sheet_update_adjustments (sheet);
}


static void
gnucash_sheet_start_editing_at_cursor (GnucashSheet *sheet, gboolean blankp,
				       gboolean cursorp)
{
        GtkEditable *editable;
        GnomeCanvas *canvas;
        const char *text;
        int virt_row, virt_col, cell_row, cell_col;
        int cursor_pos = -1;

        g_return_if_fail (sheet != NULL);
        g_return_if_fail (GNUCASH_IS_SHEET (sheet));

        canvas = GNOME_CANVAS(sheet);

        gnucash_cursor_get_virt (GNUCASH_CURSOR(sheet->cursor),
                                 &virt_row, &virt_col, &cell_row, &cell_col);

        text = gnucash_sheet_block_get_text (sheet, virt_row, virt_col,
					     cell_row, cell_col);

        item_edit_configure (ITEM_EDIT(sheet->item_editor));
        gnome_canvas_item_show (GNOME_CANVAS_ITEM (sheet->item_editor));

        editable = GTK_EDITABLE(sheet->entry);

        if (cursorp)
                cursor_pos = gtk_editable_get_position(editable);

        if (blankp)
                gtk_entry_set_text (GTK_ENTRY(sheet->entry), "");
        else
                gtk_entry_set_text (GTK_ENTRY(sheet->entry), text);

        if (cursorp)
                cursor_pos = MIN(cursor_pos, strlen(text));

        gtk_editable_set_position(editable, cursor_pos);

        sheet->editing = TRUE;

        /* set up the signals */
        sheet->insert_signal =
		gtk_signal_connect(GTK_OBJECT(sheet->entry), "insert_text",
				   GTK_SIGNAL_FUNC(gnucash_sheet_insert_cb),
						   sheet);

        sheet->delete_signal =
		gtk_signal_connect(GTK_OBJECT(sheet->entry), "delete_text",
				   GTK_SIGNAL_FUNC(gnucash_sheet_delete_cb),
				   sheet);
}

static gint
gnucash_button_press_event (GtkWidget *widget, GdkEventButton *event)
{
        GnucashSheet *sheet;
        gboolean changed_cells;
        int xoffset, yoffset;
        int x, y;

        /* physical coordinates */
        int current_p_row, current_p_col, new_p_row, new_p_col;

        /* virtual coordinates */
        int current_v_row, current_v_col, new_v_row, new_v_col;

        /* cell coordinates */
        int current_c_row, current_c_col, new_c_row, new_c_col;

        Table *table;
        gncBoolean exit_register;

        g_return_val_if_fail(widget != NULL, TRUE);
        g_return_val_if_fail(GNUCASH_IS_SHEET(widget), TRUE);
        g_return_val_if_fail(event != NULL, TRUE);

        if (event->type != GDK_BUTTON_PRESS || event->button != 1)
                return TRUE;

        sheet = GNUCASH_SHEET (widget);
        table = sheet->table;

        gnome_canvas_get_scroll_offsets (GNOME_CANVAS(sheet),
					 &xoffset, &yoffset);

        gnucash_cursor_get_phys (GNUCASH_CURSOR(sheet->cursor),
				 &current_p_row, &current_p_col);

        gnucash_cursor_get_virt (GNUCASH_CURSOR(sheet->cursor),
                                 &current_v_row, &current_v_col,
				 &current_c_row, &current_c_col);

        x = xoffset + event->x;
        y = yoffset + event->y;

	if (!gnucash_grid_find_cell_origin_by_pixel(GNUCASH_GRID(sheet->grid),
						    x, y,
						    &new_v_row, &new_v_col,
						    &new_c_row, &new_c_col,
						    NULL, NULL))
		return TRUE;

	new_p_row = table->rev_locators[new_v_row][new_v_col]->phys_row +
		new_c_row;

	new_p_col = table->rev_locators[new_v_row][new_v_col]->phys_col +
		new_c_col;

        /* and finally...process this as a POINTER_TRAVERSE */
        exit_register = gnc_table_traverse_update (table,
                                                   current_p_row,
						   current_p_col,
                                                   GNC_TABLE_TRAVERSE_POINTER,
                                                   &new_p_row, &new_p_col);

        if (exit_register)
		return TRUE;

        /* Shouldn't gnc_table_traverse_update fill in valid cells? */
        if (!gnc_register_cell_valid (table, new_p_row, new_p_col))
                return TRUE;

        new_v_row = table->locators[new_p_row][new_p_col]->virt_row;
        new_v_col = table->locators[new_p_row][new_p_col]->virt_col;

        new_c_row = table->locators[new_p_row][new_p_col]->phys_row_offset;
        new_c_col = table->locators[new_p_row][new_p_col]->phys_col_offset;

        changed_cells =
                (new_v_row != current_v_row) ||
                (new_v_col != current_v_col) ||
                (new_c_row != current_c_row) ||
                (new_c_col != current_c_col);

        gnucash_sheet_cursor_move (sheet,
                                   new_v_row, new_v_col,
                                   new_c_row, new_c_col,
                                   changed_cells);

        item_edit_set_cursor_pos (ITEM_EDIT(sheet->item_editor),
                                  x, y, changed_cells);
        return TRUE;
}


static gint
gnucash_sheet_key_press_event (GtkWidget *widget, GdkEventKey *event)
{
        GnucashSheet *sheet;
	CellBlock *header;
	int direction = 0;
	gboolean pass_on = FALSE;
        gboolean changed_cells;

        /* physical coordinates */
        int current_p_row, current_p_col, new_p_row, new_p_col;

        /* virtual coordinates */
        int current_v_row, current_v_col, new_v_row, new_v_col;

        /* cell coordinates */
        int current_c_row, current_c_col, new_c_row, new_c_col;
        
        Table *table;
        gncBoolean exit_register;

        g_return_val_if_fail(widget != NULL, TRUE);
        g_return_val_if_fail(GNUCASH_IS_SHEET(widget), TRUE);
        g_return_val_if_fail(event != NULL, TRUE);
                
        sheet = GNUCASH_SHEET (widget);
        table = sheet->table;
	header = table->handlers[0][0];

        gnucash_cursor_get_phys (GNUCASH_CURSOR(sheet->cursor),
				 &current_p_row, &current_p_col);

        gnucash_cursor_get_virt (GNUCASH_CURSOR(sheet->cursor),
                                 &current_v_row, &current_v_col,
				 &current_c_row, &current_c_col);

	/* Calculate tentative physical values */
        switch (event->keyval) {
		case GDK_Tab:
			direction = GNC_TABLE_TRAVERSE_RIGHT;
			new_p_row = current_p_row;
			new_p_col = MIN(current_p_col + 1,
					table->num_phys_cols - 1);
			break;
		case GDK_Page_Up:
			direction = GNC_TABLE_TRAVERSE_UP;
			new_p_col = 0;
			new_p_row = MAX(current_p_row -
					sheet->vadj->page_increment,
					header->numRows);
			break;
		case GDK_Page_Down:
			direction = GNC_TABLE_TRAVERSE_DOWN;
			new_p_col = 0;
			new_p_row = MIN(current_p_row +
					sheet->vadj->page_increment,
					table->num_phys_rows - 1);
			break;
		case GDK_Up:
			direction = GNC_TABLE_TRAVERSE_UP;
			new_p_col = current_p_col;
			new_p_row = MAX(current_p_row - 1,
					header->numRows);
			break;
		case GDK_Down:
			direction = GNC_TABLE_TRAVERSE_DOWN;
			new_p_col = current_p_col;
			new_p_row = MIN(current_p_row + 1,
					table->num_phys_rows - 1);
			break;
		case GDK_Left:
			/* Hmm, what to do here? */
			/* FALL DOWN */
		case GDK_Right:
			/* Hmm, what to do here? */
			/* FALL DOWN */
		default:
			pass_on = TRUE;
			break;
        }

	/* Forward the keystroke to the input line */
	if (pass_on)
		return gtk_widget_event(sheet->entry, (GdkEvent *) event);

	exit_register = gnc_table_traverse_update
		(table, current_p_row, current_p_col,
		 direction, &new_p_row, &new_p_col);

	/* If that would leave the register and we're not going right, abort */
	if (exit_register && (direction != GNC_TABLE_TRAVERSE_RIGHT))
		return TRUE;

	/* Hack: try to wrap around. Shouldn't splitreg.c do this? */
	if (exit_register) {
		new_p_row = table->rev_locators[current_v_row]
			                       [current_v_col]->phys_row;
		new_p_col = 0;
		exit_register = gnc_table_traverse_update
			(table, current_p_row, current_p_col,
			 GNC_TABLE_TRAVERSE_POINTER, &new_p_row, &new_p_col);
	}

	/* Give up */
	if (exit_register)
		return TRUE;

	/* Shouldn't gnc_table_traverse_update fill in valid cells ? */
	if (!gnc_register_cell_valid (table, new_p_row, new_p_col))
		return TRUE;

	new_v_row = table->locators[new_p_row][new_p_col]->virt_row;
	new_v_col = table->locators[new_p_row][new_p_col]->virt_col;

	new_c_row = table->locators[new_p_row][new_p_col]->phys_row_offset;
	new_c_col = table->locators[new_p_row][new_p_col]->phys_col_offset;

        changed_cells =
                (new_v_row != current_v_row) ||
                (new_v_col != current_v_col) ||
                (new_c_row != current_c_row) ||
                (new_c_col != current_c_col);

	gnucash_sheet_cursor_move(sheet, new_v_row, new_v_col,
				  new_c_row, new_c_col, changed_cells);

        /* return true because we handled the key press */
        return TRUE;
}


SheetBlock *
gnucash_sheet_get_block (GnucashSheet *sheet, gint virt_row, gint virt_col)
{
        SheetBlock block;
        
        g_return_val_if_fail (sheet != NULL, NULL);
        g_return_val_if_fail (GNUCASH_IS_SHEET(sheet), NULL);

        block.virt_row = virt_row;
        block.virt_col = virt_col;

        return g_hash_table_lookup (sheet->blocks, &block);
}


static const char *
gnucash_sheet_block_get_text (GnucashSheet *sheet, gint virt_row,
			      gint virt_col, gint cell_row, gint cell_col)
{
        SheetBlock *block;

        g_return_val_if_fail (sheet != NULL, NULL);
        g_return_val_if_fail (GNUCASH_IS_SHEET (sheet), NULL);

        block = gnucash_sheet_get_block (sheet, virt_row, virt_col);

        if (cell_row >= 0 && cell_row <= block->style->nrows
            && cell_col >= 0 && cell_col <= block->style->ncols) {

                return block->entries [cell_row][cell_col];
        }

        return NULL;
}


void
gnucash_sheet_block_clear_entries (SheetBlock *block)
{
        gint i,j;
        gint num_rows, num_cols;

        if (block && block->style) {
                num_rows = block->style->nrows;
                num_cols = block->style->ncols;

                for (i = 0; i < num_rows; i++ ) {
                        for (j = 0; j < num_cols; j++) 
                                g_free (block->entries[i][j]);
                        g_free (block->entries[i]);
                        g_free (block->fg_colors[i]);
                        g_free (block->bg_colors[i]);
                }
        }
}


static void
gnucash_sheet_block_set_entries (GnucashSheet *sheet, gint virt_row,
				 gint virt_col)
{
        gint i,j;
        SheetBlock *block;
        Table *table;
        gint phys_row_origin, phys_col_origin;
        

        block = gnucash_sheet_get_block (sheet, virt_row, virt_col);

        if (block) {
                table = sheet->table;

                phys_row_origin =
			table->rev_locators[virt_row][virt_col]->phys_row;
                phys_col_origin =
			table->rev_locators[virt_row][virt_col]->phys_col;

                for (i = 0; i < block->style->nrows; i++) {
                        for (j = 0; j < block->style->ncols; j++) {
                                if (table->entries [phys_row_origin + i]
				                   [phys_col_origin + j])
                                        block->entries[i][j] =
                                                g_strdup(table->entries [phys_row_origin + i][phys_col_origin + j]);

                                block->fg_colors[i][j] =
                                        gnucash_color_argb_to_gdk(table->fg_colors [phys_row_origin + i][phys_col_origin + j]);

                                block->bg_colors[i][j] =
                                        gnucash_color_argb_to_gdk (table->bg_colors [phys_row_origin + i][phys_col_origin + j]);
                        }
                }
        }
}


/* This fills up a block from the table; it sets the style, sizes the
   entries matrix, and sets the entries. */
void
gnucash_sheet_block_set_from_table (GnucashSheet *sheet, gint virt_row,
				    gint virt_col)
{
        Table *table;
        SheetBlock *block;
        gint i;

        block = gnucash_sheet_get_block (sheet, virt_row, virt_col);

        if (block) {
                
                table = sheet->table;

                if (block->entries) {
                        gnucash_sheet_block_clear_entries (block);
                        g_free (block->entries);
                }

                if (block->style)
                        gnucash_style_unref (block->style);

                block->style = gnucash_sheet_get_style_from_table
			(sheet, virt_row, virt_col);

                gnucash_style_ref(block->style);

                block->entries = g_new0( gchar **, block->style->nrows);
                for (i = 0; i < block->style->nrows; i++)
                        block->entries[i] = g_new0(gchar *,
						   block->style->ncols);

                block->fg_colors = g_new0( GdkColor **, block->style->nrows);
                for (i = 0; i < block->style->nrows; i++)
                        block->fg_colors[i] = g_new0(GdkColor *,
						     block->style->ncols);

                block->bg_colors = g_new0( GdkColor **, block->style->nrows);
                for (i = 0; i < block->style->nrows; i++)
                        block->bg_colors[i] = g_new0(GdkColor *,
						     block->style->ncols);

                gnucash_sheet_block_set_entries (sheet, virt_row, virt_col);
        }
}


static void
gnucash_sheet_cell_set_from_table (GnucashSheet *sheet, gint virt_row,
				   gint virt_col, gint cell_row, gint cell_col)
{
        SheetBlock *block;
        Table *table;
        gint p_row, p_col;
        gchar  *text;
        
        g_return_if_fail (sheet != NULL);
        g_return_if_fail (GNUCASH_IS_SHEET(sheet));

        table = sheet->table;

        block = gnucash_sheet_get_block (sheet, virt_row, virt_col);
        
        if (cell_row >= 0 && cell_row <= block->style->nrows
            && cell_col >= 0 && cell_col <= block->style->ncols) {

                if (block->entries[cell_row][cell_col])
                        g_free (block->entries[cell_row][cell_col]);

                p_row = table->rev_locators[virt_row][virt_col]->phys_row +
			cell_row;
                p_col = table->rev_locators[virt_row][virt_col]->phys_col +
			cell_col;

                text = table->entries[p_row][p_col];

                block->entries[cell_row][cell_col] = g_strdup(text);
        }
}


static void
gnucash_sheet_block_destroy (GnucashSheet *sheet, gint virt_row, gint virt_col)
{
        SheetBlock *block;

        block = gnucash_sheet_get_block (sheet, virt_row, virt_col);

        if (block) {
                gnucash_sheet_block_clear_entries (block);
                g_free (block->entries);
                g_free (block->fg_colors);
                g_free (block->bg_colors);

                if (block->style)
                        gnucash_style_unref (block->style);

                g_hash_table_remove (sheet->blocks, block);

                g_free (block);
        }
}


static void
gnucash_sheet_block_new (GnucashSheet *sheet, gint virt_row, gint virt_col)
{
        SheetBlock *block;

        block = gnucash_sheet_get_block (sheet, virt_row, virt_col);

        if (!block) {

                block = g_new0 (SheetBlock, 1);

                block->virt_row = virt_row;
                block->virt_col = virt_col;

                g_hash_table_insert (sheet->blocks, block, block);
        }

        return;
}


void
gnucash_sheet_resize (GnucashSheet *sheet)
{
        gint i, j;
        gint diff_rows, diff_cols;
        
        g_return_if_fail (sheet != NULL);
        g_return_if_fail (GNUCASH_IS_SHEET(sheet));

        diff_rows = sheet->table->num_virt_rows - sheet->num_virt_rows;
        diff_cols = sheet->table->num_virt_cols - sheet->num_virt_cols;

        if (diff_rows < 0) {
                /* we need to shrink the number of rows */
                for (i = 0; i < -diff_rows; i++)
                        for (j = 0; j < sheet->num_virt_cols; j++)
                                gnucash_sheet_block_destroy
					(sheet, sheet->num_virt_rows-i-1, j);
        }
        else if (diff_rows > 0) {
                /* we need some more rows */
                for (i = 0; i < diff_rows; i++)
                        for ( j = 0; j < sheet->num_virt_cols + diff_cols; j++)
                                gnucash_sheet_block_new
					(sheet, sheet->num_virt_rows + i, j);
        }

        sheet->num_virt_rows = sheet->table->num_virt_rows;

        if (diff_cols < 0) {
                /* we need to shrink the number of cols */
                for (j = 0; j < -diff_cols; j++)
                        for (i = 0; i < sheet->num_virt_rows; i++)
                                gnucash_sheet_block_destroy
					(sheet, i, sheet->num_virt_cols-j-1);
        }
        else if (diff_cols > 0) {
                /* we need some more cols */
                for (j = 0; j < diff_cols; j++)
                        for ( i = 0; i < sheet->num_virt_rows; i++)
                                gnucash_sheet_block_new
					(sheet, i, sheet->num_virt_cols + j);
        }

        sheet->num_virt_cols = sheet->table->num_virt_cols;
}

        
void
gnucash_sheet_table_load (GnucashSheet *sheet)
{
        Table *table;
        gint num_virt_rows;
        gint i, j;
                
        g_return_if_fail (sheet != NULL);
        g_return_if_fail (GNUCASH_IS_SHEET(sheet));
        g_return_if_fail (sheet->table != NULL);

        table = sheet->table;
        num_virt_rows = table->num_virt_rows;

        gnucash_sheet_stop_editing (sheet);

        /* resize the sheet */
        gnucash_sheet_resize (sheet);

        /* fill it up */
        for ( i = 0; i < table->num_virt_rows; i++)
                for (j = 0; j < table->num_virt_cols; j++)
                        gnucash_sheet_block_set_from_table (sheet, i, j);


        /* FIXME:  try to keep the cursor row in the same visual position */
        /* FIXME:  Probably we should set the bottom row instead */
        gnucash_sheet_set_top_row (sheet, 0, GNUCASH_ALIGN_TOP);

        gnucash_sheet_compute_visible_range(sheet);

        gnucash_sheet_cursor_set_from_table (sheet);
        gnucash_sheet_activate_cursor_cell (sheet, TRUE);
}


static void
gnucash_sheet_class_init (GnucashSheetClass *class)
{
        GtkObjectClass *object_class;
        GtkWidgetClass *widget_class;
        GnomeCanvasClass *canvas_class;

        object_class = (GtkObjectClass *) class;
        widget_class = (GtkWidgetClass *) class;
        canvas_class = (GnomeCanvasClass *) class;

        sheet_parent_class = gtk_type_class (gnome_canvas_get_type ());

        /* Method override */
        object_class->destroy = gnucash_sheet_destroy;

        widget_class->realize = gnucash_sheet_realize;
        widget_class->size_request = gnucash_sheet_size_request;
        widget_class->size_allocate = gnucash_sheet_size_allocate;
        widget_class->key_press_event = gnucash_sheet_key_press_event;
        widget_class->button_press_event = gnucash_button_press_event;
}


static guint
block_hash (gconstpointer key)
{
        const SheetBlock *block = (SheetBlock *)key;

        return (block->virt_row << 8) | block->virt_col;
}


static gint
block_compare (gconstpointer a, gconstpointer b)
{
        const SheetBlock *block_a, *block_b;

        block_a = (const SheetBlock *)a;
        block_b = (const SheetBlock *)b;

        if (block_a->virt_row != block_b->virt_row)
                return 0;
        if (block_a->virt_col != block_b->virt_col)
                return 0;

        return 1;
}


static void
gnucash_sheet_init (GnucashSheet *sheet)
{
        GnomeCanvas *canvas = GNOME_CANVAS (sheet);

        GTK_WIDGET_SET_FLAGS (canvas, GTK_CAN_FOCUS);
        GTK_WIDGET_SET_FLAGS (canvas, GTK_CAN_DEFAULT);

        sheet->top_block = 1;
        sheet->bottom_block = 1;
        sheet->alignment = GNUCASH_ALIGN_TOP;
        
        sheet->num_virt_rows = 0;
        sheet->num_virt_cols = 0;
        sheet->item_editor = NULL;
        sheet->entry = NULL;
        sheet->editing  = FALSE;

        sheet->blocks = g_hash_table_new(block_hash, block_compare);
}


GtkType
gnucash_sheet_get_type (void)
{
        static GtkType gnucash_sheet_type = 0;

        if (!gnucash_sheet_type){
                GtkTypeInfo gnucash_sheet_info = {
                        "GnucashSheet",
                        sizeof (GnucashSheet),
                        sizeof (GnucashSheetClass),
                        (GtkClassInitFunc) gnucash_sheet_class_init,
                        (GtkObjectInitFunc) gnucash_sheet_init,
                        NULL, /* reserved 1 */
                        NULL, /* reserved 2 */
                        (GtkClassInitFunc) NULL
                };

                gnucash_sheet_type =
			gtk_type_unique (gnome_canvas_get_type (),
					 &gnucash_sheet_info);
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
        GtkWidget *widget;

        g_return_val_if_fail (table != NULL, NULL);
  
        sheet = gnucash_sheet_create (table);

        /* FIXME: */
        gnome_canvas_set_scroll_region(GNOME_CANVAS (sheet),
				       0, 0, 100000, 100000);

        /* handy shortcuts */
        sheet_canvas = GNOME_CANVAS (sheet);
        sheet_group = gnome_canvas_root (GNOME_CANVAS(sheet));

        /* The grid */
        item = gnome_canvas_item_new (sheet_group,
                                      gnucash_grid_get_type (),
                                      "GnucashGrid::Sheet", sheet,
                                      NULL);
        sheet->grid = item;

        /* The cursor */
        sheet->cursor = gnucash_cursor_new (sheet_group);
        gnome_canvas_item_set (sheet->cursor,
                               "GnucashCursor::sheet", sheet,
                               "GnucashCursor::grid", sheet->grid,
                               NULL);

        /* The entry widgets */
        sheet->entry = gtk_entry_new ();
        gtk_widget_ref(sheet->entry);
	gtk_object_sink(GTK_OBJECT(sheet->entry));

        /* set up the editor */
        sheet->item_editor = item_edit_new(sheet_group, sheet, sheet->entry);

	gnome_canvas_item_hide (GNOME_CANVAS_ITEM(sheet->item_editor));

        widget = GTK_WIDGET (sheet);
        return widget;
}


static void
gnucash_register_class_init (GnucashRegisterClass *class)
{
        GtkObjectClass *object_class;
        GtkWidgetClass *widget_class;
        GtkTableClass *table_class;

        object_class = (GtkObjectClass *) class;
        widget_class = (GtkWidgetClass *) class;
        table_class = (GtkTableClass *) class;

        register_parent_class = gtk_type_class (gtk_table_get_type ());
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


GtkType
gnucash_register_get_type (void)
{
        static GtkType gnucash_register_type = 0;

        if (!gnucash_register_type){
                GtkTypeInfo gnucash_register_info = {
                        "GnucashRegister",
                        sizeof (GnucashRegister),
                        sizeof (GnucashRegisterClass),
                        (GtkClassInitFunc) gnucash_register_class_init,
                        (GtkObjectInitFunc) gnucash_register_init,
                        NULL, /* reserved 1 */
                        NULL, /* reserved 2 */
                        (GtkClassInitFunc) NULL
                };

                gnucash_register_type = gtk_type_unique
			(gtk_table_get_type (),
			 &gnucash_register_info);
        }

        return gnucash_register_type;
}


GtkWidget *
gnucash_register_new (Table *table)
{
        GnucashRegister *reg;
        GtkWidget *header_canvas;
        GtkWidget *widget;
        GtkWidget *sheet;
        GtkWidget *scrollbar;

        reg = gtk_type_new(gnucash_register_get_type ());
        widget = GTK_WIDGET(reg);

        sheet = gnucash_sheet_new (table);
        reg->sheet = sheet;
        GNUCASH_SHEET(sheet)->reg = widget;

        header_canvas = gnucash_header_new (GNUCASH_SHEET(sheet));
        reg->header_canvas = header_canvas;

        gtk_table_attach (GTK_TABLE(widget), header_canvas,
                          0, 1, 0, 1,
                          GTK_FILL | GTK_EXPAND | GTK_SHRINK,
                          GTK_FILL,
                          0, 0);
        
        gtk_table_attach (GTK_TABLE(widget), sheet,
                          0, 1, 1, 2,
                          GTK_FILL | GTK_EXPAND | GTK_SHRINK,
                          GTK_FILL | GTK_EXPAND | GTK_SHRINK,
                          0, 0);

        scrollbar = gtk_vscrollbar_new(GNUCASH_SHEET(sheet)->vadj);
        gtk_table_attach (GTK_TABLE(widget), GTK_WIDGET(scrollbar),
                          1, 2, 0, 2,
                          GTK_FILL,
                          GTK_EXPAND | GTK_SHRINK | GTK_FILL,
                          0, 0);
        reg->vscrollbar = scrollbar;
        gtk_widget_show(scrollbar);
        
        scrollbar = gtk_hscrollbar_new(NULL);
        gtk_table_attach (GTK_TABLE(widget), GTK_WIDGET(scrollbar),
                          0, 1, 2, 3,
                          GTK_EXPAND | GTK_SHRINK | GTK_FILL,
                          GTK_FILL,
                          0, 0);
        reg->hscrollbar = scrollbar;
        gtk_widget_show(scrollbar);

        
        return widget;
}


/*
  Local Variables:
  c-basic-offset: 8
  End:
*/
