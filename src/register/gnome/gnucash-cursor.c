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
 * The Gnucash Cursor Canvas Item
 *
 *  Based heavily (i.e., cut and pasted from) on the Gnumeric ItemCursor.
 *
 * Author:
 *     Heath Martin <martinh@pegasus.cc.ucf.edu>
 */

#include "gnucash-sheet.h"
#include "gnucash-grid.h"
#include "gnucash-color.h"
#include "gnucash-cursor.h"
#include "gnucash-style.h"

static GnomeCanvasItem *gnucash_cursor_parent_class;
static GnomeCanvasItem *gnucash_item_cursor_parent_class;

enum {
        ARG_0,
        ARG_SHEET,
        ARG_GRID,
};


static void
gnucash_cursor_get_pixel_coords (GnucashCursor *cursor, gint *x, gint *y,
				 gint *w, gint *h)
{
        GnucashSheet *sheet = cursor->sheet;
        GnucashItemCursor *item_cursor =
		GNUCASH_ITEM_CURSOR(cursor->cursor[GNUCASH_CURSOR_BLOCK]);

        gnome_canvas_get_scroll_offsets (GNOME_CANVAS(cursor->sheet), NULL, y);

        *y += gnucash_sheet_row_get_distance (sheet, sheet->top_block,
					      item_cursor->row)
                + sheet->top_block_offset;
        *x = gnucash_sheet_col_get_distance (sheet, sheet->left_block,
					      item_cursor->col)
                + sheet->left_block_offset;

        *h = gnucash_sheet_row_get_distance (sheet, item_cursor->row,
					     item_cursor->row + 1);
        *w = gnucash_sheet_col_get_distance (sheet, item_cursor->col,
					     item_cursor->col + 1);
}


static void
gnucash_cursor_request_redraw (GnucashCursor *cursor)
{
        GnomeCanvas *canvas = GNOME_CANVAS_ITEM(cursor)->canvas;
        int x, y, w,h;

        x = cursor->x;
        y = cursor->y;
        w = cursor->w;
        h = cursor->h;

        gnome_canvas_request_redraw (canvas, x, y, x+w, y+h);
}


void
gnucash_cursor_set_style (GnucashCursor  *cursor, SheetBlockStyle *style)
{
        g_return_if_fail (cursor != NULL);
        g_return_if_fail (GNUCASH_IS_CURSOR(cursor));

        cursor->style = style;
}


void
gnucash_cursor_get_phys (GnucashCursor *cursor, int *phys_row, int *phys_col)
{
        Table *table;
        int virt_row, virt_col;
        int cell_row, cell_col;

        g_return_if_fail (cursor != NULL);
        g_return_if_fail (GNUCASH_IS_CURSOR (cursor));

        table = cursor->sheet->table;

        virt_row =
		GNUCASH_ITEM_CURSOR(cursor->cursor[GNUCASH_CURSOR_BLOCK])->row;
        virt_col =
		GNUCASH_ITEM_CURSOR(cursor->cursor[GNUCASH_CURSOR_BLOCK])->col;

        cell_row =
		GNUCASH_ITEM_CURSOR(cursor->cursor[GNUCASH_CURSOR_CELL])->row;
        cell_col =
		GNUCASH_ITEM_CURSOR(cursor->cursor[GNUCASH_CURSOR_CELL])->col;

        *phys_row =
		table->rev_locators[virt_row][virt_col]->phys_row + cell_row;
        *phys_col =
		table->rev_locators[virt_row][virt_col]->phys_col + cell_col;
}


void
gnucash_cursor_get_virt (GnucashCursor *cursor, int *virt_row, int *virt_col,
                         int *cell_row, int *cell_col)
{
        Table *table;
        
        g_return_if_fail (cursor != NULL);
        g_return_if_fail (GNUCASH_IS_CURSOR (cursor));

        table = cursor->sheet->table;

        *virt_row =
		GNUCASH_ITEM_CURSOR(cursor->cursor[GNUCASH_CURSOR_BLOCK])->row;
        *virt_col =
		GNUCASH_ITEM_CURSOR(cursor->cursor[GNUCASH_CURSOR_BLOCK])->col;

        *cell_row =
		GNUCASH_ITEM_CURSOR(cursor->cursor[GNUCASH_CURSOR_CELL])->row;
        *cell_col =
		GNUCASH_ITEM_CURSOR(cursor->cursor[GNUCASH_CURSOR_CELL])->col;
}


void
gnucash_cursor_configure (GnucashCursor *cursor)
{
        GnomeCanvasItem *item;
        GnucashItemCursor *item_cursor;
        GnomeCanvasGroup *group;
        GnomeCanvas *canvas;
        gint x, y, w, h;
        double wx, wy;
                
        g_return_if_fail (cursor != NULL);
        g_return_if_fail (GNUCASH_IS_CURSOR (cursor));

        canvas = GNOME_CANVAS(GNOME_CANVAS_ITEM(cursor)->canvas);

        group = GNOME_CANVAS_GROUP(cursor);

        item = GNOME_CANVAS_ITEM (cursor);

        gnucash_cursor_get_pixel_coords (cursor, &x, &y, &w, &h);
        gnome_canvas_item_set (GNOME_CANVAS_ITEM(cursor),
			       "GnomeCanvasGroup::x", (double)x,
			       "GnomeCanvasGroup::y", (double)y,
			       NULL);
        
        item->x1 = cursor->x = x;
        item->y1 = cursor->y = y;
        cursor->w = w;
        cursor->h = h;

        item->x2 = x + w;
        item->y2 = y + h;

        item = cursor->cursor[GNUCASH_CURSOR_BLOCK];
        item_cursor = GNUCASH_ITEM_CURSOR (item);

        wx = 0;
        wy = 0;

        gnome_canvas_item_i2w (item, &wx, &wy);
        gnome_canvas_w2c (canvas, wx, wy, &item_cursor->x, &item_cursor->y);
        item_cursor->w = w;
        item_cursor->h = h;

        item->x1 = item_cursor->x;
        item->y1 = item_cursor->y;
        item->x2 = item_cursor->x + w;
        item->y2 = item_cursor->y + h;

        item = cursor->cursor[GNUCASH_CURSOR_CELL];
        item_cursor = GNUCASH_ITEM_CURSOR(item);
        
        gnucash_sheet_style_get_cell_pixel_rel_coords (cursor->style,
						       item_cursor->row,
						       item_cursor->col,
						       &x, &y, &w, &h);
        wx = x;
        wy = y;

        gnome_canvas_item_i2w (item, &wx, &wy);
        gnome_canvas_w2c (canvas, wx, wy, &item_cursor->x, &item_cursor->y);
        item_cursor->w = w;
        item_cursor->h = h;
        
        item->x1 =  item_cursor->x;
        item->y1 =  item_cursor->y;
        item->x2 =  item_cursor->x+ w;
        item->y2 =  item_cursor->y + h;
}


static void
gnucash_item_cursor_draw (GnomeCanvasItem *item, GdkDrawable *drawable,
			  int x, int y, int width, int height)
{
        GnucashItemCursor *item_cursor = GNUCASH_ITEM_CURSOR (item);
        GnucashCursor *cursor = GNUCASH_CURSOR(item->parent);
        gint dx, dy, dw, dh;

        switch (item_cursor->type) {
		case GNUCASH_CURSOR_BLOCK:
			dx = item_cursor->x - x;
			dy = item_cursor->y - y;
			dw = item_cursor->w;
			dh = item_cursor->h;

			/* draw the rectangle around the entire active 
			   virtual row */
			gdk_gc_set_line_attributes (cursor->gc, 1,
						    GDK_LINE_SOLID, -1, -1);
			gdk_gc_set_foreground (cursor->gc, &gn_black);
			gdk_gc_set_background (cursor->gc, &gn_white);
                
			gdk_draw_rectangle (drawable, cursor->gc, FALSE,
					    dx+2, dy+2, dw-4, dh-4);

			break;

		case GNUCASH_CURSOR_CELL:
			dx = item_cursor->x - x;
			dy = item_cursor->y - y;
			dw = item_cursor->w;
			dh = item_cursor->h;


			gdk_gc_set_line_attributes (cursor->gc, 1,
						    GDK_LINE_SOLID, -1, -1);

			gdk_gc_set_foreground (cursor->gc, &gn_light_gray);
			gdk_draw_rectangle (drawable, cursor->gc, FALSE,
					    dx+1, dy+1, dw-2, dh-2);

			gdk_gc_set_foreground (cursor->gc, &gn_black);      
			gdk_draw_rectangle (drawable, cursor->gc, FALSE,
					    dx+2, dy+2, dw-4, dh-4);
        }
}


static void
gnucash_cursor_set_block (GnucashCursor *cursor, gint block_row,
			  gint block_col)
{
        GnucashSheet *sheet;
        GnucashItemCursor *item_cursor;

        g_return_if_fail (cursor != NULL);
        g_return_if_fail (GNUCASH_IS_CURSOR (cursor));

        sheet  = cursor->sheet;
        item_cursor =
		GNUCASH_ITEM_CURSOR(cursor->cursor[GNUCASH_CURSOR_BLOCK]);

        if (block_row < 0 || block_row >= sheet->num_virt_rows
            || block_col < 0 || block_col >= sheet->num_virt_cols)
                return;

        cursor->style = gnucash_sheet_get_style (sheet, block_row, block_col);

        item_cursor->row = block_row;
        item_cursor->col = block_col;
}


static void
gnucash_cursor_set_cell (GnucashCursor *cursor, gint cell_row, gint cell_col)
{
        GnucashSheet *sheet;
        GnucashItemCursor *item_cursor;
        SheetBlockStyle *style;

        g_return_if_fail (cursor != NULL);
        g_return_if_fail (GNUCASH_IS_CURSOR (cursor));

        sheet  = cursor->sheet;
        item_cursor = GNUCASH_ITEM_CURSOR(cursor->cursor[GNUCASH_CURSOR_CELL]);
        style = cursor->style;

        if (cell_row < 0 || cell_row >= style->nrows
            || cell_col < 0 || cell_col >= style->ncols)
                return;

        item_cursor->row = cell_row;
        item_cursor->col = cell_col;
}

                   


void
gnucash_cursor_set (GnucashCursor *cursor, gint block_row, gint block_col,
                    gint cell_row, gint cell_col)
{
        GnucashSheet *sheet;
        
        g_return_if_fail (cursor != NULL);
        g_return_if_fail (GNUCASH_IS_CURSOR (cursor));

        sheet = cursor->sheet;

        gnucash_cursor_request_redraw (cursor);

        gnucash_cursor_set_block (cursor, block_row, block_col);
        gnucash_cursor_set_cell (cursor, cell_row, cell_col);

        gnucash_cursor_configure (cursor);

        gnome_canvas_item_set (GNOME_CANVAS_ITEM(sheet->header_item),
                               "GnucashHeader::cursor_type",
                               cursor->style->cursor_type,
                               "GnucashHeader::cursor_row", cell_row,
                               NULL);

        gnucash_cursor_request_redraw (cursor);
}


static void
gnucash_item_cursor_destroy (GtkObject *object)
{
	GNUCASH_ITEM_CURSOR(object);

        if (GTK_OBJECT_CLASS (gnucash_item_cursor_parent_class)->destroy)
                (*GTK_OBJECT_CLASS (gnucash_item_cursor_parent_class)->destroy)
			(object);
}


static void
gnucash_item_cursor_init (GnucashItemCursor *cursor)
{
        GnomeCanvasItem *item = GNOME_CANVAS_ITEM (cursor);

        item->x1 = 0;
        item->y1 = 0;
        item->x2 = 1;
        item->y2 = 1;

        cursor->col = 0;
        cursor->row   = 0;
}


static void
gnucash_cursor_realize (GnomeCanvasItem *item)
{
        GnucashCursor *cursor = GNUCASH_CURSOR (item);
        GdkWindow *window;

        if (GNOME_CANVAS_ITEM_CLASS (gnucash_cursor_parent_class)->realize)
                (*GNOME_CANVAS_ITEM_CLASS
		 (gnucash_cursor_parent_class)->realize)(item);

        window = GTK_WIDGET (item->canvas)->window;

        cursor->gc = gdk_gc_new (window);
}


static void
gnucash_cursor_unrealize (GnomeCanvasItem *item)
{
        GnucashCursor *cursor = GNUCASH_CURSOR (item);

	if (cursor->gc != NULL) {
		gdk_gc_unref (cursor->gc);
		cursor->gc = NULL;
	}

        if (GNOME_CANVAS_ITEM_CLASS (gnucash_cursor_parent_class)->unrealize)
                (*GNOME_CANVAS_ITEM_CLASS
		 (gnucash_cursor_parent_class)->unrealize)(item);
}


static void
gnucash_item_cursor_class_init (GnucashItemCursorClass *item_cursor_class)
{
        GtkObjectClass  *object_class;
        GnomeCanvasItemClass *item_class;

        gnucash_item_cursor_parent_class =
		gtk_type_class (gnome_canvas_item_get_type());

        object_class = (GtkObjectClass *) item_cursor_class;
        item_class = (GnomeCanvasItemClass *) item_cursor_class;

        object_class->destroy = gnucash_item_cursor_destroy;

        /* GnomeCanvasItem method overrides */
        item_class->draw        = gnucash_item_cursor_draw;
}


GtkType
gnucash_item_cursor_get_type (void)
{
        static GtkType gnucash_item_cursor_type = 0;

        if (!gnucash_item_cursor_type) {
                GtkTypeInfo gnucash_item_cursor_info = {
                        "GnucashItemCursor",
                        sizeof (GnucashItemCursor),
                        sizeof (GnucashItemCursorClass),
                        (GtkClassInitFunc) gnucash_item_cursor_class_init,
                        (GtkObjectInitFunc) gnucash_item_cursor_init,
                        NULL, /* reserved_1 */
                        NULL, /* reserved_2 */
                        (GtkClassInitFunc) NULL
                };

                gnucash_item_cursor_type =
			gtk_type_unique (gnome_canvas_item_get_type (),
					 &gnucash_item_cursor_info);
        }

        return gnucash_item_cursor_type;
}


static void
gnucash_cursor_destroy (GtkObject *object)
{
	GNUCASH_CURSOR(object);

        if (GTK_OBJECT_CLASS (gnucash_cursor_parent_class)->destroy)
                (*GTK_OBJECT_CLASS
		 (gnucash_cursor_parent_class)->destroy)(object);
}


static void
gnucash_cursor_set_arg (GtkObject *o, GtkArg *arg, guint arg_id)
{
        GnomeCanvasItem *item;
        GnucashCursor *cursor;

        item = GNOME_CANVAS_ITEM (o);
        cursor = GNUCASH_CURSOR (o);

        switch (arg_id){
        case ARG_SHEET:
                cursor->sheet = GTK_VALUE_POINTER (*arg);
                break;
        case ARG_GRID:
                cursor->grid = GTK_VALUE_POINTER (*arg);
                break;
        default:
                break;
        }
}


static void
gnucash_cursor_init (GnucashCursor *cursor)
{
        GnomeCanvasItem *item = GNOME_CANVAS_ITEM (cursor);

        item->x1 = 0;
        item->y1 = 0;
        item->x2 = 1;
        item->y2 = 1;
}


static void
gnucash_cursor_class_init (GnucashCursorClass *cursor_class)
{
        GtkObjectClass  *object_class;
        GnomeCanvasItemClass *item_class;

        gnucash_cursor_parent_class =
		gtk_type_class (gnome_canvas_group_get_type());

        object_class = (GtkObjectClass *) cursor_class;
        item_class = (GnomeCanvasItemClass *) cursor_class;

        gtk_object_add_arg_type ("GnucashCursor::sheet", GTK_TYPE_POINTER,
                                 GTK_ARG_WRITABLE, ARG_SHEET);
        gtk_object_add_arg_type ("GnucashCursor::grid", GTK_TYPE_POINTER,
                                 GTK_ARG_WRITABLE, ARG_GRID);

        object_class->set_arg = gnucash_cursor_set_arg;
        object_class->destroy = gnucash_cursor_destroy;

        /* GnomeCanvasItem method overrides */
        item_class->realize     = gnucash_cursor_realize;
        item_class->unrealize   = gnucash_cursor_unrealize;
}


GtkType
gnucash_cursor_get_type (void)
{
        static GtkType gnucash_cursor_type = 0;

        if (!gnucash_cursor_type) {
                GtkTypeInfo gnucash_cursor_info = {
                        "GnucashCursor",
                        sizeof (GnucashCursor),
                        sizeof (GnucashCursorClass),
                        (GtkClassInitFunc) gnucash_cursor_class_init,
                        (GtkObjectInitFunc) gnucash_cursor_init,
                        NULL, /* reserved_1 */
                        NULL, /* reserved_2 */
                        (GtkClassInitFunc) NULL
                };

                gnucash_cursor_type =
			gtk_type_unique (gnome_canvas_group_get_type (),
					 &gnucash_cursor_info);
        }

        return gnucash_cursor_type;
}


GnomeCanvasItem *
gnucash_cursor_new (GnomeCanvasGroup *parent) 
{
        GnomeCanvasItem *item;
        GnomeCanvasItem *cursor_item;
        GnucashCursor *cursor;
        GnucashItemCursor *item_cursor;

        g_return_val_if_fail (parent != NULL, NULL);
        g_return_val_if_fail (GNOME_IS_CANVAS_GROUP(parent), NULL);
        

        item = gnome_canvas_item_new (parent,
                                      gnucash_cursor_get_type(),
                                      NULL);

        cursor = GNUCASH_CURSOR(item);
        
        cursor_item = gnome_canvas_item_new (GNOME_CANVAS_GROUP(item),
					     gnucash_item_cursor_get_type(),
					     NULL);

        item_cursor = GNUCASH_ITEM_CURSOR (cursor_item);
        item_cursor->type = GNUCASH_CURSOR_CELL;
        
        cursor->cursor[GNUCASH_CURSOR_CELL] = cursor_item;

        cursor_item = gnome_canvas_item_new (GNOME_CANVAS_GROUP(item),
					     gnucash_item_cursor_get_type(),
					     NULL);

        item_cursor = GNUCASH_ITEM_CURSOR (cursor_item);
        item_cursor->type = GNUCASH_CURSOR_BLOCK;
        
        cursor->cursor[GNUCASH_CURSOR_BLOCK] = cursor_item;

        return item;
}


/*
  Local Variables:
  c-basic-offset: 8
  End:
*/
