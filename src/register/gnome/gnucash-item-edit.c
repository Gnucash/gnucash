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
 *  An editor for the gnucash sheet.  Cut and pasted
 *  from the gnumeric item-edit.c file.
 */


#include <config.h>

#include <gnome.h>
#include "gnucash-item-edit.h"
#include "gnucash-style.h"


#define CURSOR_LEN 4
static GnomeCanvasItem *item_edit_parent_class;

/* The arguments we take */
enum {
        ARG_0,
        ARG_SHEET,      /* The Sheet * argument */
        ARG_GTK_ENTRY,  /* The GtkEntry  argument */
        ARG_COMBO_SHOW, /* do we show the combo widget? */
};

/*
 * Returns the cordinates for the editor bounding box
 */
void
item_edit_get_pixel_coords (ItemEdit *item_edit, int *x, int *y,
			    int *w, int *h)
{
        GnucashSheet *sheet = item_edit->sheet;
        int xd, yd;

        gnome_canvas_get_scroll_offsets (GNOME_CANVAS(sheet), &xd, &yd);

        xd += gnucash_sheet_col_get_distance(sheet, sheet->left_block,
					     item_edit->virt_col)
                + sheet->left_block_offset;
        yd += gnucash_sheet_row_get_distance (sheet, sheet->top_block,
					      item_edit->virt_row)
                + sheet->top_block_offset;
        
        gnucash_sheet_style_get_cell_pixel_rel_coords (item_edit->style,
						       item_edit->cell_row,
						       item_edit->cell_col,
						       x, y, w, h);

        *x += xd;
        *y += yd;
}


static void
item_edit_draw (GnomeCanvasItem *item, GdkDrawable *drawable,
                int x, int y, int width, int height)
{
        ItemEdit *item_edit = ITEM_EDIT (item);
        GdkFont  *font;
        SheetBlockStyle *style = item_edit->style;
        GdkRectangle rect;

        int xd, yd, wd, hd, dx, dy;
        char *text;
        int  cursor_pos, text_len, first_part_len, total_len;
        int xoffset;

        /* be sure we're valid */
        if (item_edit->virt_row < 0 || item_edit->virt_col < 0)
                return;
        
        font = style->fonts[item_edit->cell_row][item_edit->cell_col];
 
        text = gtk_entry_get_text (GTK_ENTRY (item_edit->editor));
        text_len = strlen (text);
        cursor_pos = GTK_EDITABLE (item_edit->editor)->current_pos;

        total_len = gdk_text_measure (font, text, text_len);

        item_edit_get_pixel_coords (item_edit, &xd, &yd, &wd, &hd);

        dx = xd - x;
        dy = yd - y;

        gdk_gc_set_foreground(item_edit->gc,
			      style->active_bg_color[item_edit->cell_row]
			                            [item_edit->cell_col]);

        /* Do the drawing */
        gdk_draw_rectangle (drawable, item_edit->gc, TRUE,
                            dx+3, dy+3, wd-7, hd-5);
        
        first_part_len = gdk_text_width (font, text, cursor_pos);

        gdk_gc_set_foreground (item_edit->gc, &gn_black);

        rect.x = dx +3;
        rect.y = dy+3;
        rect.width = wd-7;
        rect.height = hd-5;
        
        gdk_gc_set_clip_rectangle (item_edit->gc, &rect);

        xoffset = MIN (0, rect.width - first_part_len);
        
        gdk_draw_text (drawable, font, item_edit->gc,
                       dx + xoffset + CELL_HPADDING, dy + hd - CELL_VPADDING,
		       text, cursor_pos);

        gdk_draw_line (drawable, item_edit->gc,
                       first_part_len + dx + xoffset + CELL_HPADDING,
                       dy + hd - CELL_VPADDING,
                       first_part_len + dx + CELL_HPADDING,
                       dy + hd - (font->ascent + CELL_VPADDING));

        gdk_draw_text (drawable, font, item_edit->gc,
                       dx + first_part_len + xoffset + CELL_HPADDING,
                       dy + hd - CELL_VPADDING,
                       text + cursor_pos, text_len - cursor_pos);

        gdk_gc_set_clip_rectangle (item_edit->gc, NULL);
}


static double
item_edit_point (GnomeCanvasItem *item, double c_x, double c_y, int cx, int cy,
                 GnomeCanvasItem **actual_item)
{
        int x, y, w, h;
 
        item_edit_get_pixel_coords (ITEM_EDIT (item), &x, &y, &w, &h);

        *actual_item = NULL;
        if ((cx < x) || (cy < y) || (cx > x+w) || (cy > y+w))
                return 10000.0;
 
        *actual_item = item;
        return 0.0;
}


static int
item_edit_event (GnomeCanvasItem *item, GdkEvent *event)
{
        return 0;
}


static void
item_edit_update (GnomeCanvasItem *item, double *affine, ArtSVP *clip_path,
		  int flags)
{
        ItemEdit *item_edit = ITEM_EDIT (item);
        int x, y, w, h;
        double xd, yd, hd;

        if (GNOME_CANVAS_ITEM_CLASS (item_edit_parent_class)->update)
                (*GNOME_CANVAS_ITEM_CLASS(item_edit_parent_class)->update)
			(item, affine, clip_path, flags);

        item_edit_get_pixel_coords (item_edit, &x, &y, &w, &h);
        item->x1 = x;
        item->y1 = y;
        item->x2 = x + w;
        item->y2 = y + h;

        xd = x+w;
        yd = y+h;
        hd = h;

#if 0
        gnome_canvas_item_set (item_edit->combo_item,
                               "x", 200.0,
                               "y", 200.0,
                               "height", 30.0, 
                               "width", 30.0,
                               "anchor", GTK_ANCHOR_SE,
                               "size_pixels", TRUE,
                               NULL);
#endif

        gnome_canvas_group_child_bounds (
                GNOME_CANVAS_GROUP (item->parent), item);
}


static void
item_edit_realize (GnomeCanvasItem *item)
{
        GnomeCanvas *canvas = item->canvas;
        GdkWindow *window;
        ItemEdit *item_edit;

        if (GNOME_CANVAS_ITEM_CLASS (item_edit_parent_class)->realize)
                (*GNOME_CANVAS_ITEM_CLASS
		 (item_edit_parent_class)->realize)(item);

        item_edit = ITEM_EDIT (item);
        window = GTK_WIDGET (canvas)->window;

        item_edit->gc = gdk_gc_new (window);
}


/*
 * Instance initialization
 */
static void
item_edit_init (ItemEdit *item_edit)
{
        GnomeCanvasItem *item = GNOME_CANVAS_ITEM (item_edit);


        item->x1 = 0;
        item->y1 = 0;
        item->x2 = 1;
        item->y2 = 1;

        /* Set invalid values so that we know when we have been fully
	   initialized */
        item_edit->sheet = NULL;
        item_edit->virt_col = -1;
        item_edit->virt_row = -1;
        item_edit->cell_col = -1;
        item_edit->cell_row = -1;
}


static void
queue_sync (ItemEdit *item_edit)
{
        GnomeCanvas *canvas = GNOME_CANVAS_ITEM (item_edit)->canvas;
        int x, y, w, h;

        item_edit_get_pixel_coords (item_edit, &x, &y, &w, &h);
 
        gnome_canvas_request_redraw (canvas, x, y, x+w, y+h);
}


static void
entry_changed (GtkEntry *entry, void *data)
{
        queue_sync(ITEM_EDIT (data));
}


static void
item_edit_destroy (GtkObject *o)
{
        ItemEdit *item_edit = ITEM_EDIT (o);

        gtk_signal_disconnect (GTK_OBJECT (item_edit->editor),
			       item_edit->signal);
        gtk_signal_disconnect (GTK_OBJECT (item_edit->editor),
			       item_edit->signal2);

        gdk_gc_destroy (item_edit->gc);
 
        if (GTK_OBJECT_CLASS (item_edit_parent_class)->destroy)
                (*GTK_OBJECT_CLASS (item_edit_parent_class)->destroy)(o);
}


static int
entry_event (GtkEntry *entry, GdkEvent *event, ItemEdit *item_edit)
{
        switch (event->type){
        case GDK_KEY_PRESS:
        case GDK_KEY_RELEASE:
        case GDK_BUTTON_PRESS:
                queue_sync (item_edit);

        default:
                break;
        }
        return FALSE;
}


static void
item_edit_set_editor (ItemEdit *item_edit, void *data)
{
        item_edit->editor = GTK_WIDGET (data);
        item_edit->signal = gtk_signal_connect
		(GTK_OBJECT (item_edit->editor), "changed",
		 GTK_SIGNAL_FUNC(entry_changed), item_edit);
        item_edit->signal2 = gtk_signal_connect_after
		(GTK_OBJECT (item_edit->editor), "event",
		 GTK_SIGNAL_FUNC(entry_event), item_edit);
}


void
item_edit_configure (ItemEdit *item_edit)
{
        GnucashSheet *sheet = item_edit->sheet;
        GnucashItemCursor *cursor;

        cursor = GNUCASH_ITEM_CURSOR
		(GNUCASH_CURSOR(sheet->cursor)->cursor[GNUCASH_CURSOR_BLOCK]);

        item_edit->virt_row = cursor->row;
        item_edit->virt_col = cursor->col;

        item_edit->style = gnucash_sheet_get_style (item_edit->sheet,
                                                    item_edit->virt_row,
						    item_edit->virt_col);

        cursor = GNUCASH_ITEM_CURSOR
		(GNUCASH_CURSOR(sheet->cursor)->cursor[GNUCASH_CURSOR_CELL]);

        item_edit->cell_row = cursor->row;
        item_edit->cell_col = cursor->col;

        item_edit_update (GNOME_CANVAS_ITEM(item_edit), NULL, NULL, 0);
}

        
static void
item_edit_set_arg (GtkObject *o, GtkArg *arg, guint arg_id)
{
        GnomeCanvasItem *item;
        ItemEdit *item_edit;
        gint show;

        item = GNOME_CANVAS_ITEM (o);
        item_edit = ITEM_EDIT (o);

        switch (arg_id){
        case ARG_SHEET:
                item_edit->sheet = (GnucashSheet *) GTK_VALUE_POINTER (*arg);
                break;
        case ARG_GTK_ENTRY:
                item_edit_set_editor (item_edit, GTK_VALUE_POINTER (*arg));
                break;
        case ARG_COMBO_SHOW:
                show = GTK_VALUE_BOOL (*arg);
#if 0                
                if (show) {
                        g_message("Showing");
                        if (item_edit->combo_item)
                                gnome_canvas_item_show (item_edit->combo_item);
                        if (GNOME_CANVAS_WIDGET(item_edit->combo_item)->widget)
                                gtk_widget_show
					(GNOME_CANVAS_WIDGET
					 (item_edit->combo_item)->widget);
                }
                else {
                        g_message("Hiding");                        
                        if (item_edit->combo_item)
                                gnome_canvas_item_hide (item_edit->combo_item);
                        if (GNOME_CANVAS_WIDGET(item_edit->combo_item)->widget)
                                gtk_widget_hide
					(GNOME_CANVAS_WIDGET
					 (item_edit->combo_item)->widget);
                }
#endif                
                item_edit_configure (item_edit);
                
        default:
                break;
        }
}

/*
 * ItemEdit class initialization
 */
static void
item_edit_class_init (ItemEditClass *item_edit_class)
{
        GtkObjectClass  *object_class;
        GnomeCanvasItemClass *item_class;

        item_edit_parent_class = gtk_type_class (gnome_canvas_item_get_type());
 
        object_class = (GtkObjectClass *) item_edit_class;
        item_class = (GnomeCanvasItemClass *) item_edit_class;

        gtk_object_add_arg_type ("ItemEdit::sheet", GTK_TYPE_POINTER,
                                 GTK_ARG_WRITABLE, ARG_SHEET);
        gtk_object_add_arg_type ("ItemEdit::GtkEntry", GTK_TYPE_POINTER,
                                 GTK_ARG_WRITABLE, ARG_GTK_ENTRY);
        gtk_object_add_arg_type ("ItemEdit::combo_show", GTK_TYPE_BOOL,
                                 GTK_ARG_WRITABLE, ARG_COMBO_SHOW);

        object_class->set_arg = item_edit_set_arg;
        object_class->destroy = item_edit_destroy;
 
        /* GnomeCanvasItem method overrides */
        item_class->update      = item_edit_update;
        item_class->draw        = item_edit_draw;
        item_class->point       = item_edit_point;
        item_class->event       = item_edit_event;
        item_class->realize     = item_edit_realize;
}

GtkType
item_edit_get_type (void)
{
        static GtkType item_edit_type = 0;

        if (!item_edit_type) {
                GtkTypeInfo item_edit_info = {
                        "ItemEdit",
                        sizeof (ItemEdit),
                        sizeof (ItemEditClass),
                        (GtkClassInitFunc) item_edit_class_init,
                        (GtkObjectInitFunc) item_edit_init,
                        NULL, /* reserved_1 */
                        NULL, /* reserved_2 */
                        (GtkClassInitFunc) NULL
                };

                item_edit_type =
			gtk_type_unique(gnome_canvas_item_get_type (),
					&item_edit_info);
        }

        return item_edit_type;
}


GnomeCanvasItem *
item_edit_new (GnomeCanvasGroup *parent, GnucashSheet *sheet, GtkWidget *entry)
{
        GnomeCanvasItem *item;
        ItemEdit *item_edit;
                
        item = gnome_canvas_item_new (parent,
				      item_edit_get_type (),
				      "ItemEdit::sheet", sheet,
				      "ItemEdit::GtkEntry", sheet->entry,
				      NULL);

        item_edit = ITEM_EDIT(item);
#if 0
        item_edit->combo_widget = gtk_combowidget_new();
        item_edit->combo_item =
		gnome_canvas_item_new (parent,
				       gnome_canvas_widget_get_type(),
				       "widget", item_edit->combo_widget,
				       NULL);
#endif
        return item;
}


/*
  Local Variables:
  c-basic-offset: 8
  End:
*/
