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
 * The Gnucash Header Canvas
 *
 * Author:
 *     Heath Martin <martinh@pegasus.cc.ucf.edu>
 */

#include "gnucash-sheet.h"
#include "gnucash-color.h"
#include "gnucash-style.h"

#include "gnucash-header.h"

static GnomeCanvasItem *gnucash_header_parent_class;

enum {
        ARG_0,
        ARG_SHEET,
        ARG_TYPE,
};


static void
gnucash_header_update (GnomeCanvasItem *item, double *affine,
		       ArtSVP *clip_path, int flags)
{
        if (GNOME_CANVAS_ITEM_CLASS(gnucash_header_parent_class)->update)
                (*GNOME_CANVAS_ITEM_CLASS(gnucash_header_parent_class)->update)
			(item, affine, clip_path, flags);

        item->x1 = 0;
        item->y1 = 0;
        item->x2 = INT_MAX;
        item->y2 = INT_MAX;
}


/* FIXME:  for now, let's only draw one row, to avoid resizing. */
static void
gnucash_header_draw (GnomeCanvasItem *item, GdkDrawable *drawable,
                     int x, int y, int width, int height)
{
        GnucashHeader *header = GNUCASH_HEADER(item);
        SheetBlockStyle *style = header->style;
        int i, j;
        int xpaint, ypaint;
        int w = 0, h = 0;
        gchar *text;
        GdkFont *font;

        gdk_gc_set_foreground(header->gc, &gn_white);
	gdk_draw_rectangle(drawable, header->gc, TRUE, 0, 0, width, height);

        gdk_gc_set_line_attributes (header->gc, 1, GDK_LINE_SOLID, -1, -1);
        gdk_gc_set_foreground (header->gc, &gn_black);
        gdk_draw_rectangle (drawable, header->gc, FALSE,
                            -x, -y, style->width-1, style->height);
        gdk_draw_line (drawable, header->gc,
                       -x, style->height+1, style->width-1, style->height+1);

        gdk_gc_set_line_attributes (header->gc, 1, GDK_LINE_SOLID, -1, -1);
        gdk_gc_set_background (header->gc, &gn_white);
        gdk_gc_set_foreground (header->gc, &gn_black);
        font = gnucash_default_font;

        xpaint = -x;
        ypaint = -y;

        for (i = 0; i < 1; i++) {
                for (j = 0; j < style->ncols; j++) {
                        w = style->pixel_widths[i][j];
                        h = style->pixel_heights[i][j];
                        
                        gdk_draw_rectangle (drawable, header->gc, FALSE,
                                            xpaint, ypaint, w, h);
                        
                        text = style->labels[i][j];
                        
                        if (text) {
                                gint x_offset, y_offset;
                                GdkRectangle rect;
                                
                                switch (style->alignments[i][j ]) {
                                default:
                                case GTK_JUSTIFY_LEFT:
                                case GTK_JUSTIFY_FILL:
                                case GTK_JUSTIFY_CENTER:
                                        x_offset = CELL_HPADDING;
                                        y_offset = h - CELL_VPADDING;
                                        break;
                                case GTK_JUSTIFY_RIGHT:
                                        y_offset = h - CELL_VPADDING;
                                        x_offset = w - CELL_VPADDING
                                                - gdk_string_measure(font,
								     text);
                                        break;
                                }
                                
                                rect.x = xpaint + CELL_HPADDING;
                                rect.y = ypaint + CELL_VPADDING;
                                rect.width = w - 2*CELL_HPADDING;
                                rect.height = h;
                                
                                gdk_gc_set_clip_rectangle (header->gc, &rect);
                                
                                gdk_draw_string (drawable,
                                                 font,
                                                 header->gc,
                                                 xpaint + x_offset,
                                                 ypaint + y_offset,
                                                 text);
                                gdk_gc_set_clip_rectangle (header->gc, NULL);
                                
                        }
                        
                        xpaint += w;
                }
                ypaint += h;
        }
}


static void
gnucash_header_request_redraw (GnucashHeader *header)
{
        GnomeCanvas *canvas = GNOME_CANVAS_ITEM(header)->canvas;

        gnome_canvas_request_redraw (canvas, 0, 0, INT_MAX, INT_MAX);
}

static void
gnucash_header_realize (GnomeCanvasItem *item)
{
        GnucashHeader *header = GNUCASH_HEADER (item);
        GdkWindow *window;

        	
        if (GNOME_CANVAS_ITEM_CLASS (gnucash_header_parent_class)->realize)
                (*GNOME_CANVAS_ITEM_CLASS
		 (gnucash_header_parent_class)->realize)(item);

        window = GTK_WIDGET (item->canvas)->window;
        
        header->gc = gdk_gc_new (window);
}


static void
gnucash_header_unrealize (GnomeCanvasItem *item)
{
        GnucashHeader *header = GNUCASH_HEADER (item);

	if (header->gc != NULL) {
		gdk_gc_unref (header->gc);
		header->gc = NULL;
	}

        if (GNOME_CANVAS_ITEM_CLASS (gnucash_header_parent_class)->unrealize)
                (*GNOME_CANVAS_ITEM_CLASS
		 (gnucash_header_parent_class)->unrealize)(item);
}


static void
gnucash_header_destroy (GtkObject *object)
{
	GNUCASH_HEADER(object);

        if (GTK_OBJECT_CLASS (gnucash_header_parent_class)->destroy)
                (*GTK_OBJECT_CLASS
		 (gnucash_header_parent_class)->destroy)(object);
}


/* FIXME:  for now, let's only draw one row, to avoid
 * resizing.
 */
void
gnucash_header_reconfigure (GnucashHeader *header)
{
        GnomeCanvas *canvas = GNOME_CANVAS_ITEM(header)->canvas;
        int w,h;
        double old_w, old_h;
        
        header->style = header->sheet->cursor_style[header->type];
        
        w = header->style->width+1;
        h = header->style->pixel_heights[0][0]+2;

        gnome_canvas_get_scroll_region(canvas, NULL, NULL, &old_w, &old_h);

        if ((int)old_w != w || (int)old_h != h) {
                gnome_canvas_set_scroll_region(GNOME_CANVAS(canvas),
					       0, 0, w, h);

                gtk_widget_set_usize (GTK_WIDGET(canvas), w, h);
        }

        gnucash_header_request_redraw (header);
}


static void
gnucash_header_set_arg (GtkObject *o, GtkArg *arg, guint arg_id)
{
        GnucashHeader *header;
        gint new_type;
        gint needs_update = FALSE;

        header = GNUCASH_HEADER (o);

        switch (arg_id){
        case ARG_SHEET:
                header->sheet = GTK_VALUE_POINTER (*arg);
                needs_update = TRUE;
                break;
        case ARG_TYPE:
                new_type = GTK_VALUE_INT (*arg);
                if (new_type != header->type) {
                        header->type = new_type;
                        needs_update = TRUE;
                }
                break;
        default:
                break;
        }

        if (header->sheet && header->type > -1 && needs_update)
                gnucash_header_reconfigure (header);
}


static void
gnucash_header_init (GnucashHeader *header)
{
        header->sheet = NULL;
        header->type = -1;
}


static void
gnucash_header_class_init (GnucashHeaderClass *header_class)
{
        GtkObjectClass  *object_class;
        GnomeCanvasItemClass *item_class;

        gnucash_header_parent_class =
		gtk_type_class(gnome_canvas_item_get_type());

        object_class = (GtkObjectClass *) header_class;
        item_class = (GnomeCanvasItemClass *) header_class;

        gtk_object_add_arg_type ("GnucashHeader::sheet", GTK_TYPE_POINTER,
                                 GTK_ARG_WRITABLE, ARG_SHEET);
        gtk_object_add_arg_type ("GnucashHeader::type", GTK_TYPE_INT,
                                 GTK_ARG_WRITABLE, ARG_TYPE);

        object_class->set_arg = gnucash_header_set_arg;
        object_class->destroy = gnucash_header_destroy;

        item_class->realize     = gnucash_header_realize;
        item_class->unrealize   = gnucash_header_unrealize;
        item_class->update      = gnucash_header_update;
        item_class->draw        = gnucash_header_draw;
}


GtkType
gnucash_header_get_type (void)
{
        static GtkType gnucash_header_type = 0;

        if (!gnucash_header_type) {
                GtkTypeInfo gnucash_header_info = {
                        "GnucashHeader",
                        sizeof (GnucashHeader),
                        sizeof (GnucashHeaderClass),
                        (GtkClassInitFunc) gnucash_header_class_init,
                        (GtkObjectInitFunc) gnucash_header_init,
                        NULL, /* reserved_1 */
                        NULL, /* reserved_2 */
                        (GtkClassInitFunc) NULL
                };

                gnucash_header_type =
			gtk_type_unique(gnome_canvas_item_get_type (),
					&gnucash_header_info);
        }

        return gnucash_header_type;
}


static void
gnucash_header_realized (GtkWidget *widget, gpointer data)
{
       	gdk_window_set_back_pixmap (GTK_LAYOUT (widget)->bin_window,
				    NULL, FALSE);
}


GtkWidget *
gnucash_header_new (GnucashSheet *sheet)
{
        GnomeCanvasGroup *group;
        GnomeCanvasItem *item;
        GtkWidget *canvas;

        canvas = gnome_canvas_new ();

        gtk_signal_connect (GTK_OBJECT (canvas), "realize",
                            (GtkSignalFunc) gnucash_header_realized,
                            NULL);

        group = GNOME_CANVAS_GROUP (GNOME_CANVAS (canvas)->root);

        item = gnome_canvas_item_new (group,
                                      gnucash_header_get_type (),
                                      "GnucashHeader::sheet", sheet,
                                      NULL);

        sheet->header_item = item;

        gtk_widget_show (canvas);
        return canvas;
}


/*
  Local Variables:
  c-basic-offset: 8
  End:
*/


