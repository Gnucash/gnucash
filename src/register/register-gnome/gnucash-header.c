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
 * The Gnucash Header Canvas
 *
 * Authors:
 *     Heath Martin <martinh@pegasus.cc.ucf.edu>
 *     Dave Peticolas <dave@krondo.com>
 */

#include "config.h"

#include <string.h>

#include "gnucash-sheet.h"
#include "gnucash-sheetP.h"
#include "gnucash-color.h"
#include "gnucash-style.h"
#include "gnucash-grid.h"
#include "gnucash-cursor.h"
#include "gnucash-item-edit.h"

#include "gnucash-header.h"

static GnomeCanvasItem *parent_class;

enum
{
    PROP_0,
    PROP_SHEET,       /*  the sheet this header is associated with */
    PROP_CURSOR_NAME, /* the name of the current cursor */
};


static void
gnc_header_update (GnomeCanvasItem *item, double *affine,
                   ArtSVP *clip_path, int flags)
{
    if (GNOME_CANVAS_ITEM_CLASS (parent_class)->update)
        (*GNOME_CANVAS_ITEM_CLASS (parent_class)->update)
        (item, affine, clip_path, flags);

    item->x1 = 0;
    item->y1 = 0;
    item->x2 = (INT_MAX / 2) - 1;
    item->y2 = (INT_MAX / 2) - 1;
}


static void
gnc_header_draw (GnomeCanvasItem *item, GdkDrawable *drawable,
                 int x, int y, int width, int height)
{
    GncHeader *header = GNC_HEADER(item);
    SheetBlockStyle *style = header->style;
    Table *table = header->sheet->table;
    VirtualLocation virt_loc;
    VirtualCell *vcell;
    CellDimensions *cd;
    GdkColor *bg_color, *fg_color;
    int xpaint, ypaint;
    const char *text;
    CellBlock *cb;
    guint32 argb, color_type;
    int i, j;
    int w, h;
    PangoLayout *layout;

    virt_loc.vcell_loc.virt_row = 0;
    virt_loc.vcell_loc.virt_col = 0;
    virt_loc.phys_row_offset = 0;
    virt_loc.phys_col_offset = 0;

    if (header->sheet->use_theme_colors)
    {
        color_type = gnc_table_get_gtkrc_bg_color (table, virt_loc,
                     NULL);
        bg_color = get_gtkrc_color(header->sheet, color_type);
        color_type = gnc_table_get_gtkrc_fg_color (table, virt_loc);
        fg_color = get_gtkrc_color(header->sheet, color_type);
    }
    else
    {
        argb = gnc_table_get_bg_color (table, virt_loc, NULL);
        bg_color = gnucash_color_argb_to_gdk (argb);
        argb = gnc_table_get_fg_color (table, virt_loc);
        fg_color = gnucash_color_argb_to_gdk (argb);
    }

    h = style->dimensions->height;
    h *= header->num_phys_rows;
    h /= header->style->nrows;

    gdk_gc_set_foreground (header->gc, bg_color);

    gdk_draw_rectangle (drawable, header->gc, TRUE, 0, 0,
                        style->dimensions->width, h);

    gdk_gc_set_line_attributes (header->gc, 1, GDK_LINE_SOLID, GDK_CAP_NOT_LAST, GDK_JOIN_MITER);
    gdk_gc_set_foreground (header->gc, fg_color);

    gdk_draw_rectangle (drawable, header->gc, FALSE, -x, -y,
                        style->dimensions->width, h);

    gdk_draw_line (drawable, header->gc, 0, h + 1,
                   style->dimensions->width, h + 1);

    gdk_gc_set_line_attributes (header->gc, 1, GDK_LINE_SOLID, GDK_CAP_NOT_LAST, GDK_JOIN_MITER);
    gdk_gc_set_background (header->gc, &gn_white);
    gdk_gc_set_foreground (header->gc, fg_color);
    /*font = gnucash_register_font;*/

    vcell = gnc_table_get_virtual_cell
            (table, table->current_cursor_loc.vcell_loc);
    cb = vcell ? vcell->cellblock : NULL;

    ypaint = -y;
    h = 0;

    for (i = 0; i < style->nrows; i++)
    {
        xpaint = -x;
        virt_loc.phys_row_offset = i;

        /* TODO: This routine is duplicated in several places.
           Can we abstract at least the cell drawing routine?
           That way we'll be sure everything is drawn
           consistently, and cut down on maintenance issues. */

        for (j = 0; j < style->ncols; j++)
        {
            /*                        gint x_offset, y_offset;*/
            GdkRectangle rect;
            BasicCell *cell;

            virt_loc.phys_col_offset = j;

            cd = gnucash_style_get_cell_dimensions (style, i, j);

            if (header->in_resize && (j == header->resize_col))
                w = header->resize_col_width;
            else
                w = cd->pixel_width;

            cell = gnc_cellblock_get_cell (cb, i, j);
            if (!cell || !cell->cell_name)
            {
                xpaint += w;
                continue;
            }

            h = cd->pixel_height;

            gdk_draw_rectangle (drawable, header->gc, FALSE,
                                xpaint, ypaint, w, h);

            virt_loc.vcell_loc =
                table->current_cursor_loc.vcell_loc;
            text = gnc_table_get_label (table, virt_loc);
            if (!text)
                text = "";

            layout = gtk_widget_create_pango_layout (GTK_WIDGET (header->sheet), text);

            /*y_offset = ((h / 2) +
                                    (((font->ascent + font->descent) / 2) -
                                     font->descent));
                        y_offset++;*/

            switch (gnc_table_get_align (table, virt_loc))
            {
            default:
            case CELL_ALIGN_LEFT:
                pango_layout_set_alignment (layout, PANGO_ALIGN_LEFT);
                break;

            case CELL_ALIGN_RIGHT:
                pango_layout_set_alignment (layout, PANGO_ALIGN_RIGHT);
                break;

            case CELL_ALIGN_CENTER:
                pango_layout_set_alignment (layout, PANGO_ALIGN_CENTER);
                break;
            }

            rect.x = xpaint + CELL_HPADDING;
            rect.y = ypaint + 1;
            rect.width = MAX (0, w - (2 * CELL_HPADDING));
            rect.height = h - 2;

            gdk_gc_set_clip_rectangle (header->gc, &rect);

            gdk_draw_layout (drawable,
                             header->gc,
                             xpaint + CELL_HPADDING,
                             ypaint + 1,
                             layout);

            g_object_unref (layout);

            gdk_gc_set_clip_rectangle (header->gc, NULL);

            xpaint += w;
        }

        ypaint += h;
    }
}


void
gnc_header_request_redraw (GncHeader *header)
{
    GnomeCanvas *canvas = GNOME_CANVAS_ITEM(header)->canvas;

    if (header->style == NULL)
        return;

    gnome_canvas_request_redraw (canvas, 0, 0,
                                 header->width + 1,
                                 header->height + 1);
}


static void
gnc_header_realize (GnomeCanvasItem *item)
{
    GncHeader *header = GNC_HEADER (item);
    GdkWindow *window;

    if (GNOME_CANVAS_ITEM_CLASS (parent_class)->realize)
        GNOME_CANVAS_ITEM_CLASS (parent_class)->realize (item);

    window = GTK_WIDGET (item->canvas)->window;

    header->gc = gdk_gc_new (window);
}


static void
gnc_header_unrealize (GnomeCanvasItem *item)
{
    GncHeader *header = GNC_HEADER (item);

    if (header->gc != NULL)
    {
        g_object_unref (header->gc);
        header->gc = NULL;
    }

    if (header->resize_cursor != NULL)
        gdk_cursor_unref (header->resize_cursor);
    header->resize_cursor = NULL;

    if (header->normal_cursor != NULL)
        gdk_cursor_unref (header->normal_cursor);
    header->normal_cursor = NULL;

    if (GNOME_CANVAS_ITEM_CLASS (parent_class)->unrealize)
        GNOME_CANVAS_ITEM_CLASS (parent_class)->unrealize (item);
}


static void
gnc_header_finalize (GObject *object)
{
    GncHeader *header;

    header = GNC_HEADER (object);

    g_free (header->cursor_name);
    header->cursor_name = NULL;

    G_OBJECT_CLASS (parent_class)->finalize (object);
}


void
gnc_header_reconfigure (GncHeader *header)
{
    GnomeCanvas *canvas;
    GnucashSheet *sheet;
    SheetBlockStyle *old_style;
    int w, h;

    g_return_if_fail (header != NULL);
    g_return_if_fail (GNC_IS_HEADER (header));

    canvas = GNOME_CANVAS_ITEM(header)->canvas;
    sheet = GNUCASH_SHEET(header->sheet);
    old_style = header->style;

    header->style = gnucash_sheet_get_style_from_cursor
                    (sheet, header->cursor_name);

    if (header->style == NULL)
        return;

    sheet->width = header->style->dimensions->width;

    w = header->style->dimensions->width;
    h = header->style->dimensions->height;
    h *= header->num_phys_rows;
    h /= header->style->nrows;
    h += 2;

    if (header->height != h ||
            header->width != w  ||
            header->style != old_style)
    {
        header->height = h;
        header->width = w;

        gnome_canvas_set_scroll_region (GNOME_CANVAS(canvas),
                                        0, 0, w, h);

        gtk_widget_set_size_request (GTK_WIDGET(canvas), -1, h);

        gnc_header_request_redraw (header);
    }
}

void
gnc_header_set_header_rows (GncHeader *header,
                            int num_phys_rows)
{
    g_return_if_fail (header != NULL);
    g_return_if_fail (GNC_IS_HEADER (header));

    header->num_phys_rows = num_phys_rows;
}

static double
gnc_header_point (GnomeCanvasItem *item,
                  double x, double y, int cx, int cy,
                  GnomeCanvasItem **actual_item)
{
    *actual_item = item;
    return 0.0;
}

/*
 *  Returns FALSE if pointer not on a resize line, else returns
 *  TRUE. Returns the index of the column to the left in the col
 *  argument.
 */
static gboolean
pointer_on_resize_line (GncHeader *header, int x, int y, int *col)
{
    SheetBlockStyle *style = header->style;
    gboolean on_the_line = FALSE;
    CellDimensions *cd;
    int pixels = 0;
    int j;

    for (j = 0; j < style->ncols; j++)
    {
        cd = gnucash_style_get_cell_dimensions (style, 0, j);
        pixels += cd->pixel_width;
        if (x >= pixels - 1 && x <= pixels + 1)
            on_the_line = TRUE;
        if (x <= pixels + 1)
            break;
    }

    if (col != NULL)
        *col = j;

    return on_the_line;
}

static int
find_resize_col (GncHeader *header, int col)
{
    SheetBlockStyle *style = header->style;
    CellDimensions *cd;
    int start = col;

    if (col < 0 || col >= style->ncols)
        return -1;

    /* skip to the right over zero-width columns */
    while ((col + 1 < style->ncols) &&
            (cd = gnucash_style_get_cell_dimensions (style, 0, col + 1)) &&
            (cd->pixel_width == 0))
        col++;

    /* now go back left till we have a resizable column */
    while (col >= start)
    {
        if (gnucash_style_col_is_resizable (style, col))
            return col;
        else
            col--;
    }

    /* didn't find a resizable column to the right of col */
    return -1;
}

static void
gnc_header_resize_column (GncHeader *header, gint col, gint width)
{
    GnucashSheet *sheet = header->sheet;

    gnucash_sheet_set_col_width (sheet, col, width);

    gnucash_cursor_configure (GNUCASH_CURSOR(sheet->cursor));
    gnc_item_edit_configure (gnucash_sheet_get_item_edit (sheet));

    gnc_header_reconfigure (header);
    gnucash_sheet_set_scroll_region (sheet);
    gnucash_sheet_update_adjustments (sheet);

    gnc_header_request_redraw (header);
    gnucash_sheet_redraw_all (sheet);
}

static void
gnc_header_auto_resize_column (GncHeader *header, gint col)
{
    int width;

    width = gnucash_sheet_col_max_width (header->sheet, 0, col);

    gnc_header_resize_column (header, col, width);
}

static gint
gnc_header_event (GnomeCanvasItem *item, GdkEvent *event)
{
    GncHeader *header = GNC_HEADER(item);
    GnomeCanvas *canvas = item->canvas;
    int x, y;
    int col;

    switch (event->type)
    {
    case GDK_MOTION_NOTIFY:

        gnome_canvas_w2c (canvas, event->motion.x, event->motion.y,
                          &x, &y);

        if (header->in_resize)
        {
            int change = x - header->resize_x;
            int new_width;

            if (!header->needs_ungrab)
            {
                gnome_canvas_item_grab (item,
                                        GDK_POINTER_MOTION_MASK |
                                        GDK_BUTTON_RELEASE_MASK,
                                        header->resize_cursor,
                                        event->button.time);
                header->needs_ungrab = TRUE;
            }

            new_width = header->resize_col_width + change;

            if (new_width >= 0)
            {
                header->resize_x = x;
                header->resize_col_width = new_width;
                gnc_header_request_redraw (header);
            }

            break;
        }

        if (pointer_on_resize_line(header, x, y, &col) &&
                gnucash_style_col_is_resizable (header->style, col))
            gdk_window_set_cursor (GTK_WIDGET(canvas)->window,
                                   header->resize_cursor);
        else
            gdk_window_set_cursor (GTK_WIDGET(canvas)->window,
                                   header->normal_cursor);
        break;

    case GDK_BUTTON_PRESS:
    {
        int col;

        if (event->button.button != 1)
            break;

        gnome_canvas_w2c (canvas, event->button.x, event->button.y,
                          &x, &y);

        if (pointer_on_resize_line (header, x, y, &col))
            col = find_resize_col (header, col);
        else
            col = -1;

        if (col > -1)
        {
            CellDimensions *cd;

            cd = gnucash_style_get_cell_dimensions
                 (header->style, 0, col);

            header->in_resize = TRUE;
            header->resize_col = col;
            header->resize_col_width = cd->pixel_width;
            header->resize_x = x;
        }

        break;
    }
    case GDK_BUTTON_RELEASE:
    {
        if (event->button.button != 1)
            break;

        gnome_canvas_w2c (canvas, event->button.x, event->button.y,
                          &x, &y);

        if (header->in_resize)
        {
            if (header->needs_ungrab)
            {
                gnome_canvas_item_ungrab (item,
                                          event->button.time);
                header->needs_ungrab = FALSE;

                gnc_header_resize_column
                (header,
                 header->resize_col,
                 header->resize_col_width);
            }
            header->in_resize = FALSE;
            header->resize_col = -1;
        }

        break;
    }

    case GDK_2BUTTON_PRESS:
    {
        gboolean on_line;
        int ptr_col;
        int resize_col;

        if (event->button.button != 1)
            break;

        gnome_canvas_w2c (canvas, event->button.x, event->button.y,
                          &x, &y);

        on_line = pointer_on_resize_line (header, x, y, &ptr_col);

        /* If we're on a resize line and the column to the right is zero
           width, resize that one. */
        if (on_line)
            resize_col = find_resize_col (header, ptr_col);
        else
            resize_col = ptr_col;

        if (resize_col > -1)
        {
            header->in_resize = FALSE;
            header->resize_col = -1;
            if (header->needs_ungrab)
            {
                gnome_canvas_item_ungrab (item,
                                          event->button.time);
                header->needs_ungrab = FALSE;
            }

            gnc_header_auto_resize_column (header, resize_col);
        }

    }
    break;

    default:
        break;
    }

    return TRUE;
}


/* Note that g_value_set_object() refs the object, as does
 * g_object_get(). But g_object_get() only unrefs once when it disgorges
 * the object, leaving an unbalanced ref, which leaks. So instead of
 * using g_value_set_object(), use g_value_take_object() which doesn't
 * ref the object when used in get_property().
 */
static void
gnc_header_get_property (GObject *object,
                         guint param_id,
                         GValue *value,
                         GParamSpec *pspec)
{
    GncHeader *header = GNC_HEADER (object);

    switch (param_id)
    {
    case PROP_SHEET:
        g_value_take_object (value, header->sheet);
        break;
    case PROP_CURSOR_NAME:
        g_value_set_string (value, header->cursor_name);
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
        break;
    }
}

static void
gnc_header_set_property (GObject *object,
                         guint param_id,
                         const GValue *value,
                         GParamSpec *pspec)
{
    GncHeader *header = GNC_HEADER (object);
    GtkLayout *layout = GTK_LAYOUT (GNOME_CANVAS_ITEM (header)->canvas);
    gboolean needs_update = FALSE;
    gchar *old_name;

    switch (param_id)
    {
    case PROP_SHEET:
        header->sheet = GNUCASH_SHEET (g_value_get_object (value));
        gtk_layout_set_hadjustment (layout, header->sheet->hadj);
        needs_update = TRUE;
        break;
    case PROP_CURSOR_NAME:
        old_name = header->cursor_name;

        header->cursor_name = g_value_dup_string (value);
        needs_update = !old_name || !header->cursor_name ||
                       strcmp (old_name, header->cursor_name) != 0;
        g_free (old_name);
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
        break;
    }

    if ((header->sheet != NULL) && needs_update)
        gnc_header_reconfigure (header);
}


static void
gnc_header_init (GncHeader *header)
{
    header->sheet = NULL;
    header->cursor_name = NULL;
    header->in_resize = FALSE;
    header->resize_col = -1;
    header->resize_cursor = gdk_cursor_new (GDK_SB_H_DOUBLE_ARROW);
    header->normal_cursor = NULL;
    header->height = 20;
    header->width = 400;
    header->style = NULL;
}


static void
gnc_header_class_init (GncHeaderClass *header_class)
{
    GObjectClass  *object_class = G_OBJECT_CLASS (header_class);
    GnomeCanvasItemClass *item_class = GNOME_CANVAS_ITEM_CLASS (header_class);

    parent_class = g_type_class_peek_parent (header_class);

    object_class->finalize = gnc_header_finalize;
    object_class->get_property = gnc_header_get_property;
    object_class->set_property = gnc_header_set_property;

    g_object_class_install_property (object_class,
                                     PROP_SHEET,
                                     g_param_spec_object ("sheet",
                                             "Sheet Value",
                                             "Sheet Value",
                                             GNUCASH_TYPE_SHEET,
                                             G_PARAM_READWRITE));
    g_object_class_install_property (object_class,
                                     PROP_CURSOR_NAME,
                                     g_param_spec_string ("cursor_name",
                                             "Cursor Name",
                                             "Cursor Name",
                                             CURSOR_HEADER,
                                             G_PARAM_READWRITE));


    item_class->realize   = gnc_header_realize;
    item_class->unrealize = gnc_header_unrealize;
    item_class->update    = gnc_header_update;
    item_class->draw      = gnc_header_draw;
    item_class->event     = gnc_header_event;
    item_class->point     = gnc_header_point;
}


GType
gnc_header_get_type (void)
{
    static GType gnc_header_type = 0;

    if (!gnc_header_type)
    {
        static const GTypeInfo gnc_header_info =
        {
            sizeof (GncHeaderClass),
            NULL,
            NULL,
            (GClassInitFunc) gnc_header_class_init,
            NULL,
            NULL,
            sizeof (GncHeader),
            0,
            (GInstanceInitFunc) gnc_header_init
        };

        gnc_header_type = g_type_register_static (gnome_canvas_item_get_type (),
                          "GncHeader",
                          &gnc_header_info, 0);
    }

    return gnc_header_type;
}


static void
gnc_header_realized (GtkWidget *widget, gpointer data)
{
    gdk_window_set_back_pixmap (GTK_LAYOUT (widget)->bin_window,
                                NULL, FALSE);
}


GtkWidget *
gnc_header_new (GnucashSheet *sheet)
{
    GnomeCanvasGroup *group;
    GnomeCanvasItem *item;
    GtkWidget *canvas;

    canvas = gnome_canvas_new ();

    g_signal_connect (G_OBJECT (canvas), "realize",
                      G_CALLBACK (gnc_header_realized),
                      NULL);

    group = GNOME_CANVAS_GROUP (GNOME_CANVAS (canvas)->root);

    item = gnome_canvas_item_new (group,
                                  gnc_header_get_type (),
                                  "sheet", sheet,
                                  "cursor_name", CURSOR_HEADER,
                                  NULL);

    sheet->header_item = item;

    gtk_widget_show (canvas);

    return canvas;
}


