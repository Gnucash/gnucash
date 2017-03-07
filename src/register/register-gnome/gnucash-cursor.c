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
 * The Gnucash Cursor Canvas Item
 *
 *  Based heavily (i.e., cut and pasted from) on the Gnumeric ItemCursor.
 *
 * Authors:
 *     Heath Martin   <martinh@pegasus.cc.ucf.edu>
 *     Dave Peticolas <dave@krondo.com>
 */

#include "config.h"

#include "gnucash-color.h"
#include "gnucash-cursor.h"
#include "gnucash-sheet.h"
#include "gnucash-sheetP.h"
#include "gnucash-style.h"

static GtkDrawingArea *gnucash_cursor_parent_class;

enum
{
    PROP_0,
    PROP_SHEET,
};


static void
gnucash_cursor_get_pixel_coords (GnucashCursor *cursor,
                                 gint *x, gint *y,
                                 gint *w, gint *h)
{
    GnucashSheet *sheet = cursor->sheet;
    VirtualCellLocation vcell_loc;
    CellDimensions *cd;
    VirtualCell *vcell;
    SheetBlock *block;
    gint col;

    vcell_loc.virt_row = cursor->row;
    vcell_loc.virt_col = cursor->col;

    block = gnucash_sheet_get_block (sheet, vcell_loc);
    if (!block)
        return;

    vcell = gnc_table_get_virtual_cell (sheet->table, vcell_loc);
    if (!vcell)
        return;

    for (col = 0; col < vcell->cellblock->num_cols; col++)
    {
        BasicCell *cell;

        cell = gnc_cellblock_get_cell (vcell->cellblock, 0, col);
        if (cell && cell->cell_name)
            break;
    }

    *y = block->origin_y;

    cd = gnucash_style_get_cell_dimensions (block->style, 0, col);
    if (cd)
        *x = cd->origin_x;
    else
        *x = block->origin_x;

    for (col = vcell->cellblock->num_cols - 1; col >= 0; col--)
    {
        BasicCell *cell;

        cell = gnc_cellblock_get_cell (vcell->cellblock, 0, col);
        if (cell && cell->cell_name)
            break;
    }

    *h = block->style->dimensions->height;

    cd = gnucash_style_get_cell_dimensions (block->style, 0, col);
    if (cd)
        *w = cd->origin_x + cd->pixel_width - *x;
    else
        *w = block->style->dimensions->width - *x;
}


static void
gnucash_cursor_request_redraw (GnucashCursor *cursor)
{
    gtk_widget_queue_draw_area (GTK_WIDGET(cursor),
                                cursor->x, cursor->y, cursor->w, cursor->h);
}


void
gnucash_cursor_set_style (GnucashCursor  *cursor, SheetBlockStyle *style)
{
    g_return_if_fail (cursor != NULL);
    g_return_if_fail (GNUCASH_IS_CURSOR(cursor));

    cursor->style = style;
}


void
gnucash_cursor_get_virt (GnucashCursor *cursor, VirtualLocation *virt_loc)
{
    g_return_if_fail (cursor != NULL);
    g_return_if_fail (GNUCASH_IS_CURSOR (cursor));

    virt_loc->vcell_loc.virt_row = cursor->row;
    virt_loc->vcell_loc.virt_col = cursor->col;

    virt_loc->phys_row_offset = cursor->cell.row;
    virt_loc->phys_col_offset = cursor->cell.col;
}

static void
gnucash_cursor_draw_offscreen (GnucashCursor *cursor)
{
    GnucashCursorCell *cc = &(cursor->cell);
    gint cx, cy;
    cairo_rgb fg_color;
    cairo_t *cr;

    if (cursor->surface)
        cairo_surface_destroy (cursor->surface);
    cursor->surface = cairo_image_surface_create (CAIRO_FORMAT_ARGB32,
                                                  cursor->w, cursor->h);

    cr = cairo_create (cursor->surface);
    to_cairo_rgb(&gn_black, &fg_color);

    /* Clear the background area.
     * FIXME doesn't work as written */
    cairo_save (cr);
    cairo_set_operator (cr, CAIRO_OPERATOR_CLEAR);
    cairo_paint (cr);
    cairo_restore (cr);

    /* draw the rectangle around the entire active
     virtual *row */
    cairo_rectangle (cr, 0.5, 0.5, cursor->w - 1.0, cursor->h - 1.0);
    cairo_move_to (cr, 0, cursor->h - 1.5);
    cairo_rel_line_to (cr, cursor->w, 0);
    cairo_set_source_rgb (cr, fg_color.red, fg_color.green, fg_color.blue);
    cairo_set_line_width (cr, 1.0);
    cairo_stroke (cr);


    // Draw the cursor's cell frame
    cx = cc->x - cursor->x;
    cy = cc->y - cursor->y;

    cairo_rectangle (cr, cx + 0.5, cy + 0.5, cc->w - 1.0, cc->h - 1.0);
    cairo_set_source_rgb (cr, fg_color.red, fg_color.green, fg_color.blue);
    cairo_set_line_width (cr, 1.0);
    cairo_stroke (cr);

    cairo_destroy (cr);
}


void
gnucash_cursor_configure (GnucashCursor *cursor)
{
    GtkLayout *layout;
    gint x, y, w, h;

    g_return_if_fail (cursor != NULL);
    g_return_if_fail (GNUCASH_IS_CURSOR (cursor));

    if (!cursor->sheet)
        return;

    g_return_if_fail (GTK_IS_LAYOUT (cursor->sheet));
    layout = GTK_LAYOUT(cursor->sheet);

    gnucash_cursor_get_pixel_coords (cursor, &x, &y, &w, &h);
    cursor->x = x;
    cursor->y = y;
    cursor->w = w;
    cursor->h = h + 2;

    gtk_layout_move (layout, GTK_WIDGET(cursor), cursor->x, cursor->y);
    gtk_widget_set_size_request(GTK_WIDGET(cursor), cursor->w, cursor->h);

    gnucash_sheet_style_get_cell_pixel_rel_coords (cursor->style,
            cursor->cell.row,  cursor->cell.col,
            &x, &y, &w, &h);
    cursor->cell.x = x - cursor->x;
    cursor->cell.y = y;
    cursor->cell.w = w;
    cursor->cell.h = h;

    gnucash_cursor_draw_offscreen (cursor);
}


static gboolean
gnucash_cursor_draw (GtkWidget *widget, GdkEventExpose *event)
{
    cairo_t *cr;
    GdkWindow *binwin = gtk_widget_get_window(widget);

    cr = gdk_cairo_create (binwin);
    cairo_set_source_surface (cr, GNUCASH_CURSOR(widget)->surface, 0, 0);
    cairo_paint (cr);
    cairo_destroy (cr);

    return TRUE;
}


static void
gnucash_cursor_set_block (GnucashCursor *cursor, VirtualCellLocation vcell_loc)
{
    GnucashSheet *sheet;

    g_return_if_fail (cursor != NULL);
    g_return_if_fail (GNUCASH_IS_CURSOR (cursor));

    sheet = cursor->sheet;

    if (vcell_loc.virt_row < 0 ||
            vcell_loc.virt_row >= sheet->num_virt_rows ||
            vcell_loc.virt_col < 0 ||
            vcell_loc.virt_col >= sheet->num_virt_cols)
        return;

    cursor->style = gnucash_sheet_get_style (sheet, vcell_loc);
    cursor->row = vcell_loc.virt_row;
    cursor->col = vcell_loc.virt_col;
}


static void
gnucash_cursor_set_cell (GnucashCursor *cursor, gint cell_row, gint cell_col)
{
    SheetBlockStyle *style;

    g_return_if_fail (cursor != NULL);
    g_return_if_fail (GNUCASH_IS_CURSOR (cursor));

    style = cursor->style;

    if (cell_row < 0 || cell_row >= style->nrows ||
            cell_col < 0 || cell_col >= style->ncols)
        return;

    cursor->cell.row = cell_row;
    cursor->cell.col = cell_col;
}


void
gnucash_cursor_set (GnucashCursor *cursor, VirtualLocation virt_loc)
{
    GnucashSheet *sheet;

    g_return_if_fail (cursor != NULL);
    g_return_if_fail (GNUCASH_IS_CURSOR (cursor));

    sheet = cursor->sheet;

    gtk_widget_queue_draw_area(GTK_WIDGET(sheet->grid), cursor->x, cursor->y,
        cursor->w, cursor->h);

    gnucash_cursor_set_block (cursor, virt_loc.vcell_loc);
    gnucash_cursor_set_cell (cursor,
                             virt_loc.phys_row_offset,
                             virt_loc.phys_col_offset);

    gnucash_cursor_configure (cursor);

    g_object_set (G_OBJECT(sheet->header_item),
                  "cursor_name",
                  cursor->style->cursor->cursor_name,
                  NULL);

    gnucash_cursor_request_redraw (cursor);
}


static void
gnucash_cursor_realize (GtkWidget *widget)
{
    GdkWindow *window;

    if (GTK_WIDGET_CLASS (gnucash_cursor_parent_class)->realize)
        (GTK_WIDGET_CLASS
         (gnucash_cursor_parent_class)->realize)(widget);

    window = gtk_widget_get_window (widget);
    gdk_window_set_back_pixmap (window, NULL, FALSE);
    gnucash_cursor_draw_offscreen(GNUCASH_CURSOR(widget));
}


static void
gnucash_cursor_unrealize (GtkWidget *widget)
{
    GnucashCursor *cursor = GNUCASH_CURSOR (widget);

    if (cursor->surface)
        cairo_surface_destroy (cursor->surface);
    cursor->surface = NULL;

    if (GTK_WIDGET_CLASS (gnucash_cursor_parent_class)->unrealize)
        (GTK_WIDGET_CLASS
         (gnucash_cursor_parent_class)->unrealize)(widget);
}


static void
gnucash_cursor_set_property (GObject         *object,
                             guint            prop_id,
                             const GValue    *value,
                             GParamSpec      *pspec)
{
    GnucashCursor *cursor;

    cursor = GNUCASH_CURSOR (object);

    switch (prop_id)
    {
    case PROP_SHEET:
        cursor->sheet =
            GNUCASH_SHEET (g_value_get_object (value));
        break;
    default:
        break;
    }
}


/* Note that g_value_set_object() refs the object, as does
 * g_object_get(). But g_object_get() only unrefs once when it disgorges
 * the object, leaving an unbalanced ref, which leaks. So instead of
 * using g_value_set_object(), use g_value_take_object() which doesn't
 * ref the object when used in get_property().
 */
static void
gnucash_cursor_get_property (GObject         *object,
                             guint            prop_id,
                             GValue          *value,
                             GParamSpec      *pspec)
{
    GnucashCursor *cursor = GNUCASH_CURSOR (object);

    switch (prop_id)
    {
    case PROP_SHEET:
        g_value_take_object (value, cursor->sheet);
        break;
    default:
        break;
    }
}


static void
gnucash_cursor_class_init (GnucashCursorClass *klass)
{
    GObjectClass  *object_class;
    GtkWidgetClass *widget_class;

    object_class = G_OBJECT_CLASS (klass);
    widget_class = GTK_WIDGET_CLASS (klass);

    gnucash_cursor_parent_class = g_type_class_peek_parent (klass);

    /* GObject method overrides */
    object_class->set_property = gnucash_cursor_set_property;
    object_class->get_property = gnucash_cursor_get_property;

    /* GtkWidget method overrides */
    widget_class->realize     = gnucash_cursor_realize;
    widget_class->unrealize   = gnucash_cursor_unrealize;
    widget_class->expose_event = gnucash_cursor_draw;

    /* properties */
    g_object_class_install_property
    (object_class,
     PROP_SHEET,
     g_param_spec_object ("sheet",
                          "Sheet Value",
                          "Sheet Value",
                          GNUCASH_TYPE_SHEET,
                          G_PARAM_READWRITE));
}


GType
gnucash_cursor_get_type (void)
{
    static GType gnucash_cursor_type = 0;

    if (!gnucash_cursor_type)
    {
        static const GTypeInfo gnucash_cursor_info =
        {
            sizeof (GnucashCursorClass),
            NULL,		/* base_init */
            NULL,		/* base_finalize */
            (GClassInitFunc) gnucash_cursor_class_init,
            NULL,		/* class_finalize */
            NULL,		/* class_data */
            sizeof (GnucashCursor),
            0,		/* n_preallocs */
            NULL        /* instance intialization */
        };

        gnucash_cursor_type =
            g_type_register_static (GTK_TYPE_DRAWING_AREA,
                                    "GnucashCursor",
                                    &gnucash_cursor_info, 0);
    }

    return gnucash_cursor_type;
}


GtkWidget *
gnucash_cursor_new (GnucashSheet *sheet)
{
    GnucashCursor *cursor;

    cursor = GNUCASH_CURSOR(
        g_object_new (gnucash_cursor_get_type(),
                      "sheet", sheet,
                      NULL));
/* FIXME doesn't seem to work as it should...
    gtk_widget_set_app_paintable(GTK_WIDGET(cursor), TRUE);*/
    gtk_layout_put (GTK_LAYOUT(sheet), GTK_WIDGET(cursor), 0, 0);

    gtk_widget_show (GTK_WIDGET (cursor));

    return GTK_WIDGET (cursor);
}


