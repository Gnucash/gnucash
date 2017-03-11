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
static GtkDrawingArea *gnucash_item_cursor_parent_class;

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
    GnucashItemCursor *item_cursor;
    VirtualCellLocation vcell_loc;
    CellDimensions *cd;
    VirtualCell *vcell;
    SheetBlock *block;
    gint col;

    item_cursor = cursor->cursor[GNUCASH_CURSOR_BLOCK];

    vcell_loc.virt_row = item_cursor->row;
    vcell_loc.virt_col = item_cursor->col;

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
    GnucashItemCursor *ic;
    gtk_widget_queue_draw_area (GTK_WIDGET(cursor),
                                cursor->x, cursor->y, cursor->w, cursor->h);
    ic = cursor->cursor[GNUCASH_CURSOR_BLOCK];
    gtk_widget_queue_draw_area (GTK_WIDGET(ic), ic->x, ic->y, ic->w, ic->h);
    ic = cursor->cursor[GNUCASH_CURSOR_CELL];
    gtk_widget_queue_draw_area (GTK_WIDGET(ic), ic->x, ic->y, ic->w, ic->h);
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

    virt_loc->vcell_loc.virt_row = cursor->cursor[GNUCASH_CURSOR_BLOCK]->row;
    virt_loc->vcell_loc.virt_col = cursor->cursor[GNUCASH_CURSOR_BLOCK]->col;

    virt_loc->phys_row_offset = cursor->cursor[GNUCASH_CURSOR_CELL]->row;
    virt_loc->phys_col_offset = cursor->cursor[GNUCASH_CURSOR_CELL]->col;
}


void
gnucash_cursor_configure (GnucashCursor *cursor)
{
    GnucashItemCursor *block_cursor;
    GnucashItemCursor *cell_cursor;
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
    cursor->h = h + 1;

    gtk_layout_move (layout, GTK_WIDGET(cursor), cursor->x, cursor->y);
    gtk_widget_set_size_request(GTK_WIDGET(cursor), cursor->w, cursor->h);

    block_cursor = cursor->cursor[GNUCASH_CURSOR_BLOCK];
    block_cursor->x = cursor->x;
    block_cursor->y = cursor->y;
    block_cursor->w = cursor->w;
    block_cursor->h = cursor->h;

    gtk_layout_move (layout, GTK_WIDGET(block_cursor),
                     block_cursor->x, block_cursor->y);
    gtk_widget_set_size_request(GTK_WIDGET(block_cursor),
                                block_cursor->w, block_cursor->h);

    cell_cursor = cursor->cursor[GNUCASH_CURSOR_CELL];
    gnucash_sheet_style_get_cell_pixel_rel_coords (cursor->style,
            cell_cursor->row,  cell_cursor->col,
            &x, &y, &w, &h);
    cell_cursor->x = x - block_cursor->x;
    cell_cursor->y = y;
    cell_cursor->w = w;
    cell_cursor->h = h;

    gtk_layout_move (layout, GTK_WIDGET(cell_cursor),
                     cell_cursor->x, cell_cursor->y);
    gtk_widget_set_size_request(GTK_WIDGET(cell_cursor),
                                cell_cursor->w, cell_cursor->h);
}


static gboolean
gnucash_item_cursor_draw (GtkWidget *widget, GdkEventExpose *event)
{
    GnucashItemCursor *item_cursor = GNUCASH_ITEM_CURSOR(widget);
    GnucashCursor *cursor = GNUCASH_CURSOR(item_cursor->parent);
    gint dx, dy, dw, dh;
    int x = event->area.x;
    int y = event->area.y;
    GdkDrawable *drawable = GDK_DRAWABLE (gtk_layout_get_bin_window (GTK_LAYOUT(widget)));

    switch (item_cursor->type)
    {
    case GNUCASH_CURSOR_BLOCK:
        dx = item_cursor->x - x;
        dy = item_cursor->y - y;
        dw = item_cursor->w;
        dh = item_cursor->h;

        /* draw the rectangle around the entire active
           virtual row */
        gdk_gc_set_line_attributes (cursor->gc, 1,
                                    GDK_LINE_SOLID, GDK_CAP_NOT_LAST, GDK_JOIN_MITER);

        gdk_gc_set_foreground (cursor->gc, &gn_black);

        gdk_draw_rectangle (drawable, cursor->gc, FALSE,
                            dx, dy, dw, dh - 1);
        gdk_draw_line (drawable, cursor->gc,
                       dx, dy + dh, dx + dw, dy + dh);

        break;

    case GNUCASH_CURSOR_CELL:
        dx = item_cursor->x - x;
        dy = item_cursor->y - y;
        dw = item_cursor->w;
        dh = item_cursor->h;

        gdk_gc_set_line_attributes (cursor->gc, 1,
                                    GDK_LINE_SOLID, GDK_CAP_NOT_LAST, GDK_JOIN_MITER);

        gdk_gc_set_foreground (cursor->gc, &gn_black);

        gdk_draw_rectangle (drawable, cursor->gc, FALSE,
                            dx, dy, dw, dh);
        break;
    }
    return TRUE;
}


static void
gnucash_cursor_set_block (GnucashCursor *cursor, VirtualCellLocation vcell_loc)
{
    GnucashSheet *sheet;
    GnucashItemCursor *item_cursor;

    g_return_if_fail (cursor != NULL);
    g_return_if_fail (GNUCASH_IS_CURSOR (cursor));

    sheet = cursor->sheet;
    item_cursor = cursor->cursor[GNUCASH_CURSOR_BLOCK];

    if (vcell_loc.virt_row < 0 ||
            vcell_loc.virt_row >= sheet->num_virt_rows ||
            vcell_loc.virt_col < 0 ||
            vcell_loc.virt_col >= sheet->num_virt_cols)
        return;

    cursor->style = gnucash_sheet_get_style (sheet, vcell_loc);

    item_cursor->row = vcell_loc.virt_row;
    item_cursor->col = vcell_loc.virt_col;
}


static void
gnucash_cursor_set_cell (GnucashCursor *cursor, gint cell_row, gint cell_col)
{
    GnucashItemCursor *item_cursor;
    SheetBlockStyle *style;

    g_return_if_fail (cursor != NULL);
    g_return_if_fail (GNUCASH_IS_CURSOR (cursor));

    item_cursor = cursor->cursor[GNUCASH_CURSOR_CELL];
    style = cursor->style;

    if (cell_row < 0 || cell_row >= style->nrows ||
            cell_col < 0 || cell_col >= style->ncols)
        return;

    item_cursor->row = cell_row;
    item_cursor->col = cell_col;
}


void
gnucash_cursor_set (GnucashCursor *cursor, VirtualLocation virt_loc)
{
    GnucashSheet *sheet;

    g_return_if_fail (cursor != NULL);
    g_return_if_fail (GNUCASH_IS_CURSOR (cursor));

    sheet = cursor->sheet;

    gnucash_cursor_request_redraw (cursor);

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
gnucash_item_cursor_init (GnucashItemCursor *cursor)
{
    cursor->col = 0;
    cursor->row   = 0;
}


static void
gnucash_cursor_realize (GtkWidget *widget)
{
    GnucashCursor *cursor = GNUCASH_CURSOR (widget);
    GdkWindow *window;

    if (GTK_WIDGET_CLASS (gnucash_cursor_parent_class)->realize)
        (GTK_WIDGET_CLASS
         (gnucash_cursor_parent_class)->realize)(widget);

    window = gtk_widget_get_window (widget);

    cursor->gc = gdk_gc_new (window);
}


static void
gnucash_cursor_unrealize (GtkWidget *widget)
{
    GnucashCursor *cursor = GNUCASH_CURSOR (widget);

    if (cursor->gc != NULL)
    {
        g_object_unref (cursor->gc);
        cursor->gc = NULL;
    }

    if (GTK_WIDGET_CLASS (gnucash_cursor_parent_class)->unrealize)
        (GTK_WIDGET_CLASS
         (gnucash_cursor_parent_class)->unrealize)(widget);
}


static void
gnucash_item_cursor_class_init (GnucashItemCursorClass *klass)
{
    GtkWidgetClass *widget_class;

    widget_class = GTK_WIDGET_CLASS (klass);

    gnucash_item_cursor_parent_class = g_type_class_peek_parent (klass);

    /* Widget method overrides */
    widget_class->expose_event = gnucash_item_cursor_draw;
}


GType
gnucash_item_cursor_get_type (void)
{
    static GType gnucash_item_cursor_type = 0;

    if (!gnucash_item_cursor_type)
    {
        static const GTypeInfo gnucash_item_cursor_info =
        {
            sizeof (GnucashItemCursorClass),
            NULL,		/* base_init */
            NULL,		/* base_finalize */
            (GClassInitFunc) gnucash_item_cursor_class_init,
            NULL,		/* class_finalize */
            NULL,		/* class_data */
            sizeof (GnucashItemCursor),
            0,		/* n_preallocs */
            (GInstanceInitFunc) gnucash_item_cursor_init
        };

        gnucash_item_cursor_type =
            g_type_register_static (GTK_TYPE_DRAWING_AREA,
                                    "GnucashItemCursor",
                                    &gnucash_item_cursor_info, 0);
    }

    return gnucash_item_cursor_type;
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
    GnucashItemCursor *item_cursor;

    cursor = GNUCASH_CURSOR(
        g_object_new (gnucash_cursor_get_type(),
                      "sheet", sheet,
                      NULL));
    gtk_layout_put (GTK_LAYOUT(sheet), GTK_WIDGET(cursor), 0, 0);

    item_cursor = GNUCASH_ITEM_CURSOR (
        g_object_new (gnucash_item_cursor_get_type(), NULL));
    item_cursor->type = GNUCASH_CURSOR_CELL;
    item_cursor->parent = GTK_WIDGET(cursor);
    gtk_layout_put (GTK_LAYOUT(sheet), GTK_WIDGET(item_cursor), 0, 0);
    cursor->cursor[GNUCASH_CURSOR_CELL] = item_cursor;


    item_cursor = GNUCASH_ITEM_CURSOR (
        g_object_new (gnucash_item_cursor_get_type(), NULL));
    item_cursor->type = GNUCASH_CURSOR_BLOCK;
    item_cursor->parent = GTK_WIDGET(cursor);
    gtk_layout_put (GTK_LAYOUT(sheet), GTK_WIDGET(item_cursor), 0, 0);
    cursor->cursor[GNUCASH_CURSOR_BLOCK] = item_cursor;

    return GTK_WIDGET (cursor);
}


