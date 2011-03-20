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
#include "gnucash-grid.h"
#include "gnucash-sheet.h"
#include "gnucash-style.h"

static GnomeCanvasItem *gnucash_cursor_parent_class;
static GnomeCanvasItem *gnucash_item_cursor_parent_class;

enum
{
    PROP_0,
    PROP_SHEET,
    PROP_GRID,
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

    item_cursor =
        GNUCASH_ITEM_CURSOR(cursor->cursor[GNUCASH_CURSOR_BLOCK]);

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
    GnomeCanvas *canvas = GNOME_CANVAS_ITEM(cursor)->canvas;
    int x, y, w, h;

    x = cursor->x;
    y = cursor->y;
    w = cursor->w;
    h = cursor->h;

    gnome_canvas_request_redraw (canvas, x, y, x + w + 1, y + h + 1);
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

    virt_loc->vcell_loc.virt_row =
        GNUCASH_ITEM_CURSOR(cursor->cursor[GNUCASH_CURSOR_BLOCK])->row;
    virt_loc->vcell_loc.virt_col =
        GNUCASH_ITEM_CURSOR(cursor->cursor[GNUCASH_CURSOR_BLOCK])->col;

    virt_loc->phys_row_offset =
        GNUCASH_ITEM_CURSOR(cursor->cursor[GNUCASH_CURSOR_CELL])->row;
    virt_loc->phys_col_offset =
        GNUCASH_ITEM_CURSOR(cursor->cursor[GNUCASH_CURSOR_CELL])->col;
}


void
gnucash_cursor_configure (GnucashCursor *cursor)
{
    GnomeCanvasItem *item;
    GnucashItemCursor *block_cursor;
    GnucashItemCursor *cell_cursor;
    GnomeCanvas *canvas;
    gint x, y, w, h;
    double wx, wy;

    g_return_if_fail (cursor != NULL);
    g_return_if_fail (GNUCASH_IS_CURSOR (cursor));

    canvas = GNOME_CANVAS(GNOME_CANVAS_ITEM(cursor)->canvas);

    item = GNOME_CANVAS_ITEM (cursor);

    gnucash_cursor_get_pixel_coords (cursor, &x, &y, &w, &h);
    gnome_canvas_item_set (GNOME_CANVAS_ITEM(cursor),
                           "GnomeCanvasGroup::x", (double)x,
                           "GnomeCanvasGroup::y", (double)y,
                           NULL);

    cursor->w = w;
    cursor->h = h + 1;

    item->x1 = cursor->x = x;
    item->y1 = cursor->y = y;
    item->x2 = x + w;
    item->y2 = y + h + 1;

    item = cursor->cursor[GNUCASH_CURSOR_BLOCK];
    block_cursor = GNUCASH_ITEM_CURSOR (item);

    wx = 0;
    wy = 0;

    gnome_canvas_item_i2w (item, &wx, &wy);
    gnome_canvas_w2c (canvas, wx, wy, &block_cursor->x, &block_cursor->y);
    block_cursor->w = w;
    block_cursor->h = h + 1;

    item->x1 = block_cursor->x;
    item->y1 = block_cursor->y;
    item->x2 = block_cursor->x + w;
    item->y2 = block_cursor->y + h + 1;

    item = cursor->cursor[GNUCASH_CURSOR_CELL];
    cell_cursor = GNUCASH_ITEM_CURSOR(item);

    gnucash_sheet_style_get_cell_pixel_rel_coords (cursor->style,
            cell_cursor->row,
            cell_cursor->col,
            &x, &y, &w, &h);
    wx = x - block_cursor->x;
    wy = y;

    gnome_canvas_item_i2w (item, &wx, &wy);
    gnome_canvas_w2c (canvas, wx, wy, &cell_cursor->x, &cell_cursor->y);
    cell_cursor->w = w;
    cell_cursor->h = h;

    item->x1 = cell_cursor->x;
    item->y1 = cell_cursor->y;
    item->x2 = cell_cursor->x + w;
    item->y2 = cell_cursor->y + h;
}


static void
gnucash_item_cursor_draw (GnomeCanvasItem *item, GdkDrawable *drawable,
                          int x, int y, int width, int height)
{
    GnucashItemCursor *item_cursor = GNUCASH_ITEM_CURSOR (item);
    GnucashCursor *cursor = GNUCASH_CURSOR(item->parent);
    gint dx, dy, dw, dh;

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
    }
}


static void
gnucash_cursor_set_block (GnucashCursor *cursor, VirtualCellLocation vcell_loc)
{
    GnucashSheet *sheet;
    GnucashItemCursor *item_cursor;

    g_return_if_fail (cursor != NULL);
    g_return_if_fail (GNUCASH_IS_CURSOR (cursor));

    sheet = cursor->sheet;
    item_cursor =
        GNUCASH_ITEM_CURSOR(cursor->cursor[GNUCASH_CURSOR_BLOCK]);

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
    GnucashSheet *sheet;
    GnucashItemCursor *item_cursor;
    SheetBlockStyle *style;

    g_return_if_fail (cursor != NULL);
    g_return_if_fail (GNUCASH_IS_CURSOR (cursor));

    sheet = cursor->sheet;
    item_cursor = GNUCASH_ITEM_CURSOR(cursor->cursor[GNUCASH_CURSOR_CELL]);
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

    gnome_canvas_item_set (GNOME_CANVAS_ITEM(sheet->header_item),
                           "cursor_name",
                           cursor->style->cursor->cursor_name,
                           NULL);

    gnucash_cursor_request_redraw (cursor);
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

    if (cursor->gc != NULL)
    {
        g_object_unref (cursor->gc);
        cursor->gc = NULL;
    }

    if (GNOME_CANVAS_ITEM_CLASS (gnucash_cursor_parent_class)->unrealize)
        (*GNOME_CANVAS_ITEM_CLASS
         (gnucash_cursor_parent_class)->unrealize)(item);
}


static void
gnucash_item_cursor_class_init (GnucashItemCursorClass *class)
{
    GObjectClass  *object_class;
    GnomeCanvasItemClass *item_class;

    object_class = G_OBJECT_CLASS (class);
    item_class = GNOME_CANVAS_ITEM_CLASS (class);

    gnucash_item_cursor_parent_class = g_type_class_peek_parent (class);

    /* GnomeCanvasItem method overrides */
    item_class->draw = gnucash_item_cursor_draw;
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
            g_type_register_static (gnome_canvas_item_get_type (),
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
    GnomeCanvasItem *item;
    GnucashCursor *cursor;

    item = GNOME_CANVAS_ITEM (object);
    cursor = GNUCASH_CURSOR (object);

    switch (prop_id)
    {
    case PROP_SHEET:
        cursor->sheet =
            GNUCASH_SHEET (g_value_get_object (value));
        break;
    case PROP_GRID:
        cursor->grid =
            GNUCASH_GRID (g_value_get_object (value));
        break;
    default:
        break;
    }
}


static void
gnucash_cursor_get_property (GObject         *object,
                             guint            prop_id,
                             GValue          *value,
                             GParamSpec      *pspec)
{
    GnomeCanvasItem *item;
    GnucashCursor *cursor;

    item = GNOME_CANVAS_ITEM (object);
    cursor = GNUCASH_CURSOR (object);

    switch (prop_id)
    {
    case PROP_SHEET:
        g_value_set_object (value, cursor->sheet);
        break;
    case PROP_GRID:
        g_value_set_object (value, cursor->grid);
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
gnucash_cursor_class_init (GnucashCursorClass *class)
{
    GObjectClass  *object_class;
    GnomeCanvasItemClass *item_class;

    object_class = G_OBJECT_CLASS (class);
    item_class = GNOME_CANVAS_ITEM_CLASS (class);

    gnucash_cursor_parent_class = g_type_class_peek_parent (class);

    /* GObject method overrides */
    object_class->set_property = gnucash_cursor_set_property;
    object_class->get_property = gnucash_cursor_get_property;

    /* GnomeCanvasItem method overrides */
    item_class->realize     = gnucash_cursor_realize;
    item_class->unrealize   = gnucash_cursor_unrealize;

    /* properties */
    g_object_class_install_property
    (object_class,
     PROP_SHEET,
     g_param_spec_object ("sheet",
                          "Sheet Value",
                          "Sheet Value",
                          GNUCASH_TYPE_SHEET,
                          G_PARAM_READWRITE));
    g_object_class_install_property
    (object_class,
     PROP_GRID,
     g_param_spec_object ("grid",
                          "Grid Value",
                          "Grid Value",
                          GNUCASH_TYPE_GRID,
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
            (GInstanceInitFunc) gnucash_cursor_init
        };

        gnucash_cursor_type =
            g_type_register_static (gnome_canvas_group_get_type (),
                                    "GnucashCursor",
                                    &gnucash_cursor_info, 0);
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
