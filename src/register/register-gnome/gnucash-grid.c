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
 * The Gnucash Grid Canvas Item
 *
 *  Based heavily (i.e., cut and pasted from) on the Gnumeric ItemGrid.
 *
 * Author:
 *     Heath Martin <martinh@pegasus.cc.ucf.edu>
 */

#include "config.h"

#include <string.h>
#include <libgnomecanvas/libgnomecanvas.h>

#include "gnucash-sheet.h"
#include "gnucash-sheetP.h"
#include "gnucash-grid.h"
#include "gnucash-color.h"
#include "gnucash-style.h"


struct _GnucashGrid
{
    GnomeCanvasItem canvas_item;

    GnucashSheet *sheet;

    /* The first and last displayed block */
    int        top_block;
    int        bottom_block;

    /* Offset from spreadsheet origin in units */
    long       top_offset;
    long       left_offset;

    GdkGC      *grid_gc;    /* Draw grid gc */
    GdkGC      *fill_gc;    /* Default background fill gc */
    GdkGC      *gc;     /* Color used for the cell */

    GdkColor   background;
    GdkColor   grid_color;
    GdkColor   default_color;
};


struct _GnucashGridClass
{
    GnomeCanvasItemClass parent_class;
};

static GnomeCanvasItem *gnucash_grid_parent_class;

/* Our arguments */
enum
{
    PROP_0,
    PROP_SHEET
};


static void
gnucash_grid_realize (GnomeCanvasItem *item)
{
    GdkWindow *window;
    GnucashGrid *gnucash_grid;
    GdkGC *gc;

    if (GNOME_CANVAS_ITEM_CLASS (gnucash_grid_parent_class)->realize)
        (GNOME_CANVAS_ITEM_CLASS
         (gnucash_grid_parent_class)->realize)(item);

    gnucash_grid = GNUCASH_GRID (item);
    window = GTK_WIDGET (item->canvas)->window;

    /* Configure the default grid gc */
    gnucash_grid->grid_gc = gc = gdk_gc_new (window);
    gnucash_grid->fill_gc = gdk_gc_new (window);
    gnucash_grid->gc = gdk_gc_new (window);

    /* Allocate the default colors */
    gnucash_grid->background = gn_white;
    gnucash_grid->grid_color = gn_black;
    gnucash_grid->default_color = gn_black;

    gdk_gc_set_foreground (gc, &gnucash_grid->grid_color);
    gdk_gc_set_background (gc, &gnucash_grid->background);

    gdk_gc_set_foreground (gnucash_grid->fill_gc,
                           &gnucash_grid->background);
    gdk_gc_set_background (gnucash_grid->fill_gc,
                           &gnucash_grid->grid_color);
}


static void
gnucash_grid_unrealize (GnomeCanvasItem *item)
{
    GnucashGrid *gnucash_grid = GNUCASH_GRID (item);

    if (gnucash_grid->grid_gc != NULL)
    {
        g_object_unref(gnucash_grid->grid_gc);
        gnucash_grid->grid_gc = NULL;
    }

    if (gnucash_grid->fill_gc != NULL)
    {
        g_object_unref(gnucash_grid->fill_gc);
        gnucash_grid->fill_gc = NULL;
    }

    if (gnucash_grid->gc != NULL)
    {
        g_object_unref(gnucash_grid->gc);
        gnucash_grid->gc = NULL;
    }

    if (GNOME_CANVAS_ITEM_CLASS (gnucash_grid_parent_class)->unrealize)
        (*GNOME_CANVAS_ITEM_CLASS
         (gnucash_grid_parent_class)->unrealize)(item);
}


static void
gnucash_grid_update (GnomeCanvasItem *item, double *affine,
                     ArtSVP *clip_path, int flags)
{
    if (GNOME_CANVAS_ITEM_CLASS (gnucash_grid_parent_class)->update)
        (* GNOME_CANVAS_ITEM_CLASS (gnucash_grid_parent_class)->update)
        (item, affine, clip_path, flags);

    item->x1 = 0;
    item->y1 = 0;
    item->x2 = INT_MAX / 2 - 1;
    item->y2 = INT_MAX / 2 - 1;
}


/*
 * Sets virt_row, virt_col to the block coordinates for the
 * block containing pixel (x, y).  Also sets o_x, o_y, to the
 * pixel coordinates of the origin of the block.  Returns
 * TRUE if a block is found, FALSE if (x,y) is not
 * in any block.
 *
 * All coordinates are with respect to the canvas origin.
 */
static SheetBlock *
gnucash_grid_find_block_by_pixel (GnucashGrid *grid,
                                  gint x, gint y,
                                  VirtualCellLocation *vcell_loc)
{
    SheetBlock *block;
    VirtualCellLocation vc_loc = { 1, 0 };

    g_return_val_if_fail(y >= 0, NULL);
    g_return_val_if_fail(x >= 0, NULL);

    do
    {
        block = gnucash_sheet_get_block (grid->sheet, vc_loc);
        if (!block)
            return NULL;

        if (block->visible &&
                y >= block->origin_y &&
                y < block->origin_y + block->style->dimensions->height)
        {
            if (vcell_loc)
                vcell_loc->virt_row = vc_loc.virt_row;
            break;
        }
        vc_loc.virt_row++;
    }
    while (vc_loc.virt_row < grid->sheet->num_virt_rows);

    if (vc_loc.virt_row == grid->sheet->num_virt_rows)
        return NULL;

    do
    {
        block = gnucash_sheet_get_block (grid->sheet, vc_loc);
        if (!block)
            return NULL;

        if (block->visible &&
                x >= block->origin_x &&
                x < block->origin_x + block->style->dimensions->width)
        {
            if (vcell_loc)
                vcell_loc->virt_col = vc_loc.virt_col;
            break;
        }
        vc_loc.virt_col++;
    }
    while (vc_loc.virt_col < grid->sheet->num_virt_cols);

    if (vc_loc.virt_col == grid->sheet->num_virt_cols)
        return NULL;

    return block;
}

static gboolean
gnucash_grid_find_cell_by_pixel (GnucashGrid *grid, gint x, gint y,
                                 VirtualLocation *virt_loc)
{
    SheetBlock *block;
    SheetBlockStyle *style;
    CellDimensions *cd;
    gint row = 0;
    gint col = 0;

    g_return_val_if_fail (virt_loc != NULL, FALSE);

    block = gnucash_sheet_get_block (grid->sheet, virt_loc->vcell_loc);
    if (block == NULL)
        return FALSE;

    /* now make x, y relative to the block origin */
    x -= block->origin_x;
    y -= block->origin_y;

    style = block->style;
    if (style == NULL)
        return FALSE;

    do
    {
        cd = gnucash_style_get_cell_dimensions (style, row, 0);

        if (y >= cd->origin_y && y < cd->origin_y + cd->pixel_height)
            break;

        row++;
    }
    while (row < style->nrows);

    if (row == style->nrows)
        return FALSE;

    do
    {
        cd = gnucash_style_get_cell_dimensions (style, row, col);

        if (x >= cd->origin_x && x < cd->origin_x + cd->pixel_width)
            break;

        col++;
    }
    while (col < style->ncols);

    if (col == style->ncols)
        return FALSE;

    if (virt_loc)
        virt_loc->phys_row_offset = row;
    if (virt_loc)
        virt_loc->phys_col_offset = col;

    return TRUE;
}

gboolean
gnucash_grid_find_loc_by_pixel (GnucashGrid *grid, gint x, gint y,
                                VirtualLocation *virt_loc)
{
    SheetBlock *block;

    if (virt_loc == NULL)
        return FALSE;

    block = gnucash_grid_find_block_by_pixel (grid, x, y,
            &virt_loc->vcell_loc);
    if (block == NULL)
        return FALSE;

    return gnucash_grid_find_cell_by_pixel (grid, x, y, virt_loc);
}

G_INLINE_FUNC void
draw_cell_line (GdkDrawable *drawable,
                GdkGC *gc, GdkColor *bg_color,
                int x1, int y1, int x2, int y2,
                PhysicalCellBorderLineStyle style);

void
draw_cell_line (GdkDrawable *drawable,
                GdkGC *gc, GdkColor *bg_color,
                int x1, int y1, int x2, int y2,
                PhysicalCellBorderLineStyle style)
{
    GdkColor *fg_color;

    switch (style)
    {
    case CELL_BORDER_LINE_NONE:
        fg_color = bg_color;
        break;

    case CELL_BORDER_LINE_LIGHT:
        fg_color = &gn_light_gray;
        break;

    case CELL_BORDER_LINE_NORMAL:
    case CELL_BORDER_LINE_HEAVY:
        fg_color = &gn_black;
        break;

    case CELL_BORDER_LINE_HIGHLIGHT:
        fg_color = &gn_red;
        break;

    default:
        return;
    }

    gdk_gc_set_foreground (gc, fg_color);
    gdk_draw_line (drawable, gc, x1, y1, x2, y2);
}

static void
get_cell_borders (GnucashSheet *sheet, VirtualLocation virt_loc,
                  PhysicalCellBorders *borders)
{
    VirtualLocation v_loc;
    PhysicalCellBorders neighbor;

    gnucash_sheet_get_borders (sheet, virt_loc, borders);

    /* top */
    v_loc = virt_loc;
    if (gnc_table_move_vertical_position (sheet->table, &v_loc, -1))
    {
        gnucash_sheet_get_borders (sheet, v_loc, &neighbor);
        borders->top = MAX (borders->top, neighbor.bottom);
    }

    /* bottom */
    v_loc = virt_loc;
    if (gnc_table_move_vertical_position (sheet->table, &v_loc, 1))
    {
        gnucash_sheet_get_borders (sheet, v_loc, &neighbor);
        borders->bottom = MAX (borders->bottom, neighbor.top);
    }

    /* left */
    v_loc = virt_loc;
    v_loc.phys_col_offset--;
    if (gnc_table_virtual_loc_valid (sheet->table, v_loc, TRUE))
    {
        gnucash_sheet_get_borders (sheet, v_loc, &neighbor);
        borders->left = MAX (borders->left, neighbor.right);
    }

    /* right */
    v_loc = virt_loc;
    v_loc.phys_col_offset++;
    if (gnc_table_virtual_loc_valid (sheet->table, v_loc, TRUE))
    {
        gnucash_sheet_get_borders (sheet, v_loc, &neighbor);
        borders->right = MAX (borders->right, neighbor.left);
    }
}

void
gnucash_draw_hatching (GdkDrawable *drawable, GdkGC *gc,
                       int x, int y, int width, int height)
{
    gdk_gc_set_foreground (gc, &gn_light_gray);

    gdk_draw_rectangle (drawable, gc, FALSE,
                        x + 2, y + 2, height / 3, height / 3);

    gdk_draw_line (drawable, gc,
                   x + 2, y + 2 + height / 3, x + 2 + height / 3, y + 2);

    gdk_draw_line (drawable, gc,
                   x + 2, y + 2, x + 2 + height / 3, y + 2 + height / 3);
}

#ifdef READONLY_LINES_WITH_CHANGED_FG_COLOR
/** For a given byte value, multiply the difference to 0xFF by a rational number,
specified by numerator and denominator. This is some simple integer arithmetics
for the case when we don't even need a conversion to floating point and
backwards. */
static guint8 inc_intensity_byte(guint8 input, int numerator, int denominator)
{
    guint8 result_inv, result;
    guint8 input_inv = 0xff - input;
    result_inv = (input_inv * numerator) / denominator;
    result = 0xff - result_inv;
    return result;
}

/** For a given RGB value, increase the color intensity for each of the three
colors indentically by 10 percent (i.e. make them "less black" and "more gray")
and return this changed RGB value. */
static guint32 inc_intensity_10percent(guint32 argb)
{
    guint32 result =
            (inc_intensity_byte((argb & 0x00FF0000) >> 16, 8, 10) << 16)
            + (inc_intensity_byte((argb & 0x0000FF00) >> 8, 8, 10) << 8)
            + (inc_intensity_byte(argb & 0x000000FF, 8, 10));
    return result;
}
#endif

/** For a given byte value, multiply the value by a rational number,
specified by numerator and denominator. This is some simple integer arithmetics
for the case when we don't even need a conversion to floating point and
backwards. */
static guint8 dec_intensity_byte(guint8 input, int numerator, int denominator)
{
    guint8 result;
    result = (input * numerator) / denominator;
    return result;
}

/** For a given RGB value, decrease the color intensity for each of the three
colors indentically by 10 percent and return this changed RGB value. */
static guint32 dec_intensity_10percent(guint32 argb)
{
    // Multiply each single byte by 9/10 i.e. by 0.9 which decreases the
    // intensity by 10 percent.
    guint32 result =
            (dec_intensity_byte((argb & 0x00FF0000) >> 16, 9, 10) << 16)
            + (dec_intensity_byte((argb & 0x0000FF00) >> 8, 9, 10) << 8)
            + (dec_intensity_byte(argb & 0x000000FF, 9, 10));
    return result;
}

static void
draw_cell (GnucashGrid *grid,
           SheetBlock *block,
           VirtualLocation virt_loc,
           GdkDrawable *drawable,
           int x, int y, int width, int height)
{
    Table *table = grid->sheet->table;
    PhysicalCellBorders borders;
    const char *text;
    PangoLayout *layout;
    PangoContext *context;
    PangoFontDescription *font;
    PangoRectangle logical_rect;
    GdkColor *bg_color;
    GdkColor *fg_color;
    /*        gint x_offset, y_offset;*/
    GdkRectangle rect;
    gboolean hatching;
    guint32 argb, color_type;
    int x_offset;

    gdk_gc_set_background (grid->gc, &gn_white);

    if (grid->sheet->use_theme_colors)
    {
        color_type = gnc_table_get_gtkrc_bg_color (table, virt_loc,
                     &hatching);
        bg_color = get_gtkrc_color(grid->sheet, color_type);
    }
    else
    {
        argb = gnc_table_get_bg_color (table, virt_loc, &hatching);
        // Are we in a read-only row? Then make the background color somewhat more gray.
        if ((virt_loc.phys_row_offset == (block->style->nrows - 1))
                && (table->model->dividing_row_upper >= 0)
                && (virt_loc.vcell_loc.virt_row < table->model->dividing_row_upper))
        {
            argb = dec_intensity_10percent(argb);
        }
        bg_color = gnucash_color_argb_to_gdk (argb);
    }

    gdk_gc_set_foreground (grid->gc, bg_color);
    gdk_draw_rectangle (drawable, grid->gc, TRUE,
                        x + 1, y + 1, width - 1, height - 1);

    get_cell_borders (grid->sheet, virt_loc, &borders);

    /* top */
    draw_cell_line (drawable, grid->gc, bg_color,
                    borders.top >= borders.left ? x : x + 1,
                    y,
                    (borders.top >= borders.right ?
                     x + width : x + width - 1),
                    y,
                    borders.top);

    /* bottom */
    draw_cell_line (drawable, grid->gc, bg_color,
                    borders.bottom >= borders.left ? x : x + 1,
                    y + height,
                    (borders.bottom >= borders.right ?
                     x + width : x + width - 1),
                    y + height,
                    borders.bottom);

    /* left */
    draw_cell_line (drawable, grid->gc, bg_color,
                    x,
                    borders.left > borders.top ? y : y + 1,
                    x,
                    (borders.left > borders.bottom ?
                     y + height : y + height - 1),
                    borders.left);

    /* right */
    draw_cell_line (drawable, grid->gc, bg_color,
                    x + width,
                    borders.right > borders.top ? y : y + 1,
                    x + width,
                    (borders.right > borders.bottom ?
                     y + height : y + height - 1),
                    borders.right);

    if (hatching)
        gnucash_draw_hatching (drawable, grid->gc,
                               x, y, width, height);

    /* dividing line upper (red) */
    if ((virt_loc.phys_row_offset == 0) &&
            (table->model->dividing_row_upper >= 0))
    {
        if (virt_loc.vcell_loc.virt_row == table->model->dividing_row_upper)
        {
            gdk_gc_set_foreground (grid->gc, &gn_red);
            gdk_draw_line (drawable, grid->gc, x, y - 1, x + width, y - 1);
            gdk_draw_line (drawable, grid->gc, x, y,   x + width, y);
            gdk_draw_line (drawable, grid->gc, x, y + 1, x + width, y + 1);
        }
    }

    if ((virt_loc.phys_row_offset == (block->style->nrows - 1)) &&
            (table->model->dividing_row_upper >= 0))
    {
        if (virt_loc.vcell_loc.virt_row ==
                (table->model->dividing_row_upper - 1))
        {
            gdk_gc_set_foreground (grid->gc, &gn_red);
            gdk_draw_line (drawable, grid->gc, x, y + height - 1,
                           x + width, y + height - 1);
            gdk_draw_line (drawable, grid->gc, x, y + height,
                           x + width, y + height);
            gdk_draw_line (drawable, grid->gc, x, y + height + 1,
                           x + width, y + height + 1);
        }
    }

    /* dividing line (blue) */
    if ((virt_loc.phys_row_offset == 0) &&
            (table->model->dividing_row >= 0))
    {
        if (virt_loc.vcell_loc.virt_row == table->model->dividing_row)
        {
            gdk_gc_set_foreground (grid->gc, &gn_blue);
            gdk_draw_line (drawable, grid->gc, x, y - 1, x + width, y - 1);
            gdk_draw_line (drawable, grid->gc, x, y,   x + width, y);
            gdk_draw_line (drawable, grid->gc, x, y + 1, x + width, y + 1);
        }
    }

    if ((virt_loc.phys_row_offset == (block->style->nrows - 1)) &&
            (table->model->dividing_row >= 0))
    {
        if (virt_loc.vcell_loc.virt_row ==
                (table->model->dividing_row - 1))
        {
            gdk_gc_set_foreground (grid->gc, &gn_blue);
            gdk_draw_line (drawable, grid->gc, x, y + height - 1,
                           x + width, y + height - 1);
            gdk_draw_line (drawable, grid->gc, x, y + height,
                           x + width, y + height);
            gdk_draw_line (drawable, grid->gc, x, y + height + 1,
                           x + width, y + height + 1);
        }
    }

    /* dividing line lower (blue) */
    if ((virt_loc.phys_row_offset == 0) &&
            (table->model->dividing_row_lower >= 0))
    {
        if (virt_loc.vcell_loc.virt_row == table->model->dividing_row_lower)
        {
            gdk_gc_set_foreground (grid->gc, &gn_blue);
            gdk_draw_line (drawable, grid->gc, x, y - 1, x + width, y - 1);
            gdk_draw_line (drawable, grid->gc, x, y,   x + width, y);
            gdk_draw_line (drawable, grid->gc, x, y + 1, x + width, y + 1);
        }
    }

    if ((virt_loc.phys_row_offset == (block->style->nrows - 1)) &&
            (table->model->dividing_row_lower >= 0))
    {
        if (virt_loc.vcell_loc.virt_row ==
                (table->model->dividing_row_lower - 1))
        {
            gdk_gc_set_foreground (grid->gc, &gn_blue);
            gdk_draw_line (drawable, grid->gc, x, y + height - 1,
                           x + width, y + height - 1);
            gdk_draw_line (drawable, grid->gc, x, y + height,
                           x + width, y + height);
            gdk_draw_line (drawable, grid->gc, x, y + height + 1,
                           x + width, y + height + 1);
        }
    }

    text = gnc_table_get_entry (table, virt_loc);

    layout = gtk_widget_create_pango_layout (GTK_WIDGET (grid->sheet), text);
    // We don't need word wrap or line wrap
    pango_layout_set_width (layout, -1);
    context = pango_layout_get_context (layout);
    font = pango_font_description_copy (pango_context_get_font_description (context));

    if (grid->sheet->use_theme_colors)
    {
        color_type = gnc_table_get_gtkrc_fg_color (table, virt_loc);
        fg_color = get_gtkrc_color(grid->sheet, color_type);
    }
    else
    {
        argb = gnc_table_get_fg_color (table, virt_loc);
#ifdef READONLY_LINES_WITH_CHANGED_FG_COLOR
        // Are we in a read-only row? Then make the foreground color somewhat less black
        if ((virt_loc.phys_row_offset == (block->style->nrows - 1))
                && (table->model->dividing_row_upper >= 0)
                && (virt_loc.vcell_loc.virt_row < table->model->dividing_row_upper))
        {
            argb = inc_intensity_10percent(argb);
        }
#endif
        fg_color = gnucash_color_argb_to_gdk (argb);
    }

    gdk_gc_set_foreground (grid->gc, fg_color);

    /* If this is the currently open transaction and
       there is no text in this cell */
    if ((table->current_cursor_loc.vcell_loc.virt_row ==
            virt_loc.vcell_loc.virt_row) &&
            (!text || strlen(text) == 0))
    {
        text = gnc_table_get_label (table, virt_loc);
        if ((text == NULL) || (*text == '\0'))
            goto exit;
        gdk_gc_set_foreground (grid->gc, &gn_light_gray);
        pango_layout_set_text (layout, text, strlen (text));
        pango_font_description_set_style (font, PANGO_STYLE_ITALIC);
        pango_context_set_font_description (context, font);
    }

    if ((text == NULL) || (*text == '\0'))
    {
        goto exit;
    }

    /*y_offset = ((height / 2) +
                (((font->ascent + font->descent) / 2) - font->descent));
    y_offset++;*/

    pango_layout_get_pixel_extents(layout,
                                   NULL,
                                   &logical_rect);

    rect.x      = x + CELL_HPADDING;
    rect.y      = y + CELL_VPADDING;
    rect.width  = MAX (0, width - (2 * CELL_HPADDING));
    rect.height = height - 2;

    gdk_gc_set_clip_rectangle (grid->gc, &rect);


    switch (gnc_table_get_align (table, virt_loc))
    {
    default:
    case CELL_ALIGN_LEFT:
        x_offset = 0;
        break;

    case CELL_ALIGN_RIGHT:
        x_offset = width - 2 * CELL_HPADDING - logical_rect.width;
        break;

    case CELL_ALIGN_CENTER:
        if (logical_rect.width > width - 2 * CELL_HPADDING)
            x_offset = 0;
        else
            x_offset = (width - 2 * CELL_HPADDING -
                        logical_rect.width) / 2;
        break;
    }



    gdk_draw_layout (drawable,
                     grid->gc,
                     x + CELL_HPADDING + x_offset,
                     y + CELL_VPADDING + 1,
                     layout);

    gdk_gc_set_clip_rectangle (grid->gc, NULL);

exit:
    pango_font_description_set_style (font, PANGO_STYLE_NORMAL);
    pango_context_set_font_description (context, font);
    pango_font_description_free (font);
    g_object_unref (layout);
}

static void
draw_block (GnucashGrid *grid,
            SheetBlock *block,
            VirtualLocation virt_loc,
            GdkDrawable *drawable,
            int x, int y, int width, int height)
{
    CellDimensions *cd;
    gint x_paint;
    gint y_paint;
    gint w, h;

    for ( virt_loc.phys_row_offset = 0;
            virt_loc.phys_row_offset < block->style->nrows ;
            virt_loc.phys_row_offset++ )
    {
        for ( virt_loc.phys_col_offset = 0;
                virt_loc.phys_col_offset < block->style->ncols ;
                virt_loc.phys_col_offset++ )
        {
            cd = gnucash_style_get_cell_dimensions
                 (block->style,
                  virt_loc.phys_row_offset,
                  virt_loc.phys_col_offset);

            x_paint = block->origin_x + cd->origin_x;
            if (x_paint > x + width)
                break;

            y_paint = block->origin_y + cd->origin_y;
            if (y_paint > y + height)
                return;

            h = cd->pixel_height;
            w = cd->pixel_width;

            if (w == 0)
                continue;

            if (x_paint + w < x)
                continue;

            if (y_paint + h < y)
                continue;

            draw_cell (grid, block, virt_loc, drawable,
                       x_paint - x, y_paint - y, w, h);
        }
    }
}

static void
gnucash_grid_draw (GnomeCanvasItem *item, GdkDrawable *drawable,
                   int x, int y, int width, int height)
{
    GnucashGrid *grid = GNUCASH_GRID (item);
    VirtualLocation virt_loc;
    SheetBlock *sheet_block;

    if (x < 0 || y < 0)
        return;

    /* compute our initial values where we start drawing */
    sheet_block = gnucash_grid_find_block_by_pixel (grid, x, y,
                  &virt_loc.vcell_loc);
    if (!sheet_block || !sheet_block->style)
        return;

    for ( ; virt_loc.vcell_loc.virt_row < grid->sheet->num_virt_rows;
            virt_loc.vcell_loc.virt_row++ )
    {
        while (TRUE)
        {
            sheet_block = gnucash_sheet_get_block
                          (grid->sheet, virt_loc.vcell_loc);

            if (!sheet_block || !sheet_block->style)
                return;

            if (sheet_block->visible)
                break;

            virt_loc.vcell_loc.virt_row++;
        }

        if (y + height < sheet_block->origin_y)
            return;

        draw_block (grid, sheet_block, virt_loc, drawable,
                    x, y, width, height);
    }
}


static void
gnucash_grid_init (GnucashGrid *grid)
{
    GnomeCanvasItem *item = GNOME_CANVAS_ITEM (grid);

    item->x1 = 0;
    item->y1 = 0;
    item->x2 = 0;
    item->y2 = 0;

    grid->top_block  = 0;
    grid->top_offset = 0;
    grid->left_offset = 0;
}


static void
gnucash_grid_set_property (GObject         *object,
                           guint            prop_id,
                           const GValue    *value,
                           GParamSpec      *pspec)
{
    GnucashGrid *grid = GNUCASH_GRID (object);

    switch (prop_id)
    {
    case PROP_SHEET:
        grid->sheet =
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
gnucash_grid_get_property (GObject         *object,
                           guint            prop_id,
                           GValue          *value,
                           GParamSpec      *pspec)
{
    GnucashGrid *grid = GNUCASH_GRID (object);

    switch (prop_id)
    {
    case PROP_SHEET:
        g_value_take_object (value, grid->sheet);
        break;
    default:
        break;
    }
}


static void
gnucash_grid_class_init (GnucashGridClass *klass)
{
    GObjectClass  *object_class;
    GnomeCanvasItemClass *item_class;

    object_class = G_OBJECT_CLASS (klass);
    item_class = GNOME_CANVAS_ITEM_CLASS (klass);

    gnucash_grid_parent_class = g_type_class_peek_parent (klass);

    /* GObject method overrides */
    object_class->set_property = gnucash_grid_set_property;
    object_class->get_property = gnucash_grid_get_property;

    /* GnomeCanvasItem method overrides */
    item_class->update      = gnucash_grid_update;
    item_class->realize     = gnucash_grid_realize;
    item_class->unrealize   = gnucash_grid_unrealize;
    item_class->draw        = gnucash_grid_draw;

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
gnucash_grid_get_type (void)
{
    static GType gnucash_grid_type = 0;

    if (!gnucash_grid_type)
    {
        static const GTypeInfo gnucash_grid_info =
        {
            sizeof (GnucashGridClass),
            NULL,		/* base_init */
            NULL,		/* base_finalize */
            (GClassInitFunc) gnucash_grid_class_init,
            NULL,		/* class_finalize */
            NULL,		/* class_data */
            sizeof (GnucashGrid),
            0,		/* n_preallocs */
            (GInstanceInitFunc) gnucash_grid_init
        };

        gnucash_grid_type =
            g_type_register_static (gnome_canvas_item_get_type (),
                                    "GnucashGrid",
                                    &gnucash_grid_info, 0);
    }

    return gnucash_grid_type;
}


