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

#include <config.h>

#include <string.h>

#include "gnucash-sheet.h"
#include "gnucash-sheetP.h"
#include "gnucash-color.h"
#include "gnucash-style.h"
#include "gnucash-cursor.h"
#include "gnucash-item-edit.h"
#include "gnc-gtk-utils.h"

#include "gnucash-header.h"

static GtkLayout *parent_class;

enum
{
    PROP_0,
    PROP_SHEET,       /*  the sheet this header is associated with */
    PROP_CURSOR_NAME, /* the name of the current cursor */
};

static void
gnc_header_draw_offscreen (GncHeader *header)
{
    SheetBlockStyle *style = header->style;
    GncItemEdit *item_edit = GNC_ITEM_EDIT(header->sheet->item_editor);
    Table *table = header->sheet->table;
    VirtualLocation virt_loc;
    VirtualCell *vcell;
    guint32 color_type;
    GtkStyleContext *stylectxt = gtk_widget_get_style_context (GTK_WIDGET(header));
    GdkRGBA color;
    int row_offset;
    CellBlock *cb;
    int i;
    cairo_t *cr;

    virt_loc.vcell_loc.virt_row = 0;
    virt_loc.vcell_loc.virt_col = 0;
    virt_loc.phys_row_offset = 0;
    virt_loc.phys_col_offset = 0;

    gtk_style_context_save (stylectxt);

    // Get the color type and apply the css class
    color_type = gnc_table_get_color (table, virt_loc, NULL);
    gnucash_get_style_classes (header->sheet, stylectxt, color_type);

    if (header->surface)
        cairo_surface_destroy (header->surface);
    header->surface = cairo_image_surface_create (CAIRO_FORMAT_ARGB32,
                                                header->width,
                                                header->height);

    cr = cairo_create (header->surface);

    // Fill background color of header
    gtk_render_background (stylectxt, cr, 0, 0, header->width, header->height);

    gdk_rgba_parse (&color, "black");
    cairo_set_source_rgb (cr, color.red, color.green, color.blue);
    cairo_rectangle (cr, 0.5, 0.5, header->width - 1.0, header->height - 1.0);
    cairo_set_line_width (cr, 1.0);
    cairo_stroke (cr);

    // Draw bottom horizontal line, makes bottom line thicker
    cairo_move_to (cr, 0.5, header->height - 1.5);
    cairo_line_to (cr, header->width - 1.0, header->height - 1.5);
    cairo_set_line_cap (cr, CAIRO_LINE_CAP_SQUARE);
    cairo_set_line_width (cr, 1.0);
    cairo_stroke (cr);

    /*font = gnucash_register_font;*/

    vcell = gnc_table_get_virtual_cell
            (table, table->current_cursor_loc.vcell_loc);
    cb = vcell ? vcell->cellblock : NULL;
    row_offset = 0;

    for (i = 0; i < style->nrows; i++)
    {
        int col_offset = 0;
        int height = 0, j;
        virt_loc.phys_row_offset = i;

        /* TODO: This routine is duplicated in several places.
           Can we abstract at least the cell drawing routine?
           That way we'll be sure everything is drawn
           consistently, and cut down on maintenance issues. */

        for (j = 0; j < style->ncols; j++)
        {
            CellDimensions *cd;
            double text_x, text_y, text_w, text_h;
            BasicCell *cell;
            const char *text;
            int width;
            PangoLayout *layout;
            PangoRectangle logical_rect;
            GdkRectangle rect;
            int x_offset;

            virt_loc.phys_col_offset = j;

            cd = gnucash_style_get_cell_dimensions (style, i, j);
            if (!cd) continue;
            
            height = cd->pixel_height;
            if (header->in_resize && (j == header->resize_col))
                width = header->resize_col_width;
            else
                width = cd->pixel_width;

            cell = gnc_cellblock_get_cell (cb, i, j);
            if (!cell || !cell->cell_name)
            {
                col_offset += width;
                continue;
            }

            cairo_rectangle (cr, col_offset - 0.5, row_offset + 0.5, width, height);
            cairo_set_line_width (cr, 1.0);
            cairo_stroke (cr);

            virt_loc.vcell_loc =
                table->current_cursor_loc.vcell_loc;
            text = gnc_table_get_label (table, virt_loc);
            if (!text)
                text = "";

            layout = gtk_widget_create_pango_layout (GTK_WIDGET (header->sheet), text);

            pango_layout_get_pixel_extents (layout, NULL, &logical_rect);

            gnucash_sheet_set_text_bounds (header->sheet, &rect,
                                           col_offset, row_offset, width, height);

            cairo_save (cr);
            cairo_rectangle (cr, rect.x, rect.y, rect.width, rect.height);
            cairo_clip (cr);

            x_offset = gnucash_sheet_get_text_offset (header->sheet, virt_loc,
                                                      rect.width, logical_rect.width);

            gtk_render_layout (stylectxt, cr, rect.x + x_offset,
                               rect.y + gnc_item_edit_get_padding_border (item_edit, top), layout);

            cairo_restore (cr);
            g_object_unref (layout);

            col_offset += width;
        }
        row_offset += height;
    }
    gtk_style_context_restore (stylectxt);

    cairo_destroy (cr);
}


static gboolean
gnc_header_draw (GtkWidget *header, cairo_t *cr)
{
    GnucashSheet *sheet = GNC_HEADER(header)->sheet;
    GdkWindow *sheet_layout_win = gtk_layout_get_bin_window (GTK_LAYOUT(sheet));
    gint x, y;

    // use this to get the scroll x value to align the header
    gdk_window_get_position (sheet_layout_win, &x, &y);

    // if the register page is moved to another window, the surface is
    // not created so test for a surface and create one if null
    if (GNC_HEADER(header)->surface == NULL)
        gnc_header_draw_offscreen (GNC_HEADER(header));

    cairo_set_source_surface (cr, GNC_HEADER(header)->surface, x, 0);
    cairo_paint (cr);

    return TRUE;
}


void
gnc_header_request_redraw (GncHeader *header)
{
    if (!header->style)
        return;

    gnc_header_draw_offscreen (header);
    gtk_widget_queue_draw (GTK_WIDGET(header));
}


static void
gnc_header_unrealize (GtkWidget *widget)
{
    GncHeader *header = GNC_HEADER (widget);
    if (header->surface)
        cairo_surface_destroy (header->surface);
    header->surface = NULL;

    if (header->resize_cursor)
        g_object_unref (header->resize_cursor);
    header->resize_cursor = NULL;

    if (header->normal_cursor)
        g_object_unref (header->normal_cursor);
    header->normal_cursor = NULL;

    if (GTK_WIDGET_CLASS (parent_class)->unrealize)
        GTK_WIDGET_CLASS (parent_class)->unrealize (GTK_WIDGET(header));
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
    GnucashSheet *sheet;
    SheetBlockStyle *old_style;
    int w, h;

    g_return_if_fail (header != NULL);
    g_return_if_fail (GNC_IS_HEADER (header));

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
        gtk_layout_set_size(GTK_LAYOUT(header), w, h);
        gtk_widget_set_size_request(GTK_WIDGET(header), -1, h);
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

/*
 *  Returns FALSE if pointer not on a resize line, else returns
 *  TRUE. Returns the index of the column to the left in the col
 *  argument.
 */
static gboolean
pointer_on_resize_line (GncHeader *header, int x, G_GNUC_UNUSED int y, int *col)
{
    SheetBlockStyle *style = header->style;
    gboolean on_the_line = FALSE;
    CellDimensions *cd;
    int pixels = 0;
    int j;

    for (j = 0; j < style->ncols; j++)
    {
        cd = gnucash_style_get_cell_dimensions (style, 0, j);
        if (!cd) continue;
        
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
            cd && (cd->pixel_width == 0))
        ++col;

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
gnc_header_event (GtkWidget *widget, GdkEvent *event)
{
    GncHeader *header = GNC_HEADER(widget);
    int x, y;
    int col;

    switch (event->type)
    {
    case GDK_MOTION_NOTIFY:
        x = event->motion.x;
        y = event->motion.y;

        if (header->in_resize)
        {
            int change = x - header->resize_x;
            int new_width = header->resize_col_width + change;

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
            gdk_window_set_cursor (gtk_widget_get_window (widget),
                                   header->resize_cursor);
        else
            gdk_window_set_cursor (gtk_widget_get_window (widget),
                                   header->normal_cursor);
        break;

    case GDK_BUTTON_PRESS:
    {
        int col;

        if (event->button.button != 1)
            break;

        x = event->button.x;
        y = event->button.y;

        if (pointer_on_resize_line (header, x, y, &col))
            col = find_resize_col (header, col);
        else
            col = -1;

        if (col > -1)
        {
            CellDimensions *cd;

            cd = gnucash_style_get_cell_dimensions
                 (header->style, 0, col);
            if (!cd) break;

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


        if (header->in_resize)
        {
            gnc_header_resize_column
                (header,
                 header->resize_col,
                 header->resize_col_width);
            header->in_resize = FALSE;
            header->resize_col = -1;
            gnc_header_request_redraw (header);
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

        x = event->button.x;
        y = event->button.y;

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
            gnc_header_auto_resize_column (header, resize_col);
        }
    }
    break;

    default:
        break;
    }
    return FALSE;
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
    GtkLayout *layout = GTK_LAYOUT (header);
    gboolean needs_update = FALSE;
    gchar *old_name;

    switch (param_id)
    {
    case PROP_SHEET:
        header->sheet = GNUCASH_SHEET (g_value_get_object (value));
        gtk_scrollable_set_hadjustment (GTK_SCROLLABLE(layout), header->sheet->hadj);
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
    header->resize_cursor = gdk_cursor_new_for_display (gdk_display_get_default (), GDK_SB_H_DOUBLE_ARROW);
    header->normal_cursor = NULL;
    header->height = 20;
    header->width = 400;
    header->style = NULL;

    // This sets a style class for when Gtk+ version is less than 3.20
    gnc_widget_set_css_name (GTK_WIDGET(header), "header");

    gtk_widget_add_events(GTK_WIDGET(header), (GDK_EXPOSURE_MASK
                          | GDK_BUTTON_PRESS_MASK
                          | GDK_BUTTON_RELEASE_MASK
                          | GDK_POINTER_MOTION_MASK
                          | GDK_POINTER_MOTION_HINT_MASK));

    g_signal_connect(G_OBJECT(header), "configure_event",
                     G_CALLBACK(gnc_header_reconfigure), NULL);
    gtk_widget_show_all (GTK_WIDGET(header));
}


static void
gnc_header_class_init (GncHeaderClass *header_class)
{
    GObjectClass  *object_class = G_OBJECT_CLASS (header_class);
    GtkWidgetClass *item_class = GTK_WIDGET_CLASS (header_class);

#if GTK_CHECK_VERSION(3,20,0)
    gtk_widget_class_set_css_name (GTK_WIDGET_CLASS(header_class), "header");
#endif

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


    item_class->unrealize = gnc_header_unrealize;
    item_class->draw      = gnc_header_draw;
    item_class->event     = gnc_header_event;
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

        gnc_header_type = g_type_register_static (GTK_TYPE_LAYOUT,
                          "GncHeader",
                          &gnc_header_info, 0);
    }

    return gnc_header_type;
}


GtkWidget *
gnc_header_new (GnucashSheet *sheet)
{
    GtkWidget *layout;

    layout = g_object_new (GNC_TYPE_HEADER,
                           "sheet", sheet,
                           "cursor_name", CURSOR_HEADER,
                           NULL);

    sheet->header_item = layout;
    return layout;
}


