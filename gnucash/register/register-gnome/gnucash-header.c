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
#include <pango/pangocairo.h>

#include "gnucash-sheet.h"
#include "gnucash-sheetP.h"
#include "gnucash-color.h"
#include "gnucash-style.h"
#include "gnucash-cursor.h"
#include "gnucash-item-edit.h"
#include "gnc-gtk-utils.h"

#include "gnucash-header.h"

enum
{
    PROP_0,
    PROP_SHEET,       /*  the sheet this header is associated with */
    PROP_CURSOR_NAME, /* the name of the current cursor */
};

G_DEFINE_TYPE (GncHeader, gnc_header, GTK_TYPE_WIDGET)

static void
gnc_header_draw_offscreen (GncHeader *header)
{
    g_return_if_fail (GTK_IS_WIDGET(header));

    SheetBlockStyle *style = header->style;
    GncItemEdit *item_edit = GNC_ITEM_EDIT(header->sheet->item_editor);
    Table *table = header->sheet->table;
    VirtualLocation virt_loc;
    VirtualCell *vcell;
    GdkRGBA color;
    int row_offset;
    CellBlock *cb;
    int i;
    int scale;
    cairo_t *cr;

    virt_loc.vcell_loc.virt_row = 0;
    virt_loc.vcell_loc.virt_col = 0;
    virt_loc.phys_row_offset = 0;
    virt_loc.phys_col_offset = 0;

    gtk_widget_get_color (GTK_WIDGET (header), &color);

    if (header->surface)
        cairo_surface_destroy (header->surface);
    scale = gtk_widget_get_scale_factor (GTK_WIDGET(header));
    if (scale < 1)
        scale = 1;
    header->surface = cairo_image_surface_create (CAIRO_FORMAT_ARGB32,
                                                  header->width * scale,
                                                  header->height * scale);
    cairo_surface_set_device_scale (header->surface, scale, scale);

    cr = cairo_create (header->surface);
    cairo_set_operator (cr, CAIRO_OPERATOR_CLEAR);
    cairo_paint (cr);
    cairo_set_operator (cr, CAIRO_OPERATOR_OVER);

    GdkRGBA bg_color;
    gdk_rgba_parse (&bg_color, "#96B183");
    cairo_set_source_rgba (cr, bg_color.red, bg_color.green, bg_color.blue, bg_color.alpha);
    cairo_rectangle (cr, 0, 0, header->width, header->height);
    cairo_fill (cr);

    cairo_set_source_rgba (cr, color.red, color.green, color.blue, color.alpha);
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

            layout = gtk_widget_create_pango_layout (GTK_WIDGET(header->sheet), text);

            pango_layout_get_pixel_extents (layout, NULL, &logical_rect);

            gnucash_sheet_set_text_bounds (header->sheet, &rect,
                                           col_offset, row_offset, width, height);

            cairo_save (cr);
            cairo_rectangle (cr, rect.x, rect.y, rect.width, rect.height);
            cairo_clip (cr);

            x_offset = gnucash_sheet_get_text_offset (header->sheet, virt_loc,
                                                      rect.width, logical_rect.width);

            cairo_set_source_rgba (cr, color.red, color.green, color.blue, color.alpha);
            cairo_move_to (cr, rect.x + x_offset,
                           rect.y + gnc_item_edit_get_padding_border (item_edit, top));
            pango_cairo_show_layout (cr, layout);

            cairo_restore (cr);
            g_object_unref (layout);

            col_offset += width;
        }
        row_offset += height;
    }
    cairo_destroy (cr);
}


gint
gnc_header_get_cell_offset (GncHeader *header, gint col, gint *cell_width)
{
    SheetBlockStyle *style = header->style;
    gint j;
    gint offset = 0;

    for (j = 0; j < style->ncols; j++)
    {
        CellDimensions *cd;

        cd = gnucash_style_get_cell_dimensions (style, 0, j);
        if (!cd) continue;

        if (j == col)
        {
            *cell_width = cd->pixel_width;
            break;
        }
        offset = offset + cd->pixel_width;
    }
    return offset;
}


static void
gnc_header_snapshot (GtkWidget *widget, GtkSnapshot *snapshot)
{
    GncHeader *header = GNC_HEADER (widget);
    graphene_rect_t bounds;
    cairo_t *cr;
    double x_offset = 0;

    GTK_WIDGET_CLASS (gnc_header_parent_class)->snapshot (widget, snapshot);

    if (!header->surface)
        gnc_header_draw_offscreen (header);
    if (!header->surface)
        return;

    if (header->sheet && header->sheet->hadj)
        x_offset = -gtk_adjustment_get_value (header->sheet->hadj);

    graphene_rect_init (&bounds, 0, 0, gtk_widget_get_width (widget),
                        gtk_widget_get_height (widget));
    cr = gtk_snapshot_append_cairo (snapshot, &bounds);
    cairo_set_source_surface (cr, header->surface, x_offset, 0);
    cairo_paint (cr);
    cairo_destroy (cr);
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
gnc_header_finalize (GObject *object)
{
    GncHeader *header;

    header = GNC_HEADER(object);

    if (header->sheet && header->sheet->hadj &&
        header->hadjustment_handler)
        g_signal_handler_disconnect (header->sheet->hadj,
                                     header->hadjustment_handler);
    header->hadjustment_handler = 0;
    g_clear_object (&header->sheet);

    g_free (header->cursor_name);
    header->cursor_name = NULL;

    G_OBJECT_CLASS(gnc_header_parent_class)->finalize (object);
}


void
gnc_header_reconfigure (GncHeader *header)
{
    GnucashSheet *sheet;
    SheetBlockStyle *old_style;
    int w, h;

    g_return_if_fail (header != NULL);
    g_return_if_fail (GNC_IS_HEADER(header));

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
        gtk_widget_set_size_request (GTK_WIDGET (header), -1, h);
        gtk_widget_queue_resize (GTK_WIDGET (header));
        gnc_header_request_redraw (header);
    }
}

void
gnc_header_set_header_rows (GncHeader *header,
                            int num_phys_rows)
{
    g_return_if_fail (header != NULL);
    g_return_if_fail (GNC_IS_HEADER(header));

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

static void
gnc_header_motion_cb (GtkEventControllerMotion *controller,
                      double x,
                      G_GNUC_UNUSED double y,
                      GncHeader *header)
{
    int col;

    if (!header->style)
        return;

    if (header->in_resize)
    {
        int change = (int) x - header->resize_x;
        int new_width = header->resize_col_width + change;

        if (new_width >= 0)
        {
            header->resize_x = (int) x;
            header->resize_col_width = new_width;
            gnc_header_request_redraw (header);
        }
        return;
    }

    if (pointer_on_resize_line (header, (int) x, 0, &col) &&
        gnucash_style_col_is_resizable (header->style, col))
        gtk_widget_set_cursor_from_name (GTK_WIDGET (header), "col-resize");
    else
        gtk_widget_set_cursor (GTK_WIDGET (header), NULL);
}

static void
gnc_header_leave_cb (G_GNUC_UNUSED GtkEventControllerMotion *controller,
                     GncHeader *header)
{
    if (!header->in_resize)
        gtk_widget_set_cursor (GTK_WIDGET (header), NULL);
}

static void
gnc_header_pressed_cb (GtkGestureClick *gesture,
                       gint n_press,
                       double x,
                       G_GNUC_UNUSED double y,
                       GncHeader *header)
{
    int col;
    int resize_col;

    if (gtk_gesture_single_get_current_button (GTK_GESTURE_SINGLE (gesture)) !=
        GDK_BUTTON_PRIMARY || !header->style)
        return;

    if (pointer_on_resize_line (header, (int) x, 0, &col))
        resize_col = find_resize_col (header, col);
    else
        resize_col = n_press == 2 ? col : -1;

    if (n_press == 2)
    {
        if (resize_col > -1)
            gnc_header_auto_resize_column (header, resize_col);
        return;
    }

    if (resize_col > -1)
    {
        CellDimensions *dimensions =
            gnucash_style_get_cell_dimensions (header->style, 0, resize_col);

        if (!dimensions)
            return;

        header->in_resize = TRUE;
        header->resize_col = resize_col;
        header->resize_col_width = dimensions->pixel_width;
        header->resize_x = (int) x;
        gtk_gesture_set_state (GTK_GESTURE (gesture), GTK_EVENT_SEQUENCE_CLAIMED);
    }
}

static void
gnc_header_released_cb (GtkGestureClick *gesture,
                        G_GNUC_UNUSED gint n_press,
                        G_GNUC_UNUSED double x,
                        G_GNUC_UNUSED double y,
                        GncHeader *header)
{
    if (gtk_gesture_single_get_current_button (GTK_GESTURE_SINGLE (gesture)) !=
        GDK_BUTTON_PRIMARY || !header->in_resize)
        return;

    if (header->resize_col_width == 0)
        header->resize_col_width = 1;

    gnc_header_resize_column (header, header->resize_col,
                              header->resize_col_width);
    header->in_resize = FALSE;
    header->resize_col = -1;
    gnc_header_request_redraw (header);
}

static void
gnc_header_hadjustment_changed_cb (GtkAdjustment *adjustment,
                                   GncHeader *header)
{
    g_return_if_fail (GTK_IS_ADJUSTMENT (adjustment));
    gtk_widget_queue_draw (GTK_WIDGET (header));
}

static void
gnc_header_measure (GtkWidget *widget,
                    GtkOrientation orientation,
                    G_GNUC_UNUSED int for_size,
                    int *minimum,
                    int *natural,
                    int *minimum_baseline,
                    int *natural_baseline)
{
    GncHeader *header = GNC_HEADER (widget);
    int size = orientation == GTK_ORIENTATION_VERTICAL ? header->height :
                                                         header->width;

    *minimum = size;
    *natural = size;
    if (minimum_baseline)
        *minimum_baseline = -1;
    if (natural_baseline)
        *natural_baseline = -1;
}

/* g_value_set_object() supplies the referenced object through this
 * GObject property getter.
 */
static void
gnc_header_get_property (GObject *object,
                         guint param_id,
                         GValue *value,
                         GParamSpec *pspec)
{
    GncHeader *header = GNC_HEADER(object);

    switch (param_id)
    {
    case PROP_SHEET:
        g_value_set_object (value, header->sheet);
        break;
    case PROP_CURSOR_NAME:
        g_value_set_string (value, header->cursor_name);
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, param_id, pspec);
        break;
    }
}

static void
gnc_header_set_property (GObject *object,
                         guint param_id,
                         const GValue *value,
                         GParamSpec *pspec)
{
    GncHeader *header = GNC_HEADER(object);
    gboolean needs_update = FALSE;
    gchar *old_name;

    switch (param_id)
    {
    case PROP_SHEET:
        if (header->sheet && header->sheet->hadj &&
            header->hadjustment_handler)
            g_signal_handler_disconnect (header->sheet->hadj,
                                         header->hadjustment_handler);
        header->hadjustment_handler = 0;
        g_set_object (&header->sheet, GNUCASH_SHEET (g_value_get_object (value)));
        if (header->sheet && header->sheet->hadj)
            header->hadjustment_handler =
                g_signal_connect (header->sheet->hadj, "value-changed",
                                  G_CALLBACK (gnc_header_hadjustment_changed_cb),
                                  header);
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
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, param_id, pspec);
        break;
    }

    if ((header->sheet != NULL) && needs_update)
        gnc_header_reconfigure (header);
}


static void
gnc_header_init (GncHeader *header)
{
    GtkEventController *motion;
    GtkGesture *click;

    header->sheet = NULL;
    header->cursor_name = NULL;
    header->in_resize = FALSE;
    header->resize_col = -1;
    header->height = 20;
    header->width = 400;
    header->style = NULL;
    header->surface = NULL;
    header->hadjustment_handler = 0;

    motion = gtk_event_controller_motion_new ();
    g_signal_connect (motion, "motion", G_CALLBACK (gnc_header_motion_cb), header);
    g_signal_connect (motion, "leave", G_CALLBACK (gnc_header_leave_cb), header);
    gtk_widget_add_controller (GTK_WIDGET (header), motion);

    click = gtk_gesture_click_new ();
    g_signal_connect (click, "pressed", G_CALLBACK (gnc_header_pressed_cb), header);
    g_signal_connect (click, "released", G_CALLBACK (gnc_header_released_cb), header);
    gtk_widget_add_controller (GTK_WIDGET (header), GTK_EVENT_CONTROLLER (click));
}

static void
gnc_header_class_init (GncHeaderClass *header_class)
{
    GObjectClass  *object_class = G_OBJECT_CLASS(header_class);
    GtkWidgetClass *item_class = GTK_WIDGET_CLASS(header_class);

    gtk_widget_class_set_css_name (GTK_WIDGET_CLASS(header_class), "gnc-id-header");

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


    item_class->measure = gnc_header_measure;
    item_class->snapshot = gnc_header_snapshot;
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
