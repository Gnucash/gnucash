/********************************************************************\
 * gnucash-item-edit.c -- cell editor cut-n-paste from gnumeric     *
 *                                                                  *
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
 *  An editor for the gnucash sheet.
 *  Cut and pasted from the gnumeric item-edit.c file.
 *
 *  And then substantially rewritten by Dave Peticolas <dave@krondo.com>.
 */


#include <config.h>

#include <string.h>

#include "gnucash-color.h"
#include "gnucash-cursor.h"
#include "gnucash-item-edit.h"
#include "gnucash-sheet.h"
#include "gnucash-sheetP.h"
#include "gnucash-style.h"


/* The arguments we take */
enum
{
    PROP_0,
    PROP_SHEET,     /* The sheet property      */
};

/* values for selection info */
enum
{
    TARGET_UTF8_STRING,
    TARGET_STRING,
    TARGET_TEXT,
    TARGET_COMPOUND_TEXT
};

#define MIN_BUTT_WIDTH 20 // minimum size for a button excluding border

static GtkBoxClass *gnc_item_edit_parent_class;

static GtkToggleButtonClass *gnc_item_edit_tb_parent_class;
static void gnc_item_edit_destroying(GtkWidget *this, gpointer data);
static void
gnc_item_edit_tb_init (GncItemEditTb *item_edit_tb)
{
    item_edit_tb->sheet = NULL;
}

static void
gnc_item_edit_tb_get_property (GObject *object,
                               guint param_id,
                               GValue *value,
                               GParamSpec *pspec)
{
    GncItemEditTb *item_edit_tb = GNC_ITEM_EDIT_TB (object);

    switch (param_id)
    {
    case PROP_SHEET:
        g_value_take_object (value, item_edit_tb->sheet);
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
        break;
    }
}

static void
gnc_item_edit_tb_set_property (GObject *object,
                               guint param_id,
                               const GValue *value,
                               GParamSpec *pspec)
{
    GncItemEditTb *item_edit_tb = GNC_ITEM_EDIT_TB (object);

    switch (param_id)
    {
    case PROP_SHEET:
        item_edit_tb->sheet = GNUCASH_SHEET (g_value_get_object (value));
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
        break;
    }
}

static void
gnc_item_edit_tb_get_preferred_width (GtkWidget *widget,
                                   gint *minimal_width,
                                   gint *natural_width)
{
    GncItemEditTb *tb = GNC_ITEM_EDIT_TB (widget);
    GncItemEdit *item_edit = GNC_ITEM_EDIT(tb->sheet->item_editor);
    GtkStyleContext *context = gtk_widget_get_style_context (GTK_WIDGET(tb));
    GtkBorder border;
    gint x, y, w, h = 2, width = 0;
    gnc_item_edit_get_pixel_coords (GNC_ITEM_EDIT (item_edit), &x, &y, &w, &h);
    width = ((h - 2)*2)/3;

    gtk_style_context_get_border (context, GTK_STATE_FLAG_NORMAL, &border);

    if (width < MIN_BUTT_WIDTH + border.left + border.right)
        width = MIN_BUTT_WIDTH + border.left + border.right;

    *minimal_width = *natural_width = width;
    item_edit->button_width = width;
}

static void
gnc_item_edit_tb_get_preferred_height (GtkWidget *widget,
                                    gint *minimal_width,
                                    gint *natural_width)
{
    GncItemEditTb *tb = GNC_ITEM_EDIT_TB (widget);
    GncItemEdit *item_edit = GNC_ITEM_EDIT(tb->sheet->item_editor);
    gint x, y, w, h = 2;
    gnc_item_edit_get_pixel_coords (GNC_ITEM_EDIT (item_edit), &x, &y, &w, &h);
    *minimal_width = *natural_width = (h - 2);
}

static void
gnc_item_edit_tb_class_init (GncItemEditTbClass *gnc_item_edit_tb_class)
{
    GObjectClass  *object_class;
    GtkWidgetClass *widget_class;

#if GTK_CHECK_VERSION(3,20,0)
    gtk_widget_class_set_css_name (GTK_WIDGET_CLASS(gnc_item_edit_tb_class), "button");
#endif

    gnc_item_edit_tb_parent_class = g_type_class_peek_parent (gnc_item_edit_tb_class);

    object_class = G_OBJECT_CLASS (gnc_item_edit_tb_class);
    widget_class = GTK_WIDGET_CLASS (gnc_item_edit_tb_class);

    object_class->get_property = gnc_item_edit_tb_get_property;
    object_class->set_property = gnc_item_edit_tb_set_property;

    g_object_class_install_property (object_class,
                                     PROP_SHEET,
                                     g_param_spec_object ("sheet",
                                             "Sheet Value",
                                             "Sheet Value",
                                             GNUCASH_TYPE_SHEET,
                                             G_PARAM_READWRITE));

    /* GtkWidget method overrides */
    widget_class->get_preferred_width = gnc_item_edit_tb_get_preferred_width;
    widget_class->get_preferred_height = gnc_item_edit_tb_get_preferred_height;
}

GType
gnc_item_edit_tb_get_type (void)
{
    static GType gnc_item_edit_tb_type = 0;

    if (!gnc_item_edit_tb_type)
    {
        static const GTypeInfo gnc_item_edit_tb_info =
        {
            sizeof (GncItemEditTbClass),
            NULL,
            NULL,
            (GClassInitFunc) gnc_item_edit_tb_class_init,
            NULL,
            NULL,
            sizeof (GncItemEditTb),
            0, /* n_preallocs */
            (GInstanceInitFunc) gnc_item_edit_tb_init,
            NULL,
        };
        gnc_item_edit_tb_type =
            g_type_register_static(GTK_TYPE_TOGGLE_BUTTON,
                                   "GncItemEditTb",
                                   &gnc_item_edit_tb_info, 0);
    }
    return gnc_item_edit_tb_type;
}

GtkWidget *
gnc_item_edit_tb_new (GnucashSheet *sheet)
{
    GtkStyleContext *context;
    GncItemEditTb *item_edit_tb =
            g_object_new (GNC_TYPE_ITEM_EDIT_TB,
                          "sheet", sheet,
                           NULL);

    // This sets a style class for when Gtk+ version is less than 3.20
    gnc_widget_set_css_name (GTK_WIDGET(item_edit_tb), "button");

    context = gtk_widget_get_style_context (GTK_WIDGET(item_edit_tb));
    gtk_style_context_add_class (context, GTK_STYLE_CLASS_BUTTON);

    return GTK_WIDGET(item_edit_tb);
}

/*
 * Returns the coordinates for the editor bounding box
 */
void
gnc_item_edit_get_pixel_coords (GncItemEdit *item_edit,
                                int *x, int *y,
                                int *w, int *h)
{
    GnucashSheet *sheet = item_edit->sheet;
    SheetBlock *block;
    int xd, yd;

    if (sheet == NULL)
        return;

    block = gnucash_sheet_get_block (sheet, item_edit->virt_loc.vcell_loc);
    if (block == NULL)
        return;

    xd = block->origin_x;
    yd = block->origin_y;

    gnucash_sheet_style_get_cell_pixel_rel_coords
    (item_edit->style,
     item_edit->virt_loc.phys_row_offset,
     item_edit->virt_loc.phys_col_offset,
     x, y, w, h);

    // alter cell size of first column
    if (item_edit->virt_loc.phys_col_offset == 0)
    {
        *x = *x + 1;
        *w = *w - 1;
    }
    *x += xd;
    *y += yd;
}

static gboolean
gnc_item_edit_update (GncItemEdit *item_edit)
{
    gint x = 0, y = 0, w, h;

    if (item_edit == NULL || item_edit->sheet == NULL)
        return FALSE;
    gnc_item_edit_get_pixel_coords (item_edit, &x, &y, &w, &h);
    gtk_layout_move (GTK_LAYOUT(item_edit->sheet),
                     GTK_WIDGET(item_edit), x, y);

    if (item_edit->is_popup)
    {
        gtk_widget_show (item_edit->popup_toggle.ebox);
        if (item_edit->show_popup)
            gnc_item_edit_show_popup (item_edit);
    }
    return FALSE;
}

void
gnc_item_edit_focus_in (GncItemEdit *item_edit)
{
    GdkEventFocus ev;

    g_return_if_fail (item_edit != NULL);
    g_return_if_fail (GNC_IS_ITEM_EDIT(item_edit));

    ev.type = GDK_FOCUS_CHANGE;
    ev.window = gtk_widget_get_window (GTK_WIDGET (item_edit->sheet));
    ev.in = TRUE;
    gtk_widget_event (item_edit->editor, (GdkEvent*) &ev);
}

void
gnc_item_edit_focus_out (GncItemEdit *item_edit)
{
    GdkEventFocus ev;

    g_return_if_fail (item_edit != NULL);
    g_return_if_fail (GNC_IS_ITEM_EDIT(item_edit));

    ev.type = GDK_FOCUS_CHANGE;
    ev.window = gtk_widget_get_window (GTK_WIDGET (item_edit->sheet));
    ev.in = FALSE;
    gtk_widget_event (item_edit->editor, (GdkEvent*) &ev);
}

/*
 * Instance initialization
 */
static void
gnc_item_edit_init (GncItemEdit *item_edit)
{
    /* Set invalid values so that we know when we have been fully
           initialized */
    gtk_orientable_set_orientation (GTK_ORIENTABLE(item_edit),
                                    GTK_ORIENTATION_HORIZONTAL);

    item_edit->sheet = NULL;
    item_edit->editor = NULL;

    item_edit->is_popup = FALSE;
    item_edit->show_popup = FALSE;

    item_edit->popup_toggle.ebox = NULL;
    item_edit->popup_toggle.tbutton = NULL;
    item_edit->popup_toggle.arrow_down = TRUE;
    item_edit->popup_toggle.signals_connected = FALSE;

    item_edit->popup_item = NULL;
    item_edit->popup_get_height = NULL;
    item_edit->popup_autosize = NULL;
    item_edit->popup_set_focus = NULL;
    item_edit->popup_post_show = NULL;
    item_edit->popup_user_data = NULL;

    item_edit->style = NULL;
    item_edit->button_width = MIN_BUTT_WIDTH;

    gnc_virtual_location_init(&item_edit->virt_loc);
}

void
gnc_item_edit_configure (GncItemEdit *item_edit)
{
    GnucashSheet *sheet = item_edit->sheet;
    GnucashCursor *cursor;
    gfloat xalign;

    cursor = GNUCASH_CURSOR(sheet->cursor);

    item_edit->virt_loc.vcell_loc.virt_row = cursor->row;
    item_edit->virt_loc.vcell_loc.virt_col = cursor->col;

    item_edit->style =
        gnucash_sheet_get_style (sheet,
                                 item_edit->virt_loc.vcell_loc);

    item_edit->virt_loc.phys_row_offset = cursor->cell.row;
    item_edit->virt_loc.phys_col_offset = cursor->cell.col;

    switch (gnc_table_get_align (sheet->table, item_edit->virt_loc))
    {
        default:
        case CELL_ALIGN_LEFT:
            xalign = 0;
            break;

        case CELL_ALIGN_RIGHT:
            xalign = 1;
            break;

        case CELL_ALIGN_CENTER:
            xalign = 0.5;
            break;
    }
    gtk_entry_set_alignment(GTK_ENTRY(item_edit->editor), xalign);

    if (!gnc_table_is_popup (sheet->table, item_edit->virt_loc))
        gnc_item_edit_set_popup (item_edit, NULL, NULL, NULL,
                                 NULL, NULL, NULL, NULL);

    g_idle_add_full (G_PRIORITY_HIGH_IDLE,
                    (GSourceFunc) gnc_item_edit_update, item_edit, NULL);
}


void
gnc_item_edit_cut_clipboard (GncItemEdit *item_edit)
{
    gtk_editable_cut_clipboard(GTK_EDITABLE(item_edit->editor));
}

void
gnc_item_edit_copy_clipboard (GncItemEdit *item_edit)
{
    gtk_editable_copy_clipboard(GTK_EDITABLE(item_edit->editor));
}

void
gnc_item_edit_paste_clipboard (GncItemEdit *item_edit)
{
    gtk_editable_paste_clipboard(GTK_EDITABLE(item_edit->editor));
}


static gboolean
key_press_popup_cb (GtkWidget *widget, GdkEventKey *event, gpointer data)
{
    GncItemEdit *item_edit = GNC_ITEM_EDIT (data);

    g_signal_stop_emission_by_name (widget, "key_press_event");

    gtk_widget_event (GTK_WIDGET(item_edit->sheet), (GdkEvent *) event);

    return TRUE;
}


static void
gnc_item_edit_popup_toggled (GtkToggleButton *button, gpointer data)
{
    GncItemEdit *item_edit = GNC_ITEM_EDIT (data);
    gboolean show_popup;

    show_popup = gtk_toggle_button_get_active (button);
    if (show_popup)
    {
        Table *table;
        VirtualLocation virt_loc;

        table = item_edit->sheet->table;
        virt_loc = table->current_cursor_loc;

        if (!gnc_table_confirm_change (table, virt_loc))
        {
            g_signal_handlers_block_matched
            (button, G_SIGNAL_MATCH_DATA,
             0, 0, NULL, NULL, data);

            gtk_toggle_button_set_active (button, FALSE);

            g_signal_handlers_unblock_matched
            (button, G_SIGNAL_MATCH_DATA,
             0, 0, NULL, NULL, data);

            return;
        }
    }

    item_edit->show_popup = show_popup;

    if (!item_edit->show_popup)
        gnc_item_edit_hide_popup (item_edit);

    gnc_item_edit_configure (item_edit);
}


static void
block_toggle_signals(GncItemEdit *item_edit)
{
    GObject *obj;

    if (!item_edit->popup_toggle.signals_connected)
        return;

    obj = G_OBJECT (item_edit->popup_toggle.tbutton);

    g_signal_handlers_block_matched (obj, G_SIGNAL_MATCH_DATA,
                                     0, 0, NULL, NULL, item_edit);
}


static void
unblock_toggle_signals(GncItemEdit *item_edit)
{
    GObject *obj;

    if (!item_edit->popup_toggle.signals_connected)
        return;

    obj = G_OBJECT (item_edit->popup_toggle.tbutton);

    g_signal_handlers_unblock_matched (obj, G_SIGNAL_MATCH_DATA,
                                       0, 0, NULL, NULL, item_edit);
}


static gboolean
draw_background_cb (GtkWidget *widget, cairo_t *cr, gpointer user_data)
{
    GtkStyleContext *stylectxt = gtk_widget_get_style_context (widget);
    GncItemEdit *item_edit = GNC_ITEM_EDIT (user_data);
    gint width = gtk_widget_get_allocated_width (widget);
    gint height = gtk_widget_get_allocated_height (widget);
    guint32 color_type;

    gtk_style_context_save (stylectxt);

    // Get the color type and apply the css class
    color_type = gnc_table_get_color (item_edit->sheet->table, item_edit->virt_loc, NULL);
    gnucash_get_style_classes (item_edit->sheet, stylectxt, color_type);

    gtk_render_background (stylectxt, cr, 0, 1, width, height - 2);

    gtk_style_context_restore (stylectxt);
    return FALSE;
}


static gboolean
draw_text_cursor_cb (GtkWidget *widget, cairo_t *cr, gpointer user_data)
{
    GncItemEdit *item_edit = GNC_ITEM_EDIT(user_data);
    GtkEditable *editable = GTK_EDITABLE(widget);
    GtkStyleContext *stylectxt = gtk_widget_get_style_context (GTK_WIDGET(widget));
    GtkStateFlags flags = gtk_widget_get_state_flags (GTK_WIDGET(widget));
    gint height = gtk_widget_get_allocated_height (widget);
    const gchar *text;
    GdkRGBA *fg_color;
    GdkRGBA color;
    gint x_offset;
    gint cursor_x = 0;

    // Get the layout x offset
    gtk_entry_get_layout_offsets (GTK_ENTRY(widget), &x_offset, NULL);

    // Get the foreground color
    gdk_rgba_parse (&color, "black");
    gtk_style_context_get_color (stylectxt, flags, &color);
    fg_color = &color;

    text = gtk_entry_get_text (GTK_ENTRY (widget));

    if ((text != NULL) && (*text != '\0'))
    {
        PangoLayout *layout;
        PangoRectangle strong_pos;
        gint start_pos, end_pos, cursor_pos, cursor_byte_pos;

        cursor_pos = gtk_editable_get_position (editable);
        cursor_byte_pos = g_utf8_offset_to_pointer (text, cursor_pos) - text;

        gtk_editable_get_selection_bounds (editable, &start_pos, &end_pos);

        layout = gtk_entry_get_layout (GTK_ENTRY(widget));
        pango_layout_get_cursor_pos (layout, cursor_byte_pos, &strong_pos, NULL);
        cursor_x = x_offset + PANGO_PIXELS (strong_pos.x);
    }
    else
        cursor_x = x_offset;

    // Now draw a vertical line
    cairo_set_source_rgb (cr, fg_color->red, fg_color->green, fg_color->blue);
    cairo_set_line_width (cr, 1.0);
#if GTK_CHECK_VERSION(3,20,0)
    cairo_move_to (cr, cursor_x + 0.5, gnc_item_edit_get_margin (item_edit, top) +
                                       gnc_item_edit_get_padding_border (item_edit, top));
    cairo_rel_line_to (cr, 0, height - gnc_item_edit_get_margin (item_edit, top_bottom) -
                                       gnc_item_edit_get_padding_border (item_edit, top_bottom));
#else
    cairo_move_to (cr, cursor_x + 0.5, gnc_item_edit_get_padding_border (item_edit, top));
    cairo_rel_line_to (cr, 0, height - gnc_item_edit_get_padding_border (item_edit, top_bottom));
#endif
    cairo_stroke (cr);

    return FALSE;
}


static gboolean
draw_arrow_cb (GtkWidget *widget, cairo_t *cr, gpointer data)
{
    GncItemEdit *item_edit = GNC_ITEM_EDIT (data);
    GtkStyleContext *context = gtk_widget_get_style_context (widget);
    gint width = gtk_widget_get_allocated_width (widget);
    gint height = gtk_widget_get_allocated_height (widget);
    gint size;

    // allow room for a border
    gtk_render_background (context, cr, 2, 2, width - 4, height - 4);

    gtk_style_context_add_class (context, GTK_STYLE_CLASS_ARROW);

    size = MIN(width / 2, height / 2);

    if (item_edit->popup_toggle.arrow_down == 0)
        gtk_render_arrow (context, cr, 0,
                         (width - size)/2, (height - size)/2, size);
    else
        gtk_render_arrow (context, cr, G_PI,
                         (width - size)/2, (height - size)/2, size);

    return FALSE;
}


static void
connect_popup_toggle_signals (GncItemEdit *item_edit)
{
    GObject *object;

    g_return_if_fail(GNC_IS_ITEM_EDIT(item_edit));

    if (item_edit->popup_toggle.signals_connected)
        return;

    object = G_OBJECT(item_edit->popup_toggle.tbutton);

    g_signal_connect (object, "toggled",
                      G_CALLBACK(gnc_item_edit_popup_toggled),
                      item_edit);

    g_signal_connect (object, "key_press_event",
                      G_CALLBACK(key_press_popup_cb),
                      item_edit);

    g_signal_connect_after (object, "draw",
                            G_CALLBACK (draw_arrow_cb),
                            item_edit);

    item_edit->popup_toggle.signals_connected = TRUE;
}


static void
disconnect_popup_toggle_signals (GncItemEdit *item_edit)
{
    g_return_if_fail(GNC_IS_ITEM_EDIT(item_edit));

    if (!item_edit->popup_toggle.signals_connected)
        return;

    g_signal_handlers_disconnect_matched
    (item_edit->popup_toggle.tbutton, G_SIGNAL_MATCH_DATA,
     0, 0, NULL, NULL, item_edit);

    item_edit->popup_toggle.signals_connected = FALSE;
}

/* Note that g_value_set_object() refs the object, as does
 * g_object_get(). But g_object_get() only unrefs once when it disgorges
 * the object, leaving an unbalanced ref, which leaks. So instead of
 * using g_value_set_object(), use g_value_take_object() which doesn't
 * ref the object when used in get_property().
 */
static void
gnc_item_edit_get_property (GObject *object,
                            guint param_id,
                            GValue *value,
                            GParamSpec *pspec)
{
    GncItemEdit *item_edit = GNC_ITEM_EDIT (object);

    switch (param_id)
    {
    case PROP_SHEET:
        g_value_take_object (value, item_edit->sheet);
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
        break;
    }
}

static void
gnc_item_edit_set_property (GObject *object,
                            guint param_id,
                            const GValue *value,
                            GParamSpec *pspec)
{
    GncItemEdit *item_edit = GNC_ITEM_EDIT (object);
    switch (param_id)
    {
    case PROP_SHEET:
        item_edit->sheet = GNUCASH_SHEET (g_value_get_object (value));
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
        break;
    }
}

static void
gnc_item_edit_get_preferred_width (GtkWidget *widget,
                                   gint *minimal_width,
                                   gint *natural_width)
{
    gint x, y, w = 1, h;
    gnc_item_edit_get_pixel_coords (GNC_ITEM_EDIT (widget), &x, &y, &w, &h);
    *minimal_width = *natural_width = w - 1;
}


static void
gnc_item_edit_get_preferred_height (GtkWidget *widget,
                                    gint *minimal_width,
                                    gint *natural_width)
{
    gint x, y, w, h = 1;
    gnc_item_edit_get_pixel_coords (GNC_ITEM_EDIT (widget), &x, &y, &w, &h);
    *minimal_width = *natural_width = h - 1;
}

/*
 * GncItemEdit class initialization
 */
static void
gnc_item_edit_class_init (GncItemEditClass *gnc_item_edit_class)
{
    GObjectClass  *object_class;
    GtkWidgetClass *widget_class;

#if GTK_CHECK_VERSION(3,20,0)
    gtk_widget_class_set_css_name (GTK_WIDGET_CLASS(gnc_item_edit_class), "cursor");
#endif

    gnc_item_edit_parent_class = g_type_class_peek_parent (gnc_item_edit_class);

    object_class = G_OBJECT_CLASS (gnc_item_edit_class);
    widget_class = GTK_WIDGET_CLASS (gnc_item_edit_class);

    object_class->get_property = gnc_item_edit_get_property;
    object_class->set_property = gnc_item_edit_set_property;

    g_object_class_install_property (object_class,
                                     PROP_SHEET,
                                     g_param_spec_object ("sheet",
                                             "Sheet Value",
                                             "Sheet Value",
                                             GNUCASH_TYPE_SHEET,
                                             G_PARAM_READWRITE));

    /* GtkWidget method overrides */
    widget_class->get_preferred_width = gnc_item_edit_get_preferred_width;
    widget_class->get_preferred_height = gnc_item_edit_get_preferred_height;
}

/* FIXME: This way of initializing GObjects is obsolete. We should be
 * using G_DECLARE_FINAL_TYPE instead of rolling _get_type by hand.
 */
GType
gnc_item_edit_get_type (void)
{
    static GType gnc_item_edit_type = 0;

    if (!gnc_item_edit_type)
    {
        static const GTypeInfo gnc_item_edit_info =
        {
            sizeof (GncItemEditClass),
            NULL,
            NULL,
            (GClassInitFunc) gnc_item_edit_class_init,
            NULL,
            NULL,
            sizeof (GncItemEdit),
            0, /* n_preallocs */
            (GInstanceInitFunc) gnc_item_edit_init,
            NULL,
        };

        gnc_item_edit_type =
            g_type_register_static(GTK_TYPE_BOX,
                                   "GncItemEdit",
                                   &gnc_item_edit_info, 0);
    }

    return gnc_item_edit_type;
}

gint
gnc_item_edit_get_margin (GncItemEdit *item_edit, Sides side)
{
    switch (side)
    {
    case left:
        return item_edit->margin.left;
    case right:
        return item_edit->margin.right;
    case top:
        return item_edit->margin.top;
    case bottom:
        return item_edit->margin.bottom;
    case left_right:
        return item_edit->margin.left + item_edit->margin.right;
    case top_bottom:
        return item_edit->margin.top + item_edit->margin.bottom;
    default:
        return 2;
    }
}

gint
gnc_item_edit_get_padding_border (GncItemEdit *item_edit, Sides side)
{
    switch (side)
    {
    case left:
        return item_edit->padding.left + item_edit->border.left;
    case right:
        return item_edit->padding.right + item_edit->border.right;
    case top:
        return item_edit->padding.top + item_edit->border.top;
    case bottom:
        return item_edit->padding.bottom + item_edit->border.bottom;
    case left_right:
        return item_edit->padding.left + item_edit->border.left +
               item_edit->padding.right + item_edit->border.right;
    case top_bottom:
        return item_edit->padding.top + item_edit->border.top +
               item_edit->padding.bottom + item_edit->border.bottom;
    default:
        return 2;
    }
}

gint
gnc_item_edit_get_button_width (GncItemEdit *item_edit)
{
    if (item_edit)
    {
        if (gtk_widget_is_visible (GTK_WIDGET(item_edit->popup_toggle.tbutton)))
            return item_edit->button_width;
        else
        {
            GtkStyleContext *context = gtk_widget_get_style_context (GTK_WIDGET(item_edit->popup_toggle.tbutton));
            GtkBorder border;

            gtk_style_context_get_border (context, GTK_STATE_FLAG_NORMAL, &border);
            return MIN_BUTT_WIDTH + border.left + border.right;
        }
    }
    return MIN_BUTT_WIDTH + 2; // add the default border
}

static gboolean
button_press_cb (GtkWidget *widget, GdkEventButton *event, gpointer *pointer)
{
    GnucashSheet *sheet = GNUCASH_SHEET(pointer);

    /* Ignore double-clicks and triple-clicks */
    if (event->button == 3 && event->type == GDK_BUTTON_PRESS)
    {
        // This is a right click event so over ride entry menu and
        // display main register popup menu.
        g_signal_emit_by_name (sheet->reg, "show_popup_menu");
        return TRUE;
    }
    return FALSE;
}

GtkWidget *
gnc_item_edit_new (GnucashSheet *sheet)
{
    GtkStyleContext *stylectxt;
    GtkBorder padding;
    GtkBorder margin;
    GtkBorder border;
    GtkWidget *vb = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
    GncItemEdit *item_edit =
            g_object_new (GNC_TYPE_ITEM_EDIT,
                          "sheet", sheet,
                          "spacing",     0,
                          "homogeneous", FALSE,
                           NULL);
    gtk_layout_put (GTK_LAYOUT(sheet), GTK_WIDGET(item_edit), 0, 0);

    // This sets a style class for when Gtk+ version is less than 3.20
    gnc_widget_set_css_name (GTK_WIDGET(item_edit), "cursor");

    /* Create the text entry */
    item_edit->editor = gtk_entry_new();
    sheet->entry = item_edit->editor;
    gtk_entry_set_width_chars (GTK_ENTRY(item_edit->editor), 1);
    gtk_box_pack_start (GTK_BOX(item_edit), item_edit->editor, TRUE, TRUE, 0);

    // Get the CSS space settings for the entry
    stylectxt = gtk_widget_get_style_context (GTK_WIDGET(item_edit->editor));
    gtk_style_context_add_class (stylectxt, "register-foreground");
    gtk_style_context_get_padding (stylectxt, GTK_STATE_FLAG_NORMAL, &padding);
    gtk_style_context_get_margin (stylectxt, GTK_STATE_FLAG_NORMAL, &margin);
    gtk_style_context_get_border (stylectxt, GTK_STATE_FLAG_NORMAL, &border);

    item_edit->padding = padding;
    item_edit->margin = margin;
    item_edit->border = border;

    // Make sure the Entry can not have focus and no frame
    gtk_widget_set_can_focus (GTK_WIDGET(item_edit->editor), FALSE);
    gtk_entry_set_has_frame (GTK_ENTRY(item_edit->editor), FALSE);

#if !GTK_CHECK_VERSION(3,20,0)
#if GTK_CHECK_VERSION(3,12,0)
    gtk_widget_set_margin_start (GTK_WIDGET(item_edit->editor),
                                 gnc_item_edit_get_margin (item_edit, left));
    gtk_widget_set_margin_end (GTK_WIDGET(item_edit->editor),
                               gnc_item_edit_get_margin (item_edit, right));
#else
    gtk_widget_set_margin_left (GTK_WIDGET(item_edit->editor),
                                gnc_item_edit_get_margin (item_edit, left));
    gtk_widget_set_margin_right (GTK_WIDGET(item_edit->editor),
                                 gnc_item_edit_get_margin (item_edit, right));
#endif
    gtk_widget_set_margin_top (GTK_WIDGET(item_edit->editor),
                               gnc_item_edit_get_margin (item_edit, top));
    gtk_widget_set_margin_bottom (GTK_WIDGET(item_edit->editor),
                                  gnc_item_edit_get_margin (item_edit, bottom));
#endif

    // Connect to the draw signal so we can draw a cursor
    g_signal_connect_after (item_edit->editor, "draw",
                            G_CALLBACK (draw_text_cursor_cb), item_edit);

    // Fill in the background so the underlying sheet cell can not be seen
    g_signal_connect (item_edit, "draw",
                            G_CALLBACK (draw_background_cb), item_edit);

    // This call back intercepts the mouse button event so the main
    // register popup menu can be displayed instead of the entry one.
    g_signal_connect (item_edit->editor, "button-press-event",
                            G_CALLBACK (button_press_cb), sheet);

    /* Create the popup button
       It will only be displayed when the cell being edited provides
       a popup item (like a calendar or account list) */
    item_edit->popup_toggle.tbutton = gnc_item_edit_tb_new (sheet);
    gtk_toggle_button_set_mode (GTK_TOGGLE_BUTTON (item_edit->popup_toggle.tbutton), FALSE);

    /* Wrap the popup button in an event box to give it its own gdkwindow.
     * Without one the button would disappear behind the grid object. */
    item_edit->popup_toggle.ebox = gtk_event_box_new();
    g_object_ref(item_edit->popup_toggle.ebox);
    gtk_container_add(GTK_CONTAINER(item_edit->popup_toggle.ebox),
                      item_edit->popup_toggle.tbutton);

    /* The button needs to be packed into a vertical box so that the height and position
     * can be controlled in earlier than Gtk3.20 versions */
    gtk_box_pack_start (GTK_BOX(vb), item_edit->popup_toggle.ebox,
                        FALSE, FALSE, 0);

    gtk_box_pack_start (GTK_BOX(item_edit), vb, FALSE, FALSE, 0);
    gtk_widget_show_all(GTK_WIDGET(item_edit));
    g_signal_connect(G_OBJECT(item_edit), "destroy",
                     G_CALLBACK(gnc_item_edit_destroying), NULL);
    return GTK_WIDGET(item_edit);
}

static void
gnc_item_edit_destroying(GtkWidget *item_edit, gpointer data)
{
    while (g_idle_remove_by_data((gpointer)item_edit))
        continue;
}

static void
check_popup_height_is_true (GtkWidget    *widget,
                            GdkRectangle *allocation,
                            gpointer user_data)
{
    GncItemEdit *item_edit = GNC_ITEM_EDIT(user_data);

    // if a larger font is specified in css for the sheet, the popup returned height value
    // on first pop does not reflect the true height but the minimum height so just to be
    // sure check this value against the allocated one.
    if (allocation->height != item_edit->popup_returned_height)
    {
        gtk_container_remove (GTK_CONTAINER(item_edit->sheet), item_edit->popup_item);

        g_idle_add_full (G_PRIORITY_HIGH_IDLE,
                        (GSourceFunc) gnc_item_edit_update, item_edit, NULL);
    }
}

void
gnc_item_edit_show_popup (GncItemEdit *item_edit)
{
    GtkToggleButton *toggle;
    GtkAdjustment *vadj, *hadj;
    GtkAllocation alloc;
    GnucashSheet *sheet;
    gint x = 0, y = 0, w = 0, h = 0;
    gint y_offset, x_offset;
    gint popup_x, popup_y;
    gint popup_w = -1, popup_h = -1;
    gint popup_max_width, popup_max_height;
    gint view_width, view_height;
    gint down_height, up_height;
    gint sheet_width;

    g_return_if_fail (item_edit != NULL);
    g_return_if_fail (GNC_IS_ITEM_EDIT(item_edit));

    if (!item_edit->is_popup)
        return;

    sheet = item_edit->sheet;

    sheet_width = sheet->width;

    gtk_widget_get_allocation (GTK_WIDGET (sheet), &alloc);
    view_height = alloc.height;

    vadj = gtk_scrollable_get_vadjustment(GTK_SCROLLABLE(sheet));
    hadj = gtk_scrollable_get_hadjustment(GTK_SCROLLABLE(sheet));

    y_offset = gtk_adjustment_get_value(vadj);
    x_offset = gtk_adjustment_get_value(hadj);
    gnc_item_edit_get_pixel_coords (item_edit, &x, &y, &w, &h);

    popup_x = x;

    up_height = y - y_offset;
    down_height = view_height - (up_height + h);

    popup_max_height = MAX (up_height, down_height);
    popup_max_width = sheet_width - popup_x + x_offset; // always pops to the right

    if (item_edit->popup_get_height)
        popup_h = item_edit->popup_get_height
                       (item_edit->popup_item, popup_max_height, h,
                        item_edit->popup_user_data);

    if (item_edit->popup_autosize)
        popup_w =
            item_edit->popup_autosize (item_edit->popup_item,
                                       popup_max_width,
                                       item_edit->popup_user_data);
    else
        popup_w = 0;

    // Adjust the popup_y point based on popping above or below
    if (up_height > down_height)
        popup_y = y - popup_h;
    else
        popup_y = y + h;

    if (!gtk_widget_get_parent (item_edit->popup_item))
        gtk_layout_put (GTK_LAYOUT(sheet), item_edit->popup_item, popup_x, popup_y);

    // Lets check popup height is the true height
    item_edit->popup_returned_height = popup_h;
    g_signal_connect_after (item_edit->popup_item, "size-allocate",
                            G_CALLBACK(check_popup_height_is_true), item_edit);

    gtk_widget_set_size_request (item_edit->popup_item, popup_w - 1, popup_h);

    toggle = GTK_TOGGLE_BUTTON(item_edit->popup_toggle.tbutton);

    if (!gtk_toggle_button_get_active (toggle))
    {
        block_toggle_signals (item_edit);
        gtk_toggle_button_set_active (toggle, TRUE);
        unblock_toggle_signals (item_edit);
    }

    // set the popup arrow direction up
    item_edit->popup_toggle.arrow_down = FALSE;

    if (item_edit->popup_set_focus)
        item_edit->popup_set_focus (item_edit->popup_item,
                                    item_edit->popup_user_data);

    if (item_edit->popup_post_show)
        item_edit->popup_post_show (item_edit->popup_item,
                                    item_edit->popup_user_data);

    if (item_edit->popup_get_width)
    {
        int popup_width;

        popup_width = item_edit->popup_get_width
                      (item_edit->popup_item,
                       item_edit->popup_user_data);

        if (popup_width > popup_w)
            popup_width = popup_w;

        if (popup_width > popup_max_width)
        {
            popup_x -= popup_width - popup_max_width;
            popup_x = MAX (0, popup_x);
        }
        else
            popup_x = x;

        gtk_layout_move (GTK_LAYOUT(sheet), item_edit->popup_item, popup_x, popup_y);
    }
}


void
gnc_item_edit_hide_popup (GncItemEdit *item_edit)
{
    g_return_if_fail(item_edit != NULL);
    g_return_if_fail(GNC_IS_ITEM_EDIT(item_edit));

    if (!item_edit->is_popup)
        return;

    if (gtk_widget_get_parent (GTK_WIDGET(item_edit->popup_item)) != GTK_WIDGET (item_edit->sheet))
        return;

    gtk_container_remove (GTK_CONTAINER(item_edit->sheet), item_edit->popup_item);

    // set the popup arrow direction down
    item_edit->popup_toggle.arrow_down = TRUE;

    gtk_toggle_button_set_active
    (GTK_TOGGLE_BUTTON(item_edit->popup_toggle.tbutton), FALSE);

    gtk_widget_grab_focus (GTK_WIDGET (item_edit->sheet));
}

void
gnc_item_edit_set_popup (GncItemEdit    *item_edit,
                         GtkWidget      *popup_item,
                         PopupGetHeight  popup_get_height,
                         PopupAutosize   popup_autosize,
                         PopupSetFocus   popup_set_focus,
                         PopupPostShow   popup_post_show,
                         PopupGetWidth   popup_get_width,
                         gpointer        popup_user_data)
{
    g_return_if_fail (GNC_IS_ITEM_EDIT(item_edit));

    if (item_edit->is_popup)
        gnc_item_edit_hide_popup (item_edit);

    item_edit->is_popup = popup_item != NULL;

    item_edit->popup_item       = popup_item;
    item_edit->popup_get_height = popup_get_height;
    item_edit->popup_autosize   = popup_autosize;
    item_edit->popup_set_focus  = popup_set_focus;
    item_edit->popup_post_show  = popup_post_show;
    item_edit->popup_get_width  = popup_get_width;
    item_edit->popup_user_data  = popup_user_data;

    if (item_edit->is_popup)
        connect_popup_toggle_signals (item_edit);
    else
    {
        disconnect_popup_toggle_signals (item_edit);

        gnc_item_edit_hide_popup (item_edit);
        gtk_widget_hide (item_edit->popup_toggle.ebox);
    }
}

gboolean
gnc_item_edit_get_has_selection (GncItemEdit *item_edit)
{
    GtkEditable *editable;

    g_return_val_if_fail ((item_edit != NULL), FALSE);
    g_return_val_if_fail (GNC_IS_ITEM_EDIT (item_edit), FALSE);

    editable = GTK_EDITABLE (item_edit->editor);
    return gtk_editable_get_selection_bounds(editable, NULL, NULL);
}

