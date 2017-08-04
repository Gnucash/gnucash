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


#include "config.h"

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

static GtkBoxClass *gnc_item_edit_parent_class;


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


int
gnc_item_edit_get_toggle_offset (int row_height)
{
    /* sync with gnc_item_edit_update */
    return row_height - (2 * (CELL_VPADDING + 1)) + 3;
}

static void
gnc_item_edit_update (GncItemEdit *item_edit)
{
    gint x, y, w, h;

//FIXME this does not appear to be realiable, widget does not always move to correct place
    gnc_item_edit_get_pixel_coords (item_edit, &x, &y, &w, &h);
    gtk_layout_move (GTK_LAYOUT(item_edit->sheet),
                     GTK_WIDGET(item_edit), x, y);
    gtk_widget_queue_resize (GTK_WIDGET (item_edit));

    if (item_edit->is_popup)
    {
        gtk_widget_show (item_edit->popup_toggle.ebox);
        if (item_edit->show_popup)
            gnc_item_edit_show_popup (item_edit);
    }
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
    gtk_orientable_set_orientation (GTK_ORIENTABLE(item_edit), GTK_ORIENTATION_HORIZONTAL);

    item_edit->sheet = NULL;
    item_edit->editor = NULL;

    item_edit->is_popup = FALSE;
    item_edit->show_popup = FALSE;

    item_edit->popup_toggle.ebox = NULL;
    item_edit->popup_toggle.tbutton = NULL;
    item_edit->popup_toggle.arrow_up = NULL;
    item_edit->popup_toggle.arrow_down = NULL;
    item_edit->popup_toggle.signals_connected = FALSE;

    item_edit->popup_item = NULL;
    item_edit->get_popup_height = NULL;
    item_edit->popup_autosize = NULL;
    item_edit->popup_set_focus = NULL;
    item_edit->popup_post_show = NULL;
    item_edit->popup_user_data = NULL;

    item_edit->style = NULL;

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

    gnc_item_edit_update (item_edit);
}


void
gnc_item_edit_cut_clipboard (GncItemEdit *item_edit)
{
    gtk_editable_set_editable(GTK_EDITABLE(item_edit->editor), TRUE);
    gtk_editable_cut_clipboard(GTK_EDITABLE(item_edit->editor));
    gtk_editable_set_editable(GTK_EDITABLE(item_edit->editor), FALSE);
}

void
gnc_item_edit_copy_clipboard (GncItemEdit *item_edit)
{
    gtk_editable_copy_clipboard(GTK_EDITABLE(item_edit->editor));
}

void
gnc_item_edit_paste_clipboard (GncItemEdit *item_edit)
{
    gtk_editable_set_editable(GTK_EDITABLE(item_edit->editor), TRUE);
    gtk_editable_paste_clipboard(GTK_EDITABLE(item_edit->editor));
    gtk_editable_set_editable(GTK_EDITABLE(item_edit->editor), FALSE);
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
    gint x, y, w, h;
    gnc_item_edit_get_pixel_coords (GNC_ITEM_EDIT (widget), &x, &y, &w, &h);
    *minimal_width = *natural_width = w - 1;
}


static void
gnc_item_edit_get_preferred_height (GtkWidget *widget,
                                    gint *minimal_width,
                                    gint *natural_width)
{
    gint x, y, w, h;
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


GtkWidget *
gnc_item_edit_new (GnucashSheet *sheet)
{
    char *hpad_str, *vpad_str, *entry_css;
    GtkWidget *box;
    GtkStyleContext *stylecontext;
    GtkCssProvider *provider;
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
    gtk_box_pack_start (GTK_BOX(item_edit), item_edit->editor,  TRUE, TRUE, 0);

    /* Force padding on the entry to align with the rest of the register */
    hpad_str = g_strdup_printf("%i", CELL_HPADDING);
    vpad_str = g_strdup_printf("%i", CELL_VPADDING);
    entry_css = g_strconcat ("* { padding: ", vpad_str, "px ", hpad_str, "px ", vpad_str, "px ", hpad_str, "px }", NULL);
    provider = gtk_css_provider_new();
    gtk_css_provider_load_from_data (provider, entry_css, -1, NULL);
    stylecontext = gtk_widget_get_style_context (item_edit->editor);
    gtk_style_context_add_provider (stylecontext, GTK_STYLE_PROVIDER (provider),
                                    GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);
    g_free (hpad_str);
    g_free (vpad_str);
    g_free (entry_css);

    /* Create the popup button
       It will only be displayed when the cell being edited provides
       a popup item (like a calendar or account list) */
    item_edit->popup_toggle.arrow_down = gtk_image_new_from_icon_name ("go-down", GTK_ICON_SIZE_BUTTON);
    item_edit->popup_toggle.arrow_up = gtk_image_new_from_icon_name ("go-up", GTK_ICON_SIZE_BUTTON);

    box = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
    gtk_box_pack_start (GTK_BOX(box), GTK_WIDGET(item_edit->popup_toggle.arrow_down), FALSE, FALSE, 0);
    gtk_box_pack_start (GTK_BOX(box), GTK_WIDGET(item_edit->popup_toggle.arrow_up),FALSE, FALSE, 0);

    item_edit->popup_toggle.tbutton = gtk_toggle_button_new();
    gtk_toggle_button_set_mode (
        GTK_TOGGLE_BUTTON (item_edit->popup_toggle.tbutton), FALSE);
    gtk_container_add(GTK_CONTAINER(item_edit->popup_toggle.tbutton), GTK_WIDGET(box));

    /* Force padding on the button to
       1. keep it small
       2. display as much as possible of the arrow */
    provider = gtk_css_provider_new();
    gtk_css_provider_load_from_data (provider, "* { padding: 1px }", -1, NULL);
    stylecontext = gtk_widget_get_style_context (item_edit->popup_toggle.tbutton);
    gtk_style_context_add_provider (stylecontext, GTK_STYLE_PROVIDER (provider),
                                    GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);

    /* Wrap the popup button in an event box to give it its own gdkwindow.
     * Without one the button would disappear behind the grid object. */
    item_edit->popup_toggle.ebox = gtk_event_box_new();
    g_object_ref(item_edit->popup_toggle.ebox);
    gtk_container_add(GTK_CONTAINER(item_edit->popup_toggle.ebox),
                      item_edit->popup_toggle.tbutton);

    gtk_box_pack_start (GTK_BOX(item_edit),
                        item_edit->popup_toggle.ebox,
                        FALSE, TRUE, 0);
    gtk_widget_show_all(GTK_WIDGET(item_edit));

    gtk_widget_hide (GTK_WIDGET(item_edit->popup_toggle.arrow_up));

    return GTK_WIDGET(item_edit);
}


void
gnc_item_edit_show_popup (GncItemEdit *item_edit)
{
    GtkToggleButton *toggle;
    GtkAdjustment *vadj, *hadj;
    GtkAllocation alloc;
    GnucashSheet *sheet;
    gint x, y, w, h;
    gint y_offset, x_offset;
    gint popup_x, popup_y;
    gint popup_w;
    gint popup_h;
    gint popup_max_width;
    gint view_height;
    gint view_width;
    gint up_height;
    gint down_height;

    g_return_if_fail (item_edit != NULL);
    g_return_if_fail (GNC_IS_ITEM_EDIT(item_edit));

    if (!item_edit->is_popup)
        return;

    sheet = item_edit->sheet;

    gtk_widget_get_allocation (GTK_WIDGET (sheet), &alloc);
    view_height = alloc.height;
    view_width  = alloc.width;

    vadj = gtk_scrollable_get_vadjustment(GTK_SCROLLABLE(sheet));
    hadj = gtk_scrollable_get_hadjustment(GTK_SCROLLABLE(sheet));

    y_offset = gtk_adjustment_get_value(vadj);
    x_offset = gtk_adjustment_get_value(hadj);
    gnc_item_edit_get_pixel_coords (item_edit, &x, &y, &w, &h);

    popup_x = x;

    up_height = y - y_offset;
    down_height = view_height - (up_height + h);

    if (up_height > down_height)
    {
        popup_y = y + y_offset;
        popup_h = up_height;
    }
    else
    {
        popup_y = y + h;
        popup_h = down_height;
    }

    popup_max_width = view_width - popup_x + x_offset;

    if (item_edit->get_popup_height)
        popup_h = item_edit->get_popup_height
                       (item_edit->popup_item, popup_h, h,
                        item_edit->popup_user_data);

    if (item_edit->popup_autosize)
        popup_w =
            item_edit->popup_autosize (item_edit->popup_item,
                                       popup_max_width,
                                       item_edit->popup_user_data);
    else
        popup_w = -1;

    if (up_height > down_height)
        popup_y = y - popup_h;

    if (!gtk_widget_get_parent (item_edit->popup_item))
        gtk_layout_put (GTK_LAYOUT(sheet), item_edit->popup_item,
                        popup_x, popup_y);
    gtk_widget_set_size_request(item_edit->popup_item, popup_w, popup_h);

    toggle = GTK_TOGGLE_BUTTON(item_edit->popup_toggle.tbutton);

    if (!gtk_toggle_button_get_active (toggle))
    {
        block_toggle_signals (item_edit);
        gtk_toggle_button_set_active (toggle, TRUE);
        unblock_toggle_signals (item_edit);
    }

    gtk_widget_hide (item_edit->popup_toggle.arrow_down);
    gtk_widget_show (item_edit->popup_toggle.arrow_up);

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

    gtk_widget_hide (item_edit->popup_toggle.arrow_up);
    gtk_widget_show (item_edit->popup_toggle.arrow_down);

    gtk_toggle_button_set_active
    (GTK_TOGGLE_BUTTON(item_edit->popup_toggle.tbutton), FALSE);

    gtk_widget_grab_focus (GTK_WIDGET (item_edit->sheet));
}

void
gnc_item_edit_set_popup (GncItemEdit    *item_edit,
                         GtkWidget      *popup_item,
                         GetPopupHeight  get_popup_height,
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
    item_edit->get_popup_height = get_popup_height;
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

