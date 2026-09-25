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
#include <qof.h>

#include "gnucash-color.h"
#include "gnucash-cursor.h"
#include "gnucash-item-edit.h"
#include "gnucash-sheet.h"
#include "gnucash-sheetP.h"
#include "gnucash-style.h"

#include "gnc-ui-util.h"
#include "gnc-gtk-utils.h"

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

static void gnc_item_edit_destroying (GtkWidget *this, gpointer data);

G_DEFINE_TYPE (GncItemEdit, gnc_item_edit, GTK_TYPE_BOX)

G_DEFINE_TYPE (GncItemEditTb, gnc_item_edit_tb, GTK_TYPE_TOGGLE_BUTTON)

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
    GncItemEditTb *item_edit_tb = GNC_ITEM_EDIT_TB(object);

    switch (param_id)
    {
    case PROP_SHEET:
        g_value_take_object (value, item_edit_tb->sheet);
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, param_id, pspec);
        break;
    }
}

static void
gnc_item_edit_tb_set_property (GObject *object,
                               guint param_id,
                               const GValue *value,
                               GParamSpec *pspec)
{
    GncItemEditTb *item_edit_tb = GNC_ITEM_EDIT_TB(object);

    switch (param_id)
    {
    case PROP_SHEET:
        item_edit_tb->sheet = GNUCASH_SHEET(g_value_get_object (value));
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, param_id, pspec);
        break;
    }
}

static void
gnc_item_edit_tb_measure (GtkWidget *widget, GtkOrientation orientation,
                          G_GNUC_UNUSED int for_size, int *minimum,
                          int *natural, int *minimum_baseline,
                          int *natural_baseline)
{
    GncItemEditTb *tb = GNC_ITEM_EDIT_TB (widget);
    GncItemEdit *item_edit = GNC_ITEM_EDIT (tb->sheet->item_editor);
    gint x, y, w, h = 2;
    gint size;

    gnc_item_edit_get_pixel_coords (item_edit, &x, &y, &w, &h);
    if (orientation == GTK_ORIENTATION_HORIZONTAL)
    {
        size = MAX (MIN_BUTT_WIDTH, ((h - 2) * 2) / 3);
        item_edit->button_width = size;
    }
    else
        size = MAX (1, h - 2);

    *minimum = size;
    *natural = size;
    if (minimum_baseline)
        *minimum_baseline = -1;
    if (natural_baseline)
        *natural_baseline = -1;
}
static void
gnc_item_edit_tb_class_init (GncItemEditTbClass *gnc_item_edit_tb_class)
{
    GObjectClass  *object_class;
    GtkWidgetClass *widget_class;

    gtk_widget_class_set_css_name (GTK_WIDGET_CLASS(gnc_item_edit_tb_class), "button");

    object_class = G_OBJECT_CLASS(gnc_item_edit_tb_class);
    widget_class = GTK_WIDGET_CLASS(gnc_item_edit_tb_class);

    object_class->get_property = gnc_item_edit_tb_get_property;
    object_class->set_property = gnc_item_edit_tb_set_property;

    g_object_class_install_property (object_class,
                                     PROP_SHEET,
                                     g_param_spec_object ("sheet",
                                             "Sheet Value",
                                             "Sheet Value",
                                             GNUCASH_TYPE_SHEET,
                                             G_PARAM_READWRITE));

    widget_class->measure = gnc_item_edit_tb_measure;
}

GtkWidget *
gnc_item_edit_tb_new (GnucashSheet *sheet)
{
    GncItemEditTb *item_edit_tb = g_object_new (GNC_TYPE_ITEM_EDIT_TB,
                                                "sheet", sheet,
                                                NULL);

    gtk_widget_add_css_class (GTK_WIDGET (item_edit_tb), "button");
    return GTK_WIDGET (item_edit_tb);
}

static void
tb_click_pressed_cb (GtkGestureClick *gesture, G_GNUC_UNUSED gint n_press,
                     G_GNUC_UNUSED double x, G_GNUC_UNUSED double y,
                     G_GNUC_UNUSED gpointer user_data)
{
    if (gtk_gesture_single_get_current_button (GTK_GESTURE_SINGLE (gesture)) ==
        GDK_BUTTON_SECONDARY)
        gtk_gesture_set_state (GTK_GESTURE (gesture),
                               GTK_EVENT_SEQUENCE_CLAIMED);
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

    gnucash_sheet_style_get_cell_pixel_rel_coords (item_edit->style,
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
    gnucash_sheet_overlay_move (item_edit->sheet,
                                GTK_WIDGET (item_edit), x, y);

    if (item_edit->is_popup)
    {
        gtk_widget_set_visible (item_edit->popup_toggle.ebox, TRUE);
        if (item_edit->show_popup)
            gnc_item_edit_show_popup (item_edit);
    }
    return FALSE;
}

void
gnc_item_edit_focus_in (GncItemEdit *item_edit)
{
    g_return_if_fail (GNC_IS_ITEM_EDIT (item_edit));

    if (!item_edit->show_popup && gtk_widget_get_visible (item_edit->editor))
        gtk_widget_grab_focus (item_edit->editor);
}

void
gnc_item_edit_focus_out (GncItemEdit *item_edit)
{
    g_return_if_fail (GNC_IS_ITEM_EDIT (item_edit));

    /* GTK4 sends focus through the real entry. Popups retain their own focus
     * while visible, so no synthetic focus event is needed. */
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
    item_edit->preedit_length = 0;

    item_edit->is_popup = FALSE;
    item_edit->show_popup = FALSE;

    item_edit->popup_toggle.ebox = NULL;
    item_edit->popup_toggle.tbutton = NULL;
    item_edit->popup_toggle.icon = NULL;
    item_edit->popup_toggle.arrow_down = TRUE;
    item_edit->popup_toggle.signals_connected = FALSE;

    item_edit->popup_item = NULL;
    item_edit->popup_get_height = NULL;
    item_edit->popup_autosize = NULL;
    item_edit->popup_set_focus = NULL;
    item_edit->popup_post_show = NULL;
    item_edit->popup_user_data = NULL;
    item_edit->popup_returned_height = 0;
    item_edit->popup_height_signal_id = 0;
    item_edit->popup_allocation_height = -1;

    item_edit->style = NULL;
    item_edit->button_width = MIN_BUTT_WIDTH;

    gnc_virtual_location_init (&item_edit->virt_loc);
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

    item_edit->style = gnucash_sheet_get_style (sheet,
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
    gtk_entry_set_alignment (GTK_ENTRY(item_edit->editor), xalign);

    if (!gnc_table_is_popup (sheet->table, item_edit->virt_loc))
        gnc_item_edit_set_popup (item_edit, NULL, NULL, NULL,
                                 NULL, NULL, NULL, NULL);

    g_idle_add_full (G_PRIORITY_HIGH_IDLE,
                    (GSourceFunc)gnc_item_edit_update, item_edit, NULL);
}


void
gnc_item_edit_cut_clipboard (GncItemEdit *item_edit)
{
    gtk_widget_activate_action (item_edit->editor, "clipboard.cut", NULL);
}

void
gnc_item_edit_copy_clipboard (GncItemEdit *item_edit)
{
    gtk_widget_activate_action (item_edit->editor, "clipboard.copy", NULL);
}

typedef struct
{
    GWeakRef item_edit;
} ItemEditPasteRequest;

static void
item_edit_paste_request_free (ItemEditPasteRequest *request)
{
    g_weak_ref_clear (&request->item_edit);
    g_free (request);
}

static void
item_edit_paste_text_ready_cb (GObject *source, GAsyncResult *result,
                               gpointer user_data)
{
    ItemEditPasteRequest *request = user_data;
    GncItemEdit *item_edit = g_weak_ref_get (&request->item_edit);
    GError *error = NULL;
    char *text = gdk_clipboard_read_text_finish (GDK_CLIPBOARD (source), result,
                                                 &error);

    if (item_edit && text &&

        !gnc_table_control_input_suspended (item_edit->sheet->table->control))
    {
        char *filtered_text = gnc_filter_text_for_control_chars (text);
        if (filtered_text)
        {
            GtkEditable *editable = GTK_EDITABLE (item_edit->editor);
            gint position = gtk_editable_get_position (editable);
            gint start_pos;
            gint end_pos;

            if (gtk_editable_get_selection_bounds (editable, &start_pos, &end_pos))
            {
                position = start_pos;
                gtk_editable_delete_selection (editable);
            }
            gtk_editable_insert_text (editable, filtered_text, -1, &position);
            gtk_editable_set_position (editable, position);
            g_free (filtered_text);
        }
    }

    g_clear_error (&error);
    g_free (text);
    g_clear_object (&item_edit);
    item_edit_paste_request_free (request);
}

void
gnc_item_edit_paste_clipboard (GncItemEdit *item_edit)
{
    ItemEditPasteRequest *request;
    GdkClipboard *clipboard;

    g_return_if_fail (GNC_IS_ITEM_EDIT (item_edit));
    if (gnc_table_control_input_suspended (item_edit->sheet->table->control))
        return;
    clipboard = gtk_widget_get_clipboard (item_edit->editor);
    if (!clipboard)
        return;

    request = g_new0 (ItemEditPasteRequest, 1);
    g_weak_ref_init (&request->item_edit, item_edit);
    gdk_clipboard_read_text_async (clipboard, NULL,
                                   item_edit_paste_text_ready_cb, request);
}



static void
gnc_item_edit_update_popup_icon (GncItemEdit *item_edit)
{
    if (!item_edit->popup_toggle.icon)
        return;

    gtk_image_set_from_icon_name
        (GTK_IMAGE (item_edit->popup_toggle.icon),
         item_edit->popup_toggle.arrow_down ? "pan-down-symbolic" :
                                              "pan-up-symbolic");
}

typedef struct
{
    GWeakRef item_edit;
    VirtualLocation virt_loc;
} GncItemEditPopupReplay;

static void
gnc_item_edit_popup_replay_destroy (gpointer user_data)
{
    GncItemEditPopupReplay *replay = user_data;

    g_weak_ref_clear (&replay->item_edit);
    g_free (replay);
}

static void
gnc_item_edit_popup_replay (Table *table, gpointer user_data)
{
    GncItemEditPopupReplay *replay = user_data;
    GncItemEdit *item_edit =
        GNC_ITEM_EDIT (g_weak_ref_get (&replay->item_edit));

    if (item_edit && item_edit->sheet &&
        item_edit->sheet->table == table &&
        virt_loc_equal (table->current_cursor_loc, replay->virt_loc) &&
        item_edit->popup_toggle.tbutton)
        gtk_toggle_button_set_active
            (GTK_TOGGLE_BUTTON (item_edit->popup_toggle.tbutton), TRUE);
    g_clear_object (&item_edit);
}

static void
gnc_item_edit_defer_popup_replay (GncItemEdit *item_edit,
                                  VirtualLocation virt_loc)
{
    GncItemEditPopupReplay *replay = g_new0 (GncItemEditPopupReplay, 1);

    replay->virt_loc = virt_loc;
    g_weak_ref_init (&replay->item_edit, item_edit);
    gnc_table_confirm_change_set_replay (item_edit->sheet->table,
                                         gnc_item_edit_popup_replay, replay,
                                         gnc_item_edit_popup_replay_destroy);
}

static void
gnc_item_edit_popup_toggled (GtkToggleButton *button, gpointer data)
{
    GncItemEdit *item_edit = GNC_ITEM_EDIT (data);
    gboolean show_popup = gtk_toggle_button_get_active (button);

    if (show_popup &&
        gnc_table_control_input_suspended (item_edit->sheet->table->control))
    {
        g_signal_handlers_block_matched (button, G_SIGNAL_MATCH_DATA,
                                         0, 0, NULL, NULL, data);
        gtk_toggle_button_set_active (button, FALSE);
        g_signal_handlers_unblock_matched (button, G_SIGNAL_MATCH_DATA,
                                           0, 0, NULL, NULL, data);
        return;
    }

    if (show_popup)
    {
        VirtualLocation virt_loc = item_edit->sheet->table->current_cursor_loc;
        GncTableConfirmResult confirmation =
            gnc_table_confirm_change (item_edit->sheet->table, virt_loc);

        if (confirmation != GNC_TABLE_CONFIRM_ACCEPT)
        {
            if (confirmation == GNC_TABLE_CONFIRM_DEFERRED)
                gnc_item_edit_defer_popup_replay (item_edit, virt_loc);
            g_signal_handlers_block_matched (button, G_SIGNAL_MATCH_DATA,
                                             0, 0, NULL, NULL, data);
            gtk_toggle_button_set_active (button, FALSE);
            g_signal_handlers_unblock_matched (button, G_SIGNAL_MATCH_DATA,
                                               0, 0, NULL, NULL, data);
            return;
        }
    }

    item_edit->show_popup = show_popup;
    if (!show_popup)
        gnc_item_edit_hide_popup (item_edit);
    gnc_item_edit_configure (item_edit);
}

static void
block_toggle_signals (GncItemEdit *item_edit)
{
    if (item_edit->popup_toggle.signals_connected)
        g_signal_handlers_block_matched (item_edit->popup_toggle.tbutton,
                                         G_SIGNAL_MATCH_DATA, 0, 0, NULL, NULL,
                                         item_edit);
}

static void
unblock_toggle_signals (GncItemEdit *item_edit)
{
    if (item_edit->popup_toggle.signals_connected)
        g_signal_handlers_unblock_matched (item_edit->popup_toggle.tbutton,
                                           G_SIGNAL_MATCH_DATA, 0, 0, NULL, NULL,
                                           item_edit);
}

static void
connect_popup_toggle_signals (GncItemEdit *item_edit)
{
    if (item_edit->popup_toggle.signals_connected)
        return;

    g_signal_connect (item_edit->popup_toggle.tbutton, "toggled",
                      G_CALLBACK (gnc_item_edit_popup_toggled), item_edit);
    item_edit->popup_toggle.signals_connected = TRUE;
    gnc_item_edit_update_popup_icon (item_edit);
}

static void
disconnect_popup_toggle_signals (GncItemEdit *item_edit)
{
    if (!item_edit->popup_toggle.signals_connected)
        return;

    g_signal_handlers_disconnect_matched (item_edit->popup_toggle.tbutton,
                                          G_SIGNAL_MATCH_DATA, 0, 0, NULL, NULL,
                                          item_edit);
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
    GncItemEdit *item_edit = GNC_ITEM_EDIT(object);

    switch (param_id)
    {
    case PROP_SHEET:
        g_value_take_object (value, item_edit->sheet);
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, param_id, pspec);
        break;
    }
}

static void
gnc_item_edit_set_property (GObject *object,
                            guint param_id,
                            const GValue *value,
                            GParamSpec *pspec)
{
    GncItemEdit *item_edit = GNC_ITEM_EDIT(object);
    switch (param_id)
    {
    case PROP_SHEET:
        item_edit->sheet = GNUCASH_SHEET(g_value_get_object (value));
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, param_id, pspec);
        break;
    }
}

static void
gnc_item_edit_measure (GtkWidget *widget, GtkOrientation orientation,
                       G_GNUC_UNUSED int for_size, int *minimum,
                       int *natural, int *minimum_baseline,
                       int *natural_baseline)
{
    gint x, y, w = 1, h = 1;

    gnc_item_edit_get_pixel_coords (GNC_ITEM_EDIT (widget), &x, &y, &w, &h);
    *minimum = orientation == GTK_ORIENTATION_HORIZONTAL ? MAX (1, w - 1) :
                                                            MAX (1, h - 1);
    *natural = *minimum;
    if (minimum_baseline)
        *minimum_baseline = -1;
    if (natural_baseline)
        *natural_baseline = -1;
}
/*
 * GncItemEdit class initialization
 */
static void
gnc_item_edit_class_init (GncItemEditClass *gnc_item_edit_class)
{
    GObjectClass  *object_class;
    GtkWidgetClass *widget_class;

    gtk_widget_class_set_css_name (GTK_WIDGET_CLASS(gnc_item_edit_class), "gnc-id-cursor");

    object_class = G_OBJECT_CLASS(gnc_item_edit_class);
    widget_class = GTK_WIDGET_CLASS(gnc_item_edit_class);

    object_class->get_property = gnc_item_edit_get_property;
    object_class->set_property = gnc_item_edit_set_property;

    g_object_class_install_property (object_class,
                                     PROP_SHEET,
                                     g_param_spec_object ("sheet",
                                             "Sheet Value",
                                             "Sheet Value",
                                             GNUCASH_TYPE_SHEET,
                                             G_PARAM_READWRITE));

    widget_class->measure = gnc_item_edit_measure;
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
    if (item_edit && gtk_widget_get_visible (item_edit->popup_toggle.tbutton))
        return item_edit->button_width;
    return MIN_BUTT_WIDTH;
}

static void
editor_click_pressed_cb (GtkGestureClick *gesture, G_GNUC_UNUSED gint n_press,
                         G_GNUC_UNUSED double x, G_GNUC_UNUSED double y,
                         GncItemEdit *item_edit)
{
    if (gtk_gesture_single_get_current_button (GTK_GESTURE_SINGLE (gesture)) !=
        GDK_BUTTON_SECONDARY)
        return;

    if (gnc_table_control_input_suspended (item_edit->sheet->table->control))
        return;

    if (!item_edit->show_popup)
        g_signal_emit_by_name (item_edit->sheet->reg, "show_popup_menu");
    gtk_gesture_set_state (GTK_GESTURE (gesture), GTK_EVENT_SEQUENCE_CLAIMED);
}

GtkWidget *
gnc_item_edit_new (GnucashSheet *sheet)
{
    GncItemEdit *item_edit;
    GtkGesture *editor_click;
    GtkGesture *toggle_click;

    item_edit = g_object_new (GNC_TYPE_ITEM_EDIT,
                              "sheet", sheet,
                              "spacing", 0,
                              "homogeneous", FALSE,
                              NULL);
    gnucash_sheet_overlay_put (sheet, GTK_WIDGET (item_edit), 0, 0);

    item_edit->editor = gtk_entry_new ();
    sheet->entry = item_edit->editor;
    gtk_editable_set_width_chars (GTK_EDITABLE (item_edit->editor), 1);
    gtk_entry_set_has_frame (GTK_ENTRY (item_edit->editor), FALSE);
    gtk_widget_set_focusable (item_edit->editor, TRUE);
    gtk_widget_add_css_class (item_edit->editor,
                              "gnc-class-register-foreground");
    item_edit->padding = (GtkBorder) { 0, 0, 0, 0 };
    item_edit->margin = (GtkBorder) { 0, 0, 0, 0 };
    item_edit->border = (GtkBorder) { 0, 0, 0, 0 };
    gnc_box_append_full (GTK_BOX (item_edit), item_edit->editor, TRUE, TRUE, 0);

    editor_click = gtk_gesture_click_new ();
    gtk_gesture_single_set_button (GTK_GESTURE_SINGLE (editor_click),
                                   GDK_BUTTON_SECONDARY);
    g_signal_connect (editor_click, "pressed",
                      G_CALLBACK (editor_click_pressed_cb), item_edit);
    gtk_widget_add_controller (item_edit->editor,
                               GTK_EVENT_CONTROLLER (editor_click));

    item_edit->popup_toggle.tbutton = gnc_item_edit_tb_new (sheet);
    item_edit->popup_toggle.icon = gtk_image_new_from_icon_name
        ("pan-down-symbolic");
    gtk_button_set_child (GTK_BUTTON (item_edit->popup_toggle.tbutton),
                          item_edit->popup_toggle.icon);
    item_edit->popup_toggle.ebox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
    gtk_box_append (GTK_BOX (item_edit->popup_toggle.ebox),
                    item_edit->popup_toggle.tbutton);
    toggle_click = gtk_gesture_click_new ();
    gtk_gesture_single_set_button (GTK_GESTURE_SINGLE (toggle_click),
                                   GDK_BUTTON_SECONDARY);
    g_signal_connect (toggle_click, "pressed",
                      G_CALLBACK (tb_click_pressed_cb), NULL);
    gtk_widget_add_controller (item_edit->popup_toggle.ebox,
                               GTK_EVENT_CONTROLLER (toggle_click));
    gnc_box_append_full (GTK_BOX (item_edit), item_edit->popup_toggle.ebox,
                         FALSE, FALSE, 0);

    gtk_widget_set_visible (GTK_WIDGET (item_edit), TRUE);
    g_signal_connect (item_edit, "destroy",
                      G_CALLBACK (gnc_item_edit_destroying), NULL);
    return GTK_WIDGET (item_edit);
}
static void
gnc_item_edit_destroying (GtkWidget *item_edit, gpointer data)
{
    if (GNC_ITEM_EDIT(item_edit)->popup_height_signal_id > 0)
        g_signal_handler_disconnect (GNC_ITEM_EDIT(item_edit)->popup_item,
                                     GNC_ITEM_EDIT(item_edit)->popup_height_signal_id);

    while (g_idle_remove_by_data ((gpointer)item_edit))
        continue;
}

static void
check_popup_height_is_true (GtkWidget *widget,
                            G_GNUC_UNUSED GParamSpec *pspec,
                            gpointer user_data)
{
    GncItemEdit *item_edit = GNC_ITEM_EDIT (user_data);
    gint height = gtk_widget_get_height (widget);

    if (height <= 0 || height == item_edit->popup_returned_height ||
        gtk_widget_get_parent (widget) != GTK_WIDGET (item_edit->sheet))
        return;

    item_edit->popup_allocation_height = height;
    gnucash_sheet_overlay_remove (item_edit->sheet, widget);
    g_idle_add_full (G_PRIORITY_HIGH_IDLE, (GSourceFunc) gnc_item_edit_update,
                     item_edit, NULL);
}

void
gnc_item_edit_show_popup (GncItemEdit *item_edit)
{
    GtkToggleButton *toggle;
    GtkAdjustment *vadj, *hadj;
    GnucashSheet *sheet;
    gint x = 0, y = 0, w = 0, h = 0;
    gint y_offset, x_offset;
    gint popup_x, popup_y;
    gint popup_w = -1, popup_h = -1;
    gint popup_max_width, popup_max_height;
    gint view_height;
    gint down_height, up_height;
    gint sheet_width;

    g_return_if_fail (item_edit != NULL);
    g_return_if_fail (GNC_IS_ITEM_EDIT(item_edit));

    if (!item_edit->is_popup)
        return;

    sheet = item_edit->sheet;

    sheet_width = sheet->width;

    view_height = gtk_widget_get_height (GTK_WIDGET (sheet));

    vadj = gtk_scrollable_get_vadjustment (GTK_SCROLLABLE(sheet));
    hadj = gtk_scrollable_get_hadjustment (GTK_SCROLLABLE(sheet));

    y_offset = gtk_adjustment_get_value (vadj);
    x_offset = gtk_adjustment_get_value (hadj);
    gnc_item_edit_get_pixel_coords (item_edit, &x, &y, &w, &h);

    popup_x = x;

    up_height = y - y_offset;
    down_height = view_height - (up_height + h);

    popup_max_height = MAX(up_height, down_height);
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
        popup_y = y - popup_h - 1;
    else
        popup_y = y + h;

    if (!gtk_widget_get_parent (item_edit->popup_item))
        gnucash_sheet_overlay_put (sheet, item_edit->popup_item, popup_x, popup_y);

    // Lets check popup height is the true height
    item_edit->popup_returned_height = popup_h;

    gint editor_width = gtk_widget_get_width (GTK_WIDGET (item_edit));

    // the calendar will be 0
    if ((popup_w != 0) && (popup_w < editor_width))
        popup_w = editor_width;

    if (popup_h == popup_max_height)
        gtk_widget_set_size_request (item_edit->popup_item, popup_w - 1, popup_h);
    else
        gtk_widget_set_size_request (item_edit->popup_item, popup_w - 1, -1);

    toggle = GTK_TOGGLE_BUTTON(item_edit->popup_toggle.tbutton);

    if (!gtk_toggle_button_get_active (toggle))
    {
        block_toggle_signals (item_edit);
        gtk_toggle_button_set_active (toggle, TRUE);
        unblock_toggle_signals (item_edit);
    }

    // set the popup arrow direction up
    item_edit->popup_toggle.arrow_down = FALSE;
    gnc_item_edit_update_popup_icon (item_edit);
    item_edit->show_popup = TRUE;

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
            popup_x = MAX(0, popup_x);
        }
        else
            popup_x = x;

        gnucash_sheet_overlay_move (sheet, item_edit->popup_item, popup_x, popup_y);
    }
}


void
gnc_item_edit_hide_popup (GncItemEdit *item_edit)
{
    g_return_if_fail (item_edit != NULL);
    g_return_if_fail (GNC_IS_ITEM_EDIT(item_edit));

    if (!item_edit->is_popup)
        return;

    if (gtk_widget_get_parent (GTK_WIDGET(item_edit->popup_item)) != GTK_WIDGET(item_edit->sheet))
        return;

    gnucash_sheet_overlay_remove (item_edit->sheet, item_edit->popup_item);

    // set the popup arrow direction down
    item_edit->popup_toggle.arrow_down = TRUE;
    gnc_item_edit_update_popup_icon (item_edit);

    gtk_toggle_button_set_active
        (GTK_TOGGLE_BUTTON(item_edit->popup_toggle.tbutton), FALSE);

    item_edit->popup_allocation_height = -1;

    gtk_widget_grab_focus (GTK_WIDGET(item_edit->sheet));
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

    /* setup size-allocate callback for popup_item height, done here as
       item_edit is constant and popup_item changes per cell */
    if (popup_item)
    {
        item_edit->popup_height_signal_id = g_signal_connect_after (
                                            popup_item, "notify::height",
                                            G_CALLBACK (check_popup_height_is_true),
                                            item_edit);
    }
    else
    {
        if (GNC_ITEM_EDIT(item_edit)->popup_height_signal_id > 0)
        {
            g_signal_handler_disconnect (item_edit->popup_item, item_edit->popup_height_signal_id);
            item_edit->popup_height_signal_id = 0;
        }
    }

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
        gtk_widget_set_visible (item_edit->popup_toggle.ebox, FALSE);
    }
}

gboolean
gnc_item_edit_get_has_selection (GncItemEdit *item_edit)
{
    GtkEditable *editable;

    g_return_val_if_fail ((item_edit != NULL), FALSE);
    g_return_val_if_fail (GNC_IS_ITEM_EDIT(item_edit), FALSE);

    editable = GTK_EDITABLE(item_edit->editor);
    return gtk_editable_get_selection_bounds (editable, NULL, NULL);
}
