/********************************************************************\
 * completioncell-gnome.c -- completion combobox cell for gnome     *
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
 * FILE: completioncell-gnome.c
 *
 * FUNCTION: Implement gnome portion of a entry completion combo widget
 *           embedded in a table cell.
 *
 * HISTORY:
 * @author Copyright (c) 2023 Robert Fewell
 */

#include <config.h>

#include <string.h>
#include <stdbool.h>
#include <gdk/gdk.h>

#include "completioncell.h"
#include "gnc-prefs.h"
#include "gnc-completion-model.h"
#include "gnucash-item-edit.h"
#include "gnucash-item-list.h"
#include "gnucash-sheet.h"
#include "gnucash-sheetP.h"
#include "table-allgui.h"
#include "gnc-string-utils.h"
#include <gnc-unicode.h>

typedef struct _PopBox
{
    GnucashSheet* sheet;
    GncItemEdit*  item_edit;
    GncItemList*  item_list;

    GncCompletionModel *completion_model; // the logical menu entries
    GListStore* item_store; // GTK4 rows for the completion popup

    gchar*        newval; // string value to find
    gint          newval_len; // length of string value to find

    gboolean      signals_connected; // list signals connected
    gboolean      list_popped;  // list is popped up

    gboolean      autosize; // autosize the popup width

    gboolean      sort_enabled; // sort of list store enabled
    gboolean      register_is_reversed; // whether the register is reversed

    gboolean      strict; // text entry must be in the list
    gboolean      in_list_select; // item selected in the list


} PopBox;

#define DONT_TEXT N_("Don't autocomplete")

static void gnc_completion_cell_gui_realize (BasicCell* bcell, gpointer w);
static void gnc_completion_cell_gui_move (BasicCell* bcell);
static void gnc_completion_cell_gui_destroy (BasicCell* bcell);
static gboolean gnc_completion_cell_enter (BasicCell* bcell,
                                           int* cursor_position,
                                           int* start_selection,
                                           int* end_selection);
static void gnc_completion_cell_leave (BasicCell* bcell);
static void gnc_completion_cell_destroy (BasicCell* bcell);

BasicCell*
gnc_completion_cell_new (void)
{
    CompletionCell* cell = g_new0 (CompletionCell, 1);
    gnc_completion_cell_init (cell);
    return &cell->cell;
}

void
gnc_completion_cell_init (CompletionCell* cell)
{
    gnc_basic_cell_init (& (cell->cell));

    cell->cell.is_popup = TRUE;

    cell->cell.destroy = gnc_completion_cell_destroy;

    cell->cell.gui_realize = gnc_completion_cell_gui_realize;
    cell->cell.gui_destroy = gnc_completion_cell_gui_destroy;

    PopBox* box = g_new0 (PopBox, 1);

    box->sheet = NULL;
    box->item_edit = NULL;
    box->item_list = NULL;
    box->item_store = NULL;

    box->signals_connected = FALSE;
    box->list_popped = FALSE;
    box->autosize = FALSE;
    box->register_is_reversed = FALSE;

    box->sort_enabled = FALSE;

    cell->cell.gui_private = box;

    box->strict = FALSE;
    box->in_list_select = FALSE;

    box->completion_model = gnc_completion_model_new ();
}

static void
hide_popup (PopBox* box)
{
    gnc_item_edit_hide_popup (box->item_edit);
    box->list_popped = FALSE;
}

static void
select_item_cb (GncItemList* item_list, char* item_string, gpointer user_data)
{
    CompletionCell* cell = user_data;
    PopBox* box = cell->cell.gui_private;

    box->in_list_select = TRUE;
    gnucash_sheet_modify_current_cell (box->sheet, item_string);
    box->in_list_select = FALSE;

    hide_popup (box);
}

static gint
text_width (PangoLayout *layout)
{
    PangoRectangle logical_rect;
    pango_layout_set_width (layout, -1);
    pango_layout_get_pixel_extents (layout, NULL, &logical_rect);
    return logical_rect.width;
}

static void
horizontal_scroll_to_found_text (PopBox *box, const char *item_string,
                                 gint found_location)
{
    GtkWidget *view = gnc_item_list_get_view (box->item_list);
    gint view_width;
    gint scroll_point = 0;
    gchar *start_string;
    PangoLayout *layout;
    PangoAttrList *attributes;
    PangoAttribute *bold_weight;
    gint item_string_width;
    gint start_string_width;

    if (!item_string || found_location < 0 || !gtk_widget_get_mapped (view))
        return;

    view_width = gtk_widget_get_width (view);
    if (view_width <= 0)
        return;

    start_string = g_utf8_substring (item_string, 0,
                                     found_location + box->newval_len);
    layout = gtk_widget_create_pango_layout (view, item_string);
    attributes = pango_attr_list_new ();
    bold_weight = pango_attr_weight_new (PANGO_WEIGHT_BOLD);
    bold_weight->start_index = found_location;
    bold_weight->end_index = found_location + box->newval_len;
    pango_attr_list_insert (attributes, bold_weight);
    pango_layout_set_attributes (layout, attributes);
    item_string_width = text_width (layout);

    pango_layout_set_text (layout, start_string, -1);
    start_string_width = text_width (layout);

    pango_attr_list_unref (attributes);
    g_object_unref (layout);
    g_free (start_string);

    if (item_string_width > view_width)
        scroll_point = MAX (0, start_string_width - view_width / 2);

    gtk_adjustment_set_value (
        gtk_scrolled_window_get_hadjustment (box->item_list->scrollwin),
        scroll_point);
}

static void
change_item_cb (GncItemList *item_list, char *item_string, gpointer user_data)
{
    CompletionCell *cell = user_data;
    PopBox *box = cell->cell.gui_private;
    gint found_location;

    box->in_list_select = TRUE;
    gnucash_sheet_modify_current_cell (box->sheet, item_string);
    box->in_list_select = FALSE;

    found_location = gnc_item_list_get_selected_found_location (item_list);
    horizontal_scroll_to_found_text (box, item_string, found_location);
}

static void
activate_item_cb (GncItemList* item_list, char* item_string, gpointer user_data)
{
    CompletionCell* cell = user_data;
    PopBox* box = cell->cell.gui_private;
    hide_popup (box);
}

static void
block_list_signals (CompletionCell* cell)
{
    PopBox* box = cell->cell.gui_private;

    if (!box->signals_connected)
        return;

    g_signal_handlers_block_matched (G_OBJECT(box->item_list),
                                     G_SIGNAL_MATCH_DATA,
                                     0, 0, NULL, NULL, cell);
}

static void
unblock_list_signals (CompletionCell* cell)
{
    PopBox* box = cell->cell.gui_private;

    if (!box->signals_connected)
        return;

    g_signal_handlers_unblock_matched (G_OBJECT(box->item_list),
                                       G_SIGNAL_MATCH_DATA,
                                       0, 0, NULL, NULL, cell);
}

static gboolean
key_press_item_cb (G_GNUC_UNUSED GncItemList *item_list,
                   guint keyval,
                   G_GNUC_UNUSED guint keycode,
                   G_GNUC_UNUSED GdkModifierType state,
                   gpointer user_data)
{
    CompletionCell *cell = user_data;
    PopBox *box = cell->cell.gui_private;

    if (keyval != GDK_KEY_Escape)
        return FALSE;

    block_list_signals (cell);
    gnc_item_list_select (box->item_list, NULL);
    unblock_list_signals (cell);
    hide_popup (box);
    return TRUE;
}

static void
completion_disconnect_signals (CompletionCell* cell)
{
    PopBox* box = cell->cell.gui_private;

    if (!box->signals_connected)
        return;

    g_signal_handlers_disconnect_matched (G_OBJECT(box->item_list),
                                          G_SIGNAL_MATCH_DATA,
                                          0, 0, NULL, NULL, cell);

    box->signals_connected = FALSE;
}

static void
completion_connect_signals (CompletionCell* cell)
{
    PopBox* box = cell->cell.gui_private;

    if (box->signals_connected)
        return;

    g_signal_connect (G_OBJECT(box->item_list), "select_item",
                      G_CALLBACK(select_item_cb), cell);

    g_signal_connect (G_OBJECT(box->item_list), "change_item",
                      G_CALLBACK(change_item_cb), cell);

    g_signal_connect (G_OBJECT(box->item_list), "activate_item",
                      G_CALLBACK(activate_item_cb), cell);

    g_signal_connect (G_OBJECT(box->item_list), "key-pressed",
                      G_CALLBACK(key_press_item_cb), cell);

    box->signals_connected = TRUE;
}

static void
gnc_completion_cell_gui_destroy (BasicCell* bcell)
{
    CompletionCell* cell = (CompletionCell*) bcell;

    if (!cell->cell.gui_realize)
    {
        PopBox* box = bcell->gui_private;
        if (box)
        {
            if (box->item_list)
            {
                completion_disconnect_signals (cell);
                g_object_unref (box->item_list);
                box->item_list = NULL;
            }
            if (box->item_store)
            {
                g_object_unref (box->item_store);
                box->item_store = NULL;
            }
        }
        /* allow the widget to be shown again */
        cell->cell.gui_realize = gnc_completion_cell_gui_realize;
        cell->cell.gui_move = NULL;
        cell->cell.enter_cell = NULL;
        cell->cell.leave_cell = NULL;
        cell->cell.gui_destroy = NULL;
    }
}

static void
gnc_completion_cell_destroy (BasicCell* bcell)
{
    CompletionCell* cell = (CompletionCell*) bcell;
    PopBox* box = cell->cell.gui_private;

    gnc_completion_cell_gui_destroy (& (cell->cell));

    if (box)
    {
        g_clear_object (&box->completion_model);

        g_free (box);
        cell->cell.gui_private = NULL;
    }
    cell->cell.gui_private = NULL;
    cell->cell.gui_realize = NULL;
}

void
gnc_completion_cell_set_sort_enabled (CompletionCell* cell,
                                      gboolean enabled)
{
    if (!cell)
        return;

    PopBox* box = cell->cell.gui_private;
    box->sort_enabled = enabled;
}

static void
item_store_clear (CompletionCell *cell)
{
    PopBox *box;

    if (!cell)
        return;

    box = cell->cell.gui_private;
    block_list_signals (cell);
    gnc_item_list_store_clear (box->item_store);
    unblock_list_signals (cell);
    hide_popup (box);
}

void
gnc_completion_cell_clear_menu (CompletionCell* cell)
{
    PopBox* box;

    if (!cell)
        return;

    box = cell->cell.gui_private;
    if (!box)
        return;

    gnc_completion_model_clear (box->completion_model);
    if (box->item_list)
        item_store_clear (cell);
}

void
gnc_completion_cell_add_menu_item (CompletionCell* cell,
                                   const char* menustr)
{
    PopBox* box;

    if (!cell || !menustr)
        return;

    box = cell->cell.gui_private;
    gnc_completion_model_add_menu_item (box->completion_model, menustr);
}

void
gnc_completion_cell_set_value (CompletionCell* cell, const char* str)
{
    if (!cell || !str)
        return;

    gnc_basic_cell_set_value (&cell->cell, str);
}

static inline void
item_store_append (GListStore *store, const gchar *string,
                   const gchar *markup, gint weight, gint found_location)
{
    g_return_if_fail (G_IS_LIST_STORE (store));
    g_return_if_fail (string != NULL);

    gnc_item_list_store_append (store, string, markup, weight, found_location);
}

static void
select_first_entry_in_list (PopBox *box)
{
    if (gnc_item_list_num_entries (box->item_list) < 2)
        return;

    /* The first suggestion is the explicit "Don't autocomplete" choice. */
    gnc_item_list_select_at (box->item_list, 1);
    gnc_item_list_show_selected (box->item_list);
}

static void
populate_item_model (CompletionCell* cell, gchar* str)
{
    PopBox* box = cell->cell.gui_private;
    GListModel *suggestions;
    guint n_suggestions;

    box->in_list_select = FALSE;
    box->item_edit->popup_allocation_height = -1;

    if (!str || !*str)
        return;

    g_free (box->newval);
    box->newval = g_strdup (str);
    box->newval_len = g_utf8_strlen (str, -1);
    suggestions = gnc_completion_model_build_suggestions (
        box->completion_model, box->newval, DONT_TEXT, box->sort_enabled);
    n_suggestions = g_list_model_get_n_items (suggestions);
    block_list_signals (cell);
    gnc_item_list_store_clear (box->item_store);

    for (guint i = 0; i < n_suggestions; i++)
    {
        GncSuggestionItem *item = g_list_model_get_item (suggestions, i);

        item_store_append (box->item_store,
                           gnc_suggestion_item_get_text (item),
                           gnc_suggestion_item_get_markup (item),
                           gnc_suggestion_item_get_weight (item),
                           gnc_suggestion_item_get_found_location (item));
        g_object_unref (item);
    }

    unblock_list_signals (cell);
    g_object_unref (suggestions);

    if (n_suggestions == 1)
        hide_popup (box);
    else
        gnc_item_edit_show_popup (box->item_edit);

    block_list_signals (cell);
    select_first_entry_in_list (box);
    unblock_list_signals (cell);
    g_clear_pointer (&box->newval, g_free);
}
static void
gnc_completion_cell_modify_verify (BasicCell* bcell,
                                   const char* change,
                                   int change_len,
                                   const char* newval,
                                   int newval_len,
                                   int* cursor_position,
                                   int* start_selection,
                                   int* end_selection)
{
    CompletionCell* cell = (CompletionCell*) bcell;
    PopBox* box = cell->cell.gui_private;

    if (box->in_list_select)
    {
        if (g_strcmp0 (newval, DONT_TEXT) == 0)
            return;
        gnc_basic_cell_set_value_internal (bcell, newval);
        *cursor_position = -1;
        *start_selection = 0;
        *end_selection = 0;
        return;
    }

    // Are were deleting or inserting in the middle.
    if (change == NULL || *cursor_position < bcell->value_chars)
        *start_selection = *end_selection = *cursor_position;

    gchar *start_of_text = g_utf8_substring (newval, 0, *cursor_position);
    populate_item_model (cell, start_of_text);
    g_free (start_of_text);

    if (g_strcmp0 (newval, "") == 0)
    {
        block_list_signals (cell); // Prevent recursion, unselect all
        gnc_item_list_select (box->item_list, NULL);
        unblock_list_signals (cell);
        hide_popup (box);
    }
    gnc_basic_cell_set_value_internal (bcell, newval);
}

static char*
get_entry_from_model_if_size_is_one (CompletionCell* cell)
{
    PopBox* box;

    if (!cell)
        return NULL;

    box = cell->cell.gui_private;
    return gnc_completion_model_dup_only_item (box->completion_model);
}

static gboolean
gnc_completion_cell_direct_update (BasicCell* bcell,
                                   int* cursor_position,
                                   int* start_selection,
                                   int* end_selection,
                                   const GncRegisterInput *input)
{
    CompletionCell* cell = (CompletionCell*) bcell;
    PopBox* box = cell->cell.gui_private;

    if (!input->pressed)
        return FALSE;

    switch (input->key)
    {
    case GNC_REGISTER_KEY_ESCAPE:
        if (bcell->changed)
        {
            const char *value = gnc_table_get_model_entry (box->sheet->table, bcell->cell_name);

            gnc_basic_cell_set_value_internal (bcell, value);
            bcell->changed = FALSE;
            *cursor_position = 0;
            *start_selection = 0;
            *end_selection = -1;
            return TRUE;
        }
        break;
    case GNC_REGISTER_KEY_TAB:
    case GNC_REGISTER_KEY_LEFT_TAB:
        {
            if (input->modifiers & GNC_REGISTER_MODIFIER_CONTROL)
            {
                char* hash_string = get_entry_from_model_if_size_is_one (cell);

                if (hash_string)
                {
                    gnc_basic_cell_set_value_internal (bcell, hash_string);
                    *cursor_position = strlen (hash_string);
                }
                g_free (hash_string);
                return TRUE;
            }

            char* string = gnc_item_list_get_selection (box->item_list);

            if (!string)
                break;

            g_signal_emit_by_name (G_OBJECT(box->item_list), "change_item",
                                   string, (gpointer)bcell);

            g_free (string);
            break;
        }
    default:
        break;
    }

    if (box->strict)
        box->in_list_select = gnc_item_in_list (box->item_list, bcell->value);

    if (!bcell->value)
        item_store_clear (cell);

    return FALSE;
}

void
gnc_completion_cell_reverse_sort (CompletionCell* cell, gboolean is_reversed)
{
    PopBox* box;

    if (!cell)
        return;

    box = cell->cell.gui_private;
    if (is_reversed == box->register_is_reversed)
        return;

    gnc_completion_model_set_reversed (box->completion_model, is_reversed);
    if (box->item_list)
        item_store_clear (cell);
    box->register_is_reversed = is_reversed;
}

static void
gnc_completion_cell_gui_realize (BasicCell* bcell, gpointer data)
{
    GnucashSheet* sheet = data;
    GncItemEdit* item_edit = gnucash_sheet_get_item_edit (sheet);
    CompletionCell* cell = (CompletionCell*) bcell;
    PopBox* box = cell->cell.gui_private;

    /* initialize gui-specific, private data */
    box->sheet = sheet;
    box->item_edit = item_edit;
    box->item_store = gnc_item_list_store_new ();
    box->item_list = GNC_ITEM_LIST(gnc_item_list_new (box->item_store));

    gtk_widget_set_visible (GTK_WIDGET(box->item_list), TRUE);
    g_object_ref_sink (box->item_list);

    /* to mark cell as realized, remove the realize method */
    cell->cell.gui_realize = NULL;
    cell->cell.gui_move = gnc_completion_cell_gui_move;
    cell->cell.enter_cell = gnc_completion_cell_enter;
    cell->cell.leave_cell = gnc_completion_cell_leave;
    cell->cell.gui_destroy = gnc_completion_cell_gui_destroy;
    cell->cell.modify_verify = gnc_completion_cell_modify_verify;
    cell->cell.direct_update = gnc_completion_cell_direct_update;
}

static void
reset_item_list_to_default_setup (BasicCell* bcell)
{
    PopBox* box = bcell->gui_private;
    PopupToggle popup_toggle;

    item_store_clear ((CompletionCell*) bcell);

    popup_toggle = box->item_edit->popup_toggle;
    gtk_widget_set_sensitive (GTK_WIDGET(popup_toggle.tbutton), TRUE);
    gtk_widget_set_visible (GTK_WIDGET(popup_toggle.tbutton), TRUE);

    box->list_popped = FALSE;
}

static void
gnc_completion_cell_gui_move (BasicCell* bcell)
{
    PopBox* box = bcell->gui_private;

    completion_disconnect_signals ((CompletionCell*) bcell);

    gnc_item_edit_set_popup (box->item_edit, NULL, NULL,
                             NULL, NULL, NULL, NULL, NULL);

    reset_item_list_to_default_setup (bcell);
}

static int
popup_get_height (GtkWidget* widget,
                  int space_available,
                  G_GNUC_UNUSED int row_height,
                  gpointer user_data)
{
    PopBox* box = user_data;
    GtkScrolledWindow* scrollwin = GNC_ITEM_LIST(widget)->scrollwin;
    int height;

    // if popup_allocation_height set use that
    if (box->item_edit->popup_allocation_height != -1)
        height = box->item_edit->popup_allocation_height;
    else
        height = gnc_item_list_get_popup_height (GNC_ITEM_LIST(widget));

    if (height < space_available)
    {
        // if the list is empty height would be 0 so return 1 instead to
        // satisfy the check_popup_height_is_true function
        gint ret_height = height ? height : 1;

        gtk_widget_set_size_request (GTK_WIDGET(scrollwin), -1, ret_height);
        gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW(scrollwin),
                                        GTK_POLICY_AUTOMATIC, GTK_POLICY_NEVER);
        return ret_height;
    }
    else
        gtk_widget_set_size_request (GTK_WIDGET(scrollwin), -1, -1);

    gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW(scrollwin),
                                    GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
    return space_available;
}

static int
popup_autosize (GtkWidget* widget,
                int max_width,
                gpointer user_data)
{
    PopBox* box = user_data;

    if (!box || !box->autosize)
        return max_width;

    return gnc_item_list_autosize (GNC_ITEM_LIST(widget)) + 20;
}

static void
popup_set_focus (GtkWidget *widget,
                 G_GNUC_UNUSED gpointer user_data)
{
    if (gnc_item_list_num_entries (GNC_ITEM_LIST (widget)))
        gtk_widget_grab_focus (gnc_item_list_get_view (GNC_ITEM_LIST (widget)));
}

static void
popup_post_show (GtkWidget *widget, gpointer user_data)
{
    PopBox *box = user_data;
    char *item_string;

    gnc_item_list_autosize (GNC_ITEM_LIST (widget));
    gnc_item_list_show_selected (GNC_ITEM_LIST (widget));
    item_string = gnc_item_list_get_selection (GNC_ITEM_LIST (widget));
    if (item_string)
    {
        horizontal_scroll_to_found_text (
            box, item_string,
            gnc_item_list_get_selected_found_location (GNC_ITEM_LIST (widget)));
        g_free (item_string);
    }
}

static int
popup_get_width (GtkWidget *widget,
                 G_GNUC_UNUSED gpointer user_data)
{
    return gtk_widget_get_width (gnc_item_list_get_view (GNC_ITEM_LIST (widget)));
}

static gboolean
gnc_completion_cell_enter (BasicCell* bcell,
                           int* cursor_position,
                           int* start_selection,
                           int* end_selection)
{
    CompletionCell* cell = (CompletionCell*) bcell;
    PopBox* box = bcell->gui_private;
    PopupToggle popup_toggle;

    gnc_item_edit_set_popup (box->item_edit,
                             GTK_WIDGET(box->item_list),
                             popup_get_height, popup_autosize,
                             popup_set_focus, popup_post_show,
                             popup_get_width, box);

    popup_toggle = box->item_edit->popup_toggle;
    gtk_widget_set_sensitive (GTK_WIDGET(popup_toggle.tbutton), FALSE);
    gtk_widget_set_visible (GTK_WIDGET(popup_toggle.tbutton), FALSE);

    completion_connect_signals (cell);

    *cursor_position = -1;
    *start_selection = 0;
    *end_selection = -1;

    return TRUE;
}

static void
gnc_completion_cell_leave (BasicCell* bcell)
{
    PopBox* box = bcell->gui_private;

    completion_disconnect_signals ((CompletionCell*) bcell);

    gnc_item_edit_set_popup (box->item_edit, NULL, NULL,
                             NULL, NULL, NULL, NULL, NULL);

    reset_item_list_to_default_setup (bcell);

    if (box->strict && !box->in_list_select)
        gnc_basic_cell_set_value_internal (bcell, "");
}

void
gnc_completion_cell_set_strict (CompletionCell* cell, gboolean strict)
{
    if (!cell)
        return;

    PopBox* box = cell->cell.gui_private;
    if (!box)
        return;

    box->strict = strict;
}

void
gnc_completion_cell_set_autosize (CompletionCell* cell, gboolean autosize)
{
    if (!cell)
        return;

    PopBox* box = cell->cell.gui_private;
    if (!box)
        return;

    box->autosize = autosize;
}
