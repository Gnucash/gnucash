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
#include <gdk/gdkkeysyms.h>

#include "completioncell.h"
#include "gnc-prefs.h"
#include "gnucash-item-edit.h"
#include "gnucash-item-list.h"
#include "gnucash-sheet.h"
#include "gnucash-sheetP.h"
#include "table-allgui.h"
#include "gnc-glib-utils.h"

typedef struct _PopBox
{
    GnucashSheet* sheet;
    GncItemEdit*  item_edit;
    GncItemList*  item_list;

    GHashTable*   item_hash; // the item hash table
    GtkListStore* item_store; // the item list store

    gchar*        newval; // string value to find
    gint          newval_len; // length of string value to find

    gboolean      signals_connected; // list signals connected
    gboolean      list_popped;  // list is popped up

    gboolean      autosize; // autosize the popup width

    gboolean      sort_enabled; // sort of list store enabled
    gboolean      register_is_reversed; // whether the register is reversed
    gboolean      stop_searching; // set when there are no results

    gboolean      strict; // text entry must be in the list
    gboolean      in_list_select; // item selected in the list

    gint          occurrence; // the position in the list

} PopBox;

#define DONT_TEXT N_("Don't autocomplete")

/** Enumeration for the list-store */
enum GncCompletionColumn
{
    TEXT_COL,           //0
    TEXT_MARKUP_COL,    //1
    WEIGHT_COL,         //2
    FOUND_LOCATION_COL, //3
};

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
    box->item_store = gtk_list_store_new (4, G_TYPE_STRING, G_TYPE_STRING,
                                             G_TYPE_INT, G_TYPE_INT);
    box->signals_connected = FALSE;
    box->list_popped = FALSE;
    box->autosize = FALSE;
    box->register_is_reversed = FALSE;

    box->sort_enabled = FALSE;

    cell->cell.gui_private = box;

    box->stop_searching = FALSE;

    box->strict = FALSE;
    box->in_list_select = FALSE;
    box->occurrence = 0;

    box->item_hash = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, NULL);
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
horizontal_scroll_to_found_text (PopBox* box, char* item_string, gint found_location)
{
    if (!gtk_widget_get_realized (GTK_WIDGET(box->item_list->tree_view)))
        return;

    GtkAllocation alloc;
    gtk_widget_get_allocation (GTK_WIDGET(box->item_list->tree_view), &alloc);
    gint scroll_point = 0;
    gchar *start_string = g_utf8_substring (item_string, 0, found_location + box->newval_len);

    PangoLayout *layout = gtk_widget_create_pango_layout (GTK_WIDGET(box->item_list->tree_view), item_string);
    PangoAttrList *atlist = pango_attr_list_new ();
    PangoAttribute *bold_weight = pango_attr_weight_new (PANGO_WEIGHT_BOLD);
    bold_weight->start_index = found_location;
    bold_weight->end_index = found_location + box->newval_len;
    pango_attr_list_insert (atlist, bold_weight);
    pango_layout_set_attributes (layout, atlist);

    gint item_string_width = text_width (layout);

    pango_layout_set_text (layout, start_string, -1);

    gint start_string_width = text_width (layout);

    pango_attr_list_unref (atlist);
    g_object_unref (layout);
    g_free (start_string);

    if (item_string_width <= alloc.width)
        scroll_point = 0;
    else
        scroll_point = start_string_width - alloc.width / 2;

    if (scroll_point < 0)
        scroll_point = 0;

    gtk_tree_view_scroll_to_point (box->item_list->tree_view, scroll_point, -1);
}

static void
change_item_cb (GncItemList* item_list, char* item_string, gpointer user_data)
{
    CompletionCell* cell = user_data;
    PopBox* box = cell->cell.gui_private;

    box->in_list_select = TRUE;
    gnucash_sheet_modify_current_cell (box->sheet, item_string);
    box->in_list_select = FALSE;

    GtkTreeModel *model = gtk_tree_view_get_model (item_list->tree_view);
    GtkTreeSelection *selection = gtk_tree_view_get_selection (item_list->tree_view);
    GtkTreeIter iter;
    if (gtk_tree_selection_get_selected (selection, &model, &iter))
    {
        gint found_location;
        gtk_tree_model_get (model, &iter, FOUND_LOCATION_COL, &found_location, -1);
        horizontal_scroll_to_found_text (box, item_string, found_location);
    }
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

static void
key_press_item_cb (GncItemList* item_list, GdkEventKey* event, gpointer user_data)
{
    CompletionCell* cell = user_data;
    PopBox* box = cell->cell.gui_private;

    switch (event->keyval)
    {
    case GDK_KEY_Escape:
        block_list_signals (cell); // Prevent recursion, unselect all
        gnc_item_list_select (box->item_list, NULL);
        unblock_list_signals (cell);
        hide_popup (box);
        break;

    default:
        gtk_widget_event (GTK_WIDGET (box->sheet),
                          (GdkEvent*) event);
        break;
    }
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

    g_signal_connect (G_OBJECT(box->item_list), "key_press_event",
                      G_CALLBACK(key_press_item_cb), cell);

    box->signals_connected = TRUE;
}

static void
gnc_completion_cell_gui_destroy (BasicCell* bcell)
{
    CompletionCell* cell = (CompletionCell*) bcell;

    if (cell->cell.gui_realize)
    {
        PopBox* box = bcell->gui_private;
        if (box && box->item_list)
        {
            completion_disconnect_signals (cell);
            g_object_unref (box->item_list);
            box->item_list = NULL;
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
        if (box->item_hash)
            g_hash_table_destroy (box->item_hash);

        g_free (box);
        cell->cell.gui_private = NULL;
    }
    cell->cell.gui_private = NULL;
    cell->cell.gui_realize = NULL;
}

static gint
sort_func (GtkTreeModel* model, GtkTreeIter* iter_a, GtkTreeIter* iter_b, gpointer user_data)
{
    gint a_weight, b_weight;
    gint ret = 0;

    gtk_tree_model_get (model, iter_a, WEIGHT_COL, &a_weight, -1);
    gtk_tree_model_get (model, iter_b, WEIGHT_COL, &b_weight, -1);

    if (a_weight < b_weight)
        ret = -1;
    else if (a_weight > b_weight)
        ret = 1;

    return ret;
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
set_sort_column_enabled (PopBox* box, gboolean enable)
{
    if (enable)
    {
        gtk_tree_sortable_set_sort_func (GTK_TREE_SORTABLE(box->item_list->list_store),
                                         WEIGHT_COL, sort_func, box->item_list, NULL);

        gnc_item_list_set_sort_column (box->item_list, WEIGHT_COL);
    }
    else
        gnc_item_list_set_sort_column (box->item_list, GTK_TREE_SORTABLE_UNSORTED_SORT_COLUMN_ID);
}

static void
item_store_clear (CompletionCell* cell)
{
    if (!cell)
        return;

    PopBox* box = cell->cell.gui_private;

    // disconnect list store from tree view
    GtkListStore *store = gnc_item_list_disconnect_store (box->item_list);

    block_list_signals (cell);

    if (box->sort_enabled) // if sorting, disable it
        set_sort_column_enabled (box, FALSE);

    box->stop_searching = FALSE;
    gtk_list_store_clear (box->item_store);

    if (box->sort_enabled) // if sorting, enable it
        set_sort_column_enabled (box, TRUE);

    unblock_list_signals (cell);

    // reconect list store to tree view
    gnc_item_list_connect_store (box->item_list, store);

    hide_popup (box);
}

void
gnc_completion_cell_clear_menu (CompletionCell* cell)
{
    if (!cell)
        return;

    PopBox* box = cell->cell.gui_private;
    if (!box)
        return;

    if (box->item_list)
    {
        g_hash_table_remove_all (box->item_hash);
        item_store_clear (cell);
        box->occurrence = 0;
    }
}

void
gnc_completion_cell_add_menu_item (CompletionCell* cell,
                                   const char* menustr)
{
    if (!cell || !menustr)
        return;

    PopBox* box = cell->cell.gui_private;

    if (box->item_hash)
    {
        gpointer value = g_hash_table_lookup (box->item_hash, menustr);
        gboolean update = FALSE;
        if (value)
        {
            if (!box->register_is_reversed)
                update = TRUE;
        }
        else
            update = TRUE;

        if (update)
        {
            g_hash_table_insert (box->item_hash, g_strdup (menustr),
                                 GINT_TO_POINTER(box->occurrence));
        }
        box->occurrence++;
    }
}

void
gnc_completion_cell_set_value (CompletionCell* cell, const char* str)
{
    if (!cell || !str)

    gnc_basic_cell_set_value (&cell->cell, str);
}

static inline void
list_store_append (GtkListStore *store, char* string,
                   char* markup, gint weight, gint found_location)
{
    GtkTreeIter iter;

    g_return_if_fail (store);
    g_return_if_fail (string);
    g_return_if_fail (markup);

    gtk_list_store_append (store, &iter);

    gtk_list_store_set (store, &iter, TEXT_COL, string,
                                      TEXT_MARKUP_COL, markup,
                                      WEIGHT_COL, weight,
                                      FOUND_LOCATION_COL, found_location, -1);
}

static char*
normalize_and_fold (char* utf8_string)
{
    char *normalized = g_utf8_normalize (utf8_string, -1, G_NORMALIZE_NFC);
    if (!normalized)
        return NULL;

    char *folded = g_utf8_casefold (normalized, -1);
    g_free (normalized);
    return folded;
}

static gint
test_and_add (PopBox* box, const gchar *text, gint start_pos,
              gpointer key, gint occurrence_difference)
{
    gint ret_value = -1;
    gint text_length = g_utf8_strlen (text, -1);

    if (start_pos > text_length)
       return ret_value;

    gchar *sub_text = g_utf8_substring (text, start_pos, text_length);
    gchar *sub_text_norm_fold = normalize_and_fold (sub_text);
    gchar *found_text_ptr = g_strstr_len (sub_text_norm_fold, -1, box->newval);

    if (found_text_ptr)
    {
        gchar *markup = NULL, *prefix = NULL, *match = NULL, *suffix = NULL;
        glong newval_length = g_utf8_strlen (box->newval, -1);
        gulong found_location = g_utf8_pointer_to_offset (sub_text_norm_fold,
                                                          found_text_ptr) + start_pos;
        gboolean have_boundary = FALSE;
        gint prefix_length;
        gint weight;

        if (found_location > 0)
            prefix = g_utf8_substring (text, 0, found_location);
        else
            prefix = g_strdup ("");

        prefix_length = g_utf8_strlen (prefix, -1);

        match = g_utf8_substring (text, found_location, found_location + newval_length);

        if (found_location - start_pos >= 1)
        {
            gunichar prev = g_utf8_get_char (g_utf8_offset_to_pointer (sub_text, found_location - start_pos - 1));
            if (prev && (g_unichar_isspace (prev) || g_unichar_ispunct (prev)))
                have_boundary = TRUE;
            else
                ret_value = found_location + 1;
        }

        suffix = g_utf8_substring (text, found_location + newval_length, text_length);

        markup = g_markup_printf_escaped ("%s<b>%s</b>%s%s", prefix, match, suffix, " ");

        if ((prefix_length == 0 ) || have_boundary)
        {
            weight = occurrence_difference; // sorted by recent first

            if (g_strcmp0 (sub_text_norm_fold, box->newval) == 0) // exact match
                weight = 1;

            list_store_append (box->item_store, key, markup, weight, found_location);
        }
        g_free (markup);
        g_free (prefix);
        g_free (match);
        g_free (suffix);
    }
    g_free (sub_text_norm_fold);
    g_free (sub_text);
    return ret_value;
}

static void
add_item (gpointer key, gpointer value, gpointer user_data)
{
    PopBox* box = user_data;
    gchar *hash_entry = g_strdup (key);

    if (hash_entry && *hash_entry)
    {
        gint start_pos = 0;
        gint occurrence_difference;
        gnc_utf8_strip_invalid_and_controls (hash_entry);

        if (box->register_is_reversed)
            occurrence_difference = GPOINTER_TO_INT(value) + 1;
        else
            occurrence_difference = box->occurrence - GPOINTER_TO_INT(value);

        do
        {
            start_pos = test_and_add (box, hash_entry, start_pos, key, occurrence_difference);
        }
        while (start_pos != -1);
    }
    g_free (hash_entry);
}

static void
select_first_entry_in_list (PopBox* box)
{
    GtkTreeModel *model = gtk_tree_view_get_model (box->item_list->tree_view);
    GtkTreeIter iter;
    gchar* string;

    if (!gtk_tree_model_get_iter_first (model, &iter))
        return;

    if (!gtk_tree_model_iter_next (model, &iter))
        return;

    gtk_tree_model_get (model, &iter, TEXT_COL, &string, -1);

    gnc_item_list_select (box->item_list, string);

    GtkTreePath* path = gtk_tree_path_new_first ();
    gtk_tree_view_scroll_to_cell (box->item_list->tree_view,
                                  path, NULL, TRUE, 0.5, 0.0);
    gtk_tree_path_free (path);
    g_free (string);
}

static void
populate_list_store (CompletionCell* cell, const gchar* str)
{
    PopBox* box = cell->cell.gui_private;

    box->in_list_select = FALSE;
    box->item_edit->popup_allocation_height = -1;

    if (box->stop_searching)
        return;

    if (str && *str)
        box->newval = normalize_and_fold ((gchar*)str);
    else
        return;

    box->newval_len = g_utf8_strlen (str, -1);

    // disconnect list store from tree view
    box->item_store = gnc_item_list_disconnect_store (box->item_list);

    block_list_signals (cell);

    if (box->sort_enabled) // if sorting, disable it
        set_sort_column_enabled (box, FALSE);

    gtk_list_store_clear (box->item_store);

    // add the don't first entry
    gchar *markup = g_markup_printf_escaped ("<i>%s</i>", DONT_TEXT);
    list_store_append (box->item_store, DONT_TEXT, markup, 0, 0);
    g_free (markup);

    // add to the list store
    g_hash_table_foreach (box->item_hash, add_item, box);

    if (box->sort_enabled) // if sorting, enable it
        set_sort_column_enabled (box, TRUE);

    unblock_list_signals (cell);

    // reconnect list store to tree view
    gnc_item_list_connect_store (box->item_list, box->item_store);

    // reset horizontal scrolling
    gtk_tree_view_column_queue_resize (gtk_tree_view_get_column (
                                       box->item_list->tree_view, TEXT_COL));

    // if no entries, do not show popup
    if (gtk_tree_model_iter_n_children (GTK_TREE_MODEL(box->item_store), NULL) == 1)
    {
        box->stop_searching = TRUE;
        hide_popup (box);
    }
    else
        gnc_item_edit_show_popup (box->item_edit);

    block_list_signals (cell); // Prevent recursion, select first entry
    select_first_entry_in_list (box);
    unblock_list_signals (cell);

    g_free (box->newval);
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
    glong newval_chars = g_utf8_strlen (newval, newval_len);

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

    // check to enable searching
    if (((*cursor_position < newval_chars) &&
         (g_utf8_strlen (bcell->value, -1) < newval_chars)) ||
         (g_utf8_strlen (bcell->value, -1) > newval_chars))
    {
         box->stop_searching = FALSE;
    }

    // Are were deleting or inserting in the middle.
    if (change == NULL || *cursor_position < bcell->value_chars)
        *start_selection = *end_selection = *cursor_position;

    gchar *start_of_text = g_utf8_substring (newval, 0, *cursor_position);
    populate_list_store (cell, start_of_text);
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
get_entry_from_hash_if_size_is_one (CompletionCell* cell)
{
    if (!cell)
        return NULL;

    PopBox* box = cell->cell.gui_private;

    if (box->item_hash && (g_hash_table_size (box->item_hash) == 1))
    {
        GList *keys = g_hash_table_get_keys (box->item_hash);
        char *ret = g_strdup (keys->data);
        g_list_free (keys);
        return ret;
    }
    return NULL;
}

static gboolean
gnc_completion_cell_direct_update (BasicCell* bcell,
                                   int* cursor_position,
                                   int* start_selection,
                                   int* end_selection,
                                   void* gui_data)
{
    CompletionCell* cell = (CompletionCell*) bcell;
    PopBox* box = cell->cell.gui_private;
    GdkEventKey* event = gui_data;

    if (event->type != GDK_KEY_PRESS)
        return FALSE;

    switch (event->keyval)
    {
    case GDK_KEY_Tab:
    case GDK_KEY_ISO_Left_Tab:
        {
            if (event->state & GDK_CONTROL_MASK)
            {
                char* hash_string = get_entry_from_hash_if_size_is_one (cell);

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
    if (!cell)
        return;

    PopBox* box = cell->cell.gui_private;

    if (is_reversed != box->register_is_reversed)
    {
        gnc_completion_cell_clear_menu (cell);
        box->register_is_reversed = is_reversed;
        box->occurrence = 0;
    }
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
    box->item_list = GNC_ITEM_LIST(gnc_item_list_new (box->item_store));

    block_list_signals (cell);
    set_sort_column_enabled (box, FALSE);
    unblock_list_signals (cell);

    gtk_widget_show_all (GTK_WIDGET(box->item_list));
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

    GtkTreeViewColumn *column = gtk_tree_view_get_column (
                                    GTK_TREE_VIEW(box->item_list->tree_view), TEXT_COL);
    gtk_tree_view_column_clear_attributes (column,box->item_list->renderer);
    gtk_tree_view_column_add_attribute (column, box->item_list->renderer,
                                        "text", TEXT_COL);
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
popup_set_focus (GtkWidget* widget,
                 G_GNUC_UNUSED gpointer user_data)
{
    /* An empty GtkTreeView grabbing focus causes the key_press events to be
     * lost because there's no entry cell to handle them.
     */
    if (gnc_item_list_num_entries (GNC_ITEM_LIST(widget)))
        gtk_widget_grab_focus (GTK_WIDGET (GNC_ITEM_LIST(widget)->tree_view));
}

static void
popup_post_show (GtkWidget* widget,
                 G_GNUC_UNUSED gpointer user_data)
{
    gnc_item_list_autosize (GNC_ITEM_LIST(widget));
    gnc_item_list_show_selected (GNC_ITEM_LIST(widget));
}

static int
popup_get_width (GtkWidget* widget,
                 G_GNUC_UNUSED gpointer user_data)
{
    GtkAllocation alloc;
    gtk_widget_get_allocation (GTK_WIDGET (GNC_ITEM_LIST(widget)->tree_view),
                               &alloc);
    return alloc.width;
}

static void
tree_view_size_allocate_cb (GtkWidget *widget,
                            G_GNUC_UNUSED GtkAllocation *allocation,
                            gpointer user_data)
{
    GtkTreeModel *model = gtk_tree_view_get_model (GTK_TREE_VIEW(widget));
    GtkTreeSelection *selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(widget));
    GtkTreeIter iter;
    if (gtk_tree_selection_get_selected (selection, &model, &iter))
    {
        PopBox* box = user_data;
        gint found_location;
        gchar *item_text;
        gtk_tree_model_get (model, &iter, TEXT_COL, &item_text,
                                          FOUND_LOCATION_COL, &found_location, -1);
        horizontal_scroll_to_found_text (box, item_text, found_location);
        g_free (item_text);
    }
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

    GtkTreeViewColumn *column = gtk_tree_view_get_column (
                                    GTK_TREE_VIEW(box->item_list->tree_view), TEXT_COL);
    gtk_tree_view_column_clear_attributes (column, box->item_list->renderer);
    gtk_tree_view_column_add_attribute (column, box->item_list->renderer,
                                        "markup", TEXT_MARKUP_COL);

    g_signal_connect (G_OBJECT(box->item_list->tree_view), "size-allocate",
                      G_CALLBACK(tree_view_size_allocate_cb), box);

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
