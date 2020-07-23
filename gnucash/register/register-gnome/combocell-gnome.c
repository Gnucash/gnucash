/********************************************************************\
 * combocell-gnome.c -- implement combobox pull down cell for gnome *
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
 * FILE: combocell-gnome.c
 *
 * FUNCTION: Implement gnome portion of a pull-down combo widget
 *           embedded in a table cell.
 *
 * HISTORY:
 * Copyright (c) 1998 Linas Vepstas <linas@linas.org>
 * Copyright (c) 1998-1999 Rob Browning <rlb@cs.utexas.edu>
 * Copyright (c) 2000 Linas Vepstas <linas@linas.org>
 * Copyright (c) 2006 David Hampton <hampton@employees.org>
 */

#include <config.h>

#include <string.h>
#include <gdk/gdkkeysyms.h>

#include "QuickFill.h"
#include "combocell.h"
#include "gnc-prefs.h"
#include "gnucash-item-edit.h"
#include "gnucash-item-list.h"
#include "gnucash-sheet.h"
#include "gnucash-sheetP.h"
#include "table-allgui.h"
#include "Account.h"

#define GNC_PREF_AUTO_RAISE_LISTS "auto-raise-lists"

typedef struct _PopBox
{
    GnucashSheet* sheet;
    GncItemEdit*  item_edit;
    GncItemList*  item_list;
    GtkListStore* tmp_store;

    gboolean signals_connected; /* list signals connected? */

    gboolean list_popped;  /* list is popped up? */

    gboolean autosize;

    QuickFill* qf;
    gboolean use_quickfill_cache;  /* If TRUE, we don't own the qf */

    gboolean in_list_select;

    gboolean strict;

    gunichar complete_char; /* char to be used for auto-completion */

    GList* ignore_strings;
} PopBox;


static void gnc_combo_cell_gui_realize (BasicCell* bcell, gpointer w);
static void gnc_combo_cell_gui_move (BasicCell* bcell);
static void gnc_combo_cell_gui_destroy (BasicCell* bcell);
static gboolean gnc_combo_cell_enter (BasicCell* bcell,
                                      int* cursor_position,
                                      int* start_selection,
                                      int* end_selection);
static void gnc_combo_cell_leave (BasicCell* bcell);
static void gnc_combo_cell_destroy (BasicCell* bcell);

static GOnce auto_pop_init_once = G_ONCE_INIT;
static gboolean auto_pop_combos = FALSE;


static void
gnc_combo_cell_set_autopop (gpointer prefs, gchar* pref, gpointer user_data)
{
    auto_pop_combos = gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL_REGISTER,
                                          GNC_PREF_AUTO_RAISE_LISTS);
}

static gpointer
gnc_combo_cell_autopop_init (gpointer unused)
{
    gulong id;
    auto_pop_combos = gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL_REGISTER,
                                          GNC_PREF_AUTO_RAISE_LISTS);

    id = gnc_prefs_register_cb (GNC_PREFS_GROUP_GENERAL_REGISTER,
                                GNC_PREF_AUTO_RAISE_LISTS,
                                gnc_combo_cell_set_autopop,
                                NULL);

    gnc_prefs_set_reg_auto_raise_lists_id (id);
    return NULL;
}

BasicCell*
gnc_combo_cell_new (void)
{
    ComboCell* cell;

    g_once (&auto_pop_init_once, gnc_combo_cell_autopop_init, NULL);

    cell = g_new0 (ComboCell, 1);

    gnc_combo_cell_init (cell);

    return &cell->cell;
}

void
gnc_combo_cell_init (ComboCell* cell)
{
    PopBox* box;

    gnc_basic_cell_init (& (cell->cell));

    cell->cell.is_popup = TRUE;

    cell->cell.destroy = gnc_combo_cell_destroy;

    cell->cell.gui_realize = gnc_combo_cell_gui_realize;
    cell->cell.gui_destroy = gnc_combo_cell_gui_destroy;

    box = g_new0 (PopBox, 1);

    box->sheet = NULL;
    box->item_edit = NULL;
    box->item_list = NULL;
    box->tmp_store = gtk_list_store_new (1, G_TYPE_STRING);
    box->signals_connected = FALSE;
    box->list_popped = FALSE;
    box->autosize = FALSE;

    cell->cell.gui_private = box;

    box->qf = gnc_quickfill_new();
    box->use_quickfill_cache = FALSE;

    box->in_list_select = FALSE;

    box->strict = TRUE;

    box->complete_char = '\0';

    box->ignore_strings = NULL;
}

static void
select_item_cb (GncItemList* item_list, char* item_string, gpointer data)
{
    ComboCell* cell = data;
    PopBox* box = cell->cell.gui_private;

    box->in_list_select = TRUE;
    gnucash_sheet_modify_current_cell (box->sheet, item_string);
    box->in_list_select = FALSE;

    gnc_item_edit_hide_popup (box->item_edit);
    box->list_popped = FALSE;
}

static void
change_item_cb (GncItemList* item_list, char* item_string, gpointer data)
{
    ComboCell* cell = data;
    PopBox* box = cell->cell.gui_private;

    box->in_list_select = TRUE;
    gnucash_sheet_modify_current_cell (box->sheet, item_string);
    box->in_list_select = FALSE;
}

static void
activate_item_cb (GncItemList* item_list, char* item_string, gpointer data)
{
    ComboCell* cell = data;
    PopBox* box = cell->cell.gui_private;

    gnc_item_edit_hide_popup (box->item_edit);
    box->list_popped = FALSE;
}

static void
key_press_item_cb (GncItemList* item_list, GdkEventKey* event, gpointer data)
{
    ComboCell* cell = data;
    PopBox* box = cell->cell.gui_private;

    switch (event->keyval)
    {
    case GDK_KEY_Escape:
        gnc_item_edit_hide_popup (box->item_edit);
        box->list_popped = FALSE;
        break;

    default:
        gtk_widget_event (GTK_WIDGET (box->sheet),
                          (GdkEvent*) event);
        break;
    }
}

static void
combo_disconnect_signals (ComboCell* cell)
{
    PopBox* box = cell->cell.gui_private;

    if (!box->signals_connected)
        return;

    g_signal_handlers_disconnect_matched (G_OBJECT (box->item_list),
                                          G_SIGNAL_MATCH_DATA,
                                          0, 0, NULL, NULL, cell);

    box->signals_connected = FALSE;
}

static void
combo_connect_signals (ComboCell* cell)
{
    PopBox* box = cell->cell.gui_private;

    if (box->signals_connected)
        return;

    g_signal_connect (G_OBJECT (box->item_list), "select_item",
                      G_CALLBACK (select_item_cb), cell);

    g_signal_connect (G_OBJECT (box->item_list), "change_item",
                      G_CALLBACK (change_item_cb), cell);

    g_signal_connect (G_OBJECT (box->item_list), "activate_item",
                      G_CALLBACK (activate_item_cb), cell);

    g_signal_connect (G_OBJECT (box->item_list), "key_press_event",
                      G_CALLBACK (key_press_item_cb), cell);

    box->signals_connected = TRUE;
}

static void
block_list_signals (ComboCell* cell)
{
    PopBox* box = cell->cell.gui_private;

    if (!box->signals_connected)
        return;

    g_signal_handlers_block_matched (G_OBJECT (box->item_list),
                                     G_SIGNAL_MATCH_DATA,
                                     0, 0, NULL, NULL, cell);
}

static void
unblock_list_signals (ComboCell* cell)
{
    PopBox* box = cell->cell.gui_private;

    if (!box->signals_connected)
        return;

    g_signal_handlers_unblock_matched (G_OBJECT (box->item_list),
                                       G_SIGNAL_MATCH_DATA,
                                       0, 0, NULL, NULL, cell);
}

static void
gnc_combo_cell_gui_destroy (BasicCell* bcell)
{
    PopBox* box = bcell->gui_private;
    ComboCell* cell = (ComboCell*) bcell;

    if (cell->cell.gui_realize == NULL)
    {
        if (box != NULL && box->item_list != NULL)
        {
            combo_disconnect_signals (cell);
            g_object_unref (box->item_list);
            box->item_list = NULL;
        }

        /* allow the widget to be shown again */
        cell->cell.gui_realize = gnc_combo_cell_gui_realize;
        cell->cell.gui_move = NULL;
        cell->cell.enter_cell = NULL;
        cell->cell.leave_cell = NULL;
        cell->cell.gui_destroy = NULL;
    }
}

static void
gnc_combo_cell_destroy (BasicCell* bcell)
{
    ComboCell* cell = (ComboCell*) bcell;
    PopBox* box = cell->cell.gui_private;

    gnc_combo_cell_gui_destroy (& (cell->cell));

    if (box != NULL)
    {
        GList* node;

        /* Don't destroy the qf if its not ours to destroy */
        if (FALSE == box->use_quickfill_cache)
        {
            gnc_quickfill_destroy (box->qf);
            box->qf = NULL;
        }

        for (node = box->ignore_strings; node; node = node->next)
        {
            g_free (node->data);
            node->data = NULL;
        }

        g_list_free (box->ignore_strings);
        box->ignore_strings = NULL;

        g_free (box);
        cell->cell.gui_private = NULL;
    }

    cell->cell.gui_private = NULL;
    cell->cell.gui_realize = NULL;
}

void
gnc_combo_cell_set_sort_enabled (ComboCell* cell, gboolean enabled)
{
    PopBox* box;

    if (cell == NULL)
        return;

    box = cell->cell.gui_private;
    if (box->item_list == NULL)
        return;

    block_list_signals (cell);
    gnc_item_list_set_sort_enabled (box->item_list, enabled);
    unblock_list_signals (cell);
}

void
gnc_combo_cell_clear_menu (ComboCell* cell)
{
    PopBox* box;

    if (cell == NULL)
        return;

    box = cell->cell.gui_private;
    if (box == NULL)
        return;

    /* Don't destroy the qf if its not ours to destroy */
    if (FALSE == box->use_quickfill_cache)
    {
        gnc_quickfill_destroy (box->qf);
        box->qf = gnc_quickfill_new();
    }

    if (box->item_list != NULL)
    {
        block_list_signals (cell);

        gnc_item_list_clear (box->item_list);
        gnc_item_edit_hide_popup (box->item_edit);
        box->list_popped = FALSE;
        unblock_list_signals (cell);
    }
    else
        gtk_list_store_clear (box->tmp_store);
}

void
gnc_combo_cell_use_quickfill_cache (ComboCell* cell, QuickFill* shared_qf)
{
    PopBox* box;

    if (cell == NULL) return;

    box = cell->cell.gui_private;
    if (NULL == box) return;

    if (FALSE == box->use_quickfill_cache)
    {
        box->use_quickfill_cache = TRUE;
        gnc_quickfill_destroy (box->qf);
    }
    box->qf = shared_qf;
}

void
gnc_combo_cell_use_list_store_cache (ComboCell* cell, gpointer data)
{
    if (cell == NULL) return;

    cell->shared_store = data;
}

void
gnc_combo_cell_add_menu_item (ComboCell* cell, const char* menustr)
{
    PopBox* box;

    if (cell == NULL)
        return;
    if (menustr == NULL)
        return;

    box = cell->cell.gui_private;

    if (box->item_list != NULL)
    {
        block_list_signals (cell);

        gnc_item_list_append (box->item_list, menustr);
        if (cell->cell.value &&
            (strcmp (menustr, cell->cell.value) == 0))
            gnc_item_list_select (box->item_list, menustr);

        unblock_list_signals (cell);
    }
    else
    {
        GtkTreeIter iter;

        gtk_list_store_append (box->tmp_store, &iter);
        gtk_list_store_set (box->tmp_store, &iter, 0, menustr, -1);
    }

    /* If we're going to be using a pre-fab quickfill,
     * then don't fill it in here */
    if (FALSE == box->use_quickfill_cache)
    {
        gnc_quickfill_insert (box->qf, menustr, QUICKFILL_ALPHA);
    }
}

void
gnc_combo_cell_add_account_menu_item (ComboCell* cell, char* menustr)
{
    PopBox* box;
    gchar* menu_copy, *value_copy;

    if (cell == NULL)
        return;
    if (menustr == NULL)
        return;

    box = cell->cell.gui_private;

    if (box->item_list != NULL)
    {
        block_list_signals (cell);

        gnc_item_list_append (box->item_list, menustr);
        if (cell->cell.value)
        {
            menu_copy = g_strdelimit (g_strdup (menustr), "-:/\\.", ' ');
            value_copy =
                g_strdelimit (g_strdup (cell->cell.value), "-:/\\.", ' ');
            if (strcmp (menu_copy, value_copy) == 0)
            {
                gnc_combo_cell_set_value (cell, menustr);
                gnc_item_list_select (box->item_list, menustr);
            }
            g_free (value_copy);
            g_free (menu_copy);
        }
        unblock_list_signals (cell);
    }

    /* If we're going to be using a pre-fab quickfill,
     * then don't fill it in here */
    if (FALSE == box->use_quickfill_cache)
    {
        gnc_quickfill_insert (box->qf, menustr, QUICKFILL_ALPHA);
    }
}

void
gnc_combo_cell_set_value (ComboCell* cell, const char* str)
{
    gnc_basic_cell_set_value (&cell->cell, str);
}

static inline void
list_store_append (GtkListStore *store, char* string)
{
    GtkTreeIter iter;

    g_return_if_fail (store != NULL);
    g_return_if_fail (string != NULL);
    gtk_list_store_append (store, &iter);
    gtk_list_store_set (store, &iter, 0, string, -1);
}

/* This function looks through full_store for a partial match with newval and
 * returns the first match (which must be subsequently freed). It fills out
 * box->item_list with found matches.
 */
static gchar*
gnc_combo_cell_type_ahead_search (const gchar* newval,
                                  GtkListStore* full_store, ComboCell *cell)
{
    GtkTreeIter iter;
    PopBox* box = cell->cell.gui_private;
    int num_found = 0;
    gchar* match_str = NULL;
    const char* sep = gnc_get_account_separator_string ();
    char* escaped_sep = g_regex_escape_string (sep, -1);
    char* escaped_newval = g_regex_escape_string (newval, -1);
    gchar* newval_rep = g_strdup_printf (".*%s.*", escaped_sep);
    GRegex* regex0 = g_regex_new (escaped_sep, 0, 0, NULL);
    char* rep_str = g_regex_replace_literal (regex0, escaped_newval, -1, 0,
                                             newval_rep, 0, NULL);
    GRegex *regex = g_regex_new (rep_str, G_REGEX_CASELESS, 0, NULL);

    gboolean valid = gtk_tree_model_get_iter_first (GTK_TREE_MODEL (full_store),
                                                    &iter);

    /* Limit the number found to keep the combo box from getting unreasonably
     * large.
     */
    static const gint MAX_NUM_MATCHES = 30;

    g_free (rep_str);
    g_free (newval_rep);
    g_free (escaped_sep);
    g_free (escaped_newval);
    g_regex_unref (regex0);

    block_list_signals (cell); //Prevent recursion from gtk_tree_view signals.
    gnc_item_edit_hide_popup (box->item_edit);
    gtk_list_store_clear (box->tmp_store);
    unblock_list_signals (cell);

    while (valid && num_found < MAX_NUM_MATCHES)
    {
        gchar* str_data = NULL;
        gchar* normalized_str_data = NULL;
        gtk_tree_model_get (GTK_TREE_MODEL (full_store), &iter, 0,
                            &str_data, -1);
        normalized_str_data = g_utf8_normalize (str_data, -1, G_NORMALIZE_ALL);

        if (g_regex_match (regex, normalized_str_data, 0, NULL))
        {
            if (!num_found)
                match_str = g_strdup (str_data);
            ++num_found;
            list_store_append (box->tmp_store, str_data);
        }
        g_free (str_data);
        g_free (normalized_str_data);
        valid = gtk_tree_model_iter_next (GTK_TREE_MODEL (full_store), &iter);
    }

    if (num_found)
    {
        gnc_item_list_set_temp_store (box->item_list, box->tmp_store);
        gnc_item_edit_show_popup (box->item_edit);
        box->list_popped = TRUE;
    }
    g_regex_unref (regex);
    return match_str;
}

static char*
quickfill_match (QuickFill *qf, const char *string)
{
    QuickFill *match = gnc_quickfill_get_string_match (qf, string);
    return g_strdup (gnc_quickfill_string (match));
}


static void
gnc_combo_cell_modify_verify (BasicCell* _cell,
                              const char* change,
                              int change_len,
                              const char* newval,
                              int newval_len,
                              int* cursor_position,
                              int* start_selection,
                              int* end_selection)
{
    ComboCell* cell = (ComboCell*) _cell;
    PopBox* box = cell->cell.gui_private;
    gchar* match_str = NULL;
    glong newval_chars;
    glong change_chars;
    const gchar* box_str = NULL;

    newval_chars = g_utf8_strlen (newval, newval_len);
    change_chars = g_utf8_strlen (change, change_len);

    if (box->in_list_select)
    {
        gnc_basic_cell_set_value_internal (_cell, newval);
        *cursor_position = -1;
        *start_selection = 0;
        *end_selection = -1;
        return;
    }

    /* If item_list is using temp then we're already partly matched by
     * type-ahead and a quickfill_match won't work.
     */
    if (!gnc_item_list_using_temp (box->item_list))
    {
        // If we were deleting or inserting in the middle, just accept.
        if (change == NULL || *cursor_position < _cell->value_chars)
        {
            gnc_basic_cell_set_value_internal (_cell, newval);
            return;
        }
        match_str = quickfill_match (box->qf, newval);
    }

    if (match_str != NULL)
    {
        *start_selection = newval_chars;
        *end_selection = -1;
        *cursor_position += change_chars;
        box_str = match_str;
    }
    else if (cell->shared_store)
    {
        // No start-of-name match, try type-ahead search, we match any substring of the full account name.
        GtkListStore *store = cell->shared_store;
        match_str = gnc_combo_cell_type_ahead_search (newval, store, cell);
        *start_selection = newval_chars;
        *end_selection = -1;
        *cursor_position = newval_chars;
        // Do not change the string in the type-in box.
        box_str = newval;
    }

    if (match_str == NULL)
    {
        if (cell->shared_store && gnc_item_list_using_temp (box->item_list))
        {
            block_list_signals (cell); //Prevent recursion
            gnc_item_list_set_temp_store (box->item_list, NULL);
            gtk_list_store_clear (box->tmp_store);
            unblock_list_signals (cell);
        }
        gnc_basic_cell_set_value_internal (_cell, newval);
        block_list_signals (cell);
        gnc_item_list_select (box->item_list, NULL);
        unblock_list_signals (cell);
        *cursor_position = *start_selection = newval_chars;
        *end_selection = -1;
        return;
    }

    if (!box->list_popped && auto_pop_combos)
    {
        gnc_item_edit_show_popup (box->item_edit);
        box->list_popped = TRUE;
    }

    block_list_signals (cell);
    gnc_item_list_select (box->item_list, match_str);
    unblock_list_signals (cell);

    gnc_basic_cell_set_value_internal (_cell, box_str);
    g_free (match_str);
}

static gboolean
gnc_combo_cell_direct_update (BasicCell* bcell,
                              int* cursor_position,
                              int* start_selection,
                              int* end_selection,
                              void* gui_data)
{
    ComboCell* cell = (ComboCell*) bcell;
    PopBox* box = cell->cell.gui_private;
    GdkEventKey* event = gui_data;
    gboolean keep_on_going = FALSE;
    gboolean extra_colon;
    gunichar unicode_value;
    QuickFill* match;
    const char* match_str;
    int prefix_len;
    int find_pos;
    int new_pos;

    if (event->type != GDK_KEY_PRESS)
        return FALSE;

    unicode_value = gdk_keyval_to_unicode (event->keyval);
    switch (event->keyval)
    {
    case GDK_KEY_slash:
        if (! (event->state & GDK_MOD1_MASK))
        {
            if (unicode_value == box->complete_char)
                break;

            return FALSE;
        }
        keep_on_going = TRUE;
    /* fall through */
    case GDK_KEY_Tab:
    case GDK_KEY_ISO_Left_Tab:
        if (gnc_item_list_using_temp (box->item_list))
        {
            char* string = gnc_item_list_get_selection (box->item_list);
            g_signal_emit_by_name (G_OBJECT (box->item_list), "change_item",
                                   string, (gpointer)bcell);
            g_free (string);
            return FALSE;
        }
        if (! (event->state & GDK_CONTROL_MASK) &&
            !keep_on_going)
            return FALSE;

        match = gnc_quickfill_get_string_len_match
                (box->qf, bcell->value, *cursor_position);
        if (match == NULL)
            return TRUE;

        match = gnc_quickfill_get_unique_len_match
                (match, &prefix_len);
        if (match == NULL)
            return TRUE;

        match_str = gnc_quickfill_string (match);

        if ((match_str != NULL) &&
            (strncmp (match_str, bcell->value,
                      strlen (bcell->value)) == 0) &&
            (strcmp (match_str, bcell->value) != 0))
        {
            gnc_basic_cell_set_value_internal (bcell,
                                               match_str);

            block_list_signals (cell);
            gnc_item_list_select (box->item_list,
                                  match_str);
            unblock_list_signals (cell);
        }

        *cursor_position += prefix_len;
        *start_selection = *cursor_position;
        *end_selection = -1;
        return TRUE;
    }

    if (box->complete_char == 0)
        return FALSE;

    if (unicode_value != box->complete_char)
        return FALSE;

    if (event->state & (GDK_CONTROL_MASK | GDK_MOD1_MASK))
        return FALSE;

    if ((*cursor_position < bcell->value_chars) &&
        ((*end_selection < bcell->value_chars) ||
         (*cursor_position < *start_selection)))
        return FALSE;

    if ((*cursor_position == bcell->value_chars) &&
        (*start_selection != *end_selection) &&
        (*end_selection < bcell->value_chars))
        return FALSE;

    find_pos = -1;
    if (*start_selection < bcell->value_chars)
    {
        int i = *start_selection;
        const char* c;
        gunichar uc;

        c = g_utf8_offset_to_pointer (bcell->value, i);
        while (*c)
        {
            uc = g_utf8_get_char (c);
            if (uc == box->complete_char)
            {
                find_pos = (i + 1);
                break;
            }
            c = g_utf8_next_char (c);
            i++;
        }
    }

    if (find_pos >= 0)
    {
        new_pos = find_pos;
        extra_colon = FALSE;
    }
    else
    {
        new_pos = bcell->value_chars;
        extra_colon = TRUE;
    }

    match = gnc_quickfill_get_string_len_match (box->qf,
                                                bcell->value, new_pos);
    if (match == NULL)
        return FALSE;

    if (extra_colon)
    {
        match = gnc_quickfill_get_char_match (match,
                                              box->complete_char);
        if (match == NULL)
            return FALSE;

        new_pos++;
    }

    match_str = gnc_quickfill_string (match);

    if ((match_str != NULL) &&
        (strncmp (match_str, bcell->value, strlen (bcell->value)) == 0) &&
        (strcmp (match_str, bcell->value) != 0))
    {
        gnc_basic_cell_set_value_internal (bcell, match_str);

        block_list_signals (cell);
        gnc_item_list_select (box->item_list, match_str);
        unblock_list_signals (cell);
    }

    *cursor_position = new_pos;
    *start_selection = new_pos;
    *end_selection = -1;

    return TRUE;
}

static void
gnc_combo_cell_gui_realize (BasicCell* bcell, gpointer data)
{
    GnucashSheet* sheet = data;
    GncItemEdit* item_edit = gnucash_sheet_get_item_edit (sheet);
    ComboCell* cell = (ComboCell*) bcell;
    PopBox* box = cell->cell.gui_private;

    /* initialize gui-specific, private data */
    box->sheet = sheet;
    box->item_edit = item_edit;
    if (cell->shared_store)
        box->item_list = GNC_ITEM_LIST (gnc_item_list_new (cell->shared_store));
    else
        box->item_list = GNC_ITEM_LIST (gnc_item_list_new (box->tmp_store));
    gtk_widget_show_all (GTK_WIDGET (box->item_list));
    g_object_ref_sink (box->item_list);

    /* to mark cell as realized, remove the realize method */
    cell->cell.gui_realize = NULL;
    cell->cell.gui_move = gnc_combo_cell_gui_move;
    cell->cell.enter_cell = gnc_combo_cell_enter;
    cell->cell.leave_cell = gnc_combo_cell_leave;
    cell->cell.gui_destroy = gnc_combo_cell_gui_destroy;
    cell->cell.modify_verify = gnc_combo_cell_modify_verify;
    cell->cell.direct_update = gnc_combo_cell_direct_update;
}

static void
gnc_combo_cell_gui_move (BasicCell* bcell)
{
    PopBox* box = bcell->gui_private;

    combo_disconnect_signals ((ComboCell*) bcell);

    gnc_item_edit_set_popup (box->item_edit, NULL, NULL,
                             NULL, NULL, NULL, NULL, NULL);

    box->list_popped = FALSE;
}

static int
popup_get_height (G_GNUC_UNUSED GtkWidget* widget,
                  int space_available,
                  int row_height,
                  gpointer user_data)
{
    PopBox* box = user_data;
    GtkScrolledWindow* scrollwin = GNC_ITEM_LIST(widget)->scrollwin;
    int count, height;

    count = gnc_item_list_num_entries (box->item_list);
    height = count * (gnc_item_list_get_cell_height (box->item_list) + 2);

    if (height < space_available)
    {
        gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrollwin),
                                        GTK_POLICY_AUTOMATIC, GTK_POLICY_NEVER);
        return height;
    }
    gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrollwin),
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

    return gnc_item_list_autosize (GNC_ITEM_LIST (widget)) + 20;
}

static void
popup_set_focus (GtkWidget* widget,
                 G_GNUC_UNUSED gpointer user_data)
{
    /* An empty GtkTreeView grabbing focus causes the key_press events to be
     * lost because there's no entry cell to handle them.
     */
    if (gnc_item_list_num_entries (GNC_ITEM_LIST (widget)))
        gtk_widget_grab_focus (GTK_WIDGET (GNC_ITEM_LIST (widget)->tree_view));
}

static void
popup_post_show (GtkWidget* widget,
                 G_GNUC_UNUSED gpointer user_data)
{
    gnc_item_list_autosize (GNC_ITEM_LIST (widget));
    gnc_item_list_show_selected (GNC_ITEM_LIST (widget));
}

static int
popup_get_width (GtkWidget* widget,
                 G_GNUC_UNUSED gpointer user_data)
{
    GtkAllocation alloc;
    gtk_widget_get_allocation (GTK_WIDGET (GNC_ITEM_LIST (widget)->tree_view),
                               &alloc);
    return alloc.width;
}

static gboolean
gnc_combo_cell_enter (BasicCell* bcell,
                      int* cursor_position,
                      int* start_selection,
                      int* end_selection)
{
    ComboCell* cell = (ComboCell*) bcell;
    PopBox* box = bcell->gui_private;
    GList* find = NULL;

    if (bcell->value)
        find = g_list_find_custom (box->ignore_strings,
                                   bcell->value,
                                   (GCompareFunc) strcmp);
    if (find)
        return FALSE;

    gnc_item_edit_set_popup (box->item_edit,
                             GTK_WIDGET (box->item_list),
                             popup_get_height, popup_autosize,
                             popup_set_focus, popup_post_show,
                             popup_get_width, box);

    block_list_signals (cell);

    if (cell->shared_store && gnc_item_list_using_temp (box->item_list))
    {
        // Clear the temp store to ensure we don't start in type-ahead mode.
        gnc_item_list_set_temp_store (box->item_list, NULL);
        gtk_list_store_clear (box->tmp_store);
    }
    gnc_item_list_select (box->item_list, bcell->value);
    unblock_list_signals (cell);

    combo_connect_signals (cell);

    *cursor_position = -1;
    *start_selection = 0;
    *end_selection = -1;

    return TRUE;
}

static void
gnc_combo_cell_leave (BasicCell* bcell)
{
    PopBox* box = bcell->gui_private;

    combo_disconnect_signals ((ComboCell*) bcell);

    gnc_item_edit_set_popup (box->item_edit, NULL, NULL,
                             NULL, NULL, NULL, NULL, NULL);

    box->list_popped = FALSE;

    if (box->strict)
    {
        if (bcell->value)
        {
            if (gnc_item_in_list (box->item_list, bcell->value))
                return;

            if (g_list_find_custom (box->ignore_strings,
                                    bcell->value,
                                    (GCompareFunc) strcmp))
                return;
        }
        gnc_basic_cell_set_value_internal (bcell, "");
    }
}

void
gnc_combo_cell_set_strict (ComboCell* cell, gboolean strict)
{
    PopBox* box;

    if (cell == NULL)
        return;

    box = cell->cell.gui_private;

    box->strict = strict;
}

void
gnc_combo_cell_set_complete_char (ComboCell* cell, gunichar complete_char)
{
    PopBox* box;

    if (cell == NULL)
        return;

    box = cell->cell.gui_private;

    box->complete_char = complete_char;
}

void
gnc_combo_cell_add_ignore_string (ComboCell* cell,
                                  const char* ignore_string)
{
    PopBox* box;

    if (cell == NULL)
        return;

    if (!ignore_string)
        return;

    box = cell->cell.gui_private;

    box->ignore_strings = g_list_prepend (box->ignore_strings,
                                          g_strdup (ignore_string));
}

void
gnc_combo_cell_set_autosize (ComboCell* cell, gboolean autosize)
{
    PopBox* box;

    if (!cell)
        return;

    box = cell->cell.gui_private;
    if (!box)
        return;

    box->autosize = autosize;
}

