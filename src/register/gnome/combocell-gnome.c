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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
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
 */

#include "config.h"

#include <gnome.h>

#include "combocell.h"
#include "gnc-engine-util.h"
#include "gnc-ui-util.h"
#include "gnucash-item-edit.h"
#include "gnucash-item-list.h"
#include "gnucash-sheet.h"
#include "messages.h"
#include "splitreg.h"
#include "table-allgui.h"


typedef struct _PopBox
{
	GList *menustrings;

	GnucashSheet *sheet;
	ItemEdit     *item_edit;
	GNCItemList  *item_list;

	gboolean signals_connected; /* list signals connected? */

	gboolean list_in_sync; /* list in sync with menustrings? */
        gboolean list_sorted;  /* list has been sorted? */
        gboolean list_popped;  /* list is popped up? */

        QuickFill *qf;
        gboolean in_list_select;

        gboolean strict;

        char complete_char; /* char to be used for auto-completion */

        gchar *ignore_string;
        gchar *ignore_help;
} PopBox;


static void block_list_signals (ComboCell *cell);
static void unblock_list_signals (ComboCell *cell);
static void realizeCombo (BasicCell *bcell, void *w);
static void moveCombo (BasicCell *bcell, VirtualLocation virt_loc);
static void destroyCombo (BasicCell *bcell);
static gboolean enterCombo (BasicCell *bcell,
                            int *cursor_position,
                            int *start_selection,
                            int *end_selection);
static void leaveCombo (BasicCell *bcell);

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_GTK_REG;
static gboolean auto_pop_combos = FALSE;


/* =============================================== */

ComboCell *xaccMallocComboCell (void)
{
	ComboCell * cell;

        cell = g_new0 (ComboCell, 1);

	xaccInitComboCell (cell);

	return cell;
}

void xaccInitComboCell (ComboCell *cell)
{
	PopBox *box;

	xaccInitBasicCell (&(cell->cell));

        cell->cell.is_popup = TRUE;

	cell->cell.realize = realizeCombo;
	cell->cell.destroy = destroyCombo;

	box = g_new0 (PopBox, 1);

	box->sheet = NULL;
	box->item_edit = NULL;
	box->item_list = NULL;
	box->menustrings = NULL;
	box->signals_connected = FALSE;
	box->list_in_sync = TRUE;
        box->list_sorted = TRUE;
        box->list_popped = FALSE;

	cell->cell.gui_private = box;

        box->qf = gnc_quickfill_new ();
        box->in_list_select = FALSE;

        box->strict = TRUE;

        box->complete_char = 0;

        box->ignore_string = NULL;
        box->ignore_help = NULL;
}

/* =============================================== */

static void
select_item_cb (GNCItemList *item_list, char *item_string, gpointer data)
{
	ComboCell *cell = data;
	PopBox *box = cell->cell.gui_private;

        box->in_list_select = TRUE;
	gnucash_sheet_modify_current_cell (box->sheet, item_string);
        box->in_list_select = FALSE;

        item_edit_hide_popup (box->item_edit);
        box->list_popped = FALSE;
}

static void
change_item_cb (GNCItemList *item_list, char *item_string, gpointer data)
{
	ComboCell *cell = data;
	PopBox *box = cell->cell.gui_private;

        box->in_list_select = TRUE;
	gnucash_sheet_modify_current_cell(box->sheet, item_string);
        box->in_list_select = FALSE;
}

static void
activate_item_cb (GNCItemList *item_list, char *item_string, gpointer data)
{
	ComboCell *cell = data;
	PopBox *box = cell->cell.gui_private;

        item_edit_hide_popup (box->item_edit);
        box->list_popped = FALSE;
}

static void
key_press_item_cb (GNCItemList *item_list, GdkEventKey *event, gpointer data)
{
	ComboCell *cell = data;
	PopBox *box = cell->cell.gui_private;

        switch(event->keyval) {
                case GDK_Escape:
                        item_edit_hide_popup (box->item_edit);
                        box->list_popped = FALSE;
                        break;
                default:
                        gtk_widget_event(GTK_WIDGET(box->sheet),
                                         (GdkEvent *) event);
                        break;
        }
}

static void
combo_disconnect_signals (ComboCell *cell)
{
	PopBox *box = cell->cell.gui_private;

	if (!box->signals_connected)
		return;

        if (GTK_OBJECT_DESTROYED(GTK_OBJECT(box->item_list)))
                return;

	gtk_signal_disconnect_by_data (GTK_OBJECT(box->item_list), cell);

	box->signals_connected = FALSE;
}

static void
combo_connect_signals (ComboCell *cell)
{
	PopBox *box = cell->cell.gui_private;

	if (box->signals_connected)
		return;

        if (GTK_OBJECT_DESTROYED(GTK_OBJECT(box->item_list)))
                return;

        gtk_signal_connect(GTK_OBJECT(box->item_list), "select_item",
                           GTK_SIGNAL_FUNC(select_item_cb), cell);

        gtk_signal_connect(GTK_OBJECT(box->item_list), "change_item",
                           GTK_SIGNAL_FUNC(change_item_cb), cell);

        gtk_signal_connect(GTK_OBJECT(box->item_list), "activate_item",
                           GTK_SIGNAL_FUNC(activate_item_cb), cell);

        gtk_signal_connect(GTK_OBJECT(box->item_list), "key_press_event",
                           GTK_SIGNAL_FUNC(key_press_item_cb), cell);

	box->signals_connected = TRUE;
}

static void
block_list_signals (ComboCell *cell)
{
	PopBox *box = cell->cell.gui_private;

	if (!box->signals_connected)
		return;

        gtk_signal_handler_block_by_data (GTK_OBJECT(box->item_list), cell);
}

static void
unblock_list_signals (ComboCell *cell)
{
	PopBox *box = cell->cell.gui_private;

	if (!box->signals_connected)
		return;

        gtk_signal_handler_unblock_by_data (GTK_OBJECT(box->item_list), cell);
}

/* =============================================== */

static void
destroyCombo (BasicCell *bcell)
{
	PopBox *box = bcell->gui_private;
	ComboCell *cell = (ComboCell *) bcell;

	if (cell->cell.realize == NULL)
	{
		if (box != NULL && box->item_list != NULL) {
			combo_disconnect_signals(cell);
			gtk_object_unref(GTK_OBJECT(box->item_list));
			box->item_list = NULL;
		}

		/* allow the widget to be shown again */
		cell->cell.realize = realizeCombo;
		cell->cell.move = NULL;
		cell->cell.enter_cell = NULL;
		cell->cell.leave_cell = NULL;
		cell->cell.destroy = NULL;
	}

	DEBUG("combo destroyed\n");
}

/* =============================================== */

static void
menustring_free(gpointer string, gpointer user_data)
{
        g_free (string);
}

/* =============================================== */

void
xaccDestroyComboCell (ComboCell *cell)
{
	PopBox *box = cell->cell.gui_private;

	destroyCombo(&(cell->cell));

	if (box != NULL)
        {
		g_list_foreach(box->menustrings, menustring_free, NULL);
		g_list_free(box->menustrings);
                box->menustrings = NULL;

                gnc_quickfill_destroy (box->qf);
                box->qf = NULL;

                g_free(box->ignore_string);
                g_free(box->ignore_help);

		g_free(box);
		cell->cell.gui_private = NULL;
	}

	cell->cell.gui_private = NULL;
	cell->cell.realize = NULL;

	xaccDestroyBasicCell(&(cell->cell));
}

/* =============================================== */

void
xaccClearComboCellMenu (ComboCell * cell)
{
        PopBox *box;

        if (cell == NULL)
                return;

        box = cell->cell.gui_private;
        if (box == NULL)
                return;
        if (box->menustrings == NULL)
                return;

        g_list_foreach(box->menustrings, menustring_free, NULL);
        g_list_free(box->menustrings);
        box->menustrings = NULL;

        gnc_quickfill_destroy (box->qf);
        box->qf = gnc_quickfill_new ();

        if (box->item_list != NULL)
        {
                block_list_signals(cell);

                gnc_item_list_clear(box->item_list);

                unblock_list_signals(cell);
        }

        box->list_in_sync = TRUE;
        box->list_sorted = TRUE;
}

/* =============================================== */

static void
gnc_append_string_to_list(gpointer _string, gpointer _item_list)
{
	char *string = _string;
	GNCItemList *item_list = GNC_ITEM_LIST(_item_list);

	gnc_item_list_append(item_list, string);
}

static void
gnc_combo_sync_edit_list(PopBox *box)
{
	if (box->list_in_sync || box->item_list == NULL)
		return;

	gnc_item_list_clear(box->item_list);
	g_list_foreach(box->menustrings, gnc_append_string_to_list,
		       box->item_list);
}

static void
gnc_combo_sort_edit_list(PopBox *box)
{
	if (box->list_sorted || box->item_list == NULL)
		return;

        gnc_item_list_sort(box->item_list);
}

void 
xaccAddComboCellMenuItem (ComboCell *cell, char * menustr)
{ 
	PopBox *box;

	if (cell == NULL)
		return;
	if (menustr == NULL)
		return;

	box = cell->cell.gui_private;
	box->menustrings = g_list_append(box->menustrings, g_strdup(menustr));

	gnc_combo_sync_edit_list(box);

	if (box->item_list != NULL)
        {
                block_list_signals(cell);

		gnc_item_list_append(box->item_list, menustr);
                if (safe_strcmp(menustr, cell->cell.value) == 0)
                        gnc_item_list_select(box->item_list, menustr);

                unblock_list_signals(cell);
        }
	else
		box->list_in_sync = FALSE;

        gnc_quickfill_insert (box->qf, menustr, QUICKFILL_ALPHA);

        box->list_sorted = FALSE;
}

/* =============================================== */

void
xaccSetComboCellValue (ComboCell *cell, const char *str)
{
	xaccSetBasicCellValue(&cell->cell, str);
}

/* =============================================== */

static void
ComboMV (BasicCell *_cell,
         const char *change,
	 const char *newval,
         int *cursor_position,
         int *start_selection,
         int *end_selection)
{
        ComboCell *cell = (ComboCell *) _cell;
	PopBox *box = cell->cell.gui_private;
        const char *match_str;
        QuickFill *match;
        gboolean pop_list;

        if (box->in_list_select)
        {
                xaccSetBasicCellValue (_cell, newval);

                *cursor_position = -1;
                *start_selection = 0;
                *end_selection = -1;

                return;
        }

        /* If deleting, just accept */
        if (change == NULL)
        {
                xaccSetBasicCellValue (_cell, newval);
                return;
        }

        /* If we are inserting in the middle, just accept */
        if (*cursor_position < strlen(_cell->value))
        {
                xaccSetBasicCellValue (_cell, newval);
                return;
        }

        match = gnc_quickfill_get_string_match (box->qf, newval);

        match_str = gnc_quickfill_string (match);

        if ((match == NULL) || (match_str == NULL))
        {
                xaccSetBasicCellValue (_cell, newval);

                block_list_signals(cell);
                gnc_item_list_select(box->item_list, NULL);
                unblock_list_signals(cell);

                return;
        }

        *start_selection = strlen(newval);
        *end_selection = -1;
        *cursor_position += strlen(change);

        if (!box->list_popped)
                pop_list = auto_pop_combos;
        else
                pop_list = FALSE;

        if (pop_list)
        {
                item_edit_show_popup (box->item_edit);
                box->list_popped = TRUE;
        }

        block_list_signals(cell);
	gnc_item_list_select(box->item_list, match_str);
        unblock_list_signals(cell);

        xaccSetBasicCellValue (_cell, match_str);
}

/* =============================================== */

static gboolean
ComboDirect (BasicCell *bcell,
             int *cursor_position,
             int *start_selection,
             int *end_selection,
             void *gui_data)
{
	ComboCell *cell = (ComboCell *) bcell;
	PopBox *box = cell->cell.gui_private;
        GdkEventKey *event = gui_data;
        gboolean keep_on_going = FALSE;
        gboolean extra_colon;
        QuickFill *match;
        const char *match_str;
        char *search;
        int prefix_len;
        int new_pos;
        int length;

        if (event->type != GDK_KEY_PRESS)
                return FALSE;

        length = strlen(bcell->value);

        switch (event->keyval) {
                case GDK_slash:
                        if (!(event->state & GDK_MOD1_MASK))
                        {
                                if (event->keyval == box->complete_char)
                                        break;

                                return FALSE;
                        }
                        keep_on_going = TRUE;
                        /* Fall through */
                case GDK_Tab:
                case GDK_ISO_Left_Tab:
                        if (!(event->state & GDK_CONTROL_MASK) &&
                            !keep_on_going)
                                return FALSE;

                        match = gnc_quickfill_get_string_len_match (box->qf, bcell->value, *cursor_position);
                        if (match == NULL)
                                return TRUE;

                        match = gnc_quickfill_get_unique_len_match (match, &prefix_len);
                        if (match == NULL)
                                return TRUE;

                        match_str = gnc_quickfill_string (match);

                        if ((match_str != NULL) &&
                            (strncmp(match_str, bcell->value, length) == 0) && 
                            (strcmp(match_str, bcell->value) != 0))
                        {
                                xaccSetBasicCellValue(bcell, match_str);

                                block_list_signals(cell);
                                gnc_item_list_select(box->item_list, match_str);
                                unblock_list_signals(cell);

                        }

                        *cursor_position += prefix_len;
                        *start_selection = *cursor_position;
                        *end_selection = -1;

                        return TRUE;
        }

        if (box->complete_char == 0)
                return FALSE;

        if (event->keyval != box->complete_char)
                return FALSE;

        if (event->state & (GDK_CONTROL_MASK | GDK_MOD1_MASK))
                return FALSE;

        if ((*cursor_position < length) &&
            ((*end_selection < length) ||
             (*cursor_position < *start_selection)))
                return FALSE;

        if ((*cursor_position == length) &&
            (*start_selection != *end_selection) &&
            (*end_selection < length))
                return FALSE;

        search = NULL;
        if (*cursor_position < length)
                search = strchr(bcell->value + *cursor_position + 1,
                                box->complete_char);

        new_pos = *cursor_position;

        if (search != NULL)
        {
                new_pos = search - bcell->value;
                extra_colon = FALSE;
        }
        else
        {
                new_pos = length;
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
            (strncmp(match_str, bcell->value, length) == 0) && 
            (strcmp(match_str, bcell->value) != 0))
        {
                xaccSetBasicCellValue(bcell, match_str);

                block_list_signals(cell);
                gnc_item_list_select(box->item_list, match_str);
                unblock_list_signals(cell);
        }

        *cursor_position = new_pos;
        *start_selection = new_pos;
        *end_selection = -1;

        return TRUE;
}

/* =============================================== */

static char *
ComboHelpValue(BasicCell *bcell)
{
        ComboCell *cell = (ComboCell *) bcell;
        PopBox *box = cell->cell.gui_private;

        if ((bcell->value != NULL) && (bcell->value[0] != 0))
        {
                if ((box->ignore_string != NULL) &&
                    (box->ignore_help != NULL) &&
                    (safe_strcmp(bcell->value, box->ignore_string) == 0))
                        return g_strdup(box->ignore_help);

                return g_strdup(bcell->value);
        }

        if (bcell->blank_help != NULL)
                return g_strdup(bcell->blank_help);

        return NULL;
}

/* =============================================== */

static void
realizeCombo (BasicCell *bcell, void *data)
{
	GnucashSheet *sheet = data;
	GnomeCanvasItem *item = sheet->item_editor;
	ItemEdit *item_edit = ITEM_EDIT(item);
	ComboCell *cell = (ComboCell *) bcell;
	PopBox *box = cell->cell.gui_private;

	/* initialize gui-specific, private data */
	box->sheet = sheet;
	box->item_edit = item_edit;
	box->item_list = item_edit_new_list(box->item_edit);
	gtk_object_ref(GTK_OBJECT(box->item_list));
	gtk_object_sink(GTK_OBJECT(box->item_list));

	/* to mark cell as realized, remove the realize method */
	cell->cell.realize = NULL;
	cell->cell.move = moveCombo;
	cell->cell.enter_cell = enterCombo;
	cell->cell.leave_cell = leaveCombo;
	cell->cell.destroy = destroyCombo;
	cell->cell.modify_verify = ComboMV;
        cell->cell.direct_update = ComboDirect;
        cell->cell.get_help_value = ComboHelpValue;
}

/* =============================================== */

static void
moveCombo (BasicCell *bcell, VirtualLocation virt_loc)
{
	PopBox *box = bcell->gui_private;

	combo_disconnect_signals((ComboCell *) bcell);

	gnome_canvas_item_set(GNOME_CANVAS_ITEM(box->item_edit),
			      "is_combo", FALSE, NULL);

	item_edit_set_list(box->item_edit, NULL);

        box->list_popped = FALSE;
}

/* =============================================== */

static gboolean
enterCombo (BasicCell *bcell,
            int *cursor_position,
            int *start_selection,
            int *end_selection)
{
        ComboCell *cell = (ComboCell *) bcell;
	PopBox *box = bcell->gui_private;

        if ((box->ignore_string != NULL) &&
            (safe_strcmp(bcell->value, box->ignore_string) == 0))
                return FALSE;

	gnc_combo_sync_edit_list(box);
        gnc_combo_sort_edit_list(box);

	item_edit_set_list(box->item_edit, box->item_list);

	gnome_canvas_item_set(GNOME_CANVAS_ITEM(box->item_edit),
			      "is_combo", TRUE, NULL);

        block_list_signals(cell);
	gnc_item_list_select(box->item_list, bcell->value);
        unblock_list_signals(cell);

	combo_connect_signals((ComboCell *) bcell);

        *cursor_position = -1;
        *start_selection = 0;
        *end_selection = -1;

	return TRUE;
}

/* =============================================== */

static void
leaveCombo (BasicCell *bcell)
{
	PopBox *box = bcell->gui_private;

	combo_disconnect_signals((ComboCell *) bcell);

	gnome_canvas_item_set(GNOME_CANVAS_ITEM(box->item_edit),
			      "is_combo", FALSE, NULL);

	item_edit_set_list(box->item_edit, NULL);

        box->list_popped = FALSE;

        if (box->strict)
        {
                GList *find;

                find = g_list_find_custom(box->menustrings,
                                          bcell->value,
                                          (GCompareFunc) safe_strcmp);

                /* The ignore string is ok, even if it's not in list. */
                if (find == NULL &&
                    ((box->ignore_string == NULL) ||
                    (safe_strcmp(bcell->value, box->ignore_string) != 0)))
                        xaccSetBasicCellValue(bcell, "");
        }
}

/* =============================================== */

void
xaccComboCellSetStrict (ComboCell *cell, gboolean strict)
{
	PopBox *box;

	if (cell == NULL)
		return;

	box = cell->cell.gui_private;

        box->strict = strict;
}

/* =============================================== */

void
xaccComboCellSetCompleteChar (ComboCell *cell, char complete_char)
{
	PopBox *box;

	if (cell == NULL)
		return;

	box = cell->cell.gui_private;

        box->complete_char = complete_char;
}

/* =============================================== */

void
xaccComboCellSetIgnoreString (ComboCell *cell, const char *ignore_string)
{
	PopBox *box;

	if (cell == NULL)
		return;

	box = cell->cell.gui_private;

        box->ignore_string = g_strdup(ignore_string);
}

/* =============================================== */

void
xaccComboCellSetIgnoreHelp (ComboCell *cell, const char *ignore_help)
{
	PopBox *box;

	if (cell == NULL)
		return;

	box = cell->cell.gui_private;

        box->ignore_help = g_strdup(ignore_help);
}

/* =============================================== */

void
xaccComboCellSetAutoPop (gboolean auto_pop_combos_arg)
{
        auto_pop_combos = auto_pop_combos_arg;
}

/* =============== end of file =================== */


/*
  Local Variables:
  c-basic-offset: 8
  End:
*/
