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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
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
 */

/*
   TODO: We have no use for the generic ComboCell->menuitems.  These
   should probably be killed.  Each GUI should probably handle it's
   own strings.
*/

#include <gnome.h>

#include "splitreg.h"
#include "table-allgui.h"
#include "table-gnome.h"
#include "combocell.h"
#include "gnucash-sheet.h"
#include "gnucash-item-edit.h"
#include "gnucash-item-list.h"
#include "global-options.h"
#include "util.h"


typedef struct _PopBox
{
	GList *menustrings;

	GnucashSheet *sheet;
	ItemEdit     *item_edit;
	GNCItemList  *item_list;

	gint     select_item_signal;
	gint     key_press_signal;
	gboolean list_signals_connected;

	gboolean list_in_sync; /* list in sync with menustrings? */
        gboolean list_sorted;  /* list has been sorted? */
        gboolean list_popped;  /* list is popped up? */

        QuickFill *qf;
        gboolean in_list_select;

        gncBoolean strict;
} PopBox;


static void realizeCombo (BasicCell *bcell, void *w, int width);
static void moveCombo (BasicCell *bcell, int phys_row, int phys_col);
static void destroyCombo (BasicCell *bcell);
static const char * enterCombo (BasicCell *bcell, const char *value,
                                int *cursor_position,
                                int *start_selection,
                                int *end_selection);
static const char * leaveCombo (BasicCell *bcell, const char *value);

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_GTK_REG;

/* =============================================== */

ComboCell *xaccMallocComboCell (void)
{
	ComboCell * cell = (ComboCell *) malloc (sizeof (ComboCell));

	assert(cell != NULL);

	xaccInitComboCell(cell);

	return cell;
}

void xaccInitComboCell (ComboCell *cell)
{
	PopBox *box;

	xaccInitBasicCell(&(cell->cell));

	cell->cell.realize = realizeCombo;
	cell->cell.destroy = destroyCombo;

	box = g_new(PopBox, 1);

	box->sheet = NULL;
	box->item_edit = NULL;
	box->item_list = NULL;
	box->menustrings = NULL;
	box->list_signals_connected = FALSE;
	box->list_in_sync = TRUE;
        box->list_sorted = TRUE;
        box->list_popped = FALSE;

	cell->cell.gui_private = box;

        box->qf = xaccMallocQuickFill();
        box->in_list_select = FALSE;

        box->strict = TRUE;
}

/* =============================================== */

static void
select_item_cb (GNCItemList *item_list, char *item_string, gpointer data)
{
	ComboCell *cell = (ComboCell *) data;
	PopBox *box = (PopBox *) cell->cell.gui_private;

        box->in_list_select = TRUE;
	gnucash_sheet_modify_current_cell(box->sheet, item_string);
        box->in_list_select = FALSE;

        item_edit_hide_list(box->item_edit);
        box->list_popped = FALSE;
}

static void
key_press_item_cb (GNCItemList *item_list, GdkEventKey *event, gpointer data)
{
	ComboCell *cell = (ComboCell *) data;
	PopBox *box = (PopBox *) cell->cell.gui_private;

        switch(event->keyval) {
                case GDK_Escape:
                        item_edit_hide_list (box->item_edit);
                        box->list_popped = FALSE;
                        break;
                default:
                        gtk_widget_event(GTK_WIDGET(box->sheet),
                                         (GdkEvent *) event);
                        break;
        }
}

static void
disconnect_list_signals (ComboCell *cell)
{
	PopBox *box = (PopBox *) cell->cell.gui_private;

	if (!box->list_signals_connected)
		return;

        if (GTK_OBJECT_DESTROYED(GTK_OBJECT(box->item_list)))
                return;

	gtk_signal_disconnect(GTK_OBJECT(box->item_list),
			      box->select_item_signal);

	gtk_signal_disconnect(GTK_OBJECT(box->item_list),
			      box->key_press_signal);

	box->list_signals_connected = FALSE;
}

static void
connect_list_signals (ComboCell *cell)
{
	PopBox *box = (PopBox *) cell->cell.gui_private;

	if (box->list_signals_connected)
		return;

        if (GTK_OBJECT_DESTROYED(GTK_OBJECT(box->item_list)))
                return;

	box->select_item_signal =
		gtk_signal_connect(GTK_OBJECT(box->item_list), "select_item",
				   GTK_SIGNAL_FUNC(select_item_cb),
				   (gpointer) cell);

	box->key_press_signal =
		gtk_signal_connect(GTK_OBJECT(box->item_list),
				   "key_press_event",
				   GTK_SIGNAL_FUNC(key_press_item_cb),
				   (gpointer) cell);

	box->list_signals_connected = TRUE;
}

/* =============================================== */

static void
destroyCombo (BasicCell *bcell)
{
	PopBox *box = (PopBox *) bcell->gui_private;
	ComboCell *cell = (ComboCell *) bcell;

	if (cell->cell.realize == NULL)
	{
		if (box != NULL && box->item_list != NULL) {
			disconnect_list_signals(cell);
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

void xaccDestroyComboCell (ComboCell *cell)
{
	PopBox *box = (PopBox *) cell->cell.gui_private;

	destroyCombo(&(cell->cell));

	if (box != NULL) {
		g_list_foreach(box->menustrings, (GFunc) g_free, NULL);
		g_list_free(box->menustrings);

                xaccFreeQuickFill(box->qf);
                box->qf = NULL;

		g_free(box);
		cell->cell.gui_private = NULL;
	}

	cell->cell.gui_private = NULL;
	cell->cell.realize = NULL;
	cell->cell.set_value = NULL;

	xaccDestroyBasicCell(&(cell->cell));
}

/* =============================================== */

void
xaccClearComboCellMenu (ComboCell * cell)
{
        PopBox *box;

        if (cell == NULL)
                return;

        box = (PopBox *) cell->cell.gui_private;
        if (box == NULL)
                return;
        if (box->menustrings == NULL)
                return;

        g_list_foreach(box->menustrings, (GFunc) g_free, NULL);
        g_list_free(box->menustrings);
        box->menustrings = NULL;

        xaccFreeQuickFill(box->qf);
        box->qf = xaccMallocQuickFill();

        if (box->item_list != NULL)
                gnc_item_list_clear(box->item_list);

        box->list_in_sync = TRUE;
        box->list_sorted = TRUE;
}

/* =============================================== */

static void
gnc_append_string_to_list(gpointer _string, gpointer _item_list)
{
	char *string = (char *) _string;
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

	box = (PopBox *) cell->cell.gui_private;
	box->menustrings = g_list_append(box->menustrings, g_strdup(menustr));

	gnc_combo_sync_edit_list(box);

	if (box->item_list != NULL)
		gnc_item_list_append(box->item_list, menustr);
	else
		box->list_in_sync = FALSE;

        xaccQFInsertText(box->qf, menustr, QUICKFILL_ALPHA);

        box->list_sorted = FALSE;
}

/* =============================================== */

void
xaccSetComboCellValue (ComboCell *cell, const char *str)
{
	xaccSetBasicCellValue(&cell->cell, str);
}

/* =============================================== */

static const char *
ComboMV (BasicCell *_cell,
         const char *oldval,
         const char *change,
	 const char *newval,
         int *cursor_position,
         int *start_selection,
         int *end_selection)
{
        QuickFillCell *cell = (QuickFillCell *) _cell;
	PopBox *box = cell->cell.gui_private;
        const char *retval;
        QuickFill *match;
        gboolean pop_list;

        if (box->in_list_select)
        {
                xaccSetBasicCellValue (_cell, newval);

                *cursor_position = -1;
                *start_selection = 0;
                *end_selection = -1;

                return newval;
        }

        /* If deleting, just accept */
        if (change == NULL)
        {
                xaccSetBasicCellValue (_cell, newval);
                return newval;
        }

        /* If we are inserting in the middle, just accept */
        if (*cursor_position < strlen(oldval))
        {
                xaccSetBasicCellValue (_cell, newval);
                return newval;
        }

        match = xaccGetQuickFillStr(box->qf, newval);

        if ((match == NULL) || (match->text == NULL))
        {
                xaccSetBasicCellValue (_cell, newval);
                gnc_item_list_select(box->item_list, NULL);
                return newval;
        }

        retval = strdup(match->text);

        *start_selection = strlen(newval);
        *end_selection = -1;
        *cursor_position += strlen(change);

        if (!box->list_popped)
                pop_list = gnc_lookup_boolean_option("Register",
                                                     "Auto-Raise Lists",
                                                     TRUE);
        else
                pop_list = FALSE;

        if (pop_list)
        {
                item_edit_show_list(box->item_edit);
                box->list_popped = TRUE;
        }

        gtk_signal_handler_block(GTK_OBJECT(box->item_list),
                                 box->select_item_signal);

	gnc_item_list_select(box->item_list, retval);

        gtk_signal_handler_unblock(GTK_OBJECT(box->item_list),
                                   box->select_item_signal);

        xaccSetBasicCellValue (_cell, retval);

        return retval;
}

/* =============================================== */

static void
realizeCombo (BasicCell *bcell, void *data, int pixel_width)
{
	GnucashSheet *sheet = (GnucashSheet *) data;
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
}

/* =============================================== */

static void
moveCombo (BasicCell *bcell, int phys_row, int phys_col)
{
	PopBox *box = (PopBox *) bcell->gui_private;

	disconnect_list_signals((ComboCell *) bcell);

	gnome_canvas_item_set(GNOME_CANVAS_ITEM(box->item_edit),
			      "is_combo", FALSE, NULL);

	item_edit_set_list(box->item_edit, NULL);

        box->list_popped = FALSE;
}

/* =============================================== */

static const char *
enterCombo (BasicCell *bcell, const char *value,
            int *cursor_position,
            int *start_selection,
            int *end_selection)
{
	PopBox *box = (PopBox *) bcell->gui_private;

	gnc_combo_sync_edit_list(box);
        gnc_combo_sort_edit_list(box);

	item_edit_set_list(box->item_edit, box->item_list);

	gnome_canvas_item_set(GNOME_CANVAS_ITEM(box->item_edit),
			      "is_combo", TRUE, NULL);

	gnc_item_list_select(box->item_list, bcell->value);

	connect_list_signals((ComboCell *) bcell);

        *cursor_position = -1;
        *start_selection = 0;
        *end_selection = -1;

	return NULL;
}

/* =============================================== */

static const char *
leaveCombo (BasicCell *bcell, const char *value)
{
        GList *find;

	PopBox *box = (PopBox *) bcell->gui_private;

	disconnect_list_signals((ComboCell *) bcell);

	gnome_canvas_item_set(GNOME_CANVAS_ITEM(box->item_edit),
			      "is_combo", FALSE, NULL);

	item_edit_set_list(box->item_edit, NULL);

        box->list_popped = FALSE;

        if (box->strict)
        {
                find = g_list_find_custom(box->menustrings,
                                          (gpointer) value,
                                          (GCompareFunc) safe_strcmp);

                /* hack, "Split" is ok, even though it's not in list */
                if (find == NULL && (safe_strcmp(value, "Split") != 0))
                        return strdup("");
        }

	return value;
}

/* =============================================== */

void
xaccComboCellSetStrict (ComboCell *cell, gncBoolean strict)
{
	PopBox *box;

	if (cell == NULL)
		return;

	box = (PopBox *) cell->cell.gui_private;

        box->strict = strict;
}

/* =============== end of file =================== */


/*
  Local Variables:
  c-basic-offset: 8
  End:
*/
