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

#ifndef GNUCASH_ITEM_EDIT_H
#define GNUCASH_ITEM_EDIT_H


#include "gnucash-sheet.h"
#include "gnucash-color.h"
#include "gnucash-grid.h"
#include "gnucash-cursor.h"
#include "gnucash-item-list.h"


#define ITEM_EDIT(obj)          (GTK_CHECK_CAST((obj), item_edit_get_type (), ItemEdit))
#define ITEM_EDIT_CLASS(k)      (GTK_CHECK_CLASS_CAST ((k), item_edit_get_type (), ItemEditClass))
#define IS_ITEM_EDIT(o)         (GTK_CHECK_TYPE((o), item_edit_get_type ()))


typedef struct _ComboToggle ComboToggle;
struct _ComboToggle
{
	GtkToggleButton *combo_button;
	GnomeCanvasItem *combo_button_item;

        gint toggle_offset;

	GtkArrow *arrow;

	gboolean signals_connected;

	gint toggle_signal;
	gint key_press_signal;
};


typedef struct
{
        GnomeCanvasItem canvas_item;

	GnomeCanvasGroup *parent;

        GnucashSheet *sheet;

        /* The editor whose status we reflect on the sheet */
        GtkWidget  *editor;

        gchar *clipboard;

        guint signal;  /* the signal we connect */
        guint signal2; /* the other signal we connect */

        gboolean has_selection;
	gboolean is_combo;
	gboolean show_list;

	ComboToggle combo_toggle;
        GNCItemList *item_list;

        GdkGC *gc;

        /* Where are we */
        int cell_col, cell_row, virt_col, virt_row;

        SheetBlockStyle  *style;
} ItemEdit;


GtkType item_edit_get_type (void);

void item_edit_configure (ItemEdit *item_edit);

void item_edit_get_pixel_coords (ItemEdit *item_edit, int *x, int *y,
				 int *w, int *h);

GnomeCanvasItem *item_edit_new (GnomeCanvasGroup *parent,
				GnucashSheet *sheet, GtkWidget *entry);

GNCItemList * item_edit_new_list (ItemEdit *item_edit);

void item_edit_set_list (ItemEdit *item_edit, GNCItemList *item_list);

void item_edit_show_list (ItemEdit *item_edit);

void item_edit_hide_list (ItemEdit *item_edit);

gboolean item_edit_set_cursor_pos (ItemEdit *item_edit,
                                   int p_row, int p_col, int x,
                                   gboolean changed_cells,
                                   gboolean extend_selection);

void item_edit_redraw (ItemEdit *item_edit);

void item_edit_claim_selection (ItemEdit *item_edit, guint32 time);

void item_edit_cut_clipboard (ItemEdit *item_edit, guint32 time);
void item_edit_copy_clipboard (ItemEdit *item_edit, guint32 time);
void item_edit_paste_clipboard (ItemEdit *item_edit, guint32 time);
void item_edit_paste_primary (ItemEdit *item_edit, guint32 time);

void item_edit_set_has_selection (ItemEdit *item_edit, gboolean has_selection);

gboolean item_edit_selection_clear (ItemEdit          *item_edit,
                                    GdkEventSelection *event);

void item_edit_selection_get (ItemEdit         *item_edit,
                              GtkSelectionData *selection_data,
                              guint             info,
                              guint             time);

void item_edit_selection_received (ItemEdit          *item_edit,
                                   GtkSelectionData  *selection_data,
                                   guint              time);

typedef struct {
        GnomeCanvasItemClass parent_class;
} ItemEditClass;


#endif /* GNUCASH_ITEM_EDIT_H */


/*
  Local Variables:
  c-basic-offset: 8
  End:
*/
