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

#ifndef GNUCASH_ITEM_EDIT_H
#define GNUCASH_ITEM_EDIT_H


#include "gnucash-date-picker.h"
#include "gnucash-item-list.h"
#include "gnucash-sheet.h"


#define ITEM_EDIT(obj)          (GTK_CHECK_CAST((obj), item_edit_get_type (), ItemEdit))
#define ITEM_EDIT_CLASS(k)      (GTK_CHECK_CLASS_CAST ((k), item_edit_get_type (), ItemEditClass))
#define IS_ITEM_EDIT(o)         (GTK_CHECK_TYPE((o), item_edit_get_type ()))


typedef int (*GetPopupHeight) (GnomeCanvasItem *item,
                               int space_available,
                               int row_height,
                               gpointer user_data);

typedef int (*PopupAutosize) (GnomeCanvasItem *item,
                              int max_width,
                              gpointer user_data);

typedef void (*PopupSetFocus) (GnomeCanvasItem *item,
                               gpointer user_data);

typedef void (*PopupPostShow) (GnomeCanvasItem *item,
                               gpointer user_data);

typedef int (*PopupGetWidth) (GnomeCanvasItem *item,
                              gpointer user_data);

typedef struct _PopupToggle PopupToggle;
struct _PopupToggle
{
	GtkToggleButton *toggle_button;
	GnomeCanvasItem *toggle_button_item;

        gint toggle_offset;

	GtkArrow *arrow;

	gboolean signals_connected;
};


typedef struct
{
        GnomeCanvasItem canvas_item;

	GnomeCanvasGroup *parent;

        GnucashSheet *sheet;

        /* The editor whose status we reflect on the sheet */
        GtkWidget *editor;

        gchar *clipboard;

        gboolean has_selection;

	gboolean is_popup;
	gboolean show_popup;

	PopupToggle popup_toggle;

        GnomeCanvasItem *popup_item;
        GetPopupHeight   get_popup_height;
        PopupAutosize    popup_autosize;
        PopupSetFocus    popup_set_focus;
        PopupPostShow    popup_post_show;
        PopupGetWidth    popup_get_width;
        gpointer         popup_user_data;

        GdkGC *gc;

#ifdef USE_XIM
        GdkIC     *ic;
        GdkICAttr *ic_attr;
#endif

        /* Where are we */
        VirtualLocation virt_loc;

        SheetBlockStyle *style;
} ItemEdit;


GtkType item_edit_get_type (void);

void item_edit_configure (ItemEdit *item_edit);

void item_edit_get_pixel_coords (ItemEdit *item_edit,
                                 int *x, int *y,
				 int *w, int *h);

GnomeCanvasItem *item_edit_new (GnomeCanvasGroup *parent,
				GnucashSheet *sheet, GtkWidget *entry);

GNCItemList * item_edit_new_list (ItemEdit *item_edit);
GNCDatePicker * item_edit_new_date_picker (ItemEdit *item_edit);

void item_edit_set_popup (ItemEdit        *item_edit,
                          GnomeCanvasItem *popup_item,
                          GetPopupHeight   get_popup_height,
                          PopupAutosize    popup_autosize,
                          PopupSetFocus    popup_set_focus,
                          PopupPostShow    popup_post_show,
                          PopupGetWidth    popup_get_width,
                          gpointer         popup_user_data);

void item_edit_show_popup (ItemEdit *item_edit);
void item_edit_hide_popup (ItemEdit *item_edit);

int item_edit_get_toggle_offset (int row_height);

gboolean item_edit_set_cursor_pos (ItemEdit *item_edit,
                                   VirtualLocation virt_loc, int x,
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

void item_edit_focus_in (ItemEdit *item_edit);
void item_edit_focus_out (ItemEdit *item_edit);


typedef struct
{
        GnomeCanvasItemClass parent_class;
} ItemEditClass;


#endif /* GNUCASH_ITEM_EDIT_H */


/*
  Local Variables:
  c-basic-offset: 8
  End:
*/
