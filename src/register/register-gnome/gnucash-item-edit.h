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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#ifndef GNUCASH_ITEM_EDIT_H
#define GNUCASH_ITEM_EDIT_H


#include "gnucash-date-picker.h"
#include "gnucash-item-list.h"
#include "gnucash-sheet.h"

#define GNC_TYPE_ITEM_EDIT        (gnc_item_edit_get_type ())
#define GNC_ITEM_EDIT(o)          (G_TYPE_CHECK_INSTANCE_CAST((o), GNC_TYPE_ITEM_EDIT, GncItemEdit))
#define GNC_ITEM_EDIT_CLASS(k)    (G_TYPE_CHECK_CLASS_CAST ((k), GNC_TYPE_ITEM_EDIT, GncItemEditClass))
#define GNC_IS_ITEM_EDIT(o)       (G_TYPE_CHECK_INSTANCE_TYPE((o), GNC_TYPE_ITEM_EDIT))


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

    gboolean reset_pos;
    gint x_offset;
    gint anchor_pos;

    /* Where are we */
    VirtualLocation virt_loc;

    SheetBlockStyle *style;
} GncItemEdit;

typedef struct
{
    GnomeCanvasItemClass parent_class;
} GncItemEditClass;


GType gnc_item_edit_get_type (void);

void gnc_item_edit_configure (GncItemEdit *item_edit);

void gnc_item_edit_get_pixel_coords (GncItemEdit *item_edit,
                                     int *x, int *y,
                                     int *w, int *h);

GnomeCanvasItem *gnc_item_edit_new (GnomeCanvasGroup *parent,
                                    GnucashSheet *sheet, GtkWidget *entry);

GncItemList * gnc_item_edit_new_list (GncItemEdit *item_edit, GtkListStore *shared_store);
GNCDatePicker * gnc_item_edit_new_date_picker (GncItemEdit *item_edit);

void gnc_item_edit_set_popup (GncItemEdit     *item_edit,
                              GnomeCanvasItem *popup_item,
                              GetPopupHeight   get_popup_height,
                              PopupAutosize    popup_autosize,
                              PopupSetFocus    popup_set_focus,
                              PopupPostShow    popup_post_show,
                              PopupGetWidth    popup_get_width,
                              gpointer         popup_user_data);

void gnc_item_edit_show_popup (GncItemEdit *item_edit);
void gnc_item_edit_hide_popup (GncItemEdit *item_edit);

int gnc_item_edit_get_toggle_offset (int row_height);

gboolean gnc_item_edit_set_cursor_pos (GncItemEdit *item_edit,
                                       VirtualLocation virt_loc, int x,
                                       gboolean changed_cells,
                                       gboolean extend_selection);

void gnc_item_edit_redraw (GncItemEdit *item_edit);

void gnc_item_edit_cut_clipboard (GncItemEdit *item_edit, guint32 time);
void gnc_item_edit_copy_clipboard (GncItemEdit *item_edit, guint32 time);
void gnc_item_edit_paste_clipboard (GncItemEdit *item_edit, guint32 time);
void gnc_item_edit_paste_primary (GncItemEdit *item_edit, guint32 time);

void gnc_item_edit_set_has_selection (GncItemEdit *item_edit, gboolean has_selection);
gboolean gnc_item_edit_get_has_selection (GncItemEdit *item_edit);

gboolean gnc_item_edit_selection_clear (GncItemEdit       *item_edit,
                                        GdkEventSelection *event);

void gnc_item_edit_selection_get (GncItemEdit         *item_edit,
                                  GtkSelectionData *selection_data,
                                  guint             info,
                                  guint             time);

void gnc_item_edit_selection_received (GncItemEdit       *item_edit,
                                       GtkSelectionData  *selection_data,
                                       guint              time);

void gnc_item_edit_focus_in (GncItemEdit *item_edit);
void gnc_item_edit_focus_out (GncItemEdit *item_edit);

void gnc_item_edit_reset_offset (GncItemEdit *item_edit);

#endif /* GNUCASH_ITEM_EDIT_H */


/*
  Local Variables:
  c-basic-offset: 8
  End:
*/
