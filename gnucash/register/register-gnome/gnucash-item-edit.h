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

#include <gtk/gtk.h>
#include "gnucash-date-picker.h"
#include "gnucash-item-list.h"
#include "gnucash-sheet.h"
/** @ingroup Register
 * @addtogroup Gnome
 * @{
 */
/** @file gnucash-item-edit.h
 * @brief Public declarations for GncItemEdit class
 */
#define GNC_TYPE_ITEM_EDIT        (gnc_item_edit_get_type ())
#define GNC_ITEM_EDIT(o)          (G_TYPE_CHECK_INSTANCE_CAST((o), GNC_TYPE_ITEM_EDIT, GncItemEdit))
#define GNC_ITEM_EDIT_CLASS(k)    (G_TYPE_CHECK_CLASS_CAST ((k), GNC_TYPE_ITEM_EDIT, GncItemEditClass))
#define GNC_IS_ITEM_EDIT(o)       (G_TYPE_CHECK_INSTANCE_TYPE((o), GNC_TYPE_ITEM_EDIT))

#define GNC_TYPE_ITEM_EDIT_TB        (gnc_item_edit_tb_get_type ())
#define GNC_ITEM_EDIT_TB(o)          (G_TYPE_CHECK_INSTANCE_CAST((o), GNC_TYPE_ITEM_EDIT_TB, GncItemEditTb))
#define GNC_ITEM_EDIT_TB_CLASS(k)    (G_TYPE_CHECK_CLASS_CAST ((k), GNC_TYPE_ITEM_EDIT_TB, GncItemEditTbClass))
#define GNC_IS_ITEM_EDIT_TB(o)       (G_TYPE_CHECK_INSTANCE_TYPE((o), GNC_TYPE_ITEM_EDIT_TB))

typedef int (*PopupGetHeight) (GtkWidget *item,
                               int space_available,
                               int row_height,
                               gpointer user_data);

typedef int (*PopupAutosize) (GtkWidget *item,
                              int max_width,
                              gpointer user_data);

typedef void (*PopupSetFocus) (GtkWidget *item,
                               gpointer user_data);

typedef void (*PopupPostShow) (GtkWidget *item,
                               gpointer user_data);

typedef int (*PopupGetWidth) (GtkWidget *item,
                              gpointer user_data);

typedef struct _PopupToggle PopupToggle;
struct _PopupToggle
{
    GtkWidget *ebox;
    GtkWidget *tbutton;
    gboolean   arrow_down;
    gboolean signals_connected;
};


typedef struct
{
    GtkBox hbox;
    GnucashSheet *sheet;

    /* The editor whose status we reflect on the sheet */
    GtkWidget *editor;

    gboolean is_popup;
    gboolean show_popup;

    PopupToggle popup_toggle;
    GtkWidget *popup_item;
    PopupGetHeight   popup_get_height;
    PopupAutosize    popup_autosize;
    PopupSetFocus    popup_set_focus;
    PopupPostShow    popup_post_show;
    PopupGetWidth    popup_get_width;
    gpointer         popup_user_data;
    gint             popup_returned_height;

    GtkBorder        padding;
    GtkBorder        margin;
    GtkBorder        border;
    gint             button_width;

    /* Where are we */
    VirtualLocation virt_loc;

    SheetBlockStyle *style;
} GncItemEdit;

typedef struct
{
    GtkBoxClass parent_class;
} GncItemEditClass;

typedef struct
{
    GtkToggleButton tb;
    GnucashSheet *sheet;
} GncItemEditTb;

typedef struct
{
    GtkToggleButtonClass parent_class;

    void (* toggled) (GncItemEditTb *item_edit_tb);
} GncItemEditTbClass;

typedef enum
{
    left,
    right,
    top,
    bottom,
    left_right,
    top_bottom,
} Sides;

GType gnc_item_edit_get_type (void);

void gnc_item_edit_configure (GncItemEdit *item_edit);

void gnc_item_edit_get_pixel_coords (GncItemEdit *item_edit,
                                     int *x, int *y,
                                     int *w, int *h);

GtkWidget *gnc_item_edit_new (GnucashSheet *sheet);

void gnc_item_edit_set_popup (GncItemEdit    *item_edit,
                              GtkWidget      *popup_item,
                              PopupGetHeight  popup_get_height,
                              PopupAutosize   popup_autosize,
                              PopupSetFocus   popup_set_focus,
                              PopupPostShow   popup_post_show,
                              PopupGetWidth   popup_get_width,
                              gpointer        popup_user_data);

void gnc_item_edit_show_popup (GncItemEdit *item_edit);
void gnc_item_edit_hide_popup (GncItemEdit *item_edit);

void gnc_item_edit_cut_clipboard (GncItemEdit *item_edit);
void gnc_item_edit_copy_clipboard (GncItemEdit *item_edit);
void gnc_item_edit_paste_clipboard (GncItemEdit *item_edit);

gboolean gnc_item_edit_get_has_selection (GncItemEdit *item_edit);
void gnc_item_edit_focus_in (GncItemEdit *item_edit);
void gnc_item_edit_focus_out (GncItemEdit *item_edit);

gint gnc_item_edit_get_margin (GncItemEdit *item_edit, Sides side);
gint gnc_item_edit_get_padding_border (GncItemEdit *item_edit, Sides side);
gint gnc_item_edit_get_button_width (GncItemEdit *item_edit);


GType gnc_item_edit_tb_get_type (void);
GtkWidget *gnc_item_edit_tb_new (GnucashSheet *sheet);

/** @} */
#endif /* GNUCASH_ITEM_EDIT_H */
