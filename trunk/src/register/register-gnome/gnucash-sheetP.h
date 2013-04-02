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

#ifndef GNUCASH_SHEETP_H
#define GNUCASH_SHEETP_H


#include "gnucash-sheet.h"
#include "gnucash-item-edit.h"
#include <libgnomecanvas/libgnomecanvas.h>


/** Type Definitions ***************************************************/

struct _GnucashSheet
{
    GnomeCanvas canvas;

    GtkWidget *window;

    GtkWidget *popup;
    gpointer popup_data;

    Table *table;

    GtkWidget *reg;

    gint num_virt_rows;
    gint num_virt_cols;

    GnomeCanvasItem *header_item;
    GnomeCanvasItem *cursor;
    GnomeCanvasItem *grid;

    GHashTable *cursor_styles;

    /* some style information associated to a sheet */
    GHashTable *dimensions_hash_table;

    GTable *blocks;

    GnomeCanvasItem *item_editor;
    GtkWidget *entry;

    gboolean   use_theme_colors;
    gboolean   use_horizontal_lines;
    gboolean   use_vertical_lines;
    GtkWidget *header_color;
    GtkWidget *primary_color;
    GtkWidget *secondary_color;
    GtkWidget *split_color;

    gboolean input_cancelled;

    gint top_block;  /* maybe not fully visible */
    gint bottom_block;
    gint left_block;
    gint right_block;

    gint num_visible_blocks;
    gint num_visible_phys_rows;

    gint width;  /* the width in pixels of the sheet */
    gint height;

    gint window_height;
    gint window_width;

    gint cell_borders;

    gint editing;

    guint button; /* mouse button being held down */
    gboolean grabbed; /* has the grab */

    guint insert_signal;
    guint delete_signal;
    guint changed_signal;

    GtkAdjustment *hadj, *vadj;

    GFunc moved_cb;
    gpointer moved_cb_data;

    /* IMContext */
    GtkIMContext *im_context;
    gint preedit_length; /* num of bytes */
    gint preedit_char_length; /* num of chars in UTF-8 */
    gint preedit_start_position; /* save preedit start position   *
                                      * combined with selection start */
    gint preedit_cursor_position; /* save preedit cursor position */
    gint preedit_selection_length;
    PangoAttrList *preedit_attrs;
    gboolean need_im_reset;
    gboolean direct_update_cell;
    guint commit_signal;
    guint preedit_changed_signal;
    guint retrieve_surrounding_signal;
    guint delete_surrounding_signal;

    guint shift_state;
    guint keyval_state;

};


struct _GnucashSheetClass
{
    GnomeCanvasClass parent_class;
};


struct _GnucashRegister
{
    GtkTable table;

    GtkWidget *vscrollbar;
    GtkWidget *hscrollbar;
    GtkWidget *sheet;
    GtkWidget *header_canvas;
    gboolean  hscrollbar_visible;
};


struct _GnucashRegisterClass
{
    GtkTableClass parent_class;

    void (*activate_cursor) (GnucashRegister *reg);
    void (*redraw_all)      (GnucashRegister *reg);
    void (*redraw_help)     (GnucashRegister *reg);
};


/** Accessor functions *************************************************/

GncItemEdit *gnucash_sheet_get_item_edit (GnucashSheet *sheet);
//Table       *gnucash_sheet_get_table (GnucashSheet *sheet);
//gint         gnucash_sheet_get_num_virt_rows (GnucashSheet *sheet);
//gint         gnucash_sheet_get_num_virt_cols (GnucashSheet *sheet);

#endif

