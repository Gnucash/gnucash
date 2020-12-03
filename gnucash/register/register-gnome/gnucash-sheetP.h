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

#include <gtk/gtk.h>
#include "gnucash-sheet.h"
#include "gnucash-item-edit.h"
#include "gnucash-cursor.h"

/** @ingroup Register
 * @addtogroup Gnome
 * @{
 */
/** @file gnucash-sheetP.h
 * @brief Private declarations for GnucashSheet class.
 */

struct _GnucashSheet
{
    GtkLayout layout;

    GtkWidget *window;

    GtkWidget *popup;
    gpointer popup_data;

    Table *table;
    gboolean read_only;

    GtkWidget *reg;

    gint num_virt_rows;
    gint num_virt_cols;

    GtkWidget *header_item;
    GnucashCursor *cursor;

    GHashTable *cursor_styles;

    /* some style information associated to a sheet */
    GHashTable *dimensions_hash_table;

    GTable *blocks;

    GtkWidget *item_editor;
    GtkWidget *entry;

    gboolean   use_gnc_color_theme;
    gboolean   use_horizontal_lines;
    gboolean   use_vertical_lines;

    gboolean input_cancelled;

    gint num_visible_blocks;
    gint num_visible_phys_rows;

    gint width;  /* the width in pixels of the sheet */
    gint height;

    gint window_height;
    gint window_width;

    gint editing;

    gboolean sheet_has_focus;

    guint button; /* mouse button being held down */
    gboolean grabbed; /* has the grab */
    gdouble button_x, button_y;

    guint insert_signal;
    guint delete_signal;

    GtkAdjustment *hadj, *vadj;
    GtkWidget *hscrollbar, *vscrollbar;

    GFunc moved_cb;
    gpointer moved_cb_data;

    GFunc open_doclink_cb;
    gpointer open_doclink_cb_data;

    guint shift_state;
    guint keyval_state;
    gboolean direct_update_cell; /** Indicates that this cell has special operation keys. */
    int pos, bound; /** Corresponds to GtkEditable's current_pos and selection_bound */

};


struct _GnucashSheetClass
{
    GtkLayoutClass parent_class;
};


GncItemEdit *gnucash_sheet_get_item_edit (GnucashSheet *sheet);
void gnucash_sheet_set_popup (GnucashSheet *sheet, GtkWidget *popup, gpointer data);
void gnucash_sheet_goto_virt_loc (GnucashSheet *sheet, VirtualLocation virt_loc);
void gnucash_sheet_refresh_from_prefs (GnucashSheet *sheet);

gboolean   gnucash_sheet_find_loc_by_pixel (GnucashSheet *sheet, gint x, gint y,
                                           VirtualLocation *vcell_loc);
gboolean gnucash_sheet_draw_internal (GnucashSheet *sheet, cairo_t *cr,
                                      GtkAllocation *alloc);
void gnucash_sheet_draw_cursor (GnucashCursor *cursor, cairo_t *cr);

/** @} */
#endif
