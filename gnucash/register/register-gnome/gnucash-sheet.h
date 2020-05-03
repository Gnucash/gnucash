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

#ifndef GNUCASH_SHEET_H
#define GNUCASH_SHEET_H

#include <gtk/gtk.h>
#include "split-register-model.h"
#include "table-allgui.h"

/** @ingroup Register
 * @addtogroup Gnome
 * @{
 */
/** @file gnucash-sheet.h
 * @brief Public declarations of GnucashSheet class.
 */

#define GNUCASH_TYPE_SHEET     (gnucash_sheet_get_type ())
#define GNUCASH_SHEET(obj)     (G_TYPE_CHECK_INSTANCE_CAST((obj), GNUCASH_TYPE_SHEET, GnucashSheet))
#define GNUCASH_SHEET_CLASS(k) (G_TYPE_CHECK_CLASS_CAST ((k), GNUCASH_TYPE_SHEET))
#define GNUCASH_IS_SHEET(o)    (G_TYPE_CHECK_INSTANCE_TYPE((o), GNUCASH_TYPE_SHEET))


typedef struct _SheetBlockStyle SheetBlockStyle;
typedef struct _GnucashSheet GnucashSheet;
typedef struct _GnucashSheetClass GnucashSheetClass;


typedef struct
{
    /** The style for this block */
    SheetBlockStyle *style;

    gint origin_x; /** x origin of block */
    gint origin_y; /** y origin of block */

    gboolean visible; /** is block visible */
} SheetBlock;


GType      gnucash_sheet_get_type (void);
GtkWidget *gnucash_sheet_new (Table *table);

void gnucash_sheet_table_load (GnucashSheet *sheet, gboolean do_scroll);

void gnucash_sheet_recompute_block_offsets (GnucashSheet *sheet);

SheetBlock *gnucash_sheet_get_block (GnucashSheet *sheet,
                                     VirtualCellLocation vcell_loc);

gint gnucash_sheet_col_max_width (GnucashSheet *sheet,
                                  gint virt_col, gint cell_col);

void gnucash_sheet_redraw_all (GnucashSheet *sheet);
void gnucash_sheet_redraw_help (GnucashSheet *sheet);

void gnucash_sheet_redraw_block (GnucashSheet *sheet,
                                 VirtualCellLocation vcell_loc);

void gnucash_sheet_cursor_set (GnucashSheet *gsheet, VirtualLocation virt_loc);

const char * gnucash_sheet_modify_current_cell(GnucashSheet *sheet,
        const gchar *new_text);

gboolean gnucash_sheet_block_set_from_table (GnucashSheet *sheet,
        VirtualCellLocation vcell_loc);

void gnucash_sheet_set_scroll_region (GnucashSheet *sheet);

void gnucash_sheet_cursor_set_from_table (GnucashSheet *sheet,
        gboolean do_scroll);

void gnucash_sheet_compute_visible_range (GnucashSheet *sheet);

void gnucash_sheet_make_cell_visible (GnucashSheet *sheet,
                                      VirtualLocation virt_loc);

void gnucash_sheet_show_range (GnucashSheet *sheet,
                               VirtualCellLocation start_loc,
                               VirtualCellLocation end_loc);

void gnucash_sheet_update_adjustments (GnucashSheet *sheet);

void gnucash_sheet_set_window (GnucashSheet *sheet, GtkWidget *window);

void gnucash_get_style_classes (GnucashSheet *sheet, GtkStyleContext *stylectxt,
                                RegisterColor field_type);

void gnucash_sheet_set_text_bounds (GnucashSheet *sheet, GdkRectangle *rect,
                                    gint x, gint y, gint width, gint height);

gint gnucash_sheet_get_text_offset (GnucashSheet *sheet, const VirtualLocation virt_loc,
                                    gint rect_width, gint logical_width);

gboolean gnucash_sheet_is_read_only (GnucashSheet *sheet);

void gnucash_sheet_set_has_focus (GnucashSheet *sheet, gboolean has_focus);

/** @} */
#endif
