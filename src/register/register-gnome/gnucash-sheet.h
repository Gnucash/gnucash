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

#define CELL_VPADDING 2
#define CELL_HPADDING 5


#define GNUCASH_TYPE_REGISTER     (gnucash_register_get_type ())
#define GNUCASH_REGISTER(obj)     (G_TYPE_CHECK_INSTANCE_CAST((obj), GNUCASH_TYPE_REGISTER, GnucashRegister))
#define GNUCASH_REGISTER_CLASS(k) (G_TYPE_CHECK_CLASS_CAST ((k), GNUCASH_TYPE_REGISTER))
#define GNUCASH_IS_REGISTER(o)    (G_TYPE_CHECK_INSTANCE_TYPE((o), GNUCASH_TYPE_REGISTER))


#define GNUCASH_TYPE_SHEET     (gnucash_sheet_get_type ())
#define GNUCASH_SHEET(obj)     (G_TYPE_CHECK_INSTANCE_CAST((obj), GNUCASH_TYPE_SHEET, GnucashSheet))
#define GNUCASH_SHEET_CLASS(k) (G_TYPE_CHECK_CLASS_CAST ((k), GNUCASH_TYPE_SHEET))
#define GNUCASH_IS_SHEET(o)    (G_TYPE_CHECK_INSTANCE_TYPE((o), GNUCASH_TYPE_SHEET))


typedef struct _SheetBlockStyle SheetBlockStyle;
typedef struct _GnucashSheet GnucashSheet;
typedef struct _GnucashSheetClass GnucashSheetClass;
typedef struct _GnucashRegister GnucashRegister;
typedef struct _GnucashRegisterClass GnucashRegisterClass;


typedef struct
{
    /* The style for this block */
    SheetBlockStyle *style;

    gint origin_x; /* x origin of block */
    gint origin_y; /* y origin of block */

    gboolean visible; /* is block visible */
} SheetBlock;


GType      gnucash_sheet_get_type (void);
GtkWidget *gnucash_sheet_new (Table *table);

void gnucash_sheet_table_load (GnucashSheet *sheet, gboolean do_scroll);

void gnucash_sheet_recompute_block_offsets (GnucashSheet *sheet);

GType gnucash_register_get_type (void);

/* this already has scrollbars attached */
GtkWidget *gnucash_register_new (Table *table);

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

void gnucash_register_goto_virt_cell (GnucashRegister *reg,
                                      VirtualCellLocation vcell_loc);

void gnucash_register_goto_virt_loc (GnucashRegister *reg,
                                     VirtualLocation virt_loc);

void gnucash_register_goto_next_virt_row (GnucashRegister *reg);

typedef gboolean (*VirtualLocationMatchFunc) (VirtualLocation virt_loc,
        gpointer user_data);

void gnucash_register_goto_next_matching_row (GnucashRegister *reg,
        VirtualLocationMatchFunc match,
        gpointer user_data);

void gnucash_register_attach_popup(GnucashRegister *reg, GtkWidget *popup,
                                   gpointer data);

gboolean gnucash_register_has_selection (GnucashRegister *reg);
void gnucash_register_cut_clipboard (GnucashRegister *reg);
void gnucash_register_copy_clipboard (GnucashRegister *reg);
void gnucash_register_paste_clipboard (GnucashRegister *reg);
void gnucash_register_refresh_from_prefs (GnucashRegister *reg);
void gnucash_register_set_moved_cb (GnucashRegister *reg,
                                    GFunc cb, gpointer cb_data);

GnucashSheet *gnucash_register_get_sheet (GnucashRegister *reg);

GdkColor *get_gtkrc_color (GnucashSheet *sheet, RegisterColor field_type);

#endif

