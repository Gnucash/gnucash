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

#include <gnome.h>

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

typedef struct  
{
        /* The style for this block */
        SheetBlockStyle *style;

        gint origin_x; /* x origin of block */
        gint origin_y; /* y origin of block */

        gboolean visible; /* is block visible */
} SheetBlock;


typedef struct
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

} GnucashSheet;


typedef struct
{
        GtkTable table;

        GtkWidget *vscrollbar;
        GtkWidget *hscrollbar;
        GtkWidget *sheet;
        GtkWidget *header_canvas;
        gboolean  hscrollbar_visible;
} GnucashRegister;


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

void gnucash_register_set_initial_rows(guint num_rows);

gboolean gnucash_register_has_selection (GnucashRegister *reg);
void gnucash_register_cut_clipboard (GnucashRegister *reg);
void gnucash_register_copy_clipboard (GnucashRegister *reg);
void gnucash_register_paste_clipboard (GnucashRegister *reg);
void gnucash_register_refresh_from_gconf (GnucashRegister *reg);
void gnucash_register_set_moved_cb (GnucashRegister *reg,
				    GFunc cb, gpointer cb_data);

typedef struct
{
        GnomeCanvasClass parent_class;
} GnucashSheetClass;


typedef struct
{
        GtkTableClass parent_class;

        void (*activate_cursor) (GnucashRegister *reg);
        void (*redraw_all)      (GnucashRegister *reg);
        void (*redraw_help)     (GnucashRegister *reg);
} GnucashRegisterClass;

GdkColor *get_gtkrc_color (GnucashSheet *sheet, RegisterColor field_type);

#endif


/*
  Local Variables:
  c-basic-offset: 8
  End:
*/
