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

#ifndef GNUCASH_SHEET_H
#define GNUCASH_SHEET_H

#include <gnome.h>

#include "splitreg.h"
#include "table-gnome.h"
#include "table-allgui.h"

#define CELL_VPADDING 5
#define CELL_HPADDING 4


#define GNUCASH_TYPE_REGISTER     (gnucash_register_get_type ())
#define GNUCASH_REGISTER(obj)     (GTK_CHECK_CAST((obj), GNUCASH_TYPE_REGISTER, GnucashRegister))
#define GNUCASH_REGISTER_CLASS(k) (GTK_CHECK_CLASS_CAST ((k), GNUCASH_TYPE_REGISTER))
#define GNUCASH_IS_REGISTER(o)    (GTK_CHECK_TYPE((o), GNUCASH_TYPE_REGISTER))


#define GNUCASH_TYPE_SHEET     (gnucash_sheet_get_type ())
#define GNUCASH_SHEET(obj)     (GTK_CHECK_CAST((obj), GNUCASH_TYPE_SHEET, GnucashSheet))
#define GNUCASH_SHEET_CLASS(k) (GTK_CHECK_CLASS_CAST ((k), GNUCASH_TYPE_SHEET))
#define GNUCASH_IS_SHEET(o)    (GTK_CHECK_TYPE((o), GNUCASH_TYPE_SHEET))


enum {
        GNUCASH_CURSOR_HEADER = 0,
        GNUCASH_CURSOR_SINGLE,
        GNUCASH_CURSOR_DOUBLE,
        GNUCASH_CURSOR_TRANS,
        GNUCASH_CURSOR_SPLIT,
        GNUCASH_CURSOR_LAST,
};


typedef enum {
        GNUCASH_ALIGN_TOP,
        GNUCASH_ALIGN_BOTTOM,
        GNUCASH_ALIGN_SAME,   /* keep the alignment the same */
} GnucashSheetAlignment;


typedef struct _CellLayoutInfo CellLayoutInfo;

typedef struct 
{
        /* totals, in pixels */
        gint height;
        gint width;

        /* per cell parameters */
        
        gint **pixel_heights;    /* in pixels, may be zero if
                                    row/column not displayed */
        gint **pixel_widths;

        gint **origin_x;   /* the origin of the cell */
        gint **origin_y;

        gint nrows, ncols;
        gint refcount;
        
} CellDimensions;

typedef struct
{
        gint nrows;
        gint ncols;

        gint reg_type;
        gint cursor_type;

        /* this one comes from the cellblock */
        gint **widths;              /* in characters */

        CellLayoutInfo *layout_info;
        CellDimensions *dimensions;

        gchar ***labels;              /* for the header */
        GdkFont *header_font;          
        
        GtkJustification **alignments;

        /* per cell fonts;  if NULL, use the grid normal font */
        GdkFont ***fonts;

        GdkColor ***active_bg_color;
        GdkColor ***inactive_bg_color;


        gint refcount;
} SheetBlockStyle;


typedef struct  
{
        /* the virtual row/column in the table this block
           is associated to */
        gint virt_row; 
        gint virt_col;

        /*  The style for this block, derived from the handlers for
            the virt row/col */
        SheetBlockStyle *style;

        GdkColor ***fg_colors;
        GdkColor ***bg_colors;                

        /*  the text of the block;  a num_phys_row by num_phys_cols array */
        gchar ***entries;
} SheetBlock;


typedef struct {
        GnomeCanvas canvas;

        Table *table;
        SplitRegister *split_register;

        GtkWidget *reg;

        gint num_virt_rows;
        gint num_virt_cols;

        GnomeCanvasItem *header_item;
        GnomeCanvasItem *cursor;
        GnomeCanvasItem *grid;

        SheetBlockStyle *cursor_style[GNUCASH_CURSOR_LAST];

        /* some style information associated to a sheet */
        GHashTable *layout_info_hash_table;
        GHashTable *dimensions_hash_table;

        GHashTable *blocks;

        GnomeCanvasItem *item_editor;
        GtkWidget *entry;   

        gint top_block;  /* maybe not fully visible */
        gint bottom_block;
        gint left_block, right_block;

        gint top_block_offset; 
        gint left_block_offset;

        gint default_width;
        gint default_height;

        gint width;  /* the width in pixels of the sheet */
        gint height;

        gint alignment;

        gint editing;

        gint button; /* mouse button being held down */
        gboolean grabbed; /* has the grab */

        guint insert_signal;
        guint delete_signal;
        guint changed_signal;

        gint smooth_scroll;
        GtkAdjustment *hadj, *vadj;
} GnucashSheet;


typedef struct 
{
        GtkTable table;

        GtkWidget *vscrollbar;
        GtkWidget *hscrollbar;
        GtkWidget *sheet;
        GtkWidget *header_canvas;
} GnucashRegister;


GtkType    gnucash_sheet_get_type (void);
GtkWidget *gnucash_sheet_new 	 (Table *table);

void gnucash_sheet_table_load (GnucashSheet *sheet);

GtkType    gnucash_register_get_type (void);

/* this already has scrollbars attached */
GtkWidget *gnucash_register_new (Table *table);

void gnucash_sheet_set_top_block (GnucashSheet *sheet, int new_top_block,
                                  gint align);


SheetBlock *gnucash_sheet_get_block (GnucashSheet *sheet, gint vrow,
				     gint vcol);
gint
gnucash_sheet_col_max_width (GnucashSheet *sheet, gint virt_col, gint cell_col);

gint gnucash_sheet_col_get_distance(GnucashSheet *sheet, int v_row, int col_a, int col_b);

gint gnucash_sheet_row_get_distance (GnucashSheet *sheet, int row_a,
				     int row_b);

void gnucash_sheet_redraw_all (GnucashSheet *sheet);
void gnucash_sheet_redraw_block (GnucashSheet *sheet, gint row, gint col);

void gnucash_sheet_cursor_set (GnucashSheet *gsheet,
                               int virt_row, int virt_col,
			       int cell_row, int cell_col);

const char * gnucash_sheet_modify_current_cell(GnucashSheet *sheet,
					       const gchar *new_text);

void gnucash_sheet_block_set_from_table (GnucashSheet *sheet, gint virt_row,
                                         gint virt_col);

void gnucash_sheet_cursor_set_from_table (GnucashSheet *sheet,
                                          gncBoolean do_scroll);

void gnucash_sheet_move_cursor (GnucashSheet *sheet, int col, int row);

void gnucash_sheet_set_cursor_bounds (GnucashSheet *sheet,
				      int start_col, int start_row,
				      int end_col,   int end_row);

void gnucash_sheet_compute_visible_range (GnucashSheet *sheet);

void gnucash_sheet_block_pixel_origin (GnucashSheet *sheet,
                                       gint vrow, gint vcol,
                                       gint *x, gint *y);

void gnucash_sheet_make_cell_visible (GnucashSheet *sheet,
				      gint virt_row, gint virt_col,
				      gint cell_row, gint cell_col);
void gnucash_sheet_update_adjustments (GnucashSheet *sheet);

void gnucash_register_goto_virt_row_col (GnucashRegister *reg,
                                         int v_row, int v_col);

void gnucash_register_goto_next_virt_row (GnucashRegister *reg);

void gnucash_register_attach_popup(GnucashRegister *reg, GtkWidget *popup,
                                   gpointer data);

void gnucash_register_set_initial_rows(guint num_rows);

void gnucash_register_cut_clipboard (GnucashRegister *reg);
void gnucash_register_copy_clipboard (GnucashRegister *reg);
void gnucash_register_paste_clipboard (GnucashRegister *reg);


typedef struct {
        GnomeCanvasClass parent_class;

        gint (*traverse)       	(GnucashSheet *sheet,
                                 gint row, gint column,
                                 gint *new_row, gint *new_column);
        
        gint (*deactivate)	(GnucashSheet *sheet,
				 gint row, gint column);
        
        gint (*activate) 	(GnucashSheet *sheet,
				 gint row, gint column);
        
} GnucashSheetClass;


typedef struct {
        GtkTableClass parent_class;

        void (*activate_cursor) (GnucashRegister *reg);

} GnucashRegisterClass;

#endif


/*
  Local Variables:
  c-basic-offset: 8
  End:
*/
