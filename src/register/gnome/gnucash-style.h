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

#ifndef GNUCASH_STYLE_H
#define GNUCASH_STYLE_H

#include <gnome.h>

#include "splitreg.h"
#include "gnucash-sheet.h"

#define STYLE_BORDER_LEFT   (1 << 0)
#define STYLE_BORDER_RIGHT  (1 << 1)
#define STYLE_BORDER_TOP    (1 << 2)
#define STYLE_BORDER_BOTTOM (1 << 3)

typedef int RegisterBorders;

typedef struct
{
        gint pixel_height;
        gint pixel_width;

        gint origin_x;
        gint origin_y;
} CellDimensions;

typedef struct
{
        gint nrows;
        gint ncols;

        /* totals, in pixels */
        gint height;
        gint width;

        /* per cell parameters */
        GTable *cell_dimensions;

        gint refcount;
} BlockDimensions;

typedef struct _CellLayoutInfo CellLayoutInfo;

struct _SheetBlockStyle
{
        gint nrows;
        gint ncols;

        gint reg_type;
        gint cursor_type;

        CellLayoutInfo *layout_info;
        BlockDimensions *dimensions;

        gchar ***labels;              /* for the header */
        GdkFont *header_font;          

        GtkJustification **alignments;

        GdkColor ***active_bg_color;
        GdkColor ***inactive_bg_color;
        int **borders;

        gint refcount;
};


void gnucash_style_init (void);

void gnucash_sheet_style_init(void);

void gnucash_style_set_register_font_name(const char *name);
void gnucash_style_set_register_hint_font_name(const char *name);

const char * gnucash_style_get_default_register_font_name(void);
const char * gnucash_style_get_default_register_hint_font_name(void);

gint gnucash_style_col_is_resizable (SheetBlockStyle *style, int col);

CellDimensions * gnucash_style_get_cell_dimensions (SheetBlockStyle *style,
                                                    int row, int col);

void gnucash_sheet_style_set_col_width (GnucashSheet *sheet,
                                        SheetBlockStyle *style,
                                        int col, int width, int same_size);

gint gnucash_style_row_width(SheetBlockStyle *style, int row);

void gnucash_sheet_style_set_dimensions (GnucashSheet *sheet,
					 SheetBlockStyle *style, int width);

void gnucash_sheet_style_destroy (GnucashSheet *sheet, SheetBlockStyle *style);

SheetBlockStyle * gnucash_sheet_style_compile (GnucashSheet *sheet,
					       CellBlock *cellblock,
                                               gint cursor_type);

void gnucash_sheet_style_recompile (SheetBlockStyle *style,
                                    CellBlock *cellblock,
                                    SplitRegister *sr,
                                    gint cursor_type);

SheetBlockStyle *gnucash_sheet_get_style (GnucashSheet *sheet,
                                          VirtualCellLocation vcell_loc);

SheetBlockStyle *
gnucash_sheet_get_style_from_table (GnucashSheet *sheet,
                                    VirtualCellLocation vcell_loc);

void gnucash_sheet_style_get_cell_pixel_rel_coords (SheetBlockStyle *style,
                                                    gint cell_row,
						    gint cell_col,
                                                    gint *x, gint *y,
						    gint *w, gint *h);

void gnucash_style_ref (SheetBlockStyle *style);
void gnucash_style_unref (SheetBlockStyle *style);

void gnucash_style_set_cell_borders (SheetBlockStyle *style,
                                     int row, int col, int border);
void gnucash_style_set_register_borders (int reg_borders_new);
void gnucash_style_set_borders (SheetBlockStyle *style, int border);
void gnucash_sheet_set_borders (GnucashSheet *sheet, int border);


extern GdkFont *gnucash_register_font;
extern GdkFont *gnucash_register_hint_font;


#endif


/*
  Local Variables:
  c-basic-offset: 8
  End:
*/
