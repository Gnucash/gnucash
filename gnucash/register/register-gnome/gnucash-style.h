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

#ifndef GNUCASH_STYLE_H
#define GNUCASH_STYLE_H

#include "gnucash-sheet.h"
/** @ingroup Register
 * @addtogroup Gnome
 * @{
 */
/** @file gnucash-style.h
 * @brief Styling functions for RegisterGnome.
 */
typedef struct
{
    gint pixel_height;
    gint pixel_width;

    gint origin_x;
    gint origin_y;

    gboolean can_span_over;
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

struct _SheetBlockStyle
{
    CellBlock * cursor;

    gint nrows;
    gint ncols;

    BlockDimensions *dimensions;

    gint refcount;
};


#ifdef __cplusplus
#define NOEXCEPT noexcept
extern "C"
{
#else
#define NOEXCEPT
#endif

gboolean gnucash_style_init (void) NOEXCEPT;

void gnucash_sheet_style_init(void) NOEXCEPT;

gint gnucash_style_col_is_resizable (SheetBlockStyle *style, int col) NOEXCEPT;

CellDimensions *gnucash_style_get_cell_dimensions (SheetBlockStyle *style,
                                                   int row, int col) NOEXCEPT;

void gnucash_sheet_set_col_width (GnucashSheet *sheet, int col, int width) NOEXCEPT;

gint gnucash_style_row_width(SheetBlockStyle *style, int row) NOEXCEPT;

void gnucash_sheet_styles_set_dimensions (GnucashSheet *sheet, int width) NOEXCEPT;

void gnucash_sheet_style_destroy (GnucashSheet *sheet, SheetBlockStyle *style) NOEXCEPT;

void gnucash_sheet_clear_styles (GnucashSheet *sheet) NOEXCEPT;
void gnucash_sheet_create_styles (GnucashSheet *sheet) NOEXCEPT;

void gnucash_sheet_compile_styles (GnucashSheet *sheet) NOEXCEPT;

void gnucash_sheet_styles_recompile (GnucashSheet *sheet) NOEXCEPT;

SheetBlockStyle *gnucash_sheet_get_style (GnucashSheet *sheet,
                                          VirtualCellLocation vcell_loc) NOEXCEPT;

SheetBlockStyle *
gnucash_sheet_get_style_from_table (GnucashSheet *sheet,
                                    VirtualCellLocation vcell_loc) NOEXCEPT;

SheetBlockStyle *
gnucash_sheet_get_style_from_cursor (GnucashSheet *sheet,
                                     const char *cursor_name) NOEXCEPT;

void gnucash_sheet_style_get_cell_pixel_rel_coords (SheetBlockStyle *style,
                                                    gint cell_row,
                                                    gint cell_col, gint *x,
                                                    gint *y, gint *w,
                                                    gint *h) NOEXCEPT;

void gnucash_sheet_style_ref   (GnucashSheet *sheet, SheetBlockStyle *style) NOEXCEPT;
void gnucash_sheet_style_unref (GnucashSheet *sheet, SheetBlockStyle *style) NOEXCEPT;

void gnucash_sheet_get_borders (GnucashSheet *sheet, VirtualLocation virt_loc,
                                PhysicalCellBorders *borders) NOEXCEPT;

typedef GHashTable *GNCHeaderWidths;

GNCHeaderWidths gnc_header_widths_new (void) NOEXCEPT;
void gnc_header_widths_destroy (GNCHeaderWidths widths) NOEXCEPT;
void gnc_header_widths_set_width (GNCHeaderWidths widths,
                                  const char *cell_name,
                                  int width) NOEXCEPT;
int gnc_header_widths_get_width (GNCHeaderWidths widths,
                                 const char *cell_name) NOEXCEPT;

void gnucash_sheet_get_header_widths (GnucashSheet *sheet,
                                      GNCHeaderWidths widths) NOEXCEPT;
void gnucash_sheet_set_header_widths (GnucashSheet *sheet,
                                      GNCHeaderWidths widths) NOEXCEPT;
#ifdef __cplusplus
}
#endif

/** @} */
#endif
