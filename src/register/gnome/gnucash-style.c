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

/*
 *  configure the cursor styles
 *
 */

#include "gnucash-style.h"
#include "gnucash-sheet.h"
#include "gnucash-color.h"

#define DEFAULT_FONT "-adobe-helvetica-medium-r-normal--*-120-*-*-*-*-*-*"
#define ITALIC_FONT "-adobe-helvetica-medium-o-normal--*-120-*-*-*-*-*-*"

GdkFont *gnucash_default_font = NULL;
GdkFont *gnucash_italic_font = NULL;

/* FIXME:  read this from a config file */
/* keep this in sync with splitreg.c */
void
gnucash_style_layout_init (SheetBlockStyle *style)
{

        switch (style->reg_type) {
        case BANK_REGISTER:
        case CASH_REGISTER:
        case ASSET_REGISTER:
        case CREDIT_REGISTER:
        case LIABILITY_REGISTER:
        case INCOME_REGISTER:
        case EXPENSE_REGISTER:
        case EQUITY_REGISTER:
        case INCOME_LEDGER: 
        case GENERAL_LEDGER:
                switch (style->cursor_type) {
                case GNUCASH_CURSOR_HEADER:
                case GNUCASH_CURSOR_SINGLE:
                        style->cell_perc [0][0] = 0.10;
                        style->cell_perc [0][1] = 0.07;
                        style->cell_perc [0][2] = 0.15;
                        style->cell_perc [0][3] = 0.30;
                        style->cell_perc [0][4] = 0.02;
                        style->cell_perc [0][5] = 0.12;
                        style->cell_perc [0][6] = 0.12;
                        style->cell_perc [0][7] = 0.12;
                        break;
                case GNUCASH_CURSOR_DOUBLE:
                        style->cell_perc [0][0] = 0.10;
                        style->cell_perc [0][1] = 0.07;
                        style->cell_perc [0][2] = 0.15;
                        style->cell_perc [0][3] = 0.30;
                        style->cell_perc [0][4] = 0.02;
                        style->cell_perc [0][5] = 0.12;
                        style->cell_perc [0][6] = 0.12;
                        style->cell_perc [0][7] = 0.12;

                        style->cell_perc [1][0] = 0.10;
                        style->cell_perc [1][1] = 0.07;
                        style->cell_perc [1][2] = 0.15;
                        style->cell_perc [1][3] = 0.68;
                        style->cell_perc [1][4] = 0.0;
                        style->cell_perc [1][5] = 0.0;
                        style->cell_perc [1][6] = 0.0;
                        style->cell_perc [1][7] = 0.0;
                        break;
                case GNUCASH_CURSOR_TRANS:
                        style->cell_perc [0][0] = 0.10;
                        style->cell_perc [0][1] = 0.07;
                        style->cell_perc [0][2] = 0.15;
                        style->cell_perc [0][3] = 0.30;
                        style->cell_perc [0][4] = 0.02;
                        style->cell_perc [0][5] = 0.12;
                        style->cell_perc [0][6] = 0.12;
                        style->cell_perc [0][7] = 0.12;
                        break;
                case GNUCASH_CURSOR_SPLIT:
                        style->cell_perc [0][0] = 0.10;
                        style->cell_perc [0][1] = 0.07;
                        style->cell_perc [0][2] = 0.15;
                        style->cell_perc [0][3] = 0.32;
                        style->cell_perc [0][4] = 0.0;
                        style->cell_perc [0][5] = 0.12;
                        style->cell_perc [0][6] = 0.12;
                        style->cell_perc [0][7] = 0.12;
                        break;
                default:
                        break;
                }
                break;
        case STOCK_REGISTER:
        case PORTFOLIO:
        case CURRENCY_REGISTER:
                switch (style->cursor_type)
                {
                case GNUCASH_CURSOR_HEADER:
                case GNUCASH_CURSOR_SINGLE:
                        style->cell_perc [0][0] = 0.09;
                        style->cell_perc [0][1] = 0.06;
                        style->cell_perc [0][2] = 0.11;
                        style->cell_perc [0][3] = 0.23;
                        style->cell_perc [0][4] = 0.01;
                        style->cell_perc [0][5] = 0.10;
                        style->cell_perc [0][6] = 0.10;
                        style->cell_perc [0][7] = 0.07;
                        style->cell_perc [0][8] = 0.07;
                        style->cell_perc [0][9] = 0.07;
                        style->cell_perc [0][10] = 0.09;
                break;
                case GNUCASH_CURSOR_DOUBLE:
                        style->cell_perc [0][0] = 0.09;
                        style->cell_perc [0][1] = 0.06;
                        style->cell_perc [0][2] = 0.11;
                        style->cell_perc [0][3] = 0.23;
                        style->cell_perc [0][4] = 0.01;
                        style->cell_perc [0][5] = 0.10;
                        style->cell_perc [0][6] = 0.10;
                        style->cell_perc [0][7] = 0.07;
                        style->cell_perc [0][8] = 0.07;
                        style->cell_perc [0][9] = 0.07;
                        style->cell_perc [0][10] = 0.09;

                        style->cell_perc [1][1] = 0.15;
                        style->cell_perc [1][2] = 0.11;
                        style->cell_perc [1][3] = 0.74;
                        break;
                case GNUCASH_CURSOR_TRANS:
                        style->cell_perc [0][0] = 0.09;
                        style->cell_perc [0][1] = 0.06;
                        style->cell_perc [0][2] = 0.11;
                        style->cell_perc [0][3] = 0.23;
                        style->cell_perc [0][4] = 0.01;
                        style->cell_perc [0][5] = 0.10;
                        style->cell_perc [0][6] = 0.10;
                        style->cell_perc [0][7] = 0.07;
                        style->cell_perc [0][8] = 0.07;
                        style->cell_perc [0][9] = 0.07;
                        style->cell_perc [0][10] = 0.09;
                        break;
                case GNUCASH_CURSOR_SPLIT:
                        style->cell_perc [0][0] = 0.0;
                        style->cell_perc [0][1] = 0.15;
                        style->cell_perc [0][2] = 0.11;
                        style->cell_perc [0][3] = 0.24;
                        style->cell_perc [0][4] = 0.0;
                        style->cell_perc [0][5] = 0.10;
                        style->cell_perc [0][6] = 0.10;
                        style->cell_perc [0][7] = 0.36;
                        break;
                }
        default:
                break;
        }
}


void
gnucash_sheet_style_set_dimensions (GnucashSheet *sheet,
				    SheetBlockStyle *style)
{
        gint i, j;
        
        g_return_if_fail (sheet != NULL);
        g_return_if_fail (GNUCASH_IS_SHEET (sheet));
        g_return_if_fail (style != NULL);

        style->height = 0;
        style->width = GTK_WIDGET (sheet)->allocation.width;

        for (i = 0; i < style->nrows; i++) {
                for (j = 0; j < style->ncols; j++) {
                        style->pixel_widths[i][j] =
				style->cell_perc[i][j] * style->width + 0.5;

                        style->pixel_heights[i][j] =
                                style->fonts[i][j]->ascent +
				style->fonts[i][j]->descent +
				2*CELL_VPADDING;
                }
                style->height += style->pixel_heights[i][0];
        }
}


void
gnucash_sheet_style_destroy (SheetBlockStyle *style)
{
        gint i, j;
        
        g_return_if_fail (style != NULL);

        for ( i = 0; i < style->nrows; i++) {
                g_free(style->widths[i]);
                g_free(style->pixel_heights[i]);
                g_free(style->pixel_widths[i]);
                g_free(style->alignments[i]);
                g_free(style->fonts[i]);
                g_free(style->active_bg_color[i]);
                g_free(style->inactive_bg_color[i]);
                g_free(style->cell_perc[i]);
                for (j = 0; j < style->ncols; j++)
                        g_free (style->labels[i][j]);
                g_free (style->labels[i]);
        }

        g_free(style->widths);
        g_free(style->pixel_heights);
        g_free(style->pixel_widths);
        g_free(style->alignments);
        g_free(style->fonts);
        g_free(style->active_bg_color);
        g_free(style->inactive_bg_color);
        g_free(style->cell_perc);
        g_free(style->labels);

        g_free (style);
}


SheetBlockStyle *
gnucash_sheet_style_compile (GnucashSheet *sheet, CellBlock *cellblock,
			     gint cursor_type)
{
        gint i, j;
        SheetBlockStyle *style;
        SplitRegister *sr;

        g_return_val_if_fail (sheet != NULL, NULL);
        g_return_val_if_fail (GNUCASH_IS_SHEET (sheet), NULL);
        g_return_val_if_fail (cellblock != NULL, NULL);

        sr = (SplitRegister *)sheet->split_register;

        style  = g_new0(SheetBlockStyle, 1);

        style->reg_type = sr->type & REG_TYPE_MASK;
        style->cursor_type = cursor_type;

        style->nrows = cellblock->numRows;
        style->ncols = cellblock->numCols;

        style->widths = g_new0(gint *, cellblock->numRows);
        style->pixel_heights = g_new0(gint *, cellblock->numRows);
        style->pixel_widths = g_new0 (gint *, cellblock->numRows);
        style->alignments = g_new0 (GtkJustification *, cellblock->numRows);
        style->fonts = g_new0 (GdkFont **, cellblock->numRows);
        style->active_bg_color = g_new0 (GdkColor **, cellblock->numRows);
        style->inactive_bg_color = g_new0 (GdkColor **, cellblock->numRows);
        style->cell_perc = g_new0 (double *, cellblock->numRows);
        style->labels = g_new0 (char **, cellblock->numRows);
        
        for ( i = 0; i < style->nrows; i++) {
                style->widths[i] = g_new0(gint, style->ncols);
                style->pixel_heights[i] = g_new0(gint , style->ncols);
                style->pixel_widths[i] = g_new0 (gint, style->ncols);
                style->alignments[i] = g_new0 (GtkJustification, style->ncols);
                style->fonts[i] = g_new0 (GdkFont *, style->ncols);
                style->active_bg_color[i] = g_new0 (GdkColor *, style->ncols);
                style->inactive_bg_color[i] = g_new0(GdkColor *, style->ncols);
                style->cell_perc[i] = g_new0 (double, style->ncols);
                style->labels[i] = g_new0 (char *, style->ncols);
        }

        gnucash_style_layout_init (style);
        
        for (i = 0; i < style->nrows; i++)
                for (j = 0; j < style->ncols; j++) {
                        gint type = cellblock->cell_types[i][j];
			char *label;
                        
                        style->widths[i][j] = cellblock->widths[j];

                        style->fonts[i][j] = gnucash_default_font;

                        if (type > -1)
                                label = sr->header_label_cells[type]->value;
			else if (cursor_type == GNUCASH_CURSOR_HEADER)
				label = cellblock->cells[i][j]->value;
			else
				label = "";

			style->labels[i][j] = g_strdup(label);

                        style->active_bg_color[i][j] = gnucash_color_argb_to_gdk (cellblock->active_bg_color);
                        style->inactive_bg_color[i][j] = gnucash_color_argb_to_gdk (cellblock->passive_bg_color);

                        switch (cellblock->alignments[j]) {
                        case ALIGN_RIGHT:
                                style->alignments[ i] [j]  = GTK_JUSTIFY_RIGHT;
                                break;
                        case ALIGN_CENTER:
                                style->alignments[i][j] = GTK_JUSTIFY_CENTER;
                                break;
                        default:
                        case ALIGN_FILL:
                        case ALIGN_LEFT:
                                style->alignments[i][j] = GTK_JUSTIFY_LEFT;
                                break;
                        }
                }
                        
        gnucash_sheet_style_set_dimensions (sheet, style);

        return style;
}


/* FIXME:  maybe we can precompute these for each style */
void
gnucash_sheet_style_get_cell_pixel_rel_coords (SheetBlockStyle *style,
                                           gint cell_row, gint cell_col,
                                           gint *x, gint *y, gint *w, gint *h)
{
        gint i;
        
        g_return_if_fail (style != NULL);
        g_return_if_fail (cell_row >= 0 && cell_row <= style->nrows);
        g_return_if_fail (cell_col >= 0 && cell_col <= style->ncols);
        
        *y = 0;
        for (i = 0; i < cell_row; i++)
                *y += style->pixel_heights[i][0];

        *h = style->pixel_heights [cell_row][0];

        *x = 0;
        for (i = 0; i < cell_col; i++)
                *x += style->pixel_widths[cell_row][i];

        *w = style->pixel_widths [cell_row][cell_col];
}


SheetBlockStyle *
gnucash_sheet_get_style (GnucashSheet *sheet, gint vrow, gint vcol)
{
        SheetBlock *block;
        
        g_return_val_if_fail (sheet != NULL, NULL);
        g_return_val_if_fail (GNUCASH_IS_SHEET(sheet), NULL);

        block = gnucash_sheet_get_block (sheet, vrow, vcol);

        if (block)
                return block->style;
        else
                return NULL;
}


SheetBlockStyle *
gnucash_sheet_get_style_from_table (GnucashSheet *sheet, gint vrow, gint vcol)
{
        Table *table;
        SplitRegister *sr;

        g_return_val_if_fail (sheet != NULL, NULL);
        g_return_val_if_fail (GNUCASH_IS_SHEET(sheet), NULL);

        table = sheet->table;
        sr = (SplitRegister *)sheet->split_register;

        if (table->handlers[vrow][vcol] == sr->single_cursor)
                return sheet->cursor_style[GNUCASH_CURSOR_SINGLE];
        else if (table->handlers[vrow][vcol] == sr->double_cursor)
                return sheet->cursor_style[GNUCASH_CURSOR_DOUBLE];
        else if (table->handlers[vrow][vcol] == sr->trans_cursor)
                return sheet->cursor_style[GNUCASH_CURSOR_TRANS];
        else if (table->handlers[vrow][vcol] == sr->split_cursor)
                return sheet->cursor_style[GNUCASH_CURSOR_SPLIT];
        else
                return sheet->cursor_style[GNUCASH_CURSOR_HEADER];
}


/*
 * For now, refcounting doesn't do much, but later we may want to
 * destroy styles
 */

void
gnucash_style_ref (SheetBlockStyle *style)
{
        g_return_if_fail (style != NULL);

        style->refcount++;
}


void
gnucash_style_unref (SheetBlockStyle *style)
{
        g_return_if_fail (style != NULL);

        style->refcount--;

        if (style->refcount < 0)
                g_warning ("Unbalanced Style ref/unref");
}


void
gnucash_style_init (void)
{
        gnucash_default_font = gdk_font_load (DEFAULT_FONT);
        gnucash_italic_font = gdk_font_load (ITALIC_FONT);

        g_assert (gnucash_default_font);
        g_assert (gnucash_italic_font);
}


/*
  Local Variables:
  c-basic-offset: 8
  End:
*/
