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

#define CHARS_FIXED   (1 << 0)  /* We use this to set the cell width */
#define PIXELS_FIXED  (1 << 1)  /* We use this as the cell width */
#define RIGHT_ALIGNED (1 << 2)  /* Right align with a specified cell */
#define LEFT_ALIGNED  (1 << 3)  /* Left align with a specified cell */
#define FILL          (1 << 4)  /* Allow this cell to expand to fill */
#define CHARS_MIN     (1 << 5)  /* Cell is no smaller than this,
                                   but won't expand further */
#define PIXELS_MIN    (1 << 6)
#define CHARS_MAX     (1 << 7)  /* Cell is no larger that this,
                                   but won't shrink further */
#define PIXELS_MAX    (1 << 8)
#define STRING_FIXED  (1 << 9)  /* Fit this string */
#define STRING_MIN    (1 << 10) /* Fit this string */
#define SAME_SIZE     (1 << 11) /* Make the cell the same size
                                   as a specified cell */


struct   _CellLayoutInfo
{
        unsigned int flags;
        int chars_width;
        int pixels_width;
        int chars_min;
        int pixels_min;
        int chars_max;
        int pixels_max;
        short left_align_r;   /* the alignment cells */
        short left_align_c;
        short right_align_r;   
        short right_align_c;
        short size_r;         /*  same size as this cell */
        short size_c;
        char *string;
};


#define SET_CELL_PERC(r, n) {  \
        for (i=0; i<r; i++)   \
           for (j=0; j<n; j++) \
        style->cell_perc[i][j] = perc[i][j];  \
}


#define SET_CELL_DATA(r, n) {  \
        for (i=0; i<r; i++)   \
           for (j=0; j<n; j++) {\
        style->cell_perc[i][j] = perc[i][j];  \
        style->layout_info[i][j].flags = li[i][j].flags; \
        style->layout_info[i][j].chars_width = li[i][j].chars_width;\
        style->layout_info[i][j].pixels_width = li[i][j].pixels_width;\
        style->layout_info[i][j].chars_min = li[i][j].chars_min;\
        style->layout_info[i][j].pixels_min = li[i][j].pixels_min;\
        style->layout_info[i][j].chars_max = li[i][j].chars_max;\
        style->layout_info[i][j].pixels_max = li[i][j].pixels_max;\
        style->layout_info[i][j].right_align_r = li[i][j].right_align_r;\
        style->layout_info[i][j].right_align_c = li[i][j].right_align_c;\
        style->layout_info[i][j].left_align_r = li[i][j].left_align_r;\
        style->layout_info[i][j].left_align_c = li[i][j].left_align_c; \
        style->layout_info[i][j].size_r  = li[i][j].size_r;  \
        style->layout_info[i][j].size_c  = li[i][j].size_c;  \
        style->layout_info[i][j].string = g_strdup(li[i][j].string); \
      } \
}

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
                        {
                                int i, j;
                                double perc[1][8] = {{0.10, 0.07, 0.15, 0.30, 0.02, 0.12, 0.12, 0.12}};
                                CellLayoutInfo li[1][8] =
                                {{{STRING_FIXED, 0, 0,0,0,0,0,0,0,0,0, 0, 0, " 88/88/8888"},
                                  {CHARS_MIN | CHARS_MAX, 0, 0, 3, 0, 5,0,0,0,0,0, 0, 0, NULL},
                                  {STRING_MIN | CHARS_MAX | FILL, 0, 0, 0 ,0, 10 ,0,0,0,0,0, 0, 0, "Transfer From"},
                                  {CHARS_MIN | FILL, 0, 0,20,0,0,0,0,0,0,0, 0,0, NULL},
                                  {STRING_FIXED, 1, 0,0,0,0,0,0,0,0,0, 0, 0, "R"},
                                  {CHARS_MIN | CHARS_MAX | FILL, 0, 0, 9,0, 10,0,0,0,0,0, 0, 0, NULL},
                                  {SAME_SIZE, 0, 0, 0,0,0,0,0,0,0,0, 0,5, NULL},
                                  {SAME_SIZE, 0, 0, 0,0,0,0,0,0,0,0, 0,5,  NULL},
                                }};
                                
                                SET_CELL_DATA (1, 8);
                        }
                        
                        break;
                case GNUCASH_CURSOR_DOUBLE:
                        {
                                int i, j;
                                double perc[2][8] = {{0.10, 0.07, 0.15, 0.30, 0.02, 0.12, 0.12, 0.12},
                                                     {0.10, 0.07, 0.15, 0.68, 0.0, 0.0, 0.0, 0.0}};
                                CellLayoutInfo li[2][8] =
                                {{{STRING_FIXED, 0, 0,0,0,0,0,0,0,0,0, 0, 0, " 88/88/8888"},
                                  {CHARS_MIN | CHARS_MAX, 0, 0, 3, 0, 5,0,0,0,0,0, 0, 0, NULL},
                                  {STRING_MIN | CHARS_MAX | FILL, 0, 0, 0 ,0, 10 ,0,0,0,0,0, 0, 0, "Transfer From"},
                                  {CHARS_MIN | FILL, 0, 0,20,0,0,0,0,0,0,0, 0,0, NULL},
                                  {STRING_FIXED, 1, 0,0,0,0,0,0,0,0,0, 0, 0, "R"},
                                  {CHARS_MIN | CHARS_MAX | FILL, 0, 0, 9,0, 10,0,0,0,0,0, 0, 0, NULL},
                                  {SAME_SIZE, 0, 0, 0,0,0,0,0,0,0,0, 0,5, NULL},
                                  {SAME_SIZE, 0, 0, 0,0,0,0,0,0,0,0, 0,5, NULL}},
                                  {{LEFT_ALIGNED|RIGHT_ALIGNED, 0, 0, 0,0,0,0,0,0,0,0, 0,0, NULL},
                                   {LEFT_ALIGNED|RIGHT_ALIGNED, 0, 0, 0,0,0,0,0,1,0,1, 0,0, NULL},
                                   {LEFT_ALIGNED|RIGHT_ALIGNED, 0, 0, 0,0,0,0,0,2,0,2, 0,0, NULL},
                                   {LEFT_ALIGNED|RIGHT_ALIGNED, 0, 0, 0,0,0,0,0,3,0,7, 0,0, NULL},
                                   {PIXELS_FIXED, 0, 0, 0,0,0,0,0,0,0,0, 0,0, NULL},
                                   {PIXELS_FIXED, 0, 0, 0,0,0,0,0,0,0,0, 0,0, NULL},
                                   {PIXELS_FIXED, 0, 0, 0,0,0,0,0,0,0,0, 0,0, NULL},
                                   {PIXELS_FIXED, 0, 0, 0,0,0,0,0,0,0,0, 0,0, NULL},
                                }};
                                
                                SET_CELL_DATA (2, 8);
                        }
                        
                        break;
                case GNUCASH_CURSOR_TRANS:
                        {
                                int i, j;
                                double perc[1][8] = {{0.10, 0.07, 0.15, 0.30, 0.02, 0.12, 0.12, 0.12}};
                                CellLayoutInfo li[1][8] =
                                {{{STRING_FIXED, 0, 0,0,0,0,0,0,0,0,0, 0, 0, " 88/88/8888"},
                                  {CHARS_MIN | CHARS_MAX, 0, 0, 3, 0, 5,0,0,0,0,0, 0, 0, NULL},
                                  {STRING_MIN | CHARS_MAX | FILL, 0, 0, 0 ,0, 10 ,0,0,0,0,0, 0, 0, "Transfer From"},
                                  {CHARS_MIN | FILL, 0, 0,20,0,0,0,0,0,0,0, 0,0, NULL},
                                  {STRING_FIXED, 1, 0,0,0,0,0,0,0,0,0, 0, 0, "R"},
                                  {CHARS_MIN | CHARS_MAX | FILL, 0, 0, 9,0, 10,0,0,0,0,0, 0, 0, NULL},
                                  {SAME_SIZE, 0, 0, 0,0,0,0,0,0,0,0, 0,5, NULL},
                                  {SAME_SIZE, 0, 0, 0,0,0,0,0,0,0,0, 0,5,  NULL},
                                }};
                                        
                                SET_CELL_DATA (1, 8);
                        }

                        break;
                case GNUCASH_CURSOR_SPLIT:
                        {
                                int i, j;
                                double perc[1][8] = {{0.10, 0.07, 0.15, 0.30, 0.02, 0.12, 0.12, 0.12}};
                                CellLayoutInfo li[1][8] =
                                {{{STRING_FIXED, 0, 0,0,0,0,0,0,0,0,0, 0, 0, " 88/88/8888"},
                                  {CHARS_MIN | CHARS_MAX, 0, 0, 3, 0, 5,0,0,0,0,0, 0, 0, NULL},
                                  {STRING_MIN | CHARS_MAX | FILL, 0, 0, 0 ,0, 10 ,0,0,0,0,0, 0, 0, "Transfer From"},
                                  {CHARS_MIN | FILL, 0, 0,20,0,0,0,0,0,0,0, 0,0, NULL},
                                  {STRING_FIXED, 1, 0,0,0,0,0,0,0,0,0, 0, 0, "R"},
                                  {CHARS_MIN | CHARS_MAX | FILL, 0, 0, 9,0, 10,0,0,0,0,0, 0, 0, NULL},
                                  {SAME_SIZE, 0, 0, 0,0,0,0,0,0,0,0, 0,5, NULL},
                                  {SAME_SIZE, 0, 0, 0,0,0,0,0,0,0,0, 0,5,  NULL},
                                }};
                                
                                SET_CELL_DATA (1, 8);
                        }
                        
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
                        {
                                int i, j;
                                double perc[1][11] = {{0.09, 0.06, 0.11, 0.23, 0.01, 0.10, 0.10, 0.07, 0.07, 0.07, 0.09}};
                                CellLayoutInfo li[1][11] =
                                {{{STRING_FIXED, 0, 0,0,0,0,0,0,0,0,0, 0, 0, " 88/88/8888"},
                                  {CHARS_MIN | CHARS_MAX, 0, 0, 3, 0, 5,0,0,0,0,0, 0, 0, NULL},
                                  {STRING_MIN | CHARS_MAX | FILL, 0, 0, 0 ,0, 10 ,0,0,0,0,0, 0, 0, "Transfer From"},
                                  {CHARS_MIN | FILL, 0, 0,20,0,0,0,0,0,0,0, 0,0, NULL},
                                  {STRING_FIXED, 1, 0,0,0,0,0,0,0,0,0, 0, 0, "R"},
                                  {CHARS_MIN | CHARS_MAX | FILL, 0, 0, 9,0, 10,0,0,0,0,0, 0, 0, NULL},
                                  {SAME_SIZE, 0, 0, 0,0,0,0,0,0,0,0, 0,5, NULL},
                                  {SAME_SIZE, 0, 0, 0,0,0,0,0,0,0,0, 0,5,  NULL},
                                  {SAME_SIZE, 0, 0, 0,0,0,0,0,0,0,0, 0,5,  NULL},
                                  {SAME_SIZE, 0, 0, 0,0,0,0,0,0,0,0, 0,5,  NULL},
                                  {SAME_SIZE, 0, 0, 0,0,0,0,0,0,0,0, 0,5,  NULL},                                  
                                }};
                                
                                SET_CELL_DATA (1, 11);
                        }

                        break;
                case GNUCASH_CURSOR_DOUBLE:
                        {
                                int i, j;
                                double perc[2][11] = {{0.09, 0.06, 0.11, 0.23, 0.01, 0.10, 0.10, 0.07, 0.07, 0.07, 0.09},
                                {0.0, 0.15, 0.11, 0.74, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0}};
                                CellLayoutInfo li[2][11] =
                                {{{STRING_FIXED, 0, 0,0,0,0,0,0,0,0,0, 0, 0, " 88/88/8888"},
                                  {CHARS_MIN | CHARS_MAX, 0, 0, 3, 0, 5,0,0,0,0,0, 0, 0, NULL},
                                  {STRING_MIN | CHARS_MAX | FILL, 0, 0, 0 ,0, 10 ,0,0,0,0,0, 0, 0, "Transfer From"},
                                  {CHARS_MIN | FILL, 0, 0,20,0,0,0,0,0,0,0, 0,0, NULL},
                                  {STRING_FIXED, 1, 0,0,0,0,0,0,0,0,0, 0, 0, "R"},
                                  {CHARS_MIN | CHARS_MAX | FILL, 0, 0, 9,0, 10,0,0,0,0,0, 0, 0, NULL},
                                  {SAME_SIZE, 0, 0, 0,0,0,0,0,0,0,0, 0,5, NULL},
                                  {SAME_SIZE, 0, 0, 0,0,0,0,0,0,0,0, 0,5,  NULL},
                                  {SAME_SIZE, 0, 0, 0,0,0,0,0,0,0,0, 0,5,  NULL},
                                  {SAME_SIZE, 0, 0, 0,0,0,0,0,0,0,0, 0,5,  NULL},
                                  {SAME_SIZE, 0, 0, 0,0,0,0,0,0,0,0, 0,5,  NULL}},                                  
                                  {{LEFT_ALIGNED|RIGHT_ALIGNED, 0, 0, 0,0,0,0,0,0,0,0, 0,0, NULL},
                                   {LEFT_ALIGNED|RIGHT_ALIGNED, 0, 0, 0,0,0,0,0,1,0,1, 0,0, NULL},
                                   {LEFT_ALIGNED|RIGHT_ALIGNED, 0, 0, 0,0,0,0,0,2,0,2, 0,0, NULL},
                                   {LEFT_ALIGNED|RIGHT_ALIGNED, 0, 0, 0,0,0,0,0,3,0,10, 0,0, NULL},
                                   {PIXELS_FIXED, 0, 0, 0,0,0,0,0,0,0,0, 0,0, NULL},
                                   {PIXELS_FIXED, 0, 0, 0,0,0,0,0,0,0,0, 0,0, NULL},
                                   {PIXELS_FIXED, 0, 0, 0,0,0,0,0,0,0,0, 0,0, NULL},
                                   {PIXELS_FIXED, 0, 0, 0,0,0,0,0,0,0,0, 0,0, NULL},
                                   {PIXELS_FIXED, 0, 0, 0,0,0,0,0,0,0,0, 0,0, NULL},
                                   {PIXELS_FIXED, 0, 0, 0,0,0,0,0,0,0,0, 0,0, NULL},
                                   {PIXELS_FIXED, 0, 0, 0,0,0,0,0,0,0,0, 0,0, NULL},
                                }};

                                SET_CELL_DATA (2, 11);
                        }

                        break;
                case GNUCASH_CURSOR_TRANS:
                        {
                                int i, j;
                                double perc[1][11] = {{0.09, 0.06, 0.11, 0.23, 0.01, 0.10, 0.10, 0.07, 0.07, 0.07, 0.09}};
                                CellLayoutInfo li[1][11] =
                                {{{STRING_FIXED, 0, 0,0,0,0,0,0,0,0,0, 0, 0, " 88/88/8888"},
                                  {CHARS_MIN | CHARS_MAX, 0, 0, 3, 0, 5,0,0,0,0,0, 0, 0, NULL},
                                  {STRING_MIN | CHARS_MAX | FILL, 0, 0, 0 ,0, 10 ,0,0,0,0,0, 0, 0, "Transfer From"},
                                  {CHARS_MIN | FILL, 0, 0,20,0,0,0,0,0,0,0, 0,0, NULL},
                                  {STRING_FIXED, 1, 0,0,0,0,0,0,0,0,0, 0, 0, "R"},
                                  {CHARS_MIN | CHARS_MAX | FILL, 0, 0, 9,0, 10,0,0,0,0,0, 0, 0, NULL},
                                  {SAME_SIZE, 0, 0, 0,0,0,0,0,0,0,0, 0,5, NULL},
                                  {SAME_SIZE, 0, 0, 0,0,0,0,0,0,0,0, 0,5,  NULL},
                                  {SAME_SIZE, 0, 0, 0,0,0,0,0,0,0,0, 0,5,  NULL},
                                  {SAME_SIZE, 0, 0, 0,0,0,0,0,0,0,0, 0,5,  NULL},
                                  {SAME_SIZE, 0, 0, 0,0,0,0,0,0,0,0, 0,5,  NULL},                                  
                                }};
                                
                                SET_CELL_DATA (1, 11);
                        }
                        
                        break;
                case GNUCASH_CURSOR_SPLIT:
                        {
                                int i, j;
                                double perc[1][11] = {{0.0, 0.15, 0.11, 0.24, 0.0, 0.10, 0.10, 0.36, 0.0, 0.0, 0.0}};
                                CellLayoutInfo li[1][11] =
                                {{{STRING_FIXED, 0, 0,0,0,0,0,0,0,0,0, 0, 0, " 88/88/8888"},
                                  {CHARS_MIN | CHARS_MAX, 0, 0, 3, 0, 5,0,0,0,0,0, 0, 0, NULL},
                                  {STRING_MIN | CHARS_MAX | FILL, 0, 0, 0 ,0, 10 ,0,0,0,0,0, 0, 0, "Transfer From"},
                                  {CHARS_MIN | FILL, 0, 0,20,0,0,0,0,0,0,0, 0,0, NULL},
                                  {STRING_FIXED, 1, 0,0,0,0,0,0,0,0,0, 0, 0, "R"},
                                  {CHARS_MIN | CHARS_MAX | FILL, 0, 0, 9,0, 10,0,0,0,0,0, 0, 0, NULL},
                                  {SAME_SIZE, 0, 0, 0,0,0,0,0,0,0,0, 0,5, NULL},
                                  {SAME_SIZE, 0, 0, 0,0,0,0,0,0,0,0, 0,5,  NULL},
                                  {SAME_SIZE, 0, 0, 0,0,0,0,0,0,0,0, 0,5,  NULL},
                                  {SAME_SIZE, 0, 0, 0,0,0,0,0,0,0,0, 0,5,  NULL},
                                  {SAME_SIZE, 0, 0, 0,0,0,0,0,0,0,0, 0,5,  NULL},                                  
                                }};
                                
                                SET_CELL_DATA (1, 11);
                        }

                        break;
                }
        default:
                break;
        }
}

static int
compute_row_width (SheetBlockStyle *style, int row, int col1, int col2)
{
        int j;
        int width = 0;

        col1 = MAX(0, col1);
        col2 = MIN(col2, style->ncols-1);

        for (j = col1; j <= col2; j++)
                width += style->pixel_widths[row][j];

        return width;
}


/*
 *   This sets the initial sizes of the cells, based on cell_perc's and the
 *   width allocation of the sheet
 */
static void
set_dimensions_pass_one (GnucashSheet *sheet, SheetBlockStyle *style, int row)
{
        int i = row, j;
        
        for (j = 0; j < style->ncols; j++) {
                        style->pixel_widths[i][j] = style->cell_perc[i][j] * style->width + 0.5;
                        style->pixel_heights[i][j] =
                                style->fonts[i][j]->ascent +
                                style->fonts[i][j]->descent +
                                2*CELL_VPADDING;
                }
                style->height += style->pixel_heights[i][0];
}

#define C_WIDTH gdk_string_measure (style->fonts[i][j], "X")


/* Now we add/subtract what's required to/from individual cells,
 *  taking into account min/max char/pixel/string widths
 *
 *  At the end we set all SAME_SIZE cells.
 */
static void
set_dimensions_pass_two (GnucashSheet *sheet, SheetBlockStyle *style, int row)
{
        int i = row, j;
        unsigned int flags;

        for (j = 0; j < style->ncols; j++) {
                flags = style->layout_info[i][j].flags;

                if  (flags & CHARS_FIXED) {
                        style->pixel_widths[i][j] =
                                style->layout_info[i][j].chars_width * C_WIDTH + 2*CELL_HPADDING;
                        style->layout_info[i][j].pixels_width = style->pixel_widths[i][j];
                        style->layout_info[i][j].flags |= PIXELS_FIXED;
                }
                else if (flags & PIXELS_FIXED) {
                        style->pixel_widths[i][j] = style->layout_info[i][j].pixels_width;
                }
                else if ((flags & STRING_FIXED) && style->layout_info[i][j].string){
                        style->pixel_widths[i][j] =
                                gdk_string_measure (style->fonts[i][j], style->layout_info[i][j].string) + 2*CELL_HPADDING;
                        style->layout_info[i][j].pixels_width = style->pixel_widths[i][j];
                        style->layout_info[i][j].flags |= PIXELS_FIXED;
                }


                if (flags & CHARS_MIN) {
                        style->layout_info[i][j].pixels_min =
                                style->layout_info[i][j].chars_min * C_WIDTH+ 2*CELL_HPADDING;
                        style->layout_info[i][j].flags |= PIXELS_MIN;                        

                        style->pixel_widths[i][j] =
                                MAX (style->layout_info[i][j].pixels_min, style->pixel_widths[i][j]);
                }
                else if (flags & PIXELS_MIN) {
                        style->pixel_widths[i][j] =
                                MAX (style->layout_info[i][j].pixels_min, style->pixel_widths[i][j]);
                }
                else if ((flags & STRING_MIN) &&  style->layout_info[i][j].string) {
                        style->layout_info[i][j].pixels_min =
                                gdk_string_measure (style->fonts[i][j], style->layout_info[i][j].string)+ 2*CELL_HPADDING;

                        style->layout_info[i][j].flags |= PIXELS_MIN;                        
                        style->pixel_widths[i][j] =
                                MAX (style->layout_info[i][j].pixels_min, style->pixel_widths[i][j]);
                }


                if (flags & CHARS_MAX) {
                        style->layout_info[i][j].pixels_max =
                                style->layout_info[i][j].chars_max * C_WIDTH+ 2*CELL_HPADDING;
                        style->layout_info[i][j].flags |= PIXELS_MAX;                        

                        style->pixel_widths[i][j] =
                                MIN (style->layout_info[i][j].pixels_max, style->pixel_widths[i][j]);
                }
                else if (flags & PIXELS_MAX) {
                        style->pixel_widths[i][j] =
                                MIN (style->layout_info[i][j].pixels_max, style->pixel_widths[i][j]);
                }
        }

        for (j = 0; j < style->ncols; j++)
                if (style->layout_info[i][j].flags & SAME_SIZE) {
                        int r = style->layout_info[i][j].size_r;
                        int c = style->layout_info[i][j].size_c;
                        style->pixel_widths[i][j] = style->pixel_widths[r][c];
                }
}


/* This is a subcase of pass_three: we have a very small amount of
 *  space to reallocate; let's just put it in the largest FILL cell,
 *  and damn the consequences; in the wierd case that there is no FILL
 *  cell at all, just stick it in the largest cell.
 */
static void
set_dimensions_plan_b (GnucashSheet *sheet, SheetBlockStyle *style,
                       int row, int space)
{
        int i = row, j;
        int fill_col = -1;
        int fill_w = 0;
        int col = 0;
        int w = 0;

        for (j = 0; j < style->ncols; j++) {

                if (style->layout_info[i][j].flags & FILL)
                        if (fill_w < style->pixel_widths[i][j]) {
                                fill_col = j;
                                fill_w = style->pixel_widths[i][j];
                        }

                if (w < style->pixel_widths[i][j]) {
                        col = j;
                        w = style->pixel_widths[i][j];
                }
        }

        if (fill_col > -1)
                style->pixel_widths[i][fill_col] -= space;
        else
                style->pixel_widths[i][col] -= space;
}



/* OK, this is the tricky one: we need to add/subtract left over or
 *  excess space from the FILL cells.  We'll try to adjust each one
 *  the same amount, but this is subject to any _MIN/_MAX
 *  restrictions, and we need to be careful with SAME_SIZE cells, too.
 *
 */
static void
set_dimensions_pass_three (GnucashSheet *sheet, SheetBlockStyle *style,
                           int row, int ideal_width)
{
        int i = row, j;
        int space = compute_row_width (style, row, 0, style->ncols-1) - ideal_width;
        int cellspace;
        int nfills;

        int *adjustments;
        int *done;

        adjustments = g_new0 (int, style->ncols);
        done = g_new0 (int, style->ncols);

        while (space != 0) {

                nfills = 0;
                /* count the number of fill cells we have left to work with */
                for (j = 0; j < style->ncols; j++) {
                        if ( (style->layout_info[i][j].flags & FILL) && !done[j])
                                nfills++;
                        else if (style->layout_info[i][j].flags & SAME_SIZE) {
                                int r = style->layout_info[i][j].size_r;
                                int c = style->layout_info[i][j].size_c;
                                if ((style->layout_info[r][c].flags & FILL) && !done[c])
                                        nfills++;
                        }
                }

                /* no more place to put/take away extra space; give up */
                if (nfills == 0)
                        break;


                /* We take care of small amounts of space in a special case */
                if ( (space < 0 ? -1 : 1)*space < nfills) {
                        set_dimensions_plan_b (sheet, style, row, space);
                        break;
                }


                /*  this is how much we should ideally adjust each cell */
                cellspace = space/nfills;

                /*  loop through again and do the best we can */
                for (j = 0; j < style->ncols; j++)
                        if ((style->layout_info[i][j].flags & FILL) && !done[j]) {
                                if (cellspace > 0) {
                                        if (style->layout_info[i][j].flags & PIXELS_MIN) {
                                                int allowed = style->pixel_widths[i][j]
                                                        - style->layout_info[i][j].pixels_min;
                                                adjustments[j] = MIN (allowed, cellspace);
                                                done[j] = (allowed < cellspace);
                                        }
                                        else adjustments[j] = cellspace;
                                        space -= adjustments[j];
                                }
                                else {
                                        if (style->layout_info[i][j].flags & PIXELS_MAX) {
                                                int allowed = style->pixel_widths[i][j]
                                                        - style->layout_info[i][j].pixels_min;
                                                adjustments[j] =  MAX (allowed, cellspace);
                                                done[j] = (allowed >cellspace);
                                        }
                                        else adjustments[j] = cellspace;
                                        space -= adjustments[j];
                                }
                        }

                /* adjust everything, including SAME_SIZE cells */
                for (j = 0; j < style->ncols; j++) {
                        if (style->layout_info[i][j].flags & SAME_SIZE) {
                                int r = style->layout_info[i][j].size_r;
                                int c = style->layout_info[i][j].size_c;
                                style->pixel_widths[i][j] -= adjustments[c];
                                space -= adjustments[c];
                        }
                        style->pixel_widths[i][j] -= adjustments[j];
                }

                for (j = 0; j < style->ncols; j++)
                        adjustments[j] = 0;
        }

        /* clean up */
        g_free (adjustments);
        g_free (done);
}


static void
set_dimensions_aligned (GnucashSheet *sheet, SheetBlockStyle *style, int row)
{
        int i = row, j;
        int c1, c2;
        int r;
        
        for (j = 0; j < style->ncols; j++) {

                if (!(style->layout_info[i][j].flags & (LEFT_ALIGNED | RIGHT_ALIGNED))) {
                        style->layout_info[i][j].pixels_max = 0;
                        style->layout_info[i][j].flags |= PIXELS_FIXED;
                        style->pixel_widths[i][j] = 0;
                }
                else {
                        r = style->layout_info[i][j].right_align_r;
                        c1 = style->layout_info[i][j].left_align_c;
                        c2 = style->layout_info[i][j].right_align_c;                

                        style->layout_info[i][j].pixels_width = compute_row_width (style, r, c1, c2);
                        style->layout_info[i][j].flags |= PIXELS_FIXED;
                        style->pixel_widths[i][j] = style->layout_info[i][j].pixels_width;
                }
        }
}


void
gnucash_sheet_style_set_dimensions (GnucashSheet *sheet,
				    SheetBlockStyle *style)
{
        int i;
        int ideal_width;
        
        g_return_if_fail (sheet != NULL);
        g_return_if_fail (GNUCASH_IS_SHEET (sheet));
        g_return_if_fail (style != NULL);

        style->height = 0;
        style->width = GTK_WIDGET (sheet)->allocation.width;
        ideal_width = style->width;

        /* First set the top rows */
        set_dimensions_pass_one (sheet, style, 0);
        set_dimensions_pass_two (sheet, style, 0);
        set_dimensions_pass_three (sheet, style, 0, ideal_width);

        /* style->width is fixed now */
        style->width = compute_row_width (style, 0, 0, style->ncols-1);

        /*  For now, let's treat the other rows as ALIGNED_ cells only */
        for (i = 1; i < style->nrows; i++) {
                set_dimensions_pass_one (sheet, style, i);
                set_dimensions_aligned (sheet, style, i);
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
                for (j = 0; j < style->ncols; j++)
                        g_free (style->layout_info[i][j].string);
                g_free(style->layout_info[i]);
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
        g_free(style->layout_info);
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
        style->layout_info = g_new0 (CellLayoutInfo *, cellblock->numRows);
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
                style->layout_info[i] = g_new0 (CellLayoutInfo, style->ncols);
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
                        style->header_font = gnucash_default_font;

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
                                style->alignments[i][j]  = GTK_JUSTIFY_RIGHT;
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
