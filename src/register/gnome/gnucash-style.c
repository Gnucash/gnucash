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

/*
 *  configure the cursor styles
 */

#include "config.h"

#include "gnucash-style.h"
#include "gnucash-sheet.h"
#include "gnucash-grid.h"
#include "gnucash-color.h"
#include "messages.h"

#include "date.h"


#define DEFAULT_FONT "-adobe-helvetica-medium-r-normal--*-120-*-*-*-*-*-*"
#define ITALIC_FONT "-adobe-helvetica-medium-o-normal--*-120-*-*-*-*-*-*"
#define DEFAULT_STYLE_WIDTH 680

GdkFont *gnucash_default_font = NULL;
GdkFont *gnucash_italic_font = NULL;


/*  layout info flags */
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

/* User mutable flags */
#define RESIZABLE (1 << 0)  /* User can resize */
#define MOVABLE (1 << 1)   /* User can move */

struct   _CellLayoutInfo
{
        unsigned int **flags;
        unsigned int **user_flags;  /* For user mutable flags */

        int **chars_width;
        int **pixels_width;
        int **chars_min;
        int **pixels_min;
        int **chars_max;
        int **pixels_max;
        short **left_align_r;   /* the alignment cells */
        short **left_align_c;
        short **right_align_r;   
        short **right_align_c;
        short **size_r;         /*  same size as this cell */
        short **size_c;
        char ***string;

        double **cell_perc;   /* for the cell layout; percentage of
                                 space this cell takes on its row */

        int refcount;
        int nrows;
        int ncols;
};

typedef struct 
{
        unsigned int flags;
        unsigned int user_flags;  /* For user mutable flags */

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
} CellLayoutData;

static RegisterBorders reg_borders = 0;

static char *
style_get_key (SheetBlockStyle *style)
{
        switch (style->cursor_type) {
        case GNUCASH_CURSOR_HEADER:
        case GNUCASH_CURSOR_SINGLE:
        case GNUCASH_CURSOR_TRANS:
        case GNUCASH_CURSOR_SPLIT:
                return "singles";
                break;

        case GNUCASH_CURSOR_DOUBLE:
                return "doubles";
                break;
        }

        g_warning("style_get_key: bad cursor type\n");
        return NULL;
}


static CellLayoutInfo *
style_layout_info_new (SheetBlockStyle *style)
{
        CellLayoutInfo *li;
        int i;

        li = g_new0 (CellLayoutInfo, 1);
        li->nrows = style->nrows;
        li->ncols = style->ncols;

        li->flags = g_new0(unsigned int *, style->nrows);
        li->user_flags = g_new0(unsigned int *, style->nrows);        
        li->chars_width = g_new0(int *, style->nrows);
        li->pixels_width = g_new0(int *, style->nrows);        
        li->chars_min = g_new0(int *, style->nrows);
        li->pixels_min = g_new0(int *, style->nrows);
        li->chars_max = g_new0(int *, style->nrows);
        li->pixels_max = g_new0(int *, style->nrows);
        li->left_align_r = g_new0(short *, style->nrows);
        li->left_align_c = g_new0(short *, style->nrows);
        li->right_align_r = g_new0(short *, style->nrows);
        li->right_align_c = g_new0(short *, style->nrows);
        li->size_r = g_new0(short *, style->nrows);
        li->size_c = g_new0(short *, style->nrows);
        li->string = g_new0 (char **, style->nrows);
        li->cell_perc = g_new0(double *, style->nrows);

        for (i = 0; i < style->nrows; i++) {
                li->flags[i] = g_new0(unsigned int, style->ncols);
                li->user_flags[i] = g_new0(unsigned int, style->ncols);
                li->chars_width[i] = g_new0(int, style->ncols);
                li->pixels_width[i] = g_new0(int, style->ncols);        
                li->chars_min[i] = g_new0(int, style->ncols);
                li->pixels_min[i] = g_new0(int, style->ncols);
                li->chars_max[i] = g_new0(int, style->ncols);
                li->pixels_max[i] = g_new0(int, style->ncols);
                li->left_align_r[i] = g_new0(short, style->ncols);
                li->left_align_c[i] = g_new0(short, style->ncols);
                li->right_align_r[i] = g_new0(short, style->ncols);
                li->right_align_c[i] = g_new0(short, style->ncols);
                li->size_r[i] = g_new0(short, style->ncols);
                li->size_c[i] = g_new0(short, style->ncols);
                li->string[i] = g_new0 (char *, style->ncols);
                li->cell_perc[i] = g_new0(double, style->ncols);
        }

        return li;
}

static void
style_layout_info_destroy (CellLayoutInfo *li)
{
        int i, j;

        for (i = 0; i < li->nrows; i++) {

                for (j = 0; j < li->ncols; j++) {
                        if (li->string[i][j])
                                g_free (li->string[i][j]);
                }

                g_free(li->flags[i]);
                g_free(li->user_flags[i]);
                g_free(li->chars_width[i]);
                g_free(li->pixels_width[i]);
                g_free(li->chars_min[i]);
                g_free(li->pixels_min[i]);
                g_free(li->chars_max[i]);
                g_free(li->pixels_max[i]);
                g_free(li->left_align_r[i]);
                g_free(li->left_align_c[i]);
                g_free(li->right_align_r[i]);
                g_free(li->right_align_c[i]);
                g_free(li->size_r[i]);
                g_free(li->size_c[i]);
                g_free(li->string[i]);
                g_free(li->cell_perc[i]);
        }

        g_free(li->flags);
        g_free(li->user_flags);        
        g_free(li->chars_width);
        g_free(li->pixels_width);
        g_free(li->chars_min);
        g_free(li->pixels_min);
        g_free(li->chars_max);
        g_free(li->pixels_max);
        g_free(li->left_align_r);
        g_free(li->left_align_c);
        g_free(li->right_align_r);
        g_free(li->right_align_c);
        g_free(li->size_r);
        g_free(li->size_c);
        g_free(li->string);
        g_free(li->cell_perc);

        g_free(li);
}


#define SET_CELL_LAYOUT_DATA(r, n) {  \
        for (i=0; i<r; i++)   \
           for (j=0; j<n; j++) {\
        layout_info->cell_perc[i][j] = perc[i][j];  \
        layout_info->flags[i][j] = ld[i][j].flags; \
        layout_info->user_flags[i][j] = ld[i][j].user_flags; \
        layout_info->chars_width[i][j] = ld[i][j].chars_width;\
        layout_info->pixels_width[i][j] = ld[i][j].pixels_width;\
        layout_info->chars_min[i][j] = ld[i][j].chars_min;\
        layout_info->pixels_min[i][j] = ld[i][j].pixels_min;\
        layout_info->chars_max[i][j] = ld[i][j].chars_max;\
        layout_info->pixels_max[i][j] = ld[i][j].pixels_max;\
        layout_info->right_align_r[i][j] = ld[i][j].right_align_r;\
        layout_info->right_align_c[i][j] = ld[i][j].right_align_c;\
        layout_info->left_align_r[i][j] = ld[i][j].left_align_r;\
        layout_info->left_align_c[i][j] = ld[i][j].left_align_c; \
        layout_info->size_r[i][j]  = ld[i][j].size_r;  \
        layout_info->size_c[i][j]  = ld[i][j].size_c;  \
        layout_info->string[i][j] = g_strdup(ld[i][j].string); \
      } \
}


static void
layout_init_normal(GnucashSheet *sheet, SheetBlockStyle *style)
{
        CellLayoutInfo *layout_info;
        char date_str[128];
        int i, j;

        double perc[1][8] = {{0.10, 0.07, 0.25, 0.20, 0.02, 0.12, 0.12, 0.12}};

        CellLayoutData ld[1][8] =
        {{{STRING_FIXED,RESIZABLE,0,0,0,0,0,0,0,0,0,0,0,0,date_str},
          {CHARS_MIN | CHARS_MAX,RESIZABLE,0,0,3,0,5,0,0,0,0,0,0,0,NULL},
          {CHARS_MIN | FILL,RESIZABLE,0,0,20,0,0,0,0,0,0,0,0,0,NULL},
          {STRING_MIN,RESIZABLE,0,0,0,0,10,0,0,0,0,0,0,0,XFRM_STR},
          {STRING_FIXED,0,1,0,0,0,0,0,0,0,0,0,0,0,"R"},
          {CHARS_MIN | CHARS_MAX,RESIZABLE,0,0,9,0,10,0,0,0,0,0,0,0,NULL},
          {SAME_SIZE,RESIZABLE,0,0,0,0,0,0,0,0,0,0,0,5,NULL},
          {SAME_SIZE,RESIZABLE,0,0,0,0,0,0,0,0,0,0,0,5,NULL},
        }};

        printDate(date_str, 29, 12, 2000);
        strcat(date_str, "0");

        layout_info = style_layout_info_new (style);

        SET_CELL_LAYOUT_DATA (1, 8);

        g_hash_table_insert (sheet->layout_info_hash_table,
                             style_get_key (style), layout_info);

        style->layout_info = layout_info;
        layout_info->refcount++;
}

static void
layout_init_ledger(GnucashSheet *sheet, SheetBlockStyle *style)
{
        CellLayoutInfo *layout_info;
        char date_str[128];
        int i, j;

        double perc[1][8] = {{0.10, 0.07, 0.21, 0.18, 0.18, 0.02, 0.12, 0.12}};

        CellLayoutData ld[1][8] =
        {{{STRING_FIXED,RESIZABLE,0,0,0,0,0,0,0,0,0,0,0,0,date_str},
          {CHARS_MIN | CHARS_MAX,RESIZABLE,0,0,3,0,5,0,0,0,0,0,0,0,NULL},
          {CHARS_MIN | FILL,RESIZABLE,0,0,20,0,0,0,0,0,0,0,0,0,NULL},
          {STRING_MIN,RESIZABLE,0,0,0,0,10,0,0,0,0,0,0,0,XFTO_STR},
          {STRING_MIN,RESIZABLE,0,0,0,0,10,0,0,0,0,0,0,0,XFRM_STR},
          {STRING_FIXED,0,1,0,0,0,0,0,0,0,0,0,0,0,"R"},
          {CHARS_MIN | CHARS_MAX,RESIZABLE,0,0,9,0,10,0,0,0,0,0,0,0,NULL},
          {SAME_SIZE,RESIZABLE,0,0,0,0,0,0,0,0,0,0,0,6,NULL},
        }};

        printDate(date_str, 29, 12, 2000);
        strcat(date_str, "0");

        layout_info = style_layout_info_new (style);

        SET_CELL_LAYOUT_DATA (1, 8);

        g_hash_table_insert (sheet->layout_info_hash_table,
                             style_get_key (style), layout_info);

        style->layout_info = layout_info;
        layout_info->refcount++;
}

static void
layout_init_double(GnucashSheet *sheet, SheetBlockStyle *style)
{
        CellLayoutInfo *layout_info;
        char date_str[128];
        int i, j;

        double perc[2][8] = {{0.10, 0.07, 0.25, 0.20, 0.02, 0.12, 0.12, 0.12},
                             {0.10, 0.07, 0.83, 0.00, 0.00, 0.00, 0.00, 0.00}};

        CellLayoutData ld[2][8] =
        {{{STRING_FIXED,RESIZABLE,0,0,0,0,0,0,0,0,0,0,0,0,date_str},
          {CHARS_MIN | CHARS_MAX,RESIZABLE,0,0,3,0,5,0,0,0,0,0,0,0,NULL},
          {CHARS_MIN | FILL,RESIZABLE,0,0,20,0,0,0,0,0,0,0,0,0,NULL},
          {STRING_MIN,RESIZABLE,0,0,0,0,0,0,0,0,0,0,0,0,XFRM_STR},
          {STRING_FIXED,0,1,0,0,0,0,0,0,0,0,0,0,0,"R"},
          {CHARS_MIN | CHARS_MAX,RESIZABLE,0,0,9,0,10,0,0,0,0,0,0,0,NULL},
          {SAME_SIZE,RESIZABLE,0,0,0,0,0,0,0,0,0,0,0,5,NULL},
          {SAME_SIZE,RESIZABLE,0,0,0,0,0,0,0,0,0,0,0,5,NULL}},
         {{LEFT_ALIGNED|RIGHT_ALIGNED,RESIZABLE,0,0,0,0,0,0,0,0,0,0,0,0,NULL},
          {LEFT_ALIGNED|RIGHT_ALIGNED,RESIZABLE,0,0,0,0,0,0,0,1,0,1,0,0,NULL},
          {LEFT_ALIGNED|RIGHT_ALIGNED,RESIZABLE,0,0,0,0,0,0,0,2,0,7,0,0,NULL},
          {PIXELS_FIXED,0,0,0,0,0,0,0,0,0,0,0,0,0,NULL},
          {PIXELS_FIXED,0,0,0,0,0,0,0,0,0,0,0,0,0,NULL},
          {PIXELS_FIXED,0,0,0,0,0,0,0,0,0,0,0,0,0,NULL},
          {PIXELS_FIXED,0,0,0,0,0,0,0,0,0,0,0,0,0,NULL},
          {PIXELS_FIXED,0,0,0,0,0,0,0,0,0,0,0,0,0,NULL},
         }};

        printDate(date_str, 29, 12, 2000);
        strcat(date_str, "0");

        layout_info = style_layout_info_new (style);

        SET_CELL_LAYOUT_DATA (2, 8);

        g_hash_table_insert (sheet->layout_info_hash_table,
                             style_get_key (style), layout_info);

        style->layout_info = layout_info;
        layout_info->refcount++;
}

static void
layout_init_ledger_double(GnucashSheet *sheet, SheetBlockStyle *style)
{
        CellLayoutInfo *layout_info;
        char date_str[128];
        int i, j;

        double perc[2][8] = {{0.10, 0.07, 0.21, 0.18, 0.18, 0.02, 0.12, 0.12},
                             {0.10, 0.07, 0.83, 0.00, 0.00, 0.00, 0.00, 0.00}};

        CellLayoutData ld[2][8] =
        {{{STRING_FIXED,RESIZABLE,0,0,0,0,0,0,0,0,0,0,0,0,date_str},
          {CHARS_MIN | CHARS_MAX,RESIZABLE,0,0,3,0,5,0,0,0,0,0,0,0,NULL},
          {CHARS_MIN | FILL,RESIZABLE,0,0,20,0,0,0,0,0,0,0,0,0,NULL},
          {STRING_MIN,RESIZABLE,0,0,0,0,0,0,0,0,0,0,0,0,XFTO_STR},
          {STRING_MIN,RESIZABLE,0,0,0,0,0,0,0,0,0,0,0,0,XFRM_STR},
          {STRING_FIXED,0,1,0,0,0,0,0,0,0,0,0,0,0,"R"},
          {CHARS_MIN | CHARS_MAX,RESIZABLE,0,0,9,0,10,0,0,0,0,0,0,0,NULL},
          {SAME_SIZE,RESIZABLE,0,0,0,0,0,0,0,0,0,0,0,6,NULL}},
         {{LEFT_ALIGNED|RIGHT_ALIGNED,RESIZABLE,0,0,0,0,0,0,0,0,0,0,0,0,NULL},
          {LEFT_ALIGNED|RIGHT_ALIGNED,RESIZABLE,0,0,0,0,0,0,0,1,0,1,0,0,NULL},
          {LEFT_ALIGNED|RIGHT_ALIGNED,RESIZABLE,0,0,0,0,0,0,0,2,0,7,0,0,NULL},
          {PIXELS_FIXED,0,0,0,0,0,0,0,0,0,0,0,0,0,NULL},
          {PIXELS_FIXED,0,0,0,0,0,0,0,0,0,0,0,0,0,NULL},
          {PIXELS_FIXED,0,0,0,0,0,0,0,0,0,0,0,0,0,NULL},
          {PIXELS_FIXED,0,0,0,0,0,0,0,0,0,0,0,0,0,NULL},
          {PIXELS_FIXED,0,0,0,0,0,0,0,0,0,0,0,0,0,NULL},
         }};

        printDate(date_str, 29, 12, 2000);
        strcat(date_str, "0");

        layout_info = style_layout_info_new (style);

        SET_CELL_LAYOUT_DATA (2, 8);

        g_hash_table_insert (sheet->layout_info_hash_table,
                             style_get_key (style), layout_info);

        style->layout_info = layout_info;
        layout_info->refcount++;
}

static void
layout_init_stock(GnucashSheet *sheet, SheetBlockStyle *style)
{
        CellLayoutInfo *layout_info;
        char date_str[128];
        int i, j;

        double perc[1][11] = {{0.09, 0.06, 0.20, 0.14, 0.01, 0.10,
                               0.10, 0.07, 0.07, 0.07, 0.09}};

        CellLayoutData ld[1][11] =
        {{{STRING_FIXED,RESIZABLE, 0,0,0,0,0,0,0,0,0,0,0,0,date_str},
          {CHARS_MIN | CHARS_MAX,RESIZABLE,0,0,3,0,5,0,0,0,0,0,0,0,NULL},
          {CHARS_MIN | FILL,RESIZABLE,0,0,20,0,0,0,0,0,0,0,0,0,NULL},
          {STRING_MIN,RESIZABLE,0,0,0,0,0,0,0,0,0,0,0,0,XFRM_STR},
          {STRING_FIXED,0,1,0,0,0,0,0,0,0,0,0,0,0,"R"},
          {CHARS_MIN | CHARS_MAX,RESIZABLE,0,0,9,0,10,0,0,0,0,0,0,0,NULL},
          {SAME_SIZE,RESIZABLE,0,0,0,0,0,0,0,0,0,0,0,5,NULL},
          {SAME_SIZE,RESIZABLE,0,0,0,0,0,0,0,0,0,0,0,5,NULL},
          {SAME_SIZE,RESIZABLE,0,0,0,0,0,0,0,0,0,0,0,5,NULL},
          {SAME_SIZE,RESIZABLE,0,0,0,0,0,0,0,0,0,0,0,5,NULL},
          {SAME_SIZE,RESIZABLE,0,0,0,0,0,0,0,0,0,0,0,5,NULL},
        }};

        printDate(date_str, 29, 12, 2000);
        strcat(date_str, "0");

        layout_info = style_layout_info_new (style);

        SET_CELL_LAYOUT_DATA (1, 11);

        g_hash_table_insert (sheet->layout_info_hash_table,
                             style_get_key (style), layout_info);

        style->layout_info = layout_info;
        layout_info->refcount++;
}

static void
layout_init_stock_ledger(GnucashSheet *sheet, SheetBlockStyle *style)
{
        CellLayoutInfo *layout_info;
        char date_str[128];
        int i, j;

        double perc[1][11] = {{0.09, 0.05, 0.18, 0.14, 0.14, 0.01,
                               0.09, 0.09, 0.07, 0.07, 0.07}};

        CellLayoutData ld[1][11] =
        {{{STRING_FIXED,RESIZABLE, 0,0,0,0,0,0,0,0,0,0,0,0,date_str},
          {CHARS_MIN | CHARS_MAX,RESIZABLE,0,0,3,0,5,0,0,0,0,0,0,0,NULL},
          {CHARS_MIN | FILL,RESIZABLE,0,0,20,0,0,0,0,0,0,0,0,0,NULL},
          {STRING_MIN,RESIZABLE,0,0,0,0,0,0,0,0,0,0,0,0,XFTO_STR},
          {STRING_MIN,RESIZABLE,0,0,0,0,0,0,0,0,0,0,0,0,XFRM_STR},
          {STRING_FIXED,0,1,0,0,0,0,0,0,0,0,0,0,0,"R"},
          {CHARS_MIN | CHARS_MAX,RESIZABLE,0,0,9,0,10,0,0,0,0,0,0,0,NULL},
          {SAME_SIZE,RESIZABLE,0,0,0,0,0,0,0,0,0,0,0,6,NULL},
          {SAME_SIZE,RESIZABLE,0,0,0,0,0,0,0,0,0,0,0,6,NULL},
          {SAME_SIZE,RESIZABLE,0,0,0,0,0,0,0,0,0,0,0,6,NULL},
          {SAME_SIZE,RESIZABLE,0,0,0,0,0,0,0,0,0,0,0,6,NULL},
        }};

        printDate(date_str, 29, 12, 2000);
        strcat(date_str, "0");

        layout_info = style_layout_info_new (style);

        SET_CELL_LAYOUT_DATA (1, 11);

        g_hash_table_insert (sheet->layout_info_hash_table,
                             style_get_key (style), layout_info);

        style->layout_info = layout_info;
        layout_info->refcount++;
}

static void
layout_init_stock_double(GnucashSheet *sheet, SheetBlockStyle *style)
{
        CellLayoutInfo *layout_info;
        char date_str[128];
        int i, j;

        double perc[2][11] = {{0.09, 0.06, 0.20, 0.14, 0.01,
                               0.10, 0.10, 0.07, 0.07, 0.07, 0.09},
                              {0.00, 0.15, 0.11, 0.74, 0.00,
                               0.00, 0.00, 0.00, 0.00, 0.00, 0.0}};

        CellLayoutData ld[2][11] =
        {{{STRING_FIXED, RESIZABLE,0,0,0,0,0,0,0,0,0,0,0,0,date_str},
          {CHARS_MIN | CHARS_MAX,RESIZABLE,0,0,3,0,5,0,0,0,0,0,0,0,NULL},
          {CHARS_MIN,RESIZABLE,0,0,20,0,0,0,0,0,0,0,0,0,NULL},
          {STRING_MIN | FILL,RESIZABLE,0,0,0,0,0,0,0,0,0,0,0,0,XFRM_STR},
          {STRING_FIXED, 0,1,0,0,0,0,0,0,0,0,0,0,0,"R"},
          {CHARS_MIN | CHARS_MAX,RESIZABLE,0,0,9,0,10,0,0,0,0,0,0,0,NULL},
          {SAME_SIZE,RESIZABLE,0,0,0,0,0,0,0,0,0,0,0,5,NULL},
          {SAME_SIZE,RESIZABLE,0,0,0,0,0,0,0,0,0,0,0,5,NULL},
          {SAME_SIZE,RESIZABLE,0,0,0,0,0,0,0,0,0,0,0,5,NULL},
          {SAME_SIZE,RESIZABLE,0,0,0,0,0,0,0,0,0,0,0,5,NULL},
          {SAME_SIZE,RESIZABLE,0,0,0,0,0,0,0,0,0,0,0,5,NULL}},
         {{LEFT_ALIGNED|RIGHT_ALIGNED,RESIZABLE,0,0,0,0,0,0,0,0,0,0,0,0,NULL},
          {LEFT_ALIGNED|RIGHT_ALIGNED,RESIZABLE,0,0,0,0,0,0,0,1,0,1,0,0,NULL},
          {LEFT_ALIGNED|RIGHT_ALIGNED,RESIZABLE,0,0,0,0,0,0,0,2,0,10,0,0,NULL},
          {PIXELS_FIXED,0,0,0,0,0,0,0,0,0,0,0,0,0,NULL},
          {PIXELS_FIXED,0,0,0,0,0,0,0,0,0,0,0,0,0,NULL},
          {PIXELS_FIXED,0,0,0,0,0,0,0,0,0,0,0,0,0,NULL},
          {PIXELS_FIXED,0,0,0,0,0,0,0,0,0,0,0,0,0,NULL},
          {PIXELS_FIXED,0,0,0,0,0,0,0,0,0,0,0,0,0,NULL},
          {PIXELS_FIXED,0,0,0,0,0,0,0,0,0,0,0,0,0,NULL},
          {PIXELS_FIXED,0,0,0,0,0,0,0,0,0,0,0,0,0,NULL},
          {PIXELS_FIXED,0,0,0,0,0,0,0,0,0,0,0,0,0,NULL},
         }};

        printDate(date_str, 29, 12, 2000);
        strcat(date_str, "0");

        layout_info = style_layout_info_new (style);

        SET_CELL_LAYOUT_DATA (2, 11);

        g_hash_table_insert (sheet->layout_info_hash_table,
                             style_get_key (style), layout_info);

        style->layout_info = layout_info;
        layout_info->refcount++;
}

static void
layout_init_stock_ledger_double(GnucashSheet *sheet, SheetBlockStyle *style)
{
        CellLayoutInfo *layout_info;
        char date_str[128];
        int i, j;

        double perc[2][11] = {{0.09, 0.05, 0.18, 0.14, 0.14,
                               0.01, 0.09, 0.09, 0.07, 0.07, 0.07},
                              {0.00, 0.15, 0.11, 0.74, 0.00,
                               0.00, 0.00, 0.00, 0.00, 0.00, 0.0}};

        CellLayoutData ld[2][11] =
        {{{STRING_FIXED, RESIZABLE,0,0,0,0,0,0,0,0,0,0,0,0,date_str},
          {CHARS_MIN | CHARS_MAX,RESIZABLE,0,0,3,0,5,0,0,0,0,0,0,0,NULL},
          {CHARS_MIN,RESIZABLE,0,0,20,0,0,0,0,0,0,0,0,0,NULL},
          {STRING_MIN | FILL,RESIZABLE,0,0,0,0,0,0,0,0,0,0,0,0,XFTO_STR},
          {STRING_MIN | FILL,RESIZABLE,0,0,0,0,0,0,0,0,0,0,0,0,XFRM_STR},
          {STRING_FIXED, 0,1,0,0,0,0,0,0,0,0,0,0,0,"R"},
          {CHARS_MIN | CHARS_MAX,RESIZABLE,0,0,9,0,10,0,0,0,0,0,0,0,NULL},
          {SAME_SIZE,RESIZABLE,0,0,0,0,0,0,0,0,0,0,0,6,NULL},
          {SAME_SIZE,RESIZABLE,0,0,0,0,0,0,0,0,0,0,0,6,NULL},
          {SAME_SIZE,RESIZABLE,0,0,0,0,0,0,0,0,0,0,0,6,NULL},
          {SAME_SIZE,RESIZABLE,0,0,0,0,0,0,0,0,0,0,0,6,NULL}},
         {{LEFT_ALIGNED|RIGHT_ALIGNED,RESIZABLE,0,0,0,0,0,0,0,0,0,0,0,0,NULL},
          {LEFT_ALIGNED|RIGHT_ALIGNED,RESIZABLE,0,0,0,0,0,0,0,1,0,1,0,0,NULL},
          {LEFT_ALIGNED|RIGHT_ALIGNED,RESIZABLE,0,0,0,0,0,0,0,2,0,10,0,0,NULL},
          {PIXELS_FIXED,0,0,0,0,0,0,0,0,0,0,0,0,0,NULL},
          {PIXELS_FIXED,0,0,0,0,0,0,0,0,0,0,0,0,0,NULL},
          {PIXELS_FIXED,0,0,0,0,0,0,0,0,0,0,0,0,0,NULL},
          {PIXELS_FIXED,0,0,0,0,0,0,0,0,0,0,0,0,0,NULL},
          {PIXELS_FIXED,0,0,0,0,0,0,0,0,0,0,0,0,0,NULL},
          {PIXELS_FIXED,0,0,0,0,0,0,0,0,0,0,0,0,0,NULL},
          {PIXELS_FIXED,0,0,0,0,0,0,0,0,0,0,0,0,0,NULL},
          {PIXELS_FIXED,0,0,0,0,0,0,0,0,0,0,0,0,0,NULL},
         }};

        printDate(date_str, 29, 12, 2000);
        strcat(date_str, "0");

        layout_info = style_layout_info_new (style);

        SET_CELL_LAYOUT_DATA (2, 11);

        g_hash_table_insert (sheet->layout_info_hash_table,
                             style_get_key (style), layout_info);

        style->layout_info = layout_info;
        layout_info->refcount++;
}

/* FIXME:  read this from a config file */
/* keep this in sync with splitreg.c */
void
gnucash_style_layout_init (GnucashSheet *sheet, SheetBlockStyle *style)
{
        CellLayoutInfo *layout_info;

        layout_info = g_hash_table_lookup (sheet->layout_info_hash_table,
                                           style_get_key (style));

        if (layout_info != NULL)
        {
                style->layout_info = layout_info;
                layout_info->refcount++;

                return;
        }

        switch (style->reg_type) {
                case BANK_REGISTER:
                case CASH_REGISTER:
                case ASSET_REGISTER:
                case CREDIT_REGISTER:
                case LIABILITY_REGISTER:
                case INCOME_REGISTER:
                case EXPENSE_REGISTER:
                case EQUITY_REGISTER:
                        switch (style->cursor_type) {
                                case GNUCASH_CURSOR_HEADER:
                                case GNUCASH_CURSOR_SINGLE:
                                case GNUCASH_CURSOR_TRANS:
                                case GNUCASH_CURSOR_SPLIT:
                                        layout_init_normal(sheet, style);
                                        break;
                                case GNUCASH_CURSOR_DOUBLE:
                                        layout_init_double(sheet, style);
                                        break;
                                default:
                                        break;
                        }
                        break;
                case INCOME_LEDGER: 
                case GENERAL_LEDGER:
                case SEARCH_LEDGER:
                        switch (style->cursor_type) {
                                case GNUCASH_CURSOR_HEADER:
                                case GNUCASH_CURSOR_SINGLE:
                                case GNUCASH_CURSOR_TRANS:
                                case GNUCASH_CURSOR_SPLIT:
                                        layout_init_ledger(sheet, style);
                                        break;
                                case GNUCASH_CURSOR_DOUBLE:
                                        layout_init_ledger_double(sheet, style);
                                        break;
                                default:
                                        break;
                        }
                        break;
                case STOCK_REGISTER:
                case CURRENCY_REGISTER:
                        switch (style->cursor_type) {
                                case GNUCASH_CURSOR_HEADER:
                                case GNUCASH_CURSOR_SINGLE:
                                case GNUCASH_CURSOR_TRANS:
                                case GNUCASH_CURSOR_SPLIT:
                                        layout_init_stock(sheet, style);
                                        break;
                                case GNUCASH_CURSOR_DOUBLE:
                                        layout_init_stock_double(sheet, style);
                                        break;
                        }
                        break;
                case PORTFOLIO_LEDGER:
                        switch (style->cursor_type) {
                                case GNUCASH_CURSOR_HEADER:
                                case GNUCASH_CURSOR_SINGLE:
                                case GNUCASH_CURSOR_TRANS:
                                case GNUCASH_CURSOR_SPLIT:
                                        layout_init_stock_ledger(sheet, style);
                                        break;
                                case GNUCASH_CURSOR_DOUBLE:
                                        layout_init_stock_ledger_double(sheet, style);
                                        break;
                        }
                        break;
                default:
                        break;
        }
}

static CellDimensions *
style_dimensions_new (SheetBlockStyle *style)
{
        CellDimensions *dimensions;
        int i, j;

        dimensions = g_new0 (CellDimensions, 1);
        dimensions->nrows = style->nrows;
        dimensions->ncols = style->ncols;

        dimensions->pixel_heights = g_new0 (gint *, style->nrows);
        dimensions->pixel_widths = g_new0 (gint *, style->nrows);
        dimensions->origin_x = g_new0 (gint *, style->nrows);
        dimensions->origin_y = g_new0 (gint *, style->nrows);        

        for (i=0; i < style->nrows; i++) {
                dimensions->pixel_heights[i] = g_new0 (gint, style->ncols);
                dimensions->pixel_widths[i] = g_new0 (gint, style->ncols);

                for (j = 0; j < style->ncols; j++)
                        dimensions->pixel_widths[i][j] = -1;
                
                dimensions->origin_x[i] = g_new0 (gint, style->ncols);
                dimensions->origin_y[i] = g_new0 (gint, style->ncols);        
        }

        

        return dimensions;
}

void
style_dimensions_destroy (CellDimensions *dimensions)
{
        int i;

        for (i=0; i < dimensions->nrows; i++) {
                g_free(dimensions->pixel_heights[i]);
                g_free(dimensions->pixel_widths[i]);
                g_free(dimensions->origin_x[i]);
                g_free(dimensions->origin_y[i]);
        }
        
        g_free(dimensions->pixel_heights);
        g_free(dimensions->pixel_widths);
        g_free(dimensions->origin_x);
        g_free(dimensions->origin_y);

        g_free(dimensions);
}


static void
gnucash_style_dimensions_init (GnucashSheet *sheet, SheetBlockStyle *style)
{
        CellDimensions *dimensions;

        dimensions = g_hash_table_lookup (sheet->dimensions_hash_table, style_get_key (style));


        if (!dimensions) {
                dimensions = style_dimensions_new (style);
                g_hash_table_insert (sheet->dimensions_hash_table, style_get_key (style), dimensions);
        }
        
        dimensions->refcount++;

        style->dimensions = dimensions;
}


static int
compute_row_width (CellDimensions *dimensions, int row, int col1, int col2)
{
        int j;
        int width = 0;

        col1 = MAX(0, col1);
        col2 = MIN(col2, dimensions->ncols-1);

        for (j = col1; j <= col2; j++)
                width += dimensions->pixel_widths[row][j];

        return width;
}


/*
 *   This sets the initial sizes of the cells, based on cell_perc's and the
 *   width allocation of the sheet
 */
static void
set_dimensions_pass_one (GnucashSheet *sheet, CellLayoutInfo *layout_info,
                         CellDimensions *dimensions, int row)
{
        int i = row, j;
        GdkFont *font = GNUCASH_GRID(sheet->grid)->normal_font;

        g_return_if_fail (font != NULL);
        
        for (j = 0; j < layout_info->ncols; j++) {

                if (dimensions->pixel_widths[i][j] < 0)
                        dimensions->pixel_widths[i][j] = layout_info->cell_perc[i][j] * dimensions->width + 0.5;
                dimensions->pixel_heights[i][j] =
                        font->ascent + font->descent +
                        2*CELL_VPADDING;
                }
                dimensions->height += dimensions->pixel_heights[i][0];
}

/*  Attempt to convert character width to pixels */
#define C_WIDTH gdk_string_measure (GNUCASH_GRID(sheet->grid)->normal_font, "X")


/* Now we add/subtract what's required to/from individual cells,
 *  taking into account min/max char/pixel/string widths
 *
 *  At the end we set all SAME_SIZE cells.
 */
static void
set_dimensions_pass_two (GnucashSheet *sheet, CellLayoutInfo *layout_info,
                         CellDimensions *dimensions, int row)
{
        int i = row, j;
        unsigned int flags;
        GdkFont *font = GNUCASH_GRID(sheet->grid)->normal_font;

        for (j = 0; j < layout_info->ncols; j++) {
                flags = layout_info->flags[i][j];

                if  (flags & CHARS_FIXED) {
                        dimensions->pixel_widths[i][j] =
                                layout_info->chars_width[i][j] * C_WIDTH + 2*CELL_HPADDING;
                        layout_info->pixels_width[i][j] = dimensions->pixel_widths[i][j];
                        layout_info->flags[i][j] |= PIXELS_FIXED;
                }
                else if (flags & PIXELS_FIXED) {
                        dimensions->pixel_widths[i][j] = layout_info->pixels_width[i][j];
                }
                else if ((flags & STRING_FIXED) && layout_info->string[i][j]){
                        dimensions->pixel_widths[i][j] =
                                gdk_string_measure (font, layout_info->string[i][j]) + 2*CELL_HPADDING;
                        layout_info->pixels_width[i][j] = dimensions->pixel_widths[i][j];
                        layout_info->flags[i][j] |= PIXELS_FIXED;
                }


                if (flags & CHARS_MIN) {
                        layout_info->pixels_min[i][j] =
                                layout_info->chars_min[i][j] * C_WIDTH+ 2*CELL_HPADDING;
                        layout_info->flags[i][j] |= PIXELS_MIN;                        

                        dimensions->pixel_widths[i][j] =
                                MAX (layout_info->pixels_min[i][j], dimensions->pixel_widths[i][j]);
                }
                else if (flags & PIXELS_MIN) {
                        dimensions->pixel_widths[i][j] =
                                MAX (layout_info->pixels_min[i][j], dimensions->pixel_widths[i][j]);
                }
                else if ((flags & STRING_MIN) &&  layout_info->string[i][j]) {
                        layout_info->pixels_min[i][j] =
                                gdk_string_measure (font, layout_info->string[i][j])+ 2*CELL_HPADDING;

                        layout_info->flags[i][j] |= PIXELS_MIN;                        
                        dimensions->pixel_widths[i][j] =
                                MAX (layout_info->pixels_min[i][j], dimensions->pixel_widths[i][j]);
                }


                if (flags & CHARS_MAX) {
                        layout_info->pixels_max[i][j] =
                                layout_info->chars_max[i][j] * C_WIDTH+ 2*CELL_HPADDING;
                        layout_info->flags[i][j] |= PIXELS_MAX;                        

                        dimensions->pixel_widths[i][j] =
                                MIN (layout_info->pixels_max[i][j], dimensions->pixel_widths[i][j]);
                }
                else if (flags & PIXELS_MAX) {
                        dimensions->pixel_widths[i][j] =
                                MIN (layout_info->pixels_max[i][j], dimensions->pixel_widths[i][j]);
                }
        }

        for (j = 0; j < layout_info->ncols; j++)
                if (layout_info->flags[i][j] & SAME_SIZE) {
                        int r = layout_info->size_r[i][j];
                        int c = layout_info->size_c[i][j];
                        dimensions->pixel_widths[i][j] = dimensions->pixel_widths[r][c];
                }
}


/* This is a subcase of pass_three: we have a very small amount of
 *  space to reallocate; let's just put it in the largest FILL cell,
 *  and damn the consequences; in the wierd case that there is no FILL
 *  cell at all, just stick it in the largest cell.
 */
static void
set_dimensions_plan_b (GnucashSheet *sheet, CellLayoutInfo *layout_info,
                       CellDimensions *dimensions,
                       int row, int space)
{
        int i = row, j;
        int fill_col = -1;
        int fill_w = 0;
        int col = 0;
        int w = 0;

        for (j = 0; j < layout_info->ncols; j++) {

                if (layout_info->flags[i][j] & FILL)
                        if (fill_w < dimensions->pixel_widths[i][j]) {
                                fill_col = j;
                                fill_w = dimensions->pixel_widths[i][j];
                        }

                if (w < dimensions->pixel_widths[i][j]) {
                        col = j;
                        w = dimensions->pixel_widths[i][j];
                }
        }

        if (fill_col > -1)
                dimensions->pixel_widths[i][fill_col] -= space;
        else
                dimensions->pixel_widths[i][col] -= space;
}



/* OK, this is the tricky one: we need to add/subtract left over or
 *  excess space from the FILL cells.  We'll try to adjust each one
 *  the same amount, but this is subject to any _MIN/_MAX
 *  restrictions, and we need to be careful with SAME_SIZE cells, too.
 *
 */
static void
set_dimensions_pass_three (GnucashSheet *sheet, CellLayoutInfo *layout_info,
                           CellDimensions *dimensions,
                           int row, int ideal_width)
{
        int i = row, j;
        int space = compute_row_width (dimensions, row, 0, dimensions->ncols-1) - ideal_width;
        int cellspace;
        int nfills;

        int *adjustments;
        int *done;

        adjustments = g_new0 (int, layout_info->ncols);
        done = g_new0 (int, layout_info->ncols);

        while (space != 0) {

                nfills = 0;
                /* count the number of fill cells we have left to work with */
                for (j = 0; j < layout_info->ncols; j++) {
                        if ( (layout_info->flags[i][j] & FILL) && !done[j])
                                nfills++;
                        else if (layout_info->flags[i][j] & SAME_SIZE) {
                                int c = layout_info->size_c[i][j];
                                if ((layout_info->flags[i][j] & FILL) &&
                                    !done[c])
                                        nfills++;
                        }
                }

                /* no more place to put/take away extra space; give up */
                if (nfills == 0)
                        break;


                /* We take care of small amounts of space in a special case */
                if ( (space < 0 ? -1 : 1)*space < nfills) {
                        set_dimensions_plan_b (sheet, layout_info, dimensions, row, space);
                        break;
                }


                /*  this is how much we should ideally adjust each cell */
                cellspace = space/nfills;

                /*  loop through again and do the best we can */
                for (j = 0; j < layout_info->ncols; j++)
                        if ((layout_info->flags[i][j] & FILL) && !done[j]) {
                                if (cellspace > 0) {
                                        if (layout_info->flags[i][j] & PIXELS_MIN) {
                                                int allowed = dimensions->pixel_widths[i][j]
                                                        - layout_info->pixels_min[i][j];
                                                adjustments[j] = MAX(0, MIN (allowed, cellspace));
                                                done[j] = (allowed < cellspace);
                                        }
                                        else adjustments[j] = cellspace;
                                        space -= adjustments[j];
                                }
                                else {
                                        if (layout_info->flags[i][j] & PIXELS_MAX) {
                                                int allowed = dimensions->pixel_widths[i][j]
                                                        - layout_info->pixels_max[i][j];
                                                adjustments[j] =  MIN(0, MAX (allowed, cellspace));
                                                done[j] = (allowed >cellspace);
                                        }
                                        else adjustments[j] = cellspace;
                                        space -= adjustments[j];
                                }
                        }

                /* adjust everything, including SAME_SIZE cells */
                for (j = 0; j < layout_info->ncols; j++) {
                        if (layout_info->flags[i][j] & SAME_SIZE) {
                                int c = layout_info->size_c[i][j];
                                dimensions->pixel_widths[i][j] -= adjustments[c];
                                space -= adjustments[c];
                        }
                        dimensions->pixel_widths[i][j] -= adjustments[j];
                }

                for (j = 0; j < layout_info->ncols; j++)
                        adjustments[j] = 0;
        }

        /* clean up */
        g_free (adjustments);
        g_free (done);
}

/*
 *  This is for rows below the first one.  Only the FIXED and ALIGNED flags
 *   are allowed
 */
static void
set_dimensions_pass_four(GnucashSheet *sheet, CellLayoutInfo *layout_info,
                         CellDimensions *dimensions, int row)
{
        int i = row, j;
        int c1, c2;
        int r;

        for (j = 0; j < layout_info->ncols; j++) {

                if (!(layout_info->flags[i][j] & (LEFT_ALIGNED | RIGHT_ALIGNED | PIXELS_FIXED))) {
                        layout_info->pixels_max[i][j] = 0;
                        layout_info->flags[i][j] |= PIXELS_FIXED;
                        dimensions->pixel_widths[i][j] = 0;
                }
                else {
                        r = layout_info->right_align_r[i][j];
                        c1 = layout_info->left_align_c[i][j];
                        c2 = layout_info->right_align_c[i][j];                

                        layout_info->pixels_width[i][j] = compute_row_width (dimensions, r, c1, c2);
                        layout_info->flags[i][j] |= PIXELS_FIXED;
                        dimensions->pixel_widths[i][j] = layout_info->pixels_width[i][j];
                }
        }
}


gint
gnucash_style_row_width(SheetBlockStyle *style, int row)
{
        CellLayoutInfo *layout_info;
        CellDimensions *dimensions;

        layout_info = style->layout_info;
        dimensions = style->dimensions;

        return compute_row_width(dimensions, row, 0, layout_info->ncols - 1);
}


static void
compute_cell_origins_x (CellDimensions *dimensions, int row)
{
        int x = 0;
        int j;

        for (j = 0; j < dimensions->ncols; j++) {
                dimensions->origin_x[row][j] = x;
                x += dimensions->pixel_widths[row][j];
        }
}

static void
compute_cell_origins_y (CellDimensions *dimensions)
{
        int y = 0;
        int i, j;

        for (i = 0; i < dimensions->nrows; i++) {
                for (j = 0; j < dimensions->ncols; j++)
                        dimensions->origin_y[i][j] = y;
                y += dimensions->pixel_heights[i][0];
        }
}

static void
style_recompute_layout_dimensions (GnucashSheet *sheet,
                                   CellLayoutInfo *layout_info,
                                   CellDimensions *dimensions, int width)
{
        int i;
        int ideal_width;

        dimensions->height = 0;
        dimensions->width = width;
        ideal_width = dimensions->width;

        /* First set the top rows */
        set_dimensions_pass_one (sheet, layout_info, dimensions, 0);
        set_dimensions_pass_two (sheet, layout_info, dimensions, 0);
        set_dimensions_pass_three (sheet, layout_info, dimensions, 0,
                                   ideal_width);

        /* style->dimensions->width is fixed now */
        dimensions->width = compute_row_width (dimensions, 0, 0,
                                               dimensions->ncols-1);
        compute_cell_origins_x (dimensions, 0);

        /*  For now, let's treat the other rows as ALIGNED_ cells only */
        for (i = 1; i < layout_info->nrows; i++) {
                set_dimensions_pass_one (sheet, layout_info, dimensions, i);
                set_dimensions_pass_four (sheet, layout_info, dimensions, i);
                compute_cell_origins_x (dimensions, i);                
        }

        compute_cell_origins_y (dimensions);
}


#if 0
static void
sheet_recompute_style_dimensions_internal (gpointer _key,
                                           gpointer _layout_info,
                                           gpointer _data)
{
        gchar *key = _key;
        CellLayoutInfo *layout_info = _layout_info;
        GnucashSheet *sheet = _data;

        style_recompute_layout_dimensions (sheet, layout_info,
                                           g_hash_table_lookup (sheet->dimensions_hash_table, key));
}


static void
gnucash_sheet_recompute_style_dimensions (GnucashSheet *sheet)
{
        g_return_if_fail (sheet != NULL);
        g_return_if_fail (GNUCASH_IS_SHEET (sheet));

        g_hash_table_foreach (sheet->layout_info_hash_table,
                              sheet_recompute_style_dimensions_internal, sheet);
}
#endif

void
gnucash_sheet_style_set_dimensions (GnucashSheet *sheet,
				    SheetBlockStyle *style, int width)
{
        g_return_if_fail (sheet != NULL);
        g_return_if_fail (GNUCASH_IS_SHEET (sheet));
        g_return_if_fail (style != NULL);

        style_recompute_layout_dimensions (sheet, style->layout_info,
                                           style->dimensions, width);
}

gint
gnucash_style_col_is_resizable (SheetBlockStyle *style, int col)
{
        if (col < 0 || col >= style->ncols)
                return FALSE;

        return (style->layout_info->user_flags[0][col] & RESIZABLE);
}
                                
/*
 * Set width of a specified cell and set the PIXEL_FIXED flag.
 *  If same_size is TRUE, also set the width of any
 *  SAME_SIZE cell which references this one.  Otherwise those
 *  cells are set to PIXELS_FIXED with their current width.
 */
void
gnucash_sheet_style_set_col_width (GnucashSheet *sheet, SheetBlockStyle *style,
                                   int col, int width, int same_size)
{
        int i, j;        
        g_return_if_fail (sheet != NULL);
        g_return_if_fail (GNUCASH_IS_SHEET(sheet));
        g_return_if_fail (style != NULL);
        g_return_if_fail (col >= 0);
        g_return_if_fail (col < style->ncols);

        if (width >= 0) {

                style->layout_info->pixels_width[0][col] = width;

                style->layout_info->flags[0][col] = PIXELS_FIXED |
                        (style->layout_info->flags[0][col] & FILL);

                /* adjust the overall width of this style */
                style->dimensions->width -=  style->dimensions->pixel_widths[0][col] - width;

                style->dimensions->pixel_widths[0][col] = width;

                for (i = 0; i < style->nrows; i++) {
                        for (j = 0; j < style->ncols; j++) {
                                if ((style->layout_info->flags[i][j] & SAME_SIZE)
                                    && (style->layout_info->size_r[i][j] == 0)
                                    && (style->layout_info->size_c[i][j] == col)) {
                                        if (same_size) {
                                                /* adjust the overall width of this style */
                                                style->dimensions->width -=  style->dimensions->pixel_widths[0][col] - width;                                                
                                                style->dimensions->pixel_widths[i][j] = width;
                                        }
                                        else
                                        {
                                                style->layout_info->flags[i][j] = PIXELS_FIXED;
                                                style->layout_info->pixels_width[i][j] =
                                                        style->dimensions->pixel_widths[i][j];
                                        }
                                        
                                }
                        }
                }

                style->dimensions->width = compute_row_width (style->dimensions, 0, 0, style->ncols - 1);
                compute_cell_origins_x (style->dimensions, 0);

                for (i = 1; i < style->nrows; i++)
                        set_dimensions_pass_four (sheet, style->layout_info, style->dimensions, i);
        }
}


void
gnucash_sheet_style_destroy (GnucashSheet *sheet, SheetBlockStyle *style)
{
        gint i, j;
        
        g_return_if_fail (style != NULL);

        for ( i = 0; i < style->nrows; i++) {
                g_free(style->widths[i]);
                g_free(style->alignments[i]);
                g_free(style->fonts[i]);
                g_free(style->active_bg_color[i]);
                g_free(style->inactive_bg_color[i]);
                for (j = 0; j < style->ncols; j++)
                        g_free (style->labels[i][j]);
                g_free (style->labels[i]);
                g_free (style->borders[i]);
        }

        g_free(style->widths);
        g_free(style->alignments);
        g_free(style->fonts);
        g_free(style->active_bg_color);
        g_free(style->inactive_bg_color);
        g_free(style->labels);
        g_free (style->borders);

        style->layout_info->refcount--;

        if (style->layout_info->refcount == 0) {
                g_hash_table_remove (sheet->layout_info_hash_table,
                                     style_get_key (style));
                style_layout_info_destroy (style->layout_info);
        }

        style->dimensions->refcount--;

        if (style->dimensions->refcount == 0) {
                g_hash_table_remove (sheet->dimensions_hash_table,
                                     style_get_key (style));
                style_dimensions_destroy (style->dimensions);
        }

        g_free (style);
}


/* Recompiles the style information from the cellblock, without
 * recomputing the layout info or the dimensions. WARNING: this
 * function assumes that the space for the style info has been
 * allocated already. */
void
gnucash_sheet_style_recompile(SheetBlockStyle *style, CellBlock *cellblock,
                              SplitRegister *sr, gint cursor_type)
{
        gint i, j, type;
        char *label;

        for (i = 0; i < style->nrows; i++) {
                for (j = 0; j < style->ncols; j++) {
                        type = cellblock->cell_types[i][j];

                        style->widths[i][j] = cellblock->widths[j];

                        style->fonts[i][j] = NULL;
                        style->header_font = gnucash_default_font;

                        gnucash_style_set_borders (style, reg_borders);

                        if (type > -1)
                                label = sr->header_label_cells[type]->value;
                        else if (cursor_type == GNUCASH_CURSOR_HEADER)
                                label = cellblock->cells[i][j]->value;
                        else
                                label = "";

                        g_free(style->labels[i][j]);
                        style->labels[i][j] = g_strdup(label);

                        style->active_bg_color[i][j] =
                                gnucash_color_argb_to_gdk
                                (cellblock->active_bg_color);

                        style->inactive_bg_color[i][j] =
                                gnucash_color_argb_to_gdk
                                (cellblock->passive_bg_color);

                        switch (cellblock->alignments[j]) {
                                case ALIGN_RIGHT:
                                        style->alignments[i][j] =
                                                GTK_JUSTIFY_RIGHT;
                                        break;
                                case ALIGN_CENTER:
                                        style->alignments[i][j] =
                                                GTK_JUSTIFY_CENTER;
                                        break;
                                default:
                                case ALIGN_FILL:
                                case ALIGN_LEFT:
                                        style->alignments[i][j] =
                                                GTK_JUSTIFY_LEFT;
                                        break;
                        }
                }
        }
}

void
gnucash_style_set_cell_borders (SheetBlockStyle *style,
                                      int row, int col, int border)
{
        g_return_if_fail (style != NULL);

        if (row >= 0 && row < style->nrows && col >= 0 && col < style->ncols) 
                style->borders[row][col] = border;
}

void
gnucash_style_set_register_borders (int reg_borders_new)
{
        reg_borders = reg_borders_new;
}


void
gnucash_style_set_borders (SheetBlockStyle *style, int border)
{
        int row, col;

        g_return_if_fail (style != NULL);

        for (row  = 0; row < style->nrows; row++) {
                for (col = 0; col < style->ncols; col++)
                        gnucash_style_set_cell_borders (style, row, col,
                                                        border);
                
                style->borders[row][0] |= STYLE_BORDER_LEFT;
                style->borders[row][style->ncols - 1] |= STYLE_BORDER_RIGHT;
        }
}

void
gnucash_sheet_set_borders (GnucashSheet *sheet, int border)
{
        int i;

        g_return_if_fail (GNUCASH_IS_SHEET (sheet));

        for (i = 0; i < GNUCASH_CURSOR_LAST; i++)
                gnucash_style_set_borders (sheet->cursor_style[i], border);
}


SheetBlockStyle *
gnucash_sheet_style_compile (GnucashSheet *sheet, CellBlock *cellblock,
			     gint cursor_type)
{
        SheetBlockStyle *style;
        SplitRegister *sr;
        gint i;

        g_return_val_if_fail (sheet != NULL, NULL);
        g_return_val_if_fail (GNUCASH_IS_SHEET (sheet), NULL);
        g_return_val_if_fail (cellblock != NULL, NULL);


        sr = (SplitRegister *)sheet->split_register;

        style  = g_new0(SheetBlockStyle, 1);

        style->reg_type = sr->type & REG_TYPE_MASK;
        style->cursor_type = cursor_type;

        style->nrows = cellblock->numRows;
        style->ncols = cellblock->numCols;

        style->widths = g_new0(gint *, style->nrows);
        style->alignments = g_new0 (GtkJustification *, style->nrows);
        style->fonts = g_new0 (GdkFont **, style->nrows);
        style->active_bg_color = g_new0 (GdkColor **, style->nrows);
        style->inactive_bg_color = g_new0 (GdkColor **, style->nrows);
        style->labels = g_new0 (char **, style->nrows);
        style->borders = g_new0 (int *, style->nrows);

        for ( i = 0; i < style->nrows; i++) {
                style->widths[i] = g_new0(gint, style->ncols);
                style->alignments[i] = g_new0 (GtkJustification, style->ncols);
                style->fonts[i] = g_new0 (GdkFont *, style->ncols);
                style->active_bg_color[i] = g_new0 (GdkColor *, style->ncols);
                style->inactive_bg_color[i] = g_new0(GdkColor *, style->ncols);
                style->labels[i] = g_new0 (char *, style->ncols);
                style->borders[i] = g_new0 (int, style->ncols);
        }

        gnucash_sheet_style_recompile(style, cellblock, sr, cursor_type);

        gnucash_style_layout_init (sheet, style);
        gnucash_style_dimensions_init (sheet, style);
        gnucash_sheet_style_set_dimensions (sheet, style, DEFAULT_STYLE_WIDTH);
        return style;
}


/* FIXME:  maybe we can precompute these for each style */
void
gnucash_sheet_style_get_cell_pixel_rel_coords (SheetBlockStyle *style,
                                               gint cell_row, gint cell_col,
                                               gint *x, gint *y,
                                               gint *w, gint *h)
{
        gint i;
        
        g_return_if_fail (style != NULL);
        g_return_if_fail (cell_row >= 0 && cell_row <= style->nrows);
        g_return_if_fail (cell_col >= 0 && cell_col <= style->ncols);
        
        *y = 0;
        for (i = 0; i < cell_row; i++)
                *y += style->dimensions->pixel_heights[i][0];

        *h = style->dimensions->pixel_heights [cell_row][0];

        *x = 0;
        for (i = 0; i < cell_col; i++)
                *x += style->dimensions->pixel_widths[cell_row][i];

        *w = style->dimensions->pixel_widths [cell_row][cell_col];
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
