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
#include "gnucash-grid.h"
#include "gnucash-color.h"
#include "messages.h"

#include "date.h"


#define DEFAULT_FONT "-adobe-helvetica-medium-r-normal--*-120-*-*-*-*-*-*"
#define HINT_FONT    "-adobe-helvetica-medium-o-normal--*-120-*-*-*-*-*-*"

#define DEFAULT_STYLE_WIDTH 680

GdkFont *gnucash_register_font = NULL;
GdkFont *gnucash_register_hint_font = NULL;

static char *register_font_name = NULL;
static char *register_hint_font_name = NULL;


/*  layout info flags */
#define CELL_FIXED   (1 << 0)   /* We use this to set the cell width */
#define EMPTY_CELL    (1 << 1)  /* Cell has no size */
#define RIGHT_ALIGNED (1 << 2)  /* Right align with a specified cell */
#define LEFT_ALIGNED  (1 << 3)  /* Left align with a specified cell */
#define FILL          (1 << 4)  /* Allow this cell to expand to fill */
#define CELL_MIN     (1 << 5)   /* Cell is no smaller than this,
                                   but won't expand further */
#define CELL_MAX     (1 << 6)   /* Cell is no larger that this,
                                   but won't shrink further */
#define STRING_FIXED  (1 << 7)  /* Fit this string */
#define STRING_MIN    (1 << 8)  /* Fit this string */
#define SAME_SIZE     (1 << 9)  /* Make the cell the same size
                                   as a specified cell */

/* User mutable flags */
#define RESIZABLE (1 << 0)  /* User can resize */
#define MOVABLE (1 << 1)   /* User can move */

struct   _CellLayoutInfo
{
        unsigned int **flags;
        unsigned int **user_flags;  /* For user mutable flags */

        int **chars_width;
        int **chars_min;
        int **chars_max;
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
        int chars_min;
        int chars_max;

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
        li->chars_min = g_new0(int *, style->nrows);
        li->chars_max = g_new0(int *, style->nrows);
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
                li->chars_min[i] = g_new0(int, style->ncols);
                li->chars_max[i] = g_new0(int, style->ncols);
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
                g_free(li->chars_min[i]);
                g_free(li->chars_max[i]);
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
        g_free(li->chars_min);
        g_free(li->chars_max);
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
        layout_info->chars_min[i][j] = ld[i][j].chars_min;\
        layout_info->chars_max[i][j] = ld[i][j].chars_max;\
        layout_info->right_align_r[i][j] = ld[i][j].right_align_r;\
        layout_info->right_align_c[i][j] = ld[i][j].right_align_c;\
        layout_info->left_align_r[i][j] = ld[i][j].left_align_r;\
        layout_info->left_align_c[i][j] = ld[i][j].left_align_c; \
        layout_info->size_r[i][j] = ld[i][j].size_r;  \
        layout_info->size_c[i][j] = ld[i][j].size_c;  \
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
        {{{STRING_FIXED,RESIZABLE,0,0,0,0,0,0,0,0,0,date_str},
          {CELL_MIN | CELL_MAX,RESIZABLE,0,3,5,0,0,0,0,0,0,NULL},
          {CELL_MIN | FILL,RESIZABLE,0,20,0,0,0,0,0,0,0,NULL},
          {STRING_MIN,RESIZABLE,0,0,10,0,0,0,0,0,0,XFRM_STR},
          {STRING_FIXED,0,1,0,0,0,0,0,0,0,0,RECONCILE_ABBREV},
          {CELL_MIN | CELL_MAX,RESIZABLE,0,9,10,0,0,0,0,0,0,NULL},
          {SAME_SIZE,RESIZABLE,0,0,0,0,0,0,0,0,5,NULL},
          {SAME_SIZE,RESIZABLE,0,0,0,0,0,0,0,0,5,NULL},
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
        {{{STRING_FIXED,RESIZABLE,0,0,0,0,0,0,0,0,0,date_str},
          {CELL_MIN | CELL_MAX,RESIZABLE,0,3,5,0,0,0,0,0,0,NULL},
          {CELL_MIN | FILL,RESIZABLE,0,20,0,0,0,0,0,0,0,NULL},
          {STRING_MIN,RESIZABLE,0,0,10,0,0,0,0,0,0,XFTO_STR},
          {STRING_MIN,RESIZABLE,0,0,10,0,0,0,0,0,0,XFRM_STR},
          {STRING_FIXED,0,1,0,0,0,0,0,0,0,0,RECONCILE_ABBREV},
          {CELL_MIN | CELL_MAX,RESIZABLE,0,9,10,0,0,0,0,0,0,NULL},
          {SAME_SIZE,RESIZABLE,0,0,0,0,0,0,0,0,6,NULL},
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
        {{{STRING_FIXED,RESIZABLE,0,0,0,0,0,0,0,0,0,date_str},
          {CELL_MIN | CELL_MAX,RESIZABLE,0,3,5,0,0,0,0,0,0,NULL},
          {CELL_MIN | FILL,RESIZABLE,0,20,0,0,0,0,0,0,0,NULL},
          {STRING_MIN,RESIZABLE,0,0,0,0,0,0,0,0,0,XFRM_STR},
          {STRING_FIXED,0,1,0,0,0,0,0,0,0,0,RECONCILE_ABBREV},
          {CELL_MIN | CELL_MAX,RESIZABLE,0,9,10,0,0,0,0,0,0,NULL},
          {SAME_SIZE,RESIZABLE,0,0,0,0,0,0,0,0,5,NULL},
          {SAME_SIZE,RESIZABLE,0,0,0,0,0,0,0,0,5,NULL}},
         {{LEFT_ALIGNED|RIGHT_ALIGNED,RESIZABLE,0,0,0,0,0,0,0,0,0,NULL},
          {LEFT_ALIGNED|RIGHT_ALIGNED,RESIZABLE,0,0,0,0,1,0,1,0,0,NULL},
          {LEFT_ALIGNED|RIGHT_ALIGNED,RESIZABLE,0,0,0,0,2,0,7,0,0,NULL},
          {EMPTY_CELL,0,0,0,0,0,0,0,0,0,0,NULL},
          {EMPTY_CELL,0,0,0,0,0,0,0,0,0,0,NULL},
          {EMPTY_CELL,0,0,0,0,0,0,0,0,0,0,NULL},
          {EMPTY_CELL,0,0,0,0,0,0,0,0,0,0,NULL},
          {EMPTY_CELL,0,0,0,0,0,0,0,0,0,0,NULL},
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
        {{{STRING_FIXED,RESIZABLE,0,0,0,0,0,0,0,0,0,date_str},
          {CELL_MIN | CELL_MAX,RESIZABLE,0,3,5,0,0,0,0,0,0,NULL},
          {CELL_MIN | FILL,RESIZABLE,0,20,0,0,0,0,0,0,0,NULL},
          {STRING_MIN,RESIZABLE,0,0,0,0,0,0,0,0,0,XFTO_STR},
          {STRING_MIN,RESIZABLE,0,0,0,0,0,0,0,0,0,XFRM_STR},
          {STRING_FIXED,0,1,0,0,0,0,0,0,0,0,RECONCILE_ABBREV},
          {CELL_MIN | CELL_MAX,RESIZABLE,0,9,10,0,0,0,0,0,0,NULL},
          {SAME_SIZE,RESIZABLE,0,0,0,0,0,0,0,0,6,NULL}},
         {{LEFT_ALIGNED|RIGHT_ALIGNED,RESIZABLE,0,0,0,0,0,0,0,0,0,NULL},
          {LEFT_ALIGNED|RIGHT_ALIGNED,RESIZABLE,0,0,0,0,1,0,1,0,0,NULL},
          {LEFT_ALIGNED|RIGHT_ALIGNED,RESIZABLE,0,0,0,0,2,0,7,0,0,NULL},
          {EMPTY_CELL,0,0,0,0,0,0,0,0,0,0,NULL},
          {EMPTY_CELL,0,0,0,0,0,0,0,0,0,0,NULL},
          {EMPTY_CELL,0,0,0,0,0,0,0,0,0,0,NULL},
          {EMPTY_CELL,0,0,0,0,0,0,0,0,0,0,NULL},
          {EMPTY_CELL,0,0,0,0,0,0,0,0,0,0,NULL},
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
        {{{STRING_FIXED,RESIZABLE,0,0,0,0,0,0,0,0,0,date_str},
          {CELL_MIN | CELL_MAX,RESIZABLE,0,3,5,0,0,0,0,0,0,NULL},
          {CELL_MIN | FILL,RESIZABLE,0,20,0,0,0,0,0,0,0,NULL},
          {STRING_MIN,RESIZABLE,0,0,0,0,0,0,0,0,0,XFRM_STR},
          {STRING_FIXED,0,1,0,0,0,0,0,0,0,0,RECONCILE_ABBREV},
          {CELL_MIN | CELL_MAX,RESIZABLE,0,9,10,0,0,0,0,0,0,NULL},
          {SAME_SIZE,RESIZABLE,0,0,0,0,0,0,0,0,5,NULL},
          {SAME_SIZE,RESIZABLE,0,0,0,0,0,0,0,0,5,NULL},
          {SAME_SIZE,RESIZABLE,0,0,0,0,0,0,0,0,5,NULL},
          {SAME_SIZE,RESIZABLE,0,0,0,0,0,0,0,0,5,NULL},
          {SAME_SIZE,RESIZABLE,0,0,0,0,0,0,0,0,5,NULL},
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
        {{{STRING_FIXED,RESIZABLE,0,0,0,0,0,0,0,0,0,date_str},
          {CELL_MIN | CELL_MAX,RESIZABLE,0,3,5,0,0,0,0,0,0,NULL},
          {CELL_MIN | FILL,RESIZABLE,0,20,0,0,0,0,0,0,0,NULL},
          {STRING_MIN,RESIZABLE,0,0,0,0,0,0,0,0,0,XFTO_STR},
          {STRING_MIN,RESIZABLE,0,0,0,0,0,0,0,0,0,XFRM_STR},
          {STRING_FIXED,0,1,0,0,0,0,0,0,0,0,RECONCILE_ABBREV},
          {CELL_MIN | CELL_MAX,RESIZABLE,0,9,10,0,0,0,0,0,0,NULL},
          {SAME_SIZE,RESIZABLE,0,0,0,0,0,0,0,0,6,NULL},
          {SAME_SIZE,RESIZABLE,0,0,0,0,0,0,0,0,6,NULL},
          {SAME_SIZE,RESIZABLE,0,0,0,0,0,0,0,0,6,NULL},
          {SAME_SIZE,RESIZABLE,0,0,0,0,0,0,0,0,6,NULL},
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
        {{{STRING_FIXED, RESIZABLE,0,0,0,0,0,0,0,0,0,date_str},
          {CELL_MIN | CELL_MAX,RESIZABLE,0,3,5,0,0,0,0,0,0,NULL},
          {CELL_MIN,RESIZABLE,0,20,0,0,0,0,0,0,0,NULL},
          {STRING_MIN | FILL,RESIZABLE,0,0,0,0,0,0,0,0,0,XFRM_STR},
          {STRING_FIXED,0,1,0,0,0,0,0,0,0,0,RECONCILE_ABBREV},
          {CELL_MIN | CELL_MAX,RESIZABLE,0,9,10,0,0,0,0,0,0,NULL},
          {SAME_SIZE,RESIZABLE,0,0,0,0,0,0,0,0,5,NULL},
          {SAME_SIZE,RESIZABLE,0,0,0,0,0,0,0,0,5,NULL},
          {SAME_SIZE,RESIZABLE,0,0,0,0,0,0,0,0,5,NULL},
          {SAME_SIZE,RESIZABLE,0,0,0,0,0,0,0,0,5,NULL},
          {SAME_SIZE,RESIZABLE,0,0,0,0,0,0,0,0,5,NULL}},
         {{LEFT_ALIGNED|RIGHT_ALIGNED,RESIZABLE,0,0,0,0,0,0,0,0,0,NULL},
          {LEFT_ALIGNED|RIGHT_ALIGNED,RESIZABLE,0,0,0,0,1,0,1,0,0,NULL},
          {LEFT_ALIGNED|RIGHT_ALIGNED,RESIZABLE,0,0,0,0,2,0,10,0,0,NULL},
          {EMPTY_CELL,0,0,0,0,0,0,0,0,0,0,NULL},
          {EMPTY_CELL,0,0,0,0,0,0,0,0,0,0,NULL},
          {EMPTY_CELL,0,0,0,0,0,0,0,0,0,0,NULL},
          {EMPTY_CELL,0,0,0,0,0,0,0,0,0,0,NULL},
          {EMPTY_CELL,0,0,0,0,0,0,0,0,0,0,NULL},
          {EMPTY_CELL,0,0,0,0,0,0,0,0,0,0,NULL},
          {EMPTY_CELL,0,0,0,0,0,0,0,0,0,0,NULL},
          {EMPTY_CELL,0,0,0,0,0,0,0,0,0,0,NULL},
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
        {{{STRING_FIXED, RESIZABLE,0,0,0,0,0,0,0,0,0,date_str},
          {CELL_MIN | CELL_MAX,RESIZABLE,0,3,5,0,0,0,0,0,0,NULL},
          {CELL_MIN,RESIZABLE,0,20,0,0,0,0,0,0,0,NULL},
          {STRING_MIN | FILL,RESIZABLE,0,0,0,0,0,0,0,0,0,XFTO_STR},
          {STRING_MIN | FILL,RESIZABLE,0,0,0,0,0,0,0,0,0,XFRM_STR},
          {STRING_FIXED,0,1,0,0,0,0,0,0,0,0,RECONCILE_ABBREV},
          {CELL_MIN | CELL_MAX,RESIZABLE,0,9,10,0,0,0,0,0,0,NULL},
          {SAME_SIZE,RESIZABLE,0,0,0,0,0,0,0,0,6,NULL},
          {SAME_SIZE,RESIZABLE,0,0,0,0,0,0,0,0,6,NULL},
          {SAME_SIZE,RESIZABLE,0,0,0,0,0,0,0,0,6,NULL},
          {SAME_SIZE,RESIZABLE,0,0,0,0,0,0,0,0,6,NULL}},
         {{LEFT_ALIGNED|RIGHT_ALIGNED,RESIZABLE,0,0,0,0,0,0,0,0,0,NULL},
          {LEFT_ALIGNED|RIGHT_ALIGNED,RESIZABLE,0,0,0,0,1,0,1,0,0,NULL},
          {LEFT_ALIGNED|RIGHT_ALIGNED,RESIZABLE,0,0,0,0,2,0,10,0,0,NULL},
          {EMPTY_CELL,0,0,0,0,0,0,0,0,0,0,NULL},
          {EMPTY_CELL,0,0,0,0,0,0,0,0,0,0,NULL},
          {EMPTY_CELL,0,0,0,0,0,0,0,0,0,0,NULL},
          {EMPTY_CELL,0,0,0,0,0,0,0,0,0,0,NULL},
          {EMPTY_CELL,0,0,0,0,0,0,0,0,0,0,NULL},
          {EMPTY_CELL,0,0,0,0,0,0,0,0,0,0,NULL},
          {EMPTY_CELL,0,0,0,0,0,0,0,0,0,0,NULL},
          {EMPTY_CELL,0,0,0,0,0,0,0,0,0,0,NULL},
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

static gpointer
cell_dimensions_new (void)
{
        CellDimensions *cd;

        cd = g_new0 (CellDimensions, 1);

        cd->pixel_width = -1;

        return cd;
}

static void
cell_dimensions_free (gpointer cd)
{
        g_free(cd);
}

static BlockDimensions *
style_dimensions_new (SheetBlockStyle *style)
{
        BlockDimensions *dimensions;

        dimensions = g_new0 (BlockDimensions, 1);

        dimensions->nrows = style->nrows;
        dimensions->ncols = style->ncols;

        dimensions->cell_dimensions = g_table_new (cell_dimensions_new,
                                                   cell_dimensions_free);

        g_table_resize (dimensions->cell_dimensions,
                        style->nrows, style->ncols);

        return dimensions;
}

static void
style_dimensions_destroy (BlockDimensions *dimensions)
{
        if (dimensions == NULL)
                return;

        g_table_destroy (dimensions->cell_dimensions);
        dimensions->cell_dimensions = NULL;

        g_free(dimensions);
}


static void
gnucash_style_dimensions_init (GnucashSheet *sheet, SheetBlockStyle *style)
{
        BlockDimensions *dimensions;

        dimensions = g_hash_table_lookup (sheet->dimensions_hash_table,
                                          style_get_key (style));

        if (!dimensions) {
                dimensions = style_dimensions_new (style);
                g_hash_table_insert (sheet->dimensions_hash_table,
                                     style_get_key (style), dimensions);
        }

        dimensions->refcount++;

        style->dimensions = dimensions;
}


CellDimensions *
gnucash_style_get_cell_dimensions (SheetBlockStyle *style, int row, int col)
{
        if (style == NULL)
                return NULL;
        if (style->dimensions == NULL)
                return NULL;
        if (style->dimensions->cell_dimensions == NULL)
                return NULL;

        return g_table_index (style->dimensions->cell_dimensions, row, col);
}

static int
compute_row_width (BlockDimensions *dimensions, int row, int col1, int col2)
{
        int j;
        int width = 0;

        col1 = MAX(0, col1);
        col2 = MIN(col2, dimensions->ncols-1);

        for (j = col1; j <= col2; j++) {
                CellDimensions *cd;
                cd = g_table_index (dimensions->cell_dimensions, row, j);
                width += cd->pixel_width;
        }

        return width;
}


/*
 *   This sets the initial sizes of the cells, based on cell_perc's and the
 *   width allocation of the sheet
 */
static void
set_dimensions_pass_one (GnucashSheet *sheet, CellLayoutInfo *layout_info,
                         BlockDimensions *dimensions, int row)
{
        GdkFont *font = GNUCASH_GRID(sheet->grid)->normal_font;
        CellDimensions *cd;
        int j;

        g_return_if_fail (font != NULL);

        for (j = 0; j < layout_info->ncols; j++) {
                cd = g_table_index (dimensions->cell_dimensions, row, j);
                if (cd->pixel_width < 0)
                        cd->pixel_width = (layout_info->cell_perc[row][j] *
                                           dimensions->width + 0.5);
                cd->pixel_height = (font->ascent + font->descent +
                                    2 * CELL_VPADDING);
        }

        cd = g_table_index (dimensions->cell_dimensions, row, 0);
        dimensions->height += cd->pixel_height;
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
                         BlockDimensions *dimensions, int row)
{
        GdkFont *font = GNUCASH_GRID(sheet->grid)->normal_font;
        CellDimensions *cd;
        unsigned int flags;
        int j;

        for (j = 0; j < layout_info->ncols; j++) {
                cd = g_table_index (dimensions->cell_dimensions, row, j);

                flags = layout_info->flags[row][j];

                if  (flags & CELL_FIXED)
                        cd->pixel_width = (layout_info->chars_width[row][j] *
                                           C_WIDTH + 2*CELL_HPADDING);
                else if (flags & EMPTY_CELL)
                        cd->pixel_width = 0;
                else if ((flags & STRING_FIXED) && layout_info->string[row][j])
                        cd->pixel_width =
                                gdk_string_measure (font,
                                                    layout_info->string[row][j]) + 2 * CELL_HPADDING;

                if (flags & CELL_MIN) {
                        int min = (layout_info->chars_min[row][j] *
                                   C_WIDTH+ 2*CELL_HPADDING);

                        cd->pixel_width = MAX (min, cd->pixel_width);
                }
                else if ((flags & STRING_MIN) &&  layout_info->string[row][j]) {
                        int min = gdk_string_measure (font, layout_info->string[row][j]) + 2*CELL_HPADDING;

                        cd->pixel_width = MAX (min, cd->pixel_width);
                }

                if (flags & CELL_MAX) {
                        int max = layout_info->chars_max[row][j] * C_WIDTH+ 2*CELL_HPADDING;

                        cd->pixel_width = MIN (max, cd->pixel_width);
                }
        }

        for (j = 0; j < layout_info->ncols; j++)
                if (layout_info->flags[row][j] & SAME_SIZE) {
                        CellDimensions *cd2;
                        int r = layout_info->size_r[row][j];
                        int c = layout_info->size_c[row][j];

                        cd = g_table_index (dimensions->cell_dimensions, row, j);
                        cd2 = g_table_index (dimensions->cell_dimensions, r, c);

                        cd->pixel_width = cd2->pixel_width;
                }
}


/* This is a subcase of pass_three: we have a very small amount of
 *  space to reallocate; let's just put it in the largest FILL cell,
 *  and damn the consequences; in the wierd case that there is no FILL
 *  cell at all, just stick it in the largest cell.
 */
static void
set_dimensions_plan_b (GnucashSheet *sheet, CellLayoutInfo *layout_info,
                       BlockDimensions *dimensions, int row, int space)
{
        CellDimensions *cd;
        int fill_col = -1;
        int fill_w = 0;
        int col = 0;
        int w = 0;
        int j;

        for (j = 0; j < layout_info->ncols; j++) {
                cd = g_table_index (dimensions->cell_dimensions, row, j);

                if (layout_info->flags[row][j] & FILL)
                        if (fill_w < cd->pixel_width) {
                                fill_col = j;
                                fill_w = cd->pixel_width;
                        }

                if (w < cd->pixel_width) {
                        col = j;
                        w = cd->pixel_width;
                }
        }

        if (fill_col > -1) {
                cd = g_table_index (dimensions->cell_dimensions, row, fill_col);
                cd->pixel_width -= space;
        }
        else {
                cd = g_table_index (dimensions->cell_dimensions, row, col);
                cd->pixel_width -= space;
        }
}


/* OK, this is the tricky one: we need to add/subtract left over or
 *  excess space from the FILL cells.  We'll try to adjust each one
 *  the same amount, but this is subject to any _MIN/_MAX
 *  restrictions, and we need to be careful with SAME_SIZE cells, too. */
static void
set_dimensions_pass_three (GnucashSheet *sheet, CellLayoutInfo *layout_info,
                           BlockDimensions *dimensions,
                           int row, int ideal_width)
{
        int space;
        int cellspace;
        int nfills;
        int j;

        int *adjustments;
        int *done;

        space = compute_row_width (dimensions, row, 0, dimensions->ncols-1)
                - ideal_width;

        adjustments = g_new0 (int, layout_info->ncols);
        done = g_new0 (int, layout_info->ncols);

        while (space != 0) {

                nfills = 0;
                /* count the number of fill cells we have left to work with */
                for (j = 0; j < layout_info->ncols; j++) {
                        if ( (layout_info->flags[row][j] & FILL) && !done[j])
                                nfills++;
                        else if (layout_info->flags[row][j] & SAME_SIZE) {
                                int c = layout_info->size_c[row][j];
                                if ((layout_info->flags[row][j] & FILL) &&
                                    !done[c])
                                        nfills++;
                        }
                }

                /* no more place to put/take away extra space; give up */
                if (nfills == 0)
                        break;

                /* We take care of small amounts of space in a special case */
                if ( (space < 0 ? -1 : 1)*space < nfills) {
                        set_dimensions_plan_b (sheet, layout_info,
                                               dimensions, row, space);
                        break;
                }

                /*  this is how much we should ideally adjust each cell */
                cellspace = space/nfills;

                /*  loop through again and do the best we can */
                for (j = 0; j < layout_info->ncols; j++)
                        if ((layout_info->flags[row][j] & FILL) && !done[j]) {
                                if (cellspace > 0) {
                                        if (layout_info->flags[row][j] & CELL_MIN) {
                                                int allowed = 0;

                                                adjustments[j] = MAX(0, MIN (allowed, cellspace));
                                                done[j] = (allowed < cellspace);
                                        }
                                        else adjustments[j] = cellspace;
                                        space -= adjustments[j];
                                }
                                else {
                                        if (layout_info->flags[row][j] & CELL_MAX) {
                                                int allowed = 0;
                                                adjustments[j] =  MIN(0, MAX (allowed, cellspace));
                                                done[j] = (allowed >cellspace);
                                        }
                                        else adjustments[j] = cellspace;
                                        space -= adjustments[j];
                                }
                        }

                /* adjust everything, including SAME_SIZE cells */
                for (j = 0; j < layout_info->ncols; j++) {
                        CellDimensions *cd;

                        cd = g_table_index (dimensions->cell_dimensions, row, j);
                        if (layout_info->flags[row][j] & SAME_SIZE) {
                                int c = layout_info->size_c[row][j];
                                cd->pixel_width -= adjustments[c];
                                space -= adjustments[c];
                        }

                        cd->pixel_width -= adjustments[j];
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
                         BlockDimensions *dimensions, int row)
{
        CellDimensions *cd;
        int c1, c2;
        int r;
        int j;

        for (j = 0; j < layout_info->ncols; j++) {
                cd = g_table_index (dimensions->cell_dimensions, row, j);
                if (!(layout_info->flags[row][j] &
                      (LEFT_ALIGNED | RIGHT_ALIGNED | CELL_FIXED)))
                        cd->pixel_width = 0;
                else {
                        r = layout_info->right_align_r[row][j];
                        c1 = layout_info->left_align_c[row][j];
                        c2 = layout_info->right_align_c[row][j];

                        cd->pixel_width = compute_row_width (dimensions, r,
                                                             c1, c2);
                }
        }
}


gint
gnucash_style_row_width(SheetBlockStyle *style, int row)
{
        CellLayoutInfo *layout_info;
        BlockDimensions *dimensions;

        layout_info = style->layout_info;
        dimensions = style->dimensions;

        return compute_row_width(dimensions, row, 0, layout_info->ncols - 1);
}


static void
compute_cell_origins_x (BlockDimensions *dimensions, int row)
{
        int x = 0;
        int j;

        for (j = 0; j < dimensions->ncols; j++) {
                CellDimensions *cd;

                cd = g_table_index (dimensions->cell_dimensions, row, j);

                cd->origin_x = x;
                x += cd->pixel_width;
        }
}

static void
compute_cell_origins_y (BlockDimensions *dimensions)
{
        CellDimensions *cd;
        int y = 0;
        int i, j;

        for (i = 0; i < dimensions->nrows; i++) {
                for (j = 0; j < dimensions->ncols; j++) {
                        cd = g_table_index (dimensions->cell_dimensions, i, j);
                        cd->origin_y = y;
                }
                cd = g_table_index (dimensions->cell_dimensions, i, 0);
                y += cd->pixel_height;
        }
}

static void
style_recompute_layout_dimensions (GnucashSheet *sheet,
                                   CellLayoutInfo *layout_info,
                                   BlockDimensions *dimensions, int width)
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
        CellDimensions *dimensions;
        int width;

        dimensions = g_hash_table_lookup (sheet->dimensions_hash_table, key);
        width = dimensions->width;

        style_recompute_layout_dimensions (sheet, layout_info,
                                           dimensions, width);
}


static void
gnucash_sheet_recompute_style_dimensions (GnucashSheet *sheet)
{
        g_return_if_fail (sheet != NULL);
        g_return_if_fail (GNUCASH_IS_SHEET (sheet));

        g_hash_table_foreach (sheet->layout_info_hash_table,
                              sheet_recompute_style_dimensions_internal,
                              sheet);
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
        CellDimensions *cd;
        int i, j;        

        g_return_if_fail (sheet != NULL);
        g_return_if_fail (GNUCASH_IS_SHEET(sheet));
        g_return_if_fail (style != NULL);
        g_return_if_fail (col >= 0);
        g_return_if_fail (col < style->ncols);

        if (width < 0)
                return;

        style->layout_info->flags[0][col] = (style->layout_info->flags[0][col] & FILL);

        cd = gnucash_style_get_cell_dimensions (style, 0, col);

        /* adjust the overall width of this style */
        style->dimensions->width -= cd->pixel_width - width;
        cd->pixel_width = width;

        for (i = 0; i < style->nrows; i++) {
                for (j = 0; j < style->ncols; j++) {
                        CellDimensions *cd2;

                        cd2 = gnucash_style_get_cell_dimensions (style, i, j);

                        if ((style->layout_info->flags[i][j] & SAME_SIZE)
                            && (style->layout_info->size_r[i][j] == 0)
                            && (style->layout_info->size_c[i][j] == col)) {
                                if (same_size) {
                                        /* adjust the width of this style */
                                        style->dimensions->width -= cd->pixel_width - width;                                                
                                        cd2->pixel_width = width;
                                }
                                else
                                        style->layout_info->flags[i][j] = CELL_FIXED;
                        }
                }
        }

        style->dimensions->width = compute_row_width (style->dimensions, 0, 0, style->ncols - 1);
        compute_cell_origins_x (style->dimensions, 0);

        for (i = 1; i < style->nrows; i++)
                set_dimensions_pass_four (sheet, style->layout_info,
                                          style->dimensions, i);
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
                        CellBlockCell *cb_cell;
                        CellStyle *cs;

                        cb_cell = gnc_cellblock_get_cell (cellblock, i, j);
                        cs = gnucash_style_get_cell_style (style, i, j);

                        type = cb_cell->cell_type;

                        style->header_font = gnucash_register_font;

                        gnucash_style_set_borders (style, reg_borders);

                        if (type > -1)
                                label = sr->header_label_cells[type]->value;
                        else if (cursor_type == GNUCASH_CURSOR_HEADER)
                                label = cb_cell->cell->value;
                        else
                                label = "";

                        g_free(cs->label);
                        cs->label = g_strdup(label);

                        cs->active_bg_color =
                                gnucash_color_argb_to_gdk
                                (cellblock->active_bg_color);

                        cs->inactive_bg_color =
                                gnucash_color_argb_to_gdk
                                (cellblock->passive_bg_color);

                        switch (cb_cell->alignment) {
                                case CELL_ALIGN_RIGHT:
                                        cs->alignment = GTK_JUSTIFY_RIGHT;
                                        break;
                                case CELL_ALIGN_CENTER:
                                        cs->alignment = GTK_JUSTIFY_CENTER;
                                        break;
                                default:
                                case CELL_ALIGN_FILL:
                                case CELL_ALIGN_LEFT:
                                        cs->alignment = GTK_JUSTIFY_LEFT;
                                        break;
                        }
                }
        }
}

void
gnucash_style_set_cell_borders (SheetBlockStyle *style,
                                int row, int col, int border)
{
        CellStyle *cs;

        if (style == NULL)
                return;

        cs = gnucash_style_get_cell_style (style, row, col);
        if (cs == NULL)
                return;

        cs->border = border;
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
                CellStyle *cs;

                for (col = 0; col < style->ncols; col++)
                        gnucash_style_set_cell_borders (style, row, col,
                                                        border);

                cs = gnucash_style_get_cell_style (style, row, 0);
                cs->border |= STYLE_BORDER_LEFT;

                cs = gnucash_style_get_cell_style (style, row,
                                                   style->ncols - 1);
                cs->border |= STYLE_BORDER_RIGHT;
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

CellStyle *
gnucash_style_get_cell_style (SheetBlockStyle *style, int row, int col)
{
        if (style == NULL)
                return NULL;

        return g_table_index (style->cell_styles, row, col);
}

static gpointer
cell_style_new (void)
{
        CellStyle *cs;

        cs = g_new0 (CellStyle, 1);

        return cs;
}

static void
cell_style_free (gpointer _cs)
{
        CellStyle *cs = _cs;

        g_free(cs->label);
        cs->label = NULL;

        g_free(cs);
}

SheetBlockStyle *
gnucash_sheet_style_compile (GnucashSheet *sheet, CellBlock *cellblock,
			     gint cursor_type)
{
        SheetBlockStyle *style;
        SplitRegister *sr;

        g_return_val_if_fail (sheet != NULL, NULL);
        g_return_val_if_fail (GNUCASH_IS_SHEET (sheet), NULL);
        g_return_val_if_fail (cellblock != NULL, NULL);

        sr = sheet->split_register;

        style  = g_new0(SheetBlockStyle, 1);

        style->reg_type = sr->type;
        style->cursor_type = cursor_type;

        style->nrows = cellblock->num_rows;
        style->ncols = cellblock->num_cols;

        style->cell_styles = g_table_new (cell_style_new, cell_style_free);
        g_table_resize (style->cell_styles, style->nrows, style->ncols);

        gnucash_sheet_style_recompile(style, cellblock, sr, cursor_type);

        gnucash_style_layout_init (sheet, style);
        gnucash_style_dimensions_init (sheet, style);
        gnucash_sheet_style_set_dimensions (sheet, style, DEFAULT_STYLE_WIDTH);

        return style;
}


void
gnucash_sheet_style_destroy (GnucashSheet *sheet, SheetBlockStyle *style)
{
        if (sheet == NULL)
                return;
        if (style == NULL)
                return;

        g_table_destroy (style->cell_styles);
        style->cell_styles = NULL;

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


/* FIXME:  maybe we can precompute these for each style */
void
gnucash_sheet_style_get_cell_pixel_rel_coords (SheetBlockStyle *style,
                                               gint cell_row, gint cell_col,
                                               gint *x, gint *y,
                                               gint *w, gint *h)
{
        CellDimensions *cd;
        gint i;

        g_return_if_fail (style != NULL);
        g_return_if_fail (cell_row >= 0 && cell_row <= style->nrows);
        g_return_if_fail (cell_col >= 0 && cell_col <= style->ncols);

        *y = 0;
        for (i = 0; i < cell_row; i++) {
                cd = gnucash_style_get_cell_dimensions (style, i, 0);
                *y += cd->pixel_height;
        }

        cd = gnucash_style_get_cell_dimensions (style, cell_row, 0);
        *h = cd->pixel_height;

        *x = 0;
        for (i = 0; i < cell_col; i++) {
                cd = gnucash_style_get_cell_dimensions (style, cell_row, i);
                *x += cd->pixel_width;
        }

        cd = gnucash_style_get_cell_dimensions (style, cell_row, cell_col);
        *w = cd->pixel_width;
}


SheetBlockStyle *
gnucash_sheet_get_style (GnucashSheet *sheet, VirtualCellLocation vcell_loc)
{
        SheetBlock *block;

        g_return_val_if_fail (sheet != NULL, NULL);
        g_return_val_if_fail (GNUCASH_IS_SHEET(sheet), NULL);

        block = gnucash_sheet_get_block (sheet, vcell_loc);

        if (block)
                return block->style;
        else
                return NULL;
}


SheetBlockStyle *
gnucash_sheet_get_style_from_table (GnucashSheet *sheet,
                                    VirtualCellLocation vcell_loc)
{
        Table *table;
        SplitRegister *sr;
        VirtualCell *vcell;
        CellBlock *cursor;

        g_return_val_if_fail (sheet != NULL, NULL);
        g_return_val_if_fail (GNUCASH_IS_SHEET(sheet), NULL);

        table = sheet->table;
        sr = sheet->split_register;

        vcell = gnc_table_get_virtual_cell (table, vcell_loc);

        cursor = vcell->cellblock;

        if (cursor == sr->single_cursor)
                return sheet->cursor_style[GNUCASH_CURSOR_SINGLE];
        else if (cursor == sr->double_cursor)
                return sheet->cursor_style[GNUCASH_CURSOR_DOUBLE];
        else if (cursor == sr->trans_cursor)
                return sheet->cursor_style[GNUCASH_CURSOR_TRANS];
        else if (cursor == sr->split_cursor)
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
gnucash_style_set_register_font_name(const char *name)
{
        g_free(register_font_name);
        register_font_name = g_strdup(name);

        if (gnucash_register_font != NULL)
        {
                gdk_font_unref(gnucash_register_font);
                gnucash_register_font = NULL;
        }

        if (register_font_name != NULL)
                gnucash_register_font = gdk_font_load(register_font_name);

        if (gnucash_register_font == NULL)
        {
                g_free(register_font_name);
                register_font_name = NULL;
                gnucash_register_font = gdk_font_load(DEFAULT_FONT);
        }

        g_assert(gnucash_register_font != NULL);

        gdk_font_ref(gnucash_register_font);
}

void
gnucash_style_set_register_hint_font_name(const char *name)
{
        g_free(register_hint_font_name);
        register_hint_font_name = g_strdup(name);

        if (gnucash_register_hint_font != NULL)
        {
                gdk_font_unref(gnucash_register_hint_font);
                gnucash_register_hint_font = NULL;
        }

        if (register_hint_font_name != NULL)
                gnucash_register_hint_font =
                        gdk_font_load(register_hint_font_name);

        if (gnucash_register_hint_font == NULL)
        {
                g_free(register_hint_font_name);
                register_hint_font_name = NULL;
                gnucash_register_hint_font = gdk_font_load(HINT_FONT);
        }

        g_assert(gnucash_register_hint_font != NULL);

        gdk_font_ref(gnucash_register_hint_font);
}

const char *
gnucash_style_get_default_register_font_name(void)
{
        return DEFAULT_FONT;
}

const char *
gnucash_style_get_default_register_hint_font_name(void)
{
        return HINT_FONT;
}

void
gnucash_style_init (void)
{
        if (gnucash_register_font == NULL)
                gnucash_style_set_register_font_name(NULL);

        if (gnucash_register_hint_font == NULL)
                gnucash_style_set_register_hint_font_name(NULL);
}


/*
  Local Variables:
  c-basic-offset: 8
  End:
*/
