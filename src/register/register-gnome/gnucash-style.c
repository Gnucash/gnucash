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

/*
 *  configure the cursor styles
 */

#include "config.h"

#include <gnome.h>

#include "gnucash-color.h"
#include "gnucash-grid.h"
#include "gnucash-item-edit.h"
#include "gnucash-style.h"
#include "gnc-gconf-utils.h"
#include "gnc-engine.h"		// For debugging, e.g. ENTER(), LEAVE()

/** GLOBALS *********************************************************/
/* This static indicates the debugging module that this .o belongs to.  */
#define DEFAULT_STYLE_WIDTH 680


/** Static Globals *****************************************************/

/* This static indicates the debugging module that this .o belongs to. */
static QofLogModule log_module = GNC_MOD_REGISTER;


/** Implementation *****************************************************/

static gpointer
style_get_key (SheetBlockStyle *style)
{
        static gint key;

        key = style->cursor->num_rows;

        return &key;
}

static gpointer
style_create_key (SheetBlockStyle *style)
{
        static gint key;

        key = style->cursor->num_rows;

        return g_memdup(&key, sizeof(key));
}

static void
cell_dimensions_construct (gpointer _cd, gpointer user_data)
{
        CellDimensions *cd = _cd;

        cd->pixel_width = -1;
        cd->can_span_over = TRUE;
}


static BlockDimensions *
style_dimensions_new (SheetBlockStyle *style)
{
        BlockDimensions *dimensions;

        dimensions = g_new0 (BlockDimensions, 1);

        dimensions->nrows = style->nrows;
        dimensions->ncols = style->ncols;

        dimensions->cell_dimensions = g_table_new (sizeof (CellDimensions),
                                                   cell_dimensions_construct,
                                                   NULL, NULL);

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
                                     style_create_key (style), dimensions);
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


/* This sets the initial sizes of the cells, based on the sample_text */
static void
set_dimensions_pass_one (GnucashSheet *sheet, CellBlock *cursor,
                         BlockDimensions *dimensions)
{
        /* GdkFont *font = GNUCASH_GRID(sheet->grid)->normal_font; */
        CellDimensions *cd;
        int row, col;
        gint max_height = -1;
        PangoLayout *layout;

        /* g_return_if_fail (font != NULL); */

        for (row = 0; row < cursor->num_rows; row++)
        {
                for (col = 0; col < cursor->num_cols; col++)
                {
                        int width;
                        char *text;
                        BasicCell *cell;

                        cd = g_table_index (dimensions->cell_dimensions,
                                            row, col);

                        cell = gnc_cellblock_get_cell (cursor, row, col);
                        if (!cell)
                                continue;

                        text = cell->sample_text;
                        if (text)
                                cd->can_span_over = FALSE;

                        if (text)
                        {
                                layout = gtk_widget_create_pango_layout (GTK_WIDGET (sheet), text);
                                pango_layout_get_pixel_size (layout, &width, &cd->pixel_height);
                                g_object_unref (layout);
                                width += 2 * CELL_HPADDING;
                                cd->pixel_height += 2 * CELL_VPADDING;
                        }
                        else
                        {
                                width = 0;
                                cd->pixel_height = (2 * CELL_VPADDING);
                        }

                        max_height = MAX(max_height, cd->pixel_height);

                        if (cd->pixel_width > 0)
                                continue;

                        if (cell && cell->is_popup)
                                width += gnc_item_edit_get_toggle_offset
                                        (cd->pixel_height);

                        cd->pixel_width = MAX (cd->pixel_width, width);
                }

                cd = g_table_index (dimensions->cell_dimensions, row, 0);
                dimensions->height += max_height;
        }

        for (row = 0; row < cursor->num_rows; row++)
        {
                for (col = 0; col < cursor->num_cols; col++)
                {
                        cd = g_table_index (dimensions->cell_dimensions,
                                            row, col);
                        cd->pixel_height = max_height;
                }
        }
}


/* Now adjust things to make everything even. This code assumes that
 * all cursors have the same number of columns!!! */
static void
set_dimensions_pass_two (GnucashSheet *sheet, int default_width)
{
        SheetBlockStyle *style;
        BlockDimensions *dimensions;
        CellDimensions *cd;
        GTable *cd_table;
        CellBlock *cursor;
        GList *cursors;
        GList *node;

        int num_cols;
        int *widths;
        int width;
        int row, col;

        style = gnucash_sheet_get_style_from_cursor (sheet, CURSOR_HEADER);
        dimensions = style->dimensions;
        cd_table = dimensions->cell_dimensions;
        cursor = style->cursor;

        width = 0;
        num_cols = cursor->num_cols;
        widths = g_new (int, num_cols);

        /* find header widths */
        for (col = 0; col < num_cols; col++)
        {
                cd = g_table_index (cd_table, 0, col);

                widths[col] = cd->pixel_width;
                width += cd->pixel_width;
        }

        if (width < default_width)
                for (col = 0; col < num_cols; col++)
                {
                        BasicCell *cell;

                        cell = gnc_cellblock_get_cell (cursor, 0, col);

                        if (!cell || !cell->expandable)
                                continue;

                        cd = g_table_index (cd_table, 0, col);

                        cd->pixel_width += (default_width - width);
                        width += (default_width - width);
                        widths[col] = cd->pixel_width;

                        break;
                }
        else if (width > default_width && width == sheet->window_width)
        {
                /*GdkFont *font = GNUCASH_GRID(sheet->grid)->normal_font;*/

                for (col = 0; col < num_cols; col++)
                {
                        BasicCell *cell;
                        const char *text;
                        int sample_width;
                        int old_width;
			PangoLayout *layout;

                        cell = gnc_cellblock_get_cell (cursor, 0, col);

                        if (!cell || !cell->expandable)
                                continue;

                        cd = g_table_index (cd_table, 0, col);

                        old_width = cd->pixel_width;

                        cd->pixel_width += (default_width - width);

                        text = cell->sample_text;
                        if (text)
                        {
				layout = gtk_widget_create_pango_layout (GTK_WIDGET (sheet), text);
				pango_layout_get_pixel_size (layout, &sample_width, NULL);
				g_object_unref (layout);
                                /*sample_width = gdk_string_width (font, text);*/
                                sample_width += 2 * CELL_HPADDING;
                        }
                        else
                                sample_width = 0;

                        cd->pixel_width = MAX (cd->pixel_width, sample_width);

                        width += cd->pixel_width - old_width;
                        widths[col] = cd->pixel_width;

                        break;
                }
        }

        cursors = gnc_table_layout_get_cursors (sheet->table->layout);

        /* adjust widths to be consistent */
        for (node = cursors; node; node = node->next)
        {
                cursor = node->data;
                style = gnucash_sheet_get_style_from_cursor
                        (sheet, cursor->cursor_name);
                dimensions = style->dimensions;
                cd_table = dimensions->cell_dimensions;

                for (row = 0; row < cursor->num_rows; row++)
                        for (col = 0; col < num_cols; col++)
                        {
                                cd = g_table_index (cd_table, row, col);

                                cd->pixel_width = widths[col];
                        }
        }

        /* now expand spanning cells */
        for (node = cursors; node; node = node->next)
        {
                CellDimensions *cd_span;

                cursor = node->data;
                style = gnucash_sheet_get_style_from_cursor
                        (sheet, cursor->cursor_name);
                dimensions = style->dimensions;
                cd_table = dimensions->cell_dimensions;

                for (row = 0; row < cursor->num_rows; row++)
                {
                        cd_span = NULL;

                        for (col = 0; col < num_cols; col++)
                        {
                                BasicCell *cell;

                                cell = gnc_cellblock_get_cell (cursor,
                                                                  row, col);
                                if (!cell)
                                        continue;

                                cd = g_table_index (cd_table, row, col);

                                if (cell->span)
                                {
                                        cd_span = cd;
                                        continue;
                                }

                                if (!cd->can_span_over)
                                        continue;

                                if (cd_span == NULL)
                                        continue;

                                if (cell->sample_text != NULL)
                                {
                                        cd_span = NULL;
                                        continue;
                                }

                                if (cd->pixel_width <= 0)
                                        continue;

                                cd_span->pixel_width += cd->pixel_width;
                                cd->pixel_width = 0;
                        }
                }
        }

        g_free (widths);
}

gint
gnucash_style_row_width(SheetBlockStyle *style, int row)
{
        BlockDimensions *dimensions;

        dimensions = style->dimensions;

        return compute_row_width(dimensions, row, 0, dimensions->ncols - 1);
}

static void
compute_cell_origins_x (BlockDimensions *dimensions)
{
        int x;
        int i, j;

        for (i = 0; i < dimensions->nrows; i++) {
                x = 0;

                for (j = 0; j < dimensions->ncols; j++) {
                        CellDimensions *cd;

                        cd = g_table_index (dimensions->cell_dimensions, i, j);

                        cd->origin_x = x;
                        x += cd->pixel_width;
                }
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

/* Calculate the widths and offsets */
static void
set_dimensions_pass_three (GnucashSheet *sheet)
{
        GList *cursors;
        GList *node;

        cursors = gnc_table_layout_get_cursors (sheet->table->layout);

        for (node = cursors; node; node = node->next)
        {
                CellBlock *cursor = node->data;

                SheetBlockStyle *style;
                BlockDimensions *dimensions;

                style = gnucash_sheet_get_style_from_cursor
                        (sheet, cursor->cursor_name);
                dimensions = style->dimensions;

                dimensions->width = compute_row_width (dimensions, 0, 0,
                                                       dimensions->ncols-1);

                compute_cell_origins_x (dimensions);
                compute_cell_origins_y (dimensions);
        }
}

static void
styles_recompute_layout_dimensions (GnucashSheet *sheet, int default_width)
{
        CellBlock *cursor;
        SheetBlockStyle *style;
        BlockDimensions *dimensions;
        GList *cursors;
        GList *node;

        cursors = gnc_table_layout_get_cursors (sheet->table->layout);

        for (node = cursors; node; node = node->next)
        {
                cursor = node->data;

                style = gnucash_sheet_get_style_from_cursor
                        (sheet, cursor->cursor_name);

                dimensions = style->dimensions;

                dimensions->height = 0;
                dimensions->width = default_width;

                set_dimensions_pass_one (sheet, cursor, dimensions);
        }

        set_dimensions_pass_two (sheet, default_width);
        set_dimensions_pass_three (sheet);
}

void
gnucash_sheet_styles_set_dimensions (GnucashSheet *sheet, int default_width)
{
        g_return_if_fail (sheet != NULL);
        g_return_if_fail (GNUCASH_IS_SHEET (sheet));

        styles_recompute_layout_dimensions (sheet, default_width);
}

gint
gnucash_style_col_is_resizable (SheetBlockStyle *style, int col)
{
        if (col < 0 || col >= style->ncols)
                return FALSE;

        return TRUE;
}

void
gnucash_sheet_set_col_width (GnucashSheet *sheet, int col, int width)
{
        CellDimensions *cd;
        SheetBlockStyle *style;
        int total;
        int diff;

        g_return_if_fail (sheet != NULL);
        g_return_if_fail (GNUCASH_IS_SHEET(sheet));
        g_return_if_fail (col >= 0);

        if (width < 0)
                return;

        style = gnucash_sheet_get_style_from_cursor (sheet, CURSOR_HEADER);

        g_return_if_fail (col < style->ncols);

        cd = gnucash_style_get_cell_dimensions (style, 0, col);

        /* adjust the overall width of this style */
        diff = cd->pixel_width - width;
        cd->pixel_width = width;

        total = MAX (sheet->window_width, sheet->width - diff);

        set_dimensions_pass_two (sheet, total);
        set_dimensions_pass_three (sheet);
}


void
gnucash_sheet_styles_recompile(GnucashSheet *sheet)
{
}


void
gnucash_sheet_get_borders (GnucashSheet *sheet, VirtualLocation virt_loc,
                           PhysicalCellBorders *borders)
{
        SheetBlockStyle *style;
        PhysicalCellBorderLineStyle line_style;

        g_return_if_fail (sheet != NULL);
        g_return_if_fail (GNUCASH_IS_SHEET (sheet));

        line_style = sheet->use_horizontal_lines ?
                CELL_BORDER_LINE_NORMAL : CELL_BORDER_LINE_NONE;

        borders->top    = line_style;
        borders->bottom = line_style;

        line_style = sheet->use_vertical_lines ?
                CELL_BORDER_LINE_NORMAL : CELL_BORDER_LINE_NONE;

        borders->left  = line_style;
        borders->right = line_style;

        if (virt_loc.phys_col_offset == 0)
                borders->left = CELL_BORDER_LINE_NORMAL;

        style = gnucash_sheet_get_style_from_cursor (sheet, CURSOR_HEADER);
        if (style)
                if (virt_loc.phys_col_offset == (style->ncols - 1))
                        borders->right = CELL_BORDER_LINE_NORMAL;

        if (virt_cell_loc_equal (virt_loc.vcell_loc,
                                 sheet->table->current_cursor_loc.vcell_loc))
        {
                borders->top    = CELL_BORDER_LINE_NORMAL;
                borders->bottom = CELL_BORDER_LINE_NORMAL;
        }

        gnc_table_get_borders (sheet->table, virt_loc, borders);
}


static SheetBlockStyle *
gnucash_sheet_style_new (GnucashSheet *sheet, CellBlock *cursor)
{
        SheetBlockStyle *style;

        g_return_val_if_fail (sheet != NULL, NULL);
        g_return_val_if_fail (GNUCASH_IS_SHEET (sheet), NULL);
        g_return_val_if_fail (cursor != NULL, NULL);

        style = g_new0 (SheetBlockStyle, 1);

        style->cursor = cursor;

        style->nrows = cursor->num_rows;
        style->ncols = cursor->num_cols;

        gnucash_style_dimensions_init (sheet, style);

        return style;
}

static void
destroy_style_helper (gpointer key, gpointer value, gpointer user_data)
{
        char *cursor_name = key;
        SheetBlockStyle *style = value;
        GnucashSheet *sheet = user_data;

        gnucash_sheet_style_destroy (sheet, style);
        g_free (cursor_name);
}

void
gnucash_sheet_clear_styles (GnucashSheet *sheet)
{
        g_return_if_fail (sheet != NULL);
        g_return_if_fail (GNUCASH_IS_SHEET (sheet));

        g_hash_table_foreach (sheet->cursor_styles,
                              destroy_style_helper, sheet);
}

void
gnucash_sheet_create_styles (GnucashSheet *sheet)
{
        GList *cursors;
        GList *node;

        g_return_if_fail (sheet != NULL);
        g_return_if_fail (GNUCASH_IS_SHEET (sheet));

        gnucash_sheet_clear_styles (sheet);

        cursors = gnc_table_layout_get_cursors (sheet->table->layout);

        for (node = cursors; node; node = node->next)
        {
                CellBlock *cursor = node->data;

                g_hash_table_insert (sheet->cursor_styles,
                                     g_strdup (cursor->cursor_name),
                                     gnucash_sheet_style_new (sheet, cursor));
        }
}

void
gnucash_sheet_compile_styles (GnucashSheet *sheet)
{
        g_return_if_fail (sheet != NULL);
        g_return_if_fail (GNUCASH_IS_SHEET (sheet));

	ENTER("sheet=%p", sheet);

        gnucash_sheet_styles_set_dimensions (sheet, DEFAULT_STYLE_WIDTH);

	LEAVE(" ");
}

void
gnucash_sheet_style_destroy (GnucashSheet *sheet, SheetBlockStyle *style)
{
        if (sheet == NULL)
                return;
        if (style == NULL)
                return;

        style->dimensions->refcount--;

        if (style->dimensions->refcount == 0) {
                g_hash_table_remove (sheet->dimensions_hash_table,
                                     style_get_key (style));
                style_dimensions_destroy (style->dimensions);
        }

        g_free (style);
}


void
gnucash_sheet_style_get_cell_pixel_rel_coords (SheetBlockStyle *style,
                                               gint cell_row, gint cell_col,
                                               gint *x, gint *y,
                                               gint *w, gint *h)
{
        CellDimensions *cd;

        g_return_if_fail (style != NULL);
        g_return_if_fail (cell_row >= 0 && cell_row <= style->nrows);
        g_return_if_fail (cell_col >= 0 && cell_col <= style->ncols);

        cd = gnucash_style_get_cell_dimensions (style, cell_row, cell_col);

        *x = cd->origin_x;
        *y = cd->origin_y;
        *h = cd->pixel_height;
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
        VirtualCell *vcell;
        CellBlock *cursor;
        SheetBlockStyle *style;

        g_return_val_if_fail (sheet != NULL, NULL);
        g_return_val_if_fail (GNUCASH_IS_SHEET(sheet), NULL);

        table = sheet->table;

        vcell = gnc_table_get_virtual_cell (table, vcell_loc);

        cursor = vcell->cellblock;

        style = gnucash_sheet_get_style_from_cursor (sheet,
                                                     cursor->cursor_name);
        if (style)
                return style;

        return gnucash_sheet_get_style_from_cursor (sheet, CURSOR_HEADER);
}

SheetBlockStyle *
gnucash_sheet_get_style_from_cursor (GnucashSheet *sheet,
                                     const char *cursor_name)
{
        g_return_val_if_fail (sheet != NULL, NULL);
        g_return_val_if_fail (GNUCASH_IS_SHEET (sheet), NULL);

        if (!cursor_name)
                return NULL;

        return g_hash_table_lookup (sheet->cursor_styles, cursor_name);
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

typedef struct
{
        char *cell_name;
        int width;
} WidthNode;

GNCHeaderWidths
gnc_header_widths_new (void)
{
        return g_hash_table_new (g_str_hash, g_str_equal);
}

static void
header_width_destroy_helper (gpointer key, gpointer value, gpointer user_data)
{
        WidthNode *wn = value;

        g_free (wn->cell_name);
        wn->cell_name = NULL;

        g_free (wn);
}

void
gnc_header_widths_destroy (GNCHeaderWidths widths)
{
        if (!widths) return;
        g_hash_table_foreach (widths, header_width_destroy_helper, NULL);
        g_hash_table_destroy (widths);
}

void
gnc_header_widths_set_width (GNCHeaderWidths widths,
                             const char *cell_name,
                             int width)
{
        WidthNode *wn;

        g_return_if_fail (widths != NULL);
        g_return_if_fail (cell_name != NULL);

        wn = g_hash_table_lookup (widths, cell_name);
        if (!wn)
        {
                wn = g_new0 (WidthNode, 1);

                wn->cell_name = g_strdup (cell_name);

                g_hash_table_insert (widths, wn->cell_name, wn);
        }

        wn->width = width;
}

int
gnc_header_widths_get_width (GNCHeaderWidths widths,
                             const char *cell_name)
{
        WidthNode *wn;

        g_return_val_if_fail (widths != NULL, 0);

        wn = g_hash_table_lookup (widths, cell_name);
        if (!wn)
                return 0;

        return wn->width;
}

void
gnucash_sheet_get_header_widths (GnucashSheet *sheet,
                                 GNCHeaderWidths widths)
{
        SheetBlockStyle *style;
        CellBlock *header;
        int row, col;

        g_return_if_fail (sheet != NULL);
        g_return_if_fail (GNUCASH_IS_SHEET(sheet));

        style = gnucash_sheet_get_style_from_cursor (sheet, CURSOR_HEADER);
        g_return_if_fail (style != NULL);

        header = style->cursor;
        g_return_if_fail (header != NULL);

        for (row = 0; row < style->nrows; row++)
                for (col = 0; col < style->ncols; col++)
                {
                        CellDimensions *cd;
                        BasicCell *cell;

                        cd = gnucash_style_get_cell_dimensions (style,
                                                                row, col);
                        if (cd == NULL)
                                continue;

                        cell = gnc_cellblock_get_cell (header, row, col);
                        if (!cell || !cell->cell_name)
                                continue;

                        gnc_header_widths_set_width (widths,
                                                     cell->cell_name,
                                                     cd->pixel_width);
                }
}

void
gnucash_sheet_set_header_widths (GnucashSheet *sheet,
                                 GNCHeaderWidths widths)
{
        SheetBlockStyle *style;
        CellBlock *header;
        int row, col;

        g_return_if_fail (sheet != NULL);
        g_return_if_fail (GNUCASH_IS_SHEET(sheet));

        style = gnucash_sheet_get_style_from_cursor (sheet, CURSOR_HEADER);
        g_return_if_fail (style != NULL);

        header = style->cursor;
        g_return_if_fail (header != NULL);

        for (row = 0; row < style->nrows; row++)
                for (col = 0; col < style->ncols; col++)
                {
                        CellDimensions *cd;
                        BasicCell *cell;

                        cd = gnucash_style_get_cell_dimensions (style,
                                                                row, col);

                        cell = gnc_cellblock_get_cell (header, row, col);
                        if (!cell || !cell->cell_name)
                                continue;

                        cd->pixel_width = gnc_header_widths_get_width
                                (widths, cell->cell_name);
                }
}

gboolean
gnucash_style_init (void)
{
	return TRUE;
}


/*
  Local Variables:
  c-basic-offset: 8
  End:
*/
