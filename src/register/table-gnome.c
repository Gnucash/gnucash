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
 * FILE:
 * table-gnome.c
 *
 * FUNCTION:
 * Implements the infrastructure for the displayed table.
 * This is the Gtk implementation;
 *
 * HISTORY:
 * Copyright (c) 1998 Linas Vepstas
 * Copyright (c) 1998 Rob Browning <rlb@cs.utexas.edu>
 * Copyright (c) 1999 Heath Martin <martinh@pegasus.cc.ucf.edu>
 * Copyright (c) 2000 Heath Martin <martinh@pegasus.cc.ucf.edu>
 */

/*
  TODO: fix up alignments in a UI independent manner.

  deal with the fact (if necessary) that the gtk UI can't directly
  "cancel" a traverse.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <gnome.h>

#include "cellblock.h"
#include "table-allgui.h"
#include "table-gnome.h"
#include "util.h"
#include "splitreg.h"

#include "gnome/gnucash-sheet.h"
#include "gnome/gnucash-color.h"
#include "gnome/gnucash-style.h"


void
xaccCreateTable (GtkWidget *widget, void *data)
{
        SplitRegister *sr;
        GnucashSheet *sheet;
        GnucashRegister *greg;
        Table *table;

        g_return_if_fail (widget != NULL);
        g_return_if_fail (GNUCASH_IS_REGISTER (widget));
        g_return_if_fail (data != NULL);

        sr = (SplitRegister *)data;

        greg = GNUCASH_REGISTER(widget);
        sheet = GNUCASH_SHEET(greg->sheet);
        sheet->split_register = data;
        table = sheet->table;

        table->table_widget = GTK_WIDGET(sheet);
        gtk_widget_ref (table->table_widget);

        /* config the cell-block styles */

        sheet->cursor_style[GNUCASH_CURSOR_HEADER] =
		gnucash_sheet_style_compile (sheet,
					     sr->header,
					     GNUCASH_CURSOR_HEADER);

        sheet->cursor_style[GNUCASH_CURSOR_SINGLE] =
		gnucash_sheet_style_compile (sheet,
					     sr->single_cursor,
					     GNUCASH_CURSOR_SINGLE);

        sheet->cursor_style[GNUCASH_CURSOR_DOUBLE] =
		gnucash_sheet_style_compile (sheet,
					     sr->double_cursor,
					     GNUCASH_CURSOR_DOUBLE);

        sheet->cursor_style[GNUCASH_CURSOR_TRANS] =
		gnucash_sheet_style_compile (sheet,
					     sr->trans_cursor,
					     GNUCASH_CURSOR_TRANS);

        sheet->cursor_style[GNUCASH_CURSOR_SPLIT] =
		gnucash_sheet_style_compile (sheet,
					     sr->split_cursor,
					     GNUCASH_CURSOR_SPLIT);

        xaccRefreshHeader (table);

        gnucash_sheet_table_load (sheet);
        gnucash_sheet_cursor_set_from_table (sheet, TRUE);
        gnucash_sheet_redraw_all (sheet);
}


void        
xaccRefreshTableGUI (Table * table)
{
        GnucashSheet *sheet;
        SheetBlockStyle *style;
        SplitRegister *sr;

        if (!table)
                return;
        if (!table->table_widget)
                return;

        g_return_if_fail (GNUCASH_IS_SHEET (table->table_widget));

        sheet = GNUCASH_SHEET(table->table_widget);
        sr = (SplitRegister *)sheet->split_register;

        style = sheet->cursor_style[GNUCASH_CURSOR_HEADER];
        gnucash_sheet_style_recompile (style, sr->header, sr,
                                       GNUCASH_CURSOR_HEADER);

        style = sheet->cursor_style[GNUCASH_CURSOR_SINGLE];
        gnucash_sheet_style_recompile (style, sr->single_cursor,
                                       sr, GNUCASH_CURSOR_SINGLE);

        style = sheet->cursor_style[GNUCASH_CURSOR_DOUBLE];
        gnucash_sheet_style_recompile (style, sr->double_cursor,
                                       sr, GNUCASH_CURSOR_DOUBLE);

        style = sheet->cursor_style[GNUCASH_CURSOR_TRANS];
        gnucash_sheet_style_recompile (style, sr->trans_cursor,
                                       sr, GNUCASH_CURSOR_TRANS);

        style = sheet->cursor_style[GNUCASH_CURSOR_SPLIT];
        gnucash_sheet_style_recompile (style, sr->split_cursor,
                                       sr, GNUCASH_CURSOR_SPLIT);

        gnucash_sheet_table_load (sheet);

        gnucash_sheet_redraw_all (sheet);
}


void        
doRefreshCursorGUI (Table * table, CellBlock *curs,
                    int from_row, int from_col, gncBoolean do_scroll)
{
        GnucashSheet *sheet;
        gint virt_row, virt_col;

        if (!table)
                return;
        if (!table->table_widget)
                return;

        g_return_if_fail (GNUCASH_IS_SHEET (table->table_widget));

        /* if the current cursor is undefined, there is nothing to do. */
        if (!curs) return;
        if ((0 > from_row) || (0 > from_col)) return;
        if ((from_row >= table->num_phys_rows) ||
            (from_col >= table->num_phys_cols))
                return;

        sheet = GNUCASH_SHEET(table->table_widget);

        /* compute the physical bounds of the current cursor */
        virt_row = table->locators[from_row][from_col]->virt_row;
        virt_col = table->locators[from_row][from_col]->virt_col;

        gnucash_sheet_cursor_set_from_table (sheet, do_scroll);
        gnucash_sheet_block_set_from_table (sheet, virt_row, virt_col);
        gnucash_sheet_redraw_block (sheet, virt_row, virt_col);
}

/* FIXME:  this won't really do what is expected, since
 * our sheet doesn't necessarily have constant width columns.
 * 
 */
int
gnc_table_column_width(Table *table, const int col)
{
        return 0;
}


/* ================== end of file ======================= */


/*
  Local Variables:
  c-basic-offset: 8
  End:
*/
