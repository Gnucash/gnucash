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
 * This is the Gnome implementation.
 *
 * HISTORY:
 * Copyright (c) 1998 Linas Vepstas
 * Copyright (c) 1998 Rob Browning <rlb@cs.utexas.edu>
 * Copyright (c) 1999 Heath Martin <martinh@pegasus.cc.ucf.edu>
 * Copyright (c) 2000 Heath Martin <martinh@pegasus.cc.ucf.edu>
 * Copyright (c) 2000 Dave Peticolas <dave@krondo.com>
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <gnome.h>

#include "cellblock.h"
#include "table-allgui.h"
#include "splitreg.h"
#include "util.h"

#include "gnucash-sheet.h"
#include "gnucash-color.h"
#include "gnucash-style.h"


static void
table_destroy_cb(Table *table)
{
        if (table == NULL)
                return;

        if (table->ui_data == NULL)
                return;

        if (table->ui_data)
                gtk_widget_unref(GTK_WIDGET(table->ui_data));

        table->ui_data = NULL;
}

void
gnc_table_init_gui (gncUIWidget widget, void *data)
{
        SplitRegister *sr;
        GnucashSheet *sheet;
        GnucashRegister *greg;
        Table *table;

        g_return_if_fail (widget != NULL);
        g_return_if_fail (GNUCASH_IS_REGISTER (widget));
        g_return_if_fail (data != NULL);

        sr = (SplitRegister *) data;

        greg = GNUCASH_REGISTER(widget);
        sheet = GNUCASH_SHEET(greg->sheet);
        sheet->split_register = data;
        table = sheet->table;

        table->destroy = table_destroy_cb;
        table->ui_data = sheet;

        gtk_widget_ref (GTK_WIDGET(sheet));

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

        gnc_table_refresh_header (table);

        gnucash_sheet_table_load (sheet);
        gnucash_sheet_cursor_set_from_table (sheet, TRUE);
        gnucash_sheet_redraw_all (sheet);
}


void        
gnc_table_refresh_gui (Table * table)
{
        GnucashSheet *sheet;
        SheetBlockStyle *style;
        SplitRegister *sr;

        if (!table)
                return;
        if (!table->ui_data)
                return;

        g_return_if_fail (GNUCASH_IS_SHEET (table->ui_data));

        sheet = GNUCASH_SHEET(table->ui_data);
        sr = sheet->split_register;

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
gnc_table_refresh_cursor_gui (Table * table,
                              CellBlock *curs,
                              PhysicalLocation phys_loc,
                              gboolean do_scroll)
{
        GnucashSheet *sheet;
        PhysicalCell *pcell;
        VirtualCellLocation vcell_loc;

        if (!table || !table->ui_data || !curs)
                return;

        g_return_if_fail (GNUCASH_IS_SHEET (table->ui_data));

        /* if the current cursor is undefined, there is nothing to do. */
        if (gnc_table_physical_cell_out_of_bounds (table, phys_loc))
                return;

        sheet = GNUCASH_SHEET(table->ui_data);

        /* compute the physical bounds of the current cursor */
        pcell = gnc_table_get_physical_cell (table, phys_loc);

        vcell_loc = pcell->virt_loc.vcell_loc;

        gnucash_sheet_cursor_set_from_table (sheet, do_scroll);
        gnucash_sheet_block_set_from_table (sheet, vcell_loc);
        gnucash_sheet_redraw_block (sheet, vcell_loc);
}

/* ================== end of file ======================= */


/*
  Local Variables:
  c-basic-offset: 8
  End:
*/
