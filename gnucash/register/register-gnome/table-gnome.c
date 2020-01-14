/********************************************************************\
 * table-gnome.c -- implementation of table object in GNOME         *
 *                                                                  *
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
 * Copyright (c) 2001 Gnumatic, Inc.
 */

#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <glib.h>

#include "gnucash-sheet.h"
#include "gnucash-sheetP.h"
#include "gnucash-style.h"
#include "table-allgui.h"
#include "table-gnome.h"
#include "gnc-prefs.h"
#include "gnc-engine.h"
#include "gnc-state.h"

#include "gnc-ledger-display.h"



/** Static Globals *****************************************************/

#define UNUSED_VAR     __attribute__ ((unused))

/* This static indicates the debugging module that this .o belongs to. */
static QofLogModule UNUSED_VAR log_module = GNC_MOD_REGISTER;


/** Implementation *****************************************************/

void
gnc_table_save_state (Table *table, gchar * state_section, gchar * account_fullname)
{
    GnucashSheet *sheet;
    GNCHeaderWidths widths;
    GList *node;
    gchar *key;
    GKeyFile *state_file = gnc_state_get_current();

    if (!table)
        return;

    if (table->ui_data == NULL)
        return;

    if (!gnc_prefs_get_bool(GNC_PREFS_GROUP_GENERAL, GNC_PREF_SAVE_GEOMETRY))
        return;
    sheet = GNUCASH_SHEET (table->ui_data);

    widths = gnc_header_widths_new ();

    gnucash_sheet_get_header_widths (sheet, widths);

    node = gnc_table_layout_get_cells (table->layout);
    for (; node; node = node->next)
    {
        BasicCell *cell = node->data;
        int width;

        width = gnc_header_widths_get_width (widths, cell->cell_name);

        /* Remember whether the column is visible */
        key = g_strdup_printf("%s_width", cell->cell_name);
        if ((width > 0) && (!cell->expandable))
        {
            g_key_file_set_integer (state_file, state_section, key, width);
        }
        else if (g_key_file_has_key (state_file, state_section, key, NULL))
            g_key_file_remove_key (state_file, state_section, key, NULL);
        g_free (key);
    }
    key = g_strdup_printf ("Register state for \"%s\"", account_fullname);
    g_key_file_set_comment (state_file, state_section, NULL, key, NULL);
    g_free (key);

    gnc_header_widths_destroy (widths);
}

static void
table_ui_redraw_cb (Table *table)
{
    GnucashSheet *sheet;

    if (table == NULL)
        return;

    if (table->ui_data == NULL)
        return;

    sheet = GNUCASH_SHEET (table->ui_data);

    gnucash_sheet_redraw_help (sheet);
}

static void
table_destroy_cb (Table *table)
{
    GnucashSheet *sheet;

    if (table == NULL)
        return;

    if (table->ui_data == NULL)
        return;

    sheet = GNUCASH_SHEET (table->ui_data);

    g_object_unref (sheet);

    table->ui_data = NULL;
}


/* Um, this function checks that data is not null but never uses it.
   Weird.  Also, since this function only works with a GnucashRegister
   widget, maybe some of it should be moved to gnucash-sheet.c. */
/* Adding to previous note:  Since data doesn't appear do anything and to
   align the function with save_state() I've removed the check for
   NULL and changed two calls in dialog_order.c and dialog_invoice.c
   to pass NULL as second parameter. */

void
gnc_table_init_gui (Table *table)
{
    table->gui_handlers.redraw_help = table_ui_redraw_cb;
    table->gui_handlers.destroy = table_destroy_cb;
}

void
gnc_table_refresh_gui (Table * table, gboolean do_scroll)
{
    GnucashSheet *sheet;

    if (!table)
        return;
    if (!table->ui_data)
        return;

    g_return_if_fail (GNUCASH_IS_SHEET (table->ui_data));

    sheet = GNUCASH_SHEET(table->ui_data);

    gnucash_sheet_styles_recompile (sheet);
    gnucash_sheet_table_load (sheet, do_scroll);
    gnucash_sheet_redraw_all (sheet);
}


static void
gnc_table_refresh_cursor_gnome (Table * table,
                                VirtualCellLocation vcell_loc,
                                gboolean do_scroll)
{
    GnucashSheet *sheet;

    if (!table || !table->ui_data)
        return;

    g_return_if_fail (GNUCASH_IS_SHEET (table->ui_data));

    if (gnc_table_virtual_cell_out_of_bounds (table, vcell_loc))
        return;

    sheet = GNUCASH_SHEET (table->ui_data);

    gnucash_sheet_cursor_set_from_table (sheet, do_scroll);

    if (gnucash_sheet_block_set_from_table (sheet, vcell_loc))
    {
        gnucash_sheet_recompute_block_offsets (sheet);
        gnucash_sheet_set_scroll_region (sheet);
        gnucash_sheet_compute_visible_range (sheet);
        gnucash_sheet_redraw_all (sheet);
    }
    else
        gnucash_sheet_redraw_block (sheet, vcell_loc);
}

void
gnc_table_show_range (Table *table,
                      VirtualCellLocation start_loc,
                      VirtualCellLocation end_loc)
{
    GnucashSheet *sheet;

    if (!table || !table->ui_data)
        return;

    g_return_if_fail (GNUCASH_IS_SHEET (table->ui_data));

    if (gnc_table_virtual_cell_out_of_bounds (table, start_loc))
        return;

    if (gnc_table_virtual_cell_out_of_bounds (table, end_loc))
        return;

    sheet = GNUCASH_SHEET (table->ui_data);

    gnucash_sheet_show_range (sheet, start_loc, end_loc);
}

void
gnc_table_gnome_init (void)
{
    TableGUIHandlers gui_handlers;

    gui_handlers.cursor_refresh = gnc_table_refresh_cursor_gnome;

    gnc_table_set_default_gui_handlers (&gui_handlers);
}

