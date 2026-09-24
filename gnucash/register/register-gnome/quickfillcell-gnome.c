/********************************************************************\
 * quickfillcell-gnome.c --  implement gnome part of quickfill cell *
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

/* quickfillcell-gnome.c
 *
 * Implements gnome dependent quickfill cell functions.
 *
 * Copyright (C) 2000 Dave Peticolas <dave@krondo.com>
 */

#include <config.h>

#include <string.h>
#include <gdk/gdk.h>

#include "quickfillcell.h"
#include "quickfillcell-gnome.h"
#include "gnucash-sheet.h"
#include "gnucash-sheetP.h"
#include "table-allgui.h"


static gboolean
gnc_quickfill_cell_direct_update (BasicCell *bcell,
                                  int *cursor_position,
                                  int *start_selection,
                                  int *end_selection,
                                  const GncRegisterInput *input)
{
    QuickFillCell *cell = (QuickFillCell *) bcell;
    const char *match_str;
    QuickFill *match;
    int prefix_len;

    if (!input->pressed)
        return FALSE;

    switch (input->key)
    {
    case GNC_REGISTER_KEY_ESCAPE:
        if (bcell->changed)
        {
            GnucashSheet *sheet = (GnucashSheet *) bcell->gui_private;
            const char *value = gnc_table_get_model_entry (sheet->table, bcell->cell_name);

            gnc_basic_cell_set_value_internal (bcell, value);
            bcell->changed = FALSE;
            *cursor_position = 0;
            *start_selection = 0;
            *end_selection = -1;
            return TRUE;
        }
        return FALSE;
    case GNC_REGISTER_KEY_SLASH:
        if (!(input->modifiers & GNC_REGISTER_MODIFIER_ALT))
            return FALSE;
        break;
    case GNC_REGISTER_KEY_TAB:
    case GNC_REGISTER_KEY_LEFT_TAB:
        if (!(input->modifiers & GNC_REGISTER_MODIFIER_CONTROL))
            return FALSE;
        break;
    default:
        return FALSE;
    }

    if ((*start_selection <= *cursor_position) &&
            (*end_selection >= *cursor_position))
        *cursor_position = *start_selection;
    else if ((*end_selection <= *cursor_position) &&
             (*start_selection >= *cursor_position))
        *cursor_position = *end_selection;

    match = gnc_quickfill_get_string_len_match (cell->qf, bcell->value,
            *cursor_position);

    if (match == NULL)
        return TRUE;

    match = gnc_quickfill_get_unique_len_match (match, &prefix_len);
    if (match == NULL)
        return TRUE;

    match_str = gnc_quickfill_string (match);

    if ((match_str != NULL) &&
            (strncmp (match_str, bcell->value, strlen (bcell->value)) == 0) &&
            (strcmp (match_str, bcell->value) != 0))
        gnc_basic_cell_set_value (bcell, match_str);

    *cursor_position += prefix_len;
    *start_selection = *cursor_position;
    *end_selection = -1;

    return TRUE;
}

BasicCell *
gnc_quickfill_cell_gnome_new (void)
{
    BasicCell *cell;

    cell = gnc_quickfill_cell_new ();

    cell->direct_update = gnc_quickfill_cell_direct_update;

    return cell;
}

