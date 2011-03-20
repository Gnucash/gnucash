/********************************************************************\
 * checkboxcell.c -- yes/no checkbox cell                           *
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
 * checkboxcell.c
 *
 * FUNCTION:
 * Implements a mouse-click cell that toggles a yes/no value.
 *
 * HISTORY:
 * Copyright (c) 1998 Linas Vepstas
 * Copyright (c) 2000 Dave Peticolas
 * Copyright (c) 2001 Derek Atkins
 */

#include "config.h"

#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "basiccell.h"
#include "gnc-engine.h"
#include "checkboxcell.h"


/* assumes we are given the untranslated form */
static void
gnc_checkbox_cell_set_value (BasicCell *_cell, const char *value)
{
    CheckboxCell *cell = (CheckboxCell *) _cell;
    gboolean flag = FALSE;

    if (value && *value == 'X')
        flag = TRUE;

    gnc_checkbox_cell_set_flag (cell, flag);
}

static gboolean
gnc_checkbox_cell_enter (BasicCell *_cell,
                         int *cursor_position,
                         int *start_selection,
                         int *end_selection)
{
    CheckboxCell *cell = (CheckboxCell *) _cell;
    gnc_checkbox_cell_set_flag (cell, !cell->flag);
    return FALSE;
}

static void
gnc_checkbox_cell_init (CheckboxCell *cell)
{
    gnc_basic_cell_init (&cell->cell);

    gnc_checkbox_cell_set_flag (cell, FALSE);
    cell->cell.enter_cell = gnc_checkbox_cell_enter;
    cell->cell.set_value = gnc_checkbox_cell_set_value;
}

BasicCell *
gnc_checkbox_cell_new (void)
{
    CheckboxCell * cell;

    cell = g_new0 (CheckboxCell, 1);

    gnc_checkbox_cell_init (cell);

    return &cell->cell;
}

void
gnc_checkbox_cell_set_flag (CheckboxCell *cell, gboolean flag)
{
    const char *string;

    g_return_if_fail (cell != NULL);

    cell->flag = flag;
    string = gnc_checkbox_cell_get_string (flag);

    gnc_basic_cell_set_value_internal (&cell->cell, string);
}

gboolean
gnc_checkbox_cell_get_flag (CheckboxCell *cell)
{
    g_return_val_if_fail (cell != NULL, '\0');

    return cell->flag;
}

const char *
gnc_checkbox_cell_get_string (gboolean flag)
{
    return (flag ? "X" : " ");
}
