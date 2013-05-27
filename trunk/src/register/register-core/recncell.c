/********************************************************************\
 * recncell.c -- reconcile checkbox cell                            *
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
 * recncell.c
 *
 * FUNCTION:
 * Implements a mouse-click cell that allows a series
 * of values to be clicked through.
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
#include "recncell.h"


static void gnc_recn_cell_set_value (BasicCell *_cell, const char *value);


static const char *
gnc_recn_cell_get_string (RecnCell *cell, char flag)
{
    static char str[2] = { 0, 0 };

    if (cell->get_string != NULL)
        return (cell->get_string)(flag);

    str[0] = flag;

    return str;
}

static gboolean
gnc_recn_cell_enter (BasicCell *_cell,
                     int *cursor_position,
                     int *start_selection,
                     int *end_selection)
{
    RecnCell *cell = (RecnCell *) _cell;
    char * this_flag;

    if (cell->confirm_cb &&
            ! (cell->confirm_cb (cell->flag, cell->confirm_data)))
        return FALSE;

    /* Find the current flag in the list of flags */
    this_flag = strchr (cell->flag_order, cell->flag);

    if (this_flag == NULL || *this_flag == '\0')
    {
        /* If it's not there (or the list is empty) use default_flag */
        cell->flag = cell->default_flag;

    }
    else
    {
        /* It is in the list -- choose the -next- item in the list (wrapping
         * around as necessary).
         */
        this_flag++;
        if (*this_flag != '\0')
            cell->flag = *this_flag;
        else
            cell->flag = *(cell->flag_order);
    }

    /* And set the display */
    gnc_recn_cell_set_flag (cell, cell->flag);

    return FALSE;
}

static void
gnc_recn_cell_init (RecnCell *cell)
{
    gnc_basic_cell_init (&cell->cell);

    gnc_recn_cell_set_flag (cell, '\0');
    cell->confirm_cb = NULL;
    cell->get_string = NULL;
    cell->valid_flags = "";
    cell->flag_order = "";

    cell->cell.enter_cell = gnc_recn_cell_enter;
    cell->cell.set_value = gnc_recn_cell_set_value;
}

BasicCell *
gnc_recn_cell_new (void)
{
    RecnCell * cell;

    cell = g_new0 (RecnCell, 1);

    gnc_recn_cell_init (cell);

    return &cell->cell;
}

/* assumes we are given the untranslated form */
static void
gnc_recn_cell_set_value (BasicCell *_cell, const char *value)
{
    RecnCell *cell = (RecnCell *) _cell;
    char flag;

    if (!value || *value == '\0')
    {
        cell->flag = cell->default_flag;
        gnc_basic_cell_set_value_internal (_cell, "");
        return;
    }

    flag = cell->default_flag;
    if (strchr (cell->valid_flags, *value) != NULL)
        flag = *value;

    gnc_recn_cell_set_flag (cell, flag);
}

void
gnc_recn_cell_set_flag (RecnCell *cell, char flag)
{
    const char *string;

    g_return_if_fail (cell != NULL);

    cell->flag = flag;
    string = gnc_recn_cell_get_string (cell, flag);

    gnc_basic_cell_set_value_internal (&cell->cell, string);
}

char
gnc_recn_cell_get_flag (RecnCell *cell)
{
    g_return_val_if_fail (cell != NULL, '\0');

    return cell->flag;
}

void
gnc_recn_cell_set_string_getter (RecnCell *cell,
                                 RecnCellStringGetter get_string)
{
    g_return_if_fail (cell != NULL);
    cell->get_string = get_string;
}

void
gnc_recn_cell_set_confirm_cb (RecnCell *cell, RecnCellConfirm confirm_cb,
                              gpointer data)
{
    g_return_if_fail (cell != NULL);

    cell->confirm_cb = confirm_cb;
    cell->confirm_data = data;
}

void
gnc_recn_cell_set_valid_flags (RecnCell *cell, const char *flags,
                               char default_flag)
{
    g_return_if_fail (cell != NULL);
    g_return_if_fail (flags != NULL);

    cell->valid_flags = (char *)flags;
    cell->default_flag = default_flag;
}

void
gnc_recn_cell_set_flag_order (RecnCell *cell, const char *flags)
{
    g_return_if_fail (cell != NULL);
    g_return_if_fail (flags != NULL);

    cell->flag_order = (char *)flags;
}
