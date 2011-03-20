/********************************************************************\
 * numcell.c -- number handling cell incl. accelarator key support  *
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
 * numcell.c
 *
 * FUNCTION:
 * implements a gui-independent number handling cell.
 *
 * HISTORY:
 * Copyright (C) 2000 Dave Peticolas <peticola@cs.ucdavis.edu>
 */

#include "config.h"

#include <limits.h>
#include <stdlib.h>
#include <string.h>

#include "numcell.h"
#include "gnc-engine.h"


/* This static indicates the debugging module that this .o belongs to. */
/* static short module = MOD_REGISTER; */

static void gnc_num_cell_init (NumCell *cell);


/* Parses the string value and returns true if it is a
 * number. In that case, *num is set to the value parsed. */
static gboolean
gnc_parse_num (const char *string, long int *num)
{
    long int number;

    if (string == NULL)
        return FALSE;

    if (!gnc_strisnum (string))
        return FALSE;

    number = strtol (string, NULL, 10);

    if ((number == LONG_MIN) || (number == LONG_MAX))
        return FALSE;

    if (num != NULL)
        *num = number;

    return TRUE;
}

static void
gnc_num_cell_modify_verify (BasicCell *_cell,
                            const char *change,
                            int change_len,
                            const char *newval,
                            int new_val_len,
                            int *cursor_position,
                            int *start_selection,
                            int *end_selection)
{
    NumCell *cell = (NumCell *) _cell;
    gboolean accel = FALSE;
    gboolean is_num;
    long int number = 0;
    gunichar uc;
    glong change_chars;

    if (change == NULL) /* if we are deleting */
        /* then just accept the proposed change */
    {
        gnc_basic_cell_set_value_internal (&cell->cell, newval);
        return;
    }

    change_chars = g_utf8_strlen (change, -1);

    if ((change_chars == 0) || /* if we are deleting       */
            (change_chars > 1))    /* or entering > 1 char     */
        /* then just accept the proposed change */
    {
        gnc_basic_cell_set_value_internal (&cell->cell, newval);
        return;
    }

    /* otherwise, it may be an accelerator key. */

    is_num = gnc_parse_num (_cell->value, &number);

    if (is_num && (number < 0))
        is_num = FALSE;

    uc = g_utf8_get_char (change);
    switch (uc)
    {
    case '+':
    case '=':
        number++;
        accel = TRUE;
        break;

    case '_':
    case '-':
        number--;
        accel = TRUE;
        break;

    case '}':
    case ']':
        number += 10;
        accel = TRUE;
        break;

    case '{':
    case '[':
        number -= 10;
        accel = TRUE;
        break;
    }

    if (number < 0)
        number = 0;

    /* If there is already a non-number there, don't accelerate. */
    if (accel && !is_num && (safe_strcmp(_cell->value, "") != 0))
        accel = FALSE;

    if (accel)
    {
        char buff[128];

        if (!is_num)
            number = cell->next_num;

        strcpy (buff, "");
        snprintf (buff, sizeof(buff), "%ld", number);

        if (safe_strcmp (buff, "") == 0)
            return;

        gnc_basic_cell_set_value_internal (&cell->cell, buff);

        *cursor_position = -1;

        return;
    }

    gnc_basic_cell_set_value_internal (&cell->cell, newval);
}

BasicCell *
gnc_num_cell_new (void)
{
    NumCell *cell;

    cell = g_new0 (NumCell, 1);

    gnc_num_cell_init (cell);

    return &cell->cell;
}

static void
gnc_num_cell_set_value_internal (BasicCell *_cell, const char *str)
{
    NumCell *cell = (NumCell *) _cell;

    if (!cell->next_num_set)
    {
        long int number;

        if (gnc_parse_num (str, &number))
            cell->next_num = number + 1;
    }

    gnc_basic_cell_set_value_internal (_cell, str);
}

void
gnc_num_cell_set_value (NumCell *cell, const char *str)
{
    if (!cell)
        return;

    gnc_num_cell_set_value_internal (&cell->cell, str);
}

gboolean
gnc_num_cell_set_last_num (NumCell *cell, const char *str)
{
    long int number;

    if (!cell)
        return FALSE;

    if (gnc_parse_num (str, &number))
    {
        cell->next_num = number + 1;
        cell->next_num_set = TRUE;
        return TRUE;
    }

    return FALSE;
}

static void
gnc_num_cell_init (NumCell *cell)
{
    gnc_basic_cell_init (&(cell->cell));

    cell->next_num = 0;
    cell->next_num_set = FALSE;

    cell->cell.modify_verify = gnc_num_cell_modify_verify;
    cell->cell.set_value = gnc_num_cell_set_value_internal;
}
