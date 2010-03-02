/********************************************************************\
 * quickfillcell.c -- autocompletion based on memorized history     *
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
 * quickfillcell.c
 *
 * FUNCTION:
 * Implements a text cell with automatic typed-phrase
 * completion.
 *
 * HISTORY:
 * Copyright (c) 1998-2000 Linas Vepstas
 * Copyright (c) 2000 Dave Peticolas
 */

#include "config.h"

#include <glib.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "basiccell.h"
#include "gnc-ui-util.h"
#include "quickfillcell.h"


static void gnc_quickfill_cell_set_original (QuickFillCell *cell,
        const char *original);


static void
gnc_quickfill_cell_set_value_internal (BasicCell *_cell,
                                       const char *val)
{
    QuickFillCell *cell = (QuickFillCell *) _cell;
    gnc_quickfill_cell_set_value (cell, val);
}

/* when entering new cell, put cursor at end and select everything */
static gboolean
gnc_quickfill_cell_enter (BasicCell *_cell,
                          int *cursor_position,
                          int *start_selection,
                          int *end_selection)
{
    QuickFillCell *cell = (QuickFillCell *) _cell;

    *cursor_position = -1;
    *start_selection = 0;
    *end_selection = -1;

    gnc_quickfill_cell_set_original (cell, NULL);

    return TRUE;
}

static gboolean
utf8_caseequal (const char *s1, const char *s2)
{
    char *s1new;
    char *s2new;
    gboolean equal = FALSE;

    if (s1 == s2)
        return TRUE;

    if (!s1 || !s2)
        return FALSE;

    s1new = g_utf8_casefold(s1, -1);
    s2new = g_utf8_casefold(s2, -1);

    if (g_utf8_collate(s1new, s2new) == 0)
        equal = TRUE;

    g_free (s1new);
    g_free (s2new);

    return equal;
}

static gboolean
utf8_caseequal_len (const char *s1, const char *s2, guint len)
{
    gchar *s1new;
    gchar *s2new;
    const gchar *s1_offset;
    const gchar *s2_offset;
    glong s1chars;
    glong s2chars;
    glong s1_bytes_len;
    glong s2_bytes_len;
    gboolean equal = FALSE;

    if (len == 0)
        return TRUE;

    if (s1 == s2)
        return TRUE;

    if (!s1 || !s2)
        return FALSE;

    /* Obtain the number of bytes for the given number of characters */
    s1_offset = g_utf8_offset_to_pointer (s1, len);
    s2_offset = g_utf8_offset_to_pointer (s2, len);
    s1_bytes_len = s1_offset - s1;
    s2_bytes_len = s2_offset - s2;

    /* Test whether the number of characters might be too small anyway
       (dont need to examine more than bytes_len bytes to check that) */
    s1chars = g_utf8_strlen (s1, s1_bytes_len);
    s2chars = g_utf8_strlen (s2, s2_bytes_len);
    if ( (s1chars < len) || (s2chars < len) )
        return FALSE;

    /* Allocate new strings that are case-independent. */
    s1new = g_utf8_casefold (s1, s1_bytes_len);
    s2new = g_utf8_casefold (s2, s2_bytes_len);

    /* equal = utf8_caseequal (s1new, s2new); */
    /* ^^ don't call this to save one string allocation; we used
       g_utf8_casefold here already. */

    /* Now really compare the two strings */
    if (g_utf8_collate(s1new, s2new) == 0)
        equal = TRUE;

    g_free (s1new);
    g_free (s2new);

    return equal;
}

static void
gnc_quickfill_cell_modify_verify (BasicCell *_cell,
                                  const char *change,
                                  int change_len,
                                  const char *newval,
                                  int newval_len,
                                  int *cursor_position,
                                  int *start_selection,
                                  int *end_selection)
{
    QuickFillCell *cell = (QuickFillCell *) _cell;
    const char *match_str;
    QuickFill *match;

    glong newval_chars;
    glong change_chars;

    newval_chars = g_utf8_strlen(newval, newval_len);
    change_chars = g_utf8_strlen(change, change_len);

    /* If deleting, just accept */
    if (change == NULL)
    {
        /* if the new value is a prefix of the original modulo case,
         * just truncate the end of the original. Otherwise, set it
         * to NULL */
        if ((*cursor_position >= newval_chars) &&
                (cell->original != NULL) &&
                (g_utf8_strlen (cell->original, -1) >= newval_chars) &&
                utf8_caseequal_len (cell->original, newval, newval_chars))
        {
            gchar *temp = g_strndup (cell->original, newval_len);
            gnc_quickfill_cell_set_original (cell, temp);
            g_free (temp);
        }
        else
            gnc_quickfill_cell_set_original (cell, NULL);

        gnc_basic_cell_set_value_internal (&cell->cell, newval);
        return;
    }

    /* If we are inserting in the middle, just accept */
    if (*cursor_position < _cell->value_chars)
    {
        gnc_basic_cell_set_value_internal (&cell->cell, newval);
        gnc_quickfill_cell_set_original (cell, NULL);
        return;
    }

    if (cell->original == NULL)
        cell->original = g_strdup (newval);
    else if (utf8_caseequal (cell->original, _cell->value))
    {
        GString *original;

        original = g_string_new (cell->original);
        g_string_append (original, change);

        g_free (cell->original);
        cell->original = g_strdup (original->str);
        g_string_free (original, TRUE);
    }
    else
    {
        g_free (cell->original);
        cell->original = NULL;
    }

    match = gnc_quickfill_get_string_match (cell->qf, newval);

    match_str = gnc_quickfill_string (match);

    if (match_str == NULL)
    {
        if (cell->original != NULL)
            newval = cell->original;

        *cursor_position = -1;

        gnc_basic_cell_set_value_internal (&cell->cell, newval);
        return;
    }

    *start_selection = newval_chars;
    *end_selection = -1;
    *cursor_position += change_chars;

    gnc_basic_cell_set_value_internal (&cell->cell, match_str);
}

/* when leaving cell, make sure that text was put into the qf */

static void
gnc_quickfill_cell_leave (BasicCell * _cell)
{
    QuickFillCell *cell = (QuickFillCell *) _cell;

    gnc_quickfill_insert (cell->qf, _cell->value, cell->sort);
}

static void
gnc_quickfill_cell_destroy (BasicCell *bcell)
{
    QuickFillCell *cell = (QuickFillCell *) bcell;

    gnc_quickfill_destroy (cell->qf);
    cell->qf = NULL;

    g_free (cell->original);
    cell->original = NULL;

    cell->cell.enter_cell    = NULL;
    cell->cell.modify_verify = NULL;
    cell->cell.leave_cell    = NULL;
    cell->cell.set_value     = NULL;
}

static void
gnc_quickfill_cell_init (QuickFillCell *cell)
{
    gnc_basic_cell_init (&(cell->cell));

    cell->qf = gnc_quickfill_new ();
    cell->sort = QUICKFILL_LIFO;
    cell->original = NULL;

    cell->cell.destroy = gnc_quickfill_cell_destroy;

    cell->cell.enter_cell    = gnc_quickfill_cell_enter;
    cell->cell.modify_verify = gnc_quickfill_cell_modify_verify;
    cell->cell.leave_cell    = gnc_quickfill_cell_leave;
    cell->cell.set_value     = gnc_quickfill_cell_set_value_internal;
}

BasicCell *
gnc_quickfill_cell_new (void)
{
    QuickFillCell *cell;

    cell = g_new0 (QuickFillCell, 1);

    gnc_quickfill_cell_init (cell);

    return &cell->cell;
}

void
gnc_quickfill_cell_set_value (QuickFillCell *cell, const char * value)
{
    if (cell == NULL)
        return;

    gnc_basic_cell_set_value_internal (&cell->cell, value);
    gnc_quickfill_insert (cell->qf, value, cell->sort);
}

void
gnc_quickfill_cell_set_sort (QuickFillCell *cell, QuickFillSort sort)
{
    if (cell == NULL)
        return;

    cell->sort = sort;
}

static void
gnc_quickfill_cell_set_original (QuickFillCell *cell, const char *original)
{
    if (cell == NULL)
        return;

    g_free (cell->original);

    if ((original != NULL) && (*original != 0))
        cell->original = strdup (original);
    else
        cell->original = NULL;
}

void
gnc_quickfill_cell_add_completion (QuickFillCell *cell, const char *completion)
{
    if (cell == NULL)
        return;

    gnc_quickfill_insert (cell->qf, completion, cell->sort);
}
