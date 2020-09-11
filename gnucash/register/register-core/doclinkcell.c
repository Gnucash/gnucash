/********************************************************************\
 * doclinkcell.c -- Document Link checkbox cell                     *
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
 * doclinkcell.c
 *
 * FUNCTION:
 * Implements a mouse-click cell that allows a series
 * of values to be clicked through and return a glyth if
 * font allows it.
 *
 * HISTORY:
 * Copyright (c) 1998 Linas Vepstas
 * Copyright (c) 2000 Dave Peticolas
 * Copyright (c) 2001 Derek Atkins
 * Copyright (c) 2020 Robert Fewell
 */

#include <config.h>

#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "basiccell.h"
#include "gnc-engine.h"
#include "doclinkcell.h"
#include "gnc-ui-util.h"

static void gnc_doclink_cell_set_value (BasicCell *_cell, const char *value);

const char *
gnc_doclink_get_glyph_from_flag (char link_flag)
{
    switch (link_flag)
    {
    case WLINK:
        return GLYPH_LINK;
    case FLINK:
        return GLYPH_PAPERCLIP;
    default:
        return " ";
    }
}

static const char
gnc_doclink_get_flag_from_glyph (const char *glyph)
{
    if (strcmp (glyph, GLYPH_LINK) == 0)
        return WLINK;
    else if (strcmp (glyph, GLYPH_PAPERCLIP) == 0)
        return FLINK;
    else
        return ' ';
}

gboolean
gnc_doclink_get_use_glyphs (Doclinkcell *cell)
{
    return cell->use_glyphs;
}

static const char *
gnc_doclink_cell_get_string (Doclinkcell *cell, char flag)
{
    static char str[2] = { 0, 0 };

    if (cell->use_glyphs)
        return gnc_doclink_get_glyph_from_flag (flag);

    if (cell->get_string != NULL)
        return (cell->get_string)(flag);

    str[0] = flag;

    return str;
}

static gboolean
gnc_doclink_cell_enter (BasicCell *_cell,
                     int *cursor_position,
                     int *start_selection,
                     int *end_selection)
{
    Doclinkcell *cell = (Doclinkcell *) _cell;
    char * this_flag;

    if (cell->confirm_cb &&
            ! (cell->confirm_cb (cell->flag, cell->confirm_data)))
        return FALSE;

    if (cell->read_only == TRUE)
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
    gnc_doclink_cell_set_flag (cell, cell->flag);

    return FALSE;
}

static void
gnc_doclink_cell_init (Doclinkcell *cell)
{
    gnc_basic_cell_init (&cell->cell);

    gnc_doclink_cell_set_flag (cell, '\0');
    cell->confirm_cb = NULL;
    cell->get_string = NULL;
    cell->valid_flags = "";
    cell->flag_order = "";
    cell->read_only = FALSE;
    cell->use_glyphs = FALSE;

    cell->cell.enter_cell = gnc_doclink_cell_enter;
    cell->cell.set_value = gnc_doclink_cell_set_value;
}

BasicCell *
gnc_doclink_cell_new (void)
{
    Doclinkcell * cell;

    cell = g_new0 (Doclinkcell, 1);

    gnc_doclink_cell_init (cell);

    return &cell->cell;
}

/* assumes we are given the untranslated form */
static void
gnc_doclink_cell_set_value (BasicCell *_cell, const char *value)
{
    Doclinkcell *cell = (Doclinkcell *) _cell;
    char flag;

    if (!value || *value == '\0')
    {
        cell->flag = cell->default_flag;
        gnc_basic_cell_set_value_internal (_cell, "");
        return;
    }

    if (cell->use_glyphs)
        flag = gnc_doclink_get_flag_from_glyph (value);
    else
    {
        flag = cell->default_flag;
        if (strchr (cell->valid_flags, *value) != NULL)
            flag = *value;
    }
    gnc_doclink_cell_set_flag (cell, flag);
}

void
gnc_doclink_cell_set_flag (Doclinkcell *cell, char flag)
{
    const char *string;

    g_return_if_fail (cell != NULL);

    cell->flag = flag;
    string = gnc_doclink_cell_get_string (cell, flag);

    gnc_basic_cell_set_value_internal (&cell->cell, string);
}

char
gnc_doclink_cell_get_flag (Doclinkcell *cell)
{
    g_return_val_if_fail (cell != NULL, '\0');

    return cell->flag;
}

void
gnc_doclink_cell_set_string_getter (Doclinkcell *cell,
                                  DoclinkcellStringGetter get_string)
{
    g_return_if_fail (cell != NULL);

    cell->get_string = get_string;
}

void
gnc_doclink_cell_set_confirm_cb (Doclinkcell *cell, DoclinkcellConfirm confirm_cb,
                               gpointer data)
{
    g_return_if_fail (cell != NULL);

    cell->confirm_cb = confirm_cb;
    cell->confirm_data = data;
}

void
gnc_doclink_cell_set_valid_flags (Doclinkcell *cell, const char *flags,
                                char default_flag)
{
    g_return_if_fail (cell != NULL);
    g_return_if_fail (flags != NULL);

    cell->valid_flags = (char *)flags;
    cell->default_flag = default_flag;
}

void
gnc_doclink_cell_set_flag_order (Doclinkcell *cell, const char *flags)
{
    g_return_if_fail (cell != NULL);
    g_return_if_fail (flags != NULL);

    cell->flag_order = (char *)flags;
}

void
gnc_doclink_cell_set_read_only (Doclinkcell *cell, gboolean read_only)
{
    g_return_if_fail (cell != NULL);

    cell->read_only = read_only;
}

void
gnc_doclink_cell_set_use_glyphs (Doclinkcell *cell)
{
#ifdef MAC_INTEGRATION
    cell->use_glyphs = FALSE;
#else 
    gboolean use_glyphs = TRUE;
    gchar *test_text;
    GtkWidget *label;
    PangoLayout *test_layout;
    gint count;

    g_return_if_fail (cell != NULL);

    label = gtk_label_new (NULL);
    test_text = g_strconcat (GLYPH_LINK, ",", GLYPH_PAPERCLIP, NULL);
    test_layout = gtk_widget_create_pango_layout (GTK_WIDGET (label), test_text);

    pango_layout_set_text (test_layout, test_text, strlen (test_text));

    count = pango_layout_get_unknown_glyphs_count (test_layout);

    if (count != 0)
        use_glyphs = FALSE;

    g_object_unref (test_layout);
    g_free (test_text);

    cell->use_glyphs = use_glyphs;
#endif
}
