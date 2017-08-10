/*   Utilities for GTimeTracker - a time tracker
 *   Copyright (C) 2001 Linas Vepstas
 *
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *   02110-1301 USA
 */

#include "config.h"

#include <gtk/gtk.h>
#include <string.h>

#include "misc-gnome-utils.h"

/* ============================================================== */

void
xxxgtk_textview_set_text (GtkTextView *text, const char *str)
{
    GtkTextBuffer *buff = gtk_text_view_get_buffer (text);
    if (!str) str = "";
    gtk_text_buffer_set_text (buff, str, strlen (str));

}

char *
xxxgtk_textview_get_text (GtkTextView *text)
{
    GtkTextIter start, end;
    GtkTextBuffer *buff = gtk_text_view_get_buffer (text);
    gtk_text_buffer_get_start_iter (buff, &start);
    gtk_text_buffer_get_end_iter (buff, &end);
    return gtk_text_buffer_get_text(buff, &start, &end, TRUE);
}


/* ===================== END OF FILE ============================ */
