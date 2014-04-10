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
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include "config.h"

#include <gnome.h>
#include "misc-gnome-utils.h"

/* ============================================================== */
#ifdef GNOME2_PORT_IS_READY
/* Use the following for the gnome2 port only -- */

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


#else /*GNOME2_PORT_IS_READY */
/* ============================================================== */

void
xxxgtk_text_set_text (GtkText *text, const char *str)
{
	gint pos=0;
	if (!str) str = "";
	gtk_editable_delete_text (GTK_EDITABLE (text), 0, -1);
	gtk_editable_insert_text (GTK_EDITABLE (text), str,
                            strlen(str), &pos);

}

const char *
xxxgtk_text_get_text (GtkText *text)
{
 	return gtk_editable_get_chars (GTK_EDITABLE(text), 0, -1);
}

#endif
/* ===================== END OF FILE ============================ */
