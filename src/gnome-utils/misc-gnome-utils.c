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

/* ===================== END OF FILE ============================ */
