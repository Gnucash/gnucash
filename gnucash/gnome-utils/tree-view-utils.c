/*
 * Copyright (C) 2013 Geert Janssens <geert@kobaltwit.be>
 * Author: Geert Janssens <geert@kobaltwit.be>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

#include <config.h>
#include "tree-view-utils.h"
void
tree_view_column_set_default_width (GtkColumnViewColumn *column, const gchar *sizing_text)
{
    g_return_if_fail (GTK_IS_COLUMN_VIEW_COLUMN (column));
    /* Conservative average glyph width preserves the old sizing hint without a widget-specific Pango layout. */ gtk_column_view_column_set_fixed_width (column, MAX (48, (gint)g_utf8_strlen (sizing_text? sizing_text: "", -1) * 8 + 10));
}
