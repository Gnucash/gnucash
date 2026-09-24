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

#ifndef TREE_VIEW_UTILS_H_
#define TREE_VIEW_UTILS_H_
#include <gtk/gtk.h>
G_BEGIN_DECLS
/* Apply a content-derived fixed width to a GTK4 ColumnView column. */
void tree_view_column_set_default_width (GtkColumnViewColumn *column, const gchar *sizing_text);
G_END_DECLS
#endif
