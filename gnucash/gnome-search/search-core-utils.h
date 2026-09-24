/*
 * search-core-utils.h -- common functions for search code
 * Copyright (C) 2006 David Hampton <hampton@employees.org>
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
 *
 */

/* Functions for the common GTK4 search-operator selector used by all search
 * widgets. Search operators are identified by their stable numeric value,
 * never by the position of the displayed row.
 */

#ifndef GNC_SEARCH_CORE_UTILS_H
#define GNC_SEARCH_CORE_UTILS_H

#include <gtk/gtk.h>

G_BEGIN_DECLS

GtkWidget *gnc_search_drop_down_new (void);
void gnc_search_drop_down_add (GtkDropDown *drop_down, const gchar *text,
                               guint value);
guint gnc_search_drop_down_get_active (GtkDropDown *drop_down);
void gnc_search_drop_down_set_active (GtkDropDown *drop_down, guint value);
void gnc_search_drop_down_changed (GtkDropDown *drop_down, guint *value);

G_END_DECLS

#endif /* GNC_SEARCH_CORE_UTILS_H */
