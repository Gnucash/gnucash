/*
 * Copyright (C) 2003 David Hampton <hampton@employees.org>         *
 * Author: David Hampton <hampton@employees.org>
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

#ifndef __GNC_TREE_VIEW_H
#define __GNC_TREE_VIEW_H
#include <gtk/gtk.h>
G_BEGIN_DECLS
#define GNC_TYPE_TREE_VIEW (gnc_tree_view_get_type ())
G_DECLARE_DERIVABLE_TYPE (GncTreeView, gnc_tree_view, GNC, TREE_VIEW, GtkBox)
#define GNC_TREE_VIEW_NAME "GncTreeView"
struct _GncTreeViewClass { GtkBoxClass parent_class; };
GtkColumnView *gnc_tree_view_get_column_view (GncTreeView *view);
void gnc_column_view_bind_grid_line_preferences (GtkColumnView *view);
void gnc_column_view_unbind_grid_line_preferences (GtkColumnView *view);
void gnc_tree_view_set_state_section (GncTreeView *view, const gchar *section);
const gchar *gnc_tree_view_get_state_section (GncTreeView *view);
void gnc_tree_view_set_show_column_menu (GncTreeView *view, gboolean visible);
gboolean gnc_tree_view_get_show_column_menu (GncTreeView *view);
G_END_DECLS
#endif
