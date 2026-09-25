/**
 * @file gnc-tree-view-sx-list.h
 * @brief GTK4 ColumnView implementation for Scheduled Transaction List.
 */
/********************************************************************
 * Copyright (C) 2007 Joshua Sled <jsled@asynchronous.org>
 * Author: Copyright (C) 2007 Joshua Sled <jsled@asynchronous.org>
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of version 2 and/or version 3 of the   *
 * GNU General Public                                               *
 * License as published by the Free Software Foundation.            *
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
 *******************************************************************/
#ifndef __GNC_TREE_VIEW_SX_LIST_H
#define __GNC_TREE_VIEW_SX_LIST_H

#include <gtk/gtk.h>
#include "SchedXaction.h"
#include "gnc-sx-instance-model.h"

G_BEGIN_DECLS

GtkColumnView* gnc_sx_list_view_new (GncSxInstanceModel *sx_instances);
GtkSelectionModel* gnc_sx_list_view_get_selection (GtkColumnView *view);
GList* gnc_sx_list_view_get_selected_sxes (GtkColumnView *view);
void gnc_sx_list_view_select_sxes (GtkColumnView *view, GList *sxs);
void gnc_sx_list_view_refresh (GtkColumnView *view);
gboolean gnc_sx_list_view_enabled_column_visible (GtkColumnView *view);

G_END_DECLS

#endif /* __GNC_TREE_VIEW_SX_LIST_H */
