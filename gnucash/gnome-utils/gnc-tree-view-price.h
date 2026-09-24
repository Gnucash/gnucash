/*
 * Copyright (C) 2003,2005 David Hampton <hampton@employees.org>    *
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

#ifndef __GNC_TREE_VIEW_PRICE_H
#define __GNC_TREE_VIEW_PRICE_H
#include <gtk/gtk.h>
#include "gnc-tree-view.h"
#include "gnc-pricedb.h"
G_BEGIN_DECLS
#define GNC_TYPE_TREE_VIEW_PRICE (gnc_tree_view_price_get_type ())
G_DECLARE_FINAL_TYPE (GncTreeViewPrice, gnc_tree_view_price, GNC, TREE_VIEW_PRICE, GncTreeView)
typedef gboolean (*gnc_tree_view_price_ns_filter_func) (gnc_commodity_namespace*, gpointer data);
typedef gboolean (*gnc_tree_view_price_cm_filter_func) (gnc_commodity*, gpointer data);
typedef gboolean (*gnc_tree_view_price_pc_filter_func) (GNCPrice*, gpointer data);
GtkWidget *gnc_tree_view_price_new (QofBook *book, const gchar *first_property_name, ...);
GtkColumnView *gnc_tree_view_price_get_column_view (GncTreeViewPrice *view);
GtkSelectionModel *gnc_tree_view_price_get_selection_model (GncTreeViewPrice *view);
void gnc_tree_view_price_set_filter (GncTreeViewPrice *view, gnc_tree_view_price_ns_filter_func ns_func, gnc_tree_view_price_cm_filter_func cm_func, gnc_tree_view_price_pc_filter_func pc_func, gpointer data, GDestroyNotify destroy);
void gnc_tree_view_price_suspend_updates (GncTreeViewPrice *view);
void gnc_tree_view_price_resume_updates (GncTreeViewPrice *view);
void gnc_tree_view_price_toggle_expand (GncTreeViewPrice *view, guint position);
GNCPrice *gnc_tree_view_price_get_cursor_price (GncTreeViewPrice *view);
GNCPrice *gnc_tree_view_price_get_selected_price (GncTreeViewPrice *view);
void gnc_tree_view_price_set_selected_price (GncTreeViewPrice *view, GNCPrice *price);
GList *gnc_tree_view_price_get_selected_prices (GncTreeViewPrice *view);
GList *gnc_tree_view_price_get_selected_commodities (GncTreeViewPrice *view);
G_END_DECLS
#endif
