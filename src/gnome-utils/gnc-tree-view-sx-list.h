/**
 * @addtogroup GUI
 * @{
 * @file gnc-tree-view-sx-list.h
 * @brief GncTreeView implementation for Scheduled Transaction List.
 * @author Copyright (C) 2007 Joshua Sled <jsled@asynchronous.org>
 **/
/********************************************************************
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of version 2 of the GNU General Public *
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

#include <gtk/gtktreemodel.h>
#include <gtk/gtktreeview.h>
#include "gnc-tree-view.h"

#include "SchedXaction.h"
#include "gnc-sx-instance-model.h"
#include "gnc-ui-util.h"

G_BEGIN_DECLS

#define GNC_TYPE_TREE_VIEW_SX_LIST            (gnc_tree_view_sx_list_get_type ())
#define GNC_TREE_VIEW_SX_LIST(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_TREE_VIEW_SX_LIST, GncTreeViewSxList))
#define GNC_TREE_VIEW_SX_LIST_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_TREE_VIEW_SX_LIST, GncTreeViewSxListClass))
#define GNC_IS_TREE_VIEW_SX_LIST(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_TREE_VIEW_SX_LIST))
#define GNC_IS_TREE_VIEW_SX_LIST_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_TREE_VIEW_SX_LIST))
#define GNC_TREE_VIEW_SX_LIST_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_TREE_VIEW_SX_LIST, GncTreeViewSxListClass))

typedef struct
{
    GncTreeView gnc_tree_view;
} GncTreeViewSxList;

typedef struct
{
    GncTreeViewClass gnc_tree_view;
} GncTreeViewSxListClass;

GType gnc_tree_view_sx_list_get_type(void);

GtkTreeView* gnc_tree_view_sx_list_new(GncSxInstanceModel *sx_instances);

SchedXaction* gnc_tree_view_sx_list_get_sx_from_path(GncTreeViewSxList *view, GtkTreePath *path);

/** @} */

G_END_DECLS

#endif /* __GNC_TREE_VIEW_SX_LIST_H */
