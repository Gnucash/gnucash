/********************************************************************\
 * gnc-tree-view-account.h -- GtkTreeView implementation to display *
 *                            accounts in a GtkTreeView.            *
 * Copyright (C) 2003 David Hampton <hampton@employees.org>         *
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#ifndef __GNC_TREE_VIEW_ACCOUNT_H
#define __GNC_TREE_VIEW_ACCOUNT_H

#include <gtk/gtktreemodel.h>
#include <gtk/gtktreeview.h>

#include "gnc-account-tree.h"
#include "Group.h"
#include "eggtreemodelfilter.h"

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_TREE_VIEW_ACCOUNT            (gnc_tree_view_account_get_type ())
#define GNC_TREE_VIEW_ACCOUNT(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_TREE_VIEW_ACCOUNT, GncTreeViewAccount))
#define GNC_TREE_VIEW_ACCOUNT_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_TREE_VIEW_ACCOUNT, GncTreeViewAccountClass))
#define GNC_IS_TREE_VIEW_ACCOUNT(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_TREE_VIEW_ACCOUNT))
#define GNC_IS_TREE_VIEW_ACCOUNT_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_TREE_VIEW_ACCOUNT))
#define GNC_TREE_VIEW_ACCOUNT_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_TREE_VIEW_ACCOUNT, GncTreeViewAccountClass))

/* typedefs & structures */
typedef struct GncTreeViewAccountPrivate GncTreeViewAccountPrivate;

typedef struct {
	GtkTreeView parent;

	GncTreeViewAccountPrivate *priv;

	int stamp;
} GncTreeViewAccount;

typedef struct {
	GtkTreeViewClass parent;
} GncTreeViewAccountClass;

/* function prototypes */
GType         gnc_tree_view_account_get_type              (void);

GtkTreeView  *gnc_tree_view_account_new                   (void);
GtkTreeView  *gnc_tree_view_account_new_filterable        (void);

void          gnc_tree_view_update_column_visibility      (GncTreeViewAccount *account_view,
							   AccountViewInfo *avi);
void          gnc_tree_view_init_default_visibility       (AccountViewInfo *avi);
void          gnc_tree_view_account_configure_columns     (GncTreeViewAccount *account_view, GSList *list);
void          gnc_tree_view_account_set_filter            (GncTreeViewAccount *account_view, 
							   EggTreeModelFilterVisibleFunc  func,
							   gpointer                       data,
							   GtkDestroyNotify               destroy);
gboolean      gnc_tree_view_account_get_selected_account  (GncTreeViewAccount *view,
							   Account **account);

G_END_DECLS

#endif /* __GNC_TREE_VIEW_ACCOUNT_H */
