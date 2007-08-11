/********************************************************************\
 * gnc-tree-view-transaction.h -- GtkTreeView implementation to     *
 *                        display Transactions in a GtkTreeView.    *
 * Copyright (C) 2006-2007 Chris Shoemaker <c.shoemaker@cox.net>    *
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

#ifndef __GNC_TREE_VIEW_TRANSACTION_H
#define __GNC_TREE_VIEW_TRANSACTION_H

#include <gtk/gtktreemodel.h>
#include <gtk/gtktreeview.h>
#include "gnc-tree-view.h"

#include "gnc-ui-util.h"
#include "gnc-tree-model-transaction.h"

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_TREE_VIEW_TRANSACTION            (gnc_tree_view_transaction_get_type ())
#define GNC_TREE_VIEW_TRANSACTION(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_TREE_VIEW_TRANSACTION, GncTreeViewTransaction))
#define GNC_TREE_VIEW_TRANSACTION_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_TREE_VIEW_TRANSACTION, GncTreeViewTransactionClass))
#define GNC_IS_TREE_VIEW_TRANSACTION(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_TREE_VIEW_TRANSACTION))
#define GNC_IS_TREE_VIEW_TRANSACTION_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_TREE_VIEW_TRANSACTION))
#define GNC_TREE_VIEW_TRANSACTION_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_TREE_VIEW_TRANSACTION, GncTreeViewTransactionClass))

/* typedefs & structures */
typedef struct GncTreeViewTransactionPrivate GncTreeViewTransactionPrivate;
typedef struct {
	GncTreeView parent;

	GncTreeViewTransactionPrivate *priv;

	int stamp;
} GncTreeViewTransaction;

typedef struct {
	GncTreeViewClass parent;
} GncTreeViewTransactionClass;



/* Standard g_object type */
GType         gnc_tree_view_transaction_get_type              (void);

GncTreeViewTransaction *
gnc_tree_view_transaction_new_with_model(GncTreeModelTransaction *model);

Account *
gnc_tree_view_transaction_get_anchor(GncTreeViewTransaction *tv);

Split * 
gnc_tree_view_transaction_get_selected_split(GncTreeViewTransaction *tv);
Transaction *
gnc_tree_view_transaction_get_selected_trans(GncTreeViewTransaction *tv);

void gnc_tree_view_transaction_delete_selected(GncTreeViewTransaction *tv);

void gnc_tree_view_transaction_reinit_trans(GncTreeViewTransaction *tv);

void gnc_tree_view_transaction_enter(GncTreeViewTransaction *tv);

void gnc_tree_view_transaction_cancel_edit(GncTreeViewTransaction *tv);

void gnc_tree_view_transaction_void(GncTreeViewTransaction *tv);
void gnc_tree_view_transaction_unvoid(GncTreeViewTransaction *tv);


void gnc_tree_view_transaction_goto_blank_trans(GncTreeViewTransaction *tv);

void gnc_tree_view_transaction_select_split(GncTreeViewTransaction *tv, 
                                            Split *split);
void gnc_tree_view_transaction_copy_trans_to_clipboard(
    GncTreeViewTransaction *tv);
void gnc_tree_view_transaction_paste_trans_from_clipboard(
    GncTreeViewTransaction *tv);


/** @} */


G_END_DECLS

#endif /* __GNC_TREE_VIEW_TRANSACTION_H */
