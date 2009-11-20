/* gnc-tree-model-account-drag.h
 * Copyright (C) 2009 Matt Lavin <matt.lavin@gmail.com>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

/** @addtogroup GUI
    @{ */
/** @addtogroup GuiTreeModel GnuCash Tree Model
    @{ */
/** @file gnc-tree-model-account-drag.h
    @brief GtkTreeModel wrapper that supports dragging Accounts for reparenting
    @author Matt Lavin <matt.lavin@gmail.com>
*/

#ifndef __GNC_TREE_MODEL_ACCOUNT_DRAG_H
#define __GNC_TREE_MODEL_ACCOUNT_DRAG_H

#include <gtk/gtk.h>
#include <gtk/gtktreemodel.h>

#include "config.h"
#include "Account.h"
#include "gnc-tree-view-account.h"

G_BEGIN_DECLS

#define GNC_TYPE_TREE_MODEL_ACCOUNT_DRAG			(gnc_tree_model_account_drag_get_type ())
#define GNC_TREE_MODEL_ACCOUNT_DRAG(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_TREE_MODEL_ACCOUNT_DRAG, GncTreeModelAccountDrag))
#define GNC_TREE_MODEL_ACCOUNT_DRAG_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_TREE_MODEL_ACCOUNT_DRAG, GncTreeModelAccountDragClass))
#define GNC_IS_TREE_MODEL_ACCOUNT_DRAG(obj)			(G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_TREE_MODEL_ACCOUNT_DRAG))
#define GNC_IS_TREE_MODEL_ACCOUNT_DRAG_CLASS(klass)		(G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_TREE_MODEL_ACCOUNT_DRAG))
#define GNC_TREE_MODEL_ACCOUNT_DRAG_GET_CLASS(obj)		(G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_TREE_MODEL_ACCOUNT_DRAG, GncTreeModelAccountDragClass))

typedef Account* (*GncTreeModelAccountDragLookupFunc) (GtkTreePath  *path,
						       gpointer user_data);

typedef struct {
  GObject parent;

  /* < private > */
  GtkTreeModel *child_model;
  GtkWidget* widget;
  GncTreeModelAccountDragLookupFunc lookup_func;
  gpointer user_data;

  gulong changed_id;
  gulong inserted_id;
  gulong has_child_toggled_id;
  gulong deleted_id;
  gulong reordered_id;
} GncTreeModelAccountDrag;

typedef struct {
  GObjectClass parent_class;
} GncTreeModelAccountDragClass;


GType         gnc_tree_model_account_drag_get_type       (void) G_GNUC_CONST;
GtkTreeModel *gnc_tree_model_account_drag_new_with_model (GtkTreeModel *child_model, GtkWidget* widget, GncTreeModelAccountDragLookupFunc lookup_func, gpointer user_data);
GtkTreeModel *gnc_tree_model_account_drag_get_model      (GncTreeModelAccountDrag *model);            

G_END_DECLS

#endif /* __GNC_TREE_MODEL_ACCOUNT_DRAG_H */
