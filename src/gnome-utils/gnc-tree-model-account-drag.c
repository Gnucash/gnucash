/* gnc-tree-model-account-drag.c
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
/** @file gnc-tree-model-account-drag.c
    @brief GtkTreeModel wrapper that supports dragging Accounts for reparenting
    @author Matt Lavin <matt.lavin@gmail.com>
*/

#include "gnc-tree-model-account-drag.h"
#include "gnc-component-manager.h"

#include <glib/gi18n.h>

/** Static Globals *******************************************************/
static QofLogModule log_module = GNC_MOD_GUI;

/* general (object/interface init, etc) */
static void gnc_tree_model_account_drag_tree_model_init       (GtkTreeModelIface     *iface);
static void gnc_tree_model_account_drag_tree_sortable_init    (GtkTreeSortableIface  *iface);
static void gnc_tree_model_account_drag_drag_source_init      (GtkTreeDragSourceIface*iface);
static void gnc_tree_model_account_drag_drag_dest_init        (GtkTreeDragDestIface  *iface);
static void gnc_tree_model_account_drag_finalize              (GObject               *object);

/* TreeModel signal handlers */
static void gnc_tree_model_account_drag_row_changed(GtkTreeModel *c_model,
                                                    GtkTreePath *c_path,
                                                    GtkTreeIter *c_iter,
                                                    gpointer data);
static void gnc_tree_model_account_drag_row_inserted(GtkTreeModel *c_model,
                                                     GtkTreePath *c_path,
                                                     GtkTreeIter *c_iter,
                                                     gpointer data);
static void gnc_tree_model_account_drag_row_has_child_toggled(GtkTreeModel *c_model,
                                                              GtkTreePath *c_path,
                                                              GtkTreeIter *c_iter,
                                                              gpointer data);
static void gnc_tree_model_account_drag_row_deleted(GtkTreeModel *c_model,
                                                    GtkTreePath *c_path,
                                                    gpointer data);
static void gnc_tree_model_account_drag_rows_reordered(GtkTreeModel *c_model,
                                                       GtkTreePath *c_path,
                                                       GtkTreeIter *c_iter,
                                                       gint *new_order,
                                                       gpointer data);

/* TreeModel interface */
static GtkTreeModelFlags gnc_tree_model_account_drag_get_flags     (GtkTreeModel          *tree_model);
static gint         gnc_tree_model_account_drag_get_n_columns      (GtkTreeModel          *tree_model);
static GType        gnc_tree_model_account_drag_get_column_type    (GtkTreeModel          *tree_model,
								    gint                   index);
static gboolean     gnc_tree_model_account_drag_get_iter           (GtkTreeModel          *tree_model,
								    GtkTreeIter           *iter,
								    GtkTreePath           *path);
static GtkTreePath *gnc_tree_model_account_drag_get_path           (GtkTreeModel          *tree_model,
								    GtkTreeIter           *iter);
static void         gnc_tree_model_account_drag_get_value          (GtkTreeModel          *tree_model,
								    GtkTreeIter           *iter,
								    gint                   column,
								    GValue                *value);
static gboolean     gnc_tree_model_account_drag_iter_next          (GtkTreeModel          *tree_model,
								    GtkTreeIter           *iter);
static gboolean     gnc_tree_model_account_drag_iter_children      (GtkTreeModel          *tree_model,
								    GtkTreeIter           *iter,
								    GtkTreeIter           *parent);
static gboolean     gnc_tree_model_account_drag_iter_has_child     (GtkTreeModel          *tree_model,
								    GtkTreeIter           *iter);
static gint         gnc_tree_model_account_drag_iter_n_children    (GtkTreeModel          *tree_model,
								    GtkTreeIter           *iter);
static gboolean     gnc_tree_model_account_drag_iter_nth_child     (GtkTreeModel          *tree_model,
								    GtkTreeIter           *iter,
								    GtkTreeIter           *parent,
								    gint                   n);
static gboolean     gnc_tree_model_account_drag_iter_parent        (GtkTreeModel          *tree_model,
								    GtkTreeIter           *iter,
								    GtkTreeIter           *child);
static void         gnc_tree_model_account_drag_ref_node           (GtkTreeModel          *tree_model,
								    GtkTreeIter           *iter);
static void         gnc_tree_model_account_drag_unref_node         (GtkTreeModel          *tree_model,
								    GtkTreeIter           *iter);

/* TreeDragSource interface */
static gboolean     gnc_tree_model_account_drag_row_draggable         (GtkTreeDragSource      *drag_source,
								       GtkTreePath            *path);
static gboolean     gnc_tree_model_account_drag_drag_data_get         (GtkTreeDragSource      *drag_source,
								       GtkTreePath            *path,
								       GtkSelectionData       *selection_data);
static gboolean     gnc_tree_model_account_drag_drag_data_delete      (GtkTreeDragSource      *drag_source,
								       GtkTreePath            *path);

/* TreeDragDest interface */
static  gboolean    gnc_tree_model_account_drag_drag_data_received(GtkTreeDragDest   *drag_dest,
								   GtkTreePath       *dest,
								   GtkSelectionData  *selection_data);

static  gboolean    gnc_tree_model_account_drag_row_drop_possible(GtkTreeDragDest   *drag_dest,
								  GtkTreePath       *dest_path,
								  GtkSelectionData  *selection_data);


/* TreeSortable interface */
static gboolean     gnc_tree_model_account_drag_get_sort_column_id    (GtkTreeSortable        *sortable,
								       gint                   *sort_column_id,
								       GtkSortType            *order);
static void         gnc_tree_model_account_drag_set_sort_column_id    (GtkTreeSortable        *sortable,
								       gint                    sort_column_id,
								       GtkSortType        order);
static void         gnc_tree_model_account_drag_set_sort_func         (GtkTreeSortable        *sortable,
								       gint                    sort_column_id,
								       GtkTreeIterCompareFunc  func,
								       gpointer                data,
								       GDestroyNotify          destroy);
static void         gnc_tree_model_account_drag_set_default_sort_func (GtkTreeSortable        *sortable,
								       GtkTreeIterCompareFunc  func,
								       gpointer                data,
								       GDestroyNotify          destroy);
static gboolean     gnc_tree_model_account_drag_has_default_sort_func (GtkTreeSortable     *sortable);

G_DEFINE_TYPE_WITH_CODE (GncTreeModelAccountDrag, gnc_tree_model_account_drag, G_TYPE_OBJECT,
			 G_IMPLEMENT_INTERFACE (GTK_TYPE_TREE_MODEL,
						gnc_tree_model_account_drag_tree_model_init)
			 G_IMPLEMENT_INTERFACE (GTK_TYPE_TREE_SORTABLE,
						gnc_tree_model_account_drag_tree_sortable_init)
			 G_IMPLEMENT_INTERFACE (GTK_TYPE_TREE_DRAG_SOURCE,
						gnc_tree_model_account_drag_drag_source_init)
			 G_IMPLEMENT_INTERFACE (GTK_TYPE_TREE_DRAG_DEST,
						gnc_tree_model_account_drag_drag_dest_init))


static void
gnc_tree_model_account_drag_init (GncTreeModelAccountDrag *tree_model_drag)
{
}

static void
gnc_tree_model_account_drag_class_init (GncTreeModelAccountDragClass *class)
{
  GObjectClass *object_class;

  object_class = (GObjectClass *) class;

  object_class->finalize = gnc_tree_model_account_drag_finalize;
}


static void
gnc_tree_model_account_drag_tree_model_init (GtkTreeModelIface *iface)
{
  iface->get_flags = gnc_tree_model_account_drag_get_flags;
  iface->get_n_columns = gnc_tree_model_account_drag_get_n_columns;
  iface->get_column_type = gnc_tree_model_account_drag_get_column_type;
  iface->get_iter = gnc_tree_model_account_drag_get_iter;
  iface->get_path = gnc_tree_model_account_drag_get_path;
  iface->get_value = gnc_tree_model_account_drag_get_value;
  iface->iter_next = gnc_tree_model_account_drag_iter_next;
  iface->iter_children = gnc_tree_model_account_drag_iter_children;
  iface->iter_has_child = gnc_tree_model_account_drag_iter_has_child;
  iface->iter_n_children = gnc_tree_model_account_drag_iter_n_children;
  iface->iter_nth_child = gnc_tree_model_account_drag_iter_nth_child;
  iface->iter_parent = gnc_tree_model_account_drag_iter_parent;
  iface->ref_node = gnc_tree_model_account_drag_ref_node;
  iface->unref_node = gnc_tree_model_account_drag_unref_node;
}

static void
gnc_tree_model_account_drag_tree_sortable_init (GtkTreeSortableIface *iface)
{
  iface->get_sort_column_id = gnc_tree_model_account_drag_get_sort_column_id;
  iface->set_sort_column_id = gnc_tree_model_account_drag_set_sort_column_id;
  iface->set_sort_func = gnc_tree_model_account_drag_set_sort_func;
  iface->set_default_sort_func = gnc_tree_model_account_drag_set_default_sort_func;
  iface->has_default_sort_func = gnc_tree_model_account_drag_has_default_sort_func;
}

static void
gnc_tree_model_account_drag_drag_source_init (GtkTreeDragSourceIface *iface)
{
  iface->row_draggable = gnc_tree_model_account_drag_row_draggable;
  iface->drag_data_delete = gnc_tree_model_account_drag_drag_data_delete;
  iface->drag_data_get = gnc_tree_model_account_drag_drag_data_get;
}

static void
gnc_tree_model_account_drag_drag_dest_init (GtkTreeDragDestIface *iface)
{
  iface->drag_data_received= gnc_tree_model_account_drag_drag_data_received;
  iface->row_drop_possible = gnc_tree_model_account_drag_row_drop_possible;
}

/**
 * gnc_tree_model_account_drag_new_with_model:
 * @child_model: A #GtkTreeModel
 *
 * Creates a new #GtkTreeModel, with @child_model as the child model.
 *
 * Return value: A new #GtkTreeModel.
 */
GtkTreeModel *
gnc_tree_model_account_drag_new_with_model (GtkTreeModel *child_model,
                                            GtkWidget* widget,
                                            GncTreeModelAccountDragLookupFunc lookup_func,
                                            gpointer user_data)
{
  GtkTreeModel *retval;
  GncTreeModelAccountDrag* model;

  g_return_val_if_fail (GTK_IS_TREE_MODEL (child_model), NULL);
  g_return_val_if_fail (lookup_func != NULL, NULL);

  retval = g_object_new (gnc_tree_model_account_drag_get_type (), NULL);

  model = GNC_TREE_MODEL_ACCOUNT_DRAG(retval);
  g_object_ref(child_model);
  model->child_model = child_model;
  g_object_ref(widget);
  model->widget = widget;
  model->lookup_func = lookup_func;
  model->user_data = user_data;

  // Register signal handlers
  model->changed_id = g_signal_connect (child_model, "row-changed",
      G_CALLBACK (gnc_tree_model_account_drag_row_changed),
      model);
  model->inserted_id = g_signal_connect (child_model, "row-inserted",
      G_CALLBACK (gnc_tree_model_account_drag_row_inserted),
      model);
  model->has_child_toggled_id
      = g_signal_connect (child_model, "row-has-child-toggled",
          G_CALLBACK (gnc_tree_model_account_drag_row_has_child_toggled),
          model);
  model->deleted_id = g_signal_connect (child_model, "row-deleted",
      G_CALLBACK (gnc_tree_model_account_drag_row_deleted),
      model);
  model->reordered_id = g_signal_connect (child_model, "rows-reordered",
      G_CALLBACK (gnc_tree_model_account_drag_rows_reordered),
      model);


  return retval;
}

GtkTreeModel *
gnc_tree_model_account_drag_get_model (GncTreeModelAccountDrag *model)
{
  g_return_val_if_fail (model != NULL, NULL);
  return model->child_model;
}

/* GObject callbacks */
static void
gnc_tree_model_account_drag_finalize (GObject *object)
{
  GncTreeModelAccountDrag *tree_model = (GncTreeModelAccountDrag *) object;

  // Disconnect signal handlers
  g_signal_handler_disconnect (tree_model->child_model,
                               tree_model->changed_id);
  g_signal_handler_disconnect (tree_model->child_model,
                               tree_model->inserted_id);
  g_signal_handler_disconnect (tree_model->child_model,
                               tree_model->has_child_toggled_id);
  g_signal_handler_disconnect (tree_model->child_model,
                               tree_model->deleted_id);
  g_signal_handler_disconnect (tree_model->child_model,
                               tree_model->reordered_id);


  g_object_unref(tree_model->child_model);
  tree_model->child_model = NULL;
  tree_model->lookup_func = NULL;
  tree_model->user_data = NULL;

  g_object_unref(tree_model->widget);
  tree_model->widget = NULL;

  /* must chain up */
  G_OBJECT_CLASS (gnc_tree_model_account_drag_parent_class)->finalize (object);
}

/* TreeModel signal handlers */
static void
gnc_tree_model_account_drag_row_changed(GtkTreeModel *c_model,
                                        GtkTreePath *c_path,
                                        GtkTreeIter *c_iter,
                                        gpointer data)
{
  g_return_if_fail(GNC_IS_TREE_MODEL_ACCOUNT_DRAG(data));
  gtk_tree_model_row_changed(GTK_TREE_MODEL(data), c_path, c_iter);
}

static void
gnc_tree_model_account_drag_row_inserted(GtkTreeModel *c_model,
                                         GtkTreePath *c_path,
                                         GtkTreeIter *c_iter,
                                         gpointer data)
{
  g_return_if_fail(GNC_IS_TREE_MODEL_ACCOUNT_DRAG(data));
  gtk_tree_model_row_inserted(GTK_TREE_MODEL(data), c_path, c_iter);
}

static void
gnc_tree_model_account_drag_row_has_child_toggled(GtkTreeModel *c_model,
                                                  GtkTreePath *c_path,
                                                  GtkTreeIter *c_iter,
                                                  gpointer data)
{
  g_return_if_fail(GNC_IS_TREE_MODEL_ACCOUNT_DRAG(data));
  gtk_tree_model_row_has_child_toggled(GTK_TREE_MODEL(data), c_path, c_iter);
}

static void
gnc_tree_model_account_drag_row_deleted(GtkTreeModel *c_model,
                                        GtkTreePath *c_path,
                                        gpointer data)
{
  g_return_if_fail(GNC_IS_TREE_MODEL_ACCOUNT_DRAG(data));
  gtk_tree_model_row_deleted(GTK_TREE_MODEL(data), c_path);
}

static void
gnc_tree_model_account_drag_rows_reordered(GtkTreeModel *c_model,
                                           GtkTreePath *c_path,
                                           GtkTreeIter *c_iter,
                                           gint *new_order,
                                           gpointer data)
{
  g_return_if_fail(GNC_IS_TREE_MODEL_ACCOUNT_DRAG(data));
  gtk_tree_model_rows_reordered(GTK_TREE_MODEL(data), c_path, c_iter, new_order);
}

/* Fulfill our model requirements */
static GtkTreeModelFlags
gnc_tree_model_account_drag_get_flags (GtkTreeModel *tree_model)
{
  GncTreeModelAccountDrag *tree_model_drag = (GncTreeModelAccountDrag *) tree_model;

  g_return_val_if_fail (tree_model_drag->child_model != NULL, 0);

  return gtk_tree_model_get_flags (tree_model_drag->child_model);
}

static gint
gnc_tree_model_account_drag_get_n_columns (GtkTreeModel *tree_model)
{
  GncTreeModelAccountDrag *tree_model_drag = (GncTreeModelAccountDrag *) tree_model;

  g_return_val_if_fail (tree_model_drag->child_model != NULL, 0);

  return gtk_tree_model_get_n_columns (tree_model_drag->child_model);
}

static GType
gnc_tree_model_account_drag_get_column_type (GtkTreeModel *tree_model,
					     gint          index)
{
  GncTreeModelAccountDrag *tree_model_drag = (GncTreeModelAccountDrag *) tree_model;

  g_return_val_if_fail (tree_model_drag->child_model != NULL, G_TYPE_INVALID);

  return gtk_tree_model_get_column_type (tree_model_drag->child_model, index);
}

static gboolean
gnc_tree_model_account_drag_get_iter (GtkTreeModel *tree_model,
				      GtkTreeIter  *iter,
				      GtkTreePath  *path)
{
  GncTreeModelAccountDrag *tree_model_drag = (GncTreeModelAccountDrag *) tree_model;

  g_return_val_if_fail (tree_model_drag->child_model != NULL, FALSE);
  
  return gtk_tree_model_get_iter (tree_model_drag->child_model, iter, path);
}  
 
static GtkTreePath *
gnc_tree_model_account_drag_get_path (GtkTreeModel *tree_model,
				      GtkTreeIter  *iter)
{
  GncTreeModelAccountDrag *tree_model_drag = (GncTreeModelAccountDrag *) tree_model;
 
  g_return_val_if_fail (tree_model_drag->child_model != NULL, NULL);
 
  return gtk_tree_model_get_path (tree_model_drag->child_model, iter);
}

static void
gnc_tree_model_account_drag_get_value (GtkTreeModel *tree_model,
				       GtkTreeIter  *iter,
				       gint          column,
				       GValue       *value)
{
  GncTreeModelAccountDrag *tree_model_drag = (GncTreeModelAccountDrag *) tree_model;

  g_return_if_fail (tree_model_drag->child_model != NULL);

  gtk_tree_model_get_value (tree_model_drag->child_model,
			    iter, column, value);
}

static gboolean
gnc_tree_model_account_drag_iter_next (GtkTreeModel *tree_model,
				       GtkTreeIter  *iter)
{
  GncTreeModelAccountDrag *tree_model_drag = (GncTreeModelAccountDrag *) tree_model;

  g_return_val_if_fail (tree_model_drag->child_model != NULL, FALSE);

  return gtk_tree_model_iter_next (tree_model_drag->child_model, iter);
}

static gboolean
gnc_tree_model_account_drag_iter_children (GtkTreeModel *tree_model,
					   GtkTreeIter  *iter,
					   GtkTreeIter  *parent)
{
  GncTreeModelAccountDrag *tree_model_drag = (GncTreeModelAccountDrag *) tree_model;

  g_return_val_if_fail (tree_model_drag->child_model != NULL, FALSE);

  return gtk_tree_model_iter_children (tree_model_drag->child_model, iter, parent);
}

static gboolean
gnc_tree_model_account_drag_iter_has_child (GtkTreeModel *tree_model,
					    GtkTreeIter  *iter)
{
  GncTreeModelAccountDrag *tree_model_drag = (GncTreeModelAccountDrag *) tree_model;

  g_return_val_if_fail (tree_model_drag->child_model != NULL, FALSE);

  return gtk_tree_model_iter_has_child (tree_model_drag->child_model, iter);
}

static gint
gnc_tree_model_account_drag_iter_n_children (GtkTreeModel *tree_model,
					     GtkTreeIter  *iter)
{
  GncTreeModelAccountDrag *tree_model_drag = (GncTreeModelAccountDrag *) tree_model;

  g_return_val_if_fail (tree_model_drag->child_model != NULL, 0);

  return gtk_tree_model_iter_n_children (tree_model_drag->child_model, iter);
}

static gboolean
gnc_tree_model_account_drag_iter_nth_child (GtkTreeModel *tree_model,
				    GtkTreeIter  *iter,
				    GtkTreeIter  *parent,
				    gint          n)
{
  GncTreeModelAccountDrag *tree_model_drag = (GncTreeModelAccountDrag *) tree_model;

  g_return_val_if_fail (tree_model_drag->child_model != NULL, FALSE);

  return gtk_tree_model_iter_nth_child(tree_model_drag->child_model, iter, parent, n);
}

static gboolean
gnc_tree_model_account_drag_iter_parent (GtkTreeModel *tree_model,
					 GtkTreeIter  *iter,
					 GtkTreeIter  *child)
{ 
  GncTreeModelAccountDrag *tree_model_drag = (GncTreeModelAccountDrag *) tree_model;

  g_return_val_if_fail (tree_model_drag->child_model != NULL, FALSE);

  return gtk_tree_model_iter_parent (tree_model_drag->child_model, iter, child);
}

static void
gnc_tree_model_account_drag_ref_node (GtkTreeModel *tree_model,
				      GtkTreeIter  *iter)
{
  GncTreeModelAccountDrag *tree_model_drag = (GncTreeModelAccountDrag *) tree_model;

  g_return_if_fail (tree_model_drag->child_model != NULL);

  gtk_tree_model_ref_node (tree_model_drag->child_model, iter);
}

static void
gnc_tree_model_account_drag_unref_node (GtkTreeModel *tree_model,
					GtkTreeIter  *iter)
{
  GncTreeModelAccountDrag *tree_model_drag = (GncTreeModelAccountDrag *) tree_model;

  g_return_if_fail (tree_model_drag->child_model != NULL);

  gtk_tree_model_unref_node (tree_model_drag->child_model, iter);
}

/* Sortable interface */
static gboolean
gnc_tree_model_account_drag_get_sort_column_id (GtkTreeSortable *sortable,
						gint            *sort_column_id,
						GtkSortType     *order)
{
  GncTreeModelAccountDrag *tree_model_drag = (GncTreeModelAccountDrag *) sortable;

  g_return_val_if_fail (tree_model_drag->child_model != NULL, FALSE);

  return gtk_tree_sortable_get_sort_column_id(GTK_TREE_SORTABLE(tree_model_drag->child_model), sort_column_id, order);
}

static void
gnc_tree_model_account_drag_set_sort_column_id (GtkTreeSortable *sortable,
						gint             sort_column_id,
						GtkSortType      order)
{
  GncTreeModelAccountDrag *tree_model_drag = (GncTreeModelAccountDrag *) sortable;

  g_return_if_fail (tree_model_drag->child_model != NULL);

  gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(tree_model_drag->child_model), sort_column_id, order);
}

static void
gnc_tree_model_account_drag_set_sort_func (GtkTreeSortable        *sortable,
					   gint                    sort_column_id,
					   GtkTreeIterCompareFunc  func,
					   gpointer                data,
					   GDestroyNotify          destroy)
{
  GncTreeModelAccountDrag *tree_model_drag = (GncTreeModelAccountDrag *) sortable;

  g_return_if_fail (tree_model_drag->child_model != NULL);

  gtk_tree_sortable_set_sort_func(GTK_TREE_SORTABLE(tree_model_drag->child_model), sort_column_id, func, data, destroy);
}

static void
gnc_tree_model_account_drag_set_default_sort_func (GtkTreeSortable        *sortable,
						   GtkTreeIterCompareFunc  func,
						   gpointer                data,
						   GDestroyNotify          destroy)
{
  GncTreeModelAccountDrag *tree_model_drag = (GncTreeModelAccountDrag *) sortable;

  g_return_if_fail (tree_model_drag->child_model != NULL);

  gtk_tree_sortable_set_default_sort_func(GTK_TREE_SORTABLE(tree_model_drag->child_model), func, data, destroy);
}

static gboolean
gnc_tree_model_account_drag_has_default_sort_func (GtkTreeSortable *sortable)
{
  GncTreeModelAccountDrag *tree_model_drag = (GncTreeModelAccountDrag *) sortable;

  g_return_val_if_fail (tree_model_drag->child_model != NULL, FALSE);

  return gtk_tree_sortable_has_default_sort_func(GTK_TREE_SORTABLE(tree_model_drag->child_model));
}

/** GtkTreeDragSource implementation methods ****************************/

static gboolean
gnc_tree_model_account_drag_row_draggable(GtkTreeDragSource *drag_source,
					  GtkTreePath *path) 
{
  DEBUG("Inside gnc_tree_model_account_drag_source_row_draggable\n");
  return TRUE;
}

static gboolean
gnc_tree_model_account_drag_drag_data_get(GtkTreeDragSource *drag_source,
					  GtkTreePath *path,
					  GtkSelectionData *selection_data)
{
  Account *account;
  gchar *full_account_name;
  GncTreeModelAccountDrag* account_drag;

  g_return_val_if_fail(GNC_IS_TREE_MODEL_ACCOUNT_DRAG(drag_source), FALSE);
  account_drag = GNC_TREE_MODEL_ACCOUNT_DRAG(drag_source);
  
  account = account_drag->lookup_func(path, account_drag->user_data);
  if (account == NULL)
    return FALSE;

  full_account_name = gnc_account_get_full_name(account);
  
  gtk_selection_data_set(selection_data, selection_data->target, 8, (const guchar *)full_account_name, strlen(full_account_name));
  DEBUG("Inside gnc_tree_model_account_drag_source_drag_data_get.  Target type \"%s\" desired.  Account name \"%s\" was selected.\n", gdk_atom_name(selection_data->target), full_account_name);

  g_free(full_account_name);

  return TRUE;
}

static gboolean 
gnc_tree_model_account_drag_drag_data_delete(GtkTreeDragSource *drag_source,
					     GtkTreePath *path)
{
  DEBUG("Inside gnc_tree_model_account_drag_source_drag_data_delete\n");
  return TRUE;
}

/** GtkTreeDragDest implementation methods ****************************/

static gboolean
gnc_tree_model_account_drag_account_is_child_of(Account *child, Account* possible_parent)
{
  Account* parent = child;

  while (parent != NULL)
    {
      if (parent == possible_parent)
        {
          return TRUE;
        }

      parent = gnc_account_get_parent(parent);
    }

  return FALSE;
}

static gboolean
gnc_tree_model_account_drag_drag_data_received(GtkTreeDragDest *drag_dest,
					       GtkTreePath *dest,
					       GtkSelectionData *selection_data)
{
  GtkTreePath *parent_path;
  Account *new_parent, *dragged_account;
  gchar *parent_full_account_name, *dragged_full_account_name;
  GncTreeModelAccountDrag* account_drag;

  DEBUG("Inside gnc_tree_model_account_drag_drag_data_received.\n");

  g_return_val_if_fail(GNC_IS_TREE_MODEL_ACCOUNT_DRAG(drag_dest), FALSE);
  account_drag = GNC_TREE_MODEL_ACCOUNT_DRAG(drag_dest);
  
  // The dest path is spec's as the element to insert before,
  // but the parent is what's really desired, so move up
  // the path to find the parent.
  parent_path = gtk_tree_path_copy(dest);
  g_return_val_if_fail(parent_path != NULL, FALSE);

  gtk_tree_path_up(parent_path);
  new_parent = account_drag->lookup_func(parent_path, account_drag->user_data);
  gtk_tree_path_free(parent_path);
  g_return_val_if_fail(new_parent != NULL, FALSE);

  parent_full_account_name = gnc_account_get_full_name(new_parent);
  DEBUG("Account name \"%s\" was drop target.\n", parent_full_account_name);
  g_free(parent_full_account_name);

  g_return_val_if_fail(g_utf8_validate((gchar*)selection_data->data, selection_data->length, NULL), FALSE);
  dragged_full_account_name = g_strndup((gchar*)selection_data->data, selection_data->length);
  DEBUG("Account name \"%s\" was dragged account.\n", dragged_full_account_name);

  dragged_account = gnc_account_lookup_by_full_name(new_parent, dragged_full_account_name);
  g_free(dragged_full_account_name);
  g_return_val_if_fail(dragged_account != NULL, FALSE);

  // Make sure that the new hierarchy would still be valid.  Accounts cannot be children
  // of themselves
  if (gnc_tree_model_account_drag_account_is_child_of(new_parent, dragged_account)) {
    GtkWindow *window;
    GtkWidget *dialog;

    window = account_drag->widget == NULL ? NULL : GTK_WINDOW (gtk_widget_get_toplevel (account_drag->widget));

    dialog = gtk_message_dialog_new (window,
                                     GTK_DIALOG_DESTROY_WITH_PARENT,
                                     GTK_MESSAGE_ERROR,
                                     GTK_BUTTONS_CLOSE,
                                     _("You cannot move an account into itself."));
    gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG(dialog),
                                              _("The destination account is a child of the dragged account."));
    gtk_dialog_run (GTK_DIALOG (dialog));
    gtk_widget_destroy (dialog);

    return FALSE;
  }

  // Make sure the parent account has a valid type for the new child
  if (xaccAccountTypesCompatible(xaccAccountGetType(new_parent), xaccAccountGetType(dragged_account)) == FALSE) {
    GtkWindow *window;
    GtkWidget *dialog;

    window = account_drag->widget == NULL ? NULL : GTK_WINDOW (gtk_widget_get_toplevel (account_drag->widget));

    dialog = gtk_message_dialog_new (window,
                                     GTK_DIALOG_DESTROY_WITH_PARENT,
                                     GTK_MESSAGE_ERROR,
                                     GTK_BUTTONS_CLOSE,
                                     _("The account cannot be moved because the account types are incompatible."));
    gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG(dialog),
                                              _("The destination account type, %s, is not compatible with the new parent's type, %s."),
                                              xaccAccountGetTypeStr(xaccAccountGetType(dragged_account)),
                                              xaccAccountGetTypeStr(xaccAccountGetType(new_parent)));
    gtk_dialog_run (GTK_DIALOG (dialog));
    gtk_widget_destroy (dialog);

    return FALSE;
  }

  gnc_suspend_gui_refresh ();
  gnc_account_append_child (new_parent, dragged_account);
  gnc_resume_gui_refresh ();

  return TRUE;
}

static gboolean
gnc_tree_model_account_drag_row_drop_possible(GtkTreeDragDest *drag_dest,
                                              GtkTreePath *dest,
                                              GtkSelectionData *selection_data)
{
  DEBUG("Inside gnc_tree_model_account_drag_dest_row_drop_possible.");
  return TRUE;
}
