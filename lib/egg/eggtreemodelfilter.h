/* eggtreemodelfilter.h
 * Copyright (C) 2000,2001  Red Hat, Inc., Jonathan Blandford <jrb@redhat.com>
 * Copyright (C) 2001,2002  Kristian Rietveld <kris@gtk.org>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#ifndef __EGG_TREE_MODEL_FILTER_H__
#define __EGG_TREE_MODEL_FILTER_H__

#include <gtk/gtktreemodel.h>

G_BEGIN_DECLS

#define EGG_TYPE_TREE_MODEL_FILTER              (egg_tree_model_filter_get_type ())
#define EGG_TREE_MODEL_FILTER(obj)              (G_TYPE_CHECK_INSTANCE_CAST ((obj), EGG_TYPE_TREE_MODEL_FILTER, EggTreeModelFilter))
#define EGG_TREE_MODEL_FILTER_CLASS(vtable)     (G_TYPE_CHECK_CLASS_CAST ((vtable), EGG_TYPE_TREE_MODEL_FILTER, EggTreeModelFilterClass))
#define EGG_IS_TREE_MODEL_FILTER(obj)           (G_TYPE_CHECK_INSTANCE_TYPE ((obj), EGG_TYPE_TREE_MODEL_FILTER))
#define EGG_IS_TREE_MODEL_FILTER_CLASS(vtable)  (G_TYPE_CHECK_CLASS_TYPE ((vtable), EGG_TYPE_TREE_MODEL_FILTER))
#define EGG_TREE_MODEL_FILTER_GET_CLASS(inst)   (G_TYPE_INSTANCE_GET_CLASS ((obj), EGG_TYPE_TREE_MODEL_FILTER, EggTreeModelFilterClass))

typedef gboolean (* EggTreeModelFilterVisibleFunc) (GtkTreeModel *model,
						    GtkTreeIter  *iter,
						    gpointer      data);
typedef void (* EggTreeModelFilterModifyFunc) (GtkTreeModel *model,
					       GtkTreeIter  *iter,
					       GValue       *value,
					       gint          column,
					       gpointer      data);

typedef struct _EggTreeModelFilter		EggTreeModelFilter;
typedef struct _EggTreeModelFilterClass		EggTreeModelFilterClass;

struct _EggTreeModelFilter
{
  GObject parent;

  /*< private >*/
  gpointer root;
  gint stamp;
  guint child_flags;
  GtkTreeModel *child_model;
  gint zero_ref_count;

  guint root_level_visible;

  GtkTreePath *virtual_root;

  EggTreeModelFilterVisibleFunc visible_func;
  gpointer visible_data;
  GtkDestroyNotify visible_destroy;

  gint modify_n_columns;
  GType *modify_types;
  EggTreeModelFilterModifyFunc modify_func;
  gpointer modify_data;
  gpointer modify_destroy;

  gint visible_column;

  gboolean visible_method_set;
  gboolean modify_func_set;

  /* signal ids */
  guint changed_id;
  guint inserted_id;
  guint has_child_toggled_id;
  guint deleted_id;
  guint reordered_id;
};

struct _EggTreeModelFilterClass
{
  GObjectClass parent_class;
};

GType   egg_tree_model_filter_get_type			(void);
GtkTreeModel *egg_tree_model_filter_new			(GtkTreeModel                 *child_model,
							 GtkTreePath                  *root);
void	egg_tree_model_filter_set_visible_func		(EggTreeModelFilter           *filter,
							 EggTreeModelFilterVisibleFunc func,
							 gpointer                      data,
							 GtkDestroyNotify              destroy);
void	egg_tree_model_filter_set_modify_func           (EggTreeModelFilter           *filter,
							 gint                          n_columns,
							 GType                        *types,
							 EggTreeModelFilterModifyFunc  func,
							 gpointer                      data,
							 GtkDestroyNotify              destroy);
void	egg_tree_model_filter_set_visible_column	(EggTreeModelFilter           *filter,
							 gint                          column);

GtkTreeModel *egg_tree_model_filter_get_model           (EggTreeModelFilter           *filter);

/* conversion */
void	egg_tree_model_filter_convert_child_iter_to_iter	(EggTreeModelFilter *filter,
								 GtkTreeIter        *filter_iter,
								 GtkTreeIter        *child_iter);
void	egg_tree_model_filter_convert_iter_to_child_iter	(EggTreeModelFilter *filter,
								 GtkTreeIter        *child_iter,
								 GtkTreeIter        *filter_iter);
GtkTreePath *egg_tree_model_filter_convert_child_path_to_path	(EggTreeModelFilter *filter,
								 GtkTreePath        *child_path);
GtkTreePath *egg_tree_model_filter_convert_path_to_child_path	(EggTreeModelFilter *path,
								 GtkTreePath        *filter_path);

void    egg_tree_model_filter_refilter                  (EggTreeModelFilter           *filter);
void	egg_tree_model_filter_clear_cache		(EggTreeModelFilter           *filter);

G_END_DECLS

#endif /* __EGG_TREE_MODEL_FILTER_H__ */
