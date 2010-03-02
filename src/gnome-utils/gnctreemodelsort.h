/* Note: This file was copied from gtk-2.8.12 gtktreemodelsort.h with the
following changes:

s/GTK_TREE_MODEL_SORT/GNC_TREE_MODEL_SORT/g
s/GTK_IS_TREE_MODEL_SORT/GNC_IS_TREE_MODEL_SORT/g
s/GTK_TYPE_TREE_MODEL_SORT/GNC_TYPE_TREE_MODEL_SORT/g
s/GtkTreeModelSort/GncTreeModelSort/g
s/gtk_tree_model_sort/gnc_tree_model_sort/g

*/

/* gtktreemodelsort.h
 * Copyright (C) 2000  Red Hat, Inc.,  Jonathan Blandford <jrb@redhat.com>
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

#ifndef __GNC_TREE_MODEL_SORT_H__
#define __GNC_TREE_MODEL_SORT_H__

#include <gtk/gtktreemodel.h>
#include <gtk/gtktreesortable.h>

G_BEGIN_DECLS

#define GNC_TYPE_TREE_MODEL_SORT			(gnc_tree_model_sort_get_type ())
#define GNC_TREE_MODEL_SORT(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_TREE_MODEL_SORT, GncTreeModelSort))
#define GNC_TREE_MODEL_SORT_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_TREE_MODEL_SORT, GncTreeModelSortClass))
#define GNC_IS_TREE_MODEL_SORT(obj)			(G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_TREE_MODEL_SORT))
#define GNC_IS_TREE_MODEL_SORT_CLASS(klass)		(G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_TREE_MODEL_SORT))
#define GNC_TREE_MODEL_SORT_GET_CLASS(obj)		(G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_TREE_MODEL_SORT, GncTreeModelSortClass))

typedef struct _GncTreeModelSort       GncTreeModelSort;
typedef struct _GncTreeModelSortClass  GncTreeModelSortClass;

struct _GncTreeModelSort
{
    GObject parent;

    /* < private > */
    gpointer root;
    gint stamp;
    guint child_flags;
    GtkTreeModel *child_model;
    gint zero_ref_count;

    /* sort information */
    GList *sort_list;
    gint sort_column_id;
    GtkSortType order;

    /* default sort */
    GtkTreeIterCompareFunc default_sort_func;
    gpointer default_sort_data;
    GtkDestroyNotify default_sort_destroy;

    /* signal ids */
    guint changed_id;
    guint inserted_id;
    guint has_child_toggled_id;
    guint deleted_id;
    guint reordered_id;
};

struct _GncTreeModelSortClass
{
    GObjectClass parent_class;

    /* Padding for future expansion */
    void (*_gtk_reserved1) (void);
    void (*_gtk_reserved2) (void);
    void (*_gtk_reserved3) (void);
    void (*_gtk_reserved4) (void);
};


GType         gnc_tree_model_sort_get_type                   (void) G_GNUC_CONST;
GtkTreeModel *gnc_tree_model_sort_new_with_model             (GtkTreeModel     *child_model);

GtkTreeModel *gnc_tree_model_sort_get_model                  (GncTreeModelSort *tree_model);
GtkTreePath  *gnc_tree_model_sort_convert_child_path_to_path (GncTreeModelSort *tree_model_sort,
        GtkTreePath      *child_path);
void          gnc_tree_model_sort_convert_child_iter_to_iter (GncTreeModelSort *tree_model_sort,
        GtkTreeIter      *sort_iter,
        GtkTreeIter      *child_iter);
GtkTreePath  *gnc_tree_model_sort_convert_path_to_child_path (GncTreeModelSort *tree_model_sort,
        GtkTreePath      *sorted_path);
void          gnc_tree_model_sort_convert_iter_to_child_iter (GncTreeModelSort *tree_model_sort,
        GtkTreeIter      *child_iter,
        GtkTreeIter      *sorted_iter);
void          gnc_tree_model_sort_reset_default_sort_func    (GncTreeModelSort *tree_model_sort);
void          gnc_tree_model_sort_clear_cache                (GncTreeModelSort *tree_model_sort);
gboolean      gnc_tree_model_sort_iter_is_valid              (GncTreeModelSort *tree_model_sort,
        GtkTreeIter      *iter);


G_END_DECLS

#endif /* __GNC_TREE_MODEL_SORT_H__ */
