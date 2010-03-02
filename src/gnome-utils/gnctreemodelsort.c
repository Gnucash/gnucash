/* Note: This file was copied from gtk-2.8.12 gtktreemodelsort.c with the
following changes:
s/GTK_TREE_MODEL_SORT/GNC_TREE_MODEL_SORT/g
s/GTK_IS_TREE_MODEL_SORT/GNC_IS_TREE_MODEL_SORT/g
s/GtkTreeModelSort/GncTreeModelSort/g
s/gtk_tree_model_sort/gnc_tree_model_sort/gi
Plus some minor changes that have been annotated with 'CAS:'
*/

/* gtktreemodelsort.c
 * Copyright (C) 2000,2001  Red Hat, Inc.,  Jonathan Blandford <jrb@redhat.com>
 * Copyright (C) 2001,2002  Kristian Rietveld <kris@gtk.org>
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

/* NOTE: There is a potential for confusion in this code as to whether an iter,
 * path or value refers to the GncTreeModelSort model, or the child model being
 * sorted.  As a convention, variables referencing the child model will have an
 * s_ prefix before them (ie. s_iter, s_value, s_path);
 */

/* ITER FORMAT:
 *
 * iter->stamp = tree_model_sort->stamp
 * iter->user_data = SortLevel
 * iter->user_data2 = SortElt
 */

/* WARNING: this code is dangerous, can cause sleepless nights,
 * can cause your dog to die among other bad things
 *
 * we warned you and we're not liable for any head injuries.
 */

#include <config.h>
#include <string.h>

#include "gnctreemodelsort.h"    //CAS: header name changed
#include "gtk/gtktreesortable.h" //CAS: path changed
#include "gtk/gtktreestore.h"    //CAS: path changed
#include "gtktreedatalist.h"
//#include "gtk/gtkintl.h"       //CAS: header commented
#include "gtk/gtkprivate.h"      //CAS: path changed
#include "gtk/gtktreednd.h"      //CAS: path changed
//#include "gtk/gtkalias.h"      //CAS: header commented
#include <glib/gi18n.h> //CAS: header added
#define P_(x) _(x)      //CAS: macro added

typedef struct _SortElt SortElt;
typedef struct _SortLevel SortLevel;
typedef struct _SortData SortData;
typedef struct _SortTuple SortTuple;

struct _SortElt
{
    GtkTreeIter  iter;
    SortLevel   *children;
    gint         offset;
    gint         ref_count;
    gint         zero_ref_count;
};

struct _SortLevel
{
    GArray    *array;
    gint       ref_count;
    SortElt   *parent_elt;
    SortLevel *parent_level;
};

struct _SortData
{
    GncTreeModelSort *tree_model_sort;
    GtkTreePath *parent_path;
    gint parent_path_depth;
    gint *parent_path_indices;
    GtkTreeIterCompareFunc sort_func;
    gpointer sort_data;
};

struct _SortTuple
{
    SortElt   *elt;
    gint       offset;
};

/* Properties */
enum
{
    PROP_0,
    /* Construct args */
    PROP_MODEL
};



#define GNC_TREE_MODEL_SORT_CACHE_CHILD_ITERS(tree_model_sort) \
	(((GncTreeModelSort *)tree_model_sort)->child_flags&GTK_TREE_MODEL_ITERS_PERSIST)
#define SORT_ELT(sort_elt) ((SortElt *)sort_elt)
#define SORT_LEVEL(sort_level) ((SortLevel *)sort_level)

#define GET_CHILD_ITER(tree_model_sort,ch_iter,so_iter) gnc_tree_model_sort_convert_iter_to_child_iter(GNC_TREE_MODEL_SORT (tree_model_sort), ch_iter, so_iter);

#define NO_SORT_FUNC ((GtkTreeIterCompareFunc) 0x1)

#define VALID_ITER(iter, tree_model_sort) (iter != NULL && iter->user_data != NULL && iter->user_data2 != NULL && tree_model_sort->stamp == iter->stamp)

/* general (object/interface init, etc) */
static void gnc_tree_model_sort_init                  (GncTreeModelSort      *tree_model_sort);
static void gnc_tree_model_sort_class_init            (GncTreeModelSortClass *tree_model_sort_class);
static void gnc_tree_model_sort_tree_model_init       (GtkTreeModelIface     *iface);
static void gnc_tree_model_sort_tree_sortable_init    (GtkTreeSortableIface  *iface);
static void gnc_tree_model_sort_drag_source_init      (GtkTreeDragSourceIface*iface);
static void gnc_tree_model_sort_finalize              (GObject               *object);
static void gnc_tree_model_sort_set_property          (GObject               *object,
        guint                  prop_id,
        const GValue          *value,
        GParamSpec            *pspec);
static void gnc_tree_model_sort_get_property          (GObject               *object,
        guint                  prop_id,
        GValue                *value,
        GParamSpec            *pspec);

/* our signal handlers */
static void gnc_tree_model_sort_row_changed           (GtkTreeModel          *model,
        GtkTreePath           *start_path,
        GtkTreeIter           *start_iter,
        gpointer               data);
static void gnc_tree_model_sort_row_inserted          (GtkTreeModel          *model,
        GtkTreePath           *path,
        GtkTreeIter           *iter,
        gpointer               data);
static void gnc_tree_model_sort_row_has_child_toggled (GtkTreeModel          *model,
        GtkTreePath           *path,
        GtkTreeIter           *iter,
        gpointer               data);
static void gnc_tree_model_sort_row_deleted           (GtkTreeModel          *model,
        GtkTreePath           *path,
        gpointer               data);
static void gnc_tree_model_sort_rows_reordered        (GtkTreeModel          *s_model,
        GtkTreePath           *s_path,
        GtkTreeIter           *s_iter,
        gint                  *new_order,
        gpointer               data);

/* TreeModel interface */
static GtkTreeModelFlags gnc_tree_model_sort_get_flags     (GtkTreeModel          *tree_model);
static gint         gnc_tree_model_sort_get_n_columns      (GtkTreeModel          *tree_model);
static GType        gnc_tree_model_sort_get_column_type    (GtkTreeModel          *tree_model,
        gint                   index);
static gboolean     gnc_tree_model_sort_get_iter           (GtkTreeModel          *tree_model,
        GtkTreeIter           *iter,
        GtkTreePath           *path);
static GtkTreePath *gnc_tree_model_sort_get_path           (GtkTreeModel          *tree_model,
        GtkTreeIter           *iter);
static void         gnc_tree_model_sort_get_value          (GtkTreeModel          *tree_model,
        GtkTreeIter           *iter,
        gint                   column,
        GValue                *value);
static gboolean     gnc_tree_model_sort_iter_next          (GtkTreeModel          *tree_model,
        GtkTreeIter           *iter);
static gboolean     gnc_tree_model_sort_iter_children      (GtkTreeModel          *tree_model,
        GtkTreeIter           *iter,
        GtkTreeIter           *parent);
static gboolean     gnc_tree_model_sort_iter_has_child     (GtkTreeModel          *tree_model,
        GtkTreeIter           *iter);
static gint         gnc_tree_model_sort_iter_n_children    (GtkTreeModel          *tree_model,
        GtkTreeIter           *iter);
static gboolean     gnc_tree_model_sort_iter_nth_child     (GtkTreeModel          *tree_model,
        GtkTreeIter           *iter,
        GtkTreeIter           *parent,
        gint                   n);
static gboolean     gnc_tree_model_sort_iter_parent        (GtkTreeModel          *tree_model,
        GtkTreeIter           *iter,
        GtkTreeIter           *child);
static void         gnc_tree_model_sort_ref_node           (GtkTreeModel          *tree_model,
        GtkTreeIter           *iter);
static void         gnc_tree_model_sort_real_unref_node    (GtkTreeModel          *tree_model,
        GtkTreeIter           *iter,
        gboolean               propagate_unref);
static void         gnc_tree_model_sort_unref_node         (GtkTreeModel          *tree_model,
        GtkTreeIter           *iter);

/* TreeDragSource interface */
static gboolean     gnc_tree_model_sort_row_draggable         (GtkTreeDragSource      *drag_source,
        GtkTreePath            *path);
static gboolean     gnc_tree_model_sort_drag_data_get         (GtkTreeDragSource      *drag_source,
        GtkTreePath            *path,
        GtkSelectionData       *selection_data);
static gboolean     gnc_tree_model_sort_drag_data_delete      (GtkTreeDragSource      *drag_source,
        GtkTreePath            *path);

/* TreeSortable interface */
static gboolean     gnc_tree_model_sort_get_sort_column_id    (GtkTreeSortable        *sortable,
        gint                   *sort_column_id,
        GtkSortType            *order);
static void         gnc_tree_model_sort_set_sort_column_id    (GtkTreeSortable        *sortable,
        gint                    sort_column_id,
        GtkSortType        order);
static void         gnc_tree_model_sort_set_sort_func         (GtkTreeSortable        *sortable,
        gint                    sort_column_id,
        GtkTreeIterCompareFunc  func,
        gpointer                data,
        GtkDestroyNotify        destroy);
static void         gnc_tree_model_sort_set_default_sort_func (GtkTreeSortable        *sortable,
        GtkTreeIterCompareFunc  func,
        gpointer                data,
        GtkDestroyNotify        destroy);
static gboolean     gnc_tree_model_sort_has_default_sort_func (GtkTreeSortable     *sortable);

/* Private functions (sort funcs, level handling and other utils) */
static void         gnc_tree_model_sort_build_level       (GncTreeModelSort *tree_model_sort,
        SortLevel        *parent_level,
        SortElt          *parent_elt);
static void         gnc_tree_model_sort_free_level        (GncTreeModelSort *tree_model_sort,
        SortLevel        *sort_level);
static void         gnc_tree_model_sort_increment_stamp   (GncTreeModelSort *tree_model_sort);
static void         gnc_tree_model_sort_sort_level        (GncTreeModelSort *tree_model_sort,
        SortLevel        *level,
        gboolean          recurse,
        gboolean          emit_reordered);
static void         gnc_tree_model_sort_sort              (GncTreeModelSort *tree_model_sort);
static gint         gnc_tree_model_sort_level_find_insert (GncTreeModelSort *tree_model_sort,
        SortLevel        *level,
        GtkTreeIter      *iter,
        gint             skip_index);
static gboolean     gnc_tree_model_sort_insert_value      (GncTreeModelSort *tree_model_sort,
        SortLevel        *level,
        GtkTreePath      *s_path,
        GtkTreeIter      *s_iter);
static GtkTreePath *gnc_tree_model_sort_elt_get_path      (SortLevel        *level,
        SortElt          *elt);
static void         gnc_tree_model_sort_set_model         (GncTreeModelSort *tree_model_sort,
        GtkTreeModel     *child_model);
static GtkTreePath *gtk_real_tree_model_sort_convert_child_path_to_path (GncTreeModelSort *tree_model_sort,
        GtkTreePath      *child_path,
        gboolean          build_levels);

static GObjectClass *parent_class = NULL;

GType
gnc_tree_model_sort_get_type (void)
{
    static GType tree_model_sort_type = 0;

    if (!tree_model_sort_type)
    {
        static const GTypeInfo tree_model_sort_info =
        {
            sizeof (GncTreeModelSortClass),
            NULL,           /* base_init */
            NULL,           /* base_finalize */
            (GClassInitFunc) gnc_tree_model_sort_class_init,
            NULL,           /* class_finalize */
            NULL,           /* class_data */
            sizeof (GncTreeModelSort),
            0,              /* n_preallocs */
            (GInstanceInitFunc) gnc_tree_model_sort_init
        };

        static const GInterfaceInfo tree_model_info =
        {
            (GInterfaceInitFunc) gnc_tree_model_sort_tree_model_init,
            NULL,
            NULL
        };

        static const GInterfaceInfo sortable_info =
        {
            (GInterfaceInitFunc) gnc_tree_model_sort_tree_sortable_init,
            NULL,
            NULL
        };

        static const GInterfaceInfo drag_source_info =
        {
            (GInterfaceInitFunc) gnc_tree_model_sort_drag_source_init,
            NULL,
            NULL
        };

        tree_model_sort_type =
            g_type_register_static (G_TYPE_OBJECT, "GncTreeModelSort",
                                    &tree_model_sort_info, 0);

        g_type_add_interface_static (tree_model_sort_type,
                                     GTK_TYPE_TREE_MODEL,
                                     &tree_model_info);

        g_type_add_interface_static (tree_model_sort_type,
                                     GTK_TYPE_TREE_SORTABLE,
                                     &sortable_info);

        g_type_add_interface_static (tree_model_sort_type,
                                     GTK_TYPE_TREE_DRAG_SOURCE,
                                     &drag_source_info);
    }

    return tree_model_sort_type;
}

static void
gnc_tree_model_sort_init (GncTreeModelSort *tree_model_sort)
{
    tree_model_sort->sort_column_id = GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID;
    tree_model_sort->stamp = 0;
    tree_model_sort->zero_ref_count = 0;
    tree_model_sort->root = NULL;
    tree_model_sort->sort_list = NULL;
}

static void
gnc_tree_model_sort_class_init (GncTreeModelSortClass *class)
{
    GObjectClass *object_class;

    object_class = (GObjectClass *) class;
    parent_class = g_type_class_peek_parent (class);

    object_class->set_property = gnc_tree_model_sort_set_property;
    object_class->get_property = gnc_tree_model_sort_get_property;

    object_class->finalize = gnc_tree_model_sort_finalize;

    /* Properties */
    g_object_class_install_property (object_class,
                                     PROP_MODEL,
                                     g_param_spec_object ("model",
                                             P_("TreeModelSort Model"),
                                             P_("The model for the TreeModelSort to sort"),
                                             GTK_TYPE_TREE_MODEL,
                                             G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));
}

static void
gnc_tree_model_sort_tree_model_init (GtkTreeModelIface *iface)
{
    iface->get_flags = gnc_tree_model_sort_get_flags;
    iface->get_n_columns = gnc_tree_model_sort_get_n_columns;
    iface->get_column_type = gnc_tree_model_sort_get_column_type;
    iface->get_iter = gnc_tree_model_sort_get_iter;
    iface->get_path = gnc_tree_model_sort_get_path;
    iface->get_value = gnc_tree_model_sort_get_value;
    iface->iter_next = gnc_tree_model_sort_iter_next;
    iface->iter_children = gnc_tree_model_sort_iter_children;
    iface->iter_has_child = gnc_tree_model_sort_iter_has_child;
    iface->iter_n_children = gnc_tree_model_sort_iter_n_children;
    iface->iter_nth_child = gnc_tree_model_sort_iter_nth_child;
    iface->iter_parent = gnc_tree_model_sort_iter_parent;
    iface->ref_node = gnc_tree_model_sort_ref_node;
    iface->unref_node = gnc_tree_model_sort_unref_node;
}

static void
gnc_tree_model_sort_tree_sortable_init (GtkTreeSortableIface *iface)
{
    iface->get_sort_column_id = gnc_tree_model_sort_get_sort_column_id;
    iface->set_sort_column_id = gnc_tree_model_sort_set_sort_column_id;
    iface->set_sort_func = gnc_tree_model_sort_set_sort_func;
    iface->set_default_sort_func = gnc_tree_model_sort_set_default_sort_func;
    iface->has_default_sort_func = gnc_tree_model_sort_has_default_sort_func;
}

static void
gnc_tree_model_sort_drag_source_init (GtkTreeDragSourceIface *iface)
{
    iface->row_draggable = gnc_tree_model_sort_row_draggable;
    iface->drag_data_delete = gnc_tree_model_sort_drag_data_delete;
    iface->drag_data_get = gnc_tree_model_sort_drag_data_get;
}

/**
 * gnc_tree_model_sort_new_with_model:
 * @child_model: A #GtkTreeModel
 *
 * Creates a new #GtkTreeModel, with @child_model as the child model.
 *
 * Return value: A new #GtkTreeModel.
 */
GtkTreeModel *
gnc_tree_model_sort_new_with_model (GtkTreeModel      *child_model)
{
    GtkTreeModel *retval;

    g_return_val_if_fail (GTK_IS_TREE_MODEL (child_model), NULL);

    retval = g_object_new (gnc_tree_model_sort_get_type (), NULL);

    gnc_tree_model_sort_set_model (GNC_TREE_MODEL_SORT (retval), child_model);

    return retval;
}

/* GObject callbacks */
static void
gnc_tree_model_sort_finalize (GObject *object)
{
    GncTreeModelSort *tree_model_sort = (GncTreeModelSort *) object;

    gnc_tree_model_sort_set_model (tree_model_sort, NULL);

    if (tree_model_sort->root)
        gnc_tree_model_sort_free_level (tree_model_sort, tree_model_sort->root);

    if (tree_model_sort->sort_list)
    {
        _gtk_tree_data_list_header_free (tree_model_sort->sort_list);
        tree_model_sort->sort_list = NULL;
    }

    /* must chain up */
    parent_class->finalize (object);
}

static void
gnc_tree_model_sort_set_property (GObject      *object,
                                  guint         prop_id,
                                  const GValue *value,
                                  GParamSpec   *pspec)
{
    GncTreeModelSort *tree_model_sort = GNC_TREE_MODEL_SORT (object);

    switch (prop_id)
    {
    case PROP_MODEL:
        gnc_tree_model_sort_set_model (tree_model_sort, g_value_get_object (value));
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
        break;
    }
}

static void
gnc_tree_model_sort_get_property (GObject    *object,
                                  guint       prop_id,
                                  GValue     *value,
                                  GParamSpec *pspec)
{
    GncTreeModelSort *tree_model_sort = GNC_TREE_MODEL_SORT (object);

    switch (prop_id)
    {
    case PROP_MODEL:
        g_value_set_object (value, gnc_tree_model_sort_get_model(tree_model_sort));
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
        break;
    }
}

static void
gnc_tree_model_sort_row_changed (GtkTreeModel *s_model,
                                 GtkTreePath  *start_s_path,
                                 GtkTreeIter  *start_s_iter,
                                 gpointer      data)
{
    GncTreeModelSort *tree_model_sort = GNC_TREE_MODEL_SORT (data);
    GtkTreePath *path = NULL;
    GtkTreeIter iter;
    GtkTreeIter tmpiter;

    SortElt tmp;
    SortElt *elt;
    SortLevel *level;

    gboolean free_s_path = FALSE;

    gint index = 0, old_index, i;

    g_return_if_fail (start_s_path != NULL || start_s_iter != NULL);

    if (!start_s_path)
    {
        free_s_path = TRUE;
        start_s_path = gtk_tree_model_get_path (s_model, start_s_iter);
    }

    path = gtk_real_tree_model_sort_convert_child_path_to_path (tree_model_sort,
            start_s_path,
            FALSE);
    if (!path)
    {
        if (free_s_path)
            gtk_tree_path_free (start_s_path);
        return;
    }

    gtk_tree_model_get_iter (GTK_TREE_MODEL (data), &iter, path);

    level = iter.user_data;
    elt = iter.user_data2;

    level->ref_count++;

    if (level->array->len < 2 ||
            (tree_model_sort->sort_column_id == GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID &&
             tree_model_sort->default_sort_func == NO_SORT_FUNC))
    {
        if (free_s_path)
            gtk_tree_path_free (start_s_path);

        gtk_tree_model_row_changed (GTK_TREE_MODEL (data), path, &iter);

        gtk_tree_path_free (path);

        level->ref_count--;

        return;
    }

    if (!GNC_TREE_MODEL_SORT_CACHE_CHILD_ITERS (tree_model_sort))
    {
        gtk_tree_model_get_iter (tree_model_sort->child_model,
                                 &tmpiter, start_s_path);
    }

    old_index = elt - SORT_ELT (level->array->data);

    memcpy (&tmp, elt, sizeof (SortElt));

    if (GNC_TREE_MODEL_SORT_CACHE_CHILD_ITERS (tree_model_sort))
        index = gnc_tree_model_sort_level_find_insert (tree_model_sort,
                level,
                &tmp.iter,
                old_index);
    else
        index = gnc_tree_model_sort_level_find_insert (tree_model_sort,
                level,
                &tmpiter,
                old_index);

    if (index < old_index)
    {
        g_memmove (level->array->data + ((index + 1)*sizeof (SortElt)),
                   level->array->data + ((index)*sizeof (SortElt)),
                   (old_index - index)* sizeof(SortElt));
    }
    else if (index > old_index)
    {
        g_memmove (level->array->data + ((old_index)*sizeof (SortElt)),
                   level->array->data + ((old_index + 1)*sizeof (SortElt)),
                   (index - old_index)* sizeof(SortElt));
    }
    memcpy (level->array->data + ((index)*sizeof (SortElt)),
            &tmp, sizeof (SortElt));

    for (i = 0; i < level->array->len; i++)
        if (g_array_index (level->array, SortElt, i).children)
            g_array_index (level->array, SortElt, i).children->parent_elt = &g_array_index (level->array, SortElt, i);

    gtk_tree_path_up (path);
    gtk_tree_path_append_index (path, index);

    gnc_tree_model_sort_increment_stamp (tree_model_sort);

    /* if the item moved, then emit rows_reordered */
    if (old_index != index)
    {
        gint *new_order;
        gint j;

        GtkTreePath *tmppath;

        new_order = g_new (gint, level->array->len);

        for (j = 0; j < level->array->len; j++)
        {
            if (index > old_index)
            {
                if (j == index)
                    new_order[j] = old_index;
                else if (j >= old_index && j < index)
                    new_order[j] = j + 1;
                else
                    new_order[j] = j;
            }
            else if (index < old_index)
            {
                if (j == index)
                    new_order[j] = old_index;
                else if (j > index && j <= old_index)
                    new_order[j] = j - 1;
                else
                    new_order[j] = j;
            }
            /* else? shouldn't really happen */
        }

        if (level->parent_elt)
        {
            iter.stamp = tree_model_sort->stamp;
            iter.user_data = level->parent_level;
            iter.user_data2 = level->parent_elt;

            tmppath = gtk_tree_model_get_path (GTK_TREE_MODEL (tree_model_sort), &iter);

            gtk_tree_model_rows_reordered (GTK_TREE_MODEL (tree_model_sort),
                                           tmppath, &iter, new_order);
        }
        else
        {
            /* toplevel */
            tmppath = gtk_tree_path_new ();

            gtk_tree_model_rows_reordered (GTK_TREE_MODEL (tree_model_sort), tmppath,
                                           NULL, new_order);
        }

        gtk_tree_path_free (tmppath);
        g_free (new_order);
    }

    level->ref_count--;

    /* emit row_changed signal (at new location) */
    gtk_tree_model_get_iter (GTK_TREE_MODEL (data), &iter, path);
    gtk_tree_model_row_changed (GTK_TREE_MODEL (data), path, &iter);

    gtk_tree_path_free (path);
    if (free_s_path)
        gtk_tree_path_free (start_s_path);
}

static void
gnc_tree_model_sort_row_inserted (GtkTreeModel          *s_model,
                                  GtkTreePath           *s_path,
                                  GtkTreeIter           *s_iter,
                                  gpointer               data)
{
    GncTreeModelSort *tree_model_sort = GNC_TREE_MODEL_SORT (data);
    GtkTreePath *path;
    GtkTreeIter iter;
    GtkTreeIter real_s_iter;

    gint i = 0;

    gboolean free_s_path = FALSE;

    SortElt *elt;
    SortLevel *level;
    SortLevel *parent_level = NULL;

    parent_level = level = SORT_LEVEL (tree_model_sort->root);

    g_return_if_fail (s_path != NULL || s_iter != NULL);

    if (!s_path)
    {
        s_path = gtk_tree_model_get_path (s_model, s_iter);
        free_s_path = TRUE;
    }

    if (!s_iter)
        gtk_tree_model_get_iter (s_model, &real_s_iter, s_path);
    else
        real_s_iter = *s_iter;

    if (!tree_model_sort->root)
    {
        gnc_tree_model_sort_build_level (tree_model_sort, NULL, NULL);

        /* the build level already put the inserted iter in the level,
        	 so no need to handle this signal anymore */

        goto done_and_submit;
    }

    /* find the parent level */
    while (i < gtk_tree_path_get_depth (s_path) - 1)
    {
        gint j;

        if (!level)
        {
            /* level not yet build, we won't cover this signal */
            goto done;
        }

        if (level->array->len < gtk_tree_path_get_indices (s_path)[i])
        {
            g_warning ("A node was inserted with a parent that's not in the tree.\n"
                       "This possibly means that a GtkTreeModel inserted a child node\n"
                       "before the parent was inserted.");
            goto done;
        }

        elt = NULL;
        for (j = 0; j < level->array->len; j++)
            if (g_array_index (level->array, SortElt, j).offset == gtk_tree_path_get_indices (s_path)[i])
            {
                elt = &g_array_index (level->array, SortElt, j);
                break;
            }

        g_return_if_fail (elt != NULL);

        if (!elt->children)
        {
            GtkTreePath *tmppath;
            GtkTreeIter  tmpiter;

            tmpiter.stamp = tree_model_sort->stamp;
            tmpiter.user_data = level;
            tmpiter.user_data2 = elt;

            tmppath = gtk_tree_model_get_path (GTK_TREE_MODEL (data), &tmpiter);
            if (tmppath)
            {
                gtk_tree_model_row_has_child_toggled (GTK_TREE_MODEL (data),
                                                      tmppath,
                                                      &tmpiter);
                gtk_tree_path_free (tmppath);
            }

            /* not covering this signal */
            goto done;
        }

        level = elt->children;
        parent_level = level;
        i++;
    }

    if (!parent_level)
        goto done;

    if (level->ref_count == 0 && level != tree_model_sort->root)
    {
        gnc_tree_model_sort_free_level (tree_model_sort, level);
        goto done;
    }

    if (!gnc_tree_model_sort_insert_value (tree_model_sort,
                                           parent_level,
                                           s_path,
                                           &real_s_iter))
        goto done;

done_and_submit:
    path = gtk_real_tree_model_sort_convert_child_path_to_path (tree_model_sort,
            s_path,
            FALSE);

    if (!path)
        return;

    gnc_tree_model_sort_increment_stamp (tree_model_sort);

    gtk_tree_model_get_iter (GTK_TREE_MODEL (data), &iter, path);
    gtk_tree_model_row_inserted (GTK_TREE_MODEL (data), path, &iter);
    gtk_tree_path_free (path);

done:
    if (free_s_path)
        gtk_tree_path_free (s_path);

    return;
}

static void
gnc_tree_model_sort_row_has_child_toggled (GtkTreeModel *s_model,
        GtkTreePath  *s_path,
        GtkTreeIter  *s_iter,
        gpointer      data)
{
    GncTreeModelSort *tree_model_sort = GNC_TREE_MODEL_SORT (data);
    GtkTreePath *path;
    GtkTreeIter iter;

    g_return_if_fail (s_path != NULL && s_iter != NULL);

    path = gtk_real_tree_model_sort_convert_child_path_to_path (tree_model_sort, s_path, FALSE);
    if (path == NULL)
        return;

    gtk_tree_model_get_iter (GTK_TREE_MODEL (data), &iter, path);
    gtk_tree_model_row_has_child_toggled (GTK_TREE_MODEL (data), path, &iter);

    gtk_tree_path_free (path);
}

/* FIXME: I still have doubts if this works */
static void
gnc_tree_model_sort_row_deleted (GtkTreeModel *s_model,
                                 GtkTreePath  *s_path,
                                 gpointer      data)
{
    GncTreeModelSort *tree_model_sort = GNC_TREE_MODEL_SORT (data);
    GtkTreePath *path = NULL;
    SortElt *elt;
    SortLevel *level;
    GtkTreeIter iter;
    gint offset;
    gint i;

    g_return_if_fail (s_path != NULL);

    path = gtk_real_tree_model_sort_convert_child_path_to_path (tree_model_sort, s_path, FALSE);
    if (path == NULL)
        return;

    gtk_tree_model_get_iter (GTK_TREE_MODEL (data), &iter, path);

    level = SORT_LEVEL (iter.user_data);
    elt = SORT_ELT (iter.user_data2);
    offset = elt->offset;

    /* we _need_ to emit ::row_deleted before we start unreffing the node
     * itself. This is because of the row refs, which start unreffing nodes
     * when we emit ::row_deleted
     */
    gtk_tree_model_row_deleted (GTK_TREE_MODEL (data), path);

    gtk_tree_model_get_iter (GTK_TREE_MODEL (data), &iter, path);

    while (elt->ref_count > 0)
        gnc_tree_model_sort_real_unref_node (GTK_TREE_MODEL (data), &iter, FALSE);

    if (level->ref_count == 0)
    {
        /* This will prune the level, so I can just emit the signal and
         * not worry about cleaning this level up.
         * Careful, root level is not cleaned up in increment stamp.
         */
        gnc_tree_model_sort_increment_stamp (tree_model_sort);
        gtk_tree_path_free (path);
        if (level == tree_model_sort->root)
        {
            gnc_tree_model_sort_free_level (tree_model_sort,
                                            tree_model_sort->root);
            tree_model_sort->root = NULL;
        }
        return;
    }

    gnc_tree_model_sort_increment_stamp (tree_model_sort);

    /* Remove the row */
    for (i = 0; i < level->array->len; i++)
        if (elt->offset == g_array_index (level->array, SortElt, i).offset)
            break;

    g_array_remove_index (level->array, i);

    /* update all offsets */
    for (i = 0; i < level->array->len; i++)
    {
        elt = & (g_array_index (level->array, SortElt, i));
        if (elt->offset > offset)
            elt->offset--;
        if (elt->children)
            elt->children->parent_elt = elt;
    }

    gtk_tree_path_free (path);
}

static void
gnc_tree_model_sort_rows_reordered (GtkTreeModel *s_model,
                                    GtkTreePath  *s_path,
                                    GtkTreeIter  *s_iter,
                                    gint         *new_order,
                                    gpointer      data)
{
    SortElt *elt;
    SortLevel *level;
    GtkTreeIter iter;
    gint *tmp_array;
    int i, j;
    GtkTreePath *path;
    GncTreeModelSort *tree_model_sort = GNC_TREE_MODEL_SORT (data);

    g_return_if_fail (new_order != NULL);

    if (s_path == NULL || gtk_tree_path_get_indices (s_path) == NULL)
    {
        if (tree_model_sort->root == NULL)
            return;
        path = gtk_tree_path_new ();
        level = SORT_LEVEL (tree_model_sort->root);
    }
    else
    {
        path = gtk_real_tree_model_sort_convert_child_path_to_path (tree_model_sort, s_path, FALSE);
        if (path == NULL)
            return;
        gtk_tree_model_get_iter (GTK_TREE_MODEL (data), &iter, path);

        level = SORT_LEVEL (iter.user_data);
        elt = SORT_ELT (iter.user_data2);

        if (!elt->children)
        {
            gtk_tree_path_free (path);
            return;
        }

        level = elt->children;
    }

    if (level->array->len < 2)
    {
        gtk_tree_path_free (path);
        return;
    }

    tmp_array = g_new (int, level->array->len);
    for (i = 0; i < level->array->len; i++)
    {
        for (j = 0; j < level->array->len; j++)
        {
            if (g_array_index (level->array, SortElt, i).offset == new_order[j])
                tmp_array[i] = j;
        }
    }

    for (i = 0; i < level->array->len; i++)
        g_array_index (level->array, SortElt, i).offset = tmp_array[i];
    g_free (tmp_array);

    if (tree_model_sort->sort_column_id == GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID &&
            tree_model_sort->default_sort_func == NO_SORT_FUNC)
    {

        gnc_tree_model_sort_sort_level (tree_model_sort, level,
                                        FALSE, FALSE);
        gnc_tree_model_sort_increment_stamp (tree_model_sort);

        if (gtk_tree_path_get_depth (path))
        {
            gtk_tree_model_get_iter (GTK_TREE_MODEL (tree_model_sort),
                                     &iter,
                                     path);
            gtk_tree_model_rows_reordered (GTK_TREE_MODEL (tree_model_sort),
                                           path, &iter, new_order);
        }
        else
        {
            gtk_tree_model_rows_reordered (GTK_TREE_MODEL (tree_model_sort),
                                           path, NULL, new_order);
        }
    }

    gtk_tree_path_free (path);
}

/* Fulfill our model requirements */
static GtkTreeModelFlags
gnc_tree_model_sort_get_flags (GtkTreeModel *tree_model)
{
    GtkTreeModelFlags flags;

    g_return_val_if_fail (GNC_IS_TREE_MODEL_SORT (tree_model), 0);
    g_return_val_if_fail (GNC_TREE_MODEL_SORT (tree_model)->child_model != NULL, 0);

    flags = gtk_tree_model_get_flags (GNC_TREE_MODEL_SORT (tree_model)->child_model);

    if ((flags & GTK_TREE_MODEL_LIST_ONLY) == GTK_TREE_MODEL_LIST_ONLY)
        return GTK_TREE_MODEL_LIST_ONLY;

    return 0;
}

static gint
gnc_tree_model_sort_get_n_columns (GtkTreeModel *tree_model)
{
    GncTreeModelSort *tree_model_sort = (GncTreeModelSort *) tree_model;

    g_return_val_if_fail (GNC_IS_TREE_MODEL_SORT (tree_model), 0);

    if (tree_model_sort->child_model == 0)
        return 0;

    return gtk_tree_model_get_n_columns (tree_model_sort->child_model);
}

static GType
gnc_tree_model_sort_get_column_type (GtkTreeModel *tree_model,
                                     gint          index)
{
    g_return_val_if_fail (GNC_IS_TREE_MODEL_SORT (tree_model), G_TYPE_INVALID);
    g_return_val_if_fail (GNC_TREE_MODEL_SORT (tree_model)->child_model != NULL, G_TYPE_INVALID);

    return gtk_tree_model_get_column_type (GNC_TREE_MODEL_SORT (tree_model)->child_model, index);
}

static gboolean
gnc_tree_model_sort_get_iter (GtkTreeModel *tree_model,
                              GtkTreeIter  *iter,
                              GtkTreePath  *path)
{
    GncTreeModelSort *tree_model_sort;
    gint *indices;
    SortLevel *level;
    gint depth, i;

    g_return_val_if_fail (GNC_IS_TREE_MODEL_SORT (tree_model), FALSE);
    g_return_val_if_fail (GNC_TREE_MODEL_SORT (tree_model)->child_model != NULL, FALSE);

    tree_model_sort = (GncTreeModelSort *) tree_model;
    indices = gtk_tree_path_get_indices (path);

    if (tree_model_sort->root == NULL)
        gnc_tree_model_sort_build_level (tree_model_sort, NULL, NULL);
    level = SORT_LEVEL (tree_model_sort->root);

    depth = gtk_tree_path_get_depth (path);
    if (depth == 0)
        return FALSE;

    for (i = 0; i < depth - 1; i++)
    {
        if ((level == NULL) ||
                (indices[i] >= level->array->len))
            return FALSE;

        if (g_array_index (level->array, SortElt, indices[i]).children == NULL)
            gnc_tree_model_sort_build_level (tree_model_sort, level, &g_array_index (level->array, SortElt, indices[i]));
        level = g_array_index (level->array, SortElt, indices[i]).children;
    }

    if (!level || indices[i] >= level->array->len)
    {
        iter->stamp = 0;
        return FALSE;
    }

    iter->stamp = tree_model_sort->stamp;
    iter->user_data = level;
    iter->user_data2 = &g_array_index (level->array, SortElt, indices[depth - 1]);

    return TRUE;
}

static GtkTreePath *
gnc_tree_model_sort_get_path (GtkTreeModel *tree_model,
                              GtkTreeIter  *iter)
{
    GtkTreePath *retval;
    SortLevel *level;
    SortElt *elt;

    g_return_val_if_fail (GNC_IS_TREE_MODEL_SORT (tree_model), NULL);
    g_return_val_if_fail (GNC_TREE_MODEL_SORT (tree_model)->child_model != NULL, NULL);
    g_return_val_if_fail (GNC_TREE_MODEL_SORT (tree_model)->stamp == iter->stamp, NULL);

    retval = gtk_tree_path_new ();
    level = iter->user_data;
    elt = iter->user_data2;
    while (level != NULL)
    {
        gtk_tree_path_prepend_index (retval, elt - (SortElt *)level->array->data);

        elt = level->parent_elt;
        level = level->parent_level;
    }

    return retval;
}

static void
gnc_tree_model_sort_get_value (GtkTreeModel *tree_model,
                               GtkTreeIter  *iter,
                               gint          column,
                               GValue       *value)
{
    GtkTreeIter child_iter;

    g_return_if_fail (GNC_IS_TREE_MODEL_SORT (tree_model));
    g_return_if_fail (GNC_TREE_MODEL_SORT (tree_model)->child_model != NULL);
    g_return_if_fail (GNC_TREE_MODEL_SORT (tree_model)->stamp == iter->stamp);

    GET_CHILD_ITER (tree_model, &child_iter, iter);
    gtk_tree_model_get_value (GNC_TREE_MODEL_SORT (tree_model)->child_model,
                              &child_iter, column, value);
}

static gboolean
gnc_tree_model_sort_iter_next (GtkTreeModel *tree_model,
                               GtkTreeIter  *iter)
{
    SortLevel *level;
    SortElt *elt;

    g_return_val_if_fail (GNC_IS_TREE_MODEL_SORT (tree_model), FALSE);
    g_return_val_if_fail (GNC_TREE_MODEL_SORT (tree_model)->child_model != NULL, FALSE);
    g_return_val_if_fail (GNC_TREE_MODEL_SORT (tree_model)->stamp == iter->stamp, FALSE);

    level = iter->user_data;
    elt = iter->user_data2;

    if (elt - (SortElt *)level->array->data >= level->array->len - 1)
    {
        iter->stamp = 0;
        return FALSE;
    }
    iter->user_data2 = elt + 1;

    return TRUE;
}

static gboolean
gnc_tree_model_sort_iter_children (GtkTreeModel *tree_model,
                                   GtkTreeIter  *iter,
                                   GtkTreeIter  *parent)
{
    GncTreeModelSort *tree_model_sort = (GncTreeModelSort *) tree_model;
    SortLevel *level;

    iter->stamp = 0;
    g_return_val_if_fail (GNC_IS_TREE_MODEL_SORT (tree_model), FALSE);
    g_return_val_if_fail (tree_model_sort->child_model != NULL, FALSE);
    if (parent) g_return_val_if_fail (tree_model_sort->stamp == parent->stamp, FALSE);

    if (parent == NULL)
    {
        if (tree_model_sort->root == NULL)
            gnc_tree_model_sort_build_level (tree_model_sort, NULL, NULL);
        if (tree_model_sort->root == NULL)
            return FALSE;

        level = tree_model_sort->root;
        iter->stamp = tree_model_sort->stamp;
        iter->user_data = level;
        iter->user_data2 = level->array->data;
    }
    else
    {
        if (((SortElt *)parent->user_data2)->children == NULL)
            gnc_tree_model_sort_build_level (tree_model_sort,
                                             (SortLevel *)parent->user_data,
                                             (SortElt *)parent->user_data2);
        if (((SortElt *)parent->user_data2)->children == NULL)
            return FALSE;
        iter->stamp = tree_model_sort->stamp;
        iter->user_data = ((SortElt *)parent->user_data2)->children;
        iter->user_data2 = ((SortLevel *)iter->user_data)->array->data;
    }

    return TRUE;
}

static gboolean
gnc_tree_model_sort_iter_has_child (GtkTreeModel *tree_model,
                                    GtkTreeIter  *iter)
{
    GtkTreeIter child_iter;

    g_return_val_if_fail (GNC_IS_TREE_MODEL_SORT (tree_model), FALSE);
    g_return_val_if_fail (GNC_TREE_MODEL_SORT (tree_model)->child_model != NULL, FALSE);
    g_return_val_if_fail (GNC_TREE_MODEL_SORT (tree_model)->stamp == iter->stamp, FALSE);

    GET_CHILD_ITER (tree_model, &child_iter, iter);

    return gtk_tree_model_iter_has_child (GNC_TREE_MODEL_SORT (tree_model)->child_model, &child_iter);
}

static gint
gnc_tree_model_sort_iter_n_children (GtkTreeModel *tree_model,
                                     GtkTreeIter  *iter)
{
    GtkTreeIter child_iter;

    g_return_val_if_fail (GNC_IS_TREE_MODEL_SORT (tree_model), 0);
    g_return_val_if_fail (GNC_TREE_MODEL_SORT (tree_model)->child_model != NULL, 0);
    if (iter) g_return_val_if_fail (GNC_TREE_MODEL_SORT (tree_model)->stamp == iter->stamp, 0);

    if (iter == NULL)
        return gtk_tree_model_iter_n_children (GNC_TREE_MODEL_SORT (tree_model)->child_model, NULL);

    GET_CHILD_ITER (tree_model, &child_iter, iter);

    return gtk_tree_model_iter_n_children (GNC_TREE_MODEL_SORT (tree_model)->child_model, &child_iter);
}

static gboolean
gnc_tree_model_sort_iter_nth_child (GtkTreeModel *tree_model,
                                    GtkTreeIter  *iter,
                                    GtkTreeIter  *parent,
                                    gint          n)
{
    SortLevel *level;
    /* We have this for the iter == parent case */
    GtkTreeIter children;

    g_return_val_if_fail (GNC_IS_TREE_MODEL_SORT (tree_model), FALSE);
    if (parent) g_return_val_if_fail (GNC_TREE_MODEL_SORT (tree_model)->stamp == parent->stamp, FALSE);

    /* Use this instead of has_child to force us to build the level, if needed */
    if (gnc_tree_model_sort_iter_children (tree_model, &children, parent) == FALSE)
    {
        iter->stamp = 0;
        return FALSE;
    }

    level = children.user_data;
    if (n >= level->array->len)
    {
        iter->stamp = 0;
        return FALSE;
    }

    iter->stamp = GNC_TREE_MODEL_SORT (tree_model)->stamp;
    iter->user_data = level;
    iter->user_data2 = &g_array_index (level->array, SortElt, n);

    return TRUE;
}

static gboolean
gnc_tree_model_sort_iter_parent (GtkTreeModel *tree_model,
                                 GtkTreeIter  *iter,
                                 GtkTreeIter  *child)
{
    SortLevel *level;

    iter->stamp = 0;
    g_return_val_if_fail (GNC_IS_TREE_MODEL_SORT (tree_model), FALSE);
    g_return_val_if_fail (GNC_TREE_MODEL_SORT (tree_model)->child_model != NULL, FALSE);
    g_return_val_if_fail (GNC_TREE_MODEL_SORT (tree_model)->stamp == child->stamp, FALSE);

    level = child->user_data;

    if (level->parent_level)
    {
        iter->stamp = GNC_TREE_MODEL_SORT (tree_model)->stamp;
        iter->user_data = level->parent_level;
        iter->user_data2 = level->parent_elt;

        return TRUE;
    }
    return FALSE;
}

static void
gnc_tree_model_sort_ref_node (GtkTreeModel *tree_model,
                              GtkTreeIter  *iter)
{
    GncTreeModelSort *tree_model_sort = (GncTreeModelSort *) tree_model;
    GtkTreeIter child_iter;
    SortLevel *level;
    SortElt *elt;

    g_return_if_fail (GNC_IS_TREE_MODEL_SORT (tree_model));
    g_return_if_fail (GNC_TREE_MODEL_SORT (tree_model)->child_model != NULL);
    g_return_if_fail (GNC_TREE_MODEL_SORT (tree_model)->stamp == iter->stamp);

    GET_CHILD_ITER (tree_model, &child_iter, iter);

    gtk_tree_model_ref_node (GNC_TREE_MODEL_SORT (tree_model)->child_model, &child_iter);

    level = iter->user_data;
    elt = iter->user_data2;

    elt->ref_count++;
    level->ref_count++;
    if (level->ref_count == 1)
    {
        SortLevel *parent_level = level->parent_level;
        SortElt *parent_elt = level->parent_elt;
        /* We were at zero -- time to decrement the zero_ref_count val */
        do
        {
            if (parent_elt)
                parent_elt->zero_ref_count--;
            else
                tree_model_sort->zero_ref_count--;

            if (parent_level)
            {
                parent_elt = parent_level->parent_elt;
                parent_level = parent_level->parent_level;
            }
        }
        while (parent_level);
    }
}

static void
gnc_tree_model_sort_real_unref_node (GtkTreeModel *tree_model,
                                     GtkTreeIter  *iter,
                                     gboolean      propagate_unref)
{
    GncTreeModelSort *tree_model_sort = (GncTreeModelSort *) tree_model;
    GtkTreeIter child_iter;
    SortLevel *level;
    SortElt *elt;

    g_return_if_fail (GNC_IS_TREE_MODEL_SORT (tree_model));
    g_return_if_fail (GNC_TREE_MODEL_SORT (tree_model)->child_model != NULL);
    g_return_if_fail (GNC_TREE_MODEL_SORT (tree_model)->stamp == iter->stamp);

    GET_CHILD_ITER (tree_model, &child_iter, iter);

    if (propagate_unref)
        gtk_tree_model_unref_node (GNC_TREE_MODEL_SORT (tree_model)->child_model, &child_iter);

    level = iter->user_data;
    elt = iter->user_data2;

    g_return_if_fail (elt->ref_count > 0);

    elt->ref_count--;
    level->ref_count--;

    if (level->ref_count == 0)
    {
        SortLevel *parent_level = level->parent_level;
        SortElt *parent_elt = level->parent_elt;

        /* We are at zero -- time to increment the zero_ref_count val */
        while (parent_level)
        {
            parent_elt->zero_ref_count++;

            parent_elt = parent_level->parent_elt;
            parent_level = parent_level->parent_level;
        }
        tree_model_sort->zero_ref_count++;
    }
}

static void
gnc_tree_model_sort_unref_node (GtkTreeModel *tree_model,
                                GtkTreeIter  *iter)
{
    gnc_tree_model_sort_real_unref_node (tree_model, iter, TRUE);
}

/* Sortable interface */
static gboolean
gnc_tree_model_sort_get_sort_column_id (GtkTreeSortable *sortable,
                                        gint            *sort_column_id,
                                        GtkSortType     *order)
{
    GncTreeModelSort *tree_model_sort = (GncTreeModelSort *)sortable;

    g_return_val_if_fail (GNC_IS_TREE_MODEL_SORT (sortable), FALSE);

    if (sort_column_id)
        *sort_column_id = tree_model_sort->sort_column_id;
    if (order)
        *order = tree_model_sort->order;

    if (tree_model_sort->sort_column_id == GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID ||
            tree_model_sort->sort_column_id == GTK_TREE_SORTABLE_UNSORTED_SORT_COLUMN_ID)
        return FALSE;

    return TRUE;
}

static void
gnc_tree_model_sort_set_sort_column_id (GtkTreeSortable *sortable,
                                        gint             sort_column_id,
                                        GtkSortType      order)
{
    GncTreeModelSort *tree_model_sort = (GncTreeModelSort *)sortable;

    g_return_if_fail (GNC_IS_TREE_MODEL_SORT (sortable));

    if (sort_column_id != GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID)
    {
        GtkTreeDataSortHeader *header = NULL;

        header = _gtk_tree_data_list_get_header (tree_model_sort->sort_list,
                 sort_column_id);

        /* we want to make sure that we have a function */
        g_return_if_fail (header != NULL);
        g_return_if_fail (header->func != NULL);
    }
    else
        g_return_if_fail (tree_model_sort->default_sort_func != NULL);

    if (tree_model_sort->sort_column_id == sort_column_id)
    {
        if (sort_column_id != GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID)
        {
            if (tree_model_sort->order == order)
                return;
        }
        else
            return;
    }

    tree_model_sort->sort_column_id = sort_column_id;
    tree_model_sort->order = order;

    gtk_tree_sortable_sort_column_changed (sortable);

    gnc_tree_model_sort_sort (tree_model_sort);
}

static void
gnc_tree_model_sort_set_sort_func (GtkTreeSortable        *sortable,
                                   gint                    sort_column_id,
                                   GtkTreeIterCompareFunc  func,
                                   gpointer                data,
                                   GtkDestroyNotify        destroy)
{
    GncTreeModelSort *tree_model_sort = (GncTreeModelSort *) sortable;
    GtkTreeDataSortHeader *header = NULL;
    GList *list;

    g_return_if_fail (GNC_IS_TREE_MODEL_SORT (sortable));
    g_return_if_fail (func != NULL);

    for (list = tree_model_sort->sort_list; list; list = list->next)
    {
        GtkTreeDataSortHeader *list_header;

        list_header = (GtkTreeDataSortHeader*) list->data;
        if (list_header->sort_column_id == sort_column_id)
        {
            header = list_header;
            break;
        }
    }

    if (header == NULL)
    {
        header = g_new0 (GtkTreeDataSortHeader, 1);
        header->sort_column_id = sort_column_id;
        tree_model_sort->sort_list = g_list_append (tree_model_sort->sort_list,
                                     header);
    }

    if (header->destroy)
    {
        GtkDestroyNotify d = header->destroy;

        header->destroy = NULL;
        d (header->data);
    }

    header->func = func;
    header->data = data;
    header->destroy = destroy;

    if (tree_model_sort->sort_column_id == sort_column_id)
        gnc_tree_model_sort_sort (tree_model_sort);
}

static void
gnc_tree_model_sort_set_default_sort_func (GtkTreeSortable        *sortable,
        GtkTreeIterCompareFunc  func,
        gpointer                data,
        GtkDestroyNotify        destroy)
{
    GncTreeModelSort *tree_model_sort = (GncTreeModelSort *)sortable;

    g_return_if_fail (GNC_IS_TREE_MODEL_SORT (sortable));

    if (tree_model_sort->default_sort_destroy)
    {
        GtkDestroyNotify d = tree_model_sort->default_sort_destroy;

        tree_model_sort->default_sort_destroy = NULL;
        d (tree_model_sort->default_sort_data);
    }

    tree_model_sort->default_sort_func = func;
    tree_model_sort->default_sort_data = data;
    tree_model_sort->default_sort_destroy = destroy;

    if (tree_model_sort->sort_column_id == GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID)
        gnc_tree_model_sort_sort (tree_model_sort);
}

static gboolean
gnc_tree_model_sort_has_default_sort_func (GtkTreeSortable *sortable)
{
    GncTreeModelSort *tree_model_sort = (GncTreeModelSort *)sortable;

    g_return_val_if_fail (GNC_IS_TREE_MODEL_SORT (sortable), FALSE);

    return (tree_model_sort->default_sort_func != NULL);
}

/* DragSource interface */
static gboolean
gnc_tree_model_sort_row_draggable (GtkTreeDragSource *drag_source,
                                   GtkTreePath       *path)
{
    GncTreeModelSort *tree_model_sort = (GncTreeModelSort *)drag_source;
    GtkTreePath *child_path;
    gboolean draggable;

    g_return_val_if_fail (GNC_IS_TREE_MODEL_SORT (drag_source), FALSE);
    g_return_val_if_fail (path != NULL, FALSE);

    child_path = gnc_tree_model_sort_convert_path_to_child_path (tree_model_sort,
                 path);
    draggable = gtk_tree_drag_source_row_draggable (GTK_TREE_DRAG_SOURCE (tree_model_sort->child_model), child_path);
    gtk_tree_path_free (child_path);

    return draggable;
}

static gboolean
gnc_tree_model_sort_drag_data_get (GtkTreeDragSource *drag_source,
                                   GtkTreePath       *path,
                                   GtkSelectionData  *selection_data)
{
    GncTreeModelSort *tree_model_sort = (GncTreeModelSort *)drag_source;
    GtkTreePath *child_path;
    gboolean gotten;

    g_return_val_if_fail (GNC_IS_TREE_MODEL_SORT (drag_source), FALSE);
    g_return_val_if_fail (path != NULL, FALSE);

    child_path = gnc_tree_model_sort_convert_path_to_child_path (tree_model_sort,
                 path);
    gotten = gtk_tree_drag_source_drag_data_get (GTK_TREE_DRAG_SOURCE (tree_model_sort->child_model), child_path, selection_data);
    gtk_tree_path_free (child_path);

    return gotten;
}

static gboolean
gnc_tree_model_sort_drag_data_delete (GtkTreeDragSource *drag_source,
                                      GtkTreePath       *path)
{
    GncTreeModelSort *tree_model_sort = (GncTreeModelSort *)drag_source;
    GtkTreePath *child_path;
    gboolean deleted;

    g_return_val_if_fail (GNC_IS_TREE_MODEL_SORT (drag_source), FALSE);
    g_return_val_if_fail (path != NULL, FALSE);

    child_path = gnc_tree_model_sort_convert_path_to_child_path (tree_model_sort,
                 path);
    deleted = gtk_tree_drag_source_drag_data_delete (GTK_TREE_DRAG_SOURCE (tree_model_sort->child_model), child_path);
    gtk_tree_path_free (child_path);

    return deleted;
}

/* sorting code - private */
static gint
gnc_tree_model_sort_compare_func (gconstpointer a,
                                  gconstpointer b,
                                  gpointer      user_data)
{
    SortData *data = (SortData *)user_data;
    GncTreeModelSort *tree_model_sort = data->tree_model_sort;
    SortTuple *sa = (SortTuple *)a;
    SortTuple *sb = (SortTuple *)b;

    GtkTreeIter iter_a, iter_b;
    gint retval;

    /* shortcut, if we've the same offsets here, they should be equal */
    if (sa->offset == sb->offset)
        return 0;

    if (GNC_TREE_MODEL_SORT_CACHE_CHILD_ITERS (tree_model_sort))
    {
        iter_a = sa->elt->iter;
        iter_b = sb->elt->iter;
    }
    else
    {
        data->parent_path_indices [data->parent_path_depth-1] = sa->elt->offset;
        gtk_tree_model_get_iter (GTK_TREE_MODEL (tree_model_sort->child_model), &iter_a, data->parent_path);
        data->parent_path_indices [data->parent_path_depth-1] = sb->elt->offset;
        gtk_tree_model_get_iter (GTK_TREE_MODEL (tree_model_sort->child_model), &iter_b, data->parent_path);
    }

    retval = (* data->sort_func) (GTK_TREE_MODEL (tree_model_sort->child_model),
                                  &iter_a, &iter_b,
                                  data->sort_data);

    if (tree_model_sort->order == GTK_SORT_DESCENDING)
    {
        if (retval > 0)
            retval = -1;
        else if (retval < 0)
            retval = 1;
    }

    return retval;
}

static gint
gnc_tree_model_sort_offset_compare_func (gconstpointer a,
        gconstpointer b,
        gpointer      user_data)
{
    gint retval;

    SortTuple *sa = (SortTuple *)a;
    SortTuple *sb = (SortTuple *)b;

    SortData *data = (SortData *)user_data;

    if (sa->elt->offset < sb->elt->offset)
        retval = -1;
    else if (sa->elt->offset > sb->elt->offset)
        retval = 1;
    else
        retval = 0;

    if (data->tree_model_sort->order == GTK_SORT_DESCENDING)
    {
        if (retval > 0)
            retval = -1;
        else if (retval < 0)
            retval = 1;
    }

    return retval;
}

static void
gnc_tree_model_sort_sort_level (GncTreeModelSort *tree_model_sort,
                                SortLevel        *level,
                                gboolean          recurse,
                                gboolean          emit_reordered)
{
    gint i;
    GArray *sort_array;
    GArray *new_array;
    gint *new_order;

    GtkTreeIter iter;
    GtkTreePath *path;

    SortData data;

    g_return_if_fail (GNC_IS_TREE_MODEL_SORT (tree_model_sort));
    g_return_if_fail (level != NULL);

    if (level->array->len < 1 && !((SortElt *)level->array->data)->children)
        return;

    level->ref_count++;

    /* Set up data */
    data.tree_model_sort = tree_model_sort;
    if (level->parent_elt)
    {
        data.parent_path = gnc_tree_model_sort_elt_get_path (level->parent_level,
                           level->parent_elt);
        gtk_tree_path_append_index (data.parent_path, 0);
    }
    else
    {
        data.parent_path = gtk_tree_path_new_first ();
    }
    data.parent_path_depth = gtk_tree_path_get_depth (data.parent_path);
    data.parent_path_indices = gtk_tree_path_get_indices (data.parent_path);

    /* make the array to be sorted */
    sort_array = g_array_sized_new (FALSE, FALSE, sizeof (SortTuple), level->array->len);
    for (i = 0; i < level->array->len; i++)
    {
        SortTuple tuple;

        tuple.elt = &g_array_index (level->array, SortElt, i);
        tuple.offset = i;

        g_array_append_val (sort_array, tuple);
    }

    if (tree_model_sort->sort_column_id != GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID)
    {
        GtkTreeDataSortHeader *header = NULL;

        header = _gtk_tree_data_list_get_header (tree_model_sort->sort_list,
                 tree_model_sort->sort_column_id);

        g_return_if_fail (header != NULL);
        g_return_if_fail (header->func != NULL);

        data.sort_func = header->func;
        data.sort_data = header->data;
    }
    else
    {
        /* absolutely SHOULD NOT happen: */
        g_return_if_fail (tree_model_sort->default_sort_func != NULL);

        data.sort_func = tree_model_sort->default_sort_func;
        data.sort_data = tree_model_sort->default_sort_data;
    }

    if (data.sort_func == NO_SORT_FUNC)
        g_array_sort_with_data (sort_array,
                                gnc_tree_model_sort_offset_compare_func,
                                &data);
    else
        g_array_sort_with_data (sort_array,
                                gnc_tree_model_sort_compare_func,
                                &data);

    gtk_tree_path_free (data.parent_path);

    new_array = g_array_sized_new (FALSE, FALSE, sizeof (SortElt), level->array->len);
    new_order = g_new (gint, level->array->len);

    for (i = 0; i < level->array->len; i++)
    {
        SortElt *elt;

        elt = g_array_index (sort_array, SortTuple, i).elt;
        new_order[i] = g_array_index (sort_array, SortTuple, i).offset;

        g_array_append_val (new_array, *elt);
        elt = &g_array_index (new_array, SortElt, i);
        if (elt->children)
            elt->children->parent_elt = elt;
    }

    g_array_free (level->array, TRUE);
    level->array = new_array;
    g_array_free (sort_array, TRUE);

    if (emit_reordered)
    {
        gnc_tree_model_sort_increment_stamp (tree_model_sort);
        if (level->parent_elt)
        {
            iter.stamp = tree_model_sort->stamp;
            iter.user_data = level->parent_level;
            iter.user_data2 = level->parent_elt;

            path = gtk_tree_model_get_path (GTK_TREE_MODEL (tree_model_sort),
                                            &iter);

            gtk_tree_model_rows_reordered (GTK_TREE_MODEL (tree_model_sort), path,
                                           &iter, new_order);
        }
        else
        {
            /* toplevel list */
            path = gtk_tree_path_new ();
            gtk_tree_model_rows_reordered (GTK_TREE_MODEL (tree_model_sort), path,
                                           NULL, new_order);
        }

        gtk_tree_path_free (path);
    }

    /* recurse, if possible */
    if (recurse)
    {
        for (i = 0; i < level->array->len; i++)
        {
            SortElt *elt = &g_array_index (level->array, SortElt, i);

            if (elt->children)
                gnc_tree_model_sort_sort_level (tree_model_sort,
                                                elt->children,
                                                TRUE, emit_reordered);
        }
    }

    g_free (new_order);

    level->ref_count--;
}

static void
gnc_tree_model_sort_sort (GncTreeModelSort *tree_model_sort)
{
    g_return_if_fail (GNC_IS_TREE_MODEL_SORT (tree_model_sort));

    if (!tree_model_sort->root)
        return;

    if (tree_model_sort->sort_column_id != GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID)
    {
        GtkTreeDataSortHeader *header = NULL;

        header = _gtk_tree_data_list_get_header (tree_model_sort->sort_list,
                 tree_model_sort->sort_column_id);

        /* we want to make sure that we have a function */
        g_return_if_fail (header != NULL);
        g_return_if_fail (header->func != NULL);
    }
    else
        g_return_if_fail (tree_model_sort->default_sort_func != NULL);

    gnc_tree_model_sort_sort_level (tree_model_sort, tree_model_sort->root,
                                    TRUE, TRUE);
}

/* signal helpers */
static gint
gnc_tree_model_sort_level_find_insert (GncTreeModelSort *tree_model_sort,
                                       SortLevel        *level,
                                       GtkTreeIter      *iter,
                                       gint             skip_index)
{
    gint start, middle, end;
    gint cmp;
    SortElt *tmp_elt;
    GtkTreeIter tmp_iter;

    GtkTreeIterCompareFunc func;
    gpointer data;

    if (tree_model_sort->sort_column_id != GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID)
    {
        GtkTreeDataSortHeader *header;

        header = _gtk_tree_data_list_get_header (tree_model_sort->sort_list,
                 tree_model_sort->sort_column_id);

        g_return_val_if_fail (header != NULL, 0);

        func = header->func;
        data = header->data;
    }
    else
    {
        func = tree_model_sort->default_sort_func;
        data = tree_model_sort->default_sort_data;

        g_return_val_if_fail (func != NO_SORT_FUNC, 0);
    }

    g_return_val_if_fail (func != NULL, 0);

    start = 0;
    end = level->array->len;
    if (skip_index < 0)
        skip_index = end;
    else
        end--;

    if (start == end)
        return 0;

    while (start != end)
    {
        middle = (start + end) / 2;

        if (middle < skip_index)
            tmp_elt = &(g_array_index (level->array, SortElt, middle));
        else
            tmp_elt = &(g_array_index (level->array, SortElt, middle + 1));

        if (!GNC_TREE_MODEL_SORT_CACHE_CHILD_ITERS (tree_model_sort))
        {
            GtkTreePath *path = gnc_tree_model_sort_elt_get_path (level, tmp_elt);
            gtk_tree_model_get_iter (tree_model_sort->child_model,
                                     &tmp_iter, path);
            gtk_tree_path_free (path);
        }
        else
            tmp_iter = tmp_elt->iter;

        if (tree_model_sort->order == GTK_SORT_ASCENDING)
            cmp = (* func) (GTK_TREE_MODEL (tree_model_sort->child_model),
                            &tmp_iter, iter, data);
        else
            cmp = (* func) (GTK_TREE_MODEL (tree_model_sort->child_model),
                            iter, &tmp_iter, data);

        if (cmp <= 0)
            start = middle + 1;
        else
            end = middle;
    }

    if (cmp <= 0)
        return middle + 1;
    else
        return middle;
}

static gboolean
gnc_tree_model_sort_insert_value (GncTreeModelSort *tree_model_sort,
                                  SortLevel        *level,
                                  GtkTreePath      *s_path,
                                  GtkTreeIter      *s_iter)
{
    gint offset, index, i;

    SortElt elt;
    SortElt *tmp_elt;

    offset = gtk_tree_path_get_indices (s_path)[gtk_tree_path_get_depth (s_path) - 1];

    if (GNC_TREE_MODEL_SORT_CACHE_CHILD_ITERS (tree_model_sort))
        elt.iter = *s_iter;
    elt.offset = offset;
    elt.zero_ref_count = 0;
    elt.ref_count = 0;
    elt.children = NULL;

    /* update all larger offsets */
    tmp_elt = SORT_ELT (level->array->data);
    for (i = 0; i < level->array->len; i++, tmp_elt++)
        if (tmp_elt->offset >= offset)
            tmp_elt->offset++;

    if (tree_model_sort->sort_column_id == GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID &&
            tree_model_sort->default_sort_func == NO_SORT_FUNC)
        index = offset;
    else
        index = gnc_tree_model_sort_level_find_insert (tree_model_sort,
                level, s_iter,
                -1);

    g_array_insert_vals (level->array, index, &elt, 1);
    tmp_elt = SORT_ELT (level->array->data);
    for (i = 0; i < level->array->len; i++, tmp_elt++)
        if (tmp_elt->children)
            tmp_elt->children->parent_elt = tmp_elt;

    return TRUE;
}

/* sort elt stuff */
static GtkTreePath *
gnc_tree_model_sort_elt_get_path (SortLevel *level,
                                  SortElt *elt)
{
    SortLevel *walker = level;
    SortElt *walker2 = elt;
    GtkTreePath *path;

    g_return_val_if_fail (level != NULL, NULL);
    g_return_val_if_fail (elt != NULL, NULL);

    path = gtk_tree_path_new ();

    while (walker)
    {
        gtk_tree_path_prepend_index (path, walker2->offset);

        walker2 = walker->parent_elt;
        walker = walker->parent_level;
    }

    return path;
}

/**
 * gnc_tree_model_sort_set_model:
 * @tree_model_sort: The #GncTreeModelSort.
 * @child_model: A #GtkTreeModel, or %NULL.
 *
 * Sets the model of @tree_model_sort to be @model.  If @model is %NULL,
 * then the old model is unset.  The sort function is unset as a result
 * of this call. The model will be in an unsorted state until a sort
 * function is set.
 **/
static void
gnc_tree_model_sort_set_model (GncTreeModelSort *tree_model_sort,
                               GtkTreeModel     *child_model)
{
    g_return_if_fail (GNC_IS_TREE_MODEL_SORT (tree_model_sort));

    if (child_model)
        g_object_ref (child_model);

    if (tree_model_sort->child_model)
    {
        g_signal_handler_disconnect (tree_model_sort->child_model,
                                     tree_model_sort->changed_id);
        g_signal_handler_disconnect (tree_model_sort->child_model,
                                     tree_model_sort->inserted_id);
        g_signal_handler_disconnect (tree_model_sort->child_model,
                                     tree_model_sort->has_child_toggled_id);
        g_signal_handler_disconnect (tree_model_sort->child_model,
                                     tree_model_sort->deleted_id);
        g_signal_handler_disconnect (tree_model_sort->child_model,
                                     tree_model_sort->reordered_id);

        /* reset our state */
        if (tree_model_sort->root)
            gnc_tree_model_sort_free_level (tree_model_sort, tree_model_sort->root);
        tree_model_sort->root = NULL;
        _gtk_tree_data_list_header_free (tree_model_sort->sort_list);
        tree_model_sort->sort_list = NULL;
        g_object_unref (tree_model_sort->child_model);
    }

    tree_model_sort->child_model = child_model;

    if (child_model)
    {
        GType *types;
        gint i, n_columns;

        tree_model_sort->changed_id =
            g_signal_connect (child_model, "row_changed",
                              G_CALLBACK (gnc_tree_model_sort_row_changed),
                              tree_model_sort);
        tree_model_sort->inserted_id =
            g_signal_connect (child_model, "row_inserted",
                              G_CALLBACK (gnc_tree_model_sort_row_inserted),
                              tree_model_sort);
        tree_model_sort->has_child_toggled_id =
            g_signal_connect (child_model, "row_has_child_toggled",
                              G_CALLBACK (gnc_tree_model_sort_row_has_child_toggled),
                              tree_model_sort);
        tree_model_sort->deleted_id =
            g_signal_connect (child_model, "row_deleted",
                              G_CALLBACK (gnc_tree_model_sort_row_deleted),
                              tree_model_sort);
        tree_model_sort->reordered_id =
            g_signal_connect (child_model, "rows_reordered",
                              G_CALLBACK (gnc_tree_model_sort_rows_reordered),
                              tree_model_sort);

        tree_model_sort->child_flags = gtk_tree_model_get_flags (child_model);
        n_columns = gtk_tree_model_get_n_columns (child_model);

        types = g_new (GType, n_columns);
        for (i = 0; i < n_columns; i++)
            types[i] = gtk_tree_model_get_column_type (child_model, i);

        tree_model_sort->sort_list = _gtk_tree_data_list_header_new (n_columns, types);
        g_free (types);

        tree_model_sort->default_sort_func = NO_SORT_FUNC;
        tree_model_sort->stamp = g_random_int ();
    }
}

/**
 * gnc_tree_model_sort_get_model:
 * @tree_model: a #GncTreeModelSort
 *
 * Returns the model the #GncTreeModelSort is sorting.
 *
 * Return value: the "child model" being sorted
 **/
GtkTreeModel *
gnc_tree_model_sort_get_model (GncTreeModelSort  *tree_model)
{
    g_return_val_if_fail (GNC_IS_TREE_MODEL_SORT (tree_model), NULL);

    return tree_model->child_model;
}


static GtkTreePath *
gtk_real_tree_model_sort_convert_child_path_to_path (GncTreeModelSort *tree_model_sort,
        GtkTreePath      *child_path,
        gboolean          build_levels)
{
    gint *child_indices;
    GtkTreePath *retval;
    SortLevel *level;
    gint i;

    g_return_val_if_fail (GNC_IS_TREE_MODEL_SORT (tree_model_sort), NULL);
    g_return_val_if_fail (tree_model_sort->child_model != NULL, NULL);
    g_return_val_if_fail (child_path != NULL, NULL);

    retval = gtk_tree_path_new ();
    child_indices = gtk_tree_path_get_indices (child_path);

    if (tree_model_sort->root == NULL && build_levels)
        gnc_tree_model_sort_build_level (tree_model_sort, NULL, NULL);
    level = SORT_LEVEL (tree_model_sort->root);

    for (i = 0; i < gtk_tree_path_get_depth (child_path); i++)
    {
        gint j;
        gboolean found_child = FALSE;

        if (!level)
        {
            gtk_tree_path_free (retval);
            return NULL;
        }

        if (child_indices[i] >= level->array->len)
        {
            gtk_tree_path_free (retval);
            return NULL;
        }
        for (j = 0; j < level->array->len; j++)
        {
            if ((g_array_index (level->array, SortElt, j)).offset == child_indices[i])
            {
                gtk_tree_path_append_index (retval, j);
                if (g_array_index (level->array, SortElt, j).children == NULL && build_levels)
                {
                    gnc_tree_model_sort_build_level (tree_model_sort, level, &g_array_index (level->array, SortElt, j));
                }
                level = g_array_index (level->array, SortElt, j).children;
                found_child = TRUE;
                break;
            }
        }
        if (! found_child)
        {
            gtk_tree_path_free (retval);
            return NULL;
        }
    }

    return retval;
}


/**
 * gnc_tree_model_sort_convert_child_path_to_path:
 * @tree_model_sort: A #GncTreeModelSort
 * @child_path: A #GtkTreePath to convert
 *
 * Converts @child_path to a path relative to @tree_model_sort.  That is,
 * @child_path points to a path in the child model.  The returned path will
 * point to the same row in the sorted model.  If @child_path isn't a valid
 * path on the child model, then %NULL is returned.
 *
 * Return value: A newly allocated #GtkTreePath, or %NULL
 **/
GtkTreePath *
gnc_tree_model_sort_convert_child_path_to_path (GncTreeModelSort *tree_model_sort,
        GtkTreePath      *child_path)
{
    g_return_val_if_fail (GNC_IS_TREE_MODEL_SORT (tree_model_sort), NULL);
    g_return_val_if_fail (tree_model_sort->child_model != NULL, NULL);
    g_return_val_if_fail (child_path != NULL, NULL);

    return gtk_real_tree_model_sort_convert_child_path_to_path (tree_model_sort, child_path, TRUE);
}

/**
 * gnc_tree_model_sort_convert_child_iter_to_iter:
 * @tree_model_sort: A #GncTreeModelSort
 * @sort_iter: An uninitialized #GtkTreeIter.
 * @child_iter: A valid #GtkTreeIter pointing to a row on the child model
 *
 * Sets @sort_iter to point to the row in @tree_model_sort that corresponds to
 * the row pointed at by @child_iter.
 **/
void
gnc_tree_model_sort_convert_child_iter_to_iter (GncTreeModelSort *tree_model_sort,
        GtkTreeIter      *sort_iter,
        GtkTreeIter      *child_iter)
{
    GtkTreePath *child_path, *path;

    g_return_if_fail (GNC_IS_TREE_MODEL_SORT (tree_model_sort));
    g_return_if_fail (tree_model_sort->child_model != NULL);
    g_return_if_fail (sort_iter != NULL);
    g_return_if_fail (child_iter != NULL);

    sort_iter->stamp = 0;

    child_path = gtk_tree_model_get_path (tree_model_sort->child_model, child_iter);
    g_return_if_fail (child_path != NULL);

    path = gnc_tree_model_sort_convert_child_path_to_path (tree_model_sort, child_path);
    gtk_tree_path_free (child_path);
    g_return_if_fail (path != NULL);

    gtk_tree_model_get_iter (GTK_TREE_MODEL (tree_model_sort), sort_iter, path);
    gtk_tree_path_free (path);
}

/**
 * gnc_tree_model_sort_convert_path_to_child_path:
 * @tree_model_sort: A #GncTreeModelSort
 * @sorted_path: A #GtkTreePath to convert
 *
 * Converts @sorted_path to a path on the child model of @tree_model_sort.
 * That is, @sorted_path points to a location in @tree_model_sort.  The
 * returned path will point to the same location in the model not being
 * sorted.  If @sorted_path does not point to a location in the child model,
 * %NULL is returned.
 *
 * Return value: A newly allocated #GtkTreePath, or %NULL
 **/
GtkTreePath *
gnc_tree_model_sort_convert_path_to_child_path (GncTreeModelSort *tree_model_sort,
        GtkTreePath      *sorted_path)
{
    gint *sorted_indices;
    GtkTreePath *retval;
    SortLevel *level;
    gint i;

    g_return_val_if_fail (GNC_IS_TREE_MODEL_SORT (tree_model_sort), NULL);
    g_return_val_if_fail (tree_model_sort->child_model != NULL, NULL);
    g_return_val_if_fail (sorted_path != NULL, NULL);

    retval = gtk_tree_path_new ();
    sorted_indices = gtk_tree_path_get_indices (sorted_path);
    if (tree_model_sort->root == NULL)
        gnc_tree_model_sort_build_level (tree_model_sort, NULL, NULL);
    level = SORT_LEVEL (tree_model_sort->root);

    for (i = 0; i < gtk_tree_path_get_depth (sorted_path); i++)
    {
        gint count = sorted_indices[i];

        if ((level == NULL) ||
                (level->array->len <= count))
        {
            gtk_tree_path_free (retval);
            return NULL;
        }

        if (g_array_index (level->array, SortElt, count).children == NULL)
            gnc_tree_model_sort_build_level (tree_model_sort, level, &g_array_index (level->array, SortElt, count));

        if (level == NULL)
        {
            gtk_tree_path_free (retval);
            break;
        }

        gtk_tree_path_append_index (retval, g_array_index (level->array, SortElt, count).offset);
        level = g_array_index (level->array, SortElt, count).children;
    }

    return retval;
}

/**
 * gnc_tree_model_sort_convert_iter_to_child_iter:
 * @tree_model_sort: A #GncTreeModelSort
 * @child_iter: An uninitialized #GtkTreeIter
 * @sorted_iter: A valid #GtkTreeIter pointing to a row on @tree_model_sort.
 *
 * Sets @child_iter to point to the row pointed to by @sorted_iter.
 **/
void
gnc_tree_model_sort_convert_iter_to_child_iter (GncTreeModelSort *tree_model_sort,
        GtkTreeIter      *child_iter,
        GtkTreeIter      *sorted_iter)
{
    g_return_if_fail (GNC_IS_TREE_MODEL_SORT (tree_model_sort));
    g_return_if_fail (tree_model_sort->child_model != NULL);
    g_return_if_fail (child_iter != NULL);
    g_return_if_fail (sorted_iter != NULL);
    g_return_if_fail (sorted_iter->stamp == tree_model_sort->stamp);

    if (GNC_TREE_MODEL_SORT_CACHE_CHILD_ITERS (tree_model_sort))
    {
        *child_iter = SORT_ELT (sorted_iter->user_data2)->iter;
    }
    else
    {
        GtkTreePath *path;

        path = gnc_tree_model_sort_elt_get_path (sorted_iter->user_data,
                sorted_iter->user_data2);
        gtk_tree_model_get_iter (tree_model_sort->child_model, child_iter, path);
        gtk_tree_path_free (path);
    }
}

static void
gnc_tree_model_sort_build_level (GncTreeModelSort *tree_model_sort,
                                 SortLevel        *parent_level,
                                 SortElt          *parent_elt)
{
    GtkTreeIter iter;
    SortLevel *new_level;
    gint length = 0;
    gint i;

    g_assert (tree_model_sort->child_model != NULL);

    if (parent_level == NULL)
    {
        if (gtk_tree_model_get_iter_first (tree_model_sort->child_model, &iter) == FALSE)
            return;
        length = gtk_tree_model_iter_n_children (tree_model_sort->child_model, NULL);
    }
    else
    {
        GtkTreeIter parent_iter;
        GtkTreeIter child_parent_iter;

        parent_iter.stamp = tree_model_sort->stamp;
        parent_iter.user_data = parent_level;
        parent_iter.user_data2 = parent_elt;

        gnc_tree_model_sort_convert_iter_to_child_iter (tree_model_sort,
                &child_parent_iter,
                &parent_iter);
        if (gtk_tree_model_iter_children (tree_model_sort->child_model,
                                          &iter,
                                          &child_parent_iter) == FALSE)
            return;

        /* stamp may have changed */
        gnc_tree_model_sort_convert_iter_to_child_iter (tree_model_sort,
                &child_parent_iter,
                &parent_iter);

        length = gtk_tree_model_iter_n_children (tree_model_sort->child_model, &child_parent_iter);
    }

    g_return_if_fail (length > 0);

    new_level = g_new (SortLevel, 1);
    new_level->array = g_array_sized_new (FALSE, FALSE, sizeof (SortElt), length);
    new_level->ref_count = 0;
    new_level->parent_elt = parent_elt;
    new_level->parent_level = parent_level;

    if (parent_elt)
        parent_elt->children = new_level;
    else
        tree_model_sort->root = new_level;

    /* increase the count of zero ref_counts.*/
    while (parent_level)
    {
        parent_elt->zero_ref_count++;

        parent_elt = parent_level->parent_elt;
        parent_level = parent_level->parent_level;
    }
    if (new_level != tree_model_sort->root)
        tree_model_sort->zero_ref_count++;

    for (i = 0; i < length; i++)
    {
        SortElt sort_elt;
        sort_elt.offset = i;
        sort_elt.zero_ref_count = 0;
        sort_elt.ref_count = 0;
        sort_elt.children = NULL;

        if (GNC_TREE_MODEL_SORT_CACHE_CHILD_ITERS (tree_model_sort))
        {
            sort_elt.iter = iter;
            if (gtk_tree_model_iter_next (tree_model_sort->child_model, &iter) == FALSE &&
                    i < length - 1)
            {
                g_warning ("There is a discrepancy between the sort model and the child model.");
                return;
            }
        }
        g_array_append_val (new_level->array, sort_elt);
    }

    /* sort level */
    gnc_tree_model_sort_sort_level (tree_model_sort, new_level,
                                    FALSE, FALSE);
}

static void
gnc_tree_model_sort_free_level (GncTreeModelSort *tree_model_sort,
                                SortLevel        *sort_level)
{
    gint i;

    g_assert (sort_level);

    if (sort_level->ref_count == 0)
    {
        SortLevel *parent_level = sort_level->parent_level;
        SortElt *parent_elt = sort_level->parent_elt;

        do
        {
            if (parent_elt)
                parent_elt->zero_ref_count--;
            else
                tree_model_sort->zero_ref_count--;

            if (parent_level)
            {
                parent_elt = parent_level->parent_elt;
                parent_level = parent_level->parent_level;
            }
        }
        while (parent_level);
    }

    for (i = 0; i < sort_level->array->len; i++)
    {
        if (g_array_index (sort_level->array, SortElt, i).children)
            gnc_tree_model_sort_free_level (tree_model_sort,
                                            SORT_LEVEL(g_array_index (sort_level->array, SortElt, i).children));
    }

    if (sort_level->parent_elt)
        sort_level->parent_elt->children = NULL;
    else
        tree_model_sort->root = NULL;

    g_array_free (sort_level->array, TRUE);
    sort_level->array = NULL;

    g_free (sort_level);
    sort_level = NULL;
}

static void
gnc_tree_model_sort_increment_stamp (GncTreeModelSort *tree_model_sort)
{
    do
    {
        tree_model_sort->stamp++;
    }
    while (tree_model_sort->stamp == 0);

    gnc_tree_model_sort_clear_cache (tree_model_sort);
}

static void
gnc_tree_model_sort_clear_cache_helper (GncTreeModelSort *tree_model_sort,
                                        SortLevel        *level)
{
    gint i;

    g_assert (level != NULL);

    for (i = 0; i < level->array->len; i++)
    {
        if (g_array_index (level->array, SortElt, i).zero_ref_count > 0)
            gnc_tree_model_sort_clear_cache_helper (tree_model_sort, g_array_index (level->array, SortElt, i).children);
    }

    if (level->ref_count == 0 && level != tree_model_sort->root)
        gnc_tree_model_sort_free_level (tree_model_sort, level);
}

/**
 * gnc_tree_model_sort_reset_default_sort_func:
 * @tree_model_sort: A #GncTreeModelSort
 *
 * This resets the default sort function to be in the 'unsorted' state.  That
 * is, it is in the same order as the child model. It will re-sort the model
 * to be in the same order as the child model only if the #GncTreeModelSort
 * is in 'unsorted' state.
 **/
void
gnc_tree_model_sort_reset_default_sort_func (GncTreeModelSort *tree_model_sort)
{
    g_return_if_fail (GNC_IS_TREE_MODEL_SORT (tree_model_sort));

    if (tree_model_sort->default_sort_destroy)
    {
        GtkDestroyNotify d = tree_model_sort->default_sort_destroy;

        tree_model_sort->default_sort_destroy = NULL;
        d (tree_model_sort->default_sort_data);
    }

    tree_model_sort->default_sort_func = NO_SORT_FUNC;
    tree_model_sort->default_sort_data = NULL;
    tree_model_sort->default_sort_destroy = NULL;

    if (tree_model_sort->sort_column_id == GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID)
        gnc_tree_model_sort_sort (tree_model_sort);
    tree_model_sort->sort_column_id = GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID;
}

/**
 * gnc_tree_model_sort_clear_cache:
 * @tree_model_sort: A #GncTreeModelSort
 *
 * This function should almost never be called.  It clears the @tree_model_sort
 * of any cached iterators that haven't been reffed with
 * gtk_tree_model_ref_node().  This might be useful if the child model being
 * sorted is static (and doesn't change often) and there has been a lot of
 * unreffed access to nodes.  As a side effect of this function, all unreffed
 * iters will be invalid.
 **/
void
gnc_tree_model_sort_clear_cache (GncTreeModelSort *tree_model_sort)
{
    g_return_if_fail (GNC_IS_TREE_MODEL_SORT (tree_model_sort));

    if (tree_model_sort->zero_ref_count)
        gnc_tree_model_sort_clear_cache_helper (tree_model_sort, (SortLevel *)tree_model_sort->root);
}

static gboolean
gnc_tree_model_sort_iter_is_valid_helper (GtkTreeIter *iter,
        SortLevel   *level)
{
    gint i;

    for (i = 0; i < level->array->len; i++)
    {
        SortElt *elt = &g_array_index (level->array, SortElt, i);

        if (iter->user_data == level && iter->user_data2 == elt)
            return TRUE;

        if (elt->children)
            if (gnc_tree_model_sort_iter_is_valid_helper (iter, elt->children))
                return TRUE;
    }

    return FALSE;
}

/**
 * gnc_tree_model_sort_iter_is_valid:
 * @tree_model_sort: A #GncTreeModelSort.
 * @iter: A #GtkTreeIter.
 *
 * <warning><para>
 * This function is slow. Only use it for debugging and/or testing purposes.
 * </para></warning>
 *
 * Checks if the given iter is a valid iter for this #GncTreeModelSort.
 *
 * Return value: %TRUE if the iter is valid, %FALSE if the iter is invalid.
 *
 * Since: 2.2
 **/
gboolean
gnc_tree_model_sort_iter_is_valid (GncTreeModelSort *tree_model_sort,
                                   GtkTreeIter      *iter)
{
    g_return_val_if_fail (GNC_IS_TREE_MODEL_SORT (tree_model_sort), FALSE);
    g_return_val_if_fail (iter != NULL, FALSE);

    if (!VALID_ITER (iter, tree_model_sort))
        return FALSE;

    return gnc_tree_model_sort_iter_is_valid_helper (iter,
            tree_model_sort->root);
}

#define __GNC_TREE_MODEL_SORT_C__
//CAS:#include "gtkaliasdef.c"
