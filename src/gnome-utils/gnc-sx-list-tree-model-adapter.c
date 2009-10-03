/* 
 * gnc-sx-list-tree-model-adapter.c
 *
 * Copyright (C) 2006 Josh Sled <jsled@asynchronous.org>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2 of the GNU General Public
 * License as published by the Free Software Foundation.
 *
 * As a special exception, permission is granted to link the binary module
 * resultant from this code with the OpenSSL project's "OpenSSL" library (or
 * modified versions of it that use the same license as the "OpenSSL"
 * library), and distribute the linked executable.  You must obey the GNU
 * General Public License in all respects for all of the code used other than
 * "OpenSSL". If you modify this file, you may extend this exception to your
 * version of the file, but you are not obligated to do so. If you do not
 * wish to do so, delete this exception statement from your version of this
 * file.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

#include "config.h"
#include <glib.h>
#include <glib/gi18n.h>
#include <glib-object.h>
#include "gnc-sx-instance-model.h"
#include "gnc-sx-list-tree-model-adapter.h"
#include <gtk/gtk.h>

struct _GncSxListTreeModelAdapter
{
    GObject parent;

    /* protected */
    gboolean disposed;
    GncSxInstanceModel *instances;
    GtkTreeStore *orig;
    GtkTreeModelSort *real;
};

struct _GncSxListTreeModelAdapterClass
{
    GObjectClass parent;
};

static GObjectClass *parent_class = NULL;

static void gnc_sx_list_tree_model_adapter_class_init(GncSxListTreeModelAdapterClass *klass);
static void gsltma_tree_model_interface_init(gpointer g_iface, gpointer iface_data);
static void gsltma_tree_sortable_interface_init(gpointer g_iface, gpointer iface_data);
static void gnc_sx_list_tree_model_adapter_init(GTypeInstance *instance, gpointer klass);
static void gnc_sx_list_tree_model_adapter_dispose(GObject *obj);
static void gnc_sx_list_tree_model_adapter_finalize(GObject *obj);

static GncSxInstances* gsltma_get_sx_instances_from_orig_iter(GncSxListTreeModelAdapter *model, GtkTreeIter *orig_iter);

GType
gnc_sx_list_tree_model_adapter_get_type(void)
{
    static GType type = 0;
    if (type == 0) {
        static const GTypeInfo info = {
            sizeof (GncSxListTreeModelAdapterClass),
            NULL,   /* base_init */
            NULL,   /* base_finalize */
            (GClassInitFunc)gnc_sx_list_tree_model_adapter_class_init,   /* class_init */
            NULL,   /* class_finalize */
            NULL,   /* class_data */
            sizeof (GncSxListTreeModelAdapter),
            0,      /* n_preallocs */
            (GInstanceInitFunc)gnc_sx_list_tree_model_adapter_init    /* instance_init */
        };
        static const GInterfaceInfo itree_model_info = {
            (GInterfaceInitFunc) gsltma_tree_model_interface_init,    /* interface_init */
            NULL,               /* interface_finalize */
            NULL                /* interface_data */
        };
        static const GInterfaceInfo itree_sortable_info = {
            (GInterfaceInitFunc) gsltma_tree_sortable_interface_init,    /* interface_init */
            NULL,               /* interface_finalize */
            NULL                /* interface_data */
        };

        type = g_type_register_static (G_TYPE_OBJECT,
                                       "GncSxListTreeModelAdapterType",
                                       &info, 0);
        g_type_add_interface_static(type,
                                    GTK_TYPE_TREE_MODEL,
                                    &itree_model_info);
        g_type_add_interface_static(type,
                                    GTK_TYPE_TREE_SORTABLE,
                                    &itree_sortable_info);
    }
    return type;
}

static void
gnc_sx_list_tree_model_adapter_class_init(GncSxListTreeModelAdapterClass *klass)
{
    GObjectClass *obj_class = G_OBJECT_CLASS(klass);

    parent_class = g_type_class_peek_parent(klass);

    obj_class->dispose = gnc_sx_list_tree_model_adapter_dispose;
    obj_class->finalize = gnc_sx_list_tree_model_adapter_finalize;

}

static GtkTreeModelFlags
gsltma_get_flags(GtkTreeModel *tree_model)
{
    return gtk_tree_model_get_flags(GTK_TREE_MODEL(GNC_SX_LIST_TREE_MODEL_ADAPTER(tree_model)->real));
}

static gint
gsltma_get_n_columns(GtkTreeModel *tree_model)
{
    return gtk_tree_model_get_n_columns(GTK_TREE_MODEL(GNC_SX_LIST_TREE_MODEL_ADAPTER(tree_model)->real));
}

static GType
gsltma_get_column_type(GtkTreeModel *tree_model, gint index)
{
    return gtk_tree_model_get_column_type(GTK_TREE_MODEL(GNC_SX_LIST_TREE_MODEL_ADAPTER(tree_model)->real), index);
}

static gboolean
gsltma_get_iter(GtkTreeModel *tree_model,
                GtkTreeIter *iter,
                GtkTreePath *path)
{
    return gtk_tree_model_get_iter(GTK_TREE_MODEL(GNC_SX_LIST_TREE_MODEL_ADAPTER(tree_model)->real), iter, path);
}

static GtkTreePath*
gsltma_get_path(GtkTreeModel *tree_model,
                GtkTreeIter *iter)
{
    return gtk_tree_model_get_path(GTK_TREE_MODEL(GNC_SX_LIST_TREE_MODEL_ADAPTER(tree_model)->real), iter);
}

static void
gsltma_get_value(GtkTreeModel *tree_model,
                 GtkTreeIter *iter,
                 gint column,
                 GValue *value)
{
    gtk_tree_model_get_value(GTK_TREE_MODEL(GNC_SX_LIST_TREE_MODEL_ADAPTER(tree_model)->real), iter, column, value);
}

static gboolean
gsltma_iter_next(GtkTreeModel *tree_model,
                 GtkTreeIter *iter)
{
    return gtk_tree_model_iter_next(GTK_TREE_MODEL(GNC_SX_LIST_TREE_MODEL_ADAPTER(tree_model)->real), iter);
}

static gboolean
gsltma_iter_children(GtkTreeModel *tree_model,
                     GtkTreeIter *iter,
                     GtkTreeIter *parent)
{
    return gtk_tree_model_iter_children(GTK_TREE_MODEL(GNC_SX_LIST_TREE_MODEL_ADAPTER(tree_model)->real), iter, parent);
}

static gboolean
gsltma_iter_has_child(GtkTreeModel *tree_model,
                      GtkTreeIter *iter)
{
    return gtk_tree_model_iter_has_child(GTK_TREE_MODEL(GNC_SX_LIST_TREE_MODEL_ADAPTER(tree_model)->real), iter);
}

static gint
gsltma_iter_n_children(GtkTreeModel *tree_model,
                       GtkTreeIter *iter)
{
    return gtk_tree_model_iter_n_children(GTK_TREE_MODEL(GNC_SX_LIST_TREE_MODEL_ADAPTER(tree_model)->real), iter);
}

static gboolean
gsltma_iter_nth_child(GtkTreeModel *tree_model,
                      GtkTreeIter *iter,
                      GtkTreeIter *parent,
                      gint n)
{
    return gtk_tree_model_iter_nth_child(GTK_TREE_MODEL(GNC_SX_LIST_TREE_MODEL_ADAPTER(tree_model)->real), iter, parent, n);
}

static gboolean
gsltma_iter_parent(GtkTreeModel *tree_model,
                   GtkTreeIter *iter,
                   GtkTreeIter *child)
{
    return gtk_tree_model_iter_parent(GTK_TREE_MODEL(GNC_SX_LIST_TREE_MODEL_ADAPTER(tree_model)->real), iter, child);
}

static void
gsltma_ref_node(GtkTreeModel *tree_model,
                GtkTreeIter *iter)
{
    gtk_tree_model_ref_node(GTK_TREE_MODEL(GNC_SX_LIST_TREE_MODEL_ADAPTER(tree_model)->real), iter);
}

static void
gsltma_unref_node(GtkTreeModel *tree_model,
                  GtkTreeIter *iter)
{
    gtk_tree_model_unref_node(GTK_TREE_MODEL(GNC_SX_LIST_TREE_MODEL_ADAPTER(tree_model)->real), iter);
}

static void
gsltma_tree_model_interface_init(gpointer g_iface, gpointer iface_data)
{
    GtkTreeModelIface *tree_model = (GtkTreeModelIface*)g_iface;
    tree_model->get_flags = gsltma_get_flags;
    tree_model->get_n_columns = gsltma_get_n_columns;
    tree_model->get_column_type = gsltma_get_column_type;
    tree_model->get_iter = gsltma_get_iter;
    tree_model->get_path = gsltma_get_path;
    tree_model->get_value = gsltma_get_value;
    tree_model->iter_next = gsltma_iter_next;
    tree_model->iter_children = gsltma_iter_children;
    tree_model->iter_has_child = gsltma_iter_has_child;
    tree_model->iter_n_children = gsltma_iter_n_children;
    tree_model->iter_nth_child = gsltma_iter_nth_child;
    tree_model->iter_parent = gsltma_iter_parent;
    tree_model->ref_node = gsltma_ref_node;
    tree_model->unref_node = gsltma_unref_node;
}

static gboolean
gsltma_get_sort_column_id(GtkTreeSortable        *sortable,
                          gint                   *sort_column_id,
                          GtkSortType            *order)
{
    return gtk_tree_sortable_get_sort_column_id(GTK_TREE_SORTABLE(GNC_SX_LIST_TREE_MODEL_ADAPTER(sortable)->real),
                                                sort_column_id,
                                                order);
}

static void
gsltma_set_sort_column_id(GtkTreeSortable        *sortable,
                          gint                    sort_column_id,
                          GtkSortType             order)
{
    gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(GNC_SX_LIST_TREE_MODEL_ADAPTER(sortable)->real),
                                         sort_column_id,
                                         order);
}

static void
gsltma_set_sort_func(GtkTreeSortable        *sortable,
                     gint                    sort_column_id,
                     GtkTreeIterCompareFunc  func,
                     gpointer                data,
                     GtkDestroyNotify        destroy)
{
    gtk_tree_sortable_set_sort_func(GTK_TREE_SORTABLE(GNC_SX_LIST_TREE_MODEL_ADAPTER(sortable)->real), 
                                    sort_column_id,
                                    func,
                                    data,
                                    destroy);
}

static void
gsltma_set_default_sort_func(GtkTreeSortable        *sortable,
                             GtkTreeIterCompareFunc  func,
                             gpointer                data,
                             GtkDestroyNotify        destroy)
{
    gtk_tree_sortable_set_default_sort_func(GTK_TREE_SORTABLE(GNC_SX_LIST_TREE_MODEL_ADAPTER(sortable)->real),
                                            func, data, destroy);
}

static gboolean
gsltma_has_default_sort_func(GtkTreeSortable        *sortable)
{
    return gtk_tree_sortable_has_default_sort_func(GTK_TREE_SORTABLE(GNC_SX_LIST_TREE_MODEL_ADAPTER(sortable)->real));
}

static void
gsltma_tree_sortable_interface_init(gpointer g_iface, gpointer iface_data)
{
    GtkTreeSortableIface *tree_sortable = (GtkTreeSortableIface*)g_iface;
    tree_sortable->get_sort_column_id = gsltma_get_sort_column_id;
    tree_sortable->set_sort_column_id = gsltma_set_sort_column_id;
    tree_sortable->set_sort_func = gsltma_set_sort_func;
    tree_sortable->set_default_sort_func = gsltma_set_default_sort_func;
    tree_sortable->has_default_sort_func = gsltma_has_default_sort_func;
    tree_sortable->get_sort_column_id = gsltma_get_sort_column_id;
    tree_sortable->set_sort_column_id = gsltma_set_sort_column_id;
    tree_sortable->set_sort_func = gsltma_set_sort_func;
    tree_sortable->set_default_sort_func = gsltma_set_default_sort_func;
    tree_sortable->has_default_sort_func = gsltma_has_default_sort_func;
}

static void
gsltma_proxy_row_changed(GtkTreeModel *treemodel,
                         GtkTreePath *arg1,
                         GtkTreeIter *arg2,
                         gpointer user_data)
{
    g_signal_emit_by_name(user_data, "row-changed", arg1, arg2);
}

static void
gsltma_proxy_row_deleted(GtkTreeModel *treemodel,
                         GtkTreePath *arg1,
                         gpointer user_data)
{
    g_signal_emit_by_name(user_data, "row-deleted", arg1);
}

static void
gsltma_proxy_row_has_child_toggled(GtkTreeModel *treemodel,
                                   GtkTreePath *arg1,
                                   GtkTreeIter *arg2,
                                   gpointer user_data)
{
    g_signal_emit_by_name(user_data, "row-has-child-toggled", arg1, arg2);
}

static void
gsltma_proxy_row_inserted(GtkTreeModel *treemodel,
                          GtkTreePath *arg1,
                          GtkTreeIter *arg2,
                          gpointer user_data)
{
    g_signal_emit_by_name(user_data, "row-inserted", arg1, arg2);
}

static void
gsltma_proxy_rows_reordered(GtkTreeModel *treemodel,
                            GtkTreePath *arg1,
                            GtkTreeIter *arg2,
                            gpointer arg3,
                            gpointer user_data)
{
    g_signal_emit_by_name(user_data, "rows-reordered", arg1, arg2, arg3);
}

static void
gsltma_proxy_sort_column_changed(GtkTreeSortable *sortable, gpointer user_data)
{
    g_signal_emit_by_name(user_data, "sort-column-changed");
}

static gint
_name_comparator(GtkTreeModel *model, GtkTreeIter *a, GtkTreeIter *b, gpointer user_data)
{
    gint rtn;
    GncSxListTreeModelAdapter *adapter = GNC_SX_LIST_TREE_MODEL_ADAPTER(user_data);
    GncSxInstances *a_inst, *b_inst;
    gchar *a_caseless, *b_caseless;

    a_inst = gsltma_get_sx_instances_from_orig_iter(adapter, a);
    b_inst = gsltma_get_sx_instances_from_orig_iter(adapter, b);

    if (a_inst == NULL && b_inst == NULL) return 0;
    if (a_inst == NULL) return 1;
    if (b_inst == NULL) return -1;

    a_caseless = g_utf8_casefold(xaccSchedXactionGetName(a_inst->sx), -1);
    b_caseless = g_utf8_casefold(xaccSchedXactionGetName(b_inst->sx), -1);
    rtn = safe_strcmp(a_caseless, b_caseless);
    g_free(a_caseless);
    g_free(b_caseless);

    return rtn;
}

static gint
_freq_comparator(GtkTreeModel *model, GtkTreeIter *a, GtkTreeIter *b, gpointer user_data)
{
    GncSxListTreeModelAdapter *adapter = GNC_SX_LIST_TREE_MODEL_ADAPTER(user_data);
    GncSxInstances *a_inst, *b_inst;

    a_inst = gsltma_get_sx_instances_from_orig_iter(adapter, a);
    b_inst = gsltma_get_sx_instances_from_orig_iter(adapter, b);
     
    if (a_inst == NULL && b_inst == NULL) return 0;
    if (a_inst == NULL) return 1;
    if (b_inst == NULL) return -1;

    return recurrenceListCmp(gnc_sx_get_schedule(a_inst->sx), gnc_sx_get_schedule(b_inst->sx));
}

static gint
_safe_invalidable_date_compare(GDate *a, GDate *b)
{
    if (!g_date_valid(a) && !g_date_valid(b))
    {
        return 0;
    }
    if (!g_date_valid(a))
    {
        return 1;
    }
    if (!g_date_valid(b))
    {
        return -1;
    }
    return g_date_compare(a, b);
}

static gint
_last_occur_comparator(GtkTreeModel *model, GtkTreeIter *a, GtkTreeIter *b, gpointer user_data)
{
    GncSxListTreeModelAdapter *adapter = GNC_SX_LIST_TREE_MODEL_ADAPTER(user_data);
    GncSxInstances *a_inst, *b_inst;

    a_inst = gsltma_get_sx_instances_from_orig_iter(adapter, a);
    b_inst = gsltma_get_sx_instances_from_orig_iter(adapter, b);

    return _safe_invalidable_date_compare(xaccSchedXactionGetLastOccurDate(a_inst->sx),
                                          xaccSchedXactionGetLastOccurDate(b_inst->sx));
}

static gint
_next_occur_comparator(GtkTreeModel *model, GtkTreeIter *a, GtkTreeIter *b, gpointer user_data)
{
    GncSxListTreeModelAdapter *adapter = GNC_SX_LIST_TREE_MODEL_ADAPTER(user_data);
    GncSxInstances *a_inst, *b_inst;

    a_inst = gsltma_get_sx_instances_from_orig_iter(adapter, a);
    b_inst = gsltma_get_sx_instances_from_orig_iter(adapter, b);

    return _safe_invalidable_date_compare(&a_inst->next_instance_date,
                                          &b_inst->next_instance_date);
}

static gint
_enabled_comparator(GtkTreeModel *model, GtkTreeIter *a, GtkTreeIter *b, gpointer user_data)
{
    GncSxListTreeModelAdapter *adapter = GNC_SX_LIST_TREE_MODEL_ADAPTER(user_data);
    GncSxInstances *a_inst, *b_inst;

    a_inst = gsltma_get_sx_instances_from_orig_iter(adapter, a);
    b_inst = gsltma_get_sx_instances_from_orig_iter(adapter, b);

    if (xaccSchedXactionGetEnabled(a_inst->sx) && !xaccSchedXactionGetEnabled(b_inst->sx)) return 1;
    if (!xaccSchedXactionGetEnabled(a_inst->sx) && xaccSchedXactionGetEnabled(b_inst->sx)) return -1;
    return 0;
}

static void
gnc_sx_list_tree_model_adapter_init(GTypeInstance *instance, gpointer klass)
{
    GncSxListTreeModelAdapter *adapter = GNC_SX_LIST_TREE_MODEL_ADAPTER(instance);
    adapter->orig = gtk_tree_store_new(5, G_TYPE_STRING, G_TYPE_BOOLEAN, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING);
    adapter->real = GTK_TREE_MODEL_SORT(gtk_tree_model_sort_new_with_model(GTK_TREE_MODEL(adapter->orig)));

    // setup sorting
    gtk_tree_sortable_set_sort_func(GTK_TREE_SORTABLE(adapter->real), SXLTMA_COL_NAME, _name_comparator, adapter, NULL);
    gtk_tree_sortable_set_sort_func(GTK_TREE_SORTABLE(adapter->real), SXLTMA_COL_ENABLED, _enabled_comparator, adapter, NULL);
    gtk_tree_sortable_set_sort_func(GTK_TREE_SORTABLE(adapter->real), SXLTMA_COL_FREQUENCY, _freq_comparator, adapter, NULL);
    gtk_tree_sortable_set_sort_func(GTK_TREE_SORTABLE(adapter->real), SXLTMA_COL_LAST_OCCUR, _last_occur_comparator, adapter, NULL);
    gtk_tree_sortable_set_sort_func(GTK_TREE_SORTABLE(adapter->real), SXLTMA_COL_NEXT_OCCUR, _next_occur_comparator, adapter, NULL);
    gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(adapter->real), SXLTMA_COL_NEXT_OCCUR, GTK_SORT_ASCENDING);

    g_signal_connect(adapter->real, "row-changed", G_CALLBACK(gsltma_proxy_row_changed), adapter);
    g_signal_connect(adapter->real, "row-deleted", G_CALLBACK(gsltma_proxy_row_deleted), adapter);
    g_signal_connect(adapter->real, "row-has-child-toggled", G_CALLBACK(gsltma_proxy_row_has_child_toggled), adapter);
    g_signal_connect(adapter->real, "row-inserted", G_CALLBACK(gsltma_proxy_row_inserted), adapter);
    g_signal_connect(adapter->real, "rows-reordered", G_CALLBACK(gsltma_proxy_rows_reordered), adapter);

    g_signal_connect(adapter->real, "sort-column-changed", G_CALLBACK(gsltma_proxy_sort_column_changed), adapter);
}

static void
_format_conditional_date(GDate *date, char *date_buf, int buf_max_length)
{
    if (date == NULL || !g_date_valid(date))
    {
	g_stpcpy(date_buf, _("never"));
    }
    else
    {
        qof_print_gdate(date_buf, buf_max_length, date);
    }
}

static void
gsltma_populate_tree_store(GncSxListTreeModelAdapter *model)
{
    GtkTreeIter iter;
    GList *list;

    for (list = model->instances->sx_instance_list; list != NULL; list = list->next)
    {
        GncSxInstances *instances = (GncSxInstances*)list->data;
        gchar *frequency_str;
        char last_occur_date_buf[MAX_DATE_LENGTH+1];
        char next_occur_date_buf[MAX_DATE_LENGTH+1];

        frequency_str = recurrenceListToCompactString(gnc_sx_get_schedule(instances->sx));

        _format_conditional_date(xaccSchedXactionGetLastOccurDate(instances->sx),
                                 last_occur_date_buf, MAX_DATE_LENGTH);
        _format_conditional_date(&instances->next_instance_date,
                                 next_occur_date_buf, MAX_DATE_LENGTH);

        gtk_tree_store_append(model->orig, &iter, NULL);
        gtk_tree_store_set(model->orig, &iter,
                           SXLTMA_COL_NAME, xaccSchedXactionGetName(instances->sx),
                           SXLTMA_COL_ENABLED, xaccSchedXactionGetEnabled(instances->sx),
                           SXLTMA_COL_FREQUENCY, frequency_str,
                           SXLTMA_COL_LAST_OCCUR, last_occur_date_buf,
                           SXLTMA_COL_NEXT_OCCUR, next_occur_date_buf,
                           -1);
        g_free(frequency_str);
    }
}

static void
gsltma_added_cb(GncSxInstanceModel *instances, SchedXaction *sx_added, gpointer user_data)
{
    GncSxListTreeModelAdapter *model = GNC_SX_LIST_TREE_MODEL_ADAPTER(user_data);
    gtk_tree_store_clear(model->orig);
    gsltma_populate_tree_store(model);
}

static void
gsltma_updated_cb(GncSxInstanceModel *instances, SchedXaction *sx_updated, gpointer user_data)
{
    GncSxListTreeModelAdapter *model = GNC_SX_LIST_TREE_MODEL_ADAPTER(user_data);
    gnc_sx_instance_model_update_sx_instances(instances, sx_updated);
    gtk_tree_store_clear(model->orig);
    gsltma_populate_tree_store(model);
}

static void
gsltma_removing_cb(GncSxInstanceModel *instances, SchedXaction *sx_removing, gpointer user_data)
{
    GncSxListTreeModelAdapter *model = GNC_SX_LIST_TREE_MODEL_ADAPTER(user_data);
    gnc_sx_instance_model_remove_sx_instances(instances, sx_removing);
    gtk_tree_store_clear(model->orig);
    gsltma_populate_tree_store(model);
}

GncSxListTreeModelAdapter*
gnc_sx_list_tree_model_adapter_new(GncSxInstanceModel *instances)
{
    GncSxListTreeModelAdapter *rtn;

    rtn = GNC_SX_LIST_TREE_MODEL_ADAPTER(g_object_new(GNC_TYPE_SX_LIST_TREE_MODEL_ADAPTER, NULL));
    rtn->instances = instances;
    g_object_ref(G_OBJECT(rtn->instances));

    gsltma_populate_tree_store(rtn);

    g_signal_connect(G_OBJECT(rtn->instances), "added", (GCallback)gsltma_added_cb, (gpointer)rtn);
    g_signal_connect(G_OBJECT(rtn->instances), "updated", (GCallback)gsltma_updated_cb, (gpointer)rtn);
    g_signal_connect(G_OBJECT(rtn->instances), "removing", (GCallback)gsltma_removing_cb, (gpointer)rtn);

    return rtn;
}

GncSxInstances*
gsltma_get_sx_instances_from_orig_iter(GncSxListTreeModelAdapter *model, GtkTreeIter *orig_iter)
{
    GtkTreePath *path;
    gint *indices;
    gint index;

    path = gtk_tree_model_get_path(GTK_TREE_MODEL(model->orig), orig_iter);
    if (gtk_tree_path_get_depth(path) > 1)
    {
        gtk_tree_path_free(path);
        return NULL;
    }
    indices = gtk_tree_path_get_indices(path);
    index = indices[0];

    gtk_tree_path_free(path);
    return (GncSxInstances*)g_list_nth_data(model->instances->sx_instance_list, index);
}

GncSxInstances*
gnc_sx_list_tree_model_adapter_get_sx_instances(GncSxListTreeModelAdapter *model, GtkTreeIter *sort_iter)
{
    GtkTreeIter translated_iter;
    gtk_tree_model_sort_convert_iter_to_child_iter(model->real,
                                                   &translated_iter,
                                                   sort_iter);
    return gsltma_get_sx_instances_from_orig_iter(model, &translated_iter);
}

static void
gnc_sx_list_tree_model_adapter_dispose(GObject *obj)
{
    GncSxListTreeModelAdapter *adapter;

    g_return_if_fail(obj != NULL);
    adapter = GNC_SX_LIST_TREE_MODEL_ADAPTER(obj);

    if (adapter->disposed) return;
    adapter->disposed = TRUE;

    g_object_unref(G_OBJECT(adapter->instances));
    adapter->instances = NULL;
    g_object_unref(G_OBJECT(adapter->real));
    adapter->real = NULL;
    g_object_unref(G_OBJECT(adapter->orig));
    adapter->orig = NULL;

    G_OBJECT_CLASS(parent_class)->dispose(obj);
}

static void
gnc_sx_list_tree_model_adapter_finalize(GObject *obj)
{
    g_return_if_fail(obj != NULL);
    G_OBJECT_CLASS(parent_class)->finalize(obj);
}
