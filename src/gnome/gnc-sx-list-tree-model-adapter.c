/* 
 * gnc-sx-list-tree-model-adapter.c
 *
 * Copyright (C) 2006 Josh Sled <jsled@asynchronous.org>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2 of the GNU General Public
 * License as published by the Free Software Foundation.
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
     GtkTreeStore *real;
};

struct _GncSxListTreeModelAdapterClass
{
     GObjectClass parent;
};

static GObjectClass *parent_class = NULL;

static void gnc_sx_list_tree_model_adapter_class_init(GncSxListTreeModelAdapterClass *klass);
static void gnc_sx_list_tree_model_adapter_interface_init(gpointer g_iface, gpointer iface_data);
static void gnc_sx_list_tree_model_adapter_init(GTypeInstance *instance, gpointer klass);
static void gnc_sx_list_tree_model_adapter_dispose(GObject *obj);
static void gnc_sx_list_tree_model_adapter_finalize(GObject *obj);

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
          static const GInterfaceInfo itreeModel_info = {
               (GInterfaceInitFunc) gnc_sx_list_tree_model_adapter_interface_init,    /* interface_init */
               NULL,               /* interface_finalize */
               NULL                /* interface_data */
          };

          type = g_type_register_static (G_TYPE_OBJECT,
                                         "GncSxListTreeModelAdapterType",
                                         &info, 0);
          g_type_add_interface_static(type,
                                      GTK_TYPE_TREE_MODEL,
                                      &itreeModel_info);
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
gnc_sx_list_tree_model_adapter_interface_init(gpointer g_iface, gpointer iface_data)
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
gnc_sx_list_tree_model_adapter_init(GTypeInstance *instance, gpointer klass)
{
     GncSxListTreeModelAdapter *adapter = GNC_SX_LIST_TREE_MODEL_ADAPTER(instance);
     adapter->real = gtk_tree_store_new(4, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING);

     g_signal_connect(adapter->real, "row-changed", G_CALLBACK(gsltma_proxy_row_changed), adapter);
     g_signal_connect(adapter->real, "row-deleted", G_CALLBACK(gsltma_proxy_row_deleted), adapter);
     g_signal_connect(adapter->real, "row-has-child-toggled", G_CALLBACK(gsltma_proxy_row_has_child_toggled), adapter);
     g_signal_connect(adapter->real, "row-inserted", G_CALLBACK(gsltma_proxy_row_inserted), adapter);
     g_signal_connect(adapter->real, "rows-reordered", G_CALLBACK(gsltma_proxy_rows_reordered), adapter);
}

static void
gsltma_populate_tree_store(GncSxListTreeModelAdapter *model)
{
     GtkTreeIter iter;
     GList *list;

     for (list = model->instances->sx_instance_list; list != NULL; list = list->next)
     {
          GncSxInstances *instances = (GncSxInstances*)list->data;
          FreqSpec *fs;
          GString *frequency_str;
          char last_occur_date_buf[MAX_DATE_LENGTH+1];
          char next_occur_date_buf[MAX_DATE_LENGTH+1];

          frequency_str = g_string_sized_new(32);
          fs = xaccSchedXactionGetFreqSpec(instances->sx);
          xaccFreqSpecGetFreqStr(fs, frequency_str);

          {
               GDate *last_occur = xaccSchedXactionGetLastOccurDate(instances->sx);
               if (last_occur == NULL || !g_date_valid(last_occur))
               {
                    g_stpcpy(last_occur_date_buf, "never");
               }
               else
               {
                    qof_print_gdate(last_occur_date_buf,
                                    MAX_DATE_LENGTH,
                                    last_occur);
               }
          }

          qof_print_gdate(next_occur_date_buf, MAX_DATE_LENGTH, &instances->next_instance_date);

          gtk_tree_store_append(model->real, &iter, NULL);
          gtk_tree_store_set(model->real, &iter,
                             0, xaccSchedXactionGetName(instances->sx),
                             1, frequency_str->str,
                             2, last_occur_date_buf,
                             3, next_occur_date_buf,
                             -1);
          g_string_free(frequency_str, TRUE);
     }
}

static void
gsltma_added_cb(GncSxInstanceModel *instances, SchedXaction *sx_added, gpointer user_data)
{
     GncSxListTreeModelAdapter *model = GNC_SX_LIST_TREE_MODEL_ADAPTER(user_data);
     printf("added\n");
     gtk_tree_store_clear(model->real);
     gsltma_populate_tree_store(model);
}

static void
gsltma_updated_cb(GncSxInstanceModel *instances, SchedXaction *sx_updated, gpointer user_data)
{
     GncSxListTreeModelAdapter *model = GNC_SX_LIST_TREE_MODEL_ADAPTER(user_data);
     printf("sx list tree model adapter update\n");
     gnc_sx_instance_model_update_sx_instances(instances, sx_updated);
     gtk_tree_store_clear(model->real);
     gsltma_populate_tree_store(model);
}

static void
gsltma_removing_cb(GncSxInstanceModel *instances, SchedXaction *sx_removing, gpointer user_data)
{
     GncSxListTreeModelAdapter *model = GNC_SX_LIST_TREE_MODEL_ADAPTER(user_data);
     printf("removing\n");
     gnc_sx_instance_model_remove_sx_instance(instances, sx_removing);
     gtk_tree_store_clear(model->real);
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
gnc_sx_list_tree_model_adapter_get_sx_instances(GncSxListTreeModelAdapter *model, GtkTreeIter *iter)
{
     GtkTreePath *path;
     gint *indices;
     gint index;

     path = gtk_tree_model_get_path(GTK_TREE_MODEL(model), iter);
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

static void
gnc_sx_list_tree_model_adapter_dispose(GObject *obj)
{
     GncSxListTreeModelAdapter *adapter;

     g_return_if_fail(obj != NULL);
     adapter = GNC_SX_LIST_TREE_MODEL_ADAPTER(obj);
     g_return_if_fail(adapter->disposed);
     adapter->disposed = TRUE;

     g_object_unref(G_OBJECT(adapter->instances));
     adapter->instances = NULL;
     g_object_unref(G_OBJECT(adapter->real));
     adapter->real = NULL;

     G_OBJECT_CLASS(parent_class)->dispose(obj);
}

static void
gnc_sx_list_tree_model_adapter_finalize(GObject *obj)
{
     g_return_if_fail(obj != NULL);
     G_OBJECT_CLASS(parent_class)->finalize(obj);
}
