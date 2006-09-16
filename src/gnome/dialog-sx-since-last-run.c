/********************************************************************\
 * dialog-sx-since-last-run.c : dialog for scheduled transaction    *
 * since-last-run processing.                                       *
 * Copyright (C) 2006 Joshua Sled <jsled@asynchronous.org>          *
 *                                                                  *
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
\********************************************************************/

#include "config.h"
#include <glib.h>
#include <gtk/gtk.h>
#include <glade/glade-xml.h>

#include "dialog-utils.h"
#include "gnc-plugin-page-sx-list.h"
#include "dialog-sx-since-last-run.h"

typedef struct _GncSxSlrTreeModelAdapter GncSxSlrTreeModelAdapter;

struct _GncSxSinceLastRunDialog
{
     GtkWidget *dialog;
     GncSxInstanceModel *instances;
     GncSxSlrTreeModelAdapter *editing_model;
     GtkTreeView *instance_view;
};

/* ------------------------------------------------------------ */

struct _GncSxSlrTreeModelAdapter
{
     GObject parent;

     /* protected */
     GncSxInstanceModel *instances;
     GtkTreeStore *real;
};

typedef struct _GncSxSlrTreeModelAdapterClass
{
     GObjectClass parent;
} GncSxSlrTreeModelAdapterClass;

GType gnc_sx_slr_tree_model_adapter_get_type(void);
static void gnc_sx_slr_tree_model_adapter_class_init(GncSxSlrTreeModelAdapterClass *klass);
static void gnc_sx_slr_tree_model_adapter_interface_init(gpointer g_iface, gpointer iface_data);
static void gnc_sx_slr_tree_model_adapter_init(GTypeInstance *instance, gpointer klass);
GncSxSlrTreeModelAdapter* gnc_sx_slr_tree_model_adapter_new(GncSxInstanceModel *instances);

GncSxInstances* gnc_sx_slr_tree_model_adapter_get_sx_instances(GncSxSlrTreeModelAdapter *model, GtkTreeIter *iter);

#define GNC_TYPE_SX_SLR_TREE_MODEL_ADAPTER	      (gnc_sx_slr_tree_model_adapter_get_type ())
#define GNC_SX_SLR_TREE_MODEL_ADAPTER(obj)	      (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_SX_SLR_TREE_MODEL_ADAPTER, GncSxSlrTreeModelAdapter))
#define GNC_SX_SLR_TREE_MODEL_ADAPTER_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_SX_SLR_TREE_MODEL_ADAPTER, GncSxSlrTreeModelAdapterClass))
#define GNC_IS_SX_SLR_TREE_MODEL_ADAPTER(obj)	      (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_SX_SLR_TREE_MODEL_ADAPTER))
#define GNC_IS_SX_SLR_TREE_MODEL_ADAPTER_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_SX_SLR_TREE_MODEL_ADAPTER))
#define GNC_SX_SLR_TREE_MODEL_ADAPTER_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_SX_SLR_TREE_MODEL_ADAPTER, GncSxSlrTreeModelAdapterClass))

/* ------------------------------------------------------------ */

static void _cell_visibility_func(GtkTreeViewColumn *tree_column,
                                  GtkCellRenderer *cell,
                                  GtkTreeModel *tree_model,
                                  GtkTreeIter *iter,
                                  gpointer data);

GType
gnc_sx_slr_tree_model_adapter_get_type(void)
{
     static GType type = 0;
     if (type == 0) {
          static const GTypeInfo info = {
               sizeof (GncSxSlrTreeModelAdapterClass),
               NULL,   /* base_init */
               NULL,   /* base_finalize */
               (GClassInitFunc)gnc_sx_slr_tree_model_adapter_class_init,   /* class_init */
               NULL,   /* class_finalize */
               NULL,   /* class_data */
               sizeof (GncSxSlrTreeModelAdapter),
               0,      /* n_preallocs */
               (GInstanceInitFunc)gnc_sx_slr_tree_model_adapter_init    /* instance_init */
          };
          static const GInterfaceInfo itreeModel_info = {
               (GInterfaceInitFunc) gnc_sx_slr_tree_model_adapter_interface_init,    /* interface_init */
               NULL,               /* interface_finalize */
               NULL                /* interface_data */
          };

          type = g_type_register_static (G_TYPE_OBJECT,
                                         "GncSxSlrTreeModelAdapterType",
                                         &info, 0);
          g_type_add_interface_static(type,
                                      GTK_TYPE_TREE_MODEL,
                                      &itreeModel_info);
     }
     return type;
}

static void
gnc_sx_slr_tree_model_adapter_class_init(GncSxSlrTreeModelAdapterClass *klass)
{
     ; /* nop */
}

static GtkTreeModelFlags
gsslrtma_get_flags(GtkTreeModel *tree_model)
{
     return gtk_tree_model_get_flags(GTK_TREE_MODEL(GNC_SX_SLR_TREE_MODEL_ADAPTER(tree_model)->real));
}

static gint
gsslrtma_get_n_columns(GtkTreeModel *tree_model)
{
     return gtk_tree_model_get_n_columns(GTK_TREE_MODEL(GNC_SX_SLR_TREE_MODEL_ADAPTER(tree_model)->real));
}

static GType
gsslrtma_get_column_type(GtkTreeModel *tree_model, gint index)
{
     return gtk_tree_model_get_column_type(GTK_TREE_MODEL(GNC_SX_SLR_TREE_MODEL_ADAPTER(tree_model)->real), index);
}

static gboolean
gsslrtma_get_iter(GtkTreeModel *tree_model,
                GtkTreeIter *iter,
                GtkTreePath *path)
{
     return gtk_tree_model_get_iter(GTK_TREE_MODEL(GNC_SX_SLR_TREE_MODEL_ADAPTER(tree_model)->real), iter, path);
}

static GtkTreePath*
gsslrtma_get_path(GtkTreeModel *tree_model,
                GtkTreeIter *iter)
{
     return gtk_tree_model_get_path(GTK_TREE_MODEL(GNC_SX_SLR_TREE_MODEL_ADAPTER(tree_model)->real), iter);
}

static void
gsslrtma_get_value(GtkTreeModel *tree_model,
                 GtkTreeIter *iter,
                 gint column,
                 GValue *value)
{
     gtk_tree_model_get_value(GTK_TREE_MODEL(GNC_SX_SLR_TREE_MODEL_ADAPTER(tree_model)->real), iter, column, value);
}

static gboolean
gsslrtma_iter_next(GtkTreeModel *tree_model,
                 GtkTreeIter *iter)
{
     return gtk_tree_model_iter_next(GTK_TREE_MODEL(GNC_SX_SLR_TREE_MODEL_ADAPTER(tree_model)->real), iter);
}

static gboolean
gsslrtma_iter_children(GtkTreeModel *tree_model,
                     GtkTreeIter *iter,
                     GtkTreeIter *parent)
{
     return gtk_tree_model_iter_children(GTK_TREE_MODEL(GNC_SX_SLR_TREE_MODEL_ADAPTER(tree_model)->real), iter, parent);
}

static gboolean
gsslrtma_iter_has_child(GtkTreeModel *tree_model,
                      GtkTreeIter *iter)
{
     return gtk_tree_model_iter_has_child(GTK_TREE_MODEL(GNC_SX_SLR_TREE_MODEL_ADAPTER(tree_model)->real), iter);
}

static gint
gsslrtma_iter_n_children(GtkTreeModel *tree_model,
                       GtkTreeIter *iter)
{
     return gtk_tree_model_iter_n_children(GTK_TREE_MODEL(GNC_SX_SLR_TREE_MODEL_ADAPTER(tree_model)->real), iter);
}

static gboolean
gsslrtma_iter_nth_child(GtkTreeModel *tree_model,
                      GtkTreeIter *iter,
                      GtkTreeIter *parent,
                      gint n)
{
     return gtk_tree_model_iter_nth_child(GTK_TREE_MODEL(GNC_SX_SLR_TREE_MODEL_ADAPTER(tree_model)->real), iter, parent, n);
}

static gboolean
gsslrtma_iter_parent(GtkTreeModel *tree_model,
                   GtkTreeIter *iter,
                   GtkTreeIter *child)
{
     return gtk_tree_model_iter_parent(GTK_TREE_MODEL(GNC_SX_SLR_TREE_MODEL_ADAPTER(tree_model)->real), iter, child);
}

static void
gsslrtma_ref_node(GtkTreeModel *tree_model,
                GtkTreeIter *iter)
{
     gtk_tree_model_ref_node(GTK_TREE_MODEL(GNC_SX_SLR_TREE_MODEL_ADAPTER(tree_model)->real), iter);
}

static void
gsslrtma_unref_node(GtkTreeModel *tree_model,
                  GtkTreeIter *iter)
{
     gtk_tree_model_unref_node(GTK_TREE_MODEL(GNC_SX_SLR_TREE_MODEL_ADAPTER(tree_model)->real), iter);
}

static void
gnc_sx_slr_tree_model_adapter_interface_init(gpointer g_iface, gpointer iface_data)
{
     GtkTreeModelIface *tree_model = (GtkTreeModelIface*)g_iface;
     tree_model->get_flags = gsslrtma_get_flags;
     tree_model->get_n_columns = gsslrtma_get_n_columns;
     tree_model->get_column_type = gsslrtma_get_column_type;
     tree_model->get_iter = gsslrtma_get_iter;
     tree_model->get_path = gsslrtma_get_path;
     tree_model->get_value = gsslrtma_get_value;
     tree_model->iter_next = gsslrtma_iter_next;
     tree_model->iter_children = gsslrtma_iter_children;
     tree_model->iter_has_child = gsslrtma_iter_has_child;
     tree_model->iter_n_children = gsslrtma_iter_n_children;
     tree_model->iter_nth_child = gsslrtma_iter_nth_child;
     tree_model->iter_parent = gsslrtma_iter_parent;
     tree_model->ref_node = gsslrtma_ref_node;
     tree_model->unref_node = gsslrtma_unref_node;
}

static void
gsslrtma_proxy_row_changed(GtkTreeModel *treemodel,
                         GtkTreePath *arg1,
                         GtkTreeIter *arg2,
                         gpointer user_data)
{
     g_signal_emit_by_name(user_data, "row-changed", arg1, arg2);
}

static void
gsslrtma_proxy_row_deleted(GtkTreeModel *treemodel,
                         GtkTreePath *arg1,
                         gpointer user_data)
{
     g_signal_emit_by_name(user_data, "row-deleted", arg1);
}

static void
gsslrtma_proxy_row_has_child_toggled(GtkTreeModel *treemodel,
                                   GtkTreePath *arg1,
                                   GtkTreeIter *arg2,
                                   gpointer user_data)
{
     g_signal_emit_by_name(user_data, "row-has-child-toggled", arg1, arg2);
}

static void
gsslrtma_proxy_row_inserted(GtkTreeModel *treemodel,
                          GtkTreePath *arg1,
                          GtkTreeIter *arg2,
                          gpointer user_data)
{
     g_signal_emit_by_name(user_data, "row-inserted", arg1, arg2);
}

static void
gsslrtma_proxy_rows_reordered(GtkTreeModel *treemodel,
                            GtkTreePath *arg1,
                            GtkTreeIter *arg2,
                            gpointer arg3,
                            gpointer user_data)
{
     g_signal_emit_by_name(user_data, "rows-reordered", arg1, arg2, arg3);
}

static void
gnc_sx_slr_tree_model_adapter_init(GTypeInstance *instance, gpointer klass)
{
     GncSxSlrTreeModelAdapter *adapter = GNC_SX_SLR_TREE_MODEL_ADAPTER(instance);
     // columns:    thing-name, instance-state, variable-value
     // at depth=0: <sx>,       N/A,            N/A
     // at depth=1: <instance>, <state>,        N/A
     // at depth=2: <variable>, N/A,            <value>
     adapter->real = gtk_tree_store_new(3, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING);

     g_signal_connect(adapter->real, "row-changed", G_CALLBACK(gsslrtma_proxy_row_changed), adapter);
     g_signal_connect(adapter->real, "row-deleted", G_CALLBACK(gsslrtma_proxy_row_deleted), adapter);
     g_signal_connect(adapter->real, "row-has-child-toggled", G_CALLBACK(gsslrtma_proxy_row_has_child_toggled), adapter);
     g_signal_connect(adapter->real, "row-inserted", G_CALLBACK(gsslrtma_proxy_row_inserted), adapter);
     g_signal_connect(adapter->real, "rows-reordered", G_CALLBACK(gsslrtma_proxy_rows_reordered), adapter);
}

static void
_build_variable_name_list(gpointer key, gpointer value, gpointer user_data)
{
     GList **name_list = (GList**)user_data;
     *name_list = g_list_append(*name_list, key);
}

/* @@fixme: i18n. **/
/* @@fixme: non-staticize. **/
static char* gnc_sx_instance_type_names[] = {
     ("Ignored"),
     ("Postponed"),
     ("To-Create"),
     ("Reminder"),
     NULL
};

static void
gsslrtma_populate_tree_store(GncSxSlrTreeModelAdapter *model)
{
     GtkTreeIter sx_iter;
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

          gtk_tree_store_append(model->real, &sx_iter, NULL);
          gtk_tree_store_set(model->real, &sx_iter,
                             0, xaccSchedXactionGetName(instances->sx),
                             1, NULL,
                             2, NULL,
                             -1);
          g_string_free(frequency_str, TRUE);

          // Insert instance information
          {
               GList *inst_iter;
               GtkTreeIter inst_tree_iter;
               char instance_date_buf[MAX_DATE_LENGTH+1];

               for (inst_iter = instances->list; inst_iter != NULL; inst_iter = inst_iter->next)
               {
                    GncSxInstance *inst = (GncSxInstance*)inst_iter->data;
                    qof_print_gdate(instance_date_buf, MAX_DATE_LENGTH, &inst->date);
                    gtk_tree_store_append(model->real, &inst_tree_iter, &sx_iter);
                    gtk_tree_store_set(model->real, &inst_tree_iter,
                                       0, instance_date_buf,
                                       1, gnc_sx_instance_type_names[inst->type],
                                       2, NULL,
                                       -1);

                    // Insert variable information
                    {
                         GList *names = NULL, *names_iter;
                         GtkTreeIter var_iter;

                         g_hash_table_foreach(inst->variable_bindings, _build_variable_name_list, &names);
                         for (names_iter = names; names_iter != NULL; names_iter = names_iter->next)
                         {
                              gtk_tree_store_append(model->real, &var_iter, &inst_tree_iter);
                              gtk_tree_store_set(model->real, &var_iter,
                                                 0, (gchar*)names_iter->data,
                                                 1, NULL,
                                                 2, "(@fixme - value)"
                                                 -1);
                         }
                    }
               }
          }
     }
}

static void
gsslrtma_updated_cb(GncSxInstanceModel *instances, gpointer user_data)
{
     GncSxSlrTreeModelAdapter *model = GNC_SX_SLR_TREE_MODEL_ADAPTER(user_data);
     printf("update\n");
     gtk_tree_store_clear(model->real);
     gsslrtma_populate_tree_store(model);
}

GncSxSlrTreeModelAdapter*
gnc_sx_slr_tree_model_adapter_new(GncSxInstanceModel *instances)
{
     GncSxSlrTreeModelAdapter *rtn;
     rtn = GNC_SX_SLR_TREE_MODEL_ADAPTER(g_object_new(GNC_TYPE_SX_SLR_TREE_MODEL_ADAPTER, NULL));
     rtn->instances = instances;
     gsslrtma_populate_tree_store(rtn);
     g_signal_connect(G_OBJECT(rtn->instances), "updated", (GCallback)gsslrtma_updated_cb, (gpointer)rtn);
     return rtn;
}


void
gnc_sx_sxsincelast_book_opened(void)
{
     // Get the instance model
     // check for mumble and futz
     // maybe create dialog.
     printf("not ready\n");
     //gnc_ui_sxsincelast_dialog_create();
}

gint
gnc_ui_sxsincelast_dialog_create(void)
{
     GDate now;
     GncSxInstanceModel *model;
     g_date_clear(&now, 1);
     g_date_set_time_t(&now, time(NULL));
     model = gnc_sx_get_instances(&now);
     gnc_ui_sx_since_last_run_dialog(model);
     return 1;
}

GncSxSinceLastRunDialog*
gnc_ui_sx_since_last_run_dialog(GncSxInstanceModel *model)
{
     GncSxSinceLastRunDialog *dialog;
     GladeXML *glade;

     dialog = g_new0(GncSxSinceLastRunDialog, 1);
     dialog->instances = model;

     glade = gnc_glade_xml_new("sched-xact.glade", "since-last-run-dialog");
     dialog->dialog = glade_xml_get_widget(glade, "since-last-run-dialog");

     dialog->editing_model = gnc_sx_slr_tree_model_adapter_new(model);

     {
          GtkCellRenderer *renderer;
          GtkTreeViewColumn *col;
          int position = -1;
          
          dialog->instance_view = GTK_TREE_VIEW(glade_xml_get_widget(glade, "instance_view"));
          gtk_tree_view_set_model(dialog->instance_view, GTK_TREE_MODEL(dialog->editing_model));

          renderer = gtk_cell_renderer_text_new();
          col = gtk_tree_view_column_new_with_attributes("SX, Instance, Variable", renderer, "text", ++position, NULL);
          gtk_tree_view_append_column(dialog->instance_view, col);

          renderer = gtk_cell_renderer_text_new();
          col = gtk_tree_view_column_new_with_attributes("Instance State", renderer, "text", ++position, NULL);
          gtk_tree_view_column_set_cell_data_func(col, renderer, _cell_visibility_func, GINT_TO_POINTER(position), NULL);
          gtk_tree_view_append_column(dialog->instance_view, col);

          renderer = gtk_cell_renderer_text_new();
          col = gtk_tree_view_column_new_with_attributes("Variable Value", renderer, "text", ++position, NULL);
          gtk_tree_view_column_set_cell_data_func(col, renderer, _cell_visibility_func, GINT_TO_POINTER(position), NULL);
          gtk_tree_view_append_column(dialog->instance_view, col);
     }

     gtk_widget_show_all(dialog->dialog);

     return dialog;
}


static void
_cell_visibility_func(GtkTreeViewColumn *tree_column,
                      GtkCellRenderer *cell,
                      GtkTreeModel *tree_model,
                      GtkTreeIter *iter,
                      gpointer data)
{
     GtkTreePath *path;
     int select_depth, path_depth;

     select_depth = GPOINTER_TO_INT(data);
     path = gtk_tree_model_get_path(tree_model, iter);
     path_depth = gtk_tree_path_get_depth(path);
     // printf("item depth: %d\n", path_depth);
     g_object_set(G_OBJECT(cell), "visible", path_depth == select_depth ? TRUE : FALSE, NULL);
}

