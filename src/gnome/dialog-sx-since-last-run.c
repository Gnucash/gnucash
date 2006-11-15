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
#include "gnc-exp-parser.h"
#include "gnc-sx-instance-model.h"
#include "dialog-sx-since-last-run.h"

#include "gnc-ui-util.h"
#include "Split.h"
#include "Transaction.h"
#include "Account.h"
#include "Scrub.h"
#include "Query.h"
#include "QueryNew.h"
#include "gnc-ledger-display.h"
#include "gnc-plugin-page-register.h"
#include "gnc-main-window.h"
#include "gnc-component-manager.h"
#include "gnc-gconf-utils.h"
#include "gnc-gui-query.h"

static QofLogModule log_module = GNC_MOD_GUI;

#define GCONF_SECTION "dialogs/scheduled_trans/since_last_run"

//typedef struct _GncSxSlrTreeModelAdapter GncSxSlrTreeModelAdapter;

struct _GncSxSinceLastRunDialog
{
     GtkWidget *dialog;
     GncSxSlrTreeModelAdapter *editing_model;
     GtkTreeView *instance_view;
     GtkToggleButton *review_created_txns_toggle;
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
static GncSxInstances* _gnc_sx_slr_tree_model_adapter_get_sx_instances(GncSxSlrTreeModelAdapter *model, GtkTreeIter *iter, gboolean check_depth);
/** @return null if the iter is not actually an GncSxInstance. **/
GncSxInstance* gnc_sx_slr_model_get_instance(GncSxSlrTreeModelAdapter *model, GtkTreeIter *iter);
static GncSxInstance* _gnc_sx_slr_model_get_instance(GncSxSlrTreeModelAdapter *model, GtkTreeIter *iter, gboolean check_depth);
/** @return false if the iter is not actaully an GncSxInstance's variable. **/
gboolean gnc_sx_slr_model_get_instance_and_variable(GncSxSlrTreeModelAdapter *model, GtkTreeIter *iter, GncSxInstance **instance_loc, GncSxVariable **var_loc);

void gnc_sx_slr_model_summarize(GncSxSlrTreeModelAdapter *model, GncSxSlrSummary *summary);

void gnc_sx_slr_model_change_instance_state(GncSxSlrTreeModelAdapter *model, GncSxInstance *instance, GncSxInstanceState new_state);
void gnc_sx_slr_model_change_variable(GncSxSlrTreeModelAdapter *model, GncSxInstance *instance, GncSxVariable *variable, gnc_numeric *new_value);

void gnc_sx_slr_model_effect_change(GncSxSlrTreeModelAdapter *model, gboolean auto_create_only, GList **created_transaction_guids, GList **creation_errors);

GtkTreeModel* gnc_sx_get_slr_state_model(void);

typedef struct _GncSxSlrVariableNeeded
{
     GncSxInstance *instance;
     GncSxVariable *variable;
} GncSxSlrVariableNeeded;

/**
 * @return Caller-owned GList<GncSxSlrVariableNeeded*>.
 **/
GList* gnc_sx_slr_model_check_variables(GncSxSlrTreeModelAdapter *model);

#define GNC_TYPE_SX_SLR_TREE_MODEL_ADAPTER	      (gnc_sx_slr_tree_model_adapter_get_type ())
#define GNC_SX_SLR_TREE_MODEL_ADAPTER(obj)	      (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_SX_SLR_TREE_MODEL_ADAPTER, GncSxSlrTreeModelAdapter))
#define GNC_SX_SLR_TREE_MODEL_ADAPTER_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_SX_SLR_TREE_MODEL_ADAPTER, GncSxSlrTreeModelAdapterClass))
#define GNC_IS_SX_SLR_TREE_MODEL_ADAPTER(obj)	      (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_SX_SLR_TREE_MODEL_ADAPTER))
#define GNC_IS_SX_SLR_TREE_MODEL_ADAPTER_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_SX_SLR_TREE_MODEL_ADAPTER))
#define GNC_SX_SLR_TREE_MODEL_ADAPTER_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_SX_SLR_TREE_MODEL_ADAPTER, GncSxSlrTreeModelAdapterClass))

/* ------------------------------------------------------------ */

static void _show_created_transactions(GncSxSinceLastRunDialog *app_dialog, GList *created_txn_guids);

static void dialog_response_cb(GtkDialog *dialog, gint response_id, GncSxSinceLastRunDialog *app_dialog);

/* ------------------------------------------------------------ */

static void
_var_numeric_to_string(gnc_numeric *value, GString **str)
{
     *str = g_string_sized_new(5);
     g_string_printf(*str, "%0.2f", gnc_numeric_to_double(*value));
}

/* ------------------------------------------------------------ */

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

// model columns
enum {
     SLR_MODEL_COL_NAME = 0,
     SLR_MODEL_COL_INSTANCE_STATE,
     SLR_MODEL_COL_VARAIBLE_VALUE,
     SLR_MODEL_COL_INSTANCE_VISIBILITY,
     SLR_MODEL_COL_VARIABLE_VISIBILITY,
     SLR_MODEL_COL_INSTANCE_STATE_SENSITIVITY,
};

static void
gnc_sx_slr_tree_model_adapter_init(GTypeInstance *instance, gpointer klass)
{
     GncSxSlrTreeModelAdapter *adapter = GNC_SX_SLR_TREE_MODEL_ADAPTER(instance);
     // columns:    thing-name, instance-state, variable-value, instance-visible, variable-visible, instance_state_sensitivity
     // at depth=0: <sx>,       N/A,            N/A,            N/A               N/A,              N/A
     // at depth=1: <instance>, <state>,        N/A,            <valid>,          N/A,              <valid>
     // at depth=2: <variable>, N/A,            <value>,        N/A,              <valid>,          N/A
     adapter->real = gtk_tree_store_new(6, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_BOOLEAN, G_TYPE_BOOLEAN, G_TYPE_BOOLEAN);

     g_signal_connect(adapter->real, "row-changed", G_CALLBACK(gsslrtma_proxy_row_changed), adapter);
     g_signal_connect(adapter->real, "row-deleted", G_CALLBACK(gsslrtma_proxy_row_deleted), adapter);
     g_signal_connect(adapter->real, "row-has-child-toggled", G_CALLBACK(gsslrtma_proxy_row_has_child_toggled), adapter);
     g_signal_connect(adapter->real, "row-inserted", G_CALLBACK(gsslrtma_proxy_row_inserted), adapter);
     g_signal_connect(adapter->real, "rows-reordered", G_CALLBACK(gsslrtma_proxy_rows_reordered), adapter);
}

/* @@fixme: i18n. **/
/* @@fixme: non-staticize. **/
static char* gnc_sx_instance_state_names[] = {
     ("Ignored"),
     ("Postponed"),
     ("To-Create"),
     ("Reminder"),
     ("Created"),
     NULL
};

static GtkTreeModel* _singleton_slr_state_model = NULL;

GtkTreeModel*
gnc_sx_get_slr_state_model(void)
{
     if (_singleton_slr_state_model == NULL)
     {
          int i;
          GtkTreeIter iter;

          _singleton_slr_state_model = GTK_TREE_MODEL(gtk_list_store_new(1, G_TYPE_STRING));
          for (i = 0; i != SX_INSTANCE_STATE_CREATED; i++)
          {
               gtk_list_store_insert_with_values(GTK_LIST_STORE(_singleton_slr_state_model),
                                                 &iter,
                                                 SX_INSTANCE_STATE_MAX_STATE + 1,
                                                 0, gnc_sx_instance_state_names[i], -1);
          }
     }
     return _singleton_slr_state_model;
}

static void
gsslrtma_populate_tree_store(GncSxSlrTreeModelAdapter *model)
{
     GtkTreeIter sx_tree_iter;
     GList *sx_iter;

     for (sx_iter = model->instances->sx_instance_list; sx_iter != NULL; sx_iter = sx_iter->next)
     {
          GncSxInstances *instances = (GncSxInstances*)sx_iter->data;
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

          gtk_tree_store_append(model->real, &sx_tree_iter, NULL);
          gtk_tree_store_set(model->real, &sx_tree_iter,
                             SLR_MODEL_COL_NAME, xaccSchedXactionGetName(instances->sx),
                             SLR_MODEL_COL_INSTANCE_STATE, NULL,
                             SLR_MODEL_COL_VARAIBLE_VALUE, NULL,
                             SLR_MODEL_COL_INSTANCE_VISIBILITY, FALSE,
                             SLR_MODEL_COL_VARIABLE_VISIBILITY, FALSE,
                             SLR_MODEL_COL_INSTANCE_STATE_SENSITIVITY, FALSE,
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
                    gtk_tree_store_append(model->real, &inst_tree_iter, &sx_tree_iter);
                    gtk_tree_store_set(model->real, &inst_tree_iter,
                                       SLR_MODEL_COL_NAME, instance_date_buf,
                                       SLR_MODEL_COL_INSTANCE_STATE, gnc_sx_instance_state_names[inst->state],
                                       SLR_MODEL_COL_VARAIBLE_VALUE, NULL,
                                       SLR_MODEL_COL_INSTANCE_VISIBILITY, TRUE,
                                       SLR_MODEL_COL_VARIABLE_VISIBILITY, FALSE,
                                       SLR_MODEL_COL_INSTANCE_STATE_SENSITIVITY, inst->state != SX_INSTANCE_STATE_CREATED,
                                       -1);

                    // Insert variable information
                    {
                         GList *vars = NULL, *var_iter;
                         GtkTreeIter var_tree_iter;

                         vars = gnc_sx_instance_get_variables(inst);
                         for (var_iter = vars; var_iter != NULL; var_iter = var_iter->next)
                         {
                              GncSxVariable *var = (GncSxVariable*)var_iter->data;
                              GString *tmp_str;
                              if (gnc_numeric_check(var->value) == GNC_ERROR_OK)
                              {
                                   _var_numeric_to_string(&var->value, &tmp_str);
                              }
                              else
                              {
                                   tmp_str = g_string_new("(need value)");
                              }
                              gtk_tree_store_append(model->real, &var_tree_iter, &inst_tree_iter);
                              gtk_tree_store_set(model->real, &var_tree_iter,
                                                 SLR_MODEL_COL_NAME, var->name,
                                                 SLR_MODEL_COL_INSTANCE_STATE, NULL,
                                                 SLR_MODEL_COL_VARAIBLE_VALUE, tmp_str->str,
                                                 SLR_MODEL_COL_INSTANCE_VISIBILITY, FALSE,
                                                 SLR_MODEL_COL_VARIABLE_VISIBILITY, TRUE,
                                                 SLR_MODEL_COL_INSTANCE_STATE_SENSITIVITY, FALSE
                                                 -1);
                              g_string_free(tmp_str, TRUE);
                         }
                         g_list_free(vars);
                    }
               }
          }
     }
}

GncSxInstances*
gnc_sx_slr_tree_model_adapter_get_sx_instances(GncSxSlrTreeModelAdapter *model, GtkTreeIter *iter)
{
     return _gnc_sx_slr_tree_model_adapter_get_sx_instances(model, iter, TRUE);
}

static GncSxInstances*
_gnc_sx_slr_tree_model_adapter_get_sx_instances(GncSxSlrTreeModelAdapter *model, GtkTreeIter *iter, gboolean check_depth)
{
     GtkTreePath *path;
     gint *indices, index;
     path = gtk_tree_model_get_path(GTK_TREE_MODEL(model), iter);
     if (check_depth && gtk_tree_path_get_depth(path) != 1)
     {
          gtk_tree_path_free(path);
          return NULL;
     }
     indices = gtk_tree_path_get_indices(path);
     index = indices[0];
     gtk_tree_path_free(path);

     return (GncSxInstances*)g_list_nth_data(model->instances->sx_instance_list, index);
}

GncSxInstance*
gnc_sx_slr_model_get_instance(GncSxSlrTreeModelAdapter *model, GtkTreeIter *iter)
{
     return _gnc_sx_slr_model_get_instance(model, iter, TRUE);
}

static GncSxInstance*
_gnc_sx_slr_model_get_instance(GncSxSlrTreeModelAdapter *model, GtkTreeIter *iter, gboolean check_depth)
{
     GtkTreePath *path;
     gint *indices, instances_index, instance_index;
     GncSxInstances *instances;
     path = gtk_tree_model_get_path(GTK_TREE_MODEL(model), iter);
     if (check_depth && gtk_tree_path_get_depth(path) != 2)
     {
          gtk_tree_path_free(path);
          return NULL;
     }
     indices = gtk_tree_path_get_indices(path);
     instances_index = indices[0];
     instance_index = indices[1];
     gtk_tree_path_free(path);

     instances = (GncSxInstances*)g_list_nth_data(model->instances->sx_instance_list, instances_index);
     if (instance_index < 0 || instance_index >= g_list_length(instances->list))
     {
          return NULL;
     }

     return (GncSxInstance*)g_list_nth_data(instances->list, instance_index);
}

gboolean
gnc_sx_slr_model_get_instance_and_variable(GncSxSlrTreeModelAdapter *model, GtkTreeIter *iter, GncSxInstance **instance_loc, GncSxVariable **var_loc)
{
     GtkTreePath *path;
     gint *indices, variable_index;
     GncSxInstance *instance;
     GList *variables;

     instance = _gnc_sx_slr_model_get_instance(model, iter, FALSE);
     if (instance == NULL)
     {
          return FALSE;
     }
     variables = gnc_sx_instance_get_variables(instance);

     path = gtk_tree_model_get_path(GTK_TREE_MODEL(model), iter);
     if (gtk_tree_path_get_depth(path) != 3)
     {
          gtk_tree_path_free(path);
          return FALSE;
     }
     indices = gtk_tree_path_get_indices(path);
     variable_index = indices[2];
     gtk_tree_path_free(path);

     if (variable_index < 0 || variable_index >= g_list_length(variables))
     {
          g_list_free(variables);
          return FALSE;
     }

     if (instance_loc != NULL)
     {
          *instance_loc = instance;
     }

     if (var_loc != NULL)
     {
          *var_loc = (GncSxVariable*)g_list_nth_data(variables, variable_index);
     }

     g_list_free(variables);
     return TRUE;
}

void
gnc_sx_slr_model_change_instance_state(GncSxSlrTreeModelAdapter *model, GncSxInstance *instance, GncSxInstanceState new_state)
{
     GtkTreePath *path;
     GtkTreeIter iter;
     GList *inst_iter;
     int indices[2];

     indices[0] = g_list_index(model->instances->sx_instance_list, instance->parent);
     if (indices[0] == -1)
          return;
     indices[1] = g_list_index(instance->parent->list, instance);
     if (indices[1] == -1)
          return;
     path = gtk_tree_path_new_from_indices(indices[0], indices[1], -1);
     gtk_tree_model_get_iter(GTK_TREE_MODEL(model), &iter, path);
     gtk_tree_path_free(path);

     instance->state = new_state;

     gtk_tree_store_set(model->real, &iter,
                        SLR_MODEL_COL_INSTANCE_STATE, gnc_sx_instance_state_names[instance->state],
                        SLR_MODEL_COL_INSTANCE_STATE_SENSITIVITY, instance->state != SX_INSTANCE_STATE_CREATED,
                        -1);

     // ensure 'remind' constraints are met
     inst_iter = g_list_find(instance->parent->list, instance);
     g_assert(inst_iter != NULL);
     if (instance->state != SX_INSTANCE_STATE_REMINDER)
     {
          // iterate backwards, making sure reminders are changed to 'postponed'
          for (inst_iter = inst_iter->prev; inst_iter != NULL; inst_iter = inst_iter->prev)
          {
               GncSxInstance *prev_inst = (GncSxInstance*)inst_iter->data;
               indices[1] -= 1;
               if (prev_inst->state != SX_INSTANCE_STATE_REMINDER)
                    continue;

               prev_inst->state = SX_INSTANCE_STATE_POSTPONED;

               path = gtk_tree_path_new_from_indices(indices[0], indices[1], -1);
               gtk_tree_model_get_iter(GTK_TREE_MODEL(model), &iter, path);
               gtk_tree_path_free(path);
               gtk_tree_store_set(model->real, &iter,
                                  SLR_MODEL_COL_INSTANCE_STATE, gnc_sx_instance_state_names[prev_inst->state],
                                  SLR_MODEL_COL_INSTANCE_STATE_SENSITIVITY, prev_inst->state != SX_INSTANCE_STATE_CREATED,
                                  -1);
          }
     }
     else
     {
          // iterate forward, make sure transactions are set to 'remind'
          for (inst_iter = inst_iter->next; inst_iter != NULL; inst_iter = inst_iter->next)
          {
               GncSxInstance *next_inst = (GncSxInstance*)inst_iter->data;
               indices[1] += 1;
               if (next_inst->state == SX_INSTANCE_STATE_REMINDER)
                    continue;

               next_inst->state = SX_INSTANCE_STATE_REMINDER;

               path = gtk_tree_path_new_from_indices(indices[0], indices[1], -1);
               gtk_tree_model_get_iter(GTK_TREE_MODEL(model), &iter, path);
               gtk_tree_path_free(path);
               gtk_tree_store_set(model->real, &iter,
                                  SLR_MODEL_COL_INSTANCE_STATE, gnc_sx_instance_state_names[next_inst->state],
                                  SLR_MODEL_COL_INSTANCE_STATE_SENSITIVITY, next_inst->state != SX_INSTANCE_STATE_CREATED,
                                  -1);
          }
     }
}

static GtkTreePath*
_get_path_for_variable(GncSxSlrTreeModelAdapter *model, GncSxInstance *instance, GncSxVariable *variable)
{
     GList *variables;
     int indices[3];
     GtkTreePath *path;

     indices[0] = g_list_index(model->instances->sx_instance_list, instance->parent);
     if (indices[0] == -1)
          return NULL;
     indices[1] = g_list_index(instance->parent->list, instance);
     if (indices[1] == -1)
          return NULL;
     variables = gnc_sx_instance_get_variables(instance);
     indices[2] = g_list_index(variables, variable);
     g_list_free(variables);
     if (indices[2] == -1)
          return NULL;
     path = gtk_tree_path_new_from_indices(indices[0], indices[1], indices[2], -1);
     return path;
}

void
gnc_sx_slr_model_change_variable(GncSxSlrTreeModelAdapter *model, GncSxInstance *instance, GncSxVariable *variable, gnc_numeric *new_value)
{
     GtkTreePath *path;
     GtkTreeIter iter;
     GString *tmp_str;

     path = _get_path_for_variable(model, instance, variable);
     if (path == NULL)
          return;
     gtk_tree_model_get_iter(GTK_TREE_MODEL(model), &iter, path);
     
     variable->value = *new_value;

     _var_numeric_to_string(&variable->value, &tmp_str);
     gtk_tree_store_set(model->real, &iter,
                        SLR_MODEL_COL_VARAIBLE_VALUE, tmp_str->str,
                        -1);
     g_string_free(tmp_str, TRUE);
     gtk_tree_path_free(path);
}

static void
gsslrtma_updated_cb(GncSxInstanceModel *instances, gpointer user_data)
{
     GncSxSlrTreeModelAdapter *model = GNC_SX_SLR_TREE_MODEL_ADAPTER(user_data); 
     printf("update\n");
     // @@fixme: this should be better about, say, trying to match up changed
     // instance-state and variable-binding values.  More of a merge
     // operation than a replace...
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

GncSxSlrTreeModelAdapter*
gnc_sx_get_slr_model()
{
     GDate now;
     GncSxInstanceModel *instance_model;
     GncSxSlrTreeModelAdapter *slr_model;
     g_date_clear(&now, 1);
     g_date_set_time_t(&now, time(NULL));
     instance_model = gnc_sx_get_instances(&now);
     slr_model = gnc_sx_slr_tree_model_adapter_new(instance_model);
     return slr_model;
}

void
gnc_sx_slr_model_summarize(GncSxSlrTreeModelAdapter *model, GncSxSlrSummary *summary)
{
     GList *sx_iter, *inst_iter;

     g_return_if_fail(model != NULL);
     g_return_if_fail(summary != NULL);

     summary->need_dialog = FALSE;
     summary->num_instances = 0;
     summary->num_to_create_instances = 0;
     summary->num_auto_create_instances = 0;
     summary->num_auto_create_no_notify_instances = 0;

     for (sx_iter = model->instances->sx_instance_list; sx_iter != NULL; sx_iter = sx_iter->next)
     {
          GncSxInstances *instances = (GncSxInstances*)sx_iter->data;
          gboolean sx_is_auto_create = FALSE, sx_notify = FALSE;
          xaccSchedXactionGetAutoCreate(instances->sx, &sx_is_auto_create, &sx_notify);
          for (inst_iter = instances->list; inst_iter != NULL; inst_iter = inst_iter->next)
          {
               GncSxInstance *inst = (GncSxInstance*)inst_iter->data;
               summary->num_instances++;

               if (inst->state == SX_INSTANCE_STATE_TO_CREATE)
               {
                    if (sx_is_auto_create)
                    {
                         if (!sx_notify)
                         {
                              summary->num_auto_create_no_notify_instances++;
                         }
                         else
                         {
                              summary->num_auto_create_instances++;
                         }
                    }
                    else
                    {
                         summary->num_to_create_instances++;
                    }
               }
          }
     }

     // if all the instances are 'auto-create, no-notify', then we don't need
     // the dialog.
     summary->need_dialog
          = (summary->num_instances != 0
             && summary->num_auto_create_no_notify_instances != summary->num_instances);
}

static void
_print_summary(GncSxSlrSummary *summary)
{
     printf("num_instances: %d\n", summary->num_instances);
     printf("num_to_create: %d\n", summary->num_to_create_instances);
     printf("num_auto_create_instances: %d\n", summary->num_auto_create_instances);
     printf("num_auto_create_no_notify_instances: %d\n", summary->num_auto_create_no_notify_instances);
     printf("need dialog? %s\n", summary->need_dialog ? "true" : "false");
}

void
gnc_sx_sxsincelast_book_opened(void)
{
     GncSxSlrTreeModelAdapter *slr_model;
     GncSxSlrSummary summary;

     if (!gnc_gconf_get_bool(GCONF_SECTION, "show_at_file_open", NULL))
          return;

     slr_model = gnc_sx_get_slr_model();
     gnc_sx_slr_model_summarize(slr_model, &summary);
     _print_summary(&summary);
     gnc_sx_slr_model_effect_change(slr_model, TRUE, NULL, NULL);

     if (summary.need_dialog)
     {
          gnc_ui_sx_since_last_run_dialog(slr_model);
     }
     else
     {
          if (summary.num_auto_create_no_notify_instances != 0)
          {
               gnc_info_dialog
                    (NULL,
                     ngettext 
                     ("There are no Scheduled Transactions to be entered at this time. "
                      "(%d transaction automatically created)",
                      "There are no Scheduled Transactions to be entered at this time. "
                      "(%d transactions automatically created)",
                      summary.num_auto_create_no_notify_instances),
                     summary.num_auto_create_no_notify_instances);
               gnc_sx_slr_model_effect_change(slr_model, TRUE, NULL, NULL);
          }
     }
     // @@fixme g_object_unref(G_OBJECT(slr_model))
}

void
gnc_ui_sxsincelast_dialog_create(void)
{
     GncSxSlrTreeModelAdapter *slr_model = gnc_sx_get_slr_model();
     gnc_sx_slr_model_effect_change(slr_model, TRUE, NULL, NULL);
     gnc_ui_sx_since_last_run_dialog(slr_model);
}

static void
instance_state_changed_cb(GtkCellRendererText *cell,
                          const gchar *path,
                          const gchar *value,
                          GncSxSinceLastRunDialog *dialog)
{
     GtkTreeIter tree_iter;
     GncSxInstance *inst;
     int i;
     GncSxInstanceState new_state;
     
     for (i = 0; i < SX_INSTANCE_STATE_CREATED; i++)
     {
          if (strcmp(value, gnc_sx_instance_state_names[i]) == 0)
               break;
     }
     if (i == SX_INSTANCE_STATE_CREATED)
     {
          printf("unknown value [%s]\n", value);
          return;
     }
     new_state = i;

     if (!gtk_tree_model_get_iter_from_string(GTK_TREE_MODEL(dialog->editing_model), &tree_iter, path))
     {
          printf("unknown path [%s]\n", path);
          return;
     }

     inst = gnc_sx_slr_model_get_instance(dialog->editing_model, &tree_iter);
     if (inst == NULL)
     {
          printf("invalid path [%s]\n", path);
          return;
     }

     gnc_sx_slr_model_change_instance_state(dialog->editing_model, inst, new_state);
}

static void
variable_value_changed_cb(GtkCellRendererText *cell,
                          const gchar *path,
                          const gchar *value,
                          GncSxSinceLastRunDialog *dialog)
{
     GncSxVariable *var;
     GncSxInstance *inst;
     GtkTreeIter tree_iter;
     gnc_numeric parsed_num;
     char *endStr = NULL;

     printf("variable to [%s] at path [%s]\n", value, path);
     if (!gtk_tree_model_get_iter_from_string(GTK_TREE_MODEL(dialog->editing_model), &tree_iter, path))
     {
          printf("invalid path [%s]\n", path);
          return;
     }

     if (!gnc_sx_slr_model_get_instance_and_variable(dialog->editing_model, &tree_iter, &inst, &var))
     {
          printf("path [%s] doesn't correspond to a valid variable\n", path);
          return;
     }

     if (!xaccParseAmount(value, TRUE, &parsed_num, &endStr)
         || gnc_numeric_check(parsed_num) != GNC_ERROR_OK)
     {
          printf("@@fixme: better parse error handling\n");
          // @fixme: set location (back) to "(need value)"
          return;
     }
     gnc_sx_slr_model_change_variable(dialog->editing_model, inst, var, &parsed_num);
}

GncSxSinceLastRunDialog*
gnc_ui_sx_since_last_run_dialog(GncSxSlrTreeModelAdapter *slr_model)
{
     GncSxSinceLastRunDialog *dialog;
     GladeXML *glade;

     dialog = g_new0(GncSxSinceLastRunDialog, 1);
     glade = gnc_glade_xml_new("sched-xact.glade", "since-last-run-dialog");
     dialog->dialog = glade_xml_get_widget(glade, "since-last-run-dialog");

     dialog->editing_model = slr_model;
     
     {
          GtkPaned *paned;

          paned = GTK_PANED(glade_xml_get_widget(glade, "paned"));
          gtk_paned_set_position(paned, 240);
     }

     dialog->review_created_txns_toggle = GTK_TOGGLE_BUTTON(glade_xml_get_widget(glade, "review_txn_toggle"));
     
     {
          GtkCellRenderer *renderer;
          GtkTreeViewColumn *col;
          
          dialog->instance_view = GTK_TREE_VIEW(glade_xml_get_widget(glade, "instance_view"));
          gtk_tree_view_set_model(dialog->instance_view, GTK_TREE_MODEL(dialog->editing_model));

          renderer = gtk_cell_renderer_text_new();
          col = gtk_tree_view_column_new_with_attributes("SX, Instance, Variable", renderer,
                                                         "text", SLR_MODEL_COL_NAME,
                                                         NULL);
          gtk_tree_view_append_column(dialog->instance_view, col);


          renderer = gtk_cell_renderer_combo_new();
          g_object_set(G_OBJECT(renderer),
                       "model", gnc_sx_get_slr_state_model(),
                       "text-column", 0,
                       "has-entry", FALSE,
                       "editable", TRUE,
                       NULL);
          g_signal_connect(G_OBJECT(renderer),
                           "edited",
                           G_CALLBACK(instance_state_changed_cb),
                           dialog);
          col = gtk_tree_view_column_new_with_attributes("Instance State", renderer,
                                                         "text", SLR_MODEL_COL_INSTANCE_STATE,
                                                         "visible", SLR_MODEL_COL_INSTANCE_VISIBILITY,
                                                         // you might think only "sensitive" is required to
                                                         // control the ability of the combo box to select
                                                         // a new state, but you'd be wrong.
                                                         "editable", SLR_MODEL_COL_INSTANCE_STATE_SENSITIVITY,
                                                         "sensitive", SLR_MODEL_COL_INSTANCE_STATE_SENSITIVITY,
                                                         NULL);
          gtk_tree_view_append_column(dialog->instance_view, col);

          
          renderer = gtk_cell_renderer_text_new();
          g_object_set(G_OBJECT(renderer),
                       "editable", TRUE,
                       NULL);
          g_signal_connect(G_OBJECT(renderer),
                           "edited",
                           G_CALLBACK(variable_value_changed_cb),
                           dialog);
          col = gtk_tree_view_column_new_with_attributes("Variable Value", renderer,
                                                         "text", SLR_MODEL_COL_VARAIBLE_VALUE,
                                                         "visible", SLR_MODEL_COL_VARIABLE_VISIBILITY,
                                                         NULL);
          gtk_tree_view_append_column(dialog->instance_view, col);

          gtk_tree_view_expand_all(dialog->instance_view);
     }

     g_signal_connect(G_OBJECT(dialog->dialog), "response", G_CALLBACK(dialog_response_cb), dialog);
     
     gtk_widget_show_all(dialog->dialog);

     return dialog;
}

static void
_show_created_transactions(GncSxSinceLastRunDialog *app_dialog, GList *created_txn_guids)
{
     GNCLedgerDisplay *ledger;
     GncPluginPage *page;
     Query *book_query, *guid_query, *query;
     GList *guid_iter;

     book_query = xaccMallocQuery();
     guid_query = xaccMallocQuery();
     xaccQuerySetBook(book_query, gnc_get_current_book());
     for (guid_iter = created_txn_guids; guid_iter != NULL; guid_iter = guid_iter->next)
     {
          xaccQueryAddGUIDMatch(guid_query, (GUID*)guid_iter->data, GNC_ID_TRANS, QUERY_OR);
     }
     query = xaccQueryMerge(book_query, guid_query, QUERY_AND);

     // inspired by dialog-find-transactions:do_find_cb:
     ledger = gnc_ledger_display_query(query, SEARCH_LEDGER, REG_STYLE_JOURNAL);
     gnc_ledger_display_refresh(ledger);
     page = gnc_plugin_page_register_new_ledger(ledger);
     g_object_set(G_OBJECT(page), "page-name", _("Created Transactions"), NULL);
     gnc_main_window_open_page(NULL, page);

     xaccFreeQuery(query);
     xaccFreeQuery(book_query);
     xaccFreeQuery(guid_query);
}

static void
dialog_response_cb(GtkDialog *dialog, gint response_id, GncSxSinceLastRunDialog *app_dialog)
{
     GList *created_txns = NULL;
     switch (response_id)
     {
     case GTK_RESPONSE_OK:
          // @@fixme validate current state(GError *errs);
          // - instance state constraints
          // - required variable binding
          // - ability to create transactions
          {
               GList *unbound_variables;
               unbound_variables = gnc_sx_slr_model_check_variables(app_dialog->editing_model);
               printf("%d variables unbound\n", g_list_length(unbound_variables));
               if (g_list_length(unbound_variables) > 0)
               {
                    // focus first variable
                    GncSxSlrVariableNeeded *first_unbound;
                    GtkTreePath *variable_path;
                    GtkTreeViewColumn *variable_col;
                    gint variable_view_column = 2;
                    gboolean start_editing = TRUE;

                    first_unbound = (GncSxSlrVariableNeeded*)unbound_variables->data;
                    variable_path = _get_path_for_variable(app_dialog->editing_model, first_unbound->instance, first_unbound->variable);
                    variable_col = gtk_tree_view_get_column(app_dialog->instance_view, variable_view_column);

                    gtk_tree_view_set_cursor(app_dialog->instance_view, variable_path, variable_col, start_editing);

                    gtk_tree_path_free(variable_path);
                    g_list_foreach(unbound_variables, (GFunc)g_free, NULL);
                    g_list_free(unbound_variables);
                    return;
               }
          }
          gnc_suspend_gui_refresh();
          gnc_sx_slr_model_effect_change(app_dialog->editing_model, FALSE, &created_txns, NULL);
          gnc_resume_gui_refresh();
          if (gtk_toggle_button_get_active(app_dialog->review_created_txns_toggle)
              && g_list_length(created_txns) > 0)
          {
               _show_created_transactions(app_dialog, created_txns);
          }
          g_list_free(created_txns);
          created_txns = NULL;
          /* FALLTHROUGH */
     case GTK_RESPONSE_CANCEL: 
     case GTK_RESPONSE_DELETE_EVENT:
          gtk_widget_destroy(GTK_WIDGET(dialog));
          // @@fixme: destroy models, &c.
          break;
     default:
          printf("unknown response id [%d]\n", response_id);
          g_assert_not_reached();
          break;
     }
}

static void
increment_sx_state(GncSxInstance *inst, GDate **last_occur_date, int *instance_count, int *remain_occur_count)
{
     if (!g_date_valid(*last_occur_date)
         || (g_date_valid(*last_occur_date)
             && g_date_compare(*last_occur_date, &inst->date) <= 0))
     {
          *last_occur_date = &inst->date;
     }

     *instance_count = gnc_sx_get_instance_count(inst->parent->sx, inst->temporal_state);

     if (*remain_occur_count > 0)
     {
          *remain_occur_count -= 1;
     }
}

typedef struct _SxTxnCreationData
{
     GncSxInstance *instance;
     GList **created_txn_guids;
     GList **creation_errors;
} SxTxnCreationData;

static gboolean
_get_template_split_account(GncSxInstance *instance, Split *template_split, Account **split_acct, GList **creation_errors)
{
     GUID *acct_guid;
     kvp_frame *split_kvpf;
     kvp_value *kvp_val;

     split_kvpf = xaccSplitGetSlots(template_split);
     /* contains the guid of the split's actual account. */
     kvp_val = kvp_frame_get_slot_path(split_kvpf,
                                       GNC_SX_ID,
                                       GNC_SX_ACCOUNT,
                                       NULL);
     if (kvp_val == NULL)
     {
          // @@fixme: this should be more of an assert...
          GString *err = g_string_new("");
          g_string_printf(err, "Null account kvp value for SX [%s], cancelling creation.",
                          xaccSchedXactionGetName(instance->parent->sx));
          *creation_errors = g_list_append(*creation_errors, err);
          return FALSE;
     }
     acct_guid = kvp_value_get_guid( kvp_val );
     *split_acct = xaccAccountLookup(acct_guid, gnc_get_current_book());
     if (*split_acct == NULL)
     {
          const char *guid_str;
          GString *err;
          guid_str = guid_to_string((const GUID*)acct_guid);
          err = g_string_new("");
          g_string_printf(err, "Unknown account for guid [%s], cancelling SX [%s] creation.",
                          guid_str, xaccSchedXactionGetName(instance->parent->sx));
          g_free((char*)guid_str);
          *creation_errors = g_list_append(*creation_errors, err);
          return FALSE;
     }

     return TRUE;
}

static void
_get_sx_formula(GncSxInstance *instance, Split *template_split, gnc_numeric *numeric, GList **creation_errors, const char *formula_key)
{
     kvp_frame *split_kvpf;
     kvp_value *kvp_val;
     char *formula_str, *parseErrorLoc;

     split_kvpf = xaccSplitGetSlots(template_split);
     kvp_val = kvp_frame_get_slot_path(split_kvpf,
                                       GNC_SX_ID,
                                       formula_key,
                                       NULL);
     formula_str = kvp_value_get_string(kvp_val);
     if (formula_str != NULL && strlen(formula_str) != 0)
     {
          GHashTable *parser_vars = gnc_sx_instance_get_variables_for_parser(instance->variable_bindings);
          if (!gnc_exp_parser_parse_separate_vars(formula_str,
                                                  numeric,
                                                  &parseErrorLoc,
                                                  parser_vars))
          {
               GString *err = g_string_new("");
               g_string_printf(err, "Error parsing SX [%s] key [%s]=formula [%s] at [%s]: %s",
                               xaccSchedXactionGetName(instance->parent->sx),
                               formula_key,
                               formula_str,
                               parseErrorLoc,
                               gnc_exp_parser_error_string());
               *creation_errors = g_list_append(*creation_errors, err);
          }
          g_hash_table_destroy(parser_vars);
     }
}

static void
_get_credit_formula(GncSxInstance *instance, Split *template_split, gnc_numeric *credit_num, GList **creation_errors)
{
     _get_sx_formula(instance, template_split, credit_num, creation_errors, GNC_SX_CREDIT_FORMULA);
}

static void
_get_debit_formula(GncSxInstance *instance, Split *template_split, gnc_numeric *debit_num, GList **creation_errors)
{
     _get_sx_formula(instance, template_split, debit_num, creation_errors, GNC_SX_DEBIT_FORMULA);
}

static gboolean
create_each_transaction_helper(Transaction *template_txn, void *user_data)
{
     Transaction *new_txn;
     GList *txn_splits, *template_splits;
     Split *copying_split;
     gnc_commodity *first_cmdty = NULL;
     gboolean err_flag = FALSE;
     SxTxnCreationData *creation_data;

     creation_data = (SxTxnCreationData*)user_data;

     /* FIXME: In general, this should [correctly] deal with errors such
        as not finding the approrpiate Accounts and not being able to
        parse the formula|credit/debit strings. */

     new_txn = xaccTransClone(template_txn);
     xaccTransBeginEdit(new_txn);

     /* clear any copied KVP data */
     qof_instance_set_slots(QOF_INSTANCE(new_txn), kvp_frame_new());

     xaccTransSetDate(new_txn,
                      g_date_get_day(&creation_data->instance->date),
                      g_date_get_month(&creation_data->instance->date),
                      g_date_get_year(&creation_data->instance->date));
        
     /* the accounts and amounts are in the kvp_frames of the splits. */
     template_splits = xaccTransGetSplitList(template_txn);
     txn_splits = xaccTransGetSplitList(new_txn);
     if ((template_splits == NULL) || (txn_splits == NULL))
     {
          PERR("\tseen transaction w/o splits. :(");
          xaccTransDestroy(new_txn);
          xaccTransCommitEdit(new_txn);
          return FALSE;
     }

     for (;
          txn_splits && template_splits;
          txn_splits = txn_splits->next, template_splits = template_splits->next)
     {
          Split *template_split;
          Account *split_acct;
          gnc_commodity *split_cmdty = NULL;
             
          /* FIXME: Ick.  This assumes that the split lists will be ordered
             identically. :( They are, but we'd rather not have to count on
             it. --jsled */
          template_split = (Split*)template_splits->data;
          copying_split = (Split*)txn_splits->data;

          /* clear out any copied Split frame data. */
          qof_instance_set_slots(QOF_INSTANCE(copying_split), kvp_frame_new());

          if (!_get_template_split_account(creation_data->instance, template_split, &split_acct, creation_data->creation_errors))
          {
               err_flag = TRUE;
               break;
          }
             
          split_cmdty = xaccAccountGetCommodity(split_acct);
          if (first_cmdty == NULL)
          {
               first_cmdty = split_cmdty;
               xaccTransSetCurrency(new_txn, first_cmdty);
          }

          xaccAccountBeginEdit(split_acct);
          xaccAccountInsertSplit(split_acct, copying_split);

          {
               gnc_numeric credit_num, debit_num, final;
               gint gncn_error;

               credit_num = gnc_numeric_zero();
               debit_num = gnc_numeric_zero();

               _get_credit_formula(creation_data->instance, template_split, &credit_num, creation_data->creation_errors);
               _get_debit_formula(creation_data->instance, template_split, &debit_num, creation_data->creation_errors);
                       
               final = gnc_numeric_sub_fixed( debit_num, credit_num );
                        
               gncn_error = gnc_numeric_check(final);
               if (gncn_error != GNC_ERROR_OK) {
                    GString *err = g_string_new("");
                    g_string_printf(err, "Error %d in SX [%s] final gnc_numeric value, using 0 instead.", 
                                    gncn_error,
                                    xaccSchedXactionGetName(creation_data->instance->parent->sx));
                    *creation_data->creation_errors = g_list_append(*creation_data->creation_errors, err);
                    final = gnc_numeric_zero();
               }

               xaccSplitSetValue(copying_split, final);
               if (! gnc_commodity_equal(split_cmdty, first_cmdty))
               {
                    GString *exchange_rate_var_name = g_string_sized_new(16);
                    GncSxVariable *exchange_rate_var;
                    gnc_numeric exchange_rate, amt;

                    /*
                      GNCPriceDB *price_db = gnc_pricedb_get_db(gnc_get_current_book());
                      GNCPrice *price;

                      price = gnc_pricedb_lookup_latest(price_db, first_cmdty, split_cmdty);
                      if (price == NULL)
                      {
                      price = gnc_pricedb_lookup_latest(price_db, split_cmdty, first_cmdty);
                      if (price == NULL)
                      {
                      GString *err = g_string_new("");
                      g_string_printf(err, "could not find pricedb entry for commodity-pair (%s, %s).",
                      gnc_commodity_get_mnemonic(first_cmdty),
                      gnc_commodity_get_mnemonic(split_cmdty));
                      exchange = gnc_numeric_create(1, 1);
                      *creation_data->creation_errors = g_list_append(*creation_data->creation_errors, err);

                      }
                      else
                      {
                      exchange = gnc_numeric_div(gnc_numeric_create(1,1),
                      gnc_price_get_value(price),
                      1000, GNC_HOW_RND_ROUND);
                      }
                      }
                      else
                      {
                      exchange = gnc_price_get_value(price);
                      }
                    */

                    exchange_rate = gnc_numeric_zero();
                    g_string_printf(exchange_rate_var_name, "%s -> %s",
                                    gnc_commodity_get_mnemonic(split_cmdty),
                                    gnc_commodity_get_mnemonic(first_cmdty));
                    exchange_rate_var = (GncSxVariable*)g_hash_table_lookup(creation_data->instance->variable_bindings,
                                                                            exchange_rate_var_name->str);
                    if (exchange_rate_var != NULL)
                    {
                         exchange_rate = exchange_rate_var->value;
                    }
                    g_string_free(exchange_rate_var_name, TRUE);

                    amt = gnc_numeric_mul(final, exchange_rate, 1000, GNC_HOW_RND_ROUND);
                    xaccSplitSetAmount(copying_split, amt);
               }

               xaccSplitScrub(copying_split);
          }

          xaccAccountCommitEdit(split_acct);
     }

     if (err_flag)
     {
          PERR("Some error in new transaction creation...");
          xaccTransDestroy(new_txn);
          xaccTransCommitEdit(new_txn);
          return FALSE;
     }

     {
          kvp_frame *txn_frame;
          /* set a kvp-frame element in the transaction indicating and
           * pointing-to the SX this was created from. */
          txn_frame = xaccTransGetSlots(new_txn);
          kvp_frame_set_guid(txn_frame, "from-sched-xaction", xaccSchedXactionGetGUID(creation_data->instance->parent->sx));
     }

     xaccTransCommitEdit(new_txn);

     if (creation_data->created_txn_guids != NULL)
     {
          *creation_data->created_txn_guids
               = g_list_append(*(creation_data->created_txn_guids), (gpointer)xaccTransGetGUID(new_txn));
     }

     return TRUE;
}

static void
create_transactions_for_instance(GncSxInstance *instance, GList **created_txn_guids, GList **creation_errors)
{
     SxTxnCreationData creation_data;
     Account *sx_template_account;

     sx_template_account = gnc_sx_get_template_transaction_account(instance->parent->sx);

     creation_data.instance = instance;
     creation_data.created_txn_guids = created_txn_guids;
     creation_data.creation_errors = creation_errors;

     xaccAccountForEachTransaction(sx_template_account,
                                   create_each_transaction_helper,
                                   &creation_data);
}

/**
 * @param auto_create_only Will only affect auto-create transactions; the
 * rest of the state will be left alone.
 **/
void
gnc_sx_slr_model_effect_change(GncSxSlrTreeModelAdapter *model,
                               gboolean auto_create_only,
                               GList **created_transaction_guids,
                               GList **creation_errors)
{
     GList *list;

     // @@fixme engine event supression

     for (list = model->instances->sx_instance_list; list != NULL; list = list->next)
     {
          GList *instance_list;
          GncSxInstances *instances = (GncSxInstances*)list->data;
          GDate *last_occur_date;
          gint instance_count = 0;
          gint remain_occur_count = 0;

          last_occur_date = xaccSchedXactionGetLastOccurDate(instances->sx);
          instance_count = gnc_sx_get_instance_count(instances->sx, NULL);
          remain_occur_count = xaccSchedXactionGetRemOccur(instances->sx);

          for (instance_list = instances->list; instance_list != NULL; instance_list = instance_list->next)
          {
               GncSxInstance *inst = (GncSxInstance*)instance_list->data;
               gboolean sx_is_auto_create;

               xaccSchedXactionGetAutoCreate(inst->parent->sx, &sx_is_auto_create, NULL);
               if (auto_create_only && !sx_is_auto_create)
               {
                    if (inst->state != SX_INSTANCE_STATE_TO_CREATE)
                    {
                         break;
                    }
                    continue;
               }

               if (inst->orig_state == SX_INSTANCE_STATE_POSTPONED
                   && inst->state != SX_INSTANCE_STATE_POSTPONED)
               {
                    // remove from postponed list
                    g_assert(inst->temporal_state != NULL);
                    gnc_sx_remove_defer_instance(inst->parent->sx, inst->temporal_state);
               }

               switch (inst->state)
               {
               case SX_INSTANCE_STATE_CREATED:
                    // nop: we've already processed this.
                    break;
               case SX_INSTANCE_STATE_IGNORED:
                    increment_sx_state(inst, &last_occur_date, &instance_count, &remain_occur_count);
                    break;
               case SX_INSTANCE_STATE_POSTPONED:
                    if (inst->orig_state != SX_INSTANCE_STATE_POSTPONED)
                    {
                         gnc_sx_add_defer_instance(instances->sx, inst->temporal_state);
                    }
                    increment_sx_state(inst, &last_occur_date, &instance_count, &remain_occur_count);
                    break;
               case SX_INSTANCE_STATE_TO_CREATE:
                    create_transactions_for_instance(inst, created_transaction_guids, creation_errors);
                    increment_sx_state(inst, &last_occur_date, &instance_count, &remain_occur_count);
                    gnc_sx_slr_model_change_instance_state(model, inst, SX_INSTANCE_STATE_CREATED);
                    break;
               case SX_INSTANCE_STATE_REMINDER:
                    // do nothing
                    // assert no non-remind instances after this?
                    break;
               default:
                    g_assert_not_reached();
                    break;
               }
          }
          
          xaccSchedXactionSetLastOccurDate(instances->sx, last_occur_date);
          gnc_sx_set_instance_count(instances->sx, instance_count);
          xaccSchedXactionSetRemOccur(instances->sx, remain_occur_count);
     }

     // @fixme: re-generate instance model, repopulate [?]
}

static void
_list_from_hash_elts(gpointer key, gpointer value, GList **result_list)
{
     *result_list = g_list_append(*result_list, value);
}

GList*
gnc_sx_slr_model_check_variables(GncSxSlrTreeModelAdapter *model)
{
     GList *rtn = NULL;
     GList *sx_iter, *inst_iter, *var_list = NULL, *var_iter;

     for (sx_iter = model->instances->sx_instance_list; sx_iter != NULL; sx_iter = sx_iter->next)
     {
          GncSxInstances *instances = (GncSxInstances*)sx_iter->data;
          for (inst_iter = instances->list; inst_iter != NULL; inst_iter = inst_iter->next)
          {
               GncSxInstance *inst = (GncSxInstance*)inst_iter->data;

               if (inst->state != SX_INSTANCE_STATE_TO_CREATE)
                    continue;

               g_hash_table_foreach(inst->variable_bindings, (GHFunc)_list_from_hash_elts, &var_list);
               for (var_iter = var_list; var_iter != NULL; var_iter = var_iter->next)
               {
                    GncSxVariable *var = (GncSxVariable*)var_iter->data;
                    if (gnc_numeric_check(var->value) != GNC_ERROR_OK)
                    {
                         GncSxSlrVariableNeeded *need = g_new0(GncSxSlrVariableNeeded, 1);
                         need->instance = inst;
                         need->variable = var;
                         rtn = g_list_append(rtn, need);
                    }
               }
               g_list_free(var_list);
               var_list = NULL;
          }
     }
     return rtn;
}
