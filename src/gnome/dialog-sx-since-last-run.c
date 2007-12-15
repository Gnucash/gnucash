/********************************************************************\
 * dialog-sx-since-last-run.c : dialog for scheduled transaction    *
 * since-last-run processing.                                       *
 * Copyright (C) 2006 Joshua Sled <jsled@asynchronous.org>          *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of version 2 of the GNU General Public *
 * License as published by the Free Software Foundation.            *
 *
 * As a special exception, permission is granted to link the binary
 * module resultant from this code with the OpenSSL project's
 * "OpenSSL" library (or modified versions of it that use the same
 * license as the "OpenSSL" library), and distribute the linked
 * executable.  You must obey the GNU General Public License in all
 * respects for all of the code used other than "OpenSSL". If you
 * modify this file, you may extend this exception to your version
 * of the file, but you are not obligated to do so. If you do not
 * wish to do so, delete this exception statement from your version
 * of this file.
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
#include "Query.h"
#include "QueryNew.h"
#include "gnc-ledger-display.h"
#include "gnc-plugin-page-register.h"
#include "gnc-main-window.h"
#include "gnc-component-manager.h"
#include "gnc-gconf-utils.h"
#include "gnc-gui-query.h"
#include "gnc-session.h"

#undef G_LOG_DOMAIN
#define G_LOG_DOMAIN "gnc.gui.sx.slr"

#define DIALOG_SX_SINCE_LAST_RUN_CM_CLASS "dialog-sx-since-last-run"
#define GCONF_SECTION "dialogs/scheduled_trans/since_last_run"

struct _GncSxSinceLastRunDialog
{
    GtkWidget *dialog;
    gint component_id;
    GncSxSlrTreeModelAdapter *editing_model;
    GtkTreeView *instance_view;
    GtkToggleButton *review_created_txns_toggle;
    GList *created_txns;
};

/* ------------------------------------------------------------ */

static GObjectClass *parent_class = NULL;

struct _GncSxSlrTreeModelAdapter
{
    GObject parent;

    /* protected: */
    gulong updated_cb_id;
    gboolean disposed;

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
static void gnc_sx_slr_tree_model_adapter_dispose(GObject *obj);
static void gnc_sx_slr_tree_model_adapter_finalize(GObject *obj);

GncSxInstanceModel* gnc_sx_slr_tree_model_adapter_get_instance_model(GncSxSlrTreeModelAdapter *slr_model);
GncSxInstances* gnc_sx_slr_tree_model_adapter_get_sx_instances(GncSxSlrTreeModelAdapter *model, GtkTreeIter *iter);
static GncSxInstances* _gnc_sx_slr_tree_model_adapter_get_sx_instances(GncSxSlrTreeModelAdapter *model, GtkTreeIter *iter, gboolean check_depth);
/** @return null if the iter is not actually an GncSxInstance. **/
GncSxInstance* gnc_sx_slr_model_get_instance(GncSxSlrTreeModelAdapter *model, GtkTreeIter *iter);
static GncSxInstance* _gnc_sx_slr_model_get_instance(GncSxSlrTreeModelAdapter *model, GtkTreeIter *iter, gboolean check_depth);
/** @return false if the iter is not actaully an GncSxInstance's variable. **/
gboolean gnc_sx_slr_model_get_instance_and_variable(GncSxSlrTreeModelAdapter *model, GtkTreeIter *iter, GncSxInstance **instance_loc, GncSxVariable **var_loc);

void gnc_sx_slr_model_effect_change(GncSxSlrTreeModelAdapter *model, gboolean auto_create_only, GList **created_transaction_guids, GList **creation_errors);

GtkTreeModel* gnc_sx_get_slr_state_model(void);

#define GNC_TYPE_SX_SLR_TREE_MODEL_ADAPTER	      (gnc_sx_slr_tree_model_adapter_get_type ())
#define GNC_SX_SLR_TREE_MODEL_ADAPTER(obj)	      (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_SX_SLR_TREE_MODEL_ADAPTER, GncSxSlrTreeModelAdapter))
#define GNC_SX_SLR_TREE_MODEL_ADAPTER_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_SX_SLR_TREE_MODEL_ADAPTER, GncSxSlrTreeModelAdapterClass))
#define GNC_IS_SX_SLR_TREE_MODEL_ADAPTER(obj)	      (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_SX_SLR_TREE_MODEL_ADAPTER))
#define GNC_IS_SX_SLR_TREE_MODEL_ADAPTER_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_SX_SLR_TREE_MODEL_ADAPTER))
#define GNC_SX_SLR_TREE_MODEL_ADAPTER_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_SX_SLR_TREE_MODEL_ADAPTER, GncSxSlrTreeModelAdapterClass))

/* ------------------------------------------------------------ */

static void _show_created_transactions(GncSxSinceLastRunDialog *app_dialog, GList *created_txn_guids);

static void close_handler(gpointer user_data);
static void dialog_destroy_cb(GtkObject *object, GncSxSinceLastRunDialog *app_dialog);
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
    static GType gsstma_type = 0;
    if (gsstma_type == 0) {
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

        gsstma_type = g_type_register_static (G_TYPE_OBJECT,
                                              "GncSxSlrTreeModelAdapterType",
                                              &info, 0);
        g_type_add_interface_static(gsstma_type,
                                    GTK_TYPE_TREE_MODEL,
                                    &itreeModel_info);
    }
    return gsstma_type;
}

static void
gnc_sx_slr_tree_model_adapter_class_init(GncSxSlrTreeModelAdapterClass *klass)
{
    GObjectClass *obj_class;

    parent_class = g_type_class_peek_parent(klass);

    obj_class = G_OBJECT_CLASS(klass);

    obj_class->dispose = gnc_sx_slr_tree_model_adapter_dispose;
    obj_class->finalize = gnc_sx_slr_tree_model_adapter_finalize;
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

static char* gnc_sx_instance_state_names[] = {
    N_("Ignored"),
    N_("Postponed"),
    N_("To-Create"),
    N_("Reminder"),
    N_("Created"),
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
                                              0, _(gnc_sx_instance_state_names[i]), -1);
        }
    }
    return _singleton_slr_state_model;
}

static void
_consume_excess_rows(GtkTreeStore *store, int last_index, GtkTreeIter *parent_iter, GtkTreeIter *maybe_invalid_iter)
{
    if (last_index == -1)
    {
        // try to get whatever was there beforehand, if it exists
        if (!gtk_tree_model_iter_children(GTK_TREE_MODEL(store), maybe_invalid_iter, parent_iter))
            return;
    }
    else
    {
        // increment the iter, or bail out.
        if (!gtk_tree_model_iter_next(GTK_TREE_MODEL(store), maybe_invalid_iter))
            return;
    }

    // consume until we're done.
    while (gtk_tree_store_remove(store, maybe_invalid_iter));
}


static void
gsslrtma_populate_tree_store(GncSxSlrTreeModelAdapter *model)
{
    GtkTreeIter sx_tree_iter;
    GList *sx_iter;
    int instances_index = -1;

    for (sx_iter = model->instances->sx_instance_list; sx_iter != NULL; sx_iter = sx_iter->next)
    {
        GncSxInstances *instances = (GncSxInstances*)sx_iter->data;
        char last_occur_date_buf[MAX_DATE_LENGTH+1];

        {
            GDate *last_occur = xaccSchedXactionGetLastOccurDate(instances->sx);
            if (last_occur == NULL || !g_date_valid(last_occur))
            {
                g_stpcpy(last_occur_date_buf, _("Never"));
            }
            else
            {
                qof_print_gdate(last_occur_date_buf,
                                MAX_DATE_LENGTH,
                                last_occur);
            }
        }

        if (!gtk_tree_model_iter_nth_child(GTK_TREE_MODEL(model->real), &sx_tree_iter, NULL, ++instances_index))
        {
            gtk_tree_store_append(model->real, &sx_tree_iter, NULL);
        }

        gtk_tree_store_set(model->real, &sx_tree_iter,
                           SLR_MODEL_COL_NAME, xaccSchedXactionGetName(instances->sx),
                           SLR_MODEL_COL_INSTANCE_STATE, NULL,
                           SLR_MODEL_COL_VARAIBLE_VALUE, NULL,
                           SLR_MODEL_COL_INSTANCE_VISIBILITY, FALSE,
                           SLR_MODEL_COL_VARIABLE_VISIBILITY, FALSE,
                           SLR_MODEL_COL_INSTANCE_STATE_SENSITIVITY, FALSE,
                           -1);

        // Insert instance information
        {
            GList *inst_iter;
            GtkTreeIter inst_tree_iter;
            char instance_date_buf[MAX_DATE_LENGTH+1];
            int instance_index = -1;

            for (inst_iter = instances->instance_list; inst_iter != NULL; inst_iter = inst_iter->next)
            {
                GncSxInstance *inst = (GncSxInstance*)inst_iter->data;
                qof_print_gdate(instance_date_buf, MAX_DATE_LENGTH, &inst->date);

                if (!gtk_tree_model_iter_nth_child(GTK_TREE_MODEL(model->real), &inst_tree_iter, &sx_tree_iter, ++instance_index))
                {
                    gtk_tree_store_append(model->real, &inst_tree_iter, &sx_tree_iter);
                }
                gtk_tree_store_set(model->real, &inst_tree_iter,
                                   SLR_MODEL_COL_NAME, instance_date_buf,
                                   SLR_MODEL_COL_INSTANCE_STATE, _(gnc_sx_instance_state_names[inst->state]),
                                   SLR_MODEL_COL_VARAIBLE_VALUE, NULL,
                                   SLR_MODEL_COL_INSTANCE_VISIBILITY, TRUE,
                                   SLR_MODEL_COL_VARIABLE_VISIBILITY, FALSE,
                                   SLR_MODEL_COL_INSTANCE_STATE_SENSITIVITY, inst->state != SX_INSTANCE_STATE_CREATED,
                                   -1);

                // Insert variable information
                {
                    GList *vars = NULL, *var_iter;
                    GtkTreeIter var_tree_iter;
                    gint visible_variable_index = -1;

                    vars = gnc_sx_instance_get_variables(inst);
                    for (var_iter = vars; var_iter != NULL; var_iter = var_iter->next)
                    {
                        GncSxVariable *var = (GncSxVariable*)var_iter->data;
                        GString *tmp_str;

                        if (!var->editable)
                            continue;

                        if (gnc_numeric_check(var->value) == GNC_ERROR_OK)
                        {
                            _var_numeric_to_string(&var->value, &tmp_str);
                        }
                        else
                        {
                            tmp_str = g_string_new(_("(Need Value)"));
                        }

                        if (!gtk_tree_model_iter_nth_child(GTK_TREE_MODEL(model->real),
                                                           &var_tree_iter, &inst_tree_iter,
                                                           ++visible_variable_index))
                        {
                            gtk_tree_store_append(model->real, &var_tree_iter, &inst_tree_iter);
                        }
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

                    _consume_excess_rows(model->real, visible_variable_index, &inst_tree_iter, &var_tree_iter);
                }
            }
               
            // if there are more instance iters, remove
            _consume_excess_rows(model->real, instance_index, &sx_tree_iter, &inst_tree_iter);
        }
    }
    _consume_excess_rows(model->real, instances_index, NULL, &sx_tree_iter);
}

GncSxInstanceModel*
gnc_sx_slr_tree_model_adapter_get_instance_model(GncSxSlrTreeModelAdapter *slr_model)
{
    return slr_model->instances;
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
    if (instance_index < 0 || instance_index >= g_list_length(instances->instance_list))
    {
        return NULL;
    }

    return (GncSxInstance*)g_list_nth_data(instances->instance_list, instance_index);
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
        // *var_loc = (GncSxVariable*)g_list_nth_data(variables, variable_index);
        GList *list_iter = variables;
        for (; list_iter != NULL; list_iter = list_iter->next)
        {
            GncSxVariable *var = (GncSxVariable*)list_iter->data;
            if (!var->editable)
                continue;
            if (variable_index-- == 0)
            {
                *var_loc = var;
                break;
            }
        }
    }

    g_list_free(variables);
    return TRUE;
}

/**
 * Special-case list indexing that only refers to "editable" variables. :(
 **/
static gint
_variable_list_index(GList *variables, GncSxVariable *variable)
{
    gint index = 0;
    for (; variables != NULL; variables = variables->next)
    {
        GncSxVariable *var = (GncSxVariable*)variables->data;
        if (!var->editable)
            continue;
        if (variable == var)
            return index;
        index++;
    }
    return -1;
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
    indices[1] = g_list_index(instance->parent->instance_list, instance);
    if (indices[1] == -1)
        return NULL;
    variables = gnc_sx_instance_get_variables(instance);
    indices[2] = _variable_list_index(variables, variable);
    g_list_free(variables);
    if (indices[2] == -1)
        return NULL;
    path = gtk_tree_path_new_from_indices(indices[0], indices[1], indices[2], -1);
    return path;
}

static void
gsslrtma_added_cb(GncSxInstanceModel *instances, SchedXaction *added_sx, gpointer user_data)
{
    GncSxSlrTreeModelAdapter *model = GNC_SX_SLR_TREE_MODEL_ADAPTER(user_data); 
    // this is wasteful, but fine.
    gsslrtma_populate_tree_store(model);
}

static void
gsslrtma_updated_cb(GncSxInstanceModel *instances, SchedXaction *updated_sx, gpointer user_data)
{
    GncSxSlrTreeModelAdapter *model = GNC_SX_SLR_TREE_MODEL_ADAPTER(user_data); 
    gnc_sx_instance_model_update_sx_instances(instances, updated_sx);
    gsslrtma_populate_tree_store(model);
}

static void
gsslrtma_removing_cb(GncSxInstanceModel *instances, SchedXaction *to_remove_sx, gpointer user_data)
{
    GncSxSlrTreeModelAdapter *model = GNC_SX_SLR_TREE_MODEL_ADAPTER(user_data); 
    GtkTreeIter tree_iter;
    GList *iter;
    int index = 0;
    // get index, create path, remove
    for (iter = instances->sx_instance_list; iter != NULL; iter = iter->next, index++)
    {
        GncSxInstances *instances = (GncSxInstances*)iter->data;
        if (instances->sx == to_remove_sx)
            break;
    }
    if (iter == NULL)
        return; // couldn't find sx in our model, which is weird.
    if (!gtk_tree_model_iter_nth_child(GTK_TREE_MODEL(model->real), &tree_iter, NULL, index))
        return; // perr(couldn't get something that should exist.
    gtk_tree_store_remove(model->real, &tree_iter);

    gnc_sx_instance_model_remove_sx_instances(instances, to_remove_sx);
}

static void
gnc_sx_slr_tree_model_adapter_dispose(GObject *obj)
{
    GncSxSlrTreeModelAdapter *adapter;
    g_return_if_fail(obj != NULL);
    adapter = GNC_SX_SLR_TREE_MODEL_ADAPTER(obj);
    g_return_if_fail(!adapter->disposed);
    adapter->disposed = TRUE;
     
    g_object_unref(G_OBJECT(adapter->instances));
    adapter->instances = NULL;
    g_object_unref(G_OBJECT(adapter->real));
    adapter->real = NULL;

    G_OBJECT_CLASS(parent_class)->dispose(obj);
}

static void
gnc_sx_slr_tree_model_adapter_finalize(GObject *obj)
{
    g_return_if_fail(obj != NULL);
    G_OBJECT_CLASS(parent_class)->finalize(obj);
}

GncSxSlrTreeModelAdapter*
gnc_sx_slr_tree_model_adapter_new(GncSxInstanceModel *instances)
{
    GncSxSlrTreeModelAdapter *rtn;
    rtn = GNC_SX_SLR_TREE_MODEL_ADAPTER(g_object_new(GNC_TYPE_SX_SLR_TREE_MODEL_ADAPTER, NULL));
    rtn->instances = instances;
    g_object_ref(G_OBJECT(rtn->instances));
    gsslrtma_populate_tree_store(rtn);
    g_signal_connect(G_OBJECT(rtn->instances), "added", (GCallback)gsslrtma_added_cb, (gpointer)rtn);
    rtn->updated_cb_id = g_signal_connect(G_OBJECT(rtn->instances), "updated", (GCallback)gsslrtma_updated_cb, (gpointer)rtn);
    g_signal_connect(G_OBJECT(rtn->instances), "removing", (GCallback)gsslrtma_removing_cb, (gpointer)rtn);
    return rtn;
}

void
gnc_sx_sxsincelast_book_opened(void)
{
    GList *auto_created_txns = NULL;
    GncSxInstanceModel *inst_model;
    GncSxSummary summary;

    if (!gnc_gconf_get_bool(GCONF_SECTION, "show_at_file_open", NULL))
        return;

    inst_model = gnc_sx_get_current_instances();
    gnc_sx_instance_model_summarize(inst_model, &summary);
    gnc_sx_summary_print(&summary);
    gnc_sx_instance_model_effect_change(inst_model, TRUE, &auto_created_txns, NULL);

    if (summary.need_dialog)
    {
        gnc_ui_sx_since_last_run_dialog(inst_model, auto_created_txns);
        auto_created_txns = NULL;
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
        }
    }
    g_list_free(auto_created_txns);
    g_object_unref(G_OBJECT(inst_model));
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
        if (strcmp(value, _(gnc_sx_instance_state_names[i])) == 0)
            break;
    }
    if (i == SX_INSTANCE_STATE_CREATED)
    {
        g_warning("unknown value [%s]", value);
        return;
    }
    new_state = i;

    if (!gtk_tree_model_get_iter_from_string(GTK_TREE_MODEL(dialog->editing_model), &tree_iter, path))
    {
        g_warning("unknown path [%s]", path);
        return;
    }

    inst = gnc_sx_slr_model_get_instance(dialog->editing_model, &tree_iter);
    if (inst == NULL)
    {
        g_warning("invalid path [%s]", path);
        return;
    }

    gnc_sx_instance_model_change_instance_state(dialog->editing_model->instances, inst, new_state);
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

    g_debug("variable to [%s] at path [%s]", value, path);
    if (!gtk_tree_model_get_iter_from_string(GTK_TREE_MODEL(dialog->editing_model), &tree_iter, path))
    {
        g_warning("invalid path [%s]", path);
        return;
    }

    if (!gnc_sx_slr_model_get_instance_and_variable(dialog->editing_model, &tree_iter, &inst, &var))
    {
        g_critical("path [%s] doesn't correspond to a valid variable", path);
        return;
    }

    if (!xaccParseAmount(value, TRUE, &parsed_num, &endStr)
        || gnc_numeric_check(parsed_num) != GNC_ERROR_OK)
    {
        gchar *value_copy = g_strdup(value);
        g_debug("value=[%s] endStr[%s]", value, endStr);
        if (strlen(g_strstrip(value_copy)) == 0)
        {
            gnc_numeric invalid_num = gnc_numeric_error(GNC_ERROR_ARG);
            gnc_sx_instance_model_set_variable(dialog->editing_model->instances, inst, var, &invalid_num);
        }
        else
        {
            g_warning("error parsing value [%s]", value);
        }
        g_free(value_copy);
        return;
    }
    gnc_sx_instance_model_set_variable(dialog->editing_model->instances, inst, var, &parsed_num);
}

GncSxSinceLastRunDialog*
gnc_ui_sx_since_last_run_dialog(GncSxInstanceModel *sx_instances, GList *auto_created_txn_guids)
{
    GncSxSinceLastRunDialog *dialog;
    GladeXML *glade;

    dialog = g_new0(GncSxSinceLastRunDialog, 1);
    glade = gnc_glade_xml_new("sched-xact.glade", "since-last-run-dialog");
    dialog->dialog = glade_xml_get_widget(glade, "since-last-run-dialog");

    dialog->editing_model = gnc_sx_slr_tree_model_adapter_new(sx_instances);
    dialog->review_created_txns_toggle = GTK_TOGGLE_BUTTON(glade_xml_get_widget(glade, "review_txn_toggle"));

    dialog->created_txns = auto_created_txn_guids;
     
    {
        GtkCellRenderer *renderer;
        GtkTreeViewColumn *col;
          
        dialog->instance_view = GTK_TREE_VIEW(glade_xml_get_widget(glade, "instance_view"));
        gtk_tree_view_set_model(dialog->instance_view, GTK_TREE_MODEL(dialog->editing_model));

        renderer = gtk_cell_renderer_text_new();
        col = gtk_tree_view_column_new_with_attributes(_("Transaction"), renderer,
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
        col = gtk_tree_view_column_new_with_attributes(_("Status"), renderer,
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
        col = gtk_tree_view_column_new_with_attributes(_("Value"), renderer,
                                                       "text", SLR_MODEL_COL_VARAIBLE_VALUE,
                                                       "visible", SLR_MODEL_COL_VARIABLE_VISIBILITY,
                                                       NULL);
        gtk_tree_view_append_column(dialog->instance_view, col);

        gtk_tree_view_expand_all(dialog->instance_view);
    }

    g_signal_connect(G_OBJECT(dialog->dialog), "response", G_CALLBACK(dialog_response_cb), dialog);
    g_signal_connect(G_OBJECT(dialog->dialog), "destroy", G_CALLBACK(dialog_destroy_cb), dialog);

    gnc_restore_window_size(GCONF_SECTION, GTK_WINDOW(dialog->dialog));

    dialog->component_id = gnc_register_gui_component
        (DIALOG_SX_SINCE_LAST_RUN_CM_CLASS, NULL, close_handler, dialog);
    gnc_gui_component_set_session(dialog->component_id,
                                  gnc_get_current_session());

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
close_handler(gpointer user_data)
{
    GncSxSinceLastRunDialog *app_dialog = user_data;

    gnc_save_window_size(GCONF_SECTION, GTK_WINDOW(app_dialog->dialog));
    gtk_widget_destroy(app_dialog->dialog);
}

static void
dialog_destroy_cb(GtkObject *object, GncSxSinceLastRunDialog *app_dialog)
{
    gnc_unregister_gui_component(app_dialog->component_id);

    g_object_unref(G_OBJECT(app_dialog->editing_model));
    app_dialog->editing_model = NULL;
}

static void
dialog_response_cb(GtkDialog *dialog, gint response_id, GncSxSinceLastRunDialog *app_dialog)
{
    switch (response_id)
    {
    case GTK_RESPONSE_OK:
        // @@fixme validate current state(GError *errs);
        // - [ ] instance state constraints
        // - [x] required variable binding
        // - [?] ability to create transactions
        {
            GList *unbound_variables;
            unbound_variables = gnc_sx_instance_model_check_variables(app_dialog->editing_model->instances);
            g_message("%d variables unbound", g_list_length(unbound_variables));
            if (g_list_length(unbound_variables) > 0)
            {
                // focus first variable
                GncSxVariableNeeded *first_unbound;
                GtkTreePath *variable_path;
                GtkTreeViewColumn *variable_col;
                gint variable_view_column = 2;
                gboolean start_editing = TRUE;

                first_unbound = (GncSxVariableNeeded*)unbound_variables->data;
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
        gnc_sx_slr_model_effect_change(app_dialog->editing_model, FALSE, &app_dialog->created_txns, NULL);
        gnc_resume_gui_refresh();
        if (gtk_toggle_button_get_active(app_dialog->review_created_txns_toggle)
            && g_list_length(app_dialog->created_txns) > 0)
        {
            _show_created_transactions(app_dialog, app_dialog->created_txns);
        }
        g_list_free(app_dialog->created_txns);
        app_dialog->created_txns = NULL;

    /* FALLTHROUGH */
    case GTK_RESPONSE_CANCEL:
    case GTK_RESPONSE_DELETE_EVENT:
        gnc_close_gui_component(app_dialog->component_id);
        break;
    default:
        g_error("unknown response id [%d]", response_id);
        break;
    }
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
    g_signal_handler_block(model->instances, model->updated_cb_id);
    gnc_sx_instance_model_effect_change(model->instances, auto_create_only, created_transaction_guids, creation_errors);
    g_signal_handler_unblock(model->instances, model->updated_cb_id);
}
