/********************************************************************\
 * dialog-sx-since-last-run.c : dialog for scheduled transaction    *
 * since-last-run processing.                                       *
 * Copyright (C) 2006 Joshua Sled <jsled@asynchronous.org>          *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of version 2 and/or version 3 of the   *
 * GNU General Public License as published by the Free Software     *
 * Foundation.                                                      *
 *                                                                  *
 * As a special exception, permission is granted to link the binary *
 * module resultant from this code with the OpenSSL project's       *
 * "OpenSSL" library (or modified versions of it that use the same  *
 * license as the "OpenSSL" library), and distribute the linked     *
 * executable.  You must obey the GNU General Public License in all *
 * respects for all of the code used other than "OpenSSL". If you   *
 * modify this file, you may extend this exception to your version  *
 * of the file, but you are not obligated to do so. If you do not   *
 * wish to do so, delete this exception statement from your version *
 * of this file.                                                    *
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

#include <config.h>
#include <glib.h>
#include <gtk/gtk.h>

#include "dialog-utils.h"
#include "gnc-sx-instance-model.h"
#include "dialog-sx-since-last-run.h"

#include "gnc-prefs.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "gnc-glib-utils.h"
#include "Query.h"
#include "qof.h"
#include "gnc-ledger-display.h"
#include "gnc-plugin-page-register.h"
#include "gnc-main-window.h"
#include "gnc-component-manager.h"
#include "gnc-gui-query.h"
#include "gnc-session.h"

#undef G_LOG_DOMAIN
#define G_LOG_DOMAIN "gnc.gui.sx.slr"

G_GNUC_UNUSED static QofLogModule log_module = GNC_MOD_GUI_SX;

#define DIALOG_SX_SINCE_LAST_RUN_CM_CLASS "dialog-sx-since-last-run"

#define GNC_PREF_SET_REVIEW     "review-transactions"
#define GNC_PREF_SLR_SORT_COL   "sort-column"
#define GNC_PREF_SLR_SORT_ASC   "sort-ascending"
#define GNC_PREF_SLR_SORT_DEPTH "sort-depth"

struct _GncSxSinceLastRunDialog
{
    GtkWidget *dialog;
    gint component_id;
    GncSxSlrTreeModelAdapter *editing_model;
    GtkTreeView *instance_view;
    GtkToggleButton *review_created_txns_toggle;
    GList *created_txns;

    GtkCellEditable *temp_ce; // used when editing values
    gint sort_selection_depth; // used when sorting transaction column
};

/* ------------------------------------------------------------ */


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

GType gnc_sx_slr_tree_model_adapter_get_type (void);
static void gnc_sx_slr_tree_model_adapter_interface_init (GtkTreeModelIface *tree_model);
GncSxSlrTreeModelAdapter* gnc_sx_slr_tree_model_adapter_new (GncSxInstanceModel *instances);
static void gnc_sx_slr_tree_model_adapter_dispose (GObject *obj);
static void gnc_sx_slr_tree_model_adapter_finalize (GObject *obj);

GncSxInstanceModel* gnc_sx_slr_tree_model_adapter_get_instance_model (GncSxSlrTreeModelAdapter *slr_model);

/** @return null if the iter is not actually an GncSxInstance. **/
GncSxInstance* gnc_sx_slr_model_get_instance (GncSxSlrTreeModelAdapter *model, GtkTreeIter *iter);
static GncSxInstance* _gnc_sx_slr_model_get_instance (GncSxSlrTreeModelAdapter *model,
                                                      GtkTreeIter *iter,
                                                      gboolean check_depth);

/** @return false if the iter is not actually an GncSxInstance's variable. **/
gboolean gnc_sx_slr_model_get_instance_and_variable (GncSxSlrTreeModelAdapter *model,
                                                     GtkTreeIter *iter,
                                                     GncSxInstance **instance_loc,
                                                     GncSxVariable **var_loc);

void gnc_sx_slr_model_effect_change (GncSxSlrTreeModelAdapter *model,
                                     gboolean auto_create_only,
                                     GList **created_transaction_guids,
                                     GList **creation_errors);

GtkTreeModel* gnc_sx_get_slr_state_model (void);

#define GNC_TYPE_SX_SLR_TREE_MODEL_ADAPTER            (gnc_sx_slr_tree_model_adapter_get_type ())
#define GNC_SX_SLR_TREE_MODEL_ADAPTER(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_SX_SLR_TREE_MODEL_ADAPTER, GncSxSlrTreeModelAdapter))
#define GNC_SX_SLR_TREE_MODEL_ADAPTER_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_SX_SLR_TREE_MODEL_ADAPTER, GncSxSlrTreeModelAdapterClass))
#define GNC_IS_SX_SLR_TREE_MODEL_ADAPTER(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_SX_SLR_TREE_MODEL_ADAPTER))
#define GNC_IS_SX_SLR_TREE_MODEL_ADAPTER_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_SX_SLR_TREE_MODEL_ADAPTER))
#define GNC_SX_SLR_TREE_MODEL_ADAPTER_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_SX_SLR_TREE_MODEL_ADAPTER, GncSxSlrTreeModelAdapterClass))

/* ------------------------------------------------------------ */

static void _show_created_transactions (GncSxSinceLastRunDialog *app_dialog, GList *created_txn_guids);

static void close_handler (gpointer user_data);
static void dialog_destroy_cb (GtkWidget *object, GncSxSinceLastRunDialog *app_dialog);
static void dialog_response_cb (GtkDialog *dialog, gint response_id, GncSxSinceLastRunDialog *app_dialog);

#define debug_path(fn, text, path) {\
    gchar *path_string = gtk_tree_path_to_string (path);\
    fn("%s %s", text, path_string? path_string : "NULL");\
    g_free (path_string);\
}

/* ------------------------------------------------------------ */

static void
_var_numeric_to_string (gnc_numeric *value, GString **str)
{
    *str = g_string_sized_new (5);
    g_string_printf (*str, "%0.2f", gnc_numeric_to_double (*value));
}

/* ------------------------------------------------------------ */

G_DEFINE_TYPE_WITH_CODE (GncSxSlrTreeModelAdapter, gnc_sx_slr_tree_model_adapter, G_TYPE_OBJECT,
     G_IMPLEMENT_INTERFACE (GTK_TYPE_TREE_MODEL, gnc_sx_slr_tree_model_adapter_interface_init))

static void
gnc_sx_slr_tree_model_adapter_class_init (GncSxSlrTreeModelAdapterClass *klass)
{
    GObjectClass *obj_class = G_OBJECT_CLASS(klass);

    obj_class->dispose = gnc_sx_slr_tree_model_adapter_dispose;
    obj_class->finalize = gnc_sx_slr_tree_model_adapter_finalize;
}

static GtkTreeModelFlags
gsslrtma_get_flags (GtkTreeModel *tree_model)
{
    return gtk_tree_model_get_flags (GTK_TREE_MODEL(GNC_SX_SLR_TREE_MODEL_ADAPTER(tree_model)->real));
}

static gint
gsslrtma_get_n_columns (GtkTreeModel *tree_model)
{
    return gtk_tree_model_get_n_columns (GTK_TREE_MODEL(GNC_SX_SLR_TREE_MODEL_ADAPTER(tree_model)->real));
}

static GType
gsslrtma_get_column_type (GtkTreeModel *tree_model, gint index)
{
    return gtk_tree_model_get_column_type (GTK_TREE_MODEL(GNC_SX_SLR_TREE_MODEL_ADAPTER(tree_model)->real), index);
}

static gboolean
gsslrtma_get_iter (GtkTreeModel *tree_model,
                   GtkTreeIter *iter,
                   GtkTreePath *path)
{
    return gtk_tree_model_get_iter (GTK_TREE_MODEL(GNC_SX_SLR_TREE_MODEL_ADAPTER(tree_model)->real), iter, path);
}

static GtkTreePath*
gsslrtma_get_path (GtkTreeModel *tree_model,
                   GtkTreeIter *iter)
{
    return gtk_tree_model_get_path (GTK_TREE_MODEL(GNC_SX_SLR_TREE_MODEL_ADAPTER(tree_model)->real), iter);
}

static void
gsslrtma_get_value (GtkTreeModel *tree_model,
                    GtkTreeIter *iter,
                    gint column,
                    GValue *value)
{
    gtk_tree_model_get_value (GTK_TREE_MODEL(GNC_SX_SLR_TREE_MODEL_ADAPTER(tree_model)->real), iter, column, value);
}

static gboolean
gsslrtma_iter_next (GtkTreeModel *tree_model,
                    GtkTreeIter *iter)
{
    return gtk_tree_model_iter_next (GTK_TREE_MODEL(GNC_SX_SLR_TREE_MODEL_ADAPTER(tree_model)->real), iter);
}

static gboolean
gsslrtma_iter_children (GtkTreeModel *tree_model,
                        GtkTreeIter *iter,
                        GtkTreeIter *parent)
{
    return gtk_tree_model_iter_children (GTK_TREE_MODEL(GNC_SX_SLR_TREE_MODEL_ADAPTER(tree_model)->real), iter, parent);
}

static gboolean
gsslrtma_iter_has_child (GtkTreeModel *tree_model,
                         GtkTreeIter *iter)
{
    return gtk_tree_model_iter_has_child (GTK_TREE_MODEL(GNC_SX_SLR_TREE_MODEL_ADAPTER(tree_model)->real), iter);
}

static gint
gsslrtma_iter_n_children (GtkTreeModel *tree_model,
                          GtkTreeIter *iter)
{
    return gtk_tree_model_iter_n_children (GTK_TREE_MODEL(GNC_SX_SLR_TREE_MODEL_ADAPTER(tree_model)->real), iter);
}

static gboolean
gsslrtma_iter_nth_child (GtkTreeModel *tree_model,
                         GtkTreeIter *iter,
                         GtkTreeIter *parent,
                         gint n)
{
    return gtk_tree_model_iter_nth_child (GTK_TREE_MODEL(GNC_SX_SLR_TREE_MODEL_ADAPTER(tree_model)->real), iter, parent, n);
}

static gboolean
gsslrtma_iter_parent (GtkTreeModel *tree_model,
                      GtkTreeIter *iter,
                      GtkTreeIter *child)
{
    return gtk_tree_model_iter_parent (GTK_TREE_MODEL(GNC_SX_SLR_TREE_MODEL_ADAPTER(tree_model)->real), iter, child);
}

static void
gsslrtma_ref_node (GtkTreeModel *tree_model,
                   GtkTreeIter *iter)
{
    gtk_tree_model_ref_node (GTK_TREE_MODEL(GNC_SX_SLR_TREE_MODEL_ADAPTER(tree_model)->real), iter);
}

static void
gsslrtma_unref_node (GtkTreeModel *tree_model,
                     GtkTreeIter *iter)
{
    gtk_tree_model_unref_node (GTK_TREE_MODEL(GNC_SX_SLR_TREE_MODEL_ADAPTER(tree_model)->real), iter);
}

static void
gnc_sx_slr_tree_model_adapter_interface_init (GtkTreeModelIface *tree_model)
{
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
gsslrtma_proxy_row_changed (GtkTreeModel *treemodel,
                            GtkTreePath *arg1,
                            GtkTreeIter *arg2,
                            gpointer user_data)
{
    g_signal_emit_by_name (user_data, "row-changed", arg1, arg2);
}

static void
gsslrtma_proxy_row_deleted (GtkTreeModel *treemodel,
                            GtkTreePath *arg1,
                            gpointer user_data)
{
    g_signal_emit_by_name (user_data, "row-deleted", arg1);
}

static void
gsslrtma_proxy_row_has_child_toggled (GtkTreeModel *treemodel,
                                      GtkTreePath *arg1,
                                      GtkTreeIter *arg2,
                                      gpointer user_data)
{
    g_signal_emit_by_name (user_data, "row-has-child-toggled", arg1, arg2);
}

static void
gsslrtma_proxy_row_inserted (GtkTreeModel *treemodel,
                             GtkTreePath *arg1,
                             GtkTreeIter *arg2,
                             gpointer user_data)
{
    g_signal_emit_by_name (user_data, "row-inserted", arg1, arg2);
}

static void
gsslrtma_proxy_rows_reordered (GtkTreeModel *treemodel,
                               GtkTreePath *arg1,
                               GtkTreeIter *arg2,
                               gpointer arg3,
                               gpointer user_data)
{
    g_signal_emit_by_name (user_data, "rows-reordered", arg1, arg2, arg3);
}

// model columns
enum
{
    SLR_MODEL_COL_NAME = 0,
    SLR_MODEL_COL_INSTANCE_PTR,
    SLR_MODEL_COL_INSTANCE_STATE,
    SLR_MODEL_COL_VARAIBLE_VALUE,
    SLR_MODEL_COL_INSTANCE_VISIBILITY,
    SLR_MODEL_COL_VARIABLE_VISIBILITY,
    SLR_MODEL_COL_INSTANCE_STATE_SENSITIVITY,
    SLR_MODEL_COL_INSTANCE_DATE,
};

static void
gnc_sx_slr_tree_model_adapter_init (GncSxSlrTreeModelAdapter *adapter)
{
    // columns:    thing-name, ptr,       instance-state, variable-value, instance-visible, variable-visible, instance_state_sensitivity, date
    // at depth=0: <sx>,       instances, N/A,            N/A             N/A,              N/A               N/A                         N/A
    // at depth=1: <instance>, instance,  <state>,        N/A,            <valid>,          N/A,              <valid>                     <date>
    // at depth=2: <variable>, var,       N/A,            <value>,        N/A,              <valid>,          N/A                         N/A
    adapter->real = gtk_tree_store_new (8, G_TYPE_STRING, G_TYPE_POINTER, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_BOOLEAN, G_TYPE_BOOLEAN, G_TYPE_BOOLEAN, G_TYPE_INT64);

    g_signal_connect (adapter->real, "row-changed", G_CALLBACK(gsslrtma_proxy_row_changed), adapter);
    g_signal_connect (adapter->real, "row-deleted", G_CALLBACK(gsslrtma_proxy_row_deleted), adapter);
    g_signal_connect (adapter->real, "row-has-child-toggled", G_CALLBACK(gsslrtma_proxy_row_has_child_toggled), adapter);
    g_signal_connect (adapter->real, "row-inserted", G_CALLBACK(gsslrtma_proxy_row_inserted), adapter);
    g_signal_connect (adapter->real, "rows-reordered", G_CALLBACK(gsslrtma_proxy_rows_reordered), adapter);
}

static char* gnc_sx_instance_state_names[] =
{
    N_("Ignored"),
    N_("Postponed"),
    N_("To-Create"),
    N_("Reminder"),
    N_("Created"),
    NULL
};

static GtkTreeModel* _singleton_slr_state_model = NULL;

GtkTreeModel*
gnc_sx_get_slr_state_model (void)
{
    if (_singleton_slr_state_model == NULL)
    {
        int i;
        GtkTreeIter iter;

        _singleton_slr_state_model = GTK_TREE_MODEL(gtk_list_store_new (1, G_TYPE_STRING));
        for (i = 0; i != SX_INSTANCE_STATE_CREATED; i++)
        {
            gtk_list_store_insert_with_values (GTK_LIST_STORE(_singleton_slr_state_model),
                                               &iter,
                                               SX_INSTANCE_STATE_MAX_STATE + 1,
                                               0, _(gnc_sx_instance_state_names[i]), -1);
        }
    }
    return _singleton_slr_state_model;
}

static void
_consume_excess_rows (GtkTreeStore *store, int last_index, GtkTreeIter *parent_iter, GtkTreeIter *maybe_invalid_iter)
{
    if (last_index == -1)
    {
        // try to get whatever was there beforehand, if it exists
        if (!gtk_tree_model_iter_children (GTK_TREE_MODEL(store), maybe_invalid_iter, parent_iter))
            return;
    }
    else
    {
        // increment the iter, or bail out.
        if (!gtk_tree_model_iter_next (GTK_TREE_MODEL(store), maybe_invalid_iter))
            return;
    }

    // consume until we're done.
    while (gtk_tree_store_remove (store, maybe_invalid_iter));
}


static void
gsslrtma_populate_tree_store (GncSxSlrTreeModelAdapter *model)
{
    GtkTreeIter sx_tree_iter;
    GList *sx_iter;
    int instances_index = -1;

    for (sx_iter = gnc_sx_instance_model_get_sx_instances_list (model->instances); sx_iter != NULL; sx_iter = sx_iter->next)
    {
        GncSxInstances *instances = (GncSxInstances*)sx_iter->data;
        char last_occur_date_buf[MAX_DATE_LENGTH+1];

        {
            const GDate *last_occur = xaccSchedXactionGetLastOccurDate (instances->sx);
            if (last_occur == NULL || !g_date_valid (last_occur))
            {
                g_stpcpy (last_occur_date_buf, _("Never"));
            }
            else
            {
                qof_print_gdate (last_occur_date_buf,
                                 MAX_DATE_LENGTH,
                                 last_occur);
            }
        }

        // if there are no instances for the instance skip adding
        if (g_list_length (instances->instance_list) == 0)
            continue;

        if (!gtk_tree_model_iter_nth_child (GTK_TREE_MODEL(model->real), &sx_tree_iter, NULL, ++instances_index))
        {
            gtk_tree_store_append (model->real, &sx_tree_iter, NULL);
        }

        gtk_tree_store_set (model->real, &sx_tree_iter,
                            SLR_MODEL_COL_NAME, xaccSchedXactionGetName (instances->sx),
                            SLR_MODEL_COL_INSTANCE_STATE, NULL,
                            SLR_MODEL_COL_VARAIBLE_VALUE, NULL,
                            SLR_MODEL_COL_INSTANCE_VISIBILITY, FALSE,
                            SLR_MODEL_COL_VARIABLE_VISIBILITY, FALSE,
                            SLR_MODEL_COL_INSTANCE_STATE_SENSITIVITY, FALSE,
                            SLR_MODEL_COL_INSTANCE_DATE, INT64_MAX,
                            SLR_MODEL_COL_INSTANCE_PTR, instances,
                            -1);

        if (qof_log_check (GNC_MOD_GUI_SX, QOF_LOG_DEBUG))
        {
            gchar *path_str = gtk_tree_path_to_string (gtk_tree_model_get_path (GTK_TREE_MODEL(model->real), &sx_tree_iter));
            DEBUG("Add schedule [%s], instances %p at path [%s]", xaccSchedXactionGetName (instances->sx), instances, path_str);
            g_free (path_str);
        }

        // Insert instance information
        {
            GList *inst_iter;
            GtkTreeIter inst_tree_iter;
            char instance_date_buf[MAX_DATE_LENGTH+1];
            int instance_index = -1;

            for (inst_iter = instances->instance_list; inst_iter != NULL; inst_iter = inst_iter->next)
            {
                GncSxInstance *inst = (GncSxInstance*)inst_iter->data;
                qof_print_gdate (instance_date_buf, MAX_DATE_LENGTH, &inst->date);
                time64 t = gdate_to_time64 (inst->date);

                if (!gtk_tree_model_iter_nth_child (GTK_TREE_MODEL(model->real), &inst_tree_iter, &sx_tree_iter, ++instance_index))
                {
                    gtk_tree_store_append (model->real, &inst_tree_iter, &sx_tree_iter);
                }
                gtk_tree_store_set (model->real, &inst_tree_iter,
                                    SLR_MODEL_COL_NAME, instance_date_buf,
                                    SLR_MODEL_COL_INSTANCE_STATE, _(gnc_sx_instance_state_names[inst->state]),
                                    SLR_MODEL_COL_VARAIBLE_VALUE, NULL,
                                    SLR_MODEL_COL_INSTANCE_VISIBILITY, TRUE,
                                    SLR_MODEL_COL_VARIABLE_VISIBILITY, FALSE,
                                    SLR_MODEL_COL_INSTANCE_STATE_SENSITIVITY, inst->state != SX_INSTANCE_STATE_CREATED,
                                    SLR_MODEL_COL_INSTANCE_DATE, t,
                                    SLR_MODEL_COL_INSTANCE_PTR, inst,
                                    -1);

                // Insert variable information
                {
                    GList *vars = NULL, *var_iter;
                    GtkTreeIter var_tree_iter;
                    gint visible_variable_index = -1;

                    vars = gnc_sx_instance_get_variables (inst);
                    for (var_iter = vars; var_iter != NULL; var_iter = var_iter->next)
                    {
                        GncSxVariable *var = (GncSxVariable*)var_iter->data;
                        GString *tmp_str;

                        if (!var->editable)
                            continue;

                        if (gnc_numeric_check (var->value) == GNC_ERROR_OK)
                        {
                            _var_numeric_to_string (&var->value, &tmp_str);
                        }
                        else
                        {
                            tmp_str = g_string_new (_("(Need Value)"));
                        }

                        if (!gtk_tree_model_iter_nth_child (GTK_TREE_MODEL(model->real),
                                                            &var_tree_iter, &inst_tree_iter,
                                                            ++visible_variable_index))
                        {
                            gtk_tree_store_append (model->real, &var_tree_iter, &inst_tree_iter);
                        }
                        gtk_tree_store_set (model->real, &var_tree_iter,
                                            SLR_MODEL_COL_NAME, var->name,
                                            SLR_MODEL_COL_INSTANCE_STATE, NULL,
                                            SLR_MODEL_COL_VARAIBLE_VALUE, tmp_str->str,
                                            SLR_MODEL_COL_INSTANCE_VISIBILITY, FALSE,
                                            SLR_MODEL_COL_VARIABLE_VISIBILITY, TRUE,
                                            SLR_MODEL_COL_INSTANCE_STATE_SENSITIVITY, FALSE,
                                            SLR_MODEL_COL_INSTANCE_DATE, INT64_MAX,
                                            SLR_MODEL_COL_INSTANCE_PTR, var,
                                            -1);
                        g_string_free (tmp_str, TRUE);
                    }
                    g_list_free (vars);

                    _consume_excess_rows (model->real, visible_variable_index, &inst_tree_iter, &var_tree_iter);
                }
            }

            // if there are more instance iters, remove
            _consume_excess_rows (model->real, instance_index, &sx_tree_iter, &inst_tree_iter);
        }
    }
    _consume_excess_rows (model->real, instances_index, NULL, &sx_tree_iter);
}

GncSxInstanceModel*
gnc_sx_slr_tree_model_adapter_get_instance_model (GncSxSlrTreeModelAdapter *slr_model)
{
    return slr_model->instances;
}

GncSxInstance*
gnc_sx_slr_model_get_instance (GncSxSlrTreeModelAdapter *model, GtkTreeIter *iter)
{
    return _gnc_sx_slr_model_get_instance (model, iter, TRUE);
}

static GncSxInstance*
_gnc_sx_slr_model_get_instance (GncSxSlrTreeModelAdapter *model, GtkTreeIter *iter, gboolean check_depth)
{
    GtkTreePath *model_path = gtk_tree_model_get_path (GTK_TREE_MODEL(model), iter);
    gint *indices, instances_index, instance_index;
    GncSxInstance *instance = NULL;
    GtkTreeIter new_iter;

    debug_path (DEBUG, "model path is:", model_path);

    if (check_depth && gtk_tree_path_get_depth (model_path) != 2)
    {
        PWARN("path depth not equal to 2");
        gtk_tree_path_free (model_path);
        return NULL;
    }

    if (gtk_tree_path_get_depth (model_path) == 1)
    {
        PWARN("path depth equal to 1");
        gtk_tree_path_free (model_path);
        return NULL;
    }

    indices = gtk_tree_path_get_indices (model_path);
    instances_index = indices[0];
    instance_index = indices[1];

    gtk_tree_path_free (model_path);

    model_path = gtk_tree_path_new_from_indices (instances_index, instance_index, -1);

    debug_path (DEBUG, "new model path is:", model_path);

    if (gtk_tree_model_get_iter (GTK_TREE_MODEL(model), &new_iter, model_path))
        gtk_tree_model_get (GTK_TREE_MODEL(model), &new_iter, SLR_MODEL_COL_INSTANCE_PTR, &instance, -1);

    gtk_tree_path_free (model_path);

    DEBUG("instance is %p", instance);

    return instance;
}

gboolean
gnc_sx_slr_model_get_instance_and_variable (GncSxSlrTreeModelAdapter *model, GtkTreeIter *iter, GncSxInstance **instance_loc, GncSxVariable **var_loc)
{
    GtkTreePath *model_path;
    gint *indices, variable_index;
    GncSxInstance *instance;
    GList *variables;

    instance = _gnc_sx_slr_model_get_instance (model, iter, FALSE);
    if (instance == NULL)
    {
        gchar *iter_str = gtk_tree_model_get_string_from_iter (GTK_TREE_MODEL(model), iter);
        PWARN("instance is NULL for iter %s", iter_str);
        g_free (iter_str);
        return FALSE;
    }
    variables = gnc_sx_instance_get_variables (instance);

    model_path = gtk_tree_model_get_path (GTK_TREE_MODEL(model), iter);
    if (gtk_tree_path_get_depth (model_path) != 3)
    {
        gchar *path_str = gtk_tree_path_to_string (model_path);
        PWARN("invalid path [%s] for variable, not at depth 3", path_str);
        gtk_tree_path_free (model_path);
        g_free (path_str);
        return FALSE;
    }

    debug_path (DEBUG, "model path is:", model_path);

    indices = gtk_tree_path_get_indices (model_path);
    variable_index = indices[2];

    gtk_tree_path_free (model_path);

    if (variable_index < 0 || variable_index >= g_list_length (variables))
    {
        PWARN("variable index %d out of range", variable_index);
        g_list_free (variables);
        return FALSE;
    }

    if (instance_loc != NULL)
    {
        *instance_loc = instance;
    }

    if (var_loc != NULL)
    {
        GncSxVariable *var;

        gtk_tree_model_get (GTK_TREE_MODEL(model), iter, SLR_MODEL_COL_INSTANCE_PTR, &var, -1);

        *var_loc = var;
    }
    g_list_free (variables);
    return TRUE;
}

/**
 * Special-case list indexing that only refers to "editable" variables. :(
 **/
static gint
_variable_list_index (GList *variables, GncSxVariable *variable)
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

typedef struct _findInstanceData
{
    gpointer     find_item;
    GtkTreePath *found_path;
} FindInstanceData;

static gboolean
for_each_find_item (GtkTreeModel *model, GtkTreePath *path, GtkTreeIter *iter, gpointer user_data)
{
    FindInstanceData* to_find = (FindInstanceData*)user_data;
    gpointer item;

    gtk_tree_model_get (model, iter, SLR_MODEL_COL_INSTANCE_PTR, &item, -1);

    if (item == to_find->find_item)
    {
        to_find->found_path = gtk_tree_path_copy (path);
        return TRUE;
    }
    return FALSE;
}

static GtkTreePath*
_get_model_path_for_item (GtkTreeModel *model, gpointer find_item)
{
    GtkTreePath *model_path = NULL;
    FindInstanceData* to_find_data;

    to_find_data = (FindInstanceData*)g_new0 (FindInstanceData, 1);
    to_find_data->find_item = find_item;
    to_find_data->found_path = NULL;

    gtk_tree_model_foreach (model, (GtkTreeModelForeachFunc)for_each_find_item, to_find_data);

    if (to_find_data->found_path)
    {
        model_path = gtk_tree_path_copy (to_find_data->found_path);
        gtk_tree_path_free (to_find_data->found_path);
    }
    g_free (to_find_data);

    return model_path;
}

static GtkTreePath*
_get_model_path_for_instance (GtkTreeModel *model, GncSxInstance *instance)
{
    return _get_model_path_for_item (model, instance);
}

static GtkTreePath*
_get_model_path_for_instances (GtkTreeModel *model, GncSxInstances *instances)
{
    return _get_model_path_for_item (model, instances);
}

static GtkTreePath*
_get_path_for_variable (GncSxSinceLastRunDialog *app_dialog, GncSxInstance *instance, GncSxVariable *variable)
{
    GncSxSlrTreeModelAdapter *model = app_dialog->editing_model;
    GtkTreeModel *sort_model = gtk_tree_view_get_model (app_dialog->instance_view);
    gint *indices, instances_index, instance_index, variable_index;
    GtkTreePath *view_path, *model_path;
    GList *variables;

    model_path = _get_model_path_for_instance (GTK_TREE_MODEL(model), instance);

    if (!model_path)
    {
        PWARN("model path is NULL for instance %p", instance);
        return NULL;
    }

    debug_path (DEBUG, "instance model path is:", model_path);

    indices = gtk_tree_path_get_indices (model_path);
    instances_index = indices[0];
    instance_index = indices[1];

    gtk_tree_path_free (model_path);

    variables = gnc_sx_instance_get_variables (instance);
    variable_index = _variable_list_index (variables, variable);
    g_list_free (variables);
    if (variable_index == -1)
        return NULL;

    model_path = gtk_tree_path_new_from_indices (instances_index, instance_index, variable_index, -1);
    debug_path (DEBUG, "model variable path is:", model_path);

    view_path = gtk_tree_model_sort_convert_child_path_to_path (GTK_TREE_MODEL_SORT(sort_model), model_path);
    gtk_tree_path_free (model_path);

    debug_path (DEBUG, "return view variable path is:", view_path);

    return view_path;
}

static void
gsslrtma_added_cb (GncSxInstanceModel *instances, SchedXaction *added_sx, gpointer user_data)
{
    GncSxSlrTreeModelAdapter *model = GNC_SX_SLR_TREE_MODEL_ADAPTER(user_data);
    // this is wasteful, but fine.
    gsslrtma_populate_tree_store (model);
}

static void
gsslrtma_updated_cb (GncSxInstanceModel *instances, SchedXaction *updated_sx, gpointer user_data)
{
    GncSxSlrTreeModelAdapter *model = GNC_SX_SLR_TREE_MODEL_ADAPTER(user_data);
    gnc_sx_instance_model_update_sx_instances (instances, updated_sx);
    gsslrtma_populate_tree_store (model);
}

static void
gsslrtma_removing_cb (GncSxInstanceModel *inst_model, SchedXaction *to_remove_sx, gpointer user_data)
{
    GncSxSlrTreeModelAdapter *model = GNC_SX_SLR_TREE_MODEL_ADAPTER(user_data);
    GtkTreePath *model_path;
    GtkTreeIter tree_iter;
    GList *iter;
    int index = 0;
    GncSxInstances *instances;

    // get index, create path, remove
    for (iter = gnc_sx_instance_model_get_sx_instances_list (inst_model); iter != NULL; iter = iter->next, index++)
    {
        instances = (GncSxInstances*)iter->data;
        if (instances->sx == to_remove_sx)
            break;
    }
    if (iter == NULL)
    {
        PWARN("could not find sx %p in the model", to_remove_sx);
        return; // couldn't find sx in our model, which is weird.
    }

    model_path = _get_model_path_for_instances (GTK_TREE_MODEL(model), instances);

    debug_path (DEBUG, "remove model_path", model_path);

    if (!gtk_tree_model_get_iter (GTK_TREE_MODEL(model->real), &tree_iter, model_path))
    {
        gchar *path_str = gtk_tree_path_to_string (model_path);
        PWARN("invalid path [%s] for instances %p to remove", path_str, instances);
        gtk_tree_path_free (model_path);
        g_free (path_str);
        return;
    }
    gtk_tree_path_free (model_path);

    gtk_tree_store_remove (model->real, &tree_iter);

    gnc_sx_instance_model_remove_sx_instances (inst_model, to_remove_sx);
}

static void
gnc_sx_slr_tree_model_adapter_dispose (GObject *obj)
{
    GncSxSlrTreeModelAdapter *adapter;
    g_return_if_fail (obj != NULL);
    adapter = GNC_SX_SLR_TREE_MODEL_ADAPTER(obj);
    g_return_if_fail (!adapter->disposed);
    adapter->disposed = TRUE;

    g_object_unref (G_OBJECT(adapter->instances));
    adapter->instances = NULL;
    g_object_unref (G_OBJECT(adapter->real));
    adapter->real = NULL;

    G_OBJECT_CLASS(gnc_sx_slr_tree_model_adapter_parent_class)->dispose (obj);
}

static void
gnc_sx_slr_tree_model_adapter_finalize (GObject *obj)
{
    g_return_if_fail (obj != NULL);
    G_OBJECT_CLASS(gnc_sx_slr_tree_model_adapter_parent_class)->finalize (obj);
}

GncSxSlrTreeModelAdapter*
gnc_sx_slr_tree_model_adapter_new (GncSxInstanceModel *instances)
{
    GncSxSlrTreeModelAdapter *rtn;
    rtn = GNC_SX_SLR_TREE_MODEL_ADAPTER(g_object_new (GNC_TYPE_SX_SLR_TREE_MODEL_ADAPTER, NULL));
    rtn->instances = instances;
    g_object_ref (G_OBJECT(rtn->instances));
    gsslrtma_populate_tree_store (rtn);
    g_signal_connect (G_OBJECT(rtn->instances), "added", (GCallback)gsslrtma_added_cb, (gpointer)rtn);
    rtn->updated_cb_id = g_signal_connect (G_OBJECT(rtn->instances), "updated", (GCallback)gsslrtma_updated_cb, (gpointer)rtn);
    g_signal_connect (G_OBJECT(rtn->instances), "removing", (GCallback)gsslrtma_removing_cb, (gpointer)rtn);
    return rtn;
}

void
gnc_ui_sx_creation_error_dialog (GList **creation_errors)
{
    GtkWidget *dialog = NULL;
    gchar *message = NULL;
    if (*creation_errors == NULL) return;
    message = gnc_g_list_stringjoin (*creation_errors, "\n");
    g_list_free_full (*creation_errors, g_free);
    creation_errors = NULL;
    dialog = gtk_message_dialog_new (NULL, 0,
                                     GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE,
                                     "\t%s\t", _("Invalid Transactions"));
    gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
                                              "%s", message);
    g_signal_connect_swapped (dialog, "response",
                              G_CALLBACK(gtk_widget_destroy), dialog);
    gtk_dialog_run (GTK_DIALOG(dialog));
    g_free (message);
}

void
gnc_sx_sxsincelast_book_opened (void)
{
    GList *auto_created_txns = NULL;
    GList *creation_errors = NULL;
    GncSxInstanceModel *inst_model;
    GncSxSummary summary;

    if (!gnc_prefs_get_bool (GNC_PREFS_GROUP_STARTUP, GNC_PREF_RUN_AT_FOPEN))
        return;

    if (qof_book_is_readonly (gnc_get_current_book ()))
    {
        /* Is the book read-only? Then don't change anything here. */
        return;
    }

    inst_model = gnc_sx_get_current_instances ();
    gnc_sx_instance_model_summarize (inst_model, &summary);
    gnc_sx_summary_print (&summary);
    gnc_sx_instance_model_effect_change (inst_model, TRUE, &auto_created_txns,
                                         &creation_errors);

    if (auto_created_txns)
        gnc_gui_refresh_all();

    if (summary.need_dialog)
    {
        gnc_ui_sx_since_last_run_dialog (gnc_ui_get_main_window (NULL), inst_model, auto_created_txns);
        /* gnc_ui_sx_since_last_run_dialog now owns this list */
        auto_created_txns = NULL;
    }
    else
    {
        g_list_free (auto_created_txns);

        if (summary.num_auto_create_no_notify_instances != 0
                && gnc_prefs_get_bool (GNC_PREFS_GROUP_STARTUP, GNC_PREF_SHOW_AT_FOPEN))
        {
            gnc_info_dialog
            (gnc_ui_get_main_window (NULL),
             ngettext
             ("There are no Scheduled Transactions to be entered at this time. "
              "(One transaction automatically created)",
              "There are no Scheduled Transactions to be entered at this time. "
              "(%d transactions automatically created)",
              summary.num_auto_create_no_notify_instances),
              summary.num_auto_create_no_notify_instances);
        }
    }

    g_object_unref (G_OBJECT(inst_model));

    if (creation_errors)
        gnc_ui_sx_creation_error_dialog (&creation_errors);
}

static GtkTreePath *
instance_get_model_path (GtkTreeView *view, const gchar *path)
{
    GtkTreePath *view_path = gtk_tree_path_new_from_string (path);
    GtkTreeModelSort *sort_model = GTK_TREE_MODEL_SORT(gtk_tree_view_get_model (view));

    GtkTreePath *model_path = gtk_tree_model_sort_convert_path_to_child_path (sort_model, view_path);

    gtk_tree_path_free (view_path);

    return model_path;
}

static void
instance_state_changed_cb (GtkCellRendererText *cell,
                           const gchar *path,
                           const gchar *value,
                           GncSxSinceLastRunDialog *dialog)
{
    GncSxInstance *inst;
    int i;
    GncSxInstanceState new_state;
    GtkTreePath *model_path = instance_get_model_path (dialog->instance_view, path);
    GtkTreeIter tree_iter;

    DEBUG("change instance state to [%s] at path [%s]", value, path);

    debug_path (DEBUG, "instance model path is:", model_path);

    for (i = 0; i < SX_INSTANCE_STATE_CREATED; i++)
    {
        if (strcmp (value, _(gnc_sx_instance_state_names[i])) == 0)
            break;
    }
    if (i == SX_INSTANCE_STATE_CREATED)
    {
        PWARN("unknown value [%s]", value);
        return;
    }
    new_state = i;

    if (!gtk_tree_model_get_iter (GTK_TREE_MODEL(dialog->editing_model), &tree_iter, model_path))
    {
        gtk_tree_path_free (model_path);
        PWARN("unknown path [%s]", path);
        return;
    }
    gtk_tree_path_free (model_path);

    inst = gnc_sx_slr_model_get_instance (dialog->editing_model, &tree_iter);

    if (inst == NULL)
    {
        PWARN("invalid path [%s]", path);
        return;
    }

    DEBUG("instance is %p", inst);

    gnc_sx_instance_model_change_instance_state (dialog->editing_model->instances, inst, new_state);
}

static void
control_scroll_bars (GncSxSinceLastRunDialog *dialog)
{
    GtkWidget *sw = gtk_widget_get_parent (GTK_WIDGET(dialog->instance_view));
    GtkWidget *vsbar = gtk_scrolled_window_get_vscrollbar (GTK_SCROLLED_WINDOW(sw));
    gboolean enable = TRUE;

    if (dialog->temp_ce)
        enable = FALSE;

    gtk_widget_set_sensitive (vsbar, enable);
    gtk_widget_set_visible (vsbar, enable);
}

static void
variable_value_changed_cb (GtkCellRendererText *cell,
                           const gchar *path,
                           const gchar *value,
                           GncSxSinceLastRunDialog *dialog)
{
    GncSxVariable *var = NULL;
    GncSxInstance *inst;
    gnc_numeric parsed_num;
    char *endStr = NULL;
    GtkTreePath *model_path = instance_get_model_path (dialog->instance_view, path);
    GtkTreeIter tree_iter;

    DEBUG("change variable to [%s] at view path [%s]", value, path);

    debug_path (DEBUG, "instance model path is:", model_path);

    dialog->temp_ce = NULL;
    control_scroll_bars (dialog);

    if (!gtk_tree_model_get_iter (GTK_TREE_MODEL(dialog->editing_model), &tree_iter, model_path))
    {
        gtk_tree_path_free (model_path);
        PWARN("invalid path [%s]", path);
        return;
    }
    gtk_tree_path_free (model_path);

    if (!gnc_sx_slr_model_get_instance_and_variable (dialog->editing_model, &tree_iter, &inst, &var))
    {
        PWARN("path [%s] doesn't correspond to a valid variable", path);
        return;
    }

    if (!xaccParseAmount (value, TRUE, &parsed_num, &endStr)
            || gnc_numeric_check (parsed_num) != GNC_ERROR_OK)
    {
        gchar *value_copy = g_strdup (value);
        DEBUG ("value=[%s] endStr[%s]", value, endStr);
        if (strlen (g_strstrip (value_copy)) == 0)
        {
            gnc_numeric invalid_num = gnc_numeric_error (GNC_ERROR_ARG);
            gnc_sx_instance_model_set_variable (dialog->editing_model->instances, inst, var, &invalid_num);
        }
        else
        {
            PWARN("error parsing value [%s]", value);
        }
        g_free (value_copy);
        return;
    }

    if (inst->state == SX_INSTANCE_STATE_REMINDER)
    {
        gnc_sx_instance_model_change_instance_state (dialog->editing_model->instances, inst,
                                                     SX_INSTANCE_STATE_TO_CREATE);
    }
    gnc_sx_instance_model_set_variable (dialog->editing_model->instances, inst, var, &parsed_num);
}

static void
variable_value_start_changed_cb (GtkCellRenderer *renderer, GtkCellEditable *editable,
                                 gchar *path, gpointer user_data)
{
    GncSxSinceLastRunDialog *dialog = user_data;
    dialog->temp_ce = editable;
    control_scroll_bars (dialog);
}

static void
variable_value_cancel_changed_cb (GtkCellRenderer *renderer, gpointer user_data)
{
    GncSxSinceLastRunDialog *dialog = user_data;
    dialog->temp_ce = NULL;
    control_scroll_bars (dialog);
}

static gint
_sort_text (const gchar *text_a, const gchar *text_b)
{
    gchar *a_caseless, *b_caseless;
    gint rtn = 0;

    if (text_a == NULL && text_b == NULL) return 0;
    if (text_a == NULL) return 1;
    if (text_b == NULL) return -1;

    a_caseless = g_utf8_casefold (text_a, -1);
    b_caseless = g_utf8_casefold (text_b, -1);
    rtn = g_strcmp0 (a_caseless, b_caseless);
    g_free (a_caseless);
    g_free (b_caseless);

    return rtn;
}

static gint
_transaction_sort_func_date (GtkTreeModel *model, GtkTreeIter *iter_a, GtkTreeIter *iter_b)
{
    GtkTreePath *path_a = gtk_tree_model_get_path (model, iter_a);
    gint depth = gtk_tree_path_get_depth (path_a);
    gint64 date_a = 0, date_b = 0;
    gint rtn = 0;

    gtk_tree_path_free (path_a);

    if (depth == 3)
        return rtn;

    // if top level, look at the first date for order
    if (depth == 1)
    {
        GtkTreeIter child_iter_a, child_iter_b;

        if (gtk_tree_model_iter_nth_child (model, &child_iter_a, iter_a, 0))
            gtk_tree_model_get (model, &child_iter_a, SLR_MODEL_COL_INSTANCE_DATE, &date_a, -1);

        if (gtk_tree_model_iter_nth_child (model, &child_iter_b, iter_b, 0))
            gtk_tree_model_get (model, &child_iter_b, SLR_MODEL_COL_INSTANCE_DATE, &date_b, -1);

        if (date_a > date_b)
            rtn = 1;
        if (date_b > date_a)
            rtn = -1;

        if (rtn == 0) // if dates are equal, look at name
        {
            gchar *name_text_a, *name_text_b;

            gtk_tree_model_get (model, iter_a, SLR_MODEL_COL_NAME, &name_text_a, -1);
            gtk_tree_model_get (model, iter_b, SLR_MODEL_COL_NAME, &name_text_b, -1);

            rtn = _sort_text (name_text_a, name_text_b);

            g_free (name_text_a);
            g_free (name_text_b);
        }
        return rtn;
    }

    gtk_tree_model_get (model, iter_a, SLR_MODEL_COL_INSTANCE_DATE, &date_a, -1);
    gtk_tree_model_get (model, iter_b, SLR_MODEL_COL_INSTANCE_DATE, &date_b, -1);

    if (date_a > date_b)
        rtn = 1;
    if (date_b > date_a)
        rtn = -1;

    return rtn;
}

static gint
_transaction_sort_func_desc (GtkTreeModel *model, GtkTreeIter *iter_a, GtkTreeIter *iter_b)
{
    GtkTreePath *path_a = gtk_tree_model_get_path (model, iter_a);
    gint depth = gtk_tree_path_get_depth (path_a);
    gchar *name_text_a, *name_text_b;
    gint rtn = 0;

    gtk_tree_path_free (path_a);

    if (depth == 3)
        return rtn;

    if (depth == 1)
    {
        gtk_tree_model_get (model, iter_a, SLR_MODEL_COL_NAME, &name_text_a, -1);
        gtk_tree_model_get (model, iter_b, SLR_MODEL_COL_NAME, &name_text_b, -1);

        rtn = _sort_text (name_text_a, name_text_b);

        g_free (name_text_a);
        g_free (name_text_b);
    }

    if (depth == 2)
    {
        gint64 date_a = 0, date_b = 0;

        gtk_tree_model_get (model, iter_a, SLR_MODEL_COL_INSTANCE_DATE, &date_a, -1);
        gtk_tree_model_get (model, iter_b, SLR_MODEL_COL_INSTANCE_DATE, &date_b, -1);

        if (date_a > date_b)
            rtn = 1;
        if (date_b > date_a)
            rtn = -1;
    }
    return rtn;
}

static gint
_transaction_sort_func (GtkTreeModel *model, GtkTreeIter *iter_a, GtkTreeIter *iter_b, gpointer user_data)
{
    GncSxSinceLastRunDialog *dialog = user_data;

    if (dialog->sort_selection_depth == 1)
        return _transaction_sort_func_desc (model, iter_a, iter_b);
    else
        return _transaction_sort_func_date (model, iter_a, iter_b);
}

static gboolean
finish_editing_before_ok_cb (GtkWidget *button, GdkEvent *event,
                             GncSxSinceLastRunDialog *dialog)
{
    // finish editing
    if (dialog->temp_ce)
        gtk_cell_editable_editing_done (dialog->temp_ce);

    dialog->temp_ce = NULL;

    return FALSE;
}

static gboolean
scroll_event (GtkWidget *widget, GdkEventScroll *event, gpointer user_data)
{
    GncSxSinceLastRunDialog *dialog = user_data;

    if (dialog->temp_ce)
        return TRUE;
    else
        return FALSE;
}

static void
set_transaction_sort_column_tooltip (GncSxSinceLastRunDialog *dialog)
{
    GtkTreeViewColumn *col = gtk_tree_view_get_column (GTK_TREE_VIEW(dialog->instance_view), 0);
    const gchar *date_text = _("Highlight a date first to sort by occurrence date.");
    const gchar *sched_text = _("Highlight a schedule first to sort by schedule name.");
    gchar *tooltip;

    if (dialog->sort_selection_depth == 1)
        tooltip = g_strconcat (sched_text, " *\n", date_text, NULL);
    else
        tooltip = g_strconcat (sched_text, "\n", date_text, " *", NULL);

    gtk_widget_set_tooltip_text (gtk_tree_view_column_get_button (col), tooltip);
    g_free (tooltip);
}

static gboolean
follow_select_tree_path (GtkTreeView *view)
{
    GtkTreeSelection *selection = gtk_tree_view_get_selection (view);
    GtkTreeModel *sort_model;
    GtkTreeIter iter;

    if (gtk_tree_selection_get_selected (selection, &sort_model, &iter))
    {
        GtkTreePath *view_path = gtk_tree_model_get_path (sort_model, &iter);

        gtk_tree_view_scroll_to_cell (view, view_path, NULL, TRUE, 0.5, 0.0);

        gtk_tree_path_free (view_path);
    }
    return FALSE;
}

static void
sort_column_changed (GtkTreeSortable* self, gpointer user_data)
{
    // this is triggered before a sort change
    GncSxSinceLastRunDialog *dialog = user_data;
    GtkTreeIter iter;
    GtkTreeSelection *selection = gtk_tree_view_get_selection (dialog->instance_view);
    GtkTreeModel *sort_model;

    if (gtk_tree_selection_get_selected (selection, &sort_model, &iter))
    {
        GtkTreePath *view_path = gtk_tree_model_get_path (sort_model, &iter);

        dialog->sort_selection_depth = gtk_tree_path_get_depth (view_path);

        gtk_tree_path_free (view_path);
    }
    else
        dialog->sort_selection_depth = 1;

    set_transaction_sort_column_tooltip (dialog);

    g_idle_add ((GSourceFunc)follow_select_tree_path, dialog->instance_view);
}

GncSxSinceLastRunDialog*
gnc_ui_sx_since_last_run_dialog (GtkWindow *parent, GncSxInstanceModel *sx_instances, GList *auto_created_txn_guids)
{
    GncSxSinceLastRunDialog *dialog;
    GtkBuilder *builder;
    GtkWidget *ok_button;

    dialog = g_new0 (GncSxSinceLastRunDialog, 1);

    builder = gtk_builder_new ();
    gnc_builder_add_from_file (builder, "dialog-sx.glade", "since_last_run_dialog");

    dialog->dialog = GTK_WIDGET(gtk_builder_get_object (builder, "since_last_run_dialog"));
    gtk_window_set_transient_for (GTK_WINDOW(dialog->dialog), parent);

    // Set the name of this dialog so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(dialog->dialog), "gnc-id-sx-since-last-run");
    gnc_widget_style_context_add_class (GTK_WIDGET(dialog->dialog), "gnc-class-sx");

    dialog->editing_model = gnc_sx_slr_tree_model_adapter_new (sx_instances);
    dialog->review_created_txns_toggle = GTK_TOGGLE_BUTTON(gtk_builder_get_object (builder, "review_txn_toggle"));

    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(dialog->review_created_txns_toggle),
                                  gnc_prefs_get_bool (GNC_PREFS_GROUP_STARTUP, GNC_PREF_SET_REVIEW));

    dialog->created_txns = auto_created_txn_guids;

    ok_button = GTK_WIDGET(gtk_builder_get_object (builder, "okbutton2"));

    g_signal_connect (G_OBJECT(ok_button), "button-press-event",
                      G_CALLBACK(finish_editing_before_ok_cb), dialog);

    {
        GtkCellRenderer *renderer;
        GtkTreeViewColumn *col;
        GtkTreeModel *sort_model = gtk_tree_model_sort_new_with_model (GTK_TREE_MODEL(dialog->editing_model));

        dialog->instance_view = GTK_TREE_VIEW(gtk_builder_get_object (builder, "instance_view"));
        gtk_tree_view_set_model (dialog->instance_view, GTK_TREE_MODEL(sort_model));
        g_object_unref (sort_model);

        /* default sort order */
        dialog->sort_selection_depth = gnc_prefs_get_int (GNC_PREFS_GROUP_STARTUP, GNC_PREF_SLR_SORT_DEPTH);
        gboolean sort_ascending = gnc_prefs_get_bool (GNC_PREFS_GROUP_STARTUP, GNC_PREF_SLR_SORT_ASC);
        gint sort_column = gnc_prefs_get_int (GNC_PREFS_GROUP_STARTUP, GNC_PREF_SLR_SORT_COL);
        GtkSortType sort_type = sort_ascending ? GTK_SORT_ASCENDING : GTK_SORT_DESCENDING;

        if (sort_column != 0)
            sort_column = 0;

        gtk_tree_sortable_set_sort_column_id (GTK_TREE_SORTABLE(sort_model),
                                              sort_column, sort_type);

        g_signal_connect (G_OBJECT(dialog->instance_view), "scroll-event",
                          G_CALLBACK(scroll_event), dialog);

        renderer = gtk_cell_renderer_text_new ();
        col = gtk_tree_view_column_new_with_attributes (_("Transaction"), renderer,
                "text", SLR_MODEL_COL_NAME,
                NULL);
        gtk_tree_view_append_column (dialog->instance_view, col);

        gtk_tree_view_column_set_sort_column_id (col, SLR_MODEL_COL_NAME);

        gtk_tree_sortable_set_sort_func (GTK_TREE_SORTABLE(sort_model), SLR_MODEL_COL_NAME,
                                         _transaction_sort_func, dialog, NULL);

        set_transaction_sort_column_tooltip (dialog);

        renderer = gtk_cell_renderer_combo_new ();
        g_object_set (G_OBJECT(renderer),
                      "model", gnc_sx_get_slr_state_model (),
                      "text-column", 0,
                      "has-entry", FALSE,
                      "editable", TRUE,
                      NULL);
        g_signal_connect (G_OBJECT(renderer),
                          "edited",
                          G_CALLBACK(instance_state_changed_cb),
                          dialog);
        col = gtk_tree_view_column_new_with_attributes (_("Status"), renderer,
                "text", SLR_MODEL_COL_INSTANCE_STATE,
                "visible", SLR_MODEL_COL_INSTANCE_VISIBILITY,
                // you might think only "sensitive" is required to
                // control the ability of the combo box to select
                // a new state, but you'd be wrong.
                "editable", SLR_MODEL_COL_INSTANCE_STATE_SENSITIVITY,
                "sensitive", SLR_MODEL_COL_INSTANCE_STATE_SENSITIVITY,
                NULL);

        g_signal_connect (G_OBJECT(sort_model), "sort-column-changed",
                          G_CALLBACK(sort_column_changed), dialog);

        renderer = gtk_cell_renderer_pixbuf_new ();
        g_object_set (G_OBJECT(renderer),
                      "icon-name", "pan-down-symbolic",
                      NULL);
        gtk_tree_view_column_pack_end (col, renderer, FALSE);
        gtk_tree_view_column_set_attributes (col, renderer,
                "visible", SLR_MODEL_COL_INSTANCE_VISIBILITY,
                "sensitive", SLR_MODEL_COL_INSTANCE_STATE_SENSITIVITY,
                NULL);
        gtk_tree_view_append_column (dialog->instance_view, col);
        gtk_tree_view_column_set_resizable (col, FALSE);

        renderer = gtk_cell_renderer_text_new ();
        g_object_set (G_OBJECT(renderer),
                      "editable", TRUE,
                      NULL);
        g_signal_connect (G_OBJECT(renderer),
                          "edited",
                          G_CALLBACK(variable_value_changed_cb),
                          dialog);

        g_signal_connect (G_OBJECT(renderer),
                          "editing-started",
                          G_CALLBACK(variable_value_start_changed_cb),
                          dialog);

        g_signal_connect (G_OBJECT(renderer),
                          "editing-canceled",
                          (GCallback)variable_value_cancel_changed_cb,
                          dialog);

        col = gtk_tree_view_column_new_with_attributes (_("Value"), renderer,
                "text", SLR_MODEL_COL_VARAIBLE_VALUE,
                "visible", SLR_MODEL_COL_VARIABLE_VISIBILITY,
                NULL);
        gtk_tree_view_append_column (dialog->instance_view, col);

        gtk_tree_view_expand_all (dialog->instance_view);
    }

    // Set grid lines option to preference
    gtk_tree_view_set_grid_lines (GTK_TREE_VIEW(dialog->instance_view), gnc_tree_view_get_grid_lines_pref ());

    g_signal_connect (G_OBJECT(dialog->dialog), "response", G_CALLBACK(dialog_response_cb), dialog);
    g_signal_connect (G_OBJECT(dialog->dialog), "destroy", G_CALLBACK(dialog_destroy_cb), dialog);

    gnc_restore_window_size (GNC_PREFS_GROUP_STARTUP, GTK_WINDOW(dialog->dialog), parent);

    dialog->component_id = gnc_register_gui_component (DIALOG_SX_SINCE_LAST_RUN_CM_CLASS,
                                                       NULL, close_handler, dialog);
    gnc_gui_component_set_session (dialog->component_id,
                                   gnc_get_current_session ());

    gtk_widget_show_all (dialog->dialog);

    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, dialog);

    g_object_unref (G_OBJECT(builder));

    return dialog;
}

static void
_show_created_transactions (GncSxSinceLastRunDialog *app_dialog, GList *created_txn_guids)
{
    GNCLedgerDisplay *ledger;
    GncPluginPage *page;
    Query *book_query, *guid_query, *query;
    GList *guid_iter;

    book_query = qof_query_create_for (GNC_ID_SPLIT);
    guid_query = qof_query_create_for (GNC_ID_SPLIT);
    qof_query_set_book (book_query, gnc_get_current_book ());
    for (guid_iter = created_txn_guids; guid_iter != NULL; guid_iter = guid_iter->next)
    {
        xaccQueryAddGUIDMatch (guid_query, (GncGUID*)guid_iter->data, GNC_ID_TRANS, QOF_QUERY_OR);
    }
    query = qof_query_merge (book_query, guid_query, QOF_QUERY_AND);
    // inspired by dialog-find-transactions:do_find_cb:
    ledger = gnc_ledger_display_query (query, SEARCH_LEDGER, REG_STYLE_JOURNAL);
    gnc_ledger_display_refresh (ledger);
    page = gnc_plugin_page_register_new_ledger (ledger);
    g_object_set (G_OBJECT(page), "page-name", _("Created Transactions"), NULL);
    gnc_main_window_open_page (NULL, page);

    qof_query_destroy (query);
    qof_query_destroy (book_query);
    qof_query_destroy (guid_query);
}

static void
close_handler (gpointer user_data)
{
    GncSxSinceLastRunDialog *app_dialog = user_data;
    GtkSortType order;
    gint column;

    if (gtk_tree_sortable_get_sort_column_id (GTK_TREE_SORTABLE(
                                              gtk_tree_view_get_model (app_dialog->instance_view)),
                                              &column, &order))
    {
        gboolean sort_ascending = TRUE;
        if (order == GTK_SORT_DESCENDING)
            sort_ascending = FALSE;

        gnc_prefs_set_bool (GNC_PREFS_GROUP_STARTUP, GNC_PREF_SLR_SORT_ASC, sort_ascending);
        gnc_prefs_set_int (GNC_PREFS_GROUP_STARTUP, GNC_PREF_SLR_SORT_COL, column);
        gnc_prefs_set_int (GNC_PREFS_GROUP_STARTUP, GNC_PREF_SLR_SORT_DEPTH,
                           app_dialog->sort_selection_depth);
    }

    gnc_save_window_size (GNC_PREFS_GROUP_STARTUP, GTK_WINDOW(app_dialog->dialog));
    gtk_widget_destroy (app_dialog->dialog);
    g_free (app_dialog);
}

static void
dialog_destroy_cb (GtkWidget *object, GncSxSinceLastRunDialog *app_dialog)
{
    gnc_unregister_gui_component (app_dialog->component_id);

    g_object_unref (G_OBJECT(app_dialog->editing_model));
    app_dialog->editing_model = NULL;
}

static void
dialog_response_cb (GtkDialog *dialog, gint response_id, GncSxSinceLastRunDialog *app_dialog)
{
    GList* creation_errors = NULL;
    switch (response_id)
    {
    case GTK_RESPONSE_HELP:
        gnc_gnome_help (GTK_WINDOW(dialog), DF_MANUAL, DL_SX_SLR);
        break;

    case GTK_RESPONSE_OK:
        // @@fixme validate current state(GError *errs);
        // - [ ] instance state constraints
        // - [x] required variable binding
        // - [?] ability to create transactions
        {
            GList *unbound_variables;
            gint unbound_len;
            unbound_variables = gnc_sx_instance_model_check_variables (app_dialog->editing_model->instances);
            unbound_len = g_list_length (unbound_variables);
            PINFO("%d variables unbound", unbound_len);
            if (unbound_len > 0)
            {
                // focus first variable
                GncSxVariableNeeded *first_unbound;
                GtkTreePath *variable_path;
                GtkTreeViewColumn *variable_col;
                gint variable_view_column = 2;
                gboolean start_editing = TRUE;

                first_unbound = (GncSxVariableNeeded*)unbound_variables->data;

                variable_path = _get_path_for_variable (app_dialog, first_unbound->instance, first_unbound->variable);
                variable_col = gtk_tree_view_get_column (app_dialog->instance_view, variable_view_column);

                gtk_tree_view_set_cursor (app_dialog->instance_view, variable_path, variable_col, start_editing);

                gtk_tree_view_scroll_to_cell (app_dialog->instance_view, variable_path, variable_col,
                                              TRUE, 0.5, 0.5);

                gtk_tree_path_free (variable_path);
                g_list_foreach (unbound_variables, (GFunc)g_free, NULL);
                g_list_free (unbound_variables);
                return;
            }
        }
        gnc_suspend_gui_refresh ();
        gnc_sx_slr_model_effect_change (app_dialog->editing_model, FALSE, &app_dialog->created_txns, &creation_errors);
        gnc_resume_gui_refresh ();
        gnc_gui_refresh_all (); // force a refresh of all registers
        if (creation_errors)
            gnc_ui_sx_creation_error_dialog (&creation_errors);

        if (gtk_toggle_button_get_active (app_dialog->review_created_txns_toggle)
                && g_list_length (app_dialog->created_txns) > 0)
        {
            _show_created_transactions (app_dialog, app_dialog->created_txns);
        }

    /* FALL THROUGH */
    case GTK_RESPONSE_CANCEL:
    case GTK_RESPONSE_DELETE_EVENT:
        g_list_free (app_dialog->created_txns);
        app_dialog->created_txns = NULL;
        gnc_close_gui_component (app_dialog->component_id);
        break;
    default:
        PWARN("unknown response id [%d]", response_id);
        break;
    }
}

/**
 * @param auto_create_only Will only affect auto-create transactions; the
 * rest of the state will be left alone.
 **/
void
gnc_sx_slr_model_effect_change (GncSxSlrTreeModelAdapter *model,
                                gboolean auto_create_only,
                                GList **created_transaction_guids,
                                GList **creation_errors)
{
    if (qof_book_is_readonly (gnc_get_current_book ()))
    {
        /* Is the book read-only? Then don't change anything here. */
        return;
    }

    g_signal_handler_block (model->instances, model->updated_cb_id);
    gnc_sx_instance_model_effect_change (model->instances, auto_create_only, created_transaction_guids, creation_errors);
    g_signal_handler_unblock (model->instances, model->updated_cb_id);
}
