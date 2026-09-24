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
/********************************************************************\
 * dialog-sx-since-last-run.c : GTK4 scheduled transaction runner   *
\********************************************************************/
#include <config.h>
#include <glib.h>
#include <glib/gi18n.h>
#include <gtk/gtk.h>

#include "dialog-utils.h"
#include "gnc-gtk-utils.h"
#include "dialog-sx-since-last-run.h"
#include "gnc-sx-instance-model.h"
#include "gnc-prefs.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "gnc-string-utils.h"
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

typedef enum
{
    SLR_ROW_SCHEDULE,
    SLR_ROW_INSTANCE,
    SLR_ROW_VARIABLE
} SlrRowKind;

typedef struct _GncSxSlrRow GncSxSlrRow;
struct _GncSxSlrRow
{
    GObject parent_instance;
    SlrRowKind kind;
    GncSxInstances *instances;
    GncSxInstance *instance;
    GncSxVariable *variable;
    GListStore *children;
    gchar *name;
};

typedef struct _GncSxSlrRowClass { GObjectClass parent_class; } GncSxSlrRowClass;
#define GNC_TYPE_SX_SLR_ROW (gnc_sx_slr_row_get_type ())
#define GNC_SX_SLR_ROW(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_SX_SLR_ROW, GncSxSlrRow))
#define GNC_IS_SX_SLR_ROW(obj) (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_SX_SLR_ROW))
GType gnc_sx_slr_row_get_type (void);

struct _GncSxSlrTreeModelAdapter
{
    GObject parent_instance;
    GncSxInstanceModel *instances;
    GListStore *roots;
    gboolean disposed;
    gboolean closing;
    gboolean sort_by_date;
    gboolean sort_ascending;
    guint refresh_source;
};
typedef struct _GncSxSlrTreeModelAdapterClass { GObjectClass parent_class; } GncSxSlrTreeModelAdapterClass;
#define GNC_TYPE_SX_SLR_TREE_MODEL_ADAPTER (gnc_sx_slr_tree_model_adapter_get_type ())
#define GNC_SX_SLR_TREE_MODEL_ADAPTER(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_SX_SLR_TREE_MODEL_ADAPTER, GncSxSlrTreeModelAdapter))
#define GNC_IS_SX_SLR_TREE_MODEL_ADAPTER(obj) (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_SX_SLR_TREE_MODEL_ADAPTER))
GType gnc_sx_slr_tree_model_adapter_get_type (void);

struct _GncSxSinceLastRunDialog
{
    GtkWidget *dialog;
    gint component_id;
    GncSxSlrTreeModelAdapter *editing_model;
    GtkTreeListModel *tree_model;
    GtkSingleSelection *selection;
    GtkColumnView *instance_view;
    GtkCheckButton *review_created_txns_toggle;
    GList *created_txns;
    gboolean shutting_down;
};

typedef struct
{
    grefcount ref_count;
    GWeakRef item;
    GWeakRef adapter;
} SlrListItemBinding;

G_GNUC_UNUSED static const gchar *instance_state_names[] =
{
    N_("Ignored"), N_("Postponed"), N_("To-Create"), N_("Reminder"), N_("Created"), NULL
};

static void slr_close_handler (gpointer user_data);
static void slr_destroy_cb (GtkWidget *object, gpointer user_data);
static void slr_refresh (GncSxSlrTreeModelAdapter *adapter);
static void show_created_transactions (GncSxSinceLastRunDialog *dialog, GList *guids);

G_DEFINE_TYPE (GncSxSlrRow, gnc_sx_slr_row, G_TYPE_OBJECT)
G_DEFINE_TYPE (GncSxSlrTreeModelAdapter, gnc_sx_slr_tree_model_adapter, G_TYPE_OBJECT)

static void
slr_row_finalize (GObject *object)
{
    GncSxSlrRow *row = GNC_SX_SLR_ROW (object);
    g_clear_object (&row->children);
    g_clear_pointer (&row->name, g_free);
    G_OBJECT_CLASS (gnc_sx_slr_row_parent_class)->finalize (object);
}

static void
gnc_sx_slr_row_class_init (GncSxSlrRowClass *klass)
{
    G_OBJECT_CLASS (klass)->finalize = slr_row_finalize;
}

static void
gnc_sx_slr_row_init (GncSxSlrRow *row)
{
}

static gchar*
slr_format_date (const GDate *date, const gchar *invalid)
{
    char buffer[MAX_DATE_LENGTH + 1];
    if (!date || !g_date_valid (date))
        return g_strdup (invalid);
    qof_print_gdate (buffer, MAX_DATE_LENGTH, date);
    return g_strdup (buffer);
}

static void
slr_numeric_to_string (const gnc_numeric *value, GString **string)
{
    *string = g_string_sized_new (5);
    g_string_printf (*string, "%0.2f", gnc_numeric_to_double (*value));
}

static GncSxSlrRow*
slr_row_new_schedule (GncSxInstances *instances)
{
    GncSxSlrRow *row = g_object_new (GNC_TYPE_SX_SLR_ROW, NULL);
    row->kind = SLR_ROW_SCHEDULE;
    row->instances = instances;
    row->name = g_strdup (xaccSchedXactionGetName (instances->sx));
    row->children = g_list_store_new (GNC_TYPE_SX_SLR_ROW);
    return row;
}

static GncSxSlrRow*
slr_row_new_instance (GncSxInstances *instances, GncSxInstance *instance)
{
    GncSxSlrRow *row = g_object_new (GNC_TYPE_SX_SLR_ROW, NULL);
    row->kind = SLR_ROW_INSTANCE;
    row->instances = instances;
    row->instance = instance;
    row->name = slr_format_date (&instance->date, _("Never"));
    row->children = g_list_store_new (GNC_TYPE_SX_SLR_ROW);
    return row;
}

static GncSxSlrRow*
slr_row_new_variable (GncSxInstances *instances, GncSxInstance *instance,
                      GncSxVariable *variable)
{
    GncSxSlrRow *row = g_object_new (GNC_TYPE_SX_SLR_ROW, NULL);
    row->kind = SLR_ROW_VARIABLE;
    row->instances = instances;
    row->instance = instance;
    row->variable = variable;
    row->name = g_strdup (variable->name);
    return row;
}

static gint
slr_instances_first_date_compare (GncSxInstances *left, GncSxInstances *right)
{
    GncSxInstance *left_instance = left->instance_list ? left->instance_list->data : NULL;
    GncSxInstance *right_instance = right->instance_list ? right->instance_list->data : NULL;
    if (!left_instance && !right_instance)
        return 0;
    if (!left_instance)
        return 1;
    if (!right_instance)
        return -1;
    return g_date_compare (&left_instance->date, &right_instance->date);
}

static gint
slr_instances_compare (gconstpointer left_data, gconstpointer right_data, gpointer user_data)
{
    GncSxSlrTreeModelAdapter *adapter = user_data;
    GncSxInstances *left = (GncSxInstances *)left_data;
    GncSxInstances *right = (GncSxInstances *)right_data;
    gint result;

    if (adapter->sort_by_date)
        result = slr_instances_first_date_compare (left, right);
    else
        result = g_utf8_collate (xaccSchedXactionGetName (left->sx),
                                 xaccSchedXactionGetName (right->sx));
    if (result == 0)
    {
        if (adapter->sort_by_date)
            result = g_utf8_collate (xaccSchedXactionGetName (left->sx),
                                     xaccSchedXactionGetName (right->sx));
        else
            result = slr_instances_first_date_compare (left, right);
    }
    return adapter->sort_ascending ? result : -result;
}

static gint
slr_instance_compare (gconstpointer left_data, gconstpointer right_data)
{
    const GncSxInstance *left = left_data;
    const GncSxInstance *right = right_data;
    return g_date_compare (&left->date, &right->date);
}

static void
slr_add_instance_children (GncSxSlrRow *instance_row)
{
    GList *variables = gnc_sx_instance_get_variables (instance_row->instance);
    for (GList *node = variables; node; node = node->next)
    {
        GncSxVariable *variable = node->data;
        if (!variable->editable)
            continue;
        GncSxSlrRow *row = slr_row_new_variable (instance_row->instances,
                                                  instance_row->instance, variable);
        g_list_store_append (instance_row->children, row);
        g_object_unref (row);
    }
    g_list_free (variables);
}

static void
slr_rebuild (GncSxSlrTreeModelAdapter *adapter)
{
    GList *schedules;

    if (adapter->disposed || adapter->closing)
        return;
    g_list_store_remove_all (adapter->roots);
    schedules = g_list_copy (gnc_sx_instance_model_get_sx_instances_list (adapter->instances));
    schedules = g_list_sort_with_data (schedules, slr_instances_compare, adapter);
    for (GList *schedule_node = schedules; schedule_node; schedule_node = schedule_node->next)
    {
        GncSxInstances *instances = schedule_node->data;
        GncSxSlrRow *schedule_row;
        GList *instance_list;
        if (!instances->instance_list)
            continue;
        schedule_row = slr_row_new_schedule (instances);
        instance_list = g_list_copy (instances->instance_list);
        instance_list = g_list_sort (instance_list, slr_instance_compare);
        for (GList *instance_node = instance_list; instance_node; instance_node = instance_node->next)
        {
            GncSxSlrRow *instance_row = slr_row_new_instance (instances, instance_node->data);
            slr_add_instance_children (instance_row);
            g_list_store_append (schedule_row->children, instance_row);
            g_object_unref (instance_row);
        }
        g_list_free (instance_list);
        g_list_store_append (adapter->roots, schedule_row);
        g_object_unref (schedule_row);
    }
    g_list_free (schedules);
}

static void
slr_adapter_added (GncSxInstanceModel *instances, SchedXaction *sx, gpointer user_data)
{
    GncSxSlrTreeModelAdapter *adapter = GNC_SX_SLR_TREE_MODEL_ADAPTER (user_data);

    if (!adapter->closing)
        slr_rebuild (adapter);
}

static void
slr_adapter_updated (GncSxInstanceModel *instances, SchedXaction *sx, gpointer user_data)
{
    GncSxSlrTreeModelAdapter *adapter = GNC_SX_SLR_TREE_MODEL_ADAPTER (user_data);

    if (adapter->closing)
        return;
    gnc_sx_instance_model_update_sx_instances (instances, sx);
    slr_rebuild (adapter);
}

static void
slr_adapter_removing (GncSxInstanceModel *instances, SchedXaction *sx, gpointer user_data)
{
    GncSxSlrTreeModelAdapter *adapter = GNC_SX_SLR_TREE_MODEL_ADAPTER (user_data);

    if (adapter->closing)
        return;
    gnc_sx_instance_model_remove_sx_instances (instances, sx);
    slr_rebuild (adapter);
}

static void
slr_adapter_cancel_refresh (GncSxSlrTreeModelAdapter *adapter)
{
    guint refresh_source = adapter->refresh_source;

    adapter->refresh_source = 0;
    if (refresh_source)
        g_source_remove (refresh_source);
}

static void
gnc_sx_slr_tree_model_adapter_dispose (GObject *object)
{
    GncSxSlrTreeModelAdapter *adapter = GNC_SX_SLR_TREE_MODEL_ADAPTER (object);
    if (adapter->disposed)
        return;
    adapter->disposed = TRUE;
    slr_adapter_cancel_refresh (adapter);
    if (adapter->instances)
    {
        g_signal_handlers_disconnect_by_data (adapter->instances, adapter);
        g_clear_object (&adapter->instances);
    }
    g_clear_object (&adapter->roots);
    G_OBJECT_CLASS (gnc_sx_slr_tree_model_adapter_parent_class)->dispose (object);
}

static void
gnc_sx_slr_tree_model_adapter_class_init (GncSxSlrTreeModelAdapterClass *klass)
{
    G_OBJECT_CLASS (klass)->dispose = gnc_sx_slr_tree_model_adapter_dispose;
}

static void
gnc_sx_slr_tree_model_adapter_init (GncSxSlrTreeModelAdapter *adapter)
{
    adapter->sort_ascending = TRUE;
    adapter->roots = g_list_store_new (GNC_TYPE_SX_SLR_ROW);
}

static GncSxSlrTreeModelAdapter*
slr_adapter_new (GncSxInstanceModel *instances)
{
    GncSxSlrTreeModelAdapter *adapter = g_object_new (GNC_TYPE_SX_SLR_TREE_MODEL_ADAPTER, NULL);
    adapter->instances = g_object_ref (instances);
    adapter->sort_by_date = gnc_prefs_get_int (GNC_PREFS_GROUP_STARTUP, GNC_PREF_SLR_SORT_DEPTH) != 1;
    adapter->sort_ascending = gnc_prefs_get_bool (GNC_PREFS_GROUP_STARTUP, GNC_PREF_SLR_SORT_ASC);
    slr_rebuild (adapter);
    g_signal_connect_object (instances, "added", G_CALLBACK (slr_adapter_added), adapter, 0);
    g_signal_connect_object (instances, "updated", G_CALLBACK (slr_adapter_updated), adapter, 0);
    g_signal_connect_object (instances, "removing", G_CALLBACK (slr_adapter_removing), adapter, 0);
    return adapter;
}

static GListModel*
slr_children_for_row (gpointer item, gpointer user_data)
{
    GncSxSlrRow *row = GNC_SX_SLR_ROW (item);
    return row->children ? G_LIST_MODEL (g_object_ref (row->children)) : NULL;
}

static void
slr_refresh (GncSxSlrTreeModelAdapter *adapter)
{
    slr_rebuild (adapter);
}

static void
slr_expand_all (GtkTreeListModel *model)
{
    for (guint position = 0; position < g_list_model_get_n_items (G_LIST_MODEL (model)); position++)
    {
        GtkTreeListRow *row = GTK_TREE_LIST_ROW (g_list_model_get_item (G_LIST_MODEL (model), position));
        if (gtk_tree_list_row_is_expandable (row))
            gtk_tree_list_row_set_expanded (row, TRUE);
        g_object_unref (row);
    }
}

static GncSxSlrRow*
slr_get_list_item_row (GtkListItem *item)
{
    GtkTreeListRow *tree_row = GTK_TREE_LIST_ROW (gtk_list_item_get_item (item));
    if (!tree_row)
        return NULL;
    GObject *row_item = gtk_tree_list_row_get_item (tree_row);
    GncSxSlrRow *row = GNC_SX_SLR_ROW (row_item);
    g_object_unref (row_item);
    return row;
}

static GncSxInstance*
slr_find_instance (GncSxInstanceModel *model, SchedXaction *sx, const GDate *date)
{
    for (GList *schedule_node = gnc_sx_instance_model_get_sx_instances_list (model);
         schedule_node; schedule_node = schedule_node->next)
    {
        GncSxInstances *instances = schedule_node->data;

        if (instances->sx != sx)
            continue;
        for (GList *instance_node = instances->instance_list;
             instance_node; instance_node = instance_node->next)
        {
            GncSxInstance *instance = instance_node->data;

            if (g_date_compare (&instance->date, date) == 0)
                return instance;
        }
    }
    return NULL;
}

static GncSxVariable*
slr_find_variable (GncSxInstance *instance, const gchar *name)
{
    GList *variables = gnc_sx_instance_get_variables (instance);
    GncSxVariable *match = NULL;

    for (GList *node = variables; node; node = node->next)
    {
        GncSxVariable *variable = node->data;

        if (g_strcmp0 (variable->name, name) == 0)
        {
            match = variable;
            break;
        }
    }
    g_list_free (variables);
    return match;
}

static void
slr_name_setup (GtkSignalListItemFactory *factory, GtkListItem *item, gpointer user_data)
{
    GtkWidget *label = gtk_label_new (NULL);
    GtkWidget *expander = gtk_tree_expander_new ();
    gtk_label_set_xalign (GTK_LABEL (label), 0.0f);
    gtk_label_set_ellipsize (GTK_LABEL (label), PANGO_ELLIPSIZE_END);
    gtk_tree_expander_set_child (GTK_TREE_EXPANDER (expander), label);
    gtk_list_item_set_child (item, expander);
}

static void
slr_name_bind (GtkSignalListItemFactory *factory, GtkListItem *item, gpointer user_data)
{
    GtkTreeListRow *tree_row = GTK_TREE_LIST_ROW (gtk_list_item_get_item (item));
    GObject *row_item = gtk_tree_list_row_get_item (tree_row);
    GncSxSlrRow *row = GNC_SX_SLR_ROW (row_item);
    GtkTreeExpander *expander = GTK_TREE_EXPANDER (gtk_list_item_get_child (item));
    GtkWidget *label = gtk_tree_expander_get_child (expander);
    gtk_tree_expander_set_list_row (expander, tree_row);
    gtk_label_set_text (GTK_LABEL (label), row->name);
    g_object_unref (row_item);
}

static gboolean
slr_refresh_idle (gpointer user_data)
{
    GncSxSlrTreeModelAdapter *adapter = GNC_SX_SLR_TREE_MODEL_ADAPTER (user_data);

    adapter->refresh_source = 0;
    if (!adapter->disposed && !adapter->closing)
        slr_refresh (adapter);
    return G_SOURCE_REMOVE;
}

static void
slr_schedule_refresh (GncSxSlrTreeModelAdapter *adapter)
{
    if (adapter->disposed || adapter->closing || adapter->refresh_source)
        return;
    adapter->refresh_source = g_idle_add_full (G_PRIORITY_DEFAULT_IDLE,
                                                slr_refresh_idle,
                                                g_object_ref (adapter),
                                                g_object_unref);
}

static SlrListItemBinding*
slr_list_item_binding_new (GtkListItem *item)
{
    SlrListItemBinding *binding = g_new0 (SlrListItemBinding, 1);

    g_ref_count_init (&binding->ref_count);
    g_weak_ref_init (&binding->item, item);
    g_weak_ref_init (&binding->adapter, NULL);
    return binding;
}

static SlrListItemBinding*
slr_list_item_binding_ref (SlrListItemBinding *binding)
{
    g_ref_count_inc (&binding->ref_count);
    return binding;
}

static void
slr_list_item_binding_unref (SlrListItemBinding *binding)
{
    if (!g_ref_count_dec (&binding->ref_count))
        return;
    g_weak_ref_clear (&binding->item);
    g_weak_ref_clear (&binding->adapter);
    g_free (binding);
}

static void
slr_list_item_binding_closure_notify (gpointer data, GClosure *closure)
{
    (void)closure;
    slr_list_item_binding_unref (data);
}

static gboolean
slr_list_item_binding_get (SlrListItemBinding *binding, GtkListItem **item,
                           GncSxSlrTreeModelAdapter **adapter)
{
    *item = NULL;
    *adapter = NULL;
    if (!binding)
        return FALSE;
    *item = g_weak_ref_get (&binding->item);
    *adapter = g_weak_ref_get (&binding->adapter);
    if (!*item || !*adapter || (*adapter)->disposed || (*adapter)->closing)
    {
        g_clear_object (item);
        g_clear_object (adapter);
        return FALSE;
    }
    return TRUE;
}

static void
slr_state_changed (GObject *dropdown, GParamSpec *pspec, gpointer user_data)
{
    SlrListItemBinding *binding = user_data;
    GtkListItem *item;
    GncSxSlrTreeModelAdapter *adapter;
    GncSxSlrRow *row;
    guint selected = gtk_drop_down_get_selected (GTK_DROP_DOWN (dropdown));

    if (!slr_list_item_binding_get (binding, &item, &adapter))
        return;
    row = slr_get_list_item_row (item);
    if (!row || !adapter || row->kind != SLR_ROW_INSTANCE ||
        row->instance->state == SX_INSTANCE_STATE_CREATED || selected >= SX_INSTANCE_STATE_CREATED)
        goto out;
    if (selected == row->instance->state)
        goto out;
    {
        GncSxInstanceModel *instances = g_object_ref (adapter->instances);

        gnc_sx_instance_model_change_instance_state (instances, row->instance, selected);
        g_object_unref (instances);
    }
    if (!adapter->closing)
        slr_schedule_refresh (adapter);

out:
    g_object_unref (item);
    g_object_unref (adapter);
}

static void
slr_state_setup (GtkSignalListItemFactory *factory, GtkListItem *item, gpointer user_data)
{
    const char *states[] = { _("Ignored"), _("Postponed"), _("To-Create"), _("Reminder"), NULL };
    GtkStringList *model = gtk_string_list_new (states);
    GtkWidget *dropdown = GTK_WIDGET (gnc_gtk_drop_down_new (G_LIST_MODEL (model), NULL));
    SlrListItemBinding *binding = slr_list_item_binding_new (item);

    g_object_set_data_full (G_OBJECT (item), "slr-list-item-binding", binding,
                            (GDestroyNotify)slr_list_item_binding_unref);
    g_signal_connect_data (dropdown, "notify::selected", G_CALLBACK (slr_state_changed),
                           slr_list_item_binding_ref (binding),
                           slr_list_item_binding_closure_notify, 0);
    gtk_list_item_set_child (item, dropdown);
}

static void
slr_state_bind (GtkSignalListItemFactory *factory, GtkListItem *item, gpointer user_data)
{
    GncSxSlrRow *row = slr_get_list_item_row (item);
    GtkWidget *dropdown = gtk_list_item_get_child (item);
    SlrListItemBinding *binding = g_object_get_data (G_OBJECT (item), "slr-list-item-binding");

    g_return_if_fail (binding);
    g_weak_ref_set (&binding->adapter, user_data);
    g_signal_handlers_block_by_func (dropdown, slr_state_changed, binding);
    gtk_widget_set_visible (dropdown, row->kind == SLR_ROW_INSTANCE);
    if (row->kind == SLR_ROW_INSTANCE)
    {
        gtk_widget_set_sensitive (dropdown, row->instance->state != SX_INSTANCE_STATE_CREATED);
        gtk_drop_down_set_selected (GTK_DROP_DOWN (dropdown),
                                     MIN ((guint)row->instance->state, SX_INSTANCE_STATE_CREATED - 1));
    }
    g_signal_handlers_unblock_by_func (dropdown, slr_state_changed, binding);
}

static void
slr_state_unbind (GtkSignalListItemFactory *factory, GtkListItem *item, gpointer user_data)
{
    SlrListItemBinding *binding = g_object_get_data (G_OBJECT (item), "slr-list-item-binding");

    if (binding)
        g_weak_ref_set (&binding->adapter, NULL);
}

static void
slr_variable_commit (GtkEntry *entry, SlrListItemBinding *binding)
{
    GtkListItem *item;
    GncSxSlrTreeModelAdapter *adapter;
    GncSxSlrRow *row;
    gnc_numeric value;
    char *end = NULL;
    const gchar *text;

    if (!slr_list_item_binding_get (binding, &item, &adapter))
        return;
    row = slr_get_list_item_row (item);
    if (!row || row->kind != SLR_ROW_VARIABLE)
        goto out;
    text = gtk_editable_get_text (GTK_EDITABLE (entry));
    if (!xaccParseAmount (text, TRUE, &value, &end) || gnc_numeric_check (value) != GNC_ERROR_OK)
    {
        gchar *copy = g_strdup (text);
        if (*g_strstrip (copy) == '\0')
        {
            gnc_numeric invalid = gnc_numeric_error (GNC_ERROR_ARG);
            GncSxInstanceModel *instances = g_object_ref (adapter->instances);

            gnc_sx_instance_model_set_variable (instances, row->instance, row->variable, &invalid);
            g_object_unref (instances);
        }
        else
        {
            gnc_warning_dialog_async (NULL, NULL, _("Invalid Value"),
                                      _("The value is not a valid amount."), _("_Close"),
                                      GTK_RESPONSE_CLOSE, TRUE, NULL, NULL);
        }
        g_free (copy);
        goto out;
    }
    {
        GncSxInstanceModel *instances = g_object_ref (adapter->instances);

        if (row->instance->state == SX_INSTANCE_STATE_REMINDER)
        {
            SchedXaction *sx = row->instance->parent->sx;
            GDate date = row->instance->date;
            gchar *name = g_strdup (row->variable->name);
            GncSxInstance *instance;
            GncSxVariable *variable;

            gnc_sx_instance_model_change_instance_state (instances, row->instance,
                                                          SX_INSTANCE_STATE_TO_CREATE);
            instance = adapter->closing ? NULL : slr_find_instance (instances, sx, &date);
            variable = instance ? slr_find_variable (instance, name) : NULL;
            if (variable)
                gnc_sx_instance_model_set_variable (instances, instance, variable, &value);
            g_free (name);
        }
        else
            gnc_sx_instance_model_set_variable (instances, row->instance, row->variable, &value);
        g_object_unref (instances);
    }
    if (!adapter->closing)
        slr_schedule_refresh (adapter);

out:
    g_object_unref (item);
    g_object_unref (adapter);
}

static void
slr_value_activate (GtkEntry *entry, gpointer user_data)
{
    slr_variable_commit (entry, user_data);
}

static void
slr_value_focus_changed (GObject *entry, GParamSpec *pspec, gpointer user_data)
{
    if (!gtk_widget_has_focus (GTK_WIDGET (entry)))
        slr_variable_commit (GTK_ENTRY (entry), user_data);
}

static void
slr_value_setup (GtkSignalListItemFactory *factory, GtkListItem *item, gpointer user_data)
{
    GtkWidget *entry = gtk_entry_new ();
    SlrListItemBinding *binding = slr_list_item_binding_new (item);

    g_object_set_data_full (G_OBJECT (item), "slr-list-item-binding", binding,
                            (GDestroyNotify)slr_list_item_binding_unref);
    g_signal_connect_data (entry, "activate", G_CALLBACK (slr_value_activate),
                           slr_list_item_binding_ref (binding),
                           slr_list_item_binding_closure_notify, 0);
    g_signal_connect_data (entry, "notify::has-focus", G_CALLBACK (slr_value_focus_changed),
                           slr_list_item_binding_ref (binding),
                           slr_list_item_binding_closure_notify, 0);
    gtk_list_item_set_child (item, entry);
}

static void
slr_value_bind (GtkSignalListItemFactory *factory, GtkListItem *item, gpointer user_data)
{
    GncSxSlrRow *row = slr_get_list_item_row (item);
    GtkWidget *entry = gtk_list_item_get_child (item);
    GString *value;
    SlrListItemBinding *binding = g_object_get_data (G_OBJECT (item), "slr-list-item-binding");

    g_return_if_fail (binding);
    g_weak_ref_set (&binding->adapter, user_data);
    g_signal_handlers_block_by_func (entry, slr_value_focus_changed, binding);
    gtk_widget_set_visible (entry, row->kind == SLR_ROW_VARIABLE);
    if (row->kind == SLR_ROW_VARIABLE)
    {
        if (gnc_numeric_check (row->variable->value) == GNC_ERROR_OK)
        {
            slr_numeric_to_string (&row->variable->value, &value);
            gtk_editable_set_text (GTK_EDITABLE (entry), value->str);
            g_string_free (value, TRUE);
        }
        else
            gtk_editable_set_text (GTK_EDITABLE (entry), _("(Need Value)"));
    }
    g_signal_handlers_unblock_by_func (entry, slr_value_focus_changed, binding);
}

static void
slr_value_unbind (GtkSignalListItemFactory *factory, GtkListItem *item, gpointer user_data)
{
    SlrListItemBinding *binding = g_object_get_data (G_OBJECT (item), "slr-list-item-binding");

    if (binding)
        g_weak_ref_set (&binding->adapter, NULL);
}

static void
slr_append_column (GtkColumnView *view, const gchar *title,
                   GCallback setup, GCallback bind, GCallback unbind,
                   GObject *bind_data, gboolean expand)
{
    GtkListItemFactory *factory = gtk_signal_list_item_factory_new ();
    GtkColumnViewColumn *column = gtk_column_view_column_new (title, factory);
    g_signal_connect (factory, "setup", setup, NULL);
    if (bind_data)
        g_signal_connect_object (factory, "bind", bind, bind_data, 0);
    else
        g_signal_connect (factory, "bind", bind, NULL);
    if (unbind)
        g_signal_connect (factory, "unbind", unbind, NULL);
    gtk_column_view_column_set_resizable (column, TRUE);
    gtk_column_view_column_set_expand (column, expand);
    gtk_column_view_append_column (view, column);
    g_object_unref (column);
}

static void
slr_select_first_unbound (GncSxSinceLastRunDialog *dialog, GncSxVariableNeeded *needed)
{
    guint n = g_list_model_get_n_items (G_LIST_MODEL (dialog->tree_model));
    for (guint position = 0; position < n; position++)
    {
        GtkTreeListRow *tree_row = GTK_TREE_LIST_ROW (g_list_model_get_item
                                                       (G_LIST_MODEL (dialog->tree_model), position));
        GObject *row_item = gtk_tree_list_row_get_item (tree_row);
        GncSxSlrRow *row = GNC_SX_SLR_ROW (row_item);
        if (row->kind == SLR_ROW_VARIABLE && row->instance == needed->instance &&
            row->variable == needed->variable)
        {
            gtk_single_selection_set_selected (dialog->selection, position);
            gtk_column_view_scroll_to (dialog->instance_view, position, NULL,
                                       GTK_LIST_SCROLL_FOCUS, NULL);
            g_object_unref (row_item);
            g_object_unref (tree_row);
            return;
        }
        g_object_unref (row_item);
        g_object_unref (tree_row);
    }
}

static void
slr_finish (GncSxSinceLastRunDialog *dialog)
{
    gnc_close_gui_component (dialog->component_id);
}

static void
slr_help_clicked (GtkButton *button, gpointer user_data)
{
    GncSxSinceLastRunDialog *dialog = user_data;
    gnc_gnome_help (GTK_WINDOW (dialog->dialog), DF_MANUAL, DL_SX_SLR);
}

static void
slr_cancel_clicked (GtkButton *button, gpointer user_data)
{
    slr_finish (user_data);
}

static void
slr_ok_clicked (GtkButton *button, gpointer user_data)
{
    GncSxSinceLastRunDialog *dialog = user_data;
    GList *errors = NULL;
    GList *unbound = gnc_sx_instance_model_check_variables (dialog->editing_model->instances);

    if (unbound)
    {
        slr_select_first_unbound (dialog, unbound->data);
        g_list_free_full (unbound, g_free);
        return;
    }
    gnc_suspend_gui_refresh ();
    gnc_sx_instance_model_effect_change (dialog->editing_model->instances, FALSE,
                                         &dialog->created_txns, &errors);
    gnc_resume_gui_refresh ();
    gnc_gui_refresh_all ();
    if (errors)
        gnc_ui_sx_creation_error_dialog (&errors);
    if (gtk_check_button_get_active (dialog->review_created_txns_toggle) && dialog->created_txns)
        show_created_transactions (dialog, dialog->created_txns);
    slr_finish (dialog);
}

static gboolean
slr_close_request (GtkWindow *window, gpointer user_data)
{
    slr_finish (user_data);
    return TRUE;
}

static void
since_last_run_dialog (GtkWindow *parent, GncSxInstanceModel *instances, GList *auto_created_txns)
{
    GncSxSinceLastRunDialog *dialog = g_new0 (GncSxSinceLastRunDialog, 1);
    GtkBuilder *builder = gtk_builder_new ();
    GtkWidget *button;

    gnc_builder_add_from_file (builder, "dialog-sx.ui", "since_last_run_dialog");
    dialog->dialog = GTK_WIDGET (gtk_builder_get_object (builder, "since_last_run_dialog"));
    dialog->editing_model = slr_adapter_new (instances);
    dialog->tree_model = gtk_tree_list_model_new
        (G_LIST_MODEL (g_object_ref (dialog->editing_model->roots)), FALSE, TRUE,
         slr_children_for_row, NULL, NULL);
    dialog->selection = gtk_single_selection_new (G_LIST_MODEL (g_object_ref (dialog->tree_model)));
    gtk_single_selection_set_autoselect (dialog->selection, FALSE);
    dialog->instance_view = GTK_COLUMN_VIEW (gtk_builder_get_object (builder, "instance_view"));
    gtk_column_view_set_model (dialog->instance_view, GTK_SELECTION_MODEL (dialog->selection));
    gtk_column_view_set_show_row_separators
        (dialog->instance_view, gnc_prefs_get_bool ("general", "grid-lines-horizontal"));
    gtk_column_view_set_show_column_separators
        (dialog->instance_view, gnc_prefs_get_bool ("general", "grid-lines-vertical"));
    slr_append_column (dialog->instance_view, _("Transaction"),
                       G_CALLBACK (slr_name_setup), G_CALLBACK (slr_name_bind), NULL, NULL, TRUE);
    slr_append_column (dialog->instance_view, _("Status"), G_CALLBACK (slr_state_setup),
                       G_CALLBACK (slr_state_bind), G_CALLBACK (slr_state_unbind),
                       G_OBJECT (dialog->editing_model), FALSE);
    slr_append_column (dialog->instance_view, _("Value"), G_CALLBACK (slr_value_setup),
                       G_CALLBACK (slr_value_bind), G_CALLBACK (slr_value_unbind),
                       G_OBJECT (dialog->editing_model), FALSE);
    slr_expand_all (dialog->tree_model);

    gtk_window_set_transient_for (GTK_WINDOW (dialog->dialog), parent);
    gtk_widget_set_name (dialog->dialog, "gnc-id-sx-since-last-run");
    gtk_widget_add_css_class (dialog->dialog, "gnc-class-sx");
    dialog->review_created_txns_toggle = GTK_CHECK_BUTTON
        (gtk_builder_get_object (builder, "review_txn_toggle"));
    gtk_check_button_set_active (dialog->review_created_txns_toggle,
        gnc_prefs_get_bool (GNC_PREFS_GROUP_STARTUP, GNC_PREF_SET_REVIEW));
    dialog->created_txns = auto_created_txns;
    button = GTK_WIDGET (gtk_builder_get_object (builder, "helpbutton2"));
    g_signal_connect (button, "clicked", G_CALLBACK (slr_help_clicked), dialog);
    button = GTK_WIDGET (gtk_builder_get_object (builder, "cancelbutton2"));
    g_signal_connect (button, "clicked", G_CALLBACK (slr_cancel_clicked), dialog);
    button = GTK_WIDGET (gtk_builder_get_object (builder, "okbutton2"));
    g_signal_connect (button, "clicked", G_CALLBACK (slr_ok_clicked), dialog);
    g_signal_connect (dialog->dialog, "close-request", G_CALLBACK (slr_close_request), dialog);
    g_signal_connect (dialog->dialog, "destroy", G_CALLBACK (slr_destroy_cb), dialog);
    gnc_restore_window_size (GNC_PREFS_GROUP_STARTUP, GTK_WINDOW (dialog->dialog), parent);
    dialog->component_id = gnc_register_gui_component (DIALOG_SX_SINCE_LAST_RUN_CM_CLASS,
        NULL, slr_close_handler, dialog);
    gnc_gui_component_set_session (dialog->component_id, gnc_get_current_session ());
    gtk_window_present (GTK_WINDOW (dialog->dialog));
    g_object_unref (builder);
}

void
gnc_ui_sx_creation_error_dialog (GList **creation_errors)
{
    GtkAlertDialog *alert;
    gchar *message;
    if (!creation_errors || !*creation_errors)
        return;
    message = gnc_g_list_stringjoin (*creation_errors, "\n");
    g_list_free_full (*creation_errors, g_free);
    *creation_errors = NULL;
    alert = gtk_alert_dialog_new ("%s", _("Invalid Transactions"));
    gtk_alert_dialog_set_detail (alert, message);
    gtk_alert_dialog_show (alert, NULL);
    g_object_unref (alert);
    g_free (message);
}

static void
sx_since_last_run_dialog (GncSxInstanceModel *instances, int book_opened)
{
    GncSxSummary summary;
    GList *auto_created = NULL;
    GList *errors = NULL;

    if (qof_book_is_readonly (gnc_get_current_book ()))
    {
        g_object_unref (instances);
        return;
    }
    gnc_sx_instance_model_summarize (instances, &summary);
    if (book_opened)
        gnc_sx_summary_print (&summary);
    gnc_sx_instance_model_effect_change (instances, TRUE, &auto_created, &errors);
    if (auto_created)
        gnc_gui_refresh_all ();
    if (summary.need_dialog)
        since_last_run_dialog (gnc_ui_get_main_window (NULL), instances, auto_created);
    else
    {
        if (summary.num_auto_create_no_notify_instances == 0)
        {
            if (!book_opened)
                gnc_info_dialog (gnc_ui_get_main_window (NULL), "%s",
                                 _("There are no Scheduled Transactions to be entered at this time."));
        }
        else if (!book_opened || gnc_prefs_get_bool (GNC_PREFS_GROUP_STARTUP, GNC_PREF_SHOW_AT_FOPEN))
        {
            gnc_info_dialog (gnc_ui_get_main_window (NULL), ngettext
                ("There are no Scheduled Transactions to be entered at this time. "
                 "(%d transaction automatically created)",
                 "There are no Scheduled Transactions to be entered at this time. "
                 "(%d transactions automatically created)", summary.num_auto_create_no_notify_instances),
                 summary.num_auto_create_no_notify_instances);
        }
        g_list_free (auto_created);
    }
    g_object_unref (instances);
    if (errors)
        gnc_ui_sx_creation_error_dialog (&errors);
}

void
gnc_ui_sx_since_last_run_dialog (GncSxInstanceModel *instances)
{
    sx_since_last_run_dialog (instances, FALSE);
}

void
gnc_sx_sxsincelast_book_opened (void)
{
    if (gnc_prefs_get_bool (GNC_PREFS_GROUP_STARTUP, GNC_PREF_RUN_AT_FOPEN))
        sx_since_last_run_dialog (gnc_sx_get_current_instances (), TRUE);
}

static void
show_created_transactions (GncSxSinceLastRunDialog *dialog, GList *guids)
{
    Query *book_query = qof_query_create_for (GNC_ID_SPLIT);
    Query *guid_query = qof_query_create_for (GNC_ID_SPLIT);
    Query *query;
    GNCLedgerDisplay *ledger;
    GncPluginPage *page;

    qof_query_set_book (book_query, gnc_get_current_book ());
    for (GList *node = guids; node; node = node->next)
        xaccQueryAddGUIDMatch (guid_query, node->data, GNC_ID_TRANS, QOF_QUERY_OR);
    query = qof_query_merge (book_query, guid_query, QOF_QUERY_AND);
    ledger = gnc_ledger_display_query (query, SEARCH_LEDGER, REG_STYLE_JOURNAL);
    gnc_ledger_display_refresh (ledger);
    page = gnc_plugin_page_register_new_ledger (ledger);
    g_object_set (page, "page-name", _("Created Transactions"), NULL);
    gnc_main_window_open_page (NULL, page);
    qof_query_destroy (query);
    qof_query_destroy (book_query);
    qof_query_destroy (guid_query);
}

static void
slr_quiesce_view (GncSxSinceLastRunDialog *dialog)
{
    /* The bound factories carry the adapter as callback data. Detach the
     * model while it is still alive so GTK unbinds the real list items first. */
    if (dialog->instance_view)
        gtk_column_view_set_model (dialog->instance_view, NULL);
}

static void
slr_disconnect_widget_callbacks (GtkWidget *widget, gpointer data)
{
    g_signal_handlers_disconnect_by_data (widget, data);
    for (GtkWidget *child = gtk_widget_get_first_child (widget); child;
         child = gtk_widget_get_next_sibling (child))
        slr_disconnect_widget_callbacks (child, data);
}

static void
slr_shutdown (GncSxSinceLastRunDialog *dialog)
{
    if (dialog->shutting_down)
        return;
    dialog->shutting_down = TRUE;
    dialog->editing_model->closing = TRUE;
    slr_disconnect_widget_callbacks (dialog->dialog, dialog);
    slr_quiesce_view (dialog);
    dialog->instance_view = NULL;
    if (dialog->component_id)
    {
        gnc_unregister_gui_component (dialog->component_id);
        dialog->component_id = 0;
    }
    slr_adapter_cancel_refresh (dialog->editing_model);
    g_clear_object (&dialog->selection);
    g_clear_object (&dialog->tree_model);
    g_clear_object (&dialog->editing_model);
    g_list_free (dialog->created_txns);
    dialog->created_txns = NULL;
}

static void
slr_destroy_cb (GtkWidget *object, gpointer user_data)
{
    GncSxSinceLastRunDialog *dialog = user_data;

    slr_shutdown (dialog);
    g_free (dialog);
}

static void
slr_close_handler (gpointer user_data)
{
    GncSxSinceLastRunDialog *dialog = user_data;

    if (dialog->shutting_down)
        return;
    gnc_prefs_set_bool (GNC_PREFS_GROUP_STARTUP, GNC_PREF_SLR_SORT_ASC,
                        dialog->editing_model->sort_ascending);
    gnc_prefs_set_int (GNC_PREFS_GROUP_STARTUP, GNC_PREF_SLR_SORT_COL, 0);
    gnc_prefs_set_int (GNC_PREFS_GROUP_STARTUP, GNC_PREF_SLR_SORT_DEPTH,
                       dialog->editing_model->sort_by_date ? 2 : 1);
    gnc_save_window_size (GNC_PREFS_GROUP_STARTUP, GTK_WINDOW (dialog->dialog));
    slr_shutdown (dialog);
    gtk_window_destroy (GTK_WINDOW (dialog->dialog));
    g_free (dialog);
}
