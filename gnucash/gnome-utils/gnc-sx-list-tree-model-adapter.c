/********************************************************************\
 * gnc-sx-list-tree-model-adapter.c                                 *
 *                                                                  *
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
 * gnc-sx-list-tree-model-adapter.c                                 *
 * GTK4 list-model adapter for scheduled transactions.              *
\********************************************************************/
#include <config.h>

#include <glib/gi18n.h>
#include <gtk/gtk.h>

#include "gnc-sx-list-tree-model-adapter.h"

struct _GncSxListRow
{
    GObject parent_instance;
    SchedXaction *sx;
    gchar *name;
    gboolean enabled;
    gchar *frequency;
    guint num_postponed;
    gchar *last_occur;
    gchar *next_occur;
};

struct _GncSxListTreeModelAdapter
{
    GObject parent_instance;
    GncSxInstanceModel *instances;
    GListStore *rows;
    gboolean disposed;
};

G_DEFINE_TYPE (GncSxListRow, gnc_sx_list_row, G_TYPE_OBJECT)
G_DEFINE_TYPE (GncSxListTreeModelAdapter, gnc_sx_list_tree_model_adapter, G_TYPE_OBJECT)

static void
gnc_sx_list_row_finalize (GObject *object)
{
    GncSxListRow *row = GNC_SX_LIST_ROW (object);

    g_clear_pointer (&row->name, g_free);
    g_clear_pointer (&row->frequency, g_free);
    g_clear_pointer (&row->last_occur, g_free);
    g_clear_pointer (&row->next_occur, g_free);
    G_OBJECT_CLASS (gnc_sx_list_row_parent_class)->finalize (object);
}

static void
gnc_sx_list_row_class_init (GncSxListRowClass *klass)
{
    G_OBJECT_CLASS (klass)->finalize = gnc_sx_list_row_finalize;
}

static void
gnc_sx_list_row_init (GncSxListRow *row)
{
}

static void
format_conditional_date (const GDate *date, char *buffer, gsize buffer_size)
{
    if (date == NULL || !g_date_valid (date))
        g_strlcpy (buffer, _("never"), buffer_size);
    else
        qof_print_gdate (buffer, buffer_size, date);
}

static GncSxListRow*
sx_list_row_new (GncSxInstances *instances)
{
    GncSxListRow *row = g_object_new (GNC_TYPE_SX_LIST_ROW, NULL);
    char last_occur[MAX_DATE_LENGTH + 1];
    char next_occur[MAX_DATE_LENGTH + 1];

    row->sx = instances->sx;
    row->name = g_strdup (xaccSchedXactionGetName (row->sx));
    row->enabled = xaccSchedXactionGetEnabled (row->sx);
    row->frequency = recurrenceListToCompactString (gnc_sx_get_schedule (row->sx));
    row->num_postponed = g_list_length (gnc_sx_get_defer_instances (row->sx));
    format_conditional_date (xaccSchedXactionGetLastOccurDate (row->sx),
                             last_occur, sizeof last_occur);
    format_conditional_date (&instances->next_instance_date,
                             next_occur, sizeof next_occur);
    row->last_occur = g_strdup (last_occur);
    row->next_occur = g_strdup (next_occur);
    return row;
}

static void
sx_list_adapter_rebuild (GncSxListTreeModelAdapter *adapter)
{
    GList *instances;

    if (adapter->disposed)
        return;

    g_list_store_remove_all (adapter->rows);
    for (instances = gnc_sx_instance_model_get_sx_instances_list (adapter->instances);
         instances != NULL; instances = instances->next)
    {
        GncSxListRow *row = sx_list_row_new (instances->data);
        g_list_store_append (adapter->rows, row);
        g_object_unref (row);
    }
}

static void
sx_list_adapter_added (GncSxInstanceModel *instances, SchedXaction *sx, gpointer user_data)
{
    sx_list_adapter_rebuild (GNC_SX_LIST_TREE_MODEL_ADAPTER (user_data));
}

static void
sx_list_adapter_updated (GncSxInstanceModel *instances, SchedXaction *sx, gpointer user_data)
{
    gnc_sx_instance_model_update_sx_instances (instances, sx);
    sx_list_adapter_rebuild (GNC_SX_LIST_TREE_MODEL_ADAPTER (user_data));
}

static void
sx_list_adapter_removing (GncSxInstanceModel *instances, SchedXaction *sx, gpointer user_data)
{
    gnc_sx_instance_model_remove_sx_instances (instances, sx);
    sx_list_adapter_rebuild (GNC_SX_LIST_TREE_MODEL_ADAPTER (user_data));
}

static void
gnc_sx_list_tree_model_adapter_dispose (GObject *object)
{
    GncSxListTreeModelAdapter *adapter = GNC_SX_LIST_TREE_MODEL_ADAPTER (object);

    if (adapter->disposed)
        return;
    adapter->disposed = TRUE;

    if (adapter->instances)
    {
        g_signal_handlers_disconnect_by_data (adapter->instances, adapter);
        g_clear_object (&adapter->instances);
    }
    g_clear_object (&adapter->rows);
    G_OBJECT_CLASS (gnc_sx_list_tree_model_adapter_parent_class)->dispose (object);
}

static void
gnc_sx_list_tree_model_adapter_class_init (GncSxListTreeModelAdapterClass *klass)
{
    G_OBJECT_CLASS (klass)->dispose = gnc_sx_list_tree_model_adapter_dispose;
}

static void
gnc_sx_list_tree_model_adapter_init (GncSxListTreeModelAdapter *adapter)
{
    adapter->rows = g_list_store_new (GNC_TYPE_SX_LIST_ROW);
}

GncSxListTreeModelAdapter*
gnc_sx_list_tree_model_adapter_new (GncSxInstanceModel *instances)
{
    GncSxListTreeModelAdapter *adapter;

    g_return_val_if_fail (GNC_IS_SX_INSTANCE_MODEL (instances), NULL);
    adapter = g_object_new (GNC_TYPE_SX_LIST_TREE_MODEL_ADAPTER, NULL);
    adapter->instances = g_object_ref (instances);
    sx_list_adapter_rebuild (adapter);
    g_signal_connect (instances, "added", G_CALLBACK (sx_list_adapter_added), adapter);
    g_signal_connect (instances, "updated", G_CALLBACK (sx_list_adapter_updated), adapter);
    g_signal_connect (instances, "removing", G_CALLBACK (sx_list_adapter_removing), adapter);
    return adapter;
}

GListModel*
gnc_sx_list_tree_model_adapter_get_model (GncSxListTreeModelAdapter *adapter)
{
    g_return_val_if_fail (GNC_IS_SX_LIST_TREE_MODEL_ADAPTER (adapter), NULL);
    return G_LIST_MODEL (adapter->rows);
}

void
gnc_sx_list_tree_model_adapter_refresh (GncSxListTreeModelAdapter *adapter)
{
    g_return_if_fail (GNC_IS_SX_LIST_TREE_MODEL_ADAPTER (adapter));
    sx_list_adapter_rebuild (adapter);
}

SchedXaction*
gnc_sx_list_row_get_sx (GncSxListRow *row)
{
    g_return_val_if_fail (GNC_IS_SX_LIST_ROW (row), NULL);
    return row->sx;
}

const gchar*
gnc_sx_list_row_get_name (GncSxListRow *row)
{
    return GNC_IS_SX_LIST_ROW (row) ? row->name : NULL;
}

gboolean
gnc_sx_list_row_get_enabled (GncSxListRow *row)
{
    return GNC_IS_SX_LIST_ROW (row) && row->enabled;
}

const gchar*
gnc_sx_list_row_get_frequency (GncSxListRow *row)
{
    return GNC_IS_SX_LIST_ROW (row) ? row->frequency : NULL;
}

guint
gnc_sx_list_row_get_num_postponed (GncSxListRow *row)
{
    return GNC_IS_SX_LIST_ROW (row) ? row->num_postponed : 0;
}

const gchar*
gnc_sx_list_row_get_last_occur (GncSxListRow *row)
{
    return GNC_IS_SX_LIST_ROW (row) ? row->last_occur : NULL;
}

const gchar*
gnc_sx_list_row_get_next_occur (GncSxListRow *row)
{
    return GNC_IS_SX_LIST_ROW (row) ? row->next_occur : NULL;
}
