/*
 * Copyright (C) 2003 Jan Arne Petersen <jpetersen@uni-bonn.de>
 * Copyright (C) 2003 David Hampton <hampton@employees.org>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
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

#include <config.h>
#include <glib/gi18n.h>
#include "gnc-tree-model-commodity.h"
#include "gnc-component-manager.h"
#include "gnc-engine.h"
#include "gnc-ui-util.h"
struct _GncTreeModelCommodityRow
{
    GObject parent_instance;
    GncTreeModelCommodityRowKind kind;
    gnc_commodity_namespace *name_space;
    gnc_commodity *commodity;
    GListStore *children;
    gchar *id;
};
struct _GncTreeModelCommodity
{
    GncTreeModel parent_instance;
    QofBook *book;
    gnc_commodity_table *commodity_table;
    GListStore *roots;
    gint event_handler_id;
};
enum
{
    REBUILDING, CHANGED, LAST_SIGNAL
};
static guint signals[LAST_SIGNAL];
G_DEFINE_TYPE (GncTreeModelCommodityRow, gnc_tree_model_commodity_row, G_TYPE_OBJECT)

G_DEFINE_TYPE (GncTreeModelCommodity, gnc_tree_model_commodity, GNC_TYPE_TREE_MODEL)

static gchar *
commodity_id (gnc_commodity *commodity)
{
    gchar guid[GUID_ENCODING_LENGTH + 1];
    guid_to_string_buff (qof_instance_get_guid (QOF_INSTANCE (commodity)), guid);
    return g_strconcat ("commodity:", guid, NULL);
}
static GncTreeModelCommodityRow *
row_new_namespace (gnc_commodity_namespace *name_space)
{
    GncTreeModelCommodityRow *row = g_object_new (GNC_TYPE_TREE_MODEL_COMMODITY_ROW, NULL);
    row->kind = GNC_TREE_MODEL_COMMODITY_ROW_NAMESPACE;
    row->name_space = name_space;
    row->id = g_strconcat ("namespace:", gnc_commodity_namespace_get_name (name_space), NULL);
    row->children = g_list_store_new (GNC_TYPE_TREE_MODEL_COMMODITY_ROW);
    return row;
}
static GncTreeModelCommodityRow *
row_new_commodity (gnc_commodity *commodity)
{
    GncTreeModelCommodityRow *row = g_object_new (GNC_TYPE_TREE_MODEL_COMMODITY_ROW, NULL);
    row->kind = GNC_TREE_MODEL_COMMODITY_ROW_COMMODITY;
    row->commodity = commodity;
    row->id = commodity_id (commodity);
    return row;
}
static void
row_dispose (GObject *object)
{
    GncTreeModelCommodityRow *row = GNC_TREE_MODEL_COMMODITY_ROW (object);
    g_clear_object (&row->children);
    g_clear_pointer (&row->id, g_free);
    G_OBJECT_CLASS (gnc_tree_model_commodity_row_parent_class)->dispose (object);
}
static void
gnc_tree_model_commodity_row_class_init (GncTreeModelCommodityRowClass *klass)
{
    G_OBJECT_CLASS (klass)->dispose = row_dispose;
}
static void
gnc_tree_model_commodity_row_init (GncTreeModelCommodityRow *row)
{
    (void)row;
}
static void
rebuild (GncTreeModelCommodity *model)
{
    GList *namespaces;
    if (!model->commodity_table) return;
    g_signal_emit (model, signals[REBUILDING], 0);
    g_list_store_remove_all (model->roots);
    namespaces = gnc_commodity_table_get_namespaces_list (model->commodity_table);
    for (GList *node = namespaces; node; node = node->next)
    {
        gnc_commodity_namespace *name_space = node->data;
        GncTreeModelCommodityRow *ns_row = row_new_namespace (name_space);
        GList *commodities = gnc_commodity_namespace_get_commodity_list (name_space);
        for (GList *child = commodities; child; child = child->next)
        {
            GncTreeModelCommodityRow *commodity_row = row_new_commodity (child->data);
            g_list_store_append (ns_row->children, commodity_row);
            g_object_unref (commodity_row);
        }
        g_list_free (commodities);
        g_list_store_append (model->roots, ns_row);
        g_object_unref (ns_row);
    }
    g_list_free (namespaces);
    g_signal_emit (model, signals[CHANGED], 0);
}
static void
event_handler (QofInstance *entity, QofEventId event_type, gpointer user_data, gpointer event_data)
{
    GncTreeModelCommodity *model = GNC_TREE_MODEL_COMMODITY (user_data);
    if (!GNC_IS_TREE_MODEL_COMMODITY (model) || !entity || qof_instance_get_book (entity) != model->book) return;
    if (GNC_IS_COMMODITY (entity) || GNC_IS_COMMODITY_NAMESPACE (entity)) rebuild (model);
    (void)event_type;
    (void)event_data;
}
static void
model_dispose (GObject *object)
{
    GncTreeModelCommodity *model = GNC_TREE_MODEL_COMMODITY (object);
    if (model->event_handler_id) qof_event_unregister_handler (model->event_handler_id);
    model->event_handler_id = 0;
    g_clear_object (&model->roots);
    model->book = NULL;
    model->commodity_table = NULL;
    G_OBJECT_CLASS (gnc_tree_model_commodity_parent_class)->dispose (object);
}
static void
gnc_tree_model_commodity_class_init (GncTreeModelCommodityClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);
    object_class->dispose = model_dispose;
    signals[REBUILDING] = g_signal_new ("rebuilding", G_TYPE_FROM_CLASS (klass), G_SIGNAL_RUN_LAST, 0, NULL, NULL, NULL, G_TYPE_NONE, 0);
    signals[CHANGED] = g_signal_new ("changed", G_TYPE_FROM_CLASS (klass), G_SIGNAL_RUN_LAST, 0, NULL, NULL, NULL, G_TYPE_NONE, 0);
}
static void
gnc_tree_model_commodity_init (GncTreeModelCommodity *model)
{
    model->roots = g_list_store_new (GNC_TYPE_TREE_MODEL_COMMODITY_ROW);
}
GncTreeModelCommodity *
gnc_tree_model_commodity_new (QofBook *book, gnc_commodity_table *ct)
{
    GncTreeModelCommodity *model;
    g_return_val_if_fail (book && ct, NULL);
    model = g_object_new (GNC_TYPE_TREE_MODEL_COMMODITY, NULL);
    model->book = book;
    model->commodity_table = ct;
    model->event_handler_id = qof_event_register_handler (event_handler, model);
    rebuild (model);
    return model;
}
GListModel *
gnc_tree_model_commodity_get_roots (GncTreeModelCommodity *model)
{
    g_return_val_if_fail (GNC_IS_TREE_MODEL_COMMODITY (model), NULL);
    return G_LIST_MODEL (model->roots);
}
GListModel *
gnc_tree_model_commodity_row_get_children (GncTreeModelCommodityRow *row)
{
    g_return_val_if_fail (GNC_IS_TREE_MODEL_COMMODITY_ROW (row), NULL);
    return row->children? G_LIST_MODEL (row->children): NULL;
}
GncTreeModelCommodityRowKind
gnc_tree_model_commodity_row_get_kind (GncTreeModelCommodityRow *row)
{
    g_return_val_if_fail (GNC_IS_TREE_MODEL_COMMODITY_ROW (row), GNC_TREE_MODEL_COMMODITY_ROW_NAMESPACE);
    return row->kind;
}
gnc_commodity_namespace *
gnc_tree_model_commodity_row_get_namespace (GncTreeModelCommodityRow *row)
{
    g_return_val_if_fail (GNC_IS_TREE_MODEL_COMMODITY_ROW (row), NULL);
    return row->kind == GNC_TREE_MODEL_COMMODITY_ROW_NAMESPACE? row->name_space: NULL;
}
gnc_commodity *
gnc_tree_model_commodity_row_get_commodity (GncTreeModelCommodityRow *row)
{
    g_return_val_if_fail (GNC_IS_TREE_MODEL_COMMODITY_ROW (row), NULL);
    return row->kind == GNC_TREE_MODEL_COMMODITY_ROW_COMMODITY? row->commodity: NULL;
}
const gchar *
gnc_tree_model_commodity_row_get_id (GncTreeModelCommodityRow *row)
{
    g_return_val_if_fail (GNC_IS_TREE_MODEL_COMMODITY_ROW (row), NULL);
    return row->id;
}
gchar *
gnc_tree_model_commodity_row_get_string (GncTreeModelCommodityRow *row, GncTreeModelCommodityColumn column)
{
    gnc_commodity *commodity;
    if (!GNC_IS_TREE_MODEL_COMMODITY_ROW (row)) return g_strdup ("");
    if (row->kind == GNC_TREE_MODEL_COMMODITY_ROW_NAMESPACE) return column == GNC_TREE_MODEL_COMMODITY_COL_NAMESPACE? g_strdup (_(gnc_commodity_namespace_get_gui_name (row->name_space))): g_strdup ("");
    commodity = row->commodity;
    switch (column)
    {
        case GNC_TREE_MODEL_COMMODITY_COL_MNEMONIC: return g_strdup (gnc_commodity_get_mnemonic (commodity));
        case GNC_TREE_MODEL_COMMODITY_COL_USER_SYMBOL: return g_strdup (gnc_commodity_get_nice_symbol (commodity));
        case GNC_TREE_MODEL_COMMODITY_COL_FULLNAME: return g_strdup (gnc_commodity_get_fullname (commodity));
        case GNC_TREE_MODEL_COMMODITY_COL_PRINTNAME: return g_strdup (gnc_commodity_get_printname (commodity));
        case GNC_TREE_MODEL_COMMODITY_COL_UNIQUE_NAME: return g_strdup (gnc_commodity_get_unique_name (commodity));
        case GNC_TREE_MODEL_COMMODITY_COL_CUSIP: return g_strdup (gnc_commodity_get_cusip (commodity));
        case GNC_TREE_MODEL_COMMODITY_COL_FRACTION: return g_strdup_printf ("%d", gnc_commodity_get_fraction (commodity));
        case GNC_TREE_MODEL_COMMODITY_COL_QUOTE_SOURCE: return g_strdup (gnc_commodity_get_quote_flag (commodity)? gnc_quote_source_get_internal_name (gnc_commodity_get_quote_source (commodity)): "");
        case GNC_TREE_MODEL_COMMODITY_COL_QUOTE_TZ: return g_strdup (gnc_commodity_get_quote_flag (commodity)? gnc_commodity_get_quote_tz (commodity): "");
        default: return g_strdup ("");
    }
}
gboolean
gnc_tree_model_commodity_row_get_boolean (GncTreeModelCommodityRow *row, GncTreeModelCommodityColumn column)
{
    if (!GNC_IS_TREE_MODEL_COMMODITY_ROW (row) || row->kind != GNC_TREE_MODEL_COMMODITY_ROW_COMMODITY) return FALSE;
    return column == GNC_TREE_MODEL_COMMODITY_COL_QUOTE_FLAG && gnc_commodity_get_quote_flag (row->commodity);
}
