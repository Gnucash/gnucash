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
#include <string.h>
#include "gnc-tree-model-price.h"
#include "gnc-component-manager.h"
#include "gnc-engine.h"
#include "gnc-ui-util.h"
struct _GncTreeModelPriceRow
{
    GObject parent_instance;
    GncTreeModelPriceRowKind kind;
    gnc_commodity_namespace *name_space;
    gnc_commodity *commodity;
    GNCPrice *price;
    GListStore *children;
    gchar *id;
};
struct _GncTreeModelPrice
{
    GncTreeModel parent_instance;
    QofBook *book;
    GNCPriceDB *price_db;
    GListStore *roots;
    gint event_handler_id;
    GNCPrintAmountInfo print_info;
};
enum
{
    REBUILDING, CHANGED, LAST_SIGNAL
};
static guint signals[LAST_SIGNAL];
G_DEFINE_TYPE (GncTreeModelPriceRow, gnc_tree_model_price_row, G_TYPE_OBJECT)

G_DEFINE_TYPE (GncTreeModelPrice, gnc_tree_model_price, GNC_TYPE_TREE_MODEL)

static gchar *
instance_id (const gchar *prefix, QofInstance *instance)
{
    gchar guid[GUID_ENCODING_LENGTH + 1];
    guid_to_string_buff (qof_instance_get_guid (instance), guid);
    return g_strconcat (prefix, guid, NULL);
}
static GncTreeModelPriceRow *
row_new_namespace (gnc_commodity_namespace *name_space)
{
    GncTreeModelPriceRow *row = g_object_new (GNC_TYPE_TREE_MODEL_PRICE_ROW, NULL);
    row->kind = GNC_TREE_MODEL_PRICE_ROW_NAMESPACE;
    row->name_space = name_space;
    row->id = g_strconcat ("namespace:", gnc_commodity_namespace_get_name (name_space), NULL);
    row->children = g_list_store_new (GNC_TYPE_TREE_MODEL_PRICE_ROW);
    return row;
}
static GncTreeModelPriceRow *
row_new_commodity (gnc_commodity *commodity)
{
    GncTreeModelPriceRow *row = g_object_new (GNC_TYPE_TREE_MODEL_PRICE_ROW, NULL);
    row->kind = GNC_TREE_MODEL_PRICE_ROW_COMMODITY;
    row->commodity = commodity;
    row->id = instance_id ("commodity:", QOF_INSTANCE (commodity));
    row->children = g_list_store_new (GNC_TYPE_TREE_MODEL_PRICE_ROW);
    return row;
}
static GncTreeModelPriceRow *
row_new_price (GNCPrice *price)
{
    GncTreeModelPriceRow *row = g_object_new (GNC_TYPE_TREE_MODEL_PRICE_ROW, NULL);
    row->kind = GNC_TREE_MODEL_PRICE_ROW_PRICE;
    row->price = price;
    gnc_price_ref (price);
    row->id = instance_id ("price:", QOF_INSTANCE (price));
    return row;
}
static void
row_dispose (GObject *object)
{
    GncTreeModelPriceRow *row = GNC_TREE_MODEL_PRICE_ROW (object);
    g_clear_object (&row->children);
    if (row->price) gnc_price_unref (row->price);
    row->price = NULL;
    g_clear_pointer (&row->id, g_free);
    G_OBJECT_CLASS (gnc_tree_model_price_row_parent_class)->dispose (object);
}
static void
gnc_tree_model_price_row_class_init (GncTreeModelPriceRowClass *klass)
{
    G_OBJECT_CLASS (klass)->dispose = row_dispose;
}
static void
gnc_tree_model_price_row_init (GncTreeModelPriceRow *row)
{
    (void)row;
}
static void
rebuild (GncTreeModelPrice *model)
{
    gnc_commodity_table *table;
    GList *namespaces;
    g_signal_emit (model, signals[REBUILDING], 0);
    g_list_store_remove_all (model->roots);
    table = gnc_commodity_table_get_table (model->book);
    namespaces = gnc_commodity_table_get_namespaces_list (table);
    for (GList *node = namespaces; node; node = node->next)
    {
        GncTreeModelPriceRow *ns_row = row_new_namespace (node->data);
        GList *commodities = gnc_commodity_namespace_get_commodity_list (node->data);
        for (GList *child = commodities; child; child = child->next)
        {
            gnc_commodity *commodity = child->data;
            GncTreeModelPriceRow *commodity_row = row_new_commodity (commodity);
            PriceList *prices = gnc_pricedb_get_prices (model->price_db, commodity, NULL);
            for (GList *price_node = prices; price_node; price_node = price_node->next)
            {
                GncTreeModelPriceRow *price_row = row_new_price (price_node->data);
                g_list_store_append (commodity_row->children, price_row);
                g_object_unref (price_row);
            }
            gnc_price_list_destroy (prices);
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
    GncTreeModelPrice *model = GNC_TREE_MODEL_PRICE (user_data);
    if (!GNC_IS_TREE_MODEL_PRICE (model) || !entity || qof_instance_get_book (entity) != model->book) return;
    if (GNC_IS_COMMODITY (entity) || GNC_IS_COMMODITY_NAMESPACE (entity) || GNC_IS_PRICE (entity))
    {
        gnc_pricedb_nth_price_reset_cache (model->price_db);
        rebuild (model);
    }
    (void)event_type;
    (void)event_data;
}
static void
model_dispose (GObject *object)
{
    GncTreeModelPrice *model = GNC_TREE_MODEL_PRICE (object);
    if (model->event_handler_id) qof_event_unregister_handler (model->event_handler_id);
    model->event_handler_id = 0;
    g_clear_object (&model->roots);
    model->book = NULL;
    model->price_db = NULL;
    G_OBJECT_CLASS (gnc_tree_model_price_parent_class)->dispose (object);
}
static void
gnc_tree_model_price_class_init (GncTreeModelPriceClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);
    object_class->dispose = model_dispose;
    signals[REBUILDING] = g_signal_new ("rebuilding", G_TYPE_FROM_CLASS (klass), G_SIGNAL_RUN_LAST, 0, NULL, NULL, NULL, G_TYPE_NONE, 0);
    signals[CHANGED] = g_signal_new ("changed", G_TYPE_FROM_CLASS (klass), G_SIGNAL_RUN_LAST, 0, NULL, NULL, NULL, G_TYPE_NONE, 0);
}
static void
gnc_tree_model_price_init (GncTreeModelPrice *model)
{
    model->roots = g_list_store_new (GNC_TYPE_TREE_MODEL_PRICE_ROW);
    model->print_info = gnc_default_price_print_info (NULL);
}
GncTreeModelPrice *
gnc_tree_model_price_new (QofBook *book, GNCPriceDB *price_db)
{
    GncTreeModelPrice *model;
    g_return_val_if_fail (book && price_db, NULL);
    model = g_object_new (GNC_TYPE_TREE_MODEL_PRICE, NULL);
    model->book = book;
    model->price_db = price_db;
    model->event_handler_id = qof_event_register_handler (event_handler, model);
    rebuild (model);
    return model;
}
GListModel *
gnc_tree_model_price_get_roots (GncTreeModelPrice *model)
{
    g_return_val_if_fail (GNC_IS_TREE_MODEL_PRICE (model), NULL);
    return G_LIST_MODEL (model->roots);
}
GListModel *
gnc_tree_model_price_row_get_children (GncTreeModelPriceRow *row)
{
    g_return_val_if_fail (GNC_IS_TREE_MODEL_PRICE_ROW (row), NULL);
    return row->children? G_LIST_MODEL (row->children): NULL;
}
GncTreeModelPriceRowKind
gnc_tree_model_price_row_get_kind (GncTreeModelPriceRow *row)
{
    g_return_val_if_fail (GNC_IS_TREE_MODEL_PRICE_ROW (row), GNC_TREE_MODEL_PRICE_ROW_NAMESPACE);
    return row->kind;
}
gnc_commodity_namespace *
gnc_tree_model_price_row_get_namespace (GncTreeModelPriceRow *row)
{
    g_return_val_if_fail (GNC_IS_TREE_MODEL_PRICE_ROW (row), NULL);
    return row->kind == GNC_TREE_MODEL_PRICE_ROW_NAMESPACE? row->name_space: NULL;
}
gnc_commodity *
gnc_tree_model_price_row_get_commodity (GncTreeModelPriceRow *row)
{
    g_return_val_if_fail (GNC_IS_TREE_MODEL_PRICE_ROW (row), NULL);
    return row->kind == GNC_TREE_MODEL_PRICE_ROW_COMMODITY? row->commodity: NULL;
}
GNCPrice *
gnc_tree_model_price_row_get_price (GncTreeModelPriceRow *row)
{
    g_return_val_if_fail (GNC_IS_TREE_MODEL_PRICE_ROW (row), NULL);
    return row->kind == GNC_TREE_MODEL_PRICE_ROW_PRICE? row->price: NULL;
}
const gchar *
gnc_tree_model_price_row_get_id (GncTreeModelPriceRow *row)
{
    g_return_val_if_fail (GNC_IS_TREE_MODEL_PRICE_ROW (row), NULL);
    return row->id;
}
gchar *
gnc_tree_model_price_row_get_string (GncTreeModelPriceRow *row, GncTreeModelPriceColumn column)
{
    GNCPrice *price;
    gnc_commodity *commodity;
    char date[MAX_DATE_LENGTH + 1] =
    {
        0
    };
    if (!GNC_IS_TREE_MODEL_PRICE_ROW (row)) return g_strdup ("");
    if (row->kind == GNC_TREE_MODEL_PRICE_ROW_NAMESPACE) return column == GNC_TREE_MODEL_PRICE_COL_COMMODITY? g_strdup (_(gnc_commodity_namespace_get_gui_name (row->name_space))): g_strdup ("");
    if (row->kind == GNC_TREE_MODEL_PRICE_ROW_COMMODITY) return column == GNC_TREE_MODEL_PRICE_COL_COMMODITY? g_strdup (gnc_commodity_get_printname (row->commodity)): g_strdup ("");
    price = row->price;
    switch (column)
    {
        case GNC_TREE_MODEL_PRICE_COL_COMMODITY: commodity = gnc_price_get_commodity (price);
        return g_strdup (commodity? gnc_commodity_get_printname (commodity): "");
        case GNC_TREE_MODEL_PRICE_COL_CURRENCY: commodity = gnc_price_get_currency (price);
        return g_strdup (commodity? gnc_commodity_get_printname (commodity): "");
        case GNC_TREE_MODEL_PRICE_COL_DATE: qof_print_date_buff (date, MAX_DATE_LENGTH, gnc_price_get_time64 (price));
        return g_strdup (date);
        case GNC_TREE_MODEL_PRICE_COL_SOURCE: return g_strdup (gettext (gnc_price_get_source_string (price)));
        case GNC_TREE_MODEL_PRICE_COL_TYPE: return g_strdup (gnc_price_get_typestr (price));
        case GNC_TREE_MODEL_PRICE_COL_VALUE: return g_strdup (xaccPrintAmount (gnc_price_get_value (price), gnc_default_price_print_info (NULL)));
        default: return g_strdup ("");
    }
}
