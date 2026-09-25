/*
 * Copyright (C) 2003 Jan Arne Petersen <jpetersen@uni-bonn.de>
 * Copyright (C) 2003 David Hampton <hampton@employees.org>
 * Author: Jan Arne Petersen <jpetersen@uni-bonn.de>
 * Author: David Hampton <hampton@employees.org>
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

#ifndef __GNC_TREE_MODEL_COMMODITY_H
#define __GNC_TREE_MODEL_COMMODITY_H
#include <gtk/gtk.h>
#include "gnc-tree-model.h"
#include "gnc-commodity.h"
G_BEGIN_DECLS
#define GNC_TYPE_TREE_MODEL_COMMODITY (gnc_tree_model_commodity_get_type ())
G_DECLARE_FINAL_TYPE (GncTreeModelCommodity, gnc_tree_model_commodity, GNC, TREE_MODEL_COMMODITY, GncTreeModel)
#define GNC_TREE_MODEL_COMMODITY_NAME "GncTreeModelCommodity"
#define GNC_TYPE_TREE_MODEL_COMMODITY_ROW (gnc_tree_model_commodity_row_get_type ())
G_DECLARE_FINAL_TYPE (GncTreeModelCommodityRow, gnc_tree_model_commodity_row, GNC, TREE_MODEL_COMMODITY_ROW, GObject)
typedef enum { GNC_TREE_MODEL_COMMODITY_ROW_NAMESPACE, GNC_TREE_MODEL_COMMODITY_ROW_COMMODITY } GncTreeModelCommodityRowKind;
typedef enum { GNC_TREE_MODEL_COMMODITY_COL_NAMESPACE, GNC_TREE_MODEL_COMMODITY_COL_MNEMONIC, GNC_TREE_MODEL_COMMODITY_COL_USER_SYMBOL, GNC_TREE_MODEL_COMMODITY_COL_FULLNAME, GNC_TREE_MODEL_COMMODITY_COL_PRINTNAME, GNC_TREE_MODEL_COMMODITY_COL_UNIQUE_NAME, GNC_TREE_MODEL_COMMODITY_COL_CUSIP, GNC_TREE_MODEL_COMMODITY_COL_FRACTION, GNC_TREE_MODEL_COMMODITY_COL_QUOTE_FLAG, GNC_TREE_MODEL_COMMODITY_COL_QUOTE_SOURCE, GNC_TREE_MODEL_COMMODITY_COL_QUOTE_TZ, GNC_TREE_MODEL_COMMODITY_COL_LAST_VISIBLE = GNC_TREE_MODEL_COMMODITY_COL_QUOTE_TZ } GncTreeModelCommodityColumn;
GncTreeModelCommodity *gnc_tree_model_commodity_new (QofBook *book, gnc_commodity_table *ct);
GListModel *gnc_tree_model_commodity_get_roots (GncTreeModelCommodity *model);
GListModel *gnc_tree_model_commodity_row_get_children (GncTreeModelCommodityRow *row);
GncTreeModelCommodityRowKind gnc_tree_model_commodity_row_get_kind (GncTreeModelCommodityRow *row);
gnc_commodity_namespace *gnc_tree_model_commodity_row_get_namespace (GncTreeModelCommodityRow *row);
gnc_commodity *gnc_tree_model_commodity_row_get_commodity (GncTreeModelCommodityRow *row);
const gchar *gnc_tree_model_commodity_row_get_id (GncTreeModelCommodityRow *row);
gchar *gnc_tree_model_commodity_row_get_string (GncTreeModelCommodityRow *row, GncTreeModelCommodityColumn column);
gboolean gnc_tree_model_commodity_row_get_boolean (GncTreeModelCommodityRow *row, GncTreeModelCommodityColumn column);
G_END_DECLS
#endif
