/* 
 * gnc-tree-model-commodity.h -- GtkTreeModel implementation to 
 *	display commodities in a GtkTreeView.
 *
 * Copyright (C) 2003 Jan Arne Petersen
 * Author: Jan Arne Petersen <jpetersen@uni-bonn.de>
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
 * Boston, MA  02111-1307,  USA       gnu@gnu.org
 */

#ifndef __GNC_TREE_MODEL_COMMODITY_H
#define __GNC_TREE_MODEL_COMMODITY_H

#include <gtk/gtktreemodel.h>
#include <gtk/gtktreeselection.h>

#include "gnc-commodity.h"

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_TREE_MODEL_COMMODITY            (gnc_tree_model_commodity_get_type ())
#define GNC_TREE_MODEL_COMMODITY(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_TREE_MODEL_COMMODITY, GncTreeModelCommodity))
#define GNC_TREE_MODEL_COMMODITY_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_TREE_MODEL_COMMODITY, GncTreeModelCommodityClass))
#define GNC_IS_TREE_MODEL_COMMODITY(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_TREE_MODEL_COMMODITY))
#define GNC_IS_TREE_MODEL_COMMODITY_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_TREE_MODEL_COMMODITY))
#define GNC_TREE_MODEL_COMMODITY_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_TREE_MODEL_COMMODITY, GncTreeModelCommodityClass))

typedef enum {
	GNC_TREE_MODEL_COMMODITY_COL_MNEMONIC,
	GNC_TREE_MODEL_COMMODITY_COL_NAMESPACE,
	GNC_TREE_MODEL_COMMODITY_COL_FULLNAME,
	GNC_TREE_MODEL_COMMODITY_COL_PRINTNAME,
	GNC_TREE_MODEL_COMMODITY_COL_EXCHANGE_CODE,
	GNC_TREE_MODEL_COMMODITY_COL_UNIQUE_NAME,
	GNC_TREE_MODEL_COMMODITY_COL_FRACTION,
	GNC_TREE_MODEL_COMMODITY_COL_QUOTE_FLAG,
	GNC_TREE_MODEL_COMMODITY_COL_QUOTE_SOURCE,
	GNC_TREE_MODEL_COMMODITY_COL_QUOTE_TZ,
	GNC_TREE_MODEL_COMMODITY_NUM_COLUMNS
} GncTreeModelCommodityColumn;

/* typedefs & structures */
typedef struct GncTreeModelCommodityPrivate GncTreeModelCommodityPrivate;

typedef struct {
	GObject parent;

	GncTreeModelCommodityPrivate *priv;

	int stamp;
} GncTreeModelCommodity;

typedef struct {
	GObjectClass parent;
} GncTreeModelCommodityClass;

/* function prototypes */
GType          gnc_tree_model_commodity_get_type        (void);

GtkTreeModel  *gnc_tree_model_commodity_new             (GList *commodities);

void           gnc_tree_model_commodity_add_commodity (GncTreeModelCommodity *model,
						       gpointer commodity);

void           gnc_tree_model_commodity_remove_commodity (GncTreeModelCommodity *model,
							  gpointer commodity);

void           gnc_tree_model_commodity_set_commodities (GncTreeModelCommodity *model,
                                                         GList *commodities);

gnc_commodity *gnc_tree_model_commodity_get_commodity   (GncTreeModelCommodity *model,
                                                         GtkTreeIter *iter);

G_END_DECLS

#endif /* __GNC_TREE_MODEL_COMMODITY_H */
