/* 
 * gnc-tree-model-commodity.h -- GtkTreeModel implementation to display commodities in a GtkTreeView.
 * Copyright (C) 2003 Jan Arne Petersen
 * Author: Jan Arne Petersen <jpetersen@uni-bonn.de>
 */

#ifndef __GNC_TREE_MODEL_COMMODITY_H
#define __GNC_TREE_MODEL_COMMODITY_H

#include <gtk/gtktreemodel.h>

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
	GNC_TREE_MODEL_COMMODITY_COL_MARK,
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
void           gnc_tree_model_commodity_set_commodities (GncTreeModelCommodity *model,
                                                         GList *commodities);

gnc_commodity *gnc_tree_model_commodity_get_commodity   (GncTreeModelCommodity *model,
                                                         GtkTreeIter *iter);

G_END_DECLS

#endif /* __GNC_TREE_MODEL_COMMODITY_H */
