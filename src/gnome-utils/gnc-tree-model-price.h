/* 
 * gnc-tree-model-price.h -- GtkTreeModel implementation to display prices in a GtkTreeView.
 * Copyright (C) 2003 Jan Arne Petersen
 * Author: Jan Arne Petersen <jpetersen@uni-bonn.de>
 */

#ifndef __GNC_TREE_MODEL_PRICE_H
#define __GNC_TREE_MODEL_PRICE_H

#include <gtk/gtktreemodel.h>

#include "gnc-pricedb.h"

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_TREE_MODEL_PRICE            (gnc_tree_model_price_get_type ())
#define GNC_TREE_MODEL_PRICE(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_TREE_MODEL_PRICE, GncTreeModelPrice))
#define GNC_TREE_MODEL_PRICE_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_TREE_MODEL_PRICE, GncTreeModelPriceClass))
#define GNC_IS_TREE_MODEL_PRICE(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_TREE_MODEL_PRICE))
#define GNC_IS_TREE_MODEL_PRICE_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_TREE_MODEL_PRICE))
#define GNC_TREE_MODEL_PRICE_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_TREE_MODEL_PRICE, GncTreeModelPriceClass))

typedef enum {
	GNC_TREE_MODEL_PRICE_COL_COMMODITY,
	GNC_TREE_MODEL_PRICE_COL_CURRENCY,
	GNC_TREE_MODEL_PRICE_COL_TIME,
	GNC_TREE_MODEL_PRICE_COL_SOURCE,
	GNC_TREE_MODEL_PRICE_COL_TYPE,
	GNC_TREE_MODEL_PRICE_COL_VALUE,
	GNC_TREE_MODEL_PRICE_NUM_COLUMNS
} GncTreeModelPriceColumn;

/* typedefs & structures */
typedef struct GncTreeModelPricePrivate GncTreeModelPricePrivate;

typedef struct {
	GObject parent;

	GncTreeModelPricePrivate *priv;

	int stamp;
} GncTreeModelPrice;

typedef struct {
	GObjectClass parent;
} GncTreeModelPriceClass;

/* function prototypes */
GType         gnc_tree_model_price_get_type   (void);

GtkTreeModel *gnc_tree_model_price_new        (GList *prices);
void          gnc_tree_model_price_set_prices (GncTreeModelPrice *model,
                                               GList *prices);

GNCPrice     *gnc_tree_model_price_get_price  (GncTreeModelPrice *model,
                                               GtkTreeIter *iter);

G_END_DECLS

#endif /* __GNC_TREE_MODEL_PRICE_H */
