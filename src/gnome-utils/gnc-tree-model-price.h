/* 
 * gnc-tree-model-price.h -- GtkTreeModel implementation to display
 *	prices in a GtkTreeView.
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
