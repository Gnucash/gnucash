/* 
 * gnc-tree-model-price.c -- GtkTreeModel implementation to display prices in a GtkTreeView.
 * Copyright (C) 2003 Jan Arne Petersen
 * Author: Jan Arne Petersen <jpetersen@uni-bonn.de>
 */

#include "config.h"

#include <string.h>

#include "gnc-tree-model-price.h"
#include "gnc-ui-util.h"
#include "messages.h"

static void gnc_tree_model_price_class_init (GncTreeModelPriceClass *klass);
static void gnc_tree_model_price_init (GncTreeModelPrice *model);
static void gnc_tree_model_price_finalize (GObject *object);

static void gnc_tree_model_price_tree_model_init (GtkTreeModelIface *iface);
static guint gnc_tree_model_price_get_flags (GtkTreeModel *tree_model);
static int gnc_tree_model_price_get_n_columns (GtkTreeModel *tree_model);
static GType gnc_tree_model_price_get_column_type (GtkTreeModel *tree_model,
						       int index);
static gboolean gnc_tree_model_price_get_iter (GtkTreeModel *tree_model,
						   GtkTreeIter *iter,
						   GtkTreePath *path);
static GtkTreePath *gnc_tree_model_price_get_path (GtkTreeModel *tree_model,
						       GtkTreeIter *iter);
static void gnc_tree_model_price_get_value (GtkTreeModel *tree_model,
						GtkTreeIter *iter,
						int column,
						GValue *value);
static gboolean	gnc_tree_model_price_iter_next (GtkTreeModel *tree_model,
						    GtkTreeIter *iter);
static gboolean	gnc_tree_model_price_iter_children (GtkTreeModel *tree_model,
							GtkTreeIter *iter,
							GtkTreeIter *parent);
static gboolean	gnc_tree_model_price_iter_has_child (GtkTreeModel *tree_model,
							 GtkTreeIter *iter);
static int gnc_tree_model_price_iter_n_children (GtkTreeModel *tree_model,
						     GtkTreeIter *iter);
static gboolean	gnc_tree_model_price_iter_nth_child (GtkTreeModel *tree_model,
							 GtkTreeIter *iter,
							 GtkTreeIter *parent,
							 int n);
static gboolean	gnc_tree_model_price_iter_parent (GtkTreeModel *tree_model,
						      GtkTreeIter *iter,
    						      GtkTreeIter *child);

struct GncTreeModelPricePrivate
{
	GList *prices;
};

static GObjectClass *parent_class = NULL;

GType
gnc_tree_model_price_get_type (void)
{
	static GType gnc_tree_model_price_type = 0;

	if (gnc_tree_model_price_type == 0) {
		static const GTypeInfo our_info = {
			sizeof (GncTreeModelPriceClass),
			NULL,
			NULL,
			(GClassInitFunc) gnc_tree_model_price_class_init,
			NULL,
			NULL,
			sizeof (GncTreeModelPrice),
			0,
			(GInstanceInitFunc) gnc_tree_model_price_init
		};
		
		static const GInterfaceInfo tree_model_info = {
			(GInterfaceInitFunc) gnc_tree_model_price_tree_model_init,
			NULL,
			NULL
		};

		gnc_tree_model_price_type = g_type_register_static (G_TYPE_OBJECT,
									"GncTreeModelPrice",
									&our_info, 0);
		
		g_type_add_interface_static (gnc_tree_model_price_type,
					     GTK_TYPE_TREE_MODEL,
					     &tree_model_info);
	}

	return gnc_tree_model_price_type;
}

static void
gnc_tree_model_price_class_init (GncTreeModelPriceClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	parent_class = g_type_class_peek_parent (klass);

	object_class->finalize = gnc_tree_model_price_finalize;
}

static void
gnc_tree_model_price_init (GncTreeModelPrice *model)
{
	while (model->stamp == 0) {
		model->stamp = g_random_int ();
	}

	model->priv = g_new0 (GncTreeModelPricePrivate, 1);
}

static void
gnc_tree_model_price_finalize (GObject *object)
{
	GncTreeModelPrice *model;

	g_return_if_fail (object != NULL);
	g_return_if_fail (GNC_IS_TREE_MODEL_PRICE (object));

	model = GNC_TREE_MODEL_PRICE (object);

	g_return_if_fail (model->priv != NULL);

	g_list_free (model->priv->prices);
	g_free (model->priv);

	G_OBJECT_CLASS (parent_class)->finalize (object);
}

GtkTreeModel *
gnc_tree_model_price_new (GList *prices)
{
	GncTreeModelPrice *model;

	model = g_object_new (GNC_TYPE_TREE_MODEL_PRICE,
			      NULL);

	model->priv->prices = prices;

	return GTK_TREE_MODEL (model);
}

void
gnc_tree_model_price_set_prices (GncTreeModelPrice *model,
				 GList *prices)
{
	g_return_if_fail (GNC_IS_TREE_MODEL_PRICE (model));
	g_return_if_fail (model->priv != NULL);

	g_list_free (model->priv->prices);

	model->priv->prices = prices;
}

GNCPrice *
gnc_tree_model_price_get_price (GncTreeModelPrice *model,
				GtkTreeIter *iter)
{
	g_return_val_if_fail (GNC_IS_TREE_MODEL_PRICE (model), NULL);
	g_return_val_if_fail (iter != NULL, NULL);
	g_return_val_if_fail (iter->user_data != NULL, NULL);
	g_return_val_if_fail (iter->stamp == model->stamp, NULL);
	
	return (GNCPrice *) ((GList *) iter->user_data)->data;
}

static void
gnc_tree_model_price_tree_model_init (GtkTreeModelIface *iface)
{
	iface->get_flags       = gnc_tree_model_price_get_flags;
	iface->get_n_columns   = gnc_tree_model_price_get_n_columns;
	iface->get_column_type = gnc_tree_model_price_get_column_type;
	iface->get_iter        = gnc_tree_model_price_get_iter;
	iface->get_path        = gnc_tree_model_price_get_path;
	iface->get_value       = gnc_tree_model_price_get_value;
	iface->iter_next       = gnc_tree_model_price_iter_next;
	iface->iter_children   = gnc_tree_model_price_iter_children;
	iface->iter_has_child  = gnc_tree_model_price_iter_has_child;
	iface->iter_n_children = gnc_tree_model_price_iter_n_children;
	iface->iter_nth_child  = gnc_tree_model_price_iter_nth_child;
	iface->iter_parent     = gnc_tree_model_price_iter_parent;
}

static guint
gnc_tree_model_price_get_flags (GtkTreeModel *tree_model)
{
	return 0;
}

static int
gnc_tree_model_price_get_n_columns (GtkTreeModel *tree_model)
{
	return GNC_TREE_MODEL_PRICE_NUM_COLUMNS;
}

static GType
gnc_tree_model_price_get_column_type (GtkTreeModel *tree_model,
				      int index)
{
	g_return_val_if_fail (GNC_IS_TREE_MODEL_PRICE (tree_model), G_TYPE_INVALID);
	g_return_val_if_fail ((index < GNC_TREE_MODEL_PRICE_NUM_COLUMNS) && (index >= 0), G_TYPE_INVALID);

	switch (index) {
		case GNC_TREE_MODEL_PRICE_COL_COMMODITY:
		case GNC_TREE_MODEL_PRICE_COL_CURRENCY:
		case GNC_TREE_MODEL_PRICE_COL_TIME:
		case GNC_TREE_MODEL_PRICE_COL_SOURCE:
		case GNC_TREE_MODEL_PRICE_COL_TYPE:
		case GNC_TREE_MODEL_PRICE_COL_VALUE:
			return G_TYPE_STRING;
		default:
			g_assert_not_reached ();
			return G_TYPE_INVALID;
	}
}

static gboolean
gnc_tree_model_price_get_iter (GtkTreeModel *tree_model,
			       GtkTreeIter *iter,
			       GtkTreePath *path)
{
	GncTreeModelPrice *model = GNC_TREE_MODEL_PRICE (tree_model);
	guint i;

	g_return_val_if_fail (GNC_IS_TREE_MODEL_PRICE (model), FALSE);
	g_return_val_if_fail (gtk_tree_path_get_depth (path) > 0, FALSE);
	
	if (model->priv->prices == NULL)
		return FALSE;

	i = gtk_tree_path_get_indices (path)[0];

	g_return_val_if_fail (i >= 0 && i < g_list_length (model->priv->prices), FALSE);

	iter->stamp = model->stamp;
	iter->user_data = g_list_nth (model->priv->prices, i);

	if (iter->user_data == NULL) {
		iter->stamp = 0;
		return FALSE;
	}

	return TRUE;
}

static GtkTreePath *
gnc_tree_model_price_get_path (GtkTreeModel *tree_model,
			       GtkTreeIter *iter)
{
	GncTreeModelPrice *model = GNC_TREE_MODEL_PRICE (tree_model);
	GtkTreePath *path;

	g_return_val_if_fail (GNC_IS_TREE_MODEL_PRICE (model), NULL);
	g_return_val_if_fail (iter != NULL, NULL);
	g_return_val_if_fail (iter->user_data != NULL, NULL);
	g_return_val_if_fail (iter->stamp == model->stamp, NULL);
	
	if (model->priv->prices == NULL)
		return NULL;

	path = gtk_tree_path_new ();

	gtk_tree_path_append_index (path, g_list_position (model->priv->prices, iter->user_data));

	return path;
}

static void
gnc_tree_model_price_get_value (GtkTreeModel *tree_model,
				GtkTreeIter *iter,
				int column,
				GValue *value)
{
	GncTreeModelPrice *model = GNC_TREE_MODEL_PRICE (tree_model);
	GNCPrice *price;
	gnc_commodity *commodity;

	g_return_if_fail (GNC_IS_TREE_MODEL_PRICE (model));
	g_return_if_fail (iter != NULL);
	g_return_if_fail (iter->user_data != NULL);
	g_return_if_fail (iter->stamp == model->stamp);

	price = (GNCPrice *)((GList *) iter->user_data)->data;
	g_return_if_fail (price != NULL);

	switch (column) {
		case GNC_TREE_MODEL_PRICE_COL_COMMODITY:
			g_value_init (value, G_TYPE_STRING);

			commodity = gnc_price_get_commodity (price);

			g_value_set_string (value, gnc_commodity_get_printname (commodity));
			break;
		case GNC_TREE_MODEL_PRICE_COL_CURRENCY:
			g_value_init (value, G_TYPE_STRING);

			commodity = gnc_price_get_currency (price);

			g_value_set_string (value, gnc_commodity_get_printname (commodity));
			break;
		case GNC_TREE_MODEL_PRICE_COL_TIME:
			g_value_init (value, G_TYPE_STRING);

			g_value_set_string (value, gnc_print_date (gnc_price_get_time (price)));
			break;
		case GNC_TREE_MODEL_PRICE_COL_SOURCE:
			g_value_init (value, G_TYPE_STRING);

			g_value_set_string (value, gettext (gnc_price_get_source (price)));
			break;
		case GNC_TREE_MODEL_PRICE_COL_TYPE:
			g_value_init (value, G_TYPE_STRING);

			g_value_set_string (value, gnc_price_get_type (price));
			break;
		case GNC_TREE_MODEL_PRICE_COL_VALUE:
			g_value_init (value, G_TYPE_STRING);

			g_value_set_string (value, xaccPrintAmount (gnc_price_get_value (price),
							            gnc_default_price_print_info ()));
			break;
		default:
			g_assert_not_reached ();
	}
}

static gboolean
gnc_tree_model_price_iter_next (GtkTreeModel *tree_model,
				GtkTreeIter *iter)
{
	GncTreeModelPrice *model = GNC_TREE_MODEL_PRICE (tree_model);

	g_return_val_if_fail (GNC_IS_TREE_MODEL_PRICE (model), FALSE);
	g_return_val_if_fail (iter != NULL, FALSE);
	g_return_val_if_fail (iter->user_data != NULL, FALSE);
	g_return_val_if_fail (iter->stamp == model->stamp, FALSE);

	if (g_list_next (iter->user_data) == NULL)
		return FALSE;

	iter->user_data = g_list_next (iter->user_data);

	return TRUE;
}

static gboolean
gnc_tree_model_price_iter_children (GtkTreeModel *tree_model,
				    GtkTreeIter *iter,
				    GtkTreeIter *parent)
{

	g_return_val_if_fail (GNC_IS_TREE_MODEL_PRICE (tree_model), FALSE);

	if (parent != NULL)
		return FALSE;

	iter->stamp = GNC_TREE_MODEL_PRICE (tree_model)->stamp;
	iter->user_data = GNC_TREE_MODEL_PRICE (tree_model)->priv->prices;

	return TRUE;
}

static gboolean
gnc_tree_model_price_iter_has_child (GtkTreeModel *tree_model,
				     GtkTreeIter *iter)
{
	return FALSE;
}

static int
gnc_tree_model_price_iter_n_children (GtkTreeModel *tree_model,
				      GtkTreeIter *iter)
{
	g_return_val_if_fail (GNC_IS_TREE_MODEL_PRICE (tree_model), -1);

	if (iter == NULL)
		return g_list_length (GNC_TREE_MODEL_PRICE (tree_model)->priv->prices);
	
	g_return_val_if_fail (GNC_TREE_MODEL_PRICE (tree_model)->stamp == iter->stamp, -1);

	return 0;
}

static gboolean
gnc_tree_model_price_iter_nth_child (GtkTreeModel *tree_model,
				     GtkTreeIter *iter,
				     GtkTreeIter *parent,
					       int n)
{
	GncTreeModelPrice *model;
	
	g_return_val_if_fail (GNC_IS_TREE_MODEL_PRICE (tree_model), FALSE);

	if (parent != NULL)
		return FALSE;

	model = GNC_TREE_MODEL_PRICE (tree_model);

	g_return_val_if_fail (n >= 0 && n < (int)g_list_length (model->priv->prices), FALSE);

	iter->stamp = model->stamp;
	iter->user_data = g_list_nth (model->priv->prices, n);

	if (iter->user_data == NULL) {
		iter->stamp = 0;
		return FALSE;
	}

	return TRUE;
}

static gboolean
gnc_tree_model_price_iter_parent (GtkTreeModel *tree_model,
				  GtkTreeIter *iter,
    				  GtkTreeIter *child)
{
	return FALSE;
}
