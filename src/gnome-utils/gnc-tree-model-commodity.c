/* 
 * gnc-tree-model-commodity.c -- GtkTreeModel implementation to display commodities in a GtkTreeView.
 * Copyright (C) 2003 Jan Arne Petersen
 * Author: Jan Arne Petersen <jpetersen@uni-bonn.de>
 */

#include "config.h"

#include <string.h>

#include "gnc-tree-model-commodity.h"

static void gnc_tree_model_commodity_class_init (GncTreeModelCommodityClass *klass);
static void gnc_tree_model_commodity_init (GncTreeModelCommodity *model);
static void gnc_tree_model_commodity_finalize (GObject *object);

static void gnc_tree_model_commodity_tree_model_init (GtkTreeModelIface *iface);
static guint gnc_tree_model_commodity_get_flags (GtkTreeModel *tree_model);
static int gnc_tree_model_commodity_get_n_columns (GtkTreeModel *tree_model);
static GType gnc_tree_model_commodity_get_column_type (GtkTreeModel *tree_model,
						       int index);
static gboolean gnc_tree_model_commodity_get_iter (GtkTreeModel *tree_model,
						   GtkTreeIter *iter,
						   GtkTreePath *path);
static GtkTreePath *gnc_tree_model_commodity_get_path (GtkTreeModel *tree_model,
						       GtkTreeIter *iter);
static void gnc_tree_model_commodity_get_value (GtkTreeModel *tree_model,
						GtkTreeIter *iter,
						int column,
						GValue *value);
static gboolean	gnc_tree_model_commodity_iter_next (GtkTreeModel *tree_model,
						    GtkTreeIter *iter);
static gboolean	gnc_tree_model_commodity_iter_children (GtkTreeModel *tree_model,
							GtkTreeIter *iter,
							GtkTreeIter *parent);
static gboolean	gnc_tree_model_commodity_iter_has_child (GtkTreeModel *tree_model,
							 GtkTreeIter *iter);
static int gnc_tree_model_commodity_iter_n_children (GtkTreeModel *tree_model,
						     GtkTreeIter *iter);
static gboolean	gnc_tree_model_commodity_iter_nth_child (GtkTreeModel *tree_model,
							 GtkTreeIter *iter,
							 GtkTreeIter *parent,
							 int n);
static gboolean	gnc_tree_model_commodity_iter_parent (GtkTreeModel *tree_model,
						      GtkTreeIter *iter,
    						      GtkTreeIter *child);

struct GncTreeModelCommodityPrivate
{
	GList *commodities;
};

static GObjectClass *parent_class = NULL;

GType
gnc_tree_model_commodity_get_type (void)
{
	static GType gnc_tree_model_commodity_type = 0;

	if (gnc_tree_model_commodity_type == 0) {
		static const GTypeInfo our_info = {
			sizeof (GncTreeModelCommodityClass),
			NULL,
			NULL,
			(GClassInitFunc) gnc_tree_model_commodity_class_init,
			NULL,
			NULL,
			sizeof (GncTreeModelCommodity),
			0,
			(GInstanceInitFunc) gnc_tree_model_commodity_init
		};
		
		static const GInterfaceInfo tree_model_info = {
			(GInterfaceInitFunc) gnc_tree_model_commodity_tree_model_init,
			NULL,
			NULL
		};

		gnc_tree_model_commodity_type = g_type_register_static (G_TYPE_OBJECT,
									"GncTreeModelCommodity",
									&our_info, 0);
		
		g_type_add_interface_static (gnc_tree_model_commodity_type,
					     GTK_TYPE_TREE_MODEL,
					     &tree_model_info);
	}

	return gnc_tree_model_commodity_type;
}

static void
gnc_tree_model_commodity_class_init (GncTreeModelCommodityClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	parent_class = g_type_class_peek_parent (klass);

	object_class->finalize = gnc_tree_model_commodity_finalize;
}

static void
gnc_tree_model_commodity_init (GncTreeModelCommodity *model)
{
	while (model->stamp == 0) {
		model->stamp = g_random_int ();
	}

	model->priv = g_new0 (GncTreeModelCommodityPrivate, 1);
}

static void
gnc_tree_model_commodity_finalize (GObject *object)
{
	GncTreeModelCommodity *model;

	g_return_if_fail (object != NULL);
	g_return_if_fail (GNC_IS_TREE_MODEL_COMMODITY (object));

	model = GNC_TREE_MODEL_COMMODITY (object);

	g_return_if_fail (model->priv != NULL);

	g_list_free (model->priv->commodities);
	g_free (model->priv);

	G_OBJECT_CLASS (parent_class)->finalize (object);
}

GtkTreeModel *
gnc_tree_model_commodity_new (GList *commodities)
{
	GncTreeModelCommodity *model;

	model = g_object_new (GNC_TYPE_TREE_MODEL_COMMODITY,
			      NULL);

	model->priv->commodities = commodities;

	return GTK_TREE_MODEL (model);
}

void
gnc_tree_model_commodity_set_commodities (GncTreeModelCommodity *model,
					  GList *commodities)
{
	g_return_if_fail (GNC_IS_TREE_MODEL_COMMODITY (model));
	g_return_if_fail (model->priv != NULL);

	g_list_free (model->priv->commodities);

	model->priv->commodities = commodities;
}

gnc_commodity *
gnc_tree_model_commodity_get_commodity (GncTreeModelCommodity *model,
					GtkTreeIter *iter)
{
	g_return_val_if_fail (GNC_IS_TREE_MODEL_COMMODITY (model), NULL);
	g_return_val_if_fail (iter != NULL, NULL);
	g_return_val_if_fail (iter->user_data != NULL, NULL);
	g_return_val_if_fail (iter->stamp == model->stamp, NULL);
	
	return (gnc_commodity *) ((GList *) iter->user_data)->data;
}

static void
gnc_tree_model_commodity_tree_model_init (GtkTreeModelIface *iface)
{
	iface->get_flags       = gnc_tree_model_commodity_get_flags;
	iface->get_n_columns   = gnc_tree_model_commodity_get_n_columns;
	iface->get_column_type = gnc_tree_model_commodity_get_column_type;
	iface->get_iter        = gnc_tree_model_commodity_get_iter;
	iface->get_path        = gnc_tree_model_commodity_get_path;
	iface->get_value       = gnc_tree_model_commodity_get_value;
	iface->iter_next       = gnc_tree_model_commodity_iter_next;
	iface->iter_children   = gnc_tree_model_commodity_iter_children;
	iface->iter_has_child  = gnc_tree_model_commodity_iter_has_child;
	iface->iter_n_children = gnc_tree_model_commodity_iter_n_children;
	iface->iter_nth_child  = gnc_tree_model_commodity_iter_nth_child;
	iface->iter_parent     = gnc_tree_model_commodity_iter_parent;
}

static guint
gnc_tree_model_commodity_get_flags (GtkTreeModel *tree_model)
{
	return 0;
}

static int
gnc_tree_model_commodity_get_n_columns (GtkTreeModel *tree_model)
{
	return GNC_TREE_MODEL_COMMODITY_NUM_COLUMNS;
}

static GType
gnc_tree_model_commodity_get_column_type (GtkTreeModel *tree_model,
					        int index)
{
	g_return_val_if_fail (GNC_IS_TREE_MODEL_COMMODITY (tree_model), G_TYPE_INVALID);
	g_return_val_if_fail ((index < GNC_TREE_MODEL_COMMODITY_NUM_COLUMNS) && (index >= 0), G_TYPE_INVALID);

	switch (index) {
		case GNC_TREE_MODEL_COMMODITY_COL_MNEMONIC:
		case GNC_TREE_MODEL_COMMODITY_COL_NAMESPACE:
		case GNC_TREE_MODEL_COMMODITY_COL_FULLNAME:
		case GNC_TREE_MODEL_COMMODITY_COL_PRINTNAME:
		case GNC_TREE_MODEL_COMMODITY_COL_EXCHANGE_CODE:
		case GNC_TREE_MODEL_COMMODITY_COL_UNIQUE_NAME:
			return G_TYPE_STRING;
		case GNC_TREE_MODEL_COMMODITY_COL_FRACTION:
		case GNC_TREE_MODEL_COMMODITY_COL_MARK:
			return G_TYPE_INT;
		default:
			g_assert_not_reached ();
			return G_TYPE_INVALID;
	}
}

static gboolean
gnc_tree_model_commodity_get_iter (GtkTreeModel *tree_model,
				   GtkTreeIter *iter,
				   GtkTreePath *path)
{
	GncTreeModelCommodity *model = GNC_TREE_MODEL_COMMODITY (tree_model);
	guint i;

	g_return_val_if_fail (GNC_IS_TREE_MODEL_COMMODITY (model), FALSE);
	g_return_val_if_fail (gtk_tree_path_get_depth (path) > 0, FALSE);
	
	if (model->priv->commodities == NULL)
		return FALSE;

	i = gtk_tree_path_get_indices (path)[0];

	g_return_val_if_fail (i >= 0 && i < g_list_length (model->priv->commodities), FALSE);

	iter->stamp = model->stamp;
	iter->user_data = g_list_nth (model->priv->commodities, i);

	if (iter->user_data == NULL) {
		iter->stamp = 0;
		return FALSE;
	}

	return TRUE;
}

static GtkTreePath *
gnc_tree_model_commodity_get_path (GtkTreeModel *tree_model,
				   GtkTreeIter *iter)
{
	GncTreeModelCommodity *model = GNC_TREE_MODEL_COMMODITY (tree_model);
	GtkTreePath *path;

	g_return_val_if_fail (GNC_IS_TREE_MODEL_COMMODITY (model), NULL);
	g_return_val_if_fail (iter != NULL, NULL);
	g_return_val_if_fail (iter->user_data != NULL, NULL);
	g_return_val_if_fail (iter->stamp == model->stamp, NULL);
	
	if (model->priv->commodities == NULL)
		return NULL;

	path = gtk_tree_path_new ();

	gtk_tree_path_append_index (path, g_list_position (model->priv->commodities, iter->user_data));

	return path;
}

static void
gnc_tree_model_commodity_get_value (GtkTreeModel *tree_model,
				    GtkTreeIter *iter,
				    int column,
				    GValue *value)
{
	GncTreeModelCommodity *model = GNC_TREE_MODEL_COMMODITY (tree_model);
	gnc_commodity *commodity;

	g_return_if_fail (GNC_IS_TREE_MODEL_COMMODITY (model));
	g_return_if_fail (iter != NULL);
	g_return_if_fail (iter->user_data != NULL);
	g_return_if_fail (iter->stamp == model->stamp);

	commodity = (gnc_commodity *)((GList *) iter->user_data)->data;
	g_return_if_fail (commodity != NULL);

	switch (column) {
		case GNC_TREE_MODEL_COMMODITY_COL_MNEMONIC:
			g_value_init (value, G_TYPE_STRING);

			g_value_set_string (value, gnc_commodity_get_mnemonic (commodity));
			break;
		case GNC_TREE_MODEL_COMMODITY_COL_NAMESPACE:
			g_value_init (value, G_TYPE_STRING);

			g_value_set_string (value, gnc_commodity_get_namespace (commodity));
			break;
		case GNC_TREE_MODEL_COMMODITY_COL_FULLNAME:
			g_value_init (value, G_TYPE_STRING);

			g_value_set_string (value, gnc_commodity_get_fullname (commodity));
			break;
		case GNC_TREE_MODEL_COMMODITY_COL_PRINTNAME:
			g_value_init (value, G_TYPE_STRING);

			g_value_set_string (value, gnc_commodity_get_printname (commodity));
			break;
		case GNC_TREE_MODEL_COMMODITY_COL_EXCHANGE_CODE:
			g_value_init (value, G_TYPE_STRING);

			g_value_set_string (value, gnc_commodity_get_exchange_code (commodity));
			break;
		case GNC_TREE_MODEL_COMMODITY_COL_UNIQUE_NAME:
			g_value_init (value, G_TYPE_STRING);

			g_value_set_string (value, gnc_commodity_get_unique_name (commodity));
			break;
		case GNC_TREE_MODEL_COMMODITY_COL_FRACTION:
			g_value_init (value, G_TYPE_INT);

			g_value_set_int (value, gnc_commodity_get_fraction (commodity));
			break;
		case GNC_TREE_MODEL_COMMODITY_COL_MARK:
			g_value_init (value, G_TYPE_INT);

			g_value_set_int (value, gnc_commodity_get_mark (commodity));
			break;
		default:
			g_assert_not_reached ();
	}
}

static gboolean
gnc_tree_model_commodity_iter_next (GtkTreeModel *tree_model,
					  GtkTreeIter *iter)
{
	GncTreeModelCommodity *model = GNC_TREE_MODEL_COMMODITY (tree_model);

	g_return_val_if_fail (GNC_IS_TREE_MODEL_COMMODITY (model), FALSE);
	g_return_val_if_fail (iter != NULL, FALSE);
	g_return_val_if_fail (iter->user_data != NULL, FALSE);
	g_return_val_if_fail (iter->stamp == model->stamp, FALSE);

	if (g_list_next (iter->user_data) == NULL)
		return FALSE;

	iter->user_data = g_list_next (iter->user_data);

	return TRUE;
}

static gboolean
gnc_tree_model_commodity_iter_children (GtkTreeModel *tree_model,
					      GtkTreeIter *iter,
					      GtkTreeIter *parent)
{

	g_return_val_if_fail (GNC_IS_TREE_MODEL_COMMODITY (tree_model), FALSE);

	if (parent != NULL)
		return FALSE;

	iter->stamp = GNC_TREE_MODEL_COMMODITY (tree_model)->stamp;
	iter->user_data = GNC_TREE_MODEL_COMMODITY (tree_model)->priv->commodities;

	return TRUE;
}

static gboolean
gnc_tree_model_commodity_iter_has_child (GtkTreeModel *tree_model,
					       GtkTreeIter *iter)
{
	return FALSE;
}

static int
gnc_tree_model_commodity_iter_n_children (GtkTreeModel *tree_model,
						GtkTreeIter *iter)
{
	g_return_val_if_fail (GNC_IS_TREE_MODEL_COMMODITY (tree_model), -1);

	if (iter == NULL)
		return g_list_length (GNC_TREE_MODEL_COMMODITY (tree_model)->priv->commodities);
	
	g_return_val_if_fail (GNC_TREE_MODEL_COMMODITY (tree_model)->stamp == iter->stamp, -1);

	return 0;
}

static gboolean
gnc_tree_model_commodity_iter_nth_child (GtkTreeModel *tree_model,
					       GtkTreeIter *iter,
					       GtkTreeIter *parent,
					       int n)
{
	GncTreeModelCommodity *model;
	
	g_return_val_if_fail (GNC_IS_TREE_MODEL_COMMODITY (tree_model), FALSE);

	if (parent != NULL)
		return FALSE;

	model = GNC_TREE_MODEL_COMMODITY (tree_model);

	g_return_val_if_fail (n >= 0 && n < (int)g_list_length (model->priv->commodities), FALSE);

	iter->stamp = model->stamp;
	iter->user_data = g_list_nth (model->priv->commodities, n);

	if (iter->user_data == NULL) {
		iter->stamp = 0;
		return FALSE;
	}

	return TRUE;
}

static gboolean
gnc_tree_model_commodity_iter_parent (GtkTreeModel *tree_model,
					    GtkTreeIter *iter,
    					    GtkTreeIter *child)
{
	return FALSE;
}
