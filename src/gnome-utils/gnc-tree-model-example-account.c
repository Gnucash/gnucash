/* 
 * gnc-tree-model-example-account.c -- GtkTreeModel implementation to
 *	display example accounts in a GtkTreeView.
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

#include "config.h"

#include "qofbook.h"
#include "gnc-tree-model-example-account.h"
#include "io-example-account.h"
#include "Group.h"

static void gnc_tree_model_example_account_class_init (GncTreeModelExampleAccountClass *klass);
static void gnc_tree_model_example_account_init (GncTreeModelExampleAccount *model);
static void gnc_tree_model_example_account_finalize (GObject *object);

static void gnc_tree_model_example_account_tree_model_init (GtkTreeModelIface *iface);
static guint gnc_tree_model_example_account_get_flags (GtkTreeModel *tree_model);
static int gnc_tree_model_example_account_get_n_columns (GtkTreeModel *tree_model);
static GType gnc_tree_model_example_account_get_column_type (GtkTreeModel *tree_model,
							     int index);
static gboolean gnc_tree_model_example_account_get_iter (GtkTreeModel *tree_model,
							 GtkTreeIter *iter,
							 GtkTreePath *path);
static GtkTreePath *gnc_tree_model_example_account_get_path (GtkTreeModel *tree_model,
							     GtkTreeIter *iter);
static void gnc_tree_model_example_account_get_value (GtkTreeModel *tree_model,
						      GtkTreeIter *iter,
						      int column,
						      GValue *value);
static gboolean	gnc_tree_model_example_account_iter_next (GtkTreeModel *tree_model,
							  GtkTreeIter *iter);
static gboolean	gnc_tree_model_example_account_iter_children (GtkTreeModel *tree_model,
							      GtkTreeIter *iter,
							      GtkTreeIter *parent);
static gboolean	gnc_tree_model_example_account_iter_has_child (GtkTreeModel *tree_model,
							       GtkTreeIter *iter);
static int gnc_tree_model_example_account_iter_n_children (GtkTreeModel *tree_model,
							   GtkTreeIter *iter);
static gboolean	gnc_tree_model_example_account_iter_nth_child (GtkTreeModel *tree_model,
							       GtkTreeIter *iter,
							       GtkTreeIter *parent,
							       int n);
static gboolean	gnc_tree_model_example_account_iter_parent (GtkTreeModel *tree_model,
							    GtkTreeIter *iter,
    							    GtkTreeIter *child);

struct GncTreeModelExampleAccountPrivate
{
	GSList *accounts;
};

static GObjectClass *parent_class = NULL;

GType
gnc_tree_model_example_account_get_type (void)
{
	static GType gnc_tree_model_example_account_type = 0;

	if (gnc_tree_model_example_account_type == 0) {
		static const GTypeInfo our_info = {
			sizeof (GncTreeModelExampleAccountClass),
			NULL,
			NULL,
			(GClassInitFunc) gnc_tree_model_example_account_class_init,
			NULL,
			NULL,
			sizeof (GncTreeModelExampleAccount),
			0,
			(GInstanceInitFunc) gnc_tree_model_example_account_init
		};
		
		static const GInterfaceInfo tree_model_info = {
			(GInterfaceInitFunc) gnc_tree_model_example_account_tree_model_init,
			NULL,
			NULL
		};

		gnc_tree_model_example_account_type = g_type_register_static (G_TYPE_OBJECT,
								      "GncTreeModelExampleAccount",
								      &our_info, 0);
		
		g_type_add_interface_static (gnc_tree_model_example_account_type,
					     GTK_TYPE_TREE_MODEL,
					     &tree_model_info);
	}

	return gnc_tree_model_example_account_type;
}

static void
gnc_tree_model_example_account_class_init (GncTreeModelExampleAccountClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	parent_class = g_type_class_peek_parent (klass);

	object_class->finalize = gnc_tree_model_example_account_finalize;
}

static void
gnc_tree_model_example_account_init (GncTreeModelExampleAccount *model)
{
	while (model->stamp == 0) {
		model->stamp = g_random_int ();
	}

	model->priv = g_new0 (GncTreeModelExampleAccountPrivate, 1);
}

static void
gnc_tree_model_example_account_finalize (GObject *object)
{
	GncTreeModelExampleAccount *model;

	g_return_if_fail (object != NULL);
	g_return_if_fail (GNC_IS_TREE_MODEL_EXAMPLE_ACCOUNT (object));

	model = GNC_TREE_MODEL_EXAMPLE_ACCOUNT (object);

	g_return_if_fail (model->priv != NULL);

	g_slist_free (model->priv->accounts);
	g_free (model->priv);

	G_OBJECT_CLASS (parent_class)->finalize (object);
}

GtkTreeModel *
gnc_tree_model_example_account_new (GSList *accounts)
{
	GncTreeModelExampleAccount *model;

	model = g_object_new (GNC_TYPE_TREE_MODEL_EXAMPLE_ACCOUNT,
			      NULL);

	model->priv->accounts = accounts;

	return GTK_TREE_MODEL (model);
}

void
gnc_tree_model_example_account_set_accounts (GncTreeModelExampleAccount *model,
                                             GSList *accounts)
{
	g_return_if_fail (model != NULL);

	g_slist_free (model->priv->accounts);

	model->priv->accounts = accounts;
}

GncExampleAccount *
gnc_tree_model_example_account_get_account  (GncTreeModelExampleAccount *model,
		                             GtkTreeIter *iter)
{
	g_return_val_if_fail (GNC_IS_TREE_MODEL_EXAMPLE_ACCOUNT (model), NULL);
	g_return_val_if_fail (iter != NULL, NULL);
	g_return_val_if_fail (iter->user_data != NULL, NULL);
	g_return_val_if_fail (iter->stamp == model->stamp, NULL);
	
	return (GncExampleAccount *) ((GSList *) iter->user_data)->data;
}

static void
gnc_tree_model_example_account_tree_model_init (GtkTreeModelIface *iface)
{
	iface->get_flags       = gnc_tree_model_example_account_get_flags;
	iface->get_n_columns   = gnc_tree_model_example_account_get_n_columns;
	iface->get_column_type = gnc_tree_model_example_account_get_column_type;
	iface->get_iter        = gnc_tree_model_example_account_get_iter;
	iface->get_path        = gnc_tree_model_example_account_get_path;
	iface->get_value       = gnc_tree_model_example_account_get_value;
	iface->iter_next       = gnc_tree_model_example_account_iter_next;
	iface->iter_children   = gnc_tree_model_example_account_iter_children;
	iface->iter_has_child  = gnc_tree_model_example_account_iter_has_child;
	iface->iter_n_children = gnc_tree_model_example_account_iter_n_children;
	iface->iter_nth_child  = gnc_tree_model_example_account_iter_nth_child;
	iface->iter_parent     = gnc_tree_model_example_account_iter_parent;
}

static guint
gnc_tree_model_example_account_get_flags (GtkTreeModel *tree_model)
{
	return GTK_TREE_MODEL_ITERS_PERSIST | GTK_TREE_MODEL_LIST_ONLY;
}

static int
gnc_tree_model_example_account_get_n_columns (GtkTreeModel *tree_model)
{
	return GNC_TREE_MODEL_EXAMPLE_ACCOUNT_NUM_COLUMNS;
}

static GType
gnc_tree_model_example_account_get_column_type (GtkTreeModel *tree_model,
					        int index)
{
	g_return_val_if_fail (GNC_IS_TREE_MODEL_EXAMPLE_ACCOUNT (tree_model), G_TYPE_INVALID);
	g_return_val_if_fail ((index < GNC_TREE_MODEL_EXAMPLE_ACCOUNT_NUM_COLUMNS) && (index >= 0), G_TYPE_INVALID);

	switch (index) {
		case GNC_TREE_MODEL_EXAMPLE_ACCOUNT_COL_TITLE:
		case GNC_TREE_MODEL_EXAMPLE_ACCOUNT_COL_SHORT_DESCRIPTION:
		case GNC_TREE_MODEL_EXAMPLE_ACCOUNT_COL_LONG_DESCRIPTION:
			return G_TYPE_STRING;
		default:
			g_assert_not_reached ();
			return G_TYPE_INVALID;
	}
}

static gboolean
gnc_tree_model_example_account_get_iter (GtkTreeModel *tree_model,
					 GtkTreeIter *iter,
					 GtkTreePath *path)
{
	GncTreeModelExampleAccount *model = GNC_TREE_MODEL_EXAMPLE_ACCOUNT (tree_model);
	guint i;

	g_return_val_if_fail (GNC_IS_TREE_MODEL_EXAMPLE_ACCOUNT (model), FALSE);
	g_return_val_if_fail (gtk_tree_path_get_depth (path) > 0, FALSE);
	
	if (model->priv->accounts == NULL)
		return FALSE;

	i = gtk_tree_path_get_indices (path)[0];

	g_return_val_if_fail (i >= 0 && i < g_slist_length (model->priv->accounts), FALSE);

	iter->stamp = model->stamp;
	iter->user_data = g_slist_nth (model->priv->accounts, i);

	if (iter->user_data == NULL) {
		iter->stamp = 0;
		return FALSE;
	}

	return TRUE;
}

static GtkTreePath *
gnc_tree_model_example_account_get_path (GtkTreeModel *tree_model,
					 GtkTreeIter *iter)
{
	GncTreeModelExampleAccount *model = GNC_TREE_MODEL_EXAMPLE_ACCOUNT (tree_model);
	GtkTreePath *path;

	g_return_val_if_fail (GNC_IS_TREE_MODEL_EXAMPLE_ACCOUNT (model), NULL);
	g_return_val_if_fail (iter != NULL, NULL);
	g_return_val_if_fail (iter->user_data != NULL, NULL);
	g_return_val_if_fail (iter->stamp == model->stamp, NULL);
	
	if (model->priv->accounts == NULL)
		return NULL;

	path = gtk_tree_path_new ();

	gtk_tree_path_append_index (path, g_slist_position (model->priv->accounts, iter->user_data));

	return path;
}

static void
gnc_tree_model_example_account_get_value (GtkTreeModel *tree_model,
					  GtkTreeIter *iter,
					  int column,
					  GValue *value)
{
	GncTreeModelExampleAccount *model = GNC_TREE_MODEL_EXAMPLE_ACCOUNT (tree_model);
	GncExampleAccount *account;

	g_return_if_fail (GNC_IS_TREE_MODEL_EXAMPLE_ACCOUNT (model));
	g_return_if_fail (iter != NULL);
	g_return_if_fail (iter->user_data != NULL);
	g_return_if_fail (iter->stamp == model->stamp);

	account = (GncExampleAccount *)((GSList *) iter->user_data)->data;

	switch (column) {
		case GNC_TREE_MODEL_EXAMPLE_ACCOUNT_COL_TITLE:
			g_value_init (value, G_TYPE_STRING);

			g_value_set_string (value, account->title);
			break;
		case GNC_TREE_MODEL_EXAMPLE_ACCOUNT_COL_SHORT_DESCRIPTION:
			g_value_init (value, G_TYPE_STRING);

			g_value_set_string (value, account->short_description);
			break;
		case GNC_TREE_MODEL_EXAMPLE_ACCOUNT_COL_LONG_DESCRIPTION:
			g_value_init (value, G_TYPE_STRING);

			g_value_set_string (value, account->long_description);
			break;
		default:
			g_assert_not_reached ();
	}
}

static gboolean
gnc_tree_model_example_account_iter_next (GtkTreeModel *tree_model,
					  GtkTreeIter *iter)
{
	GncTreeModelExampleAccount *model = GNC_TREE_MODEL_EXAMPLE_ACCOUNT (tree_model);

	g_return_val_if_fail (GNC_IS_TREE_MODEL_EXAMPLE_ACCOUNT (model), FALSE);
	g_return_val_if_fail (iter != NULL, FALSE);
	g_return_val_if_fail (iter->user_data != NULL, FALSE);
	g_return_val_if_fail (iter->stamp == model->stamp, FALSE);

	if (g_list_next (iter->user_data) == NULL)
		return FALSE;

	iter->user_data = g_slist_next (iter->user_data);

	return TRUE;
}

static gboolean
gnc_tree_model_example_account_iter_children (GtkTreeModel *tree_model,
					      GtkTreeIter *iter,
					      GtkTreeIter *parent)
{

	g_return_val_if_fail (GNC_IS_TREE_MODEL_EXAMPLE_ACCOUNT (tree_model), FALSE);

	if (parent != NULL)
		return FALSE;

	iter->stamp = GNC_TREE_MODEL_EXAMPLE_ACCOUNT (tree_model)->stamp;
	iter->user_data = GNC_TREE_MODEL_EXAMPLE_ACCOUNT (tree_model)->priv->accounts;

	return TRUE;
}

static gboolean
gnc_tree_model_example_account_iter_has_child (GtkTreeModel *tree_model,
					       GtkTreeIter *iter)
{
	return FALSE;
}

static int
gnc_tree_model_example_account_iter_n_children (GtkTreeModel *tree_model,
						GtkTreeIter *iter)
{
	g_return_val_if_fail (GNC_IS_TREE_MODEL_EXAMPLE_ACCOUNT (tree_model), -1);

	if (iter == NULL)
		return g_slist_length (GNC_TREE_MODEL_EXAMPLE_ACCOUNT (tree_model)->priv->accounts);
	
	g_return_val_if_fail (GNC_TREE_MODEL_EXAMPLE_ACCOUNT (tree_model)->stamp == iter->stamp, -1);

	return 0;
}

static gboolean
gnc_tree_model_example_account_iter_nth_child (GtkTreeModel *tree_model,
					       GtkTreeIter *iter,
					       GtkTreeIter *parent,
					       int n)
{
	GncTreeModelExampleAccount *model;
	
	g_return_val_if_fail (GNC_IS_TREE_MODEL_EXAMPLE_ACCOUNT (tree_model), FALSE);

	if (parent != NULL)
		return FALSE;

	model = GNC_TREE_MODEL_EXAMPLE_ACCOUNT (tree_model);

	g_return_val_if_fail (n >= 0 && n < (int)g_slist_length (model->priv->accounts), FALSE);

	iter->stamp = model->stamp;
	iter->user_data = g_slist_nth (model->priv->accounts, n);

	if (iter->user_data == NULL) {
		iter->stamp = 0;
		return FALSE;
	}

	return TRUE;
}

static gboolean
gnc_tree_model_example_account_iter_parent (GtkTreeModel *tree_model,
					    GtkTreeIter *iter,
    					    GtkTreeIter *child)
{
	return FALSE;
}
