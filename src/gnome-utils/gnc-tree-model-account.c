/* 
 * gnc-tree-model-account.c -- GtkTreeModel implementation to display accounts in a GtkTreeView.
 * Copyright (C) 2003 Jan Arne Petersen
 * Author: Jan Arne Petersen <jpetersen@uni-bonn.de>
 */

#include "config.h"

#include "gnc-tree-model-account.h"
#include "Account.h"
#include "Group.h"

static void gnc_tree_model_account_class_init (GNCTreeModelAccountClass *klass);
static void gnc_tree_model_account_init (GNCTreeModelAccount *model);
static void gnc_tree_model_account_finalize (GObject *object);

static void gnc_tree_model_account_tree_model_init (GtkTreeModelIface *iface);
static guint gnc_tree_model_account_get_flags (GtkTreeModel *tree_model);
static int gnc_tree_model_account_get_n_columns (GtkTreeModel *tree_model);
static GType gnc_tree_model_account_get_column_type (GtkTreeModel *tree_model,
						     int index);
static gboolean gnc_tree_model_account_get_iter (GtkTreeModel *tree_model,
						 GtkTreeIter *iter,
						 GtkTreePath *path);
static GtkTreePath *gnc_tree_model_account_get_path (GtkTreeModel *tree_model,
						     GtkTreeIter *iter);
static void gnc_tree_model_account_get_value (GtkTreeModel *tree_model,
					      GtkTreeIter *iter,
					      int column,
					      GValue *value);
static gboolean	gnc_tree_model_account_iter_next (GtkTreeModel *tree_model,
						  GtkTreeIter *iter);
static gboolean	gnc_tree_model_account_iter_children (GtkTreeModel *tree_model,
						      GtkTreeIter *iter,
						      GtkTreeIter *parent);
static gboolean	gnc_tree_model_account_iter_has_child (GtkTreeModel *tree_model,
						       GtkTreeIter *iter);
static int gnc_tree_model_account_iter_n_children (GtkTreeModel *tree_model,
						   GtkTreeIter *iter);
static gboolean	gnc_tree_model_account_iter_nth_child (GtkTreeModel *tree_model,
						       GtkTreeIter *iter,
						       GtkTreeIter *parent,
						       int n);
static gboolean	gnc_tree_model_account_iter_parent (GtkTreeModel *tree_model,
						    GtkTreeIter *iter,
    						    GtkTreeIter *child);

/*static gpointer account_row_deleted (Account *account,
				     gpointer data);*/
static gpointer account_row_inserted (Account *account,
				      gpointer data);


struct GNCTreeModelAccountPrivate
{
	AccountGroup *root;
};

static GObjectClass *parent_class = NULL;

GType
gnc_tree_model_account_get_type (void)
{
	static GType gnc_tree_model_account_type = 0;

	if (gnc_tree_model_account_type == 0) {
		static const GTypeInfo our_info = {
			sizeof (GNCTreeModelAccountClass),
			NULL,
			NULL,
			(GClassInitFunc) gnc_tree_model_account_class_init,
			NULL,
			NULL,
			sizeof (GNCTreeModelAccount),
			0,
			(GInstanceInitFunc) gnc_tree_model_account_init
		};
		
		static const GInterfaceInfo tree_model_info = {
			(GInterfaceInitFunc) gnc_tree_model_account_tree_model_init,
			NULL,
			NULL
		};

		gnc_tree_model_account_type = g_type_register_static (G_TYPE_OBJECT,
								      "GNCTreeModelAccount",
								      &our_info, 0);
		
		g_type_add_interface_static (gnc_tree_model_account_type,
					     GTK_TYPE_TREE_MODEL,
					     &tree_model_info);
	}

	return gnc_tree_model_account_type;
}

static void
gnc_tree_model_account_class_init (GNCTreeModelAccountClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	parent_class = g_type_class_peek_parent (klass);

	object_class->finalize = gnc_tree_model_account_finalize;
}

static void
gnc_tree_model_account_init (GNCTreeModelAccount *model)
{
	while (model->stamp == 0) {
		model->stamp = g_random_int ();
	}

	model->priv = g_new0 (GNCTreeModelAccountPrivate, 1);
}

static void
gnc_tree_model_account_finalize (GObject *object)
{
	GNCTreeModelAccount *model;

	g_return_if_fail (object != NULL);
	g_return_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (object));

	model = GNC_TREE_MODEL_ACCOUNT (object);

	g_return_if_fail (model->priv != NULL);

	g_free (model->priv);

	G_OBJECT_CLASS (parent_class)->finalize (object);
}

GNCTreeModelAccount *
gnc_tree_model_account_new (AccountGroup *group)
{
	GNCTreeModelAccount *model;

	model = g_object_new (GNC_TYPE_TREE_MODEL_ACCOUNT,
			      NULL);

	model->priv->root = group;

	return model;
}

void
gnc_tree_model_account_set_root (GNCTreeModelAccount *model,
		                 AccountGroup *group)
{
	GtkTreePath *path;
	gint i;

	g_return_if_fail (model != NULL);
	g_return_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (model));

	if (model->priv->root != NULL) {
		path = gtk_tree_path_new_first ();
		for (i = 0; i < xaccGroupGetNumAccounts (model->priv->root); i++) {
			gtk_tree_model_row_deleted (GTK_TREE_MODEL (model), path);
		}
		gtk_tree_path_free (path);
	}

	model->priv->root = group;

	if (model->priv->root != NULL) {
		xaccGroupForEachAccount (model->priv->root, account_row_inserted, model, TRUE);
	}
}

Account *
gnc_tree_model_account_get_account (GNCTreeModelAccount *model,
				    GtkTreeIter *iter)
{
	g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (model), NULL);
	g_return_val_if_fail (iter != NULL, NULL);
	g_return_val_if_fail (iter->user_data != NULL, NULL);
	g_return_val_if_fail (iter->stamp == model->stamp, NULL);

	return (Account *) iter->user_data;
}

static void
gnc_tree_model_account_tree_model_init (GtkTreeModelIface *iface)
{
	iface->get_flags       = gnc_tree_model_account_get_flags;
	iface->get_n_columns   = gnc_tree_model_account_get_n_columns;
	iface->get_column_type = gnc_tree_model_account_get_column_type;
	iface->get_iter        = gnc_tree_model_account_get_iter;
	iface->get_path        = gnc_tree_model_account_get_path;
	iface->get_value       = gnc_tree_model_account_get_value;
	iface->iter_next       = gnc_tree_model_account_iter_next;
	iface->iter_children   = gnc_tree_model_account_iter_children;
	iface->iter_has_child  = gnc_tree_model_account_iter_has_child;
	iface->iter_n_children = gnc_tree_model_account_iter_n_children;
	iface->iter_nth_child  = gnc_tree_model_account_iter_nth_child;
	iface->iter_parent     = gnc_tree_model_account_iter_parent;
}

static guint
gnc_tree_model_account_get_flags (GtkTreeModel *tree_model)
{
	return 0;
}

static int
gnc_tree_model_account_get_n_columns (GtkTreeModel *tree_model)
{
	return GNC_TREE_MODEL_ACCOUNT_NUM_COLUMNS;
}

static GType
gnc_tree_model_account_get_column_type (GtkTreeModel *tree_model,
					int index)
{
	g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (tree_model), G_TYPE_INVALID);
	g_return_val_if_fail ((index < GNC_TREE_MODEL_ACCOUNT_NUM_COLUMNS) && (index >= 0), G_TYPE_INVALID);

	switch (index) {
		case GNC_TREE_MODEL_ACCOUNT_COL_TYPE:
		case GNC_TREE_MODEL_ACCOUNT_COL_NAME:
		case GNC_TREE_MODEL_ACCOUNT_COL_CODE:
		case GNC_TREE_MODEL_ACCOUNT_COL_DESCRIPTION:
		case GNC_TREE_MODEL_ACCOUNT_COL_NOTES:
		case GNC_TREE_MODEL_ACCOUNT_COL_LASTNUM:
			return G_TYPE_STRING;
		case GNC_TREE_MODEL_ACCOUNT_COL_PLACEHOLDER:
			return G_TYPE_BOOLEAN;
		default:
			g_assert_not_reached ();
			return G_TYPE_INVALID;
	}
}

static gboolean
gnc_tree_model_account_get_iter (GtkTreeModel *tree_model,
				 GtkTreeIter *iter,
				 GtkTreePath *path)
{
	GNCTreeModelAccount *model;
	Account *account = NULL;
	AccountGroup *group = NULL, *children;
	gint i = 0, *indices;

	g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (tree_model), FALSE);

	model = GNC_TREE_MODEL_ACCOUNT (tree_model);
	
	if (model->priv->root == NULL)
		return FALSE;

	children = model->priv->root;

	indices = gtk_tree_path_get_indices (path);
	for (i = 0; i < gtk_tree_path_get_depth (path); i++) {
		group = children;
		if (indices[i] >= xaccGroupGetNumAccounts (group)) {
			iter->stamp = 0;

			return FALSE;
		}

		account = xaccGroupGetAccount (group, indices[i]);
		children = xaccAccountGetChildren (account);
	}

	if (account == NULL || group == NULL) {
		iter->stamp = 0;

		return FALSE;
	}

	iter->stamp = model->stamp;
	iter->user_data = account;
	iter->user_data2 = group;
	iter->user_data3 = GINT_TO_POINTER (indices[i - 1]);

	return TRUE;
}

static GtkTreePath *
gnc_tree_model_account_get_path (GtkTreeModel *tree_model,
				 GtkTreeIter *iter)
{
	GNCTreeModelAccount *model = GNC_TREE_MODEL_ACCOUNT (tree_model);
	Account *account;
	AccountGroup *group;
	GtkTreePath *path;
	gint i;
	gboolean found, finished = FALSE;

	g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (model), NULL);
	g_return_val_if_fail (iter != NULL, NULL);
	g_return_val_if_fail (iter->user_data != NULL, NULL);
	g_return_val_if_fail (iter->stamp == model->stamp, NULL);
	
	if (model->priv->root == NULL)
		return NULL;

	account = (Account *) iter->user_data;
	group = (AccountGroup *) iter->user_data2;

	path = gtk_tree_path_new ();

	do {
		found = FALSE;
		for (i = 0; i < xaccGroupGetNumAccounts (group); i++) {
			if (xaccGroupGetAccount (group, i) == account) {
				found = TRUE;
				if (group == model->priv->root)
					finished = TRUE;
				break;
			}
		}

		if (!found) {
			gtk_tree_path_free (path);
			return NULL;
		}

		gtk_tree_path_prepend_index (path, i);

		account = xaccAccountGetParentAccount (account);
		group = xaccAccountGetParent (account);
	} while (!finished);

	return path;
}

static void
gnc_tree_model_account_get_value (GtkTreeModel *tree_model,
				  GtkTreeIter *iter,
				  int column,
				  GValue *value)
{
	GNCTreeModelAccount *model = GNC_TREE_MODEL_ACCOUNT (tree_model);
	Account *account;

	g_return_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (model));
	g_return_if_fail (iter != NULL);
	g_return_if_fail (iter->user_data != NULL);
	g_return_if_fail (iter->stamp == model->stamp);

	account = (Account *) iter->user_data;

	switch (column) {
		case GNC_TREE_MODEL_ACCOUNT_COL_TYPE:
			g_value_init (value, G_TYPE_STRING);

			g_value_set_string (value, 
					    xaccAccountGetTypeStr (xaccAccountGetType (account)));
			break;
		case GNC_TREE_MODEL_ACCOUNT_COL_NAME:
			g_value_init (value, G_TYPE_STRING);

			g_value_set_string (value, xaccAccountGetName (account));
			break;
		case GNC_TREE_MODEL_ACCOUNT_COL_CODE:
			g_value_init (value, G_TYPE_STRING);

			g_value_set_string (value, xaccAccountGetCode (account));
			break;
		case GNC_TREE_MODEL_ACCOUNT_COL_DESCRIPTION:
			g_value_init (value, G_TYPE_STRING);

			g_value_set_string (value, xaccAccountGetDescription (account));
			break;
		case GNC_TREE_MODEL_ACCOUNT_COL_NOTES:
			g_value_init (value, G_TYPE_STRING);

			g_value_set_string (value, xaccAccountGetNotes (account));
			break;
		case GNC_TREE_MODEL_ACCOUNT_COL_LASTNUM:
			g_value_init (value, G_TYPE_STRING);

			g_value_set_string (value, xaccAccountGetLastNum (account));
			break;
		case GNC_TREE_MODEL_ACCOUNT_COL_PLACEHOLDER:
			g_value_init (value, G_TYPE_BOOLEAN);

			g_value_set_boolean (value, xaccAccountGetPlaceholder (account));
			break;
		default:
			g_assert_not_reached ();
	}
}

static gboolean
gnc_tree_model_account_iter_next (GtkTreeModel *tree_model,
				  GtkTreeIter *iter)
{
	GNCTreeModelAccount *model = GNC_TREE_MODEL_ACCOUNT (tree_model);
	Account *account;
	AccountGroup *group;
	gint i;

	g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (model), FALSE);
	g_return_val_if_fail (iter != NULL, FALSE);
	g_return_val_if_fail (iter->user_data != NULL, FALSE);
	g_return_val_if_fail (iter->user_data2 != NULL, FALSE);
	g_return_val_if_fail (iter->stamp == model->stamp, FALSE);

	group = (AccountGroup *) iter->user_data2;
	i = GPOINTER_TO_INT (iter->user_data3);

	if (i > xaccGroupGetNumAccounts (group) - 2) {
		iter->stamp = 0;

		return FALSE;
	}

	account = xaccGroupGetAccount (group, i + 1);

	if (account == NULL) {
		iter->stamp = 0;

		return FALSE;
	}

	iter->user_data = account;
	iter->user_data2 = group;
	iter->user_data3 = GINT_TO_POINTER (i + 1);

	return TRUE;
}

static gboolean
gnc_tree_model_account_iter_children (GtkTreeModel *tree_model,
				      GtkTreeIter *iter,
				      GtkTreeIter *parent)
{
	GNCTreeModelAccount *model;
	Account *account;
	AccountGroup *group;

	g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (tree_model), FALSE);

	model = GNC_TREE_MODEL_ACCOUNT (tree_model);

	if (model->priv->root == NULL || xaccGroupGetNumAccounts (model->priv->root) == 0) {
		iter->stamp = 0;

		return FALSE;
	}

	if (parent == NULL) {
		account = xaccGroupGetAccount (model->priv->root, 0);
		
		if (account == NULL) {
			iter->stamp = 0;

			return FALSE;
		}

		iter->user_data = account;
		iter->user_data2 = model->priv->root;
		iter->user_data3 = GINT_TO_POINTER (0);
		iter->stamp = model->stamp;

		return TRUE;	
	}

	g_return_val_if_fail (parent != NULL, FALSE);
	g_return_val_if_fail (parent->user_data != NULL, FALSE);
	g_return_val_if_fail (parent->stamp == model->stamp, FALSE);	

	group = xaccAccountGetChildren ((Account *) parent->user_data);

	if (group == NULL || xaccGroupGetNumAccounts (group) == 0) {
		iter->stamp = 0;

		return FALSE;
	}

	account = xaccGroupGetAccount (group, 0);
	
	if (account == NULL) {
		iter->stamp = 0;

		return FALSE;
	}

	iter->user_data = account;
	iter->user_data2 = group;
	iter->user_data3 = GINT_TO_POINTER (0);
	iter->stamp = model->stamp;

	return TRUE;
}

static gboolean
gnc_tree_model_account_iter_has_child (GtkTreeModel *tree_model,
				       GtkTreeIter *iter)
{
	GNCTreeModelAccount *model;
	AccountGroup *group;

	g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (tree_model), FALSE);

	model = GNC_TREE_MODEL_ACCOUNT (tree_model);

	g_return_val_if_fail (iter != NULL, FALSE);
	g_return_val_if_fail (iter->user_data != NULL, FALSE);
	g_return_val_if_fail (iter->stamp == model->stamp, FALSE);

	group = xaccAccountGetChildren ((Account *) iter->user_data);

	if (group == NULL || xaccGroupGetNumAccounts (group) == 0) {
		return FALSE;
	}

	return TRUE;
}

static int
gnc_tree_model_account_iter_n_children (GtkTreeModel *tree_model,
					GtkTreeIter *iter)
{
	GNCTreeModelAccount *model;
	AccountGroup *group;

	g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (tree_model), FALSE);

	model = GNC_TREE_MODEL_ACCOUNT (tree_model);

	if (iter == NULL) {
		return xaccGroupGetNumAccounts (model->priv->root);
	}

	g_return_val_if_fail (iter != NULL, FALSE);
	g_return_val_if_fail (iter->user_data != NULL, FALSE);
	g_return_val_if_fail (iter->stamp == model->stamp, FALSE);

	group = xaccAccountGetChildren ((Account *) iter->user_data);

	return xaccGroupGetNumAccounts (group);
}

static gboolean
gnc_tree_model_account_iter_nth_child (GtkTreeModel *tree_model,
				       GtkTreeIter *iter,
				       GtkTreeIter *parent,
				       int n)
{
	GNCTreeModelAccount *model;
	Account *account;
	AccountGroup *group;

	g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (tree_model), FALSE);

	model = GNC_TREE_MODEL_ACCOUNT (tree_model);

	if (parent == NULL) {
		account = xaccGroupGetAccount (model->priv->root, n);

		if (account == NULL) {
			iter->stamp = 0;
			
			return FALSE;
		}

		iter->user_data = xaccGroupGetAccount (model->priv->root, n);
		iter->user_data2 = model->priv->root;
		iter->user_data3 = GINT_TO_POINTER (n);
		iter->stamp = model->stamp;

		return TRUE;
	}

	g_return_val_if_fail (parent->user_data != NULL, FALSE);
	g_return_val_if_fail (parent->stamp == model->stamp, FALSE);

	group = xaccAccountGetChildren ((Account *) parent->user_data);

	if (group == NULL || xaccGroupGetNumAccounts (group) <= n) {
		iter->stamp = 0;
	
		return FALSE;
	}

	account = xaccGroupGetAccount (group, n);
	
	if (account == NULL) {
		iter->stamp = 0;

		return FALSE;
	}

	iter->user_data = account;
	iter->user_data2 = group;
	iter->user_data3 = GINT_TO_POINTER (n);
	iter->stamp = model->stamp;

	return TRUE;
}

static gboolean
gnc_tree_model_account_iter_parent (GtkTreeModel *tree_model,
				    GtkTreeIter *iter,
    				    GtkTreeIter *child)
{
	GNCTreeModelAccount *model;
	Account *account;
	AccountGroup *group;
	gint i;

	g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (tree_model), FALSE);

	model = GNC_TREE_MODEL_ACCOUNT (tree_model);

	g_return_val_if_fail (child != NULL, FALSE);
	g_return_val_if_fail (child->user_data != NULL, FALSE);
	g_return_val_if_fail (child->stamp == model->stamp, FALSE);

	account = (Account *) child->user_data;

	account = xaccAccountGetParentAccount (account);
	group = xaccAccountGetParent (account);

	if (account == NULL || group == NULL) {
		iter->stamp = 0;

		return FALSE;
	}

	for (i = 0; i < xaccGroupGetNumAccounts (group); i++) {
		if (xaccGroupGetAccount (group, i) == account) {
			iter->user_data = account;
			iter->user_data2 = group;
			iter->user_data3 = GINT_TO_POINTER (i);
			iter->stamp = model->stamp;

			return TRUE;	
		}
	}

	iter->stamp = 0;

	return FALSE;
}

static void
iter_from_real_account (GNCTreeModelAccount *model,
			Account *account,
			AccountGroup *group,
		       	GtkTreeIter *iter)
{
	gint i;
	
	for (i = 0; i < xaccGroupGetNumAccounts (group); i++) {
		if (xaccGroupGetAccount (group, i) == account) {
			break;
		}
	}

	iter->user_data = account;
	iter->user_data2 = group;
	iter->user_data3 = GINT_TO_POINTER (i);
	iter->stamp = model->stamp;
}

/*
static gpointer
account_row_deleted (Account *account,
		     gpointer data)
{
	AccountGroup *group;
	GtkTreePath *path;
	GtkTreeIter iter;

	group = xaccAccountGetParent (account);

	iter_from_real_account (GNC_TREE_MODEL_ACCOUNT (data), account, group, &iter);

	path = gtk_tree_model_get_path (GTK_TREE_MODEL (data), &iter);

	gtk_tree_model_row_deleted (GTK_TREE_MODEL (data), path);

	gtk_tree_path_free (path);

	return NULL;
}
*/
static gpointer
account_row_inserted (Account *account,
		      gpointer data)
{
	AccountGroup *group;
	GtkTreePath *path;
	GtkTreeIter iter;

	group = xaccAccountGetParent (account);

	iter_from_real_account (GNC_TREE_MODEL_ACCOUNT (data), account, group, &iter);

	path = gtk_tree_model_get_path (GTK_TREE_MODEL (data), &iter);

	gtk_tree_model_row_inserted (GTK_TREE_MODEL (data), path, &iter);

	gtk_tree_path_free (path);

	return NULL;
}
