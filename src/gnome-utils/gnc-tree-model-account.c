/* 
 * gnc-tree-model-account.c -- GtkTreeModel implementation to display accounts in a GtkTreeView.
 * Copyright (C) 2003 Jan Arne Petersen
 * Author: Jan Arne Petersen <jpetersen@uni-bonn.de>
 */

#include "config.h"

#include "gnc-tree-model-account.h"

#include "gnc-component-manager.h"
#include "Account.h"
#include "Group.h"

#define TREE_MODEL_ACCOUNT_CM_CLASS "tree-model-account"

static void gnc_tree_model_account_class_init (GncTreeModelAccountClass *klass);
static void gnc_tree_model_account_init (GncTreeModelAccount *model);
static void gnc_tree_model_account_finalize (GObject *object);
static void gnc_tree_model_account_dispose (GObject *object);

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

static gpointer account_row_inserted (Account *account,
				      gpointer data);
static void gnc_tree_model_account_refresh_handler (GHashTable *changes,
						    gpointer data);
static void gnc_tree_model_account_refresh (GncTreeModelAccount *model);


struct GncTreeModelAccountPrivate
{
	AccountGroup *root;
	Account *toplevel;
	gint component_id;
};

static GObjectClass *parent_class = NULL;

GType
gnc_tree_model_account_get_type (void)
{
	static GType gnc_tree_model_account_type = 0;

	if (gnc_tree_model_account_type == 0) {
		static const GTypeInfo our_info = {
			sizeof (GncTreeModelAccountClass),
			NULL,
			NULL,
			(GClassInitFunc) gnc_tree_model_account_class_init,
			NULL,
			NULL,
			sizeof (GncTreeModelAccount),
			0,
			(GInstanceInitFunc) gnc_tree_model_account_init
		};
		
		static const GInterfaceInfo tree_model_info = {
			(GInterfaceInitFunc) gnc_tree_model_account_tree_model_init,
			NULL,
			NULL
		};

		gnc_tree_model_account_type = g_type_register_static (G_TYPE_OBJECT,
								      "GncTreeModelAccount",
								      &our_info, 0);
		
		g_type_add_interface_static (gnc_tree_model_account_type,
					     GTK_TYPE_TREE_MODEL,
					     &tree_model_info);
	}

	return gnc_tree_model_account_type;
}

static void
gnc_tree_model_account_class_init (GncTreeModelAccountClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	parent_class = g_type_class_peek_parent (klass);

	object_class->finalize = gnc_tree_model_account_finalize;
	object_class->dispose = gnc_tree_model_account_dispose;
}

static void
gnc_tree_model_account_init (GncTreeModelAccount *model)
{

	while (model->stamp == 0) {
		model->stamp = g_random_int ();
	}

	model->priv = g_new0 (GncTreeModelAccountPrivate, 1);
	model->priv->root = NULL;
	model->priv->toplevel = NULL;
}

static void
gnc_tree_model_account_finalize (GObject *object)
{
	GncTreeModelAccount *model;

	g_return_if_fail (object != NULL);
	g_return_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (object));

	model = GNC_TREE_MODEL_ACCOUNT (object);

	g_return_if_fail (model->priv != NULL);

	g_free (model->priv);

	G_OBJECT_CLASS (parent_class)->finalize (object);
}

static void
gnc_tree_model_account_dispose (GObject *object)
{
	GncTreeModelAccount *model;

	g_return_if_fail (object != NULL);
	g_return_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (object));

	model = GNC_TREE_MODEL_ACCOUNT (object);

	gnc_unregister_gui_component (model->priv->component_id);

	G_OBJECT_CLASS (parent_class)->dispose (object);
}

GtkTreeModel *
gnc_tree_model_account_new (AccountGroup *group)
{
	GncTreeModelAccount *model;
	
	model = g_object_new (GNC_TYPE_TREE_MODEL_ACCOUNT,
			      NULL);

	model->priv->root = group;

	model->priv->component_id = gnc_register_gui_component (TREE_MODEL_ACCOUNT_CM_CLASS,
								gnc_tree_model_account_refresh_handler,
	     							NULL,
	     							model);

	gnc_gui_component_watch_entity_type (model->priv->component_id,
					     GNC_ID_ACCOUNT,
      					     GNC_EVENT_MODIFY | GNC_EVENT_DESTROY);

	return GTK_TREE_MODEL (model);
}

void
gnc_tree_model_account_set_root (GncTreeModelAccount *model,
		                 AccountGroup *group)
{
	GtkTreePath *path;
	gint i;

	g_return_if_fail (model != NULL);
	g_return_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (model));

	if (model->priv->root != NULL) {
		path = gtk_tree_path_new_first ();
		if (model->priv->toplevel != NULL) {
			gtk_tree_path_append_index (path, 0);
		}
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
gnc_tree_model_account_get_account (GncTreeModelAccount *model,
				    GtkTreeIter *iter)
{
	g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (model), NULL);
	g_return_val_if_fail (iter != NULL, NULL);
	g_return_val_if_fail (iter->user_data != NULL, NULL);
	g_return_val_if_fail (iter->stamp == model->stamp, NULL);

	return (Account *) iter->user_data;
}

void
gnc_tree_model_account_set_toplevel (GncTreeModelAccount *model,
                                     Account *toplevel)
{
	GtkTreePath *path;
	gint i;
	GtkTreeIter iter;

	g_return_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (model));

	if (model->priv->toplevel != NULL) {
		path = gtk_tree_path_new_first ();
		gtk_tree_model_row_deleted (GTK_TREE_MODEL (model), path);
		gtk_tree_path_free (path);
	} else {
		path = gtk_tree_path_new_first ();
		for (i = 0; i < xaccGroupGetNumAccounts (model->priv->root); i++) {
			gtk_tree_model_row_deleted (GTK_TREE_MODEL (model), path);
		}
		gtk_tree_path_free (path);
	}

	model->priv->toplevel = toplevel;

	if (model->priv->toplevel != NULL) {
		path = gtk_tree_path_new_first ();
		gtk_tree_model_get_iter (GTK_TREE_MODEL (model), &iter, path);
		gtk_tree_model_row_inserted (GTK_TREE_MODEL (model), path, &iter);
		gtk_tree_path_free (path);
	}

	if (model->priv->root != NULL) {
		xaccGroupForEachAccount (model->priv->root, account_row_inserted, model, TRUE);
	}
}

Account *
gnc_tree_model_account_get_toplevel (GncTreeModelAccount *model)
{
	g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (model), NULL);

	return model->priv->toplevel;
}

void
gnc_tree_model_account_get_iter_from_account (GncTreeModelAccount *model,
					      Account *account,
					      GtkTreeIter *iter)
{
	AccountGroup *group;
	gint i;
	
	g_return_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (model));
	g_return_if_fail (account != NULL);

	iter->user_data = account;
	iter->stamp = model->stamp;

	if (account == model->priv->toplevel) {
		iter->user_data2 = NULL;
		iter->user_data3 = GINT_TO_POINTER (0);
		return;
	}

	group = xaccAccountGetParent (account);

	for (i = 0; i < xaccGroupGetNumAccounts (group); i++) {
		if (xaccGroupGetAccount (group, i) == account) {
			break;
		}
	}

	iter->user_data2 = group;
	iter->user_data3 = GINT_TO_POINTER (i);
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
	GncTreeModelAccount *model;
	Account *account = NULL;
	AccountGroup *group = NULL, *children;
	gint i = 0, *indices;
	GtkTreePath *path_copy;

	g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (tree_model), FALSE);

	model = GNC_TREE_MODEL_ACCOUNT (tree_model);

	path_copy = gtk_tree_path_copy (path);

	if (model->priv->toplevel != NULL) {
		if (gtk_tree_path_get_depth (path) > 1) {
			i++;
		} else {

			iter->user_data = model->priv->toplevel;
			iter->user_data2 = NULL;
			iter->user_data3 = GINT_TO_POINTER (0);
			iter->stamp = model->stamp;

			return TRUE;
		}
	}

	if (model->priv->root == NULL)
		return FALSE;

	children = model->priv->root;

	indices = gtk_tree_path_get_indices (path);
	for (; i < gtk_tree_path_get_depth (path); i++) {
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
	GncTreeModelAccount *model = GNC_TREE_MODEL_ACCOUNT (tree_model);
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

	if (model->priv->toplevel != NULL) {
		if (account == model->priv->toplevel) {
			gtk_tree_path_append_index (path, 0);

			return path;
		}
	}

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

	if (model->priv->toplevel != NULL) {
		gtk_tree_path_prepend_index (path, 0);
	}

	return path;
}

static void
gnc_tree_model_account_get_value (GtkTreeModel *tree_model,
				  GtkTreeIter *iter,
				  int column,
				  GValue *value)
{
	GncTreeModelAccount *model = GNC_TREE_MODEL_ACCOUNT (tree_model);
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
	GncTreeModelAccount *model = GNC_TREE_MODEL_ACCOUNT (tree_model);
	Account *account;
	AccountGroup *group;
	gint i;

	g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (model), FALSE);
	g_return_val_if_fail (iter != NULL, FALSE);
	g_return_val_if_fail (iter->user_data != NULL, FALSE);
	g_return_val_if_fail (iter->stamp == model->stamp, FALSE);

	if (iter->user_data == model->priv->toplevel) {
		iter->stamp = 0;

		return FALSE;
	}

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
	GncTreeModelAccount *model;
	Account *account;
	AccountGroup *group;

	g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (tree_model), FALSE);

	model = GNC_TREE_MODEL_ACCOUNT (tree_model);

	if (model->priv->toplevel != NULL) {
		if (parent == NULL) {
			iter->user_data = model->priv->toplevel;
			iter->user_data2 = NULL;
			iter->user_data3 = GINT_TO_POINTER (0);
			iter->stamp = model->stamp;

			return TRUE;
		} else if (parent->user_data == model->priv->toplevel) {
			parent = NULL;
		}
	}

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
	GncTreeModelAccount *model;
	AccountGroup *group;

	g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (tree_model), FALSE);

	model = GNC_TREE_MODEL_ACCOUNT (tree_model);

	g_return_val_if_fail (iter != NULL, FALSE);
	g_return_val_if_fail (iter->user_data != NULL, FALSE);
	g_return_val_if_fail (iter->stamp == model->stamp, FALSE);

	if (iter->user_data == model->priv->toplevel) {
		group = model->priv->root;
	} else {
		group = xaccAccountGetChildren ((Account *) iter->user_data);
	}

	if (group == NULL || xaccGroupGetNumAccounts (group) == 0) {
		return FALSE;
	}

	return TRUE;
}

static int
gnc_tree_model_account_iter_n_children (GtkTreeModel *tree_model,
					GtkTreeIter *iter)
{
	GncTreeModelAccount *model;
	AccountGroup *group;

	g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (tree_model), FALSE);

	model = GNC_TREE_MODEL_ACCOUNT (tree_model);

	if (iter == NULL) {
		if (model->priv->toplevel != NULL) {
			return 1;
		} else {
			return xaccGroupGetNumAccounts (model->priv->root);
		}
	}

	g_return_val_if_fail (iter != NULL, FALSE);
	g_return_val_if_fail (iter->user_data != NULL, FALSE);
	g_return_val_if_fail (iter->stamp == model->stamp, FALSE);

	if (model->priv->toplevel == iter->user_data) {
		group = model->priv->root;
	} else {
		group = xaccAccountGetChildren ((Account *) iter->user_data);
	}

	return xaccGroupGetNumAccounts (group);
}

static gboolean
gnc_tree_model_account_iter_nth_child (GtkTreeModel *tree_model,
				       GtkTreeIter *iter,
				       GtkTreeIter *parent,
				       int n)
{
	GncTreeModelAccount *model;
	Account *account;
	AccountGroup *group;

	g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (tree_model), FALSE);

	model = GNC_TREE_MODEL_ACCOUNT (tree_model);

	if (parent == NULL) {
		if (model->priv->toplevel != NULL) {
			if (n > 0) {
				iter->stamp = 0;

				return FALSE;
			} else {
				iter->user_data = model->priv->toplevel;
				iter->user_data2 = NULL;
				iter->user_data3 = GINT_TO_POINTER (0);
				iter->stamp = model->stamp;

				return TRUE;
			}
		}

		account = xaccGroupGetAccount (model->priv->root, n);

		if (account == NULL) {
			iter->stamp = 0;
			
			return FALSE;
		}

		iter->user_data = account;
		iter->user_data2 = model->priv->root;
		iter->user_data3 = GINT_TO_POINTER (n);
		iter->stamp = model->stamp;

		return TRUE;
	}

	g_return_val_if_fail (parent->user_data != NULL, FALSE);
	g_return_val_if_fail (parent->stamp == model->stamp, FALSE);

	if (model->priv->toplevel == parent->user_data) {
		group = model->priv->root;
	} else {
		group = xaccAccountGetChildren ((Account *) parent->user_data);
	}

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
	GncTreeModelAccount *model;
	Account *account;
	AccountGroup *group;
	gint i;

	g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (tree_model), FALSE);

	model = GNC_TREE_MODEL_ACCOUNT (tree_model);

	g_return_val_if_fail (child != NULL, FALSE);
	g_return_val_if_fail (child->user_data != NULL, FALSE);
	g_return_val_if_fail (child->stamp == model->stamp, FALSE);

	account = (Account *) child->user_data;

	if (account == model->priv->toplevel) {
		iter->stamp = 0;

		return FALSE;
	}

	account = xaccAccountGetParentAccount (account);
	group = xaccAccountGetParent (account);

	if (account == NULL || group == NULL) {
		if (model->priv->toplevel != NULL) {
			iter->user_data = model->priv->toplevel;
			iter->user_data2 = NULL;
			iter->user_data3 = GINT_TO_POINTER (0);
			iter->stamp = model->stamp;

			return TRUE;
		} else {
			iter->stamp = 0;

			return FALSE;
		}
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

	if (model->priv->toplevel != NULL) {
		iter->user_data = model->priv->toplevel;
		iter->user_data2 = NULL;
		iter->user_data3 = GINT_TO_POINTER (0);
		iter->stamp = model->stamp;

		return TRUE;
	}
	iter->stamp = 0;

	return FALSE;
}

static gpointer
account_row_inserted (Account *account,
		      gpointer data)
{
	GtkTreePath *path;
	GtkTreeIter iter;

	gnc_tree_model_account_get_iter_from_account (GNC_TREE_MODEL_ACCOUNT (data), account, &iter);

	path = gtk_tree_model_get_path (GTK_TREE_MODEL (data), &iter);

	gtk_tree_model_row_inserted (GTK_TREE_MODEL (data), path, &iter);

	gtk_tree_path_free (path);

	return NULL;
}

static void
gnc_tree_model_account_refresh_handler (GHashTable *changes, gpointer user_data)
{
	g_return_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (user_data));

	gnc_tree_model_account_refresh (GNC_TREE_MODEL_ACCOUNT (user_data));
}

static void
gnc_tree_model_account_refresh (GncTreeModelAccount *model)
{
	GtkTreePath *path;
	gint i;
	
	if (model->priv->root == NULL) {
		return;
	}

	path = gtk_tree_path_new_first ();
	if (model->priv->toplevel != NULL) {
		gtk_tree_path_append_index (path, 0);
	}
	for (i = 0; i < xaccGroupGetNumAccounts (model->priv->root); i++) {
		gtk_tree_model_row_deleted (GTK_TREE_MODEL (model), path);
	}
	gtk_tree_path_free (path);

	xaccGroupForEachAccount (model->priv->root, account_row_inserted, model, TRUE);
}
