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
#include "gnc-commodity.h"
#include "gnc-engine-util.h"
#include "gnc-ui-util.h"

#define TREE_MODEL_ACCOUNT_CM_CLASS "tree-model-account"

/** Static Globals *******************************************************/
static short module = MOD_GUI;
static GList *active_models = NULL;

/** Declarations *********************************************************/
static void gnc_tree_model_account_class_init (GncTreeModelAccountClass *klass);
static void gnc_tree_model_account_init (GncTreeModelAccount *model);
static void gnc_tree_model_account_finalize (GObject *object);
static void gnc_tree_model_account_destroy (GtkObject *object);

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
void gnc_tree_model_account_event_handler (GUID *entity, QofIdType type,
					   GNCEngineEventType event_type,
					   gpointer user_data);

struct GncTreeModelAccountPrivate
{
	AccountGroup *root;
	Account *toplevel;
	gint event_handler_id;
};

static GtkObjectClass *parent_class = NULL;

GType
gnc_tree_model_account_get_type (void)
{
	static GType gnc_tree_model_account_type = 0;

	if (gnc_tree_model_account_type == 0) {
		static const GTypeInfo our_info = {
			sizeof (GncTreeModelAccountClass), /* class_size */
			NULL,   			   /* base_init */
			NULL,				   /* base_finalize */
			(GClassInitFunc) gnc_tree_model_account_class_init,
			NULL,				   /* class_finalize */
			NULL,				   /* class_data */
			sizeof (GncTreeModelAccount),	   /* */
			0,				   /* n_preallocs */
			(GInstanceInitFunc) gnc_tree_model_account_init
		};
		
		static const GInterfaceInfo tree_model_info = {
			(GInterfaceInitFunc) gnc_tree_model_account_tree_model_init,
			NULL,
			NULL
		};

		gnc_tree_model_account_type = g_type_register_static (GTK_TYPE_OBJECT,
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
	GObjectClass *o_class;
	GtkObjectClass *object_class;

	parent_class = g_type_class_peek_parent (klass);

	o_class = G_OBJECT_CLASS (klass);
	object_class = GTK_OBJECT_CLASS (klass);

	/* GObject signals */
	o_class->finalize = gnc_tree_model_account_finalize;

	/* GtkObject signals */
	object_class->destroy = gnc_tree_model_account_destroy;
}

static void
gnc_tree_model_account_init (GncTreeModelAccount *model)
{
	ENTER("model %p", model);
	while (model->stamp == 0) {
		model->stamp = g_random_int ();
	}

	model->priv = g_new0 (GncTreeModelAccountPrivate, 1);
	model->priv->root = NULL;
	model->priv->toplevel = NULL;
	LEAVE(" ");
}

static void
gnc_tree_model_account_finalize (GObject *object)
{
	GncTreeModelAccount *model;

	ENTER("model %p", object);
	g_return_if_fail (object != NULL);
	g_return_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (object));

	model = GNC_TREE_MODEL_ACCOUNT (object);
	g_free (model->priv);

	if (G_OBJECT_CLASS (parent_class)->finalize)
	  (* G_OBJECT_CLASS (parent_class)->finalize) (object);
	LEAVE(" ");
}

static void
gnc_tree_model_account_destroy (GtkObject *object)
{
	GncTreeModelAccount *model;

	ENTER("model %p", object);
	g_return_if_fail (object != NULL);
	g_return_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (object));

	model = GNC_TREE_MODEL_ACCOUNT (object);

	active_models = g_list_remove (active_models, model);

	if (model->priv->event_handler_id) {
	  gnc_engine_unregister_event_handler (model->priv->event_handler_id);
	  model->priv->event_handler_id = 0;
	}

	if (GTK_OBJECT_CLASS (parent_class)->destroy)
	  (* GTK_OBJECT_CLASS (parent_class)->destroy) (object);
	LEAVE(" ");
}

GtkTreeModel *
gnc_tree_model_account_new (AccountGroup *group)
{
	GncTreeModelAccount *model;
	GncTreeModelAccountPrivate *priv;
	GList *item;
	
	ENTER("group %p", group);
	for (item = active_models; item; item = g_list_next(item)) {
		model = (GncTreeModelAccount *)item->data;
		if (model->priv->root == group) {
			LEAVE("returning existing model %p", model);
			return GTK_TREE_MODEL(model);
		}
	}

	model = g_object_new (GNC_TYPE_TREE_MODEL_ACCOUNT,
			      NULL);

	priv = model->priv;
	priv->root = group;

	priv->event_handler_id =
	  gnc_engine_register_event_handler (gnc_tree_model_account_event_handler, model);

	active_models = g_list_append (active_models, model);
	LEAVE("model %p", model);
	return GTK_TREE_MODEL (model);
}

void
gnc_tree_model_account_set_root (GncTreeModelAccount *model,
		                 AccountGroup *group)
{
	GtkTreePath *path;
	gint i;

	ENTER("model %p, group %p", model, group);
	g_return_if_fail (model != NULL);
	g_return_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (model));

	DEBUG("old root %p", model->priv->root);
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
	LEAVE("new root %p", model->priv->root);
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

	ENTER("model %p, toplevel %p", model, toplevel);
	g_return_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (model));

	DEBUG("old toplevel %p", model->priv->toplevel);
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
	LEAVE("new toplevel %p", model->priv->root);
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
	
	ENTER("model %p, account %p, iter %p", model, account, iter);
	g_return_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (model));
	g_return_if_fail (account != NULL);
	g_return_if_fail (iter != NULL);

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
	LEAVE("iter [stamp:%x data:%p, %p, %d]", iter->stamp, iter->user_data, iter->user_data2, GPOINTER_TO_INT(iter->user_data3));
}

GtkTreePath *
gnc_tree_model_account_get_path_from_account (GncTreeModelAccount *model,
					      Account *account)
{
	GtkTreeIter tree_iter;
	GtkTreePath *tree_path;

	ENTER("model %p, account %p", model, account);
	g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (model), NULL);
	g_return_val_if_fail (account != NULL, NULL);

	gnc_tree_model_account_get_iter_from_account (model, account, &tree_iter);
	tree_path = gtk_tree_model_get_path (GTK_TREE_MODEL(model), &tree_iter);
	if (tree_path) {
	  gchar *path_string = gtk_tree_path_to_string(tree_path);
	  LEAVE("path (2) %s", path_string);
	  g_free(path_string);
	}
	return tree_path;
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
		case GNC_TREE_MODEL_ACCOUNT_COL_NAME:
		case GNC_TREE_MODEL_ACCOUNT_COL_TYPE:
		case GNC_TREE_MODEL_ACCOUNT_COL_COMMODITY:
		case GNC_TREE_MODEL_ACCOUNT_COL_CODE:
		case GNC_TREE_MODEL_ACCOUNT_COL_DESCRIPTION:
		case GNC_TREE_MODEL_ACCOUNT_COL_PRESENT:
		case GNC_TREE_MODEL_ACCOUNT_COL_PRESENT_REPORT:
		case GNC_TREE_MODEL_ACCOUNT_COL_BALANCE:
		case GNC_TREE_MODEL_ACCOUNT_COL_BALANCE_REPORT:
		case GNC_TREE_MODEL_ACCOUNT_COL_CLEARED:
		case GNC_TREE_MODEL_ACCOUNT_COL_CLEARED_REPORT:
		case GNC_TREE_MODEL_ACCOUNT_COL_RECONCILED:
		case GNC_TREE_MODEL_ACCOUNT_COL_RECONCILED_REPORT:
		case GNC_TREE_MODEL_ACCOUNT_COL_FUTURE_MIN:
		case GNC_TREE_MODEL_ACCOUNT_COL_FUTURE_MIN_REPORT:
		case GNC_TREE_MODEL_ACCOUNT_COL_TOTAL:
		case GNC_TREE_MODEL_ACCOUNT_COL_TOTAL_REPORT:
		case GNC_TREE_MODEL_ACCOUNT_COL_NOTES:
		case GNC_TREE_MODEL_ACCOUNT_COL_TAX_INFO:
		case GNC_TREE_MODEL_ACCOUNT_COL_LASTNUM:

		case GNC_TREE_MODEL_ACCOUNT_COL_COLOR_PRESENT:
		case GNC_TREE_MODEL_ACCOUNT_COL_COLOR_BALANCE:
		case GNC_TREE_MODEL_ACCOUNT_COL_COLOR_CLEARED:
		case GNC_TREE_MODEL_ACCOUNT_COL_COLOR_RECONCILED:
		case GNC_TREE_MODEL_ACCOUNT_COL_COLOR_FUTURE_MIN:
		case GNC_TREE_MODEL_ACCOUNT_COL_COLOR_TOTAL:
			return G_TYPE_STRING;

		case GNC_TREE_MODEL_ACCOUNT_COL_PLACEHOLDER:
			return G_TYPE_BOOLEAN;

		case GNC_TREE_MODEL_ACCOUNT_COL_ALIGN_RIGHT:
			return G_TYPE_FLOAT;

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

	{
	  gchar *path_string = gtk_tree_path_to_string(path);
	  ENTER("model %p, iter %p, path %s", tree_model, iter, path_string);
	  g_free(path_string);
	}
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

			LEAVE("iter (1) [stamp:%x data:%p, %p, %d]", iter->stamp, iter->user_data, iter->user_data2, GPOINTER_TO_INT(iter->user_data3));
			return TRUE;
		}
	}

	if (model->priv->root == NULL) {
		LEAVE("failed (2)");
		return FALSE;
	}

	children = model->priv->root;

	indices = gtk_tree_path_get_indices (path);
	for (; i < gtk_tree_path_get_depth (path); i++) {
		group = children;
		if (indices[i] >= xaccGroupGetNumAccounts (group)) {
			iter->stamp = 0;

			LEAVE("failed (3)");
			return FALSE;
		}

		account = xaccGroupGetAccount (group, indices[i]);
		children = xaccAccountGetChildren (account);
	}

	if (account == NULL || group == NULL) {
		iter->stamp = 0;

		LEAVE("failed (4)");
		return FALSE;
	}

	iter->stamp = model->stamp;
	iter->user_data = account;
	iter->user_data2 = group;
	iter->user_data3 = GINT_TO_POINTER (indices[i - 1]);

	LEAVE("iter (5) [stamp:%x data:%p, %p, %d]", iter->stamp, iter->user_data, iter->user_data2, GPOINTER_TO_INT(iter->user_data3));
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

	ENTER("model %p, iter [stamp:%x data:%p, %p, %d]", model,
	      iter->stamp, iter->user_data, iter->user_data2, GPOINTER_TO_INT(iter->user_data3));
	g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (model), NULL);
	g_return_val_if_fail (iter != NULL, NULL);
	g_return_val_if_fail (iter->user_data != NULL, NULL);
	g_return_val_if_fail (iter->stamp == model->stamp, NULL);
	
	if (model->priv->root == NULL) {
		LEAVE("failed (1)");
		return NULL;
	}

	account = (Account *) iter->user_data;
	group = (AccountGroup *) iter->user_data2;

	path = gtk_tree_path_new ();

	if (model->priv->toplevel != NULL) {
		if (account == model->priv->toplevel) {
			gtk_tree_path_append_index (path, 0);

			{
			  gchar *path_string = gtk_tree_path_to_string(path);
			  LEAVE("path (2) %s", path_string);
			  g_free(path_string);
			}
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
			LEAVE("failed (3)");
			return NULL;
		}

		gtk_tree_path_prepend_index (path, i);

		account = xaccAccountGetParentAccount (account);
		group = xaccAccountGetParent (account);
	} while (!finished);

	if (model->priv->toplevel != NULL) {
		gtk_tree_path_prepend_index (path, 0);
	}

	{
	  gchar *path_string = gtk_tree_path_to_string(path);
	  LEAVE("path (4) %s", path_string);
	  g_free(path_string);
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
	gboolean negative; /* used to set "defecit style" aka red numbers */
	gchar *string;

	//ENTER("model %p, iter [stamp:%x data:%p, %p, %d], col %d", tree_model,
	//      iter->stamp, iter->user_data, iter->user_data2, GPOINTER_TO_INT(iter->user_data3), column);
	g_return_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (model));
	g_return_if_fail (iter != NULL);
	g_return_if_fail (iter->user_data != NULL);
	g_return_if_fail (iter->stamp == model->stamp);

	account = (Account *) iter->user_data;

	switch (column) {
		case GNC_TREE_MODEL_ACCOUNT_COL_NAME:
			g_value_init (value, G_TYPE_STRING);
			g_value_set_string (value, xaccAccountGetName (account));
			break;
		case GNC_TREE_MODEL_ACCOUNT_COL_TYPE:
			g_value_init (value, G_TYPE_STRING);
			g_value_set_string (value, 
					    xaccAccountGetTypeStr (xaccAccountGetType (account)));
			break;
		case GNC_TREE_MODEL_ACCOUNT_COL_CODE:
			g_value_init (value, G_TYPE_STRING);
			g_value_set_string (value, xaccAccountGetCode (account));
			break;
		case GNC_TREE_MODEL_ACCOUNT_COL_COMMODITY:
			g_value_init (value, G_TYPE_STRING);
			g_value_set_string (value,
					    gnc_commodity_get_fullname(xaccAccountGetCommodity (account)));
			break;
		case GNC_TREE_MODEL_ACCOUNT_COL_DESCRIPTION:
			g_value_init (value, G_TYPE_STRING);
			g_value_set_string (value, xaccAccountGetDescription (account));
			break;

		case GNC_TREE_MODEL_ACCOUNT_COL_PRESENT:
			g_value_init (value, G_TYPE_STRING);
			string = gnc_ui_account_get_print_balance(xaccAccountGetPresentBalanceInCurrency,
								  account, FALSE, &negative);
			g_value_set_string_take_ownership (value, string);
			break;
		case GNC_TREE_MODEL_ACCOUNT_COL_PRESENT_REPORT:
			g_value_init (value, G_TYPE_STRING);
			string = gnc_ui_account_get_print_report_balance(xaccAccountGetPresentBalanceInCurrency,
									 account, FALSE, &negative);
			g_value_set_string_take_ownership (value, string);
			break;
		case GNC_TREE_MODEL_ACCOUNT_COL_COLOR_PRESENT:
			g_value_init (value, G_TYPE_STRING);
			string = gnc_ui_account_get_print_balance(xaccAccountGetPresentBalanceInCurrency,
								  account, FALSE, &negative);
			g_value_set_static_string (value, negative ? "red" : "black");
			break;

		case GNC_TREE_MODEL_ACCOUNT_COL_BALANCE:
			g_value_init (value, G_TYPE_STRING);
			string = gnc_ui_account_get_print_balance(xaccAccountGetBalanceInCurrency,
								  account, FALSE, &negative);
			g_value_set_string_take_ownership (value, string);
			break;
		case GNC_TREE_MODEL_ACCOUNT_COL_BALANCE_REPORT:
			g_value_init (value, G_TYPE_STRING);
			string = gnc_ui_account_get_print_report_balance(xaccAccountGetBalanceInCurrency,
									 account, FALSE, &negative);
			g_value_set_string_take_ownership (value, string);
			break;
		case GNC_TREE_MODEL_ACCOUNT_COL_COLOR_BALANCE:
			g_value_init (value, G_TYPE_STRING);
			string = gnc_ui_account_get_print_balance(xaccAccountGetBalanceInCurrency,
								  account, FALSE, &negative);
			g_value_set_static_string (value, negative ? "red" : "black");
			break;

		case GNC_TREE_MODEL_ACCOUNT_COL_CLEARED:
			g_value_init (value, G_TYPE_STRING);
			string = gnc_ui_account_get_print_balance(xaccAccountGetClearedBalanceInCurrency,
								  account, FALSE, &negative);
			g_value_set_string_take_ownership (value, string);
			break;
		case GNC_TREE_MODEL_ACCOUNT_COL_CLEARED_REPORT:
			g_value_init (value, G_TYPE_STRING);
			string = gnc_ui_account_get_print_report_balance(xaccAccountGetClearedBalanceInCurrency,
									 account, FALSE, &negative);
			g_value_set_string_take_ownership (value, string);
			break;
		case GNC_TREE_MODEL_ACCOUNT_COL_COLOR_CLEARED:
			g_value_init (value, G_TYPE_STRING);
			string = gnc_ui_account_get_print_balance(xaccAccountGetClearedBalanceInCurrency,
								  account, FALSE, &negative);
			g_value_set_static_string (value, negative ? "red" : "black");
			break;

		case GNC_TREE_MODEL_ACCOUNT_COL_RECONCILED:
			g_value_init (value, G_TYPE_STRING);
			string = gnc_ui_account_get_print_balance(xaccAccountGetReconciledBalanceInCurrency,
								  account, FALSE, &negative);
			g_value_set_string_take_ownership (value, string);
			break;
		case GNC_TREE_MODEL_ACCOUNT_COL_RECONCILED_REPORT:
			g_value_init (value, G_TYPE_STRING);
			string = gnc_ui_account_get_print_report_balance(xaccAccountGetReconciledBalanceInCurrency,
									 account, FALSE, &negative);
			g_value_set_string_take_ownership (value, string);
			break;
		case GNC_TREE_MODEL_ACCOUNT_COL_COLOR_RECONCILED:
			g_value_init (value, G_TYPE_STRING);
			string = gnc_ui_account_get_print_balance(xaccAccountGetReconciledBalanceInCurrency,
								  account, FALSE, &negative);
			g_value_set_static_string (value, negative ? "red" : "black");
			break;

		case GNC_TREE_MODEL_ACCOUNT_COL_FUTURE_MIN:
			g_value_init (value, G_TYPE_STRING);
			string = gnc_ui_account_get_print_balance(xaccAccountGetProjectedMinimumBalanceInCurrency,
								  account, FALSE, &negative);
			g_value_set_string_take_ownership (value, string);
			break;
		case GNC_TREE_MODEL_ACCOUNT_COL_FUTURE_MIN_REPORT:
			g_value_init (value, G_TYPE_STRING);
			string = gnc_ui_account_get_print_report_balance(xaccAccountGetProjectedMinimumBalanceInCurrency,
									 account, FALSE, &negative);
			g_value_set_string_take_ownership (value, string);
			break;
		case GNC_TREE_MODEL_ACCOUNT_COL_COLOR_FUTURE_MIN:
			g_value_init (value, G_TYPE_STRING);
			string = gnc_ui_account_get_print_balance(xaccAccountGetProjectedMinimumBalanceInCurrency,
								  account, FALSE, &negative);
			g_value_set_static_string (value, negative ? "red" : "black");
			break;

		case GNC_TREE_MODEL_ACCOUNT_COL_TOTAL:
			g_value_init (value, G_TYPE_STRING);
			string = gnc_ui_account_get_print_balance(xaccAccountGetBalanceInCurrency,
								  account, TRUE, &negative);
			g_value_set_string_take_ownership (value, string);
			break;
		case GNC_TREE_MODEL_ACCOUNT_COL_TOTAL_REPORT:
			g_value_init (value, G_TYPE_STRING);
			string = gnc_ui_account_get_print_report_balance(xaccAccountGetBalanceInCurrency,
									 account, TRUE, &negative);
			g_value_set_string_take_ownership (value, string);
			break;
		case GNC_TREE_MODEL_ACCOUNT_COL_COLOR_TOTAL:
			g_value_init (value, G_TYPE_STRING);
			string = gnc_ui_account_get_print_balance(xaccAccountGetBalanceInCurrency,
								  account, TRUE, &negative);
			g_value_set_static_string (value, negative ? "red" : "black");
			break;

		case GNC_TREE_MODEL_ACCOUNT_COL_NOTES:
			g_value_init (value, G_TYPE_STRING);
			g_value_set_string (value, xaccAccountGetNotes (account));
			break;

		case GNC_TREE_MODEL_ACCOUNT_COL_TAX_INFO:
			g_value_init (value, G_TYPE_STRING);
			g_value_set_string (value, gnc_ui_account_get_tax_info_string (account));
			break;

		case GNC_TREE_MODEL_ACCOUNT_COL_LASTNUM:
			g_value_init (value, G_TYPE_STRING);
			g_value_set_string (value, xaccAccountGetLastNum (account));
			break;
		case GNC_TREE_MODEL_ACCOUNT_COL_PLACEHOLDER:
			g_value_init (value, G_TYPE_BOOLEAN);
			g_value_set_boolean (value, xaccAccountGetPlaceholder (account));
			break;

		case GNC_TREE_MODEL_ACCOUNT_COL_ALIGN_RIGHT:
			g_value_init (value, G_TYPE_FLOAT);
			g_value_set_float (value, 1.0);
			break;

		default:
			g_assert_not_reached ();
	}
	//LEAVE(" ");
}

static gboolean
gnc_tree_model_account_iter_next (GtkTreeModel *tree_model,
				  GtkTreeIter *iter)
{
	GncTreeModelAccount *model = GNC_TREE_MODEL_ACCOUNT (tree_model);
	Account *account;
	AccountGroup *group;
	gint i;

	ENTER("model %p, iter [stamp:%x data:%p, %p, %d]", tree_model,
	      iter->stamp, iter->user_data, iter->user_data2, GPOINTER_TO_INT(iter->user_data3));
	g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (model), FALSE);
	g_return_val_if_fail (iter != NULL, FALSE);
	g_return_val_if_fail (iter->user_data != NULL, FALSE);
	g_return_val_if_fail (iter->stamp == model->stamp, FALSE);

	if (iter->user_data == model->priv->toplevel) {
		iter->stamp = 0;
		LEAVE("failed (1)");
		return FALSE;
	}

	group = (AccountGroup *) iter->user_data2;
	i = GPOINTER_TO_INT (iter->user_data3);

	if (i > xaccGroupGetNumAccounts (group) - 2) {
		iter->stamp = 0;
		LEAVE("failed (2)");
		return FALSE;
	}

	account = xaccGroupGetAccount (group, i + 1);

	if (account == NULL) {
		iter->stamp = 0;
		LEAVE("failed (3)");
		return FALSE;
	}

	iter->user_data = account;
	iter->user_data2 = group;
	iter->user_data3 = GINT_TO_POINTER (i + 1);

	LEAVE("iter [stamp:%x data:%p, %p, %d]",
	      iter->stamp, iter->user_data, iter->user_data2, GPOINTER_TO_INT(iter->user_data3));
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

	if (parent) {
	  ENTER("model %p, iter [stamp:%x data:%p, %p, %d], parent [stamp:%x data:%p, %p, %d]", tree_model,
		iter->stamp, iter->user_data, iter->user_data2, GPOINTER_TO_INT(iter->user_data3),
		parent->stamp, parent->user_data, parent->user_data2, GPOINTER_TO_INT(parent->user_data3));
	} else {
	  ENTER("model %p, iter [stamp:%x data:%p, %p, %d], parent (null)", tree_model,
		iter->stamp, iter->user_data, iter->user_data2, GPOINTER_TO_INT(iter->user_data3));
	}
	g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (tree_model), FALSE);

	model = GNC_TREE_MODEL_ACCOUNT (tree_model);

	if (model->priv->toplevel != NULL) {
		if (parent == NULL) {
			iter->user_data = model->priv->toplevel;
			iter->user_data2 = NULL;
			iter->user_data3 = GINT_TO_POINTER (0);
			iter->stamp = model->stamp;
			LEAVE("iter (1) [stamp:%x data:%p, %p, %d]",
			      iter->stamp, iter->user_data, iter->user_data2, GPOINTER_TO_INT(iter->user_data3));
			return TRUE;
		} else if (parent->user_data == model->priv->toplevel) {
			parent = NULL;
		}
	}

	if (model->priv->root == NULL || xaccGroupGetNumAccounts (model->priv->root) == 0) {
		iter->stamp = 0;
		LEAVE("failed (1)");
		return FALSE;
	}

	if (parent == NULL) {
		account = xaccGroupGetAccount (model->priv->root, 0);
		
		if (account == NULL) {
			iter->stamp = 0;
			LEAVE("failed (2)");
			return FALSE;
		}

		iter->user_data = account;
		iter->user_data2 = model->priv->root;
		iter->user_data3 = GINT_TO_POINTER (0);
		iter->stamp = model->stamp;
		LEAVE("iter (2) [stamp:%x data:%p, %p, %d]",
		      iter->stamp, iter->user_data, iter->user_data2, GPOINTER_TO_INT(iter->user_data3));
		return TRUE;	
	}

	g_return_val_if_fail (parent != NULL, FALSE);
	g_return_val_if_fail (parent->user_data != NULL, FALSE);
	g_return_val_if_fail (parent->stamp == model->stamp, FALSE);	

	group = xaccAccountGetChildren ((Account *) parent->user_data);

	if (group == NULL || xaccGroupGetNumAccounts (group) == 0) {
		iter->stamp = 0;
		LEAVE("failed (3)");
		return FALSE;
	}

	account = xaccGroupGetAccount (group, 0);
	
	if (account == NULL) {
		iter->stamp = 0;
		LEAVE("failed (4)");
		return FALSE;
	}

	iter->user_data = account;
	iter->user_data2 = group;
	iter->user_data3 = GINT_TO_POINTER (0);
	iter->stamp = model->stamp;
	LEAVE("iter (3) [stamp:%x data:%p, %p, %d]",
	      iter->stamp, iter->user_data, iter->user_data2, GPOINTER_TO_INT(iter->user_data3));
	return TRUE;
}

static gboolean
gnc_tree_model_account_iter_has_child (GtkTreeModel *tree_model,
				       GtkTreeIter *iter)
{
	GncTreeModelAccount *model;
	AccountGroup *group;

	ENTER("model %p, iter [stamp:%x data:%p, %p, %d]", tree_model,
	      iter->stamp, iter->user_data, iter->user_data2, GPOINTER_TO_INT(iter->user_data3));
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
		LEAVE("no");
		return FALSE;
	}

	LEAVE("yes");
	return TRUE;
}

static int
gnc_tree_model_account_iter_n_children (GtkTreeModel *tree_model,
					GtkTreeIter *iter)
{
	GncTreeModelAccount *model;
	AccountGroup *group;

	ENTER("model %p, iter [stamp:%x data:%p, %p, %d]", tree_model,
	      iter->stamp, iter->user_data, iter->user_data2, GPOINTER_TO_INT(iter->user_data3));
	g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (tree_model), FALSE);

	model = GNC_TREE_MODEL_ACCOUNT (tree_model);

	if (iter == NULL) {
		if (model->priv->toplevel != NULL) {
			LEAVE("count is 1");
			return 1;
		} else {
			LEAVE("count is %d", xaccGroupGetNumAccounts (model->priv->root));
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

	LEAVE("count is %d", xaccGroupGetNumAccounts (group));
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

	if (parent) {
	  ENTER("model %p, iter [stamp:%x data:%p, %p, %d], parent [stamp:%x data:%p, %p, %d], n %d",
		tree_model,
		iter->stamp, iter->user_data, iter->user_data2, GPOINTER_TO_INT(iter->user_data3),
		parent->stamp, parent->user_data, parent->user_data2, GPOINTER_TO_INT(parent->user_data3), n);
	} else {
	  ENTER("model %p, iter [stamp:%x data:%p, %p, %d], parent (null), n %d",
		tree_model,
		iter->stamp, iter->user_data, iter->user_data2, GPOINTER_TO_INT(iter->user_data3), n);
	}
	g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (tree_model), FALSE);

	model = GNC_TREE_MODEL_ACCOUNT (tree_model);

	if (parent == NULL) {
		if (model->priv->toplevel != NULL) {
			if (n > 0) {
				iter->stamp = 0;
				LEAVE("failed (1)");
				return FALSE;
			} else {
				iter->user_data = model->priv->toplevel;
				iter->user_data2 = NULL;
				iter->user_data3 = GINT_TO_POINTER (0);
				iter->stamp = model->stamp;
				LEAVE("iter (1) [stamp:%x data:%p, %p, %d]",
				      iter->stamp, iter->user_data, iter->user_data2, GPOINTER_TO_INT(iter->user_data3));
				return TRUE;
			}
		}

		account = xaccGroupGetAccount (model->priv->root, n);

		if (account == NULL) {
			iter->stamp = 0;
			LEAVE("failed (2)");			
			return FALSE;
		}

		iter->user_data = account;
		iter->user_data2 = model->priv->root;
		iter->user_data3 = GINT_TO_POINTER (n);
		iter->stamp = model->stamp;
		LEAVE("iter (2) [stamp:%x data:%p, %p, %d]",
		      iter->stamp, iter->user_data, iter->user_data2, GPOINTER_TO_INT(iter->user_data3));
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
		LEAVE("failed (3)");
		return FALSE;
	}

	account = xaccGroupGetAccount (group, n);
	
	if (account == NULL) {
		iter->stamp = 0;
		LEAVE("failed (4)");
		return FALSE;
	}

	iter->user_data = account;
	iter->user_data2 = group;
	iter->user_data3 = GINT_TO_POINTER (n);
	iter->stamp = model->stamp;
	LEAVE("iter (3) [stamp:%x data:%p, %p, %d]",
	      iter->stamp, iter->user_data, iter->user_data2, GPOINTER_TO_INT(iter->user_data3));
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

	if (child) {
	  ENTER("model %p, iter [stamp:%x data:%p, %p, %d], child [stamp:%x data:%p, %p, %d]",
		tree_model,
		iter->stamp, iter->user_data, iter->user_data2, GPOINTER_TO_INT(iter->user_data3),
		child->stamp, child->user_data, child->user_data2, GPOINTER_TO_INT(child->user_data3));
	} else {
	  ENTER("model %p, iter [stamp:%x data:%p, %p, %d], child (null)",
		tree_model,
		iter->stamp, iter->user_data, iter->user_data2, GPOINTER_TO_INT(iter->user_data3));
	}
	g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (tree_model), FALSE);

	model = GNC_TREE_MODEL_ACCOUNT (tree_model);

	g_return_val_if_fail (child != NULL, FALSE);
	g_return_val_if_fail (child->user_data != NULL, FALSE);
	g_return_val_if_fail (child->stamp == model->stamp, FALSE);

	account = (Account *) child->user_data;

	if (account == model->priv->toplevel) {
		iter->stamp = 0;
		LEAVE("failed (1)");
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
			LEAVE("iter (1) [stamp:%x data:%p, %p, %d]",
			      iter->stamp, iter->user_data, iter->user_data2, GPOINTER_TO_INT(iter->user_data3));
			return TRUE;
		} else {
			iter->stamp = 0;
			LEAVE("failed (2)");
			return FALSE;
		}
	}

	for (i = 0; i < xaccGroupGetNumAccounts (group); i++) {
		if (xaccGroupGetAccount (group, i) == account) {
			iter->user_data = account;
			iter->user_data2 = group;
			iter->user_data3 = GINT_TO_POINTER (i);
			iter->stamp = model->stamp;
			LEAVE("iter (2) [stamp:%x data:%p, %p, %d]",
			      iter->stamp, iter->user_data, iter->user_data2, GPOINTER_TO_INT(iter->user_data3));
			return TRUE;	
		}
	}

	if (model->priv->toplevel != NULL) {
		iter->user_data = model->priv->toplevel;
		iter->user_data2 = NULL;
		iter->user_data3 = GINT_TO_POINTER (0);
		iter->stamp = model->stamp;
		LEAVE("iter (2) [stamp:%x data:%p, %p, %d]",
		      iter->stamp, iter->user_data, iter->user_data2, GPOINTER_TO_INT(iter->user_data3));
		return TRUE;
	}
	iter->stamp = 0;
	LEAVE("failed (3)");
	return FALSE;
}

static gpointer
account_row_inserted (Account *account,
		      gpointer data)
{
	GtkTreePath *path;
	GtkTreeIter iter;

	ENTER("account %p, model %p", account, data);
	gnc_tree_model_account_get_iter_from_account (GNC_TREE_MODEL_ACCOUNT (data), account, &iter);

	path = gtk_tree_model_get_path (GTK_TREE_MODEL (data), &iter);

	gtk_tree_model_row_inserted (GTK_TREE_MODEL (data), path, &iter);

	gtk_tree_path_free (path);

	LEAVE(" ");
	return NULL;
}

void gnc_tree_model_account_event_handler (GUID *entity, QofIdType type,
					   GNCEngineEventType event_type,
					   gpointer user_data)
{
  	GncTreeModelAccount *model;

	g_return_if_fail(GNC_IS_TREE_MODEL_ACCOUNT(user_data));

	model = (GncTreeModelAccount *)user_data;
	do {
		model->stamp++;
	} while (model->stamp == 0);
}
