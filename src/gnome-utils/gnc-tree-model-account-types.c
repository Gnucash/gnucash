/* 
 * gnc-tree-model-account-types.c -- GtkTreeModel implementation to display account types in a GtkTreeView.
 * Copyright (C) 2003 Jan Arne Petersen
 * Author: Jan Arne Petersen <jpetersen@uni-bonn.de>
 */

#include "config.h"

#include "gnc-tree-model-account-types.h"

#include "Account.h"

static void gnc_tree_model_account_types_class_init (GncTreeModelAccountTypesClass * klass);
static void gnc_tree_model_account_types_init (GncTreeModelAccountTypes * model);
static void gnc_tree_model_account_types_finalize (GObject * object);

static void gnc_tree_model_account_types_tree_model_init (GtkTreeModelIface * iface);
static guint gnc_tree_model_account_types_get_flags (GtkTreeModel * tree_model);
static int gnc_tree_model_account_types_get_n_columns (GtkTreeModel * tree_model);
static GType gnc_tree_model_account_types_get_column_type (GtkTreeModel * tree_model, int index);
static gboolean gnc_tree_model_account_types_get_iter (GtkTreeModel * tree_model,
						       GtkTreeIter * iter, GtkTreePath * path);
static GtkTreePath *gnc_tree_model_account_types_get_path (GtkTreeModel * tree_model, GtkTreeIter * iter);
static void gnc_tree_model_account_types_get_value (GtkTreeModel * tree_model,
						    GtkTreeIter * iter, int column, GValue * value);
static gboolean gnc_tree_model_account_types_iter_next (GtkTreeModel * tree_model, GtkTreeIter * iter);
static gboolean gnc_tree_model_account_types_iter_children (GtkTreeModel * tree_model,
							    GtkTreeIter * iter, GtkTreeIter * parent);
static gboolean gnc_tree_model_account_types_iter_has_child (GtkTreeModel * tree_model, GtkTreeIter * iter);
static int gnc_tree_model_account_types_iter_n_children (GtkTreeModel * tree_model, GtkTreeIter * iter);
static gboolean gnc_tree_model_account_types_iter_nth_child (GtkTreeModel * tree_model,
							     GtkTreeIter * iter, GtkTreeIter * parent, int n);
static gboolean gnc_tree_model_account_types_iter_parent (GtkTreeModel * tree_model,
							  GtkTreeIter * iter, GtkTreeIter * child);

struct GncTreeModelAccountTypesPrivate
{
	guint32 selected;
};

static GObjectClass *parent_class = NULL;

GType
gnc_tree_model_account_types_get_type (void)
{
	static GType gnc_tree_model_account_types_type = 0;

	if (gnc_tree_model_account_types_type == 0) {
		static const GTypeInfo our_info = {
			sizeof (GncTreeModelAccountTypesClass),
			NULL,
			NULL,
			(GClassInitFunc) gnc_tree_model_account_types_class_init,
			NULL,
			NULL,
			sizeof (GncTreeModelAccountTypes),
			0,
			(GInstanceInitFunc) gnc_tree_model_account_types_init
		};

		static const GInterfaceInfo tree_model_info = {
			(GInterfaceInitFunc) gnc_tree_model_account_types_tree_model_init,
			NULL,
			NULL
		};

		gnc_tree_model_account_types_type = g_type_register_static (G_TYPE_OBJECT,
									    "GncTreeModelAccountTypes",
									    &our_info, 0);

		g_type_add_interface_static (gnc_tree_model_account_types_type,
					     GTK_TYPE_TREE_MODEL, &tree_model_info);
	}

	return gnc_tree_model_account_types_type;
}

static void
gnc_tree_model_account_types_class_init (GncTreeModelAccountTypesClass * klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	parent_class = g_type_class_peek_parent (klass);

	object_class->finalize = gnc_tree_model_account_types_finalize;
}

static void
gnc_tree_model_account_types_init (GncTreeModelAccountTypes * model)
{
	while (model->stamp == 0) {
		model->stamp = g_random_int ();
	}

	model->priv = g_new0 (GncTreeModelAccountTypesPrivate, 1);
}

static void
gnc_tree_model_account_types_finalize (GObject * object)
{
	GncTreeModelAccountTypes *model;

	g_return_if_fail (object != NULL);
	g_return_if_fail (GNC_IS_TREE_MODEL_ACCOUNT_TYPES (object));

	model = GNC_TREE_MODEL_ACCOUNT_TYPES (object);

	g_return_if_fail (model->priv != NULL);

	g_free (model->priv);

	G_OBJECT_CLASS (parent_class)->finalize (object);
}

GtkTreeModel *
gnc_tree_model_account_types_new (guint32 selected)
{
	GncTreeModelAccountTypes *model;

	model = g_object_new (GNC_TYPE_TREE_MODEL_ACCOUNT_TYPES, NULL);

	model->priv->selected = selected;

	return GTK_TREE_MODEL (model);
}

guint32
gnc_tree_model_account_types_get_selected (GncTreeModelAccountTypes * model)
{
	g_return_val_if_fail (model != NULL, 0);
	g_return_val_if_fail (model->priv != NULL, 0);

	return model->priv->selected;
}

void
gnc_tree_model_account_types_set_selected (GncTreeModelAccountTypes * model,
					   guint32 selected)
{
	g_return_if_fail (model != NULL);
	g_return_if_fail (model->priv != NULL);

	model->priv->selected = selected;
}

static void
gnc_tree_model_account_types_tree_model_init (GtkTreeModelIface * iface)
{
	iface->get_flags = gnc_tree_model_account_types_get_flags;
	iface->get_n_columns = gnc_tree_model_account_types_get_n_columns;
	iface->get_column_type = gnc_tree_model_account_types_get_column_type;
	iface->get_iter = gnc_tree_model_account_types_get_iter;
	iface->get_path = gnc_tree_model_account_types_get_path;
	iface->get_value = gnc_tree_model_account_types_get_value;
	iface->iter_next = gnc_tree_model_account_types_iter_next;
	iface->iter_children = gnc_tree_model_account_types_iter_children;
	iface->iter_has_child = gnc_tree_model_account_types_iter_has_child;
	iface->iter_n_children = gnc_tree_model_account_types_iter_n_children;
	iface->iter_nth_child = gnc_tree_model_account_types_iter_nth_child;
	iface->iter_parent = gnc_tree_model_account_types_iter_parent;
}

static guint
gnc_tree_model_account_types_get_flags (GtkTreeModel * tree_model)
{
	return 0;
}

static int
gnc_tree_model_account_types_get_n_columns (GtkTreeModel * tree_model)
{
	return GNC_TREE_MODEL_ACCOUNT_TYPES_NUM_COLUMNS;
}

static GType
gnc_tree_model_account_types_get_column_type (GtkTreeModel * tree_model, int index)
{
	g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT_TYPES (tree_model), G_TYPE_INVALID);
	g_return_val_if_fail ((index < GNC_TREE_MODEL_ACCOUNT_TYPES_NUM_COLUMNS)
			      && (index >= 0), G_TYPE_INVALID);

	switch (index) {
		case GNC_TREE_MODEL_ACCOUNT_TYPES_COL_TYPE:
			return G_TYPE_UINT;
	       	case GNC_TREE_MODEL_ACCOUNT_TYPES_COL_NAME:
			return G_TYPE_STRING;
		case GNC_TREE_MODEL_ACCOUNT_TYPES_COL_SELECTED:
			return G_TYPE_BOOLEAN;
		default:
			g_assert_not_reached ();
			return G_TYPE_INVALID;
	}
}

static gboolean
gnc_tree_model_account_types_get_iter (GtkTreeModel * tree_model, GtkTreeIter * iter, GtkTreePath * path)
{
	GncTreeModelAccountTypes *model = GNC_TREE_MODEL_ACCOUNT_TYPES (tree_model);
	gint i;

	g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT_TYPES (model), FALSE);
	g_return_val_if_fail (gtk_tree_path_get_depth (path) > 0, FALSE);

	i = gtk_tree_path_get_indices (path)[0];

	if (i > NO_TYPE && i < NUM_ACCOUNT_TYPES) {
		iter->stamp = 0;

		return FALSE;		
	}

	iter->stamp = model->stamp;
	iter->user_data = GINT_TO_POINTER (i);

	return TRUE;
}

static GtkTreePath *
gnc_tree_model_account_types_get_path (GtkTreeModel * tree_model, GtkTreeIter * iter)
{
	GncTreeModelAccountTypes *model = GNC_TREE_MODEL_ACCOUNT_TYPES (tree_model);
	GtkTreePath *path;

	g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT_TYPES (model), NULL);
	g_return_val_if_fail (iter != NULL, NULL);
	g_return_val_if_fail (iter->stamp == model->stamp, NULL);

	path = gtk_tree_path_new ();

	gtk_tree_path_append_index (path, GPOINTER_TO_INT (iter->user_data));

	return path;
}

static void
gnc_tree_model_account_types_get_value (GtkTreeModel * tree_model,
					GtkTreeIter * iter, int column, GValue * value)
{
	GncTreeModelAccountTypes *model = GNC_TREE_MODEL_ACCOUNT_TYPES (tree_model);

	g_return_if_fail (GNC_IS_TREE_MODEL_ACCOUNT_TYPES (model));
	g_return_if_fail (model->priv != NULL);
	g_return_if_fail (iter != NULL);
	g_return_if_fail (iter->stamp == model->stamp);

	switch (column) {
		case GNC_TREE_MODEL_ACCOUNT_TYPES_COL_TYPE:
			g_value_init (value, G_TYPE_INT);

			g_value_set_int (value, GPOINTER_TO_INT (iter->user_data));
			break;
		case GNC_TREE_MODEL_ACCOUNT_TYPES_COL_NAME:
			g_value_init (value, G_TYPE_STRING);

			g_value_set_string (value, xaccAccountGetTypeStr (GPOINTER_TO_INT (iter->user_data)));
			break;
		case GNC_TREE_MODEL_ACCOUNT_TYPES_COL_SELECTED:
			g_value_init (value, G_TYPE_BOOLEAN);

			g_value_set_boolean (value, model->priv->selected & (1 << GPOINTER_TO_INT (iter->user_data)));
			break;
		default:
			g_assert_not_reached ();
	}
}

static gboolean
gnc_tree_model_account_types_iter_next (GtkTreeModel * tree_model, GtkTreeIter * iter)
{
	GncTreeModelAccountTypes *model = GNC_TREE_MODEL_ACCOUNT_TYPES (tree_model);

	g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT_TYPES (model), FALSE);
	g_return_val_if_fail (iter != NULL, FALSE);
	g_return_val_if_fail (iter->stamp == model->stamp, FALSE);

	if (GPOINTER_TO_INT (iter->user_data) < NUM_ACCOUNT_TYPES - 1) {
		iter->user_data = GINT_TO_POINTER (GPOINTER_TO_INT (iter->user_data) + 1);
		
		return TRUE;
	}

	iter->stamp = 0;

	return FALSE;
}

static gboolean
gnc_tree_model_account_types_iter_children (GtkTreeModel * tree_model,
					    GtkTreeIter * iter, GtkTreeIter * parent)
{

	g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT_TYPES (tree_model), FALSE);

	if (parent != NULL)
		return FALSE;

	iter->stamp = GNC_TREE_MODEL_ACCOUNT_TYPES (tree_model)->stamp;
	iter->user_data = GINT_TO_POINTER (0);

	return TRUE;
}

static gboolean
gnc_tree_model_account_types_iter_has_child (GtkTreeModel * tree_model, GtkTreeIter * iter)
{
	return FALSE;
}

static int
gnc_tree_model_account_types_iter_n_children (GtkTreeModel * tree_model, GtkTreeIter * iter)
{
	g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT_TYPES (tree_model), -1);

	if (iter == NULL)
		return NUM_ACCOUNT_TYPES;

	g_return_val_if_fail (GNC_TREE_MODEL_ACCOUNT_TYPES (tree_model)->stamp == iter->stamp, -1);

	return 0;
}

static gboolean
gnc_tree_model_account_types_iter_nth_child (GtkTreeModel * tree_model,
					     GtkTreeIter * iter, GtkTreeIter * parent, int n)
{
	GncTreeModelAccountTypes *model;

	g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT_TYPES (tree_model), FALSE);

	if (parent != NULL)
		return FALSE;

	model = GNC_TREE_MODEL_ACCOUNT_TYPES (tree_model);

	if (n > NO_TYPE && n < NUM_ACCOUNT_TYPES) {
		iter->stamp = 0;

		return FALSE;		
	}

	iter->stamp = model->stamp;
	iter->user_data = GINT_TO_POINTER (n);

	return TRUE;
}

static gboolean
gnc_tree_model_account_types_iter_parent (GtkTreeModel * tree_model, GtkTreeIter * iter, GtkTreeIter * child)
{
	return FALSE;
}
