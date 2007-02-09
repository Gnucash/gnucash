/* 
 * gnc-tree-model-commodity.c -- GtkTreeModel implementation to
 *	display commodities in a GtkTreeView.
 *
 * Copyright (C) 2003 Jan Arne Petersen <jpetersen@uni-bonn.de>
 * Copyright (C) 2003 David Hampton <hampton@employees.org>
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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

/*
 * iter->user_data   Type NAMESPACE | COMMODITY
 * iter->user_data2  A pointer to the namespace/commodity
 * iter->user_data3  The index of the namespace/commodity within its parent list
 */

#include "config.h"

#include <gtk/gtk.h>
#include <string.h>

#include "gnc-tree-model-commodity.h"
#include "gnc-component-manager.h"
#include "gnc-engine.h"
#include "gnc-gobject-utils.h"
#include "gnc-ui-util.h"

#define ITER_IS_NAMESPACE GINT_TO_POINTER(1)
#define ITER_IS_COMMODITY GINT_TO_POINTER(2)

/** Static Globals *******************************************************/
static QofLogModule log_module = GNC_MOD_GUI;

/** Declarations *********************************************************/
static void gnc_tree_model_commodity_class_init (GncTreeModelCommodityClass *klass);
static void gnc_tree_model_commodity_init (GncTreeModelCommodity *model);
static void gnc_tree_model_commodity_finalize (GObject *object);
static void gnc_tree_model_commodity_dispose (GObject *object);

static void gnc_tree_model_commodity_tree_model_init (GtkTreeModelIface *iface);
static GtkTreeModelFlags gnc_tree_model_commodity_get_flags (GtkTreeModel *tree_model);
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
static void gnc_tree_model_commodity_event_handler (QofEntity *entity,
						    QofEventId event_type,
						    gpointer user_data,
						    gpointer event_data);

/** The instance private data for a commodity database tree model. */
typedef struct GncTreeModelCommodityPrivate
{
	QofBook *book;
	gnc_commodity_table *commodity_table;
	gint event_handler_id;
} GncTreeModelCommodityPrivate;

#define GNC_TREE_MODEL_COMMODITY_GET_PRIVATE(o)  \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_TREE_MODEL_COMMODITY, GncTreeModelCommodityPrivate))

/** A pointer to the parent class of a commodity tree model. */
static GtkObjectClass *parent_class = NULL;

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

		gnc_tree_model_commodity_type = g_type_register_static (GNC_TYPE_TREE_MODEL,
									GNC_TREE_MODEL_COMMODITY_NAME,
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
	GObjectClass *o_class = G_OBJECT_CLASS (klass);

	parent_class = g_type_class_peek_parent (klass);

	o_class->finalize = gnc_tree_model_commodity_finalize;
	o_class->dispose = gnc_tree_model_commodity_dispose;

	g_type_class_add_private(klass, sizeof(GncTreeModelCommodityPrivate));
}

static void
gnc_tree_model_commodity_init (GncTreeModelCommodity *model)
{
	while (model->stamp == 0) {
		model->stamp = g_random_int ();
	}
}

static void
gnc_tree_model_commodity_finalize (GObject *object)
{
	GncTreeModelCommodity *model;
	GncTreeModelCommodityPrivate *priv;

	g_return_if_fail (object != NULL);
	g_return_if_fail (GNC_IS_TREE_MODEL_COMMODITY (object));

	ENTER("model %p", object);

	model = GNC_TREE_MODEL_COMMODITY (object);
	priv = GNC_TREE_MODEL_COMMODITY_GET_PRIVATE(model);
	priv->book = NULL;
	priv->commodity_table = NULL;

	G_OBJECT_CLASS (parent_class)->finalize (object);
	LEAVE(" ");
}

static void
gnc_tree_model_commodity_dispose (GObject *object)
{
	GncTreeModelCommodity *model;
	GncTreeModelCommodityPrivate *priv;

	g_return_if_fail (object != NULL);
	g_return_if_fail (GNC_IS_TREE_MODEL_COMMODITY (object));

	ENTER("model %p", object);
	model = GNC_TREE_MODEL_COMMODITY (object);
	priv = GNC_TREE_MODEL_COMMODITY_GET_PRIVATE(model);

	if (priv->event_handler_id) {
	  qof_event_unregister_handler (priv->event_handler_id);
	  priv->event_handler_id = 0;
	}

	if (G_OBJECT_CLASS (parent_class)->dispose)
            G_OBJECT_CLASS (parent_class)->dispose (object);
	LEAVE(" ");
}

GtkTreeModel *
gnc_tree_model_commodity_new (QofBook *book, gnc_commodity_table *ct)
{
	GncTreeModelCommodity *model;
	GncTreeModelCommodityPrivate *priv;
	const GList *item;

    ENTER("");

	item = gnc_gobject_tracking_get_list(GNC_TREE_MODEL_COMMODITY_NAME);
	for ( ; item; item = g_list_next(item)) {
		model = (GncTreeModelCommodity *)item->data;
		priv = GNC_TREE_MODEL_COMMODITY_GET_PRIVATE(model);
		if (priv->commodity_table == ct) {
			g_object_ref(G_OBJECT(model));
			LEAVE("returning existing model %p", model);
			return GTK_TREE_MODEL(model);
		}
	}

	model = g_object_new (GNC_TYPE_TREE_MODEL_COMMODITY, NULL);
	priv = GNC_TREE_MODEL_COMMODITY_GET_PRIVATE(model);
	priv->book = book;
	priv->commodity_table = ct;

	priv->event_handler_id =
	  qof_event_register_handler (gnc_tree_model_commodity_event_handler, model);

    LEAVE("");
	return GTK_TREE_MODEL (model);
}

gboolean
gnc_tree_model_commodity_iter_is_namespace (GncTreeModelCommodity *model,
					    GtkTreeIter *iter)
{
	g_return_val_if_fail (GNC_IS_TREE_MODEL_COMMODITY (model), FALSE);
	g_return_val_if_fail (iter != NULL, FALSE);
	g_return_val_if_fail (iter->user_data != NULL, FALSE);
	g_return_val_if_fail (iter->stamp == model->stamp, FALSE);

	return (iter->user_data == ITER_IS_NAMESPACE);
}

gboolean
gnc_tree_model_commodity_iter_is_commodity (GncTreeModelCommodity *model,
					    GtkTreeIter *iter)
{
	g_return_val_if_fail (GNC_IS_TREE_MODEL_COMMODITY (model), FALSE);
	g_return_val_if_fail (iter != NULL, FALSE);
	g_return_val_if_fail (iter->user_data != NULL, FALSE);
	g_return_val_if_fail (iter->stamp == model->stamp, FALSE);

	return (iter->user_data == ITER_IS_COMMODITY);
}

gnc_commodity_namespace *
gnc_tree_model_commodity_get_namespace (GncTreeModelCommodity *model,
					GtkTreeIter *iter)
{
	g_return_val_if_fail (GNC_IS_TREE_MODEL_COMMODITY (model), NULL);
	g_return_val_if_fail (iter != NULL, NULL);
	g_return_val_if_fail (iter->user_data != NULL, NULL);
	g_return_val_if_fail (iter->stamp == model->stamp, NULL);

	if (iter->user_data != ITER_IS_NAMESPACE)
	  return NULL;
	return (gnc_commodity_namespace *)iter->user_data2;
}

gnc_commodity *
gnc_tree_model_commodity_get_commodity (GncTreeModelCommodity *model,
					GtkTreeIter *iter)
{
	g_return_val_if_fail (GNC_IS_TREE_MODEL_COMMODITY (model), NULL);
	g_return_val_if_fail (iter != NULL, NULL);
	g_return_val_if_fail (iter->user_data != NULL, NULL);
	g_return_val_if_fail (iter->stamp == model->stamp, NULL);

	if (iter->user_data != ITER_IS_COMMODITY)
	  return NULL;
	return (gnc_commodity *)iter->user_data2;
}

/************************************************************/
/*        Gnc Tree Model Debugging Utility Function         */
/************************************************************/

#define debug_path(fn, path) {				\
    gchar *path_string = gtk_tree_path_to_string(path); \
    fn("tree path %s", path_string);			\
    g_free(path_string);				\
  }

#define ITER_STRING_LEN 128

static const gchar *
iter_to_string (GtkTreeIter *iter)
{
  gnc_commodity_namespace *namespace;
  gnc_commodity *commodity = NULL;
#ifdef G_THREADS_ENABLED
  static GStaticPrivate gtmits_buffer_key = G_STATIC_PRIVATE_INIT;
  gchar *string;

  string = g_static_private_get (&gtmits_buffer_key);
  if (string == NULL) {
    string = g_malloc(ITER_STRING_LEN + 1);
    g_static_private_set (&gtmits_buffer_key, string, g_free);
  }
#else
  static char string[ITER_STRING_LEN + 1];
#endif
  if (iter) {
    switch (GPOINTER_TO_INT(iter->user_data)) {
     case GPOINTER_TO_INT(ITER_IS_NAMESPACE):
      namespace = (gnc_commodity_namespace *) iter->user_data2;
      snprintf(string, ITER_STRING_LEN, 
	       "[stamp:%x data:%d (NAMESPACE), %p (%s), %d]",
	       iter->stamp, GPOINTER_TO_INT(iter->user_data),
	       iter->user_data2, gnc_commodity_namespace_get_name (namespace),
	       GPOINTER_TO_INT(iter->user_data3));
      break;
    
     case GPOINTER_TO_INT(ITER_IS_COMMODITY):
      commodity = (gnc_commodity *) iter->user_data2;
      snprintf(string, ITER_STRING_LEN, 
	       "[stamp:%x data:%d (COMMODITY), %p (%s), %d]",
	       iter->stamp, GPOINTER_TO_INT(iter->user_data),
	       iter->user_data2, gnc_commodity_get_mnemonic (commodity),
	       GPOINTER_TO_INT(iter->user_data3));
      break;

     default:
      snprintf(string, ITER_STRING_LEN, 
	       "[stamp:%x data:%d (UNKNOWN), %p, %d]",
	       iter->stamp,
	       GPOINTER_TO_INT(iter->user_data),
	       iter->user_data2,
	       GPOINTER_TO_INT(iter->user_data3));
      break;
    }
  }
  return string;
}


/************************************************************/
/*       Gtk Tree Model Required Interface Functions        */
/************************************************************/

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

static GtkTreeModelFlags
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
		case GNC_TREE_MODEL_COMMODITY_COL_CUSIP:
		case GNC_TREE_MODEL_COMMODITY_COL_UNIQUE_NAME:
		case GNC_TREE_MODEL_COMMODITY_COL_QUOTE_SOURCE:
		case GNC_TREE_MODEL_COMMODITY_COL_QUOTE_TZ:
			return G_TYPE_STRING;
		case GNC_TREE_MODEL_COMMODITY_COL_FRACTION:
			return G_TYPE_INT;
		case GNC_TREE_MODEL_COMMODITY_COL_QUOTE_FLAG:
		case GNC_TREE_MODEL_COMMODITY_COL_VISIBILITY:
			return G_TYPE_BOOLEAN;
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
	GncTreeModelCommodity *model;
	GncTreeModelCommodityPrivate *priv;
	gnc_commodity_table *ct;
	gnc_commodity_namespace *namespace;
	gnc_commodity *commodity = NULL;
	GList *list;
	guint i, depth;

	iter->stamp = 0;
	g_return_val_if_fail (GNC_IS_TREE_MODEL_COMMODITY (tree_model), FALSE);
	g_return_val_if_fail (iter != NULL, FALSE);
	g_return_val_if_fail (path != NULL, FALSE);

	depth = gtk_tree_path_get_depth (path);
	ENTER("model %p, iter, %p, path %p (depth %d)", tree_model, iter, path, depth);
	debug_path(DEBUG, path);

	model = GNC_TREE_MODEL_COMMODITY (tree_model);
	priv = GNC_TREE_MODEL_COMMODITY_GET_PRIVATE(model);
	ct = priv->commodity_table;
	if (ct == NULL) {
	  LEAVE("no commodity table");
	  return FALSE;
	}

	if (depth == 0) {
	  LEAVE("depth too small");
	  return FALSE;
	}
	if (depth > 2) {
	  LEAVE("depth too big");
	  return FALSE;
	}

	list = gnc_commodity_table_get_namespaces_list(ct);
	i = gtk_tree_path_get_indices (path)[0];
    {
         if (!(i >= 0 && i < g_list_length (list))) { LEAVE(""); }
         g_return_val_if_fail (i >= 0 && i < g_list_length (list), FALSE);
    }
	namespace = g_list_nth_data (list, i);

	if (depth == 1) {
	  iter->stamp      = model->stamp;
	  iter->user_data  = ITER_IS_NAMESPACE;
	  iter->user_data2 = namespace;
	  iter->user_data3 = GINT_TO_POINTER(i);
	  LEAVE("iter (ns) %s", iter_to_string(iter));
	  return TRUE;
	}

	list = gnc_commodity_namespace_get_commodity_list(namespace);
	i = gtk_tree_path_get_indices (path)[1];
    {
         if (!(i >= 0 && i < g_list_length (list))) { LEAVE(""); }
         g_return_val_if_fail (i >= 0 && i < g_list_length (list), FALSE);
    }
	commodity = g_list_nth_data (list, i);

	iter->stamp      = model->stamp;
	iter->user_data  = ITER_IS_COMMODITY;
	iter->user_data2 = commodity;
	iter->user_data3 = GINT_TO_POINTER(i);
	LEAVE("iter (cm) %s", iter_to_string(iter));
	return TRUE;
}

static GtkTreePath *
gnc_tree_model_commodity_get_path (GtkTreeModel *tree_model,
				   GtkTreeIter *iter)
{
	GncTreeModelCommodity *model;
	GncTreeModelCommodityPrivate *priv;
	GtkTreePath *path;
	gnc_commodity_table *ct;
	gnc_commodity_namespace *namespace;
	GList *ns_list;

	g_return_val_if_fail (GNC_IS_TREE_MODEL_COMMODITY (tree_model), NULL);
	model = GNC_TREE_MODEL_COMMODITY (tree_model);
	g_return_val_if_fail (iter != NULL, NULL);
	g_return_val_if_fail (iter->user_data != NULL, NULL);
	g_return_val_if_fail (iter->user_data2 != NULL, NULL);
	g_return_val_if_fail (iter->stamp == model->stamp, NULL);
	ENTER("model %p, iter %p (%s)", tree_model, iter, iter_to_string(iter));
	
	priv = GNC_TREE_MODEL_COMMODITY_GET_PRIVATE(model);
	ct = priv->commodity_table;
	if (ct == NULL) {
	  LEAVE("no commodity table");
	  return FALSE;
	}

	if (iter->user_data == ITER_IS_NAMESPACE) {
	  path = gtk_tree_path_new ();
	  gtk_tree_path_append_index (path, GPOINTER_TO_INT(iter->user_data3));
	  debug_path(LEAVE, path);
	  return path;
	}

	ns_list = gnc_commodity_table_get_namespaces_list(ct);
	namespace = gnc_commodity_get_namespace_ds((gnc_commodity*)iter->user_data2);
	path = gtk_tree_path_new ();
	gtk_tree_path_append_index (path, g_list_index (ns_list, namespace));
	gtk_tree_path_append_index (path, GPOINTER_TO_INT(iter->user_data3));
	debug_path(LEAVE, path);
	return path;
}

static void
gnc_tree_model_commodity_get_value (GtkTreeModel *tree_model,
				    GtkTreeIter *iter,
				    int column,
				    GValue *value)
{
	GncTreeModelCommodity *model = GNC_TREE_MODEL_COMMODITY (tree_model);
	gnc_commodity_namespace *namespace;
	gnc_commodity *commodity;
	gnc_quote_source *source;

	g_return_if_fail (GNC_IS_TREE_MODEL_COMMODITY (tree_model));
	g_return_if_fail (iter != NULL);
	g_return_if_fail (iter->user_data != NULL);
	g_return_if_fail (iter->user_data2 != NULL);
	g_return_if_fail (iter->stamp == model->stamp);

	if (iter->user_data == ITER_IS_NAMESPACE) {
	  namespace = (gnc_commodity_namespace *)iter->user_data2;
	  switch (column) {
	   case GNC_TREE_MODEL_COMMODITY_COL_NAMESPACE:
	    g_value_init (value, G_TYPE_STRING);
	    g_value_set_string (value, gnc_commodity_namespace_get_name (namespace));
	    break;
	   default:
	    g_value_init (value, G_TYPE_STRING);
	    g_value_set_string (value, "");
	    break;
	   case GNC_TREE_MODEL_COMMODITY_COL_FRACTION:
	    g_value_init (value, G_TYPE_INT);
	    g_value_set_int (value, 0);
	    break;
	   case GNC_TREE_MODEL_COMMODITY_COL_QUOTE_FLAG:
	    g_value_init (value, G_TYPE_BOOLEAN);
	    g_value_set_boolean (value, FALSE);
	    break;
	   case GNC_TREE_MODEL_COMMODITY_COL_VISIBILITY:
	    g_value_init (value, G_TYPE_BOOLEAN);
	    g_value_set_boolean (value, FALSE);
	    break;
	  }
	  return;
	}

	commodity = (gnc_commodity *)iter->user_data2;
	switch (column) {
		case GNC_TREE_MODEL_COMMODITY_COL_MNEMONIC:
			g_value_init (value, G_TYPE_STRING);

			g_value_set_string (value, gnc_commodity_get_mnemonic (commodity));
			break;
		case GNC_TREE_MODEL_COMMODITY_COL_NAMESPACE:
			g_value_init (value, G_TYPE_STRING);

//			g_value_set_string (value, gnc_commodity_get_namespace (commodity));
			g_value_set_string (value, NULL);
			break;
		case GNC_TREE_MODEL_COMMODITY_COL_FULLNAME:
			g_value_init (value, G_TYPE_STRING);

			g_value_set_string (value, gnc_commodity_get_fullname (commodity));
			break;
		case GNC_TREE_MODEL_COMMODITY_COL_PRINTNAME:
			g_value_init (value, G_TYPE_STRING);

			g_value_set_string (value, gnc_commodity_get_printname (commodity));
			break;
		case GNC_TREE_MODEL_COMMODITY_COL_CUSIP:
			g_value_init (value, G_TYPE_STRING);

			g_value_set_string (value, gnc_commodity_get_cusip (commodity));
			break;
		case GNC_TREE_MODEL_COMMODITY_COL_UNIQUE_NAME:
			g_value_init (value, G_TYPE_STRING);

			g_value_set_string (value, gnc_commodity_get_unique_name (commodity));
			break;
		case GNC_TREE_MODEL_COMMODITY_COL_FRACTION:
			g_value_init (value, G_TYPE_INT);

			g_value_set_int (value, gnc_commodity_get_fraction (commodity));
			break;
		case GNC_TREE_MODEL_COMMODITY_COL_QUOTE_FLAG:
			g_value_init (value, G_TYPE_BOOLEAN);

			g_value_set_boolean (value, gnc_commodity_get_quote_flag (commodity));
			break;
		case GNC_TREE_MODEL_COMMODITY_COL_QUOTE_SOURCE:
			g_value_init (value, G_TYPE_STRING);

			if (gnc_commodity_get_quote_flag (commodity)) {
			  source = gnc_commodity_get_quote_source (commodity);
			  g_value_set_string (value, gnc_quote_source_get_internal_name(source));
			} else {
			  g_value_set_static_string (value, "");
			}
			break;
		case GNC_TREE_MODEL_COMMODITY_COL_QUOTE_TZ:
			g_value_init (value, G_TYPE_STRING);

			if (gnc_commodity_get_quote_flag (commodity)) {
			  g_value_set_string (value, gnc_commodity_get_quote_tz (commodity));
			} else {
			  g_value_set_static_string (value, "");
			}
			break;
		case GNC_TREE_MODEL_COMMODITY_COL_VISIBILITY:
	       		g_value_init (value, G_TYPE_BOOLEAN);
			g_value_set_boolean (value, TRUE);
			break;
		default:
			g_assert_not_reached ();
	}
}

static gboolean
gnc_tree_model_commodity_iter_next (GtkTreeModel *tree_model,
				    GtkTreeIter *iter)
{
	GncTreeModelCommodity *model;
	GncTreeModelCommodityPrivate *priv;
	gnc_commodity_table *ct;
	gnc_commodity_namespace *namespace;
	GList *list;
	int n;

	g_return_val_if_fail (GNC_IS_TREE_MODEL_COMMODITY (tree_model), FALSE);
	model = GNC_TREE_MODEL_COMMODITY (tree_model);
	g_return_val_if_fail (iter != NULL, FALSE);
	g_return_val_if_fail (iter->user_data != NULL, FALSE);
	g_return_val_if_fail (iter->user_data2 != NULL, FALSE);
	g_return_val_if_fail (iter->stamp == model->stamp, FALSE);

	ENTER("model %p, iter %p(%s)", tree_model, iter, iter_to_string(iter));
	priv = GNC_TREE_MODEL_COMMODITY_GET_PRIVATE(model);
	if (iter->user_data == ITER_IS_NAMESPACE) {
	  ct = priv->commodity_table;
	  list = gnc_commodity_table_get_namespaces_list(ct);
	} else if (iter->user_data == ITER_IS_COMMODITY) {
	  namespace = gnc_commodity_get_namespace_ds((gnc_commodity *)iter->user_data2);
	  list = gnc_commodity_namespace_get_commodity_list(namespace);
	} else {
	  LEAVE("unknown iter type");
	  return FALSE;
	}

	n = GPOINTER_TO_INT(iter->user_data3) + 1;
	iter->user_data2 = g_list_nth_data(list, n);
	if (iter->user_data2 == NULL) {
	  LEAVE("no next iter");
	  return FALSE;
	}
	iter->user_data3 = GINT_TO_POINTER(n);
	LEAVE("iter %p(%s)", iter, iter_to_string(iter));
	return TRUE;
}


static gboolean
gnc_tree_model_commodity_iter_children (GtkTreeModel *tree_model,
					GtkTreeIter *iter,
					GtkTreeIter *parent)
{
	GncTreeModelCommodity *model;
	GncTreeModelCommodityPrivate *priv;
	gnc_commodity_table *ct;
	gnc_commodity_namespace *namespace;
	GList *list;

	g_return_val_if_fail (GNC_IS_TREE_MODEL_COMMODITY (tree_model), FALSE);

	ENTER("model %p, iter %p, parent %p (%s)",
	      tree_model, iter, parent, iter_to_string(parent));
	model = GNC_TREE_MODEL_COMMODITY (tree_model);
	priv = GNC_TREE_MODEL_COMMODITY_GET_PRIVATE(model);

	if (parent == NULL) {
	  ct = priv->commodity_table;
	  list = gnc_commodity_table_get_namespaces_list(ct);
	  if (list == NULL) {
	    LEAVE("no namespaces");
	    return FALSE;
	  }

	  iter->stamp      = model->stamp;
	  iter->user_data  = ITER_IS_NAMESPACE;
	  iter->user_data2 = g_list_nth_data(list, 0);
	  iter->user_data3 = GINT_TO_POINTER(0);
	  LEAVE("ns iter %p (%s)", iter, iter_to_string(iter));
	  return TRUE;
	}

	if (parent->user_data == ITER_IS_NAMESPACE) {
	  namespace = (gnc_commodity_namespace *)parent->user_data2;
	  list = gnc_commodity_namespace_get_commodity_list(namespace);
	  if (list == NULL) {
	    LEAVE("no commodities");
	    return FALSE;
	  }

	  iter->stamp      = model->stamp;
	  iter->user_data  = ITER_IS_COMMODITY;
	  iter->user_data2 = g_list_nth_data(list, 0);
	  iter->user_data3 = GINT_TO_POINTER(0);
	  LEAVE("cm iter %p (%s)", iter, iter_to_string(iter));
	  return TRUE;
	}

	LEAVE("FALSE");
	return FALSE;
}

static gboolean
gnc_tree_model_commodity_iter_has_child (GtkTreeModel *tree_model,
					 GtkTreeIter *iter)
{
	gnc_commodity_namespace *namespace;
	GList *list;

	g_return_val_if_fail (iter != NULL, FALSE);
	ENTER("model %p, iter %p (%s)", tree_model,
	      iter, iter_to_string(iter));

	if (iter->user_data != ITER_IS_NAMESPACE) {
	  LEAVE("no children (not ns)");
	  return FALSE;
	}

	namespace = (gnc_commodity_namespace *)iter->user_data2;
	list = gnc_commodity_namespace_get_commodity_list(namespace);
	LEAVE("%s children", list ? "has" : "no");
	return list != NULL;
}

static int
gnc_tree_model_commodity_iter_n_children (GtkTreeModel *tree_model,
					  GtkTreeIter *iter)
{
	GncTreeModelCommodity *model;
	GncTreeModelCommodityPrivate *priv;
	gnc_commodity_table *ct;
	gnc_commodity_namespace *namespace;
	GList *list;

	g_return_val_if_fail (GNC_IS_TREE_MODEL_COMMODITY (tree_model), -1);

	ENTER("model %p, iter %p (%s)", tree_model, iter, iter_to_string(iter));
	model = GNC_TREE_MODEL_COMMODITY (tree_model);
	priv = GNC_TREE_MODEL_COMMODITY_GET_PRIVATE(model);

	if (iter == NULL) {
	  ct = priv->commodity_table;
	  list = gnc_commodity_table_get_namespaces_list(ct);
	  LEAVE("ns list length %d", g_list_length(list));
	  return g_list_length (list);
	}

	if (iter->user_data == ITER_IS_NAMESPACE) {
	  namespace = (gnc_commodity_namespace *)iter->user_data2;
	  list = gnc_commodity_namespace_get_commodity_list(namespace);
	  LEAVE("cm list length %d", g_list_length(list));
	  return g_list_length (list);
	}

	LEAVE("0");
	return 0;
}

static gboolean
gnc_tree_model_commodity_iter_nth_child (GtkTreeModel *tree_model,
					 GtkTreeIter *iter,
					 GtkTreeIter *parent,
					 int n)
{
	GncTreeModelCommodity *model;
	GncTreeModelCommodityPrivate *priv;
	gnc_commodity_table *ct;
	gnc_commodity_namespace *namespace;
	GList *list;
	
	g_return_val_if_fail (GNC_IS_TREE_MODEL_COMMODITY (tree_model), FALSE);
	g_return_val_if_fail (iter != NULL, FALSE);

	ENTER("model %p, iter %p, parent %p (%s)",
	      tree_model, iter, parent, iter_to_string(parent));
	model = GNC_TREE_MODEL_COMMODITY (tree_model);
	priv = GNC_TREE_MODEL_COMMODITY_GET_PRIVATE(model);

	if (parent == NULL) {
	  ct = priv->commodity_table;
	  list = gnc_commodity_table_get_namespaces_list(ct);

	  iter->stamp      = model->stamp;
	  iter->user_data  = ITER_IS_NAMESPACE;
	  iter->user_data2 = g_list_nth_data(list, n);
	  iter->user_data3 = GINT_TO_POINTER(n);
	  LEAVE("ns iter %p (%s)", iter, iter_to_string(iter));
	  return iter->user_data2 != NULL;
	}

	if (parent->user_data == ITER_IS_NAMESPACE) {
	  namespace = (gnc_commodity_namespace *)parent->user_data2;
	  list = gnc_commodity_namespace_get_commodity_list(namespace);

	  iter->stamp      = model->stamp;
	  iter->user_data  = ITER_IS_COMMODITY;
	  iter->user_data2 = g_list_nth_data(list, n);
	  iter->user_data3 = GINT_TO_POINTER(n);
	  LEAVE("cm iter %p (%s)", iter, iter_to_string(iter));
	  return iter->user_data2 != NULL;
	}

	iter->stamp = 0;
	LEAVE("FALSE");
	return FALSE;
}

static gboolean
gnc_tree_model_commodity_iter_parent (GtkTreeModel *tree_model,
				      GtkTreeIter *iter,
				      GtkTreeIter *child)
{
	GncTreeModelCommodity *model;
	GncTreeModelCommodityPrivate *priv;
	gnc_commodity_table *ct;
	gnc_commodity_namespace *namespace;
	GList *list;
	
	g_return_val_if_fail (GNC_IS_TREE_MODEL_COMMODITY (tree_model), FALSE);
	g_return_val_if_fail (iter != NULL, FALSE);
	g_return_val_if_fail (child != NULL, FALSE);

	ENTER("model %p, iter %p, child %p (%s)",
	      tree_model, iter, child, iter_to_string(child));
	model = GNC_TREE_MODEL_COMMODITY (tree_model);
	priv = GNC_TREE_MODEL_COMMODITY_GET_PRIVATE(model);

	if (child->user_data == ITER_IS_NAMESPACE) {
	  LEAVE("ns has no parent");
	  return FALSE;
	}

	ct = priv->commodity_table;
	list = gnc_commodity_table_get_namespaces_list(ct);
	namespace = gnc_commodity_get_namespace_ds((gnc_commodity*)child->user_data2);

	iter->stamp      = model->stamp;
	iter->user_data  = ITER_IS_NAMESPACE;
	iter->user_data2 = namespace;
	iter->user_data3 = GINT_TO_POINTER(g_list_index(list, namespace));
	LEAVE("ns iter %p (%s)", iter, iter_to_string(iter));
	return TRUE;
}

/************************************************************/
/*              Commodity Tree View Functions               */
/************************************************************/

/*
 * Convert a model/commodity pair into a gtk_tree_model_iter.  This
 * routine should only be called from the file
 * gnc-tree-view-commodity.c.
 */
gboolean
gnc_tree_model_commodity_get_iter_from_commodity (GncTreeModelCommodity *model,
						  gnc_commodity *commodity,
						  GtkTreeIter *iter)
{
	gnc_commodity_namespace *namespace;
	GList *list;
	gint n;
	
	g_return_val_if_fail (GNC_IS_TREE_MODEL_COMMODITY (model), FALSE);
	g_return_val_if_fail ((commodity != NULL), FALSE);
	g_return_val_if_fail ((iter != NULL), FALSE);

	ENTER("model %p, commodity %p, iter %p", model, commodity, iter);

	namespace = gnc_commodity_get_namespace_ds(commodity);
	if (namespace == NULL) {
	  LEAVE("no namespace");
	  return FALSE;
	}

	list = gnc_commodity_namespace_get_commodity_list(namespace);
	if (list == NULL) {
	  LEAVE("empty list");
	  return FALSE;
	}

	n = g_list_index(list, commodity);
	if (n == -1) {
	  LEAVE("not in list");
	  return FALSE;
	}

	iter->stamp = model->stamp;
	iter->user_data  = ITER_IS_COMMODITY;
	iter->user_data2 = commodity;
	iter->user_data3 = GINT_TO_POINTER(n);
	LEAVE("iter %s", iter_to_string(iter));
	return TRUE;
}

/*
 * Convert a model/commodity pair into a gtk_tree_model_path.  This
 * routine should only be called from the file
 * gnc-tree-view-commodity.c.
 */
GtkTreePath *
gnc_tree_model_commodity_get_path_from_commodity (GncTreeModelCommodity *model,
						  gnc_commodity *commodity)
{
	GtkTreeIter tree_iter;
	GtkTreePath *tree_path;

	g_return_val_if_fail (GNC_IS_TREE_MODEL_COMMODITY (model), NULL);
	g_return_val_if_fail (commodity != NULL, NULL);
	ENTER("model %p, commodity %p", model, commodity);

	if (!gnc_tree_model_commodity_get_iter_from_commodity (model, commodity, &tree_iter)) {
	  LEAVE("no iter");
	  return NULL;
	}

	tree_path = gtk_tree_model_get_path (GTK_TREE_MODEL(model), &tree_iter);
	if (tree_path) {
	  gchar *path_string = gtk_tree_path_to_string(tree_path);
	  LEAVE("path (2) %s", path_string);
	  g_free(path_string);
	} else {
	  LEAVE("no path");
	}
	return tree_path;
}

/*
 * Convert a model/namespace pair into a gtk_tree_model_iter.  This
 * routine should only be called from the file
 * gnc-tree-view-commodity.c.
 */
gboolean
gnc_tree_model_commodity_get_iter_from_namespace (GncTreeModelCommodity *model,
						  gnc_commodity_namespace *namespace,
						  GtkTreeIter *iter)
{
	GncTreeModelCommodityPrivate *priv;
	GList *list;
	gint n;
	
	g_return_val_if_fail (GNC_IS_TREE_MODEL_COMMODITY (model), FALSE);
	g_return_val_if_fail ((namespace != NULL), FALSE);
	g_return_val_if_fail ((iter != NULL), FALSE);

	ENTER("model %p, namespace %p, iter %p", model, namespace, iter);

	priv = GNC_TREE_MODEL_COMMODITY_GET_PRIVATE(model);
	list = gnc_commodity_table_get_namespaces_list(priv->commodity_table);
	if (list == NULL)
    {
      LEAVE("");
	  return FALSE;
    }

	n = g_list_index(list, namespace);
	if (n == -1)
    {
      LEAVE("");
	  return FALSE;
    }

	iter->stamp = model->stamp;
	iter->user_data  = ITER_IS_NAMESPACE;
	iter->user_data2 = namespace;
	iter->user_data3 = GINT_TO_POINTER(n);
	LEAVE("iter %s", iter_to_string(iter));
	return TRUE;
}

/*
 * Convert a model/namespace pair into a gtk_tree_model_path.  This
 * routine should only be called from the file
 * gnc-tree-view-commodity.c.
 */
GtkTreePath *
gnc_tree_model_commodity_get_path_from_namespace (GncTreeModelCommodity *model,
						  gnc_commodity_namespace *namespace)
{
	GtkTreeIter tree_iter;
	GtkTreePath *tree_path;

	ENTER("model %p, namespace %p", model, namespace);
	g_return_val_if_fail (GNC_IS_TREE_MODEL_COMMODITY (model), NULL);
	g_return_val_if_fail (namespace != NULL, NULL);

	if (!gnc_tree_model_commodity_get_iter_from_namespace (model, namespace, &tree_iter)) {
	  LEAVE("no iter");
	  return NULL;
	}

	tree_path = gtk_tree_model_get_path (GTK_TREE_MODEL(model), &tree_iter);
	if (tree_path) {
	  gchar *path_string = gtk_tree_path_to_string(tree_path);
	  LEAVE("path (2) %s", path_string);
	  g_free(path_string);
	} else {
	  LEAVE("no path");
	}
	return tree_path;
}

/************************************************************/
/*  Commodity Tree Model - Engine Event Handling Functions  */
/************************************************************/

typedef struct _remove_data {
  GncTreeModelCommodity *model;
  GtkTreePath           *path;
} remove_data;

static GSList *pending_removals = NULL;

/** This function performs updating to the model after an commodity
 *  has been added.  The parent entry needs to be tapped on the
 *  shoulder so that it can correctly update the disclosure triangle
 *  (first added child) or possibly rebuild its child list of that
 *  level of accounts is visible.
 *
 *  @internal
 *
 *  @param model The commodity tree model containing the commodity
 *  that has been added.
 *
 *  @param path The path to the newly added item.
 */
static void
gnc_tree_model_commodity_path_added (GncTreeModelCommodity *model,
				     GtkTreeIter *iter)
{
  gnc_commodity_namespace *namespace;
  GtkTreePath *path;
  GtkTreeIter ns_iter;
  GList *list;

  ENTER("model %p, iter (%p)%s", model, iter, iter_to_string(iter));

  if (iter->user_data == ITER_IS_COMMODITY) {
    /* Reach out and touch the namespace first */
    gnc_tree_model_commodity_iter_parent (GTK_TREE_MODEL(model), &ns_iter, iter);
    namespace = gnc_tree_model_commodity_get_namespace (model, &ns_iter);
    list = gnc_commodity_namespace_get_commodity_list(namespace);
    if (g_list_length(list) == 1) {
      path = gnc_tree_model_commodity_get_path (GTK_TREE_MODEL(model), &ns_iter);
      gtk_tree_model_row_changed(GTK_TREE_MODEL(model), path, &ns_iter);
      gtk_tree_model_row_has_child_toggled(GTK_TREE_MODEL(model), path, &ns_iter);
      gtk_tree_path_free(path);
    }
  }

  /* Now for either namespace or commodity */
  path = gnc_tree_model_commodity_get_path (GTK_TREE_MODEL(model), iter);
  gtk_tree_model_row_inserted (GTK_TREE_MODEL(model), path, iter);
  gtk_tree_path_free(path);

  do {
    model->stamp++;
  } while (model->stamp == 0);
  LEAVE(" ");
}


/** This function performs common updating to the model after an
 *  commodity has been removed.  The parent entry needs to be tapped
 *  on the shoulder so that it can correctly update the disclosure
 *  triangle (last removed child) or possibly rebuild its child list
 *  of that level of accounts is visible.
 *
 *  @internal
 *
 *  @param model The commodity tree model containing the commodity
 *  that has been added or deleted.
 *
 *  @param path The path to the newly added item, or the just removed
 *  item.
 */
static void
gnc_tree_model_commodity_path_deleted (GncTreeModelCommodity *model,
				       GtkTreePath *path)
{
  gnc_commodity_namespace *namespace;
  GtkTreeIter iter;
  GList *list;
  gint depth;

  debug_path(ENTER, path);

  depth = gtk_tree_path_get_depth(path);
  if (depth == 2) {
    /* It seems sufficient to tell the model that the parent row
     * changed. This appears to force a reload of all its child rows,
     * which handles removing the now gone commodity. */
    if (gtk_tree_path_up (path)) {
      gnc_tree_model_commodity_get_iter (GTK_TREE_MODEL(model), &iter, path);
      debug_path(DEBUG, path);
      DEBUG("iter %s", iter_to_string(&iter));
      gtk_tree_model_row_changed (GTK_TREE_MODEL(model), path, &iter);
      namespace = gnc_tree_model_commodity_get_namespace (model, &iter);
      if (namespace) {
           list = gnc_commodity_namespace_get_commodity_list(namespace);
           if (g_list_length(list) == 0) {
                gtk_tree_model_row_has_child_toggled(GTK_TREE_MODEL(model), path, &iter);
           }
      }
    }
  }

  do {
    model->stamp++;
  } while (model->stamp == 0);
  LEAVE(" ");
}


/** This function is a one-shot helper routine for the following
 *  gnc_tree_model_price_event_handler() function.  It must be armed
 *  each time an item is removed from the model.  This function will
 *  be called as an idle function some time after the user requests
 *  the deleteion.  (Most likely when the event handler for the "OK"
 *  button click returns.  This function will send the "row_deleted"
 *  signal to any/all parent models for each entry deleted.
 *
 *  @internal
 *
 *  @param unused
 *
 *  @return FALSE.  Tells the glib idle function to remove this
 *  handler, making it a one-shot that will be re-armed at the next
 *  item removal.
 */
static gboolean
gnc_tree_model_commodity_do_deletions (gpointer unused)
{
  GSList *iter, *next = NULL;
  remove_data *data;

  for (iter = pending_removals; iter != NULL; iter = next) {
    next = g_slist_next(iter);
    data = iter->data;
    pending_removals = g_slist_delete_link (pending_removals, iter);

    gtk_tree_model_row_deleted (GTK_TREE_MODEL(data->model), data->path);
    gnc_tree_model_commodity_path_deleted (data->model, data->path);
    gtk_tree_path_free(data->path);
    g_free(data);
  }

  /* Remove me */
  return FALSE;
}


/** This function is the handler for all event messages from the
 *  engine.  Its purpose is to update the commodity tree model any
 *  time a commodity or namespace is added to the engine or deleted
 *  from the engine.  This change to the model is then propagated to
 *  any/all overlying filters and views.  This function listens to the
 *  ADD, REMOVE, and DESTROY events.
 *
 *  @internal
 *
 *  @warning There is a "Catch 22" situation here.
 *  gtk_tree_model_row_deleted() can't be called until after the item
 *  has been deleted from the real model (which is the engine's
 *  commodity table for us), but once the commodity has been deleted
 *  from the engine we have no way to determine the path to pass to
 *  row_deleted().  This is a PITA, but the only ither choice is to
 *  have this model mirror the engine's commodity table instead of
 *  referencing it directly.
 *
 *  @param entity The affected item.
 *
 *  @param event type The type of the event. This function only cares
 *  about items of type ADD, REMOVE, and DESTROY.
 *
 *  @param user_data A pointer to the account tree model.
 *
 *  @param event_data A pointer to additional data about this event.
 */
static void
gnc_tree_model_commodity_event_handler (QofEntity *entity,
					QofEventId event_type,
					gpointer user_data,
					gpointer event_data)
{
  	GncTreeModelCommodity *model;
	GtkTreePath *path;
	GtkTreeIter iter;
	remove_data *data;
	const gchar *name;

	model = (GncTreeModelCommodity *)user_data;

	/* hard failures */
	g_return_if_fail(GNC_IS_TREE_MODEL_COMMODITY(model));

	ENTER("entity %p, event %d, model %p, event data %p",
	      entity, event_type, user_data, event_data);

	/* get type specific data */
	if (GNC_IS_COMMODITY(entity)) {
	  gnc_commodity *commodity;

	  commodity = GNC_COMMODITY(entity);
	  name = gnc_commodity_get_mnemonic(commodity);
	  if (event_type != QOF_EVENT_DESTROY) {
	    if (!gnc_tree_model_commodity_get_iter_from_commodity (model, commodity, &iter)) {
	      LEAVE("no iter");
	      return;
	    }
	  }
	} else if (GNC_IS_COMMODITY_NAMESPACE(entity)) {
	  gnc_commodity_namespace *namespace;

	  namespace = GNC_COMMODITY_NAMESPACE(entity);
	  name = gnc_commodity_namespace_get_name(namespace);
	  if (event_type != QOF_EVENT_DESTROY) {
	    if (!gnc_tree_model_commodity_get_iter_from_namespace (model, namespace, &iter)) {
	      LEAVE("no iter");
	      return;
	    }
	  }
	} else {
      LEAVE("");
	  return;
	}

	switch (event_type) {
	 case QOF_EVENT_ADD:
	  /* Tell the filters/views where the new account was added. */
	  DEBUG("add %s", name);
	  gnc_tree_model_commodity_path_added (model, &iter);
	  break;

	 case QOF_EVENT_REMOVE:
	  /* Record the path of this account for later use in destruction */
	  DEBUG("remove %s", name);
	  path = gtk_tree_model_get_path (GTK_TREE_MODEL(model), &iter);
	  if (path == NULL) {
	    LEAVE("not in model");
	    return;
	  }

	  data = g_new0 (remove_data, 1);
	  data->model = model;
	  data->path = path;
	  pending_removals = g_slist_append (pending_removals, data);
	  g_idle_add_full(G_PRIORITY_HIGH_IDLE,
			  gnc_tree_model_commodity_do_deletions, NULL, NULL);
	  LEAVE(" ");
	  return;

	 case QOF_EVENT_MODIFY:
	  DEBUG("change %s", name);
	  path = gtk_tree_model_get_path (GTK_TREE_MODEL(model), &iter);
	  if (path == NULL) {
	    LEAVE("not in model");
	    return;
	  }
	  gtk_tree_model_row_changed(GTK_TREE_MODEL(model), path, &iter);
	  gtk_tree_path_free(path);
	  LEAVE(" ");
	  return;

	 default:
	  LEAVE("ignored event for %s", name);
	  return;
	}
	LEAVE(" new stamp %u", model->stamp);
}
