/********************************************************************\
 * gnc-tree-view-commodity.c -- GtkTreeView implementation to       *
 *                            display commodities in a GtkTreeView. *
 * Copyright (C) 2003,2005 David Hampton <hampton@employees.org>    *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <string.h>

#include "gnc-tree-view.h"
#include "gnc-tree-model-commodity.h"
#include "gnc-tree-view-commodity.h"

#include "gnc-commodity.h"
#include "gnc-component-manager.h"
#include "gnc-engine.h"
#include "gnc-gconf-utils.h"
#include "gnc-glib-utils.h"
#include "gnc-gnome-utils.h"
#include "gnc-icons.h"
#include "gnc-ui-util.h"


/** Static Globals *******************************************************/

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

/** Declarations *********************************************************/
static void gnc_tree_view_commodity_class_init (GncTreeViewCommodityClass *klass);
static void gnc_tree_view_commodity_init (GncTreeViewCommodity *view);
static void gnc_tree_view_commodity_finalize (GObject *object);
static void gnc_tree_view_commodity_destroy (GtkObject *object);

typedef struct GncTreeViewCommodityPrivate
{
  gpointer dummy;
} GncTreeViewCommodityPrivate;

#define GNC_TREE_VIEW_COMMODITY_GET_PRIVATE(o)  \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_TREE_VIEW_COMMODITY, GncTreeViewCommodityPrivate))


/************************************************************/
/*               g_object required functions                */
/************************************************************/

static GObjectClass *parent_class = NULL;

GType
gnc_tree_view_commodity_get_type (void)
{
	static GType gnc_tree_view_commodity_type = 0;

	if (gnc_tree_view_commodity_type == 0) {
		static const GTypeInfo our_info = {
			sizeof (GncTreeViewCommodityClass),
			NULL,
			NULL,
			(GClassInitFunc) gnc_tree_view_commodity_class_init,
			NULL,
			NULL,
			sizeof (GncTreeViewCommodity),
			0,
			(GInstanceInitFunc) gnc_tree_view_commodity_init
		};
		
		gnc_tree_view_commodity_type = g_type_register_static (GNC_TYPE_TREE_VIEW,
								     "GncTreeViewCommodity",
								     &our_info, 0);
	}

	return gnc_tree_view_commodity_type;
}

static void
gnc_tree_view_commodity_class_init (GncTreeViewCommodityClass *klass)
{
	GObjectClass *o_class;
	GtkObjectClass *object_class;

	parent_class = g_type_class_peek_parent (klass);

	o_class = G_OBJECT_CLASS (klass);
	object_class = GTK_OBJECT_CLASS (klass);

	/* GObject signals */
	o_class->finalize = gnc_tree_view_commodity_finalize;

	/* GtkObject signals */
	object_class->destroy = gnc_tree_view_commodity_destroy;

	g_type_class_add_private(klass, sizeof(GncTreeViewCommodityPrivate));
}

static void
gnc_tree_view_commodity_init (GncTreeViewCommodity *view)
{
}

static void
gnc_tree_view_commodity_finalize (GObject *object)
{
  GncTreeViewCommodity *view;
  GncTreeViewCommodityPrivate *priv;

  g_return_if_fail (object != NULL);
  g_return_if_fail (GNC_IS_TREE_VIEW_COMMODITY (object));

  ENTER("view %p", object);
  view = GNC_TREE_VIEW_COMMODITY (object);
  priv = GNC_TREE_VIEW_COMMODITY_GET_PRIVATE (view);

  if (G_OBJECT_CLASS (parent_class)->finalize)
    (* G_OBJECT_CLASS (parent_class)->finalize) (object);
  LEAVE(" ");
}

static void
gnc_tree_view_commodity_destroy (GtkObject *object)
{
  GncTreeViewCommodity *view;

  g_return_if_fail (object != NULL);
  g_return_if_fail (GNC_IS_TREE_VIEW_COMMODITY (object));

  ENTER("view %p", object);
  view = GNC_TREE_VIEW_COMMODITY (object);

  if (GTK_OBJECT_CLASS (parent_class)->destroy)
    (* GTK_OBJECT_CLASS (parent_class)->destroy) (object);
  LEAVE(" ");
}


/************************************************************/
/*                      sort functions                      */
/************************************************************/

static gboolean
get_commodities_w_iters (GtkTreeModel *f_model,
			 GtkTreeIter *f_iter_a,
			 GtkTreeIter *f_iter_b,
			 GtkTreeModel **model_p,
			 GtkTreeIter *iter_a,
			 GtkTreeIter *iter_b,
			 gnc_commodity **comm_a,
			 gnc_commodity **comm_b)
{
  GncTreeModelCommodity *model;
  GtkTreeModel *tree_model;

  tree_model = gtk_tree_model_filter_get_model(GTK_TREE_MODEL_FILTER(f_model));
  model = GNC_TREE_MODEL_COMMODITY(tree_model);

  gtk_tree_model_filter_convert_iter_to_child_iter (GTK_TREE_MODEL_FILTER(f_model),
						    iter_a,
						    f_iter_a);

  gtk_tree_model_filter_convert_iter_to_child_iter (GTK_TREE_MODEL_FILTER(f_model),
						    iter_b,
						    f_iter_b);

  /* Both iters must point to commodities for this to be meaningful */
  if (!gnc_tree_model_commodity_iter_is_commodity (model, iter_a))
    return FALSE;
  if (!gnc_tree_model_commodity_iter_is_commodity (model, iter_b))
    return FALSE;

  *comm_a = gnc_tree_model_commodity_get_commodity (model, iter_a);
  *comm_b = gnc_tree_model_commodity_get_commodity (model, iter_b);
  if (model_p)
    *model_p = tree_model;
  return TRUE;
}

static gboolean
get_commodities (GtkTreeModel *f_model,
		 GtkTreeIter *f_iter_a,
		 GtkTreeIter *f_iter_b,
		 GtkTreeModel **model_p,
		 gnc_commodity **comm_a,
		 gnc_commodity **comm_b)
{
  GtkTreeIter iter_a, iter_b;

  return get_commodities_w_iters(f_model, f_iter_a, f_iter_b, model_p,
				 &iter_a, &iter_b, comm_a, comm_b);
}

static gint
sort_namespace (GtkTreeModel *f_model,
		GtkTreeIter *f_iter_a,
		GtkTreeIter *f_iter_b)
{
  GncTreeModelCommodity *model;
  GtkTreeModel *tree_model;
  GtkTreeIter iter_a, iter_b;
  gnc_commodity_namespace *ns_a, *ns_b;

  tree_model = gtk_tree_model_filter_get_model(GTK_TREE_MODEL_FILTER(f_model));
  model = GNC_TREE_MODEL_COMMODITY(tree_model);

  gtk_tree_model_filter_convert_iter_to_child_iter (GTK_TREE_MODEL_FILTER(f_model),
						    &iter_a,
						    f_iter_a);
  gtk_tree_model_filter_convert_iter_to_child_iter (GTK_TREE_MODEL_FILTER(f_model),
						    &iter_b,
						    f_iter_b);

  ns_a = gnc_tree_model_commodity_get_namespace (model, &iter_a);
  ns_b = gnc_tree_model_commodity_get_namespace (model, &iter_b);
  return safe_utf8_collate (gnc_commodity_namespace_get_name (ns_a),
			    gnc_commodity_namespace_get_name (ns_b));
}

static gint
default_sort (gnc_commodity *comm_a, gnc_commodity *comm_b)
{
  gint fraction_a, fraction_b, result;

  result = safe_utf8_collate (gnc_commodity_get_namespace (comm_a),
			      gnc_commodity_get_namespace (comm_b));
  if (result != 0) return result;

  result = safe_utf8_collate (gnc_commodity_get_mnemonic (comm_a),
			      gnc_commodity_get_mnemonic (comm_b));
  if (result != 0) return result;

  result = safe_utf8_collate (gnc_commodity_get_fullname (comm_a),
			      gnc_commodity_get_fullname (comm_b));
  if (result != 0) return result;

  result = safe_utf8_collate (gnc_commodity_get_cusip (comm_a),
			      gnc_commodity_get_cusip (comm_b));
  if (result != 0) return result;

  fraction_a = gnc_commodity_get_fraction (comm_a);
  fraction_b = gnc_commodity_get_fraction (comm_b);

  if (fraction_a < fraction_b)
    return -1;

  if (fraction_b < fraction_a)
    return 1;

  return 0;
}

static gint
sort_by_commodity_string (GtkTreeModel *f_model,
			  GtkTreeIter *f_iter_a,
			  GtkTreeIter *f_iter_b,
			  gpointer user_data)
{
  GtkTreeModel *model;
  GtkTreeIter iter_a, iter_b;
  gnc_commodity *comm_a, *comm_b;
  gchar *str1, *str2;
  gint column = GPOINTER_TO_INT(user_data);
  gint result;

  if (!get_commodities_w_iters(f_model, f_iter_a, f_iter_b,
			       &model, &iter_a, &iter_b, &comm_a, &comm_b))
    return sort_namespace (f_model, f_iter_a, f_iter_b);

  /* Get the strings. */
  gtk_tree_model_get(GTK_TREE_MODEL(model), &iter_a,  column, &str1, -1);
  gtk_tree_model_get(GTK_TREE_MODEL(model), &iter_b,  column, &str2, -1);

  result = safe_utf8_collate(str1, str2);
  g_free(str1);
  g_free(str2);
  if (result != 0)
    return result;
  return default_sort(comm_a, comm_b);
}


static gint
sort_by_fraction (GtkTreeModel *f_model,
		  GtkTreeIter *f_iter_a,
		  GtkTreeIter *f_iter_b,
		  gpointer user_data)
{
  gnc_commodity *comm_a, *comm_b;
  gint fraction_a, fraction_b;

  if (!get_commodities (f_model, f_iter_a, f_iter_b, NULL, &comm_a, &comm_b))
    return sort_namespace (f_model, f_iter_a, f_iter_b);

  fraction_a = gnc_commodity_get_fraction (comm_a);
  fraction_b = gnc_commodity_get_fraction (comm_b);

  if (fraction_a < fraction_b)
    return -1;

  if (fraction_b < fraction_a)
    return 1;

  return default_sort(comm_a, comm_b);
}

static gint
sort_by_quote_flag (GtkTreeModel *f_model,
		    GtkTreeIter *f_iter_a,
		    GtkTreeIter *f_iter_b,
		    gpointer user_data)
{
  gnc_commodity *comm_a, *comm_b;
  gboolean flag_a, flag_b;

  if (!get_commodities (f_model, f_iter_a, f_iter_b, NULL, &comm_a, &comm_b))
    return sort_namespace (f_model, f_iter_a, f_iter_b);

  flag_a = gnc_commodity_get_quote_flag(comm_a);
  flag_b = gnc_commodity_get_quote_flag(comm_b);

  if (flag_a < flag_b)
    return -1;
  else if (flag_a > flag_b)
    return 1;
  return default_sort(comm_a, comm_b);
}

/************************************************************/
/*                    New View Creation                     */
/************************************************************/

/*
 * Create a new commodity tree view with (optional) top level root node.
 * This view will be based on a model that is common to all view of
 * the same set of books, but will have its own private filter on that
 * model.
 */
GtkTreeView *
gnc_tree_view_commodity_new (QofBook *book,
			     const gchar *first_property_name,
			     ...)
{
  GncTreeView *view;
  GtkTreeModel *model, *f_model, *s_model;
  GtkTreeViewColumn *col;
  gnc_commodity_table *ct;
  va_list var_args;

  ENTER(" ");
  /* Create/get a pointer to the existing model for this set of books. */
  ct = gnc_book_get_commodity_table (book);
  model = gnc_tree_model_commodity_new (book, ct);

  /* Set up the view private filter on the common model. */
  f_model = gtk_tree_model_filter_new (model, NULL);
  g_object_unref(G_OBJECT(model));
  s_model = gtk_tree_model_sort_new_with_model (f_model);
  g_object_unref(G_OBJECT(f_model));

  /* Create our view */
  view = g_object_new (GNC_TYPE_TREE_VIEW_COMMODITY,
                       "name", "commodity_tree", NULL);
  gnc_tree_view_set_model (view, s_model);
  g_object_unref(G_OBJECT(s_model));

  DEBUG("model ref count is %d",   G_OBJECT(model)->ref_count);
  DEBUG("f_model ref count is %d", G_OBJECT(f_model)->ref_count);
  DEBUG("s_model ref count is %d", G_OBJECT(s_model)->ref_count);

  /* Set default visibilities */
  gtk_tree_view_set_headers_visible (GTK_TREE_VIEW(view), FALSE);

  col = gnc_tree_view_add_text_column (
      view, _("Namespace"), "namespace", NULL, "NASDAQ",
      GNC_TREE_MODEL_COMMODITY_COL_NAMESPACE,
      GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS,
      sort_by_commodity_string);
  col = gnc_tree_view_add_text_column (
      view, _("Symbol"), "symbol", NULL, "ACMEACME",
      GNC_TREE_MODEL_COMMODITY_COL_MNEMONIC,
      GNC_TREE_MODEL_COMMODITY_COL_VISIBILITY,
      sort_by_commodity_string);
  g_object_set_data(G_OBJECT(col), DEFAULT_VISIBLE, GINT_TO_POINTER(1));
  col = gnc_tree_view_add_text_column (
      view, _("Name"), "name", NULL, "Acme Corporation, Inc.",
      GNC_TREE_MODEL_COMMODITY_COL_FULLNAME,
      GNC_TREE_MODEL_COMMODITY_COL_VISIBILITY,
      sort_by_commodity_string);
  g_object_set_data(G_OBJECT(col), DEFAULT_VISIBLE, GINT_TO_POINTER(1));
  col = gnc_tree_view_add_text_column (
      view, _("Print Name"), "printname", NULL,
      "ACMEACME (Acme Corporation, Inc.)",
      GNC_TREE_MODEL_COMMODITY_COL_PRINTNAME,
      GNC_TREE_MODEL_COMMODITY_COL_VISIBILITY,
      sort_by_commodity_string);
  col = gnc_tree_view_add_text_column (
      view, _("Unique Name"), "uniquename", NULL,
      "NASDAQ::ACMEACME", GNC_TREE_MODEL_COMMODITY_COL_UNIQUE_NAME,
      GNC_TREE_MODEL_COMMODITY_COL_VISIBILITY, 
      sort_by_commodity_string);
  col = gnc_tree_view_add_text_column (
      /* Translators: Again replace CUSIP by the name of your 
         National Securities Identifying Number. */
      view, _("ISIN/CUSIP"), "cusip_code", NULL, "QWERTYUIOP",
      GNC_TREE_MODEL_COMMODITY_COL_CUSIP,
      GNC_TREE_MODEL_COMMODITY_COL_VISIBILITY,
      sort_by_commodity_string);
  g_object_set_data(G_OBJECT(col), DEFAULT_VISIBLE, GINT_TO_POINTER(1));
  col = gnc_tree_view_add_numeric_column (
      view, _("Fraction"), "fraction", "10000",
      GNC_TREE_MODEL_COMMODITY_COL_FRACTION,
      GNC_TREE_VIEW_COLUMN_COLOR_NONE,
      GNC_TREE_MODEL_COMMODITY_COL_VISIBILITY,
      sort_by_fraction);
  g_object_set_data(G_OBJECT(col), DEFAULT_VISIBLE, GINT_TO_POINTER(1));
  col = gnc_tree_view_add_toggle_column(
      view, _("Get Quotes"),
      /* Translators: This string has a context prefix; the translation
         must only contain the part after the | character. */
      Q_("Column letter for 'Get Quotes'|Q"), "quote_flag",
      GNC_TREE_MODEL_COMMODITY_COL_QUOTE_FLAG,
      GNC_TREE_MODEL_COMMODITY_COL_VISIBILITY,
      sort_by_quote_flag,
      NULL);
  col = gnc_tree_view_add_text_column (
      view, _("Source"), "quote_source", NULL, "yahoo",
      GNC_TREE_MODEL_COMMODITY_COL_QUOTE_SOURCE,
      GNC_TREE_MODEL_COMMODITY_COL_VISIBILITY,
      sort_by_commodity_string);
  col = gnc_tree_view_add_text_column (
      view, _("Timezone"), "quote_timezone", NULL, "America/New_York",
      GNC_TREE_MODEL_COMMODITY_COL_QUOTE_TZ,
      GNC_TREE_MODEL_COMMODITY_COL_VISIBILITY,
      sort_by_commodity_string);
  g_object_set_data(G_OBJECT(col), DEFAULT_VISIBLE, GINT_TO_POINTER(1));

  gnc_tree_view_configure_columns(view);

  /* Set properties */
  va_start (var_args, first_property_name);
  g_object_set_valist (G_OBJECT(view), first_property_name, var_args);
  va_end (var_args);

  /* Sort on the name column by default. This allows for a consistent
   * sort if commodities are briefly removed and re-added. */
  if (!gtk_tree_sortable_get_sort_column_id(GTK_TREE_SORTABLE(s_model),
                                            NULL, NULL)) {
    gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(s_model),
                                         GNC_TREE_MODEL_COMMODITY_COL_FULLNAME,
                                         GTK_SORT_ASCENDING);
  }

  gtk_widget_show(GTK_WIDGET(view));
  LEAVE(" %p", view);
  return GTK_TREE_VIEW(view);
}

/************************************************************/
/*                   Auxiliary Functions                    */
/************************************************************/

#define debug_path(fn, path) {				\
    gchar *path_string = gtk_tree_path_to_string(path); \
    fn("tree path %s", path_string);			\
    g_free(path_string);				\
  }

#if 0
static GtkTreePath *
gnc_tree_view_commodity_get_path_from_commodity (GncTreeViewCommodity *view,
						 gnc_commodity *commodity)
{
  GtkTreeModel *model, *f_model, *s_model;
  GtkTreePath *path, *f_path, *s_path;

  ENTER("view %p, commodity %p (%s)", view, commodity, gnc_commodity_get_name(commodity));

  if (commodity == NULL) {
    LEAVE("no commodity");
    return NULL;
  }

  /* Reach down to the real model and get a path for this commodity */
  s_model = gtk_tree_view_get_model(GTK_TREE_VIEW(view));
  f_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(s_model));
  model = gtk_tree_model_filter_get_model(GTK_TREE_MODEL_FILTER(f_model));
  path = gnc_tree_model_commodity_get_path_from_commodity (GNC_TREE_MODEL_COMMODITY(model), commodity);
  if (path == NULL) {
    LEAVE("no path");
    return NULL;
  }

  /* convert back to a filtered path */
  f_path = gtk_tree_model_filter_convert_child_path_to_path (GTK_TREE_MODEL_FILTER (f_model), path);
  gtk_tree_path_free(path);
  if (!f_path) {
    LEAVE("no filter path");
    return NULL;
  }

  /* convert back to a sorted path */
  s_path = gtk_tree_model_filter_convert_child_path_to_path (GTK_TREE_MODEL_SORT (s_model), f_path);
  gtk_tree_path_free(f_path);
  debug_path(LEAVE, s_path);
  return s_path;
}
#endif

static gboolean
gnc_tree_view_commodity_get_iter_from_commodity (GncTreeViewCommodity *view,
						 gnc_commodity *commodity,
						 GtkTreeIter *s_iter)
{
  GtkTreeModel *model, *f_model, *s_model;
  GtkTreeIter iter, f_iter;

  g_return_val_if_fail(GNC_IS_TREE_VIEW_COMMODITY(view), FALSE);
  g_return_val_if_fail(commodity != NULL, FALSE);
  g_return_val_if_fail(s_iter != NULL, FALSE);
  
  ENTER("view %p, commodity %p (%s)", view, commodity, gnc_commodity_get_mnemonic(commodity));

  /* Reach down to the real model and get an iter for this commodity */
  s_model = gtk_tree_view_get_model(GTK_TREE_VIEW(view));
  f_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(s_model));
  model = gtk_tree_model_filter_get_model(GTK_TREE_MODEL_FILTER(f_model));
  if (!gnc_tree_model_commodity_get_iter_from_commodity (GNC_TREE_MODEL_COMMODITY(model), commodity, &iter)) {
    LEAVE("model_get_iter_from_commodity failed");
    return FALSE;
  }

  /* convert back to a sort iter */
  gtk_tree_model_filter_convert_child_iter_to_iter (GTK_TREE_MODEL_FILTER(f_model),
						    &f_iter, &iter);
  gtk_tree_model_sort_convert_child_iter_to_iter (GTK_TREE_MODEL_SORT(s_model),
						    s_iter, &f_iter);
  LEAVE(" ");
  return TRUE;
}

gint
gnc_tree_view_commodity_count_children (GncTreeViewCommodity *view,
					gnc_commodity *commodity)
{
  GtkTreeModel *s_model;
  GtkTreeIter s_iter;
  gint num_children;

  ENTER("view %p, commodity %p (%s)", view, commodity, gnc_commodity_get_mnemonic(commodity));

  if (commodity == NULL) {
    LEAVE("no commodity");
    return 0;
  }

  if (!gnc_tree_view_commodity_get_iter_from_commodity (view, commodity, &s_iter)) {
    LEAVE("view_get_iter_from_commodity failed");
    return 0;
  }

  /* Any children? */
  s_model = gtk_tree_view_get_model(GTK_TREE_VIEW(view));
  num_children = gtk_tree_model_iter_n_children(s_model, &s_iter);
  LEAVE("%d children", num_children);
  return num_children;
}


gnc_commodity *
gnc_tree_view_commodity_get_commodity_from_column (GtkTreeViewColumn *column,
						   GtkTreeModel *s_model,
						   GtkTreeIter  *s_iter)
{
  GtkTreeModel *model, *f_model;
  GtkTreeIter iter, f_iter;
  gnc_commodity *commodity;

  g_return_val_if_fail (GTK_IS_TREE_VIEW_COLUMN(column), NULL);
  g_return_val_if_fail (GTK_IS_TREE_MODEL_SORT(s_model), NULL);
  g_return_val_if_fail (s_iter != NULL, NULL);

  ENTER("column %p, model %p, iter %p", column, s_model, s_iter);
  gtk_tree_model_sort_convert_iter_to_child_iter (GTK_TREE_MODEL_SORT(s_model),
						  &f_iter,
						  s_iter);
  f_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(s_model));
  gtk_tree_model_filter_convert_iter_to_child_iter (GTK_TREE_MODEL_FILTER(f_model),
						    &iter,
						    &f_iter);
  model = gtk_tree_model_filter_get_model(GTK_TREE_MODEL_FILTER(f_model));
  commodity = gnc_tree_model_commodity_get_commodity (GNC_TREE_MODEL_COMMODITY(model), &iter);
  LEAVE("commodity %p (%s)", commodity, gnc_commodity_get_mnemonic (commodity));
  return commodity;
}

/************************************************************/
/*            Commodity Tree View Filter Functions            */
/************************************************************/

/*
 * Set the list of columns that will be visible in an commodity tree view.
 */
void
gnc_tree_view_commodity_configure_columns (GncTreeViewCommodity *view,
					   GSList *column_names)
{
  GtkTreeViewColumn *column;
  GSList *node;
  int i;

  ENTER(" ");

  for (i = 1; i < GNC_TREE_MODEL_COMMODITY_NUM_COLUMNS; i++) {
    column = gtk_tree_view_get_column (GTK_TREE_VIEW(view), i);
    gtk_tree_view_column_set_visible (column, FALSE);
  }

  for (node = column_names; node != NULL; node = node->next)
  {
    for (i = 0; i < GNC_TREE_MODEL_COMMODITY_NUM_COLUMNS; i++) {
      column = gtk_tree_view_get_column (GTK_TREE_VIEW(view), i);
      gtk_tree_view_column_set_visible (column, TRUE);
    }
  }

  LEAVE(" ");
}

/************************************************************/
/*          Commodity Tree View Visibility Filter           */
/************************************************************/

typedef struct {
  gnc_tree_view_commodity_ns_filter_func user_ns_fn;
  gnc_tree_view_commodity_cm_filter_func user_cm_fn;
  gpointer                               user_data;
  GtkDestroyNotify                       user_destroy;
} filter_user_data;

static void
gnc_tree_view_commodity_filter_destroy (gpointer data)
{
  filter_user_data *fd = data;

  if (fd->user_destroy)
    fd->user_destroy(fd->user_data);
  g_free(fd);
}

static gboolean
gnc_tree_view_commodity_filter_helper (GtkTreeModel *model,
				     GtkTreeIter *iter,
				     gpointer data)
{
  gnc_commodity_namespace *namespace;
  gnc_commodity *commodity;
  filter_user_data *fd = data;

  g_return_val_if_fail (GNC_IS_TREE_MODEL_COMMODITY (model), FALSE);
  g_return_val_if_fail (iter != NULL, FALSE);

  if (gnc_tree_model_commodity_iter_is_namespace (GNC_TREE_MODEL_COMMODITY(model), iter)) {
    if (fd->user_ns_fn) {
      namespace = gnc_tree_model_commodity_get_namespace (GNC_TREE_MODEL_COMMODITY(model), iter);
      return fd->user_ns_fn(namespace, fd->user_data);
    }
    return TRUE;
  }

  if (gnc_tree_model_commodity_iter_is_commodity (GNC_TREE_MODEL_COMMODITY(model), iter)) {
    if (fd->user_cm_fn) {
      commodity = gnc_tree_model_commodity_get_commodity (GNC_TREE_MODEL_COMMODITY(model), iter);
      return fd->user_cm_fn(commodity, fd->user_data);
    }
    return TRUE;
  }

  return FALSE;
}

/*
 * Set an GtkTreeModel visible filter on this commodity.  This filter will be
 * called for each commodity that the tree is about to show, and the
 * commodity will be passed to the callback function.
 */
void
gnc_tree_view_commodity_set_filter (GncTreeViewCommodity *view,
				    gnc_tree_view_commodity_ns_filter_func ns_func,
				    gnc_tree_view_commodity_cm_filter_func cm_func,
				    gpointer data,
				    GtkDestroyNotify destroy)
{
  GtkTreeModel *f_model, *s_model;
  filter_user_data *fd = data;

  g_return_if_fail(GNC_IS_TREE_VIEW_COMMODITY(view));
  g_return_if_fail((ns_func != NULL) || (cm_func != NULL));

  ENTER("view %p, ns func %p, cm func %p, data %p, destroy %p",
	view, ns_func, cm_func, data, destroy);

  fd = g_malloc(sizeof(filter_user_data));
  fd->user_ns_fn   = ns_func;
  fd->user_cm_fn   = cm_func;
  fd->user_data    = data;
  fd->user_destroy = destroy;

  s_model = gtk_tree_view_get_model(GTK_TREE_VIEW(view));
  f_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(s_model));
  gtk_tree_model_filter_set_visible_func (GTK_TREE_MODEL_FILTER (f_model),
					  gnc_tree_view_commodity_filter_helper,
					  fd,
					  gnc_tree_view_commodity_filter_destroy);

  /* Whack any existing levels. The top two levels have been created
   * before this routine can be called. */
  gtk_tree_model_filter_refilter (GTK_TREE_MODEL_FILTER (f_model));
  LEAVE(" ");
}

/*
 * Forces the entire commodity tree to be re-evaluated for visibility.
 */
void
gnc_tree_view_commodity_refilter (GncTreeViewCommodity *view)
{
  GtkTreeModel *f_model, *s_model;

  g_return_if_fail(GNC_IS_TREE_VIEW_COMMODITY(view));

  ENTER("view %p", view);
  s_model = gtk_tree_view_get_model (GTK_TREE_VIEW(view));
  f_model = gtk_tree_model_sort_get_model (GTK_TREE_MODEL_SORT (s_model));
  gtk_tree_model_filter_refilter (GTK_TREE_MODEL_FILTER (f_model));
  LEAVE(" ");
}


/************************************************************/
/*           Commodity Tree View Get/Set Functions            */
/************************************************************/

/*
 * Retrieve the selected commodity from an commodity tree view.  The
 * commodity tree must be in single selection mode.
 */
gnc_commodity *
gnc_tree_view_commodity_get_commodity_from_path (GncTreeViewCommodity *view,
						 GtkTreePath *s_path)
{
    GtkTreeModel *model, *f_model, *s_model;
    GtkTreePath *path, *f_path;
    GtkTreeIter iter;
    gnc_commodity *commodity;

    g_return_val_if_fail (GNC_IS_TREE_VIEW_COMMODITY (view), NULL);
    g_return_val_if_fail (s_path != NULL, NULL);

    ENTER("view %p", view);
    s_model = gtk_tree_view_get_model(GTK_TREE_VIEW(view));
    f_path = gtk_tree_model_sort_convert_path_to_child_path (GTK_TREE_MODEL_SORT (s_model), s_path);
    if (!f_path) {
      LEAVE("no filter path");
      return NULL;
    }

    f_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(s_model));
    path = gtk_tree_model_filter_convert_path_to_child_path (GTK_TREE_MODEL_FILTER (f_model), f_path);
    gtk_tree_path_free(f_path);
    if (!path) {
      LEAVE("no path");
      return NULL;
    }

    model = gtk_tree_model_filter_get_model(GTK_TREE_MODEL_FILTER(f_model));
    if (!gtk_tree_model_get_iter (model, &iter, path)) {
      LEAVE("no iter");
      return NULL;
    }

    commodity = gnc_tree_model_commodity_get_commodity (GNC_TREE_MODEL_COMMODITY(model),
							&iter);
    gtk_tree_path_free(path);
    LEAVE("commodity %p (%s)", commodity, gnc_commodity_get_mnemonic (commodity));
    return commodity;
}

/*
 * Retrieve the selected commodity from an commodity tree view.  The
 * commodity tree must be in single selection mode.
 */
gnc_commodity *
gnc_tree_view_commodity_get_selected_commodity (GncTreeViewCommodity *view)
{
    GtkTreeSelection *selection;
    GtkTreeModel *model, *f_model, *s_model;
    GtkTreeIter iter, f_iter, s_iter;
    gnc_commodity *commodity;

    g_return_val_if_fail (GNC_IS_TREE_VIEW_COMMODITY (view), NULL);

    ENTER("view %p", view);

    selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(view));
    if (!gtk_tree_selection_get_selected (selection, &s_model, &s_iter)) {
      LEAVE("no commodity, get_selected failed");
      return FALSE;
    }

    gtk_tree_model_sort_convert_iter_to_child_iter (GTK_TREE_MODEL_SORT (s_model),
						    &f_iter, &s_iter);

    f_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(s_model));
    gtk_tree_model_filter_convert_iter_to_child_iter (GTK_TREE_MODEL_FILTER (f_model),
						      &iter, &f_iter);

    model = gtk_tree_model_filter_get_model(GTK_TREE_MODEL_FILTER(f_model));
    commodity = gnc_tree_model_commodity_get_commodity (GNC_TREE_MODEL_COMMODITY(model),
							&iter);
    LEAVE("commodity %p (%s)", commodity,
          commodity? gnc_commodity_get_mnemonic(commodity) : "");
    return commodity;
}

/*
 * Selects a single commodity in the commodity tree view.  The commodity
 * tree must be in single selection mode.
 */
void
gnc_tree_view_commodity_set_selected_commodity (GncTreeViewCommodity *view,
						gnc_commodity *commodity)
{
  GtkTreeModel *model, *f_model, *s_model;
  GtkTreePath *path, *f_path, *s_path, *parent_path;
  GtkTreeSelection *selection;

  ENTER("view %p, commodity %p (%s)", view,
	commodity, gnc_commodity_get_mnemonic (commodity));

  /* Clear any existing selection. */
  selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(view));
  gtk_tree_selection_unselect_all (selection);

  if (commodity == NULL)
  {
    LEAVE("");
    return;
  }

  s_model = gtk_tree_view_get_model(GTK_TREE_VIEW(view));
  f_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(s_model));
  model = gtk_tree_model_filter_get_model (GTK_TREE_MODEL_FILTER (f_model));

  path = gnc_tree_model_commodity_get_path_from_commodity (GNC_TREE_MODEL_COMMODITY(model), commodity);
  if (path == NULL) {
    LEAVE("get_path_from_commodity failed");
    return;
  }
  debug_path(DEBUG, path);

  f_path = gtk_tree_model_filter_convert_child_path_to_path (GTK_TREE_MODEL_FILTER (f_model),
							     path);
  gtk_tree_path_free(path);
  if (f_path == NULL) {
    LEAVE("no filter path");
    return;
  }
  debug_path(DEBUG, f_path);

  s_path = gtk_tree_model_sort_convert_child_path_to_path (GTK_TREE_MODEL_SORT (s_model),
							   f_path);
  gtk_tree_path_free(f_path);
  if (s_path == NULL) {
    LEAVE("no sort path");
    return;
  }

  /* gtk_tree_view requires that a row be visible before it can be selected */
  parent_path = gtk_tree_path_copy (s_path);
  if (gtk_tree_path_up (parent_path)) {
    /* This function is misnamed.  It expands the actual item
     * specified, not the path to the item specified. I.E. It expands
     * one level too many, thus the get of the parent. */
    gtk_tree_view_expand_to_path(GTK_TREE_VIEW(view), parent_path);
  }
  gtk_tree_path_free(parent_path);

  gtk_tree_selection_select_path (selection, s_path);
  gtk_tree_view_scroll_to_cell (GTK_TREE_VIEW(view), s_path, NULL, FALSE, 0.0, 0.0);
  debug_path(LEAVE, s_path);
  gtk_tree_path_free(s_path);
}

/*
 * This helper function is called once for each row in the tree view
 * that is currently selected.  Its task is to an the corresponding
 * commodity to the end of a glist.
 */
static void
get_selected_commodities_helper (GtkTreeModel *s_model,
				 GtkTreePath *s_path,
				 GtkTreeIter *s_iter,
				 gpointer data)
{
  GList **return_list = data;
  GtkTreeModel *model, *f_model;
  GtkTreeIter iter, f_iter;
  gnc_commodity *commodity;

  gtk_tree_model_sort_convert_iter_to_child_iter (GTK_TREE_MODEL_SORT (s_model),
						  &f_iter, s_iter);

  f_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(s_model));
  gtk_tree_model_filter_convert_iter_to_child_iter (GTK_TREE_MODEL_FILTER (f_model),
						    &iter, &f_iter);

  model = gtk_tree_model_filter_get_model(GTK_TREE_MODEL_FILTER(f_model));
  commodity = gnc_tree_model_commodity_get_commodity (GNC_TREE_MODEL_COMMODITY(model),
						      &iter);
  *return_list = g_list_append(*return_list, commodity);
}

/*
 * Given an commodity tree view, return a list of the selected commodities. The
 * commodity tree must be in multiple selection mode.
 *
 * Note: It is the responsibility of the caller to free the returned
 * list.
 */
GList *
gnc_tree_view_commodity_get_selected_commodities (GncTreeViewCommodity *view)
{
  GtkTreeSelection *selection;
  GList *return_list = NULL;

  selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(view));
  gtk_tree_selection_selected_foreach(selection, get_selected_commodities_helper, &return_list);
  return return_list;
}

/*
 * Given an commodity tree view and a list of commodities, select those
 * commodities in the tree view.
 */
void
gnc_tree_view_commodity_set_selected_commodities (GncTreeViewCommodity *view,
						  GList *commodity_list,
						  gboolean show_last)
{
  GtkTreeModel *model, *f_model, *s_model;
  GtkTreePath *path, *f_path, *s_path, *parent_path;
  GtkTreeSelection *selection;
  GList *element;
  gnc_commodity *commodity;

  s_model = gtk_tree_view_get_model(GTK_TREE_VIEW(view));
  f_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(s_model));
  model = gtk_tree_model_filter_get_model(GTK_TREE_MODEL_FILTER(f_model));

  /* Clear any existing selection. */
  selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(view));
  gtk_tree_selection_unselect_all (selection);
  gtk_tree_view_collapse_all (GTK_TREE_VIEW(view));

  /* Now go select what the user requested. */
  for (element = commodity_list; element; ) {
    commodity = element->data;
    element = g_list_next(element);

    path = gnc_tree_model_commodity_get_path_from_commodity (GNC_TREE_MODEL_COMMODITY(model), commodity);
    if (path == NULL) {
      /*
       * Oops.  Someone must have deleted this commodity and not cleaned
       * up all references to it.
       */
      continue;
    }

    f_path = gtk_tree_model_filter_convert_child_path_to_path (GTK_TREE_MODEL_FILTER (f_model),
							       path);
    gtk_tree_path_free(path);
    if (f_path == NULL)
      continue;

    s_path = gtk_tree_model_sort_convert_child_path_to_path (GTK_TREE_MODEL_SORT (s_model),
							     f_path);
    gtk_tree_path_free(f_path);
    if (s_path == NULL)
      continue;

    /* gtk_tree_view requires that a row be visible before it can be selected */
    parent_path = gtk_tree_path_copy (s_path);
    if (gtk_tree_path_up (parent_path)) {
      /* This function is misnamed.  It expands the actual item
       * specified, not the path to the item specified. I.E. It
       * expands one level too many, thus the get of the parent. */
      gtk_tree_view_expand_to_path(GTK_TREE_VIEW(view), parent_path);
    }
    gtk_tree_path_free(parent_path);

    gtk_tree_selection_select_path (selection, s_path);
    if (show_last && (element == NULL))
      gtk_tree_view_scroll_to_cell (GTK_TREE_VIEW(view), s_path, NULL, FALSE, 0.0, 0.0);
    gtk_tree_path_free(s_path);
  }
}

#ifdef NEEDED
/*
 * Retrieve the commodity currently under the cursor.
 */
gnc_commodity *
gnc_tree_view_commodity_get_cursor_account (GncTreeViewCommodity *view)
{
    GtkTreeModel *s_model;
    GtkTreePath *s_path;
    gnc_commodity *commodity;

    g_return_val_if_fail (GNC_IS_TREE_VIEW_COMMODITY (view), NULL);

    ENTER("view %p", view);
    s_model = gtk_tree_view_get_model (GTK_TREE_VIEW(view));
    gtk_tree_view_get_cursor (GTK_TREE_VIEW(view), &s_path, NULL);
    if (s_path) {
      account = gnc_tree_view_commodity_get_account_from_path (view, s_path);
      gtk_tree_path_free(s_path);
    } else {
      account = NULL;
    }
    LEAVE("account %p (%s)", account, gnc_commodity_get_mnemonic (account));
    return account;
}
#endif
