/********************************************************************\
 * gnc-budget-list-tree-model.c -- Implementation of the Budget List*
 *                                 Tree model.                      *
 * Copyright (C) 09 feb 2004    Darin Willits <darin@willits.ca>    *
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

/** @addtogroup GUI
 *     @{ */
/** @addtogroup Budget_Tree
 *     @{ */
/** @file gnc-budget-list-tree-model.c
 *  @brief Implementation of the Budget List Tree model.
 *  @author Created by Darin Willits 09 feb 2004 
 *  @author Copyright (c) 09 feb 2004 Darin Willits <darin@willits.ca>
 *
 */


// Includes
#include "config.h"
#include <string.h>
#include "gnc-trace.h"

#include "gnc-budget-list-tree-model.h"
#include "gnc-budget-book.h"
#include "gnc-engine-util.h"
#include "gnc-event.h"


static short module = MOD_GUI;

/** Initialize and clean-up functions. */
static void gnc_budget_list_tree_model_class_init (GncBudgetListTreeModelClass *klass);
static void gnc_budget_list_tree_model_init (GncBudgetListTreeModel *model);
static void gnc_budget_list_tree_model_finalize (GObject *object);
static void gnc_budget_list_tree_model_destroy (GtkObject *object);
static void gnc_budget_list_tree_model_gtk_tree_model_init (GtkTreeModelIface *iface);

/* GtkTreeModel Interface functions. */
static guint gnc_budget_list_tree_model_get_flags (GtkTreeModel *tree_model);
static int gnc_budget_list_tree_model_get_n_columns (GtkTreeModel *tree_model);
static GType gnc_budget_list_tree_model_get_column_type (GtkTreeModel *tree_model,
						     int index);
static gboolean gnc_budget_list_tree_model_get_iter (GtkTreeModel *tree_model,
						 GtkTreeIter *iter,
						 GtkTreePath *path);
static GtkTreePath *gnc_budget_list_tree_model_get_path (GtkTreeModel *tree_model,
						     GtkTreeIter *iter);
static void gnc_budget_list_tree_model_get_value (GtkTreeModel *tree_model,
					      GtkTreeIter *iter,
					      int column,
					      GValue *value);
static gboolean	gnc_budget_list_tree_model_iter_next (GtkTreeModel *tree_model,
						  GtkTreeIter *iter);
static gboolean	gnc_budget_list_tree_model_iter_children (GtkTreeModel *tree_model,
						      GtkTreeIter *iter,
						      GtkTreeIter *parent);
static gboolean	gnc_budget_list_tree_model_iter_has_child (GtkTreeModel *tree_model,
						       GtkTreeIter *iter);
static int gnc_budget_list_tree_model_iter_n_children (GtkTreeModel *tree_model,
						   GtkTreeIter *iter);
static gboolean	gnc_budget_list_tree_model_iter_nth_child (GtkTreeModel *tree_model,
						       GtkTreeIter *iter,
						       GtkTreeIter *parent,
						       int n);
static gboolean	gnc_budget_list_tree_model_iter_parent (GtkTreeModel *tree_model,
						        GtkTreeIter *iter,
    						    GtkTreeIter *child);
static gboolean gnc_budget_list_tree_model_get_iter_from_budget (GncBudgetListTreeModel *model,
					      GncBudget *budget,
					      GtkTreeIter *iter);

static void gnc_budget_list_tree_model_event_handler (GUID *entity, QofIdType type,
						  GNCEngineEventType event_type,
						  gpointer user_data);

/* Private internal data structure. */
struct GncBudgetListTreeModelPrivate
{
    QofBook* book;
    GList* budgets;
	gint event_handler_id;
};

/************************************************************/
/*               g_object required functions                */
/************************************************************/

static GtkObjectClass *parent_class = NULL;

GType
gnc_budget_list_tree_model_get_type (void)
{
	static GType gnc_budget_list_tree_model_type = 0;

	if (gnc_budget_list_tree_model_type == 0) {
		static const GTypeInfo our_info = {
			sizeof (GncBudgetListTreeModelClass), /* class_size */
			NULL,   			   /* base_init */
			NULL,				   /* base_finalize */
			(GClassInitFunc) gnc_budget_list_tree_model_class_init,
			NULL,				   /* class_finalize */
			NULL,				   /* class_data */
			sizeof (GncBudgetListTreeModel),	   /* */
			0,				   /* n_preallocs */
			(GInstanceInitFunc) gnc_budget_list_tree_model_init
		};
		
		static const GInterfaceInfo budget_list_tree_model_info = {
			(GInterfaceInitFunc) gnc_budget_list_tree_model_gtk_tree_model_init,
			NULL,
			NULL
		};

		gnc_budget_list_tree_model_type = g_type_register_static (GTK_TYPE_OBJECT,
								      "GncBudgetListTreeModel",
								      &our_info, 0);
        
		g_type_add_interface_static (gnc_budget_list_tree_model_type,
					     GTK_TYPE_TREE_MODEL,
					     &budget_list_tree_model_info);
	}
    
	return gnc_budget_list_tree_model_type;
}

static void
gnc_budget_list_tree_model_class_init (GncBudgetListTreeModelClass *klass)
{
	GObjectClass *o_class;
	GtkObjectClass *object_class;

	parent_class = g_type_class_peek_parent (klass);

	o_class = G_OBJECT_CLASS (klass);
	object_class = GTK_OBJECT_CLASS (klass);

	/* GObject signals */
	o_class->finalize = gnc_budget_list_tree_model_finalize;

	/* GtkObject signals */
	object_class->destroy = gnc_budget_list_tree_model_destroy;
}

static void
gnc_budget_list_tree_model_init (GncBudgetListTreeModel *model)
{
	ENTER("model %p", model);
	while (model->stamp == 0) {
		model->stamp = g_random_int ();
	}

	model->priv = g_new0 (GncBudgetListTreeModelPrivate, 1);
	model->priv->book = NULL;
	LEAVE(" ");
}

static void
gnc_budget_list_tree_model_finalize (GObject *object)
{
	GncBudgetListTreeModel *model;

	ENTER("model %p", object);
	g_return_if_fail (object != NULL);
	g_return_if_fail (GNC_IS_BUDGET_LIST_TREE_MODEL (object));

	model = GNC_BUDGET_LIST_TREE_MODEL(object);
	g_free (model->priv);

	if (G_OBJECT_CLASS (parent_class)->finalize)
	  (* G_OBJECT_CLASS (parent_class)->finalize) (object);
	LEAVE(" ");
}

static void
gnc_budget_list_tree_model_destroy (GtkObject *object)
{
	GncBudgetListTreeModel *model;

	ENTER("model %p", object);
	g_return_if_fail (object != NULL);
	g_return_if_fail (GNC_IS_BUDGET_LIST_TREE_MODEL(object));

	model = GNC_BUDGET_LIST_TREE_MODEL(object);

	if (GTK_OBJECT_CLASS (parent_class)->destroy)
	  (* GTK_OBJECT_CLASS (parent_class)->destroy) (object);
	LEAVE(" ");
}

/******************************************************
 * GtkTreeModel Interface Methods.
 *
 * ****************************************************/

static void gnc_budget_list_tree_model_gtk_tree_model_init(GtkTreeModelIface *iface)
{
	iface->get_flags       = gnc_budget_list_tree_model_get_flags;
	iface->get_n_columns   = gnc_budget_list_tree_model_get_n_columns;
	iface->get_column_type = gnc_budget_list_tree_model_get_column_type;
	iface->get_iter        = gnc_budget_list_tree_model_get_iter;
	iface->get_path        = gnc_budget_list_tree_model_get_path;
	iface->get_value       = gnc_budget_list_tree_model_get_value;
	iface->iter_next       = gnc_budget_list_tree_model_iter_next;
	iface->iter_children   = gnc_budget_list_tree_model_iter_children;
	iface->iter_has_child  = gnc_budget_list_tree_model_iter_has_child;
	iface->iter_n_children = gnc_budget_list_tree_model_iter_n_children;
	iface->iter_nth_child  = gnc_budget_list_tree_model_iter_nth_child;
	iface->iter_parent     = gnc_budget_list_tree_model_iter_parent;
}

static guint gnc_budget_list_tree_model_get_flags (GtkTreeModel *tree_model)
{
    return 0;
}

static int gnc_budget_list_tree_model_get_n_columns (GtkTreeModel *tree_model)
{
    return GNC_BUDGET_LIST_TREE_MODEL_NUM_COLUMNS;
}

static GType gnc_budget_list_tree_model_get_column_type (GtkTreeModel *tree_model, int index)
{
 	g_return_val_if_fail (GNC_IS_BUDGET_LIST_TREE_MODEL(tree_model), G_TYPE_INVALID);
    //g_return_val_if_fail ((index < GNC_BUDGET_TREE_MODEL_NUM_COLUMNS) && (index >= 0), G_TYPE_INVALID);

    return G_TYPE_STRING;
/*
	switch (index) {
        case GNC_BUDGET_TREE_MODEL_COL_NAME:
		case GNC_BUDGET_TREE_MODEL_COL_VALUE:
        case GNC_BUDGET_TREE_MODEL_COL_FREQ:
			return G_TYPE_STRING;
		default:
			g_assert_not_reached ();
			return G_TYPE_INVALID;
	}
*/
}

static gboolean gnc_budget_list_tree_model_get_iter (GtkTreeModel *tree_model, 
                                                GtkTreeIter *iter,
						                        GtkTreePath *path)
{
	GncBudgetListTreeModel *model = GNC_BUDGET_LIST_TREE_MODEL (tree_model);
	guint i;

	g_return_val_if_fail (GNC_IS_BUDGET_LIST_TREE_MODEL(model), FALSE);
	g_return_val_if_fail (gtk_tree_path_get_depth (path) > 0, FALSE);
	
	if (model->priv->budgets == NULL)
		return FALSE;

	i = gtk_tree_path_get_indices (path)[0];

	g_return_val_if_fail (i >= 0 && i < g_list_length (model->priv->budgets), FALSE);

	iter->stamp = model->stamp;
	iter->user_data = g_list_nth(model->priv->budgets, i);

	if (iter->user_data == NULL) {
		iter->stamp = 0;
		return FALSE;
	}

	return TRUE;
}

static GtkTreePath *gnc_budget_list_tree_model_get_path (GtkTreeModel *tree_model,
                        						     GtkTreeIter *iter)
{
    GncBudgetListTreeModel *model = GNC_BUDGET_LIST_TREE_MODEL(tree_model);
	GtkTreePath *path;

	g_return_val_if_fail (GNC_IS_BUDGET_LIST_TREE_MODEL(model), NULL);
	g_return_val_if_fail (iter != NULL, NULL);
	g_return_val_if_fail (iter->user_data != NULL, NULL);
	g_return_val_if_fail (iter->stamp == model->stamp, NULL);
	
	if (model->priv->budgets== NULL)
		return NULL;

	path = gtk_tree_path_new ();

	gtk_tree_path_append_index (path, g_list_position (model->priv->budgets, iter->user_data));

	return path;

}

static void gnc_budget_list_tree_model_get_value (GtkTreeModel *tree_model,
				                    	      GtkTreeIter *iter,
                					          int column,
					                          GValue *value)
{
	GncBudgetListTreeModel* model = GNC_BUDGET_LIST_TREE_MODEL(tree_model);
    GncBudget* budget;
	
    g_return_if_fail (GNC_IS_BUDGET_LIST_TREE_MODEL(model));
	g_return_if_fail (iter != NULL);
	g_return_if_fail (iter->user_data != NULL);
	g_return_if_fail (iter->stamp == model->stamp);

    budget = (GncBudget*)((GList*)iter->user_data)->data;
    
	switch (column) {
		case GNC_BUDGET_LIST_TREE_MODEL_COL_NAME:
			g_value_init (value, G_TYPE_STRING);
			g_value_set_string (value, gnc_budget_get_name(budget));
			break;
		case GNC_BUDGET_LIST_TREE_MODEL_COL_DESCRIPTION:
			g_value_init (value, G_TYPE_STRING);
			g_value_set_string(value, gnc_budget_get_description(budget));
			break;
		default:
			g_assert_not_reached ();
	}
}

static gboolean	gnc_budget_list_tree_model_iter_next (GtkTreeModel *tree_model, GtkTreeIter *iter)
{
 	GncBudgetListTreeModel *model = GNC_BUDGET_LIST_TREE_MODEL(tree_model);

	g_return_val_if_fail (GNC_IS_BUDGET_LIST_TREE_MODEL(model), FALSE);
	g_return_val_if_fail (iter != NULL, FALSE);
	g_return_val_if_fail (iter->user_data != NULL, FALSE);
	g_return_val_if_fail (iter->stamp == model->stamp, FALSE);

	if (g_list_next (iter->user_data) == NULL)
		return FALSE;

	iter->user_data = g_list_next(iter->user_data);

	return TRUE;   
}

static gboolean	gnc_budget_list_tree_model_iter_children (GtkTreeModel *tree_model,
                        						      GtkTreeIter *iter,
						                              GtkTreeIter *parent)
{
 	GncBudgetListTreeModel *model = GNC_BUDGET_LIST_TREE_MODEL(tree_model);
    g_return_val_if_fail (GNC_IS_BUDGET_LIST_TREE_MODEL(tree_model), FALSE);
    g_return_val_if_fail (iter != NULL, FALSE);

	if (parent != NULL){
		return FALSE;
    }
    
    iter->user_data = model->priv->budgets;
    iter->stamp = model->stamp;
        
	return TRUE;   
}

static gboolean	gnc_budget_list_tree_model_iter_has_child (GtkTreeModel *tree_model,
                        						       GtkTreeIter *iter)
{
	return FALSE;
}

static int gnc_budget_list_tree_model_iter_n_children (GtkTreeModel *tree_model,
                        						   GtkTreeIter *iter)
{
	g_return_val_if_fail (GNC_IS_BUDGET_LIST_TREE_MODEL(tree_model), FALSE);
	
    if (iter == NULL)
		return g_list_length (GNC_BUDGET_LIST_TREE_MODEL(tree_model)->priv->budgets);

    g_return_val_if_fail (GNC_BUDGET_LIST_TREE_MODEL(tree_model)->stamp == iter->stamp, -1);

	return 0;
}

static gboolean	gnc_budget_list_tree_model_iter_nth_child (GtkTreeModel *tree_model,
                        						        GtkTreeIter *iter,
						                                GtkTreeIter *parent,
						                                int n)
{
	GncBudgetListTreeModel *model;
	
	g_return_val_if_fail(GNC_IS_BUDGET_LIST_TREE_MODEL(tree_model), FALSE);
    g_return_val_if_fail (iter != NULL, FALSE);

	model = GNC_BUDGET_LIST_TREE_MODEL(tree_model);

	if (parent != NULL){
        return FALSE;
    }
    
	g_return_val_if_fail (n >= 0 && n < (int)g_list_length (model->priv->budgets), FALSE);

	iter->stamp = model->stamp;
	iter->user_data = g_list_nth (model->priv->budgets, n);


	if (iter->user_data == NULL) {
		iter->stamp = 0;
		return FALSE;
	}

	return TRUE;
}

static gboolean	gnc_budget_list_tree_model_iter_parent (GtkTreeModel *tree_model,
                        						    GtkTreeIter *iter,
    					                    	    GtkTreeIter *child)
{
    return FALSE;
}

/*
 * Convert a model/account pair into a gtk_tree_model_iter.  This
 * routine should only be called from the file
 * gnc-tree-view-account.c.
 */
gboolean
gnc_budget_list_tree_model_get_iter_from_budget (GncBudgetListTreeModel *model,
					      GncBudget *budget,
					      GtkTreeIter *iter)
{
    
	ENTER("model %p, budget %p, iter %p", model, budget, iter);
	g_return_val_if_fail (GNC_IS_BUDGET_LIST_TREE_MODEL(model), FALSE);
	g_return_val_if_fail ((budget != NULL), FALSE);
	g_return_val_if_fail ((iter != NULL), FALSE);
    
	if (model->priv->book != gnc_budget_get_book(budget)) {
		LEAVE("Books don't match");
		return FALSE;
	}

	iter->user_data = g_list_find(model->priv->budgets, budget);
	iter->stamp = model->stamp;

    return TRUE;
}




/*******************************************************
 * Public Interface.
 * 
 * *****************************************************/

GtkTreeModel* gnc_budget_list_tree_model_new(QofBook* book)
{
	GncBudgetListTreeModel *model;
	
	model = g_object_new (GNC_TYPE_BUDGET_LIST_TREE_MODEL, NULL);

	model->priv->book = book;
	model->priv->budgets = gnc_book_get_budgets(book);
	
    model->priv->event_handler_id =
	  gnc_engine_register_event_handler (gnc_budget_list_tree_model_event_handler, model);
	
	return GTK_TREE_MODEL (model);
}


void gnc_budget_list_tree_model_event_handler (GUID *entity, QofIdType type,
						  GNCEngineEventType event_type,
						  gpointer user_data)
{
   	GncBudgetListTreeModel *model;
	GtkTreePath *path;
	GtkTreeIter iter;
	GncBudget *budget;
	const gchar *budget_name;

	/* hard failures */
	g_return_if_fail(GNC_IS_BUDGET_LIST_TREE_MODEL(user_data));

	/* soft failures */
	if (safe_strcmp(type, GNC_ID_BUDGET) != 0)
	  return;

	ENTER("entity %p of type %s, event %d, model %p",
	      entity, type, event_type, user_data);
	model = (GncBudgetListTreeModel*)user_data;

	/* Get the account.*/
	/* DRH - Put the book in the model private data so this code
	 * supports multiple simultaneous books. */
	budget = gnc_budget_lookup(entity, model->priv->book);
    budget_name = gnc_budget_get_name(budget);
    	
    switch (event_type) {
	 case GNC_EVENT_ADD:
	  /* Tell the filters/views where the new account was added. */
	  DEBUG("add budget %p (%s)", budget, budget_name);

      /* We have to get the new list of budgets since it has now probably
       * changed. */
      model->priv->budgets = gnc_book_get_budgets(model->priv->book);
      
	  if (gnc_budget_list_tree_model_get_iter_from_budget (model, budget, &iter)) {
	    path = gtk_tree_model_get_path (GTK_TREE_MODEL(model), &iter);
	    gtk_tree_model_row_inserted (GTK_TREE_MODEL(model), path, &iter);
	    //gnc_tree_model_account_path_changed (model, path);
	    gtk_tree_path_free(path);
	  }
	  break;

	 case GNC_EVENT_REMOVE:
	  /* Record the path of this account for later use in destruction 
	  DEBUG("remove account %p (%s)", account, account_name);
	  path = gnc_tree_model_account_get_path_from_account (model, account);
	  if (path == NULL) {
	    LEAVE("account not in model");
	    return;
	  }

	  data = malloc(sizeof(*data));
	  data->guid = *entity;
	  data->model = model;
	  data->path = path;
	  pending_removals = g_slist_append (pending_removals, data);
	  LEAVE(" ");
      */
	  return;

	 case GNC_EVENT_MODIFY:
	  /*DEBUG("change account %p (%s)", account, account_name);
	  path = gnc_tree_model_account_get_path_from_account (model, account);
	  if (path == NULL) {
	    LEAVE("account not in model");
	    return;
	  }
	  if (!gtk_tree_model_get_iter (GTK_TREE_MODEL(model), &iter, path)) {
	    gtk_tree_path_free(path);
	    LEAVE("can't find iter for path");
	    return;
	  }
	  gtk_tree_model_row_changed(GTK_TREE_MODEL(model), path, &iter);
	  gtk_tree_path_free(path);
	  LEAVE(" ");*/
	  return;

	 case GNC_EVENT_DESTROY:
	  /* Tell the filters/view the account has been deleted. */
	  /*DEBUG("destroy account %p (%s)", account, account_name);
	  g_slist_foreach (pending_removals,
			   (GFunc)gnc_tree_model_account_delete_event_helper,
			   entity);*/
	  break;

	 default:
	  LEAVE("ignored event for %p (%s)", budget, budget_name);
	  return;
	}
	LEAVE(" new stamp %u", model->stamp);
    
}

/** @} */
/** @} */
