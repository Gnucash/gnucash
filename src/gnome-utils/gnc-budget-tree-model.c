/********************************************************************\
 * gnc-budget-tree-model.c -- Implementation of the budget tree     *
 *                            model.                                *
 * Copyright (C) 09 sep 2003    Darin Willits <darin@willits.ca>    *
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
/** @file gnc-budget-tree-model.c
 *  @brief Implementation of the budget tree model.
 *  @author Created by Darin Willits 09 sep 2003 
 *  @author Copyright (c) 09 sep 2003 Darin Willits <darin@willits.ca>
 *
 *  This should be fun...
 */

// Includes
#include "config.h"
#include <string.h>
#include "gnc-trace.h"

#include "gnc-budget-tree-model.h"
#include "gnc-budget-cat.h"

static short module = MOD_GUI;

/** Initialize and clean-up functions. */
static void gnc_budget_tree_model_class_init (GncBudgetTreeModelClass *klass);
static void gnc_budget_tree_model_init (GncBudgetTreeModel *model);
static void gnc_budget_tree_model_finalize (GObject *object);
static void gnc_budget_tree_model_destroy (GtkObject *object);
static void gnc_budget_tree_model_gtk_tree_model_init (GtkTreeModelIface *iface);

/* GtkTreeModel Interface functions. */
static guint gnc_budget_tree_model_get_flags (GtkTreeModel *tree_model);
static int gnc_budget_tree_model_get_n_columns (GtkTreeModel *tree_model);
static GType gnc_budget_tree_model_get_column_type (GtkTreeModel *tree_model,
						     int index);
static gboolean gnc_budget_tree_model_get_iter (GtkTreeModel *tree_model,
						 GtkTreeIter *iter,
						 GtkTreePath *path);
static GtkTreePath *gnc_budget_tree_model_get_path (GtkTreeModel *tree_model,
						     GtkTreeIter *iter);
static void gnc_budget_tree_model_get_value (GtkTreeModel *tree_model,
					      GtkTreeIter *iter,
					      int column,
					      GValue *value);
static gboolean	gnc_budget_tree_model_iter_next (GtkTreeModel *tree_model,
						  GtkTreeIter *iter);
static gboolean	gnc_budget_tree_model_iter_children (GtkTreeModel *tree_model,
						      GtkTreeIter *iter,
						      GtkTreeIter *parent);
static gboolean	gnc_budget_tree_model_iter_has_child (GtkTreeModel *tree_model,
						       GtkTreeIter *iter);
static int gnc_budget_tree_model_iter_n_children (GtkTreeModel *tree_model,
						   GtkTreeIter *iter);
static gboolean	gnc_budget_tree_model_iter_nth_child (GtkTreeModel *tree_model,
						       GtkTreeIter *iter,
						       GtkTreeIter *parent,
						       int n);
static gboolean	gnc_budget_tree_model_iter_parent (GtkTreeModel *tree_model,
						    GtkTreeIter *iter,
    						    GtkTreeIter *child);

/* Private internal data structure. */
struct GncBudgetTreeModelPrivate
{
    GncBudget* budget;
    GncBudgetCategory* top_category;
    GncBudgetCategory* balance_category;
	gint event_handler_id;
};

/************************************************************/
/*               g_object required functions                */
/************************************************************/

static GtkObjectClass *parent_class = NULL;

GType
gnc_budget_tree_model_get_type (void)
{
	static GType gnc_budget_tree_model_type = 0;

	if (gnc_budget_tree_model_type == 0) {
		static const GTypeInfo our_info = {
			sizeof (GncBudgetTreeModelClass), /* class_size */
			NULL,   			   /* base_init */
			NULL,				   /* base_finalize */
			(GClassInitFunc) gnc_budget_tree_model_class_init,
			NULL,				   /* class_finalize */
			NULL,				   /* class_data */
			sizeof (GncBudgetTreeModel),	   /* */
			0,				   /* n_preallocs */
			(GInstanceInitFunc) gnc_budget_tree_model_init
		};
		
		static const GInterfaceInfo budget_tree_model_info = {
			(GInterfaceInitFunc) gnc_budget_tree_model_gtk_tree_model_init,
			NULL,
			NULL
		};

		gnc_budget_tree_model_type = g_type_register_static (GTK_TYPE_OBJECT,
								      "GncBudgetTreeModel",
								      &our_info, 0);
        
		g_type_add_interface_static (gnc_budget_tree_model_type,
					     GTK_TYPE_TREE_MODEL,
					     &budget_tree_model_info);
	}
    
	return gnc_budget_tree_model_type;
}

static void
gnc_budget_tree_model_class_init (GncBudgetTreeModelClass *klass)
{
	GObjectClass *o_class;
	GtkObjectClass *object_class;

	parent_class = g_type_class_peek_parent (klass);

	o_class = G_OBJECT_CLASS (klass);
	object_class = GTK_OBJECT_CLASS (klass);

	/* GObject signals */
	o_class->finalize = gnc_budget_tree_model_finalize;

	/* GtkObject signals */
	object_class->destroy = gnc_budget_tree_model_destroy;
}

static void
gnc_budget_tree_model_init (GncBudgetTreeModel *model)
{
	ENTER("model %p", model);
	while (model->stamp == 0) {
		model->stamp = g_random_int ();
	}

	model->priv = g_new0 (GncBudgetTreeModelPrivate, 1);
	model->priv->budget = NULL;
	LEAVE(" ");
}

static void
gnc_budget_tree_model_finalize (GObject *object)
{
	GncBudgetTreeModel *model;

	ENTER("model %p", object);
	g_return_if_fail (object != NULL);
	g_return_if_fail (GNC_IS_BUDGET_TREE_MODEL (object));

	model = GNC_BUDGET_TREE_MODEL(object);
	g_free (model->priv);

	if (G_OBJECT_CLASS (parent_class)->finalize)
	  (* G_OBJECT_CLASS (parent_class)->finalize) (object);
	LEAVE(" ");
}

static void
gnc_budget_tree_model_destroy (GtkObject *object)
{
	GncBudgetTreeModel *model;

	ENTER("model %p", object);
	g_return_if_fail (object != NULL);
	g_return_if_fail (GNC_IS_BUDGET_TREE_MODEL(object));

	model = GNC_BUDGET_TREE_MODEL(object);

	/*active_models = g_list_remove(active_models, model);

	if (model->priv->event_handler_id) {
	  gnc_engine_unregister_event_handler (model->priv->event_handler_id);
	  model->priv->event_handler_id = 0;
	}
    */

	if (GTK_OBJECT_CLASS (parent_class)->destroy)
	  (* GTK_OBJECT_CLASS (parent_class)->destroy) (object);
	LEAVE(" ");
}

/******************************************************
 * GtkTreeModel Interface Methods.
 *
 * ****************************************************/

static void gnc_budget_tree_model_gtk_tree_model_init(GtkTreeModelIface *iface)
{
	iface->get_flags       = gnc_budget_tree_model_get_flags;
	iface->get_n_columns   = gnc_budget_tree_model_get_n_columns;
	iface->get_column_type = gnc_budget_tree_model_get_column_type;
	iface->get_iter        = gnc_budget_tree_model_get_iter;
	iface->get_path        = gnc_budget_tree_model_get_path;
	iface->get_value       = gnc_budget_tree_model_get_value;
	iface->iter_next       = gnc_budget_tree_model_iter_next;
	iface->iter_children   = gnc_budget_tree_model_iter_children;
	iface->iter_has_child  = gnc_budget_tree_model_iter_has_child;
	iface->iter_n_children = gnc_budget_tree_model_iter_n_children;
	iface->iter_nth_child  = gnc_budget_tree_model_iter_nth_child;
	iface->iter_parent     = gnc_budget_tree_model_iter_parent;
}

static guint gnc_budget_tree_model_get_flags (GtkTreeModel *tree_model)
{
    return 0;
}

static int gnc_budget_tree_model_get_n_columns (GtkTreeModel *tree_model)
{
    GncBudgetTreeModel *model;
    gint numCols;
	model = GNC_BUDGET_TREE_MODEL(tree_model);
    numCols =  GNC_BUDGET_TREE_MODEL_NUM_STATIC_COLUMNS + 
                        gnc_budget_get_num_periods(model->priv->budget);
    printf("NumCols: %d\n", numCols);
    return numCols;
}

static GType gnc_budget_tree_model_get_column_type (GtkTreeModel *tree_model, int index)
{
 	g_return_val_if_fail (GNC_IS_BUDGET_TREE_MODEL(tree_model), G_TYPE_INVALID);
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

static gboolean gnc_budget_tree_model_get_iter (GtkTreeModel *tree_model, 
                                                GtkTreeIter *iter,
						                        GtkTreePath *path)
{
	GncBudgetTreeModel *model;
    GncBudgetCategory* category;
	gint i = 0, *indices;
	GtkTreePath *path_copy;

	{
	    gchar *path_string = gtk_tree_path_to_string(path);
	    ENTER("model %p, iter %p, path %s", tree_model, iter, path_string);
	    g_free(path_string);
	}
	g_return_val_if_fail (GNC_IS_BUDGET_TREE_MODEL(tree_model), FALSE);

	model = GNC_BUDGET_TREE_MODEL(tree_model);

	path_copy = gtk_tree_path_copy(path);

	if ((model->priv->budget == NULL) || (model->priv->top_category == NULL)) {
		LEAVE("failed (2)");
		return FALSE;
	}

    category = model->priv->top_category;
    
    indices = gtk_tree_path_get_indices (path);
    for(i = 0; i < gtk_tree_path_get_depth (path); i++){
        category = gnc_budget_category_get_child(category, indices[i]);
        
        if(category == NULL){
            LEAVE("failed (3)");
            return FALSE;
        }
    }
    
    if(category == NULL){
        return FALSE;
    }
    
	iter->stamp = model->stamp;
	iter->user_data = category;

	LEAVE("category %s", gnc_budget_category_get_name(category));
	return TRUE;    
}

static GtkTreePath *gnc_budget_tree_model_get_path (GtkTreeModel *tree_model,
                        						     GtkTreeIter *iter)
{
 	GncBudgetTreeModel *model = GNC_BUDGET_TREE_MODEL(tree_model);
	GtkTreePath *path;
    GncBudgetCategory* category, *parent;

	g_return_val_if_fail (GNC_IS_BUDGET_TREE_MODEL(model), NULL);
	g_return_val_if_fail (iter != NULL, NULL);
	g_return_val_if_fail (iter->user_data != NULL, NULL);
	g_return_val_if_fail (iter->stamp == model->stamp, NULL);
	
	if (model->priv->budget == NULL){
		return NULL;
    }

	path = gtk_tree_path_new ();
    category = iter->user_data;

    if(category == model->priv->top_category){
        gtk_tree_path_prepend_index(path, 0);

	    {
		    gchar* path_string = gtk_tree_path_to_string(path);
			LEAVE("path (2) %s", path_string);
			g_free(path_string);
		}
		return path;
    }

    
    do{
        parent = gnc_budget_category_get_parent(category);
        gtk_tree_path_prepend_index(path,
                gnc_budget_category_get_index_in_parent_list(category));
                                            
        category = gnc_budget_category_get_parent(category);
    }while(parent != model->priv->top_category);

    {
        gchar* path_string = gtk_tree_path_to_string(path);
        LEAVE("path (2) %s", path_string);
        g_free(path_string);
    } 
	return path;   
}

static void gnc_budget_tree_model_get_value (GtkTreeModel *tree_model,
				                    	      GtkTreeIter *iter,
                					          int column,
					                          GValue *value)
{
	GncBudgetTreeModel* model = GNC_BUDGET_TREE_MODEL(tree_model);
    GncBudgetCategory* category;
    char buffer[256];
    gchar* tempStr;
    double floatValue;
    GString* freqStr;
    int index;

	g_return_if_fail (GNC_IS_BUDGET_TREE_MODEL(model));
	g_return_if_fail (iter != NULL);
	g_return_if_fail (iter->user_data != NULL);
	g_return_if_fail (iter->stamp == model->stamp);

    category = (GncBudgetCategory*)iter->user_data;
    ENTER("Col: %d, Category: %s", column, gnc_budget_category_get_name(category));
    if(column == GNC_BUDGET_TREE_MODEL_COL_EDITABLE){
        g_value_init(value, G_TYPE_BOOLEAN);
        
        if((category == gnc_budget_get_inflow_category(model->priv->budget)) ||
           (category == gnc_budget_get_outflow_category(model->priv->budget)) ||
           (category == model->priv->balance_category)){
            g_value_set_boolean(value, FALSE);
        }
        else{
            g_value_set_boolean(value, TRUE);
        }
        return;
    }

    if((column >= GNC_BUDGET_TREE_MODEL_NUM_STATIC_COLUMNS) && 
        (column < (GNC_BUDGET_TREE_MODEL_NUM_STATIC_COLUMNS + 
                    gnc_budget_get_num_periods(model->priv->budget)))){
        index = column - (GNC_BUDGET_TREE_MODEL_NUM_STATIC_COLUMNS);
        g_value_init (value, G_TYPE_STRING);
        
        if(category == gnc_budget_get_inflow_category(model->priv->budget)){
            floatValue = gnc_budget_category_sum_child_values_by_index(
                                    gnc_budget_get_inflow_category(model->priv->budget), index);
        }
        else if(category == gnc_budget_get_outflow_category(model->priv->budget)){
            floatValue = gnc_budget_category_sum_child_values_by_index(
                                    gnc_budget_get_outflow_category(model->priv->budget), index);
        }
        else if(category == model->priv->balance_category){
            floatValue = gnc_budget_category_sum_child_values_by_index(
                                    gnc_budget_get_inflow_category(model->priv->budget), index);
            floatValue += (-1) * gnc_budget_category_sum_child_values_by_index(
                                    gnc_budget_get_outflow_category(model->priv->budget), index);
        }
        else{
            floatValue = gnc_numeric_to_double(
                gnc_budget_category_get_value_by_index(category, index));
        }
        
        sprintf(buffer, "%0.2f", floatValue);
        tempStr = g_strdup(buffer);
		
        g_value_set_string(value, tempStr);
        g_free(tempStr);
        return;
    }
    
	switch (column) {
		case GNC_BUDGET_TREE_MODEL_COL_NAME:
			g_value_init (value, G_TYPE_STRING);
			g_value_set_string (value, gnc_budget_category_get_name(category));
			break;
		case GNC_BUDGET_TREE_MODEL_COL_VALUE:
			/*g_value_init (value, G_TYPE_STRING);
			g_value_set_string (value, "");*/
			g_value_init (value, G_TYPE_STRING);
            
            floatValue = gnc_numeric_to_double(gnc_budget_category_get_value(category));
            sprintf(buffer, "%0.2f", floatValue);
            tempStr = g_strdup(buffer);
			g_value_set_string(value, tempStr);
            g_free(tempStr);
			break;
		case GNC_BUDGET_TREE_MODEL_COL_FREQ:
			/*g_value_init (value, G_TYPE_STRING);
			g_value_set_string (value, "");*/
			g_value_init (value, G_TYPE_STRING);
            freqStr = g_string_sized_new(16);
            xaccFreqSpecGetFreqStr(gnc_budget_category_get_frequency(category), freqStr);
			g_value_set_string (value, freqStr->str);
			break;
        case GNC_BUDGET_TREE_MODEL_COL_LAST_OVERFLOW:
			g_value_init (value, G_TYPE_STRING);
			g_value_set_string (value, "0.0");
            break;
        case GNC_BUDGET_TREE_MODEL_COL_ACTUAL:
			g_value_init (value, G_TYPE_STRING);
			g_value_set_string (value, "0.0");
            break;
        case GNC_BUDGET_TREE_MODEL_COL_PLANNED:
			g_value_init (value, G_TYPE_STRING);
			g_value_set_string (value, "0.0");
            break;
        case GNC_BUDGET_TREE_MODEL_COL_CUR_OVERFLOW:
			g_value_init (value, G_TYPE_STRING);
			g_value_set_string (value, "0.0");
            break;
		default:
			g_value_init (value, G_TYPE_STRING);
			g_value_set_string (value, "0.0");
            break;
	}
    
    LEAVE("category %s, column %d", gnc_budget_category_get_name(category), column);
}

static gboolean	gnc_budget_tree_model_iter_next (GtkTreeModel *tree_model, GtkTreeIter *iter)
{
 	GncBudgetTreeModel *model = GNC_BUDGET_TREE_MODEL(tree_model);
    int numChildren, index;
    GncBudgetCategory* parent, *category;

	g_return_val_if_fail (GNC_IS_BUDGET_TREE_MODEL(model), FALSE);
	g_return_val_if_fail (iter != NULL, FALSE);
	g_return_val_if_fail (iter->user_data != NULL, FALSE);
	g_return_val_if_fail (iter->stamp == model->stamp, FALSE);
    ENTER("iter_next:category %s", gnc_budget_category_get_name(iter->user_data));

    category = iter->user_data;

    if(category == model->priv->top_category){
	    iter->stamp = 0;
		LEAVE("failed (1)");
        return FALSE; 
    }
    
    parent = gnc_budget_category_get_parent(category);
    
    numChildren = gnc_budget_category_get_num_children(parent);
    index = gnc_budget_category_get_index_in_parent_list(category);

    if(index > (numChildren - 2)){
		iter->stamp = 0;
		LEAVE("failed (2)");
		return FALSE;
    }

    category = gnc_budget_category_get_child(parent, index + 1);

    if(category == NULL){
		iter->stamp = 0;
		LEAVE("failed (3)");
		return FALSE;
    }

    iter->user_data = category;

    LEAVE("iter_next:category %s", gnc_budget_category_get_name(iter->user_data));
	return TRUE;   
}

static gboolean	gnc_budget_tree_model_iter_children (GtkTreeModel *tree_model,
                        						      GtkTreeIter *iter,
						                              GtkTreeIter *parent)
{
    GncBudgetCategory* parentCat, *category;
 	GncBudgetTreeModel *model = GNC_BUDGET_TREE_MODEL(tree_model);
    g_return_val_if_fail (GNC_IS_BUDGET_TREE_MODEL(tree_model), FALSE);
    g_return_val_if_fail (iter != NULL, FALSE);

	if (parent == NULL){
        iter->user_data = model->priv->top_category;
	    iter->stamp = model->stamp;
		return TRUE;
    }
    parentCat = parent->user_data;
    category = gnc_budget_category_get_child(parentCat, 0);

    if(category == NULL){
        return FALSE;
    }

    iter->user_data = category;
    iter->stamp = model->stamp;
        
    LEAVE("iter_children");
	return TRUE;   
}

static gboolean	gnc_budget_tree_model_iter_has_child (GtkTreeModel *tree_model,
                        						       GtkTreeIter *iter)
{
	GncBudgetTreeModel *model;
    GncBudgetCategory* category;

	g_return_val_if_fail (GNC_IS_BUDGET_TREE_MODEL(tree_model), FALSE);

	model = GNC_BUDGET_TREE_MODEL(tree_model);

	g_return_val_if_fail (iter != NULL, FALSE);
	g_return_val_if_fail (iter->user_data != NULL, FALSE);
	g_return_val_if_fail (iter->stamp == model->stamp, FALSE);

    category = iter->user_data;

    if(gnc_budget_category_get_num_children(category) == 0){
        LEAVE("no");
        return FALSE;
    }

	LEAVE("yes");
	return TRUE;
}

static int gnc_budget_tree_model_iter_n_children (GtkTreeModel *tree_model,
                        						   GtkTreeIter *iter)
{
	GncBudgetTreeModel *model;
    GncBudgetCategory* category;

	g_return_val_if_fail (GNC_IS_BUDGET_TREE_MODEL(tree_model), FALSE);

	model = GNC_BUDGET_TREE_MODEL(tree_model);

	g_return_val_if_fail (iter != NULL, FALSE);
	g_return_val_if_fail (iter->user_data != NULL, FALSE);
	g_return_val_if_fail (iter->stamp == model->stamp, FALSE);

    category = iter->user_data;

	LEAVE("count is %d", gnc_budget_category_get_num_children(category));
	return gnc_budget_category_get_num_children(category);    
}

static gboolean	gnc_budget_tree_model_iter_nth_child (GtkTreeModel *tree_model,
                        						        GtkTreeIter *iter,
						                                GtkTreeIter *parent,
						                                int n)
{
	GncBudgetTreeModel *model;
    GncBudgetCategory* category, *parentCat;
	
	g_return_val_if_fail(GNC_IS_BUDGET_TREE_MODEL(tree_model), FALSE);
    g_return_val_if_fail (iter != NULL, FALSE);

	model = GNC_BUDGET_TREE_MODEL(tree_model);

	if (parent == NULL){
        parentCat = model->priv->top_category;
    }
    else{
        parentCat = parent->user_data;
    }

    category = gnc_budget_category_get_child(parentCat, n);
    
	iter->user_data = category;
	iter->stamp = model->stamp;

	if (iter->user_data == NULL) {
		iter->stamp = 0;
		return FALSE;
	}

	return TRUE;
}

static gboolean	gnc_budget_tree_model_iter_parent (GtkTreeModel *tree_model,
                        						    GtkTreeIter *iter,
    					                    	    GtkTreeIter *child)
{
 	GncBudgetTreeModel *model;
    GncBudgetCategory* category, *parentCat;
	
    model = GNC_BUDGET_TREE_MODEL(tree_model);
	
	g_return_val_if_fail(GNC_IS_BUDGET_TREE_MODEL(tree_model), FALSE);
	g_return_val_if_fail (child != NULL, FALSE);
	g_return_val_if_fail (child->user_data != NULL, FALSE);
	g_return_val_if_fail (child->stamp == model->stamp, FALSE);
	
    g_return_val_if_fail (iter != NULL, FALSE);


    category = child->user_data;
    
    parentCat = gnc_budget_category_get_parent(category);
    
	iter->user_data = parentCat;
	iter->stamp = model->stamp;

	if (iter->user_data == NULL) {
		iter->stamp = 0;
		return FALSE;
	}

    return TRUE;
}











/*******************************************************
 * Public Interface.
 * 
 * *****************************************************/

GtkTreeModel* gnc_budget_tree_model_new(GncBudget* budget)
{
	GncBudgetTreeModel *model;
	
	ENTER("budget %p", budget);

    /*
	for (item = active_models; item; item = g_list_next(item)) {
		model = (GncTreeModelAccount *)item->data;
		if (model->priv->root == group) {
			LEAVE("returning existing model %p", model);
			return GTK_TREE_MODEL(model);
		}
	}
    */

	model = g_object_new (GNC_TYPE_BUDGET_TREE_MODEL, NULL);

	model->priv->budget = budget;
    printf("Creating dummy category.  Budget %p\n", budget);
	model->priv->top_category = gnc_budget_category_new(gnc_budget_get_book(budget), budget);
    gnc_budget_category_set_name(model->priv->top_category, "Dummy Category");
    printf("Adding inflow category...\n");
    gnc_budget_category_add_child(model->priv->top_category, 
                                    gnc_budget_get_inflow_category(budget));
    printf("Adding outflow category...\n");
    gnc_budget_category_add_child(model->priv->top_category, 
                                    gnc_budget_get_outflow_category(budget));
	
    model->priv->balance_category = gnc_budget_category_new(gnc_budget_get_book(budget), budget);
    gnc_budget_category_set_name(model->priv->balance_category, "Total Budget Balance");
    gnc_budget_category_add_child(model->priv->top_category, model->priv->balance_category);

    /*
	priv->event_handler_id =
	  gnc_engine_register_event_handler (gnc_tree_model_account_event_handler, model);
    
	active_models = g_list_append (active_models, model);
    */
    
	LEAVE("model %p", model);
	return GTK_TREE_MODEL (model);

}


GncBudgetCategory* gnc_budget_tree_model_get_category(GncBudgetTreeModel* model, GtkTreeIter* iter)
{
	g_return_val_if_fail (GNC_IS_BUDGET_TREE_MODEL(model), NULL);
	g_return_val_if_fail (iter != NULL, NULL);
	g_return_val_if_fail (iter->user_data != NULL, NULL);
	g_return_val_if_fail (iter->stamp == model->stamp, NULL);

    printf("Category Selected: %s\n", gnc_budget_category_get_name(iter->user_data));
	return (GncBudgetCategory *) iter->user_data;    
}

/** @} */
/** @} */
