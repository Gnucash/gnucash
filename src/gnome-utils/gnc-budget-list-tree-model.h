/********************************************************************\
 * gnc-budget-list-tree-model.h -- Tree Model for the internal list *
 *                                 of budgets.                      *
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
/** @file gnc-budget-list-tree-model.h
 *  @brief The tree model which represents the internal list of Budget
 *  objects.
 *  @author Created by Darin Willits 09 feb 2004 
 *  @author Copyright (c) 09 feb 2004 Darin Willits <darin@willits.ca>
 *
 */


#ifndef __GNC_BUDGET_LIST_TREE_MODEL_H__
#define __GNC_BUDGET_LIST_TREE_MODEL_H__

#include <gtk/gtktreemodel.h>
#include "qofbook.h"

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_BUDGET_LIST_TREE_MODEL              (gnc_budget_list_tree_model_get_type ())
#define GNC_BUDGET_LIST_TREE_MODEL(obj)              (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_BUDGET_LIST_TREE_MODEL, GncBudgetListTreeModel))
#define GNC_BUDGET_LIST_TREE_MODEL_CLASS(klass)      (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_BUDGET_LIST_TREE_MODEL, GncBudgetListTreeModelClass))
#define GNC_IS_BUDGET_LIST_TREE_MODEL(obj)           (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_BUDGET_LIST_TREE_MODEL))
#define GNC_IS_BUDGET_LIST_TREE_MODEL_CLASS(klass)   (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_BUDGET_LIST_TREE_MODEL))
#define GNC_BUDGET_LIST_TREE_MODEL_GET_CLASS(obj)    (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_BUDGET_LIST_TREE_MODEL, GncBudgetListTreeModelClass))

/** Column defintions */
typedef enum{
    GNC_BUDGET_LIST_TREE_MODEL_COL_NAME,
    GNC_BUDGET_LIST_TREE_MODEL_COL_DESCRIPTION,
    GNC_BUDGET_TREE_MODEL_COL_EDITABLE,
    GNC_BUDGET_LIST_TREE_MODEL_NUM_COLUMNS,
} GncBudgetListTreeModelColumns;


/** Structure definitions */
typedef struct GncBudgetListTreeModelPrivate GncBudgetListTreeModelPrivate;

typedef struct{
	GtkObject parent;

	GncBudgetListTreeModelPrivate *priv;

	int stamp;
} GncBudgetListTreeModel;

typedef struct {
	GtkObjectClass parent;
} GncBudgetListTreeModelClass;



/* Standard g_object type */
GType gnc_budget_list_tree_model_get_type(void);

/* Create a new GncBudgetTreeModel object. */
GtkTreeModel* gnc_budget_list_tree_model_new(QofBook* book);

G_END_DECLS


#endif // __GNC_BUDGET_LIST_TREE_MODEL_H__

/** @} */
/** @} */
