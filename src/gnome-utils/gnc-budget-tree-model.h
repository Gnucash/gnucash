/********************************************************************\
 * gnc-budget-tree-model.h --  Defintion of the tree model for      *
 *                             budget objects.                      *
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
/** @addtogroup Budget_Tree Tree model for budget objects
 *     @{ */
/** @file gnc-budget-tree-model.h
 *  @brief Definition of the tree model for budget objects.
 *  @author Created by Darin Willits 09 sep 2003 
 *  @author Copyright (c) 09 sep 2003 Darin Willits <darin@willits.ca>
 *
 *  Lets try implementing our own GtkTreeModel shall we??
 */

#ifndef __GNC_BUDGET_TREE_MODEL_H__
#define __GNC_BUDGET_TREE_MODEL_H__

#include <gtk/gtktreemodel.h>
#include "gnc-budget.h"

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_BUDGET_TREE_MODEL              (gnc_budget_tree_model_get_type ())
#define GNC_BUDGET_TREE_MODEL(obj)              (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_BUDGET_TREE_MODEL, GncBudgetTreeModel))
#define GNC_BUDGET_TREE_MODEL_CLASS(klass)      (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_BUDGET_TREE_MODEL, GncBudgetTreeModelClass))
#define GNC_IS_BUDGET_TREE_MODEL(obj)           (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_BUDGET_TREE_MODEL))
#define GNC_IS_BUDGET_TREE_MODEL_CLASS(klass)   (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_BUDGET_TREE_MODEL))
#define GNC_BUDGET_TREE_MODEL_GET_CLASS(obj)    (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_BUDGET_TREE_MODEL, GncBudgetTreeModelClass))

/** Column defintions */
typedef enum{
    GNC_BUDGET_TREE_MODEL_COL_NAME,
    GNC_BUDGET_TREE_MODEL_COL_VALUE,
    GNC_BUDGET_TREE_MODEL_COL_FREQ,
    GNC_BUDGET_TREE_MODEL_COL_LAST_OVERFLOW,
    GNC_BUDGET_TREE_MODEL_COL_ACTUAL,
    GNC_BUDGET_TREE_MODEL_COL_PLANNED,
    GNC_BUDGET_TREE_MODEL_COL_CUR_OVERFLOW,
    GNC_BUDGET_TREE_MODEL_COL_EDITABLE,
    GNC_BUDGET_TREE_MODEL_NUM_STATIC_COLUMNS
} GncBudgetTreeModelColumns;


/** Structure definitions */
typedef struct GncBudgetTreeModelPrivate GncBudgetTreeModelPrivate;

typedef struct{
	GtkObject parent;

	GncBudgetTreeModelPrivate *priv;

	int stamp;
} GncBudgetTreeModel;

typedef struct {
	GtkObjectClass parent;
} GncBudgetTreeModelClass;



/* Standard g_object type */
GType gnc_budget_tree_model_get_type(void);

/* Create a new GncBudgetTreeModel object. */
GtkTreeModel* gnc_budget_tree_model_new(GncBudget* budget);

GncBudgetCategory* gnc_budget_tree_model_get_category(GncBudgetTreeModel* model, GtkTreeIter* iter);

G_END_DECLS
#endif // __GNC_BUDGET_TREE_MODEL_H__

/** @} */
/** @} */
