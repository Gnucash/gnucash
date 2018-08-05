/*
 * Copyright (C) 2005, Chris Shoemaker <c.shoemaker@cox.net>
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

/** @addtogroup budget
   @{ */

/** @file gnc-tree-model-budget.h
 * @brief provides some utilities for working with the list of
 * budgets in a book.*/

/** @todo This file is poorly named, since it
 * covers both model and view.*/

#ifndef __GNC_TREE_MODEL_BUDGET_H__
#define __GNC_TREE_MODEL_BUDGET_H__

#include "gnc-budget.h"

/* The budget list columns. */
enum
{
    BUDGET_GUID_COLUMN,
    BUDGET_NAME_COLUMN,
    BUDGET_DESCRIPTION_COLUMN,
    BUDGET_LIST_NUM_COLS
};

GtkTreeModel * gnc_tree_model_budget_new(QofBook *book);

void gnc_tree_view_budget_set_model(GtkTreeView *tv, GtkTreeModel *tm);

GncBudget *gnc_tree_model_budget_get_budget(GtkTreeModel *tm,
        GtkTreeIter *iter);

gboolean gnc_tree_model_budget_get_iter_for_budget(GtkTreeModel *tm,
        GtkTreeIter *iter,
        GncBudget *bgt);
/** @} */
#endif // __GNC_TREE_MODEL_BUDGET_H__
