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
 * @brief Provides GTK4 list-model utilities for budgets in a book. */

#ifndef __GNC_TREE_MODEL_BUDGET_H__
#define __GNC_TREE_MODEL_BUDGET_H__

#include <gio/gio.h>
#include "gnc-budget.h"

#ifdef __cplusplus
extern "C" {
#endif

#define GNC_TYPE_BUDGET_LIST_ITEM (gnc_budget_list_item_get_type ())
G_DECLARE_FINAL_TYPE (GncBudgetListItem, gnc_budget_list_item, GNC,
                      BUDGET_LIST_ITEM, GObject)

/**
 * Create a stable list model of the budgets in @a book. Each item retains the
 * budget GUID and displayed text, so callers resolve the current engine object
 * only when the user acts on a selection.
 */
GListModel *gnc_budget_list_model_new (QofBook *book);

GncBudget *gnc_budget_list_item_get_budget (GncBudgetListItem *item);
const gchar *gnc_budget_list_item_get_name (GncBudgetListItem *item);
const gchar *gnc_budget_list_item_get_description (GncBudgetListItem *item);

/** Return the position of @a budget in @a model, or G_MAXUINT if absent. */
guint gnc_budget_list_model_get_position (GListModel *model, GncBudget *budget);

/** @} */
#ifdef __cplusplus
}
#endif

#endif // __GNC_TREE_MODEL_BUDGET_H__
