/********************************************************************\
 * gnc-budget-cat.h -- Public interface for the budget category.    *
 * Copyright (C) 05 sep 2003    Darin Willits <darin@willits.ca>    *
 *                                                                 *
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

/** @addtogroup Engine
 *     @{ */
/** @addtogroup Budget
 *     @{ */
/** @file gnc-budget-cat.h
 *  @brief Public interface of the budget category structure.
 *  @author Created by Darin Willits 05 sep 2003 
 *  @author Copyright (c) 05 sep 2003 Darin Willits <darin@willits.ca>
 *
 *  A budget category encapsulates the concept of a single budgetable
 *  item.  By default a category is linked to a single account from
 *  account tree.  However a category can be linked to any number of 
 *  regular accounts including none at all.  Hopefully this flexibility
 *  will allow different users to utilize the budgeting engine to capture
 *  their own concept of budgeting.
 *
 *  This is pre-alpha software.  Beware.
 */

#ifndef __GNC_BUDGET_CAT_H__
#define __GNC_BUDGET_CAT_H__

#include <glib.h>

/** The budget data.*/
typedef struct gncp_budget_category GncBudgetCategory;
typedef GList GncBudgetCategoryList;

#include "Account.h"
#include "gnc-numeric.h"
#include "qofbook.h"
#include "gnc-budget.h"
#include "FreqSpec.h"


typedef enum{
    BUDGET_CATEGORY_INFLOW,
    BUDGET_CATEGORY_OUTFLOW,
} GncBudgetCategoryType;


/**
 * Creates and initializes a Budget Category.
 **/
GncBudgetCategory *gnc_budget_category_new(QofBook* book, GncBudget* budget);

/** Deletes the given budget category object.*/
void gnc_budget_category_delete(GncBudgetCategory* category);

/**  Set the Name of the Budget Category.
 */
void gnc_budget_category_set_name(GncBudgetCategory* category, const gchar* name);

/**  Retieve the Name of the Budget.
 */
gchar* gnc_budget_category_get_name(GncBudgetCategory* category);

/**  Set the Description of the Budget Category.
 */
void gnc_budget_category_set_description(GncBudgetCategory* category, const gchar* description); 

/**  Retieve the Description of the Budget.
 */
gchar* gnc_budget_category_get_description(GncBudgetCategory* category);


/** Set the value for this budget category.
 */
void gnc_budget_category_set_value(GncBudgetCategory* category, gnc_numeric value);

/** Retrieve the value for this budget category.
 */
gnc_numeric gnc_budget_category_get_value(GncBudgetCategory* category);

/* Add the account to the list of accounts.
 */
void gnc_budget_category_add_account(GncBudgetCategory* category, Account* account);

/* Remove the account from the list of accounts.
 */
void gnc_budget_category_remove_account(GncBudgetCategory* category, Account* account);

/* Return the list of related accounts.
 */
AccountList* gnc_budget_category_get_related_accounts(GncBudgetCategory* category);

/* Set the list of related accounts to the one given.
 * Delete the current list if it exists.
 */
void gnc_budget_category_set_related_accounts(GncBudgetCategory* category, AccountList* accounts);


/* Set the frequency.
 */
void gnc_budget_category_set_frequency(GncBudgetCategory* category, FreqSpec* freq);

/** Retrieve the frequency. 
 */
FreqSpec* gnc_budget_category_get_frequency(GncBudgetCategory* category);


/** Set the budget category type
 */
void gnc_budget_category_set_type(GncBudgetCategory* category, GncBudgetCategoryType type);

/** Retrieve the category type.
 */
GncBudgetCategoryType gnc_budget_category_get_type(GncBudgetCategory* category);

/** Set the commodity.
 */
void gnc_budget_category_set_commodity(GncBudgetCategory* category, gnc_commodity* commodity);

/* Retrieve the commodity.
 */
gnc_commodity* gnc_budget_category_get_commodity(GncBudgetCategory* category);


/* Add a category to our list of children.
 */
void gnc_budget_category_add_child(GncBudgetCategory* parent, GncBudgetCategory* child);

/* Remove a category to our list of children.
 */
void gnc_budget_category_remove_child(GncBudgetCategory* parent, GncBudgetCategory* child);

/* Retrieve a child category from the given index.
 */
GncBudgetCategory* gnc_budget_category_get_child(GncBudgetCategory* parent, gint index);

/** Retrieve the entire list of our children.
 */
GncBudgetCategoryList* gnc_budget_category_get_children(GncBudgetCategory* parent);

/** Get the number of child account we have.
 */
gint gnc_budget_category_get_num_children(GncBudgetCategory* parent);

/* Return the parent of this category.
 */
GncBudgetCategory* gnc_budget_category_get_parent(GncBudgetCategory* category);

/** Return the position of this category in its parents list. */
gint gnc_budget_category_get_index_in_parent_list(GncBudgetCategory* category);


/* Generate the budget values.
 * This function will generate the list of budget values based on the 
 * list of periods in the associated budget object.
 */
void gnc_budget_category_generate_values(GncBudgetCategory* category);

/* Retrieve the budget value based on an index.
 */
gnc_numeric gnc_budget_category_get_value_by_index(GncBudgetCategory* category, gint index);

/* Set the period value at the given index.
 */
void gnc_budget_category_set_period_value_by_index(GncBudgetCategory* category, gint index, 
                                                    gnc_numeric value);

/** Get the sum of values for child accounts at the given period index.
 */
double gnc_budget_category_sum_child_values_by_index(GncBudgetCategory* category, gint index);

#endif // __BUDGET_CAT_H__

/** @} */
/** @} */
