/********************************************************************\
 * gnc-budget.h -- Budget public handling routines.                     *
 * Copyright (C) 04 sep 2003    Darin Willits <darin@willits.ca>    *
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


/** @addtogroup Engine
 *     @{ */
/** @addtogroup Budget Budget objects
 * A budget object contains a list of budget_categories each of 
 * which define an individual budgetable item.  When a budget is first
 * created these catagories will be initialized based on the existing 
 * account hierarchy.
 *     @{ */
/** @file gnc-budget.h
 *  @brief Public interface for budget objects.
 *  @author Created by Darin Willits 04 sep 2003 
 *  @author Copyright (c) 04 Darin Willits <darin@willits.ca>
 *
 * This file contains the public methods to interact with a budgeting
 * object. 
 * This is pre-alpha code right now.  User beware.
 */


#ifndef __GNC_BUDGET_H__
#define __GNC_BUDGET_H__

#include <glib.h>

/** The budget data.*/
typedef struct gncp_budget GncBudget;

#include "guid.h"
#include "qofbook.h"
#include "gnc-budget-cat.h"
#include "FreqSpec.h"


/**
 * Creates and initializes a Budget.
 **/
GncBudget *gnc_budget_new(QofBook *book);

/** Deletes the given budget object.*/
void gnc_budget_delete(GncBudget* budget);



/**  Set the Name of the Budget.
 */
void gnc_budget_set_name(GncBudget* budget, const gchar* name);

/**  Retieve the Name of the Budget.
 */
gchar* gnc_budget_get_name(GncBudget* budget);



/**  Set the Description of the Budget.
 */
void gnc_budget_set_description(GncBudget* budget, const gchar* description);

/**  Retieve the Description of the Budget.
 */
gchar* gnc_budget_get_description(GncBudget* budget);



/** Set the period frequency.
 */
void gnc_budget_set_period_frequency(GncBudget* budget, FreqSpec* period);

/** Get the period frequency.
 */
FreqSpec* gnc_budget_get_period_frequency(GncBudget* budget);



/** Set the start date*/
void gnc_budget_set_start_date(GncBudget* budget, GDate* date);

/** Return the start date */
GDate* gnc_budget_get_start_date(GncBudget* budget);


/** Set the Length for this budget in terms of a number of months.
 */
void gnc_budget_set_length_months(GncBudget* budget, gint months);

/** Set the Length for this budget in terms of a number of months.
 */
void gnc_budget_set_length_years(GncBudget* budget, gint years);

/** Retrieve the Length for this budget in number of months.
 */
gint gnc_budget_get_length_months(GncBudget* budget);

/** Retrieve the Length for this budget in number of years.
 */
gint gnc_budget_get_length_years(GncBudget* budget);



/* Add the inflow category for this budget.  
 * This is a wrapper function to make it easier to add a category
 * as a child of the inflow category.  It's here cause I'm a lazy ass.
 */
void gnc_budget_add_inflow_category(GncBudget* budget, GncBudgetCategory* inflow);

/* Remove the inflow category for this budget.  
 */
void gnc_budget_remove_inflow_category(GncBudget* budget, GncBudgetCategory* category);

/* Set the inflow category for this budget.  
 */
void gnc_budget_set_inflow_category(GncBudget* budget, GncBudgetCategory* inflow);

/** Retrieve the inflow category for this budget. 
 */
GncBudgetCategory* gnc_budget_get_inflow_category(GncBudget* budget);




/* Add the outflow category for this budget.  
 */
void gnc_budget_add_outflow_category(GncBudget* budget, GncBudgetCategory* outflow);

/* Remove the outflow category for this budget.  
 */
void gnc_budget_remove_outflow_category(GncBudget* budget, GncBudgetCategory* inflow);

/* Set the outflow category for this budget.  
 */
void gnc_budget_set_outflow_category(GncBudget* budget, GncBudgetCategory* category);

/** Retrieve the outflow category for this budget. 
 */
GncBudgetCategory* gnc_budget_get_outflow_category(GncBudget* budget);



/** Retrieve the book that this budget is associated with.*/
QofBook* gnc_budget_get_book(GncBudget* budget);


/** Generate the list of periods.
 * This function will use the start date and budget length to generate a 
 * list of budget periods.  The periods are represented by a list of start
 * dates for each period.
 * As well this function regenerates the list of values for each
 * category.
 * */
void gnc_budget_generate_periods(GncBudget* budget);

/** Get the number of periods.
 */
gint gnc_budget_get_num_periods(GncBudget* budget);

/* Return the list of periods.
 */
GList* gnc_budget_get_period_list(GncBudget* budget);


/** Retrieve the budget object associated with the given GUID from 
 * the given book.
 */
GncBudget* gnc_budget_lookup (const GUID *guid, QofBook *book);

#endif // __BUDGET_H__

/** @} */
/** @} */
