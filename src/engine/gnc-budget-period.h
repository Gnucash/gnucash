/********************************************************************\
 * gnc-budget-period.h -- Public interface for the budget period        *
 *                      structure.                                  *
 * Copyright (C) 16 sep 2003    Darin Willits <darin@willits.ca>    *
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
/** @addtogroup Budget
 *     @{ */
/** @file gnc-budget-period.h
 *  @brief Public interface for the budget period structure.
 *  @author Created by Darin Willits 16 sep 2003 
 *  @author Copyright (c) 16 sep 2003 Darin Willits <darin@willits.ca>
 *
 * A budget period is simply a begin and end date right now although
 * it may eventually have to encapsulate more information.  Each budget
 * category maintains a list of values each of which refers to a given 
 * budget period object.
 */

#ifndef __GNC_BUDGET_PERIOD_H__
#define __GNC_BUDGET_PERIOD_H__

#include <glib.h>

typedef struct gncp_budget_period GncBudgetPeriod;

/* Create a new budget period object.
 */
GncBudgetPeriod* gnc_budget_period_new(void);

/** Set the start date for the period.
 */
void gnc_budget_period_set_start_date(GncBudgetPeriod* period, GDate* startDate);

/* Get the start date for the period.
 */
GDate* gnc_budget_period_get_start_date(GncBudgetPeriod* period);


/** Set the end date for the period.
 */
void gnc_budget_period_set_end_date(GncBudgetPeriod* period, GDate* endDate);

/* Get the end date for the period.
 */
GDate* gnc_budget_period_get_end_date(GncBudgetPeriod* period);


#endif // __BUDGET_PERIOD_H__

/** @} */
/** @} */
