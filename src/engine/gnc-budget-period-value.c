/********************************************************************\
 * gnc-budget-period-value.c --  Implmentation of the budget period     *
 *                              value structure.                    *
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
/** @file gnc-budget-period-value.c
 *  @brief Implementation of the budget-period-value structure.
 *  @author Created by Darin Willits 16 sep 2003 
 *  @author Copyright (c) 16 sep 2003 Darin Willits <darin@willits.ca>
 *
 */

// Includes
#include "gnc-budget-period-value.h"
#include "gnc-budget-period-value-p.h"
#include "gnc-numeric.h"

GncBudgetPeriodValue* gnc_budget_period_value_new(void)
{
    GncBudgetPeriodValue* periodValue;

    periodValue = g_new0(GncBudgetPeriodValue, 1);

    periodValue->value = gnc_numeric_zero();
    periodValue->adjustment = gnc_numeric_zero();

    return periodValue;
}

void gnc_budget_period_value_delete(GncBudgetPeriodValue* periodValue)
{
    g_free(periodValue);
}



void gnc_budget_period_value_set_value(GncBudgetPeriodValue* periodValue, gnc_numeric value)
{
    if(periodValue == NULL){
        return;
    }
    periodValue->value = value;
}

void gnc_budget_period_value_set_value_double(GncBudgetPeriodValue* periodValue, double value)
{
    if(periodValue == NULL){
        return;
    }
    periodValue->value = double_to_gnc_numeric(value, 1, GNC_RND_NEVER);
}

gnc_numeric gnc_budget_period_value_get_value(GncBudgetPeriodValue* periodValue)
{
    if(periodValue == NULL){
        return gnc_numeric_zero();
    }
    return periodValue->value;
}


/** @} */

/** @} */
