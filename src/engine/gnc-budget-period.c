/********************************************************************\
 * gnc-budget-period.c --  Implementation of the budget period      *
 *                      interface.                                  *
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
/** @file gnc-budget-period.c
 *  @brief Implementation of the budget period interface.
 *  @author Created by Darin Willits 16 sep 2003 
 *  @author Copyright (c) 16 sep 2003 Darin Willits <darin@willits.ca>
 *
 */


// Includes
#include <glib.h>

#include "gnc-budget-period.h"
#include "gnc-budget-period-p.h"

GncBudgetPeriod* gnc_budget_period_new(void)
{
    GncBudgetPeriod* period;

    period = g_new0(GncBudgetPeriod, 1);

    g_date_clear(&period->start_date, 1);
    g_date_clear(&period->end_date, 1);

    return period;
}

void gnc_budget_period_set_start_date(GncBudgetPeriod* period, GDate* startDate)
{
    if(period == NULL){
        return;
    }
    period->start_date = *startDate;
}

GDate* gnc_budget_period_get_start_date(GncBudgetPeriod* period)
{
    if(period == NULL){
        return NULL;
    }
    return &period->start_date;
}



void gnc_budget_period_set_end_date(GncBudgetPeriod* period, GDate* endDate)
{
    if(period == NULL){
        return;
    }
    period->end_date = *endDate;
}

GDate* gnc_budget_period_get_end_date(GncBudgetPeriod* period)
{
    if(period == NULL){
        return NULL;
    }
    return &period->end_date;
}

/** @} */
/** @} */
