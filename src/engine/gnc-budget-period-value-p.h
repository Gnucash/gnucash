/********************************************************************\
 * gnc-budget-period-value-p.h -- Private structure definition of the   *
 *                              budget period value objects.        *
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
/** @file gnc-budget-period-value-p.h
 *  @brief Private structure definition of the budget period value 
 *          structure.
 *  @author Created by Darin Willits 16 sep 2003 
 *  @author Copyright (c) 16 sep 2003 Darin Willits <darin@willits.ca>
 *
 * A budget period value is the value associated with each budget period.
 * Each category maintains a list of these values.  If the budget period
 * is changed this list needs to be regenerated.
 * Note: We should probably add a reference here to the actual budget
 * period we are referring to.  Right now we just depend on their order
 * in their respective lists.
 */

#ifndef __GNC_BUDGET_PERIOD_VALUE_P_H__
#define __GNC_BUDGET_PERIOD_VALUE_P_H__

#include "gnc-budget-period-value.h"

struct gncp_budget_period_value{
    gnc_numeric value;
    gnc_numeric adjustment;
};


#endif // __GNC_BUDGET_PERIOD_VALUE_P_H__

/** @} */
/** @} */
