/********************************************************************\
 * gnc-budget-cat-p.h -- Definition of the budget category structure.   *
 * Copyright (C) 05 sep 2003    Darin Willits <darin@willits.ca>    *
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
/** @file gnc-budget-cat-p.h
 *  @brief Definition of the private budget category structure.
 *  @author Created by Darin Willits 05 sep 2003 
 *  @author Copyright (c) 05 sep 2003 Darin Willits <darin@willits.ca>
 *
 */

#ifndef __GNC_BUDGET_CAT_P_H__
#define __GNC_BUDGET_CAT_P_H__

#include "gnc-numeric.h"
#include "gnc-budget-cat.h"
#include "gnc-budget.h"
#include "Account.h"
#include "FreqSpec.h"
#include "qofbook.h"
#include "gnc-engine.h"
#include "qofinstance-p.h"

struct gncp_budget_category{

    QofInstance     inst;
    
    char*       name;
    char*       description;
    GncBudget*  budget;

    gnc_numeric value;
    GncBudgetCategoryType type;
    gnc_commodity* commodity;
    GList* period_values;

    GncBudgetCategory* parent;
    GncBudgetCategoryList* children;
    
    GList* accountList;    

    FreqSpec*   freq;
};


#endif // __BUDGET_CAT_P_H__

/** @} */
/** @} */
