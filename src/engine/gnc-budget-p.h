/********************************************************************\
 * gnc-budget-p.h -- Private structure defintion for the budget object. *
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
/** @addtogroup Budget
 *     @{ */
/** @file gnc-budget-p.h
 *  @brief Private structure definition for the budget object.
 *  @author Created by Darin Willits 04 sep 2003 
 *  @author Copyright (c) 04 Darin Willits <darin@willits.ca>
 *
 *  
 */

#ifndef __GNC_BUDGET_P_H__
#define __GNC_BUDGET_P_H__

#include <glib.h>
#include "qofbook.h"
#include "guid.h"
#include "gnc-budget.h"
#include "FreqSpec.h"
#include "qofinstance-p.h"

struct gncp_budget{

    QofInstance     inst;
    
    char* name;
    char* description;
    
    FreqSpec* period_freq;
    GDate start_date;
    gint  length;

    GList* period_list;

    GncBudgetCategory* inflow_category;
    GncBudgetCategory* outflow_category;

    /** FIXME: I think we actually have access to this value
     * through the OofInstance member.  We should probably 
     * use that one to reduce confusion and duplication. */
    QofBook*        book;
};


#endif // __GNC_BUDGET_P_H__

/** @} */
/** @} */
