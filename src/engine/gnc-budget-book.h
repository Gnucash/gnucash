/********************************************************************\
 * gnc-budget-book.h -- Linkage between budgets and books.          *
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
/** @file gnc-budget-book.h
 *  @brief Linkage between budgets and books.
 *  @author Created by Darin Willits 04 sep 2003 
 *  @author Copyright (c) 04 sep 2003 Darin Willits <darin@willits.ca>
 *
 * 
 */

#ifndef __GNC_BUDGETBOOK_H__
#define __GNC_BUDGETBOOK_H__

#include "config.h"

#include <glib.h>

#include "qofbook.h"
#include "gnc-budget.h"
#include "gnc-engine.h"


typedef struct xaccBudgetsDef GncBudgets;

/** Retrieve the budget list structure for the given book. */
GncBudgets* gnc_book_get_budget_list( QofBook *book );

/** Retrieve the list of budgets for the given book.*/
GList* gnc_book_get_budgets( QofBook *book );

/** Add the given budget to the list of budgets for the given book. */
void gnc_book_add_budget(QofBook* book, GncBudget* budget);

/** Delete the given budget from the list of budgets for the given book. */
void gnc_book_delete_budget(QofBook* book, GncBudget* budget);

#endif // __BUDGETBOOK_H__

/** @} */
/** @} */
