/********************************************************************\
 * gnc-budget-book-p.h --  Private functions for budget/book linkage*
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
/** @file gnc-budget-book-p.h
 *  @brief Private functions for budget/book linkage.
 *  @author Created by Darin Willits 04 sep 2003 
 *  @author Copyright (c) 04 sep 2003 Darin Willits <darin@willits.ca>
 *
 * 
 */

#ifndef __GNC_BUDGETBOOK_P_H__
#define __GNC_BUDGETBOOK_P_H__

#include "qofbook.h"
#include "gnc-budget-book.h"

struct xaccBudgetsDef{

    QofBook *book;
    GList   *budget_list;
    gboolean budget_notsaved;
    
};

/** Associate the given budget list to the book.*/
void gnc_book_set_budgets( QofBook *book, GList *newList );

gboolean gnc_budget_register (void);

#endif // __BUDGETBOOK_P_H__

/** @} */
/** @} */
