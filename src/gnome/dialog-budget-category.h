/********************************************************************\
 * dialog-budget-category.h --  Defintion of the public interface   *
 *                              for the budget category dialog.     *
 * Copyright (C) 10 sep 2003    Darin Willits <darin@willits.ca>    *
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

/** @addtogroup GUI
 *     @{ */
/** @addtogroup Budget_GUI Budget dialogs
 *     @{ */
 /** @file dialog-budget-category.h
 *  @brief Definition of the public interface of the budget category dialog.
 *  @author Created by Darin Willits 10 sep 2003 
 *  @author Copyright (c) 10 sep 2003 Darin Willits <darin@willits.ca>
 *
 */

#ifndef __DIALOG_BUDGET_CATEGORY_H__
#define __DIALOG_BUDGET_CATEGORY_H__

#include "gnc-budget-cat.h"

/* Launch the modify category dialog.
 */
void gnc_budget_category_dialog_create(GncBudget* budget, GncBudgetCategory* category);

/* Launch the modify category dialog without an existing category.
 * A new one will be created and inserted into the buget on success.
 */
void gnc_budget_category_new_dialog_create(GncBudget* budget);
#endif // __DIALOG_BUDGET_CATEGORY_H__

/** @} */
/** @} */
