/********************************************************************\
 * dialog-budget-workbench.h -- Definition of the interface         *
 *                              for the budget workbench dialog.    *
 * Copyright (C) 14 sep 2003    Darin Willits <darin@willits.ca>    *
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
/** @addtogroup Budget_GUI
 *     @{ */
/** @file dialog-budget-workbench.h
 *  @brief Definition of the budget workbench dialog interface.
 *  @author Created by Darin Willits 14 sep 2003 
 *  @author Copyright (c) 14 sep 2003 Darin Willits <darin@willits.ca>
 *
 */

#ifndef __DIALOG_BUDGET_WORKBENCH_H__
#define __DIALOG_BUDGET_WORKBENCH_H__

#include "gnc-budget.h"

/* Create a budget workbench dialog using the given budget object.
 */
void gnc_budget_workbench_dialog_create(GncBudget* budget);

#endif // __DIALOG_BUDGET_WORKBENCH_H__

/** @} */
/** @} */
