/********************************************************************\
 * druid-budget-create.h -- Public interface for the Budget         *
 *                          creation druid.                         *
 * Copyright (C) 08 sep 2003    Darin Willits <darin@willits.ca>    *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License or more details.                      *
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
/** @file druid-budget-create.h
 *  @brief Public interface for the budget creation druid.
 *  @author Created by Darin Willits 08 sep 2003 
 *  @author Copyright (c) 08 sep 2003 Darin Willits <darin@willits.ca>
 *
 */

#ifndef __DRUID_BUDGET_CREATE_H__
#define __DRUID_BUDGET_CREATE_H__

#include <glib.h>
#include <gnome.h>

/** Launch the Create Budget druid. */
void gnc_budget_druid_create(GtkTreeModel* treeModel);


#endif // __DRUID_BUDGET_CREATE_H__

/** @} */
/** @} */
