/********************************************************************\
 * dialog-budget.h : dialog for entering a budget                   *
 * Copyright (C) 2000 Dave Peticolas <peticola@cs.ucdavis.edu>      *
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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
\********************************************************************/

#ifndef __DIALOG_BUDGET_H_
#define __DIALOG_BUDGET_H_

#include "config.h"

#include <gnome.h>
#include <guile/gh.h>

#include "glade-gnc-dialogs.h"

typedef struct _BudgetDialog BudgetDialog;

BudgetDialog * gnc_ui_budget_dialog_create(SCM budget, SCM apply_func);
void gnc_ui_budget_dialog_destroy(BudgetDialog *bd);

#endif
