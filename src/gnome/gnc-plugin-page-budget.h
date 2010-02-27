/* Copyright (C) 2005 Chris Shoemaker <c.shoemaker@cox.net>
 *
 * gnc-plugin-page-budget.h --
 *   (based on gnc-plugin-page-account-tree.h)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

/** @addtogroup budget Budgets
    @{ */
/** @file gnc-plugin-page-budget.h
    @brief
*/

#ifndef __GNC_PLUGIN_PAGE_BUDGET_H
#define __GNC_PLUGIN_PAGE_BUDGET_H

#include <gtk/gtkwindow.h>

#include "gnc-plugin-page.h"
#include "gnc-budget.h"

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_PLUGIN_PAGE_BUDGET            (gnc_plugin_page_budget_get_type ())
#define GNC_PLUGIN_PAGE_BUDGET(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_PLUGIN_PAGE_BUDGET, GncPluginPageBudget))
#define GNC_PLUGIN_PAGE_BUDGET_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_PLUGIN_PAGE_BUDGET, GncPluginPageBudgetClass))
#define GNC_IS_PLUGIN_PAGE_BUDGET(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_PLUGIN_PAGE_BUDGET))
#define GNC_IS_PLUGIN_PAGE_BUDGET_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_PLUGIN_PAGE_BUDGET))
#define GNC_PLUGIN_PAGE_BUDGET_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_PLUGIN_PAGE_BUDGET, GncPluginPageBudgetClass))

#define GNC_PLUGIN_PAGE_BUDGET_NAME "GncPluginPageBudget"

/* typedefs & structures */
typedef struct
{
    GncPluginPage gnc_plugin_page;
} GncPluginPageBudget;

typedef struct
{
    GncPluginPageClass gnc_plugin_page;
} GncPluginPageBudgetClass;

/* function prototypes */
GType gnc_plugin_page_budget_get_type (void);


/** Create a new "budget" plugin page.
 *
 *  @return The newly created plugin page.
 */
GncPluginPage *gnc_plugin_page_budget_new  (GncBudget *budget);

void gnc_budget_gui_delete_budget(GncBudget *budget);

G_END_DECLS

#endif /* __GNC_PLUGIN_PAGE_BUDGET_H */
/** @} */
