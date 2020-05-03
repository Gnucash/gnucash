/* Copyright (C) 2013 Phil Longstaff <phil.longstaff@yahoo.ca>
 *
 * gnc-budget-view.h --
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
/** @file gnc-budget-view.h
    @brief
*/

#ifndef __GNC_BUDGET_VIEW_H
#define __GNC_BUDGET_VIEW_H

#include <gtk/gtk.h>

#if 0
#include "gnc-plugin-page.h"
#endif
#include "gnc-budget.h"
#include "gnc-tree-view-account.h"

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_BUDGET_VIEW            (gnc_budget_view_get_type ())
#define GNC_BUDGET_VIEW(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_BUDGET_VIEW, GncBudgetView))
#define GNC_BUDGET_VIEW_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_BUDGET_VIEW, GncBudgetViewClass))
#define GNC_IS_BUDGET_VIEW(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_BUDGET_VIEW))
#define GNC_IS_BUDGET_VIEW_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_BUDGET_VIEW))
#define GNC_BUDGET_VIEW_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_BUDGET_VIEW, GncBudgetViewClass))

#define GNC_BUDGET_VIEW_NAME "GncBudgetView"

/* typedefs & structures */
typedef struct _GncBudgetView GncBudgetView;
typedef struct _GncBudgetViewClass GncBudgetViewClass;

/* function prototypes */
GType gnc_budget_view_get_type (void);

/** Create a new "budget" display widget.
 *
 *  @return The newly created widget
 */
GncBudgetView *gnc_budget_view_new (GncBudget *budget, AccountFilterDialog* fd);
void gnc_budget_view_save (GncBudgetView* budget_view, GKeyFile *key_file, const gchar* group_name);
void gnc_budget_view_refresh (GncBudgetView* budget_view);
void gnc_budget_view_delete_budget (GncBudgetView* budget_view);
void gnc_budget_view_save_account_filter (GncBudgetView *budget_view);
gboolean gnc_budget_view_restore (GncBudgetView* budget_view, GKeyFile *key_file, const gchar* group_name);
GtkTreeSelection* gnc_budget_view_get_selection (GncBudgetView* budget_view);
Account* gnc_budget_view_get_account_from_path (GncBudgetView* budget_view, GtkTreePath* path);
GList* gnc_budget_view_get_selected_accounts (GncBudgetView* budget_view);
GtkWidget *gnc_budget_view_get_account_tree_view (GncBudgetView* budget_view);

G_END_DECLS

#endif /* __GNC_BUDGET_VIEW_H */
/** @} */
