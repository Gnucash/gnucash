/*
 * gnc-plugin-budget.h --
 *
 * Copyright (C) 2005 Chris Shoemaker <c.shoemaker@cox.net>
 *
 * Based on gnc-plugin-account.h, by:
 *   Jan Arne Petersen <jpetersen@uni-bonn.de>
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

#ifndef __GNC_PLUGIN_BUDGET_H
#define __GNC_PLUGIN_BUDGET_H

#include <gtk/gtk.h>
#include "gnc-plugin.h"
#include "gnc-budget.h"

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_PLUGIN_BUDGET            (gnc_plugin_budget_get_type ())
G_DECLARE_FINAL_TYPE (GncPluginBudget, gnc_plugin_budget, GNC, PLUGIN_BUDGET, GncPlugin)

#define GNC_PLUGIN_BUDGET_NAME "gnc-plugin-budget"

/* function prototypes */
GncPlugin *gnc_plugin_budget_new (void);

/** Complete once after closing the selector. The selected budget is borrowed
 * only for the callback; cancellation or a changed session supplies NULL. */
typedef void (*GncBudgetSelectionCallback) (GtkWindow *parent, GncBudget *budget,
                                          gpointer user_data);
void gnc_budget_gui_select_budget_async (GtkWindow *parent, QofBook *book,
                                        GncBudgetSelectionCallback callback,
                                        gpointer user_data);


G_END_DECLS

#endif /* __GNC_PLUGIN_BUDGET_H */
