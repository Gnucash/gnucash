/* Copyright (C) 2006 Chris Shoemaker <c.shoemaker@cox.net>
 *
 * gnc-plugin-page-transactions.h --
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

/** @addtogroup transactions Transactionss
    @{ */
/** @file gnc-plugin-page-transactions.h
    @brief
*/

#ifndef __GNC_PLUGIN_PAGE_TRANSACTIONS_H
#define __GNC_PLUGIN_PAGE_TRANSACTIONS_H

#include <gtk/gtkwindow.h>

#include "gnc-plugin-page.h"
#include "gnc-tree-view-transaction.h"

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_PLUGIN_PAGE_TRANSACTIONS            (gnc_plugin_page_transactions_get_type ())
#define GNC_PLUGIN_PAGE_TRANSACTIONS(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_PLUGIN_PAGE_TRANSACTIONS, GncPluginPageTransactions))
#define GNC_PLUGIN_PAGE_TRANSACTIONS_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_PLUGIN_PAGE_TRANSACTIONS, GncPluginPageTransactionsClass))
#define GNC_IS_PLUGIN_PAGE_TRANSACTIONS(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_PLUGIN_PAGE_TRANSACTIONS))
#define GNC_IS_PLUGIN_PAGE_TRANSACTIONS_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_PLUGIN_PAGE_TRANSACTIONS))
#define GNC_PLUGIN_PAGE_TRANSACTIONS_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_PLUGIN_PAGE_TRANSACTIONS, GncPluginPageTransactionsClass))

#define GNC_PLUGIN_PAGE_TRANSACTIONS_NAME "GncPluginPageTransactions"

/* typedefs & structures */
typedef struct {
    GncPluginPage gnc_plugin_page;
} GncPluginPageTransactions;

typedef struct {
    GncPluginPageClass gnc_plugin_page;
} GncPluginPageTransactionsClass;

/* function prototypes */
GType gnc_plugin_page_transactions_get_type(void);

/** Create a new "transactions" plugin page.
 *
 *  @return The newly created plugin page.
 */
GncPluginPage *gnc_plugin_page_transactions_new(GncTreeViewTransaction *tv);

G_END_DECLS

#endif /* __GNC_PLUGIN_PAGE_TRANSACTIONS_H */
/** @} */
