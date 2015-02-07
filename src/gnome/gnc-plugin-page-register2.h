/**********************************************************************
 * gnc-plugin-page-register2.h -- register page functions              *
 *                                                                    *
 * Copyright (C) 2003 Jan Arne Petersen <jpetersen@uni-bonn.de>       *
 * Copyright (C) 2003,2005,2006 David Hampton <hampton@employees.org> *
 *                                                                    *
 * This program is free software; you can redistribute it and/or      *
 * modify it under the terms of the GNU General Public License as     *
 * published by the Free Software Foundation; either version 2 of     *
 * the License, or (at your option) any later version.                *
 *                                                                    *
 * This program is distributed in the hope that it will be useful,    *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of     *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *
 * GNU General Public License for more details.                       *
 *                                                                    *
 * You should have received a copy of the GNU General Public License  *
 * along with this program; if not, contact:                          *
 *                                                                    *
 * Free Software Foundation           Voice:  +1-617-542-5942         *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652         *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                     *
 **********************************************************************/

/** @addtogroup ContentPlugins
    @{ */
/** @addtogroup Register2Plugin Register2 Page
    @{ */
/** @file gnc-plugin-page-register.h
    @brief  Functions providing a register page for the GnuCash UI
    @author Copyright (C) 2003 Jan Arne Petersen <jpetersen@uni-bonn.de>
    @author Copyright (C) 2003 David Hampton <hampton@employees.org>
*/

#ifndef __GNC_PLUGIN_PAGE_REGISTER2_H
#define __GNC_PLUGIN_PAGE_REGISTER2_H

#include <gtk/gtk.h>

#include "Account.h"
#include "gnc-ledger-display2.h"
#include "gnc-plugin-page.h"
#include "gnc-split-reg2.h"

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_PLUGIN_PAGE_REGISTER2            (gnc_plugin_page_register2_get_type ())
#define GNC_PLUGIN_PAGE_REGISTER2(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_PLUGIN_PAGE_REGISTER2, GncPluginPageRegister2))
#define GNC_PLUGIN_PAGE_REGISTER2_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_PLUGIN_PAGE_REGISTER2, GncPluginPageRegister2Class))
#define GNC_IS_PLUGIN_PAGE_REGISTER2(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_PLUGIN_PAGE_REGISTER2))
#define GNC_IS_PLUGIN_PAGE_REGISTER2_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_PLUGIN_PAGE_REGISTER2))
#define GNC_PLUGIN_PAGE_REGISTER2_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_PLUGIN_PAGE_REGISTER2, GncPluginPageRegister2Class))
#define GNC_PLUGIN_PAGE_REGISTER2_NAME "GncPluginPageRegister2"

/* typedefs & structures */
typedef struct
{
    GncPluginPage gnc_plugin_page;
} GncPluginPageRegister2;

typedef struct
{
    GncPluginPageClass gnc_plugin_page;
} GncPluginPageRegister2Class;

/* function prototypes */

/** Retrieve the type number for the plugin page.
 *
 *  @return The type number.
 */
GType gnc_plugin_page_register2_get_type (void);


/** Create a new "register" plugin page, given a pointer to an
 *  account.
 *
 *  @param account The pointer to the account to embed in the
 *  register.
 *
 *  @param subaccounts TRUE if all the sub-accounts of the specified
 *  account should be included in the register.
 *
 *  @return The newly created plugin page.
 */
GncPluginPage *
gnc_plugin_page_register2_new (Account *account, gboolean subaccounts);


/** Create a new "register" plugin page, given a pointer to an already
 *  created ledger.  This function should be used when the ledger
 *  already exists.  (E.G.  From the "find transaction" code, or from
 *  the scheduled transaction code.)
 *
 *  @param ledger The pointer to the ledger to embed into the
 *  register.
 *
 *  @return The newly created plugin page.
 */
GncPluginPage *
gnc_plugin_page_register2_new_ledger (GNCLedgerDisplay2 *ledger);


/** Create a new "register" plugin page containing a general journal.
 *
 *  @return The newly created plugin page.
 */
GncPluginPage *
gnc_plugin_page_register2_new_gl (void);


/** Set various register options on a newly created "register" plugin page.
 *
 *  @param plugin_page The "register" page to modify.
 *
 *  @param lines_default Used to calculate the minimum preferred height of
 *                       the plugin page.
 *
 *  @param read_only True if the register should be read-only.
 */
void
gnc_plugin_page_register2_set_options (GncPluginPage *plugin_page,
                                      gint lines_default,
                                      gboolean read_only);


/** Get the GNCSplitReg data structure associated with this register page.
 *
 *  @param plugin_page A "register" page.
 */
GNCSplitReg2 *
gnc_plugin_page_register2_get_gsr (GncPluginPage *plugin_page);


/** Get the GNCLedgerDisplay data structure associated with this register page.
 *
 *  @param plugin_page A "register" page.
 */
GNCLedgerDisplay2 *
gnc_plugin_page_register2_get_ledger (GncPluginPage *plugin_page);


/** Get the Account associated with this register page.
 *
 *  @param page A "register" page.
 *
 *  @return The account if the register contains only a single
 *  account, or an account and its sub-accounts.  NULL otherwise.
 */
Account *
gnc_plugin_page_register2_get_account (GncPluginPageRegister2 *page);

G_END_DECLS
/** @} */
/** @} */

#endif /* __GNC_PLUGIN_PAGE_REGISTER2_H */

