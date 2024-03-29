/*
 * gnc-plugin-account-tree.h --
 *
 * Copyright (C) 2003 Jan Arne Petersen
 * Author: Jan Arne Petersen <jpetersen@uni-bonn.de>
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

/** @addtogroup MenuPlugins
    @{ */
/** @addtogroup GncPluginAccountTree An Account Tree Plugin
    @{ */
/** @file gnc-plugin-account-tree.h
    @brief Provide the menus to create a chart of account page.
    @author Copyright (C) 2003 Jan Arne Petersen <jpetersen@uni-bonn.de>
*/

#ifndef __GNC_PLUGIN_ACCOUNT_TREE_H
#define __GNC_PLUGIN_ACCOUNT_TREE_H

#include <gtk/gtk.h>

#include "gnc-plugin.h"

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_PLUGIN_ACCOUNT_TREE            (gnc_plugin_account_tree_get_type ())
G_DECLARE_FINAL_TYPE (GncPluginAccountTree, gnc_plugin_account_tree, GNC, PLUGIN_ACCOUNT_TREE, GncPlugin)

#define GNC_PLUGIN_ACCOUNT_TREE_NAME "gnc-plugin-account-tree"

/** Create a new account tree menu plugin.
 *
 *  @return A pointer to the new object.
 */
GncPlugin *gnc_plugin_account_tree_new (void);

G_END_DECLS

#endif /* __GNC_PLUGIN_ACCOUNT_TREE_H */

/** @} */
/** @} */
