/*
 * gnc-plugin-menu-additions.h --
 * Copyright (C) 2005 David Hampton <hampton@employees.org>
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
/** @addtogroup PluginMenuAdditions Non-GAction Menu Support
    @{ */
/** @file gnc-plugin-menu-additions.h
    @brief Functions providing menu items from scheme code.
    @author Copyright (C) 2005 David Hampton <hampton@employees.org>

    The GncPluginMenuAdditions code handles menu/toolbar actions that
    come from the Scheme side of Gnucash.  It is responsible for
    adding these actions to any new window that is opened.  This code
    does not use the base object code to automatically handle these
    functions, but performs them manually.
*/

#ifndef __GNC_PLUGIN_MENU_ADDITIONS_H
#define __GNC_PLUGIN_MENU_ADDITIONS_H

#include "gnc-plugin.h"

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_PLUGIN_MENU_ADDITIONS            (gnc_plugin_menu_additions_get_type ())
G_DECLARE_FINAL_TYPE (GncPluginMenuAdditions, gnc_plugin_menu_additions, GNC, PLUGIN_MENU_ADDITIONS, GncPlugin)

#define GNC_PLUGIN_MENU_ADDITIONS_NAME "gnc-plugin-menu-additions"

/* function prototypes */

/** Create a new menu_additions plugin.  This plugin attaches the menu
 *  items from Scheme code to any window that is opened.
 *
 *  @return A pointer to the new object.
 */
GncPlugin *gnc_plugin_menu_additions_new (void);

G_END_DECLS

#endif /* __GNC_PLUGIN_MENU_ADDITIONS_H */

/** @} */
/** @} */
