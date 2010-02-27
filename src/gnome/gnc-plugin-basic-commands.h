/*
 * gnc-plugin-basic-commands.h --
 *
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
/** @addtogroup GncPluginAccountTree An Account Tree Plugin
    @{ */
/** @file gnc-plugin-basic-commands.h
    @brief Functions providing a basic set of menu items.
    @author Copyright (C) 2005 David Hampton <hampton@employees.org>
*/

#ifndef __GNC_PLUGIN_BASIC_COMMANDS_H
#define __GNC_PLUGIN_BASIC_COMMANDS_H

#include <gtk/gtkwindow.h>

#include "gnc-plugin.h"

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_PLUGIN_BASIC_COMMANDS            (gnc_plugin_basic_commands_get_type ())
#define GNC_PLUGIN_BASIC_COMMANDS(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_PLUGIN_BASIC_COMMANDS, GncPluginBasicCommands))
#define GNC_PLUGIN_BASIC_COMMANDS_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_PLUGIN_BASIC_COMMANDS, GncPluginBasicCommandsClass))
#define GNC_IS_PLUGIN_BASIC_COMMANDS(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_PLUGIN_BASIC_COMMANDS))
#define GNC_IS_PLUGIN_BASIC_COMMANDS_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_PLUGIN_BASIC_COMMANDS))
#define GNC_PLUGIN_BASIC_COMMANDS_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_PLUGIN_BASIC_COMMANDS, GncPluginBasicCommandsClass))

#define GNC_PLUGIN_BASIC_COMMANDS_NAME "gnc-plugin-basic-commands"

/* typedefs & structures */

/** The instance data structure for an basic commands menu plugin. */
typedef struct
{
    /** The parent object for this widget */
    GncPlugin gnc_plugin;
} GncPluginBasicCommands;


/** The class data structure for a basic commands menu plugin. */
typedef struct
{
    /** The parent class for this widget. */
    GncPluginClass gnc_plugin;
} GncPluginBasicCommandsClass;


/** Get the type of the basic commands menu plugin.
 *
 *  @return A GType.
 */
GType gnc_plugin_basic_commands_get_type (void);


/** Create a new basic commands menu plugin.
 *
 *  @return A pointer to the new object.
 */
GncPlugin *gnc_plugin_basic_commands_new (void);

G_END_DECLS

#endif /* __GNC_PLUGIN_BASIC_COMMANDS_H */

/** @} */
/** @} */
