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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
 * Boston, MA  02111-1307,  USA       gnu@gnu.org
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
typedef struct GncPluginBasicCommandsPrivate GncPluginBasicCommandsPrivate;

typedef struct {
	GncPlugin parent;

	GncPluginBasicCommandsPrivate *priv;
} GncPluginBasicCommands;

typedef struct {
	GncPluginClass parent;
} GncPluginBasicCommandsClass;

/* function prototypes */
GType      gnc_plugin_basic_commands_get_type (void);

GncPlugin *gnc_plugin_basic_commands_new      (void);

void       gnc_new_basic_commands             (GncMainWindow *window);

G_END_DECLS

#endif /* __GNC_PLUGIN_BASIC_COMMANDS_H */
