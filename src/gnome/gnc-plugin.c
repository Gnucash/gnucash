/* 
 * gnc-plugin.c -- 
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
 * Boston, MA  02111-1307,  USA       gnu@gnu.org
 */

#include "config.h"

#include "gnc-plugin.h"

GType
gnc_plugin_get_type (void)
{
	static GType gnc_plugin_type = 0;

	if (gnc_plugin_type == 0) {
		static const GTypeInfo our_info = {
			sizeof (GncPluginIface),
			NULL,
			NULL,
			NULL,
			NULL,
			NULL,
			0,
			0,
			NULL
		};

		gnc_plugin_type = g_type_register_static (G_TYPE_INTERFACE,
							  "GncPlugin",
							   &our_info, 0);
		g_type_interface_add_prerequisite (gnc_plugin_type, G_TYPE_OBJECT);
	}

	return gnc_plugin_type;
}

void
gnc_plugin_add_to_window (GncPlugin *plugin,
			  GncMainWindow *window,
			  GQuark type)
{
	g_return_if_fail (GNC_IS_PLUGIN (plugin));
	g_return_if_fail (GNC_PLUGIN_GET_IFACE (plugin)->add_to_window != NULL);

	GNC_PLUGIN_GET_IFACE (plugin)->add_to_window (plugin, window, type);
}

void
gnc_plugin_remove_from_window (GncPlugin *plugin,
			       GncMainWindow *window,
			       GQuark type)
{
	g_return_if_fail (GNC_IS_PLUGIN (plugin));
	g_return_if_fail (GNC_PLUGIN_GET_IFACE (plugin)->remove_from_window != NULL);

	GNC_PLUGIN_GET_IFACE (plugin)->remove_from_window (plugin, window, type);
}

const gchar *
gnc_plugin_get_name (GncPlugin *plugin)
{
	g_return_val_if_fail (GNC_IS_PLUGIN (plugin), NULL);
	g_return_val_if_fail (GNC_PLUGIN_GET_IFACE (plugin)->get_name != NULL, NULL);

	return GNC_PLUGIN_GET_IFACE (plugin)->get_name (plugin);
}

GncPluginPage *
gnc_plugin_create_page (GncPlugin *plugin,
			const gchar *uri)
{
	g_return_val_if_fail (GNC_IS_PLUGIN (plugin), NULL);

	if (!GNC_PLUGIN_GET_IFACE (plugin)->create_page)
	  return NULL;
	return GNC_PLUGIN_GET_IFACE (plugin)->create_page (plugin, uri);
}

/*
static void
gnc_plugin_base_init (gpointer klass)
{
	static gboolean initialized = FALSE;

	if (!initialized) {
		initialized = TRUE;

		signals[MERGE_ACTIONS] = g_signal_new ("merge-actions",
						       G_OBJECT_CLASS_TYPE (klass),
						       G_SIGNAL_RUN_FIRST,
						       G_STRUCT_OFFSET (GncPluginClass, merge_actions),
						       NULL, NULL,
						       g_cclosure_marshal_VOID__POINTER,
						       G_TYPE_NONE,
						       1,
						       EGG_TYPE_MENU_MERGE);
		signals[UNMERGE_ACTIONS] = g_signal_new ("unmerge-actions",
							 G_OBJECT_CLASS_TYPE (klass),
							 G_SIGNAL_RUN_FIRST,
							 G_STRUCT_OFFSET (GncPluginClass, unmerge_actions),
							 NULL, NULL,
							 g_cclosure_marshal_VOID__POINTER,
							 G_TYPE_NONE,
							 1,
							 EGG_TYPE_MENU_MERGE);
	}
}
*/
