/* 
 * gnc-plugin.c -- 
 * Copyright (C) 2003 Jan Arne Petersen
 * Author: Jan Arne Petersen <jpetersen@uni-bonn.de>
 */

#include "config.h"

#include "gnc-plugin.h"

/*static void gnc_plugin_base_init (gpointer klass);

enum {
	MERGE_ACTIONS,
	UNMERGE_ACTIONS,
	LAST_SIGNAL
};

static guint         signals[LAST_SIGNAL] = { 0 };
*/

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
gnc_plugin_merge_actions (GncPlugin *plugin,
			  EggMenuMerge *ui_merge)
{
	g_return_if_fail (GNC_IS_PLUGIN (plugin));
	g_return_if_fail (GNC_PLUGIN_GET_IFACE (plugin)->merge_actions != NULL);

	GNC_PLUGIN_GET_IFACE (plugin)->merge_actions (plugin, ui_merge);
}

void
gnc_plugin_unmerge_actions (GncPlugin *plugin,
			    EggMenuMerge *ui_merge)
{
	g_return_if_fail (GNC_IS_PLUGIN (plugin));
	g_return_if_fail (GNC_PLUGIN_GET_IFACE (plugin)->unmerge_actions != NULL);

	GNC_PLUGIN_GET_IFACE (plugin)->unmerge_actions (plugin, ui_merge);
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
	g_return_val_if_fail (GNC_PLUGIN_GET_IFACE (plugin)->create_page != NULL, NULL);

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
