/* 
 * gnc-plugin-qif-import.c -- 
 * Copyright (C) 2003 Jan Arne Petersen
 * Author: Jan Arne Petersen <jpetersen@uni-bonn.de>
 */

#include "config.h"

#include "gnc-plugin-qif-import.h"

#include "druid-qif-import.h"
#include "messages.h"

#define GNC_PLUGIN_QIF_IMPORT_DEFAULT_ACTIONS "gnc-plugin-qif-import-default-actions"

static void gnc_plugin_qif_import_plugin_init (GncPluginIface *iface);

static void gnc_plugin_qif_import_add_to_window (GncPlugin *plugin, GncMainWindow *window, GQuark type);
static void gnc_plugin_qif_import_remove_from_window (GncPlugin *plugin, GncMainWindow *window, GQuark type);
static const gchar *gnc_plugin_qif_import_get_name (GncPlugin *plugin);
static GncPluginPage *gnc_plugin_qif_import_create_page (GncPlugin *plugin, const gchar *uri);

/* Command callbacks */
static void gnc_plugin_qif_import_cmd_new_qif_import (EggAction *action, gpointer data);

static EggActionGroupEntry gnc_plugin_qif_import_actions [] = {
	{ "QIFImportAction", N_("Import _QIF..."), GTK_STOCK_CONVERT, "<control>i",
	  N_("Import a Quicken QIF file"),
	  G_CALLBACK (gnc_plugin_qif_import_cmd_new_qif_import), NULL },
};
static guint gnc_plugin_qif_import_n_actions = G_N_ELEMENTS (gnc_plugin_qif_import_actions);

GType
gnc_plugin_qif_import_get_type (void)
{
	static GType gnc_plugin_qif_import_type = 0;

	if (gnc_plugin_qif_import_type == 0) {
		static const GTypeInfo our_info = {
			sizeof (GncPluginQifImportClass),
			NULL,
			NULL,
			NULL,
			NULL,
			NULL,
			sizeof (GncPluginQifImport),
			0,
			NULL
		};
		
		static const GInterfaceInfo plugin_info = {
			(GInterfaceInitFunc) gnc_plugin_qif_import_plugin_init,
			NULL,
			NULL
		};

		gnc_plugin_qif_import_type = g_type_register_static (G_TYPE_OBJECT,
								       "GncPluginQifImport",
								       &our_info, 0);

		g_type_add_interface_static (gnc_plugin_qif_import_type,
					     GNC_TYPE_PLUGIN,
					     &plugin_info);
	}

	return gnc_plugin_qif_import_type;
}

GncPlugin *
gnc_plugin_qif_import_new (void)
{
	return GNC_PLUGIN (g_object_new (GNC_TYPE_PLUGIN_QIF_IMPORT, NULL));
}


static void
gnc_plugin_qif_import_plugin_init (GncPluginIface *iface)
{
	iface->add_to_window   = gnc_plugin_qif_import_add_to_window;
	iface->remove_from_window = gnc_plugin_qif_import_remove_from_window;
	iface->get_name        = gnc_plugin_qif_import_get_name;
	iface->create_page     = gnc_plugin_qif_import_create_page;
}

static void
gnc_plugin_qif_import_add_to_window (GncPlugin *plugin,
				     GncMainWindow *window,
				     GQuark type)
{
	g_return_if_fail (GNC_IS_PLUGIN_QIF_IMPORT (plugin));
	g_return_if_fail (GNC_IS_MAIN_WINDOW (window));

	gnc_main_window_merge_actions (window, GNC_PLUGIN_QIF_IMPORT_DEFAULT_ACTIONS,
				       gnc_plugin_qif_import_actions, gnc_plugin_qif_import_n_actions,
				       GNC_UI_DIR "/gnc-plugin-qif-import-ui.xml", NULL);
}
	
static void
gnc_plugin_qif_import_remove_from_window (GncPlugin *plugin,
					  GncMainWindow *window,
					  GQuark type)
{
	g_return_if_fail (GNC_IS_PLUGIN_QIF_IMPORT (plugin));
	g_return_if_fail (GNC_IS_MAIN_WINDOW (window));

	gnc_main_window_unmerge_actions (window, GNC_PLUGIN_QIF_IMPORT_DEFAULT_ACTIONS);
}

static const gchar *
gnc_plugin_qif_import_get_name (GncPlugin *plugin)
{
	return GNC_PLUGIN_QIF_IMPORT_NAME;
}

static GncPluginPage *
gnc_plugin_qif_import_create_page (GncPlugin *plugin,
				   const gchar *uri)
{
	return NULL;
}


/* Command callbacks */
static void
gnc_plugin_qif_import_cmd_new_qif_import (EggAction *action, gpointer data)
{
	gnc_ui_qif_import_druid_make ();
}
