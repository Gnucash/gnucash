/* 
 * gnc-plugin-qif-import.c -- 
 * Copyright (C) 2003 Jan Arne Petersen
 * Author: Jan Arne Petersen <jpetersen@uni-bonn.de>
 */

#include "config.h"

#include "gnc-plugin-qif-import.h"

#include "druid-qif-import.h"
#include "messages.h"

static void gnc_plugin_qif_import_class_init (GncPluginQifImportClass *klass);
static void gnc_plugin_qif_import_init (GncPluginQifImport *plugin);
static void gnc_plugin_qif_import_finalize (GObject *object);

static void gnc_plugin_qif_import_plugin_init (GncPluginIface *iface);

static void gnc_plugin_qif_import_merge_actions (GncPlugin *plugin, EggMenuMerge *ui_merge);
static void gnc_plugin_qif_import_unmerge_actions (GncPlugin *plugin, EggMenuMerge *ui_merge);
static const gchar *gnc_plugin_qif_import_get_name (GncPlugin *plugin);
static GncPluginPage *gnc_plugin_qif_import_create_page (GncPlugin *plugin, const gchar *uri);

/* Command callbacks */
static void gnc_plugin_qif_import_cmd_new_qif_import (EggAction *action, GncPluginQifImport *plugin);

static EggActionGroupEntry gnc_plugin_qif_import_actions [] = {
	{ "QIFImportAction", N_("Import _QIF..."), GTK_STOCK_CONVERT, "<control>i",
	  N_("Import a Quicken QIF file"),
	  G_CALLBACK (gnc_plugin_qif_import_cmd_new_qif_import), NULL },
};
static guint gnc_plugin_qif_import_n_actions = G_N_ELEMENTS (gnc_plugin_qif_import_actions);

struct GncPluginQifImportPrivate
{
	EggActionGroup *action_group;
	guint merge_id;
};

static GObjectClass *parent_class = NULL;

GType
gnc_plugin_qif_import_get_type (void)
{
	static GType gnc_plugin_qif_import_type = 0;

	if (gnc_plugin_qif_import_type == 0) {
		static const GTypeInfo our_info = {
			sizeof (GncPluginQifImportClass),
			NULL,
			NULL,
			(GClassInitFunc) gnc_plugin_qif_import_class_init,
			NULL,
			NULL,
			sizeof (GncPluginQifImport),
			0,
			(GInstanceInitFunc) gnc_plugin_qif_import_init
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
	GncPluginQifImport *plugin;

	plugin = g_object_new (GNC_TYPE_PLUGIN_QIF_IMPORT,
			      NULL);

	return GNC_PLUGIN (plugin);
}

static void
gnc_plugin_qif_import_class_init (GncPluginQifImportClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	parent_class = g_type_class_peek_parent (klass);

	object_class->finalize = gnc_plugin_qif_import_finalize;
}

static void
gnc_plugin_qif_import_init (GncPluginQifImport *plugin)
{
	gint i;

	plugin->priv = g_new0 (GncPluginQifImportPrivate, 1);

	/* Create menu and toolbar information */
	for (i = 0; i < gnc_plugin_qif_import_n_actions; i++) {
		gnc_plugin_qif_import_actions[i].user_data = plugin;
	}

	plugin->priv->action_group = egg_action_group_new ("GncPluginQifImportActions");
	egg_action_group_add_actions (plugin->priv->action_group, gnc_plugin_qif_import_actions,
				      gnc_plugin_qif_import_n_actions);
}

static void
gnc_plugin_qif_import_finalize (GObject *object)
{
	GncPluginQifImport *model = GNC_PLUGIN_QIF_IMPORT (object);

	g_return_if_fail (GNC_IS_PLUGIN_QIF_IMPORT (model));
	g_return_if_fail (model->priv != NULL);

	g_free (model->priv);

	G_OBJECT_CLASS (parent_class)->finalize (object);
}

static void
gnc_plugin_qif_import_plugin_init (GncPluginIface *iface)
{
	iface->merge_actions   = gnc_plugin_qif_import_merge_actions;
	iface->unmerge_actions = gnc_plugin_qif_import_unmerge_actions;
	iface->get_name        = gnc_plugin_qif_import_get_name;
	iface->create_page     = gnc_plugin_qif_import_create_page;
}

static void
gnc_plugin_qif_import_merge_actions (GncPlugin *plugin,
				     EggMenuMerge *ui_merge)
{
	GncPluginQifImport *plugin_qif_import = GNC_PLUGIN_QIF_IMPORT(plugin);
	
	g_return_if_fail (GNC_IS_PLUGIN_QIF_IMPORT (plugin_qif_import));

	egg_menu_merge_insert_action_group (ui_merge, plugin_qif_import->priv->action_group, 0);

	plugin_qif_import->priv->merge_id = egg_menu_merge_add_ui_from_file (ui_merge,
									     GNC_UI_DIR "/gnc-plugin-qif-import-ui.xml",
									     NULL);
	egg_menu_merge_ensure_update (ui_merge);
}
	
static void
gnc_plugin_qif_import_unmerge_actions (GncPlugin *plugin,
					      EggMenuMerge *ui_merge)
{
	GncPluginQifImport *plugin_qif_import = GNC_PLUGIN_QIF_IMPORT(plugin);
	
	g_return_if_fail (GNC_IS_PLUGIN_QIF_IMPORT (plugin_qif_import));
	g_return_if_fail (plugin_qif_import->priv->merge_id != 0);
	g_return_if_fail (plugin_qif_import->priv->action_group != NULL);

	egg_menu_merge_remove_action_group (ui_merge, plugin_qif_import->priv->action_group);
	egg_menu_merge_remove_ui (ui_merge, plugin_qif_import->priv->merge_id);
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
gnc_plugin_qif_import_cmd_new_qif_import (EggAction *action, GncPluginQifImport *plugin)
{
	gnc_ui_qif_import_druid_make ();
}
