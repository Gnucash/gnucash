/* 
 * gnc-plugin-account-tree.c -- 
 * Copyright (C) 2003 Jan Arne Petersen
 * Author: Jan Arne Petersen <jpetersen@uni-bonn.de>
 */

#include "config.h"

#include <string.h>

#include "gnc-plugin-account-tree.h"

#include "gnc-plugin-page-account-tree.h"

#include "messages.h"

static void gnc_plugin_account_tree_class_init (GncPluginAccountTreeClass *klass);
static void gnc_plugin_account_tree_init (GncPluginAccountTree *plugin);
static void gnc_plugin_account_tree_finalize (GObject *object);

static void gnc_plugin_account_tree_plugin_init (GncPluginIface *iface);

static void gnc_plugin_account_tree_add_to_window (GncPlugin *plugin, GncMainWindow *window, GQuark type);
static void gnc_plugin_account_tree_remove_from_window (GncPlugin *plugin, GncMainWindow *window, GQuark type);
static const gchar *gnc_plugin_account_tree_get_name (GncPlugin *plugin);
static GncPluginPage *gnc_plugin_account_tree_create_page (GncPlugin *plugin, const gchar *uri);

/* Command callbacks */
static void gnc_plugin_account_tree_cmd_new_account_tree (EggAction *action, GncMainWindowActionData *data);

static EggActionGroupEntry gnc_plugin_account_tree_actions [] = {
	{ "FileNewAccountTreeAction", N_("New Account Tree"), NULL, NULL,
	  N_("Open a new Account Tree page"),
	  G_CALLBACK (gnc_plugin_account_tree_cmd_new_account_tree), NULL },
};
static guint gnc_plugin_account_tree_n_actions = G_N_ELEMENTS (gnc_plugin_account_tree_actions);

struct GncPluginAccountTreePrivate
{
};

static GObjectClass *parent_class = NULL;

GType
gnc_plugin_account_tree_get_type (void)
{
	static GType gnc_plugin_account_tree_type = 0;

	if (gnc_plugin_account_tree_type == 0) {
		static const GTypeInfo our_info = {
			sizeof (GncPluginAccountTreeClass),
			NULL,
			NULL,
			(GClassInitFunc) gnc_plugin_account_tree_class_init,
			NULL,
			NULL,
			sizeof (GncPluginAccountTree),
			0,
			(GInstanceInitFunc) gnc_plugin_account_tree_init
		};
		
		static const GInterfaceInfo plugin_info = {
			(GInterfaceInitFunc) gnc_plugin_account_tree_plugin_init,
			NULL,
			NULL
		};

		gnc_plugin_account_tree_type = g_type_register_static (G_TYPE_OBJECT,
								       "GncPluginAccountTree",
								       &our_info, 0);

		g_type_add_interface_static (gnc_plugin_account_tree_type,
					     GNC_TYPE_PLUGIN,
					     &plugin_info);
	}

	return gnc_plugin_account_tree_type;
}

GncPlugin *
gnc_plugin_account_tree_new (void)
{
	GncPluginAccountTree *plugin;

	plugin = g_object_new (GNC_TYPE_PLUGIN_ACCOUNT_TREE,
			      NULL);

	return GNC_PLUGIN (plugin);
}

static void
gnc_plugin_account_tree_class_init (GncPluginAccountTreeClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	parent_class = g_type_class_peek_parent (klass);

	object_class->finalize = gnc_plugin_account_tree_finalize;
}

static void
gnc_plugin_account_tree_init (GncPluginAccountTree *plugin)
{
	plugin->priv = g_new0 (GncPluginAccountTreePrivate, 1);
}

static void
gnc_plugin_account_tree_finalize (GObject *object)
{
	GncPluginAccountTree *model = GNC_PLUGIN_ACCOUNT_TREE (object);

	g_return_if_fail (GNC_IS_PLUGIN_ACCOUNT_TREE (model));
	g_return_if_fail (model->priv != NULL);

	g_free (model->priv);

	G_OBJECT_CLASS (parent_class)->finalize (object);
}

static void
gnc_plugin_account_tree_plugin_init (GncPluginIface *iface)
{
	iface->add_to_window      = gnc_plugin_account_tree_add_to_window;
	iface->remove_from_window = gnc_plugin_account_tree_remove_from_window;
	iface->get_name           = gnc_plugin_account_tree_get_name;
	iface->create_page        = gnc_plugin_account_tree_create_page;
}

static void
gnc_plugin_account_tree_add_to_window (GncPlugin *plugin,
				       GncMainWindow *window,
				       GQuark type)
{
	g_return_if_fail (GNC_IS_PLUGIN_ACCOUNT_TREE (plugin));
	g_return_if_fail (GNC_IS_MAIN_WINDOW (window));

	gnc_main_window_merge_actions (window, "gnc-plugin-account-tree-default-actions",
			               gnc_plugin_account_tree_actions, gnc_plugin_account_tree_n_actions,
				       GNC_UI_DIR "/gnc-plugin-account-tree-ui.xml", plugin);
}
	
static void
gnc_plugin_account_tree_remove_from_window (GncPlugin *plugin,
					    GncMainWindow *window,
					    GQuark type)
{
	g_return_if_fail (GNC_IS_PLUGIN_ACCOUNT_TREE (plugin));
	g_return_if_fail (GNC_IS_MAIN_WINDOW (window));

	gnc_main_window_unmerge_actions (window, "gnc-plugin-account-tree-default-actions");
}

static const gchar *
gnc_plugin_account_tree_get_name (GncPlugin *plugin)
{
	return GNC_PLUGIN_ACCOUNT_TREE_NAME;
}

static GncPluginPage *
gnc_plugin_account_tree_create_page (GncPlugin *plugin,
				     const gchar *uri)
{
	g_return_val_if_fail (GNC_IS_PLUGIN_ACCOUNT_TREE (plugin), NULL);
	g_return_val_if_fail (uri != NULL, NULL);

	/* FIXME add better URI handling */
	if (strcmp ("default:", uri)) {
		return NULL;
	}
	
	return gnc_plugin_page_account_tree_new ();
}


/* Command callbacks */
static void
gnc_plugin_account_tree_cmd_new_account_tree (EggAction *action, GncMainWindowActionData *data)
{
	GncPluginPage *page;

	g_return_if_fail (data != NULL);

	page = gnc_plugin_page_account_tree_new ();
	gnc_main_window_open_page (data->window, page);
}
