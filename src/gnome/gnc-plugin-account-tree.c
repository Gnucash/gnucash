/* 
 * gnc-plugin-account-tree.c -- 
 * Copyright (C) 2003 Jan Arne Petersen
 * Author: Jan Arne Petersen <jpetersen@uni-bonn.de>
 */

#include "config.h"

#include "gnc-plugin-account-tree.h"

#include "gnc-plugin-page-account-tree.h"

#include "messages.h"

static void gnc_plugin_account_tree_class_init (GncPluginAccountTreeClass *klass);
static void gnc_plugin_account_tree_init (GncPluginAccountTree *plugin);
static void gnc_plugin_account_tree_finalize (GObject *object);

static void gnc_plugin_account_tree_plugin_init (GncPluginIface *iface);

static void gnc_plugin_account_tree_merge_actions (GncPlugin *plugin, EggMenuMerge *ui_merge);
static void gnc_plugin_account_tree_unmerge_actions (GncPlugin *plugin, EggMenuMerge *ui_merge);
static const gchar *gnc_plugin_account_tree_get_name (GncPlugin *plugin);
static GncPluginPage *gnc_plugin_account_tree_create_page (GncPlugin *plugin, const gchar *uri);

/* Command callbacks */
static void gnc_plugin_account_tree_cmd_new_account_tree (EggAction *action, GncPluginAccountTree *plugin);

static EggActionGroupEntry gnc_plugin_account_tree_actions [] = {
	{ "FileNewAccountTreeAction", N_("New Account Tree"), NULL, NULL,
	  N_("Open a new Account Tree page"),
	  G_CALLBACK (gnc_plugin_account_tree_cmd_new_account_tree), NULL },
};
static guint gnc_plugin_account_tree_n_actions = G_N_ELEMENTS (gnc_plugin_account_tree_actions);

struct GncPluginAccountTreePrivate
{
	GncMainWindow *main_window;
	
	EggActionGroup *action_group;
	guint merge_id;

	GtkWidget *widget;
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
gnc_plugin_account_tree_new (GncMainWindow *main_window)
{
	GncPluginAccountTree *plugin;

	plugin = g_object_new (GNC_TYPE_PLUGIN_ACCOUNT_TREE,
			      NULL);

	plugin->priv->main_window = main_window;

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
	gint i;

	plugin->priv = g_new0 (GncPluginAccountTreePrivate, 1);

	/* Create menu and toolbar information */
	for (i = 0; i < gnc_plugin_account_tree_n_actions; i++) {
		gnc_plugin_account_tree_actions[i].user_data = plugin;
	}

	plugin->priv->action_group = egg_action_group_new ("GncPluginAccountTreeActions");
	egg_action_group_add_actions (plugin->priv->action_group, gnc_plugin_account_tree_actions,
				      gnc_plugin_account_tree_n_actions);
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
	iface->merge_actions   = gnc_plugin_account_tree_merge_actions;
	iface->unmerge_actions = gnc_plugin_account_tree_unmerge_actions;
	iface->get_name        = gnc_plugin_account_tree_get_name;
	iface->create_page     = gnc_plugin_account_tree_create_page;
}

static void
gnc_plugin_account_tree_merge_actions (GncPlugin *plugin,
					    EggMenuMerge *ui_merge)
{
	GncPluginAccountTree *plugin_account_tree = GNC_PLUGIN_ACCOUNT_TREE(plugin);
	
	g_return_if_fail (GNC_IS_PLUGIN_ACCOUNT_TREE (plugin_account_tree));

	egg_menu_merge_insert_action_group (ui_merge, plugin_account_tree->priv->action_group, 0);

	plugin_account_tree->priv->merge_id = egg_menu_merge_add_ui_from_file (ui_merge,
									       GNC_UI_DIR "/gnc-plugin-account-tree-ui.xml",
									       NULL);
	egg_menu_merge_ensure_update (ui_merge);
}
	
static void
gnc_plugin_account_tree_unmerge_actions (GncPlugin *plugin,
					      EggMenuMerge *ui_merge)
{
	GncPluginAccountTree *plugin_account_tree = GNC_PLUGIN_ACCOUNT_TREE(plugin);
	
	g_return_if_fail (GNC_IS_PLUGIN_ACCOUNT_TREE (plugin_account_tree));
	g_return_if_fail (plugin_account_tree->priv->merge_id != 0);
	g_return_if_fail (plugin_account_tree->priv->action_group != NULL);

	egg_menu_merge_remove_action_group (ui_merge, plugin_account_tree->priv->action_group);
	egg_menu_merge_remove_ui (ui_merge, plugin_account_tree->priv->merge_id);
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
	return NULL;
}


/* Command callbacks */
static void
gnc_plugin_account_tree_cmd_new_account_tree (EggAction *action, GncPluginAccountTree *plugin)
{
	GncPluginPage *page;

	page = gnc_plugin_page_account_tree_new ();
	gnc_main_window_open_page (plugin->priv->main_window, page);
}
