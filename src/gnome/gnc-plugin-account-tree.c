/* 
 * gnc-plugin-account-tree.c -- 
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

#include <string.h>

#include "gnc-plugin-account-tree.h"
#include "gnc-plugin-page-account-tree.h"

#include "messages.h"

static void gnc_plugin_account_tree_class_init (GncPluginAccountTreeClass *klass);
static void gnc_plugin_account_tree_init (GncPluginAccountTree *plugin);
static void gnc_plugin_account_tree_finalize (GObject *object);

/* plugin window interface */
static GncPluginPage *gnc_plugin_account_tree_create_page (GncPlugin *plugin, const gchar *uri);

/* Command callbacks */
static void gnc_plugin_account_tree_cmd_new_account_tree (GtkAction *action, GncMainWindowActionData *data);


#define PLUGIN_ACTIONS_NAME "gnc-plugin-account-tree-actions"
#define PLUGIN_UI_FILENAME  "gnc-plugin-account-tree-ui.xml"

static GtkActionEntry gnc_plugin_actions [] = {
	{ "FileNewAccountTreeAction", NULL, N_("New Accounts _Page"), NULL,
	  N_("Open a new Account Tree page"),
	  G_CALLBACK (gnc_plugin_account_tree_cmd_new_account_tree) },
};
static guint gnc_plugin_n_actions = G_N_ELEMENTS (gnc_plugin_actions);


typedef struct GncPluginAccountTreePrivate
{
	gpointer dummy;
} GncPluginAccountTreePrivate;

#define GNC_PLUGIN_ACCOUNT_TREE_GET_PRIVATE(o)  \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_PLUGIN_ACCOUNT_TREE, GncPluginAccountTreePrivate))

static GObjectClass *parent_class = NULL;

GType
gnc_plugin_account_tree_get_type (void)
{
	static GType gnc_plugin_account_tree_type = 0;

	if (gnc_plugin_account_tree_type == 0) {
		static const GTypeInfo our_info = {
			sizeof (GncPluginAccountTreeClass),
			NULL,		/* base_init */
			NULL,		/* base_finalize */
			(GClassInitFunc) gnc_plugin_account_tree_class_init,
			NULL,		/* class_finalize */
			NULL,		/* class_data */
			sizeof (GncPluginAccountTree),
			0,		/* n_preallocs */
			(GInstanceInitFunc) gnc_plugin_account_tree_init
		};
		
		gnc_plugin_account_tree_type = g_type_register_static (GNC_TYPE_PLUGIN,
								       "GncPluginAccountTree",
								       &our_info, 0);
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
	GncPluginClass *plugin_class = GNC_PLUGIN_CLASS (klass);

	parent_class = g_type_class_peek_parent (klass);

	object_class->finalize = gnc_plugin_account_tree_finalize;

	/* plugin info */
	plugin_class->plugin_name  = GNC_PLUGIN_ACCOUNT_TREE_NAME;

	/* function overrides */
	plugin_class->create_page  = gnc_plugin_account_tree_create_page;

	/* widget addition/removal */
	plugin_class->actions_name = PLUGIN_ACTIONS_NAME;
	plugin_class->actions      = gnc_plugin_actions;
	plugin_class->n_actions    = gnc_plugin_n_actions;
	plugin_class->ui_filename  = PLUGIN_UI_FILENAME;

	g_type_class_add_private(klass, sizeof(GncPluginAccountTreePrivate));
}

static void
gnc_plugin_account_tree_init (GncPluginAccountTree *plugin)
{
}

static void
gnc_plugin_account_tree_finalize (GObject *object)
{
	GncPluginAccountTree *plugin;
	GncPluginAccountTreePrivate *priv;

	g_return_if_fail (GNC_IS_PLUGIN_ACCOUNT_TREE (object));

	plugin = GNC_PLUGIN_ACCOUNT_TREE (object);
	priv = GNC_PLUGIN_ACCOUNT_TREE_GET_PRIVATE (object);

	G_OBJECT_CLASS (parent_class)->finalize (object);
}

/************************************************************
 *              Plugin Function Implementation              *
 ************************************************************/

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

/************************************************************
 *                    Command Callbacks                     *
 ************************************************************/

static void
gnc_plugin_account_tree_cmd_new_account_tree (GtkAction *action,
					      GncMainWindowActionData *data)
{
	g_return_if_fail (data != NULL);
	gnc_new_account_tree (data->window);
}

/************************************************************
 *                     Other Functions                      *
 ************************************************************/

void
gnc_new_account_tree (GncMainWindow *window)
{
	GncPluginPage *page;

	page = gnc_plugin_page_account_tree_new ();
	gnc_main_window_open_page (window, page);
}
