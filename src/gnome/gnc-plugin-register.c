/* 
 * gnc-plugin-register.c -- 
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

#include "gnc-plugin-register.h"
#include "gnc-plugin-page-register.h"

#include "messages.h"

static void gnc_plugin_register_class_init (GncPluginRegisterClass *klass);
static void gnc_plugin_register_init (GncPluginRegister *plugin);
static void gnc_plugin_register_finalize (GObject *object);

/* plugin window interface */
static GncPluginPage *gnc_plugin_register_create_page (GncPlugin *plugin, const gchar *uri);

/* Command callbacks */
static void gnc_plugin_register_cmd_general_ledger (EggAction *action, GncMainWindowActionData *data);

#define PLUGIN_ACTIONS_NAME "gnc-plugin-register-actions"
#define PLUGIN_UI_FILENAME  "gnc-plugin-register-ui.xml"

static EggActionEntry gnc_plugin_actions [] = {
	{ "ToolsGeneralLedgerAction", N_("_General Ledger"), NULL, NULL,
	  N_("Open a general ledger window"),
	  G_CALLBACK (gnc_plugin_register_cmd_general_ledger) },
};
static guint gnc_plugin_n_actions = G_N_ELEMENTS (gnc_plugin_actions);

struct GncPluginRegisterPrivate
{
	gpointer dummy;
};

static GObjectClass *parent_class = NULL;

GType
gnc_plugin_register_get_type (void)
{
	static GType gnc_plugin_register_type = 0;

	if (gnc_plugin_register_type == 0) {
		static const GTypeInfo our_info = {
			sizeof (GncPluginRegisterClass),
			NULL,		/* base_init */
			NULL,		/* base_finalize */
			(GClassInitFunc) gnc_plugin_register_class_init,
			NULL,		/* class_finalize */
			NULL,		/* class_data */
			sizeof (GncPluginRegister),
			0,		/* n_preallocs */
			(GInstanceInitFunc) gnc_plugin_register_init
		};
		
		gnc_plugin_register_type = g_type_register_static (GNC_TYPE_PLUGIN,
								   "GncPluginRegister",
								   &our_info, 0);
	}

	return gnc_plugin_register_type;
}

GncPlugin *
gnc_plugin_register_new (void)
{
	GncPluginRegister *plugin;

	plugin = g_object_new (GNC_TYPE_PLUGIN_REGISTER,
			      NULL);

	return GNC_PLUGIN (plugin);
}

static void
gnc_plugin_register_class_init (GncPluginRegisterClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	GncPluginClass *plugin_class = GNC_PLUGIN_CLASS (klass);

	parent_class = g_type_class_peek_parent (klass);

	object_class->finalize = gnc_plugin_register_finalize;

	/* plugin info */
	plugin_class->plugin_name  = GNC_PLUGIN_REGISTER_NAME;

	/* function overrides */
	plugin_class->create_page  = gnc_plugin_register_create_page;

	/* widget addition/removal */
	plugin_class->actions_name = PLUGIN_ACTIONS_NAME;
	plugin_class->actions      = gnc_plugin_actions;
	plugin_class->n_actions    = gnc_plugin_n_actions;
	plugin_class->ui_filename  = PLUGIN_UI_FILENAME;
}

static void
gnc_plugin_register_init (GncPluginRegister *plugin)
{
	plugin->priv = g_new0 (GncPluginRegisterPrivate, 1);
}

static void
gnc_plugin_register_finalize (GObject *object)
{
	GncPluginRegister *plugin;

	g_return_if_fail (GNC_IS_PLUGIN_REGISTER (object));

	plugin = GNC_PLUGIN_REGISTER (object);
	g_return_if_fail (plugin->priv != NULL);

	g_free (plugin->priv);

	G_OBJECT_CLASS (parent_class)->finalize (object);
}

/************************************************************
 *              Plugin Function Implementation              *
 ************************************************************/

static GncPluginPage *
gnc_plugin_register_create_page (GncPlugin *plugin,
				 const gchar *uri)
{
	g_return_val_if_fail (GNC_IS_PLUGIN_REGISTER (plugin), NULL);
	g_return_val_if_fail (uri != NULL, NULL);

	/* FIXME add better URI handling */
	if (strcmp ("default:", uri)) {
		return NULL;
	}
	
	return NULL;
}

/************************************************************
 *                    Command Callbacks                     *
 ************************************************************/

static void
gnc_plugin_register_cmd_general_ledger (EggAction *action,
					GncMainWindowActionData *data)
{
	GncPluginPage *page;

	g_return_if_fail (data != NULL);

	page = gnc_plugin_page_register_new_gl ();
	gnc_main_window_open_page (data->window, page);
}

#if 0
static void
gnc_main_window_cmd_tools_general_ledger (EggAction *action,
					  GncMainWindow *window)
{
	GNCLedgerDisplay *ld;
	GNCSplitReg *gsr;
	RegWindow *regData;

	ld = gnc_ledger_display_gl ();
	gsr = gnc_ledger_display_get_user_data (ld);
	if (!gsr) {
		regData = regWindowLedger (ld);
		gnc_register_raise (regData);
	} else {
		gnc_split_reg_raise (gsr);
	}
}
#endif
